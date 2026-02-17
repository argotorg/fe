use async_lsp::ResponseError;
use async_lsp::lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};
use common::InputDb;
use hir::{hir_def::ItemKind, lower::map_file_to_mod, span::LazySpan};

use crate::{backend::Backend, util::to_lsp_range_from_span};

/// Handle textDocument/foldingRange.
pub async fn handle_folding_range(
    backend: &Backend,
    params: FoldingRangeParams,
) -> Result<Option<Vec<FoldingRange>>, ResponseError> {
    let internal_url = backend.map_client_uri_to_internal(params.text_document.uri);
    let Some(file) = backend.db.workspace().get(&backend.db, &internal_url) else {
        return Ok(None);
    };

    let top_mod = map_file_to_mod(&backend.db, file);
    let scope_graph = top_mod.scope_graph(&backend.db);

    let mut ranges = Vec::new();

    for item in scope_graph.items_dfs(&backend.db) {
        let item_span: hir::span::DynLazySpan = item.span().into();
        let Some(span) = item_span.resolve(&backend.db) else {
            continue;
        };
        let Ok(range) = to_lsp_range_from_span(span, &backend.db) else {
            continue;
        };

        // If the span ends at column 0, the actual content ends on the
        // previous line (trailing newline included in the parser span).
        let end_line = if range.end.character == 0 && range.end.line > 0 {
            range.end.line - 1
        } else {
            range.end.line
        };

        // Only fold multi-line items
        if range.start.line == end_line {
            continue;
        }

        let kind = match item {
            ItemKind::Func(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_) => Some(FoldingRangeKind::Region),
            _ => None,
        };

        ranges.push(FoldingRange {
            start_line: range.start.line,
            start_character: Some(range.start.character),
            end_line,
            end_character: Some(range.end.character),
            kind,
            collapsed_text: None,
        });
    }

    if ranges.is_empty() {
        Ok(None)
    } else {
        Ok(Some(ranges))
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{lower::map_file_to_mod, span::LazySpan};
    use url::Url;

    use crate::util::to_lsp_range_from_span;

    fn collect_folding_ranges(
        db: &DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod,
    ) -> Vec<(u32, u32)> {
        let scope_graph = top_mod.scope_graph(db);
        let mut ranges = Vec::new();

        for item in scope_graph.items_dfs(db) {
            let item_span: hir::span::DynLazySpan = item.span().into();
            let Some(span) = item_span.resolve(db) else {
                continue;
            };
            let Ok(range) = to_lsp_range_from_span(span, db) else {
                continue;
            };
            let end_line = if range.end.character == 0 && range.end.line > 0 {
                range.end.line - 1
            } else {
                range.end.line
            };
            if range.start.line != end_line {
                ranges.push((range.start.line, end_line));
            }
        }

        ranges
    }

    #[test]
    fn test_folding_basic_function() {
        let mut db = DriverDataBase::default();
        let code = "fn foo() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let ranges = collect_folding_ranges(&db, top_mod);

        assert!(!ranges.is_empty(), "should have folding range for function");
        assert_eq!(ranges[0], (0, 2), "function should fold from line 0 to 2");
    }

    #[test]
    fn test_folding_struct_and_impl() {
        let mut db = DriverDataBase::default();
        let code = r#"struct Point {
    x: i32
    y: i32
}

impl Point {
    fn sum(self) -> i32 {
        self.x
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let ranges = collect_folding_ranges(&db, top_mod);

        // Should have ranges for: struct, impl, fn sum
        assert!(
            ranges.len() >= 3,
            "should have at least 3 folding ranges, got {}",
            ranges.len()
        );
    }

    #[test]
    fn test_folding_single_line_no_fold() {
        let mut db = DriverDataBase::default();
        // A single-line function shouldn't produce a folding range
        let code = "fn x() -> i32 { 1 }\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let ranges = collect_folding_ranges(&db, top_mod);

        assert!(ranges.is_empty(), "single-line items should not fold");
    }

    #[test]
    fn test_folding_trait_and_impl_trait() {
        let mut db = DriverDataBase::default();
        let code = r#"trait Runnable {
    fn run(self) -> i32
}

struct Task {
    id: i32
}

impl Runnable for Task {
    fn run(self) -> i32 {
        self.id
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let ranges = collect_folding_ranges(&db, top_mod);

        // trait, struct, impl trait, fn run (at minimum)
        assert!(
            ranges.len() >= 4,
            "should have at least 4 folding ranges, got {}",
            ranges.len()
        );
    }
}
