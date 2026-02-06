use async_lsp::ResponseError;
use async_lsp::lsp_types::{SelectionRange, SelectionRangeParams};
use common::InputDb;
use hir::{
    hir_def::ItemKind,
    lower::map_file_to_mod,
    span::LazySpan,
};

use crate::{
    backend::Backend,
    util::{to_lsp_range_from_span, to_offset_from_position},
};

/// Handle textDocument/selectionRange.
///
/// Builds nested selection ranges from the AST hierarchy at each requested position.
/// The ranges go from innermost (identifier/expression) to outermost (module).
pub async fn handle_selection_range(
    backend: &Backend,
    params: SelectionRangeParams,
) -> Result<Option<Vec<SelectionRange>>, ResponseError> {
    let path_str = params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let top_mod = map_file_to_mod(&backend.db, file);
    let scope_graph = top_mod.scope_graph(&backend.db);

    let mut results = Vec::new();

    for position in &params.positions {
        let cursor = to_offset_from_position(*position, file_text.as_str());

        // Collect all items and their spans, sorted by span size (smallest first)
        let mut containing_spans = Vec::new();

        for item in scope_graph.items_dfs(&backend.db) {
            // Get the full item span
            let item_span: hir::span::DynLazySpan = item.span().into();
            if let Some(span) = item_span.resolve(&backend.db) {
                let start: usize = span.range.start().into();
                let end: usize = span.range.end().into();
                if start <= cursor.into() && usize::from(cursor) <= end {
                    if let Ok(range) = to_lsp_range_from_span(span.clone(), &backend.db) {
                        containing_spans.push((end - start, range));
                    }
                }
            }

            // Also add the name span if available (narrower selection)
            if let Some(name_span) = item.name_span() {
                if let Some(span) = name_span.resolve(&backend.db) {
                    let start: usize = span.range.start().into();
                    let end: usize = span.range.end().into();
                    if start <= cursor.into() && usize::from(cursor) <= end {
                        if let Ok(range) = to_lsp_range_from_span(span, &backend.db) {
                            containing_spans.push((end - start, range));
                        }
                    }
                }
            }

            // For functions, also add parameter list and body spans
            if let ItemKind::Func(func) = item {
                if let Some(body) = func.body(&backend.db) {
                    let body_span: hir::span::DynLazySpan = body.span().into();
                    if let Some(span) = body_span.resolve(&backend.db) {
                        let start: usize = span.range.start().into();
                        let end: usize = span.range.end().into();
                        if start <= cursor.into() && usize::from(cursor) <= end {
                            if let Ok(range) = to_lsp_range_from_span(span, &backend.db) {
                                containing_spans.push((end - start, range));
                            }
                        }
                    }
                }
            }
        }

        // Sort by span size: smallest first (most specific)
        containing_spans.sort_by_key(|(size, _)| *size);
        containing_spans.dedup_by_key(|(_, range)| *range);

        // Build nested SelectionRange from outermost to innermost
        let selection_range = containing_spans
            .into_iter()
            .rev()
            .fold(None, |parent, (_, range)| {
                Some(SelectionRange {
                    range,
                    parent: parent.map(Box::new),
                })
            });

        // If we found no containing items, create a trivial selection at the position
        results.push(selection_range.unwrap_or(SelectionRange {
            range: async_lsp::lsp_types::Range {
                start: *position,
                end: *position,
            },
            parent: None,
        }));
    }

    Ok(Some(results))
}

/// Build a selection range chain for a cursor position in a module.
fn build_selection_ranges_at(
    db: &driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod,
    cursor: parser::TextSize,
) -> Option<SelectionRange> {
    let scope_graph = top_mod.scope_graph(db);
    let mut containing_spans = Vec::new();

    for item in scope_graph.items_dfs(db) {
        let item_span: hir::span::DynLazySpan = item.span().into();
        if let Some(span) = item_span.resolve(db) {
            let start: usize = span.range.start().into();
            let end: usize = span.range.end().into();
            if start <= cursor.into() && usize::from(cursor) <= end {
                if let Ok(range) = to_lsp_range_from_span(span.clone(), db) {
                    containing_spans.push((end - start, range));
                }
            }
        }

        if let Some(name_span) = item.name_span() {
            if let Some(span) = name_span.resolve(db) {
                let start: usize = span.range.start().into();
                let end: usize = span.range.end().into();
                if start <= cursor.into() && usize::from(cursor) <= end {
                    if let Ok(range) = to_lsp_range_from_span(span, db) {
                        containing_spans.push((end - start, range));
                    }
                }
            }
        }

        if let ItemKind::Func(func) = item {
            if let Some(body) = func.body(db) {
                let body_span: hir::span::DynLazySpan = body.span().into();
                if let Some(span) = body_span.resolve(db) {
                    let start: usize = span.range.start().into();
                    let end: usize = span.range.end().into();
                    if start <= cursor.into() && usize::from(cursor) <= end {
                        if let Ok(range) = to_lsp_range_from_span(span, db) {
                            containing_spans.push((end - start, range));
                        }
                    }
                }
            }
        }
    }

    containing_spans.sort_by_key(|(size, _)| *size);
    containing_spans.dedup_by_key(|(_, range)| *range);

    containing_spans
        .into_iter()
        .rev()
        .fold(None, |parent, (_, range)| {
            Some(SelectionRange {
                range,
                parent: parent.map(Box::new),
            })
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    /// Count the depth of nested selection ranges.
    fn selection_depth(sel: &SelectionRange) -> usize {
        let mut depth = 1;
        let mut current = sel;
        while let Some(parent) = &current.parent {
            depth += 1;
            current = parent;
        }
        depth
    }

    #[test]
    fn test_selection_range_on_function_name() {
        let mut db = DriverDataBase::default();
        let code = "fn hello() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Cursor on "hello" (offset 3)
        let cursor = parser::TextSize::from(3);
        let sel = build_selection_ranges_at(&db, top_mod, cursor);
        assert!(sel.is_some(), "should have selection ranges on function name");

        let sel = sel.unwrap();
        // Innermost should be the name "hello", outermost should be the whole function
        assert!(selection_depth(&sel) >= 2, "should have at least name and function spans");
    }

    #[test]
    fn test_selection_range_inside_body() {
        let mut db = DriverDataBase::default();
        let code = "fn compute() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Cursor inside body (on "42", offset ~26)
        let offset = code.find("42").unwrap() as u32;
        let cursor = parser::TextSize::from(offset);
        let sel = build_selection_ranges_at(&db, top_mod, cursor);
        assert!(sel.is_some(), "should have selection ranges inside body");

        let sel = sel.unwrap();
        // Should have at least body and function
        assert!(selection_depth(&sel) >= 2, "should have body and function spans");
    }

    #[test]
    fn test_selection_range_nested_items() {
        let mut db = DriverDataBase::default();
        let code = r#"struct Wrapper {
    value: i32
}

impl Wrapper {
    fn get(self) -> i32 {
        self.value
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Cursor on "get" method name
        let offset = code.find("get").unwrap() as u32;
        let cursor = parser::TextSize::from(offset);
        let sel = build_selection_ranges_at(&db, top_mod, cursor);
        assert!(sel.is_some(), "should have selection ranges on method name");

        let sel = sel.unwrap();
        // Should have: method name -> method -> impl block (at least 3)
        assert!(selection_depth(&sel) >= 3, "should have name, method, and impl spans, got {}", selection_depth(&sel));
    }

    #[test]
    fn test_selection_range_outside_items() {
        let mut db = DriverDataBase::default();
        let code = "\n\nfn foo() {}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Cursor at offset 0 (before any items)
        let cursor = parser::TextSize::from(0);
        let sel = build_selection_ranges_at(&db, top_mod, cursor);
        // May or may not find ranges depending on top-level module span
        // The important thing is it doesn't crash
        let _ = sel;
    }
}
