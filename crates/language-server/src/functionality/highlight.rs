use async_lsp::ResponseError;
use async_lsp::lsp_types::{DocumentHighlight, DocumentHighlightKind};
use common::InputDb;
use hir::{
    core::semantic::reference::Target, hir_def::TopLevelMod, lower::map_file_to_mod, span::LazySpan,
};

use crate::{
    backend::Backend,
    util::{to_lsp_range_from_span, to_offset_from_position},
};

use super::goto::Cursor;

/// Find all occurrences of the symbol at the cursor position within the same file.
fn find_highlights_at_cursor<'db>(
    db: &'db impl hir::SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Vec<DocumentHighlight> {
    // Get the target at cursor (handles references, definitions, and bindings)
    let resolution = top_mod.target_at(db, cursor);
    let Some(target) = resolution.first() else {
        return vec![];
    };

    let mut highlights = vec![];

    // Search within this module using the unified API
    for matched in top_mod.references_to_target(db, target) {
        if let Some(span) = matched.span.resolve(db)
            && let Ok(range) = to_lsp_range_from_span(span, db)
        {
            highlights.push(DocumentHighlight {
                range,
                kind: Some(DocumentHighlightKind::READ),
            });
        }
    }

    // Include the definition
    match &target {
        Target::Scope(target_scope) => {
            // Check if definition is in this file
            if let Some(name_span) = target_scope.name_span(db)
                && let Some(def_span) = name_span.resolve(db)
            {
                // Get the top mod's span to compare files
                if let Some(mod_span) = top_mod.span().resolve(db)
                    && def_span.file == mod_span.file
                    && let Ok(range) = to_lsp_range_from_span(def_span, db)
                {
                    highlights.insert(
                        0,
                        DocumentHighlight {
                            range,
                            kind: Some(DocumentHighlightKind::WRITE),
                        },
                    );
                }
            }
        }
        Target::Local { span, .. } => {
            // Local definition is always in this file
            if let Some(resolved) = span.resolve(db)
                && let Ok(range) = to_lsp_range_from_span(resolved, db)
            {
                highlights.insert(
                    0,
                    DocumentHighlight {
                        range,
                        kind: Some(DocumentHighlightKind::WRITE),
                    },
                );
            }
        }
    }

    highlights
}

#[cfg(test)]
fn lsp_pos(line: u32, character: u32) -> async_lsp::lsp_types::Position {
    async_lsp::lsp_types::Position { line, character }
}

pub async fn handle_document_highlight(
    backend: &Backend,
    params: async_lsp::lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );

    // Quick existence check on actor thread
    if backend.db.workspace().get(&backend.db, &url).is_none() {
        return Ok(None);
    }

    let position = params.text_document_position_params.position;

    // Spawn heavy highlight resolution on the worker pool
    let rx = backend.spawn_on_workers(move |db| {
        let Some(file) = db.workspace().get(db, &url) else {
            return vec![];
        };
        let file_text = file.text(db);
        let cursor: Cursor = to_offset_from_position(position, file_text.as_str());
        let top_mod = map_file_to_mod(db, file);
        find_highlights_at_cursor(db, top_mod, cursor)
    });

    let highlights: Vec<DocumentHighlight> = rx.await.map_err(|_| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            "Worker task cancelled".to_string(),
        )
    })?;

    if highlights.is_empty() {
        Ok(None)
    } else {
        Ok(Some(highlights))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    fn run_highlight(code: &str, line: u32, col: u32) -> Vec<DocumentHighlight> {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///test.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url, Some(code.to_string()));
        let file_text = file.text(&db);
        let cursor = to_offset_from_position(lsp_pos(line, col), file_text.as_str());
        let top_mod = map_file_to_mod(&db, file);
        find_highlights_at_cursor(&db, top_mod, cursor)
    }

    #[test]
    fn highlight_function() {
        let code = "fn greet() -> i32 {\n    42\n}\n\nfn main() -> i32 {\n    greet()\n}\n";
        let highlights = run_highlight(code, 0, 3);
        // Definition (WRITE) + call site (READ)
        assert_eq!(highlights.len(), 2, "expected 2 highlights, got {highlights:?}");
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::WRITE));
        assert_eq!(highlights[1].kind, Some(DocumentHighlightKind::READ));
    }

    #[test]
    fn highlight_local_variable() {
        let code = "fn foo() -> i32 {\n    let x = 10\n    x + 1\n}\n";
        let highlights = run_highlight(code, 1, 8);
        assert_eq!(highlights.len(), 2, "expected def + usage, got {highlights:?}");
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::WRITE));
    }

    #[test]
    fn highlight_struct_type() {
        let code = r#"struct Point {
    x: i32
}

fn make() -> Point {
    Point { x: 1 }
}
"#;
        let highlights = run_highlight(code, 0, 7);
        // Definition + return type + construction = 3
        assert!(
            highlights.len() >= 3,
            "expected at least 3 highlights for struct, got {highlights:?}"
        );
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::WRITE));
    }

    #[test]
    fn highlight_enum_variant() {
        let code = r#"enum Color {
    Red,
    Green
}

fn check(c: Color) -> bool {
    match c {
        Color::Red => true
        Color::Green => false
    }
}
"#;
        // Highlight on "Red" variant definition (line 1, col 4)
        let highlights = run_highlight(code, 1, 4);
        // Definition + match arm usage
        assert!(
            highlights.len() >= 2,
            "expected at least 2 highlights for variant, got {highlights:?}"
        );
    }

    #[test]
    fn highlight_no_target() {
        let code = "fn foo() -> i32 {\n    42\n}\n";
        // Cursor on whitespace
        let highlights = run_highlight(code, 1, 0);
        assert!(
            highlights.is_empty(),
            "expected no highlights on whitespace"
        );
    }
}
