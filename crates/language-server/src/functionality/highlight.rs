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
    // Get the reference at cursor and resolve its target
    let Some(reference) = top_mod.reference_at(db, cursor) else {
        return vec![];
    };

    let Some(target) = reference.target_at(db, cursor) else {
        return vec![];
    };

    let mut highlights = vec![];

    // Search within this module using the unified API
    for ref_view in top_mod.references_to_target(db, target.clone()) {
        if let Some(span) = ref_view.span().resolve(db)
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

pub async fn handle_document_highlight(
    backend: &Backend,
    params: async_lsp::lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>, ResponseError> {
    let path_str = params
        .text_document_position_params
        .text_document
        .uri
        .path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let highlights = find_highlights_at_cursor(&backend.db, top_mod, cursor);

    if highlights.is_empty() {
        Ok(None)
    } else {
        Ok(Some(highlights))
    }
}
