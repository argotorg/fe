use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::TopLevelMod,
    lower::map_file_to_mod,
    span::LazySpan,
};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

use super::goto::Cursor;

/// Find all references to the symbol at the cursor position.
fn find_references_at_cursor<'db>(
    db: &'db impl hir::SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Vec<async_lsp::lsp_types::Location> {
    // Get the reference at cursor and resolve its target
    let Some(reference) = top_mod.reference_at(db, cursor) else {
        return vec![];
    };

    let Some(target) = reference.target_at(db, cursor) else {
        return vec![];
    };

    match target {
        Target::Scope(target_scope) => {
            // Find all references to this scope across all modules in the ingot
            let mut locations = vec![];

            // Get the ingot containing this module
            let ingot = top_mod.ingot(db);

            // Search all .fe files in the ingot
            for (url, file) in ingot.files(db).iter() {
                // Skip non-.fe files
                if !url.path().ends_with(".fe") {
                    continue;
                }
                let mod_ = map_file_to_mod(db, file);
                for ref_view in mod_.references_to(db, target_scope) {
                    if ref_view.span().resolve(db).is_some() {
                        if let Ok(location) = to_lsp_location_from_lazy_span(db, ref_view.span()) {
                            locations.push(location);
                        }
                    }
                }
            }

            // Also include the definition location
            if let Ok(def_location) = to_lsp_location_from_scope(db, target_scope) {
                // Insert at beginning so definition comes first
                locations.insert(0, def_location);
            }

            locations
        }
        Target::Local { .. } => {
            // For local bindings, references are only within the same function body
            // Currently references_to only supports Scope targets
            // TODO: Support local binding references
            vec![]
        }
    }
}

pub async fn handle_references(
    backend: &Backend,
    params: async_lsp::lsp_types::ReferenceParams,
) -> Result<Option<Vec<async_lsp::lsp_types::Location>>, ResponseError> {
    let path_str = params
        .text_document_position
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
        params.text_document_position.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let locations = find_references_at_cursor(&backend.db, top_mod, cursor);

    if locations.is_empty() {
        Ok(None)
    } else {
        Ok(Some(locations))
    }
}
