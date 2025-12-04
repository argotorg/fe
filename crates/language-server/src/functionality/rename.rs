use async_lsp::ResponseError;
use async_lsp::lsp_types::{TextEdit, WorkspaceEdit};
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::{ItemKind, scope_graph::ScopeId},
    lower::map_file_to_mod,
    span::LazySpan,
};
use rustc_hash::FxHashMap;

use crate::{
    backend::Backend,
    util::{to_lsp_range_from_span, to_offset_from_position},
};

use super::goto::Cursor;

pub async fn handle_rename(
    backend: &Backend,
    params: async_lsp::lsp_types::RenameParams,
) -> Result<Option<WorkspaceEdit>, ResponseError> {
    let path_str = params.text_document_position.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor =
        to_offset_from_position(params.text_document_position.position, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);

    // Get the reference at cursor and resolve its target
    let Some(reference) = top_mod.reference_at(&backend.db, cursor) else {
        return Ok(None);
    };

    let Some(target) = reference.target_at(&backend.db, cursor) else {
        return Ok(None);
    };

    let new_name = &params.new_name;

    let mut changes: FxHashMap<url::Url, Vec<TextEdit>> = FxHashMap::default();

    match &target {
        Target::Scope(target_scope) => {
            // Skip module renames - they require file operations
            if is_module_scope(*target_scope) {
                return Err(ResponseError::new(
                    async_lsp::ErrorCode::INVALID_REQUEST,
                    "Renaming modules is not supported - it would require renaming files"
                        .to_string(),
                ));
            }

            // Get the ingot containing this module
            let ingot = top_mod.ingot(&backend.db);

            // Search all .fe files in the ingot for references
            for (file_url, file) in ingot.files(&backend.db).iter() {
                // Skip non-.fe files
                if !file_url.path().ends_with(".fe") {
                    continue;
                }
                let mod_ = map_file_to_mod(&backend.db, file);
                for ref_view in mod_.references_to_target(&backend.db, target.clone()) {
                    if let Some(span) = ref_view.span().resolve(&backend.db)
                        && let Ok(range) = to_lsp_range_from_span(span, &backend.db)
                    {
                        changes.entry(file_url.clone()).or_default().push(TextEdit {
                            range,
                            new_text: new_name.clone(),
                        });
                    }
                }
            }

            // Also rename the definition itself
            if let Some(name_span) = target_scope.name_span(&backend.db)
                && let Some(span) = name_span.resolve(&backend.db)
                && let Some(def_url) = span.file.url(&backend.db)
                && let Ok(range) = to_lsp_range_from_span(span, &backend.db)
            {
                changes.entry(def_url).or_default().push(TextEdit {
                    range,
                    new_text: new_name.clone(),
                });
            }
        }
        Target::Local { span, .. } => {
            // For local bindings, search within the function body
            for ref_view in top_mod.references_to_target(&backend.db, target.clone()) {
                if let Some(resolved) = ref_view.span().resolve(&backend.db)
                    && let Some(ref_url) = resolved.file.url(&backend.db)
                    && let Ok(range) = to_lsp_range_from_span(resolved, &backend.db)
                {
                    changes.entry(ref_url).or_default().push(TextEdit {
                        range,
                        new_text: new_name.clone(),
                    });
                }
            }

            // Also rename the definition itself (the binding site)
            if let Some(resolved) = span.resolve(&backend.db)
                && let Some(def_url) = resolved.file.url(&backend.db)
                && let Ok(range) = to_lsp_range_from_span(resolved, &backend.db)
            {
                changes.entry(def_url).or_default().push(TextEdit {
                    range,
                    new_text: new_name.clone(),
                });
            }
        }
    }

    if changes.is_empty() {
        Ok(None)
    } else {
        Ok(Some(WorkspaceEdit {
            changes: Some(changes.into_iter().collect()),
            document_changes: None,
            change_annotations: None,
        }))
    }
}

/// Check if a scope refers to a module (top-level or nested)
fn is_module_scope(scope: ScopeId) -> bool {
    matches!(scope.item(), ItemKind::TopMod(_) | ItemKind::Mod(_))
}
