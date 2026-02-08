use async_lsp::ResponseError;
use async_lsp::lsp_types::{TextEdit, WorkspaceEdit};
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::{HirIngot, ItemKind, Trait, scope_graph::ScopeId},
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
    let lsp_uri = params.text_document_position.text_document.uri.clone();
    if backend.is_builtin_tmp_uri(&lsp_uri) {
        return Err(ResponseError::new(
            async_lsp::ErrorCode::INVALID_REQUEST,
            "Renaming symbols in built-in library files is not supported.".to_string(),
        ));
    }
    if backend.is_git_cache_uri(&lsp_uri) {
        return Err(ResponseError::new(
            async_lsp::ErrorCode::INVALID_REQUEST,
            "Renaming symbols in git dependency cache files is not supported.".to_string(),
        ));
    }

    let internal_url = backend.map_client_uri_to_internal(lsp_uri);
    let Some(file) = backend.db.workspace().get(&backend.db, &internal_url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor =
        to_offset_from_position(params.text_document_position.position, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);

    // Get the target at cursor (handles references, definitions, and bindings)
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    if let Target::Scope(scope) = target
        && scope
            .name_span(&backend.db)
            .and_then(|span| span.resolve(&backend.db))
            .and_then(|span| span.file.url(&backend.db))
            .is_some_and(|url| {
                url.scheme().starts_with("builtin-") || backend.is_git_cache_uri(&url)
            })
    {
        return Err(ResponseError::new(
            async_lsp::ErrorCode::INVALID_REQUEST,
            "Renaming symbols defined in read-only library sources is not supported.".to_string(),
        ));
    }

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
                for ref_view in mod_.references_to_target(&backend.db, target) {
                    // Skip `Self` type paths - they shouldn't be renamed
                    if ref_view.is_self_ty_path(&backend.db) {
                        continue;
                    }
                    if let Some(span) = ref_view.rename_span(&backend.db).resolve(&backend.db)
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

            // If this is a trait method, also rename implementations
            if let ItemKind::Func(func) = target_scope.item()
                && let Some(ScopeId::Item(ItemKind::Trait(trait_))) =
                    target_scope.parent(&backend.db)
            {
                rename_trait_method_implementations(
                    &backend.db,
                    trait_,
                    func.name(&backend.db),
                    new_name,
                    &mut changes,
                );
            }
        }
        Target::Local { span, .. } => {
            // For local bindings, search within the function body
            for ref_view in top_mod.references_to_target(&backend.db, target) {
                if let Some(resolved) = ref_view.rename_span(&backend.db).resolve(&backend.db)
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

    // Never propose edits to embedded built-in library sources.
    changes
        .retain(|url, _| !url.scheme().starts_with("builtin-") && !backend.is_git_cache_uri(url));

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

/// Rename all implementations of a trait method in impl blocks.
fn rename_trait_method_implementations<'db>(
    db: &'db driver::DriverDataBase,
    trait_: Trait<'db>,
    method_name: hir::hir_def::Partial<hir::hir_def::IdentId<'db>>,
    new_name: &str,
    changes: &mut FxHashMap<url::Url, Vec<TextEdit>>,
) {
    let Some(method_name) = method_name.to_opt() else {
        return;
    };

    let ingot = trait_.top_mod(db).ingot(db);

    // Find all impl blocks for this trait and rename matching methods
    for impl_trait in ingot.all_impl_traits(db) {
        if let Some(implemented_trait) = impl_trait.trait_def(db)
            && implemented_trait == trait_
        {
            // Find the method in this impl block
            for method in impl_trait.methods(db) {
                if method.name(db).to_opt() == Some(method_name)
                    && let Some(name_span) = method.scope().name_span(db)
                    && let Some(span) = name_span.resolve(db)
                    && let Some(impl_url) = span.file.url(db)
                    && let Ok(range) = to_lsp_range_from_span(span, db)
                {
                    changes.entry(impl_url).or_default().push(TextEdit {
                        range,
                        new_text: new_name.to_string(),
                    });
                }
            }
        }
    }
}
