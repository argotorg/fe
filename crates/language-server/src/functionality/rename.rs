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

/// Result from the rename computation worker.
enum RenameOutcome {
    /// No target found at cursor.
    NoTarget,
    /// An error that should be returned to the client.
    ClientError(async_lsp::ErrorCode, String),
    /// Computed edits (with internal URLs, before URI mapping).
    Edits(FxHashMap<url::Url, Vec<TextEdit>>),
}

pub async fn handle_rename(
    backend: &Backend,
    params: async_lsp::lsp_types::RenameParams,
) -> Result<Option<WorkspaceEdit>, ResponseError> {
    let lsp_uri = params.text_document_position.text_document.uri.clone();
    if backend.is_virtual_uri(&lsp_uri) {
        return Err(ResponseError::new(
            async_lsp::ErrorCode::INVALID_REQUEST,
            "Renaming symbols in built-in library files is not supported.".to_string(),
        ));
    }

    let internal_url = backend.map_client_uri_to_internal(lsp_uri);

    // Quick existence check on actor thread
    if backend.db.workspace().get(&backend.db, &internal_url).is_none() {
        return Ok(None);
    }

    let position = params.text_document_position.position;
    let new_name = params.new_name.clone();

    // Spawn heavy rename computation on the worker pool
    let rx = backend.spawn_on_workers(move |db| {
        compute_rename_edits(db, &internal_url, position, &new_name)
    });

    let outcome: RenameOutcome = rx.await.map_err(|_| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            "Worker task cancelled".to_string(),
        )
    })?;

    match outcome {
        RenameOutcome::NoTarget => Ok(None),
        RenameOutcome::ClientError(code, msg) => Err(ResponseError::new(code, msg)),
        RenameOutcome::Edits(changes) => {
            // Filter builtins and map URIs on actor thread (lightweight)
            let changes: FxHashMap<url::Url, Vec<TextEdit>> = changes
                .into_iter()
                .filter(|(url, _)| !url.scheme().starts_with("builtin-"))
                .map(|(url, edits)| (backend.map_internal_uri_to_client(url), edits))
                .collect();

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
    }
}

/// Heavy computation: resolve target, find all references, build edit map.
/// Runs on the worker thread with a salsa db snapshot.
fn compute_rename_edits(
    db: &driver::DriverDataBase,
    file_url: &url::Url,
    position: async_lsp::lsp_types::Position,
    new_name: &str,
) -> RenameOutcome {
    let Some(file) = db.workspace().get(db, file_url) else {
        return RenameOutcome::NoTarget;
    };

    let file_text = file.text(db);
    let cursor: Cursor = to_offset_from_position(position, file_text.as_str());

    let top_mod = map_file_to_mod(db, file);

    // Get the target at cursor
    let resolution = top_mod.target_at(db, cursor);
    let Some(target) = resolution.first() else {
        return RenameOutcome::NoTarget;
    };

    // Check if target is defined in builtins
    if let Target::Scope(scope) = target
        && scope
            .name_span(db)
            .and_then(|span| span.resolve(db))
            .and_then(|span| span.file.url(db))
            .is_some_and(|url| url.scheme().starts_with("builtin-"))
    {
        return RenameOutcome::ClientError(
            async_lsp::ErrorCode::INVALID_REQUEST,
            "Renaming symbols defined in the built-in libraries is not supported.".to_string(),
        );
    }

    let mut changes: FxHashMap<url::Url, Vec<TextEdit>> = FxHashMap::default();

    match &target {
        Target::Scope(target_scope) => {
            // Skip module renames - they require file operations
            if is_module_scope(*target_scope) {
                return RenameOutcome::ClientError(
                    async_lsp::ErrorCode::INVALID_REQUEST,
                    "Renaming modules is not supported - it would require renaming files"
                        .to_string(),
                );
            }

            // Get the ingot containing this module
            let ingot = top_mod.ingot(db);

            // Search all .fe files in the ingot for references
            for (file_url, file) in ingot.files(db).iter() {
                if !file_url.path().ends_with(".fe") {
                    continue;
                }
                let mod_ = map_file_to_mod(db, file);
                for matched in mod_.references_to_target(db, target) {
                    if matched.is_self_ty {
                        continue;
                    }
                    if let Some(span) = matched.span.resolve(db)
                        && let Ok(range) = to_lsp_range_from_span(span, db)
                    {
                        changes.entry(file_url.clone()).or_default().push(TextEdit {
                            range,
                            new_text: new_name.to_string(),
                        });
                    }
                }
            }

            // Also rename the definition itself
            if let Some(name_span) = target_scope.name_span(db)
                && let Some(span) = name_span.resolve(db)
                && let Some(def_url) = span.file.url(db)
                && let Ok(range) = to_lsp_range_from_span(span, db)
            {
                changes.entry(def_url).or_default().push(TextEdit {
                    range,
                    new_text: new_name.to_string(),
                });
            }

            // If this is a trait method, also rename implementations
            if let ItemKind::Func(func) = target_scope.item()
                && let Some(ScopeId::Item(ItemKind::Trait(trait_))) = target_scope.parent(db)
            {
                rename_trait_method_implementations(
                    db,
                    trait_,
                    func.name(db),
                    new_name,
                    &mut changes,
                );
            }
        }
        Target::Local { span, .. } => {
            // For local bindings, search within the function body
            for matched in top_mod.references_to_target(db, target) {
                if let Some(resolved) = matched.span.resolve(db)
                    && let Some(ref_url) = resolved.file.url(db)
                    && let Ok(range) = to_lsp_range_from_span(resolved, db)
                {
                    changes.entry(ref_url).or_default().push(TextEdit {
                        range,
                        new_text: new_name.to_string(),
                    });
                }
            }

            // Also rename the definition itself (the binding site)
            if let Some(resolved) = span.resolve(db)
                && let Some(def_url) = resolved.file.url(db)
                && let Ok(range) = to_lsp_range_from_span(resolved, db)
            {
                changes.entry(def_url).or_default().push(TextEdit {
                    range,
                    new_text: new_name.to_string(),
                });
            }
        }
    }

    RenameOutcome::Edits(changes)
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
