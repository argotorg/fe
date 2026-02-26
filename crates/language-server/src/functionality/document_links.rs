use async_lsp::ResponseError;
use async_lsp::lsp_types::{DocumentLink, DocumentLinkParams};
use common::InputDb;
use hir::{lower::map_file_to_mod, semantic::reference::resolved_item_scope_targets, span::LazySpan};

use crate::backend::Backend;

/// Provide document links for all scope references in the given file.
///
/// Each link points to the HTTP docs page for the referenced item.
/// Links are only emitted when the documentation server URL is configured.
pub async fn handle_document_links(
    backend: &Backend,
    params: DocumentLinkParams,
) -> Result<Option<Vec<DocumentLink>>, ResponseError> {
    let lsp_uri = params.text_document.uri.clone();
    let Some(base_url) = backend.docs_url.clone() else {
        return Ok(None);
    };
    if backend.is_virtual_uri(&lsp_uri) {
        return Ok(None);
    }

    let internal_url = backend.map_client_uri_to_internal(lsp_uri);
    if backend.db.workspace().get(&backend.db, &internal_url).is_none() {
        return Ok(None);
    }

    let rx = backend.spawn_on_workers(move |db| {
        let Some(file) = db.workspace().get(db, &internal_url) else {
            return vec![];
        };
        let top_mod = map_file_to_mod(db, file);
        let mut links = Vec::new();

        for item in top_mod.scope_graph(db).items_dfs(db) {
            for resolved in resolved_item_scope_targets(db, item) {
                if resolved.is_self_ty {
                    continue;
                }
                let Some(doc_path) = hir::semantic::scope_to_doc_path(db, resolved.scope) else {
                    continue;
                };
                let Some(span) = resolved.span.resolve(db) else {
                    continue;
                };
                let Ok(range) = crate::util::to_lsp_range_from_span(span, db) else {
                    continue;
                };

                let url_str = format!("{base_url}#{doc_path}");
                let Ok(target_uri) = url::Url::parse(&url_str) else {
                    continue;
                };

                links.push(DocumentLink {
                    range,
                    target: Some(target_uri),
                    tooltip: Some(format!("Open docs: {doc_path}")),
                    data: None,
                });
            }
        }
        links
    });

    let links = rx.await.map_err(|_| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            "Worker task cancelled".to_string(),
        )
    })?;

    if links.is_empty() {
        Ok(None)
    } else {
        Ok(Some(links))
    }
}
