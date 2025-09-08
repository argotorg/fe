use async_lsp::ResponseError;
use common::InputDb;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};
// use tracing::error;

use crate::{backend::Backend, util::to_offset_from_position};
// Note: DriverDataBase and tracing are only used in tests below.
pub type Cursor = parser::TextSize;

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Convert the position to an offset in the file using the workspace's current content
    let params = params.text_document_position_params;
    // Use URI directly to avoid path/encoding/case issues
    let url = params.text_document.uri.clone();
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in index: {url}"),
            )
        })?;
    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Use unified SemanticQuery API
    let mut locs: Vec<async_lsp::lsp_types::Location> = Vec::new();
    let query = SemanticQuery::at_cursor(&backend.db, top_mod, cursor);
    let candidates = query.goto_definition();
    for def in candidates.into_iter() {
        if let Some(span) = def.span.resolve(&backend.db) {
            let url = span.file.url(&backend.db).expect("Failed to get file URL");
            let range = crate::util::to_lsp_range_from_span(span, &backend.db).map_err(|e| {
                ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("{e}"))
            })?;
            locs.push(async_lsp::lsp_types::Location { uri: url, range });
        }
    }
    match locs.len() {
        0 => Ok(None),
        1 => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Scalar(
            locs.remove(0),
        ))),
        _ => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locs,
        ))),
    }
}
