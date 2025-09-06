use async_lsp::ResponseError;
use common::InputDb;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};

use crate::{backend::Backend, util::to_offset_from_position};

pub type Cursor = parser::TextSize;

// Custom LSP request for goto implementation
pub enum GotoImplementation {}

impl async_lsp::lsp_types::request::Request for GotoImplementation {
    type Params = async_lsp::lsp_types::TextDocumentPositionParams;
    type Result = Option<async_lsp::lsp_types::GotoDefinitionResponse>;
    const METHOD: &'static str = "textDocument/implementation";
}

pub async fn handle_goto_implementation(
    backend: &Backend,
    params: async_lsp::lsp_types::TextDocumentPositionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
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
    let query = SemanticQuery::at_cursor(&backend.db, top_mod, cursor);
    let implementations = query.find_implementations();

    let mut locs: Vec<async_lsp::lsp_types::Location> = Vec::new();
    for impl_def in implementations {
        if let Some(span) = impl_def.span.resolve(&backend.db) {
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
            locs.into_iter().next().unwrap(),
        ))),
        _ => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locs,
        ))),
    }
}
