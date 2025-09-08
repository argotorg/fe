use async_lsp::ResponseError;
use common::InputDb;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};
use std::collections::HashMap;

use crate::{backend::Backend, util::to_offset_from_position};

pub type Cursor = parser::TextSize;

// Custom LSP request for rename
pub enum Rename {}

impl async_lsp::lsp_types::request::Request for Rename {
    type Params = async_lsp::lsp_types::RenameParams;
    type Result = Option<async_lsp::lsp_types::WorkspaceEdit>;
    const METHOD: &'static str = "textDocument/rename";
}

pub async fn handle_rename(
    backend: &Backend,
    params: async_lsp::lsp_types::RenameParams,
) -> Result<Option<async_lsp::lsp_types::WorkspaceEdit>, ResponseError> {
    // Use URI directly to avoid path/encoding/case issues
    let url = params.text_document_position.text_document.uri.clone();
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
    let cursor: Cursor =
        to_offset_from_position(params.text_document_position.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Use unified SemanticQuery API
    let query = SemanticQuery::at_cursor(&backend.db, top_mod, cursor);
    let rename_locations = query.find_rename_locations();

    if rename_locations.is_empty() {
        return Ok(None);
    }

    // Group edits by file URL
    let mut changes: HashMap<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::TextEdit>> =
        HashMap::new();

    for location in rename_locations {
        if let Some(span) = location.span.resolve(&backend.db) {
            let file_url = span.file.url(&backend.db).expect("Failed to get file URL");
            let range = crate::util::to_lsp_range_from_span(span, &backend.db).map_err(|e| {
                ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("{e}"))
            })?;

            let text_edit = async_lsp::lsp_types::TextEdit {
                range,
                new_text: params.new_name.clone(),
            };

            changes.entry(file_url).or_default().push(text_edit);
        }
    }

    if changes.is_empty() {
        Ok(None)
    } else {
        Ok(Some(async_lsp::lsp_types::WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }))
    }
}
