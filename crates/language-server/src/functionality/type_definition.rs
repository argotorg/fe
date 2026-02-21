use async_lsp::ResponseError;
use common::InputDb;
use hir::{core::semantic::reference::Target, lower::map_file_to_mod};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_offset_from_position},
};

use super::goto::Cursor;

pub async fn handle_goto_type_definition(
    backend: &Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    let internal_url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );

    let Some(file) = backend.db.workspace().get(&backend.db, &internal_url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);

    // Get the target at cursor (handles references, definitions, and bindings)
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    // For local bindings, go to the type definition
    let location = match target {
        Target::Local { ty, .. } => {
            // Get the span of the type's definition
            ty.name_span(&backend.db)
                .and_then(|name_span| to_lsp_location_from_lazy_span(&backend.db, name_span).ok())
        }
        Target::Scope(_) => {
            // For scopes, go-to-type-definition doesn't make sense
            // (you're already on a type/function definition)
            None
        }
    };

    Ok(location
        .map(|mut location| {
            location.uri = backend.map_internal_uri_to_client(location.uri);
            location
        })
        .map(async_lsp::lsp_types::GotoDefinitionResponse::Scalar))
}
