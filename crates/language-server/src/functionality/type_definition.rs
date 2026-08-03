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
    let location = type_definition_location(&backend.db, target);

    Ok(location
        .map(|mut location| {
            location.uri = backend.map_internal_uri_to_client(location.uri);
            location
        })
        .map(async_lsp::lsp_types::GotoDefinitionResponse::Scalar))
}

fn type_definition_location(
    db: &driver::DriverDataBase,
    target: &Target<'_>,
) -> Option<async_lsp::lsp_types::Location> {
    match target {
        Target::Local { ty, .. } => {
            let payload = ty.as_capability(db).map_or(*ty, |(_, payload)| payload);
            payload
                .as_closure(db)
                .map(|closure| {
                    let def = closure.def(db);
                    def.expr.span(def.body).into()
                })
                .or_else(|| ty.name_span(db))
                .and_then(|name_span| to_lsp_location_from_lazy_span(db, name_span).ok())
        }
        Target::Scope(_) => {
            // For scopes, go-to-type-definition doesn't make sense
            // (you're already on a type/function definition)
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    #[test]
    fn closure_type_definition_is_its_literal() {
        let mut db = DriverDataBase::default();
        let source = r#"fn main() -> u256 {
    let add = |value: own u256| -> u256 { value + 1 }
    add(41)
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let call_offset = source.rfind("add(41)").unwrap() as u32;
        let resolution = top_mod.target_at(&db, parser::TextSize::from(call_offset));
        let location =
            type_definition_location(&db, resolution.first().expect("closure binding target"))
                .expect("closure literal location");
        assert_eq!(location.range.start.line, 1);
        assert_eq!(location.range.start.character, 14);
    }
}
