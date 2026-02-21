use async_lsp::ResponseError;
use async_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location};
use common::InputDb;
use hir::{core::semantic::reference::Target, hir_def::ItemKind, lower::map_file_to_mod};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_scope, to_offset_from_position},
};

/// Handle textDocument/implementation request.
///
/// For traits: returns all `impl Trait for Type` blocks
/// For trait methods: returns all implementations of that method
pub async fn handle_goto_implementation(
    backend: &Backend,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>, ResponseError> {
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
    let cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);

    // Get the target at cursor
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let locations = match target {
        Target::Scope(scope) => find_implementations(&backend.db, scope.item()),
        Target::Local { .. } => {
            // Local variables don't have implementations
            vec![]
        }
    }
    .into_iter()
    .map(|mut location| {
        location.uri = backend.map_internal_uri_to_client(location.uri);
        location
    })
    .collect::<Vec<_>>();

    match locations.len() {
        0 => Ok(None),
        1 => Ok(Some(GotoDefinitionResponse::Scalar(
            locations.into_iter().next().unwrap(),
        ))),
        _ => Ok(Some(GotoDefinitionResponse::Array(locations))),
    }
}

/// Find implementations for the given item.
fn find_implementations<'db>(
    db: &'db driver::DriverDataBase,
    item: ItemKind<'db>,
) -> Vec<Location> {
    match item {
        ItemKind::Trait(trait_) => trait_
            .all_impl_traits(db)
            .into_iter()
            .filter_map(|it| to_lsp_location_from_scope(db, it.scope()).ok())
            .collect(),
        ItemKind::Func(func) => {
            let Some(trait_) = func
                .containing_trait(db)
                .or_else(|| func.containing_impl_trait(db)?.trait_def(db))
            else {
                return vec![];
            };
            let Some(method_name) = func.name(db).to_opt() else {
                return vec![];
            };
            trait_
                .method_implementations(db, method_name)
                .into_iter()
                .filter_map(|m| to_lsp_location_from_scope(db, m.scope()).ok())
                .collect()
        }
        // For structs/enums/contracts: find all impl blocks (both inherent and trait)
        ItemKind::Struct(s) => s
            .all_impls(db)
            .into_iter()
            .map(|i| i.scope())
            .chain(s.all_impl_traits(db).into_iter().map(|i| i.scope()))
            .filter_map(|scope| to_lsp_location_from_scope(db, scope).ok())
            .collect(),
        ItemKind::Enum(e) => e
            .all_impls(db)
            .into_iter()
            .map(|i| i.scope())
            .chain(e.all_impl_traits(db).into_iter().map(|i| i.scope()))
            .filter_map(|scope| to_lsp_location_from_scope(db, scope).ok())
            .collect(),
        _ => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    #[test]
    fn test_goto_implementation_trait() {
        let mut db = DriverDataBase::default();

        let code = r#"
trait Display {
    fn show(self) -> i32
}

struct Counter {
    value: i32
}

impl Display for Counter {
    fn show(self) -> i32 {
        self.value
    }
}
"#;

        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Position cursor on "Display" in "trait Display"
        // Line 1 (0-indexed): "trait Display {"
        // "Display" starts at column 6
        let cursor = parser::TextSize::from(7); // After "trait D"

        // Get the target at cursor
        let resolution = top_mod.target_at(&db, cursor);

        if let Some(Target::Scope(scope)) = resolution.first() {
            let locations = find_implementations(&db, scope.item());
            assert!(
                !locations.is_empty(),
                "Should find impl Display for Counter"
            );
        } else {
            panic!(
                "Expected Target::Scope for trait name, got {:?}",
                resolution
            );
        }
    }
}
