use async_lsp::ResponseError;
use async_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location};
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::{HirIngot, ItemKind, Trait, scope_graph::ScopeId},
    lower::map_file_to_mod,
};

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
    let path_str = params.text_document_position_params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);

    // Get the target at cursor
    let Some(target) = top_mod.target_at(&backend.db, cursor) else {
        return Ok(None);
    };

    let locations = match &target {
        Target::Scope(scope) => find_implementations(&backend.db, *scope),
        Target::Local { .. } => {
            // Local variables don't have implementations
            vec![]
        }
    };

    match locations.len() {
        0 => Ok(None),
        1 => Ok(Some(GotoDefinitionResponse::Scalar(
            locations.into_iter().next().unwrap(),
        ))),
        _ => Ok(Some(GotoDefinitionResponse::Array(locations))),
    }
}

/// Find implementations for the given scope.
fn find_implementations<'db>(
    db: &'db driver::DriverDataBase,
    scope: ScopeId<'db>,
) -> Vec<Location> {
    match scope.item() {
        ItemKind::Trait(trait_) => find_trait_implementations(db, trait_),
        ItemKind::Func(func) => {
            // Check if this function is a trait method
            if let Some(ScopeId::Item(ItemKind::Trait(trait_))) = scope.parent(db) {
                find_trait_method_implementations(db, trait_, func.name(db))
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Find all `impl Trait for Type` blocks for the given trait.
fn find_trait_implementations<'db>(
    db: &'db driver::DriverDataBase,
    trait_: Trait<'db>,
) -> Vec<Location> {
    let ingot = trait_.top_mod(db).ingot(db);

    let mut locations = vec![];

    // Get all impl traits in the ingot and filter for those implementing this trait
    for impl_trait in ingot.all_impl_traits(db) {
        if let Some(implemented_trait) = impl_trait.trait_def(db)
            && implemented_trait == trait_
            && let Ok(location) = to_lsp_location_from_scope(db, impl_trait.scope())
        {
            locations.push(location);
        }
    }

    locations
}

/// Find all implementations of a specific trait method.
fn find_trait_method_implementations<'db>(
    db: &'db driver::DriverDataBase,
    trait_: Trait<'db>,
    method_name: hir::hir_def::Partial<hir::hir_def::IdentId<'db>>,
) -> Vec<Location> {
    let Some(method_name) = method_name.to_opt() else {
        return vec![];
    };

    let ingot = trait_.top_mod(db).ingot(db);

    let mut locations = vec![];

    // Get all impl traits in the ingot and filter for those implementing this trait
    for impl_trait in ingot.all_impl_traits(db) {
        if let Some(implemented_trait) = impl_trait.trait_def(db)
            && implemented_trait == trait_
        {
            // Find the method in this impl block
            for method in impl_trait.methods(db) {
                if method.name(db).to_opt() == Some(method_name)
                    && let Ok(location) = to_lsp_location_from_scope(db, method.scope())
                {
                    locations.push(location);
                }
            }
        }
    }

    locations
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
        let target = top_mod.target_at(&db, cursor);

        if let Some(Target::Scope(scope)) = target {
            let locations = find_implementations(&db, scope);
            assert!(!locations.is_empty(), "Should find impl Display for Counter");
        } else {
            panic!("Expected Target::Scope for trait name, got {:?}", target);
        }
    }
}
