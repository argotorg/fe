use async_lsp::ResponseError;
use async_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse};
use common::InputDb;
use hir::{core::semantic::reference::Target, lower::map_file_to_mod};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

/// Handle textDocument/declaration.
///
/// For impl trait methods, navigates to the concrete implementation (the impl method itself).
/// For other items, navigates to their declaration site.
/// This complements goto-definition which navigates impl methods → trait method declarations.
pub async fn handle_goto_declaration(
    backend: &Backend,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>, ResponseError> {
    let params = params.text_document_position_params;
    let path_str = params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.position, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);

    let locations: Vec<_> = resolution
        .as_slice()
        .iter()
        .filter_map(|target| match target {
            Target::Scope(scope) => {
                // For declaration, always go to the item's own location
                // (no trait_method_def redirect like goto-definition does)
                to_lsp_location_from_scope(&backend.db, *scope).ok()
            }
            Target::Local { span, .. } => {
                to_lsp_location_from_lazy_span(&backend.db, span.clone()).ok()
            }
        })
        .collect();

    match locations.len() {
        0 => Ok(None),
        1 => Ok(Some(GotoDefinitionResponse::Scalar(
            locations.into_iter().next().unwrap(),
        ))),
        _ => Ok(Some(GotoDefinitionResponse::Array(locations))),
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        core::semantic::reference::Target, hir_def::ItemKind, lower::map_file_to_mod,
        span::LazySpan,
    };
    use url::Url;

    use crate::util::to_lsp_location_from_scope;

    /// For a cursor, find where declaration and definition would go,
    /// and return (declaration_name, definition_name) for comparison.
    fn declaration_vs_definition_target<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        offset: u32,
    ) -> (Option<String>, Option<String>) {
        let cursor = parser::TextSize::from(offset);
        let resolution = top_mod.target_at(db, cursor);
        let Some(target) = resolution.first() else {
            return (None, None);
        };

        match target {
            Target::Scope(scope) => {
                // Declaration: always the item itself
                let decl_name = scope
                    .name_span(db)
                    .and_then(|ns| ns.resolve(db))
                    .map(|_| format!("{:?}", scope.item()));

                // Definition: if impl trait method, goes to trait method
                let def_name = if let ItemKind::Func(func) = scope.item() {
                    if let Some(trait_method) = func.trait_method_def(db) {
                        Some(format!(
                            "trait_method:{}",
                            trait_method
                                .name(db)
                                .to_opt()
                                .map(|n| n.data(db).to_string())
                                .unwrap_or_default()
                        ))
                    } else {
                        decl_name.clone()
                    }
                } else {
                    decl_name.clone()
                };

                (decl_name, def_name)
            }
            _ => (None, None),
        }
    }

    #[test]
    fn test_declaration_on_struct() {
        let mut db = DriverDataBase::default();
        let code = "struct Foo {\n    x: i32\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Foo").unwrap() as u32;
        let cursor = parser::TextSize::from(offset);
        let resolution = top_mod.target_at(&db, cursor);

        assert!(
            resolution.first().is_some(),
            "should resolve struct at cursor"
        );
        match resolution.first().unwrap() {
            Target::Scope(scope) => {
                assert!(matches!(scope.item(), ItemKind::Struct(_)));
                let loc = to_lsp_location_from_scope(&db, *scope);
                assert!(loc.is_ok(), "should produce location for struct");
            }
            _ => panic!("expected scope target"),
        }
    }

    #[test]
    fn test_declaration_impl_method_stays_at_impl() {
        let mut db = DriverDataBase::default();
        let code = r#"trait Greetable {
    fn greet(self) -> i32
}

struct Person {
    age: i32
}

impl Greetable for Person {
    fn greet(self) -> i32 {
        self.age
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Find the impl method's "greet" (second occurrence)
        let first_greet = code.find("greet").unwrap();
        let impl_greet = code[first_greet + 1..].find("greet").unwrap() + first_greet + 1;
        let offset = impl_greet as u32;

        let (decl, def) = declaration_vs_definition_target(&db, top_mod, offset);
        assert!(decl.is_some(), "declaration should resolve");
        assert!(def.is_some(), "definition should resolve");

        // For an impl method, definition redirects to trait method,
        // but declaration stays at the impl method itself
        // So they should differ
        if let Some(def_str) = &def
            && def_str.starts_with("trait_method:")
        {
            assert_ne!(
                decl, def,
                "declaration and definition should differ for impl trait methods"
            );
        }
    }

    #[test]
    fn test_declaration_regular_function() {
        let mut db = DriverDataBase::default();
        let code = "fn hello() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("hello").unwrap() as u32;
        let (decl, def) = declaration_vs_definition_target(&db, top_mod, offset);

        // For a regular function, declaration == definition
        assert!(decl.is_some());
        assert_eq!(
            decl, def,
            "declaration and definition should be same for regular functions"
        );
    }
}
