use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    SymbolKind, TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams,
};
use common::InputDb;
use hir::{
    analysis::ty::adt_def::AdtRef, core::semantic::reference::Target, hir_def::ItemKind,
    lower::map_file_to_mod,
};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

fn trait_to_hierarchy_item(
    db: &driver::DriverDataBase,
    trait_: hir::hir_def::Trait,
) -> Option<TypeHierarchyItem> {
    let location = to_lsp_location_from_scope(db, trait_.scope()).ok()?;
    let name = trait_.name(db).to_opt()?.data(db).to_string();

    let item_span: hir::span::DynLazySpan = trait_.span().into();
    let item_location = to_lsp_location_from_lazy_span(db, item_span).ok()?;

    Some(TypeHierarchyItem {
        name,
        kind: SymbolKind::INTERFACE,
        tags: None,
        detail: None,
        uri: location.uri,
        range: item_location.range,
        selection_range: location.range,
        data: None,
    })
}

fn struct_to_hierarchy_item(
    db: &driver::DriverDataBase,
    struct_: hir::hir_def::Struct,
) -> Option<TypeHierarchyItem> {
    let location = to_lsp_location_from_scope(db, struct_.scope()).ok()?;
    let name = struct_.name(db).to_opt()?.data(db).to_string();

    let item_span: hir::span::DynLazySpan = struct_.span().into();
    let item_location = to_lsp_location_from_lazy_span(db, item_span).ok()?;

    Some(TypeHierarchyItem {
        name,
        kind: SymbolKind::STRUCT,
        tags: None,
        detail: None,
        uri: location.uri,
        range: item_location.range,
        selection_range: location.range,
        data: None,
    })
}

fn enum_to_hierarchy_item(
    db: &driver::DriverDataBase,
    enum_: hir::hir_def::Enum,
) -> Option<TypeHierarchyItem> {
    let location = to_lsp_location_from_scope(db, enum_.scope()).ok()?;
    let name = enum_.name(db).to_opt()?.data(db).to_string();

    let item_span: hir::span::DynLazySpan = enum_.span().into();
    let item_location = to_lsp_location_from_lazy_span(db, item_span).ok()?;

    Some(TypeHierarchyItem {
        name,
        kind: SymbolKind::ENUM,
        tags: None,
        detail: None,
        uri: location.uri,
        range: item_location.range,
        selection_range: location.range,
        data: None,
    })
}

fn adt_ref_to_hierarchy_item(
    db: &driver::DriverDataBase,
    adt: AdtRef,
) -> Option<TypeHierarchyItem> {
    match adt {
        AdtRef::Struct(s) => struct_to_hierarchy_item(db, s),
        AdtRef::Enum(e) => enum_to_hierarchy_item(db, e),
    }
}

/// Find supertypes for an item (traits it implements, or super-traits).
fn find_supertypes(db: &driver::DriverDataBase, item: ItemKind) -> Vec<TypeHierarchyItem> {
    match item {
        ItemKind::Struct(s) => s
            .all_impl_traits(db)
            .into_iter()
            .filter_map(|it| {
                let trait_ = it.trait_def(db)?;
                trait_to_hierarchy_item(db, trait_)
            })
            .collect(),
        ItemKind::Enum(e) => e
            .all_impl_traits(db)
            .into_iter()
            .filter_map(|it| {
                let trait_ = it.trait_def(db)?;
                trait_to_hierarchy_item(db, trait_)
            })
            .collect(),
        ItemKind::Trait(trait_) => trait_
            .super_trait_bounds(db)
            .filter_map(|inst| {
                let super_trait = inst.def(db);
                trait_to_hierarchy_item(db, super_trait)
            })
            .collect(),
        _ => vec![],
    }
}

/// Find subtypes for an item (structs/enums implementing a trait).
fn find_subtypes(db: &driver::DriverDataBase, item: ItemKind) -> Vec<TypeHierarchyItem> {
    match item {
        ItemKind::Trait(trait_) => trait_
            .all_impl_traits(db)
            .into_iter()
            .filter_map(|it| {
                let adt = it.implementing_adt(db)?;
                adt_ref_to_hierarchy_item(db, adt)
            })
            .collect(),
        _ => vec![],
    }
}

/// Handle textDocument/prepareTypeHierarchy.
pub async fn handle_prepare(
    backend: &Backend,
    params: TypeHierarchyPrepareParams,
) -> Result<Option<Vec<TypeHierarchyItem>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let item = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Struct(s) => struct_to_hierarchy_item(&backend.db, s),
            ItemKind::Enum(e) => enum_to_hierarchy_item(&backend.db, e),
            ItemKind::Trait(t) => trait_to_hierarchy_item(&backend.db, t),
            _ => None,
        },
        _ => None,
    };

    Ok(item
        .map(|mut i| {
            i.uri = backend.map_internal_uri_to_client(i.uri);
            i
        })
        .map(|i| vec![i]))
}

/// Handle typeHierarchy/supertypes.
pub async fn handle_supertypes(
    backend: &Backend,
    params: TypeHierarchySupertypesParams,
) -> Result<Option<Vec<TypeHierarchyItem>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.item.uri.clone());
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.item.selection_range.start, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let items = match target {
        Target::Scope(scope) => find_supertypes(&backend.db, scope.item()),
        _ => vec![],
    }
    .into_iter()
    .map(|mut item| {
        item.uri = backend.map_internal_uri_to_client(item.uri);
        item
    })
    .collect::<Vec<_>>();

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(items))
    }
}

/// Handle typeHierarchy/subtypes.
pub async fn handle_subtypes(
    backend: &Backend,
    params: TypeHierarchySubtypesParams,
) -> Result<Option<Vec<TypeHierarchyItem>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.item.uri.clone());
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.item.selection_range.start, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let items = match target {
        Target::Scope(scope) => find_subtypes(&backend.db, scope.item()),
        _ => vec![],
    }
    .into_iter()
    .map(|mut item| {
        item.uri = backend.map_internal_uri_to_client(item.uri);
        item
    })
    .collect::<Vec<_>>();

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(items))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    fn item_at<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        offset: u32,
    ) -> Option<ItemKind<'db>> {
        let cursor = parser::TextSize::from(offset);
        let resolution = top_mod.target_at(db, cursor);
        match resolution.first()? {
            Target::Scope(scope) => Some(scope.item()),
            _ => None,
        }
    }

    #[test]
    fn test_prepare_struct() {
        let mut db = DriverDataBase::default();
        let code = "struct Foo {\n    x: i32\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Foo").unwrap() as u32;
        let item = item_at(&db, top_mod, offset);
        assert!(matches!(item, Some(ItemKind::Struct(_))));

        if let Some(ItemKind::Struct(s)) = item {
            let hierarchy = struct_to_hierarchy_item(&db, s);
            assert!(hierarchy.is_some());
            let h = hierarchy.unwrap();
            assert_eq!(h.name, "Foo");
            assert_eq!(h.kind, SymbolKind::STRUCT);
        }
    }

    #[test]
    fn test_prepare_trait() {
        let mut db = DriverDataBase::default();
        let code = "trait Bar {\n    fn do_thing(self) -> i32\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Bar").unwrap() as u32;
        let item = item_at(&db, top_mod, offset);
        assert!(matches!(item, Some(ItemKind::Trait(_))));

        if let Some(ItemKind::Trait(t)) = item {
            let hierarchy = trait_to_hierarchy_item(&db, t);
            assert!(hierarchy.is_some());
            let h = hierarchy.unwrap();
            assert_eq!(h.name, "Bar");
            assert_eq!(h.kind, SymbolKind::INTERFACE);
        }
    }

    #[test]
    fn test_supertypes_of_struct() {
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

        let offset = code.find("Person").unwrap() as u32;
        let item = item_at(&db, top_mod, offset).expect("should find struct");
        let supertypes = find_supertypes(&db, item);

        let names: Vec<&str> = supertypes.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(names, vec!["Greetable"]);
    }

    #[test]
    fn test_subtypes_of_trait() {
        let mut db = DriverDataBase::default();
        let code = r#"trait Printable {
    fn print(self) -> i32
}

struct Doc {
    pages: i32
}

impl Printable for Doc {
    fn print(self) -> i32 {
        self.pages
    }
}

struct Report {
    lines: i32
}

impl Printable for Report {
    fn print(self) -> i32 {
        self.lines
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Printable").unwrap() as u32;
        let item = item_at(&db, top_mod, offset).expect("should find trait");
        let subtypes = find_subtypes(&db, item);

        let mut names: Vec<&str> = subtypes.iter().map(|s| s.name.as_str()).collect();
        names.sort();
        assert_eq!(names, vec!["Doc", "Report"]);
    }

    #[test]
    fn test_struct_no_supertypes() {
        let mut db = DriverDataBase::default();
        let code = "struct Lonely {\n    x: i32\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Lonely").unwrap() as u32;
        let item = item_at(&db, top_mod, offset).expect("should find struct");
        let supertypes = find_supertypes(&db, item);
        assert!(supertypes.is_empty());
    }

    #[test]
    fn test_trait_no_subtypes() {
        let mut db = DriverDataBase::default();
        let code = "trait Unimplemented {\n    fn nope(self) -> i32\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("Unimplemented").unwrap() as u32;
        let item = item_at(&db, top_mod, offset).expect("should find trait");
        let subtypes = find_subtypes(&db, item);
        assert!(subtypes.is_empty());
    }
}
