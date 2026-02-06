use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    SymbolKind, TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams,
};
use common::InputDb;
use hir::{
    analysis::ty::adt_def::AdtRef,
    core::semantic::reference::Target,
    hir_def::ItemKind,
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

/// Handle textDocument/prepareTypeHierarchy.
pub async fn handle_prepare(
    backend: &Backend,
    params: TypeHierarchyPrepareParams,
) -> Result<Option<Vec<TypeHierarchyItem>>, ResponseError> {
    let path_str = params
        .text_document_position_params
        .text_document
        .uri
        .path();

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

    Ok(item.map(|i| vec![i]))
}

/// Handle typeHierarchy/supertypes.
pub async fn handle_supertypes(
    backend: &Backend,
    params: TypeHierarchySupertypesParams,
) -> Result<Option<Vec<TypeHierarchyItem>>, ResponseError> {
    let path_str = params.item.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.item.selection_range.start,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let items: Vec<TypeHierarchyItem> = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Struct(s) => s
                .all_impl_traits(&backend.db)
                .into_iter()
                .filter_map(|it| {
                    let trait_ = it.trait_def(&backend.db)?;
                    trait_to_hierarchy_item(&backend.db, trait_)
                })
                .collect(),
            ItemKind::Enum(e) => e
                .all_impl_traits(&backend.db)
                .into_iter()
                .filter_map(|it| {
                    let trait_ = it.trait_def(&backend.db)?;
                    trait_to_hierarchy_item(&backend.db, trait_)
                })
                .collect(),
            ItemKind::Trait(trait_) => trait_
                .super_trait_bounds(&backend.db)
                .filter_map(|inst| {
                    let super_trait = inst.def(&backend.db);
                    trait_to_hierarchy_item(&backend.db, super_trait)
                })
                .collect(),
            _ => vec![],
        },
        _ => vec![],
    };

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
    let path_str = params.item.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.item.selection_range.start,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let items: Vec<TypeHierarchyItem> = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Trait(trait_) => trait_
                .all_impl_traits(&backend.db)
                .into_iter()
                .filter_map(|it| {
                    let adt = it.implementing_adt(&backend.db)?;
                    adt_ref_to_hierarchy_item(&backend.db, adt)
                })
                .collect(),
            _ => vec![],
        },
        _ => vec![],
    };

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(items))
    }
}
