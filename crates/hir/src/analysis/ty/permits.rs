//! Bounded canonical establishment over ordinary resolved identities.
use super::{
    corelib::resolve_lib_type_path,
    trait_def::TraitInstId,
    trait_resolution::PredicateListId,
    ty_def::{TyData, TyId},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{PathRes, resolve_path},
    },
    hir_def::{Const, HirIngot, ImplTrait, ItemKind},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PermitKind {
    Authority,
    Permit,
}

pub fn permit_kind<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<PermitKind> {
    let (base, _) = ty.decompose_ty_app(db);
    let scope = base.adt_def(db)?.adt_ref(db).scope();
    if base.ingot(db)?.kind(db) != common::ingot::IngotKind::Core {
        return None;
    }
    [
        (PermitKind::Authority, "core::permits::PermitAuthority"),
        (PermitKind::Permit, "core::permits::ImplPermit"),
    ]
    .into_iter()
    .find_map(|(kind, path)| {
        (resolve_lib_type_path(db, scope, path)?.base_ty(db) == base).then_some(kind)
    })
}

pub fn is_root_authority<'db>(db: &'db dyn HirAnalysisDb, root: Const<'db>) -> bool {
    let ingot = root.top_mod(db).ingot(db);
    matches!(
        ingot.kind(db),
        common::ingot::IngotKind::Local | common::ingot::IngotKind::StandAlone
    ) && root.scope().parent(db) == Some(ingot.root_mod(db).scope())
        && ItemKind::Const(root).attrs(db).is_some_and(|attrs| {
            let markers: Vec<_> = attrs
                .data(db)
                .iter()
                .filter_map(|a| {
                    let crate::hir_def::Attr::Normal(a) = a else {
                        return None;
                    };
                    (a.path.to_opt()?.as_ident(db)?.data(db) == "permit_root").then_some(a)
                })
                .collect();
            matches!(markers.as_slice(), [a] if !a.has_value && !a.has_args)
        })
        && permit_kind(db, root.ty(db)) == Some(PermitKind::Authority)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CanonicalEstablishment<'db> {
    pub goal: TraitInstId<'db>,
    pub home: ImplTrait<'db>,
    pub root: Const<'db>,
}

/// Prototype attribute bridge; no new permit registry, solver or context.
pub fn establishment_for_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    home: ImplTrait<'db>,
    goal: TraitInstId<'db>,
) -> Result<Option<CanonicalEstablishment<'db>>, &'static str> {
    if !ItemKind::Trait(goal.def(db))
        .attrs(db)
        .is_some_and(|a| a.has_attr(db, "canonical"))
    {
        return Ok(None);
    }
    if home.selection_name(db).is_some() {
        return Err("a canonical goal accepts only its ordinary home provision");
    }
    let attrs = ItemKind::ImplTrait(home)
        .attrs(db)
        .expect("impl attributes");
    let matching: Vec<_> = attrs
        .data(db)
        .iter()
        .filter_map(|a| {
            let crate::hir_def::Attr::Normal(a) = a else {
                return None;
            };
            (a.path.to_opt()?.as_ident(db)?.data(db) == "permit").then_some(a)
        })
        .collect();
    let [attr] = matching.as_slice() else {
        return Err("canonical establishment requires exactly one root authority annotation");
    };
    if attr.has_value || !attr.has_args || attr.args.len() != 1 || attr.args[0].has_value {
        return Err("expected prototype #[permit(ROOT)] with one ordinary constant path");
    }
    let Some(path) = attr.args[0].key.to_opt() else {
        return Err("invalid root authority path");
    };
    let Ok(PathRes::Const(root, ty)) = resolve_path(
        db,
        path,
        home.scope(),
        PredicateListId::empty_list(db),
        true,
    ) else {
        return Err("root authority must resolve to an immutable constant");
    };
    if !crate::analysis::name_resolution::is_scope_visible_from(db, root.scope(), home.scope()) {
        return Err("root authority is not visible here");
    }
    if permit_kind(db, ty) != Some(PermitKind::Authority) {
        return Err(
            "root authority must be the compiler-designated core::permits::PermitAuthority",
        );
    }
    // A local path dependency can itself have IngotKind::Local. The bridge
    // is confined to the declaration's resolved owning ingot; program-wide
    // root injection into dependency declarations is an explicit missing API.
    if root.top_mod(db).ingot(db) != home.top_mod(db).ingot(db) || !is_root_authority(db, root) {
        return Err(
            "authority must be designated by the current program root, not a foreign or nested root",
        );
    }
    let (_, args) = ty.decompose_ty_app(db);
    if !matches!(args, [term] if matches!(term.data(db), TyData::GoalDescriptor(granted) if *granted == goal))
    {
        return Err("root authority authorizes a different resolved trait goal");
    }
    if root.body(db).to_opt().is_none()
        || !crate::analysis::ty::ty_check::check_const_body(db, root)
            .0
            .is_empty()
        || !crate::analysis::ty::ty_check::check_const_value(db, root).is_empty()
    {
        return Err("root authority constant must be a valid compile-time value");
    }
    Ok(Some(CanonicalEstablishment { goal, home, root }))
}
