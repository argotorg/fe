use crate::core::hir_def::{ItemKind, Use, scope_graph::ScopeId};

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::ConstTyData,
        ty_def::{TyBase, TyData, TyId},
    },
};

/// Return `true` if the given `scope` is visible from `from_scope`.
pub(crate) fn is_scope_visible_from(
    db: &dyn HirAnalysisDb,
    scope: ScopeId,
    from_scope: ScopeId,
) -> bool {
    // If resolved is public, then it is visible.
    if scope.data(db).vis.is_pub() {
        return true;
    }

    let Some(def_scope) = (match scope {
        ScopeId::Item(ItemKind::Func(func)) => {
            let parent_item = scope.parent_item(db);
            if matches!(parent_item, Some(ItemKind::Trait(..))) {
                return true;
            }

            if func.is_associated_func(db) {
                scope
                    .parent_item(db)
                    .and_then(|item| ScopeId::Item(item).parent(db))
            } else {
                scope.parent(db)
            }
        }
        ScopeId::Item(_) => scope.parent(db),
        ScopeId::Field(..) | ScopeId::Variant(..) => {
            let parent_item = scope.item();
            ScopeId::Item(parent_item).parent(db)
        }

        _ => scope.parent(db),
    }) else {
        return false;
    };

    from_scope.is_transitive_child_of(db, def_scope)
}

pub(crate) fn is_ty_visible_from(db: &dyn HirAnalysisDb, ty: TyId, from_scope: ScopeId) -> bool {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(base) => match base {
            TyBase::Prim(_) => true,
            TyBase::Adt(adt) => is_scope_visible_from(db, adt.scope(db), from_scope),
            TyBase::Contract(c) => is_scope_visible_from(db, c.scope(), from_scope),
            TyBase::Func(func) => is_scope_visible_from(db, func.scope(), from_scope),
        },
        TyData::TyParam(param) => is_scope_visible_from(db, param.scope(db), from_scope),
        // Associated type projections (e.g., `T::Assoc`, `<T as Trait>::Assoc`) are
        // semantic types, not free type items. They should be usable wherever the
        // projection type itself is well-formed, regardless of the visibility of
        // the associated type declaration inside the trait. Treat them as visible.
        TyData::AssocTy(_assoc_ty) => true,

        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::TyVar(_, _) => true,
            ConstTyData::TyParam(param, _) => {
                is_scope_visible_from(db, param.scope(db), from_scope)
            }
            ConstTyData::Evaluated(_, _) => true,
            ConstTyData::Abstract(_, _) => true,
            ConstTyData::UnEvaluated { body, .. } => {
                is_scope_visible_from(db, body.scope(), from_scope)
            }
        },
        TyData::TyVar(_) | TyData::Never | TyData::Invalid(_) => true,
        TyData::QualifiedTy(trait_inst) => {
            is_scope_visible_from(db, trait_inst.def(db).scope(), from_scope)
        }
        TyData::TyApp(_, _) => unreachable!(),
    }
}

/// Return `true` if the given `use_` is visible from the `ref_scope`.
pub(super) fn is_use_visible(db: &dyn HirAnalysisDb, ref_scope: ScopeId, use_: Use) -> bool {
    let use_scope = ScopeId::from_item(use_.into());

    if use_scope.data(db).vis.is_pub() {
        return true;
    }

    let use_def_scope = use_scope.parent(db).unwrap();
    ref_scope.is_transitive_child_of(db, use_def_scope)
}
