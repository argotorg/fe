use crate::core::hir_def::{ItemKind, Use, scope_graph::ScopeId};

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::ConstTyData,
        ty_def::{TyBase, TyData, TyId},
    },
};

/// Compute the "defining scope" for visibility purposes.
///
/// For a private item, this is the scope within which the item is visible.
/// - For associated functions: the parent of the parent item (i.e. the module containing the impl/trait)
/// - For trait members: always visible (returns `None` to signal "always visible")
/// - For fields/variants: the parent of the containing type
/// - For other items: the direct parent scope
fn def_scope_for_vis<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<ScopeId<'db>> {
    match scope {
        ScopeId::Item(ItemKind::Func(func)) => {
            if func.is_associated_func(db) {
                scope
                    .parent_item(db)
                    .and_then(|item| ScopeId::Item(item).parent(db))
            } else {
                scope.parent(db)
            }
        }
        ScopeId::Item(_) => scope.parent(db),
        // Like associated functions: private consts are visible within the
        // module containing the impl, not just inside the impl block.
        ScopeId::ImplConst(..) | ScopeId::Field(..) | ScopeId::Variant(..) => {
            let parent_item = scope.item();
            ScopeId::Item(parent_item).parent(db)
        }
        _ => scope.parent(db),
    }
}

/// Return `true` if the given `scope` is visible from `from_scope`.
pub fn is_scope_visible_from(db: &dyn HirAnalysisDb, scope: ScopeId, from_scope: ScopeId) -> bool {
    use crate::core::hir_def::item::Visibility;

    let vis = scope.data(db).vis;

    // Fast path: unrestricted `pub` is visible everywhere.
    if vis.is_pub() {
        return true;
    }

    // Trait members are always visible regardless of their declared visibility.
    if matches!(scope, ScopeId::Item(ItemKind::Func(_)))
        && matches!(scope.parent_item(db), Some(ItemKind::Trait(..)))
    {
        return true;
    }

    match vis {
        Visibility::Public => unreachable!(),

        Visibility::PubIngot => {
            // Visible within the same ingot.
            scope.ingot(db) == from_scope.ingot(db)
        }

        Visibility::PubSuper => {
            // Visible within the grandparent scope (parent of the defining module).
            let Some(def_scope) = def_scope_for_vis(db, scope) else {
                return false;
            };
            match def_scope.parent(db) {
                Some(parent_of_def) => from_scope.is_transitive_child_of(db, parent_of_def),
                // At the ingot root, pub(super) behaves like pub(ingot)
                // (matches Rust where pub(super) at crate root = pub(crate)).
                None => scope.ingot(db) == from_scope.ingot(db),
            }
        }

        Visibility::Private => {
            // Visible within the defining scope only.
            let Some(def_scope) = def_scope_for_vis(db, scope) else {
                return false;
            };
            from_scope.is_transitive_child_of(db, def_scope)
        }
    }
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
            ConstTyData::Hole(..) => true,
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
    is_scope_visible_from(db, use_scope, ref_scope)
}
