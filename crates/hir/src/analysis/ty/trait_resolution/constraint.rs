use crate::core::hir_def::{
    GenericParam, GenericParamOwner, GenericParamView, ItemKind, TraitRefId, TypeBound,
    WhereClauseOwner, scope_graph::ScopeId, types::TypeId as HirTypeId,
};
use common::indexmap::IndexSet;
use either::Either;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        adt_def::{AdtDef},
        binder::Binder,
        func_def::HirFuncDefKind,
        trait_def::{TraitDef, TraitInstId},
        trait_lower::{lower_impl_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
};

/// Returns a constraints list which is derived from the given type.
#[salsa::tracked]
pub(crate) fn ty_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> PredicateListId<'db> {
    let (base, args) = ty.decompose_ty_app(db);
    let (params, base_constraints) = match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => (adt.params(db), collect_adt_constraints(db, *adt)),
        TyData::TyBase(TyBase::Func(func_def)) => (
            func_def.params(db),
            collect_func_def_constraints(db, func_def.hir_def(db), true),
        ),
        _ => {
            return PredicateListId::empty_list(db);
        }
    };

    let mut args = args.to_vec();

    // Generalize unbound type parameters.
    for &arg in params.iter().skip(args.len()) {
        let key = InferenceKey(args.len() as u32, Default::default());
        let ty_var = TyId::ty_var(db, TyVarSort::General, arg.kind(db).clone(), key);
        args.push(ty_var);
    }

    base_constraints.instantiate(db, &args)
}

/// Collect super traits of the given trait.
/// The returned trait ref is bound by the given trait's generic parameters.
#[salsa::tracked(return_ref)]
pub(crate) fn collect_super_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
) -> IndexSet<Binder<TraitInstId<'db>>> {
    let hir_trait = trait_.trait_(db);
    let self_param = trait_.self_param(db);
    let scope = trait_.trait_(db).scope();

    // Use the trait's own constraints as assumptions when lowering super traits
    let assumptions = collect_constraints(db, hir_trait.into()).instantiate_identity();

    let mut super_traits = IndexSet::new();
    for &super_ in hir_trait.super_traits(db).iter() {
        if let Ok(inst) = lower_trait_ref(db, self_param, super_, scope, assumptions) {
            super_traits.insert(Binder::bind(inst));
        }
    }

    for pred in WhereClauseOwner::Trait(hir_trait).clause(db).predicates(db) {
        if !pred.is_self_subject(db) {
            continue;
        }
        for bound in pred.bounds(db) {
            if let Some(inst) = bound.as_trait_inst_with_subject(db, self_param) {
                super_traits.insert(Binder::bind(inst));
            }
        }
    }

    super_traits
}

#[salsa::tracked(return_ref)]
pub fn super_trait_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
) -> Option<Vec<TraitDef<'db>>> {
    super_trait_cycle_impl(db, trait_, &[])
}

pub fn super_trait_cycle_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
    chain: &[TraitDef<'db>],
) -> Option<Vec<TraitDef<'db>>> {
    if chain.contains(&trait_) {
        return Some(chain.to_vec());
    }
    let bounds = collect_super_traits(db, trait_);
    if bounds.is_empty() {
        return None;
    }

    let chain = [chain, &[trait_]].concat();
    for t in bounds {
        if let Some(cycle) = super_trait_cycle_impl(db, t.skip_binder().def(db), &chain)
            && cycle.contains(&trait_)
        {
            return Some(cycle.clone());
        }
    }
    None
}

/// Collect constraints that are specified by the given ADT definition.
pub(crate) fn collect_adt_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> Binder<PredicateListId<'db>> {
    let Some(owner) = adt.as_generic_param_owner(db) else {
        return Binder::bind(PredicateListId::empty_list(db));
    };
    collect_constraints(db, owner)
}

#[salsa::tracked]
pub(crate) fn collect_func_def_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    func: HirFuncDefKind<'db>,
    include_parent: bool,
) -> Binder<PredicateListId<'db>> {
    let hir_func = match func {
        HirFuncDefKind::Func(func) => func,
        HirFuncDefKind::VariantCtor(var) => {
            let adt = var.enum_.as_adt(db);
            if include_parent {
                return collect_adt_constraints(db, adt);
            } else {
                return Binder::bind(PredicateListId::empty_list(db));
            }
        }
    };

    let func_constraints = collect_constraints(db, hir_func.into());
    if !include_parent {
        return func_constraints;
    }

    let parent_constraints = match hir_func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => collect_constraints(db, trait_.into()),

        Some(ItemKind::Impl(impl_)) => collect_constraints(db, impl_.into()),

        Some(ItemKind::ImplTrait(impl_trait)) => {
            // Only include constraints if the impl trait lowers successfully
            if lower_impl_trait(db, impl_trait).is_none() {
                return func_constraints;
            }
            collect_constraints(db, impl_trait.into())
        }

        _ => return func_constraints,
    };

    Binder::bind(
        func_constraints
            .instantiate_identity()
            .merge(db, parent_constraints.instantiate_identity()),
    )
}

#[salsa::tracked]
pub fn collect_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Binder<PredicateListId<'db>> {
    let mut deferred: Vec<Deferred<'db>> = Vec::new();
    let owner_scope = owner.scope();

    // Generic parameter bounds
    let param_set = collect_generic_params(db, owner);
    let params = owner.params(db);
    for (idx, GenericParamView { param, .. }) in params.enumerate() {
        let GenericParam::Type(hir_param) = param else {
            continue;
        };
        let ty = param_set.param_by_original_idx(db, idx).unwrap();
        for bound in &hir_param.bounds {
            if let TypeBound::Trait(trait_ref) = bound {
                deferred.push(Deferred {
                    bound_ty: Either::Right(ty),
                    trait_ref: *trait_ref,
                    scope: owner_scope,
                });
            }
        }
    }

    // Where-clause predicates (via semantic view)
    if let Some(w_owner) = owner.where_clause_owner() {
        for w_pred in w_owner.clause(db).id.data(db) {
            let Some(hir_ty) = w_pred.ty.to_opt() else {
                continue;
            };
            for bound in &w_pred.bounds {
                // Filter out super trait constraints; handled in `collect_super_traits`
                if hir_ty.is_self_ty(db) && matches!(owner, GenericParamOwner::Trait(_)) {
                    continue;
                }

                if let TypeBound::Trait(trait_ref) = bound {
                    deferred.push(Deferred {
                        bound_ty: Either::Left(hir_ty),
                        trait_ref: *trait_ref,
                        scope: owner_scope,
                    });
                }
            }
        }
    }

    let mut all_predicates: IndexSet<TraitInstId<'db>> = IndexSet::new();

    // fixed-point iteration over deferred predicates
    while !deferred.is_empty() {
        let assumptions =
            PredicateListId::new(db, all_predicates.iter().copied().collect::<Vec<_>>());

        let before = deferred.len();
        deferred.retain(|p| match try_resolve_type_bound(db, p, assumptions) {
            Some(inst) => {
                all_predicates.insert(inst);
                false
            }
            None => true,
        });
        if deferred.len() == before {
            return Binder::bind(assumptions);
        }
    }

    Binder::bind(PredicateListId::new(
        db,
        all_predicates.into_iter().collect::<Vec<_>>(),
    ))
}

struct Deferred<'db> {
    bound_ty: Either<HirTypeId<'db>, TyId<'db>>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
}

fn try_resolve_type_bound<'db>(
    db: &'db dyn HirAnalysisDb,
    deferred: &Deferred<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TraitInstId<'db>> {
    let ty = match deferred.bound_ty {
        Either::Left(hir_ty) => {
            let ty = lower_hir_ty(db, hir_ty, deferred.scope, assumptions);
            if ty.has_invalid(db) {
                return None;
            }
            ty
        }
        Either::Right(ty) => ty,
    };

    lower_trait_ref(db, ty, deferred.trait_ref, deferred.scope, assumptions).ok()
}
