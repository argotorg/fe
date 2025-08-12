use common::indexmap::{IndexMap, IndexSet};
use hir::hir_def::{
    scope_graph::ScopeId, GenericParam, GenericParamOwner, ItemKind, TypeBound, WhereClauseId,
};
use itertools::Itertools;

use crate::{
    ty::{
        adt_def::{lower_adt, AdtDef},
        binder::Binder,
        func_def::HirFuncDefKind,
        trait_def::{TraitDef, TraitInstId},
        trait_lower::{lower_impl_trait, lower_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
    HirAnalysisDb,
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

    // xxx use generic param bounds
    let assumptions = PredicateListId::empty_list(db);

    let mut super_traits = IndexSet::new();
    for &super_ in hir_trait.super_traits(db).iter() {
        if let Ok(inst) = lower_trait_ref(db, self_param, super_, scope, assumptions) {
            super_traits.insert(Binder::bind(inst));
        }
    }

    for pred in hir_trait.where_clause(db).data(db) {
        if pred
            .ty
            .to_opt()
            .map(|ty| ty.is_self_ty(db))
            .unwrap_or_default()
        {
            for bound in &pred.bounds {
                if let TypeBound::Trait(bound) = bound {
                    if let Ok(inst) = lower_trait_ref(db, self_param, *bound, scope, assumptions) {
                        super_traits.insert(Binder::bind(inst));
                    }
                }
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
        if let Some(cycle) = super_trait_cycle_impl(db, t.skip_binder().def(db), &chain) {
            if cycle.contains(&trait_) {
                return Some(cycle.clone());
            }
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
            let adt = lower_adt(db, var.enum_.into());
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
            // xxx remove?
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
    let mut predicates = IndexSet::new();

    collect_constraints_from_generic_params(db, owner, &mut predicates);

    if let Some(where_clause) = owner.where_clause(db) {
        collect_constraints_from_where_clause(db, where_clause, owner.scope(), &mut predicates);
    }

    // Collect super traits from the trait definition and add them to the predicate
    // list.
    if let GenericParamOwner::Trait(trait_) = owner {
        let trait_def = lower_trait(db, trait_);
        predicates.insert(TraitInstId::new(
            db,
            trait_def,
            collect_generic_params(db, owner).params(db).to_vec(),
            IndexMap::new(),
        ));
    }

    Binder::bind(PredicateListId::new(
        db,
        predicates.into_iter().collect::<Vec<_>>(),
    ))
}

fn collect_constraints_from_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    predicates: &mut IndexSet<TraitInstId<'db>>,
) {
    let param_set = collect_generic_params(db, owner);
    let param_list = owner.params(db);

    for (i, hir_param) in param_list.data(db).iter().enumerate() {
        let GenericParam::Type(hir_param) = hir_param else {
            continue;
        };

        let ty = param_set.param_by_original_idx(db, i).unwrap();
        let bounds = &hir_param.bounds;
        add_bounds_to_constraint_set(db, owner.scope(), ty, bounds, predicates);
    }
}

fn collect_constraints_from_where_clause<'db>(
    db: &'db dyn HirAnalysisDb,
    where_clause: WhereClauseId<'db>,
    scope: ScopeId<'db>,
    predicates: &mut IndexSet<TraitInstId<'db>>,
) {
    for hir_pred in where_clause.data(db) {
        let Some(hir_ty) = hir_pred.ty.to_opt() else {
            continue;
        };

        // TODO: this isn't sufficient; it requires a specific ordering of constraints.
        //   eg. `where T: A, T::Atype: B` works,
        //   but `where T::Atype: B, T: A` doesn't.
        let assumptions_so_far = PredicateListId::new(db, predicates.iter().copied().collect_vec());
        let ty = lower_hir_ty(db, hir_ty, scope, assumptions_so_far);

        // We don't need to collect super traits, please refer to
        // [`collect_super_traits`] function for details.
        if ty.has_invalid(db) || ty.is_trait_self(db) {
            continue;
        }

        add_bounds_to_constraint_set(db, scope, ty, &hir_pred.bounds, predicates);
    }
}

pub(crate) fn add_bounds_to_constraint_set<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    bound_ty: TyId<'db>,
    bounds: &[TypeBound<'db>],
    set: &mut IndexSet<TraitInstId<'db>>,
) {
    for bound in bounds {
        let TypeBound::Trait(trait_ref) = bound else {
            continue;
        };

        let Ok(trait_inst) = lower_trait_ref(
            db,
            bound_ty,
            *trait_ref,
            scope,
            PredicateListId::empty_list(db),
        ) else {
            continue;
        };

        set.insert(trait_inst);
    }
}
