use crate::core::hir_def::{
    GenericParam, GenericParamOwner, GenericParamView, ItemKind, Trait, TraitRefId, TypeBound,
    scope_graph::ScopeId, types::TypeId as HirTypeId,
};
use crate::hir_def::CallableDef;
use common::indexmap::{IndexMap, IndexSet};
use either::Either;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        adt_def::AdtDef,
        binder::Binder,
        corelib::resolve_core_trait,
        effects::{EffectKeyKind, effect_key_kind, place_effect_provider_param_index_map},
        trait_def::TraitInstId,
        trait_lower::{lower_impl_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
};

fn collect_effect_constraints_for_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TraitInstId<'db>> {
    let provider_map = place_effect_provider_param_index_map(db, func);
    let provider_params = CallableDef::Func(func).params(db);

    let effect_ref_trait = resolve_core_trait(db, func.scope(), &["effect_ref", "EffectRef"])
        .expect("missing required core trait `core::effect_ref::EffectRef`");
    let effect_ref_mut_trait =
        resolve_core_trait(db, func.scope(), &["effect_ref", "EffectRefMut"])
            .expect("missing required core trait `core::effect_ref::EffectRefMut`");

    let mut out = Vec::new();
    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };

        let key_kind = effect_key_kind(db, key_path, func.scope());
        if !matches!(key_kind, EffectKeyKind::Type | EffectKeyKind::Trait) {
            continue;
        }

        let Some(provider_idx) = provider_map.get(effect.index()).copied().flatten() else {
            continue;
        };
        let Some(&provider_ty) = provider_params.get(provider_idx) else {
            continue;
        };

        match key_kind {
            EffectKeyKind::Trait => {
                let Ok(PathRes::Trait(inst)) =
                    resolve_path(db, key_path, func.scope(), assumptions, false)
                else {
                    continue;
                };

                let mut args = inst.args(db).to_vec();
                if args.is_empty() {
                    args.push(provider_ty);
                } else {
                    args[0] = provider_ty;
                }
                out.push(TraitInstId::new(
                    db,
                    inst.def(db),
                    args,
                    inst.assoc_type_bindings(db).clone(),
                ));
            }
            EffectKeyKind::Type => {
                let Ok(PathRes::Ty(target_ty) | PathRes::TyAlias(_, target_ty)) =
                    resolve_path(db, key_path, func.scope(), assumptions, false)
                else {
                    continue;
                };
                if !target_ty.is_star_kind(db) {
                    continue;
                }

                out.push(TraitInstId::new(
                    db,
                    effect_ref_trait,
                    vec![provider_ty, target_ty],
                    IndexMap::new(),
                ));

                if effect.is_mut(db) {
                    out.push(TraitInstId::new(
                        db,
                        effect_ref_mut_trait,
                        vec![provider_ty, target_ty],
                        IndexMap::new(),
                    ));
                }
            }
            EffectKeyKind::Other => {}
        }
    }

    out
}

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
            collect_func_def_constraints(db, *func_def, true),
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
pub fn super_trait_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Option<Vec<Trait<'db>>> {
    super_trait_cycle_impl(db, trait_, &[])
}

pub fn super_trait_cycle_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
    chain: &[Trait<'db>],
) -> Option<Vec<Trait<'db>>> {
    if chain.contains(&trait_) {
        return Some(chain.to_vec());
    }
    let bounds = trait_.super_traits(db);
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
    func: CallableDef<'db>,
    include_parent: bool,
) -> Binder<PredicateListId<'db>> {
    let hir_func = match func {
        CallableDef::Func(func) => func,
        CallableDef::VariantCtor(var) => {
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

    // Where-clause predicates (directly over HIR)
    //
    // We intentionally operate on the raw where-clause HIR here rather than the
    // semantic view helpers to avoid re-entering constraint collection via
    // `constraints_for`. This keeps the fixed-point iteration semantics
    // identical to the legacy implementation while the semantic layer remains
    // the main traversal API for other callers.
    if let Some(w_owner) = owner.where_clause_owner() {
        let where_clause = w_owner.where_clause(db);
        for pred in where_clause.data(db).iter() {
            let Some(hir_ty) = pred.ty.to_opt() else {
                continue;
            };

            // Filter out super-trait constraints on `Self`; those are handled
            // in `collect_super_traits`.
            if hir_ty.is_self_ty(db) && matches!(owner, GenericParamOwner::Trait(_)) {
                continue;
            }

            for bound in &pred.bounds {
                if let TypeBound::Trait(trait_ref) = *bound {
                    deferred.push(Deferred {
                        bound_ty: Either::Left(hir_ty),
                        trait_ref,
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
            break;
        }
    }

    // Collect implicit effect constraints on provider generic parameters.
    if let GenericParamOwner::Func(func) = owner {
        let assumptions =
            PredicateListId::new(db, all_predicates.iter().copied().collect::<Vec<_>>());
        for inst in collect_effect_constraints_for_func(db, func, assumptions) {
            all_predicates.insert(inst);
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

    lower_trait_ref(
        db,
        ty,
        deferred.trait_ref,
        deferred.scope,
        assumptions,
        None,
    )
    .ok()
}
