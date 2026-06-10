use crate::core::hir_def::{
    GenericParam, GenericParamOwner, GenericParamView, ItemKind, Trait, TraitRefId, TypeBound,
    WhereClauseOwner, scope_graph::ScopeId, types::TypeId as HirTypeId,
};
use crate::hir_def::CallableDef;
use common::indexmap::{IndexMap, IndexSet};
use either::Either;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        adt_def::AdtDef,
        binder::Binder,
        corelib::resolve_core_trait,
        effects::{
            EffectKeyCanonMode, EffectKeyKind, canonical_effect_identity_for_binding,
            place_effect_provider_param_index_map,
        },
        layout_holes::{collect_layout_hole_tys_in_order, ty_contains_const_hole},
        trait_def::TraitInstId,
        trait_lower::{lower_impl_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
};

/// Collect provider constraints induced by a function's `uses` clause.
///
/// Type-key effects produce `EffectRef`/`EffectRefMut` bounds from the hidden
/// provider type to the visible effect target, while trait-key effects produce
/// the corresponding trait bound on the hidden provider type.
pub(crate) fn collect_func_effect_provider_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<TraitInstId<'db>> {
    let provider_map = place_effect_provider_param_index_map(db, func);
    let provider_params = CallableDef::Func(func).params(db);
    let scope = func.scope();
    let assumptions = collect_func_decl_constraints(db, func.into(), true)
        .instantiate_identity()
        .extend_all_bounds(db);

    let mut out = Vec::new();
    let mut effect_ref_traits = None;
    for binding in func.effect_requirements(db) {
        if !matches!(
            binding.key.kind(),
            EffectKeyKind::Type | EffectKeyKind::Trait
        ) {
            continue;
        }

        let Some(provider_idx) = provider_map
            .get(binding.binding_idx as usize)
            .copied()
            .flatten()
        else {
            continue;
        };
        let Some(&provider_ty) = provider_params.get(provider_idx) else {
            continue;
        };

        let identity = canonical_effect_identity_for_binding(
            db,
            binding,
            scope,
            assumptions,
            None,
            EffectKeyCanonMode::Solver,
        );

        match (identity.key_ty, identity.key_trait) {
            (_, Some(inst)) => {
                debug_assert!(
                    collect_layout_hole_tys_in_order(db, inst).is_empty(),
                    "effect constraint trait key still contains unresolved layout holes"
                );
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
            (Some(target_ty), None) => {
                if !target_ty.is_star_kind(db) {
                    continue;
                }
                debug_assert!(
                    !ty_contains_const_hole(db, target_ty) || target_ty.has_invalid(db),
                    "effect constraint type key still contains unresolved layout holes"
                );
                let (effect_ref_trait, effect_ref_mut_trait) =
                    effect_ref_traits.unwrap_or_else(|| {
                        let Some(effect_ref_trait) =
                            resolve_core_trait(db, func.scope(), &["EffectRef"])
                        else {
                            panic!("missing required core trait EffectRef — stdlib is broken");
                        };
                        let Some(effect_ref_mut_trait) =
                            resolve_core_trait(db, func.scope(), &["EffectRefMut"])
                        else {
                            panic!("missing required core trait EffectRefMut — stdlib is broken");
                        };
                        let traits = (effect_ref_trait, effect_ref_mut_trait);
                        effect_ref_traits = Some(traits);
                        traits
                    });

                out.push(TraitInstId::new(
                    db,
                    effect_ref_trait,
                    vec![provider_ty, target_ty],
                    IndexMap::new(),
                ));

                if identity.is_mut {
                    out.push(TraitInstId::new(
                        db,
                        effect_ref_mut_trait,
                        vec![provider_ty, target_ty],
                        IndexMap::new(),
                    ));
                }
            }
            _ => {}
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

#[salsa::tracked(
    cycle_fn=collect_func_def_constraints_cycle_recover,
    cycle_initial=collect_func_def_constraints_cycle_initial
)]
pub(crate) fn collect_func_decl_constraints<'db>(
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
            }
            return Binder::bind(PredicateListId::empty_list(db));
        }
    };

    if !include_parent {
        let func_constraints = collect_decl_constraints(db, hir_func.into());
        return func_constraints;
    }

    Binder::bind(PredicateListId::new(
        db,
        collect_func_decl_constraint_pairs(db, func)
            .into_iter()
            .map(|(inst, _)| inst)
            .collect::<Vec<_>>(),
    ))
}

/// The constraints enforced on a call to `func` (parent constraints first,
/// then the function's own — the list `collect_func_decl_constraints(..,
/// true)` interns, in the same order), each paired with the written bound it
/// was collected from.
pub(crate) fn collect_func_decl_constraint_pairs<'db>(
    db: &'db dyn HirAnalysisDb,
    func: CallableDef<'db>,
) -> Vec<(TraitInstId<'db>, PredicateSource<'db>)> {
    let hir_func = match func {
        CallableDef::Func(func) => func,
        CallableDef::VariantCtor(var) => {
            let adt = var.enum_.as_adt(db);
            return match adt.as_generic_param_owner(db) {
                Some(owner) => decl_constraint_pairs(db, owner).clone(),
                None => Vec::new(),
            };
        }
    };

    let parent_pairs = match hir_func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => decl_constraint_pairs(db, trait_.into()),

        Some(ItemKind::Impl(impl_)) => decl_constraint_pairs(db, impl_.into()),

        Some(ItemKind::ImplTrait(impl_trait)) => {
            if lower_impl_trait(db, impl_trait).is_none() {
                return decl_constraint_pairs(db, hir_func.into()).clone();
            }
            decl_constraint_pairs(db, impl_trait.into())
        }

        _ => return decl_constraint_pairs(db, hir_func.into()).clone(),
    };

    collect_decl_constraint_pairs_impl(db, hir_func.into(), parent_pairs)
}

#[salsa::tracked(
    cycle_fn=collect_func_def_constraints_cycle_recover,
    cycle_initial=collect_func_def_constraints_cycle_initial
)]
pub(crate) fn collect_func_def_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    func: CallableDef<'db>,
    include_parent: bool,
) -> Binder<PredicateListId<'db>> {
    let CallableDef::Func(hir_func) = func else {
        return collect_func_decl_constraints(db, func, include_parent);
    };

    let mut predicates: IndexSet<_> = collect_func_decl_constraints(db, func, include_parent)
        .instantiate_identity()
        .list(db)
        .iter()
        .copied()
        .collect();
    for inst in collect_func_effect_provider_constraints(db, hir_func) {
        predicates.insert(inst);
    }

    Binder::bind(PredicateListId::new(
        db,
        predicates.into_iter().collect::<Vec<_>>(),
    ))
}

fn collect_func_def_constraints_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _func: CallableDef<'db>,
    _include_parent: bool,
) -> Binder<PredicateListId<'db>> {
    Binder::bind(PredicateListId::empty_list(db))
}

fn collect_func_def_constraints_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Binder<PredicateListId<'db>>,
    _count: u32,
    _func: CallableDef<'db>,
    _include_parent: bool,
) -> salsa::CycleRecoveryAction<Binder<PredicateListId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

/// Where a collected predicate was written. Recorded during constraint
/// collection so diagnostics can point at the originating bound without
/// re-deriving it from a list index: the final list is parent-first,
/// deduplicated, and resolution-ordered, so indices don't map back to
/// written bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) enum PredicateSource<'db> {
    GenericParamBound {
        owner: GenericParamOwner<'db>,
        param_idx: usize,
        bound_idx: usize,
    },
    WherePredicateBound {
        owner: WhereClauseOwner<'db>,
        pred_idx: usize,
        bound_idx: usize,
    },
}

#[salsa::tracked(
    cycle_fn=collect_constraints_cycle_recover,
    cycle_initial=collect_constraints_cycle_initial
)]
pub(crate) fn collect_decl_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Binder<PredicateListId<'db>> {
    Binder::bind(PredicateListId::new(
        db,
        decl_constraint_pairs(db, owner)
            .iter()
            .map(|(inst, _)| *inst)
            .collect::<Vec<_>>(),
    ))
}

/// Declared constraints of `owner` paired with the bound each was collected
/// from. The key sequence is exactly the `PredicateListId` order.
#[salsa::tracked(
    return_ref,
    cycle_fn=decl_constraint_pairs_cycle_recover,
    cycle_initial=decl_constraint_pairs_cycle_initial
)]
pub(crate) fn decl_constraint_pairs<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Vec<(TraitInstId<'db>, PredicateSource<'db>)> {
    collect_decl_constraint_pairs_impl(db, owner, &[])
}

fn decl_constraint_pairs_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _owner: GenericParamOwner<'db>,
) -> Vec<(TraitInstId<'db>, PredicateSource<'db>)> {
    Vec::new()
}

fn decl_constraint_pairs_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Vec<(TraitInstId<'db>, PredicateSource<'db>)>,
    _count: u32,
    _owner: GenericParamOwner<'db>,
) -> salsa::CycleRecoveryAction<Vec<(TraitInstId<'db>, PredicateSource<'db>)>> {
    salsa::CycleRecoveryAction::Iterate
}

fn collect_decl_constraint_pairs_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    initial: &[(TraitInstId<'db>, PredicateSource<'db>)],
) -> Vec<(TraitInstId<'db>, PredicateSource<'db>)> {
    let mut deferred: Vec<Deferred<'db>> = Vec::new();
    let owner_scope = owner.scope();

    // Generic parameter bounds
    let param_set = collect_generic_params(db, owner);
    let params = owner.params(db);
    for (idx, GenericParamView { param, .. }) in params.enumerate() {
        let GenericParam::Type(hir_param) = param else {
            continue;
        };
        let Some(ty) = param_set.param_by_original_idx(db, idx) else {
            continue;
        };
        for (bound_idx, bound) in hir_param.bounds.iter().enumerate() {
            if let TypeBound::Trait(trait_ref) = bound {
                deferred.push(Deferred {
                    bound_ty: Either::Right(ty),
                    trait_ref: *trait_ref,
                    scope: owner_scope,
                    source: PredicateSource::GenericParamBound {
                        owner,
                        param_idx: idx,
                        bound_idx,
                    },
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
        for (pred_idx, pred) in where_clause.data(db).iter().enumerate() {
            let Some(hir_ty) = pred.ty.to_opt() else {
                continue;
            };

            // Filter out super-trait constraints on `Self`; those are handled
            // in `collect_super_traits`.
            if hir_ty.is_self_ty(db) && matches!(owner, GenericParamOwner::Trait(_)) {
                continue;
            }

            for (bound_idx, bound) in pred.bounds.iter().enumerate() {
                if let TypeBound::Trait(trait_ref) = *bound {
                    deferred.push(Deferred {
                        bound_ty: Either::Left(hir_ty),
                        trait_ref,
                        scope: owner_scope,
                        source: PredicateSource::WherePredicateBound {
                            owner: w_owner,
                            pred_idx,
                            bound_idx,
                        },
                    });
                }
            }
        }
    }

    let mut all_predicates: IndexMap<TraitInstId<'db>, PredicateSource<'db>> =
        initial.iter().copied().collect();

    // fixed-point iteration over deferred predicates
    while !deferred.is_empty() {
        let assumptions =
            PredicateListId::new(db, all_predicates.keys().copied().collect::<Vec<_>>());

        let before = deferred.len();
        deferred.retain(|p| match try_resolve_type_bound(db, p, assumptions) {
            Some(inst) => {
                all_predicates.entry(inst).or_insert(p.source);
                false
            }
            None => true,
        });
        if deferred.len() == before {
            break;
        }
    }

    all_predicates.into_iter().collect()
}

fn collect_constraints_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _owner: GenericParamOwner<'db>,
) -> Binder<PredicateListId<'db>> {
    Binder::bind(PredicateListId::empty_list(db))
}

fn collect_constraints_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Binder<PredicateListId<'db>>,
    _count: u32,
    _owner: GenericParamOwner<'db>,
) -> salsa::CycleRecoveryAction<Binder<PredicateListId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_fn=collect_constraints_cycle_recover, cycle_initial=collect_constraints_cycle_initial)]
pub fn collect_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Binder<PredicateListId<'db>> {
    match owner {
        GenericParamOwner::Func(func) => collect_func_def_constraints(db, func.into(), true),
        _ => collect_decl_constraints(db, owner),
    }
}

struct Deferred<'db> {
    bound_ty: Either<HirTypeId<'db>, TyId<'db>>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    source: PredicateSource<'db>,
}

/// What `Self` denotes in bounds written at `scope`: the nearest enclosing
/// trait's `Self` parameter, if any.
///
/// Bound lowering substitutes `Self` occurrences in trait-ref args with the
/// bound's subject by default, which is only correct when the subject *is*
/// `Self` (super-traits, impl headers). For bounds on other subjects inside a
/// trait (`trait Tr<T: A<Self::Item>>`), `Self` must keep referring to the
/// enclosing trait's `Self`.
pub(crate) fn enclosing_trait_self_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<TyId<'db>> {
    let mut item = Some(scope.item());
    while let Some(current) = item {
        match current {
            ItemKind::Trait(trait_) => {
                return collect_generic_params(db, trait_.into()).trait_self(db);
            }
            // `Self` inside impls resolves to the implementor type directly.
            ItemKind::Impl(_) | ItemKind::ImplTrait(_) => return None,
            _ => {}
        }
        item = current.scope().parent_item(db);
    }
    None
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
        enclosing_trait_self_ty(db, deferred.scope),
    )
    .ok()
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::*;
    use crate::analysis::ty::{
        GoalSatisfiability, TraitSolveCx, corelib::resolve_lib_type_path, is_goal_satisfiable,
        layout_holes::ty_contains_const_hole,
    };
    use crate::test_db::HirAnalysisTestDb;

    fn find_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        func_name: &str,
    ) -> crate::hir_def::Func<'db> {
        top_mod
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(db)
                        .to_opt()
                        .is_some_and(|name| name.data(db) == func_name) =>
                {
                    Some(func)
                }
                _ => None,
            })
            .expect("missing function")
    }

    fn find_trait<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        trait_name: &str,
    ) -> Trait<'db> {
        top_mod
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(db)
                        .to_opt()
                        .is_some_and(|name| name.data(db) == trait_name) =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing trait")
    }

    #[test]
    fn method_where_clause_resolves_with_parent_constraints() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("method_where_clause_resolves_with_parent_constraints.fe"),
            r#"
trait Decoder<A> {}

trait Abi {
    type Decoder: * -> *
}

fn needs_decoder<A, D>()
where D: Decoder<A>
{}

trait Decode<A: Abi> {
    fn decode_from<I>()
        where A::Decoder<I>: Decoder<A>
    {
        needs_decoder<A, A::Decoder<I>>()
    }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
    }

    #[test]
    fn effect_provider_constraints_include_concrete_type_key_bounds() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_provider_constraints_include_concrete_type_key_bounds.fe"),
            r#"
use std::evm::Evm

fn f() uses (evm: mut Evm) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let provider_map = place_effect_provider_param_index_map(&db, func);
        let provider_idx = provider_map[0].expect("missing provider slot for evm");
        let provider_ty = CallableDef::Func(func).params(&db)[provider_idx];
        let evm_ty = resolve_lib_type_path(&db, func.scope(), "std::evm::Evm")
            .expect("missing std::evm::Evm");
        let effect_ref_trait = resolve_core_trait(&db, func.scope(), &["EffectRef"]).unwrap();
        let effect_ref_mut_trait =
            resolve_core_trait(&db, func.scope(), &["EffectRefMut"]).unwrap();
        let constraints = collect_func_effect_provider_constraints(&db, func);

        assert_ne!(provider_ty, evm_ty);
        assert!(constraints.iter().any(|inst| {
            inst.def(&db) == effect_ref_trait && inst.args(&db).as_slice() == [provider_ty, evm_ty]
        }));
        assert!(constraints.iter().any(|inst| {
            inst.def(&db) == effect_ref_mut_trait
                && inst.args(&db).as_slice() == [provider_ty, evm_ty]
        }));
    }

    #[test]
    fn effect_constraints_elaborate_distinct_callable_type_key_holes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_constraints_elaborate_distinct_callable_type_key_holes.fe"),
            r#"
struct Distinct<const LEFT: u256 = _, const RIGHT: u256 = _> {}

fn f() uses (slots: Distinct) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let effect_ref_trait = resolve_core_trait(&db, func.scope(), &["EffectRef"]).unwrap();
        let constraints = collect_func_effect_provider_constraints(&db, func);
        let effect_ref = constraints
            .into_iter()
            .find(|inst| inst.def(&db) == effect_ref_trait)
            .expect("missing EffectRef constraint");
        let target_ty = effect_ref.args(&db)[1];
        assert!(!ty_contains_const_hole(&db, target_ty));
        let args = target_ty.generic_args(&db);
        assert_eq!(args.len(), 2);
        let left = args[0];
        let right = args[1];
        assert_ne!(left, right);
    }

    #[test]
    fn effect_constraints_preserve_repeated_callable_type_key_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_constraints_preserve_repeated_callable_type_key_identity.fe"),
            r#"
struct Slot<const ROOT: u256 = _> {}
type Repeated<const ROOT: u256 = _> = (Slot<ROOT>, Slot<ROOT>)

fn f() uses (slots: Repeated) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let effect_ref_trait = resolve_core_trait(&db, func.scope(), &["EffectRef"]).unwrap();
        let constraints = collect_func_effect_provider_constraints(&db, func);
        let effect_ref = constraints
            .into_iter()
            .find(|inst| inst.def(&db) == effect_ref_trait)
            .expect("missing EffectRef constraint");
        let target_ty = effect_ref.args(&db)[1];
        assert!(!ty_contains_const_hole(&db, target_ty));
        let fields = target_ty.field_types(&db);
        assert_eq!(fields.len(), 2);
        let left = fields[0].generic_args(&db)[0];
        let right = fields[1].generic_args(&db)[0];
        assert_eq!(left, right);
    }

    #[test]
    fn effect_constraints_canonicalize_omitted_const_expr_defaults() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_constraints_canonicalize_omitted_const_expr_defaults.fe"),
            r#"
const fn plus1(_ x: usize) -> usize {
    x + 1
}

trait Cap<T> {}

struct Slot<const N: usize, const M: usize = plus1(N)> {}

fn f() uses (cap: Cap<Slot<4>>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let cap_trait = find_trait(&db, top_mod, "Cap");
        let constraints = collect_func_effect_provider_constraints(&db, func);
        let cap_inst = constraints
            .into_iter()
            .find(|inst| inst.def(&db) == cap_trait)
            .expect("missing Cap constraint");
        let key_ty = cap_inst.args(&db)[1];
        assert_eq!(key_ty.pretty_print(&db).to_string(), "Slot<4, 5>");
    }

    #[test]
    fn effect_constraints_elaborate_callable_trait_key_holes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_constraints_elaborate_callable_trait_key_holes.fe"),
            r#"
trait Cap<const LEFT: u256 = _, const RIGHT: u256 = _> {}

fn f() uses (cap: Cap) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let cap_trait = find_trait(&db, top_mod, "Cap");
        let constraints = collect_func_effect_provider_constraints(&db, func);
        let cap_inst = constraints
            .into_iter()
            .find(|inst| inst.def(&db) == cap_trait)
            .expect("missing Cap constraint");
        assert!(collect_layout_hole_tys_in_order(&db, cap_inst).is_empty());
        let args = cap_inst.args(&db);
        assert!(
            args.len() >= 3,
            "expected provider self plus two const args"
        );
        assert_ne!(args[1], args[2]);
    }

    #[test]
    fn effect_constraints_resolve_assoc_type_keys_and_provider_slots() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_constraints_resolve_assoc_type_keys_and_provider_slots.fe"),
            r#"
trait HasSlot {
    type Assoc
}

trait Cap {}
struct Slot<T, const ROOT: u256 = _> {}

fn f<T>() uses (cap: Cap, slot: T::Assoc)
where
    T: HasSlot<Assoc = Slot<u256>>
{}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let provider_map = place_effect_provider_param_index_map(&db, func);
        assert_eq!(provider_map.len(), 2);
        let cap_provider_idx = provider_map[0].expect("missing provider slot for cap");
        let slot_provider_idx = provider_map[1].expect("missing provider slot for slot");
        assert!(cap_provider_idx < slot_provider_idx);

        let effect_bindings = func.effect_requirements(&db);
        let slot_binding = effect_bindings
            .iter()
            .find(|binding| binding.binding_name.data(&db) == "slot")
            .expect("missing slot binding");
        let key_ty = slot_binding.key.key_ty().expect("missing type effect key");
        assert!(!ty_contains_const_hole(&db, key_ty));
        let args = key_ty.generic_args(&db);
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], TyId::u256(&db));
        assert!(matches!(args[1].data(&db), TyData::ConstTy(_)));

        let effect_ref_trait = resolve_core_trait(&db, func.scope(), &["EffectRef"]).unwrap();
        let constraints = collect_constraints(&db, func.into()).instantiate_identity();
        let effect_ref = constraints
            .list(&db)
            .iter()
            .copied()
            .find(|inst| inst.def(&db) == effect_ref_trait && inst.args(&db)[1] == key_ty)
            .expect("missing EffectRef constraint for slot effect");
        assert!(!ty_contains_const_hole(&db, effect_ref.args(&db)[1]));

        let provider_names: Vec<_> = CallableDef::Func(func)
            .params(&db)
            .iter()
            .filter_map(|ty| match ty.data(&db) {
                TyData::TyParam(param) if param.is_effect_provider() => {
                    Some(param.pretty_print(&db))
                }
                _ => None,
            })
            .collect();
        assert_eq!(provider_names, vec!["cap".to_string(), "slot".to_string()]);
    }

    #[test]
    fn function_decl_constraints_instantiate_concrete_abi_trait_args() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("function_decl_constraints_instantiate_concrete_abi_trait_args.fe"),
            r#"
use core::abi::{Decode, Encode}
use std::abi::Sol

fn require_encode<T>() where T: Encode<Sol> {}
fn require_decode<T>() where T: Decode<Sol> {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let require_encode = find_func(&db, top_mod, "require_encode");
        let require_decode = find_func(&db, top_mod, "require_decode");
        let u256_ty = TyId::u256(&db);

        for (label, func) in [("encode", require_encode), ("decode", require_decode)] {
            let constraints =
                collect_func_decl_constraints(&db, func.into(), true).instantiate(&db, &[u256_ty]);
            let goal = *constraints
                .list(&db)
                .first()
                .unwrap_or_else(|| panic!("missing {label} constraint"));
            let solve_cx = TraitSolveCx::new(&db, func.scope());
            match is_goal_satisfiable(&db, solve_cx, goal) {
                GoalSatisfiability::Satisfied(_) => {}
                other => panic!(
                    "expected instantiated `{label}` goal `{}` to be satisfiable, got {other:?}",
                    goal.pretty_print(&db, true)
                ),
            }
        }
    }
}
