use crate::core::hir_def::{
    GenericParam, GenericParamOwner, GenericParamView, ItemKind, Trait, TraitRefId, TypeBound,
    scope_graph::ScopeId, types::TypeId as HirTypeId,
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
        effects::{EffectKeyKind, place_effect_provider_param_index_map},
        layout_holes::{collect_layout_hole_tys_in_order, ty_contains_const_hole},
        trait_def::TraitInstId,
        trait_lower::{lower_impl_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{base_generic_params, collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
};

fn collect_effect_constraints_for_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<TraitInstId<'db>> {
    let provider_map = place_effect_provider_param_index_map(db, func);
    let provider_params = CallableDef::Func(func).params(db);

    let Some(effect_ref_trait) = resolve_core_trait(db, func.scope(), &["effect_ref", "EffectRef"])
    else {
        // EffectRef is a required stdlib trait. If it can't be resolved the
        // stdlib is broken — returning empty constraints here would silently
        // skip effect-bound checking and allow incorrect code to compile.
        panic!("missing required core trait EffectRef — stdlib is broken");
    };
    let Some(effect_ref_mut_trait) =
        resolve_core_trait(db, func.scope(), &["effect_ref", "EffectRefMut"])
    else {
        panic!("missing required core trait EffectRefMut — stdlib is broken");
    };

    let mut out = Vec::new();
    for binding in func.effect_bindings(db) {
        if !matches!(binding.key_kind, EffectKeyKind::Type | EffectKeyKind::Trait) {
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

        match (binding.key_ty, binding.key_trait) {
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

                out.push(TraitInstId::new(
                    db,
                    effect_ref_trait,
                    vec![provider_ty, target_ty],
                    IndexMap::new(),
                ));

                if binding.is_mut {
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

    let func_constraints = collect_decl_constraints(db, hir_func.into());
    if !include_parent {
        return func_constraints;
    }

    let parent_constraints = match hir_func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => collect_constraints(db, trait_.into()),

        Some(ItemKind::Impl(impl_)) => collect_constraints(db, impl_.into()),

        Some(ItemKind::ImplTrait(impl_trait)) => {
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
    for inst in collect_effect_constraints_for_func(db, hir_func) {
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

#[salsa::tracked(cycle_fn=collect_constraints_cycle_recover, cycle_initial=collect_constraints_cycle_initial)]
pub(crate) fn collect_decl_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Binder<PredicateListId<'db>> {
    let mut deferred: Vec<Deferred<'db>> = Vec::new();
    let owner_scope = owner.scope();

    // Generic parameter bounds
    let param_set = match owner {
        GenericParamOwner::Func(_) => base_generic_params(db, owner),
        _ => collect_generic_params(db, owner),
    };
    let params = owner.params(db);
    for (idx, GenericParamView { param, .. }) in params.enumerate() {
        let GenericParam::Type(hir_param) = param else {
            continue;
        };
        let Some(ty) = param_set.param_by_original_idx(db, idx) else {
            continue;
        };
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

    Binder::bind(PredicateListId::new(
        db,
        all_predicates.into_iter().collect::<Vec<_>>(),
    ))
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

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::*;
    use crate::analysis::ty::layout_holes::ty_contains_const_hole;
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
        let effect_ref_trait =
            resolve_core_trait(&db, func.scope(), &["effect_ref", "EffectRef"]).unwrap();
        let constraints = collect_effect_constraints_for_func(&db, func);
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
        let effect_ref_trait =
            resolve_core_trait(&db, func.scope(), &["effect_ref", "EffectRef"]).unwrap();
        let constraints = collect_effect_constraints_for_func(&db, func);
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
        let constraints = collect_effect_constraints_for_func(&db, func);
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

        let effect_bindings = func.effect_bindings(&db);
        let slot_binding = effect_bindings
            .iter()
            .find(|binding| binding.binding_name.data(&db) == "slot")
            .expect("missing slot binding");
        let key_ty = slot_binding.key_ty.expect("missing type effect key");
        assert!(!ty_contains_const_hole(&db, key_ty));
        let args = key_ty.generic_args(&db);
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], TyId::u256(&db));
        assert!(matches!(args[1].data(&db), TyData::ConstTy(_)));

        let effect_ref_trait =
            resolve_core_trait(&db, func.scope(), &["effect_ref", "EffectRef"]).unwrap();
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
}
