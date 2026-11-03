use thin_vec::ThinVec;

use super::{
    assoc_const::AssocConstUse,
    binder::Binder,
    const_expr::ConstExpr,
    const_ty::{
        ConstCanonEnv, ConstCanonMode, ConstTyData, ConstTyId, canonicalize_ty_for_mode,
        const_ty_from_assoc_const_use, normalize_const_tys_for_comparison,
    },
    diagnostics::{ImplDiag, TyDiagCollection},
    effects::{
        CanonicalEffectIdentity, EffectKeyCanonMode, EffectKeyKind,
        canonical_effect_identity_for_binding, place_effect_provider_param_index_map,
        rows::{RowKey, row_is_empty},
    },
    fold::{TyFoldable, TyFolder},
    normalize::{normalize_from_assumptions, normalize_ty},
    shape::{Shape, names_shape_without_mode},
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitSolveCx,
        constraint::collect_func_def_constraints, is_goal_satisfiable,
    },
    ty_check::{ConstRef, check_anon_const_body},
    ty_def::{BorrowKind, InvalidCause, Kind, TyData, TyId},
    ty_lower::{CompleteSubst, ParamDomainId, ParamSchemaId, PartialSubst},
    unify::UnificationTable,
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{CallableDef, Expr, Partial, PathKind, scope_graph::ScopeId};
use common::indexmap::IndexMap;

/// Compares the implementation method with the trait method to ensure they
/// match.
///
/// This function performs the following checks:
///
/// 1. Unsafety.
/// 2. Number of generic parameters.
/// 3. Kinds of generic parameters.
/// 4. Arity (number of arguments).
/// 5. Argument labels.
/// 6. Argument types and return type.
/// 7. Method constraints.
///
/// If any of these checks fail, the function will record the appropriate
/// diagnostics.
///
/// # Arguments
///
/// * `db` - Reference to the database implementing the `HirAnalysisDb` trait.
/// * `impl_m` - The implementation method to compare.
/// * `trait_m` - The trait method to compare against.
/// * `trait_inst` - The instance of the trait being checked.
/// * `implementor` - The implementor that contains associated type bindings.
/// * `sink` - A mutable reference to a vector where diagnostic messages will be
///   collected.
///
/// A method without a mode can be a `mut?` trait method's body at one of
/// its modes, `body_mode` (the `ref` body unless it is the `mut` one of a
/// pair); a `mut?` method implements it at both.
pub(super) fn compare_impl_method<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    trait_inst: TraitInstId<'db>,
    body_mode: BorrowKind,
    sink: &mut Vec<TyDiagCollection<'db>>,
) {
    if let (CallableDef::Func(impl_func), CallableDef::Func(trait_func)) = (impl_m, trait_m)
        && impl_func.is_unsafe(db) != trait_func.is_unsafe(db)
    {
        sink.push(ImplDiag::MethodUnsafeMismatch { trait_m, impl_m }.into());
    }

    if !compare_generic_param_num(db, impl_m, trait_m, sink) {
        return;
    }

    if !compare_generic_param_kind(db, impl_m, trait_m, sink) {
        return;
    }

    let both = [
        (Some(BorrowKind::Ref), Some(BorrowKind::Ref)),
        (Some(BorrowKind::Mut), Some(BorrowKind::Mut)),
    ];
    let single = [(Some(body_mode), None)];
    let modes: &[(Option<BorrowKind>, Option<BorrowKind>)] =
        match (trait_m.modes(db), impl_m.modes(db)) {
            ([None], [None]) => &[(None, None)],
            ([None], _) => {
                sink.push(ImplDiag::MethodMaybeMutMismatch { trait_m, impl_m }.into());
                return;
            }
            (_, [None]) => &single,
            _ => &both,
        };

    // The effects pair at each mode, where a `mut?` effect has its mode.
    let substs: Vec<_> = modes
        .iter()
        .map(|&(trait_mode, impl_mode)| {
            let (subst, _, paired) = trait_to_impl_param_subst(
                db,
                (impl_m, impl_mode),
                (trait_m, trait_mode),
                trait_inst,
            );
            (subst.residualize(db), paired)
        })
        .collect();

    // A spread lists the parameters of the shape the impl's trait instance
    // gives it.
    let spread = trait_m.spread(db).and_then(|idx| trait_m.arg_ty(db, idx));
    let listed = match spread {
        Some(spread) => {
            let shape = normalize_from_assumptions(
                db,
                instantiate_trait_method_template(db, spread, &substs[0].0),
                impl_m.scope(),
                PredicateListId::new(db, vec![trait_inst]),
            );
            let Some(listed) = shape.as_params_shape(db) else {
                // A spread of no shape is reported where it is declared.
                if *shape.kind(db) == Kind::Shape && !shape.has_invalid(db) {
                    sink.push(
                        ImplDiag::SpreadNotListed {
                            trait_m,
                            impl_m,
                            shape,
                        }
                        .into(),
                    );
                }
                return;
            };
            listed
        }
        None => Vec::new(),
    };
    let arity = trait_m.arg_tys(db).len() - usize::from(spread.is_some()) + listed.len();
    if impl_m.arg_tys(db).len() != arity {
        sink.push(
            ImplDiag::MethodArgNumMismatch {
                trait_m,
                impl_m,
                expected: arity,
            }
            .into(),
        );
        return;
    }

    // Compare the argument labels, argument types, and return type of the impl
    // method with the trait method.
    let mut err = !compare_arg_label(db, impl_m, trait_m, sink);
    let unpaired: Vec<_> = modes
        .iter()
        .zip(&substs)
        .filter(|(_, (_, paired))| !paired)
        .map(|(&(trait_mode, _), _)| trait_mode)
        .collect();
    if matches!(
        (impl_m, trait_m),
        (CallableDef::Func(_), CallableDef::Func(_))
    ) && !unpaired.is_empty()
    {
        // A `mut?` trait method's effects differ at one of its modes alone.
        let mode = match unpaired.as_slice() {
            [mode] => *mode,
            _ => None,
        };
        sink.push(
            ImplDiag::MethodEffectMismatch {
                trait_m,
                impl_m,
                mode,
            }
            .into(),
        );
        err = true;
    }
    // A `mut?` method that differs at both modes is reported once, at the
    // first.
    for (&(trait_mode, impl_mode), (param_subst, _)) in modes.iter().zip(&substs) {
        if !compare_ty(
            db,
            (impl_m, impl_mode),
            (trait_m, trait_mode),
            param_subst,
            &listed,
            trait_inst,
            sink,
        ) {
            return;
        }
    }
    if err {
        return;
    }

    for (&(trait_mode, impl_mode), (param_subst, _)) in modes.iter().zip(&substs) {
        if !compare_constraints(
            db,
            (impl_m, impl_mode),
            (trait_m, trait_mode),
            trait_inst,
            param_subst,
            sink,
        ) {
            return;
        }
    }
}

/// Checks if the number of generic parameters of the implemented method is the
/// same as the number of generic parameters of the trait method.
/// Returns `false` if the comparison fails.
fn compare_generic_param_num<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_params = impl_m.explicit_params(db);
    let trait_params = trait_m.explicit_params(db);

    if impl_params.len() == trait_params.len() {
        true
    } else {
        sink.push(ImplDiag::MethodTypeParamNumMismatch { trait_m, impl_m }.into());
        false
    }
}

/// Checks if the generic parameter kinds are the same.
/// Returns `false` if the comparison fails.
fn compare_generic_param_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    for (idx, (&trait_m_param, &impl_m_param)) in trait_m
        .explicit_params(db)
        .iter()
        .zip(impl_m.explicit_params(db))
        .enumerate()
    {
        let trait_m_kind = trait_m_param.kind(db);
        let impl_m_kind = impl_m_param.kind(db);

        if !trait_m_kind.does_match(impl_m_kind) {
            sink.push(
                ImplDiag::MethodTypeParamKindMismatch {
                    trait_m,
                    impl_m,
                    param_idx: idx,
                }
                .into(),
            );
            err = true;
        }
    }

    !err
}

/// Checks if the argument labels of the implemented method are the same as the
/// argument labels of the trait method.
/// Returns `false` if the comparison fails.
fn compare_arg_label<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    for idx in 0..impl_m.arg_tys(db).len() {
        let trait_param = trait_m.declared_param(db, idx);
        let Some(expected_label) = trait_m.param_label_or_name(db, trait_param) else {
            continue;
        };
        let Some(method_label) = impl_m.param_label_or_name(db, idx) else {
            continue;
        };
        if expected_label != method_label {
            sink.push(
                ImplDiag::MethodArgLabelMismatch {
                    trait_m,
                    impl_m,
                    param_idx: idx,
                }
                .into(),
            );
            err = true;
        }
    }

    !err
}

/// Checks if the argument types and return type of the implemented method,
/// used at its mode, are the same as the argument types and return type of
/// the trait method at its mode.
/// Returns `false` if the comparison fails.
fn compare_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    (impl_m, impl_mode): (CallableDef<'db>, Option<BorrowKind>),
    (trait_m, trait_mode): (CallableDef<'db>, Option<BorrowKind>),
    param_subst: &CompleteSubst<'db>,
    listed: &[Shape<'db>],
    trait_inst: TraitInstId<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    let evidence = PredicateListId::new(db, vec![trait_inst]);
    let impl_assumptions = collect_func_def_constraints(db, impl_m, true).instantiate_identity();
    let trait_assumptions = instantiate_trait_method_template(
        db,
        collect_func_def_constraints(db, trait_m, true),
        param_subst,
    );
    let compare_assumptions = impl_assumptions
        .merge(db, trait_assumptions)
        .extend_all_bounds(db);
    let trait_scope = impl_m.scope();
    // A trait shape at the impl, where an associated shape it names is known.
    let trait_shape = |shape: Binder<'db, Shape<'db>>| {
        let shape = instantiate_trait_method_template(db, shape, param_subst);
        let shape = normalize_from_assumptions(db, shape, trait_scope, evidence);
        normalize_ty(db, shape, trait_scope, compare_assumptions)
    };

    // The trait method's parameters, with those its spread lists.
    let trait_params = trait_m
        .param_shapes(db, trait_mode)
        .0
        .into_iter()
        .map(trait_shape)
        .chain(listed.iter().cloned());
    let impl_params = impl_m.param_shapes(db, impl_mode).0;

    for (idx, (trait_param, impl_param)) in trait_params.zip(impl_params).enumerate() {
        // 1) Instantiate trait method's type params into the impl's generics
        let trait_m_ty = trait_param.erased_ty(db);
        if trait_m_ty.has_invalid(db) {
            continue;
        }
        // An impl's shape may name an associated shape its impl gives
        // (`Self::Step`).
        let impl_param = normalize_ty(
            db,
            impl_param.instantiate_identity(),
            trait_scope,
            compare_assumptions,
        );
        let impl_m_ty = impl_param.erased_ty(db);

        // 2) Substitute associated types using the provided trait instance.
        let trait_m_ty_substituted =
            normalize_from_assumptions(db, trait_m_ty, trait_scope, evidence);

        // 3) Normalize both types to resolve any further nested associated types.
        let trait_m_ty_normalized =
            normalize_ty(db, trait_m_ty_substituted, trait_scope, compare_assumptions);
        let impl_m_ty_normalized = normalize_ty(db, impl_m_ty, trait_scope, compare_assumptions);
        let trait_m_ty_normalized = normalize_compare_assoc_consts(
            db,
            trait_m_ty_normalized,
            trait_scope,
            compare_assumptions,
            trait_inst,
            true,
        );
        let impl_m_ty_normalized = normalize_compare_assoc_consts(
            db,
            impl_m_ty_normalized,
            trait_scope,
            compare_assumptions,
            trait_inst,
            false,
        );
        // 4) Compare for equality, including the parameter's mode, or its
        // parts' modes for a product.
        if !impl_m_ty.has_invalid(db)
            && (trait_m_ty_normalized != impl_m_ty_normalized
                || !trait_param.same_modes(db, &impl_param))
        {
            sink.push(
                ImplDiag::MethodArgTyMismatch {
                    trait_m,
                    impl_m,
                    trait_m_ty: trait_m_ty_normalized,
                    impl_m_ty: impl_m_ty_normalized,
                    param_idx: idx,
                    trait_mode,
                    impl_mode,
                    spread_part: (Some(trait_m.declared_param(db, idx)) == trait_m.spread(db))
                        .then_some(trait_param),
                }
                .into(),
            );
            err = true;
        }
    }

    // A trait return naming a shape without a mode is reported at the trait.
    if trait_m
        .ret_shape(db, trait_mode)
        .is_some_and(|shape| names_shape_without_mode(db, shape.skip_binder()))
    {
        return !err;
    }
    let impl_m_ret_ty = impl_m.ret_ty(db).instantiate_identity();
    let trait_m_ret_ty = instantiate_trait_method_template(db, trait_m.ret_ty(db), param_subst);

    // Substitute and normalize the return type as well.
    let trait_m_ret_ty_substituted =
        normalize_from_assumptions(db, trait_m_ret_ty, trait_scope, evidence);
    let trait_m_ret_ty_normalized = normalize_ty(
        db,
        trait_m_ret_ty_substituted,
        trait_scope,
        compare_assumptions,
    );
    let impl_m_ret_ty_normalized =
        normalize_ty(db, impl_m_ret_ty, trait_scope, compare_assumptions);
    let trait_m_ret_ty_normalized = normalize_compare_assoc_consts(
        db,
        trait_m_ret_ty_normalized,
        trait_scope,
        compare_assumptions,
        trait_inst,
        true,
    );
    let impl_m_ret_ty_normalized = normalize_compare_assoc_consts(
        db,
        impl_m_ret_ty_normalized,
        trait_scope,
        compare_assumptions,
        trait_inst,
        false,
    );
    // A projection's return modes are part of its signature.
    let same_modes = match (
        impl_m.ret_shape(db, impl_mode),
        trait_m.ret_shape(db, trait_mode),
    ) {
        (Some(impl_shape), Some(trait_ret)) => normalize_ty(
            db,
            impl_shape.skip_binder().clone(),
            trait_scope,
            compare_assumptions,
        )
        .same_modes(db, &trait_shape(trait_ret)),
        (impl_shape, trait_shape) => impl_shape.is_none() && trait_shape.is_none(),
    };
    if !impl_m_ret_ty.has_invalid(db)
        && !trait_m_ret_ty.has_invalid(db)
        && (trait_m_ret_ty_normalized != impl_m_ret_ty_normalized || !same_modes)
    {
        sink.push(
            ImplDiag::MethodRetTyMismatch {
                trait_m,
                impl_m,
                trait_ty: trait_m_ret_ty_normalized,
                impl_ty: impl_m_ret_ty_normalized,
                trait_mode,
                impl_mode,
            }
            .into(),
        );

        err = true;
    }

    !err
}

fn insert_param_mapping<'db>(
    db: &'db dyn HirAnalysisDb,
    out: &mut PartialSubst<'db>,
    from: TyId<'db>,
    to: TyId<'db>,
) {
    let Some(key) = out.domain().schema(db).original_key(db, from) else {
        return;
    };
    out.bind(db, key, to)
        .expect("method parameter correspondence must agree");
}

/// The trait method's params as the impl method's, the pairs of their
/// effects at the methods' modes, and whether every effect of the trait
/// method has its pair. An impl method's extra effects are stricter bounds.
pub(crate) fn trait_to_impl_param_subst<'db>(
    db: &'db dyn HirAnalysisDb,
    (impl_m, impl_mode): (CallableDef<'db>, Option<BorrowKind>),
    (trait_m, trait_mode): (CallableDef<'db>, Option<BorrowKind>),
    trait_inst: TraitInstId<'db>,
) -> (PartialSubst<'db>, Vec<(usize, usize)>, bool) {
    let schema = ParamSchemaId::callable(db, trait_m);
    let mut out = PartialSubst::new(db, ParamDomainId::full(db, schema));

    // Map inherited trait params (trait Self + trait generics) using the trait method's
    // lowered param prefix. These are the TyIds that actually occur inside the method's
    // signature and constraints.
    let trait_inst_args = trait_inst.args(db);
    let trait_m_params = trait_m.params(db);
    let trait_def_params = trait_inst.def(db).params(db);
    assert!(trait_m_params.len() >= trait_inst_args.len());
    assert!(trait_def_params.len() >= trait_inst_args.len());
    for (idx, &arg) in trait_inst_args.iter().enumerate() {
        if let Some(&trait_param) = trait_m_params.get(idx) {
            insert_param_mapping(db, &mut out, trait_param, arg);
        }
        if let Some(&trait_param) = trait_def_params.get(idx) {
            insert_param_mapping(db, &mut out, trait_param, arg);
        }
    }

    // Map explicit method generics by identity.
    for (&trait_param, &impl_param) in trait_m
        .explicit_params(db)
        .iter()
        .zip(impl_m.explicit_params(db).iter())
    {
        insert_param_mapping(db, &mut out, trait_param, impl_param);
    }

    let (effect_pairs, effects_paired) = map_effect_provider_params_by_identity(
        db,
        &mut out,
        (impl_m, impl_mode),
        (trait_m, trait_mode),
        trait_inst,
    );

    (out, effect_pairs, effects_paired)
}

#[derive(Debug, Clone, PartialEq, Eq, salsa::SalsaValue)]
pub(crate) struct MethodParamCorrespondence {
    pub body_to_trait_slots: Vec<Option<usize>>,
    pub trait_effect_to_body: Vec<(usize, usize)>,
}

/// The same declaration correspondence used by method conformance, at the
/// methods' modes, expressed as body slots so call elaboration cannot
/// reinterpret it by position. Resolving the same trait method at many call
/// sites asks for it repeatedly.
#[salsa::tracked]
pub(crate) fn method_param_correspondence<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    impl_mode: Option<BorrowKind>,
    trait_m: CallableDef<'db>,
    trait_mode: Option<BorrowKind>,
    trait_inst: TraitInstId<'db>,
) -> MethodParamCorrespondence {
    let (mapping, trait_effect_to_body, _) =
        trait_to_impl_param_subst(db, (impl_m, impl_mode), (trait_m, trait_mode), trait_inst);
    let impl_params = impl_m.params(db);
    let mut body_to_trait = vec![None; impl_params.len()];
    for (trait_slot, &trait_param) in trait_m.params(db).iter().enumerate() {
        let Some(key) = mapping.domain().schema(db).original_key(db, trait_param) else {
            continue;
        };
        let Some(body_param) = mapping.get(db, key) else {
            continue;
        };
        if let Some(body_slot) = impl_params.iter().position(|param| *param == body_param) {
            body_to_trait[body_slot] = Some(trait_slot);
        }
    }
    MethodParamCorrespondence {
        body_to_trait_slots: body_to_trait,
        trait_effect_to_body,
    }
}

#[derive(Clone, Copy)]
struct EffectProviderEntry<'db> {
    effect_idx: usize,
    provider_param: Option<TyId<'db>>,
    identity: CanonicalEffectIdentity<'db>,
}

fn map_effect_provider_params_by_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    out: &mut PartialSubst<'db>,
    (impl_m, impl_mode): (CallableDef<'db>, Option<BorrowKind>),
    (trait_m, trait_mode): (CallableDef<'db>, Option<BorrowKind>),
    trait_inst: TraitInstId<'db>,
) -> (Vec<(usize, usize)>, bool) {
    let assumptions = collect_func_def_constraints(db, impl_m, true).instantiate_identity();
    // An impl method splices the impl's own rows into its effects: a trait
    // method's entry naming one pairs with the splice, not by identity. A row
    // known to be empty needs no entry at all.
    let impl_func = match impl_m {
        CallableDef::Func(func) => Some(func),
        CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) => None,
    };
    let trait_entries: Vec<_> = collect_effect_provider_entries(
        db,
        (trait_m, trait_mode),
        Some(out),
        trait_inst,
        impl_m.scope(),
        assumptions,
    )
    .into_iter()
    .filter(|entry| {
        !entry.identity.key_row.is_some_and(|row| {
            row_is_empty(db, row, impl_m.scope(), assumptions)
                || row.inst.def(db) == trait_inst.def(db)
                    && row
                        .name(db)
                        .zip(impl_func)
                        .is_some_and(|(name, func)| func.row_splice(db, name).is_some())
        })
    })
    .collect();
    let impl_entries: Vec<_> = collect_effect_provider_entries(
        db,
        (impl_m, impl_mode),
        None,
        trait_inst,
        impl_m.scope(),
        assumptions,
    )
    .into_iter()
    .filter(|entry| !impl_func.is_some_and(|func| func.effect_from_row(db, entry.effect_idx)))
    .collect();
    let mut used_impl_entries = vec![false; impl_entries.len()];
    let mut effect_pairs = Vec::new();
    let trait_effects = trait_entries.len();

    for trait_entry in trait_entries {
        let Some((impl_idx, impl_entry)) =
            impl_entries.iter().enumerate().find(|(idx, impl_entry)| {
                !used_impl_entries[*idx]
                    && effect_identity_matches(db, trait_entry.identity, impl_entry.identity)
            })
        else {
            continue;
        };
        used_impl_entries[impl_idx] = true;
        effect_pairs.push((trait_entry.effect_idx, impl_entry.effect_idx));
        if let (Some(trait_param), Some(impl_param)) =
            (trait_entry.provider_param, impl_entry.provider_param)
        {
            insert_param_mapping(db, out, trait_param, impl_param);
        }
    }
    let paired = effect_pairs.len() == trait_effects;
    (effect_pairs, paired)
}

fn collect_effect_provider_entries<'db>(
    db: &'db dyn HirAnalysisDb,
    (method, mode): (CallableDef<'db>, Option<BorrowKind>),
    param_subst: Option<&PartialSubst<'db>>,
    trait_inst: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<EffectProviderEntry<'db>> {
    let CallableDef::Func(func) = method else {
        return Vec::new();
    };
    let param_subst = param_subst.map(|subst| subst.residualize(db));

    let provider_param_map = place_effect_provider_param_index_map(db, func);
    let params = method.params(db);

    func.effect_requirements(db, mode)
        .iter()
        .map(|binding| {
            let effect_idx = binding.binding_idx as usize;
            let provider_param = provider_param_map
                .get(binding.binding_idx as usize)
                .copied()
                .flatten()
                .and_then(|provider_param_idx| params.get(provider_param_idx).copied());
            let mut binding = binding.clone();
            if let Some(subst) = &param_subst {
                binding.key = match binding.key {
                    crate::core::semantic::EffectRequirementKey::Type(key_ty) => {
                        crate::core::semantic::EffectRequirementKey::Type(
                            instantiate_trait_method_template(
                                db,
                                Binder::bind(method.generic_owner(), key_ty),
                                subst,
                            ),
                        )
                    }
                    crate::core::semantic::EffectRequirementKey::Trait(key_trait) => {
                        crate::core::semantic::EffectRequirementKey::Trait(
                            instantiate_trait_method_template(
                                db,
                                Binder::bind(method.generic_owner(), key_trait),
                                subst,
                            ),
                        )
                    }
                    crate::core::semantic::EffectRequirementKey::Row(row) => {
                        crate::core::semantic::EffectRequirementKey::Row(
                            instantiate_trait_method_template(
                                db,
                                Binder::bind(method.generic_owner(), row),
                                subst,
                            ),
                        )
                    }
                    crate::core::semantic::EffectRequirementKey::Other => {
                        crate::core::semantic::EffectRequirementKey::Other
                    }
                };
            }
            let rebase_same_trait_uses = param_subst.is_some();
            let mut identity = canonical_effect_identity_for_binding(
                db,
                &binding,
                scope,
                assumptions,
                Some(trait_inst),
                EffectKeyCanonMode::Compare,
            );
            identity.key_ty = identity.key_ty.map(|ty| {
                normalize_compare_assoc_consts(
                    db,
                    normalize_ty(db, ty, scope, assumptions),
                    scope,
                    assumptions,
                    trait_inst,
                    rebase_same_trait_uses,
                )
            });
            let normalize = |inst| {
                normalize_predicate_for_comparison(
                    db,
                    inst,
                    scope,
                    assumptions,
                    trait_inst,
                    rebase_same_trait_uses,
                )
            };
            identity.key_trait = identity.key_trait.map(normalize);
            identity.key_row = identity.key_row.map(|row| RowKey {
                inst: normalize(row.inst),
                ..row
            });

            EffectProviderEntry {
                effect_idx,
                provider_param,
                identity,
            }
        })
        .collect()
}

pub(crate) fn trait_effect_key_matches_with<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TraitInstId<'db>,
    actual: TraitInstId<'db>,
    mut cmp: impl FnMut(TyId<'db>, TyId<'db>) -> bool,
) -> bool {
    if expected.def(db) != actual.def(db) || expected.args(db).len() != actual.args(db).len() {
        return false;
    }

    let expected_assoc = expected.assoc_type_bindings(db);
    let actual_assoc = actual.assoc_type_bindings(db);
    if expected_assoc.len() != actual_assoc.len() {
        return false;
    }

    for (&expected_arg, &actual_arg) in expected
        .args(db)
        .iter()
        .skip(1)
        .zip(actual.args(db).iter().skip(1))
    {
        if !cmp(expected_arg, actual_arg) {
            return false;
        }
    }

    for (name, &expected_ty) in expected_assoc {
        let Some(&actual_ty) = actual_assoc.get(name) else {
            return false;
        };
        if !cmp(expected_ty, actual_ty) {
            return false;
        }
    }

    true
}

fn effect_identity_matches<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_identity: CanonicalEffectIdentity<'db>,
    impl_identity: CanonicalEffectIdentity<'db>,
) -> bool {
    if trait_identity.key_kind != impl_identity.key_kind
        || trait_identity.is_mut != impl_identity.is_mut
    {
        return false;
    }

    match trait_identity.key_kind {
        EffectKeyKind::Type => match (trait_identity.key_ty, impl_identity.key_ty) {
            (Some(trait_key_ty), Some(impl_key_ty)) => {
                effect_identity_tys_match(db, trait_key_ty, impl_key_ty)
            }
            _ => false,
        },
        EffectKeyKind::Trait => match (trait_identity.key_trait, impl_identity.key_trait) {
            (Some(trait_key_trait), Some(impl_key_trait)) => {
                trait_effect_key_matches_with(db, trait_key_trait, impl_key_trait, |lhs, rhs| {
                    effect_identity_tys_match(db, lhs, rhs)
                })
            }
            _ => false,
        },
        // A row is its self type's, unlike a trait key, whose `Self` is the
        // provider.
        EffectKeyKind::Row => match (trait_identity.key_row, impl_identity.key_row) {
            (Some(trait_row), Some(impl_row)) => {
                trait_row.row == impl_row.row
                    && effect_identity_tys_match(
                        db,
                        trait_row.inst.self_ty(db),
                        impl_row.inst.self_ty(db),
                    )
                    && trait_effect_key_matches_with(
                        db,
                        trait_row.inst,
                        impl_row.inst,
                        |lhs, rhs| effect_identity_tys_match(db, lhs, rhs),
                    )
            }
            _ => false,
        },
        EffectKeyKind::Other => trait_identity.key_syntax == impl_identity.key_syntax,
    }
}

fn effect_identity_tys_match<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TyId<'db>,
    actual: TyId<'db>,
) -> bool {
    UnificationTable::new(db).unify(expected, actual).is_ok()
}

fn instantiate_trait_method_template<'db, T>(
    db: &'db dyn HirAnalysisDb,
    binder: Binder<'db, T>,
    param_subst: &CompleteSubst<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    binder
        .instantiate_subst(db, param_subst)
        .expect("trait method comparison uses the declared parameter domain")
}

pub(crate) fn normalize_predicate_for_comparison<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
    rebase_same_trait_uses: bool,
) -> TraitInstId<'db> {
    let args: Vec<_> = pred
        .args(db)
        .iter()
        .copied()
        .map(|ty| {
            let ty = normalize_ty(db, ty, scope, assumptions);
            normalize_compare_assoc_consts(
                db,
                ty,
                scope,
                assumptions,
                trait_inst,
                rebase_same_trait_uses,
            )
        })
        .collect();
    let mut assoc_type_bindings: Vec<_> = pred
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, &ty)| {
            let ty = normalize_ty(db, ty, scope, assumptions);
            (
                *name,
                normalize_compare_assoc_consts(
                    db,
                    ty,
                    scope,
                    assumptions,
                    trait_inst,
                    rebase_same_trait_uses,
                ),
            )
        })
        .collect();
    assoc_type_bindings.sort_by(|(lhs, _), (rhs, _)| lhs.canonical_cmp(db, rhs));
    TraitInstId::new(
        db,
        pred.def(db),
        args,
        assoc_type_bindings.into_iter().collect::<IndexMap<_, _>>(),
        pred.mode(db),
    )
}

fn normalize_predicates_for_comparison<'db>(
    db: &'db dyn HirAnalysisDb,
    preds: PredicateListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
    rebase_same_trait_uses: bool,
) -> PredicateListId<'db> {
    PredicateListId::new(
        db,
        preds
            .list(db)
            .iter()
            .copied()
            .map(|pred| {
                normalize_predicate_for_comparison(
                    db,
                    pred,
                    scope,
                    assumptions,
                    trait_inst,
                    rebase_same_trait_uses,
                )
            })
            .collect::<Vec<_>>(),
    )
}

pub(crate) fn normalize_compare_assoc_consts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: super::trait_resolution::PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
    rebase_same_trait_uses: bool,
) -> TyId<'db> {
    fn is_identity_trait_inst<'db>(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> bool {
        let trait_def = inst.def(db);
        inst.assoc_type_bindings(db).is_empty() && inst.args(db) == trait_def.params(db)
    }

    let ty = normalize_const_tys_for_comparison(db, ty);

    struct CompareAssocConstNormalizer<'db> {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        assumptions: super::trait_resolution::PredicateListId<'db>,
        trait_inst: TraitInstId<'db>,
        rebase_same_trait_uses: bool,
    }

    impl<'db> CompareAssocConstNormalizer<'db> {
        fn canonicalize_assoc_use(
            &self,
            db: &'db dyn HirAnalysisDb,
            assoc: AssocConstUse<'db>,
        ) -> AssocConstUse<'db> {
            let assoc = assoc.with_env(self.scope, self.assumptions);
            if self.rebase_same_trait_uses
                && assoc.inst().def(db) == self.trait_inst.def(db)
                && is_identity_trait_inst(db, assoc.inst())
            {
                assoc.with_inst(self.trait_inst)
            } else {
                assoc
            }
        }

        fn normalize_assoc_const_ty(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            assoc: AssocConstUse<'db>,
            expected_ty: TyId<'db>,
        ) -> Option<TyId<'db>> {
            let assoc = self.canonicalize_assoc_use(db, assoc);
            let const_ty = const_ty_from_assoc_const_use(db, assoc)?;
            let evaluated = const_ty.evaluate(db, Some(expected_ty));
            if matches!(
                evaluated.ty(db).invalid_cause(db),
                Some(InvalidCause::ConstEvalUnsupported { .. })
            ) {
                return None;
            }

            Some(self.fold_ty(db, TyId::new(db, TyData::ConstTy(evaluated))))
        }

        fn normalize_unevaluated_assoc_const(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            body: crate::hir_def::Body<'db>,
            expected_ty: TyId<'db>,
            capture: &super::const_ty::ConstCaptureEnv<'db>,
        ) -> Option<TyId<'db>> {
            let (diags, typed_body) = check_anon_const_body(db, body, expected_ty);
            if !diags.is_empty() {
                return None;
            }

            let typed_body = capture.specialize(db, typed_body.clone());
            let ConstRef::TraitConst { use_: assoc, .. } =
                typed_body.expr_const_ref(body.expr(db))?
            else {
                return None;
            };

            self.normalize_assoc_const_ty(db, assoc, expected_ty)
        }

        fn normalize_unevaluated_const_fn_call(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            const_ty: ConstTyId<'db>,
            expected_ty: TyId<'db>,
        ) -> Option<TyId<'db>> {
            let evaluated = const_ty.evaluate(db, Some(expected_ty));
            if evaluated == const_ty || evaluated.ty(db).invalid_cause(db).is_some() {
                return None;
            }
            if !matches!(
                evaluated.data(db),
                ConstTyData::Value(..) | ConstTyData::Description(..) | ConstTyData::Abstract(..)
            ) {
                return None;
            }
            Some(self.fold_ty(db, TyId::new(db, TyData::ConstTy(evaluated))))
        }
    }

    impl<'db> TyFolder<'db> for CompareAssocConstNormalizer<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let ty = ty.super_fold_with(db, self);
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return ty;
            };

            match const_ty.data(db) {
                ConstTyData::Abstract(expr, expected_ty) => {
                    let ConstExpr::TraitConst(assoc) = expr.data(db) else {
                        return ty;
                    };
                    let assoc = self.canonicalize_assoc_use(db, *assoc);
                    if let Some(ty) = self.normalize_assoc_const_ty(db, assoc, *expected_ty) {
                        return ty;
                    }
                    let expr =
                        super::const_expr::ConstExprId::new(db, ConstExpr::TraitConst(assoc));
                    TyId::new(
                        db,
                        TyData::ConstTy(super::const_ty::ConstTyId::new(
                            db,
                            ConstTyData::Abstract(expr, *expected_ty),
                        )),
                    )
                }
                ConstTyData::UnEvaluated {
                    body,
                    ty: expected_ty,
                    const_def,
                    capture,
                    ..
                } => {
                    if const_def.is_some() {
                        return ty;
                    }
                    let Some(expected_ty) = *expected_ty else {
                        return ty;
                    };
                    let expr = body.expr(db);
                    let Partial::Present(Expr::Path(path)) = expr.data(db, *body) else {
                        return self
                            .normalize_unevaluated_const_fn_call(db, *const_ty, expected_ty)
                            .unwrap_or(ty);
                    };
                    let Some(path) = path.to_opt() else {
                        return ty;
                    };
                    let Some(parent) = path.parent(db) else {
                        return ty;
                    };
                    let PathKind::Ident {
                        generic_args: path_generic_args,
                        ..
                    } = path.kind(db)
                    else {
                        return ty;
                    };
                    if !parent.is_self_ty(db) || !path_generic_args.is_empty(db) {
                        return self
                            .normalize_unevaluated_const_fn_call(db, *const_ty, expected_ty)
                            .unwrap_or(ty);
                    }

                    self.normalize_unevaluated_assoc_const(db, *body, expected_ty, capture)
                        .unwrap_or(ty)
                }
                _ => ty,
            }
        }
    }

    let mut folder = CompareAssocConstNormalizer {
        scope,
        assumptions,
        trait_inst,
        rebase_same_trait_uses,
    };
    let ty = canonicalize_ty_for_mode(
        db,
        ty.fold_with(db, &mut folder),
        ConstCanonEnv::new(scope, assumptions, None),
        ConstCanonMode::Identity,
    );
    ty.fold_with(db, &mut folder)
}

/// Checks if the method constraints are stricter than the trait constraints.
/// This check is performed by checking if the `impl_method` constraints are
/// satisfied under the assumptions that is obtained from the `expected_method`
/// constraints.
/// Returns `false` if the comparison fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ConstraintEntailment {
    Proven,
    Disproven,
    Incomplete,
}

pub(crate) fn classify_constraint_entailment(
    result: GoalSatisfiability<'_>,
) -> ConstraintEntailment {
    match result {
        GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {
            ConstraintEntailment::Proven
        }
        // Method comparison asks only whether a proof exists, so one admitted
        // answer is sufficient even if uniqueness or saturation is unknown.
        GoalSatisfiability::NeedsConfirmation { solutions, .. } if !solutions.is_empty() => {
            ConstraintEntailment::Proven
        }
        GoalSatisfiability::NeedsConfirmation { .. } => ConstraintEntailment::Incomplete,
        GoalSatisfiability::UnSat(_) => ConstraintEntailment::Disproven,
    }
}

fn compare_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    (impl_m, impl_mode): (CallableDef<'db>, Option<BorrowKind>),
    (trait_m, trait_mode): (CallableDef<'db>, Option<BorrowKind>),
    trait_inst: TraitInstId<'db>,
    param_subst: &CompleteSubst<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_constraints = collect_func_def_constraints(db, impl_m, false)
        .instantiate_identity()
        .at_mode(db, impl_mode);
    let trait_m_constraints = instantiate_trait_method_template(
        db,
        collect_func_def_constraints(db, trait_m, false),
        param_subst,
    )
    .at_mode(db, trait_mode);
    let compare_assumptions = impl_m_constraints
        .merge(db, trait_m_constraints)
        .extend_all_bounds(db);
    let impl_m_constraints = normalize_predicates_for_comparison(
        db,
        impl_m_constraints,
        trait_m.scope(),
        compare_assumptions,
        trait_inst,
        false,
    );
    let trait_m_constraints = normalize_predicates_for_comparison(
        db,
        trait_m_constraints,
        trait_m.scope(),
        compare_assumptions,
        trait_inst,
        true,
    );
    // A row's components bound their providers as the row's implementation
    // requires; the trait method's row stands for them.
    let row_providers: Vec<TyId<'db>> = match impl_m {
        CallableDef::Func(func) => place_effect_provider_param_index_map(db, func)
            .iter()
            .enumerate()
            .filter(|(effect, _)| func.effect_from_row(db, *effect))
            .filter_map(|(_, param)| impl_m.params(db).get((*param)?).copied())
            .collect(),
        CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) => Vec::new(),
    };
    let mut unsatisfied_goals = ThinVec::new();
    for &goal in impl_m_constraints.list(db) {
        if row_providers.contains(&goal.self_ty(db)) {
            continue;
        }
        // Source-level concrete assumptions are rejected, so the solver does
        // not consult assumptions for concrete goals. Method comparison can
        // legitimately produce a concrete goal after substituting the owning
        // impl, and an identical declared trait bound entails itself without
        // requiring trait selection.
        if trait_m_constraints.list(db).contains(&goal) {
            continue;
        }
        let entailment = classify_constraint_entailment(is_goal_satisfiable(
            db,
            TraitSolveCx::new(db, trait_m.scope()).with_assumptions(trait_m_constraints),
            goal,
        ));
        match entailment {
            ConstraintEntailment::Proven => {}
            // An incomplete search cannot establish that the trait method
            // entails the impl method's bound. Diagnose it conservatively
            // instead of accepting a potentially stricter implementation.
            ConstraintEntailment::Disproven | ConstraintEntailment::Incomplete => {
                unsatisfied_goals.push(goal);
            }
        }
    }

    if unsatisfied_goals.is_empty() {
        true
    } else {
        sink.push(
            ImplDiag::MethodStricterBound {
                span: impl_m.name_span(),
                stricter_bounds: unsatisfied_goals,
            }
            .into(),
        );
        false
    }
}
