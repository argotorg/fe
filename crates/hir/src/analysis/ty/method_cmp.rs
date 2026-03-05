use thin_vec::ThinVec;

use super::{
    binder::Binder,
    canonical::Canonical,
    const_expr::ConstExpr,
    const_ty::{ConstTyData, const_ty_from_trait_const},
    diagnostics::{ImplDiag, TyDiagCollection},
    effects::{EffectKeyKind, place_effect_provider_param_index_map},
    fold::{AssocTySubst, TyFoldable, TyFolder},
    layout_holes::{
        alpha_rename_hidden_layout_placeholders, callable_input_layout_implicit_params_by_origin,
    },
    normalize::normalize_ty,
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitSolveCx,
        constraint::collect_func_def_constraints, is_goal_satisfiable,
    },
    ty_def::{InvalidCause, TyData, TyId},
    ty_lower::CallableInputLayoutHoleOrigin,
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{CallableDef, Expr, Partial, PathId, PathKind, scope_graph::ScopeId};
use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;

type ParamSubstMap<'db> = FxHashMap<(ScopeId<'db>, usize), TyId<'db>>;

/// Compares the implementation method with the trait method to ensure they
/// match.
///
/// This function performs the following checks:
///
/// 1. Number of generic parameters.
/// 2. Kinds of generic parameters.
/// 3. Arity (number of arguments).
/// 4. Argument labels.
/// 5. Argument types and return type.
/// 6. Method constraints.
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
pub(super) fn compare_impl_method<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    trait_inst: TraitInstId<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) {
    if !compare_generic_param_num(db, impl_m, trait_m, sink) {
        return;
    }

    if !compare_generic_param_kind(db, impl_m, trait_m, sink) {
        return;
    }

    if !compare_arity(db, impl_m, trait_m, sink) {
        return;
    }

    // Compare the argument labels, argument types, and return type of the impl
    // method with the trait method.
    let mut err = !compare_arg_label(db, impl_m, trait_m, sink);

    let param_subst = trait_to_impl_param_subst(db, impl_m, trait_m, trait_inst);
    err |= !compare_ty(db, impl_m, trait_m, &param_subst, trait_inst, sink);
    if err {
        return;
    }

    compare_constraints(db, impl_m, trait_m, &param_subst, sink);
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

/// Checks if the arity of the implemented method is the same as the arity of
/// the trait method.
/// Returns `false` if the comparison fails.
fn compare_arity<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_arity = impl_m.arg_tys(db).len();
    let trait_m_arity = trait_m.arg_tys(db).len();

    // Checks if the arity are the same.
    if impl_m_arity == trait_m_arity {
        true
    } else {
        sink.push(ImplDiag::MethodArgNumMismatch { impl_m, trait_m }.into());
        false
    }
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
    let len = impl_m.arg_tys(db).len().min(trait_m.arg_tys(db).len());
    for idx in 0..len {
        let Some(expected_label) = trait_m.param_label_or_name(db, idx) else {
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

/// Checks if the argument types and return type of the implemented method are
/// the same as the argument types and return type of the trait method.
/// Returns `false` if the comparison fails.
fn compare_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    param_subst: &ParamSubstMap<'db>,
    trait_inst: TraitInstId<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    let impl_m_arg_tys = impl_m.arg_tys(db);
    let trait_m_arg_tys = trait_m.arg_tys(db);

    let mut substituter = AssocTySubst::new(trait_inst);
    let assumptions = collect_func_def_constraints(db, impl_m, true).instantiate_identity();
    let solve_cx = TraitSolveCx::new(db, impl_m.scope()).with_assumptions(assumptions);

    for (idx, (trait_m_ty, impl_m_ty)) in trait_m_arg_tys
        .iter()
        .zip(impl_m_arg_tys.iter())
        .enumerate()
    {
        // 1) Instantiate trait method's type params into the impl's generics
        let trait_m_ty = instantiate_with_partial_map(db, *trait_m_ty, param_subst);
        if trait_m_ty.has_invalid(db) {
            continue;
        }
        let impl_m_ty = impl_m_ty.instantiate_identity();

        // 2) Substitute associated types using the provided trait instance.
        let trait_m_ty_substituted = trait_m_ty.fold_with(db, &mut substituter);

        // 3) Normalize both types to resolve any further nested associated types.
        let trait_m_ty_normalized =
            normalize_ty(db, trait_m_ty_substituted, impl_m.scope(), assumptions);
        let impl_m_ty_normalized = normalize_ty(db, impl_m_ty, impl_m.scope(), assumptions);
        let trait_m_ty_normalized =
            normalize_const_tys(db, trait_m_ty_normalized, solve_cx, trait_inst);
        let impl_m_ty_normalized =
            normalize_const_tys(db, impl_m_ty_normalized, solve_cx, trait_inst);
        let trait_m_ty_normalized = alpha_rename_hidden_layout_placeholders(
            db,
            trait_m_ty_normalized,
            impl_m_ty_normalized,
        );

        // 4) Compare for equality
        if !impl_m_ty.has_invalid(db) && trait_m_ty_normalized != impl_m_ty_normalized {
            sink.push(
                ImplDiag::MethodArgTyMismatch {
                    trait_m,
                    impl_m,
                    trait_m_ty: trait_m_ty_normalized,
                    impl_m_ty: impl_m_ty_normalized,
                    param_idx: idx,
                }
                .into(),
            );
            err = true;
        }
    }

    let impl_m_ret_ty = impl_m.ret_ty(db).instantiate_identity();
    let trait_m_ret_ty = instantiate_with_partial_map(db, trait_m.ret_ty(db), param_subst);

    // Substitute and normalize the return type as well.
    let trait_m_ret_ty_substituted = trait_m_ret_ty.fold_with(db, &mut substituter);
    let trait_m_ret_ty_normalized =
        normalize_ty(db, trait_m_ret_ty_substituted, impl_m.scope(), assumptions);
    let impl_m_ret_ty_normalized = normalize_ty(db, impl_m_ret_ty, impl_m.scope(), assumptions);
    let trait_m_ret_ty_normalized =
        normalize_const_tys(db, trait_m_ret_ty_normalized, solve_cx, trait_inst);
    let impl_m_ret_ty_normalized =
        normalize_const_tys(db, impl_m_ret_ty_normalized, solve_cx, trait_inst);
    let trait_m_ret_ty_normalized = alpha_rename_hidden_layout_placeholders(
        db,
        trait_m_ret_ty_normalized,
        impl_m_ret_ty_normalized,
    );

    if !impl_m_ret_ty.has_invalid(db)
        && !trait_m_ret_ty.has_invalid(db)
        && trait_m_ret_ty_normalized != impl_m_ret_ty_normalized
    {
        sink.push(
            ImplDiag::MethodRetTyMismatch {
                trait_m,
                impl_m,
                trait_ty: trait_m_ret_ty_normalized,
                impl_ty: impl_m_ret_ty_normalized,
            }
            .into(),
        );

        err = true;
    }

    !err
}

fn param_owner_and_idx<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<(ScopeId<'db>, usize)> {
    match ty.data(db) {
        TyData::TyParam(param) => Some((param.owner, param.idx)),
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::TyParam(param, _) => Some((param.owner, param.idx)),
            _ => None,
        },
        _ => None,
    }
}

fn insert_param_mapping<'db>(
    db: &'db dyn HirAnalysisDb,
    out: &mut ParamSubstMap<'db>,
    from: TyId<'db>,
    to: TyId<'db>,
) {
    let Some(key) = param_owner_and_idx(db, from) else {
        return;
    };
    out.insert(key, to);
}

fn trait_to_impl_param_subst<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    trait_inst: TraitInstId<'db>,
) -> ParamSubstMap<'db> {
    let mut out: ParamSubstMap<'db> = FxHashMap::default();

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

    let trait_layout = callable_input_layout_implicit_params_by_origin(db, trait_m);
    let impl_layout = callable_input_layout_implicit_params_by_origin(db, impl_m);

    // Map receiver/value-param layout implicit const params by stable origin.
    for (&origin, trait_params) in &trait_layout {
        match origin {
            CallableInputLayoutHoleOrigin::Receiver
            | CallableInputLayoutHoleOrigin::ValueParam(_) => {
                let Some(impl_params) = impl_layout.get(&origin) else {
                    continue;
                };
                if trait_params.len() != impl_params.len() {
                    continue;
                }
                for (&trait_p, &impl_p) in trait_params.iter().zip(impl_params.iter()) {
                    insert_param_mapping(db, &mut out, trait_p, impl_p);
                }
            }
            CallableInputLayoutHoleOrigin::Effect(_) => {}
        }
    }

    map_effect_provider_params_by_identity(
        db,
        &mut out,
        impl_m,
        trait_m,
        trait_inst,
        &trait_layout,
        &impl_layout,
    );

    out
}

#[derive(Clone, Copy)]
struct EffectIdentity<'db> {
    key_kind: EffectKeyKind,
    key_ty: Option<TyId<'db>>,
    key_trait: Option<TraitInstId<'db>>,
    key_path: PathId<'db>,
    is_mut: bool,
}

#[derive(Clone, Copy)]
struct EffectProviderEntry<'db> {
    effect_idx: usize,
    provider_param: TyId<'db>,
    identity: EffectIdentity<'db>,
}

fn map_effect_provider_params_by_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    out: &mut ParamSubstMap<'db>,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    trait_inst: TraitInstId<'db>,
    trait_layout: &FxHashMap<CallableInputLayoutHoleOrigin, Vec<TyId<'db>>>,
    impl_layout: &FxHashMap<CallableInputLayoutHoleOrigin, Vec<TyId<'db>>>,
) {
    let assumptions = collect_func_def_constraints(db, impl_m, true).instantiate_identity();
    let trait_entries = collect_effect_provider_entries(
        db,
        trait_m,
        Some(out),
        trait_inst,
        impl_m.scope(),
        assumptions,
    );
    let impl_entries =
        collect_effect_provider_entries(db, impl_m, None, trait_inst, impl_m.scope(), assumptions);
    let mut used_impl_entries = vec![false; impl_entries.len()];

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

        insert_param_mapping(
            db,
            out,
            trait_entry.provider_param,
            impl_entry.provider_param,
        );

        let trait_origin = CallableInputLayoutHoleOrigin::Effect(trait_entry.effect_idx);
        let impl_origin = CallableInputLayoutHoleOrigin::Effect(impl_entry.effect_idx);
        if let (Some(trait_params), Some(impl_params)) = (
            trait_layout.get(&trait_origin),
            impl_layout.get(&impl_origin),
        ) {
            if trait_params.len() != impl_params.len() {
                continue;
            }
            for (&trait_p, &impl_p) in trait_params.iter().zip(impl_params.iter()) {
                insert_param_mapping(db, out, trait_p, impl_p);
            }
        }
    }
}

fn collect_effect_provider_entries<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
    param_subst: Option<&ParamSubstMap<'db>>,
    trait_inst: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<EffectProviderEntry<'db>> {
    let CallableDef::Func(func) = method else {
        return Vec::new();
    };

    let provider_param_map = place_effect_provider_param_index_map(db, func);
    let params = method.params(db);

    func.effect_bindings(db)
        .iter()
        .filter_map(|binding| {
            let effect_idx = binding.binding_idx as usize;
            let provider_param_idx = provider_param_map
                .get(binding.binding_idx as usize)
                .copied()
                .flatten()?;
            let provider_param = *params.get(provider_param_idx)?;
            let key_ty = binding.key_ty.map(|key_ty| {
                let key_ty = param_subst.map_or(key_ty, |subst| {
                    instantiate_with_partial_map(db, Binder::bind(key_ty), subst)
                });
                normalize_effect_identity_ty(db, key_ty, scope, assumptions, trait_inst)
            });
            let key_trait = binding.key_trait.map(|key_trait| {
                let key_trait = param_subst.map_or(key_trait, |subst| {
                    instantiate_with_partial_map(db, Binder::bind(key_trait), subst)
                });
                normalize_effect_identity_trait(db, key_trait, scope, assumptions, trait_inst)
            });
            Some(EffectProviderEntry {
                effect_idx,
                provider_param,
                identity: EffectIdentity {
                    key_kind: binding.key_kind,
                    key_ty,
                    key_trait,
                    key_path: binding.binding_path,
                    is_mut: binding.is_mut,
                },
            })
        })
        .collect()
}

fn normalize_effect_identity_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
) -> TyId<'db> {
    let mut substituter = AssocTySubst::new(trait_inst);
    let ty = ty.fold_with(db, &mut substituter);
    let ty = normalize_ty(db, ty, scope, assumptions);
    normalize_const_tys(
        db,
        ty,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
        trait_inst,
    )
}

fn normalize_effect_identity_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
) -> TraitInstId<'db> {
    let mut substituter = AssocTySubst::new(trait_inst);
    let trait_key = trait_key.fold_with(db, &mut substituter);
    let args: Vec<TyId<'db>> = trait_key
        .args(db)
        .iter()
        .copied()
        .map(|ty| normalize_effect_identity_ty(db, ty, scope, assumptions, trait_inst))
        .collect();
    let assoc_type_bindings: IndexMap<_, _> = trait_key
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, &ty)| {
            (
                *name,
                normalize_effect_identity_ty(db, ty, scope, assumptions, trait_inst),
            )
        })
        .collect();
    TraitInstId::new(db, trait_key.def(db), args, assoc_type_bindings)
}

fn effect_identity_matches<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_identity: EffectIdentity<'db>,
    impl_identity: EffectIdentity<'db>,
) -> bool {
    if trait_identity.key_kind != impl_identity.key_kind
        || trait_identity.is_mut != impl_identity.is_mut
    {
        return false;
    }

    match trait_identity.key_kind {
        EffectKeyKind::Type => match (trait_identity.key_ty, impl_identity.key_ty) {
            (Some(trait_key_ty), Some(impl_key_ty)) => {
                alpha_rename_hidden_layout_placeholders(db, trait_key_ty, impl_key_ty)
                    == impl_key_ty
            }
            _ => false,
        },
        EffectKeyKind::Trait => match (trait_identity.key_trait, impl_identity.key_trait) {
            (Some(trait_key_trait), Some(impl_key_trait))
                if trait_key_trait.def(db) == impl_key_trait.def(db)
                    && trait_key_trait.args(db).len() == impl_key_trait.args(db).len() =>
            {
                let trait_args = trait_key_trait.args(db);
                let impl_args = impl_key_trait.args(db);
                let args_match = trait_args.iter().skip(1).zip(impl_args.iter().skip(1)).all(
                    |(&trait_arg, &impl_arg)| {
                        alpha_rename_hidden_layout_placeholders(db, trait_arg, impl_arg) == impl_arg
                    },
                );
                if !args_match {
                    return false;
                }

                let trait_assoc = trait_key_trait.assoc_type_bindings(db);
                let impl_assoc = impl_key_trait.assoc_type_bindings(db);
                trait_assoc.len() == impl_assoc.len()
                    && trait_assoc.iter().all(|(name, &trait_ty)| {
                        impl_assoc.get(name).is_some_and(|&impl_ty| {
                            alpha_rename_hidden_layout_placeholders(db, trait_ty, impl_ty)
                                == impl_ty
                        })
                    })
            }
            _ => false,
        },
        EffectKeyKind::Other => trait_identity.key_path == impl_identity.key_path,
    }
}

fn instantiate_with_partial_map<'db, T>(
    db: &'db dyn HirAnalysisDb,
    binder: Binder<T>,
    param_subst: &ParamSubstMap<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    binder.instantiate_with(db, |param_ty| {
        let Some(key) = param_owner_and_idx(db, param_ty) else {
            return param_ty;
        };
        param_subst.get(&key).copied().unwrap_or(param_ty)
    })
}

fn normalize_const_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    solve_cx: TraitSolveCx<'db>,
    trait_inst: TraitInstId<'db>,
) -> TyId<'db> {
    struct ConstFolder<'db> {
        solve_cx: TraitSolveCx<'db>,
        trait_inst: TraitInstId<'db>,
    }

    impl<'db> TyFolder<'db> for ConstFolder<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return ty.super_fold_with(db, self);
            };

            match const_ty.data(db) {
                super::const_ty::ConstTyData::Abstract(expr, expected_ty) => {
                    let ConstExpr::TraitConst { inst, name } = expr.data(db) else {
                        return ty.super_fold_with(db, self);
                    };

                    let Some(const_ty) = const_ty_from_trait_const(db, self.solve_cx, *inst, *name)
                    else {
                        return ty.super_fold_with(db, self);
                    };

                    let evaluated = const_ty.evaluate(db, Some(*expected_ty));
                    if matches!(
                        evaluated.ty(db).invalid_cause(db),
                        Some(InvalidCause::ConstEvalUnsupported { .. })
                    ) {
                        return ty.super_fold_with(db, self);
                    }

                    TyId::new(db, TyData::ConstTy(evaluated))
                }
                super::const_ty::ConstTyData::UnEvaluated {
                    body,
                    ty: expected_ty,
                    ..
                } => {
                    let Some(expected_ty) = *expected_ty else {
                        return ty.super_fold_with(db, self);
                    };
                    let expr = body.expr(db);
                    let Partial::Present(expr) = expr.data(db, *body) else {
                        return ty.super_fold_with(db, self);
                    };
                    let Expr::Path(path) = expr else {
                        return ty.super_fold_with(db, self);
                    };
                    let Some(path) = path.to_opt() else {
                        return ty.super_fold_with(db, self);
                    };

                    let mut const_ty = *const_ty;
                    if let Some(parent) = path.parent(db)
                        && parent.is_self_ty(db)
                        && let PathKind::Ident {
                            ident,
                            generic_args,
                        } = path.kind(db)
                        && generic_args.is_empty(db)
                        && let Some(name) = ident.to_opt()
                        && let Some(repl) =
                            const_ty_from_trait_const(db, self.solve_cx, self.trait_inst, name)
                    {
                        const_ty = repl;
                    }

                    TyId::new(
                        db,
                        TyData::ConstTy(const_ty.evaluate(db, Some(expected_ty))),
                    )
                }
                _ => ty.super_fold_with(db, self),
            }
        }
    }

    let mut folder = ConstFolder {
        solve_cx,
        trait_inst,
    };
    ty.fold_with(db, &mut folder)
}

/// Checks if the method constraints are stricter than the trait constraints.
/// This check is performed by checking if the `impl_method` constraints are
/// satisfied under the assumptions that is obtained from the `expected_method`
/// constraints.
/// Returns `false` if the comparison fails.
fn compare_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: CallableDef<'db>,
    trait_m: CallableDef<'db>,
    param_subst: &ParamSubstMap<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_constraints = collect_func_def_constraints(db, impl_m, false).instantiate_identity();
    let trait_m_constraints = instantiate_with_partial_map(
        db,
        collect_func_def_constraints(db, trait_m, false),
        param_subst,
    );
    let mut unsatisfied_goals = ThinVec::new();
    for &goal in impl_m_constraints.list(db) {
        let canonical_goal = Canonical::new(db, goal);
        match is_goal_satisfiable(
            db,
            TraitSolveCx::new(db, trait_m.scope()).with_assumptions(trait_m_constraints),
            canonical_goal,
        ) {
            GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
            GoalSatisfiability::NeedsConfirmation(_) => unreachable!(),
            GoalSatisfiability::UnSat(_) => {
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
