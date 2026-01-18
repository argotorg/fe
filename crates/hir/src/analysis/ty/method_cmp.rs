use thin_vec::ThinVec;

use super::{
    canonical::Canonical,
    const_ty::const_ty_from_trait_const,
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::{AssocTySubst, TyFoldable, TyFolder},
    normalize::normalize_ty,
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, constraint::collect_func_def_constraints, is_goal_satisfiable,
    },
    ty_def::{TyData, TyId},
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{CallableDef, Expr, Partial, PathKind};

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

    let map_to_impl: Vec<_> = trait_inst
        .args(db)
        .iter()
        .chain(impl_m.explicit_params(db).iter())
        .copied()
        .collect();
    err |= !compare_ty(db, impl_m, trait_m, &map_to_impl, trait_inst, sink);
    if err {
        return;
    }

    compare_constraints(db, impl_m, trait_m, &map_to_impl, sink);
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
    map_to_impl: &[TyId<'db>],
    trait_inst: TraitInstId<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    let impl_m_arg_tys = impl_m.arg_tys(db);
    let trait_m_arg_tys = trait_m.arg_tys(db);

    let mut substituter = AssocTySubst::new(trait_inst);
    let assumptions = collect_func_def_constraints(db, impl_m, true).instantiate_identity();

    for (idx, (trait_m_ty, impl_m_ty)) in trait_m_arg_tys
        .iter()
        .zip(impl_m_arg_tys.iter())
        .enumerate()
    {
        // 1) Instantiate trait method's type params into the impl's generics
        let trait_m_ty = trait_m_ty.instantiate(db, map_to_impl);
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
        let trait_m_ty_normalized = normalize_const_tys(db, trait_m_ty_normalized, trait_inst);
        let impl_m_ty_normalized = normalize_const_tys(db, impl_m_ty_normalized, trait_inst);

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
    let trait_m_ret_ty = trait_m.ret_ty(db).instantiate(db, map_to_impl);

    // Substitute and normalize the return type as well.
    let trait_m_ret_ty_substituted = trait_m_ret_ty.fold_with(db, &mut substituter);
    let trait_m_ret_ty_normalized =
        normalize_ty(db, trait_m_ret_ty_substituted, impl_m.scope(), assumptions);
    let impl_m_ret_ty_normalized = normalize_ty(db, impl_m_ret_ty, impl_m.scope(), assumptions);
    let trait_m_ret_ty_normalized = normalize_const_tys(db, trait_m_ret_ty_normalized, trait_inst);
    let impl_m_ret_ty_normalized = normalize_const_tys(db, impl_m_ret_ty_normalized, trait_inst);

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

fn normalize_const_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    trait_inst: TraitInstId<'db>,
) -> TyId<'db> {
    struct ConstFolder<'db> {
        trait_inst: TraitInstId<'db>,
    }

    impl<'db> TyFolder<'db> for ConstFolder<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return ty.super_fold_with(db, self);
            };

            let super::const_ty::ConstTyData::UnEvaluated {
                body,
                ty: expected_ty,
                ..
            } = const_ty.data(db)
            else {
                return ty.super_fold_with(db, self);
            };
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
                && let Some(repl) = const_ty_from_trait_const(db, self.trait_inst, name)
            {
                const_ty = repl;
            }

            TyId::new(
                db,
                TyData::ConstTy(const_ty.evaluate(db, Some(expected_ty))),
            )
        }
    }

    let mut folder = ConstFolder { trait_inst };
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
    map_to_impl: &[TyId<'db>],
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_constraints = collect_func_def_constraints(db, impl_m, false).instantiate_identity();
    let trait_m_constraints =
        collect_func_def_constraints(db, trait_m, false).instantiate(db, map_to_impl);
    let mut unsatisfied_goals = ThinVec::new();
    for &goal in impl_m_constraints.list(db) {
        let canonical_goal = Canonical::new(db, goal);
        let ingot = trait_m.ingot(db);
        match is_goal_satisfiable(db, ingot, canonical_goal, trait_m_constraints) {
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
