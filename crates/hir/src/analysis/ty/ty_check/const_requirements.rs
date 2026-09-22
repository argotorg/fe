//! Const function requirements are discharged after inference, for every body
//! owner. Concrete discharge uses ordinary CTFE. Symbolic forwarding compares
//! resolved, typed expressions after scoped substitution, without evaluating
//! unknown parameters or assuming the obligation being checked.
use super::*;
use crate::analysis::ty::binder::Binder;
use crate::hir_def::{UnOp, scope_graph::ScopeId};

#[derive(Debug, Clone, PartialEq, Eq)]
struct PredicateKey<'db> {
    ty: TyId<'db>,
    arithmetic: crate::hir_def::attr::ArithmeticMode,
    operation: Option<Callable<'db>>,
    term: PredicateTerm<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PredicateTerm<'db> {
    Literal(LitKind<'db>),
    TypeConst(TyId<'db>),
    Const(Const<'db>),
    TraitConst(TraitInstId<'db>, crate::hir_def::IdentId<'db>),
    InherentConst(
        crate::hir_def::Impl<'db>,
        TyId<'db>,
        crate::hir_def::IdentId<'db>,
    ),
    Binary(BinOp, Box<PredicateKey<'db>>, Box<PredicateKey<'db>>),
    Unary(UnOp, Box<PredicateKey<'db>>),
    Cast(Box<PredicateKey<'db>>),
    Call(Vec<PredicateKey<'db>>),
}

fn predicate_key<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed: &TypedBody<'db>,
    expr: ExprId,
    scope: ScopeId<'db>,
    args: &[TyId<'db>],
) -> Option<PredicateKey<'db>> {
    let subst = |ty| Binder::bind(ty).instantiate_scoped(db, scope, args);
    let child = |expr| predicate_key(db, body, typed, expr, scope, args);
    let term = match expr.data(db, body).borrowed().to_opt()? {
        Expr::Lit(lit) => PredicateTerm::Literal(*lit),
        Expr::Path(_) => {
            if let Some(ValuePathRef::TypeConst(ty)) = typed.value_path_ref(expr) {
                PredicateTerm::TypeConst(subst(ty))
            } else {
                // A reference's lookup scope is provenance, not its identity.
                // Keep the resolved declaration and substituted receiver/goal.
                match typed.expr_const_ref(expr)? {
                    ConstRef::Const(constant) => PredicateTerm::Const(constant),
                    ConstRef::TraitConst(reference) => PredicateTerm::TraitConst(
                        Binder::bind(reference.inst()).instantiate_scoped(db, scope, args),
                        reference.name(),
                    ),
                    ConstRef::InherentConst(reference) => PredicateTerm::InherentConst(
                        reference.impl_(),
                        subst(reference.receiver_ty()),
                        reference.name(),
                    ),
                }
            }
        }
        Expr::Bin(lhs, rhs, op) => {
            PredicateTerm::Binary(*op, Box::new(child(*lhs)?), Box::new(child(*rhs)?))
        }
        Expr::Un(value, op) => PredicateTerm::Unary(*op, Box::new(child(*value)?)),
        Expr::Cast(value, _) => PredicateTerm::Cast(Box::new(child(*value)?)),
        Expr::Call(_, call_args) => {
            typed.callable_expr(expr)?;
            PredicateTerm::Call(
                call_args
                    .iter()
                    .map(|arg| child(arg.expr))
                    .collect::<Option<_>>()?,
            )
        }
        _ => return None,
    };
    Some(PredicateKey {
        ty: subst(typed.expr_ty(db, expr)),
        arithmetic: BodyOwner::AnonConstBody {
            body,
            expected: TyId::bool(db),
        }
        .arithmetic_mode(db),
        operation: typed
            .callable_expr(expr)
            .cloned()
            .map(|callable| Binder::bind(callable).instantiate_scoped(db, scope, args)),
        term,
    })
}

fn predicate_flags<'db>(db: &'db dyn HirAnalysisDb, mut typed: TypedBody<'db>) -> TyFlags {
    // Ambient trait assumptions are not dependencies of the expression itself.
    typed.assumptions = PredicateListId::empty_list(db);
    collect_flags(db, typed)
}

#[salsa::tracked(return_ref, cycle_initial=formation_cycle_initial, cycle_fn=formation_cycle_recover)]
pub(super) fn check_predicate_formation<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let owner = BodyOwner::AnonConstBody {
        body,
        expected: TyId::bool(db),
    };
    let (mut diags, typed) = infer_body(db, owner).clone();
    if diags.is_empty() || static_assert_ignorable_type_diags(db, &diags) {
        diags.extend(
            crate::analysis::ty::const_check::check_const_body_expressions(db, body, &typed),
        );
        diags.extend(check_body_requirements(db, owner, &typed));
    }
    if has_recursive_requirement(&diags) {
        diags = vec![BodyDiag::RecursiveConstRequirement(body.span().into()).into()];
    }
    (diags, typed)
}

pub(super) fn predicate_may_depend_on_params<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> bool {
    let typed = &infer_body(
        db,
        BodyOwner::AnonConstBody {
            body,
            expected: TyId::bool(db),
        },
    )
    .1;
    predicate_flags(db, typed.clone()).contains(TyFlags::HAS_PARAM)
}

pub(super) fn check_body_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    typed: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let Some(body) = typed.body() else {
        return Vec::new();
    };
    // A predicate body cannot use its enclosing function's predicates as
    // premises: doing so could let the requirement establish itself.
    let caller = match owner {
        BodyOwner::Func(func) if !func.is_associated_func(db) => Some(func),
        _ => None,
    };
    let direct_callees: FxHashSet<_> = body
        .exprs(db)
        .values()
        .filter_map(|expr| match expr.borrowed().to_opt()? {
            Expr::Call(callee, _) => Some(*callee),
            _ => None,
        })
        .collect();
    let mut diags = Vec::new();
    for (expr, _) in body.exprs(db).iter() {
        if direct_callees.contains(&expr) && typed.callable_expr(expr).is_none() {
            continue;
        }
        let (definition, args) = if let Some(callable) = typed.callable_expr(expr) {
            (callable.callable_def(), callable.generic_args())
        } else {
            let ty = typed.expr_ty(db, expr);
            let (base, args) = ty.decompose_ty_app(db);
            let TyData::TyBase(TyBase::Func(definition)) = base.data(db) else {
                continue;
            };
            (*definition, args)
        };
        let CallableDef::Func(func) = definition else {
            continue;
        };
        // Ground clauses are already mandatory declaration checks. Unsupported
        // associated/generic owner contexts are rejected at their declarations.
        let predicates = WhereClauseOwner::Func(func)
            .where_clause(db)
            .const_predicates(db);
        if predicates.is_empty()
            || func.is_associated_func(db)
            || collect_generic_params(db, func.into())
                .params(db)
                .is_empty()
        {
            continue;
        }
        let caller = caller.filter(|_| args.iter().any(|ty| ty.has_param(db)));
        for &predicate in predicates {
            let failures = discharge_requirement(db, func, predicate, args.to_vec(), caller);
            if !failures.is_empty() {
                diags.push(
                    BodyDiag::ConstRequirementNotSatisfied {
                        primary: expr.span(body).into(),
                        predicate: predicate.span().into(),
                        reason: requirement_reason(failures),
                    }
                    .into(),
                );
                // The diagnostic above already names the failure. The raw
                // failures point at the callee's predicate, which is not at
                // fault, so only a recursion marker is kept: predicate
                // formation needs it to absorb a cycle.
                diags.extend(
                    failures
                        .iter()
                        .filter(|diag| {
                            matches!(
                                diag,
                                FuncBodyDiag::Body(BodyDiag::RecursiveConstRequirement(_))
                            )
                        })
                        .cloned(),
                );
            }
        }
    }
    diags
}

fn requirement_reason(diags: &[FuncBodyDiag<'_>]) -> String {
    for diag in diags {
        match diag {
            FuncBodyDiag::Body(BodyDiag::RecursiveConstRequirement(_)) => {
                return "recursive const requirement cannot establish itself".into();
            }
            FuncBodyDiag::Body(BodyDiag::WhereConstPredicateFailed(_)) => {
                return "condition evaluated to `false`".into();
            }
            FuncBodyDiag::Body(BodyDiag::ConstRequirementNotSatisfied { reason, .. }) => {
                return reason.clone();
            }
            FuncBodyDiag::Ty(TyDiagCollection::Ty(diag)) => {
                let reason = match diag {
                    TyLowerDiag::ConstEvalDivisionByZero(_) => {
                        "constant evaluation encountered division by zero"
                    }
                    TyLowerDiag::ConstEvalArithmeticOverflow(_) => "constant evaluation overflowed",
                    TyLowerDiag::ConstEvalStepLimitExceeded(_) => {
                        "constant evaluation exceeded its step limit"
                    }
                    TyLowerDiag::ConstEvalRecursionLimitExceeded(_) => {
                        "constant evaluation exceeded its recursion limit"
                    }
                    TyLowerDiag::ConstEvalRecursiveConst(_) => "recursive constant evaluation",
                    _ => continue,
                };
                return reason.into();
            }
            _ => {}
        }
    }
    "condition could not be established; the predicate must be a well-formed, evaluable bool".into()
}

#[salsa::tracked(return_ref, cycle_initial=requirement_cycle_initial, cycle_fn=requirement_cycle_recover)]
fn discharge_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    predicate: Body<'db>,
    args: Vec<TyId<'db>>,
    caller: Option<Func<'db>>,
) -> Vec<FuncBodyDiag<'db>> {
    let expected = TyId::bool(db);
    let (diags, typed) = check_predicate_formation(db, predicate);
    if has_recursive_requirement(diags) {
        return vec![BodyDiag::RecursiveConstRequirement(predicate.span().into()).into()];
    }
    if !diags.is_empty() && !static_assert_ignorable_type_diags(db, diags) {
        return diags.clone();
    }
    let mut instantiated = Binder::bind(typed.clone()).instantiate_scoped(db, func.scope(), &args);
    // TypedBody deliberately preserves formal TypeConst paths for runtime ABI
    // selection. Substitute these references only in this dependency view.
    for reference in instantiated.value_path_refs.values_mut().flatten() {
        if let ValuePathRef::TypeConst(ty) = reference {
            *ty = Binder::bind(*ty).instantiate_scoped(db, func.scope(), &args);
        }
    }
    let symbolic = predicate_flags(db, instantiated).contains(TyFlags::HAS_PARAM);
    if symbolic {
        let key = predicate_key(
            db,
            predicate,
            typed,
            predicate.expr(db),
            func.scope(),
            &args,
        );
        if let (Some(key), Some(caller)) = (&key, caller) {
            let caller_args = collect_generic_params(db, caller.into()).params(db);
            for &premise in WhereClauseOwner::Func(caller)
                .where_clause(db)
                .const_predicates(db)
            {
                let (diags, typed) = check_predicate_formation(db, premise);
                if (!diags.is_empty() && !static_assert_ignorable_type_diags(db, diags))
                    || predicate_key(
                        db,
                        premise,
                        typed,
                        premise.expr(db),
                        caller.scope(),
                        caller_args,
                    )
                    .as_ref()
                        != Some(key)
                {
                    continue;
                }
                return Vec::new();
            }
        }
        // An unused type parameter does not make a ground predicate unknown.
        if predicate_may_depend_on_params(db, predicate) {
            return vec![
                BodyDiag::ConstRequirementNotSatisfied {
                    primary: predicate.span().into(),
                    predicate: predicate.span().into(),
                    reason: if key.is_some() {
                        "no matching const requirement in the caller after substitution".into()
                    } else {
                        "symbolic forwarding of this expression is not supported; \
                         concrete evaluation is required"
                            .into()
                    },
                }
                .into(),
            ];
        }
    }
    let owner = BodyOwner::AnonConstBody {
        body: predicate,
        expected,
    };
    let outcome = eval_body_owner_const(db, owner, args);
    const_predicate_outcome_diags(db, predicate, owner, outcome)
}

fn requirement_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _func: Func<'db>,
    predicate: Body<'db>,
    _args: Vec<TyId<'db>>,
    _caller: Option<Func<'db>>,
) -> Vec<FuncBodyDiag<'db>> {
    vec![BodyDiag::RecursiveConstRequirement(predicate.span().into()).into()]
}

fn requirement_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &[FuncBodyDiag<'db>],
    _count: u32,
    _func: Func<'db>,
    _predicate: Body<'db>,
    _args: Vec<TyId<'db>>,
    _caller: Option<Func<'db>>,
) -> salsa::CycleRecoveryAction<Vec<FuncBodyDiag<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

// A recursive failure is absorbing. Canonicalize it to this query's own
// predicate instead of growing a diagnostic stack on each fixpoint iteration.
fn has_recursive_requirement(diags: &[FuncBodyDiag<'_>]) -> bool {
    diags.iter().any(|diag| {
        matches!(
            diag,
            FuncBodyDiag::Body(BodyDiag::RecursiveConstRequirement(_))
        )
    })
}

fn formation_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let typed = infer_body(
        db,
        BodyOwner::AnonConstBody {
            body,
            expected: TyId::bool(db),
        },
    )
    .1
    .clone();
    (
        vec![BodyDiag::RecursiveConstRequirement(body.span().into()).into()],
        typed,
    )
}
fn formation_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &(Vec<FuncBodyDiag<'db>>, TypedBody<'db>),
    _count: u32,
    _body: Body<'db>,
) -> salsa::CycleRecoveryAction<(Vec<FuncBodyDiag<'db>>, TypedBody<'db>)> {
    salsa::CycleRecoveryAction::Iterate
}
