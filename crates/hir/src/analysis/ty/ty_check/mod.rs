mod callable;
mod callable_body;
mod contract;
mod effect_env;
pub(crate) mod env;
mod expr;
mod owner;
mod pat;
mod path;
mod stmt;

pub(crate) use self::contract::eval_msg_variant_selector;
pub use self::contract::{
    ResolvedRecvVariant, VariantResError, check_contract_field_address_spaces,
    check_contract_immutable_fields_initialized, check_contract_init_body,
    check_contract_recv_arm_body, check_contract_recv_block, check_contract_recv_blocks,
    resolve_variant_bare, resolve_variant_in_msg,
};
pub use self::path::RecordLike;
pub use super::ty_def::ClosureCaptureAccess;
use crate::analysis::name_resolution::ResolvedVariant;
pub use crate::analysis::ty::ProviderAddressSpace;
use crate::analysis::ty::corelib::resolve_lib_type_path;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::method_table::ProbedMethod;
use crate::analysis::ty::provider::{ProviderKind, ProviderLayoutEvidence, provider_semantics};
use crate::analysis::ty::trait_lower::lower_impl_trait;
use crate::analysis::ty::trait_resolution::constraint::{
    PredicateSource, collect_func_decl_constraint_pairs,
};
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor};
use crate::hir_def::{CallableDef, ImplTrait, Trait};
use crate::{
    hir_def::{
        BinOp, Body, ClosureDef, CondId, Const, Contract, ContractRecvArm, Expr, ExprId, Func,
        GenericParamOwner, LitKind, ManualContractRootAttr, Partial, Pat, PatId, PathId,
        StaticAssert, StaticAssertComparison, Stmt, StmtId, StringId, TypeId as HirTyId,
        WhereClauseOwner,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
use callable::{CallGenericArgUnifyError, unify_explicit_call_generic_args};
pub use callable::{Callable, EffectProviderProvenance, EffectProviderSpecialization};
pub use callable_body::{
    TypedCallableBody, TypedClosureBody, TypedClosureCapture, TypedClosureParam,
};
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption};
use ena::unify::InPlace;
use env::TyCheckEnv;
pub use env::{
    CLOSURE_ARGS_PARAM_IDX, ClosureCapture, ClosureCaptureConstruction, ClosureInfo,
    EffectParamSite, ExprProp, LocalBinding, ParamSite, PatBindingMode, PathReadSemantics,
    ValueAccess,
};
pub(super) use expr::TraitOps;
use num_traits::ToPrimitive;
pub use owner::{BodyOwner, ClosureReceiverMode, EffectParamOwner};
pub use stmt::ForLoopSeq;

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::analysis::place::{Place, PlaceBase, PlaceProjection};

use super::{
    LayoutBundlePath, LayoutBundlePathStep,
    assoc_const::{AssocConstUse, InherentConstUse},
    canonical::{Canonical, Canonicalized},
    diagnostics::{
        BodyDiag, CallConstraintDiagInfo, FuncBodyDiag, StaticAssertComparisonValues,
        TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{EffectKeyKind, ResolvedEffectKey, resolve_effect_key},
    layout_holes::merge_equated_layout_holes,
    trait_def::{TraitInstId, resolve_trait_method_instance},
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
        constraint::collect_func_decl_constraints, goal_query_has_no_distinct_solution,
        is_goal_query_satisfiable, is_goal_satisfiable,
    },
    ty_contains_const_hole,
    ty_def::{
        BorrowKind, CapabilityKind, InvalidCause, Kind, MAX_INLINE_STRING_BYTES, StringFallback,
        TyId, TyVarSort,
    },
    ty_lower::{
        CallableInputLayoutBackingSource, callable_input_layout_backing_index_lengths,
        callable_input_layout_backing_sources, layout_param_projection_paths_in_ty, lower_hir_ty,
        resolve_callable_input_effect_key,
    },
    unify::{InferenceKey, Snapshot, UnificationError, UnificationTable},
};
use crate::analysis::semantic::SemanticCodeRegionRef;
use crate::analysis::semantic::{SemConstId, SemConstScalar, SemConstValue, eval_body_owner_const};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::{
    const_ty::{
        BodyHoleSite, CallableInputLayoutHoleOrigin, ConstTyData, HoleAnchor, HoleMinter,
        invalid_cause_from_ctfe_error,
    },
    fold::AssocTySubst,
    normalize::normalize_ty,
    pattern_ir::{
        ConstructorKind, PatternAnalysisStatus, PatternStore, ValidatedPatId, ValidatedPatKind,
    },
    pattern_types::{
        PatternDestructureMode, apply_pattern_borrow_mode, destructure_pattern_source,
    },
    ty_error::collect_ty_lower_errors,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        PathRes, PathResError, diagnostics::PathResDiag, resolve_path_with_observer_and_minter,
    },
    ty::{
        ty_def::{TyFlags, inference_keys},
        visitor::collect_flags,
    },
};

#[salsa::tracked(return_ref)]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Func(func))
}

#[salsa::tracked(
    return_ref,
    cycle_initial=check_const_body_cycle_initial,
    cycle_fn=check_const_body_cycle_recover
)]
pub fn check_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Const(const_))
}

fn check_const_body_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _const_: Const<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    (Vec::new(), TypedBody::empty(db))
}

fn check_const_body_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    value: &(Vec<FuncBodyDiag<'db>>, TypedBody<'db>),
    _count: u32,
    _const_: Const<'db>,
) -> salsa::CycleRecoveryAction<(Vec<FuncBodyDiag<'db>>, TypedBody<'db>)> {
    // Value evaluation owns recursive-constant diagnostics. A provisional
    // empty body only breaks its dependency cycle with body type checking.
    salsa::CycleRecoveryAction::Fallback(value.clone())
}

#[salsa::tracked(
    return_ref,
    cycle_initial=check_anon_const_body_cycle_initial,
    cycle_fn=check_anon_const_body_cycle_recover
)]
pub fn check_anon_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::AnonConstBody { body, expected })
}

fn check_anon_const_body_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _body: Body<'db>,
    _expected: TyId<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    (Vec::new(), TypedBody::empty(db))
}

fn check_anon_const_body_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    value: &(Vec<FuncBodyDiag<'db>>, TypedBody<'db>),
    _count: u32,
    _body: Body<'db>,
    _expected: TyId<'db>,
) -> salsa::CycleRecoveryAction<(Vec<FuncBodyDiag<'db>>, TypedBody<'db>)> {
    // Const bodies can participate in signature/trait-environment cycles.
    // Keep the provisional body result as the fixpoint candidate instead of
    // repeatedly expanding a specialization-dependent cycle.
    salsa::CycleRecoveryAction::Fallback(value.clone())
}

/// Type-checks the value bodies of an `impl Trait` block's associated consts.
/// Bodies are checked against the trait's declared type for the const
/// (instantiated with the impl's trait args) so a wrong impl-side header
/// doesn't cascade; consts not declared in the trait fall back to their own
/// header type. Header-vs-trait conformance itself is reported by the
/// `ImplTrait` item diagnostics.
#[salsa::tracked(return_ref)]
pub fn check_impl_trait_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    // Only check impls the user wrote; attribute-expanded impls (e.g.
    // `#[event]`) would re-report their cascade failures at the expansion
    // site on already-diagnosed code.
    if !matches!(impl_trait.origin(db), crate::span::HirOrigin::Raw(_)) {
        return Vec::new();
    }
    let Some(implementor) = lower_impl_trait(db, impl_trait) else {
        return Vec::new();
    };
    let implementor = implementor.instantiate_identity();
    let trait_hir = implementor.trait_def(db);
    let trait_args = implementor.trait_(db).args(db);

    let mut diags = Vec::new();
    for impl_const in impl_trait.assoc_consts(db) {
        let Some(body) = impl_const.value_body(db) else {
            continue;
        };
        let expected_ty = impl_const
            .name(db)
            .and_then(|name| trait_hir.const_(db, name))
            .and_then(|c| c.ty_binder(db))
            .map(|binder| binder.instantiate(db, trait_args))
            .or_else(|| impl_const.ty(db));
        let Some(expected_ty) = expected_ty else {
            continue;
        };
        if expected_ty.has_invalid(db) {
            continue;
        }
        diags.extend(
            check_anon_const_body(db, body, expected_ty)
                .0
                .iter()
                .cloned(),
        );
    }
    diags
}

/// Type-checks the default value bodies of a trait's associated consts
/// against their declared types. Nothing else checks these: impls that don't
/// override a default only force its evaluation transitively, and a trait
/// with no concrete impl never checks it at all.
#[salsa::tracked(return_ref)]
pub fn check_trait_const_default_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    if !matches!(trait_.origin(db), crate::span::HirOrigin::Raw(_)) {
        return Vec::new();
    }

    let mut diags = Vec::new();
    for trait_const in trait_.assoc_consts(db) {
        let Some(body) = trait_const.default_body(db) else {
            continue;
        };
        let Some(expected_ty) = trait_const.ty(db) else {
            continue;
        };
        let flags = expected_ty.flags(db);
        if flags.contains(TyFlags::HAS_INVALID) {
            continue;
        }
        let body_diags = &check_anon_const_body(db, body, expected_ty).0;
        if flags.contains(TyFlags::HAS_PARAM) {
            // A declared type mentioning `Self` or generic params (`const C:
            // T = 1`) types the default per instantiation: impl overrides are
            // checked with the impl's bindings, and the value is retyped at
            // use sites. Mismatches against the rigid param are expected
            // here; instantiation-independent errors (unresolved names, bad
            // calls, ...) still must surface.
            diags.extend(
                body_diags
                    .iter()
                    .filter(|diag| !diag_depends_on_param_instantiation(db, diag))
                    .cloned(),
            );
        } else {
            diags.extend(body_diags.iter().cloned());
        }
    }
    diags
}

/// Whether a default-body diagnostic could be an artifact of checking
/// against a rigid generic param instead of a per-instantiation type.
fn diag_depends_on_param_instantiation<'db>(
    db: &'db dyn HirAnalysisDb,
    diag: &FuncBodyDiag<'db>,
) -> bool {
    match diag {
        FuncBodyDiag::Body(BodyDiag::TypeMismatch {
            expected, given, ..
        }) => expected.has_param(db) || given.has_param(db),
        FuncBodyDiag::Body(BodyDiag::TypeAnnotationNeeded { .. }) => true,
        _ => false,
    }
}

#[salsa::tracked(return_ref)]
pub fn check_static_assert<'db>(
    db: &'db dyn HirAnalysisDb,
    assert_: StaticAssert<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let condition = assert_.condition(db);
    let expected = TyId::bool(db);
    let owner = BodyOwner::AnonConstBody {
        body: condition,
        expected,
    };
    let (body_diags, typed_body) = check_anon_const_body(db, condition, expected);
    let ignorable_body_diags = static_assert_ignorable_type_diags(db, body_diags);
    if !body_diags.is_empty() && !ignorable_body_diags {
        return body_diags.clone();
    }

    match eval_body_owner_const(db, owner, Vec::new()) {
        Ok(value) => match static_assert_bool_value(db, value) {
            Some(true) => {}
            Some(false) => {
                let mut diags = body_diags.clone();
                let comparison = if body_diags.is_empty() {
                    assert_.comparison(db).and_then(|comparison| {
                        static_assert_comparison_values(db, condition, typed_body, comparison)
                    })
                } else {
                    None
                };
                diags.push(
                    BodyDiag::StaticAssertFailed {
                        primary: condition.span().into(),
                        comparison,
                    }
                    .into(),
                );
                return diags;
            }
            None => {
                return vec![BodyDiag::ConstValueMustBeKnown(condition.span().into()).into()];
            }
        },
        Err(crate::analysis::semantic::CtfeError::NotConstEvaluable { .. }) => {
            return vec![BodyDiag::ConstValueMustBeKnown(condition.span().into()).into()];
        }
        Err(err) => {
            let ty = TyId::invalid(db, invalid_cause_from_ctfe_error(db, owner, err));
            if let Some(diag) = ty.emit_diag(db, condition.span().into()) {
                return vec![diag.into()];
            }
        }
    }

    Vec::new()
}

fn static_assert_bool_value<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Option<bool> {
    let SemConstValue::Scalar {
        value: SemConstScalar::Bool(value),
        ..
    } = value.value(db)
    else {
        return None;
    };
    Some(value)
}

fn static_assert_ignorable_type_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    diags: &[FuncBodyDiag<'db>],
) -> bool {
    !diags.is_empty()
        && diags.iter().all(|diag| {
            matches!(
                diag,
                FuncBodyDiag::Body(BodyDiag::TypeAnnotationNeeded { ty, .. })
                    if ty.is_integral_var(db)
            )
        })
}

fn static_assert_comparison_values<'db>(
    db: &'db dyn HirAnalysisDb,
    condition: Body<'db>,
    typed_body: &TypedBody<'db>,
    comparison: StaticAssertComparison<'db>,
) -> Option<StaticAssertComparisonValues> {
    let Partial::Present(Expr::Bin(lhs_expr, rhs_expr, BinOp::Comp(op))) =
        condition.expr(db).data(db, condition)
    else {
        return None;
    };
    if *op != comparison.op {
        return None;
    }

    let lhs = eval_static_assert_comparison_operand(
        db,
        comparison.lhs,
        typed_body.expr_ty(db, *lhs_expr),
    )?;
    let rhs = eval_static_assert_comparison_operand(
        db,
        comparison.rhs,
        typed_body.expr_ty(db, *rhs_expr),
    )?;

    Some(StaticAssertComparisonValues {
        op: comparison.op,
        lhs: lhs.pretty_print(db),
        rhs: rhs.pretty_print(db),
    })
}

fn eval_static_assert_comparison_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> Option<SemConstId<'db>> {
    if expected.has_invalid(db) {
        return None;
    }
    let owner = BodyOwner::AnonConstBody { body, expected };
    let body_diags = &check_anon_const_body(db, body, expected).0;
    if !body_diags.is_empty() && !static_assert_ignorable_type_diags(db, body_diags) {
        return None;
    }
    eval_body_owner_const(db, owner, Vec::new()).ok()
}

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (
            Vec::new(),
            match owner {
                BodyOwner::Func(func) => typed_body_for_bodyless_func(db, func),
                BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. }
                | BodyOwner::Closure { .. } => TypedBody::empty(db),
            },
        );
    };

    checker.run();
    let (mut diags, typed_body) = checker.finish();
    if let BodyOwner::Func(func) = owner
        && func.is_const(db)
        && !func.is_extern(db)
    {
        diags.extend(crate::analysis::ty::const_check::check_const_fn_body(
            db,
            func,
            &typed_body,
        ));
    }

    (diags, typed_body)
}

/// Forces evaluation of a const item's value and reports failures
/// (unknowable values, recursion, arithmetic errors). This is deliberately a
/// separate query from `check_const_body`: forcing evaluates *other* consts
/// the body references, and their `ensure_const_evaluable` checks call
/// `check_const_body` back — if typing and forcing were one query, a
/// recursive const definition would be an unrecoverable salsa cycle through
/// body checking instead of converging to a `RecursiveConst` error in the
/// eval queries.
#[salsa::tracked(return_ref)]
pub fn check_const_value<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    if !check_const_body(db, const_).0.is_empty() {
        return Vec::new();
    }
    let Some(body) = const_.body(db).to_opt() else {
        return Vec::new();
    };
    if const_.ty(db).has_invalid(db) {
        return Vec::new();
    }

    const_body_ctfe_diags(db, body, const_.ty(db), false)
}

/// CTFE-validates an anonymous const body (a top-level `const` initializer or
/// an inherent associated-const value). Mirrors the validation top-level
/// consts get, so a body that type-checks but is not const-evaluable (e.g.
/// `const C: u256 = ordinary_fn()`) is rejected here rather than only when
/// referenced.
///
/// `allow_type_level` must be true for consts on generic impls: their value
/// (e.g. `256 / BITS`) is legitimately parametric and is fully evaluated per
/// instantiation, so a `TypeLevel` result is expected, not an error. A
/// genuinely non-const body still yields `NotConstEvaluable` and is flagged
/// regardless.
pub(crate) fn const_body_ctfe_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
    allow_type_level: bool,
) -> Vec<FuncBodyDiag<'db>> {
    let owner = BodyOwner::AnonConstBody { body, expected };
    let mut diags = Vec::new();
    match eval_body_owner_const(db, owner, Vec::new()) {
        Ok(value) => {
            if !allow_type_level && matches!(value.value(db), SemConstValue::TypeLevel { .. }) {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
            }
        }
        Err(crate::analysis::semantic::CtfeError::NotConstEvaluable { .. }) => {
            diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
        }
        Err(err) => {
            let ty = TyId::invalid(db, invalid_cause_from_ctfe_error(db, owner, err));
            if let Some(diag) = ty.emit_diag(db, body.span().into()) {
                diags.push(diag.into());
            }
        }
    }
    diags
}

fn typed_body_for_bodyless_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> TypedBody<'db> {
    let assumptions = crate::semantic::func_body_assumptions(db, func).extend_all_bounds(db);
    let mut result_ty = func.return_ty(db);
    if !result_ty.is_star_kind(db) || ty_contains_const_hole(db, result_ty) {
        result_ty = TyId::invalid(db, InvalidCause::Other);
    }
    let param_bindings = func
        .params(db)
        .enumerate()
        .map(|(idx, view)| {
            let mut ty = *func
                .arg_tys(db)
                .get(idx)
                .map(|binder| binder.skip_binder())
                .unwrap_or(&TyId::invalid(db, InvalidCause::ParseError));
            if !ty.is_star_kind(db) || (!view.is_self_param(db) && ty_contains_const_hole(db, ty)) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                mode: view.mode(db),
                ty,
                is_mut: view.is_mut(db),
            }
        })
        .collect();
    TypedBody {
        body: None,
        result_ty,
        assumptions,
        pat_ty: SecondaryMap::new(),
        expr_ty: SecondaryMap::new(),
        expr_normal_completion: SecondaryMap::new(),
        expr_normal_bool_value: SecondaryMap::new(),
        cond_normal_bool_value: SecondaryMap::new(),
        assignment_rebinds_capability: SecondaryMap::new(),
        contextual_view_sources: SecondaryMap::new(),
        const_refs: SecondaryMap::new(),
        value_path_refs: SecondaryMap::new(),
        semantic_expr_lowering: SecondaryMap::new(),
        record_init_lowering: SecondaryMap::new(),
        closure_infos: SecondaryMap::new(),
        resolved_field_index: SecondaryMap::new(),
        call_effect_args: SecondaryMap::new(),
        return_borrow_provider: None,
        param_bindings,
        pat_bindings: SecondaryMap::new(),
        pat_binding_modes: SecondaryMap::new(),
        pattern_store: PatternStore::default(),
        pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
        for_loop_seq: SecondaryMap::new(),
        expr_place: SecondaryMap::new(),
        expr_places: PrimaryMap::new(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingInterfaceShape<'db> {
    OrdinaryValue,
    ProviderValue {
        kind: ProviderKind,
        value_ty: TyId<'db>,
    },
    PlaceCarrier {
        value_ty: TyId<'db>,
    },
    DirectCarrier {
        target_ty: TyId<'db>,
    },
}

#[derive(Clone)]
pub(super) enum PatternLayoutContext<'db> {
    ContractField {
        field: &'db crate::semantic::FieldStorageLayout<'db>,
        view: crate::semantic::LayoutViewKind,
        base_projections: Vec<crate::semantic::LayoutProjection>,
    },
    CallableInput {
        func: Func<'db>,
        origin: crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin,
        base_path: LayoutBundlePath,
    },
}

pub struct TyChecker<'db> {
    pub(crate) db: &'db dyn HirAnalysisDb,
    pub(crate) env: TyCheckEnv<'db>,
    pub(crate) table: UnificationTable<'db>,
    expected: TyId<'db>,
    closure_expectations: FxHashMap<ExprId, ClosureExpectation<'db>>,
    closure_type_expectations: Vec<(TyId<'db>, ClosureExpectation<'db>)>,
    active_closure_replay_defs: FxHashSet<ClosureDef<'db>>,
    pending_place_checks: Vec<PendingPlaceCheck>,
    pending_array_repeat_copy_checks: Vec<ExprId>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    diags: Vec<FuncBodyDiag<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ClosureExpectation<'db> {
    pub params: Vec<TyId<'db>>,
    pub ret_ty: TyId<'db>,
}

#[derive(Debug, Clone, Copy)]
enum PendingPlaceCheck {
    Assign {
        assignment: Option<ExprId>,
        lhs: ExprId,
        captured: bool,
        rebinds_capability: bool,
    },
    Borrow {
        expr: ExprId,
        source: ExprId,
        kind: BorrowKind,
        captured: bool,
    },
}

pub(crate) struct TyCheckerSnapshot<'db> {
    table: Snapshot<InPlace<InferenceKey<'db>>>,
    deferred_len: usize,
    pending_place_check_len: usize,
    pending_array_repeat_copy_check_len: usize,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    expected: TyId<'db>,
    closure_expectations: FxHashMap<ExprId, ClosureExpectation<'db>>,
    closure_type_expectations: Vec<(TyId<'db>, ClosureExpectation<'db>)>,
    active_closure_replay_defs: FxHashSet<ClosureDef<'db>>,
}

pub(super) struct ClosureReplayTransaction<'db> {
    checker: TyCheckerSnapshot<'db>,
    env: env::ClosureReplayEnvSnapshot<'db>,
    diag_len: usize,
}

enum TraitObligationOutcome<'db> {
    Discharged,
    Progressed,
    Requeue(env::TraitObligation<'db>),
}

impl<'db> TyChecker<'db> {
    fn string_literal_fallback(&self) -> StringFallback {
        StringFallback::Fixed
    }

    fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let env = TyCheckEnv::new(db, owner)?;
        let expected = env.compute_expected_return();
        let mut checker = Self::new_internal(db, env, expected);
        checker.seed_effect_witnesses();
        Ok(checker)
    }

    fn run(&mut self) {
        self.check_effect_param_keys_resolve();
        self.check_own_param_types();

        if let BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } = self.env.owner()
        {
            let recv_span = self.env.owner().recv_span().unwrap();
            let arm_span = self.env.owner().recv_arm_span().unwrap();
            let arm = contract
                .recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                .expect("recv arm exists");
            let msg_path = contract
                .recvs(self.db)
                .data(self.db)
                .get(recv_idx as usize)
                .and_then(|r| r.msg_path);
            let (pat_ty, ret_ty) =
                self.resolve_recv_arm_types(contract, msg_path, arm, recv_span.path(), arm_span);
            self.expected = ret_ty;
            self.check_pat(arm.pat, pat_ty);
            self.seed_pat_bindings(arm.pat);
            self.env.flush_pending_bindings();
        }

        let root_expr = self
            .env
            .owner()
            .root_expr(self.db)
            .unwrap_or_else(|| self.env.body().expr(self.db));
        self.check_expr(root_expr, self.expected);
        self.record_owned_value_use(root_expr, self.expected);
    }

    fn check_own_param_types(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(_) => {}
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(self.db) else {
                    return;
                };
                let scope = self.env.scope();
                let assumptions = self.env.assumptions();
                for (idx, param) in init.params(self.db).data(self.db).iter().enumerate() {
                    let Some(hir_ty) = param.ty.to_opt() else {
                        continue;
                    };
                    let ty = lower_hir_ty(self.db, hir_ty, scope, assumptions);

                    if ty_contains_const_hole(self.db, ty) {
                        self.push_diag(TyDiagCollection::from(
                            TyLowerDiag::ConstHoleInValuePosition {
                                span: contract.span().init_block().params().param(idx).ty().into(),
                                ty,
                            },
                        ));
                        continue;
                    }

                    if param.mode != crate::hir_def::params::FuncParamMode::Own {
                        continue;
                    }

                    if ty.as_borrow(self.db).is_some() {
                        self.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                            primary: contract.span().init_block().params().param(idx).ty().into(),
                            ty,
                        });
                    }
                }
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractRecvArm { .. }
            | BodyOwner::Closure { .. } => {}
        }
    }

    fn check_effect_param_keys_resolve(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(func) => self.check_free_func_effect_list(func, func.effects(self.db)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            owner @ BodyOwner::ContractInit { contract } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
            owner @ BodyOwner::ContractRecvArm { contract, .. } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
        }
    }

    fn check_free_func_effect_list(
        &mut self,
        func: Func<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect
                .key_path
                .to_opt()
                .filter(|path| path.ident(self.db).is_present())
            else {
                continue;
            };

            if !matches!(
                resolve_callable_input_effect_key(
                    self.db,
                    func,
                    idx,
                    key_path,
                    self.env.assumptions(),
                ),
                ResolvedEffectKey::Type(_) | ResolvedEffectKey::Trait(_)
            ) {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner: EffectParamOwner::Func(func),
                    key: key_path,
                    idx,
                });
            }
        }
    }

    fn check_contract_scoped_effect_list(
        &mut self,
        owner: BodyOwner<'db>,
        contract: Contract<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        let (owner, site) = match owner {
            BodyOwner::Func(func) => (EffectParamOwner::Func(func), EffectParamSite::Func(func)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                unreachable!()
            }
            BodyOwner::ContractInit { contract } => (
                EffectParamOwner::ContractInit { contract },
                EffectParamSite::ContractInit { contract },
            ),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => (
                EffectParamOwner::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
                EffectParamSite::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
            ),
        };
        let view = crate::core::semantic::EffectEnvView::new(site);

        let assumptions = PredicateListId::empty_list(self.db);
        let root_effect_ty = crate::analysis::ty::registered_root_providers(
            self.db,
            EffectParamSite::Contract(contract),
        )
        .first()
        .map(|registration| registration.provider_ty);

        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect
                .key_path
                .to_opt()
                .filter(|path| path.ident(self.db).is_present())
            else {
                continue;
            };

            // Labeled effects are always type/trait keyed: `name: Type`.
            if effect.name.is_some() {
                match resolve_effect_key(self.db, key_path, contract.scope(), assumptions) {
                    ResolvedEffectKey::Trait(schema) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req = super::instantiate_trait_self(
                            self.db,
                            schema.into_trait_inst(self.db),
                            root_effect_ty,
                        );
                        if matches!(
                            is_goal_satisfiable(
                                self.db,
                                TraitSolveCx::new(self.db, contract.scope()),
                                trait_req
                            ),
                            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                        ) {
                            self.push_diag(BodyDiag::ContractRootEffectTraitNotImplemented {
                                owner,
                                idx,
                                root_ty: root_effect_ty,
                                trait_req,
                            });
                        }
                    }
                    ResolvedEffectKey::Type(schema) => {
                        let given =
                            normalize_ty(self.db, schema.carrier, contract.scope(), assumptions);
                        if !given.is_zero_sized(self.db) {
                            self.push_diag(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner,
                                key: key_path,
                                idx,
                                given,
                            });
                        }
                    }
                    ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => {
                        self.push_diag(BodyDiag::InvalidEffectKey {
                            owner,
                            key: key_path,
                            idx,
                        });
                    }
                }
                continue;
            }

            match view.resolved_binding(self.db, idx) {
                Some(binding)
                    if matches!(
                        binding.provider.source,
                        crate::core::semantic::ProviderSource::ContractField { .. }
                            | crate::core::semantic::ProviderSource::RootProvider { .. }
                    ) =>
                {
                    // Contract fields are immutable unless declared `mut`. The
                    // one exception — assigning immutable code-backed fields in
                    // `init` — is already reflected in the provider's
                    // site-specific mutability.
                    if binding.requirement.is_mut
                        && !binding.provider.is_mut
                        && let crate::core::semantic::ProviderSource::ContractField { field } =
                            binding.provider.source
                    {
                        self.push_diag(BodyDiag::ImmutableContractFieldMutBinding {
                            primary: owner.effect_param_path_span(self.db, idx),
                            field: binding.requirement.binding_name,
                            field_span: crate::hir_def::FieldParent::Contract(field.contract)
                                .field_name_span(field.index as usize),
                        });
                    }
                }
                _ => {
                    self.push_diag(BodyDiag::InvalidEffectKey {
                        owner,
                        key: key_path,
                        idx,
                    });
                }
            }
        }
    }
    fn finish(self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        TyCheckerFinalizer::new(self).finish()
    }

    pub(crate) fn snapshot_state(&mut self) -> TyCheckerSnapshot<'db> {
        TyCheckerSnapshot {
            table: self.table.snapshot(),
            deferred_len: self.env.deferred_len(),
            pending_place_check_len: self.pending_place_checks.len(),
            pending_array_repeat_copy_check_len: self.pending_array_repeat_copy_checks.len(),
            effect_provider_keys: self.effect_provider_keys.clone(),
            expected: self.expected,
            closure_expectations: self.closure_expectations.clone(),
            closure_type_expectations: self.closure_type_expectations.clone(),
            active_closure_replay_defs: self.active_closure_replay_defs.clone(),
        }
    }

    pub(super) fn snapshot_closure_replay_transaction(&mut self) -> ClosureReplayTransaction<'db> {
        ClosureReplayTransaction {
            checker: self.snapshot_state(),
            env: self.env.snapshot_closure_replay_state(),
            diag_len: self.diags.len(),
        }
    }

    pub(super) fn rollback_closure_replay_transaction(
        &mut self,
        snapshot: ClosureReplayTransaction<'db>,
    ) {
        self.env.restore_closure_replay_state(snapshot.env);
        self.diags.truncate(snapshot.diag_len);
        self.rollback_state(snapshot.checker);
    }

    pub(super) fn commit_closure_replay_transaction(
        &mut self,
        snapshot: ClosureReplayTransaction<'db>,
    ) {
        self.commit_state(snapshot.checker);
    }

    pub(crate) fn rollback_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.rollback_to(snapshot.table);
        self.env.truncate_deferred_tasks(snapshot.deferred_len);
        self.pending_place_checks
            .truncate(snapshot.pending_place_check_len);
        self.pending_array_repeat_copy_checks
            .truncate(snapshot.pending_array_repeat_copy_check_len);
        self.effect_provider_keys = snapshot.effect_provider_keys;
        self.expected = snapshot.expected;
        self.closure_expectations = snapshot.closure_expectations;
        self.closure_type_expectations = snapshot.closure_type_expectations;
        self.active_closure_replay_defs = snapshot.active_closure_replay_defs;
    }

    fn commit_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.commit(snapshot.table);
    }

    fn concrete_borrow_provider_for_effect_handle_ty(
        &self,
        ty: TyId<'db>,
    ) -> Option<ProviderAddressSpace> {
        let semantics = provider_semantics(self.db, self.env.scope(), self.env.assumptions(), ty);
        // Root-object defaults are binding-site semantics, not type-implied address spaces.
        (!matches!(semantics.kind, ProviderKind::RootObject)).then_some(semantics.address_space)?
    }

    fn concrete_borrow_provider_for_effect_field(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ProviderAddressSpace> {
        self.env
            .resolved_provider_binding(site, idx)?
            .semantics
            .address_space
    }

    fn concrete_borrow_provider_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<ProviderAddressSpace> {
        let binding_ty = self.env.lookup_binding_ty(&binding);
        match binding {
            LocalBinding::Local { pat, .. } => self.env.local_borrow_provider(pat),
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self
                .env
                .provider_binding(site, provider_idx)
                .and_then(|provider| provider.semantics.address_space),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.concrete_borrow_provider_for_effect_field(site, idx),
            _ => None,
        }
        .or_else(|| {
            matches!(
                binding,
                LocalBinding::Local { .. } | LocalBinding::Param { .. }
            )
            .then(|| {
                binding_ty
                    .as_capability(self.db)
                    .map(|_| ProviderAddressSpace::Memory)
            })?
        })
    }

    fn concrete_borrow_provider_for_place(
        &self,
        place: &Place<'db>,
    ) -> Option<ProviderAddressSpace> {
        let PlaceBase::Binding(binding) = place.base;
        let binding_ty = normalize_ty(
            self.db,
            self.env.lookup_binding_ty(&binding),
            self.env.scope(),
            self.env.assumptions(),
        );
        let place_result_ty = normalize_ty(
            self.db,
            place
                .projections
                .last()
                .map(|projection| projection.result_ty())
                .unwrap_or(binding_ty),
            self.env.scope(),
            self.env.assumptions(),
        );
        if binding_ty.as_capability(self.db).is_some() {
            return self.concrete_borrow_provider_for_binding(binding);
        }
        let binding_provider = match binding {
            LocalBinding::EffectParam { .. } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Param {
                site: ParamSite::EffectField(..),
                ..
            } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        binding_provider
            .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(place_result_ty))
            .or_else(|| {
                matches!(
                    binding,
                    LocalBinding::Local { .. } | LocalBinding::Param { .. }
                )
                .then_some(ProviderAddressSpace::Memory)
            })
    }

    fn merge_concrete_borrow_providers(
        &mut self,
        previous_span: DynLazySpan<'db>,
        previous: Option<ProviderAddressSpace>,
        current_span: DynLazySpan<'db>,
        current: Option<ProviderAddressSpace>,
    ) -> Option<ProviderAddressSpace> {
        if let (Some(previous), Some(current)) = (previous, current)
            && previous != current
        {
            self.push_diag(BodyDiag::IncompatibleBorrowProviders {
                primary: current_span,
                previous: previous_span,
                previous_provider: previous,
                current_provider: current,
            });
        }

        previous
            .zip(current)
            .and_then(|(previous, current)| (previous == current).then_some(previous))
    }

    fn has_dead_inference_keys<T>(&self, value: &T) -> bool
    where
        T: TyVisitable<'db>,
    {
        inference_keys(self.db, value)
            .into_iter()
            .any(|key| key.0 as usize >= self.table.len())
    }

    pub(super) fn normalize_trait_goal(&mut self, goal: TraitInstId<'db>) -> TraitInstId<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let goal = goal.fold_with(db, &mut self.table);
        let args: Vec<_> = goal
            .args(db)
            .iter()
            .copied()
            .map(|ty| normalize_ty(db, ty, scope, assumptions))
            .collect();
        let assoc_type_bindings: IndexMap<_, _> = goal
            .assoc_type_bindings(db)
            .iter()
            .map(|(&name, &ty)| (name, normalize_ty(db, ty, scope, assumptions)))
            .collect();
        TraitInstId::new(db, goal.def(db), args, assoc_type_bindings)
    }

    fn trait_goal_is_concrete_for_diagnostics(&self, goal: TraitInstId<'db>) -> bool {
        !collect_flags(self.db, goal).intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
    }

    fn dedup_equivalent_trait_insts(&self, insts: Vec<TraitInstId<'db>>) -> Vec<TraitInstId<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for inst in insts {
            if seen.insert(Canonical::new(db, inst)) {
                unique.push(inst);
            }
        }
        unique
    }

    fn dedup_equivalent_pending_method_candidates(
        &self,
        candidates: Vec<env::PendingMethodCandidate<'db>>,
    ) -> Vec<env::PendingMethodCandidate<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for candidate in candidates {
            if seen.insert(Canonical::new(db, candidate.inst)) {
                unique.push(candidate);
            }
        }
        unique
    }

    fn call_constraint_diag_info(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<CallConstraintDiagInfo<'db>> {
        Some(CallConstraintDiagInfo {
            callable_def,
            bound_span: self.call_constraint_bound_span(callable_def, constraint_idx)?,
        })
    }

    fn call_constraint_bound_span(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<DynLazySpan<'db>> {
        let pairs = collect_func_decl_constraint_pairs(self.db, callable_def);
        let (_, source) = pairs.get(constraint_idx)?;
        Some(self.call_constraint_owner_bound_span(*source))
    }

    fn call_constraint_owner_bound_span(&self, bound: PredicateSource<'db>) -> DynLazySpan<'db> {
        match bound {
            PredicateSource::GenericParamBound {
                owner,
                param_idx,
                bound_idx,
            } => match owner {
                GenericParamOwner::Func(func) => func
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Struct(struct_) => struct_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Enum(enum_) => enum_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::TypeAlias(type_alias) => type_alias
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Impl(impl_) => impl_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Trait(trait_) => trait_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
            PredicateSource::WherePredicateBound {
                owner,
                pred_idx,
                bound_idx,
            } => match owner {
                WhereClauseOwner::Func(func) => func
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Struct(struct_) => struct_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Enum(enum_) => enum_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Impl(impl_) => impl_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Trait(trait_) => trait_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
        }
    }

    fn process_trait_obligation(
        &mut self,
        mut obligation: env::TraitObligation<'db>,
        final_pass: bool,
    ) -> TraitObligationOutcome<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        if self.has_dead_inference_keys(&obligation.goal) {
            return TraitObligationOutcome::Discharged;
        }

        if final_pass {
            // Apply literal fallbacks (e.g. string type variables defaulting to
            // `String<N>`) before the last solving attempt. The typed body will
            // contain the fallback types anyway, and without this an
            // unsatisfied goal that mentions a literal variable would be
            // discharged as "not concrete" below, silently suppressing the
            // diagnostic and letting the error surface as an ICE in later
            // lowering stages.
            let mut prober = env::Prober::new(&mut self.table, scope);
            obligation.goal = obligation.goal.fold_with(db, &mut prober);
        }

        obligation.goal = self.normalize_trait_goal(obligation.goal);
        let goal = obligation.goal;
        let flags = collect_flags(db, goal);
        if flags.contains(TyFlags::HAS_INVALID) || self.has_dead_inference_keys(&goal) {
            return TraitObligationOutcome::Discharged;
        }

        let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
        let query = CanonicalGoalQuery::new(db, goal, assumptions);
        match is_goal_query_satisfiable(db, solve_cx, &query) {
            GoalSatisfiability::Satisfied(solution) => {
                if goal.self_ty(db).is_ty_var(db) {
                    let outcome = if final_pass {
                        // Don't invent the self type at the end either; leave it alone
                        TraitObligationOutcome::Discharged
                    } else {
                        TraitObligationOutcome::Requeue(obligation)
                    };
                    return outcome;
                }
                let solved = query.extract_solution(&mut self.table, solution).inst;
                if self.has_dead_inference_keys(&solved) {
                    return TraitObligationOutcome::Discharged;
                }
                self.table.unify(goal, solved).unwrap();
                if self.normalize_trait_goal(goal) != goal {
                    TraitObligationOutcome::Progressed
                } else {
                    TraitObligationOutcome::Discharged
                }
            }
            GoalSatisfiability::NeedsConfirmation {
                solutions: ambiguous,
                completion,
            } => {
                // Answer identity includes the selected implementor, whereas
                // inference below deduplicates by the resulting trait instance.
                // If that proof-identity cutoff is reached with only one
                // distinct inferred instance, ask a fair fallback search
                // whether any different instance exists. `NotFound` is only
                // returned after saturation; resource/depth stops remain
                // incomplete and cannot justify committing inference.
                let first_projected = ambiguous
                    .first()
                    .map(|solution| Canonical::new(db, solution.value.inst));
                let fallback_proved_complete = completion.hit_root_answer_limit()
                    && first_projected.is_some_and(|first_projected| {
                        ambiguous.iter().all(|solution| {
                            Canonical::new(db, solution.value.inst) == first_projected
                        }) && goal_query_has_no_distinct_solution(
                            db,
                            solve_cx,
                            &query,
                            first_projected,
                        )
                    });
                let answers_complete_for_inference =
                    completion.is_saturated() || fallback_proved_complete;

                let mut candidates: Vec<_> = ambiguous
                    .iter()
                    .map(|solution| query.extract_solution(&mut self.table, *solution).inst)
                    .collect();
                candidates.retain(|candidate| !self.has_dead_inference_keys(candidate));
                let candidates = self.dedup_equivalent_trait_insts(candidates);

                if let [solution] = candidates.as_slice()
                    && answers_complete_for_inference
                    && !goal.self_ty(db).is_ty_var(db)
                {
                    if self.table.unify(goal, *solution).is_ok()
                        && self.normalize_trait_goal(goal) != goal
                    {
                        TraitObligationOutcome::Progressed
                    } else {
                        TraitObligationOutcome::Discharged
                    }
                } else {
                    if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                        let required_by = match obligation.origin {
                            env::TraitObligationOrigin::CallConstraint {
                                callable_def,
                                constraint_idx,
                                ..
                            } => self.call_constraint_diag_info(callable_def, constraint_idx),
                            env::TraitObligationOrigin::GenericConfirmation { .. } => None,
                        };
                        self.push_diag(BodyDiag::AmbiguousTraitInst {
                            primary: obligation.span.clone(),
                            cands: candidates.into_iter().collect(),
                            required_by,
                        });
                        return TraitObligationOutcome::Discharged;
                    }

                    if final_pass {
                        TraitObligationOutcome::Discharged
                    } else {
                        TraitObligationOutcome::Requeue(obligation)
                    }
                }
            }
            GoalSatisfiability::UnSat(subgoal) => {
                if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                    let required_by = match obligation.origin {
                        env::TraitObligationOrigin::CallConstraint {
                            callable_def,
                            constraint_idx,
                            ..
                        } => self.call_constraint_diag_info(callable_def, constraint_idx),
                        env::TraitObligationOrigin::GenericConfirmation { .. } => None,
                    };
                    let unsat = subgoal.map(|goal| query.extract_subgoal(&mut self.table, goal));
                    self.push_diag(TyDiagCollection::from(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: obligation.span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: unsat,
                            required_by,
                        },
                    ));
                    TraitObligationOutcome::Discharged
                } else if final_pass {
                    TraitObligationOutcome::Discharged
                } else {
                    TraitObligationOutcome::Requeue(obligation)
                }
            }
            GoalSatisfiability::ContainsInvalid => TraitObligationOutcome::Discharged,
        }
    }

    fn eagerly_process_callable_constraints(
        &mut self,
        callable: &Callable<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) {
        let db = self.db;
        let constraints = collect_func_decl_constraints(db, callable.callable_def(), true);
        let instantiated = constraints.instantiate(db, callable.generic_args());

        for (constraint_idx, &constraint) in instantiated.list(db).iter().enumerate() {
            let constraint = if let Some(inst) = callable.trait_inst() {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(db, &mut subst)
            } else {
                constraint
            };
            let constraint = self.normalize_trait_goal(constraint);
            if collect_flags(db, constraint).contains(TyFlags::HAS_INVALID) {
                continue;
            }

            let obligation = env::TraitObligation {
                goal: constraint,
                origin: env::TraitObligationOrigin::CallConstraint {
                    call_expr,
                    callable_def: callable.callable_def(),
                    constraint_idx,
                },
                span: span.clone(),
            };
            match self.process_trait_obligation(obligation, false) {
                TraitObligationOutcome::Discharged | TraitObligationOutcome::Progressed => {}
                TraitObligationOutcome::Requeue(_) => {}
            }
        }
    }

    fn callable_requirements_may_be_satisfied(&mut self, callable: &Callable<'db>) -> bool {
        let db = self.db;
        let solve_cx =
            TraitSolveCx::new(db, self.env.scope()).with_assumptions(self.env.assumptions());
        let mut goals = Vec::new();
        if let Some(inst) = callable.trait_inst() {
            goals.push(self.normalize_trait_goal(inst));
        }

        let constraints = collect_func_decl_constraints(db, callable.callable_def(), true)
            .instantiate(db, callable.generic_args());
        for &constraint in constraints.list(db) {
            let constraint = if let Some(inst) = callable.trait_inst() {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(db, &mut subst)
            } else {
                constraint
            };
            goals.push(self.normalize_trait_goal(constraint));
        }

        goals.into_iter().all(|goal| {
            !matches!(
                is_goal_satisfiable(db, solve_cx, goal),
                GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
            )
        })
    }

    fn diagnose_callable_requirements(
        &mut self,
        callable: &Callable<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) {
        if let Some(inst) = callable.trait_inst() {
            let goal = self.normalize_trait_goal(inst);
            let _ = self.process_trait_obligation(
                env::TraitObligation {
                    goal,
                    origin: env::TraitObligationOrigin::GenericConfirmation { expr: call_expr },
                    span: span.clone(),
                },
                true,
            );
        }

        let constraints = collect_func_decl_constraints(self.db, callable.callable_def(), true)
            .instantiate(self.db, callable.generic_args());
        for (constraint_idx, &constraint) in constraints.list(self.db).iter().enumerate() {
            let constraint = if let Some(inst) = callable.trait_inst() {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(self.db, &mut subst)
            } else {
                constraint
            };
            let goal = self.normalize_trait_goal(constraint);
            let _ = self.process_trait_obligation(
                env::TraitObligation {
                    goal,
                    origin: env::TraitObligationOrigin::CallConstraint {
                        call_expr,
                        callable_def: callable.callable_def(),
                        constraint_idx,
                    },
                    span: span.clone(),
                },
                true,
            );
        }
    }

    fn resolve_deferred(&mut self) {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum CandidateViability {
            Viable,
            ReturnTypeMismatch,
            Incompatible,
        }

        let probe_candidate = |this: &mut Self,
                               pending: &env::PendingMethod<'db>,
                               expr_ty: TyId<'db>,
                               receiver: ExprId,
                               generic_args: Option<crate::hir_def::GenericArgListId<'db>>,
                               call_args: &[crate::hir_def::CallArg<'db>],
                               candidate: env::PendingMethodCandidate<'db>,
                               collect_diagnostics: bool| {
            let snap = this.snapshot_closure_replay_transaction();
            let diag_len = this.diags.len();
            let mut return_type_matches = true;

            let result = (|| {
                let inst = candidate.inst;
                let recv_ty = {
                    let mut prober = env::Prober::new(&mut this.table, scope);
                    pending.recv_ty.fold_with(db, &mut prober)
                };

                let inst_self = this.table.instantiate_to_term(inst.self_ty(db));
                let recv_ty = if this.table.unify(inst_self, recv_ty).is_ok() {
                    recv_ty
                } else {
                    let recv_inner = recv_ty.as_capability(db).map(|(_, inner)| inner)?;
                    this.table.unify(inst_self, recv_inner).ok()?;
                    recv_inner
                };

                let func_ty = try_instantiate_trait_method(
                    db,
                    candidate.method,
                    &mut this.table,
                    recv_ty,
                    inst,
                )
                .ok()?;
                let func_ty = this.table.instantiate_to_term(func_ty);
                let mut callable =
                    Callable::new(db, func_ty, receiver.span(body).into(), Some(inst)).ok()?;

                let call_args_pack = callable.call_trait_args_pack_ty(db, scope).map(|ty| {
                    normalize_ty(db, ty.fold_with(db, &mut this.table), scope, assumptions)
                });
                if let Some(args_ty) = call_args_pack.filter(|ty| !ty.is_tuple(db)) {
                    if collect_diagnostics {
                        let primary = if pending.callee_is_receiver {
                            pending.expr.span(body).into_call_expr().args().into()
                        } else {
                            pending
                                .expr
                                .span(body)
                                .into_method_call_expr()
                                .args()
                                .into()
                        };
                        this.push_diag(BodyDiag::CallArgsMustBeTuple { primary, args_ty });
                    }
                    return None;
                }
                let expected_arg_tys = if let Some(pack) = call_args_pack {
                    let mut args = vec![callable.arg_ty(db, 0)?];
                    args.extend(pack.field_types(db));
                    args
                } else {
                    (0..callable.callable_def.arg_tys(db).len())
                        .map(|idx| callable.arg_ty(db, idx))
                        .collect::<Option<Vec<_>>>()?
                };
                let expected_arity = expected_arg_tys.len();
                let given_arity = call_args.len() + 1;
                if expected_arity != given_arity {
                    return None;
                }

                if let Some(generic_args) = generic_args {
                    match unify_explicit_call_generic_args(
                        &mut callable,
                        this,
                        generic_args,
                        HoleAnchor::BodySyntax {
                            body,
                            site: BodyHoleSite::Expr(pending.expr),
                        },
                        |this, _, given, current| this.table.unify(given, *current).is_ok(),
                    ) {
                        Ok(()) => {}
                        Err(CallGenericArgUnifyError::ArityMismatch { .. })
                        | Err(CallGenericArgUnifyError::UnificationFailed) => return None,
                    }
                }

                let receiver_prop = this.env.typed_expr(receiver)?;
                let mut all_args = Vec::with_capacity(call_args.len() + 1);
                all_args.push((receiver, receiver_prop.clone()));
                for arg in call_args.iter() {
                    let prop = this.env.typed_expr(arg.expr)?;
                    all_args.push((arg.expr, prop));
                }

                for ((arg_expr, arg_prop), expected) in all_args.iter().zip(expected_arg_tys) {
                    let expected = normalize_ty(
                        db,
                        expected.fold_with(db, &mut this.table),
                        scope,
                        assumptions,
                    );
                    let given = normalize_ty(
                        db,
                        arg_prop.ty.fold_with(db, &mut this.table),
                        scope,
                        assumptions,
                    );
                    let given = this
                        .try_coerce_capability_to_expected(given, expected)
                        .unwrap_or(given);
                    if !this.ty_unifies(given, expected) {
                        if collect_diagnostics {
                            this.unify_ty(
                                Typeable::Expr(*arg_expr, arg_prop.clone()),
                                given,
                                expected,
                            );
                        }
                        return None;
                    }
                    this.table.unify(given, expected).ok()?;
                }

                let args_span = if pending.callee_is_receiver {
                    pending.expr.span(body).into_call_expr().args()
                } else {
                    pending.expr.span(body).into_method_call_expr().args()
                };
                callable.check_args(
                    this,
                    call_args,
                    args_span,
                    Some((receiver, receiver_prop)),
                    true,
                );
                callable.process_constraints(this, pending.expr, pending.span.clone());
                this.specialize_callable_layout_args(&mut callable, Some(receiver), call_args);
                this.check_callable_effects(pending.expr, &mut callable);
                if this.diags.len() != diag_len {
                    return None;
                }
                let ret_ty = normalize_ty(
                    db,
                    callable.ret_ty(db).fold_with(db, &mut this.table),
                    scope,
                    assumptions,
                );
                let expected_ret_ty = normalize_ty(
                    db,
                    expr_ty.fold_with(db, &mut this.table),
                    scope,
                    assumptions,
                );
                let comparable_ret_ty = this
                    .try_coerce_capability_to_expected(ret_ty, expected_ret_ty)
                    .unwrap_or(ret_ty);
                return_type_matches = this.ty_unifies(comparable_ret_ty, expected_ret_ty);
                if candidate.needs_confirmation {
                    this.env.register_trait_obligation(env::TraitObligation {
                        goal: inst,
                        origin: env::TraitObligationOrigin::GenericConfirmation {
                            expr: pending.expr,
                        },
                        span: pending.span.clone(),
                    });
                }
                if pending.callee_is_receiver {
                    this.env
                        .register_semantic_value_call(pending.expr, callable);
                } else {
                    this.env.register_semantic_call(pending.expr, callable);
                }
                let original_prop = this.env.typed_expr(pending.expr)?;
                let (replayed, replay_satisfied) = this.replay_deferred_expr_with_closure_context(
                    pending.expr,
                    ExprProp::new(ret_ty, true),
                );
                if !replay_satisfied
                    || (return_type_matches
                        && !this.reconcile_deferred_expr_prop(
                            pending.expr,
                            original_prop.clone(),
                            replayed.clone(),
                        ))
                {
                    return None;
                }
                if !return_type_matches && collect_diagnostics {
                    this.unify_ty(
                        Typeable::Expr(pending.expr, original_prop),
                        replayed.ty,
                        expected_ret_ty,
                    );
                }
                let replayed_callable = this.env.callable_expr(pending.expr)?.clone();
                if !this.callable_requirements_may_be_satisfied(&replayed_callable) {
                    if collect_diagnostics {
                        this.diagnose_callable_requirements(
                            &replayed_callable,
                            pending.expr,
                            pending.span.clone(),
                        );
                    }
                    return None;
                }
                replayed_callable.enqueue_constraints(this, pending.expr, pending.span.clone());

                Some(())
            })()
            .is_some()
                && this.diags.len() == diag_len;
            let viability = if result {
                if return_type_matches {
                    CandidateViability::Viable
                } else {
                    CandidateViability::ReturnTypeMismatch
                }
            } else {
                CandidateViability::Incompatible
            };
            let diagnostics = this.diags[diag_len..].to_vec();
            this.rollback_closure_replay_transaction(snap);
            (viability, diagnostics)
        };

        let mut final_method_restarts = FxHashSet::default();
        'scheduler: loop {
            // Fixed-point pass over deferred tasks.
            let mut progressed = true;
            while progressed {
                progressed = false;
                let task_count = self.env.deferred_len();
                for _ in 0..task_count {
                    let Some(task) = self.env.pop_deferred_task() else {
                        break;
                    };
                    match task {
                        env::DeferredTask::Obligation(obligation) => {
                            match self.process_trait_obligation(obligation, false) {
                                TraitObligationOutcome::Discharged => {}
                                TraitObligationOutcome::Progressed => progressed = true,
                                TraitObligationOutcome::Requeue(obligation) => {
                                    self.env.register_trait_obligation(obligation);
                                }
                            }
                        }
                        env::DeferredTask::Method(pending) => {
                            let (receiver, generic_args, call_args) =
                                match (pending.callee_is_receiver, pending.expr.data(db, body)) {
                                    (
                                        false,
                                        Partial::Present(Expr::MethodCall(
                                            receiver,
                                            _,
                                            generic_args,
                                            args,
                                        )),
                                    ) => (*receiver, Some(*generic_args), args.as_slice()),
                                    (true, Partial::Present(Expr::Call(receiver, args))) => {
                                        (*receiver, None, args.as_slice())
                                    }
                                    _ => continue,
                                };

                            let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                                continue;
                            };
                            let expr_ty = {
                                let mut prober = env::Prober::new(&mut self.table, scope);
                                expr_prop.ty.fold_with(db, &mut prober)
                            };
                            if expr_ty.has_invalid(db) {
                                self.env.register_pending_method(pending);
                                continue;
                            }

                            let (strict, soft): (Vec<_>, Vec<_>) = pending
                                .candidates
                                .iter()
                                .copied()
                                .filter_map(|candidate| {
                                    let (viability, _) = probe_candidate(
                                        self,
                                        &pending,
                                        expr_ty,
                                        receiver,
                                        generic_args,
                                        call_args,
                                        candidate,
                                        false,
                                    );
                                    (viability != CandidateViability::Incompatible)
                                        .then_some((candidate, viability))
                                })
                                .partition(|(_, viability)| {
                                    *viability == CandidateViability::Viable
                                });
                            let mut viable = if strict.is_empty() {
                                soft.into_iter().map(|(candidate, _)| candidate).collect()
                            } else {
                                strict.into_iter().map(|(candidate, _)| candidate).collect()
                            };
                            viable = self.dedup_equivalent_pending_method_candidates(viable);
                            if let Some(priority) =
                                viable.iter().map(|candidate| candidate.priority).min()
                            {
                                viable.retain(|candidate| candidate.priority == priority);
                            }

                            if let [candidate] = viable.as_slice() {
                                if self.env.callable_expr(pending.expr).is_none() {
                                    let transaction = self
                                        .env
                                        .deferred_closure_replay_context(pending.expr)
                                        .is_some()
                                        .then(|| self.snapshot_closure_replay_transaction());
                                    let inst = candidate.inst;

                                    let receiver_prop = self
                                        .env
                                        .typed_expr(receiver)
                                        .unwrap_or_else(|| ExprProp::invalid(db));
                                    let recv_ty = {
                                        let mut prober = env::Prober::new(&mut self.table, scope);
                                        pending.recv_ty.fold_with(db, &mut prober)
                                    };

                                    let func_ty = self.instantiate_trait_method_to_term(
                                        candidate.method,
                                        recv_ty,
                                        inst,
                                    );

                                    let mut callable = match Callable::new(
                                        db,
                                        func_ty,
                                        receiver.span(body).into(),
                                        Some(inst),
                                    ) {
                                        Ok(callable) => callable,
                                        Err(diag) => {
                                            if let Some(transaction) = transaction {
                                                self.rollback_closure_replay_transaction(
                                                    transaction,
                                                );
                                            }
                                            self.push_diag(diag);
                                            progressed = true;
                                            continue;
                                        }
                                    };

                                    if candidate.needs_confirmation {
                                        self.env.register_trait_obligation(env::TraitObligation {
                                            goal: inst,
                                            origin:
                                                env::TraitObligationOrigin::GenericConfirmation {
                                                    expr: pending.expr,
                                                },
                                            span: pending.span.clone(),
                                        });
                                    }

                                    if let Some(generic_args) = generic_args {
                                        let call_span =
                                            pending.expr.span(body).into_method_call_expr();
                                        if !callable.unify_generic_args(
                                            self,
                                            generic_args,
                                            HoleAnchor::BodySyntax {
                                                body,
                                                site: BodyHoleSite::Expr(pending.expr),
                                            },
                                            call_span.generic_args(),
                                        ) {
                                            if let Some(transaction) = transaction {
                                                self.rollback_closure_replay_transaction(
                                                    transaction,
                                                );
                                            }
                                            progressed = true;
                                            continue;
                                        }
                                    }

                                    let args_span = if pending.callee_is_receiver {
                                        pending.expr.span(body).into_call_expr().args()
                                    } else {
                                        pending.expr.span(body).into_method_call_expr().args()
                                    };
                                    callable.check_args(
                                        self,
                                        call_args,
                                        args_span,
                                        Some((receiver, receiver_prop)),
                                        true,
                                    );
                                    callable.process_constraints(
                                        self,
                                        pending.expr,
                                        pending.span.clone(),
                                    );
                                    self.specialize_callable_layout_args(
                                        &mut callable,
                                        Some(receiver),
                                        call_args,
                                    );

                                    self.check_callable_effects(pending.expr, &mut callable);

                                    let ret_ty = self.normalize_ty(callable.ret_ty(db));
                                    let code_region_kind = if !pending.callee_is_receiver
                                        && let Some(kind) = self
                                            .code_region_method_kind(recv_ty, pending.method_name)
                                        && call_args.len() == 1
                                        && self
                                            .env
                                            .typed_expr(call_args[0].expr)
                                            .map(|prop| {
                                                ty_may_be_code_region_token(
                                                    db,
                                                    normalize_ty(db, prop.ty, scope, assumptions),
                                                )
                                            })
                                            .unwrap_or(false)
                                    {
                                        Some(kind)
                                    } else {
                                        None
                                    };
                                    if let Some(kind) = code_region_kind {
                                        if !self.reconcile_deferred_expr_ty(
                                            pending.expr,
                                            expr_prop,
                                            ret_ty,
                                        ) {
                                            if let Some(transaction) = transaction {
                                                self.rollback_closure_replay_transaction(
                                                    transaction,
                                                );
                                            }
                                            progressed = true;
                                            continue;
                                        }
                                        self.env.register_code_region_intrinsic(
                                            pending.expr,
                                            callable.clone(),
                                            call_args[0].expr,
                                            kind,
                                        );
                                        if let Some(transaction) = transaction {
                                            self.env.consume_deferred_closure_replay_context(
                                                pending.expr,
                                            );
                                            self.commit_closure_replay_transaction(transaction);
                                        }
                                    } else {
                                        if pending.callee_is_receiver {
                                            self.env.register_semantic_value_call(
                                                pending.expr,
                                                callable.clone(),
                                            );
                                        } else {
                                            self.env.register_semantic_call(
                                                pending.expr,
                                                callable.clone(),
                                            );
                                        }
                                        let resolved = ExprProp::new(ret_ty, true);
                                        if let Some(transaction) = transaction {
                                            let (resolved, replay_satisfied) = self
                                                .replay_deferred_expr_with_closure_context(
                                                    pending.expr,
                                                    resolved,
                                                );
                                            if !replay_satisfied
                                                || !self.reconcile_deferred_expr_prop(
                                                    pending.expr,
                                                    expr_prop.clone(),
                                                    resolved,
                                                )
                                            {
                                                self.rollback_closure_replay_transaction(
                                                    transaction,
                                                );
                                                self.env.register_pending_method(pending.clone());
                                                continue;
                                            }
                                            let replayed_callable = self
                                                .env
                                                .callable_expr(pending.expr)
                                                .cloned()
                                                .expect("replayed method callable must remain registered");
                                            replayed_callable.enqueue_constraints(
                                                self,
                                                pending.expr,
                                                pending.span.clone(),
                                            );
                                            self.env.consume_deferred_closure_replay_context(
                                                pending.expr,
                                            );
                                            self.commit_closure_replay_transaction(transaction);
                                        } else if !self.reconcile_deferred_expr_prop(
                                            pending.expr,
                                            expr_prop.clone(),
                                            resolved,
                                        ) {
                                            progressed = true;
                                            continue;
                                        } else {
                                            let finalized_callable = self
                                                .env
                                                .callable_expr(pending.expr)
                                                .cloned()
                                                .expect("finalized method callable must remain registered");
                                            finalized_callable.enqueue_constraints(
                                                self,
                                                pending.expr,
                                                pending.span.clone(),
                                            );
                                        }
                                    }
                                }

                                progressed = true;
                            } else {
                                self.env.register_pending_method(pending);
                            }
                        }
                        env::DeferredTask::MethodLookup(pending) => {
                            match self.resolve_pending_method_lookup(&pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_method_lookup(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                        env::DeferredTask::CallableLookup(pending) => {
                            match self.resolve_pending_callable_lookup(pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_callable_lookup(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                        env::DeferredTask::PrimitiveOp(pending) => {
                            match self.resolve_pending_primitive_op(&pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_primitive_op(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                        env::DeferredTask::Field(pending) => {
                            match self.resolve_pending_field(&pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_field(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                        env::DeferredTask::Cast(pending) => {
                            match self.resolve_pending_cast(&pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_cast(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                        env::DeferredTask::ForLoopSeq(pending) => {
                            match self.resolve_pending_for_loop_seq(pending) {
                                expr::PendingPrimitiveOpResolution::Pending => {
                                    self.env.register_pending_for_loop_seq(pending);
                                }
                                expr::PendingPrimitiveOpResolution::Resolved => {
                                    progressed = true;
                                }
                                expr::PendingPrimitiveOpResolution::Done => {}
                            }
                        }
                    }
                }
            }

            // Emit diagnostics for remaining tasks. Resolving a final
            // obligation can enqueue new work, so keep draining the live
            // queue. If that work leaves exactly one viable method, return it
            // to the fixed-point phase once so it is finalized rather than
            // silently discarded.
            while let Some(task) = self.env.pop_deferred_task() {
                match task {
                    env::DeferredTask::Obligation(obligation) => {
                        let _ = self.process_trait_obligation(obligation, true);
                    }
                    env::DeferredTask::Method(pending) => {
                        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                            continue;
                        };
                        let expr_ty = {
                            let mut prober = env::Prober::new(&mut self.table, scope);
                            expr_prop.ty.fold_with(db, &mut prober)
                        };
                        if expr_ty.has_invalid(db) {
                            continue;
                        }

                        let (receiver, generic_args, call_args) =
                            match (pending.callee_is_receiver, pending.expr.data(db, body)) {
                                (
                                    false,
                                    Partial::Present(Expr::MethodCall(
                                        receiver,
                                        _,
                                        generic_args,
                                        args,
                                    )),
                                ) => (*receiver, Some(*generic_args), args.as_slice()),
                                (true, Partial::Present(Expr::Call(receiver, args))) => {
                                    (*receiver, None, args.as_slice())
                                }
                                _ => continue,
                            };

                        let probes: Vec<_> = pending
                            .candidates
                            .iter()
                            .copied()
                            .map(|candidate| {
                                let (viable, diagnostics) = probe_candidate(
                                    self,
                                    &pending,
                                    expr_ty,
                                    receiver,
                                    generic_args,
                                    call_args,
                                    candidate,
                                    true,
                                );
                                (candidate, viable, diagnostics)
                            })
                            .collect();
                        let failure_diagnostics =
                            probes.iter().find_map(|(_, viability, diagnostics)| {
                                (*viability == CandidateViability::Incompatible
                                    && !diagnostics.is_empty())
                                .then(|| diagnostics.clone())
                            });
                        let candidates =
                            probes.into_iter().filter_map(|(candidate, viability, _)| {
                                (viability != CandidateViability::Incompatible)
                                    .then_some((candidate, viability))
                            });
                        let (strict, soft): (Vec<_>, Vec<_>) = candidates
                            .partition(|(_, viability)| *viability == CandidateViability::Viable);
                        let mut viable = if strict.is_empty() {
                            soft.into_iter().map(|(candidate, _)| candidate).collect()
                        } else {
                            strict.into_iter().map(|(candidate, _)| candidate).collect()
                        };
                        viable = self.dedup_equivalent_pending_method_candidates(viable);
                        if let Some(priority) =
                            viable.iter().map(|candidate| candidate.priority).min()
                        {
                            viable.retain(|candidate| candidate.priority == priority);
                        }
                        if viable.len() == 1 {
                            if final_method_restarts.insert(pending.expr) {
                                self.env.register_pending_method(pending);
                                continue 'scheduler;
                            }
                            // The candidate was viable in both diagnostic
                            // probes but could not commit during a complete
                            // fixed-point pass. Treat that as unresolved
                            // inference state; it is neither an ambiguity nor
                            // evidence that the concrete trait is absent.
                            self.push_diag(BodyDiag::TypeMustBeKnown(
                                pending.expr.span(body).into(),
                            ));
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        } else if viable.len() > 1 {
                            self.push_diag(BodyDiag::AmbiguousTrait {
                                primary: pending.span.clone(),
                                method_name: pending.method_name,
                                traits: viable
                                    .into_iter()
                                    .map(|candidate| candidate.inst)
                                    .collect(),
                            });
                        } else if let Some(diagnostics) = failure_diagnostics {
                            self.diags.extend(diagnostics);
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        } else if expr_ty.has_var(db) {
                            self.push_diag(BodyDiag::TypeMustBeKnown(
                                pending.expr.span(body).into(),
                            ));
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        } else {
                            // A concrete candidate can still fail before it
                            // produces a richer local diagnostic (for example,
                            // at an explicit generic argument). Preserve a
                            // deterministic method-level error instead of
                            // dropping the call silently or claiming that its
                            // receiver type is unknown.
                            let trait_name = pending
                                .candidates
                                .first()
                                .and_then(|candidate| candidate.inst.def(db).name(db).to_opt())
                                .unwrap_or(pending.method_name);
                            self.push_diag(BodyDiag::TraitNotImplemented {
                                primary: pending.span.clone(),
                                ty: expr_ty.pretty_print(db).to_string(),
                                trait_name,
                            });
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        }
                    }
                    env::DeferredTask::MethodLookup(pending) => {
                        if matches!(
                            self.resolve_pending_method_lookup(&pending),
                            expr::PendingPrimitiveOpResolution::Pending
                        ) && let Partial::Present(Expr::MethodCall(receiver, ..)) =
                            pending.expr.data(db, body)
                        {
                            self.push_diag(BodyDiag::TypeMustBeKnown(receiver.span(body).into()));
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        }
                    }
                    env::DeferredTask::CallableLookup(pending) => {
                        if matches!(
                            self.resolve_pending_callable_lookup(pending),
                            expr::PendingPrimitiveOpResolution::Pending
                        ) && let Partial::Present(Expr::Call(callee, ..)) =
                            pending.expr.data(db, body)
                        {
                            self.push_diag(BodyDiag::TypeMustBeKnown(callee.span(body).into()));
                            self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                        }
                    }
                    env::DeferredTask::PrimitiveOp(pending) => {
                        let _ = self.resolve_pending_primitive_op(&pending);
                    }
                    env::DeferredTask::Field(pending) => {
                        if matches!(
                            self.resolve_pending_field(&pending),
                            expr::PendingPrimitiveOpResolution::Pending
                        ) {
                            self.push_diag(BodyDiag::TypeMustBeKnown(
                                pending.lhs.span(body).into(),
                            ));
                        }
                    }
                    env::DeferredTask::Cast(pending) => {
                        if matches!(
                            self.resolve_pending_cast(&pending),
                            expr::PendingPrimitiveOpResolution::Pending
                        ) {
                            self.push_diag(BodyDiag::TypeMustBeKnown(
                                pending.inner.span(body).into(),
                            ));
                        }
                    }
                    env::DeferredTask::ForLoopSeq(pending) => {
                        self.push_diag(BodyDiag::TypeMustBeKnown(pending.expr.span(body).into()));
                    }
                }
            }
            break;
        }
    }

    fn new_internal(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
            closure_expectations: FxHashMap::default(),
            closure_type_expectations: Vec::new(),
            active_closure_replay_defs: FxHashSet::default(),
            pending_place_checks: Vec::new(),
            pending_array_repeat_copy_checks: Vec::new(),
            effect_provider_keys: FxHashSet::default(),
            diags: Vec::new(),
        }
    }

    /// Resolves the pattern type and return type for a recv arm.
    /// Returns (pattern_type, return_type).
    fn resolve_recv_arm_types(
        &mut self,
        contract: Contract<'db>,
        msg_path: Option<PathId<'db>>,
        arm: ContractRecvArm<'db>,
        path_span: LazyPathSpan<'db>,
        arm_span: crate::span::item::LazyRecvArmSpan<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        let invalid_ty = TyId::invalid(self.db, InvalidCause::Other);

        if arm.is_fallback(self.db) {
            return (TyId::unit(self.db), TyId::unit(self.db));
        }

        // Get variant path from arm pattern
        let Some(variant_path) = arm.variant_path(self.db) else {
            return (invalid_ty, invalid_ty);
        };

        let assumptions = self.env.assumptions();

        // Resolve based on whether this is a named or bare recv block
        let resolved = if let Some(msg_mod) = contract::resolve_recv_msg_mod(
            self.db,
            contract,
            msg_path,
            path_span,
            &mut self.diags,
            false,
        ) {
            // Named recv block - resolve within the msg module
            match contract::resolve_variant_in_msg(self.db, msg_mod, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    // when the pattern doesn't resolve to a valid msg variant
                    return (invalid_ty, invalid_ty);
                }
            }
        } else if msg_path.is_none() {
            // Bare recv block - resolve from contract scope
            match contract::resolve_variant_bare(self.db, contract, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    return (invalid_ty, invalid_ty);
                }
            }
        } else {
            // msg_path was Some but didn't resolve - diagnostics already emitted
            return (invalid_ty, invalid_ty);
        };

        let pat_ty = resolved.ty;

        // Get annotated return type from the arm
        let arm_ret_span = arm_span.clone().ret_ty();
        let annotated = arm
            .ret_ty
            .map(|hir_ty| self.lower_ty(hir_ty, arm_ret_span.clone(), true));
        let variant_ret = contract::get_msg_variant_return_type(self.db, pat_ty, self.env.scope());

        let ret_ty = match (variant_ret, annotated) {
            (Some(var_ty), Some(annot_ty)) => {
                self.equate_ty(annot_ty, var_ty, arm_ret_span.into());
                var_ty
            }
            (Some(var_ty), None) => {
                // Only require annotation if return type is not unit
                if var_ty != TyId::unit(self.db) {
                    self.push_diag(BodyDiag::RecvArmRetTypeMissing {
                        primary: arm_span.pat().into(),
                        expected: var_ty,
                    });
                }
                var_ty
            }
            (None, Some(annot_ty)) => annot_ty,
            (None, None) => TyId::unit(self.db),
        };

        (pat_ty, ret_ty)
    }

    fn push_diag(&mut self, diag: impl Into<FuncBodyDiag<'db>>) {
        self.diags.push(diag.into())
    }

    fn body(&self) -> Body<'db> {
        self.env.body()
    }

    fn ty_is_copy(&self, ty: TyId<'db>) -> bool {
        crate::analysis::ty::ty_is_copy(self.db, self.env.scope(), ty, self.env.assumptions())
    }

    fn copy_inner_from_borrow(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        let (_, inner) = ty.as_capability(self.db)?;
        self.ty_is_copy(inner).then_some(inner)
    }

    fn ty_unifies(&mut self, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
        let snapshot = self.snapshot_state();
        let unifies = self.table.unify(lhs, rhs).is_ok();
        self.rollback_state(snapshot);
        unifies
    }

    fn binding_interface_shape(
        &mut self,
        binding: LocalBinding<'db>,
    ) -> BindingInterfaceShape<'db> {
        let ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if let Some((_, value_ty)) = ty.as_capability(self.db) {
            return BindingInterfaceShape::PlaceCarrier {
                value_ty: self.normalize_ty(value_ty),
            };
        }
        let semantics = provider_semantics(self.db, self.env.scope(), self.env.assumptions(), ty);
        if matches!(
            semantics.evidence,
            ProviderLayoutEvidence::ResolvedHandle(_)
        ) && let Some(target_ty) = semantics.target_ty
        {
            return BindingInterfaceShape::DirectCarrier {
                target_ty: self.normalize_ty(target_ty),
            };
        }

        let provider = match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.env.resolved_provider_binding(site, idx),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        provider.map_or(BindingInterfaceShape::OrdinaryValue, |provider| {
            BindingInterfaceShape::ProviderValue {
                kind: provider.semantics.kind,
                value_ty: ty,
            }
        })
    }

    fn binding_path_read_semantics(
        &mut self,
        binding: LocalBinding<'db>,
        expr_ty: TyId<'db>,
    ) -> PathReadSemantics {
        let expr_ty = self.normalize_ty(expr_ty);
        let binding_ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if binding_ty == expr_ty {
            return PathReadSemantics::ReuseLocal;
        }

        match self.binding_interface_shape(binding) {
            BindingInterfaceShape::OrdinaryValue => PathReadSemantics::MaterializeValue,
            BindingInterfaceShape::ProviderValue { .. }
            | BindingInterfaceShape::DirectCarrier { .. } => PathReadSemantics::ForwardInterface,
            BindingInterfaceShape::PlaceCarrier { .. } => expr_ty
                .as_capability(self.db)
                .map_or(PathReadSemantics::MaterializeValue, |_| {
                    PathReadSemantics::ForwardInterface
                }),
        }
    }

    /// Contextual capability coercion:
    /// - `mut T -> ref T`
    /// - `mut/ref/view T -> view T`
    /// - `mut/ref/view T -> T` when `T: Copy`
    /// - `T -> view T`
    fn try_coerce_capability_to_expected(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        self.try_coerce_capability_expr_to_expected(None, actual, expected)
    }

    fn try_coerce_capability_for_expr_to_expected(
        &mut self,
        expr: ExprId,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let actual = self.normalize_ty(actual);
        let coerced = self.try_coerce_capability_expr_to_expected(Some(expr), actual, expected)?;
        if actual.as_capability(self.db).is_none()
            && matches!(
                coerced.as_capability(self.db),
                Some((CapabilityKind::View, _))
            )
            && self.env.expr_place(expr).is_none()
        {
            self.env.register_contextual_view_source(expr, actual);
        }
        Some(coerced)
    }

    fn reconcile_deferred_expr_ty(
        &mut self,
        expr: ExprId,
        expr_prop: ExprProp<'db>,
        actual: TyId<'db>,
    ) -> bool {
        let expected = {
            let mut prober = env::Prober::new(&mut self.table, self.env.scope());
            expr_prop.ty.fold_with(self.db, &mut prober)
        };
        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return false;
        }
        let actual = self
            .try_coerce_capability_for_expr_to_expected(expr, actual, expected)
            .unwrap_or(actual);
        !self
            .unify_ty(Typeable::Expr(expr, expr_prop), actual, expected)
            .has_invalid(self.db)
    }

    fn reconcile_deferred_expr_prop(
        &mut self,
        expr: ExprId,
        expr_prop: ExprProp<'db>,
        mut actual: ExprProp<'db>,
    ) -> bool {
        if !self.reconcile_deferred_expr_ty(expr, expr_prop, actual.ty) {
            return false;
        }
        actual.ty = self
            .env
            .typed_expr(expr)
            .expect("reconciled expression must remain typed")
            .ty;
        self.env.type_expr(expr, actual);
        true
    }

    fn try_coerce_capability_expr_to_expected(
        &mut self,
        expr: Option<ExprId>,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        if expected.is_ty_var(self.db) {
            if let Some((kind, inner)) = actual.as_capability(self.db)
                && self.ty_is_copy(inner)
                && (kind == CapabilityKind::View
                    || !matches!(
                        expected.data(self.db),
                        TyData::TyVar(var) if var.sort == TyVarSort::General
                    ))
            {
                return Some(inner);
            }
            return None;
        }

        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return None;
        }

        let actual_cap = actual.as_capability(self.db);
        let expected_cap = expected.as_capability(self.db);

        match (actual_cap, expected_cap) {
            (Some((given_kind, given_inner)), Some((required_kind, required_inner))) => {
                if given_kind.rank() < required_kind.rank() {
                    return None;
                }
                if !self.ty_unifies(given_inner, required_inner) {
                    return None;
                }
                let coerced = match required_kind {
                    CapabilityKind::Mut => TyId::borrow_mut_of(self.db, given_inner),
                    CapabilityKind::Ref => TyId::borrow_ref_of(self.db, given_inner),
                    CapabilityKind::View => TyId::view_of(self.db, given_inner),
                };
                Some(coerced)
            }
            (Some((given_kind, given_inner)), None) => {
                if !self.ty_unifies(given_inner, expected) {
                    return None;
                }

                if self.ty_is_copy(given_inner)
                    || self.ty_is_copy(expected)
                    || matches!(given_kind, CapabilityKind::View)
                        && expr.is_some_and(|expr| {
                            self.env.contextual_view_source(expr).is_some_and(|source| {
                                source.as_capability(self.db).is_none()
                                    && self.ty_unifies(source, given_inner)
                            })
                        })
                {
                    return Some(given_inner);
                }

                None
            }
            (None, Some((CapabilityKind::View, required_inner))) => {
                if !self.ty_unifies(actual, required_inner) {
                    return None;
                }
                Some(TyId::view_of(self.db, actual))
            }
            (None, Some((CapabilityKind::Ref | CapabilityKind::Mut, _))) | (None, None) => None,
        }
    }

    /// In "owned" contexts, non-`Copy` values are implicitly moved from places.
    ///
    /// `Copy` values may be duplicated implicitly.
    fn record_owned_value_use(&mut self, expr: ExprId, ty: TyId<'db>) {
        self.record_value_use(expr, ty, false);
    }

    /// Records the final consumer of a value returned from the innermost
    /// active closure. Return consumers are tracked separately from ordinary
    /// body uses because a deferred callable expectation can refine an owned
    /// return into a borrowed capability after the closure body was checked.
    fn record_return_value_use(&mut self, expr: ExprId, ty: TyId<'db>) {
        self.record_value_use(expr, ty, true);
    }

    fn record_value_use(&mut self, expr: ExprId, ty: TyId<'db>, closure_return: bool) {
        let mut ty = self.normalize_ty(ty);
        if ty.has_invalid(self.db) {
            let Some(prop) = self.env.typed_expr(expr) else {
                return;
            };
            ty = self.normalize_ty(prop.ty);
        }
        if ty.has_invalid(self.db) || ty == TyId::unit(self.db) || ty.is_never(self.db) {
            return;
        }

        let (access, capture_access) = if ty.has_var(self.db) {
            (
                ValueAccess::MoveIfNonCopy,
                ClosureCaptureAccess::MoveIfNonCopy,
            )
        } else if self.ty_is_copy(ty) {
            (ValueAccess::Read, ClosureCaptureAccess::Read)
        } else {
            (ValueAccess::Move, ClosureCaptureAccess::Move)
        };
        self.record_expr_value_use_kind(expr, access, capture_access, closure_return);
    }

    /// Records the ownership semantics of a value-producing expression at its
    /// underlying place sources.
    ///
    /// Blocks, effect scopes, casts, and control-flow expressions merely
    /// forward a value. Recursing through them here gives type checking,
    /// capture planning, semantic lowering, and borrow checking one shared
    /// account of which source places are read or moved.
    fn record_expr_value_use(
        &mut self,
        expr: ExprId,
        access: ValueAccess,
        capture_access: ClosureCaptureAccess,
    ) {
        self.record_expr_value_use_kind(expr, access, capture_access, false);
    }

    fn record_expr_value_use_kind(
        &mut self,
        expr: ExprId,
        access: ValueAccess,
        capture_access: ClosureCaptureAccess,
        closure_return: bool,
    ) {
        let db = self.db;
        let body = self.body();
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                self.env.set_expr_value_access(expr, access);
                let Some(last) = stmts.last() else {
                    return;
                };
                let Partial::Present(stmt) = last.data(db, body) else {
                    return;
                };
                let crate::hir_def::Stmt::Expr(tail) = stmt else {
                    return;
                };
                self.record_expr_value_use_kind(*tail, access, capture_access, closure_return);
            }
            Expr::With(_, body_expr) | Expr::Cast(body_expr, _) | Expr::If(_, body_expr, None) => {
                self.env.set_expr_value_access(expr, access);
                self.record_expr_value_use_kind(*body_expr, access, capture_access, closure_return);
            }
            Expr::If(_, then_expr, Some(else_expr)) => {
                self.env.set_expr_value_access(expr, access);
                self.record_expr_value_use_kind(*then_expr, access, capture_access, closure_return);
                self.record_expr_value_use_kind(*else_expr, access, capture_access, closure_return);
            }
            Expr::Match(_, arms) => {
                self.env.set_expr_value_access(expr, access);
                let Partial::Present(arms) = arms else {
                    return;
                };
                for arm in arms {
                    self.record_expr_value_use_kind(
                        arm.body,
                        access,
                        capture_access,
                        closure_return,
                    );
                }
            }
            _ => {
                if self.env.expr_place(expr).is_none() {
                    return;
                }
                self.env.set_expr_value_access(expr, access);
                if let Some(binding) = self.find_base_binding(expr) {
                    if closure_return {
                        self.env
                            .record_return_capture_access(binding, expr, capture_access);
                    } else {
                        self.env
                            .record_capture_expr_access(binding, expr, capture_access);
                    }
                }
            }
        }
    }

    fn lit_ty(&mut self, lit: &LitKind<'db>) -> TyId<'db> {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db);
                if len_bytes > MAX_INLINE_STRING_BYTES {
                    return TyId::invalid(
                        self.db,
                        InvalidCause::StringTooLarge {
                            max: MAX_INLINE_STRING_BYTES,
                            given: len_bytes,
                        },
                    );
                }
                self.table.new_var(
                    TyVarSort::String {
                        min_len: len_bytes,
                        fallback: self.string_literal_fallback(),
                    },
                    &Kind::Star,
                )
            }
        }
    }

    pub(crate) fn string_literal_ty(
        &mut self,
        string_id: StringId<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        if self.string_literal_should_use_byte_array(expected) {
            return self.string_literal_byte_array_ty(string_id.len_bytes(self.db));
        }
        if expected.is_core_dyn_string(self.db) {
            return expected;
        }

        let len_bytes = string_id.len_bytes(self.db);
        self.table.new_var(
            TyVarSort::String {
                min_len: len_bytes,
                fallback: self.string_literal_fallback(),
            },
            &Kind::Star,
        )
    }

    pub(crate) fn string_literal_byte_array_ty(&self, len_bytes: usize) -> TyId<'db> {
        let u8_ty = TyId::new(
            self.db,
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8)),
        );
        TyId::array_with_len(self.db, u8_ty, len_bytes)
    }

    pub(crate) fn string_literal_should_use_byte_array(&self, expected: TyId<'db>) -> bool {
        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());
        let (base, args) = expected.decompose_ty_app(self.db);
        matches!(
            base.data(self.db),
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::Array))
        ) && args.len() == 2
            && matches!(
                args[0].data(self.db),
                TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8))
            )
    }

    fn lower_ty(
        &mut self,
        hir_ty: HirTyId<'db>,
        span: LazyTySpan<'db>,
        star_kind_required: bool,
    ) -> TyId<'db> {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope(), self.env.assumptions());

        // If lowering failed, try to produce precise diagnostics (e.g., path resolution errors)
        if ty.has_invalid(self.db) {
            let diags = collect_ty_lower_errors(
                self.db,
                self.env.scope(),
                hir_ty,
                span.clone(),
                self.env.assumptions(),
            );
            if !diags.is_empty() {
                for d in diags {
                    self.push_diag(d);
                }
                // Avoid cascading kind errors for already-invalid types
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        }

        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.push_diag(diag)
        }

        if ty_contains_const_hole(self.db, ty) {
            self.push_diag(TyDiagCollection::from(
                TyLowerDiag::ConstHoleInValuePosition {
                    span: span.clone().into(),
                    ty,
                },
            ));
            return TyId::invalid(self.db, InvalidCause::Other);
        }

        if star_kind_required && ty.is_star_kind(self.db) {
            ty
        } else {
            let diag: TyDiagCollection = TyLowerDiag::ExpectedStarKind(span.into()).into();
            self.push_diag(diag);
            TyId::invalid(self.db, InvalidCause::Other)
        }
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the sort is `General`.
    fn fresh_ty(&mut self) -> TyId<'db> {
        self.table.new_var(TyVarSort::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId<'db>> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn capability_fallback_candidates(&self, ty: TyId<'db>) -> Vec<TyId<'db>> {
        let mut candidates = vec![ty];
        if let TyData::TyVar(var) = ty.base_ty(self.db).data(self.db)
            && matches!(var.sort, TyVarSort::String { .. })
            && let Some(text_ty) =
                resolve_lib_type_path(self.db, self.env.scope(), "core::abi::DynString")
        {
            candidates.push(text_ty);
            candidates.push(TyId::view_of(self.db, text_ty));
        }
        if let Some((cap, inner)) = ty.as_capability(self.db) {
            if matches!(cap, CapabilityKind::Mut) {
                candidates.push(TyId::borrow_ref_of(self.db, inner));
            }
            if !matches!(cap, CapabilityKind::View) {
                candidates.push(TyId::view_of(self.db, inner));
            }
            candidates.push(inner);
        } else {
            candidates.push(TyId::view_of(self.db, ty));
        }
        candidates.dedup();
        candidates
    }

    fn pattern_binds_any(&self, pat: PatId) -> bool {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return false;
        };
        match pat_data {
            Pat::WildCard | Pat::Rest | Pat::Lit(_) => false,
            Pat::Path(..) => self
                .env
                .pat_binding(pat)
                .is_some_and(|binding| matches!(binding, LocalBinding::Local { .. })),
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                pats.iter().any(|pat| self.pattern_binds_any(*pat))
            }
            Pat::Record(_, fields) => fields.iter().any(|field| self.pattern_binds_any(field.pat)),
            Pat::Or(lhs, rhs) => self.pattern_binds_any(*lhs) || self.pattern_binds_any(*rhs),
        }
    }

    fn pattern_value_capture_access(&mut self, pat: PatId) -> ClosureCaptureAccess {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return ClosureCaptureAccess::Read;
        };
        match pat_data {
            Pat::WildCard | Pat::Rest | Pat::Lit(_) => ClosureCaptureAccess::Read,
            Pat::Path(..) => {
                let Some(binding) = self.env.pat_binding(pat) else {
                    return ClosureCaptureAccess::Read;
                };
                let ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
                if ty.has_invalid(self.db) || self.ty_is_copy(ty) {
                    ClosureCaptureAccess::Read
                } else if ty.has_var(self.db) {
                    ClosureCaptureAccess::MoveIfNonCopy
                } else {
                    ClosureCaptureAccess::Move
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                let mut access = ClosureCaptureAccess::Read;
                for pat in pats {
                    access.include(self.pattern_value_capture_access(*pat));
                }
                access
            }
            Pat::Record(_, fields) => {
                let mut access = ClosureCaptureAccess::Read;
                for field in fields {
                    access.include(self.pattern_value_capture_access(field.pat));
                }
                access
            }
            Pat::Or(lhs, rhs) => {
                let mut access = self.pattern_value_capture_access(*lhs);
                access.include(self.pattern_value_capture_access(*rhs));
                access
            }
        }
    }

    fn record_pattern_value_use(&mut self, expr: ExprId, capture_access: ClosureCaptureAccess) {
        self.record_expr_value_use(expr, ValueAccess::Read, capture_access);
    }

    fn destructure_source_mode(&self, ty: TyId<'db>) -> (TyId<'db>, PatternDestructureMode) {
        destructure_pattern_source(self.db, ty)
    }

    fn retype_pattern_bindings_for_borrow(&mut self, pat: PatId, kind: BorrowKind) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match pat_data {
            Pat::Path(..) => {
                let Some(binding) = self.env.pat_binding(pat) else {
                    return;
                };
                if !matches!(binding, LocalBinding::Local { .. }) {
                    return;
                }
                self.env.set_pat_binding_mode(pat, PatBindingMode::ByBorrow);
                let inner = self.env.lookup_binding_ty(&binding);
                if inner.has_invalid(self.db) || inner.as_capability(self.db).is_some() {
                    return;
                }
                let borrow_ty =
                    apply_pattern_borrow_mode(self.db, PatternDestructureMode::Borrow(kind), inner);
                self.env.type_pat(pat, borrow_ty);
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for pat in pats {
                    self.retype_pattern_bindings_for_borrow(*pat, kind);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.retype_pattern_bindings_for_borrow(field.pat, kind);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.retype_pattern_bindings_for_borrow(*lhs, kind);
                self.retype_pattern_bindings_for_borrow(*rhs, kind);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    /// Ensure all binding patterns are registered in the current scope.
    fn seed_pat_bindings(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.env.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(path, is_mut) => {
                let Partial::Present(path) = path else {
                    return;
                };
                let Some(LocalBinding::Local { .. }) = self.env.pat_binding(pat) else {
                    return;
                };
                if let Some(ident) = path.as_ident(self.db) {
                    let current = self.env.current_block_idx();
                    if self.env.get_block(current).lookup_var(ident).is_none() {
                        let binding = LocalBinding::local(pat, *is_mut);
                        self.env.register_pending_binding(ident, binding);
                    }
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &p in pats {
                    self.seed_pat_bindings(p);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.seed_pat_bindings(field.pat);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.seed_pat_bindings(*lhs);
                self.seed_pat_bindings(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId<'db>, expected: TyId<'db>) -> TyId<'db>
    where
        T: Into<Typeable<'db>>,
    {
        let t = t.into();
        let span = t.clone().span(self.env.body());
        let actual = self.equate_ty(actual, expected, span);

        self.retype_expr_or_pat(t, actual);
        actual
    }

    fn retype_expr_or_pat(&mut self, typeable: Typeable<'db>, ty: TyId<'db>) {
        match typeable {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = ty;
                if typed_expr.borrow_provider.is_none() {
                    typed_expr.borrow_provider =
                        self.concrete_borrow_provider_for_effect_handle_ty(ty);
                }
                typed_expr.path_read_semantics = typed_expr
                    .binding
                    .map(|binding| self.binding_path_read_semantics(binding, ty));
                self.env.type_expr(expr, typed_expr);
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, ty),
        }
    }

    fn equate_ty(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> TyId<'db> {
        // FIXME: This is a temporary workaround, this should be removed when we
        // implement subtyping.
        if expected.is_never(self.db) && !actual.is_never(self.db) {
            if !actual.has_invalid(self.db) && !expected.has_invalid(self.db) {
                let diag = BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                };
                self.push_diag(diag);
            }
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        // Resolve associated types before unification
        let actual = actual.fold_with(self.db, &mut self.table);
        let expected = expected.fold_with(self.db, &mut self.table);
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        match self.table.unify(actual, expected) {
            Ok(()) => {
                // FIXME: This is a temporary workaround, this should be removed when we
                // implement subtyping.
                let actual = actual.fold_with(self.db, &mut self.table);
                if actual.is_never(self.db) {
                    expected
                } else {
                    let expected = expected.fold_with(self.db, &mut self.table);
                    merge_equated_layout_holes(self.db, actual, expected)
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                if !actual.has_invalid(self.db) && !expected.has_invalid(self.db) {
                    self.push_diag(BodyDiag::TypeMismatch {
                        span,
                        expected,
                        given: actual,
                    });
                }
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                self.push_diag(BodyDiag::InfiniteOccurrence(span));

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }

    fn resolve_path(
        &mut self,
        path: PathId<'db>,
        resolve_tail_as_value: bool,
        span: LazyPathSpan<'db>,
        minter: &HoleMinter<'db>,
    ) -> Result<PathRes<'db>, PathResError<'db>> {
        let scope = self.env.scope();
        let mut invisible = None;
        let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let res = match resolve_path_with_observer_and_minter(
            self.db,
            path,
            scope,
            self.env.assumptions(),
            resolve_tail_as_value,
            &mut check_visibility,
            minter,
        ) {
            Ok(r) => Ok(r.map_over_ty(|ty| self.instantiate_to_term(ty))),
            Err(err) => Err(err),
        };

        if let Some((path, deriv_span)) = invisible {
            let span = span.clone().segment(path.segment_index(self.db)).ident();
            let ident = path.ident(self.db);
            let diag = PathResDiag::Invisible(span.into(), ident.unwrap(), deriv_span);
            self.diags.push(diag.into());
        }

        res
    }

    fn instantiate_to_term(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let base = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Func(def)) = base.data(self.db) else {
            return self.table.instantiate_to_term(ty);
        };
        self.instantiate_callable_to_term(ty, *def)
    }

    fn instantiate_trait_method_to_term(
        &mut self,
        method: Func<'db>,
        receiver_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_method(self.db, method, &mut self.table, receiver_ty, inst);
        self.instantiate_callable_to_term(ty, method.as_callable(self.db).unwrap())
    }

    fn instantiate_trait_assoc_fn_to_term(
        &mut self,
        method: CallableDef<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_assoc_fn(self.db, method, inst);
        self.instantiate_callable_to_term(ty, method)
    }

    /// Brings a probed inherent-method candidate into this checker's
    /// inference context: extracts the bound candidate from the probe's
    /// solution, registers effect-provider inference keys for its args, and
    /// unifies the bound probe key against the receiver so the solution's
    /// bindings propagate into receiver inference variables (the inherent
    /// analog of the trait path unifying the instance's self type). The
    /// matching rule itself — key choice, normalization, binder application —
    /// lives in the method-table probe; nothing is re-derived here.
    fn extract_inherent_method_to_term(
        &mut self,
        canonical_receiver: &Canonicalized<'db, TyId<'db>>,
        cand: ProbedMethod<'db>,
        receiver_ty: TyId<'db>,
    ) -> TyId<'db> {
        let bound = canonical_receiver.extract_solution(&mut self.table, cand.bound);
        self.register_effect_provider_args(cand.def, bound.func_ty);
        let snapshot = self.table.snapshot();
        if self.table.unify(bound.key_ty, receiver_ty).is_err() {
            self.table.rollback_to(snapshot);
        }
        bound.func_ty
    }

    fn instantiate_callable_to_term(
        &mut self,
        mut ty: TyId<'db>,
        callable: CallableDef<'db>,
    ) -> TyId<'db> {
        if ty.has_invalid(self.db) {
            return ty;
        }

        while let Some(prop) = ty.applicable_ty(self.db) {
            let (_, args) = ty.decompose_ty_app(self.db);
            let param_index = args.len();
            let param_ty = callable.params(self.db).get(param_index).copied();
            let arg = self.table.new_var_for(prop);
            if let Some(param_ty) = param_ty
                && self.is_effect_provider_param(param_ty)
            {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, &arg));
            }
            ty = TyId::app(self.db, ty, arg);
        }

        ty
    }

    fn is_effect_provider_param(&self, param_ty: TyId<'db>) -> bool {
        matches!(param_ty.data(self.db), TyData::TyParam(p) if p.is_effect_provider())
            || matches!(
                param_ty.data(self.db),
                TyData::ConstTy(const_ty)
                    if matches!(
                        const_ty.data(self.db),
                        ConstTyData::TyParam(param, _) if param.is_implicit()
                    )
            )
    }

    /// Registers effect-provider inference keys for the generic args of an
    /// already-applied callable type (the applied counterpart of the
    /// bookkeeping `instantiate_callable_to_term` does while applying).
    fn register_effect_provider_args(&mut self, callable: CallableDef<'db>, ty: TyId<'db>) {
        let params = callable.params(self.db);
        for (param_index, arg) in ty.generic_args(self.db).iter().enumerate() {
            let Some(&param_ty) = params.get(param_index) else {
                break;
            };
            if self.is_effect_provider_param(param_ty) {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, arg));
            }
        }
    }

    /// Resolve associated type to concrete type if possible
    fn normalize_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(
            self.db,
            ty.fold_with(self.db, &mut self.table),
            self.env.scope(),
            self.env.assumptions(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum EffectArg<'db> {
    Place(Place<'db>),
    Value(ExprId),
    Binding(LocalBinding<'db>),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectPassMode {
    /// The provided effect is already a place; pass it directly.
    ByPlace,
    /// The provided effect is an rvalue; materialize it into a block-scoped temp place.
    ByTempPlace,
    ByValue,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ResolvedEffectArg<'db> {
    pub param_idx: usize,
    pub binding_idx: u32,
    pub key: PathId<'db>,
    pub arg: EffectArg<'db>,
    pub pass_mode: EffectPassMode,
    /// Lexical closure depth at which the selected provider's source binding
    /// was declared.
    pub provider_closure_depth: usize,
    pub provider_is_external_to_closure: bool,
    pub required_mut: bool,
    pub key_kind: EffectKeyKind,
    pub instantiated_key_ty: Option<TyId<'db>>,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<ProviderAddressSpace>,
}

/// Resolved reference for a `const`-valued path expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstRef<'db> {
    Const(Const<'db>),
    TraitConst(AssocConstUse<'db>),
    InherentConst(InherentConstUse<'db>),
}

impl<'db> TyVisitable<'db> for ConstRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            ConstRef::Const(_) => {}
            ConstRef::TraitConst(assoc) => assoc.visit_with(visitor),
            ConstRef::InherentConst(use_) => use_.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ConstRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            ConstRef::Const(const_def) => ConstRef::Const(const_def),
            ConstRef::TraitConst(assoc) => ConstRef::TraitConst(assoc.fold_with(db, folder)),
            ConstRef::InherentConst(use_) => ConstRef::InherentConst(use_.fold_with(db, folder)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
struct ExprPlaceId(u32);
entity_impl!(ExprPlaceId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody<'db> {
    body: Option<Body<'db>>,
    result_ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
    pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
    expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
    expr_normal_completion: SecondaryMap<ExprId, Option<bool>>,
    /// Boolean value shared by all normal completions of an expression.
    ///
    /// Escape paths may coexist with this fact; `None` means both values remain
    /// possible (or no finalized fact was recorded).
    expr_normal_bool_value: SecondaryMap<ExprId, Option<bool>>,
    /// Boolean value shared by all normal completions of a condition.
    cond_normal_bool_value: SecondaryMap<CondId, Option<bool>>,
    assignment_rebinds_capability: SecondaryMap<ExprId, Option<bool>>,
    contextual_view_sources: SecondaryMap<ExprId, Option<TyId<'db>>>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<RecordInitLowering<'db>>>,
    closure_infos: SecondaryMap<ExprId, Option<ClosureInfo<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
    call_effect_args: SecondaryMap<ExprId, Option<Vec<ResolvedEffectArg<'db>>>>,
    return_borrow_provider: Option<ProviderAddressSpace>,
    /// Bindings for function parameters (indexed by param position)
    param_bindings: Vec<LocalBinding<'db>>,
    /// Bindings for local variables (keyed by the pattern that introduces them)
    pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
    pattern_store: PatternStore<'db>,
    pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,
    /// Resolved Seq trait methods for for-loops
    for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
    expr_place: SecondaryMap<ExprId, PackedOption<ExprPlaceId>>,
    expr_places: PrimaryMap<ExprPlaceId, Place<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingSource {
    pub init_expr: ExprId,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnIndexSource {
    pub origin: CallableInputLayoutHoleOrigin,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReturnProjectionStep {
    Field(u16),
    VariantField { variant: u16, field: u16 },
    ConstantIndex(usize),
    DynamicIndex(ReturnIndexSource),
    AnyIndex,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnSource {
    pub result_projection: Vec<ReturnProjectionStep>,
    pub origin: CallableInputLayoutHoleOrigin,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnProvenance {
    Fresh,
    Forwarded(Vec<ReturnSource>),
    Unknown,
}

struct ReturnProvenanceCx<'db> {
    owner: BodyOwner<'db>,
    seen_funcs: FxHashSet<Func<'db>>,
    // Exact provenance must fail when any return path cannot be replayed from
    // callable inputs. The separate source query is a conservative may-analysis
    // used to retain input-derived borrow sources from those mixed paths.
    allow_partial_sources: bool,
    /// Whether partial collection has skipped any return-value alternative or
    /// aggregate component that could not be represented by an input source.
    ///
    /// The collected sources remain a useful MAY set, but consumers may only
    /// treat it as exhaustive while this remains true.
    return_sources_complete: bool,
    visited_mutable_bindings: FxHashSet<LocalBinding<'db>>,
}

struct ReturnProjectionCall<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    args: &'a [ExprId],
    args_are_packed: bool,
}

impl<'db> ReturnProvenanceCx<'db> {
    fn new(owner: BodyOwner<'db>) -> Self {
        Self {
            owner,
            seen_funcs: FxHashSet::default(),
            allow_partial_sources: false,
            return_sources_complete: true,
            visited_mutable_bindings: FxHashSet::default(),
        }
    }

    fn for_forwarded_sources(owner: BodyOwner<'db>) -> Self {
        Self {
            owner,
            seen_funcs: FxHashSet::default(),
            allow_partial_sources: true,
            return_sources_complete: true,
            visited_mutable_bindings: FxHashSet::default(),
        }
    }

    fn note_unrepresented_return_source(&mut self) {
        if self.allow_partial_sources {
            self.return_sources_complete = false;
        }
    }
}

#[salsa::tracked(
    return_ref,
    cycle_fn=func_return_provenance_cycle_recover,
    cycle_initial=func_return_provenance_cycle_initial
)]
fn func_return_provenance<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> ReturnProvenance {
    let (diags, typed_body) = check_func_body(db, func);
    if !diags.is_empty() {
        return ReturnProvenance::Unknown;
    }
    typed_body.return_provenance_for_func(db, func)
}

fn func_return_provenance_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _func: Func<'db>,
) -> ReturnProvenance {
    ReturnProvenance::Forwarded(Vec::new())
}

fn func_return_provenance_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &ReturnProvenance,
    _count: u32,
    _func: Func<'db>,
) -> salsa::CycleRecoveryAction<ReturnProvenance> {
    salsa::CycleRecoveryAction::Iterate
}

fn call_like_expr_args(expr: &Expr<'_>, callee_is_receiver: bool) -> Option<Vec<ExprId>> {
    match expr {
        Expr::Call(callee, args) => {
            let mut call_args = Vec::with_capacity(args.len() + usize::from(callee_is_receiver));
            if callee_is_receiver {
                call_args.push(*callee);
            }
            call_args.extend(args.iter().map(|arg| arg.expr));
            Some(call_args)
        }
        Expr::MethodCall(receiver, _, _, args) => {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(*receiver);
            call_args.extend(args.iter().map(|arg| arg.expr));
            Some(call_args)
        }
        Expr::Un(receiver, _) => Some(vec![*receiver]),
        Expr::Bin(receiver, arg, _) | Expr::AugAssign(receiver, arg, _) => {
            Some(vec![*receiver, *arg])
        }
        _ => None,
    }
}

fn resolved_callable_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &TypedBody<'db>,
    body: Body<'db>,
    callable: &Callable<'db>,
) -> Option<(Func<'db>, Vec<TyId<'db>>)> {
    let CallableDef::Func(mut func) = callable.callable_def() else {
        return None;
    };
    let mut generic_args = callable.generic_args().to_vec();
    if let Some(inst) = callable.trait_inst()
        && let Some(name) = func.name(db).to_opt()
        && let Some((impl_func, impl_args)) = resolve_trait_method_instance(
            db,
            TraitSolveCx::new(db, body.scope()).with_assumptions(typed.assumptions()),
            inst,
            name,
        )
    {
        let trait_arg_len = inst.args(db).len();
        let mut resolved_args = impl_args;
        resolved_args.extend_from_slice(
            generic_args
                .get(trait_arg_len..)
                .unwrap_or(generic_args.as_slice()),
        );
        func = impl_func;
        generic_args = resolved_args;
    }
    Some((func, generic_args))
}

type LayoutBackingEquivalenceKey = (
    usize,
    Option<(CallableInputLayoutHoleOrigin, Vec<LayoutBundlePathStep>)>,
);

fn layout_backing_equivalence_key(
    param_idx: usize,
    source: &CallableInputLayoutBackingSource,
) -> LayoutBackingEquivalenceKey {
    let family = source
        .projection
        .iter()
        .rposition(|step| matches!(step, LayoutBundlePathStep::Index))
        .map(|last_index| (source.origin, source.projection[..=last_index].to_vec()));
    (param_idx, family)
}

fn degenerate_return_projection(
    path: &[LayoutBundlePathStep],
    index_lengths: &[usize],
) -> Option<Vec<ReturnProjectionStep>> {
    let mut projection = Vec::new();
    let mut path_idx = 0;
    let mut length_idx = 0;
    while path_idx < path.len() {
        match path[path_idx] {
            LayoutBundlePathStep::Field(field) => {
                projection.push(ReturnProjectionStep::Field(field));
                path_idx += 1;
            }
            LayoutBundlePathStep::Variant(variant) => {
                let Some(LayoutBundlePathStep::Field(field)) = path.get(path_idx + 1) else {
                    return None;
                };
                projection.push(ReturnProjectionStep::VariantField {
                    variant,
                    field: *field,
                });
                path_idx += 2;
            }
            LayoutBundlePathStep::Index => {
                if index_lengths.get(length_idx) != Some(&1) {
                    return None;
                }
                projection.push(ReturnProjectionStep::ConstantIndex(0));
                path_idx += 1;
                length_idx += 1;
            }
            LayoutBundlePathStep::ConstParam(_) => {
                path_idx += 1;
            }
        }
    }
    (length_idx == index_lengths.len()).then_some(projection)
}

fn layout_param_indices_in_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Vec<usize> {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        indices: FxHashSet<usize>,
    }

    impl<'db> TyVisitor<'db> for Collector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_const_param(
            &mut self,
            param: &super::ty_def::TyParam<'db>,
            _const_ty_ty: TyId<'db>,
        ) {
            self.indices.insert(param.idx);
        }

        fn visit_closure(&mut self, closure: super::ty_def::ClosureTy<'db>) {
            closure.captures(self.db).visit_with(self);
        }
    }

    let mut collector = Collector {
        db,
        indices: FxHashSet::default(),
    };
    ty.visit_with(&mut collector);
    let mut indices = collector.indices.into_iter().collect::<Vec<_>>();
    indices.sort_unstable();
    indices
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub enum ValuePathRef<'db> {
    UnitVariant(ResolvedVariant<'db>),
    TypeConst(TyId<'db>),
    FunctionItem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum CodeRegionIntrinsicKind {
    Offset,
    Len,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstIntrinsicKind {
    SizeOf,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum SemanticExprLowering<'db> {
    Call {
        callable: Callable<'db>,
        callee_is_receiver: bool,
    },
    CodeRegionIntrinsic {
        callable: Callable<'db>,
        region_arg: ExprId,
        kind: CodeRegionIntrinsicKind,
    },
    ConstIntrinsic {
        callable: Callable<'db>,
        kind: ConstIntrinsicKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RecordInitLowering<'db> {
    Struct,
    EnumVariant(ResolvedVariant<'db>),
}

impl<'db> TyVisitable<'db> for ValuePathRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::UnitVariant(variant) => variant.ty.visit_with(visitor),
            Self::TypeConst(ty) => ty.visit_with(visitor),
            Self::FunctionItem => {}
        }
    }
}

impl<'db> TyFoldable<'db> for ValuePathRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::UnitVariant(variant) => Self::UnitVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
            // A type-level value path is a symbolic reference into the
            // template owner's generic parameter list. Keep that reference
            // formal: semantic const evaluation applies the instance's
            // substitution explicitly, while runtime lowering needs the
            // original parameter index to select an exact runtime ABI value.
            // Folding it here would retain only the actual value and erase
            // which formal const parameter the expression referenced.
            Self::TypeConst(ty) => Self::TypeConst(ty),
            Self::FunctionItem => Self::FunctionItem,
        }
    }
}

impl<'db> TyVisitable<'db> for SemanticExprLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Call { callable, .. }
            | Self::CodeRegionIntrinsic { callable, .. }
            | Self::ConstIntrinsic { callable, .. } => callable.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for SemanticExprLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Call {
                callable,
                callee_is_receiver,
            } => Self::Call {
                callable: callable.fold_with(db, folder),
                callee_is_receiver,
            },
            Self::CodeRegionIntrinsic {
                callable,
                region_arg,
                kind,
            } => Self::CodeRegionIntrinsic {
                callable: callable.fold_with(db, folder),
                region_arg,
                kind,
            },
            Self::ConstIntrinsic { callable, kind } => Self::ConstIntrinsic {
                callable: callable.fold_with(db, folder),
                kind,
            },
        }
    }
}

impl<'db> TyVisitable<'db> for RecordInitLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        if let Self::EnumVariant(variant) = self {
            variant.ty.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for RecordInitLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Struct => Self::Struct,
            Self::EnumVariant(variant) => Self::EnumVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
        }
    }
}

impl<'db> TyVisitable<'db> for TypedBody<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.result_ty.visit_with(visitor);
        for ty in self.pat_ty.values().flatten() {
            ty.visit_with(visitor);
        }
        for prop in self.expr_ty.values().flatten() {
            prop.visit_with(visitor);
        }
        for cref in self.const_refs.values().flatten() {
            cref.visit_with(visitor);
        }
        for value_path in self.value_path_refs.values().flatten() {
            value_path.visit_with(visitor);
        }
        for lowering in self.semantic_expr_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for lowering in self.record_init_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for info in self.closure_infos.values().flatten() {
            info.visit_with(visitor);
        }
        for place in self.expr_places.values() {
            place.visit_with(visitor);
        }
        self.pattern_store.visit_with(visitor);
        for seq in self.for_loop_seq.values().flatten() {
            seq.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TypedBody<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        let mut this = self;
        this.result_ty = this.result_ty.fold_with(db, folder);
        this.assumptions = this.assumptions.fold_with(db, folder);
        this.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(db, folder));
        this.expr_ty
            .values_mut()
            .flatten()
            .for_each(|prop| *prop = prop.clone().fold_with(db, folder));
        this.const_refs
            .values_mut()
            .flatten()
            .for_each(|cref| *cref = (*cref).fold_with(db, folder));
        this.value_path_refs
            .values_mut()
            .flatten()
            .for_each(|path| *path = (*path).fold_with(db, folder));
        this.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(db, folder));
        this.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(db, folder));
        this.closure_infos
            .values_mut()
            .flatten()
            .for_each(|info| *info = info.clone().fold_with(db, folder));
        for args in this.call_effect_args.values_mut().flatten() {
            for arg in args {
                *arg = arg.clone().fold_with(db, folder);
            }
        }
        this.param_bindings
            .iter_mut()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pat_bindings
            .values_mut()
            .flatten()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pattern_store = this.pattern_store.fold_with(db, folder);
        this.for_loop_seq
            .values_mut()
            .flatten()
            .for_each(|seq| *seq = seq.clone().fold_with(db, folder));
        this.expr_places
            .values_mut()
            .for_each(|place| *place = place.clone().fold_with(db, folder));
        this
    }
}

unsafe impl<'db> Update for TypedBody<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if *old_value == new_value {
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

impl<'db> TypedBody<'db> {
    pub fn body(&self) -> Option<Body<'db>> {
        self.body
    }

    pub fn result_ty(&self) -> TyId<'db> {
        self.result_ty
    }

    pub fn has_smir_lowering_blocker(&self, db: &'db dyn HirAnalysisDb) -> bool {
        let Some(body) = self.body else {
            return false;
        };

        body.exprs(db)
            .iter()
            .any(|(expr, expr_data)| self.expr_has_smir_lowering_blocker(db, expr, expr_data))
            || body
                .stmts(db)
                .iter()
                .any(|(stmt, stmt_data)| self.stmt_has_smir_lowering_blocker(stmt, stmt_data))
            || body
                .conds(db)
                .iter()
                .any(|(_, cond_data)| matches!(cond_data, Partial::Absent))
    }

    fn expr_has_smir_lowering_blocker(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
        expr_data: &Partial<Expr<'db>>,
    ) -> bool {
        let Partial::Present(expr_data) = expr_data else {
            return true;
        };
        let expr_ty = self.expr_ty(db, expr);

        match expr_data {
            Expr::Path(_) => {
                expr_ty.has_invalid(db)
                    && self.expr_binding(expr).is_none()
                    && self.expr_const_ref(expr).is_none()
                    && self.expr_code_region_ref(db, expr).is_none()
                    && self.value_path_ref(expr).is_none()
            }
            Expr::Call(..) | Expr::MethodCall(..) => self.semantic_expr_lowering(expr).is_none(),
            Expr::Assert(_) => expr_ty.has_invalid(db),
            Expr::RecordInit(..) => self.record_init_lowering(expr).is_none(),
            Expr::Un(inner, crate::hir_def::expr::UnOp::Mut | crate::hir_def::expr::UnOp::Ref) => {
                self.expr_place(*inner).is_none()
            }
            Expr::Assign(dst, _) => self.expr_place(*dst).is_none(),
            Expr::AugAssign(dst, _, _) => {
                self.semantic_expr_lowering(expr).is_none() && self.expr_place(*dst).is_none()
            }
            Expr::Match(_, Partial::Absent) => true,
            Expr::Match(_, Partial::Present(arms)) => arms
                .iter()
                .any(|arm| !self.pattern_status(arm.pat).is_ready()),
            _ => false,
        }
    }

    fn stmt_has_smir_lowering_blocker(&self, stmt: StmtId, stmt_data: &Partial<Stmt<'db>>) -> bool {
        match stmt_data {
            Partial::Present(Stmt::For(..)) => self.for_loop_seq(stmt).is_none(),
            Partial::Present(_) => false,
            Partial::Absent => true,
        }
    }

    pub fn assumptions(&self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.expr_ty
            .get(expr)
            .cloned()
            .flatten()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn expr_can_complete_normally(&self, expr: ExprId) -> bool {
        self.expr_normal_completion
            .get(expr)
            .copied()
            .flatten()
            .unwrap_or(true)
    }

    pub fn expr_normal_bool_value(&self, expr: ExprId) -> Option<bool> {
        self.expr_normal_bool_value.get(expr).copied().flatten()
    }

    pub fn cond_normal_bool_value(&self, cond: CondId) -> Option<bool> {
        self.cond_normal_bool_value.get(cond).copied().flatten()
    }

    pub fn assignment_rebinds_capability(&self, expr: ExprId) -> bool {
        self.assignment_rebinds_capability
            .get(expr)
            .copied()
            .flatten()
            .unwrap_or(false)
    }

    pub fn contextual_view_source(&self, expr: ExprId) -> Option<TyId<'db>> {
        self.contextual_view_sources[expr]
    }

    pub fn expr_value_access(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ValueAccess {
        self.expr_prop(db, expr).value_access
    }

    /// All const references registered in this body, in arbitrary order.
    pub fn const_refs(&self) -> impl Iterator<Item = ConstRef<'db>> + '_ {
        self.const_refs.values().flatten().copied()
    }

    pub fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.const_refs[expr]
    }

    pub fn value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.value_path_refs[expr]
    }

    pub fn expr_code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        self.expr_binding(expr)
            .and_then(|binding| manual_contract_root_ref_from_ty(db, self.binding_ty(db, binding)))
            .or_else(|| manual_contract_root_ref_from_ty(db, self.expr_ty(db, expr)))
    }

    pub fn semantic_expr_lowering(&self, expr: ExprId) -> Option<&SemanticExprLowering<'db>> {
        self.semantic_expr_lowering[expr].as_ref()
    }

    pub fn record_init_lowering(&self, expr: ExprId) -> Option<RecordInitLowering<'db>> {
        self.record_init_lowering[expr]
    }

    pub fn closure_info(&self, expr: ExprId) -> Option<&ClosureInfo<'db>> {
        self.closure_infos[expr].as_ref()
    }

    pub fn closure_infos(&self) -> impl Iterator<Item = (ExprId, &ClosureInfo<'db>)> + '_ {
        self.closure_infos
            .iter()
            .filter_map(|(expr, info)| info.as_ref().map(|info| (expr, info)))
    }

    pub fn resolved_field_index(&self, expr: ExprId) -> Option<u16> {
        self.resolved_field_index[expr]
    }

    // Final typed pattern/binding view. This can intentionally differ from
    // validated-pattern match types when destructuring borrowed carriers.
    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pat_ty
            .get(pat)
            .copied()
            .flatten()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::Call { callable, .. }
            | SemanticExprLowering::CodeRegionIntrinsic { callable, .. }
            | SemanticExprLowering::ConstIntrinsic { callable, .. } => Some(callable),
        }
    }

    pub fn code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::CodeRegionIntrinsic { region_arg, .. } => {
                self.expr_code_region_ref(db, *region_arg)
            }
            SemanticExprLowering::Call { .. } | SemanticExprLowering::ConstIntrinsic { .. } => None,
        }
    }

    pub fn call_effect_args(&self, call_expr: ExprId) -> Option<&[ResolvedEffectArg<'db>]> {
        self.call_effect_args[call_expr].as_deref()
    }

    pub fn return_borrow_provider(&self) -> Option<ProviderAddressSpace> {
        self.return_borrow_provider
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.param_bindings.get(idx).copied()
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings[pat]
    }

    /// Get how this local binding is captured by its source pattern destructuring.
    pub fn pat_binding_mode(&self, pat: PatId) -> Option<PatBindingMode> {
        self.pat_binding_modes[pat]
    }

    pub fn binding_ty(&self, db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self.pat_ty(db, pat),
            LocalBinding::Param { ty, .. } => ty,
            LocalBinding::EffectParam { site, idx, .. } => {
                crate::core::semantic::EffectEnvView::new(site)
                    .visible_effect_binding_ty(db, idx)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub fn path_expr_read_semantics(&self, expr: ExprId) -> Option<PathReadSemantics> {
        self.expr_ty[expr]
            .as_ref()
            .and_then(|prop| prop.path_read_semantics)
    }

    pub fn path_expr_reuses_local(&self, expr: ExprId) -> bool {
        matches!(
            self.path_expr_read_semantics(expr),
            Some(PathReadSemantics::ReuseLocal)
        )
    }

    pub fn pattern_store(&self) -> &PatternStore<'db> {
        &self.pattern_store
    }

    pub fn pattern_status(&self, pat: PatId) -> PatternAnalysisStatus {
        self.pattern_status[pat]
    }

    pub fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.pattern_status(pat).ready_root()
    }

    /// Get the resolved Seq methods for a for-loop statement.
    pub fn for_loop_seq(&self, stmt: StmtId) -> Option<&ForLoopSeq<'db>> {
        self.for_loop_seq[stmt].as_ref()
    }

    pub fn binding_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> Option<BindingSource> {
        let LocalBinding::Local { pat, .. } = binding else {
            return None;
        };
        let body = self.body()?;

        for (_, stmt) in body.stmts(db).iter() {
            if let Partial::Present(crate::hir_def::Stmt::Let(stmt_pat, _, Some(init_expr))) = stmt
                && let Some(projection) = self.binding_projection_in_pat(*stmt_pat, pat)
            {
                return Some(BindingSource {
                    init_expr: *init_expr,
                    projection,
                });
            }
        }

        None
    }

    fn binding_projection_in_pat(
        &self,
        pat: PatId,
        binding_pat: PatId,
    ) -> Option<Vec<ReturnProjectionStep>> {
        self.binding_projection_in_validated_pat(self.pattern_root(pat)?, binding_pat)
    }

    fn binding_projection_in_validated_pat(
        &self,
        pat: ValidatedPatId,
        binding_pat: PatId,
    ) -> Option<Vec<ReturnProjectionStep>> {
        match self.pattern_store.node(pat).kind() {
            ValidatedPatKind::Wildcard { binding } => binding
                .filter(|binding| binding.representative_pat == binding_pat)
                .map(|_| Vec::new()),
            ValidatedPatKind::Constructor { ctor, fields } => {
                fields.iter().enumerate().find_map(|(field_idx, child)| {
                    let suffix = self.binding_projection_in_validated_pat(*child, binding_pat)?;
                    let field = u16::try_from(field_idx).ok()?;
                    let step = match ctor {
                        ConstructorKind::Variant(variant, _) => {
                            ReturnProjectionStep::VariantField {
                                variant: variant.idx,
                                field,
                            }
                        }
                        ConstructorKind::Type(_) => ReturnProjectionStep::Field(field),
                        ConstructorKind::Literal(_, _) => return None,
                    };
                    let mut path = Vec::with_capacity(suffix.len() + 1);
                    path.push(step);
                    path.extend(suffix);
                    Some(path)
                })
            }
            ValidatedPatKind::Or(pats) => {
                let mut paths = pats
                    .iter()
                    .filter_map(|pat| self.binding_projection_in_validated_pat(*pat, binding_pat))
                    .collect::<Vec<_>>();
                let first = paths.pop()?;
                paths.into_iter().all(|path| path == first).then_some(first)
            }
        }
    }

    pub(super) fn callable_return_provenance(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> ReturnProvenance {
        let provenance = match owner {
            BodyOwner::Func(func) => func_return_provenance(db, func).clone(),
            BodyOwner::Closure { .. } => {
                let mut context = ReturnProvenanceCx::new(owner);
                self.collect_return_sources_for_owner(db, owner, &mut context)
                    .map_or(ReturnProvenance::Unknown, |(sources, saw_non_param)| {
                        if saw_non_param {
                            ReturnProvenance::Fresh
                        } else {
                            ReturnProvenance::Forwarded(sources)
                        }
                    })
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => ReturnProvenance::Unknown,
        };
        match provenance {
            ReturnProvenance::Forwarded(sources) if sources.is_empty() => ReturnProvenance::Fresh,
            provenance => provenance,
        }
    }

    pub(super) fn callable_forwarded_return_sources(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> Vec<ReturnSource> {
        self.callable_forwarded_return_sources_with_completeness(db, owner)
            .0
    }

    pub(super) fn callable_forwarded_return_sources_with_completeness(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> (Vec<ReturnSource>, bool) {
        let mut context = ReturnProvenanceCx::for_forwarded_sources(owner);
        let sources = match owner {
            BodyOwner::Func(func) => self.collect_return_sources_for_func(db, func, &mut context),
            BodyOwner::Closure { .. } => {
                self.collect_return_sources_for_owner(db, owner, &mut context)
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => None,
        };
        sources
            .map(|(sources, saw_non_param)| {
                (sources, !saw_non_param && context.return_sources_complete)
            })
            .unwrap_or_else(|| (Vec::new(), false))
    }

    fn return_provenance_for_func(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
    ) -> ReturnProvenance {
        let mut context = ReturnProvenanceCx::new(BodyOwner::Func(func));
        let Some((sources, saw_non_param)) =
            self.collect_return_sources_for_func(db, func, &mut context)
        else {
            return ReturnProvenance::Unknown;
        };
        if !saw_non_param {
            ReturnProvenance::Forwarded(sources)
        } else {
            ReturnProvenance::Fresh
        }
    }

    fn collect_return_sources_for_func(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        context: &mut ReturnProvenanceCx<'db>,
    ) -> Option<(Vec<ReturnSource>, bool)> {
        if !context.seen_funcs.insert(func) {
            return None;
        }
        let previous_owner = context.owner;
        context.owner = BodyOwner::Func(func);

        let (diags, typed_body) = check_func_body(db, func);
        if !diags.is_empty() {
            context.owner = previous_owner;
            context.seen_funcs.remove(&func);
            return None;
        }
        let sources =
            typed_body.collect_return_sources_for_owner(db, BodyOwner::Func(func), context);
        context.owner = previous_owner;
        context.seen_funcs.remove(&func);
        sources
    }

    fn collect_return_sources_for_owner(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        context: &mut ReturnProvenanceCx<'db>,
    ) -> Option<(Vec<ReturnSource>, bool)> {
        let body = self.body()?;
        let root_expr = owner.root_expr(db)?;
        let mut out = FxHashSet::default();
        let mut saw_non_param = false;
        self.collect_explicit_return_param_sources_in_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            context,
        );
        self.collect_implicit_return_param_sources_from_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            context,
        );
        let mut sources = out.into_iter().collect::<Vec<_>>();
        sources.sort_unstable();
        Some((sources, saw_non_param))
    }

    fn forwarded_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return None;
        };

        if let Some(SemanticExprLowering::Call {
            callable,
            callee_is_receiver,
        }) = self.semantic_expr_lowering(expr)
        {
            let args = call_like_expr_args(expr_data, *callee_is_receiver)?;
            if let CallableDef::VariantCtor(variant) = callable.callable_def() {
                let mut sources = Vec::new();
                for (field, arg) in args.into_iter().enumerate() {
                    let Some(mut field_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arg,
                        seen,
                        visited_locals,
                    ) else {
                        seen.note_unrepresented_return_source();
                        continue;
                    };
                    prefix_return_sources(
                        &mut field_sources,
                        &[ReturnProjectionStep::VariantField {
                            variant: variant.idx,
                            field: u16::try_from(field).ok()?,
                        }],
                    );
                    sources.extend(field_sources);
                }
                return (!sources.is_empty())
                    .then_some(sources)
                    .or_else(|| self.inferred_fresh_layout_return_sources(db, expr, seen));
            }
            return self.forwarded_return_param_sources_from_call(
                db,
                body,
                expr,
                &args,
                seen,
                visited_locals,
            );
        }

        if (!matches!(expr_data, Expr::Path(_))
            || matches!(
                self.path_expr_read_semantics(expr),
                Some(PathReadSemantics::ReuseLocal | PathReadSemantics::ForwardInterface)
            )
            || {
                let ty = self.expr_ty(db, expr);
                ty.has_param(db) || ty.has_var(db) || ty_contains_const_hole(db, ty)
            })
            && let Some(place) = self.expr_place(expr).cloned()
            && let Some(sources) =
                self.forwarded_return_sources_from_place(db, body, &place, seen, visited_locals)
        {
            return Some(sources);
        }

        let forwarded = match expr_data {
            Expr::Block(stmts) => {
                let tail = stmts.last()?;
                match tail.data(db, body) {
                    Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *tail_expr,
                            seen,
                            visited_locals,
                        ),
                    Partial::Present(crate::hir_def::Stmt::Return(Some(return_expr))) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *return_expr,
                            seen,
                            visited_locals,
                        ),
                    _ => None,
                }
            }
            Expr::If(_, then_expr, else_expr) => {
                let then_sources = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    seen,
                    visited_locals,
                );
                let else_sources = else_expr.and_then(|else_expr| {
                    self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        else_expr,
                        seen,
                        visited_locals,
                    )
                });
                if seen.allow_partial_sources {
                    if then_sources.is_none() || else_sources.is_none() {
                        seen.note_unrepresented_return_source();
                    }
                    merge_partial_forwarded_param_sets(then_sources, else_sources)
                } else {
                    merge_forwarded_param_sets(then_sources, else_sources)
                }
            }
            Expr::Match(_, arms) => {
                let Partial::Present(arms) = arms else {
                    return None;
                };
                let mut merged = FxHashSet::default();
                for arm in arms {
                    let arm_sources = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arm.body,
                        seen,
                        visited_locals,
                    );
                    if seen.allow_partial_sources {
                        if let Some(arm_sources) = arm_sources {
                            merged.extend(arm_sources);
                        } else {
                            seen.note_unrepresented_return_source();
                        }
                    } else {
                        merged.extend(arm_sources?);
                    }
                }
                let mut out = merged.into_iter().collect::<Vec<_>>();
                out.sort_unstable();
                if seen.allow_partial_sources {
                    (!out.is_empty()).then_some(out)
                } else {
                    Some(out)
                }
            }
            Expr::With(_, with_body) => self.forwarded_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                seen,
                visited_locals,
            ),
            Expr::Un(inner, crate::hir_def::UnOp::Mut | crate::hir_def::UnOp::Ref) => self
                .forwarded_return_param_sources_from_expr(db, body, *inner, seen, visited_locals),
            Expr::RecordInit(_, fields) => {
                let (record_like, variant) = match self.record_init_lowering(expr)? {
                    RecordInitLowering::Struct => (RecordLike::Type(self.expr_ty(db, expr)), None),
                    RecordInitLowering::EnumVariant(variant) => {
                        (RecordLike::from_variant(variant), Some(variant.variant.idx))
                    }
                };
                let mut sources = Vec::new();
                for field in fields {
                    let label = field.label_eagerly(db, body)?;
                    let field_idx = record_like.record_field_idx(db, label)?;
                    let field_idx = u16::try_from(field_idx).ok()?;
                    let Some(mut field_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        field.expr,
                        seen,
                        visited_locals,
                    ) else {
                        seen.note_unrepresented_return_source();
                        continue;
                    };
                    let projection =
                        variant.map_or(ReturnProjectionStep::Field(field_idx), |variant| {
                            ReturnProjectionStep::VariantField {
                                variant,
                                field: field_idx,
                            }
                        });
                    prefix_return_sources(&mut field_sources, &[projection]);
                    sources.extend(field_sources);
                }
                (!sources.is_empty()).then_some(sources)
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                let is_array = matches!(expr_data, Expr::Array(_));
                let mut sources = Vec::new();
                for (idx, item) in items.iter().copied().enumerate() {
                    let Some(mut item_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        item,
                        seen,
                        visited_locals,
                    ) else {
                        seen.note_unrepresented_return_source();
                        continue;
                    };
                    let projection = if is_array {
                        ReturnProjectionStep::ConstantIndex(idx)
                    } else {
                        ReturnProjectionStep::Field(u16::try_from(idx).ok()?)
                    };
                    prefix_return_sources(&mut item_sources, &[projection]);
                    sources.extend(item_sources);
                }
                (!sources.is_empty()).then_some(sources)
            }
            Expr::ArrayRep(value, _) => {
                let mut sources = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *value,
                    seen,
                    visited_locals,
                )?;
                prefix_return_sources(&mut sources, &[ReturnProjectionStep::AnyIndex]);
                Some(sources)
            }
            Expr::Closure { .. } => {
                let mut sources = Vec::new();
                for (field, capture) in self.closure_info(expr)?.captures.iter().enumerate() {
                    let Some(mut capture_sources) = self.forwarded_return_sources_from_place(
                        db,
                        body,
                        &Place::new(PlaceBase::Binding(capture.binding)),
                        seen,
                        visited_locals,
                    ) else {
                        seen.note_unrepresented_return_source();
                        continue;
                    };
                    prefix_return_sources(
                        &mut capture_sources,
                        &[ReturnProjectionStep::Field(u16::try_from(field).ok()?)],
                    );
                    sources.extend(capture_sources);
                }
                (!sources.is_empty()).then_some(sources)
            }
            Expr::Path(_) => None,
            Expr::Call(..) | Expr::MethodCall(..) => None,
            _ => None,
        };
        forwarded.or_else(|| self.inferred_fresh_layout_return_sources(db, expr, seen))
    }

    fn inferred_fresh_layout_return_sources(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
        context: &ReturnProvenanceCx<'db>,
    ) -> Option<Vec<ReturnSource>> {
        let result_ty = self.expr_ty(db, expr);
        let mut out = FxHashSet::default();
        let mut found_result_root = false;
        for param_idx in layout_param_indices_in_ty(db, result_ty) {
            let output_paths = layout_param_projection_paths_in_ty(db, result_ty, param_idx);
            if output_paths.is_empty() {
                continue;
            }
            found_result_root = true;
            let output_projections = output_paths
                .iter()
                .map(|(path, lengths)| degenerate_return_projection(path, lengths))
                .collect::<Option<Vec<_>>>()?;
            let mut input_groups = FxHashMap::default();
            for (source, lengths) in
                self.return_layout_backing_sources(db, context.owner, param_idx)?
            {
                let Some(projection) = degenerate_return_projection(&source.projection, &lengths)
                else {
                    continue;
                };
                input_groups
                    .entry(layout_backing_equivalence_key(param_idx, &source))
                    .or_insert((source.origin, projection));
            }
            if input_groups.len() != 1 {
                return None;
            }
            let (origin, projection) = input_groups
                .into_values()
                .next()
                .expect("one layout-source equivalence group must contain one source");
            for result_projection in output_projections {
                out.insert(ReturnSource {
                    result_projection,
                    origin,
                    projection: projection.clone(),
                });
            }
        }
        if !found_result_root || out.is_empty() {
            return None;
        }
        let mut out = out.into_iter().collect::<Vec<_>>();
        out.sort_unstable();
        Some(out)
    }

    fn return_layout_backing_sources(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        param_idx: usize,
    ) -> Option<Vec<(CallableInputLayoutBackingSource, Vec<usize>)>> {
        match owner {
            BodyOwner::Func(func) => callable_input_layout_backing_sources(db, func, param_idx)
                .into_iter()
                .map(|source| {
                    let lengths = callable_input_layout_backing_index_lengths(db, func, &source)?;
                    Some((source, lengths))
                })
                .collect(),
            BodyOwner::Closure { ty, def, .. } => {
                let info = self.closure_info(def.expr)?;
                if info.captures.len() != ty.captures(db).len() {
                    return None;
                }

                let mut sources = Vec::new();
                for (idx, capture_ty) in ty.captures(db).iter().enumerate() {
                    let field = u16::try_from(idx).ok()?;
                    for (mut projection, lengths) in
                        layout_param_projection_paths_in_ty(db, *capture_ty, param_idx)
                    {
                        projection.insert(0, LayoutBundlePathStep::Field(field));
                        sources.push((
                            CallableInputLayoutBackingSource {
                                origin: CallableInputLayoutHoleOrigin::Receiver,
                                projection,
                            },
                            lengths,
                        ));
                    }
                }
                for (idx, ty) in ty.params(db).iter().copied().enumerate() {
                    for (mut projection, lengths) in
                        layout_param_projection_paths_in_ty(db, ty, param_idx)
                    {
                        projection.insert(0, LayoutBundlePathStep::Field(u16::try_from(idx).ok()?));
                        sources.push((
                            CallableInputLayoutBackingSource {
                                origin: CallableInputLayoutHoleOrigin::ValueParam(1),
                                projection,
                            },
                            lengths,
                        ));
                    }
                }
                Some(sources)
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => None,
        }
    }

    fn forwarded_return_param_sources_from_call(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        call_args: &[ExprId],
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let callable = self.callable_expr(expr)?;
        let (func, _) = resolved_callable_instance(db, self, body, callable)?;
        let callee_sources = self.forwarded_return_param_sources_from_callable(db, func, seen)?;
        let call = ReturnProjectionCall {
            db,
            body,
            expr,
            args: call_args,
            args_are_packed: callable.call_trait_args_pack_ty(db, body.scope()).is_some(),
        };
        let mut merged = FxHashSet::default();
        for callee_source in callee_sources {
            let mapped = self.forwarded_return_sources_from_call_source(
                &call,
                &callee_source,
                seen,
                visited_locals,
            );
            if seen.allow_partial_sources {
                if let Some(mapped) = mapped {
                    merged.extend(mapped);
                } else {
                    seen.note_unrepresented_return_source();
                }
            } else {
                merged.extend(mapped?);
            }
        }
        let mut out = merged.into_iter().collect::<Vec<_>>();
        out.sort_unstable();
        if seen.allow_partial_sources {
            (!out.is_empty()).then_some(out)
        } else {
            Some(out)
        }
    }

    fn forwarded_return_sources_from_place(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        place: &Place<'db>,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let PlaceBase::Binding(binding) = place.base;
        if binding.is_mut() && !seen.allow_partial_sources {
            return None;
        }
        let sources = if let binding @ LocalBinding::Local { pat, .. } = binding {
            if !visited_locals.insert(pat) {
                return None;
            }
            let initial_sources = self.binding_source(db, binding).and_then(|binding_source| {
                let sources = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    binding_source.init_expr,
                    seen,
                    visited_locals,
                )?;
                project_return_sources(sources, &binding_source.projection)
            });
            if initial_sources.is_none() {
                seen.note_unrepresented_return_source();
            }
            let mut sources = initial_sources.unwrap_or_default();
            if binding.is_mut() {
                sources.extend(self.mutable_binding_assignment_sources(
                    db,
                    body,
                    binding,
                    seen,
                    visited_locals,
                ));
            }
            visited_locals.remove(&pat);
            if sources.is_empty() {
                return None;
            }
            sources
        } else {
            let mut sources = vec![self.return_source_for_binding(db, seen.owner, binding)?];
            if binding.is_mut() {
                sources.extend(self.mutable_binding_assignment_sources(
                    db,
                    body,
                    binding,
                    seen,
                    visited_locals,
                ));
            }
            sources
        };
        let mut projection = Vec::with_capacity(place.projections.len());
        for place_projection in &place.projections {
            projection.push(match place_projection {
                PlaceProjection::Field { index, .. } => ReturnProjectionStep::Field(*index),
                PlaceProjection::Index { index_expr, .. } => {
                    self.return_index_projection(db, body, *index_expr, seen, visited_locals)?
                }
            });
        }
        project_return_sources(sources, &projection)
    }

    fn mutable_binding_assignment_sources(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        binding: LocalBinding<'db>,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Vec<ReturnSource> {
        // This is deliberately path-insensitive: for may-provenance, every
        // syntactic assignment is a possible source. Exact provenance rejects
        // mutable bindings before reaching this helper.
        if !seen.visited_mutable_bindings.insert(binding) {
            return Vec::new();
        }
        let mut out = FxHashSet::default();
        for (_, expr) in body.exprs(db).iter() {
            let Partial::Present(Expr::Assign(lhs, rhs)) = expr else {
                continue;
            };
            let Some(place) = self.expr_place(*lhs) else {
                continue;
            };
            let PlaceBase::Binding(assigned) = place.base;
            if assigned != binding {
                continue;
            }
            let Some(mut sources) =
                self.forwarded_return_param_sources_from_expr(db, body, *rhs, seen, visited_locals)
            else {
                seen.note_unrepresented_return_source();
                continue;
            };
            let mut result_projection = Vec::with_capacity(place.projections.len());
            let mut valid_projection = true;
            for projection in &place.projections {
                let step = match projection {
                    PlaceProjection::Field { index, .. } => ReturnProjectionStep::Field(*index),
                    PlaceProjection::Index { index_expr, .. } => {
                        let Some(step) = self.return_index_projection(
                            db,
                            body,
                            *index_expr,
                            seen,
                            visited_locals,
                        ) else {
                            valid_projection = false;
                            break;
                        };
                        step
                    }
                };
                result_projection.push(step);
            }
            if !valid_projection {
                seen.note_unrepresented_return_source();
                continue;
            }
            prefix_return_sources(&mut sources, &result_projection);
            out.extend(sources);
        }
        seen.visited_mutable_bindings.remove(&binding);
        let mut out = out.into_iter().collect::<Vec<_>>();
        out.sort_unstable();
        out
    }

    fn return_source_for_binding(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        binding: LocalBinding<'db>,
    ) -> Option<ReturnSource> {
        let (origin, projection) = match owner {
            BodyOwner::Func(owner_func) => match binding {
                LocalBinding::Param {
                    site: ParamSite::Func(func),
                    idx,
                    ..
                } if func == owner_func => (
                    if func.is_method(db) && idx == 0 {
                        CallableInputLayoutHoleOrigin::Receiver
                    } else {
                        CallableInputLayoutHoleOrigin::ValueParam(idx)
                    },
                    Vec::new(),
                ),
                LocalBinding::Param {
                    site: ParamSite::EffectField(_),
                    idx,
                    ..
                }
                | LocalBinding::EffectParam { idx, .. } => {
                    (CallableInputLayoutHoleOrigin::Effect(idx), Vec::new())
                }
                LocalBinding::Local { .. } | LocalBinding::Param { .. } => return None,
            },
            BodyOwner::Closure { def, .. } => {
                if let Some(field) = self
                    .closure_info(def.expr)?
                    .captures
                    .iter()
                    .position(|capture| capture.binding == binding)
                    .and_then(|field| u16::try_from(field).ok())
                {
                    (
                        CallableInputLayoutHoleOrigin::Receiver,
                        vec![ReturnProjectionStep::Field(field)],
                    )
                } else {
                    match binding {
                        LocalBinding::Param {
                            site: ParamSite::Closure(site),
                            idx,
                            ..
                        } if site == def => (
                            CallableInputLayoutHoleOrigin::ValueParam(1),
                            vec![ReturnProjectionStep::Field(u16::try_from(idx).ok()?)],
                        ),
                        LocalBinding::Param {
                            site: ParamSite::ClosureEnv(site),
                            ..
                        } if site == def => (CallableInputLayoutHoleOrigin::Receiver, Vec::new()),
                        LocalBinding::Local { .. }
                        | LocalBinding::Param { .. }
                        | LocalBinding::EffectParam { .. } => return None,
                    }
                }
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => return None,
        };
        Some(ReturnSource {
            result_projection: Vec::new(),
            origin,
            projection,
        })
    }

    fn forwarded_return_sources_from_call_source(
        &self,
        call: &ReturnProjectionCall<'_, 'db>,
        callee_source: &ReturnSource,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        if call.args_are_packed
            && callee_source.origin == CallableInputLayoutHoleOrigin::ValueParam(1)
            && callee_source.projection.is_empty()
        {
            let mut merged = Vec::new();
            for (idx, expr) in call.args.iter().copied().skip(1).enumerate() {
                let sources = self.forwarded_return_param_sources_from_expr(
                    call.db,
                    call.body,
                    expr,
                    seen,
                    visited_locals,
                );
                let Some(mut sources) = sources else {
                    if seen.allow_partial_sources {
                        seen.note_unrepresented_return_source();
                        continue;
                    }
                    return None;
                };
                let mut result_projection = callee_source.result_projection.clone();
                result_projection.push(ReturnProjectionStep::Field(u16::try_from(idx).ok()?));
                prefix_return_sources(&mut sources, &result_projection);
                merged.extend(sources);
            }
            return (!merged.is_empty()).then_some(merged);
        }

        let (sources, projection) = match callee_source.origin {
            CallableInputLayoutHoleOrigin::Receiver => (
                self.forwarded_return_param_sources_from_expr(
                    call.db,
                    call.body,
                    *call.args.first()?,
                    seen,
                    visited_locals,
                ),
                callee_source.projection.as_slice(),
            ),
            CallableInputLayoutHoleOrigin::ValueParam(param_idx) if call.args_are_packed => {
                if param_idx != 1 {
                    return None;
                }
                let (ReturnProjectionStep::Field(field), projection) =
                    callee_source.projection.split_first()?
                else {
                    return None;
                };
                (
                    self.forwarded_return_param_sources_from_expr(
                        call.db,
                        call.body,
                        *call.args.get(usize::from(*field) + 1)?,
                        seen,
                        visited_locals,
                    ),
                    projection,
                )
            }
            CallableInputLayoutHoleOrigin::ValueParam(param_idx) => (
                self.forwarded_return_param_sources_from_expr(
                    call.db,
                    call.body,
                    *call.args.get(param_idx)?,
                    seen,
                    visited_locals,
                ),
                callee_source.projection.as_slice(),
            ),
            CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
                let effect_arg = self
                    .call_effect_args(call.expr)?
                    .iter()
                    .find(|arg| arg.binding_idx as usize == effect_idx)?;
                let sources = match &effect_arg.arg {
                    EffectArg::Place(place) => self.forwarded_return_sources_from_place(
                        call.db,
                        call.body,
                        place,
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Value(expr) => self.forwarded_return_param_sources_from_expr(
                        call.db,
                        call.body,
                        *expr,
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Binding(binding) => self.forwarded_return_sources_from_place(
                        call.db,
                        call.body,
                        &Place::new(PlaceBase::Binding(*binding)),
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Unknown => None,
                };
                (sources, callee_source.projection.as_slice())
            }
        };
        let sources = sources?;
        let projection =
            self.instantiate_return_projection(call, projection, seen, visited_locals)?;
        let mut sources = project_return_sources(sources, &projection)?;
        prefix_return_sources(&mut sources, &callee_source.result_projection);
        Some(sources)
    }

    fn return_index_projection(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<ReturnProjectionStep> {
        if let Partial::Present(Expr::Lit(LitKind::Int(value))) = expr.data(db, body) {
            return value
                .data(db)
                .to_usize()
                .map(ReturnProjectionStep::ConstantIndex)
                .or_else(|| {
                    seen.allow_partial_sources
                        .then_some(ReturnProjectionStep::AnyIndex)
                });
        }
        let Some(mut sources) =
            self.forwarded_return_param_sources_from_expr(db, body, expr, seen, visited_locals)
        else {
            return seen
                .allow_partial_sources
                .then_some(ReturnProjectionStep::AnyIndex);
        };
        let Some(source) = sources.pop() else {
            return seen
                .allow_partial_sources
                .then_some(ReturnProjectionStep::AnyIndex);
        };
        if !sources.is_empty()
            || !source.result_projection.is_empty()
            || !self.return_index_source_is_stable(db, seen.owner, &source)
        {
            return seen
                .allow_partial_sources
                .then_some(ReturnProjectionStep::AnyIndex);
        }
        Some(ReturnProjectionStep::DynamicIndex(ReturnIndexSource {
            origin: source.origin,
            projection: source.projection,
        }))
    }

    fn instantiate_return_projection(
        &self,
        call: &ReturnProjectionCall<'_, 'db>,
        projection: &[ReturnProjectionStep],
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnProjectionStep>> {
        projection
            .iter()
            .map(|step| match step {
                ReturnProjectionStep::DynamicIndex(source) => self
                    .instantiate_return_index_projection(call, source, seen, visited_locals)
                    .or_else(|| {
                        seen.allow_partial_sources
                            .then_some(ReturnProjectionStep::AnyIndex)
                    }),
                ReturnProjectionStep::AnyIndex => Some(ReturnProjectionStep::AnyIndex),
                step => Some(step.clone()),
            })
            .collect()
    }

    fn instantiate_return_index_projection(
        &self,
        call: &ReturnProjectionCall<'_, 'db>,
        source: &ReturnIndexSource,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<ReturnProjectionStep> {
        if source.projection.is_empty() {
            let expr = match source.origin {
                CallableInputLayoutHoleOrigin::Receiver => *call.args.first()?,
                CallableInputLayoutHoleOrigin::ValueParam(param_idx) => {
                    *call.args.get(param_idx)?
                }
                CallableInputLayoutHoleOrigin::Effect(_) => {
                    return self.instantiate_projected_return_index_source(
                        call,
                        source,
                        seen,
                        visited_locals,
                    );
                }
            };
            return self.return_index_projection(call.db, call.body, expr, seen, visited_locals);
        }
        self.instantiate_projected_return_index_source(call, source, seen, visited_locals)
    }

    fn instantiate_projected_return_index_source(
        &self,
        call: &ReturnProjectionCall<'_, 'db>,
        source: &ReturnIndexSource,
        seen: &mut ReturnProvenanceCx<'db>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<ReturnProjectionStep> {
        let index_source = ReturnSource {
            result_projection: Vec::new(),
            origin: source.origin,
            projection: source.projection.clone(),
        };
        let mut sources = self.forwarded_return_sources_from_call_source(
            call,
            &index_source,
            seen,
            visited_locals,
        )?;
        let source = sources.pop()?;
        if !sources.is_empty()
            || !source.result_projection.is_empty()
            || !self.return_index_source_is_stable(call.db, seen.owner, &source)
        {
            return None;
        }
        Some(ReturnProjectionStep::DynamicIndex(ReturnIndexSource {
            origin: source.origin,
            projection: source.projection,
        }))
    }

    fn return_index_source_is_stable(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        source: &ReturnSource,
    ) -> bool {
        if source.projection.iter().any(|step| {
            !matches!(
                step,
                ReturnProjectionStep::Field(_) | ReturnProjectionStep::ConstantIndex(_)
            )
        }) {
            return false;
        }

        let (binding, ty, projection) = match (owner, source.origin) {
            (_, CallableInputLayoutHoleOrigin::Effect(_)) => return false,
            (BodyOwner::Func(_), origin) => {
                let binding = self
                    .param_bindings
                    .iter()
                    .copied()
                    .find(|binding| binding.callable_input_origin(db) == Some(origin));
                let Some(binding) = binding else {
                    return false;
                };
                (
                    binding,
                    self.binding_ty(db, binding),
                    source.projection.as_slice(),
                )
            }
            (BodyOwner::Closure { ty, def, .. }, CallableInputLayoutHoleOrigin::Receiver) => {
                let Some((ReturnProjectionStep::Field(field), projection)) =
                    source.projection.split_first()
                else {
                    return false;
                };
                let Some((capture, capture_ty)) = self.closure_info(def.expr).and_then(|info| {
                    info.captures
                        .get(usize::from(*field))
                        .zip(ty.captures(db).get(usize::from(*field)))
                }) else {
                    return false;
                };
                (capture.binding, *capture_ty, projection)
            }
            (
                BodyOwner::Closure { ty, def, .. },
                CallableInputLayoutHoleOrigin::ValueParam(param_idx),
            ) => {
                if param_idx != 1 {
                    return false;
                }
                let Some((ReturnProjectionStep::Field(field), projection)) =
                    source.projection.split_first()
                else {
                    return false;
                };
                let idx = usize::from(*field);
                let Some((binding, param_ty)) = self.closure_info(def.expr).and_then(|info| {
                    info.params
                        .get(idx)
                        .copied()
                        .zip(ty.params(db).get(idx).copied())
                }) else {
                    return false;
                };
                (binding, param_ty, projection)
            }
            (
                BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. },
                _,
            ) => return false,
        };
        !binding.is_mut() && return_index_projection_is_immutable(db, ty, projection)
    }
}

fn return_index_projection_is_immutable<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    projection: &[ReturnProjectionStep],
) -> bool {
    for step in projection {
        if matches!(ty.as_capability(db), Some((CapabilityKind::Mut, _))) {
            return false;
        }
        ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
        ty = match step {
            ReturnProjectionStep::Field(field) => {
                let Some(field_ty) = ty.field_types(db).get(usize::from(*field)).copied() else {
                    return false;
                };
                field_ty
            }
            ReturnProjectionStep::ConstantIndex(_) => {
                let Some(element_ty) = ty.generic_args(db).first() else {
                    return false;
                };
                *element_ty
            }
            ReturnProjectionStep::VariantField { .. }
            | ReturnProjectionStep::DynamicIndex(_)
            | ReturnProjectionStep::AnyIndex => return false,
        };
    }
    !matches!(ty.as_capability(db), Some((CapabilityKind::Mut, _)))
}

impl<'db> TypedBody<'db> {
    fn forwarded_return_param_sources_from_callable(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        seen: &mut ReturnProvenanceCx<'db>,
    ) -> Option<Vec<ReturnSource>> {
        if seen.allow_partial_sources {
            return self
                .collect_return_sources_for_func(db, func, seen)
                .map(|(sources, _)| sources)
                .filter(|sources| !sources.is_empty());
        }
        match func_return_provenance(db, func) {
            ReturnProvenance::Forwarded(sources) => Some(sources.clone()),
            ReturnProvenance::Fresh | ReturnProvenance::Unknown => None,
        }
    }

    fn collect_explicit_return_param_sources_in_stmt(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        stmt: StmtId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut ReturnProvenanceCx<'db>,
    ) {
        let Partial::Present(stmt_data) = stmt.data(db, body) else {
            return;
        };

        match stmt_data {
            crate::hir_def::Stmt::Let(_, _, Some(init)) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *init,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::For(_, iter, loop_body, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *iter,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::While(cond, loop_body) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::Return(Some(expr)) => {
                let mut visited_locals = FxHashSet::default();
                if let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *expr,
                    seen,
                    &mut visited_locals,
                ) {
                    out.extend(indices);
                } else {
                    *saw_non_param = true;
                }
            }
            crate::hir_def::Stmt::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Stmt::Let(_, _, None)
            | crate::hir_def::Stmt::Return(None)
            | crate::hir_def::Stmt::Continue
            | crate::hir_def::Stmt::Break => {}
        }
    }

    fn collect_explicit_return_param_sources_in_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut ReturnProvenanceCx<'db>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                for stmt in stmts {
                    self.collect_explicit_return_param_sources_in_stmt(
                        db,
                        body,
                        *stmt,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Un(inner, _) | Expr::Cast(inner, _) | Expr::Field(inner, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *inner,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Call(callee, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *callee,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Assert(args) => {
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *receiver,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::RecordInit(_, fields) => {
                for field in fields {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        field.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for item in items {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *item,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::ArrayRep(value, _) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *value,
                out,
                saw_non_param,
                seen,
            ),
            Expr::If(cond, then_expr, else_expr) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *scrutinee,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_explicit_return_param_sources_in_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(bindings, with_body) => {
                for binding in bindings {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        binding.value,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *with_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            // Returns inside a nested closure belong to that closure. The
            // enclosing callable's implicit-return walk classifies the
            // closure expression itself if it is actually returned.
            Expr::Closure { .. } => {}
            Expr::Lit(_) | Expr::Path(_) => {}
        }
    }

    fn collect_implicit_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut ReturnProvenanceCx<'db>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                if let Some(last_stmt) = stmts.last()
                    && let Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) =
                        last_stmt.data(db, body)
                {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *tail_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::If(_, then_expr, else_expr) => {
                self.collect_implicit_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(_, arms) => {
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_implicit_return_param_sources_from_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(_, with_body) => self.collect_implicit_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                out,
                saw_non_param,
                seen,
            ),
            _ => {
                let mut visited_locals = FxHashSet::default();
                let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    expr,
                    seen,
                    &mut visited_locals,
                ) else {
                    *saw_non_param = true;
                    return;
                };
                out.extend(indices);
            }
        }
    }

    fn collect_explicit_return_param_sources_in_cond(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        cond: crate::hir_def::CondId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut ReturnProvenanceCx<'db>,
    ) {
        let Partial::Present(cond_data) = cond.data(db, body) else {
            return;
        };

        match cond_data {
            crate::hir_def::Cond::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Cond::Let(_, value) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *value,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Cond::Bin(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
        }
    }

    /// Get the definition span for an expression that references a local binding.
    ///
    /// Returns `Some(span)` if the expression references a local variable, parameter,
    /// or effect parameter. Returns `None` if the expression doesn't have a binding
    /// or if no body is available.
    ///
    /// This is used by the language server for goto-definition on local variables.
    pub fn expr_binding_def_span(&self, func: Func<'db>, expr: ExprId) -> Option<DynLazySpan<'db>> {
        let body = self.body?;
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_with(body, func))
    }

    /// Like `expr_binding_def_span` but takes a `Body` directly.
    ///
    /// Use this when the body may not belong to a function (e.g., contract bodies).
    pub fn expr_binding_def_span_in_body(
        &self,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<DynLazySpan<'db>> {
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_in_body(body))
    }

    /// Get the binding kind for an expression that references a local binding.
    ///
    /// Returns the identity of the binding (param index, pattern id, or effect param ident).
    pub fn expr_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.expr_ty[expr].as_ref().and_then(|prop| prop.binding)
    }

    /// Returns a place representation for `expr` if it denotes an assignable location.
    pub fn expr_place(&self, expr: ExprId) -> Option<&Place<'db>> {
        self.expr_place[expr]
            .expand()
            .and_then(|place_id| self.expr_places.get(place_id))
    }

    /// Find all expressions that reference the same local binding as the given expression.
    ///
    /// Returns a list of ExprIds that share the same local binding (variable, parameter,
    /// or effect parameter). Returns an empty list if the expression doesn't have a binding.
    ///
    /// This is used by the language server for find-all-references and rename on local variables.
    pub fn local_references(&self, expr: ExprId) -> Vec<ExprId> {
        let Some(binding) = self.expr_ty[expr].as_ref().and_then(|prop| prop.binding) else {
            return vec![];
        };

        self.expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find all expressions that reference a binding by its kind.
    ///
    /// This is the general method for finding all references to any kind of binding
    /// (param, local, or effect param).
    pub fn references_by_binding(&self, binding: LocalBinding<'db>) -> Vec<ExprId> {
        self.expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub(crate) fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            body: None,
            result_ty: TyId::unit(db),
            assumptions: PredicateListId::empty_list(db),
            pat_ty: SecondaryMap::new(),
            expr_ty: SecondaryMap::new(),
            expr_normal_completion: SecondaryMap::new(),
            expr_normal_bool_value: SecondaryMap::new(),
            cond_normal_bool_value: SecondaryMap::new(),
            assignment_rebinds_capability: SecondaryMap::new(),
            contextual_view_sources: SecondaryMap::new(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            closure_infos: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            call_effect_args: SecondaryMap::new(),
            return_borrow_provider: None,
            param_bindings: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            for_loop_seq: SecondaryMap::new(),
            expr_place: SecondaryMap::new(),
            expr_places: PrimaryMap::new(),
        }
    }
}

pub(super) fn manual_contract_root_ref_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<SemanticCodeRegionRef<'db>> {
    let ty = strip_code_region_token_wrapper(db, ty);
    let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = ty.base_ty(db).data(db) else {
        return None;
    };
    match func.manual_contract_root_attr(db)? {
        ManualContractRootAttr::Init { .. } | ManualContractRootAttr::Runtime { .. } => {
            Some(SemanticCodeRegionRef::ManualContractRoot { func: *func })
        }
        ManualContractRootAttr::Error(_) => None,
    }
}

/// Returns true if the trait-const goal's args contain generic params that
/// are not in scope at `scope`. Such params come from the selected impl's or
/// trait's own binder and are existentials awaiting inference (e.g.
/// `GenericForwardStruct<7>::B` selecting through
/// `impl<const N> GenericForward<N> for GenericForwardStruct<N>`); checking
/// the goal with them treated as rigid universals produces false
/// "unsatisfied bound" errors.
pub(super) fn trait_const_goal_has_foreign_params<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
) -> bool {
    use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_ty};

    fn is_enclosing_item<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        owner: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> bool {
        let owner_item = owner.item();
        let mut item = Some(scope.item());
        while let Some(current) = item {
            if current == owner_item {
                return true;
            }
            item = current.scope().parent_item(db);
        }
        false
    }

    struct ForeignParamFinder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for ForeignParamFinder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            let param = match ty.data(self.db) {
                TyData::TyParam(param) => Some(param),
                TyData::ConstTy(const_ty) => match const_ty.data(self.db) {
                    crate::analysis::ty::const_ty::ConstTyData::TyParam(param, _) => Some(param),
                    _ => None,
                },
                _ => None,
            };
            if let Some(param) = param
                && !is_enclosing_item(self.db, self.scope, param.owner)
            {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = ForeignParamFinder {
        db,
        scope,
        found: false,
    };
    for &arg in inst.args(db) {
        arg.visit_with(&mut finder);
        if finder.found {
            return true;
        }
    }
    false
}

pub(super) fn ty_may_be_code_region_token<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let ty = strip_code_region_token_wrapper(db, ty);
    manual_contract_root_ref_from_ty(db, ty).is_some()
        || matches!(
            ty.base_ty(db).data(db),
            TyData::TyParam(_) | TyData::TyVar(_)
        )
}

fn strip_code_region_token_wrapper<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
) -> TyId<'db> {
    while let Some((_, inner)) = ty.as_capability(db) {
        ty = inner;
    }
    ty
}

fn merge_forwarded_param_sets(
    lhs: Option<Vec<ReturnSource>>,
    rhs: Option<Vec<ReturnSource>>,
) -> Option<Vec<ReturnSource>> {
    let mut merged = FxHashSet::default();
    for source in lhs?.into_iter().chain(rhs?) {
        merged.insert(source);
    }
    let mut out = merged.into_iter().collect::<Vec<_>>();
    out.sort_unstable();
    Some(out)
}

fn merge_partial_forwarded_param_sets(
    lhs: Option<Vec<ReturnSource>>,
    rhs: Option<Vec<ReturnSource>>,
) -> Option<Vec<ReturnSource>> {
    let mut merged = FxHashSet::default();
    merged.extend(lhs.into_iter().flatten());
    merged.extend(rhs.into_iter().flatten());
    let mut out = merged.into_iter().collect::<Vec<_>>();
    out.sort_unstable();
    (!out.is_empty()).then_some(out)
}

fn return_projection_step_matches(
    pattern: &ReturnProjectionStep,
    candidate: &ReturnProjectionStep,
) -> bool {
    pattern == candidate
        || matches!(
            (pattern, candidate),
            (
                ReturnProjectionStep::AnyIndex,
                ReturnProjectionStep::AnyIndex
                    | ReturnProjectionStep::ConstantIndex(_)
                    | ReturnProjectionStep::DynamicIndex(_)
            )
        )
}

fn return_projection_is_prefix(
    prefix: &[ReturnProjectionStep],
    path: &[ReturnProjectionStep],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .zip(path)
            .all(|(pattern, candidate)| return_projection_step_matches(pattern, candidate))
}

fn project_return_sources(
    sources: Vec<ReturnSource>,
    projection: &[ReturnProjectionStep],
) -> Option<Vec<ReturnSource>> {
    let mut projected = FxHashSet::default();
    for mut source in sources {
        if return_projection_is_prefix(&source.result_projection, projection) {
            source
                .projection
                .extend_from_slice(&projection[source.result_projection.len()..]);
            source.result_projection.clear();
            projected.insert(source);
        } else if return_projection_is_prefix(projection, &source.result_projection) {
            source.result_projection.drain(..projection.len());
            projected.insert(source);
        }
    }
    let mut projected = projected.into_iter().collect::<Vec<_>>();
    projected.sort_unstable();
    (!projected.is_empty()).then_some(projected)
}

fn prefix_return_sources(sources: &mut [ReturnSource], prefix: &[ReturnProjectionStep]) {
    for source in sources {
        let mut result_projection = prefix.to_vec();
        result_projection.append(&mut source.result_projection);
        source.result_projection = result_projection;
    }
}

#[derive(Clone, PartialEq, Eq, derive_more::From)]
enum Typeable<'db> {
    Expr(ExprId, ExprProp<'db>),
    Pat(PatId),
}

impl Typeable<'_> {
    fn span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr, ..) => expr.span(body).into(),
            Self::Pat(pat) => pat.span(body).into(),
        }
    }
}

fn try_instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> Result<TyId<'db>, UnificationError> {
    // Positional application is sound here because the args and the binder
    // come from the same definition: a trait method's leading binder params
    // are its trait's params in declaration order, and `inst.args` are
    // exactly their instantiations. Positionally bridging *across*
    // definitions (e.g. a receiver's type args into an impl's binder) is
    // not — an impl's self type is an arbitrary pattern over its params;
    // that binding comes from the method-table probe's unification.
    let ty = TyId::foldl(
        db,
        TyId::func(db, method.as_callable(db).unwrap()),
        inst.args(db),
    );

    let inst_self = table.instantiate_to_term(inst.self_ty(db));
    table.unify(inst_self, receiver_ty)?;

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    Ok(ty.fold_with(db, &mut subst))
}

fn instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    try_instantiate_trait_method(db, method, table, receiver_ty, inst)
        .expect("selected trait method candidate receiver should unify")
}

/// Instantiate a trait-associated function type (no receiver), e.g. `T::make`.
fn instantiate_trait_assoc_fn<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    // Same-definition positional application: see
    // `try_instantiate_trait_method`.
    let ty = TyId::foldl(db, TyId::func(db, method), inst.args(db));

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    ty.fold_with(db, &mut subst)
}

struct TyCheckerFinalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    body: TypedBody<'db>,
    assumptions: PredicateListId<'db>,
    ty_vars: FxHashSet<InferenceKey<'db>>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    direct_call_callees: FxHashSet<ExprId>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> Visitor<'db> for TyCheckerFinalizer<'db> {
    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        _: &Pat<'db>,
    ) {
        let ty = self.body.pat_ty(self.db, pat);
        let span = ctxt.span().unwrap();
        self.check_unknown(ty, span.clone().into());

        walk_pat(self, ctxt, pat)
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Skip the check if the expr is block.
        if !matches!(expr_data, Expr::Block(..)) {
            let prop = self.body.expr_prop(self.db, expr);
            let span = ctxt.span().unwrap();
            self.check_unknown(prop.ty, span.clone().into());
            let is_direct_call_callee =
                matches!(expr_data, Expr::Path(..)) && self.direct_call_callees.contains(&expr);
            if prop.binding.is_none() && !is_direct_call_callee {
                self.check_wf(prop.ty, span.into());
            }
        }

        // We need this additional check for method call because the callable type is
        // not tied to the expression type. Well-formedness is not re-checked
        // here: the callable's declared constraints were already registered as
        // call-constraint obligations (with a `required by this bound` note),
        // so a WF check would report every unsatisfied bound a second time —
        // the same reason direct-call callees are excluded above.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.into());
        }

        walk_expr(self, ctxt, expr);
    }

    fn visit_item(
        &mut self,
        _: &mut VisitorCtxt<'db, crate::core::visitor::prelude::LazyItemSpan<'db>>,
        _: crate::core::hir_def::ItemKind<'db>,
    ) {
    }
}

impl<'db> TyCheckerFinalizer<'db> {
    fn new(mut checker: TyChecker<'db>) -> Self {
        let assumptions = checker.env.assumptions();
        checker.resolve_deferred();
        let closure_ty_replacements = checker.finalize_late_closure_captures();
        checker.finalize_deferred_assignment_modes();
        checker.finalize_array_repeat_copy_checks();
        checker.resolve_pending_place_checks();
        checker.revalidate_late_closure_capability_changes(&closure_ty_replacements);
        checker.refresh_assignment_flow_metadata();
        let return_borrow_provider = checker
            .env
            .first_return_borrow_provider
            .as_ref()
            .map(|(_, provider)| *provider);
        let mut body = checker.env.finish(&mut checker.table);
        body.return_borrow_provider = return_borrow_provider;
        let direct_call_callees = body.body.map_or_else(FxHashSet::default, |body_id| {
            body_id
                .exprs(checker.db)
                .iter()
                .filter_map(|(_expr, expr_data)| match expr_data {
                    Partial::Present(Expr::Call(callee, ..)) => Some(*callee),
                    _ => None,
                })
                .collect()
        });

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            effect_provider_keys: checker.effect_provider_keys,
            direct_call_callees,
            diags: checker.diags,
        }
    }

    fn finish(mut self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        self.check_unknown_types();
        (self.diags, self.body)
    }

    fn check_unknown_types(&mut self) {
        if let Some(body) = self.body.body {
            let mut ctxt = VisitorCtxt::with_body(self.db, body);
            self.visit_body(&mut ctxt, body);
        }
    }

    fn check_unknown(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || !flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let keys = inference_keys(self.db, &ty);
        if !keys.is_empty()
            && keys
                .iter()
                .all(|key| self.effect_provider_keys.contains(key))
        {
            return;
        }

        let mut skip_diag = false;
        for key in keys {
            // If at least one of the inference keys are already seen, we will skip emitting
            // diagnostics.
            skip_diag |= !self.ty_vars.insert(key);
        }

        if !skip_diag {
            let diag = BodyDiag::TypeAnnotationNeeded { span, ty };
            self.diags.push(diag.into())
        }
    }

    fn check_wf(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let solve_cx = TraitSolveCx::new(self.db, self.body.body.unwrap().scope());
        if let Some(diag) = ty.emit_wf_diag(self.db, solve_cx, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        hir_def::ItemKind,
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn closure_replay_transaction_restores_all_rewritten_type_state() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "closure_replay_transaction_snapshot.fe".into(),
            r#"
trait Marker {}

fn probe() {
    let first = || 1 as u256
    let second = || 2 as u256
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let probe = find_func(&db, top_mod, "probe");
        let marker = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Marker") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing Marker trait");
        let (_, typed_body) = check_func_body(&db, probe);
        let mut closures = typed_body
            .closure_infos()
            .map(|(_, info)| (info.def.expr, TyId::closure(&db, info.ty)))
            .collect::<Vec<_>>();
        closures.sort_by_key(|(expr, _)| *expr);
        let [(expr, old), (_, new)] = closures.as_slice() else {
            panic!("expected exactly two closure types");
        };
        assert_ne!(old, new);

        let mut checker =
            TyChecker::new(&db, BodyOwner::Func(probe)).expect("type checker construction");
        let expectation = ClosureExpectation {
            params: vec![*old],
            ret_ty: *old,
        };
        checker.expected = *old;
        checker
            .closure_expectations
            .insert(*expr, expectation.clone());
        checker
            .closure_type_expectations
            .push((*old, expectation.clone()));
        let old_predicate = TraitInstId::new(&db, marker, vec![*old], IndexMap::new());
        checker
            .env
            .seed_closure_replay_predicates_for_test(old_predicate);
        let old_env_predicates = checker.env.closure_replay_predicates_for_test();

        let transaction = checker.snapshot_closure_replay_transaction();
        checker.apply_contextual_closure_ty_replacements(&FxHashMap::from_iter([(*old, *new)]));

        let rewritten_expectation = ClosureExpectation {
            params: vec![*new],
            ret_ty: *new,
        };
        let new_predicate = TraitInstId::new(&db, marker, vec![*new], IndexMap::new());
        let new_predicate_list = PredicateListId::new(&db, vec![new_predicate]);
        assert_eq!(checker.expected, *new);
        assert_eq!(
            checker.closure_expectations.get(expr),
            Some(&rewritten_expectation),
        );
        assert_eq!(
            checker.closure_type_expectations,
            vec![(*new, rewritten_expectation)],
        );
        assert_eq!(
            checker.env.closure_replay_predicates_for_test(),
            (vec![new_predicate], new_predicate_list, new_predicate_list),
        );

        checker.rollback_closure_replay_transaction(transaction);

        assert_eq!(checker.expected, *old);
        assert_eq!(checker.closure_expectations.get(expr), Some(&expectation),);
        assert_eq!(checker.closure_type_expectations, vec![(*old, expectation)],);
        assert_eq!(
            checker.env.closure_replay_predicates_for_test(),
            old_env_predicates,
        );
    }
}
