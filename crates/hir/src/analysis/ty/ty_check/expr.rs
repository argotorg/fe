use either::Either;
use num_bigint::{BigUint, Sign};
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;

use crate::core::hir_def::{
    ArithBinOp, BinOp, CallArg, CallArg as HirCallArg, CallableDef, ClosureDef, Cond, CondId, Expr,
    ExprId, FieldIndex, IdentId, IntegerId, LitKind, LogicalBinOp, Partial, Pat, PatId, PathId,
    Stmt, StmtId, TypeKind, TypeMode, UnOp, VariantKind, WithBinding,
};
use crate::{
    span::{DynLazySpan, expr::LazyExprSpan},
    visitor::{Visitor, VisitorCtxt, walk_expr},
};

use super::{
    BodyOwner, ClosureCapture, ClosureCaptureAccess, ClosureCaptureConstruction,
    CodeRegionIntrinsicKind, ConstIntrinsicKind, ConstRef, PatternLayoutContext, PendingPlaceCheck,
    RecordLike, SemanticExprLowering, Typeable, ValueAccess, ValuePathRef,
    effect_env::{
        FamilyKeyedEntry, FrameLookupResult, MatchedForwarder, MatchedKeyedEntry, MatchedWitness,
    },
    env::{
        ClosureInfo, EffectOrigin, EffectParamSite, ExprProp, LateClosureCaptureContribution,
        LocalBinding, ParamSite, PendingCallableLookup, PendingCast, PendingField,
        PendingMethodLookup, PendingPrimitiveOp, ProvidedEffect, TraitObligation,
        TraitObligationOrigin, TyCheckEnv,
    },
    path::ResolvedPathInBody,
    ty_may_be_code_region_token,
};
use crate::analysis::place::{Place, PlaceBase, PlaceProjection};
use crate::analysis::ty::{
    adt_def::AdtRef,
    assoc_const::{AssocConstUse, InherentConstUse},
    canonical::{Canonical, Canonicalized, Solution},
    closure::ClosureCallTrait,
    const_ty::{BodyHoleSite, HoleAnchor, HoleMinter, instantiate_inherent_const_decl_ty},
    corelib::{
        resolve_core_range_types, resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path,
    },
    diagnostics::{
        BodyDiag, FuncBodyDiag, MustUseSubject, TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{
        BarrierReason, EffectBarrier, EffectKeyKind, EffectPatternKey, EffectQuery,
        EffectRequirementDecl, EffectRequirementKey, EffectWitness, ForwardedEffectKey,
        PatternSlotKind, StoredEffectKey, StoredTraitKey, StoredTypeKey, TraitPatternKey,
        TypePatternKey, WitnessTransport,
        elaborate::{
            build_barrier_pattern_for_with_key,
            build_conservative_same_family_barrier_pattern_in_scope, build_effect_query_for_call,
            contains_projection_or_invalid_query_state, effect_requirement_decls_for_callable,
            finalize_stored_effect_key, query_contains_unresolved_inference,
        },
        match_::{
            KeyMatchCommit, apply_key_match_commit, instantiate_trait_pattern_in,
            instantiate_trait_pattern_in_with_bindings, query_matches_forwarder,
            query_matches_witness,
        },
        place_effect_provider_param_index_map, stored_value_contains_implicit_layout_params,
        stored_value_contains_out_of_scope_params,
    },
    fold::{AssocTySubst, TyFoldable as _, TyFolder, rewrite_types},
    layout_holes::{layout_hole_fallback_ty, rewrite_structural_holes},
    pattern_ir::{
        KnownPatternScrutinee, PatternBranchReachability, known_pattern_scrutinee_from_const,
        known_scrutinee_arm_reachability, single_pattern_branch_reachability,
    },
    provider::{
        ProviderLayoutEvidence, ProviderTransport, provider_semantics,
        provider_semantics_for_specialized_call,
    },
    trait_def::{ImplementorOrigin, TraitInstId, impls_for_trait_in_ingots},
    trait_resolution::{
        GoalSatisfiability, PredicateListId, Selection, TraitGoalSolution, TraitSolveCx,
        WellFormedness, check_ty_wf, is_goal_satisfiable,
    },
    ty_check::callable::{Callable, EffectProviderProvenance, EffectProviderSpecialization},
    ty_contains_const_hole,
    ty_def::{
        BorrowKind, CapabilityKind, ClosureCallMode, ClosureCaptures, ClosureParamMode,
        ClosureSignature, ClosureTy, MAX_CLOSURE_FIELDS, PrimTy, TyBase, TyData, TyVarSort,
        closure_field_count_is_supported, prim_int_bits,
    },
    ty_error::collect_hir_ty_diags,
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb, Spanned,
    name_resolution::{
        EarlyNameQueryId, ExpectedPathKind, NameDomain, NameResBucket, NameResolutionError,
        PathRes, QueryDirective,
        diagnostics::{CallableFieldCallHint, PathResDiag},
        is_scope_visible_from,
        method_selection::{
            MethodCandidate, MethodSelectionError, TraitMethodCand, select_method_candidate,
        },
        resolve_name_res_with_minter, resolve_query,
    },
    place::resolve_place_field_index,
    semantic::{
        SemConstScalar, SemConstValue, SemOrigin, eval_const_ref,
        instance::resolve_semantic_const_ref,
    },
    ty::{
        LayoutBundlePathStep,
        const_expr::ConstExpr,
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, try_eval_const_int_expr},
        normalize::normalize_ty,
        ty_check::{RecordInitLowering, TyChecker, path::RecordInitChecker},
        ty_def::{InvalidCause, TyId},
        ty_lower::{
            callable_input_carrier_projected_layout_ty, callable_input_layout_origin_ty,
            callable_input_layout_projection_paths, callable_input_projected_layout_ty,
            instantiate_callable_effect_layout_args, instantiate_callable_projection_layout_args,
            lower_hir_ty,
        },
    },
};
use crate::hir_def::{FieldParent, ItemKind, scope_graph::ScopeId};
use crate::semantic::{
    FieldStorageLayout, LayoutProjection, LayoutViewError, LayoutViewKind, ProviderBinding,
};
use common::indexmap::IndexMap;

#[derive(Debug, Clone, Copy)]
pub(super) enum TypeEffectBindingMatch<'db> {
    Direct {
        given: TyId<'db>,
    },
    Provider {
        resolution: ProviderTargetResolution<'db>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureReplayOutcome {
    Unchanged,
    Replayed,
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TraitImplementorSelection<'db> {
    Unique(ImplementorOrigin<'db>),
    Ambiguous(FxHashSet<ImplementorOrigin<'db>>),
}

type CallableValueSelectionError<'db> = (IdentId<'db>, TyId<'db>, MethodSelectionError<'db>);
type CallableValueSelection<'db> = (
    TyId<'db>,
    Canonicalized<'db, TyId<'db>>,
    MethodCandidate<'db>,
);
type PendingCallableValueSelection<'db> = (TyId<'db>, Vec<super::env::PendingMethodCandidate<'db>>);

impl ClosureReplayOutcome {
    fn include(&mut self, other: Self) {
        *self = match (*self, other) {
            (Self::Failed, _) | (_, Self::Failed) => Self::Failed,
            (Self::Replayed, _) | (_, Self::Replayed) => Self::Replayed,
            (Self::Unchanged, Self::Unchanged) => Self::Unchanged,
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct ProviderTargetResolution<'db> {
    target_ty: TyId<'db>,
    target_seed_ty: TyId<'db>,
    handle_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
    effect_ref_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
    effect_ref_mut_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignmentRhsOutcome {
    Capability,
    Payload,
    /// A deferred callable or projection has not established its final result
    /// carrier yet. Do not let it constrain a surrounding control-flow join.
    Unresolved,
    Never,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EffectCaptureFootprint<'db> {
    Binding(LateClosureCaptureContribution<'db>),
    ExternalRvalue {
        expr: ExprId,
        provider_closure_depth: usize,
    },
}

#[derive(Debug, Clone, Copy)]
struct AssignmentFlow {
    normal: bool,
    breaks: bool,
    continues: bool,
    returns: bool,
}

impl AssignmentFlow {
    const NORMAL: Self = Self {
        normal: true,
        breaks: false,
        continues: false,
        returns: false,
    };

    fn then(self, next: Self) -> Self {
        Self {
            normal: self.normal && next.normal,
            breaks: self.breaks || self.normal && next.breaks,
            continues: self.continues || self.normal && next.continues,
            returns: self.returns || self.normal && next.returns,
        }
    }

    fn without_normal(self) -> Self {
        Self {
            normal: false,
            ..self
        }
    }

    fn or(self, other: Self) -> Self {
        Self {
            normal: self.normal || other.normal,
            breaks: self.breaks || other.breaks,
            continues: self.continues || other.continues,
            returns: self.returns || other.returns,
        }
    }

    fn has_control_exit(self) -> bool {
        self.breaks || self.continues || self.returns
    }
}

#[derive(Debug, Clone, Copy)]
struct AssignmentBoolFlow {
    on_true: bool,
    on_false: bool,
    breaks: bool,
    continues: bool,
    returns: bool,
}

impl AssignmentBoolFlow {
    fn from_flow(flow: AssignmentFlow) -> Self {
        Self {
            on_true: flow.normal,
            on_false: flow.normal,
            breaks: flow.breaks,
            continues: flow.continues,
            returns: flow.returns,
        }
    }

    fn as_flow(self) -> AssignmentFlow {
        AssignmentFlow {
            normal: self.on_true || self.on_false,
            breaks: self.breaks,
            continues: self.continues,
            returns: self.returns,
        }
    }

    fn or(self, other: Self) -> Self {
        Self {
            on_true: self.on_true || other.on_true,
            on_false: self.on_false || other.on_false,
            breaks: self.breaks || other.breaks,
            continues: self.continues || other.continues,
            returns: self.returns || other.returns,
        }
    }

    /// The value shared by every normal completion, if one is known.
    ///
    /// Escape paths do not invalidate this fact because they never forward an
    /// expression result.
    fn normal_value(self) -> Option<bool> {
        match (self.on_true, self.on_false) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (true, true) | (false, false) => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ResultExpectation<'db> {
    Single(TyId<'db>),
    CapabilityAssignment { slot: TyId<'db>, payload: TyId<'db> },
}

impl ResultExpectation<'_> {
    fn is_capability_assignment(self) -> bool {
        matches!(self, Self::CapabilityAssignment { .. })
    }
}

fn layout_projections_from_callable_path(
    path: &[LayoutBundlePathStep],
) -> Option<Vec<LayoutProjection>> {
    let mut projections = Vec::new();
    let mut steps = path.iter();
    while let Some(step) = steps.next() {
        match *step {
            LayoutBundlePathStep::Field(field) => {
                projections.push(LayoutProjection::Field(field));
            }
            LayoutBundlePathStep::Variant(variant) => {
                let LayoutBundlePathStep::Field(field) = *steps.next()? else {
                    return None;
                };
                projections.push(LayoutProjection::VariantField { variant, field });
            }
            LayoutBundlePathStep::Index => {
                projections.push(LayoutProjection::Index(None));
            }
            LayoutBundlePathStep::ConstParam(param) => {
                projections.push(LayoutProjection::ConstParam(param));
            }
        }
    }
    Some(projections)
}

impl<'db> ProviderTargetResolution<'db> {
    fn direct(target_ty: TyId<'db>) -> Self {
        Self {
            target_ty,
            target_seed_ty: target_ty,
            handle_proof: None,
            effect_ref_proof: None,
            effect_ref_mut_proof: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct KeyedWitnessBuildScope<'db> {
    pub(super) scope: ScopeId<'db>,
    pub(super) assumptions: PredicateListId<'db>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct KeyedWitnessBuildOptions<'db> {
    pub(super) scope: KeyedWitnessBuildScope<'db>,
    pub(super) emit_diag: bool,
    pub(super) mode: WitnessBuildMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum WitnessBuildMode {
    ExplicitKeyedWith,
    SeededRequirement,
}

#[derive(Debug, Clone)]
enum EffectEvidence<'db> {
    Keyed {
        provider: ProvidedEffect<'db>,
        key_kind: EffectKeyKind,
        target_ty: Option<TyId<'db>>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
    UnkeyedType {
        provider: ProvidedEffect<'db>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
    UnkeyedTrait {
        provider: ProvidedEffect<'db>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EffectArgStyle {
    Place,
    TempPlace,
    Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignLhsStatus {
    Assignable,
    Immutable,
    NonAssignable,
    Deferred,
}

#[derive(Debug, Clone, Default)]
pub(super) struct EffectCommitPlan<'db> {
    key_match: Option<KeyMatchCommit<'db>>,
    trait_solutions: SmallVec<[(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>); 2]>,
    provider_resolution: Option<ProviderTargetResolution<'db>>,
    extra_unifications: SmallVec<[(TyId<'db>, TyId<'db>); 4]>,
}

#[derive(Debug, Clone)]
enum EffectResolution<'db> {
    Chosen(Box<EffectEvidence<'db>>),
    BlockedByBarrier,
    Missing,
    Ambiguous,
}

fn evidence_provider<'db>(evidence: &EffectEvidence<'db>) -> ProvidedEffect<'db> {
    match evidence {
        EffectEvidence::Keyed { provider, .. }
        | EffectEvidence::UnkeyedType { provider, .. }
        | EffectEvidence::UnkeyedTrait { provider, .. } => *provider,
    }
}

pub(super) enum PendingPrimitiveOpResolution {
    Pending,
    Resolved,
    Done,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureAnnotationPosition {
    Param,
    Return,
}

impl<'db> TyChecker<'db> {
    pub(super) fn expr_contains_closure_syntax(&self, expr: ExprId) -> bool {
        struct ClosureFinder {
            found: bool,
        }

        impl<'db> Visitor<'db> for ClosureFinder {
            fn visit_expr(
                &mut self,
                ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
                expr: ExprId,
                expr_data: &Expr<'db>,
            ) {
                if matches!(expr_data, Expr::Closure { .. }) {
                    self.found = true;
                } else if !self.found {
                    walk_expr(self, ctxt, expr);
                }
            }

            fn visit_item(
                &mut self,
                _: &mut VisitorCtxt<'db, crate::span::item::LazyItemSpan<'db>>,
                _: crate::hir_def::ItemKind<'db>,
            ) {
            }
        }

        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };
        let mut finder = ClosureFinder { found: false };
        let mut ctxt = VisitorCtxt::with_expr(self.db, self.env.scope(), self.body(), expr);
        finder.visit_expr(&mut ctxt, expr, expr_data);
        finder.found
    }

    pub(super) fn expr_can_replay_contextual_closure(&mut self, expr: ExprId) -> bool {
        if self.expr_contains_closure_syntax(expr) {
            return true;
        }
        let Some(prop) = self.env.typed_expr(expr) else {
            return false;
        };

        fn contains_checked_closure<'db>(
            checker: &TyChecker<'db>,
            ty: TyId<'db>,
            origins: Option<&FxHashSet<ClosureDef<'db>>>,
        ) -> bool {
            if let Some(closure) = ty.base_ty(checker.db).as_closure(checker.db) {
                let def = closure.def(checker.db);
                return def.body == checker.body()
                    && checker.env.closure_info(def.expr).is_some()
                    && origins.is_none_or(|origins| origins.contains(&def));
            }
            let (_, args) = ty.decompose_ty_app(checker.db);
            args.iter()
                .copied()
                .any(|arg| contains_checked_closure(checker, arg, origins))
        }

        let ty = self.normalize_ty(prop.ty);
        let origins = prop
            .binding
            .and_then(|binding| self.env.contextual_closure_binding_origins(binding));
        if prop.binding.is_some() && origins.is_none() {
            return false;
        }
        contains_checked_closure(self, ty, origins)
    }

    fn contextual_closure_value_origins(&mut self, expr: ExprId) -> FxHashSet<ClosureDef<'db>> {
        fn collect_ty_defs<'db>(
            checker: &TyChecker<'db>,
            ty: TyId<'db>,
            defs: &mut FxHashSet<ClosureDef<'db>>,
        ) {
            if let Some(closure) = ty.base_ty(checker.db).as_closure(checker.db) {
                let def = closure.def(checker.db);
                if def.body == checker.body() && checker.env.closure_info(def.expr).is_some() {
                    defs.insert(def);
                }
            }
            let (_, args) = ty.decompose_ty_app(checker.db);
            for &arg in args {
                collect_ty_defs(checker, arg, defs);
            }
        }

        struct AliasOriginFinder<'a, 'db> {
            checker: &'a TyChecker<'db>,
            origins: FxHashSet<ClosureDef<'db>>,
        }

        impl<'db> Visitor<'db> for AliasOriginFinder<'_, 'db> {
            fn visit_expr(
                &mut self,
                ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
                expr: ExprId,
                expr_data: &Expr<'db>,
            ) {
                if let Some(binding) = self
                    .checker
                    .env
                    .typed_expr(expr)
                    .and_then(|prop| prop.binding)
                    && let Some(origins) =
                        self.checker.env.contextual_closure_binding_origins(binding)
                {
                    self.origins.extend(origins.iter().copied());
                }
                // A closure body computes a later value; its internal aliases
                // are not provenance for the closure value being initialized.
                if !matches!(expr_data, Expr::Closure { .. }) {
                    walk_expr(self, ctxt, expr);
                }
            }

            fn visit_item(
                &mut self,
                _: &mut VisitorCtxt<'db, crate::span::item::LazyItemSpan<'db>>,
                _: crate::hir_def::ItemKind<'db>,
            ) {
            }
        }

        let Some(prop) = self.env.typed_expr(expr) else {
            return FxHashSet::default();
        };
        let value_ty = self.normalize_ty(prop.ty);
        let mut value_defs = FxHashSet::default();
        collect_ty_defs(self, value_ty, &mut value_defs);
        if value_defs.is_empty() {
            return value_defs;
        }

        let mut origins = FxHashSet::default();
        if self.expr_contains_closure_syntax(expr) {
            origins.extend(value_defs.iter().copied());
        }
        let mut finder = AliasOriginFinder {
            checker: self,
            origins,
        };
        let mut ctxt = VisitorCtxt::with_expr(self.db, self.env.scope(), self.body(), expr);
        if let Partial::Present(expr_data) = expr.data(self.db, self.body()) {
            finder.visit_expr(&mut ctxt, expr, expr_data);
        }
        finder.origins.retain(|def| value_defs.contains(def));
        finder.origins
    }

    pub(super) fn record_contextual_closure_binding_origins(
        &mut self,
        pat: PatId,
        initializer: ExprId,
    ) {
        let origins = self.contextual_closure_value_origins(initializer);

        fn record_pat<'db>(
            checker: &mut TyChecker<'db>,
            pat: PatId,
            origins: &FxHashSet<ClosureDef<'db>>,
        ) {
            let Partial::Present(pat_data) = pat.data(checker.db, checker.body()) else {
                return;
            };
            match pat_data {
                Pat::Path(..) => {
                    if let Some(binding @ LocalBinding::Local { .. }) = checker.env.pat_binding(pat)
                    {
                        checker
                            .env
                            .set_contextual_closure_binding_origins(binding, origins.clone());
                    }
                }
                Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                    for &pat in pats {
                        record_pat(checker, pat, origins);
                    }
                }
                Pat::Record(_, fields) => {
                    for field in fields {
                        record_pat(checker, field.pat, origins);
                    }
                }
                Pat::Or(lhs, rhs) => {
                    record_pat(checker, *lhs, origins);
                    record_pat(checker, *rhs, origins);
                }
                Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
            }
        }
        record_pat(self, pat, &origins);
    }

    pub(super) fn record_contextual_closure_call_param_origins(
        &mut self,
        closure: ClosureTy<'db>,
        param_idx: usize,
        arg: ExprId,
    ) {
        let Some(binding) = self
            .env
            .closure_info(closure.def(self.db).expr)
            .and_then(|info| info.params.get(param_idx))
            .copied()
        else {
            return;
        };
        let origins = self.contextual_closure_value_origins(arg);
        if origins.is_empty() {
            return;
        }
        let mut merged = self
            .env
            .contextual_closure_binding_origins(binding)
            .cloned()
            .unwrap_or_default();
        merged.extend(origins);
        self.env
            .set_contextual_closure_binding_origins(binding, merged);
    }

    fn call_args_include_closure(&mut self, args: &[CallArg<'db>]) -> bool {
        args.iter()
            .any(|arg| self.expr_can_replay_contextual_closure(arg.expr))
    }

    fn code_region_intrinsic_kind(
        &self,
        callable_def: CallableDef<'db>,
    ) -> Option<CodeRegionIntrinsicKind> {
        let CallableDef::Func(func) = callable_def else {
            return None;
        };
        let offset = resolve_lib_func_path(
            self.db,
            self.env.scope(),
            "std::evm::intrinsic::code_region_offset",
        );
        let len = resolve_lib_func_path(
            self.db,
            self.env.scope(),
            "std::evm::intrinsic::code_region_len",
        );
        Some(if Some(func) == offset {
            CodeRegionIntrinsicKind::Offset
        } else if Some(func) == len {
            CodeRegionIntrinsicKind::Len
        } else {
            return None;
        })
    }

    pub(super) fn code_region_method_kind(
        &self,
        receiver_ty: TyId<'db>,
        method_name: IdentId<'db>,
    ) -> Option<CodeRegionIntrinsicKind> {
        let evm_ty = resolve_lib_type_path(self.db, self.env.scope(), "std::evm::Evm")?;
        if receiver_ty != evm_ty {
            return None;
        }
        let name = method_name.data(self.db);
        Some(if name == "code_region_offset" {
            CodeRegionIntrinsicKind::Offset
        } else if name == "code_region_len" {
            CodeRegionIntrinsicKind::Len
        } else {
            return None;
        })
    }

    fn const_intrinsic_kind(&self, callable_def: CallableDef<'db>) -> Option<ConstIntrinsicKind> {
        let CallableDef::Func(func) = callable_def else {
            return None;
        };
        (resolve_lib_func_path(self.db, self.env.scope(), "core::size_of") == Some(func))
            .then_some(ConstIntrinsicKind::SizeOf)
    }

    pub(super) fn check_expr(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        if !self.closure_type_expectations.is_empty() {
            self.env.record_deferred_closure_replay_context(
                expr,
                expected,
                &self.closure_type_expectations,
            );
        }
        self.check_expr_with_result_context(expr, ResultExpectation::Single(expected), false)
    }

    pub(super) fn check_expr_unknown(&mut self, expr: ExprId) -> ExprProp<'db> {
        let t = self.fresh_ty();
        self.check_expr(expr, t)
    }

    pub(super) fn check_expr_with_closure_expectation(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        closure_expected: super::ClosureExpectation<'db>,
    ) -> ExprProp<'db> {
        if self.env.closure_info(expr).is_some() && self.env.typed_expr(expr).is_some() {
            return self
                .replay_checked_closure_with_expectation(expr, expected, closure_expected)
                .0;
        }
        let previous = self.closure_expectations.insert(expr, closure_expected);
        let result = self.check_expr(expr, expected);
        if let Some(previous) = previous {
            self.closure_expectations.insert(expr, previous);
        } else {
            self.closure_expectations.remove(&expr);
        }
        result
    }

    fn replay_checked_closure_with_expectation(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        closure_expected: super::ClosureExpectation<'db>,
    ) -> (ExprProp<'db>, ClosureReplayOutcome) {
        let Some(mut info) = self.env.closure_info(expr).cloned() else {
            return (ExprProp::invalid(self.db), ClosureReplayOutcome::Failed);
        };
        let Some(mut prop) = self.env.typed_expr(expr) else {
            return (ExprProp::invalid(self.db), ClosureReplayOutcome::Failed);
        };
        let original_ty = prop.ty;

        self.check_closure_expected_arity(expr, info.ty.params(self.db).len(), &closure_expected);
        for (idx, (&actual, &expected)) in info
            .ty
            .params(self.db)
            .iter()
            .zip(&closure_expected.params)
            .enumerate()
        {
            self.equate_closure_param_ty(
                actual,
                info.ty.param_modes(self.db)[idx],
                expected,
                expr.span(self.body())
                    .into_closure_expr()
                    .params()
                    .param(idx)
                    .into(),
            );
        }

        let actual_ret = self.normalize_ty(info.ty.ret_ty(self.db));
        let expected_ret = self.normalize_ty(closure_expected.ret_ty);
        let mut access_updates = FxHashMap::default();
        for &return_expr in &info.return_exprs {
            let Some(return_prop) = self.env.typed_expr(return_expr) else {
                continue;
            };
            let return_actual = self.normalize_ty(return_prop.ty);
            if return_actual.is_never(self.db) || return_actual == TyId::unit(self.db) {
                continue;
            }
            let (_, outcome) = self.replay_typed_expr_with_closure_type_expectations_inner(
                return_expr,
                expected_ret,
                &mut access_updates,
            );
            if outcome == ClosureReplayOutcome::Failed {
                self.equate_ty(actual_ret, expected_ret, expr.span(self.body()).into());
                return (prop, ClosureReplayOutcome::Failed);
            }
        }

        let body_prop = self
            .env
            .typed_expr(info.body)
            .expect("checked closure body must remain typed");
        let ret_ty = self.unify_ty(
            Typeable::Expr(info.body, body_prop.clone()),
            body_prop.ty,
            expected_ret,
        );
        if ret_ty.has_invalid(self.db) {
            return (prop, ClosureReplayOutcome::Failed);
        }

        for expr_accesses in &mut info.capture_expr_accesses {
            for (&access_expr, access) in expr_accesses {
                if let Some(updated) = access_updates.get(&access_expr) {
                    *access = *updated;
                }
            }
        }
        for (capture, expr_accesses) in info.captures.iter_mut().zip(&info.capture_expr_accesses) {
            capture.access = capture.access_without_return;
            for access in expr_accesses.values() {
                capture.access.include(*access);
            }
        }

        let closure_ty = ClosureTy::new(
            self.db,
            info.def,
            info.ty.parent_args(self.db).to_vec(),
            ClosureCaptures::new(
                info.captures.iter().map(|capture| capture.ty).collect(),
                info.captures.iter().map(|capture| capture.access).collect(),
            ),
            ClosureSignature::new(
                info.ty.params(self.db).to_vec(),
                info.ty.param_modes(self.db).to_vec(),
                ret_ty,
            ),
        );
        info.ty = closure_ty;
        self.env.replace_closure_info(expr, info);
        let rebuilt_ty = TyId::closure(self.db, closure_ty);
        prop.ty = rebuilt_ty;
        self.env.type_expr(expr, prop.clone());
        let actual = prop.ty;
        prop.ty = if self.normalize_ty(expected) == original_ty {
            rebuilt_ty
        } else {
            self.unify_ty(Typeable::Expr(expr, prop.clone()), actual, expected)
        };
        self.env.type_expr(expr, prop.clone());
        if original_ty != rebuilt_ty {
            self.apply_contextual_closure_ty_replacements(&FxHashMap::from_iter([(
                original_ty,
                rebuilt_ty,
            )]));
            prop = self
                .env
                .typed_expr(expr)
                .expect("closure replacement must retain its expression property");
        }
        (prop, ClosureReplayOutcome::Replayed)
    }

    pub(super) fn check_expr_with_closure_type_expectations(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        expectations: Vec<(TyId<'db>, super::ClosureExpectation<'db>)>,
    ) -> ExprProp<'db> {
        let previous_len = self.closure_type_expectations.len();
        self.closure_type_expectations.extend(expectations);
        if self.env.typed_expr(expr).is_some() {
            let (result, _) = self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            self.closure_type_expectations.truncate(previous_len);
            return result;
        }
        if matches!(
            expr.data(self.db, self.body()),
            Partial::Present(Expr::Closure { .. })
        ) && let Some(expectation) = self.closure_expectation_for_type(expected)
        {
            self.closure_type_expectations.truncate(previous_len);
            return self.check_expr_with_closure_expectation(expr, expected, expectation);
        }
        let result = self.check_expr(expr, expected);
        if !result.ty.has_invalid(self.db) && self.expr_can_replay_contextual_closure(expr) {
            let (replayed, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            if outcome == ClosureReplayOutcome::Replayed {
                self.closure_type_expectations.truncate(previous_len);
                return replayed;
            }
        }
        self.closure_type_expectations.truncate(previous_len);
        result
    }

    pub(super) fn replay_deferred_expr_with_closure_context(
        &mut self,
        expr: ExprId,
        resolved: ExprProp<'db>,
    ) -> (ExprProp<'db>, bool) {
        let Some(context) = self.env.deferred_closure_replay_context(expr).cloned() else {
            return (resolved, true);
        };
        let previous_prop = self.env.typed_expr(expr);
        self.env.type_expr(expr, resolved.clone());
        let previous_len = self.closure_type_expectations.len();
        self.closure_type_expectations
            .extend(context.expectations.clone());
        let (replayed, outcome) =
            self.replay_typed_expr_with_closure_type_expectations(expr, context.expected);
        self.closure_type_expectations.truncate(previous_len);
        let satisfied = outcome != ClosureReplayOutcome::Failed
            && self.deferred_closure_replay_context_is_satisfied(&context);
        if satisfied {
            (replayed, true)
        } else {
            if let Some(previous_prop) = previous_prop {
                self.env.type_expr(expr, previous_prop);
            }
            (resolved, false)
        }
    }

    fn deferred_closure_replay_context_is_satisfied(
        &mut self,
        context: &super::env::DeferredClosureReplayContext<'db>,
    ) -> bool {
        for (subject, expectation) in &context.expectations {
            if !self.ty_contains_exact(context.expected, *subject) {
                continue;
            }
            let subject = self.table.fold_ty(self.db, *subject);
            let Some(closure) = subject.base_ty(self.db).as_closure(self.db) else {
                return false;
            };
            if closure.params(self.db).len() != expectation.params.len() {
                return false;
            }
            for (actual, expected) in closure
                .params(self.db)
                .iter()
                .copied()
                .zip(expectation.params.iter().copied())
            {
                if !self.ty_unifies(actual, expected) {
                    return false;
                }
            }
            if !self.ty_unifies(closure.ret_ty(self.db), expectation.ret_ty) {
                return false;
            }
        }
        true
    }

    fn ty_contains_exact(&self, ty: TyId<'db>, needle: TyId<'db>) -> bool {
        if ty == needle {
            return true;
        }
        let (_, args) = ty.decompose_ty_app(self.db);
        args.iter()
            .copied()
            .any(|arg| self.ty_contains_exact(arg, needle))
    }

    fn clear_terminal_deferred_closure_replay_context(
        &mut self,
        expr: ExprId,
        outcome: PendingPrimitiveOpResolution,
    ) -> PendingPrimitiveOpResolution {
        if !matches!(outcome, PendingPrimitiveOpResolution::Pending) {
            self.env.consume_deferred_closure_replay_context(expr);
            debug_assert!(
                self.env.deferred_closure_replay_context(expr).is_none(),
                "terminal deferred resolution must not retain contextual replay state"
            );
        }
        outcome
    }

    pub(super) fn apply_contextual_closure_ty_replacements(
        &mut self,
        replacements: &FxHashMap<TyId<'db>, TyId<'db>>,
    ) {
        if replacements.is_empty() {
            return;
        }
        self.expected = rewrite_types(self.db, self.expected, replacements);
        for expectation in self.closure_expectations.values_mut() {
            for param in &mut expectation.params {
                *param = rewrite_types(self.db, *param, replacements);
            }
            expectation.ret_ty = rewrite_types(self.db, expectation.ret_ty, replacements);
        }
        for (subject, expectation) in &mut self.closure_type_expectations {
            *subject = rewrite_types(self.db, *subject, replacements);
            for param in &mut expectation.params {
                *param = rewrite_types(self.db, *param, replacements);
            }
            expectation.ret_ty = rewrite_types(self.db, expectation.ret_ty, replacements);
        }
        self.env.apply_closure_ty_replacements(replacements);
    }

    /// Replays contextual closure expectations through an expression tree that
    /// was already checked while a deferred callable was unresolved.
    ///
    /// Re-entering ordinary checking is not safe here: aggregate and call
    /// lowerings are registered exactly once, and rebuilding only the nested
    /// closure would otherwise leave its enclosing aggregate specialized with
    /// the obsolete closure type. This traversal follows only expression
    /// forms through which an expected result type structurally flows.
    fn replay_typed_expr_with_closure_type_expectations(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
    ) -> (ExprProp<'db>, ClosureReplayOutcome) {
        // The entire structural walk shares one environment transaction.
        // Recursive `_inner` calls only publish environment changes after
        // producing `Replayed`; `Unchanged` paths operate on local callable
        // clones, and any `Failed` descendant poisons the enclosing outcome.
        // One whole-body snapshot therefore gives atomic rollback without
        // cloning all typed maps once per aggregate node.
        let checker_snapshot = self.snapshot_state();
        let env_snapshot = self.env.snapshot_closure_replay_state();
        let diag_len = self.diags.len();
        let mut access_updates = FxHashMap::default();
        let result = self.replay_typed_expr_with_closure_type_expectations_inner(
            expr,
            expected,
            &mut access_updates,
        );
        if result.1 == ClosureReplayOutcome::Replayed {
            self.commit_state(checker_snapshot);
            result
        } else {
            self.env.restore_closure_replay_state(env_snapshot);
            self.diags.truncate(diag_len);
            self.rollback_state(checker_snapshot);
            (
                self.env
                    .typed_expr(expr)
                    .unwrap_or_else(|| ExprProp::invalid(self.db)),
                result.1,
            )
        }
    }

    fn replay_typed_expr_with_closure_type_expectations_inner(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        access_updates: &mut FxHashMap<ExprId, ClosureCaptureAccess>,
    ) -> (ExprProp<'db>, ClosureReplayOutcome) {
        let Some(mut prop) = self.env.typed_expr(expr) else {
            return (ExprProp::invalid(self.db), ClosureReplayOutcome::Failed);
        };
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return (prop, ClosureReplayOutcome::Failed);
        };
        let expected = self.normalize_ty(expected);
        if !self.replay_type_shape_compatible(prop.ty, expected) {
            return (prop, ClosureReplayOutcome::Failed);
        }

        if matches!(expr_data, Expr::Closure { .. }) {
            let Some(expectation) = self.closure_expectation_for_type(expected) else {
                return (prop, ClosureReplayOutcome::Failed);
            };
            return self.replay_checked_closure_with_expectation(expr, expected, expectation);
        }

        let mut outcome = ClosureReplayOutcome::Unchanged;
        match expr_data {
            Expr::Block(stmts) => {
                if let Some(last) = stmts.last()
                    && let Partial::Present(Stmt::Expr(tail)) = last.data(self.db, self.body())
                {
                    outcome = self
                        .replay_typed_expr_with_closure_type_expectations_inner(
                            *tail,
                            expected,
                            access_updates,
                        )
                        .1;
                }
            }
            Expr::With(_, body) => {
                outcome = self
                    .replay_typed_expr_with_closure_type_expectations_inner(
                        *body,
                        expected,
                        access_updates,
                    )
                    .1;
            }
            Expr::If(_, then_expr, Some(else_expr)) => {
                outcome.include(
                    self.replay_typed_expr_with_closure_type_expectations_inner(
                        *then_expr,
                        expected,
                        access_updates,
                    )
                    .1,
                );
                outcome.include(
                    self.replay_typed_expr_with_closure_type_expectations_inner(
                        *else_expr,
                        expected,
                        access_updates,
                    )
                    .1,
                );
            }
            Expr::Match(scrutinee, Partial::Present(arms)) => {
                let reachable = self
                    .assignment_match_arm_reachability(*scrutinee, arms)
                    .unwrap_or_else(|| vec![true; arms.len()]);
                for (arm, reachable) in arms.iter().zip(reachable) {
                    if !reachable {
                        continue;
                    }
                    outcome.include(
                        self.replay_typed_expr_with_closure_type_expectations_inner(
                            arm.body,
                            expected,
                            access_updates,
                        )
                        .1,
                    );
                }
            }
            Expr::Tuple(elems) => {
                let (base, expected_elems) = expected.decompose_ty_app(self.db);
                if base.is_tuple(self.db) && expected_elems.len() == elems.len() {
                    for (&elem, &elem_expected) in elems.iter().zip(expected_elems) {
                        outcome.include(
                            self.replay_typed_expr_with_closure_type_expectations_inner(
                                elem,
                                elem_expected,
                                access_updates,
                            )
                            .1,
                        );
                    }
                }
            }
            Expr::Array(elems) => {
                let (base, args) = expected.decompose_ty_app(self.db);
                if base.is_array(self.db)
                    && let Some(&elem_expected) = args.first()
                {
                    for &elem in elems {
                        outcome.include(
                            self.replay_typed_expr_with_closure_type_expectations_inner(
                                elem,
                                elem_expected,
                                access_updates,
                            )
                            .1,
                        );
                    }
                }
            }
            Expr::ArrayRep(elem, _) => {
                let (base, args) = expected.decompose_ty_app(self.db);
                if base.is_array(self.db)
                    && let Some(&elem_expected) = args.first()
                {
                    outcome = self
                        .replay_typed_expr_with_closure_type_expectations_inner(
                            *elem,
                            elem_expected,
                            access_updates,
                        )
                        .1;
                }
            }
            Expr::Cast(inner, _) => {
                if let Some(inner_expected) =
                    self.replay_cast_inner_expected_ty(*inner, prop.ty, expected)
                {
                    outcome = self
                        .replay_typed_expr_with_closure_type_expectations_inner(
                            *inner,
                            inner_expected,
                            access_updates,
                        )
                        .1;
                }
            }
            Expr::RecordInit(_, fields) => {
                let lowering = self.env.record_init_lowering(expr);
                let record_like = match lowering {
                    Some(RecordInitLowering::Struct) => Some(RecordLike::from_ty(expected)),
                    Some(RecordInitLowering::EnumVariant(variant)) => {
                        Some(RecordLike::from_variant(
                            crate::analysis::name_resolution::ResolvedVariant {
                                ty: expected,
                                ..variant
                            },
                        ))
                    }
                    None => None,
                };
                if let Some(record_like) = record_like {
                    for field in fields {
                        let Some(label) = field.label_eagerly(self.db, self.body()) else {
                            continue;
                        };
                        let Some(field_expected) = record_like.record_field_ty(self.db, label)
                        else {
                            continue;
                        };
                        outcome.include(
                            self.replay_typed_expr_with_closure_type_expectations_inner(
                                field.expr,
                                field_expected,
                                access_updates,
                            )
                            .1,
                        );
                    }
                }
                if outcome == ClosureReplayOutcome::Replayed
                    && let Some(RecordInitLowering::EnumVariant(variant)) = lowering
                {
                    let ty = self.normalize_ty(expected);
                    self.env.replace_record_init_lowering(
                        expr,
                        RecordInitLowering::EnumVariant(
                            crate::analysis::name_resolution::ResolvedVariant { ty, ..variant },
                        ),
                    );
                }
            }
            Expr::Path(_) => {
                outcome = self.replay_path_closure_alias_with_expectations(
                    expr,
                    prop.ty,
                    expected,
                    access_updates,
                );
            }
            Expr::Field(base, Partial::Present(field)) => {
                if let Some(base_expected) =
                    self.replay_field_base_expected_ty(*base, *field, prop.ty, expected)
                {
                    outcome = self
                        .replay_typed_expr_with_closure_type_expectations_inner(
                            *base,
                            base_expected,
                            access_updates,
                        )
                        .1;
                }
            }
            Expr::Un(receiver, _) if self.env.callable_expr(expr).is_some() => {
                outcome = self.replay_typed_call_with_closure_type_expectations(
                    expr,
                    expected,
                    None,
                    Some(*receiver),
                    &[],
                    access_updates,
                );
            }
            Expr::Bin(receiver, rhs, _) if self.env.callable_expr(expr).is_some() => {
                let args = [HirCallArg {
                    label: None,
                    expr: *rhs,
                }];
                outcome = self.replay_typed_call_with_closure_type_expectations(
                    expr,
                    expected,
                    None,
                    Some(*receiver),
                    &args,
                    access_updates,
                );
            }
            Expr::Bin(base, _, BinOp::Index) => {
                if let Some(base_expected) =
                    self.replay_index_base_expected_ty(*base, prop.ty, expected)
                {
                    outcome = self
                        .replay_typed_expr_with_closure_type_expectations_inner(
                            *base,
                            base_expected,
                            access_updates,
                        )
                        .1;
                }
            }
            Expr::Call(callee, args) => {
                let callee_is_receiver = matches!(
                    self.env.semantic_expr_lowering(expr),
                    Some(SemanticExprLowering::Call {
                        callee_is_receiver: true,
                        ..
                    })
                );
                outcome = self.replay_typed_call_with_closure_type_expectations(
                    expr,
                    expected,
                    (!callee_is_receiver).then_some(*callee),
                    callee_is_receiver.then_some(*callee),
                    args,
                    access_updates,
                );
            }
            Expr::MethodCall(receiver, _, _, args) => {
                outcome = self.replay_typed_call_with_closure_type_expectations(
                    expr,
                    expected,
                    None,
                    Some(*receiver),
                    args,
                    access_updates,
                );
            }
            _ => {}
        }

        if outcome == ClosureReplayOutcome::Replayed {
            let expected = self.normalize_ty(expected);
            prop.ty = self.env.rewrite_closure_types(expected);
            if matches!(
                expr_data,
                Expr::Block(..) | Expr::With(..) | Expr::If(..) | Expr::Match(..) | Expr::Cast(..)
            ) {
                let (value_access, _) = self.contextual_value_access(prop.ty);
                self.env.replace_expr_value_access(expr, value_access);
                prop.value_access = value_access;
            }
            self.env.type_expr(expr, prop.clone());
        } else if let Some(coerced) =
            self.try_coerce_capability_for_expr_to_expected(expr, prop.ty, expected)
            && self.normalize_ty(coerced) != self.normalize_ty(prop.ty)
        {
            prop.ty = self.unify_ty(Typeable::Expr(expr, prop.clone()), coerced, expected);
            if prop.ty.has_invalid(self.db) {
                return (prop, ClosureReplayOutcome::Failed);
            }
            let (value_access, capture_access) = self.contextual_value_access(prop.ty);
            self.env.replace_expr_value_access(expr, value_access);
            prop.value_access = value_access;
            self.env.type_expr(expr, prop.clone());
            access_updates.insert(expr, capture_access);
            outcome = ClosureReplayOutcome::Replayed;
        }
        (prop, outcome)
    }

    fn replay_path_closure_alias_with_expectations(
        &mut self,
        expr: ExprId,
        actual: TyId<'db>,
        expected: TyId<'db>,
        access_updates: &mut FxHashMap<ExprId, ClosureCaptureAccess>,
    ) -> ClosureReplayOutcome {
        let mut replacements = FxHashMap::default();
        if !self.collect_closure_replay_type_replacements(actual, expected, &mut replacements) {
            return ClosureReplayOutcome::Failed;
        }
        if replacements.is_empty() {
            self.collect_bound_contextual_closure_replays(actual, &mut replacements);
        }
        if replacements.is_empty() {
            return ClosureReplayOutcome::Unchanged;
        }
        let Some(origins) = self
            .env
            .typed_expr(expr)
            .and_then(|prop| prop.binding)
            .and_then(|binding| self.env.contextual_closure_binding_origins(binding))
            .cloned()
        else {
            return ClosureReplayOutcome::Failed;
        };
        let mut closure_replays = Vec::new();
        for (&old, &expected) in &replacements {
            let Some(closure) = old.as_closure(self.db) else {
                return ClosureReplayOutcome::Failed;
            };
            let def = closure.def(self.db);
            if def.body != self.body()
                || self.env.closure_info(def.expr).is_none()
                || !origins.contains(&def)
            {
                return ClosureReplayOutcome::Failed;
            }
            closure_replays.push((def, expected));
        }
        closure_replays.sort_by_key(|(def, _)| def.expr);

        for (def, expected) in closure_replays {
            if !self.active_closure_replay_defs.insert(def) {
                return ClosureReplayOutcome::Failed;
            }
            let (_, outcome) = self.replay_typed_expr_with_closure_type_expectations_inner(
                def.expr,
                expected,
                access_updates,
            );
            self.active_closure_replay_defs.remove(&def);
            if outcome == ClosureReplayOutcome::Failed {
                return ClosureReplayOutcome::Failed;
            }
        }

        let Some(prop) = self.env.typed_expr(expr) else {
            return ClosureReplayOutcome::Failed;
        };
        // A path can still carry an inference variable whose table binding
        // contains the old closure nominal. Environment-wide replacement
        // cannot rewrite that binding in place, so validate the folded path
        // type after applying the same structural replacement explicitly.
        let replayed_actual = self.normalize_ty(prop.ty);
        let replayed_actual = rewrite_types(self.db, replayed_actual, &replacements);
        let replayed_actual = self.env.rewrite_closure_types(replayed_actual);
        let replayed_expected = self.normalize_ty(expected);
        let replayed_expected = self.env.rewrite_closure_types(replayed_expected);
        if !self.ty_unifies(replayed_actual, replayed_expected) {
            return ClosureReplayOutcome::Failed;
        }
        ClosureReplayOutcome::Replayed
    }

    fn collect_bound_contextual_closure_replays(
        &mut self,
        ty: TyId<'db>,
        replacements: &mut FxHashMap<TyId<'db>, TyId<'db>>,
    ) {
        let ty = self.normalize_ty(ty);
        if ty.as_closure(self.db).is_some() && self.closure_expectation_for_type(ty).is_some() {
            replacements.entry(ty).or_insert(ty);
            return;
        }
        let (_, args) = ty.decompose_ty_app(self.db);
        for arg in args {
            self.collect_bound_contextual_closure_replays(*arg, replacements);
        }
    }

    fn replay_field_base_expected_ty(
        &mut self,
        base: ExprId,
        field: FieldIndex<'db>,
        projected_actual: TyId<'db>,
        projected_expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let base_actual = self.normalize_ty(self.env.typed_expr(base)?.ty);
        let base_payload = base_actual
            .as_capability(self.db)
            .map_or(base_actual, |(_, payload)| payload);
        let field_actual = match field {
            FieldIndex::Ident(label) => {
                RecordLike::from_ty(base_payload).record_field_ty(self.db, label)?
            }
            FieldIndex::Index(index) => {
                if !base_payload.is_tuple(self.db) {
                    return None;
                }
                let (_, fields) = base_payload.decompose_ty_app(self.db);
                fields.get(index.data(self.db).to_usize()?).copied()?
            }
        };
        self.replay_projected_base_expected_ty(
            base_actual,
            field_actual,
            projected_actual,
            projected_expected,
        )
    }

    fn replay_cast_inner_expected_ty(
        &mut self,
        inner: ExprId,
        cast_actual: TyId<'db>,
        cast_expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let source_actual = self.normalize_ty(self.env.typed_expr(inner)?.ty);
        let mut replacements = FxHashMap::default();
        if !self.collect_closure_replay_type_replacements(
            cast_actual,
            cast_expected,
            &mut replacements,
        ) {
            return None;
        }
        if replacements.is_empty() {
            // A cast is a transparent return carrier when its result is
            // contextually viewed. Replay that access through the source so
            // a captured non-Copy value is read rather than left as a move.
            let coerced = self.try_coerce_capability_to_expected(cast_actual, cast_expected)?;
            if self.normalize_ty(coerced) != self.normalize_ty(cast_expected)
                || cast_expected.as_view(self.db).is_none()
            {
                return None;
            }
            let source_payload = source_actual
                .as_capability(self.db)
                .map_or(source_actual, |(_, payload)| payload);
            return Some(TyId::view_of(self.db, source_payload));
        }
        let source_expected = rewrite_types(self.db, source_actual, &replacements);
        (source_expected != source_actual).then_some(source_expected)
    }

    fn replay_index_base_expected_ty(
        &mut self,
        base: ExprId,
        projected_actual: TyId<'db>,
        projected_expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let base_actual = self.normalize_ty(self.env.typed_expr(base)?.ty);
        let base_payload = base_actual
            .as_capability(self.db)
            .map_or(base_actual, |(_, payload)| payload);
        if !base_payload.is_array(self.db) {
            return None;
        }
        let (_, args) = base_payload.decompose_ty_app(self.db);
        let element_actual = *args.first()?;
        self.replay_projected_base_expected_ty(
            base_actual,
            element_actual,
            projected_actual,
            projected_expected,
        )
    }

    fn replay_projected_base_expected_ty(
        &mut self,
        base_actual: TyId<'db>,
        selected_actual: TyId<'db>,
        projected_actual: TyId<'db>,
        projected_expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let selected_actual = self.normalize_ty(selected_actual);
        let projected_actual = self.normalize_ty(projected_actual);
        if selected_actual != projected_actual {
            return None;
        }
        let projected_expected = self.normalize_ty(projected_expected);
        let replacements = FxHashMap::from_iter([(selected_actual, projected_expected)]);
        let base_expected = rewrite_types(self.db, base_actual, &replacements);
        (base_expected != base_actual).then_some(base_expected)
    }

    fn contextual_value_access(&mut self, ty: TyId<'db>) -> (ValueAccess, ClosureCaptureAccess) {
        let ty = self.normalize_ty(ty);
        if ty.has_var(self.db) {
            (
                ValueAccess::MoveIfNonCopy,
                ClosureCaptureAccess::MoveIfNonCopy,
            )
        } else if self.ty_is_copy(ty) {
            (ValueAccess::Read, ClosureCaptureAccess::Read)
        } else {
            (ValueAccess::Move, ClosureCaptureAccess::Move)
        }
    }

    fn replay_type_shape_compatible(&mut self, actual: TyId<'db>, expected: TyId<'db>) -> bool {
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        if actual == expected {
            return true;
        }
        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return false;
        }
        if actual.is_ty_var(self.db) || expected.is_ty_var(self.db) {
            return true;
        }
        if let Some(actual_closure) = actual.as_closure(self.db) {
            return expected
                .as_closure(self.db)
                .is_some_and(|expected_closure| {
                    actual_closure.def(self.db) == expected_closure.def(self.db)
                })
                || self.closure_expectation_for_type(expected).is_some();
        }
        if let Some(coerced) = self.try_coerce_capability_to_expected(actual, expected)
            && coerced != actual
        {
            return self.replay_type_shape_compatible(coerced, expected);
        }

        let (actual_base, actual_args) = actual.decompose_ty_app(self.db);
        let (expected_base, expected_args) = expected.decompose_ty_app(self.db);
        actual_base == expected_base
            && actual_args.len() == expected_args.len()
            && !actual_args.is_empty()
            && actual_args
                .iter()
                .copied()
                .zip(expected_args.iter().copied())
                .all(|(actual, expected)| self.replay_type_shape_compatible(actual, expected))
    }

    fn collect_closure_replay_type_replacements(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        replacements: &mut FxHashMap<TyId<'db>, TyId<'db>>,
    ) -> bool {
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        if actual == expected {
            return true;
        }
        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return false;
        }

        if let TyData::AssocTy(assoc) = actual.data(self.db)
            && let Some(binding) = assoc
                .trait_
                .assoc_type_bindings(self.db)
                .get(&assoc.name)
                .copied()
                .or_else(|| self.applicable_trait_assoc_binding(assoc.trait_, assoc.name))
        {
            return self.collect_closure_replay_type_replacements(binding, expected, replacements);
        }

        if let Some(actual_closure) = actual.as_closure(self.db)
            && (expected
                .as_closure(self.db)
                .is_some_and(|expected_closure| {
                    actual_closure.def(self.db) == expected_closure.def(self.db)
                })
                || self.closure_expectation_for_type(expected).is_some())
        {
            return match replacements.get(&actual).copied() {
                Some(previous) => previous == expected,
                None => {
                    replacements.insert(actual, expected);
                    true
                }
            };
        }

        if let Some(coerced) = self.try_coerce_capability_to_expected(actual, expected)
            && coerced != actual
        {
            return self.collect_closure_replay_type_replacements(coerced, expected, replacements);
        }

        if actual.is_ty_var(self.db) || expected.is_ty_var(self.db) {
            return true;
        }

        let (actual_base, actual_args) = actual.decompose_ty_app(self.db);
        let (expected_base, expected_args) = expected.decompose_ty_app(self.db);
        actual_base == expected_base
            && actual_args.len() == expected_args.len()
            && !actual_args.is_empty()
            && actual_args
                .iter()
                .copied()
                .zip(expected_args.iter().copied())
                .all(|(actual, expected)| {
                    self.collect_closure_replay_type_replacements(actual, expected, replacements)
                })
    }

    fn trait_implementor_selection(
        &self,
        inst: TraitInstId<'db>,
    ) -> Option<TraitImplementorSelection<'db>> {
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        match solve_cx.select_impl(self.db, inst) {
            Selection::Unique(implementor) => Some(TraitImplementorSelection::Unique(
                implementor.origin(self.db),
            )),
            Selection::Ambiguous(implementors) => Some(TraitImplementorSelection::Ambiguous(
                implementors
                    .iter()
                    .map(|implementor| implementor.origin(self.db))
                    .collect(),
            )),
            Selection::NotFound => None,
        }
    }

    fn callable_trait_implementor_origin(
        &self,
        callable: &Callable<'db>,
    ) -> Option<ImplementorOrigin<'db>> {
        let CallableDef::Func(func) = callable.callable_def() else {
            return None;
        };
        if let Some(impl_trait) = func.containing_impl_trait(self.db) {
            Some(ImplementorOrigin::Hir(impl_trait))
        } else if func.containing_trait(self.db).is_some() {
            Some(ImplementorOrigin::Assumption)
        } else {
            None
        }
    }

    fn applicable_trait_implementor_origins(
        &self,
        inst: TraitInstId<'db>,
    ) -> FxHashSet<ImplementorOrigin<'db>> {
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, inst);
        let canonical_inst = Canonical::new(self.db, inst);
        let implementors = impls_for_trait_in_ingots(self.db, primary, secondary, canonical_inst);
        let mut origins = FxHashSet::default();
        for implementor in implementors.iter().copied() {
            let mut table = UnificationTable::new(self.db);
            let inst = canonical_inst.extract_identity(&mut table);
            let implementor = table.instantiate_with_fresh_vars(implementor);
            if table.unify(implementor.trait_inst(self.db), inst).is_ok() {
                origins.insert(implementor.origin(self.db));
            }
        }
        origins
    }

    fn applicable_trait_assoc_binding(
        &self,
        inst: TraitInstId<'db>,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, inst);
        let canonical_inst = Canonical::new(self.db, inst);
        let implementors = impls_for_trait_in_ingots(self.db, primary, secondary, canonical_inst);
        let mut resolved = None;
        for implementor in implementors.iter().copied() {
            let mut table = UnificationTable::new(self.db);
            let goal = canonical_inst.extract_identity(&mut table);
            let implementor = table.instantiate_with_fresh_vars(implementor);
            let implemented = implementor.trait_inst(self.db);
            if implemented.def(self.db) != goal.def(self.db)
                || implemented.args(self.db).len() != goal.args(self.db).len()
                || implemented
                    .args(self.db)
                    .iter()
                    .copied()
                    .zip(goal.args(self.db).iter().copied())
                    .any(|(implemented, goal)| table.unify(implemented, goal).is_err())
            {
                continue;
            }
            let Some(&binding) = implemented.assoc_type_bindings(self.db).get(&name) else {
                continue;
            };
            let binding = binding.fold_with(self.db, &mut table);
            if binding.has_var(self.db) {
                return None;
            }
            let binding = normalize_ty(self.db, binding, self.env.scope(), self.env.assumptions());
            match resolved {
                Some(previous) if previous != binding => return None,
                Some(_) => {}
                None => resolved = Some(binding),
            }
        }
        resolved
    }

    fn replay_typed_call_with_closure_type_expectations(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        direct_callee: Option<ExprId>,
        receiver: Option<ExprId>,
        args: &[HirCallArg<'db>],
        access_updates: &mut FxHashMap<ExprId, ClosureCaptureAccess>,
    ) -> ClosureReplayOutcome {
        let Some(mut callable) = self.env.callable_expr(expr).cloned() else {
            return ClosureReplayOutcome::Failed;
        };
        let original_callable = callable.clone();
        let original_receiver_ty = receiver
            .and_then(|receiver| self.env.typed_expr(receiver))
            .map(|prop| self.normalize_ty(prop.ty));
        let original_arg_tys = args
            .iter()
            .map(|arg| {
                self.env
                    .typed_expr(arg.expr)
                    .map(|prop| self.normalize_ty(prop.ty))
            })
            .collect::<Vec<_>>();
        let original_trait_inst = callable
            .trait_inst()
            .map(|inst| self.normalize_trait_goal(inst));
        let original_trait_selection =
            original_trait_inst.and_then(|inst| self.trait_implementor_selection(inst));
        let original_trait_origin = self.callable_trait_implementor_origin(&callable);
        let original_applicable_trait_origins = original_trait_inst
            .map(|inst| self.applicable_trait_implementor_origins(inst))
            .unwrap_or_default();
        let effectful = matches!(
            callable.callable_def(),
            CallableDef::Func(func) if func.has_effects(self.db)
        );
        let old_effect_args = if effectful {
            self.env
                .call_effect_args(expr)
                .map_or_else(Vec::new, <[super::ResolvedEffectArg<'db>]>::to_vec)
        } else {
            Vec::new()
        };

        let mut replacements = FxHashMap::default();
        if !self.collect_closure_replay_type_replacements(
            callable.ret_ty(self.db),
            expected,
            &mut replacements,
        ) {
            return ClosureReplayOutcome::Failed;
        }
        if replacements.is_empty() {
            let replayed_ret = self.normalize_ty(callable.ret_ty(self.db));
            if let (TyData::AssocTy(assoc), Some(inst)) =
                (replayed_ret.data(self.db), callable.trait_inst())
                && assoc.trait_.def(self.db) == inst.def(self.db)
                && let Some(&binding) = inst.assoc_type_bindings(self.db).get(&assoc.name)
                && !self.collect_closure_replay_type_replacements(
                    binding,
                    expected,
                    &mut replacements,
                )
            {
                return ClosureReplayOutcome::Failed;
            }
        }
        if replacements.is_empty() {
            let raw_ret = callable
                .callable_def()
                .ret_ty(self.db)
                .instantiate(self.db, callable.generic_args());
            if !self.collect_closure_replay_type_replacements(raw_ret, expected, &mut replacements)
            {
                return ClosureReplayOutcome::Failed;
            }
        }
        if !replacements.is_empty() {
            callable = callable.fold_with(self.db, &mut self.table);
            callable = rewrite_types(self.db, callable, &replacements);
        }

        let formal_ret = callable
            .callable_def()
            .ret_ty(self.db)
            .instantiate_identity();
        callable.specialize_params_from_actual(self.db, formal_ret, expected);
        let arg_offset = usize::from(receiver.is_some());
        let mut outcome = ClosureReplayOutcome::Unchanged;
        if let Some(receiver) = receiver {
            let Some(receiver_expected) = callable.arg_ty(self.db, 0) else {
                return ClosureReplayOutcome::Failed;
            };
            let receiver_expected =
                self.replay_callable_argument_value_ty(&callable, 0, receiver_expected);
            let (receiver_prop, receiver_outcome) = self
                .replay_typed_expr_with_closure_type_expectations_inner(
                    receiver,
                    receiver_expected,
                    access_updates,
                );
            outcome.include(receiver_outcome);
            if receiver_outcome == ClosureReplayOutcome::Replayed {
                callable.specialize_arg_from_actual(self.db, 0, receiver_prop.ty);
            }
        }

        for (idx, arg) in args.iter().enumerate() {
            let arg_idx = idx + arg_offset;
            let Some(arg_expected) = callable.arg_ty(self.db, arg_idx) else {
                continue;
            };
            let arg_expected =
                self.replay_callable_argument_value_ty(&callable, arg_idx, arg_expected);
            let (arg_prop, arg_outcome) = self
                .replay_typed_expr_with_closure_type_expectations_inner(
                    arg.expr,
                    arg_expected,
                    access_updates,
                );
            outcome.include(arg_outcome);
            if arg_outcome == ClosureReplayOutcome::Replayed {
                callable.specialize_arg_from_actual(self.db, arg_idx, arg_prop.ty);
            }
        }
        if outcome == ClosureReplayOutcome::Unchanged && callable != original_callable {
            outcome = ClosureReplayOutcome::Replayed;
        }
        if outcome != ClosureReplayOutcome::Replayed {
            return outcome;
        }

        self.specialize_callable_layout_args(&mut callable, receiver, args);
        callable = callable.fold_with(self.db, &mut self.table);
        for arg in callable.generic_args_mut() {
            *arg = self.normalize_ty(*arg);
        }
        if let Some(inst) = callable.trait_inst() {
            callable.trait_inst = Some(self.normalize_trait_goal(inst));
        }
        if let (Some(original_inst), Some(replayed_inst)) =
            (original_trait_inst, callable.trait_inst())
            && original_inst != replayed_inst
        {
            let replayed_selection = self.trait_implementor_selection(replayed_inst);
            let preserves_origin = match (&original_trait_selection, original_trait_origin) {
                (Some(original), _) => Some(original) == replayed_selection.as_ref(),
                (None, callable_origin) => match replayed_selection {
                    Some(TraitImplementorSelection::Unique(origin)) => {
                        original_applicable_trait_origins == FxHashSet::from_iter([origin])
                            || (original_applicable_trait_origins.is_empty()
                                && callable_origin == Some(origin))
                    }
                    Some(TraitImplementorSelection::Ambiguous(_)) | None => false,
                },
            };
            if !preserves_origin {
                return ClosureReplayOutcome::Failed;
            }
        }

        let replayed_ret = self.normalize_ty(callable.ret_ty(self.db));
        let replayed_ret = self
            .try_coerce_capability_to_expected(replayed_ret, expected)
            .unwrap_or(replayed_ret);
        if !self.ty_unifies(replayed_ret, expected) {
            return ClosureReplayOutcome::Failed;
        }

        if let Some(receiver) = receiver {
            let Some(receiver_expected) = callable.arg_ty(self.db, 0) else {
                return ClosureReplayOutcome::Failed;
            };
            let receiver_expected =
                self.replay_callable_argument_value_ty(&callable, 0, receiver_expected);
            if !self.expr_can_replay_contextual_closure(receiver)
                && let Some(original) = original_receiver_ty
            {
                let original = self
                    .try_coerce_capability_for_expr_to_expected(
                        receiver,
                        original,
                        receiver_expected,
                    )
                    .unwrap_or(original);
                if !self.ty_unifies(original, receiver_expected) {
                    return ClosureReplayOutcome::Failed;
                }
            }
            let Some(receiver_prop) = self.env.typed_expr(receiver) else {
                return ClosureReplayOutcome::Failed;
            };
            let actual = self.normalize_ty(receiver_prop.ty);
            let actual = self
                .try_coerce_capability_for_expr_to_expected(receiver, actual, receiver_expected)
                .unwrap_or(actual);
            if self
                .unify_ty(
                    Typeable::Expr(receiver, receiver_prop),
                    actual,
                    receiver_expected,
                )
                .has_invalid(self.db)
            {
                return ClosureReplayOutcome::Failed;
            }
        }
        for (idx, arg) in args.iter().enumerate() {
            let arg_idx = idx + arg_offset;
            let Some(arg_expected) = callable.arg_ty(self.db, arg_idx) else {
                continue;
            };
            let arg_expected =
                self.replay_callable_argument_value_ty(&callable, arg_idx, arg_expected);
            if !self.expr_can_replay_contextual_closure(arg.expr)
                && let Some(original) = original_arg_tys.get(idx).copied().flatten()
            {
                let original = self
                    .try_coerce_capability_for_expr_to_expected(arg.expr, original, arg_expected)
                    .unwrap_or(original);
                if !self.ty_unifies(original, arg_expected) {
                    return ClosureReplayOutcome::Failed;
                }
            }
            let Some(arg_prop) = self.env.typed_expr(arg.expr) else {
                continue;
            };
            let actual = self.normalize_ty(arg_prop.ty);
            let actual = self
                .try_coerce_capability_for_expr_to_expected(arg.expr, actual, arg_expected)
                .unwrap_or(actual);
            if self
                .unify_ty(Typeable::Expr(arg.expr, arg_prop), actual, arg_expected)
                .has_invalid(self.db)
            {
                return ClosureReplayOutcome::Failed;
            }
        }

        if effectful {
            let diag_len = self.diags.len();
            let new_effect_args =
                self.resolve_callable_effects_in_lexical_context(expr, &mut callable);
            if self.diags.len() != diag_len
                || old_effect_args.len() != new_effect_args.len()
                || !old_effect_args
                    .iter()
                    .zip(&new_effect_args)
                    .all(|(old, new)| {
                        old.param_idx == new.param_idx
                            && old.binding_idx == new.binding_idx
                            && old.key == new.key
                    })
            {
                return ClosureReplayOutcome::Failed;
            }
            let old_footprint = self.effect_capture_footprint(&old_effect_args);
            let new_footprint = self.effect_capture_footprint(&new_effect_args);
            if old_footprint != new_footprint {
                return ClosureReplayOutcome::Failed;
            }
            self.env.replace_call_effect_args(expr, new_effect_args);
        }

        let constraints =
            crate::analysis::ty::trait_resolution::constraint::collect_func_decl_constraints(
                self.db,
                callable.callable_def(),
                true,
            )
            .instantiate(self.db, callable.generic_args());
        let mut replayed_constraint_goals = FxHashMap::default();
        for (constraint_idx, &constraint) in constraints.list(self.db).iter().enumerate() {
            let constraint = if let Some(inst) = callable.trait_inst() {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(self.db, &mut subst)
            } else {
                constraint
            };
            let constraint = self.normalize_trait_goal(constraint);
            replayed_constraint_goals.insert(constraint_idx, constraint);
        }
        self.env.replace_call_constraint_obligations(
            expr,
            callable.callable_def(),
            &replayed_constraint_goals,
        );
        if let Some(inst) = callable.trait_inst() {
            self.env.replace_generic_confirmation_obligation(expr, inst);
            if let Some(callee) = direct_callee {
                self.env
                    .replace_generic_confirmation_obligation(callee, inst);
            }
        }
        self.env.replace_semantic_callable(expr, callable.clone());
        if let Some(callee) = direct_callee
            && self.env.callable_expr(callee).is_some()
        {
            self.env.replace_callable(callee, callable.clone());
            if let Some(mut callee_prop) = self.env.typed_expr(callee) {
                callee_prop.ty = callable.ty(self.db);
                self.env.type_expr(callee, callee_prop);
            }
        }
        ClosureReplayOutcome::Replayed
    }

    fn replay_callable_argument_value_ty(
        &mut self,
        callable: &Callable<'db>,
        arg_idx: usize,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        let expected = self.normalize_ty(expected);
        let strips_view_carrier = match callable.callable_def() {
            // Variant constructor arguments are stored fields. A `view T`
            // field therefore expects the capability itself, not its payload.
            CallableDef::VariantCtor(_) => false,
            CallableDef::Func(func) => {
                if callable
                    .call_trait_args_pack_ty(self.db, self.env.scope())
                    .is_some()
                    && arg_idx > 0
                {
                    // Expanded closure-call packs synthesize view mode for a
                    // capability argument.
                    expected.as_view(self.db).is_some()
                } else {
                    func.params(self.db).nth(arg_idx).is_some_and(|param| {
                        param.mode(self.db) == crate::hir_def::params::FuncParamMode::View
                    })
                }
            }
        };
        if strips_view_carrier {
            expected.as_view(self.db).unwrap_or(expected)
        } else {
            expected
        }
    }

    fn closure_expectation_for_type(
        &mut self,
        expected: TyId<'db>,
    ) -> Option<super::ClosureExpectation<'db>> {
        let expected = self.table.fold_ty(self.db, expected);
        let mut found = None;
        for (subject, expectation) in self.closure_type_expectations.clone() {
            if self.table.fold_ty(self.db, subject) != expected {
                continue;
            }
            if found
                .as_ref()
                .is_some_and(|previous| previous != &expectation)
            {
                return None;
            }
            found = Some(expectation);
        }
        found
    }

    fn check_closure_expected_arity(
        &mut self,
        expr: ExprId,
        given: usize,
        expected: &super::ClosureExpectation<'db>,
    ) {
        if given != expected.params.len() {
            self.push_diag(BodyDiag::ClosureParamNumMismatch {
                primary: expr.span(self.body()).into_closure_expr().params().into(),
                given,
                expected: expected.params.len(),
            });
        }
    }

    fn equate_closure_param_ty(
        &mut self,
        actual: TyId<'db>,
        actual_mode: ClosureParamMode,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> TyId<'db> {
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        let Some(expected_mode) = ClosureParamMode::try_from_carrier(self.db, expected) else {
            return self.equate_ty(actual, expected, span);
        };
        let actual_payload = actual_mode.payload(self.db, actual);
        let expected_payload = expected_mode.payload(self.db, expected);
        let payload = self.equate_ty(actual_payload, expected_payload, span.clone());
        if actual_mode != expected_mode
            && !actual.has_invalid(self.db)
            && !expected.has_invalid(self.db)
        {
            self.push_diag(BodyDiag::ClosureParamModeMismatch {
                primary: span,
                given: actual_mode,
                expected: expected_mode,
            });
        }
        actual_mode.carrier(self.db, payload)
    }

    pub(super) fn check_expr_with_discarded_result(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let prop =
            self.check_expr_with_result_context(expr, ResultExpectation::Single(expected), true);
        self.record_owned_value_use(expr, prop.ty);
        if !self.expr_propagates_discarded_result(expr) {
            self.check_unused_must_use(expr, prop.clone());
        }
        prop
    }

    fn check_expr_with_result_context(
        &mut self,
        expr: ExprId,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let typed = ExprProp::invalid(self.db);
            self.env.type_expr(expr, typed.clone());
            return typed;
        };

        let expected = match result_expectation {
            ResultExpectation::Single(expected) => expected,
            ResultExpectation::CapabilityAssignment { slot, payload } => match expr_data {
                Expr::Un(_, UnOp::Mut | UnOp::Ref) => slot,
                Expr::Un(_, op) if self.assignment_unary_may_lower_to_semantic_rvalue(*op) => {
                    self.fresh_ty()
                }
                Expr::Block(..)
                | Expr::Call(..)
                | Expr::MethodCall(..)
                | Expr::Cast(..)
                | Expr::Field(..)
                | Expr::If(..)
                | Expr::Match(..)
                | Expr::With(..) => self.fresh_ty(),
                Expr::Bin(_, _, op) if self.assignment_binary_may_lower_to_semantic_rvalue(*op) => {
                    self.fresh_ty()
                }
                _ => payload,
            },
        };
        let assignment_payload_hint = match result_expectation {
            ResultExpectation::CapabilityAssignment { payload, .. } => Some(payload),
            ResultExpectation::Single(_) => None,
        };
        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());

        self.env.enter_expr(expr);
        let mut actual = match expr_data {
            Expr::Lit(LitKind::String(string_id)) => {
                ExprProp::new(self.string_literal_ty(*string_id, expected), true)
            }
            Expr::Lit(lit) => ExprProp::new(self.lit_ty_for_expected(lit, expected), true),
            Expr::Block(..) => self.check_block(
                expr,
                expr_data,
                expected,
                result_expectation,
                result_discarded,
            ),
            Expr::Closure { .. } => self.check_closure(expr, expr_data, expected),
            Expr::Un(..) => self.check_unary(expr, expr_data, expected, assignment_payload_hint),
            Expr::Cast(inner, ty) => self.check_cast(expr, *inner, *ty),
            Expr::Bin(lhs, rhs, op) => {
                self.check_binary(expr, *lhs, *rhs, *op, expected, assignment_payload_hint)
            }
            Expr::Call(..) => self.check_call(expr, expr_data, expected),
            Expr::Assert(args) => self.check_assert(expr, args),
            Expr::MethodCall(..) => self.check_method_call(expr, expr_data, expected),
            Expr::Path(..) => self.check_path(expr, expr_data),
            Expr::RecordInit(..) => self.check_record_init(expr, expr_data, expected),
            Expr::Field(..) => self.check_field(expr, expr_data, expected),
            Expr::Tuple(..) => self.check_tuple(expr, expr_data, expected),
            Expr::Array(..) => self.check_array(expr, expr_data, expected),
            Expr::ArrayRep(..) => self.check_array_rep(expr, expr_data, expected),
            Expr::If(..) => self.check_if(
                expr,
                expr_data,
                expected,
                result_expectation,
                result_discarded,
            ),
            Expr::Match(..) => self.check_match(
                expr,
                expr_data,
                expected,
                result_expectation,
                result_discarded,
            ),
            Expr::Assign(..) => self.check_assign(expr, expr_data),
            Expr::AugAssign(..) => self.check_aug_assign(expr, expr_data),
            Expr::With(bindings, body) => self.check_with(
                bindings,
                *body,
                expected,
                result_expectation,
                result_discarded,
            ),
        };
        self.env.leave_expr();

        actual.ty = normalize_ty(self.db, actual.ty, self.env.scope(), self.env.assumptions());
        let flow = self.assignment_expr_flow(expr);
        let can_complete_normally = flow.normal;
        self.env
            .set_expr_normal_completion(expr, can_complete_normally);
        let normal_bool_value = self.assignment_expr_bool_flow(expr).normal_value();
        self.env.set_expr_normal_bool_value(expr, normal_bool_value);
        // A definitely non-completing expression imposes no result-type
        // constraint. Preserve the expression's structural type for semantic
        // lowering (for example, a record whose eager field diverges) while
        // suppressing the otherwise-spurious mismatch at this boundary.
        if can_complete_normally || flow.has_control_exit() || actual.ty.is_never(self.db) {
            if let Some(coerced) =
                self.try_coerce_capability_for_expr_to_expected(expr, actual.ty, expected)
            {
                actual.ty = coerced;
            }
            let typeable = Typeable::Expr(expr, actual.clone());
            actual.ty = self.unify_ty(typeable, actual.ty, expected);
        } else {
            self.env.type_expr(expr, actual.clone());
        }
        match expr_data {
            Expr::Call(..) => {
                if let Some(callable) = self.env.callable_expr(expr).cloned() {
                    let span = expr.span(self.body()).into_call_expr().callee().into();
                    callable.enqueue_constraints(self, expr, span);
                }
            }
            Expr::MethodCall(..) => {
                if let Some(callable) = self.env.callable_expr(expr).cloned() {
                    let span = expr
                        .span(self.body())
                        .into_method_call_expr()
                        .method_name()
                        .into();
                    callable.enqueue_constraints(self, expr, span);
                }
            }
            _ => {}
        }
        actual
    }

    fn expr_propagates_discarded_result(&self, expr: ExprId) -> bool {
        matches!(
            self.env.expr_data(expr),
            Partial::Present(Expr::Block(..) | Expr::If(..) | Expr::Match(..) | Expr::With(..))
        )
    }

    fn lit_ty_for_expected(&mut self, lit: &LitKind<'db>, expected: TyId<'db>) -> TyId<'db> {
        match lit {
            LitKind::String(_) if expected.is_core_dyn_string(self.db) => expected,
            _ => self.lit_ty(lit),
        }
    }

    fn check_block(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::Block(stmts) = expr_data else {
            unreachable!()
        };

        if stmts.is_empty() {
            ExprProp::new(TyId::unit(self.db), true)
        } else {
            self.env.enter_scope(expr);
            for &stmt in stmts[..stmts.len() - 1].iter() {
                self.check_discarded_stmt(stmt);
            }

            let last_stmt = stmts[stmts.len() - 1];
            let res = if result_discarded {
                let expected = if expected == TyId::unit(self.db) {
                    self.fresh_ty()
                } else {
                    expected
                };
                ExprProp::new(
                    self.check_discarded_stmt_with_expected(last_stmt, expected),
                    true,
                )
            } else if expected == TyId::unit(self.db) {
                self.check_discarded_stmt(last_stmt);
                ExprProp::new(TyId::unit(self.db), true)
            } else {
                match self.env.stmt_data(last_stmt) {
                    Partial::Present(Stmt::Expr(expr))
                        if result_expectation.is_capability_assignment() =>
                    {
                        self.check_expr_with_result_context(
                            *expr,
                            result_expectation,
                            result_discarded,
                        )
                    }
                    Partial::Present(Stmt::Expr(expr)) => self.check_expr(*expr, expected),
                    Partial::Present(_) => {
                        ExprProp::new(self.check_stmt(last_stmt, expected), true)
                    }
                    Partial::Absent => ExprProp::invalid(self.db),
                }
            };
            self.env.leave_scope();
            res
        }
    }

    fn check_discarded_stmt(&mut self, stmt: StmtId) {
        let ty = self.fresh_ty();
        self.check_discarded_stmt_with_expected(stmt, ty);
    }

    fn check_discarded_stmt_with_expected(
        &mut self,
        stmt: StmtId,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        match self.env.stmt_data(stmt) {
            Partial::Present(Stmt::Expr(expr)) => {
                self.check_expr_with_discarded_result(*expr, expected).ty
            }
            Partial::Present(_) => self.check_stmt(stmt, expected),
            Partial::Absent => TyId::invalid(self.db, InvalidCause::ParseError),
        }
    }

    fn check_unused_must_use(&mut self, expr: ExprId, prop: ExprProp<'db>) {
        if prop.ty.has_invalid(self.db) {
            return;
        }

        if let Some(adt_ref) = prop.ty.adt_ref(self.db)
            && adt_ref.is_must_use(self.db)
        {
            self.push_diag(BodyDiag::UnusedMustUse {
                primary: expr.span(self.body()).into(),
                subject: MustUseSubject::Type(prop.ty),
            });
            return;
        }

        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return;
        };
        if matches!(expr_data, Expr::Call(..) | Expr::MethodCall(..))
            && let Some(callable) = self.env.callable_expr(expr)
            && callable.callable_def().is_must_use(self.db)
        {
            self.push_diag(BodyDiag::UnusedMustUse {
                primary: expr.span(self.body()).into(),
                subject: MustUseSubject::Function(callable.callable_def()),
            });
        }
    }

    fn check_closure(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Closure {
            params,
            ret_ty,
            body,
        } = expr_data
        else {
            unreachable!()
        };
        let def = ClosureDef {
            body: self.body(),
            expr,
        };
        let param_count = params.data(self.db).len();
        if !closure_field_count_is_supported(param_count) {
            self.push_diag(BodyDiag::ClosureFieldLimitExceeded {
                primary: expr
                    .span(self.body())
                    .into_closure_expr()
                    .params()
                    .param(MAX_CLOSURE_FIELDS)
                    .name()
                    .into(),
                captures: false,
                given: param_count,
                max: MAX_CLOSURE_FIELDS,
            });
            return ExprProp::new(TyId::invalid(self.db, InvalidCause::Other), false);
        }
        let closure_expected = self
            .closure_expectations
            .get(&expr)
            .cloned()
            .or_else(|| self.closure_expectation_for_type(expected));
        if let Some(expected) = &closure_expected {
            self.check_closure_expected_arity(expr, params.data(self.db).len(), expected);
        }

        if matches!(
            self.env.owner(),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. }
        ) {
            self.push_diag(BodyDiag::ClosureInConstContext {
                primary: expr.span(self.body()).into(),
            });
        }

        self.env.enter_closure(def);
        self.env.enter_lexical_scope();

        let mut param_tys = Vec::with_capacity(params.data(self.db).len());
        let mut param_modes = Vec::with_capacity(params.data(self.db).len());
        let mut param_names = FxHashMap::default();
        for (idx, param) in params.data(self.db).iter().enumerate() {
            let param_span = expr
                .span(self.body())
                .into_closure_expr()
                .params()
                .param(idx);
            let duplicate_name = param.name().and_then(|name| {
                if let Some(first_idx) = param_names.get(&name).copied() {
                    let params_span = expr.span(self.body()).into_closure_expr().params();
                    self.push_diag(BodyDiag::DuplicateClosureParam {
                        primary: params_span.clone().param(idx).name().into(),
                        conflict_with: params_span.param(first_idx).name().into(),
                        name,
                    });
                    Some(name)
                } else {
                    param_names.insert(name, idx);
                    None
                }
            });
            let param_mode = if param.mode == crate::hir_def::params::FuncParamMode::Own {
                ClosureParamMode::Own
            } else {
                match param.ty.to_opt().map(|ty| ty.data(self.db)) {
                    Some(TypeKind::Mode(TypeMode::Mut, _)) => ClosureParamMode::Mut,
                    Some(TypeKind::Mode(TypeMode::Ref, _)) => ClosureParamMode::Ref,
                    Some(TypeKind::Mode(TypeMode::View, _)) => ClosureParamMode::View,
                    _ => ClosureParamMode::View,
                }
            };
            if param.is_mut && param_mode != ClosureParamMode::Own {
                self.push_diag(TyDiagCollection::from(
                    TyLowerDiag::InvalidMutParamPrefixWithoutOwnType {
                        span: param_span.clone().mut_kw().into(),
                    },
                ));
            }
            let expected_carrier = closure_expected
                .as_ref()
                .and_then(|expected| expected.params.get(idx))
                .copied();
            let expected_payload = expected_carrier.and_then(|expected| {
                let expected = self.normalize_ty(expected);
                ClosureParamMode::try_from_carrier(self.db, expected)
                    .map(|mode| mode.payload(self.db, expected))
            });
            let mut ty = match param.ty.to_opt() {
                None => {
                    let payload = expected_payload.unwrap_or_else(|| self.fresh_ty());
                    param_mode.carrier(self.db, payload)
                }
                Some(hir_ty) => match hir_ty.data(self.db) {
                    TypeKind::Mode(_, Partial::Absent) => {
                        let payload = expected_payload.unwrap_or_else(|| self.fresh_ty());
                        param_mode.carrier(self.db, payload)
                    }
                    _ => self.lower_closure_annotation(
                        hir_ty,
                        param_span.clone().ty(),
                        ClosureAnnotationPosition::Param,
                    ),
                },
            };
            if param_mode != ClosureParamMode::Own && ty.as_capability(self.db).is_none() {
                ty = param_mode.carrier(self.db, ty);
            }
            if param_mode == ClosureParamMode::Own && ty.as_capability(self.db).is_some() {
                self.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                    primary: param_span.ty().into(),
                    ty,
                });
                ty = TyId::invalid(self.db, InvalidCause::Other);
            }
            if let Some(expected) = expected_carrier {
                ty = self.equate_closure_param_ty(
                    ty,
                    param_mode,
                    expected,
                    expr.span(self.body())
                        .into_closure_expr()
                        .params()
                        .param(idx)
                        .into(),
                );
            }
            if !ty.is_star_kind(self.db) || ty_contains_const_hole(self.db, ty) {
                ty = TyId::invalid(self.db, InvalidCause::Other);
            }

            let binding = LocalBinding::Param {
                site: ParamSite::Closure(def),
                idx,
                mode: param.mode,
                ty,
                is_mut: param.is_mut,
            };
            self.env
                .register_closure_param(param.name().filter(|_| duplicate_name.is_none()), binding);
            param_tys.push(ty);
            param_modes.push(param_mode);
        }

        let ret_expected = match ret_ty {
            Some(ret_ty) => {
                let ret_ty = self.lower_closure_annotation(
                    *ret_ty,
                    expr.span(self.body()).into_closure_expr().ret_ty(),
                    ClosureAnnotationPosition::Return,
                );
                if !ret_ty.has_invalid(self.db) {
                    if let Some(expected) =
                        closure_expected.as_ref().map(|expected| expected.ret_ty)
                    {
                        self.equate_ty(
                            ret_ty,
                            expected,
                            expr.span(self.body()).into_closure_expr().ret_ty().into(),
                        )
                    } else {
                        ret_ty
                    }
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
            None => closure_expected
                .as_ref()
                .map_or_else(|| self.fresh_ty(), |expected| expected.ret_ty),
        };
        let saved_body_ctx = self.env.take_body_ctx();
        let saved_expected = std::mem::replace(&mut self.expected, ret_expected);
        let body_prop = self.check_expr(*body, ret_expected);
        self.expected = saved_expected;
        if let Some(provider) = body_prop.borrow_provider {
            if let Some((previous_span, previous_provider)) =
                self.env.first_return_borrow_provider.clone()
            {
                self.merge_concrete_borrow_providers(
                    previous_span,
                    Some(previous_provider),
                    body.span(self.body()).into(),
                    Some(provider),
                );
            } else {
                self.env.first_return_borrow_provider =
                    Some((body.span(self.body()).into(), provider));
            }
        }
        let closure_body_ctx = self.env.take_body_ctx();
        self.env.restore_body_ctx(saved_body_ctx);
        let ret_ty = self.normalize_ty(body_prop.ty);
        self.env.record_active_closure_return_expr(*body);
        self.record_return_value_use(*body, ret_ty);

        self.env.leave_scope();
        let (param_bindings, return_exprs, pending_captures) = self.env.leave_closure();
        let (captures, capture_expr_accesses) = pending_captures
            .into_iter()
            .map(|capture| {
                (
                    ClosureCapture {
                        binding: capture.binding,
                        ty: capture.ty,
                        construction: if capture.ty.as_capability(self.db).is_some()
                            || self.ty_is_copy(capture.ty)
                        {
                            ClosureCaptureConstruction::Copy
                        } else if capture.ty.has_var(self.db) {
                            ClosureCaptureConstruction::Deferred
                        } else {
                            ClosureCaptureConstruction::Move
                        },
                        access_without_return: capture.access_without_return,
                        access: capture.access,
                    },
                    capture.expr_accesses,
                )
            })
            .unzip::<_, _, Vec<_>, Vec<_>>();
        if !closure_field_count_is_supported(captures.len()) {
            self.push_diag(BodyDiag::ClosureFieldLimitExceeded {
                primary: expr.span(self.body()).into(),
                captures: true,
                given: captures.len(),
                max: MAX_CLOSURE_FIELDS,
            });
            return ExprProp::new(TyId::invalid(self.db, InvalidCause::Other), false);
        }
        for capture in &captures {
            let access = match capture.construction {
                ClosureCaptureConstruction::Copy => continue,
                ClosureCaptureConstruction::Deferred => ClosureCaptureAccess::MoveIfNonCopy,
                ClosureCaptureConstruction::Move => ClosureCaptureAccess::Move,
            };
            self.env.record_capture_access(capture.binding, access);
        }
        let capture_tys = captures
            .iter()
            .map(|capture| capture.ty)
            .collect::<Vec<_>>();
        let capture_accesses = captures
            .iter()
            .map(|capture| capture.access)
            .collect::<Vec<_>>();
        let parent_args = match self.env.owner() {
            BodyOwner::Func(func) => CallableDef::Func(func).params(self.db).to_vec(),
            _ => Vec::new(),
        };
        let closure_ty = ClosureTy::new(
            self.db,
            def,
            parent_args,
            ClosureCaptures::new(capture_tys, capture_accesses),
            ClosureSignature::new(param_tys, param_modes, ret_ty),
        );
        self.env.register_closure_info(
            expr,
            ClosureInfo {
                def,
                body: *body,
                return_exprs,
                params: param_bindings,
                captures,
                capture_expr_accesses,
                ty: closure_ty,
                return_borrow_provider: closure_body_ctx.return_borrow_provider(),
            },
        );
        ExprProp::new(TyId::closure(self.db, closure_ty), false)
    }

    fn lower_closure_annotation(
        &mut self,
        hir_ty: crate::hir_def::TypeId<'db>,
        span: crate::span::types::LazyTySpan<'db>,
        position: ClosureAnnotationPosition,
    ) -> TyId<'db> {
        let diags = collect_hir_ty_diags(
            self.db,
            self.env.scope(),
            hir_ty,
            span.clone(),
            self.env.assumptions(),
        );
        if !diags.is_empty() {
            for diag in diags {
                self.push_diag(diag);
            }
            return TyId::invalid(self.db, InvalidCause::Other);
        }

        let mut ty = lower_hir_ty(self.db, hir_ty, self.env.scope(), self.env.assumptions());
        let span: DynLazySpan<'db> = span.into();
        if !ty.is_star_kind(self.db) {
            self.push_diag(TyDiagCollection::from(TyLowerDiag::ExpectedStarKind(span)));
            return TyId::invalid(self.db, InvalidCause::Other);
        }
        if ty.is_const_ty(self.db) {
            self.push_diag(TyDiagCollection::from(TyLowerDiag::NormalTypeExpected {
                span,
                given: ty,
            }));
            return TyId::invalid(self.db, InvalidCause::Other);
        }
        if position == ClosureAnnotationPosition::Param {
            let db = self.db;
            let table = &mut self.table;
            let mut inferred_roots = FxHashMap::default();
            ty = rewrite_structural_holes(db, ty, |hole, hole_ty| {
                let root = hole.root(db);
                if let Some(inferred) = inferred_roots.get(&root) {
                    return Some(*inferred);
                }

                let hole_ty = layout_hole_fallback_ty(db, hole_ty);
                let key = table.new_key(hole_ty.kind(db), TyVarSort::General);
                let inferred = TyId::const_ty_var(db, hole_ty, key);
                inferred_roots.insert(root, inferred);
                Some(inferred)
            });
        }
        if ty_contains_const_hole(self.db, ty) {
            self.push_diag(TyDiagCollection::from(
                TyLowerDiag::ConstHoleInValuePosition { span, ty },
            ));
            return TyId::invalid(self.db, InvalidCause::Other);
        }
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            self.db,
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions()),
            ty,
        ) {
            self.push_diag(TyDiagCollection::from(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                },
            ));
        }
        ty
    }

    fn check_unary(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        assignment_payload_hint: Option<TyId<'db>>,
    ) -> ExprProp<'db> {
        let Expr::Un(lhs, op) = expr_data else {
            unreachable!()
        };
        let prop = self.check_expr_unknown(*lhs);
        if prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let place_ty = prop
            .ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(prop.ty);
        if place_ty.is_integral_var(self.db)
            && matches!(op, UnOp::Plus | UnOp::Minus | UnOp::Not | UnOp::BitNot)
            && let Some(payload) = assignment_payload_hint
        {
            self.try_apply_assignment_operator_payload_hint(place_ty, payload);
        }
        if (place_ty.is_integral_var(self.db) || place_ty.base_ty(self.db).is_ty_var(self.db))
            && matches!(op, UnOp::Plus | UnOp::Minus | UnOp::Not | UnOp::BitNot)
        {
            self.env
                .register_pending_primitive_op(PendingPrimitiveOp::Unary {
                    expr,
                    inner: *lhs,
                    op: *op,
                });
            return if place_ty.is_integral_var(self.db) {
                prop
            } else {
                ExprProp::new(self.fresh_ty(), prop.is_mut)
            };
        }

        if matches!(op, UnOp::Mut | UnOp::Ref) {
            let captured = self
                .find_base_binding(*lhs)
                .is_some_and(|binding| self.env.binding_is_capture(binding));
            let deferred = self.place_check_requires_deferred(*lhs);
            let place = self.current_expr_place(*lhs);
            if deferred {
                self.pending_place_checks.push(PendingPlaceCheck::Borrow {
                    expr,
                    source: *lhs,
                    kind: match op {
                        UnOp::Mut => BorrowKind::Mut,
                        UnOp::Ref => BorrowKind::Ref,
                        _ => unreachable!(),
                    },
                    captured,
                });
            } else if place.is_none() {
                self.push_diag(BodyDiag::BorrowFromNonPlace {
                    primary: expr.span(self.body()).into(),
                });
                return ExprProp::invalid(self.db);
            }

            let place_ty = prop
                .ty
                .as_capability(self.db)
                .map(|(_, inner)| inner)
                .unwrap_or(prop.ty);
            let borrow_provider = place
                .as_ref()
                .and_then(|place| self.concrete_borrow_provider_for_place(place));

            return match op {
                UnOp::Ref => ExprProp {
                    ty: TyId::borrow_ref_of(self.db, place_ty),
                    is_mut: false,
                    binding: None,
                    borrow_provider,
                    path_read_semantics: None,
                    value_access: ValueAccess::Infer,
                },
                UnOp::Mut => {
                    if !deferred
                        && captured
                        && !self.place_reaches_mut_capability(
                            place
                                .as_ref()
                                .expect("non-deferred borrow source must be a place"),
                        )
                    {
                        let binding = self.find_base_binding(*lhs);
                        self.push_diag(BodyDiag::BorrowMutFromCapturedBinding {
                            primary: expr.span(self.body()).into(),
                            binding: binding.map(|binding| {
                                (binding.binding_name(&self.env), binding.def_span(&self.env))
                            }),
                        });
                        return ExprProp::invalid(self.db);
                    }
                    if !deferred && !prop.is_mut {
                        let binding = self.find_base_binding(*lhs).map(|binding| {
                            (binding.binding_name(&self.env), binding.def_span(&self.env))
                        });
                        self.push_diag(BodyDiag::CannotBorrowMut {
                            primary: expr.span(self.body()).into(),
                            binding,
                        });
                        return ExprProp::invalid(self.db);
                    }
                    ExprProp {
                        ty: TyId::borrow_mut_of(self.db, place_ty),
                        is_mut: true,
                        binding: None,
                        borrow_provider,
                        path_read_semantics: None,
                        value_access: ValueAccess::Infer,
                    }
                }
                _ => unreachable!(),
            };
        }

        let base_ty = prop.ty.base_ty(self.db);
        if base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if *op == UnOp::Plus {
            if prop.ty.is_integral(self.db) {
                return prop;
            }
            let diag = BodyDiag::UnsupportedUnaryPlus(expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        let lhs_ty = self.copy_inner_from_borrow(prop.ty).unwrap_or(prop.ty);
        if lhs_ty != prop.ty {
            self.unify_ty(Typeable::Expr(*lhs, prop.clone()), lhs_ty, lhs_ty);
        }

        self.check_ops_trait(expr, lhs_ty, op, None, Some(expected))
    }

    fn check_cast(
        &mut self,
        expr: ExprId,
        inner_expr: ExprId,
        target_ty: Partial<crate::hir_def::TypeId<'db>>,
    ) -> ExprProp<'db> {
        let inner_prop = self.check_expr_unknown(inner_expr);
        if inner_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let Some(hir_target_ty) = target_ty.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let span = expr.span(self.body()).into_cast_expr().ty();
        let target_ty = self.lower_ty(hir_target_ty, span, true);
        if target_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let (from, to) = self.normalized_cast_types(inner_prop.ty, target_ty);

        if from == to {
            return ExprProp::new(to, true);
        }

        if let Partial::Present(Expr::Lit(LitKind::Int(int_id))) =
            inner_expr.data(self.db, self.body())
        {
            let value = int_id.data(self.db);
            if to.is_string(self.db) && self.int_literal_fits_in_ty(value, TyId::u256(self.db)) {
                let _ = self.table.unify(from, TyId::u256(self.db));
                return ExprProp::new(to, true);
            }

            if self.int_literal_fits_in_ty(value, to) {
                // Unify the literal's type variable with the target leaf type
                // so it doesn't remain unresolved.
                let leaf = self.peel_transparent_newtypes(to);
                let _ = self.table.unify(from, leaf);
                return ExprProp::new(to, true);
            }

            let leaf = self.peel_transparent_newtypes(to);
            // Unify to prevent a spurious "type annotation needed" error.
            let _ = self.table.unify(from, leaf);
            let diag = BodyDiag::InvalidCast {
                primary: expr.span(self.body()).into(),
                from,
                to,
                hint: Some(format!(
                    "integer literal `{}` is not representable in `{}`",
                    value,
                    leaf.pretty_print(self.db),
                )),
            };
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // Fail if the source type is unknown.
        if from.has_var(self.db) || to.has_var(self.db) {
            self.env.register_pending_cast(PendingCast {
                expr,
                inner: inner_expr,
                target: target_ty,
            });
            return ExprProp::new(to, true);
        }

        self.check_known_cast(expr, inner_expr, from, to)
    }

    fn normalized_cast_types(
        &mut self,
        source: TyId<'db>,
        target: TyId<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        let mut from = normalize_ty(self.db, source, self.env.scope(), self.env.assumptions());
        let to = normalize_ty(self.db, target, self.env.scope(), self.env.assumptions());

        // Casts operate on values, so for Copy capabilities treat the source as
        // the inner value type. This allows widening/narrowing checks such as
        // `(selector as u256)` when `selector` comes from a view parameter.
        if let Some((_, inner)) = from.as_capability(self.db)
            && self.ty_is_copy(inner)
        {
            from = inner;
        }
        (from, to)
    }

    fn check_known_cast(
        &mut self,
        expr: ExprId,
        inner_expr: ExprId,
        from: TyId<'db>,
        to: TyId<'db>,
    ) -> ExprProp<'db> {
        if from == to
            || self.is_lossless_cast(from, to)
            || self.is_provably_lossless_cast_expr(inner_expr, from, to)
        {
            return ExprProp::new(to, true);
        }

        // Check if the cast failed due to invisible struct fields.
        let hint = if self.is_single_field_struct_with_invisible_field(from)
            || self.is_single_field_struct_with_invisible_field(to)
        {
            Some("cast is not allowed because the struct field is not `pub`".to_string())
        } else {
            None
        };

        let diag = BodyDiag::InvalidCast {
            primary: expr.span(self.body()).into(),
            from,
            to,
            hint,
        };
        self.push_diag(diag);
        ExprProp::invalid(self.db)
    }

    // Allow eg `(some_u256 >> 224) as u32`
    fn is_provably_lossless_cast_expr(&self, expr: ExprId, from: TyId<'db>, to: TyId<'db>) -> bool {
        let body = self.body();
        if let Some((false, from_bits)) =
            self.prim_int_signed_bits(self.peel_transparent_newtypes(from))
            && let Some((false, to_bits)) =
                self.prim_int_signed_bits(self.peel_transparent_newtypes(to))
            && let Partial::Present(Expr::Bin(_, rhs, BinOp::Arith(ArithBinOp::RShift))) =
                expr.data(self.db, body)
            && let Partial::Present(Expr::Lit(LitKind::Int(shift_int))) = rhs.data(self.db, body)
            && let Some(shift) = shift_int.data(self.db).to_usize()
        {
            to_bits >= from_bits.saturating_sub(shift)
        } else {
            false
        }
    }

    fn is_lossless_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        if from == to {
            return true;
        }

        let from_leaf = self.peel_transparent_newtypes(from);
        let to_leaf = self.peel_transparent_newtypes(to);

        if from_leaf == to_leaf {
            return true;
        }

        if from_leaf.is_bool(self.db) {
            return self.prim_int_signed_bits(to_leaf).is_some();
        }

        if to_leaf.is_bool(self.db) {
            return false;
        }

        if self.is_string_word_cast(from_leaf, to_leaf) {
            return true;
        }

        self.is_lossless_int_cast(from_leaf, to_leaf)
    }

    fn is_string_word_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        (from.is_string(self.db) && self.is_plain_u256(to))
            || (self.is_plain_u256(from) && to.is_string(self.db))
    }

    fn is_plain_u256(&self, ty: TyId<'db>) -> bool {
        matches!(
            ty.base_ty(self.db).data(self.db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256))
        )
    }

    fn transparent_newtype_field_ty(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        if ty.is_tuple(self.db) {
            let field_tys = ty.field_types(self.db);
            return (field_tys.len() == 1).then(|| field_tys[0]);
        }

        if ty.is_struct(self.db) {
            let field_tys = ty.field_types(self.db);
            if field_tys.len() != 1 {
                return None;
            }

            // Reject cast if the struct field is not visible from the current scope.
            if self.is_single_field_struct_with_invisible_field(ty) {
                return None;
            }

            return Some(field_tys[0]);
        }

        None
    }

    /// Returns `true` if `ty` is a single-field struct whose field is not visible
    /// from the current scope.
    fn is_single_field_struct_with_invisible_field(&self, ty: TyId<'db>) -> bool {
        if !ty.is_struct(self.db) {
            return false;
        }
        let field_tys = ty.field_types(self.db);
        if field_tys.len() != 1 {
            return false;
        }
        let Some(adt_def) = ty.adt_def(self.db) else {
            return false;
        };
        let AdtRef::Struct(s) = adt_def.adt_ref(self.db) else {
            return false;
        };
        let field_scope = ScopeId::Field(FieldParent::Struct(s), 0);
        !is_scope_visible_from(self.db, field_scope, self.env.scope())
    }

    fn peel_transparent_newtypes(&self, mut ty: TyId<'db>) -> TyId<'db> {
        while let Some(inner) = self.transparent_newtype_field_ty(ty) {
            ty = inner;
        }
        ty
    }

    fn prim_int_signed_bits(&self, ty: TyId<'db>) -> Option<(bool, usize)> {
        let base = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Prim(prim)) = base.data(self.db) else {
            return None;
        };
        let bits = prim_int_bits(*prim)?;
        let signed = matches!(
            prim,
            PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Isize
        );
        Some((signed, bits))
    }

    fn is_lossless_int_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        let Some((from_signed, from_bits)) = self.prim_int_signed_bits(from) else {
            return false;
        };
        let Some((to_signed, to_bits)) = self.prim_int_signed_bits(to) else {
            return false;
        };

        match (from_signed, to_signed) {
            (false, false) => to_bits >= from_bits,
            (true, true) => to_bits >= from_bits,
            (false, true) => to_bits > from_bits,
            (true, false) => false,
        }
    }

    fn int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        let leaf = self.peel_transparent_newtypes(target_ty);
        let Some((signed, bits)) = self.prim_int_signed_bits(leaf) else {
            return false;
        };

        if signed {
            let max = (BigUint::from(1u8) << (bits - 1)) - BigUint::from(1u8);
            value <= &max
        } else {
            let max = (BigUint::from(1u8) << bits) - BigUint::from(1u8);
            value <= &max
        }
    }

    fn negated_int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        let leaf = self.peel_transparent_newtypes(target_ty);
        let Some((signed, bits)) = self.prim_int_signed_bits(leaf) else {
            return false;
        };
        if !signed {
            return false;
        }

        let max = BigUint::from(1u8) << (bits - 1);
        value <= &max
    }

    fn check_binary(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
        expected: TyId<'db>,
        assignment_payload_hint: Option<TyId<'db>>,
    ) -> ExprProp<'db> {
        // Logical operands must be bools
        if let BinOp::Logical(_) = op {
            let bool = TyId::bool(self.db);
            let lhs = self.check_expr(lhs_expr, bool);
            let rhs = self.check_expr(rhs_expr, bool);
            return if lhs.ty.is_bool(self.db) && rhs.ty.is_bool(self.db) {
                ExprProp::new(bool, true)
            } else {
                ExprProp::invalid(self.db)
            };
        }

        // Range expressions construct Range types directly
        if matches!(op, BinOp::Arith(ArithBinOp::Range)) {
            return self.check_range_expr(expr, lhs_expr, rhs_expr);
        }

        let lhs = self.check_expr_unknown(lhs_expr);
        if lhs.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let lhs_place_ty = lhs
            .ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs.ty);
        if lhs_place_ty.is_integral_var(self.db)
            && matches!(op, BinOp::Arith(arith) if arith != ArithBinOp::Range)
            && let Some(payload) = assignment_payload_hint
        {
            self.try_apply_assignment_operator_payload_hint(lhs_place_ty, payload);
        }

        if matches!(op, BinOp::Index) && lhs_place_ty.is_array(self.db) {
            return self.check_array_index(lhs_expr, rhs_expr, &lhs, lhs_place_ty, Some(expected));
        } else if lhs_place_ty.is_integral_var(self.db)
            || lhs_place_ty.base_ty(self.db).is_ty_var(self.db)
        {
            // Defer operator selection until later call sites or contextual closure bounds
            // have constrained an inferred parameter payload.
            if lhs_place_ty.is_integral_var(self.db) {
                self.check_expr(rhs_expr, lhs.ty);
            } else {
                self.check_expr_unknown(rhs_expr);
            }
            self.env
                .register_pending_primitive_op(PendingPrimitiveOp::Binary {
                    expr,
                    lhs: lhs_expr,
                    rhs: rhs_expr,
                    op,
                });

            if matches!(op, BinOp::Comp(_)) {
                return ExprProp::new(TyId::bool(self.db), true);
            }

            return if lhs_place_ty.is_integral_var(self.db) {
                lhs
            } else {
                ExprProp::new(self.fresh_ty(), lhs.is_mut)
            };
        }

        // Fail if lhs ty is unknown
        if lhs_place_ty.base_ty(self.db).is_ty_var(self.db) {
            self.check_expr_unknown(rhs_expr);
            let diag = BodyDiag::TypeMustBeKnown(lhs_expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        let lhs_ty = self.copy_inner_from_borrow(lhs.ty).unwrap_or(lhs.ty);
        if lhs_ty != lhs.ty {
            self.unify_ty(Typeable::Expr(lhs_expr, lhs.clone()), lhs_ty, lhs_ty);
        }

        self.check_ops_trait(expr, lhs_ty, &op, Some(rhs_expr), Some(expected))
    }

    fn try_apply_assignment_operator_payload_hint(
        &mut self,
        operand_ty: TyId<'db>,
        payload_ty: TyId<'db>,
    ) {
        let snapshot = self.snapshot_state();
        if self.table.unify(operand_ty, payload_ty).is_ok() {
            self.commit_state(snapshot);
        } else {
            self.rollback_state(snapshot);
        }
    }

    fn check_array_index(
        &mut self,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        lhs: &ExprProp<'db>,
        lhs_place_ty: TyId<'db>,
        contextual_expected: Option<TyId<'db>>,
    ) -> ExprProp<'db> {
        // Built-in array indexing (TODO: move to trait impl).
        let args = lhs_place_ty.generic_args(self.db);
        let elem_ty = args[0];
        let index_ty = args[1].const_ty_ty(self.db).unwrap();
        self.check_or_constrain_expr_to_expected(rhs_expr, index_ty);
        if let Some(index) = self.try_eval_static_int(rhs_expr, index_ty)
            && let Some(len) = lhs_place_ty.array_len(self.db)
            && index.data(self.db) >= &BigUint::from(len)
        {
            self.push_diag(BodyDiag::ArrayIndexOutOfBounds {
                primary: rhs_expr.span(self.body()).into(),
                index,
                len,
            })
        }
        let mut result =
            if let Some(projected) = self.contract_field_projected_index_ty(lhs_expr, rhs_expr) {
                let projected = self.table.fold_ty(self.db, projected);
                let is_mut = self.projected_place_mutability(lhs.is_mut, projected);
                ExprProp::new(projected, is_mut)
            } else {
                let is_mut = self.projected_place_mutability(lhs.is_mut, elem_ty);
                ExprProp::new(elem_ty, is_mut)
            };
        if let Some(expected) = contextual_expected
            && self.closure_expectation_for_type(expected).is_some()
            && let Some(base_expected) =
                self.replay_index_base_expected_ty(lhs_expr, result.ty, expected)
        {
            let (_, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(lhs_expr, base_expected);
            if outcome == ClosureReplayOutcome::Replayed {
                result.ty = self.normalize_ty(expected);
            }
        }
        result
    }

    fn projected_place_mutability(&mut self, inherited: bool, ty: TyId<'db>) -> bool {
        match self.normalize_ty(ty).as_capability(self.db) {
            Some((CapabilityKind::Mut, _)) => true,
            Some((CapabilityKind::Ref, _)) => false,
            Some((CapabilityKind::View, _)) | None => inherited,
        }
    }

    pub(super) fn resolve_pending_method_lookup(
        &mut self,
        pending: &PendingMethodLookup<'db>,
    ) -> PendingPrimitiveOpResolution {
        self.resolve_pending_method_lookup_inner(pending, true)
    }

    fn resolve_pending_method_lookup_inner(
        &mut self,
        pending: &PendingMethodLookup<'db>,
        allow_contextual_replay: bool,
    ) -> PendingPrimitiveOpResolution {
        if self.env.callable_expr(pending.expr).is_some() {
            return PendingPrimitiveOpResolution::Done;
        }
        let Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) =
            pending.expr.data(self.db, self.body())
        else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(mut receiver_prop) = self.env.typed_expr(*receiver) else {
            return PendingPrimitiveOpResolution::Done;
        };
        receiver_prop.ty = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            receiver_prop.ty.fold_with(self.db, &mut prober)
        };
        if receiver_prop.ty.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }
        let (selected_receiver_ty, canonical_r_ty, candidate) =
            self.select_method_call_candidate(*receiver, &receiver_prop, pending.method_name, None);
        let candidate = match candidate {
            Ok(candidate) => candidate,
            Err(MethodSelectionError::ReceiverTypeMustBeKnown) => {
                return PendingPrimitiveOpResolution::Pending;
            }
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                let candidates = ambiguous
                    .candidates
                    .into_iter()
                    .map(|candidate| {
                        let inst =
                            canonical_r_ty.extract_solution(&mut self.table, candidate.cand.inst);
                        let inst =
                            self.specialize_same_trait_method_inst(pending.method_name, inst);
                        super::env::PendingMethodCandidate {
                            inst,
                            method: candidate.cand.method,
                            needs_confirmation: candidate.needs_confirmation,
                            priority: 0,
                        }
                    })
                    .collect();
                self.env.register_pending_method(super::env::PendingMethod {
                    expr: pending.expr,
                    recv_ty: selected_receiver_ty,
                    method_name: pending.method_name,
                    candidates,
                    span: pending.span.clone(),
                    callee_is_receiver: false,
                });
                return PendingPrimitiveOpResolution::Resolved;
            }
            Err(err) => {
                self.push_diag(body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(selected_receiver_ty, receiver.span(self.body()).into()),
                    Spanned::new(pending.method_name, pending.span.clone()),
                ));
                return PendingPrimitiveOpResolution::Done;
            }
        };

        let transaction = (allow_contextual_replay
            && self
                .env
                .deferred_closure_replay_context(pending.expr)
                .is_some())
        .then(|| self.snapshot_closure_replay_transaction());
        let resolved = self.check_selected_method_call(
            pending.expr,
            *receiver,
            pending.method_name,
            *generic_args,
            args,
            receiver_prop,
            selected_receiver_ty,
            canonical_r_ty,
            candidate,
            true,
            expr_prop.ty,
            transaction.is_some(),
        );
        if let Some(transaction) = transaction {
            let (resolved, replay_satisfied) =
                self.replay_deferred_expr_with_closure_context(pending.expr, resolved);
            if replay_satisfied
                && !resolved.ty.has_invalid(self.db)
                && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
            {
                if let Some(callable) = self.env.callable_expr(pending.expr).cloned() {
                    callable.enqueue_constraints(self, pending.expr, pending.span.clone());
                }
                self.env
                    .consume_deferred_closure_replay_context(pending.expr);
                self.commit_closure_replay_transaction(transaction);
                return PendingPrimitiveOpResolution::Resolved;
            }
            self.rollback_closure_replay_transaction(transaction);
            let outcome = self.resolve_pending_method_lookup_inner(pending, false);
            return self.clear_terminal_deferred_closure_replay_context(pending.expr, outcome);
        }

        if resolved.ty.has_invalid(self.db)
            || !self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
        {
            return PendingPrimitiveOpResolution::Done;
        }
        if let Some(callable) = self.env.callable_expr(pending.expr).cloned() {
            callable.enqueue_constraints(self, pending.expr, pending.span.clone());
        }
        PendingPrimitiveOpResolution::Resolved
    }

    pub(super) fn resolve_pending_callable_lookup(
        &mut self,
        pending: PendingCallableLookup,
    ) -> PendingPrimitiveOpResolution {
        self.resolve_pending_callable_lookup_inner(pending, true)
    }

    fn resolve_pending_callable_lookup_inner(
        &mut self,
        pending: PendingCallableLookup,
        allow_contextual_replay: bool,
    ) -> PendingPrimitiveOpResolution {
        if self.env.callable_expr(pending.expr).is_some() {
            return PendingPrimitiveOpResolution::Done;
        }
        let Partial::Present(Expr::Call(callee, args)) = pending.expr.data(self.db, self.body())
        else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(mut callee_prop) = self.env.typed_expr(*callee) else {
            return PendingPrimitiveOpResolution::Done;
        };
        callee_prop.ty = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            callee_prop.ty.fold_with(self.db, &mut prober)
        };
        if callee_prop.ty.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }
        if !callee_prop.ty.has_var(self.db)
            && callee_prop
                .ty
                .base_ty(self.db)
                .as_closure(self.db)
                .is_none()
        {
            let resolved =
                self.defer_callable_value_call(pending.expr, *callee, args, callee_prop, true);
            return if resolved.ty.has_invalid(self.db) {
                PendingPrimitiveOpResolution::Done
            } else {
                PendingPrimitiveOpResolution::Resolved
            };
        }

        let selection = match self.select_callable_value_candidate(*callee, &callee_prop) {
            Ok(Some(selection)) => selection,
            Ok(None) if callee_prop.ty.has_var(self.db) => {
                return PendingPrimitiveOpResolution::Pending;
            }
            Ok(None) => {
                self.push_diag(BodyDiag::NotCallable(
                    callee.span(self.body()).into(),
                    callee_prop.ty,
                ));
                self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                return PendingPrimitiveOpResolution::Done;
            }
            Err((_, _, MethodSelectionError::ReceiverTypeMustBeKnown)) => {
                return PendingPrimitiveOpResolution::Pending;
            }
            Err((method_name, selected_receiver_ty, err)) => {
                self.push_diag(body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(selected_receiver_ty, callee.span(self.body()).into()),
                    Spanned::new(
                        method_name,
                        pending
                            .expr
                            .span(self.body())
                            .into_call_expr()
                            .callee()
                            .into(),
                    ),
                ));
                self.env.type_expr(pending.expr, ExprProp::invalid(self.db));
                return PendingPrimitiveOpResolution::Done;
            }
        };

        let transaction = (allow_contextual_replay
            && self
                .env
                .deferred_closure_replay_context(pending.expr)
                .is_some())
        .then(|| self.snapshot_closure_replay_transaction());
        let (selected_receiver_ty, canonical_r_ty, candidate) = selection;
        let resolved = self.check_selected_callable_value_call(
            pending.expr,
            *callee,
            args,
            callee_prop,
            selected_receiver_ty,
            canonical_r_ty,
            candidate,
            expr_prop.ty,
            true,
        );
        if let Some(transaction) = transaction {
            let (resolved, replay_satisfied) =
                self.replay_deferred_expr_with_closure_context(pending.expr, resolved);
            if replay_satisfied
                && !resolved.ty.has_invalid(self.db)
                && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
            {
                if let Some(callable) = self.env.callable_expr(pending.expr).cloned() {
                    callable.enqueue_constraints(
                        self,
                        pending.expr,
                        pending
                            .expr
                            .span(self.body())
                            .into_call_expr()
                            .callee()
                            .into(),
                    );
                }
                self.env
                    .consume_deferred_closure_replay_context(pending.expr);
                self.commit_closure_replay_transaction(transaction);
                return PendingPrimitiveOpResolution::Resolved;
            }
            self.rollback_closure_replay_transaction(transaction);
            let outcome = self.resolve_pending_callable_lookup_inner(pending, false);
            return self.clear_terminal_deferred_closure_replay_context(pending.expr, outcome);
        }

        if resolved.ty.has_invalid(self.db)
            || !self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
        {
            return PendingPrimitiveOpResolution::Done;
        }
        if let Some(callable) = self.env.callable_expr(pending.expr).cloned() {
            callable.enqueue_constraints(
                self,
                pending.expr,
                pending
                    .expr
                    .span(self.body())
                    .into_call_expr()
                    .callee()
                    .into(),
            );
        }
        PendingPrimitiveOpResolution::Resolved
    }

    pub(super) fn resolve_pending_primitive_op(
        &mut self,
        pending: &PendingPrimitiveOp,
    ) -> PendingPrimitiveOpResolution {
        self.resolve_pending_primitive_op_inner(pending, true)
    }

    fn resolve_pending_primitive_op_inner(
        &mut self,
        pending: &PendingPrimitiveOp,
        allow_contextual_replay: bool,
    ) -> PendingPrimitiveOpResolution {
        if self.env.callable_expr(pending.expr()).is_some() {
            return PendingPrimitiveOpResolution::Done;
        }

        let Some(expr_prop) = self.env.typed_expr(pending.expr()) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let transaction = (allow_contextual_replay
            && self
                .env
                .deferred_closure_replay_context(pending.expr())
                .is_some())
        .then(|| self.snapshot_closure_replay_transaction());
        let resolved = match self.compute_pending_primitive_op(pending) {
            Ok(resolved) => resolved,
            Err(outcome) => {
                if let Some(transaction) = transaction {
                    self.rollback_closure_replay_transaction(transaction);
                    return match outcome {
                        PendingPrimitiveOpResolution::Pending => outcome,
                        PendingPrimitiveOpResolution::Resolved
                        | PendingPrimitiveOpResolution::Done => {
                            let outcome = self.resolve_pending_primitive_op_inner(pending, false);
                            self.clear_terminal_deferred_closure_replay_context(
                                pending.expr(),
                                outcome,
                            )
                        }
                    };
                }
                return outcome;
            }
        };

        if let Some(transaction) = transaction {
            let (resolved, replay_satisfied) =
                self.replay_deferred_expr_with_closure_context(pending.expr(), resolved);
            if replay_satisfied
                && !resolved.ty.has_invalid(self.db)
                && self.reconcile_deferred_expr_prop(pending.expr(), expr_prop, resolved)
            {
                self.env
                    .consume_deferred_closure_replay_context(pending.expr());
                self.commit_closure_replay_transaction(transaction);
                return PendingPrimitiveOpResolution::Resolved;
            }
            self.rollback_closure_replay_transaction(transaction);
            let outcome = self.resolve_pending_primitive_op_inner(pending, false);
            return self.clear_terminal_deferred_closure_replay_context(pending.expr(), outcome);
        }

        if !resolved.ty.has_invalid(self.db)
            && self.reconcile_deferred_expr_prop(pending.expr(), expr_prop, resolved)
        {
            PendingPrimitiveOpResolution::Resolved
        } else {
            PendingPrimitiveOpResolution::Done
        }
    }

    fn compute_pending_primitive_op(
        &mut self,
        pending: &PendingPrimitiveOp,
    ) -> Result<ExprProp<'db>, PendingPrimitiveOpResolution> {
        Ok(match pending {
            PendingPrimitiveOp::Unary { expr, inner, op } => {
                let Some(inner_prop) = self.env.typed_expr(*inner) else {
                    return Err(PendingPrimitiveOpResolution::Done);
                };
                let operand_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    inner_prop.ty.fold_with(self.db, &mut prober)
                };
                let operand_ty = operand_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(operand_ty);
                let operand_ty = self.normalize_ty(operand_ty);
                if operand_ty.has_invalid(self.db) {
                    return Err(PendingPrimitiveOpResolution::Done);
                }
                if operand_ty.is_integral_var(self.db)
                    || operand_ty.base_ty(self.db).is_ty_var(self.db)
                {
                    return Err(PendingPrimitiveOpResolution::Pending);
                }
                if matches!(op, UnOp::Plus) {
                    if operand_ty.is_integral(self.db) {
                        ExprProp::new(operand_ty, inner_prop.is_mut)
                    } else {
                        self.push_diag(BodyDiag::UnsupportedUnaryPlus(
                            expr.span(self.body()).into(),
                        ));
                        return Err(PendingPrimitiveOpResolution::Done);
                    }
                } else if matches!(op, UnOp::Minus)
                    && let Some(int_id) = self.try_get_literal_int(*inner)
                {
                    let literal = int_id.data(self.db);
                    if self.negated_int_literal_fits_in_ty(literal, operand_ty) {
                        ExprProp::new(operand_ty, inner_prop.is_mut)
                    } else if self
                        .peel_transparent_newtypes(operand_ty)
                        .base_ty(self.db)
                        .is_prim(self.db)
                    {
                        self.push_diag(BodyDiag::IntLiteralOutOfRange {
                            primary: expr.span(self.body()).into(),
                            literal: format!("-{literal}"),
                            ty: operand_ty,
                        });
                        return Err(PendingPrimitiveOpResolution::Done);
                    } else {
                        self.check_ops_trait(*expr, operand_ty, op, None, None)
                    }
                } else {
                    self.check_ops_trait(*expr, operand_ty, op, None, None)
                }
            }
            PendingPrimitiveOp::Binary { expr, lhs, rhs, op } => {
                let Some(lhs_prop) = self.env.typed_expr(*lhs) else {
                    return Err(PendingPrimitiveOpResolution::Done);
                };
                let Some(rhs_prop) = self.env.typed_expr(*rhs) else {
                    return Err(PendingPrimitiveOpResolution::Done);
                };
                let lhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    lhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let rhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    rhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let lhs_ty = lhs_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(lhs_ty);
                let lhs_ty = self.normalize_ty(lhs_ty);
                let rhs_ty = rhs_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(rhs_ty);
                let rhs_ty = self.normalize_ty(rhs_ty);
                if lhs_ty.has_invalid(self.db) || rhs_ty.has_invalid(self.db) {
                    return Err(PendingPrimitiveOpResolution::Done);
                }
                if matches!(op, BinOp::Index) && lhs_ty.is_array(self.db) {
                    self.check_array_index(*lhs, *rhs, &lhs_prop, lhs_ty, None)
                } else {
                    if lhs_ty.is_integral_var(self.db) || lhs_ty.base_ty(self.db).is_ty_var(self.db)
                    {
                        return Err(PendingPrimitiveOpResolution::Pending);
                    }
                    self.check_ops_trait(*expr, lhs_ty, op, Some(*rhs), None)
                }
            }
            PendingPrimitiveOp::AugAssign { expr, lhs, rhs, op } => {
                let Some(lhs_prop) = self.env.typed_expr(*lhs) else {
                    return Err(PendingPrimitiveOpResolution::Done);
                };
                let Some(rhs_prop) = self.env.typed_expr(*rhs) else {
                    return Err(PendingPrimitiveOpResolution::Done);
                };
                let lhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    lhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let rhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    rhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let lhs_ty = lhs_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(lhs_ty);
                let lhs_ty = self.normalize_ty(lhs_ty);
                if lhs_ty.has_invalid(self.db) || rhs_ty.has_invalid(self.db) {
                    return Err(PendingPrimitiveOpResolution::Done);
                }
                if lhs_ty.is_integral_var(self.db) || lhs_ty.base_ty(self.db).is_ty_var(self.db) {
                    return Err(PendingPrimitiveOpResolution::Pending);
                }
                self.check_ops_trait(*expr, lhs_ty, &AugAssignOp(*op), Some(*rhs), None)
            }
        })
    }

    pub(super) fn resolve_pending_field(
        &mut self,
        pending: &PendingField<'db>,
    ) -> PendingPrimitiveOpResolution {
        self.resolve_pending_field_inner(pending, true)
    }

    fn resolve_pending_field_inner(
        &mut self,
        pending: &PendingField<'db>,
        allow_contextual_replay: bool,
    ) -> PendingPrimitiveOpResolution {
        let Some(lhs_prop) = self.env.typed_expr(pending.lhs) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let lhs_ty = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            lhs_prop.ty.fold_with(self.db, &mut prober)
        };
        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        let lhs_place_ty = self.normalize_ty(lhs_place_ty);
        if lhs_place_ty.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }
        if lhs_place_ty.base_ty(self.db).is_ty_var(self.db) {
            return PendingPrimitiveOpResolution::Pending;
        }
        let transaction = (allow_contextual_replay
            && self
                .env
                .deferred_closure_replay_context(pending.expr)
                .is_some())
        .then(|| self.snapshot_closure_replay_transaction());
        let resolved = self.check_known_field(
            pending.expr,
            pending.lhs,
            pending.field,
            lhs_prop,
            lhs_place_ty,
        );
        if let Some(transaction) = transaction {
            let (resolved, replay_satisfied) =
                self.replay_deferred_expr_with_closure_context(pending.expr, resolved);
            if replay_satisfied
                && !resolved.ty.has_invalid(self.db)
                && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
            {
                self.env
                    .consume_deferred_closure_replay_context(pending.expr);
                self.commit_closure_replay_transaction(transaction);
                return PendingPrimitiveOpResolution::Resolved;
            }
            self.rollback_closure_replay_transaction(transaction);
            let outcome = self.resolve_pending_field_inner(pending, false);
            return self.clear_terminal_deferred_closure_replay_context(pending.expr, outcome);
        }

        if !resolved.ty.has_invalid(self.db)
            && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
        {
            PendingPrimitiveOpResolution::Resolved
        } else {
            PendingPrimitiveOpResolution::Done
        }
    }

    pub(super) fn resolve_pending_cast(
        &mut self,
        pending: &PendingCast<'db>,
    ) -> PendingPrimitiveOpResolution {
        self.resolve_pending_cast_inner(pending, true)
    }

    fn resolve_pending_cast_inner(
        &mut self,
        pending: &PendingCast<'db>,
        allow_contextual_replay: bool,
    ) -> PendingPrimitiveOpResolution {
        let Some(inner_prop) = self.env.typed_expr(pending.inner) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let source = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            inner_prop.ty.fold_with(self.db, &mut prober)
        };
        let target = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            pending.target.fold_with(self.db, &mut prober)
        };
        let (from, to) = self.normalized_cast_types(source, target);
        if from.has_invalid(self.db) || to.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }
        if from.has_var(self.db) || to.has_var(self.db) {
            if allow_contextual_replay
                && self
                    .env
                    .deferred_closure_replay_context(pending.expr)
                    .is_some()
            {
                let transaction = self.snapshot_closure_replay_transaction();
                let (replayed, replay_satisfied) = self.replay_deferred_expr_with_closure_context(
                    pending.expr,
                    ExprProp::new(to, true),
                );
                if replay_satisfied {
                    let replayed_source = self
                        .env
                        .typed_expr(pending.inner)
                        .map(|prop| prop.ty)
                        .unwrap_or(source);
                    let (replayed_from, replayed_to) =
                        self.normalized_cast_types(replayed_source, replayed.ty);
                    if !replayed_from.has_var(self.db)
                        && !replayed_to.has_var(self.db)
                        && !replayed_from.has_invalid(self.db)
                        && !replayed_to.has_invalid(self.db)
                    {
                        let checked = self.check_known_cast(
                            pending.expr,
                            pending.inner,
                            replayed_from,
                            replayed_to,
                        );
                        if !checked.ty.has_invalid(self.db)
                            && self.reconcile_deferred_expr_prop(
                                pending.expr,
                                expr_prop.clone(),
                                checked,
                            )
                        {
                            self.env
                                .consume_deferred_closure_replay_context(pending.expr);
                            self.commit_closure_replay_transaction(transaction);
                            return PendingPrimitiveOpResolution::Resolved;
                        }
                    }
                }
                self.rollback_closure_replay_transaction(transaction);
            }
            return PendingPrimitiveOpResolution::Pending;
        }

        let transaction = (allow_contextual_replay
            && self
                .env
                .deferred_closure_replay_context(pending.expr)
                .is_some())
        .then(|| self.snapshot_closure_replay_transaction());
        let resolved = self.check_known_cast(pending.expr, pending.inner, from, to);
        if let Some(transaction) = transaction {
            let (resolved, replay_satisfied) =
                self.replay_deferred_expr_with_closure_context(pending.expr, resolved);
            if replay_satisfied
                && !resolved.ty.has_invalid(self.db)
                && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
            {
                self.env
                    .consume_deferred_closure_replay_context(pending.expr);
                self.commit_closure_replay_transaction(transaction);
                return PendingPrimitiveOpResolution::Resolved;
            }
            self.rollback_closure_replay_transaction(transaction);
            let outcome = self.resolve_pending_cast_inner(pending, false);
            return self.clear_terminal_deferred_closure_replay_context(pending.expr, outcome);
        }

        if !resolved.ty.has_invalid(self.db)
            && self.reconcile_deferred_expr_prop(pending.expr, expr_prop, resolved)
        {
            PendingPrimitiveOpResolution::Resolved
        } else {
            PendingPrimitiveOpResolution::Done
        }
    }

    fn check_let_condition(&mut self, pat: PatId, scrutinee: ExprId) -> ExprProp<'db> {
        let scrutinee_ty = self.fresh_ty();
        let scrutinee_prop = self.check_expr(scrutinee, scrutinee_ty);
        let (pat_expected, mode) = self.destructure_source_mode(scrutinee_prop.ty);
        let layout = self.pattern_layout_context(scrutinee);
        self.check_pat_with_layout(pat, pat_expected, layout.as_ref());
        if let super::PatternDestructureMode::Borrow(kind) = mode {
            self.retype_pattern_bindings_for_borrow(pat, kind);
        }
        if mode == super::PatternDestructureMode::Owned {
            let capture_access = self.pattern_value_capture_access(pat);
            self.record_pattern_value_use(scrutinee, capture_access);
        }

        ExprProp::new(TyId::bool(self.db), true)
    }

    pub(super) fn check_cond(&mut self, cond: CondId) -> ExprProp<'db> {
        let Partial::Present(cond_data) = cond.data(self.db, self.body()) else {
            return ExprProp::invalid(self.db);
        };

        match cond_data {
            Cond::Expr(expr) => self.check_expr(*expr, TyId::bool(self.db)),
            Cond::Let(pat, scrutinee) => self.check_let_condition(*pat, *scrutinee),
            Cond::Bin(lhs, rhs, op) => {
                let lhs = self.check_cond(*lhs);
                match op {
                    LogicalBinOp::And => self.env.flush_pending_bindings(),
                    LogicalBinOp::Or => self.env.clear_pending_bindings(),
                }
                let rhs = self.check_cond(*rhs);
                if lhs.ty.is_bool(self.db) && rhs.ty.is_bool(self.db) {
                    ExprProp::new(TyId::bool(self.db), true)
                } else {
                    ExprProp::invalid(self.db)
                }
            }
        }
    }

    /// Check a range expression `start..end` and return the Range type.
    ///
    /// Both operands must be `usize`. The result type depends on whether bounds
    /// are compile-time constants:
    /// - `Range<Known<S>, Known<E>>` when both are literals (0 words)
    /// - `Range<Known<S>, Unknown>` when only start is literal (1 word)
    /// - `Range<Unknown, Known<E>>` when only end is literal (1 word)
    /// - `Range<Unknown, Unknown>` when neither is literal (2 words)
    fn check_range_expr(
        &mut self,
        _expr: ExprId,
        start_expr: ExprId,
        end_expr: ExprId,
    ) -> ExprProp<'db> {
        let usize_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));

        // Check that both operands are usize
        self.check_expr(start_expr, usize_ty);
        self.check_expr(end_expr, usize_ty);

        // Try to detect if bounds are literal integers
        let start_lit = self.try_get_literal_int(start_expr);
        let end_lit = self.try_get_literal_int(end_expr);

        // Resolve Range types from core library
        match resolve_core_range_types(self.db, self.env.scope()) {
            Some(types) => {
                // Construct appropriate bound types based on constness
                let start_bound =
                    self.make_range_bound(start_lit, types.known, types.unknown, usize_ty);
                let end_bound =
                    self.make_range_bound(end_lit, types.known, types.unknown, usize_ty);

                // Construct Range<StartBound, EndBound>
                let range_s = TyId::app(self.db, types.range, start_bound);
                let range_full = TyId::app(self.db, range_s, end_bound);
                ExprProp::new(range_full, true)
            }
            _ => {
                // Fallback: if Range/Known/Unknown isn't found, return invalid
                // This shouldn't happen in normal usage
                ExprProp::invalid(self.db)
            }
        }
    }

    /// Try to extract a literal integer value from an expression.
    /// Returns `Some(IntegerId)` if the expression is a literal integer, `None` otherwise.
    fn try_get_literal_int(&self, expr: ExprId) -> Option<IntegerId<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Lit(LitKind::Int(int_id)) => Some(*int_id),
            _ => None,
        }
    }

    fn try_eval_static_int(&self, expr: ExprId, expected: TyId<'db>) -> Option<IntegerId<'db>> {
        if let Some(value) = self.try_get_literal_int(expr) {
            return Some(value);
        }
        if let Some(const_ref) = self.env.expr_const_ref(expr)
            && let Some(const_ref) =
                resolve_semantic_const_ref(self.db, const_ref, expected, SemOrigin::Expr(expr))
            && let Ok(value) = eval_const_ref(self.db, const_ref)
            && let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = value.value(self.db)
        {
            let (sign, bytes) = value.to_bytes_be();
            if sign != Sign::Minus {
                return Some(IntegerId::new(self.db, BigUint::from_bytes_be(&bytes)));
            }
        }
        let value = try_eval_const_int_expr(self.db, self.body(), expr, expected)?;
        let (sign, bytes) = value.to_bytes_be();
        (sign != Sign::Minus).then(|| IntegerId::new(self.db, BigUint::from_bytes_be(&bytes)))
    }

    /// Create a range bound type: either `Known<N>` for a literal or `Unknown`.
    fn make_range_bound(
        &self,
        lit: Option<IntegerId<'db>>,
        known_base: TyId<'db>,
        unknown_ty: TyId<'db>,
        usize_ty: TyId<'db>,
    ) -> TyId<'db> {
        match lit {
            Some(int_id) => {
                // Create Known<N> where N is the literal value
                let const_value = EvaluatedConstTy::LitInt(int_id);
                let const_data = ConstTyData::Evaluated(const_value, usize_ty);
                let const_ty = ConstTyId::new(self.db, const_data);
                let const_ty_id = TyId::const_ty(self.db, const_ty);
                TyId::app(self.db, known_base, const_ty_id)
            }
            None => unknown_ty,
        }
    }

    fn forwarded_effect_value_bindings(&self, expr: ExprId) -> FxHashSet<LocalBinding<'db>> {
        fn collect<'db>(
            checker: &TyChecker<'db>,
            expr: ExprId,
            bindings: &mut FxHashSet<LocalBinding<'db>>,
        ) {
            if let Some(place) = checker.env.expr_place(expr) {
                let PlaceBase::Binding(binding) = place.base;
                bindings.insert(binding);
                return;
            }
            let Partial::Present(expr_data) = expr.data(checker.db, checker.body()) else {
                return;
            };
            match expr_data {
                Expr::Block(stmts) => {
                    if let Some(last) = stmts.last()
                        && let Partial::Present(Stmt::Expr(tail)) =
                            last.data(checker.db, checker.body())
                    {
                        collect(checker, *tail, bindings);
                    }
                }
                Expr::With(_, body) | Expr::Cast(body, _) => {
                    collect(checker, *body, bindings);
                }
                Expr::If(_, then_expr, Some(else_expr)) => {
                    collect(checker, *then_expr, bindings);
                    collect(checker, *else_expr, bindings);
                }
                Expr::Match(_, Partial::Present(arms)) => {
                    for arm in arms {
                        collect(checker, arm.body, bindings);
                    }
                }
                _ => {}
            }
        }

        let mut bindings = FxHashSet::default();
        collect(self, expr, &mut bindings);
        bindings
    }

    fn check_with(
        &mut self,
        bindings: &[WithBinding<'db>],
        body_expr: ExprId,
        expected: TyId<'db>,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        self.env.effect_env_mut().push_frame();

        for binding in bindings {
            let value_prop = self.check_expr_unknown(binding.value);

            let is_mut = value_prop
                .binding
                .map(|b| b.is_mut())
                .unwrap_or(value_prop.is_mut);
            let closure_depth = self.env.closure_depth();
            let source_closure_depth = self
                .forwarded_effect_value_bindings(binding.value)
                .into_iter()
                .filter_map(|binding| self.env.binding_closure_depth(binding))
                .min()
                .unwrap_or(closure_depth);

            let provided = ProvidedEffect {
                origin: EffectOrigin::With {
                    value_expr: binding.value,
                },
                closure_depth,
                source_closure_depth,
                ty: self.table.fold_ty(self.db, value_prop.ty),
                is_mut,
                binding: value_prop.binding,
            };

            match binding.key_path {
                Some(key_path) => {
                    if let Some(key_path) = key_path.to_opt() {
                        let folded_provider_ty = self.table.fold_ty(self.db, provided.ty);
                        match self.validate_keyed_with(
                            key_path,
                            ProvidedEffect {
                                ty: folded_provider_ty,
                                ..provided
                            },
                            binding.value.span(self.body()).into(),
                        ) {
                            Ok((witness, commit)) => {
                                let committed = self.apply_effect_commit_plan(commit);
                                debug_assert!(
                                    committed,
                                    "validated keyed `with` binding commit failed"
                                );
                                if !committed {
                                    let barrier = EffectBarrier {
                                        pattern: build_barrier_pattern_for_with_key(self, key_path)
                                            .expect("validated keyed binding should have a barrier pattern"),
                                        reason: self.barrier_reason_for_pattern(
                                            key_path,
                                            witness.key.clone(),
                                            binding.value.span(self.body()).into(),
                                        ),
                                    };
                                    self.insert_effect_barrier(barrier);
                                    continue;
                                }
                                self.env.effect_env_mut().insert_witness(self.db, witness);
                            }
                            Err(barrier) => {
                                self.insert_effect_barrier(*barrier);
                            }
                        }
                    }
                }
                None => {
                    self.env.effect_env_mut().insert_unkeyed(provided);
                }
            }
        }

        let result = if result_discarded {
            self.check_expr_with_discarded_result(body_expr, expected)
        } else if result_expectation.is_capability_assignment() {
            self.check_expr_with_result_context(body_expr, result_expectation, false)
        } else {
            self.check_expr(body_expr, expected)
        };
        self.env.effect_env_mut().pop_frame();
        result
    }

    fn constrain_callable_result_from_expected(&mut self, result: TyId<'db>, expected: TyId<'db>) {
        let result = self.normalize_ty(result);
        let expected = self.normalize_ty(expected);
        if expected.is_ty_var(self.db) {
            return;
        }
        let result = result
            .as_capability(self.db)
            .map_or(result, |(_, payload)| payload);
        let expected = expected
            .as_capability(self.db)
            .map_or(expected, |(_, payload)| payload);
        self.table.unify(result, expected).ok();
    }

    fn check_call(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Call(callee, args) = expr_data else {
            unreachable!()
        };
        let callee_prop = self.check_expr_unknown(*callee);
        if callee_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let mut callable = if matches!(
            callee.data(self.db, self.body()),
            Partial::Present(Expr::Path(..))
        ) && let Some(existing) = self.env.callable_expr(*callee)
        {
            existing.clone()
        } else {
            match Callable::new(
                self.db,
                callee_prop.ty,
                callee.span(self.body()).into(),
                None,
            ) {
                Ok(callable) => callable,
                Err(_) => {
                    return self.check_callable_value_call(
                        expr,
                        *callee,
                        args,
                        callee_prop,
                        expected,
                    );
                }
            }
        };

        let call_span = expr.span(self.body()).into_call_expr();
        self.constrain_callable_result_from_expected(callable.ret_ty(self.db), expected);

        if let Some(kind) = self.code_region_intrinsic_kind(callable.callable_def())
            && args.len() == 1
            && let Some(result) = self.check_code_region_intrinsic(
                expr,
                &mut callable,
                args,
                kind,
                None,
                Some(*callee),
            )
        {
            return result;
        }

        callable.check_args(self, args, call_span.clone().args(), None, false);
        self.specialize_callable_layout_args(&mut callable, None, args);
        if self.call_args_include_closure(args) {
            self.eagerly_process_callable_constraints(
                &callable,
                expr,
                call_span.clone().callee().into(),
            );
        }

        self.check_callable_effects(expr, &mut callable);

        callable.process_constraints(self, expr, call_span.callee().into());

        let ret_ty = callable.ret_ty(self.db);
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        if let Some(kind) = self.const_intrinsic_kind(callable.callable_def()) {
            self.env.register_const_intrinsic(expr, callable, kind);
        } else {
            self.env.register_semantic_call(expr, callable);
        }
        let result = ExprProp::new(normalized_ret_ty, true);
        if !self.closure_type_expectations.is_empty() && self.env.callable_expr(expr).is_some() {
            self.env.type_expr(expr, result.clone());
            let (replayed, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            if outcome == ClosureReplayOutcome::Replayed {
                return replayed;
            }
        }
        result
    }

    fn check_callable_value_call(
        &mut self,
        expr: ExprId,
        callee: ExprId,
        args: &[HirCallArg<'db>],
        callee_prop: ExprProp<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let call_span = expr.span(self.body()).into_call_expr();
        if self
            .normalize_ty(callee_prop.ty)
            .base_ty(self.db)
            .as_closure(self.db)
            .is_none()
        {
            return self.defer_callable_value_call(expr, callee, args, callee_prop, false);
        }
        let selection = match self.select_callable_value_candidate(callee, &callee_prop) {
            Ok(selection) => selection,
            Err((_, _, MethodSelectionError::ReceiverTypeMustBeKnown)) => {
                let ret_ty = self.fresh_ty();
                let typed = ExprProp::new(ret_ty, true);
                self.env.type_expr(expr, typed.clone());
                self.constrain_pending_direct_closure_call(&callee_prop, args, expected);
                self.env
                    .register_pending_callable_lookup(PendingCallableLookup { expr });
                return typed;
            }
            Err((method_name, selected_receiver_ty, err)) => {
                let diag = body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(selected_receiver_ty, callee.span(self.body()).into()),
                    Spanned::new(method_name, call_span.clone().callee().into()),
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        let Some((selected_receiver_ty, canonical_r_ty, candidate)) = selection else {
            self.push_diag(BodyDiag::NotCallable(
                callee.span(self.body()).into(),
                callee_prop.ty,
            ));
            return ExprProp::invalid(self.db);
        };

        self.check_selected_callable_value_call(
            expr,
            callee,
            args,
            callee_prop,
            selected_receiver_ty,
            canonical_r_ty,
            candidate,
            expected,
            false,
        )
    }

    fn defer_callable_value_call(
        &mut self,
        expr: ExprId,
        callee: ExprId,
        args: &[HirCallArg<'db>],
        callee_prop: ExprProp<'db>,
        already_typed: bool,
    ) -> ExprProp<'db> {
        let call_span = expr.span(self.body()).into_call_expr();
        let (selected_receiver_ty, candidates) =
            match self.collect_callable_value_candidates(callee, &callee_prop) {
                Ok(Some(selection)) => selection,
                Ok(None) if callee_prop.ty.has_var(self.db) => {
                    let ret_ty = self
                        .env
                        .typed_expr(expr)
                        .map_or_else(|| self.fresh_ty(), |prop| prop.ty);
                    let typed = ExprProp::new(ret_ty, true);
                    self.env.type_expr(expr, typed.clone());
                    if !already_typed {
                        for arg in args {
                            self.check_expr_unknown(arg.expr);
                        }
                    }
                    self.env
                        .register_pending_callable_lookup(PendingCallableLookup { expr });
                    return typed;
                }
                Ok(None) => {
                    self.push_diag(BodyDiag::NotCallable(
                        callee.span(self.body()).into(),
                        callee_prop.ty,
                    ));
                    return ExprProp::invalid(self.db);
                }
                Err((_, _, MethodSelectionError::ReceiverTypeMustBeKnown)) => {
                    let ret_ty = self
                        .env
                        .typed_expr(expr)
                        .map_or_else(|| self.fresh_ty(), |prop| prop.ty);
                    let typed = ExprProp::new(ret_ty, true);
                    self.env.type_expr(expr, typed.clone());
                    if !already_typed {
                        for arg in args {
                            self.check_expr_unknown(arg.expr);
                        }
                    }
                    self.env
                        .register_pending_callable_lookup(PendingCallableLookup { expr });
                    return typed;
                }
                Err((method_name, selected_receiver_ty, err)) => {
                    self.push_diag(body_diag_from_method_selection_err(
                        self.db,
                        err,
                        Spanned::new(selected_receiver_ty, callee.span(self.body()).into()),
                        Spanned::new(method_name, call_span.clone().callee().into()),
                    ));
                    return ExprProp::invalid(self.db);
                }
            };

        if let Some(args_ty) = self.definitely_malformed_callable_args_pack(&candidates) {
            self.push_diag(BodyDiag::CallArgsMustBeTuple {
                primary: call_span.args().into(),
                args_ty,
            });
            return ExprProp::invalid(self.db);
        }

        let ret_ty = self
            .env
            .typed_expr(expr)
            .map_or_else(|| self.fresh_ty(), |prop| prop.ty);
        let typed = ExprProp::new(ret_ty, true);
        self.env.type_expr(expr, typed.clone());
        if !already_typed {
            for arg in args {
                self.check_expr_unknown(arg.expr);
            }
        }
        self.env.register_pending_method(super::env::PendingMethod {
            expr,
            recv_ty: selected_receiver_ty,
            method_name: IdentId::new(self.db, ClosureCallTrait::Fn.method_name().to_string()),
            candidates,
            span: call_span.callee().into(),
            callee_is_receiver: true,
        });
        typed
    }

    fn definitely_malformed_callable_args_pack(
        &mut self,
        candidates: &[super::env::PendingMethodCandidate<'db>],
    ) -> Option<TyId<'db>> {
        let mut malformed = None;
        for candidate in candidates {
            let pack = candidate.inst.args(self.db).get(1).copied()?;
            let pack = self.normalize_ty(pack);
            if pack.has_var(self.db) || pack.is_tuple(self.db) {
                return None;
            }
            malformed.get_or_insert(pack);
        }
        malformed
    }

    fn collect_callable_value_candidates(
        &mut self,
        callee: ExprId,
        callee_prop: &ExprProp<'db>,
    ) -> Result<Option<PendingCallableValueSelection<'db>>, CallableValueSelectionError<'db>> {
        let mut selected_ty = None;
        let mut candidates = Vec::new();
        for (priority, call_trait) in [ClosureCallTrait::Fn, ClosureCallTrait::FnOnce]
            .into_iter()
            .enumerate()
        {
            let Some(trait_def) = call_trait.trait_def(self.db, self.env.scope()) else {
                continue;
            };
            let method_name = IdentId::new(self.db, call_trait.method_name().to_string());
            let (receiver_ty, canonical_r_ty, selection) = self.select_method_call_candidate(
                callee,
                callee_prop,
                method_name,
                Some(trait_def),
            );
            let mut push_candidate = |candidate: TraitMethodCand<'db>, needs_confirmation| {
                selected_ty.get_or_insert(receiver_ty);
                let inst = canonical_r_ty.extract_solution(&mut self.table, candidate.inst);
                candidates.push(super::env::PendingMethodCandidate {
                    inst,
                    method: candidate.method,
                    needs_confirmation,
                    priority: priority as u8,
                });
            };
            match selection {
                Ok(MethodCandidate::TraitMethod(candidate)) => push_candidate(candidate, false),
                Ok(MethodCandidate::NeedsConfirmation(candidate)) => {
                    push_candidate(candidate, true);
                }
                Ok(MethodCandidate::InherentMethod(_)) => {
                    unreachable!("exact callable-trait lookup cannot select an inherent method")
                }
                Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                    for candidate in ambiguous.candidates {
                        let needs_confirmation = candidate.needs_confirmation;
                        push_candidate(candidate.cand, needs_confirmation);
                    }
                }
                Err(MethodSelectionError::NotFound) => {}
                Err(err) => return Err((method_name, receiver_ty, err)),
            }
        }
        Ok(selected_ty.map(|selected_ty| (selected_ty, candidates)))
    }

    fn select_callable_value_candidate(
        &self,
        callee: ExprId,
        callee_prop: &ExprProp<'db>,
    ) -> Result<Option<CallableValueSelection<'db>>, CallableValueSelectionError<'db>> {
        for call_trait in [ClosureCallTrait::Fn, ClosureCallTrait::FnOnce] {
            let Some(trait_def) = call_trait.trait_def(self.db, self.env.scope()) else {
                continue;
            };
            let method_name = IdentId::new(self.db, call_trait.method_name().to_string());
            let (selected_receiver_ty, canonical_r_ty, candidate) = self
                .select_method_call_candidate(callee, callee_prop, method_name, Some(trait_def));
            match candidate {
                Ok(candidate) => {
                    return Ok(Some((selected_receiver_ty, canonical_r_ty, candidate)));
                }
                Err(MethodSelectionError::NotFound) => {}
                Err(err) => return Err((method_name, selected_receiver_ty, err)),
            }
        }
        Ok(None)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_selected_callable_value_call(
        &mut self,
        expr: ExprId,
        callee: ExprId,
        args: &[HirCallArg<'db>],
        callee_prop: ExprProp<'db>,
        selected_receiver_ty: TyId<'db>,
        canonical_r_ty: Canonicalized<'db, TyId<'db>>,
        candidate: MethodCandidate<'db>,
        expected: TyId<'db>,
        already_typed: bool,
    ) -> ExprProp<'db> {
        let call_span = expr.span(self.body()).into_call_expr();
        let needs_confirmation = matches!(candidate, MethodCandidate::NeedsConfirmation(_));
        let (MethodCandidate::TraitMethod(candidate)
        | MethodCandidate::NeedsConfirmation(candidate)) = candidate
        else {
            unreachable!("callable value dispatch only selects core callable traits")
        };
        let inst = canonical_r_ty.extract_solution(&mut self.table, candidate.inst);
        let func_ty =
            self.instantiate_trait_method_to_term(candidate.method, selected_receiver_ty, inst);
        let mut callable = match Callable::new(
            self.db,
            func_ty,
            callee.span(self.body()).into(),
            Some(inst),
        ) {
            Ok(callable) => callable,
            Err(diag) => {
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };
        self.constrain_callable_result_from_expected(callable.ret_ty(self.db), expected);
        callable.check_args(
            self,
            args,
            call_span.clone().args(),
            Some((callee, callee_prop)),
            already_typed,
        );
        self.specialize_callable_layout_args(&mut callable, Some(callee), args);
        if self.call_args_include_closure(args) {
            self.eagerly_process_callable_constraints(
                &callable,
                expr,
                call_span.clone().callee().into(),
            );
        }
        self.check_callable_effects(expr, &mut callable);
        callable.process_constraints(self, expr, call_span.clone().callee().into());

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        self.env.register_semantic_value_call(expr, callable);
        let mut result = ExprProp::new(ret_ty, true);
        if !self.closure_type_expectations.is_empty() && self.env.callable_expr(expr).is_some() {
            self.env.type_expr(expr, result.clone());
            let (replayed, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            if outcome == ClosureReplayOutcome::Replayed {
                result = replayed;
            }
        }
        if needs_confirmation
            && let Some(goal) = self.env.callable_expr(expr).and_then(Callable::trait_inst)
        {
            self.env.register_trait_obligation(TraitObligation {
                goal,
                origin: TraitObligationOrigin::GenericConfirmation { expr },
                span: call_span.callee().into(),
            });
        }
        result
    }

    fn constrain_pending_direct_closure_call(
        &mut self,
        callee_prop: &ExprProp<'db>,
        args: &[HirCallArg<'db>],
        expected: TyId<'db>,
    ) {
        let Some(closure) = self
            .normalize_ty(callee_prop.ty)
            .base_ty(self.db)
            .as_closure(self.db)
        else {
            for arg in args {
                self.check_expr_unknown(arg.expr);
            }
            return;
        };
        self.constrain_callable_result_from_expected(closure.ret_ty(self.db), expected);
        if args.len() != closure.params(self.db).len() {
            for arg in args {
                self.check_expr_unknown(arg.expr);
            }
            return;
        }
        for (arg, &expected) in args.iter().zip(closure.params(self.db)) {
            self.check_or_constrain_expr_to_expected(arg.expr, expected);
        }
    }

    fn check_assert(&mut self, expr: ExprId, args: &[HirCallArg<'db>]) -> ExprProp<'db> {
        if !(1..=2).contains(&args.len()) {
            for arg in args {
                self.check_expr_unknown(arg.expr);
            }
            self.push_diag(BodyDiag::AssertArgNumMismatch {
                primary: expr.span(self.body()).into_macro_call_expr().args().into(),
                given: args.len(),
            });
            return ExprProp::invalid(self.db);
        }

        self.check_expr(args[0].expr, TyId::bool(self.db));
        if let Some(message_arg) = args.get(1) {
            let message = message_arg.expr;
            let Partial::Present(Expr::Lit(LitKind::String(string_id))) =
                message.data(self.db, self.body())
            else {
                self.check_expr_unknown(message);
                self.push_diag(BodyDiag::AssertMessageMustBeStringLiteral {
                    primary: message.span(self.body()).into(),
                });
                return ExprProp::invalid(self.db);
            };

            let expected = self.string_literal_byte_array_ty(string_id.len_bytes(self.db));
            self.check_expr(message, expected);
        }
        ExprProp::new(TyId::unit(self.db), true)
    }

    fn check_code_region_intrinsic(
        &mut self,
        expr: ExprId,
        callable: &mut Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
        kind: CodeRegionIntrinsicKind,
        receiver: Option<(ExprId, ExprProp<'db>)>,
        direct_callee: Option<ExprId>,
    ) -> Option<ExprProp<'db>> {
        let arg_expr = args[0].expr;
        callable.check_args(
            self,
            args,
            expr.span(self.body()).into_call_expr().args(),
            receiver,
            false,
        );
        let arg_ty = self
            .env
            .typed_expr(arg_expr)
            .map(|prop| self.normalize_ty(prop.ty))
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
        if !ty_may_be_code_region_token(self.db, arg_ty) {
            return None;
        }
        if let Some(callee) = direct_callee {
            self.env
                .type_expr(callee, ExprProp::new(callable.ty(self.db), true));
        }
        self.env
            .register_code_region_intrinsic(expr, callable.clone(), arg_expr, kind);
        Some(ExprProp::new(TyId::u256(self.db), true))
    }

    pub(super) fn check_callable_effects(&mut self, expr: ExprId, callable: &mut Callable<'db>) {
        self.env.record_expr_effect_env(expr);
        let lexical_depth = self.env.expr_closure_depth(expr);
        let args = self.resolve_callable_effects_in_lexical_context(expr, callable);

        let checking_in_lexical_closure = self.env.closure_depth() == lexical_depth;
        let mut late_contributions = Vec::new();
        for arg in args {
            if let Some(contribution) = self.effect_arg_capture_contribution(&arg, true) {
                if checking_in_lexical_closure {
                    self.env
                        .record_capture_if_needed(contribution.binding, contribution.ty);
                    self.env
                        .record_capture_access(contribution.binding, contribution.access);
                } else {
                    late_contributions.push(contribution);
                }
            }
            self.env.push_call_effect_arg(expr, arg);
        }
        self.env
            .replace_late_effect_capture_contributions(expr, late_contributions);
    }

    /// Applies capture requirements discovered while resolving deferred
    /// effectful calls after their lexical closures have already been built.
    ///
    /// Contributions are first coalesced by `(closure, binding)` in source
    /// order. Rebuilding then proceeds from inner closures to outer closures
    /// so any nested closure type replacement is already available when an
    /// enclosing descriptor is reconstructed.
    pub(super) fn finalize_late_closure_captures(&mut self) -> FxHashMap<TyId<'db>, TyId<'db>> {
        fn merge_capture_ty<'db>(
            db: &'db dyn HirAnalysisDb,
            current: TyId<'db>,
            requested: TyId<'db>,
        ) -> TyId<'db> {
            if current == requested {
                return current;
            }
            let current_cap = current.as_capability(db);
            let requested_cap = requested.as_capability(db);
            match (current_cap, requested_cap) {
                (None, Some((_, requested_inner))) if current == requested_inner => requested,
                (Some((_, current_inner)), None) if current_inner == requested => current,
                (Some((current_kind, current_inner)), Some((requested_kind, requested_inner)))
                    if current_inner == requested_inner =>
                {
                    let rank = |kind| match kind {
                        CapabilityKind::View => 0,
                        CapabilityKind::Ref => 1,
                        CapabilityKind::Mut => 2,
                    };
                    if rank(requested_kind) > rank(current_kind) {
                        requested
                    } else {
                        current
                    }
                }
                _ => current,
            }
        }

        let body = self.body();
        let mut contributions_by_closure: IndexMap<
            ClosureDef<'db>,
            IndexMap<LocalBinding<'db>, LateClosureCaptureContribution<'db>>,
        > = IndexMap::new();

        // Expr IDs follow HIR source order, and IndexMap preserves the first
        // occurrence of both a closure and a binding. This makes duplicate
        // contributions deterministic while retaining capture field order.
        for expr in body.exprs(self.db).keys() {
            let Some(contributions) = self
                .env
                .late_effect_capture_contributions(expr)
                .map(<[LateClosureCaptureContribution<'db>]>::to_vec)
            else {
                continue;
            };
            let ancestry = self.env.expr_closure_ancestry(expr).to_vec();
            for contribution in contributions {
                if contribution.provider_closure_depth >= ancestry.len() {
                    continue;
                }
                for &closure in ancestry.iter().skip(contribution.provider_closure_depth) {
                    let by_binding = contributions_by_closure.entry(closure).or_default();
                    if let Some(existing) = by_binding.get_mut(&contribution.binding) {
                        existing.ty = merge_capture_ty(self.db, existing.ty, contribution.ty);
                        existing.access.include(contribution.access);
                        existing.provider_closure_depth = existing
                            .provider_closure_depth
                            .min(contribution.provider_closure_depth);
                    } else {
                        by_binding.insert(contribution.binding, contribution);
                    }
                }
            }
        }

        if contributions_by_closure.is_empty() {
            self.env.set_closure_ty_replacements(FxHashMap::default());
            return FxHashMap::default();
        }

        let mut infos: IndexMap<ClosureDef<'db>, ClosureInfo<'db>> = IndexMap::new();
        let mut original_tys = FxHashMap::default();
        let mut late_bindings: FxHashMap<ClosureDef<'db>, FxHashSet<LocalBinding<'db>>> =
            FxHashMap::default();

        for (&closure, by_binding) in &contributions_by_closure {
            let Some(mut info) = self.env.closure_info(closure.expr).cloned() else {
                continue;
            };
            original_tys.insert(closure, info.ty);
            let planned_capture_count = info.captures.len().saturating_add(
                by_binding
                    .keys()
                    .filter(|binding| {
                        !info
                            .captures
                            .iter()
                            .any(|capture| capture.binding == **binding)
                    })
                    .count(),
            );
            let mut field_limit_reported = false;

            for (&binding, contribution) in by_binding {
                if let Some(capture) = info
                    .captures
                    .iter_mut()
                    .find(|capture| capture.binding == binding)
                {
                    capture.ty = merge_capture_ty(self.db, capture.ty, contribution.ty);
                    capture.access_without_return.include(contribution.access);
                    capture.access.include(contribution.access);
                } else {
                    // Check before appending: semantic closure fields are
                    // indexed by u16, so even an invalid program must never
                    // construct an unrepresentable environment descriptor.
                    if !closure_field_count_is_supported(info.captures.len().saturating_add(1)) {
                        if !field_limit_reported {
                            self.push_diag(BodyDiag::ClosureFieldLimitExceeded {
                                primary: closure.expr.span(body).into(),
                                captures: true,
                                given: planned_capture_count,
                                max: MAX_CLOSURE_FIELDS,
                            });
                            field_limit_reported = true;
                        }
                        continue;
                    }
                    info.captures.push(ClosureCapture {
                        binding,
                        ty: contribution.ty,
                        construction: ClosureCaptureConstruction::Deferred,
                        access_without_return: contribution.access,
                        access: contribution.access,
                    });
                    info.capture_expr_accesses.push(IndexMap::new());
                }
                late_bindings.entry(closure).or_default().insert(binding);
            }

            for capture in &mut info.captures {
                if !late_bindings
                    .get(&closure)
                    .is_some_and(|bindings| bindings.contains(&capture.binding))
                {
                    continue;
                }
                capture.ty = self.normalize_ty(capture.ty);
                capture.construction =
                    if capture.ty.as_capability(self.db).is_some() || self.ty_is_copy(capture.ty) {
                        ClosureCaptureConstruction::Copy
                    } else if capture.ty.has_var(self.db) {
                        ClosureCaptureConstruction::Deferred
                    } else {
                        ClosureCaptureConstruction::Move
                    };
            }
            infos.insert(closure, info);
        }

        // Constructing a nested closure can itself move a captured binding.
        // Mirror the eager closure path by including that access in every
        // already-capturing ancestor.
        let mut propagation = Vec::new();
        for (&closure, bindings) in &late_bindings {
            let Some(info) = infos.get(&closure) else {
                continue;
            };
            for &binding in bindings {
                let Some(capture) = info
                    .captures
                    .iter()
                    .find(|capture| capture.binding == binding)
                else {
                    continue;
                };
                let access = match capture.construction {
                    ClosureCaptureConstruction::Copy => continue,
                    ClosureCaptureConstruction::Deferred => ClosureCaptureAccess::MoveIfNonCopy,
                    ClosureCaptureConstruction::Move => ClosureCaptureAccess::Move,
                };
                for &ancestor in self.env.expr_closure_ancestry(closure.expr) {
                    propagation.push((ancestor, binding, access));
                }
            }
        }
        for (ancestor, binding, access) in propagation {
            if !infos.contains_key(&ancestor) {
                let Some(info) = self.env.closure_info(ancestor.expr).cloned() else {
                    continue;
                };
                if !info
                    .captures
                    .iter()
                    .any(|capture| capture.binding == binding)
                {
                    continue;
                }
                original_tys.insert(ancestor, info.ty);
                infos.insert(ancestor, info);
            }
            if let Some(capture) = infos.get_mut(&ancestor).and_then(|info| {
                info.captures
                    .iter_mut()
                    .find(|capture| capture.binding == binding)
            }) {
                capture.access_without_return.include(access);
                capture.access.include(access);
            }
        }

        let mut closures = infos.keys().copied().collect::<Vec<_>>();
        closures
            .sort_by_key(|closure| std::cmp::Reverse(self.env.expr_closure_depth(closure.expr)));

        let mut replacements = FxHashMap::default();
        for closure in closures {
            let Some(info) = infos.remove(&closure) else {
                continue;
            };
            let old_ty = original_tys[&closure];
            let mut info = rewrite_types(self.db, info, &replacements);
            for capture in &mut info.captures {
                capture.ty = self.normalize_ty(capture.ty);
            }
            let closure_ty = ClosureTy::new(
                self.db,
                info.def,
                info.ty.parent_args(self.db).to_vec(),
                ClosureCaptures::new(
                    info.captures.iter().map(|capture| capture.ty).collect(),
                    info.captures.iter().map(|capture| capture.access).collect(),
                ),
                ClosureSignature::new(
                    info.ty.params(self.db).to_vec(),
                    info.ty.param_modes(self.db).to_vec(),
                    info.ty.ret_ty(self.db),
                ),
            );
            let old_ty = TyId::closure(self.db, old_ty);
            let new_ty = TyId::closure(self.db, closure_ty);
            self.env.replace_closure_info(
                closure.expr,
                ClosureInfo {
                    ty: closure_ty,
                    ..info
                },
            );
            if old_ty != new_ty {
                replacements.insert(old_ty, new_ty);
            }
        }

        self.env.set_closure_ty_replacements(replacements.clone());
        replacements
    }

    /// Re-checks decisions whose validity depends on whether a closure
    /// implements reusable `Fn`. Deferred effect resolution can add a
    /// non-Copy by-value capture after those decisions were made, changing the
    /// closure from `Reusable` to `Consuming`.
    pub(super) fn revalidate_late_closure_capability_changes(
        &mut self,
        replacements: &FxHashMap<TyId<'db>, TyId<'db>>,
    ) {
        if replacements.is_empty() {
            return;
        }

        let mut normalized_replacements = FxHashMap::default();
        let mut consuming_defs = FxHashSet::default();
        for (&old, &new) in replacements {
            let old = self.normalize_ty(old);
            let new = self.normalize_ty(new);
            normalized_replacements.insert(old, new);
            let (Some(old_closure), Some(new_closure)) = (
                old.base_ty(self.db).as_closure(self.db),
                new.base_ty(self.db).as_closure(self.db),
            ) else {
                continue;
            };
            if old_closure.call_mode_with_assumptions(self.db, self.env.assumptions())
                == ClosureCallMode::Reusable
                && new_closure.call_mode_with_assumptions(self.db, self.env.assumptions())
                    == ClosureCallMode::Consuming
            {
                consuming_defs.insert(new_closure.def(self.db));
            }
        }
        if consuming_defs.is_empty() {
            return;
        }

        let body = self.body();
        let exprs = body.exprs(self.db).keys().collect::<Vec<_>>();

        // Direct invocation deliberately selects the strongest available
        // callable trait, preferring reusable `Fn`. If late effect resolution
        // changes a concrete closure to consuming, preserve the source-level
        // call by reselecting its `FnOnce::call_once` implementation.
        for &expr in &exprs {
            let Partial::Present(Expr::Call(callee, args)) = expr.data(self.db, body) else {
                continue;
            };
            let Some(SemanticExprLowering::Call {
                callable: old_callable,
                callee_is_receiver: true,
            }) = self.env.semantic_expr_lowering(expr).cloned()
            else {
                continue;
            };
            let Some(old_inst) = old_callable.trait_inst() else {
                continue;
            };
            if ClosureCallTrait::for_trait(self.db, self.env.scope(), old_inst.def(self.db))
                != Some(ClosureCallTrait::Fn)
            {
                continue;
            }

            let Some(mut callee_prop) = self.env.typed_expr(*callee) else {
                continue;
            };
            let old_callee_ty = self.normalize_ty(callee_prop.ty);
            let new_callee_ty = rewrite_types(self.db, old_callee_ty, &normalized_replacements);
            let (Some(old_closure), Some(new_closure)) = (
                old_callee_ty.base_ty(self.db).as_closure(self.db),
                new_callee_ty.base_ty(self.db).as_closure(self.db),
            ) else {
                continue;
            };
            if old_closure.def(self.db) != new_closure.def(self.db)
                || !consuming_defs.contains(&new_closure.def(self.db))
            {
                continue;
            }

            callee_prop.ty = new_callee_ty;
            let Ok(Some((selected_receiver_ty, canonical_r_ty, candidate))) =
                self.select_callable_value_candidate(*callee, &callee_prop)
            else {
                debug_assert!(false, "a consuming closure must implement FnOnce");
                continue;
            };
            let (MethodCandidate::TraitMethod(candidate)
            | MethodCandidate::NeedsConfirmation(candidate)) = candidate
            else {
                unreachable!("callable value dispatch only selects core callable traits")
            };
            let inst = canonical_r_ty.extract_solution(&mut self.table, candidate.inst);
            debug_assert_eq!(
                ClosureCallTrait::for_trait(self.db, self.env.scope(), inst.def(self.db)),
                Some(ClosureCallTrait::FnOnce),
            );
            let func_ty =
                self.instantiate_trait_method_to_term(candidate.method, selected_receiver_ty, inst);
            let Ok(mut callable) =
                Callable::new(self.db, func_ty, callee.span(body).into(), Some(inst))
            else {
                debug_assert!(false, "selected FnOnce method must have a callable type");
                continue;
            };
            self.specialize_callable_layout_args(&mut callable, Some(*callee), args);
            self.record_owned_value_use(*callee, new_callee_ty);
            self.env.replace_semantic_callable(expr, callable);
        }

        // A direct `.call` was selected through the reusable `Fn` builtin.
        // Once the receiver becomes consuming only `.call_once` remains.
        for &expr in &exprs {
            let Partial::Present(Expr::MethodCall(receiver, method_name, _, _)) =
                expr.data(self.db, body)
            else {
                continue;
            };
            let Some(method_name) = method_name.to_opt() else {
                continue;
            };
            if method_name.data(self.db) != "call" || self.env.callable_expr(expr).is_none() {
                continue;
            }
            let Some(receiver_prop) = self.env.typed_expr(*receiver) else {
                continue;
            };
            let old_receiver_ty = self.normalize_ty(receiver_prop.ty);
            let new_receiver_ty = rewrite_types(self.db, old_receiver_ty, &normalized_replacements);
            let Some(old_closure) = old_receiver_ty.base_ty(self.db).as_closure(self.db) else {
                continue;
            };
            let Some(new_closure) = new_receiver_ty.base_ty(self.db).as_closure(self.db) else {
                continue;
            };
            if old_closure.def(self.db) != new_closure.def(self.db)
                || !consuming_defs.contains(&new_closure.def(self.db))
            {
                continue;
            }
            let call_span = expr.span(body).into_method_call_expr();
            self.push_diag(body_diag_from_method_selection_err(
                self.db,
                MethodSelectionError::NotFound,
                Spanned::new(new_receiver_ty, receiver.span(body).into()),
                Spanned::new(method_name, call_span.method_name().into()),
            ));
        }

        // Generic callable bounds were solved against the provisional closure
        // descriptor. Re-instantiate only changed `Fn` obligations and run the
        // ordinary final-pass solver so diagnostics retain their normal
        // call-site and `required by this bound` context.
        for expr in exprs {
            let expr_data = expr.data(self.db, body);
            let span: DynLazySpan<'db> = match expr_data {
                Partial::Present(Expr::Call(callee, _)) => callee.span(body).into(),
                Partial::Present(Expr::MethodCall(_, _, _, _)) => {
                    expr.span(body).into_method_call_expr().method_name().into()
                }
                _ => continue,
            };
            let Some(callable) = self.env.callable_expr(expr).cloned() else {
                continue;
            };
            let callable = callable.fold_with(self.db, &mut self.table);
            let callable = rewrite_types(self.db, callable, &normalized_replacements);
            let constraints =
                crate::analysis::ty::trait_resolution::constraint::collect_func_decl_constraints(
                    self.db,
                    callable.callable_def(),
                    true,
                )
                .instantiate(self.db, callable.generic_args());
            for (constraint_idx, &constraint) in constraints.list(self.db).iter().enumerate() {
                let constraint = if let Some(inst) = callable.trait_inst() {
                    let mut subst = AssocTySubst::new(inst);
                    constraint.fold_with(self.db, &mut subst)
                } else {
                    constraint
                };
                let constraint = self.normalize_trait_goal(constraint);
                let constraint = rewrite_types(self.db, constraint, &normalized_replacements);
                let constraint = self.normalize_trait_goal(constraint);
                if ClosureCallTrait::for_trait(self.db, self.env.scope(), constraint.def(self.db))
                    != Some(ClosureCallTrait::Fn)
                {
                    continue;
                }
                let Some(closure) = constraint
                    .self_ty(self.db)
                    .base_ty(self.db)
                    .as_closure(self.db)
                else {
                    continue;
                };
                if !consuming_defs.contains(&closure.def(self.db)) {
                    continue;
                }
                let obligation = TraitObligation {
                    goal: constraint,
                    origin: TraitObligationOrigin::CallConstraint {
                        call_expr: expr,
                        callable_def: callable.callable_def(),
                        constraint_idx,
                    },
                    span: span.clone(),
                };
                let _ = self.process_trait_obligation(obligation, true);
            }
        }
    }

    fn resolve_callable_effects_in_lexical_context(
        &mut self,
        expr: ExprId,
        callable: &mut Callable<'db>,
    ) -> Vec<super::ResolvedEffectArg<'db>> {
        let lexical_depth = self.env.expr_closure_depth(expr);
        let lexical_effect_env = self.env.expr_effect_env(expr).cloned();
        let saved_effect_env =
            lexical_effect_env.map(|lexical| std::mem::replace(self.env.effect_env_mut(), lexical));
        let call_span: DynLazySpan<'db> = expr.span(self.body()).into();
        let args = self.resolve_callable_effects_at_depth(call_span, callable, lexical_depth);
        if let Some(saved) = saved_effect_env {
            *self.env.effect_env_mut() = saved;
        }
        args
    }

    fn effect_arg_capture_contribution(
        &mut self,
        arg: &super::ResolvedEffectArg<'db>,
        emit_unbound_diag: bool,
    ) -> Option<LateClosureCaptureContribution<'db>> {
        if !arg.provider_is_external_to_closure {
            return None;
        }
        let binding = match &arg.arg {
            super::EffectArg::Place(place) => {
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(expr) => {
                let bindings = self.forwarded_effect_value_bindings(*expr);
                if bindings.len() == 1 {
                    bindings.into_iter().next()
                } else {
                    if emit_unbound_diag {
                        self.push_diag(BodyDiag::ClosureEffectProviderMustBeBound {
                            primary: expr.span(self.body()).into(),
                        });
                    }
                    None
                }
            }
            super::EffectArg::Unknown => None,
        };
        let binding = binding?;
        let binding_ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        let capture_ty = if matches!(arg.pass_mode, super::EffectPassMode::ByPlace)
            && binding_ty.as_capability(self.db).is_none()
        {
            if arg.required_mut {
                TyId::borrow_mut_of(self.db, binding_ty)
            } else {
                TyId::borrow_ref_of(self.db, binding_ty)
            }
        } else {
            binding_ty
        };
        let access = if matches!(
            arg.pass_mode,
            super::EffectPassMode::ByValue | super::EffectPassMode::ByTempPlace
        ) {
            ClosureCaptureAccess::MoveIfNonCopy
        } else {
            ClosureCaptureAccess::Read
        };
        Some(LateClosureCaptureContribution {
            binding,
            ty: capture_ty,
            access,
            provider_closure_depth: arg.provider_closure_depth,
        })
    }

    fn effect_capture_footprint(
        &mut self,
        args: &[super::ResolvedEffectArg<'db>],
    ) -> Vec<EffectCaptureFootprint<'db>> {
        args.iter()
            .filter_map(|arg| {
                if let Some(contribution) = self.effect_arg_capture_contribution(arg, false) {
                    return Some(EffectCaptureFootprint::Binding(contribution));
                }
                if arg.provider_is_external_to_closure
                    && let super::EffectArg::Value(expr) = arg.arg
                    && self.env.expr_place(expr).is_none()
                {
                    return Some(EffectCaptureFootprint::ExternalRvalue {
                        expr,
                        provider_closure_depth: arg.provider_closure_depth,
                    });
                }
                None
            })
            .collect()
    }

    pub(super) fn resolve_callable_effects(
        &mut self,
        call_span: DynLazySpan<'db>,
        callable: &mut Callable<'db>,
    ) -> Vec<super::ResolvedEffectArg<'db>> {
        self.resolve_callable_effects_at_depth(call_span, callable, self.env.closure_depth())
    }

    fn resolve_callable_effects_at_depth(
        &mut self,
        call_span: DynLazySpan<'db>,
        callable: &mut Callable<'db>,
        lexical_closure_depth: usize,
    ) -> Vec<super::ResolvedEffectArg<'db>> {
        let CallableDef::Func(func) = callable.callable_def else {
            return Vec::new();
        };

        if !func.has_effects(self.db) {
            return Vec::new();
        }

        let mut resolved_args: Vec<super::ResolvedEffectArg<'db>> = Vec::new();
        let mut specialized_providers: FxHashMap<u32, EffectProviderSpecialization<'db>> =
            FxHashMap::default();

        let body = self.body();
        let callee_provider_arg_idx_by_effect =
            place_effect_provider_param_index_map(self.db, func);
        let callee_effect_env =
            crate::core::semantic::EffectEnvView::new(EffectParamSite::Func(func));

        let provided_span = |provided: ProvidedEffect<'db>| match provided.origin {
            EffectOrigin::With { value_expr } => Some(value_expr.span(body).into()),
            EffectOrigin::Param { .. } => None,
        };
        let reqs = effect_requirement_decls_for_callable(self.db, callable.callable_def);
        for (param_idx, req) in reqs.iter().enumerate() {
            let Some(key_path) = req.key_path else {
                continue;
            };
            let Some(query) = build_effect_query_for_call(self, callable, req) else {
                continue;
            };
            let provider_arg_idx_for_param = callee_provider_arg_idx_by_effect
                .get(req.binding_idx as usize)
                .copied()
                .flatten();

            match self.resolve_effect_query(func, req.clone(), query.clone(), call_span.clone()) {
                EffectResolution::Chosen(evidence) => {
                    let (provider, arg_style, key_kind, instantiated_key_ty) = match *evidence {
                        EffectEvidence::Keyed {
                            provider,
                            key_kind,
                            target_ty,
                            commit,
                            arg_style,
                        } => {
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(committed, "chosen keyed effect evidence commit failed");
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, key_kind, target_ty)
                        }
                        EffectEvidence::UnkeyedType {
                            provider,
                            commit,
                            arg_style,
                        } => {
                            let target_ty = match commit.key_match.clone() {
                                Some(KeyMatchCommit::QueryToType { actual, .. }) => {
                                    Some(self.table.fold_ty(self.db, actual))
                                }
                                _ => match req.key.clone() {
                                    EffectRequirementKey::Type(_) => self
                                        .query_type_key(&query.key)
                                        .map(|ty| self.table.fold_ty(self.db, ty)),
                                    EffectRequirementKey::Trait(_) => None,
                                },
                            };
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(
                                committed,
                                "chosen unkeyed type effect evidence commit failed"
                            );
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, EffectKeyKind::Type, target_ty)
                        }
                        EffectEvidence::UnkeyedTrait {
                            provider,
                            commit,
                            arg_style,
                        } => {
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(
                                committed,
                                "chosen unkeyed trait effect evidence commit failed"
                            );
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, EffectKeyKind::Trait, None)
                        }
                    };

                    let (arg, pass_mode) =
                        self.effect_arg_for_provider(provider, arg_style, req.required_mut);
                    let provider_target_ty = self.provider_target_ty_for_effect_arg(
                        provider,
                        instantiated_key_ty,
                        req.required_mut,
                    );
                    if req.required_mut && matches!(pass_mode, super::EffectPassMode::Unknown) {
                        let diag = BodyDiag::EffectMutabilityMismatch {
                            primary: call_span.clone(),
                            func,
                            key: key_path,
                            provided_span: provided_span(provider),
                        };
                        self.push_diag(diag);
                        continue;
                    }
                    if !self.effect_arg_is_valid(&arg, pass_mode) {
                        let diag = BodyDiag::MissingEffect {
                            primary: call_span.clone(),
                            func,
                            key: key_path,
                        };
                        self.push_diag(diag);
                        continue;
                    }
                    if let Some(provider_arg_idx) = provider_arg_idx_for_param
                        && let Some(provider_var) =
                            callable.generic_args().get(provider_arg_idx).copied()
                        && let Some(given) = self.inferred_provider_ty_for_effect_arg(
                            provider,
                            &arg,
                            pass_mode,
                            provider_target_ty,
                        )
                    {
                        let existing_provider = self.table.fold_ty(self.db, provider_var);
                        let snapshot = self.table.snapshot();
                        if self.table.unify(provider_var, given).is_err() {
                            self.table.rollback_to(snapshot);
                            self.push_diag(BodyDiag::EffectProviderMismatch {
                                primary: call_span.clone(),
                                func,
                                key: key_path,
                                expected: existing_provider,
                                given,
                                provided_span: provided_span(provider),
                            });
                        }
                    }
                    if let Some(target_ty) = instantiated_key_ty {
                        self.instantiate_callable_effect_layout_args(
                            callable,
                            func,
                            req.binding_idx as usize,
                            target_ty,
                        );
                    }
                    self.specialize_callable_effect_layout_projections(
                        callable,
                        func,
                        req.binding_idx as usize,
                        provider,
                        &arg,
                    );
                    let provider_space = self
                        .effect_arg_provider_space(&arg, pass_mode)
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(provider.ty))
                        .or_else(|| {
                            matches!(pass_mode, super::EffectPassMode::ByTempPlace)
                                .then_some(super::ProviderAddressSpace::Memory)
                        });
                    if matches!(
                        pass_mode,
                        super::EffectPassMode::ByPlace | super::EffectPassMode::ByTempPlace
                    ) && provider_space.is_none()
                        && !self.effect_arg_provider_space_can_remain_unresolved(&arg)
                    {
                        panic!(
                            "effect arg provider space must be explicit for {pass_mode:?} at {key_path:?}"
                        );
                    }
                    if let Some(resolved_binding) =
                        callee_effect_env.resolved_binding(self.db, req.binding_idx as usize)
                    {
                        let provider_idx = resolved_binding.provider.provider_idx;
                        let specialization = self.specialize_effect_provider_binding(
                            resolved_binding.provider,
                            provider,
                            &arg,
                            pass_mode,
                            provider_target_ty,
                            provider_space,
                        );
                        if let Some(previous) =
                            specialized_providers.insert(provider_idx, specialization.clone())
                        {
                            assert_eq!(
                                previous, specialization,
                                "conflicting call-site provider specialization for function effect provider slot {} at {:?}",
                                provider_idx, key_path,
                            );
                        }
                    }
                    resolved_args.push(super::ResolvedEffectArg {
                        param_idx,
                        binding_idx: req.binding_idx,
                        key: key_path,
                        arg,
                        pass_mode,
                        provider_closure_depth: provider.source_closure_depth,
                        provider_is_external_to_closure: provider.source_closure_depth
                            < lexical_closure_depth,
                        required_mut: req.required_mut,
                        key_kind,
                        instantiated_key_ty,
                        provider_target_ty,
                        provider: provider_space,
                    });
                }
                EffectResolution::BlockedByBarrier => {}
                EffectResolution::Missing => {
                    self.push_diag(BodyDiag::MissingEffect {
                        primary: call_span.clone(),
                        func,
                        key: key_path,
                    });
                }
                EffectResolution::Ambiguous => {
                    self.push_diag(BodyDiag::AmbiguousEffect {
                        primary: call_span.clone(),
                        func,
                        key: key_path,
                    });
                }
            }
        }
        let mut providers = specialized_providers.into_values().collect::<Vec<_>>();
        providers.sort_by_key(|provider| provider.provider.provider_idx);
        *callable.effect_providers_mut() = providers;

        resolved_args
    }

    fn instantiate_callable_effect_layout_args(
        &mut self,
        callable: &mut Callable<'db>,
        callee: crate::hir_def::Func<'db>,
        effect_idx: usize,
        actual_key_ty: TyId<'db>,
    ) {
        instantiate_callable_effect_layout_args(
            self.db,
            callee,
            effect_idx,
            self.table.fold_ty(self.db, actual_key_ty),
            callable.generic_args_mut(),
        );
    }

    fn resolve_effect_query(
        &mut self,
        func: crate::hir_def::Func<'db>,
        req: EffectRequirementDecl<'db>,
        query: EffectQuery<'db>,
        call_span: DynLazySpan<'db>,
    ) -> EffectResolution<'db> {
        let mut viable: SmallVec<[EffectEvidence<'db>; 2]> = SmallVec::new();
        let effect_env = self.env.effect_env().clone();
        for frame in effect_env.lookup_effect_frames(&query, self) {
            match frame {
                FrameLookupResult::KeyedMatched {
                    entries,
                    blocked_by_barrier,
                    barrier_reason,
                } => {
                    for matched in entries.iter().cloned() {
                        if let Some(evidence) =
                            self.evaluate_keyed_entry(query.required_mut, matched)
                        {
                            viable.push(evidence);
                        }
                    }
                    if viable.is_empty() {
                        return if blocked_by_barrier {
                            let _ = barrier_reason;
                            EffectResolution::BlockedByBarrier
                        } else {
                            EffectResolution::Missing
                        };
                    }
                    return self.choose_effect_evidence(req.name, viable);
                }
                FrameLookupResult::KeyedFamily { entries, providers } => {
                    let mut family_viable = SmallVec::new();
                    for entry in entries.iter().cloned() {
                        let Some(matched) = self.match_family_keyed_entry(&query.key, entry) else {
                            continue;
                        };
                        if let Some(evidence) =
                            self.evaluate_keyed_entry(query.required_mut, matched)
                        {
                            family_viable.push(evidence);
                        }
                    }
                    for provider in providers.iter().copied() {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => self
                                .evaluate_unkeyed_type_provider(
                                    type_query,
                                    provider,
                                    query.required_mut,
                                ),
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider)
                            }
                        };
                        if let Some(evidence) = evidence {
                            family_viable.push(evidence);
                        }
                    }
                    if !family_viable.is_empty() {
                        return self.choose_effect_evidence(req.name, family_viable);
                    }
                }
                FrameLookupResult::Unkeyed { providers } => {
                    for provider in providers {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => self
                                .evaluate_unkeyed_type_provider(
                                    type_query,
                                    provider,
                                    query.required_mut,
                                ),
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider)
                            }
                        };
                        if let Some(evidence) = evidence {
                            viable.push(evidence);
                        }
                    }
                    if !viable.is_empty() {
                        return self.choose_effect_evidence(req.name, viable);
                    }
                }
            }
        }
        let _ = (func, call_span);
        EffectResolution::Missing
    }

    fn match_family_keyed_entry(
        &mut self,
        query: &EffectPatternKey<'db>,
        entry: FamilyKeyedEntry<'db>,
    ) -> Option<MatchedKeyedEntry<'db>> {
        match entry {
            FamilyKeyedEntry::Witness(witness) => {
                let key_commit = query_matches_witness(self, query, &witness.key)?;
                Some(MatchedKeyedEntry::Witness(MatchedWitness {
                    witness,
                    key_commit,
                }))
            }
            FamilyKeyedEntry::Forwarder(forwarder) => {
                let key_commit = query_matches_forwarder(self, query, &forwarder.key)?;
                Some(MatchedKeyedEntry::Forwarder(MatchedForwarder {
                    forwarder,
                    key_commit,
                }))
            }
        }
    }

    fn choose_effect_evidence(
        &self,
        required_name: Option<IdentId<'db>>,
        viable: SmallVec<[EffectEvidence<'db>; 2]>,
    ) -> EffectResolution<'db> {
        match viable.as_slice() {
            [only] => EffectResolution::Chosen(Box::new(only.clone())),
            _ => {
                let Some(required_name) = required_name else {
                    return EffectResolution::Ambiguous;
                };
                let mut name_matches = viable.into_iter().filter(|evidence| {
                    let provider = evidence_provider(evidence);
                    match (provider.origin, provider.binding) {
                        (
                            EffectOrigin::Param {
                                name: Some(name), ..
                            },
                            _,
                        ) => name == required_name,
                        (EffectOrigin::With { .. }, Some(binding)) => {
                            binding.binding_name(&self.env) == required_name
                        }
                        _ => false,
                    }
                });
                if let Some(best) = name_matches.next()
                    && name_matches.next().is_none()
                {
                    EffectResolution::Chosen(Box::new(best))
                } else {
                    EffectResolution::Ambiguous
                }
            }
        }
    }

    fn evaluate_keyed_entry(
        &mut self,
        required_mut: bool,
        matched: MatchedKeyedEntry<'db>,
    ) -> Option<EffectEvidence<'db>> {
        let (provider, transport, key_kind, target_ty, key_commit) = match matched {
            MatchedKeyedEntry::Witness(matched) => {
                let (key_kind, target_ty) = match matched.witness.key {
                    StoredEffectKey::Type(stored) => (EffectKeyKind::Type, Some(stored.carrier)),
                    StoredEffectKey::Trait(_) => (EffectKeyKind::Trait, None),
                };
                (
                    matched.witness.provider,
                    matched.witness.transport,
                    key_kind,
                    target_ty,
                    matched.key_commit,
                )
            }
            MatchedKeyedEntry::Forwarder(matched) => {
                let (key_kind, target_ty) = match matched.forwarder.key {
                    ForwardedEffectKey::Type(forwarded) => {
                        (EffectKeyKind::Type, Some(forwarded.carrier))
                    }
                    ForwardedEffectKey::Trait(_) => (EffectKeyKind::Trait, None),
                };
                (
                    matched.forwarder.provider,
                    matched.forwarder.transport,
                    key_kind,
                    target_ty,
                    matched.key_commit,
                )
            }
        };
        let arg_style = match (transport, target_ty) {
            (WitnessTransport::Direct, Some(target_ty)) => self
                .direct_arg_style_for_provider(provider, target_ty, required_mut)
                .unwrap_or(EffectArgStyle::Value),
            _ => EffectArgStyle::Value,
        };
        Some(EffectEvidence::Keyed {
            provider,
            key_kind,
            target_ty,
            commit: EffectCommitPlan {
                key_match: Some(key_commit),
                trait_solutions: SmallVec::new(),
                provider_resolution: None,
                extra_unifications: SmallVec::new(),
            },
            arg_style,
        })
    }

    fn evaluate_unkeyed_type_provider(
        &mut self,
        query: TypePatternKey<'db>,
        provider: ProvidedEffect<'db>,
        required_mut: bool,
    ) -> Option<EffectEvidence<'db>> {
        let direct_style =
            self.direct_arg_style_for_provider(provider, query.carrier, required_mut);
        if let Some(arg_style) = direct_style {
            let snapshot = self.snapshot_state();
            let ok = apply_key_match_commit(
                self,
                KeyMatchCommit::QueryToType {
                    query: query.clone(),
                    actual: provider
                        .ty
                        .as_capability(self.db)
                        .map(|(_, inner)| inner)
                        .unwrap_or(provider.ty),
                },
            );
            self.rollback_state(snapshot);
            if ok {
                return Some(EffectEvidence::UnkeyedType {
                    provider,
                    commit: EffectCommitPlan {
                        key_match: Some(KeyMatchCommit::QueryToType {
                            query: query.clone(),
                            actual: provider
                                .ty
                                .as_capability(self.db)
                                .map(|(_, inner)| inner)
                                .unwrap_or(provider.ty),
                        }),
                        trait_solutions: SmallVec::new(),
                        provider_resolution: None,
                        extra_unifications: SmallVec::new(),
                    },
                    arg_style,
                });
            }
        }

        let resolution = self.effect_provider_target_resolution(provider.ty, required_mut)?;
        let snapshot = self.snapshot_state();
        let ok = apply_key_match_commit(
            self,
            KeyMatchCommit::QueryToType {
                query: query.clone(),
                actual: resolution.target_ty,
            },
        );
        self.rollback_state(snapshot);
        ok.then_some(EffectEvidence::UnkeyedType {
            provider,
            commit: EffectCommitPlan {
                key_match: Some(KeyMatchCommit::QueryToType {
                    query,
                    actual: resolution.target_ty,
                }),
                trait_solutions: SmallVec::new(),
                provider_resolution: Some(resolution),
                extra_unifications: SmallVec::new(),
            },
            arg_style: EffectArgStyle::Value,
        })
    }

    fn evaluate_unkeyed_trait_provider(
        &mut self,
        query: TraitPatternKey<'db>,
        provider: ProvidedEffect<'db>,
    ) -> Option<EffectEvidence<'db>> {
        let provider_ty = self.table.fold_ty(self.db, provider.ty);
        if provider_ty.has_var(self.db) {
            return None;
        }
        let instantiated = instantiate_trait_pattern_in(self.db, &mut self.table, query);
        let args = std::iter::once(provider_ty)
            .chain(instantiated.args(self.db).iter().skip(1).copied())
            .collect::<Vec<_>>();
        let trait_goal = TraitInstId::new(
            self.db,
            instantiated.def(self.db),
            args,
            instantiated.assoc_type_bindings(self.db).clone(),
        );
        let GoalSatisfiability::Satisfied(solution) =
            self.trait_effect_goal_satisfiability(trait_goal)
        else {
            return None;
        };
        Some(EffectEvidence::UnkeyedTrait {
            provider,
            commit: EffectCommitPlan {
                key_match: None,
                trait_solutions: SmallVec::from_iter([(trait_goal, solution)]),
                provider_resolution: None,
                extra_unifications: SmallVec::new(),
            },
            arg_style: EffectArgStyle::Value,
        })
    }

    fn direct_arg_style_for_provider(
        &self,
        provider: ProvidedEffect<'db>,
        target_ty: TyId<'db>,
        _: bool,
    ) -> Option<EffectArgStyle> {
        let target_ty = normalize_ty(self.db, target_ty, self.env.scope(), self.env.assumptions());
        match provider_semantics(self.db, self.env.scope(), self.env.assumptions(), target_ty)
            .evidence
        {
            ProviderLayoutEvidence::ResolvedHandle(_) => return Some(EffectArgStyle::Value),
            ProviderLayoutEvidence::InvalidHandle(_) => return None,
            ProviderLayoutEvidence::Capability
            | ProviderLayoutEvidence::NotHandle
            | ProviderLayoutEvidence::ContractField => {}
        }
        let place = match provider.origin {
            EffectOrigin::With { value_expr } => self.env.expr_place(value_expr),
            EffectOrigin::Param { .. } => provider
                .binding
                .map(|binding| Place::new(PlaceBase::Binding(binding))),
        };
        Some(match place {
            Some(_) => EffectArgStyle::Place,
            None if matches!(provider.origin, EffectOrigin::With { .. }) => {
                EffectArgStyle::TempPlace
            }
            None => return None,
        })
    }

    fn effect_arg_for_provider(
        &mut self,
        provider: ProvidedEffect<'db>,
        arg_style: EffectArgStyle,
        required_mut: bool,
    ) -> (super::EffectArg<'db>, super::EffectPassMode) {
        if required_mut && !self.provider_supports_mut(provider) {
            return (super::EffectArg::Unknown, super::EffectPassMode::Unknown);
        }

        match arg_style {
            EffectArgStyle::Place => {
                let place = match provider.origin {
                    EffectOrigin::With { value_expr } => self.env.expr_place(value_expr),
                    EffectOrigin::Param { .. } => provider
                        .binding
                        .map(|binding| Place::new(PlaceBase::Binding(binding))),
                };
                (
                    place
                        .map(super::EffectArg::Place)
                        .unwrap_or(super::EffectArg::Unknown),
                    super::EffectPassMode::ByPlace,
                )
            }
            EffectArgStyle::TempPlace => match provider.origin {
                EffectOrigin::With { value_expr } => (
                    super::EffectArg::Value(value_expr),
                    super::EffectPassMode::ByTempPlace,
                ),
                EffectOrigin::Param { .. } => {
                    (super::EffectArg::Unknown, super::EffectPassMode::Unknown)
                }
            },
            EffectArgStyle::Value => (
                match provider.origin {
                    EffectOrigin::With { value_expr } => super::EffectArg::Value(value_expr),
                    EffectOrigin::Param { .. } => provider
                        .binding
                        .map(super::EffectArg::Binding)
                        .unwrap_or(super::EffectArg::Unknown),
                },
                super::EffectPassMode::ByValue,
            ),
        }
    }

    fn provider_supports_mut(&mut self, provider: ProvidedEffect<'db>) -> bool {
        if let Some((kind, _)) = provider.ty.as_capability(self.db) {
            return matches!(kind, CapabilityKind::Mut)
                || self
                    .effect_provider_target_resolution(provider.ty, true)
                    .is_some();
        }

        provider.is_mut
            || self
                .effect_provider_target_resolution(provider.ty, true)
                .is_some()
    }

    fn inferred_provider_ty_for_effect_arg(
        &mut self,
        provider: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
        provider_target_ty: Option<TyId<'db>>,
    ) -> Option<TyId<'db>> {
        match pass_mode {
            super::EffectPassMode::ByValue => Some(self.table.fold_ty(self.db, provider.ty)),
            super::EffectPassMode::ByTempPlace => {
                let target_ty = self.table.fold_ty(self.db, provider_target_ty?);
                let mem_ptr_ctor =
                    resolve_lib_type_path(self.db, self.env.scope(), "core::effect_ref::MemPtr")?;
                Some(TyId::app(self.db, mem_ptr_ctor, target_ty))
            }
            super::EffectPassMode::ByPlace => {
                let binding = match arg {
                    super::EffectArg::Place(place) => {
                        let PlaceBase::Binding(binding) = place.base;
                        Some(binding)
                    }
                    super::EffectArg::Binding(binding) => Some(*binding),
                    super::EffectArg::Value(_) | super::EffectArg::Unknown => provider.binding,
                }?;

                let inferred = match binding {
                    LocalBinding::EffectParam { site, idx, .. } => self
                        .env
                        .resolved_provider_binding(site, idx)
                        .map(|binding| binding.provider_ty)
                        .unwrap_or(provider.ty),
                    LocalBinding::Param {
                        site: ParamSite::EffectField(effect_site),
                        ..
                    } => {
                        let contract = match effect_site {
                            EffectParamSite::Contract(contract)
                            | EffectParamSite::ContractInit { contract }
                            | EffectParamSite::ContractRecvArm { contract, .. } => contract,
                            EffectParamSite::Func(_) => {
                                unreachable!(
                                    "effect field bindings cannot originate from function sites"
                                )
                            }
                        };
                        let ident = binding.binding_name(&self.env);
                        contract
                            .fields(self.db)
                            .get(&ident)
                            .map(|field| field.declared_shape_ty())?
                    }
                    LocalBinding::Local { .. } | LocalBinding::Param { .. } => provider.ty,
                };

                Some(self.table.fold_ty(self.db, inferred))
            }
            super::EffectPassMode::Unknown => None,
        }
    }

    fn provider_target_ty_for_effect_arg(
        &mut self,
        provider: ProvidedEffect<'db>,
        instantiated_key_ty: Option<TyId<'db>>,
        required_mut: bool,
    ) -> Option<TyId<'db>> {
        let instantiated_key_ty = instantiated_key_ty.map(|ty| self.table.fold_ty(self.db, ty));
        if let Some(target_ty) = instantiated_key_ty {
            let semantics =
                provider_semantics(self.db, self.env.scope(), self.env.assumptions(), target_ty);
            if matches!(
                semantics.evidence,
                ProviderLayoutEvidence::ResolvedHandle(_)
            ) {
                return semantics
                    .target_ty
                    .map(|target_ty| self.table.fold_ty(self.db, target_ty));
            }
            if matches!(semantics.evidence, ProviderLayoutEvidence::InvalidHandle(_)) {
                return None;
            }
        }
        let provider_ty = self.table.fold_ty(self.db, provider.ty);
        self.effect_provider_target_resolution(provider_ty, required_mut)
            .map(|resolution| self.table.fold_ty(self.db, resolution.target_ty))
            .or(instantiated_key_ty)
    }

    fn effect_arg_is_valid(
        &self,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
    ) -> bool {
        match pass_mode {
            super::EffectPassMode::ByPlace => matches!(arg, &super::EffectArg::Place(_)),
            super::EffectPassMode::ByTempPlace => matches!(arg, &super::EffectArg::Value(_)),
            super::EffectPassMode::ByValue => !matches!(
                arg,
                &super::EffectArg::Unknown | &super::EffectArg::Place(_)
            ),
            super::EffectPassMode::Unknown => false,
        }
    }

    fn effect_arg_provider_space(
        &self,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
    ) -> Option<super::ProviderAddressSpace> {
        match pass_mode {
            super::EffectPassMode::ByTempPlace => match arg {
                super::EffectArg::Value(expr) => self.env.typed_expr(*expr).and_then(|prop| {
                    prop.borrow_provider
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(prop.ty))
                }),
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::ByPlace => match arg {
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Value(_) | super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::ByValue => match arg {
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Value(expr) => self.env.typed_expr(*expr).and_then(|prop| {
                    prop.borrow_provider
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(prop.ty))
                }),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::Unknown => None,
        }
    }

    fn effect_arg_provider_space_can_remain_unresolved(&self, arg: &super::EffectArg<'db>) -> bool {
        let binding = match arg {
            super::EffectArg::Place(place) => {
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => None,
        };

        matches!(binding, Some(LocalBinding::EffectParam { .. }))
    }

    fn specialize_effect_provider_binding(
        &mut self,
        slot: ProviderBinding<'db>,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
        provider_target_ty: Option<TyId<'db>>,
        provider_space: Option<super::ProviderAddressSpace>,
    ) -> EffectProviderSpecialization<'db> {
        let provenance = self
            .effect_provider_provenance(provided, arg)
            .unwrap_or_else(|| {
                panic!(
                    "missing call-site provider provenance for {:?} in {:?}",
                    slot.provider_idx,
                    self.env.owner(),
                )
            });
        let target_ty = provider_target_ty.map(|ty| self.table.fold_ty(self.db, ty));
        let provider = self
            .existing_provider_binding_for_effect_arg(provided, arg)
            .filter(|provider| {
                target_ty.is_none_or(|target_ty| provider.effective_target_ty() == target_ty)
            })
            .map(|provider| ProviderBinding {
                provider_idx: slot.provider_idx,
                ..provider
            })
            .unwrap_or_else(|| {
                let provider_ty = self
                    .inferred_provider_ty_for_effect_arg(
                        provided,
                        arg,
                        pass_mode,
                        provider_target_ty,
                    )
                    .unwrap_or_else(|| self.table.fold_ty(self.db, provided.ty));
                let semantics = provider_semantics_for_specialized_call(
                    self.db,
                    self.env.scope(),
                    self.env.assumptions(),
                    provider_ty,
                    target_ty,
                    provider_space,
                    match pass_mode {
                        super::EffectPassMode::ByPlace => ProviderTransport::ByPlace,
                        super::EffectPassMode::ByTempPlace => ProviderTransport::ByTempPlace,
                        super::EffectPassMode::ByValue | super::EffectPassMode::Unknown => {
                            ProviderTransport::ByValue
                        }
                    },
                );
                ProviderBinding {
                    provider_idx: slot.provider_idx,
                    provider_ty,
                    is_mut: provided.is_mut,
                    source: slot.source,
                    semantics,
                    layout_env: None,
                }
            });
        EffectProviderSpecialization {
            provider,
            provenance,
        }
    }

    fn existing_provider_binding_for_effect_arg(
        &self,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
    ) -> Option<ProviderBinding<'db>> {
        let binding = match arg {
            super::EffectArg::Place(place) => {
                if !place.projections.is_empty() {
                    return None;
                }
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => provided.binding,
        }?;
        match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx),
            LocalBinding::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            } => self.env.resolved_provider_binding(effect_site, idx),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        }
    }

    fn effect_provider_provenance(
        &self,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
    ) -> Option<EffectProviderProvenance<'db>> {
        let owner = self.env.owner();
        let binding = match arg {
            super::EffectArg::Place(place) => {
                if !place.projections.is_empty()
                    && let EffectOrigin::With { value_expr } = provided.origin
                {
                    return Some(EffectProviderProvenance::Expr {
                        owner,
                        expr: value_expr,
                    });
                }
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => provided.binding,
        };
        binding
            .map(|binding| EffectProviderProvenance::Binding { owner, binding })
            .or(match provided.origin {
                EffectOrigin::With { value_expr } => Some(EffectProviderProvenance::Expr {
                    owner,
                    expr: value_expr,
                }),
                EffectOrigin::Param { .. } => None,
            })
    }

    fn query_type_key(&self, key: &EffectPatternKey<'db>) -> Option<TyId<'db>> {
        match key {
            EffectPatternKey::Type(key) => Some(key.carrier),
            EffectPatternKey::Trait(_) => None,
        }
    }

    fn trait_effect_goal_satisfiability(
        &self,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        self.trait_effect_goal_satisfiability_in_scope(
            self.env.scope(),
            self.env.assumptions(),
            trait_goal,
        )
    }

    pub(super) fn trait_effect_goal_satisfiability_in_scope(
        &self,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        let solve_cx = TraitSolveCx::new(self.db, scope).with_assumptions(assumptions);
        let query = crate::analysis::ty::trait_resolution::CanonicalGoalQuery::new(
            self.db,
            trait_goal,
            assumptions,
        );
        crate::analysis::ty::trait_resolution::is_goal_query_satisfiable(self.db, solve_cx, &query)
            .clone()
    }

    pub(super) fn commit_trait_goal_solution(
        &mut self,
        trait_goal: TraitInstId<'db>,
        solution: Solution<TraitGoalSolution<'db>>,
    ) -> TraitGoalSolution<'db> {
        let canonical_goal = Canonicalized::new(self.db, trait_goal);
        let solved = canonical_goal.extract_solution(&mut self.table, solution);
        self.table.unify(trait_goal, solved.inst).unwrap();
        solved
    }

    fn commit_provider_target_resolution(
        &mut self,
        resolution: ProviderTargetResolution<'db>,
    ) -> TyId<'db> {
        self.commit_provider_target_resolution_in_scope(
            resolution,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    pub(super) fn commit_provider_target_resolution_in_scope(
        &mut self,
        resolution: ProviderTargetResolution<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> TyId<'db> {
        let mut target_ty = self.renormalize_effect_provider_target_ty_in_scope(
            resolution.target_seed_ty,
            scope,
            assumptions,
        );
        if let Some((handle_goal, handle_solution)) = resolution.handle_proof {
            self.commit_trait_goal_solution(handle_goal, handle_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        if let Some((effect_ref_goal, effect_ref_solution)) = resolution.effect_ref_proof {
            self.commit_trait_goal_solution(effect_ref_goal, effect_ref_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        if let Some((effect_ref_mut_goal, effect_ref_mut_solution)) =
            resolution.effect_ref_mut_proof
        {
            self.commit_trait_goal_solution(effect_ref_mut_goal, effect_ref_mut_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        target_ty
    }

    fn renormalize_effect_provider_target_ty_in_scope(
        &mut self,
        target_ty: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> TyId<'db> {
        normalize_ty(
            self.db,
            self.table.fold_ty(self.db, target_ty),
            scope,
            assumptions,
        )
        .fold_with(self.db, &mut self.table)
    }

    fn select_type_effect_binding_match(
        &mut self,
        pattern: TypePatternKey<'db>,
        provided: ProvidedEffect<'db>,
    ) -> Option<TypeEffectBindingMatch<'db>> {
        self.select_type_effect_binding_match_in_scope(
            pattern,
            provided,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    pub(super) fn select_type_effect_binding_match_in_scope(
        &mut self,
        pattern: TypePatternKey<'db>,
        provided: ProvidedEffect<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<TypeEffectBindingMatch<'db>> {
        let can_commit_key_relation = |this: &mut Self, given: TyId<'db>| {
            let snapshot = this.snapshot_state();
            let ok = apply_key_match_commit(
                this,
                KeyMatchCommit::QueryToType {
                    query: pattern.clone(),
                    actual: given,
                },
            );
            this.rollback_state(snapshot);
            ok
        };

        let matches_key = |this: &mut Self, actual_ty: TyId<'db>| {
            if can_commit_key_relation(this, actual_ty) {
                return true;
            }

            false
        };
        let direct_ty = if let Some((_, inner)) = provided.ty.as_capability(self.db) {
            inner
        } else {
            provided.ty
        };

        if matches_key(self, direct_ty) {
            return Some(TypeEffectBindingMatch::Direct { given: direct_ty });
        }

        self.effect_provider_target_resolution_in_scope(provided.ty, false, scope, assumptions)
            .and_then(|resolution| {
                matches_key(self, resolution.target_ty)
                    .then_some(TypeEffectBindingMatch::Provider { resolution })
            })
    }

    fn effect_provider_target_resolution(
        &mut self,
        provided_ty: TyId<'db>,
        required_mut: bool,
    ) -> Option<ProviderTargetResolution<'db>> {
        self.effect_provider_target_resolution_in_scope(
            provided_ty,
            required_mut,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    fn effect_provider_target_resolution_in_scope(
        &mut self,
        provided_ty: TyId<'db>,
        required_mut: bool,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<ProviderTargetResolution<'db>> {
        if let Some((kind, inner_ty)) = provided_ty.as_capability(self.db) {
            if required_mut && !matches!(kind, CapabilityKind::Mut) {
                return None;
            }
            return Some(ProviderTargetResolution::direct(inner_ty));
        }

        let effect_ref_trait = resolve_core_trait(self.db, scope, &["EffectRef"])
            .expect("missing required core trait `core::EffectRef`");
        let effect_ref_mut_trait = resolve_core_trait(self.db, scope, &["EffectRefMut"])
            .expect("missing required core trait `core::EffectRefMut`");
        let effect_handle_trait = resolve_core_trait(self.db, scope, &["EffectHandle"])
            .expect("missing required core trait `core::EffectHandle`");
        let target_ident = IdentId::new(self.db, "Target".to_string());
        let effect_handle_inst = TraitInstId::new(
            self.db,
            effect_handle_trait,
            vec![provided_ty],
            IndexMap::new(),
        );
        let GoalSatisfiability::Satisfied(handle_solution) =
            self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, effect_handle_inst)
        else {
            return None;
        };

        let snapshot = self.snapshot_state();
        let resolution = (|| {
            self.commit_trait_goal_solution(effect_handle_inst, handle_solution);

            let target_assoc = effect_handle_inst.assoc_ty(self.db, target_ident)?;
            let mut target_ty = normalize_ty(self.db, target_assoc, scope, assumptions)
                .fold_with(self.db, &mut self.table);
            let mut provided_ty = self.table.fold_ty(self.db, provided_ty);

            let effect_ref_inst = TraitInstId::new(
                self.db,
                effect_ref_trait,
                vec![provided_ty, target_ty],
                IndexMap::new(),
            );
            let GoalSatisfiability::Satisfied(effect_ref_solution) =
                self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, effect_ref_inst)
            else {
                return None;
            };
            self.commit_trait_goal_solution(effect_ref_inst, effect_ref_solution);
            provided_ty = self.table.fold_ty(self.db, provided_ty);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);

            let effect_ref_mut_proof = if required_mut {
                let effect_ref_mut_inst = TraitInstId::new(
                    self.db,
                    effect_ref_mut_trait,
                    vec![provided_ty, target_ty],
                    IndexMap::new(),
                );
                let GoalSatisfiability::Satisfied(effect_ref_mut_solution) = self
                    .trait_effect_goal_satisfiability_in_scope(
                        scope,
                        assumptions,
                        effect_ref_mut_inst,
                    )
                else {
                    return None;
                };
                self.commit_trait_goal_solution(effect_ref_mut_inst, effect_ref_mut_solution);
                target_ty = self.renormalize_effect_provider_target_ty_in_scope(
                    target_ty,
                    scope,
                    assumptions,
                );
                Some((effect_ref_mut_inst, effect_ref_mut_solution))
            } else {
                None
            };

            Some(ProviderTargetResolution {
                target_ty,
                target_seed_ty: target_assoc,
                handle_proof: Some((effect_handle_inst, handle_solution)),
                effect_ref_proof: Some((effect_ref_inst, effect_ref_solution)),
                effect_ref_mut_proof,
            })
        })();
        self.rollback_state(snapshot);
        resolution
    }

    pub(super) fn apply_effect_commit_plan(&mut self, commit: EffectCommitPlan<'db>) -> bool {
        let snapshot = self.snapshot_state();
        let ok = self.apply_effect_commit_plan_inner(commit);
        if ok {
            self.commit_state(snapshot);
        } else {
            self.rollback_state(snapshot);
        }
        ok
    }

    fn apply_effect_commit_plan_inner(&mut self, commit: EffectCommitPlan<'db>) -> bool {
        for (goal, solution) in commit.trait_solutions {
            self.commit_trait_goal_solution(goal, solution);
        }
        if let Some(resolution) = commit.provider_resolution {
            self.commit_provider_target_resolution(resolution);
        }
        if let Some(key_match) = commit.key_match
            && !apply_key_match_commit(self, key_match)
        {
            return false;
        }
        commit
            .extra_unifications
            .into_iter()
            .all(|(expected, given)| self.table.unify(expected, given).is_ok())
    }

    fn validate_keyed_with(
        &mut self,
        key_path: PathId<'db>,
        provider: ProvidedEffect<'db>,
        span: DynLazySpan<'db>,
    ) -> Result<
        (
            EffectWitness<'db, ProvidedEffect<'db>>,
            EffectCommitPlan<'db>,
        ),
        Box<EffectBarrier<'db>>,
    > {
        let Some(pattern) = build_barrier_pattern_for_with_key(self, key_path) else {
            let fallback = build_conservative_same_family_barrier_pattern_in_scope(
                self.db,
                self.env.scope(),
                self.env.assumptions(),
                key_path,
            );
            if let Some(EffectPatternKey::Trait(pattern)) = fallback.clone() {
                self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                    primary: span.clone(),
                    key: key_path,
                    trait_req: TraitInstId::new(
                        self.db,
                        pattern.def,
                        std::iter::once(provider.ty)
                            .chain(pattern.args_no_self.iter().copied())
                            .collect::<Vec<_>>(),
                        pattern
                            .assoc_bindings
                            .iter()
                            .copied()
                            .collect::<IndexMap<_, _>>(),
                    ),
                    given: provider.ty,
                });
                return Err(Box::new(EffectBarrier {
                    pattern: EffectPatternKey::Trait(pattern),
                    reason: BarrierReason::InvalidExplicitTraitKey { span, key_path },
                }));
            }
            let expected = TyId::invalid(self.db, InvalidCause::Other);
            self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                primary: span.clone(),
                key: key_path,
                expected,
                given: provider.ty,
            });
            return Err(Box::new(EffectBarrier {
                pattern: fallback.unwrap_or(EffectPatternKey::Type(TypePatternKey {
                    carrier: expected,
                    family: crate::analysis::ty::effects::effect_family_for_type(self.db, expected),
                    slots: crate::analysis::ty::effects::PatternSlots::empty(),
                })),
                reason: BarrierReason::InvalidExplicitTypeKey { span, key_path },
            }));
        };

        self.build_keyed_witness_from_pattern_in_scope(
            pattern,
            key_path,
            provider,
            span,
            KeyedWitnessBuildOptions {
                scope: KeyedWitnessBuildScope {
                    scope: self.env.scope(),
                    assumptions: self.env.base_assumptions(),
                },
                emit_diag: true,
                mode: WitnessBuildMode::ExplicitKeyedWith,
            },
        )
    }

    pub(super) fn build_keyed_witness_from_pattern_in_scope(
        &mut self,
        pattern: EffectPatternKey<'db>,
        key_path: PathId<'db>,
        provider: ProvidedEffect<'db>,
        span: DynLazySpan<'db>,
        options: KeyedWitnessBuildOptions<'db>,
    ) -> Result<
        (
            EffectWitness<'db, ProvidedEffect<'db>>,
            EffectCommitPlan<'db>,
        ),
        Box<EffectBarrier<'db>>,
    > {
        let KeyedWitnessBuildOptions {
            scope: KeyedWitnessBuildScope { scope, assumptions },
            emit_diag,
            mode,
        } = options;
        match pattern {
            EffectPatternKey::Type(pattern) => {
                if matches!(mode, WitnessBuildMode::ExplicitKeyedWith)
                    && pattern
                        .slots
                        .entries
                        .iter()
                        .any(|slot| slot.kind == PatternSlotKind::OmittedExplicitArg)
                {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                let Some(binding_match) =
                    self.select_type_effect_binding_match(pattern.clone(), provider)
                else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                let (actual, provider_resolution, transport) = match binding_match {
                    TypeEffectBindingMatch::Direct { given } => {
                        (given, None, WitnessTransport::Direct)
                    }
                    TypeEffectBindingMatch::Provider { resolution } => (
                        resolution.target_ty,
                        Some(resolution),
                        WitnessTransport::ByValue,
                    ),
                };
                let snapshot = self.snapshot_state();
                let specialized = self
                    .specialize_type_pattern_key(pattern.clone(), actual)
                    .map(|carrier| StoredTypeKey {
                        carrier,
                        family: pattern.family,
                    })
                    .and_then(|stored| {
                        finalize_stored_effect_key(
                            self.db,
                            StoredEffectKey::Type(stored),
                            scope,
                            assumptions,
                        )
                    });
                self.rollback_state(snapshot);
                let Some(key) = specialized.filter(|key| {
                    !matches!(
                        (mode, key),
                        (WitnessBuildMode::ExplicitKeyedWith, StoredEffectKey::Type(stored))
                            if stored_value_contains_implicit_layout_params(
                                self.db,
                                stored.carrier,
                            ) || stored_value_contains_out_of_scope_params(
                                self.db,
                                scope,
                                stored.carrier,
                            )
                    )
                }) else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                Ok((
                    EffectWitness {
                        key,
                        provider,
                        transport,
                    },
                    EffectCommitPlan {
                        key_match: Some(KeyMatchCommit::QueryToType {
                            query: pattern,
                            actual,
                        }),
                        trait_solutions: SmallVec::new(),
                        provider_resolution,
                        extra_unifications: SmallVec::new(),
                    },
                ))
            }
            EffectPatternKey::Trait(pattern) => {
                if matches!(mode, WitnessBuildMode::ExplicitKeyedWith)
                    && pattern
                        .slots
                        .entries
                        .iter()
                        .any(|slot| slot.kind == PatternSlotKind::OmittedExplicitArg)
                {
                    let trait_req = TraitInstId::new(
                        self.db,
                        pattern.def,
                        std::iter::once(provider.ty)
                            .chain(pattern.args_no_self.iter().copied())
                            .collect::<Vec<_>>(),
                        pattern
                            .assoc_bindings
                            .iter()
                            .copied()
                            .collect::<IndexMap<_, _>>(),
                    );
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            trait_req,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                if provider.ty.has_var(self.db) {
                    if emit_diag {
                        self.push_diag(BodyDiag::TypeAnnotationNeeded {
                            span: span.clone(),
                            ty: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::UnstableExplicitKeyedProvider {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                let mut table = UnificationTable::new(self.db);
                let (instantiated, slot_bindings) = instantiate_trait_pattern_in_with_bindings(
                    self.db,
                    &mut table,
                    pattern.clone(),
                );
                let args = std::iter::once(provider.ty)
                    .chain(instantiated.args(self.db).iter().skip(1).copied())
                    .collect::<Vec<_>>();
                let trait_goal = TraitInstId::new(
                    self.db,
                    instantiated.def(self.db),
                    args,
                    instantiated.assoc_type_bindings(self.db).clone(),
                );
                let GoalSatisfiability::Satisfied(solution) =
                    self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, trait_goal)
                else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            trait_req: trait_goal,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                let canonical_goal = Canonicalized::new(self.db, trait_goal);
                let solved = canonical_goal.extract_solution(&mut table, solution);
                let solved_inst = solved
                    .implementor
                    .trait_inst(self.db)
                    .fold_with(self.db, &mut table);
                let instantiated = reify_unresolved_pattern_slots(
                    self.db,
                    instantiated.fold_with(self.db, &mut table),
                    &slot_bindings,
                );
                let specialized = if matches!(mode, WitnessBuildMode::SeededRequirement)
                    && (stored_value_contains_implicit_layout_params(self.db, solved_inst)
                        || stored_value_contains_out_of_scope_params(self.db, scope, solved_inst))
                {
                    instantiated
                } else {
                    solved_inst
                };
                let Some(key) = finalize_stored_effect_key(
                    self.db,
                    StoredEffectKey::Trait(StoredTraitKey {
                        def: specialized.def(self.db),
                        args_no_self: specialized.args(self.db)[1..].iter().copied().collect(),
                        assoc_bindings: specialized
                            .assoc_ty_bindings(self.db)
                            .into_iter()
                            .collect(),
                        family: pattern.family,
                    }),
                    scope,
                    assumptions,
                ) else {
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey { span, key_path },
                    }));
                };
                Ok((
                    EffectWitness {
                        key,
                        provider,
                        transport: WitnessTransport::ByValue,
                    },
                    EffectCommitPlan {
                        key_match: None,
                        trait_solutions: SmallVec::new(),
                        provider_resolution: None,
                        extra_unifications: SmallVec::new(),
                    },
                ))
            }
        }
    }

    fn specialize_type_pattern_key(
        &mut self,
        pattern: TypePatternKey<'db>,
        actual: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let key_match = KeyMatchCommit::QueryToType {
            query: pattern,
            actual,
        };
        apply_key_match_commit(self, key_match).then(|| self.table.fold_ty(self.db, actual))
    }

    fn barrier_reason_for_pattern(
        &self,
        key_path: PathId<'db>,
        key: StoredEffectKey<'db>,
        span: DynLazySpan<'db>,
    ) -> BarrierReason<'db> {
        match key {
            StoredEffectKey::Type(_) => BarrierReason::InvalidExplicitTypeKey { span, key_path },
            StoredEffectKey::Trait(_) => BarrierReason::InvalidExplicitTraitKey { span, key_path },
        }
    }

    fn insert_effect_barrier(&mut self, barrier: EffectBarrier<'db>) {
        let Some(barrier) = self.refine_effect_barrier(barrier) else {
            return;
        };
        self.env
            .effect_env_mut()
            .insert_barrier(barrier.pattern.clone().family(), barrier);
    }

    fn refine_effect_barrier(&self, barrier: EffectBarrier<'db>) -> Option<EffectBarrier<'db>> {
        if self.barrier_pattern_is_precise(&barrier.pattern) {
            return Some(barrier);
        }

        // Exact invalid keyed barriers participate only when precise; otherwise we downgrade to
        // a same-family conservative barrier so invalid explicit keyed bindings still shadow outer
        // providers conservatively.
        let key_path = match &barrier.reason {
            BarrierReason::InvalidExplicitTypeKey { key_path, .. }
            | BarrierReason::InvalidExplicitTraitKey { key_path, .. }
            | BarrierReason::UnstableExplicitKeyedProvider { key_path, .. } => *key_path,
        };
        let pattern = build_conservative_same_family_barrier_pattern_in_scope(
            self.db,
            self.env.scope(),
            self.env.assumptions(),
            key_path,
        )?;
        Some(EffectBarrier {
            pattern,
            reason: barrier.reason,
        })
    }

    fn barrier_pattern_is_precise(&self, pattern: &EffectPatternKey<'db>) -> bool {
        !query_contains_unresolved_inference(self.db, pattern)
            && !contains_projection_or_invalid_query_state(self.db, pattern)
    }

    fn specialize_same_trait_method_inst(
        &self,
        method_name: IdentId<'db>,
        inst: TraitInstId<'db>,
    ) -> TraitInstId<'db> {
        let Some(CallableDef::Func(current_func)) = self.env.func() else {
            return inst;
        };
        if current_func.name(self.db).to_opt() != Some(method_name) {
            return inst;
        }

        let Some(enclosing_inst) = (match current_func.scope().parent_item(self.db) {
            Some(ItemKind::Trait(trait_)) => Some(TraitInstId::new(
                self.db,
                trait_,
                trait_.params(self.db).to_vec(),
                IndexMap::new(),
            )),
            Some(ItemKind::ImplTrait(impl_trait)) => impl_trait.trait_inst_result(self.db).ok(),
            _ => None,
        }) else {
            return inst;
        };

        if enclosing_inst.def(self.db) != inst.def(self.db) {
            return inst;
        }

        let enclosing_args = enclosing_inst.args(self.db);
        let inst_args = inst.args(self.db);
        if inst_args.len() != enclosing_args.len() || inst_args.is_empty() {
            return inst;
        }

        let mut args = inst_args.to_vec();
        args[1..].clone_from_slice(&enclosing_args[1..]);
        TraitInstId::new(
            self.db,
            inst.def(self.db),
            args,
            enclosing_inst.assoc_type_bindings(self.db).clone(),
        )
    }

    fn check_method_call(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::MethodCall(receiver, method_name, generic_args, args) = expr_data else {
            unreachable!()
        };
        let call_span = expr.span(self.body()).into_method_call_expr();
        let Some(method_name) = method_name.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let receiver_prop = self.check_expr_unknown(*receiver);
        if receiver_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }
        if matches!(method_name.data(self.db).as_str(), "call" | "call_once")
            && let Some(closure) = receiver_prop.ty.base_ty(self.db).as_closure(self.db)
        {
            self.constrain_callable_result_from_expected(closure.ret_ty(self.db), expected);
        }

        let (selected_receiver_ty, canonical_r_ty, candidate) =
            self.select_method_call_candidate(*receiver, &receiver_prop, method_name, None);
        let candidate = match candidate {
            Ok(candidate) => candidate,
            Err(MethodSelectionError::ReceiverTypeMustBeKnown) => {
                let ret_ty = self.fresh_ty();
                let typed = ExprProp::new(ret_ty, true);
                self.env.type_expr(expr, typed.clone());
                for arg in args.iter() {
                    self.check_expr_unknown(arg.expr);
                }
                self.env
                    .register_pending_method_lookup(PendingMethodLookup {
                        expr,
                        method_name,
                        span: call_span.method_name().into(),
                    });
                return typed;
            }
            Err(err) => {
                if matches!(&err, MethodSelectionError::NotFound)
                    && let Some(diag) = self.callable_field_method_syntax_diag(
                        *receiver,
                        &receiver_prop,
                        method_name,
                        args.len(),
                        call_span.clone().method_name().into(),
                    )
                {
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
                if let MethodSelectionError::AmbiguousTraitMethod(ambiguous) = err {
                    // Defer resolution using return-type constraints.
                    let ret_ty = self.fresh_ty();
                    let typed = ExprProp::new(ret_ty, true);
                    self.env.type_expr(expr, typed.clone());
                    // Still type-check argument expressions so they have types and can
                    // participate in later constraint solving.
                    for arg in args.iter() {
                        self.check_expr_unknown(arg.expr);
                    }
                    let candidates = ambiguous
                        .candidates
                        .into_iter()
                        .map(|candidate| {
                            let inst = canonical_r_ty
                                .extract_solution(&mut self.table, candidate.cand.inst);
                            let inst = self.specialize_same_trait_method_inst(method_name, inst);
                            super::env::PendingMethodCandidate {
                                inst,
                                method: candidate.cand.method,
                                needs_confirmation: candidate.needs_confirmation,
                                priority: 0,
                            }
                        })
                        .collect();

                    self.env.register_pending_method(super::env::PendingMethod {
                        expr,
                        recv_ty: selected_receiver_ty,
                        method_name,
                        candidates,
                        span: call_span.method_name().into(),
                        callee_is_receiver: false,
                    });
                    return typed;
                }
                let diag = body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(selected_receiver_ty, receiver.span(self.body()).into()),
                    Spanned::new(method_name, call_span.method_name().into()),
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        self.check_selected_method_call(
            expr,
            *receiver,
            method_name,
            *generic_args,
            args,
            receiver_prop,
            selected_receiver_ty,
            canonical_r_ty,
            candidate,
            false,
            expected,
            false,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn check_selected_method_call(
        &mut self,
        expr: ExprId,
        receiver: ExprId,
        method_name: IdentId<'db>,
        generic_args: crate::hir_def::GenericArgListId<'db>,
        args: &[HirCallArg<'db>],
        receiver_prop: ExprProp<'db>,
        selected_receiver_ty: TyId<'db>,
        canonical_r_ty: Canonicalized<'db, TyId<'db>>,
        candidate: MethodCandidate<'db>,
        already_typed: bool,
        expected: TyId<'db>,
        defer_result_reconciliation: bool,
    ) -> ExprProp<'db> {
        let call_span = expr.span(self.body()).into_method_call_expr();
        let needs_confirmation = matches!(candidate, MethodCandidate::NeedsConfirmation(_));
        let (func_ty, trait_inst) = match candidate {
            MethodCandidate::InherentMethod(cand) => (
                self.extract_inherent_method_to_term(&canonical_r_ty, cand, selected_receiver_ty),
                None,
            ),

            MethodCandidate::TraitMethod(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_receiver_ty, inst);
                (func_ty, Some(inst))
            }

            MethodCandidate::NeedsConfirmation(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_receiver_ty, inst);
                (func_ty, Some(inst))
            }
        };

        let mut callable = match Callable::new(
            self.db,
            func_ty,
            receiver.span(self.body()).into(),
            trait_inst,
        ) {
            Ok(callable) => callable,
            Err(diag) => {
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        let anchor = HoleAnchor::BodySyntax {
            body: self.body(),
            site: BodyHoleSite::Expr(expr),
        };
        if !callable.unify_generic_args(
            self,
            generic_args,
            anchor,
            call_span.clone().generic_args(),
        ) {
            return ExprProp::invalid(self.db);
        }
        if !defer_result_reconciliation {
            self.constrain_callable_result_from_expected(callable.ret_ty(self.db), expected);
        }

        if !callable.callable_def.is_method(self.db) {
            let diag = BodyDiag::NotAMethod {
                span: call_span,
                receiver_ty: receiver_prop.ty,
                func_name: method_name,
                func_ty,
            };
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if let Some(kind) = self
            .code_region_intrinsic_kind(callable.callable_def())
            .or_else(|| self.code_region_method_kind(selected_receiver_ty, method_name))
            && args.len() == 1
            && let Some(result) = self.check_code_region_intrinsic(
                expr,
                &mut callable,
                args,
                kind,
                Some((receiver, receiver_prop.clone())),
                None,
            )
        {
            if needs_confirmation && let Some(goal) = callable.trait_inst() {
                self.env.register_trait_obligation(TraitObligation {
                    goal,
                    origin: TraitObligationOrigin::GenericConfirmation { expr },
                    span: call_span.clone().into(),
                });
            }
            return result;
        }

        callable.check_args(
            self,
            args,
            call_span.clone().args(),
            Some((receiver, receiver_prop)),
            already_typed,
        );
        self.specialize_callable_layout_args(&mut callable, Some(receiver), args);

        if self.call_args_include_closure(args) {
            self.eagerly_process_callable_constraints(
                &callable,
                expr,
                call_span.clone().method_name().into(),
            );
        }

        // Check required effects for the method call
        self.check_callable_effects(expr, &mut callable);

        callable.process_constraints(self, expr, call_span.clone().method_name().into());

        let ret_ty = callable.ret_ty(self.db);
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        if let Some(kind) = self.const_intrinsic_kind(callable.callable_def()) {
            self.env.register_const_intrinsic(expr, callable, kind);
        } else {
            self.env.register_semantic_call(expr, callable);
        }
        let mut result = ExprProp::new(normalized_ret_ty, true);
        if !self.closure_type_expectations.is_empty() && self.env.callable_expr(expr).is_some() {
            self.env.type_expr(expr, result.clone());
            let (replayed, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            if outcome == ClosureReplayOutcome::Replayed {
                result = replayed;
            }
        }
        if needs_confirmation
            && let Some(goal) = self.env.callable_expr(expr).and_then(Callable::trait_inst)
        {
            self.env.register_trait_obligation(TraitObligation {
                goal,
                origin: TraitObligationOrigin::GenericConfirmation { expr },
                span: call_span.into(),
            });
        }
        result
    }

    fn method_receiver_tys(
        &self,
        _receiver: ExprId,
        receiver_prop: &ExprProp<'db>,
    ) -> Vec<TyId<'db>> {
        self.capability_fallback_candidates(receiver_prop.ty)
    }

    fn callable_field_method_syntax_diag(
        &mut self,
        receiver: ExprId,
        receiver_prop: &ExprProp<'db>,
        field_name: IdentId<'db>,
        arg_count: usize,
        primary: DynLazySpan<'db>,
    ) -> Option<PathResDiag<'db>> {
        let receiver_ty = self.normalize_ty(receiver_prop.ty);
        let record_ty = receiver_ty
            .as_capability(self.db)
            .map_or(receiver_ty, |(_, payload)| payload);
        let field_ty = RecordLike::from_ty(record_ty).record_field_ty(self.db, field_name)?;
        if !self.ty_supports_direct_call(field_ty) {
            return None;
        }

        Some(PathResDiag::MethodNotFound {
            primary,
            method_name: field_name,
            receiver: Either::Left(record_ty),
            callable_field: Some(CallableFieldCallHint {
                receiver: receiver.span(self.body()).into(),
                arg_count,
            }),
        })
    }

    fn ty_supports_direct_call(&mut self, ty: TyId<'db>) -> bool {
        let ty = self.normalize_ty(ty);
        if ty.is_func(self.db) || ty.as_closure(self.db).is_some() {
            return true;
        }

        [ClosureCallTrait::Fn, ClosureCallTrait::FnOnce]
            .into_iter()
            .any(|call_trait| {
                let Some(trait_def) = call_trait.trait_def(self.db, self.env.scope()) else {
                    return false;
                };
                let method_name = IdentId::new(self.db, call_trait.method_name().to_string());
                let canonical_ty = Canonicalized::new(self.db, ty);
                matches!(
                    select_method_candidate(
                        self.db,
                        &canonical_ty,
                        method_name,
                        self.env.scope(),
                        self.env.assumptions(),
                        Some(trait_def),
                    ),
                    Ok(_) | Err(MethodSelectionError::AmbiguousTraitMethod(_))
                )
            })
    }

    fn select_method_call_candidate(
        &self,
        receiver: ExprId,
        receiver_prop: &ExprProp<'db>,
        method_name: IdentId<'db>,
        trait_: Option<crate::hir_def::Trait<'db>>,
    ) -> (
        TyId<'db>,
        Canonicalized<'db, TyId<'db>>,
        Result<MethodCandidate<'db>, MethodSelectionError<'db>>,
    ) {
        let receiver_tys = self.method_receiver_tys(receiver, receiver_prop);
        let method_assumptions = self.env.assumptions();
        let mut selected_receiver_ty = receiver_tys[0];
        let mut canonical_r_ty = Canonicalized::new(self.db, selected_receiver_ty);
        let mut candidate = select_method_candidate(
            self.db,
            &canonical_r_ty,
            method_name,
            self.env.scope(),
            method_assumptions,
            trait_,
        );
        if matches!(
            candidate,
            Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
        ) {
            for &receiver_ty in receiver_tys.iter().skip(1) {
                let fallback_canonical = Canonicalized::new(self.db, receiver_ty);
                let fallback = select_method_candidate(
                    self.db,
                    &fallback_canonical,
                    method_name,
                    self.env.scope(),
                    method_assumptions,
                    trait_,
                );
                if fallback.is_ok() || !matches!(fallback, Err(MethodSelectionError::NotFound)) {
                    selected_receiver_ty = receiver_ty;
                    canonical_r_ty = fallback_canonical;
                    candidate = fallback;
                    break;
                }
            }
        }
        (selected_receiver_ty, canonical_r_ty, candidate)
    }

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Path(path) = expr_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };
        let path = *path;

        let path_expr_span = expr.span(self.body()).into_path_expr();
        let path_span = path_expr_span.clone().path();

        let is_call_callee = self.env.parent_expr().is_some_and(|parent| {
            matches!(
                self.body().exprs(self.db)[parent],
                Partial::Present(Expr::Call(callee, _)) if callee == expr
            )
        });

        let idx = path.segment_index(self.db);
        let generic_args = path.generic_args(self.db);
        let generic_args_span = path_span.clone().segment(idx).generic_args();
        let anchor = HoleAnchor::BodySyntax {
            body: self.body(),
            site: BodyHoleSite::Expr(expr),
        };
        let minter = HoleMinter::new(anchor);
        let unify_generic_args = |tc: &mut Self, callable: &mut Callable<'db>| {
            callable.unify_generic_args(tc, generic_args, anchor, generic_args_span.clone())
        };

        let res = if path.is_bare_ident(self.db) {
            let ident_span: DynLazySpan<'db> = path_expr_span.clone().into();
            resolve_ident_expr(self.db, &self.env, path, ident_span, &minter)
        } else {
            match self.resolve_path(path, true, path_span.clone(), &minter) {
                Ok(r) => ResolvedPathInBody::Reso(r),
                Err(err) => {
                    let expected_kind = if is_call_callee {
                        ExpectedPathKind::Function
                    } else {
                        ExpectedPathKind::Value
                    };

                    if let Some(diag) =
                        err.into_diag(self.db, path, path_span.clone(), expected_kind)
                    {
                        self.push_diag(diag)
                    }
                    ResolvedPathInBody::Invalid
                }
            }
        };

        match res {
            ResolvedPathInBody::Binding(binding) => {
                let ty = self
                    .env
                    .lookup_binding_ty(&binding)
                    .fold_with(self.db, &mut self.table);
                let ty = self.normalize_ty(ty);
                let capture_ty = match binding {
                    LocalBinding::EffectParam { is_mut, .. }
                        if self.env.binding_is_capture(binding)
                            && ty.as_capability(self.db).is_none() =>
                    {
                        if is_mut {
                            TyId::borrow_mut_of(self.db, ty)
                        } else {
                            TyId::borrow_ref_of(self.db, ty)
                        }
                    }
                    _ => ty,
                };
                self.env.record_capture_if_needed(binding, capture_ty);
                let is_mut = self.projected_place_mutability(binding.is_mut(), ty);
                ExprProp {
                    ty,
                    is_mut,
                    binding: Some(binding),
                    borrow_provider: self.concrete_borrow_provider_for_binding(binding),
                    path_read_semantics: None,
                    value_access: ValueAccess::Infer,
                }
            }
            ResolvedPathInBody::NewBinding(ident) => {
                let diag = BodyDiag::UndefinedVariable(path_expr_span.into(), ident);
                self.push_diag(diag);

                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Diag(diag) => {
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Invalid => ExprProp::invalid(self.db),

            ResolvedPathInBody::Reso(reso) => match reso {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    if let Some(const_ty_ty) = ty.const_ty_ty(self.db) {
                        self.env
                            .register_value_path_ref(expr, ValuePathRef::TypeConst(ty));
                        ExprProp::new(self.table.instantiate_to_term(const_ty_ty), true)
                    } else {
                        let diag = if ty.is_struct(self.db) {
                            let record_like = RecordLike::from_ty(ty);
                            BodyDiag::unit_variant_expected(
                                self.db,
                                path_expr_span.clone().into(),
                                record_like,
                            )
                        } else {
                            BodyDiag::NotValue {
                                primary: path_expr_span.clone().into(),
                                given: Either::Right(ty),
                            }
                        };
                        self.push_diag(diag);

                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::Func(ty) => {
                    let mut callable =
                        Callable::new(self.db, ty, expr.span(self.body()).into(), None)
                            .expect("function item path should resolve to callable");
                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    let ty = self.instantiate_to_term(callable.ty(self.db));
                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(ty, true)
                }
                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::EnumVariant(variant) => {
                    let ty = match variant.kind(self.db) {
                        VariantKind::Unit => {
                            self.env
                                .register_value_path_ref(expr, ValuePathRef::UnitVariant(variant));
                            variant.ty
                        }
                        VariantKind::Tuple(_) => {
                            let ty = variant.constructor_func_ty(self.db).unwrap();
                            self.env
                                .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                            self.instantiate_to_term(ty)
                        }
                        VariantKind::Record(_) => {
                            let record_like = RecordLike::from_variant(variant);
                            let diag = BodyDiag::unit_variant_expected(
                                self.db,
                                expr.span(self.body()).into(),
                                record_like,
                            );
                            self.push_diag(diag);

                            TyId::invalid(self.db, InvalidCause::Other)
                        }
                    };

                    ExprProp::new(self.instantiate_to_term(ty), true)
                }
                PathRes::Const(const_def, ty) => {
                    self.env
                        .register_const_ref(expr, ConstRef::Const(const_def));
                    ExprProp::new(ty, true)
                }
                PathRes::Method(receiver_ty, candidate) => {
                    let canonical_r_ty = Canonicalized::new(self.db, receiver_ty);
                    let (method_ty, trait_inst) = match candidate {
                        MethodCandidate::InherentMethod(cand) => (
                            self.extract_inherent_method_to_term(
                                &canonical_r_ty,
                                cand,
                                receiver_ty,
                            ),
                            None,
                        ),
                        MethodCandidate::TraitMethod(cand)
                        | MethodCandidate::NeedsConfirmation(cand) => {
                            let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                            let inst = if inst
                                .self_ty(self.db)
                                .kind(self.db)
                                .does_match(receiver_ty.kind(self.db))
                            {
                                let mut args = inst.args(self.db).to_vec();
                                if let Some(self_arg) = args.first_mut() {
                                    *self_arg = receiver_ty;
                                }
                                TraitInstId::new(
                                    self.db,
                                    inst.def(self.db),
                                    args,
                                    inst.assoc_type_bindings(self.db).clone(),
                                )
                            } else {
                                inst
                            };
                            if matches!(candidate, MethodCandidate::NeedsConfirmation(_)) {
                                self.env.register_trait_obligation(TraitObligation {
                                    goal: inst,
                                    origin: TraitObligationOrigin::GenericConfirmation { expr },
                                    span: path_expr_span.clone().into(),
                                });
                            }
                            let method_ty = if cand.method.is_method(self.db) {
                                self.instantiate_trait_method_to_term(
                                    cand.method,
                                    receiver_ty,
                                    inst,
                                )
                            } else {
                                self.instantiate_trait_assoc_fn_to_term(
                                    cand.method.as_callable(self.db).unwrap(),
                                    inst,
                                )
                            };
                            (method_ty, Some(inst))
                        }
                    };

                    let mut callable = Callable::new(
                        self.db,
                        method_ty,
                        expr.span(self.body()).into(),
                        trait_inst,
                    )
                    .expect("method path should resolve to callable");

                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    let method_ty = callable.ty(self.db);
                    self.env.register_callable(expr, callable);
                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(method_ty, true)
                }
                PathRes::TraitMethod(trait_inst, method) => {
                    if let Some(existing) = self.env.callable_expr(expr) {
                        return ExprProp::new(existing.ty(self.db), true);
                    }

                    let inst = if matches!(
                        trait_inst.self_ty(self.db).data(self.db),
                        TyData::TyParam(param) if param.is_trait_self()
                    ) {
                        let old_self = trait_inst.self_ty(self.db);
                        let new_self = self.table.new_var_from_param(old_self);

                        struct ReplaceSelf<'db> {
                            from: TyId<'db>,
                            to: TyId<'db>,
                        }

                        impl<'db> TyFolder<'db> for ReplaceSelf<'db> {
                            fn fold_ty(
                                &mut self,
                                db: &'db dyn HirAnalysisDb,
                                ty: TyId<'db>,
                            ) -> TyId<'db> {
                                if ty == self.from {
                                    return self.to;
                                }
                                ty.super_fold_with(db, self)
                            }
                        }

                        let mut folder = ReplaceSelf {
                            from: old_self,
                            to: new_self,
                        };

                        let args = trait_inst
                            .args(self.db)
                            .iter()
                            .map(|&ty| ty.fold_with(self.db, &mut folder))
                            .collect::<Vec<_>>();

                        let assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>> = trait_inst
                            .assoc_type_bindings(self.db)
                            .iter()
                            .map(|(name, ty)| (*name, ty.fold_with(self.db, &mut folder)))
                            .collect();

                        TraitInstId::new(
                            self.db,
                            trait_inst.def(self.db),
                            args,
                            assoc_type_bindings,
                        )
                    } else {
                        trait_inst
                    };

                    self.env.register_trait_obligation(TraitObligation {
                        goal: inst,
                        origin: TraitObligationOrigin::GenericConfirmation { expr },
                        span: path_expr_span.clone().into(),
                    });

                    let func_ty = self.instantiate_trait_assoc_fn_to_term(
                        method.as_callable(self.db).unwrap(),
                        inst,
                    );

                    let mut callable =
                        Callable::new(self.db, func_ty, expr.span(self.body()).into(), Some(inst))
                            .expect("trait method path should resolve to callable");

                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    let func_ty = callable.ty(self.db);
                    self.env.register_callable(expr, callable);
                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(func_ty, true)
                }
                PathRes::TraitConst(recv_ty, inst, name) => {
                    let mut args = inst.args(self.db).clone();
                    if let Some(self_arg) = args.first_mut() {
                        *self_arg = recv_ty;
                    }
                    let inst = TraitInstId::new(
                        self.db,
                        inst.def(self.db),
                        args,
                        inst.assoc_type_bindings(self.db).clone(),
                    );

                    if !super::trait_const_goal_has_foreign_params(self.db, inst, self.env.scope())
                        && let GoalSatisfiability::UnSat(_) = is_goal_satisfiable(
                            self.db,
                            TraitSolveCx::new(self.db, self.env.scope())
                                .with_assumptions(self.env.assumptions()),
                            inst,
                        )
                    {
                        self.push_diag(TyDiagCollection::from(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: path_expr_span.clone().into(),
                                primary_goal: inst,
                                unsat_subgoal: None,
                                required_by: None,
                            },
                        ));
                        return ExprProp::invalid(self.db);
                    }

                    self.env.register_const_ref(
                        expr,
                        ConstRef::TraitConst(AssocConstUse::new(
                            self.env.scope(),
                            self.env.assumptions(),
                            inst,
                            name,
                        )),
                    );
                    // Look up the associated const's declared type in the trait and
                    // instantiate it with the trait instance's args (including Self).
                    let trait_ = inst.def(self.db);
                    if let Some(const_view) = trait_.const_(self.db, name)
                        && let Some(ty_binder) = const_view.ty_binder(self.db)
                    {
                        // Instantiate with the concrete args of the trait instance
                        let instantiated = ty_binder.instantiate(self.db, inst.args(self.db));
                        let ty = self.table.instantiate_to_term(instantiated);

                        ExprProp::new(ty, true)
                    } else {
                        // Fallback to invalid type if the declaration isn't found
                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::InherentConst(recv_ty, impl_, name) => {
                    if let Some(ty) = instantiate_inherent_const_decl_ty(
                        self.db,
                        &mut self.table,
                        impl_,
                        recv_ty,
                        name,
                    ) {
                        self.env.register_const_ref(
                            expr,
                            ConstRef::InherentConst(InherentConstUse::new(
                                self.env.scope(),
                                self.env.assumptions(),
                                impl_,
                                recv_ty,
                                name,
                            )),
                        );
                        ExprProp::new(ty, true)
                    } else {
                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::FuncParam(..) => {
                    unreachable!("func params should be resolved as bindings")
                }
            },
        }
    }

    fn check_record_init(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::RecordInit(path, ..) = expr_data else {
            unreachable!()
        };
        let span = expr.span(self.body()).into_record_init_expr();

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let path_span = span.clone().path();
        let minter = HoleMinter::new(HoleAnchor::BodySyntax {
            body: self.body(),
            site: BodyHoleSite::Expr(expr),
        });
        let reso = match self.resolve_path(*path, true, path_span.clone(), &minter) {
            Ok(reso) => reso,
            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, *path, path_span, ExpectedPathKind::Record)
                {
                    self.push_diag(diag);
                }
                return ExprProp::invalid(self.db);
            }
        };

        match reso {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                // Use the expected type to constrain the record's generic args
                // before checking fields. A structural layout hole unifies as
                // a wildcard, so a successful nominal match must also adopt
                // the expected application; that is how an assigned layout
                // view reaches aggregate construction.
                let snapshot = self.snapshot_state();
                let ty = if self.table.unify(ty, expected).is_ok() {
                    self.commit_state(snapshot);
                    self.canonical_nominal_ty_from_expected(ty, expected)
                } else {
                    self.rollback_state(snapshot);
                    ty.fold_with(self.db, &mut self.table)
                };

                let record_like = RecordLike::from_ty(ty);
                if record_like.is_record(self.db) {
                    self.env
                        .register_record_init_lowering(expr, RecordInitLowering::Struct);
                    self.check_record_init_fields(&record_like, expr);
                    ExprProp::new(ty, true)
                } else {
                    let diag =
                        BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
            }

            PathRes::Func(ty)
            | PathRes::Const(_, ty)
            | PathRes::TraitConst(ty, ..)
            | PathRes::InherentConst(ty, ..) => {
                let record_like = RecordLike::from_ty(ty);
                let diag =
                    BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                let diag = BodyDiag::record_expected(self.db, span.path().into(), None);
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }

            PathRes::EnumVariant(variant) => {
                // Constrain the variant type with the expected type before
                // checking fields (same rationale as record inits).
                let resolved_ty = variant.ty;
                let snapshot = self.snapshot_state();
                let ty = if self.table.unify(resolved_ty, expected).is_ok() {
                    self.commit_state(snapshot);
                    self.canonical_nominal_ty_from_expected(resolved_ty, expected)
                } else {
                    self.rollback_state(snapshot);
                    resolved_ty.fold_with(self.db, &mut self.table)
                };
                let variant = crate::analysis::name_resolution::ResolvedVariant { ty, ..variant };

                let record_like = RecordLike::from_variant(variant);
                if record_like.is_record(self.db) {
                    self.env.register_record_init_lowering(
                        expr,
                        RecordInitLowering::EnumVariant(variant),
                    );
                    self.check_record_init_fields(&record_like, expr);
                    ExprProp::new(ty, true)
                } else {
                    let diag = BodyDiag::record_expected(self.db, span.path().into(), None);
                    self.push_diag(diag);

                    ExprProp::invalid(self.db)
                }
            }
            PathRes::Mod(scope) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(scope.item()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::Trait(trait_) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(trait_.def(self.db).into()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
        }
    }

    fn check_record_init_fields(&mut self, record_like: &RecordLike<'db>, expr: ExprId) {
        let hir_db = self.db;

        let Partial::Present(Expr::RecordInit(_, fields)) = expr.data(hir_db, self.body()) else {
            unreachable!()
        };
        let span = expr.span(self.body()).into_record_init_expr().fields();

        let mut rec_checker = RecordInitChecker::new(self, record_like);

        for (i, field) in fields.iter().enumerate() {
            let label = field.label_eagerly(rec_checker.tc.db, rec_checker.tc.body());
            let field_span = span.clone().field(i).into();

            let expected = match rec_checker.feed_label(label, field_span) {
                Ok(ty) => ty,
                Err(diag) => {
                    rec_checker.tc.push_diag(diag);
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                }
            };

            let prop = rec_checker.tc.check_expr(field.expr, expected);
            rec_checker.tc.record_owned_value_use(field.expr, prop.ty);
        }

        if let Err(diag) = rec_checker.finalize(span.into(), false) {
            self.push_diag(diag);
        }
    }

    fn check_field(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Field(lhs, index) = expr_data else {
            unreachable!()
        };
        let Partial::Present(field) = index else {
            return ExprProp::invalid(self.db);
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        // let lhs_ty = normalize_ty(self.db, lhs_ty, self.env.scope(), self.env.assumptions());

        let (ty_base, _) = lhs_place_ty.decompose_ty_app(self.db);

        if ty_base.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if ty_base.is_ty_var(self.db) {
            self.env.register_pending_field(PendingField {
                expr,
                lhs: *lhs,
                field: *field,
            });
            return ExprProp::new(self.fresh_ty(), typed_lhs.is_mut);
        }

        let mut result = self.check_known_field(expr, *lhs, *field, typed_lhs, lhs_place_ty);
        if self.closure_expectation_for_type(expected).is_some()
            && let Some(base_expected) =
                self.replay_field_base_expected_ty(*lhs, *field, result.ty, expected)
        {
            let (_, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(*lhs, base_expected);
            if outcome == ClosureReplayOutcome::Replayed {
                result.ty = self.normalize_ty(expected);
            }
        }
        result
    }

    fn check_known_field(
        &mut self,
        expr: ExprId,
        lhs: ExprId,
        field: FieldIndex<'db>,
        typed_lhs: ExprProp<'db>,
        lhs_place_ty: TyId<'db>,
    ) -> ExprProp<'db> {
        let (_, ty_args) = lhs_place_ty.decompose_ty_app(self.db);
        let ty_base = lhs_place_ty;
        match field {
            FieldIndex::Ident(label) => {
                let record_like = RecordLike::from_ty(lhs_place_ty);
                if let Some(field_ty) = record_like.record_field_ty(self.db, label) {
                    if let Some(scope) = record_like.record_field_scope(self.db, label)
                        && !is_scope_visible_from(self.db, scope, self.env.scope())
                    {
                        // Check the visibility of the field.
                        let diag = PathResDiag::Invisible(
                            expr.span(self.body()).into_field_expr().accessor().into(),
                            label,
                            scope.name_span(self.db),
                        );

                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                    if let Some(field_index) =
                        resolve_place_field_index(self.db, lhs_place_ty, field)
                    {
                        self.env.register_resolved_field_index(expr, field_index);
                        if let Some(projected) =
                            self.contract_field_projected_field_ty(lhs, field_index)
                        {
                            let projected = self.table.fold_ty(self.db, projected);
                            let is_mut =
                                self.projected_place_mutability(typed_lhs.is_mut, projected);
                            return ExprProp::new(projected, is_mut);
                        }
                        if let Some(projected) =
                            self.callable_input_projected_field_ty(lhs, field_index)
                        {
                            let projected = self.table.fold_ty(self.db, projected);
                            let is_mut =
                                self.projected_place_mutability(typed_lhs.is_mut, projected);
                            return ExprProp::new(projected, is_mut);
                        }
                    }
                    let is_mut = self.projected_place_mutability(typed_lhs.is_mut, field_ty);
                    return ExprProp::new(field_ty, is_mut);
                }
            }

            FieldIndex::Index(i) => {
                let arg_len = ty_args.len().into();
                if ty_base.is_tuple(self.db) && i.data(self.db) < &arg_len {
                    let i: usize = i.data(self.db).try_into().unwrap();
                    if let Ok(field_index) = u16::try_from(i) {
                        self.env.register_resolved_field_index(expr, field_index);
                        if let Some(projected) =
                            self.contract_field_projected_field_ty(lhs, field_index)
                        {
                            let projected = self.table.fold_ty(self.db, projected);
                            let is_mut =
                                self.projected_place_mutability(typed_lhs.is_mut, projected);
                            return ExprProp::new(projected, is_mut);
                        }
                        if let Some(projected) =
                            self.callable_input_projected_field_ty(lhs, field_index)
                        {
                            let projected = self.table.fold_ty(self.db, projected);
                            let is_mut =
                                self.projected_place_mutability(typed_lhs.is_mut, projected);
                            return ExprProp::new(projected, is_mut);
                        }
                    }
                    let ty = ty_args[i];
                    let is_mut = self.projected_place_mutability(typed_lhs.is_mut, ty);
                    return ExprProp::new(ty, is_mut);
                }
            }
        };

        let diag = BodyDiag::AccessedFieldNotFound {
            primary: expr.span(self.body()).into(),
            given_ty: lhs_place_ty,
            index: field,
        };
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    fn contract_field_projected_field_ty(
        &self,
        lhs: ExprId,
        field_index: u16,
    ) -> Option<TyId<'db>> {
        let (field, view, mut projections) = self.contract_field_layout_context(lhs)?;
        projections.push(LayoutProjection::Field(field_index));
        let selection = field
            .selection_for_projections(self.db, view, &projections)
            .ok()?;
        self.selected_contract_layout_ty(field, view, &selection)
    }

    fn contract_field_projected_index_ty(&self, lhs: ExprId, index: ExprId) -> Option<TyId<'db>> {
        let (field, view, mut projections) = self.contract_field_layout_context(lhs)?;
        projections.push(LayoutProjection::Index(
            self.try_get_literal_int(index)
                .and_then(|value| value.data(self.db).to_usize()),
        ));
        let selection = field
            .selection_for_projections(self.db, view, &projections)
            .ok()?;
        self.selected_contract_layout_ty(field, view, &selection)
    }

    fn selected_contract_layout_ty(
        &self,
        field: &FieldStorageLayout<'db>,
        view: LayoutViewKind,
        selection: &crate::semantic::LayoutSelection,
    ) -> Option<TyId<'db>> {
        match field.projected_concrete_ty(self.db, view, selection) {
            Ok(ty) => Some(ty),
            Err(
                LayoutViewError::NonPhysicalRoot { .. }
                | LayoutViewError::RootNeedsLanding { .. }
                | LayoutViewError::RootNeedsIndex { .. },
            ) => field
                .project(self.db, view, selection)
                .ok()
                .map(|view| view.shape_ty()),
            Err(
                LayoutViewError::RootNotClassified { .. }
                | LayoutViewError::InvalidIndex { .. }
                | LayoutViewError::MissingAllocation { .. }
                | LayoutViewError::InvalidProjection,
            ) => None,
        }
    }

    fn contract_field_layout_context(
        &self,
        expr: ExprId,
    ) -> Option<(
        &'db FieldStorageLayout<'db>,
        LayoutViewKind,
        Vec<LayoutProjection>,
    )> {
        let place = self.env.expr_place(expr)?;
        self.contract_field_layout_context_for_place(&place)
    }

    fn contract_field_layout_context_for_place(
        &self,
        place: &Place<'db>,
    ) -> Option<(
        &'db FieldStorageLayout<'db>,
        LayoutViewKind,
        Vec<LayoutProjection>,
    )> {
        let PlaceBase::Binding(binding) = place.base;
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
        }?;
        let layout_env = provider.layout_env?;
        let field = layout_env
            .field
            .contract
            .storage_layout(self.db)
            .values()
            .find(|field| field.field == layout_env.field)?;
        let projections = place
            .projections
            .iter()
            .map(|projection| match projection {
                PlaceProjection::Field { index, .. } => LayoutProjection::Field(*index),
                PlaceProjection::Index { index_expr, .. } => {
                    let index = match index_expr.data(self.db, self.body()) {
                        Partial::Present(Expr::Lit(LitKind::Int(value))) => {
                            value.data(self.db).to_usize()
                        }
                        _ => None,
                    };
                    LayoutProjection::Index(index)
                }
            })
            .collect::<Vec<_>>();
        Some((field, layout_env.view, projections))
    }

    fn contract_field_layout_context_for_effect_arg(
        &self,
        arg: &super::EffectArg<'db>,
    ) -> Option<(
        &'db FieldStorageLayout<'db>,
        LayoutViewKind,
        Vec<LayoutProjection>,
    )> {
        match arg {
            super::EffectArg::Place(place) => self.contract_field_layout_context_for_place(place),
            super::EffectArg::Binding(binding) => self
                .contract_field_layout_context_for_place(&Place::new(PlaceBase::Binding(*binding))),
            super::EffectArg::Value(expr) => self.contract_field_layout_context(*expr),
            super::EffectArg::Unknown => None,
        }
    }

    pub(super) fn specialize_callable_layout_args(
        &self,
        callable: &mut Callable<'db>,
        receiver: Option<ExprId>,
        args: &[HirCallArg<'db>],
    ) {
        let CallableDef::Func(func) = callable.callable_def() else {
            return;
        };
        if let Some(receiver) = receiver {
            self.specialize_callable_layout_origin(
                callable,
                func,
                receiver,
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Receiver,
            );
        }
        let param_offset = usize::from(receiver.is_some());
        for (idx, arg) in args.iter().enumerate() {
            self.specialize_callable_layout_origin(
                callable,
                func,
                arg.expr,
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::ValueParam(
                    idx + param_offset,
                ),
            );
        }
    }

    fn specialize_callable_layout_origin(
        &self,
        callable: &mut Callable<'db>,
        func: crate::hir_def::Func<'db>,
        expr: ExprId,
        origin: crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin,
    ) {
        let Some((field, view, base_projections)) = self.contract_field_layout_context(expr) else {
            return;
        };
        self.specialize_callable_layout_context(
            callable,
            func,
            origin,
            field,
            view,
            &base_projections,
        );
    }

    fn specialize_callable_layout_context(
        &self,
        callable: &mut Callable<'db>,
        func: crate::hir_def::Func<'db>,
        origin: crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin,
        field: &FieldStorageLayout<'db>,
        view: LayoutViewKind,
        base_projections: &[LayoutProjection],
    ) {
        if let Ok(selection) = field.selection_for_projections(self.db, view, base_projections)
            && let Some(actual_ty) = self.selected_contract_layout_ty(field, view, &selection)
        {
            match origin {
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Receiver => {
                    callable.specialize_arg_from_actual(self.db, 0, actual_ty);
                }
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::ValueParam(idx) => {
                    callable.specialize_arg_from_actual(self.db, idx, actual_ty);
                }
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Effect(_) => {
                    if let Some(expected_ty) =
                        callable_input_layout_origin_ty(self.db, func, origin)
                    {
                        callable.specialize_params_from_actual(self.db, expected_ty, actual_ty);
                    }
                }
            }
        }
        for path in callable_input_layout_projection_paths(self.db, func, origin) {
            let Some(path_projections) = layout_projections_from_callable_path(&path) else {
                continue;
            };
            let mut projections = base_projections.to_vec();
            projections.extend(path_projections);
            let Ok(selection) = field.selection_for_projections(self.db, view, &projections) else {
                continue;
            };
            let Some(actual_ty) = self.selected_contract_layout_ty(field, view, &selection) else {
                continue;
            };
            instantiate_callable_projection_layout_args(
                self.db,
                func,
                origin,
                &path,
                actual_ty,
                callable.generic_args_mut(),
            );
        }
    }

    fn specialize_callable_effect_layout_projections(
        &self,
        callable: &mut Callable<'db>,
        callee: crate::hir_def::Func<'db>,
        effect_idx: usize,
        provider: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
    ) {
        let callee_origin =
            crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Effect(effect_idx);
        if let Some((field, view, base_projections)) =
            self.contract_field_layout_context_for_effect_arg(arg)
        {
            self.specialize_callable_layout_context(
                callable,
                callee,
                callee_origin,
                field,
                view,
                &base_projections,
            );
            return;
        }
        let Some(LocalBinding::EffectParam {
            site: EffectParamSite::Func(caller),
            idx: caller_effect_idx,
            ..
        }) = provider.binding
        else {
            return;
        };
        let caller_origin =
            crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Effect(caller_effect_idx);
        for path in callable_input_layout_projection_paths(self.db, callee, callee_origin) {
            let Some(actual_ty) =
                callable_input_projected_layout_ty(self.db, caller, caller_origin, &path)
            else {
                continue;
            };
            instantiate_callable_projection_layout_args(
                self.db,
                callee,
                callee_origin,
                &path,
                actual_ty,
                callable.generic_args_mut(),
            );
        }
    }

    pub(super) fn pattern_layout_context(&self, expr: ExprId) -> Option<PatternLayoutContext<'db>> {
        self.pattern_layout_context_for_projection(expr, &[])
    }

    pub(super) fn pattern_layout_context_for_projection(
        &self,
        expr: ExprId,
        projection: &[LayoutBundlePathStep],
    ) -> Option<PatternLayoutContext<'db>> {
        if let Some((field, view, mut base_projections)) = self.contract_field_layout_context(expr)
        {
            base_projections.extend(layout_projections_from_callable_path(projection)?);
            return Some(PatternLayoutContext::ContractField {
                field,
                view,
                base_projections,
            });
        }
        let place = self.env.expr_place(expr)?;
        let (func, origin) = match place.base {
            PlaceBase::Binding(LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            }) => {
                let param = func.params(self.db).nth(idx)?;
                let origin = if param.is_self_param(self.db) {
                    crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Receiver
                } else {
                    crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::ValueParam(idx)
                };
                (func, origin)
            }
            PlaceBase::Binding(LocalBinding::EffectParam {
                site: EffectParamSite::Func(func),
                idx,
                ..
            })
            | PlaceBase::Binding(LocalBinding::Param {
                site: ParamSite::EffectField(EffectParamSite::Func(func)),
                idx,
                ..
            }) => (
                func,
                crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Effect(idx),
            ),
            PlaceBase::Binding(
                LocalBinding::Local { .. }
                | LocalBinding::Param { .. }
                | LocalBinding::EffectParam { .. },
            ) => return None,
        };
        let mut base_path = place
            .projections
            .iter()
            .map(|projection| match projection {
                PlaceProjection::Field { index, .. } => LayoutBundlePathStep::Field(*index),
                PlaceProjection::Index { .. } => LayoutBundlePathStep::Index,
            })
            .collect::<Vec<_>>();
        base_path.extend_from_slice(projection);
        Some(PatternLayoutContext::CallableInput {
            func,
            origin,
            base_path,
        })
    }

    pub(super) fn projected_pattern_layout_ty(
        &self,
        context: &PatternLayoutContext<'db>,
        path: &[LayoutBundlePathStep],
    ) -> Option<TyId<'db>> {
        match context {
            PatternLayoutContext::ContractField {
                field,
                view,
                base_projections,
            } => {
                let mut projections = base_projections.clone();
                projections.extend(layout_projections_from_callable_path(path)?);
                let selection = field
                    .selection_for_projections(self.db, *view, &projections)
                    .ok()?;
                self.selected_contract_layout_ty(field, *view, &selection)
            }
            PatternLayoutContext::CallableInput {
                func,
                origin,
                base_path,
            } => {
                let mut projection = base_path.clone();
                projection.extend_from_slice(path);
                callable_input_projected_layout_ty(self.db, *func, *origin, &projection)
            }
        }
    }

    fn callable_input_projected_field_ty(
        &self,
        lhs: ExprId,
        field_index: u16,
    ) -> Option<TyId<'db>> {
        let place = self.env.expr_place(lhs)?;
        let PlaceBase::Binding(LocalBinding::Param {
            site: ParamSite::Func(func),
            idx,
            ..
        }) = place.base
        else {
            return None;
        };
        let param = func.params(self.db).nth(idx)?;
        let origin = if param.is_self_param(self.db) {
            crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Receiver
        } else {
            crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::ValueParam(idx)
        };
        let mut path = place
            .projections
            .iter()
            .map(|projection| match projection {
                PlaceProjection::Field { index, .. } => LayoutBundlePathStep::Field(*index),
                PlaceProjection::Index { .. } => LayoutBundlePathStep::Index,
            })
            .collect::<Vec<_>>();
        path.push(LayoutBundlePathStep::Field(field_index));
        callable_input_carrier_projected_layout_ty(self.db, func, origin, &path)
    }

    fn check_tuple(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Tuple(elems) = expr_data else {
            unreachable!()
        };

        let elem_tys = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) && args.len() == elems.len() => args.to_vec(),
            _ => self.fresh_tys_n(elems.len()),
        };

        for (elem, elem_ty) in elems.iter().zip(elem_tys.iter()) {
            let prop = self.check_expr(*elem, *elem_ty);
            self.record_owned_value_use(*elem, prop.ty);
        }

        let ty = TyId::tuple_with_elems(self.db, &elem_tys);
        ExprProp::new(ty, true)
    }

    fn check_array(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Array(elems) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        for elem in elems {
            let prop = self.check_expr(*elem, expected_elem_ty);
            expected_elem_ty = prop.ty;
            self.record_owned_value_use(*elem, expected_elem_ty);
        }

        let ty = TyId::array_with_len(self.db, expected_elem_ty, elems.len());
        ExprProp::new(ty, true)
    }

    fn check_array_rep(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::ArrayRep(elem, len) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        let prop = self.check_expr(*elem, expected_elem_ty);
        expected_elem_ty = prop.ty;
        // A deferred callable may later replay a contextual capability into
        // this element (for example `T` -> `view T`). Diagnose Copy only after
        // all such replay has finalized the element type.
        self.pending_array_repeat_copy_checks.push(expr);

        let array = TyId::array(self.db, expected_elem_ty);
        let ty = if let Some(len_body) = len.to_opt() {
            let expected_len_ty = array
                .applicable_ty(self.db)
                .and_then(|applicable| applicable.const_ty);

            let len_ty = ConstTyId::from_body(self.db, len_body, expected_len_ty, None);
            let len_ty = TyId::const_ty(self.db, len_ty);
            let array_ty = TyId::app(self.db, array, len_ty);

            if let Some(diag) = array_ty.emit_diag(self.db, len_body.span().into()) {
                self.push_diag(diag);
            }

            let mut requires_known_const = false;
            if !array_ty.has_invalid(self.db)
                && let (_, args) = array_ty.decompose_ty_app(self.db)
                && let Some(len_ty) = args.get(1)
                && let TyData::ConstTy(const_ty) = len_ty.data(self.db)
                && !self.array_len_const_is_acceptable(*const_ty)
            {
                requires_known_const = true;
                self.push_diag(BodyDiag::ConstValueMustBeKnown(len_body.span().into()));
            }

            if requires_known_const && expected.base_ty(self.db).is_array(self.db) {
                expected
            } else {
                array_ty
            }
        } else {
            let len_ty = ConstTyId::invalid(self.db, InvalidCause::ParseError);
            let len_ty = TyId::const_ty(self.db, len_ty);
            TyId::app(self.db, array, len_ty)
        };

        ExprProp::new(ty, true)
    }

    pub(super) fn finalize_array_repeat_copy_checks(&mut self) {
        for expr in std::mem::take(&mut self.pending_array_repeat_copy_checks) {
            let Partial::Present(Expr::ArrayRep(elem, _)) = expr.data(self.db, self.body()) else {
                continue;
            };
            let Some(prop) = self.env.typed_expr(*elem) else {
                continue;
            };
            let ty = self.normalize_ty(prop.ty);
            if !ty.has_invalid(self.db) && !ty.has_var(self.db) && !self.ty_is_copy(ty) {
                self.push_diag(BodyDiag::ArrayRepeatRequiresCopy {
                    primary: elem.span(self.body()).into(),
                    ty,
                });
            }
        }
    }

    /// Whether `const_ty` is acceptable as an array-repeat length: a known
    /// literal, or a symbolic const that resolves per monomorphization — a bare
    /// const param (`N`) or a bare trait-const projection (`T::N`). The latter
    /// two stay symbolic during checking and become concrete once the owning
    /// type parameters are.
    fn array_len_const_is_acceptable(&self, const_ty: ConstTyId<'db>) -> bool {
        match const_ty.data(self.db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(_), _) | ConstTyData::TyParam(..) => {
                true
            }
            ConstTyData::Abstract(expr, _) => {
                matches!(expr.data(self.db), ConstExpr::TraitConst(_))
            }
            _ => false,
        }
    }

    fn check_if(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        // Keep let-chain bindings scoped to this conditional so they can flow
        // into the then branch (and chained `&&` segments) without leaking to
        // the enclosing scope.
        self.env.enter_lexical_scope();
        self.check_cond(*cond);

        match else_ {
            Some(else_) => {
                let has_contextual_closure_expectation =
                    self.closure_expectation_for_type(expected).is_some();
                let infer_result = !result_discarded
                    && !has_contextual_closure_expectation
                    && matches!(
                        self.normalize_ty(expected).data(self.db),
                        TyData::TyVar(var) if var.sort == TyVarSort::General
                    );
                let capability_expected =
                    !infer_result && self.normalize_ty(expected).as_capability(self.db).is_some();
                let assignment_context = result_expectation.is_capability_assignment();
                self.env.enter_scope(*then);
                self.env.flush_pending_bindings();
                let then_pre_never = !assignment_context
                    && capability_expected
                    && self.assignment_rhs_outcome(*then) == AssignmentRhsOutcome::Never;
                let then_expected = if infer_result || then_pre_never {
                    self.fresh_ty()
                } else {
                    expected
                };
                let mut then_prop = if assignment_context && !result_discarded {
                    self.check_expr_with_result_context(*then, result_expectation, false)
                } else if result_discarded {
                    self.check_expr_with_discarded_result(*then, then_expected)
                } else {
                    self.check_expr(*then, then_expected)
                };
                if then_pre_never {
                    // Check an unreachable branch without pushing a capability
                    // expectation into dead payload expressions, but retain the
                    // established contextual type on the branch expression.
                    then_prop.ty = expected;
                    self.env.type_expr(*then, then_prop.clone());
                }
                self.env.leave_scope();
                self.env.clear_pending_bindings();
                self.env.leave_scope();
                let else_pre_never = !assignment_context
                    && capability_expected
                    && self.assignment_rhs_outcome(*else_) == AssignmentRhsOutcome::Never;
                let else_expected = if infer_result || else_pre_never {
                    self.fresh_ty()
                } else {
                    expected
                };
                let mut else_prop = self.check_expr_in_new_scope(
                    *else_,
                    else_expected,
                    result_expectation,
                    result_discarded,
                );
                if else_pre_never {
                    else_prop.ty = expected;
                    self.env.type_expr(*else_, else_prop.clone());
                }
                let then_outcome = self.assignment_rhs_outcome(*then);
                let else_outcome = self.assignment_rhs_outcome(*else_);
                let mut then_never = if assignment_context {
                    then_outcome == AssignmentRhsOutcome::Never
                } else {
                    then_pre_never
                        || (capability_expected && then_outcome == AssignmentRhsOutcome::Never)
                };
                let mut else_never = if assignment_context {
                    else_outcome == AssignmentRhsOutcome::Never
                } else {
                    else_pre_never
                        || (capability_expected && else_outcome == AssignmentRhsOutcome::Never)
                };
                if assignment_context {
                    let reachable = self.assignment_cond_flow(*cond);
                    then_never |= !reachable.on_true;
                    else_never |= !reachable.on_false;
                }
                let result_ty = if infer_result
                    && assignment_context
                    && ((!then_never && then_outcome == AssignmentRhsOutcome::Unresolved)
                        || (!else_never && else_outcome == AssignmentRhsOutcome::Unresolved))
                {
                    expected
                } else if infer_result {
                    self.infer_branch_result_ty(
                        &[
                            (!then_never).then_some((*then, then_prop.clone())),
                            (!else_never).then_some((*else_, else_prop.clone())),
                        ]
                        .into_iter()
                        .flatten()
                        .collect::<Vec<_>>(),
                    )
                } else if then_never || else_never {
                    expected
                } else {
                    else_prop.ty
                };
                let then_prop = self.env.typed_expr(*then).unwrap_or(then_prop);
                let else_prop = self.env.typed_expr(*else_).unwrap_or(else_prop);
                let borrow_provider =
                    result_ty
                        .as_capability(self.db)
                        .and_then(|_| match (then_never, else_never) {
                            (true, false) => else_prop.borrow_provider,
                            (false, true) => then_prop.borrow_provider,
                            (true, true) => None,
                            (false, false) => self.merge_concrete_borrow_providers(
                                then.span(self.body()).into(),
                                then_prop.borrow_provider,
                                else_.span(self.body()).into(),
                                else_prop.borrow_provider,
                            ),
                        });
                ExprProp {
                    ty: result_ty,
                    is_mut: true,
                    binding: None,
                    borrow_provider,
                    path_read_semantics: None,
                    value_access: ValueAccess::Infer,
                }
            }

            None => {
                let if_ty = self.fresh_ty();
                // If there is no else branch, the if expression itself typed as `()`
                self.env.enter_scope(*then);
                self.env.flush_pending_bindings();
                self.check_expr_with_discarded_result(*then, if_ty);
                self.env.leave_scope();
                self.env.clear_pending_bindings();
                self.env.leave_scope();
                ExprProp::new(TyId::unit(self.db), true)
            }
        }
    }

    fn check_match(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::Match(scrutinee, arms) = expr_data else {
            unreachable!()
        };

        let scrutinee_ty = self.fresh_ty();
        let scrutinee_ty = self.check_expr(*scrutinee, scrutinee_ty).ty;
        let (scrutinee_pat_ty, mode) = self.destructure_source_mode(scrutinee_ty);
        let pattern_layout = self.pattern_layout_context(*scrutinee);

        let Partial::Present(arms) = arms else {
            return ExprProp::invalid(self.db);
        };

        let mut match_ty = expected;
        let mut first_provider: Option<(DynLazySpan<'db>, super::ProviderAddressSpace)> = None;
        let mut provider_unknown = false;
        let mut provider_conflict = false;
        let mut arm_statuses = Vec::with_capacity(arms.len());
        let mut arm_props = Vec::with_capacity(arms.len());
        let mut arm_never_statuses = Vec::with_capacity(arms.len());
        let mut arm_statically_unreachable = vec![false; arms.len()];
        let mut capture_access = ClosureCaptureAccess::Read;
        let has_contextual_closure_expectation =
            self.closure_expectation_for_type(expected).is_some();
        let infer_result = !result_discarded
            && !has_contextual_closure_expectation
            && matches!(
                self.normalize_ty(expected).data(self.db),
                TyData::TyVar(var) if var.sort == TyVarSort::General
            );
        let capability_expected =
            !infer_result && self.normalize_ty(expected).as_capability(self.db).is_some();
        let assignment_context = result_expectation.is_capability_assignment();

        for (arm_idx, arm) in arms.iter().enumerate() {
            let pat_result =
                self.check_pat_with_layout(arm.pat, scrutinee_pat_ty, pattern_layout.as_ref());
            if let super::PatternDestructureMode::Borrow(kind) = mode {
                self.retype_pattern_bindings_for_borrow(arm.pat, kind);
            }
            if mode == super::PatternDestructureMode::Owned {
                capture_access.include(self.pattern_value_capture_access(arm.pat));
            }
            arm_statuses.push(pat_result.analysis);

            let statically_unreachable = self
                .assignment_match_arm_reachability(*scrutinee, &arms[..=arm_idx])
                .and_then(|reachable| reachable.last().copied())
                == Some(false);
            arm_statically_unreachable[arm_idx] = statically_unreachable;
            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();
            let arm_pre_never = !assignment_context
                && capability_expected
                && self.assignment_rhs_outcome(arm.body) == AssignmentRhsOutcome::Never;
            let isolate_dead_contextual_result =
                statically_unreachable && has_contextual_closure_expectation;
            let arm_expected = if infer_result || arm_pre_never || isolate_dead_contextual_result {
                self.fresh_ty()
            } else {
                match_ty
            };
            let mut arm_prop = if assignment_context && !result_discarded {
                self.check_expr_with_result_context(arm.body, result_expectation, false)
            } else if result_discarded {
                self.check_expr_with_discarded_result(arm.body, arm_expected)
            } else {
                self.check_expr(arm.body, arm_expected)
            };
            if arm_pre_never {
                arm_prop.ty = expected;
                self.env.type_expr(arm.body, arm_prop.clone());
            } else if !infer_result && !statically_unreachable {
                match_ty = arm_prop.ty;
            }
            self.env.leave_scope();
            let mut arm_never = if assignment_context {
                self.assignment_rhs_outcome(arm.body) == AssignmentRhsOutcome::Never
            } else {
                arm_pre_never
                    || (capability_expected
                        && self.assignment_rhs_outcome(arm.body) == AssignmentRhsOutcome::Never)
            };
            arm_never |= statically_unreachable;
            arm_props.push((arm.body, arm_prop));
            arm_never_statuses.push(arm_never);
        }

        if let Some(reachable) = self.assignment_match_arm_reachability(*scrutinee, arms) {
            for ((arm_never, statically_unreachable), reachable) in arm_never_statuses
                .iter_mut()
                .zip(&mut arm_statically_unreachable)
                .zip(reachable)
            {
                *statically_unreachable = !reachable;
                *arm_never |= !reachable;
            }
            if !infer_result {
                match_ty = expected;
            }
        }

        let has_unresolved_reachable_arm = assignment_context
            && arms.iter().zip(&arm_never_statuses).any(|(arm, never)| {
                !*never && self.assignment_rhs_outcome(arm.body) == AssignmentRhsOutcome::Unresolved
            });
        if infer_result && has_unresolved_reachable_arm {
            match_ty = expected;
        } else if infer_result {
            let reachable_arm_props = arm_props
                .iter()
                .zip(&arm_never_statuses)
                .filter(|(_, never)| !**never)
                .map(|(prop, _)| prop.clone())
                .collect::<Vec<_>>();
            match_ty = self.infer_branch_result_ty(&reachable_arm_props);
        }

        if infer_result || has_contextual_closure_expectation {
            for ((arm_expr, _), statically_unreachable) in
                arm_props.iter().zip(&arm_statically_unreachable)
            {
                if *statically_unreachable {
                    self.resolve_unreachable_arm_inference(*arm_expr, match_ty);
                }
            }
        }

        for ((arm, arm_prop), arm_never) in arms.iter().zip(arm_props).zip(arm_never_statuses) {
            if arm_never {
                continue;
            }
            let arm_prop = self.env.typed_expr(arm.body).unwrap_or(arm_prop.1);
            if arm_prop.ty.as_capability(self.db).is_some() {
                if let Some(provider) = arm_prop.borrow_provider {
                    if let Some((ref span, previous)) = first_provider {
                        provider_conflict |= self
                            .merge_concrete_borrow_providers(
                                span.clone(),
                                Some(previous),
                                arm.body.span(self.body()).into(),
                                Some(provider),
                            )
                            .is_none();
                    } else {
                        first_provider = Some((arm.body.span(self.body()).into(), provider));
                    }
                } else {
                    provider_unknown = true;
                }
            }
        }
        if mode == super::PatternDestructureMode::Owned {
            self.record_pattern_value_use(*scrutinee, capture_access);
        }

        if !scrutinee_pat_ty.has_invalid(self.db)
            && arm_statuses.iter().all(|status| status.is_ready())
        {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            let pattern_store = self
                .env
                .pattern_store()
                .clone()
                .fold_with(self.db, &mut prober);
            let scrutinee_pat_ty = scrutinee_pat_ty.fold_with(self.db, &mut prober);
            let roots: Vec<_> = arm_statuses
                .iter()
                .filter_map(|status| status.ready_root())
                .collect();
            let analysis = crate::analysis::ty::pattern_analysis::analyze_match(
                self.db,
                &pattern_store,
                &roots,
                scrutinee_pat_ty,
            );

            for i in analysis.unreachable_arms {
                let diag = BodyDiag::UnreachablePattern {
                    primary: arms[i].pat.span(self.body()).into(),
                };
                self.push_diag(diag);
            }

            match analysis.exhaustiveness {
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::Exhaustive => {}
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::NonExhaustive(
                    missing_patterns,
                ) => {
                    let diag = BodyDiag::NonExhaustiveMatch {
                        primary: expr.span(self.body()).into(),
                        scrutinee_ty: scrutinee_pat_ty,
                        missing_patterns,
                    };
                    self.push_diag(diag);
                }
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::Inconclusive(
                    reason,
                ) => {
                    let diag = BodyDiag::PatternAnalysisInconclusive {
                        primary: expr.span(self.body()).into(),
                        reason,
                    };
                    self.push_diag(diag);
                }
            }
        }

        ExprProp {
            ty: match_ty,
            is_mut: true,
            binding: None,
            borrow_provider: if provider_unknown || provider_conflict {
                None
            } else {
                first_provider.map(|(_, provider)| provider)
            },
            path_read_semantics: None,
            value_access: ValueAccess::Infer,
        }
    }

    fn resolve_unreachable_arm_inference(&mut self, expr: ExprId, fallback: TyId<'db>) {
        let Some(mut prop) = self.env.typed_expr(expr) else {
            return;
        };
        let actual = self.normalize_ty(prop.ty);
        let fallback = self.normalize_ty(fallback);

        // A proven-dead arm still needs its own unresolved literal/generic
        // variables finalized so the typed body is complete. This is only a
        // best-effort fallback, not a match-result compatibility constraint:
        // concrete mismatches are accepted, and capability carriers stay
        // wholly isolated from the live arm's type and borrow provider.
        if !actual.has_var(self.db)
            || actual.as_capability(self.db).is_some()
            || fallback.as_capability(self.db).is_some()
            || !self.unreachable_fallback_only_resolves_actual(actual, fallback)
        {
            return;
        }

        if self.table.unify(actual, fallback).is_err() {
            return;
        }

        prop.ty = self.table.fold_ty(self.db, actual);
        self.env.type_expr(expr, prop);
    }

    fn unreachable_fallback_only_resolves_actual(
        &self,
        actual: TyId<'db>,
        fallback: TyId<'db>,
    ) -> bool {
        if !fallback.has_var(self.db) {
            return true;
        }

        match (actual.data(self.db), fallback.data(self.db)) {
            (TyData::TyVar(actual), TyData::TyVar(fallback)) => {
                // Equal or narrower fallback sorts can resolve the dead arm;
                // a broader fallback would instead let the dead arm narrow
                // the live result (for example, general -> integral).
                fallback.sort >= actual.sort
            }
            (
                TyData::TyApp(actual_base, actual_arg),
                TyData::TyApp(fallback_base, fallback_arg),
            ) => {
                self.unreachable_fallback_only_resolves_actual(*actual_base, *fallback_base)
                    && self.unreachable_fallback_only_resolves_actual(*actual_arg, *fallback_arg)
            }
            _ => false,
        }
    }

    fn infer_branch_result_ty(&mut self, branches: &[(ExprId, ExprProp<'db>)]) -> TyId<'db> {
        let Some(first_idx) = branches.iter().position(|(expr, branch)| {
            !self.normalize_ty(branch.ty).is_never(self.db)
                && self.assignment_rhs_outcome(*expr) != AssignmentRhsOutcome::Never
        }) else {
            return TyId::never(self.db);
        };
        let first = &branches[first_idx].1;
        let mut joined = self.normalize_ty(first.ty);
        for (expr, branch) in branches.iter().skip(first_idx + 1) {
            let branch_ty = self.normalize_ty(branch.ty);
            if branch_ty.is_never(self.db)
                || self.assignment_rhs_outcome(*expr) == AssignmentRhsOutcome::Never
            {
                continue;
            }
            joined = match (
                joined.as_capability(self.db),
                branch_ty.as_capability(self.db),
            ) {
                (Some((left_kind, left_inner)), Some((right_kind, right_inner)))
                    if self.table.unify(left_inner, right_inner).is_ok() =>
                {
                    let inner = self.table.fold_ty(self.db, left_inner);
                    match if left_kind.rank() <= right_kind.rank() {
                        left_kind
                    } else {
                        right_kind
                    } {
                        CapabilityKind::Mut => TyId::borrow_mut_of(self.db, inner),
                        CapabilityKind::Ref => TyId::borrow_ref_of(self.db, inner),
                        CapabilityKind::View => TyId::view_of(self.db, inner),
                    }
                }
                (Some((_, inner)), None) if self.table.unify(inner, branch_ty).is_ok() => {
                    let inner = self.table.fold_ty(self.db, inner);
                    if self.ty_is_copy(inner) {
                        inner
                    } else {
                        TyId::view_of(self.db, inner)
                    }
                }
                (None, Some((_, inner))) if self.table.unify(joined, inner).is_ok() => {
                    let inner = self.table.fold_ty(self.db, inner);
                    if self.ty_is_copy(inner) {
                        inner
                    } else {
                        TyId::view_of(self.db, inner)
                    }
                }
                (None, None) if self.table.unify(joined, branch_ty).is_ok() => {
                    self.table.fold_ty(self.db, joined)
                }
                _ => joined,
            };
        }

        for (expr, prop) in branches {
            let branch_ty = self.normalize_ty(prop.ty);
            if !branch_ty.is_never(self.db)
                && self.assignment_rhs_outcome(*expr) == AssignmentRhsOutcome::Never
            {
                continue;
            }
            let actual = self
                .try_coerce_capability_for_expr_to_expected(*expr, prop.ty, joined)
                .unwrap_or(prop.ty);
            let resolved = self.unify_ty(Typeable::Expr(*expr, prop.clone()), actual, joined);
            if !resolved.has_invalid(self.db) {
                joined = resolved;
            }
        }
        self.normalize_ty(joined)
    }

    fn check_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Assign(lhs, rhs) = expr_data else {
            unreachable!()
        };

        let typed_lhs = self.check_expr_unknown(*lhs);
        let lhs_capability = self.normalize_ty(typed_lhs.ty).as_capability(self.db);
        let payload_ty = lhs_capability
            .map(|(_, inner)| inner)
            .unwrap_or(typed_lhs.ty);
        let may_rebind_capability =
            lhs_capability.is_some() && self.assignment_rhs_may_construct_capability(*rhs);
        let defer_result_mode =
            may_rebind_capability && self.assignment_rhs_contains_deferred_carrier(*rhs);
        let mut rhs_prop = if may_rebind_capability {
            self.check_expr_with_result_context(
                *rhs,
                ResultExpectation::CapabilityAssignment {
                    slot: typed_lhs.ty,
                    payload: payload_ty,
                },
                false,
            )
        } else {
            self.check_expr(*rhs, payload_ty)
        };
        let rebinds_capability = !defer_result_mode
            && may_rebind_capability
            && self.assignment_rhs_outcome(*rhs) == AssignmentRhsOutcome::Capability;
        self.env
            .set_assignment_rebinds_capability(expr, rebinds_capability);
        let lhs_ty = if rebinds_capability {
            typed_lhs.ty
        } else {
            payload_ty
        };
        // Assignment is an expected-type boundary. In particular, an assigned
        // contract-field view can carry concrete layout roots that must reach
        // aggregate constructors before their runtime layout is selected.
        if !defer_result_mode {
            if let Some(coerced) =
                self.try_coerce_capability_for_expr_to_expected(*rhs, rhs_prop.ty, lhs_ty)
            {
                rhs_prop.ty = coerced;
            }
            rhs_prop.ty =
                self.unify_ty(Typeable::Expr(*rhs, rhs_prop.clone()), rhs_prop.ty, lhs_ty);
            rhs_prop = self.env.typed_expr(*rhs).unwrap_or(rhs_prop);
        }

        let lhs_status = self.check_assign_lhs_with_mode_context(
            *lhs,
            &typed_lhs,
            rebinds_capability,
            Some(expr),
            defer_result_mode,
        );
        self.record_owned_value_use(*rhs, rhs_prop.ty);

        if lhs_status == AssignLhsStatus::Assignable {
            self.merge_assignment_borrow_providers(*lhs, *rhs, &typed_lhs);
        }

        ExprProp::new(TyId::unit(self.db), true)
    }

    /// Returns whether `expr` can construct the top-level capability assigned
    /// to a capability slot.
    ///
    /// This intentionally follows only result-forwarding forms. A borrow
    /// nested inside an ordinary aggregate is part of the payload and must not
    /// make that aggregate lose the destination's expected payload type.
    fn assignment_rhs_may_construct_capability(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };
        match expr_data {
            Expr::Un(_, UnOp::Mut | UnOp::Ref) => true,
            Expr::Un(_, op) => self.assignment_unary_may_lower_to_semantic_rvalue(*op),
            // Call results are rvalues. If their finalized return type is a
            // capability, assignment stores that capability rather than
            // writing through it. The type may be established only after
            // deferred callable or method resolution, so this predicate must
            // remain syntactic and conservative.
            Expr::Call(..) | Expr::MethodCall(..) => true,
            // A capability projected from a temporary aggregate is itself an
            // rvalue carrier. Place projections are classified as
            // write-through after checking, once `expr_place` is available.
            Expr::Field(..) | Expr::Cast(..) => true,
            Expr::Bin(_, _, op) => self.assignment_binary_may_lower_to_semantic_rvalue(*op),
            Expr::Block(stmts) => stmts.last().is_some_and(|stmt| {
                matches!(
                    stmt.data(self.db, self.body()),
                    Partial::Present(Stmt::Expr(tail))
                        if self.assignment_rhs_may_construct_capability(*tail)
                )
            }),
            Expr::With(_, body) => self.assignment_rhs_may_construct_capability(*body),
            Expr::If(_, then_expr, Some(else_expr)) => {
                self.assignment_rhs_may_construct_capability(*then_expr)
                    || self.assignment_rhs_may_construct_capability(*else_expr)
            }
            Expr::Match(_, Partial::Present(arms)) => arms
                .iter()
                .any(|arm| self.assignment_rhs_may_construct_capability(arm.body)),
            _ => false,
        }
    }

    /// Whether the assignment result mode can depend on callable resolution.
    ///
    /// These are the same transparent result carriers accepted by
    /// `assignment_rhs_may_construct_capability`, but explicit borrow leaves
    /// do not need late validation.
    fn assignment_rhs_contains_deferred_carrier(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };
        match expr_data {
            Expr::Call(..) | Expr::MethodCall(..) => true,
            Expr::Un(_, op) => self.assignment_unary_may_lower_to_semantic_rvalue(*op),
            // Field and cast result carriers can be finalized only after
            // deferred base/projection typing, even without explicit call
            // syntax in the expression itself.
            Expr::Field(..) | Expr::Cast(..) => true,
            // Operators may resolve through a deferred semantic call even
            // when their operands contain no explicit call syntax.
            Expr::Bin(_, _, op) => self.assignment_binary_may_lower_to_semantic_rvalue(*op),
            Expr::Block(stmts) => stmts.last().is_some_and(|stmt| {
                matches!(
                    stmt.data(self.db, self.body()),
                    Partial::Present(Stmt::Expr(tail))
                        if self.assignment_rhs_contains_deferred_carrier(*tail)
                )
            }),
            Expr::With(_, body) => self.assignment_rhs_contains_deferred_carrier(*body),
            Expr::If(_, then_expr, Some(else_expr)) => {
                self.assignment_rhs_contains_deferred_carrier(*then_expr)
                    || self.assignment_rhs_contains_deferred_carrier(*else_expr)
            }
            Expr::Match(_, Partial::Present(arms)) => arms
                .iter()
                .any(|arm| self.assignment_rhs_contains_deferred_carrier(arm.body)),
            _ => false,
        }
    }

    fn assignment_unary_may_lower_to_semantic_rvalue(&self, op: UnOp) -> bool {
        matches!(op, UnOp::Minus | UnOp::Not | UnOp::BitNot)
    }

    fn assignment_binary_may_lower_to_semantic_rvalue(&self, op: BinOp) -> bool {
        !matches!(op, BinOp::Logical(_) | BinOp::Arith(ArithBinOp::Range))
    }

    fn assignment_expr_can_complete_normally(&self, expr: ExprId) -> bool {
        self.assignment_expr_flow(expr).normal
    }

    /// A small, type-checker-local HIR completion analysis.
    ///
    /// Semantic CFG reachability is built after type checking and cannot be
    /// queried here without a cycle. Keeping the four escaping outcomes
    /// separate is important for loops: a nested loop consumes its own
    /// `break`/`continue`, while `return` still escapes the enclosing body.
    fn assignment_expr_flow(&self, expr: ExprId) -> AssignmentFlow {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return AssignmentFlow::NORMAL;
        };

        let flow = match expr_data {
            Expr::Lit(_) | Expr::Path(_) | Expr::Closure { .. } => AssignmentFlow::NORMAL,
            Expr::Block(stmts) => self.assignment_block_flow(stmts),
            Expr::Un(inner, _) | Expr::Cast(inner, _) | Expr::Field(inner, _) => {
                self.assignment_expr_flow(*inner)
            }
            Expr::Bin(_, _, BinOp::Logical(_)) => self.assignment_expr_bool_flow(expr).as_flow(),
            Expr::Bin(lhs, rhs, _) => self
                .assignment_expr_flow(*lhs)
                .then(self.assignment_expr_flow(*rhs)),
            Expr::Call(callee, args) => {
                let mut flow = self.assignment_expr_flow(*callee);
                for arg in args {
                    flow = flow.then(self.assignment_expr_flow(arg.expr));
                }
                if self.assignment_selected_call_returns_never(expr) {
                    flow.without_normal()
                } else {
                    flow
                }
            }
            Expr::Assert(args) => {
                let Some(condition) = args.first() else {
                    return AssignmentFlow::NORMAL;
                };
                let condition = self.assignment_expr_bool_flow(condition.expr);
                let trailing =
                    self.assignment_sequence_expr_flows(args.iter().skip(1).map(|arg| arg.expr));
                AssignmentFlow {
                    // `assert!` completes only along the true condition path.
                    normal: condition.on_true && trailing.normal,
                    breaks: condition.breaks || condition.on_true && trailing.breaks,
                    continues: condition.continues || condition.on_true && trailing.continues,
                    returns: condition.returns || condition.on_true && trailing.returns,
                }
            }
            Expr::MethodCall(receiver, _, _, args) => {
                let mut flow = self.assignment_expr_flow(*receiver);
                for arg in args {
                    flow = flow.then(self.assignment_expr_flow(arg.expr));
                }
                if self.assignment_selected_call_returns_never(expr) {
                    flow.without_normal()
                } else {
                    flow
                }
            }
            Expr::RecordInit(_, fields) => {
                self.assignment_sequence_expr_flows(fields.iter().map(|field| field.expr))
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                self.assignment_sequence_expr_flows(items.iter().copied())
            }
            Expr::ArrayRep(value, _) => self.assignment_expr_flow(*value),
            Expr::If(cond, then_expr, else_expr) => {
                let cond = self.assignment_cond_flow(*cond);
                let then_flow = self.assignment_expr_flow(*then_expr);
                let else_flow = else_expr
                    .map(|else_expr| self.assignment_expr_flow(else_expr))
                    .unwrap_or(AssignmentFlow::NORMAL);
                AssignmentFlow {
                    normal: cond.on_true && then_flow.normal || cond.on_false && else_flow.normal,
                    breaks: cond.breaks
                        || cond.on_true && then_flow.breaks
                        || cond.on_false && else_flow.breaks,
                    continues: cond.continues
                        || cond.on_true && then_flow.continues
                        || cond.on_false && else_flow.continues,
                    returns: cond.returns
                        || cond.on_true && then_flow.returns
                        || cond.on_false && else_flow.returns,
                }
            }
            Expr::Match(scrutinee_expr, arms) => {
                let scrutinee_flow = self.assignment_expr_flow(*scrutinee_expr);
                let arm_flow = match arms {
                    Partial::Present(arms) if !arms.is_empty() => {
                        let reachable =
                            self.assignment_match_arm_reachability(*scrutinee_expr, arms);
                        arms.iter()
                            .enumerate()
                            .filter(|(idx, _)| {
                                reachable.as_ref().is_none_or(|reachable| reachable[*idx])
                            })
                            .map(|(_, arm)| self.assignment_expr_flow(arm.body))
                            .reduce(AssignmentFlow::or)
                            .unwrap_or(AssignmentFlow::NORMAL)
                    }
                    _ => AssignmentFlow::NORMAL,
                };
                scrutinee_flow.then(arm_flow)
            }
            Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => self
                .assignment_expr_flow(*lhs)
                .then(self.assignment_expr_flow(*rhs)),
            Expr::With(bindings, body) => {
                let mut flow = self
                    .assignment_sequence_expr_flows(bindings.iter().map(|binding| binding.value));
                flow = flow.then(self.assignment_expr_flow(*body));
                flow
            }
        };

        if self
            .env
            .typed_expr(expr)
            .is_some_and(|prop| self.assignment_normalize_ty(prop.ty).is_never(self.db))
        {
            flow.without_normal()
        } else {
            flow
        }
    }

    fn assignment_sequence_expr_flows(
        &self,
        exprs: impl IntoIterator<Item = ExprId>,
    ) -> AssignmentFlow {
        exprs
            .into_iter()
            .fold(AssignmentFlow::NORMAL, |flow, expr| {
                flow.then(self.assignment_expr_flow(expr))
            })
    }

    fn assignment_block_flow(&self, stmts: &[StmtId]) -> AssignmentFlow {
        stmts.iter().fold(AssignmentFlow::NORMAL, |flow, stmt| {
            flow.then(self.assignment_stmt_flow(*stmt))
        })
    }

    fn assignment_stmt_flow(&self, stmt: StmtId) -> AssignmentFlow {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body()) else {
            return AssignmentFlow::NORMAL;
        };
        match stmt_data {
            Stmt::Let(_, _, Some(value)) | Stmt::Expr(value) => self.assignment_expr_flow(*value),
            Stmt::Let(_, _, None) => AssignmentFlow::NORMAL,
            Stmt::For(_, iterable, body, _) => {
                let iterable = self.assignment_expr_flow(*iterable);
                let body = self.assignment_expr_flow(*body);
                iterable.then(AssignmentFlow {
                    // A `for` loop may execute zero times. Break and continue
                    // from its body are consumed by this loop.
                    normal: true,
                    breaks: false,
                    continues: false,
                    returns: body.returns,
                })
            }
            Stmt::While(cond, body) => {
                let cond = self.assignment_cond_flow(*cond);
                let body = self.assignment_expr_flow(*body);
                AssignmentFlow {
                    // A known-true loop completes only through a reachable
                    // break in this loop's body. Nested-loop breaks have
                    // already been consumed by their own statement flow.
                    normal: cond.on_false || cond.on_true && body.breaks,
                    breaks: cond.breaks,
                    continues: cond.continues,
                    returns: cond.returns || cond.on_true && body.returns,
                }
            }
            Stmt::Continue => AssignmentFlow {
                normal: false,
                breaks: false,
                continues: true,
                returns: false,
            },
            Stmt::Break => AssignmentFlow {
                normal: false,
                breaks: true,
                continues: false,
                returns: false,
            },
            Stmt::Return(value) => {
                let value = value
                    .map(|value| self.assignment_expr_flow(value))
                    .unwrap_or(AssignmentFlow::NORMAL);
                AssignmentFlow {
                    normal: false,
                    breaks: value.breaks,
                    continues: value.continues,
                    returns: value.returns || value.normal,
                }
            }
        }
    }

    fn assignment_expr_bool_flow(&self, expr: ExprId) -> AssignmentBoolFlow {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return AssignmentBoolFlow::from_flow(AssignmentFlow::NORMAL);
        };
        if matches!(expr_data, Expr::Path(_))
            && let Some(value) = self
                .assignment_known_pattern_scrutinee(expr)
                .as_ref()
                .and_then(KnownPatternScrutinee::known_bool)
        {
            let flow = self.assignment_expr_flow(expr);
            return AssignmentBoolFlow {
                on_true: flow.normal && value,
                on_false: flow.normal && !value,
                breaks: flow.breaks,
                continues: flow.continues,
                returns: flow.returns,
            };
        }
        match expr_data {
            Expr::Lit(LitKind::Bool(value)) => AssignmentBoolFlow {
                on_true: *value,
                on_false: !*value,
                breaks: false,
                continues: false,
                returns: false,
            },
            Expr::Un(inner, UnOp::Not) => {
                let inner = self.assignment_expr_bool_flow(*inner);
                AssignmentBoolFlow {
                    on_true: inner.on_false,
                    on_false: inner.on_true,
                    breaks: inner.breaks,
                    continues: inner.continues,
                    returns: inner.returns,
                }
            }
            Expr::Bin(lhs, rhs, BinOp::Logical(op)) => {
                let lhs = self.assignment_expr_bool_flow(*lhs);
                let rhs = self.assignment_expr_bool_flow(*rhs);
                self.assignment_join_logical_bool_flows(lhs, rhs, *op)
            }
            Expr::Block(stmts) => self.assignment_block_bool_flow(stmts),
            Expr::If(cond, then_expr, Some(else_expr)) => {
                let cond = self.assignment_cond_flow(*cond);
                let then_flow = self.assignment_expr_bool_flow(*then_expr);
                let else_flow = self.assignment_expr_bool_flow(*else_expr);
                AssignmentBoolFlow {
                    on_true: cond.on_true && then_flow.on_true
                        || cond.on_false && else_flow.on_true,
                    on_false: cond.on_true && then_flow.on_false
                        || cond.on_false && else_flow.on_false,
                    breaks: cond.breaks
                        || cond.on_true && then_flow.breaks
                        || cond.on_false && else_flow.breaks,
                    continues: cond.continues
                        || cond.on_true && then_flow.continues
                        || cond.on_false && else_flow.continues,
                    returns: cond.returns
                        || cond.on_true && then_flow.returns
                        || cond.on_false && else_flow.returns,
                }
            }
            Expr::Match(scrutinee, Partial::Present(arms)) if !arms.is_empty() => {
                let prefix = self.assignment_expr_flow(*scrutinee);
                let reachable = self.assignment_match_arm_reachability(*scrutinee, arms);
                let value = arms
                    .iter()
                    .enumerate()
                    .filter(|(idx, _)| reachable.as_ref().is_none_or(|reachable| reachable[*idx]))
                    .map(|(_, arm)| self.assignment_expr_bool_flow(arm.body))
                    .reduce(AssignmentBoolFlow::or)
                    .unwrap_or_else(|| AssignmentBoolFlow::from_flow(AssignmentFlow::NORMAL));
                self.assignment_prefix_bool_flow(prefix, value)
            }
            Expr::With(bindings, body) => {
                let prefix = self
                    .assignment_sequence_expr_flows(bindings.iter().map(|binding| binding.value));
                self.assignment_prefix_bool_flow(prefix, self.assignment_expr_bool_flow(*body))
            }
            _ => AssignmentBoolFlow::from_flow(self.assignment_expr_flow(expr)),
        }
    }

    fn assignment_block_bool_flow(&self, stmts: &[StmtId]) -> AssignmentBoolFlow {
        let Some((&last, prefix)) = stmts.split_last() else {
            return AssignmentBoolFlow::from_flow(AssignmentFlow::NORMAL);
        };
        let prefix = prefix.iter().fold(AssignmentFlow::NORMAL, |flow, stmt| {
            flow.then(self.assignment_stmt_flow(*stmt))
        });
        match last.data(self.db, self.body()) {
            Partial::Present(Stmt::Expr(tail)) => {
                self.assignment_prefix_bool_flow(prefix, self.assignment_expr_bool_flow(*tail))
            }
            _ => AssignmentBoolFlow::from_flow(prefix.then(self.assignment_stmt_flow(last))),
        }
    }

    fn assignment_prefix_bool_flow(
        &self,
        prefix: AssignmentFlow,
        value: AssignmentBoolFlow,
    ) -> AssignmentBoolFlow {
        AssignmentBoolFlow {
            on_true: prefix.normal && value.on_true,
            on_false: prefix.normal && value.on_false,
            breaks: prefix.breaks || prefix.normal && value.breaks,
            continues: prefix.continues || prefix.normal && value.continues,
            returns: prefix.returns || prefix.normal && value.returns,
        }
    }

    fn assignment_cond_flow(&self, cond: CondId) -> AssignmentBoolFlow {
        let Partial::Present(cond_data) = cond.data(self.db, self.body()) else {
            return AssignmentBoolFlow::from_flow(AssignmentFlow::NORMAL);
        };
        match cond_data {
            Cond::Expr(expr) => self.assignment_expr_bool_flow(*expr),
            Cond::Let(pat, expr) => {
                let flow = self.assignment_expr_flow(*expr);
                let known_scrutinee = self.assignment_known_pattern_scrutinee(*expr);
                let reachable = single_pattern_branch_reachability(
                    self.db,
                    self.env.pattern_store(),
                    *pat,
                    known_scrutinee.as_ref(),
                )
                .unwrap_or(PatternBranchReachability::BOTH);
                AssignmentBoolFlow {
                    on_true: flow.normal && reachable.can_match,
                    on_false: flow.normal && reachable.can_miss,
                    breaks: flow.breaks,
                    continues: flow.continues,
                    returns: flow.returns,
                }
            }
            Cond::Bin(lhs, rhs, op) => {
                let lhs = self.assignment_cond_flow(*lhs);
                let rhs = self.assignment_cond_flow(*rhs);
                self.assignment_join_logical_bool_flows(lhs, rhs, *op)
            }
        }
    }

    fn assignment_join_logical_bool_flows(
        &self,
        lhs: AssignmentBoolFlow,
        rhs: AssignmentBoolFlow,
        op: LogicalBinOp,
    ) -> AssignmentBoolFlow {
        match op {
            LogicalBinOp::And => AssignmentBoolFlow {
                on_true: lhs.on_true && rhs.on_true,
                on_false: lhs.on_false || lhs.on_true && rhs.on_false,
                breaks: lhs.breaks || lhs.on_true && rhs.breaks,
                continues: lhs.continues || lhs.on_true && rhs.continues,
                returns: lhs.returns || lhs.on_true && rhs.returns,
            },
            LogicalBinOp::Or => AssignmentBoolFlow {
                on_true: lhs.on_true || lhs.on_false && rhs.on_true,
                on_false: lhs.on_false && rhs.on_false,
                breaks: lhs.breaks || lhs.on_false && rhs.breaks,
                continues: lhs.continues || lhs.on_false && rhs.continues,
                returns: lhs.returns || lhs.on_false && rhs.returns,
            },
        }
    }

    fn assignment_selected_call_returns_never(&self, call_expr: ExprId) -> bool {
        self.env.callable_expr(call_expr).is_some_and(|callable| {
            self.assignment_normalize_ty(callable.ret_ty(self.db))
                .is_never(self.db)
        })
    }

    fn assignment_known_pattern_scrutinee(
        &self,
        expr: ExprId,
    ) -> Option<KnownPatternScrutinee<'db>> {
        if let Some(const_ref) = self.env.expr_const_ref(expr)
            && let Some(prop) = self.env.typed_expr(expr)
            && let Some(const_ref) = resolve_semantic_const_ref(
                self.db,
                const_ref,
                self.assignment_normalize_ty(prop.ty),
                SemOrigin::Expr(expr),
            )
            && let Ok(value) = eval_const_ref(self.db, const_ref)
        {
            return Some(known_pattern_scrutinee_from_const(self.db, value));
        }
        if let Some(prop) = self.env.typed_expr(expr) {
            let ty = self.assignment_normalize_ty(prop.ty);
            if ty.is_integral(self.db)
                && let Some(value) = self.try_eval_static_int(expr, ty)
            {
                return Some(KnownPatternScrutinee::Literal(LitKind::Int(value)));
            }
        }

        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return None;
        };
        match expr_data {
            Expr::Lit(lit) => Some(KnownPatternScrutinee::Literal(*lit)),
            Expr::Path(_) => match self.env.value_path_ref(expr) {
                Some(ValuePathRef::UnitVariant(variant)) => Some(KnownPatternScrutinee::variant(
                    variant.variant,
                    std::iter::empty(),
                )),
                Some(ValuePathRef::TypeConst(_) | ValuePathRef::FunctionItem) | None => None,
            },
            Expr::Tuple(fields) | Expr::Array(fields) => {
                let ty = self
                    .env
                    .typed_expr(expr)
                    .map(|prop| self.assignment_normalize_ty(prop.ty))?;
                Some(KnownPatternScrutinee::type_constructor(
                    ty,
                    fields.iter().map(|field| {
                        self.assignment_known_pattern_scrutinee(*field)
                            .unwrap_or(KnownPatternScrutinee::Unknown)
                    }),
                ))
            }
            Expr::RecordInit(_, fields) => {
                let (record_like, constructor) = match self.env.record_init_lowering(expr)? {
                    RecordInitLowering::Struct => {
                        let ty = self
                            .env
                            .typed_expr(expr)
                            .map(|prop| self.assignment_normalize_ty(prop.ty))?;
                        (
                            RecordLike::Type(ty),
                            KnownPatternScrutinee::type_constructor(ty, std::iter::empty()),
                        )
                    }
                    RecordInitLowering::EnumVariant(variant) => (
                        RecordLike::from_variant(variant),
                        KnownPatternScrutinee::variant(variant.variant, std::iter::empty()),
                    ),
                };
                let mut known_fields =
                    vec![KnownPatternScrutinee::Unknown; record_like.record_labels(self.db).len()];
                for field in fields {
                    let Some(label) = field.label_eagerly(self.db, self.body()) else {
                        continue;
                    };
                    let Some(field_idx) = record_like.record_field_idx(self.db, label) else {
                        continue;
                    };
                    let Some(slot) = known_fields.get_mut(field_idx) else {
                        continue;
                    };
                    *slot = self
                        .assignment_known_pattern_scrutinee(field.expr)
                        .unwrap_or(KnownPatternScrutinee::Unknown);
                }
                Some(match constructor {
                    KnownPatternScrutinee::Variant { variant, .. } => {
                        KnownPatternScrutinee::variant(variant, known_fields)
                    }
                    KnownPatternScrutinee::Type { ty, .. } => {
                        KnownPatternScrutinee::type_constructor(ty, known_fields)
                    }
                    KnownPatternScrutinee::Unknown | KnownPatternScrutinee::Literal(_) => {
                        unreachable!("record constructors have a structural shape")
                    }
                })
            }
            Expr::Call(_, args) => {
                let callable = self.env.callable_expr(expr)?;
                match callable.callable_def() {
                    CallableDef::VariantCtor(variant) => Some(KnownPatternScrutinee::variant(
                        variant,
                        args.iter().map(|arg| {
                            self.assignment_known_pattern_scrutinee(arg.expr)
                                .unwrap_or(KnownPatternScrutinee::Unknown)
                        }),
                    )),
                    CallableDef::Func(_) => None,
                }
            }
            Expr::Block(stmts) => {
                let tail = stmts.last()?;
                match tail.data(self.db, self.body()) {
                    Partial::Present(Stmt::Expr(tail)) => {
                        self.assignment_known_pattern_scrutinee(*tail)
                    }
                    _ => None,
                }
            }
            Expr::With(_, body) => self.assignment_known_pattern_scrutinee(*body),
            Expr::If(cond, then_expr, Some(else_expr)) => {
                let cond = self.assignment_cond_flow(*cond);
                self.assignment_merge_known_pattern_scrutinees(
                    [
                        cond.on_true.then_some(*then_expr),
                        cond.on_false.then_some(*else_expr),
                    ]
                    .into_iter()
                    .flatten(),
                )
            }
            Expr::Match(scrutinee, Partial::Present(arms)) => {
                if !self.assignment_expr_flow(*scrutinee).normal {
                    return None;
                }
                let reachable = self.assignment_match_arm_reachability(*scrutinee, arms);
                self.assignment_merge_known_pattern_scrutinees(
                    arms.iter()
                        .enumerate()
                        .filter(|(idx, _)| {
                            reachable.as_ref().is_none_or(|reachable| reachable[*idx])
                        })
                        .map(|(_, arm)| arm.body),
                )
            }
            Expr::Cast(inner, _)
                if self.env.typed_expr(expr).is_some_and(|prop| {
                    self.assignment_normalize_ty(prop.ty).is_integral(self.db)
                }) =>
            {
                match self.assignment_known_pattern_scrutinee(*inner)? {
                    KnownPatternScrutinee::Literal(LitKind::Int(value)) => {
                        Some(KnownPatternScrutinee::Literal(LitKind::Int(value)))
                    }
                    KnownPatternScrutinee::Literal(LitKind::Bool(value)) => {
                        Some(KnownPatternScrutinee::Literal(LitKind::Int(
                            IntegerId::new(self.db, BigUint::from(u8::from(value))),
                        )))
                    }
                    KnownPatternScrutinee::Unknown
                    | KnownPatternScrutinee::Variant { .. }
                    | KnownPatternScrutinee::Type { .. }
                    | KnownPatternScrutinee::Literal(_) => None,
                }
            }
            Expr::Closure { .. }
            | Expr::Bin(..)
            | Expr::Un(..)
            | Expr::Cast(..)
            | Expr::Assert(..)
            | Expr::MethodCall(..)
            | Expr::Field(..)
            | Expr::ArrayRep(..)
            | Expr::If(..)
            | Expr::Match(_, Partial::Absent)
            | Expr::Assign(..)
            | Expr::AugAssign(..) => None,
        }
    }

    fn assignment_merge_known_pattern_scrutinees(
        &self,
        exprs: impl IntoIterator<Item = ExprId>,
    ) -> Option<KnownPatternScrutinee<'db>> {
        let mut merged = None;
        for expr in exprs {
            if !self.assignment_expr_flow(expr).normal {
                continue;
            }
            let value = self
                .assignment_known_pattern_scrutinee(expr)
                .unwrap_or(KnownPatternScrutinee::Unknown);
            match &merged {
                Some(previous) if previous != &value => return None,
                Some(_) => {}
                None => merged = Some(value),
            }
        }
        merged
    }

    fn assignment_match_arm_reachability(
        &self,
        scrutinee: ExprId,
        arms: &[crate::hir_def::MatchArm],
    ) -> Option<Vec<bool>> {
        let scrutinee = self.assignment_known_pattern_scrutinee(scrutinee)?;
        known_scrutinee_arm_reachability(
            self.db,
            self.env.pattern_store(),
            arms.iter().map(|arm| arm.pat),
            &scrutinee,
        )
    }

    /// An explicit borrow expression on the right hand side assigns the
    /// capability itself when the destination stores a capability. Ordinary
    /// capability-valued paths keep the existing write-through behavior:
    /// `holder.target = replacement` copies the replacement's value, while
    /// `holder.target = mut replacement` rebinds the stored handle.
    ///
    /// Value-forwarding blocks are transparent so braces do not change that
    /// distinction. A control-flow expression constructs a capability when
    /// every branch that can produce a value does. Branches proven to have the
    /// never type do not participate in the result join.
    fn assignment_rhs_outcome(&self, expr: ExprId) -> AssignmentRhsOutcome {
        if !self.assignment_expr_can_complete_normally(expr) {
            return AssignmentRhsOutcome::Never;
        }

        // Calls, methods, and overloaded operators all converge on semantic
        // expression lowering. Classify that common rvalue representation
        // instead of maintaining a syntax-specific list of call-like forms.
        if self.env.semantic_expr_lowering(expr).is_some() {
            return self.assignment_typed_rvalue_outcome(expr);
        }

        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return AssignmentRhsOutcome::Payload;
        };
        match expr_data {
            Expr::Un(_, UnOp::Mut | UnOp::Ref) => AssignmentRhsOutcome::Capability,
            Expr::Call(..) | Expr::MethodCall(..) | Expr::Field(..) | Expr::Cast(..) => {
                self.assignment_typed_rvalue_outcome(expr)
            }
            Expr::Un(_, op) if self.assignment_unary_may_lower_to_semantic_rvalue(*op) => {
                self.assignment_typed_rvalue_outcome(expr)
            }
            Expr::Bin(_, _, op) if self.assignment_binary_may_lower_to_semantic_rvalue(*op) => {
                self.assignment_typed_rvalue_outcome(expr)
            }
            Expr::Block(stmts) => self.assignment_block_result_outcome(stmts),
            Expr::With(_, body) => self.assignment_rhs_outcome(*body),
            Expr::If(cond, then_expr, Some(else_expr)) => {
                let cond = self.assignment_cond_flow(*cond);
                let mut outcomes = Vec::with_capacity(2);
                if cond.on_true {
                    outcomes.push(self.assignment_rhs_outcome(*then_expr));
                }
                if cond.on_false {
                    outcomes.push(self.assignment_rhs_outcome(*else_expr));
                }
                self.join_assignment_rhs_outcomes(outcomes)
            }
            // An `if` without `else` is unit-valued, even when its condition
            // is a constant.
            Expr::If(_, _, None) => AssignmentRhsOutcome::Payload,
            Expr::Match(scrutinee, Partial::Present(arms)) => {
                let reachable = self.assignment_match_arm_reachability(*scrutinee, arms);
                self.join_assignment_rhs_outcomes(
                    arms.iter()
                        .enumerate()
                        .filter(|(idx, _)| {
                            reachable.as_ref().is_none_or(|reachable| reachable[*idx])
                        })
                        .map(|(_, arm)| self.assignment_rhs_outcome(arm.body)),
                )
            }
            _ => AssignmentRhsOutcome::Payload,
        }
    }

    fn assignment_typed_rvalue_outcome(&self, expr: ExprId) -> AssignmentRhsOutcome {
        let Some(prop) = self.env.typed_expr(expr) else {
            return AssignmentRhsOutcome::Unresolved;
        };
        let ty = self.assignment_normalize_ty(prop.ty);
        if ty.has_var(self.db) {
            AssignmentRhsOutcome::Unresolved
        } else if ty.as_capability(self.db).is_some() && self.env.expr_place(expr).is_none() {
            AssignmentRhsOutcome::Capability
        } else {
            AssignmentRhsOutcome::Payload
        }
    }

    fn assignment_block_result_outcome(&self, stmts: &[StmtId]) -> AssignmentRhsOutcome {
        let Some(&last) = stmts.last() else {
            return AssignmentRhsOutcome::Payload;
        };
        match last.data(self.db, self.body()) {
            Partial::Present(Stmt::Expr(tail)) => self.assignment_rhs_outcome(*tail),
            _ => AssignmentRhsOutcome::Payload,
        }
    }

    fn join_assignment_rhs_outcomes(
        &self,
        outcomes: impl IntoIterator<Item = AssignmentRhsOutcome>,
    ) -> AssignmentRhsOutcome {
        let mut saw_capability = false;
        let mut saw_unresolved = false;
        for outcome in outcomes {
            match outcome {
                AssignmentRhsOutcome::Capability => saw_capability = true,
                AssignmentRhsOutcome::Payload => return AssignmentRhsOutcome::Payload,
                AssignmentRhsOutcome::Unresolved => saw_unresolved = true,
                AssignmentRhsOutcome::Never => {}
            }
        }
        if saw_unresolved {
            AssignmentRhsOutcome::Unresolved
        } else if saw_capability {
            AssignmentRhsOutcome::Capability
        } else {
            AssignmentRhsOutcome::Never
        }
    }

    fn assignment_normalize_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(self.db, ty, self.env.scope(), self.env.assumptions())
    }

    /// Reconciles assignments whose payload-vs-capability mode depended on a
    /// call result after deferred callable and closure resolution has run.
    pub(super) fn finalize_deferred_assignment_modes(&mut self) {
        let assignments = self
            .body()
            .exprs(self.db)
            .iter()
            .filter_map(|(expr, data)| match data {
                Partial::Present(Expr::Assign(lhs, rhs))
                    if self.assignment_rhs_contains_deferred_carrier(*rhs) =>
                {
                    Some((expr, *lhs, *rhs))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        for (assignment, lhs, rhs) in assignments {
            let Some(typed_lhs) = self.env.typed_expr(lhs) else {
                continue;
            };
            let Some(mut rhs_prop) = self.env.typed_expr(rhs) else {
                continue;
            };
            let Some((_, payload_ty)) = self
                .assignment_normalize_ty(typed_lhs.ty)
                .as_capability(self.db)
            else {
                continue;
            };
            let rebinds_capability = self.assignment_rhs_may_construct_capability(rhs)
                && self.assignment_rhs_outcome(rhs) == AssignmentRhsOutcome::Capability;
            self.env
                .set_assignment_rebinds_capability(assignment, rebinds_capability);
            let expected = if rebinds_capability {
                typed_lhs.ty
            } else {
                payload_ty
            };
            if let Some(coerced) =
                self.try_coerce_capability_for_expr_to_expected(rhs, rhs_prop.ty, expected)
            {
                rhs_prop.ty = coerced;
            }
            self.unify_ty(Typeable::Expr(rhs, rhs_prop.clone()), rhs_prop.ty, expected);
        }
    }

    pub(super) fn refresh_assignment_flow_metadata(&mut self) {
        let exprs = self
            .body()
            .exprs(self.db)
            .iter()
            .map(|(expr, _)| expr)
            .collect::<Vec<_>>();
        let conds = self
            .body()
            .conds(self.db)
            .iter()
            .map(|(cond, _)| cond)
            .collect::<Vec<_>>();
        for expr in &exprs {
            let normal = self.assignment_expr_flow(*expr).normal;
            self.env.set_expr_normal_completion(*expr, normal);
            let normal_bool_value = self.assignment_expr_bool_flow(*expr).normal_value();
            self.env
                .set_expr_normal_bool_value(*expr, normal_bool_value);
        }
        for cond in conds {
            let normal_bool_value = self.assignment_cond_flow(cond).normal_value();
            self.env.set_cond_normal_bool_value(cond, normal_bool_value);
        }
        for expr in exprs {
            let Partial::Present(Expr::Assign(lhs, rhs)) = expr.data(self.db, self.body()) else {
                continue;
            };
            let lhs_is_capability = self.env.typed_expr(*lhs).is_some_and(|prop| {
                self.assignment_normalize_ty(prop.ty)
                    .as_capability(self.db)
                    .is_some()
            });
            let rebinds = lhs_is_capability
                && self.assignment_rhs_may_construct_capability(*rhs)
                && self.assignment_rhs_outcome(*rhs) == AssignmentRhsOutcome::Capability;
            self.env.set_assignment_rebinds_capability(expr, rebinds);
        }
    }

    fn check_aug_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::AugAssign(lhs, rhs, op) = expr_data else {
            unreachable!()
        };

        let unit = ExprProp::new(TyId::unit(self.db), true);

        let typed_lhs = self.check_expr_unknown(*lhs);
        let lhs_ty = typed_lhs.ty;
        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        if lhs_ty.has_invalid(self.db) {
            return unit;
        }
        if self.check_assign_lhs(*lhs, &typed_lhs) == AssignLhsStatus::NonAssignable {
            return unit;
        }

        // Avoid 'type must be known' diagnostics for unknown integer ty
        if lhs_place_ty.is_integral_var(self.db) {
            self.check_expr(*rhs, lhs_place_ty);
            return unit;
        }

        let lhs_base_ty = lhs_place_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            self.check_expr_unknown(*rhs);
            self.env
                .register_pending_primitive_op(PendingPrimitiveOp::AugAssign {
                    expr,
                    lhs: *lhs,
                    rhs: *rhs,
                    op: *op,
                });
            return unit;
        }

        // `x += y` is semantically defined by the `*Assign` traits. Primitive
        // integer fast paths are introduced later during MIR lowering without
        // changing which trait method the source program resolves to here.
        self.check_ops_trait(expr, lhs_place_ty, &AugAssignOp(*op), Some(*rhs), None);

        // Return unit ty even if trait resolution fails
        unit
    }

    /// Resolve a core::ops trait method for an operator on a given LHS type and
    /// optionally check the RHS against the inferred method parameter type.
    /// Returns the fully-instantiated function type and concrete trait instance.
    fn check_ops_trait(
        &mut self,
        expr: ExprId,
        lhs_ty: TyId<'db>,
        op: &dyn TraitOps,
        rhs_expr: Option<ExprId>,
        contextual_expected: Option<TyId<'db>>,
    ) -> ExprProp<'db> {
        let Some(trait_def) =
            resolve_core_trait(self.db, self.env.scope(), &op.trait_path_segments())
        else {
            return ExprProp::invalid(self.db);
        };

        let lhs_candidates = self.capability_fallback_candidates(lhs_ty);
        let method_assumptions = self.env.assumptions();

        let mut selected_lhs_ty = lhs_candidates[0];
        let mut c_lhs_ty = Canonicalized::new(self.db, selected_lhs_ty);
        let mut method_candidate = select_method_candidate(
            self.db,
            &c_lhs_ty,
            op.trait_method(self.db),
            self.env.scope(),
            method_assumptions,
            Some(trait_def),
        );
        if matches!(
            method_candidate,
            Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
        ) {
            for &candidate_ty in lhs_candidates.iter().skip(1) {
                let c_candidate_ty = Canonicalized::new(self.db, candidate_ty);
                let fallback = select_method_candidate(
                    self.db,
                    &c_candidate_ty,
                    op.trait_method(self.db),
                    self.env.scope(),
                    method_assumptions,
                    Some(trait_def),
                );
                if fallback.is_ok() || !matches!(fallback, Err(MethodSelectionError::NotFound)) {
                    selected_lhs_ty = candidate_ty;
                    c_lhs_ty = c_candidate_ty;
                    method_candidate = fallback;
                    break;
                }
            }
        }

        let mut needs_confirmation = false;
        let (method, inst) = match method_candidate {
            Ok(MethodCandidate::InherentMethod(_)) => unreachable!(),
            Ok(
                res @ (MethodCandidate::TraitMethod(cand)
                | MethodCandidate::NeedsConfirmation(cand)),
            ) => {
                let inst = c_lhs_ty.extract_solution(&mut self.table, cand.inst);
                if matches!(res, MethodCandidate::NeedsConfirmation(_)) {
                    needs_confirmation = true;
                }

                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_lhs_ty, inst);

                if let Some(rhs_expr) = rhs_expr
                    && let Some(expected_rhs) = self.instantiated_ops_rhs_ty(func_ty, inst)
                {
                    self.check_or_constrain_expr_to_expected(rhs_expr, expected_rhs);
                }

                (func_ty, inst)
            }
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                let Some(rhs_expr) = rhs_expr else {
                    unreachable!("unary core::ops ambiguity");
                };

                let rhs = self.check_expr_unknown(rhs_expr);
                if rhs.ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                let mut viable = Vec::new();
                for candidate in ambiguous.candidates.iter().copied() {
                    let snapshot = self.snapshot_state();
                    let unifies = (|| {
                        let inst = c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst);
                        let func_ty = super::try_instantiate_trait_method(
                            self.db,
                            candidate.cand.method,
                            &mut self.table,
                            selected_lhs_ty,
                            inst,
                        )
                        .ok()?;
                        let func_ty = self.table.instantiate_to_term(func_ty);
                        let expected_rhs = self.instantiated_ops_rhs_ty(func_ty, inst)?;
                        let rhs_ty = self
                            .try_coerce_capability_for_expr_to_expected(
                                rhs_expr,
                                rhs.ty,
                                expected_rhs,
                            )
                            .unwrap_or(rhs.ty);
                        self.table.unify(rhs_ty, expected_rhs).ok()
                    })()
                    .is_some();
                    self.rollback_state(snapshot);
                    if unifies {
                        viable.push(candidate);
                    }
                }

                match viable.len() {
                    0 => {
                        let diag = BodyDiag::ops_trait_not_implemented(
                            self.db,
                            expr.span(self.body()).into(),
                            lhs_ty,
                            op,
                        );
                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                    1 => {
                        let candidate = viable.pop().unwrap();
                        let inst = c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst);
                        if candidate.needs_confirmation {
                            needs_confirmation = true;
                        }
                        let func_ty = self.instantiate_trait_method_to_term(
                            candidate.cand.method,
                            selected_lhs_ty,
                            inst,
                        );
                        if let Some(expected_rhs) = self.instantiated_ops_rhs_ty(func_ty, inst) {
                            let rhs_ty = self
                                .try_coerce_capability_for_expr_to_expected(
                                    rhs_expr,
                                    rhs.ty,
                                    expected_rhs,
                                )
                                .unwrap_or(rhs.ty);
                            self.unify_ty(
                                Typeable::Expr(rhs_expr, rhs.clone()),
                                rhs_ty,
                                expected_rhs,
                            );
                        }
                        (func_ty, inst)
                    }
                    _ => {
                        let cands = viable
                            .into_iter()
                            .map(|candidate| {
                                c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst)
                            })
                            .collect();
                        self.push_diag(BodyDiag::AmbiguousTraitInst {
                            primary: expr.span(self.body()).into(),
                            cands,
                            required_by: None,
                        });
                        return ExprProp::invalid(self.db);
                    }
                }
            }
            Err(MethodSelectionError::NotFound) => {
                let diag = BodyDiag::ops_trait_not_implemented(
                    self.db,
                    expr.span(self.body()).into(),
                    lhs_ty,
                    op,
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
            Err(err) => {
                let span = expr.span(self.body());
                let diag = body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(lhs_ty, span.clone().into()),
                    Spanned::new(op.trait_method(self.db), span.into()),
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        let callable = Callable::new(self.db, method, expr.span(self.body()).into(), Some(inst))
            .expect("failed to create Callable for core::ops trait method");

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        self.env.register_semantic_call(expr, callable);
        let mut result = ExprProp::new(ret_ty, true);
        if !self.closure_type_expectations.is_empty()
            && let Some(expected) = contextual_expected
        {
            self.env.type_expr(expr, result.clone());
            let (replayed, outcome) =
                self.replay_typed_expr_with_closure_type_expectations(expr, expected);
            if outcome == ClosureReplayOutcome::Replayed {
                result = replayed;
            }
        }
        if needs_confirmation {
            let goal = self
                .env
                .callable_expr(expr)
                .and_then(Callable::trait_inst)
                .unwrap_or(inst);
            self.env.register_trait_obligation(TraitObligation {
                goal,
                origin: TraitObligationOrigin::GenericConfirmation { expr },
                span: expr.span(self.body()).into(),
            });
        }
        result
    }

    fn instantiated_ops_rhs_ty(
        &mut self,
        func_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> Option<TyId<'db>> {
        let (base, gen_args) = func_ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) else {
            return None;
        };
        let mut expected_rhs = func_def
            .arg_tys(self.db)
            .get(1)?
            .instantiate(self.db, gen_args);
        let mut subst = AssocTySubst::new(inst);
        expected_rhs = self.normalize_ty(expected_rhs.fold_with(self.db, &mut subst));
        Some(expected_rhs)
    }

    fn check_or_constrain_expr_to_expected(&mut self, expr: ExprId, expected: TyId<'db>) {
        let Some(prop) = self.env.typed_expr(expr) else {
            self.check_expr(expr, expected);
            return;
        };
        let actual = self
            .try_coerce_capability_for_expr_to_expected(expr, prop.ty, expected)
            .unwrap_or(prop.ty);
        self.unify_ty(Typeable::Expr(expr, prop), actual, expected);
    }

    fn check_assign_lhs(&mut self, lhs: ExprId, typed_lhs: &ExprProp<'db>) -> AssignLhsStatus {
        self.check_assign_lhs_with_mode(lhs, typed_lhs, false)
    }

    fn check_assign_lhs_with_mode(
        &mut self,
        lhs: ExprId,
        typed_lhs: &ExprProp<'db>,
        rebinds_capability: bool,
    ) -> AssignLhsStatus {
        self.check_assign_lhs_with_mode_context(lhs, typed_lhs, rebinds_capability, None, false)
    }

    fn check_assign_lhs_with_mode_context(
        &mut self,
        lhs: ExprId,
        typed_lhs: &ExprProp<'db>,
        rebinds_capability: bool,
        assignment: Option<ExprId>,
        force_deferred: bool,
    ) -> AssignLhsStatus {
        if !self.is_assignable_expr(lhs) {
            if !typed_lhs.ty.has_invalid(self.db) {
                let diag = BodyDiag::NonAssignableExpr(lhs.span(self.body()).into());
                self.push_diag(diag);
            }

            return AssignLhsStatus::NonAssignable;
        }

        let captured = self
            .find_base_binding(lhs)
            .is_some_and(|binding| self.env.binding_is_capture(binding));
        if force_deferred || self.place_check_requires_deferred(lhs) {
            self.pending_place_checks.push(PendingPlaceCheck::Assign {
                assignment,
                lhs,
                captured,
                rebinds_capability,
            });
            return AssignLhsStatus::Deferred;
        }
        let Some(place) = self.current_expr_place(lhs) else {
            if !typed_lhs.ty.has_invalid(self.db) {
                self.push_diag(BodyDiag::NonAssignableExpr(lhs.span(self.body()).into()));
            }
            return AssignLhsStatus::NonAssignable;
        };
        self.check_resolved_assign_lhs(lhs, typed_lhs, &place, captured, rebinds_capability)
    }

    fn check_resolved_assign_lhs(
        &mut self,
        lhs: ExprId,
        typed_lhs: &ExprProp<'db>,
        place: &Place<'db>,
        captured: bool,
        rebinds_capability: bool,
    ) -> AssignLhsStatus {
        let PlaceBase::Binding(binding) = place.base;
        let reaches_mut_capability = if rebinds_capability {
            self.place_reaches_mut_capability_before_result(place)
        } else {
            self.place_reaches_mut_capability(place)
        };
        if captured && !reaches_mut_capability {
            self.push_diag(BodyDiag::AssignToCapturedBinding {
                primary: lhs.span(self.body()).into(),
                binding: Some((binding.binding_name(&self.env), binding.def_span(&self.env))),
            });
            return AssignLhsStatus::Immutable;
        }

        let slot_is_mut = !rebinds_capability && typed_lhs.is_mut
            || rebinds_capability && (binding.is_mut() || reaches_mut_capability);
        if !slot_is_mut {
            let diag = BodyDiag::ImmutableAssignment {
                primary: lhs.span(self.body()).into(),
                binding: Some((binding.binding_name(&self.env), binding.def_span(&self.env))),
                capability_rebind: rebinds_capability,
            };

            self.push_diag(diag);
            return AssignLhsStatus::Immutable;
        }

        AssignLhsStatus::Assignable
    }

    fn merge_assignment_borrow_providers(
        &mut self,
        lhs: ExprId,
        rhs: ExprId,
        typed_lhs: &ExprProp<'db>,
    ) {
        if typed_lhs.ty.as_capability(self.db).is_none() {
            return;
        }
        let Some(place) = self.env.expr_place(lhs) else {
            return;
        };
        if !place.projections.is_empty() {
            return;
        }
        let PlaceBase::Binding(binding) = place.base;
        self.merge_concrete_borrow_providers(
            binding.def_span(&self.env),
            self.concrete_borrow_provider_for_binding(binding),
            rhs.span(self.body()).into(),
            self.env
                .typed_expr(rhs)
                .and_then(|prop| prop.borrow_provider),
        );
    }

    fn place_reaches_mut_capability_before_result(&mut self, place: &Place<'db>) -> bool {
        if place.projections.is_empty() {
            return false;
        }
        let PlaceBase::Binding(binding) = place.base;
        std::iter::once(self.env.lookup_binding_ty(&binding))
            .chain(
                place
                    .projections
                    .iter()
                    .take(place.projections.len() - 1)
                    .copied()
                    .map(PlaceProjection::result_ty),
            )
            .any(|ty| {
                matches!(
                    self.normalize_ty(ty).as_capability(self.db),
                    Some((CapabilityKind::Mut, _))
                )
            })
    }

    fn place_reaches_mut_capability(&mut self, place: &Place<'db>) -> bool {
        let PlaceBase::Binding(binding) = place.base;
        if matches!(binding, LocalBinding::EffectParam { is_mut: true, .. }) {
            return true;
        }
        let mut tys = vec![self.env.lookup_binding_ty(&binding)];
        tys.extend(
            place
                .projections
                .iter()
                .copied()
                .map(PlaceProjection::result_ty),
        );
        tys.into_iter().any(|ty| {
            matches!(
                self.normalize_ty(ty).as_capability(self.db),
                Some((CapabilityKind::Mut, _))
            )
        })
    }

    fn place_check_requires_deferred(&self, expr: ExprId) -> bool {
        self.find_base_binding(expr).is_some() && self.place_expr_has_inference(expr)
    }

    fn place_expr_has_inference(&self, expr: ExprId) -> bool {
        if self
            .env
            .typed_expr(expr)
            .is_some_and(|prop| prop.ty.has_var(self.db))
        {
            return true;
        }
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Field(base, ..))
            | Partial::Present(Expr::Bin(base, _, BinOp::Index)) => {
                self.place_expr_has_inference(*base)
            }
            Partial::Present(Expr::Un(base, UnOp::Mut | UnOp::Ref)) => {
                self.place_expr_has_inference(*base)
            }
            _ => false,
        }
    }

    fn current_expr_place(&mut self, expr: ExprId) -> Option<Place<'db>> {
        let db = self.db;
        let body = self.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let env = &self.env;
        let table = &mut self.table;
        Place::from_expr_in_body(
            db,
            body,
            expr,
            |expr| env.typed_expr(expr).and_then(|prop| prop.binding),
            |expr| {
                let ty = env
                    .typed_expr(expr)
                    .map_or_else(|| TyId::invalid(db, InvalidCause::Other), |prop| prop.ty);
                let mut prober = super::env::Prober::new(table, scope);
                normalize_ty(db, ty.fold_with(db, &mut prober), scope, assumptions)
            },
        )
    }

    pub(super) fn resolve_pending_place_checks(&mut self) {
        for check in std::mem::take(&mut self.pending_place_checks) {
            let source = match check {
                PendingPlaceCheck::Assign { lhs, .. } => lhs,
                PendingPlaceCheck::Borrow { source, .. } => source,
            };
            let Some(mut source_prop) = self.env.typed_expr(source) else {
                continue;
            };
            source_prop.ty = self.normalize_ty(source_prop.ty);
            if source_prop.ty.has_invalid(self.db) || source_prop.ty.has_var(self.db) {
                continue;
            }
            let Some(place) = self.current_expr_place(source) else {
                self.push_diag(match check {
                    PendingPlaceCheck::Assign { .. } => {
                        BodyDiag::NonAssignableExpr(source.span(self.body()).into())
                    }
                    PendingPlaceCheck::Borrow { expr, .. } => BodyDiag::BorrowFromNonPlace {
                        primary: expr.span(self.body()).into(),
                    },
                });
                continue;
            };
            match check {
                PendingPlaceCheck::Assign {
                    assignment,
                    lhs,
                    captured,
                    rebinds_capability,
                } => {
                    let rebinds_capability = assignment
                        .and_then(|assignment| {
                            let Partial::Present(Expr::Assign(_, rhs)) =
                                assignment.data(self.db, self.body())
                            else {
                                return None;
                            };
                            let rebinds = self
                                .assignment_normalize_ty(source_prop.ty)
                                .as_capability(self.db)
                                .is_some()
                                && self.assignment_rhs_may_construct_capability(*rhs)
                                && self.assignment_rhs_outcome(*rhs)
                                    == AssignmentRhsOutcome::Capability;
                            self.env
                                .set_assignment_rebinds_capability(assignment, rebinds);
                            Some(rebinds)
                        })
                        .unwrap_or(rebinds_capability);
                    let status = self.check_resolved_assign_lhs(
                        lhs,
                        &source_prop,
                        &place,
                        captured,
                        rebinds_capability,
                    );
                    if status == AssignLhsStatus::Assignable
                        && let Some(assignment) = assignment
                        && let Partial::Present(Expr::Assign(_, rhs)) =
                            assignment.data(self.db, self.body())
                    {
                        self.merge_assignment_borrow_providers(lhs, *rhs, &source_prop);
                    }
                }
                PendingPlaceCheck::Borrow {
                    expr,
                    kind,
                    captured,
                    ..
                } => {
                    let valid = match kind {
                        BorrowKind::Ref => true,
                        BorrowKind::Mut
                            if captured && !self.place_reaches_mut_capability(&place) =>
                        {
                            let PlaceBase::Binding(binding) = place.base;
                            self.push_diag(BodyDiag::BorrowMutFromCapturedBinding {
                                primary: expr.span(self.body()).into(),
                                binding: Some((
                                    binding.binding_name(&self.env),
                                    binding.def_span(&self.env),
                                )),
                            });
                            false
                        }
                        BorrowKind::Mut if !source_prop.is_mut => {
                            let PlaceBase::Binding(binding) = place.base;
                            self.push_diag(BodyDiag::CannotBorrowMut {
                                primary: expr.span(self.body()).into(),
                                binding: Some((
                                    binding.binding_name(&self.env),
                                    binding.def_span(&self.env),
                                )),
                            });
                            false
                        }
                        BorrowKind::Mut => true,
                    };
                    if valid && let Some(mut prop) = self.env.typed_expr(expr) {
                        prop.borrow_provider = self.concrete_borrow_provider_for_place(&place);
                        self.env.type_expr(expr, prop);
                    }
                }
            }
        }
    }

    fn check_expr_in_new_scope(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        result_expectation: ResultExpectation<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        self.env.enter_scope(expr);
        let ty = if result_discarded {
            self.check_expr_with_discarded_result(expr, expected)
        } else if result_expectation.is_capability_assignment() {
            self.check_expr_with_result_context(expr, result_expectation, false)
        } else {
            self.check_expr(expr, expected)
        };
        self.env.leave_scope();

        ty
    }

    /// Returns the base binding for a given expression if it exists.
    ///
    /// This function traverses the expression tree to find the base binding,
    /// which is the original variable or binding that the expression refers to.
    ///
    /// # Parameters
    ///
    /// - `expr`: The expression ID for which to find the base binding.
    ///
    /// # Returns
    ///
    /// An `Option` containing the `LocalBinding` if a base binding is found,
    /// or `None` if there is no base binding.
    pub(super) fn find_base_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Field(lhs, ..) => self.find_base_binding(*lhs),
            Expr::Bin(lhs, _rhs, op) if *op == BinOp::Index => self.find_base_binding(*lhs),
            Expr::Path(..) => self.env.typed_expr(expr)?.binding,
            _ => None,
        }
    }

    /// Returns `true`` if the expression can be used as an left hand side of an
    /// assignment.
    /// This method doesn't take mutability into account.
    fn is_assignable_expr(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        match expr_data {
            Expr::Path(..) | Expr::Field(..) => true,
            Expr::Bin(_, _, op) if *op == BinOp::Index => true,
            _ => false,
        }
    }
}

fn reify_unresolved_pattern_slots<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    slot_bindings: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> T
where
    T: crate::analysis::ty::fold::TyFoldable<'db>,
{
    struct SlotReifier<'a, 'db> {
        slot_bindings: &'a FxHashMap<TyId<'db>, TyId<'db>>,
    }

    impl<'db> crate::analysis::ty::fold::TyFolder<'db> for SlotReifier<'_, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            self.slot_bindings
                .get(&ty)
                .copied()
                .unwrap_or_else(|| ty.super_fold_with(db, self))
        }
    }

    value.fold_with(db, &mut SlotReifier { slot_bindings })
}

fn body_diag_from_method_selection_err<'db>(
    db: &'db dyn HirAnalysisDb,
    err: MethodSelectionError<'db>,
    receiver: Spanned<'db, TyId<'db>>,
    method: Spanned<'db, IdentId<'db>>,
) -> FuncBodyDiag<'db> {
    match err {
        MethodSelectionError::ReceiverTypeMustBeKnown => {
            BodyDiag::TypeMustBeKnown(receiver.span).into()
        }
        MethodSelectionError::AmbiguousInherentMethod(candidates) => {
            BodyDiag::AmbiguousInherentMethodCall {
                primary: method.span,
                method_name: method.data,
                candidates,
            }
            .into()
        }

        MethodSelectionError::AmbiguousTraitMethod(ambiguous) => BodyDiag::AmbiguousTrait {
            primary: method.span,
            method_name: method.data,
            traits: ambiguous.diagnostic_traits,
        }
        .into(),

        MethodSelectionError::NotFound => {
            let base_ty = receiver.data.base_ty(db);
            PathResDiag::MethodNotFound {
                primary: method.span,
                method_name: method.data,
                receiver: Either::Left(base_ty),
                callable_field: None,
            }
            .into()
        }

        MethodSelectionError::InvisibleInherentMethod(func) => {
            PathResDiag::Invisible(method.span, method.data, func.name_span().into()).into()
        }

        MethodSelectionError::InvisibleTraitMethod(traits) => BodyDiag::InvisibleAmbiguousTrait {
            primary: method.span,
            traits,
        }
        .into(),
    }
}

fn resolve_ident_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    env: &TyCheckEnv<'db>,
    path: PathId<'db>,
    ident_span: DynLazySpan<'db>,
    minter: &HoleMinter<'db>,
) -> ResolvedPathInBody<'db> {
    let ident = path.ident(db).unwrap();

    let resolve_bucket = |bucket: &NameResBucket<'db>, scope| {
        // First, surface any ambiguity/conflict in the bucket as a dedicated
        // name-resolution diagnostic instead of silently degrading to
        // "undefined variable".
        for (_, err) in bucket.errors() {
            match err {
                NameResolutionError::Ambiguous(cands) => {
                    let mut cand_spans = Vec::new();
                    for name in cands.iter() {
                        if let Some(span) = name.kind.name_span(db) {
                            let from_implicit = name
                                .derivation
                                .use_stmt()
                                .map(|use_| use_.is_synthetic_use(db))
                                .unwrap_or(false);
                            cand_spans.push((span, from_implicit));
                        }
                    }

                    let diag = PathResDiag::Ambiguous(ident_span.clone(), ident, cand_spans);
                    return ResolvedPathInBody::Diag(diag.into());
                }
                NameResolutionError::Conflict(conf_ident, spans) => {
                    let diag = PathResDiag::Conflict(*conf_ident, spans.clone());
                    return ResolvedPathInBody::Diag(diag.into());
                }
                _ => {}
            }
        }

        let Ok(res) = bucket.pick_any(&[NameDomain::VALUE, NameDomain::TYPE]) else {
            return ResolvedPathInBody::Invalid;
        };
        let Ok(reso) =
            resolve_name_res_with_minter(db, res, None, path, scope, env.assumptions(), minter)
        else {
            return ResolvedPathInBody::Invalid;
        };
        ResolvedPathInBody::Reso(reso)
    };

    let mut current_idx = env.current_block_idx();

    loop {
        let block = env.get_block(current_idx);
        if let Some(binding) = block.lookup_var(ident) {
            return ResolvedPathInBody::Binding(binding);
        }

        let scope = block.scope;
        let directive = QueryDirective::for_scope(db, scope).disallow_lex();
        let query = EarlyNameQueryId::new(db, ident, scope, directive);
        let bucket = resolve_query(db, query);

        let resolved = resolve_bucket(bucket, scope);
        if matches!(resolved, ResolvedPathInBody::Invalid) {
            if current_idx == 0 {
                break;
            } else {
                current_idx -= 1;
            }
        } else {
            return resolved;
        }
    }

    let body_scope = env.body().scope();
    let directive = QueryDirective::for_scope(db, body_scope);
    let query = EarlyNameQueryId::new(db, ident, body_scope, directive);
    let bucket = resolve_query(db, query);
    match resolve_bucket(bucket, env.scope()) {
        ResolvedPathInBody::Invalid => ResolvedPathInBody::NewBinding(ident),
        r => r,
    }
}

/// This traits are intended to be implemented by the operators that can work as
/// a syntax sugar for a trait method. For example, binary `+` operator
/// implements this trait to be able to work as a syntax sugar for
/// `core::ops::Add` trait method.
///
/// TODO: We need to refine this trait definition to connect core library traits
/// smoothly.
pub(crate) trait TraitOps {
    fn trait_path_segments(&self) -> [&str; 2] {
        ["ops", self.triple()[0]]
    }

    fn core_trait_path<'db>(&self, db: &'db dyn HirAnalysisDb) -> PathId<'db> {
        let mut path = PathId::from_ident(db, IdentId::new(db, "core".to_string()));
        for s in self.trait_path_segments() {
            path = path.push_ident(db, IdentId::new(db, s.to_string()));
        }
        path
    }

    fn trait_method<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        IdentId::new(db, self.triple()[1].to_string())
    }

    fn op_symbol<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        IdentId::new(db, self.triple()[2].to_string())
    }

    fn triple(&self) -> [&str; 3];
}

impl TraitOps for UnOp {
    fn triple(&self) -> [&str; 3] {
        match self {
            UnOp::Plus => ["UnaryPlus", "add", "+"],
            UnOp::Minus => ["Neg", "neg", "-"],
            UnOp::Not => ["Not", "not", "!"],
            UnOp::BitNot => ["BitNot", "bit_not", "~"],
            UnOp::Mut => ["MutBorrow", "mut_borrow", "mut"],
            UnOp::Ref => ["RefBorrow", "ref_borrow", "ref"],
        }
    }
}

impl TraitOps for BinOp {
    fn triple(&self) -> [&str; 3] {
        match self {
            BinOp::Arith(arith_op) => {
                use ArithBinOp::*;

                match arith_op {
                    Add => ["Add", "add", "+"],
                    Sub => ["Sub", "sub", "-"],
                    Mul => ["Mul", "mul", "*"],
                    Div => ["Div", "div", "/"],
                    Rem => ["Rem", "rem", "%"],
                    Pow => ["Pow", "pow", "**"],
                    LShift => ["Shl", "shl", "<<"],
                    RShift => ["Shr", "shr", ">>"],
                    BitAnd => ["BitAnd", "bitand", "&"],
                    BitOr => ["BitOr", "bitor", "|"],
                    BitXor => ["BitXor", "bitxor", "^"],
                    // Range is handled specially - it constructs a Range type
                    // rather than calling a trait method
                    Range => ["Range", "range", ".."],
                }
            }

            BinOp::Comp(comp_op) => {
                use crate::core::hir_def::CompBinOp::*;

                // Comp
                match comp_op {
                    Eq => ["Eq", "eq", "=="],
                    NotEq => ["Eq", "ne", "!="],
                    Lt => ["Ord", "lt", "<"],
                    LtEq => ["Ord", "le", "<="],
                    Gt => ["Ord", "gt", ">"],
                    GtEq => ["Ord", "ge", ">="],
                }
            }

            BinOp::Logical(_) => {
                unreachable!()
            }

            BinOp::Index => ["Index", "index", "[]"],
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct AugAssignOp(ArithBinOp);

impl TraitOps for AugAssignOp {
    fn triple(&self) -> [&str; 3] {
        use ArithBinOp::*;
        match self.0 {
            Add => ["AddAssign", "add_assign", "+="],
            Sub => ["SubAssign", "sub_assign", "-="],
            Mul => ["MulAssign", "mul_assign", "*="],
            Div => ["DivAssign", "div_assign", "/="],
            Rem => ["RemAssign", "rem_assign", "%="],
            Pow => ["PowAssign", "pow_assign", "**="],
            LShift => ["ShlAssign", "shl_assign", "<<="],
            RShift => ["ShrAssign", "shr_assign", ">>="],
            BitAnd => ["BitAndAssign", "bitand_assign", "&="],
            BitOr => ["BitOrAssign", "bitor_assign", "|="],
            BitXor => ["BitXorAssign", "bitxor_assign", "^="],
            // Range doesn't have an augmented assignment form
            Range => unreachable!("Range operator cannot be used in augmented assignment"),
        }
    }
}
