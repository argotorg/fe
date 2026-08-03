use crate::{
    analysis::place::Place,
    hir_def::{
        ArithBinOp, BinOp, Body, ClosureDef, CondId, Contract, Expr, ExprId, FieldIndex, Func,
        IdentId, ItemKind, Partial, Pat, PatId, PathId, Stmt, StmtId, UnOp, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use std::collections::VecDeque;
use thin_vec::ThinVec;

use super::effect_env as keyed_effect_env;
use super::owner::{BodyOwner, ClosureReceiverMode};
use super::{
    Callable, ConstIntrinsicKind, ConstRef, SemanticExprLowering, TyChecker, TypedBody,
    ValuePathRef, stmt::ForLoopSeq,
};
use crate::analysis::ty::pattern_ir::{
    PatternAnalysisStatus, PatternStore, ValidatedPat, ValidatedPatId,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::CallableInputLayoutHoleOrigin,
        corelib::resolve_lib_type_path,
        effects::{
            EffectKeyKind,
            elaborate::{build_pattern_from_requirement_decl, seed_forwarder_from_requirement},
            model::EffectRequirementDecl,
        },
        fold::{TyFoldable, TyFolder, rewrite_types},
        provider::ProviderAddressSpace,
        trait_def::TraitInstId,
        trait_resolution::{PredicateListId, constraint::collect_func_effect_provider_constraints},
        ty_contains_const_hole,
        ty_def::{
            ClosureCaptureAccess, ClosureTy, InvalidCause, StringFallback, TyData, TyId, TyVarSort,
        },
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
};
use crate::core::semantic::{
    EffectEnvView, EffectRequirement, ProviderBinding, ResolvedEffectBindingInfo,
};

pub(crate) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    body: Body<'db>,

    pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
    expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
    expr_normal_completion: SecondaryMap<ExprId, Option<bool>>,
    /// Boolean value produced by every normal completion of the expression.
    ///
    /// `None` means either value remains possible (or no finalized fact has
    /// been recorded); escape paths do not invalidate a value known on the
    /// remaining normal paths.
    expr_normal_bool_value: SecondaryMap<ExprId, Option<bool>>,
    /// Boolean value produced by every normal completion of a condition.
    cond_normal_bool_value: SecondaryMap<CondId, Option<bool>>,
    assignment_rebinds_capability: SecondaryMap<ExprId, Option<bool>>,
    contextual_view_sources: SecondaryMap<ExprId, Option<TyId<'db>>>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    callables: SecondaryMap<ExprId, Option<Callable<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<super::RecordInitLowering<'db>>>,
    closure_infos: SecondaryMap<ExprId, Option<ClosureInfo<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
    /// Contextual closure constraints in force when a deferred expression was
    /// first checked.
    ///
    /// Deferred resolution happens after the ordinary expression stack has
    /// unwound, so these constraints must live with the body environment
    /// rather than in a temporary checker stack. One entry is retained per
    /// expression and consumed only after final reconciliation succeeds.
    deferred_closure_replay_contexts:
        SecondaryMap<ExprId, Option<DeferredClosureReplayContext<'db>>>,

    deferred: VecDeque<DeferredTask<'db>>,

    effect_env: keyed_effect_env::EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    base_assumptions: PredicateListId<'db>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    binding_block_idx: FxHashMap<LocalBinding<'db>, usize>,
    binding_closure_depth: FxHashMap<LocalBinding<'db>, usize>,
    /// Closure literals whose values flow into a local binding's initializer.
    ///
    /// A matching inferred nominal type alone is not enough to establish
    /// alias provenance: an independent parameter can be unified with the
    /// same nominal through a repeated generic argument.
    contextual_closure_binding_origins: FxHashMap<LocalBinding<'db>, FxHashSet<ClosureDef<'db>>>,
    closure_stack: Vec<ActiveClosure<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,
    /// Lexical closure ancestry at the expression's original check site.
    ///
    /// Deferred resolution and contextual replay can revisit an expression
    /// after its enclosing closures have left `closure_stack`. Keep the
    /// original ancestry so effect-provider provenance and late capture
    /// contributions can still be attributed to the source closures rather
    /// than the finalization context.
    expr_closure_ancestry: SecondaryMap<ExprId, Option<Vec<ClosureDef<'db>>>>,
    /// Lexical effect-provider frames at the expression's original check site.
    ///
    /// In particular, a deferred method may not resolve until an enclosing
    /// `with` frame has been popped. Retaining the original environment lets
    /// late resolution use the providers that were actually in source scope.
    expr_effect_env: SecondaryMap<ExprId, Option<keyed_effect_env::EffectEnv<'db>>>,
    pub(super) first_return_borrow_provider: Option<(DynLazySpan<'db>, ProviderAddressSpace)>,

    /// Param bindings for transfer to TypedBody
    param_bindings: Vec<LocalBinding<'db>>,
    /// Pat bindings for transfer to TypedBody
    pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
    local_borrow_providers: SecondaryMap<PatId, Option<ProviderAddressSpace>>,
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
    pattern_store: PatternStore<'db>,
    pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,

    /// Resolved effect arguments at call sites, keyed by the call expression.
    call_effect_args: SecondaryMap<ExprId, Option<Vec<super::ResolvedEffectArg<'db>>>>,
    /// Capture contributions discovered after their lexical closures have
    /// already been checked, keyed by the effectful call expression.
    late_effect_capture_contributions:
        SecondaryMap<ExprId, Option<Vec<LateClosureCaptureContribution<'db>>>>,
    /// Closure descriptor replacements produced after deferred effect
    /// resolution. Final type folding applies these transitively to every
    /// typed artifact.
    closure_ty_replacements: FxHashMap<TyId<'db>, TyId<'db>>,

    /// Resolved Seq trait methods for for-loops, keyed by the for statement.
    for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ClosureInfo<'db> {
    pub def: ClosureDef<'db>,
    pub body: ExprId,
    /// Every value-producing expression consumed as a return from this
    /// closure, including the implicit body result and explicit `return`
    /// expressions.
    pub return_exprs: Vec<ExprId>,
    pub params: Vec<LocalBinding<'db>>,
    pub captures: Vec<ClosureCapture<'db>>,
    /// Expression-local capture accesses in the same order as `captures`.
    ///
    /// Contextual replay can refine one leaf of an already-typed aggregate
    /// return without changing unrelated uses of the same binding. Keeping
    /// the contributions separate lets replay replace that leaf and then
    /// recompute the aggregate capture access exactly.
    pub(crate) capture_expr_accesses: Vec<IndexMap<ExprId, ClosureCaptureAccess>>,
    pub ty: ClosureTy<'db>,
    pub return_borrow_provider: Option<ProviderAddressSpace>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ClosureCapture<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
    pub construction: ClosureCaptureConstruction,
    pub access_without_return: ClosureCaptureAccess,
    pub access: ClosureCaptureAccess,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ClosureCaptureConstruction {
    Copy,
    Deferred,
    Move,
}

#[derive(Debug, Clone)]
pub(super) struct PendingClosureCapture<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
    pub access_without_return: ClosureCaptureAccess,
    pub access: ClosureCaptureAccess,
    pub expr_accesses: IndexMap<ExprId, ClosureCaptureAccess>,
}

#[derive(Debug, Clone)]
struct ActiveClosure<'db> {
    def: ClosureDef<'db>,
    boundary_block_idx: usize,
    params: Vec<LocalBinding<'db>>,
    return_exprs: Vec<ExprId>,
    captures: IndexMap<LocalBinding<'db>, PendingClosureCapture<'db>>,
}

pub(super) struct BodyCtxSnapshot<'db> {
    loop_stack: Vec<StmtId>,
    first_return_borrow_provider: Option<(DynLazySpan<'db>, ProviderAddressSpace)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct DeferredClosureReplayContext<'db> {
    pub expected: TyId<'db>,
    pub expectations: Vec<(TyId<'db>, super::ClosureExpectation<'db>)>,
}

/// Mutable type-checking artifacts that contextual closure replay can replace.
///
/// Replay is intentionally transactional: aggregates, calls, closure
/// descriptors, expression properties, and resolved effect arguments must all
/// describe the same specialization. Whole-map snapshots keep rollback
/// complete without trying to predict which descendants a structural replay
/// will reach.
pub(super) struct ClosureReplayEnvSnapshot<'db> {
    pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
    expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
    expr_normal_completion: SecondaryMap<ExprId, Option<bool>>,
    expr_normal_bool_value: SecondaryMap<ExprId, Option<bool>>,
    cond_normal_bool_value: SecondaryMap<CondId, Option<bool>>,
    assignment_rebinds_capability: SecondaryMap<ExprId, Option<bool>>,
    contextual_view_sources: SecondaryMap<ExprId, Option<TyId<'db>>>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    callables: SecondaryMap<ExprId, Option<Callable<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<super::RecordInitLowering<'db>>>,
    closure_infos: SecondaryMap<ExprId, Option<ClosureInfo<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
    deferred_closure_replay_contexts:
        SecondaryMap<ExprId, Option<DeferredClosureReplayContext<'db>>>,
    deferred: VecDeque<DeferredTask<'db>>,
    effect_env: keyed_effect_env::EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    base_assumptions: PredicateListId<'db>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    binding_block_idx: FxHashMap<LocalBinding<'db>, usize>,
    binding_closure_depth: FxHashMap<LocalBinding<'db>, usize>,
    contextual_closure_binding_origins: FxHashMap<LocalBinding<'db>, FxHashSet<ClosureDef<'db>>>,
    closure_stack: Vec<ActiveClosure<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,
    expr_closure_ancestry: SecondaryMap<ExprId, Option<Vec<ClosureDef<'db>>>>,
    expr_effect_env: SecondaryMap<ExprId, Option<keyed_effect_env::EffectEnv<'db>>>,
    first_return_borrow_provider: Option<(DynLazySpan<'db>, ProviderAddressSpace)>,
    param_bindings: Vec<LocalBinding<'db>>,
    pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
    local_borrow_providers: SecondaryMap<PatId, Option<ProviderAddressSpace>>,
    pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
    pattern_store: PatternStore<'db>,
    pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,
    call_effect_args: SecondaryMap<ExprId, Option<Vec<super::ResolvedEffectArg<'db>>>>,
    late_effect_capture_contributions:
        SecondaryMap<ExprId, Option<Vec<LateClosureCaptureContribution<'db>>>>,
    closure_ty_replacements: FxHashMap<TyId<'db>, TyId<'db>>,
    for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct LateClosureCaptureContribution<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
    pub access: ClosureCaptureAccess,
    pub provider_closure_depth: usize,
}

impl BodyCtxSnapshot<'_> {
    pub(super) fn return_borrow_provider(&self) -> Option<ProviderAddressSpace> {
        self.first_return_borrow_provider
            .as_ref()
            .map(|(_, provider)| *provider)
    }
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        fn const_owner_preds<'db>(
            db: &'db dyn HirAnalysisDb,
            scope: ScopeId<'db>,
        ) -> PredicateListId<'db> {
            match scope.parent_item(db) {
                Some(ItemKind::Trait(trait_)) => {
                    crate::semantic::constraints_for(db, trait_.into())
                }
                Some(ItemKind::ImplTrait(impl_trait)) => {
                    crate::semantic::constraints_for(db, impl_trait.into())
                }
                Some(ItemKind::Impl(impl_)) => crate::semantic::constraints_for(db, impl_.into()),
                _ => PredicateListId::empty_list(db),
            }
        }

        let Some(body) = owner.body(db) else {
            return Err(());
        };

        let owner_scope = owner.scope();

        // Compute base assumptions (without effect-derived bounds) up-front
        let (base_preds, base_assumptions) = match owner {
            BodyOwner::Func(func) => {
                // Trait methods implicitly assume `Self: Trait` in their bodies so
                // default method calls resolve against the trait being implemented.
                let preds = crate::semantic::func_body_assumptions(db, func);
                let assumptions = preds.extend_all_bounds(db);
                (preds, assumptions)
            }
            BodyOwner::AnonConstBody { .. } => {
                let containing_func = match owner_scope.parent_item(db) {
                    Some(ItemKind::Func(func)) => Some(func),
                    Some(ItemKind::Body(parent)) => parent.containing_func(db),
                    _ => None,
                };
                if let Some(func) = containing_func {
                    let preds = crate::semantic::func_body_assumptions(db, func);
                    let assumptions = preds.extend_all_bounds(db);
                    (preds, assumptions)
                } else {
                    // Walk up through nested body scopes to find an enclosing item (trait/impl).
                    let mut enclosing = owner_scope;
                    let mut parent_item = enclosing.parent_item(db);
                    while let Some(ItemKind::Body(parent)) = parent_item {
                        enclosing = parent.scope();
                        parent_item = enclosing.parent_item(db);
                    }

                    let preds = const_owner_preds(db, enclosing);
                    let assumptions = preds.extend_all_bounds(db);
                    (preds, assumptions)
                }
            }
            BodyOwner::Const(const_) => {
                let preds = const_owner_preds(db, const_.scope());
                let assumptions = preds.extend_all_bounds(db);
                (preds, assumptions)
            }
            _ => {
                let empty = PredicateListId::empty_list(db);
                (empty, empty)
            }
        };

        let mut env = Self {
            db,
            owner,
            owner_scope,
            body,
            pat_ty: SecondaryMap::new(),
            expr_ty: SecondaryMap::new(),
            expr_normal_completion: SecondaryMap::new(),
            expr_normal_bool_value: SecondaryMap::new(),
            cond_normal_bool_value: SecondaryMap::new(),
            assignment_rebinds_capability: SecondaryMap::new(),
            contextual_view_sources: SecondaryMap::new(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            callables: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            closure_infos: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            deferred_closure_replay_contexts: SecondaryMap::new(),
            deferred: VecDeque::new(),
            effect_env: keyed_effect_env::EffectEnv::new(),
            effect_bounds: ThinVec::new(),
            base_assumptions,
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            binding_block_idx: FxHashMap::default(),
            binding_closure_depth: FxHashMap::default(),
            contextual_closure_binding_origins: FxHashMap::default(),
            closure_stack: Vec::new(),
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
            expr_closure_ancestry: SecondaryMap::new(),
            expr_effect_env: SecondaryMap::new(),
            first_return_borrow_provider: None,
            param_bindings: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            local_borrow_providers: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            call_effect_args: SecondaryMap::new(),
            late_effect_capture_contributions: SecondaryMap::new(),
            closure_ty_replacements: FxHashMap::default(),
            for_loop_seq: SecondaryMap::new(),
        };

        env.enter_scope(body.expr(db));

        match owner {
            BodyOwner::Func(func) => {
                let arg_tys = func.arg_tys(db);
                for (idx, view) in func.params(db).enumerate() {
                    let mut ty = *arg_tys
                        .get(idx)
                        .map(|b| b.skip_binder())
                        .unwrap_or(&TyId::invalid(db, InvalidCause::ParseError));

                    if !ty.is_star_kind(db) {
                        ty = TyId::invalid(db, InvalidCause::Other);
                    }
                    if !view.is_self_param(db) && ty_contains_const_hole(db, ty) {
                        ty = TyId::invalid(db, InvalidCause::Other);
                    }
                    let var = LocalBinding::Param {
                        site: ParamSite::Func(func),
                        idx,
                        mode: view.mode(db),
                        ty,
                        is_mut: view.is_mut(db),
                    };

                    env.param_bindings.push(var);
                    if let Some(name) = view.name(db) {
                        env.register_var_in_current_scope(name, var);
                    };
                }
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(db) else {
                    return Ok(env);
                };
                let assumptions = base_assumptions;
                for (idx, param) in init.params(db).data(db).iter().enumerate() {
                    let mut ty = match param.ty.to_opt() {
                        Some(hir_ty) => lower_hir_ty(db, hir_ty, owner_scope, assumptions),
                        None => TyId::invalid(db, InvalidCause::ParseError),
                    };
                    if param.mode == FuncParamMode::View && ty.as_capability(db).is_none() {
                        ty = TyId::view_of(db, ty);
                    }

                    if !ty.is_star_kind(db) {
                        ty = TyId::invalid(db, InvalidCause::Other);
                    }
                    if ty_contains_const_hole(db, ty) {
                        ty = TyId::invalid(db, InvalidCause::Other);
                    }

                    let var = LocalBinding::Param {
                        site: ParamSite::ContractInit(contract),
                        idx,
                        mode: param.mode,
                        ty,
                        is_mut: param.is_mut,
                    };
                    env.param_bindings.push(var);
                    if let Some(name) = param.name() {
                        env.register_var_in_current_scope(name, var);
                    }
                }
            }
            BodyOwner::ContractRecvArm { .. } => {}
        }

        env.register_effect_bindings(base_assumptions);

        // Finalize assumptions by merging in effect-derived bounds
        let mut preds = base_preds.list(db).to_vec();
        preds.extend(env.effect_bounds.iter().copied());
        env.assumptions = PredicateListId::new(db, preds).extend_all_bounds(db);

        Ok(env)
    }

    fn register_effect_bindings(&mut self, base_assumptions: PredicateListId<'db>) {
        match self.owner {
            BodyOwner::Func(func) => self.register_func_effect_bindings(func),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            BodyOwner::ContractInit { .. } => {
                self.register_contract_effect_bindings(base_assumptions)
            }
            BodyOwner::ContractRecvArm { .. } => {
                self.register_contract_effect_bindings(base_assumptions)
            }
        }
    }

    fn register_func_effect_bindings(&mut self, func: Func<'db>) {
        self.effect_bounds
            .extend(collect_func_effect_provider_constraints(self.db, func));
        for binding in func.effect_requirements(self.db) {
            if !matches!(
                binding.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            ) {
                continue;
            }
            let idx = binding.binding_idx as usize;
            let Some(resolved_binding) =
                self.resolved_effect_binding(EffectParamSite::Func(func), idx)
            else {
                continue;
            };
            self.register_var_in_current_scope(
                resolved_binding.requirement.binding_name,
                LocalBinding::effect_param(&resolved_binding),
            );
        }
    }

    fn contract_effect_site(&self) -> Option<(Contract<'db>, EffectParamSite<'db>)> {
        match self.owner {
            BodyOwner::ContractInit { contract } => {
                Some((contract, EffectParamSite::ContractInit { contract }))
            }
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => Some((
                contract,
                EffectParamSite::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
            )),
            BodyOwner::Func(_)
            | BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::Closure { .. } => None,
        }
    }

    fn contract_effect_env_view(&self) -> Option<(Contract<'db>, EffectEnvView<'db>)> {
        self.contract_effect_site()
            .map(|(contract, site)| (contract, EffectEnvView::new(site)))
    }

    pub(super) fn semantic_effect_requirement(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<EffectRequirement<'db>> {
        self.resolved_effect_binding(site, idx)
            .map(|binding| binding.requirement)
    }

    pub(super) fn resolved_effect_binding(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ResolvedEffectBindingInfo<'db>> {
        EffectEnvView::new(site).resolved_binding(self.db, idx)
    }

    pub(super) fn provider_binding(
        &self,
        site: EffectParamSite<'db>,
        provider_idx: u32,
    ) -> Option<ProviderBinding<'db>> {
        EffectEnvView::new(site)
            .providers(self.db)
            .into_iter()
            .find(|provider| provider.provider_idx == provider_idx)
    }

    pub(super) fn resolved_provider_binding(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ProviderBinding<'db>> {
        self.resolved_effect_binding(site, idx)
            .map(|binding| binding.provider)
    }

    fn effect_binding_scope(&self, site: EffectParamSite<'db>) -> ScopeId<'db> {
        match site {
            EffectParamSite::Func(func) => func.scope(),
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => contract.scope(),
        }
    }

    fn resolved_effect_param_ty(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<TyId<'db>> {
        EffectEnvView::new(site).visible_effect_binding_ty(self.db, idx)
    }

    fn register_contract_effect_bindings(&mut self, _base_assumptions: PredicateListId<'db>) {
        let Some((_contract, view)) = self.contract_effect_env_view() else {
            return;
        };
        for binding in view.requirements(self.db) {
            if !matches!(
                binding.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            ) {
                continue;
            }

            if let (Some(provider), Some(trait_inst)) = (
                self.resolved_provider_binding(binding.binding_site, binding.binding_idx as usize),
                binding.key.key_trait(),
            ) {
                self.effect_bounds
                    .push(super::super::instantiate_trait_self(
                        self.db,
                        trait_inst,
                        provider.provider_ty,
                    ));
            }

            let idx = binding.binding_idx as usize;
            let Some(resolved_binding) = self.resolved_effect_binding(binding.binding_site, idx)
            else {
                continue;
            };
            self.register_var_in_current_scope(
                resolved_binding.requirement.binding_name,
                LocalBinding::effect_param(&resolved_binding),
            );
        }
    }

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<ExprProp<'db>> {
        self.expr_ty[expr].clone()
    }

    pub(super) fn expr_place(&self, expr: ExprId) -> Option<Place<'db>> {
        Place::from_expr_in_body(
            self.db,
            self.body,
            expr,
            |expr| self.typed_expr(expr).and_then(|p| p.binding),
            |expr| {
                self.typed_expr(expr).map_or_else(
                    || TyId::invalid(self.db, InvalidCause::Other),
                    |prop| prop.ty,
                )
            },
        )
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables[expr].replace(callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }

    pub(super) fn register_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) {
        if self.const_refs[expr].replace(const_ref).is_some() {
            panic!("const ref is already registered for the given expr")
        }
    }

    pub(super) fn register_value_path_ref(&mut self, expr: ExprId, value_path: ValuePathRef<'db>) {
        if self.value_path_refs[expr].replace(value_path).is_some() {
            panic!("value path ref is already registered for the given expr")
        }
    }

    pub(super) fn value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.value_path_refs[expr]
    }

    pub(super) fn register_for_loop_seq(&mut self, stmt: StmtId, seq: ForLoopSeq<'db>) {
        if self.for_loop_seq[stmt].replace(seq).is_some() {
            panic!("for loop seq is already registered for the given stmt")
        }
    }

    fn register_var_in_current_scope(&mut self, name: IdentId<'db>, binding: LocalBinding<'db>) {
        let block_idx = self.current_block_idx();
        self.var_env
            .last_mut()
            .expect("scope exists")
            .register_var(name, binding);
        self.binding_block_idx.insert(binding, block_idx);
        self.binding_closure_depth
            .insert(binding, self.closure_stack.len());
    }

    pub(super) fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables[expr].as_ref()
    }

    pub(super) fn semantic_expr_lowering(
        &self,
        expr: ExprId,
    ) -> Option<&SemanticExprLowering<'db>> {
        self.semantic_expr_lowering[expr].as_ref()
    }

    pub(super) fn replace_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        let slot = &mut self.callables[expr];
        assert!(
            slot.is_some(),
            "callable must be registered before it can be replaced"
        );
        *slot = Some(callable);
    }

    pub(super) fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.const_refs[expr]
    }

    pub(super) fn register_semantic_expr_lowering(
        &mut self,
        expr: ExprId,
        lowering: SemanticExprLowering<'db>,
    ) {
        if self.semantic_expr_lowering[expr]
            .replace(lowering)
            .is_some()
        {
            panic!("semantic expr lowering is already registered for the given expr")
        }
    }

    pub(super) fn register_record_init_lowering(
        &mut self,
        expr: ExprId,
        lowering: super::RecordInitLowering<'db>,
    ) {
        if self.record_init_lowering[expr].replace(lowering).is_some() {
            panic!("record init lowering is already registered for the given expr")
        }
    }

    pub(super) fn record_init_lowering(
        &self,
        expr: ExprId,
    ) -> Option<super::RecordInitLowering<'db>> {
        self.record_init_lowering[expr]
    }

    pub(super) fn replace_record_init_lowering(
        &mut self,
        expr: ExprId,
        lowering: super::RecordInitLowering<'db>,
    ) {
        let slot = &mut self.record_init_lowering[expr];
        assert!(
            slot.is_some(),
            "record init lowering must be registered before it can be replaced"
        );
        *slot = Some(lowering);
    }

    pub(super) fn register_closure_info(&mut self, expr: ExprId, info: ClosureInfo<'db>) {
        if self.closure_infos[expr].replace(info).is_some() {
            panic!("closure info is already registered for the given expr")
        }
    }

    pub(super) fn replace_closure_info(&mut self, expr: ExprId, info: ClosureInfo<'db>) {
        let slot = &mut self.closure_infos[expr];
        assert!(
            slot.is_some(),
            "closure info must be registered before it can be replaced"
        );
        *slot = Some(info);
    }

    pub(super) fn closure_info(&self, expr: ExprId) -> Option<&ClosureInfo<'db>> {
        self.closure_infos[expr].as_ref()
    }

    pub(super) fn record_deferred_closure_replay_context(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        expectations: &[(TyId<'db>, super::ClosureExpectation<'db>)],
    ) {
        let slot = &mut self.deferred_closure_replay_contexts[expr];
        let context = slot.get_or_insert_with(|| DeferredClosureReplayContext {
            expected,
            expectations: Vec::new(),
        });
        assert_eq!(
            context.expected, expected,
            "an expression cannot have conflicting deferred closure replay result contexts"
        );
        for expectation in expectations {
            if !context.expectations.contains(expectation) {
                context.expectations.push(expectation.clone());
            }
        }
    }

    pub(super) fn deferred_closure_replay_context(
        &self,
        expr: ExprId,
    ) -> Option<&DeferredClosureReplayContext<'db>> {
        self.deferred_closure_replay_contexts[expr].as_ref()
    }

    pub(super) fn consume_deferred_closure_replay_context(&mut self, expr: ExprId) {
        self.deferred_closure_replay_contexts[expr] = None;
    }

    pub(super) fn register_resolved_field_index(&mut self, expr: ExprId, field_index: u16) {
        if self.resolved_field_index[expr]
            .replace(field_index)
            .is_some()
        {
            panic!("resolved field index is already registered for the given expr")
        }
    }

    pub(super) fn register_semantic_call(&mut self, expr: ExprId, callable: Callable<'db>) {
        self.register_callable(expr, callable.clone());
        self.register_semantic_expr_lowering(
            expr,
            SemanticExprLowering::Call {
                callable,
                callee_is_receiver: false,
            },
        );
    }

    pub(super) fn register_semantic_value_call(&mut self, expr: ExprId, callable: Callable<'db>) {
        self.register_callable(expr, callable.clone());
        self.register_semantic_expr_lowering(
            expr,
            SemanticExprLowering::Call {
                callable,
                callee_is_receiver: true,
            },
        );
    }

    pub(super) fn replace_semantic_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        self.replace_callable(expr, callable.clone());
        let slot = &mut self.semantic_expr_lowering[expr];
        let Some(lowering) = slot.as_mut() else {
            panic!("semantic call lowering must be registered before it can be replaced")
        };
        match lowering {
            SemanticExprLowering::Call {
                callable: stored, ..
            }
            | SemanticExprLowering::CodeRegionIntrinsic {
                callable: stored, ..
            }
            | SemanticExprLowering::ConstIntrinsic {
                callable: stored, ..
            } => *stored = callable,
        }
    }

    pub(super) fn register_code_region_intrinsic(
        &mut self,
        expr: ExprId,
        callable: Callable<'db>,
        region_arg: ExprId,
        kind: super::CodeRegionIntrinsicKind,
    ) {
        self.register_callable(expr, callable.clone());
        self.register_semantic_expr_lowering(
            expr,
            SemanticExprLowering::CodeRegionIntrinsic {
                callable,
                region_arg,
                kind,
            },
        );
    }

    pub(super) fn register_const_intrinsic(
        &mut self,
        expr: ExprId,
        callable: Callable<'db>,
        kind: ConstIntrinsicKind,
    ) {
        self.register_callable(expr, callable.clone());
        self.register_semantic_expr_lowering(
            expr,
            SemanticExprLowering::ConstIntrinsic { callable, kind },
        );
    }

    pub(super) fn pattern_store(&self) -> &PatternStore<'db> {
        &self.pattern_store
    }

    /// Returns a callable if the body owner is a function.
    pub(super) fn func(&self) -> Option<CallableDef<'db>> {
        match self.owner {
            BodyOwner::Func(func) => func.as_callable(self.db),
            _ => None,
        }
    }

    pub(crate) fn assumptions(&self) -> PredicateListId<'db> {
        // Return the assumptions we computed in new, which includes
        // both generic bounds (if any) AND the effect parameter bounds.
        self.assumptions
    }

    pub(crate) fn base_assumptions(&self) -> PredicateListId<'db> {
        self.base_assumptions
    }

    #[cfg(test)]
    pub(super) fn seed_closure_replay_predicates_for_test(&mut self, predicate: TraitInstId<'db>) {
        self.effect_bounds.clear();
        self.effect_bounds.push(predicate);
        let assumptions = PredicateListId::new(self.db, vec![predicate]);
        self.base_assumptions = assumptions;
        self.assumptions = assumptions;
    }

    #[cfg(test)]
    pub(super) fn closure_replay_predicates_for_test(
        &self,
    ) -> (
        Vec<TraitInstId<'db>>,
        PredicateListId<'db>,
        PredicateListId<'db>,
    ) {
        (
            self.effect_bounds.iter().copied().collect(),
            self.base_assumptions,
            self.assumptions,
        )
    }

    pub(super) fn body(&self) -> Body<'db> {
        self.body
    }

    pub(super) fn owner(&self) -> BodyOwner<'db> {
        self.owner
    }

    pub(super) fn compute_expected_return(&self) -> TyId<'db> {
        match self.owner {
            BodyOwner::Func(func) => {
                let rt = func.return_ty(self.db);
                if func.has_explicit_return_ty(self.db) {
                    if rt.is_star_kind(self.db) && !ty_contains_const_hole(self.db, rt) {
                        rt
                    } else {
                        TyId::invalid(self.db, InvalidCause::Other)
                    }
                } else {
                    rt
                }
            }
            BodyOwner::Const(const_) => {
                let ty = const_.ty(self.db);
                if ty.is_star_kind(self.db) {
                    ty
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
            BodyOwner::AnonConstBody { expected, .. } => {
                if expected.is_star_kind(self.db) {
                    expected
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
            BodyOwner::ContractInit { .. } => TyId::unit(self.db),
            BodyOwner::ContractRecvArm { .. } => {
                let Some(arm) = self.owner.recv_arm(self.db) else {
                    return TyId::invalid(self.db, InvalidCause::Other);
                };
                let Some(ret_ty) = arm.ret_ty else {
                    return TyId::unit(self.db);
                };

                let ty = lower_hir_ty(self.db, ret_ty, self.owner_scope, self.assumptions());
                if ty.is_star_kind(self.db) && !ty_contains_const_hole(self.db, ty) {
                    ty
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
            BodyOwner::Closure { ty, .. } => ty.ret_ty(self.db),
        }
    }

    pub(super) fn lookup_binding_ty(&self, binding: &LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self
                .pat_ty
                .get(*pat)
                .copied()
                .flatten()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param { ty, .. } => *ty,

            LocalBinding::EffectParam { site, idx, .. } => self
                .resolved_effect_param_ty(*site, *idx)
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),
        }
    }

    pub(super) fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings[pat]
    }

    pub(super) fn contextual_closure_binding_origins(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<&FxHashSet<ClosureDef<'db>>> {
        self.contextual_closure_binding_origins.get(&binding)
    }

    pub(super) fn set_contextual_closure_binding_origins(
        &mut self,
        binding: LocalBinding<'db>,
        origins: FxHashSet<ClosureDef<'db>>,
    ) {
        if origins.is_empty() {
            self.contextual_closure_binding_origins.remove(&binding);
        } else {
            self.contextual_closure_binding_origins
                .insert(binding, origins);
        }
    }

    pub(super) fn local_borrow_provider(&self, pat: PatId) -> Option<ProviderAddressSpace> {
        self.local_borrow_providers[pat]
    }

    pub(super) fn set_local_borrow_provider(
        &mut self,
        pat: PatId,
        provider: Option<ProviderAddressSpace>,
    ) {
        self.local_borrow_providers[pat] = provider;
    }

    pub(super) fn set_pat_binding_mode(&mut self, pat: PatId, mode: PatBindingMode) {
        if self.pat_bindings[pat].is_some() {
            self.pat_binding_modes[pat] = Some(mode);
        }
    }

    pub(super) fn discard_pat_binding(&mut self, pat: PatId) {
        let Some(binding) = self.pat_bindings[pat].take() else {
            return;
        };
        self.local_borrow_providers[pat] = None;
        self.pat_binding_modes[pat] = None;
        self.pending_vars.retain(|_, pending| *pending != binding);
    }

    pub(super) fn effect_env_mut(&mut self) -> &mut keyed_effect_env::EffectEnv<'db> {
        &mut self.effect_env
    }

    pub(crate) fn effect_env(&self) -> &keyed_effect_env::EffectEnv<'db> {
        &self.effect_env
    }

    pub(super) fn push_call_effect_arg(
        &mut self,
        call_expr: ExprId,
        arg: super::ResolvedEffectArg<'db>,
    ) {
        self.call_effect_args[call_expr]
            .get_or_insert_default()
            .push(arg);
    }

    pub(super) fn call_effect_args(
        &self,
        call_expr: ExprId,
    ) -> Option<&[super::ResolvedEffectArg<'db>]> {
        self.call_effect_args[call_expr].as_deref()
    }

    pub(super) fn replace_call_effect_args(
        &mut self,
        call_expr: ExprId,
        args: Vec<super::ResolvedEffectArg<'db>>,
    ) {
        self.call_effect_args[call_expr] = (!args.is_empty()).then_some(args);
    }

    pub(super) fn replace_late_effect_capture_contributions(
        &mut self,
        call_expr: ExprId,
        contributions: Vec<LateClosureCaptureContribution<'db>>,
    ) {
        self.late_effect_capture_contributions[call_expr] =
            (!contributions.is_empty()).then_some(contributions);
    }

    pub(super) fn late_effect_capture_contributions(
        &self,
        call_expr: ExprId,
    ) -> Option<&[LateClosureCaptureContribution<'db>]> {
        self.late_effect_capture_contributions[call_expr].as_deref()
    }

    pub(super) fn set_closure_ty_replacements(
        &mut self,
        replacements: FxHashMap<TyId<'db>, TyId<'db>>,
    ) {
        self.apply_closure_ty_replacements(&replacements);
    }

    pub(super) fn rewrite_closure_types<T>(&self, value: T) -> T
    where
        T: TyFoldable<'db>,
    {
        rewrite_types(self.db, value, &self.closure_ty_replacements)
    }

    pub(super) fn apply_closure_ty_replacements(
        &mut self,
        replacements: &FxHashMap<TyId<'db>, TyId<'db>>,
    ) {
        if replacements.is_empty() {
            return;
        }

        for replacement in self.closure_ty_replacements.values_mut() {
            *replacement = rewrite_types(self.db, *replacement, replacements);
        }
        for (&old, &new) in replacements {
            self.closure_ty_replacements.insert(old, new);
        }

        self.expr_ty.values_mut().flatten().for_each(|prop| {
            *prop = rewrite_types(self.db, prop.clone(), replacements);
        });
        self.contextual_view_sources
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = rewrite_types(self.db, *ty, replacements));
        self.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = rewrite_types(self.db, *ty, replacements));
        self.const_refs
            .values_mut()
            .flatten()
            .for_each(|value| *value = rewrite_types(self.db, *value, replacements));
        self.value_path_refs
            .values_mut()
            .flatten()
            .for_each(|value| *value = rewrite_types(self.db, *value, replacements));
        self.callables.values_mut().flatten().for_each(|callable| {
            *callable = rewrite_types(self.db, callable.clone(), replacements);
        });
        self.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| {
                *lowering = rewrite_types(self.db, lowering.clone(), replacements);
            });
        self.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| {
                *lowering = rewrite_types(self.db, *lowering, replacements);
            });
        self.closure_infos.values_mut().flatten().for_each(|info| {
            *info = rewrite_types(self.db, info.clone(), replacements);
        });
        self.call_effect_args
            .values_mut()
            .flatten()
            .for_each(|args| {
                *args = rewrite_types(self.db, args.clone(), replacements);
            });
        self.late_effect_capture_contributions
            .values_mut()
            .flatten()
            .for_each(|contributions| {
                for contribution in contributions {
                    contribution.binding =
                        rewrite_types(self.db, contribution.binding, replacements);
                    contribution.ty = rewrite_types(self.db, contribution.ty, replacements);
                }
            });
        self.for_loop_seq.values_mut().flatten().for_each(|seq| {
            *seq = rewrite_types(self.db, seq.clone(), replacements);
        });
        self.param_bindings.iter_mut().for_each(|binding| {
            *binding = rewrite_types(self.db, *binding, replacements);
        });
        self.pat_bindings
            .values_mut()
            .flatten()
            .for_each(|binding| {
                *binding = rewrite_types(self.db, *binding, replacements);
            });

        for context in self.deferred_closure_replay_contexts.values_mut().flatten() {
            context.expected = rewrite_types(self.db, context.expected, replacements);
            for (subject, expectation) in &mut context.expectations {
                *subject = rewrite_types(self.db, *subject, replacements);
                for param in &mut expectation.params {
                    *param = rewrite_types(self.db, *param, replacements);
                }
                expectation.ret_ty = rewrite_types(self.db, expectation.ret_ty, replacements);
            }
        }

        for task in &mut self.deferred {
            match task {
                DeferredTask::Obligation(obligation) => {
                    obligation.goal = rewrite_types(self.db, obligation.goal, replacements);
                }
                DeferredTask::Method(pending) => {
                    pending.recv_ty = rewrite_types(self.db, pending.recv_ty, replacements);
                    for candidate in &mut pending.candidates {
                        candidate.inst = rewrite_types(self.db, candidate.inst, replacements);
                    }
                }
                DeferredTask::Cast(pending) => {
                    pending.target = rewrite_types(self.db, pending.target, replacements);
                }
                DeferredTask::ForLoopSeq(pending) => {
                    pending.iterable_ty = rewrite_types(self.db, pending.iterable_ty, replacements);
                    pending.elem_ty = rewrite_types(self.db, pending.elem_ty, replacements);
                }
                DeferredTask::MethodLookup(_)
                | DeferredTask::CallableLookup(_)
                | DeferredTask::PrimitiveOp(_)
                | DeferredTask::Field(_) => {}
            }
        }

        for block in &mut self.var_env {
            for binding in block.vars.values_mut() {
                *binding = rewrite_types(self.db, *binding, replacements);
            }
        }
        for binding in self.pending_vars.values_mut() {
            *binding = rewrite_types(self.db, *binding, replacements);
        }
        self.binding_block_idx = std::mem::take(&mut self.binding_block_idx)
            .into_iter()
            .map(|(binding, idx)| (rewrite_types(self.db, binding, replacements), idx))
            .collect();
        self.binding_closure_depth = std::mem::take(&mut self.binding_closure_depth)
            .into_iter()
            .map(|(binding, depth)| (rewrite_types(self.db, binding, replacements), depth))
            .collect();
        for closure in &mut self.closure_stack {
            for binding in &mut closure.params {
                *binding = rewrite_types(self.db, *binding, replacements);
            }
            closure.captures = std::mem::take(&mut closure.captures)
                .into_iter()
                .map(|(binding, mut capture)| {
                    let binding = rewrite_types(self.db, binding, replacements);
                    capture.binding = binding;
                    capture.ty = rewrite_types(self.db, capture.ty, replacements);
                    (binding, capture)
                })
                .collect();
        }

        self.base_assumptions = rewrite_types(self.db, self.base_assumptions, replacements);
        self.assumptions = rewrite_types(self.db, self.assumptions, replacements);
        for bound in &mut self.effect_bounds {
            *bound = rewrite_types(self.db, *bound, replacements);
        }
        self.effect_env.rewrite_types(self.db, replacements);
        self.expr_effect_env
            .values_mut()
            .flatten()
            .for_each(|effect_env| effect_env.rewrite_types(self.db, replacements));
        self.pattern_store = rewrite_types(self.db, self.pattern_store.clone(), replacements);
    }

    pub(super) fn snapshot_closure_replay_state(&self) -> ClosureReplayEnvSnapshot<'db> {
        ClosureReplayEnvSnapshot {
            pat_ty: self.pat_ty.clone(),
            expr_ty: self.expr_ty.clone(),
            expr_normal_completion: self.expr_normal_completion.clone(),
            expr_normal_bool_value: self.expr_normal_bool_value.clone(),
            cond_normal_bool_value: self.cond_normal_bool_value.clone(),
            assignment_rebinds_capability: self.assignment_rebinds_capability.clone(),
            contextual_view_sources: self.contextual_view_sources.clone(),
            const_refs: self.const_refs.clone(),
            value_path_refs: self.value_path_refs.clone(),
            callables: self.callables.clone(),
            semantic_expr_lowering: self.semantic_expr_lowering.clone(),
            record_init_lowering: self.record_init_lowering.clone(),
            closure_infos: self.closure_infos.clone(),
            resolved_field_index: self.resolved_field_index.clone(),
            deferred_closure_replay_contexts: self.deferred_closure_replay_contexts.clone(),
            deferred: self.deferred.clone(),
            effect_env: self.effect_env.clone(),
            effect_bounds: self.effect_bounds.clone(),
            base_assumptions: self.base_assumptions,
            assumptions: self.assumptions,
            var_env: self.var_env.clone(),
            binding_block_idx: self.binding_block_idx.clone(),
            binding_closure_depth: self.binding_closure_depth.clone(),
            contextual_closure_binding_origins: self.contextual_closure_binding_origins.clone(),
            closure_stack: self.closure_stack.clone(),
            pending_vars: self.pending_vars.clone(),
            loop_stack: self.loop_stack.clone(),
            expr_stack: self.expr_stack.clone(),
            expr_closure_ancestry: self.expr_closure_ancestry.clone(),
            expr_effect_env: self.expr_effect_env.clone(),
            first_return_borrow_provider: self.first_return_borrow_provider.clone(),
            param_bindings: self.param_bindings.clone(),
            pat_bindings: self.pat_bindings.clone(),
            local_borrow_providers: self.local_borrow_providers.clone(),
            pat_binding_modes: self.pat_binding_modes.clone(),
            pattern_store: self.pattern_store.clone(),
            pattern_status: self.pattern_status.clone(),
            call_effect_args: self.call_effect_args.clone(),
            late_effect_capture_contributions: self.late_effect_capture_contributions.clone(),
            closure_ty_replacements: self.closure_ty_replacements.clone(),
            for_loop_seq: self.for_loop_seq.clone(),
        }
    }

    pub(super) fn restore_closure_replay_state(&mut self, snapshot: ClosureReplayEnvSnapshot<'db>) {
        self.pat_ty = snapshot.pat_ty;
        self.expr_ty = snapshot.expr_ty;
        self.expr_normal_completion = snapshot.expr_normal_completion;
        self.expr_normal_bool_value = snapshot.expr_normal_bool_value;
        self.cond_normal_bool_value = snapshot.cond_normal_bool_value;
        self.assignment_rebinds_capability = snapshot.assignment_rebinds_capability;
        self.contextual_view_sources = snapshot.contextual_view_sources;
        self.const_refs = snapshot.const_refs;
        self.value_path_refs = snapshot.value_path_refs;
        self.callables = snapshot.callables;
        self.semantic_expr_lowering = snapshot.semantic_expr_lowering;
        self.record_init_lowering = snapshot.record_init_lowering;
        self.closure_infos = snapshot.closure_infos;
        self.resolved_field_index = snapshot.resolved_field_index;
        self.deferred_closure_replay_contexts = snapshot.deferred_closure_replay_contexts;
        self.deferred = snapshot.deferred;
        self.effect_env = snapshot.effect_env;
        self.effect_bounds = snapshot.effect_bounds;
        self.base_assumptions = snapshot.base_assumptions;
        self.assumptions = snapshot.assumptions;
        self.var_env = snapshot.var_env;
        self.binding_block_idx = snapshot.binding_block_idx;
        self.binding_closure_depth = snapshot.binding_closure_depth;
        self.contextual_closure_binding_origins = snapshot.contextual_closure_binding_origins;
        self.closure_stack = snapshot.closure_stack;
        self.pending_vars = snapshot.pending_vars;
        self.loop_stack = snapshot.loop_stack;
        self.expr_stack = snapshot.expr_stack;
        self.expr_closure_ancestry = snapshot.expr_closure_ancestry;
        self.expr_effect_env = snapshot.expr_effect_env;
        self.first_return_borrow_provider = snapshot.first_return_borrow_provider;
        self.param_bindings = snapshot.param_bindings;
        self.pat_bindings = snapshot.pat_bindings;
        self.local_borrow_providers = snapshot.local_borrow_providers;
        self.pat_binding_modes = snapshot.pat_binding_modes;
        self.pattern_store = snapshot.pattern_store;
        self.pattern_status = snapshot.pattern_status;
        self.call_effect_args = snapshot.call_effect_args;
        self.late_effect_capture_contributions = snapshot.late_effect_capture_contributions;
        self.closure_ty_replacements = snapshot.closure_ty_replacements;
        self.for_loop_seq = snapshot.for_loop_seq;
    }

    pub(super) fn enter_scope(&mut self, block: ExprId) {
        let new_scope = match block.data(self.db, self.body) {
            Partial::Present(Expr::Block(_)) => ScopeId::Block(self.body, block),
            _ => self.scope(),
        };

        let var_env = BlockEnv::new(new_scope, self.var_env.len());
        self.var_env.push(var_env);
    }

    pub(super) fn enter_lexical_scope(&mut self) {
        let var_env = BlockEnv::new(self.scope(), self.var_env.len());
        self.var_env.push(var_env);
    }

    pub(super) fn take_body_ctx(&mut self) -> BodyCtxSnapshot<'db> {
        BodyCtxSnapshot {
            loop_stack: std::mem::take(&mut self.loop_stack),
            first_return_borrow_provider: self.first_return_borrow_provider.take(),
        }
    }

    pub(super) fn restore_body_ctx(&mut self, snapshot: BodyCtxSnapshot<'db>) {
        self.loop_stack = snapshot.loop_stack;
        self.first_return_borrow_provider = snapshot.first_return_borrow_provider;
    }

    pub(super) fn enter_closure(&mut self, def: ClosureDef<'db>) {
        self.closure_stack.push(ActiveClosure {
            def,
            boundary_block_idx: self.current_block_idx(),
            params: Vec::new(),
            return_exprs: Vec::new(),
            captures: IndexMap::new(),
        });
    }

    pub(super) fn register_closure_param(
        &mut self,
        name: Option<IdentId<'db>>,
        binding: LocalBinding<'db>,
    ) {
        if let Some(active) = self.closure_stack.last_mut() {
            active.params.push(binding);
        }
        if let Some(name) = name {
            self.register_var_in_current_scope(name, binding);
        } else {
            self.binding_block_idx
                .insert(binding, self.current_block_idx());
            self.binding_closure_depth
                .insert(binding, self.closure_stack.len());
        }
    }

    pub(super) fn leave_closure(
        &mut self,
    ) -> (
        Vec<LocalBinding<'db>>,
        Vec<ExprId>,
        Vec<PendingClosureCapture<'db>>,
    ) {
        let active = self
            .closure_stack
            .pop()
            .expect("closure stack is non-empty");
        debug_assert_eq!(active.def.body, self.body);
        (
            active.params,
            active.return_exprs,
            active
                .captures
                .into_iter()
                .map(|(_, capture)| capture)
                .collect(),
        )
    }

    pub(super) fn leave_scope(&mut self) {
        self.var_env.pop().unwrap();
    }

    pub(super) fn enter_loop(&mut self, stmt: StmtId) {
        self.loop_stack.push(stmt);
    }

    pub(super) fn leave_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub(super) fn current_loop(&self) -> Option<StmtId> {
        self.loop_stack.last().copied()
    }

    pub(super) fn enter_expr(&mut self, expr: ExprId) {
        if self.expr_closure_ancestry[expr].is_none() {
            self.expr_closure_ancestry[expr] = Some(
                self.closure_stack
                    .iter()
                    .map(|closure| closure.def)
                    .collect(),
            );
        }
        self.expr_stack.push(expr);
    }

    pub(super) fn expr_closure_ancestry(&self, expr: ExprId) -> &[ClosureDef<'db>] {
        self.expr_closure_ancestry[expr]
            .as_deref()
            .unwrap_or_default()
    }

    pub(super) fn expr_closure_depth(&self, expr: ExprId) -> usize {
        self.expr_closure_ancestry(expr).len()
    }

    pub(super) fn expr_effect_env(
        &self,
        expr: ExprId,
    ) -> Option<&keyed_effect_env::EffectEnv<'db>> {
        self.expr_effect_env[expr].as_ref()
    }

    pub(super) fn record_expr_effect_env(&mut self, expr: ExprId) {
        if self.expr_effect_env[expr].is_none() {
            self.expr_effect_env[expr] = Some(self.effect_env.clone());
        }
    }

    pub(super) fn leave_expr(&mut self) {
        self.expr_stack.pop();
    }

    pub(super) fn parent_expr(&self) -> Option<ExprId> {
        self.expr_stack.iter().nth_back(1).copied()
    }

    pub(super) fn type_expr(&mut self, expr: ExprId, mut typed: ExprProp<'db>) {
        if let Some(previous) = self.expr_ty[expr].as_ref()
            && typed.value_access == ValueAccess::Infer
        {
            typed.value_access = previous.value_access;
        }
        self.expr_ty[expr] = Some(typed);
    }

    pub(super) fn register_contextual_view_source(&mut self, expr: ExprId, source_ty: TyId<'db>) {
        self.contextual_view_sources[expr] = Some(source_ty);
    }

    pub(super) fn contextual_view_source(&self, expr: ExprId) -> Option<TyId<'db>> {
        self.contextual_view_sources[expr]
    }

    pub(super) fn binding_is_capture(&self, binding: LocalBinding<'db>) -> bool {
        let Some(active) = self.closure_stack.last() else {
            return false;
        };
        self.binding_block_idx
            .get(&binding)
            .is_some_and(|&idx| idx <= active.boundary_block_idx)
    }

    pub(super) fn closure_depth(&self) -> usize {
        self.closure_stack.len()
    }

    pub(super) fn binding_closure_depth(&self, binding: LocalBinding<'db>) -> Option<usize> {
        self.binding_closure_depth.get(&binding).copied()
    }

    pub(super) fn active_closure(&self) -> Option<ClosureDef<'db>> {
        self.closure_stack.last().map(|active| active.def)
    }

    pub(super) fn record_capture_if_needed(&mut self, binding: LocalBinding<'db>, ty: TyId<'db>) {
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
                        crate::analysis::ty::ty_def::CapabilityKind::View => 0,
                        crate::analysis::ty::ty_def::CapabilityKind::Ref => 1,
                        crate::analysis::ty::ty_def::CapabilityKind::Mut => 2,
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

        let Some(binding_block_idx) = self.binding_block_idx.get(&binding).copied() else {
            return;
        };
        for active in self.closure_stack.iter_mut().rev() {
            if binding_block_idx <= active.boundary_block_idx {
                let capture = active
                    .captures
                    .entry(binding)
                    .or_insert(PendingClosureCapture {
                        binding,
                        ty,
                        access_without_return: ClosureCaptureAccess::Read,
                        access: ClosureCaptureAccess::Read,
                        expr_accesses: IndexMap::new(),
                    });
                capture.ty = merge_capture_ty(self.db, capture.ty, ty);
            }
        }
    }

    pub(super) fn record_capture_access(
        &mut self,
        binding: LocalBinding<'db>,
        access: ClosureCaptureAccess,
    ) {
        let Some(binding_block_idx) = self.binding_block_idx.get(&binding).copied() else {
            return;
        };
        for active in self.closure_stack.iter_mut().rev() {
            if binding_block_idx <= active.boundary_block_idx
                && let Some(capture) = active.captures.get_mut(&binding)
            {
                capture.access_without_return.include(access);
                capture.access.include(access);
            }
        }
    }

    /// Records a replaceable expression-local capture contribution.
    ///
    /// Only the innermost closure owns the expression's contextual result
    /// semantics. Enclosing closures observe the nested use as an ordinary
    /// access because rebuilding the inner closure can itself consume their
    /// capture.
    pub(super) fn record_capture_expr_access(
        &mut self,
        binding: LocalBinding<'db>,
        expr: ExprId,
        access: ClosureCaptureAccess,
    ) {
        let Some(binding_block_idx) = self.binding_block_idx.get(&binding).copied() else {
            return;
        };
        let innermost_idx = self.closure_stack.len().checked_sub(1);
        for (idx, active) in self.closure_stack.iter_mut().enumerate().rev() {
            if binding_block_idx <= active.boundary_block_idx
                && let Some(capture) = active.captures.get_mut(&binding)
            {
                if Some(idx) == innermost_idx {
                    capture
                        .expr_accesses
                        .entry(expr)
                        .or_insert(ClosureCaptureAccess::Read)
                        .include(access);
                } else {
                    capture.access_without_return.include(access);
                }
                capture.access.include(access);
            }
        }
    }

    /// Records an access contributed by a closure return consumer. It remains
    /// part of the closure's full access plan but is excluded from the
    /// return-independent baseline so deferred return coercion can replace it.
    ///
    /// Enclosing closures still see the access as ordinary: only the innermost
    /// active closure owns this return site.
    pub(super) fn record_return_capture_access(
        &mut self,
        binding: LocalBinding<'db>,
        expr: ExprId,
        access: ClosureCaptureAccess,
    ) {
        let Some(binding_block_idx) = self.binding_block_idx.get(&binding).copied() else {
            return;
        };
        let innermost_idx = self.closure_stack.len().checked_sub(1);
        for (idx, active) in self.closure_stack.iter_mut().enumerate().rev() {
            if binding_block_idx <= active.boundary_block_idx
                && let Some(capture) = active.captures.get_mut(&binding)
            {
                if Some(idx) != innermost_idx {
                    capture.access_without_return.include(access);
                } else {
                    capture
                        .expr_accesses
                        .entry(expr)
                        .or_insert(ClosureCaptureAccess::Read)
                        .include(access);
                }
                capture.access.include(access);
            }
        }
    }

    pub(super) fn record_active_closure_return_expr(&mut self, expr: ExprId) {
        if let Some(active) = self.closure_stack.last_mut()
            && !active.return_exprs.contains(&expr)
        {
            active.return_exprs.push(expr);
        }
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId<'db>) {
        self.pat_ty[pat] = Some(ty);
    }

    pub(super) fn set_expr_normal_completion(&mut self, expr: ExprId, normal: bool) {
        self.expr_normal_completion[expr] = Some(normal);
    }

    pub(super) fn set_expr_normal_bool_value(&mut self, expr: ExprId, value: Option<bool>) {
        self.expr_normal_bool_value[expr] = value;
    }

    pub(super) fn set_cond_normal_bool_value(&mut self, cond: CondId, value: Option<bool>) {
        self.cond_normal_bool_value[cond] = value;
    }

    pub(super) fn set_assignment_rebinds_capability(&mut self, expr: ExprId, rebinds: bool) {
        self.assignment_rebinds_capability[expr] = Some(rebinds);
    }

    pub(super) fn alloc_validated_pat(&mut self, pat: ValidatedPat<'db>) -> ValidatedPatId {
        self.pattern_store.alloc(pat)
    }

    pub(super) fn set_pattern_status(&mut self, pat: PatId, status: PatternAnalysisStatus) {
        match status {
            PatternAnalysisStatus::Ready(root) => self.pattern_store.set_root(pat, root),
            PatternAnalysisStatus::Invalid | PatternAnalysisStatus::Unsupported => {
                self.pattern_store.clear_root(pat)
            }
        }
        self.pattern_status[pat] = status;
    }

    /// Registers a new pending binding.
    ///
    /// This function adds a binding to the list of pending variables. If a
    /// binding with the same name already exists, it returns the existing
    /// binding. Otherwise, it returns `None`.
    ///
    /// To flush pending bindings to the designated scope, call
    /// [`flush_pending_bindings`] in the scope.
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier of the variable.
    /// * `binding` - The local binding to be registered.
    ///
    /// # Returns
    ///
    /// * `Some(LocalBinding)` if a binding with the same name already exists.
    /// * `None` if the binding was successfully registered.
    pub(super) fn register_pending_binding(
        &mut self,
        name: IdentId<'db>,
        binding: LocalBinding<'db>,
    ) -> Option<LocalBinding<'db>> {
        // Also store in pat_bindings for transfer to TypedBody
        if let LocalBinding::Local { pat, .. } = binding {
            self.pat_bindings[pat] = Some(binding);
            if self.pat_binding_modes[pat].is_none() {
                self.pat_binding_modes[pat] = Some(PatBindingMode::ByValue);
            }
        }
        self.pending_vars.insert(name, binding)
    }

    /// Flushes all pending variable bindings into the current variable
    /// environment.
    ///
    /// This function moves all pending bindings from the `pending_vars` map
    /// into the latest `BlockEnv` in `var_env`. After this operation, the
    /// `pending_vars` map will be empty.
    pub(super) fn flush_pending_bindings(&mut self) {
        let block_idx = self.current_block_idx();
        let var_env = self.var_env.last_mut().unwrap();
        for (name, binding) in self.pending_vars.drain() {
            var_env.register_var(name, binding);
            self.binding_block_idx.insert(binding, block_idx);
            self.binding_closure_depth
                .insert(binding, self.closure_stack.len());
        }
    }

    pub(super) fn clear_pending_bindings(&mut self) {
        self.pending_vars.clear();
    }

    pub(super) fn register_trait_obligation(&mut self, obligation: TraitObligation<'db>) {
        for task in &mut self.deferred {
            let DeferredTask::Obligation(existing) = task else {
                continue;
            };
            if existing.origin == obligation.origin {
                existing.goal = obligation.goal;
                existing.span = obligation.span;
                return;
            }
        }
        self.deferred
            .push_back(DeferredTask::Obligation(obligation))
    }

    pub(super) fn replace_generic_confirmation_obligation(
        &mut self,
        expr: ExprId,
        goal: TraitInstId<'db>,
    ) -> bool {
        let mut replaced = false;
        for task in &mut self.deferred {
            let DeferredTask::Obligation(obligation) = task else {
                continue;
            };
            if matches!(
                obligation.origin,
                TraitObligationOrigin::GenericConfirmation {
                    expr: obligation_expr
                } if obligation_expr == expr
            ) {
                obligation.goal = goal;
                replaced = true;
            }
        }
        replaced
    }

    pub(super) fn replace_call_constraint_obligations(
        &mut self,
        call_expr: ExprId,
        callable_def: CallableDef<'db>,
        goals: &FxHashMap<usize, TraitInstId<'db>>,
    ) {
        for task in &mut self.deferred {
            let DeferredTask::Obligation(obligation) = task else {
                continue;
            };
            let TraitObligationOrigin::CallConstraint {
                call_expr: obligation_expr,
                callable_def: obligation_def,
                constraint_idx,
            } = obligation.origin
            else {
                continue;
            };
            if obligation_expr == call_expr
                && obligation_def == callable_def
                && let Some(&goal) = goals.get(&constraint_idx)
            {
                obligation.goal = goal;
            }
        }
    }

    pub(super) fn deferred_len(&self) -> usize {
        self.deferred.len()
    }

    pub(super) fn truncate_deferred_tasks(&mut self, len: usize) {
        self.deferred.truncate(len);
    }

    pub(super) fn register_pending_method(&mut self, pending: PendingMethod<'db>) {
        self.record_expr_effect_env(pending.expr);
        self.deferred.push_back(DeferredTask::Method(pending))
    }

    pub(super) fn register_pending_method_lookup(&mut self, pending: PendingMethodLookup<'db>) {
        self.record_expr_effect_env(pending.expr);
        self.deferred.push_back(DeferredTask::MethodLookup(pending))
    }

    pub(super) fn register_pending_callable_lookup(&mut self, pending: PendingCallableLookup) {
        self.record_expr_effect_env(pending.expr);
        self.deferred
            .push_back(DeferredTask::CallableLookup(pending))
    }

    pub(super) fn register_pending_primitive_op(&mut self, pending: PendingPrimitiveOp) {
        self.deferred.push_back(DeferredTask::PrimitiveOp(pending))
    }

    pub(super) fn register_pending_field(&mut self, pending: PendingField<'db>) {
        self.deferred.push_back(DeferredTask::Field(pending))
    }

    pub(super) fn register_pending_cast(&mut self, pending: PendingCast<'db>) {
        self.deferred.push_back(DeferredTask::Cast(pending))
    }

    pub(super) fn register_pending_for_loop_seq(&mut self, pending: PendingForLoopSeq<'db>) {
        self.deferred.push_back(DeferredTask::ForLoopSeq(pending))
    }

    pub(super) fn set_expr_value_access(&mut self, expr: ExprId, access: ValueAccess) {
        let Some(prop) = self.expr_ty[expr].as_mut() else {
            panic!("expression must be typed before assigning value access: {expr:?}");
        };
        prop.value_access = match (prop.value_access, access) {
            (ValueAccess::Infer, access) | (access, ValueAccess::Infer) => access,
            (ValueAccess::Move, _) | (_, ValueAccess::Move) => ValueAccess::Move,
            (ValueAccess::MoveIfNonCopy, _) | (_, ValueAccess::MoveIfNonCopy) => {
                ValueAccess::MoveIfNonCopy
            }
            (ValueAccess::Read, ValueAccess::Read) => ValueAccess::Read,
        };
    }

    pub(super) fn replace_expr_value_access(&mut self, expr: ExprId, access: ValueAccess) {
        let Some(prop) = self.expr_ty[expr].as_mut() else {
            panic!("expression must be typed before replacing value access: {expr:?}");
        };
        prop.value_access = access;
    }

    /// Completes the type checking environment by finalizing pending trait
    /// obligations and folding types with the unification table.
    ///
    /// # Arguments
    ///
    /// * `table` - A mutable reference to the unification table used for type
    ///   unification.
    ///
    /// # Returns
    ///
    /// * A tuple containing the `TypedBody` and a vector of `FuncBodyDiag`.
    ///
    /// The `TypedBody` includes the body of the function, pattern types,
    /// expression types, and callables, all of which have been folded with
    /// the unification table.
    ///
    pub(super) fn finish(mut self, table: &mut UnificationTable<'db>) -> TypedBody<'db> {
        let raw_closure_ty_replacements = std::mem::take(&mut self.closure_ty_replacements);
        let mut closure_ty_replacements = FxHashMap::default();
        if !raw_closure_ty_replacements.is_empty() {
            let mut replacement_prober = Prober {
                table,
                scope: self.scope(),
            };
            for (old, new) in raw_closure_ty_replacements {
                let old = old.fold_with(self.db, &mut replacement_prober);
                let new = new.fold_with(self.db, &mut replacement_prober);
                closure_ty_replacements.insert(old, new);
            }
        }
        let mut prober = Prober {
            table,
            scope: self.scope(),
        };

        self.expr_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.clone().fold_with(self.db, &mut prober));
        self.contextual_view_sources
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(self.db, &mut prober));

        self.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(self.db, &mut prober));

        self.const_refs
            .values_mut()
            .flatten()
            .for_each(|cref| *cref = (*cref).fold_with(self.db, &mut prober));

        self.call_effect_args
            .values_mut()
            .flatten()
            .for_each(|args| {
                for arg in args {
                    arg.instantiated_key_ty = arg
                        .instantiated_key_ty
                        .map(|ty| ty.fold_with(self.db, &mut prober));
                    arg.provider_target_ty = arg
                        .provider_target_ty
                        .map(|ty| ty.fold_with(self.db, &mut prober));
                }
            });
        let mut assumptions = self.assumptions.fold_with(self.db, &mut prober);
        let scope = self.scope();
        let mut pattern_store = self.pattern_store.fold_with(self.db, &mut prober);

        self.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(self.db, &mut prober));
        self.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(self.db, &mut prober));
        self.closure_infos
            .values_mut()
            .flatten()
            .for_each(|info| *info = info.clone().fold_with(self.db, &mut prober));

        self.for_loop_seq
            .values_mut()
            .flatten()
            .for_each(|seq| *seq = seq.clone().fold_with(self.db, &mut prober));

        if !closure_ty_replacements.is_empty() {
            use crate::analysis::ty::fold::rewrite_types;

            self.expr_ty.values_mut().flatten().for_each(|prop| {
                *prop = rewrite_types(self.db, prop.clone(), &closure_ty_replacements)
            });
            self.contextual_view_sources
                .values_mut()
                .flatten()
                .for_each(|ty| {
                    *ty = rewrite_types(self.db, *ty, &closure_ty_replacements);
                });
            self.pat_ty.values_mut().flatten().for_each(|ty| {
                *ty = rewrite_types(self.db, *ty, &closure_ty_replacements);
            });
            self.const_refs
                .values_mut()
                .flatten()
                .for_each(|const_ref| {
                    *const_ref = rewrite_types(self.db, *const_ref, &closure_ty_replacements);
                });
            self.value_path_refs
                .values_mut()
                .flatten()
                .for_each(|value_ref| {
                    *value_ref = rewrite_types(self.db, *value_ref, &closure_ty_replacements);
                });
            self.call_effect_args
                .values_mut()
                .flatten()
                .for_each(|args| {
                    *args = rewrite_types(self.db, args.clone(), &closure_ty_replacements);
                });
            self.semantic_expr_lowering
                .values_mut()
                .flatten()
                .for_each(|lowering| {
                    *lowering = rewrite_types(self.db, lowering.clone(), &closure_ty_replacements);
                });
            self.record_init_lowering
                .values_mut()
                .flatten()
                .for_each(|lowering| {
                    *lowering = rewrite_types(self.db, *lowering, &closure_ty_replacements);
                });
            self.closure_infos.values_mut().flatten().for_each(|info| {
                *info = rewrite_types(self.db, info.clone(), &closure_ty_replacements);
            });
            self.for_loop_seq.values_mut().flatten().for_each(|seq| {
                *seq = rewrite_types(self.db, seq.clone(), &closure_ty_replacements);
            });
            self.param_bindings.iter_mut().for_each(|binding| {
                *binding = rewrite_types(self.db, *binding, &closure_ty_replacements);
            });
            self.pat_bindings
                .values_mut()
                .flatten()
                .for_each(|binding| {
                    *binding = rewrite_types(self.db, *binding, &closure_ty_replacements);
                });
            assumptions = rewrite_types(self.db, assumptions, &closure_ty_replacements);
            pattern_store = rewrite_types(self.db, pattern_store, &closure_ty_replacements);
        }

        let db = self.db;
        self.expr_ty.values_mut().flatten().for_each(|prop| {
            if prop.value_access == ValueAccess::MoveIfNonCopy {
                prop.value_access =
                    if crate::analysis::ty::ty_is_copy(db, scope, prop.ty, assumptions) {
                        ValueAccess::Read
                    } else {
                        ValueAccess::Move
                    };
            }
        });

        let mut expr_place = SecondaryMap::new();
        let mut expr_places: PrimaryMap<super::ExprPlaceId, Place<'db>> = PrimaryMap::new();
        for expr in self.body.exprs(self.db).keys() {
            if let Some(place) = Place::from_expr_in_body(
                self.db,
                self.body,
                expr,
                |expr| self.expr_ty[expr].as_ref().and_then(|prop| prop.binding),
                |expr| {
                    self.expr_ty[expr].as_ref().map_or_else(
                        || TyId::invalid(self.db, InvalidCause::Other),
                        |prop| prop.ty,
                    )
                },
            ) {
                let place_id = expr_places.push(place);
                expr_place[expr] = place_id.into();
            }
        }
        let result_ty = self.expr_ty[self.body.expr(self.db)].as_ref().map_or_else(
            || TyId::invalid(self.db, InvalidCause::Other),
            |prop| prop.ty,
        );

        TypedBody {
            body: Some(self.body),
            result_ty,
            assumptions,
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            expr_normal_completion: self.expr_normal_completion,
            expr_normal_bool_value: self.expr_normal_bool_value,
            cond_normal_bool_value: self.cond_normal_bool_value,
            assignment_rebinds_capability: self.assignment_rebinds_capability,
            contextual_view_sources: self.contextual_view_sources,
            const_refs: self.const_refs,
            value_path_refs: self.value_path_refs,
            semantic_expr_lowering: self.semantic_expr_lowering,
            record_init_lowering: self.record_init_lowering,
            closure_infos: self.closure_infos,
            resolved_field_index: self.resolved_field_index,
            call_effect_args: self.call_effect_args,
            return_borrow_provider: None,
            param_bindings: self.param_bindings,
            pat_bindings: self.pat_bindings,
            pat_binding_modes: self.pat_binding_modes,
            pattern_store,
            pattern_status: self.pattern_status,
            for_loop_seq: self.for_loop_seq,
            expr_place,
            expr_places,
        }
    }

    pub(super) fn expr_data(&self, expr: ExprId) -> &'db Partial<Expr<'db>> {
        expr.data(self.db, self.body)
    }

    pub(super) fn stmt_data(&self, stmt: StmtId) -> &'db Partial<Stmt<'db>> {
        stmt.data(self.db, self.body)
    }

    pub(crate) fn scope(&self) -> ScopeId<'db> {
        self.var_env.last().unwrap().scope
    }

    pub(super) fn current_block_idx(&self) -> usize {
        self.var_env.last().unwrap().idx
    }

    pub(super) fn get_block(&self, idx: usize) -> &BlockEnv<'db> {
        &self.var_env[idx]
    }

    pub(super) fn pop_deferred_task(&mut self) -> Option<DeferredTask<'db>> {
        self.deferred.pop_front()
    }
}

impl<'db> TyChecker<'db> {
    pub(super) fn seed_effect_witnesses(&mut self) {
        match self.env.owner {
            BodyOwner::Func(func) => self.seed_func_effect_witnesses(func),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                self.seed_contract_effect_witnesses();
            }
        }
    }

    fn seed_func_effect_witnesses(&mut self, func: Func<'db>) {
        let assumptions = self.env.base_assumptions();

        for binding in func.effect_requirements(self.db) {
            if !matches!(
                binding.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            ) {
                continue;
            }

            let idx = binding.binding_idx as usize;
            let resolved_binding = self
                .env
                .resolved_effect_binding(EffectParamSite::Func(func), idx)
                .unwrap_or_else(|| panic!("missing provider binding for effect at index {idx}"));
            let local_binding = LocalBinding::effect_param(&resolved_binding);
            let closure_depth = self.env.closure_depth();
            let provided = ProvidedEffect {
                origin: EffectOrigin::Param {
                    site: EffectParamSite::Func(func),
                    index: idx,
                    name: Some(resolved_binding.requirement.binding_name),
                },
                closure_depth,
                source_closure_depth: self
                    .env
                    .binding_closure_depth(local_binding)
                    .unwrap_or(closure_depth),
                ty: EffectEnvView::new(EffectParamSite::Func(func))
                    .visible_effect_binding_ty(self.db, idx)
                    .unwrap_or_else(|| self.env.lookup_binding_ty(&local_binding)),
                is_mut: local_binding.is_mut(),
                binding: Some(local_binding),
            };

            if let Some(req) = EffectRequirementDecl::from_effect_requirement(self.db, binding)
                && let Some(forwarder) =
                    seed_forwarder_from_requirement(self, &req, provided, func.scope(), assumptions)
            {
                self.env
                    .effect_env_mut()
                    .insert_forwarder(self.db, forwarder);
            }
        }
    }

    fn seed_contract_effect_witnesses(&mut self) {
        let Some((_contract, view)) = self.env.contract_effect_env_view() else {
            return;
        };

        let assumptions = self.env.base_assumptions();
        for binding in view.requirements(self.db) {
            let Some(req) = EffectRequirementDecl::from_effect_requirement(self.db, &binding)
            else {
                continue;
            };
            let Some(provider) = self.contract_effect_provider(&binding) else {
                continue;
            };
            self.seed_constrained_contract_requirement_witness(
                &req,
                provider,
                self.env.effect_binding_scope(binding.binding_site),
                assumptions,
            );
        }
    }

    fn contract_effect_provider(
        &self,
        binding: &EffectRequirement<'db>,
    ) -> Option<ProvidedEffect<'db>> {
        let idx = binding.binding_idx as usize;
        let origin = EffectOrigin::Param {
            site: binding.binding_site,
            index: idx,
            name: Some(binding.binding_name),
        };

        let resolved_binding = self
            .env
            .resolved_effect_binding(binding.binding_site, idx)?;
        let local_binding = LocalBinding::effect_param(&resolved_binding);
        let closure_depth = self.env.closure_depth();
        Some(ProvidedEffect {
            origin,
            closure_depth,
            source_closure_depth: self
                .env
                .binding_closure_depth(local_binding)
                .unwrap_or(closure_depth),
            ty: EffectEnvView::new(binding.binding_site)
                .visible_effect_binding_ty(self.db, idx)
                .unwrap_or_else(|| self.env.lookup_binding_ty(&local_binding)),
            is_mut: binding.is_mut,
            binding: Some(local_binding),
        })
    }

    fn seed_constrained_contract_requirement_witness(
        &mut self,
        req: &EffectRequirementDecl<'db>,
        provider: ProvidedEffect<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> bool {
        let snapshot = self.snapshot_state();
        let pattern = build_pattern_from_requirement_decl(self.db, req, scope, assumptions);
        let Some(key_path) = req.key_path else {
            self.rollback_state(snapshot);
            return false;
        };
        let span = match provider.origin {
            EffectOrigin::Param { site, index, .. } => effect_param_span(site, index),
            EffectOrigin::With { value_expr } => value_expr.span(self.body()).into(),
        };
        let Some((witness, commit)) = self
            .build_keyed_witness_from_pattern_in_scope(
                pattern,
                key_path,
                provider,
                span,
                super::expr::KeyedWitnessBuildOptions {
                    scope: super::expr::KeyedWitnessBuildScope { scope, assumptions },
                    emit_diag: false,
                    mode: super::expr::WitnessBuildMode::SeededRequirement,
                },
            )
            .ok()
        else {
            self.rollback_state(snapshot);
            return false;
        };
        if !self.apply_effect_commit_plan(commit) {
            self.rollback_state(snapshot);
            return false;
        }
        self.commit_state(snapshot);
        self.env.effect_env_mut().insert_witness(self.db, witness);
        true
    }
}

#[derive(Clone)]
pub(super) struct BlockEnv<'db> {
    pub(super) scope: ScopeId<'db>,
    pub(super) vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    idx: usize,
}

impl<'db> BlockEnv<'db> {
    pub(super) fn lookup_var(&self, var: IdentId<'db>) -> Option<LocalBinding<'db>> {
        self.vars.get(&var).cloned()
    }

    fn new(scope: ScopeId<'db>, idx: usize) -> Self {
        Self {
            scope,
            vars: FxHashMap::default(),
            idx,
        }
    }

    fn register_var(&mut self, name: IdentId<'db>, var: LocalBinding<'db>) {
        self.vars.insert(name, var);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectParamSite<'db> {
    Func(Func<'db>),
    Contract(Contract<'db>),
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ParamSite<'db> {
    Func(Func<'db>),
    ContractInit(Contract<'db>),
    Closure(ClosureDef<'db>),
    ClosureEnv(ClosureDef<'db>),
    ClosureArgs(ClosureDef<'db>),
    /// Effect param that resolves to a contract field.
    EffectField(EffectParamSite<'db>),
}

pub const CLOSURE_ARGS_PARAM_IDX: usize = 1;

fn param_span(site: ParamSite<'_>, idx: usize) -> DynLazySpan<'_> {
    match site {
        ParamSite::Func(func) => func.span().params().param(idx).name().into(),
        ParamSite::ContractInit(contract) => contract
            .span()
            .init_block()
            .params()
            .param(idx)
            .name()
            .into(),
        ParamSite::Closure(def) => def
            .expr
            .span(def.body)
            .into_closure_expr()
            .params()
            .param(idx)
            .name()
            .into(),
        ParamSite::ClosureEnv(def) => def.expr.span(def.body).into(),
        ParamSite::ClosureArgs(def) => def.expr.span(def.body).into(),
        ParamSite::EffectField(effect_site) => effect_param_span(effect_site, idx),
    }
}

fn param_name<'db>(
    db: &'db dyn HirAnalysisDb,
    site: ParamSite<'db>,
    idx: usize,
) -> Option<IdentId<'db>> {
    match site {
        ParamSite::Func(func) => func.params(db).nth(idx).and_then(|p| p.name(db)),
        ParamSite::ContractInit(contract) => contract
            .init(db)?
            .params(db)
            .data(db)
            .get(idx)
            .and_then(|p| p.name()),
        ParamSite::Closure(def) => {
            let Partial::Present(Expr::Closure { params, .. }) = def.expr.data(db, def.body) else {
                return None;
            };
            params.data(db).get(idx).and_then(|param| param.name())
        }
        ParamSite::ClosureEnv(_) => Some(IdentId::new(db, "%closure".to_string())),
        ParamSite::ClosureArgs(_) => Some(IdentId::new(db, "%args".to_string())),
        ParamSite::EffectField(effect_site) => effect_param_name(db, effect_site, idx),
    }
}

fn effect_param_name<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    idx: usize,
) -> Option<IdentId<'db>> {
    match site {
        EffectParamSite::Func(func) => func.effect_params(db).nth(idx).and_then(|p| p.name(db)),
        EffectParamSite::Contract(contract) => {
            contract.effects(db).data(db).get(idx).and_then(|p| p.name)
        }
        EffectParamSite::ContractInit { contract } => contract
            .init(db)?
            .effects(db)
            .data(db)
            .get(idx)
            .and_then(|p| p.name),
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => contract
            .recv_arm(db, recv_idx as usize, arm_idx as usize)?
            .effects
            .data(db)
            .get(idx)
            .and_then(|p| p.name),
    }
}

fn effect_param_span(site: EffectParamSite<'_>, idx: usize) -> DynLazySpan<'_> {
    match site {
        EffectParamSite::Func(func) => func.span().effects().param_idx(idx).name().into(),
        EffectParamSite::Contract(contract) => {
            contract.span().effects().param_idx(idx).name().into()
        }
        EffectParamSite::ContractInit { contract } => contract
            .span()
            .init_block()
            .effects()
            .param_idx(idx)
            .name()
            .into(),
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => contract
            .span()
            .recv(recv_idx as usize)
            .arms()
            .arm(arm_idx as usize)
            .effects()
            .param_idx(idx)
            .name()
            .into(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ProvidedEffect<'db> {
    pub origin: EffectOrigin<'db>,
    /// Closure depth of the lexical effect frame that introduced this
    /// provider.
    pub closure_depth: usize,
    /// Closure depth at which the provider's actual source binding was
    /// declared. This can be shallower than `closure_depth` for a `with`
    /// binding inside a closure that forwards an enclosing value.
    pub source_closure_depth: usize,
    pub ty: TyId<'db>,
    pub is_mut: bool,
    pub binding: Option<LocalBinding<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EffectOrigin<'db> {
    Param {
        site: EffectParamSite<'db>,
        index: usize,
        name: Option<IdentId<'db>>,
    },
    With {
        value_expr: ExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ExprProp<'db> {
    pub ty: TyId<'db>,
    pub is_mut: bool,
    pub binding: Option<LocalBinding<'db>>,
    pub borrow_provider: Option<ProviderAddressSpace>,
    pub path_read_semantics: Option<PathReadSemantics>,
    pub value_access: ValueAccess,
}

impl<'db> ExprProp<'db> {
    pub(super) fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
            borrow_provider: None,
            path_read_semantics: None,
            value_access: ValueAccess::Infer,
        }
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
            borrow_provider: None,
            path_read_semantics: None,
            value_access: ValueAccess::Infer,
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Update)]
pub enum ValueAccess {
    #[default]
    Infer,
    Read,
    MoveIfNonCopy,
    Move,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PathReadSemantics {
    ReuseLocal,
    ForwardInterface,
    MaterializeValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LocalBinding<'db> {
    Local {
        pat: PatId,
        is_mut: bool,
    },
    Param {
        site: ParamSite<'db>,
        idx: usize,
        mode: FuncParamMode,
        ty: TyId<'db>,
        is_mut: bool,
    },
    EffectParam {
        site: EffectParamSite<'db>,
        idx: usize,
        binding_name: IdentId<'db>,
        provider_idx: u32,
        key_path: PathId<'db>,
        is_mut: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PatBindingMode {
    ByValue,
    ByBorrow,
}

impl<'db> LocalBinding<'db> {
    pub(super) fn local(pat: PatId, is_mut: bool) -> Self {
        Self::Local { pat, is_mut }
    }

    pub fn closure_env(
        db: &'db dyn HirAnalysisDb,
        ty: ClosureTy<'db>,
        receiver_mode: ClosureReceiverMode,
    ) -> Self {
        let (mode, binding_ty, is_mut) = match receiver_mode {
            ClosureReceiverMode::View => (
                FuncParamMode::View,
                TyId::view_of(db, TyId::closure(db, ty)),
                false,
            ),
            ClosureReceiverMode::Own => (FuncParamMode::Own, TyId::closure(db, ty), true),
        };
        Self::Param {
            site: ParamSite::ClosureEnv(ty.def(db)),
            idx: 0,
            mode,
            ty: binding_ty,
            is_mut,
        }
    }

    pub fn closure_args(db: &'db dyn HirAnalysisDb, ty: ClosureTy<'db>) -> Self {
        Self::Param {
            site: ParamSite::ClosureArgs(ty.def(db)),
            idx: CLOSURE_ARGS_PARAM_IDX,
            mode: FuncParamMode::Own,
            ty: ty.args_pack_ty(db),
            is_mut: false,
        }
    }

    pub fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. }
            | LocalBinding::Param { is_mut, .. }
            | LocalBinding::EffectParam { is_mut, .. } => *is_mut,
        }
    }

    pub fn callable_input_origin(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<CallableInputLayoutHoleOrigin> {
        match self {
            Self::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            } => Some(if func.is_method(db) && idx == 0 {
                CallableInputLayoutHoleOrigin::Receiver
            } else {
                CallableInputLayoutHoleOrigin::ValueParam(idx)
            }),
            Self::Param {
                site: ParamSite::EffectField(_),
                idx,
                ..
            }
            | Self::EffectParam { idx, .. } => Some(CallableInputLayoutHoleOrigin::Effect(idx)),
            Self::Param {
                site: ParamSite::ClosureEnv(_),
                ..
            } => Some(CallableInputLayoutHoleOrigin::Receiver),
            Self::Param {
                site: ParamSite::ClosureArgs(_),
                ..
            } => Some(CallableInputLayoutHoleOrigin::ValueParam(1)),
            Self::Param {
                site: ParamSite::Closure(_),
                ..
            } => None,
            Self::Local { .. }
            | Self::Param {
                site: ParamSite::ContractInit(_),
                ..
            } => None,
        }
    }

    pub(crate) fn effect_param(binding: &ResolvedEffectBindingInfo<'db>) -> Self {
        Self::EffectParam {
            site: binding.requirement.binding_site,
            idx: binding.requirement.binding_idx as usize,
            binding_name: binding.requirement.binding_name,
            provider_idx: binding.provider.provider_idx,
            key_path: binding.requirement.binding_path,
            is_mut: binding.requirement.is_mut,
        }
    }

    pub(super) fn binding_name(&self, env: &TyCheckEnv<'db>) -> IdentId<'db> {
        match self {
            Self::Local { pat, .. } => {
                let hir_db = env.db;
                let Partial::Present(Pat::Path(Partial::Present(path), ..)) =
                    pat.data(hir_db, env.body())
                else {
                    unreachable!();
                };
                path.ident(hir_db).unwrap()
            }

            Self::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            } => env
                .semantic_effect_requirement(*effect_site, *idx)
                .map(|binding| binding.binding_name)
                .or_else(|| param_name(env.db, ParamSite::EffectField(*effect_site), *idx))
                .unwrap_or_else(|| IdentId::new(env.db, "_".to_string())),
            Self::Param { site, idx, .. } => param_name(env.db, *site, *idx)
                .unwrap_or_else(|| IdentId::new(env.db, "_".to_string())),
            Self::EffectParam { binding_name, .. } => *binding_name,
        }
    }

    pub(super) fn def_span(&self, env: &TyCheckEnv<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(env.body).into(),
            LocalBinding::Param { site, idx, .. } => param_span(*site, *idx),
            LocalBinding::EffectParam { site, idx, .. } => effect_param_span(*site, *idx),
        }
    }

    /// Get the definition span for this binding, given the body and function directly.
    ///
    /// This is used by `TypedBody::expr_binding_def_span` to get the definition
    /// span without needing a full `TyCheckEnv`.
    pub(super) fn def_span_with(&self, body: Body<'db>, _func: Func<'db>) -> DynLazySpan<'db> {
        self.def_span_in_body(body)
    }

    /// Get the definition span for this binding given just the body.
    pub(crate) fn def_span_in_body(&self, body: Body<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(body).into(),
            LocalBinding::Param { site, idx, .. } => param_span(*site, *idx),
            LocalBinding::EffectParam { site, idx, .. } => effect_param_span(*site, *idx),
        }
    }

    pub(crate) fn pretty_name_in_body(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
    ) -> String {
        match self {
            Self::Local { pat, .. } => {
                let Partial::Present(Pat::Path(Partial::Present(path), ..)) = pat.data(db, body)
                else {
                    return "_".to_string();
                };
                path.ident(db)
                    .to_opt()
                    .map(|ident| ident.data(db).to_string())
                    .unwrap_or_else(|| "_".to_string())
            }
            Self::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            } => effect_param_name(db, *effect_site, *idx)
                .or_else(|| param_name(db, ParamSite::EffectField(*effect_site), *idx))
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| format!("%param{idx}")),
            Self::Param { site, idx, .. } => param_name(db, *site, *idx)
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| format!("%param{idx}")),
            Self::EffectParam {
                binding_name, idx, ..
            } => Some(*binding_name)
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| format!("%effect{idx}")),
        }
    }
}

pub(super) struct Prober<'db, 'a> {
    table: &'a mut UnificationTable<'db>,
    scope: ScopeId<'db>,
}

impl<'db, 'a> Prober<'db, 'a> {
    pub(super) fn new(table: &'a mut UnificationTable<'db>, scope: ScopeId<'db>) -> Self {
        Self { table, scope }
    }
}

impl<'db> TyFolder<'db> for Prober<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let ty = self.table.fold_ty(db, ty);
        let TyData::TyVar(var) = ty.data(db) else {
            return ty.super_fold_with(db, self);
        };

        // String type variable fallback.
        if let TyVarSort::String { min_len, fallback } = var.sort {
            match fallback {
                StringFallback::Dynamic => {
                    resolve_lib_type_path(db, self.scope, "core::abi::DynString")
                        .unwrap_or_else(|| TyId::string_with_len(db, min_len))
                }
                StringFallback::Fixed => TyId::string_with_len(db, min_len),
            }
        } else {
            ty.super_fold_with(db, self)
        }
    }
}
#[derive(Debug, Clone)]
pub(super) struct PendingMethod<'db> {
    pub expr: crate::core::hir_def::ExprId,
    pub recv_ty: TyId<'db>,
    pub method_name: crate::core::hir_def::IdentId<'db>,
    pub candidates: Vec<PendingMethodCandidate<'db>>,
    pub span: DynLazySpan<'db>,
    pub callee_is_receiver: bool,
}

#[derive(Debug, Clone)]
pub(super) struct PendingMethodLookup<'db> {
    pub expr: ExprId,
    pub method_name: IdentId<'db>,
    pub span: DynLazySpan<'db>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PendingCallableLookup {
    pub expr: ExprId,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PendingMethodCandidate<'db> {
    pub inst: TraitInstId<'db>,
    pub method: Func<'db>,
    pub needs_confirmation: bool,
    /// Lower values are preferred after argument/result viability is known.
    /// Direct invocation uses this to prefer `Fn` over `FnOnce`; ordinary
    /// method lookup gives every candidate priority zero.
    pub priority: u8,
}

#[derive(Debug, Clone)]
pub(super) enum PendingPrimitiveOp {
    Unary {
        expr: ExprId,
        inner: ExprId,
        op: UnOp,
    },
    Binary {
        expr: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: BinOp,
    },
    AugAssign {
        expr: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: ArithBinOp,
    },
}

#[derive(Debug, Clone)]
pub(super) struct PendingField<'db> {
    pub expr: ExprId,
    pub lhs: ExprId,
    pub field: FieldIndex<'db>,
}

#[derive(Debug, Clone)]
pub(super) struct PendingCast<'db> {
    pub expr: ExprId,
    pub inner: ExprId,
    pub target: TyId<'db>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PendingForLoopSeq<'db> {
    pub stmt: StmtId,
    pub expr: ExprId,
    pub iterable_ty: TyId<'db>,
    pub elem_ty: TyId<'db>,
}

impl PendingPrimitiveOp {
    pub(super) fn expr(&self) -> ExprId {
        match self {
            Self::Unary { expr, .. } | Self::Binary { expr, .. } | Self::AugAssign { expr, .. } => {
                *expr
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeferredTask<'db> {
    Obligation(TraitObligation<'db>),
    Method(PendingMethod<'db>),
    MethodLookup(PendingMethodLookup<'db>),
    CallableLookup(PendingCallableLookup),
    PrimitiveOp(PendingPrimitiveOp),
    Field(PendingField<'db>),
    Cast(PendingCast<'db>),
    ForLoopSeq(PendingForLoopSeq<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TraitObligationOrigin<'db> {
    CallConstraint {
        call_expr: ExprId,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    },
    GenericConfirmation {
        expr: ExprId,
    },
}

#[derive(Debug, Clone)]
pub(super) struct TraitObligation<'db> {
    pub goal: TraitInstId<'db>,
    pub origin: TraitObligationOrigin<'db>,
    pub span: DynLazySpan<'db>,
}

impl<'db> TyCheckEnv<'db> {}
