use crate::{
    analysis::place::Place,
    hir_def::{
        BinOp, Body, Contract, Expr, ExprId, Func, IdentId, ItemKind, Partial, Pat, PatId, PathId,
        Stmt, StmtId, UnOp, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;
use cranelift_entity::{PrimaryMap, SecondaryMap};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use thin_vec::ThinVec;

use super::effect_env as keyed_effect_env;
use super::owner::BodyOwner;
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
        corelib::resolve_lib_type_path,
        effects::{
            EffectKeyKind,
            elaborate::{build_pattern_from_requirement_decl, seed_forwarder_from_requirement},
            model::EffectRequirementDecl,
        },
        fold::{TyFoldable, TyFolder},
        provider::ProviderAddressSpace,
        trait_def::TraitInstId,
        trait_resolution::{
            PredicateListId,
            constraint::{collect_constraints, collect_func_effect_provider_constraints},
        },
        ty_contains_const_hole,
        ty_def::{InvalidCause, StringFallback, TyData, TyId, TyVarSort},
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
    implicit_moves: FxHashSet<ExprId>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    callables: SecondaryMap<ExprId, Option<Callable<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<super::RecordInitLowering<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,

    deferred: Vec<DeferredTask<'db>>,

    effect_env: keyed_effect_env::EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    base_assumptions: PredicateListId<'db>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,

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

    /// Resolved Seq trait methods for for-loops, keyed by the for statement.
    for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        fn const_owner_preds<'db>(
            db: &'db dyn HirAnalysisDb,
            scope: ScopeId<'db>,
        ) -> PredicateListId<'db> {
            match scope.parent_item(db) {
                Some(ItemKind::Trait(trait_)) => PredicateListId::new(
                    db,
                    vec![crate::semantic::trait_self_predicate(db, trait_)],
                ),
                Some(ItemKind::ImplTrait(impl_trait)) => {
                    collect_constraints(db, impl_trait.into()).instantiate_identity()
                }
                Some(ItemKind::Impl(impl_)) => {
                    collect_constraints(db, impl_.into()).instantiate_identity()
                }
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
            implicit_moves: FxHashSet::default(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            callables: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            deferred: Vec::new(),
            effect_env: keyed_effect_env::EffectEnv::new(),
            effect_bounds: ThinVec::new(),
            base_assumptions,
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
            param_bindings: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            local_borrow_providers: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            call_effect_args: SecondaryMap::new(),
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
                        env.var_env.last_mut().unwrap().register_var(name, var);
                    };
                }
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
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
                        env.var_env.last_mut().unwrap().register_var(name, var);
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
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
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
            self.var_env
                .last_mut()
                .expect("function scope exists")
                .register_var(
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
            BodyOwner::Func(_) | BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => None,
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
            self.var_env.last_mut().expect("scope exists").register_var(
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

    pub(super) fn register_for_loop_seq(&mut self, stmt: StmtId, seq: ForLoopSeq<'db>) {
        if self.for_loop_seq[stmt].replace(seq).is_some() {
            panic!("for loop seq is already registered for the given stmt")
        }
    }

    pub(super) fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables[expr].as_ref()
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
        self.register_semantic_expr_lowering(expr, SemanticExprLowering::Call { callable });
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
        self.expr_stack.push(expr);
    }

    pub(super) fn leave_expr(&mut self) {
        self.expr_stack.pop();
    }

    pub(super) fn parent_expr(&self) -> Option<ExprId> {
        self.expr_stack.iter().nth_back(1).copied()
    }

    pub(super) fn type_expr(&mut self, expr: ExprId, typed: ExprProp<'db>) {
        self.expr_ty[expr] = Some(typed);
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId<'db>) {
        self.pat_ty[pat] = Some(ty);
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
        let var_env = self.var_env.last_mut().unwrap();
        for (name, binding) in self.pending_vars.drain() {
            var_env.register_var(name, binding);
        }
    }

    pub(super) fn clear_pending_bindings(&mut self) {
        self.pending_vars.clear();
    }

    pub(super) fn register_trait_obligation(&mut self, obligation: TraitObligation<'db>) {
        self.deferred.push(DeferredTask::Obligation(obligation))
    }

    pub(super) fn deferred_len(&self) -> usize {
        self.deferred.len()
    }

    pub(super) fn truncate_deferred_tasks(&mut self, len: usize) {
        self.deferred.truncate(len);
    }

    pub(super) fn register_pending_method(&mut self, pending: PendingMethod<'db>) {
        self.deferred.push(DeferredTask::Method(pending))
    }

    pub(super) fn register_pending_primitive_op(&mut self, pending: PendingPrimitiveOp) {
        self.deferred.push(DeferredTask::PrimitiveOp(pending))
    }

    pub(super) fn record_implicit_move(&mut self, expr: ExprId) {
        self.implicit_moves.insert(expr);
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
        let mut prober = Prober {
            table,
            scope: self.scope(),
        };

        self.expr_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.clone().fold_with(self.db, &mut prober));

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
        let assumptions = self.assumptions.fold_with(self.db, &mut prober);
        let pattern_store = self.pattern_store.fold_with(self.db, &mut prober);

        self.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(self.db, &mut prober));
        self.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(self.db, &mut prober));

        self.for_loop_seq
            .values_mut()
            .flatten()
            .for_each(|seq| *seq = seq.clone().fold_with(self.db, &mut prober));
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
            implicit_moves: self.implicit_moves,
            const_refs: self.const_refs,
            value_path_refs: self.value_path_refs,
            semantic_expr_lowering: self.semantic_expr_lowering,
            record_init_lowering: self.record_init_lowering,
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

    pub(super) fn take_deferred_tasks(&mut self) -> Vec<DeferredTask<'db>> {
        std::mem::take(&mut self.deferred)
    }
}

impl<'db> TyChecker<'db> {
    pub(super) fn seed_effect_witnesses(&mut self) {
        match self.env.owner {
            BodyOwner::Func(func) => self.seed_func_effect_witnesses(func),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
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
            let provided = ProvidedEffect {
                origin: EffectOrigin::Param {
                    site: EffectParamSite::Func(func),
                    index: idx,
                    name: Some(resolved_binding.requirement.binding_name),
                },
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
        Some(ProvidedEffect {
            origin,
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
    /// Effect param that resolves to a contract field.
    EffectField(EffectParamSite<'db>),
}

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
}

impl<'db> ExprProp<'db> {
    pub(super) fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
            borrow_provider: None,
            path_read_semantics: None,
        }
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
            borrow_provider: None,
            path_read_semantics: None,
        }
    }
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

    pub fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. }
            | LocalBinding::Param { is_mut, .. }
            | LocalBinding::EffectParam { is_mut, .. } => *is_mut,
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
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PendingMethodCandidate<'db> {
    pub inst: TraitInstId<'db>,
    pub method: Func<'db>,
    pub needs_confirmation: bool,
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
}

impl PendingPrimitiveOp {
    pub(super) fn expr(&self) -> ExprId {
        match self {
            Self::Unary { expr, .. } | Self::Binary { expr, .. } => *expr,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeferredTask<'db> {
    Obligation(TraitObligation<'db>),
    Method(PendingMethod<'db>),
    PrimitiveOp(PendingPrimitiveOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TraitObligationOrigin<'db> {
    CallConstraint {
        call_expr: ExprId,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    },
    GenericConfirmation,
}

#[derive(Debug, Clone)]
pub(super) struct TraitObligation<'db> {
    pub goal: TraitInstId<'db>,
    pub origin: TraitObligationOrigin<'db>,
    pub span: DynLazySpan<'db>,
}

impl<'db> TyCheckEnv<'db> {}
