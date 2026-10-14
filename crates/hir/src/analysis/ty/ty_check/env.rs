use crate::{
    analysis::place::{Place, is_pointer_place_expr},
    hir_def::{
        BinOp, Body, Contract, Expr, ExprId, Func, IdentId, Partial, Pat, PatId, Stmt, StmtId,
        UnOp, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;
use cranelift_entity::{PrimaryMap, SecondaryMap};
use rustc_hash::FxHashMap;
use salsa::Update;
use thin_vec::ThinVec;

use super::effect_env as keyed_effect_env;
use super::owner::BodyOwner;
use super::{
    Callable, ConstIntrinsicKind, ConstRef, SemanticExprLowering, TyChecker, TypedBody,
    TypedBodyTables, ValuePathRef, stmt::ForLoopSeq,
};
use crate::analysis::ty::pattern_ir::{
    PatternAnalysisStatus, PatternStore, ValidatedPat, ValidatedPatId,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::{CallableInputLayoutHoleOrigin, const_body_assumptions},
        corelib::resolve_lib_type_path,
        effects::{
            EffectKeyKind,
            elaborate::{build_pattern_from_requirement_decl, seed_forwarder_from_requirement},
            model::EffectRequirementDecl,
        },
        fold::{TyFoldable, TyFolder},
        normalize::normalize_ty,
        provider::ProviderAddressSpace,
        trait_def::TraitInstId,
        trait_resolution::{
            PredicateListId, TraitSolveCx, constraint::collect_func_effect_provider_constraints,
        },
        ty_contains_const_hole,
        ty_def::{InvalidCause, StringFallback, TyData, TyId, TyVarSort},
        ty_is_copy,
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
    /// Owned-context moves and the type each value is moved as. Whether that
    /// type is `Copy` is decided in `finish`, once inference has resolved it.
    implicit_moves: FxHashMap<ExprId, TyId<'db>>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    const_openings: SecondaryMap<ExprId, Vec<super::TraitOpening<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    callables: SecondaryMap<ExprId, Option<Callable<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<super::RecordInitLowering<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,

    deferred: Vec<DeferredTask<'db>>,

    effect_env: keyed_effect_env::EffectEnv<'db>,
    trait_openings: super::opening::TraitOpenings<'db>,
    with_openings: SecondaryMap<ExprId, Option<super::TraitOpening<'db>>>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    base_assumptions: PredicateListId<'db>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    binding_openings: FxHashMap<LocalBinding<'db>, Vec<super::TraitOpening<'db>>>,
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

    /// The constrained type applications that resolving the body's paths
    /// passed through, each with the span of the expression or pattern whose
    /// path it is.
    path_applications: Vec<(DynLazySpan<'db>, TyId<'db>)>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
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
            BodyOwner::AnonConstBody { .. } | BodyOwner::Const(_) => {
                let preds = const_body_assumptions(db, owner_scope);
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
            implicit_moves: FxHashMap::default(),
            const_refs: SecondaryMap::new(),
            const_openings: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            callables: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            deferred: Vec::new(),
            effect_env: keyed_effect_env::EffectEnv::new(),
            trait_openings: super::opening::TraitOpenings::new(),
            with_openings: SecondaryMap::new(),
            effect_bounds: ThinVec::new(),
            base_assumptions,
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            binding_openings: FxHashMap::default(),
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
            path_applications: Vec::new(),
        };

        env.enter_scope(body.expr(db));

        match owner {
            BodyOwner::Func(func) => {
                let arg_tys = func.arg_tys(db);
                for (idx, view) in func.params(db).enumerate() {
                    let mut ty = *arg_tys
                        .get(idx)
                        .map_or(&TyId::invalid(db, InvalidCause::ParseError), |b| {
                            b.skip_binder()
                        });

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
            |expr| self.typed_expr_ty(expr),
        )
    }

    /// Returns `true` if `expr` is an assignable, borrowable location: a
    /// binding-rooted place, or memory addressed by a pointer.
    pub(super) fn is_place_expr(&self, expr: ExprId) -> bool {
        self.expr_place(expr).is_some() || self.is_pointer_place_expr(expr)
    }

    pub(super) fn is_pointer_place_expr(&self, expr: ExprId) -> bool {
        is_pointer_place_expr(self.db, self.body, expr, &mut |expr| {
            self.typed_expr_ty(expr)
        })
    }

    fn typed_expr_ty(&self, expr: ExprId) -> TyId<'db> {
        self.typed_expr(expr).map_or_else(
            || TyId::invalid(self.db, InvalidCause::Other),
            |prop| prop.ty,
        )
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables[expr].replace(callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }

    pub(super) fn register_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) {
        if matches!(const_ref, ConstRef::TraitConst(_)) {
            self.const_openings[expr] = self.trait_openings.visible(self.db);
        }
        if self.const_refs[expr].replace(const_ref).is_some() {
            panic!("const ref is already registered for the given expr")
        }
    }

    pub(super) fn register_value_path_ref(&mut self, expr: ExprId, value_path: ValuePathRef<'db>) {
        if self.value_path_refs[expr].replace(value_path).is_some() {
            panic!("value path ref is already registered for the given expr")
        }
    }

    pub(super) fn register_path_applications(
        &mut self,
        site: DynLazySpan<'db>,
        applications: impl IntoIterator<Item = TyId<'db>>,
    ) {
        self.path_applications.extend(
            applications
                .into_iter()
                .map(|application| (site.clone(), application)),
        );
    }

    pub(super) fn register_for_loop_seq(&mut self, stmt: StmtId, seq: ForLoopSeq<'db>) {
        if self.for_loop_seq[stmt].replace(seq).is_some() {
            panic!("for loop seq is already registered for the given stmt")
        }
    }

    pub(super) fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables[expr].as_ref()
    }

    pub(super) fn expr_value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.value_path_refs[expr]
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

    pub(super) fn register_resolved_field_index(&mut self, expr: ExprId, field_index: u16) {
        if self.resolved_field_index[expr]
            .replace(field_index)
            .is_some()
        {
            panic!("resolved field index is already registered for the given expr")
        }
    }

    pub(super) fn register_semantic_call(&mut self, expr: ExprId, mut callable: Callable<'db>) {
        callable.record_source_expr(expr);
        callable.record_lexical_openings(self.trait_openings.visible(self.db));
        let operands = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(_, args)) => args.iter().map(|arg| arg.expr).collect(),
            Partial::Present(Expr::MethodCall(receiver, _, _, args)) => std::iter::once(*receiver)
                .chain(args.iter().map(|arg| arg.expr))
                .collect(),
            Partial::Present(Expr::Bin(lhs, rhs, _) | Expr::AugAssign(lhs, rhs, _)) => {
                vec![*lhs, *rhs]
            }
            Partial::Present(Expr::Un(value, _)) => vec![*value],
            _ => Vec::new(),
        };
        callable.record_argument_operands(operands);
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
        // Type lookup also reads the accepted output equalities of openings.
        let visible = self.trait_openings.visible(self.db);
        if visible.is_empty() {
            return self.assumptions;
        }
        let mut premises = self.assumptions.list(self.db).to_vec();
        premises.extend(visible.into_iter().map(|opening| opening.goal()));
        PredicateListId::new(self.db, premises).extend_all_bounds(self.db)
    }

    pub(super) fn enter_with_frame(&mut self) {
        self.effect_env.push_frame();
        self.trait_openings.enter();
    }
    pub(super) fn leave_with_frame(&mut self) {
        self.trait_openings.leave();
        self.effect_env.pop_frame();
    }
    pub(super) fn opening_for(&self, goal: TraitInstId<'db>) -> Option<super::TraitOpening<'db>> {
        self.trait_openings.lookup(self.db, goal)
    }
    pub(super) fn insert_opening(
        &mut self,
        value: ExprId,
        opening: super::TraitOpening<'db>,
    ) -> Result<(), ()> {
        self.trait_openings.insert(self.db, opening)?;
        self.with_openings[value] = Some(opening);
        Ok(())
    }

    /// Validate each opening against the completed frame, excluding its own
    /// premise. Other opened witnesses must also pass their entry validation.
    pub(super) fn finalize_opening_admission(&mut self, value: ExprId) {
        let Some(super::TraitOpening::Evidence { operand, goal, .. }) = self.with_openings[value]
        else {
            return;
        };
        let premises: Vec<_> = self
            .trait_openings
            .visible(self.db)
            .into_iter()
            .filter_map(|opening| match opening {
                super::TraitOpening::Evidence {
                    operand: other,
                    goal,
                    ..
                } if other != operand => Some(goal),
                _ => None,
            })
            .collect();
        let lexical = (!premises.is_empty())
            .then(|| PredicateListId::new(self.db, premises).extend_all_bounds(self.db));
        let mut assumptions = self.assumptions.list(self.db).to_vec();
        if let Some(lexical) = lexical {
            assumptions.extend(lexical.list(self.db).iter().copied());
        }
        let context = TraitSolveCx::new(self.db, self.scope())
            .with_assumptions(PredicateListId::new(self.db, assumptions).extend_all_bounds(self.db))
            .with_lexical_premises(lexical);
        self.with_openings[value] = Some(super::TraitOpening::Evidence {
            operand,
            goal,
            admission: Some(context),
        });
        self.trait_openings.finalize_evidence(operand, context);
    }

    /// Project the context owned by the checker at its current lexical position.
    /// Root/effect premises are formed once in this environment, not by readers.
    pub(crate) fn proof_context(&self) -> TraitSolveCx<'db> {
        // Named applications provide a choice, not an extra solver premise.
        // Their output equalities remain available to lexical type lookup.
        let visible = self.trait_openings.visible(self.db);
        // A named implementation body is checked under its declared prerequisites.
        // Admission of that implementation independently establishes these at use.
        // Keep ordinary declaration assumptions under master's existing policy.
        let declared = match self.owner {
            BodyOwner::Func(func) => func
                .containing_impl_trait(self.db)
                .filter(|implementation| implementation.selection_name(self.db).is_some())
                .map(|implementation| {
                    crate::analysis::ty::trait_resolution::constraint::collect_constraints(
                        self.db,
                        implementation.into(),
                    )
                    .instantiate_identity()
                }),
            _ => None,
        };
        if visible.is_empty() && declared.is_none() {
            return TraitSolveCx::new(self.db, self.scope()).with_assumptions(self.assumptions);
        }
        let mut lexical: Vec<_> = visible
            .into_iter()
            .filter_map(|opening| match opening {
                super::TraitOpening::Evidence { goal, .. } => Some(goal),
                super::TraitOpening::Named(_) => None,
            })
            .collect();
        if let Some(declared) = declared {
            lexical.extend(declared.list(self.db).iter().copied());
        }
        let lexical = (!lexical.is_empty())
            .then(|| PredicateListId::new(self.db, lexical).extend_all_bounds(self.db));
        let mut premises = self.assumptions.list(self.db).to_vec();
        if let Some(lexical) = lexical {
            premises.extend(lexical.list(self.db).iter().copied());
        }
        TraitSolveCx::new(self.db, self.scope())
            .with_assumptions(PredicateListId::new(self.db, premises).extend_all_bounds(self.db))
            .with_lexical_premises(lexical)
    }

    /// Read a record through the shared checked-operand interpretation.
    /// This is advisory type contextualization; unavailable values stay pending.
    fn known_operand_application(
        &self,
        operand: ExprId,
    ) -> Option<crate::analysis::ty::trait_def::ImplApplicationRecord<'db>> {
        use crate::analysis::semantic::{
            SemConstValue,
            operand_recovery::{KnownOperandRecovery, recover_known_operand},
        };
        let KnownOperandRecovery::Value(value) = recover_known_operand(self, operand) else {
            return None;
        };
        match value.value(self.db) {
            SemConstValue::Evidence { application, .. } => Some(*application),
            _ => None,
        }
    }

    /// Resolve a type-level constant through the admitted lexical choice.
    pub(super) fn contextualize_const_types(&self, ty: TyId<'db>) -> TyId<'db> {
        self.contextualize_const_types_at(ty, &self.trait_openings.visible(self.db), false)
    }

    pub(super) fn has_trait_openings(&self) -> bool {
        !self.trait_openings.visible(self.db).is_empty()
    }

    pub(super) fn binding_openings(
        &self,
        binding: LocalBinding<'db>,
    ) -> Vec<super::TraitOpening<'db>> {
        self.binding_openings
            .get(&binding)
            .cloned()
            .unwrap_or_default()
    }

    pub(super) fn contextualize_const_types_at(
        &self,
        ty: TyId<'db>,
        openings: &[super::TraitOpening<'db>],
        source: bool,
    ) -> TyId<'db> {
        struct Selected<'a, 'db>(&'a TyCheckEnv<'db>, &'a [super::TraitOpening<'db>], bool);
        impl<'db> TyFolder<'db> for Selected<'_, 'db> {
            fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                use crate::analysis::ty::{
                    const_expr::ConstExpr,
                    const_ty::{ConstTyData, const_ty_from_resolved_trait_const},
                };
                let ty = ty.super_fold_with(db, self);
                if let TyData::ConstTy(value) = ty.data(db)
                    && let ConstTyData::Abstract(expr, expected) = value.data(db)
                    && let ConstExpr::TraitConst(assoc) = expr.data(db)
                    && let Some(opening) = self.1.iter().copied().find(|opening| {
                        let goal = opening.goal();
                        goal.def(db) == assoc.inst().def(db)
                            && goal.args(db) == assoc.inst().args(db)
                    })
                    && let Some(record) = match opening {
                        super::TraitOpening::Named(record) => Some(record),
                        super::TraitOpening::Evidence { operand, .. } => {
                            self.0.known_operand_application(operand)
                        }
                    }
                {
                    return match record.revalidate(
                        db,
                        if self.2 {
                            record.origin_context()
                        } else {
                            match opening {
                                super::TraitOpening::Evidence {
                                    admission: Some(context),
                                    ..
                                } => context,
                                _ => self.0.proof_context(),
                            }
                        },
                        assoc.inst(),
                    ) {
                        Ok(application) => const_ty_from_resolved_trait_const(
                            db,
                            application.resolved(),
                            assoc.name(),
                        )
                        .map(|value| TyId::const_ty(db, value.evaluate(db, Some(*expected))))
                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
                        Err(_) => TyId::invalid(db, InvalidCause::Other),
                    };
                }
                ty
            }
        }
        ty.fold_with(self.db, &mut Selected(self, openings, source))
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
        self.binding_openings
            .insert(binding, self.trait_openings.visible(self.db));
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

    pub(super) fn record_implicit_move(&mut self, expr: ExprId, ty: TyId<'db>) {
        self.implicit_moves.insert(expr, ty);
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
        let scope = prober.scope;
        let implicit_moves = self
            .implicit_moves
            .iter()
            .filter_map(|(expr, ty)| {
                let ty = normalize_ty(
                    self.db,
                    ty.fold_with(self.db, &mut prober),
                    scope,
                    assumptions,
                );
                (!ty_is_copy(self.db, scope, ty, assumptions)).then_some(*expr)
            })
            .collect();

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
        self.path_applications
            .iter_mut()
            .for_each(|(_, ty)| *ty = ty.fold_with(self.db, &mut prober));
        self.with_openings
            .values_mut()
            .flatten()
            .for_each(|opening| *opening = opening.fold_with(self.db, &mut prober));
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

        TypedBodyTables {
            body: Some(self.body),
            with_openings: self.with_openings,
            result_ty,
            assumptions,
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            implicit_moves,
            const_refs: self.const_refs,
            const_openings: self.const_openings,
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
            path_applications: self.path_applications,
        }
        .into()
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
        let Some(key_path) = req.key_ty.as_path(self.db) else {
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
        self.vars.get(&var).copied()
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
                    .map_or_else(|| "_".to_string(), |ident| ident.data(db).to_string())
            }
            Self::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            } => effect_param_name(db, *effect_site, *idx)
                .or_else(|| param_name(db, ParamSite::EffectField(*effect_site), *idx))
                .map_or_else(|| format!("%param{idx}"), |ident| ident.data(db).clone()),
            Self::Param { site, idx, .. } => param_name(db, *site, *idx)
                .map_or_else(|| format!("%param{idx}"), |ident| ident.data(db).clone()),
            Self::EffectParam {
                binding_name, idx, ..
            } => Some(*binding_name)
                .map_or_else(|| format!("%effect{idx}"), |ident| ident.data(db).clone()),
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
    pub context: TraitSolveCx<'db>,
    pub goal: TraitInstId<'db>,
    pub origin: TraitObligationOrigin<'db>,
    pub span: DynLazySpan<'db>,
}

impl<'db> TyCheckEnv<'db> {}

#[cfg(test)]
mod pr1797_admission_review {
    use super::*;
    use crate::test_db::{HirAnalysisTestDb, find_func};

    #[test]
    fn active_opening_uses_finalized_self_excluding_admission() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("active-admission.fe".into(),
            "use core::obligations::Evidence\ntrait Goal {}\nstruct Subject {}\nfn inspect(_ witness: own Evidence<Goal<Subject>>) {}\n");
        let (module, _) = db.top_mod(file);
        let func = find_func(&db, module, "inspect");
        let mut env = TyCheckEnv::new(&db, BodyOwner::Func(func)).unwrap();
        let ty = *func.arg_tys(&db)[0].skip_binder();
        let goal = crate::analysis::ty::corelib::evidence_goal(&db, func.scope(), ty).unwrap();
        let value = env.body.expr(&db);
        env.enter_with_frame();
        env.insert_opening(
            value,
            super::super::TraitOpening::Evidence {
                operand: value,
                goal,
                admission: None,
            },
        )
        .unwrap();
        env.finalize_opening_admission(value);
        let active = env.opening_for(goal).unwrap();
        let super::super::TraitOpening::Evidence {
            admission: Some(context),
            ..
        } = active
        else {
            panic!(
                "active consumers must receive finalized admission, not own-goal fallback: {active:?}"
            )
        };
        assert!(!context.assumptions().list(&db).contains(&goal));
        assert_eq!(env.with_openings[value], Some(active));
    }
}

impl<'db> crate::analysis::semantic::operand_recovery::OperandRecoveryCx<'db> for TyCheckEnv<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }
    fn body(&self) -> Option<Body<'db>> {
        Some(self.body)
    }
    fn owner(&self) -> BodyOwner<'db> {
        self.owner
    }
    fn opening(&self, operand: ExprId) -> Option<super::TraitOpening<'db>> {
        self.with_openings[operand]
    }
    fn expr_ty(&self, _db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.typed_expr_ty(expr)
    }
    fn binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.typed_expr(expr).and_then(|prop| prop.binding)
    }
    fn binding_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> Option<super::BindingSource> {
        super::binding_source::binding_source(
            db,
            self.body,
            &self.pattern_store,
            &|pat| self.pattern_status[pat].ready_root(),
            binding,
        )
    }
    fn record_init_lowering(&self, expr: ExprId) -> Option<super::RecordInitLowering<'db>> {
        self.record_init_lowering[expr]
    }
    fn field_index(&self, expr: ExprId) -> Option<u16> {
        self.resolved_field_index[expr]
    }
    fn projected_evidence(
        &self,
        _expr: ExprId,
    ) -> Option<crate::analysis::ty::trait_def::ImplApplicationRecord<'db>> {
        None
    }
    fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.pattern_status[pat].ready_root()
    }
    fn pattern_store(&self) -> &PatternStore<'db> {
        &self.pattern_store
    }
    fn callable(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callable_expr(expr)
    }
    fn leaf(
        &self,
        operand: ExprId,
        stop: &mut Option<crate::analysis::semantic::operand_recovery::KnownOperandRecovery<'db>>,
        active: &mut Vec<ExprId>,
    ) -> Option<crate::analysis::semantic::SemConstId<'db>> {
        use crate::analysis::semantic::{
            ConstDesc, CtfeConfig, EvalOutcome, SemOrigin, SemanticInstanceKey,
            VerifiedConstValueId, const_computation_for_instance, force_const_computation,
            operand_recovery::{KnownOperandRecovery, recover_value},
        };
        let ty = self.typed_expr(operand)?.ty;
        if let Some(reference) = self.expr_const_ref(operand) {
            let reference = match reference {
                ConstRef::TraitConst(assoc) if !self.const_openings[operand].is_empty() => {
                    // Use the source expression's checked frame snapshot, not
                    // the current frame or ordinary trait discovery. A pending
                    // record stays pending and cannot authorize a fallback.
                    let records =
                        crate::analysis::semantic::operand_recovery::saved_opening_applications(
                            self.db,
                            assoc.inst(),
                            &self.const_openings[operand],
                            |operand| self.known_operand_application(operand),
                        )?;
                    let key =
                        crate::analysis::semantic::instance::semantic_const_key_for_assoc_const(
                            self.db, assoc, records,
                        )?;
                    crate::analysis::semantic::SemanticConstRef::new(
                        self.db,
                        key,
                        ty,
                        SemOrigin::Expr(operand),
                    )
                }
                _ => crate::analysis::semantic::instance::resolve_semantic_const_ref(
                    self.db,
                    reference,
                    ty,
                    SemOrigin::Expr(operand),
                )?,
            };
            // Never force a reference back into the checker currently borrowing these tables.
            if reference.instance(self.db).owner(self.db) == self.owner {
                return None;
            }
            return match crate::analysis::semantic::eval_const_ref(self.db, reference) {
                EvalOutcome::Ready(value) => Some(value),
                EvalOutcome::Blocked(info) => {
                    *stop = Some(KnownOperandRecovery::Blocked {
                        owner: reference.instance(self.db).owner(self.db),
                        info,
                        result_ty: ty,
                    });
                    None
                }
                EvalOutcome::Failed(failure) => {
                    *stop = Some(KnownOperandRecovery::Failed {
                        owner: reference.instance(self.db).owner(self.db),
                        failure,
                        result_ty: ty,
                    });
                    None
                }
            };
        }
        let callable = self.callable_expr(operand)?;
        let CallableDef::Func(func) = callable.callable_def() else {
            return None;
        };
        if BodyOwner::Func(func) == self.owner
            || !func.is_const(self.db)
            || callable.checked_input_tys()?.len() != callable.argument_operands().len()
            || !self.call_effect_args[operand]
                .as_deref()
                .unwrap_or(&[])
                .is_empty()
            || callable.generic_args().iter().any(|arg| {
                arg.has_param(self.db)
                    || arg.has_var(self.db)
                    || arg.has_invalid(self.db)
                    || arg.has_hole(self.db)
                    || arg.has_projection(self.db)
            })
        {
            return None;
        }
        let values = callable
            .argument_operands()
            .iter()
            .map(|&operand| recover_value(self, operand, stop, active))
            .collect::<Option<Vec<_>>>()?;
        let opened = if let Some((opened, _)) = callable.opened_trait_application() {
            let value = recover_value(self, opened, stop, active)?;
            let crate::analysis::semantic::SemConstValue::Evidence { application, .. } =
                value.value(self.db)
            else {
                return None;
            };
            Some(*application)
        } else {
            None
        };
        let callee = crate::analysis::semantic::instance::checked_operand_callee(
            self.db,
            self.scope(),
            self.assumptions,
            callable,
            opened,
        )
        .ok()??;
        if !callee.concrete_dispatch {
            let dependency = callee
                .key
                .subst(self.db)
                .generic_args(self.db)
                .first()
                .copied()
                .unwrap_or(ty);
            *stop = Some(KnownOperandRecovery::Blocked {
                owner: callee.key.owner(self.db),
                result_ty: ty,
                info: crate::analysis::semantic::BlockedInfo::new(
                    crate::analysis::semantic::ConstDemandKind::CallableSelection,
                    crate::analysis::semantic::ConstDependency::Type(dependency),
                    SemOrigin::Expr(operand),
                ),
            });
            return None;
        }
        let key = callee.key;
        let parameters = values.iter().copied().enumerate().collect::<Vec<_>>();
        let env = key
            .impl_env(self.db)
            .with_argument_evidence_values(self.db, &parameters);
        let key = SemanticInstanceKey::new(
            self.db,
            key.owner(self.db),
            key.subst(self.db),
            key.effect_providers(self.db),
            env,
        );
        let inputs = values
            .into_iter()
            .map(|value| {
                VerifiedConstValueId::from_complete_execution(self.db, value)
                    .ok()
                    .map(|value| ConstDesc::value(self.db, value, key.owner(self.db).scope()))
            })
            .collect::<Option<Vec<_>>>()?;
        match force_const_computation(
            self.db,
            const_computation_for_instance(self.db, key, inputs),
            CtfeConfig::default(),
        ) {
            EvalOutcome::Ready(value) => Some(value.value()),
            EvalOutcome::Blocked(info) => {
                *stop = Some(KnownOperandRecovery::Blocked {
                    owner: key.owner(self.db),
                    info,
                    result_ty: ty,
                });
                None
            }
            EvalOutcome::Failed(failure) => {
                *stop = Some(KnownOperandRecovery::Failed {
                    owner: key.owner(self.db),
                    failure,
                    result_ty: ty,
                });
                None
            }
        }
    }
}
