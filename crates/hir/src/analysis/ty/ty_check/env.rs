use crate::{
    hir_def::{
        Body, Contract, EffectParamListId, Expr, ExprId, FieldParent, Func, IdentId, IntegerId,
        Partial, Pat, PatId, PathId, Stmt, StmtId, TraitRefId, prim_ty::PrimTy,
        scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec1::SmallVec;
use thin_vec::ThinVec;

use super::owner::{BodyOwner, RecvArmInfo};
use super::{Callable, TypedBody};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        canonical::{Canonical, Canonicalized},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
        fold::{AssocTySubst, TyFoldable, TyFolder},
        normalize::normalize_ty,
        trait_def::TraitInstId,
        trait_lower::{TraitRefLowerError, lower_trait_ref},
        trait_resolution::{
            GoalSatisfiability, PredicateListId, constraint::collect_func_def_constraints,
            is_goal_satisfiable,
        },
        ty_def::{InvalidCause, TyBase, TyData, TyId, TyParam, TyVarSort},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
};

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    body: Body<'db>,
    recv_info: Option<RecvArmInfo<'db>>,

    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,

    deferred: Vec<DeferredTask<'db>>,

    effect_env: EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let Some(body) = owner.body(db) else {
            return Err(());
        };

        let owner_scope = owner.scope();
        let recv_info = owner.recv_arm_info(db);

        // Compute base assumptions (without effect-derived bounds) up-front
        let (base_preds, base_assumptions) = match owner {
            BodyOwner::Func(func) => {
                let preds =
                    collect_func_def_constraints(db, func.into(), true).instantiate_identity();
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
            recv_info,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            callables: FxHashMap::default(),
            deferred: Vec::new(),
            effect_env: EffectEnv::new(),
            effect_bounds: ThinVec::new(),
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
        };

        env.enter_scope(body.expr(db));

        env.register_params(base_assumptions);

        env.seed_effects(base_assumptions);

        // Finalize assumptions by merging in effect-derived bounds
        let mut preds = base_preds.list(db).to_vec();
        preds.extend(env.effect_bounds.iter().copied());
        env.assumptions = PredicateListId::new(db, preds).extend_all_bounds(db);

        Ok(env)
    }

    fn register_params(&mut self, base_assumptions: PredicateListId<'db>) {
        match self.owner {
            BodyOwner::Func(func) => {
                let arg_tys = func.arg_tys(self.db);
                for (idx, view) in func.params(self.db).enumerate() {
                    let Some(name) = view.name(self.db) else {
                        continue;
                    };

                    let mut ty = *arg_tys
                        .get(idx)
                        .map(|b| b.skip_binder())
                        .unwrap_or(&TyId::invalid(self.db, InvalidCause::ParseError));

                    if !ty.is_star_kind(self.db) {
                        ty = TyId::invalid(self.db, InvalidCause::Other);
                    }
                    let def_span = func.span().params().param(idx).name().into();
                    let var = LocalBinding::Param {
                        ident: name,
                        ty,
                        is_mut: view.is_mut(self.db),
                        def_span,
                    };

                    self.var_env.last_mut().unwrap().register_var(name, var);
                }
            }
            BodyOwner::ContractInit(contract) => {
                let Some(params) = contract.init_params(self.db) else {
                    return;
                };

                for (idx, param) in params.data(self.db).iter().enumerate() {
                    let Some(name) = param.name() else {
                        continue;
                    };
                    let ty = match param.ty.to_opt() {
                        Some(hir_ty) => {
                            lower_hir_ty(self.db, hir_ty, contract.scope(), base_assumptions)
                        }
                        None => TyId::invalid(self.db, InvalidCause::ParseError),
                    };
                    let ty = if ty.is_star_kind(self.db) {
                        ty
                    } else {
                        TyId::invalid(self.db, InvalidCause::Other)
                    };
                    let def_span = contract
                        .span()
                        .init_block()
                        .params()
                        .param(idx)
                        .name()
                        .into();
                    let var = LocalBinding::Param {
                        ident: name,
                        ty,
                        is_mut: param.is_mut,
                        def_span,
                    };
                    self.var_env.last_mut().unwrap().register_var(name, var);
                }
            }
            BodyOwner::ContractRecvArm { .. } => {}
        }
    }

    fn seed_effects(&mut self, base_assumptions: PredicateListId<'db>) {
        match self.owner {
            BodyOwner::Func(func) => self.seed_func_effects(func, base_assumptions),
            BodyOwner::ContractInit(_) | BodyOwner::ContractRecvArm { .. } => {
                self.seed_contract_effects(base_assumptions)
            }
        }
    }

    fn seed_func_effects(&mut self, func: Func<'db>, base_assumptions: PredicateListId<'db>) {
        for effect in func.effect_params(self.db) {
            let idx = effect.index();
            let Some(key_path) = effect.key_path(self.db) else {
                continue;
            };

            // Create an effect type param E and try to interpret key as a trait bound on E
            let ident = effect.name(self.db).unwrap_or_else(|| {
                key_path
                    .ident(self.db)
                    .to_opt()
                    .unwrap_or(IdentId::new(self.db, "_effect".to_string()))
            });
            let e_ty = TyId::new(
                self.db,
                TyData::TyParam(TyParam::effect_param(ident, idx, func.scope())),
            );

            let trait_ref = TraitRefId::new(self.db, Partial::Present(key_path));
            let provided_ty = match lower_trait_ref(
                self.db,
                e_ty,
                trait_ref,
                func.scope(),
                base_assumptions,
                None,
            ) {
                Ok(inst) => {
                    self.effect_bounds.push(inst);
                    e_ty
                }
                Err(TraitRefLowerError::InvalidDomain(
                    PathRes::Ty(ty) | PathRes::TyAlias(_, ty),
                )) if ty.is_star_kind(self.db) => ty,
                _ => TyId::invalid(self.db, InvalidCause::Other),
            };

            let origin = EffectOrigin::Param {
                site: EffectParamSite::Func(func),
                index: idx,
                name: effect.name(self.db),
            };
            let provided = ProvidedEffect {
                origin,
                ty: provided_ty,
                is_mut: effect.is_mut(self.db),
            };
            if let Some(key) =
                self.effect_key_for_path_in_scope(key_path, func.scope(), base_assumptions)
            {
                self.effect_env.insert(key, provided);
            }

            if let Some(ident) = effect.name(self.db) {
                let def_span = func.span().effects().param_idx(idx).name().into();
                let binding = LocalBinding::EffectParam {
                    ident,
                    key_path,
                    is_mut: effect.is_mut(self.db),
                    def_span,
                };
                self.var_env
                    .last_mut()
                    .expect("function scope exists")
                    .register_var(ident, binding);
            }
        }
    }

    fn seed_contract_effects(&mut self, base_assumptions: PredicateListId<'db>) {
        let mut global_idx = 0usize;
        for (site, list) in self.effect_sites() {
            for (idx, effect) in list.data(self.db).iter().enumerate() {
                let Some(key_path) = effect.key_path.to_opt() else {
                    continue;
                };

                let field_ty = self.contract_field_effect_ty(site, key_path);
                let binding_ident = effect.name.or_else(|| {
                    field_ty
                        .is_some()
                        .then(|| key_path.ident(self.db).to_opt())
                        .flatten()
                });

                let ident = effect.name.unwrap_or_else(|| {
                    key_path
                        .ident(self.db)
                        .to_opt()
                        .unwrap_or(IdentId::new(self.db, "_effect".to_string()))
                });

                let e_ty = TyId::new(
                    self.db,
                    TyData::TyParam(TyParam::effect_param(ident, global_idx, self.owner_scope)),
                );

                let trait_ref = TraitRefId::new(self.db, Partial::Present(key_path));
                let provided_ty = if let Some(field_ty) = field_ty {
                    field_ty
                } else {
                    match lower_trait_ref(
                        self.db,
                        e_ty,
                        trait_ref,
                        self.owner_scope,
                        base_assumptions,
                        None,
                    ) {
                        Ok(inst) => {
                            self.effect_bounds.push(inst);
                            e_ty
                        }
                        Err(TraitRefLowerError::InvalidDomain(
                            PathRes::Ty(ty) | PathRes::TyAlias(_, ty),
                        )) if ty.is_star_kind(self.db) => ty,
                        _ => TyId::invalid(self.db, InvalidCause::Other),
                    }
                };

                let origin = EffectOrigin::Param {
                    site,
                    index: global_idx,
                    name: binding_ident,
                };
                let provided = ProvidedEffect {
                    origin,
                    ty: provided_ty,
                    is_mut: effect.is_mut,
                };
                if field_ty.is_none()
                    && let Some(key) = self.effect_key_for_path_in_scope(
                        key_path,
                        self.owner_scope,
                        base_assumptions,
                    )
                {
                    self.effect_env.insert(key, provided);
                }

                if let Some(ident) = binding_ident {
                    let def_span = self.effect_param_span(site, idx);
                    let binding = if let Some(field_ty) = field_ty {
                        LocalBinding::Param {
                            ident,
                            ty: field_ty,
                            is_mut: effect.is_mut,
                            def_span,
                        }
                    } else {
                        LocalBinding::EffectParam {
                            ident,
                            key_path,
                            is_mut: effect.is_mut,
                            def_span,
                        }
                    };
                    self.var_env
                        .last_mut()
                        .expect("scope exists")
                        .register_var(ident, binding);
                }

                global_idx += 1;
            }
        }
    }

    fn effect_sites(&self) -> Vec<(EffectParamSite<'db>, EffectParamListId<'db>)> {
        match self.owner {
            BodyOwner::Func(_) => Vec::new(),
            BodyOwner::ContractInit(contract) => vec![
                (
                    EffectParamSite::ContractUses(contract),
                    contract.effects(self.db),
                ),
                (
                    EffectParamSite::ContractInitUses(contract),
                    contract.init_effects(self.db),
                ),
            ],
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => {
                let recv = contract.recvs(self.db).data(self.db)[recv_idx as usize].clone();
                let arm = recv.arms.data(self.db)[arm_idx as usize].clone();
                vec![
                    (
                        EffectParamSite::ContractUses(contract),
                        contract.effects(self.db),
                    ),
                    (
                        EffectParamSite::ContractRecvArmUses {
                            contract,
                            recv_idx,
                            arm_idx,
                        },
                        arm.effects,
                    ),
                ]
            }
        }
    }

    fn effect_param_span(&self, site: EffectParamSite<'db>, idx: usize) -> DynLazySpan<'db> {
        match site {
            EffectParamSite::Func(func) => func.span().effects().param_idx(idx).name().into(),
            EffectParamSite::ContractUses(contract) => {
                contract.span().effects().param_idx(idx).name().into()
            }
            EffectParamSite::ContractInitUses(contract) => contract
                .span()
                .init_block()
                .effects()
                .param_idx(idx)
                .name()
                .into(),
            EffectParamSite::ContractRecvArmUses {
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

    fn contract_from_site(site: EffectParamSite<'db>) -> Option<Contract<'db>> {
        match site {
            EffectParamSite::ContractUses(contract)
            | EffectParamSite::ContractInitUses(contract) => Some(contract),
            EffectParamSite::ContractRecvArmUses { contract, .. } => Some(contract),
            EffectParamSite::Func(_) => None,
        }
    }

    fn contract_field_effect_ty(
        &self,
        site: EffectParamSite<'db>,
        key_path: PathId<'db>,
    ) -> Option<TyId<'db>> {
        let contract = Self::contract_from_site(site)?;
        let ident = key_path.ident(self.db).to_opt()?;
        let parent = FieldParent::Contract(contract);
        let field_ty = parent
            .fields(self.db)
            .find(|f| f.name(self.db) == Some(ident))?
            .ty(self.db);

        Some(if field_ty.is_star_kind(self.db) {
            field_ty
        } else {
            TyId::invalid(self.db, InvalidCause::Other)
        })
    }

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<ExprProp<'db>> {
        self.expr_ty.get(&expr).cloned()
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables.insert(expr, callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }

    pub(super) fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables.get(&expr)
    }

    /// Returns a callable if the body owner is a function.
    pub(super) fn func(&self) -> Option<CallableDef<'db>> {
        match self.owner {
            BodyOwner::Func(func) => func.as_callable(self.db),
            _ => None,
        }
    }

    pub(super) fn assumptions(&self) -> PredicateListId<'db> {
        // Return the assumptions we computed in new, which includes
        // both generic bounds (if any) AND the effect parameter bounds.
        self.assumptions
    }

    pub(super) fn body(&self) -> Body<'db> {
        self.body
    }

    pub(super) fn recv_arm_info(&self) -> Option<&RecvArmInfo<'db>> {
        self.recv_info.as_ref()
    }

    pub(super) fn compute_expected_return(&self) -> TyId<'db> {
        match self.owner {
            BodyOwner::Func(func) => {
                let rt = func.return_ty(self.db);
                if func.has_explicit_return_ty(self.db) {
                    if rt.is_star_kind(self.db) {
                        rt
                    } else {
                        TyId::invalid(self.db, InvalidCause::Other)
                    }
                } else {
                    rt
                }
            }
            BodyOwner::ContractInit(_) => TyId::unit(self.db),
            BodyOwner::ContractRecvArm { .. } => {
                let Some(info) = self.recv_info.as_ref() else {
                    return TyId::unit(self.db);
                };
                let Some(ret_ty) = info.arm.ret_ty else {
                    return TyId::unit(self.db);
                };

                let ty = lower_hir_ty(self.db, ret_ty, self.owner_scope, self.assumptions());
                if ty.is_star_kind(self.db) {
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
                .get(pat)
                .copied()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param { ty, .. } => *ty,

            LocalBinding::EffectParam { key_path, .. } => {
                if let Some(key) = self.effect_key_for_path_in_scope(
                    *key_path,
                    self.owner_scope,
                    self.assumptions(),
                ) {
                    self.effect_env
                        .lookup(key)
                        .map(|binding| binding.ty)
                        .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other))
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
        }
    }

    pub(super) fn push_effect_frame(&mut self) {
        self.effect_env.push_frame();
    }

    pub(super) fn pop_effect_frame(&mut self) {
        self.effect_env.pop_frame();
    }

    pub(super) fn insert_effect_binding(
        &mut self,
        key_path: PathId<'db>,
        binding: ProvidedEffect<'db>,
    ) {
        // Prefer a key derived from the provided type (preserves generic args)
        // but fall back to the resolved path if bases don't match.
        if let Ok(path_res) =
            resolve_path(self.db, key_path, self.scope(), self.assumptions(), false)
        {
            let key = match path_res {
                PathRes::Ty(resolved) | PathRes::TyAlias(_, resolved) => {
                    let provided_base = binding.ty.base_ty(self.db).as_scope(self.db);
                    let resolved_base = resolved.base_ty(self.db).as_scope(self.db);
                    let ty = if provided_base == resolved_base {
                        binding.ty
                    } else {
                        resolved
                    };
                    Some(EffectKey::Type(ty))
                }
                PathRes::Trait(trait_inst) => Some(EffectKey::Trait(trait_inst)),
                _ => None,
            };
            if let Some(key) = key {
                self.effect_env.insert(key, binding);
            }
        }
    }

    pub(super) fn effect_candidates_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> SmallVec<[ProvidedEffect<'db>; 2]> {
        let mut out = SmallVec::new();
        let Some(path_res) = resolve_path(self.db, key_path, scope, assumptions, false).ok() else {
            return out;
        };

        match path_res {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                if let Some(b) = self.effect_env.lookup(EffectKey::Type(ty)) {
                    out.push(b);
                    return out;
                }
            }
            PathRes::Trait(tr) => {
                if let Some(b) = self.effect_env.lookup(EffectKey::Trait(tr)) {
                    out.push(b);
                    return out;
                }
            }
            _ => {}
        }

        for frame in self.effect_env.frames.iter().rev() {
            for (effect_key, provided) in &frame.bindings {
                match (&path_res, effect_key) {
                    (PathRes::Ty(req) | PathRes::TyAlias(_, req), EffectKey::Type(got)) => {
                        if req.base_ty(self.db).as_scope(self.db)
                            == got.base_ty(self.db).as_scope(self.db)
                        {
                            out.push(*provided);
                        }
                    }
                    (PathRes::Trait(req), EffectKey::Trait(got)) => {
                        if req.def(self.db) == got.def(self.db) {
                            out.push(*provided);
                        }
                    }
                    _ => {}
                }
            }
        }

        out
    }

    pub(super) fn enter_scope(&mut self, block: ExprId) {
        let new_scope = match block.data(self.db, self.body) {
            Partial::Present(Expr::Block(_)) => ScopeId::Block(self.body, block),
            _ => self.scope(),
        };

        let var_env = BlockEnv::new(new_scope, self.var_env.len());
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
        self.expr_ty.insert(expr, typed);
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId<'db>) {
        self.pat_ty.insert(pat, ty);
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

    pub(super) fn register_confirmation(&mut self, inst: TraitInstId<'db>, span: DynLazySpan<'db>) {
        self.deferred.push(DeferredTask::Confirm { inst, span })
    }

    pub(super) fn register_pending_method(&mut self, pending: PendingMethod<'db>) {
        self.deferred.push(DeferredTask::Method(pending))
    }

    /// Completes the type checking environment by finalizing pending trait
    /// confirmations, folding types with the unification table, and collecting
    /// diagnostics.
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
    /// The vector of `FuncBodyDiag` contains diagnostics related to function
    /// bodies, such as ambiguous trait instances.
    pub(super) fn finish(
        mut self,
        table: &mut UnificationTable<'db>,
        sink: &mut Vec<FuncBodyDiag<'db>>,
    ) -> TypedBody<'db> {
        let mut prober = Prober { table };
        // Resolve all deferred tasks (confirmations + method disambiguations)
        self.perform_deferred(&mut prober, sink);

        self.expr_ty
            .values_mut()
            .for_each(|ty| *ty = ty.clone().fold_with(self.db, &mut prober));

        self.pat_ty
            .values_mut()
            .for_each(|ty| *ty = ty.fold_with(self.db, &mut prober));

        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(self.db, &mut prober)))
            .collect();

        TypedBody {
            body: Some(self.body),
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            callables,
        }
    }

    pub(super) fn expr_data(&self, expr: ExprId) -> &'db Partial<Expr<'db>> {
        expr.data(self.db, self.body)
    }

    pub(super) fn stmt_data(&self, stmt: StmtId) -> &'db Partial<Stmt<'db>> {
        stmt.data(self.db, self.body)
    }

    pub(super) fn scope(&self) -> ScopeId<'db> {
        self.var_env.last().unwrap().scope
    }

    pub(super) fn current_block_idx(&self) -> usize {
        self.var_env.last().unwrap().idx
    }

    pub(super) fn get_block(&self, idx: usize) -> &BlockEnv<'db> {
        &self.var_env[idx]
    }

    /// Performs pending trait confirmations and collects diagnostics.
    ///
    /// This function attempts to satisfy all pending trait confirmations by
    /// iteratively probing and unifying trait instances until a fixed point
    /// is reached. If any trait instance remains ambiguous, a diagnostic is
    /// generated and added to the diagnostics vector.
    fn perform_deferred(
        &mut self,
        prober: &mut Prober<'db, '_>,
        sink: &mut Vec<FuncBodyDiag<'db>>,
    ) {
        let db = self.db;
        let scope = self.scope();
        let assumptions = self.assumptions();
        let ingot = self.body().top_mod(db).ingot(db);

        let compute_return_ty = |prober: &mut Prober<'db, '_>,
                                 recv_ty: TyId<'db>,
                                 inst: TraitInstId<'db>,
                                 method_name: IdentId<'db>| {
            let trait_method = *inst.def(db).method_defs(db).get(&method_name).unwrap();
            let func_ty =
                super::instantiate_trait_method(db, trait_method, prober.table, recv_ty, inst);
            let (base, gen_args) = func_ty.decompose_ty_app(db);
            let TyData::TyBase(TyBase::Func(func_def)) = base.data(db) else {
                unreachable!();
            };
            let mut ret = func_def.ret_ty(db).instantiate(db, gen_args);
            let mut subst = AssocTySubst::new(inst);
            ret = ret.fold_with(self.db, &mut subst);
            normalize_ty(db, ret, scope, assumptions)
        };

        let is_viable = |prober: &mut Prober<'db, '_>,
                         pending: &PendingMethod<'db>,
                         expr_ty: TyId<'db>,
                         inst: &TraitInstId<'db>| {
            let snap = prober.table.snapshot();
            let recv_ty = pending.recv_ty.fold_with(self.db, prober);
            let inst_self = prober.table.instantiate_to_term(inst.self_ty(db));
            if prober.table.unify(inst_self, recv_ty).is_err() {
                prober.table.rollback_to(snap);
                return false;
            }
            let ret_ty = compute_return_ty(prober, recv_ty, *inst, pending.method_name);
            let ok = prober.table.unify(expr_ty, ret_ty).is_ok();
            prober.table.rollback_to(snap);
            ok
        };

        // Fixed-point pass over deferred tasks
        let mut progressed = true;
        while progressed {
            progressed = false;
            let mut next: Vec<DeferredTask<'db>> = Vec::new();
            for task in self.deferred.drain(..) {
                match task {
                    DeferredTask::Confirm { inst, span } => {
                        let inst = inst.fold_with(self.db, prober);
                        let canonical_inst = Canonicalized::new(db, inst);
                        match is_goal_satisfiable(db, ingot, canonical_inst.value, assumptions) {
                            GoalSatisfiability::Satisfied(solution) => {
                                let solution =
                                    canonical_inst.extract_solution(prober.table, *solution);
                                prober.table.unify(inst, solution).unwrap();
                                let new_can = Canonical::new(db, inst.fold_with(db, prober.table));
                                if new_can != canonical_inst.value {
                                    progressed = true;
                                }
                            }
                            _ => next.push(DeferredTask::Confirm { inst, span }),
                        }
                    }
                    DeferredTask::Method(pending) => {
                        let recv_ty = pending.recv_ty.fold_with(self.db, prober);
                        let expr_ty = self.expr_ty[&pending.expr].ty.fold_with(self.db, prober);
                        if expr_ty.has_invalid(db) {
                            next.push(DeferredTask::Method(pending));
                            continue;
                        }
                        let viable: Vec<_> = pending
                            .candidates
                            .iter()
                            .copied()
                            .filter(|inst| is_viable(prober, &pending, expr_ty, inst))
                            .collect();
                        if let [inst] = viable.as_slice() {
                            let ret_ty =
                                compute_return_ty(prober, recv_ty, *inst, pending.method_name);
                            prober.table.unify(expr_ty, ret_ty).unwrap();
                            progressed = true;
                        } else {
                            next.push(DeferredTask::Method(pending));
                        }
                    }
                }
            }
            self.deferred = next;
        }

        // Emit diagnostics for remaining tasks
        for task in self.deferred.drain(..) {
            match task {
                DeferredTask::Confirm { inst, span } => {
                    let inst = inst.fold_with(self.db, prober);
                    let canonical_inst = Canonicalized::new(db, inst);
                    match is_goal_satisfiable(db, ingot, canonical_inst.value, assumptions) {
                        GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                            let cands = ambiguous
                                .iter()
                                .map(|s| canonical_inst.extract_solution(prober.table, *s))
                                .collect::<ThinVec<_>>();
                            if !inst.self_ty(db).has_var(db) {
                                sink.push(
                                    BodyDiag::AmbiguousTraitInst {
                                        primary: span.clone(),
                                        cands,
                                    }
                                    .into(),
                                )
                            }
                        }
                        GoalSatisfiability::UnSat(subgoal) => {
                            if !inst.self_ty(db).has_var(db) {
                                let unsat = subgoal
                                    .map(|s| canonical_inst.extract_solution(prober.table, s));
                                sink.push(
                                    TyDiagCollection::from(TraitConstraintDiag::TraitBoundNotSat {
                                        span: span.clone(),
                                        primary_goal: inst,
                                        unsat_subgoal: unsat,
                                    })
                                    .into(),
                                )
                            }
                        }
                        _ => {}
                    }
                }
                DeferredTask::Method(pending) => {
                    let expr_ty = self.expr_ty[&pending.expr].ty.fold_with(self.db, prober);
                    if expr_ty.has_invalid(self.db) {
                        continue;
                    }
                    let viable: ThinVec<_> = pending
                        .candidates
                        .iter()
                        .copied()
                        .filter(|inst| is_viable(prober, &pending, expr_ty, inst))
                        .collect();
                    if viable.len() > 1 {
                        sink.push(
                            BodyDiag::AmbiguousTrait {
                                primary: pending.span.clone(),
                                method_name: pending.method_name,
                                traits: viable,
                            }
                            .into(),
                        );
                    }
                }
            }
        }
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

/// A key for looking up effect bindings.
/// This includes the definition scope and any type arguments, so that
/// `SomeTrait<u8>` and `SomeTrait<u16>` are distinct keys, and
/// `Storage<u8>` and `Storage<u16>` are also distinct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EffectKey<'db> {
    /// A type with its full generic arguments (e.g., `Storage<u8>`)
    Type(TyId<'db>),
    /// A trait with type arguments (e.g., `SomeTrait<u8>`)
    Trait(TraitInstId<'db>),
}

#[derive(Default)]
struct EffectFrame<'db> {
    bindings: FxHashMap<EffectKey<'db>, ProvidedEffect<'db>>,
}

pub(super) struct EffectEnv<'db> {
    frames: Vec<EffectFrame<'db>>,
}

impl<'db> EffectEnv<'db> {
    pub fn new() -> Self {
        Self {
            frames: vec![EffectFrame::default()],
        }
    }

    pub fn push_frame(&mut self) {
        self.frames.push(EffectFrame::default());
    }

    pub fn pop_frame(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    pub fn insert(&mut self, key: EffectKey<'db>, binding: ProvidedEffect<'db>) {
        self.frames
            .last_mut()
            .expect("EffectEnv must always have at least one frame")
            .bindings
            .insert(key, binding);
    }

    pub fn lookup(&self, key: EffectKey<'db>) -> Option<ProvidedEffect<'db>> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.bindings.get(&key).copied())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EffectParamSite<'db> {
    Func(Func<'db>),
    ContractUses(Contract<'db>),
    ContractInitUses(Contract<'db>),
    ContractRecvArmUses {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct ProvidedEffect<'db> {
    pub origin: EffectOrigin<'db>,
    pub ty: TyId<'db>,
    pub is_mut: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EffectOrigin<'db> {
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
    pub(crate) binding: Option<LocalBinding<'db>>,
}

impl<'db> ExprProp<'db> {
    pub(super) fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
        }
    }

    pub(super) fn new_binding_ref(ty: TyId<'db>, is_mut: bool, binding: LocalBinding<'db>) -> Self {
        Self {
            ty,
            is_mut,
            binding: Some(binding),
        }
    }

    pub(super) fn binding(&self) -> Option<LocalBinding<'db>> {
        self.binding.clone()
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum LocalBinding<'db> {
    Local {
        pat: PatId,
        is_mut: bool,
    },
    Param {
        ident: IdentId<'db>,
        ty: TyId<'db>,
        is_mut: bool,
        def_span: DynLazySpan<'db>,
    },
    EffectParam {
        ident: IdentId<'db>,
        key_path: PathId<'db>,
        is_mut: bool,
        def_span: DynLazySpan<'db>,
    },
}

impl<'db> LocalBinding<'db> {
    pub(super) fn local(pat: PatId, is_mut: bool) -> Self {
        Self::Local { pat, is_mut }
    }

    pub(super) fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. }
            | LocalBinding::Param { is_mut, .. }
            | LocalBinding::EffectParam { is_mut, .. } => *is_mut,
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

            Self::Param { ident, .. } | Self::EffectParam { ident, .. } => *ident,
        }
    }

    pub(super) fn def_span(&self, env: &TyCheckEnv<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(env.body).into(),
            LocalBinding::Param { def_span, .. } => def_span.clone(),
            LocalBinding::EffectParam { def_span, .. } => def_span.clone(),
        }
    }
}

struct Prober<'db, 'a> {
    table: &'a mut UnificationTable<'db>,
}

impl<'db> TyFolder<'db> for Prober<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let ty = self.table.fold_ty(db, ty);
        let TyData::TyVar(var) = ty.data(db) else {
            return ty.super_fold_with(db, self);
        };

        // String type variable fallback.
        if let TyVarSort::String(len) = var.sort {
            let ty = TyId::new(db, TyData::TyBase(PrimTy::String.into()));
            let len = EvaluatedConstTy::LitInt(IntegerId::new(db, BigUint::from(len)));
            let len = ConstTyData::Evaluated(len, ty.applicable_ty(db).unwrap().const_ty.unwrap());
            let len = TyId::const_ty(db, ConstTyId::new(db, len));
            TyId::app(db, ty, len)
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
    pub candidates: Vec<TraitInstId<'db>>,
    pub span: DynLazySpan<'db>,
}

#[derive(Debug, Clone)]
enum DeferredTask<'db> {
    Confirm {
        inst: TraitInstId<'db>,
        span: DynLazySpan<'db>,
    },
    Method(PendingMethod<'db>),
}

impl<'db> TyCheckEnv<'db> {
    /// Compute a normalized effect key for a given `key_path` resolved in `scope`
    /// under `assumptions`. The key includes type arguments so that different
    /// instantiations are distinct:
    /// - `SomeTrait<u8>` vs `SomeTrait<u16>` (traits)
    /// - `Storage<u8>` vs `Storage<u16>` (types)
    pub(super) fn effect_key_for_path_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<EffectKey<'db>> {
        let path_res = resolve_path(self.db, key_path, scope, assumptions, false).ok()?;
        match path_res {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                // Use the full TyId which includes generic arguments
                Some(EffectKey::Type(ty))
            }
            PathRes::Trait(trait_inst) => Some(EffectKey::Trait(trait_inst)),
            _ => None,
        }
    }
}
