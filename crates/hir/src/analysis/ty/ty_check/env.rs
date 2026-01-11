use crate::{
    analysis::place::Place,
    hir_def::{
        Body, Contract, EffectParamListId, Expr, ExprId, Func, IdentId, IntegerId, ItemKind,
        Partial, Pat, PatId, PathId, Stmt, StmtId, TraitRefId, prim_ty::PrimTy,
        scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use common::indexmap::IndexMap;
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec1::SmallVec;
use thin_vec::ThinVec;

use super::owner::BodyOwner;
use super::{Callable, ConstRef, TypedBody};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        fold::{TyFoldable, TyFolder},
        trait_def::TraitInstId,
        trait_lower::{TraitRefLowerError, lower_trait_ref},
        trait_resolution::{PredicateListId, constraint::collect_func_def_constraints},
        ty_def::{InvalidCause, TyData, TyId, TyParam, TyVarSort},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
};

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    body: Body<'db>,

    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    const_refs: FxHashMap<ExprId, ConstRef<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,

    deferred: Vec<DeferredTask<'db>>,

    effect_env: EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,

    /// Param bindings for transfer to TypedBody
    param_bindings: Vec<LocalBinding<'db>>,
    /// Pat bindings for transfer to TypedBody
    pat_bindings: FxHashMap<PatId, LocalBinding<'db>>,

    /// Resolved effect arguments at call sites, keyed by the call expression.
    call_effect_args: FxHashMap<ExprId, Vec<super::ResolvedEffectArg<'db>>>,
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
                let mut preds =
                    collect_func_def_constraints(db, func.into(), true).instantiate_identity();
                // Methods inside a trait implicitly assume `Self: Trait` in their bodies so
                // default method calls resolve against the trait being implemented.
                if let Some(ItemKind::Trait(trait_)) = func.scope().parent_item(db) {
                    let self_pred =
                        TraitInstId::new(db, trait_, trait_.params(db).to_vec(), IndexMap::new());
                    let mut merged = preds.list(db).to_vec();
                    merged.push(self_pred);
                    preds = PredicateListId::new(db, merged);
                }
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
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            const_refs: FxHashMap::default(),
            callables: FxHashMap::default(),
            deferred: Vec::new(),
            effect_env: EffectEnv::new(),
            effect_bounds: ThinVec::new(),
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
            param_bindings: Vec::new(),
            pat_bindings: FxHashMap::default(),
            call_effect_args: FxHashMap::default(),
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
                    let var = LocalBinding::Param {
                        site: ParamSite::Func(func),
                        idx,
                        ty,
                        is_mut: view.is_mut(db),
                    };

                    env.param_bindings.push(var);
                    if let Some(name) = view.name(db) {
                        env.var_env.last_mut().unwrap().register_var(name, var);
                    };
                }
            }
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

                    if !ty.is_star_kind(db) {
                        ty = TyId::invalid(db, InvalidCause::Other);
                    }

                    let var = LocalBinding::Param {
                        site: ParamSite::ContractInit(contract),
                        idx,
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

        env.seed_effects(base_assumptions);

        // Finalize assumptions by merging in effect-derived bounds
        let mut preds = base_preds.list(db).to_vec();
        preds.extend(env.effect_bounds.iter().copied());
        env.assumptions = PredicateListId::new(db, preds).extend_all_bounds(db);

        Ok(env)
    }

    fn seed_effects(&mut self, base_assumptions: PredicateListId<'db>) {
        match self.owner {
            BodyOwner::Func(func) => {
                if self.parent_contract_for_func(func).is_some() {
                    self.seed_contract_effects(base_assumptions)
                } else {
                    self.seed_func_effects(func, base_assumptions)
                }
            }
            BodyOwner::ContractInit { .. } => self.seed_contract_effects(base_assumptions),
            BodyOwner::ContractRecvArm { .. } => self.seed_contract_effects(base_assumptions),
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

            let binding_ident = effect
                .name(self.db)
                .or_else(|| key_path.ident(self.db).to_opt());
            let binding = LocalBinding::EffectParam {
                site: EffectParamSite::Func(func),
                idx,
                key_path,
                is_mut: effect.is_mut(self.db),
            };
            if let Some(ident) = binding_ident {
                self.var_env
                    .last_mut()
                    .expect("function scope exists")
                    .register_var(ident, binding);
            }

            let origin = EffectOrigin::Param {
                site: EffectParamSite::Func(func),
                index: idx,
                name: effect.name(self.db),
            };
            let provided = ProvidedEffect {
                origin,
                ty: provided_ty,
                is_mut: effect.is_mut(self.db),
                binding: Some(binding),
            };
            if let Some(key) =
                self.effect_key_for_path_in_scope(key_path, func.scope(), base_assumptions)
            {
                self.effect_env.insert(key, provided);
            }
        }
    }

    fn seed_contract_effects(&mut self, base_assumptions: PredicateListId<'db>) {
        let effect_scope = self.effect_owner_scope();
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
                    TyData::TyParam(TyParam::effect_param(ident, global_idx, effect_scope)),
                );

                let trait_ref = TraitRefId::new(self.db, Partial::Present(key_path));
                let provided_ty = if let Some(field_ty) = field_ty {
                    field_ty
                } else {
                    match lower_trait_ref(
                        self.db,
                        e_ty,
                        trait_ref,
                        effect_scope,
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

                let binding = if let Some(field_ty) = field_ty {
                    LocalBinding::Param {
                        site: ParamSite::EffectField(site),
                        idx,
                        ty: field_ty,
                        is_mut: effect.is_mut,
                    }
                } else {
                    LocalBinding::EffectParam {
                        site,
                        idx,
                        key_path,
                        is_mut: effect.is_mut,
                    }
                };
                if let Some(ident) = binding_ident {
                    self.var_env
                        .last_mut()
                        .expect("scope exists")
                        .register_var(ident, binding);
                }

                let origin = EffectOrigin::Param {
                    site,
                    index: global_idx,
                    name: binding_ident,
                };
                let provided = ProvidedEffect {
                    origin,
                    ty: provided_ty,
                    is_mut: effect.is_mut,
                    binding: Some(binding),
                };
                if let Some(field_ty) = field_ty {
                    // Insert effect keyed by the field's type (e.g., TokenStore)
                    self.effect_env.insert(EffectKey::Type(field_ty), provided);
                } else if let Some(key) =
                    self.effect_key_for_path_in_scope(key_path, effect_scope, base_assumptions)
                {
                    self.effect_env.insert(key, provided);
                }

                global_idx += 1;
            }
        }
    }

    fn effect_sites(&self) -> Vec<(EffectParamSite<'db>, EffectParamListId<'db>)> {
        match self.owner {
            BodyOwner::Func(func) => {
                let Some(contract) = self.parent_contract_for_func(func) else {
                    return Vec::new();
                };
                vec![
                    (
                        EffectParamSite::Contract(contract),
                        contract.effects(self.db),
                    ),
                    (EffectParamSite::Func(func), func.effects(self.db)),
                ]
            }
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(self.db) else {
                    return Vec::new();
                };
                vec![
                    (
                        EffectParamSite::Contract(contract),
                        contract.effects(self.db),
                    ),
                    (
                        EffectParamSite::ContractInit { contract },
                        init.effects(self.db),
                    ),
                ]
            }
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => {
                let Some(arm) = contract.recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                else {
                    return Vec::new();
                };
                vec![
                    (
                        EffectParamSite::Contract(contract),
                        contract.effects(self.db),
                    ),
                    (
                        EffectParamSite::ContractRecvArm {
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

    fn parent_contract_for_func(&self, func: Func<'db>) -> Option<Contract<'db>> {
        if let Some(ItemKind::Contract(contract)) = func.scope().parent_item(self.db) {
            Some(contract)
        } else {
            None
        }
    }

    fn effect_owner_scope(&self) -> ScopeId<'db> {
        match self.owner {
            BodyOwner::Func(func) => self
                .parent_contract_for_func(func)
                .map(|c| c.scope())
                .unwrap_or(self.owner_scope),
            _ => self.owner_scope,
        }
    }

    fn contract_from_site(&self, site: EffectParamSite<'db>) -> Option<Contract<'db>> {
        match site {
            EffectParamSite::Contract(contract) => Some(contract),
            EffectParamSite::ContractInit { contract } => Some(contract),
            EffectParamSite::ContractRecvArm { contract, .. } => Some(contract),
            EffectParamSite::Func(func) => self.parent_contract_for_func(func),
        }
    }

    fn contract_field_effect_ty(
        &self,
        site: EffectParamSite<'db>,
        key_path: PathId<'db>,
    ) -> Option<TyId<'db>> {
        let contract = self.contract_from_site(site)?;
        let ident = key_path.ident(self.db).to_opt()?;
        let idx = contract.hir_fields(self.db).field_idx(self.db, ident)?;

        // `Contract::field_infos` is cached (tracked) and safe to call in the type-checker.
        let infos = contract.field_infos(self.db);
        let ty = infos
            .get(idx)
            .map(|info| info.target_ty)
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));

        Some(if ty.is_star_kind(self.db) {
            ty
        } else {
            TyId::invalid(self.db, InvalidCause::Other)
        })
    }

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<ExprProp<'db>> {
        self.expr_ty.get(&expr).cloned()
    }

    pub(super) fn expr_place(&self, expr: ExprId) -> Option<Place<'db>> {
        Place::from_expr_in_body(self.db, self.body, expr, |expr| {
            self.typed_expr(expr).and_then(|p| p.binding)
        })
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables.insert(expr, callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }

    pub(super) fn register_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) {
        if self.const_refs.insert(expr, const_ref).is_some() {
            panic!("const ref is already registered for the given expr")
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

    pub(super) fn owner(&self) -> BodyOwner<'db> {
        self.owner
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
            BodyOwner::ContractInit { .. } => TyId::unit(self.db),
            BodyOwner::ContractRecvArm { .. } => {
                let Some(arm) = self.owner.recv_arm(self.db) else {
                    return TyId::invalid(self.db, InvalidCause::Other);
                };
                let Some(ret_ty) = arm.ret_ty else {
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

            LocalBinding::EffectParam { .. } => self
                .effect_env
                .lookup_by_binding(*binding)
                .map(|binding| binding.ty)
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),
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

    pub(super) fn insert_unkeyed_effect_binding(&mut self, binding: ProvidedEffect<'db>) {
        self.effect_env.insert_unkeyed(binding);
    }

    pub(super) fn push_call_effect_arg(
        &mut self,
        call_expr: ExprId,
        arg: super::ResolvedEffectArg<'db>,
    ) {
        self.call_effect_args
            .entry(call_expr)
            .or_default()
            .push(arg);
    }

    pub(super) fn effect_candidate_frames_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Vec<SmallVec<[ProvidedEffect<'db>; 2]>> {
        let mut frames_out = Vec::new();
        let Some(path_res) = resolve_path(self.db, key_path, scope, assumptions, false).ok() else {
            return frames_out;
        };

        for frame in self.effect_env.frames.iter().rev() {
            let mut out = SmallVec::new();
            for (effect_key, provided) in &frame.bindings {
                match (&path_res, effect_key) {
                    (PathRes::Ty(req) | PathRes::TyAlias(_, req), EffectKey::Type(got)) => {
                        if req.base_ty(self.db).as_scope(self.db)
                            == got.base_ty(self.db).as_scope(self.db)
                        {
                            out.extend_from_slice(provided);
                        }
                    }
                    (PathRes::Trait(req), EffectKey::Trait(got)) => {
                        if req.def(self.db) == got.def(self.db) {
                            out.extend_from_slice(provided);
                        }
                    }
                    _ => {}
                }
            }

            for provided in frame.unkeyed.iter() {
                if provided.ty.has_invalid(self.db) {
                    continue;
                }
                match &path_res {
                    PathRes::Ty(_) | PathRes::TyAlias(_, _) => out.push(*provided),
                    PathRes::Trait(_) => {
                        // Trait satisfaction is checked at the call site so we
                        // can consider type arguments and current assumptions.
                        out.push(*provided);
                    }
                    _ => {}
                }
            }
            if out.is_empty() {
                continue;
            }

            // Prefer call-site validation over eager deduplication: multiple distinct providers
            // may share the same type (e.g. two `StorageMap<u256, u256, 0>` effects).
            let mut seen = rustc_hash::FxHashSet::default();
            out.retain(|p| seen.insert(*p));
            frames_out.push(out);
        }

        frames_out
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
        // Also store in pat_bindings for transfer to TypedBody
        if let LocalBinding::Local { pat, .. } = binding {
            self.pat_bindings.insert(pat, binding);
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

    pub(super) fn register_confirmation(&mut self, inst: TraitInstId<'db>, span: DynLazySpan<'db>) {
        self.deferred.push(DeferredTask::Confirm { inst, span })
    }

    pub(super) fn register_pending_method(&mut self, pending: PendingMethod<'db>) {
        self.deferred.push(DeferredTask::Method(pending))
    }

    /// Completes the type checking environment by finalizing pending trait
    /// confirmations and folding types with the unification table.
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
        let mut prober = Prober { table };

        self.expr_ty
            .values_mut()
            .for_each(|ty| *ty = ty.clone().fold_with(self.db, &mut prober));

        self.pat_ty
            .values_mut()
            .for_each(|ty| *ty = ty.fold_with(self.db, &mut prober));

        self.const_refs
            .values_mut()
            .for_each(|cref| *cref = (*cref).fold_with(self.db, &mut prober));

        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(self.db, &mut prober)))
            .collect();

        TypedBody {
            body: Some(self.body),
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            const_refs: self.const_refs,
            callables,
            call_effect_args: self.call_effect_args,
            param_bindings: self.param_bindings,
            pat_bindings: self.pat_bindings,
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

    pub(super) fn take_deferred_tasks(&mut self) -> Vec<DeferredTask<'db>> {
        std::mem::take(&mut self.deferred)
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
    bindings: FxHashMap<EffectKey<'db>, Vec<ProvidedEffect<'db>>>,
    unkeyed: Vec<ProvidedEffect<'db>>,
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
            .entry(key)
            .or_default()
            .push(binding);
    }

    pub fn insert_unkeyed(&mut self, binding: ProvidedEffect<'db>) {
        self.frames
            .last_mut()
            .expect("EffectEnv must always have at least one frame")
            .unkeyed
            .push(binding);
    }

    pub fn lookup_by_binding(&self, binding: LocalBinding<'db>) -> Option<ProvidedEffect<'db>> {
        for frame in self.frames.iter().rev() {
            for provided in frame.unkeyed.iter().copied() {
                if provided.binding == Some(binding) {
                    return Some(provided);
                }
            }
            for provided in frame.bindings.values().flat_map(|v| v.iter().copied()) {
                if provided.binding == Some(binding) {
                    return Some(provided);
                }
            }
        }
        None
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
pub(super) struct ProvidedEffect<'db> {
    pub origin: EffectOrigin<'db>,
    pub ty: TyId<'db>,
    pub is_mut: bool,
    pub binding: Option<LocalBinding<'db>>,
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
    pub binding: Option<LocalBinding<'db>>,
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

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
        }
    }
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
        ty: TyId<'db>,
        is_mut: bool,
    },
    EffectParam {
        site: EffectParamSite<'db>,
        idx: usize,
        key_path: PathId<'db>,
        is_mut: bool,
    },
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

            Self::Param { site, idx, .. } => param_name(env.db, *site, *idx).unwrap(),
            Self::EffectParam { key_path, .. } => key_path.ident(env.db).unwrap(),
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
        match self {
            LocalBinding::Local { pat, .. } => pat.span(body).into(),
            LocalBinding::Param { site, idx, .. } => param_span(*site, *idx),
            LocalBinding::EffectParam { site, idx, .. } => effect_param_span(*site, *idx),
        }
    }
}

pub(super) struct Prober<'db, 'a> {
    table: &'a mut UnificationTable<'db>,
}

impl<'db, 'a> Prober<'db, 'a> {
    pub(super) fn new(table: &'a mut UnificationTable<'db>) -> Self {
        Self { table }
    }
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
pub(super) enum DeferredTask<'db> {
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
