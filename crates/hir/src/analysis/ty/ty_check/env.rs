use crate::{
    analysis::place::Place,
    hir_def::{
        BinOp, Body, Contract, EffectParamListId, Expr, ExprId, Func, IdentId, IntegerId, ItemKind,
        Partial, Pat, PatId, PathId, Stmt, StmtId, UnOp, prim_ty::PrimTy, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;
use common::indexmap::IndexMap;
use num_bigint::BigUint;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use smallvec1::SmallVec;
use thin_vec::ThinVec;

use super::owner::BodyOwner;
use super::{Callable, ConstRef, TypedBody, stmt::ForLoopSeq};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        effects::{
            EffectKeyKind, ResolvedEffectKey, normalize_effect_identity_trait,
            normalize_effect_identity_ty, place_effect_provider_param_index_map,
            resolve_effect_key,
        },
        fold::{TyFoldable, TyFolder},
        layout_holes::{
            LayoutPlaceholderPolicy, collect_layout_hole_tys_in_order,
            collect_unique_layout_placeholders_in_order_with_policy, layout_hole_fallback_ty,
            substitute_layout_placeholders_by_identity,
        },
        method_cmp::trait_effect_key_matches_with,
        trait_def::TraitInstId,
        trait_resolution::{
            PredicateListId,
            constraint::{
                collect_constraints, collect_func_decl_constraints, collect_func_def_constraints,
            },
        },
        ty_contains_const_hole,
        ty_def::{InvalidCause, TyData, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
        visitor::TyVisitor,
    },
};
use crate::core::semantic::EffectBinding;

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    body: Body<'db>,

    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    implicit_moves: FxHashSet<ExprId>,
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
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: FxHashMap<PatId, PatBindingMode>,

    /// Resolved effect arguments at call sites, keyed by the call expression.
    call_effect_args: FxHashMap<ExprId, Vec<super::ResolvedEffectArg<'db>>>,

    /// Resolved Seq trait methods for for-loops, keyed by the for statement.
    for_loop_seq: FxHashMap<StmtId, ForLoopSeq<'db>>,
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
            BodyOwner::AnonConstBody { .. } => {
                let containing_func = match owner_scope.parent_item(db) {
                    Some(ItemKind::Func(func)) => Some(func),
                    Some(ItemKind::Body(parent)) => parent.containing_func(db),
                    _ => None,
                };
                if let Some(func) = containing_func {
                    let mut preds =
                        collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
                    if let Some(ItemKind::Trait(trait_)) = func.scope().parent_item(db) {
                        let self_pred = TraitInstId::new(
                            db,
                            trait_,
                            trait_.params(db).to_vec(),
                            IndexMap::new(),
                        );
                        let mut merged = preds.list(db).to_vec();
                        merged.push(self_pred);
                        preds = PredicateListId::new(db, merged);
                    }
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

                    let preds = match parent_item {
                        Some(ItemKind::Trait(trait_)) => {
                            let self_pred = TraitInstId::new(
                                db,
                                trait_,
                                trait_.params(db).to_vec(),
                                IndexMap::new(),
                            );
                            PredicateListId::new(db, vec![self_pred])
                        }
                        Some(ItemKind::ImplTrait(impl_trait)) => {
                            collect_constraints(db, impl_trait.into()).instantiate_identity()
                        }
                        Some(ItemKind::Impl(impl_)) => {
                            collect_constraints(db, impl_.into()).instantiate_identity()
                        }
                        _ => PredicateListId::empty_list(db),
                    };
                    let assumptions = preds.extend_all_bounds(db);
                    (preds, assumptions)
                }
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
            implicit_moves: FxHashSet::default(),
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
            pat_binding_modes: FxHashMap::default(),
            call_effect_args: FxHashMap::default(),
            for_loop_seq: FxHashMap::default(),
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
                    self.seed_func_effects(func)
                }
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
            BodyOwner::ContractInit { .. } => self.seed_contract_effects(base_assumptions),
            BodyOwner::ContractRecvArm { .. } => self.seed_contract_effects(base_assumptions),
        }
    }

    fn seed_func_effects(&mut self, func: Func<'db>) {
        let assumptions = self.assumptions();
        let provider_map = place_effect_provider_param_index_map(self.db, func);
        let provider_params = CallableDef::Func(func).params(self.db);
        for binding in func.effect_bindings(self.db) {
            if !matches!(binding.key_kind, EffectKeyKind::Type | EffectKeyKind::Trait) {
                continue;
            }
            let idx = binding.binding_idx as usize;

            let Some(provider_param_idx) = provider_map.get(idx).copied().flatten() else {
                panic!("missing provider param for effect at index {idx}");
            };
            let Some(&provider_ty) = provider_params.get(provider_param_idx) else {
                panic!("provider param index {provider_param_idx} out of range");
            };

            let provided_ty = match binding.key_kind {
                EffectKeyKind::Trait => provider_ty,
                EffectKeyKind::Type => binding
                    .key_ty
                    .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),
                EffectKeyKind::Other => unreachable!(),
            };

            let local_binding = LocalBinding::EffectParam {
                site: EffectParamSite::Func(func),
                idx,
                key_path: binding.binding_path,
                is_mut: binding.is_mut,
            };
            if let Some(ident) = Some(binding.binding_name) {
                self.var_env
                    .last_mut()
                    .expect("function scope exists")
                    .register_var(ident, local_binding);
            }

            let origin = EffectOrigin::Param {
                site: EffectParamSite::Func(func),
                index: idx,
                name: func
                    .effect_params(self.db)
                    .nth(idx)
                    .and_then(|effect| effect.name(self.db)),
            };
            let provided = ProvidedEffect {
                origin,
                ty: provided_ty,
                is_mut: local_binding.is_mut(),
                binding: Some(local_binding),
            };
            if let Some(key) = effect_key_from_binding(self.db, binding) {
                let key = self.normalize_effect_key_in_scope(key, func.scope(), assumptions);
                self.effect_env.insert(key, provided);
            }
        }
    }

    fn seed_contract_effects(&mut self, _base_assumptions: PredicateListId<'db>) {
        let (contract, list_site) = match self.owner {
            BodyOwner::Func(func) => {
                let Some(contract) = self.parent_contract_for_func(func) else {
                    return;
                };
                (contract, EffectParamSite::Func(func))
            }
            BodyOwner::ContractInit { contract } => {
                (contract, EffectParamSite::ContractInit { contract })
            }
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => (
                contract,
                EffectParamSite::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
            ),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => return,
        };

        let assumptions = self.assumptions();
        let root_effect_ty =
            super::super::resolve_default_root_effect_ty(self.db, contract.scope(), assumptions);

        let mut contract_named: FxHashMap<IdentId<'db>, (usize, PathId<'db>, bool)> =
            FxHashMap::default();
        for (idx, e) in contract.effects(self.db).data(self.db).iter().enumerate() {
            if let (Some(name), Some(key)) = (e.name, e.key_path.to_opt()) {
                contract_named.insert(name, (idx, key, e.is_mut));
            }
        }

        let body_effects = match self.owner {
            BodyOwner::Func(func) => func.effects(self.db),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {
                EffectParamListId::new(self.db, Vec::new())
            }
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(self.db) else {
                    return;
                };
                init.effects(self.db)
            }
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => {
                let Some(arm) = contract.recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                else {
                    return;
                };
                arm.effects
            }
        };

        for (idx_in_body, effect) in body_effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect.key_path.to_opt() else {
                continue;
            };

            if let Some(binding_name) = effect.name {
                let Ok(path_res) =
                    resolve_path(self.db, key_path, contract.scope(), assumptions, false)
                else {
                    continue;
                };

                let provided_ty = match path_res {
                    PathRes::Trait(trait_inst) => match root_effect_ty {
                        Some(ty) => {
                            self.effect_bounds
                                .push(super::super::instantiate_trait_self(
                                    self.db, trait_inst, ty,
                                ));
                            ty
                        }
                        None => continue,
                    },
                    PathRes::Ty(ty) | PathRes::TyAlias(_, ty) if ty.is_star_kind(self.db) => ty,
                    _ => TyId::invalid(self.db, InvalidCause::Other),
                };

                let binding = LocalBinding::EffectParam {
                    site: list_site,
                    idx: idx_in_body,
                    key_path,
                    is_mut: effect.is_mut,
                };
                self.var_env
                    .last_mut()
                    .expect("scope exists")
                    .register_var(binding_name, binding);

                let origin = EffectOrigin::Param {
                    site: list_site,
                    index: idx_in_body,
                    name: Some(binding_name),
                };
                let provided = ProvidedEffect {
                    origin,
                    ty: provided_ty,
                    is_mut: effect.is_mut,
                    binding: Some(binding),
                };

                if let Some(key) =
                    self.effect_key_for_path_in_scope(key_path, contract.scope(), assumptions)
                {
                    self.effect_env.insert(key, provided);
                }
                continue;
            }

            if key_path.len(self.db) != 1 {
                continue;
            }

            let Some(ident) = key_path.ident(self.db).to_opt() else {
                continue;
            };

            if let Some(field_ty) = self.contract_field_effect_ty(list_site, key_path) {
                let binding = LocalBinding::Param {
                    site: ParamSite::EffectField(list_site),
                    idx: idx_in_body,
                    mode: FuncParamMode::View,
                    ty: field_ty,
                    is_mut: effect.is_mut,
                };
                self.var_env
                    .last_mut()
                    .expect("scope exists")
                    .register_var(ident, binding);

                let origin = EffectOrigin::Param {
                    site: list_site,
                    index: idx_in_body,
                    name: Some(ident),
                };
                let provided = ProvidedEffect {
                    origin,
                    ty: field_ty,
                    is_mut: effect.is_mut,
                    binding: Some(binding),
                };
                let key = self.normalize_effect_key_in_scope(
                    EffectKey::Type(field_ty),
                    contract.scope(),
                    assumptions,
                );
                self.effect_env.insert(key, provided);
                continue;
            }

            if let Some((_, decl_key, decl_is_mut)) = contract_named.get(&ident).copied() {
                let Ok(path_res) =
                    resolve_path(self.db, decl_key, contract.scope(), assumptions, false)
                else {
                    continue;
                };
                let provided_ty = match path_res {
                    PathRes::Trait(trait_inst) => match root_effect_ty {
                        Some(ty) => {
                            self.effect_bounds
                                .push(super::super::instantiate_trait_self(
                                    self.db, trait_inst, ty,
                                ));
                            ty
                        }
                        None => continue,
                    },
                    PathRes::Ty(ty) | PathRes::TyAlias(_, ty) if ty.is_star_kind(self.db) => ty,
                    _ => TyId::invalid(self.db, InvalidCause::Other),
                };

                let binding = LocalBinding::EffectParam {
                    site: list_site,
                    idx: idx_in_body,
                    key_path,
                    is_mut: decl_is_mut,
                };
                self.var_env
                    .last_mut()
                    .expect("scope exists")
                    .register_var(ident, binding);

                let origin = EffectOrigin::Param {
                    site: list_site,
                    index: idx_in_body,
                    name: Some(ident),
                };
                let provided = ProvidedEffect {
                    origin,
                    ty: provided_ty,
                    is_mut: decl_is_mut,
                    binding: Some(binding),
                };

                if let Some(key) =
                    self.effect_key_for_path_in_scope(decl_key, contract.scope(), assumptions)
                {
                    self.effect_env.insert(key, provided);
                }
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
        let ty = contract
            .fields(self.db)
            .get(&ident)
            .map(|info| info.target_ty)?;

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

    pub(super) fn register_for_loop_seq(&mut self, stmt: StmtId, seq: ForLoopSeq<'db>) {
        if self.for_loop_seq.insert(stmt, seq).is_some() {
            panic!("for loop seq is already registered for the given stmt")
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

    pub(super) fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings.get(&pat).copied()
    }

    pub(super) fn set_pat_binding_mode(&mut self, pat: PatId, mode: PatBindingMode) {
        if self.pat_bindings.contains_key(&pat) {
            self.pat_binding_modes.insert(pat, mode);
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
        key: EffectKey<'db>,
        binding: ProvidedEffect<'db>,
    ) {
        self.effect_env.insert(key, binding);
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

    #[cfg(test)]
    pub(super) fn effect_candidate_frames_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Vec<SmallVec<[EffectCandidate<'db>; 2]>> {
        let Some(lookup) = self.requested_effect_lookup(key_path, scope, assumptions) else {
            return Vec::new();
        };
        self.effect_candidate_frames_for_lookup(lookup)
    }

    pub(super) fn effect_candidate_frames_for_key(
        &self,
        key: EffectKey<'db>,
    ) -> Vec<SmallVec<[EffectCandidate<'db>; 2]>> {
        self.effect_candidate_frames_for_lookup(self.requested_effect_lookup_for_key(key))
    }

    fn effect_candidate_frames_for_lookup(
        &self,
        lookup: RequestedEffectLookup<'db>,
    ) -> Vec<SmallVec<[EffectCandidate<'db>; 2]>> {
        let scan_frames = |lookup: RequestedEffectLookup<'db>| {
            let lookup_has_keyed_barrier = lookup.has_keyed_barrier();
            let mut frames_out = Vec::new();
            let mut saw_keyed_barrier = false;
            for frame in self.effect_env.frames.iter().rev() {
                let mut out = SmallVec::new();
                let mut frame_has_keyed_barrier = false;
                match lookup {
                    RequestedEffectLookup::Exact(key) => {
                        if let Some(provided) = frame.bindings.get(&key) {
                            out.extend(provided.iter().copied().map(|provided| EffectCandidate {
                                provided,
                                matched_key: Some(key),
                            }));
                            frame_has_keyed_barrier = true;
                            saw_keyed_barrier = true;
                        }
                    }
                    RequestedEffectLookup::IdentityType(req) => {
                        for (effect_key, provided) in &frame.bindings {
                            if let EffectKey::Type(got) = effect_key
                                && type_effect_key_matches_by_identity(self.db, req, *got)
                            {
                                out.extend(provided.iter().copied().map(|provided| {
                                    EffectCandidate {
                                        provided,
                                        matched_key: Some(*effect_key),
                                    }
                                }));
                                frame_has_keyed_barrier = true;
                                saw_keyed_barrier = true;
                            }
                        }
                    }
                    RequestedEffectLookup::IdentityTrait(req) => {
                        for (effect_key, provided) in &frame.bindings {
                            if let EffectKey::Trait(got) = effect_key
                                && trait_effect_key_matches_by_identity(self.db, req, *got)
                            {
                                out.extend(provided.iter().copied().map(|provided| {
                                    EffectCandidate {
                                        provided,
                                        matched_key: Some(*effect_key),
                                    }
                                }));
                                frame_has_keyed_barrier = true;
                                saw_keyed_barrier = true;
                            }
                        }
                    }
                    RequestedEffectLookup::SchematicType(req) => {
                        for (effect_key, provided) in &frame.bindings {
                            if let EffectKey::Type(got) = effect_key
                                && req.base_ty(self.db).as_scope(self.db)
                                    == got.base_ty(self.db).as_scope(self.db)
                            {
                                out.extend(provided.iter().copied().map(|provided| {
                                    EffectCandidate {
                                        provided,
                                        matched_key: Some(*effect_key),
                                    }
                                }));
                            }
                        }
                    }
                    RequestedEffectLookup::SchematicTrait(req) => {
                        for (effect_key, provided) in &frame.bindings {
                            if let EffectKey::Trait(got) = effect_key
                                && req.def(self.db) == got.def(self.db)
                            {
                                out.extend(provided.iter().copied().map(|provided| {
                                    EffectCandidate {
                                        provided,
                                        matched_key: Some(*effect_key),
                                    }
                                }));
                            }
                        }
                    }
                }

                if !(frame_has_keyed_barrier && lookup_has_keyed_barrier) {
                    for provided in frame.unkeyed.iter() {
                        if provided.ty.has_invalid(self.db) {
                            continue;
                        }
                        match lookup {
                            RequestedEffectLookup::Exact(EffectKey::Type(_))
                            | RequestedEffectLookup::IdentityType(_)
                            | RequestedEffectLookup::SchematicType(_) => {
                                out.push(EffectCandidate {
                                    provided: *provided,
                                    matched_key: None,
                                })
                            }
                            RequestedEffectLookup::Exact(EffectKey::Trait(_))
                            | RequestedEffectLookup::IdentityTrait(_)
                            | RequestedEffectLookup::SchematicTrait(_) => {
                                // Trait satisfaction is checked at the call site so we
                                // can consider type arguments and current assumptions.
                                out.push(EffectCandidate {
                                    provided: *provided,
                                    matched_key: None,
                                });
                            }
                        }
                    }
                }

                if out.is_empty() {
                    if frame_has_keyed_barrier && lookup_has_keyed_barrier {
                        break;
                    }
                    continue;
                }

                // Prefer call-site validation over eager deduplication: multiple distinct
                // providers may share the same type (e.g. two `StorageMap<u256, u256, 0>`
                // effects).
                let mut seen = rustc_hash::FxHashSet::default();
                out.retain(|p| seen.insert(*p));
                frames_out.push(out);

                if frame_has_keyed_barrier && lookup_has_keyed_barrier {
                    break;
                }
            }
            (frames_out, saw_keyed_barrier)
        };

        let mut current = Some(lookup);
        while let Some(lookup) = current {
            let (frames_out, saw_keyed_barrier) = scan_frames(lookup);
            if saw_keyed_barrier {
                return frames_out;
            }
            current = lookup.fallback();
            if current.is_none() {
                return frames_out;
            }
        }
        unreachable!("lookup iteration always returns from inside the loop")
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
            self.pat_binding_modes
                .entry(pat)
                .or_insert(PatBindingMode::ByValue);
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

    pub(super) fn register_confirmation(&mut self, inst: TraitInstId<'db>, span: DynLazySpan<'db>) {
        self.deferred.push(DeferredTask::Confirm { inst, span })
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

        self.call_effect_args.values_mut().for_each(|args| {
            for arg in args {
                arg.instantiated_target_ty = arg
                    .instantiated_target_ty
                    .map(|ty| ty.fold_with(self.db, &mut prober));
            }
        });

        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(self.db, &mut prober)))
            .collect();

        let for_loop_seq = self
            .for_loop_seq
            .into_iter()
            .map(|(stmt, seq)| (stmt, seq.fold_with(self.db, &mut prober)))
            .collect();

        TypedBody {
            body: Some(self.body),
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            implicit_moves: self.implicit_moves,
            const_refs: self.const_refs,
            callables,
            call_effect_args: self.call_effect_args,
            param_bindings: self.param_bindings,
            pat_bindings: self.pat_bindings,
            pat_binding_modes: self.pat_binding_modes,
            for_loop_seq,
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

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::*;
    use crate::analysis::ty::layout_holes::{
        collect_layout_hole_tys_in_order, ty_contains_const_hole,
    };
    use crate::test_db::HirAnalysisTestDb;

    fn find_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        func_name: &str,
    ) -> crate::hir_def::Func<'db> {
        top_mod
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(db)
                        .to_opt()
                        .is_some_and(|name| name.data(db) == func_name) =>
                {
                    Some(func)
                }
                _ => None,
            })
            .expect("missing function")
    }

    fn seeded_env<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> TyCheckEnv<'db> {
        TyCheckEnv::new(db, BodyOwner::Func(func)).expect("function body should lower")
    }

    fn marker_effect<'db>(
        db: &'db HirAnalysisTestDb,
        func: Func<'db>,
        ty: TyId<'db>,
    ) -> ProvidedEffect<'db> {
        ProvidedEffect {
            origin: EffectOrigin::With {
                value_expr: func.body(db).expect("missing function body").expr(db),
            },
            ty,
            is_mut: false,
            binding: None,
        }
    }

    fn assert_hole_free_effect_key<'db>(db: &'db HirAnalysisTestDb, key: EffectKey<'db>) {
        match key {
            EffectKey::Type(ty) => assert!(
                !ty_contains_const_hole(db, ty),
                "seeded effect type key should be hole-free"
            ),
            EffectKey::Trait(trait_inst) => assert!(
                collect_layout_hole_tys_in_order(db, trait_inst).is_empty(),
                "seeded effect trait key should be hole-free"
            ),
        }
    }

    #[test]
    fn seed_func_effects_uses_canonical_distinct_callable_type_keys() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("seed_func_effects_uses_canonical_distinct_callable_type_keys.fe"),
            r#"
struct Slot<const ROOT: u256 = _> {}

fn f() uses (a: Slot, b: Slot) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let env = seeded_env(&db, func);
        let frame = env
            .effect_env
            .frames
            .last()
            .expect("missing root effect frame");
        let expected_keys: Vec<_> = func
            .effect_bindings(&db)
            .iter()
            .map(|binding| effect_key_from_binding(&db, binding).expect("missing canonical key"))
            .collect();

        assert_eq!(frame.bindings.len(), 2);
        assert_eq!(expected_keys.len(), 2);
        assert_ne!(expected_keys[0], expected_keys[1]);
        for key in expected_keys {
            assert_hole_free_effect_key(&db, key);
            assert!(frame.bindings.contains_key(&key));
        }
    }

    #[test]
    fn seed_func_effects_uses_canonical_assoc_type_keys() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("seed_func_effects_uses_canonical_assoc_type_keys.fe"),
            r#"
trait HasSlot {
    type Assoc
}

struct Slot<const ROOT: u256 = _> {}

fn f<T>() uses (slot: T::Assoc)
where
    T: HasSlot<Assoc = Slot<u256>>
{}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let env = seeded_env(&db, func);
        let frame = env
            .effect_env
            .frames
            .last()
            .expect("missing root effect frame");
        let binding = &func.effect_bindings(&db)[0];
        let expected_key = effect_key_from_binding(&db, binding).expect("missing canonical key");

        assert_hole_free_effect_key(&db, expected_key);
        assert_eq!(frame.bindings.len(), 1);
        assert!(frame.bindings.contains_key(&expected_key));
        assert_eq!(
            expected_key,
            EffectKey::Type(binding.key_ty.expect("missing type key"))
        );
    }

    #[test]
    fn seed_func_effects_uses_canonical_trait_keys() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("seed_func_effects_uses_canonical_trait_keys.fe"),
            r#"
trait Cap<const LEFT: u256 = _, const RIGHT: u256 = _> {}

fn f() uses (cap: Cap) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "f");
        let env = seeded_env(&db, func);
        let frame = env
            .effect_env
            .frames
            .last()
            .expect("missing root effect frame");
        let binding = &func.effect_bindings(&db)[0];
        let expected_key = effect_key_from_binding(&db, binding).expect("missing canonical key");

        assert_hole_free_effect_key(&db, expected_key);
        assert_eq!(frame.bindings.len(), 1);
        assert!(frame.bindings.contains_key(&expected_key));
        assert_eq!(
            expected_key,
            EffectKey::Trait(binding.key_trait.expect("missing trait key"))
        );
    }

    #[test]
    fn effect_candidate_frames_skip_inner_near_miss_for_concrete_type_keys() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_skip_inner_near_miss_for_concrete_type_keys.fe",
            ),
            r#"
struct Storage<T> {
    value: T,
}

fn needs_u8() uses (store: Storage<u8>) {}
fn needs_u16() uses (store: Storage<u16>) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_u8 = find_func(&db, top_mod, "needs_u8");
        let needs_u16 = find_func(&db, top_mod, "needs_u16");
        let host = find_func(&db, top_mod, "host");
        let request = needs_u8
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let outer_key = seeded_env(&db, needs_u8)
            .effect_key_for_path_in_scope(request, host.scope(), PredicateListId::empty_list(&db))
            .expect("missing outer key");
        let inner_request = needs_u16
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing inner request path");
        let inner_key = seeded_env(&db, needs_u16)
            .effect_key_for_path_in_scope(
                inner_request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing inner key");

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(outer_key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env.push_frame();
        env.effect_env
            .insert(inner_key, marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_in_scope(
            request,
            host.scope(),
            PredicateListId::empty_list(&db),
        );
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[0][0].matched_key, Some(outer_key));
    }

    #[test]
    fn effect_candidate_frames_fall_back_to_family_candidates_when_exact_type_key_is_missing() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_fall_back_to_family_candidates_when_exact_type_key_is_missing.fe",
            ),
            r#"
struct Storage<T> {
    value: T,
}

fn needs_u8() uses (store: Storage<u8>) {}
fn needs_u16() uses (store: Storage<u16>) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_u8 = find_func(&db, top_mod, "needs_u8");
        let needs_u16 = find_func(&db, top_mod, "needs_u16");
        let host = find_func(&db, top_mod, "host");
        let request = needs_u8
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let inner_request = needs_u16
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing inner request path");
        let inner_key = seeded_env(&db, needs_u16)
            .effect_key_for_path_in_scope(
                inner_request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing inner key");

        let mut env = seeded_env(&db, host);
        env.effect_env.push_frame();
        env.effect_env
            .insert(inner_key, marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_in_scope(
            request,
            host.scope(),
            PredicateListId::empty_list(&db),
        );
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::u256(&db));
        assert_eq!(frames[0][0].matched_key, Some(inner_key));
    }

    #[test]
    fn effect_candidate_frames_skip_inner_near_miss_for_concrete_trait_keys() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_skip_inner_near_miss_for_concrete_trait_keys.fe",
            ),
            r#"
struct Slot<T> {}
trait Cap<T> {}

fn needs_u8() uses (cap: Cap<Slot<u8>>) {}
fn needs_u16() uses (cap: Cap<Slot<u16>>) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_u8 = find_func(&db, top_mod, "needs_u8");
        let needs_u16 = find_func(&db, top_mod, "needs_u16");
        let host = find_func(&db, top_mod, "host");
        let request = needs_u8
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let outer_key = seeded_env(&db, needs_u8)
            .effect_key_for_path_in_scope(request, host.scope(), PredicateListId::empty_list(&db))
            .expect("missing outer key");
        let inner_request = needs_u16
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing inner request path");
        let inner_key = seeded_env(&db, needs_u16)
            .effect_key_for_path_in_scope(
                inner_request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing inner key");

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(outer_key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env.push_frame();
        env.effect_env
            .insert(inner_key, marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_in_scope(
            request,
            host.scope(),
            PredicateListId::empty_list(&db),
        );
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[0][0].matched_key, Some(outer_key));
    }

    #[test]
    fn effect_candidate_frames_skip_same_frame_unkeyed_when_exact_trait_key_exists() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_skip_same_frame_unkeyed_when_exact_trait_key_exists.fe",
            ),
            r#"
struct Slot<T> {}
trait Cap<T> {}

fn needs_u8() uses (cap: Cap<Slot<u8>>) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_u8 = find_func(&db, top_mod, "needs_u8");
        let host = find_func(&db, top_mod, "host");
        let request = needs_u8
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let key = seeded_env(&db, needs_u8)
            .effect_key_for_path_in_scope(request, host.scope(), PredicateListId::empty_list(&db))
            .expect("missing key");

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env
            .insert_unkeyed(marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_in_scope(
            request,
            host.scope(),
            PredicateListId::empty_list(&db),
        );
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[0][0].matched_key, Some(key));
    }

    #[test]
    fn effect_candidate_frames_skip_same_frame_unkeyed_when_layout_hole_trait_key_exists() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_skip_same_frame_unkeyed_when_layout_hole_trait_key_exists.fe",
            ),
            r#"
struct Slot<const ROOT: u256 = _> {}
trait Cap<T> {
    fn cap(self)
}

fn needs() uses (cap: Cap<Slot>) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs = find_func(&db, top_mod, "needs");
        let host = find_func(&db, top_mod, "host");
        let request = needs
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let request_key = effect_key_from_binding(
            &db,
            needs
                .effect_bindings(&db)
                .first()
                .expect("missing canonical binding"),
        )
        .expect("missing canonical key");
        let keyed_key = seeded_env(&db, host)
            .effect_key_for_with_binding_in_scope(
                request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing keyed with key");

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(keyed_key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env
            .insert_unkeyed(marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_for_key(request_key);
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[0][0].matched_key, Some(keyed_key));
    }

    #[test]
    fn effect_candidate_frames_skip_same_frame_unkeyed_when_layout_hole_type_key_exists() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "effect_candidate_frames_skip_same_frame_unkeyed_when_layout_hole_type_key_exists.fe",
            ),
            r#"
struct Slot<const ROOT: u256 = _> {}

fn needs() uses (slot: Slot) {}
fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs = find_func(&db, top_mod, "needs");
        let host = find_func(&db, top_mod, "host");
        let request = needs
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let request_key = effect_key_from_binding(
            &db,
            needs
                .effect_bindings(&db)
                .first()
                .expect("missing canonical binding"),
        )
        .expect("missing canonical key");
        let keyed_key = seeded_env(&db, host)
            .effect_key_for_with_binding_in_scope(
                request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing keyed with key");

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(keyed_key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env
            .insert_unkeyed(marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_for_key(request_key);
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[0][0].matched_key, Some(keyed_key));
    }

    #[test]
    fn effect_candidate_frames_keep_schematic_type_key_ambiguity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("effect_candidate_frames_keep_schematic_type_key_ambiguity.fe"),
            r#"
pub struct Storage<T> {
    value: T,
}

pub fn needs_u8() uses (store: Storage<u8>) {}
pub fn needs_u16() uses (store: Storage<u16>) {}
pub fn needs_storage<T>() uses (store: Storage<T>) {}
pub fn host() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_u8 = find_func(&db, top_mod, "needs_u8");
        let needs_u16 = find_func(&db, top_mod, "needs_u16");
        let needs_storage = find_func(&db, top_mod, "needs_storage");
        let host = find_func(&db, top_mod, "host");
        let request = needs_storage
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing request path");
        let outer_request = needs_u8
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing outer request path");
        let inner_request = needs_u16
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("missing inner request path");
        let outer_key = seeded_env(&db, needs_u8)
            .effect_key_for_path_in_scope(
                outer_request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing outer key");
        let inner_key = seeded_env(&db, needs_u16)
            .effect_key_for_path_in_scope(
                inner_request,
                host.scope(),
                PredicateListId::empty_list(&db),
            )
            .expect("missing inner key");
        let generic_env = seeded_env(&db, needs_storage);

        let mut env = seeded_env(&db, host);
        env.effect_env
            .insert(outer_key, marker_effect(&db, host, TyId::bool(&db)));
        env.effect_env.push_frame();
        env.effect_env
            .insert(inner_key, marker_effect(&db, host, TyId::u256(&db)));

        let frames = env.effect_candidate_frames_in_scope(
            request,
            needs_storage.scope(),
            generic_env.assumptions,
        );
        assert_eq!(frames.len(), 2);
        assert_eq!(frames[0].len(), 1);
        assert_eq!(frames[0][0].provided.ty, TyId::u256(&db));
        assert_eq!(frames[0][0].matched_key, Some(inner_key));
        assert_eq!(frames[1].len(), 1);
        assert_eq!(frames[1][0].provided.ty, TyId::bool(&db));
        assert_eq!(frames[1][0].matched_key, Some(outer_key));
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

pub(super) fn effect_key_from_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &EffectBinding<'db>,
) -> Option<EffectKey<'db>> {
    match binding.key_kind {
        EffectKeyKind::Type => {
            let ty = binding.key_ty?;
            debug_assert!(
                !ty_contains_const_hole(db, ty) || ty.has_invalid(db),
                "canonical function effect type key still contains unresolved layout holes"
            );
            Some(EffectKey::Type(ty))
        }
        EffectKeyKind::Trait => {
            let trait_inst = binding.key_trait?;
            debug_assert!(
                collect_layout_hole_tys_in_order(db, trait_inst).is_empty(),
                "canonical function effect trait key still contains unresolved layout holes"
            );
            Some(EffectKey::Trait(trait_inst))
        }
        EffectKeyKind::Other => None,
    }
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
pub(super) struct EffectCandidate<'db> {
    pub provided: ProvidedEffect<'db>,
    pub matched_key: Option<EffectKey<'db>>,
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
        mode: FuncParamMode,
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

            Self::Param { site, idx, .. } => param_name(env.db, *site, *idx)
                .unwrap_or_else(|| IdentId::new(env.db, "_".to_string())),
            Self::EffectParam { key_path, .. } => key_path
                .ident(env.db)
                .to_opt()
                .unwrap_or_else(|| IdentId::new(env.db, "_".to_string())),
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
    pub(super) fn def_span_in_body(&self, body: Body<'db>) -> DynLazySpan<'db> {
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
    Confirm {
        inst: TraitInstId<'db>,
        span: DynLazySpan<'db>,
    },
    Method(PendingMethod<'db>),
    PrimitiveOp(PendingPrimitiveOp),
}

impl<'db> TyCheckEnv<'db> {
    fn normalize_effect_key_in_scope(
        &self,
        key: EffectKey<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> EffectKey<'db> {
        match key {
            EffectKey::Type(ty) => EffectKey::Type(normalize_effect_identity_ty(
                self.db,
                ty,
                scope,
                assumptions,
                None,
            )),
            EffectKey::Trait(trait_inst) => EffectKey::Trait(normalize_effect_identity_trait(
                self.db,
                trait_inst,
                scope,
                assumptions,
                None,
            )),
        }
    }

    /// Compute a normalized effect-identity key for a given `key_path` resolved
    /// in `scope` under `assumptions`. The key includes type arguments so that
    /// different instantiations are distinct:
    /// - `SomeTrait<u8>` vs `SomeTrait<u16>` (traits)
    /// - `Storage<u8>` vs `Storage<u16>` (types)
    pub(super) fn effect_key_for_path_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<EffectKey<'db>> {
        match resolve_effect_key(self.db, key_path, scope, assumptions) {
            ResolvedEffectKey::Type(ty) => {
                Some(self.normalize_effect_key_in_scope(EffectKey::Type(ty), scope, assumptions))
            }
            ResolvedEffectKey::Trait(trait_inst) => Some(self.normalize_effect_key_in_scope(
                EffectKey::Trait(trait_inst),
                scope,
                assumptions,
            )),
            ResolvedEffectKey::Other => None,
        }
    }

    pub(super) fn effect_key_for_with_binding_in_scope(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<EffectKey<'db>> {
        if let Some(key) = self.effect_key_for_path_in_scope(key_path, scope, assumptions) {
            return Some(key);
        }

        match resolve_path(self.db, key_path, scope, assumptions, false).ok()? {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                Some(self.normalize_effect_key_in_scope(EffectKey::Type(ty), scope, assumptions))
            }
            PathRes::Trait(trait_inst) => Some(self.normalize_effect_key_in_scope(
                EffectKey::Trait(trait_inst),
                scope,
                assumptions,
            )),
            _ => None,
        }
    }

    #[cfg(test)]
    fn requested_effect_lookup(
        &self,
        key_path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<RequestedEffectLookup<'db>> {
        let key = self.effect_key_for_path_in_scope(key_path, scope, assumptions)?;
        Some(self.requested_effect_lookup_for_key(key))
    }

    fn requested_effect_lookup_for_key(&self, key: EffectKey<'db>) -> RequestedEffectLookup<'db> {
        if effect_key_contains_schematic_structure(self.db, key) {
            match key {
                EffectKey::Type(ty) => RequestedEffectLookup::SchematicType(ty),
                EffectKey::Trait(trait_inst) => RequestedEffectLookup::SchematicTrait(trait_inst),
            }
        } else {
            RequestedEffectLookup::Exact(key)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RequestedEffectLookup<'db> {
    Exact(EffectKey<'db>),
    IdentityType(TyId<'db>),
    IdentityTrait(TraitInstId<'db>),
    SchematicType(TyId<'db>),
    SchematicTrait(TraitInstId<'db>),
}

impl<'db> RequestedEffectLookup<'db> {
    fn has_keyed_barrier(self) -> bool {
        matches!(
            self,
            Self::Exact(_) | Self::IdentityType(_) | Self::IdentityTrait(_)
        )
    }

    fn fallback(self) -> Option<Self> {
        match self {
            Self::Exact(EffectKey::Type(ty)) => Some(Self::IdentityType(ty)),
            Self::Exact(EffectKey::Trait(trait_inst)) => Some(Self::IdentityTrait(trait_inst)),
            Self::IdentityType(ty) => Some(Self::SchematicType(ty)),
            Self::IdentityTrait(trait_inst) => Some(Self::SchematicTrait(trait_inst)),
            Self::SchematicType(_) | Self::SchematicTrait(_) => None,
        }
    }
}

fn type_effect_key_matches_by_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TyId<'db>,
    actual: TyId<'db>,
) -> bool {
    normalize_hidden_layout_placeholders_for_identity(db, expected)
        == normalize_hidden_layout_placeholders_for_identity(db, actual)
}

fn trait_effect_key_matches_by_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TraitInstId<'db>,
    actual: TraitInstId<'db>,
) -> bool {
    trait_effect_key_matches_with(db, expected, actual, |lhs, rhs| {
        normalize_hidden_layout_placeholders_for_identity(db, lhs)
            == normalize_hidden_layout_placeholders_for_identity(db, rhs)
    })
}

fn normalize_hidden_layout_placeholders_for_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> TyId<'db> {
    let layout_args = collect_unique_layout_placeholders_in_order_with_policy(
        db,
        ty,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    )
    .into_iter()
    .filter_map(|placeholder| {
        let TyData::ConstTy(const_ty) = placeholder.data(db) else {
            return None;
        };

        let fallback = match const_ty.data(db) {
            ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(db, *hole_ty),
            ConstTyData::TyParam(param, fallback_ty) if param.is_implicit() => *fallback_ty,
            _ => return None,
        };
        Some((placeholder, fallback))
    })
    .collect::<FxHashMap<_, _>>();

    substitute_layout_placeholders_by_identity(
        db,
        ty,
        &layout_args,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    )
}

fn effect_key_contains_schematic_structure<'db>(
    db: &'db dyn HirAnalysisDb,
    key: EffectKey<'db>,
) -> bool {
    match key {
        EffectKey::Type(ty) => ty_value_contains_schematic_structure(db, ty),
        EffectKey::Trait(trait_inst) => ty_value_contains_schematic_structure(db, trait_inst),
    }
}

pub(super) fn ty_value_contains_schematic_structure<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl crate::analysis::ty::visitor::TyVisitable<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, _: &crate::analysis::ty::ty_def::TyVar<'db>) {
            self.found = true;
        }

        fn visit_param(&mut self, param: &crate::analysis::ty::ty_def::TyParam<'db>) {
            self.found = !(param.is_trait_self() || param.is_effect_provider());
        }

        fn visit_const_param(
            &mut self,
            param: &crate::analysis::ty::ty_def::TyParam<'db>,
            _: TyId<'db>,
        ) {
            self.found = !param.is_implicit();
        }

        fn visit_assoc_ty(&mut self, _: &crate::analysis::ty::ty_def::AssocTy<'db>) {
            self.found = true;
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            crate::analysis::ty::visitor::walk_ty(self, ty);
        }

        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            if self.found {
                return;
            }
            crate::analysis::ty::visitor::walk_const_ty(self, const_ty);
        }
    }

    let mut finder = Finder { db, found: false };
    value.visit_with(&mut finder);
    finder.found
}
