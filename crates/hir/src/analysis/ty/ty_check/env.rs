use crate::{
    analysis::place::{Place, is_grant_place_expr, is_pointer_place_expr},
    hir_def::{
        BinOp, Body, Contract, Expr, ExprId, Func, IdentId, ModeMarker, Partial, Pat, PatId, Stmt,
        StmtId, TypeId as HirTypeId, UnOp, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::hir_def::CallableDef;
use crate::hir_def::params::{FuncParam, FuncParamMode};
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap};
use num_bigint::BigInt;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::SalsaValue;
use thin_vec::ThinVec;

use super::effect_env as keyed_effect_env;
use super::owner::BodyOwner;
use super::{
    Callable, ConstIntrinsicKind, ConstRef, SemanticExprLowering, TyChecker, TypedBody,
    TypedBodyTables, ValuePathRef, stmt::ForLoopPlan,
};
use crate::analysis::ty::pattern_ir::{
    PatternAnalysisStatus, PatternStore, ValidatedPat, ValidatedPatId,
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::consts::int_const,
    ty::{
        closure::closure_effect_requirements,
        const_ty::{
            CallableInputOrigin, ConstTyData, const_body_assumptions, const_ty_from_sem_const,
        },
        corelib::resolve_lib_type_path,
        drop_glue::implements_drop,
        effects::{
            EffectKeyKind,
            elaborate::{build_pattern_from_requirement_decl, seed_forwarder_from_requirement},
            model::EffectRequirementDecl,
            rows::{RowComponent, expand_rows},
        },
        evidence::prove,
        fold::{TyFoldable, TyFolder},
        normalize::normalize_ty,
        provider::AddressSpace,
        shape::Shape,
        trait_def::TraitInstId,
        trait_resolution::{
            PredicateListId, TraitSolveCx, constraint::collect_func_effect_provider_constraints,
        },
        ty_def::{
            BorrowKind, ClosureDef, ClosureTy, InvalidCause, StringFallback, TyData, TyId,
            TyVarSort,
        },
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
};
use crate::core::semantic::{
    EffectEnvView, EffectRequirement, EffectRequirementKey, ProviderBinding,
    ResolvedEffectBindingInfo, row_effect_provider,
};

/// The assumptions `func`'s body is checked under at `mode`, before a
/// closure in the body adds its own: its declared bounds
/// (`func_declared_bounds`) extended with what they imply.
pub(crate) fn func_signature<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    mode: Option<BorrowKind>,
) -> PredicateListId<'db> {
    func_declared_bounds(db, func, mode).extend_all_bounds(db)
}

/// The bounds `func`'s body declares at `mode`: its own and its parent's,
/// with a trait method's `Self: Trait`, and those its trait-keyed effects and
/// their rows' components put on their providers. A call proves these for
/// the instance of `func` it makes; the instance proves what they imply.
#[salsa::tracked(returns(copy))]
pub(crate) fn func_declared_bounds<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    mode: Option<BorrowKind>,
) -> PredicateListId<'db> {
    let base = crate::semantic::func_body_assumptions(db, func).at_mode(db, mode);
    let mut preds = base.list(db).to_vec();
    preds.extend(collect_func_effect_provider_constraints(db, func));
    if let Some(body) = func.body(db) {
        // A row that stays abstract may expand in an instance and renumber
        // the components after it, so components are only bound when every
        // row expands.
        let rows = expand_rows(
            db,
            func.effect_requirements(db, mode),
            func.scope(),
            base.extend_all_bounds(db),
        );
        if rows.abstract_rows.is_empty()
            && rows
                .components
                .iter()
                .all(|component| component.requirement.key.key_row().is_none())
        {
            preds.extend(rows.components.iter().filter_map(|component| {
                let trait_inst = component.requirement.key.key_trait()?;
                let provider = row_effect_provider(db, body.scope(), None, &component.requirement);
                Some(super::super::instantiate_trait_self(
                    db,
                    trait_inst,
                    provider.provider_ty,
                ))
            }));
        }
    }
    PredicateListId::new(db, preds)
}

pub(crate) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    body: Body<'db>,

    pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
    expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    callables: SecondaryMap<ExprId, Option<Callable<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<super::RecordInitLowering<'db>>>,
    closure_infos: SecondaryMap<ExprId, Option<ClosureInfo<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,

    deferred: Vec<DeferredTask<'db>>,

    effect_env: keyed_effect_env::EffectEnv<'db>,
    effect_bounds: ThinVec<TraitInstId<'db>>,
    base_assumptions: PredicateListId<'db>,
    /// The assumptions the body's owner declares, before a closure in the
    /// body adds its own: what an instance receives proofs of.
    signature: PredicateListId<'db>,
    assumptions: PredicateListId<'db>,
    var_env: Vec<BlockEnv<'db>>,
    /// The index of the block each binding is registered in, which tells
    /// whether a closure body's use of it is a capture.
    binding_block_idx: FxHashMap<LocalBinding<'db>, usize>,
    /// The closures whose bodies are being checked, innermost last.
    closure_stack: Vec<ActiveClosure<'db>>,
    closure_expectations: FxHashMap<ExprId, ClosureExpectation<'db>>,
    /// Accesses closures capture whose types were not yet known, with the
    /// path that captures each, to check once types are known.
    access_captures: Vec<(ExprId, LocalBinding<'db>, TyId<'db>)>,
    /// The rows of the closures checked so far.
    closure_effects: FxHashMap<ExprId, Vec<ResolvedEffectBindingInfo<'db>>>,
    /// The effect environments of the closures checked so far, where their
    /// deferred calls resolve.
    closure_envs: FxHashMap<ExprId, keyed_effect_env::EffectEnv<'db>>,
    /// The checked closure a deferred call being resolved is in, with the
    /// environment its own replaced.
    late_closure: Option<(ClosureDef<'db>, keyed_effect_env::EffectEnv<'db>)>,
    /// The components of the rows a function's effects name, when its view
    /// expands every row: the body uses them as its own effects.
    row_components: Vec<ResolvedEffectBindingInfo<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,

    /// Param bindings for transfer to TypedBody
    param_bindings: Vec<LocalBinding<'db>>,
    /// The inputs a call passes the body: each parameter, or each part of a
    /// product parameter.
    input_bindings: Vec<LocalBinding<'db>>,
    /// The shapes of the product parameters, by index.
    param_products: Vec<(ParamSite<'db>, usize, Shape<'db>)>,
    /// The opaque parts of products that destructured parameters bind whole,
    /// by the name's pattern.
    transport_parts: Vec<(PatId, Shape<'db>)>,
    /// Pat bindings for transfer to TypedBody
    pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
    local_borrow_providers: SecondaryMap<PatId, Option<AddressSpace>>,
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
    pattern_store: PatternStore<'db>,
    pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,

    /// Resolved effect arguments at call sites, keyed by the call expression.
    call_effect_args: SecondaryMap<ExprId, Option<Vec<super::ResolvedEffectArg<'db>>>>,

    /// Expressions granting accesses that were used as accesses: bound by a
    /// pattern, passed to a view or `mut` parameter, used as a receiver or
    /// place base, or yielded. Any other use of `ref p`, `mut p` or a tuple or
    /// sum shape is a value use, which is an error.
    consumed_accesses: FxHashSet<ExprId>,
    /// The places a pattern is matched against, which bind each component
    /// where it lies.
    matched_places: FxHashSet<ExprId>,
    /// The matched places whose patterns have no `ref` or `mut` binding.
    inspected_places: FxHashSet<ExprId>,
    /// Index expressions naming an entry of a collection whose elements are
    /// places (`core::ops::StateIndex`).
    place_entries: FxHashSet<ExprId>,
    /// The tuples that are unsafe splits (`check_unsafe_split`).
    unsafe_splits: FxHashSet<ExprId>,
    /// The tuples passed to product parameters, a part each.
    product_args: FxHashSet<ExprId>,
    /// The calls `f(..)` of callable values, whose callee is the receiver.
    value_calls: FxHashSet<ExprId>,
    covered_providers: FxHashSet<ExprId>,
    /// The part of a projection's return shape each yield site grants.
    yield_shapes: SecondaryMap<ExprId, Option<Shape<'db>>>,

    /// The Collection protocol plan of each for loop, keyed by its statement.
    for_loop_plans: SecondaryMap<StmtId, Option<ForLoopPlan<'db>>>,

    /// The constrained type applications that resolving the body's paths
    /// passed through, each with the span of the expression or pattern whose
    /// path it is.
    path_applications: Vec<(DynLazySpan<'db>, TyId<'db>)>,
}

/// A closure expression's parameters and captured bindings, in the order of
/// its type's environment fields.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct ClosureInfo<'db> {
    pub params: Vec<LocalBinding<'db>>,
    pub captures: Vec<ClosureCapture<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct ClosureCapture<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
}

struct ActiveClosure<'db> {
    def: ClosureDef<'db>,
    /// The block that contains the closure expression: bindings registered
    /// in it or an outer block are captures.
    boundary_block_idx: usize,
    /// The enclosing body's effect environment, which the closure's body
    /// does not see: what it uses of it are the closure's row's components.
    enclosing_effects: keyed_effect_env::EffectEnv<'db>,
    effects: Vec<ResolvedEffectBindingInfo<'db>>,
    params: Vec<LocalBinding<'db>>,
    captures: IndexMap<LocalBinding<'db>, TyId<'db>>,
}

/// The callable shape a closure literal is expected to implement, from the
/// bound on the parameter it is passed to, deduced before its body is
/// checked.
#[derive(Debug, Clone)]
pub(super) struct ClosureExpectation<'db> {
    /// The shape of each parameter.
    pub(super) params: Vec<Shape<'db>>,
    pub(super) ret: Option<TyId<'db>>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let Some(body) = owner.body(db) else {
            return Err(());
        };

        let owner_scope = owner.scope();

        // Compute base assumptions (without effect-derived bounds) up-front
        let (base_preds, base_assumptions) = match owner {
            BodyOwner::Func(func, mode) => {
                // Trait methods implicitly assume `Self: Trait` in their bodies so
                // default method calls resolve against the trait being implemented.
                // A `mut?` bound is the body's mode's.
                let preds = crate::semantic::func_body_assumptions(db, func).at_mode(db, mode);
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
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            callables: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            closure_infos: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            deferred: Vec::new(),
            effect_env: keyed_effect_env::EffectEnv::new(),
            effect_bounds: ThinVec::new(),
            base_assumptions,
            signature: base_assumptions,
            assumptions: base_assumptions,
            var_env: vec![BlockEnv::new(owner_scope, 0)],
            binding_block_idx: FxHashMap::default(),
            closure_stack: Vec::new(),
            closure_expectations: FxHashMap::default(),
            access_captures: Vec::new(),
            closure_effects: FxHashMap::default(),
            closure_envs: FxHashMap::default(),
            late_closure: None,
            row_components: Vec::new(),
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
            param_bindings: Vec::new(),
            input_bindings: Vec::new(),
            param_products: Vec::new(),
            transport_parts: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            local_borrow_providers: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            call_effect_args: SecondaryMap::new(),
            consumed_accesses: FxHashSet::default(),
            matched_places: FxHashSet::default(),
            inspected_places: FxHashSet::default(),
            place_entries: FxHashSet::default(),
            unsafe_splits: FxHashSet::default(),
            product_args: FxHashSet::default(),
            value_calls: FxHashSet::default(),
            covered_providers: FxHashSet::default(),
            yield_shapes: SecondaryMap::new(),
            for_loop_plans: SecondaryMap::new(),
            path_applications: Vec::new(),
        };

        env.enter_scope(body.expr(db));

        match owner {
            BodyOwner::Func(func, mode) => {
                let bindings = func_param_bindings(db, func, mode);
                for (view, &var) in func.params(db).zip(&bindings.params) {
                    if let Some(name) = view.hir(db).and_then(FuncParam::binding_name) {
                        env.register_var_in_current_scope(name, var);
                    };
                }
                env.param_bindings = bindings.params;
                env.input_bindings = bindings.inputs;
                env.param_products = bindings.products;
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(db) else {
                    return Ok(env);
                };
                let assumptions = base_assumptions;
                for (idx, param) in init.params(db).data(db).iter().enumerate() {
                    let mut ty = match param
                        .ty
                        .to_opt()
                        .and_then(|ty| ty.without_mode(db).to_opt())
                    {
                        Some(hir_ty) => lower_hir_ty(db, hir_ty, owner_scope, assumptions),
                        None => TyId::invalid(db, InvalidCause::ParseError),
                    };

                    if !ty.is_star_kind(db) {
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
                    env.input_bindings.push(var);
                    if let Some(name) = param.binding_name() {
                        env.register_var_in_current_scope(name, var);
                    }
                }
            }
            BodyOwner::ContractRecvArm { .. } | BodyOwner::Closure { .. } => {}
        }

        env.register_effect_bindings(base_assumptions);

        // Finalize assumptions by merging in effect-derived bounds
        env.assumptions = if let BodyOwner::Func(func, mode) = owner {
            func_signature(db, func, mode)
        } else {
            let mut preds = base_preds.list(db).to_vec();
            preds.extend(env.effect_bounds.iter().copied());
            PredicateListId::new(db, preds).extend_all_bounds(db)
        };
        env.signature = env.assumptions;

        Ok(env)
    }

    fn register_effect_bindings(&mut self, base_assumptions: PredicateListId<'db>) {
        match self.owner {
            BodyOwner::Func(func, mode) => self.register_func_effect_bindings(func, mode),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            BodyOwner::ContractInit { .. } => {
                self.register_contract_effect_bindings(base_assumptions)
            }
            BodyOwner::ContractRecvArm { .. } => {
                self.register_contract_effect_bindings(base_assumptions)
            }
        }
    }

    fn register_func_effect_bindings(&mut self, func: Func<'db>, mode: Option<BorrowKind>) {
        for binding in func.effect_requirements(self.db, mode) {
            if !matches!(
                binding.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            ) {
                continue;
            }
            let idx = binding.binding_idx as usize;
            let Some(resolved_binding) =
                self.resolved_effect_binding(EffectParamSite::Func(func, mode), idx)
            else {
                continue;
            };
            self.register_var_in_current_scope(
                resolved_binding.requirement.binding_name,
                LocalBinding::effect_param(&resolved_binding),
            );
        }
        // A row that stays abstract may expand in an instance and renumber
        // the components after it, so components are only seeded when every
        // row expands.
        let rows = expand_rows(
            self.db,
            func.effect_requirements(self.db, mode),
            func.scope(),
            self.base_assumptions,
        );
        if !rows.abstract_rows.is_empty()
            || rows
                .components
                .iter()
                .any(|component| component.requirement.key.key_row().is_some())
        {
            return;
        }
        for component in rows.components {
            let requirement = component.requirement;
            let provider = row_effect_provider(self.db, self.body.scope(), None, &requirement);
            self.row_components.push(ResolvedEffectBindingInfo {
                requirement,
                provider,
            });
        }
    }

    /// The row component of the function body at `site` numbered `idx`.
    fn row_component(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<&ResolvedEffectBindingInfo<'db>> {
        self.row_components.iter().find(|component| {
            component.requirement.binding_site == site
                && component.requirement.binding_idx as usize == idx
        })
    }

    pub(super) fn row_components(&self) -> &[ResolvedEffectBindingInfo<'db>] {
        &self.row_components
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
            BodyOwner::Func(_, _)
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
        // A closure's row is the body's own, still being checked.
        if let EffectParamSite::Closure(def) = site {
            return self
                .closure_stack
                .iter()
                .find(|active| active.def == def)
                .map(|active| &active.effects)
                .or_else(|| self.closure_effects.get(&def.expr))?
                .get(idx)
                .cloned();
        }
        if let Some(component) = self.row_component(site, idx) {
            return Some(component.clone());
        }
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
            EffectParamSite::Func(func, _) => func.scope(),
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => contract.scope(),
            EffectParamSite::Closure(def) => def.body.scope(),
        }
    }

    fn resolved_effect_param_ty(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<TyId<'db>> {
        if let EffectParamSite::Closure(_) = site {
            return self
                .resolved_effect_binding(site, idx)?
                .requirement
                .key
                .binding_ty(self.db);
        }
        if let Some(component) = self.row_component(site, idx) {
            return match component.requirement.key {
                EffectRequirementKey::Trait(_) => Some(component.provider.provider_ty),
                _ => component.requirement.key.binding_ty(self.db),
            };
        }
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

    fn register_var_in_current_scope(&mut self, name: IdentId<'db>, binding: LocalBinding<'db>) {
        let block_idx = self.current_block_idx();
        self.var_env
            .last_mut()
            .expect("scope exists")
            .register_var(name, binding);
        self.binding_block_idx.insert(binding, block_idx);
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
            |expr| self.place_entries.contains(&expr),
        )
    }

    /// Returns `true` if `expr` is an assignable, borrowable location: a
    /// binding-rooted place, memory addressed by a pointer, or a place in a
    /// projection's grant.
    pub(super) fn is_place_expr(&self, expr: ExprId) -> bool {
        self.expr_place(expr).is_some()
            || self.is_pointer_place_expr(expr)
            || self.is_grant_place(expr)
    }

    pub(super) fn is_grant_place(&self, expr: ExprId) -> bool {
        is_grant_place_expr(self.db, self.body, expr, &|expr| {
            self.typed_expr(expr)
                .is_some_and(|prop| prop.access().is_some())
        })
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

    /// Calls the callee `expr` resolved to at `mode`.
    pub(super) fn set_call_mode(&mut self, expr: ExprId, mode: Option<BorrowKind>) {
        if let Some(callable) = &mut self.callables[expr] {
            callable.set_mode(mode);
        }
        if let Some(SemanticExprLowering::Call { callable }) =
            &mut self.semantic_expr_lowering[expr]
        {
            callable.set_mode(mode);
        }
    }

    pub(super) fn register_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) {
        if self.const_refs[expr].replace(const_ref).is_some() {
            panic!("const ref is already registered for the given expr")
        }
    }

    pub(super) fn value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.value_path_refs[expr]
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

    pub(super) fn register_for_loop_plan(&mut self, stmt: StmtId, plan: ForLoopPlan<'db>) {
        if self.for_loop_plans[stmt].replace(plan).is_some() {
            panic!("for loop seq is already registered for the given stmt")
        }
    }

    pub(super) fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables[expr].as_ref()
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
            BodyOwner::Func(func, _) => func.as_callable(self.db),
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
            BodyOwner::Func(func, _) => {
                let rt = func.return_ty(self.db);
                // A return reported where it is written checks nothing.
                if func.has_explicit_return_ty(self.db)
                    && (!rt.is_star_kind(self.db) || func.return_lacks_mode(self.db))
                {
                    TyId::invalid(self.db, InvalidCause::Other)
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
                if ty.is_star_kind(self.db) {
                    ty
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }
            BodyOwner::Closure { ty, .. } => ty.ret_ty(self.db),
        }
    }

    pub(super) fn param_bindings(&self) -> &[LocalBinding<'db>] {
        &self.param_bindings
    }

    /// The shape of `binding` if it is a product parameter's, or an opaque
    /// part of one that a destructured parameter binds whole.
    pub(super) fn param_product(&self, binding: &LocalBinding<'db>) -> Option<&Shape<'db>> {
        param_product(&self.param_products, &self.transport_parts, binding)
    }

    /// Records that the name `pat` binds an opaque part of a destructured
    /// parameter, of `shape`, whole: a transport binding, as an opaque
    /// parameter is.
    pub(super) fn register_transport_part(&mut self, pat: PatId, shape: Shape<'db>) {
        self.transport_parts.push((pat, shape));
    }

    /// Records that parameter `idx` of `site` is a product of `shape`.
    pub(super) fn register_param_product(
        &mut self,
        site: ParamSite<'db>,
        idx: usize,
        shape: Shape<'db>,
    ) {
        self.param_products.push((site, idx, shape));
    }

    pub(super) fn lookup_binding_ty(&self, binding: &LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self
                .pat_ty
                .get(*pat)
                .copied()
                .flatten()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param { ty, .. } | LocalBinding::ParamPart { ty, .. } => *ty,

            LocalBinding::EffectParam { site, idx, .. } => self
                .resolved_effect_param_ty(*site, *idx)
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),
        }
    }

    pub(super) fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings[pat]
    }

    pub(super) fn consume_access(&mut self, expr: ExprId) {
        self.consumed_accesses.insert(expr);
    }

    pub(super) fn is_consumed_access(&self, expr: ExprId) -> bool {
        self.consumed_accesses.contains(&expr)
    }

    pub(super) fn open_matched_place(
        &mut self,
        expr: ExprId,
        prop: ExprProp<'db>,
        inspected: bool,
    ) {
        self.matched_places.insert(expr);
        if inspected {
            self.inspected_places.insert(expr);
        }
        self.type_expr(expr, prop);
    }

    pub(super) fn is_matched_place(&self, expr: ExprId) -> bool {
        self.matched_places.contains(&expr)
    }

    pub(super) fn record_place_entry(&mut self, expr: ExprId) {
        self.place_entries.insert(expr);
    }

    pub(super) fn record_unsafe_split(&mut self, tuple: ExprId) {
        self.unsafe_splits.insert(tuple);
    }

    pub(super) fn record_product_arg(&mut self, tuple: ExprId) {
        self.product_args.insert(tuple);
    }

    pub(super) fn is_product_arg(&self, expr: ExprId) -> bool {
        self.product_args.contains(&expr)
    }

    pub(super) fn record_value_call(&mut self, call: ExprId) {
        self.value_calls.insert(call);
    }

    pub(super) fn is_value_call(&self, call: ExprId) -> bool {
        self.value_calls.contains(&call)
    }

    /// Records that the function's own authority covers the resources the
    /// `with` value `expr` names.
    pub(super) fn cover_provider(&mut self, expr: ExprId) {
        self.covered_providers.insert(expr);
    }

    pub(super) fn record_yield_shape(&mut self, expr: ExprId, shape: Shape<'db>) {
        self.yield_shapes[expr] = Some(shape);
    }

    pub(super) fn is_yield_site(&self, expr: ExprId) -> bool {
        self.yield_shapes.get(expr).is_some_and(Option::is_some)
    }

    pub(super) fn is_unsafe_split(&self, expr: ExprId) -> bool {
        self.unsafe_splits.contains(&expr)
    }

    pub(super) fn binding_access(&self, binding: &LocalBinding<'db>) -> Option<BindingAccess> {
        binding_access(binding, |pat| self.pat_binding_modes[pat])
    }

    pub(super) fn binding_has_authority(&self, binding: &LocalBinding<'db>) -> bool {
        binding_has_authority(binding, |pat| self.pat_binding_modes[pat])
    }

    pub(super) fn local_borrow_provider(&self, pat: PatId) -> Option<AddressSpace> {
        self.local_borrow_providers[pat]
    }

    pub(super) fn set_local_borrow_provider(&mut self, pat: PatId, provider: Option<AddressSpace>) {
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

    pub(super) fn call_effect_args_mut(
        &mut self,
        call_expr: ExprId,
    ) -> Option<&mut Vec<super::ResolvedEffectArg<'db>>> {
        self.call_effect_args[call_expr].as_mut()
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
            Partial::Present(Expr::Block(..)) => ScopeId::Block(self.body, block),
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

    /// Starts checking the body of a closure whose expression is in the
    /// current block. Its parameters are registered in a new scope.
    pub(super) fn enter_closure(&mut self, def: ClosureDef<'db>) {
        self.closure_stack.push(ActiveClosure {
            def,
            boundary_block_idx: self.current_block_idx(),
            enclosing_effects: std::mem::replace(
                &mut self.effect_env,
                keyed_effect_env::EffectEnv::new(),
            ),
            effects: Vec::new(),
            params: Vec::new(),
            captures: IndexMap::new(),
        });
        self.enter_lexical_scope();
    }

    pub(super) fn register_closure_param(
        &mut self,
        name: Option<IdentId<'db>>,
        binding: LocalBinding<'db>,
    ) {
        if let Some(active) = self.closure_stack.last_mut() {
            active.params.push(binding);
        }
        match name {
            Some(name) => self.register_var_in_current_scope(name, binding),
            None => {
                self.binding_block_idx
                    .insert(binding, self.current_block_idx());
            }
        }
    }

    /// Ends checking the innermost closure's body, with its parameters and
    /// captures. Moving a capture out of the closure's environment, in its
    /// body or into a closure nested in it, is recorded to be reported if
    /// the value is not `Copy`.
    pub(super) fn leave_closure(
        &mut self,
        expr: ExprId,
    ) -> (ClosureInfo<'db>, Vec<RowComponent<'db>>) {
        self.leave_scope();
        let active = self
            .closure_stack
            .pop()
            .expect("closure stack is non-empty");
        let env = std::mem::replace(&mut self.effect_env, active.enclosing_effects);
        self.closure_envs.insert(expr, env);
        let effects = active
            .effects
            .iter()
            .map(|effect| RowComponent {
                name: effect.requirement.binding_name,
                key: effect.requirement.key.clone(),
                is_mut: effect.requirement.is_mut,
                key_syntax: effect.requirement.binding_ty,
            })
            .collect();
        self.closure_effects.insert(expr, active.effects);
        let info = ClosureInfo {
            params: active.params,
            captures: active
                .captures
                .into_iter()
                .map(|(binding, ty)| ClosureCapture { binding, ty })
                .collect(),
        };
        (info, effects)
    }

    /// The component of the innermost closure's row keyed by `key`, added
    /// as `name` if the row has none, with its provider if it is new. A
    /// component required `mut` anywhere is `mut`. A new trait-keyed
    /// component's provider is assumed to implement the trait for the rest
    /// of the body; a row component has no provider. The row of the late
    /// closure, already checked, is fixed: it gives only a component it has,
    /// `mut` if `is_mut`.
    pub(super) fn closure_effect(
        &mut self,
        key: EffectRequirementKey<'db>,
        name: IdentId<'db>,
        key_syntax: HirTypeId<'db>,
        is_mut: bool,
    ) -> Option<(LocalBinding<'db>, Option<ProvidedEffect<'db>>)> {
        let db = self.db;
        // A trait-keyed component is keyed by its provider: `P: Trait`.
        let same_key = |held: &EffectRequirementKey<'db>| match (held, &key) {
            (EffectRequirementKey::Trait(held), EffectRequirementKey::Trait(key)) => {
                held.def(db) == key.def(db)
                    && held.args(db)[1..] == key.args(db)[1..]
                    && held.assoc_ty_bindings(db) == key.assoc_ty_bindings(db)
            }
            (held, key) => held == key,
        };
        if let Some((def, _)) = &self.late_closure {
            let effect = self.closure_effects[&def.expr]
                .iter()
                .find(|effect| same_key(&effect.requirement.key))?;
            return (!is_mut || effect.requirement.is_mut)
                .then(|| (LocalBinding::effect_param(effect), None));
        }
        let active = self.closure_stack.last_mut()?;
        if let Some(effect) = active
            .effects
            .iter_mut()
            .find(|effect| same_key(&effect.requirement.key))
        {
            effect.requirement.is_mut |= is_mut;
            effect.provider.is_mut |= is_mut;
            return Some((LocalBinding::effect_param(effect), None));
        }
        let requirement = EffectRequirement {
            binding_name: name,
            key,
            is_mut,
            binding_site: EffectParamSite::Closure(active.def),
            binding_idx: active.effects.len() as u32,
            binding_ty: key_syntax,
        };
        let mut info = ResolvedEffectBindingInfo {
            provider: row_effect_provider(
                db,
                active.def.body.scope(),
                Some(active.def.expr),
                &requirement,
            ),
            requirement,
        };
        let provider_ty = info.provider.provider_ty;
        // The body assumes its provider meets the key. The key keeps the
        // trait's own `Self` for its provider, for the row's to fill.
        let bound = info
            .requirement
            .key
            .key_trait()
            .map(|key| super::super::instantiate_trait_self(db, key, provider_ty));
        if let EffectRequirementKey::Trait(inst) = info.requirement.key {
            info.requirement.key = EffectRequirementKey::Trait(inst.with_self_ty(db, provider_ty));
        }
        let binding = LocalBinding::effect_param(&info);
        // A row is forwarded whole, never provided.
        let provided = match info.requirement.key {
            EffectRequirementKey::Trait(_) => Some(provider_ty),
            ref key => key.binding_ty(db),
        }
        .map(|ty| ProvidedEffect {
            origin: EffectOrigin::Param {
                site: info.requirement.binding_site,
                index: info.requirement.binding_idx as usize,
                name: Some(name),
            },
            ty,
            is_mut: true,
            binding: Some(binding),
        });
        active.effects.push(info);
        if let Some(bound) = bound {
            let mut preds = self.assumptions.list(db).to_vec();
            preds.push(bound);
            self.assumptions = PredicateListId::new(db, preds).extend_all_bounds(db);
        }
        Some((binding, provided))
    }

    /// Marks effect `idx` of `closure`'s row as used `mut`, returning whether
    /// it is: the row of a closure already checked is fixed.
    pub(super) fn require_mut_closure_effect(
        &mut self,
        closure: ClosureDef<'db>,
        idx: usize,
    ) -> bool {
        match self
            .closure_stack
            .iter_mut()
            .find(|active| active.def == closure)
        {
            Some(active) => {
                if let Some(effect) = active.effects.get_mut(idx) {
                    effect.requirement.is_mut = true;
                    effect.provider.is_mut = true;
                }
                true
            }
            None => self
                .closure_effects
                .get(&closure.expr)
                .and_then(|effects| effects.get(idx))
                .is_some_and(|effect| effect.requirement.is_mut),
        }
    }

    /// Where a call checked now is: see `CallSite`.
    pub(super) fn call_site(&self) -> CallSite<'db> {
        CallSite {
            frames: self.effect_env.with_frames(),
            closure: self.closure_stack.last().map(|active| active.def),
        }
    }

    /// Resolves what follows at `site`, a deferred call's, until
    /// `leave_call_site`: under its `with` blocks, in its closure's body. A
    /// closure already checked gives its finished environment, whose row is
    /// fixed.
    pub(super) fn enter_call_site(&mut self, site: &CallSite<'db>) {
        debug_assert!(
            site.closure.is_some() || self.closure_stack.is_empty(),
            "a call outside closures resolves after them"
        );
        if let Some(def) = site.closure
            && self
                .closure_stack
                .last()
                .is_none_or(|active| active.def != def)
        {
            let env = self
                .closure_envs
                .remove(&def.expr)
                .expect("a checked closure keeps its effect environment");
            let enclosing = std::mem::replace(&mut self.effect_env, env);
            self.late_closure = Some((def, enclosing));
        }
        self.effect_env.reopen_with_frames(site.frames.clone());
    }

    pub(super) fn leave_call_site(&mut self) {
        self.effect_env.close_with_frames();
        if let Some((def, enclosing)) = self.late_closure.take() {
            let env = std::mem::replace(&mut self.effect_env, enclosing);
            self.closure_envs.insert(def.expr, env);
        }
    }

    /// The checked closure a deferred call being resolved is in.
    pub(super) fn late_closure(&self) -> Option<ClosureDef<'db>> {
        self.late_closure.as_ref().map(|(def, _)| *def)
    }

    pub(super) fn in_closure(&self) -> bool {
        !self.closure_stack.is_empty()
    }

    /// Whether the body is a `Drop::drop`, which declares no effects.
    pub(super) fn in_drop(&self) -> bool {
        matches!(self.owner, BodyOwner::Func(func, _) if implements_drop(self.db, func))
    }

    pub(super) fn record_access_capture(
        &mut self,
        expr: ExprId,
        binding: LocalBinding<'db>,
        ty: TyId<'db>,
    ) {
        self.access_captures.push((expr, binding, ty));
    }

    pub(super) fn take_access_captures(&mut self) -> Vec<(ExprId, LocalBinding<'db>, TyId<'db>)> {
        std::mem::take(&mut self.access_captures)
    }

    /// Whether `binding`, used in the innermost closure being checked, is
    /// one of its captures.
    pub(super) fn binding_is_capture(&self, binding: LocalBinding<'db>) -> bool {
        self.closure_stack.last().is_some_and(|active| {
            self.binding_block_idx
                .get(&binding)
                .is_some_and(|&idx| idx <= active.boundary_block_idx)
        })
    }

    /// Records `binding`, of type `ty`, as a capture of every closure being
    /// checked that it is outside of.
    pub(super) fn record_capture(&mut self, binding: LocalBinding<'db>, ty: TyId<'db>) {
        let Some(&binding_block_idx) = self.binding_block_idx.get(&binding) else {
            return;
        };
        for active in &mut self.closure_stack {
            if binding_block_idx <= active.boundary_block_idx {
                active.captures.entry(binding).or_insert(ty);
            }
        }
    }

    pub(super) fn swap_loop_stack(&mut self, loop_stack: Vec<StmtId>) -> Vec<StmtId> {
        std::mem::replace(&mut self.loop_stack, loop_stack)
    }

    pub(super) fn expect_closure(&mut self, expr: ExprId, expectation: ClosureExpectation<'db>) {
        self.closure_expectations.insert(expr, expectation);
    }

    pub(super) fn take_closure_expectation(
        &mut self,
        expr: ExprId,
    ) -> Option<ClosureExpectation<'db>> {
        self.closure_expectations.remove(&expr)
    }

    pub(super) fn register_closure_info(&mut self, expr: ExprId, info: ClosureInfo<'db>) {
        self.closure_infos[expr] = Some(info);
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

    pub(super) fn pat_ty(&self, pat: PatId) -> Option<TyId<'db>> {
        self.pat_ty.get(pat).copied().flatten()
    }

    pub(super) fn alloc_validated_pat(&mut self, pat: ValidatedPat<'db>) -> ValidatedPatId {
        self.pattern_store.alloc(pat)
    }

    pub(super) fn set_pattern_status(&mut self, pat: PatId, status: PatternAnalysisStatus) {
        match status {
            PatternAnalysisStatus::Ready(root) => self.pattern_store.set_root(pat, root),
            PatternAnalysisStatus::Invalid => self.pattern_store.clear_root(pat),
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

    /// The binding `name` names at this point of the body, if a binding has
    /// it.
    pub(super) fn visible_binding(&self, name: IdentId<'db>) -> Option<LocalBinding<'db>> {
        (0..=self.current_block_idx())
            .rev()
            .find_map(|idx| self.get_block(idx).lookup_var(name))
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

    /// Whether a method call in `closure`'s body waits to resolve.
    pub(super) fn has_pending_method_in(&self, closure: ClosureDef<'db>) -> bool {
        self.deferred.iter().any(|task| {
            matches!(task, DeferredTask::Method(pending) if pending.site.closure == Some(closure))
        })
    }

    pub(super) fn register_pending_primitive_op(&mut self, pending: PendingPrimitiveOp) {
        self.deferred.push(DeferredTask::PrimitiveOp(pending))
    }

    pub(super) fn register_type_equality(&mut self, equality: TypeEquality<'db>) {
        self.deferred.push(DeferredTask::TypeEquality(equality))
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
        self.yield_shapes
            .values_mut()
            .flatten()
            .for_each(|shape| *shape = shape.clone().fold_with(self.db, &mut prober));

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
        let signature = self.signature.fold_with(self.db, &mut prober);
        let pattern_store = self.pattern_store.fold_with(self.db, &mut prober);

        self.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(self.db, &mut prober));
        self.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(self.db, &mut prober));

        self.for_loop_plans
            .values_mut()
            .flatten()
            .for_each(|plan| *plan = plan.clone().fold_with(self.db, &mut prober));
        // Each trait goal a call or constant uses is now final: prove it once,
        // here, for every later stage.
        let solve_cx = TraitSolveCx::new(self.db, prober.scope).with_assumptions(assumptions);
        for lowering in self.semantic_expr_lowering.values_mut().flatten() {
            let callable = lowering.callable_mut();
            callable.record_evidence(self.db, solve_cx);
            callable.record_bound_evidence(self.db, solve_cx);
        }
        for plan in self.for_loop_plans.values_mut().flatten() {
            for call in &mut plan.calls {
                call.callable.record_evidence(self.db, solve_cx);
                call.callable.record_bound_evidence(self.db, solve_cx);
            }
        }
        for const_ref in self.const_refs.values_mut().flatten() {
            if let ConstRef::TraitConst { use_, evidence, .. } = const_ref {
                *evidence = prove(self.db, use_.solve_cx(self.db), use_.inst());
            }
            const_ref.record_bound_evidence(self.db);
        }
        self.closure_infos
            .values_mut()
            .flatten()
            .for_each(|info| *info = info.clone().fold_with(self.db, &mut prober));
        self.path_applications
            .iter_mut()
            .for_each(|(_, ty)| *ty = ty.fold_with(self.db, &mut prober));
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
                |expr| self.place_entries.contains(&expr),
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
            result_ty,
            signature,
            assumptions,
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            yield_shapes: self.yield_shapes,
            place_entries: self.place_entries,
            unsafe_splits: self.unsafe_splits,
            product_args: self.product_args,
            value_calls: self.value_calls,
            matched_places: self.matched_places,
            inspected_places: self.inspected_places,
            covered_providers: self.covered_providers,
            const_refs: self.const_refs,
            value_path_refs: self.value_path_refs,
            semantic_expr_lowering: self.semantic_expr_lowering,
            record_init_lowering: self.record_init_lowering,
            closure_infos: self.closure_infos,
            resolved_field_index: self.resolved_field_index,
            call_effect_args: self.call_effect_args,
            return_borrow_provider: None,
            param_bindings: self.param_bindings,
            input_bindings: self.input_bindings,
            param_products: self.param_products,
            transport_parts: self.transport_parts,
            pat_bindings: self.pat_bindings,
            pat_binding_modes: self.pat_binding_modes,
            pattern_store,
            pattern_status: self.pattern_status,
            for_loop_plans: self.for_loop_plans,
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
            BodyOwner::Func(func, mode) => self.seed_func_effect_witnesses(func, mode),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
            BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                self.seed_contract_effect_witnesses();
            }
        }
    }

    fn seed_func_effect_witnesses(&mut self, func: Func<'db>, mode: Option<BorrowKind>) {
        let assumptions = self.env.base_assumptions();
        let site = EffectParamSite::Func(func, mode);

        for binding in func.effect_requirements(self.db, mode) {
            if !matches!(
                binding.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            ) {
                continue;
            }

            let idx = binding.binding_idx as usize;
            let resolved_binding = self
                .env
                .resolved_effect_binding(site, idx)
                .unwrap_or_else(|| panic!("missing provider binding for effect at index {idx}"));
            let local_binding = LocalBinding::effect_param(&resolved_binding);
            let provided = ProvidedEffect {
                origin: EffectOrigin::Param {
                    site,
                    index: idx,
                    name: Some(resolved_binding.requirement.binding_name),
                },
                ty: EffectEnvView::new(site)
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

        for component in self.env.row_components().to_vec() {
            let requirement = &component.requirement;
            let local_binding = LocalBinding::effect_param(&component);
            let provided = ProvidedEffect {
                origin: EffectOrigin::Param {
                    site,
                    index: requirement.binding_idx as usize,
                    name: Some(requirement.binding_name),
                },
                ty: self.env.lookup_binding_ty(&local_binding),
                is_mut: local_binding.is_mut(),
                binding: Some(local_binding),
            };
            if let Some(req) = EffectRequirementDecl::from_effect_requirement(self.db, requirement)
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
        let span = provider.origin.span(self.db, self.body());
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum EffectParamSite<'db> {
    /// A function's effects, at the mode of the body that binds them (`None`
    /// for a function without one).
    Func(Func<'db>, Option<BorrowKind>),
    Contract(Contract<'db>),
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
    /// A closure's row: the effects its body uses that its own `with`
    /// blocks do not provide.
    Closure(ClosureDef<'db>),
}

impl<'db> EffectParamSite<'db> {
    /// The site naming effect slot `idx` of this one, as a provider source:
    /// both bodies of a `mut?` function share a slot whose effect is not
    /// `mut?`, so that what they pass it to is the same instance.
    pub fn slot_site(self, db: &'db dyn HirAnalysisDb, idx: u32) -> Self {
        match self {
            Self::Func(func, Some(_))
                if func
                    .effects(db)
                    .data(db)
                    .get(idx as usize)
                    .is_none_or(|effect| effect.mode != ModeMarker::MaybeMut) =>
            {
                Self::Func(func, None)
            }
            site => site,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ParamSite<'db> {
    Func(Func<'db>),
    ContractInit(Contract<'db>),
    /// A closure's parameter.
    Closure(ClosureDef<'db>),
    /// A closure body's environment: the closure value, its `self`.
    ClosureEnv(ClosureDef<'db>),
    /// Effect param that resolves to a contract field.
    EffectField(EffectParamSite<'db>),
}

fn param_span<'db>(
    db: &'db dyn HirAnalysisDb,
    site: ParamSite<'db>,
    idx: usize,
) -> DynLazySpan<'db> {
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
        ParamSite::EffectField(effect_site) => effect_param_span(db, effect_site, idx),
    }
}

fn param_name<'db>(
    db: &'db dyn HirAnalysisDb,
    site: ParamSite<'db>,
    idx: usize,
) -> Option<IdentId<'db>> {
    match site {
        ParamSite::ClosureEnv(_) => Some(IdentId::make_self(db)),
        ParamSite::EffectField(effect_site) => effect_param_name(db, effect_site, idx),
        _ => hir_param(db, site, idx)?.name(),
    }
}

/// The shape of `binding` among `products` if it is a product parameter's,
/// the product its body destructures, or among `parts` if it binds an opaque
/// part of one whole.
pub(super) fn param_product<'a, 'db>(
    products: &'a [(ParamSite<'db>, usize, Shape<'db>)],
    parts: &'a [(PatId, Shape<'db>)],
    binding: &LocalBinding<'db>,
) -> Option<&'a Shape<'db>> {
    match binding {
        LocalBinding::Param { site, idx, .. } => {
            products.iter().find_map(|(param_site, param, product)| {
                (param_site == site && param == idx).then_some(product)
            })
        }
        LocalBinding::Local { pat, .. } => parts
            .iter()
            .find_map(|(part, shape)| (part == pat).then_some(shape)),
        LocalBinding::ParamPart { .. } | LocalBinding::EffectParam { .. } => None,
    }
}

/// The inputs a call passes parameter `idx` of `site`, a product of shape
/// `product`: one for each of its parts.
pub(super) fn product_inputs<'db>(
    db: &'db dyn HirAnalysisDb,
    site: ParamSite<'db>,
    idx: usize,
    product: &Shape<'db>,
) -> impl Iterator<Item = LocalBinding<'db>> {
    product
        .leaves(db)
        .into_iter()
        .zip(0..)
        .map(move |((access, ty), part)| LocalBinding::ParamPart {
            site,
            idx,
            part,
            access,
            ty,
        })
}

/// The bindings of a function's parameters at a mode.
#[derive(Default)]
pub(super) struct FuncParamBindings<'db> {
    /// One per parameter: a product parameter's is the product its body
    /// destructures.
    pub(super) params: Vec<LocalBinding<'db>>,
    /// The inputs a call passes the function: each parameter, or each part
    /// of a product parameter.
    pub(super) inputs: Vec<LocalBinding<'db>>,
    /// The shapes of the product parameters, by site and index.
    pub(super) products: Vec<(ParamSite<'db>, usize, Shape<'db>)>,
}

/// The bindings of `func`'s parameters where it is used at `mode`.
pub(super) fn func_param_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    mode: Option<BorrowKind>,
) -> FuncParamBindings<'db> {
    let arg_tys = func.arg_tys(db);
    let site = ParamSite::Func(func);
    let mut bindings = FuncParamBindings::default();
    for (idx, view) in func.params(db).enumerate() {
        let mut ty = *arg_tys
            .get(idx)
            .map_or(&TyId::invalid(db, InvalidCause::ParseError), |b| {
                b.skip_binder()
            });
        if !ty.is_star_kind(db) {
            ty = TyId::invalid(db, InvalidCause::Other);
        }
        // A shape whose impl is known is the parts it lists (`Self::Step` in
        // an impl).
        let product = view
            .product(db, mode)
            .map(|product| normalize_ty(db, product.clone(), func.scope(), func.assumptions(db)));
        // A product parameter's binding owns the carriers of its parts.
        let param = LocalBinding::Param {
            site,
            idx,
            mode: if product.is_some() {
                FuncParamMode::Own
            } else {
                view.mode_at(db, mode)
            },
            ty,
            is_mut: view.is_mut(db),
        };
        bindings.params.push(param);
        match product {
            Some(product) => {
                bindings
                    .inputs
                    .extend(product_inputs(db, site, idx, &product));
                bindings.products.push((site, idx, product));
            }
            // A spread's inputs are an instance's: those its shape lists.
            None if view.hir(db).is_some_and(FuncParam::is_spread) => {}
            None => bindings.inputs.push(param),
        }
    }
    bindings
}

/// The declaration of parameter `idx` of `site`: a function's, a contract
/// `init`'s or a closure's.
pub(crate) fn hir_param<'db>(
    db: &'db dyn HirAnalysisDb,
    site: ParamSite<'db>,
    idx: usize,
) -> Option<&'db FuncParam<'db>> {
    let params = match site {
        ParamSite::Func(func) => return func.params(db).nth(idx)?.hir(db),
        ParamSite::ContractInit(contract) => contract.init(db)?.params(db),
        ParamSite::Closure(def) => match def.expr.data(db, def.body) {
            Partial::Present(Expr::Closure { params, .. }) => *params,
            _ => return None,
        },
        ParamSite::ClosureEnv(_) | ParamSite::EffectField(_) => return None,
    };
    params.data(db).get(idx)
}

fn effect_param_name<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    idx: usize,
) -> Option<IdentId<'db>> {
    match site {
        EffectParamSite::Func(func, _) => func.effect_params(db).nth(idx).and_then(|p| p.name(db)),
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
        EffectParamSite::Closure(def) => closure_effect_requirements(db, def)
            .get(idx)
            .map(|requirement| requirement.binding_name),
    }
}

pub(super) fn effect_param_span<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    idx: usize,
) -> DynLazySpan<'db> {
    match site {
        EffectParamSite::Func(func, _) => func
            .span()
            .effects()
            .param_idx(func.effect_origin(db, idx))
            .into(),
        EffectParamSite::Contract(contract) => contract.span().effects().param_idx(idx).into(),
        EffectParamSite::ContractInit { contract } => {
            contract.span().init_block().effects().param_idx(idx).into()
        }
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
            .into(),
        EffectParamSite::Closure(def) => def.expr.span(def.body).into(),
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

impl<'db> EffectOrigin<'db> {
    /// Where the effect is given: a `with` value or an effect parameter.
    pub(super) fn span(self, db: &'db dyn HirAnalysisDb, body: Body<'db>) -> DynLazySpan<'db> {
        match self {
            Self::Param { site, index, .. } => effect_param_span(db, site, index),
            Self::With { value_expr } => value_expr.span(body).into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct ExprProp<'db> {
    pub ty: TyId<'db>,
    /// Whether the place this expression names is writable.
    pub is_mut: bool,
    pub binding: Option<LocalBinding<'db>>,
    pub borrow_provider: Option<AddressSpace>,
    pub path_read_semantics: Option<PathReadSemantics>,
    /// The accesses this expression grants: `Access` for `ref p`/`mut p`,
    /// the instantiated return shape for a projection call. `None` for a
    /// value or a place.
    pub shape: Option<Shape<'db>>,
}

impl<'db> ExprProp<'db> {
    pub(super) fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
            borrow_provider: None,
            path_read_semantics: None,
            shape: None,
        }
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(TyId::invalid(db, InvalidCause::Other), true)
    }

    /// The kind of the single access this expression grants, if it is an
    /// access (`ref p`, `mut p`, or a call to a projection returning
    /// `ref T`/`mut T`).
    pub fn access(&self) -> Option<BorrowKind> {
        match self.shape {
            Some(Shape::Access(kind, _)) => Some(kind),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum PathReadSemantics {
    ReuseLocal,
    ForwardInterface,
    MaterializeValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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
    /// A part of a product parameter (`_ (i, x): (own usize, ref T)`), which
    /// a call passes as an input of its own: the `part`th leaf of parameter
    /// `idx`'s shape, an `access` of the caller's place, or owned.
    ParamPart {
        site: ParamSite<'db>,
        idx: usize,
        part: u16,
        access: Option<BorrowKind>,
        ty: TyId<'db>,
    },
    EffectParam {
        site: EffectParamSite<'db>,
        idx: usize,
        binding_name: IdentId<'db>,
        provider_idx: u32,
        is_mut: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum PatBindingMode {
    ByValue,
    /// The binding is a named access to a place, open until its last use,
    /// with the effect authority of the place it names (`binding_has_authority`).
    Access {
        kind: BorrowKind,
        authority: bool,
    },
}

/// How a binding refers to its value: an owned value has no access; a view
/// parameter reads the caller's place; `ref`/`mut` bindings and `mut`
/// parameters are accesses of their kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum BindingAccess {
    View,
    Ref,
    Mut,
}

impl BindingAccess {
    pub fn is_mut(self) -> bool {
        self == Self::Mut
    }
}

pub(crate) fn binding_access<'db>(
    binding: &LocalBinding<'db>,
    pat_mode: impl FnOnce(PatId) -> Option<PatBindingMode>,
) -> Option<BindingAccess> {
    match binding {
        LocalBinding::Local { pat, .. } => match pat_mode(*pat)? {
            PatBindingMode::ByValue => None,
            PatBindingMode::Access {
                kind: BorrowKind::Ref,
                ..
            } => Some(BindingAccess::Ref),
            PatBindingMode::Access {
                kind: BorrowKind::Mut,
                ..
            } => Some(BindingAccess::Mut),
        },
        LocalBinding::Param { mode, .. } => match mode {
            FuncParamMode::View => Some(BindingAccess::View),
            FuncParamMode::Mut => Some(BindingAccess::Mut),
            FuncParamMode::Own => None,
        },
        LocalBinding::ParamPart { access, .. } => access.map(|access| match access {
            BorrowKind::Ref => BindingAccess::Ref,
            BorrowKind::Mut => BindingAccess::Mut,
        }),
        LocalBinding::EffectParam { .. } => None,
    }
}

/// Whether a place rooted in `binding` lies in an effect provider, and so
/// carries its authority: an effect binding, or an access to such a place.
/// A data parameter carries none: its caller's place may be a copy.
pub(crate) fn binding_has_authority<'db>(
    binding: &LocalBinding<'db>,
    pat_mode: impl FnOnce(PatId) -> Option<PatBindingMode>,
) -> bool {
    match binding {
        LocalBinding::EffectParam { .. }
        | LocalBinding::Param {
            site: ParamSite::EffectField(_),
            ..
        } => true,
        LocalBinding::Param { .. } | LocalBinding::ParamPart { .. } => false,
        LocalBinding::Local { pat, .. } => matches!(
            pat_mode(*pat),
            Some(PatBindingMode::Access {
                authority: true,
                ..
            })
        ),
    }
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
            LocalBinding::ParamPart { .. } => false,
        }
    }

    pub fn callable_input_origin(self, db: &'db dyn HirAnalysisDb) -> Option<CallableInputOrigin> {
        match self {
            Self::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            } => Some(if func.is_method(db) && idx == 0 {
                CallableInputOrigin::Receiver
            } else {
                CallableInputOrigin::ValueParam(idx)
            }),
            Self::Param {
                site: ParamSite::EffectField(_),
                idx,
                ..
            }
            | Self::EffectParam { idx, .. } => Some(CallableInputOrigin::Effect(idx)),
            // A part is never named: the product it is a part of is what
            // the body reads.
            Self::Local { .. }
            | Self::ParamPart { .. }
            | Self::Param {
                site: ParamSite::ContractInit(_) | ParamSite::Closure(_) | ParamSite::ClosureEnv(_),
                ..
            } => None,
        }
    }

    /// The first parameter of a closure body: the closure value itself, its
    /// environment of captures, viewed or `mut` as the callable trait's
    /// receiver is. The body's lowering and its callers must agree on it, so
    /// it is constructed only here.
    pub fn closure_env(
        db: &'db dyn HirAnalysisDb,
        closure: ClosureTy<'db>,
        receiver: FuncParamMode,
    ) -> Self {
        Self::Param {
            site: ParamSite::ClosureEnv(closure.def(db)),
            idx: 0,
            mode: receiver,
            ty: TyId::closure(db, closure),
            is_mut: false,
        }
    }

    pub(crate) fn effect_param(binding: &ResolvedEffectBindingInfo<'db>) -> Self {
        Self::effect(&binding.requirement, binding.provider.provider_idx)
    }

    /// The binding of `requirement`, provided by `provider_idx`. A closure's
    /// effects are mutable bindings whatever its row requires, which its
    /// body's uses decide only once they are all checked.
    pub(crate) fn effect(requirement: &EffectRequirement<'db>, provider_idx: u32) -> Self {
        let site = requirement.binding_site;
        Self::EffectParam {
            site,
            idx: requirement.binding_idx as usize,
            binding_name: requirement.binding_name,
            provider_idx,
            is_mut: requirement.is_mut || matches!(site, EffectParamSite::Closure(_)),
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
            Self::Param { site, idx, .. } | Self::ParamPart { site, idx, .. } => {
                param_name(env.db, *site, *idx)
                    .unwrap_or_else(|| IdentId::new(env.db, "_".to_string()))
            }
            Self::EffectParam { binding_name, .. } => *binding_name,
        }
    }

    pub(super) fn def_span(&self, env: &TyCheckEnv<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(env.body).into(),
            LocalBinding::Param { site, idx, .. } | LocalBinding::ParamPart { site, idx, .. } => {
                param_span(env.db, *site, *idx)
            }
            LocalBinding::EffectParam { site, idx, .. } => effect_param_span(env.db, *site, *idx),
        }
    }

    /// Get the definition span for this binding, given the body and function directly.
    ///
    /// This is used by `TypedBody::expr_binding_def_span` to get the definition
    /// span without needing a full `TyCheckEnv`.
    pub(super) fn def_span_with(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        _func: Func<'db>,
    ) -> DynLazySpan<'db> {
        self.def_span_in_body(db, body)
    }

    /// Get the definition span for this binding given just the body.
    pub(crate) fn def_span_in_body(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
    ) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(body).into(),
            LocalBinding::Param { site, idx, .. } | LocalBinding::ParamPart { site, idx, .. } => {
                param_span(db, *site, *idx)
            }
            LocalBinding::EffectParam { site, idx, .. } => effect_param_span(db, *site, *idx),
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
        if let TyData::ConstTy(const_ty) = ty.data(db)
            && let ConstTyData::TyVar(var, len_ty) = const_ty.data(db)
            && let Some(TyVarSort::StringLen { min_len }) = self.table.unbound_sort(var.key)
        {
            let len = const_ty_from_sem_const(db, int_const(db, *len_ty, BigInt::from(min_len)));
            return TyId::const_ty(db, len);
        }
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
    pub site: CallSite<'db>,
}

/// Where a call is checked, for one resolved later to take its effects
/// there: the frames of the `with` blocks it is under, and the closure whose
/// body it is in.
#[derive(Debug, Clone)]
pub(super) struct CallSite<'db> {
    frames: Vec<keyed_effect_env::EffectFrame<'db>>,
    pub closure: Option<ClosureDef<'db>>,
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
    TypeEquality(TypeEquality<'db>),
}

#[derive(Debug, Clone)]
pub(super) struct TypeEquality<'db> {
    pub actual: TyId<'db>,
    pub expected: TyId<'db>,
    pub span: DynLazySpan<'db>,
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
