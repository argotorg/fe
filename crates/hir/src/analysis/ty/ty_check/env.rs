use crate::{
    hir_def::{
        Body, BodyKind, Expr, ExprId, Func, IdentId, IntegerId, Partial, Pat, PatId, Stmt, StmtId,
        prim_ty::PrimTy, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use salsa::Update;
use thin_vec::ThinVec;

use super::{Callable, TypedBody};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        canonical::{Canonical, Canonicalized},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
        fold::{AssocTySubst, TyFoldable, TyFolder},
        func_def::{FuncDef, lower_func},
        normalize::normalize_ty,
        trait_def::TraitInstId,
        trait_resolution::{
            GoalSatisfiability, PredicateListId, constraint::collect_func_def_constraints,
            is_goal_satisfiable,
        },
        ty_def::{InvalidCause, TyBase, TyData, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
};

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,

    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,

    deferred: Vec<DeferredTask<'db>>,

    var_env: Vec<BlockEnv<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
    expr_stack: Vec<ExprId>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Result<Self, ()> {
        let Some(body) = func.body(db) else {
            return Err(());
        };

        let mut env = Self {
            db,
            body,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            callables: FxHashMap::default(),
            deferred: Vec::new(),
            var_env: vec![BlockEnv::new(func.scope(), 0)],
            pending_vars: FxHashMap::default(),
            loop_stack: Vec::new(),
            expr_stack: Vec::new(),
        };

        env.enter_scope(body.expr(db));

        let Some(params) = func.params(db).to_opt() else {
            return Err(());
        };

        for (idx, param) in params.data(db).iter().enumerate() {
            let Some(name) = param.name() else {
                continue;
            };

            let mut ty = match param.ty {
                Partial::Present(hir_ty) => {
                    lower_hir_ty(db, hir_ty, func.scope(), env.assumptions())
                }
                Partial::Absent => TyId::invalid(db, InvalidCause::ParseError),
            };

            if !ty.is_star_kind(db) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }
            let var = LocalBinding::Param {
                idx,
                ty,
                is_mut: param.is_mut,
            };

            env.var_env.last_mut().unwrap().register_var(name, var);
        }

        Ok(env)
    }

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<ExprProp<'db>> {
        self.expr_ty.get(&expr).copied()
    }

    pub(super) fn binding_def_span(&self, binding: LocalBinding<'db>) -> DynLazySpan<'db> {
        binding.def_span(self)
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables.insert(expr, callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }
    pub(super) fn binding_name(&self, binding: LocalBinding<'db>) -> IdentId<'db> {
        binding.binding_name(self)
    }

    /// Returns a function if the `body` being checked has `BodyKind::FuncBody`.
    /// If the `body` has `BodyKind::Anonymous`, returns None
    pub(super) fn func(&self) -> Option<FuncDef<'db>> {
        let func = self.hir_func()?;

        lower_func(self.db, func)
    }

    fn hir_func(&self) -> Option<Func<'db>> {
        match self.body.body_kind(self.db) {
            BodyKind::FuncBody => self.var_env.first()?.scope.item().try_into().ok(),
            BodyKind::Anonymous => None,
        }
    }

    pub(super) fn assumptions(&self) -> PredicateListId<'db> {
        match self.hir_func() {
            Some(func) => {
                // Include all implied bounds (super traits and associated type bounds)
                collect_func_def_constraints(self.db, func.into(), true)
                    .instantiate_identity()
                    .extend_all_bounds(self.db)
            }
            None => PredicateListId::empty_list(self.db),
        }
    }

    pub(super) fn body(&self) -> Body<'db> {
        self.body
    }

    pub(super) fn lookup_binding_ty(&self, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self
                .pat_ty
                .get(&pat)
                .copied()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param { ty, .. } => ty,
        }
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
            .for_each(|ty| *ty = ty.fold_with(self.db, &mut prober));

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
            let trait_method = inst.def(db).methods(db).get(&method_name).unwrap();
            let func_ty = trait_method.instantiate_with_inst(prober.table, recv_ty, inst);
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
        self.binding
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
pub(crate) enum LocalBinding<'db> {
    Local {
        pat: PatId,
        is_mut: bool,
    },
    Param {
        idx: usize,
        ty: TyId<'db>,
        is_mut: bool,
    },
}

impl<'db> LocalBinding<'db> {
    pub(super) fn local(pat: PatId, is_mut: bool) -> Self {
        Self::Local { pat, is_mut }
    }

    pub(super) fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. } | LocalBinding::Param { is_mut, .. } => *is_mut,
        }
    }

    pub(super) fn binding_name(&self, env: &TyCheckEnv<'db>) -> IdentId<'db> {
        let hir_db = env.db;
        match self {
            Self::Local { pat, .. } => {
                let Partial::Present(Pat::Path(Partial::Present(path), ..)) =
                    pat.data(hir_db, env.body())
                else {
                    unreachable!();
                };
                path.ident(hir_db).unwrap()
            }

            Self::Param { idx, .. } => {
                let func = env.func().unwrap();
                let Partial::Present(func_params) =
                    func.hir_func_def(env.db).unwrap().params(hir_db)
                else {
                    unreachable!();
                };

                func_params.data(hir_db)[*idx].name().unwrap()
            }
        }
    }

    fn def_span(&self, env: &TyCheckEnv<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.span(env.body).into(),
            LocalBinding::Param { idx, .. } => {
                let hir_func = env.func().unwrap().hir_func_def(env.db).unwrap();
                hir_func.span().params().param(*idx).name().into()
            }
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
