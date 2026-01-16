mod callable;
mod contract;
mod env;
mod expr;
mod owner;
mod pat;
mod path;
mod stmt;

pub use self::contract::{
    ResolvedRecvVariant, VariantResError, check_contract_init_body, check_contract_recv_arm_body,
    check_contract_recv_block, check_contract_recv_blocks, resolve_variant_bare,
    resolve_variant_in_msg,
};
pub use self::path::RecordLike;
use crate::analysis::name_resolution::resolve_path;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::visitor::TyVisitable;
use crate::hir_def::CallableDef;
use crate::{
    hir_def::{
        Body, Const, Contract, ContractRecvArm, Expr, ExprId, Func, IdentId, LitKind, Partial, Pat,
        PatId, PathId, StmtId, TypeId as HirTyId,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
pub use callable::Callable;
use env::TyCheckEnv;
pub use env::{EffectParamSite, ExprProp, LocalBinding, ParamSite};
pub(super) use expr::TraitOps;
pub use owner::BodyOwner;
pub use owner::EffectParamOwner;
pub use stmt::ForLoopSeq;

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::analysis::place::Place;

use super::{
    canonical::{Canonical, Canonicalized},
    diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    trait_def::TraitInstId,
    trait_resolution::{GoalSatisfiability, PredicateListId, is_goal_satisfiable},
    ty_def::{InvalidCause, Kind, TyId, TyVarSort},
    ty_lower::lower_hir_ty,
    unify::{InferenceKey, UnificationError, UnificationTable},
};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::{
    fold::AssocTySubst, normalize::normalize_ty, ty_error::collect_ty_lower_errors,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        PathRes, PathResError, diagnostics::PathResDiag, resolve_path_with_observer,
    },
    ty::ty_def::{TyFlags, inference_keys},
};

#[salsa::tracked(return_ref)]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Func(func))
}

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (Vec::new(), TypedBody::empty());
    };

    checker.run();
    checker.finish()
}

pub struct TyChecker<'db> {
    db: &'db dyn HirAnalysisDb,
    env: TyCheckEnv<'db>,
    table: UnificationTable<'db>,
    expected: TyId<'db>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> TyChecker<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let env = TyCheckEnv::new(db, owner)?;
        let expected = env.compute_expected_return();

        Ok(Self::new_internal(db, env, expected))
    }

    fn run(&mut self) {
        self.check_effect_param_keys_resolve();

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

        let root_expr = self.env.body().expr(self.db);
        self.check_expr(root_expr, self.expected);
    }

    fn check_effect_param_keys_resolve(&mut self) {
        match self.env.owner() {
            owner @ BodyOwner::Func(func) => {
                if let Some(crate::hir_def::ItemKind::Contract(contract)) =
                    func.scope().parent_item(self.db)
                {
                    self.check_contract_scoped_effect_list(owner, contract, func.effects(self.db));
                } else {
                    self.check_free_func_effect_list(func, func.effects(self.db));
                }
            }
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
            let Some(key_path) = effect.key_path.to_opt() else {
                continue;
            };

            if resolve_path(
                self.db,
                key_path,
                func.scope(),
                self.env.assumptions(),
                false,
            )
            .is_err()
            {
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
        let owner = match owner {
            BodyOwner::Func(func) => EffectParamOwner::Func(func),
            BodyOwner::ContractInit { contract } => EffectParamOwner::ContractInit { contract },
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => EffectParamOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            },
        };
        let contract_effect_names: FxHashSet<_> = contract
            .effects(self.db)
            .data(self.db)
            .iter()
            .filter_map(|e| e.name)
            .collect();
        let contract_field_names: FxHashSet<_> = crate::hir_def::FieldParent::Contract(contract)
            .fields(self.db)
            .filter_map(|f| f.name(self.db))
            .collect();

        let assumptions = PredicateListId::empty_list(self.db);
        let root_effect_ty =
            super::resolve_default_root_effect_ty(self.db, contract.scope(), assumptions);
        let contract_ingot = contract.top_mod(self.db).ingot(self.db);

        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect.key_path.to_opt() else {
                continue;
            };

            // Labeled effects are always type/trait keyed: `name: Type`.
            if effect.name.is_some() {
                let resolved = resolve_path(
                    self.db,
                    key_path,
                    contract.scope(),
                    self.env.assumptions(),
                    false,
                );
                match resolved {
                    Ok(PathRes::Trait(trait_inst)) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req =
                            super::instantiate_trait_self(self.db, trait_inst, root_effect_ty);
                        let goal = Canonicalized::new(self.db, trait_req).value;
                        if matches!(
                            is_goal_satisfiable(self.db, contract_ingot, goal, assumptions),
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
                    Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
                        let given = normalize_ty(self.db, ty, contract.scope(), assumptions);
                        if !given.is_zero_sized(self.db) {
                            self.push_diag(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner,
                                key: key_path,
                                idx,
                                given,
                            });
                        }
                    }
                    Ok(_) | Err(_) => self.push_diag(BodyDiag::InvalidEffectKey {
                        owner,
                        key: key_path,
                        idx,
                    }),
                }
                continue;
            }

            // Unlabeled contract-scoped effects refer to a contract field name or an
            // existing named contract effect (e.g. `ctx`).
            let Some(ident) = key_path.ident(self.db).to_opt() else {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner,
                    key: key_path,
                    idx,
                });
                continue;
            };

            if key_path.len(self.db) != 1
                || (!contract_effect_names.contains(&ident)
                    && !contract_field_names.contains(&ident))
            {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner,
                    key: key_path,
                    idx,
                });
            }
        }
    }

    fn finish(self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        TyCheckerFinalizer::new(self).finish()
    }

    fn resolve_deferred(&mut self) {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let ingot = body.top_mod(db).ingot(db);

        let is_viable = |this: &mut Self,
                         pending: &env::PendingMethod<'db>,
                         expr_ty: TyId<'db>,
                         receiver: ExprId,
                         generic_args: crate::hir_def::GenericArgListId<'db>,
                         call_args: &[crate::hir_def::CallArg<'db>],
                         inst: TraitInstId<'db>| {
            let snap = this.table.snapshot();

            let result = (|| {
                let recv_ty = {
                    let mut prober = env::Prober::new(&mut this.table);
                    pending.recv_ty.fold_with(db, &mut prober)
                };

                let inst_self = this.table.instantiate_to_term(inst.self_ty(db));
                this.table.unify(inst_self, recv_ty).ok()?;

                let trait_method = *inst.def(db).method_defs(db).get(&pending.method_name)?;
                let func_ty =
                    instantiate_trait_method(db, trait_method, &mut this.table, recv_ty, inst);
                let func_ty = this.table.instantiate_to_term(func_ty);
                let callable =
                    Callable::new(db, func_ty, receiver.span(body).into(), Some(inst)).ok()?;

                let expected_arity = callable.callable_def.arg_tys(db).len();
                let given_arity = call_args.len() + 1;
                if expected_arity != given_arity {
                    return None;
                }

                if generic_args.is_given(db) {
                    let given_args = crate::analysis::ty::ty_lower::lower_generic_arg_list(
                        db,
                        generic_args,
                        scope,
                        assumptions,
                    );
                    let offset = callable.callable_def.offset_to_explicit_params_position(db);
                    let current_args = &callable.generic_args()[offset..];
                    if current_args.len() != given_args.len() {
                        return None;
                    }
                    for (&given, &current) in given_args.iter().zip(current_args.iter()) {
                        this.table.unify(given, current).ok()?;
                    }
                }

                let receiver_prop = this.env.typed_expr(receiver)?;
                let mut all_arg_tys = Vec::with_capacity(call_args.len() + 1);
                all_arg_tys.push(receiver_prop.ty);
                for arg in call_args.iter() {
                    let prop = this.env.typed_expr(arg.expr)?;
                    all_arg_tys.push(prop.ty);
                }

                for (&given, expected) in all_arg_tys
                    .iter()
                    .zip(callable.callable_def.arg_tys(db).iter())
                {
                    let mut expected = expected.instantiate(db, callable.generic_args());
                    if let Some(inst) = callable.trait_inst() {
                        let mut subst = AssocTySubst::new(inst);
                        expected = expected.fold_with(db, &mut subst);
                    }
                    let expected = normalize_ty(
                        db,
                        expected.fold_with(db, &mut this.table),
                        scope,
                        assumptions,
                    );
                    let given =
                        normalize_ty(db, given.fold_with(db, &mut this.table), scope, assumptions);
                    this.table.unify(given, expected).ok()?;
                }

                let ret_ty = normalize_ty(
                    db,
                    callable.ret_ty(db).fold_with(db, &mut this.table),
                    scope,
                    assumptions,
                );
                this.table.unify(expr_ty, ret_ty).ok()?;

                Some(())
            })()
            .is_some();

            this.table.rollback_to(snap);
            result
        };

        // Fixed-point pass over deferred tasks.
        let mut progressed = true;
        while progressed {
            progressed = false;
            let tasks = self.env.take_deferred_tasks();
            for task in tasks {
                match task {
                    env::DeferredTask::Confirm { inst, span } => {
                        let inst = {
                            let mut prober = env::Prober::new(&mut self.table);
                            inst.fold_with(db, &mut prober)
                        };
                        let inst = inst.normalize(db, scope, assumptions);
                        let canonical_inst = Canonicalized::new(db, inst);
                        match is_goal_satisfiable(db, ingot, canonical_inst.value, assumptions) {
                            GoalSatisfiability::Satisfied(solution) => {
                                let solution =
                                    canonical_inst.extract_solution(&mut self.table, *solution);
                                self.table.unify(inst, solution).unwrap();
                                let new_can =
                                    Canonical::new(db, inst.fold_with(db, &mut self.table));
                                if new_can != canonical_inst.value {
                                    progressed = true;
                                }
                            }
                            _ => self.env.register_confirmation(inst, span),
                        }
                    }
                    env::DeferredTask::Method(pending) => {
                        let (receiver, generic_args, call_args) = match pending.expr.data(db, body)
                        {
                            Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                                (*receiver, *generic_args, args.as_slice())
                            }
                            _ => continue,
                        };

                        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                            continue;
                        };
                        let expr_ty = {
                            let mut prober = env::Prober::new(&mut self.table);
                            expr_prop.ty.fold_with(db, &mut prober)
                        };
                        if expr_ty.has_invalid(db) {
                            self.env.register_pending_method(pending);
                            continue;
                        }

                        let viable: Vec<_> = pending
                            .candidates
                            .iter()
                            .copied()
                            .filter(|&inst| {
                                is_viable(
                                    self,
                                    &pending,
                                    expr_ty,
                                    receiver,
                                    generic_args,
                                    call_args,
                                    inst,
                                )
                            })
                            .collect();

                        if let [inst] = viable.as_slice() {
                            if self.env.callable_expr(pending.expr).is_none() {
                                let call_span = pending.expr.span(body).into_method_call_expr();

                                let receiver_prop = self
                                    .env
                                    .typed_expr(receiver)
                                    .unwrap_or_else(|| ExprProp::invalid(db));
                                let recv_ty = receiver_prop.ty;

                                let trait_method = *inst
                                    .def(db)
                                    .method_defs(db)
                                    .get(&pending.method_name)
                                    .unwrap();
                                let func_ty = self.instantiate_trait_method_to_term(
                                    trait_method,
                                    recv_ty,
                                    *inst,
                                );

                                let mut callable = match Callable::new(
                                    db,
                                    func_ty,
                                    receiver.span(body).into(),
                                    Some(*inst),
                                ) {
                                    Ok(callable) => callable,
                                    Err(diag) => {
                                        self.push_diag(diag);
                                        progressed = true;
                                        continue;
                                    }
                                };

                                if !callable.unify_generic_args(
                                    self,
                                    generic_args,
                                    call_span.clone().generic_args(),
                                ) {
                                    progressed = true;
                                    continue;
                                }

                                callable.check_args(
                                    self,
                                    call_args,
                                    call_span.clone().args(),
                                    Some((receiver, receiver_prop)),
                                    true,
                                );

                                self.check_callable_effects(pending.expr, &callable);
                                callable.check_constraints(self, call_span.method_name().into());

                                let ret_ty = self.normalize_ty(callable.ret_ty(db));
                                self.table.unify(expr_prop.ty, ret_ty).unwrap();

                                self.env.register_callable(pending.expr, callable);
                            }

                            progressed = true;
                        } else {
                            self.env.register_pending_method(pending);
                        }
                    }
                }
            }
        }

        // Emit diagnostics for remaining tasks.
        for task in self.env.take_deferred_tasks() {
            match task {
                env::DeferredTask::Confirm { inst, span } => {
                    let inst = {
                        let mut prober = env::Prober::new(&mut self.table);
                        inst.fold_with(db, &mut prober)
                    };
                    let inst = inst.normalize(db, scope, assumptions);
                    let canonical_inst = Canonicalized::new(db, inst);
                    match is_goal_satisfiable(db, ingot, canonical_inst.value, assumptions) {
                        GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                            let cands = ambiguous
                                .iter()
                                .map(|s| canonical_inst.extract_solution(&mut self.table, *s))
                                .collect::<thin_vec::ThinVec<_>>();
                            if !inst.self_ty(db).has_var(db) {
                                self.push_diag(BodyDiag::AmbiguousTraitInst {
                                    primary: span.clone(),
                                    cands,
                                });
                            }
                        }
                        GoalSatisfiability::UnSat(subgoal) => {
                            if !inst.self_ty(db).has_var(db) {
                                let unsat = subgoal
                                    .map(|s| canonical_inst.extract_solution(&mut self.table, s));
                                self.push_diag(TyDiagCollection::from(
                                    TraitConstraintDiag::TraitBoundNotSat {
                                        span: span.clone(),
                                        primary_goal: inst,
                                        unsat_subgoal: unsat,
                                    },
                                ));
                            }
                        }
                        _ => {}
                    }
                }
                env::DeferredTask::Method(pending) => {
                    let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                        continue;
                    };
                    let expr_ty = {
                        let mut prober = env::Prober::new(&mut self.table);
                        expr_prop.ty.fold_with(db, &mut prober)
                    };
                    if expr_ty.has_invalid(db) {
                        continue;
                    }

                    let (receiver, generic_args, call_args) = match pending.expr.data(db, body) {
                        Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                            (*receiver, *generic_args, args.as_slice())
                        }
                        _ => continue,
                    };

                    let viable: thin_vec::ThinVec<_> = pending
                        .candidates
                        .iter()
                        .copied()
                        .filter(|&inst| {
                            is_viable(
                                self,
                                &pending,
                                expr_ty,
                                receiver,
                                generic_args,
                                call_args,
                                inst,
                            )
                        })
                        .collect();
                    if viable.len() > 1 {
                        self.push_diag(BodyDiag::AmbiguousTrait {
                            primary: pending.span.clone(),
                            method_name: pending.method_name,
                            traits: viable,
                        });
                    }
                }
            }
        }
    }

    fn new_internal(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
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

    fn parent_expr(&self) -> Option<&'db Expr<'db>> {
        let id = self.env.parent_expr()?;
        match &self.body().exprs(self.db)[id] {
            Partial::Present(expr) => Some(expr),
            Partial::Absent => None,
        }
    }

    fn lit_ty(&mut self, lit: &LitKind<'db>) -> TyId<'db> {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db);
                self.table
                    .new_var(TyVarSort::String(len_bytes), &Kind::Star)
            }
        }
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

        match t {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = actual;
                self.env.type_expr(expr, typed_expr)
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
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
            let diag = BodyDiag::TypeMismatch {
                span,
                expected,
                given: actual,
            };
            self.push_diag(diag);
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
                    actual
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                self.push_diag(BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                });
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

        let res = match resolve_path_with_observer(
            self.db,
            path,
            scope,
            self.env.assumptions(),
            resolve_tail_as_value,
            &mut check_visibility,
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
        self.instantiate_to_term(ty)
    }

    fn instantiate_trait_assoc_fn_to_term(
        &mut self,
        method: CallableDef<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_assoc_fn(self.db, method, inst);
        self.instantiate_to_term(ty)
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
                && matches!(param_ty.data(self.db), TyData::TyParam(p) if p.is_effect_provider())
            {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, &arg));
            }
            ty = TyId::app(self.db, ty, arg);
        }

        ty
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
    pub key: PathId<'db>,
    pub arg: EffectArg<'db>,
    pub pass_mode: EffectPassMode,
}

/// Resolved reference for a `const`-valued path expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstRef<'db> {
    Const(Const<'db>),
    TraitConst {
        inst: TraitInstId<'db>,
        name: IdentId<'db>,
    },
}

impl<'db> TyVisitable<'db> for ConstRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            ConstRef::Const(_) => {}
            ConstRef::TraitConst { inst, .. } => inst.visit_with(visitor),
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
            ConstRef::TraitConst { inst, name } => ConstRef::TraitConst {
                inst: inst.fold_with(db, folder),
                name,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct TypedBody<'db> {
    body: Option<Body<'db>>,
    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    const_refs: FxHashMap<ExprId, ConstRef<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,
    call_effect_args: FxHashMap<ExprId, Vec<ResolvedEffectArg<'db>>>,
    /// Bindings for function parameters (indexed by param position)
    param_bindings: Vec<LocalBinding<'db>>,
    /// Bindings for local variables (keyed by the pattern that introduces them)
    pat_bindings: FxHashMap<PatId, LocalBinding<'db>>,
    /// Resolved Seq trait methods for for-loops
    for_loop_seq: FxHashMap<StmtId, ForLoopSeq<'db>>,
}

impl<'db> TyVisitable<'db> for TypedBody<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        for ty in self.pat_ty.values() {
            ty.visit_with(visitor);
        }
        for prop in self.expr_ty.values() {
            prop.visit_with(visitor);
        }
        for cref in self.const_refs.values() {
            cref.visit_with(visitor);
        }
        for callable in self.callables.values() {
            callable.visit_with(visitor);
        }
        for seq in self.for_loop_seq.values() {
            seq.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TypedBody<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        let pat_ty = self
            .pat_ty
            .into_iter()
            .map(|(pat, ty)| (pat, ty.fold_with(db, folder)))
            .collect();
        let expr_ty = self
            .expr_ty
            .into_iter()
            .map(|(expr, prop)| (expr, prop.fold_with(db, folder)))
            .collect();
        let const_refs = self
            .const_refs
            .into_iter()
            .map(|(expr, cref)| (expr, cref.fold_with(db, folder)))
            .collect();
        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(db, folder)))
            .collect();
        let for_loop_seq = self
            .for_loop_seq
            .into_iter()
            .map(|(stmt, seq)| (stmt, seq.fold_with(db, folder)))
            .collect();

        Self {
            body: self.body,
            pat_ty,
            expr_ty,
            const_refs,
            callables,
            call_effect_args: self.call_effect_args,
            param_bindings: self.param_bindings,
            pat_bindings: self.pat_bindings,
            for_loop_seq,
        }
    }
}

impl<'db> TypedBody<'db> {
    pub fn body(&self) -> Option<Body<'db>> {
        self.body
    }

    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.expr_ty
            .get(&expr)
            .cloned()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.const_refs.get(&expr).copied()
    }

    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables.get(&expr)
    }

    pub fn call_effect_args(&self, call_expr: ExprId) -> Option<&[ResolvedEffectArg<'db>]> {
        self.call_effect_args.get(&call_expr).map(|v| v.as_slice())
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.param_bindings.get(idx).copied()
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings.get(&pat).copied()
    }

    /// Get the resolved Seq methods for a for-loop statement.
    pub fn for_loop_seq(&self, stmt: StmtId) -> Option<&ForLoopSeq<'db>> {
        self.for_loop_seq.get(&stmt)
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

    /// Get the binding kind for an expression that references a local binding.
    ///
    /// Returns the identity of the binding (param index, pattern id, or effect param ident).
    pub fn expr_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.expr_ty.get(&expr)?.binding
    }

    /// Returns a place representation for `expr` if it denotes an assignable location.
    pub fn expr_place(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> Option<Place<'db>> {
        Place::from_expr(db, self, expr)
    }

    /// Find all expressions that reference the same local binding as the given expression.
    ///
    /// Returns a list of ExprIds that share the same local binding (variable, parameter,
    /// or effect parameter). Returns an empty list if the expression doesn't have a binding.
    ///
    /// This is used by the language server for find-all-references and rename on local variables.
    pub fn local_references(&self, expr: ExprId) -> Vec<ExprId> {
        let Some(binding) = self.expr_ty.get(&expr).and_then(|p| p.binding) else {
            return vec![];
        };

        self.expr_ty
            .iter()
            .filter_map(|(id, p)| {
                if p.binding == Some(binding) {
                    Some(*id)
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
            .filter_map(|(id, p)| {
                if p.binding == Some(binding) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn empty() -> Self {
        Self {
            body: None,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            const_refs: FxHashMap::default(),
            callables: FxHashMap::default(),
            call_effect_args: FxHashMap::default(),
            param_bindings: Vec::new(),
            pat_bindings: FxHashMap::default(),
            for_loop_seq: FxHashMap::default(),
        }
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

fn instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    let ty = TyId::foldl(
        db,
        TyId::func(db, method.as_callable(db).unwrap()),
        inst.args(db),
    );

    let inst_self = table.instantiate_to_term(inst.self_ty(db));
    table.unify(inst_self, receiver_ty).unwrap();

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    ty.fold_with(db, &mut subst)
}

/// Instantiate a trait-associated function type (no receiver), e.g. `T::make`.
fn instantiate_trait_assoc_fn<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
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
            if prop.binding.is_none() {
                self.check_wf(prop.ty, span.into());
            }
        }

        // We need this additional check for method call because the callable type is
        // not tied to the expression type.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.clone().into());
            self.check_wf(callable_ty, span.into())
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
        let body = checker.env.finish(&mut checker.table);

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            effect_provider_keys: checker.effect_provider_keys,
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

        let hir_db = self.db;
        let ingot = self.body.body.unwrap().top_mod(hir_db).ingot(hir_db);
        if let Some(diag) = ty.emit_wf_diag(self.db, ingot, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}
