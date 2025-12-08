use std::panic;

use common::ingot::IngotKind;
use either::Either;

use crate::core::hir_def::{
    ArithBinOp, BinOp, CallableDef, Expr, ExprId, FieldIndex, IdentId, Partial, Pat, PatId, PathId,
    UnOp, VariantKind, WithBinding, scope_graph::ScopeId,
};
use crate::span::DynLazySpan;

use super::{
    RecordLike, Typeable,
    env::{EffectOrigin, ExprProp, LocalBinding, ProvidedEffect, TyCheckEnv},
    path::ResolvedPathInBody,
};
use crate::analysis::ty::{
    binder::Binder,
    canonical::Canonicalized,
    diagnostics::{BodyDiag, FuncBodyDiag},
    fold::{AssocTySubst, TyFoldable as _},
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, PredicateListId, constraint::collect_func_def_constraints,
        is_goal_satisfiable,
    },
    ty_check::callable::Callable,
    ty_def::{TyBase, TyData},
};
use crate::analysis::{
    HirAnalysisDb, Spanned,
    name_resolution::{
        EarlyNameQueryId, ExpectedPathKind, NameDomain, NameResBucket, NameResolutionError,
        PathRes, QueryDirective,
        diagnostics::PathResDiag,
        is_scope_visible_from,
        method_selection::{MethodCandidate, MethodSelectionError, select_method_candidate},
        resolve_ident_to_bucket, resolve_name_res, resolve_path, resolve_query,
    },
    ty::{
        const_ty::ConstTyId,
        normalize::normalize_ty,
        ty_check::{TyChecker, path::RecordInitChecker},
        ty_def::{InvalidCause, TyId},
    },
};

enum EffectRequirement<'db> {
    Type(TyId<'db>),
    Trait(TraitInstId<'db>),
}

impl<'db> TyChecker<'db> {
    pub(super) fn check_expr(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let typed = ExprProp::invalid(self.db);
            self.env.type_expr(expr, typed);
            return typed;
        };

        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());

        self.env.enter_expr(expr);
        let mut actual = match expr_data {
            Expr::Lit(lit) => ExprProp::new(self.lit_ty(lit), true),
            Expr::Block(..) => self.check_block(expr, expr_data, expected),
            Expr::Un(..) => self.check_unary(expr, expr_data),
            Expr::Bin(lhs, rhs, op) => self.check_binary(expr, *lhs, *rhs, *op),
            Expr::Call(..) => self.check_call(expr, expr_data),
            Expr::MethodCall(..) => self.check_method_call(expr, expr_data),
            Expr::Path(..) => self.check_path(expr, expr_data),
            Expr::RecordInit(..) => self.check_record_init(expr, expr_data),
            Expr::Field(..) => self.check_field(expr, expr_data),
            Expr::Tuple(..) => self.check_tuple(expr, expr_data, expected),
            Expr::Array(..) => self.check_array(expr, expr_data, expected),
            Expr::ArrayRep(..) => self.check_array_rep(expr, expr_data, expected),
            Expr::If(..) => self.check_if(expr, expr_data),
            Expr::Match(..) => self.check_match(expr, expr_data),
            Expr::Assign(..) => self.check_assign(expr, expr_data),
            Expr::AugAssign(..) => self.check_aug_assign(expr, expr_data),
            Expr::With(bindings, body) => self.check_with(bindings, *body, expected),
        };
        self.env.leave_expr();

        let typeable = Typeable::Expr(expr, actual);
        actual.ty = normalize_ty(self.db, actual.ty, self.env.scope(), self.env.assumptions());
        actual.ty = self.unify_ty(typeable, actual.ty, expected);
        actual
    }

    pub(super) fn check_expr_unknown(&mut self, expr: ExprId) -> ExprProp<'db> {
        let t = self.fresh_ty();
        self.check_expr(expr, t)
    }

    fn check_block(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Block(stmts) = expr_data else {
            unreachable!()
        };

        if stmts.is_empty() {
            ExprProp::new(TyId::unit(self.db), true)
        } else {
            self.env.enter_scope(expr);
            for &stmt in stmts[..stmts.len() - 1].iter() {
                let ty = self.fresh_ty();
                self.check_stmt(stmt, ty);
            }

            let last_stmt = stmts[stmts.len() - 1];
            let res = self.check_stmt(last_stmt, expected);
            self.env.leave_scope();
            ExprProp::new(res, true)
        }
    }

    fn check_unary(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Un(lhs, op) = expr_data else {
            unreachable!()
        };
        let prop = self.check_expr_unknown(*lhs);
        if *op == UnOp::Plus {
            // TODO: remove support for unary plus? what should it do?
            return prop;
        }
        if prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if prop.ty.is_integral_var(self.db) && matches!(op, UnOp::Plus | UnOp::Minus | UnOp::BitNot)
        {
            return prop;
        }

        let base_ty = prop.ty.base_ty(self.db);
        if base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        self.check_ops_trait(expr, prop.ty, op, None)
    }

    fn check_binary(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
    ) -> ExprProp<'db> {
        // Logical operands must be bools
        if matches!(op, BinOp::Logical(_)) {
            let bool = TyId::bool(self.db);
            let lhs = self.check_expr(lhs_expr, bool);
            let rhs = self.check_expr(rhs_expr, bool);
            return if lhs.ty.is_bool(self.db) && rhs.ty.is_bool(self.db) {
                ExprProp::new(bool, true)
            } else {
                ExprProp::invalid(self.db)
            };
        }

        let lhs = self.check_expr_unknown(lhs_expr);
        if lhs.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if matches!(op, BinOp::Index) && lhs.ty.is_array(self.db) {
            // Built-in array indexing (TODO: move to trait impl)
            let args = lhs.ty.generic_args(self.db);
            let elem_ty = args[0];
            let index_ty = args[1].const_ty_ty(self.db).unwrap();
            self.check_expr(rhs_expr, index_ty);
            return ExprProp::new(elem_ty, lhs.is_mut);
        } else if lhs.ty.is_integral_var(self.db) {
            // Avoid 'type must be known' diagnostics when lhs is an unknown integer ty
            self.check_expr(rhs_expr, lhs.ty);
            return lhs;
        }

        // Fail if lhs ty is unknown
        if lhs.ty.base_ty(self.db).is_ty_var(self.db) {
            self.check_expr_unknown(rhs_expr);
            let diag = BodyDiag::TypeMustBeKnown(lhs_expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        self.check_ops_trait(expr, lhs.ty, &op, Some(rhs_expr))
    }

    fn check_with(
        &mut self,
        bindings: &[WithBinding<'db>],
        body_expr: ExprId,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        self.env.push_effect_frame();

        for binding in bindings {
            let value_prop = self.check_expr_unknown(binding.value);
            let Some(key_path) = binding.key_path.to_opt() else {
                continue;
            };

            let is_mut = value_prop
                .binding()
                .map(|b| b.is_mut())
                .unwrap_or(value_prop.is_mut);

            let provided = ProvidedEffect {
                origin: EffectOrigin::With {
                    value_expr: binding.value,
                },
                ty: value_prop.ty,
                is_mut,
            };

            self.env.insert_effect_binding(key_path, provided);
        }

        let result = self.check_expr(body_expr, expected);
        self.env.pop_effect_frame();
        result
    }

    fn check_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Call(callee, args) = expr_data else {
            unreachable!()
        };
        let callee_prop = self.check_expr_unknown(*callee);
        if callee_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let mut callable = if let Some(existing) = self.env.callable_expr(*callee) {
            existing.clone()
        } else {
            match Callable::new(
                self.db,
                callee_prop.ty,
                callee.span(self.body()).into(),
                None,
            ) {
                Ok(callable) => callable,
                Err(diag) => {
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
            }
        };

        let call_span = expr.span(self.body()).into_call_expr();

        if let Partial::Present(Expr::Path(Partial::Present(path))) =
            callee.data(self.db, self.body())
        {
            let idx = path.segment_index(self.db);

            if !callable.unify_generic_args(
                self,
                path.generic_args(self.db),
                expr.span(self.body())
                    .into_path_expr()
                    .path()
                    .segment(idx)
                    .generic_args(),
            ) {
                return ExprProp::invalid(self.db);
            }
        };

        callable.check_args(self, args, call_span.args(), None);

        self.check_callable_effects(expr, &callable);

        let ret_ty = callable.ret_ty(self.db);
        // Normalize the return type to resolve any associated types
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        self.env.register_callable(expr, callable);
        ExprProp::new(normalized_ret_ty, true)
    }

    fn check_callable_effects(&mut self, expr: ExprId, callable: &Callable<'db>) {
        let CallableDef::Func(func) = callable.callable_def else {
            return;
        };

        if !func.has_effects(self.db) {
            return;
        }

        let call_span = expr.span(self.body());
        let callee_assumptions = collect_func_def_constraints(self.db, func.into(), true)
            .instantiate_identity()
            .extend_all_bounds(self.db);

        for effect in func.effect_params(self.db) {
            let Some(key_path) = effect.key_path(self.db) else {
                continue;
            };

            let cands =
                self.env
                    .effect_candidates_in_scope(key_path, func.scope(), callee_assumptions);
            let provided = match cands.as_slice() {
                [] => {
                    let diag = BodyDiag::MissingEffect {
                        primary: call_span.clone().into(),
                        func,
                        key: key_path,
                    };
                    self.push_diag(diag);
                    continue;
                }
                [one] => *one,
                _ => {
                    let diag = BodyDiag::AmbiguousEffect {
                        primary: call_span.clone().into(),
                        func,
                        key: key_path,
                    };
                    self.push_diag(diag);
                    continue;
                }
            };

            if effect.is_mut(self.db) && !provided.is_mut {
                let provided_span = match provided.origin {
                    EffectOrigin::With { value_expr } => Some(value_expr.span(self.body()).into()),
                    EffectOrigin::Param { .. } => None,
                };
                let diag = BodyDiag::EffectMutabilityMismatch {
                    primary: call_span.clone().into(),
                    func,
                    key: key_path,
                    provided_span,
                };
                self.push_diag(diag);
                continue;
            }

            let Some(requirement) = self.resolve_effect_requirement(
                key_path,
                callable,
                func.scope(),
                callee_assumptions,
                provided.ty,
            ) else {
                continue;
            };

            match requirement {
                EffectRequirement::Type(expected) => {
                    if self.table.unify(expected, provided.ty).is_err() {
                        let provided_span = match provided.origin {
                            EffectOrigin::With { value_expr } => {
                                Some(value_expr.span(self.body()).into())
                            }
                            EffectOrigin::Param { .. } => None,
                        };
                        let diag = BodyDiag::EffectTypeMismatch {
                            primary: call_span.clone().into(),
                            func,
                            key: key_path,
                            expected,
                            given: provided.ty,
                            provided_span,
                        };
                        self.push_diag(diag);
                    }
                }
                EffectRequirement::Trait(trait_req) => {
                    let canonical = Canonicalized::new(self.db, trait_req);
                    let ingot = self.env.body().top_mod(self.db).ingot(self.db);
                    match is_goal_satisfiable(
                        self.db,
                        ingot,
                        canonical.value,
                        self.env.assumptions(),
                    ) {
                        GoalSatisfiability::UnSat(_) => {
                            let provided_span = match provided.origin {
                                EffectOrigin::With { value_expr } => {
                                    Some(value_expr.span(self.body()).into())
                                }
                                EffectOrigin::Param { .. } => None,
                            };
                            let diag = BodyDiag::EffectTraitUnsatisfied {
                                primary: call_span.clone().into(),
                                func,
                                key: key_path,
                                trait_req,
                                given: provided.ty,
                                provided_span,
                            };
                            self.push_diag(diag);
                        }
                        GoalSatisfiability::ContainsInvalid => {}
                        _ => {}
                    }
                }
            }
        }
    }

    fn resolve_effect_requirement(
        &mut self,
        key_path: PathId<'db>,
        callable: &Callable<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        provided_ty: TyId<'db>,
    ) -> Option<EffectRequirement<'db>> {
        let path_res = resolve_path(self.db, key_path, scope, assumptions, false).ok()?;

        match path_res {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                let mut expected = Binder::bind(ty).instantiate(self.db, callable.generic_args());
                if let Some(inst) = callable.trait_inst() {
                    let mut subst = AssocTySubst::new(inst);
                    expected = expected.fold_with(self.db, &mut subst);
                }
                Some(EffectRequirement::Type(expected))
            }
            PathRes::Trait(trait_inst) => {
                let mut instantiation_args = Vec::with_capacity(1 + callable.generic_args().len());
                instantiation_args.push(provided_ty);
                instantiation_args.extend_from_slice(callable.generic_args());

                let mut trait_req =
                    Binder::bind(trait_inst).instantiate(self.db, &instantiation_args);
                if let Some(inst) = callable.trait_inst() {
                    let mut subst = AssocTySubst::new(inst);
                    trait_req = trait_req.fold_with(self.db, &mut subst);
                }
                Some(EffectRequirement::Trait(trait_req))
            }
            _ => None,
        }
    }

    fn check_method_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
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

        let canonical_r_ty = Canonicalized::new(self.db, receiver_prop.ty);
        let candidate = match select_method_candidate(
            self.db,
            canonical_r_ty.value,
            method_name,
            self.env.scope(),
            self.env.assumptions(),
            None,
        ) {
            Ok(candidate) => candidate,
            Err(err) => {
                match err {
                    MethodSelectionError::AmbiguousTraitMethod(insts) => {
                        // Defer resolution using return-type constraints
                        let ret_ty = self.fresh_ty();
                        let typed = ExprProp::new(ret_ty, true);
                        self.env.type_expr(expr, typed);
                        // Instantiate candidates with fresh inference vars so
                        // later unifications can bind their parameters.
                        let cands: Vec<_> = insts
                            .into_iter()
                            .map(|inst| {
                                self.table.instantiate_with_fresh_vars(
                                    crate::analysis::ty::binder::Binder::bind(inst),
                                )
                            })
                            .collect();

                        self.env.register_pending_method(super::env::PendingMethod {
                            expr,
                            recv_ty: receiver_prop.ty,
                            method_name,
                            candidates: cands,
                            span: call_span.method_name().into(),
                        });
                        return typed;
                    }
                    _ => {
                        let diag = body_diag_from_method_selection_err(
                            self.db,
                            err,
                            Spanned::new(
                                canonical_r_ty.value.value,
                                receiver.span(self.body()).into(),
                            ),
                            Spanned::new(method_name, call_span.method_name().into()),
                        );
                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                }
            }
        };

        let (func_ty, trait_inst) = match candidate {
            MethodCandidate::InherentMethod(func_def) => {
                let func_ty = TyId::func(self.db, func_def);
                (self.table.instantiate_to_term(func_ty), None)
            }

            MethodCandidate::TraitMethod(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let func_ty = super::instantiate_trait_method(
                    self.db,
                    cand.method,
                    &mut self.table,
                    receiver_prop.ty,
                    inst,
                );
                (func_ty, Some(inst))
            }

            MethodCandidate::NeedsConfirmation(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                self.env
                    .register_confirmation(inst, call_span.clone().into());
                let func_ty = super::instantiate_trait_method(
                    self.db,
                    cand.method,
                    &mut self.table,
                    receiver_prop.ty,
                    inst,
                );
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

        if !callable.unify_generic_args(self, *generic_args, call_span.clone().generic_args()) {
            return ExprProp::invalid(self.db);
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

        callable.check_args(
            self,
            args,
            call_span.clone().args(),
            Some((*receiver, receiver_prop)),
        );

        // Check required effects for the method call
        self.check_callable_effects(expr, &callable);

        // Check function constraints after instantiation
        callable.check_constraints(self, call_span.method_name().into());

        let ret_ty = callable.ret_ty(self.db);

        // Normalize the return type to resolve any associated types
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        self.env.register_callable(expr, callable);
        ExprProp::new(normalized_ret_ty, true)
    }

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Path(path) = expr_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let path_expr_span = expr.span(self.body()).into_path_expr();
        let path_span = path_expr_span.clone().path();

        let res = if path.is_bare_ident(self.db) {
            let ident_span: DynLazySpan<'db> = path_expr_span.clone().into();
            resolve_ident_expr(self.db, &self.env, *path, ident_span)
        } else {
            match self.resolve_path(*path, true, path_span.clone()) {
                Ok(r) => ResolvedPathInBody::Reso(r),
                Err(err) => {
                    let expected_kind = if matches!(self.parent_expr(), Some(Expr::Call(..))) {
                        ExpectedPathKind::Function
                    } else {
                        ExpectedPathKind::Value
                    };

                    if let Some(diag) =
                        err.into_diag(self.db, *path, path_span.clone(), expected_kind)
                    {
                        self.push_diag(diag)
                    }
                    ResolvedPathInBody::Invalid
                }
            }
        };

        match res {
            ResolvedPathInBody::Binding(binding) => {
                let ty = self.env.lookup_binding_ty(binding);
                let is_mut = binding.is_mut();
                ExprProp::new_binding_ref(ty, is_mut, binding)
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
                PathRes::Func(ty) => ExprProp::new(self.table.instantiate_to_term(ty), true),
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
                        VariantKind::Unit => variant.ty,
                        VariantKind::Tuple(_) => {
                            let ty = variant.constructor_func_ty(self.db).unwrap();
                            self.table.instantiate_to_term(ty)
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

                    ExprProp::new(self.table.instantiate_to_term(ty), true)
                }
                PathRes::Const(_, ty) => ExprProp::new(ty, true),
                PathRes::Method(receiver_ty, candidate) => {
                    let canonical_r_ty = Canonicalized::new(self.db, receiver_ty);
                    let (method_ty, trait_inst) = match candidate {
                        MethodCandidate::InherentMethod(func_def) => {
                            // TODO: move this to path resolver
                            let mut method_ty = TyId::func(self.db, func_def);
                            for &arg in receiver_ty.generic_args(self.db) {
                                // If the method is defined in "specialized" impl block
                                // of a generic type (eg `impl Option<i32>`), then
                                // calling `TyId::app(db, method_ty, ..)` will result in
                                // `TyId::invalid`.
                                if method_ty.applicable_ty(self.db).is_some() {
                                    method_ty = TyId::app(self.db, method_ty, arg);
                                } else {
                                    break;
                                }
                            }
                            (method_ty, None)
                        }
                        MethodCandidate::TraitMethod(cand)
                        | MethodCandidate::NeedsConfirmation(cand) => {
                            let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                            if matches!(candidate, MethodCandidate::NeedsConfirmation(_)) {
                                self.env
                                    .register_confirmation(inst, path_expr_span.clone().into());
                            }
                            let method_ty = super::instantiate_trait_method(
                                self.db,
                                cand.method,
                                &mut self.table,
                                receiver_ty,
                                inst,
                            );
                            (method_ty, Some(inst))
                        }
                    };

                    let instantiated_method_ty = self.table.instantiate_to_term(method_ty);
                    if self.env.callable_expr(expr).is_none() {
                        let callable = Callable::new(
                            self.db,
                            instantiated_method_ty,
                            expr.span(self.body()).into(),
                            trait_inst,
                        )
                        .expect("method path should resolve to callable");
                        self.env.register_callable(expr, callable);
                    }

                    ExprProp::new(instantiated_method_ty, true)
                }
                PathRes::TraitConst(_recv_ty, inst, name) => {
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
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::FuncParam(item, _) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(item),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
            },
        }
    }

    fn check_record_init(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::RecordInit(path, ..) = expr_data else {
            unreachable!()
        };
        let span = expr.span(self.body()).into_record_init_expr();

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let Ok(reso) = self.resolve_path(*path, true, span.clone().path()) else {
            return ExprProp::invalid(self.db);
        };

        match reso {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                let record_like = RecordLike::from_ty(ty);
                if record_like.is_record(self.db) {
                    self.check_record_init_fields(&record_like, expr);
                    ExprProp::new(ty, true)
                } else {
                    let diag =
                        BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
            }

            PathRes::Func(ty) | PathRes::Const(_, ty) | PathRes::TraitConst(ty, ..) => {
                let record_like = RecordLike::from_ty(ty);
                let diag =
                    BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::Method(..) | PathRes::FuncParam(..) => {
                let diag = BodyDiag::record_expected(self.db, span.path().into(), None);
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }

            PathRes::EnumVariant(variant) => {
                let ty = variant.ty;
                let record_like = RecordLike::from_variant(variant);
                if record_like.is_record(self.db) {
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

            rec_checker.tc.check_expr(field.expr, expected);
        }

        if let Err(diag) = rec_checker.finalize(span.into(), false) {
            self.push_diag(diag);
        }
    }

    fn check_field(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Field(lhs, index) = expr_data else {
            unreachable!()
        };
        let Partial::Present(field) = index else {
            return ExprProp::invalid(self.db);
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        // let lhs_ty = normalize_ty(self.db, lhs_ty, self.env.scope(), self.env.assumptions());

        let (ty_base, ty_args) = lhs_ty.decompose_ty_app(self.db);

        if ty_base.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }
        let ty_base = lhs_ty;

        if ty_base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        match field {
            FieldIndex::Ident(label) => {
                let record_like = RecordLike::from_ty(lhs_ty);
                if let Some(field_ty) = record_like.record_field_ty(self.db, *label) {
                    if let Some(scope) = record_like.record_field_scope(self.db, *label)
                        && !is_scope_visible_from(self.db, scope, self.env.scope())
                    {
                        // Check the visibility of the field.
                        let diag = PathResDiag::Invisible(
                            expr.span(self.body()).into_field_expr().accessor().into(),
                            *label,
                            scope.name_span(self.db),
                        );

                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                    return ExprProp::new(field_ty, typed_lhs.is_mut);
                }
            }

            FieldIndex::Index(i) => {
                let arg_len = ty_args.len().into();
                if ty_base.is_tuple(self.db) && i.data(self.db) < &arg_len {
                    let i: usize = i.data(self.db).try_into().unwrap();
                    let ty = ty_args[i];
                    return ExprProp::new(ty, typed_lhs.is_mut);
                }
            }
        };

        let diag = BodyDiag::AccessedFieldNotFound {
            primary: expr.span(self.body()).into(),
            given_ty: lhs_ty,
            index: *field,
        };
        self.push_diag(diag);

        ExprProp::invalid(self.db)
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
            self.check_expr(*elem, *elem_ty);
        }

        let ty = TyId::tuple_with_elems(self.db, &elem_tys);
        ExprProp::new(ty, true)
    }

    fn resolve_core_trait(
        &self,
        trait_path: PathId<'db>,
    ) -> Option<crate::core::hir_def::Trait<'db>> {
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let mut module_path = trait_path.parent(self.db)?;

        // If we are inside the core ingot, replace `core` with `ingot` in the trait path.
        let ingot = self.env.body().top_mod(self.db).ingot(self.db);
        if ingot.kind(self.db) == IngotKind::Core && trait_path.is_core_lib_path(self.db) {
            module_path = module_path.replace_root(
                IdentId::make_core(self.db),
                IdentId::make_ingot(self.db),
                self.db,
            );
        }
        let trait_name = trait_path.ident(self.db).to_opt()?;
        let Ok(PathRes::Mod(mod_scope)) =
            resolve_path(self.db, module_path, scope, assumptions, false)
        else {
            return None;
        };

        let bucket =
            resolve_ident_to_bucket(self.db, PathId::from_ident(self.db, trait_name), mod_scope);
        let nameres = bucket.pick(NameDomain::TYPE).as_ref().ok()?;
        nameres.trait_()
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
            expected_elem_ty = self.check_expr(*elem, expected_elem_ty).ty;
        }

        let ty = TyId::array_with_len(self.db, expected_elem_ty, elems.len());
        ExprProp::new(ty, true)
    }

    fn check_array_rep(
        &mut self,
        _expr: ExprId,
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

        expected_elem_ty = self.check_expr(*elem, expected_elem_ty).ty;

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

            array_ty
        } else {
            let len_ty = ConstTyId::invalid(self.db, InvalidCause::ParseError);
            let len_ty = TyId::const_ty(self.db, len_ty);
            TyId::app(self.db, array, len_ty)
        };

        ExprProp::new(ty, true)
    }

    fn check_if(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        let if_ty = self.fresh_ty();
        let ty = match else_ {
            Some(else_) => {
                self.check_expr_in_new_scope(*then, if_ty);
                self.check_expr_in_new_scope(*else_, if_ty).ty
            }

            None => {
                // If there is no else branch, the if expression itself typed as `()`
                self.check_expr_in_new_scope(*then, if_ty);
                TyId::unit(self.db)
            }
        };

        ExprProp::new(ty, true)
    }

    fn check_match(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Match(scrutinee, arms) = expr_data else {
            unreachable!()
        };

        let scrutinee_ty = self.fresh_ty();
        let scrutinee_ty = self.check_expr(*scrutinee, scrutinee_ty).ty;

        let Partial::Present(arms) = arms else {
            return ExprProp::invalid(self.db);
        };

        let mut match_ty = self.fresh_ty();
        // Store cloned HirPat data and the original PatId for diagnostics.
        let mut hir_pats_with_ids: Vec<(&Pat<'db>, PatId)> = Vec::with_capacity(arms.len());

        // First loop: Type check patterns, collect HIR patterns for analysis, and type check arm bodies.
        for arm in arms.iter() {
            self.check_pat(arm.pat, scrutinee_ty);

            let pat_data_partial = arm.pat.data(self.db, self.body());
            if let Partial::Present(actual_pat_data) = pat_data_partial {
                // Clone the Pat data for ownership in the vector.
                hir_pats_with_ids.push((actual_pat_data, arm.pat));
            }
            // If pat_data is Partial::Absent, check_pat should have already emitted an error.
            // We only include valid patterns in the exhaustiveness/reachability analysis.

            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();
            match_ty = self.check_expr(arm.body, match_ty).ty;
            self.env.leave_scope();
        }

        // Collect owned HirPat data for analysis.
        let collected_hir_pats: Vec<Pat<'db>> = hir_pats_with_ids
            .iter()
            .map(|(p, _id)| (*p).clone())
            .collect();

        // Perform reachability analysis.
        let reachability = crate::analysis::ty::pattern_analysis::check_reachability(
            self.db,
            &collected_hir_pats,
            self.body(),
            self.env.scope(),
            scrutinee_ty,
        );

        for (i, is_reachable) in reachability.iter().enumerate() {
            if !is_reachable {
                let (_current_hir_pat, current_pat_id) = &hir_pats_with_ids[i];
                let diag = BodyDiag::UnreachablePattern {
                    primary: current_pat_id.span(self.body()).into(),
                };
                self.push_diag(diag);
            }
        }

        // Perform exhaustiveness analysis.
        if let Err(missing_patterns) = crate::analysis::ty::pattern_analysis::check_exhaustiveness(
            self.db,
            &collected_hir_pats,
            self.body(),
            self.env.scope(),
            scrutinee_ty,
        ) {
            let diag = BodyDiag::NonExhaustiveMatch {
                primary: expr.span(self.body()).into(),
                scrutinee_ty,
                missing_patterns,
            };
            self.push_diag(diag);
        }

        ExprProp::new(match_ty, true)
    }

    fn check_assign(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Assign(lhs, rhs) = expr_data else {
            unreachable!()
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        self.check_expr(*rhs, lhs_ty);

        let result_ty = TyId::unit(self.db);

        self.check_assign_lhs(*lhs, &typed_lhs);

        ExprProp::new(result_ty, true)
    }

    fn check_aug_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::AugAssign(lhs, rhs, op) = expr_data else {
            unreachable!()
        };

        let unit = ExprProp::new(TyId::unit(self.db), true);

        let typed_lhs = self.check_expr_unknown(*lhs);
        let lhs_ty = typed_lhs.ty;
        if lhs_ty.has_invalid(self.db) {
            return unit;
        }
        self.check_assign_lhs(*lhs, &typed_lhs);

        // Avoid 'type must be known' diagnostics for unknown integer ty
        if lhs_ty.is_integral_var(self.db) {
            self.check_expr(*rhs, lhs_ty);
            return unit;
        }

        let lhs_base_ty = lhs_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return unit;
        }

        self.check_ops_trait(expr, lhs_ty, &AugAssignOp(*op), Some(*rhs));

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
    ) -> ExprProp<'db> {
        let Some(trait_def) = self.resolve_core_trait(op.trait_path(self.db)) else {
            panic!("failed to resolve core::ops trait");
        };

        let c_lhs_ty = Canonicalized::new(self.db, lhs_ty);

        let (method, inst) = match select_method_candidate(
            self.db,
            c_lhs_ty.value,
            op.trait_method(self.db),
            self.env.scope(),
            self.env.assumptions(),
            Some(trait_def),
        ) {
            Ok(MethodCandidate::InherentMethod(_)) => unreachable!(),
            Ok(
                res @ (MethodCandidate::TraitMethod(cand)
                | MethodCandidate::NeedsConfirmation(cand)),
            ) => {
                let inst = c_lhs_ty.extract_solution(&mut self.table, cand.inst);
                if matches!(res, MethodCandidate::NeedsConfirmation(_)) {
                    self.env
                        .register_confirmation(inst, expr.span(self.body()).into());
                }

                let func_ty = super::instantiate_trait_method(
                    self.db,
                    cand.method,
                    &mut self.table,
                    lhs_ty,
                    inst,
                );

                if let Some(rhs_expr) = rhs_expr {
                    // Derive expected RHS type from the instantiated function type
                    let (base, gen_args) = func_ty.decompose_ty_app(self.db);
                    if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
                        let mut expected_rhs =
                            func_def.arg_tys(self.db)[1].instantiate(self.db, gen_args);
                        let mut subst = AssocTySubst::new(inst);
                        expected_rhs =
                            self.normalize_ty(expected_rhs.fold_with(self.db, &mut subst));
                        self.check_expr(rhs_expr, expected_rhs);
                    }
                }

                (func_ty, inst)
            }
            Err(MethodSelectionError::AmbiguousTraitMethod(insts)) => {
                let Some(rhs_expr) = rhs_expr else {
                    unreachable!("unary core::ops ambiguity");
                };

                let rhs = self.check_expr_unknown(rhs_expr);
                if rhs.ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                let method_ident = op.trait_method(self.db);
                let trait_method = *trait_def.method_defs(self.db).get(&method_ident).unwrap();

                let mut viable: Vec<(TyId<'db>, TraitInstId<'db>, TyId<'db>)> = Vec::new();
                for inst in insts.iter().copied() {
                    let snapshot = self.table.snapshot();
                    let candidate_func_ty = super::instantiate_trait_method(
                        self.db,
                        trait_method,
                        &mut self.table,
                        lhs_ty,
                        inst,
                    );
                    let (base, gen_args) = candidate_func_ty.decompose_ty_app(self.db);
                    let expected_rhs =
                        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
                            let mut subst = AssocTySubst::new(inst);
                            let ty = func_def.arg_tys(self.db)[1].instantiate(self.db, gen_args);
                            self.normalize_ty(ty.fold_with(self.db, &mut subst))
                        } else {
                            unreachable!("candidate func ty should be a func");
                        };
                    let unifies = self.table.unify(rhs.ty, expected_rhs).is_ok();
                    self.table.rollback_to(snapshot);
                    if unifies {
                        viable.push((candidate_func_ty, inst, expected_rhs));
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
                        let (func_ty, inst, expected_rhs) = viable.pop().unwrap();
                        self.env
                            .register_confirmation(inst, expr.span(self.body()).into());
                        self.unify_ty(Typeable::Expr(rhs_expr, rhs), rhs.ty, expected_rhs);
                        (func_ty, inst)
                    }
                    _ => {
                        self.push_diag(BodyDiag::AmbiguousTraitInst {
                            primary: expr.span(self.body()).into(),
                            cands: viable.into_iter().map(|(_, inst, _)| inst).collect(),
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
                unreachable!("unexpected error: {err:?}");
            }
        };

        let callable = Callable::new(self.db, method, expr.span(self.body()).into(), Some(inst))
            .expect("failed to create Callable for core::ops trait method");

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        self.env.register_callable(expr, callable);
        ExprProp::new(ret_ty, true)
    }

    fn check_assign_lhs(&mut self, lhs: ExprId, typed_lhs: &ExprProp<'db>) {
        if !self.is_assignable_expr(lhs) {
            let diag = BodyDiag::NonAssignableExpr(lhs.span(self.body()).into());
            self.push_diag(diag);

            return;
        }

        if !typed_lhs.is_mut {
            let binding = self.find_base_binding(lhs);
            let diag = match binding {
                Some(binding) => {
                    let (ident, def_span) = (
                        self.env.binding_name(binding),
                        self.env.binding_def_span(binding),
                    );

                    BodyDiag::ImmutableAssignment {
                        primary: lhs.span(self.body()).into(),
                        binding: Some((ident, def_span)),
                    }
                }

                None => BodyDiag::ImmutableAssignment {
                    primary: lhs.span(self.body()).into(),
                    binding: None,
                },
            };

            self.push_diag(diag);
        }
    }

    fn check_expr_in_new_scope(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        self.env.enter_scope(expr);
        let ty = self.check_expr(expr, expected);
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
    fn find_base_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Field(lhs, ..) => self.find_base_binding(*lhs),
            Expr::Bin(lhs, _rhs, op) if *op == BinOp::Index => self.find_base_binding(*lhs),
            Expr::Path(..) => self.env.typed_expr(expr)?.binding(),
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

        MethodSelectionError::AmbiguousTraitMethod(traits) => BodyDiag::AmbiguousTrait {
            primary: method.span,
            method_name: method.data,
            traits,
        }
        .into(),

        MethodSelectionError::NotFound => {
            let base_ty = receiver.data.base_ty(db);
            PathResDiag::MethodNotFound {
                primary: method.span,
                method_name: method.data,
                receiver: Either::Left(base_ty),
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
                            let from_prelude = name
                                .derivation
                                .use_stmt()
                                .map(|use_| use_.is_prelude_use(db))
                                .unwrap_or(false);
                            cand_spans.push((span, from_prelude));
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
        let Ok(reso) = resolve_name_res(db, res, None, path, scope, env.assumptions()) else {
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
        let directive = QueryDirective::new().disallow_lex();
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

    let query = EarlyNameQueryId::new(db, ident, env.body().scope(), QueryDirective::default());
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
    fn trait_path<'db>(&self, db: &'db dyn HirAnalysisDb) -> PathId<'db> {
        let path = core_ops_path(db);
        path.push_ident(db, self.trait_name(db))
    }

    fn trait_name<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.triple(db)[0]
    }

    fn trait_method<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.triple(db)[1]
    }

    fn op_symbol<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.triple(db)[2]
    }

    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3];
}

impl TraitOps for UnOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        let triple = match self {
            UnOp::Plus => ["UnaryPlus", "add", "+"],
            UnOp::Minus => ["Neg", "neg", "-"],
            UnOp::Not => ["Not", "not", "!"],
            UnOp::BitNot => ["BitNot", "bit_not", "~"],
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

impl TraitOps for BinOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        let triple = match self {
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
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

#[derive(Clone, Copy, Debug)]
struct AugAssignOp(ArithBinOp);

impl TraitOps for AugAssignOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        use ArithBinOp::*;
        let triple = match self.0 {
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
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

fn core_ops_path(db: &dyn HirAnalysisDb) -> PathId<'_> {
    let core = IdentId::new(db, "core".to_string());
    let ops_ = IdentId::new(db, "ops".to_string());
    PathId::from_ident(db, core).push_ident(db, ops_)
}
