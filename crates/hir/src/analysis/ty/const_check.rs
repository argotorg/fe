use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use crate::analysis::ty::trait_def::resolve_trait_method_instance;
use crate::analysis::ty::trait_resolution::{Selection, TraitSolveCx};
use crate::analysis::ty::ty_check::{
    Callable, EffectArgLayoutView, EffectParamSite, EffectPassMode, TypedBody,
};
use crate::analysis::ty::ty_def::InvalidCause;
use crate::hir_def::{
    Body, CallableDef, Cond, CondId, Expr, ExprId, Func, Partial, Pat, Stmt, StmtId,
};
use crate::semantic::{EffectEnvView, EffectRequirementKey};
use crate::span::DynLazySpan;

/// Const transport admits immutable trait dictionaries, not storage providers.
pub(crate) fn const_effects_supported(db: &dyn HirAnalysisDb, func: Func<'_>) -> bool {
    if !func.has_effects(db) {
        return true;
    }
    if func.is_extern(db) {
        return false;
    }
    let requirements = EffectEnvView::new(EffectParamSite::Func(func)).requirements(db);
    requirements.len() == func.effects(db).data(db).len()
        && requirements.iter().all(|requirement| {
            !requirement.is_mut && matches!(requirement.key, EffectRequirementKey::Trait(_))
        })
}

pub(crate) fn check_const_fn_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    typed_body: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let Some(body) = func.body(db) else {
        return Vec::new();
    };

    let mut diags = check_const_body_expressions(db, body, typed_body);
    if !const_effects_supported(db, func) {
        diags.insert(
            0,
            BodyDiag::ConstFnEffectsNotAllowed(func.span().effects().into()).into(),
        );
    }
    diags
}

/// Shared const-language checks for function bodies and declaration predicates.
pub(crate) fn check_const_body_expressions<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    ConstFnChecker::run(db, body, typed_body).diags
}

/// The first call that a constant may not make, as the failure evaluation
/// reports at that call. A generic constant waits on its parameters before
/// reaching such a call, so constants are held to the const language before
/// they are evaluated, as `const fn` bodies are.
pub(crate) fn const_body_language_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &TypedBody<'db>,
) -> Option<InvalidCause<'db>> {
    ConstFnChecker::run(db, body, typed_body).failure
}

struct ConstFnChecker<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    diags: Vec<FuncBodyDiag<'db>>,
    failure: Option<InvalidCause<'db>>,
}

impl<'db, 'a> ConstFnChecker<'db, 'a> {
    fn run(db: &'db dyn HirAnalysisDb, body: Body<'db>, typed_body: &'a TypedBody<'db>) -> Self {
        let mut checker = Self {
            db,
            body,
            typed_body,
            diags: Vec::new(),
            failure: None,
        };
        checker.check_expr(body.expr(db));
        checker
    }

    fn push(&mut self, diag: BodyDiag<'db>, failure: InvalidCause<'db>) {
        self.diags.push(diag.into());
        self.failure.get_or_insert(failure);
    }

    /// Reports a callee that a `const fn` cannot call at `site`, and returns
    /// whether it reported one.
    fn check_callable(
        &mut self,
        primary: DynLazySpan<'db>,
        site: ExprId,
        callable: &Callable<'db>,
    ) -> bool {
        let Some(callee) = self.callable_func(callable) else {
            return false;
        };

        let (body, expr) = (self.body, site);
        let (diag, failure) = if !callee.is_const(self.db) {
            (
                BodyDiag::ConstFnNonConstCall {
                    primary,
                    callee: callable.callable_def(),
                },
                InvalidCause::ConstEvalNonConstCall { body, expr },
            )
        } else if !const_effects_supported(self.db, callee) {
            (
                BodyDiag::ConstFnEffectfulCall {
                    primary,
                    callee: callable.callable_def(),
                },
                InvalidCause::ConstEvalUnsupported { body, expr },
            )
        } else {
            return false;
        };
        self.push(diag, failure);
        true
    }

    fn callable_func(&self, callable: &Callable<'db>) -> Option<Func<'db>> {
        let CallableDef::Func(func) = callable.callable_def() else {
            return None;
        };
        if let Some(inst) = callable.trait_inst()
            && let Some(name) = func.name(self.db).to_opt()
            && let Selection::Unique(method) = resolve_trait_method_instance(
                self.db,
                TraitSolveCx::new(self.db, self.body.scope())
                    .with_assumptions(self.typed_body.assumptions()),
                inst,
                name,
            )
            && let Some(impl_func) = method.body()
        {
            return Some(impl_func);
        }
        Some(func)
    }

    fn check_call_target(&mut self, expr: ExprId) {
        let Some(callable) = self.typed_body.callable_expr(expr) else {
            return;
        };
        // One diagnostic per call: the providers a call passes are checked
        // only when the callee itself may be called.
        if self.check_callable(expr.span(self.body).into(), expr, callable) {
            return;
        }
        if self.typed_body.call_effect_args(expr).is_some_and(|args| {
            args.iter().any(|arg| {
                arg.required_mut
                    || arg.key_kind != super::effects::EffectKeyKind::Trait
                    || arg.pass_mode != EffectPassMode::ByValue
                    || arg.layout_view != EffectArgLayoutView::Direct
                    || arg.provider.is_some_and(|space| {
                        space != crate::analysis::ty::provider::ProviderAddressSpace::Memory
                    })
                    || arg.provider_target_ty.is_some()
            })
        }) {
            self.push(
                BodyDiag::ConstFnEffectfulCall {
                    primary: expr.span(self.body).into(),
                    callee: callable.callable_def(),
                },
                InvalidCause::ConstEvalUnsupported {
                    body: self.body,
                    expr,
                },
            );
        }
    }

    fn check_stmt(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            return;
        };

        match stmt_data {
            Stmt::Let(pat, _ty, init) => {
                self.check_let_pat(*pat);
                if let Some(init) = init {
                    self.check_expr(*init);
                }
            }
            Stmt::For(pat, iter, body, _) => {
                self.check_let_pat(*pat);
                self.check_expr(*iter);
                if let Some(seq) = self.typed_body.for_loop_seq(stmt) {
                    let span: DynLazySpan<'db> = stmt.span(self.body).into();
                    self.check_callable(span.clone(), *iter, &seq.len_callable);
                    self.check_callable(span, *iter, &seq.get_callable);
                }
                self.check_expr(*body);
            }
            Stmt::While(cond, body) => {
                self.check_cond(*cond);
                self.check_expr(*body);
            }
            Stmt::Continue | Stmt::Break => {}
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(*expr);
                }
            }
            Stmt::Expr(expr) => self.check_expr(*expr),
        }
    }

    fn check_let_pat(&mut self, pat: crate::hir_def::PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return;
        };

        match pat_data {
            Pat::WildCard | Pat::Rest => {}
            Pat::Lit(_) | Pat::Path(_, _) => {}
            Pat::Tuple(elems) | Pat::PathTuple(_, elems) => {
                elems.iter().for_each(|elem| self.check_let_pat(*elem));
            }
            Pat::Record(_, fields) => fields
                .iter()
                .for_each(|field| self.check_let_pat(field.pat)),
            Pat::Or(lhs, rhs) => {
                self.check_let_pat(*lhs);
                self.check_let_pat(*rhs);
            }
        }
    }

    fn check_expr(&mut self, expr: ExprId) {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return;
        };

        match expr_data {
            Expr::Lit(
                crate::hir_def::LitKind::Int(_)
                | crate::hir_def::LitKind::Bool(_)
                | crate::hir_def::LitKind::String(_),
            )
            | Expr::Path(_) => {}

            Expr::Block(stmts) => stmts.iter().for_each(|stmt| self.check_stmt(*stmt)),

            Expr::Bin(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.check_call_target(expr);
            }

            Expr::Un(inner, _) => {
                self.check_expr(*inner);
                self.check_call_target(expr);
            }

            Expr::Field(inner, _) | Expr::ArrayRep(inner, _) | Expr::Cast(inner, _) => {
                self.check_expr(*inner);
            }

            Expr::If(cond, then, else_) => {
                self.check_cond(*cond);
                self.check_expr(*then);
                if let Some(else_) = else_ {
                    self.check_expr(*else_);
                }
            }
            Expr::Call(_callee, args) => {
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::UnsupportedMacroCall => {}
            Expr::Assert(args) => args.iter().for_each(|arg| self.check_expr(arg.expr)),
            Expr::MethodCall(receiver, _name, _generic_args, args) => {
                self.check_expr(*receiver);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::Match(scrutinee, arms) => {
                self.check_expr(*scrutinee);
                if let Some(arms) = arms.clone().to_opt() {
                    arms.iter().for_each(|arm| {
                        self.check_match_pat(arm.pat);
                        self.check_expr(arm.body);
                    });
                }
            }
            Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.check_call_target(expr);
            }
            Expr::With(bindings, body) => {
                for binding in bindings {
                    self.check_expr(binding.value);
                }
                self.check_expr(*body);
            }
            Expr::RecordInit(_path, fields) => {
                fields.iter().for_each(|field| self.check_expr(field.expr));
            }

            Expr::Tuple(elems) | Expr::Array(elems) => {
                elems.iter().for_each(|elem| self.check_expr(*elem));
            }
        }
    }

    fn check_cond(&mut self, cond: CondId) {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            return;
        };

        match cond_data {
            Cond::Expr(expr) => self.check_expr(*expr),
            Cond::Let(pat, value) => {
                self.check_let_pat(*pat);
                self.check_expr(*value);
            }
            Cond::Bin(lhs, rhs, _) => {
                self.check_cond(*lhs);
                self.check_cond(*rhs);
            }
        }
    }

    fn check_match_pat(&mut self, pat: crate::hir_def::PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return;
        };

        match pat_data {
            Pat::WildCard | Pat::Rest => {}
            Pat::Lit(_) | Pat::Path(_, _) => {}
            Pat::Tuple(elems) | Pat::PathTuple(_, elems) => {
                elems.iter().for_each(|elem| self.check_match_pat(*elem));
            }
            Pat::Record(_, fields) => fields
                .iter()
                .for_each(|field| self.check_match_pat(field.pat)),
            Pat::Or(lhs, rhs) => {
                self.check_match_pat(*lhs);
                self.check_match_pat(*rhs);
            }
        }
    }
}
