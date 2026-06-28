use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use crate::analysis::ty::trait_def::resolve_trait_method_instance;
use crate::analysis::ty::trait_resolution::ProvisionEnv;
use crate::analysis::ty::ty_check::{Callable, TypedBody};
use crate::hir_def::{
    Body, CallableDef, Cond, CondId, Expr, ExprId, Func, Partial, Pat, Stmt, StmtId,
};
use crate::span::DynLazySpan;

pub(crate) fn check_const_fn_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    typed_body: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let Some(body) = func.body(db) else {
        return Vec::new();
    };

    let mut checker = ConstFnChecker {
        db,
        body,
        typed_body,
        diags: Vec::new(),
    };

    if func.has_effects(db) {
        checker
            .diags
            .push(BodyDiag::ConstFnEffectsNotAllowed(func.span().effects().into()).into());
    }

    checker.check_expr(body.expr(db));
    checker.diags
}

struct ConstFnChecker<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> ConstFnChecker<'db, '_> {
    fn push(&mut self, diag: BodyDiag<'db>) {
        self.diags.push(diag.into());
    }

    fn check_callable(&mut self, primary: DynLazySpan<'db>, callable: &Callable<'db>) {
        let Some(callee) = self.callable_func(callable) else {
            return;
        };

        if !callee.is_const(self.db) {
            self.push(BodyDiag::ConstFnNonConstCall {
                primary,
                callee: callable.callable_def(),
            });
        } else if callee.has_effects(self.db) {
            self.push(BodyDiag::ConstFnEffectfulCall {
                primary,
                callee: callable.callable_def(),
            });
        }
    }

    fn callable_func(&self, callable: &Callable<'db>) -> Option<Func<'db>> {
        let CallableDef::Func(func) = callable.callable_def() else {
            return None;
        };
        if let Some(inst) = callable.trait_inst()
            && let Some(name) = func.name(self.db).to_opt()
            && let Some(resolved) = resolve_trait_method_instance(
                self.db,
                ProvisionEnv::for_scope(self.body.scope(), self.typed_body.assumptions())
                    .solve_cx(self.db),
                inst,
                name,
            )
        {
            return Some(resolved.func);
        }
        Some(func)
    }

    fn check_call_target(&mut self, expr: ExprId) {
        if let Some(callable) = self.typed_body.callable_expr(expr) {
            self.check_callable(expr.span(self.body).into(), callable);
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
                    self.check_callable(span.clone(), &seq.len_callable);
                    self.check_callable(span, &seq.get_callable);
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
            // Splice hole pats only occur inside quote bodies, which this
            // pass never enters (see the quote arms in `check_expr`).
            Pat::QuoteHole(..) => {}
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
            Expr::With(_bindings, _body) => {
                self.push(BodyDiag::ConstFnWithNotAllowed(expr.span(self.body).into()));
            }
            Expr::RecordInit(_path, fields) => {
                fields.iter().for_each(|field| self.check_expr(field.expr));
            }

            Expr::Tuple(elems) | Expr::Array(elems) => {
                elems.iter().for_each(|elem| self.check_expr(*elem));
            }

            // Quotes outside derive provider bodies are rejected by the
            // type checker (`QuoteOutsideProvider`); provider bodies are
            // exempt from this pass entirely.
            Expr::Quote { .. } | Expr::QuoteHole(..) | Expr::QuoteFieldHole(..) => {}
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
            // Splice hole pats only occur inside quote bodies, which this
            // pass never enters (see the quote arms in `check_expr`).
            Pat::QuoteHole(..) => {}
        }
    }
}
