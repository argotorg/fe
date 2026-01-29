use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use crate::analysis::ty::ty_check::TypedBody;
use crate::hir_def::{Body, CallableDef, Expr, ExprId, Func, Partial, Pat, Stmt, StmtId};

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

    fn check_call_target(&mut self, expr: ExprId) {
        let Some(callable) = self.typed_body.callable_expr(expr) else {
            return;
        };
        let CallableDef::Func(callee) = callable.callable_def else {
            self.push(BodyDiag::ConstFnAggregateNotAllowed(
                expr.span(self.body).into(),
            ));
            return;
        };

        if !callee.is_const(self.db) {
            self.push(BodyDiag::ConstFnNonConstCall {
                primary: expr.span(self.body).into(),
                callee: callable.callable_def,
            });
        } else if callee.has_effects(self.db) {
            self.push(BodyDiag::ConstFnEffectfulCall {
                primary: expr.span(self.body).into(),
                callee: callable.callable_def,
            });
        }
    }

    fn check_stmt(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            return;
        };

        match stmt_data {
            Stmt::Let(pat, _ty, init) => {
                self.check_pat_simple(*pat);
                if let Some(init) = init {
                    self.check_expr(*init);
                }
            }
            Stmt::For(..) | Stmt::While(..) | Stmt::Continue | Stmt::Break => {
                self.push(BodyDiag::ConstFnLoopNotAllowed(stmt.span(self.body).into()));
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(*expr);
                }
            }
            Stmt::Expr(expr) => self.check_expr(*expr),
        }
    }

    fn check_pat_simple(&mut self, pat: crate::hir_def::PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return;
        };

        match pat_data {
            Pat::WildCard => {}
            Pat::Path(_, is_mut) => {
                if *is_mut {
                    self.push(BodyDiag::ConstFnMutableBindingNotAllowed(
                        pat.span(self.body).into(),
                    ));
                }
            }
            _ => self.push(BodyDiag::ConstFnAggregateNotAllowed(
                pat.span(self.body).into(),
            )),
        }
    }

    fn check_expr(&mut self, expr: ExprId) {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return;
        };

        match expr_data {
            Expr::Lit(crate::hir_def::LitKind::Int(_) | crate::hir_def::LitKind::Bool(_))
            | Expr::Path(_) => {}

            Expr::Lit(_) => self.push(BodyDiag::ConstFnAggregateNotAllowed(
                expr.span(self.body).into(),
            )),

            Expr::Block(stmts) => stmts.iter().for_each(|stmt| self.check_stmt(*stmt)),

            Expr::Bin(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
            }

            Expr::Un(inner, _)
            | Expr::Field(inner, _)
            | Expr::ArrayRep(inner, _)
            | Expr::Cast(inner, _) => {
                self.check_expr(*inner);
            }

            Expr::If(cond, then, else_) => {
                self.check_expr(*cond);
                self.check_expr(*then);
                if let Some(else_) = else_ {
                    self.check_expr(*else_);
                }
            }
            Expr::Call(_callee, args) => {
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::MethodCall(receiver, _name, _generic_args, args) => {
                self.check_expr(*receiver);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::Match(scrutinee, arms) => {
                self.check_expr(*scrutinee);
                if let Some(arms) = arms.clone().to_opt() {
                    arms.iter().for_each(|arm| self.check_expr(arm.body));
                }
                self.push(BodyDiag::ConstFnMatchNotAllowed(
                    expr.span(self.body).into(),
                ));
            }
            Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.push(BodyDiag::ConstFnAssignmentNotAllowed(
                    expr.span(self.body).into(),
                ));
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
        }
    }
}
