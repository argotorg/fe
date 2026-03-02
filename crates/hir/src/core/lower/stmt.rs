use parser::ast::{self, AttrListOwner, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{Cond, Expr, Pat, TypeId, stmt::*},
    span::HirOrigin,
};

impl<'db> Stmt<'db> {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Stmt) -> StmtId {
        let (stmt, origin_kind) = match ast.kind() {
            ast::StmtKind::Let(let_) => {
                let pat = Pat::lower_ast_opt(ctxt, let_.pat());
                let ty = let_
                    .type_annotation()
                    .map(|ty| TypeId::lower_ast(ctxt.f_ctxt, ty));
                let init = let_.initializer().map(|init| Expr::lower_ast(ctxt, init));
                (Stmt::Let(pat, ty, init), HirOrigin::raw(&ast))
            }
            ast::StmtKind::For(for_) => {
                let bind = Pat::lower_ast_opt(ctxt, for_.pat());
                let iter = Expr::push_to_body_opt(ctxt, for_.iterable());
                let body = Expr::push_to_body_opt(
                    ctxt,
                    for_.body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );

                // Check for #[unroll] or #[no_unroll] attribute
                let unroll_hint = for_.attr_list().and_then(|attrs| {
                    for attr in attrs.normal_attrs() {
                        if let Some(path) = attr.path() {
                            let name = path.text();
                            if name == "unroll" {
                                return Some(true); // Force unroll
                            } else if name == "no_unroll" {
                                return Some(false); // Prevent unroll
                            }
                        }
                    }
                    None // No unroll-related attribute, use auto heuristics
                });

                (
                    Stmt::For(bind, iter, body, unroll_hint),
                    HirOrigin::raw(&ast),
                )
            }

            ast::StmtKind::While(while_) => {
                let cond = Cond::push_to_body_opt(ctxt, while_.cond());
                let body = Expr::push_to_body_opt(
                    ctxt,
                    while_
                        .body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );

                (Stmt::While(cond, body), HirOrigin::raw(&ast))
            }

            ast::StmtKind::Continue(_) => (Stmt::Continue, HirOrigin::raw(&ast)),

            ast::StmtKind::Break(_) => (Stmt::Break, HirOrigin::raw(&ast)),

            ast::StmtKind::Return(ret) => {
                let expr = ret
                    .has_value()
                    .then(|| Expr::push_to_body_opt(ctxt, ret.expr()));
                (Stmt::Return(expr), HirOrigin::raw(&ast))
            }

            ast::StmtKind::Expr(expr) => {
                let expr = Expr::push_to_body_opt(ctxt, expr.expr());
                (Stmt::Expr(expr), HirOrigin::raw(&ast))
            }
        };

        ctxt.push_stmt(stmt, origin_kind)
    }
}
