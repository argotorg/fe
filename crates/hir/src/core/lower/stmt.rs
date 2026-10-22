use parser::ast::{self, AttrListOwner, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{AttrListId, Cond, Expr, Pat, TypeId, stmt::*},
    span::HirOrigin,
};

impl<'db> Stmt<'db> {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Stmt) -> StmtId {
        let (stmt, origin_kind) = match ast.kind() {
            ast::StmtKind::Let(let_) => {
                let saved_binder_len = ctxt.active_binder_groups.len();
                let saved_local_len = ctxt.local_binders.len();
                let pat = Pat::lower_let_ast(ctxt, let_.pat(), let_.var_token().is_some());
                let new_binder_groups: Vec<_> = ctxt
                    .active_binder_groups
                    .drain(saved_binder_len..)
                    .collect();
                let new_local_binders: Vec<_> =
                    ctxt.local_binders.drain(saved_local_len..).collect();
                let ty = let_
                    .type_annotation()
                    .map(|ty| TypeId::lower_ast(ctxt.f_ctxt, ty));
                let init = let_.initializer().map(|init| Expr::lower_ast(ctxt, init));
                let else_ = let_
                    .else_block()
                    .and_then(|block| ast::Expr::cast(block.syntax().clone()))
                    .map(|block| Expr::lower_ast(ctxt, block));
                ctxt.active_binder_groups.extend(new_binder_groups);
                ctxt.local_binders.extend(new_local_binders);
                (Stmt::Let(pat, ty, init, else_), HirOrigin::raw(&ast))
            }
            ast::StmtKind::For(for_) => {
                super::item::validate_for_loop_attrs(ctxt.f_ctxt, for_.attr_list());
                let saved_binder_len = ctxt.active_binder_groups.len();
                let saved_local_len = ctxt.local_binders.len();
                let bind = Pat::lower_ast_opt(ctxt, for_.pat());
                let new_binder_groups: Vec<_> = ctxt
                    .active_binder_groups
                    .drain(saved_binder_len..)
                    .collect();
                let new_local_binders: Vec<_> =
                    ctxt.local_binders.drain(saved_local_len..).collect();
                let iter = Expr::push_to_body_opt(ctxt, for_.iterable());
                ctxt.active_binder_groups.extend(new_binder_groups);
                ctxt.local_binders.extend(new_local_binders);
                let body = Expr::push_to_body_opt(
                    ctxt,
                    for_.body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );
                ctxt.active_binder_groups.truncate(saved_binder_len);
                ctxt.local_binders.truncate(saved_local_len);
                let unroll_hint = lower_loop_unroll_hint(ctxt, for_.attr_list());

                (
                    Stmt::For(bind, iter, body, unroll_hint),
                    HirOrigin::raw(&ast),
                )
            }

            ast::StmtKind::While(while_) => {
                let saved_binder_len = ctxt.active_binder_groups.len();
                let saved_local_len = ctxt.local_binders.len();
                let cond = Cond::push_to_body_opt(ctxt, while_.cond());
                let body = Expr::push_to_body_opt(
                    ctxt,
                    while_
                        .body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );
                ctxt.active_binder_groups.truncate(saved_binder_len);
                ctxt.local_binders.truncate(saved_local_len);

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

            ast::StmtKind::Yield(yield_) => {
                let expr = Expr::push_to_body_opt(ctxt, yield_.expr());
                (Stmt::Yield(expr), HirOrigin::raw(&ast))
            }

            ast::StmtKind::Expr(expr) => {
                let expr = Expr::push_to_body_opt(ctxt, expr.expr());
                (Stmt::Expr(expr), HirOrigin::raw(&ast))
            }
        };

        ctxt.push_stmt(stmt, origin_kind)
    }
}

fn lower_loop_unroll_hint<'db>(
    ctxt: &mut BodyCtxt<'_, 'db>,
    attrs: Option<ast::AttrList>,
) -> Option<bool> {
    let lowered_attrs = AttrListId::lower_ast_opt(ctxt.f_ctxt, attrs);
    let db = ctxt.f_ctxt.db();

    lowered_attrs.parse_loop_unroll_attr(db).unwrap_or_default()
}
