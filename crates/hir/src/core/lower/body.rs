use parser::ast;

use super::FileLowerCtxt;
use crate::{
    hir_def::{
        Body, BodyKind, BodySourceMap, Expr, ExprId, NodeStore, Partial, Pat, PatId, Stmt, StmtId,
        TrackedItemId, TrackedItemVariant,
    },
    span::HirOrigin,
};

impl<'db> Body<'db> {
    pub(super) fn lower_ast(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        Self::lower_ast_with_variant(
            f_ctxt,
            Some(ast),
            TrackedItemVariant::FuncBody,
            BodyKind::FuncBody,
        )
    }

    pub(super) fn lower_ast_nameless(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        Self::lower_ast_with_variant(
            f_ctxt,
            Some(ast),
            TrackedItemVariant::NamelessBody,
            BodyKind::Anonymous,
        )
    }

    pub(super) fn lower_ast_with_variant(
        f_ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Expr>,
        variant: TrackedItemVariant<'db>,
        body_kind: BodyKind,
    ) -> Self {
        let id = f_ctxt.joined_id(variant);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let body_expr = Expr::push_to_body_opt(&mut ctxt, ast.clone());
        ctxt.build(ast.as_ref(), body_expr, body_kind)
    }
}

pub(super) struct BodyCtxt<'ctxt, 'db> {
    pub(super) f_ctxt: &'ctxt mut FileLowerCtxt<'db>,
    pub(super) id: TrackedItemId<'db>,

    pub(super) stmts: NodeStore<StmtId, Partial<Stmt<'db>>>,
    pub(super) exprs: NodeStore<ExprId, Partial<Expr<'db>>>,
    pub(super) pats: NodeStore<PatId, Partial<Pat<'db>>>,
    pub(super) source_map: BodySourceMap,
}

impl<'ctxt, 'db> BodyCtxt<'ctxt, 'db> {
    pub(super) fn push_expr(&mut self, expr: Expr<'db>, origin: HirOrigin<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Partial::Present(expr));
        self.source_map.expr_map.insert(expr_id, origin);

        expr_id
    }

    pub(super) fn push_invalid_expr(&mut self, origin: HirOrigin<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Partial::Absent);
        self.source_map.expr_map.insert(expr_id, origin);

        expr_id
    }

    pub(super) fn push_missing_expr(&mut self) -> ExprId {
        let expr_id = self.exprs.push(Partial::Absent);
        self.source_map.expr_map.insert(expr_id, HirOrigin::None);
        expr_id
    }

    pub(super) fn push_stmt(&mut self, stmt: Stmt<'db>, origin: HirOrigin<ast::Stmt>) -> StmtId {
        let stmt_id = self.stmts.push(Partial::Present(stmt));
        self.source_map.stmt_map.insert(stmt_id, origin);

        stmt_id
    }

    pub(super) fn push_pat(&mut self, pat: Pat<'db>, origin: HirOrigin<ast::Pat>) -> PatId {
        let pat_id = self.pats.push(Partial::Present(pat));
        self.source_map.pat_map.insert(pat_id, origin);
        pat_id
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(Partial::Absent);
        self.source_map.pat_map.insert(pat_id, HirOrigin::None);
        pat_id
    }

    pub(super) fn new(f_ctxt: &'ctxt mut FileLowerCtxt<'db>, id: TrackedItemId<'db>) -> Self {
        f_ctxt.enter_body_scope(id);
        Self {
            f_ctxt,
            id,
            stmts: NodeStore::new(),
            exprs: NodeStore::new(),
            pats: NodeStore::new(),
            source_map: BodySourceMap::default(),
        }
    }

    pub(super) fn build(
        self,
        ast: Option<&ast::Expr>,
        body_expr: ExprId,
        body_kind: BodyKind,
    ) -> Body<'db> {
        let origin = ast.map(HirOrigin::raw).unwrap_or(HirOrigin::None);
        let body = Body::new(
            self.f_ctxt.db(),
            self.id,
            body_expr,
            body_kind,
            self.stmts,
            self.exprs,
            self.pats,
            self.f_ctxt.top_mod(),
            self.source_map,
            origin,
        );

        self.f_ctxt.leave_item_scope(body);
        body
    }
}
