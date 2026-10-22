use parser::ast::{self, SyntaxNodePtr, prelude::AstNode as _};

use super::FileLowerCtxt;
use crate::{
    hir_def::{
        Body, BodyKind, BodySourceMap, Cond, CondId, Expr, ExprId, FuncParamName, IdentId,
        NodeStore, Partial, Pat, PatId, PathId, Stmt, StmtId, TrackedItemId, TrackedItemVariant,
    },
    span::HirOrigin,
};

impl<'db> Body<'db> {
    pub(super) fn lower_ast(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        Self::lower_ast_with_variant(
            f_ctxt,
            Some(ast),
            None,
            TrackedItemVariant::FuncBody,
            BodyKind::FuncBody,
        )
    }

    /// The body of the function `func`, which binds its parameters.
    /// `initial_binders` are the names the function itself binds, so that quoted
    /// open names can be checked against them.
    pub(super) fn lower_func_body(
        f_ctxt: &mut FileLowerCtxt<'db>,
        func: &ast::Func,
        body: ast::BlockExpr,
        initial_binders: Vec<IdentId<'db>>,
    ) -> Self {
        let ast = ast::Expr::cast(body.syntax().clone());
        let id = f_ctxt.joined_id(TrackedItemVariant::FuncBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        ctxt.local_binders = initial_binders;
        let lets = ctxt.lower_param_lets(func.sig().params());
        let body_expr = Expr::lower_ast_after(&mut ctxt, ast.clone(), lets);
        ctxt.build(ast.as_ref(), body_expr, BodyKind::FuncBody)
    }

    pub(super) fn lower_ast_nameless(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        Self::lower_ast_with_variant(
            f_ctxt,
            Some(ast),
            None,
            TrackedItemVariant::NamelessBody,
            BodyKind::Anonymous,
        )
    }

    /// A body whose expression is `path`, an attribute argument that names a
    /// constant (`#[slot(BASE)]`).
    pub(super) fn lower_attr_path(
        f_ctxt: &mut FileLowerCtxt<'db>,
        path: ast::Path,
        arg: &ast::AttrArg,
    ) -> Self {
        let id = f_ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let path = PathId::lower_ast_partial(ctxt.f_ctxt, Some(path));
        let expr = ctxt.push_expr(
            Expr::Path(path),
            HirOrigin::Expanded(SyntaxNodePtr::new(arg.syntax())),
        );
        ctxt.build(None, expr, BodyKind::Anonymous)
    }

    /// A body that binds `params`, if it has any.
    pub(super) fn lower_ast_with_variant(
        f_ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Expr>,
        params: Option<ast::FuncParamList>,
        variant: TrackedItemVariant<'db>,
        body_kind: BodyKind,
    ) -> Self {
        let id = f_ctxt.joined_id(variant);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let lets = ctxt.lower_param_lets(params);
        let body_expr = Expr::lower_ast_after(&mut ctxt, ast.clone(), lets);
        ctxt.build(ast.as_ref(), body_expr, body_kind)
    }
}

pub(super) struct BodyCtxt<'ctxt, 'db> {
    pub(super) f_ctxt: &'ctxt mut FileLowerCtxt<'db>,
    pub(super) id: TrackedItemId<'db>,

    pub(super) stmts: NodeStore<StmtId, Partial<Stmt<'db>>>,
    pub(super) exprs: NodeStore<ExprId, Partial<Expr<'db>>>,
    pub(super) conds: NodeStore<CondId, Partial<Cond>>,
    pub(super) pats: NodeStore<PatId, Partial<Pat<'db>>>,
    pub(super) source_map: BodySourceMap,
    pub(super) active_binder_groups: Vec<(common::generation::FrozenRecordRef, usize)>,
    pub(super) local_binders: Vec<IdentId<'db>>,
}

impl<'ctxt, 'db> BodyCtxt<'ctxt, 'db> {
    pub(super) fn record_local_binder(
        &mut self,
        local_ident: IdentId<'db>,
        display_ident: Option<IdentId<'db>>,
    ) {
        self.local_binders.push(local_ident);
        if let Some(display) = display_ident {
            let text = display.data(self.f_ctxt.db()).to_string();
            self.f_ctxt.record_bound_name(&text);
        }
    }

    pub(super) fn validate_frame_open_names(
        &mut self,
        frame: &super::generated::ActiveQuoteFrame<'db>,
    ) {
        let db = self.f_ctxt.db();
        let file = frame.quote_top_mod.file(db);
        for (name, range) in &frame.open_names {
            let target_ident = self.f_ctxt.resolve_sub_quote_local_ident(name);
            if !self.local_binders.contains(&target_ident) {
                self.f_ctxt.file_scoped_error(
                    file,
                    *range,
                    format!("quoted open name `{name}` has no enclosing binder"),
                );
            }
        }
    }

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

    pub(super) fn push_cond(&mut self, cond: Cond) -> CondId {
        self.conds.push(Partial::Present(cond))
    }

    pub(super) fn push_missing_cond(&mut self) -> CondId {
        self.conds.push(Partial::Absent)
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

    /// `let pat = %idx` for each of `params` that destructures, which the body
    /// binding them begins with (see `FuncParamName::Destructured`).
    pub(super) fn lower_param_lets(&mut self, params: Option<ast::FuncParamList>) -> Vec<StmtId> {
        params
            .into_iter()
            .flatten()
            .enumerate()
            .filter_map(|(idx, param)| {
                let pat = Pat::lower_ast(self, param.pat()?);
                let name = FuncParamName::destructured(self.f_ctxt.db(), idx);
                let ptr = SyntaxNodePtr::new(param.syntax());
                let source = self.push_expr(
                    Expr::Path(Partial::Present(PathId::from_ident(self.f_ctxt.db(), name))),
                    HirOrigin::Expanded(ptr),
                );
                Some(self.push_stmt(
                    Stmt::Let(pat, None, Some(source), None),
                    HirOrigin::Expanded(ptr),
                ))
            })
            .collect()
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
            conds: NodeStore::new(),
            pats: NodeStore::new(),
            source_map: BodySourceMap::default(),
            active_binder_groups: Vec::new(),
            local_binders: Vec::new(),
        }
    }

    pub(super) fn build(
        self,
        ast: Option<&ast::Expr>,
        body_expr: ExprId,
        body_kind: BodyKind,
    ) -> Body<'db> {
        let origin = ast.map_or(HirOrigin::None, HirOrigin::raw);
        let body = Body::new(
            self.f_ctxt.db(),
            self.id,
            body_expr,
            body_kind,
            self.stmts,
            self.exprs,
            self.conds,
            self.pats,
            self.f_ctxt.top_mod(),
            self.f_ctxt.lazy_func(),
            self.source_map,
            origin,
        );

        self.f_ctxt.leave_item_scope(body);
        body
    }
}
