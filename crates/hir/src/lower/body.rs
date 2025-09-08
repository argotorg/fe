use parser::ast;

use super::FileLowerCtxt;
use crate::{
    hir_def::{
        params::{GenericArg, GenericArgListId},
        Body, BodyKind, BodyPathIndex, BodySourceMap, Expr, ExprId, NodeStore, Partial, Pat, PatId,
        PathId, Stmt, StmtId, TrackedItemId, TrackedItemVariant, TupleTypeId, TypeId, TypeKind,
    },
    span::HirOrigin,
};

impl<'db> Body<'db> {
    pub(super) fn lower_ast(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        let id = f_ctxt.joined_id(TrackedItemVariant::FuncBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let body_expr = Expr::lower_ast(&mut ctxt, ast.clone());
        ctxt.build(&ast, body_expr, BodyKind::FuncBody)
    }

    pub(super) fn lower_ast_nameless(f_ctxt: &mut FileLowerCtxt<'db>, ast: ast::Expr) -> Self {
        let id = f_ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let body_expr = Expr::lower_ast(&mut ctxt, ast.clone());
        ctxt.build(&ast, body_expr, BodyKind::Anonymous)
    }
}

pub(super) struct BodyCtxt<'ctxt, 'db> {
    pub(super) f_ctxt: &'ctxt mut FileLowerCtxt<'db>,
    pub(super) id: TrackedItemId<'db>,

    pub(super) stmts: NodeStore<StmtId, Partial<Stmt<'db>>>,
    pub(super) exprs: NodeStore<ExprId, Partial<Expr<'db>>>,
    pub(super) pats: NodeStore<PatId, Partial<Pat<'db>>>,
    pub(super) source_map: BodySourceMap,
    pub(super) path_index: BodyPathIndex<'db>,
}

impl<'ctxt, 'db> BodyCtxt<'ctxt, 'db> {
    pub(super) fn push_expr(&mut self, expr: Expr<'db>, origin: HirOrigin<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Partial::Present(expr));
        self.source_map.expr_map.insert(expr_id, origin);

        expr_id
    }

    /// Record all PathId occurrences reachable from a TypeId, including nested
    /// tuple/array elements and generic args on path segments.
    pub(super) fn record_type_paths(&mut self, ty: TypeId<'db>) {
        match ty.data(self.f_ctxt.db()) {
            TypeKind::Path(p) => {
                if let Partial::Present(pid) = p {
                    // Record the path itself.
                    let idx = self.path_index.entries.len();
                    self.path_index.entries.insert(idx, *pid);
                    // Record any type paths inside generic args of each segment.
                    self.record_path_generic_arg_types(*pid);
                }
            }
            TypeKind::Ptr(inner) => {
                if let Partial::Present(inner) = inner {
                    self.record_type_paths(*inner);
                }
            }
            TypeKind::Tuple(tup) => {
                self.record_tuple_type_paths(*tup);
            }
            TypeKind::Array(elem, _len) => {
                if let Partial::Present(elem) = elem {
                    self.record_type_paths(*elem);
                }
            }
            TypeKind::Never => {}
        }
    }

    fn record_tuple_type_paths(&mut self, tup: TupleTypeId<'db>) {
        for part in tup.data(self.f_ctxt.db()).iter() {
            if let Partial::Present(ty) = part {
                self.record_type_paths(*ty);
            }
        }
    }

    /// Walk generic args of each segment in a PathId and record type paths.
    fn record_path_generic_arg_types(&mut self, path: PathId<'db>) {
        let db = self.f_ctxt.db();
        let segs = path.len(db);
        for i in 0..segs {
            if let Some(seg) = path.segment(db, i) {
                let args = seg.generic_args(db);
                self.record_generic_arg_types(args);
            }
        }
    }

    /// Record type-paths present in a generic arg list.
    pub(super) fn record_generic_arg_types(&mut self, args: GenericArgListId<'db>) {
        let db = self.f_ctxt.db();
        for arg in args.data(db).iter() {
            match arg {
                GenericArg::Type(t) => {
                    if let Partial::Present(ty) = t.ty {
                        self.record_type_paths(ty);
                    }
                }
                GenericArg::AssocType(a) => {
                    if let Partial::Present(ty) = a.ty {
                        self.record_type_paths(ty);
                    }
                }
                GenericArg::Const(_c) => {
                    // no PathId inside const generic bodies yet
                }
            }
        }
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

    fn new(f_ctxt: &'ctxt mut FileLowerCtxt<'db>, id: TrackedItemId<'db>) -> Self {
        f_ctxt.enter_body_scope(id);
        Self {
            f_ctxt,
            id,
            stmts: NodeStore::new(),
            exprs: NodeStore::new(),
            pats: NodeStore::new(),
            source_map: BodySourceMap::default(),
            path_index: BodyPathIndex::default(),
        }
    }

    fn build(self, ast: &ast::Expr, body_expr: ExprId, body_kind: BodyKind) -> Body<'db> {
        let origin = HirOrigin::raw(ast);
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
            self.path_index,
        );

        self.f_ctxt.leave_item_scope(body);
        body
    }
}
