use parser::ast::{self, SyntaxNodePtr, prelude::AstNode as _};

use super::FileLowerCtxt;
use crate::{
    hir_def::{
        BindingMarker, Body, BodyKind, BodySourceMap, Cond, CondId, Expr, ExprId, FuncParamName,
        IdentId, NodeStore, Partial, Pat, PatId, PathId, QuoteOrigins, RecordPatField, Stmt,
        StmtId, TopLevelMod, TrackedItemId, TrackedItemVariant,
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
        ctxt.local_binders = initial_binders
            .into_iter()
            .map(LocalBinder::certain)
            .collect();
        let lets = ctxt.lower_param_lets(func.sig().params());
        let body_expr = Expr::lower_ast_after(&mut ctxt, ast.clone(), lets);
        ctxt.build(ast.as_ref(), body_expr, BodyKind::FuncBody)
    }

    pub(super) fn lower_const_param_name(
        f_ctxt: &mut FileLowerCtxt<'db>,
        name: IdentId<'db>,
    ) -> Self {
        let id = f_ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let path = PathId::from_ident(ctxt.f_ctxt.db(), name);
        let expr = ctxt.push_expr(Expr::Path(Partial::Present(path)), HirOrigin::None);
        ctxt.build(None, expr, BodyKind::Anonymous)
    }

    /// A body whose expression is `path`, already bound.
    pub(super) fn lower_bound_path(
        f_ctxt: &mut FileLowerCtxt<'db>,
        path: Partial<PathId<'db>>,
    ) -> Self {
        let id = f_ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let expr = ctxt.push_expr(Expr::Path(path), HirOrigin::None);
        ctxt.build(None, expr, BodyKind::Anonymous)
    }

    /// A body whose expression is a frozen constant, `4 as usize`, as a
    /// scalar splice lowers it.
    pub(super) fn lower_frozen_scalar(
        f_ctxt: &mut FileLowerCtxt<'db>,
        scalar: &common::generation::FrozenScalar,
    ) -> Self {
        use crate::hir_def::{IntegerId, LitKind, TypeId, TypeKind, UnOp, prim_ty::PrimTy};
        use common::generation::FrozenScalar;
        let id = f_ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut ctxt = BodyCtxt::new(f_ctxt, id);
        let db = ctxt.f_ctxt.db();
        let expr = match scalar {
            FrozenScalar::Bool(value) => {
                ctxt.push_expr(Expr::Lit(LitKind::Bool(*value)), HirOrigin::None)
            }
            FrozenScalar::Int {
                negative,
                bytes,
                primitive,
            } => {
                let integer = IntegerId::new(db, num_bigint::BigUint::from_bytes_be(bytes));
                let mut value = ctxt.push_expr(Expr::Lit(LitKind::Int(integer)), HirOrigin::None);
                if *negative {
                    value = ctxt.push_expr(Expr::Un(value, UnOp::Minus), HirOrigin::None);
                }
                let primitive = PrimTy::from_name(db, IdentId::new(db, primitive.clone()))
                    .expect("validated frozen integer primitive");
                let ty = TypeId::new(
                    db,
                    TypeKind::Primitive(primitive, crate::hir_def::GenericArgListId::none(db)),
                );
                ctxt.push_expr(Expr::Cast(value, Partial::Present(ty)), HirOrigin::None)
            }
        };
        ctxt.build(None, expr, BodyKind::Anonymous)
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
    pub(super) local_binders: Vec<LocalBinder<'db>>,
    /// How many quotations were active where this body began.
    base_frames: usize,
    /// The quotations around each expression lowered inside a fragment that
    /// the body splices, which the body's own quotations do not describe.
    spliced: Vec<(ExprId, Vec<TopLevelMod<'db>>)>,
    /// The same for each pattern lowered inside such a fragment.
    spliced_pats: Vec<(PatId, Vec<TopLevelMod<'db>>)>,
}

/// A name in scope, for checking the open names of the quotes spliced there.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct LocalBinder<'db> {
    pub(super) ident: IdentId<'db>,
    /// The pattern of a bare name such as `V` in `Some(V)`. It binds only if
    /// it names no constant or unit variant, which type checking decides by
    /// resolution, as for any pattern.
    pub(super) bare_pat: Option<PatId>,
}

impl<'db> LocalBinder<'db> {
    pub(super) fn certain(ident: IdentId<'db>) -> Self {
        Self {
            ident,
            bare_pat: None,
        }
    }
}

/// Whether `binders` bind `ident`: `Some` with no patterns if one certainly
/// does, `Some(pats)` if only the bare pattern names `pats` may, and `None` if
/// none does.
pub(super) fn open_name_binders<'db>(
    binders: &[LocalBinder<'db>],
    ident: IdentId<'db>,
) -> Option<Vec<PatId>> {
    let mut pats = Vec::new();
    for binder in binders.iter().filter(|binder| binder.ident == ident) {
        match binder.bare_pat {
            Some(pat) => pats.push(pat),
            None => return Some(Vec::new()),
        }
    }
    (!pats.is_empty()).then_some(pats)
}

impl<'ctxt, 'db> BodyCtxt<'ctxt, 'db> {
    pub(super) fn record_local_binder(
        &mut self,
        local_ident: IdentId<'db>,
        display_ident: Option<IdentId<'db>>,
    ) {
        self.record_binder(LocalBinder::certain(local_ident), display_ident);
    }

    /// Records the bare pattern name `pat`, which may name a constant or unit
    /// variant instead of binding.
    pub(super) fn record_bare_pat_binder(
        &mut self,
        local_ident: IdentId<'db>,
        display_ident: Option<IdentId<'db>>,
        pat: PatId,
    ) {
        self.record_binder(
            LocalBinder {
                ident: local_ident,
                bare_pat: Some(pat),
            },
            display_ident,
        );
    }

    /// Records the name that the path pattern `pat` binds, if its path is a
    /// bare name: certainly with a binding marker, and as a bare pattern name
    /// without one.
    pub(super) fn record_path_pat_binder(
        &mut self,
        path: Partial<PathId<'db>>,
        marker: BindingMarker,
        pat: PatId,
    ) {
        let Partial::Present(path_id) = path else {
            return;
        };
        let db = self.f_ctxt.db();
        if path_id.parent(db).is_some() || !path_id.generic_args(db).is_empty(db) {
            return;
        }
        let Some(local_ident) = path_id.local_ident(db) else {
            return;
        };
        let display_ident = path_id.ident(db).to_opt();
        if marker == BindingMarker::Plain {
            self.record_bare_pat_binder(local_ident, display_ident, pat);
        } else {
            self.record_local_binder(local_ident, display_ident);
        }
    }

    fn record_binder(&mut self, binder: LocalBinder<'db>, display_ident: Option<IdentId<'db>>) {
        self.local_binders.push(binder);
        if let Some(display) = display_ident
            && binder.ident == display
        {
            let text = display.data(self.f_ctxt.db()).to_string();
            self.f_ctxt.record_bound_name(&text);
        }
    }

    /// Checks that the names in scope bind the open name `name` of a quote
    /// written in `file`. When only bare pattern names may bind it, type
    /// checking decides once it has resolved them.
    pub(super) fn check_open_name(
        &mut self,
        binding: Option<Vec<PatId>>,
        name: &str,
        file: common::file::File,
        range: parser::TextRange,
        message: String,
    ) {
        match binding {
            None => self.f_ctxt.file_scoped_error(file, range, message),
            Some(pats) if !pats.is_empty() => {
                let requirement = crate::hir_def::OpenNameBinders {
                    name: name.to_string(),
                    pats,
                };
                if !self.source_map.open_name_binders.contains(&requirement) {
                    self.source_map.open_name_binders.push(requirement);
                }
            }
            Some(_) => {}
        }
    }

    pub(super) fn validate_open_name_use(&mut self, name: &str, range: parser::TextRange) {
        let db = self.f_ctxt.db();
        if let Some(frame) = self.f_ctxt.quote_frames.last()
            && frame.open_names.iter().any(|(opened, _)| opened == name)
        {
            let file = frame.quote_top_mod.file(db);
            let binding =
                open_name_binders(&self.local_binders, IdentId::new(db, name.to_string()));
            self.check_open_name(
                binding,
                name,
                file,
                range,
                format!("quoted open name `{name}` has no enclosing binder at this use"),
            );
        }
    }

    pub(super) fn validate_frame_open_names(
        &mut self,
        frame: &super::generated::ActiveQuoteFrame<'db>,
    ) {
        let db = self.f_ctxt.db();
        let file = frame.quote_top_mod.file(db);
        for (name, range) in &frame.open_names {
            let binding = open_name_binders(&self.local_binders, IdentId::new(db, name.clone()));
            self.check_open_name(
                binding,
                name,
                file,
                *range,
                format!("quoted open name `{name}` has no enclosing binder"),
            );
        }
    }

    fn current_quote_mod_idx(&mut self) -> Option<u16> {
        let idx = self.f_ctxt.current_quote_mod_idx()?;
        if !self.source_map.quote_mods.contains(&idx) {
            self.source_map.quote_mods.push(idx);
        }
        Some(idx)
    }

    /// Notes the quotations around `expr` if they are not the body's own: it
    /// is part of a fragment that the body splices.
    fn note_quote_chain(&mut self, expr: ExprId) {
        if self.f_ctxt.quote_frames.len() > self.base_frames {
            self.spliced.push((expr, self.f_ctxt.quote_chain()));
        }
    }

    /// Notes the quotations around `pat` if they are not the body's own.
    fn note_pat_quote_chain(&mut self, pat: PatId) {
        if self.f_ctxt.quote_frames.len() > self.base_frames {
            self.spliced_pats.push((pat, self.f_ctxt.quote_chain()));
        }
    }

    pub(super) fn push_expr(&mut self, expr: Expr<'db>, origin: HirOrigin<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Partial::Present(expr));
        let quote_mod = self.current_quote_mod_idx();
        self.source_map
            .expr_map
            .insert_with_quote_mod(expr_id, origin, quote_mod);
        self.note_quote_chain(expr_id);

        expr_id
    }

    pub(super) fn push_invalid_expr(&mut self, origin: HirOrigin<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Partial::Absent);
        let quote_mod = self.current_quote_mod_idx();
        self.source_map
            .expr_map
            .insert_with_quote_mod(expr_id, origin, quote_mod);
        self.note_quote_chain(expr_id);

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
        let quote_mod = self.current_quote_mod_idx();
        self.source_map
            .stmt_map
            .insert_with_quote_mod(stmt_id, origin, quote_mod);

        stmt_id
    }

    pub(super) fn push_pat(&mut self, pat: Pat<'db>, origin: HirOrigin<ast::Pat>) -> PatId {
        let pat_id = self.pats.push(Partial::Present(pat));
        let quote_mod = self.current_quote_mod_idx();
        self.source_map
            .pat_map
            .insert_with_quote_mod(pat_id, origin, quote_mod);
        self.note_pat_quote_chain(pat_id);
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

    /// `let pat = %label` for each field of `arm_pat`, the pattern of a recv
    /// arm lowered from `ast`, whose pattern destructures it, which the arm's
    /// body begins with. The arm's pattern binds such a field by its `%` name
    /// (see `FuncParamName::destructured_field`), so the arm receives each
    /// field it binds from whole.
    pub(super) fn lower_field_lets(
        &mut self,
        arm_pat: PatId,
        ast: Option<ast::Pat>,
    ) -> Vec<StmtId> {
        let (Some(ast::PatKind::Record(record)), Partial::Present(Pat::Record(path, fields))) =
            (ast.map(|ast| ast.kind()), self.pats[arm_pat].clone())
        else {
            return Vec::new();
        };
        let db = self.f_ctxt.db();
        let mut lets = Vec::new();
        let fields = fields
            .into_iter()
            .zip(record.fields().into_iter().flatten())
            .map(|(field, field_ast)| {
                let Partial::Present(label) = field.label else {
                    return field;
                };
                if matches!(
                    self.pats[field.pat],
                    Partial::Absent | Partial::Present(Pat::Path(..) | Pat::WildCard | Pat::Rest)
                ) || !self.has_name(field.pat)
                {
                    return field;
                }
                let name = PathId::from_ident(db, FuncParamName::destructured_field(db, label));
                let ptr = SyntaxNodePtr::new(field_ast.syntax());
                let pat = self.push_pat(
                    Pat::Path(Partial::Present(name), BindingMarker::Plain),
                    HirOrigin::Expanded(ptr),
                );
                let source =
                    self.push_expr(Expr::Path(Partial::Present(name)), HirOrigin::Expanded(ptr));
                lets.push(self.push_stmt(
                    Stmt::Let(field.pat, None, Some(source), None),
                    HirOrigin::Expanded(ptr),
                ));
                RecordPatField {
                    label: field.label,
                    pat,
                }
            })
            .collect();
        self.pats[arm_pat] = Partial::Present(Pat::Record(path, fields));
        lets
    }

    /// Whether `pat` has a lone name in it, which may bind.
    fn has_name(&self, pat: PatId) -> bool {
        match &self.pats[pat] {
            Partial::Present(Pat::Path(Partial::Present(path), _)) => {
                path.as_ident(self.f_ctxt.db()).is_some()
            }
            Partial::Present(Pat::Tuple(pats) | Pat::PathTuple(_, pats)) => {
                pats.iter().any(|&pat| self.has_name(pat))
            }
            Partial::Present(Pat::Record(_, fields)) => {
                fields.iter().any(|field| self.has_name(field.pat))
            }
            Partial::Present(Pat::Or(lhs, rhs)) => self.has_name(*lhs) || self.has_name(*rhs),
            Partial::Present(
                Pat::Path(Partial::Absent, _) | Pat::WildCard | Pat::Rest | Pat::Lit(_),
            )
            | Partial::Absent => false,
        }
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(Partial::Absent);
        self.source_map.pat_map.insert(pat_id, HirOrigin::None);
        pat_id
    }

    /// The quotations that wrote and placed this body, for it to keep.
    pub(super) fn take_quote_origins(&mut self) -> QuoteOrigins<'db> {
        QuoteOrigins {
            syntax_top_mod: self.f_ctxt.syntax_top_mod(),
            frames: self.f_ctxt.quote_chain(),
            spliced: std::mem::take(&mut self.spliced),
            spliced_pats: std::mem::take(&mut self.spliced_pats),
        }
    }

    pub(super) fn new(f_ctxt: &'ctxt mut FileLowerCtxt<'db>, id: TrackedItemId<'db>) -> Self {
        f_ctxt.enter_body_scope(id);
        let base_frames = f_ctxt.quote_frames.len();
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
            base_frames,
            spliced: Vec::new(),
            spliced_pats: Vec::new(),
        }
    }

    pub(super) fn build(
        mut self,
        ast: Option<&ast::Expr>,
        body_expr: ExprId,
        body_kind: BodyKind,
    ) -> Body<'db> {
        let origin = ast.map_or(HirOrigin::None, HirOrigin::raw);
        let quote_origins = self.take_quote_origins();
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
            quote_origins,
            self.source_map,
            origin,
        );

        self.f_ctxt.leave_item_scope(body);
        body
    }
}
