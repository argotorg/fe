use common::ingot::IngotKind;
use parser::ast::{self, SyntaxNodePtr, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{
        BindingMarker, Body, FuncParamListId, FuncParamName, GenericArgListId, IdentId, IntegerId,
        ItemKind, LitKind, Partial, Pat, PathId, Stmt, StmtId, StringId, TypeId, TypeKind, expr::*,
    },
    span::HirOrigin,
};

impl<'db> Expr<'db> {
    pub(super) fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Expr) -> ExprId {
        let expr = match ast.kind() {
            ast::ExprKind::Quote(quote) => {
                // Retain the original quote source for the checked constructor
                // to report unsupported syntax; never descend into nested holes.
                let holes = crate::core::meta::quote_holes(&quote)
                    .unwrap_or_default()
                    .into_iter()
                    .map(|hole| Self::push_to_body_opt(ctxt, hole.value()))
                    .collect();
                Self::ItemQuote(holes)
            }
            ast::ExprKind::QuoteHole(hole) => {
                use common::generation::{FrozenHoleValue, FrozenScalar};
                match ctxt.f_ctxt.current_hole(hole.syntax().text_range()) {
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Bool(value))) => {
                        Self::Lit(LitKind::Bool(value))
                    }
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Int {
                        negative,
                        bytes,
                        primitive,
                    })) => {
                        let integer = IntegerId::new(
                            ctxt.f_ctxt.db(),
                            num_bigint::BigUint::from_bytes_be(&bytes),
                        );
                        let mut value =
                            ctxt.push_expr(Self::Lit(LitKind::Int(integer)), HirOrigin::raw(&ast));
                        if negative {
                            value =
                                ctxt.push_expr(Self::Un(value, UnOp::Minus), HirOrigin::raw(&ast));
                        }
                        let primitive = crate::hir_def::prim_ty::PrimTy::from_name(
                            ctxt.f_ctxt.db(),
                            IdentId::new(ctxt.f_ctxt.db(), primitive),
                        )
                        .expect("validated frozen integer primitive");
                        let ty = TypeId::new(
                            ctxt.f_ctxt.db(),
                            crate::hir_def::TypeKind::Primitive(
                                primitive,
                                crate::hir_def::GenericArgListId::none(ctxt.f_ctxt.db()),
                            ),
                        );
                        Self::Cast(value, crate::hir_def::Partial::Present(ty))
                    }
                    Some(FrozenHoleValue::Field(_)) => {
                        let ident = ctxt
                            .f_ctxt
                            .lower_field_hole_ident(hole.syntax().text_range());
                        Self::Path(
                            ident
                                .to_opt()
                                .map(|ident| PathId::from_ident(ctxt.f_ctxt.db(), ident))
                                .into(),
                        )
                    }
                    Some(FrozenHoleValue::Code(code)) => {
                        return Self::lower_code(ctxt, &code, &ast);
                    }
                    _ => {
                        ctxt.f_ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "expression quotation hole requires Code<Expr> or scalar splice",
                        );
                        return ctxt.push_invalid_expr(HirOrigin::raw(&ast));
                    }
                }
            }
            ast::ExprKind::Lit(lit) => {
                if let Some(lit) = lit
                    .lit()
                    .and_then(|lit| LitKind::lower_ast(ctxt.f_ctxt, lit))
                {
                    Self::Lit(lit)
                } else {
                    return ctxt.push_invalid_expr(HirOrigin::raw(&ast));
                }
            }

            ast::ExprKind::Block(block) => {
                return Self::lower_block(ctxt, &ast, block, BlockKind::Normal, Vec::new());
            }

            ast::ExprKind::Closure(closure) => {
                let params = closure
                    .params()
                    .map(|params| FuncParamListId::lower_ast(ctxt.f_ctxt, params, false))
                    .unwrap_or_else(|| FuncParamListId::new(ctxt.f_ctxt.db(), Vec::new()));
                let ret_ty = closure
                    .ret_ty()
                    .map(|ret_ty| TypeId::lower_ast(ctxt.f_ctxt, ret_ty));
                let saved_local_len = ctxt.local_binders.len();
                // Within a spliced quote the uses of a binder carry its
                // hygienic name (`resolve_sub_quote_local_ident`), so a
                // parameter is bound under that name.
                let db = ctxt.f_ctxt.db();
                let mut binders = Vec::new();
                let mut hygienic = Vec::new();
                for param in params.data(db) {
                    let mut param = param.clone();
                    if let Some(written) = param.name() {
                        let local = ctxt.f_ctxt.resolve_sub_quote_local_ident(written.data(db));
                        param.name = Partial::Present(FuncParamName::Ident(local));
                        binders.push((local, written));
                    }
                    hygienic.push(param);
                }
                let params = FuncParamListId::new(db, hygienic);
                for (local, written) in binders {
                    ctxt.record_local_binder(local, Some(written));
                }
                let lets = ctxt.lower_param_lets(closure.params());
                let body = Self::lower_ast_after(ctxt, closure.body(), lets);
                ctxt.local_binders.truncate(saved_local_len);
                Self::Closure {
                    params,
                    ret_ty,
                    body,
                }
            }

            ast::ExprKind::Unsafe(unsafe_) => {
                return match unsafe_.block() {
                    Some(block) => {
                        Self::lower_block(ctxt, &ast, block, BlockKind::Unsafe, Vec::new())
                    }
                    None => ctxt.push_invalid_expr(HirOrigin::raw(&ast)),
                };
            }

            ast::ExprKind::Bin(bin) => {
                let lhs = Self::push_to_body_opt(ctxt, bin.lhs());
                let rhs = Self::push_to_body_opt(ctxt, bin.rhs());
                let op = bin.op().expect("parser guarantees op presence");
                let op = BinOp::lower_ast(op);
                Self::Bin(lhs, rhs, op)
            }

            ast::ExprKind::Un(un) => {
                let expr = Self::push_to_body_opt(ctxt, un.expr());
                let op = un.op().expect("parser guarantees op presence");
                let op = UnOp::lower_ast(op);
                Self::Un(expr, op)
            }

            ast::ExprKind::Cast(cast) => {
                let expr = Self::push_to_body_opt(ctxt, cast.expr());
                let ty = TypeId::lower_ast_partial(ctxt.f_ctxt, cast.ty());
                Self::Cast(expr, ty)
            }

            ast::ExprKind::Try(try_) => Self::Try(Self::push_to_body_opt(ctxt, try_.expr())),

            ast::ExprKind::Call(call) => {
                let args = call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::lower_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                let callee = Self::push_to_body_opt(ctxt, call.callee());
                Self::Call(callee, args)
            }

            ast::ExprKind::MacroCall(call) => {
                let name = builtin_macro_name(&call);
                if let Some(format) = name.as_deref().and_then(FormatMacro::from_name) {
                    return Self::lower_format_macro(ctxt, &ast, &call, format);
                }
                if name.as_deref() != Some("assert") {
                    return ctxt.push_expr(Self::UnsupportedMacroCall, HirOrigin::raw(&ast));
                }
                let args = call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::lower_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::Assert(args)
            }

            ast::ExprKind::MethodCall(method_call) => {
                let receiver = Self::push_to_body_opt(ctxt, method_call.receiver());
                let method_name =
                    IdentId::lower_token_partial(ctxt.f_ctxt, method_call.method_name());
                let generic_args =
                    GenericArgListId::lower_ast_opt(ctxt.f_ctxt, method_call.generic_args());
                let args = method_call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::lower_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::MethodCall(receiver, method_name, generic_args, args)
            }

            ast::ExprKind::Path(path) => {
                if let Some(authored) = path.path()
                    && let Some(segment) = authored.segments().next()
                    && let Some(ast::PathSegmentKind::Ident(ident)) = segment.kind()
                {
                    ctxt.validate_open_name_use(ident.text(), ident.text_range());
                }
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path.path());
                Self::Path(path)
            }

            ast::ExprKind::RecordInit(record_init) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, record_init.path());
                let fields = record_init
                    .fields()
                    .map(|fields| {
                        fields
                            .into_iter()
                            .map(|field| Field::lower_ast(ctxt, field))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::RecordInit(path, fields)
            }

            ast::ExprKind::Field(field) => {
                let receiver = Self::push_to_body_opt(ctxt, field.receiver());
                let field = if let Some(hole) = field.field_hole() {
                    ctxt.f_ctxt
                        .lower_field_hole_index(hole.syntax().text_range())
                } else if let Some(name) = field.field_name() {
                    Some(FieldIndex::Ident(IdentId::lower_token(ctxt.f_ctxt, name))).into()
                } else if let Some(num) = field.field_index() {
                    Some(FieldIndex::Index(IntegerId::lower_ast(ctxt.f_ctxt, num))).into()
                } else {
                    None.into()
                };
                Self::Field(receiver, field)
            }

            ast::ExprKind::Index(index) => {
                let indexed = Self::push_to_body_opt(ctxt, index.expr());
                let index = Self::push_to_body_opt(ctxt, index.index());
                Self::Bin(indexed, index, BinOp::Index)
            }

            ast::ExprKind::Tuple(tup) => {
                let elems = tup
                    .elems()
                    .map(|elem| Self::push_to_body_opt(ctxt, elem))
                    .collect();

                Self::Tuple(elems)
            }

            ast::ExprKind::Array(array) => {
                let elems = array
                    .elems()
                    .map(|elem| Self::push_to_body_opt(ctxt, elem))
                    .collect();
                Self::Array(elems)
            }

            ast::ExprKind::ArrayRep(array_rep) => {
                let val = Self::push_to_body_opt(ctxt, array_rep.val());
                let len = array_rep
                    .len()
                    .map(|ast| Body::lower_ast_nameless(ctxt.f_ctxt, ast))
                    .into();
                Self::ArrayRep(val, len)
            }

            ast::ExprKind::If(if_) => {
                let saved_binder_len = ctxt.active_binder_groups.len();
                let saved_local_len = ctxt.local_binders.len();
                let cond = Cond::push_to_body_opt(ctxt, if_.cond());
                let then = Expr::push_to_body_opt(
                    ctxt,
                    if_.then()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );
                ctxt.active_binder_groups.truncate(saved_binder_len);
                ctxt.local_binders.truncate(saved_local_len);
                let else_ = if_.else_().map(|ast| Self::lower_ast(ctxt, ast));
                Self::If(cond, then, else_)
            }

            ast::ExprKind::Match(match_) => {
                let scrutinee = Self::push_to_body_opt(ctxt, match_.scrutinee());
                let arm = match_
                    .arms()
                    .map(|arms| {
                        let mut out = Vec::new();
                        MatchArm::lower_arm_list(ctxt, arms, &mut out);
                        out
                    })
                    .into();

                Self::Match(scrutinee, arm)
            }

            ast::ExprKind::With(with_) => {
                // Lower `with (K = v, ..) { body }` and `with (v, ..) { body }`
                // into HIR::Expr::With(bindings, body)
                let mut bindings = Vec::new();
                if let Some(params) = with_.params() {
                    for p in params {
                        let value = Self::push_to_body_opt(ctxt, p.value_expr());
                        let key_path = if p.eq().is_some() {
                            // Lower key path directly so multi-segment paths are preserved.
                            Some(PathId::lower_ast_partial(ctxt.f_ctxt, p.path()))
                        } else {
                            None
                        };
                        bindings.push(super::super::hir_def::expr::WithBinding { key_path, value });
                    }
                }

                let body_expr = with_
                    .body()
                    .and_then(|b| ast::Expr::cast(b.syntax().clone()));
                let body = Self::push_to_body_opt(ctxt, body_expr);
                Self::With(bindings, body)
            }

            ast::ExprKind::Paren(paren) => {
                return Self::push_to_body_opt(ctxt, paren.expr());
            }

            ast::ExprKind::Assign(assign) => {
                let lhs = Self::push_to_body_opt(ctxt, assign.lhs_expr());
                let rhs = Self::push_to_body_opt(ctxt, assign.rhs_expr());
                Self::Assign(lhs, rhs)
            }

            ast::ExprKind::AugAssign(aug_assign) => {
                let lhs = Self::push_to_body_opt(ctxt, aug_assign.lhs_expr());
                let rhs = Self::push_to_body_opt(ctxt, aug_assign.rhs_expr());
                let op = aug_assign.op().expect("parser guarantees op presence");
                let op = ArithBinOp::lower_ast(op);
                Self::AugAssign(lhs, rhs, op)
            }

            // `let` expressions are condition-only and lowered through `Cond`.
            ast::ExprKind::Let(_) => return ctxt.push_invalid_expr(HirOrigin::raw(&ast)),
        };

        ctxt.push_expr(expr, HirOrigin::raw(&ast))
    }

    /// Lowers `block` as the expression `ast`, which is the block itself or
    /// the `unsafe` block wrapping it, its statements following `leading`.
    fn lower_block(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: &ast::Expr,
        block: ast::BlockExpr,
        kind: BlockKind,
        leading: Vec<StmtId>,
    ) -> ExprId {
        ctxt.f_ctxt.enter_block_scope();
        let saved_binder_len = ctxt.active_binder_groups.len();
        let saved_local_len = ctxt.local_binders.len();
        let mut stmts = leading;
        for stmt in block.stmts() {
            if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                && let Some(expr) = expr_stmt.expr()
                && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                && let Some(common::generation::FrozenHoleValue::Code(code)) =
                    ctxt.f_ctxt.current_hole(hole.syntax().text_range())
                && matches!(
                    code,
                    common::generation::FrozenCode::Empty {
                        kind: common::generation::FrozenCodeKind::Item
                    } | common::generation::FrozenCode::Template {
                        kind: common::generation::FrozenCodeKind::Item,
                        ..
                    }
                )
            {
                Self::lower_block_item_code(ctxt.f_ctxt, &code);
                continue;
            }
            stmts.push(Stmt::push_to_body(ctxt, stmt));
        }
        ctxt.active_binder_groups.truncate(saved_binder_len);
        ctxt.local_binders.truncate(saved_local_len);
        let expr_id = ctxt.push_expr(Self::Block(stmts, kind), HirOrigin::raw(ast));

        for item in block.items() {
            ItemKind::lower_ast(ctxt.f_ctxt, item);
        }

        ctxt.f_ctxt.leave_block_scope(expr_id);
        expr_id
    }

    fn lower_block_item_code(
        ctxt: &mut super::FileLowerCtxt<'db>,
        code: &common::generation::FrozenCode,
    ) {
        use common::generation::{FrozenCode, FrozenCodeKind, FrozenHoleValue};
        let FrozenCode::Template {
            kind: FrozenCodeKind::Item,
            quote_file,
            quote_range,
            producer_module,
            producer_block,
            holes,
        } = code
        else {
            return;
        };
        let (quote, frame) = ctxt.resolve_quote_template(
            quote_file,
            *quote_range,
            *producer_module,
            *producer_block,
            true,
            holes.clone(),
        );
        ctxt.quote_frames.push(frame);
        if let Some(item_list) = quote.item_list() {
            for child in item_list.syntax().children() {
                if let Some(item) = ast::Item::cast(child.clone()) {
                    ItemKind::lower_ast(ctxt, item);
                } else if let Some(hole) = ast::QuoteHole::cast(child)
                    && let Some(FrozenHoleValue::Code(nested)) =
                        ctxt.current_hole(hole.syntax().text_range())
                {
                    Self::lower_block_item_code(ctxt, &nested);
                }
            }
        } else if let Some(block) = quote.body() {
            for item in block.items() {
                ItemKind::lower_ast(ctxt, item);
            }
            for stmt in block.stmts() {
                if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                    && let Some(expr) = expr_stmt.expr()
                    && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                    && let Some(FrozenHoleValue::Code(nested)) =
                        ctxt.current_hole(hole.syntax().text_range())
                {
                    Self::lower_block_item_code(ctxt, &nested);
                }
            }
        }
        ctxt.quote_frames.pop();
    }

    pub(super) fn push_to_body_opt(ctxt: &mut BodyCtxt<'_, '_>, ast: Option<ast::Expr>) -> ExprId {
        if let Some(ast) = ast {
            Expr::lower_ast(ctxt, ast)
        } else {
            ctxt.push_missing_expr()
        }
    }

    /// `ast` begun by `lets`, the destructuring of its parameters: a block
    /// takes them before its own statements, and any other expression is
    /// the tail of a block of them.
    pub(super) fn lower_ast_after(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: Option<ast::Expr>,
        lets: Vec<StmtId>,
    ) -> ExprId {
        if lets.is_empty() {
            return Self::push_to_body_opt(ctxt, ast);
        }
        let Some(ast) = ast else {
            return ctxt.push_missing_expr();
        };
        if let ast::ExprKind::Block(block) = ast.kind() {
            return Self::lower_block(ctxt, &ast, block, BlockKind::Normal, lets);
        }
        ctxt.f_ctxt.enter_block_scope();
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let tail = Self::lower_ast(ctxt, ast);
        let tail = ctxt.push_stmt(Stmt::Expr(tail), HirOrigin::Expanded(ptr));
        let stmts = lets.into_iter().chain([tail]).collect();
        let block = ctxt.push_expr(
            Self::Block(stmts, BlockKind::Normal),
            HirOrigin::Expanded(ptr),
        );
        ctxt.f_ctxt.leave_block_scope(block);
        block
    }

    fn lower_code(
        ctxt: &mut BodyCtxt<'_, 'db>,
        code: &common::generation::FrozenCode,
        origin_ast: &ast::Expr,
    ) -> ExprId {
        use common::generation::{FrozenCode, FrozenCodeKind};
        match code {
            FrozenCode::VariantBinderField {
                record,
                group,
                field_index,
            } => {
                if !ctxt
                    .active_binder_groups
                    .iter()
                    .any(|(active_record, active_group)| {
                        active_record == record && *active_group == *group
                    })
                {
                    ctxt.f_ctxt.record_generation_error(
                        origin_ast.syntax().text_range(),
                        "binder group is not bound in this scope",
                    );
                    return ctxt.push_invalid_expr(HirOrigin::raw(origin_ast));
                }
                let ident =
                    IdentId::new(ctxt.f_ctxt.db(), format!("\0binder_{group}_{field_index}"));
                let path = PathId::from_ident(ctxt.f_ctxt.db(), ident);
                ctxt.push_expr(
                    Self::Path(crate::hir_def::Partial::Present(path)),
                    HirOrigin::raw(origin_ast),
                )
            }
            FrozenCode::Template {
                kind: FrozenCodeKind::Expr,
                quote_file,
                quote_range,
                producer_module,
                producer_block,
                holes,
            } => {
                let (quote, frame) = ctxt.f_ctxt.resolve_quote_template(
                    quote_file,
                    *quote_range,
                    *producer_module,
                    *producer_block,
                    true,
                    holes.clone(),
                );
                ctxt.validate_frame_open_names(&frame);
                ctxt.f_ctxt.quote_frames.push(frame);
                let expr_id = if let Some(block) = quote.body() {
                    let stmts = block.stmts().collect::<Vec<_>>();
                    if stmts.len() == 1
                        && block.items().next().is_none()
                        && let ast::StmtKind::Expr(expr_stmt) = stmts[0].kind()
                        && let Some(expr) = expr_stmt.expr()
                    {
                        Self::lower_ast(ctxt, expr)
                    } else {
                        Self::lower_block(ctxt, origin_ast, block, BlockKind::Normal, Vec::new())
                    }
                } else {
                    ctxt.f_ctxt.record_generation_error(
                        *quote_range,
                        "expression quotation requires a block expression body",
                    );
                    ctxt.push_invalid_expr(HirOrigin::raw(origin_ast))
                };
                ctxt.f_ctxt.quote_frames.pop();
                expr_id
            }
            FrozenCode::Construct { record, fields } => {
                Self::lower_construct(ctxt, record, fields, origin_ast)
            }
            FrozenCode::FieldStatements { fields } => {
                ctxt.f_ctxt.enter_block_scope();
                let ptr = SyntaxNodePtr::new(origin_ast.syntax());
                let mut stmts = Vec::with_capacity(fields.len() + 1);
                for field in fields {
                    let expr = Self::lower_code(ctxt, field, origin_ast);
                    stmts.push(ctxt.push_stmt(Stmt::Expr(expr), HirOrigin::Expanded(ptr)));
                }
                let unit = ctxt.push_expr(Self::Tuple(Vec::new()), HirOrigin::Expanded(ptr));
                stmts.push(ctxt.push_stmt(Stmt::Expr(unit), HirOrigin::Expanded(ptr)));
                let block = ctxt.push_expr(
                    Self::Block(stmts, BlockKind::Normal),
                    HirOrigin::Expanded(ptr),
                );
                ctxt.f_ctxt.leave_block_scope(block);
                block
            }
            _ => {
                ctxt.f_ctxt.record_generation_error(
                    origin_ast.syntax().text_range(),
                    "expression quotation hole requires Code<Expr> splice",
                );
                ctxt.push_invalid_expr(HirOrigin::raw(origin_ast))
            }
        }
    }

    /// The constructor of `record`, a struct or a selected variant, applied to
    /// `fields`, one initializer for each field in declaration order. The
    /// record and its variant are named by their declarations, so no name at
    /// the place of the splice can stand for them, and each initializer is
    /// lowered as the fragment it is, in its own quote frame.
    fn lower_construct(
        ctxt: &mut BodyCtxt<'_, 'db>,
        record: &common::generation::FrozenRecordRef,
        fields: &[common::generation::FrozenCode],
        origin_ast: &ast::Expr,
    ) -> ExprId {
        use super::AuthoredFields;
        use crate::hir_def::PathKind;

        let db = ctxt.f_ctxt.db();
        let authored = ctxt.f_ctxt.authored_record(record);
        let generic_args = match &record.args {
            Some(args) => GenericArgListId::thaw(ctxt.f_ctxt, args),
            None => GenericArgListId::none(db),
        };
        let record_path = authored.path(db, generic_args);
        let range = origin_ast.syntax().text_range();
        let Some(shape) = authored.fields(db, record.variant_index) else {
            ctxt.f_ctxt
                .record_generation_error(range, "invalid record for construct");
            return ctxt.push_invalid_expr(HirOrigin::raw(origin_ast));
        };
        let field_count = match &shape {
            AuthoredFields::Unit => 0,
            AuthoredFields::Tuple(len) => *len,
            AuthoredFields::Named(labels) => labels.len(),
        };
        if field_count != fields.len() {
            ctxt.f_ctxt.record_generation_error(
                range,
                "construct initializer count differs from its record",
            );
            return ctxt.push_invalid_expr(HirOrigin::raw(origin_ast));
        }
        let exprs: Vec<ExprId> = fields
            .iter()
            .map(|code| Self::lower_code(ctxt, code, origin_ast))
            .collect();
        let call = |ctxt: &mut BodyCtxt<'_, 'db>, callee: PathId<'db>| {
            let callee = ctxt.push_expr(
                Self::Path(Partial::Present(callee)),
                HirOrigin::raw(origin_ast),
            );
            let args = exprs
                .iter()
                .map(|&expr| CallArg { label: None, expr })
                .collect();
            ctxt.push_expr(Self::Call(callee, args), HirOrigin::raw(origin_ast))
        };
        let record_init = |ctxt: &mut BodyCtxt<'_, 'db>,
                           path: PathId<'db>,
                           labels: Vec<Partial<IdentId<'db>>>| {
            let fields = labels
                .into_iter()
                .zip(exprs.iter().copied())
                .map(|(label, expr)| Field {
                    label: label.to_opt(),
                    expr,
                })
                .collect();
            ctxt.push_expr(
                Self::RecordInit(Partial::Present(path), fields),
                HirOrigin::raw(origin_ast),
            )
        };
        match record.variant_index {
            Some(variant_index) => {
                let variant_name = authored
                    .variant(variant_index)
                    .and_then(|variant| variant.name())
                    .map(|name| IdentId::new(db, name.text().to_string()))
                    .into();
                let variant_path = PathId::new(
                    db,
                    PathKind::Ident {
                        ident: variant_name,
                        generic_args: GenericArgListId::none(db),
                    },
                    Some(record_path),
                );
                match shape {
                    AuthoredFields::Unit => ctxt.push_expr(
                        Self::Path(Partial::Present(variant_path)),
                        HirOrigin::raw(origin_ast),
                    ),
                    AuthoredFields::Tuple(_) => call(ctxt, variant_path),
                    AuthoredFields::Named(labels) => record_init(ctxt, variant_path, labels),
                }
            }
            None => match shape {
                AuthoredFields::Named(_) if authored.is_tuple_struct() => call(ctxt, record_path),
                AuthoredFields::Named(labels) => record_init(ctxt, record_path, labels),
                AuthoredFields::Unit | AuthoredFields::Tuple(_) => {
                    ctxt.f_ctxt
                        .record_generation_error(range, "invalid record for construct");
                    ctxt.push_invalid_expr(HirOrigin::raw(origin_ast))
                }
            },
        }
    }
}

/// The name of a builtin macro call, `name!(..)`.
fn builtin_macro_name(call: &ast::MacroCallExpr) -> Option<String> {
    let Some(ast::ExprKind::Path(callee)) = call.callee().map(|callee| callee.kind()) else {
        return None;
    };
    let mut segments = callee.path()?.segments();
    let segment = segments.next()?;
    if segments.next().is_some() || segment.generic_args().is_some() {
        return None;
    }
    let Some(ast::PathSegmentKind::Ident(ident)) = segment.kind() else {
        return None;
    };
    Some(ident.text().to_string())
}

/// The builtin macros that format their arguments: `format!`, `write!` and
/// the `print!` family.
#[derive(Clone, Copy)]
enum FormatMacro {
    Format,
    Write,
    Writeln,
    Print,
    Println,
    Eprint,
    Eprintln,
}

impl FormatMacro {
    fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "format" => Self::Format,
            "write" => Self::Write,
            "writeln" => Self::Writeln,
            "print" => Self::Print,
            "println" => Self::Println,
            "eprint" => Self::Eprint,
            "eprintln" => Self::Eprintln,
            _ => return None,
        })
    }
}

impl<'db> Expr<'db> {
    /// Expands `format!(a, b)` to
    /// `{ var %text = core::MemBuffer::empty(); std::io::Display::fmt(a, mut %text); ..; %text }`.
    /// Each argument is formatted by a statement of its own, so it may be a
    /// view. `print!` passes the text to `std::io::print_buffer` instead,
    /// `eprint!` to `std::io::eprint_buffer`, and their `ln` forms add a
    /// newline first.
    ///
    /// `write!(mut buf, a, b)` formats into `buf` itself, bound once before
    /// the arguments as `let %text: core::MemBuffer = mut buf`, as is a block
    /// that ends in such a grant, and `writeln!` adds a newline. A buffer
    /// written otherwise, or none, takes the text `format!` builds
    /// through `core::MemBuffer::extend_from(buf, %text.span())`, a call that
    /// checks it as the `mut` argument it must be, so `write!(buf, a)` asks
    /// for `mut buf`.
    fn lower_format_macro(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: &ast::Expr,
        call: &ast::MacroCallExpr,
        format: FormatMacro,
    ) -> ExprId {
        let db = ctxt.f_ctxt.db();
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let text = PathId::from_ident(db, IdentId::new(db, "%text".to_owned()));
        // `core` and `std` are `ingot` within themselves, so std can use the
        // macros.
        let buffer_ty = ctxt
            .f_ctxt
            .builtin_ingot_path(IngotKind::Core, "core")
            .push_str(db, "MemBuffer");
        let io = ctxt
            .f_ctxt
            .builtin_ingot_path(IngotKind::Std, "std")
            .push_str(db, "io");
        let text_path = |ctxt: &mut BodyCtxt<'_, 'db>| {
            ctxt.push_expr(Self::Path(Partial::Present(text)), HirOrigin::Expanded(ptr))
        };
        let call_path = |ctxt: &mut BodyCtxt<'_, 'db>, callee, args: Vec<ExprId>, ptr| {
            let callee = ctxt.push_expr(
                Self::Path(Partial::Present(callee)),
                HirOrigin::Expanded(ptr),
            );
            let args = args
                .into_iter()
                .map(|expr| CallArg { label: None, expr })
                .collect();
            ctxt.push_expr(Self::Call(callee, args), HirOrigin::Expanded(ptr))
        };
        let mut args = call.args().into_iter().flatten();

        ctxt.f_ctxt.enter_block_scope();
        // `write!`'s buffer, if given, and the span its errors point at.
        let buffer = matches!(format, FormatMacro::Write | FormatMacro::Writeln).then(|| {
            args.next().map_or((None, ptr), |arg| {
                let buffer = Self::push_to_body_opt(ctxt, arg.expr());
                (Some(buffer), SyntaxNodePtr::new(arg.syntax()))
            })
        });
        let access = buffer.and_then(|(buffer, _)| buffer).filter(|&buffer| {
            let mut expr = buffer;
            loop {
                match &ctxt.exprs[expr] {
                    Partial::Present(Self::Un(_, UnOp::Mut)) => return true,
                    Partial::Present(Self::Block(stmts, _)) => {
                        match stmts.last().map(|&stmt| &ctxt.stmts[stmt]) {
                            Some(Partial::Present(Stmt::Expr(tail))) => expr = *tail,
                            _ => return false,
                        }
                    }
                    _ => return false,
                }
            }
        });
        let (marker, ty, init) = match access {
            Some(buffer) => {
                let ty = TypeId::new(db, TypeKind::Path(Partial::Present(buffer_ty)));
                (BindingMarker::Plain, Some(ty), buffer)
            }
            None => {
                let empty = call_path(ctxt, buffer_ty.push_str(db, "empty"), Vec::new(), ptr);
                (BindingMarker::Var, None, empty)
            }
        };
        let pat = ctxt.push_pat(
            Pat::Path(Partial::Present(text), marker),
            HirOrigin::Expanded(ptr),
        );
        let mut stmts = vec![ctxt.push_stmt(
            Stmt::Let(pat, ty, Some(init), None),
            HirOrigin::Expanded(ptr),
        )];
        // Each argument's call has the argument's span, which its errors
        // point at.
        let mut values = args
            .map(|arg| {
                let ptr = SyntaxNodePtr::new(arg.syntax());
                (Self::push_to_body_opt(ctxt, arg.expr()), ptr)
            })
            .collect::<Vec<_>>();
        if matches!(
            format,
            FormatMacro::Writeln | FormatMacro::Println | FormatMacro::Eprintln
        ) {
            let newline = ctxt.push_expr(
                Self::Lit(LitKind::String(StringId::from_str(db, "\n"))),
                HirOrigin::Expanded(ptr),
            );
            values.push((newline, ptr));
        }
        for (value, ptr) in values {
            let target = text_path(ctxt);
            let target = ctxt.push_expr(Self::Un(target, UnOp::Mut), HirOrigin::Expanded(ptr));
            let fmt = io.push_str(db, "Display").push_str(db, "fmt");
            let call = call_path(ctxt, fmt, vec![value, target], ptr);
            stmts.push(ctxt.push_stmt(Stmt::Expr(call), HirOrigin::Expanded(ptr)));
        }
        let tail = match format {
            FormatMacro::Format => Some(text_path(ctxt)),
            FormatMacro::Write | FormatMacro::Writeln => {
                buffer.filter(|_| access.is_none()).map(|(buffer, ptr)| {
                    let text = text_path(ctxt);
                    let span = ctxt.push_expr(
                        Self::MethodCall(
                            text,
                            Partial::Present(IdentId::new(db, "span".to_owned())),
                            GenericArgListId::none(db),
                            Vec::new(),
                        ),
                        HirOrigin::Expanded(ptr),
                    );
                    let args = buffer.into_iter().chain([span]).collect();
                    call_path(ctxt, buffer_ty.push_str(db, "extend_from"), args, ptr)
                })
            }
            FormatMacro::Print
            | FormatMacro::Println
            | FormatMacro::Eprint
            | FormatMacro::Eprintln => {
                let sink = if matches!(format, FormatMacro::Print | FormatMacro::Println) {
                    "print_buffer"
                } else {
                    "eprint_buffer"
                };
                let text = text_path(ctxt);
                Some(call_path(ctxt, io.push_str(db, sink), vec![text], ptr))
            }
        };
        stmts.extend(tail.map(|tail| ctxt.push_stmt(Stmt::Expr(tail), HirOrigin::Expanded(ptr))));
        let block = ctxt.push_expr(
            Self::Block(stmts, BlockKind::Normal),
            HirOrigin::Expanded(ptr),
        );
        ctxt.f_ctxt.leave_block_scope(block);
        block
    }
}

impl Cond {
    pub(super) fn push_to_body_opt<'db>(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: Option<ast::Expr>,
    ) -> CondId {
        if let Some(ast) = ast {
            Self::lower_ast(ctxt, ast)
        } else {
            ctxt.push_missing_cond()
        }
    }

    fn lower_ast<'db>(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Expr) -> CondId {
        let cond = match ast.kind() {
            ast::ExprKind::Let(let_expr) => {
                let saved_binder_len = ctxt.active_binder_groups.len();
                let saved_local_len = ctxt.local_binders.len();
                let pat = Pat::lower_ast_opt(ctxt, let_expr.pat());
                let new_binders: Vec<_> = ctxt
                    .active_binder_groups
                    .drain(saved_binder_len..)
                    .collect();
                let new_locals: Vec<_> = ctxt.local_binders.drain(saved_local_len..).collect();
                let scrutinee = Expr::push_to_body_opt(ctxt, let_expr.expr());
                ctxt.active_binder_groups.extend(new_binders);
                ctxt.local_binders.extend(new_locals);
                Self::Let(pat, scrutinee)
            }

            ast::ExprKind::Bin(bin)
                if matches!(
                    bin.op(),
                    Some(ast::BinOp::Logical(ast::LogicalBinOp::And(_)))
                        | Some(ast::BinOp::Logical(ast::LogicalBinOp::Or(_)))
                ) =>
            {
                let op = LogicalBinOp::lower_ast(match bin.op().expect("parser guarantees op") {
                    ast::BinOp::Logical(op) => op,
                    _ => unreachable!(),
                });
                if matches!(op, LogicalBinOp::Or) {
                    let saved_binder_len = ctxt.active_binder_groups.len();
                    let saved_local_len = ctxt.local_binders.len();
                    let lhs = Cond::push_to_body_opt(ctxt, bin.lhs());
                    ctxt.active_binder_groups.truncate(saved_binder_len);
                    ctxt.local_binders.truncate(saved_local_len);
                    let rhs = Cond::push_to_body_opt(ctxt, bin.rhs());
                    ctxt.active_binder_groups.truncate(saved_binder_len);
                    ctxt.local_binders.truncate(saved_local_len);
                    Self::Bin(lhs, rhs, op)
                } else {
                    let lhs = Cond::push_to_body_opt(ctxt, bin.lhs());
                    let rhs = Cond::push_to_body_opt(ctxt, bin.rhs());
                    Self::Bin(lhs, rhs, op)
                }
            }

            ast::ExprKind::Paren(paren) => {
                return Cond::push_to_body_opt(ctxt, paren.expr());
            }

            _ => {
                let expr = Expr::lower_ast(ctxt, ast);
                Self::Expr(expr)
            }
        };

        ctxt.push_cond(cond)
    }
}

impl BinOp {
    pub(super) fn lower_ast(ast: ast::BinOp) -> Self {
        match ast {
            ast::BinOp::Arith(arith) => ArithBinOp::lower_ast(arith).into(),
            ast::BinOp::Comp(arith) => CompBinOp::lower_ast(arith).into(),
            ast::BinOp::Logical(arith) => LogicalBinOp::lower_ast(arith).into(),
        }
    }
}

impl ArithBinOp {
    pub(super) fn lower_ast(ast: ast::ArithBinOp) -> Self {
        match ast {
            ast::ArithBinOp::Add(_) => Self::Add,
            ast::ArithBinOp::Sub(_) => Self::Sub,
            ast::ArithBinOp::Mul(_) => Self::Mul,
            ast::ArithBinOp::Div(_) => Self::Div,
            ast::ArithBinOp::Mod(_) => Self::Rem,
            ast::ArithBinOp::Pow(_) => Self::Pow,
            ast::ArithBinOp::LShift(_) => Self::LShift,
            ast::ArithBinOp::RShift(_) => Self::RShift,
            ast::ArithBinOp::BitAnd(_) => Self::BitAnd,
            ast::ArithBinOp::BitOr(_) => Self::BitOr,
            ast::ArithBinOp::BitXor(_) => Self::BitXor,
            ast::ArithBinOp::Range(_) => Self::Range,
        }
    }
}

impl CompBinOp {
    pub(super) fn lower_ast(ast: ast::CompBinOp) -> Self {
        match ast {
            ast::CompBinOp::Eq(_) => Self::Eq,
            ast::CompBinOp::NotEq(_) => Self::NotEq,
            ast::CompBinOp::Lt(_) => Self::Lt,
            ast::CompBinOp::LtEq(_) => Self::LtEq,
            ast::CompBinOp::Gt(_) => Self::Gt,
            ast::CompBinOp::GtEq(_) => Self::GtEq,
        }
    }
}

impl LogicalBinOp {
    pub(super) fn lower_ast(ast: ast::LogicalBinOp) -> Self {
        match ast {
            ast::LogicalBinOp::And(_) => Self::And,
            ast::LogicalBinOp::Or(_) => Self::Or,
        }
    }
}

impl UnOp {
    fn lower_ast(ast: ast::UnOp) -> Self {
        match ast {
            ast::UnOp::Plus(_) => Self::Plus,
            ast::UnOp::Minus(_) => Self::Minus,
            ast::UnOp::Not(_) => Self::Not,
            ast::UnOp::BitNot(_) => Self::BitNot,
            ast::UnOp::Mut(_) => Self::Mut,
            ast::UnOp::MaybeMut(_) => Self::MaybeMut,
            ast::UnOp::Ref(_) => Self::Ref,
            ast::UnOp::Deref(_) => Self::Deref,
        }
    }
}

impl MatchArm {
    fn lower_ast(ctxt: &mut BodyCtxt<'_, '_>, ast: ast::MatchArm) -> Self {
        let saved_binder_len = ctxt.active_binder_groups.len();
        let saved_local_len = ctxt.local_binders.len();
        let pat = Pat::lower_ast_opt(ctxt, ast.pat());
        let body = Expr::push_to_body_opt(ctxt, ast.body());
        ctxt.active_binder_groups.truncate(saved_binder_len);
        ctxt.local_binders.truncate(saved_local_len);
        Self { pat, body }
    }

    fn lower_arm_list(ctxt: &mut BodyCtxt<'_, '_>, arms: ast::MatchArmList, out: &mut Vec<Self>) {
        for child in arms.syntax().children() {
            if let Some(arm) = ast::MatchArm::cast(child.clone()) {
                out.push(Self::lower_ast(ctxt, arm));
            } else if let Some(hole) = ast::QuoteHoleArm::cast(child) {
                if let Some(common::generation::FrozenHoleValue::Code(code)) =
                    ctxt.f_ctxt.current_hole(hole.syntax().text_range())
                {
                    Self::lower_code_into(ctxt, &code, hole.syntax().text_range(), out);
                } else {
                    ctxt.f_ctxt.record_generation_error(
                        hole.syntax().text_range(),
                        "match-arm quotation hole requires Code<Arm> splice",
                    );
                }
            }
        }
    }

    fn lower_code_into(
        ctxt: &mut BodyCtxt<'_, '_>,
        code: &common::generation::FrozenCode,
        origin_range: parser::TextRange,
        out: &mut Vec<Self>,
    ) {
        use common::generation::{FrozenCode, FrozenCodeKind, FrozenHoleValue};
        match code {
            FrozenCode::Empty { .. } => {}
            FrozenCode::Template {
                kind: FrozenCodeKind::Arm,
                quote_file,
                quote_range,
                producer_module,
                producer_block,
                holes,
            } => {
                let (quote, frame) = ctxt.f_ctxt.resolve_quote_template(
                    quote_file,
                    *quote_range,
                    *producer_module,
                    *producer_block,
                    true,
                    holes.clone(),
                );
                ctxt.validate_frame_open_names(&frame);
                ctxt.f_ctxt.quote_frames.push(frame);
                if let Some(arm_list) = quote.arm_list() {
                    Self::lower_arm_list(ctxt, arm_list, out);
                } else if let Some(block) = quote.body() {
                    for stmt in block.stmts() {
                        if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                            && let Some(expr) = expr_stmt.expr()
                            && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                            && let Some(FrozenHoleValue::Code(nested)) =
                                ctxt.f_ctxt.current_hole(hole.syntax().text_range())
                        {
                            Self::lower_code_into(ctxt, &nested, hole.syntax().text_range(), out);
                        } else {
                            ctxt.f_ctxt.record_generation_error(
                                stmt.syntax().text_range(),
                                "match-arm quotation requires match arms",
                            );
                        }
                    }
                } else {
                    ctxt.f_ctxt.record_generation_error(
                        *quote_range,
                        "match-arm quotation requires match arms",
                    );
                }
                ctxt.f_ctxt.quote_frames.pop();
            }
            _ => {
                ctxt.f_ctxt.record_generation_error(
                    origin_range,
                    "match-arm quotation hole requires Code<Arm> splice",
                );
            }
        }
    }
}

impl<'db> CallArg<'db> {
    fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::CallArg) -> Self {
        let label = ast
            .label()
            .map(|label| IdentId::lower_token(ctxt.f_ctxt, label));
        let expr = Expr::push_to_body_opt(ctxt, ast.expr());
        Self { label, expr }
    }
}

impl<'db> Field<'db> {
    fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::RecordField) -> Self {
        let label = if let Some(hole) = ast.label_hole() {
            ctxt.f_ctxt
                .lower_field_hole_ident(hole.syntax().text_range())
                .to_opt()
        } else {
            ast.label()
                .map(|label| IdentId::lower_token(ctxt.f_ctxt, label))
        };
        let expr = Expr::push_to_body_opt(ctxt, ast.expr());
        Self { label, expr }
    }
}
