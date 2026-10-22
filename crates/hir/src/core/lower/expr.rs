use parser::ast::{self, SyntaxNodePtr, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{
        BindingMarker, Body, FuncParamListId, FuncParamName, GenericArgListId, IdentId, IntegerId,
        ItemKind, LitKind, Partial, Pat, PathId, Stmt, StmtId, StringId, TypeId, expr::*,
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
                            crate::hir_def::TypeKind::Primitive(primitive),
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
                    .map(|params| FuncParamListId::lower_ast(ctxt.f_ctxt, params))
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
        let stmts = leading
            .into_iter()
            .chain(block.stmts().map(|stmt| Stmt::push_to_body(ctxt, stmt)))
            .collect();
        ctxt.active_binder_groups.truncate(saved_binder_len);
        ctxt.local_binders.truncate(saved_local_len);
        let expr_id = ctxt.push_expr(Self::Block(stmts, kind), HirOrigin::raw(ast));

        for item in block.items() {
            ItemKind::lower_ast(ctxt.f_ctxt, item);
        }

        ctxt.f_ctxt.leave_block_scope(expr_id);
        expr_id
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
            _ => {
                ctxt.f_ctxt.record_generation_error(
                    origin_ast.syntax().text_range(),
                    "expression quotation hole requires Code<Expr> splice",
                );
                ctxt.push_invalid_expr(HirOrigin::raw(origin_ast))
            }
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

/// The builtin macros that format their arguments: `format!` and the
/// `print!` family.
#[derive(Clone, Copy)]
enum FormatMacro {
    Format,
    Print,
    Println,
    Eprint,
    Eprintln,
}

impl FormatMacro {
    fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "format" => Self::Format,
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
    fn lower_format_macro(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: &ast::Expr,
        call: &ast::MacroCallExpr,
        format: FormatMacro,
    ) -> ExprId {
        let db = ctxt.f_ctxt.db();
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let path = |ctxt: &mut BodyCtxt<'_, 'db>, path: PathId<'db>| {
            ctxt.push_expr(Self::Path(Partial::Present(path)), HirOrigin::Expanded(ptr))
        };
        let text = PathId::from_ident(db, IdentId::new(db, "%text".to_owned()));

        ctxt.f_ctxt.enter_block_scope();
        let pat = ctxt.push_pat(
            Pat::Path(Partial::Present(text), BindingMarker::Var),
            HirOrigin::Expanded(ptr),
        );
        let empty = path(
            ctxt,
            PathId::from_segments(db, &["core", "MemBuffer", "empty"]),
        );
        let init = ctxt.push_expr(Self::Call(empty, Vec::new()), HirOrigin::Expanded(ptr));
        let mut stmts = vec![ctxt.push_stmt(
            Stmt::Let(pat, None, Some(init), None),
            HirOrigin::Expanded(ptr),
        )];
        // Each argument's call has the argument's span, which its errors
        // point at.
        let mut values = call
            .args()
            .into_iter()
            .flatten()
            .map(|arg| {
                let ptr = SyntaxNodePtr::new(arg.syntax());
                (Self::push_to_body_opt(ctxt, arg.expr()), ptr)
            })
            .collect::<Vec<_>>();
        if matches!(format, FormatMacro::Println | FormatMacro::Eprintln) {
            let newline = ctxt.push_expr(
                Self::Lit(LitKind::String(StringId::from_str(db, "\n"))),
                HirOrigin::Expanded(ptr),
            );
            values.push((newline, ptr));
        }
        for (value, ptr) in values {
            let fmt = ctxt.push_expr(
                Self::Path(Partial::Present(PathId::from_segments(
                    db,
                    &["std", "io", "Display", "fmt"],
                ))),
                HirOrigin::Expanded(ptr),
            );
            let target = path(ctxt, text);
            let target = ctxt.push_expr(Self::Un(target, UnOp::Mut), HirOrigin::Expanded(ptr));
            let call = ctxt.push_expr(
                Self::Call(
                    fmt,
                    vec![
                        CallArg {
                            label: None,
                            expr: value,
                        },
                        CallArg {
                            label: None,
                            expr: target,
                        },
                    ],
                ),
                HirOrigin::Expanded(ptr),
            );
            stmts.push(ctxt.push_stmt(Stmt::Expr(call), HirOrigin::Expanded(ptr)));
        }
        let sink = match format {
            FormatMacro::Format => None,
            FormatMacro::Print | FormatMacro::Println => Some("print_buffer"),
            FormatMacro::Eprint | FormatMacro::Eprintln => Some("eprint_buffer"),
        };
        let tail = path(ctxt, text);
        let tail = match sink {
            None => tail,
            Some(sink) => {
                let sink = path(ctxt, PathId::from_segments(db, &["std", "io", sink]));
                ctxt.push_expr(
                    Self::Call(
                        sink,
                        vec![CallArg {
                            label: None,
                            expr: tail,
                        }],
                    ),
                    HirOrigin::Expanded(ptr),
                )
            }
        };
        stmts.push(ctxt.push_stmt(Stmt::Expr(tail), HirOrigin::Expanded(ptr)));
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
