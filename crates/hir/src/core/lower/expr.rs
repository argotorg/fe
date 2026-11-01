use common::ingot::IngotKind;
use parser::ast::{self, SyntaxNodePtr, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{
        BindingMarker, Body, FuncParamListId, GenericArgListId, IdentId, IntegerId, ItemKind,
        LitKind, Partial, Pat, PathId, Stmt, StmtId, StringId, TypeId, TypeKind, expr::*,
    },
    span::HirOrigin,
};

impl<'db> Expr<'db> {
    pub(super) fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Expr) -> ExprId {
        let expr = match ast.kind() {
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
                let lets = ctxt.lower_param_lets(closure.params());
                let body = Self::lower_ast_after(ctxt, closure.body(), lets);
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
                let field = if let Some(name) = field.field_name() {
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
                let cond = Cond::push_to_body_opt(ctxt, if_.cond());
                let then = Expr::push_to_body_opt(
                    ctxt,
                    if_.then()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );
                let else_ = if_.else_().map(|ast| Self::lower_ast(ctxt, ast));
                Self::If(cond, then, else_)
            }

            ast::ExprKind::Match(match_) => {
                let scrutinee = Self::push_to_body_opt(ctxt, match_.scrutinee());
                let arm = match_
                    .arms()
                    .map(|arms| {
                        arms.into_iter()
                            .map(|arm| MatchArm::lower_ast(ctxt, arm))
                            .collect()
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
        let stmts = leading
            .into_iter()
            .chain(block.stmts().map(|stmt| Stmt::push_to_body(ctxt, stmt)))
            .collect();
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
    /// `write!(mut buf, a, b)` formats into `buf` itself, bound once as
    /// `let %text: core::MemBuffer = mut buf`, and `writeln!` adds a newline.
    /// A buffer written otherwise, or none, takes the text `format!` builds
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
            matches!(ctxt.exprs[buffer], Partial::Present(Self::Un(_, UnOp::Mut)))
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
                let pat = Pat::lower_ast_opt(ctxt, let_expr.pat());
                let scrutinee = Expr::push_to_body_opt(ctxt, let_expr.expr());
                Self::Let(pat, scrutinee)
            }

            ast::ExprKind::Bin(bin)
                if matches!(
                    bin.op(),
                    Some(ast::BinOp::Logical(ast::LogicalBinOp::And(_)))
                        | Some(ast::BinOp::Logical(ast::LogicalBinOp::Or(_)))
                ) =>
            {
                let lhs = Cond::push_to_body_opt(ctxt, bin.lhs());
                let rhs = Cond::push_to_body_opt(ctxt, bin.rhs());
                let op = LogicalBinOp::lower_ast(match bin.op().expect("parser guarantees op") {
                    ast::BinOp::Logical(op) => op,
                    _ => unreachable!(),
                });
                Self::Bin(lhs, rhs, op)
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
        let pat = Pat::lower_ast_opt(ctxt, ast.pat());
        let body = Expr::push_to_body_opt(ctxt, ast.body());
        Self { pat, body }
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
        let label = ast
            .label()
            .map(|label| IdentId::lower_token(ctxt.f_ctxt, label));
        let expr = Expr::push_to_body_opt(ctxt, ast.expr());
        Self { label, expr }
    }
}
