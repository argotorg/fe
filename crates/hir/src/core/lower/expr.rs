use parser::ast::{self, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{
        Body, GenericArgListId, IdentId, IntegerId, ItemKind, LitKind, Pat, PathId, Stmt, TypeId,
        expr::*,
    },
    span::HirOrigin,
};

impl<'db> Expr<'db> {
    pub(super) fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Expr) -> ExprId {
        let expr = match ast.kind() {
            ast::ExprKind::Lit(lit) => {
                if let Some(lit) = lit.lit() {
                    let lit = LitKind::lower_ast(ctxt.f_ctxt, lit);
                    Self::Lit(lit)
                } else {
                    return ctxt.push_invalid_expr(HirOrigin::raw(&ast));
                }
            }

            ast::ExprKind::Block(block) => {
                ctxt.f_ctxt.enter_block_scope();
                let mut stmts = vec![];

                for stmt in block.stmts() {
                    let stmt = Stmt::push_to_body(ctxt, stmt);
                    stmts.push(stmt);
                }
                let expr_id = ctxt.push_expr(Self::Block(stmts), HirOrigin::raw(&ast));

                for item in block.items() {
                    ItemKind::lower_ast(ctxt.f_ctxt, item);
                }

                ctxt.f_ctxt.leave_block_scope(expr_id);
                return expr_id;
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

            ast::ExprKind::Call(call) => {
                let callee = Self::push_to_body_opt(ctxt, call.callee());
                let args = call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::lower_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::Call(callee, args)
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
                let cond = Self::push_to_body_opt(ctxt, if_.cond());
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
        };

        ctxt.push_expr(expr, HirOrigin::raw(&ast))
    }

    pub(super) fn push_to_body_opt(ctxt: &mut BodyCtxt<'_, '_>, ast: Option<ast::Expr>) -> ExprId {
        if let Some(ast) = ast {
            Expr::lower_ast(ctxt, ast)
        } else {
            ctxt.push_missing_expr()
        }
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
