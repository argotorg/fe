use std::fmt;

use hir::{
    HirDb,
    hir_def::{
        Body, Expr, ExprId, Func, LitKind, Partial, Stmt, TopLevelMod,
        expr::{ArithBinOp, BinOp},
    },
};

use crate::DriverDataBase;

#[derive(Debug)]
pub enum SimpleYulError {
    MissingBody(String),
    Unsupported(String),
}

impl fmt::Display for SimpleYulError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SimpleYulError::MissingBody(name) => {
                write!(f, "function `{name}` does not have a body")
            }
            SimpleYulError::Unsupported(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for SimpleYulError {}

pub fn emit_simple_function_yul(
    db: &DriverDataBase,
    func: Func<'_>,
) -> Result<String, SimpleYulError> {
    let func_name = func
        .name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string());

    let body = func.body(db).ok_or_else(|| SimpleYulError::MissingBody(func_name.clone()))?;
    let expr_id = extract_expr(db, body, body.expr(db))
        .ok_or_else(|| SimpleYulError::Unsupported(format!("`{func_name}` must end with an expression literal or addition")))?;
    let expr_str = lower_expr(db, body, expr_id)?;

    Ok(format!(
        "{{\n  function {func_name}() -> ret {{\n    ret := {expr_str}\n  }}\n}}"
    ))
}

pub fn emit_module_simple_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Vec<Result<String, SimpleYulError>> {
    top_mod
        .all_funcs(db)
        .iter()
        .map(|&func| emit_simple_function_yul(db, func))
        .collect()
}

fn extract_expr<'db>(db: &'db dyn HirDb, body: Body<'db>, expr_id: ExprId) -> Option<ExprId> {
    match expr_id.data(db, body) {
        Partial::Present(Expr::Block(stmts)) => stmts
            .last()
            .and_then(|stmt_id| match stmt_id.data(db, body) {
                Partial::Present(Stmt::Expr(expr)) => extract_expr(db, body, *expr),
                _ => None,
            }),
        Partial::Present(_) => Some(expr_id),
        Partial::Absent => None,
    }
}

fn lower_expr(db: &DriverDataBase, body: Body<'_>, expr_id: ExprId) -> Result<String, SimpleYulError> {
    let expr = match expr_id.data(db, body) {
        Partial::Present(expr) => expr,
        Partial::Absent => {
            return Err(SimpleYulError::Unsupported(
                "expression data unavailable".into(),
            ))
        }
    };
    match expr {
        Expr::Lit(LitKind::Int(int_id)) => Ok(int_id.data(db).to_string()),
        Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Add)) => {
            let left = lower_expr(db, body, *lhs)?;
            let right = lower_expr(db, body, *rhs)?;
            Ok(format!("add({left}, {right})"))
        }
        _ => Err(SimpleYulError::Unsupported(
            "only integer literals and `+` expressions are supported".into(),
        )),
    }
}
