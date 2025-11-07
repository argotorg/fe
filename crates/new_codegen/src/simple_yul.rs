use std::fmt;

use driver::DriverDataBase;
use hir::hir_def::{Body, Expr, ExprId, Func, LitKind, Partial, Stmt, StmtId, TopLevelMod};
use mir::{lower_module, MirFunction, Terminator, ValueId, ValueOrigin};

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

pub fn emit_module_simple_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<Vec<Result<String, SimpleYulError>>, mir::MirLowerError> {
    let module = lower_module(db, top_mod)?;
    Ok(module
        .functions
        .iter()
        .map(|func| emit_function(db, func))
        .collect())
}

fn emit_function(db: &DriverDataBase, mir_func: &MirFunction<'_>) -> Result<String, SimpleYulError> {
    let func_name = function_name(db, mir_func.func);
    let body = mir_func
        .func
        .body(db)
        .ok_or_else(|| SimpleYulError::MissingBody(func_name.clone()))?;

    let entry = mir_func.body.entry;
    let block = mir_func
        .body
        .blocks
        .get(entry.index())
        .ok_or_else(|| SimpleYulError::Unsupported("entry block missing".into()))?;

    let Terminator::Return(Some(ret_val)) = block.terminator else {
        return Err(SimpleYulError::Unsupported(
            "only simple return expressions are supported".into(),
        ));
    };

    let expr_str = lower_value(db, mir_func, body, ret_val)?;

    Ok(format!(
        "{{\n  function {func_name}() -> ret {{\n    ret := {expr_str}\n  }}\n}}"
    ))
}

fn lower_value(
    db: &DriverDataBase,
    mir_func: &MirFunction<'_>,
    body: Body<'_>,
    value_id: ValueId,
) -> Result<String, SimpleYulError> {
    let value = mir_func.body.value(value_id);
    match value.origin {
        ValueOrigin::Expr(expr_id) => lower_expr(db, body, expr_id),
        _ => Err(SimpleYulError::Unsupported(
            "only expression-derived values are supported".into(),
        )),
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
        Expr::Bin(lhs, rhs, bin_op) => {
            use hir::hir_def::expr::ArithBinOp::Add;
            use hir::hir_def::expr::BinOp;

            match bin_op {
                BinOp::Arith(Add) => {
                    let left = lower_expr(db, body, *lhs)?;
                    let right = lower_expr(db, body, *rhs)?;
                    Ok(format!("add({left}, {right})"))
                }
                _ => Err(SimpleYulError::Unsupported(
                    "only addition is supported".into(),
                )),
            }
        }
        Expr::Block(stmts) => {
            let Some(expr) = last_expr(db, body, stmts) else {
                return Err(SimpleYulError::Unsupported(
                    "block without terminal expression".into(),
                ));
            };
            lower_expr(db, body, expr)
        }
        _ => Err(SimpleYulError::Unsupported(
            "only integer literals and addition are supported".into(),
        )),
    }
}

fn last_expr(db: &DriverDataBase, body: Body<'_>, stmts: &[StmtId]) -> Option<ExprId> {
    stmts.iter().rev().find_map(|stmt_id| {
        let Partial::Present(stmt) = stmt_id.data(db, body) else {
            return None;
        };
        if let Stmt::Expr(expr) = stmt {
            Some(*expr)
        } else {
            None
        }
    })
}

fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}
