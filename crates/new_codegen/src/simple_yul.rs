use std::fmt;

use driver::DriverDataBase;
use hir::hir_def::{
    Body, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod,
    expr::{ArithBinOp, BinOp, LogicalBinOp, UnOp},
};
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

    let statements = render_statements(db, mir_func, body, &block.insts)?;
    let expr_str = lower_value(db, mir_func, body, ret_val)?;

    let mut lines = statements;
    lines.push(format!("    ret := {expr_str}"));
    let body_text = lines.join("\n");
    Ok(format!(
        "{{\n  function {func_name}() -> ret {{\n{body_text}\n  }}\n}}"
    ))
}

fn render_statements(
    db: &DriverDataBase,
    mir_func: &MirFunction<'_>,
    body: Body<'_>,
    insts: &[mir::MirInst<'_>],
) -> Result<Vec<String>, SimpleYulError> {
    let mut stmts = Vec::new();
    for inst in insts {
        match inst {
            mir::MirInst::Let { pat, value, .. } => {
                let name = pattern_ident(db, body, *pat)?;
                let value = match value {
                    Some(val) => lower_value(db, mir_func, body, *val)?,
                    None => "0".into(),
                };
                stmts.push(format!("    let {name} := {value}"));
            }
            mir::MirInst::Assign { .. } | mir::MirInst::AugAssign { .. } => {
                return Err(SimpleYulError::Unsupported(
                    "assignment statements are not supported yet".into(),
                ))
            }
            mir::MirInst::Eval { .. } => {}
            mir::MirInst::ForLoop { .. }
            | mir::MirInst::WhileLoop { .. }
            | mir::MirInst::Break { .. }
            | mir::MirInst::Continue { .. }
            | mir::MirInst::Return { .. } => {
                return Err(SimpleYulError::Unsupported(
                    "control flow statements are not supported yet".into(),
                ))
            }
        }
    }
    Ok(stmts)
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
        Expr::Lit(LitKind::Bool(value)) => Ok(if *value { "1" } else { "0" }.into()),
        Expr::Lit(LitKind::String(str_id)) => Ok(format!("0x{}", hex::encode(str_id.data(db).as_bytes()))),
        Expr::Un(inner, op) => {
            let value = lower_expr(db, body, *inner)?;
            match op {
                UnOp::Minus => Ok(format!("sub(0, {value})")),
                UnOp::Not => Ok(format!("iszero({value})")),
                UnOp::Plus => Ok(value),
                UnOp::BitNot => Err(SimpleYulError::Unsupported(
                    "bitwise not is not supported yet".into(),
                )),
            }
        }
        Expr::Tuple(values) => {
            let parts = values
                .iter()
                .map(|expr| lower_expr(db, body, *expr))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("tuple({})", parts.join(", ")))
        }
        Expr::Bin(lhs, rhs, bin_op) => match bin_op {
            BinOp::Arith(op) => {
                let left = lower_expr(db, body, *lhs)?;
                let right = lower_expr(db, body, *rhs)?;
                let func = match op {
                    ArithBinOp::Add => "add",
                    ArithBinOp::Sub => "sub",
                    ArithBinOp::Mul => "mul",
                    ArithBinOp::Div => "div",
                    ArithBinOp::Rem => "mod",
                    ArithBinOp::Pow | ArithBinOp::LShift | ArithBinOp::RShift
                    | ArithBinOp::BitAnd | ArithBinOp::BitOr | ArithBinOp::BitXor => {
                        return Err(SimpleYulError::Unsupported(
                            "bitwise and power operations are not supported yet".into(),
                        ));
                    }
                };
                Ok(format!("{func}({left}, {right})"))
            }
            BinOp::Logical(op) => {
                let left = lower_expr(db, body, *lhs)?;
                let right = lower_expr(db, body, *rhs)?;
                let func = match op {
                    LogicalBinOp::And => "and",
                    LogicalBinOp::Or => "or",
                };
                Ok(format!("{func}({left}, {right})"))
            }
            _ => Err(SimpleYulError::Unsupported(
                "only arithmetic/logical binary expressions are supported right now".into(),
            )),
        },
        Expr::Block(stmts) => {
            let Some(expr) = last_expr(db, body, stmts) else {
                return Err(SimpleYulError::Unsupported(
                    "block without terminal expression".into(),
                ));
            };
            lower_expr(db, body, expr)
        }
        Expr::Path(path) => path_ident(db, *path)
            .ok_or_else(|| SimpleYulError::Unsupported("unsupported path expression".into())),
        _ => Err(SimpleYulError::Unsupported(
            "only simple expressions are supported".into(),
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

fn pattern_ident(db: &DriverDataBase, body: Body<'_>, pat_id: PatId) -> Result<String, SimpleYulError> {
    let pat = match pat_id.data(db, body) {
        Partial::Present(pat) => pat,
        Partial::Absent => {
            return Err(SimpleYulError::Unsupported(
                "unsupported pattern".into(),
            ))
        }
    };
    match pat {
        Pat::Path(path, _) => path_ident(db, *path)
            .ok_or_else(|| SimpleYulError::Unsupported("unsupported pattern path".into())),
        _ => Err(SimpleYulError::Unsupported(
            "only identifier patterns are supported".into(),
        )),
    }
}

fn path_ident(db: &DriverDataBase, path: Partial<PathId<'_>>) -> Option<String> {
    let path = path.to_opt()?;
    path.as_ident(db).map(|id| id.data(db).to_string())
}
