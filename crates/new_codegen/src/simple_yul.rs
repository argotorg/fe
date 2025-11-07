use std::fmt;

use driver::DriverDataBase;
use hir::hir_def::{
    Body, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod,
    expr::{ArithBinOp, BinOp, LogicalBinOp, UnOp},
};
use mir::{lower_module, MirFunction, Terminator, ValueId, ValueOrigin};
use rustc_hash::FxHashMap;

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
        .map(|func| SimpleYulEmitter::new(db, func).and_then(|emitter| emitter.emit()))
        .collect())
}

struct SimpleYulEmitter<'db> {
    db: &'db DriverDataBase,
    mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    locals: FxHashMap<String, String>,
    param_names: Vec<String>,
    next_local: usize,
}

impl<'db> SimpleYulEmitter<'db> {
    fn new(db: &'db DriverDataBase, mir_func: &'db MirFunction<'db>) -> Result<Self, SimpleYulError> {
        let body = mir_func.func.body(db).ok_or_else(|| {
            SimpleYulError::MissingBody(function_name(db, mir_func.func))
        })?;
        let mut this = Self {
            db,
            mir_func,
            body,
            locals: FxHashMap::default(),
            param_names: Vec::new(),
            next_local: 0,
        };
        this.init_params();
        Ok(this)
    }

    fn emit(mut self) -> Result<String, SimpleYulError> {
        let func_name = function_name(self.db, self.mir_func.func);
        let params = if self.param_names.is_empty() {
            String::new()
        } else {
            self.param_names.join(", ")
        };
        let entry = self.mir_func.body.entry;
        let block = self
            .mir_func
            .body
            .blocks
            .get(entry.index())
            .ok_or_else(|| SimpleYulError::Unsupported("entry block missing".into()))?;

        let Terminator::Return(Some(ret_val)) = block.terminator else {
            return Err(SimpleYulError::Unsupported(
                "only simple return expressions are supported".into(),
            ));
        };

        let statements = self.render_statements(&block.insts)?;
        let expr_str = self.lower_value(ret_val)?;

        let mut lines = statements;
        lines.push(format!("    ret := {expr_str}"));
        let body_text = lines.join("\n");
        if params.is_empty() {
            Ok(format!(
                "{{\n  function {func_name}() -> ret {{\n{body_text}\n  }}\n}}"
            ))
        } else {
            Ok(format!(
                "{{\n  function {func_name}({params}) -> ret {{\n{body_text}\n  }}\n}}"
            ))
        }
    }

    fn init_params(&mut self) {
        if let Some(params) = self.mir_func.func.params(self.db).to_opt() {
            for (idx, param) in params.data(self.db).iter().enumerate() {
                let original = param
                    .name
                    .to_opt()
                    .and_then(|name| name.ident())
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| format!("arg{idx}"));
                let yul_name = original.clone();
                self.param_names.push(yul_name.clone());
                self.locals.entry(original).or_insert(yul_name);
            }
        }
    }
    fn render_statements(
        &mut self,
        insts: &[mir::MirInst<'_>],
    ) -> Result<Vec<String>, SimpleYulError> {
        let mut stmts = Vec::new();
        for inst in insts {
            match inst {
                mir::MirInst::Let { pat, value, .. } => {
                let binding = self.pattern_ident(*pat)?;
                let yul_name = if let Some(existing) = self.locals.get(&binding) {
                    existing.clone()
                } else {
                    let name = self.alloc_local();
                    self.locals.insert(binding.clone(), name.clone());
                    name
                };
                    let value = match value {
                        Some(val) => self.lower_value(*val)?,
                        None => "0".into(),
                    };
                    stmts.push(format!("    let {yul_name} := {value}"));
                }
                mir::MirInst::Assign { target, value, .. } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = self.locals.get(&binding).cloned().ok_or_else(|| {
                        SimpleYulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let value = self.lower_value(*value)?;
                    stmts.push(format!("    {yul_name} := {value}"));
                }
                mir::MirInst::AugAssign { .. } => {
                    return Err(SimpleYulError::Unsupported(
                        "augmented assignment is not supported yet".into(),
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

    fn lower_value(&mut self, value_id: ValueId) -> Result<String, SimpleYulError> {
        let value = self.mir_func.body.value(value_id);
        match value.origin {
            ValueOrigin::Expr(expr_id) => self.lower_expr(expr_id),
            _ => Err(SimpleYulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    fn lower_expr(&mut self, expr_id: ExprId) -> Result<String, SimpleYulError> {
        let expr = match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                return Err(SimpleYulError::Unsupported(
                    "expression data unavailable".into(),
                ))
            }
        };
        match expr {
            Expr::Lit(LitKind::Int(int_id)) => Ok(int_id.data(self.db).to_string()),
            Expr::Lit(LitKind::Bool(value)) => Ok(if *value { "1" } else { "0" }.into()),
            Expr::Lit(LitKind::String(str_id)) => {
                Ok(format!("0x{}", hex::encode(str_id.data(self.db).as_bytes())))
            }
            Expr::Un(inner, op) => {
                let value = self.lower_expr(*inner)?;
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
                    .map(|expr| self.lower_expr(*expr))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(format!("tuple({})", parts.join(", ")))
            }
            Expr::Bin(lhs, rhs, bin_op) => match bin_op {
                BinOp::Arith(op) => {
                    let left = self.lower_expr(*lhs)?;
                    let right = self.lower_expr(*rhs)?;
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
                let left = self.lower_expr(*lhs)?;
                let right = self.lower_expr(*rhs)?;
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
            let Some(expr) = self.last_expr(stmts) else {
                return Err(SimpleYulError::Unsupported(
                    "block without terminal expression".into(),
                ));
            };
            self.lower_expr(expr)
        }
        Expr::Path(path) => {
            let original = self
                .path_ident(*path)
                .ok_or_else(|| SimpleYulError::Unsupported("unsupported path expression".into()))?;
            Ok(self.locals.get(&original).cloned().unwrap_or(original))
        }
        _ => Err(SimpleYulError::Unsupported(
            "only simple expressions are supported".into(),
        )),
        }
    }

    fn last_expr(&self, stmts: &[StmtId]) -> Option<ExprId> {
        stmts.iter().rev().find_map(|stmt_id| {
            let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                return None;
            };
            if let Stmt::Expr(expr) = stmt {
                Some(*expr)
            } else {
                None
            }
        })
    }

    fn pattern_ident(&mut self, pat_id: PatId) -> Result<String, SimpleYulError> {
        let pat = match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => pat,
            Partial::Absent => {
                return Err(SimpleYulError::Unsupported(
                    "unsupported pattern".into(),
                ))
            }
        };
        match pat {
            Pat::Path(path, _) => self
                .path_ident(*path)
                .ok_or_else(|| SimpleYulError::Unsupported("unsupported pattern path".into())),
            _ => Err(SimpleYulError::Unsupported(
                "only identifier patterns are supported".into(),
            )),
        }
    }

    fn path_from_expr(&self, expr_id: ExprId) -> Result<String, SimpleYulError> {
        let expr = match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                return Err(SimpleYulError::Unsupported(
                    "unsupported assignment target".into(),
                ))
            }
        };
        if let Expr::Path(path) = expr {
            self.path_ident(*path)
                .ok_or_else(|| SimpleYulError::Unsupported("unsupported assignment target".into()))
        } else {
            Err(SimpleYulError::Unsupported(
                "only identifier assignments are supported".into(),
            ))
        }
    }

    fn path_ident(&self, path: Partial<PathId<'_>>) -> Option<String> {
        let path = path.to_opt()?;
        path.as_ident(self.db).map(|id| id.data(self.db).to_string())
    }

    fn alloc_local(&mut self) -> String {
        let name = format!("v{}", self.next_local);
        self.next_local += 1;
        name
    }
}

fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}
