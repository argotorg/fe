use std::fmt;

use driver::DriverDataBase;
use hir::hir_def::{
    Body, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use mir::{
    ir::{MatchArmPattern, SwitchOrigin, SwitchValue},
    BasicBlockId, CallOrigin, LoopInfo, MirFunction, Terminator, ValueId, ValueOrigin,
    lower_module,
};
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub enum YulError {
    MissingBody(String),
    Unsupported(String),
}

impl fmt::Display for YulError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YulError::MissingBody(name) => {
                write!(f, "function `{name}` does not have a body")
            }
            YulError::Unsupported(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for YulError {}

pub fn emit_module_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<Vec<Result<String, YulError>>, mir::MirLowerError> {
    let module = lower_module(db, top_mod)?;
    Ok(module
        .functions
        .iter()
        .map(|func| YulEmitter::new(db, func).and_then(|emitter| emitter.emit()))
        .collect())
}

#[derive(Clone)]
struct YulEmitter<'db> {
    db: &'db DriverDataBase,
    mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    locals: FxHashMap<String, String>,
    param_names: Vec<String>,
    next_local: usize,
    match_values: FxHashMap<ExprId, String>,
}

#[derive(Clone, Copy)]
struct LoopEmitCtx {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
    implicit_continue: Option<BasicBlockId>,
}

impl<'db> YulEmitter<'db> {
    fn new(
        db: &'db DriverDataBase,
        mir_func: &'db MirFunction<'db>,
    ) -> Result<Self, YulError> {
        let body = mir_func
            .func
            .body(db)
            .ok_or_else(|| YulError::MissingBody(function_name(db, mir_func.func)))?;
        let mut this = Self {
            db,
            mir_func,
            body,
            locals: FxHashMap::default(),
            param_names: Vec::new(),
            next_local: 0,
            match_values: FxHashMap::default(),
        };
        this.init_params();
        Ok(this)
    }

    fn emit(mut self) -> Result<String, YulError> {
        let func_name = function_name(self.db, self.mir_func.func);
        let params = if self.param_names.is_empty() {
            String::new()
        } else {
            self.param_names.join(", ")
        };
        let lines = self.emit_block(self.mir_func.body.entry)?;
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
    fn emit_block(&mut self, block_id: BasicBlockId) -> Result<Vec<String>, YulError> {
        self.emit_block_with_ctx(block_id, None)
    }

    fn emit_block_with_ctx(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
    ) -> Result<Vec<String>, YulError> {
        let block = self
            .mir_func
            .body
            .blocks
            .get(block_id.index())
            .ok_or_else(|| YulError::Unsupported("invalid block".into()))?;

        if let Terminator::Switch {
            origin: SwitchOrigin::MatchExpr(expr_id),
            ..
        } = &block.terminator
        {
            if !self.match_values.contains_key(expr_id) {
                let temp = self.alloc_local();
                self.match_values.insert(*expr_id, temp);
            }
        }

        let mut lines = self.render_statements(&block.insts)?;
        match block.terminator {
            Terminator::Return(Some(val)) => {
                let expr = match self.mir_func.body.value(val).origin {
                    ValueOrigin::Expr(expr_id) => {
                        self.lower_expr_with_statements(expr_id, &mut lines)?
                    }
                    _ => self.lower_value(val)?,
                };
                lines.push(format!("    ret := {expr}"));
                Ok(lines)
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond_expr = self.lower_value(cond)?;
                let mut then_emitter = self.clone();
                let then_lines = then_emitter.emit_block_with_ctx(then_bb, loop_ctx)?;
                let mut else_emitter = self.clone();
                let else_lines = else_emitter.emit_block_with_ctx(else_bb, loop_ctx)?;

                lines.push(format!("    if {cond_expr} {{"));
                for line in then_lines {
                    lines.push(format!("  {line}"));
                }
                lines.push("    }".into());

                lines.push(format!("    if iszero({cond_expr}) {{"));
                for line in else_lines {
                    lines.push(format!("  {line}"));
                }
                lines.push("    }".into());
                Ok(lines)
            }
            Terminator::Switch {
                discr,
                ref targets,
                default,
                origin,
            } => match origin {
                SwitchOrigin::MatchExpr(expr_id) => {
                    let discr_expr = self.lower_value(discr)?;
                    let temp = self
                        .match_values
                        .get(&expr_id)
                        .cloned()
                        .unwrap_or_else(|| self.alloc_local());
                    let match_info = self
                        .mir_func
                        .body
                        .match_info(expr_id)
                        .ok_or_else(|| {
                            YulError::Unsupported(
                                "missing match lowering info for switch".into(),
                            )
                        })?;

                    lines.push(format!("    let {temp} := 0"));
                    lines.push(format!("    switch {discr_expr}"));

                    let mut default_body = None;
                    for arm in &match_info.arms {
                        match &arm.pattern {
                            MatchArmPattern::Literal(value) => {
                                let body_expr = self.lower_expr(arm.body)?;
                                let literal = Self::switch_value_literal(value);
                                lines.push(format!("      case {literal} {{"));
                                lines.push(format!("        {temp} := {body_expr}"));
                                lines.push("      }".into());
                            }
                            MatchArmPattern::Wildcard => {
                                let body_expr = self.lower_expr(arm.body)?;
                                default_body = Some(body_expr);
                            }
                        }
                    }

                    let default_expr = default_body.ok_or_else(|| {
                        YulError::Unsupported(
                            "match lowering missing wildcard arm".into(),
                        )
                    })?;
                    lines.push("      default {".into());
                    lines.push(format!("        {temp} := {default_expr}"));
                    lines.push("      }".into());
                    Ok(lines)
                }
                SwitchOrigin::None => {
                    let discr_expr = self.lower_value(discr)?;
                    let mut cases = Vec::with_capacity(targets.len());
                    for target in targets {
                        let mut case_emitter = self.clone();
                        let case_lines =
                            case_emitter.emit_block_with_ctx(target.block, loop_ctx)?;
                        let literal = Self::switch_value_literal(&target.value);
                        cases.push((literal, case_lines));
                    }
                    let mut default_emitter = self.clone();
                    let default_lines = default_emitter.emit_block_with_ctx(default, loop_ctx)?;

                    lines.push(format!("    switch {discr_expr}"));
                    for (value, body_lines) in cases {
                        lines.push(format!("      case {value} {{"));
                        for line in body_lines {
                            lines.push(format!("  {line}"));
                        }
                        lines.push("      }".into());
                    }
                    lines.push("      default {".into());
                    for line in default_lines {
                        lines.push(format!("  {line}"));
                    }
                    lines.push("      }".into());
                    Ok(lines)
                }
            },
            Terminator::Unreachable => Ok(lines),
            Terminator::Goto { target } => {
                if let Some(ctx) = loop_ctx {
                    if target == ctx.continue_target {
                        if ctx.implicit_continue == Some(block_id) {
                            return Ok(lines);
                        }
                        lines.push("    continue".into());
                        return Ok(lines);
                    }
                    if target == ctx.break_target {
                        lines.push("    break".into());
                        return Ok(lines);
                    }
                }

                if let Some(loop_info) = self.loop_info(target) {
                    let (mut loop_lines, exit_block) = self.emit_loop(target, loop_info)?;
                    lines.append(&mut loop_lines);
                    let mut after = self.emit_block_with_ctx(exit_block, loop_ctx)?;
                    lines.append(&mut after);
                    Ok(lines)
                } else {
                    let mut next_lines = self.emit_block_with_ctx(target, loop_ctx)?;
                    lines.append(&mut next_lines);
                    Ok(lines)
                }
            }
            Terminator::Return(None) => {
                lines.push("    ret := 0".into());
                Ok(lines)
            }
        }
    }

    fn loop_info(&self, header: BasicBlockId) -> Option<LoopInfo> {
        self.mir_func.body.loop_headers.get(&header).copied()
    }

    fn emit_loop(
        &mut self,
        header: BasicBlockId,
        info: LoopInfo,
    ) -> Result<(Vec<String>, BasicBlockId), YulError> {
        let block = self
            .mir_func
            .body
            .blocks
            .get(header.index())
            .ok_or_else(|| YulError::Unsupported("invalid loop header".into()))?;
        let Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        } = block.terminator
        else {
            return Err(YulError::Unsupported(
                "loop header missing branch terminator".into(),
            ));
        };
        if then_bb != info.body || else_bb != info.exit {
            return Err(YulError::Unsupported(
                "loop metadata inconsistent with terminator".into(),
            ));
        }
        let cond_expr = self.lower_value(cond)?;
        let loop_ctx = LoopEmitCtx {
            continue_target: header,
            break_target: info.exit,
            implicit_continue: info.backedge,
        };
        let body_lines = self.emit_block_with_ctx(info.body, Some(loop_ctx))?;
        let mut lines = Vec::new();
        lines.push(format!("    for {{ }} {cond_expr} {{ }} {{"));
        for line in body_lines {
            lines.push(format!("  {line}"));
        }
        lines.push("    }".into());
        Ok((lines, info.exit))
    }

    fn render_statements(
        &mut self,
        insts: &[mir::MirInst<'_>],
    ) -> Result<Vec<String>, YulError> {
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
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let value = self.lower_value(*value)?;
                    stmts.push(format!("    {yul_name} := {value}"));
                }
                mir::MirInst::AugAssign { target, value, op, .. } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = self.locals.get(&binding).cloned().ok_or_else(|| {
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let rhs = self.lower_value(*value)?;
                    let func = match op {
                        ArithBinOp::Add => "add",
                        ArithBinOp::Sub => "sub",
                        ArithBinOp::Mul => "mul",
                        ArithBinOp::Div => "div",
                        ArithBinOp::Rem => "mod",
                        _ => {
                            return Err(YulError::Unsupported(
                                "augmented assignment only supports add/sub/mul/div/mod".into(),
                            ))
                        }
                    };
                    stmts.push(format!("    {yul_name} := {func}({yul_name}, {rhs})"));
                }
                mir::MirInst::Eval { .. } => {}
            }
        }
        Ok(stmts)
    }

    fn lower_value(&mut self, value_id: ValueId) -> Result<String, YulError> {
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Expr(expr_id) => {
                if let Some(temp) = self.match_values.get(expr_id) {
                    Ok(temp.clone())
                } else {
                    self.lower_expr(*expr_id)
                }
            }
            ValueOrigin::Call(call) => self.lower_call_value(call),
            _ => Err(YulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    fn lower_expr(&mut self, expr_id: ExprId) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id) {
            if let ValueOrigin::Call(call) = &self.mir_func.body.value(*value_id).origin {
                return self.lower_call_value(call);
            }
        }

        let expr = match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                return Err(YulError::Unsupported(
                    "expression data unavailable".into(),
                ));
            }
        };
        match expr {
            Expr::Lit(LitKind::Int(int_id)) => Ok(int_id.data(self.db).to_string()),
            Expr::Lit(LitKind::Bool(value)) => Ok(if *value { "1" } else { "0" }.into()),
            Expr::Lit(LitKind::String(str_id)) => Ok(format!(
                "0x{}",
                hex::encode(str_id.data(self.db).as_bytes())
            )),
            Expr::Un(inner, op) => {
                let value = self.lower_expr(*inner)?;
                match op {
                    UnOp::Minus => Ok(format!("sub(0, {value})")),
                    UnOp::Not => Ok(format!("iszero({value})")),
                    UnOp::Plus => Ok(value),
                    UnOp::BitNot => Ok(format!("not({value})")),
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
                        ArithBinOp::Pow => "exp",
                        ArithBinOp::LShift => "shl",
                        ArithBinOp::RShift => "shr",
                        ArithBinOp::BitAnd => "and",
                        ArithBinOp::BitOr => "or",
                        ArithBinOp::BitXor => "xor",
                    };
                    Ok(format!("{func}({left}, {right})"))
                }
                BinOp::Comp(op) => {
                    let left = self.lower_expr(*lhs)?;
                    let right = self.lower_expr(*rhs)?;
                    let expr = match op {
                        CompBinOp::Eq => format!("eq({left}, {right})"),
                        CompBinOp::NotEq => format!("iszero(eq({left}, {right}))"),
                        CompBinOp::Lt => format!("lt({left}, {right})"),
                        CompBinOp::LtEq => format!("iszero(gt({left}, {right}))"),
                        CompBinOp::Gt => format!("gt({left}, {right})"),
                        CompBinOp::GtEq => format!("iszero(lt({left}, {right}))"),
                    };
                    Ok(expr)
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
                _ => Err(YulError::Unsupported(
                    "only arithmetic/logical binary expressions are supported right now".into(),
                )),
            },
            Expr::Block(stmts) => {
                let Some(expr) = self.last_expr(stmts) else {
                    return Err(YulError::Unsupported(
                        "block without terminal expression".into(),
                    ));
                };
                self.lower_expr(expr)
            }
            Expr::Path(path) => {
                let original = self.path_ident(*path).ok_or_else(|| {
                    YulError::Unsupported("unsupported path expression".into())
                })?;
                Ok(self.locals.get(&original).cloned().unwrap_or(original))
            }
            _ => Err(YulError::Unsupported(
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

    fn pattern_ident(&mut self, pat_id: PatId) -> Result<String, YulError> {
        let pat = match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => pat,
            Partial::Absent => {
                return Err(YulError::Unsupported("unsupported pattern".into()));
            }
        };
        match pat {
            Pat::Path(path, _) => self
                .path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported pattern path".into())),
            _ => Err(YulError::Unsupported(
                "only identifier patterns are supported".into(),
            )),
        }
    }

    fn path_from_expr(&self, expr_id: ExprId) -> Result<String, YulError> {
        let expr = match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                return Err(YulError::Unsupported(
                    "unsupported assignment target".into(),
                ));
            }
        };
        if let Expr::Path(path) = expr {
            self.path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported assignment target".into()))
        } else {
            Err(YulError::Unsupported(
                "only identifier assignments are supported".into(),
            ))
        }
    }

    fn path_ident(&self, path: Partial<PathId<'_>>) -> Option<String> {
        let path = path.to_opt()?;
        path.as_ident(self.db)
            .map(|id| id.data(self.db).to_string())
    }

    fn lower_call_value(&mut self, call: &CallOrigin<'_>) -> Result<String, YulError> {
        let Some(func) = call.callable.func_def.hir_func_def(self.db) else {
            return Err(YulError::Unsupported(
                "callable without hir function definition is not supported yet".into(),
            ));
        };
        let callee = function_name(self.db, func);
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg)?);
        }
        if lowered_args.is_empty() {
            Ok(format!("{callee}()"))
        } else {
            Ok(format!("{callee}({})", lowered_args.join(", ")))
        }
    }

    fn alloc_local(&mut self) -> String {
        let name = format!("v{}", self.next_local);
        self.next_local += 1;
        name
    }

    fn lower_expr_with_statements(
        &mut self,
        expr_id: ExprId,
        lines: &mut Vec<String>,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }

        let expr = match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                return Err(YulError::Unsupported(
                    "expression data unavailable".into(),
                ));
            }
        };
        if let Expr::If(cond, then_expr, else_expr) = expr {
            let temp = self.alloc_local();
            lines.push(format!("    let {temp} := 0"));
            let cond_expr = self.lower_expr(*cond)?;
            let then_expr_str = self.lower_expr(*then_expr)?;
            lines.push(format!("    if {cond_expr} {{"));
            lines.push(format!("      {temp} := {then_expr_str}"));
            lines.push("    }".into());
            if let Some(else_expr) = else_expr {
                let else_expr_str = self.lower_expr(*else_expr)?;
                lines.push(format!("    if iszero({cond_expr}) {{"));
                lines.push(format!("      {temp} := {else_expr_str}"));
                lines.push("    }".into());
            }
            Ok(temp)
        } else {
            self.lower_expr(expr_id)
        }
    }

    fn switch_value_literal(value: &SwitchValue) -> String {
        match value {
            SwitchValue::Bool(true) => "1".into(),
            SwitchValue::Bool(false) => "0".into(),
            SwitchValue::Int(int) => int.to_string(),
        }
    }
}

fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}
