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

struct YulEmitter<'db> {
    db: &'db DriverDataBase,
    mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    match_values: FxHashMap<ExprId, String>,
}

#[derive(Clone)]
struct BlockState {
    locals: FxHashMap<String, String>,
    next_local: usize,
}

impl BlockState {
    fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
            next_local: 0,
        }
    }

    fn alloc_local(&mut self) -> String {
        let name = format!("v{}", self.next_local);
        self.next_local += 1;
        name
    }

    fn insert_binding(&mut self, binding: String, name: String) -> String {
        self.locals.insert(binding, name.clone());
        name
    }

    fn binding(&self, binding: &str) -> Option<String> {
        self.locals.get(binding).cloned()
    }

    fn resolve_name(&self, binding: &str) -> String {
        self.binding(binding).unwrap_or_else(|| binding.to_string())
    }
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
        Ok(Self {
            db,
            mir_func,
            body,
            match_values: FxHashMap::default(),
        })
    }

    fn emit(mut self) -> Result<String, YulError> {
        let func_name = function_name(self.db, self.mir_func.func);
        let (param_names, mut state) = self.init_function_state();
        let lines = self.emit_block(self.mir_func.body.entry, &mut state)?;
        let body_text = lines.join("\n");
        Ok(format!(
            "{{\n  {} {{\n{body_text}\n  }}\n}}",
            self.format_function_signature(&func_name, &param_names)
        ))
    }

    fn init_function_state(&self) -> (Vec<String>, BlockState) {
        let mut state = BlockState::new();
        let mut params_out = Vec::new();
        if let Some(params) = self.mir_func.func.params(self.db).to_opt() {
            for (idx, param) in params.data(self.db).iter().enumerate() {
                let original = param
                    .name
                    .to_opt()
                    .and_then(|name| name.ident())
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| format!("arg{idx}"));
                let yul_name = original.clone();
                params_out.push(yul_name.clone());
                state.insert_binding(original, yul_name);
            }
        }
        (params_out, state)
    }

    fn format_function_signature(&self, func_name: &str, params: &[String]) -> String {
        let params_str = params.join(", ");
        if params.is_empty() {
            format!("function {func_name}() -> ret")
        } else {
            format!("function {func_name}({params_str}) -> ret")
        }
    }

    fn emit_block(
        &mut self,
        block_id: BasicBlockId,
        state: &mut BlockState,
    ) -> Result<Vec<String>, YulError> {
        self.emit_block_with_ctx(block_id, None, state)
    }

    fn emit_block_with_ctx(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
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
            self.match_values
                .entry(*expr_id)
                .or_insert_with(|| state.alloc_local());
        }

        let mut lines = self.render_statements(&block.insts, state)?;
        match block.terminator {
            Terminator::Return(Some(val)) => {
                let expr = match self.mir_func.body.value(val).origin {
                    ValueOrigin::Expr(expr_id) => {
                        self.lower_expr_with_statements(expr_id, &mut lines, state)?
                    }
                    _ => self.lower_value(val, state)?,
                };
                lines.push(format!("    ret := {expr}"));
                Ok(lines)
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond_expr = self.lower_value(cond, state)?;
                let mut then_state = state.clone();
                let then_lines =
                    self.emit_block_with_ctx(then_bb, loop_ctx, &mut then_state)?;
                let mut else_state = state.clone();
                let else_lines =
                    self.emit_block_with_ctx(else_bb, loop_ctx, &mut else_state)?;
                lines.push(format!("    if {cond_expr} {{"));
                extend_with_indent(&mut lines, then_lines, 2);
                lines.push("    }".into());

                lines.push(format!("    if iszero({cond_expr}) {{"));
                extend_with_indent(&mut lines, else_lines, 2);
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
                    let discr_expr = self.lower_value(discr, state)?;
                    let temp = self
                        .match_values
                        .get(&expr_id)
                        .cloned()
                        .expect("match temp must exist");
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
                                let body_expr = self.lower_expr(arm.body, state)?;
                                let literal = Self::switch_value_literal(value);
                                lines.push(format!("      case {literal} {{"));
                                lines.push(format!("        {temp} := {body_expr}"));
                                lines.push("      }".into());
                            }
                            MatchArmPattern::Wildcard => {
                                let body_expr = self.lower_expr(arm.body, state)?;
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
                    let discr_expr = self.lower_value(discr, state)?;
                    let mut cases = Vec::with_capacity(targets.len());
                    for target in targets {
                        let mut case_state = state.clone();
                        let case_lines =
                            self.emit_block_with_ctx(target.block, loop_ctx, &mut case_state)?;
                        let literal = Self::switch_value_literal(&target.value);
                        cases.push((literal, case_lines));
                    }
                    let mut default_state = state.clone();
                    let default_lines =
                        self.emit_block_with_ctx(default, loop_ctx, &mut default_state)?;

                    lines.push(format!("    switch {discr_expr}"));
                    for (value, body_lines) in cases {
                        lines.push(format!("      case {value} {{"));
                        extend_with_indent(&mut lines, body_lines, 2);
                        lines.push("      }".into());
                    }
                    lines.push("      default {".into());
                    extend_with_indent(&mut lines, default_lines, 2);
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
                    let mut loop_state = state.clone();
                    let (mut loop_lines, exit_block) =
                        self.emit_loop(target, loop_info, &mut loop_state)?;
                    lines.append(&mut loop_lines);
                    let mut after = self.emit_block_with_ctx(exit_block, loop_ctx, state)?;
                    lines.append(&mut after);
                    Ok(lines)
                } else {
                    let mut next_lines = self.emit_block_with_ctx(target, loop_ctx, state)?;
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
        state: &mut BlockState,
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
        let cond_expr = self.lower_value(cond, state)?;
        let loop_ctx = LoopEmitCtx {
            continue_target: header,
            break_target: info.exit,
            implicit_continue: info.backedge,
        };
        let body_lines = self.emit_block_with_ctx(info.body, Some(loop_ctx), state)?;
        let mut lines = Vec::new();
        lines.push(format!("    for {{ }} {cond_expr} {{ }} {{"));
        extend_with_indent(&mut lines, body_lines, 2);
        lines.push("    }".into());
        Ok((lines, info.exit))
    }

    fn render_statements(
        &mut self,
        insts: &[mir::MirInst<'_>],
        state: &mut BlockState,
    ) -> Result<Vec<String>, YulError> {
        let mut stmts = Vec::new();
        for inst in insts {
            match inst {
                mir::MirInst::Let { pat, value, .. } => {
                    let binding = self.pattern_ident(*pat)?;
                    let yul_name = if let Some(existing) = state.binding(&binding) {
                        existing
                    } else {
                        let temp = state.alloc_local();
                        state.insert_binding(binding.clone(), temp.clone())
                    };
                    let value = match value {
                        Some(val) => self.lower_value(*val, state)?,
                        None => "0".into(),
                    };
                    stmts.push(format!("    let {yul_name} := {value}"));
                }
                mir::MirInst::Assign { target, value, .. } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = state.binding(&binding).ok_or_else(|| {
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let value = self.lower_value(*value, state)?;
                    stmts.push(format!("    {yul_name} := {value}"));
                }
                mir::MirInst::AugAssign { target, value, op, .. } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = state.binding(&binding).ok_or_else(|| {
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let rhs = self.lower_value(*value, state)?;
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

    fn lower_value(&self, value_id: ValueId, state: &BlockState) -> Result<String, YulError> {
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Expr(expr_id) => {
                if let Some(temp) = self.match_values.get(expr_id) {
                    Ok(temp.clone())
                } else {
                    self.lower_expr(*expr_id, state)
                }
            }
            ValueOrigin::Call(call) => self.lower_call_value(call, state),
            _ => Err(YulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    fn lower_expr(&self, expr_id: ExprId, state: &BlockState) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id) {
            if let ValueOrigin::Call(call) = &self.mir_func.body.value(*value_id).origin {
                return self.lower_call_value(call, state);
            }
        }

        let expr = self.expect_expr(expr_id)?;
        match expr {
            Expr::Lit(LitKind::Int(int_id)) => Ok(int_id.data(self.db).to_string()),
            Expr::Lit(LitKind::Bool(value)) => Ok(if *value { "1" } else { "0" }.into()),
            Expr::Lit(LitKind::String(str_id)) => Ok(format!(
                "0x{}",
                hex::encode(str_id.data(self.db).as_bytes())
            )),
            Expr::Un(inner, op) => {
                let value = self.lower_expr(*inner, state)?;
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
                    .map(|expr| self.lower_expr(*expr, state))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(format!("tuple({})", parts.join(", ")))
            }
            Expr::Bin(lhs, rhs, bin_op) => match bin_op {
                BinOp::Arith(op) => {
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
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
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
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
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
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
                self.lower_expr(expr, state)
            }
            Expr::Path(path) => {
                let original = self.path_ident(*path).ok_or_else(|| {
                    YulError::Unsupported("unsupported path expression".into())
                })?;
                Ok(state.resolve_name(&original))
            }
            _ => Err(YulError::Unsupported(
                "only simple expressions are supported".into(),
            )),
        }
    }

    fn last_expr(&self, stmts: &[StmtId]) -> Option<ExprId> {
        stmts.iter().rev().find_map(|stmt_id| {
            let Ok(stmt) = self.expect_stmt(*stmt_id) else {
                return None;
            };
            if let Stmt::Expr(expr) = stmt {
                Some(*expr)
            } else {
                None
            }
        })
    }

    fn pattern_ident(&self, pat_id: PatId) -> Result<String, YulError> {
        let pat = self.expect_pat(pat_id)?;
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
        let expr = self.expect_expr(expr_id)?;
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

    fn expect_expr(&self, expr_id: ExprId) -> Result<&Expr<'db>, YulError> {
        match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => Ok(expr),
            Partial::Absent => Err(YulError::Unsupported(
                "expression data unavailable".into(),
            )),
        }
    }

    fn expect_pat(&self, pat_id: PatId) -> Result<&Pat<'db>, YulError> {
        match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => Ok(pat),
            Partial::Absent => Err(YulError::Unsupported(
                "unsupported pattern".into(),
            )),
        }
    }

    fn expect_stmt(&self, stmt_id: StmtId) -> Result<&Stmt<'db>, YulError> {
        match stmt_id.data(self.db, self.body) {
            Partial::Present(stmt) => Ok(stmt),
            Partial::Absent => Err(YulError::Unsupported(
                "statement data unavailable".into(),
            )),
        }
    }

    fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let Some(func) = call.callable.func_def.hir_func_def(self.db) else {
            return Err(YulError::Unsupported(
                "callable without hir function definition is not supported yet".into(),
            ));
        };
        let callee = function_name(self.db, func);
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        if lowered_args.is_empty() {
            Ok(format!("{callee}()"))
        } else {
            Ok(format!("{callee}({})", lowered_args.join(", ")))
        }
    }

    fn lower_expr_with_statements(
        &mut self,
        expr_id: ExprId,
        lines: &mut Vec<String>,
        state: &mut BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }

        let expr = self.expect_expr(expr_id)?;
        if let Expr::If(cond, then_expr, else_expr) = expr {
            let temp = state.alloc_local();
            lines.push(format!("    let {temp} := 0"));
            let cond_expr = self.lower_expr(*cond, state)?;
            let then_expr_str = self.lower_expr(*then_expr, state)?;
            lines.push(format!("    if {cond_expr} {{"));
            lines.push(format!("      {temp} := {then_expr_str}"));
            lines.push("    }".into());
            if let Some(else_expr) = else_expr {
                let else_expr_str = self.lower_expr(*else_expr, state)?;
                lines.push(format!("    if iszero({cond_expr}) {{"));
                lines.push(format!("      {temp} := {else_expr_str}"));
                lines.push("    }".into());
            }
            Ok(temp)
        } else {
            self.lower_expr(expr_id, state)
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

fn extend_with_indent(lines: &mut Vec<String>, body: Vec<String>, spaces: usize) {
    if body.is_empty() {
        return;
    }
    let pad = " ".repeat(spaces);
    for line in body {
        lines.push(format!("{pad}{line}"));
    }
}
