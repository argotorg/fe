use driver::DriverDataBase;
use hir::hir_def::{
    Body, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use mir::{
    BasicBlockId, CallOrigin, LoopInfo, MirFunction, Terminator, ValueId, ValueOrigin,
    ir::{MatchArmPattern, SwitchOrigin, SwitchTarget, SwitchValue, SyntheticValue},
    lower_module,
};
use rustc_hash::FxHashMap;

use super::{
    doc::{YulDoc, render_docs},
    errors::YulError,
    state::BlockState,
};

/// Emits Yul for every function in the lowered MIR module.
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

/// Lowers a single MIR function into the Yul document tree.
struct YulEmitter<'db> {
    db: &'db DriverDataBase,
    mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    match_values: FxHashMap<ExprId, String>,
}

#[derive(Clone, Copy)]
/// Captures where `break`/`continue` should target when rendering loops.
struct LoopEmitCtx {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
    implicit_continue: Option<BasicBlockId>,
}

impl<'db> YulEmitter<'db> {
    fn new(db: &'db DriverDataBase, mir_func: &'db MirFunction<'db>) -> Result<Self, YulError> {
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

    /// Produces the final Yul text for the current MIR function.
    fn emit(mut self) -> Result<String, YulError> {
        let func_name = self.mir_func.symbol_name.as_str();
        let (param_names, mut state) = self.init_function_state();
        let body_docs = self.emit_block(self.mir_func.body.entry, &mut state)?;
        let mut lines = Vec::new();
        render_docs(&body_docs, 4, &mut lines);
        let body_text = lines.join("\n");
        Ok(format!(
            "{{\n  {} {{\n{body_text}\n  }}\n}}",
            self.format_function_signature(&func_name, &param_names)
        ))
    }

    /// Initializes the `BlockState` with parameter bindings and returns their Yul names.
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

    /// Formats the Fe function name and parameters into a Yul signature.
    fn format_function_signature(&self, func_name: &str, params: &[String]) -> String {
        let params_str = params.join(", ");
        if params.is_empty() {
            format!("function {func_name}() -> ret")
        } else {
            format!("function {func_name}({params_str}) -> ret")
        }
    }

    /// Emits the Yul docs for a basic block starting without any active loop context.
    fn emit_block(
        &mut self,
        block_id: BasicBlockId,
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
        self.emit_block_with_ctx(block_id, None, state)
    }

    /// Emits a block while honoring the provided loop context (if any).
    fn emit_block_with_ctx(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
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

        let mut docs = self.render_statements(&block.insts, state)?;
        match block.terminator {
            Terminator::Return(Some(val)) => {
                let expr = match self.mir_func.body.value(val).origin {
                    ValueOrigin::Expr(expr_id) => {
                        self.lower_expr_with_statements(expr_id, &mut docs, state)?
                    }
                    _ => self.lower_value(val, state)?,
                };
                docs.push(YulDoc::line(format!("ret := {expr}")));
                Ok(docs)
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond_expr = self.lower_value(cond, state)?;
                let mut then_state = state.clone();
                let mut else_state = state.clone();
                let then_docs = self.emit_block_with_ctx(then_bb, loop_ctx, &mut then_state)?;
                docs.push(YulDoc::block(format!("if {cond_expr} "), then_docs));
                let else_docs = self.emit_block_with_ctx(else_bb, loop_ctx, &mut else_state)?;
                docs.push(YulDoc::block(format!("if iszero({cond_expr}) "), else_docs));
                Ok(docs)
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
                    let match_info = self.mir_func.body.match_info(expr_id).ok_or_else(|| {
                        YulError::Unsupported("missing match lowering info for switch".into())
                    })?;

                    docs.push(YulDoc::line(format!("let {temp} := 0")));
                    docs.push(YulDoc::line(format!("switch {discr_expr}")));

                    let mut default_body = None;
                    for arm in &match_info.arms {
                        match &arm.pattern {
                            MatchArmPattern::Literal(value) => {
                                let body_expr = self.lower_expr(arm.body, state)?;
                                let literal = switch_value_literal(value);
                                docs.push(YulDoc::wide_block(
                                    format!("  case {literal} "),
                                    vec![YulDoc::line(format!("{temp} := {body_expr}"))],
                                ));
                            }
                            MatchArmPattern::Enum { variant_index, .. } => {
                                let body_expr = self.lower_expr(arm.body, state)?;
                                let literal =
                                    switch_value_literal(&SwitchValue::Enum(*variant_index));
                                docs.push(YulDoc::wide_block(
                                    format!("  case {literal} "),
                                    vec![YulDoc::line(format!("{temp} := {body_expr}"))],
                                ));
                            }
                            MatchArmPattern::Wildcard => {
                                let body_expr = self.lower_expr(arm.body, state)?;
                                default_body = Some(body_expr);
                            }
                        }
                    }

                    let default_expr = default_body.ok_or_else(|| {
                        YulError::Unsupported("match lowering missing wildcard arm".into())
                    })?;
                    docs.push(YulDoc::wide_block(
                        "  default ",
                        vec![YulDoc::line(format!("{temp} := {default_expr}"))],
                    ));
                    if let Some(merge_block) = self.match_merge_block(targets, default)? {
                        let next_docs = self.emit_block_with_ctx(merge_block, loop_ctx, state)?;
                        docs.extend(next_docs);
                    }
                    Ok(docs)
                }
                SwitchOrigin::None => {
                    let discr_expr = self.lower_value(discr, state)?;
                    docs.push(YulDoc::line(format!("switch {discr_expr}")));
                    for target in targets {
                        let mut case_state = state.clone();
                        let literal = switch_value_literal(&target.value);
                        let case_docs =
                            self.emit_block_with_ctx(target.block, loop_ctx, &mut case_state)?;
                        docs.push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
                    }
                    let mut default_state = state.clone();

                    let default_docs =
                        self.emit_block_with_ctx(default, loop_ctx, &mut default_state)?;
                    docs.push(YulDoc::wide_block("  default ", default_docs));
                    Ok(docs)
                }
            },
            Terminator::Unreachable => Ok(docs),
            Terminator::Goto { target } => {
                if let Some(ctx) = loop_ctx {
                    if target == ctx.continue_target {
                        if ctx.implicit_continue == Some(block_id) {
                            return Ok(docs);
                        }
                        docs.push(YulDoc::line("continue"));
                        return Ok(docs);
                    }
                    if target == ctx.break_target {
                        docs.push(YulDoc::line("break"));
                        return Ok(docs);
                    }
                }

                if let Some(loop_info) = self.loop_info(target) {
                    let mut loop_state = state.clone();
                    let (loop_doc, exit_block) =
                        self.emit_loop(target, loop_info, &mut loop_state)?;
                    docs.push(loop_doc);
                    let after_docs = self.emit_block_with_ctx(exit_block, loop_ctx, state)?;
                    docs.extend(after_docs);
                    return Ok(docs);
                }
                let next_docs = self.emit_block_with_ctx(target, loop_ctx, state)?;
                docs.extend(next_docs);
                Ok(docs)
            }
            Terminator::Return(None) => {
                docs.push(YulDoc::line("ret := 0"));
                Ok(docs)
            }
        }
    }

    /// Finds the unified merge block that all literal match arms jump to, if any.
    fn match_merge_block(
        &self,
        targets: &[SwitchTarget],
        default: BasicBlockId,
    ) -> Result<Option<BasicBlockId>, YulError> {
        let mut merge = None;
        for block_id in targets
            .iter()
            .map(|target| target.block)
            .chain(std::iter::once(default))
        {
            let block = self
                .mir_func
                .body
                .blocks
                .get(block_id.index())
                .ok_or_else(|| YulError::Unsupported("invalid block in match lowering".into()))?;
            match block.terminator {
                Terminator::Goto { target } => match merge {
                    Some(existing) if existing != target => {
                        return Err(YulError::Unsupported(
                            "match arms must converge to a single merge block".into(),
                        ));
                    }
                    None => merge = Some(target),
                    _ => {}
                },
                Terminator::Unreachable => {}
                _ => {
                    return Err(YulError::Unsupported(
                        "match arms must jump to a merge block".into(),
                    ));
                }
            }
        }
        Ok(merge)
    }

    /// Looks up metadata about the loop that starts at `header`, if it exists.
    fn loop_info(&self, header: BasicBlockId) -> Option<LoopInfo> {
        self.mir_func.body.loop_headers.get(&header).copied()
    }

    /// Emits a Yul `for` loop for the given header block and returns the exit block.
    fn emit_loop(
        &mut self,
        header: BasicBlockId,
        info: LoopInfo,
        state: &mut BlockState,
    ) -> Result<(YulDoc, BasicBlockId), YulError> {
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
        let body_docs = self.emit_block_with_ctx(info.body, Some(loop_ctx), state)?;
        let loop_doc = YulDoc::block(format!("for {{ }} {cond_expr} {{ }} "), body_docs);
        Ok((loop_doc, info.exit))
    }

    /// Lowers straight-line MIR instructions into Yul docs.
    fn render_statements(
        &mut self,
        insts: &[mir::MirInst<'_>],
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
        let mut docs = Vec::new();
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
                    docs.push(YulDoc::line(format!("let {yul_name} := {value}")));
                }
                mir::MirInst::Assign { target, value, .. } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = state.binding(&binding).ok_or_else(|| {
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let value = self.lower_value(*value, state)?;
                    docs.push(YulDoc::line(format!("{yul_name} := {value}")));
                }
                mir::MirInst::AugAssign {
                    target, value, op, ..
                } => {
                    let binding = self.path_from_expr(*target)?;
                    let yul_name = state.binding(&binding).ok_or_else(|| {
                        YulError::Unsupported("assignment to unknown binding".into())
                    })?;
                    let rhs = self.lower_value(*value, state)?;
                    let assignment = match op {
                        ArithBinOp::Add => format!("add({yul_name}, {rhs})"),
                        ArithBinOp::Sub => format!("sub({yul_name}, {rhs})"),
                        ArithBinOp::Mul => format!("mul({yul_name}, {rhs})"),
                        ArithBinOp::Div => format!("div({yul_name}, {rhs})"),
                        ArithBinOp::Rem => format!("mod({yul_name}, {rhs})"),
                        ArithBinOp::Pow => format!("exp({yul_name}, {rhs})"),
                        ArithBinOp::LShift => format!("shl({rhs}, {yul_name})"),
                        ArithBinOp::RShift => format!("shr({rhs}, {yul_name})"),
                        ArithBinOp::BitAnd => format!("and({yul_name}, {rhs})"),
                        ArithBinOp::BitOr => format!("or({yul_name}, {rhs})"),
                        ArithBinOp::BitXor => format!("xor({yul_name}, {rhs})"),
                    };
                    docs.push(YulDoc::line(format!("{yul_name} := {assignment}")));
                }
                mir::MirInst::Eval { .. } => {}
            }
        }
        Ok(docs)
    }

    /// Lowers a MIR `ValueId` into a Yul expression string.
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
            ValueOrigin::Synthetic(synth) => self.lower_synthetic_value(synth),
            _ => Err(YulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    /// Lowers a HIR expression into a Yul expression string.
    fn lower_expr(&self, expr_id: ExprId, state: &BlockState) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id)
            && let ValueOrigin::Call(call) = &self.mir_func.body.value(*value_id).origin
        {
            return self.lower_call_value(call, state);
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
            Expr::Call(callee, call_args) => {
                let callee_expr = self.lower_expr(*callee, state)?;
                let mut lowered_args = Vec::with_capacity(call_args.len());
                for arg in call_args {
                    lowered_args.push(self.lower_expr(arg.expr, state)?);
                }
                if lowered_args.is_empty() {
                    Ok(format!("{callee_expr}()"))
                } else {
                    Ok(format!("{callee_expr}({})", lowered_args.join(", ")))
                }
            }
            Expr::Bin(lhs, rhs, bin_op) => match bin_op {
                BinOp::Arith(op) => {
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
                    match op {
                        ArithBinOp::Add => Ok(format!("add({left}, {right})")),
                        ArithBinOp::Sub => Ok(format!("sub({left}, {right})")),
                        ArithBinOp::Mul => Ok(format!("mul({left}, {right})")),
                        ArithBinOp::Div => Ok(format!("div({left}, {right})")),
                        ArithBinOp::Rem => Ok(format!("mod({left}, {right})")),
                        ArithBinOp::Pow => Ok(format!("exp({left}, {right})")),
                        ArithBinOp::LShift => Ok(format!("shl({right}, {left})")),
                        ArithBinOp::RShift => Ok(format!("shr({right}, {left})")),
                        ArithBinOp::BitAnd => Ok(format!("and({left}, {right})")),
                        ArithBinOp::BitOr => Ok(format!("or({left}, {right})")),
                        ArithBinOp::BitXor => Ok(format!("xor({left}, {right})")),
                    }
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
                let original = self
                    .path_ident(*path)
                    .ok_or_else(|| YulError::Unsupported("unsupported path expression".into()))?;
                Ok(state.resolve_name(&original))
            }
            Expr::Field(..) => {
                let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
                Err(YulError::Unsupported(format!(
                    "field expressions should be rewritten before codegen (expr type {})",
                    ty.pretty_print(self.db)
                )))
            }
            other => Err(YulError::Unsupported(format!(
                "only simple expressions are supported: {other:?}"
            ))),
        }
    }

    /// Returns the last expression statement in a block, if any.
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

    /// Extracts the identifier bound by a pattern.
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

    /// Resolves an expression that should represent a path (e.g. assignment target).
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

    /// Returns the identifier name represented by a path, if it is a plain ident.
    fn path_ident(&self, path: Partial<PathId<'_>>) -> Option<String> {
        let path = path.to_opt()?;
        path.as_ident(self.db)
            .map(|id| id.data(self.db).to_string())
    }

    /// Fetches the expression from HIR, converting missing data into `YulError`.
    fn expect_expr(&self, expr_id: ExprId) -> Result<&Expr<'db>, YulError> {
        match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => Ok(expr),
            Partial::Absent => Err(YulError::Unsupported("expression data unavailable".into())),
        }
    }

    /// Fetches the pattern from HIR, converting missing data into `YulError`.
    fn expect_pat(&self, pat_id: PatId) -> Result<&Pat<'db>, YulError> {
        match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => Ok(pat),
            Partial::Absent => Err(YulError::Unsupported("unsupported pattern".into())),
        }
    }

    /// Fetches the statement from HIR, converting missing data into `YulError`.
    fn expect_stmt(&self, stmt_id: StmtId) -> Result<&Stmt<'db>, YulError> {
        match stmt_id.data(self.db, self.body) {
            Partial::Present(stmt) => Ok(stmt),
            Partial::Absent => Err(YulError::Unsupported("statement data unavailable".into())),
        }
    }

    /// Lowers a MIR call into a Yul function invocation.
    fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            let Some(func) = call.callable.func_def.hir_func_def(self.db) else {
                return Err(YulError::Unsupported(
                    "callable without hir function definition is not supported yet".into(),
                ));
            };
            function_name(self.db, func)
        };
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

    fn lower_synthetic_value(&self, value: &SyntheticValue) -> Result<String, YulError> {
        match value {
            SyntheticValue::Int(int) => Ok(int.to_string()),
            SyntheticValue::Bool(flag) => Ok(if *flag { "1" } else { "0" }.into()),
        }
    }

    /// Lowers expressions that may require extra statements (e.g. `if`).
    fn lower_expr_with_statements(
        &mut self,
        expr_id: ExprId,
        docs: &mut Vec<YulDoc>,
        state: &mut BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }

        let expr = self.expect_expr(expr_id)?;
        if let Expr::If(cond, then_expr, else_expr) = expr {
            let temp = state.alloc_local();
            docs.push(YulDoc::line(format!("let {temp} := 0")));
            let cond_expr = self.lower_expr(*cond, state)?;
            let then_expr_str = self.lower_expr(*then_expr, state)?;
            docs.push(YulDoc::block(
                format!("if {cond_expr} "),
                vec![YulDoc::line(format!("{temp} := {then_expr_str}"))],
            ));
            if let Some(else_expr) = else_expr {
                let else_expr_str = self.lower_expr(*else_expr, state)?;
                docs.push(YulDoc::block(
                    format!("if iszero({cond_expr}) "),
                    vec![YulDoc::line(format!("{temp} := {else_expr_str}"))],
                ));
            }
            Ok(temp)
        } else {
            self.lower_expr(expr_id, state)
        }
    }
}

/// Translates MIR switch literal kinds into their Yul literal strings.
fn switch_value_literal(value: &SwitchValue) -> String {
    match value {
        SwitchValue::Bool(true) => "1".into(),
        SwitchValue::Bool(false) => "0".into(),
        SwitchValue::Int(int) => int.to_string(),
        SwitchValue::Enum(val) => val.to_string(),
    }
}

/// Returns the display name of a function or `<anonymous>` if one does not exist.
fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}
