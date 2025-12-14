//! Helpers for lowering MIR control-flow constructs into Yul blocks.

use hir::hir_def::ExprId;
use mir::{
    BasicBlockId, LoopInfo, Terminator, ValueId, ValueOrigin,
    ir::{
        MatchArmLowering, MatchArmPattern, MatchLoweringInfo, SwitchOrigin, SwitchTarget,
        SwitchValue,
    },
};

use crate::yul::{doc::YulDoc, errors::YulError, state::BlockState};

use super::function::FunctionEmitter;

/// Captures the `break`/`continue` destinations for loop lowering.
#[derive(Clone, Copy)]
pub(super) struct LoopEmitCtx {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
    implicit_continue: Option<BasicBlockId>,
}

/// Shared mutable context passed through control-flow helpers.
pub(super) struct BlockEmitCtx<'state, 'docs> {
    pub(super) loop_ctx: Option<LoopEmitCtx>,
    pub(super) state: &'state mut BlockState,
    pub(super) docs: &'docs mut Vec<YulDoc>,
}

impl<'state, 'docs> BlockEmitCtx<'state, 'docs> {
    /// Convenience helper for cloning the block state.
    fn cloned_state(&self) -> BlockState {
        self.state.clone()
    }
}

impl<'db> FunctionEmitter<'db> {
    /// Emits the Yul docs for a basic block starting without any active loop context.
    ///
    /// * `block_id` - Entry block to render.
    /// * `state` - Current SSA-like binding state.
    ///
    /// Returns the rendered statements for the block.
    pub(super) fn emit_block(
        &mut self,
        block_id: BasicBlockId,
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
        self.emit_block_internal(block_id, None, state, None)
    }

    /// Emits a block while honoring the provided loop context (if any).
    ///
    /// * `block_id` - Destination to render.
    /// * `loop_ctx` - Active loop metadata or `None`.
    /// * `state` - Mutable block state containing bindings.
    ///
    /// Returns the rendered Yul docs for the block.
    pub(super) fn emit_block_with_ctx(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
        self.emit_block_internal(block_id, loop_ctx, state, None)
    }

    /// Emits a block while preventing recursion into `stop_block`.
    ///
    /// * `block_id` - Entry block to render.
    /// * `loop_ctx` - Active loop metadata or `None`.
    /// * `state` - Mutable binding state.
    /// * `stop_block` - Optional merge block that should not be revisited.
    ///
    /// Returns the rendered docs, stopping before re-entering `stop_block`.
    pub(super) fn emit_block_with_stop(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
        stop_block: Option<BasicBlockId>,
    ) -> Result<Vec<YulDoc>, YulError> {
        self.emit_block_internal(block_id, loop_ctx, state, stop_block)
    }

    /// Core implementation shared by the various block emitters.
    ///
    /// * `block_id` - Entry block.
    /// * `loop_ctx` - Optional surrounding loop context.
    /// * `state` - Current binding state.
    /// * `stop_block` - Merge block to skip (if any).
    ///
    /// Returns the rendered statements produced while traversing the block.
    fn emit_block_internal(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
        stop_block: Option<BasicBlockId>,
    ) -> Result<Vec<YulDoc>, YulError> {
        if Some(block_id) == stop_block {
            return Ok(Vec::new());
        }
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
            && !self.expr_is_unit(*expr_id)
        {
            self.match_values
                .entry(*expr_id)
                .or_insert_with(|| state.alloc_local());
        }

        let mut docs = self.render_statements(&block.insts, state)?;
        {
            let mut ctx = BlockEmitCtx {
                loop_ctx,
                state,
                docs: &mut docs,
            };
            self.emit_block_terminator(block_id, &block.terminator, &mut ctx)?;
        }
        Ok(docs)
    }

    /// Renders the control-flow terminator for a block after its linear statements.
    ///
    /// * `block_id` - Current block emitting statements.
    /// * `terminator` - MIR terminator describing the outgoing control flow.
    /// * `ctx` - Shared mutable context spanning the block's docs and bindings.
    fn emit_block_terminator(
        &mut self,
        block_id: BasicBlockId,
        terminator: &Terminator,
        ctx: &mut BlockEmitCtx<'_, '_>,
    ) -> Result<(), YulError> {
        match terminator {
            Terminator::Return(Some(val)) => self.emit_return_with_value(*val, ctx.docs, ctx.state),
            Terminator::Return(None) => {
                if self.returns_value() {
                    ctx.docs.push(YulDoc::line("ret := 0"));
                }
                Ok(())
            }
            Terminator::ReturnData { offset, size } => {
                let offset_expr = self.lower_value(*offset, ctx.state)?;
                let size_expr = self.lower_value(*size, ctx.state)?;
                ctx.docs
                    .push(YulDoc::line(format!("return({offset_expr}, {size_expr})")));
                Ok(())
            }
            Terminator::Revert { offset, size } => {
                let offset_expr = self.lower_value(*offset, ctx.state)?;
                let size_expr = self.lower_value(*size, ctx.state)?;
                ctx.docs
                    .push(YulDoc::line(format!("revert({offset_expr}, {size_expr})")));
                Ok(())
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => self.emit_branch_terminator(*cond, *then_bb, *else_bb, ctx),
            Terminator::Switch {
                discr,
                targets,
                default,
                origin,
            } => self.emit_switch_terminator(*discr, targets, *default, origin, ctx),
            Terminator::Goto { target } => self.emit_goto_terminator(block_id, *target, ctx),
            Terminator::Unreachable => Ok(()),
        }
    }

    /// Emits a return terminator. When the function has no return value, this merely
    /// evaluates the expression for side effects.
    ///
    /// * `value_id` - MIR value selected by the `return` terminator.
    /// * `docs` - Doc list collecting emitted statements.
    /// * `state` - Binding table used when lowering the return expression.
    ///
    /// Returns an error if the return value could not be lowered.
    fn emit_return_with_value(
        &mut self,
        value_id: ValueId,
        docs: &mut Vec<YulDoc>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if self.emit_intrinsic_return(value_id, docs, state, self.returns_value())? {
            return Ok(());
        }
        if !self.returns_value() {
            return Ok(());
        }
        let value = self.mir_func.body.value(value_id);
        if value.ty.is_tuple(self.db) && value.ty.field_count(self.db) == 0 {
            docs.push(YulDoc::line("ret := 0"));
            return Ok(());
        }
        let expr = match &value.origin {
            ValueOrigin::Expr(expr_id) => self.lower_expr_with_statements(*expr_id, docs, state)?,
            _ => self.lower_value(value_id, state)?,
        };
        docs.push(YulDoc::line(format!("ret := {expr}")));
        Ok(())
    }

    /// Lowers an `if cond -> then else` branch terminator into Yul conditionals.
    ///
    /// * `cond` - MIR value representing the branch predicate.
    /// * `then_bb` / `else_bb` - Successor blocks for each branch.
    /// * `ctx` - Shared block context containing loop metadata and bindings.
    fn emit_branch_terminator(
        &mut self,
        cond: ValueId,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
        ctx: &mut BlockEmitCtx<'_, '_>,
    ) -> Result<(), YulError> {
        let cond_expr = self.lower_value(cond, ctx.state)?;
        let cond_temp = ctx.state.alloc_local();
        ctx.docs
            .push(YulDoc::line(format!("let {cond_temp} := {cond_expr}")));
        let loop_ctx = ctx.loop_ctx;
        let mut then_state = ctx.cloned_state();
        let mut else_state = ctx.cloned_state();
        let then_docs = self.emit_block_with_ctx(then_bb, loop_ctx, &mut then_state)?;
        ctx.docs
            .push(YulDoc::block(format!("if {cond_temp} "), then_docs));
        let else_docs = self.emit_block_with_ctx(else_bb, loop_ctx, &mut else_state)?;
        ctx.docs
            .push(YulDoc::block(format!("if iszero({cond_temp}) "), else_docs));
        Ok(())
    }

    /// Emits a `switch` terminator, handling both match-driven and raw switches.
    ///
    /// * `discr` - MIR value containing the discriminant expression.
    /// * `targets` - All concrete switch targets.
    /// * `default` - Default target block.
    /// * `origin` - Whether this switch originated from a match expression.
    /// * `ctx` - Shared block context reused across successor emission.
    fn emit_switch_terminator(
        &mut self,
        discr: ValueId,
        targets: &[SwitchTarget],
        default: BasicBlockId,
        origin: &SwitchOrigin,
        ctx: &mut BlockEmitCtx<'_, '_>,
    ) -> Result<(), YulError> {
        match origin {
            SwitchOrigin::MatchExpr(expr_id) => {
                self.emit_match_switch(*expr_id, discr, targets, default, ctx)
            }
            SwitchOrigin::None => {
                let discr_expr = self.lower_value(discr, ctx.state)?;
                ctx.docs.push(YulDoc::line(format!("switch {discr_expr}")));
                let loop_ctx = ctx.loop_ctx;
                for target in targets {
                    let mut case_state = ctx.cloned_state();
                    let literal = switch_value_literal(&target.value);
                    let case_docs =
                        self.emit_block_with_ctx(target.block, loop_ctx, &mut case_state)?;
                    ctx.docs
                        .push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
                }
                let mut default_state = ctx.cloned_state();
                let default_docs =
                    self.emit_block_with_ctx(default, loop_ctx, &mut default_state)?;
                ctx.docs
                    .push(YulDoc::wide_block("  default ", default_docs));
                Ok(())
            }
        }
    }

    /// Emits the specialized lowering used for match expressions backed by a switch.
    ///
    /// * `ctx` - Shared block context containing current state/docs.
    fn emit_match_switch(
        &mut self,
        expr_id: ExprId,
        discr: ValueId,
        targets: &[SwitchTarget],
        default: BasicBlockId,
        ctx: &mut BlockEmitCtx<'_, '_>,
    ) -> Result<(), YulError> {
        let discr_expr = self.lower_value(discr, ctx.state)?;
        let match_info = self.mir_func.body.match_info(expr_id).ok_or_else(|| {
            YulError::Unsupported("missing match lowering info for switch".into())
        })?;
        let merge_block = self.match_merge_block(match_info)?;
        if self.expr_is_unit(expr_id) {
            ctx.docs.push(YulDoc::line(format!("switch {discr_expr}")));
            let loop_ctx = ctx.loop_ctx;
            for target in targets {
                let mut case_state = ctx.cloned_state();
                let case_docs = self.emit_block_with_stop(
                    target.block,
                    loop_ctx,
                    &mut case_state,
                    merge_block,
                )?;
                let literal = switch_value_literal(&target.value);
                ctx.docs
                    .push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
            }
            let mut default_state = ctx.cloned_state();
            let default_docs =
                self.emit_block_with_stop(default, loop_ctx, &mut default_state, merge_block)?;
            ctx.docs
                .push(YulDoc::wide_block("  default ", default_docs));
            if let Some(merge_block) = merge_block {
                let next_docs = self.emit_block_with_ctx(merge_block, loop_ctx, ctx.state)?;
                ctx.docs.extend(next_docs);
            }
            return Ok(());
        }

        let temp = merge_block.map(|_| {
            self.match_values
                .entry(expr_id)
                .or_insert_with(|| ctx.state.alloc_local())
                .to_owned()
        });

        if let Some(temp_name) = temp.as_ref() {
            ctx.docs.push(YulDoc::line(format!("let {temp_name} := 0")));
        }
        ctx.docs.push(YulDoc::line(format!("switch {discr_expr}")));

        let mut default_body = None;
        for arm in &match_info.arms {
            let mut arm_state = ctx.cloned_state();
            let mut arm_docs = Vec::new();

            // Emit enum variant bindings if present.
            if let MatchArmPattern::Enum { bindings, .. } = &arm.pattern
                && !bindings.is_empty()
            {
                for binding in bindings {
                    let binding_name = self.pattern_ident(binding.pat_id)?;
                    let value_id = binding.value.ok_or_else(|| {
                        YulError::Unsupported(
                            "enum binding missing lowered get_variant_field load".into(),
                        )
                    })?;
                    let load_expr = self.lower_value(value_id, &arm_state)?;
                    let temp_name = arm_state.alloc_local();
                    arm_docs.push(YulDoc::line(format!("let {temp_name} := {load_expr}")));
                    arm_state.insert_binding(binding_name, temp_name);
                }
            }

            // Emit arm body - either as a terminating block or as a value assignment.
            let case_docs = self.emit_match_arm_body(
                arm,
                temp.as_ref(),
                merge_block,
                ctx.loop_ctx,
                &mut arm_state,
                arm_docs,
            )?;

            // Route to either a numbered case or the default arm.
            match &arm.pattern {
                MatchArmPattern::Literal(value) => {
                    let literal = switch_value_literal(value);
                    ctx.docs
                        .push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
                }
                MatchArmPattern::Enum { variant_index, .. } => {
                    let literal = switch_value_literal(&SwitchValue::Enum(*variant_index));
                    ctx.docs
                        .push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
                }
                MatchArmPattern::Wildcard => {
                    default_body = Some(case_docs);
                }
            }
        }

        let loop_ctx = ctx.loop_ctx;
        if let Some(default_docs) = default_body {
            ctx.docs
                .push(YulDoc::wide_block("  default ", default_docs));
        } else {
            let default_block = self
                .mir_func
                .body
                .blocks
                .get(default.index())
                .ok_or_else(|| YulError::Unsupported("invalid block in match lowering".into()))?;
            if !matches!(default_block.terminator, Terminator::Unreachable) {
                return Err(YulError::Unsupported(
                    "match lowering missing wildcard arm".into(),
                ));
            }
            let mut default_state = ctx.cloned_state();
            let default_docs =
                self.emit_block_with_stop(default, loop_ctx, &mut default_state, merge_block)?;
            ctx.docs
                .push(YulDoc::wide_block("  default ", default_docs));
        }
        if let Some(merge_block) = merge_block {
            let next_docs = self.emit_block_with_ctx(merge_block, loop_ctx, ctx.state)?;
            ctx.docs.extend(next_docs);
        }
        Ok(())
    }

    /// Emits the body of a single match arm, handling both terminating and value-producing arms.
    ///
    /// For terminating arms (or when no temp variable exists), emits the arm's block and appends
    /// an exit instruction (`leave` or `return`). For value-producing arms, lowers the body
    /// expression and assigns it to the temp variable.
    ///
    /// * `arm` - Match arm metadata from MIR lowering.
    /// * `temp` - Optional temp variable name for storing the match result.
    /// * `merge_block` - Block where non-terminating arms converge.
    /// * `loop_ctx` - Active loop context, if any.
    /// * `arm_state` - Binding state for this arm.
    /// * `arm_docs` - Pre-populated docs (e.g. enum bindings) to include in output.
    ///
    /// Returns the complete docs for the arm's case body.
    fn emit_match_arm_body(
        &mut self,
        arm: &MatchArmLowering,
        temp: Option<&String>,
        merge_block: Option<BasicBlockId>,
        loop_ctx: Option<LoopEmitCtx>,
        arm_state: &mut BlockState,
        mut arm_docs: Vec<YulDoc>,
    ) -> Result<Vec<YulDoc>, YulError> {
        // Terminating arms or arms without a temp emit the block directly.
        if arm.terminates || temp.is_none() {
            let mut case_docs =
                self.emit_block_with_stop(arm.block, loop_ctx, arm_state, merge_block)?;
            // Prepend enum bindings to case body.
            arm_docs.append(&mut case_docs);
            if arm.terminates {
                if self.returns_value() {
                    arm_docs.push(YulDoc::line("leave"));
                } else {
                    arm_docs.push(YulDoc::line("return(0, 0)"));
                }
            }
            return Ok(arm_docs);
        }

        // Value-producing arms assign to the temp variable.
        let body_expr = self.lower_expr(arm.body, arm_state)?;
        let temp = temp.expect("temp must exist for non-terminating arms with merge block");
        arm_docs.push(YulDoc::line(format!("{temp} := {body_expr}")));
        Ok(arm_docs)
    }

    /// Handles `goto` terminators, translating loop jumps into `break`/`continue`
    /// and recursively emitting successor blocks otherwise.
    ///
    /// * `block_id` - Current block index (used for implicit continues).
    /// * `target` - Destination block selected by the `goto`.
    /// * `ctx` - Shared context holding the current bindings and docs.
    fn emit_goto_terminator(
        &mut self,
        block_id: BasicBlockId,
        target: BasicBlockId,
        ctx: &mut BlockEmitCtx<'_, '_>,
    ) -> Result<(), YulError> {
        if let Some(loop_ctx) = ctx.loop_ctx {
            if target == loop_ctx.continue_target {
                if loop_ctx.implicit_continue == Some(block_id) {
                    return Ok(());
                }
                ctx.docs.push(YulDoc::line("continue"));
                return Ok(());
            }
            if target == loop_ctx.break_target {
                ctx.docs.push(YulDoc::line("break"));
                return Ok(());
            }
        }

        if let Some(loop_info) = self.loop_info(target) {
            let mut loop_state = ctx.cloned_state();
            let (loop_doc, exit_block) = self.emit_loop(target, loop_info, &mut loop_state)?;
            ctx.docs.push(loop_doc);
            let after_docs = self.emit_block_with_ctx(exit_block, ctx.loop_ctx, ctx.state)?;
            ctx.docs.extend(after_docs);
            return Ok(());
        }
        let next_docs = self.emit_block_with_ctx(target, ctx.loop_ctx, ctx.state)?;
        ctx.docs.extend(next_docs);
        Ok(())
    }

    /// Retrieves the merge block recorded during match lowering.
    ///
    /// Returns `Some(block)` when any arm continues execution, `None` when all arms
    /// terminate, and errors if lowering failed to supply consistent metadata.
    fn match_merge_block(
        &self,
        match_info: &MatchLoweringInfo,
    ) -> Result<Option<BasicBlockId>, YulError> {
        if let Some(merge) = match_info.merge_block {
            return Ok(Some(merge));
        }
        if match_info.arms.iter().all(|arm| arm.terminates) {
            return Ok(None);
        }
        Err(YulError::Unsupported(
            "match lowering missing merge block".into(),
        ))
    }

    /// Looks up metadata about the loop that starts at `header`, if it exists.
    /// Fetches MIR loop metadata for the requested header block.
    ///
    /// * `header` - Loop header to query.
    ///
    /// Returns the associated [`LoopInfo`] when the MIR builder recorded one.
    fn loop_info(&self, header: BasicBlockId) -> Option<LoopInfo> {
        self.mir_func.body.loop_headers.get(&header).copied()
    }

    /// Emits a Yul `for` loop for the given header block and returns the exit block.
    ///
    /// * `header` - Loop header block chosen by MIR.
    /// * `info` - Loop metadata describing body/backedge/exit blocks.
    /// * `state` - Mutable binding state used while rendering body and exit.
    ///
    /// Returns the loop doc plus the block ID that execution continues at after the loop exits.
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
}

/// Translates MIR switch literal kinds into their Yul literal strings.
///
/// * `value` - Switch value representation.
///
/// Returns the string literal used inside the `switch`.
fn switch_value_literal(value: &SwitchValue) -> String {
    match value {
        SwitchValue::Bool(true) => "1".into(),
        SwitchValue::Bool(false) => "0".into(),
        SwitchValue::Int(int) => int.to_string(),
        SwitchValue::Enum(val) => val.to_string(),
    }
}
