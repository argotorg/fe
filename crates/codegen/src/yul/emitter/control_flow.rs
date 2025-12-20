//! Helpers for lowering MIR control-flow constructs into Yul blocks.

use mir::{
    BasicBlockId, LoopInfo, Terminator, ValueId,
    ir::{IntrinsicValue, SwitchTarget, SwitchValue},
};
use rustc_hash::{FxHashMap, FxHashSet};

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
pub(super) struct BlockEmitCtx<'state, 'docs, 'stop> {
    pub(super) loop_ctx: Option<LoopEmitCtx>,
    pub(super) state: &'state mut BlockState,
    pub(super) docs: &'docs mut Vec<YulDoc>,
    pub(super) stop_blocks: &'stop [BasicBlockId],
}

impl<'state, 'docs, 'stop> BlockEmitCtx<'state, 'docs, 'stop> {
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
        self.emit_block_internal(block_id, None, state, &[])
    }

    /// Core implementation shared by the various block emitters.
    ///
    /// * `block_id` - Entry block.
    /// * `loop_ctx` - Optional surrounding loop context.
    /// * `state` - Current binding state.
    /// * `stop_blocks` - Blocks to skip.
    ///
    /// Returns the rendered statements produced while traversing the block.
    fn emit_block_internal(
        &mut self,
        block_id: BasicBlockId,
        loop_ctx: Option<LoopEmitCtx>,
        state: &mut BlockState,
        stop_blocks: &[BasicBlockId],
    ) -> Result<Vec<YulDoc>, YulError> {
        if stop_blocks.contains(&block_id) {
            return Ok(Vec::new());
        }
        let block = self
            .mir_func
            .body
            .blocks
            .get(block_id.index())
            .ok_or_else(|| YulError::Unsupported("invalid block".into()))?;

        let mut docs = self.render_statements(&block.insts, state)?;
        {
            let mut ctx = BlockEmitCtx {
                loop_ctx,
                state,
                docs: &mut docs,
                stop_blocks,
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
        terminator: &Terminator<'db>,
        ctx: &mut BlockEmitCtx<'_, '_, '_>,
    ) -> Result<(), YulError> {
        match terminator {
            Terminator::Return(Some(val)) => {
                self.emit_return_with_value(*val, ctx.docs, ctx.state)?;
                ctx.docs.push(YulDoc::line("leave"));
                Ok(())
            }
            Terminator::Return(None) => {
                if self.returns_value() {
                    ctx.docs.push(YulDoc::line("ret := 0"));
                }
                ctx.docs.push(YulDoc::line("leave"));
                Ok(())
            }
            Terminator::TerminatingCall(call) => match call {
                mir::TerminatingCall::Call(call) => {
                    let call_expr = self.lower_call_value(call, ctx.state)?;
                    ctx.docs.push(YulDoc::line(call_expr));
                    Ok(())
                }
                mir::TerminatingCall::Intrinsic { op, args } => {
                    let intr = IntrinsicValue {
                        op: *op,
                        args: args.clone(),
                    };
                    if let Some(doc) = self.lower_intrinsic_stmt(&intr, ctx.state)? {
                        ctx.docs.push(doc);
                        Ok(())
                    } else {
                        Err(YulError::Unsupported(
                            "terminating intrinsic must be statement-like".into(),
                        ))
                    }
                }
            },
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => self.emit_branch_terminator(*cond, *then_bb, *else_bb, ctx),
            Terminator::Switch {
                discr,
                targets,
                default,
            } => self.emit_switch_terminator(block_id, *discr, targets, *default, ctx),
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
        if !self.returns_value() {
            return Ok(());
        }
        let expr = self.lower_value(value_id, state)?;
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
        ctx: &mut BlockEmitCtx<'_, '_, '_>,
    ) -> Result<(), YulError> {
        let cond_expr = self.lower_value(cond, ctx.state)?;
        let cond_temp = ctx.state.alloc_local();
        ctx.docs
            .push(YulDoc::line(format!("let {cond_temp} := {cond_expr}")));
        let loop_ctx = ctx.loop_ctx;
        let then_term = &self
            .mir_func
            .body
            .blocks
            .get(then_bb.index())
            .ok_or_else(|| YulError::Unsupported("invalid then block".into()))?
            .terminator;
        let else_term = &self
            .mir_func
            .body
            .blocks
            .get(else_bb.index())
            .ok_or_else(|| YulError::Unsupported("invalid else block".into()))?
            .terminator;

        let then_target = match then_term {
            Terminator::Goto { target } => Some(*target),
            _ => None,
        };
        let else_target = match else_term {
            Terminator::Goto { target } => Some(*target),
            _ => None,
        };

        // Common patterns:
        // - if-without-else: then -> goto else_bb, else_bb is the join.
        // - if/else: then -> goto join, else -> goto join.
        let join = if then_target == Some(else_bb) {
            Some(else_bb)
        } else if else_target == Some(then_bb) {
            Some(then_bb)
        } else if then_target.is_some_and(|then| else_target == Some(then)) {
            then_target
        } else {
            None
        };
        let emit_false_branch = !(join == Some(else_bb) && then_target == Some(else_bb));

        let then_exits =
            join != Some(then_bb) && matches!(then_term, Terminator::TerminatingCall(_));
        let else_exits =
            join != Some(else_bb) && matches!(else_term, Terminator::TerminatingCall(_));

        let mut then_state = ctx.cloned_state();
        let mut else_state = ctx.cloned_state();
        let mut branch_stops = ctx.stop_blocks.to_vec();
        if let Some(join) = join
            && !branch_stops.contains(&join)
        {
            branch_stops.push(join);
        }
        if emit_false_branch && (then_exits || else_exits) {
            if then_exits {
                let then_docs =
                    self.emit_block_internal(then_bb, loop_ctx, &mut then_state, &branch_stops)?;
                ctx.docs
                    .push(YulDoc::block(format!("if {cond_temp} "), then_docs));

                let fallthrough_docs =
                    self.emit_block_internal(else_bb, loop_ctx, ctx.state, ctx.stop_blocks)?;
                ctx.docs.extend(fallthrough_docs);
                return Ok(());
            }

            let else_docs =
                self.emit_block_internal(else_bb, loop_ctx, &mut else_state, &branch_stops)?;
            ctx.docs
                .push(YulDoc::block(format!("if iszero({cond_temp}) "), else_docs));

            let fallthrough_docs =
                self.emit_block_internal(then_bb, loop_ctx, ctx.state, ctx.stop_blocks)?;
            ctx.docs.extend(fallthrough_docs);
            return Ok(());
        }

        let then_docs =
            self.emit_block_internal(then_bb, loop_ctx, &mut then_state, &branch_stops)?;
        ctx.docs
            .push(YulDoc::block(format!("if {cond_temp} "), then_docs));
        if emit_false_branch {
            let else_docs =
                self.emit_block_internal(else_bb, loop_ctx, &mut else_state, &branch_stops)?;
            ctx.docs
                .push(YulDoc::block(format!("if iszero({cond_temp}) "), else_docs));
        }

        if let Some(join) = join
            && !ctx.stop_blocks.contains(&join)
        {
            let join_docs = self.emit_block_internal(join, loop_ctx, ctx.state, ctx.stop_blocks)?;
            ctx.docs.extend(join_docs);
        }
        Ok(())
    }

    /// Emits a `switch` terminator.
    ///
    /// * `discr` - MIR value containing the discriminant expression.
    /// * `targets` - All concrete switch targets.
    /// * `default` - Default target block.
    /// * `ctx` - Shared block context reused across successor emission.
    fn emit_switch_terminator(
        &mut self,
        block_id: BasicBlockId,
        discr: ValueId,
        targets: &[SwitchTarget],
        default: BasicBlockId,
        ctx: &mut BlockEmitCtx<'_, '_, '_>,
    ) -> Result<(), YulError> {
        let discr_expr = self.lower_value(discr, ctx.state)?;
        let join = self.switch_join_candidate(block_id, ctx.stop_blocks);
        let mut switch_stops = ctx.stop_blocks.to_vec();
        if let Some(join) = join
            && !switch_stops.contains(&join)
        {
            switch_stops.push(join);
        }

        ctx.docs.push(YulDoc::line(format!("switch {discr_expr}")));
        let loop_ctx = ctx.loop_ctx;

        for target in targets {
            let mut case_state = ctx.cloned_state();
            let literal = switch_value_literal(&target.value);
            let case_docs =
                self.emit_block_internal(target.block, loop_ctx, &mut case_state, &switch_stops)?;
            ctx.docs
                .push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
        }

        let mut default_state = ctx.cloned_state();
        let default_docs =
            self.emit_block_internal(default, loop_ctx, &mut default_state, &switch_stops)?;
        ctx.docs
            .push(YulDoc::wide_block("  default ", default_docs));

        if let Some(join) = join
            && !ctx.stop_blocks.contains(&join)
        {
            let join_docs = self.emit_block_internal(join, loop_ctx, ctx.state, ctx.stop_blocks)?;
            ctx.docs.extend(join_docs);
        }
        Ok(())
    }

    fn switch_join_candidate(
        &self,
        root: BasicBlockId,
        stop_blocks: &[BasicBlockId],
    ) -> Option<BasicBlockId> {
        if let Some(join) = self.ipdom(root)
            && !stop_blocks.contains(&join)
        {
            return Some(join);
        }

        let mut visited: FxHashSet<BasicBlockId> = FxHashSet::default();
        let mut stack = vec![root];
        let mut goto_targets: FxHashMap<BasicBlockId, usize> = FxHashMap::default();

        while let Some(block_id) = stack.pop() {
            if stop_blocks.contains(&block_id) || !visited.insert(block_id) {
                continue;
            }

            let Some(block) = self.mir_func.body.blocks.get(block_id.index()) else {
                continue;
            };

            match &block.terminator {
                Terminator::Goto { target } => {
                    if stop_blocks.contains(target)
                        || self.mir_func.body.loop_headers.contains_key(target)
                    {
                        continue;
                    }
                    *goto_targets.entry(*target).or_default() += 1;
                }
                Terminator::Branch {
                    then_bb, else_bb, ..
                } => {
                    stack.push(*then_bb);
                    stack.push(*else_bb);
                }
                Terminator::Switch {
                    targets, default, ..
                } => {
                    stack.extend(targets.iter().map(|target| target.block));
                    stack.push(*default);
                }
                Terminator::Return(..)
                | Terminator::TerminatingCall(_)
                | Terminator::Unreachable => {}
            }
        }

        if goto_targets.len() == 1 {
            return goto_targets.keys().next().copied();
        }

        goto_targets
            .into_iter()
            .max_by_key(|(_, count)| *count)
            .and_then(|(block, count)| (count >= 2).then_some(block))
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
        ctx: &mut BlockEmitCtx<'_, '_, '_>,
    ) -> Result<(), YulError> {
        if ctx.stop_blocks.contains(&target) {
            return Ok(());
        }
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
            let (loop_doc, exit_block) =
                self.emit_loop(target, loop_info, &mut loop_state, ctx.stop_blocks)?;
            ctx.docs.push(loop_doc);
            let after_docs =
                self.emit_block_internal(exit_block, ctx.loop_ctx, ctx.state, ctx.stop_blocks)?;
            ctx.docs.extend(after_docs);
            return Ok(());
        }
        let next_docs =
            self.emit_block_internal(target, ctx.loop_ctx, ctx.state, ctx.stop_blocks)?;
        ctx.docs.extend(next_docs);
        Ok(())
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
        stop_blocks: &[BasicBlockId],
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
        let body_docs = self.emit_block_internal(info.body, Some(loop_ctx), state, stop_blocks)?;
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
