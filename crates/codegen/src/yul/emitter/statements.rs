//! Helpers for lowering linear MIR statements into Yul docs.
//!
//! The functions defined in this module operate within `FunctionEmitter` and walk
//! straight-line MIR instructions (non-terminators) to produce Yul statements.

use hir::hir_def::{ExprId, PatId, expr::ArithBinOp};
use mir::ir::{IntrinsicOp, IntrinsicValue};
use mir::{self, ValueId, ValueOrigin};

use crate::yul::{doc::YulDoc, state::BlockState};

use super::{YulError, function::FunctionEmitter};

impl<'db> FunctionEmitter<'db> {
    /// Lowers a linear sequence of MIR instructions into Yul docs.
    ///
    /// * `insts` - MIR instructions belonging to the current block.
    /// * `state` - Mutable binding table shared across the block.
    ///
    /// Returns all emitted Yul statements prior to the block terminator.
    pub(super) fn render_statements(
        &mut self,
        insts: &[mir::MirInst<'_>],
        state: &mut BlockState,
    ) -> Result<Vec<YulDoc>, YulError> {
        let mut docs = Vec::new();
        for inst in insts {
            self.emit_inst(&mut docs, inst, state)?;
        }
        Ok(docs)
    }

    /// Dispatches an individual MIR instruction to the appropriate lowering helper.
    ///
    /// * `docs` - Accumulator that stores every emitted Yul statement.
    /// * `inst` - Instruction being lowered.
    /// * `state` - Mutable per-block binding state shared across helpers.
    ///
    /// Returns `Ok(())` once the instruction has been lowered.
    fn emit_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        inst: &mir::MirInst<'_>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        match inst {
            mir::MirInst::Let { pat, value, .. } => {
                self.emit_let_inst(docs, *pat, *value, state)?
            }
            mir::MirInst::Assign { target, value, .. } => {
                self.emit_assign_inst(docs, *target, *value, state)?
            }
            mir::MirInst::AugAssign {
                target, value, op, ..
            } => self.emit_augassign_inst(docs, *target, *value, *op, state)?,
            mir::MirInst::Eval { value, .. } => self.emit_eval_inst(docs, *value, state)?,
            mir::MirInst::EvalExpr {
                expr,
                value,
                bind_value,
            } => self.emit_eval_expr_inst(docs, *expr, *value, *bind_value, state)?,
            mir::MirInst::IntrinsicStmt { op, args, .. } => {
                self.emit_intrinsic_inst(docs, *op, args, state)?
            }
        }
        Ok(())
    }

    /// Lowers a `let` instruction, allocating or updating the binding.
    ///
    /// * `docs` - Output vector receiving the generated Yul statement.
    /// * `pat` - Pattern representing the binding name.
    /// * `value` - Optional initializer value.
    /// * `state` - Binding table used to store/reuse the lowered slot.
    ///
    /// Returns `Ok(())` when the binding has been populated.
    fn emit_let_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        pat: PatId,
        value: Option<ValueId>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let binding = self.pattern_ident(pat)?;
        let existing = state.binding(&binding);
        let value = match value {
            Some(val) => self.lower_value(val, state)?,
            None => "0".into(),
        };
        if let Some(name) = existing {
            docs.push(YulDoc::line(format!("{name} := {value}")));
        } else {
            let temp = state.alloc_local();
            state.insert_binding(binding.clone(), temp.clone());
            docs.push(YulDoc::line(format!("let {temp} := {value}")));
        }
        Ok(())
    }

    /// Lowers a plain assignment (`x = y`) into Yul.
    ///
    /// * `docs` - Collection where the resulting assignment is appended.
    /// * `target` - Assignment LHS expression.
    /// * `value` - MIR value providing the RHS.
    /// * `state` - Block state storing active Yul bindings.
    ///
    /// Returns `Ok(())` once the assignment has been recorded.
    fn emit_assign_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        target: ExprId,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let binding = self.path_from_expr(target)?;
        let yul_name = state
            .binding(&binding)
            .ok_or_else(|| YulError::Unsupported("assignment to unknown binding".into()))?;
        let value_expr = self.lower_value(value, state)?;
        docs.push(YulDoc::line(format!("{yul_name} := {value_expr}")));
        Ok(())
    }

    /// Emits an augmented assignment (`+=`, `-=`, …) into the corresponding Yul op.
    ///
    /// * `docs` - Collector for the generated statement.
    /// * `target` - Assignment LHS expression.
    /// * `value` - RHS operand.
    /// * `op` - Arithmetic operator used in the augmentation.
    /// * `state` - Block state providing access to binding slots.
    ///
    /// Returns `Ok(())` when the augmented assignment has been emitted.
    fn emit_augassign_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        target: ExprId,
        value: ValueId,
        op: ArithBinOp,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let binding = self.path_from_expr(target)?;
        let yul_name = state
            .binding(&binding)
            .ok_or_else(|| YulError::Unsupported("assignment to unknown binding".into()))?;
        let rhs = self.lower_value(value, state)?;
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
        Ok(())
    }

    /// Emits an expression statement whose value is not reused.
    ///
    /// * `docs` - Accumulator for any generated docs.
    /// * `value` - MIR value used for the expression statement.
    /// * `state` - Block state containing active bindings.
    ///
    /// Refrains from re-emitting expressions consumed elsewhere and returns
    /// `Ok(())` after optionally pushing a doc.
    fn emit_eval_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if self.value_use_counts[value.index()] == 1
            && let Some(doc) = self.render_eval(value, state)?
        {
            docs.push(doc);
        }
        Ok(())
    }

    /// Emits evaluation logic for expressions that optionally bind to a temporary.
    ///
    /// * `docs` - Output buffer for the emitted stmt or binding.
    /// * `expr` - Source expression ID.
    /// * `value` - MIR value produced for that expression.
    /// * `bind_value` - Whether the result should be bound for reuse.
    /// * `state` - Mutable per-block scope that owns temporaries.
    ///
    /// Returns `Ok(())` when the expression has been handled.
    fn emit_eval_expr_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        expr: ExprId,
        value: ValueId,
        bind_value: bool,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let lowered = self.lower_value(value, state)?;
        if bind_value {
            let temp = state.alloc_local();
            self.expr_temps.insert(expr, temp.clone());
            docs.push(YulDoc::line(format!("let {temp} := {lowered}")));
        } else {
            let value_data = self.mir_func.body.value(value);
            let is_call = matches!(value_data.origin, ValueOrigin::Call(..));
            let is_zero_sized = (value_data.ty.is_tuple(self.db)
                && value_data.ty.field_count(self.db) == 0)
                || value_data.ty.is_never(self.db);
            if is_call && !is_zero_sized {
                docs.push(YulDoc::line(format!("pop({lowered})")));
            } else {
                docs.push(YulDoc::line(lowered));
            }
        }
        Ok(())
    }

    /// Emits Yul for a statement-only intrinsic (e.g. `mstore`).
    ///
    /// * `docs` - Collection to append the statement to when one is emitted.
    /// * `op` - Intrinsic opcode.
    /// * `args` - MIR value arguments.
    /// * `state` - Block-local bindings used to lower the arguments.
    ///
    /// Returns `Ok(())` once the intrinsic (if applicable) has been appended.
    fn emit_intrinsic_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        op: IntrinsicOp,
        args: &[ValueId],
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let intr = IntrinsicValue {
            op,
            args: args.to_vec(),
            code_region: None,
        };
        if let Some(doc) = self.lower_intrinsic_stmt(&intr, state)? {
            docs.push(doc);
        }
        Ok(())
    }

    /// Emits statements for expression statements, returning a doc when work was done.
    ///
    /// * `value_id` - MIR value representing the expression.
    /// * `state` - Block state containing active bindings.
    ///
    /// Returns a doc describing the evaluation side effects, if any.
    pub(super) fn render_eval(
        &mut self,
        value_id: ValueId,
        state: &mut BlockState,
    ) -> Result<Option<YulDoc>, YulError> {
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Intrinsic(intr) => self.lower_intrinsic_stmt(intr, state),
            ValueOrigin::Call(call) => {
                let call_expr = self.lower_call_value(call, state)?;
                let is_zero_sized = (value.ty.is_tuple(self.db)
                    && value.ty.field_count(self.db) == 0)
                    || value.ty.is_never(self.db);
                let line = if is_zero_sized {
                    call_expr
                } else {
                    format!("pop({call_expr})")
                };
                Ok(Some(YulDoc::line(line)))
            }
            _ => Ok(None),
        }
    }

    /// Handles `return intrinsic::<op>(...)` for void intrinsics by emitting the
    /// side effect plus a `ret := 0`.
    ///
    /// * `value_id` - MIR value representing the intrinsic call.
    /// * `docs` - Yul statement accumulator.
    /// * `state` - Immutable view over block bindings to resolve arguments.
    ///
    /// Returns `Ok(true)` when the intrinsic produced a replacement return statement.
    pub(super) fn emit_intrinsic_return(
        &mut self,
        value_id: ValueId,
        docs: &mut Vec<YulDoc>,
        state: &BlockState,
        bind_ret: bool,
    ) -> Result<bool, YulError> {
        let value = self.mir_func.body.value(value_id);
        if let ValueOrigin::Intrinsic(intr) = &value.origin
            && !intr.op.returns_value()
        {
            if let Some(doc) = self.lower_intrinsic_stmt(intr, state)? {
                docs.push(doc);
            }
            if matches!(intr.op, IntrinsicOp::ReturnData) {
                return Ok(true);
            }
            if bind_ret {
                docs.push(YulDoc::line("ret := 0"));
            }
            return Ok(true);
        }
        Ok(false)
    }

    /// Converts intrinsic value-producing operations (`mload`/`sload`) into Yul.
    ///
    /// * `intr` - Intrinsic call metadata containing opcode and arguments.
    /// * `state` - Read-only block state needed to lower arguments.
    ///
    /// Returns the Yul expression describing the intrinsic invocation.
    pub(super) fn lower_intrinsic_value(
        &self,
        intr: &IntrinsicValue<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if !intr.op.returns_value() {
            return Err(YulError::Unsupported(
                "intrinsic does not yield a value".into(),
            ));
        }
        if matches!(
            intr.op,
            IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
        ) {
            return self.lower_code_region_query(intr);
        }
        let args = self.lower_intrinsic_args(intr, state)?;
        self.expect_intrinsic_arity(intr.op, &args, 1)?;
        Ok(format!("{}({})", self.intrinsic_name(intr.op), args[0]))
    }

    /// Lowers `code_region_offset/len` into `dataoffset/datasize`.
    fn lower_code_region_query(&self, intr: &IntrinsicValue<'db>) -> Result<String, YulError> {
        let target = intr.code_region.as_ref().ok_or_else(|| {
            YulError::Unsupported("code region intrinsic missing target metadata".into())
        })?;
        let symbol = target.symbol.as_deref().ok_or_else(|| {
            YulError::Unsupported("code region intrinsic has no resolved symbol".into())
        })?;
        let label = self.code_regions.get(symbol).ok_or_else(|| {
            YulError::Unsupported(format!("no code region available for `{symbol}`"))
        })?;
        let op = match intr.op {
            IntrinsicOp::CodeRegionOffset => "dataoffset",
            IntrinsicOp::CodeRegionLen => "datasize",
            _ => unreachable!(),
        };
        Ok(format!("{op}(\"{label}\")"))
    }

    /// Converts intrinsic statement operations (`mstore`, …) into Yul.
    ///
    /// * `intr` - Intrinsic call metadata describing the opcode and args.
    /// * `state` - Block state needed to lower the intrinsic operands.
    ///
    /// Returns the emitted doc when the intrinsic performs work.
    pub(super) fn lower_intrinsic_stmt(
        &self,
        intr: &IntrinsicValue<'db>,
        state: &BlockState,
    ) -> Result<Option<YulDoc>, YulError> {
        if intr.op.returns_value() {
            return Ok(None);
        }
        let args = self.lower_intrinsic_args(intr, state)?;
        let expected = match intr.op {
            IntrinsicOp::Codecopy => 3,
            IntrinsicOp::Mstore | IntrinsicOp::Mstore8 | IntrinsicOp::Sstore => 2,
            IntrinsicOp::ReturnData => 2,
            _ => unreachable!(),
        };
        self.expect_intrinsic_arity(intr.op, &args, expected)?;
        let line = match intr.op {
            IntrinsicOp::Mstore => format!("mstore({}, {})", args[0], args[1]),
            IntrinsicOp::Mstore8 => format!("mstore8({}, {})", args[0], args[1]),
            IntrinsicOp::Sstore => format!("sstore({}, {})", args[0], args[1]),
            IntrinsicOp::ReturnData => format!("return({}, {})", args[0], args[1]),
            IntrinsicOp::Codecopy => format!("codecopy({}, {}, {})", args[0], args[1], args[2]),
            _ => unreachable!(),
        };
        Ok(Some(YulDoc::line(line)))
    }

    /// Lowers all intrinsic arguments into Yul expressions.
    ///
    /// * `intr` - Intrinsic call describing the operands.
    /// * `state` - Block state used to lower each operand.
    ///
    /// Returns the lowered argument list in call order.
    fn lower_intrinsic_args(
        &self,
        intr: &IntrinsicValue<'db>,
        state: &BlockState,
    ) -> Result<Vec<String>, YulError> {
        intr.args
            .iter()
            .map(|arg| self.lower_value(*arg, state))
            .collect()
    }

    /// Ensures the intrinsic received the expected number of operands.
    ///
    /// * `op` - Intrinsic opcode being validated.
    /// * `args` - Lowered operand list.
    /// * `expected` - Required arity for the opcode.
    ///
    /// Returns `Ok(())` when the arity matches, otherwise an unsupported error.
    fn expect_intrinsic_arity(
        &self,
        op: IntrinsicOp,
        args: &[String],
        expected: usize,
    ) -> Result<(), YulError> {
        if args.len() == expected {
            Ok(())
        } else {
            Err(YulError::Unsupported(format!(
                "intrinsic `{}` expects {expected} arguments, got {}",
                self.intrinsic_name(op),
                args.len()
            )))
        }
    }

    /// Returns the Yul builtin name for an intrinsic opcode.
    ///
    /// * `op` - Intrinsic opcode to translate.
    ///
    /// Returns the canonical Yul mnemonic corresponding to the opcode.
    fn intrinsic_name(&self, op: IntrinsicOp) -> &'static str {
        match op {
            IntrinsicOp::Mload => "mload",
            IntrinsicOp::Calldataload => "calldataload",
            IntrinsicOp::Mstore => "mstore",
            IntrinsicOp::Mstore8 => "mstore8",
            IntrinsicOp::Sload => "sload",
            IntrinsicOp::Sstore => "sstore",
            IntrinsicOp::ReturnData => "return",
            IntrinsicOp::Codecopy => "codecopy",
            IntrinsicOp::CodeRegionOffset => "code_region_offset",
            IntrinsicOp::CodeRegionLen => "code_region_len",
        }
    }
}
