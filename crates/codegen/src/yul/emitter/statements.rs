//! Helpers for lowering linear MIR statements into Yul docs.
//!
//! The functions defined in this module operate within `FunctionEmitter` and walk
//! straight-line MIR instructions (non-terminators) to produce Yul statements.

use hir::hir_def::expr::ArithBinOp;
use hir::projection::Projection;
use mir::ir::{IntrinsicOp, IntrinsicValue};
use mir::{self, LocalId, MirProjectionPath, ValueId, ValueOrigin, layout};

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
        insts: &[mir::MirInst<'db>],
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
        inst: &mir::MirInst<'db>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        match inst {
            mir::MirInst::LocalInit { local, value, .. } => {
                self.emit_local_init_inst(docs, *local, *value, state)?
            }
            mir::MirInst::LocalSet { local, value, .. } => {
                self.emit_local_set_inst(docs, *local, *value, state)?
            }
            mir::MirInst::LocalAugAssign {
                local, value, op, ..
            } => self.emit_local_augassign_inst(docs, *local, *value, *op, state)?,
            mir::MirInst::Eval { value, .. } => self.emit_eval_inst(docs, *value, state)?,
            mir::MirInst::EvalValue { value } => self.emit_eval_inst(docs, *value, state)?,
            mir::MirInst::BindValue { value } => self.emit_bind_value_inst(docs, *value, state)?,
            mir::MirInst::IntrinsicStmt { op, args } => {
                self.emit_intrinsic_inst(docs, *op, args, state)?
            }
            mir::MirInst::Store { place, value } => {
                self.emit_store_inst(docs, place, *value, state)?
            }
            mir::MirInst::InitAggregate { place, inits } => {
                self.emit_init_aggregate_inst(docs, place, inits, state)?
            }
            mir::MirInst::SetDiscriminant { place, variant } => {
                self.emit_set_discriminant_inst(docs, place, *variant, state)?
            }
        }
        Ok(())
    }

    fn emit_local_init_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        local: LocalId,
        value: Option<ValueId>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if let Some(value_id) = value {
            let value_ty = self.mir_func.body.value(value_id).ty;
            if (value_ty.is_tuple(self.db) && value_ty.field_count(self.db) == 0)
                || value_ty.is_never(self.db)
            {
                if let Some(doc) = self.render_eval(value_id, state)? {
                    docs.push(doc);
                }
                state.insert_local(local, "0".into());
                return Ok(());
            }

            let rhs = self.lower_value(value_id, state)?;
            if let Some(existing) = state.local(local) {
                docs.push(YulDoc::line(format!("{existing} := {rhs}")));
                return Ok(());
            }
            let temp = state.alloc_local();
            state.insert_local(local, temp.clone());
            docs.push(YulDoc::line(format!("let {temp} := {rhs}")));
            return Ok(());
        }

        if let Some(existing) = state.local(local) {
            docs.push(YulDoc::line(format!("{existing} := 0")));
            return Ok(());
        }
        let temp = state.alloc_local();
        state.insert_local(local, temp.clone());
        docs.push(YulDoc::line(format!("let {temp} := 0")));
        Ok(())
    }

    fn emit_local_set_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        local: LocalId,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if !self.mir_func.body.local(local).is_mut {
            return Err(YulError::Unsupported(
                "assignment to immutable local".into(),
            ));
        }
        let Some(yul_name) = state.local(local) else {
            return Err(YulError::Unsupported("assignment to unknown local".into()));
        };
        let rhs = self.lower_value(value, state)?;
        docs.push(YulDoc::line(format!("{yul_name} := {rhs}")));
        Ok(())
    }

    fn emit_local_augassign_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        local: LocalId,
        value: ValueId,
        op: ArithBinOp,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if !self.mir_func.body.local(local).is_mut {
            return Err(YulError::Unsupported(
                "assignment to immutable local".into(),
            ));
        }
        let Some(yul_name) = state.local(local) else {
            return Err(YulError::Unsupported("assignment to unknown local".into()));
        };
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
    ///
    /// However, unit-returning calls are always emitted here because the return
    /// terminator won't emit them (it skips unit values).
    fn emit_eval_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let value_data = self.mir_func.body.value(value);
        let is_unit_call = matches!(&value_data.origin, ValueOrigin::Call(..))
            && value_data.ty.is_tuple(self.db)
            && value_data.ty.field_count(self.db) == 0;

        // Unit-returning calls must always be emitted here since the return
        // terminator won't render them.
        if (self.value_use_counts[value.index()] == 1 || is_unit_call)
            && let Some(doc) = self.render_eval(value, state)?
        {
            docs.push(doc);
        }
        Ok(())
    }

    fn emit_bind_value_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if state.value_temp(value.index()).is_some() {
            return Ok(());
        }

        let value_data = self.mir_func.body.value(value);
        if (value_data.ty.is_tuple(self.db) && value_data.ty.field_count(self.db) == 0)
            || value_data.ty.is_never(self.db)
        {
            if let Some(doc) = self.render_eval(value, state)? {
                docs.push(doc);
            }
            return Ok(());
        }

        let temp = state.alloc_local();
        match &value_data.origin {
            ValueOrigin::Alloc { .. } => {
                let size_bytes =
                    layout::ty_size_bytes(self.db, value_data.ty).ok_or_else(|| {
                        YulError::Unsupported("alloc requires a statically sized type".into())
                    })?;
                state.insert_value_temp(value.index(), temp.clone());
                self.emit_alloc_value(docs, &temp, size_bytes);
                Ok(())
            }
            _ => {
                let lowered = self.lower_value(value, state)?;
                state.insert_value_temp(value.index(), temp.clone());
                docs.push(YulDoc::line(format!("let {temp} := {lowered}")));
                Ok(())
            }
        }
    }

    fn emit_alloc_value(&self, docs: &mut Vec<YulDoc>, temp: &str, size_bytes: usize) {
        let size = size_bytes.to_string();
        docs.push(YulDoc::line(format!("let {temp} := mload(0x40)")));
        docs.push(YulDoc::block(
            format!("if iszero({temp}) "),
            vec![YulDoc::line(format!("{temp} := 0x80"))],
        ));
        docs.push(YulDoc::line(format!("mstore(0x40, add({temp}, {size}))")));
    }

    fn emit_store_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        place: &mir::ir::Place<'db>,
        value: ValueId,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let value_data = self.mir_func.body.value(value);
        let value_ty = value_data.ty;
        if self.is_aggregate_ty(value_ty) {
            if state.value_temp(value.index()).is_none() {
                let rhs = self.lower_value(value, state)?;
                let temp = state.alloc_local();
                state.insert_value_temp(value.index(), temp.clone());
                docs.push(YulDoc::line(format!("let {temp} := {rhs}")));
            }
            let src_space = self.source_address_space(value, mir::ir::AddressSpaceKind::Memory);
            let src_place = mir::ir::Place::new(value, MirProjectionPath::new(), src_space);
            return self.emit_store_from_places(docs, place, &src_place, value_ty, state);
        }

        let addr = self.lower_place_ref(place, state)?;
        let rhs = self.lower_value(value, state)?;
        let stored = self.apply_to_word_conversion(&rhs, value_ty);
        let line = match place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({addr}, {stored})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({addr}, {stored})"),
        };
        docs.push(YulDoc::line(line));
        Ok(())
    }

    fn emit_init_aggregate_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        place: &mir::ir::Place<'db>,
        inits: &[(MirProjectionPath<'db>, ValueId)],
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        for (path, value) in inits {
            let mut target = place.clone();
            for proj in path.iter() {
                target = self.extend_place(&target, proj.clone());
            }
            self.emit_store_inst(docs, &target, *value, state)?;
        }
        Ok(())
    }

    fn emit_store_from_places(
        &mut self,
        docs: &mut Vec<YulDoc>,
        dst_place: &mir::ir::Place<'db>,
        src_place: &mir::ir::Place<'db>,
        value_ty: hir::analysis::ty::ty_def::TyId<'db>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        if value_ty.is_array(self.db) {
            let Some(len) = layout::array_len(self.db, value_ty) else {
                return Err(YulError::Unsupported(
                    "array store requires a constant length".into(),
                ));
            };
            let elem_ty = layout::array_elem_ty(self.db, value_ty)
                .ok_or_else(|| YulError::Unsupported("array store requires element type".into()))?;
            for idx in 0..len {
                let dst_elem = self.extend_place(
                    dst_place,
                    Projection::Index(hir::projection::IndexSource::Constant(idx)),
                );
                let src_elem = self.extend_place(
                    src_place,
                    Projection::Index(hir::projection::IndexSource::Constant(idx)),
                );
                self.emit_store_from_places(docs, &dst_elem, &src_elem, elem_ty, state)?;
            }
            return Ok(());
        }

        if value_ty.field_count(self.db) > 0 {
            let field_tys = value_ty.field_types(self.db);
            for (field_idx, field_ty) in field_tys.into_iter().enumerate() {
                let dst_field = self.extend_place(dst_place, Projection::Field(field_idx));
                let src_field = self.extend_place(src_place, Projection::Field(field_idx));
                self.emit_store_from_places(docs, &dst_field, &src_field, field_ty, state)?;
            }
            return Ok(());
        }

        if value_ty
            .adt_ref(self.db)
            .is_some_and(|adt| matches!(adt, hir::analysis::ty::adt_def::AdtRef::Enum(_)))
        {
            return self.emit_enum_store(docs, dst_place, src_place, value_ty, state);
        }

        let addr = self.lower_place_ref(dst_place, state)?;
        let rhs = self.lower_place_load(src_place, value_ty, state)?;
        let stored = self.apply_to_word_conversion(&rhs, value_ty);
        let line = match dst_place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({addr}, {stored})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({addr}, {stored})"),
        };
        docs.push(YulDoc::line(line));
        Ok(())
    }

    fn emit_enum_store(
        &mut self,
        docs: &mut Vec<YulDoc>,
        dst_place: &mir::ir::Place<'db>,
        src_place: &mir::ir::Place<'db>,
        enum_ty: hir::analysis::ty::ty_def::TyId<'db>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let src_addr = self.lower_place_ref(src_place, state)?;
        let dst_addr = self.lower_place_ref(dst_place, state)?;
        let discr = match dst_place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mload({src_addr})"),
            mir::ir::AddressSpaceKind::Storage => format!("sload({src_addr})"),
        };
        let discr_temp = state.alloc_local();
        docs.push(YulDoc::line(format!("let {discr_temp} := {discr}")));
        let store_discr = match dst_place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({dst_addr}, {discr_temp})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({dst_addr}, {discr_temp})"),
        };
        docs.push(YulDoc::line(store_discr));

        let Some(adt_def) = enum_ty.adt_def(self.db) else {
            return Err(YulError::Unsupported("enum store requires enum adt".into()));
        };
        let hir::analysis::ty::adt_def::AdtRef::Enum(enm) = adt_def.adt_ref(self.db) else {
            return Err(YulError::Unsupported("enum store requires enum adt".into()));
        };

        docs.push(YulDoc::line(format!("switch {discr_temp}")));
        let variants = adt_def.fields(self.db);
        for (idx, _) in variants.iter().enumerate() {
            let enum_variant = hir::hir_def::EnumVariant::new(enm, idx);
            let ctor = hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(
                enum_variant,
                enum_ty,
            );
            let field_tys = ctor.field_types(self.db);
            let mut case_docs = Vec::new();
            for (field_idx, field_ty) in field_tys.iter().enumerate() {
                let proj = Projection::VariantField {
                    variant: enum_variant,
                    enum_ty,
                    field_idx,
                };
                let dst_field = self.extend_place(dst_place, proj.clone());
                let src_field = self.extend_place(src_place, proj);
                self.emit_store_from_places(
                    &mut case_docs,
                    &dst_field,
                    &src_field,
                    *field_ty,
                    state,
                )?;
            }
            let literal = idx as u64;
            docs.push(YulDoc::wide_block(format!("  case {literal} "), case_docs));
        }
        docs.push(YulDoc::wide_block("  default ", Vec::new()));
        Ok(())
    }

    fn emit_set_discriminant_inst(
        &mut self,
        docs: &mut Vec<YulDoc>,
        place: &mir::ir::Place<'db>,
        variant: hir::hir_def::EnumVariant<'db>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let addr = self.lower_place_ref(place, state)?;
        let value = variant.idx as u64;
        let line = match place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({addr}, {value})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({addr}, {value})"),
        };
        docs.push(YulDoc::line(line));
        Ok(())
    }

    fn source_address_space(
        &self,
        value: ValueId,
        fallback: mir::ir::AddressSpaceKind,
    ) -> mir::ir::AddressSpaceKind {
        let value_data = self.mir_func.body.value(value);
        match &value_data.origin {
            ValueOrigin::PlaceRef(place) | ValueOrigin::PlaceLoad(place) => place.address_space,
            ValueOrigin::Alloc { address_space } => *address_space,
            _ => fallback,
        }
    }

    fn extend_place(
        &self,
        place: &mir::ir::Place<'db>,
        proj: Projection<
            hir::analysis::ty::ty_def::TyId<'db>,
            hir::hir_def::EnumVariant<'db>,
            ValueId,
        >,
    ) -> mir::ir::Place<'db> {
        let mut path = place.projection.clone();
        path.push(proj);
        mir::ir::Place::new(place.base, path, place.address_space)
    }

    fn is_aggregate_ty(&self, ty: hir::analysis::ty::ty_def::TyId<'db>) -> bool {
        ty.field_count(self.db) > 0
            || ty.is_array(self.db)
            || ty
                .adt_ref(self.db)
                .is_some_and(|adt| matches!(adt, hir::analysis::ty::adt_def::AdtRef::Enum(_)))
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
        intr: &IntrinsicValue,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if !intr.op.returns_value() {
            return Err(YulError::Unsupported(
                "intrinsic does not yield a value".into(),
            ));
        }
        if matches!(intr.op, IntrinsicOp::AddrOf) {
            let args = self.lower_intrinsic_args(intr, state)?;
            debug_assert_eq!(args.len(), 1, "addr_of expects 1 argument");
            return Ok(args.into_iter().next().expect("addr_of expects 1 argument"));
        }
        if matches!(intr.op, IntrinsicOp::StorAt) {
            let args = self.lower_intrinsic_args(intr, state)?;
            debug_assert_eq!(args.len(), 1, "stor_at expects 1 argument");
            return Ok(args.into_iter().next().expect("stor_at expects 1 argument"));
        }
        if matches!(
            intr.op,
            IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
        ) {
            return self.lower_code_region_query(intr);
        }
        let args = self.lower_intrinsic_args(intr, state)?;
        Ok(format!(
            "{}({})",
            self.intrinsic_name(intr.op),
            args.join(", ")
        ))
    }

    /// Lowers `code_region_offset/len` into `dataoffset/datasize`.
    fn lower_code_region_query(&self, intr: &IntrinsicValue) -> Result<String, YulError> {
        debug_assert_eq!(
            intr.args.len(),
            1,
            "code region intrinsic expects 1 argument"
        );
        let arg = intr.args[0];
        let symbol = match &self.mir_func.body.value(arg).origin {
            mir::ValueOrigin::FuncItem(root) => root.symbol.as_deref().ok_or_else(|| {
                YulError::Unsupported(
                    "code region function item is missing a resolved symbol".into(),
                )
            })?,
            _ => {
                return Err(YulError::Unsupported(
                    "code region intrinsic argument must be a function item".into(),
                ));
            }
        };
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
        intr: &IntrinsicValue,
        state: &BlockState,
    ) -> Result<Option<YulDoc>, YulError> {
        if intr.op.returns_value() {
            return Ok(None);
        }
        let args = self.lower_intrinsic_args(intr, state)?;
        let line = match intr.op {
            IntrinsicOp::Mstore => {
                format!("mstore({}, {})", args[0], args[1])
            }
            IntrinsicOp::Mstore8 => {
                format!("mstore8({}, {})", args[0], args[1])
            }
            IntrinsicOp::Sstore => {
                format!("sstore({}, {})", args[0], args[1])
            }
            IntrinsicOp::ReturnData => {
                format!("return({}, {})", args[0], args[1])
            }
            IntrinsicOp::Revert => {
                format!("revert({}, {})", args[0], args[1])
            }
            IntrinsicOp::Codecopy => {
                format!("codecopy({}, {}, {})", args[0], args[1], args[2])
            }
            IntrinsicOp::Calldatacopy => {
                format!("calldatacopy({}, {}, {})", args[0], args[1], args[2])
            }
            IntrinsicOp::Returndatacopy => {
                format!("returndatacopy({}, {}, {})", args[0], args[1], args[2])
            }
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
        intr: &IntrinsicValue,
        state: &BlockState,
    ) -> Result<Vec<String>, YulError> {
        intr.args
            .iter()
            .map(|arg| self.lower_value(*arg, state))
            .collect()
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
            IntrinsicOp::Calldatacopy => "calldatacopy",
            IntrinsicOp::Calldatasize => "calldatasize",
            IntrinsicOp::Returndatacopy => "returndatacopy",
            IntrinsicOp::Returndatasize => "returndatasize",
            IntrinsicOp::AddrOf => "addr_of",
            IntrinsicOp::Mstore => "mstore",
            IntrinsicOp::Mstore8 => "mstore8",
            IntrinsicOp::Sload => "sload",
            IntrinsicOp::Sstore => "sstore",
            IntrinsicOp::ReturnData => "return",
            IntrinsicOp::Revert => "revert",
            IntrinsicOp::Codecopy => "codecopy",
            IntrinsicOp::Codesize => "codesize",
            IntrinsicOp::CodeRegionOffset => "code_region_offset",
            IntrinsicOp::CodeRegionLen => "code_region_len",
            IntrinsicOp::Keccak => "keccak256",
            IntrinsicOp::Caller => "caller",
            IntrinsicOp::StorAt => "stor_at",
        }
    }
}
