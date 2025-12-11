//! Helpers for lowering linear MIR statements into Yul docs.
//!
//! The functions defined in this module operate within `FunctionEmitter` and walk
//! straight-line MIR instructions (non-terminators) to produce Yul statements.

use hir::analysis::ty::ty_check::LocalBinding;
use hir::hir_def::{
    Expr, ExprId, Partial, Pat, PatId,
    expr::{ArithBinOp, BinOp},
};
use hir::projection::Projection;
use mir::ir::{IntrinsicOp, IntrinsicValue};
use mir::{self, MirProjectionPath, ValueId, ValueOrigin, layout};

use crate::yul::{doc::YulDoc, state::BlockState};

use super::{YulError, function::FunctionEmitter};

impl<'db> FunctionEmitter<'db> {
    fn try_emit_field_assign(
        &self,
        docs: &mut Vec<YulDoc>,
        target: ExprId,
        value: ValueId,
        state: &BlockState,
    ) -> Result<bool, YulError> {
        match self.expect_expr(target)? {
            Expr::Field(..) | Expr::Bin(_, _, BinOp::Index) => {}
            _ => return Ok(false),
        }
        let Some(value_id) = self.mir_func.body.expr_values.get(&target).copied() else {
            return Err(YulError::Unsupported(
                "missing lowered value for field assignment target".into(),
            ));
        };
        let target_value = self.mir_func.body.value(value_id);
        let ValueOrigin::PlaceLoad(place) = &target_value.origin else {
            return Err(YulError::Unsupported(
                "field assignment target must be a scalar place".into(),
            ));
        };

        let addr = self.lower_place_ref(place, state)?;
        let rhs = self.lower_value(value, state)?;
        let line = match place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({addr}, {rhs})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({addr}, {rhs})"),
        };
        docs.push(YulDoc::line(line));
        Ok(true)
    }

    fn try_emit_field_augassign(
        &self,
        docs: &mut Vec<YulDoc>,
        target: ExprId,
        value: ValueId,
        op: ArithBinOp,
        state: &BlockState,
    ) -> Result<bool, YulError> {
        match self.expect_expr(target)? {
            Expr::Field(..) | Expr::Bin(_, _, BinOp::Index) => {}
            _ => return Ok(false),
        };
        let Some(value_id) = self.mir_func.body.expr_values.get(&target).copied() else {
            return Err(YulError::Unsupported(
                "missing lowered value for field assignment target".into(),
            ));
        };
        let target_value = self.mir_func.body.value(value_id);
        let ValueOrigin::PlaceLoad(place) = &target_value.origin else {
            return Err(YulError::Unsupported(
                "field augmented assignment target must be a scalar place".into(),
            ));
        };

        let addr = self.lower_place_ref(place, state)?;
        let lhs = self.lower_value(value_id, state)?;
        let rhs = self.lower_value(value, state)?;

        let updated = match op {
            ArithBinOp::Add => format!("add({lhs}, {rhs})"),
            ArithBinOp::Sub => format!("sub({lhs}, {rhs})"),
            ArithBinOp::Mul => format!("mul({lhs}, {rhs})"),
            ArithBinOp::Div => format!("div({lhs}, {rhs})"),
            ArithBinOp::Rem => format!("mod({lhs}, {rhs})"),
            ArithBinOp::Pow => format!("exp({lhs}, {rhs})"),
            ArithBinOp::LShift => format!("shl({rhs}, {lhs})"),
            ArithBinOp::RShift => format!("shr({rhs}, {lhs})"),
            ArithBinOp::BitAnd => format!("and({lhs}, {rhs})"),
            ArithBinOp::BitOr => format!("or({lhs}, {rhs})"),
            ArithBinOp::BitXor => format!("xor({lhs}, {rhs})"),
        };

        let line = match place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mstore({addr}, {updated})"),
            mir::ir::AddressSpaceKind::Storage => format!("sstore({addr}, {updated})"),
        };
        docs.push(YulDoc::line(line));
        Ok(true)
    }

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
            mir::MirInst::Store { expr, place, value } => {
                self.emit_store_inst(docs, *expr, place, *value, state)?
            }
            mir::MirInst::SetDiscriminant {
                expr,
                place,
                variant,
            } => self.emit_set_discriminant_inst(docs, *expr, place, *variant, state)?,
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
        pat_id: PatId,
        value: Option<ValueId>,
        state: &mut BlockState,
    ) -> Result<(), YulError> {
        let pat = self.expect_pat(pat_id)?;
        match pat {
            Pat::Path(..) => {
                let binding = self.pattern_ident(pat_id)?;
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
            Pat::Tuple(pats) => {
                let value_id = value.ok_or_else(|| {
                    YulError::Unsupported("tuple pattern bindings require an initializer".into())
                })?;

                let tuple_ty = self.mir_func.body.value(value_id).ty;
                if !tuple_ty.is_tuple(self.db) {
                    return Err(YulError::Unsupported(
                        "tuple patterns require a tuple initializer".into(),
                    ));
                }
                let field_types = tuple_ty.field_types(self.db);
                if field_types.len() != pats.len() {
                    return Err(YulError::Unsupported(format!(
                        "tuple pattern has {} elements but initializer has {} fields",
                        pats.len(),
                        field_types.len()
                    )));
                }

                let base = self.lower_value(value_id, state)?;
                let base_temp = state.alloc_local();
                state.insert_value_temp(value_id.index(), base_temp.clone());
                docs.push(YulDoc::line(format!("let {base_temp} := {base}")));

                for (field_idx, field_pat_id) in pats.iter().enumerate() {
                    let field_pat = self.expect_pat(*field_pat_id)?;
                    let Pat::Path(..) = field_pat else {
                        if matches!(field_pat, Pat::WildCard | Pat::Rest) {
                            continue;
                        }
                        return Err(YulError::Unsupported(
                            "only identifier patterns are supported".into(),
                        ));
                    };
                    let binding = self.pattern_ident(*field_pat_id)?;
                    let field_ty = field_types[field_idx];
                    let is_aggregate = field_ty.field_count(self.db) > 0;
                    let place = mir::ir::Place::new(
                        value_id,
                        MirProjectionPath::from_projection(Projection::Field(field_idx)),
                        mir::ir::AddressSpaceKind::Memory,
                    );
                    let value = if is_aggregate {
                        self.lower_place_ref(&place, state)?
                    } else {
                        self.lower_place_load(&place, field_ty, state)?
                    };

                    if let Some(existing) = state.binding(&binding) {
                        docs.push(YulDoc::line(format!("{existing} := {value}")));
                    } else {
                        let local = state.alloc_local();
                        state.insert_binding(binding.clone(), local.clone());
                        docs.push(YulDoc::line(format!("let {local} := {value}")));
                    }
                }

                Ok(())
            }
            Pat::WildCard | Pat::Rest => {
                if let Some(value) = value {
                    let lowered = self.lower_value(value, state)?;
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
            _ => Err(YulError::Unsupported(
                "only identifier and tuple patterns are supported".into(),
            )),
        }
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
        if self.try_emit_field_assign(docs, target, value, state)? {
            return Ok(());
        }
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
        if self.try_emit_field_augassign(docs, target, value, op, state)? {
            return Ok(());
        }
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
        let value_data = self.mir_func.body.value(value);
        if let ValueOrigin::Alloc { size_bytes, .. } = value_data.origin {
            if !bind_value {
                return Err(YulError::Unsupported(
                    "alloc values must be bound to a temporary".into(),
                ));
            }
            let temp = state.alloc_local();
            self.expr_temps.insert(expr, temp.clone());
            state.insert_value_temp(value.index(), temp.clone());
            self.emit_alloc_value(docs, &temp, size_bytes);
            return Ok(());
        }

        let lowered = self.lower_value(value, state)?;
        if bind_value {
            let temp = state.alloc_local();
            self.expr_temps.insert(expr, temp.clone());
            // Also cache by ValueId in the BlockState so that arguments referencing
            // this value can reuse it (properly scoped to the current branch).
            state.insert_value_temp(value.index(), temp.clone());
            docs.push(YulDoc::line(format!("let {temp} := {lowered}")));
        } else {
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
        _expr: ExprId,
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
        _expr: ExprId,
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
            ValueOrigin::Alloc { address_space, .. } => *address_space,
            ValueOrigin::Expr(expr_id) => self.expr_address_space(*expr_id),
            _ => fallback,
        }
    }

    fn expr_address_space(&self, expr: ExprId) -> mir::ir::AddressSpaceKind {
        let Some(body) = self.mir_func.typed_body.body() else {
            return mir::ir::AddressSpaceKind::Memory;
        };
        let exprs = body.exprs(self.db);
        if let Partial::Present(expr_data) = &exprs[expr] {
            match expr_data {
                Expr::Field(base, _) => {
                    if matches!(
                        self.expr_address_space(*base),
                        mir::ir::AddressSpaceKind::Storage
                    ) {
                        return mir::ir::AddressSpaceKind::Storage;
                    }
                }
                Expr::Bin(base, _, BinOp::Index) => {
                    if matches!(
                        self.expr_address_space(*base),
                        mir::ir::AddressSpaceKind::Storage
                    ) {
                        return mir::ir::AddressSpaceKind::Storage;
                    }
                }
                _ => {}
            }
        }

        let prop = self.mir_func.typed_body.expr_prop(self.db, expr);
        if let Some(binding) = prop.binding {
            self.address_space_for_binding(&binding)
        } else {
            mir::ir::AddressSpaceKind::Memory
        }
    }

    fn address_space_for_binding(&self, binding: &LocalBinding<'db>) -> mir::ir::AddressSpaceKind {
        match binding {
            LocalBinding::EffectParam { idx, .. } => match self
                .mir_func
                .effect_provider_kinds
                .get(*idx)
                .copied()
                .unwrap_or(mir::ir::EffectProviderKind::Storage)
            {
                mir::ir::EffectProviderKind::Memory => mir::ir::AddressSpaceKind::Memory,
                mir::ir::EffectProviderKind::Storage => mir::ir::AddressSpaceKind::Storage,
                mir::ir::EffectProviderKind::Calldata => mir::ir::AddressSpaceKind::Memory,
            },
            LocalBinding::Local { pat, .. } => self
                .mir_func
                .body
                .pat_address_space
                .get(pat)
                .copied()
                .unwrap_or(mir::ir::AddressSpaceKind::Memory),
            LocalBinding::Param { idx, .. } => {
                if *idx == 0 {
                    return self
                        .mir_func
                        .receiver_space
                        .unwrap_or(mir::ir::AddressSpaceKind::Memory);
                }
                mir::ir::AddressSpaceKind::Memory
            }
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
        if matches!(intr.op, IntrinsicOp::AddrOf) {
            let args = self.lower_intrinsic_args(intr, state)?;
            self.expect_intrinsic_arity(intr.op, &args, 1)?;
            return Ok(args
                .into_iter()
                .next()
                .expect("addr_of arity already checked"));
        }
        if matches!(intr.op, IntrinsicOp::StorAt) {
            let args = self.lower_intrinsic_args(intr, state)?;
            self.expect_intrinsic_arity(intr.op, &args, 1)?;
            return Ok(args
                .into_iter()
                .next()
                .expect("stor_at arity already checked"));
        }
        if matches!(
            intr.op,
            IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
        ) {
            return self.lower_code_region_query(intr);
        }
        let args = self.lower_intrinsic_args(intr, state)?;
        let expected = match intr.op {
            IntrinsicOp::Keccak => 2,
            IntrinsicOp::Caller => 0,
            IntrinsicOp::Codesize => 0,
            IntrinsicOp::Calldatasize | IntrinsicOp::Returndatasize => 0,
            _ => 1,
        };
        self.expect_intrinsic_arity(intr.op, &args, expected)?;
        Ok(format!(
            "{}({})",
            self.intrinsic_name(intr.op),
            args.join(", ")
        ))
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
            IntrinsicOp::Calldatacopy | IntrinsicOp::Returndatacopy => 3,
            IntrinsicOp::Mstore | IntrinsicOp::Mstore8 | IntrinsicOp::Sstore => 2,
            IntrinsicOp::ReturnData | IntrinsicOp::Revert => 2,
            IntrinsicOp::Caller => 0,
            _ => unreachable!(),
        };
        self.expect_intrinsic_arity(intr.op, &args, expected)?;
        let line = match intr.op {
            IntrinsicOp::Mstore => format!("mstore({}, {})", args[0], args[1]),
            IntrinsicOp::Mstore8 => format!("mstore8({}, {})", args[0], args[1]),
            IntrinsicOp::Sstore => format!("sstore({}, {})", args[0], args[1]),
            IntrinsicOp::ReturnData => format!("return({}, {})", args[0], args[1]),
            IntrinsicOp::Revert => format!("revert({}, {})", args[0], args[1]),
            IntrinsicOp::Codecopy => format!("codecopy({}, {}, {})", args[0], args[1], args[2]),
            IntrinsicOp::Calldatacopy => {
                format!("calldatacopy({}, {}, {})", args[0], args[1], args[2])
            }
            IntrinsicOp::Returndatacopy => {
                format!("returndatacopy({}, {}, {})", args[0], args[1], args[2])
            }
            IntrinsicOp::Caller => String::from("caller()"),
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
