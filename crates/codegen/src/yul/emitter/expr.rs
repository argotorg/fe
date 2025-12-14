//! Expression and value lowering helpers shared across the Yul emitter.

use hir::analysis::ty::simplified_pattern::ConstructorKind;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
use hir::hir_def::{
    CallableDef,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use hir::projection::{IndexSource, Projection};
use hir::span::LazySpan;
use mir::{
    CallOrigin, ValueId, ValueOrigin,
    ir::{FieldPtrOrigin, MirFunctionOrigin, Place, SyntheticValue},
    layout,
};

use crate::yul::state::BlockState;

use super::{
    YulError,
    function::FunctionEmitter,
    util::{function_name, is_std_evm_ops, prefix_yul_name},
};

impl<'db> FunctionEmitter<'db> {
    fn format_hir_expr_context(&self, expr: hir::hir_def::ExprId) -> String {
        let Some(body) = (match self.mir_func.origin {
            MirFunctionOrigin::Hir(func) => func.body(self.db),
            MirFunctionOrigin::Synthetic(_) => None,
        }) else {
            return format!(
                "func={} expr={expr:?} (missing HIR body)",
                self.mir_func.symbol_name
            );
        };

        let span = expr.span(body).resolve(self.db);
        let span_context = if let Some(span) = span {
            let path = span
                .file
                .path(self.db)
                .as_ref()
                .map(|p| p.to_string())
                .unwrap_or_else(|| "<unknown file>".into());
            let start: usize = u32::from(span.range.start()) as usize;
            let text = span.file.text(self.db);
            let (mut line, mut col) = (1usize, 1usize);
            for byte in text.as_bytes().iter().take(start) {
                if *byte == b'\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }
            format!("{path}:{line}:{col}")
        } else {
            "<no span>".into()
        };

        let expr_data = match expr.data(self.db, body) {
            hir::hir_def::Partial::Present(expr) => format!("{expr:?}"),
            hir::hir_def::Partial::Absent => "<absent>".into(),
        };

        format!(
            "func={} expr={expr:?} at {}: {}",
            self.mir_func.symbol_name, span_context, expr_data
        )
    }

    /// Lowers a MIR `ValueId` into a Yul expression string.
    ///
    /// * `value_id` - Identifier selecting the MIR value.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns the Yul expression referencing the value or an error if unsupported.
    pub(super) fn lower_value(
        &self,
        value_id: ValueId,
        state: &BlockState,
    ) -> Result<String, YulError> {
        // Check if this value was already bound to a temp in the current scope
        if let Some(temp) = state.value_temp(value_id.index()) {
            return Ok(temp.clone());
        }
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Expr(expr) => unreachable!(
                "unlowered HIR expression reached codegen (MIR lowering should have failed earlier): {}",
                self.format_hir_expr_context(*expr)
            ),
            ValueOrigin::ControlFlowResult { expr } => unreachable!(
                "control-flow result value reached codegen without binding (MIR lowering should have inserted/used a temp): {}",
                self.format_hir_expr_context(*expr)
            ),
            ValueOrigin::Unit => Ok("0".into()),
            ValueOrigin::Unary { op, inner } => {
                let value = self.lower_value(*inner, state)?;
                match op {
                    UnOp::Minus => Ok(format!("sub(0, {value})")),
                    UnOp::Not => Ok(format!("iszero({value})")),
                    UnOp::Plus => Ok(value),
                    UnOp::BitNot => Ok(format!("not({value})")),
                }
            }
            ValueOrigin::Binary { op, lhs, rhs } => {
                let left = self.lower_value(*lhs, state)?;
                let right = self.lower_value(*rhs, state)?;
                match op {
                    BinOp::Arith(op) => match op {
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
                        // Range should be lowered to Range type construction before codegen
                        ArithBinOp::Range => {
                            todo!("Range operator should be handled during type checking/MIR lowering")
                        }
                    },
                    BinOp::Comp(op) => {
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
                        let func = match op {
                            LogicalBinOp::And => "and",
                            LogicalBinOp::Or => "or",
                        };
                        Ok(format!("{func}({left}, {right})"))
                    }
                    BinOp::Index => Err(YulError::Unsupported(
                        "index expressions should be lowered to places before codegen".into(),
                    )),
                }
            }
            ValueOrigin::Local(local) => state
                .resolve_local(*local)
                .ok_or_else(|| {
                    let local_data = self.mir_func.body.local(*local);
                    let is_param = self.mir_func.body.param_locals.contains(local);
                    let is_effect = self.mir_func.body.effect_param_locals.contains(local);
                    YulError::Unsupported(format!(
                        "unbound MIR local reached codegen (func={}, local=l{} `{}`, ty={}, param={is_param}, effect={is_effect})",
                        self.mir_func.symbol_name,
                        local.index(),
                        local_data.name,
                        local_data.ty.pretty_print(self.db),
                    ))
                }),
            ValueOrigin::FuncItem(_) => {
                debug_assert!(
                    layout::is_zero_sized_ty_in(self.db, &self.layout, value.ty),
                    "function item values should be zero-sized (ty={})",
                    value.ty.pretty_print(self.db)
                );
                Ok("0".into())
            }
            ValueOrigin::Synthetic(synth) => self.lower_synthetic_value(synth),
            ValueOrigin::FieldPtr(field_ptr) => self.lower_field_ptr(field_ptr, state),
            ValueOrigin::PlaceRef(place) => self.lower_place_ref(place, state),
            ValueOrigin::TransparentCast { value } => self.lower_value(*value, state),
        }
    }

    /// Lowers a MIR call into a Yul function invocation.
    ///
    /// * `call` - Call origin describing the callee and arguments.
    /// * `state` - Binding state used to lower argument expressions.
    ///
    /// Returns the Yul invocation string for the call.
    pub(super) fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if call
            .hir_target
            .as_ref()
            .and_then(|target| target.callable_def.name(self.db))
            .is_some_and(|name| name.data(self.db) == "contract_field_slot")
        {
            return Err(YulError::Unsupported(
                "`contract_field_slot` must be constant-folded before codegen".into(),
            ));
        }

        let is_evm_op = match call.hir_target.as_ref() {
            Some(target) => {
                matches!(
                    target.callable_def,
                    CallableDef::Func(func) if is_std_evm_ops(self.db, func)
                )
            }
            None => false,
        };
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            let Some(target) = call.hir_target.as_ref() else {
                return Err(YulError::Unsupported(
                    "call is missing a resolved symbol name".into(),
                ));
            };
            match target.callable_def {
                CallableDef::Func(func) => function_name(self.db, func),
                CallableDef::VariantCtor(_) => {
                    return Err(YulError::Unsupported(
                        "callable without hir function definition is not supported yet".into(),
                    ));
                }
            }
        };
        let callee = if is_evm_op {
            callee
        } else {
            prefix_yul_name(&callee)
        };
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        for &arg in &call.effect_args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        if lowered_args.is_empty() {
            Ok(format!("{callee}()"))
        } else {
            Ok(format!("{callee}({})", lowered_args.join(", ")))
        }
    }

    /// Lowers special MIR synthetic values such as constants into Yul expressions.
    ///
    /// * `value` - Synthetic value emitted during MIR construction.
    ///
    /// Returns the literal Yul expression for the synthetic value.
    fn lower_synthetic_value(&self, value: &SyntheticValue) -> Result<String, YulError> {
        match value {
            SyntheticValue::Int(int) => Ok(int.to_string()),
            SyntheticValue::Bool(flag) => Ok(if *flag { "1" } else { "0" }.into()),
            SyntheticValue::Bytes(bytes) => Ok(format!("0x{}", hex::encode(bytes))),
        }
    }

    /// Lowers a FieldPtr (pointer arithmetic for nested struct access) into a Yul add expression.
    ///
    /// * `field_ptr` - The FieldPtrOrigin containing base pointer and offset.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns a Yul expression representing `base + offset`.
    fn lower_field_ptr(
        &self,
        field_ptr: &FieldPtrOrigin,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let base = self.lower_value(field_ptr.base, state)?;
        if field_ptr.offset_bytes == 0 {
            Ok(base)
        } else {
            let offset = match field_ptr.addr_space {
                mir::ir::AddressSpaceKind::Memory | mir::ir::AddressSpaceKind::Calldata => {
                    field_ptr.offset_bytes
                }
                mir::ir::AddressSpaceKind::Storage
                | mir::ir::AddressSpaceKind::TransientStorage => field_ptr.offset_bytes / 32,
            };
            Ok(format!("add({}, {})", base, offset))
        }
    }

    /// Lowers a PlaceLoad (load value from a place with projection path).
    ///
    /// Walks the projection path to compute the byte offset from the base,
    /// then emits a load instruction based on the address space, applying
    /// the appropriate type conversion (masking, sign extension, etc.).
    pub(super) fn lower_place_load(
        &self,
        place: &Place<'db>,
        loaded_ty: TyId<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if layout::ty_size_bytes_in(self.db, &self.layout, loaded_ty).is_some_and(|size| size == 0)
        {
            return Ok("0".into());
        }
        let addr = self.lower_place_address(place, state)?;
        let raw_load = match self.mir_func.body.place_address_space(place) {
            mir::ir::AddressSpaceKind::Memory => format!("mload({addr})"),
            mir::ir::AddressSpaceKind::Calldata => format!("calldataload({addr})"),
            mir::ir::AddressSpaceKind::Storage => format!("sload({addr})"),
            mir::ir::AddressSpaceKind::TransientStorage => format!("tload({addr})"),
        };

        // Apply type-specific conversion (std::evm::word::WordRepr::from_word equivalent)
        Ok(self.apply_from_word_conversion(&raw_load, loaded_ty))
    }

    /// Applies the `WordRepr::from_word` conversion for a given type.
    ///
    /// This mirrors the stdlib word-conversion semantics defined in:
    /// - `library/std/src/evm/word.fe` (`WordRepr` trait)
    ///
    /// Conversion rules:
    /// - bool: word != 0
    /// - u8/u16/u32/u64/u128: mask to appropriate width
    /// - u256: identity
    /// - i8/i16/i32/i64/i128/i256: sign extension
    ///
    /// NOTE: This is a single source of truth for codegen. If the stdlib word
    /// conversion semantics change, this function must be updated to match.
    fn apply_from_word_conversion(&self, raw_load: &str, ty: TyId<'db>) -> String {
        let ty = mir::repr::word_conversion_leaf_ty(self.db, ty);
        let base_ty = ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(self.db) {
            match prim {
                PrimTy::Bool => {
                    // bool: iszero(eq(word, 0)) which is equivalent to word != 0
                    format!("iszero(eq({raw_load}, 0))")
                }
                PrimTy::U8 => format!("and({raw_load}, 0xff)"),
                PrimTy::U16 => format!("and({raw_load}, 0xffff)"),
                PrimTy::U32 => format!("and({raw_load}, 0xffffffff)"),
                PrimTy::U64 => format!("and({raw_load}, 0xffffffffffffffff)"),
                PrimTy::U128 => {
                    format!("and({raw_load}, 0xffffffffffffffffffffffffffffffff)")
                }
                PrimTy::U256 | PrimTy::Usize => {
                    // No conversion needed for full-width unsigned
                    raw_load.to_string()
                }
                PrimTy::I8 => {
                    // Sign extension for i8
                    format!("signextend(0, and({raw_load}, 0xff))")
                }
                PrimTy::I16 => {
                    format!("signextend(1, and({raw_load}, 0xffff))")
                }
                PrimTy::I32 => {
                    format!("signextend(3, and({raw_load}, 0xffffffff))")
                }
                PrimTy::I64 => {
                    format!("signextend(7, and({raw_load}, 0xffffffffffffffff))")
                }
                PrimTy::I128 => {
                    format!("signextend(15, and({raw_load}, 0xffffffffffffffffffffffffffffffff))")
                }
                PrimTy::I256 | PrimTy::Isize => {
                    // Full-width signed doesn't need masking, sign is already there
                    raw_load.to_string()
                }
                // String, Array, Tuple, Ptr are aggregate/pointer types - no conversion
                PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) | PrimTy::Ptr => {
                    raw_load.to_string()
                }
            }
        } else {
            // Non-primitive types (aggregates, etc.) - no conversion
            raw_load.to_string()
        }
    }

    /// Applies the `WordRepr::to_word` conversion for a given type.
    pub(super) fn apply_to_word_conversion(&self, raw_value: &str, ty: TyId<'db>) -> String {
        let ty = mir::repr::word_conversion_leaf_ty(self.db, ty);
        let base_ty = ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(self.db) {
            match prim {
                PrimTy::Bool => format!("iszero(iszero({raw_value}))"),
                PrimTy::U8 => format!("and({raw_value}, 0xff)"),
                PrimTy::U16 => format!("and({raw_value}, 0xffff)"),
                PrimTy::U32 => format!("and({raw_value}, 0xffffffff)"),
                PrimTy::U64 => format!("and({raw_value}, 0xffffffffffffffff)"),
                PrimTy::U128 => {
                    format!("and({raw_value}, 0xffffffffffffffffffffffffffffffff)")
                }
                PrimTy::U256 | PrimTy::Usize => raw_value.to_string(),
                PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Isize => raw_value.to_string(),
                PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) | PrimTy::Ptr => {
                    raw_value.to_string()
                }
            }
        } else {
            raw_value.to_string()
        }
    }

    /// Lowers a PlaceRef (reference to a place with projection path).
    ///
    /// Walks the projection path to compute the byte offset from the base,
    /// returning the pointer without loading.
    pub(super) fn lower_place_ref(
        &self,
        place: &Place<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        self.lower_place_address(place, state)
    }

    /// Computes the address for a place by walking the projection path.
    ///
    /// Returns a Yul expression representing the memory/storage address.
    /// For memory, computes byte offsets. For storage, computes slot offsets.
    fn lower_place_address(
        &self,
        place: &Place<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let mut base_expr = self.lower_value(place.base, state)?;

        if place.projection.is_empty() {
            return Ok(base_expr);
        }

        // Get the base value's type to navigate projections
        let base_value = self.mir_func.body.value(place.base);
        let mut current_ty = base_value.ty;
        let mut total_offset: usize = 0;
        let is_slot_addressed = matches!(
            self.mir_func.body.place_address_space(place),
            mir::ir::AddressSpaceKind::Storage | mir::ir::AddressSpaceKind::TransientStorage
        );

        for proj in place.projection.iter() {
            match proj {
                Projection::Field(field_idx) => {
                    let field_types = current_ty.field_types(self.db);
                    if field_types.is_empty() {
                        return Err(YulError::Unsupported(format!(
                            "place projection: no field types for type but accessing field {}",
                            field_idx
                        )));
                    }
                    // Use slot-based offsets for storage, byte-based for memory
                    total_offset += if is_slot_addressed {
                        layout::field_offset_slots(self.db, current_ty, *field_idx)
                    } else {
                        layout::field_offset_bytes_or_word_aligned_in(
                            self.db,
                            &self.layout,
                            current_ty,
                            *field_idx,
                        )
                    };
                    // Update current type to the field's type
                    current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                        YulError::Unsupported(format!(
                            "place projection: target field {} out of bounds (have {} fields)",
                            field_idx,
                            field_types.len()
                        ))
                    })?;
                }
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    // Skip discriminant then compute field offset
                    // Use slot-based offsets for storage, byte-based for memory
                    if is_slot_addressed {
                        total_offset += 1;
                        total_offset += layout::variant_field_offset_slots(
                            self.db, *enum_ty, *variant, *field_idx,
                        );
                    } else {
                        total_offset += self.layout.discriminant_size_bytes;
                        total_offset += layout::variant_field_offset_bytes_or_word_aligned_in(
                            self.db,
                            &self.layout,
                            *enum_ty,
                            *variant,
                            *field_idx,
                        );
                    }
                    // Update current type to the field's type
                    let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                    let field_types = ctor.field_types(self.db);
                    current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                        YulError::Unsupported(format!(
                            "place projection: target variant field {} out of bounds (have {} fields)",
                            field_idx,
                            field_types.len()
                        ))
                    })?;
                }
                Projection::Discriminant => {
                    current_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
                }
                Projection::Index(idx_source) => {
                    let stride = if is_slot_addressed {
                        layout::array_elem_stride_slots(self.db, current_ty)
                    } else {
                        layout::array_elem_stride_bytes_in(self.db, &self.layout, current_ty)
                    }
                    .ok_or_else(|| {
                        YulError::Unsupported(
                            "place projection: array index access on non-array type".to_string(),
                        )
                    })?;

                    match idx_source {
                        IndexSource::Constant(idx) => {
                            total_offset += idx * stride;
                        }
                        IndexSource::Dynamic(value_id) => {
                            if total_offset != 0 {
                                base_expr = format!("add({base_expr}, {total_offset})");
                                total_offset = 0;
                            }
                            let idx_expr = self.lower_value(*value_id, state)?;
                            let offset_expr = if stride == 1 {
                                idx_expr
                            } else {
                                format!("mul({idx_expr}, {stride})")
                            };
                            base_expr = format!("add({base_expr}, {offset_expr})");
                        }
                    }

                    // Update current type to the element type.
                    let (base_ty, args) = current_ty.decompose_ty_app(self.db);
                    if !base_ty.is_array(self.db) || args.is_empty() {
                        return Err(YulError::Unsupported(
                            "place projection: array index on non-array type".to_string(),
                        ));
                    }
                    current_ty = args[0];
                }
                Projection::Deref => {
                    return Err(YulError::Unsupported(
                        "place projection: pointer dereference not yet implemented".to_string(),
                    ));
                }
            }
        }

        if total_offset != 0 {
            base_expr = format!("add({base_expr}, {total_offset})");
        }
        Ok(base_expr)
    }
}
