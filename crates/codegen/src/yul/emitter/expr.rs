//! Expression and value lowering helpers shared across the Yul emitter.

use hir::analysis::ty::decision_tree::Projection;
use hir::analysis::ty::simplified_pattern::ConstructorKind;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
use hir::hir_def::{
    CallableDef, Expr, ExprId, ItemKind, LitKind, Stmt, StmtId,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
    scope_graph::ScopeId,
};
use mir::{
    CallOrigin, ValueId, ValueOrigin,
    ir::{FieldPtrOrigin, Place, SyntheticValue},
    layout,
};

use crate::yul::{doc::YulDoc, state::BlockState};

use super::{
    YulError,
    function::FunctionEmitter,
    util::{function_name, try_collapse_cast_shim},
};

impl<'db> FunctionEmitter<'db> {
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
        let value_ty = value.ty;
        match &value.origin {
            ValueOrigin::Expr(expr_id) => {
                if let Some(temp) = self.match_values.get(expr_id) {
                    return Ok(temp.clone());
                }
                // Check if there's a better (non-Expr) value for this expression in expr_values.
                // This can happen when a tuple was pre-lowered via emit_alloc but the call's
                // args still reference an old ValueId with ValueOrigin::Expr.
                if let Some(better_value_id) = self.mir_func.body.expr_values.get(expr_id)
                    && *better_value_id != value_id
                {
                    let better_value = self.mir_func.body.value(*better_value_id);
                    if !matches!(&better_value.origin, ValueOrigin::Expr(_)) {
                        return self.lower_value(*better_value_id, state);
                    }
                }
                self.lower_expr(*expr_id, state)
            }
            ValueOrigin::BindingName(name) => Ok(state.resolve_name(name)),
            ValueOrigin::Call(call) => self.lower_call_value(call, state),
            ValueOrigin::Intrinsic(intr) => self.lower_intrinsic_value(intr, state),
            ValueOrigin::Synthetic(synth) => self.lower_synthetic_value(synth),
            ValueOrigin::FieldPtr(field_ptr) => self.lower_field_ptr(field_ptr, state),
            ValueOrigin::PlaceLoad(place) => self.lower_place_load(place, value_ty, state),
            ValueOrigin::PlaceRef(place) => self.lower_place_ref(place, state),
            _ => Err(YulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    /// Lowers a HIR expression into a Yul expression string.
    ///
    /// * `expr_id` - Expression to render.
    /// * `state` - Binding state used for nested expressions.
    ///
    /// Returns the fully-lowered Yul expression.
    pub(super) fn lower_expr(
        &self,
        expr_id: ExprId,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.expr_temps.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id) {
            let value = self.mir_func.body.value(*value_id);
            let value_ty = value.ty;
            match &value.origin {
                ValueOrigin::Call(call) => return self.lower_call_value(call, state),
                ValueOrigin::Synthetic(synth) => {
                    return self.lower_synthetic_value(synth);
                }
                ValueOrigin::FieldPtr(field_ptr) => {
                    return self.lower_field_ptr(field_ptr, state);
                }
                ValueOrigin::Intrinsic(intr) => {
                    return self.lower_intrinsic_value(intr, state);
                }
                ValueOrigin::PlaceLoad(place) => {
                    return self.lower_place_load(place, value_ty, state);
                }
                ValueOrigin::PlaceRef(place) => {
                    return self.lower_place_ref(place, state);
                }
                // For Expr origins, we just continue to process the expression below
                // to avoid infinite recursion when expr_values[expr_id] points to Expr(expr_id)
                ValueOrigin::Expr(_) => {}
                _ => {}
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
            Expr::Tuple(elems) => {
                // Tuples should be lowered to struct-like alloc+store_field operations in MIR.
                // If we reach here, something went wrong in MIR lowering.
                // Check if this is a unit tuple, which doesn't need allocation
                if elems.is_empty() {
                    return Ok("0".to_string()); // Unit tuple represented as 0
                }
                // Non-empty tuples must have been lowered - panic with diagnostic info
                let func_name = self
                    .mir_func
                    .func
                    .name(self.db)
                    .to_opt()
                    .map(|n| n.data(self.db).to_string())
                    .unwrap_or_default();
                panic!(
                    "tuple expression with {} elements was not lowered before codegen in function '{}'. \
                     expr_id: {:?}, expr_values contains: {}",
                    elems.len(),
                    func_name,
                    expr_id,
                    self.mir_func.body.expr_values.contains_key(&expr_id)
                );
            }
            Expr::Call(callee, call_args) => {
                let callee_expr = self.lower_expr(*callee, state)?;
                let mut lowered_args = Vec::with_capacity(call_args.len());
                for arg in call_args {
                    lowered_args.push(self.lower_expr(arg.expr, state)?);
                }
                Self::lower_and_format_call(&callee_expr, lowered_args)
            }
            Expr::MethodCall(receiver, _method_name, _generic_args, call_args) => {
                let callable =
                    self.mir_func
                        .typed_body
                        .callable_expr(expr_id)
                        .ok_or_else(|| {
                            YulError::Unsupported(
                                "method call expression does not have a registered callable".into(),
                            )
                        })?;

                let callee = match callable.callable_def {
                    CallableDef::Func(func) => {
                        let base = function_name(self.db, func);
                        let ScopeId::Item(ItemKind::Impl(impl_block)) =
                            func.scope().parent(self.db).unwrap()
                        else {
                            unreachable!()
                        };
                        let prefix = impl_block.ty(self.db).pretty_print(self.db).to_lowercase();
                        format!("{prefix}_{base}")
                    }
                    CallableDef::VariantCtor(_) => {
                        return Err(YulError::Unsupported(
                            "variant constructor method calls are not supported yet".into(),
                        ));
                    }
                };

                let receiver_expr = self.lower_expr(*receiver, state)?;
                let mut lowered_args = vec![receiver_expr];

                for arg in call_args {
                    lowered_args.push(self.lower_expr(arg.expr, state)?);
                }

                if let CallableDef::Func(func_def) = callable.callable_def {
                    let effect_args = self.lower_effect_arguments(func_def, state)?;
                    lowered_args.extend(effect_args);
                }

                Self::lower_and_format_call(&callee, lowered_args)
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
                if let Some(expr) = self.last_expr(stmts) {
                    self.lower_expr(expr, state)
                } else {
                    Ok("0".into())
                }
            }
            Expr::Path(path) => {
                let original = self.path_ident(*path).ok_or_else(|| {
                    let pretty = path
                        .to_opt()
                        .map(|path| path.pretty_print(self.db))
                        .unwrap_or_else(|| "_".to_string());
                    YulError::Unsupported(format!("unsupported path expression `{pretty}`"))
                })?;
                Ok(state.resolve_name(&original))
            }
            Expr::Field(..) => {
                // Field expressions should have been lowered to FieldPtr or get_field calls.
                // We already handled FieldPtr at the top of lower_expr. If we're here,
                // the field access wasn't properly lowered.
                let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
                Err(YulError::Unsupported(format!(
                    "field expressions should be rewritten before codegen (expr type {})",
                    ty.pretty_print(self.db)
                )))
            }
            Expr::RecordInit(..) => {
                if let Some(temp) = self.expr_temps.get(&expr_id) {
                    Ok(temp.clone())
                } else {
                    Err(YulError::Unsupported(
                        "record initializers should be lowered before codegen".into(),
                    ))
                }
            }
            other => Err(YulError::Unsupported(format!(
                "only simple expressions are supported: {other:?}"
            ))),
        }
    }

    /// Returns the last expression statement in a block, if any.
    ///
    /// * `stmts` - Slice of statement IDs to inspect.
    ///
    /// Returns the expression ID for the trailing expression statement when present.
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

    /// Formats a function call with the given callee and arguments, handling cast shim collapsing.
    ///
    /// * `callee` - Function name to call.
    /// * `lowered_args` - Already-lowered argument expressions.
    ///
    /// Returns the Yul function call string, possibly collapsed via cast shim.
    fn lower_and_format_call(callee: &str, lowered_args: Vec<String>) -> Result<String, YulError> {
        if let Some(arg) = try_collapse_cast_shim(callee, &lowered_args)? {
            return Ok(arg);
        }
        let result = if lowered_args.is_empty() {
            format!("{callee}()")
        } else {
            format!("{callee}({})", lowered_args.join(", "))
        };
        Ok(result)
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
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            match call.callable.callable_def {
                CallableDef::Func(func) => function_name(self.db, func),
                CallableDef::VariantCtor(_) => {
                    return Err(YulError::Unsupported(
                        "callable without hir function definition is not supported yet".into(),
                    ));
                }
            }
        };
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        for &arg in &call.effect_args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        Self::lower_and_format_call(&callee, lowered_args)
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
        }
    }

    /// Lowers expressions that may require extra statements (e.g. `if`).
    ///
    /// * `expr_id` - Expression to lower.
    /// * `docs` - Doc list to append emitted statements into.
    /// * `state` - Binding state for allocating temporaries.
    ///
    /// Returns either the inline expression or the name of a temporary containing the result.
    pub(super) fn lower_expr_with_statements(
        &mut self,
        expr_id: ExprId,
        docs: &mut Vec<YulDoc>,
        state: &mut BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.expr_temps.get(&expr_id) {
            return Ok(temp.clone());
        }
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

    /// Returns `true` when the given expression's type is the unit tuple.
    ///
    /// * `expr_id` - Expression identifier whose type should be tested.
    ///
    /// Returns `true` if the expression's type is the unit tuple.
    pub(super) fn expr_is_unit(&self, expr_id: ExprId) -> bool {
        let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
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
                mir::ir::AddressSpaceKind::Memory => field_ptr.offset_bytes,
                mir::ir::AddressSpaceKind::Storage => field_ptr.offset_bytes / 32,
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
        if layout::ty_size_bytes(self.db, loaded_ty) == Some(0) {
            return Ok("0".into());
        }
        let addr = self.lower_place_address(place, state)?;
        let raw_load = match place.address_space {
            mir::ir::AddressSpaceKind::Memory => format!("mload({addr})"),
            mir::ir::AddressSpaceKind::Storage => format!("sload({addr})"),
        };

        // Apply type-specific conversion (LoadableScalar::from_word equivalent)
        Ok(self.apply_from_word_conversion(&raw_load, loaded_ty))
    }

    /// Applies the LoadableScalar::from_word conversion for a given type.
    ///
    /// This mirrors the Fe core library's from_word implementations defined in:
    /// - `library/core/src/ptr.fe` (LoadableScalar trait)
    /// - `library/core/src/enum_repr.fe` (enum discriminant handling)
    ///
    /// Conversion rules:
    /// - bool: word != 0
    /// - u8/u16/u32/u64/u128: mask to appropriate width
    /// - u256: identity
    /// - i8/i16/i32/i64/i128/i256: sign extension
    ///
    /// NOTE: This is a single source of truth for codegen. If the core library
    /// semantics change, this function must be updated to match.
    fn apply_from_word_conversion(&self, raw_load: &str, ty: TyId<'db>) -> String {
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
        let base = self.lower_value(place.base, state)?;

        if place.projection.is_empty() {
            return Ok(base);
        }

        // Get the base value's type to navigate projections
        let base_value = self.mir_func.body.value(place.base);
        let mut current_ty = base_value.ty;
        let mut total_offset = 0;
        let is_storage = matches!(place.address_space, mir::ir::AddressSpaceKind::Storage);

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
                    total_offset += if is_storage {
                        layout::field_offset_slots(self.db, current_ty, *field_idx)
                    } else {
                        layout::field_offset_bytes_or_word_aligned(self.db, current_ty, *field_idx)
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
                    if is_storage {
                        total_offset += 1;
                        total_offset += layout::variant_field_offset_slots(
                            self.db, *enum_ty, *variant, *field_idx,
                        );
                    } else {
                        total_offset += layout::DISCRIMINANT_SIZE_BYTES;
                        total_offset += layout::variant_field_offset_bytes_or_word_aligned(
                            self.db, *enum_ty, *variant, *field_idx,
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
                // Index and Deref projections are not yet implemented
                Projection::Index(_) => {
                    return Err(YulError::Unsupported(
                        "place projection: array index access not yet implemented".to_string(),
                    ));
                }
                Projection::Deref => {
                    return Err(YulError::Unsupported(
                        "place projection: pointer dereference not yet implemented".to_string(),
                    ));
                }
            }
        }

        if total_offset == 0 {
            Ok(base)
        } else {
            Ok(format!("add({base}, {total_offset})"))
        }
    }

    /// Lowers effect arguments for a function call by looking up effect parameter bindings in the current state.
    ///
    /// * `func_def` - The function being called.
    /// * `state` - Current binding state containing effect parameter values.
    ///
    /// Returns a vector of lowered effect argument expressions.
    fn lower_effect_arguments(
        &self,
        func_def: hir::hir_def::Func<'db>,
        state: &BlockState,
    ) -> Result<Vec<String>, YulError> {
        let mut effect_args = Vec::new();
        for effect in func_def.effect_params(self.db) {
            let binding = self.effect_binding_name(effect);
            let yul_expr = state.resolve_name(&binding);
            effect_args.push(yul_expr);
        }
        Ok(effect_args)
    }
}
