//! Aggregate lowering and layout helpers for MIR: allocations, field/variant stores, sizing, and
//! synthetic literals used by records and enums.

use crate::layout;

use super::*;
use hir::analysis::ty::ty_def::prim_int_bits;

#[derive(Copy, Clone)]
struct AggregateCopyCtx {
    expr: ExprId,
    block: BasicBlockId,
    dst_base: ValueId,
    dst_offset: usize,
    addr_space: AddressSpaceKind,
}

impl AggregateCopyCtx {
    fn with_offset(self, offset: usize) -> Self {
        Self {
            dst_offset: self.dst_offset + offset,
            ..self
        }
    }
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Emits an `alloc` call for the requested size and binds it to the expression.
    ///
    /// # Parameters
    /// - `expr`: Expression id associated with the allocation.
    /// - `block`: Block to emit the allocation in.
    /// - `size_bytes`: Allocation size in bytes.
    ///
    /// # Returns
    /// The `ValueId` of the allocated pointer.
    pub(super) fn emit_alloc(
        &mut self,
        expr: ExprId,
        block: BasicBlockId,
        size_bytes: usize,
    ) -> ValueId {
        let alloc_callable = self.core.make_callable(expr, CoreHelper::Alloc, &[]);
        let alloc_value_ty = self.typed_body.expr_ty(self.db, expr);
        let size_value = self.synthetic_u256(BigUint::from(size_bytes));
        let alloc_value = self.mir_body.alloc_value(ValueData {
            ty: alloc_value_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: alloc_callable,
                args: vec![size_value],
                effect_args: Vec::new(),
                effect_kinds: Vec::new(),
                receiver_space: None,
                resolved_name: None,
            }),
        });
        self.mir_body.expr_values.insert(expr, alloc_value);
        self.value_address_space
            .insert(alloc_value, AddressSpaceKind::Memory);
        self.push_inst(
            block,
            MirInst::EvalExpr {
                expr,
                value: alloc_value,
                bind_value: true,
            },
        );
        alloc_value
    }

    /// Emits a `store_discriminant` call for an enum allocation.
    ///
    /// # Parameters
    /// - `expr`: Expression id for source context.
    /// - `block`: Block to emit the store in.
    /// - `base_value`: Pointer to the enum allocation.
    /// - `variant_index`: Discriminant value to store.
    pub(super) fn emit_store_discriminant(
        &mut self,
        expr: ExprId,
        block: BasicBlockId,
        base_value: ValueId,
        variant_index: u64,
    ) {
        let ptr_ty = match self.value_address_space(base_value) {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        let store_discr_callable =
            self.core
                .make_callable(expr, CoreHelper::StoreDiscriminant, &[ptr_ty]);
        let discr_value = self.synthetic_u256(BigUint::from(variant_index));
        let store_ret_ty = store_discr_callable.ret_ty(self.db);
        let store_discr_call = self.mir_body.alloc_value(ValueData {
            ty: store_ret_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: store_discr_callable,
                args: vec![base_value, discr_value],
                effect_args: Vec::new(),
                effect_kinds: Vec::new(),
                receiver_space: None,
                resolved_name: None,
            }),
        });
        self.push_inst(
            block,
            MirInst::EvalExpr {
                expr,
                value: store_discr_call,
                bind_value: false,
            },
        );
    }

    /// Emits a batch of field or variant stores using the provided core helper.
    ///
    /// # Parameters
    /// - `expr`: Expression id for context.
    /// - `block`: Block to emit stores in.
    /// - `base_value`: Pointer to the allocated record/variant.
    /// - `stores`: List of `(offset_bytes, field_ty, field_value)` tuples.
    /// - `helper`: Core helper used to perform the store.
    ///
    /// # Returns
    /// Nothing; stores are emitted as instructions.
    pub(super) fn emit_store_fields(
        &mut self,
        expr: ExprId,
        block: BasicBlockId,
        base_value: ValueId,
        stores: &[(usize, TyId<'db>, ValueId)],
    ) {
        if stores.is_empty() {
            return;
        }
        let addr_space = self.value_address_space(base_value);
        let ptr_ty = match addr_space {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        for (offset_bytes, field_ty, field_value) in stores {
            if self.is_zero_sized_ty(*field_ty) {
                continue;
            }
            let is_aggregate = field_ty.field_count(self.db) > 0;

            if is_aggregate {
                // For aggregate fields, recursively copy each nested field
                let ctx = AggregateCopyCtx {
                    expr,
                    block,
                    dst_base: base_value,
                    dst_offset: *offset_bytes,
                    addr_space,
                };
                self.emit_aggregate_copy(*field_ty, *field_value, ctx);
            } else {
                // For primitive fields, emit a store_field call
                let callable =
                    self.core
                        .make_callable(expr, CoreHelper::StoreField, &[ptr_ty, *field_ty]);
                let offset_units = self.offset_units_for_space(addr_space, *offset_bytes);
                let offset_value = self.synthetic_u256(BigUint::from(offset_units));
                let store_ret_ty = callable.ret_ty(self.db);
                let store_call = self.mir_body.alloc_value(ValueData {
                    ty: store_ret_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr,
                        callable,
                        args: vec![base_value, offset_value, *field_value],
                        effect_args: Vec::new(),
                        effect_kinds: Vec::new(),
                        receiver_space: None,
                        resolved_name: None,
                    }),
                });
                self.push_inst(
                    block,
                    MirInst::EvalExpr {
                        expr,
                        value: store_call,
                        bind_value: false,
                    },
                );
            }
        }
    }

    /// Recursively copies an aggregate (struct) from source to destination at the given offset.
    ///
    /// # Parameters
    /// - `expr`: Expression id for context.
    /// - `block`: Block to emit stores in.
    /// - `dst_base`: Destination base pointer.
    /// - `dst_offset`: Byte offset within destination for this aggregate.
    /// - `ty`: Type of the aggregate being copied.
    /// - `src_ptr`: Source pointer to the aggregate.
    /// - `addr_space`: Address space of the destination.
    fn emit_aggregate_copy(&mut self, ty: TyId<'db>, src_ptr: ValueId, ctx: AggregateCopyCtx) {
        if self.is_zero_sized_ty(ty) {
            return;
        }
        let field_types = ty.field_types(self.db);
        let ptr_ty = match ctx.addr_space {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        // Determine source address space for loading
        let src_space = self.value_address_space(src_ptr);
        let src_ptr_ty = match src_space {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };

        for (field_idx, field_ty) in field_types.iter().enumerate() {
            if self.is_zero_sized_ty(*field_ty) {
                continue;
            }
            let is_nested_aggregate = field_ty.field_count(self.db) > 0;
            let field_offset = layout::field_offset_bytes_or_word_aligned(self.db, ty, field_idx);

            if is_nested_aggregate {
                // Recursively handle nested aggregates
                // Create a FieldPtr for the source field
                let src_field_ptr = if field_offset == 0 {
                    src_ptr
                } else {
                    let ptr = self.mir_body.alloc_value(ValueData {
                        ty: *field_ty,
                        origin: ValueOrigin::FieldPtr(FieldPtrOrigin {
                            base: src_ptr,
                            offset_bytes: field_offset,
                            addr_space: src_space,
                        }),
                    });
                    self.value_address_space.insert(ptr, src_space);
                    ptr
                };
                self.emit_aggregate_copy(*field_ty, src_field_ptr, ctx.with_offset(field_offset));
            } else {
                // Load from source and store to destination
                let src_offset_units = self.offset_units_for_space(src_space, field_offset);
                let src_offset_value = self.synthetic_u256(BigUint::from(src_offset_units));
                let get_callable = self.core.make_callable(
                    ctx.expr,
                    CoreHelper::GetField,
                    &[src_ptr_ty, *field_ty],
                );
                let loaded_value = self.mir_body.alloc_value(ValueData {
                    ty: *field_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: ctx.expr,
                        callable: get_callable,
                        args: vec![src_ptr, src_offset_value],
                        effect_args: Vec::new(),
                        effect_kinds: Vec::new(),
                        receiver_space: None,
                        resolved_name: None,
                    }),
                });

                // Store to destination
                let dst_offset_units =
                    self.offset_units_for_space(ctx.addr_space, ctx.dst_offset + field_offset);
                let dst_offset_value = self.synthetic_u256(BigUint::from(dst_offset_units));
                let store_callable =
                    self.core
                        .make_callable(ctx.expr, CoreHelper::StoreField, &[ptr_ty, *field_ty]);
                let store_ret_ty = store_callable.ret_ty(self.db);
                let store_call = self.mir_body.alloc_value(ValueData {
                    ty: store_ret_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: ctx.expr,
                        callable: store_callable,
                        args: vec![ctx.dst_base, dst_offset_value, loaded_value],
                        effect_args: Vec::new(),
                        effect_kinds: Vec::new(),
                        receiver_space: None,
                        resolved_name: None,
                    }),
                });
                self.push_inst(
                    ctx.block,
                    MirInst::EvalExpr {
                        expr: ctx.expr,
                        value: store_call,
                        bind_value: false,
                    },
                );
            }
        }
    }

    /// Lowers a record literal into an allocation plus `store_field` calls.
    ///
    /// # Parameters
    /// - `block`: Block to start lowering in.
    /// - `expr`: Record literal expression id.
    /// - `fields`: Field initializers.
    ///
    /// # Returns
    /// The successor block and the value representing the allocated record.
    pub(super) fn try_lower_record(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        fields: &[Field<'db>],
    ) -> (Option<BasicBlockId>, ValueId) {
        let mut current = Some(block);
        let mut lowered_fields = Vec::with_capacity(fields.len());
        for field in fields {
            let Some(curr_block) = current else {
                break;
            };
            let (next_block, value) = self.lower_expr_in(curr_block, field.expr);
            current = next_block;
            let Some(label) = field.label_eagerly(self.db, self.body) else {
                let value_id = self.ensure_value(expr);
                return (current, value_id);
            };
            lowered_fields.push((label, value));
        }

        let Some(curr_block) = current else {
            let value_id = self.ensure_value(expr);
            return (None, value_id);
        };

        let record_ty = self.typed_body.expr_ty(self.db, expr);
        let record_base = record_ty.base_ty(self.db);
        let effect_ptr_bases = [
            self.core
                .helper_ty(CoreHelperTy::EffectMemPtr)
                .base_ty(self.db),
            self.core
                .helper_ty(CoreHelperTy::EffectStorPtr)
                .base_ty(self.db),
            self.core
                .helper_ty(CoreHelperTy::EffectCalldataPtr)
                .base_ty(self.db),
        ];
        if effect_ptr_bases.contains(&record_base) && lowered_fields.len() == 1 {
            let value = lowered_fields[0].1;
            self.mir_body.expr_values.insert(expr, value);
            return (Some(curr_block), value);
        }
        let record_like = RecordLike::from_ty(record_ty);
        let Some(size_bytes) = self.record_size_bytes(&record_like) else {
            let value_id = self.ensure_value(expr);
            return (Some(curr_block), value_id);
        };
        if size_bytes == 0 {
            let value_id = self.synthetic_zero_for_ty(record_ty);
            self.mir_body.expr_values.insert(expr, value_id);
            return (Some(curr_block), value_id);
        }

        let value_id = self.emit_alloc(expr, curr_block, size_bytes);

        let mut stores = Vec::with_capacity(lowered_fields.len());
        for (label, field_value) in lowered_fields {
            let field_index = FieldIndex::Ident(label);
            let Some(info) = self.field_access_info(record_ty, field_index) else {
                continue;
            };
            stores.push((info.offset_bytes, info.field_ty, field_value));
        }
        self.emit_store_fields(expr, curr_block, value_id, &stores);

        (Some(curr_block), value_id)
    }

    /// Lowers a tuple literal into an allocation plus `store_field` calls.
    ///
    /// Tuples are treated as struct-like aggregates: memory is allocated for the
    /// full tuple size, and each element is stored at its computed byte offset.
    ///
    /// # Parameters
    /// - `block`: Block to start lowering in.
    /// - `expr`: Tuple literal expression id.
    /// - `elems`: Element expressions.
    ///
    /// # Returns
    /// The successor block and the value representing the allocated tuple.
    pub(super) fn try_lower_tuple(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        elems: &[ExprId],
    ) -> (Option<BasicBlockId>, ValueId) {
        let tuple_ty = self.typed_body.expr_ty(self.db, expr);

        // Handle unit tuple () - zero size, no allocation needed
        if tuple_ty.field_count(self.db) == 0 {
            let value_id = self.ensure_value(expr);
            return (Some(block), value_id);
        }

        // Lower all element expressions
        let mut current = Some(block);
        let mut lowered_elems = Vec::with_capacity(elems.len());
        for &elem_expr in elems {
            let Some(curr_block) = current else {
                break;
            };
            let (next_block, value) = self.lower_expr_in(curr_block, elem_expr);
            current = next_block;
            lowered_elems.push(value);
        }

        let Some(curr_block) = current else {
            let value_id = self.ensure_value(expr);
            return (None, value_id);
        };

        // Compute tuple size and allocate.
        // When size is unknown (e.g., tuple contains unmonomorphized type parameters),
        // fall back to word-aligned layout (32 bytes per element). This matches EVM
        // conventions and will be corrected during monomorphization when concrete
        // types are substituted.
        let Some(size_bytes) = layout::ty_size_bytes(self.db, tuple_ty) else {
            let size_bytes = tuple_ty.field_count(self.db) * 32;
            let value_id = self.emit_alloc(expr, curr_block, size_bytes);
            let field_types = tuple_ty.field_types(self.db);
            let mut stores = Vec::with_capacity(lowered_elems.len());
            for (i, elem_value) in lowered_elems.into_iter().enumerate() {
                let field_ty = field_types[i];
                let offset = i * 32;
                stores.push((offset, field_ty, elem_value));
            }
            self.emit_store_fields(expr, curr_block, value_id, &stores);
            return (Some(curr_block), value_id);
        };
        if size_bytes == 0 {
            let value_id = self.synthetic_zero_for_ty(tuple_ty);
            self.mir_body.expr_values.insert(expr, value_id);
            return (Some(curr_block), value_id);
        }

        let value_id = self.emit_alloc(expr, curr_block, size_bytes);

        // Store each element at its computed offset using centralized layout API
        let field_types = tuple_ty.field_types(self.db);
        let mut stores = Vec::with_capacity(lowered_elems.len());
        for (i, elem_value) in lowered_elems.into_iter().enumerate() {
            let field_ty = field_types[i];
            let offset = layout::field_offset_bytes_or_word_aligned(self.db, tuple_ty, i);
            stores.push((offset, field_ty, elem_value));
        }
        self.emit_store_fields(expr, curr_block, value_id, &stores);

        (Some(curr_block), value_id)
    }

    /// Returns the bit width for a primitive integer type.
    ///
    /// # Parameters
    /// - `ty`: Type to inspect.
    ///
    /// # Returns
    /// Bit width when the type is a supported primitive, otherwise `None`.
    pub(super) fn int_type_bits(&self, ty: TyId<'db>) -> Option<usize> {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Prim(prim)) => prim_int_bits(*prim),
            _ => None,
        }
    }

    pub(super) fn is_zero_sized_ty(&self, ty: TyId<'db>) -> bool {
        matches!(layout::ty_size_bytes(self.db, ty), Some(0))
    }

    pub(super) fn synthetic_zero_for_ty(&mut self, ty: TyId<'db>) -> ValueId {
        let value = self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(0u8))),
        });
        self.value_address_space
            .entry(value)
            .or_insert(AddressSpaceKind::Memory);
        value
    }

    /// Returns the field type and byte offset for a given receiver/field pair.
    ///
    /// # Parameters
    /// - `owner_ty`: Type containing the field.
    /// - `field_index`: Field identifier (by name or index).
    ///
    /// # Returns
    /// Field type and offset in bytes, or `None` if the field cannot be resolved.
    pub(super) fn field_access_info(
        &self,
        owner_ty: TyId<'db>,
        field_index: FieldIndex<'db>,
    ) -> Option<FieldAccessInfo<'db>> {
        let record_like = RecordLike::from_ty(owner_ty);
        let idx = match field_index {
            FieldIndex::Ident(label) => record_like.record_field_idx(self.db, label)?,
            FieldIndex::Index(integer) => integer.data(self.db).to_usize()?,
        };
        let (field_ty, offset_bytes) = self.field_layout_by_index(&record_like, idx)?;
        Some(FieldAccessInfo {
            field_ty,
            offset_bytes,
            field_idx: idx,
        })
    }

    /// Computes `(field_ty, offset_bytes)` for the `idx`th field of a record-like type.
    ///
    /// # Parameters
    /// - `record_like`: Record or variant wrapper.
    /// - `idx`: Zero-based field index.
    ///
    /// # Returns
    /// The field type and byte offset, or `None` if out of bounds.
    pub(super) fn field_layout_by_index(
        &self,
        record_like: &RecordLike<'db>,
        idx: usize,
    ) -> Option<(TyId<'db>, usize)> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::EnumVariant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);
        if idx >= field_types.len() {
            return None;
        }

        let offset = layout::field_offset_bytes(self.db, ty, idx)?;
        Some((field_types[idx], offset))
    }

    /// Computes the total byte width of a record by summing its fields.
    ///
    /// # Parameters
    /// - `record_like`: Record or variant wrapper.
    ///
    /// # Returns
    /// Total size in bytes if all field sizes are known.
    pub(super) fn record_size_bytes(&self, record_like: &RecordLike<'db>) -> Option<usize> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::EnumVariant(variant) => variant.ty,
        };
        layout::ty_size_bytes(self.db, ty)
    }

    /// Computes the total byte width of an enum: discriminant plus largest payload.
    ///
    /// # Parameters
    /// - `enum_ty`: Enum type to size.
    ///
    /// # Returns
    /// Total enum size in bytes when layout is known.
    pub(super) fn enum_size_bytes(&self, enum_ty: TyId<'db>) -> Option<usize> {
        let (base_ty, args) = enum_ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Adt(adt_def)) = base_ty.data(self.db) else {
            return None;
        };
        if !matches!(adt_def.adt_ref(self.db), AdtRef::Enum(_)) {
            return None;
        }

        let mut max_payload = 0;
        for variant in adt_def.fields(self.db) {
            let mut payload = 0;
            for ty in variant.iter_types(self.db) {
                let field_ty = ty.instantiate(self.db, args);
                payload += layout::ty_size_bytes(self.db, field_ty).unwrap_or(32);
            }
            max_payload = max_payload.max(payload);
        }

        Some(layout::DISCRIMINANT_SIZE_BYTES + max_payload)
    }

    /// Returns the ABI-encoded byte width for statically-sized values.
    ///
    /// This matches the head size used by the ABI encoder/decoder: primitive values occupy one
    /// 32-byte word, while tuples/records are the concatenation of their fields.
    pub(super) fn abi_static_size_bytes(&self, ty: TyId<'db>) -> Option<usize> {
        if ty.is_tuple(self.db)
            || ty
                .adt_ref(self.db)
                .is_some_and(|adt| matches!(adt, AdtRef::Struct(_)))
        {
            let mut size = 0;
            for field_ty in ty.field_types(self.db) {
                size += self.abi_static_size_bytes(field_ty)?;
            }
            return Some(size);
        }

        if let TyData::TyBase(TyBase::Prim(_)) = ty.base_ty(self.db).data(self.db) {
            return Some(32);
        }

        None
    }

    /// Lowers an assignment to a field target into a `store_field` helper call
    /// (for primitives) or a recursive field-by-field copy (for aggregates).
    ///
    /// # Parameters
    /// - `block`: Basic block where the assignment occurs.
    /// - `expr`: Expression id for context.
    /// - `target`: Assignment target expression (expected to be `Expr::Field`).
    /// - `value`: Lowered value to store.
    ///
    /// # Returns
    /// Updated block when handled, or `None` to fall back to a normal assignment.
    pub(super) fn try_lower_field_assign(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        target: ExprId,
        value: ValueId,
    ) -> Option<BasicBlockId> {
        let Partial::Present(Expr::Field(lhs, field_index)) = target.data(self.db, self.body)
        else {
            return None;
        };
        let field_index = field_index.to_opt()?;
        let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
        let info = self.field_access_info(lhs_ty, field_index)?;
        if self.is_zero_sized_ty(info.field_ty) {
            return Some(block);
        }

        let addr_value = self.ensure_value(*lhs);
        let addr_space = self.value_address_space(addr_value);
        let is_aggregate = info.field_ty.field_count(self.db) > 0;

        if is_aggregate {
            // For aggregate fields, recursively copy each nested field
            let ctx = AggregateCopyCtx {
                expr,
                block,
                dst_base: addr_value,
                dst_offset: info.offset_bytes,
                addr_space,
            };
            self.emit_aggregate_copy(info.field_ty, value, ctx);
            return Some(block);
        }

        // For primitive fields, emit a store_field call
        let ptr_ty = match addr_space {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        let offset_units = self.offset_units_for_space(addr_space, info.offset_bytes);
        let offset_value = self.synthetic_u256(BigUint::from(offset_units));
        let callable =
            self.core
                .make_callable(expr, CoreHelper::StoreField, &[ptr_ty, info.field_ty]);

        let store_ret_ty = callable.ret_ty(self.db);
        let store_call = self.mir_body.alloc_value(ValueData {
            ty: store_ret_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable,
                args: vec![addr_value, offset_value, value],
                effect_args: Vec::new(),
                effect_kinds: Vec::new(),
                receiver_space: None,
                resolved_name: None,
            }),
        });

        self.push_inst(
            block,
            MirInst::EvalExpr {
                expr,
                value: store_call,
                bind_value: false,
            },
        );
        Some(block)
    }

    /// Emits a synthetic `u256` literal value.
    ///
    /// # Parameters
    /// - `value`: Integer literal to encode.
    ///
    /// # Returns
    /// The allocated synthetic value id.
    pub(super) fn synthetic_u256(&mut self, value: BigUint) -> ValueId {
        let ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(value)),
        })
    }
}
