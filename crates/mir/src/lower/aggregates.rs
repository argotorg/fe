//! Aggregate lowering and layout helpers for MIR: allocations, field/variant stores, sizing, and
//! synthetic literals used by records and enums.

use super::*;
use hir::analysis::ty::ty_def::prim_int_bits;

#[derive(Copy, Clone)]
struct AggregateCopyCtx {
    expr: ExprId,
    block: BasicBlockId,
    dst_base: ValueId,
    dst_offset: u64,
    addr_space: AddressSpaceKind,
}

impl AggregateCopyCtx {
    fn with_offset(self, offset: u64) -> Self {
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
        size_bytes: u64,
    ) -> ValueId {
        let alloc_callable = self.core.make_callable(expr, CoreHelper::Alloc, &[]);
        let alloc_ret_ty = alloc_callable.ret_ty(self.db);
        let size_value = self.synthetic_u256(BigUint::from(size_bytes));
        let alloc_value = self.mir_body.alloc_value(ValueData {
            ty: alloc_ret_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: alloc_callable,
                args: vec![size_value],
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
        stores: &[(u64, TyId<'db>, ValueId)],
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
                let offset_value = self.synthetic_u256(BigUint::from(*offset_bytes));
                let store_ret_ty = callable.ret_ty(self.db);
                let store_call = self.mir_body.alloc_value(ValueData {
                    ty: store_ret_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr,
                        callable,
                        args: vec![base_value, offset_value, *field_value],
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

        let mut field_offset = 0u64;
        for field_ty in field_types {
            let is_nested_aggregate = field_ty.field_count(self.db) > 0;
            let field_size = self.ty_size_bytes(field_ty).unwrap_or(32);

            if is_nested_aggregate {
                // Recursively handle nested aggregates
                // Create a FieldPtr for the source field
                let src_field_ptr = if field_offset == 0 {
                    src_ptr
                } else {
                    let ptr = self.mir_body.alloc_value(ValueData {
                        ty: field_ty,
                        origin: ValueOrigin::FieldPtr(FieldPtrOrigin {
                            base: src_ptr,
                            offset_bytes: field_offset,
                        }),
                    });
                    self.value_address_space.insert(ptr, src_space);
                    ptr
                };
                self.emit_aggregate_copy(field_ty, src_field_ptr, ctx.with_offset(field_offset));
            } else {
                // Load from source and store to destination
                let src_offset_value = self.synthetic_u256(BigUint::from(field_offset));
                let get_callable = self.core.make_callable(
                    ctx.expr,
                    CoreHelper::GetField,
                    &[src_ptr_ty, field_ty],
                );
                let loaded_value = self.mir_body.alloc_value(ValueData {
                    ty: field_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: ctx.expr,
                        callable: get_callable,
                        args: vec![src_ptr, src_offset_value],
                        receiver_space: None,
                        resolved_name: None,
                    }),
                });

                // Store to destination
                let dst_offset_value =
                    self.synthetic_u256(BigUint::from(ctx.dst_offset + field_offset));
                let store_callable =
                    self.core
                        .make_callable(ctx.expr, CoreHelper::StoreField, &[ptr_ty, field_ty]);
                let store_ret_ty = store_callable.ret_ty(self.db);
                let store_call = self.mir_body.alloc_value(ValueData {
                    ty: store_ret_ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: ctx.expr,
                        callable: store_callable,
                        args: vec![ctx.dst_base, dst_offset_value, loaded_value],
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
            field_offset += field_size;
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
        let record_like = RecordLike::from_ty(record_ty);
        let Some(size_bytes) = self.record_size_bytes(&record_like) else {
            let value_id = self.ensure_value(expr);
            return (Some(curr_block), value_id);
        };

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

    /// Returns the bit width for a primitive integer type.
    ///
    /// # Parameters
    /// - `ty`: Type to inspect.
    ///
    /// # Returns
    /// Bit width when the type is a supported primitive, otherwise `None`.
    pub(super) fn int_type_bits(&self, ty: TyId<'db>) -> Option<u16> {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Prim(prim)) => prim_int_bits(*prim),
            _ => None,
        }
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
    ) -> Option<(TyId<'db>, u64)> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::EnumVariant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);
        if idx >= field_types.len() {
            return None;
        }

        let mut offset = 0u64;
        for field_ty in field_types.iter().take(idx) {
            let size = self.ty_size_bytes(*field_ty)?;
            offset += size;
        }
        Some((field_types[idx], offset))
    }

    /// Computes the total byte width of a record by summing its fields.
    ///
    /// # Parameters
    /// - `record_like`: Record or variant wrapper.
    ///
    /// # Returns
    /// Total size in bytes if all field sizes are known.
    pub(super) fn record_size_bytes(&self, record_like: &RecordLike<'db>) -> Option<u64> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::EnumVariant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);

        let mut size = 0u64;
        for field_ty in field_types {
            let field_size = self.ty_size_bytes(field_ty)?;
            size += field_size;
        }
        Some(size)
    }

    /// Computes the total byte width of an enum: discriminant plus largest payload.
    ///
    /// # Parameters
    /// - `enum_ty`: Enum type to size.
    ///
    /// # Returns
    /// Total enum size in bytes when layout is known.
    pub(super) fn enum_size_bytes(&self, enum_ty: TyId<'db>) -> Option<u64> {
        let (base_ty, args) = enum_ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Adt(adt_def)) = base_ty.data(self.db) else {
            return None;
        };
        if !matches!(adt_def.adt_ref(self.db), AdtRef::Enum(_)) {
            return None;
        }

        let mut max_payload = 0u64;
        for variant in adt_def.fields(self.db) {
            let mut payload = 0u64;
            for ty in variant.iter_types(self.db) {
                let field_ty = ty.instantiate(self.db, args);
                payload += self.ty_size_bytes(field_ty).unwrap_or(32);
            }
            max_payload = max_payload.max(payload);
        }

        Some(super::ENUM_DISCRIMINANT_SIZE_BYTES + max_payload)
    }

    /// Returns the byte width of primitive integer/bool types we can layout today.
    ///
    /// # Parameters
    /// - `ty`: Type to measure.
    ///
    /// # Returns
    /// Size in bytes when known.
    pub(super) fn ty_size_bytes(&self, ty: TyId<'db>) -> Option<u64> {
        // Handle tuples first (check base type for TyApp cases)
        if ty.is_tuple(self.db) {
            let mut size = 0u64;
            for field_ty in ty.field_types(self.db) {
                size += self.ty_size_bytes(field_ty)?;
            }
            return Some(size);
        }

        // Handle primitives
        if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(self.db).data(self.db) {
            if *prim == PrimTy::Bool {
                return Some(1);
            }
            if let Some(bits) = prim_int_bits(*prim) {
                return Some((bits / 8) as u64);
            }
        }

        // Handle ADT types (structs) - use adt_def() which handles TyApp
        if let Some(adt_def) = ty.adt_def(self.db)
            && matches!(adt_def.adt_ref(self.db), AdtRef::Struct(_))
        {
            let record_like = RecordLike::from_ty(ty);
            return self.record_size_bytes(&record_like);
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
        let offset_value = self.synthetic_u256(BigUint::from(info.offset_bytes));
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
