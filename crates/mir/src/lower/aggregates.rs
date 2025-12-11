//! Aggregate lowering and layout helpers for MIR: allocations, field/variant stores, sizing, and
//! synthetic literals used by records and enums.

use crate::layout;

use super::*;
use hir::analysis::ty::ty_def::prim_int_bits;
use hir::hir_def::EnumVariant;
use hir::projection::{IndexSource, Projection};
use num_bigint::BigUint;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Emits an allocation for the requested size and binds it to the expression.
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
        alloc_ty: TyId<'db>,
        size_bytes: usize,
    ) -> ValueId {
        let alloc_value = self.mir_body.alloc_value(ValueData {
            ty: alloc_ty,
            origin: ValueOrigin::Alloc {
                size_bytes,
                address_space: AddressSpaceKind::Memory,
            },
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

    /// Emits a discriminant store for an enum allocation.
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
        variant: EnumVariant<'db>,
    ) {
        let addr_space = self.value_address_space(base_value);
        let place = Place::new(
            base_value,
            MirProjectionPath::from_projection(Projection::Discriminant),
            addr_space,
        );
        self.push_inst(
            block,
            MirInst::SetDiscriminant {
                expr,
                place,
                variant,
            },
        );
    }

    /// Emits a batch of field stores as MIR place writes.
    ///
    /// # Parameters
    /// - `expr`: Expression id for context.
    /// - `block`: Block to emit stores in.
    /// - `base_value`: Pointer to the allocated record/variant.
    /// - `stores`: List of `(field_idx, field_value)` tuples.
    /// - `helper`: Core helper used to perform the store.
    ///
    /// # Returns
    /// Nothing; stores are emitted as instructions.
    pub(super) fn emit_store_fields(
        &mut self,
        expr: ExprId,
        block: BasicBlockId,
        base_value: ValueId,
        stores: &[(usize, ValueId)],
    ) {
        if stores.is_empty() {
            return;
        }
        let addr_space = self.value_address_space(base_value);
        for (field_idx, field_value) in stores {
            let place = Place::new(
                base_value,
                MirProjectionPath::from_projection(Projection::Field(*field_idx)),
                addr_space,
            );
            self.push_inst(
                block,
                MirInst::Store {
                    expr,
                    place,
                    value: *field_value,
                },
            );
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

        let value_id = self.emit_alloc(expr, curr_block, record_ty, size_bytes);

        let mut stores = Vec::with_capacity(lowered_fields.len());
        for (label, field_value) in lowered_fields {
            let field_index = FieldIndex::Ident(label);
            let Some(info) = self.field_access_info(record_ty, field_index) else {
                continue;
            };
            stores.push((info.field_idx, field_value));
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

        // Compute tuple size and allocate
        let Some(size_bytes) = layout::ty_size_bytes(self.db, tuple_ty) else {
            let value_id = self.ensure_value(expr);
            return (Some(curr_block), value_id);
        };

        let value_id = self.emit_alloc(expr, curr_block, tuple_ty, size_bytes);

        // Store each element by field index
        let mut stores = Vec::with_capacity(lowered_elems.len());
        for (i, elem_value) in lowered_elems.into_iter().enumerate() {
            stores.push((i, elem_value));
        }
        self.emit_store_fields(expr, curr_block, value_id, &stores);

        (Some(curr_block), value_id)
    }

    /// Lowers an array literal into an allocation plus element stores.
    pub(super) fn try_lower_array(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        elems: &[ExprId],
    ) -> (Option<BasicBlockId>, ValueId) {
        let array_ty = self.typed_body.expr_ty(self.db, expr);
        let elem_ty = match array_ty.generic_args(self.db).first() {
            Some(ty) => *ty,
            None => {
                let value_id = self.ensure_value(expr);
                return (Some(block), value_id);
            }
        };

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

        let elem_size = layout::ty_size_bytes(self.db, elem_ty).unwrap_or(layout::WORD_SIZE_BYTES);
        let size_bytes = elem_size * lowered_elems.len();
        let value_id = self.emit_alloc(expr, curr_block, array_ty, size_bytes);

        let addr_space = self.value_address_space(value_id);
        for (idx, elem_value) in lowered_elems.into_iter().enumerate() {
            let place = Place::new(
                value_id,
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx))),
                addr_space,
            );
            self.push_inst(
                curr_block,
                MirInst::Store {
                    expr,
                    place,
                    value: elem_value,
                },
            );
        }

        (Some(curr_block), value_id)
    }

    /// Lowers an array repetition literal into an allocation plus repeated stores.
    pub(super) fn try_lower_array_rep(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        elem: ExprId,
        len: Partial<Body<'db>>,
    ) -> (Option<BasicBlockId>, ValueId) {
        let array_ty = self.typed_body.expr_ty(self.db, expr);
        let elem_ty = match array_ty.generic_args(self.db).first() {
            Some(ty) => *ty,
            None => {
                let value_id = self.ensure_value(expr);
                return (Some(block), value_id);
            }
        };

        let Some(len_body) = len.to_opt() else {
            let value_id = self.ensure_value(expr);
            return (Some(block), value_id);
        };
        let Some(count) = self.const_usize_from_body(len_body) else {
            let value_id = self.ensure_value(expr);
            return (Some(block), value_id);
        };

        let (next_block, elem_value) = self.lower_expr_in(block, elem);
        let Some(curr_block) = next_block else {
            let value_id = self.ensure_value(expr);
            return (None, value_id);
        };

        let elem_size = layout::ty_size_bytes(self.db, elem_ty).unwrap_or(layout::WORD_SIZE_BYTES);
        let size_bytes = elem_size * count;
        let value_id = self.emit_alloc(expr, curr_block, array_ty, size_bytes);

        let addr_space = self.value_address_space(value_id);
        for idx in 0..count {
            let place = Place::new(
                value_id,
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx))),
                addr_space,
            );
            self.push_inst(
                curr_block,
                MirInst::Store {
                    expr,
                    place,
                    value: elem_value,
                },
            );
        }

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
        Some(FieldAccessInfo {
            field_ty: self.field_ty_by_index(&record_like, idx)?,
            field_idx: idx,
        })
    }

    /// Computes the field type for the `idx`th field of a record-like type.
    ///
    /// # Parameters
    /// - `record_like`: Record or variant wrapper.
    /// - `idx`: Zero-based field index.
    ///
    /// # Returns
    /// The field type, or `None` if out of bounds.
    pub(super) fn field_ty_by_index(
        &self,
        record_like: &RecordLike<'db>,
        idx: usize,
    ) -> Option<TyId<'db>> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::EnumVariant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);
        if idx >= field_types.len() {
            return None;
        }
        Some(field_types[idx])
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
