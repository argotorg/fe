//! Aggregate lowering helpers for MIR: allocations, initializer emission, and type helpers.

use super::*;
use hir::analysis::ty::ty_def::prim_int_bits;
use hir::projection::{IndexSource, Projection};
use num_bigint::BigUint;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Emits an allocation for the given type and binds it to the expression.
    ///
    /// # Parameters
    /// - `expr`: Expression id associated with the allocation.
    ///
    /// # Returns
    /// The `ValueId` of the allocated pointer.
    pub(super) fn emit_alloc(&mut self, expr: ExprId, alloc_ty: TyId<'db>) -> ValueId {
        let alloc_value = self.builder.body.alloc_value(ValueData {
            ty: alloc_ty,
            origin: ValueOrigin::Alloc {
                address_space: AddressSpaceKind::Memory,
            },
        });
        self.builder.body.expr_values.insert(expr, alloc_value);
        self.value_address_space
            .insert(alloc_value, AddressSpaceKind::Memory);
        self.push_inst_here(MirInst::EvalExpr {
            expr,
            value: alloc_value,
            bind_value: true,
        });
        alloc_value
    }

    pub(super) fn emit_init_aggregate(
        &mut self,
        expr: ExprId,
        base_value: ValueId,
        inits: Vec<(MirProjectionPath<'db>, ValueId)>,
    ) {
        if inits.is_empty() {
            return;
        }
        let addr_space = self.value_address_space(base_value);
        let place = Place::new(base_value, MirProjectionPath::new(), addr_space);
        self.push_inst_here(MirInst::InitAggregate { expr, place, inits });
    }

    /// Lowers a record literal into an allocation plus `store_field` calls.
    ///
    /// # Parameters
    /// - `expr`: Record literal expression id.
    /// - `fields`: Field initializers.
    ///
    /// # Returns
    /// The value representing the allocated record.
    pub(super) fn try_lower_record(&mut self, expr: ExprId, fields: &[Field<'db>]) -> ValueId {
        let fallback = self.ensure_value(expr);
        if self.current_block().is_none() {
            return fallback;
        }
        let mut lowered_fields = Vec::with_capacity(fields.len());
        for field in fields {
            let value = self.lower_expr(field.expr);
            if self.current_block().is_none() {
                return fallback;
            }
            let Some(label) = field.label_eagerly(self.db, self.body) else {
                return fallback;
            };
            lowered_fields.push((label, value));
        }

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
            self.builder.body.expr_values.insert(expr, value);
            return value;
        }

        let value_id = self.emit_alloc(expr, record_ty);

        let mut inits = Vec::with_capacity(lowered_fields.len());
        for (label, field_value) in lowered_fields {
            let field_index = FieldIndex::Ident(label);
            let Some(info) = self.field_access_info(record_ty, field_index) else {
                continue;
            };
            inits.push((
                MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                field_value,
            ));
        }
        self.emit_init_aggregate(expr, value_id, inits);

        value_id
    }

    /// Lowers a tuple literal into an allocation plus `store_field` calls.
    ///
    /// Tuples are treated as struct-like aggregates: memory is allocated for the
    /// full tuple size, and each element is stored at its computed byte offset.
    ///
    /// # Parameters
    /// - `expr`: Tuple literal expression id.
    /// - `elems`: Element expressions.
    ///
    /// # Returns
    /// The value representing the allocated tuple.
    pub(super) fn try_lower_tuple(&mut self, expr: ExprId, elems: &[ExprId]) -> ValueId {
        let fallback = self.ensure_value(expr);
        let tuple_ty = self.typed_body.expr_ty(self.db, expr);

        // Handle unit tuple () - zero size, no allocation needed
        if tuple_ty.field_count(self.db) == 0 {
            return fallback;
        }

        // Lower all element expressions
        let mut lowered_elems = Vec::with_capacity(elems.len());
        for &elem_expr in elems {
            lowered_elems.push(self.lower_expr(elem_expr));
            if self.current_block().is_none() {
                return fallback;
            }
        }

        let value_id = self.emit_alloc(expr, tuple_ty);

        // Store each element by field index
        let mut inits = Vec::with_capacity(lowered_elems.len());
        for (i, elem_value) in lowered_elems.into_iter().enumerate() {
            inits.push((
                MirProjectionPath::from_projection(Projection::Field(i)),
                elem_value,
            ));
        }
        self.emit_init_aggregate(expr, value_id, inits);

        value_id
    }

    /// Lowers an array literal into an allocation plus element stores.
    pub(super) fn try_lower_array(&mut self, expr: ExprId, elems: &[ExprId]) -> ValueId {
        let fallback = self.ensure_value(expr);
        let array_ty = self.typed_body.expr_ty(self.db, expr);
        if array_ty.generic_args(self.db).is_empty() {
            return fallback;
        }

        let mut lowered_elems = Vec::with_capacity(elems.len());
        for &elem_expr in elems {
            lowered_elems.push(self.lower_expr(elem_expr));
            if self.current_block().is_none() {
                return fallback;
            }
        }

        let value_id = self.emit_alloc(expr, array_ty);

        let addr_space = self.value_address_space(value_id);
        let mut inits = Vec::with_capacity(lowered_elems.len());
        for (idx, elem_value) in lowered_elems.into_iter().enumerate() {
            let proj =
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx)));
            inits.push((proj, elem_value));
        }
        let place = Place::new(value_id, MirProjectionPath::new(), addr_space);
        self.push_inst_here(MirInst::InitAggregate { expr, place, inits });

        value_id
    }

    /// Lowers an array repetition literal into an allocation plus repeated stores.
    pub(super) fn try_lower_array_rep(
        &mut self,
        expr: ExprId,
        elem: ExprId,
        len: Partial<Body<'db>>,
    ) -> ValueId {
        let fallback = self.ensure_value(expr);
        let array_ty = self.typed_body.expr_ty(self.db, expr);
        if array_ty.generic_args(self.db).is_empty() {
            return fallback;
        }

        let Some(len_body) = len.to_opt() else {
            return fallback;
        };
        let Some(count) = self.const_usize_from_body(len_body) else {
            return fallback;
        };

        let elem_value = self.lower_expr(elem);
        if self.current_block().is_none() {
            return fallback;
        }

        let value_id = self.emit_alloc(expr, array_ty);

        let addr_space = self.value_address_space(value_id);
        let mut inits = Vec::with_capacity(count);
        for idx in 0..count {
            let proj =
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx)));
            inits.push((proj, elem_value));
        }
        let place = Place::new(value_id, MirProjectionPath::new(), addr_space);
        self.push_inst_here(MirInst::InitAggregate { expr, place, inits });

        value_id
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
        self.builder.body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(value)),
        })
    }
}
