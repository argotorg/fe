//! Aggregate lowering helpers for MIR: allocations, initializer emission, and type helpers.

use super::*;
use hir::{
    analysis::ty::const_eval::{ConstValue, try_eval_const_body},
    projection::{IndexSource, Projection},
};
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
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let dest = self.alloc_temp_local(alloc_ty, false, "alloc");
        self.emit_alloc_into_local(expr, dest)
    }

    pub(super) fn emit_alloc_into_local(&mut self, expr: ExprId, dest: LocalId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        self.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
        self.push_inst_here(MirInst::Assign {
            stmt: None,
            dest: Some(dest),
            rvalue: crate::ir::Rvalue::Alloc {
                address_space: AddressSpaceKind::Memory,
            },
        });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        self.builder.body.values[value_id.index()].repr = ValueRepr::Ref(AddressSpaceKind::Memory);
        value_id
    }

    pub(super) fn emit_init_aggregate(
        &mut self,
        base_value: ValueId,
        inits: Vec<(MirProjectionPath<'db>, ValueId)>,
    ) {
        if inits.is_empty() {
            return;
        }
        let place = Place::new(base_value, MirProjectionPath::new());
        self.push_inst_here(MirInst::InitAggregate { place, inits });
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

        // Transparent newtypes (single-field structs) are represented identically to their single
        // field, so record literals lower to a representation-preserving cast.
        if crate::repr::transparent_newtype_field_ty(self.db, record_ty).is_some()
            && lowered_fields.len() == 1
        {
            let field_value = lowered_fields[0].1;
            self.builder.body.values[fallback.index()].origin =
                ValueOrigin::TransparentCast { value: field_value };
            self.builder.body.values[fallback.index()].repr =
                self.value_repr_for_expr(expr, record_ty);
            return fallback;
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
        self.emit_init_aggregate(value_id, inits);

        value_id
    }

    /// Lowers a tuple literal into an allocation plus `store_field` calls.
    ///
    /// Tuples are treated as struct-like aggregates: memory is allocated for the
    /// full tuple size, and each element is stored at its computed byte offset.
    ///
    /// Transparent single-element tuples `(T,)` are represented identically to `T`, so tuple
    /// literals lower to a representation-preserving cast.
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

        if crate::repr::transparent_newtype_field_ty(self.db, tuple_ty).is_some()
            && lowered_elems.len() == 1
        {
            let elem_value = lowered_elems[0];
            self.builder.body.values[fallback.index()].origin =
                ValueOrigin::TransparentCast { value: elem_value };
            self.builder.body.values[fallback.index()].repr =
                self.value_repr_for_expr(expr, tuple_ty);
            return fallback;
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
        self.emit_init_aggregate(value_id, inits);

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

        let mut inits = Vec::with_capacity(lowered_elems.len());
        for (idx, elem_value) in lowered_elems.into_iter().enumerate() {
            let proj =
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx)));
            inits.push((proj, elem_value));
        }
        self.emit_init_aggregate(value_id, inits);

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
        let Some(ConstValue::Int(count)) = try_eval_const_body(self.db, len_body) else {
            unreachable!("array len must be an int")
        };
        let count = count.to_u32().expect("array with more than 2^32 elements") as usize;

        let elem_value = self.lower_expr(elem);
        if self.current_block().is_none() {
            return fallback;
        }

        let value_id = self.emit_alloc(expr, array_ty);

        let mut inits = Vec::with_capacity(count);
        for idx in 0..count {
            let proj =
                MirProjectionPath::from_projection(Projection::Index(IndexSource::Constant(idx)));
            inits.push((proj, elem_value));
        }
        self.emit_init_aggregate(value_id, inits);

        value_id
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
            repr: ValueRepr::Word,
        })
    }
}
