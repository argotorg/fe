use hir::analysis::{semantic::FieldIndex, ty::AddressSpace};
use mir::{
    Layout, LayoutId, ResolvedPlaceElem, RuntimeClass, RuntimeMemoryLayout, ScalarRepr, VariantId,
    runtime::place::is_packed_newtype,
};
use sonatina_ir::{
    Type, ValueId,
    inst::{
        arith::{Add, Mul, Shl, Shr, Sub},
        cast::Zext,
        cmp::{Eq, Ne},
        control_flow::{BrTable, Jump, Phi, PhiArgs, Unreachable},
        data::{ConstLoad, InsertValue, ObjAlloc, ObjLoad, ObjMaterializeStack, ObjStore},
        evm::EvmSignExtend,
        logic::{And, Not, Or},
    },
    types::CompoundType,
};

use super::{
    CopySource, EnumLoad, FunctionLowerer, LowerError, Lowered, LoweringInstSet, cast_int_value,
    condition_to_i1, scalar_ty,
};

// The descriptor's layout also records the referent's address space. Provider
// kinds may be erased when a reference is stored inside an ordinary typed slot.
const REFERENCE_LAYOUTS: [Option<AddressSpace>; 8] = [
    Some(AddressSpace::Memory),
    None, // Native Sonatina object layout in memory.
    Some(AddressSpace::Storage),
    Some(AddressSpace::Transient),
    Some(AddressSpace::Calldata),
    Some(AddressSpace::Code),
    // A packed field: its lane's byte offset is in bits 8..16 of the layout,
    // its byte width in bits 16..24.
    Some(AddressSpace::Storage),
    Some(AddressSpace::Transient),
];
const PACKED_STORAGE: u64 = 6;
const NATIVE_MEMORY: u64 = 1;

#[derive(Clone, Copy)]
pub(super) enum ReferentLayout {
    Raw(AddressSpace),
    Object,
}

/// Stored references are one-word pointers to immutable address/layout
/// descriptors. Copying a reference never copies its referent.
///
/// A stored reference is an access, which ends before the frame that creates
/// it returns or the code creating it runs again, so its descriptor and an
/// exported object both stay in that frame.
#[derive(Clone, Copy)]
pub(super) struct MemoryReference {
    pub(super) addr: ValueId,
    pub(super) layout: ValueId,
}

impl<'db, I: LoweringInstSet + 'static> FunctionLowerer<'_, 'db, '_, I> {
    pub(super) fn store_memory_reference(
        &mut self,
        reference: MemoryReference,
    ) -> Result<ValueId, LowerError> {
        let ty = self.fb.module_builder.declare_array_type(Type::I256, 2);
        let descriptor = self.fb.insert_inst(
            ObjAlloc::new(self.module.inst_set(), ty),
            self.fb.module_builder.objref_type(ty),
        );
        let mut value = self.fb.make_undef_value(ty);
        for (idx, word) in [reference.addr, reference.layout].into_iter().enumerate() {
            let word = self.coerce_value_to_ty(word, Type::I256)?;
            let idx = self.index_value(idx as u64);
            value = self.fb.insert_inst(
                InsertValue::new(self.module.inst_set(), value, idx, word),
                ty,
            );
        }
        self.fb
            .insert_inst_no_result(ObjStore::new(self.module.inst_set(), descriptor, value));
        let ptr_ty = self.fb.ptr_type(ty);
        let descriptor = self.fb.insert_inst(
            ObjMaterializeStack::new(self.module.inst_set(), descriptor),
            ptr_ty,
        );
        self.coerce_value_to_ty(descriptor, self.module.address_ty(AddressSpace::Memory))
    }

    pub(super) fn load_memory_reference(
        &mut self,
        descriptor: ValueId,
    ) -> Result<MemoryReference, LowerError> {
        let addr = self.load_word(descriptor, AddressSpace::Memory)?;
        let layout_addr = self.offset_address_unscaled(descriptor, 32)?;
        let layout = self.load_word(layout_addr, AddressSpace::Memory)?;
        Ok(MemoryReference { addr, layout })
    }

    pub(super) fn export_object_reference(
        &mut self,
        object: ValueId,
    ) -> Result<ValueId, LowerError> {
        let Some(CompoundType::ObjRef(pointee)) = self
            .fb
            .type_of(object)
            .resolve_compound(&self.fb.module_builder.ctx)
        else {
            return Err(LowerError::Internal(
                "memory-reference export requires object storage".into(),
            ));
        };
        let ptr_ty = self.fb.ptr_type(pointee);
        // This exports the original allocation, including all aliases. It must
        // not be replaced with an allocation followed by a referent copy.
        let addr = self.fb.insert_inst(
            ObjMaterializeStack::new(self.module.inst_set(), object),
            ptr_ty,
        );
        self.store_native_reference(addr)
    }

    pub(super) fn store_native_reference(&mut self, addr: ValueId) -> Result<ValueId, LowerError> {
        let layout = self.index_value(NATIVE_MEMORY);
        self.store_memory_reference(MemoryReference { addr, layout })
    }

    pub(super) fn store_raw_reference(
        &mut self,
        addr: ValueId,
        space: AddressSpace,
    ) -> Result<ValueId, LowerError> {
        let tag = REFERENCE_LAYOUTS
            .iter()
            .position(|candidate| *candidate == Some(space))
            .expect("every raw address space has a descriptor layout");
        let layout = self.index_value(tag as u64);
        self.store_memory_reference(MemoryReference { addr, layout })
    }

    fn reference_layout_tag(&mut self, layout: ValueId) -> ValueId {
        let mask = self.index_value(0xff);
        self.fb
            .insert_inst(And::new(self.module.inst_set(), layout, mask), Type::I256)
    }

    /// The byte at bit `shift` of a packed descriptor's `layout`: its lane's
    /// byte offset at 8 and its byte width at 16.
    fn reference_layout_byte(&mut self, layout: ValueId, shift: u64) -> ValueId {
        let shift = self.index_value(shift);
        let shifted = self
            .fb
            .insert_inst(Shr::new(self.module.inst_set(), shift, layout), Type::I256);
        let mask = self.index_value(0xff);
        self.fb
            .insert_inst(And::new(self.module.inst_set(), shifted, mask), Type::I256)
    }

    /// The bit offset of the lane a packed descriptor's `layout` records.
    fn reference_lane_shift(&mut self, layout: ValueId) -> ValueId {
        let bytes = self.reference_layout_byte(layout, 8);
        let eight = self.index_value(8);
        self.fb
            .insert_inst(Mul::new(self.module.inst_set(), bytes, eight), Type::I256)
    }

    /// A mask of as many low-order bytes of a word as the lane a packed
    /// descriptor's `layout` records holds.
    fn reference_lane_mask(&mut self, layout: ValueId) -> ValueId {
        let bytes = self.reference_layout_byte(layout, 16);
        let eight = self.index_value(8);
        let bits = self
            .fb
            .insert_inst(Mul::new(self.module.inst_set(), bytes, eight), Type::I256);
        let one = self.index_value(1);
        let bit = self
            .fb
            .insert_inst(Shl::new(self.module.inst_set(), bits, one), Type::I256);
        self.fb
            .insert_inst(Sub::new(self.module.inst_set(), bit, one), Type::I256)
    }

    /// `value` sign-extended from the top byte of the lane a packed
    /// descriptor's `layout` records.
    fn reference_sign_extend(
        &mut self,
        layout: ValueId,
        value: ValueId,
    ) -> Result<ValueId, LowerError> {
        let bytes = self.reference_layout_byte(layout, 16);
        let one = self.index_value(1);
        let byte = self
            .fb
            .insert_inst(Sub::new(self.module.inst_set(), bytes, one), Type::I256);
        Ok(self.fb.insert_inst(
            EvmSignExtend::new(self.module.required_inst::<EvmSignExtend>()?, byte, value),
            Type::I256,
        ))
    }

    /// Whether a referent of class `class` can lie in a packed lane: a bool,
    /// an integer, or a packed newtype, whose field's value lies there.
    fn lies_in_lane(&self, class: &RuntimeClass<'db>) -> bool {
        match class {
            RuntimeClass::Scalar(scalar) => {
                matches!(scalar.repr, ScalarRepr::Bool | ScalarRepr::Int { .. })
            }
            _ => is_packed_newtype(self.module.db, class),
        }
    }

    pub(super) fn load_referent(
        &mut self,
        reference: MemoryReference,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = match class {
            RuntimeClass::Scalar(scalar) => self.module.scalar_repr_ty(scalar),
            _ => self.module.ty_for_class(class)?,
        };
        let packed = self.lies_in_lane(class).then_some(class);
        self.load_reference_value(reference, ty, packed, |this, memory| {
            this.load_memory_value(reference.addr, memory, class)
        })
    }

    pub(super) fn load_referent_enum_tag(
        &mut self,
        reference: MemoryReference,
        layout: LayoutId<'db>,
    ) -> Result<ValueId, LowerError> {
        let Layout::Enum(data) = layout.data(self.module.db) else {
            return Err(LowerError::Internal("enum tag requires enum layout".into()));
        };
        let ty = self.module.enum_tag_ty(layout)?;
        self.load_reference_value(reference, ty, None, |this, memory| {
            // Validate the discriminant without reading or reconstructing payloads.
            this.load_enum_from_ptr(reference.addr, memory, layout, &data, EnumLoad::Tag)
        })
    }

    fn load_reference_value(
        &mut self,
        reference: MemoryReference,
        ty: Type,
        packed: Option<&RuntimeClass<'db>>,
        mut load: impl FnMut(&mut Self, ReferentLayout) -> Result<ValueId, LowerError>,
    ) -> Result<ValueId, LowerError> {
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let native = self.module.is_native_target();
        let blocks = REFERENCE_LAYOUTS
            .iter()
            .enumerate()
            .filter(|(tag, _)| *tag < PACKED_STORAGE as usize || packed.is_some())
            .filter(|(_, space)| !native || matches!(space, None | Some(AddressSpace::Memory)))
            .map(|(tag, space)| (tag, self.fb.append_block(), *space))
            .collect::<Vec<_>>();
        let cases = blocks
            .iter()
            .map(|(tag, block, _)| (self.index_value(*tag as u64), *block))
            .collect::<Vec<_>>();
        let tag = if packed.is_some() {
            self.reference_layout_tag(reference.layout)
        } else {
            reference.layout
        };
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            tag,
            Some(invalid),
            cases,
        ));
        let mut values = PhiArgs::with_capacity(blocks.len());
        for (tag, block, space) in blocks {
            self.fb.switch_to_block(block);
            let value = if tag >= PACKED_STORAGE as usize {
                let class = packed.expect("a referent that can lie in a lane");
                let word = self.load_word(reference.addr, space.expect("packed space"))?;
                let shift = self.reference_lane_shift(reference.layout);
                let word = self
                    .fb
                    .insert_inst(Shr::new(self.module.inst_set(), shift, word), Type::I256);
                // `extract_packed_scalar` for a lane whose width the
                // descriptor records.
                self.lane_value(class, &mut |this, scalar| {
                    let mask = this.reference_lane_mask(reference.layout);
                    let value = this
                        .fb
                        .insert_inst(And::new(this.module.inst_set(), word, mask), Type::I256);
                    let ty = scalar_ty(scalar);
                    if ty == Type::I1 {
                        return Ok(condition_to_i1(&mut this.fb, value, this.module.inst_set()));
                    }
                    let value = if scalar.is_signed_int() {
                        this.reference_sign_extend(reference.layout, value)?
                    } else {
                        value
                    };
                    let is = this.module.inst_set();
                    Ok(cast_int_value(
                        &mut this.fb,
                        is,
                        value,
                        ty,
                        scalar.is_signed_int(),
                    ))
                })?
            } else {
                load(
                    self,
                    space.map_or(ReferentLayout::Object, ReferentLayout::Raw),
                )?
            };
            let pred = self
                .fb
                .current_block()
                .expect("referent load has a current block");
            values.push((value, pred));
            self.fb
                .insert_inst_no_result(Jump::new(self.module.inst_set(), done));
        }
        self.fb.switch_to_block(invalid);
        self.fb
            .insert_inst_no_result(Unreachable::new(self.module.inst_set()));
        self.fb.switch_to_block(done);
        Ok(self
            .fb
            .insert_inst(Phi::new(self.module.inst_set(), values), ty))
    }

    pub(super) fn store_referent(
        &mut self,
        reference: MemoryReference,
        class: &RuntimeClass<'db>,
        value: ValueId,
    ) -> Result<(), LowerError> {
        let ty = match class {
            RuntimeClass::Scalar(scalar) => self.module.scalar_repr_ty(scalar),
            _ => self.module.ty_for_class(class)?,
        };
        let value = self.coerce_value_to_ty(value, ty)?;
        let packed = self.lies_in_lane(class);
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let native = self.module.is_native_target();
        let blocks = REFERENCE_LAYOUTS
            .iter()
            .enumerate()
            .filter(|(tag, _)| *tag < PACKED_STORAGE as usize || packed)
            .filter(|(_, space)| {
                (!native || matches!(space, None | Some(AddressSpace::Memory)))
                    && !matches!(space, Some(AddressSpace::Code | AddressSpace::Calldata))
            })
            .map(|(tag, space)| (tag, self.fb.append_block(), *space))
            .collect::<Vec<_>>();
        let cases = blocks
            .iter()
            .map(|(tag, block, _)| (self.index_value(*tag as u64), *block))
            .collect::<Vec<_>>();
        let tag = if packed {
            self.reference_layout_tag(reference.layout)
        } else {
            reference.layout
        };
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            tag,
            Some(invalid),
            cases,
        ));
        for (tag, block, space) in blocks {
            self.fb.switch_to_block(block);
            if tag >= PACKED_STORAGE as usize {
                let space = space.expect("packed space");
                // `packed_scalar_bits` for a lane whose width the descriptor
                // records.
                let bits = self.lane_bits(value, class, &mut |this, value, scalar| {
                    let signed = scalar.is_signed_int();
                    let word = this.cast_scalar_with_signedness(value, Type::I256, signed)?;
                    let mask = this.reference_lane_mask(reference.layout);
                    let clean = this
                        .fb
                        .insert_inst(And::new(this.module.inst_set(), word, mask), Type::I256);
                    // A lane is at least a byte, so only a wider integer, a
                    // packed newtype's field, can be narrowed and not fit.
                    if matches!(scalar.repr, ScalarRepr::Int { bits, .. } if bits > 8) {
                        let fitted = if signed {
                            this.reference_sign_extend(reference.layout, clean)?
                        } else {
                            clean
                        };
                        let lost = this
                            .fb
                            .insert_inst(Ne::new(this.module.inst_set(), fitted, word), Type::I1);
                        this.emit_empty_revert(lost)?;
                    }
                    Ok(clean)
                })?;
                let shift = self.reference_lane_shift(reference.layout);
                let bits = self
                    .fb
                    .insert_inst(Shl::new(self.module.inst_set(), shift, bits), Type::I256);
                let mask = self.reference_lane_mask(reference.layout);
                let mask = self
                    .fb
                    .insert_inst(Shl::new(self.module.inst_set(), shift, mask), Type::I256);
                let keep = self
                    .fb
                    .insert_inst(Not::new(self.module.inst_set(), mask), Type::I256);
                let old = self.load_word(reference.addr, space)?;
                let kept = self
                    .fb
                    .insert_inst(And::new(self.module.inst_set(), old, keep), Type::I256);
                let word = self
                    .fb
                    .insert_inst(Or::new(self.module.inst_set(), kept, bits), Type::I256);
                self.store_word(reference.addr, space, word)?;
            } else if let Some(space) = space {
                self.copy_to_ptr(reference.addr, space, class, value)?;
            } else {
                self.copy_memory_value(reference.addr, ReferentLayout::Object, class, value)?;
            }
            self.fb
                .insert_inst_no_result(Jump::new(self.module.inst_set(), done));
        }
        self.fb.switch_to_block(invalid);
        self.fb
            .insert_inst_no_result(Unreachable::new(self.module.inst_set()));
        self.fb.switch_to_block(done);
        Ok(())
    }

    pub(super) fn copy_source_value(
        &mut self,
        source: CopySource<'db>,
        target: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let class = self.copy_source_class(&source).clone();
        let ty = self.module.ty_for_class(&class)?;
        let value = match source {
            CopySource::Value { value, .. } => value,
            CopySource::Object { value, .. } => self
                .fb
                .insert_inst(ObjLoad::new(self.module.inst_set(), value), ty),
            CopySource::Const { value, .. } => self
                .fb
                .insert_inst(ConstLoad::new(self.module.inst_set(), value), ty),
            CopySource::Ptr {
                addr, space, lane, ..
            } => self.load_from_ptr_lane(addr, space, lane, &class)?,
        };
        self.retype_value_for_class(value, &class, target)
    }

    /// The offset of field `index` of `class`'s Sonatina allocation, which
    /// stored references address natively.
    pub(super) fn native_field_offset(
        &mut self,
        class: &RuntimeClass<'db>,
        index: usize,
    ) -> Result<u64, LowerError> {
        let ty = self.module.ty_for_class(class)?;
        self.fb
            .module_builder
            .ctx
            .aggregate_elem_offset(ty, index)
            .map(|(offset, _)| offset as u64)
            .ok_or_else(|| LowerError::Unsupported("unrepresentable native field offset".into()))
    }

    /// The offset of `variant`'s field `field` in its enum's Sonatina
    /// allocation, whose variants share one payload.
    pub(super) fn native_variant_field_offset(
        &mut self,
        variant: VariantId<'db>,
        field: usize,
    ) -> Result<u64, LowerError> {
        let ty = self.module.ty_for_class(&RuntimeClass::AggregateValue {
            layout: variant.enum_layout,
        })?;
        self.fb
            .module_builder
            .ctx
            .enum_variant_field_offset(ty, variant.index as usize, field)
            .map(|(offset, _)| offset as u64)
            .ok_or_else(|| LowerError::Unsupported("unrepresentable native variant offset".into()))
    }

    /// The integer type of the tag of an enum with `layout`'s Sonatina
    /// allocation.
    pub(super) fn native_tag_ty(&mut self, layout: LayoutId<'db>) -> Result<Type, LowerError> {
        let Type::Compound(enum_ty) = self.module.ty_for_layout(layout)? else {
            return Err(LowerError::Internal("enum layout is not compound".into()));
        };
        self.fb
            .module_builder
            .ctx
            .with_ty_store(|store| store.enum_data(enum_ty).map(|data| data.tag_ty()))
            .ok_or_else(|| LowerError::Internal("enum tag layout missing".into()))
    }

    pub(super) fn referent_field_address(
        &mut self,
        addr: ValueId,
        memory: ReferentLayout,
        class: &RuntimeClass<'db>,
        index: usize,
    ) -> Result<ValueId, LowerError> {
        let offset = match memory {
            ReferentLayout::Object => self.native_field_offset(class, index)?,
            ReferentLayout::Raw(space) => {
                let layout = class
                    .aggregate_layout()
                    .ok_or_else(|| LowerError::Internal("field requires aggregate".into()))?;
                let raw = RuntimeMemoryLayout::for_space(self.module.db, space);
                match layout.data(self.module.db) {
                    Layout::Struct(data) => raw.struct_field_offset(&data, index)?,
                    Layout::Array(data) => raw.array_element_offset(&data, index as u64)?,
                    Layout::Enum(_) => {
                        return Err(LowerError::Internal("enum field requires variant".into()));
                    }
                }
            }
        };
        self.offset_address_unscaled(addr, offset)
    }

    pub(super) fn referent_variant_address(
        &mut self,
        addr: ValueId,
        memory: ReferentLayout,
        variant: VariantId<'db>,
        field: usize,
    ) -> Result<ValueId, LowerError> {
        let offset = match memory {
            ReferentLayout::Object => self.native_variant_field_offset(variant, field)?,
            ReferentLayout::Raw(space) => RuntimeMemoryLayout::for_space(self.module.db, space)
                .variant_field_offset(variant, FieldIndex(field as u16))?,
        };
        self.offset_address_unscaled(addr, offset)
    }

    fn memory_layout_offset(
        &mut self,
        layout: ValueId,
        packed: u64,
        native: u64,
        word: u64,
    ) -> ValueId {
        let native_tag = self.index_value(NATIVE_MEMORY);
        let native_flag = self.fb.insert_inst(
            Eq::new(self.module.inst_set(), layout, native_tag),
            Type::I1,
        );
        let storage_tag = self.index_value(2);
        let transient_tag = self.index_value(3);
        let storage_flag = self.fb.insert_inst(
            Eq::new(self.module.inst_set(), layout, storage_tag),
            Type::I1,
        );
        let transient_flag = self.fb.insert_inst(
            Eq::new(self.module.inst_set(), layout, transient_tag),
            Type::I1,
        );
        let word_flag = self.fb.insert_inst(
            Or::new(self.module.inst_set(), storage_flag, transient_flag),
            Type::I1,
        );
        let packed = self.index_value(packed);
        let mut offset = packed;
        for (flag, size) in [(native_flag, native), (word_flag, word)] {
            let flag = self.fb.insert_inst(
                Zext::new(self.module.inst_set(), flag, Type::I256),
                Type::I256,
            );
            let size = self.index_value(size);
            let difference = self
                .fb
                .insert_inst(Sub::new(self.module.inst_set(), size, packed), Type::I256);
            let difference = self.fb.insert_inst(
                Mul::new(self.module.inst_set(), flag, difference),
                Type::I256,
            );
            offset = self.fb.insert_inst(
                Add::new(self.module.inst_set(), offset, difference),
                Type::I256,
            );
        }
        offset
    }

    pub(super) fn project_memory_reference(
        &mut self,
        reference: MemoryReference,
        base: &RuntimeClass<'db>,
        elem: &ResolvedPlaceElem<'db>,
    ) -> Result<Lowered<(MemoryReference, RuntimeClass<'db>)>, LowerError> {
        let raw = RuntimeMemoryLayout::raw(self.module.db);
        let words = RuntimeMemoryLayout::for_space(self.module.db, AddressSpace::Storage);
        let mut projected_layout = reference.layout;
        let (offset, class) = match elem {
            ResolvedPlaceElem::Field { field, class } => {
                let placement = words.field_placement(base, *field)?;
                if let Some(lane) = placement.lane.filter(|_| placement.partial) {
                    // Memory fields remain unpacked. Only storage/transient
                    // descriptors acquire a packed tag and their lane. A
                    // packed descriptor keeps its lane: only a packed
                    // newtype's field lies in one, and in the same bytes.
                    for tag in [2u64, 3] {
                        let old_tag = self.index_value(tag);
                        let flag = self.fb.insert_inst(
                            Eq::new(self.module.inst_set(), reference.layout, old_tag),
                            Type::I1,
                        );
                        let flag = self.fb.insert_inst(
                            Zext::new(self.module.inst_set(), flag, Type::I256),
                            Type::I256,
                        );
                        let delta = self.index_value(
                            4 + (u64::from(lane.byte_offset) << 8)
                                + (u64::from(lane.byte_width) << 16),
                        );
                        let delta = self
                            .fb
                            .insert_inst(Mul::new(self.module.inst_set(), flag, delta), Type::I256);
                        projected_layout = self.fb.insert_inst(
                            Add::new(self.module.inst_set(), projected_layout, delta),
                            Type::I256,
                        );
                    }
                }
                let native = self.native_field_offset(base, field.0 as usize)?;
                (
                    self.memory_layout_offset(
                        reference.layout,
                        raw.field_offset(base, *field)?,
                        native,
                        placement.offset,
                    ),
                    class,
                )
            }
            ResolvedPlaceElem::Index { index, class } => {
                let index = self.checked_index_value(base, index)?;
                let ty = self.module.ty_for_class(class)?;
                let native = self.fb.module_builder.ctx.size_of(ty).map_err(|err| {
                    LowerError::Unsupported(format!(
                        "unrepresentable native memory layout: {err:?}"
                    ))
                })? as u64;
                let stride = self.memory_layout_offset(
                    reference.layout,
                    raw.index_stride(base)?,
                    native,
                    words.index_stride(base)?,
                );
                (
                    self.fb
                        .insert_inst(Mul::new(self.module.inst_set(), index, stride), Type::I256),
                    class,
                )
            }
            ResolvedPlaceElem::VariantField {
                variant,
                field,
                class,
            } => {
                let native = self.native_variant_field_offset(*variant, field.0 as usize)?;
                (
                    self.memory_layout_offset(
                        reference.layout,
                        raw.variant_field_offset(*variant, *field)?,
                        native,
                        words.variant_field_offset(*variant, *field)?,
                    ),
                    class,
                )
            }
            ResolvedPlaceElem::Deref { .. } => {
                return Err(LowerError::Internal(
                    "dereference requires loading the stored carrier".into(),
                ));
            }
        };
        let addr = self.fb.insert_inst(
            Add::new(self.module.inst_set(), reference.addr, offset),
            Type::I256,
        );
        Ok(Lowered::Value((
            MemoryReference {
                addr,
                layout: projected_layout,
            },
            class.clone(),
        )))
    }
}
