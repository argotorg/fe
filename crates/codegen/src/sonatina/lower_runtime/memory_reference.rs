use common::layout::StorageLane;
use hir::analysis::semantic::FieldIndex;
use mir::{
    AddressSpaceKind, Layout, LayoutId, ResolvedPlaceElem, RuntimeClass, RuntimeMemoryLayout,
    ScalarClass, ScalarRepr, VariantId,
};
use sonatina_codegen::transform::aggregate::EnumLoweredLayout;
use sonatina_ir::{
    Type, ValueId,
    inst::{
        arith::{Add, Mul, Shl, Shr, Sub},
        cast::Zext,
        cmp::Eq,
        control_flow::{BrTable, Jump, Phi, PhiArgs, Unreachable},
        data::{ConstLoad, Mstore, ObjLoad, ObjMaterializeHeap},
        logic::{And, Not, Or},
    },
    types::CompoundType,
};

use super::{
    CopySource, EnumLoad, FunctionLowerer, LowerError, Lowered, LoweringInstSet, scalar_ty,
};

// The descriptor's layout also records the referent's address space. Provider
// kinds may be erased when a reference is stored inside an ordinary typed slot.
const REFERENCE_LAYOUTS: [Option<AddressSpaceKind>; 8] = [
    Some(AddressSpaceKind::Memory),
    None, // Native Sonatina object layout in memory.
    Some(AddressSpaceKind::Storage),
    Some(AddressSpaceKind::Transient),
    Some(AddressSpaceKind::Calldata),
    Some(AddressSpaceKind::Code),
    Some(AddressSpaceKind::Storage), // Packed field; byte offset in layout >> 8.
    Some(AddressSpaceKind::Transient),
];
const PACKED_STORAGE: u64 = 6;
const NATIVE_MEMORY: u64 = 1;

#[derive(Clone, Copy)]
pub(super) enum ReferentLayout {
    Raw(AddressSpaceKind),
    Object,
}

/// Stored references are one-word pointers to immutable address/layout
/// descriptors. Copying a reference never copies its referent.
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
        let size = self.index_value(64);
        let descriptor = self.allocate_bytes(size, Type::I256)?;
        let descriptor = self.coerce_value_to_ty(descriptor, Type::I256)?;
        let addr = self.coerce_value_to_ty(reference.addr, Type::I256)?;
        self.fb.insert_inst_no_result(Mstore::new(
            self.module.inst_set(),
            descriptor,
            addr,
            Type::I256,
        ));
        let layout_addr = self.offset_address_unscaled(descriptor, 32)?;
        self.fb.insert_inst_no_result(Mstore::new(
            self.module.inst_set(),
            layout_addr,
            reference.layout,
            Type::I256,
        ));
        Ok(descriptor)
    }

    pub(super) fn load_memory_reference(
        &mut self,
        descriptor: ValueId,
    ) -> Result<MemoryReference, LowerError> {
        let addr = self.load_word(descriptor, AddressSpaceKind::Memory)?;
        let layout_addr = self.offset_address_unscaled(descriptor, 32)?;
        let layout = self.load_word(layout_addr, AddressSpaceKind::Memory)?;
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
            ObjMaterializeHeap::new(self.module.inst_set(), object),
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
        space: AddressSpaceKind,
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

    fn reference_lane_shift(&mut self, layout: ValueId) -> ValueId {
        let eight = self.index_value(8);
        let bytes = self
            .fb
            .insert_inst(Shr::new(self.module.inst_set(), eight, layout), Type::I256);
        self.fb
            .insert_inst(Mul::new(self.module.inst_set(), bytes, eight), Type::I256)
    }

    fn reference_scalar_lane(class: &RuntimeClass<'db>) -> Option<StorageLane> {
        let RuntimeClass::Scalar(scalar) = class else {
            return None;
        };
        let byte_width = match scalar.repr {
            ScalarRepr::Bool => 1,
            ScalarRepr::Int { bits, .. } if bits < 256 => u32::from(bits / 8),
            _ => return None,
        };
        Some(StorageLane {
            byte_offset: 0,
            byte_width,
        })
    }

    pub(super) fn load_referent(
        &mut self,
        reference: MemoryReference,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = match class {
            RuntimeClass::Scalar(scalar) => scalar_ty(scalar),
            _ => self.module.ty_for_class(class)?,
        };
        let packed = match class {
            RuntimeClass::Scalar(scalar) => {
                Self::reference_scalar_lane(class).map(|lane| (lane, scalar))
            }
            _ => None,
        };
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
        packed: Option<(StorageLane, &ScalarClass<'db>)>,
        mut load: impl FnMut(&mut Self, ReferentLayout) -> Result<ValueId, LowerError>,
    ) -> Result<ValueId, LowerError> {
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let native = self.module.is_native_target();
        let blocks = REFERENCE_LAYOUTS
            .iter()
            .enumerate()
            .filter(|(tag, _)| *tag < PACKED_STORAGE as usize || packed.is_some())
            .filter(|(_, space)| !native || matches!(space, None | Some(AddressSpaceKind::Memory)))
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
                let (lane, scalar) = packed.expect("packed scalar");
                let word = self.load_word(reference.addr, space.expect("packed space"))?;
                let shift = self.reference_lane_shift(reference.layout);
                let word = self
                    .fb
                    .insert_inst(Shr::new(self.module.inst_set(), shift, word), Type::I256);
                self.extract_packed_scalar(word, lane, scalar)
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
            RuntimeClass::Scalar(scalar) => scalar_ty(scalar),
            _ => self.module.ty_for_class(class)?,
        };
        let value = self.coerce_value_to_ty(value, ty)?;
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let native = self.module.is_native_target();
        let blocks = REFERENCE_LAYOUTS
            .iter()
            .enumerate()
            .filter(|(tag, _)| {
                *tag < PACKED_STORAGE as usize || Self::reference_scalar_lane(class).is_some()
            })
            .filter(|(_, space)| {
                (!native || matches!(space, None | Some(AddressSpaceKind::Memory)))
                    && !matches!(
                        space,
                        Some(AddressSpaceKind::Code | AddressSpaceKind::Calldata)
                    )
            })
            .map(|(tag, space)| (tag, self.fb.append_block(), *space))
            .collect::<Vec<_>>();
        let cases = blocks
            .iter()
            .map(|(tag, block, _)| (self.index_value(*tag as u64), *block))
            .collect::<Vec<_>>();
        let tag = if Self::reference_scalar_lane(class).is_some() {
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
                let RuntimeClass::Scalar(scalar) = class else {
                    unreachable!()
                };
                let lane = Self::reference_scalar_lane(class).expect("packed scalar");
                let space = space.expect("packed space");
                let bits = self.packed_scalar_bits(value, lane, scalar)?;
                let shift = self.reference_lane_shift(reference.layout);
                let bits = self
                    .fb
                    .insert_inst(Shl::new(self.module.inst_set(), shift, bits), Type::I256);
                let mask = self.fb.make_imm_value(super::lane_mask(lane, false));
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

    pub(super) fn native_class_layout(
        &mut self,
        class: &RuntimeClass<'db>,
    ) -> Result<EnumLoweredLayout, LowerError> {
        let ty = self.module.ty_for_class(class)?;
        // Stored references address the original Sonatina allocation. Its enum
        // products and target padding must match the eventual legalization,
        // without rewriting types while this module is still being emitted.
        Ok(EnumLoweredLayout::new(&self.fb.module_builder.ctx, ty))
    }

    pub(super) fn referent_field_address(
        &mut self,
        addr: ValueId,
        memory: ReferentLayout,
        class: &RuntimeClass<'db>,
        index: usize,
    ) -> Result<ValueId, LowerError> {
        let offset = match memory {
            ReferentLayout::Object => self
                .native_class_layout(class)?
                .field_offset(index)
                .ok_or_else(|| {
                    LowerError::Unsupported("unrepresentable native field offset".into())
                })? as u64,
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
            ReferentLayout::Object => self
                .native_class_layout(&RuntimeClass::AggregateValue {
                    layout: variant.enum_layout,
                })?
                .variant_field_offset(variant.index as usize, field)
                .ok_or_else(|| {
                    LowerError::Unsupported("unrepresentable native variant offset".into())
                })? as u64,
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
        let words = RuntimeMemoryLayout::for_space(self.module.db, AddressSpaceKind::Storage);
        let mut projected_layout = reference.layout;
        let (offset, class) = match elem {
            ResolvedPlaceElem::Field { field, class } => {
                let placement = words.field_placement(base, *field)?;
                if let Some(lane) = placement.lane
                    && (placement.shared || lane.byte_offset != 0)
                {
                    // Memory fields remain unpacked. Only storage/transient
                    // descriptors acquire a packed tag and a byte offset.
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
                        let delta = self.index_value(4 + (u64::from(lane.byte_offset) << 8));
                        let delta = self
                            .fb
                            .insert_inst(Mul::new(self.module.inst_set(), flag, delta), Type::I256);
                        projected_layout = self.fb.insert_inst(
                            Add::new(self.module.inst_set(), projected_layout, delta),
                            Type::I256,
                        );
                    }
                }
                let native = self
                    .native_class_layout(base)?
                    .field_offset(field.0 as usize)
                    .ok_or_else(|| {
                        LowerError::Unsupported("unrepresentable native field offset".into())
                    })?;
                (
                    self.memory_layout_offset(
                        reference.layout,
                        raw.field_offset(base, *field)?,
                        native as u64,
                        placement.offset,
                    ),
                    class,
                )
            }
            ResolvedPlaceElem::Index { index, class } => {
                let Lowered::Value(index) = self.checked_index_value(base, index)? else {
                    return Ok(Lowered::Terminated);
                };
                let native = self.native_class_layout(class)?.size().map_err(|err| {
                    LowerError::Unsupported(format!(
                        "unrepresentable native memory layout: {err:?}"
                    ))
                })?;
                let stride = self.memory_layout_offset(
                    reference.layout,
                    raw.index_stride(base)?,
                    native as u64,
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
                let native = self
                    .native_class_layout(base)?
                    .variant_field_offset(variant.index as usize, field.0 as usize)
                    .ok_or_else(|| {
                        LowerError::Unsupported("unrepresentable native variant offset".into())
                    })?;
                (
                    self.memory_layout_offset(
                        reference.layout,
                        raw.variant_field_offset(*variant, *field)?,
                        native as u64,
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
