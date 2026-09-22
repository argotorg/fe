use mir::{AddressSpaceKind, Layout, ResolvedPlaceElem, RuntimeClass, RuntimeMemoryLayout};
use sonatina_ir::{
    Type, ValueId,
    inst::{
        arith::{Add, Mul, Sub},
        cast::Zext,
        cmp::Eq,
        control_flow::{BrTable, Jump, Phi, Unreachable},
        data::{ConstLoad, Mload, Mstore, ObjLoad, ObjMaterializeHeap},
        logic::Or,
    },
    types::CompoundType,
};

use super::{CopySource, FunctionLowerer, LowerError, Lowered, LoweringInstSet, scalar_ty};

// The descriptor's layout also records the referent's address space. Provider
// kinds may be erased when a reference is stored inside an ordinary typed slot.
const REFERENCE_LAYOUTS: [Option<AddressSpaceKind>; 6] = [
    Some(AddressSpaceKind::Memory),
    None, // Native Sonatina object layout in memory.
    Some(AddressSpaceKind::Storage),
    Some(AddressSpaceKind::Transient),
    Some(AddressSpaceKind::Calldata),
    Some(AddressSpaceKind::Code),
];
const NATIVE_MEMORY: u64 = 1;

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

    pub(super) fn load_referent(
        &mut self,
        reference: MemoryReference,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = match class {
            RuntimeClass::Scalar(scalar) => scalar_ty(scalar),
            _ => self.module.ty_for_class(class)?,
        };
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let native = self.module.is_native_target();
        let blocks = REFERENCE_LAYOUTS
            .iter()
            .enumerate()
            .filter(|(_, space)| !native || matches!(space, None | Some(AddressSpaceKind::Memory)))
            .map(|(tag, space)| (tag, self.fb.append_block(), *space))
            .collect::<Vec<_>>();
        let cases = blocks
            .iter()
            .map(|(tag, block, _)| (self.index_value(*tag as u64), *block))
            .collect::<Vec<_>>();
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            reference.layout,
            Some(invalid),
            cases,
        ));
        let mut values = Vec::with_capacity(blocks.len());
        for (_, block, space) in blocks {
            self.fb.switch_to_block(block);
            let value = if let Some(space) = space {
                self.load_from_ptr(reference.addr, space, class)?
            } else {
                self.fb
                    .insert_inst(Mload::new(self.module.inst_set(), reference.addr, ty), ty)
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
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            reference.layout,
            Some(invalid),
            cases,
        ));
        for (_, block, space) in blocks {
            self.fb.switch_to_block(block);
            if let Some(space) = space {
                self.copy_to_ptr(reference.addr, space, class, value)?;
            } else {
                self.fb.insert_inst_no_result(Mstore::new(
                    self.module.inst_set(),
                    reference.addr,
                    value,
                    ty,
                ));
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
            CopySource::Ptr { addr, space, .. } => self.load_from_ptr(addr, space, &class)?,
        };
        self.retype_value_for_class(value, &class, target)
    }

    fn native_class_size(&mut self, class: &RuntimeClass<'db>) -> Result<u64, LowerError> {
        let ty = self.module.ty_for_class(class)?;
        let size = self
            .module
            .builder
            .ctx
            .type_layout
            .size_of(ty, &self.fb.module_builder.ctx)
            .map_err(|err| {
                LowerError::Unsupported(format!("unrepresentable native memory layout: {err:?}"))
            })?;
        u64::try_from(size)
            .map_err(|_| LowerError::Unsupported("native layout size exceeds u64".into()))
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
        let (offset, class) = match elem {
            ResolvedPlaceElem::Field { field, class } => {
                let Some(layout) = base.aggregate_layout() else {
                    return Err(LowerError::Internal(
                        "memory field requires aggregate".into(),
                    ));
                };
                let Layout::Struct(layout) = layout.data(self.module.db) else {
                    return Err(LowerError::Internal("memory field requires struct".into()));
                };
                let native = layout.fields.iter().take(field.0 as usize).try_fold(
                    0u64,
                    |offset, field| {
                        offset
                            .checked_add(self.native_class_size(field)?)
                            .ok_or_else(|| {
                                LowerError::Unsupported("native field offset overflow".into())
                            })
                    },
                )?;
                (
                    self.memory_layout_offset(
                        reference.layout,
                        raw.field_offset(base, *field)?,
                        native,
                        words.field_offset(base, *field)?,
                    ),
                    class,
                )
            }
            ResolvedPlaceElem::Index { index, class } => {
                let Lowered::Value(index) = self.checked_index_value(base, index)? else {
                    return Ok(Lowered::Terminated);
                };
                let native = self.native_class_size(class)?;
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
                let Layout::Enum(layout) = variant.enum_layout.data(self.module.db) else {
                    return Err(LowerError::Internal("variant field requires enum".into()));
                };
                // Sonatina concatenates variant payloads; raw Fe memory overlays them.
                let mut fields = layout
                    .variants
                    .iter()
                    .take(variant.index as usize)
                    .flat_map(|variant| variant.fields.iter())
                    .chain(
                        layout.variants[variant.index as usize]
                            .fields
                            .iter()
                            .take(field.0 as usize),
                    );
                let native = fields.try_fold(32u64, |offset, field| {
                    offset
                        .checked_add(self.native_class_size(field)?)
                        .ok_or_else(|| {
                            LowerError::Unsupported("native variant offset overflow".into())
                        })
                })?;
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
                layout: reference.layout,
            },
            class.clone(),
        )))
    }
}
