use common::layout::TargetDataLayout;
use hir::analysis::semantic::FieldIndex;
use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ArrayLayout, ConstNode, ConstRegionId, ConstScalar, Layout, LayoutId,
        LowerError, RuntimeClass, ScalarClass, ScalarRepr, StructLayout, VariantId,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeMemoryLayoutError {
    Overflow,
    Recursive,
    InvalidProjection(&'static str),
}

impl From<RuntimeMemoryLayoutError> for LowerError {
    fn from(error: RuntimeMemoryLayoutError) -> Self {
        match error {
            RuntimeMemoryLayoutError::Overflow => LowerError::Unsupported(
                "runtime memory layout exceeds the supported 64-bit size".to_string(),
            ),
            RuntimeMemoryLayoutError::Recursive => {
                LowerError::Unsupported("recursive runtime memory layout".to_string())
            }
            RuntimeMemoryLayoutError::InvalidProjection(projection) => LowerError::Unsupported(
                format!("invalid {projection} projection for runtime memory layout"),
            ),
        }
    }
}

#[derive(Clone, Copy)]
enum RuntimeMemoryUnit {
    Byte,
    Word,
}

#[derive(Clone, Copy)]
pub struct RuntimeMemoryLayout<'db> {
    db: &'db dyn MirDb,
    unit: RuntimeMemoryUnit,
}

impl<'db> RuntimeMemoryLayout<'db> {
    pub fn for_space(db: &'db dyn MirDb, space: AddressSpaceKind) -> Self {
        Self {
            db,
            unit: if space.is_byte_addressed() {
                RuntimeMemoryUnit::Byte
            } else {
                RuntimeMemoryUnit::Word
            },
        }
    }

    pub fn raw(db: &'db dyn MirDb) -> Self {
        Self::for_space(db, AddressSpaceKind::Memory)
    }

    pub fn class_size(self, class: &RuntimeClass<'db>) -> Result<u64, RuntimeMemoryLayoutError> {
        self.class_size_inner(class, &mut FxHashSet::default())
    }

    pub fn layout_size(self, layout: LayoutId<'db>) -> Result<u64, RuntimeMemoryLayoutError> {
        self.layout_size_inner(layout, &mut FxHashSet::default())
    }

    pub fn field_offset(
        self,
        class: &RuntimeClass<'db>,
        field: FieldIndex,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        let layout = class
            .aggregate_layout()
            .ok_or(RuntimeMemoryLayoutError::InvalidProjection("field"))?;
        let Layout::Struct(layout) = layout.data(self.db) else {
            return Err(RuntimeMemoryLayoutError::InvalidProjection("field"));
        };
        self.struct_field_offset(&layout, field.0 as usize)
    }

    pub fn struct_field_offset(
        self,
        layout: &StructLayout<'db>,
        field: usize,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        layout
            .fields
            .get(field)
            .ok_or(RuntimeMemoryLayoutError::InvalidProjection("field"))?;
        self.class_sizes_sum(&layout.fields[..field], &mut FxHashSet::default())
    }

    pub fn array_element_offset(
        self,
        layout: &ArrayLayout<'db>,
        index: u64,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        if index >= layout.len {
            return Err(RuntimeMemoryLayoutError::InvalidProjection("array element"));
        }
        self.class_size(&layout.elem)?
            .checked_mul(index)
            .ok_or(RuntimeMemoryLayoutError::Overflow)
    }

    pub fn index_stride(self, class: &RuntimeClass<'db>) -> Result<u64, RuntimeMemoryLayoutError> {
        let layout = class
            .aggregate_layout()
            .ok_or(RuntimeMemoryLayoutError::InvalidProjection("index"))?;
        let Layout::Array(layout) = layout.data(self.db) else {
            return Err(RuntimeMemoryLayoutError::InvalidProjection("index"));
        };
        self.class_size(&layout.elem)
    }

    pub fn variant_field_offset(
        self,
        variant: VariantId<'db>,
        field: FieldIndex,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        let Layout::Enum(layout) = variant.enum_layout.data(self.db) else {
            return Err(RuntimeMemoryLayoutError::InvalidProjection("variant field"));
        };
        let variant = layout
            .variants
            .get(variant.index as usize)
            .ok_or(RuntimeMemoryLayoutError::InvalidProjection("variant field"))?;
        let field = field.0 as usize;
        variant
            .fields
            .get(field)
            .ok_or(RuntimeMemoryLayoutError::InvalidProjection("variant field"))?;
        self.scalar_size(&layout.tag)
            .checked_add(self.class_sizes_sum(&variant.fields[..field], &mut FxHashSet::default())?)
            .ok_or(RuntimeMemoryLayoutError::Overflow)
    }

    fn class_size_inner(
        self,
        class: &RuntimeClass<'db>,
        visiting: &mut FxHashSet<LayoutId<'db>>,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        match class {
            RuntimeClass::Scalar(scalar) => Ok(self.scalar_size(scalar)),
            RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => Ok(self.pointer_size()),
            RuntimeClass::AggregateValue { layout } => self.layout_size_inner(*layout, visiting),
        }
    }

    fn layout_size_inner(
        self,
        layout: LayoutId<'db>,
        visiting: &mut FxHashSet<LayoutId<'db>>,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        if !visiting.insert(layout) {
            return Err(RuntimeMemoryLayoutError::Recursive);
        }
        let size = match layout.data(self.db) {
            Layout::Struct(data) => self.class_sizes_sum(&data.fields, visiting),
            Layout::Array(data) => self
                .class_size_inner(&data.elem, visiting)?
                .checked_mul(data.len)
                .ok_or(RuntimeMemoryLayoutError::Overflow),
            Layout::Enum(data) => self
                .scalar_size(&data.tag)
                .checked_add(
                    data.variants
                        .iter()
                        .map(|variant| self.class_sizes_sum(&variant.fields, visiting))
                        .try_fold(0u64, |max, size| Ok(max.max(size?)))?,
                )
                .ok_or(RuntimeMemoryLayoutError::Overflow),
        };
        visiting.remove(&layout);
        size
    }

    fn class_sizes_sum(
        self,
        classes: &[RuntimeClass<'db>],
        visiting: &mut FxHashSet<LayoutId<'db>>,
    ) -> Result<u64, RuntimeMemoryLayoutError> {
        classes.iter().try_fold(0u64, |size, class| {
            size.checked_add(self.class_size_inner(class, visiting)?)
                .ok_or(RuntimeMemoryLayoutError::Overflow)
        })
    }

    fn scalar_size(self, scalar: &ScalarClass<'_>) -> u64 {
        match self.unit {
            RuntimeMemoryUnit::Byte => scalar_raw_memory_size_bytes(scalar),
            RuntimeMemoryUnit::Word => 1,
        }
    }

    fn pointer_size(self) -> u64 {
        match self.unit {
            RuntimeMemoryUnit::Byte => 32,
            RuntimeMemoryUnit::Word => 1,
        }
    }
}

pub fn scalar_raw_memory_size_bytes(scalar: &ScalarClass<'_>) -> u64 {
    match scalar.repr {
        ScalarRepr::Bool => 1,
        ScalarRepr::Int { bits, .. } | ScalarRepr::Address { bits } => u64::from(bits.div_ceil(8)),
        ScalarRepr::FixedBytes { len } => u64::from(len),
    }
}

fn layout_size_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    target: TargetDataLayout,
) -> Result<usize, LowerError> {
    match layout.data(db) {
        Layout::Struct(data) => data.fields.iter().try_fold(0usize, |size, field| {
            checked_const_layout_add(size, memory_size_bytes_for_class(db, field, target)?)
        }),
        Layout::Array(data) => {
            let len = usize::try_from(data.len).map_err(|_| const_layout_overflow())?;
            round_up_to_word(
                checked_const_layout_mul(len, array_elem_size_bytes(db, layout, target)?)?,
                target,
            )
        }
        Layout::Enum(data) => {
            let payload = data.variants.iter().try_fold(0usize, |max, variant| {
                variant
                    .fields
                    .iter()
                    .try_fold(0usize, |size, field| {
                        checked_const_layout_add(
                            size,
                            memory_size_bytes_for_class(db, field, target)?,
                        )
                    })
                    .map(|size| max.max(size))
            })?;
            checked_const_layout_add(scalar_storage_size_bytes(data.tag.repr), payload)
        }
    }
}

fn array_elem_size_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    target: TargetDataLayout,
) -> Result<usize, LowerError> {
    let Layout::Array(data) = layout.data(db) else {
        return Err(LowerError::Unsupported(format!(
            "array element layout requested for non-array `{layout:?}`"
        )));
    };
    if packed_array_scalar_stride(&data.elem).is_some() {
        Ok(1)
    } else {
        memory_size_bytes_for_class(db, &data.elem, target)
    }
}

pub fn serialize_const_region_bytes<'db>(
    db: &'db dyn MirDb,
    region: ConstRegionId<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    let region = region.data(db);
    serialize_const_node_to_layout_bytes(db, region.layout, &region.value, target)
}

fn serialize_const_node_to_layout_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    node: &ConstNode<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    match layout.data(db) {
        Layout::Struct(data) => serialize_struct_bytes(db, layout, &data.fields, node, target),
        Layout::Array(data) => serialize_array_bytes(db, layout, &data, node, target),
        Layout::Enum(data) => serialize_enum_bytes(db, layout, &data, node, target),
    }
}

fn serialize_struct_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    fields: &[RuntimeClass<'db>],
    node: &ConstNode<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    let ConstNode::Aggregate {
        layout: node_layout,
        fields: nodes,
    } = node
    else {
        return Err(LowerError::Unsupported(format!(
            "struct const region `{layout:?}` is not an aggregate node"
        )));
    };
    if *node_layout != layout || nodes.len() != fields.len() {
        return Err(LowerError::Unsupported(format!(
            "struct const region `{layout:?}` does not match expected field shape"
        )));
    }
    let mut out = Vec::with_capacity(layout_size_bytes(db, layout, target)?);
    for (field_class, field_node) in fields.iter().zip(nodes.iter()) {
        out.extend(serialize_const_node_for_class(
            db,
            field_class,
            field_node,
            target,
        )?);
    }
    Ok(out)
}

fn serialize_array_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    data: &crate::runtime::ArrayLayout<'db>,
    node: &ConstNode<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    let ConstNode::Aggregate {
        layout: node_layout,
        fields,
    } = node
    else {
        return Err(LowerError::Unsupported(format!(
            "array const region `{layout:?}` is not an aggregate node"
        )));
    };
    if *node_layout != layout || fields.len() != data.len as usize {
        return Err(LowerError::Unsupported(format!(
            "array const region `{layout:?}` does not match expected element shape"
        )));
    }
    let stride = array_elem_size_bytes(db, layout, target)?;
    let total = layout_size_bytes(db, layout, target)?;
    let mut out = Vec::with_capacity(total);
    for elem in fields {
        out.extend(serialize_const_node_with_size(
            db, &data.elem, elem, stride, target,
        )?);
    }
    if out.len() > total {
        return Err(LowerError::Unsupported(format!(
            "array const region `{layout:?}` overflows serialized size"
        )));
    }
    out.resize(total, 0);
    Ok(out)
}

fn serialize_enum_bytes<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    data: &crate::runtime::EnumLayout<'db>,
    node: &ConstNode<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    let ConstNode::Aggregate {
        layout: node_layout,
        fields,
    } = node
    else {
        return Err(LowerError::Unsupported(format!(
            "enum const region `{layout:?}` is not an aggregate node"
        )));
    };
    if *node_layout != layout || fields.is_empty() {
        return Err(LowerError::Unsupported(format!(
            "enum const region `{layout:?}` does not contain a tag node"
        )));
    }
    let tag_size = scalar_storage_size_bytes(data.tag.repr);
    let tag = serialize_const_scalar_with_size(enum_tag_scalar(fields[0].clone())?, tag_size)?;
    let variant_index = enum_variant_index(&fields[0])?;
    let Some(variant) = data.variants.get(variant_index) else {
        return Err(LowerError::Unsupported(format!(
            "enum const region `{layout:?}` has invalid variant index {variant_index}"
        )));
    };
    if fields.len() != 1 + variant.fields.len() {
        return Err(LowerError::Unsupported(format!(
            "enum const region `{layout:?}` payload shape does not match variant {variant_index}"
        )));
    }
    let payload_capacity = layout_size_bytes(db, layout, target)?
        .checked_sub(tag_size)
        .ok_or_else(const_layout_overflow)?;
    let mut payload = Vec::with_capacity(payload_capacity);
    for (field_class, field_node) in variant.fields.iter().zip(fields.iter().skip(1)) {
        payload.extend(serialize_const_node_for_class(
            db,
            field_class,
            field_node,
            target,
        )?);
    }
    if payload.len() > payload_capacity {
        return Err(LowerError::Unsupported(format!(
            "enum const region `{layout:?}` overflows serialized payload size"
        )));
    }
    payload.resize(payload_capacity, 0);
    let mut out = tag;
    out.extend(payload);
    Ok(out)
}

fn serialize_const_node_for_class<'db>(
    db: &'db dyn MirDb,
    class: &RuntimeClass<'db>,
    node: &ConstNode<'db>,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    let size = memory_size_bytes_for_class(db, class, target)?;
    serialize_const_node_with_size(db, class, node, size, target)
}

fn serialize_const_node_with_size<'db>(
    db: &'db dyn MirDb,
    class: &RuntimeClass<'db>,
    node: &ConstNode<'db>,
    size: usize,
    target: TargetDataLayout,
) -> Result<Vec<u8>, LowerError> {
    match class {
        RuntimeClass::Scalar(class) => {
            let ConstNode::Scalar(scalar) = node else {
                return Err(LowerError::Unsupported(
                    "scalar const node expected scalar payload".to_string(),
                ));
            };
            serialize_const_scalar_with_size(repr_coerced_scalar(class.repr, scalar.clone())?, size)
        }
        RuntimeClass::Ref { .. } => Err(LowerError::Unsupported(
            "reference const nodes are not serializable runtime data".to_string(),
        )),
        RuntimeClass::AggregateValue { layout } => {
            serialize_const_node_to_layout_bytes(db, *layout, node, target)
        }
        RuntimeClass::RawAddr {
            pointee: Some(pointee),
            ..
        } if matches!(pointee.as_ref(), RuntimeClass::AggregateValue { .. }) => {
            let RuntimeClass::AggregateValue { layout } = pointee.as_ref() else {
                unreachable!()
            };
            serialize_const_node_to_layout_bytes(db, *layout, node, target)
        }
        RuntimeClass::RawAddr { .. } => {
            let ConstNode::Scalar(scalar) = node else {
                return Err(LowerError::Unsupported(
                    "raw address const node expected scalar payload".to_string(),
                ));
            };
            serialize_const_scalar_with_size(scalar.clone(), size)
        }
    }
}

fn memory_size_bytes_for_class<'db>(
    db: &'db dyn MirDb,
    class: &RuntimeClass<'db>,
    target: TargetDataLayout,
) -> Result<usize, LowerError> {
    match class {
        RuntimeClass::Scalar(class) => {
            round_up_to_word(scalar_storage_size_bytes(class.repr), target)
        }
        RuntimeClass::AggregateValue { layout } => layout_size_bytes(db, *layout, target),
        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => Ok(target.word_size_bytes),
    }
}

fn scalar_storage_size_bytes(repr: ScalarRepr) -> usize {
    match repr {
        ScalarRepr::Bool => 1,
        ScalarRepr::Int { bits, .. } | ScalarRepr::Address { bits } => bits.div_ceil(8) as usize,
        ScalarRepr::FixedBytes { len } => len as usize,
    }
}

fn packed_array_scalar_stride(class: &RuntimeClass<'_>) -> Option<usize> {
    match class {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Bool,
            ..
        })
        | RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int { bits: 8, .. },
            ..
        }) => Some(1),
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => None,
    }
}

fn round_up_to_word(size: usize, target: TargetDataLayout) -> Result<usize, LowerError> {
    if size == 0 {
        Ok(0)
    } else {
        checked_const_layout_mul(
            size.div_ceil(target.word_size_bytes),
            target.word_size_bytes,
        )
    }
}

fn checked_const_layout_add(lhs: usize, rhs: usize) -> Result<usize, LowerError> {
    lhs.checked_add(rhs).ok_or_else(const_layout_overflow)
}

fn checked_const_layout_mul(lhs: usize, rhs: usize) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs).ok_or_else(const_layout_overflow)
}

fn const_layout_overflow() -> LowerError {
    LowerError::Unsupported("constant-region layout exceeds the host size limit".to_string())
}

fn serialize_const_scalar_with_size(
    scalar: ConstScalar,
    size: usize,
) -> Result<Vec<u8>, LowerError> {
    let raw = match scalar {
        ConstScalar::Bool(flag) => vec![u8::from(flag)],
        ConstScalar::Int { words, .. } => words,
        ConstScalar::FixedBytes(bytes) => bytes,
        ConstScalar::Address { bytes, .. } => bytes,
    };
    if raw.len() > size {
        return Err(LowerError::Unsupported(format!(
            "const scalar of {} bytes does not fit into {size} bytes",
            raw.len()
        )));
    }
    let mut out = vec![0; size];
    let start = size - raw.len();
    out[start..].copy_from_slice(&raw);
    Ok(out)
}

fn repr_coerced_scalar(repr: ScalarRepr, scalar: ConstScalar) -> Result<ConstScalar, LowerError> {
    match (repr, scalar) {
        (ScalarRepr::Bool, ConstScalar::Bool(flag)) => Ok(ConstScalar::Bool(flag)),
        (
            ScalarRepr::Int { bits, signed },
            ConstScalar::Int {
                bits: scalar_bits,
                signed: scalar_signed,
                words,
            },
        ) if bits == scalar_bits && signed == scalar_signed => Ok(ConstScalar::Int {
            bits,
            signed,
            words,
        }),
        (ScalarRepr::FixedBytes { len }, ConstScalar::FixedBytes(bytes))
            if len as usize == bytes.len() =>
        {
            Ok(ConstScalar::FixedBytes(bytes))
        }
        (
            ScalarRepr::Address { bits },
            ConstScalar::Address {
                bits: scalar_bits,
                bytes,
            },
        ) if bits == scalar_bits => Ok(ConstScalar::Address { bits, bytes }),
        (expected, actual) => Err(LowerError::Unsupported(format!(
            "const scalar `{actual:?}` does not match expected repr `{expected:?}`"
        ))),
    }
}

fn enum_variant_index(node: &ConstNode<'_>) -> Result<usize, LowerError> {
    let ConstNode::Scalar(ConstScalar::Int { words, .. }) = node else {
        return Err(LowerError::Unsupported(
            "enum tag const node is not an integer scalar".to_string(),
        ));
    };
    Ok(words
        .iter()
        .fold(0usize, |acc, byte| (acc << 8) | (*byte as usize)))
}

fn enum_tag_scalar(node: ConstNode<'_>) -> Result<ConstScalar, LowerError> {
    let ConstNode::Scalar(scalar) = node else {
        return Err(LowerError::Unsupported(
            "enum tag const node is not a scalar".to_string(),
        ));
    };
    Ok(scalar)
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;

    use super::*;
    use crate::runtime::{
        ArrayLayout, EnumLayoutKey, EnumVariantLayout, LayoutKey, ScalarRole, StructLayout,
    };

    fn scalar(bits: u16) -> RuntimeClass<'static> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    #[test]
    fn raw_array_layout_size_is_checked() {
        let db = DriverDataBase::default();
        let layout = LayoutId::new(
            &db,
            LayoutKey::Array(ArrayLayout {
                elem: scalar(256),
                len: u64::MAX,
            }),
        );

        assert_eq!(
            RuntimeMemoryLayout::raw(&db).layout_size(layout),
            Err(RuntimeMemoryLayoutError::Overflow)
        );
    }

    #[test]
    fn address_space_layout_uses_bytes_or_words_consistently() {
        let db = DriverDataBase::default();
        let struct_layout = StructLayout {
            fields: vec![scalar(8), scalar(256)].into_boxed_slice(),
        };
        let array_layout = ArrayLayout {
            elem: scalar(16),
            len: 3,
        };

        let bytes = RuntimeMemoryLayout::for_space(&db, AddressSpaceKind::Memory);
        assert_eq!(bytes.struct_field_offset(&struct_layout, 1), Ok(1));
        assert_eq!(bytes.array_element_offset(&array_layout, 2), Ok(4));

        let words = RuntimeMemoryLayout::for_space(&db, AddressSpaceKind::Storage);
        assert_eq!(words.struct_field_offset(&struct_layout, 1), Ok(1));
        assert_eq!(words.array_element_offset(&array_layout, 2), Ok(2));
    }

    #[test]
    fn wide_enum_layout_uses_its_declared_tag_width() {
        let db = DriverDataBase::default();
        let mut variants = (0..257)
            .map(|_| EnumVariantLayout {
                fields: Box::new([]),
            })
            .collect::<Vec<_>>();
        variants[0].fields = vec![scalar(8)].into_boxed_slice();
        let layout = LayoutId::new(
            &db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: variants.into_boxed_slice(),
            }),
        );
        let variant = VariantId {
            enum_layout: layout,
            index: 0,
        };

        let bytes = RuntimeMemoryLayout::raw(&db);
        assert_eq!(bytes.layout_size(layout), Ok(3));
        assert_eq!(bytes.variant_field_offset(variant, FieldIndex(0)), Ok(2));

        let words = RuntimeMemoryLayout::for_space(&db, AddressSpaceKind::Storage);
        assert_eq!(words.layout_size(layout), Ok(2));
        assert_eq!(words.variant_field_offset(variant, FieldIndex(0)), Ok(1));

        let region = ConstRegionId::new(
            &db,
            layout,
            ConstNode::Aggregate {
                layout,
                fields: vec![ConstNode::Scalar(ConstScalar::Int {
                    bits: 16,
                    signed: false,
                    words: vec![1, 0],
                })]
                .into_boxed_slice(),
            },
        );
        let serialized = serialize_const_region_bytes(&db, region, common::layout::EVM_LAYOUT)
            .expect("wide enum constant should serialize");
        assert_eq!(&serialized[..2], &[1, 0]);
        assert_eq!(serialized.len(), 34);
    }
}
