use common::layout::enum_tag_bits;
use cranelift_entity::{EntityRef, entity_impl};
use hir::analysis::ty::AddressSpace;
use hir::analysis::{
    semantic::{FieldIndex, SemOrigin, SemanticInstance},
    ty::ty_def::TyId,
};
use hir::hir_def::{BinOp, Contract, Func, TopLevelMod, UnOp};
use hir::projection::IndexSource;
use hir::semantic::{ContractFieldId, ProviderBinding};
use ruint::aliases::U256;
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    instance::{RuntimeInstance, RuntimeInstanceKey},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeClass<'db> {
    Scalar(ScalarClass<'db>),
    AggregateValue {
        layout: LayoutId<'db>,
    },
    Ref {
        pointee: Box<RuntimeClass<'db>>,
        kind: RefKind<'db>,
        view: RefView<'db>,
    },
    RawAddr {
        space: AddressSpace,
        pointee: Option<RawPointeeId<'db>>,
    },
}

/// The target of a raw address.
#[salsa::interned]
#[derive(Debug)]
pub struct RawPointeeId<'db> {
    pub target: RuntimeClass<'db>,
}

impl<'db> RuntimeClass<'db> {
    pub fn is_transport(&self) -> bool {
        matches!(self, Self::Ref { .. } | Self::RawAddr { .. })
    }

    /// Whether dereferencing this transport yields `target`. An opaque raw
    /// address and a non-transport target nothing.
    pub fn targets(&self, db: &'db dyn MirDb, target: &RuntimeClass<'db>) -> bool {
        match self {
            Self::Ref { pointee, .. } => **pointee == *target,
            Self::RawAddr {
                pointee: Some(pointee),
                ..
            } => pointee.target(db) == target,
            Self::RawAddr { pointee: None, .. } | Self::Scalar(_) | Self::AggregateValue { .. } => {
                false
            }
        }
    }

    pub fn contains_ref(&self, db: &'db dyn MirDb) -> bool {
        match self {
            Self::Scalar(_) | Self::RawAddr { .. } => false,
            Self::Ref { .. } => true,
            Self::AggregateValue { layout } => match layout.data(db) {
                Layout::Struct(layout) => layout.fields.iter().any(|field| field.contains_ref(db)),
                Layout::Array(layout) => layout.elem.contains_ref(db),
                Layout::Enum(layout) => layout
                    .variants
                    .iter()
                    .any(|variant| variant.fields.iter().any(|field| field.contains_ref(db))),
            },
        }
    }

    pub fn contains_transport(&self, db: &'db dyn MirDb) -> bool {
        match self {
            Self::Scalar(_) => false,
            Self::Ref { .. } | Self::RawAddr { .. } => true,
            Self::AggregateValue { layout } => match layout.data(db) {
                Layout::Struct(layout) => layout
                    .fields
                    .iter()
                    .any(|field| field.contains_transport(db)),
                Layout::Array(layout) => layout.elem.contains_transport(db),
                Layout::Enum(layout) => layout.variants.iter().any(|variant| {
                    variant
                        .fields
                        .iter()
                        .any(|field| field.contains_transport(db))
                }),
            },
        }
    }

    pub fn is_runtime_zst(&self, db: &'db dyn MirDb) -> bool {
        match self {
            Self::Scalar(_) | Self::Ref { .. } | Self::RawAddr { .. } => false,
            Self::AggregateValue { layout } => match layout.data(db) {
                Layout::Struct(layout) => {
                    layout.fields.iter().all(|field| field.is_runtime_zst(db))
                }
                Layout::Array(layout) => layout.len == 0 || layout.elem.is_runtime_zst(db),
                Layout::Enum(_) => false,
            },
        }
    }

    pub fn const_ref(layout: LayoutId<'db>) -> Self {
        Self::Ref {
            pointee: Box::new(Self::AggregateValue { layout }),
            kind: RefKind::Const,
            view: RefView::Whole,
        }
    }

    pub fn object_ref(layout: LayoutId<'db>) -> Self {
        Self::Ref {
            pointee: Box::new(Self::AggregateValue { layout }),
            kind: RefKind::Object,
            view: RefView::Whole,
        }
    }

    pub fn provider_ref(
        layout: LayoutId<'db>,
        provider_ty: TyId<'db>,
        space: AddressSpace,
    ) -> Self {
        Self::Ref {
            pointee: Box::new(Self::AggregateValue { layout }),
            kind: RefKind::Provider { provider_ty, space },
            view: RefView::Whole,
        }
    }

    pub fn raw_addr(db: &'db dyn MirDb, space: AddressSpace, pointee: Self) -> Self {
        Self::RawAddr {
            space,
            pointee: Some(RawPointeeId::new(db, pointee)),
        }
    }

    pub fn opaque_raw_addr(space: AddressSpace) -> Self {
        Self::RawAddr {
            space,
            pointee: None,
        }
    }

    pub fn aggregate_layout(&self) -> Option<LayoutId<'db>> {
        match self {
            RuntimeClass::Scalar(_) => None,
            RuntimeClass::AggregateValue { layout } => Some(*layout),
            RuntimeClass::Ref { pointee, .. } => pointee.aggregate_layout(),
            RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn aggregate_value_class(&self) -> Option<Self> {
        self.aggregate_layout()
            .map(|layout| Self::AggregateValue { layout })
    }

    pub fn ref_pointee(&self) -> Option<&RuntimeClass<'db>> {
        match self {
            RuntimeClass::Ref { pointee, .. } => Some(pointee),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn raw_pointee(&self) -> Option<RawPointeeId<'db>> {
        match self {
            RuntimeClass::RawAddr { pointee, .. } => *pointee,
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref { .. } => None,
        }
    }

    /// The class reached by dereferencing this transport, resolving a raw
    /// target on demand. `None` for non-transports and opaque raw addresses.
    pub fn deref_target(&self, db: &'db dyn MirDb) -> Option<RuntimeClass<'db>> {
        match self {
            RuntimeClass::Ref { pointee, .. } => Some((**pointee).clone()),
            RuntimeClass::RawAddr {
                pointee: Some(pointee),
                ..
            } => Some(pointee.target(db).clone()),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { pointee: None, .. } => None,
        }
    }

    pub fn as_ref_kind(&self) -> Option<&RefKind<'db>> {
        match self {
            RuntimeClass::Ref { kind, .. } => Some(kind),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn address_space(&self) -> Option<AddressSpace> {
        match self {
            RuntimeClass::Ref {
                kind: RefKind::Provider { space, .. },
                ..
            }
            | RuntimeClass::RawAddr { space, .. } => Some(*space),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref {
                kind: RefKind::Const | RefKind::Object | RefKind::Native,
                ..
            } => None,
        }
    }

    pub fn is_signed_scalar(&self) -> bool {
        matches!(self, Self::Scalar(scalar) if scalar.is_signed_int())
    }

    pub fn array_len(&self, db: &'db dyn MirDb) -> Option<u64> {
        match self {
            RuntimeClass::AggregateValue { layout } => match layout.data(db) {
                Layout::Array(data) => Some(data.len),
                Layout::Struct(_) | Layout::Enum(_) => None,
            },
            RuntimeClass::Ref { pointee, .. } => pointee.array_len(db),
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn is_zero_sized(&self, db: &'db dyn MirDb) -> bool {
        match self {
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                false
            }
            RuntimeClass::AggregateValue { layout } => match layout.data(db) {
                Layout::Struct(data) => {
                    !data.pinned && data.fields.iter().all(|field| field.is_zero_sized(db))
                }
                Layout::Array(data) => data.len == 0 || data.elem.is_zero_sized(db),
                Layout::Enum(_) => false,
            },
        }
    }

    pub fn shares_runtime_rep_with(&self, db: &'db dyn MirDb, desired: &RuntimeClass<'db>) -> bool {
        match (self, desired) {
            (RuntimeClass::Scalar(actual), RuntimeClass::Scalar(desired)) => actual == desired,
            (
                RuntimeClass::AggregateValue { layout: actual },
                RuntimeClass::AggregateValue { layout: desired },
            ) => layouts_share_runtime_rep(db, *actual, *desired),
            (
                RuntimeClass::Ref {
                    pointee: actual_pointee,
                    kind: actual_kind,
                    view: actual_view,
                },
                RuntimeClass::Ref {
                    pointee: desired_pointee,
                    kind: desired_kind,
                    view: desired_view,
                },
            ) => {
                actual_view == desired_view
                    && ref_kinds_share_runtime_rep(actual_kind, desired_kind)
                    && actual_pointee.shares_runtime_rep_with(db, desired_pointee)
            }
            (
                RuntimeClass::RawAddr {
                    space: actual_space,
                    ..
                },
                RuntimeClass::RawAddr {
                    space: desired_space,
                    ..
                },
            ) => actual_space == desired_space,
            (
                RuntimeClass::Scalar(_),
                RuntimeClass::AggregateValue { .. }
                | RuntimeClass::Ref { .. }
                | RuntimeClass::RawAddr { .. },
            )
            | (
                RuntimeClass::AggregateValue { .. },
                RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
            )
            | (
                RuntimeClass::Ref { .. },
                RuntimeClass::Scalar(_)
                | RuntimeClass::AggregateValue { .. }
                | RuntimeClass::RawAddr { .. },
            )
            | (
                RuntimeClass::RawAddr { .. },
                RuntimeClass::Scalar(_)
                | RuntimeClass::AggregateValue { .. }
                | RuntimeClass::Ref { .. },
            ) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RefKind<'db> {
    /// An address/layout descriptor for a native reference stored in a typed slot.
    /// Unlike static views, this representation preserves any supported referent.
    Native,
    Const,
    Object,
    Provider {
        provider_ty: TyId<'db>,
        space: AddressSpace,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RefView<'db> {
    Whole,
    EnumVariant(VariantId<'db>),
}

/// The bytes of a storage word that hold a packed scalar struct field, a
/// lane, counted from the low-order end (see
/// `common::layout::storage_fields_layout`). A lane has no address.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct StorageLaneView {
    pub byte_offset: u8,
    pub byte_width: u8,
    /// Whether other fields share the word, so a write must preserve it.
    pub shared: bool,
}

impl StorageLaneView {
    pub fn lane(self) -> common::layout::StorageLane {
        common::layout::StorageLane {
            byte_offset: u32::from(self.byte_offset),
            byte_width: u32::from(self.byte_width),
        }
    }
}

fn layouts_share_runtime_rep<'db>(
    db: &'db dyn MirDb,
    actual: LayoutId<'db>,
    desired: LayoutId<'db>,
) -> bool {
    match (actual.data(db), desired.data(db)) {
        (Layout::Struct(actual), Layout::Struct(desired)) => {
            actual.fields.len() == desired.fields.len()
                && actual
                    .fields
                    .iter()
                    .zip(desired.fields.iter())
                    .all(|(actual, desired)| actual.shares_runtime_rep_with(db, desired))
        }
        (Layout::Array(actual), Layout::Array(desired)) => {
            actual.len == desired.len && actual.elem.shares_runtime_rep_with(db, &desired.elem)
        }
        (Layout::Enum(actual), Layout::Enum(desired)) => {
            actual.tag.repr == desired.tag.repr
                && actual.variants.len() == desired.variants.len()
                && actual
                    .variants
                    .iter()
                    .zip(desired.variants.iter())
                    .all(|(actual, desired)| {
                        actual.fields.len() == desired.fields.len()
                            && actual.fields.iter().zip(desired.fields.iter()).all(
                                |(actual, desired)| actual.shares_runtime_rep_with(db, desired),
                            )
                    })
        }
        (Layout::Struct(_), Layout::Array(_) | Layout::Enum(_))
        | (Layout::Array(_), Layout::Struct(_) | Layout::Enum(_))
        | (Layout::Enum(_), Layout::Struct(_) | Layout::Array(_)) => false,
    }
}

fn ref_kinds_share_runtime_rep<'db>(actual: &RefKind<'db>, desired: &RefKind<'db>) -> bool {
    match (actual, desired) {
        (RefKind::Native, RefKind::Native)
        | (RefKind::Const, RefKind::Const)
        | (RefKind::Object, RefKind::Object) => true,
        (
            RefKind::Object,
            RefKind::Provider {
                space: AddressSpace::Memory,
                ..
            },
        )
        | (
            RefKind::Provider {
                space: AddressSpace::Memory,
                ..
            },
            RefKind::Object,
        ) => true,
        (RefKind::Provider { space: actual, .. }, RefKind::Provider { space: desired, .. }) => {
            actual == desired
        }
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeCarrier<'db> {
    Erased,
    Value(RuntimeClass<'db>),
}

impl<'db> RuntimeCarrier<'db> {
    pub fn value_class(&self) -> Option<&RuntimeClass<'db>> {
        match self {
            RuntimeCarrier::Erased => None,
            RuntimeCarrier::Value(class) => Some(class),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ScalarClass<'db> {
    pub repr: ScalarRepr,
    pub role: ScalarRole<'db>,
}

impl ScalarClass<'_> {
    pub fn is_signed_int(&self) -> bool {
        matches!(self.repr, ScalarRepr::Int { signed: true, .. })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ScalarRepr {
    Bool,
    Int { bits: u16, signed: bool },
    FixedBytes { len: u16 },
    Address { bits: u16 },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ScalarRole<'db> {
    Plain,
    EnumTag {
        enum_layout: LayoutId<'db>,
    },
    /// A memory address, which native targets hold as a pointer.
    Pointer,
}

#[salsa::interned]
#[derive(Debug)]
pub struct LayoutId<'db> {
    pub key: LayoutKey<'db>,
}

impl<'db> LayoutId<'db> {
    pub fn data(self, db: &'db dyn MirDb) -> Layout<'db> {
        match self.key(db) {
            LayoutKey::Struct(layout) => Layout::Struct(layout.clone()),
            LayoutKey::Array(layout) => Layout::Array(layout.clone()),
            LayoutKey::Enum(layout) => Layout::Enum(EnumLayout {
                tag: ScalarClass {
                    repr: enum_tag_repr(layout.variants.len()),
                    role: ScalarRole::EnumTag { enum_layout: self },
                },
                variants: layout.variants.clone(),
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum LayoutKey<'db> {
    Struct(StructLayout<'db>),
    Array(ArrayLayout<'db>),
    Enum(EnumLayoutKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum Layout<'db> {
    Struct(StructLayout<'db>),
    Array(ArrayLayout<'db>),
    Enum(EnumLayout<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct StructLayout<'db> {
    pub fields: Box<[RuntimeClass<'db>]>,
    /// The layout of a pinned type (a `core::marker::Pinned` implementor):
    /// its root number identifies its region, so it takes at least
    /// one storage slot and is never zero-sized, even without fields.
    pub pinned: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ArrayLayout<'db> {
    pub elem: RuntimeClass<'db>,
    pub len: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct EnumLayout<'db> {
    pub tag: ScalarClass<'db>,
    pub variants: Box<[EnumVariantLayout<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct EnumLayoutKey<'db> {
    pub variants: Box<[EnumVariantLayout<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct EnumVariantLayout<'db> {
    pub fields: Box<[RuntimeClass<'db>]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct VariantId<'db> {
    pub enum_layout: LayoutId<'db>,
    pub index: u16,
}

impl<'db> VariantId<'db> {
    pub fn layout(self, db: &'db dyn MirDb) -> Option<EnumLayout<'db>> {
        match self.enum_layout.data(db) {
            Layout::Enum(layout) => Some(layout),
            Layout::Struct(_) | Layout::Array(_) => None,
        }
    }
}

fn enum_tag_repr(variant_count: usize) -> ScalarRepr {
    ScalarRepr::Int {
        bits: enum_tag_bits(variant_count),
        signed: false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ConstRegion<'db> {
    pub layout: LayoutId<'db>,
    pub value: ConstNode<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct ConstRegionId<'db> {
    #[returns(copy)]
    pub layout: LayoutId<'db>,
    pub value: ConstNode<'db>,
}

impl<'db> ConstRegionId<'db> {
    pub fn data(self, db: &'db dyn MirDb) -> ConstRegion<'db> {
        ConstRegion {
            layout: self.layout(db),
            value: self.value(db).clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ConstNode<'db> {
    Scalar(ConstScalar),
    Aggregate {
        layout: LayoutId<'db>,
        fields: Box<[ConstNode<'db>]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ConstScalar {
    Bool(bool),
    Int {
        bits: u16,
        signed: bool,
        words: Vec<u8>,
    },
    FixedBytes(Vec<u8>),
    Address {
        bits: u16,
        bytes: Vec<u8>,
    },
}

impl ConstScalar {
    pub fn fits_repr(&self, repr: ScalarRepr) -> bool {
        match (self, repr) {
            (Self::Bool(_), ScalarRepr::Bool) => true,
            (
                Self::Int { bits, signed, .. },
                ScalarRepr::Int {
                    bits: expected_bits,
                    signed: expected_signed,
                },
            ) => *bits == expected_bits && *signed == expected_signed,
            (Self::FixedBytes(bytes), ScalarRepr::FixedBytes { len }) => {
                bytes.len() <= usize::from(len)
            }
            (
                Self::Address { bits, .. },
                ScalarRepr::Address {
                    bits: expected_bits,
                },
            ) => *bits == expected_bits,
            (
                Self::Bool(_) | Self::Int { .. } | Self::FixedBytes(_) | Self::Address { .. },
                ScalarRepr::Bool
                | ScalarRepr::Int { .. }
                | ScalarRepr::FixedBytes { .. }
                | ScalarRepr::Address { .. },
            ) => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct RLocalId(u32);
entity_impl!(RLocalId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct RBlockId(u32);
entity_impl!(RBlockId);

pub type RValueId = RLocalId;

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeBody<'db> {
    pub owner: RuntimeInstance<'db>,
    pub key: RuntimeInstanceKey<'db>,
    pub signature: RuntimeInterfaceSignature<'db>,
    pub provider_bindings: Vec<RuntimeProviderBinding<'db>>,
    pub locals: Vec<RLocal<'db>>,
    pub blocks: Vec<RBlock<'db>>,
    pub stmt_origins: Vec<Vec<SemOrigin<'db>>>,
    pub terminator_origins: Vec<SemOrigin<'db>>,
}

impl<'db> RuntimeBody<'db> {
    /// Each block's predecessors.
    pub fn predecessors(&self) -> Vec<Vec<RBlockId>> {
        let mut predecessors = vec![Vec::new(); self.blocks.len()];
        for (block, data) in self.blocks.iter().enumerate() {
            for successor in data.terminator.successors() {
                predecessors[successor.index()].push(RBlockId::from_u32(block as u32));
            }
        }
        predecessors
    }

    /// Whether `value` holds `variant` on entry to `block`: the block's one
    /// predecessor branches to it, and only to it, on an `EnumIsVariant` test
    /// of `value` that it does not reassign after.
    pub fn variant_known_on_entry(
        &self,
        predecessors: &[Vec<RBlockId>],
        block: RBlockId,
        value: RValueId,
        variant: VariantId<'db>,
    ) -> bool {
        let [pred] = predecessors[block.index()].as_slice() else {
            return false;
        };
        let pred = &self.blocks[pred.index()];
        let RTerminator::Branch {
            cond,
            then_bb,
            else_bb,
        } = pred.terminator
        else {
            return false;
        };
        let assigns =
            |stmt: &RStmt<'db>, local| matches!(stmt, RStmt::Assign { dst, .. } if *dst == local);
        let Some(test) = pred.stmts.iter().rposition(|stmt| assigns(stmt, cond)) else {
            return false;
        };
        then_bb == block
            && else_bb != block
            && matches!(
                &pred.stmts[test],
                RStmt::Assign {
                    expr: RExpr::EnumIsVariant {
                        value: tested,
                        variant: tested_variant,
                    },
                    ..
                } if *tested == value && *tested_variant == variant
            )
            && !pred.stmts[test + 1..]
                .iter()
                .any(|stmt| assigns(stmt, value))
    }

    /// Drops the variant assertions a branch into their block already proves.
    /// Sonatina proves the extracts they guard from the branch, and an
    /// assertion would keep the tested enum value alive.
    pub fn elide_proven_variant_asserts(&mut self) {
        let predecessors = self.predecessors();
        for block in 0..self.blocks.len() {
            let block_id = RBlockId::from_u32(block as u32);
            let mut assigned = Vec::new();
            let keep: Vec<bool> = self.blocks[block]
                .stmts
                .iter()
                .map(|stmt| match stmt {
                    RStmt::EnumAssertVariant { value, variant } => {
                        assigned.contains(value)
                            || !self.variant_known_on_entry(
                                &predecessors,
                                block_id,
                                *value,
                                *variant,
                            )
                    }
                    RStmt::Assign { dst, .. } => {
                        assigned.push(*dst);
                        true
                    }
                    _ => true,
                })
                .collect();
            let mut keep_stmt = keep.iter().copied();
            self.blocks[block]
                .stmts
                .retain(|_| keep_stmt.next().unwrap_or(true));
            let mut keep_origin = keep.iter().copied();
            self.stmt_origins[block].retain(|_| keep_origin.next().unwrap_or(true));
        }
    }

    pub fn local(&self, id: RLocalId) -> Option<&RLocal<'db>> {
        self.locals.get(id.index())
    }

    pub fn block(&self, id: RBlockId) -> Option<&RBlock<'db>> {
        self.blocks.get(id.index())
    }

    pub fn value_class(&self, value: RValueId) -> Option<&RuntimeClass<'db>> {
        match &self.local(value)?.carrier {
            RuntimeCarrier::Erased => None,
            RuntimeCarrier::Value(class) => Some(class),
        }
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct LoweredRuntimeBody<'db> {
    pub body: RuntimeBody<'db>,
    pub direct_callees: Vec<RuntimeCallEdge<'db>>,
    pub referenced_const_regions: Vec<ConstRegionId<'db>>,
    pub referenced_code_regions: Vec<RuntimeCodeRegion<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RLocal<'db> {
    pub semantic_ty: TyId<'db>,
    pub carrier: RuntimeCarrier<'db>,
    pub root: RuntimeLocalRoot<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeLocalRoot<'db> {
    None,
    Slot(RuntimeClass<'db>),
    Ref(RuntimeClass<'db>),
    Ptr {
        space: AddressSpace,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct RuntimeProviderBindingId(u32);
entity_impl!(RuntimeProviderBindingId);

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeProviderBinding<'db> {
    pub provider: ProviderBinding<'db>,
    pub value: RLocalId,
    pub provider_class: RuntimeClass<'db>,
    pub place_class: RuntimeClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeInterfaceSignature<'db> {
    pub params: Vec<RuntimeParam<'db>>,
    pub ret: Option<RuntimeClass<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeExitBehavior {
    MayReturn,
    NeverReturns,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeParam<'db> {
    pub local: RLocalId,
    pub class: RuntimeClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RBlock<'db> {
    pub stmts: Vec<RStmt<'db>>,
    pub terminator: RTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeCallEdge<'db> {
    pub callee: RuntimeInstance<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeCodeRegion<'db> {
    #[returns(copy)]
    pub key: RuntimeCodeRegionKey<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeCodeRegionKey<'db> {
    ContractInit { contract: Contract<'db> },
    ContractRuntime { contract: Contract<'db> },
    ManualContractRoot { func: Func<'db> },
}

#[salsa::interned]
#[derive(Debug)]
pub struct ResolvedCodeRegion<'db> {
    #[returns(copy)]
    pub region: RuntimeCodeRegion<'db>,
    pub symbol: String,
    pub source: RuntimeSectionRef,
    #[returns(copy)]
    pub root: RuntimeFunction<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimePackage<'db> {
    #[returns(copy)]
    pub top_mod: TopLevelMod<'db>,
    pub functions: Vec<RuntimeFunction<'db>>,
    #[returns(copy)]
    pub plan: RuntimePackagePlan<'db>,
}

impl<'db> RuntimePackage<'db> {
    pub fn objects(self, db: &'db dyn MirDb) -> &'db [RuntimeObject<'db>] {
        self.plan(db).objects(db)
    }

    pub fn const_regions(self, db: &'db dyn MirDb) -> &'db [ConstRegionId<'db>] {
        self.plan(db).const_regions(db)
    }

    pub fn code_regions(self, db: &'db dyn MirDb) -> &'db [ResolvedCodeRegion<'db>] {
        self.plan(db).code_regions(db)
    }

    pub fn root_objects(self, db: &'db dyn MirDb) -> &'db [RuntimeObject<'db>] {
        self.plan(db).root_objects(db)
    }

    pub fn primary_object(self, db: &'db dyn MirDb) -> Option<RuntimeObject<'db>> {
        self.plan(db).primary_object(db)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimePackagePlan<'db> {
    pub objects: Vec<RuntimeObject<'db>>,
    pub const_regions: Vec<ConstRegionId<'db>>,
    pub code_regions: Vec<ResolvedCodeRegion<'db>>,
    pub root_objects: Vec<RuntimeObject<'db>>,
    #[returns(copy)]
    pub primary_object: Option<RuntimeObject<'db>>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeFunction<'db> {
    #[returns(copy)]
    pub instance: RuntimeInstance<'db>,
    pub symbol: String,
    #[returns(copy)]
    pub linkage: RuntimeLinkage,
    #[returns(copy)]
    pub inline_hint: RuntimeInlineHint,
    pub owner: RuntimeFunctionOwner<'db>,
    pub referenced_const_regions: Vec<ConstRegionId<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeLinkage {
    Private,
    Internal,
    External,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeInlineHint {
    Auto,
    Hint,
    Always,
    Never,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeFunctionOwner<'db> {
    Semantic(SemanticInstance<'db>),
    Synthetic(RuntimeSyntheticSpec<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeSyntheticSpec<'db> {
    MainRoot {
        callee: RuntimeInstance<'db>,
        entry_args: EntrySemanticArgsPlan<'db>,
    },
    TestRoot {
        name: String,
        callee: RuntimeInstance<'db>,
        entry_args: EntrySemanticArgsPlan<'db>,
    },
    ManualContractRoot {
        func: Func<'db>,
        callee: RuntimeInstance<'db>,
        entry_args: EntrySemanticArgsPlan<'db>,
    },
    ContractInitAbi {
        plan: ContractInitAbiPlan<'db>,
    },
    ContractRecvAbi {
        plan: ContractRecvAbiPlan<'db>,
    },
    ContractInitRoot {
        contract: Contract<'db>,
        init_abi: RuntimeInstance<'db>,
        runtime_region: RuntimeCodeRegion<'db>,
    },
    ContractRuntimeRoot {
        contract: Contract<'db>,
        dispatch: Box<[DispatchArm<'db>]>,
        default: DispatchDefault<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ContractInitAbiPlan<'db> {
    pub contract: Contract<'db>,
    pub payable: bool,
    pub user_init: Option<RuntimeInstance<'db>>,
    pub entry_args: EntrySemanticArgsPlan<'db>,
    pub init_args: InitArgsPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ContractRecvAbiPlan<'db> {
    pub contract: Contract<'db>,
    pub selector: Option<u32>,
    pub payable: bool,
    pub user_recv: RuntimeInstance<'db>,
    pub entry_args: EntrySemanticArgsPlan<'db>,
    pub input: RuntimeInputPlan<'db>,
    pub ret: RuntimeReturnPlan<'db>,
}

/// Runtime arguments that a synthetic boundary supplies on behalf of a
/// semantic owner: its physical effect carriers.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct EntrySemanticArgsPlan<'db> {
    pub effects: Box<[EntryEffectArgPlan<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum EntryEffectArgPlan<'db> {
    ContractField(ContractFieldBinding<'db>),
    TargetRootProvider(TargetRootProviderBinding<'db>),
}

/// Location of a contract field within its backing address space.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ContractFieldSlot {
    /// Word offset within the field's address space (storage, transient, or
    /// the init-time memory immutables buffer).
    Words(U256),
    /// Byte offset relative to the end of the deployed code (always
    /// negative), addressing the immutable data section appended after the
    /// runtime bytecode.
    CodeTailBytes(i128),
}

impl std::fmt::Display for ContractFieldSlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Words(words) => write!(f, "words:{words}"),
            Self::CodeTailBytes(bytes) => write!(f, "code_tail_bytes:{bytes}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ContractFieldBinding<'db> {
    /// Nominal source identity retained for package-level HIR/MIR seam checks.
    pub field: ContractFieldId<'db>,
    pub slot: ContractFieldSlot,
    pub declared_ty: TyId<'db>,
    pub class: RuntimeClass<'db>,
    pub kind: RefKind<'db>,
    pub init_immutable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct TargetRootProviderBinding<'db> {
    pub declared_ty: TyId<'db>,
    pub class: RuntimeClass<'db>,
    pub materialization: TargetRootProviderMaterialization<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum TargetRootProviderMaterialization<'db> {
    MemoryObject { layout: LayoutId<'db> },
    MemoryRawAddr { layout: LayoutId<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeBoundarySpec<'db> {
    ExactTransport(RuntimeClass<'db>),
    ExactShape(RuntimeClass<'db>),
    BorrowLike {
        pointee: RuntimeClass<'db>,
        access: BorrowAccess,
        allow: BorrowTransportSet,
    },
}

impl<'db> RuntimeBoundarySpec<'db> {
    pub fn default_exact_boundary_for_class(class: RuntimeClass<'db>) -> Self {
        if class.is_transport() {
            Self::ExactShape(class)
        } else {
            Self::ExactTransport(class)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeParamPlan<'db> {
    Erased,
    Boundary(RuntimeBoundarySpec<'db>),
    ReadOnlyView {
        value: RuntimeClass<'db>,
        borrow: RuntimeBoundarySpec<'db>,
    },
    PassActual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum BorrowAccess {
    ReadOnly,
    ReadWrite,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct BorrowTransportSet {
    pub allow_object: bool,
    pub allow_const: bool,
    pub provider_spaces: Box<[AddressSpace]>,
    pub allow_raw_addr: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum InitArgsPlan<'db> {
    None,
    DecodeInitTail {
        tuple_ty: TyId<'db>,
        decode_fn: RuntimeInstance<'db>,
        projected_fields: Box<[u32]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeInputPlan<'db> {
    None,
    DecodeHostPayload {
        msg_ty: TyId<'db>,
        host: TargetRootProviderBinding<'db>,
        decode_args_fn: RuntimeInstance<'db>,
        projected_fields: Box<[u32]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeReturnPlan<'db> {
    Unit,
    Value {
        ty: TyId<'db>,
        host: TargetRootProviderBinding<'db>,
        return_value: RuntimeInstance<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct DispatchArm<'db> {
    pub selector: u32,
    pub wrapper: RuntimeInstance<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum DispatchDefault<'db> {
    RevertEmpty,
    Call { wrapper: RuntimeInstance<'db> },
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeObject<'db> {
    pub name: String,
    pub sections: Vec<RuntimeSection<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeSection<'db> {
    pub name: RuntimeSectionName,
    pub entry: RuntimeFunction<'db>,
    pub embeds: Vec<RuntimeEmbed>,
    pub const_regions: Vec<ConstRegionId<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeSectionName {
    Init,
    Runtime,
    Main,
    Test(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimeEmbed {
    pub source: RuntimeSectionRef,
    pub as_symbol: String,
}

/// Refers to a section by the stable `(object name, section name)` pair so the
/// reference stays valid when objects are rebuilt (re-interned).
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeSectionRef {
    Local {
        object: String,
        section: RuntimeSectionName,
    },
    External {
        object: String,
        section: RuntimeSectionName,
    },
}

impl RuntimeSectionRef {
    pub fn object(&self) -> &str {
        match self {
            RuntimeSectionRef::Local { object, .. }
            | RuntimeSectionRef::External { object, .. } => object,
        }
    }

    pub fn section(&self) -> &RuntimeSectionName {
        match self {
            RuntimeSectionRef::Local { section, .. }
            | RuntimeSectionRef::External { section, .. } => section,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum PlaceRoot<'db> {
    Slot(RLocalId),
    Ref(RValueId),
    Provider(RuntimeProviderBindingId),
    Ptr {
        addr: RValueId,
        space: AddressSpace,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RuntimePlace<'db> {
    pub root: PlaceRoot<'db>,
    pub path: Box<[PlaceElem<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum PlaceElem<'db> {
    Field(FieldIndex),
    Index(IndexSource<RValueId>),
    VariantField {
        variant: VariantId<'db>,
        field: FieldIndex,
    },
    Deref,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ResolvedRuntimePlace<'db> {
    pub root_kind: ResolvedPlaceRootKind<'db>,
    pub result_class: RuntimeClass<'db>,
    pub path: Box<[ResolvedPlaceElem<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ResolvedPlaceRootKind<'db> {
    Slot {
        local: RLocalId,
        class: RuntimeClass<'db>,
    },
    Ref {
        value: RValueId,
        class: RuntimeClass<'db>,
    },
    Provider {
        binding: RuntimeProviderBindingId,
        value: RLocalId,
        provider_class: RuntimeClass<'db>,
        class: RuntimeClass<'db>,
    },
    Ptr {
        addr: RValueId,
        space: AddressSpace,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ResolvedPlaceElem<'db> {
    Field {
        field: FieldIndex,
        class: RuntimeClass<'db>,
    },
    Index {
        index: IndexSource<RValueId>,
        class: RuntimeClass<'db>,
    },
    VariantField {
        variant: VariantId<'db>,
        field: FieldIndex,
        class: RuntimeClass<'db>,
    },
    Deref {
        carrier_class: RuntimeClass<'db>,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeBuiltin<'db> {
    Mload {
        addr: RValueId,
    },
    Mstore {
        addr: RValueId,
        value: RValueId,
    },
    Mstore8 {
        addr: RValueId,
        value: RValueId,
    },
    Mcopy {
        dst: RValueId,
        src: RValueId,
        len: RValueId,
    },
    ZeroMem {
        dst: RValueId,
        len: RValueId,
    },
    /// Writes `bytes` into memory from `dst` on, zero-padded to whole words.
    StoreConstBytes {
        dst: RValueId,
        bytes: Box<[u8]>,
    },
    Msize,
    Sload {
        slot: RValueId,
    },
    Sstore {
        slot: RValueId,
        value: RValueId,
    },
    CallValue,
    ReturnDataSize,
    ReturnDataCopy {
        dst: RValueId,
        offset: RValueId,
        len: RValueId,
    },
    CallDataSize,
    CallDataLoad {
        offset: RValueId,
    },
    CallDataCopy {
        dst: RValueId,
        offset: RValueId,
        len: RValueId,
    },
    CodeSize,
    CodeCopy {
        dst: RValueId,
        offset: RValueId,
        len: RValueId,
    },
    ExtCodeSize {
        addr: RValueId,
    },
    ExtCodeCopy {
        addr: RValueId,
        dst: RValueId,
        offset: RValueId,
        len: RValueId,
    },
    ExtCodeHash {
        addr: RValueId,
    },
    Keccak256 {
        offset: RValueId,
        len: RValueId,
    },
    /// `keccak256` of the words, each as 32 big-endian bytes, in order: a
    /// value of the words alone, read from no memory.
    KeccakWords {
        words: Box<[RValueId]>,
    },
    AddMod {
        lhs: RValueId,
        rhs: RValueId,
        modulus: RValueId,
    },
    MulMod {
        lhs: RValueId,
        rhs: RValueId,
        modulus: RValueId,
    },
    LeadingZeros {
        value: RValueId,
    },
    /// Reverses the bytes of an integer.
    SwapBytes {
        value: RValueId,
    },
    /// Returns a scalar unchanged while hiding it from the optimizer.
    BlackBox {
        value: RValueId,
    },
    Byte {
        pos: RValueId,
        value: RValueId,
    },
    SignExtend {
        byte: RValueId,
        value: RValueId,
    },
    IntrinsicArith {
        op: IntrinsicArithBinOp,
        checked: bool,
        lhs: RValueId,
        rhs: RValueId,
        class: ScalarClass<'db>,
    },
    Saturating {
        op: SaturatingBinOp,
        lhs: RValueId,
        rhs: RValueId,
        class: ScalarClass<'db>,
    },
    Address,
    Caller,
    Origin,
    GasPrice,
    CoinBase,
    Balance {
        addr: RValueId,
    },
    Timestamp,
    Number,
    PrevRandao,
    GasLimit,
    ChainId,
    BaseFee,
    SelfBalance,
    BlockHash {
        block: RValueId,
    },
    BlobHash {
        index: RValueId,
    },
    BlobBaseFee,
    Gas,
    CurrentCodeRegionLen,
    CodeRegionOffset {
        region: RuntimeCodeRegion<'db>,
    },
    CodeRegionLen {
        region: RuntimeCodeRegion<'db>,
    },
    Malloc {
        size: RValueId,
    },
    /// Like `Malloc`, but yields a null address when the host cannot
    /// allocate.
    TryMalloc {
        size: RValueId,
    },
    /// Releases an allocation from `Malloc` or `TryMalloc`.
    Free {
        ptr: RValueId,
    },
    PtrOffsetBytes {
        ptr: RValueId,
        offset: RValueId,
    },
    Call {
        gas: RValueId,
        addr: RValueId,
        value: RValueId,
        args_offset: RValueId,
        args_len: RValueId,
        ret_offset: RValueId,
        ret_len: RValueId,
    },
    StaticCall {
        gas: RValueId,
        addr: RValueId,
        args_offset: RValueId,
        args_len: RValueId,
        ret_offset: RValueId,
        ret_len: RValueId,
    },
    DelegateCall {
        gas: RValueId,
        addr: RValueId,
        args_offset: RValueId,
        args_len: RValueId,
        ret_offset: RValueId,
        ret_len: RValueId,
    },
    Create {
        value: RValueId,
        offset: RValueId,
        len: RValueId,
    },
    Create2 {
        value: RValueId,
        offset: RValueId,
        len: RValueId,
        salt: RValueId,
    },
    Log0 {
        offset: RValueId,
        len: RValueId,
    },
    Log1 {
        offset: RValueId,
        len: RValueId,
        topic0: RValueId,
    },
    Log2 {
        offset: RValueId,
        len: RValueId,
        topic0: RValueId,
        topic1: RValueId,
    },
    Log3 {
        offset: RValueId,
        len: RValueId,
        topic0: RValueId,
        topic1: RValueId,
        topic2: RValueId,
    },
    Log4 {
        offset: RValueId,
        len: RValueId,
        topic0: RValueId,
        topic1: RValueId,
        topic2: RValueId,
        topic3: RValueId,
    },
    CallDataSelector,
    MakeContractFieldRef {
        slot: ContractFieldSlot,
        class: RuntimeClass<'db>,
        kind: RefKind<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SaturatingBinOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum IntrinsicArithBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RExpr<'db> {
    Use(RValueId),
    ConstScalar(ConstScalar),
    Placeholder {
        class: RuntimeClass<'db>,
    },
    Builtin(RuntimeBuiltin<'db>),
    Unary {
        op: UnOp,
        value: RValueId,
    },
    Binary {
        op: BinOp,
        lhs: RValueId,
        rhs: RValueId,
    },
    Cast {
        value: RValueId,
        to: ScalarClass<'db>,
    },
    ConstRef {
        region: ConstRegionId<'db>,
        layout: LayoutId<'db>,
    },
    AllocObject {
        layout: LayoutId<'db>,
    },
    MaterializeToObject {
        src: RValueId,
    },
    MaterializePlaceToObject {
        place: RuntimePlace<'db>,
    },
    NativeRef {
        value: RValueId,
    },
    ProviderRefFromRaw {
        raw: RValueId,
        provider_ty: TyId<'db>,
        space: AddressSpace,
    },
    WordToRawAddr {
        value: RValueId,
        space: AddressSpace,
    },
    ProviderRefToRaw {
        value: RValueId,
    },
    RetagRef {
        value: RValueId,
    },
    AddrOf {
        place: RuntimePlace<'db>,
    },
    Load {
        place: RuntimePlace<'db>,
    },
    AggregateExtract {
        value: RValueId,
        index: u32,
    },
    AggregateMake {
        layout: LayoutId<'db>,
        fields: Box<[RValueId]>,
    },
    Call {
        callee: RuntimeInstance<'db>,
        args: Box<[RValueId]>,
    },
    EnumMake {
        layout: LayoutId<'db>,
        variant: VariantId<'db>,
        fields: Box<[RValueId]>,
    },
    EnumTagOfValue {
        value: RValueId,
    },
    EnumIsVariant {
        value: RValueId,
        variant: VariantId<'db>,
    },
    EnumExtract {
        value: RValueId,
        variant: VariantId<'db>,
        field: FieldIndex,
    },
    EnumGetTag {
        place: RuntimePlace<'db>,
    },
    EnumAssertVariantRef {
        root: RValueId,
        variant: VariantId<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RStmt<'db> {
    Assign {
        dst: RLocalId,
        expr: RExpr<'db>,
    },
    AssertIndexInBounds {
        index: IndexSource<RValueId>,
        len: u64,
    },
    EnumAssertVariant {
        value: RValueId,
        variant: VariantId<'db>,
    },
    Store {
        dst: RuntimePlace<'db>,
        src: RValueId,
    },
    CopyInto {
        dst: RuntimePlace<'db>,
        src: RValueId,
    },
    EnumSetTag {
        root: RValueId,
        variant: VariantId<'db>,
    },
    EnumWriteVariant {
        root: RValueId,
        variant: VariantId<'db>,
        fields: Box<[RValueId]>,
    },
    /// Finishes the projection session whose call assigned `session`: the
    /// session's slide runs here once the call is inlined.
    End {
        session: RLocalId,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RTerminator<'db> {
    Goto(RBlockId),
    Branch {
        cond: RValueId,
        then_bb: RBlockId,
        else_bb: RBlockId,
    },
    SwitchScalar {
        discr: RValueId,
        cases: Box<[(ConstScalar, RBlockId)]>,
        default: RBlockId,
    },
    MatchEnumTag {
        tag: RValueId,
        enum_layout: LayoutId<'db>,
        cases: Box<[(VariantId<'db>, RBlockId)]>,
        default: Option<RBlockId>,
    },
    TerminalCall {
        callee: RuntimeInstance<'db>,
        args: Box<[RValueId]>,
    },
    ReturnData {
        offset: RValueId,
        len: RValueId,
    },
    Revert {
        offset: RValueId,
        len: RValueId,
    },
    RevertEmpty,
    /// Reverts with Solidity `Panic(uint256)` data for the word `code`.
    Panic {
        code: RValueId,
    },
    SelfDestruct {
        beneficiary: RValueId,
    },
    Trap,
    Return(Option<RValueId>),
    /// A projection grants `value` to its caller; the caller's `End` of the
    /// session resumes at `resume`, which runs the slide and returns.
    Yield {
        value: Option<RValueId>,
        resume: RBlockId,
    },
    Stop,
}

impl<'db> RuntimePlace<'db> {
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut RValueId> {
        let root = match &mut self.root {
            PlaceRoot::Slot(local) | PlaceRoot::Ref(local) | PlaceRoot::Ptr { addr: local, .. } => {
                Some(local)
            }
            PlaceRoot::Provider(_) => None,
        };
        root.into_iter()
            .chain(self.path.iter_mut().filter_map(|elem| match elem {
                PlaceElem::Index(IndexSource::Dynamic(index)) => Some(index),
                PlaceElem::Index(_)
                | PlaceElem::Field(_)
                | PlaceElem::VariantField { .. }
                | PlaceElem::Deref => None,
            }))
    }
}

impl<'db> RuntimeBuiltin<'db> {
    /// Whether running the builtin can be observed beyond its result: it
    /// writes memory or state, calls out, logs, frees, can revert, or hides
    /// its operand from the optimizer.
    pub fn has_effect(&self) -> bool {
        match self {
            Self::Mstore { .. }
            | Self::Mstore8 { .. }
            | Self::Mcopy { .. }
            | Self::ZeroMem { .. }
            | Self::StoreConstBytes { .. }
            | Self::Sstore { .. }
            | Self::ReturnDataCopy { .. }
            | Self::CallDataCopy { .. }
            | Self::CodeCopy { .. }
            | Self::ExtCodeCopy { .. }
            | Self::BlackBox { .. }
            | Self::Free { .. }
            | Self::Call { .. }
            | Self::StaticCall { .. }
            | Self::DelegateCall { .. }
            | Self::Create { .. }
            | Self::Create2 { .. }
            | Self::Log0 { .. }
            | Self::Log1 { .. }
            | Self::Log2 { .. }
            | Self::Log3 { .. }
            | Self::Log4 { .. } => true,
            Self::IntrinsicArith { checked, .. } => *checked,
            Self::Mload { .. }
            | Self::Msize
            | Self::Sload { .. }
            | Self::CallValue
            | Self::ReturnDataSize
            | Self::CallDataSize
            | Self::CallDataLoad { .. }
            | Self::CodeSize
            | Self::ExtCodeSize { .. }
            | Self::ExtCodeHash { .. }
            | Self::Keccak256 { .. }
            | Self::KeccakWords { .. }
            | Self::AddMod { .. }
            | Self::MulMod { .. }
            | Self::LeadingZeros { .. }
            | Self::SwapBytes { .. }
            | Self::Byte { .. }
            | Self::SignExtend { .. }
            | Self::Saturating { .. }
            | Self::Address
            | Self::Caller
            | Self::Origin
            | Self::GasPrice
            | Self::CoinBase
            | Self::Balance { .. }
            | Self::Timestamp
            | Self::Number
            | Self::PrevRandao
            | Self::GasLimit
            | Self::ChainId
            | Self::BaseFee
            | Self::SelfBalance
            | Self::BlockHash { .. }
            | Self::BlobHash { .. }
            | Self::BlobBaseFee
            | Self::Gas
            | Self::CurrentCodeRegionLen
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. }
            | Self::Malloc { .. }
            | Self::TryMalloc { .. }
            | Self::PtrOffsetBytes { .. }
            | Self::CallDataSelector
            | Self::MakeContractFieldRef { .. } => false,
        }
    }

    pub fn values_mut(&mut self) -> Vec<&mut RValueId> {
        match self {
            Self::Mload { addr }
            | Self::ExtCodeSize { addr }
            | Self::ExtCodeHash { addr }
            | Self::Balance { addr }
            | Self::StoreConstBytes { dst: addr, .. } => vec![addr],
            Self::Sload { slot: value }
            | Self::CallDataLoad { offset: value }
            | Self::LeadingZeros { value }
            | Self::SwapBytes { value }
            | Self::BlackBox { value }
            | Self::BlockHash { block: value }
            | Self::BlobHash { index: value }
            | Self::Malloc { size: value }
            | Self::TryMalloc { size: value }
            | Self::Free { ptr: value } => vec![value],
            Self::Mstore { addr: a, value: b }
            | Self::Mstore8 { addr: a, value: b }
            | Self::ZeroMem { dst: a, len: b }
            | Self::Sstore { slot: a, value: b }
            | Self::Keccak256 { offset: a, len: b }
            | Self::Byte { pos: a, value: b }
            | Self::SignExtend { byte: a, value: b }
            | Self::IntrinsicArith { lhs: a, rhs: b, .. }
            | Self::Saturating { lhs: a, rhs: b, .. }
            | Self::PtrOffsetBytes { ptr: a, offset: b }
            | Self::Log0 { offset: a, len: b } => vec![a, b],
            Self::Mcopy {
                dst: a,
                src: b,
                len: c,
            }
            | Self::ReturnDataCopy {
                dst: a,
                offset: b,
                len: c,
            }
            | Self::CallDataCopy {
                dst: a,
                offset: b,
                len: c,
            }
            | Self::CodeCopy {
                dst: a,
                offset: b,
                len: c,
            }
            | Self::AddMod {
                lhs: a,
                rhs: b,
                modulus: c,
            }
            | Self::MulMod {
                lhs: a,
                rhs: b,
                modulus: c,
            }
            | Self::Create {
                value: a,
                offset: b,
                len: c,
            }
            | Self::Log1 {
                offset: a,
                len: b,
                topic0: c,
            } => vec![a, b, c],
            Self::ExtCodeCopy {
                addr: a,
                dst: b,
                offset: c,
                len: d,
            }
            | Self::Create2 {
                value: a,
                offset: b,
                len: c,
                salt: d,
            }
            | Self::Log2 {
                offset: a,
                len: b,
                topic0: c,
                topic1: d,
            } => vec![a, b, c, d],
            Self::Log3 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
            } => vec![offset, len, topic0, topic1, topic2],
            Self::Log4 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
                topic3,
            } => vec![offset, len, topic0, topic1, topic2, topic3],
            Self::StaticCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            }
            | Self::DelegateCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => vec![gas, addr, args_offset, args_len, ret_offset, ret_len],
            Self::Call {
                gas,
                addr,
                value,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => vec![gas, addr, value, args_offset, args_len, ret_offset, ret_len],
            Self::KeccakWords { words } => words.iter_mut().collect(),
            Self::Msize
            | Self::CallValue
            | Self::ReturnDataSize
            | Self::CallDataSize
            | Self::CodeSize
            | Self::Address
            | Self::Caller
            | Self::Origin
            | Self::GasPrice
            | Self::CoinBase
            | Self::Timestamp
            | Self::Number
            | Self::PrevRandao
            | Self::GasLimit
            | Self::ChainId
            | Self::BaseFee
            | Self::SelfBalance
            | Self::BlobBaseFee
            | Self::Gas
            | Self::CurrentCodeRegionLen
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. }
            | Self::CallDataSelector
            | Self::MakeContractFieldRef { .. } => Vec::new(),
        }
    }
}

impl<'db> RExpr<'db> {
    /// The values the expression reads, including those in its places.
    pub fn values_mut(&mut self) -> Vec<&mut RValueId> {
        match self {
            Self::Use(value)
            | Self::Unary { value, .. }
            | Self::Cast { value, .. }
            | Self::MaterializeToObject { src: value }
            | Self::NativeRef { value }
            | Self::ProviderRefFromRaw { raw: value, .. }
            | Self::WordToRawAddr { value, .. }
            | Self::ProviderRefToRaw { value }
            | Self::RetagRef { value }
            | Self::AggregateExtract { value, .. }
            | Self::EnumTagOfValue { value }
            | Self::EnumIsVariant { value, .. }
            | Self::EnumExtract { value, .. }
            | Self::EnumAssertVariantRef { root: value, .. } => vec![value],
            Self::Binary { lhs, rhs, .. } => vec![lhs, rhs],
            Self::MaterializePlaceToObject { place }
            | Self::AddrOf { place }
            | Self::Load { place }
            | Self::EnumGetTag { place } => place.values_mut().collect(),
            Self::AggregateMake { fields, .. }
            | Self::Call { args: fields, .. }
            | Self::EnumMake { fields, .. } => fields.iter_mut().collect(),
            Self::Builtin(builtin) => builtin.values_mut(),
            Self::ConstScalar(_)
            | Self::Placeholder { .. }
            | Self::ConstRef { .. }
            | Self::AllocObject { .. } => Vec::new(),
        }
    }
}

impl<'db> RExpr<'db> {
    pub fn place_mut(&mut self) -> Option<&mut RuntimePlace<'db>> {
        match self {
            Self::MaterializePlaceToObject { place }
            | Self::AddrOf { place }
            | Self::Load { place }
            | Self::EnumGetTag { place } => Some(place),
            Self::Use(_)
            | Self::ConstScalar(_)
            | Self::Placeholder { .. }
            | Self::Builtin(_)
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::Cast { .. }
            | Self::ConstRef { .. }
            | Self::AllocObject { .. }
            | Self::MaterializeToObject { .. }
            | Self::NativeRef { .. }
            | Self::ProviderRefFromRaw { .. }
            | Self::WordToRawAddr { .. }
            | Self::ProviderRefToRaw { .. }
            | Self::RetagRef { .. }
            | Self::AggregateExtract { .. }
            | Self::AggregateMake { .. }
            | Self::Call { .. }
            | Self::EnumMake { .. }
            | Self::EnumTagOfValue { .. }
            | Self::EnumIsVariant { .. }
            | Self::EnumExtract { .. }
            | Self::EnumAssertVariantRef { .. } => None,
        }
    }
}

impl<'db> RStmt<'db> {
    pub fn place_mut(&mut self) -> Option<&mut RuntimePlace<'db>> {
        match self {
            Self::Assign { expr, .. } => expr.place_mut(),
            Self::Store { dst, .. } | Self::CopyInto { dst, .. } => Some(dst),
            Self::AssertIndexInBounds { .. }
            | Self::EnumAssertVariant { .. }
            | Self::EnumSetTag { .. }
            | Self::EnumWriteVariant { .. }
            | Self::End { .. } => None,
        }
    }

    /// The locals the statement reads or writes, including those in its places.
    pub fn locals_mut(&mut self) -> Vec<&mut RLocalId> {
        match self {
            Self::Assign { dst, expr } => {
                let mut locals = expr.values_mut();
                locals.push(dst);
                locals
            }
            Self::AssertIndexInBounds { index, .. } => match index {
                IndexSource::Dynamic(index) => vec![index],
                IndexSource::Constant(_) | IndexSource::Any => Vec::new(),
            },
            Self::EnumAssertVariant { value, .. }
            | Self::EnumSetTag { root: value, .. }
            | Self::End { session: value } => vec![value],
            Self::Store { dst, src } | Self::CopyInto { dst, src } => {
                let mut locals: Vec<_> = dst.values_mut().collect();
                locals.push(src);
                locals
            }
            Self::EnumWriteVariant { root, fields, .. } => {
                std::iter::once(root).chain(fields.iter_mut()).collect()
            }
        }
    }
}

impl<'db> RTerminator<'db> {
    pub fn values_mut(&mut self) -> Vec<&mut RValueId> {
        match self {
            Self::Branch { cond: value, .. }
            | Self::SwitchScalar { discr: value, .. }
            | Self::MatchEnumTag { tag: value, .. }
            | Self::SelfDestruct { beneficiary: value }
            | Self::Panic { code: value }
            | Self::Return(Some(value))
            | Self::Yield {
                value: Some(value), ..
            } => vec![value],
            Self::TerminalCall { args, .. } => args.iter_mut().collect(),
            Self::ReturnData { offset, len } | Self::Revert { offset, len } => vec![offset, len],
            Self::Goto(_)
            | Self::RevertEmpty
            | Self::Trap
            | Self::Return(None)
            | Self::Yield { value: None, .. }
            | Self::Stop => Vec::new(),
        }
    }

    pub fn successors(&self) -> Vec<RBlockId> {
        self.clone()
            .successors_mut()
            .into_iter()
            .map(|block| *block)
            .collect()
    }

    pub fn successors_mut(&mut self) -> Vec<&mut RBlockId> {
        match self {
            Self::Goto(target) | Self::Yield { resume: target, .. } => vec![target],
            Self::Branch {
                then_bb, else_bb, ..
            } => vec![then_bb, else_bb],
            Self::SwitchScalar { cases, default, .. } => cases
                .iter_mut()
                .map(|(_, target)| target)
                .chain(std::iter::once(default))
                .collect(),
            Self::MatchEnumTag { cases, default, .. } => cases
                .iter_mut()
                .map(|(_, target)| target)
                .chain(default.iter_mut())
                .collect(),
            Self::TerminalCall { .. }
            | Self::ReturnData { .. }
            | Self::Revert { .. }
            | Self::RevertEmpty
            | Self::Panic { .. }
            | Self::SelfDestruct { .. }
            | Self::Trap
            | Self::Return(_)
            | Self::Stop => Vec::new(),
        }
    }
}

pub trait RuntimeProgramView<'db> {
    fn interface_signature(&self, id: RuntimeInstance<'db>) -> RuntimeInterfaceSignature<'db>;
    fn exit_behavior(&self, id: RuntimeInstance<'db>) -> RuntimeExitBehavior;
    fn body(&self, id: RuntimeInstance<'db>) -> RuntimeBody<'db>;
    fn layout(&self, id: LayoutId<'db>) -> Layout<'db>;
    fn const_region(&self, id: ConstRegionId<'db>) -> ConstRegion<'db>;
    fn code_region(&self, id: RuntimeCodeRegion<'db>) -> Option<ResolvedCodeRegion<'db>>;
}

impl<'db> RuntimeProgramView<'db> for &'db dyn MirDb {
    fn interface_signature(&self, id: RuntimeInstance<'db>) -> RuntimeInterfaceSignature<'db> {
        id.interface_signature(*self).clone()
    }

    fn exit_behavior(&self, id: RuntimeInstance<'db>) -> RuntimeExitBehavior {
        id.exit_behavior(*self)
    }

    fn body(&self, id: RuntimeInstance<'db>) -> RuntimeBody<'db> {
        id.body(*self).clone()
    }

    fn layout(&self, id: LayoutId<'db>) -> Layout<'db> {
        id.data(*self)
    }

    fn const_region(&self, id: ConstRegionId<'db>) -> ConstRegion<'db> {
        id.data(*self)
    }

    fn code_region(&self, _id: RuntimeCodeRegion<'db>) -> Option<ResolvedCodeRegion<'db>> {
        None
    }
}
