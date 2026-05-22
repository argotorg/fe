use common::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};
use cranelift_entity::{EntityRef, entity_impl};
use hir::analysis::{
    semantic::{FieldIndex, SemanticInstance},
    ty::ty_def::TyId,
};
use hir::hir_def::{BinOp, Contract, Func, TopLevelMod, UnOp};
use hir::projection::IndexSource;
use hir::semantic::ProviderBinding;
use salsa::Update;

use crate::{
    db::MirDb,
    instance::{RuntimeInstance, RuntimeInstanceKey, runtime::RuntimeInstanceSource},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum AddressSpaceKind {
    Memory,
    Storage,
    Transient,
    Calldata,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
        space: AddressSpaceKind,
        target: Option<LayoutId<'db>>,
    },
}

impl<'db> RuntimeClass<'db> {
    pub fn is_transport(&self) -> bool {
        matches!(self, Self::Ref { .. } | Self::RawAddr { .. })
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
        space: AddressSpaceKind,
    ) -> Self {
        Self::Ref {
            pointee: Box::new(Self::AggregateValue { layout }),
            kind: RefKind::Provider { provider_ty, space },
            view: RefView::Whole,
        }
    }

    pub fn aggregate_layout(&self) -> Option<LayoutId<'db>> {
        match self {
            RuntimeClass::Scalar(_) => None,
            RuntimeClass::AggregateValue { layout } => Some(*layout),
            RuntimeClass::Ref { pointee, .. } => pointee.aggregate_layout(),
            RuntimeClass::RawAddr { target, .. } => *target,
        }
    }

    pub fn pointee(&self) -> Option<&RuntimeClass<'db>> {
        match self {
            RuntimeClass::Ref { pointee, .. } => Some(pointee),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn deref_target(&self) -> Option<RuntimeClass<'db>> {
        match self {
            RuntimeClass::Ref { pointee, .. } => Some((**pointee).clone()),
            RuntimeClass::RawAddr {
                target: Some(layout),
                ..
            } => Some(RuntimeClass::AggregateValue { layout: *layout }),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { target: None, .. } => None,
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

    pub fn address_space(&self) -> Option<AddressSpaceKind> {
        match self {
            RuntimeClass::Ref {
                kind: RefKind::Provider { space, .. },
                ..
            }
            | RuntimeClass::RawAddr { space, .. } => Some(*space),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref {
                kind: RefKind::Const | RefKind::Object,
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

    pub fn index_stride_words(&self, db: &'db dyn MirDb) -> Option<u64> {
        match self {
            RuntimeClass::AggregateValue { layout } => match layout.data(db) {
                Layout::Array(data) => Some(data.elem.span_words(db)),
                Layout::Struct(_) | Layout::Enum(_) => None,
            },
            RuntimeClass::Ref { pointee, .. } => pointee.index_stride_words(db),
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => None,
        }
    }

    pub fn field_offset_words(&self, db: &'db dyn MirDb, field: FieldIndex) -> Option<u64> {
        if matches!(self, RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. }) {
            return None;
        }
        let layout = self.aggregate_layout()?;
        let Layout::Struct(data) = layout.data(db) else {
            return None;
        };
        Some(data.field_offset_words(db, field.0 as usize))
    }

    pub fn span_words(&self, db: &'db dyn MirDb) -> u64 {
        match self {
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => 1,
            RuntimeClass::AggregateValue { layout } => match layout.data(db) {
                Layout::Struct(data) => data.fields.iter().map(|field| field.span_words(db)).sum(),
                Layout::Array(data) => data.elem.span_words(db) * data.len,
                Layout::Enum(data) => {
                    1 + data
                        .variants
                        .iter()
                        .map(|variant| {
                            variant
                                .fields
                                .iter()
                                .map(|field| field.span_words(db))
                                .sum::<u64>()
                        })
                        .max()
                        .unwrap_or(0)
                }
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
                    target: actual_target,
                },
                RuntimeClass::RawAddr {
                    space: desired_space,
                    target: desired_target,
                },
            ) => {
                actual_space == desired_space
                    && match (actual_target, desired_target) {
                        (Some(actual), Some(desired)) => {
                            layouts_share_runtime_rep(db, *actual, *desired)
                        }
                        (None, None) => true,
                        (Some(_), None) | (None, Some(_)) => false,
                    }
            }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RefKind<'db> {
    Const,
    Object,
    Provider {
        provider_ty: TyId<'db>,
        space: AddressSpaceKind,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RefView<'db> {
    Whole,
    EnumVariant(VariantId<'db>),
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
            actual.tag == desired.tag
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
        (RefKind::Const, RefKind::Const) | (RefKind::Object, RefKind::Object) => true,
        (
            RefKind::Object,
            RefKind::Provider {
                space: AddressSpaceKind::Memory,
                ..
            },
        )
        | (
            RefKind::Provider {
                space: AddressSpaceKind::Memory,
                ..
            },
            RefKind::Object,
        )
        | (
            RefKind::Provider {
                space: AddressSpaceKind::Memory,
                ..
            },
            RefKind::Provider {
                space: AddressSpaceKind::Memory,
                ..
            },
        ) => true,
        (
            RefKind::Provider {
                space: actual_space,
                ..
            },
            RefKind::Provider {
                space: desired_space,
                ..
            },
        ) => actual_space == desired_space,
        (RefKind::Const, RefKind::Object | RefKind::Provider { .. })
        | (RefKind::Object | RefKind::Provider { .. }, RefKind::Const)
        | (RefKind::Object, RefKind::Provider { .. })
        | (RefKind::Provider { .. }, RefKind::Object) => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeCarrier<'db> {
    Erased,
    Value(RuntimeClass<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ScalarClass<'db> {
    pub repr: ScalarRepr,
    pub role: ScalarRole<'db>,
}

impl ScalarClass<'_> {
    pub fn is_signed_int(&self) -> bool {
        matches!(self.repr, ScalarRepr::Int { signed: true, .. })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ScalarRepr {
    Bool,
    Int { bits: u16, signed: bool },
    FixedBytes { len: u16 },
    Address { bits: u16 },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ScalarRole<'db> {
    Plain,
    EnumTag { enum_layout: LayoutId<'db> },
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
                source_ty: layout.source_ty,
                tag: ScalarClass {
                    repr: enum_tag_repr(layout.variants.len()),
                    role: ScalarRole::EnumTag { enum_layout: self },
                },
                variants: layout.variants.clone(),
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutKey<'db> {
    Struct(StructLayout<'db>),
    Array(ArrayLayout<'db>),
    Enum(EnumLayoutKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum Layout<'db> {
    Struct(StructLayout<'db>),
    Array(ArrayLayout<'db>),
    Enum(EnumLayout<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct StructLayout<'db> {
    pub source_ty: TyId<'db>,
    pub fields: Box<[RuntimeClass<'db>]>,
}

impl<'db> StructLayout<'db> {
    pub fn field_offset_words(&self, db: &'db dyn MirDb, idx: usize) -> u64 {
        self.fields
            .iter()
            .take(idx)
            .map(|field| field.span_words(db))
            .sum()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ArrayLayout<'db> {
    pub source_ty: TyId<'db>,
    pub elem: RuntimeClass<'db>,
    pub len: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct EnumLayout<'db> {
    pub source_ty: TyId<'db>,
    pub tag: ScalarClass<'db>,
    pub variants: Box<[EnumVariantLayout<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct EnumLayoutKey<'db> {
    pub source_ty: TyId<'db>,
    pub variants: Box<[EnumVariantLayout<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct EnumVariantLayout<'db> {
    pub name: String,
    pub fields: Box<[RuntimeClass<'db>]>,
}

impl<'db> EnumVariantLayout<'db> {
    pub fn payload_field_offset_words(&self, db: &'db dyn MirDb, field: FieldIndex) -> u64 {
        self.fields
            .iter()
            .take(field.0 as usize)
            .map(|field| field.span_words(db))
            .sum()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
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

    pub fn field_offset_words(self, db: &'db dyn MirDb, field: FieldIndex) -> Option<u64> {
        let layout = self.layout(db)?;
        Some(1 + layout.variants[self.index as usize].payload_field_offset_words(db, field))
    }
}

fn enum_tag_repr(variant_count: usize) -> ScalarRepr {
    let bits = if variant_count <= u8::MAX as usize + 1 {
        8
    } else if variant_count <= u16::MAX as usize + 1 {
        16
    } else {
        32
    };
    ScalarRepr::Int {
        bits,
        signed: false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ConstRegion<'db> {
    pub layout: LayoutId<'db>,
    pub value: ConstNode<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct ConstRegionId<'db> {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstNode<'db> {
    Scalar(ConstScalar),
    Aggregate {
        layout: LayoutId<'db>,
        fields: Box<[ConstNode<'db>]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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

impl IrDescribe for ConstScalar {
    fn describe<C: IrConsumer>(&self, _cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node("ConstScalar");
        match self {
            ConstScalar::Bool(b) => {
                c.field_u64(Dim::Structure, 0);
                c.field_bool(Dim::Constants, *b);
            }
            ConstScalar::Int {
                bits,
                signed,
                words,
            } => {
                c.field_u64(Dim::Structure, 1);
                c.field_u64(Dim::Types, *bits as u64);
                c.field_bool(Dim::Types, *signed);
                c.field_bytes(Dim::Constants, words);
            }
            ConstScalar::FixedBytes(b) => {
                c.field_u64(Dim::Structure, 2);
                c.field_bytes(Dim::Constants, b);
            }
            ConstScalar::Address { bits, bytes } => {
                c.field_u64(Dim::Structure, 3);
                c.field_u64(Dim::Types, *bits as u64);
                c.field_bytes(Dim::Constants, bytes);
            }
        }
        c.exit_node();
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RLocalId(u32);
entity_impl!(RLocalId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RBlockId(u32);
entity_impl!(RBlockId);

pub type RValueId = RLocalId;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeBody<'db> {
    pub owner: RuntimeInstance<'db>,
    pub key: RuntimeInstanceKey<'db>,
    pub signature: RuntimeInterfaceSignature<'db>,
    pub semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    pub provider_bindings: Vec<RuntimeProviderBinding<'db>>,
    pub locals: Vec<RLocal<'db>>,
    pub blocks: Vec<RBlock<'db>>,
}

impl<'db> RuntimeBody<'db> {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RLocal<'db> {
    pub semantic_ty: TyId<'db>,
    pub carrier: RuntimeCarrier<'db>,
    pub root: RuntimeLocalRoot<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeLocalRoot<'db> {
    None,
    Slot(RuntimeClass<'db>),
    Ref(RuntimeClass<'db>),
    Ptr {
        space: AddressSpaceKind,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeLocalLowering<'db> {
    Erased,
    DirectValue,
    PlaceCarrier {
        place_class: RuntimeClass<'db>,
    },
    PlaceBoundValue {
        provider: Option<RuntimeProviderBindingId>,
        place_class: RuntimeClass<'db>,
    },
    DirectCarrier {
        provider: Option<RuntimeProviderBindingId>,
        place_class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeProviderBindingId(u32);
entity_impl!(RuntimeProviderBindingId);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeProviderBinding<'db> {
    pub provider: ProviderBinding<'db>,
    pub value: RLocalId,
    pub provider_class: RuntimeClass<'db>,
    pub place_class: RuntimeClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeInterfaceSignature<'db> {
    pub params: Vec<RuntimeParam<'db>>,
    pub ret: Option<RuntimeClass<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeExitBehavior {
    MayReturn,
    NeverReturns,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeParam<'db> {
    pub local: RLocalId,
    pub class: RuntimeClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RBlock<'db> {
    pub stmts: Vec<RStmt<'db>>,
    pub stmt_origins: Vec<common::provenance::ProvenanceNodeId>,
    pub terminator: RTerminator<'db>,
    pub terminator_origin: common::provenance::ProvenanceNodeId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeCallEdge<'db> {
    pub callee: RuntimeInstance<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeCodeRegion<'db> {
    pub key: RuntimeCodeRegionKey<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeCodeRegionKey<'db> {
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRuntime {
        contract: Contract<'db>,
    },
    ManualContractRoot {
        func: Func<'db>,
    },
    FunctionRoot {
        symbol: String,
        callee: RuntimeInstance<'db>,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub struct ResolvedCodeRegion<'db> {
    pub region: RuntimeCodeRegion<'db>,
    pub symbol: String,
    pub source: RuntimeSectionRef<'db>,
    pub root: RuntimeFunction<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimePackage<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub functions: Vec<RuntimeFunction<'db>>,
    pub plan: RuntimePackagePlan<'db>,
}

impl<'db> RuntimePackage<'db> {
    pub fn objects(self, db: &'db dyn MirDb) -> Vec<RuntimeObject<'db>> {
        self.plan(db).objects(db)
    }

    pub fn const_regions(self, db: &'db dyn MirDb) -> Vec<ConstRegionId<'db>> {
        self.plan(db).const_regions(db)
    }

    pub fn code_regions(self, db: &'db dyn MirDb) -> Vec<ResolvedCodeRegion<'db>> {
        self.plan(db).code_regions(db)
    }

    pub fn root_objects(self, db: &'db dyn MirDb) -> Vec<RuntimeObject<'db>> {
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
    pub primary_object: Option<RuntimeObject<'db>>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeFunction<'db> {
    pub instance: RuntimeInstance<'db>,
    pub symbol: String,
    pub linkage: RuntimeLinkage,
    pub inline_hint: RuntimeInlineHint,
    pub owner: RuntimeFunctionOwner<'db>,
    pub referenced_const_regions: Vec<ConstRegionId<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeLinkage {
    Private,
    Internal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeInlineHint {
    Auto,
    Hint,
    Always,
    Never,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeFunctionOwner<'db> {
    Semantic(SemanticInstance<'db>),
    Synthetic(RuntimeSyntheticSpec<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeSyntheticSpec<'db> {
    MainRoot {
        callee: RuntimeInstance<'db>,
        entry_effect_args: Box<[EntryEffectArgPlan<'db>]>,
    },
    TestRoot {
        name: String,
        callee: RuntimeInstance<'db>,
        entry_effect_args: Box<[EntryEffectArgPlan<'db>]>,
    },
    ManualContractRoot {
        func: Func<'db>,
        callee: RuntimeInstance<'db>,
        entry_effect_args: Box<[EntryEffectArgPlan<'db>]>,
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
    CodeRegionRoot {
        symbol: String,
        callee: RuntimeInstance<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ContractInitAbiPlan<'db> {
    pub contract: Contract<'db>,
    pub payable: bool,
    pub user_init: Option<RuntimeInstance<'db>>,
    pub entry_effect_args: Box<[EntryEffectArgPlan<'db>]>,
    pub init_args: InitArgsPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ContractRecvAbiPlan<'db> {
    pub contract: Contract<'db>,
    pub selector: Option<u32>,
    pub payable: bool,
    pub user_recv: RuntimeInstance<'db>,
    pub entry_effect_args: Box<[EntryEffectArgPlan<'db>]>,
    pub input: RuntimeInputPlan<'db>,
    pub ret: RuntimeReturnPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum EntryEffectArgPlan<'db> {
    ContractField(ContractFieldBinding<'db>),
    TargetRootProvider(TargetRootProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ContractFieldBinding<'db> {
    pub slot: u128,
    pub declared_ty: TyId<'db>,
    pub class: RuntimeClass<'db>,
    pub kind: RefKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct TargetRootProviderBinding<'db> {
    pub declared_ty: TyId<'db>,
    pub class: RuntimeClass<'db>,
    pub materialization: TargetRootProviderMaterialization<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum TargetRootProviderMaterialization<'db> {
    MemoryObject { layout: LayoutId<'db> },
    MemoryRawAddr { layout: LayoutId<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeParamPlan<'db> {
    Erased,
    Boundary(RuntimeBoundarySpec<'db>),
    ReadOnlyView {
        value: RuntimeClass<'db>,
        borrow: RuntimeBoundarySpec<'db>,
    },
    PassActual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum BorrowAccess {
    ReadOnly,
    ReadWrite,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct BorrowTransportSet {
    pub allow_object: bool,
    pub allow_const: bool,
    pub provider_spaces: Box<[AddressSpaceKind]>,
    pub allow_raw_addr: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum InitArgsPlan<'db> {
    None,
    DecodeInitTail {
        tuple_ty: TyId<'db>,
        decode_fn: RuntimeInstance<'db>,
        projected_fields: Box<[u32]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeInputPlan<'db> {
    None,
    DecodeHostPayload {
        msg_ty: TyId<'db>,
        host: TargetRootProviderBinding<'db>,
        decode_args_fn: RuntimeInstance<'db>,
        projected_fields: Box<[u32]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeReturnPlan<'db> {
    Unit,
    Value { ty: TyId<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct DispatchArm<'db> {
    pub selector: u32,
    pub wrapper: RuntimeInstance<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeSection<'db> {
    pub name: RuntimeSectionName,
    pub entry: RuntimeFunction<'db>,
    pub embeds: Vec<RuntimeEmbed<'db>>,
    pub const_regions: Vec<ConstRegionId<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeSectionName {
    Init,
    Runtime,
    Main,
    Test(String),
    CodeRegion(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeEmbed<'db> {
    pub source: RuntimeSectionRef<'db>,
    pub as_symbol: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeSectionRef<'db> {
    Local {
        object: RuntimeObject<'db>,
        section: RuntimeSectionName,
    },
    External {
        object: RuntimeObject<'db>,
        section: RuntimeSectionName,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum PlaceRoot<'db> {
    Slot(RLocalId),
    Ref(RValueId),
    Provider(RuntimeProviderBindingId),
    Ptr {
        addr: RValueId,
        space: AddressSpaceKind,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimePlace<'db> {
    pub root: PlaceRoot<'db>,
    pub path: Box<[PlaceElem<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum PlaceElem<'db> {
    Field(FieldIndex),
    Index(IndexSource<RValueId>),
    VariantField {
        variant: VariantId<'db>,
        field: FieldIndex,
    },
    Deref,
}

impl<'db> IrDescribe for RuntimePlace<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node("Place");
        match &self.root {
            PlaceRoot::Slot(local) => {
                c.field_u64(Dim::Structure, 0);
                c.field_u64(Dim::Structure, local.as_u32() as u64);
            }
            PlaceRoot::Ref(value) => {
                c.field_u64(Dim::Structure, 1);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
            }
            PlaceRoot::Provider(binding) => {
                c.field_u64(Dim::Structure, 2);
                c.field_u64(Dim::Structure, binding.as_u32() as u64);
            }
            PlaceRoot::Ptr { addr, space, class } => {
                c.field_u64(Dim::Structure, 3);
                c.field_u64(Dim::Structure, addr.as_u32() as u64);
                c.field_u64(Dim::Structure, *space as u64);
                describe_class(cx, c, class);
            }
        }
        c.field_u64(Dim::Structure, self.path.len() as u64);
        for elem in self.path.iter() {
            match elem {
                PlaceElem::Field(idx) => {
                    c.field_u64(Dim::Structure, 0);
                    c.field_u64(Dim::Structure, idx.0 as u64);
                }
                PlaceElem::Index(idx_src) => {
                    c.field_u64(Dim::Structure, 1);
                    match idx_src {
                        IndexSource::Constant(n) => c.field_u64(Dim::Structure, *n as u64),
                        IndexSource::Dynamic(v) => c.field_u64(Dim::Structure, v.as_u32() as u64),
                    }
                }
                PlaceElem::VariantField { variant, field } => {
                    c.field_u64(Dim::Structure, 2);
                    c.field_u64(Dim::Structure, variant.index as u64);
                    c.field_u64(Dim::Structure, field.0 as u64);
                }
                PlaceElem::Deref => {
                    c.field_u64(Dim::Structure, 3);
                }
            }
        }
        c.exit_node();
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ResolvedRuntimePlace<'db> {
    pub root_kind: ResolvedPlaceRootKind<'db>,
    pub result_class: RuntimeClass<'db>,
    pub path: Box<[ResolvedPlaceElem<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
        space: AddressSpaceKind,
        class: RuntimeClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
    Keccak256 {
        offset: RValueId,
        len: RValueId,
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
        slot: u128,
        class: RuntimeClass<'db>,
        kind: RefKind<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SaturatingBinOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum IntrinsicArithBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl<'db> IrDescribe for RuntimeBuiltin<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        use RuntimeBuiltin::*;
        match self {
            IntrinsicArith {
                op,
                checked,
                lhs,
                rhs,
                class,
            } => {
                c.enter_node("IntrinsicArith");
                c.field_u64(Dim::Structure, *op as u64);
                c.field_bool(Dim::Structure, *checked);
                c.field_u64(Dim::Structure, lhs.as_u32() as u64);
                c.field_u64(Dim::Structure, rhs.as_u32() as u64);
                describe_scalar_class(c, class);
                c.exit_node();
            }
            Saturating {
                op,
                lhs,
                rhs,
                class,
            } => {
                c.enter_node("Saturating");
                c.field_u64(Dim::Structure, *op as u64);
                c.field_u64(Dim::Structure, lhs.as_u32() as u64);
                c.field_u64(Dim::Structure, rhs.as_u32() as u64);
                describe_scalar_class(c, class);
                c.exit_node();
            }
            Mload { addr } => {
                c.enter_node("Mload");
                c.field_u64(Dim::Structure, addr.as_u32() as u64);
                c.exit_node();
            }
            Mstore { addr, value } => {
                c.enter_node("Mstore");
                c.field_u64(Dim::Structure, addr.as_u32() as u64);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            Mstore8 { addr, value } => {
                c.enter_node("Mstore8");
                c.field_u64(Dim::Structure, addr.as_u32() as u64);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            Mcopy { dst, src, len } => {
                c.enter_node("Mcopy");
                c.field_u64(Dim::Structure, dst.as_u32() as u64);
                c.field_u64(Dim::Structure, src.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            Sload { slot } => {
                c.enter_node("Sload");
                c.effect("storage_read");
                c.field_u64(Dim::Structure, slot.as_u32() as u64);
                c.exit_node();
            }
            Sstore { slot, value } => {
                c.enter_node("Sstore");
                c.effect("storage_write");
                c.field_u64(Dim::Structure, slot.as_u32() as u64);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            SignExtend { byte, value } => {
                c.enter_node("SignExtend");
                c.field_u64(Dim::Structure, byte.as_u32() as u64);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            Keccak256 { offset, len } => {
                c.enter_node("Keccak256");
                c.effect("keccak256");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            AddMod { lhs, rhs, modulus } => {
                c.enter_node("AddMod");
                c.field_u64(Dim::Structure, lhs.as_u32() as u64);
                c.field_u64(Dim::Structure, rhs.as_u32() as u64);
                c.field_u64(Dim::Structure, modulus.as_u32() as u64);
                c.exit_node();
            }
            MulMod { lhs, rhs, modulus } => {
                c.enter_node("MulMod");
                c.field_u64(Dim::Structure, lhs.as_u32() as u64);
                c.field_u64(Dim::Structure, rhs.as_u32() as u64);
                c.field_u64(Dim::Structure, modulus.as_u32() as u64);
                c.exit_node();
            }
            CallDataLoad { offset } => {
                c.enter_node("CallDataLoad");
                c.effect("calldata_read");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.exit_node();
            }
            CallDataCopy { dst, offset, len } => {
                c.enter_node("CallDataCopy");
                c.effect("calldata_read");
                c.field_u64(Dim::Structure, dst.as_u32() as u64);
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            ReturnDataCopy { dst, offset, len } => {
                c.enter_node("ReturnDataCopy");
                c.effect("returndata_read");
                c.field_u64(Dim::Structure, dst.as_u32() as u64);
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            CodeCopy { dst, offset, len } => {
                c.enter_node("CodeCopy");
                c.field_u64(Dim::Structure, dst.as_u32() as u64);
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            BlockHash { block } => {
                c.enter_node("BlockHash");
                c.field_u64(Dim::Structure, block.as_u32() as u64);
                c.exit_node();
            }
            Malloc { size } => {
                c.enter_node("Malloc");
                c.field_u64(Dim::Structure, size.as_u32() as u64);
                c.exit_node();
            }
            Call {
                gas,
                addr,
                value,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                c.enter_node("EvmCall");
                c.effect("external_call");
                for v in [gas, addr, value, args_offset, args_len, ret_offset, ret_len] {
                    c.field_u64(Dim::Structure, v.as_u32() as u64);
                }
                c.exit_node();
            }
            StaticCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                c.enter_node("StaticCall");
                c.effect("external_call");
                for v in [gas, addr, args_offset, args_len, ret_offset, ret_len] {
                    c.field_u64(Dim::Structure, v.as_u32() as u64);
                }
                c.exit_node();
            }
            DelegateCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                c.enter_node("DelegateCall");
                c.effect("external_call");
                for v in [gas, addr, args_offset, args_len, ret_offset, ret_len] {
                    c.field_u64(Dim::Structure, v.as_u32() as u64);
                }
                c.exit_node();
            }
            Create { value, offset, len } => {
                c.enter_node("Create");
                c.effect("external_call");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            Create2 {
                value,
                offset,
                len,
                salt,
            } => {
                c.enter_node("Create2");
                c.effect("external_call");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.field_u64(Dim::Structure, salt.as_u32() as u64);
                c.exit_node();
            }
            Log0 { offset, len } => {
                c.enter_node("Log0");
                c.effect("event_emit");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            Log1 {
                offset,
                len,
                topic0,
            } => {
                c.enter_node("Log1");
                c.effect("event_emit");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.field_u64(Dim::Structure, topic0.as_u32() as u64);
                c.exit_node();
            }
            Log2 {
                offset,
                len,
                topic0,
                topic1,
            } => {
                c.enter_node("Log2");
                c.effect("event_emit");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.field_u64(Dim::Structure, topic0.as_u32() as u64);
                c.field_u64(Dim::Structure, topic1.as_u32() as u64);
                c.exit_node();
            }
            Log3 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
            } => {
                c.enter_node("Log3");
                c.effect("event_emit");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.field_u64(Dim::Structure, topic0.as_u32() as u64);
                c.field_u64(Dim::Structure, topic1.as_u32() as u64);
                c.field_u64(Dim::Structure, topic2.as_u32() as u64);
                c.exit_node();
            }
            Log4 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
                topic3,
            } => {
                c.enter_node("Log4");
                c.effect("event_emit");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.field_u64(Dim::Structure, topic0.as_u32() as u64);
                c.field_u64(Dim::Structure, topic1.as_u32() as u64);
                c.field_u64(Dim::Structure, topic2.as_u32() as u64);
                c.field_u64(Dim::Structure, topic3.as_u32() as u64);
                c.exit_node();
            }
            Msize => {
                c.enter_node("Msize");
                c.exit_node();
            }
            CallValue => {
                c.enter_node("CallValue");
                c.effect("msg_value_read");
                c.exit_node();
            }
            ReturnDataSize => {
                c.enter_node("ReturnDataSize");
                c.exit_node();
            }
            CallDataSize => {
                c.enter_node("CallDataSize");
                c.effect("calldata_read");
                c.exit_node();
            }
            CodeSize => {
                c.enter_node("CodeSize");
                c.exit_node();
            }
            Address => {
                c.enter_node("Address");
                c.exit_node();
            }
            Caller => {
                c.enter_node("Caller");
                c.effect("msg_sender_read");
                c.exit_node();
            }
            Origin => {
                c.enter_node("Origin");
                c.effect("tx_origin_read");
                c.exit_node();
            }
            GasPrice => {
                c.enter_node("GasPrice");
                c.exit_node();
            }
            CoinBase => {
                c.enter_node("CoinBase");
                c.exit_node();
            }
            Timestamp => {
                c.enter_node("Timestamp");
                c.exit_node();
            }
            Number => {
                c.enter_node("Number");
                c.exit_node();
            }
            PrevRandao => {
                c.enter_node("PrevRandao");
                c.exit_node();
            }
            GasLimit => {
                c.enter_node("GasLimit");
                c.exit_node();
            }
            ChainId => {
                c.enter_node("ChainId");
                c.exit_node();
            }
            BaseFee => {
                c.enter_node("BaseFee");
                c.exit_node();
            }
            SelfBalance => {
                c.enter_node("SelfBalance");
                c.exit_node();
            }
            Gas => {
                c.enter_node("Gas");
                c.exit_node();
            }
            CurrentCodeRegionLen => {
                c.enter_node("CurrentCodeRegionLen");
                c.exit_node();
            }
            CodeRegionOffset { region } => {
                c.enter_node("CodeRegionOffset");
                describe_code_region(cx, c, region);
                c.exit_node();
            }
            CodeRegionLen { region } => {
                c.enter_node("CodeRegionLen");
                describe_code_region(cx, c, region);
                c.exit_node();
            }
            CallDataSelector => {
                c.enter_node("CallDataSelector");
                c.exit_node();
            }
            MakeContractFieldRef { slot, class, kind } => {
                c.enter_node("MakeContractFieldRef");
                c.effect("storage_ref");
                c.field_u64(Dim::Structure, *slot as u64);
                describe_class(cx, c, class);
                describe_ref_kind(cx, c, kind);
                c.exit_node();
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
    ProviderFromRaw {
        raw: RValueId,
        provider_ty: TyId<'db>,
        space: AddressSpaceKind,
        target: Option<LayoutId<'db>>,
    },
    WordToRawAddr {
        value: RValueId,
        space: AddressSpaceKind,
        target: Option<LayoutId<'db>>,
    },
    ProviderToRaw {
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
        root: RValueId,
    },
    EnumAssertVariantRef {
        root: RValueId,
        variant: VariantId<'db>,
    },
}

impl<'db> IrDescribe for RExpr<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        match self {
            RExpr::Use(v) => {
                c.enter_node("Use");
                c.field_u64(Dim::Structure, v.as_u32() as u64);
                c.exit_node();
            }
            RExpr::ConstScalar(s) => {
                c.enter_node("Const");
                c.child(cx, s);
                c.exit_node();
            }
            RExpr::Binary { op, lhs, rhs } => {
                c.enter_node("Binary");
                c.field_u64(Dim::Structure, binop_tag(op));
                c.field_u64(Dim::Structure, lhs.as_u32() as u64);
                c.field_u64(Dim::Structure, rhs.as_u32() as u64);
                c.exit_node();
            }
            RExpr::Unary { op, value } => {
                c.enter_node("Unary");
                c.field_u64(Dim::Structure, *op as u64);
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            RExpr::Call { callee, args } => {
                c.enter_node("Call");
                describe_runtime_instance(cx, c, callee);
                for arg in args.iter() {
                    c.field_u64(Dim::Structure, arg.as_u32() as u64);
                }
                c.exit_node();
            }
            RExpr::Load { place } => {
                c.enter_node("Load");
                c.child(cx, place);
                c.exit_node();
            }
            RExpr::AggregateExtract { value, index } => {
                c.enter_node("AggregateExtract");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, *index as u64);
                c.exit_node();
            }
            RExpr::Builtin(b) => {
                c.enter_node("Builtin");
                c.child(cx, b);
                c.exit_node();
            }
            RExpr::Placeholder { class } => {
                c.enter_node("Placeholder");
                describe_class(cx, c, class);
                c.exit_node();
            }
            RExpr::Cast { value, to } => {
                c.enter_node("Cast");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                describe_scalar_class(c, to);
                c.exit_node();
            }
            RExpr::ConstRef { region: _, layout } => {
                c.enter_node("ConstRef");
                describe_layout_id(cx, c, layout);
                c.exit_node();
            }
            RExpr::AllocObject { layout } => {
                c.enter_node("AllocObject");
                describe_layout_id(cx, c, layout);
                c.exit_node();
            }
            RExpr::MaterializeToObject { src } => {
                c.enter_node("Materialize");
                c.field_u64(Dim::Structure, src.as_u32() as u64);
                c.exit_node();
            }
            RExpr::MaterializePlaceToObject { place } => {
                c.enter_node("MaterializePlace");
                c.child(cx, place);
                c.exit_node();
            }
            RExpr::ProviderFromRaw {
                raw,
                provider_ty: _,
                space,
                target,
            } => {
                c.enter_node("ProviderFromRaw");
                c.field_u64(Dim::Structure, raw.as_u32() as u64);
                c.field_u64(Dim::Structure, *space as u64);
                if let Some(layout) = target {
                    describe_layout_id(cx, c, layout);
                }
                c.exit_node();
            }
            RExpr::WordToRawAddr {
                value,
                space,
                target,
            } => {
                c.enter_node("WordToRawAddr");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, *space as u64);
                if let Some(layout) = target {
                    describe_layout_id(cx, c, layout);
                }
                c.exit_node();
            }
            RExpr::ProviderToRaw { value } => {
                c.enter_node("ProviderToRaw");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            RExpr::RetagRef { value } => {
                c.enter_node("RetagRef");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            RExpr::AddrOf { place } => {
                c.enter_node("AddrOf");
                c.child(cx, place);
                c.exit_node();
            }
            RExpr::EnumMake {
                layout,
                variant,
                fields,
            } => {
                c.enter_node("EnumMake");
                describe_layout_id(cx, c, layout);
                c.field_u64(Dim::Structure, variant.index as u64);
                for f in fields.iter() {
                    c.field_u64(Dim::Structure, f.as_u32() as u64);
                }
                c.exit_node();
            }
            RExpr::EnumTagOfValue { value } => {
                c.enter_node("EnumTagOfValue");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.exit_node();
            }
            RExpr::EnumIsVariant { value, variant } => {
                c.enter_node("EnumIsVariant");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                c.exit_node();
            }
            RExpr::EnumExtract {
                value,
                variant,
                field,
            } => {
                c.enter_node("EnumExtract");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                c.field_u64(Dim::Structure, field.0 as u64);
                c.exit_node();
            }
            RExpr::EnumGetTag { root } => {
                c.enter_node("EnumGetTag");
                c.field_u64(Dim::Structure, root.as_u32() as u64);
                c.exit_node();
            }
            RExpr::EnumAssertVariantRef { root, variant } => {
                c.enter_node("EnumAssertVariantRef");
                c.field_u64(Dim::Structure, root.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                c.exit_node();
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RStmt<'db> {
    Assign {
        dst: RLocalId,
        expr: RExpr<'db>,
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
}

impl<'db> IrDescribe for RStmt<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        match self {
            RStmt::Assign { dst, expr } => {
                c.enter_node("Assign");
                c.field_u64(Dim::Structure, dst.as_u32() as u64);
                c.child(cx, expr);
                c.exit_node();
            }
            RStmt::EnumAssertVariant { value, variant } => {
                c.enter_node("EnumAssertVariant");
                c.field_u64(Dim::Structure, value.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                c.exit_node();
            }
            RStmt::Store { dst, src } => {
                c.enter_node("Store");
                c.child(cx, dst);
                c.field_u64(Dim::Structure, src.as_u32() as u64);
                c.exit_node();
            }
            RStmt::CopyInto { dst, src } => {
                c.enter_node("CopyInto");
                c.child(cx, dst);
                c.field_u64(Dim::Structure, src.as_u32() as u64);
                c.exit_node();
            }
            RStmt::EnumSetTag { root, variant } => {
                c.enter_node("EnumSetTag");
                c.field_u64(Dim::Structure, root.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                c.exit_node();
            }
            RStmt::EnumWriteVariant {
                root,
                variant,
                fields,
            } => {
                c.enter_node("EnumWriteVariant");
                c.field_u64(Dim::Structure, root.as_u32() as u64);
                c.field_u64(Dim::Structure, variant.index as u64);
                for f in fields.iter() {
                    c.field_u64(Dim::Structure, f.as_u32() as u64);
                }
                c.exit_node();
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
    SelfDestruct {
        beneficiary: RValueId,
    },
    Trap,
    Return(Option<RValueId>),
    Stop,
}

impl<'db> IrDescribe for RTerminator<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        match self {
            RTerminator::Goto(target) => {
                c.enter_node("Goto");
                c.field_u64(Dim::Structure, target.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                c.enter_node("Branch");
                c.field_u64(Dim::Structure, cond.as_u32() as u64);
                c.field_u64(Dim::Structure, then_bb.as_u32() as u64);
                c.field_u64(Dim::Structure, else_bb.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::Return(v) => {
                c.enter_node("Return");
                if let Some(v) = v {
                    c.field_u64(Dim::Structure, v.as_u32() as u64);
                }
                c.exit_node();
            }
            RTerminator::Trap => {
                c.enter_node("Trap");
                c.effect("revert");
                c.exit_node();
            }
            RTerminator::Stop => {
                c.enter_node("Stop");
                c.exit_node();
            }
            RTerminator::SelfDestruct { beneficiary } => {
                c.enter_node("SelfDestruct");
                c.effect("selfdestruct");
                c.field_u64(Dim::Structure, beneficiary.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::ReturnData { offset, len } => {
                c.enter_node("ReturnData");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::Revert { offset, len } => {
                c.enter_node("Revert");
                c.effect("revert");
                c.field_u64(Dim::Structure, offset.as_u32() as u64);
                c.field_u64(Dim::Structure, len.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::TerminalCall { callee, args } => {
                c.enter_node("TerminalCall");
                describe_runtime_instance(cx, c, callee);
                for arg in args.iter() {
                    c.field_u64(Dim::Structure, arg.as_u32() as u64);
                }
                c.exit_node();
            }
            RTerminator::SwitchScalar {
                discr,
                cases,
                default,
            } => {
                c.enter_node("SwitchScalar");
                c.field_u64(Dim::Structure, discr.as_u32() as u64);
                c.field_u64(Dim::Structure, cases.len() as u64);
                for (scalar, target) in cases.iter() {
                    c.child(cx, scalar);
                    c.field_u64(Dim::Structure, target.as_u32() as u64);
                }
                c.field_u64(Dim::Structure, default.as_u32() as u64);
                c.exit_node();
            }
            RTerminator::MatchEnumTag {
                tag,
                enum_layout,
                cases,
                default,
            } => {
                c.enter_node("MatchEnumTag");
                c.field_u64(Dim::Structure, tag.as_u32() as u64);
                describe_layout_id(cx, c, enum_layout);
                c.field_u64(Dim::Structure, cases.len() as u64);
                for (variant, target) in cases.iter() {
                    c.field_u64(Dim::Structure, variant.index as u64);
                    c.field_u64(Dim::Structure, target.as_u32() as u64);
                }
                if let Some(d) = default {
                    c.field_u64(Dim::Structure, d.as_u32() as u64);
                }
                c.exit_node();
            }
        }
    }
}

pub(crate) fn terminator_successors(term: &RTerminator<'_>) -> Vec<(&'static str, RBlockId)> {
    match term {
        RTerminator::Goto(target) => vec![("goto", *target)],
        RTerminator::Branch {
            then_bb, else_bb, ..
        } => {
            vec![("then", *then_bb), ("else", *else_bb)]
        }
        RTerminator::SwitchScalar { cases, default, .. } => {
            let mut succs: Vec<_> = cases.iter().map(|(_, t)| ("case", *t)).collect();
            succs.push(("default", *default));
            succs
        }
        RTerminator::MatchEnumTag { cases, default, .. } => {
            let mut succs: Vec<_> = cases.iter().map(|(_, t)| ("case", *t)).collect();
            if let Some(d) = default {
                succs.push(("default", *d));
            }
            succs
        }
        RTerminator::TerminalCall { .. }
        | RTerminator::ReturnData { .. }
        | RTerminator::Revert { .. }
        | RTerminator::SelfDestruct { .. }
        | RTerminator::Trap
        | RTerminator::Return(_)
        | RTerminator::Stop => vec![],
    }
}

pub(crate) fn describe_class(
    cx: &DescribeCtx<'_>,
    c: &mut impl IrConsumer,
    class: &RuntimeClass<'_>,
) {
    match class {
        RuntimeClass::Scalar(scalar) => {
            c.field_u64(Dim::Types, 0);
            describe_scalar_class(c, scalar);
        }
        RuntimeClass::AggregateValue { layout } => {
            c.field_u64(Dim::Types, 1);
            describe_layout_id(cx, c, layout);
        }
        RuntimeClass::Ref {
            pointee,
            kind,
            view,
        } => {
            c.field_u64(Dim::Types, 2);
            describe_ref_kind(cx, c, kind);
            match view {
                RefView::Whole => c.field_u64(Dim::Types, 0),
                RefView::EnumVariant(v) => {
                    c.field_u64(Dim::Types, 1);
                    c.field_u64(Dim::Types, v.index as u64);
                }
            }
            describe_class(cx, c, pointee);
        }
        RuntimeClass::RawAddr { space, target } => {
            c.field_u64(Dim::Types, 3);
            c.field_u64(Dim::Types, *space as u64);
            if let Some(layout) = target {
                describe_layout_id(cx, c, layout);
            }
        }
    }
}

fn binop_tag(op: &BinOp) -> u64 {
    match op {
        BinOp::Arith(a) => *a as u64,
        BinOp::Comp(c) => 100 + *c as u64,
        BinOp::Logical(l) => 200 + *l as u64,
        BinOp::Index => 300,
    }
}

fn describe_ref_kind(_cx: &DescribeCtx<'_>, c: &mut impl IrConsumer, kind: &RefKind<'_>) {
    match kind {
        RefKind::Const => c.field_u64(Dim::Types, 0),
        RefKind::Object => c.field_u64(Dim::Types, 1),
        RefKind::Provider { provider_ty, space } => {
            c.field_u64(Dim::Types, 2);
            c.field_u64(Dim::Types, *space as u64);
            let ty_str = format!("{provider_ty:?}");
            c.field_str(Dim::Types, &ty_str);
        }
    }
}

fn describe_code_region(
    cx: &DescribeCtx<'_>,
    c: &mut impl IrConsumer,
    region: &RuntimeCodeRegion<'_>,
) {
    let db: &dyn MirDb = cx.db();
    match &region.key(db) {
        RuntimeCodeRegionKey::ContractInit { contract } => {
            c.field_u64(Dim::Structure, 0);
            let hir_db: &dyn hir::HirDb = db.as_dyn_database().as_view();
            if let hir::hir_def::Partial::Present(name) = contract.name(hir_db) {
                c.field_str(Dim::Names, name.data(hir_db));
            }
        }
        RuntimeCodeRegionKey::ContractRuntime { contract } => {
            c.field_u64(Dim::Structure, 1);
            let hir_db: &dyn hir::HirDb = db.as_dyn_database().as_view();
            if let hir::hir_def::Partial::Present(name) = contract.name(hir_db) {
                c.field_str(Dim::Names, name.data(hir_db));
            }
        }
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            c.field_u64(Dim::Structure, 2);
            let hir_db: &dyn hir::HirDb = db.as_dyn_database().as_view();
            if let hir::hir_def::Partial::Present(name) = func.name(hir_db) {
                c.field_str(Dim::Names, name.data(hir_db));
            }
        }
        RuntimeCodeRegionKey::FunctionRoot { symbol, callee } => {
            c.field_u64(Dim::Structure, 3);
            c.field_str(Dim::Names, symbol);
            describe_runtime_instance(cx, c, callee);
        }
    }
}

fn describe_scalar_class(c: &mut impl IrConsumer, sc: &ScalarClass<'_>) {
    match sc.repr {
        ScalarRepr::Bool => c.field_u64(Dim::Types, 0),
        ScalarRepr::Int { bits, signed } => {
            c.field_u64(Dim::Types, 1);
            c.field_u64(Dim::Types, bits as u64);
            c.field_bool(Dim::Types, signed);
        }
        ScalarRepr::FixedBytes { len } => {
            c.field_u64(Dim::Types, 2);
            c.field_u64(Dim::Types, len as u64);
        }
        ScalarRepr::Address { bits } => {
            c.field_u64(Dim::Types, 3);
            c.field_u64(Dim::Types, bits as u64);
        }
    }
}

fn describe_runtime_instance(
    cx: &DescribeCtx<'_>,
    c: &mut impl IrConsumer,
    instance: &RuntimeInstance<'_>,
) {
    let db: &dyn MirDb = cx.db();
    let key = instance.key(db);
    let params = key.params(db);
    match key.source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            c.field_u64(Dim::Structure, 0);
            let sem_key = semantic.key(db);
            let owner = sem_key.owner(db);
            let hir_db: &dyn hir::HirDb = db.as_dyn_database().as_view();
            // Extract name from the BodyOwner variant -- only Func and Const
            // have names; contract init/recv arms are identified structurally.
            match owner {
                hir::analysis::ty::ty_check::BodyOwner::Func(func) => {
                    if let hir::hir_def::Partial::Present(name) = func.name(hir_db) {
                        c.field_str(Dim::Names, name.data(hir_db));
                    }
                }
                hir::analysis::ty::ty_check::BodyOwner::Const(const_) => {
                    if let hir::hir_def::Partial::Present(name) = const_.name(hir_db) {
                        c.field_str(Dim::Names, name.data(hir_db));
                    }
                }
                _ => {}
            }
        }
        RuntimeInstanceSource::Synthetic(synth) => {
            c.field_u64(Dim::Structure, 1);
            c.field_str(Dim::Structure, &format!("{:?}", synth.spec(db)));
        }
    }
    c.field_u64(Dim::Structure, params.len() as u64);
    for param in params {
        describe_class(cx, c, param);
    }
}

fn describe_layout_id(cx: &DescribeCtx<'_>, c: &mut impl IrConsumer, layout: &LayoutId<'_>) {
    let db: &dyn MirDb = cx.db();
    match layout.key(db) {
        LayoutKey::Struct(s) => {
            c.field_u64(Dim::Types, 0);
            c.field_u64(Dim::Types, s.fields.len() as u64);
            for field in s.fields.iter() {
                describe_class(cx, c, field);
            }
        }
        LayoutKey::Array(a) => {
            c.field_u64(Dim::Types, 1);
            describe_class(cx, c, &a.elem);
            c.field_u64(Dim::Types, a.len);
        }
        LayoutKey::Enum(e) => {
            c.field_u64(Dim::Types, 2);
            c.field_u64(Dim::Types, e.variants.len() as u64);
            for v in e.variants.iter() {
                c.field_str(Dim::Names, &v.name);
                c.field_u64(Dim::Types, v.fields.len() as u64);
                for f in v.fields.iter() {
                    describe_class(cx, c, f);
                }
            }
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
        id.interface_signature(*self)
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
