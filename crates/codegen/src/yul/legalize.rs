use common::layout::TargetDataLayout;
use driver::DriverDataBase;
use hir::analysis::{
    semantic::{FieldIndex, Mutability, normalize_semantic_body},
    ty::ty_check::{LocalBinding, ParamSite},
};
use hir::hir_def::{BinOp, TopLevelMod, UnOp};
use hir::projection::{IndexSource, Projection, ProjectionPath};
use mir::runtime::RefKind;
use mir::{
    AddressSpaceKind, ConstNode, ConstRegionId, ConstScalar, IntrinsicArithBinOp, Layout, LayoutId,
    PlaceElem, PlaceRoot, RExpr, RLocalId, RStmt, RTerminator, RValueId, ResolvedPlaceElem,
    ResolvedPlaceRootKind, RuntimeBuiltin, RuntimeClass, RuntimeCodeRegion, RuntimeFunction,
    RuntimeFunctionOwner, RuntimeInlineHint, RuntimeInstance, RuntimeLinkage, RuntimeObject,
    RuntimePackage, RuntimeSection, RuntimeSectionName, RuntimeSectionRef, RuntimeSyntheticSpec,
    SaturatingBinOp, ScalarClass, ScalarRepr, VariantId, array_elem_size_bytes, layout_size_bytes,
    resolve_runtime_place, serialize_const_region_bytes,
};
use rustc_hash::FxHashMap;

use crate::{
    function_symbols::{FunctionSymbolInput, FunctionSymbolStyle, assign_function_symbols},
    yul::{
        errors::YulError,
        names::{prefix_yul_name, sanitize_yul_ident},
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct YLocalId(pub u32);

impl YLocalId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct YBlockId(pub u32);

impl YBlockId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct YFunctionId(pub u32);

impl YFunctionId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum YulAddressSpace {
    Memory,
    Storage,
    Transient,
    Calldata,
    Code,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YulValueClass<'db> {
    Word(ScalarClass<'db>),
    MemoryPtr { layout: LayoutId<'db> },
    CodePtr { layout: LayoutId<'db> },
    StoragePtr { layout: LayoutId<'db> },
    TransientPtr { layout: LayoutId<'db> },
    CalldataPtr { layout: LayoutId<'db> },
}

impl<'db> YulValueClass<'db> {
    fn for_runtime_class(db: &'db DriverDataBase, class: &RuntimeClass<'db>) -> Self {
        match class {
            RuntimeClass::Scalar(class) => Self::Word(class.clone()),
            RuntimeClass::Ref { pointee, kind, .. } => match kind {
                RefKind::Const => pointee
                    .aggregate_layout()
                    .map(|layout| Self::CodePtr { layout })
                    .unwrap_or_else(pointer_word_class),
                RefKind::Object => pointee
                    .aggregate_layout()
                    .map(|layout| Self::MemoryPtr { layout })
                    .unwrap_or_else(pointer_word_class),
                RefKind::Provider { space, .. } => pointee
                    .aggregate_layout()
                    .map(|layout| yul_ptr_class(yul_space_from_runtime(*space), layout))
                    .unwrap_or_else(pointer_word_class),
            },
            RuntimeClass::RawAddr { space, target } => target
                .map(|layout| yul_ptr_class(yul_space_from_runtime(*space), layout))
                .unwrap_or_else(pointer_word_class),
            RuntimeClass::AggregateValue { layout } => layout_scalar_word_class(db, *layout)
                .map(Self::Word)
                .unwrap_or(Self::MemoryPtr { layout: *layout }),
        }
    }

    fn for_place_path(
        db: &'db DriverDataBase,
        class: &RuntimeClass<'db>,
        remaining_path: bool,
    ) -> Self {
        if remaining_path && let RuntimeClass::AggregateValue { layout } = class {
            return Self::MemoryPtr { layout: *layout };
        }
        Self::for_runtime_class(db, class)
    }
}

pub type YTransportPath<'db> = ProjectionPath<LayoutId<'db>, VariantId<'db>, YLocalId>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YTransportInfo<'db> {
    pub root_alias: Option<YulAddressSpace>,
    pub leaves: Box<[(YTransportPath<'db>, YulAddressSpace)]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum YulStorageKind {
    Cell,
    Bytes,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum YulParamKind {
    Visible(usize),
    Effect(usize),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct YulFunctionKey<'db> {
    runtime_function: RuntimeFunction<'db>,
    effect_spaces: Box<[Option<YulAddressSpace>]>,
    param_transports: Box<[YTransportInfo<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulPackage<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub functions: Vec<YulFunctionPlan<'db>>,
    pub objects: Vec<YulObjectPlan<'db>>,
    pub const_region_labels: Vec<(ConstRegionId<'db>, String)>,
    pub code_region_labels: Vec<(RuntimeCodeRegion<'db>, String)>,
    pub primary_object: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulObjectPlan<'db> {
    pub name: String,
    pub root: bool,
    pub sections: Vec<YulSectionPlan<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulSectionPlan<'db> {
    pub object_name: String,
    pub name: RuntimeSectionName,
    pub entry: YFunctionId,
    pub functions: Vec<YFunctionId>,
    pub const_regions: Vec<YulDataRegionPlan<'db>>,
    pub embeds: Vec<YulEmbedPlan>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulEmbedPlan {
    pub source_object: String,
    pub source_section: RuntimeSectionName,
    pub label: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulDataRegionPlan<'db> {
    pub region: ConstRegionId<'db>,
    pub label: String,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulFunctionPlan<'db> {
    pub id: YFunctionId,
    pub runtime_function: RuntimeFunction<'db>,
    pub symbol: String,
    variant_suffix: String,
    pub linkage: RuntimeLinkage,
    pub inline_hint: RuntimeInlineHint,
    pub param_kinds: Vec<YulParamKind>,
    pub params: Vec<YulValueClass<'db>>,
    pub ret: Option<YulValueClass<'db>>,
    pub ret_transport: Option<YTransportInfo<'db>>,
    pub param_locals: Vec<YLocalId>,
    pub locals: Vec<YulLocal<'db>>,
    pub blocks: Vec<YulBlock<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulLocal<'db> {
    pub class: Option<YulValueClass<'db>>,
    pub root: YulLocalRoot<'db>,
    pub transport: YTransportInfo<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YulLocalRoot<'db> {
    None,
    MemorySlot { class: RuntimeClass<'db> },
    PtrRoot { class: YulValueClass<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulBlock<'db> {
    pub stmts: Vec<YStmt<'db>>,
    pub terminator: YTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct YulPlace<'db> {
    pub root: YulPlaceRoot<'db>,
    pub path: Box<[YulPlaceElem<'db>]>,
    pub storage_kind: YulStorageKind,
    pub packed_byte_access: bool,
    pub runtime_result_class: RuntimeClass<'db>,
    pub result_class: YulValueClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YulPlaceRoot<'db> {
    Slot(YLocalId),
    Ptr {
        local: YLocalId,
        space: YulAddressSpace,
        class: YulValueClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YulPlaceElem<'db> {
    Field {
        field: FieldIndex,
        class: YulValueClass<'db>,
    },
    Index {
        index: IndexSource<YLocalId>,
        class: YulValueClass<'db>,
    },
    VariantField {
        variant: VariantId<'db>,
        field: FieldIndex,
        class: YulValueClass<'db>,
    },
    Deref {
        carrier_class: RuntimeClass<'db>,
        class: YulValueClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YBuiltin<'db> {
    Mload {
        addr: YLocalId,
    },
    Mstore {
        addr: YLocalId,
        value: YLocalId,
    },
    Mstore8 {
        addr: YLocalId,
        value: YLocalId,
    },
    Mcopy {
        dst: YLocalId,
        src: YLocalId,
        len: YLocalId,
    },
    Msize,
    Sload {
        slot: YLocalId,
    },
    Sstore {
        slot: YLocalId,
        value: YLocalId,
    },
    CallValue,
    ReturnDataSize,
    ReturnDataCopy {
        dst: YLocalId,
        offset: YLocalId,
        len: YLocalId,
    },
    CallDataSize,
    CallDataLoad {
        offset: YLocalId,
    },
    CallDataCopy {
        dst: YLocalId,
        offset: YLocalId,
        len: YLocalId,
    },
    CodeSize,
    CodeCopy {
        dst: YLocalId,
        offset: YLocalId,
        len: YLocalId,
    },
    Keccak256 {
        offset: YLocalId,
        len: YLocalId,
    },
    AddMod {
        lhs: YLocalId,
        rhs: YLocalId,
        modulus: YLocalId,
    },
    MulMod {
        lhs: YLocalId,
        rhs: YLocalId,
        modulus: YLocalId,
    },
    SignExtend {
        byte: YLocalId,
        value: YLocalId,
    },
    IntrinsicArith {
        op: IntrinsicArithBinOp,
        checked: bool,
        lhs: YLocalId,
        rhs: YLocalId,
        class: ScalarClass<'db>,
    },
    Saturating {
        op: SaturatingBinOp,
        lhs: YLocalId,
        rhs: YLocalId,
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
        block: YLocalId,
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
        size: YLocalId,
    },
    Call {
        gas: YLocalId,
        addr: YLocalId,
        value: YLocalId,
        args_offset: YLocalId,
        args_len: YLocalId,
        ret_offset: YLocalId,
        ret_len: YLocalId,
    },
    StaticCall {
        gas: YLocalId,
        addr: YLocalId,
        args_offset: YLocalId,
        args_len: YLocalId,
        ret_offset: YLocalId,
        ret_len: YLocalId,
    },
    DelegateCall {
        gas: YLocalId,
        addr: YLocalId,
        args_offset: YLocalId,
        args_len: YLocalId,
        ret_offset: YLocalId,
        ret_len: YLocalId,
    },
    Create {
        value: YLocalId,
        offset: YLocalId,
        len: YLocalId,
    },
    Create2 {
        value: YLocalId,
        offset: YLocalId,
        len: YLocalId,
        salt: YLocalId,
    },
    Log0 {
        offset: YLocalId,
        len: YLocalId,
    },
    Log1 {
        offset: YLocalId,
        len: YLocalId,
        topic0: YLocalId,
    },
    Log2 {
        offset: YLocalId,
        len: YLocalId,
        topic0: YLocalId,
        topic1: YLocalId,
    },
    Log3 {
        offset: YLocalId,
        len: YLocalId,
        topic0: YLocalId,
        topic1: YLocalId,
        topic2: YLocalId,
    },
    Log4 {
        offset: YLocalId,
        len: YLocalId,
        topic0: YLocalId,
        topic1: YLocalId,
        topic2: YLocalId,
        topic3: YLocalId,
    },
    CallDataSelector,
    MakeContractFieldRef {
        slot: u128,
        class: YulValueClass<'db>,
        kind: RefKind<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YExpr<'db> {
    Use(YLocalId),
    ConstWord(ConstScalar),
    Placeholder {
        class: YulValueClass<'db>,
    },
    Builtin(YBuiltin<'db>),
    Unary {
        op: UnOp,
        value: YLocalId,
    },
    Binary {
        op: BinOp,
        lhs: YLocalId,
        rhs: YLocalId,
    },
    Cast {
        value: YLocalId,
        to: ScalarClass<'db>,
    },
    ConstRef {
        region: ConstRegionId<'db>,
        layout: LayoutId<'db>,
    },
    AllocObject {
        layout: LayoutId<'db>,
        bytes: usize,
    },
    MaterializeToObject {
        src: YLocalId,
        layout: LayoutId<'db>,
    },
    MaterializePlaceToObject {
        place: YulPlace<'db>,
        layout: LayoutId<'db>,
    },
    ProviderFromRaw {
        raw: YLocalId,
        class: YulValueClass<'db>,
    },
    WordToRawAddr {
        value: YLocalId,
        class: YulValueClass<'db>,
    },
    ProviderToRaw {
        value: YLocalId,
    },
    AddrOf {
        place: YulPlace<'db>,
    },
    Load {
        place: YulPlace<'db>,
    },
    Call {
        callee: YFunctionId,
        args: Box<[YLocalId]>,
    },
    EnumMake {
        layout: LayoutId<'db>,
        variant: VariantId<'db>,
        fields: Box<[YLocalId]>,
    },
    EnumTagOfValue {
        value: YLocalId,
    },
    EnumIsVariant {
        value: YLocalId,
        variant: VariantId<'db>,
    },
    EnumExtract {
        value: YLocalId,
        variant: VariantId<'db>,
        field: FieldIndex,
    },
    EnumGetTag {
        root: YLocalId,
    },
    EnumAssertVariantRef {
        root: YLocalId,
        variant: VariantId<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YStmt<'db> {
    Assign {
        dst: YLocalId,
        expr: YExpr<'db>,
    },
    Call {
        callee: YFunctionId,
        args: Box<[YLocalId]>,
    },
    Builtin(YBuiltin<'db>),
    Store {
        dst: YulPlace<'db>,
        src: YLocalId,
    },
    CopyInto {
        dst: YulPlace<'db>,
        src: YLocalId,
    },
    EnumAssertVariant {
        value: YLocalId,
        variant: VariantId<'db>,
    },
    EnumSetTag {
        root: YLocalId,
        variant: VariantId<'db>,
    },
    EnumWriteVariant {
        root: YLocalId,
        variant: VariantId<'db>,
        fields: Box<[YLocalId]>,
    },
}

fn builtin_is_statement_only(builtin: &YBuiltin<'_>) -> bool {
    matches!(
        builtin,
        YBuiltin::Mstore { .. }
            | YBuiltin::Mstore8 { .. }
            | YBuiltin::Mcopy { .. }
            | YBuiltin::Sstore { .. }
            | YBuiltin::ReturnDataCopy { .. }
            | YBuiltin::CallDataCopy { .. }
            | YBuiltin::CodeCopy { .. }
            | YBuiltin::Log0 { .. }
            | YBuiltin::Log1 { .. }
            | YBuiltin::Log2 { .. }
            | YBuiltin::Log3 { .. }
            | YBuiltin::Log4 { .. }
    )
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum YTerminator<'db> {
    Goto(YBlockId),
    Branch {
        cond: YLocalId,
        then_bb: YBlockId,
        else_bb: YBlockId,
    },
    SwitchWord {
        discr: YLocalId,
        cases: Box<[(ConstScalar, YBlockId)]>,
        default: YBlockId,
    },
    MatchEnumTag {
        tag: YLocalId,
        enum_layout: LayoutId<'db>,
        cases: Box<[(VariantId<'db>, YBlockId)]>,
        default: Option<YBlockId>,
    },
    TerminalCall {
        callee: YFunctionId,
        args: Box<[YLocalId]>,
    },
    ReturnData {
        offset: YLocalId,
        len: YLocalId,
    },
    Revert {
        offset: YLocalId,
        len: YLocalId,
    },
    SelfDestruct {
        beneficiary: YLocalId,
    },
    Trap,
    Return(Option<YLocalId>),
    Stop,
}

impl<'db> YTransportInfo<'db> {
    fn empty() -> Self {
        Self {
            root_alias: None,
            leaves: Box::default(),
        }
    }

    fn root_alias(space: YulAddressSpace) -> Self {
        Self {
            root_alias: Some(space),
            leaves: Box::default(),
        }
    }

    fn with_replaced_root_alias(&self, root_alias: Option<YulAddressSpace>) -> Self {
        Self {
            root_alias,
            leaves: self.leaves.clone(),
        }
    }

    fn projected(&self, prefix: &YTransportPath<'db>) -> Self {
        let mut root_alias = if prefix.is_empty()
            || self
                .root_alias
                .is_some_and(|space| !matches!(space, YulAddressSpace::Memory))
        {
            self.root_alias
        } else {
            None
        };
        let mut leaves = Vec::new();
        for (path, space) in &self.leaves {
            let Some(suffix) = path.strip_prefix(prefix) else {
                continue;
            };
            if suffix.is_empty() {
                root_alias = Some(*space);
            } else {
                leaves.push((suffix, *space));
            }
        }
        Self {
            root_alias,
            leaves: leaves.into_boxed_slice(),
        }
    }

    fn without_default_memory_root(&self, default_class: Option<&YulValueClass<'db>>) -> Self {
        let root_alias = match (self.root_alias, default_class) {
            (Some(space), Some(class)) if Some(space) == yul_space_for_class(class) => None,
            (root_alias, _) => root_alias,
        };
        let leaves = self
            .leaves
            .iter()
            .filter(|(_, space)| !matches!(space, YulAddressSpace::Memory))
            .cloned()
            .collect();
        Self { root_alias, leaves }
    }

    fn actualize_bytes_copy_to_memory(&self) -> Self {
        let remap = |space| match space {
            YulAddressSpace::Memory | YulAddressSpace::Code | YulAddressSpace::Calldata => {
                YulAddressSpace::Memory
            }
            YulAddressSpace::Storage | YulAddressSpace::Transient => space,
        };
        Self {
            root_alias: self.root_alias.map(remap),
            leaves: self
                .leaves
                .iter()
                .map(|(path, space)| (path.clone(), remap(*space)))
                .collect(),
        }
    }
}

fn yul_space_suffix(space: YulAddressSpace) -> &'static str {
    match space {
        YulAddressSpace::Memory => "mem",
        YulAddressSpace::Storage => "stor",
        YulAddressSpace::Transient => "tstor",
        YulAddressSpace::Calldata => "calldata",
        YulAddressSpace::Code => "code",
    }
}

fn projection_path_suffix(path: &YTransportPath<'_>) -> String {
    if path.is_empty() {
        return "root".to_string();
    }
    path.iter()
        .map(|projection| match projection {
            Projection::Field(idx) => format!("f{idx}"),
            Projection::VariantField { field_idx, .. } => format!("vf{field_idx}"),
            Projection::Index(IndexSource::Constant(idx)) => format!("i{idx}"),
            Projection::Index(IndexSource::Dynamic(_)) => "idyn".to_string(),
            Projection::Discriminant => "discr".to_string(),
            Projection::Deref => "deref".to_string(),
        })
        .collect::<Vec<_>>()
        .join("_")
}

fn function_variant_suffix<'db>(key: &YulFunctionKey<'db>) -> String {
    let mut parts = Vec::new();
    for (idx, space) in key.effect_spaces.iter().enumerate() {
        if let Some(space) = space {
            parts.push(format!("eff{idx}_{}", yul_space_suffix(*space)));
        }
    }
    for (idx, transport) in key.param_transports.iter().enumerate() {
        if let Some(space) = transport.root_alias {
            parts.push(format!("arg{idx}_root_{}", yul_space_suffix(space)));
        }
        parts.extend(transport.leaves.iter().map(|(path, space)| {
            format!(
                "arg{idx}_{}_{}",
                projection_path_suffix(path),
                yul_space_suffix(*space)
            )
        }));
    }
    parts.join("_")
}

const YUL_FUNCTION_SYMBOL_STYLE: FunctionSymbolStyle = FunctionSymbolStyle {
    segment_separator: "$",
    variant_separator: "_",
    fallback_separator: "_",
    sanitize_segment: sanitize_yul_ident,
    namespace_key: prefix_yul_name,
};

fn assign_yul_function_symbols<'db>(
    db: &'db DriverDataBase,
    functions: &mut [YulFunctionPlan<'db>],
) {
    let inputs = functions
        .iter()
        .map(|func| FunctionSymbolInput {
            owner: func.runtime_function.owner(db).clone(),
            fallback_symbol: func.runtime_function.symbol(db).clone(),
            variant_suffix: func.variant_suffix.clone(),
            disambiguator: mir::runtime_instance_symbol_key(db, func.runtime_function.instance(db)),
        })
        .collect::<Vec<_>>();
    let symbols = assign_function_symbols(db, &inputs, &YUL_FUNCTION_SYMBOL_STYLE);
    for (func, symbol) in functions.iter_mut().zip(symbols) {
        func.symbol = symbol;
    }
}

fn specialize_yul_class_root<'db>(
    class: YulValueClass<'db>,
    root_alias: Option<YulAddressSpace>,
) -> YulValueClass<'db> {
    match (class, root_alias) {
        (YulValueClass::Word(class), _) => YulValueClass::Word(class),
        (class, None) => class,
        (class, Some(space)) => yul_ptr_class(
            space,
            layout_from_yul_class(&class)
                .expect("non-word Yul class must carry a layout for root specialization"),
        ),
    }
}

fn yul_space_for_class(class: &YulValueClass<'_>) -> Option<YulAddressSpace> {
    match class {
        YulValueClass::Word(_) => None,
        YulValueClass::MemoryPtr { .. } => Some(YulAddressSpace::Memory),
        YulValueClass::CodePtr { .. } => Some(YulAddressSpace::Code),
        YulValueClass::StoragePtr { .. } => Some(YulAddressSpace::Storage),
        YulValueClass::TransientPtr { .. } => Some(YulAddressSpace::Transient),
        YulValueClass::CalldataPtr { .. } => Some(YulAddressSpace::Calldata),
    }
}

fn yul_storage_kind_for_runtime_class(class: &RuntimeClass<'_>) -> YulStorageKind {
    match class {
        RuntimeClass::AggregateValue { .. } => YulStorageKind::Bytes,
        RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
            YulStorageKind::Cell
        }
    }
}

fn runtime_class_transport<'db>(class: &YulValueClass<'db>) -> YTransportInfo<'db> {
    yul_space_for_class(class)
        .map(YTransportInfo::root_alias)
        .unwrap_or_else(YTransportInfo::empty)
}

struct YulLegalizer<'pkg, 'db> {
    db: &'db DriverDataBase,
    package: &'pkg RuntimePackage<'db>,
    layout: TargetDataLayout,
    const_region_labels: Vec<(ConstRegionId<'db>, String)>,
    const_region_label_map: FxHashMap<ConstRegionId<'db>, String>,
    code_region_labels: Vec<(RuntimeCodeRegion<'db>, String)>,
    function_by_instance:
        std::collections::HashMap<mir::RuntimeInstance<'db>, RuntimeFunction<'db>>,
    function_variants: Vec<Option<YulFunctionPlan<'db>>>,
    function_variant_map: std::collections::HashMap<YulFunctionKey<'db>, YFunctionId>,
}

#[derive(Clone)]
struct LocalValueInfo<'db> {
    class: Option<YulValueClass<'db>>,
    transport: YTransportInfo<'db>,
    const_value: Option<ConstNode<'db>>,
}

#[derive(Clone, Debug, Default)]
struct PendingTransportTree<'db> {
    root_alias: Option<YulAddressSpace>,
    children: Vec<(
        Projection<LayoutId<'db>, VariantId<'db>, YLocalId>,
        PendingTransportTree<'db>,
    )>,
}

impl<'db> PendingTransportTree<'db> {
    fn from_transport(transport: &YTransportInfo<'db>) -> Self {
        let mut tree = Self {
            root_alias: transport.root_alias,
            children: Vec::new(),
        };
        for (path, space) in &transport.leaves {
            tree.insert_leaf(path.iter().cloned().collect::<Vec<_>>().as_slice(), *space);
        }
        tree
    }

    fn to_transport(&self) -> YTransportInfo<'db> {
        let mut leaves = Vec::new();
        self.collect_leaves(&YTransportPath::new(), &mut leaves);
        YTransportInfo {
            root_alias: self.root_alias,
            leaves: leaves.into_boxed_slice(),
        }
    }

    fn insert_leaf(
        &mut self,
        path: &[Projection<LayoutId<'db>, VariantId<'db>, YLocalId>],
        space: YulAddressSpace,
    ) {
        if let Some((first, rest)) = path.split_first() {
            self.child_mut(first.clone()).insert_leaf(rest, space);
        } else {
            self.root_alias = Some(space);
        }
    }

    fn overlay_transport(
        &mut self,
        path: &[Projection<LayoutId<'db>, VariantId<'db>, YLocalId>],
        transport: &YTransportInfo<'db>,
    ) {
        if let Some((first, rest)) = path.split_first() {
            self.child_mut(first.clone())
                .overlay_transport(rest, transport);
        } else {
            *self = Self::from_transport(transport);
        }
    }

    fn child_mut(
        &mut self,
        projection: Projection<LayoutId<'db>, VariantId<'db>, YLocalId>,
    ) -> &mut Self {
        if let Some(idx) = self
            .children
            .iter()
            .position(|(candidate, _)| *candidate == projection)
        {
            return &mut self.children[idx].1;
        }
        self.children.push((projection, Self::default()));
        &mut self
            .children
            .last_mut()
            .expect("newly pushed pending transport child should exist")
            .1
    }

    fn collect_leaves(
        &self,
        prefix: &YTransportPath<'db>,
        leaves: &mut Vec<(YTransportPath<'db>, YulAddressSpace)>,
    ) {
        for (projection, child) in &self.children {
            let child_prefix = prefix.concat(&YTransportPath::from_projection(projection.clone()));
            if let Some(space) = child.root_alias {
                leaves.push((child_prefix.clone(), space));
            }
            child.collect_leaves(&child_prefix, leaves);
        }
    }
}

struct PendingAggregateObject<'db> {
    local: RLocalId,
    layout: LayoutId<'db>,
    fallback_stmts: Vec<YStmt<'db>>,
    transport: PendingTransportTree<'db>,
    value_writes: FxHashMap<Box<[PlaceElem<'db>]>, RLocalId>,
    const_writes: FxHashMap<Box<[PlaceElem<'db>]>, ConstNode<'db>>,
}

impl<'pkg, 'db> YulLegalizer<'pkg, 'db> {
    fn new(
        db: &'db DriverDataBase,
        package: &'pkg RuntimePackage<'db>,
        layout: TargetDataLayout,
    ) -> Self {
        let const_region_labels = package
            .const_regions(db)
            .into_iter()
            .enumerate()
            .map(|(idx, region)| (region, format!("const_{idx}")))
            .collect::<Vec<_>>();
        Self {
            db,
            package,
            layout,
            const_region_label_map: const_region_labels
                .iter()
                .map(|(region, label)| (*region, label.clone()))
                .collect(),
            const_region_labels,
            code_region_labels: package
                .code_regions(db)
                .into_iter()
                .map(|resolved| (resolved.region(db), resolved.symbol(db).clone()))
                .collect(),
            function_by_instance: package
                .functions(db)
                .into_iter()
                .map(|function| (function.instance(db), function))
                .collect(),
            function_variants: Vec::new(),
            function_variant_map: std::collections::HashMap::new(),
        }
    }

    fn legalize(mut self) -> Result<YulPackage<'db>, YulError> {
        let root_names = self
            .package
            .root_objects(self.db)
            .iter()
            .map(|object| object.name(self.db).clone())
            .collect::<std::collections::HashSet<_>>();
        let objects = self
            .package
            .objects(self.db)
            .into_iter()
            .map(|object| {
                self.legalize_runtime_object(object, root_names.contains(&object.name(self.db)))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut functions = self
            .function_variants
            .into_iter()
            .map(|plan| {
                plan.ok_or_else(|| {
                    YulError::InvalidYulPackage(
                        "missing legalized function variant plan".to_string(),
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        assign_yul_function_symbols(self.db, &mut functions);
        functions.sort_by(|lhs, rhs| lhs.symbol.cmp(&rhs.symbol));
        Ok(YulPackage {
            top_mod: self.package.top_mod(self.db),
            functions,
            objects,
            const_region_labels: self.const_region_labels,
            code_region_labels: self.code_region_labels,
            primary_object: self
                .package
                .primary_object(self.db)
                .map(|object| object.name(self.db).clone()),
        })
    }
}

pub fn legalize_runtime_package<'db>(
    db: &'db DriverDataBase,
    package: &RuntimePackage<'db>,
    layout: TargetDataLayout,
) -> Result<YulPackage<'db>, YulError> {
    YulLegalizer::new(db, package, layout).legalize()
}

impl<'pkg, 'db> YulLegalizer<'pkg, 'db> {
    fn legalize_runtime_object(
        &mut self,
        object: RuntimeObject<'db>,
        root: bool,
    ) -> Result<YulObjectPlan<'db>, YulError> {
        let sections = object
            .sections(self.db)
            .iter()
            .cloned()
            .map(|section| self.legalize_runtime_section(object, section))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(YulObjectPlan {
            name: object.name(self.db).clone(),
            root,
            sections,
        })
    }

    fn legalize_runtime_section(
        &mut self,
        object: RuntimeObject<'db>,
        section: RuntimeSection<'db>,
    ) -> Result<YulSectionPlan<'db>, YulError> {
        let entry = self.ensure_function_variant(self.default_function_key(section.entry))?;
        let mut functions = self.collect_specialized_section_functions(entry);
        functions.sort_by(|lhs, rhs| {
            self.function_variants[lhs.index()]
                .as_ref()
                .unwrap()
                .symbol
                .cmp(&self.function_variants[rhs.index()].as_ref().unwrap().symbol)
        });
        let const_regions =
            self.collect_section_const_regions(section.const_regions.iter().copied(), &functions)?;
        let embeds = section
            .embeds
            .iter()
            .cloned()
            .map(|embed| {
                let (source_object, source_section) = match embed.source {
                    RuntimeSectionRef::Local { object, section }
                    | RuntimeSectionRef::External { object, section } => {
                        (object.name(self.db).clone(), section)
                    }
                };
                Ok(YulEmbedPlan {
                    source_object,
                    source_section,
                    label: embed.as_symbol,
                })
            })
            .collect::<Result<Vec<_>, YulError>>()?;
        Ok(YulSectionPlan {
            object_name: object.name(self.db).clone(),
            name: section.name,
            entry,
            functions,
            const_regions,
            embeds,
        })
    }

    fn collect_specialized_section_functions(&self, entry: YFunctionId) -> Vec<YFunctionId> {
        let mut seen = std::collections::HashSet::new();
        let mut stack = vec![entry];
        let mut out = Vec::new();
        while let Some(function) = stack.pop() {
            if !seen.insert(function) {
                continue;
            }
            out.push(function);
            let Some(plan) = self.function_variants[function.index()].as_ref() else {
                continue;
            };
            for block in &plan.blocks {
                for stmt in &block.stmts {
                    match stmt {
                        YStmt::Assign {
                            expr: YExpr::Call { callee, .. },
                            ..
                        }
                        | YStmt::Call { callee, .. } => stack.push(*callee),
                        YStmt::Assign { .. }
                        | YStmt::Builtin(_)
                        | YStmt::Store { .. }
                        | YStmt::CopyInto { .. }
                        | YStmt::EnumAssertVariant { .. }
                        | YStmt::EnumSetTag { .. }
                        | YStmt::EnumWriteVariant { .. } => {}
                    }
                }
                if let YTerminator::TerminalCall { callee, .. } = &block.terminator {
                    stack.push(*callee);
                }
            }
        }
        out
    }

    fn ensure_const_region_label(&mut self, region: ConstRegionId<'db>) -> String {
        if let Some(label) = self.const_region_label_map.get(&region) {
            return label.clone();
        }
        let label = format!("const_{}", self.const_region_labels.len());
        self.const_region_labels.push((region, label.clone()));
        self.const_region_label_map.insert(region, label.clone());
        label
    }

    fn yul_data_region_plan(
        &mut self,
        region: ConstRegionId<'db>,
    ) -> Result<YulDataRegionPlan<'db>, YulError> {
        let bytes = serialize_const_region_bytes(self.db, region, self.layout).map_err(|err| {
            YulError::ConstSerialization(format!(
                "failed to serialize const region `{region:?}` for Yul: {err}"
            ))
        })?;
        Ok(YulDataRegionPlan {
            region,
            label: self.ensure_const_region_label(region),
            bytes,
        })
    }

    fn collect_section_const_regions(
        &mut self,
        declared: impl Iterator<Item = ConstRegionId<'db>>,
        functions: &[YFunctionId],
    ) -> Result<Vec<YulDataRegionPlan<'db>>, YulError> {
        let mut seen = std::collections::HashSet::new();
        let mut regions = Vec::new();
        let function_regions = functions
            .iter()
            .flat_map(|function| self.function_const_regions(*function))
            .collect::<Vec<_>>();
        for region in declared.chain(function_regions) {
            if seen.insert(region) {
                regions.push(self.yul_data_region_plan(region)?);
            }
        }
        Ok(regions)
    }

    fn function_const_regions(&self, function: YFunctionId) -> Vec<ConstRegionId<'db>> {
        let Some(plan) = self.function_variants[function.index()].as_ref() else {
            return Vec::new();
        };
        let mut seen = std::collections::HashSet::new();
        let mut out = Vec::new();
        for block in &plan.blocks {
            for stmt in &block.stmts {
                if let YStmt::Assign {
                    expr: YExpr::ConstRef { region, .. },
                    ..
                } = stmt
                    && seen.insert(*region)
                {
                    out.push(*region);
                }
            }
        }
        out
    }

    fn default_function_key(&self, runtime_function: RuntimeFunction<'db>) -> YulFunctionKey<'db> {
        let param_kinds = self.param_kinds_for_function(runtime_function);
        let visible_count = param_kinds
            .iter()
            .filter(|kind| matches!(kind, YulParamKind::Visible(_)))
            .count();
        let effect_count = param_kinds
            .iter()
            .filter(|kind| matches!(kind, YulParamKind::Effect(_)))
            .count();
        YulFunctionKey {
            runtime_function,
            effect_spaces: vec![None; effect_count].into_boxed_slice(),
            param_transports: vec![YTransportInfo::empty(); visible_count].into_boxed_slice(),
        }
    }

    fn ensure_function_variant(
        &mut self,
        key: YulFunctionKey<'db>,
    ) -> Result<YFunctionId, YulError> {
        if let Some(id) = self.function_variant_map.get(&key) {
            return Ok(*id);
        }
        let id = YFunctionId(self.function_variants.len() as u32);
        self.function_variants.push(None);
        self.function_variant_map.insert(key.clone(), id);
        let plan = self.build_function_plan(id, &key)?;
        self.function_variants[id.index()] = Some(plan);
        Ok(id)
    }

    fn build_function_plan(
        &mut self,
        id: YFunctionId,
        key: &YulFunctionKey<'db>,
    ) -> Result<YulFunctionPlan<'db>, YulError> {
        let runtime_function = key.runtime_function;
        let instance = runtime_function.instance(self.db);
        let body = instance.body(self.db);
        let param_kinds = self.param_kinds_for_function(runtime_function);
        let mut local_values = body
            .locals
            .iter()
            .map(|local| LocalValueInfo {
                class: match &local.carrier {
                    mir::RuntimeCarrier::Erased => None,
                    mir::RuntimeCarrier::Value(class) => {
                        Some(yul_class_for_runtime_class(self.db, class))
                    }
                },
                transport: YTransportInfo::empty(),
                const_value: None,
            })
            .collect::<Vec<_>>();
        let code_backable_locals = self.code_backable_locals(runtime_function, &body);

        let mut param_locals = Vec::with_capacity(body.signature.params.len());
        let mut params = Vec::with_capacity(body.signature.params.len());
        for (param, kind) in body.signature.params.iter().zip(&param_kinds) {
            let default_class = yul_class_for_runtime_class(self.db, &param.class);
            let (class, transport) = match *kind {
                YulParamKind::Visible(idx) => {
                    let transport =
                        key.param_transports[idx].without_default_memory_root(Some(&default_class));
                    (
                        specialize_yul_class_root(default_class.clone(), transport.root_alias),
                        transport,
                    )
                }
                YulParamKind::Effect(idx) => {
                    let space = key.effect_spaces[idx];
                    (
                        specialize_yul_class_root(default_class.clone(), space),
                        space
                            .map(YTransportInfo::root_alias)
                            .unwrap_or_else(YTransportInfo::empty),
                    )
                }
            };
            let transport = yul_actual_transport(&class, &transport);
            local_values[param.local.as_u32() as usize] = LocalValueInfo {
                class: Some(class.clone()),
                transport: transport.clone(),
                const_value: None,
            };
            params.push(class);
            param_locals.push(YLocalId(param.local.as_u32()));
        }

        let mut blocks = Vec::with_capacity(body.blocks.len());
        for block in &body.blocks {
            let mut stmts = Vec::with_capacity(block.stmts.len());
            let mut pending_const_objects = Vec::new();
            for stmt in &block.stmts {
                self.flush_incompatible_pending_aggregates(
                    &mut pending_const_objects,
                    &mut local_values,
                    stmt,
                    &mut stmts,
                );
                if self.capture_pending_aggregate_stmt(
                    &body,
                    &code_backable_locals,
                    &mut local_values,
                    &mut pending_const_objects,
                    &mut stmts,
                    stmt,
                )? {
                    continue;
                }
                stmts.push(self.legalize_runtime_stmt(
                    &body,
                    &mut local_values,
                    &code_backable_locals,
                    stmt,
                )?);
            }
            self.flush_all_pending_aggregates(
                &mut pending_const_objects,
                &mut local_values,
                &mut stmts,
            );
            let terminator = self.legalize_runtime_terminator(&local_values, &block.terminator)?;
            blocks.push(YulBlock { stmts, terminator });
        }

        let ret_transport = body.signature.ret.as_ref().and_then(|_| {
            let mut observed = None::<YTransportInfo<'db>>;
            for block in &body.blocks {
                let RTerminator::Return(Some(value)) = &block.terminator else {
                    continue;
                };
                let transport = local_values[value.as_u32() as usize].transport.clone();
                match &observed {
                    None => observed = Some(transport),
                    Some(current) if current == &transport => {}
                    Some(_) => return Some(YTransportInfo::empty()),
                }
            }
            observed
        });
        let ret = body.signature.ret.as_ref().map(|class| {
            let default_class = yul_class_for_runtime_class(self.db, class);
            let transport = ret_transport
                .as_ref()
                .map(|transport| transport.without_default_memory_root(Some(&default_class)))
                .unwrap_or_else(YTransportInfo::empty);
            specialize_yul_class_root(default_class, transport.root_alias)
        });
        let locals = body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, local)| {
                let info = &local_values[idx];
                let root = self.legalize_local_root(local, info);
                YulLocal {
                    class: info.class.clone(),
                    root,
                    transport: info.transport.clone(),
                }
            })
            .collect();

        let variant_suffix = function_variant_suffix(key);
        let symbol = if variant_suffix.is_empty() {
            runtime_function.symbol(self.db).clone()
        } else {
            format!("{}_{variant_suffix}", runtime_function.symbol(self.db))
        };
        Ok(YulFunctionPlan {
            id,
            runtime_function,
            symbol,
            variant_suffix,
            linkage: runtime_function.linkage(self.db).clone(),
            inline_hint: runtime_function.inline_hint(self.db),
            param_kinds,
            params,
            ret,
            ret_transport,
            param_locals,
            locals,
            blocks,
        })
    }

    fn code_backable_locals(
        &self,
        runtime_function: RuntimeFunction<'db>,
        body: &mir::RuntimeBody<'db>,
    ) -> Vec<bool> {
        let Some(semantic) = runtime_function
            .instance(self.db)
            .key(self.db)
            .semantic(self.db)
        else {
            return vec![false; body.locals.len()];
        };
        let normalized = normalize_semantic_body(self.db, semantic)
            .expect("semantic normalization should succeed before Yul legalization");
        let semantic_len = normalized.locals.len();
        let mut code_backable = body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, local)| {
                let aggregate_like = match &local.carrier {
                    mir::RuntimeCarrier::Value(class) => {
                        Some(yul_class_for_runtime_class(self.db, class))
                    }
                    mir::RuntimeCarrier::Erased => None,
                }
                .is_some_and(|class| {
                    matches!(
                        class,
                        YulValueClass::MemoryPtr { .. } | YulValueClass::CodePtr { .. }
                    )
                });
                aggregate_like
                    && if idx < semantic_len {
                        normalized.locals[idx].mutability == Mutability::Immutable
                    } else {
                        true
                    }
            })
            .collect::<Vec<_>>();
        let mut mark_escaped = |value: RValueId| {
            if let Some(local) = code_backable.get_mut(value.as_u32() as usize) {
                *local = false;
            }
        };
        for block in &body.blocks {
            for stmt in &block.stmts {
                match stmt {
                    RStmt::Assign {
                        expr: RExpr::AddrOf { place },
                        ..
                    } => {
                        if let PlaceRoot::Ref(value) = place.root {
                            mark_escaped(value);
                        }
                    }
                    RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } => {
                        for arg in self.call_args_requiring_materialization(*callee, args) {
                            mark_escaped(arg);
                        }
                    }
                    RStmt::Assign {
                        expr: RExpr::MaterializeToObject { src },
                        ..
                    } => mark_escaped(*src),
                    RStmt::Assign { .. } | RStmt::EnumAssertVariant { .. } => {}
                    RStmt::Store { dst, .. } | RStmt::CopyInto { dst, .. } => {
                        if let PlaceRoot::Ref(value) = dst.root {
                            mark_escaped(value);
                        }
                    }
                    RStmt::EnumSetTag { root, .. } | RStmt::EnumWriteVariant { root, .. } => {
                        mark_escaped(*root);
                    }
                }
            }
            match &block.terminator {
                RTerminator::TerminalCall { callee, args } => {
                    for arg in self.call_args_requiring_materialization(*callee, args) {
                        mark_escaped(arg);
                    }
                }
                RTerminator::Return(Some(value)) => mark_escaped(*value),
                RTerminator::Goto(_)
                | RTerminator::Branch { .. }
                | RTerminator::SwitchScalar { .. }
                | RTerminator::MatchEnumTag { .. }
                | RTerminator::ReturnData { .. }
                | RTerminator::Revert { .. }
                | RTerminator::SelfDestruct { .. }
                | RTerminator::Trap
                | RTerminator::Return(None)
                | RTerminator::Stop => {}
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            for block in &body.blocks {
                for stmt in &block.stmts {
                    let RStmt::Assign { dst, expr } = stmt else {
                        continue;
                    };
                    let src = match expr {
                        RExpr::Use(src) | RExpr::RetagRef { value: src } => *src,
                        RExpr::ConstScalar(_)
                        | RExpr::Placeholder { .. }
                        | RExpr::Builtin(_)
                        | RExpr::Unary { .. }
                        | RExpr::Binary { .. }
                        | RExpr::Cast { .. }
                        | RExpr::ConstRef { .. }
                        | RExpr::AllocObject { .. }
                        | RExpr::MaterializeToObject { .. }
                        | RExpr::MaterializePlaceToObject { .. }
                        | RExpr::ProviderFromRaw { .. }
                        | RExpr::WordToRawAddr { .. }
                        | RExpr::ProviderToRaw { .. }
                        | RExpr::AddrOf { .. }
                        | RExpr::Load { .. }
                        | RExpr::Call { .. }
                        | RExpr::EnumMake { .. }
                        | RExpr::EnumTagOfValue { .. }
                        | RExpr::EnumIsVariant { .. }
                        | RExpr::EnumExtract { .. }
                        | RExpr::EnumGetTag { .. }
                        | RExpr::EnumAssertVariantRef { .. } => continue,
                    };
                    if !code_backable
                        .get(dst.as_u32() as usize)
                        .copied()
                        .unwrap_or(false)
                        && let Some(src) = code_backable.get_mut(src.as_u32() as usize)
                        && *src
                    {
                        *src = false;
                        changed = true;
                    }
                }
            }
        }
        code_backable
    }

    fn call_args_requiring_materialization(
        &self,
        callee: RuntimeInstance<'db>,
        args: &[RValueId],
    ) -> Vec<RValueId> {
        let Some(runtime_function) = self.function_by_instance.get(&callee).copied() else {
            return args.to_vec();
        };
        let param_kinds = self.param_kinds_for_function(runtime_function);
        let body = runtime_function.instance(self.db).body(self.db);
        if args.len() != body.signature.params.len() || args.len() != param_kinds.len() {
            return args.to_vec();
        }
        args.iter()
            .zip(param_kinds)
            .zip(body.signature.params.iter())
            .filter_map(|((arg, kind), param)| match kind {
                YulParamKind::Visible(_)
                    if self.visible_param_preserves_code_backing(
                        runtime_function,
                        param.local,
                        &param.class,
                    ) =>
                {
                    None
                }
                YulParamKind::Visible(_) | YulParamKind::Effect(_) => Some(*arg),
            })
            .collect()
    }

    fn visible_param_preserves_code_backing(
        &self,
        runtime_function: RuntimeFunction<'db>,
        local: RLocalId,
        class: &RuntimeClass<'db>,
    ) -> bool {
        if !matches!(
            yul_class_for_runtime_class(self.db, class),
            YulValueClass::MemoryPtr { .. } | YulValueClass::CodePtr { .. }
        ) {
            return false;
        }
        match runtime_function.owner(self.db) {
            RuntimeFunctionOwner::Semantic(semantic) => normalize_semantic_body(self.db, semantic)
                .expect("semantic normalization should succeed before Yul legalization")
                .locals
                .get(local.as_u32() as usize)
                .is_some_and(|local| local.mutability == Mutability::Immutable),
            RuntimeFunctionOwner::Synthetic(_) => true,
        }
    }

    fn flush_incompatible_pending_aggregates(
        &self,
        pending: &mut Vec<PendingAggregateObject<'db>>,
        local_values: &mut [LocalValueInfo<'db>],
        stmt: &RStmt<'db>,
        stmts: &mut Vec<YStmt<'db>>,
    ) {
        let mut idx = 0;
        while idx < pending.len() {
            if self.stmt_uses_pending_const_object(stmt, pending[idx].local) {
                idx += 1;
            } else {
                self.flush_pending_aggregate(pending, idx, local_values, stmts);
            }
        }
    }

    fn flush_all_pending_aggregates(
        &self,
        pending: &mut Vec<PendingAggregateObject<'db>>,
        local_values: &mut [LocalValueInfo<'db>],
        stmts: &mut Vec<YStmt<'db>>,
    ) {
        while !pending.is_empty() {
            self.flush_pending_aggregate(pending, 0, local_values, stmts);
        }
    }

    fn flush_pending_aggregate(
        &self,
        pending: &mut Vec<PendingAggregateObject<'db>>,
        idx: usize,
        local_values: &mut [LocalValueInfo<'db>],
        stmts: &mut Vec<YStmt<'db>>,
    ) {
        let object = pending.remove(idx);
        let info = &local_values[object.local.as_u32() as usize];
        if matches!(info.class, Some(YulValueClass::Word(_)))
            && info.transport.root_alias.is_none()
            && info.transport.leaves.is_empty()
            && let Some((expr, class, const_value)) =
                self.try_build_pending_scalar_value(&object, local_values)
        {
            local_values[object.local.as_u32() as usize] = LocalValueInfo {
                class: Some(class),
                transport: YTransportInfo::empty(),
                const_value,
            };
            stmts.push(YStmt::Assign {
                dst: YLocalId(object.local.as_u32()),
                expr,
            });
            return;
        }
        local_values[object.local.as_u32() as usize].transport = object.transport.to_transport();
        stmts.extend(object.fallback_stmts);
    }

    fn stmt_uses_pending_const_object(&self, stmt: &RStmt<'db>, local: RLocalId) -> bool {
        match stmt {
            RStmt::Assign {
                expr:
                    RExpr::Load {
                        place:
                            mir::RuntimePlace {
                                root: PlaceRoot::Ref(value),
                                path,
                            },
                    },
                ..
            } => *value == local && path.is_empty(),
            RStmt::Store { dst, .. } | RStmt::CopyInto { dst, .. } => {
                matches!(dst.root, PlaceRoot::Ref(value) if value == local)
            }
            RStmt::Assign { .. }
            | RStmt::EnumAssertVariant { .. }
            | RStmt::EnumSetTag { .. }
            | RStmt::EnumWriteVariant { .. } => false,
        }
    }

    fn pending_aggregate_index(
        pending: &[PendingAggregateObject<'db>],
        local: RLocalId,
    ) -> Option<usize> {
        pending.iter().position(|object| object.local == local)
    }

    fn capture_pending_aggregate_stmt(
        &mut self,
        body: &mir::RuntimeBody<'db>,
        code_backable_locals: &[bool],
        local_values: &mut [LocalValueInfo<'db>],
        pending: &mut Vec<PendingAggregateObject<'db>>,
        stmts: &mut Vec<YStmt<'db>>,
        stmt: &RStmt<'db>,
    ) -> Result<bool, YulError> {
        match stmt {
            RStmt::Assign {
                dst,
                expr: RExpr::AllocObject { layout },
            } => {
                let (expr, class, transport, const_value) = self.legalize_runtime_expr(
                    body,
                    local_values,
                    code_backable_locals,
                    Some(*dst),
                    &RExpr::AllocObject { layout: *layout },
                )?;
                local_values[dst.as_u32() as usize] = LocalValueInfo {
                    class,
                    transport,
                    const_value,
                };
                pending.push(PendingAggregateObject {
                    local: *dst,
                    layout: *layout,
                    fallback_stmts: vec![YStmt::Assign {
                        dst: YLocalId(dst.as_u32()),
                        expr,
                    }],
                    transport: PendingTransportTree::from_transport(&runtime_class_transport(
                        &YulValueClass::MemoryPtr { layout: *layout },
                    )),
                    value_writes: FxHashMap::default(),
                    const_writes: FxHashMap::default(),
                });
                Ok(true)
            }
            RStmt::Store { dst, src } => {
                let Some(local) = self.const_object_handle_root(dst) else {
                    return Ok(false);
                };
                let Some(idx) = Self::pending_aggregate_index(pending, local) else {
                    return Ok(false);
                };
                let (dst_place, _) = self.legalize_place(body, local_values, dst)?;
                let fallback = YStmt::Store {
                    dst: dst_place.clone(),
                    src: YLocalId(src.as_u32()),
                };
                let Some(const_path) = self.const_object_path(dst) else {
                    self.flush_pending_aggregate(pending, idx, local_values, stmts);
                    return Ok(false);
                };
                let Some(transport_path) = self.transport_path_for_yul_place(&dst_place) else {
                    self.flush_pending_aggregate(pending, idx, local_values, stmts);
                    return Ok(false);
                };
                let src_transport = match dst_place.storage_kind {
                    YulStorageKind::Bytes => local_values[src.as_u32() as usize]
                        .transport
                        .actualize_bytes_copy_to_memory(),
                    YulStorageKind::Cell => local_values[src.as_u32() as usize].transport.clone(),
                };
                pending[idx].fallback_stmts.push(fallback);
                pending[idx].transport.overlay_transport(
                    transport_path
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                    &src_transport,
                );
                pending[idx].value_writes.insert(const_path.clone(), *src);
                if let Some(node) = local_values[src.as_u32() as usize].const_value.clone() {
                    pending[idx].const_writes.insert(const_path, node);
                }
                Ok(true)
            }
            RStmt::CopyInto { dst, src } => {
                let Some(local) = self.const_object_handle_root(dst) else {
                    return Ok(false);
                };
                let Some(idx) = Self::pending_aggregate_index(pending, local) else {
                    return Ok(false);
                };
                let (dst_place, _) = self.legalize_place(body, local_values, dst)?;
                let fallback = YStmt::CopyInto {
                    dst: dst_place.clone(),
                    src: YLocalId(src.as_u32()),
                };
                let Some(const_path) = self.const_object_path(dst) else {
                    self.flush_pending_aggregate(pending, idx, local_values, stmts);
                    return Ok(false);
                };
                let Some(transport_path) = self.transport_path_for_yul_place(&dst_place) else {
                    self.flush_pending_aggregate(pending, idx, local_values, stmts);
                    return Ok(false);
                };
                let src_transport = match dst_place.storage_kind {
                    YulStorageKind::Bytes => local_values[src.as_u32() as usize]
                        .transport
                        .actualize_bytes_copy_to_memory(),
                    YulStorageKind::Cell => local_values[src.as_u32() as usize].transport.clone(),
                };
                pending[idx].fallback_stmts.push(fallback);
                pending[idx].transport.overlay_transport(
                    transport_path
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                    &src_transport,
                );
                pending[idx].value_writes.insert(const_path.clone(), *src);
                if let Some(node) = local_values[src.as_u32() as usize].const_value.clone() {
                    pending[idx].const_writes.insert(const_path, node);
                }
                Ok(true)
            }
            RStmt::Assign {
                dst,
                expr: RExpr::Load { place },
            } => {
                let Some(local) = self.const_object_handle_root(place) else {
                    return Ok(false);
                };
                if !place.path.is_empty() {
                    return Ok(false);
                }
                let Some(idx) = Self::pending_aggregate_index(pending, local) else {
                    return Ok(false);
                };
                if pending[idx].value_writes.is_empty()
                    && pending[idx].const_writes.is_empty()
                    && pending[idx].transport.to_transport().leaves.is_empty()
                {
                    self.flush_pending_aggregate(pending, idx, local_values, stmts);
                    return Ok(false);
                }
                if code_backable_locals
                    .get(dst.as_u32() as usize)
                    .copied()
                    .unwrap_or(false)
                    && let Some(region) = self.try_finalize_pending_const_object(&pending[idx])?
                {
                    let layout = pending[idx].layout;
                    let class = YulValueClass::CodePtr { layout };
                    let transport = runtime_class_transport(&class);
                    let const_value = Some(region.value(self.db).clone());
                    local_values[dst.as_u32() as usize] = LocalValueInfo {
                        class: Some(class),
                        transport: transport.clone(),
                        const_value,
                    };
                    pending.remove(idx);
                    stmts.push(YStmt::Assign {
                        dst: YLocalId(dst.as_u32()),
                        expr: YExpr::ConstRef { region, layout },
                    });
                    return Ok(true);
                }
                if let Some((expr, class, const_value)) =
                    self.try_build_pending_scalar_value(&pending[idx], local_values)
                {
                    pending.remove(idx);
                    local_values[dst.as_u32() as usize] = LocalValueInfo {
                        class: Some(class),
                        transport: YTransportInfo::empty(),
                        const_value,
                    };
                    stmts.push(YStmt::Assign {
                        dst: YLocalId(dst.as_u32()),
                        expr,
                    });
                    return Ok(true);
                }
                let yul_class = body
                    .value_class(*dst)
                    .map(|class| yul_class_for_runtime_class(self.db, class));
                let transport = pending[idx].transport.to_transport();
                let yul_place = self.legalize_place(body, local_values, place)?.0;
                local_values[dst.as_u32() as usize] = LocalValueInfo {
                    class: yul_class.clone(),
                    transport,
                    const_value: None,
                };
                self.flush_pending_aggregate(pending, idx, local_values, stmts);
                stmts.push(YStmt::Assign {
                    dst: YLocalId(dst.as_u32()),
                    expr: YExpr::Load { place: yul_place },
                });
                Ok(true)
            }
            RStmt::Assign { .. }
            | RStmt::EnumAssertVariant { .. }
            | RStmt::EnumSetTag { .. }
            | RStmt::EnumWriteVariant { .. } => Ok(false),
        }
    }

    fn const_object_handle_root(&self, place: &mir::RuntimePlace<'db>) -> Option<RLocalId> {
        match place.root {
            PlaceRoot::Ref(local) => Some(local),
            PlaceRoot::Slot(_) | PlaceRoot::Provider(_) | PlaceRoot::Ptr { .. } => None,
        }
    }

    fn const_object_path(&self, place: &mir::RuntimePlace<'db>) -> Option<Box<[PlaceElem<'db>]>> {
        let mut path = Vec::with_capacity(place.path.len());
        for elem in place.path.iter() {
            match elem {
                PlaceElem::Field(field) => path.push(PlaceElem::Field(*field)),
                PlaceElem::Index(IndexSource::Constant(value)) => {
                    path.push(PlaceElem::Index(IndexSource::Constant(*value)));
                }
                PlaceElem::VariantField { variant, field } => path.push(PlaceElem::VariantField {
                    variant: *variant,
                    field: *field,
                }),
                PlaceElem::Index(IndexSource::Dynamic(_)) | PlaceElem::Deref => return None,
            }
        }
        Some(path.into_boxed_slice())
    }

    fn transport_path_for_yul_place(&self, place: &YulPlace<'db>) -> Option<YTransportPath<'db>> {
        let mut path = YTransportPath::new();
        for elem in place.path.iter() {
            match elem {
                YulPlaceElem::Field { field, .. } => {
                    path.push(Projection::Field(field.0 as usize));
                }
                YulPlaceElem::Index { index, .. } => {
                    let IndexSource::Constant(value) = index else {
                        return None;
                    };
                    path.push(Projection::Index(IndexSource::Constant(*value)));
                }
                YulPlaceElem::VariantField { variant, field, .. } => {
                    path.push(Projection::VariantField {
                        variant: *variant,
                        enum_ty: variant.enum_layout,
                        field_idx: field.0 as usize,
                    });
                }
                YulPlaceElem::Deref { .. } => return None,
            }
        }
        Some(path)
    }

    fn try_finalize_pending_const_object(
        &mut self,
        object: &PendingAggregateObject<'db>,
    ) -> Result<Option<ConstRegionId<'db>>, YulError> {
        let Some(value) = self.build_const_node_for_class(
            &RuntimeClass::AggregateValue {
                layout: object.layout,
            },
            &[],
            &object.const_writes,
        ) else {
            return Ok(None);
        };
        let region = ConstRegionId::new(self.db, object.layout, value);
        self.ensure_const_region_label(region);
        Ok(Some(region))
    }

    fn try_build_pending_scalar_value(
        &self,
        object: &PendingAggregateObject<'db>,
        local_values: &[LocalValueInfo<'db>],
    ) -> Option<(YExpr<'db>, YulValueClass<'db>, Option<ConstNode<'db>>)> {
        self.build_scalar_value_for_class(
            &RuntimeClass::AggregateValue {
                layout: object.layout,
            },
            &[],
            local_values,
            &object.value_writes,
            &object.const_writes,
        )
    }

    fn build_scalar_value_for_class(
        &self,
        class: &RuntimeClass<'db>,
        path: &[PlaceElem<'db>],
        local_values: &[LocalValueInfo<'db>],
        value_writes: &FxHashMap<Box<[PlaceElem<'db>]>, RLocalId>,
        const_writes: &FxHashMap<Box<[PlaceElem<'db>]>, ConstNode<'db>>,
    ) -> Option<(YExpr<'db>, YulValueClass<'db>, Option<ConstNode<'db>>)> {
        if let Some(src) = value_writes.get(path)
            && let Some(class) = local_values[src.as_u32() as usize].class.clone()
            && matches!(class, YulValueClass::Word(_))
        {
            return Some((
                YExpr::Use(YLocalId(src.as_u32())),
                class,
                local_values[src.as_u32() as usize].const_value.clone(),
            ));
        }
        if let Some(node) = const_writes.get(path) {
            return self.build_scalar_value_from_const_node(class, node);
        }
        match class {
            RuntimeClass::AggregateValue { layout } => match layout.data(self.db) {
                Layout::Struct(data) if data.fields.len() == 1 => {
                    let mut child = path.to_vec();
                    child.push(PlaceElem::Field(FieldIndex(0)));
                    self.build_scalar_value_for_class(
                        &data.fields[0],
                        &child,
                        local_values,
                        value_writes,
                        const_writes,
                    )
                }
                Layout::Array(data) if data.len == 1 => {
                    let mut child = path.to_vec();
                    child.push(PlaceElem::Index(IndexSource::Constant(0)));
                    self.build_scalar_value_for_class(
                        &data.elem,
                        &child,
                        local_values,
                        value_writes,
                        const_writes,
                    )
                }
                Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_) => None,
            },
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                None
            }
        }
    }

    fn build_scalar_value_from_const_node(
        &self,
        class: &RuntimeClass<'db>,
        node: &ConstNode<'db>,
    ) -> Option<(YExpr<'db>, YulValueClass<'db>, Option<ConstNode<'db>>)> {
        match (class, node) {
            (RuntimeClass::Scalar(class), ConstNode::Scalar(value)) => Some((
                YExpr::ConstWord(value.clone()),
                YulValueClass::Word(class.clone()),
                Some(ConstNode::Scalar(value.clone())),
            )),
            (RuntimeClass::AggregateValue { layout }, ConstNode::Scalar(_)) => {
                match layout.data(self.db) {
                    Layout::Struct(data) if data.fields.len() == 1 => {
                        self.build_scalar_value_from_const_node(&data.fields[0], node)
                    }
                    Layout::Array(data) if data.len == 1 => {
                        self.build_scalar_value_from_const_node(&data.elem, node)
                    }
                    Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_) => None,
                }
            }
            (RuntimeClass::AggregateValue { layout }, ConstNode::Aggregate { fields, .. }) => {
                match layout.data(self.db) {
                    Layout::Struct(data) if data.fields.len() == 1 => {
                        self.build_scalar_value_from_const_node(&data.fields[0], fields.first()?)
                    }
                    Layout::Array(data) if data.len == 1 => {
                        self.build_scalar_value_from_const_node(&data.elem, fields.first()?)
                    }
                    Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_) => None,
                }
            }
            (RuntimeClass::Scalar(_), ConstNode::Aggregate { .. })
            | (RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. }, _) => None,
        }
    }

    fn reify_const_node_for_class(
        &self,
        class: &RuntimeClass<'db>,
        node: &ConstNode<'db>,
    ) -> Option<ConstNode<'db>> {
        match (class, node) {
            (RuntimeClass::Scalar(_), ConstNode::Scalar(_))
            | (RuntimeClass::AggregateValue { .. }, ConstNode::Aggregate { .. }) => {
                Some(node.clone())
            }
            (RuntimeClass::AggregateValue { layout }, ConstNode::Scalar(_)) => {
                match layout.data(self.db) {
                    Layout::Struct(data) if data.fields.len() == 1 => Some(ConstNode::Aggregate {
                        layout: *layout,
                        fields: vec![self.reify_const_node_for_class(&data.fields[0], node)?]
                            .into_boxed_slice(),
                    }),
                    Layout::Array(data) if data.len == 1 => Some(ConstNode::Aggregate {
                        layout: *layout,
                        fields: vec![self.reify_const_node_for_class(&data.elem, node)?]
                            .into_boxed_slice(),
                    }),
                    Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_) => None,
                }
            }
            (
                RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
                ConstNode::Aggregate { .. },
            ) => None,
            (RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. }, ConstNode::Scalar(_)) => None,
        }
    }

    fn build_const_node_for_class(
        &self,
        class: &RuntimeClass<'db>,
        path: &[PlaceElem<'db>],
        writes: &FxHashMap<Box<[PlaceElem<'db>]>, ConstNode<'db>>,
    ) -> Option<ConstNode<'db>> {
        if let Some(node) = writes.get(path) {
            return self.reify_const_node_for_class(class, node);
        }
        match class {
            RuntimeClass::AggregateValue { layout } => {
                self.build_const_node_for_layout(*layout, path, writes)
            }
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                None
            }
        }
    }

    fn build_const_node_for_layout(
        &self,
        layout: LayoutId<'db>,
        path: &[PlaceElem<'db>],
        writes: &FxHashMap<Box<[PlaceElem<'db>]>, ConstNode<'db>>,
    ) -> Option<ConstNode<'db>> {
        if let Some(node) = writes.get(path) {
            return Some(node.clone());
        }
        match layout.data(self.db) {
            Layout::Struct(data) => Some(ConstNode::Aggregate {
                layout,
                fields: data
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(idx, field)| {
                        let mut child = path.to_vec();
                        child.push(PlaceElem::Field(FieldIndex(idx as u16)));
                        self.build_const_node_for_class(field, &child, writes)
                    })
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            }),
            Layout::Array(data) => Some(ConstNode::Aggregate {
                layout,
                fields: (0..data.len)
                    .map(|idx| {
                        let mut child = path.to_vec();
                        child.push(PlaceElem::Index(IndexSource::Constant(idx as usize)));
                        self.build_const_node_for_class(&data.elem, &child, writes)
                    })
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            }),
            Layout::Enum(_) => None,
        }
    }

    fn param_kinds_for_function(&self, function: RuntimeFunction<'db>) -> Vec<YulParamKind> {
        let mut visible_idx = 0;
        let mut effect_idx = 0;
        let body = function.instance(self.db).body(self.db);
        match function.owner(self.db) {
            RuntimeFunctionOwner::Semantic(semantic) => {
                let semantic_body = semantic.body(self.db);
                body.signature
                    .params
                    .iter()
                    .map(|param| {
                        let source = semantic_body
                            .locals
                            .get(param.local.as_u32() as usize)
                            .and_then(|local| local.source);
                        match source {
                            Some(LocalBinding::EffectParam { .. })
                            | Some(LocalBinding::Param {
                                site: ParamSite::EffectField(_),
                                ..
                            }) => {
                                let idx = effect_idx;
                                effect_idx += 1;
                                YulParamKind::Effect(idx)
                            }
                            Some(LocalBinding::Local { .. })
                            | Some(LocalBinding::Param { .. })
                            | None => {
                                let idx = visible_idx;
                                visible_idx += 1;
                                YulParamKind::Visible(idx)
                            }
                        }
                    })
                    .collect()
            }
            RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractInitAbi { plan }) => {
                let effect_start = body
                    .signature
                    .params
                    .len()
                    .saturating_sub(plan.entry_effect_args.len());
                body.signature
                    .params
                    .iter()
                    .enumerate()
                    .map(|(idx, _)| {
                        if idx >= effect_start {
                            let effect = effect_idx;
                            effect_idx += 1;
                            YulParamKind::Effect(effect)
                        } else {
                            let visible = visible_idx;
                            visible_idx += 1;
                            YulParamKind::Visible(visible)
                        }
                    })
                    .collect()
            }
            RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { plan }) => {
                let effect_start = body
                    .signature
                    .params
                    .len()
                    .saturating_sub(plan.entry_effect_args.len());
                body.signature
                    .params
                    .iter()
                    .enumerate()
                    .map(|(idx, _)| {
                        if idx >= effect_start {
                            let effect = effect_idx;
                            effect_idx += 1;
                            YulParamKind::Effect(effect)
                        } else {
                            let visible = visible_idx;
                            visible_idx += 1;
                            YulParamKind::Visible(visible)
                        }
                    })
                    .collect()
            }
            RuntimeFunctionOwner::Synthetic(
                RuntimeSyntheticSpec::MainRoot { .. }
                | RuntimeSyntheticSpec::TestRoot { .. }
                | RuntimeSyntheticSpec::ManualContractRoot { .. }
                | RuntimeSyntheticSpec::ContractInitRoot { .. }
                | RuntimeSyntheticSpec::ContractRuntimeRoot { .. }
                | RuntimeSyntheticSpec::CodeRegionRoot { .. },
            ) => body
                .signature
                .params
                .iter()
                .enumerate()
                .map(|_| {
                    let idx = visible_idx;
                    visible_idx += 1;
                    YulParamKind::Visible(idx)
                })
                .collect(),
        }
    }

    fn effect_space_for_arg(&self, info: &LocalValueInfo<'db>) -> Option<YulAddressSpace> {
        info.transport
            .root_alias
            .filter(|space| !matches!(space, YulAddressSpace::Memory))
            .or_else(|| info.class.as_ref().and_then(yul_space_for_class))
            .filter(|space| !matches!(space, YulAddressSpace::Memory))
    }

    fn transport_for_arg(&self, info: &LocalValueInfo<'db>) -> YTransportInfo<'db> {
        let Some(class) = info.class.as_ref() else {
            return info.transport.clone();
        };
        yul_actual_transport(class, &info.transport)
    }

    fn specialize_call_key(
        &self,
        runtime_function: RuntimeFunction<'db>,
        args: &[RValueId],
        local_values: &[LocalValueInfo<'db>],
    ) -> YulFunctionKey<'db> {
        let param_kinds = self.param_kinds_for_function(runtime_function);
        let body = runtime_function.instance(self.db).body(self.db);
        let mut key = self.default_function_key(runtime_function);
        for ((arg, kind), param) in args
            .iter()
            .zip(param_kinds)
            .zip(body.signature.params.iter())
        {
            match kind {
                YulParamKind::Visible(idx) => {
                    let default_class = yul_class_for_runtime_class(self.db, &param.class);
                    key.param_transports[idx] = self
                        .transport_for_arg(&local_values[arg.as_u32() as usize])
                        .without_default_memory_root(Some(&default_class));
                }
                YulParamKind::Effect(idx) => {
                    key.effect_spaces[idx] =
                        self.effect_space_for_arg(&local_values[arg.as_u32() as usize]);
                }
            }
        }
        key
    }

    fn legalize_runtime_stmt(
        &mut self,
        body: &mir::RuntimeBody<'db>,
        local_values: &mut [LocalValueInfo<'db>],
        code_backable_locals: &[bool],
        stmt: &RStmt<'db>,
    ) -> Result<YStmt<'db>, YulError> {
        Ok(match stmt {
            RStmt::Assign { dst, expr } => {
                if let RExpr::Builtin(builtin) = expr {
                    let builtin = legalize_builtin(self.db, builtin)?;
                    if builtin_is_statement_only(&builtin) {
                        local_values[dst.as_u32() as usize] = LocalValueInfo {
                            class: None,
                            transport: YTransportInfo::empty(),
                            const_value: None,
                        };
                        return Ok(YStmt::Builtin(builtin));
                    }
                }
                if let RExpr::Call { callee, args } = expr
                    && body.value_class(*dst).is_none()
                {
                    let runtime_function =
                        *self.function_by_instance.get(callee).ok_or_else(|| {
                            YulError::InvalidYulPackage(format!(
                                "missing declared function for runtime call target `{callee:?}`"
                            ))
                        })?;
                    let key = self.specialize_call_key(runtime_function, args, local_values);
                    let callee = self.ensure_function_variant(key)?;
                    local_values[dst.as_u32() as usize] = LocalValueInfo {
                        class: None,
                        transport: YTransportInfo::empty(),
                        const_value: None,
                    };
                    return Ok(YStmt::Call {
                        callee,
                        args: map_values(args),
                    });
                }
                let (expr, class, transport, const_value) = self.legalize_runtime_expr(
                    body,
                    local_values,
                    code_backable_locals,
                    Some(*dst),
                    expr,
                )?;
                local_values[dst.as_u32() as usize] = LocalValueInfo {
                    class,
                    transport,
                    const_value,
                };
                YStmt::Assign {
                    dst: YLocalId(dst.as_u32()),
                    expr,
                }
            }
            RStmt::Store { dst, src } => YStmt::Store {
                dst: self.legalize_place(body, local_values, dst)?.0,
                src: YLocalId(src.as_u32()),
            },
            RStmt::CopyInto { dst, src } => YStmt::CopyInto {
                dst: self.legalize_place(body, local_values, dst)?.0,
                src: YLocalId(src.as_u32()),
            },
            RStmt::EnumAssertVariant { value, variant } => YStmt::EnumAssertVariant {
                value: YLocalId(value.as_u32()),
                variant: *variant,
            },
            RStmt::EnumSetTag { root, variant } => YStmt::EnumSetTag {
                root: YLocalId(root.as_u32()),
                variant: *variant,
            },
            RStmt::EnumWriteVariant {
                root,
                variant,
                fields,
            } => YStmt::EnumWriteVariant {
                root: YLocalId(root.as_u32()),
                variant: *variant,
                fields: map_values(fields),
            },
        })
    }

    fn legalize_runtime_expr(
        &mut self,
        body: &mir::RuntimeBody<'db>,
        local_values: &[LocalValueInfo<'db>],
        code_backable_locals: &[bool],
        dst: Option<RLocalId>,
        expr: &RExpr<'db>,
    ) -> Result<
        (
            YExpr<'db>,
            Option<YulValueClass<'db>>,
            YTransportInfo<'db>,
            Option<ConstNode<'db>>,
        ),
        YulError,
    > {
        let default_dst_class = dst
            .and_then(|local| body.value_class(local))
            .map(|class| yul_class_for_runtime_class(self.db, class));
        Ok(match expr {
            RExpr::Use(local) => (
                YExpr::Use(YLocalId(local.as_u32())),
                local_values[local.as_u32() as usize].class.clone(),
                local_values[local.as_u32() as usize]
                    .class
                    .as_ref()
                    .map(|class| {
                        yul_actual_transport(
                            class,
                            &local_values[local.as_u32() as usize].transport,
                        )
                    })
                    .unwrap_or_else(|| local_values[local.as_u32() as usize].transport.clone()),
                local_values[local.as_u32() as usize].const_value.clone(),
            ),
            RExpr::ConstScalar(value) => (
                YExpr::ConstWord(value.clone()),
                default_dst_class,
                YTransportInfo::empty(),
                Some(ConstNode::Scalar(value.clone())),
            ),
            RExpr::Placeholder { class } => {
                let class = default_dst_class
                    .unwrap_or_else(|| yul_class_for_runtime_class(self.db, class));
                (
                    YExpr::Placeholder {
                        class: class.clone(),
                    },
                    Some(class),
                    YTransportInfo::empty(),
                    None,
                )
            }
            RExpr::Builtin(builtin) => {
                let builtin = legalize_builtin(self.db, builtin)?;
                let class = default_dst_class;
                (
                    YExpr::Builtin(builtin),
                    class,
                    YTransportInfo::empty(),
                    None,
                )
            }
            RExpr::Unary { op, value } => (
                YExpr::Unary {
                    op: *op,
                    value: YLocalId(value.as_u32()),
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::Binary { op, lhs, rhs } => (
                YExpr::Binary {
                    op: *op,
                    lhs: YLocalId(lhs.as_u32()),
                    rhs: YLocalId(rhs.as_u32()),
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::Cast { value, to } => {
                let class = YulValueClass::Word(to.clone());
                (
                    YExpr::Cast {
                        value: YLocalId(value.as_u32()),
                        to: to.clone(),
                    },
                    Some(class),
                    YTransportInfo::empty(),
                    None,
                )
            }
            RExpr::ConstRef { region, layout } => {
                let class = YulValueClass::CodePtr { layout: *layout };
                (
                    YExpr::ConstRef {
                        region: *region,
                        layout: *layout,
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class),
                    Some(region.value(self.db).clone()),
                )
            }
            RExpr::AllocObject { layout } => {
                let class = YulValueClass::MemoryPtr { layout: *layout };
                (
                    YExpr::AllocObject {
                        layout: *layout,
                        bytes: layout_size_bytes(self.db, *layout, self.layout),
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class),
                    None,
                )
            }
            RExpr::MaterializeToObject { src } => {
                let src_info = &local_values[src.as_u32() as usize];
                if dst.is_some_and(|dst| {
                    code_backable_locals
                        .get(dst.as_u32() as usize)
                        .copied()
                        .unwrap_or(false)
                }) && let Some(space) = src_info.transport.root_alias
                    && !matches!(space, YulAddressSpace::Memory)
                    && let Some(class) = src_info.class.clone().or(default_dst_class.clone())
                {
                    return Ok((
                        YExpr::Use(YLocalId(src.as_u32())),
                        Some(specialize_yul_class_root(class, Some(space))),
                        src_info.transport.clone(),
                        src_info.const_value.clone(),
                    ));
                }
                let layout = default_dst_class
                    .as_ref()
                    .and_then(layout_from_yul_class)
                    .or_else(|| local_values[src.as_u32() as usize].class.as_ref().and_then(layout_from_yul_class))
                    .ok_or_else(|| {
                        YulError::Layout(format!(
                            "materialize-to-object result is missing an aggregate layout for `{src:?}`"
                        ))
                    })?;
                let class = YulValueClass::MemoryPtr { layout };
                let transport = if matches!(src_info.class, Some(YulValueClass::MemoryPtr { .. })) {
                    src_info.transport.clone()
                } else {
                    src_info.transport.actualize_bytes_copy_to_memory()
                };
                (
                    YExpr::MaterializeToObject {
                        src: YLocalId(src.as_u32()),
                        layout,
                    },
                    Some(class.clone()),
                    yul_actual_transport(&class, &transport),
                    None,
                )
            }
            RExpr::MaterializePlaceToObject { place } => {
                let layout = default_dst_class
                    .as_ref()
                    .and_then(layout_from_yul_class)
                    .ok_or_else(|| {
                        YulError::Layout(
                            "materialize-place-to-object result is missing an aggregate layout"
                                .to_string(),
                        )
                    })?;
                let class = YulValueClass::MemoryPtr { layout };
                (
                    YExpr::MaterializePlaceToObject {
                        place: self.legalize_place(body, local_values, place)?.0,
                        layout,
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class),
                    None,
                )
            }
            RExpr::ProviderFromRaw {
                raw,
                provider_ty: _,
                space,
                target,
            } => {
                let class = target.map_or_else(pointer_word_class, |layout| {
                    yul_ptr_class(yul_space_from_runtime(*space), layout)
                });
                (
                    YExpr::ProviderFromRaw {
                        raw: YLocalId(raw.as_u32()),
                        class: class.clone(),
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class)
                        .with_replaced_root_alias(Some(yul_space_from_runtime(*space))),
                    None,
                )
            }
            RExpr::WordToRawAddr {
                value,
                space,
                target,
            } => {
                let class = yul_class_for_raw_addr(*space, *target);
                let transport = yul_space_for_class(&class)
                    .map(YTransportInfo::root_alias)
                    .unwrap_or_else(YTransportInfo::empty);
                (
                    YExpr::WordToRawAddr {
                        value: YLocalId(value.as_u32()),
                        class: class.clone(),
                    },
                    Some(class),
                    transport,
                    None,
                )
            }
            RExpr::ProviderToRaw { value } => (
                YExpr::ProviderToRaw {
                    value: YLocalId(value.as_u32()),
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::RetagRef { value } => (
                YExpr::Use(YLocalId(value.as_u32())),
                local_values[value.as_u32() as usize].class.clone(),
                local_values[value.as_u32() as usize]
                    .class
                    .as_ref()
                    .map(|class| {
                        yul_actual_transport(
                            class,
                            &local_values[value.as_u32() as usize].transport,
                        )
                    })
                    .unwrap_or_else(|| local_values[value.as_u32() as usize].transport.clone()),
                local_values[value.as_u32() as usize].const_value.clone(),
            ),
            RExpr::AddrOf { place } => {
                let (place, transport) = self.legalize_place(body, local_values, place)?;
                let class = default_dst_class.unwrap_or_else(|| place.result_class.clone());
                let root_alias = transport.root_alias.or_else(|| yul_space_for_class(&class));
                (
                    YExpr::AddrOf { place },
                    Some(class.clone()),
                    transport.with_replaced_root_alias(root_alias),
                    None,
                )
            }
            RExpr::Load { place } => {
                let (place, transport) = self.legalize_place(body, local_values, place)?;
                let class = place.result_class.clone();
                let transport = match place.storage_kind {
                    YulStorageKind::Bytes => {
                        transport.with_replaced_root_alias(yul_space_for_class(&class))
                    }
                    YulStorageKind::Cell => yul_actual_transport(&class, &transport),
                };
                (YExpr::Load { place }, Some(class), transport, None)
            }
            RExpr::Call { callee, args } => {
                let runtime_function = *self.function_by_instance.get(callee).ok_or_else(|| {
                    YulError::InvalidYulPackage(format!(
                        "missing declared function for runtime call target `{callee:?}`"
                    ))
                })?;
                let key = self.specialize_call_key(runtime_function, args, local_values);
                let callee = self.ensure_function_variant(key)?;
                let plan = self.function_variants[callee.index()]
                    .as_ref()
                    .expect("newly legalized callee plan should exist");
                (
                    YExpr::Call {
                        callee,
                        args: map_values(args),
                    },
                    plan.ret.clone(),
                    plan.ret_transport
                        .clone()
                        .unwrap_or_else(YTransportInfo::empty),
                    None,
                )
            }
            RExpr::EnumMake {
                layout,
                variant,
                fields,
            } => {
                let class =
                    default_dst_class.unwrap_or(YulValueClass::MemoryPtr { layout: *layout });
                (
                    YExpr::EnumMake {
                        layout: *layout,
                        variant: *variant,
                        fields: map_values(fields),
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class),
                    None,
                )
            }
            RExpr::EnumTagOfValue { value } => (
                YExpr::EnumTagOfValue {
                    value: YLocalId(value.as_u32()),
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::EnumIsVariant { value, variant } => (
                YExpr::EnumIsVariant {
                    value: YLocalId(value.as_u32()),
                    variant: *variant,
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::EnumExtract {
                value,
                variant,
                field,
            } => {
                let class = default_dst_class.unwrap_or_else(|| {
                    local_values[value.as_u32() as usize]
                        .class
                        .clone()
                        .unwrap_or_else(pointer_word_class)
                });
                (
                    YExpr::EnumExtract {
                        value: YLocalId(value.as_u32()),
                        variant: *variant,
                        field: *field,
                    },
                    Some(class.clone()),
                    runtime_class_transport(&class),
                    None,
                )
            }
            RExpr::EnumGetTag { root } => (
                YExpr::EnumGetTag {
                    root: YLocalId(root.as_u32()),
                },
                default_dst_class,
                YTransportInfo::empty(),
                None,
            ),
            RExpr::EnumAssertVariantRef { root, variant } => (
                YExpr::EnumAssertVariantRef {
                    root: YLocalId(root.as_u32()),
                    variant: *variant,
                },
                local_values[root.as_u32() as usize].class.clone(),
                local_values[root.as_u32() as usize].transport.clone(),
                None,
            ),
        })
    }

    fn legalize_runtime_terminator(
        &mut self,
        local_values: &[LocalValueInfo<'db>],
        terminator: &RTerminator<'db>,
    ) -> Result<YTerminator<'db>, YulError> {
        Ok(match terminator {
            RTerminator::Goto(target) => YTerminator::Goto(YBlockId(target.as_u32())),
            RTerminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => YTerminator::Branch {
                cond: YLocalId(cond.as_u32()),
                then_bb: YBlockId(then_bb.as_u32()),
                else_bb: YBlockId(else_bb.as_u32()),
            },
            RTerminator::SwitchScalar {
                discr,
                cases,
                default,
            } => YTerminator::SwitchWord {
                discr: YLocalId(discr.as_u32()),
                cases: cases
                    .iter()
                    .map(|(value, block)| (value.clone(), YBlockId(block.as_u32())))
                    .collect(),
                default: YBlockId(default.as_u32()),
            },
            RTerminator::MatchEnumTag {
                tag,
                enum_layout,
                cases,
                default,
            } => YTerminator::MatchEnumTag {
                tag: YLocalId(tag.as_u32()),
                enum_layout: *enum_layout,
                cases: cases
                    .iter()
                    .map(|(variant, block)| (*variant, YBlockId(block.as_u32())))
                    .collect(),
                default: default.map(|block| YBlockId(block.as_u32())),
            },
            RTerminator::TerminalCall { callee, args } => {
                let runtime_function = *self.function_by_instance.get(callee).ok_or_else(|| {
                    YulError::InvalidYulPackage(format!(
                        "missing runtime function for terminal call target `{callee:?}`"
                    ))
                })?;
                let key = self.specialize_call_key(runtime_function, args, local_values);
                YTerminator::TerminalCall {
                    callee: self.ensure_function_variant(key)?,
                    args: map_values(args),
                }
            }
            RTerminator::ReturnData { offset, len } => YTerminator::ReturnData {
                offset: YLocalId(offset.as_u32()),
                len: YLocalId(len.as_u32()),
            },
            RTerminator::Revert { offset, len } => YTerminator::Revert {
                offset: YLocalId(offset.as_u32()),
                len: YLocalId(len.as_u32()),
            },
            RTerminator::SelfDestruct { beneficiary } => YTerminator::SelfDestruct {
                beneficiary: YLocalId(beneficiary.as_u32()),
            },
            RTerminator::Trap => YTerminator::Trap,
            RTerminator::Return(value) => {
                YTerminator::Return(value.map(|value| YLocalId(value.as_u32())))
            }
            RTerminator::Stop => YTerminator::Stop,
        })
    }

    fn legalize_place(
        &self,
        body: &mir::RuntimeBody<'db>,
        local_values: &[LocalValueInfo<'db>],
        place: &mir::RuntimePlace<'db>,
    ) -> Result<(YulPlace<'db>, YTransportInfo<'db>), YulError> {
        let program = &(self.db as &dyn mir::MirDb);
        let resolved = resolve_runtime_place(self.db, program, body, place).map_err(|err| {
            YulError::InvalidYulPackage(format!(
                "failed to resolve runtime place `{place:?}`: {err:?}"
            ))
        })?;

        let (root, mut transport) = match &resolved.root_kind {
            ResolvedPlaceRootKind::Slot { local, .. } => {
                let info = &local_values[local.as_u32() as usize];
                match (
                    info.class.clone(),
                    info.class
                        .as_ref()
                        .and_then(|class| yul_non_memory_root_space(class, &info.transport)),
                ) {
                    (Some(class), Some(space)) if !matches!(class, YulValueClass::Word(_)) => (
                        YulPlaceRoot::Ptr {
                            local: YLocalId(local.as_u32()),
                            space,
                            class,
                        },
                        info.transport.clone(),
                    ),
                    _ => (
                        YulPlaceRoot::Slot(YLocalId(local.as_u32())),
                        info.transport.clone(),
                    ),
                }
            }
            ResolvedPlaceRootKind::Ref { value, class } => {
                let runtime_class = body.value_class(*value).ok_or_else(|| {
                    YulError::InvalidYulPackage(format!(
                        "ref root {value:?} does not have a runtime class"
                    ))
                })?;
                let space = match runtime_class {
                    RuntimeClass::Ref {
                        kind: RefKind::Const,
                        ..
                    } => YulAddressSpace::Code,
                    RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    } => YulAddressSpace::Memory,
                    RuntimeClass::Ref {
                        kind: RefKind::Provider { space, .. },
                        ..
                    }
                    | RuntimeClass::RawAddr { space, .. } => yul_space_from_runtime(*space),
                    _ => yul_space_for_root_class(&yul_class_for_runtime_class(
                        self.db,
                        runtime_class,
                    ))?,
                };
                (
                    YulPlaceRoot::Ptr {
                        local: YLocalId(value.as_u32()),
                        space,
                        class: yul_place_root_class(space, class),
                    },
                    local_values[value.as_u32() as usize].transport.clone(),
                )
            }
            ResolvedPlaceRootKind::Provider {
                value,
                provider_class,
                class,
                ..
            } => {
                let space = match &provider_class {
                    RuntimeClass::RawAddr { space, .. } => yul_space_from_runtime(*space),
                    RuntimeClass::Ref {
                        kind: RefKind::Provider { space, .. },
                        ..
                    } => yul_space_from_runtime(*space),
                    _ => yul_space_for_root_class(&yul_class_for_runtime_class(
                        self.db,
                        provider_class,
                    ))?,
                };
                (
                    YulPlaceRoot::Ptr {
                        local: YLocalId(value.as_u32()),
                        space,
                        class: yul_place_root_class(space, class),
                    },
                    local_values[value.as_u32() as usize].transport.clone(),
                )
            }
            ResolvedPlaceRootKind::Ptr { addr, space, class } => {
                let space = yul_space_from_runtime(*space);
                (
                    YulPlaceRoot::Ptr {
                        local: YLocalId(addr.as_u32()),
                        space,
                        class: yul_place_root_class(space, class),
                    },
                    local_values[addr.as_u32() as usize].transport.clone(),
                )
            }
        };
        let packed_byte_access = yul_place_uses_packed_byte_access(
            self.db,
            &resolved,
            match &root {
                YulPlaceRoot::Slot(_) => YulAddressSpace::Memory,
                YulPlaceRoot::Ptr { space, .. } => *space,
            },
            self.layout,
        );

        let mut path = Vec::with_capacity(resolved.path.len());
        for (idx, elem) in resolved.path.iter().enumerate() {
            let remaining_path = idx + 1 < resolved.path.len();
            let yul_elem = match elem {
                ResolvedPlaceElem::Field { field, class } => {
                    transport = transport.projected(&YTransportPath::from_projection(
                        Projection::Field(field.0 as usize),
                    ));
                    let class = specialize_yul_class_root(
                        YulValueClass::for_place_path(self.db, class, remaining_path),
                        transport.root_alias,
                    );
                    YulPlaceElem::Field {
                        field: *field,
                        class,
                    }
                }
                ResolvedPlaceElem::Index { index, class } => {
                    let index = match index {
                        IndexSource::Constant(value) => IndexSource::Constant(*value),
                        IndexSource::Dynamic(value) => {
                            IndexSource::Dynamic(YLocalId(value.as_u32()))
                        }
                    };
                    transport = transport
                        .projected(&YTransportPath::from_projection(Projection::Index(index)));
                    let class = specialize_yul_class_root(
                        YulValueClass::for_place_path(self.db, class, remaining_path),
                        transport.root_alias,
                    );
                    YulPlaceElem::Index { index, class }
                }
                ResolvedPlaceElem::VariantField {
                    variant,
                    field,
                    class,
                } => {
                    transport = transport.projected(&YTransportPath::from_projection(
                        Projection::VariantField {
                            variant: *variant,
                            enum_ty: variant.enum_layout,
                            field_idx: field.0 as usize,
                        },
                    ));
                    let class = specialize_yul_class_root(
                        YulValueClass::for_place_path(self.db, class, remaining_path),
                        transport.root_alias,
                    );
                    YulPlaceElem::VariantField {
                        variant: *variant,
                        field: *field,
                        class,
                    }
                }
                ResolvedPlaceElem::Deref {
                    carrier_class,
                    class,
                } => {
                    let root_alias =
                        yul_space_for_deref_carrier(carrier_class).ok_or_else(|| {
                            YulError::InvalidYulPackage(format!(
                                "cannot follow non-transport runtime class `{carrier_class:?}`"
                            ))
                        })?;
                    transport = transport.with_replaced_root_alias(Some(root_alias));
                    let class = specialize_yul_class_root(
                        YulValueClass::for_place_path(self.db, class, remaining_path),
                        transport.root_alias,
                    );
                    YulPlaceElem::Deref {
                        carrier_class: carrier_class.clone(),
                        class,
                    }
                }
            };
            path.push(yul_elem);
        }
        let default_result_class = yul_class_for_runtime_class(self.db, &resolved.result_class);
        let transport = transport.without_default_memory_root(Some(&default_result_class));
        let result_class = specialize_yul_class_root(default_result_class, transport.root_alias);
        Ok((
            YulPlace {
                root,
                path: path.into_boxed_slice(),
                storage_kind: yul_storage_kind_for_runtime_class(&resolved.result_class),
                packed_byte_access,
                runtime_result_class: resolved.result_class.clone(),
                result_class,
            },
            transport,
        ))
    }

    fn legalize_local_root(
        &self,
        local: &mir::RLocal<'db>,
        info: &LocalValueInfo<'db>,
    ) -> YulLocalRoot<'db> {
        match &local.root {
            mir::RuntimeLocalRoot::None => {
                if let Some(class) = info.class.clone()
                    && let Some(space) = info.transport.root_alias
                    && !matches!(class, YulValueClass::Word(_))
                {
                    YulLocalRoot::PtrRoot {
                        class: specialize_yul_class_root(class, Some(space)),
                    }
                } else {
                    YulLocalRoot::None
                }
            }
            mir::RuntimeLocalRoot::Slot(class) => {
                if let Some(value_class) = info.class.clone()
                    && let Some(space) = yul_non_memory_root_space(&value_class, &info.transport)
                    && !matches!(value_class, YulValueClass::Word(_))
                {
                    YulLocalRoot::PtrRoot {
                        class: specialize_yul_class_root(value_class, Some(space)),
                    }
                } else {
                    YulLocalRoot::MemorySlot {
                        class: class.clone(),
                    }
                }
            }
            mir::RuntimeLocalRoot::Ref(class) => YulLocalRoot::PtrRoot {
                class: yul_class_for_runtime_class(self.db, class),
            },
            mir::RuntimeLocalRoot::Ptr { space, class } => YulLocalRoot::PtrRoot {
                class: specialize_yul_class_root(
                    yul_class_for_runtime_class(self.db, class),
                    Some(yul_space_from_runtime(*space)),
                ),
            },
        }
    }
}

fn yul_non_memory_root_space<'db>(
    class: &YulValueClass<'db>,
    transport: &YTransportInfo<'db>,
) -> Option<YulAddressSpace> {
    transport
        .root_alias
        .or_else(|| yul_space_for_class(class))
        .filter(|space| !matches!(space, YulAddressSpace::Memory))
}

fn yul_actual_transport<'db>(
    class: &YulValueClass<'db>,
    transport: &YTransportInfo<'db>,
) -> YTransportInfo<'db> {
    yul_non_memory_root_space(class, transport)
        .map(|space| transport.with_replaced_root_alias(Some(space)))
        .unwrap_or_else(|| transport.clone())
}

pub fn yul_class_for_runtime_class<'db>(
    db: &'db DriverDataBase,
    class: &RuntimeClass<'db>,
) -> YulValueClass<'db> {
    YulValueClass::for_runtime_class(db, class)
}

pub fn yul_space_for_root_class(class: &YulValueClass<'_>) -> Result<YulAddressSpace, YulError> {
    match class {
        YulValueClass::MemoryPtr { .. } => Ok(YulAddressSpace::Memory),
        YulValueClass::CodePtr { .. } => Ok(YulAddressSpace::Code),
        YulValueClass::StoragePtr { .. } => Ok(YulAddressSpace::Storage),
        YulValueClass::TransientPtr { .. } => Ok(YulAddressSpace::Transient),
        YulValueClass::CalldataPtr { .. } => Ok(YulAddressSpace::Calldata),
        YulValueClass::Word(_) => Err(YulError::InvalidYulPackage(
            "word-valued local cannot act as a place root".to_string(),
        )),
    }
}

fn yul_space_from_runtime(space: AddressSpaceKind) -> YulAddressSpace {
    match space {
        AddressSpaceKind::Memory => YulAddressSpace::Memory,
        AddressSpaceKind::Storage => YulAddressSpace::Storage,
        AddressSpaceKind::Transient => YulAddressSpace::Transient,
        AddressSpaceKind::Calldata => YulAddressSpace::Calldata,
    }
}

pub(super) fn yul_space_for_deref_carrier(class: &RuntimeClass<'_>) -> Option<YulAddressSpace> {
    match class {
        RuntimeClass::Ref {
            kind: RefKind::Const,
            ..
        } => Some(YulAddressSpace::Code),
        RuntimeClass::Ref {
            kind: RefKind::Object,
            ..
        } => Some(YulAddressSpace::Memory),
        RuntimeClass::Ref {
            kind: RefKind::Provider { space, .. },
            ..
        }
        | RuntimeClass::RawAddr { space, .. } => Some(yul_space_from_runtime(*space)),
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => None,
    }
}

fn yul_ptr_class<'db>(space: YulAddressSpace, layout: LayoutId<'db>) -> YulValueClass<'db> {
    match space {
        YulAddressSpace::Memory => YulValueClass::MemoryPtr { layout },
        YulAddressSpace::Storage => YulValueClass::StoragePtr { layout },
        YulAddressSpace::Transient => YulValueClass::TransientPtr { layout },
        YulAddressSpace::Calldata => YulValueClass::CalldataPtr { layout },
        YulAddressSpace::Code => YulValueClass::CodePtr { layout },
    }
}

fn pointer_word_class<'db>() -> YulValueClass<'db> {
    YulValueClass::Word(ScalarClass {
        repr: ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        role: mir::ScalarRole::Plain,
    })
}

fn yul_place_root_class<'db>(
    space: YulAddressSpace,
    class: &RuntimeClass<'db>,
) -> YulValueClass<'db> {
    class
        .aggregate_layout()
        .map(|layout| yul_ptr_class(space, layout))
        .unwrap_or_else(pointer_word_class)
}

fn yul_place_uses_packed_byte_access<'db>(
    db: &'db DriverDataBase,
    resolved: &mir::ResolvedRuntimePlace<'db>,
    space: YulAddressSpace,
    target: TargetDataLayout,
) -> bool {
    let mut space = space;
    let mut current_layout = match &resolved.root_kind {
        ResolvedPlaceRootKind::Slot { class, .. }
        | ResolvedPlaceRootKind::Ref { class, .. }
        | ResolvedPlaceRootKind::Provider { class, .. }
        | ResolvedPlaceRootKind::Ptr { class, .. } => class.aggregate_layout(),
    };
    for elem in resolved.path.iter() {
        let class = match elem {
            ResolvedPlaceElem::Field { class, .. }
            | ResolvedPlaceElem::Index { class, .. }
            | ResolvedPlaceElem::VariantField { class, .. }
            | ResolvedPlaceElem::Deref { class, .. } => class,
        };
        if matches!(elem, ResolvedPlaceElem::Index { .. })
            && current_layout.is_some_and(|layout| array_elem_size_bytes(db, layout, target) == 1)
            && matches!(space, YulAddressSpace::Memory | YulAddressSpace::Code)
            && matches!(class, RuntimeClass::Scalar(_))
        {
            return true;
        }
        if let ResolvedPlaceElem::Deref { carrier_class, .. } = elem {
            let Some(deref_space) = yul_space_for_deref_carrier(carrier_class) else {
                return false;
            };
            space = deref_space;
        }
        current_layout = class.aggregate_layout();
    }
    false
}

fn layout_scalar_word_class<'db>(
    db: &'db DriverDataBase,
    layout: LayoutId<'db>,
) -> Option<ScalarClass<'db>> {
    match layout.data(db) {
        Layout::Struct(data) if data.fields.len() == 1 => class_scalar_word(db, &data.fields[0]),
        Layout::Array(data) if data.len == 1 => class_scalar_word(db, &data.elem),
        Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_) => None,
    }
}

fn class_scalar_word<'db>(
    db: &'db DriverDataBase,
    class: &RuntimeClass<'db>,
) -> Option<ScalarClass<'db>> {
    match class {
        RuntimeClass::Scalar(class) => Some(class.clone()),
        RuntimeClass::AggregateValue { layout } => layout_scalar_word_class(db, *layout),
        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => None,
    }
}

fn layout_from_yul_class<'db>(class: &YulValueClass<'db>) -> Option<LayoutId<'db>> {
    match class {
        YulValueClass::Word(_) => None,
        YulValueClass::MemoryPtr { layout }
        | YulValueClass::CodePtr { layout }
        | YulValueClass::StoragePtr { layout }
        | YulValueClass::TransientPtr { layout }
        | YulValueClass::CalldataPtr { layout } => Some(*layout),
    }
}

fn yul_class_for_raw_addr<'db>(
    space: AddressSpaceKind,
    target: Option<LayoutId<'db>>,
) -> YulValueClass<'db> {
    target
        .map(|layout| yul_ptr_class(yul_space_from_runtime(space), layout))
        .unwrap_or_else(pointer_word_class)
}

fn legalize_builtin<'db>(
    db: &'db DriverDataBase,
    builtin: &RuntimeBuiltin<'db>,
) -> Result<YBuiltin<'db>, YulError> {
    Ok(match builtin {
        RuntimeBuiltin::Mload { addr } => YBuiltin::Mload {
            addr: YLocalId(addr.as_u32()),
        },
        RuntimeBuiltin::Mstore { addr, value } => YBuiltin::Mstore {
            addr: YLocalId(addr.as_u32()),
            value: YLocalId(value.as_u32()),
        },
        RuntimeBuiltin::Mstore8 { addr, value } => YBuiltin::Mstore8 {
            addr: YLocalId(addr.as_u32()),
            value: YLocalId(value.as_u32()),
        },
        RuntimeBuiltin::Mcopy { dst, src, len } => YBuiltin::Mcopy {
            dst: YLocalId(dst.as_u32()),
            src: YLocalId(src.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::Msize => YBuiltin::Msize,
        RuntimeBuiltin::Sload { slot } => YBuiltin::Sload {
            slot: YLocalId(slot.as_u32()),
        },
        RuntimeBuiltin::Sstore { slot, value } => YBuiltin::Sstore {
            slot: YLocalId(slot.as_u32()),
            value: YLocalId(value.as_u32()),
        },
        RuntimeBuiltin::CallValue => YBuiltin::CallValue,
        RuntimeBuiltin::ReturnDataSize => YBuiltin::ReturnDataSize,
        RuntimeBuiltin::ReturnDataCopy { dst, offset, len } => YBuiltin::ReturnDataCopy {
            dst: YLocalId(dst.as_u32()),
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::CallDataSize => YBuiltin::CallDataSize,
        RuntimeBuiltin::CallDataLoad { offset } => YBuiltin::CallDataLoad {
            offset: YLocalId(offset.as_u32()),
        },
        RuntimeBuiltin::CallDataCopy { dst, offset, len } => YBuiltin::CallDataCopy {
            dst: YLocalId(dst.as_u32()),
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::CodeSize => YBuiltin::CodeSize,
        RuntimeBuiltin::CodeCopy { dst, offset, len } => YBuiltin::CodeCopy {
            dst: YLocalId(dst.as_u32()),
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::Keccak256 { offset, len } => YBuiltin::Keccak256 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::AddMod { lhs, rhs, modulus } => YBuiltin::AddMod {
            lhs: YLocalId(lhs.as_u32()),
            rhs: YLocalId(rhs.as_u32()),
            modulus: YLocalId(modulus.as_u32()),
        },
        RuntimeBuiltin::MulMod { lhs, rhs, modulus } => YBuiltin::MulMod {
            lhs: YLocalId(lhs.as_u32()),
            rhs: YLocalId(rhs.as_u32()),
            modulus: YLocalId(modulus.as_u32()),
        },
        RuntimeBuiltin::SignExtend { byte, value } => YBuiltin::SignExtend {
            byte: YLocalId(byte.as_u32()),
            value: YLocalId(value.as_u32()),
        },
        RuntimeBuiltin::IntrinsicArith {
            op,
            checked,
            lhs,
            rhs,
            class,
        } => YBuiltin::IntrinsicArith {
            op: *op,
            checked: *checked,
            lhs: YLocalId(lhs.as_u32()),
            rhs: YLocalId(rhs.as_u32()),
            class: class.clone(),
        },
        RuntimeBuiltin::Saturating {
            op,
            lhs,
            rhs,
            class,
        } => YBuiltin::Saturating {
            op: *op,
            lhs: YLocalId(lhs.as_u32()),
            rhs: YLocalId(rhs.as_u32()),
            class: class.clone(),
        },
        RuntimeBuiltin::Address => YBuiltin::Address,
        RuntimeBuiltin::Caller => YBuiltin::Caller,
        RuntimeBuiltin::Origin => YBuiltin::Origin,
        RuntimeBuiltin::GasPrice => YBuiltin::GasPrice,
        RuntimeBuiltin::CoinBase => YBuiltin::CoinBase,
        RuntimeBuiltin::Timestamp => YBuiltin::Timestamp,
        RuntimeBuiltin::Number => YBuiltin::Number,
        RuntimeBuiltin::PrevRandao => YBuiltin::PrevRandao,
        RuntimeBuiltin::GasLimit => YBuiltin::GasLimit,
        RuntimeBuiltin::ChainId => YBuiltin::ChainId,
        RuntimeBuiltin::BaseFee => YBuiltin::BaseFee,
        RuntimeBuiltin::SelfBalance => YBuiltin::SelfBalance,
        RuntimeBuiltin::BlockHash { block } => YBuiltin::BlockHash {
            block: YLocalId(block.as_u32()),
        },
        RuntimeBuiltin::Gas => YBuiltin::Gas,
        RuntimeBuiltin::CurrentCodeRegionLen => YBuiltin::CurrentCodeRegionLen,
        RuntimeBuiltin::CodeRegionOffset { region } => {
            YBuiltin::CodeRegionOffset { region: *region }
        }
        RuntimeBuiltin::CodeRegionLen { region } => YBuiltin::CodeRegionLen { region: *region },
        RuntimeBuiltin::Malloc { size } => YBuiltin::Malloc {
            size: YLocalId(size.as_u32()),
        },
        RuntimeBuiltin::Call {
            gas,
            addr,
            value,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => YBuiltin::Call {
            gas: YLocalId(gas.as_u32()),
            addr: YLocalId(addr.as_u32()),
            value: YLocalId(value.as_u32()),
            args_offset: YLocalId(args_offset.as_u32()),
            args_len: YLocalId(args_len.as_u32()),
            ret_offset: YLocalId(ret_offset.as_u32()),
            ret_len: YLocalId(ret_len.as_u32()),
        },
        RuntimeBuiltin::StaticCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => YBuiltin::StaticCall {
            gas: YLocalId(gas.as_u32()),
            addr: YLocalId(addr.as_u32()),
            args_offset: YLocalId(args_offset.as_u32()),
            args_len: YLocalId(args_len.as_u32()),
            ret_offset: YLocalId(ret_offset.as_u32()),
            ret_len: YLocalId(ret_len.as_u32()),
        },
        RuntimeBuiltin::DelegateCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => YBuiltin::DelegateCall {
            gas: YLocalId(gas.as_u32()),
            addr: YLocalId(addr.as_u32()),
            args_offset: YLocalId(args_offset.as_u32()),
            args_len: YLocalId(args_len.as_u32()),
            ret_offset: YLocalId(ret_offset.as_u32()),
            ret_len: YLocalId(ret_len.as_u32()),
        },
        RuntimeBuiltin::Create { value, offset, len } => YBuiltin::Create {
            value: YLocalId(value.as_u32()),
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::Create2 {
            value,
            offset,
            len,
            salt,
        } => YBuiltin::Create2 {
            value: YLocalId(value.as_u32()),
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
            salt: YLocalId(salt.as_u32()),
        },
        RuntimeBuiltin::Log0 { offset, len } => YBuiltin::Log0 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
        },
        RuntimeBuiltin::Log1 {
            offset,
            len,
            topic0,
        } => YBuiltin::Log1 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
            topic0: YLocalId(topic0.as_u32()),
        },
        RuntimeBuiltin::Log2 {
            offset,
            len,
            topic0,
            topic1,
        } => YBuiltin::Log2 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
            topic0: YLocalId(topic0.as_u32()),
            topic1: YLocalId(topic1.as_u32()),
        },
        RuntimeBuiltin::Log3 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
        } => YBuiltin::Log3 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
            topic0: YLocalId(topic0.as_u32()),
            topic1: YLocalId(topic1.as_u32()),
            topic2: YLocalId(topic2.as_u32()),
        },
        RuntimeBuiltin::Log4 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
            topic3,
        } => YBuiltin::Log4 {
            offset: YLocalId(offset.as_u32()),
            len: YLocalId(len.as_u32()),
            topic0: YLocalId(topic0.as_u32()),
            topic1: YLocalId(topic1.as_u32()),
            topic2: YLocalId(topic2.as_u32()),
            topic3: YLocalId(topic3.as_u32()),
        },
        RuntimeBuiltin::CallDataSelector => YBuiltin::CallDataSelector,
        RuntimeBuiltin::MakeContractFieldRef { slot, class, kind } => {
            YBuiltin::MakeContractFieldRef {
                slot: *slot,
                class: yul_class_for_runtime_class(db, class),
                kind: kind.clone(),
            }
        }
    })
}

fn map_values(values: &[RValueId]) -> Box<[YLocalId]> {
    values
        .iter()
        .map(|value| YLocalId(value.as_u32()))
        .collect()
}
