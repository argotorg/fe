use std::fmt;

mod body_builder;

pub use body_builder::{BodyBuilder, LocalValue};

use common::diagnostics::Span;
use hir::analysis::ty::trait_def::TraitInstId;
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::{CallableDef, Contract, EnumVariant, ExprId, Func, PatId, TopLevelMod};
use hir::projection::{Projection, ProjectionPath};
use num_bigint::BigUint;
use rustc_hash::FxHashMap;

/// MIR for an entire top-level module.
#[derive(Debug, Clone)]
pub struct MirModule<'db> {
    pub top_mod: TopLevelMod<'db>,
    /// All lowered functions in the module.
    pub functions: Vec<MirFunction<'db>>,
}

impl<'db> MirModule<'db> {
    pub fn new(top_mod: TopLevelMod<'db>) -> Self {
        Self {
            top_mod,
            functions: Vec::new(),
        }
    }
}

/// MIR for a single function.
#[derive(Debug, Clone)]
pub struct MirFunction<'db> {
    pub origin: MirFunctionOrigin<'db>,
    pub body: MirBody<'db>,
    pub typed_body: Option<hir::analysis::ty::ty_check::TypedBody<'db>>,
    /// Concrete generic arguments used to instantiate this function instance.
    pub generic_args: Vec<TyId<'db>>,
    /// Return type after monomorphization and associated-type normalization.
    pub ret_ty: TyId<'db>,
    /// Whether this function has a runtime return value (`ret_ty` is not zero-sized).
    pub returns_value: bool,
    /// Optional contract association declared via attributes.
    pub contract_function: Option<ContractFunction>,
    /// Symbol name used for codegen (includes monomorphization suffix when present).
    pub symbol_name: String,
    /// For methods, the address space variant of the receiver for this instance.
    pub receiver_space: Option<AddressSpaceKind>,
}

/// Source identity of a MIR function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirFunctionOrigin<'db> {
    Hir(Func<'db>),
    Synthetic(SyntheticId<'db>),
}

/// Stable identity for compiler-generated MIR-only functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntheticId<'db> {
    ContractInitEntrypoint(Contract<'db>),
    ContractRuntimeEntrypoint(Contract<'db>),
    ContractInitHandler(Contract<'db>),
    ContractRecvArmHandler {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
    ContractInitCodeOffset(Contract<'db>),
    ContractInitCodeLen(Contract<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirBackend {
    EvmYul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirStage {
    Capability,
    Repr(MirBackend),
}

/// A function body expressed as basic blocks.
#[derive(Debug, Clone)]
pub struct MirBody<'db> {
    pub stage: MirStage,
    pub entry: BasicBlockId,
    pub blocks: Vec<BasicBlock<'db>>,
    pub values: Vec<ValueData<'db>>,
    pub locals: Vec<LocalData<'db>>,
    /// Eager spans for MIR nodes, stored out-of-line so sources don't affect hashing/dedup.
    ///
    /// `SourceInfoId(0)` is always the synthetic/no-span entry.
    pub source_infos: Vec<SourceInfo>,
    /// Local IDs for explicit function parameters in source order.
    pub param_locals: Vec<LocalId>,
    /// Local IDs for effect parameters in source order.
    pub effect_param_locals: Vec<LocalId>,
    /// Mapping from an address-taken word/ptr/ZST local to its spill slot local.
    pub spill_slots: FxHashMap<LocalId, LocalId>,
    pub expr_values: FxHashMap<ExprId, ValueId>,
    pub pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub loop_headers: FxHashMap<BasicBlockId, LoopInfo>,
}

impl<'db> MirBody<'db> {
    pub fn new() -> Self {
        Self {
            stage: MirStage::Capability,
            entry: BasicBlockId(0),
            blocks: Vec::new(),
            values: Vec::new(),
            locals: Vec::new(),
            source_infos: vec![SourceInfo { span: None }],
            param_locals: Vec::new(),
            effect_param_locals: Vec::new(),
            spill_slots: FxHashMap::default(),
            expr_values: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            loop_headers: FxHashMap::default(),
        }
    }

    pub fn assert_stage(&self, expected: MirStage) {
        debug_assert!(
            self.stage == expected,
            "expected MIR stage {expected:?}, got {:?}",
            self.stage
        );
    }

    pub fn source_span(&self, source: SourceInfoId) -> Option<Span> {
        self.source_infos
            .get(source.index())
            .unwrap_or_else(|| panic!("invalid SourceInfoId {source:?}"))
            .span
            .clone()
    }

    pub fn alloc_source_info(&mut self, span: Option<Span>) -> SourceInfoId {
        let id = SourceInfoId(self.source_infos.len() as u32);
        self.source_infos.push(SourceInfo { span });
        id
    }

    pub fn push_block(&mut self, block: BasicBlock<'db>) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len() as u32);
        if self.blocks.is_empty() {
            self.entry = id;
        }
        self.blocks.push(block);
        id
    }

    pub fn block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock<'db> {
        &mut self.blocks[id.index()]
    }

    pub fn alloc_value(&mut self, data: ValueData<'db>) -> ValueId {
        let id = ValueId(self.values.len() as u32);
        self.values.push(data);
        id
    }

    pub fn alloc_local(&mut self, data: LocalData<'db>) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(data);
        id
    }

    pub fn value(&self, id: ValueId) -> &ValueData<'db> {
        &self.values[id.index()]
    }

    pub fn local(&self, id: LocalId) -> &LocalData<'db> {
        &self.locals[id.index()]
    }

    /// Determines the address space associated with a MIR value.
    ///
    /// This is only meaningful for pointer-like values (`ValueRepr::Ptr`/`ValueRepr::Ref`) and for
    /// locals that store such values. Calling this on a pure scalar value is a bug and triggers a
    /// debug assertion.
    pub fn value_address_space(&self, value: ValueId) -> AddressSpaceKind {
        if let Some(space) = self.try_value_address_space(value) {
            return space;
        }

        let value_data = self.value(value);
        panic!(
            "requested address space for MIR value without one: {value:?} (repr={:?}, origin={:?})",
            value_data.repr, value_data.origin
        );
    }

    /// Determines the address space associated with a place.
    pub fn place_address_space(&self, place: &Place<'db>) -> AddressSpaceKind {
        self.value_address_space(place.base)
    }

    fn try_value_address_space(&self, value: ValueId) -> Option<AddressSpaceKind> {
        try_value_address_space_in(&self.values, &self.locals, value)
    }
}

pub(crate) fn try_value_address_space_in<'db>(
    values: &[ValueData<'db>],
    locals: &[LocalData<'db>],
    value: ValueId,
) -> Option<AddressSpaceKind> {
    let value_data = values.get(value.index())?;
    if let Some(space) = value_data.repr.address_space() {
        return Some(space);
    }
    match &value_data.origin {
        ValueOrigin::Local(local) => locals.get(local.index()).map(|l| l.address_space),
        ValueOrigin::PlaceRoot(local) => locals.get(local.index()).map(|l| l.address_space),
        ValueOrigin::TransparentCast { value } => {
            try_value_address_space_in(values, locals, *value)
        }
        ValueOrigin::FieldPtr(field_ptr) => Some(field_ptr.addr_space),
        ValueOrigin::PlaceRef(place) => try_value_address_space_in(values, locals, place.base),
        ValueOrigin::MoveOut { place } => try_value_address_space_in(values, locals, place.base),
        _ => None,
    }
}

pub(crate) fn lookup_local_capability_space<'db>(
    locals: &[LocalData<'db>],
    local: LocalId,
    projection: &MirProjectionPath<'db>,
) -> Option<AddressSpaceKind> {
    let local = locals.get(local.index())?;
    let mut best: Option<(usize, AddressSpaceKind)> = None;
    for (path, space) in &local.capability_spaces {
        if !path.is_prefix_of(projection) {
            continue;
        }
        let path_len = path.len();
        if best
            .map(|(best_len, _)| path_len > best_len)
            .unwrap_or(true)
        {
            best = Some((path_len, *space));
        }
    }
    best.map(|(_, space)| space)
}

pub(crate) fn projection_strip_prefix<'db>(
    path: &MirProjectionPath<'db>,
    prefix: &MirProjectionPath<'db>,
) -> Option<MirProjectionPath<'db>> {
    if !prefix.is_prefix_of(path) {
        return None;
    }

    let mut suffix = MirProjectionPath::new();
    for proj in path.iter().skip(prefix.len()) {
        suffix.push(proj.clone());
    }
    Some(suffix)
}

pub(crate) fn resolve_local_projection_root<'db>(
    values: &[ValueData<'db>],
    value: ValueId,
) -> Option<(LocalId, MirProjectionPath<'db>)> {
    let mut current = value;
    let mut projection = MirProjectionPath::new();

    loop {
        match &values.get(current.index())?.origin {
            ValueOrigin::TransparentCast { value } => current = *value,
            ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
                return Some((*local, projection));
            }
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                projection = place.projection.concat(&projection);
                current = place.base;
            }
            _ => return None,
        }
    }
}

impl<'db> Default for MirBody<'db> {
    fn default() -> Self {
        Self::new()
    }
}

/// Identifier for a basic block (dense index into `MirBody::blocks`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub u32);

impl BasicBlockId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

impl ValueId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

impl LocalId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Stable index into [`MirBody::source_infos`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceInfoId(pub u32);

impl SourceInfoId {
    pub const SYNTHETIC: Self = Self(0);

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct SourceInfo {
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct LocalData<'db> {
    pub name: String,
    pub ty: TyId<'db>,
    pub is_mut: bool,
    pub source: SourceInfoId,
    /// Address space for pointer-like values stored in this local.
    ///
    /// For non-pointer scalars this is ignored, but storing it here lets codegen
    /// interpret locals that represent aggregate pointers (memory vs storage).
    pub address_space: AddressSpaceKind,
    /// Capability pointee-space metadata for values stored in this local.
    ///
    /// Entries map projection paths (from the local root) to the address space
    /// of capability leaves at that path.
    pub capability_spaces: Vec<(MirProjectionPath<'db>, AddressSpaceKind)>,
}

/// MIR projection using MIR value IDs for dynamic indices.
pub type MirProjection<'db> = Projection<TyId<'db>, EnumVariant<'db>, ValueId>;

/// MIR projection path using MIR value IDs for dynamic indices.
pub type MirProjectionPath<'db> = ProjectionPath<TyId<'db>, EnumVariant<'db>, ValueId>;

/// A linear sequence of MIR instructions terminated by a control-flow edge.
#[derive(Debug, Clone)]
pub struct BasicBlock<'db> {
    pub insts: Vec<MirInst<'db>>,
    pub terminator: Terminator<'db>,
}

impl<'db> BasicBlock<'db> {
    pub fn new() -> Self {
        Self {
            insts: Vec::new(),
            terminator: Terminator::Unreachable {
                source: SourceInfoId::SYNTHETIC,
            },
        }
    }

    pub fn push_inst(&mut self, inst: MirInst<'db>) {
        self.insts.push(inst);
    }

    pub fn set_terminator(&mut self, term: Terminator<'db>) {
        self.terminator = term;
    }
}

impl<'db> Default for BasicBlock<'db> {
    fn default() -> Self {
        Self::new()
    }
}

/// General MIR instruction (does not change control flow).
#[derive(Debug, Clone)]
pub enum MirInst<'db> {
    /// Assigns an rvalue, optionally storing its result into `dest`.
    ///
    /// When `dest` is `None`, the rvalue is evaluated for side effects only.
    Assign {
        source: SourceInfoId,
        dest: Option<LocalId>,
        rvalue: Rvalue<'db>,
    },
    /// Store a value into a place (projection write).
    Store {
        source: SourceInfoId,
        place: Place<'db>,
        value: ValueId,
    },
    /// Initialize an aggregate place (record/tuple/array/enum) from a set of projected writes.
    ///
    /// This is a higher-level form used during lowering so that codegen can decide the final
    /// layout/offsets for the target architecture while still preserving evaluation order
    /// (values are lowered before this instruction is emitted).
    InitAggregate {
        source: SourceInfoId,
        place: Place<'db>,
        inits: Vec<(MirProjectionPath<'db>, ValueId)>,
    },
    /// Store an enum discriminant into a place.
    SetDiscriminant {
        source: SourceInfoId,
        place: Place<'db>,
        variant: EnumVariant<'db>,
    },
    /// Bind a value into a temporary for later reuse.
    ///
    /// This instruction is keyed by `ValueId` (not `ExprId`) so later MIR transforms can move
    /// and rewrite values without needing to preserve HIR node IDs.
    BindValue {
        source: SourceInfoId,
        value: ValueId,
    },
}

/// Rvalue describing a computation or effectful operation.
#[derive(Debug, Clone)]
pub enum Rvalue<'db> {
    /// Default-initialize a destination to zero (backend-specific representation, typically `0`).
    ///
    /// This is used to declare locals in a scope before conditional assignments without needing
    /// a well-typed `ValueId` for the default.
    ZeroInit,
    /// Pure MIR value (`ValueId` DAG).
    Value(ValueId),
    /// Effectful call (user function or lowered intrinsic wrapper).
    Call(CallOrigin<'db>),
    /// Low-level intrinsic operation, optionally yielding a value.
    Intrinsic { op: IntrinsicOp, args: Vec<ValueId> },
    /// Load a scalar value from a place.
    Load { place: Place<'db> },
    /// Allocate an address in the given address space.
    Alloc { address_space: AddressSpaceKind },
    /// Backend-neutral constant aggregate data.
    ///
    /// Pre-computed constant bytes (e.g., constant array literals) that backends
    /// materialize however they choose:
    /// - Yul: emit as data sections + datacopy
    /// - Sonatina: inline mstore sequence or memcpy
    ConstAggregate {
        /// Raw constant bytes in big-endian EVM word format.
        data: Vec<u8>,
        /// The aggregate type being initialized.
        ty: TyId<'db>,
    },
}

/// Control-flow terminating instruction.
#[derive(Debug, Clone)]
pub enum Terminator<'db> {
    /// Return from the function with an optional value.
    Return {
        source: SourceInfoId,
        value: Option<ValueId>,
    },
    /// A call that does not return (callee has return type `!`).
    TerminatingCall {
        source: SourceInfoId,
        call: TerminatingCall<'db>,
    },
    /// Unconditional jump to another block.
    Goto {
        source: SourceInfoId,
        target: BasicBlockId,
    },
    /// Conditional branch based on a boolean value.
    Branch {
        source: SourceInfoId,
        cond: ValueId,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    },
    /// Switch on an integer discriminant.
    Switch {
        source: SourceInfoId,
        discr: ValueId,
        targets: Vec<SwitchTarget>,
        default: BasicBlockId,
    },
    /// Unreachable terminator (used for bodies without an expression).
    Unreachable { source: SourceInfoId },
}

/// A call-like operation used as a terminator (never returns).
#[derive(Debug, Clone)]
pub enum TerminatingCall<'db> {
    Call(CallOrigin<'db>),
    Intrinsic { op: IntrinsicOp, args: Vec<ValueId> },
}

#[derive(Debug, Clone)]
pub struct SwitchTarget {
    pub value: SwitchValue,
    pub block: BasicBlockId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SwitchValue {
    Bool(bool),
    Int(BigUint),
    Enum(u64),
}

impl SwitchValue {
    pub fn as_biguint(&self) -> BigUint {
        match self {
            Self::Bool(value) => {
                if *value {
                    BigUint::from(1u8)
                } else {
                    BigUint::from(0u8)
                }
            }
            Self::Int(value) => value.clone(),
            Self::Enum(value) => BigUint::from(*value),
        }
    }
}

impl fmt::Display for SwitchValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Enum(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopInfo {
    pub body: BasicBlockId,
    pub exit: BasicBlockId,
    pub backedge: Option<BasicBlockId>,
    /// Optional block containing the loop initialization (e.g., iterator variable init for for-loops).
    /// If present, this block's instructions are rendered as the Yul for-loop init section.
    pub init_block: Option<BasicBlockId>,
    /// Optional block containing the post-iteration code (e.g., increment for for-loops).
    /// If present, this block's instructions are rendered as the Yul for-loop post section.
    pub post_block: Option<BasicBlockId>,
    /// Unroll hint from source attributes: `Some(true)` = `#[unroll]`, `Some(false)` = `#[no_unroll]`, `None` = auto.
    pub unroll_hint: Option<bool>,
    /// Statically-known trip count, if the iterator length is a compile-time constant.
    /// Backends use this together with `unroll_hint` to decide whether to unroll.
    pub trip_count: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct ValueData<'db> {
    pub ty: TyId<'db>,
    pub origin: ValueOrigin<'db>,
    pub source: SourceInfoId,
    /// Runtime representation category for this MIR value.
    ///
    /// Most values are represented as a single EVM word. Aggregate values (structs/tuples/arrays/enums)
    /// are represented by-reference as an address/slot pointing at their backing storage.
    pub repr: ValueRepr,
}

#[derive(Debug, Clone)]
pub enum ValueOrigin<'db> {
    /// Unlowered HIR expression.
    ///
    /// This should not reach codegen; it exists as a construction-time placeholder for
    /// expressions that require additional MIR lowering.
    Expr(ExprId),
    /// Value produced by control-flow lowering (e.g. `if`/`match` expressions).
    ///
    /// Codegen should treat this as a value that is assigned along CFG edges (phi-like).
    ControlFlowResult {
        expr: ExprId,
    },
    /// Unit value `()`.
    Unit,
    /// Unary scalar operation.
    Unary {
        op: hir::hir_def::expr::UnOp,
        inner: ValueId,
    },
    /// Binary scalar operation (arithmetic, comparison, logical).
    Binary {
        op: hir::hir_def::expr::BinOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    Synthetic(SyntheticValue),
    /// Reference a MIR local (function parameter, effect parameter, or local variable).
    Local(LocalId),
    /// A semantic root place for a local.
    ///
    /// This is used by capability-stage MIR to model "place" operations (load/store/projections)
    /// over locals/params without implying the local is backed by a runtime address.
    PlaceRoot(LocalId),
    /// A first-class reference to a function item (compile-time only).
    ///
    /// This is not a runtime value, but can be consumed by intrinsics like
    /// `code_region_offset/len` that refer to function code regions.
    FuncItem(CodeRegionRoot<'db>),
    /// Pointer arithmetic for accessing a nested struct field (no load, just offset).
    FieldPtr(FieldPtrOrigin),
    /// Reference to a place (for aggregates - pointer arithmetic only, no load).
    PlaceRef(Place<'db>),
    /// Marker for ownership moves from places (runtime no-op, analysis hook).
    MoveOut {
        place: Place<'db>,
    },
    /// A representation-preserving coercion (e.g. transparent wrapper construction).
    ///
    /// This keeps the logical type of the value (`ValueData::ty`) while reusing the runtime
    /// representation of `value` unchanged.
    TransparentCast {
        value: ValueId,
    },
}

/// Captures compile-time literals synthesized by lowering.
#[derive(Debug, Clone)]
pub enum SyntheticValue {
    /// Integer literal emitted directly into Yul.
    Int(BigUint),
    /// Boolean literal stored as `0` or `1`.
    Bool(bool),
    /// Byte string literal encoded as a `0x...` word.
    ///
    /// This is a stopgap representation: the literal is emitted inline as a numeric constant.
    /// Only suitable for data that fits in a single EVM word (≤32 bytes).
    Bytes(Vec<u8>),
}

/// Address space where a value lives.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressSpaceKind {
    Memory,
    Calldata,
    Storage,
    TransientStorage,
}

/// Runtime representation category for a MIR value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueRepr {
    /// A direct EVM word value (e.g. integer, bool, pointer-provider newtypes).
    Word,
    /// A pointer-like word value in an address space.
    ///
    /// Unlike [`ValueRepr::Ref`], this does *not* imply by-reference aggregate semantics (deep
    /// copy on assignment). It is used for opaque pointers such as effect pointer providers
    /// (`MemPtr`/`StorPtr`/`CalldataPtr`) where the value is a raw address/slot.
    Ptr(AddressSpaceKind),
    /// An address/slot into an address space that points at a value of `ValueData::ty`.
    Ref(AddressSpaceKind),
}

impl ValueRepr {
    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref(_))
    }

    pub fn address_space(self) -> Option<AddressSpaceKind> {
        match self {
            Self::Word => None,
            Self::Ptr(space) => Some(space),
            Self::Ref(space) => Some(space),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallOrigin<'db> {
    pub expr: Option<ExprId>,
    pub hir_target: Option<HirCallTarget<'db>>,
    pub args: Vec<ValueId>,
    /// Explicit lowered effect arguments for this call, in callee effect-param order.
    pub effect_args: Vec<ValueId>,
    /// Final lowered symbol name of the callee after monomorphization.
    pub resolved_name: Option<String>,
    /// For methods on struct types, the statically known address space of the receiver.
    pub receiver_space: Option<AddressSpaceKind>,
}

/// HIR-based call target metadata used by monomorphization and diagnostics.
#[derive(Debug, Clone)]
pub struct HirCallTarget<'db> {
    pub callable_def: CallableDef<'db>,
    pub generic_args: Vec<TyId<'db>>,
    pub trait_inst: Option<TraitInstId<'db>>,
}

/// Pointer offset for accessing a nested aggregate field (struct within struct).
/// This represents pure pointer arithmetic with no load/store.
#[derive(Debug, Clone)]
pub struct FieldPtrOrigin {
    /// Base pointer value being offset.
    pub base: ValueId,
    /// Byte offset to add to the base pointer.
    pub offset_bytes: usize,
    /// Address space of the base pointer (controls offset scaling in codegen).
    pub addr_space: AddressSpaceKind,
}

/// A place describes a location that can be read from or written to.
/// Consists of a base value and a projection path describing how to navigate
/// from the base to the actual location.
#[derive(Debug, Clone)]
pub struct Place<'db> {
    /// The base value (e.g., a local variable, parameter, or allocation).
    pub base: ValueId,
    /// Sequence of projections to apply to reach the target location.
    pub projection: MirProjectionPath<'db>,
}

impl<'db> Place<'db> {
    pub fn new(base: ValueId, projection: MirProjectionPath<'db>) -> Self {
        Self { base, projection }
    }
}

#[derive(Debug, Clone)]
pub struct IntrinsicValue {
    /// Which intrinsic operation this value represents.
    pub op: IntrinsicOp,
    /// Already-lowered argument `ValueId`s (still need converting to Yul expressions later).
    pub args: Vec<ValueId>,
}

/// Identifies the root function for a code region along with its concrete instantiation.
#[derive(Debug, Clone)]
pub struct CodeRegionRoot<'db> {
    pub origin: MirFunctionOrigin<'db>,
    pub generic_args: Vec<TyId<'db>>,
    pub symbol: Option<String>,
}

/// Kind of contract-related function declared via attributes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContractFunctionKind {
    Init,
    Runtime,
}

/// Metadata attached to functions marked as contract init/runtime entrypoints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractFunction {
    pub contract_name: String,
    pub kind: ContractFunctionKind,
}

/// Low-level runtime operations that bypass normal function calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOp {
    /// `mload(address)`
    Mload,
    /// `calldataload(offset)`
    Calldataload,
    /// `calldatacopy(dest, offset, size)`
    Calldatacopy,
    /// `calldatasize()`
    Calldatasize,
    /// `returndatacopy(dest, offset, size)`
    Returndatacopy,
    /// `returndatasize()`
    Returndatasize,
    /// `addr_of(ptr)` - returns the address of a pointer value as `u256`.
    AddrOf,
    /// `mstore(address, value)`
    Mstore,
    /// `mstore8(address, byte)`
    Mstore8,
    /// `sload(slot)`
    Sload,
    /// `sstore(slot, value)`
    Sstore,
    /// `return(offset, size)`
    ReturnData,
    /// `codecopy(dest, offset, size)`
    Codecopy,
    /// `codesize()`
    Codesize,
    /// `dataoffset` of the code region rooted at a function.
    CodeRegionOffset,
    /// `datasize` of the code region rooted at a function.
    CodeRegionLen,
    /// `datasize` of the current code section/object.
    CurrentCodeRegionLen,
    /// `keccak256(ptr, len)`
    Keccak,
    /// `addmod(a, b, m)` — (a + b) % m without overflow
    Addmod,
    /// `mulmod(a, b, m)` — (a * b) % m without overflow
    Mulmod,
    /// `revert(offset, size)`
    Revert,
    /// `caller()`
    Caller,
    /// `alloc(size)` - allocate `size` bytes in EVM linear memory and return the start pointer.
    Alloc,
}

impl IntrinsicOp {
    /// Returns `true` if this intrinsic yields a value (load), `false` for pure side effects.
    pub fn returns_value(self) -> bool {
        matches!(
            self,
            IntrinsicOp::Mload
                | IntrinsicOp::Sload
                | IntrinsicOp::Calldataload
                | IntrinsicOp::Calldatasize
                | IntrinsicOp::Returndatasize
                | IntrinsicOp::AddrOf
                | IntrinsicOp::Codesize
                | IntrinsicOp::CodeRegionOffset
                | IntrinsicOp::CodeRegionLen
                | IntrinsicOp::CurrentCodeRegionLen
                | IntrinsicOp::Keccak
                | IntrinsicOp::Addmod
                | IntrinsicOp::Mulmod
                | IntrinsicOp::Caller
                | IntrinsicOp::Alloc
        )
    }
}
