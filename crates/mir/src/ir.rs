use std::fmt;

mod body_builder;

pub use body_builder::BodyBuilder;

use hir::analysis::ty::ty_check::{Callable, TypedBody};
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::{
    EnumVariant, ExprId, Func, PatId, StmtId, TopLevelMod, TypeId as HirTypeId, expr::ArithBinOp,
};
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
    pub func: Func<'db>,
    pub body: MirBody<'db>,
    pub typed_body: TypedBody<'db>,
    /// Concrete generic arguments used to instantiate this function instance.
    pub generic_args: Vec<TyId<'db>>,
    /// Effect provider kinds for this instance, indexed by effect param position.
    pub effect_provider_kinds: Vec<EffectProviderKind>,
    /// Optional contract association declared via attributes.
    pub contract_function: Option<ContractFunction>,
    /// Symbol name used for codegen (includes monomorphization suffix when present).
    pub symbol_name: String,
    /// For methods, the address space variant of the receiver for this instance.
    pub receiver_space: Option<AddressSpaceKind>,
}

/// A function body expressed as basic blocks.
#[derive(Debug, Clone)]
pub struct MirBody<'db> {
    pub entry: BasicBlockId,
    pub blocks: Vec<BasicBlock<'db>>,
    pub values: Vec<ValueData<'db>>,
    pub expr_values: FxHashMap<ExprId, ValueId>,
    pub pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub loop_headers: FxHashMap<BasicBlockId, LoopInfo>,
    pub match_info: FxHashMap<ExprId, MatchLoweringInfo>,
}

impl<'db> MirBody<'db> {
    pub fn new() -> Self {
        Self {
            entry: BasicBlockId(0),
            blocks: Vec::new(),
            values: Vec::new(),
            expr_values: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            loop_headers: FxHashMap::default(),
            match_info: FxHashMap::default(),
        }
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

    pub fn value(&self, id: ValueId) -> &ValueData<'db> {
        &self.values[id.index()]
    }

    pub fn match_info(&self, expr: ExprId) -> Option<&MatchLoweringInfo> {
        self.match_info.get(&expr)
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

/// MIR projection using MIR value IDs for dynamic indices.
pub type MirProjection<'db> = Projection<TyId<'db>, EnumVariant<'db>, ValueId>;

/// MIR projection path using MIR value IDs for dynamic indices.
pub type MirProjectionPath<'db> = ProjectionPath<TyId<'db>, EnumVariant<'db>, ValueId>;

/// A linear sequence of MIR instructions terminated by a control-flow edge.
#[derive(Debug, Clone)]
pub struct BasicBlock<'db> {
    pub insts: Vec<MirInst<'db>>,
    pub terminator: Terminator,
}

impl<'db> BasicBlock<'db> {
    pub fn new() -> Self {
        Self {
            insts: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }

    pub fn push_inst(&mut self, inst: MirInst<'db>) {
        self.insts.push(inst);
    }

    pub fn set_terminator(&mut self, term: Terminator) {
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
    /// A `let` binding statement.
    Let {
        stmt: StmtId,
        pat: PatId,
        ty: Option<HirTypeId<'db>>,
        value: Option<ValueId>,
    },
    /// Desugared assignment statement.
    Assign {
        stmt: StmtId,
        target: ExprId,
        value: ValueId,
    },
    /// Augmented assignment (`+=`, `-=`, ...).
    AugAssign {
        stmt: StmtId,
        target: ExprId,
        value: ValueId,
        op: ArithBinOp,
    },
    /// Store a value into a place (projection write).
    Store {
        expr: ExprId,
        place: Place<'db>,
        value: ValueId,
    },
    /// Initialize an aggregate place (record/tuple/array/enum) from a set of projected writes.
    ///
    /// This is a higher-level form used during lowering so that codegen can decide the final
    /// layout/offsets for the target architecture while still preserving evaluation order
    /// (values are lowered before this instruction is emitted).
    InitAggregate {
        expr: ExprId,
        place: Place<'db>,
        inits: Vec<(MirProjectionPath<'db>, ValueId)>,
    },
    /// Store an enum discriminant into a place.
    SetDiscriminant {
        expr: ExprId,
        place: Place<'db>,
        variant: EnumVariant<'db>,
    },
    /// Plain expression statement (no bindings).
    Eval { stmt: StmtId, value: ValueId },
    /// Synthetic expression emitted during lowering (no originating statement). This lets
    /// expression lowering insert helper calls (e.g. alloc/store_field) while still keeping
    /// the resulting value associated with its originating expression.
    EvalExpr {
        expr: ExprId,
        value: ValueId,
        /// Whether the value should be bound to a temporary for later reuse.
        bind_value: bool,
    },
    /// Statement-only intrinsic (e.g. `mstore`) that produces no value.
    IntrinsicStmt {
        expr: ExprId,
        op: IntrinsicOp,
        args: Vec<ValueId>,
    },
}

/// Control-flow terminating instruction.
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from the function with an optional value.
    Return(Option<ValueId>),
    /// Return from the function using raw memory pointer/size (core::return_data).
    ReturnData { offset: ValueId, size: ValueId },
    /// Abort execution and revert with memory region at `offset`/`size`.
    Revert { offset: ValueId, size: ValueId },
    /// Unconditional jump to another block.
    Goto { target: BasicBlockId },
    /// Conditional branch based on a boolean value.
    Branch {
        cond: ValueId,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    },
    /// Switch on an integer discriminant.
    Switch {
        discr: ValueId,
        targets: Vec<SwitchTarget>,
        default: BasicBlockId,
        origin: SwitchOrigin,
    },
    /// Unreachable terminator (used for bodies without an expression).
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct SwitchTarget {
    pub value: SwitchValue,
    pub block: BasicBlockId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwitchOrigin {
    None,
    MatchExpr(ExprId),
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
}

#[derive(Debug, Clone)]
pub struct ValueData<'db> {
    pub ty: TyId<'db>,
    pub origin: ValueOrigin<'db>,
}

#[derive(Debug, Clone)]
pub enum ValueOrigin<'db> {
    Expr(ExprId),
    Synthetic(SyntheticValue),
    Pat(PatId),
    Param(Func<'db>, usize),
    /// Reference a named Yul binding in the current function scope.
    BindingName(String),
    Call(CallOrigin<'db>),
    /// Call to a compiler intrinsic that should lower to a raw opcode, not a function call.
    Intrinsic(IntrinsicValue<'db>),
    /// Pointer arithmetic for accessing a nested struct field (no load, just offset).
    FieldPtr(FieldPtrOrigin),
    /// Load a value from a place (for primitives/scalars).
    PlaceLoad(Place<'db>),
    /// Reference to a place (for aggregates - pointer arithmetic only, no load).
    PlaceRef(Place<'db>),
    /// Allocate memory for an aggregate value.
    Alloc {
        address_space: AddressSpaceKind,
    },
}

impl<'db> ValueOrigin<'db> {
    /// Returns the contained call origin if this value represents a function call.
    pub fn as_call(&self) -> Option<&CallOrigin<'db>> {
        match self {
            ValueOrigin::Call(call) => Some(call),
            _ => None,
        }
    }

    /// Returns the contained call origin mutably if this value represents a call.
    pub fn as_call_mut(&mut self) -> Option<&mut CallOrigin<'db>> {
        match self {
            ValueOrigin::Call(call) => Some(call),
            _ => None,
        }
    }
}

/// Captures compile-time literals synthesized by lowering.
#[derive(Debug, Clone)]
pub enum SyntheticValue {
    /// Integer literal emitted directly into Yul.
    Int(BigUint),
    /// Boolean literal stored as `0` or `1`.
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct MatchLoweringInfo {
    /// The scrutinee value (e.g. enum pointer) for this match expression.
    pub scrutinee: ValueId,
    /// Merge block selected when any arm continues execution; `None` when all arms terminate.
    pub merge_block: Option<BasicBlockId>,
    pub arms: Vec<MatchArmLowering>,
}

#[derive(Debug, Clone)]
pub struct MatchArmLowering {
    pub pattern: MatchArmPattern,
    pub body: ExprId,
    pub block: BasicBlockId,
    pub terminates: bool,
    /// Resulting value for value-producing matches/ifs.
    ///
    /// When present, codegen should evaluate the arm's MIR block (for side effects and
    /// temporaries) and then assign this value into the match/if result temp.
    pub value: Option<ValueId>,
    /// Bindings from decision tree pattern matching (for tuple/struct patterns).
    /// These map variable names to MIR values extracted from the scrutinee.
    pub decision_tree_bindings: Vec<DecisionTreeBinding>,
}

/// A binding from decision tree pattern matching.
/// Maps a variable name to the MIR value representing its extracted value.
#[derive(Debug, Clone)]
pub struct DecisionTreeBinding {
    /// The variable name (e.g., "x", "y").
    pub name: String,
    /// MIR value referencing the bound location (pointer/address expression).
    ///
    /// This is emitted as a `PlaceRef` value so downstream passes that care about
    /// reference semantics can recover the underlying place/projection path even
    /// when the binding's value is loaded eagerly.
    pub place: ValueId,
    /// MIR value for the extracted field (computed via lower_occurrence).
    pub value: ValueId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchArmPattern {
    Literal(SwitchValue),
    Wildcard,
    Enum {
        variant_index: u64,
        enum_name: String,
    },
}

/// Address space where a value lives.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressSpaceKind {
    Memory,
    Storage,
}

/// Runtime "domain" for an effect provider value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectProviderKind {
    Memory,
    Storage,
    Calldata,
}

#[derive(Debug, Clone)]
pub struct CallOrigin<'db> {
    pub expr: ExprId,
    pub callable: Callable<'db>,
    pub args: Vec<ValueId>,
    /// Explicit lowered effect arguments for this call, in callee effect-param order.
    pub effect_args: Vec<ValueId>,
    /// Effect provider kinds for this call, in callee effect-param order.
    pub effect_kinds: Vec<EffectProviderKind>,
    /// Final lowered symbol name of the callee after monomorphization.
    pub resolved_name: Option<String>,
    /// For methods on struct types, the statically known address space of the receiver.
    pub receiver_space: Option<AddressSpaceKind>,
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
    /// Address space where this place resides (memory vs storage).
    pub address_space: AddressSpaceKind,
}

impl<'db> Place<'db> {
    pub fn new(
        base: ValueId,
        projection: MirProjectionPath<'db>,
        address_space: AddressSpaceKind,
    ) -> Self {
        Self {
            base,
            projection,
            address_space,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntrinsicValue<'db> {
    /// Which intrinsic operation this value represents.
    pub op: IntrinsicOp,
    /// Already-lowered argument `ValueId`s (still need converting to Yul expressions later).
    pub args: Vec<ValueId>,
    /// Target metadata for intrinsics that refer to a function's code region.
    pub code_region: Option<CodeRegionRoot<'db>>,
}

/// Identifies the root function for a code region along with its concrete instantiation.
#[derive(Debug, Clone)]
pub struct CodeRegionRoot<'db> {
    pub func: Func<'db>,
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
    /// `keccak256(ptr, len)`
    Keccak,
    /// `revert(offset, size)`
    Revert,
    /// `caller()`
    Caller,
    /// `stor_at(slot)` - interpret a slot as a storage-backed `T` pointer value.
    StorAt,
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
                | IntrinsicOp::Keccak
                | IntrinsicOp::Caller
                | IntrinsicOp::StorAt
        )
    }
}
