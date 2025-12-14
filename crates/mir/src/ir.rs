use std::fmt;

use hir::analysis::ty::ty_check::{Callable, TypedBody};
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::{
    ExprId, Func, PatId, StmtId, TopLevelMod, TypeId as HirTypeId, expr::ArithBinOp,
};
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
    Call(CallOrigin<'db>),
    /// Call to a compiler intrinsic that should lower to a raw opcode, not a function call.
    Intrinsic(IntrinsicValue<'db>),
    /// Pointer arithmetic for accessing a nested struct field (no load, just offset).
    FieldPtr(FieldPtrOrigin),
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
}

/// Information about a variable binding extracted from a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternBinding {
    /// The pattern ID representing the binding (e.g., `x` in `Some(x)`).
    pub pat_id: PatId,
    /// Byte offset of this field within the variant's data region (after discriminant).
    pub field_offset: u64,
    /// MIR value representing the lowered load for this binding (if synthesized).
    pub value: Option<ValueId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchArmPattern {
    Literal(SwitchValue),
    Wildcard,
    Enum {
        variant_index: u64,
        enum_name: String,
        /// Bindings extracted from the variant's data (empty for unit variants).
        bindings: Vec<PatternBinding>,
    },
}

/// Address space where a value lives.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressSpaceKind {
    Memory,
    Storage,
}

#[derive(Debug, Clone)]
pub struct CallOrigin<'db> {
    pub expr: ExprId,
    pub callable: Callable<'db>,
    pub args: Vec<ValueId>,
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
    pub offset_bytes: u64,
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
}

impl IntrinsicOp {
    /// Returns `true` if this intrinsic yields a value (load), `false` for pure side effects.
    pub fn returns_value(self) -> bool {
        matches!(
            self,
            IntrinsicOp::Mload
                | IntrinsicOp::Sload
                | IntrinsicOp::Calldataload
                | IntrinsicOp::AddrOf
                | IntrinsicOp::CodeRegionOffset
                | IntrinsicOp::CodeRegionLen
                | IntrinsicOp::Keccak
                | IntrinsicOp::Caller
        )
    }
}
