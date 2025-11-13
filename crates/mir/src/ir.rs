use std::fmt;

use hir::hir_def::{
    ExprId, Func, PatId, StmtId, TopLevelMod, TypeId as HirTypeId, expr::ArithBinOp,
};
use hir_analysis::ty::ty_check::{Callable, TypedBody};
use hir_analysis::ty::ty_def::TyId;
use num_bigint::BigUint;
use rustc_hash::FxHashMap;

/// MIR for an entire top-level module.
#[derive(Debug, Clone)]
pub struct MirModule<'db> {
    pub top_mod: TopLevelMod<'db>,
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
    /// Symbol name used for codegen (includes monomorphization suffix when present).
    pub symbol_name: String,
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
}

/// Control-flow terminating instruction.
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from the function with an optional value.
    Return(Option<ValueId>),
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
    pub arms: Vec<MatchArmLowering>,
}

#[derive(Debug, Clone)]
pub struct MatchArmLowering {
    pub pattern: MatchArmPattern,
    pub body: ExprId,
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

#[derive(Debug, Clone)]
pub struct CallOrigin<'db> {
    pub expr: ExprId,
    pub callable: Callable<'db>,
    pub args: Vec<ValueId>,
    /// Final lowered symbol name of the callee after monomorphization.
    pub resolved_name: Option<String>,
}
