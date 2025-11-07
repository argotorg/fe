use hir::hir_def::{ExprId, Func, TopLevelMod};
use hir_analysis::ty::ty_check::TypedBody;

/// MIR for an entire top-level module.
#[derive(Debug)]
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
#[derive(Debug)]
pub struct MirFunction<'db> {
    pub func: Func<'db>,
    pub body: MirBody,
    pub typed_body: TypedBody<'db>,
}

/// A function body expressed as basic blocks.
#[derive(Debug)]
pub struct MirBody {
    pub entry: BasicBlockId,
    pub blocks: Vec<BasicBlock>,
}

impl MirBody {
    pub fn new() -> Self {
        Self {
            entry: BasicBlockId(0),
            blocks: Vec::new(),
        }
    }

    pub fn push_block(&mut self, block: BasicBlock) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len());
        if self.blocks.is_empty() {
            self.entry = id;
        }
        self.blocks.push(block);
        id
    }
}

/// Identifier for a basic block (dense index into `MirBody::blocks`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub usize);

/// A linear sequence of MIR instructions terminated by a control-flow edge.
#[derive(Debug)]
pub struct BasicBlock {
    pub insts: Vec<MirInst>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(terminator: Terminator) -> Self {
        Self {
            insts: Vec::new(),
            terminator,
        }
    }
}

/// General MIR instruction (does not change control flow).
#[derive(Debug)]
pub enum MirInst {
    /// Placeholder instruction referencing a typed expression.
    Expr(ExprId),
}

/// Control-flow terminating instruction.
#[derive(Debug)]
pub enum Terminator {
    /// Return from the function with an optional value.
    Return(Option<ExprId>),
    /// Unreachable terminator (used for bodies without an expression).
    Unreachable,
}
