use hir::hir_def::{
    ExprId, Func, PatId, StmtId, TopLevelMod, TypeId as HirTypeId,
    expr::ArithBinOp,
};
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
    pub body: MirBody<'db>,
    pub typed_body: TypedBody<'db>,
}

/// A function body expressed as basic blocks.
#[derive(Debug)]
pub struct MirBody<'db> {
    pub entry: BasicBlockId,
    pub blocks: Vec<BasicBlock<'db>>,
}

impl<'db> MirBody<'db> {
    pub fn new() -> Self {
        Self {
            entry: BasicBlockId(0),
            blocks: Vec::new(),
        }
    }

    pub fn push_block(&mut self, block: BasicBlock<'db>) -> BasicBlockId {
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
pub struct BasicBlock<'db> {
    pub insts: Vec<MirInst<'db>>,
    pub terminator: Terminator,
}

impl<'db> BasicBlock<'db> {
    pub fn new(terminator: Terminator) -> Self {
        Self {
            insts: Vec::new(),
            terminator,
        }
    }

    pub fn push_inst(&mut self, inst: MirInst<'db>) {
        self.insts.push(inst);
    }
}

/// General MIR instruction (does not change control flow).
#[derive(Debug)]
pub enum MirInst<'db> {
    /// A `let` binding statement.
    Let {
        stmt: StmtId,
        pat: PatId,
        ty: Option<HirTypeId<'db>>,
        value: Option<ExprId>,
    },
    /// Desugared assignment statement.
    Assign {
        stmt: StmtId,
        target: ExprId,
        value: ExprId,
    },
    /// Augmented assignment (`+=`, `-=`, ...).
    AugAssign {
        stmt: StmtId,
        target: ExprId,
        value: ExprId,
        op: ArithBinOp,
    },
    /// Plain expression statement (no bindings).
    Expr {
        stmt: StmtId,
        expr: ExprId,
    },
    /// High-level representation of a `for` loop.
    ForLoop {
        stmt: StmtId,
        pat: PatId,
        iter: ExprId,
        body: ExprId,
    },
    /// High-level representation of a `while` loop.
    WhileLoop {
        stmt: StmtId,
        cond: ExprId,
        body: ExprId,
    },
    /// `break` statement.
    Break {
        stmt: StmtId,
    },
    /// `continue` statement.
    Continue {
        stmt: StmtId,
    },
    /// Explicit `return` statement inside the block.
    Return {
        stmt: StmtId,
        value: Option<ExprId>,
    },
}

/// Control-flow terminating instruction.
#[derive(Debug)]
pub enum Terminator {
    /// Return from the function with an optional value.
    Return(Option<ExprId>),
    /// Unreachable terminator (used for bodies without an expression).
    Unreachable,
}
