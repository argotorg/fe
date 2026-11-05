use cranelift_entity::entity_impl;

use super::{Body, CondId, ExprId, Partial, PatId, TypeId};
use crate::{HirDb, span::stmt::LazyStmtSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum Stmt<'db> {
    /// The `let` statement. The first `PatId` is the pattern for binding, the
    /// second `Option<TypeId>` is the type annotation, the third
    /// `Option<ExprId>` is the expression for initialization, and the fourth
    /// the `else` block of a `let … else`, which runs when the pattern does
    /// not match and must diverge.
    Let(PatId, Option<TypeId<'db>>, Option<ExprId>, Option<ExprId>),
    /// For loop statement.
    ///
    /// The first `PatId` is the pattern for binding which can be used in the
    /// for-loop body.
    ///
    /// The second `ExprId` is the iterable expression.
    ///
    /// The third `ExprId` is the for-loop body.
    ///
    /// The fourth field is the unroll hint:
    /// - `None`: no attribute, use auto-unroll heuristics (unroll if < 10 iterations)
    /// - `Some(true)`: #[unroll] attribute forces unrolling
    /// - `Some(false)`: #[unroll(never)] attribute prevents unrolling
    For(PatId, ExprId, ExprId, Option<bool>),

    /// The first `CondId` is the condition of the while-loop.
    /// The second `ExprId` is the body of the while-loop.
    While(CondId, ExprId),
    Continue,
    Break,
    Return(Option<ExprId>),
    /// A projection's `yield`; the statements after it on its path are its
    /// slide.
    Yield(ExprId),
    Expr(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct StmtId(u32);
entity_impl!(StmtId);

impl StmtId {
    pub fn span(self, body: Body) -> LazyStmtSpan {
        LazyStmtSpan::new(body, self)
    }

    pub fn data<'db>(self, db: &'db dyn HirDb, body: Body<'db>) -> &'db Partial<Stmt<'db>> {
        &body.stmts(db)[self]
    }
}
