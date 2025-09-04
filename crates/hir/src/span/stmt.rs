use parser::ast;
use salsa::Update;

use super::{
    body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, ResolvedOrigin, SpanTransitionChain},
};
use crate::{
    SpannedHirDb,
    hir_def::{Body, StmtId},
    span::types::LazyTySpan,
};

define_lazy_span_node!(LazyStmtSpan, ast::Stmt,);
impl<'db> LazyStmtSpan<'db> {
    pub fn new(body: Body<'db>, stmt: StmtId) -> Self {
        let root = StmtRoot { stmt, body };
        Self(SpanTransitionChain::new(root))
    }

    pub fn into_let_stmt(self) -> LazyLetStmtSpan<'db> {
        LazyLetStmtSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyLetStmtSpan,
    ast::LetStmt,
    @node {
        (ty, type_annotation, LazyTySpan),
    }
);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Update)]
pub(crate) struct StmtRoot<'db> {
    stmt: StmtId,
    pub(crate) body: Body<'db>,
}

impl ChainInitiator for StmtRoot<'_> {
    fn init(&self, db: &dyn SpannedHirDb) -> ResolvedOrigin {
        let source_map = body_source_map(db, self.body);
        let origin = source_map.stmt_map.node_to_source(self.stmt);
        let top_mod = self.body.top_mod(db);
        ResolvedOrigin::resolve(db, top_mod, origin)
    }
}
