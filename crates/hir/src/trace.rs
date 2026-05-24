use trace_facts::{OriginNodeFact, OriginNodeKind, TraceFact};

use crate::origin::{
    HIR_EXPR_EXPORT_KIND, HIR_STMT_EXPORT_KIND, HirExprOrigin, HirOriginBodyOwnerKey, HirStmtOrigin,
};

/// Emit HIR-owned trace facts for exported HIR origins.
///
/// HIR owns HIR expression/statement identity. It does not emit MIR, backend,
/// storage, or instruction facts.
pub fn emit_hir_facts<'db>(
    stable_body_key: &HirOriginBodyOwnerKey,
    exprs: impl IntoIterator<Item = HirExprOrigin<'db>>,
    stmts: impl IntoIterator<Item = HirStmtOrigin<'db>>,
) -> Vec<TraceFact> {
    exprs
        .into_iter()
        .map(|origin| origin_node(origin.export_key(stable_body_key), HIR_EXPR_EXPORT_KIND))
        .chain(
            stmts.into_iter().map(|origin| {
                origin_node(origin.export_key(stable_body_key), HIR_STMT_EXPORT_KIND)
            }),
        )
        .collect()
}

fn origin_node(key: common::origin::OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}
