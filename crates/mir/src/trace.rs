use trace_facts::{OriginNodeFact, OriginNodeKind, TraceFact};

use crate::{
    MirDb, RuntimePackage,
    origin::{
        RUNTIME_STMT_EXPORT_KIND, RUNTIME_TERMINATOR_EXPORT_KIND, RuntimeInstanceOwnerKey,
        RuntimeStmtIndex, RuntimeStmtOrigin, RuntimeStmtSite, RuntimeTerminatorOrigin,
        RuntimeTerminatorSite,
    },
    runtime::RBlockId,
};

/// Emit MIR/runtime-owned trace facts for a runtime package.
///
/// MIR owns runtime statement and terminator identity. Backend storage slots,
/// registers, final instructions, and codegen events are emitted by codegen.
pub fn emit_mir_facts<'db>(db: &'db dyn MirDb, package: RuntimePackage<'db>) -> Vec<TraceFact> {
    let mut facts = Vec::new();
    for function in package.functions(db) {
        let owner_key = RuntimeInstanceOwnerKey::new(format!("runtime:{}", function.symbol(db)));
        let instance = function.instance(db);
        let body = instance.body(db);
        for (block_index, runtime_block) in body.blocks.iter().enumerate() {
            let block = RBlockId::from_u32(block_index as u32);
            for (stmt_index, _) in runtime_block.stmts.iter().enumerate() {
                let site =
                    RuntimeStmtSite::new(block, RuntimeStmtIndex::from_u32(stmt_index as u32));
                facts.push(origin_node(
                    RuntimeStmtOrigin::new(instance, site).export_key(&owner_key),
                    RUNTIME_STMT_EXPORT_KIND,
                ));
            }
            let terminator_site = RuntimeTerminatorSite::new(block);
            facts.push(origin_node(
                RuntimeTerminatorOrigin::new(instance, terminator_site).export_key(&owner_key),
                RUNTIME_TERMINATOR_EXPORT_KIND,
            ));
        }
    }
    facts
}

fn origin_node(key: common::origin::OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}
