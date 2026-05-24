use trace_facts::{OriginNodeFact, OriginNodeKind, TraceFact};

use crate::debug::BytecodeSourceMapEntry;

/// Emit codegen-owned trace facts for bytecode/source-map records.
///
/// Codegen owns bytecode PC identity. It does not create HIR or MIR origin
/// identity; edges to those origins are emitted only when codegen has that
/// phase-owned mapping.
pub fn emit_codegen_facts<'a>(
    entries: impl IntoIterator<Item = &'a BytecodeSourceMapEntry>,
) -> Vec<TraceFact> {
    entries
        .into_iter()
        .map(|entry| {
            TraceFact::OriginNode(OriginNodeFact::new(
                entry.origin.clone(),
                OriginNodeKind::new(entry.origin.kind()),
            ))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{TraceFact, TraceValidator};

    use crate::{BytecodePcRange, BytecodeSourceMapEntry, trace::emit_codegen_facts};

    #[test]
    fn codegen_trace_emits_only_bytecode_origin_nodes() {
        let origin =
            OriginExportKey::try_from_raw_parts("bytecode.pc", "runtime:main", "pc:0..2").unwrap();
        let entry = BytecodeSourceMapEntry::non_source(
            origin.clone(),
            BytecodePcRange::try_new(0, 2).unwrap(),
            "abi dispatch",
        )
        .unwrap();

        let facts = emit_codegen_facts([&entry]);
        assert_eq!(TraceValidator::validate(&facts).unwrap().node_count, 1);
        assert!(matches!(
            &facts[0],
            TraceFact::OriginNode(node) if node.key == origin
        ));
    }
}
