use std::fmt::Write as _;

use common::source_ord::FunctionSourceTable;
use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact};

use crate::sonatina::MirToIrEntry;

pub struct DatalogFacts {
    pub facts: String,
}

pub fn generate_datalog_facts(
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
) -> DatalogFacts {
    generate_datalog_facts_full(artifacts, provenance, &[], &[])
}

pub fn generate_datalog_facts_full(
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
    mir_to_ir: &[MirToIrEntry],
    source_tables: &[(u32, &FunctionSourceTable)],
) -> DatalogFacts {
    let mut out = String::new();

    // MIR-level facts: mir_stmt(func, block, stmt, file, line, col)
    for (func_id, table) in source_tables {
        for (ord, entry) in table.iter() {
            writeln!(
                &mut out,
                "mir_source({}, {}, \"{}\", {}, {}, {}, {}).",
                func_id,
                ord.index(),
                entry.file_path,
                entry.start_line,
                entry.start_col,
                entry.end_line,
                entry.end_col,
            )
            .unwrap();
        }
    }

    // Cross-level: mir_to_ir(func, ir_inst, mir_block, mir_stmt)
    for entry in mir_to_ir {
        writeln!(
            &mut out,
            "mir_to_ir({}, {}, {}, {}).",
            entry.func.as_u32(),
            entry.inst.0,
            entry.mir_block,
            entry.mir_stmt,
        )
        .unwrap();

        if !entry.source_ord.is_default() {
            writeln!(
                &mut out,
                "ir_source_ord({}, {}, {}).",
                entry.func.as_u32(),
                entry.inst.0,
                entry.source_ord.index(),
            )
            .unwrap();
        }
    }

    // Bytecode-level facts from observability
    for artifact in artifacts {
        let object_name = &artifact.object.0;
        for (section_name, section) in &artifact.sections {
            let Some(observability) = &section.observability else {
                continue;
            };

            for entry in &observability.pc_map {
                writeln!(
                    &mut out,
                    "bytecode_range(\"{}\", \"{}\", {}, {}, {}, {}).",
                    object_name,
                    section_name.0,
                    entry.pc_start,
                    entry.pc_end,
                    entry.func.as_u32(),
                    entry.block.0,
                )
                .unwrap();

                if let Some(ir_inst) = entry.ir_inst {
                    writeln!(
                        &mut out,
                        "ir_to_pc({}, {}, {}, {}).",
                        entry.func.as_u32(),
                        ir_inst.0,
                        entry.pc_start,
                        entry.pc_end,
                    )
                    .unwrap();

                    if let Some(prov) = provenance.get(&(entry.func, ir_inst)) {
                        if let Some((file, start_line, start_col, end_line, end_col)) =
                            parse_provenance(prov)
                        {
                            writeln!(
                                &mut out,
                                "source_at({}, {}, \"{}\", {}, {}, {}, {}).",
                                entry.func.as_u32(),
                                ir_inst.0,
                                file,
                                start_line,
                                start_col,
                                end_line,
                                end_col,
                            )
                            .unwrap();

                            writeln!(
                                &mut out,
                                "pc_source({}, {}, \"{}\", {}, {}).",
                                entry.pc_start,
                                entry.pc_end,
                                file,
                                start_line,
                                start_col,
                            )
                            .unwrap();
                        }
                    }
                }

                if let Some(reason) = entry.unmapped_reason {
                    writeln!(
                        &mut out,
                        "unmapped({}, {}, \"{}\").",
                        entry.pc_start,
                        entry.pc_end,
                        reason.as_str(),
                    )
                    .unwrap();
                }
            }
        }
    }

    DatalogFacts { facts: out }
}

fn parse_provenance(prov: &str) -> Option<(&str, u32, u32, u32, u32)> {
    let (file_and_start, end_part) = prov.rsplit_once('-')?;
    let (file_and_line, start_col_str) = file_and_start.rsplit_once(':')?;
    let (file_path, start_line_str) = file_and_line.rsplit_once(':')?;
    let (end_line_str, end_col_str) = end_part.rsplit_once(':')?;

    Some((
        file_path,
        start_line_str.parse().ok()?,
        start_col_str.parse().ok()?,
        end_line_str.parse().ok()?,
        end_col_str.parse().ok()?,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_provenance_extracts_fields() {
        let (file, sl, sc, el, ec) = parse_provenance("src/main.fe:10:5-10:15").unwrap();
        assert_eq!(file, "src/main.fe");
        assert_eq!((sl, sc, el, ec), (10, 5, 10, 15));
    }

    #[test]
    fn empty_artifacts_produce_empty_facts() {
        let facts = generate_datalog_facts(&[], &Default::default());
        assert!(facts.facts.is_empty());
    }

    #[test]
    fn full_facts_include_cross_level_mappings() {
        let mir_entries = vec![MirToIrEntry {
            func: sonatina_ir::module::FuncRef::from_u32(1),
            inst: sonatina_ir::InstId(5),
            mir_block: 0,
            mir_stmt: 2,
            source_ord: common::source_ord::SourceOrd::new(0),
        }];

        let mut table = FunctionSourceTable::new();
        table.push("test.fe".to_string(), 10, 5, 10, 20);

        let facts = generate_datalog_facts_full(
            &[],
            &Default::default(),
            &mir_entries,
            &[(1, &table)],
        );

        assert!(
            facts.facts.contains("mir_to_ir(1, 5, 0, 2)."),
            "should have mir_to_ir fact"
        );
        assert!(
            facts.facts.contains("ir_source_ord(1, 5, 0)."),
            "should have ir_source_ord fact"
        );
        assert!(
            facts.facts.contains("mir_source(1, 0, \"test.fe\""),
            "should have mir_source fact"
        );
    }
}
