use std::fmt::Write as _;

use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact};

pub struct DatalogFacts {
    pub facts: String,
}

pub fn generate_datalog_facts(
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
) -> DatalogFacts {
    let mut out = String::new();

    for artifact in artifacts {
        let object_name = &artifact.object.0;
        for (section_name, section) in &artifact.sections {
            let Some(observability) = &section.observability else {
                continue;
            };

            for entry in &observability.pc_map {
                // bytecode_range(object, section, pc_start, pc_end, func, block)
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
                    // ir_inst_at(func, inst, pc_start, pc_end)
                    writeln!(
                        &mut out,
                        "ir_inst_at({}, {}, {}, {}).",
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
                            // source_at(func, inst, file, start_line, start_col, end_line, end_col)
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

                            // pc_source(pc_start, pc_end, file, line, col)
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
}
