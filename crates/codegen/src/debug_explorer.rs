use std::collections::BTreeMap;
use std::fmt::Write as _;

use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact};

use crate::sonatina::MirToIrEntry;
use common::source_ord::FunctionSourceTable;

/// An occurrence of a symbol at a specific location in a representation.
struct Occurrence {
    line: u32,
    col: u32,
    end_col: u32,
    symbol: String,
}

/// A "file" (representation) in the multi-representation SCIP index.
struct RepFile {
    contents: String,
    occurrences: Vec<Occurrence>,
}

/// Generate a multi-representation SCIP index as JSON.
///
/// Each representation (source, MIR, Sonatina IR, bytecode, etc.) is a
/// virtual "file" with SCIP-style symbol occurrences. Shared symbol
/// strings across files enable cross-representation hover highlighting.
pub fn generate_multi_rep_scip(
    source_code: &str,
    source_path: &str,
    mir_dump: &str,
    sonatina_ir_dump: &str,
    sonatina_ir_opt_dump: &str,
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
    mir_to_ir: &[MirToIrEntry],
    source_tables: &[(u32, &FunctionSourceTable)],
    func_names: &[(u32, &str)],
) -> String {
    let mut files: BTreeMap<&str, RepFile> = BTreeMap::new();

    // Source file — annotate lines with function-level and expression-level symbols
    let mut source_occs = Vec::new();
    for (func_id, func_name) in func_names {
        // Find source lines for this function from source tables
        if let Some((_, table)) = source_tables.iter().find(|(fid, _)| fid == func_id) {
            for (ord, entry) in table.iter() {
                let sym = format!("{}$ord:{}", func_name, ord.index());
                source_occs.push(Occurrence {
                    line: entry.start_line,
                    col: entry.start_col,
                    end_col: entry.end_col,
                    symbol: sym,
                });
            }
        }
    }
    files.insert("source", RepFile {
        contents: source_code.to_string(),
        occurrences: source_occs,
    });

    // MIR dump — for now just include the text; annotation requires
    // correlating MIR pretty-print positions with stmt indices
    files.insert("mir", RepFile {
        contents: mir_dump.to_string(),
        occurrences: Vec::new(),
    });

    // Sonatina IR (pre-opt)
    files.insert("sonatina_ir", RepFile {
        contents: sonatina_ir_dump.to_string(),
        occurrences: Vec::new(),
    });

    // Sonatina IR (optimized)
    if !sonatina_ir_opt_dump.is_empty() {
        files.insert("sonatina_ir_opt", RepFile {
            contents: sonatina_ir_opt_dump.to_string(),
            occurrences: Vec::new(),
        });
    }

    // Bytecode — annotate PC ranges with symbols from provenance
    let mut bytecode_lines = String::new();
    let mut bytecode_occs = Vec::new();
    let mut line_num = 1u32;

    for artifact in artifacts {
        for (section_name, section) in &artifact.sections {
            writeln!(&mut bytecode_lines, "--- {} ---", section_name.0).unwrap();
            line_num += 1;

            let Some(obs) = &section.observability else { continue };
            let mut entries: Vec<_> = obs.pc_map.iter().collect();
            entries.sort_by_key(|e| e.pc_start);

            for entry in entries {
                let prov_str = entry.ir_inst
                    .and_then(|ir_inst| provenance.get(&(entry.func, ir_inst)));

                let line_text = format!(
                    "[{:04x}..{:04x}) {}",
                    entry.pc_start, entry.pc_end, entry.func_name
                );
                writeln!(&mut bytecode_lines, "{}", line_text).unwrap();

                // Find matching SourceOrd for this IR inst to get the symbol
                if let Some(ir_inst) = entry.ir_inst {
                    let func_id = entry.func.as_u32();
                    if let Some(mir_entry) = mir_to_ir.iter().find(|m| {
                        m.func.as_u32() == func_id && m.inst.0 == ir_inst.0
                    }) {
                        if !mir_entry.source_ord.is_default() {
                            let func_name = func_names
                                .iter()
                                .find(|(fid, _)| *fid == func_id)
                                .map(|(_, name)| *name)
                                .unwrap_or("?");
                            let sym = format!(
                                "{}$ord:{}",
                                func_name,
                                mir_entry.source_ord.index()
                            );
                            bytecode_occs.push(Occurrence {
                                line: line_num,
                                col: 1,
                                end_col: (line_text.len() + 1) as u32,
                                symbol: sym,
                            });
                        }
                    }
                }
                line_num += 1;
            }
        }
    }

    files.insert("bytecode", RepFile {
        contents: bytecode_lines,
        occurrences: bytecode_occs,
    });

    // Serialize as JSON
    serialize_scip_index(&files)
}

fn serialize_scip_index(files: &BTreeMap<&str, RepFile>) -> String {
    let mut out = String::new();
    out.push_str("{\"files\":{");
    for (idx, (name, file)) in files.iter().enumerate() {
        if idx > 0 { out.push(','); }
        write!(&mut out, "\"{}\":{{", json_escape(name)).unwrap();
        write!(&mut out, "\"contents\":{}", json_string(&file.contents)).unwrap();
        write!(&mut out, ",\"occurrences\":[").unwrap();
        for (oidx, occ) in file.occurrences.iter().enumerate() {
            if oidx > 0 { out.push(','); }
            write!(
                &mut out,
                "{{\"line\":{},\"col\":{},\"endCol\":{},\"symbol\":\"{}\"}}",
                occ.line, occ.col, occ.end_col, json_escape(&occ.symbol)
            ).unwrap();
        }
        out.push_str("]}");
    }
    out.push_str("}}");
    out
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c < '\x20' => write!(out, "\\u{:04x}", c as u32).unwrap(),
            c => out.push(c),
        }
    }
    out
}

fn json_string(s: &str) -> String {
    format!("\"{}\"", json_escape(s))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn multi_rep_scip_generates_valid_json() {
        let json = generate_multi_rep_scip(
            "fn add(x: u256) -> u256 { return x }",
            "test.fe",
            "bb0: assign v0 = use v1",
            "func %add: block0: v0 = add v1 v2",
            "",
            &[],
            &Default::default(),
            &[],
            &[],
            &[],
        );
        assert!(json.starts_with("{\"files\":{"));
        assert!(json.contains("\"source\""));
        assert!(json.contains("\"mir\""));
        assert!(json.contains("\"bytecode\""));
        assert!(json.ends_with("}}"));
    }
}
