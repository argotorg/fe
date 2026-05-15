use std::collections::BTreeMap;
use std::fmt::Write as _;

use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact};

use crate::sonatina::MirToIrEntry;
use common::source_ord::FunctionSourceTable;

pub fn generate_explorer_payload(
    source_code: &str,
    source_path: &str,
    mir_dump: &str,
    sonatina_ir_dump: &str,
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
    mir_to_ir: &[MirToIrEntry],
    source_tables: &[(u32, &FunctionSourceTable)],
) -> String {
    let mut groups: Vec<ExplorerGroup> = Vec::new();
    let mut group_id = 0u32;

    for entry in mir_to_ir {
        if entry.source_ord.is_default() {
            continue;
        }
        let func_id = entry.func.as_u32();

        let source_entry = source_tables
            .iter()
            .find(|(fid, _)| *fid == func_id)
            .and_then(|(_, table)| table.get(entry.source_ord));

        let source_lines: Vec<u32> = source_entry
            .map(|e| (e.start_line..=e.end_line).collect())
            .unwrap_or_default();

        let mut pc_ranges = Vec::new();
        for artifact in artifacts {
            for section in artifact.sections.values() {
                let Some(obs) = &section.observability else { continue };
                for pc_entry in &obs.pc_map {
                    if pc_entry.func.as_u32() == func_id
                        && pc_entry.ir_inst.map(|i| i.0) == Some(entry.inst.0)
                    {
                        pc_ranges.push(PcRange {
                            start: pc_entry.pc_start,
                            end: pc_entry.pc_end,
                            func_name: pc_entry.func_name.clone(),
                        });
                    }
                }
            }
        }

        groups.push(ExplorerGroup {
            id: group_id,
            source_lines,
            mir_stmts: vec![MirStmt {
                func: func_id,
                block: entry.mir_block,
                stmt: entry.mir_stmt,
            }],
            ir_insts: vec![IrInst {
                func: func_id,
                inst: entry.inst.0,
            }],
            pc_ranges,
        });
        group_id += 1;
    }

    let mut out = String::new();
    out.push('{');
    write!(
        out,
        "\"source\":{{\"path\":\"{}\",\"code\":{}}},",
        json_escape(source_path),
        json_string(source_code)
    )
    .unwrap();
    write!(out, "\"mir\":{},", json_string(mir_dump)).unwrap();
    write!(out, "\"sonatina_ir\":{},", json_string(sonatina_ir_dump)).unwrap();
    write!(out, "\"groups\":[").unwrap();
    for (idx, group) in groups.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        write_group(&mut out, group);
    }
    write!(out, "]}}").unwrap();
    out
}

struct ExplorerGroup {
    id: u32,
    source_lines: Vec<u32>,
    mir_stmts: Vec<MirStmt>,
    ir_insts: Vec<IrInst>,
    pc_ranges: Vec<PcRange>,
}

struct MirStmt {
    func: u32,
    block: u32,
    stmt: u32,
}

struct IrInst {
    func: u32,
    inst: u32,
}

struct PcRange {
    start: u32,
    end: u32,
    func_name: String,
}

fn write_group(out: &mut String, g: &ExplorerGroup) {
    write!(out, "{{\"id\":{}", g.id).unwrap();
    write!(out, ",\"source_lines\":[").unwrap();
    for (i, line) in g.source_lines.iter().enumerate() {
        if i > 0 { out.push(','); }
        write!(out, "{line}").unwrap();
    }
    out.push(']');
    write!(out, ",\"mir_stmts\":[").unwrap();
    for (i, s) in g.mir_stmts.iter().enumerate() {
        if i > 0 { out.push(','); }
        write!(out, "{{\"func\":{},\"block\":{},\"stmt\":{}}}", s.func, s.block, s.stmt).unwrap();
    }
    out.push(']');
    write!(out, ",\"ir_insts\":[").unwrap();
    for (i, inst) in g.ir_insts.iter().enumerate() {
        if i > 0 { out.push(','); }
        write!(out, "{{\"func\":{},\"inst\":{}}}", inst.func, inst.inst).unwrap();
    }
    out.push(']');
    write!(out, ",\"pc_ranges\":[").unwrap();
    for (i, pc) in g.pc_ranges.iter().enumerate() {
        if i > 0 { out.push(','); }
        write!(
            out,
            "{{\"start\":{},\"end\":{},\"func_name\":\"{}\"}}",
            pc.start, pc.end, json_escape(&pc.func_name)
        )
        .unwrap();
    }
    out.push(']');
    out.push('}');
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
    fn payload_is_valid_json_structure() {
        let payload = generate_explorer_payload(
            "fn add(x: u256) -> u256 { return x }",
            "test.fe",
            "bb0: assign v0 = use v1",
            "func %add: block0: v0 = add v1 v2",
            &[],
            &Default::default(),
            &[],
            &[],
        );
        assert!(payload.starts_with('{'));
        assert!(payload.ends_with('}'));
        assert!(payload.contains("\"source\""));
        assert!(payload.contains("\"groups\""));
    }
}
