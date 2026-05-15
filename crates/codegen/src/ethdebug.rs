use std::collections::BTreeMap;
use std::fmt::Write as _;

use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact, PcMapEntry};

#[derive(Debug, Clone)]
pub struct EthdebugInfo {
    pub json: String,
}

#[derive(Debug, Clone)]
pub struct EthdebugSource {
    pub id: usize,
    pub path: String,
    pub contents: Option<String>,
}

pub struct EthdebugBuilder {
    compiler_name: String,
    compiler_version: String,
    sources: Vec<EthdebugSource>,
    programs: Vec<EthdebugProgram>,
    types: Vec<EthdebugType>,
}

struct EthdebugProgram {
    contract_name: String,
    environment: &'static str,
    instructions: Vec<EthdebugInstruction>,
}

struct EthdebugInstruction {
    offset: u32,
    opcode: Option<u8>,
    source: Option<EthdebugSourceRange>,
}

#[derive(Clone)]
struct EthdebugSourceRange {
    source_id: usize,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
}

pub struct EthdebugType {
    pub name: String,
    pub kind: String,
    pub bits: Option<u32>,
    pub fields: Vec<EthdebugTypeField>,
}

pub struct EthdebugTypeField {
    pub name: String,
    pub type_kind: String,
    pub type_bits: Option<u32>,
}

impl EthdebugBuilder {
    pub fn new(compiler_name: &str, compiler_version: &str) -> Self {
        Self {
            compiler_name: compiler_name.to_string(),
            compiler_version: compiler_version.to_string(),
            sources: Vec::new(),
            programs: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn add_type_from_desc(&mut self, desc: &crate::dwarf::FeTypeDesc) {
        use crate::dwarf::FeTypeDesc;
        let (name, kind, bits, fields) = match desc {
            FeTypeDesc::UInt { bits } => (format!("u{bits}"), "uint".to_string(), Some(*bits), vec![]),
            FeTypeDesc::Int { bits } => (format!("i{bits}"), "int".to_string(), Some(*bits), vec![]),
            FeTypeDesc::Bool => ("bool".to_string(), "bool".to_string(), None, vec![]),
            FeTypeDesc::Address => ("address".to_string(), "address".to_string(), None, vec![]),
            FeTypeDesc::String => ("String".to_string(), "string".to_string(), None, vec![]),
            FeTypeDesc::Bytes { len } => (
                len.map_or("bytes".to_string(), |n| format!("bytes{n}")),
                "bytes".to_string(),
                len.map(|n| n * 8),
                vec![],
            ),
            FeTypeDesc::Struct { name, fields } => (
                name.clone(),
                "struct".to_string(),
                None,
                fields.iter().map(|(fname, fty)| {
                    let (fkind, fbits) = match fty {
                        FeTypeDesc::UInt { bits } => ("uint".to_string(), Some(*bits)),
                        FeTypeDesc::Int { bits } => ("int".to_string(), Some(*bits)),
                        FeTypeDesc::Bool => ("bool".to_string(), None),
                        _ => ("unknown".to_string(), None),
                    };
                    EthdebugTypeField { name: fname.clone(), type_kind: fkind, type_bits: fbits }
                }).collect(),
            ),
            FeTypeDesc::Enum { name, variants } => (
                name.clone(),
                "enum".to_string(),
                None,
                variants.iter().map(|v| EthdebugTypeField {
                    name: v.clone(), type_kind: "uint".to_string(), type_bits: Some(8),
                }).collect(),
            ),
            FeTypeDesc::Array { elem, len } => (
                len.map_or("Array".to_string(), |n| format!("Array[{n}]")),
                "array".to_string(),
                None,
                vec![],
            ),
        };
        self.types.push(EthdebugType { name, kind, bits, fields });
    }

    pub fn add_source(&mut self, path: &str, contents: Option<&str>) -> usize {
        let id = self.sources.len();
        self.sources.push(EthdebugSource {
            id,
            path: path.to_string(),
            contents: contents.map(|s| s.to_string()),
        });
        id
    }

    pub fn add_program_from_artifact(
        &mut self,
        artifact: &ObjectArtifact,
        provenance: &FrontendProvenanceMap,
        contract_name: &str,
        environment: &'static str,
    ) {
        let mut instructions = Vec::new();

        for section in artifact.sections.values() {
            let Some(observability) = &section.observability else {
                continue;
            };

            for entry in &observability.pc_map {
                let source = entry
                    .ir_inst
                    .and_then(|ir_inst| provenance.get(&(entry.func, ir_inst)))
                    .and_then(|prov_str| self.parse_provenance_to_source_range(prov_str));

                instructions.push(EthdebugInstruction {
                    offset: entry.pc_start,
                    opcode: None,
                    source,
                });
            }
        }

        instructions.sort_by_key(|i| i.offset);
        instructions.dedup_by_key(|i| i.offset);

        self.programs.push(EthdebugProgram {
            contract_name: contract_name.to_string(),
            environment,
            instructions,
        });
    }

    fn parse_provenance_to_source_range(&self, prov: &str) -> Option<EthdebugSourceRange> {
        let (file_and_start, end_part) = prov.rsplit_once('-')?;
        let (file_and_line, start_col_str) = file_and_start.rsplit_once(':')?;
        let (file_path, start_line_str) = file_and_line.rsplit_once(':')?;
        let (end_line_str, end_col_str) = end_part.rsplit_once(':')?;

        let source_id = self
            .sources
            .iter()
            .position(|s| s.path == file_path)
            .or_else(|| {
                if self.sources.is_empty() {
                    None
                } else {
                    Some(0)
                }
            })?;

        Some(EthdebugSourceRange {
            source_id,
            start_line: start_line_str.parse().ok()?,
            start_col: start_col_str.parse().ok()?,
            end_line: end_line_str.parse().ok()?,
            end_col: end_col_str.parse().ok()?,
        })
    }

    pub fn build(&self) -> EthdebugInfo {
        let mut out = String::new();
        self.write_json(&mut out);
        EthdebugInfo { json: out }
    }

    fn write_json(&self, out: &mut String) {
        out.push('{');

        // compilation
        write!(out, "\"compilation\":{{").unwrap();
        write!(
            out,
            "\"compiler\":{{\"name\":\"{}\",\"version\":\"{}\"}}",
            json_escape(&self.compiler_name),
            json_escape(&self.compiler_version)
        )
        .unwrap();
        write!(out, ",\"sources\":[").unwrap();
        for (idx, source) in self.sources.iter().enumerate() {
            if idx > 0 {
                out.push(',');
            }
            write!(
                out,
                "{{\"id\":{},\"path\":\"{}\"",
                source.id,
                json_escape(&source.path)
            )
            .unwrap();
            if let Some(contents) = &source.contents {
                write!(out, ",\"contents\":\"{}\"", json_escape(contents)).unwrap();
            }
            write!(out, ",\"language\":\"Fe\"}}").unwrap();
        }
        write!(out, "]}}").unwrap();

        // types
        if !self.types.is_empty() {
            write!(out, ",\"types\":{{").unwrap();
            for (idx, ty) in self.types.iter().enumerate() {
                if idx > 0 {
                    out.push(',');
                }
                write!(
                    out,
                    "\"{}\":{{\"kind\":\"{}\"",
                    json_escape(&ty.name),
                    json_escape(&ty.kind)
                )
                .unwrap();
                if let Some(bits) = ty.bits {
                    write!(out, ",\"bits\":{bits}").unwrap();
                }
                if !ty.fields.is_empty() {
                    write!(out, ",\"contains\":[").unwrap();
                    for (fidx, field) in ty.fields.iter().enumerate() {
                        if fidx > 0 {
                            out.push(',');
                        }
                        write!(
                            out,
                            "{{\"name\":\"{}\",\"type\":{{\"kind\":\"{}\"",
                            json_escape(&field.name),
                            json_escape(&field.type_kind)
                        )
                        .unwrap();
                        if let Some(bits) = field.type_bits {
                            write!(out, ",\"bits\":{bits}").unwrap();
                        }
                        write!(out, "}}}}").unwrap();
                    }
                    write!(out, "]").unwrap();
                }
                out.push('}');
            }
            write!(out, "}}").unwrap();
        }

        // programs
        write!(out, ",\"programs\":[").unwrap();
        for (prog_idx, program) in self.programs.iter().enumerate() {
            if prog_idx > 0 {
                out.push(',');
            }
            write!(out, "{{").unwrap();
            write!(
                out,
                "\"contract\":{{\"name\":\"{}\"}}",
                json_escape(&program.contract_name)
            )
            .unwrap();
            write!(
                out,
                ",\"environment\":\"{}\"",
                program.environment
            )
            .unwrap();

            write!(out, ",\"instructions\":[").unwrap();
            for (inst_idx, inst) in program.instructions.iter().enumerate() {
                if inst_idx > 0 {
                    out.push(',');
                }
                write!(out, "{{\"offset\":{}", inst.offset).unwrap();
                if let Some(opcode) = inst.opcode {
                    write!(out, ",\"operation\":{{\"mnemonic\":\"0x{:02x}\"}}", opcode).unwrap();
                }
                if let Some(source) = &inst.source {
                    write!(
                        out,
                        ",\"source\":{{\"id\":{},\"start\":{{\"line\":{},\"column\":{}}},\"end\":{{\"line\":{},\"column\":{}}}}}",
                        source.source_id,
                        source.start_line,
                        source.start_col,
                        source.end_line,
                        source.end_col,
                    )
                    .unwrap();
                }
                out.push('}');
            }
            write!(out, "]}}").unwrap();
        }
        write!(out, "]}}").unwrap();
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ethdebug_builder_produces_valid_json() {
        let mut builder = EthdebugBuilder::new("fe", "26.1.0");
        builder.add_source("src/main.fe", Some("fn add(x: u256, y: u256) -> u256 { return x + y }"));

        let info = builder.build();
        assert!(info.json.starts_with('{'));
        assert!(info.json.ends_with('}'));
        assert!(info.json.contains("\"compiler\""));
        assert!(info.json.contains("\"Fe\""));
        assert!(info.json.contains("src/main.fe"));
    }

    #[test]
    fn ethdebug_builder_with_instructions() {
        let mut builder = EthdebugBuilder::new("fe", "26.1.0");
        let source_id = builder.add_source("test.fe", None);

        builder.programs.push(EthdebugProgram {
            contract_name: "Counter".to_string(),
            environment: "call",
            instructions: vec![
                EthdebugInstruction {
                    offset: 0,
                    opcode: Some(0x60),
                    source: Some(EthdebugSourceRange {
                        source_id,
                        start_line: 5,
                        start_col: 1,
                        end_line: 5,
                        end_col: 20,
                    }),
                },
                EthdebugInstruction {
                    offset: 2,
                    opcode: Some(0x54),
                    source: None,
                },
            ],
        });

        let info = builder.build();
        assert!(info.json.contains("\"Counter\""));
        assert!(info.json.contains("\"call\""));
        assert!(info.json.contains("\"offset\":0"));
        assert!(info.json.contains("\"offset\":2"));
        assert!(info.json.contains("\"line\":5"));
        assert!(info.json.contains("\"mnemonic\":\"0x60\""));
    }
}
