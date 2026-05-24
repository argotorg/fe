use std::collections::BTreeMap;

use gimli::write::{
    Address, AttributeValue, DwarfUnit, EndianVec, LineProgram, LineString, Sections,
};
use gimli::{Encoding, Format, LineEncoding, LittleEndian};
use trace_facts::PcRange;

use crate::model::{
    AttributionConfidence, DebugBundle, DebugSourceFile, InstructionClassification,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DwarfLineTable {
    pub debug_abbrev: Vec<u8>,
    pub debug_info: Vec<u8>,
    pub debug_line: Vec<u8>,
    pub debug_line_str: Vec<u8>,
    pub debug_str: Vec<u8>,
    pub rows: Vec<DwarfLineRow>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DwarfLineRow {
    pub pc_range: PcRange,
    pub line: u32,
    pub column: u32,
}

pub fn emit_dwarf_line_table(bundle: &DebugBundle) -> Result<DwarfLineTable, String> {
    let (source, rows) = high_confidence_rows(bundle)?;
    let encoding = Encoding {
        format: Format::Dwarf32,
        version: 5,
        address_size: 8,
    };
    let mut dwarf = DwarfUnit::new(encoding);
    let comp_dir_ref = dwarf.strings.add(".");
    let file_ref = dwarf.strings.add(source.display_name.as_str());
    let mut program = LineProgram::new(
        encoding,
        LineEncoding::default(),
        LineString::StringRef(comp_dir_ref),
        None,
        LineString::StringRef(file_ref),
        None,
    );
    let file_id = program.add_file(
        LineString::StringRef(file_ref),
        program.default_directory(),
        None,
    );

    let end_pc = rows
        .iter()
        .map(|row| row.pc_range.end)
        .max()
        .ok_or_else(|| "no high-confidence source-mapped instructions for DWARF".to_string())?;
    program.begin_sequence(Some(Address::Constant(0)));
    for row in &rows {
        let line_row = program.row();
        line_row.address_offset = u64::from(row.pc_range.start);
        line_row.file = file_id;
        line_row.line = u64::from(row.line);
        line_row.column = u64::from(row.column);
        line_row.is_statement = true;
        program.generate_row();
    }
    program.end_sequence(u64::from(end_pc));
    dwarf.unit.line_program = program;

    let root = dwarf.unit.root();
    let unit = dwarf.unit.get_mut(root);
    unit.set(
        gimli::DW_AT_producer,
        AttributeValue::String(format!("fe {}", bundle.compiler.commit).into_bytes()),
    );
    unit.set(
        gimli::DW_AT_language,
        AttributeValue::Language(gimli::DW_LANG_C),
    );
    unit.set(gimli::DW_AT_name, AttributeValue::StringRef(file_ref));
    unit.set(
        gimli::DW_AT_comp_dir,
        AttributeValue::StringRef(comp_dir_ref),
    );
    unit.set(
        gimli::DW_AT_low_pc,
        AttributeValue::Address(Address::Constant(0)),
    );
    unit.set(
        gimli::DW_AT_high_pc,
        AttributeValue::Udata(u64::from(end_pc)),
    );

    let mut sections = Sections::new(EndianVec::new(LittleEndian));
    dwarf
        .write(&mut sections)
        .map_err(|err| format!("failed to write DWARF line table: {err}"))?;

    Ok(DwarfLineTable {
        debug_abbrev: sections.debug_abbrev.0.take(),
        debug_info: sections.debug_info.0.take(),
        debug_line: sections.debug_line.0.take(),
        debug_line_str: sections.debug_line_str.0.take(),
        debug_str: sections.debug_str.0.take(),
        rows,
    })
}

fn high_confidence_rows(
    bundle: &DebugBundle,
) -> Result<(&DebugSourceFile, Vec<DwarfLineRow>), String> {
    let spans = bundle
        .source_spans
        .iter()
        .map(|span| (span.origin.clone(), span))
        .collect::<BTreeMap<_, _>>();

    let mut candidates = Vec::new();
    for instruction in &bundle.instructions {
        if instruction.classification != InstructionClassification::SourceMapped
            || instruction.confidence != AttributionConfidence::High
        {
            continue;
        }
        let Some(primary_source) = &instruction.primary_source else {
            continue;
        };
        let Some(span) = spans.get(primary_source) else {
            continue;
        };
        if instruction.pc_range.start >= instruction.pc_range.end {
            continue;
        }
        candidates.push((span.file.clone(), instruction.pc_range, *span));
    }

    let Some((file_key, _, _)) = candidates.first() else {
        return Err("no high-confidence source-mapped instructions for DWARF".to_string());
    };
    let file_key = file_key.clone();
    let source = bundle
        .sources
        .iter()
        .find(|source| source.file_key == file_key)
        .ok_or_else(|| "primary source file is missing from DebugBundle".to_string())?;
    let mut rows = candidates
        .into_iter()
        .filter(|(candidate_file, _, _)| candidate_file == &file_key)
        .map(|(_, pc_range, span)| DwarfLineRow {
            pc_range,
            line: span.start_line,
            column: span.start_column,
        })
        .collect::<Vec<_>>();
    rows.sort_by_key(|row| (row.pc_range.start, row.pc_range.end));
    Ok((source, rows))
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::PcRange;

    use crate::model::{
        AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugBundle,
        DebugInstruction, DebugSourceFile, DebugSourceSpan, InstructionClassification,
    };

    use super::emit_dwarf_line_table;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn bundle_with_mixed_instruction_mappings() -> DebugBundle {
        let file = key("source.file", "demo", "src/main.fe");
        let expr = key("hir.expr", "demo", "expr:add");
        let function = key("function", "demo", "runtime");
        DebugBundle {
            trace_hash: "blake3:0000000000000000000000000000000000000000000000000000000000000001"
                .to_string(),
            compiler: CompilerInfo {
                commit: "abc123".to_string(),
                target: "evm/sonatina".to_string(),
                command: vec!["fe".to_string()],
                flags: vec![],
                input_path: "src/main.fe".to_string(),
                data_source: "compiler_emitted".to_string(),
            },
            sources: vec![DebugSourceFile {
                file_key: file.clone(),
                uri: "file:///src/main.fe".to_string(),
                display_name: "src/main.fe".to_string(),
                content_hash:
                    "blake3:0000000000000000000000000000000000000000000000000000000000001234"
                        .to_string(),
                source_id: Some(0),
            }],
            source_spans: vec![DebugSourceSpan {
                origin: expr.clone(),
                file,
                start_byte: 10,
                end_byte: 13,
                start_line: 7,
                start_column: 5,
                end_line: 7,
                end_column: 8,
            }],
            code_objects: vec![],
            functions: vec![],
            scopes: vec![],
            variables: vec![],
            types: vec![],
            instructions: vec![
                DebugInstruction {
                    key: key("bytecode.pc", "demo", "pc:4"),
                    function: function.clone(),
                    code_object: None,
                    pc_range: PcRange::new(4, 5),
                    opcode_or_mnemonic: "ADD".to_string(),
                    primary_source: Some(expr),
                    all_origins: vec![],
                    classification: InstructionClassification::SourceMapped,
                    classification_reason: None,
                    category: None,
                    confidence: AttributionConfidence::High,
                },
                DebugInstruction {
                    key: key("bytecode.pc", "demo", "pc:5"),
                    function: function.clone(),
                    code_object: None,
                    pc_range: PcRange::new(5, 6),
                    opcode_or_mnemonic: "PUSH0".to_string(),
                    primary_source: None,
                    all_origins: vec![],
                    classification: InstructionClassification::Synthetic,
                    classification_reason: Some("test".to_string()),
                    category: None,
                    confidence: AttributionConfidence::Unmapped,
                },
                DebugInstruction {
                    key: key("bytecode.pc", "demo", "pc:6"),
                    function,
                    code_object: None,
                    pc_range: PcRange::new(6, 7),
                    opcode_or_mnemonic: "STOP".to_string(),
                    primary_source: None,
                    all_origins: vec![],
                    classification: InstructionClassification::Unmapped,
                    classification_reason: None,
                    category: None,
                    confidence: AttributionConfidence::Unmapped,
                },
            ],
            locations: vec![],
            gas: vec![],
            attribution_policy: AttributionPolicyVersion::PrimarySourceV1,
        }
    }

    #[test]
    fn dwarf_line_table_contains_only_confident_source_mapped_rows() {
        let table = emit_dwarf_line_table(&bundle_with_mixed_instruction_mappings()).unwrap();

        assert_eq!(table.rows.len(), 1);
        assert_eq!(table.rows[0].pc_range, PcRange::new(4, 5));
        assert_eq!(table.rows[0].line, 7);
        assert_eq!(table.rows[0].column, 5);
        assert!(!table.debug_abbrev.is_empty());
        assert!(!table.debug_info.is_empty());
        assert!(!table.debug_line.is_empty());
    }

    #[test]
    fn dwarf_line_table_fails_closed_without_confident_source_rows() {
        let mut bundle = bundle_with_mixed_instruction_mappings();
        for instruction in &mut bundle.instructions {
            instruction.classification = InstructionClassification::Unmapped;
            instruction.confidence = AttributionConfidence::Unmapped;
            instruction.primary_source = None;
        }

        let err = emit_dwarf_line_table(&bundle).unwrap_err();

        assert!(err.contains("no high-confidence source-mapped instructions"));
    }
}
