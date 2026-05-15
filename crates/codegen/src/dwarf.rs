use std::collections::BTreeMap;

use gimli::write::{
    Address, AttributeValue, DwarfUnit, EndianVec, LineProgram, LineString,
    Sections,
};
use gimli::{
    DW_AT_byte_size, DW_AT_comp_dir, DW_AT_encoding, DW_AT_language, DW_AT_name,
    DW_AT_stmt_list, DW_ATE_boolean, DW_ATE_signed, DW_ATE_unsigned, DW_LANG_Rust,
    DW_TAG_base_type, DW_TAG_member, DW_TAG_structure_type, Encoding, Format, LineEncoding,
    RunTimeEndian,
};
use sonatina_codegen::object::{ObjectArtifact, PcMapEntry};

pub struct DwarfDebugInfo {
    pub debug_info: Vec<u8>,
    pub debug_abbrev: Vec<u8>,
    pub debug_line: Vec<u8>,
    pub debug_str: Vec<u8>,
    pub debug_ranges: Vec<u8>,
    pub debug_rnglists: Vec<u8>,
}

impl DwarfDebugInfo {
    /// Write DWARF sections into an ELF object file.
    /// The ELF contains only debug sections (no code) and can be read
    /// by llvm-dwarfdump, readelf, or other DWARF consumers.
    pub fn to_elf(&self) -> Vec<u8> {
        use object::write::Object;
        use object::{Architecture, BinaryFormat, Endianness};

        // Use x86_64 as the nominal architecture for the ELF container.
        // The ELF contains only DWARF debug sections, no executable code.
        let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

        let sections: &[(&[u8], &str)] = &[
            (&self.debug_info, ".debug_info"),
            (&self.debug_abbrev, ".debug_abbrev"),
            (&self.debug_line, ".debug_line"),
            (&self.debug_str, ".debug_str"),
            (&self.debug_ranges, ".debug_ranges"),
            (&self.debug_rnglists, ".debug_rnglists"),
        ];

        for (data, name) in sections {
            if data.is_empty() {
                continue;
            }
            let section_id = obj.add_section(
                Vec::new(),
                name.as_bytes().to_vec(),
                object::SectionKind::Debug,
            );
            obj.set_section_data(section_id, data.to_vec(), 1);
        }

        obj.write().expect("ELF generation should not fail for debug-only object")
    }
}

pub fn generate_dwarf(
    artifacts: &[ObjectArtifact],
    provenance: &sonatina_codegen::object::FrontendProvenanceMap,
) -> Option<DwarfDebugInfo> {
    let pc_entries = collect_provenance_entries(artifacts, provenance);
    generate_dwarf_from_entries(&pc_entries)
}

pub fn generate_dwarf_from_entries(pc_entries: &[ResolvedProvenanceEntry]) -> Option<DwarfDebugInfo> {
    if pc_entries.is_empty() {
        return None;
    }

    let encoding = Encoding {
        format: Format::Dwarf32,
        version: 5,
        address_size: 4, // EVM uses 32-bit PCs
    };

    let line_encoding = LineEncoding::default();

    let mut dwarf = DwarfUnit::new(encoding);

    let comp_dir = LineString::String(b"/".to_vec());
    let comp_name = LineString::String(b"<fe-compilation>".to_vec());

    let mut line_program = LineProgram::new(
        encoding,
        line_encoding,
        comp_dir.clone(),
        comp_name.clone(),
        None,
    );

    let mut file_ids: BTreeMap<String, gimli::write::FileId> = BTreeMap::new();

    for entry in pc_entries {
        if !file_ids.contains_key(&entry.file_path) {
            let dir_id = line_program.default_directory();
            let file_id = line_program.add_file(
                LineString::String(entry.file_path.as_bytes().to_vec()),
                dir_id,
                None,
            );
            file_ids.insert(entry.file_path.clone(), file_id);
        }
    }

    let mut sorted_entries = pc_entries.to_vec();
    sorted_entries.sort_by_key(|e| e.pc_start);

    for entry in &sorted_entries {
        let file_id = file_ids[&entry.file_path];
        line_program.begin_sequence(Some(Address::Constant(entry.pc_start as u64)));
        line_program.row().file = file_id;
        line_program.row().line = entry.start_line as u64;
        line_program.row().column = entry.start_col as u64;
        line_program.row().address_offset = 0;
        line_program.generate_row();

        line_program.end_sequence((entry.pc_end - entry.pc_start) as u64);
    }

    dwarf.unit.line_program = line_program;

    let root = dwarf.unit.root();
    let name_id = dwarf.strings.add(b"<fe-compilation>");
    let dir_id = dwarf.strings.add(b"/");
    dwarf.unit.get_mut(root).set(DW_AT_name, AttributeValue::StringRef(name_id));
    dwarf.unit.get_mut(root).set(DW_AT_comp_dir, AttributeValue::StringRef(dir_id));
    dwarf.unit.get_mut(root).set(DW_AT_language, AttributeValue::Language(DW_LANG_Rust));
    dwarf.unit.get_mut(root).set(DW_AT_stmt_list, AttributeValue::LineProgramRef);

    let mut sections = Sections::new(EndianVec::new(RunTimeEndian::Little));
    if let Err(_) = dwarf.write(&mut sections) {
        return None;
    }

    Some(DwarfDebugInfo {
        debug_info: sections.debug_info.0.into_vec(),
        debug_abbrev: sections.debug_abbrev.0.into_vec(),
        debug_line: sections.debug_line.0.into_vec(),
        debug_str: sections.debug_str.0.into_vec(),
        debug_ranges: sections.debug_ranges.0.into_vec(),
        debug_rnglists: sections.debug_rnglists.0.into_vec(),
    })
}

#[derive(Clone, Debug)]
pub enum FeTypeDesc {
    UInt { bits: u32 },
    Int { bits: u32 },
    Bool,
    Address,
    Bytes { len: Option<u32> },
    String,
    Struct { name: String, fields: Vec<(String, FeTypeDesc)> },
    Enum { name: String, variants: Vec<String> },
    Array { elem: Box<FeTypeDesc>, len: Option<u64> },
}

pub fn add_type_dies(dwarf: &mut DwarfUnit, types: &[FeTypeDesc]) {
    let root = dwarf.unit.root();
    for ty in types {
        add_type_die(dwarf, root, ty);
    }
}

fn add_type_die(
    dwarf: &mut DwarfUnit,
    parent: gimli::write::UnitEntryId,
    ty: &FeTypeDesc,
) -> gimli::write::UnitEntryId {
    match ty {
        FeTypeDesc::UInt { bits } => {
            let id = dwarf.unit.add(parent, DW_TAG_base_type);
            let name = dwarf.strings.add(format!("u{bits}").as_bytes());
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name));
            dwarf.unit.get_mut(id).set(DW_AT_byte_size, AttributeValue::Data1((*bits / 8) as u8));
            dwarf.unit.get_mut(id).set(DW_AT_encoding, AttributeValue::Encoding(DW_ATE_unsigned));
            id
        }
        FeTypeDesc::Int { bits } => {
            let id = dwarf.unit.add(parent, DW_TAG_base_type);
            let name = dwarf.strings.add(format!("i{bits}").as_bytes());
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name));
            dwarf.unit.get_mut(id).set(DW_AT_byte_size, AttributeValue::Data1((*bits / 8) as u8));
            dwarf.unit.get_mut(id).set(DW_AT_encoding, AttributeValue::Encoding(DW_ATE_signed));
            id
        }
        FeTypeDesc::Bool => {
            let id = dwarf.unit.add(parent, DW_TAG_base_type);
            let name = dwarf.strings.add(b"bool");
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name));
            dwarf.unit.get_mut(id).set(DW_AT_byte_size, AttributeValue::Data1(1));
            dwarf.unit.get_mut(id).set(DW_AT_encoding, AttributeValue::Encoding(DW_ATE_boolean));
            id
        }
        FeTypeDesc::Address => {
            let id = dwarf.unit.add(parent, DW_TAG_base_type);
            let name = dwarf.strings.add(b"address");
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name));
            dwarf.unit.get_mut(id).set(DW_AT_byte_size, AttributeValue::Data1(20));
            dwarf.unit.get_mut(id).set(DW_AT_encoding, AttributeValue::Encoding(DW_ATE_unsigned));
            id
        }
        FeTypeDesc::Struct { name, fields } => {
            let id = dwarf.unit.add(parent, DW_TAG_structure_type);
            let name_id = dwarf.strings.add(name.as_bytes());
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name_id));
            for (field_name, field_ty) in fields {
                let field_id = dwarf.unit.add(id, DW_TAG_member);
                let fname_id = dwarf.strings.add(field_name.as_bytes());
                dwarf.unit.get_mut(field_id).set(DW_AT_name, AttributeValue::StringRef(fname_id));
                add_type_die(dwarf, id, field_ty);
            }
            id
        }
        FeTypeDesc::Bytes { .. }
        | FeTypeDesc::String
        | FeTypeDesc::Enum { .. }
        | FeTypeDesc::Array { .. } => {
            let id = dwarf.unit.add(parent, DW_TAG_base_type);
            let name = dwarf.strings.add(format!("{ty:?}").as_bytes());
            dwarf.unit.get_mut(id).set(DW_AT_name, AttributeValue::StringRef(name));
            id
        }
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedProvenanceEntry {
    pub pc_start: u32,
    pub pc_end: u32,
    pub file_path: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

pub fn fe_ty_to_type_desc<'db>(
    db: &'db dyn hir::analysis::HirAnalysisDb,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> FeTypeDesc {
    use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData};

    match ty.data(db) {
        TyData::TyBase(base) => match base {
            TyBase::Prim(prim) => match prim {
                PrimTy::Bool => FeTypeDesc::Bool,
                PrimTy::U8 => FeTypeDesc::UInt { bits: 8 },
                PrimTy::U16 => FeTypeDesc::UInt { bits: 16 },
                PrimTy::U32 => FeTypeDesc::UInt { bits: 32 },
                PrimTy::U64 => FeTypeDesc::UInt { bits: 64 },
                PrimTy::U128 => FeTypeDesc::UInt { bits: 128 },
                PrimTy::U256 | PrimTy::Usize => FeTypeDesc::UInt { bits: 256 },
                PrimTy::I8 => FeTypeDesc::Int { bits: 8 },
                PrimTy::I16 => FeTypeDesc::Int { bits: 16 },
                PrimTy::I32 => FeTypeDesc::Int { bits: 32 },
                PrimTy::I64 => FeTypeDesc::Int { bits: 64 },
                PrimTy::I128 => FeTypeDesc::Int { bits: 128 },
                PrimTy::I256 | PrimTy::Isize => FeTypeDesc::Int { bits: 256 },
                PrimTy::String => FeTypeDesc::String,
                PrimTy::Array => FeTypeDesc::Array {
                    elem: Box::new(FeTypeDesc::UInt { bits: 8 }),
                    len: None,
                },
                PrimTy::Tuple(n) => FeTypeDesc::Struct {
                    name: format!("Tuple{n}"),
                    fields: (0..*n)
                        .map(|i| (format!("{i}"), FeTypeDesc::UInt { bits: 256 }))
                        .collect(),
                },
                PrimTy::Ptr | PrimTy::View | PrimTy::BorrowMut | PrimTy::BorrowRef => {
                    FeTypeDesc::UInt { bits: 256 }
                }
            },
            TyBase::Adt(adt_def) => {
                use hir::analysis::ty::adt_def::AdtRef;
                let name = adt_def
                    .adt_ref(db)
                    .name(db)
                    .map(|n| n.data(db).clone())
                    .unwrap_or_else(|| "<anon>".to_string());
                match adt_def.adt_ref(db) {
                    AdtRef::Struct(_) => {
                        let fields = adt_def
                            .fields(db)
                            .iter()
                            .enumerate()
                            .flat_map(|(group_idx, field)| {
                                (0..field.num_types())
                                    .map(move |i| {
                                        let fname = format!("field_{group_idx}_{i}");
                                        let fty = fe_ty_to_type_desc(
                                            db,
                                            *field.ty(db, i).skip_binder(),
                                        );
                                        (fname, fty)
                                    })
                            })
                            .collect();
                        FeTypeDesc::Struct { name, fields }
                    }
                    AdtRef::Enum(_) => {
                        let variants = adt_def
                            .fields(db)
                            .iter()
                            .enumerate()
                            .map(|(i, _)| format!("variant_{i}"))
                            .collect();
                        FeTypeDesc::Enum { name, variants }
                    }
                }
            }
            TyBase::Contract(_) => FeTypeDesc::Address,
            TyBase::Func(_) => FeTypeDesc::UInt { bits: 256 },
        },
        _ => FeTypeDesc::UInt { bits: 256 },
    }
}

fn collect_provenance_entries(
    artifacts: &[ObjectArtifact],
    provenance: &sonatina_codegen::object::FrontendProvenanceMap,
) -> Vec<ResolvedProvenanceEntry> {
    let mut entries = Vec::new();

    for artifact in artifacts {
        for section in artifact.sections.values() {
            let Some(observability) = &section.observability else {
                continue;
            };
            for pc_entry in &observability.pc_map {
                let Some(ir_inst) = pc_entry.ir_inst else {
                    continue;
                };
                let Some(prov_str) = provenance.get(&(pc_entry.func, ir_inst)) else {
                    continue;
                };
                if let Some(resolved) = parse_provenance_string(prov_str, pc_entry) {
                    entries.push(resolved);
                }
            }
        }
    }

    entries
}

fn parse_provenance_string(
    prov: &str,
    pc_entry: &PcMapEntry,
) -> Option<ResolvedProvenanceEntry> {
    // Format: "file.fe:start_line:start_col-end_line:end_col"
    let (file_and_start, end_part) = prov.rsplit_once('-')?;
    let (file_and_line, start_col_str) = file_and_start.rsplit_once(':')?;
    let (file_path, start_line_str) = file_and_line.rsplit_once(':')?;
    let (end_line_str, end_col_str) = end_part.rsplit_once(':')?;

    Some(ResolvedProvenanceEntry {
        pc_start: pc_entry.pc_start,
        pc_end: pc_entry.pc_end,
        file_path: file_path.to_string(),
        start_line: start_line_str.parse().ok()?,
        start_col: start_col_str.parse().ok()?,
        end_line: end_line_str.parse().ok()?,
        end_col: end_col_str.parse().ok()?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_provenance_string_roundtrip() {
        let prov = "src/main.fe:10:5-10:15";
        let pc_entry = PcMapEntry {
            pc_start: 0,
            pc_end: 4,
            func: sonatina_ir::module::FuncRef::from_u32(0),
            func_name: "test".to_string(),
            block: sonatina_ir::BlockId(0),
            vcode_inst: sonatina_codegen::machinst::vcode::VCodeInst::from_u32(0),
            ir_inst: Some(sonatina_ir::InstId(0)),
            frontend_provenance: None,
            unmapped_reason: None,
        };
        let resolved = parse_provenance_string(prov, &pc_entry).unwrap();
        assert_eq!(resolved.file_path, "src/main.fe");
        assert_eq!(resolved.start_line, 10);
        assert_eq!(resolved.start_col, 5);
        assert_eq!(resolved.end_line, 10);
        assert_eq!(resolved.end_col, 15);
        assert_eq!(resolved.pc_start, 0);
        assert_eq!(resolved.pc_end, 4);
    }

    #[test]
    fn dwarf_to_elf_produces_valid_elf_header() {
        let entries = vec![ResolvedProvenanceEntry {
            pc_start: 0,
            pc_end: 10,
            file_path: "test.fe".to_string(),
            start_line: 1,
            start_col: 1,
            end_line: 1,
            end_col: 10,
        }];
        let dwarf = generate_dwarf_from_entries(&entries).unwrap();
        let elf = dwarf.to_elf();
        assert!(elf.len() > 52, "ELF should be larger than header");
        assert_eq!(&elf[..4], b"\x7fELF", "should start with ELF magic");
    }

    #[test]
    fn generate_dwarf_produces_valid_sections() {
        let entries = vec![
            ResolvedProvenanceEntry {
                pc_start: 0,
                pc_end: 10,
                file_path: "src/main.fe".to_string(),
                start_line: 3,
                start_col: 5,
                end_line: 3,
                end_col: 19,
            },
            ResolvedProvenanceEntry {
                pc_start: 10,
                pc_end: 25,
                file_path: "src/main.fe".to_string(),
                start_line: 4,
                start_col: 5,
                end_line: 4,
                end_col: 20,
            },
            ResolvedProvenanceEntry {
                pc_start: 25,
                pc_end: 40,
                file_path: "src/lib.fe".to_string(),
                start_line: 10,
                start_col: 1,
                end_line: 12,
                end_col: 2,
            },
        ];

        let dwarf = generate_dwarf_from_entries(&entries);
        assert!(dwarf.is_some(), "DWARF generation should produce output");

        let dwarf = dwarf.unwrap();
        assert!(
            !dwarf.debug_line.is_empty(),
            "debug_line section should be non-empty"
        );
        assert!(
            !dwarf.debug_info.is_empty(),
            "debug_info section should be non-empty"
        );
        assert!(
            !dwarf.debug_abbrev.is_empty(),
            "debug_abbrev section should be non-empty"
        );
        assert!(
            dwarf.debug_line.len() > 20,
            "debug_line should contain meaningful data, got {} bytes",
            dwarf.debug_line.len()
        );
    }
}
