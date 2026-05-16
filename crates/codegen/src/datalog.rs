use std::fmt::Write as _;

use common::source_ord::FunctionSourceTable;
use sonatina_codegen::object::{FrontendProvenanceMap, ObjectArtifact};
use sonatina_ir::Module;

use crate::sonatina::MirToIrEntry;

pub struct DatalogFacts {
    pub facts: String,
}

pub fn generate_datalog_facts(
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
) -> DatalogFacts {
    generate_datalog_facts_full(artifacts, provenance, &[], &[], None)
}

pub fn generate_datalog_facts_full(
    artifacts: &[ObjectArtifact],
    provenance: &FrontendProvenanceMap,
    mir_to_ir: &[MirToIrEntry],
    source_tables: &[(u32, &FunctionSourceTable)],
    module: Option<&Module>,
) -> DatalogFacts {
    let mut out = String::new();

    // MIR-level facts
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

    // Cross-level: mir_to_ir
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

    // IR-level facts: instruction semantics, control flow, data flow
    if let Some(module) = module {
        emit_ir_facts(&mut out, module);
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
                                entry.pc_start, entry.pc_end, file, start_line, start_col,
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

fn emit_ir_facts(out: &mut String, module: &Module) {
    for func_ref in module.funcs() {
        let func_id = func_ref.as_u32();

        module.func_store.view(func_ref, |func| {
            // Control flow edges
            let mut cfg = sonatina_ir::ControlFlowGraph::default();
            cfg.compute(func);

            for block in func.layout.iter_block() {
                let block_id = block.0;

                // Block entry/exit instructions
                if let Some(first) = func.layout.first_inst_of(block) {
                    writeln!(out, "block_entry({func_id}, {block_id}, {}).", first.0).unwrap();
                }
                if let Some(last) = func.layout.last_inst_of(block) {
                    writeln!(out, "block_exit({func_id}, {block_id}, {}).", last.0).unwrap();
                }

                // CFG edges
                for succ in cfg.succs_of(block) {
                    writeln!(out, "cfg_edge({func_id}, {block_id}, {}).", succ.0).unwrap();
                }

                // Instruction-level facts
                for inst in func.layout.iter_inst(block) {
                    let inst_id = inst.0;
                    let inst_data = &func.dfg.insts[inst];

                    // Instruction type
                    let type_name = classify_inst(inst_data.as_ref());
                    writeln!(out, "inst_type({func_id}, {inst_id}, \"{type_name}\").").unwrap();

                    // Instruction results
                    for (ridx, &result) in func.dfg.inst_results(inst).iter().enumerate() {
                        writeln!(
                            out,
                            "inst_result({func_id}, {inst_id}, {ridx}, {}).",
                            result.0
                        )
                        .unwrap();
                        writeln!(out, "value_def({func_id}, {}, {inst_id}).", result.0).unwrap();
                    }

                    // Instruction operands
                    let operands = inst_data.collect_values();
                    for (oidx, &operand) in operands.iter().enumerate() {
                        writeln!(
                            out,
                            "inst_operand({func_id}, {inst_id}, {oidx}, {}).",
                            operand.0
                        )
                        .unwrap();
                    }
                }
            }
        });
    }
}

fn classify_inst(inst: &dyn sonatina_ir::Inst) -> &'static str {
    let name = inst.as_text();
    // Map known instruction names to semantic categories
    match name {
        "call" => "call",
        "return" => "return",
        "jump" => "jump",
        "br" => "branch",
        "br_table" => "switch",
        "phi" => "phi",
        "add" => "add",
        "sub" => "sub",
        "mul" => "mul",
        "neg" => "neg",
        "shl" => "shl",
        "shr" => "shr",
        "sar" => "sar",
        "and" => "and",
        "or" => "or",
        "xor" => "xor",
        "not" => "not",
        "eq" => "eq",
        "ne" => "ne",
        "lt" => "lt",
        "gt" => "gt",
        "slt" => "slt",
        "is_zero" => "is_zero",
        "sext" => "sext",
        "zext" => "zext",
        "trunc" => "trunc",
        "bitcast" => "bitcast",
        "int_to_ptr" => "int_to_ptr",
        "ptr_to_int" => "ptr_to_int",
        "alloca" => "alloca",
        "mload" => "mload",
        "mstore" => "mstore",
        "evm_sload" => "sload",
        "evm_sstore" => "sstore",
        "evm_tload" => "tload",
        "evm_tstore" => "tstore",
        "evm_call" => "external_call",
        "evm_static_call" => "static_call",
        "evm_delegate_call" => "delegate_call",
        "evm_create" => "create",
        "evm_create2" => "create2",
        "evm_return" => "evm_return",
        "evm_revert" => "revert",
        "evm_stop" => "stop",
        "evm_invalid" => "invalid",
        "evm_self_destruct" => "selfdestruct",
        "evm_log0" => "log",
        "evm_log1" => "log",
        "evm_log2" => "log",
        "evm_log3" => "log",
        "evm_log4" => "log",
        "evm_keccak256" => "keccak",
        "evm_calldataload" => "calldata_read",
        "evm_calldatacopy" => "calldata_read",
        "evm_calldatasize" => "calldata_read",
        "evm_returndatacopy" => "returndata_read",
        "evm_returndatasize" => "returndata_read",
        "evm_codecopy" => "code_read",
        "evm_codesize" => "code_read",
        "evm_balance" | "evm_self_balance" => "balance",
        "evm_caller" | "evm_origin" | "evm_address" => "context",
        "evm_callvalue" | "evm_gas" | "evm_gasprice" => "context",
        "evm_number" | "evm_timestamp" | "evm_chainid" => "context",
        "evm_blockhash" | "evm_coinbase" | "evm_gaslimit" => "context",
        "evm_basefee" | "evm_prevrandao" => "context",
        "evm_msize" => "memory_info",
        "evm_exp" => "exp",
        "evm_signextend" => "signextend",
        "evm_addmod" | "evm_mulmod" => "modular_arith",
        "evm_udiv" | "evm_sdiv" | "evm_umod" | "evm_smod" => "division",
        "unreachable" => "unreachable",
        _ => name,
    }
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

        let facts =
            generate_datalog_facts_full(&[], &Default::default(), &mir_entries, &[(1, &table)], None);

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
