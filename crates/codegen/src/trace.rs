use common::origin::OriginExportKey;
use trace_facts::{
    CategorySource, EvmSchedule, GasConfidence, GasCostFact, GasKind, GasSource,
    InstructionCategory, InstructionCategoryFact, InstructionFact, OpcodeCategory, OpcodeFact,
    OriginNodeFact, OriginNodeKind, TraceFact,
};

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

/// Emit codegen-owned instruction facts from actual emitted EVM bytecode.
pub fn emit_bytecode_instruction_facts(
    owner_key: &str,
    function_local_key: &str,
    bytecode: &[u8],
) -> Vec<TraceFact> {
    let function =
        OriginExportKey::try_from_raw_parts("bytecode.function", owner_key, function_local_key)
            .expect("codegen bytecode function key must be valid");
    let mut facts = vec![origin_node(function.clone(), "bytecode.function")];
    let mut pc = 0;
    let mut index = 0;
    while pc < bytecode.len() {
        let opcode = bytecode[pc];
        let instruction =
            OriginExportKey::try_from_raw_parts("bytecode.pc", owner_key, format!("pc:{pc}"))
                .expect("codegen bytecode PC key must be valid");
        let mnemonic = evm_mnemonic(opcode).to_string();
        let immediate_len = evm_push_immediate_len(opcode);
        let immediate = (immediate_len > 0).then(|| {
            let end = (pc + 1 + immediate_len).min(bytecode.len());
            format!("0x{}", hex::encode(&bytecode[pc + 1..end]))
        });
        facts.push(origin_node(instruction.clone(), "bytecode.pc"));
        facts.push(TraceFact::Instruction(InstructionFact::new(
            instruction.clone(),
            function.clone(),
            index,
            mnemonic.clone(),
        )));
        facts.push(TraceFact::InstructionCategory(
            InstructionCategoryFact::new(
                instruction.clone(),
                evm_instruction_category(opcode),
                CategorySource::BackendEmissionReason,
            ),
        ));
        facts.push(TraceFact::Opcode(OpcodeFact::new(
            instruction.clone(),
            mnemonic,
            immediate,
            evm_opcode_category(opcode),
        )));
        facts.push(TraceFact::GasCost(GasCostFact::new(
            instruction,
            GasKind::OpcodeStatic,
            evm_static_gas(opcode),
            EvmSchedule::new("cancun"),
            GasConfidence::ConservativeStatic,
            GasSource::OpcodeTable,
        )));
        pc += 1 + immediate_len;
        index += 1;
    }
    facts
}

fn origin_node(key: OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}

fn evm_push_immediate_len(opcode: u8) -> usize {
    if (0x60..=0x7f).contains(&opcode) {
        (opcode - 0x5f) as usize
    } else {
        0
    }
}

fn evm_mnemonic(opcode: u8) -> &'static str {
    match opcode {
        0x00 => "STOP",
        0x01 => "ADD",
        0x02 => "MUL",
        0x03 => "SUB",
        0x04 => "DIV",
        0x10 => "LT",
        0x11 => "GT",
        0x14 => "EQ",
        0x15 => "ISZERO",
        0x16 => "AND",
        0x17 => "OR",
        0x19 => "NOT",
        0x20 => "KECCAK256",
        0x35 => "CALLDATALOAD",
        0x36 => "CALLDATASIZE",
        0x37 => "CALLDATACOPY",
        0x39 => "CODECOPY",
        0x51 => "MLOAD",
        0x52 => "MSTORE",
        0x53 => "MSTORE8",
        0x54 => "SLOAD",
        0x55 => "SSTORE",
        0x56 => "JUMP",
        0x57 => "JUMPI",
        0x5b => "JUMPDEST",
        0x5f => "PUSH0",
        0x60..=0x7f => "PUSH",
        0x80..=0x8f => "DUP",
        0x90..=0x9f => "SWAP",
        0xf3 => "RETURN",
        0xfd => "REVERT",
        _ => "OP",
    }
}

fn evm_instruction_category(opcode: u8) -> InstructionCategory {
    match opcode {
        0x01..=0x07 | 0x10..=0x1d => InstructionCategory::Arithmetic,
        0x35 | 0x36 | 0x37 | 0x39 | 0x51 | 0x54 => InstructionCategory::Load,
        0x52 | 0x53 | 0x55 => InstructionCategory::Store,
        0x56 => InstructionCategory::Jump,
        0x57 => InstructionCategory::Branch,
        0x5f..=0x7f | 0x80..=0x9f => InstructionCategory::Move,
        _ => InstructionCategory::Unknown,
    }
}

fn evm_opcode_category(opcode: u8) -> OpcodeCategory {
    match opcode {
        0x01..=0x07 | 0x16..=0x1d => OpcodeCategory::Arithmetic,
        0x10..=0x15 => OpcodeCategory::Comparison,
        0x35..=0x37 => OpcodeCategory::CallData,
        0x39 | 0x51..=0x53 => OpcodeCategory::Memory,
        0x54 | 0x55 => OpcodeCategory::Storage,
        0x56 | 0x57 | 0x5b => OpcodeCategory::ControlFlow,
        0x5f..=0x7f => OpcodeCategory::Push,
        0x80..=0x9f => OpcodeCategory::Stack,
        0xf3 | 0xfd => OpcodeCategory::Return,
        _ => OpcodeCategory::Unknown,
    }
}

fn evm_static_gas(opcode: u8) -> u64 {
    match opcode {
        0x00 => 0,
        0x01..=0x03 | 0x10..=0x19 | 0x1b..=0x1d => 3,
        0x04..=0x07 => 5,
        0x20 => 30,
        0x35 | 0x36 => 3,
        0x37 | 0x39 => 3,
        0x51 | 0x52 | 0x53 => 3,
        0x54 => 100,
        0x55 => 100,
        0x56 => 8,
        0x57 => 10,
        0x5b => 1,
        0x5f..=0x7f => 3,
        0x80..=0x8f => 3,
        0x90..=0x9f => 3,
        0xf3 | 0xfd => 0,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{TraceFact, TraceValidator};

    use crate::{
        BytecodePcRange, BytecodeSourceMapEntry,
        trace::{emit_bytecode_instruction_facts, emit_codegen_facts},
    };

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

    #[test]
    fn codegen_trace_emits_instruction_facts_from_actual_bytecode() {
        let facts =
            emit_bytecode_instruction_facts("contract:Fib", "runtime", &[0x5f, 0x60, 0x01, 0x01]);
        let summary = TraceValidator::validate(&facts).unwrap();

        assert_eq!(summary.instruction_count, 3);
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Instruction(instruction) if instruction.mnemonic == "ADD"
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Opcode(opcode) if opcode.opcode == "PUSH"
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::GasCost(gas) if gas.gas > 0
        )));
    }
}
