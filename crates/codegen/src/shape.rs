use common::origin::OriginExportKey;
use shape_address::{ShapeDimension, ShapeError, ShapeGraph, ShapeGraphKey, ShapeNodeKey};

pub fn describe_bytecode_shape(
    owner_key: &str,
    function_local_key: &str,
    bytecode: &[u8],
) -> Result<ShapeGraph, ShapeError> {
    let function =
        OriginExportKey::try_from_raw_parts("bytecode.function", owner_key, function_local_key)
            .expect("codegen bytecode function key must be valid");
    let code_object = OriginExportKey::try_from_raw_parts("code.object", owner_key, "runtime")
        .expect("codegen bytecode code object key must be valid");
    let function_node = ShapeNodeKey::entity(function);
    let code_object_node = ShapeNodeKey::entity(code_object.clone());
    let mut graph = ShapeGraph::new(ShapeGraphKey::new(code_object, "bytecode-shape")?);
    graph.add_node(code_object_node.clone(), "code.object")?;
    graph.add_node(function_node.clone(), "bytecode.function")?;
    graph.add_child(&code_object_node, "function", 0, &function_node)?;

    let mut pc = 0;
    let mut index = 0;
    while pc < bytecode.len() {
        let opcode = bytecode[pc];
        let instruction =
            OriginExportKey::try_from_raw_parts("bytecode.pc", owner_key, format!("pc:{pc}"))
                .expect("codegen bytecode PC key must be valid");
        let instruction_node = ShapeNodeKey::entity(instruction);
        graph.add_node(instruction_node.clone(), "bytecode.instruction")?;
        graph.add_field(
            &instruction_node,
            ShapeDimension::Structure,
            "opcode",
            format!("0x{opcode:02x}"),
        )?;
        let immediate_len = evm_push_immediate_len(opcode);
        if immediate_len > 0 {
            let end = (pc + 1 + immediate_len).min(bytecode.len());
            graph.add_field(
                &instruction_node,
                ShapeDimension::Constants,
                "immediate",
                format!("0x{}", hex::encode(&bytecode[pc + 1..end])),
            )?;
        }
        graph.add_child(&function_node, "instruction", index, &instruction_node)?;
        pc += 1 + immediate_len;
        index += 1;
    }

    Ok(graph)
}

fn evm_push_immediate_len(opcode: u8) -> usize {
    if (0x60..=0x7f).contains(&opcode) {
        (opcode - 0x5f) as usize
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use shape_address::{
        ShapeCyclePolicy, ShapeDimension, ShapeHashPolicy, ShapeViewMode, hash_shape_graph,
    };

    use super::*;

    #[test]
    fn bytecode_shape_separates_opcode_structure_from_push_immediate_constants() {
        let policy = ShapeHashPolicy::new(
            "bytecode",
            ShapeViewMode::AnonymousShape,
            ShapeCyclePolicy::Reject,
        )
        .unwrap();
        let one = describe_bytecode_shape("contract:Fib", "runtime", &[0x60, 0x01]).unwrap();
        let two = describe_bytecode_shape("contract:Fib", "runtime", &[0x60, 0x02]).unwrap();
        let one_hashes = hash_shape_graph(&policy, &one).unwrap();
        let two_hashes = hash_shape_graph(&policy, &two).unwrap();

        assert_eq!(
            one_hashes.graph.get(ShapeDimension::Structure),
            two_hashes.graph.get(ShapeDimension::Structure)
        );
        assert_ne!(
            one_hashes.graph.get(ShapeDimension::Constants),
            two_hashes.graph.get(ShapeDimension::Constants)
        );
    }
}
