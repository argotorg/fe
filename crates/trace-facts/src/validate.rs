use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;

use crate::fact::{InlineContextFact, LoopMembershipFact, OriginEdgeFact, StorageFact, TraceFact};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TraceValidationSummary {
    pub fact_count: usize,
    pub node_count: usize,
    pub edge_count: usize,
    pub instruction_count: usize,
}

pub struct TraceValidator;

impl TraceValidator {
    pub fn validate(facts: &[TraceFact]) -> Result<TraceValidationSummary, TraceValidationError> {
        let mut nodes = BTreeSet::new();
        let mut edges = Vec::new();
        let mut storage = Vec::new();
        let mut instructions = Vec::new();
        let mut instruction_categories = Vec::new();
        let mut loop_memberships = Vec::new();
        let mut inline_contexts = Vec::new();
        let mut compiler_events = Vec::new();

        for fact in facts {
            match fact {
                TraceFact::OriginNode(node) => {
                    if !nodes.insert(node.key.clone()) {
                        return Err(TraceValidationError::DuplicateOriginNode {
                            key: node.key.clone(),
                        });
                    }
                }
                TraceFact::OriginEdge(edge) => edges.push(edge),
                TraceFact::CompilerEvent(event) => compiler_events.push(event),
                TraceFact::Storage(storage_fact) => storage.push(storage_fact),
                TraceFact::Instruction(instruction) => instructions.push(instruction),
                TraceFact::InstructionCategory(category) => {
                    instruction_categories.push(&category.instruction)
                }
                TraceFact::LoopMembership(membership) => loop_memberships.push(membership),
                TraceFact::InlineContext(context) => inline_contexts.push(context),
            }
        }

        let mut instruction_owners = BTreeMap::new();
        for instruction in &instructions {
            require_node(&nodes, &instruction.instruction, "instruction")?;
            require_node(&nodes, &instruction.function, "instruction.function")?;
            match instruction_owners.insert(
                instruction.instruction.clone(),
                instruction.function.clone(),
            ) {
                Some(existing) if existing != instruction.function => {
                    return Err(TraceValidationError::InstructionHasMultipleFunctions {
                        instruction: instruction.instruction.clone(),
                        first_function: existing,
                        second_function: instruction.function.clone(),
                    });
                }
                _ => {}
            }
        }

        for edge in &edges {
            validate_edge(edge, &nodes)?;
        }
        for storage_fact in storage {
            validate_storage(storage_fact, &nodes)?;
        }
        for event in compiler_events {
            require_node(&nodes, &event.event, "compiler_event.event")?;
            for input in &event.inputs {
                require_node(&nodes, input, "compiler_event.input")?;
            }
            for output in &event.outputs {
                require_node(&nodes, output, "compiler_event.output")?;
            }
        }
        for instruction in instruction_categories {
            require_node(&nodes, instruction, "instruction_category.instruction")?;
            if !instruction_owners.contains_key(instruction) {
                return Err(
                    TraceValidationError::InstructionCategoryWithoutInstruction {
                        instruction: instruction.clone(),
                    },
                );
            }
        }
        for membership in loop_memberships {
            validate_loop_membership(membership, &nodes, &instruction_owners)?;
        }
        for context in inline_contexts {
            validate_inline_context(context, &nodes)?;
        }

        Ok(TraceValidationSummary {
            fact_count: facts.len(),
            node_count: nodes.len(),
            edge_count: edges.len(),
            instruction_count: instructions.len(),
        })
    }
}

fn validate_edge(
    edge: &OriginEdgeFact,
    nodes: &BTreeSet<OriginExportKey>,
) -> Result<(), TraceValidationError> {
    require_node(nodes, &edge.from, "origin_edge.from")?;
    require_node(nodes, &edge.to, "origin_edge.to")
}

fn validate_storage(
    storage: &StorageFact,
    nodes: &BTreeSet<OriginExportKey>,
) -> Result<(), TraceValidationError> {
    require_node(nodes, &storage.subject, "storage.subject")
}

fn validate_loop_membership(
    membership: &LoopMembershipFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
) -> Result<(), TraceValidationError> {
    require_node(nodes, &membership.loop_key, "loop_membership.loop_key")?;
    require_node(
        nodes,
        &membership.instruction,
        "loop_membership.instruction",
    )?;
    if !instruction_owners.contains_key(&membership.instruction) {
        return Err(TraceValidationError::LoopMembershipWithoutInstruction {
            instruction: membership.instruction.clone(),
        });
    }
    Ok(())
}

fn validate_inline_context(
    context: &InlineContextFact,
    nodes: &BTreeSet<OriginExportKey>,
) -> Result<(), TraceValidationError> {
    require_node(
        nodes,
        &context.inline_instance,
        "inline_context.inline_instance",
    )?;
    require_node(
        nodes,
        &context.caller_function,
        "inline_context.caller_function",
    )?;
    require_node(
        nodes,
        &context.callee_function,
        "inline_context.callee_function",
    )?;
    require_node(nodes, &context.callsite, "inline_context.callsite")
}

fn require_node(
    nodes: &BTreeSet<OriginExportKey>,
    key: &OriginExportKey,
    role: &'static str,
) -> Result<(), TraceValidationError> {
    if nodes.contains(key) {
        Ok(())
    } else {
        Err(TraceValidationError::MissingOriginNode {
            role,
            key: key.clone(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceValidationError {
    DuplicateOriginNode {
        key: OriginExportKey,
    },
    MissingOriginNode {
        role: &'static str,
        key: OriginExportKey,
    },
    InstructionHasMultipleFunctions {
        instruction: OriginExportKey,
        first_function: OriginExportKey,
        second_function: OriginExportKey,
    },
    InstructionCategoryWithoutInstruction {
        instruction: OriginExportKey,
    },
    LoopMembershipWithoutInstruction {
        instruction: OriginExportKey,
    },
}

impl fmt::Display for TraceValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateOriginNode { key } => {
                write!(f, "duplicate origin node {}", key.display_label())
            }
            Self::MissingOriginNode { role, key } => {
                write!(
                    f,
                    "{role} references unknown origin node {}",
                    key.display_label()
                )
            }
            Self::InstructionHasMultipleFunctions {
                instruction,
                first_function,
                second_function,
            } => write!(
                f,
                "instruction {} belongs to multiple functions: {} and {}",
                instruction.display_label(),
                first_function.display_label(),
                second_function.display_label()
            ),
            Self::InstructionCategoryWithoutInstruction { instruction } => write!(
                f,
                "instruction category references {} but no instruction fact defines it",
                instruction.display_label()
            ),
            Self::LoopMembershipWithoutInstruction { instruction } => write!(
                f,
                "loop membership references {} but no instruction fact defines it",
                instruction.display_label()
            ),
        }
    }
}

impl std::error::Error for TraceValidationError {}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use crate::{
        CategorySource, CompilerPhase, InlineContextFact, InstructionCategory,
        InstructionCategoryFact, InstructionFact, LoopDerivation, LoopMembershipFact,
        OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind, StorageFact,
        StorageLocation, StorageReason, TraceFact, TraceValidationError, TraceValidator,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(kind: &str, owner: &str, local: &str) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key(kind, owner, local),
            OriginNodeKind::new(kind),
        ))
    }

    #[test]
    fn validator_accepts_connected_trace_facts() {
        let function = key("function", "fib", "recv");
        let callee = key("function", "fib", "fib");
        let callsite = key("hir.expr", "fib", "expr:call");
        let inline_instance = key("inline.instance", "fib", "inline:0");
        let loop_key = key("loop", "fib", "loop:0");
        let local = key("runtime.local", "fib", "local:b");
        let instruction = key("asm.inst", "fib", "inst:6");

        let facts = vec![
            node("function", "fib", "recv"),
            node("function", "fib", "fib"),
            node("hir.expr", "fib", "expr:call"),
            node("inline.instance", "fib", "inline:0"),
            node("loop", "fib", "loop:0"),
            node("runtime.local", "fib", "local:b"),
            node("asm.inst", "fib", "inst:6"),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                local.clone(),
                OriginEdgeLabel::LoadOf,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::Storage(StorageFact::new(
                local,
                CompilerPhase::Mir,
                StorageLocation::MemoryPlace,
                StorageReason::MutableLocalLowering,
            )),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function.clone(),
                6,
                "lw",
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::StackLoad,
                CategorySource::PosthocClassifier {
                    version: "test".to_string(),
                },
            )),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key,
                instruction,
                LoopDerivation::BackendBlockMapping,
            )),
            TraceFact::InlineContext(InlineContextFact::new(
                inline_instance,
                function,
                callee,
                callsite,
            )),
        ];

        let summary = TraceValidator::validate(&facts).unwrap();
        assert_eq!(summary.node_count, 7);
        assert_eq!(summary.instruction_count, 1);
    }

    #[test]
    fn validator_rejects_edges_to_unknown_nodes() {
        let known = key("asm.inst", "fib", "inst:6");
        let missing = key("runtime.local", "fib", "local:b");
        let facts = vec![
            node("asm.inst", "fib", "inst:6"),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                known,
                missing.clone(),
                OriginEdgeLabel::LoadOf,
                Some(CompilerPhase::Backend),
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::MissingOriginNode {
                role: "origin_edge.to",
                key: missing,
            })
        );
    }

    #[test]
    fn validator_rejects_instruction_category_without_instruction_fact() {
        let instruction = key("asm.inst", "fib", "inst:6");
        let facts = vec![
            node("asm.inst", "fib", "inst:6"),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::StackLoad,
                CategorySource::BackendEmissionReason,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InstructionCategoryWithoutInstruction { instruction })
        );
    }
}
