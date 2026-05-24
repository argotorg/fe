use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;

use crate::fact::{
    CategorySource, CompilerEventFact, CompilerPhase, InlineContextFact, InstructionCategoryFact,
    InstructionFact, LoopDerivation, LoopMembershipFact, OpcodeFact, OriginEdgeFact,
    OriginNodeFact, StorageFact, StorageLocation, TraceFact,
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TraceValidationSummary {
    pub fact_count: usize,
    pub node_count: usize,
    pub edge_count: usize,
    pub instruction_count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraceValidationReport {
    pub summary: TraceValidationSummary,
    pub diagnostics: Vec<TraceValidationDiagnostic>,
}

impl TraceValidationReport {
    pub fn first_error(&self) -> Option<&TraceValidationError> {
        self.diagnostics.iter().find_map(|diagnostic| {
            if let TraceValidationDiagnostic::Error(error) = diagnostic {
                Some(error)
            } else {
                None
            }
        })
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.level() == TraceValidationLevel::Error)
            .count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.level() == TraceValidationLevel::Warning)
            .count()
    }

    pub fn info_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.level() == TraceValidationLevel::Info)
            .count()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TraceValidationLevel {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceValidationDiagnostic {
    Error(TraceValidationError),
    Warning(TraceValidationWarning),
    Info(TraceValidationInfo),
}

impl TraceValidationDiagnostic {
    pub const fn level(&self) -> TraceValidationLevel {
        match self {
            Self::Error(_) => TraceValidationLevel::Error,
            Self::Warning(_) => TraceValidationLevel::Warning,
            Self::Info(_) => TraceValidationLevel::Info,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceValidationWarning {
    UnknownInstructionCategory { instruction: OriginExportKey },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceValidationInfo {
    PosthocInstructionCategory {
        instruction: OriginExportKey,
        version: String,
    },
}

pub struct TraceValidator;

impl TraceValidator {
    pub fn validate(facts: &[TraceFact]) -> Result<TraceValidationSummary, TraceValidationError> {
        let report = Self::check(facts);
        if let Some(error) = report.first_error() {
            Err(error.clone())
        } else {
            Ok(report.summary)
        }
    }

    pub fn check(facts: &[TraceFact]) -> TraceValidationReport {
        let mut diagnostics = Vec::new();
        let mut nodes = BTreeSet::new();
        let mut edges = Vec::new();
        let mut storage = Vec::new();
        let mut instructions = Vec::new();
        let mut instruction_categories = Vec::new();
        let mut loop_memberships = Vec::new();
        let mut inline_contexts = Vec::new();
        let mut compiler_events = Vec::new();
        let mut opcodes = Vec::new();
        let mut gas_costs = Vec::new();

        for fact in facts {
            match fact {
                TraceFact::OriginNode(node) => {
                    validate_origin_node(node, &mut diagnostics);
                    if !nodes.insert(node.key.clone()) {
                        push_error(
                            &mut diagnostics,
                            TraceValidationError::DuplicateOriginNode {
                                key: node.key.clone(),
                            },
                        );
                    }
                }
                TraceFact::OriginEdge(edge) => edges.push(edge),
                TraceFact::CompilerEvent(event) => compiler_events.push(event),
                TraceFact::Storage(storage_fact) => storage.push(storage_fact),
                TraceFact::Instruction(instruction) => instructions.push(instruction),
                TraceFact::InstructionCategory(category) => instruction_categories.push(category),
                TraceFact::LoopMembership(membership) => loop_memberships.push(membership),
                TraceFact::InlineContext(context) => inline_contexts.push(context),
                TraceFact::Opcode(opcode) => opcodes.push(opcode),
                TraceFact::GasCost(gas_cost) => gas_costs.push(gas_cost),
            }
        }

        let mut instruction_owners = BTreeMap::new();
        let mut instruction_sites = BTreeMap::new();
        let mut instruction_keys = BTreeSet::new();
        for instruction in &instructions {
            validate_instruction(instruction, &mut diagnostics);
            require_node(
                &nodes,
                &instruction.instruction,
                "instruction",
                &mut diagnostics,
            );
            require_node(
                &nodes,
                &instruction.function,
                "instruction.function",
                &mut diagnostics,
            );
            if !instruction_keys.insert(instruction.instruction.clone()) {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::DuplicateInstruction {
                        instruction: instruction.instruction.clone(),
                    },
                );
            }
            match instruction_owners.insert(
                instruction.instruction.clone(),
                instruction.function.clone(),
            ) {
                Some(existing) if existing != instruction.function => {
                    push_error(
                        &mut diagnostics,
                        TraceValidationError::InstructionHasMultipleFunctions {
                            instruction: instruction.instruction.clone(),
                            first_function: existing,
                            second_function: instruction.function.clone(),
                        },
                    );
                }
                _ => {}
            }
            let site = (instruction.function.clone(), instruction.index);
            if let Some(first_instruction) =
                instruction_sites.insert(site, instruction.instruction.clone())
            {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::DuplicateInstructionSite {
                        function: instruction.function.clone(),
                        index: instruction.index,
                        first_instruction,
                        second_instruction: instruction.instruction.clone(),
                    },
                );
            }
        }

        for edge in &edges {
            validate_edge(edge, &nodes, &mut diagnostics);
        }
        for storage_fact in storage {
            validate_storage(storage_fact, &nodes, &mut diagnostics);
        }
        for event in compiler_events {
            validate_compiler_event(event, &nodes, &mut diagnostics);
        }
        let mut instruction_categories_by_instruction = BTreeMap::new();
        for category in instruction_categories {
            validate_instruction_category(category, &nodes, &instruction_owners, &mut diagnostics);
            match instruction_categories_by_instruction
                .insert(category.instruction.clone(), category.category)
            {
                Some(existing) if existing == category.category => {
                    push_error(
                        &mut diagnostics,
                        TraceValidationError::DuplicateInstructionCategory {
                            instruction: category.instruction.clone(),
                            category: category.category,
                        },
                    );
                }
                Some(existing) => {
                    push_error(
                        &mut diagnostics,
                        TraceValidationError::AmbiguousInstructionCategory {
                            instruction: category.instruction.clone(),
                            first_category: existing,
                            second_category: category.category,
                        },
                    );
                }
                None => {}
            }
        }
        for membership in loop_memberships {
            validate_loop_membership(membership, &nodes, &instruction_owners, &mut diagnostics);
        }
        for context in inline_contexts {
            validate_inline_context(context, &nodes, &mut diagnostics);
        }
        for opcode in opcodes {
            validate_opcode(opcode, &nodes, &mut diagnostics);
        }
        for gas_cost in gas_costs {
            validate_gas_cost(gas_cost, &nodes, &mut diagnostics);
        }

        TraceValidationReport {
            summary: TraceValidationSummary {
                fact_count: facts.len(),
                node_count: nodes.len(),
                edge_count: edges.len(),
                instruction_count: instructions.len(),
            },
            diagnostics,
        }
    }
}

fn validate_origin_node(node: &OriginNodeFact, diagnostics: &mut Vec<TraceValidationDiagnostic>) {
    if node.kind().trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyOriginNodeKind {
                key: node.key.clone(),
            },
        );
    }
}

fn validate_instruction(
    instruction: &InstructionFact,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    if instruction.mnemonic.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyInstructionMnemonic {
                instruction: instruction.instruction.clone(),
            },
        );
    }
}

fn validate_edge(
    edge: &OriginEdgeFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &edge.from, "origin_edge.from", diagnostics);
    require_node(nodes, &edge.to, "origin_edge.to", diagnostics);
}

fn validate_storage(
    storage: &StorageFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &storage.subject, "storage.subject", diagnostics);
    match &storage.location {
        StorageLocation::VirtualRegister(name) if name.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyRegisterName {
                    subject: storage.subject.clone(),
                    location_kind: "virtual_register",
                },
            );
        }
        StorageLocation::PhysicalRegister(name) if name.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyRegisterName {
                    subject: storage.subject.clone(),
                    location_kind: "physical_register",
                },
            );
        }
        _ => {}
    }
    if !valid_storage_phase_location(storage.phase, &storage.location) {
        push_error(
            diagnostics,
            TraceValidationError::InvalidStoragePhaseLocation {
                subject: storage.subject.clone(),
                phase: storage.phase,
                location: storage.location.clone(),
            },
        );
    }
}

fn validate_compiler_event(
    event: &CompilerEventFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &event.event, "compiler_event.event", diagnostics);
    for input in &event.inputs {
        require_node(nodes, input, "compiler_event.input", diagnostics);
    }
    for output in &event.outputs {
        require_node(nodes, output, "compiler_event.output", diagnostics);
    }
    if let Some(reason) = &event.reason
        && reason.as_str().trim().is_empty()
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyCompilerReason {
                event: event.event.clone(),
            },
        );
    }
}

fn validate_instruction_category(
    category: &InstructionCategoryFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &category.instruction,
        "instruction_category.instruction",
        diagnostics,
    );
    if !instruction_owners.contains_key(&category.instruction) {
        push_error(
            diagnostics,
            TraceValidationError::InstructionCategoryWithoutInstruction {
                instruction: category.instruction.clone(),
            },
        );
    }
    match &category.source {
        CategorySource::PosthocClassifier { version } if version.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyPosthocClassifierVersion {
                    instruction: category.instruction.clone(),
                },
            );
        }
        CategorySource::PosthocClassifier { version } => {
            diagnostics.push(TraceValidationDiagnostic::Info(
                TraceValidationInfo::PosthocInstructionCategory {
                    instruction: category.instruction.clone(),
                    version: version.clone(),
                },
            ));
        }
        _ => {}
    }
}

fn validate_loop_membership(
    membership: &LoopMembershipFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &membership.loop_key,
        "loop_membership.loop_key",
        diagnostics,
    );
    require_node(
        nodes,
        &membership.instruction,
        "loop_membership.instruction",
        diagnostics,
    );
    if !instruction_owners.contains_key(&membership.instruction) {
        push_error(
            diagnostics,
            TraceValidationError::LoopMembershipWithoutInstruction {
                instruction: membership.instruction.clone(),
            },
        );
    }
    if let LoopDerivation::NaturalLoopAnalysis { cfg_hash } = &membership.derived_from
        && cfg_hash.trim().is_empty()
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyLoopCfgHash {
                loop_key: membership.loop_key.clone(),
            },
        );
    }
}

fn validate_inline_context(
    context: &InlineContextFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &context.inline_instance,
        "inline_context.inline_instance",
        diagnostics,
    );
    require_node(
        nodes,
        &context.caller_function,
        "inline_context.caller_function",
        diagnostics,
    );
    require_node(
        nodes,
        &context.callee_function,
        "inline_context.callee_function",
        diagnostics,
    );
    require_node(
        nodes,
        &context.callsite,
        "inline_context.callsite",
        diagnostics,
    );
}

fn validate_opcode(
    opcode: &OpcodeFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &opcode.pc, "opcode.pc", diagnostics);
    if opcode.pc.kind() != "bytecode.pc" {
        push_error(
            diagnostics,
            TraceValidationError::InvalidOpcodeSubjectKind {
                pc: opcode.pc.clone(),
            },
        );
    }
    if opcode.opcode.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyOpcode {
                pc: opcode.pc.clone(),
            },
        );
    }
}

fn validate_gas_cost(
    gas_cost: &crate::fact::GasCostFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &gas_cost.subject, "gas_cost.subject", diagnostics);
    if gas_cost.schedule.as_str().trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyGasSchedule {
                subject: gas_cost.subject.clone(),
            },
        );
    }
    if gas_cost.gas_kind == crate::fact::GasKind::OpcodeStatic
        && gas_cost.subject.kind() != "bytecode.pc"
    {
        push_error(
            diagnostics,
            TraceValidationError::InvalidOpcodeGasSubjectKind {
                subject: gas_cost.subject.clone(),
            },
        );
    }
}

fn valid_storage_phase_location(phase: CompilerPhase, location: &StorageLocation) -> bool {
    match location {
        StorageLocation::SsaValue => matches!(
            phase,
            CompilerPhase::Mir | CompilerPhase::SonatinaPreOpt | CompilerPhase::SonatinaPostOpt
        ),
        StorageLocation::MemoryPlace => phase == CompilerPhase::Mir,
        StorageLocation::StackSlot { .. } => {
            matches!(
                phase,
                CompilerPhase::Backend | CompilerPhase::BytecodeEmission
            )
        }
        StorageLocation::VirtualRegister(_) => matches!(
            phase,
            CompilerPhase::SonatinaPreOpt | CompilerPhase::SonatinaPostOpt | CompilerPhase::Backend
        ),
        StorageLocation::PhysicalRegister(_) => phase == CompilerPhase::Backend,
        StorageLocation::Unknown => true,
    }
}

fn require_node(
    nodes: &BTreeSet<OriginExportKey>,
    key: &OriginExportKey,
    role: &'static str,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    if !nodes.contains(key) {
        push_error(
            diagnostics,
            TraceValidationError::MissingOriginNode {
                role,
                key: key.clone(),
            },
        );
    }
}

fn push_error(diagnostics: &mut Vec<TraceValidationDiagnostic>, error: TraceValidationError) {
    diagnostics.push(TraceValidationDiagnostic::Error(error));
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceValidationError {
    DuplicateOriginNode {
        key: OriginExportKey,
    },
    EmptyOriginNodeKind {
        key: OriginExportKey,
    },
    MissingOriginNode {
        role: &'static str,
        key: OriginExportKey,
    },
    DuplicateInstruction {
        instruction: OriginExportKey,
    },
    DuplicateInstructionSite {
        function: OriginExportKey,
        index: u32,
        first_instruction: OriginExportKey,
        second_instruction: OriginExportKey,
    },
    EmptyInstructionMnemonic {
        instruction: OriginExportKey,
    },
    InstructionHasMultipleFunctions {
        instruction: OriginExportKey,
        first_function: OriginExportKey,
        second_function: OriginExportKey,
    },
    EmptyPosthocClassifierVersion {
        instruction: OriginExportKey,
    },
    DuplicateInstructionCategory {
        instruction: OriginExportKey,
        category: crate::fact::InstructionCategory,
    },
    AmbiguousInstructionCategory {
        instruction: OriginExportKey,
        first_category: crate::fact::InstructionCategory,
        second_category: crate::fact::InstructionCategory,
    },
    InstructionCategoryWithoutInstruction {
        instruction: OriginExportKey,
    },
    LoopMembershipWithoutInstruction {
        instruction: OriginExportKey,
    },
    EmptyLoopCfgHash {
        loop_key: OriginExportKey,
    },
    EmptyCompilerReason {
        event: OriginExportKey,
    },
    EmptyRegisterName {
        subject: OriginExportKey,
        location_kind: &'static str,
    },
    InvalidStoragePhaseLocation {
        subject: OriginExportKey,
        phase: CompilerPhase,
        location: StorageLocation,
    },
    EmptyOpcode {
        pc: OriginExportKey,
    },
    InvalidOpcodeSubjectKind {
        pc: OriginExportKey,
    },
    EmptyGasSchedule {
        subject: OriginExportKey,
    },
    InvalidOpcodeGasSubjectKind {
        subject: OriginExportKey,
    },
}

impl fmt::Display for TraceValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateOriginNode { key } => {
                write!(f, "duplicate origin node {}", key.display_label())
            }
            Self::EmptyOriginNodeKind { key } => {
                write!(f, "origin node {} has an empty kind", key.display_label())
            }
            Self::MissingOriginNode { role, key } => {
                write!(
                    f,
                    "{role} references unknown origin node {}",
                    key.display_label()
                )
            }
            Self::DuplicateInstruction { instruction } => {
                write!(f, "duplicate instruction {}", instruction.display_label())
            }
            Self::DuplicateInstructionSite {
                function,
                index,
                first_instruction,
                second_instruction,
            } => write!(
                f,
                "function {} has multiple instructions at index {index}: {} and {}",
                function.display_label(),
                first_instruction.display_label(),
                second_instruction.display_label()
            ),
            Self::EmptyInstructionMnemonic { instruction } => write!(
                f,
                "instruction {} has an empty mnemonic",
                instruction.display_label()
            ),
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
            Self::EmptyPosthocClassifierVersion { instruction } => write!(
                f,
                "instruction {} has an empty posthoc classifier version",
                instruction.display_label()
            ),
            Self::DuplicateInstructionCategory {
                instruction,
                category,
            } => write!(
                f,
                "instruction {} has duplicate category {category:?}",
                instruction.display_label()
            ),
            Self::AmbiguousInstructionCategory {
                instruction,
                first_category,
                second_category,
            } => write!(
                f,
                "instruction {} has ambiguous categories {first_category:?} and {second_category:?}",
                instruction.display_label()
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
            Self::EmptyLoopCfgHash { loop_key } => write!(
                f,
                "loop membership for {} has an empty CFG hash",
                loop_key.display_label()
            ),
            Self::EmptyCompilerReason { event } => write!(
                f,
                "compiler event {} has an empty reason",
                event.display_label()
            ),
            Self::EmptyRegisterName {
                subject,
                location_kind,
            } => write!(
                f,
                "storage fact for {} has an empty {location_kind} name",
                subject.display_label()
            ),
            Self::InvalidStoragePhaseLocation {
                subject,
                phase,
                location,
            } => write!(
                f,
                "storage fact for {} has invalid phase/location combination: {phase:?} with {location:?}",
                subject.display_label()
            ),
            Self::EmptyOpcode { pc } => {
                write!(
                    f,
                    "opcode fact for {} has an empty opcode",
                    pc.display_label()
                )
            }
            Self::InvalidOpcodeSubjectKind { pc } => write!(
                f,
                "opcode fact subject {} is not a bytecode PC origin",
                pc.display_label()
            ),
            Self::EmptyGasSchedule { subject } => write!(
                f,
                "gas cost for {} has an empty schedule",
                subject.display_label()
            ),
            Self::InvalidOpcodeGasSubjectKind { subject } => write!(
                f,
                "opcode static gas subject {} is not a bytecode PC origin",
                subject.display_label()
            ),
        }
    }
}

impl std::error::Error for TraceValidationError {}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use crate::{
        CategorySource, CompilerPhase, EvmSchedule, GasConfidence, GasCostFact, GasKind, GasSource,
        InlineContextFact, InstructionCategory, InstructionCategoryFact, InstructionFact,
        LoopDerivation, LoopMembershipFact, OpcodeCategory, OpcodeFact, OriginEdgeFact,
        OriginEdgeLabel, OriginNodeFact, OriginNodeKind, StorageFact, StorageLocation,
        StorageReason, TraceFact, TraceValidationError, TraceValidationLevel, TraceValidator,
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

    #[test]
    fn validator_rejects_empty_instruction_mnemonic() {
        let function = key("function", "fib", "recv");
        let instruction = key("asm.inst", "fib", "inst:0");
        let facts = vec![
            node("function", "fib", "recv"),
            node("asm.inst", "fib", "inst:0"),
            TraceFact::Instruction(InstructionFact::new(instruction.clone(), function, 0, " ")),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::EmptyInstructionMnemonic { instruction })
        );
    }

    #[test]
    fn validator_rejects_duplicate_instruction_keys() {
        let function = key("function", "fib", "recv");
        let instruction = key("asm.inst", "fib", "inst:0");
        let facts = vec![
            node("function", "fib", "recv"),
            node("asm.inst", "fib", "inst:0"),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function.clone(),
                0,
                "lw",
            )),
            TraceFact::Instruction(InstructionFact::new(instruction.clone(), function, 0, "lw")),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::DuplicateInstruction { instruction })
        );
    }

    #[test]
    fn validator_rejects_duplicate_function_instruction_indexes() {
        let function = key("function", "fib", "recv");
        let first = key("asm.inst", "fib", "inst:0");
        let second = key("asm.inst", "fib", "inst:1");
        let facts = vec![
            node("function", "fib", "recv"),
            node("asm.inst", "fib", "inst:0"),
            node("asm.inst", "fib", "inst:1"),
            TraceFact::Instruction(InstructionFact::new(
                first.clone(),
                function.clone(),
                0,
                "lw",
            )),
            TraceFact::Instruction(InstructionFact::new(
                second.clone(),
                function.clone(),
                0,
                "sw",
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::DuplicateInstructionSite {
                function,
                index: 0,
                first_instruction: first,
                second_instruction: second,
            })
        );
    }

    #[test]
    fn validator_rejects_duplicate_instruction_categories() {
        let function = key("function", "fib", "recv");
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("function", "fib", "recv"),
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "MLOAD",
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::Load,
                CategorySource::BackendEmissionReason,
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::Load,
                CategorySource::ManualAnnotation,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::DuplicateInstructionCategory {
                instruction,
                category: InstructionCategory::Load,
            })
        );
    }

    #[test]
    fn validator_rejects_ambiguous_instruction_categories() {
        let function = key("function", "fib", "recv");
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("function", "fib", "recv"),
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "MLOAD",
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::Load,
                CategorySource::BackendEmissionReason,
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction.clone(),
                InstructionCategory::Arithmetic,
                CategorySource::ManualAnnotation,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::AmbiguousInstructionCategory {
                instruction,
                first_category: InstructionCategory::Load,
                second_category: InstructionCategory::Arithmetic,
            })
        );
    }

    #[test]
    fn validator_rejects_bad_storage_phase_location_pairs() {
        let local = key("runtime.local", "fib", "local:b");
        let facts = vec![
            node("runtime.local", "fib", "local:b"),
            TraceFact::Storage(StorageFact::new(
                local.clone(),
                CompilerPhase::Hir,
                StorageLocation::StackSlot { offset: 0 },
                StorageReason::FrameSlot,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidStoragePhaseLocation {
                subject: local,
                phase: CompilerPhase::Hir,
                location: StorageLocation::StackSlot { offset: 0 },
            })
        );
    }

    #[test]
    fn validator_reports_info_for_posthoc_classification() {
        let function = key("function", "fib", "recv");
        let instruction = key("asm.inst", "fib", "inst:0");
        let facts = vec![
            node("function", "fib", "recv"),
            node("asm.inst", "fib", "inst:0"),
            TraceFact::Instruction(InstructionFact::new(instruction.clone(), function, 0, "lw")),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                instruction,
                InstructionCategory::StackLoad,
                CategorySource::PosthocClassifier {
                    version: "test-classifier".to_string(),
                },
            )),
        ];

        let report = TraceValidator::check(&facts);
        assert_eq!(report.error_count(), 0);
        assert_eq!(report.warning_count(), 0);
        assert_eq!(report.info_count(), 1);
        assert_eq!(report.diagnostics[0].level(), TraceValidationLevel::Info);
    }

    #[test]
    fn validator_rejects_opcode_facts_without_bytecode_pc_subjects() {
        let instruction = key("asm.inst", "fib", "inst:0");
        let facts = vec![
            node("asm.inst", "fib", "inst:0"),
            TraceFact::Opcode(OpcodeFact::new(
                instruction.clone(),
                "ADD",
                None,
                OpcodeCategory::Arithmetic,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidOpcodeSubjectKind { pc: instruction })
        );
    }

    #[test]
    fn validator_rejects_opcode_static_gas_without_bytecode_pc_subjects() {
        let instruction = key("asm.inst", "fib", "inst:0");
        let facts = vec![
            node("asm.inst", "fib", "inst:0"),
            TraceFact::GasCost(GasCostFact::new(
                instruction.clone(),
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new("cancun"),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidOpcodeGasSubjectKind {
                subject: instruction,
            })
        );
    }

    #[test]
    fn validator_rejects_empty_gas_schedule() {
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::GasCost(GasCostFact::new(
                instruction.clone(),
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new(" "),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::EmptyGasSchedule {
                subject: instruction,
            })
        );
    }
}
