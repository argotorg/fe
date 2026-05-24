use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;
use shape_address::{DimensionDigests, ShapeHashPolicy, ShapeLevel, ShapePolicyId};

use crate::fact::{
    BlockFact, CategorySource, CfgEdgeFact, CodeObjectFact, CompilerEventFact, CompilerPhase,
    DisplayNameFact, DisplayNameKind, DynamicGasStepFact, FunctionFact, InlineContextFact,
    InstructionBlockFact, InstructionCategoryFact, InstructionExtentFact, InstructionFact,
    LexicalScopeFact, LocationExpr, LocationRangeFact, LoopBlockFact, LoopBlockRole,
    LoopDerivation, LoopFact, LoopMembershipFact, OpcodeFact, OriginEdgeFact, OriginNodeFact,
    ShapeComponentHashFact, ShapeGraphHashFact, ShapeNodeHashFact, ShapePolicyFact, SourceFileFact,
    SourceSpanFact, StaticGasFact, StorageFact, StorageLocation, TraceFact, TypeFact,
    ValueLocation, ValueProperty, ValuePropertyFact, VariableFact,
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
    UnknownInstructionCategory {
        instruction: OriginExportKey,
    },
    DuplicateDisplayName {
        subject: OriginExportKey,
        kind: DisplayNameKind,
        first_name: String,
        second_name: String,
    },
    DuplicateStaticGas {
        instruction: OriginExportKey,
        schedule: String,
        dynamic_cost_kind: Option<String>,
        first_base_cost: u64,
        second_base_cost: u64,
    },
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
        let mut blocks = Vec::new();
        let mut cfg_edges = Vec::new();
        let mut loops = Vec::new();
        let mut loop_blocks = Vec::new();
        let mut instruction_blocks = Vec::new();
        let mut instruction_extents = Vec::new();
        let mut loop_memberships = Vec::new();
        let mut inline_contexts = Vec::new();
        let mut compiler_events = Vec::new();
        let mut opcodes = Vec::new();
        let mut gas_costs = Vec::new();
        let mut display_names = Vec::new();
        let mut value_properties = Vec::new();
        let mut source_files = Vec::new();
        let mut source_spans = Vec::new();
        let mut code_objects = Vec::new();
        let mut functions = Vec::new();
        let mut lexical_scopes = Vec::new();
        let mut types = Vec::new();
        let mut variables = Vec::new();
        let mut location_ranges = Vec::new();
        let mut static_gas = Vec::new();
        let mut dynamic_gas_steps = Vec::new();
        let mut shape_policies = Vec::new();
        let mut shape_node_hashes = Vec::new();
        let mut shape_component_hashes = Vec::new();
        let mut shape_graph_hashes = Vec::new();

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
                TraceFact::Block(block) => blocks.push(block),
                TraceFact::CfgEdge(edge) => cfg_edges.push(edge),
                TraceFact::Loop(loop_fact) => loops.push(loop_fact),
                TraceFact::LoopBlock(block) => loop_blocks.push(block),
                TraceFact::InstructionBlock(block) => instruction_blocks.push(block),
                TraceFact::InstructionExtent(extent) => instruction_extents.push(extent),
                TraceFact::LoopMembership(membership) => loop_memberships.push(membership),
                TraceFact::InlineContext(context) => inline_contexts.push(context),
                TraceFact::Opcode(opcode) => opcodes.push(opcode),
                TraceFact::GasCost(gas_cost) => gas_costs.push(gas_cost),
                TraceFact::DisplayName(display_name) => display_names.push(display_name),
                TraceFact::ValueProperty(value_property) => value_properties.push(value_property),
                TraceFact::SourceFile(source_file) => source_files.push(source_file),
                TraceFact::SourceSpan(source_span) => source_spans.push(source_span),
                TraceFact::CodeObject(code_object) => code_objects.push(code_object),
                TraceFact::Function(function) => functions.push(function),
                TraceFact::LexicalScope(scope) => lexical_scopes.push(scope),
                TraceFact::Type(ty) => types.push(ty),
                TraceFact::Variable(variable) => variables.push(variable),
                TraceFact::LocationRange(location_range) => location_ranges.push(location_range),
                TraceFact::StaticGas(gas) => static_gas.push(gas),
                TraceFact::DynamicGasStep(step) => dynamic_gas_steps.push(step),
                TraceFact::ShapePolicy(policy) => shape_policies.push(policy),
                TraceFact::ShapeNodeHash(hash) => shape_node_hashes.push(hash),
                TraceFact::ShapeComponentHash(hash) => shape_component_hashes.push(hash),
                TraceFact::ShapeGraphHash(hash) => shape_graph_hashes.push(hash),
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

        let mut block_functions = BTreeMap::new();
        let mut block_ordinals = BTreeMap::new();
        for block in blocks {
            validate_block(block, &nodes, &mut diagnostics);
            if let Some(existing_function) =
                block_functions.insert(block.block.clone(), block.function.clone())
                && existing_function != block.function
            {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::BlockHasMultipleFunctions {
                        block: block.block.clone(),
                        first_function: existing_function,
                        second_function: block.function.clone(),
                    },
                );
            }
            let site = (block.function.clone(), block.ordinal);
            if let Some(first_block) = block_ordinals.insert(site, block.block.clone()) {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::DuplicateBlockOrdinal {
                        function: block.function.clone(),
                        ordinal: block.ordinal,
                        first_block,
                        second_block: block.block.clone(),
                    },
                );
            }
        }

        for cfg_edge in cfg_edges {
            validate_cfg_edge(cfg_edge, &nodes, &block_functions, &mut diagnostics);
        }

        let mut loop_functions = BTreeMap::new();
        let mut loop_headers = BTreeMap::new();
        for loop_fact in loops {
            validate_loop_fact(loop_fact, &nodes, &block_functions, &mut diagnostics);
            loop_functions.insert(loop_fact.loop_key.clone(), loop_fact.function.clone());
            loop_headers.insert(loop_fact.loop_key.clone(), loop_fact.header_block.clone());
        }

        let mut loop_header_role_counts = BTreeMap::<OriginExportKey, usize>::new();
        let mut loop_block_sites = BTreeSet::new();
        for loop_block in loop_blocks {
            validate_loop_block(
                loop_block,
                &nodes,
                &block_functions,
                &loop_functions,
                &loop_headers,
                &mut diagnostics,
            );
            if !loop_block_sites.insert((loop_block.loop_key.clone(), loop_block.block.clone())) {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::DuplicateLoopBlock {
                        loop_key: loop_block.loop_key.clone(),
                        block: loop_block.block.clone(),
                    },
                );
            }
            if loop_block.role == LoopBlockRole::Header {
                *loop_header_role_counts
                    .entry(loop_block.loop_key.clone())
                    .or_default() += 1;
            }
        }
        for loop_key in loop_functions.keys() {
            match loop_header_role_counts.get(loop_key).copied().unwrap_or(0) {
                1 => {}
                0 => push_error(
                    &mut diagnostics,
                    TraceValidationError::InvalidLoopBlockRoles {
                        loop_key: loop_key.clone(),
                        reason: "loop must have exactly one header block role",
                    },
                ),
                _ => push_error(
                    &mut diagnostics,
                    TraceValidationError::InvalidLoopBlockRoles {
                        loop_key: loop_key.clone(),
                        reason: "loop must not have multiple header block roles",
                    },
                ),
            }
        }

        let mut instruction_block_sites = BTreeMap::new();
        for instruction_block in instruction_blocks {
            validate_instruction_block(
                instruction_block,
                &nodes,
                &instruction_owners,
                &block_functions,
                &mut diagnostics,
            );
            if let Some(first_block) = instruction_block_sites.insert(
                instruction_block.instruction.clone(),
                instruction_block.block.clone(),
            ) {
                push_error(
                    &mut diagnostics,
                    TraceValidationError::DuplicateInstructionBlock {
                        instruction: instruction_block.instruction.clone(),
                        first_block,
                        second_block: instruction_block.block.clone(),
                    },
                );
            }
        }

        for extent in instruction_extents {
            validate_instruction_extent(extent, &nodes, &instruction_owners, &mut diagnostics);
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
        let mut display_name_sites = BTreeMap::new();
        for display_name in display_names {
            validate_display_name(display_name, &nodes, &mut diagnostics);
            let site = (display_name.subject.clone(), display_name.kind);
            if let Some(first_name) =
                display_name_sites.insert(site.clone(), display_name.name.clone())
            {
                diagnostics.push(TraceValidationDiagnostic::Warning(
                    TraceValidationWarning::DuplicateDisplayName {
                        subject: site.0,
                        kind: site.1,
                        first_name,
                        second_name: display_name.name.clone(),
                    },
                ));
            }
        }
        for value_property in value_properties {
            validate_value_property(value_property, &nodes, &mut diagnostics);
        }
        for source_file in source_files {
            validate_source_file(source_file, &nodes, &mut diagnostics);
        }
        for source_span in source_spans {
            validate_source_span(source_span, &nodes, &mut diagnostics);
        }
        for code_object in code_objects {
            validate_code_object(code_object, &nodes, &mut diagnostics);
        }
        for function in functions {
            validate_function(function, &nodes, &mut diagnostics);
        }
        for scope in lexical_scopes {
            validate_lexical_scope(scope, &nodes, &mut diagnostics);
        }
        for ty in types {
            validate_type(ty, &nodes, &mut diagnostics);
        }
        for variable in variables {
            validate_variable(variable, &nodes, &mut diagnostics);
        }
        for location_range in location_ranges {
            validate_location_range(location_range, &nodes, &mut diagnostics);
        }
        let mut static_gas_sites = BTreeMap::new();
        for gas in static_gas {
            validate_static_gas(gas, &nodes, &instruction_owners, &mut diagnostics);
            let dynamic_cost_kind = gas.dynamic_cost_kind.map(|kind| format!("{kind:?}"));
            let site = (
                gas.instruction.clone(),
                gas.schedule.to_string(),
                dynamic_cost_kind.clone(),
            );
            if let Some(first_base_cost) = static_gas_sites.insert(site.clone(), gas.base_cost) {
                diagnostics.push(TraceValidationDiagnostic::Warning(
                    TraceValidationWarning::DuplicateStaticGas {
                        instruction: site.0,
                        schedule: site.1,
                        dynamic_cost_kind,
                        first_base_cost,
                        second_base_cost: gas.base_cost,
                    },
                ));
            }
        }
        for step in dynamic_gas_steps {
            validate_dynamic_gas_step(step, &nodes, &mut diagnostics);
        }
        let shape_policy_by_id = validate_shape_policies(shape_policies, &mut diagnostics);
        for hash in shape_node_hashes {
            validate_shape_node_hash(hash, &nodes, &shape_policy_by_id, &mut diagnostics);
        }
        for hash in shape_component_hashes {
            validate_shape_component_hash(hash, &nodes, &shape_policy_by_id, &mut diagnostics);
        }
        for hash in shape_graph_hashes {
            validate_shape_graph_hash(hash, &nodes, &shape_policy_by_id, &mut diagnostics);
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

fn validate_block(
    block: &BlockFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &block.block, "block.block", diagnostics);
    require_node(nodes, &block.function, "block.function", diagnostics);
    if block
        .name
        .as_deref()
        .is_some_and(|name| name.trim().is_empty())
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyBlockName {
                block: block.block.clone(),
            },
        );
    }
}

fn validate_cfg_edge(
    edge: &CfgEdgeFact,
    nodes: &BTreeSet<OriginExportKey>,
    block_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &edge.function, "cfg_edge.function", diagnostics);
    require_node(nodes, &edge.from_block, "cfg_edge.from_block", diagnostics);
    require_node(nodes, &edge.to_block, "cfg_edge.to_block", diagnostics);
    if let Some(condition) = &edge.condition_origin {
        require_node(nodes, condition, "cfg_edge.condition_origin", diagnostics);
    }
    validate_block_owner(
        "cfg_edge.from_block",
        &edge.function,
        &edge.from_block,
        block_functions,
        diagnostics,
    );
    validate_block_owner(
        "cfg_edge.to_block",
        &edge.function,
        &edge.to_block,
        block_functions,
        diagnostics,
    );
}

fn validate_loop_fact(
    loop_fact: &LoopFact,
    nodes: &BTreeSet<OriginExportKey>,
    block_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &loop_fact.loop_key, "loop.loop_key", diagnostics);
    require_node(nodes, &loop_fact.function, "loop.function", diagnostics);
    require_node(
        nodes,
        &loop_fact.header_block,
        "loop.header_block",
        diagnostics,
    );
    validate_block_owner(
        "loop.header_block",
        &loop_fact.function,
        &loop_fact.header_block,
        block_functions,
        diagnostics,
    );
    validate_loop_derivation(&loop_fact.loop_key, &loop_fact.derivation, diagnostics);
}

fn validate_loop_block(
    loop_block: &LoopBlockFact,
    nodes: &BTreeSet<OriginExportKey>,
    block_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    loop_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    loop_headers: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &loop_block.loop_key,
        "loop_block.loop_key",
        diagnostics,
    );
    require_node(nodes, &loop_block.block, "loop_block.block", diagnostics);
    let Some(loop_function) = loop_functions.get(&loop_block.loop_key) else {
        push_error(
            diagnostics,
            TraceValidationError::LoopBlockWithoutLoop {
                loop_key: loop_block.loop_key.clone(),
            },
        );
        return;
    };
    validate_block_owner(
        "loop_block.block",
        loop_function,
        &loop_block.block,
        block_functions,
        diagnostics,
    );
    if loop_block.role == LoopBlockRole::Header
        && let Some(header_block) = loop_headers.get(&loop_block.loop_key)
        && header_block != &loop_block.block
    {
        push_error(
            diagnostics,
            TraceValidationError::LoopHeaderRoleMismatch {
                loop_key: loop_block.loop_key.clone(),
                header_block: header_block.clone(),
                role_block: loop_block.block.clone(),
            },
        );
    }
}

fn validate_instruction_block(
    instruction_block: &InstructionBlockFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
    block_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &instruction_block.instruction,
        "instruction_block.instruction",
        diagnostics,
    );
    require_node(
        nodes,
        &instruction_block.block,
        "instruction_block.block",
        diagnostics,
    );
    let Some(instruction_function) = instruction_owners.get(&instruction_block.instruction) else {
        push_error(
            diagnostics,
            TraceValidationError::InstructionBlockWithoutInstruction {
                instruction: instruction_block.instruction.clone(),
            },
        );
        return;
    };
    validate_block_owner(
        "instruction_block.block",
        instruction_function,
        &instruction_block.block,
        block_functions,
        diagnostics,
    );
}

fn validate_instruction_extent(
    extent: &InstructionExtentFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &extent.instruction,
        "instruction_extent.instruction",
        diagnostics,
    );
    require_node(
        nodes,
        &extent.code_object,
        "instruction_extent.code_object",
        diagnostics,
    );
    if !instruction_owners.contains_key(&extent.instruction) {
        push_error(
            diagnostics,
            TraceValidationError::InstructionExtentWithoutInstruction {
                instruction: extent.instruction.clone(),
            },
        );
    }
    if !extent.pc_range.is_valid() {
        push_error(
            diagnostics,
            TraceValidationError::InvalidInstructionExtent {
                instruction: extent.instruction.clone(),
                reason: "PC range must be non-empty and ordered",
            },
        );
        return;
    }
    if extent.byte_len == 0 {
        push_error(
            diagnostics,
            TraceValidationError::InvalidInstructionExtent {
                instruction: extent.instruction.clone(),
                reason: "byte_len must be non-zero",
            },
        );
    }
    if extent.pc_range.end - extent.pc_range.start != extent.byte_len {
        push_error(
            diagnostics,
            TraceValidationError::InvalidInstructionExtent {
                instruction: extent.instruction.clone(),
                reason: "byte_len must equal pc_range length",
            },
        );
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
    validate_loop_derivation(&membership.loop_key, &membership.derived_from, diagnostics);
}

fn validate_loop_derivation(
    loop_key: &OriginExportKey,
    derivation: &LoopDerivation,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    if let LoopDerivation::NaturalLoopAnalysis { cfg_hash } = derivation
        && cfg_hash.trim().is_empty()
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyLoopCfgHash {
                loop_key: loop_key.clone(),
            },
        );
    }
}

fn validate_block_owner(
    role: &'static str,
    function: &OriginExportKey,
    block: &OriginExportKey,
    block_functions: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    match block_functions.get(block) {
        Some(block_function) if block_function == function => {}
        Some(block_function) => push_error(
            diagnostics,
            TraceValidationError::BlockFunctionMismatch {
                role,
                function: function.clone(),
                block: block.clone(),
                block_function: block_function.clone(),
            },
        ),
        None => push_error(
            diagnostics,
            TraceValidationError::BlockFactMissing {
                role,
                block: block.clone(),
            },
        ),
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

fn validate_display_name(
    display_name: &DisplayNameFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &display_name.subject,
        "display_name.subject",
        diagnostics,
    );
    if display_name.name.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyDisplayName {
                subject: display_name.subject.clone(),
            },
        );
    }
}

fn validate_value_property(
    value_property: &ValuePropertyFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &value_property.subject,
        "value_property.subject",
        diagnostics,
    );
    if matches!(
        &value_property.property,
        ValueProperty::KnownUnsignedWidth { bits: 0 }
    ) {
        push_error(
            diagnostics,
            TraceValidationError::InvalidValueProperty {
                subject: value_property.subject.clone(),
                reason: "known unsigned width must be non-zero",
            },
        );
    }
    if let Some(reason) = &value_property.reason
        && reason.as_str().trim().is_empty()
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyValuePropertyReason {
                subject: value_property.subject.clone(),
            },
        );
    }
}

fn validate_source_file(
    source_file: &SourceFileFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &source_file.file_key,
        "source_file.file_key",
        diagnostics,
    );
    if source_file.uri.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptySourceFileField {
                file: source_file.file_key.clone(),
                field: "uri",
            },
        );
    }
    if source_file.display_name.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptySourceFileField {
                file: source_file.file_key.clone(),
                field: "display_name",
            },
        );
    }
    if source_file.content_hash.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptySourceFileField {
                file: source_file.file_key.clone(),
                field: "content_hash",
            },
        );
    }
}

fn validate_source_span(
    source_span: &SourceSpanFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &source_span.origin,
        "source_span.origin",
        diagnostics,
    );
    require_node(nodes, &source_span.file, "source_span.file", diagnostics);
    if source_span.start_byte > source_span.end_byte
        || (source_span.start_line, source_span.start_column)
            > (source_span.end_line, source_span.end_column)
    {
        push_error(
            diagnostics,
            TraceValidationError::InvalidSourceSpanRange {
                origin: source_span.origin.clone(),
            },
        );
    }
}

fn validate_code_object(
    code_object: &CodeObjectFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &code_object.code_object,
        "code_object.code_object",
        diagnostics,
    );
    if let Some(owner) = &code_object.owner_function_or_contract {
        require_node(nodes, owner, "code_object.owner", diagnostics);
    }
    if code_object.target.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyCodeObjectTarget {
                code_object: code_object.code_object.clone(),
            },
        );
    }
    if code_object
        .code_hash
        .as_deref()
        .is_some_and(|hash| hash.trim().is_empty())
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyCodeObjectHash {
                code_object: code_object.code_object.clone(),
            },
        );
    }
}

fn validate_function(
    function: &FunctionFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &function.function, "function.function", diagnostics);
    if function.name.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyFunctionName {
                function: function.function.clone(),
            },
        );
    }
    if let Some(source_origin) = &function.source_origin {
        require_node(nodes, source_origin, "function.source_origin", diagnostics);
    }
    if let Some(code_object) = &function.code_object {
        require_node(nodes, code_object, "function.code_object", diagnostics);
    }
}

fn validate_lexical_scope(
    scope: &LexicalScopeFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &scope.scope, "lexical_scope.scope", diagnostics);
    if let Some(parent) = &scope.parent {
        require_node(nodes, parent, "lexical_scope.parent", diagnostics);
    }
    require_node(
        nodes,
        &scope.function,
        "lexical_scope.function",
        diagnostics,
    );
    if let Some(source_origin) = &scope.source_origin {
        require_node(
            nodes,
            source_origin,
            "lexical_scope.source_origin",
            diagnostics,
        );
    }
}

fn validate_type(
    ty: &TypeFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &ty.ty, "type.ty", diagnostics);
    if ty
        .name
        .as_deref()
        .is_some_and(|name| name.trim().is_empty())
    {
        push_error(
            diagnostics,
            TraceValidationError::EmptyTypeName { ty: ty.ty.clone() },
        );
    }
    if ty.bit_width == Some(0) {
        push_error(
            diagnostics,
            TraceValidationError::InvalidTypeBitWidth { ty: ty.ty.clone() },
        );
    }
    for field in &ty.fields {
        require_node(nodes, &field.ty, "type.field.ty", diagnostics);
        if field.name.trim().is_empty() {
            push_error(
                diagnostics,
                TraceValidationError::EmptyTypeFieldName { ty: ty.ty.clone() },
            );
        }
    }
}

fn validate_variable(
    variable: &VariableFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &variable.variable, "variable.variable", diagnostics);
    require_node(nodes, &variable.ty, "variable.ty", diagnostics);
    require_node(
        nodes,
        &variable.declaration_origin,
        "variable.declaration_origin",
        diagnostics,
    );
    if let Some(scope) = &variable.scope {
        require_node(nodes, scope, "variable.scope", diagnostics);
    }
    if variable.name.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyVariableName {
                variable: variable.variable.clone(),
            },
        );
    }
}

fn validate_location_range(
    location_range: &LocationRangeFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &location_range.subject,
        "location_range.subject",
        diagnostics,
    );
    require_node(
        nodes,
        &location_range.code_object,
        "location_range.code_object",
        diagnostics,
    );
    if !location_range.pc_range.is_valid() {
        push_error(
            diagnostics,
            TraceValidationError::InvalidPcRange {
                subject: location_range.subject.clone(),
            },
        );
    }
    validate_value_location(
        &location_range.subject,
        &location_range.location,
        nodes,
        diagnostics,
    );
}

fn validate_static_gas(
    gas: &StaticGasFact,
    nodes: &BTreeSet<OriginExportKey>,
    instruction_owners: &BTreeMap<OriginExportKey, OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &gas.instruction,
        "static_gas.instruction",
        diagnostics,
    );
    if !instruction_owners.contains_key(&gas.instruction) {
        push_error(
            diagnostics,
            TraceValidationError::StaticGasWithoutInstruction {
                instruction: gas.instruction.clone(),
            },
        );
    }
    if gas.schedule.as_str().trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyGasSchedule {
                subject: gas.instruction.clone(),
            },
        );
    }
}

fn validate_dynamic_gas_step(
    step: &DynamicGasStepFact,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &step.code_object,
        "dynamic_gas_step.code_object",
        diagnostics,
    );
    if let Some(instruction) = &step.instruction {
        require_node(
            nodes,
            instruction,
            "dynamic_gas_step.instruction",
            diagnostics,
        );
    }
    if step.trace_id.trim().is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::EmptyDynamicGasTraceId {
                code_object: step.code_object.clone(),
            },
        );
    }
    if step.gas_after > step.gas_before {
        push_error(
            diagnostics,
            TraceValidationError::InvalidDynamicGasStep {
                code_object: step.code_object.clone(),
                reason: "gas_after must not exceed gas_before",
            },
        );
    } else if step.gas_before - step.gas_after != step.gas_cost {
        push_error(
            diagnostics,
            TraceValidationError::InvalidDynamicGasStep {
                code_object: step.code_object.clone(),
                reason: "gas_cost must equal gas_before - gas_after",
            },
        );
    }
}

fn validate_shape_policies<'a>(
    policies: Vec<&'a ShapePolicyFact>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) -> BTreeMap<ShapePolicyId, &'a ShapePolicyFact> {
    let mut by_id = BTreeMap::new();
    for policy in policies {
        if policy.schema_version == 0 {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapePolicy {
                    policy: policy.policy.clone(),
                    reason: "schema_version must be non-zero",
                },
            );
        }
        if policy.level.trim().is_empty() {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapePolicy {
                    policy: policy.policy.clone(),
                    reason: "level must not be empty",
                },
            );
        }
        if policy.dimensions.is_empty() {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapePolicy {
                    policy: policy.policy.clone(),
                    reason: "dimensions must not be empty",
                },
            );
        }
        let mut dimensions = BTreeSet::new();
        for dimension in &policy.dimensions {
            if !dimensions.insert(*dimension) {
                push_error(
                    diagnostics,
                    TraceValidationError::InvalidShapePolicy {
                        policy: policy.policy.clone(),
                        reason: "dimensions must not contain duplicates",
                    },
                );
            }
        }
        if let Some(existing) = by_id.insert(policy.policy.clone(), policy)
            && existing != policy
        {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapePolicy {
                    policy: policy.policy.clone(),
                    reason: "policy id is reused for different policies",
                },
            );
        }
        if !policy.level.trim().is_empty() {
            let level = ShapeLevel::new(policy.level.clone(), "shape level")
                .expect("non-empty shape level must be constructible");
            let expected_policy = ShapeHashPolicy {
                schema_version: policy.schema_version,
                algorithm: policy.algorithm,
                level,
                dimensions: policy.dimensions.iter().copied().collect(),
                view_mode: policy.view_mode,
                cycle_policy: policy.cycle_policy,
            };
            if expected_policy.policy_id() != policy.policy {
                push_error(
                    diagnostics,
                    TraceValidationError::InvalidShapePolicy {
                        policy: policy.policy.clone(),
                        reason: "policy id must match policy fields",
                    },
                );
            }
        }
    }
    by_id
}

fn validate_shape_node_hash(
    hash: &ShapeNodeHashFact,
    nodes: &BTreeSet<OriginExportKey>,
    policies: &BTreeMap<ShapePolicyId, &ShapePolicyFact>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(nodes, &hash.node, "shape_node_hash.node", diagnostics);
    require_node(
        nodes,
        &hash.graph.owner,
        "shape_node_hash.graph.owner",
        diagnostics,
    );
    if let Some(policy) = require_shape_policy(&hash.policy, policies, diagnostics) {
        validate_dimension_digests(policy, &hash.local, "shape_node_hash.local", diagnostics);
        validate_dimension_digests(policy, &hash.tree, "shape_node_hash.tree", diagnostics);
        if let Some(component) = &hash.component {
            validate_dimension_digests(policy, component, "shape_node_hash.component", diagnostics);
        }
    }
}

fn validate_shape_component_hash(
    hash: &ShapeComponentHashFact,
    nodes: &BTreeSet<OriginExportKey>,
    policies: &BTreeMap<ShapePolicyId, &ShapePolicyFact>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &hash.graph.owner,
        "shape_component_hash.graph.owner",
        diagnostics,
    );
    for member in &hash.members {
        require_node(nodes, member, "shape_component_hash.member", diagnostics);
    }
    if let Some(policy) = require_shape_policy(&hash.policy, policies, diagnostics) {
        validate_dimension_digests(
            policy,
            &hash.digests,
            "shape_component_hash.digests",
            diagnostics,
        );
    }
}

fn validate_shape_graph_hash(
    hash: &ShapeGraphHashFact,
    nodes: &BTreeSet<OriginExportKey>,
    policies: &BTreeMap<ShapePolicyId, &ShapePolicyFact>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    require_node(
        nodes,
        &hash.graph.owner,
        "shape_graph_hash.graph.owner",
        diagnostics,
    );
    if let Some(policy) = require_shape_policy(&hash.policy, policies, diagnostics) {
        validate_dimension_digests(
            policy,
            &hash.digests,
            "shape_graph_hash.digests",
            diagnostics,
        );
    }
}

fn require_shape_policy<'a>(
    policy: &ShapePolicyId,
    policies: &'a BTreeMap<ShapePolicyId, &ShapePolicyFact>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) -> Option<&'a ShapePolicyFact> {
    match policies.get(policy).copied() {
        Some(policy) => Some(policy),
        None => {
            push_error(
                diagnostics,
                TraceValidationError::MissingShapePolicy {
                    policy: policy.clone(),
                },
            );
            None
        }
    }
}

fn validate_dimension_digests(
    policy: &ShapePolicyFact,
    digests: &DimensionDigests,
    role: &'static str,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    if digests.values.is_empty() {
        push_error(
            diagnostics,
            TraceValidationError::InvalidShapeDigestSet {
                policy: policy.policy.clone(),
                role,
                reason: "digest set must not be empty",
            },
        );
        return;
    }
    for dimension in &policy.dimensions {
        if !digests.values.contains_key(dimension) {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapeDigestSet {
                    policy: policy.policy.clone(),
                    role,
                    reason: "digest set is missing a policy dimension",
                },
            );
        }
    }
    let policy_dimensions = policy.dimensions.iter().copied().collect::<BTreeSet<_>>();
    for dimension in digests.values.keys() {
        if !policy_dimensions.contains(dimension) {
            push_error(
                diagnostics,
                TraceValidationError::InvalidShapeDigestSet {
                    policy: policy.policy.clone(),
                    role,
                    reason: "digest set contains a dimension outside the policy",
                },
            );
        }
    }
}

fn validate_value_location(
    subject: &OriginExportKey,
    location: &ValueLocation,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    match location {
        ValueLocation::SsaValue { value } => {
            require_node(
                nodes,
                value,
                "location_range.location.ssa_value",
                diagnostics,
            );
        }
        ValueLocation::Register { name } if name.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyLocationField {
                    subject: subject.clone(),
                    field: "register.name",
                },
            );
        }
        ValueLocation::EvmMemory { offset, length }
        | ValueLocation::EvmCalldata { offset, length } => {
            validate_location_expr(subject, offset, nodes, diagnostics);
            if let Some(length) = length {
                validate_location_expr(subject, length, nodes, diagnostics);
            }
        }
        ValueLocation::EvmStorage { slot, .. } => {
            validate_location_expr(subject, slot, nodes, diagnostics);
        }
        ValueLocation::Unknown { reason } if reason.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyLocationField {
                    subject: subject.clone(),
                    field: "unknown.reason",
                },
            );
        }
        _ => {}
    }
}

fn validate_location_expr(
    subject: &OriginExportKey,
    expr: &LocationExpr,
    nodes: &BTreeSet<OriginExportKey>,
    diagnostics: &mut Vec<TraceValidationDiagnostic>,
) {
    match expr {
        LocationExpr::Origin { origin } => {
            require_node(nodes, origin, "location_expr.origin", diagnostics);
        }
        LocationExpr::Unknown { reason } if reason.trim().is_empty() => {
            push_error(
                diagnostics,
                TraceValidationError::EmptyLocationField {
                    subject: subject.clone(),
                    field: "location_expr.unknown.reason",
                },
            );
        }
        _ => {}
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
    EmptyBlockName {
        block: OriginExportKey,
    },
    BlockHasMultipleFunctions {
        block: OriginExportKey,
        first_function: OriginExportKey,
        second_function: OriginExportKey,
    },
    DuplicateBlockOrdinal {
        function: OriginExportKey,
        ordinal: u32,
        first_block: OriginExportKey,
        second_block: OriginExportKey,
    },
    BlockFactMissing {
        role: &'static str,
        block: OriginExportKey,
    },
    BlockFunctionMismatch {
        role: &'static str,
        function: OriginExportKey,
        block: OriginExportKey,
        block_function: OriginExportKey,
    },
    LoopBlockWithoutLoop {
        loop_key: OriginExportKey,
    },
    DuplicateLoopBlock {
        loop_key: OriginExportKey,
        block: OriginExportKey,
    },
    LoopHeaderRoleMismatch {
        loop_key: OriginExportKey,
        header_block: OriginExportKey,
        role_block: OriginExportKey,
    },
    InvalidLoopBlockRoles {
        loop_key: OriginExportKey,
        reason: &'static str,
    },
    InstructionBlockWithoutInstruction {
        instruction: OriginExportKey,
    },
    DuplicateInstructionBlock {
        instruction: OriginExportKey,
        first_block: OriginExportKey,
        second_block: OriginExportKey,
    },
    InstructionExtentWithoutInstruction {
        instruction: OriginExportKey,
    },
    InvalidInstructionExtent {
        instruction: OriginExportKey,
        reason: &'static str,
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
    EmptyDisplayName {
        subject: OriginExportKey,
    },
    InvalidValueProperty {
        subject: OriginExportKey,
        reason: &'static str,
    },
    EmptyValuePropertyReason {
        subject: OriginExportKey,
    },
    EmptySourceFileField {
        file: OriginExportKey,
        field: &'static str,
    },
    InvalidSourceSpanRange {
        origin: OriginExportKey,
    },
    EmptyCodeObjectTarget {
        code_object: OriginExportKey,
    },
    EmptyCodeObjectHash {
        code_object: OriginExportKey,
    },
    EmptyFunctionName {
        function: OriginExportKey,
    },
    EmptyTypeName {
        ty: OriginExportKey,
    },
    InvalidTypeBitWidth {
        ty: OriginExportKey,
    },
    EmptyTypeFieldName {
        ty: OriginExportKey,
    },
    EmptyVariableName {
        variable: OriginExportKey,
    },
    InvalidPcRange {
        subject: OriginExportKey,
    },
    EmptyLocationField {
        subject: OriginExportKey,
        field: &'static str,
    },
    StaticGasWithoutInstruction {
        instruction: OriginExportKey,
    },
    EmptyDynamicGasTraceId {
        code_object: OriginExportKey,
    },
    InvalidDynamicGasStep {
        code_object: OriginExportKey,
        reason: &'static str,
    },
    MissingShapePolicy {
        policy: ShapePolicyId,
    },
    InvalidShapePolicy {
        policy: ShapePolicyId,
        reason: &'static str,
    },
    InvalidShapeDigestSet {
        policy: ShapePolicyId,
        role: &'static str,
        reason: &'static str,
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
            Self::EmptyBlockName { block } => {
                write!(f, "block {} has an empty name", block.display_label())
            }
            Self::BlockHasMultipleFunctions {
                block,
                first_function,
                second_function,
            } => write!(
                f,
                "block {} belongs to multiple functions: {} and {}",
                block.display_label(),
                first_function.display_label(),
                second_function.display_label()
            ),
            Self::DuplicateBlockOrdinal {
                function,
                ordinal,
                first_block,
                second_block,
            } => write!(
                f,
                "function {} has multiple blocks at ordinal {ordinal}: {} and {}",
                function.display_label(),
                first_block.display_label(),
                second_block.display_label()
            ),
            Self::BlockFactMissing { role, block } => write!(
                f,
                "{role} references {} but no block fact defines it",
                block.display_label()
            ),
            Self::BlockFunctionMismatch {
                role,
                function,
                block,
                block_function,
            } => write!(
                f,
                "{role} references block {} owned by {}, not {}",
                block.display_label(),
                block_function.display_label(),
                function.display_label()
            ),
            Self::LoopBlockWithoutLoop { loop_key } => write!(
                f,
                "loop block references {} but no loop fact defines it",
                loop_key.display_label()
            ),
            Self::DuplicateLoopBlock { loop_key, block } => write!(
                f,
                "loop {} contains duplicate block {}",
                loop_key.display_label(),
                block.display_label()
            ),
            Self::LoopHeaderRoleMismatch {
                loop_key,
                header_block,
                role_block,
            } => write!(
                f,
                "loop {} declares header {} but assigns header role to {}",
                loop_key.display_label(),
                header_block.display_label(),
                role_block.display_label()
            ),
            Self::InvalidLoopBlockRoles { loop_key, reason } => write!(
                f,
                "loop {} has invalid block roles: {reason}",
                loop_key.display_label()
            ),
            Self::InstructionBlockWithoutInstruction { instruction } => write!(
                f,
                "instruction block references {} but no instruction fact defines it",
                instruction.display_label()
            ),
            Self::DuplicateInstructionBlock {
                instruction,
                first_block,
                second_block,
            } => write!(
                f,
                "instruction {} belongs to multiple blocks: {} and {}",
                instruction.display_label(),
                first_block.display_label(),
                second_block.display_label()
            ),
            Self::InstructionExtentWithoutInstruction { instruction } => write!(
                f,
                "instruction extent references {} but no instruction fact defines it",
                instruction.display_label()
            ),
            Self::InvalidInstructionExtent {
                instruction,
                reason,
            } => write!(
                f,
                "instruction extent for {} is invalid: {reason}",
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
            Self::EmptyDisplayName { subject } => {
                write!(f, "display name for {} is empty", subject.display_label())
            }
            Self::InvalidValueProperty { subject, reason } => write!(
                f,
                "value property for {} is invalid: {reason}",
                subject.display_label()
            ),
            Self::EmptyValuePropertyReason { subject } => write!(
                f,
                "value property for {} has an empty reason",
                subject.display_label()
            ),
            Self::EmptySourceFileField { file, field } => write!(
                f,
                "source file {} has an empty {field}",
                file.display_label()
            ),
            Self::InvalidSourceSpanRange { origin } => write!(
                f,
                "source span for {} has an invalid range",
                origin.display_label()
            ),
            Self::EmptyCodeObjectTarget { code_object } => write!(
                f,
                "code object {} has an empty target",
                code_object.display_label()
            ),
            Self::EmptyCodeObjectHash { code_object } => write!(
                f,
                "code object {} has an empty code hash",
                code_object.display_label()
            ),
            Self::EmptyFunctionName { function } => {
                write!(f, "function {} has an empty name", function.display_label())
            }
            Self::EmptyTypeName { ty } => {
                write!(f, "type {} has an empty name", ty.display_label())
            }
            Self::InvalidTypeBitWidth { ty } => {
                write!(f, "type {} has an invalid bit width", ty.display_label())
            }
            Self::EmptyTypeFieldName { ty } => {
                write!(f, "type {} has an empty field name", ty.display_label())
            }
            Self::EmptyVariableName { variable } => {
                write!(f, "variable {} has an empty name", variable.display_label())
            }
            Self::InvalidPcRange { subject } => write!(
                f,
                "location range for {} has an invalid PC range",
                subject.display_label()
            ),
            Self::EmptyLocationField { subject, field } => write!(
                f,
                "location range for {} has an empty {field}",
                subject.display_label()
            ),
            Self::StaticGasWithoutInstruction { instruction } => write!(
                f,
                "static gas references {} but no instruction fact defines it",
                instruction.display_label()
            ),
            Self::EmptyDynamicGasTraceId { code_object } => write!(
                f,
                "dynamic gas step for {} has an empty trace_id",
                code_object.display_label()
            ),
            Self::InvalidDynamicGasStep {
                code_object,
                reason,
            } => write!(
                f,
                "dynamic gas step for {} is invalid: {reason}",
                code_object.display_label()
            ),
            Self::MissingShapePolicy { policy } => {
                write!(f, "shape hash references unknown policy {policy}")
            }
            Self::InvalidShapePolicy { policy, reason } => {
                write!(f, "shape policy {policy} is invalid: {reason}")
            }
            Self::InvalidShapeDigestSet {
                policy,
                role,
                reason,
            } => {
                write!(
                    f,
                    "shape digest set {role} for policy {policy} is invalid: {reason}"
                )
            }
        }
    }
}

impl std::error::Error for TraceValidationError {}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use shape_address::{
        DimensionDigests, ShapeCyclePolicy, ShapeDigest, ShapeDimension, ShapeGraphKey,
        ShapeHashPolicy, ShapeViewMode,
    };

    use crate::{
        BlockFact, CategorySource, CfgEdgeFact, CfgEdgeKind, CodeObjectFact, CodeObjectKind,
        CompilerPhase, DisplayNameFact, DisplayNameKind, DynamicGasStepFact, EvmSchedule,
        FunctionFact, GasConfidence, GasCostFact, GasKind, GasSource, InlineContextFact,
        InstructionBlockFact, InstructionCategory, InstructionCategoryFact, InstructionExtentFact,
        InstructionFact, LoopBlockFact, LoopBlockRole, LoopConfidence, LoopDerivation, LoopFact,
        LoopMembershipFact, OpcodeCategory, OpcodeFact, OriginEdgeFact, OriginEdgeLabel,
        OriginNodeFact, OriginNodeKind, PcRange, ShapeComponentHashFact, ShapeGraphHashFact,
        ShapeNodeHashFact, ShapePolicyFact, SourceFileFact, SourceSpanFact, StaticGasFact,
        StorageFact, StorageLocation, StorageReason, TraceFact, TraceValidationDiagnostic,
        TraceValidationError, TraceValidationLevel, TraceValidationWarning, TraceValidator,
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

    fn shape_policy() -> ShapeHashPolicy {
        ShapeHashPolicy::with_dimensions(
            "hir",
            [ShapeDimension::Structure],
            ShapeViewMode::IdentityBound,
            ShapeCyclePolicy::Reject,
        )
        .unwrap()
    }

    fn shape_digests(hex_digit: char) -> DimensionDigests {
        let mut digests = DimensionDigests::default();
        digests.insert(
            ShapeDimension::Structure,
            ShapeDigest::new(hex_digit.to_string().repeat(64)).unwrap(),
        );
        digests
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
    fn validator_accepts_cfg_loop_block_and_extent_facts() {
        let function = key("mir.function", "fib", "recv");
        let code_object = key("code.object", "fib", "runtime");
        let header = key("mir.block", "fib", "block:0");
        let latch = key("mir.block", "fib", "block:1");
        let loop_key = key("mir.loop", "fib", "loop:0");
        let instruction = key("mir.inst", "fib", "inst:0");

        let facts = vec![
            node("mir.function", "fib", "recv"),
            node("code.object", "fib", "runtime"),
            node("mir.block", "fib", "block:0"),
            node("mir.block", "fib", "block:1"),
            node("mir.loop", "fib", "loop:0"),
            node("mir.inst", "fib", "inst:0"),
            TraceFact::Block(BlockFact::new(
                header.clone(),
                function.clone(),
                CompilerPhase::Mir,
                0,
                Some("loop_header".to_string()),
            )),
            TraceFact::Block(BlockFact::new(
                latch.clone(),
                function.clone(),
                CompilerPhase::Mir,
                1,
                Some("loop_latch".to_string()),
            )),
            TraceFact::CfgEdge(CfgEdgeFact::new(
                function.clone(),
                header.clone(),
                latch.clone(),
                CfgEdgeKind::Fallthrough,
                None,
            )),
            TraceFact::CfgEdge(CfgEdgeFact::new(
                function.clone(),
                latch.clone(),
                header.clone(),
                CfgEdgeKind::Backedge,
                None,
            )),
            TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function.clone(),
                CompilerPhase::Mir,
                header.clone(),
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: "cfg:abc".to_string(),
                },
                LoopConfidence::MirCfg,
            )),
            TraceFact::LoopBlock(LoopBlockFact::new(
                loop_key.clone(),
                header,
                LoopBlockRole::Header,
            )),
            TraceFact::LoopBlock(LoopBlockFact::new(
                loop_key,
                latch.clone(),
                LoopBlockRole::Latch,
            )),
            TraceFact::Instruction(InstructionFact::new(instruction.clone(), function, 0, "br")),
            TraceFact::InstructionBlock(InstructionBlockFact::new(
                instruction.clone(),
                latch,
                CompilerPhase::Mir,
            )),
            TraceFact::InstructionExtent(InstructionExtentFact::new(
                instruction,
                code_object,
                PcRange::new(4, 5),
                1,
            )),
        ];

        assert!(TraceValidator::validate(&facts).is_ok());
    }

    #[test]
    fn validator_rejects_duplicate_block_ordinals() {
        let function = key("mir.function", "fib", "recv");
        let first = key("mir.block", "fib", "block:0");
        let second = key("mir.block", "fib", "block:1");
        let facts = vec![
            node("mir.function", "fib", "recv"),
            node("mir.block", "fib", "block:0"),
            node("mir.block", "fib", "block:1"),
            TraceFact::Block(BlockFact::new(
                first.clone(),
                function.clone(),
                CompilerPhase::Mir,
                0,
                None,
            )),
            TraceFact::Block(BlockFact::new(
                second.clone(),
                function.clone(),
                CompilerPhase::Mir,
                0,
                None,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::DuplicateBlockOrdinal {
                function,
                ordinal: 0,
                first_block: first,
                second_block: second,
            })
        );
    }

    #[test]
    fn validator_rejects_loop_without_header_role() {
        let function = key("mir.function", "fib", "recv");
        let block = key("mir.block", "fib", "block:0");
        let loop_key = key("mir.loop", "fib", "loop:0");
        let facts = vec![
            node("mir.function", "fib", "recv"),
            node("mir.block", "fib", "block:0"),
            node("mir.loop", "fib", "loop:0"),
            TraceFact::Block(BlockFact::new(
                block.clone(),
                function.clone(),
                CompilerPhase::Mir,
                0,
                None,
            )),
            TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function,
                CompilerPhase::Mir,
                block.clone(),
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: "cfg:abc".to_string(),
                },
                LoopConfidence::MirCfg,
            )),
            TraceFact::LoopBlock(LoopBlockFact::new(
                loop_key.clone(),
                block,
                LoopBlockRole::Body,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidLoopBlockRoles {
                loop_key,
                reason: "loop must have exactly one header block role",
            })
        );
    }

    #[test]
    fn validator_rejects_instruction_extent_mismatch() {
        let function = key("bytecode.function", "fib", "runtime");
        let code_object = key("code.object", "fib", "runtime");
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("bytecode.function", "fib", "runtime"),
            node("code.object", "fib", "runtime"),
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "PUSH1",
            )),
            TraceFact::InstructionExtent(InstructionExtentFact::new(
                instruction.clone(),
                code_object,
                PcRange::new(0, 2),
                1,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidInstructionExtent {
                instruction,
                reason: "byte_len must equal pc_range length",
            })
        );
    }

    #[test]
    fn validator_rejects_bad_loop_cfg_hash() {
        let function = key("mir.function", "fib", "recv");
        let block = key("mir.block", "fib", "block:0");
        let loop_key = key("mir.loop", "fib", "loop:0");
        let facts = vec![
            node("mir.function", "fib", "recv"),
            node("mir.block", "fib", "block:0"),
            node("mir.loop", "fib", "loop:0"),
            TraceFact::Block(BlockFact::new(
                block.clone(),
                function.clone(),
                CompilerPhase::Mir,
                0,
                None,
            )),
            TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function,
                CompilerPhase::Mir,
                block,
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: " ".to_string(),
                },
                LoopConfidence::MirCfg,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::EmptyLoopCfgHash { loop_key })
        );
    }

    #[test]
    fn validator_rejects_bad_loop_membership_cfg_hash() {
        let function = key("mir.function", "fib", "recv");
        let loop_key = key("mir.loop", "fib", "loop:0");
        let instruction = key("mir.inst", "fib", "inst:0");
        let facts = vec![
            node("mir.function", "fib", "recv"),
            node("mir.loop", "fib", "loop:0"),
            node("mir.inst", "fib", "inst:0"),
            TraceFact::Instruction(InstructionFact::new(instruction.clone(), function, 0, "br")),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key.clone(),
                instruction,
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: String::new(),
                },
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::EmptyLoopCfgHash { loop_key })
        );
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
    fn validator_warns_on_duplicate_display_names() {
        let local = key("runtime.local", "fib", "local:b");
        let facts = vec![
            node("runtime.local", "fib", "local:b"),
            TraceFact::DisplayName(DisplayNameFact::new(
                local.clone(),
                DisplayNameKind::SourceLocal,
                "b",
            )),
            TraceFact::DisplayName(DisplayNameFact::new(
                local.clone(),
                DisplayNameKind::SourceLocal,
                "b2",
            )),
        ];

        let report = TraceValidator::check(&facts);
        assert_eq!(report.error_count(), 0);
        assert_eq!(report.warning_count(), 1);
        assert_eq!(
            report.diagnostics[0],
            TraceValidationDiagnostic::Warning(TraceValidationWarning::DuplicateDisplayName {
                subject: local,
                kind: DisplayNameKind::SourceLocal,
                first_name: "b".to_string(),
                second_name: "b2".to_string(),
            })
        );
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

    #[test]
    fn validator_accepts_debug_bundle_base_facts() {
        let source_file = key("source.file", "fib", "fib_demo.fe");
        let source_expr = key("hir.expr", "fib", "expr:while");
        let code_object = key("code.object", "fib", "runtime");
        let function = key("bytecode.function", "fib", "runtime");
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("source.file", "fib", "fib_demo.fe"),
            node("hir.expr", "fib", "expr:while"),
            node("code.object", "fib", "runtime"),
            node("bytecode.function", "fib", "runtime"),
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///fib_demo.fe",
                "fib_demo.fe",
                "fnv64:abcd",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source_expr.clone(),
                source_file,
                0,
                5,
                1,
                0,
                1,
                5,
            )),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                Some("fnv64:beef".to_string()),
            )),
            TraceFact::Function(FunctionFact::new(
                function.clone(),
                "runtime",
                Some(source_expr),
                Some(code_object),
            )),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "STOP",
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                instruction,
                EvmSchedule::new("cancun"),
                0,
                None,
            )),
        ];

        assert!(TraceValidator::validate(&facts).is_ok());
    }

    #[test]
    fn validator_warns_on_duplicate_static_gas_for_same_instruction_schedule_and_kind() {
        let function = key("bytecode.function", "fib", "runtime");
        let instruction = key("bytecode.pc", "fib", "pc:0");
        let facts = vec![
            node("bytecode.function", "fib", "runtime"),
            node("bytecode.pc", "fib", "pc:0"),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "STOP",
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                instruction.clone(),
                EvmSchedule::new("cancun"),
                0,
                None,
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                instruction.clone(),
                EvmSchedule::new("cancun"),
                1,
                None,
            )),
        ];

        let report = TraceValidator::check(&facts);
        assert_eq!(report.error_count(), 0);
        assert_eq!(report.warning_count(), 1);
        assert_eq!(
            report.diagnostics[0],
            TraceValidationDiagnostic::Warning(TraceValidationWarning::DuplicateStaticGas {
                instruction,
                schedule: "cancun".to_string(),
                dynamic_cost_kind: None,
                first_base_cost: 0,
                second_base_cost: 1,
            })
        );
    }

    #[test]
    fn validator_rejects_bad_dynamic_gas_arithmetic() {
        let code_object = key("code.object", "fib", "runtime");
        let facts = vec![
            node("code.object", "fib", "runtime"),
            TraceFact::DynamicGasStep(DynamicGasStepFact::new(
                "tx:1",
                0,
                code_object.clone(),
                0,
                None,
                10,
                6,
                3,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidDynamicGasStep {
                code_object,
                reason: "gas_cost must equal gas_before - gas_after",
            })
        );
    }

    #[test]
    fn validator_rejects_invalid_source_span_ranges() {
        let source_file = key("source.file", "fib", "fib_demo.fe");
        let source_expr = key("hir.expr", "fib", "expr:while");
        let facts = vec![
            node("source.file", "fib", "fib_demo.fe"),
            node("hir.expr", "fib", "expr:while"),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source_expr.clone(),
                source_file,
                10,
                5,
                2,
                0,
                1,
                0,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::InvalidSourceSpanRange {
                origin: source_expr,
            })
        );
    }

    #[test]
    fn validator_accepts_shape_hash_facts() {
        let graph_owner = key("hir.body", "fib", "body:0");
        let expr = key("hir.expr", "fib", "expr:0");
        let policy = shape_policy();
        let policy_id = policy.policy_id();
        let graph = ShapeGraphKey::new(graph_owner.clone(), "hir-body-shape").unwrap();
        let facts = vec![
            node("hir.body", "fib", "body:0"),
            node("hir.expr", "fib", "expr:0"),
            TraceFact::ShapePolicy(ShapePolicyFact::from_policy(&policy)),
            TraceFact::ShapeNodeHash(ShapeNodeHashFact::new(
                expr.clone(),
                graph.clone(),
                policy_id.clone(),
                shape_digests('a'),
                shape_digests('b'),
                None,
            )),
            TraceFact::ShapeComponentHash(ShapeComponentHashFact::new(
                graph.clone(),
                policy_id.clone(),
                0,
                vec![expr],
                shape_digests('c'),
            )),
            TraceFact::ShapeGraphHash(ShapeGraphHashFact::new(
                graph,
                policy_id,
                shape_digests('d'),
            )),
        ];

        assert!(TraceValidator::validate(&facts).is_ok());
    }

    #[test]
    fn validator_rejects_shape_policy_id_that_does_not_match_level() {
        let policy = shape_policy();
        let bad_policy_id = ShapeDigest::new("f".repeat(64)).unwrap();
        let fact = ShapePolicyFact::new(
            bad_policy_id.clone(),
            policy.schema_version,
            policy.algorithm,
            "mir",
            policy.dimensions.iter().copied().collect(),
            policy.view_mode,
            policy.cycle_policy,
        );

        assert_eq!(
            TraceValidator::validate(&[TraceFact::ShapePolicy(fact)]),
            Err(TraceValidationError::InvalidShapePolicy {
                policy: bad_policy_id,
                reason: "policy id must match policy fields",
            })
        );
    }

    #[test]
    fn validator_rejects_shape_hashes_for_unknown_nodes() {
        let graph_owner = key("hir.body", "fib", "body:0");
        let missing = key("hir.expr", "fib", "expr:0");
        let policy = shape_policy();
        let facts = vec![
            node("hir.body", "fib", "body:0"),
            TraceFact::ShapePolicy(ShapePolicyFact::from_policy(&policy)),
            TraceFact::ShapeNodeHash(ShapeNodeHashFact::new(
                missing.clone(),
                ShapeGraphKey::new(graph_owner, "hir-body-shape").unwrap(),
                policy.policy_id(),
                shape_digests('a'),
                shape_digests('b'),
                None,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::MissingOriginNode {
                role: "shape_node_hash.node",
                key: missing,
            })
        );
    }

    #[test]
    fn validator_rejects_shape_hashes_without_declared_policy() {
        let graph_owner = key("hir.body", "fib", "body:0");
        let expr = key("hir.expr", "fib", "expr:0");
        let policy_id = shape_policy().policy_id();
        let facts = vec![
            node("hir.body", "fib", "body:0"),
            node("hir.expr", "fib", "expr:0"),
            TraceFact::ShapeNodeHash(ShapeNodeHashFact::new(
                expr,
                ShapeGraphKey::new(graph_owner, "hir-body-shape").unwrap(),
                policy_id.clone(),
                shape_digests('a'),
                shape_digests('b'),
                None,
            )),
        ];

        assert_eq!(
            TraceValidator::validate(&facts),
            Err(TraceValidationError::MissingShapePolicy { policy: policy_id })
        );
    }
}
