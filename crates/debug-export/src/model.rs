use std::collections::{BTreeMap, BTreeSet, VecDeque};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use trace_facts::{
    CodeObjectFact, FunctionFact, GasCostFact, GasKind, InstructionCategory, InstructionFact,
    LocationRangeFact, OpcodeFact, OriginEdgeFact, OriginEdgeLabel, PcRange, SourceSpanFact,
    StaticGasFact, TraceDataSource, TraceFact, TraceSnapshot,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugBundle {
    pub trace_hash: String,
    pub compiler: CompilerInfo,
    pub sources: Vec<DebugSourceFile>,
    pub source_spans: Vec<DebugSourceSpan>,
    pub code_objects: Vec<DebugCodeObject>,
    pub functions: Vec<DebugFunction>,
    pub scopes: Vec<DebugScope>,
    pub variables: Vec<DebugVariable>,
    pub types: Vec<DebugType>,
    pub instructions: Vec<DebugInstruction>,
    pub locations: Vec<DebugLocationRange>,
    pub gas: Vec<DebugGasRecord>,
    pub attribution_policy: AttributionPolicyVersion,
}

impl DebugBundle {
    pub fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        build_debug_bundle(snapshot)
    }
}

pub fn build_debug_bundle(snapshot: &TraceSnapshot) -> DebugBundle {
    let index = DebugFactIndex::new(snapshot);
    DebugBundle {
        trace_hash: snapshot.trace_hash().to_string(),
        compiler: CompilerInfo {
            commit: snapshot.metadata().compiler_commit.clone(),
            target: snapshot.metadata().target.clone(),
            command: snapshot.metadata().command.clone(),
            flags: snapshot.metadata().flags.clone(),
            input_path: snapshot.metadata().input_path.clone(),
            data_source: match snapshot.metadata().data_source {
                TraceDataSource::Fixture => "fixture",
                TraceDataSource::CompilerEmitted => "compiler_emitted",
            }
            .to_string(),
        },
        sources: index.sources(),
        source_spans: index.source_spans(),
        code_objects: index.code_objects(),
        functions: index.functions(),
        scopes: index.scopes(),
        variables: index.variables(),
        types: index.types(),
        instructions: index.instructions(),
        locations: index.locations(),
        gas: index.gas(),
        attribution_policy: AttributionPolicyVersion::PrimarySourceV1,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompilerInfo {
    pub commit: String,
    pub target: String,
    pub command: Vec<String>,
    pub flags: Vec<String>,
    pub input_path: String,
    pub data_source: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugSourceFile {
    pub file_key: OriginExportKey,
    pub uri: String,
    pub display_name: String,
    pub content_hash: String,
    pub source_id: Option<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugSourceSpan {
    pub origin: OriginExportKey,
    pub file: OriginExportKey,
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugCodeObject {
    pub key: OriginExportKey,
    pub kind: String,
    pub owner_function_or_contract: Option<OriginExportKey>,
    pub target: String,
    pub code_hash: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugFunction {
    pub key: OriginExportKey,
    pub name: String,
    pub source_origin: Option<OriginExportKey>,
    pub code_object: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugScope {
    pub key: OriginExportKey,
    pub parent: Option<OriginExportKey>,
    pub function: OriginExportKey,
    pub source_origin: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugVariable {
    pub key: OriginExportKey,
    pub name: String,
    pub ty: OriginExportKey,
    pub declaration_origin: OriginExportKey,
    pub scope: Option<OriginExportKey>,
    pub storage_class: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugType {
    pub key: OriginExportKey,
    pub kind: String,
    pub name: Option<String>,
    pub bit_width: Option<u32>,
    pub fields: Vec<DebugTypeField>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugTypeField {
    pub name: String,
    pub ty: OriginExportKey,
    pub offset_bits: Option<u32>,
    pub width_bits: Option<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugInstruction {
    pub key: OriginExportKey,
    pub function: OriginExportKey,
    pub code_object: Option<OriginExportKey>,
    pub pc_range: PcRange,
    pub opcode_or_mnemonic: String,
    pub primary_source: Option<OriginExportKey>,
    pub all_origins: Vec<OriginExportKey>,
    pub classification: InstructionClassification,
    pub category: Option<InstructionCategory>,
    pub confidence: AttributionConfidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugLocationRange {
    pub subject: OriginExportKey,
    pub code_object: OriginExportKey,
    pub pc_range: PcRange,
    pub location: String,
    pub reason: String,
    pub confidence: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugGasRecord {
    pub instruction: OriginExportKey,
    pub schedule: String,
    pub gas: u64,
    pub kind: String,
    pub confidence: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AttributionPolicyVersion {
    PrimarySourceV1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AttributionConfidence {
    High,
    Ambiguous,
    Unmapped,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InstructionClassification {
    SourceMapped,
    Synthetic,
    Ambiguous,
    Unmapped,
}

struct DebugFactIndex<'a> {
    facts: &'a [TraceFact],
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    outgoing_edges: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    opcodes: BTreeMap<OriginExportKey, &'a OpcodeFact>,
    function_facts: BTreeMap<OriginExportKey, &'a FunctionFact>,
    function_code_objects: BTreeMap<OriginExportKey, OriginExportKey>,
    categories: BTreeMap<OriginExportKey, InstructionCategory>,
}

impl<'a> DebugFactIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut source_spans = BTreeMap::new();
        let mut outgoing_edges: BTreeMap<OriginExportKey, Vec<&OriginEdgeFact>> = BTreeMap::new();
        let mut opcodes = BTreeMap::new();
        let mut function_facts = BTreeMap::new();
        let mut function_code_objects = BTreeMap::new();
        let mut categories = BTreeMap::new();

        for fact in snapshot.facts() {
            match fact {
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                TraceFact::OriginEdge(edge) => {
                    outgoing_edges
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                }
                TraceFact::Opcode(opcode) => {
                    opcodes.insert(opcode.pc.clone(), opcode);
                }
                TraceFact::Function(function) => {
                    if let Some(code_object) = &function.code_object {
                        function_code_objects
                            .insert(function.function.clone(), code_object.clone());
                    }
                    function_facts.insert(function.function.clone(), function);
                }
                TraceFact::InstructionCategory(category) => {
                    categories.insert(category.instruction.clone(), category.category);
                }
                _ => {}
            }
        }

        Self {
            facts: snapshot.facts(),
            source_spans,
            outgoing_edges,
            opcodes,
            function_facts,
            function_code_objects,
            categories,
        }
    }

    fn sources(&self) -> Vec<DebugSourceFile> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::SourceFile(source) = fact {
                    Some(DebugSourceFile {
                        file_key: source.file_key.clone(),
                        uri: source.uri.clone(),
                        display_name: source.display_name.clone(),
                        content_hash: source.content_hash.clone(),
                        source_id: source.source_id,
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    fn source_spans(&self) -> Vec<DebugSourceSpan> {
        self.source_spans
            .values()
            .map(|span| DebugSourceSpan {
                origin: span.origin.clone(),
                file: span.file.clone(),
                start_byte: span.start_byte,
                end_byte: span.end_byte,
                start_line: span.start_line,
                start_column: span.start_column,
                end_line: span.end_line,
                end_column: span.end_column,
            })
            .collect()
    }

    fn code_objects(&self) -> Vec<DebugCodeObject> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::CodeObject(code_object) = fact {
                    Some(debug_code_object(code_object))
                } else {
                    None
                }
            })
            .collect()
    }

    fn functions(&self) -> Vec<DebugFunction> {
        let mut functions: BTreeMap<OriginExportKey, DebugFunction> = self
            .function_facts
            .values()
            .map(|function| {
                (
                    function.function.clone(),
                    DebugFunction {
                        key: function.function.clone(),
                        name: function.name.clone(),
                        source_origin: function.source_origin.clone(),
                        code_object: function.code_object.clone(),
                    },
                )
            })
            .collect();

        for fact in self.facts {
            if let TraceFact::Instruction(instruction) = fact {
                functions
                    .entry(instruction.function.clone())
                    .or_insert_with(|| DebugFunction {
                        key: instruction.function.clone(),
                        name: instruction.function.display_label(),
                        source_origin: None,
                        code_object: self
                            .function_code_objects
                            .get(&instruction.function)
                            .cloned(),
                    });
            }
        }

        functions.into_values().collect()
    }

    fn scopes(&self) -> Vec<DebugScope> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::LexicalScope(scope) = fact {
                    Some(DebugScope {
                        key: scope.scope.clone(),
                        parent: scope.parent.clone(),
                        function: scope.function.clone(),
                        source_origin: scope.source_origin.clone(),
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    fn variables(&self) -> Vec<DebugVariable> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::Variable(variable) = fact {
                    Some(DebugVariable {
                        key: variable.variable.clone(),
                        name: variable.name.clone(),
                        ty: variable.ty.clone(),
                        declaration_origin: variable.declaration_origin.clone(),
                        scope: variable.scope.clone(),
                        storage_class: format!("{:?}", variable.storage_class),
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    fn types(&self) -> Vec<DebugType> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::Type(ty) = fact {
                    Some(DebugType {
                        key: ty.ty.clone(),
                        kind: format!("{:?}", ty.kind),
                        name: ty.name.clone(),
                        bit_width: ty.bit_width,
                        fields: ty
                            .fields
                            .iter()
                            .map(|field| DebugTypeField {
                                name: field.name.clone(),
                                ty: field.ty.clone(),
                                offset_bits: field.offset_bits,
                                width_bits: field.width_bits,
                            })
                            .collect(),
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    fn instructions(&self) -> Vec<DebugInstruction> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::Instruction(instruction) = fact {
                    Some(self.debug_instruction(instruction))
                } else {
                    None
                }
            })
            .collect()
    }

    fn debug_instruction(&self, instruction: &InstructionFact) -> DebugInstruction {
        let source_candidates = self.source_candidates(&instruction.instruction);
        let primary_source = (source_candidates.len() == 1)
            .then(|| source_candidates.iter().next().cloned())
            .flatten();
        let classification = self.classification(&instruction.instruction, source_candidates.len());
        let confidence = match classification {
            InstructionClassification::SourceMapped => AttributionConfidence::High,
            InstructionClassification::Ambiguous => AttributionConfidence::Ambiguous,
            InstructionClassification::Synthetic | InstructionClassification::Unmapped => {
                AttributionConfidence::Unmapped
            }
        };

        DebugInstruction {
            key: instruction.instruction.clone(),
            function: instruction.function.clone(),
            code_object: self
                .function_code_objects
                .get(&instruction.function)
                .cloned(),
            pc_range: self.instruction_pc_range(instruction),
            opcode_or_mnemonic: instruction.mnemonic.clone(),
            primary_source,
            all_origins: source_candidates.into_iter().collect(),
            classification,
            category: self.categories.get(&instruction.instruction).copied(),
            confidence,
        }
    }

    fn instruction_pc_range(&self, instruction: &InstructionFact) -> PcRange {
        if instruction.instruction.kind() == "bytecode.pc"
            && let Some(start) = instruction
                .instruction
                .local_key()
                .strip_prefix("pc:")
                .and_then(|pc| pc.parse::<u32>().ok())
        {
            let byte_len = self
                .opcodes
                .get(&instruction.instruction)
                .map_or(1, |opcode| 1 + immediate_byte_len(opcode));
            return PcRange::new(start, start.saturating_add(byte_len));
        }
        PcRange::new(instruction.index, instruction.index.saturating_add(1))
    }

    fn source_candidates(&self, instruction: &OriginExportKey) -> BTreeSet<OriginExportKey> {
        let mut visited = BTreeSet::new();
        let mut candidates = BTreeSet::new();
        let mut queue = VecDeque::from([instruction.clone()]);

        while let Some(current) = queue.pop_front() {
            if !visited.insert(current.clone()) {
                continue;
            }
            if self.source_spans.contains_key(&current) {
                candidates.insert(current.clone());
            }
            for edge in self.outgoing_edges.get(&current).into_iter().flatten() {
                queue.push_back(edge.to.clone());
            }
        }

        candidates
    }

    fn classification(
        &self,
        instruction: &OriginExportKey,
        source_count: usize,
    ) -> InstructionClassification {
        match source_count {
            1 => InstructionClassification::SourceMapped,
            n if n > 1 => InstructionClassification::Ambiguous,
            _ if self.synthetic_edge_labels(instruction) => InstructionClassification::Synthetic,
            _ => InstructionClassification::Unmapped,
        }
    }

    fn synthetic_edge_labels(&self, instruction: &OriginExportKey) -> bool {
        self.outgoing_edges
            .get(instruction)
            .into_iter()
            .flatten()
            .any(|edge| {
                matches!(
                    edge.label,
                    OriginEdgeLabel::SyntheticFor
                        | OriginEdgeLabel::BackendPrepared
                        | OriginEdgeLabel::Unmapped
                )
            })
    }

    fn locations(&self) -> Vec<DebugLocationRange> {
        self.facts
            .iter()
            .filter_map(|fact| {
                if let TraceFact::LocationRange(location) = fact {
                    Some(debug_location(location))
                } else {
                    None
                }
            })
            .collect()
    }

    fn gas(&self) -> Vec<DebugGasRecord> {
        let mut gas = Vec::new();
        for fact in self.facts {
            match fact {
                TraceFact::StaticGas(static_gas) => gas.push(debug_static_gas(static_gas)),
                TraceFact::GasCost(gas_cost) if gas_cost.gas_kind == GasKind::OpcodeStatic => {
                    gas.push(debug_legacy_static_gas(gas_cost));
                }
                _ => {}
            }
        }
        gas
    }
}

fn debug_code_object(code_object: &CodeObjectFact) -> DebugCodeObject {
    DebugCodeObject {
        key: code_object.code_object.clone(),
        kind: format!("{:?}", code_object.kind),
        owner_function_or_contract: code_object.owner_function_or_contract.clone(),
        target: code_object.target.clone(),
        code_hash: code_object.code_hash.clone(),
    }
}

fn debug_location(location: &LocationRangeFact) -> DebugLocationRange {
    DebugLocationRange {
        subject: location.subject.clone(),
        code_object: location.code_object.clone(),
        pc_range: location.pc_range,
        location: format!("{:?}", location.location),
        reason: format!("{:?}", location.reason),
        confidence: format!("{:?}", location.confidence),
    }
}

fn debug_static_gas(static_gas: &StaticGasFact) -> DebugGasRecord {
    DebugGasRecord {
        instruction: static_gas.instruction.clone(),
        schedule: static_gas.schedule.to_string(),
        gas: static_gas.base_cost,
        kind: "opcode_static".to_string(),
        confidence: "conservative_static".to_string(),
    }
}

fn debug_legacy_static_gas(gas_cost: &GasCostFact) -> DebugGasRecord {
    DebugGasRecord {
        instruction: gas_cost.subject.clone(),
        schedule: gas_cost.schedule.to_string(),
        gas: gas_cost.gas,
        kind: format!("{:?}", gas_cost.gas_kind),
        confidence: format!("{:?}", gas_cost.confidence),
    }
}

fn immediate_byte_len(opcode: &OpcodeFact) -> u32 {
    opcode
        .immediate
        .as_deref()
        .and_then(|value| value.strip_prefix("0x"))
        .map_or(0, |hex| (hex.len() / 2) as u32)
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{
        CodeObjectKind, EvmSchedule, FunctionFact, InstructionFact, OriginEdgeFact,
        OriginEdgeLabel, OriginNodeFact, OriginNodeKind, SourceFileFact, SourceSpanFact,
        StaticGasFact, TraceBundle, TraceFact, TraceMetadata, TraceSnapshot, TraceValidator,
    };

    use super::{AttributionConfidence, DebugBundle, InstructionClassification};

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        let kind = OriginNodeKind::new(key.kind());
        TraceFact::OriginNode(OriginNodeFact::new(key, kind))
    }

    #[test]
    fn debug_bundle_maps_instruction_to_primary_source_without_inventing_identity() {
        let source_file = key("source.file", "demo", "src/main.fe");
        let source_expr = key("hir.expr", "demo", "expr:add");
        let code_object = key("code.object", "demo", "runtime");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:4");

        let facts = vec![
            node(source_file.clone()),
            node(source_expr.clone()),
            node(code_object.clone()),
            node(function.clone()),
            node(instruction.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///src/main.fe",
                "src/main.fe",
                "fnv64:1234",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source_expr.clone(),
                source_file,
                10,
                13,
                1,
                10,
                1,
                13,
            )),
            TraceFact::CodeObject(trace_facts::CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                Some("fnv64:beef".to_string()),
            )),
            TraceFact::Function(FunctionFact::new(
                function.clone(),
                "runtime",
                None,
                Some(code_object.clone()),
            )),
            TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function,
                0,
                "ADD",
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                source_expr.clone(),
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                instruction,
                EvmSchedule::new("cancun"),
                3,
                None,
            )),
        ];
        TraceValidator::validate(&facts).unwrap();
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "src/main.fe",
                vec![],
            ),
            facts,
        ))
        .unwrap();

        let bundle = DebugBundle::from_snapshot(&snapshot);

        assert_eq!(bundle.sources.len(), 1);
        assert_eq!(bundle.code_objects.len(), 1);
        assert_eq!(bundle.gas.len(), 1);
        assert_eq!(bundle.instructions[0].primary_source, Some(source_expr));
        assert_eq!(
            bundle.instructions[0].classification,
            InstructionClassification::SourceMapped
        );
        assert_eq!(
            bundle.instructions[0].confidence,
            AttributionConfidence::High
        );
        assert_eq!(bundle.instructions[0].code_object, Some(code_object));
    }

    #[test]
    fn debug_bundle_marks_missing_source_as_unmapped() {
        let function = key("function", "demo", "main");
        let instruction = key("asm.inst", "demo", "inst:0");
        let facts = vec![
            node(function.clone()),
            node(instruction.clone()),
            TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "mv")),
        ];
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "riscv64",
                vec!["fe".to_string()],
                "main.fe",
                vec![],
            ),
            facts,
        ))
        .unwrap();

        let bundle = DebugBundle::from_snapshot(&snapshot);

        assert_eq!(
            bundle.instructions[0].classification,
            InstructionClassification::Unmapped
        );
        assert_eq!(bundle.instructions[0].primary_source, None);
    }
}
