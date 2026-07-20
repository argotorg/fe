use std::collections::BTreeMap;
use std::fmt;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};

use crate::{
    DynamicGasStepFact, ExecutionStepFact, ExecutionTraceSessionFact, OriginNodeFact,
    RuntimeCaptureMode, RuntimeCodeObjectBindingFact, RuntimePcJoinConfidence,
    RuntimeTraceDataSource, RuntimeValue, RuntimeValuePolicy, StackSampleFact, TraceFact,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvmExecutionTrace {
    pub trace_id: String,
    pub code_object: OriginExportKey,
    pub source: RuntimeTraceDataSource,
    pub steps: Vec<EvmExecutionStep>,
}

impl EvmExecutionTrace {
    pub fn new(
        trace_id: impl Into<String>,
        code_object: OriginExportKey,
        steps: Vec<EvmExecutionStep>,
    ) -> Self {
        Self {
            trace_id: trace_id.into(),
            code_object,
            source: RuntimeTraceDataSource::DebugTraceTransaction,
            steps,
        }
    }

    pub fn from_debug_trace_json(
        trace_id: impl Into<String>,
        code_object: OriginExportKey,
        json: &str,
    ) -> Result<Self, EvmExecutionTraceError> {
        let value = serde_json::from_str::<serde_json::Value>(json)?;
        let steps = match value.get("structLogs") {
            Some(struct_logs) => serde_json::from_value(struct_logs.clone())?,
            None => serde_json::from_value(value)?,
        };
        Ok(Self::new(trace_id, code_object, steps))
    }

    pub fn from_eip3155_jsonl(
        trace_id: impl Into<String>,
        code_object: OriginExportKey,
        jsonl: &str,
    ) -> Result<Self, EvmExecutionTraceError> {
        let mut steps = Vec::new();
        for (index, line) in jsonl.lines().enumerate() {
            if line.trim().is_empty() {
                continue;
            }
            let step = serde_json::from_str::<EvmExecutionStep>(line).map_err(|source| {
                EvmExecutionTraceError::JsonLine {
                    line: index + 1,
                    source,
                }
            })?;
            steps.push(step);
        }
        Ok(Self {
            trace_id: trace_id.into(),
            code_object,
            source: RuntimeTraceDataSource::Eip3155,
            steps,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct EvmExecutionStep {
    pub pc: u32,
    pub opcode: Option<String>,
    pub depth: Option<u32>,
    pub gas_before: u64,
    pub gas_cost: Option<u64>,
    pub gas_after: Option<u64>,
    pub stack: Vec<String>,
}

/// Accepts both spec-shaped EIP-3155 output (hex-string gas, numeric `op`,
/// `opName`) and geth `structLogs` shape (decimal numbers, string opcode).
#[derive(Deserialize)]
#[serde(untagged)]
enum FlexU64 {
    Number(u64),
    Text(String),
}

impl FlexU64 {
    fn resolve<E: serde::de::Error>(self, field: &'static str) -> Result<u64, E> {
        match self {
            Self::Number(value) => Ok(value),
            Self::Text(text) => {
                let trimmed = text.trim();
                let parsed = if let Some(hex) = trimmed.strip_prefix("0x") {
                    u64::from_str_radix(hex, 16)
                } else {
                    trimmed.parse::<u64>()
                };
                parsed.map_err(|err| {
                    E::custom(format!("invalid numeric value {text:?} for {field}: {err}"))
                })
            }
        }
    }
}

#[derive(Deserialize)]
#[serde(untagged)]
enum FlexOpcode {
    Number(u8),
    Text(String),
}

impl<'de> Deserialize<'de> for EvmExecutionStep {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        #[derive(Deserialize)]
        struct RawStep {
            pc: FlexU64,
            #[serde(default)]
            op: Option<FlexOpcode>,
            #[serde(default, rename = "opName")]
            op_name: Option<String>,
            #[serde(default)]
            opcode: Option<String>,
            #[serde(default)]
            depth: Option<FlexU64>,
            #[serde(default)]
            gas: Option<FlexU64>,
            #[serde(default)]
            gas_before: Option<FlexU64>,
            #[serde(default, alias = "gasCost")]
            gas_cost: Option<FlexU64>,
            #[serde(default)]
            gas_after: Option<FlexU64>,
            #[serde(default)]
            stack: Vec<String>,
        }

        let raw = RawStep::deserialize(deserializer)?;
        let pc = raw.pc.resolve::<D::Error>("pc")?;
        let pc = u32::try_from(pc).map_err(|_| D::Error::custom(format!("pc {pc} exceeds u32")))?;
        let opcode = raw
            .opcode
            .or(raw.op_name)
            .or(match raw.op {
                Some(FlexOpcode::Text(text)) => Some(text),
                Some(FlexOpcode::Number(byte)) => Some(format!("0x{byte:02x}")),
                None => None,
            })
            .filter(|value| !value.is_empty());
        let depth = raw
            .depth
            .map(|value| value.resolve::<D::Error>("depth"))
            .transpose()?
            .map(|value| {
                u32::try_from(value)
                    .map_err(|_| D::Error::custom(format!("depth {value} exceeds u32")))
            })
            .transpose()?;
        let gas_before = raw
            .gas_before
            .or(raw.gas)
            .ok_or_else(|| D::Error::custom("missing field `gas` (or `gas_before`)"))?
            .resolve::<D::Error>("gas")?;
        let gas_cost = raw
            .gas_cost
            .map(|value| value.resolve::<D::Error>("gasCost"))
            .transpose()?;
        let gas_after = raw
            .gas_after
            .map(|value| value.resolve::<D::Error>("gas_after"))
            .transpose()?;
        Ok(Self {
            pc,
            opcode,
            depth,
            gas_before,
            gas_cost,
            gas_after,
            stack: raw.stack,
        })
    }
}

impl EvmExecutionStep {
    pub fn new(pc: u32, gas_before: u64, gas_after: u64) -> Self {
        Self {
            pc,
            opcode: None,
            depth: None,
            gas_before,
            gas_cost: Some(gas_before.saturating_sub(gas_after)),
            gas_after: Some(gas_after),
            stack: Vec::new(),
        }
    }

    fn resolved_gas_after(&self) -> Result<u64, EvmExecutionTraceError> {
        match (self.gas_after, self.gas_cost) {
            (Some(gas_after), Some(gas_cost))
                if self.gas_before.saturating_sub(gas_after) != gas_cost =>
            {
                Err(EvmExecutionTraceError::InvalidStepGas {
                    pc: self.pc,
                    reason: "gasCost does not equal gas_before - gas_after",
                })
            }
            (Some(gas_after), _) => Ok(gas_after),
            (None, Some(gas_cost)) if gas_cost <= self.gas_before => Ok(self.gas_before - gas_cost),
            (None, Some(_)) => Err(EvmExecutionTraceError::InvalidStepGas {
                pc: self.pc,
                reason: "gasCost exceeds gas_before",
            }),
            (None, None) => Err(EvmExecutionTraceError::InvalidStepGas {
                pc: self.pc,
                reason: "missing gas_after or gasCost",
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeImportConfig {
    pub session: OriginExportKey,
    pub code_object: OriginExportKey,
    pub runtime_code_hash: String,
    pub source: RuntimeTraceDataSource,
    pub capture_mode: RuntimeCaptureMode,
    pub value_policy: RuntimeValuePolicy,
    pub instruction_by_pc: BTreeMap<u32, OriginExportKey>,
}

impl RuntimeImportConfig {
    pub fn new(
        session: OriginExportKey,
        code_object: OriginExportKey,
        runtime_code_hash: impl Into<String>,
        source: RuntimeTraceDataSource,
    ) -> Self {
        Self {
            session,
            code_object,
            runtime_code_hash: runtime_code_hash.into(),
            source,
            capture_mode: RuntimeCaptureMode::Standard,
            value_policy: RuntimeValuePolicy::HashOnly,
            instruction_by_pc: BTreeMap::new(),
        }
    }

    pub fn with_capture_mode(mut self, capture_mode: RuntimeCaptureMode) -> Self {
        self.capture_mode = capture_mode;
        self
    }

    pub fn with_value_policy(mut self, value_policy: RuntimeValuePolicy) -> Self {
        self.value_policy = value_policy;
        self
    }

    pub fn with_instruction_by_pc(
        mut self,
        instruction_by_pc: impl IntoIterator<Item = (u32, OriginExportKey)>,
    ) -> Self {
        self.instruction_by_pc = instruction_by_pc.into_iter().collect();
        self
    }
}

pub fn dynamic_gas_facts_from_evm_trace(
    trace: &EvmExecutionTrace,
    base_facts: &[TraceFact],
) -> Result<Vec<TraceFact>, EvmExecutionTraceError> {
    let pc_to_instruction = instruction_pc_index(&trace.code_object, base_facts);
    trace
        .steps
        .iter()
        .enumerate()
        .map(|(step_index, step)| {
            let gas_after = step.resolved_gas_after()?;
            Ok(TraceFact::DynamicGasStep(DynamicGasStepFact::new(
                trace.trace_id.clone(),
                step_index as u64,
                trace.code_object.clone(),
                step.pc,
                pc_to_instruction.get(&step.pc).cloned(),
                step.gas_before,
                gas_after,
                step.gas_before.saturating_sub(gas_after),
            )))
        })
        .collect()
}

pub fn runtime_facts_from_debug_trace_json(
    config: RuntimeImportConfig,
    json: &str,
) -> Result<Vec<TraceFact>, EvmExecutionTraceError> {
    let trace = EvmExecutionTrace::from_debug_trace_json(
        config.session.canonical_storage_key(),
        config.code_object.clone(),
        json,
    )?;
    runtime_facts_from_evm_execution_trace(&config, &trace)
}

pub fn runtime_facts_from_eip3155_jsonl(
    config: RuntimeImportConfig,
    jsonl: &str,
) -> Result<Vec<TraceFact>, EvmExecutionTraceError> {
    let trace = EvmExecutionTrace::from_eip3155_jsonl(
        config.session.canonical_storage_key(),
        config.code_object.clone(),
        jsonl,
    )?;
    runtime_facts_from_evm_execution_trace(&config, &trace)
}

pub fn runtime_facts_from_evm_execution_trace(
    config: &RuntimeImportConfig,
    trace: &EvmExecutionTrace,
) -> Result<Vec<TraceFact>, EvmExecutionTraceError> {
    let mut facts = Vec::new();
    let binding = runtime_key(&config.session, "runtime.binding", "binding:runtime");
    facts.push(node(config.session.clone()));
    facts.push(node(binding.clone()));
    facts.push(TraceFact::ExecutionTraceSession(
        ExecutionTraceSessionFact {
            session: config.session.clone(),
            source: config.source,
            capture_mode: config.capture_mode,
            value_policy: config.value_policy,
            transaction_hash: None,
            chain_id: None,
            block_number: None,
            entry_code_object: Some(config.code_object.clone()),
        },
    ));
    facts.push(TraceFact::RuntimeCodeObjectBinding(
        RuntimeCodeObjectBindingFact {
            binding,
            session: config.session.clone(),
            code_object: config.code_object.clone(),
            runtime_code_hash: config.runtime_code_hash.clone(),
            address: None,
            confidence: RuntimePcJoinConfidence::PcOnlyWithinUniqueCodeObject,
        },
    ));
    for (index, step) in trace.steps.iter().enumerate() {
        let gas_after = step.resolved_gas_after()?;
        let step_key = runtime_key(&config.session, "runtime.step", format!("step:{index}"));
        let instruction = config.instruction_by_pc.get(&step.pc).cloned();
        let join_confidence = if instruction.is_some() {
            RuntimePcJoinConfidence::PcOnlyWithinUniqueCodeObject
        } else {
            RuntimePcJoinConfidence::MissingStaticInstruction
        };
        facts.push(node(step_key.clone()));
        facts.push(TraceFact::ExecutionStep(ExecutionStepFact {
            step: step_key.clone(),
            session: config.session.clone(),
            step_index: index as u64,
            code_object: config.code_object.clone(),
            pc: step.pc,
            opcode: step.opcode.clone().unwrap_or_else(|| "UNKNOWN".to_string()),
            instruction: instruction.clone(),
            gas_before: step.gas_before,
            gas_after,
            gas_cost: step.gas_before.saturating_sub(gas_after),
            depth: step.depth.unwrap_or(1),
            join_confidence,
        }));
        facts.push(TraceFact::DynamicGasStep(DynamicGasStepFact::new(
            trace.trace_id.clone(),
            index as u64,
            config.code_object.clone(),
            step.pc,
            instruction,
            step.gas_before,
            gas_after,
            step.gas_before.saturating_sub(gas_after),
        )));
        if matches!(
            config.capture_mode,
            RuntimeCaptureMode::Full | RuntimeCaptureMode::DebugFull
        ) && !step.stack.is_empty()
        {
            let sample = runtime_key(&config.session, "runtime.stack", format!("sample:{index}"));
            facts.push(node(sample.clone()));
            facts.push(TraceFact::StackSample(StackSampleFact {
                sample,
                step: step_key,
                policy: config.value_policy,
                values_top_first: step
                    .stack
                    .iter()
                    .rev()
                    .map(|value| imported_runtime_value(value, config.value_policy))
                    .collect(),
            }));
        }
    }
    Ok(facts)
}

fn instruction_pc_index(
    code_object: &OriginExportKey,
    facts: &[TraceFact],
) -> BTreeMap<u32, OriginExportKey> {
    // Instruction extents carry the authoritative instruction -> code-object
    // binding; validation guarantees they do not overlap per code object.
    let mut pc_to_instruction = BTreeMap::new();
    for fact in facts {
        if let TraceFact::InstructionExtent(extent) = fact
            && extent.code_object == *code_object
        {
            pc_to_instruction.insert(extent.pc_range.start, extent.instruction.clone());
        }
    }
    if !pc_to_instruction.is_empty() {
        return pc_to_instruction;
    }

    // Legacy bundles without extents: join only instructions whose function is
    // explicitly bound to this code object. Unbound instructions must not be
    // indexed into every trace; in a multi-code-object bundle that attaches
    // runtime steps to the wrong static instruction.
    let mut function_code_objects = BTreeMap::new();
    for fact in facts {
        if let TraceFact::Function(function) = fact
            && let Some(function_code_object) = &function.code_object
        {
            function_code_objects.insert(function.function.clone(), function_code_object.clone());
        }
    }
    for fact in facts {
        let TraceFact::Instruction(instruction) = fact else {
            continue;
        };
        if function_code_objects.get(&instruction.function) != Some(code_object) {
            continue;
        }
        let Some(pc) = instruction_pc(&instruction.instruction) else {
            continue;
        };
        pc_to_instruction.insert(pc, instruction.instruction.clone());
    }
    pc_to_instruction
}

fn instruction_pc(instruction: &OriginExportKey) -> Option<u32> {
    (instruction.kind() == "bytecode.pc")
        .then(|| instruction.local_key().strip_prefix("pc:")?.parse().ok())
        .flatten()
}

fn node(key: OriginExportKey) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::from_key(key))
}

fn runtime_key(session: &OriginExportKey, kind: &str, local: impl Into<String>) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, session.owner_key(), local.into())
        .expect("runtime import key should be valid")
}

fn imported_runtime_value(value: &str, policy: RuntimeValuePolicy) -> RuntimeValue {
    match policy {
        RuntimeValuePolicy::Redacted => RuntimeValue::redacted(),
        RuntimeValuePolicy::HashOnly => RuntimeValue::hash(
            "blake3",
            blake3::hash(value.as_bytes()).to_hex().to_string(),
        ),
        RuntimeValuePolicy::Full => RuntimeValue::bytes(normalize_hex(value)),
    }
}

fn normalize_hex(value: &str) -> String {
    let trimmed = value.trim().strip_prefix("0x").unwrap_or(value.trim());
    if trimmed.len().is_multiple_of(2) {
        trimmed.to_string()
    } else {
        format!("0{trimmed}")
    }
}

#[derive(Debug)]
pub enum EvmExecutionTraceError {
    Json(serde_json::Error),
    JsonLine {
        line: usize,
        source: serde_json::Error,
    },
    InvalidStepGas {
        pc: u32,
        reason: &'static str,
    },
}

impl fmt::Display for EvmExecutionTraceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json(err) => write!(f, "invalid EVM execution trace JSON: {err}"),
            Self::JsonLine { line, source } => {
                write!(f, "invalid EVM execution trace JSONL line {line}: {source}")
            }
            Self::InvalidStepGas { pc, reason } => {
                write!(f, "invalid EVM execution trace step at pc {pc}: {reason}")
            }
        }
    }
}

impl std::error::Error for EvmExecutionTraceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Json(source) | Self::JsonLine { source, .. } => Some(source),
            Self::InvalidStepGas { .. } => None,
        }
    }
}

impl From<serde_json::Error> for EvmExecutionTraceError {
    fn from(err: serde_json::Error) -> Self {
        Self::Json(err)
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    #[test]
    fn spec_shaped_eip3155_line_parses() {
        // Hex-string gas, numeric `op`, and `opName` all present, as emitted
        // by evmone / geth `--json` EIP-3155 tracers.
        let line = r#"{"pc":0,"op":96,"opName":"PUSH1","gas":"0x2540be400","gasCost":"0x3","memSize":0,"stack":[],"depth":1,"refund":0}"#;
        let step: super::EvmExecutionStep = serde_json::from_str(line).unwrap();

        assert_eq!(step.pc, 0);
        assert_eq!(step.opcode.as_deref(), Some("PUSH1"));
        assert_eq!(step.gas_before, 0x2540be400);
        assert_eq!(step.gas_cost, Some(3));
        assert_eq!(step.depth, Some(1));
    }

    #[test]
    fn numeric_only_op_parses_as_hex_mnemonic() {
        let line = r#"{"pc":4,"op":1,"gas":100,"gasCost":3}"#;
        let step: super::EvmExecutionStep = serde_json::from_str(line).unwrap();

        assert_eq!(step.opcode.as_deref(), Some("0x01"));
    }

    use crate::{
        CodeObjectFact, CodeObjectKind, EvmExecutionTrace, FunctionFact, InstructionFact,
        OriginNodeFact, OriginNodeKind, RuntimeCaptureMode, RuntimeImportConfig,
        RuntimeTraceDataSource, RuntimeValuePolicy, TraceFact, TraceValidator,
        dynamic_gas_facts_from_evm_trace, runtime_facts_from_debug_trace_json,
        runtime_facts_from_eip3155_jsonl,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    #[test]
    fn ingests_struct_logs_and_joins_pc_to_instruction_identity() {
        let code_object = key("code.object", "demo", "runtime");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:4");
        let base = vec![
            node(code_object.clone()),
            node(function.clone()),
            node(instruction.clone()),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                None,
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
        ];
        let trace = EvmExecutionTrace::from_debug_trace_json(
            "tx:1",
            code_object,
            r#"{"structLogs":[{"pc":4,"gas":100,"gasCost":3}]}"#,
        )
        .unwrap();

        let facts = dynamic_gas_facts_from_evm_trace(&trace, &base).unwrap();

        let TraceFact::DynamicGasStep(step) = &facts[0] else {
            panic!("expected dynamic gas step");
        };
        assert_eq!(step.instruction, Some(instruction));
        assert_eq!(step.gas_before, 100);
        assert_eq!(step.gas_after, 97);
        assert_eq!(step.gas_cost, 3);
    }

    #[test]
    fn imports_debug_trace_as_valid_runtime_facts() {
        let code_object = key("code.object", "demo", "runtime");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:4");
        let session = key("runtime.session", "tx:1", "session");
        let mut facts = vec![
            node(code_object.clone()),
            node(function.clone()),
            node(instruction.clone()),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                Some(
                    "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                        .to_string(),
                ),
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
        ];
        let config = RuntimeImportConfig::new(
            session,
            code_object,
            "blake3:000000000000000000000000000000000000000000000000000000000000beef",
            RuntimeTraceDataSource::DebugTraceTransaction,
        )
        .with_capture_mode(RuntimeCaptureMode::Full)
        .with_value_policy(RuntimeValuePolicy::Full)
        .with_instruction_by_pc([(4, instruction)]);

        facts.extend(
            runtime_facts_from_debug_trace_json(
                config,
                r#"{"structLogs":[{"pc":4,"op":"ADD","gas":100,"gasCost":3,"depth":1,"stack":["0x01","0x02"]}]}"#,
            )
            .unwrap(),
        );

        assert!(
            facts
                .iter()
                .any(|fact| matches!(fact, TraceFact::ExecutionStep(step) if step.opcode == "ADD"))
        );
        assert!(
            facts
                .iter()
                .any(|fact| matches!(fact, TraceFact::StackSample(_)))
        );
        assert!(TraceValidator::validate(&facts).is_ok());
    }

    #[test]
    fn imports_eip3155_jsonl_as_runtime_facts() {
        let code_object = key("code.object", "demo", "runtime");
        let session = key("runtime.session", "tx:1", "session");
        let mut facts = vec![node(code_object.clone())];
        let config = RuntimeImportConfig::new(
            session,
            code_object,
            "blake3:000000000000000000000000000000000000000000000000000000000000beef",
            RuntimeTraceDataSource::Eip3155,
        );

        facts.extend(
            runtime_facts_from_eip3155_jsonl(
                config,
                r#"{"pc":0,"opName":"STOP","gas":10,"gasCost":0}"#,
            )
            .unwrap(),
        );

        assert!(
            facts.iter().any(
                |fact| matches!(fact, TraceFact::ExecutionStep(step) if step.opcode == "STOP")
            )
        );
        assert!(TraceValidator::validate(&facts).is_ok());
    }
}
