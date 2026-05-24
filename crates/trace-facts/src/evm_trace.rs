use std::collections::BTreeMap;
use std::fmt;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};

use crate::{DynamicGasStepFact, TraceFact};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvmExecutionTrace {
    pub trace_id: String,
    pub code_object: OriginExportKey,
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
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvmExecutionStep {
    pub pc: u32,
    #[serde(alias = "gas")]
    pub gas_before: u64,
    #[serde(default, alias = "gasCost")]
    pub gas_cost: Option<u64>,
    #[serde(default)]
    pub gas_after: Option<u64>,
}

impl EvmExecutionStep {
    pub fn new(pc: u32, gas_before: u64, gas_after: u64) -> Self {
        Self {
            pc,
            gas_before,
            gas_cost: Some(gas_before.saturating_sub(gas_after)),
            gas_after: Some(gas_after),
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

fn instruction_pc_index(
    code_object: &OriginExportKey,
    facts: &[TraceFact],
) -> BTreeMap<u32, OriginExportKey> {
    let mut function_code_objects = BTreeMap::new();
    for fact in facts {
        if let TraceFact::Function(function) = fact
            && let Some(function_code_object) = &function.code_object
        {
            function_code_objects.insert(function.function.clone(), function_code_object.clone());
        }
    }

    let mut pc_to_instruction = BTreeMap::new();
    for fact in facts {
        let TraceFact::Instruction(instruction) = fact else {
            continue;
        };
        if function_code_objects
            .get(&instruction.function)
            .is_some_and(|candidate| candidate != code_object)
        {
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

#[derive(Debug)]
pub enum EvmExecutionTraceError {
    Json(serde_json::Error),
    InvalidStepGas { pc: u32, reason: &'static str },
}

impl fmt::Display for EvmExecutionTraceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json(err) => write!(f, "invalid EVM execution trace JSON: {err}"),
            Self::InvalidStepGas { pc, reason } => {
                write!(f, "invalid EVM execution trace step at pc {pc}: {reason}")
            }
        }
    }
}

impl std::error::Error for EvmExecutionTraceError {}

impl From<serde_json::Error> for EvmExecutionTraceError {
    fn from(err: serde_json::Error) -> Self {
        Self::Json(err)
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use crate::{
        CodeObjectFact, CodeObjectKind, EvmExecutionTrace, FunctionFact, InstructionFact,
        OriginNodeFact, OriginNodeKind, TraceFact, dynamic_gas_facts_from_evm_trace,
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
}
