use std::collections::BTreeMap;

use common::origin::OriginExportKey;
use revm::{
    context::result::{ExecutionResult, Output},
    interpreter::{
        CallInputs, CallOutcome, CreateInputs, CreateOutcome, Interpreter, InterpreterTypes,
        interpreter_types::{Jumps, StackTr},
    },
    primitives::{Address, Log, U256},
};
use trace_facts::{
    CallFact, DynamicGasStepFact, ExecutionStepFact, ExecutionTraceSessionFact, LogFact,
    MemoryAccessFact, MemoryAccessKind, OriginNodeFact, ReturnDataFact, ReturnDataKind,
    RuntimeCallKind, RuntimeCaptureMode, RuntimeCodeObjectBindingFact, RuntimePcJoinConfidence,
    RuntimeTraceDataSource, RuntimeValue, RuntimeValuePolicy, SelfdestructFact, StackSampleFact,
    StorageAccessFact, StorageAccessKind, TraceFact,
};

#[derive(Clone, Debug)]
pub struct RuntimeTraceConfig {
    pub session: OriginExportKey,
    pub code_object: OriginExportKey,
    pub runtime_code_hash: String,
    pub address: Option<Address>,
    pub capture_mode: RuntimeCaptureMode,
    pub value_policy: RuntimeValuePolicy,
    pub instruction_by_pc: BTreeMap<u32, OriginExportKey>,
    pub max_stack_values: usize,
}

impl RuntimeTraceConfig {
    pub fn new(
        session: OriginExportKey,
        code_object: OriginExportKey,
        runtime_code_hash: impl Into<String>,
    ) -> Self {
        Self {
            session,
            code_object,
            runtime_code_hash: runtime_code_hash.into(),
            address: None,
            capture_mode: RuntimeCaptureMode::Standard,
            value_policy: RuntimeValuePolicy::HashOnly,
            instruction_by_pc: BTreeMap::new(),
            max_stack_values: 16,
        }
    }

    pub fn with_address(mut self, address: Address) -> Self {
        self.address = Some(address);
        self
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

pub trait RuntimeTraceSink {
    fn emit(&mut self, fact: TraceFact);
}

#[derive(Clone, Debug, Default)]
pub struct VecRuntimeTraceSink {
    facts: Vec<TraceFact>,
}

impl VecRuntimeTraceSink {
    pub fn into_facts(self) -> Vec<TraceFact> {
        self.facts
    }
}

impl RuntimeTraceSink for VecRuntimeTraceSink {
    fn emit(&mut self, fact: TraceFact) {
        self.facts.push(fact);
    }
}

impl RuntimeTraceSink for Vec<TraceFact> {
    fn emit(&mut self, fact: TraceFact) {
        self.push(fact);
    }
}

#[derive(Clone, Debug)]
pub struct FeRuntimeTraceInspector<S> {
    config: RuntimeTraceConfig,
    sink: S,
    next_step_index: u64,
    next_aux_index: u64,
    frame_depth: u32,
    pending_step: Option<PendingStep>,
}

#[derive(Clone, Debug)]
struct PendingStep {
    step: OriginExportKey,
    pc: u32,
    opcode: u8,
    gas_before: u64,
    stack_before: Vec<U256>,
    depth: u32,
}

impl FeRuntimeTraceInspector<VecRuntimeTraceSink> {
    pub fn new_vec(config: RuntimeTraceConfig) -> Self {
        Self::new(config, VecRuntimeTraceSink::default())
    }

    pub fn into_facts(self) -> Vec<TraceFact> {
        self.sink.into_facts()
    }
}

impl<S: RuntimeTraceSink> FeRuntimeTraceInspector<S> {
    pub fn new(config: RuntimeTraceConfig, sink: S) -> Self {
        let mut inspector = Self {
            config,
            sink,
            next_step_index: 0,
            next_aux_index: 0,
            frame_depth: 0,
            pending_step: None,
        };
        inspector.emit_session_facts();
        inspector
    }

    pub fn finish_result(&mut self, result: &ExecutionResult) {
        match result {
            ExecutionResult::Success {
                output: Output::Call(bytes),
                ..
            } => {
                let Some(step) = self.last_step_key() else {
                    return;
                };
                let event = self.runtime_key("runtime.return_data", "return");
                self.emit_node(event.clone());
                self.emit(TraceFact::ReturnData(ReturnDataFact {
                    event,
                    step,
                    kind: ReturnDataKind::Return,
                    data: bytes_runtime_value(bytes.as_ref(), self.config.value_policy),
                    policy: self.config.value_policy,
                }));
            }
            ExecutionResult::Revert { output, .. } => {
                let Some(step) = self.last_step_key() else {
                    return;
                };
                let event = self.runtime_key("runtime.revert", "revert");
                self.emit_node(event.clone());
                self.emit(TraceFact::Revert(trace_facts::RevertFact {
                    revert: event,
                    step,
                    reason: None,
                    data: bytes_runtime_value(output.as_ref(), self.config.value_policy),
                    policy: self.config.value_policy,
                }));
            }
            _ => {}
        }
    }

    fn emit_session_facts(&mut self) {
        let session = self.config.session.clone();
        let binding = self.runtime_key("runtime.binding", "binding:runtime");
        self.emit_node(session.clone());
        self.emit_node(binding.clone());
        self.emit(TraceFact::ExecutionTraceSession(
            ExecutionTraceSessionFact {
                session: session.clone(),
                source: RuntimeTraceDataSource::RevmInspector,
                capture_mode: self.config.capture_mode,
                value_policy: self.config.value_policy,
                transaction_hash: None,
                chain_id: None,
                block_number: None,
                entry_code_object: Some(self.config.code_object.clone()),
            },
        ));
        self.emit(TraceFact::RuntimeCodeObjectBinding(
            RuntimeCodeObjectBindingFact {
                binding,
                session,
                code_object: self.config.code_object.clone(),
                runtime_code_hash: self.config.runtime_code_hash.clone(),
                address: self.config.address.map(address_hex),
                confidence: RuntimePcJoinConfidence::ExactCodeObjectAndPc,
            },
        ));
    }

    fn begin_step<INTR: InterpreterTypes>(&mut self, interp: &mut Interpreter<INTR>) {
        let step = self.runtime_key("runtime.step", format!("step:{}", self.next_step_index));
        let stack_before = interp.stack.data().to_vec();
        self.pending_step = Some(PendingStep {
            step,
            pc: interp.bytecode.pc() as u32,
            opcode: interp.bytecode.opcode(),
            gas_before: interp.gas.remaining(),
            stack_before,
            depth: self.frame_depth.max(1),
        });
    }

    fn finish_step<INTR: InterpreterTypes>(&mut self, interp: &mut Interpreter<INTR>) {
        let Some(pending) = self.pending_step.take() else {
            return;
        };
        let gas_after = interp.gas.remaining();
        let gas_cost = pending.gas_before.saturating_sub(gas_after);
        let instruction = self.config.instruction_by_pc.get(&pending.pc).cloned();
        let join_confidence = if instruction.is_some() {
            RuntimePcJoinConfidence::ExactCodeObjectAndPc
        } else {
            RuntimePcJoinConfidence::MissingStaticInstruction
        };
        self.emit_node(pending.step.clone());
        self.emit(TraceFact::ExecutionStep(ExecutionStepFact {
            step: pending.step.clone(),
            session: self.config.session.clone(),
            step_index: self.next_step_index,
            code_object: self.config.code_object.clone(),
            pc: pending.pc,
            opcode: opcode_name(pending.opcode).to_string(),
            instruction: instruction.clone(),
            gas_before: pending.gas_before,
            gas_after,
            gas_cost,
            depth: pending.depth,
            join_confidence,
        }));
        self.emit(TraceFact::DynamicGasStep(DynamicGasStepFact::new(
            self.config.session.canonical_storage_key(),
            self.next_step_index,
            self.config.code_object.clone(),
            pending.pc,
            instruction.clone(),
            pending.gas_before,
            gas_after,
            gas_cost,
        )));
        if matches!(
            self.config.capture_mode,
            RuntimeCaptureMode::Full | RuntimeCaptureMode::DebugFull
        ) {
            self.emit_stack_sample(&pending);
            self.emit_memory_or_storage_access(&pending, instruction);
        }
        self.next_step_index += 1;
    }

    fn emit_stack_sample(&mut self, pending: &PendingStep) {
        let sample = self.runtime_key("runtime.stack", format!("sample:{}", self.next_aux_index));
        self.next_aux_index += 1;
        self.emit_node(sample.clone());
        self.emit(TraceFact::StackSample(StackSampleFact {
            sample,
            step: pending.step.clone(),
            policy: self.config.value_policy,
            values_top_first: pending
                .stack_before
                .iter()
                .rev()
                .take(self.config.max_stack_values)
                .map(|value| u256_runtime_value(value, self.config.value_policy))
                .collect(),
        }));
    }

    fn emit_memory_or_storage_access(
        &mut self,
        pending: &PendingStep,
        instruction: Option<OriginExportKey>,
    ) {
        match pending.opcode {
            0x51 => self.emit_memory_access(pending, MemoryAccessKind::Read, 0, 32),
            0x52 => self.emit_memory_access(pending, MemoryAccessKind::Write, 0, 32),
            0x53 => self.emit_memory_access(pending, MemoryAccessKind::Write, 0, 1),
            0x54 => self.emit_storage_access(pending, instruction, StorageAccessKind::Read),
            0x55 => self.emit_storage_access(pending, instruction, StorageAccessKind::Write),
            _ => {}
        }
    }

    fn emit_memory_access(
        &mut self,
        pending: &PendingStep,
        kind: MemoryAccessKind,
        stack_offset_index: usize,
        fallback_length: u64,
    ) {
        let access = self.runtime_key("runtime.memory", format!("access:{}", self.next_aux_index));
        self.next_aux_index += 1;
        let offset = stack_value(&pending.stack_before, stack_offset_index)
            .map(|value| value.saturating_to::<u64>())
            .unwrap_or(0);
        self.emit_node(access.clone());
        self.emit(TraceFact::MemoryAccess(MemoryAccessFact {
            access,
            step: pending.step.clone(),
            kind,
            offset,
            length: fallback_length,
            value: None,
            policy: self.config.value_policy,
        }));
    }

    fn emit_storage_access(
        &mut self,
        pending: &PendingStep,
        instruction: Option<OriginExportKey>,
        kind: StorageAccessKind,
    ) {
        let access = self.runtime_key("runtime.storage", format!("access:{}", self.next_aux_index));
        self.next_aux_index += 1;
        let slot = stack_value(&pending.stack_before, 0)
            .map(|value| u256_runtime_value(&value, self.config.value_policy))
            .unwrap_or_else(RuntimeValue::redacted);
        let value = stack_value(&pending.stack_before, 1)
            .map(|value| u256_runtime_value(&value, self.config.value_policy));
        self.emit_node(access.clone());
        self.emit(TraceFact::StorageAccess(StorageAccessFact {
            access,
            step: pending.step.clone(),
            code_object: self.config.code_object.clone(),
            instruction,
            kind,
            address: self.config.address.map(address_hex),
            slot,
            value_before: None,
            value_after: (kind == StorageAccessKind::Write)
                .then_some(value)
                .flatten(),
            policy: self.config.value_policy,
        }));
    }

    fn emit_call(&mut self, step: Option<OriginExportKey>, call: CallFact) {
        if !matches!(
            self.config.capture_mode,
            RuntimeCaptureMode::Standard | RuntimeCaptureMode::Full | RuntimeCaptureMode::DebugFull
        ) {
            return;
        }
        self.emit_node(call.call.clone());
        let mut call = call;
        if let Some(step) = step {
            call.step = step;
            self.emit(TraceFact::Call(call));
        }
    }

    fn emit(&mut self, fact: TraceFact) {
        self.sink.emit(fact);
    }

    fn emit_node(&mut self, key: OriginExportKey) {
        self.emit(TraceFact::OriginNode(OriginNodeFact::from_key(key)));
    }

    fn runtime_key(&self, kind: &str, local: impl Into<String>) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, self.config.session.owner_key(), local.into())
            .expect("runtime trace key should be valid")
    }

    fn last_step_key(&self) -> Option<OriginExportKey> {
        (self.next_step_index > 0)
            .then(|| self.runtime_key("runtime.step", format!("step:{}", self.next_step_index - 1)))
    }
}

impl<S, CTX, INTR> revm::Inspector<CTX, INTR> for FeRuntimeTraceInspector<S>
where
    S: RuntimeTraceSink,
    INTR: InterpreterTypes,
{
    fn initialize_interp(&mut self, _interp: &mut Interpreter<INTR>, _context: &mut CTX) {
        self.frame_depth = self.frame_depth.saturating_add(1);
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, _context: &mut CTX) {
        self.begin_step(interp);
    }

    fn step_end(&mut self, interp: &mut Interpreter<INTR>, _context: &mut CTX) {
        self.finish_step(interp);
    }

    fn log_full(&mut self, _interp: &mut Interpreter<INTR>, _context: &mut CTX, log: Log) {
        if !matches!(
            self.config.capture_mode,
            RuntimeCaptureMode::Standard | RuntimeCaptureMode::Full | RuntimeCaptureMode::DebugFull
        ) {
            return;
        }
        let Some(step) = self.last_step_key() else {
            return;
        };
        let event = self.runtime_key("runtime.log", format!("log:{}", self.next_aux_index));
        self.next_aux_index += 1;
        self.emit_node(event.clone());
        self.emit(TraceFact::Log(LogFact {
            log: event,
            step,
            address: Some(address_hex(log.address)),
            topics: log
                .topics()
                .iter()
                .map(|topic| bytes_runtime_value(topic.as_slice(), self.config.value_policy))
                .collect(),
            data: bytes_runtime_value(log.data.data.as_ref(), self.config.value_policy),
            policy: self.config.value_policy,
        }));
    }

    fn call(&mut self, _context: &mut CTX, inputs: &mut CallInputs) -> Option<CallOutcome> {
        let call = self.runtime_key("runtime.call", format!("call:{}", self.next_aux_index));
        self.next_aux_index += 1;
        self.emit_call(
            self.last_step_key(),
            CallFact {
                call,
                step: self.config.session.clone(),
                kind: runtime_call_kind(inputs.scheme),
                caller: Some(address_hex(inputs.caller)),
                callee: Some(address_hex(inputs.target_address)),
                value: Some(u256_runtime_value(
                    &inputs.call_value(),
                    self.config.value_policy,
                )),
                gas_requested: Some(inputs.gas_limit),
                gas_used: None,
                success: None,
                callsite_instruction: None,
                policy: self.config.value_policy,
            },
        );
        None
    }

    fn create(&mut self, _context: &mut CTX, inputs: &mut CreateInputs) -> Option<CreateOutcome> {
        let call = self.runtime_key("runtime.call", format!("call:{}", self.next_aux_index));
        self.next_aux_index += 1;
        self.emit_call(
            self.last_step_key(),
            CallFact {
                call,
                step: self.config.session.clone(),
                kind: match inputs.scheme {
                    revm::context_interface::CreateScheme::Create => RuntimeCallKind::Create,
                    revm::context_interface::CreateScheme::Create2 { .. } => {
                        RuntimeCallKind::Create2
                    }
                    revm::context_interface::CreateScheme::Custom { .. } => RuntimeCallKind::Create,
                },
                caller: Some(address_hex(inputs.caller)),
                callee: None,
                value: Some(u256_runtime_value(&inputs.value, self.config.value_policy)),
                gas_requested: Some(inputs.gas_limit),
                gas_used: None,
                success: None,
                callsite_instruction: None,
                policy: self.config.value_policy,
            },
        );
        None
    }

    fn selfdestruct(&mut self, contract: Address, target: Address, value: U256) {
        let Some(step) = self.last_step_key() else {
            return;
        };
        let event = self.runtime_key(
            "runtime.selfdestruct",
            format!("selfdestruct:{}", self.next_aux_index),
        );
        self.next_aux_index += 1;
        self.emit_node(event.clone());
        self.emit(TraceFact::Selfdestruct(SelfdestructFact {
            event,
            step,
            contract: Some(address_hex(contract)),
            beneficiary: Some(address_hex(target)),
            balance: Some(u256_runtime_value(&value, self.config.value_policy)),
            policy: self.config.value_policy,
        }));
    }
}

fn runtime_call_kind(scheme: revm::interpreter::CallScheme) -> RuntimeCallKind {
    match scheme {
        revm::interpreter::CallScheme::Call => RuntimeCallKind::Call,
        revm::interpreter::CallScheme::CallCode => RuntimeCallKind::CallCode,
        revm::interpreter::CallScheme::DelegateCall => RuntimeCallKind::DelegateCall,
        revm::interpreter::CallScheme::StaticCall => RuntimeCallKind::StaticCall,
    }
}

fn stack_value(stack: &[U256], index_from_top: usize) -> Option<U256> {
    stack.iter().rev().nth(index_from_top).copied()
}

fn u256_runtime_value(value: &U256, policy: RuntimeValuePolicy) -> RuntimeValue {
    match policy {
        RuntimeValuePolicy::Redacted => RuntimeValue::redacted(),
        RuntimeValuePolicy::HashOnly => {
            let hex = u256_hex(value);
            RuntimeValue::hash("blake3", blake3::hash(hex.as_bytes()).to_hex().to_string())
        }
        RuntimeValuePolicy::Full => RuntimeValue::bytes(u256_hex(value)),
    }
}

fn bytes_runtime_value(bytes: &[u8], policy: RuntimeValuePolicy) -> RuntimeValue {
    match policy {
        RuntimeValuePolicy::Redacted => RuntimeValue::redacted(),
        RuntimeValuePolicy::HashOnly => {
            RuntimeValue::hash("blake3", blake3::hash(bytes).to_hex().to_string())
        }
        RuntimeValuePolicy::Full => RuntimeValue::bytes(hex::encode(bytes)),
    }
}

fn u256_hex(value: &U256) -> String {
    let raw = format!("{value:x}");
    if raw.len() % 2 == 0 {
        raw
    } else {
        format!("0{raw}")
    }
}

fn address_hex(address: Address) -> String {
    format!("0x{}", hex::encode(address.as_slice()))
}

fn opcode_name(opcode: u8) -> &'static str {
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
        0x18 => "XOR",
        0x20 => "KECCAK256",
        0x31 => "BALANCE",
        0x33 => "CALLER",
        0x34 => "CALLVALUE",
        0x35 => "CALLDATALOAD",
        0x36 => "CALLDATASIZE",
        0x37 => "CALLDATACOPY",
        0x51 => "MLOAD",
        0x52 => "MSTORE",
        0x53 => "MSTORE8",
        0x54 => "SLOAD",
        0x55 => "SSTORE",
        0x56 => "JUMP",
        0x57 => "JUMPI",
        0x5b => "JUMPDEST",
        0xf0 => "CREATE",
        0xf1 => "CALL",
        0xf3 => "RETURN",
        0xf4 => "DELEGATECALL",
        0xf5 => "CREATE2",
        0xfa => "STATICCALL",
        0xfd => "REVERT",
        0xff => "SELFDESTRUCT",
        0x60..=0x7f => "PUSH",
        0x80..=0x8f => "DUP",
        0x90..=0x9f => "SWAP",
        0xa0..=0xa4 => "LOG",
        _ => "UNKNOWN",
    }
}

#[cfg(test)]
mod tests {
    use trace_facts::{
        CodeObjectFact, CodeObjectKind, OriginNodeFact, OriginNodeKind, TraceValidator,
    };

    use super::*;
    use crate::{ExecutionOptions, RuntimeInstance};

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn runtime_trace_replay_emits_valid_step_and_stack_facts() {
        let code_object = key("code.object", "runtime-test", "runtime");
        let session = key("runtime.session", "runtime-test", "tx:0");
        let mut facts = vec![
            TraceFact::OriginNode(OriginNodeFact::new(
                code_object.clone(),
                OriginNodeKind::new("code.object"),
            )),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                None,
                "evm/revm",
                Some(
                    "blake3:0000000000000000000000000000000000000000000000000000000000000001"
                        .to_string(),
                ),
            )),
        ];
        let instance = RuntimeInstance::new("600160020100").unwrap();
        let trace = instance
            .call_raw_runtime_trace(
                &[],
                ExecutionOptions::default(),
                RuntimeTraceConfig::new(
                    session,
                    code_object,
                    "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                )
                .with_capture_mode(RuntimeCaptureMode::Full)
                .with_value_policy(RuntimeValuePolicy::HashOnly),
            )
            .unwrap();
        facts.extend(trace);

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
}
