//! Bounded, compiler-independent observations from the actual EVM execution.
//! Payload equality and nested failures are observations, not causal attribution.
use std::collections::VecDeque;

use revm::{
    context_interface::{ContextTr, LocalContextTr},
    interpreter::{
        CallInput, CallInputs, CallOutcome, CreateInputs, CreateOutcome, Interpreter,
        InterpreterResult, InterpreterTypes,
        interpreter_types::{InputsTr, Jumps, LegacyBytecode, StackTr},
    },
    primitives::{Address, keccak256},
};
use serde::{Deserialize, Serialize};

fn address_hex(address: Address) -> String {
    format!("0x{}", hex::encode(address.as_slice()))
}

#[derive(Clone, Debug)]
pub struct CaptureLimits {
    pub frames: usize,
    pub payload_bytes: usize,
    pub total_payload_bytes: usize,
    pub steps: usize,
    pub stack_items: usize,
    /// Compiler-declared code boundaries to hash, independent of payload retention.
    /// At most 256 distinct boundaries are accepted per capture.
    pub code_prefix_lengths: Vec<usize>,
}

impl Default for CaptureLimits {
    fn default() -> Self {
        Self {
            frames: 4096,
            payload_bytes: 16384,
            total_payload_bytes: 4 * 1024 * 1024,
            steps: 0,
            stack_items: 0,
            code_prefix_lengths: vec![],
        }
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct CapturedBytes {
    pub hex: String,
    pub original_len: usize,
}
impl CapturedBytes {
    pub fn truncated(&self) -> bool {
        self.hex.len() / 2 < self.original_len
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct InstructionLocation {
    pub pc: usize,
    pub opcode: u8,
}

/// A hash of an explicitly requested compiler code boundary, never a guessed match.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct CodePrefixHash {
    pub length: usize,
    pub hash: String,
}

fn code_prefix_hashes(bytes: &[u8], lengths: &[usize]) -> Vec<CodePrefixHash> {
    lengths
        .iter()
        .copied()
        .filter(|&length| length <= bytes.len())
        .map(|length| CodePrefixHash {
            length,
            hash: format!("{:#x}", keccak256(&bytes[..length])),
        })
        .collect()
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionFrame {
    pub id: usize,
    pub parent: Option<usize>,
    pub entered: u64,
    pub exited: Option<u64>,
    pub kind: String,
    pub phase: String,
    pub caller: String,
    pub code_address: Option<String>,
    pub context_address: Option<String>,
    pub code_hash: Option<String>,
    pub deployed_code_hash: Option<String>,
    pub code_len: Option<usize>,
    pub code_prefix_hashes: Vec<CodePrefixHash>,
    pub deployed_code_len: Option<usize>,
    pub deployed_code_prefix_hashes: Vec<CodePrefixHash>,
    pub call_site: Option<InstructionLocation>,
    pub terminal: Option<InstructionLocation>,
    pub interpreter_started: bool,
    pub execution_kind: String,
    pub outcome: Option<String>,
    pub success: Option<bool>,
    pub gas_used: Option<u64>,
    pub input: CapturedBytes,
    pub output: CapturedBytes,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionStep {
    pub frame: usize,
    pub location: InstructionLocation,
    pub gas_remaining: u64,
    pub stack_len: usize,
    pub stack: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionTrace {
    pub schema_version: String,
    pub frames: Vec<ExecutionFrame>,
    pub omitted_frames: usize,
    pub omitted_code_prefixes: usize,
    pub steps: VecDeque<ExecutionStep>,
    pub total_steps: u64,
}

impl Default for ExecutionTrace {
    fn default() -> Self {
        Self {
            schema_version: "fe-execution-trace-v1".into(),
            frames: vec![],
            omitted_frames: 0,
            omitted_code_prefixes: 0,
            steps: VecDeque::new(),
            total_steps: 0,
        }
    }
}

/// Optional legacy renderer observes the same hooks as the structured recorder.
/// It is enabled only when the caller requests the historical unbounded trace.
#[derive(Debug)]
pub struct ExecutionInspector {
    pub trace: ExecutionTrace,
    pub legacy: Option<crate::CallTracer>,
    pub(crate) precompile_addresses: Vec<Address>,
    limits: CaptureLimits,
    // None preserves nesting even after the frame limit has been reached.
    stack: Vec<Option<usize>>,
    last_locations: Vec<Option<InstructionLocation>>,
    sequence: u64,
    remaining_bytes: usize,
}

impl ExecutionInspector {
    pub fn new(mut limits: CaptureLimits, legacy: bool) -> Self {
        limits.code_prefix_lengths.sort_unstable();
        limits.code_prefix_lengths.dedup();
        let omitted_code_prefixes = limits.code_prefix_lengths.len().saturating_sub(256);
        limits.code_prefix_lengths.truncate(256);
        Self {
            remaining_bytes: limits.total_payload_bytes,
            limits,
            trace: ExecutionTrace {
                omitted_code_prefixes,
                ..Default::default()
            },
            legacy: legacy.then(crate::CallTracer::new),
            precompile_addresses: vec![],
            stack: vec![],
            last_locations: vec![],
            sequence: 0,
        }
    }

    fn capture(&mut self, bytes: &[u8]) -> CapturedBytes {
        let len = bytes
            .len()
            .min(self.limits.payload_bytes)
            .min(self.remaining_bytes);
        self.remaining_bytes -= len;
        CapturedBytes {
            hex: hex::encode(&bytes[..len]),
            original_len: bytes.len(),
        }
    }

    fn enter(&mut self, mut frame: ExecutionFrame) {
        self.sequence += 1;
        if self.trace.frames.len() < self.limits.frames {
            frame.id = self.trace.frames.len();
            frame.parent = self.stack.last().copied().flatten();
            frame.call_site = self.last_locations.last().copied().flatten();
            frame.entered = self.sequence;
            self.stack.push(Some(frame.id));
            self.trace.frames.push(frame);
        } else {
            self.trace.omitted_frames += 1;
            self.stack.push(None);
        }
        self.last_locations.push(None);
    }

    fn leave(&mut self, result: &InterpreterResult) {
        self.sequence += 1;
        let terminal = self.last_locations.pop().flatten();
        if let Some(Some(id)) = self.stack.pop() {
            let output = self.capture(&result.output);
            let frame = &mut self.trace.frames[id];
            frame.exited = Some(self.sequence);
            frame.terminal = terminal;
            frame.success = Some(result.result.is_ok());
            frame.outcome = Some(format!("{:?}", result.result));
            frame.gas_used = Some(result.gas.spent());
            frame.output = output;
        }
    }

    fn frame(&self, kind: String, phase: &str, caller: String) -> ExecutionFrame {
        ExecutionFrame {
            id: 0,
            parent: None,
            entered: 0,
            exited: None,
            kind,
            phase: phase.into(),
            caller,
            code_address: None,
            context_address: None,
            code_hash: None,
            deployed_code_hash: None,
            code_len: None,
            code_prefix_hashes: vec![],
            deployed_code_len: None,
            deployed_code_prefix_hashes: vec![],
            call_site: None,
            terminal: None,
            interpreter_started: false,
            execution_kind: "not_executed".into(),
            outcome: None,
            success: None,
            gas_used: None,
            input: CapturedBytes::default(),
            output: CapturedBytes::default(),
        }
    }
}

impl<CTX: ContextTr, INTR: InterpreterTypes> revm::Inspector<CTX, INTR> for ExecutionInspector {
    fn call(&mut self, context: &mut CTX, inputs: &mut CallInputs) -> Option<CallOutcome> {
        if let Some(legacy) = &mut self.legacy {
            <crate::CallTracer as revm::Inspector<CTX, INTR>>::call(legacy, context, inputs);
        }
        let mut frame = self.frame(
            format!("{:?}", inputs.scheme),
            "runtime",
            address_hex(inputs.caller),
        );
        frame.code_address = Some(address_hex(inputs.bytecode_address));
        frame.context_address = Some(address_hex(inputs.target_address));
        if self.trace.frames.len() < self.limits.frames {
            frame.input = match &inputs.input {
                CallInput::Bytes(bytes) => self.capture(bytes),
                CallInput::SharedBuffer(range) => {
                    match context.local().shared_memory_buffer_slice(range.clone()) {
                        Some(bytes) => self.capture(&bytes),
                        None => CapturedBytes {
                            hex: String::new(),
                            original_len: range.len(),
                        },
                    }
                }
            };
        }
        self.enter(frame);
        None
    }

    fn call_end(&mut self, context: &mut CTX, inputs: &CallInputs, outcome: &mut CallOutcome) {
        if let Some(legacy) = &mut self.legacy {
            <crate::CallTracer as revm::Inspector<CTX, INTR>>::call_end(
                legacy, context, inputs, outcome,
            );
        }
        if let Some(Some(id)) = self.stack.last() {
            let frame = &mut self.trace.frames[*id];
            if !frame.interpreter_started {
                let is_precompile = self.precompile_addresses.contains(&inputs.bytecode_address);
                if is_precompile
                    && (outcome.result.result.is_ok()
                        || matches!(
                            outcome.result.result,
                            revm::interpreter::InstructionResult::PrecompileError
                                | revm::interpreter::InstructionResult::PrecompileOOG
                        ))
                {
                    frame.execution_kind = "precompile".into();
                } else if outcome.result.result.is_ok() {
                    frame.execution_kind = "no_code".into();
                }
            }
        }
        self.leave(&outcome.result);
    }

    fn create(&mut self, context: &mut CTX, inputs: &mut CreateInputs) -> Option<CreateOutcome> {
        if let Some(legacy) = &mut self.legacy {
            <crate::CallTracer as revm::Inspector<CTX, INTR>>::create(legacy, context, inputs);
        }
        let kind = match inputs.scheme {
            revm::context_interface::CreateScheme::Create => "CREATE",
            revm::context_interface::CreateScheme::Create2 { .. } => "CREATE2",
            revm::context_interface::CreateScheme::Custom { .. } => "CREATE_CUSTOM",
        };
        let mut frame = self.frame(kind.into(), "create", address_hex(inputs.caller));
        if self.trace.frames.len() < self.limits.frames {
            frame.input = self.capture(&inputs.init_code);
            frame.code_len = Some(inputs.init_code.len());
            frame.code_prefix_hashes =
                code_prefix_hashes(&inputs.init_code, &self.limits.code_prefix_lengths);
            frame.code_hash = Some(format!("{:#x}", keccak256(&inputs.init_code)));
        }
        self.enter(frame);
        None
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        inputs: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        if let Some(legacy) = &mut self.legacy {
            <crate::CallTracer as revm::Inspector<CTX, INTR>>::create_end(
                legacy, context, inputs, outcome,
            );
        }
        if let Some(Some(id)) = self.stack.last() {
            let frame = &mut self.trace.frames[*id];
            frame.context_address = outcome
                .address
                .map(address_hex)
                .or(frame.context_address.take());
            if outcome.result.result.is_ok() {
                frame.deployed_code_len = Some(outcome.result.output.len());
                frame.deployed_code_prefix_hashes =
                    code_prefix_hashes(&outcome.result.output, &self.limits.code_prefix_lengths);
                frame.deployed_code_hash =
                    Some(format!("{:#x}", keccak256(&outcome.result.output)));
            }
        }
        self.leave(&outcome.result);
    }

    fn initialize_interp(&mut self, interp: &mut Interpreter<INTR>, _context: &mut CTX) {
        if let Some(Some(id)) = self.stack.last() {
            let frame = &mut self.trace.frames[*id];
            frame.interpreter_started = true;
            frame.execution_kind = "bytecode".into();
            frame.code_len = Some(interp.bytecode.bytecode_slice().len());
            // Runtime matching uses the full hash. Prefixes are meaningful only for
            // a creation with a compiler-declared constructor argument boundary.
            if frame.phase == "create" {
                frame.code_prefix_hashes = code_prefix_hashes(
                    interp.bytecode.bytecode_slice(),
                    &self.limits.code_prefix_lengths,
                );
            }
            frame.context_address = Some(address_hex(interp.input.target_address()));
            frame.code_hash = Some(format!(
                "{:#x}",
                keccak256(interp.bytecode.bytecode_slice())
            ));
        }
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, _context: &mut CTX) {
        let location = InstructionLocation {
            pc: interp.bytecode.pc(),
            opcode: interp.bytecode.opcode(),
        };
        if let Some(last) = self.last_locations.last_mut() {
            *last = Some(location);
        }
        self.trace.total_steps += 1;
        if self.limits.steps > 0
            && let Some(Some(frame)) = self.stack.last()
        {
            if self.trace.steps.len() == self.limits.steps {
                self.trace.steps.pop_front();
            }
            self.trace.steps.push_back(ExecutionStep {
                frame: *frame,
                location,
                gas_remaining: interp.gas.remaining(),
                stack_len: interp.stack.data().len(),
                stack: interp
                    .stack
                    .data()
                    .iter()
                    .rev()
                    .take(self.limits.stack_items)
                    .rev()
                    .map(|v| format!("{v:#x}"))
                    .collect(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ExecutionOptions, RuntimeInstance};
    use revm::{
        bytecode::Bytecode,
        primitives::{Address, Bytes, U256},
        state::AccountInfo,
    };

    fn install(instance: &mut RuntimeInstance, address: Address, code: &str) {
        let bytecode = Bytecode::new_raw(Bytes::from(hex::decode(code).unwrap()));
        instance
            .evm
            .ctx
            .journaled_state
            .database
            .insert_account_info(
                address,
                AccountInfo::new(U256::ZERO, 0, bytecode.hash_slow(), bytecode),
            );
    }

    #[test]
    #[ignore = "manual timing experiment; no timing assertions"]
    #[allow(clippy::print_stderr)]
    fn measure_failure_tracing_compilation_overhead() {
        use common::InputDb;
        use driver::DriverDataBase;
        use std::time::Instant;
        let sources = [
            (
                "small",
                "use std::evm::Evm\n#[test]\nfn small() uses (evm: mut Evm) { assert!(true) }\n",
            ),
            (
                "nested",
                r#"
use std::evm::{Call, Create, Evm}
msg ReadMsg {
    #[selector = 1]
    Read -> u256,
}
pub contract Child {
    recv ReadMsg {
        Read -> u256 { 7 }
    }
}
#[test]
fn nested() uses (evm: mut Evm) {
    let child = evm.create2<Child>(value: 0, args: (), salt: 0)
    let value = evm.call(addr: child, gas: 100000, value: 0, message: ReadMsg::Read {})
    assert!(value == 7)
}
"#,
            ),
        ];
        for (label, source) in sources {
            for observed in [false, true] {
                let mut timings = Vec::new();
                let mut retained = 0;
                for _ in 0..3 {
                    let start = Instant::now();
                    let mut db = DriverDataBase::default();
                    let uri = url::Url::parse(&format!("file:///tmp/tracing-benchmark-{label}.fe"))
                        .unwrap();
                    let file = db.workspace().touch(&mut db, uri, Some(source.into()));
                    let diagnostics = db.run_on_top_mod(db.top_mod(file));
                    if diagnostics.has_errors(&db) {
                        diagnostics.emit(&db);
                        panic!("invalid {label} benchmark");
                    }
                    let output = codegen::emit_test_module_sonatina(
                        &db,
                        db.top_mod(file),
                        codegen::OptLevel::O0,
                        codegen::SonatinaTestOptions {
                            emit_observability: observed,
                            explain_failure: observed,
                        },
                        None,
                    )
                    .unwrap();
                    timings.push(start.elapsed().as_secs_f64() * 1000.0);
                    if let Some(info) = output.tests[0].debug_info.as_deref() {
                        retained = serde_json::to_vec(info).unwrap().len();
                    }
                    std::hint::black_box(output);
                }
                timings.sort_by(f64::total_cmp);
                eprintln!(
                    "compile {label} observed={observed}: median_ms={:.3} debug_json_bytes={retained}",
                    timings[1]
                );
            }
        }
    }

    #[test]
    #[ignore = "manual timing experiment; no timing assertions"]
    #[allow(clippy::print_stderr)]
    fn measure_failure_tracing_execution_overhead() {
        use std::time::Instant;
        for (label, runtime, calls) in [
            ("small", "00".to_string(), 1000usize),
            (
                "200_children",
                format!("{}00", "5f5f5f5f6112345afa50".repeat(200)),
                100usize,
            ),
        ] {
            for observed in [false, true] {
                let mut timings = Vec::new();
                let mut retained = 0;
                for _ in 0..3 {
                    let mut instance = RuntimeInstance::new(&runtime).unwrap();
                    install(
                        &mut instance,
                        Address::from_slice(
                            &hex::decode("0000000000000000000000000000000000001234").unwrap(),
                        ),
                        "00",
                    );
                    let start = Instant::now();
                    let mut last = None;
                    for _ in 0..calls {
                        if observed {
                            let (result, trace, _) = instance.call_raw_observed(
                                &[],
                                ExecutionOptions::default(),
                                CaptureLimits::default(),
                                false,
                            );
                            assert!(result.is_ok());
                            last = Some(trace);
                        } else {
                            assert!(instance.call_raw(&[], ExecutionOptions::default()).is_ok());
                        }
                    }
                    timings.push(start.elapsed().as_secs_f64() * 1000000.0 / calls as f64);
                    if let Some(trace) = last {
                        retained = serde_json::to_vec(&trace).unwrap().len();
                    }
                }
                timings.sort_by(f64::total_cmp);
                eprintln!(
                    "execute {label} observed={observed}: median_us={:.3} trace_json_bytes={retained}",
                    timings[1]
                );
            }
        }
    }

    #[test]
    fn independent_identical_reverts_remain_separate_observations() {
        // The child reverts with 0x2a. The parent ignores that result and
        // constructs its own 0x2a; matching bytes are not proof of propagation.
        let mut instance = RuntimeInstance::new("5f5f5f5f5f6112345af150602a5f5360015ffd").unwrap();
        install(
            &mut instance,
            Address::from_slice(&hex::decode("0000000000000000000000000000000000001234").unwrap()),
            "602a5f5360015ffd",
        );
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(result.is_err());
        assert_eq!(trace.frames.len(), 2);
        let (parent, child) = (&trace.frames[0], &trace.frames[1]);
        assert_eq!(parent.output.hex, "2a");
        assert_eq!(parent.output, child.output);
        assert_ne!(parent.code_hash, child.code_hash);
        assert_ne!(parent.terminal.unwrap().pc, child.terminal.unwrap().pc);
        assert_eq!(parent.success, Some(false));
        assert_eq!(child.success, Some(false));
        assert_eq!(child.parent, Some(parent.id));
    }

    #[test]
    fn reentrancy_retains_distinct_frames_for_identical_code_and_address() {
        // Reenter this account once with nonempty calldata; that invocation returns.
        let mut instance =
            RuntimeInstance::new("3660125760015f535f5f60015f5f305af1505b00").unwrap();
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(result.is_ok());
        assert_eq!(trace.frames.len(), 2);
        let (parent, child) = (&trace.frames[0], &trace.frames[1]);
        assert_eq!(child.parent, Some(parent.id));
        assert_ne!(child.id, parent.id);
        assert_eq!(child.code_hash, parent.code_hash);
        assert_eq!(child.code_address, parent.code_address);
        assert_eq!(child.input.hex, "01");
        assert!(parent.entered < child.entered);
        assert!(child.exited < parent.exited);
        assert_eq!(child.call_site.unwrap().opcode, 0xf1);
    }

    #[test]
    fn observed_execution_preserves_event_logs_and_return_bytes() {
        let runtime = "602a5f5260205fa060015f5560205ff3";
        let mut plain = RuntimeInstance::new(runtime).unwrap();
        let mut traced = RuntimeInstance::new(runtime).unwrap();
        let expected = plain
            .call_raw_with_logs(&[], ExecutionOptions::default())
            .unwrap();
        let (observed, _, _) = traced.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        let observed = observed.unwrap();
        assert_eq!(observed.logs, expected.logs);
        assert!(!observed.logs.is_empty());
        assert_eq!(observed.result.return_data, expected.result.return_data);
        assert_eq!(observed.result.gas_used, expected.result.gas_used);
    }

    #[test]
    fn precompiles_and_empty_accounts_have_no_fabricated_instruction() {
        // STATICCALL the identity precompile and an empty account, ignoring results.
        let mut instance =
            RuntimeInstance::new("5f5f5f5f60045afa505f5f5f5f6112345afa5000").unwrap();
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(result.is_ok());
        assert_eq!(trace.frames.len(), 3);
        assert_eq!(trace.frames[1].execution_kind, "precompile");
        assert_eq!(trace.frames[2].execution_kind, "no_code");
        for frame in &trace.frames[1..] {
            assert!(!frame.interpreter_started);
            assert!(frame.terminal.is_none());
            assert!(frame.code_hash.is_none());
        }
    }

    #[test]
    fn deployment_identity_survives_payload_truncation() {
        // Return a one-byte STOP runtime. Extra bytes model constructor arguments.
        let init = hex::decode("6001600c5f3960015ff300000000aabbcc").unwrap();
        let limits = CaptureLimits {
            payload_bytes: 0,
            total_payload_bytes: 0,
            code_prefix_lengths: vec![12, 1, 12, 1000],
            ..Default::default()
        };
        let (result, trace) =
            RuntimeInstance::deploy_observed(&hex::encode(&init), &[], Some(limits));
        assert!(result.is_ok());
        let trace = trace.unwrap();
        let frame = &trace.frames[0];
        assert!(frame.input.truncated());
        assert!(frame.output.truncated());
        assert_eq!(frame.code_len, Some(init.len()));
        assert_eq!(frame.deployed_code_len, Some(1));
        assert_eq!(
            frame.code_hash.as_deref(),
            Some(format!("{:#x}", keccak256(&init)).as_str())
        );
        assert!(frame.code_prefix_hashes.contains(&CodePrefixHash {
            length: 12,
            hash: format!("{:#x}", keccak256(&init[..12])),
        }));
        assert_eq!(
            frame.deployed_code_prefix_hashes,
            vec![CodePrefixHash {
                length: 1,
                hash: format!("{:#x}", keccak256([0])),
            }]
        );
    }

    #[test]
    fn observed_execution_preserves_gas_state_and_legacy_trace() {
        // Increment slot zero, then return it: a replay accidentally committed twice is visible.
        let code = "5f546001015f555f545f5260205ff3";
        let mut plain = RuntimeInstance::new(code).unwrap();
        let mut observed = RuntimeInstance::new(code).unwrap();
        let options = ExecutionOptions::default();
        let legacy = observed.call_raw_traced(&[], options).to_string();
        let expected = plain.call_raw(&[], options).unwrap();
        let (actual, trace, old) =
            observed.call_raw_observed(&[], options, CaptureLimits::default(), true);
        let actual = actual.unwrap().result;
        assert_eq!(actual.return_data, expected.return_data);
        assert_eq!(actual.gas_used, expected.gas_used);
        assert_eq!(old.unwrap().to_string(), legacy);
        assert_eq!(trace.frames.len(), 1);
        assert_eq!(
            trace.frames[0].code_hash.as_deref(),
            Some(format!("{:#x}", keccak256(hex::decode(code).unwrap())).as_str())
        );
        assert_eq!(trace.frames[0].terminal.unwrap().opcode, 0xf3);
        assert_eq!(observed.call_raw(&[], options).unwrap().return_data[31], 2);
    }

    #[test]
    fn caught_revert_survives_opcode_eviction() {
        let mut instance = RuntimeInstance::new("5f5f5f5f5f60aa61fffff15000").unwrap();
        install(&mut instance, Address::with_last_byte(0xaa), "5f5ffd");
        let limits = CaptureLimits {
            steps: 1,
            ..CaptureLimits::default()
        };
        let (result, trace, _) =
            instance.call_raw_observed(&[], ExecutionOptions::default(), limits, false);
        assert!(result.is_ok());
        assert_eq!(trace.frames.len(), 2);
        assert_eq!(trace.frames[1].parent, Some(0));
        assert_eq!(trace.frames[1].success, Some(false));
        assert_eq!(trace.frames[0].success, Some(true));
        assert_eq!(trace.frames[1].terminal.unwrap().opcode, 0xfd);
        assert_eq!(trace.frames[1].call_site.unwrap().opcode, 0xf1);
        assert!(trace.frames[1].exited < trace.frames[0].exited);
        assert_eq!(trace.steps.len(), 1);
        assert_eq!(trace.steps[0].frame, 0);
        assert!(trace.total_steps > 1);
    }

    #[test]
    fn delegatecall_keeps_code_and_storage_addresses_distinct() {
        let mut instance = RuntimeInstance::new("5f5f5f5f60aa61fffff45000").unwrap();
        let implementation = Address::with_last_byte(0xaa);
        install(&mut instance, implementation, "00");
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(result.is_ok());
        let child = &trace.frames[1];
        assert_eq!(child.code_address, Some(address_hex(implementation)));
        assert_eq!(child.context_address, Some(address_hex(instance.address)));
        assert_eq!(
            child.code_hash.as_deref(),
            Some(format!("{:#x}", keccak256([0])).as_str())
        );
    }

    #[test]
    fn bounded_frames_and_payloads_do_not_change_verdict() {
        let code = "5f5f5f5f5f60aa61fffff15f5ffd";
        let mut instance = RuntimeInstance::new(code).unwrap();
        install(&mut instance, Address::with_last_byte(0xaa), "00");
        let limits = CaptureLimits {
            frames: 1,
            payload_bytes: 2,
            total_payload_bytes: 1,
            ..CaptureLimits::default()
        };
        let (result, trace, _) =
            instance.call_raw_observed(&[1, 2, 3], ExecutionOptions::default(), limits, false);
        assert!(matches!(result, Err(crate::HarnessError::Revert(_))));
        assert_eq!(trace.frames.len(), 1);
        assert_eq!(trace.omitted_frames, 1);
        assert_eq!(trace.frames[0].input.hex, "01");
        assert!(trace.frames[0].input.truncated());
        assert_eq!(trace.frames[0].terminal.unwrap().opcode, 0xfd);
        assert_eq!(trace.frames[0].success, Some(false));
    }

    #[test]
    fn failed_root_deployment_retains_constructor_location() {
        let (result, trace) =
            RuntimeInstance::deploy_observed("5f5ffd", &[], Some(CaptureLimits::default()));
        assert!(matches!(result, Err(crate::HarnessError::Revert(_))));
        let trace = trace.unwrap();
        assert_eq!(trace.frames.len(), 1);
        assert_eq!(trace.frames[0].phase, "create");
        assert_eq!(trace.frames[0].terminal.unwrap().pc, 2);
        assert_eq!(trace.frames[0].success, Some(false));
    }

    #[test]
    fn exceptional_halt_is_not_reported_as_revert() {
        let mut instance = RuntimeInstance::new("fe").unwrap();
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(matches!(result, Err(crate::HarnessError::Halted { .. })));
        assert_eq!(trace.frames[0].terminal.unwrap().opcode, 0xfe);
        assert_ne!(trace.frames[0].outcome.as_deref(), Some("Revert"));
    }
}
