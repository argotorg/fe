//! Test harness utilities for compiling Fe contracts and exercising their runtimes with `revm`.
use codegen::{OptLevel, emit_module_sonatina_bytecode};
use common::InputDb;
use driver::DriverDataBase;
use ethers_core::abi::{AbiParser, ParamType, ParseError as AbiParseError, Token, decode};
use hex::FromHex;
pub use revm::primitives::{Address, Log, U256};
use revm::{
    InspectCommitEvm,
    bytecode::Bytecode,
    context::{
        Context, TxEnv,
        result::{ExecutionResult, HaltReason, Output},
    },
    database::InMemoryDB,
    handler::{ExecuteCommitEvm, MainBuilder, MainContext, MainnetContext, MainnetEvm},
    interpreter::interpreter_types::Jumps,
    primitives::{Bytes as EvmBytes, TxKind},
    state::AccountInfo,
};
use std::{
    cmp::Reverse,
    collections::{HashMap, VecDeque},
    fmt,
    io::Write,
    path::{Path, PathBuf},
};
use thiserror::Error;
use url::Url;

/// Default in-memory file path used when compiling inline Fe sources.
const MEMORY_SOURCE_URL: &str = "file:///contract.fe";
/// Tests may emit oversized helper contracts that would never be deployed on-chain.
const TEST_CONTRACT_CODE_SIZE_LIMIT: usize = 1024 * 1024;
const TEST_CONTRACT_INITCODE_SIZE_LIMIT: usize = 2 * TEST_CONTRACT_CODE_SIZE_LIMIT;
/// Test-only execution budget for deploying and calling generated helper contracts.
const TEST_GAS_LIMIT: u64 = 1_000_000_000;

/// Error type returned by the harness.
#[derive(Error)]
pub enum HarnessError {
    #[error("fe compiler diagnostics:\n{0}")]
    CompilerDiagnostics(String),
    #[error("failed to emit Sonatina bytecode: {0}")]
    EmitSonatina(String),
    #[error("abi encoding failed: {0}")]
    Abi(#[from] ethers_core::abi::Error),
    #[error("failed to parse function signature: {0}")]
    AbiSignature(#[from] AbiParseError),
    #[error("execution failed: {0}")]
    Execution(String),
    #[error("runtime reverted with data {0}")]
    Revert(RevertData),
    #[error("runtime halted: {reason:?} (gas_used={gas_used})")]
    Halted { reason: HaltReason, gas_used: u64 },
    #[error("unexpected output variant from runtime")]
    UnexpectedOutput,
    #[error("invalid hex string: {0}")]
    Hex(#[from] hex::FromHexError),
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

impl fmt::Debug for HarnessError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Captures raw revert data and provides a nicer `Display` implementation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RevertData(pub Vec<u8>);

impl fmt::Display for RevertData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{}", hex::encode(&self.0))
    }
}

/// Options that control how the Fe source is compiled.
#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub opt_level: OptLevel,
}

/// Options that control the execution context fed into `revm`.
#[derive(Debug, Clone, Copy)]
pub struct ExecutionOptions {
    pub caller: Address,
    pub gas_limit: u64,
    pub gas_price: u128,
    pub value: U256,
    /// Optional transaction nonce; when absent the harness uses the caller's
    /// current nonce from the in-memory database.
    pub nonce: Option<u64>,
}

/// Optional tracing settings for a runtime call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvmTraceOptions {
    /// Number of trailing EVM steps to keep in the ring buffer.
    pub keep_steps: usize,
    /// Number of stack values to render for each traced step.
    pub stack_n: usize,
    /// Optional output file for trace text. When absent, tracing only goes to stderr.
    pub out_path: Option<PathBuf>,
    /// Whether to mirror trace output to stderr.
    pub write_stderr: bool,
}

impl Default for EvmTraceOptions {
    fn default() -> Self {
        Self {
            keep_steps: 200,
            stack_n: 0,
            out_path: None,
            write_stderr: true,
        }
    }
}

impl Default for ExecutionOptions {
    fn default() -> Self {
        Self {
            caller: Address::ZERO,
            gas_limit: TEST_GAS_LIMIT,
            gas_price: 0,
            value: U256::ZERO,
            nonce: None,
        }
    }
}

/// Output returned from executing contract runtime bytecode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallResult {
    pub return_data: Vec<u8>,
    pub gas_used: u64,
}

/// Output returned from executing contract runtime bytecode along with logs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallResultWithLogs {
    pub result: CallResult,
    pub logs: Vec<String>,
    pub raw_logs: Vec<Log>,
}

/// Per-call gas attribution gathered from a full instruction trace replay.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CallGasProfile {
    /// Number of EVM instructions executed.
    pub step_count: u64,
    /// Sum of per-step gas deltas on the root runtime frame.
    pub total_step_gas: u64,
    /// Root-frame gas attributed to CREATE opcode steps (`0xF0`).
    pub create_opcode_gas: u64,
    /// Root-frame gas attributed to CREATE2 opcode steps (`0xF5`).
    pub create2_opcode_gas: u64,
    /// Root-frame gas attributed to all non-CREATE/CREATE2 opcode steps.
    pub non_create_opcode_gas: u64,
    /// Number of CREATE opcode steps (`0xF0`).
    pub create_opcode_steps: u64,
    /// Number of CREATE2 opcode steps (`0xF5`).
    pub create2_opcode_steps: u64,
    /// Gas reported by CREATE frame outcomes (constructor execution envelope).
    pub constructor_frame_gas: u64,
    /// Root-frame gas outside constructor execution (`total_step_gas - constructor_frame_gas`).
    pub non_constructor_frame_gas: u64,
}

fn prepare_account(
    runtime_bytecode_hex: &str,
) -> Result<(Bytecode, Address, InMemoryDB), HarnessError> {
    let code = hex_to_bytes(runtime_bytecode_hex)?;
    let bytecode = Bytecode::new_raw(EvmBytes::from(code));
    let address = Address::with_last_byte(0xff);
    Ok((bytecode, address, InMemoryDB::default()))
}

fn transact(
    evm: &mut MainnetEvm<MainnetContext<InMemoryDB>>,
    address: Address,
    calldata: &[u8],
    options: ExecutionOptions,
    nonce: u64,
    trace_options: Option<&EvmTraceOptions>,
) -> Result<CallResult, HarnessError> {
    let outcome = transact_with_logs(evm, address, calldata, options, nonce, trace_options)?;
    Ok(outcome.result)
}

/// Executes a call transaction and returns the result plus formatted logs.
///
/// * `evm` - Mutable EVM instance to execute against.
/// * `address` - Target contract address.
/// * `calldata` - ABI-encoded call data.
/// * `options` - Execution options (gas, caller, value).
/// * `nonce` - Transaction nonce to use.
///
/// Returns the call result along with any logs emitted by the execution.
fn transact_with_logs(
    evm: &mut MainnetEvm<MainnetContext<InMemoryDB>>,
    address: Address,
    calldata: &[u8],
    options: ExecutionOptions,
    nonce: u64,
    trace_options: Option<&EvmTraceOptions>,
) -> Result<CallResultWithLogs, HarnessError> {
    let build_tx = || {
        TxEnv::builder()
            .caller(options.caller)
            .gas_limit(options.gas_limit)
            .gas_price(options.gas_price)
            .to(address)
            .value(options.value)
            .data(EvmBytes::copy_from_slice(calldata))
            .nonce(nonce)
            .build()
    };

    if let Some(trace_options) = trace_options {
        trace_tx(evm, build_tx().expect("tx builder is valid"), trace_options);
    }

    let tx = build_tx().map_err(|err| HarnessError::Execution(format!("{err:?}")))?;

    let result = evm
        .transact_commit(tx)
        .map_err(|err| HarnessError::Execution(err.to_string()))?;
    match result {
        ExecutionResult::Success {
            output: Output::Call(bytes),
            gas_used,
            logs,
            ..
        } => Ok(CallResultWithLogs {
            result: CallResult {
                return_data: bytes.to_vec(),
                gas_used,
            },
            logs: format_logs(&logs),
            raw_logs: logs,
        }),
        ExecutionResult::Success {
            output: Output::Create(..),
            ..
        } => Err(HarnessError::UnexpectedOutput),
        ExecutionResult::Revert { output, .. } => {
            Err(HarnessError::Revert(RevertData(output.to_vec())))
        }
        ExecutionResult::Halt { reason, gas_used } => {
            Err(HarnessError::Halted { reason, gas_used })
        }
    }
}

fn trace_tx(evm: &MainnetEvm<MainnetContext<InMemoryDB>>, tx: TxEnv, options: &EvmTraceOptions) {
    #[derive(Clone, Debug)]
    struct Step {
        pc: usize,
        opcode: u8,
        stack_len: usize,
        gas_remaining: u64,
        stack_top: Vec<String>,
    }

    #[derive(Clone, Debug)]
    struct RingTrace {
        keep: usize,
        stack_n: usize,
        steps: VecDeque<Step>,
        total_steps: u64,
    }

    impl RingTrace {
        fn new(keep: usize, stack_n: usize) -> Self {
            Self {
                keep,
                stack_n,
                steps: VecDeque::with_capacity(keep),
                total_steps: 0,
            }
        }

        fn push(&mut self, step: Step) {
            self.total_steps += 1;
            if self.steps.len() == self.keep {
                self.steps.pop_front();
            }
            self.steps.push_back(step);
        }

        fn format(&self) -> String {
            let mut out = String::new();
            out.push_str(&format!(
                "TRACE (last {} of {} steps)\n",
                self.steps.len(),
                self.total_steps
            ));
            for s in &self.steps {
                if self.stack_n > 0 {
                    out.push_str(&format!(
                        "pc={:04} op=0x{:02x} stack={} gas_rem={} top={}\n",
                        s.pc,
                        s.opcode,
                        s.stack_len,
                        s.gas_remaining,
                        s.stack_top.join(",")
                    ));
                } else {
                    out.push_str(&format!(
                        "pc={:04} op=0x{:02x} stack={} gas_rem={}\n",
                        s.pc, s.opcode, s.stack_len, s.gas_remaining
                    ));
                }
            }
            out
        }
    }

    impl<CTX, INTR: revm::interpreter::InterpreterTypes> revm::Inspector<CTX, INTR> for RingTrace {
        fn step(&mut self, interp: &mut revm::interpreter::Interpreter<INTR>, _context: &mut CTX) {
            let stack_top = if self.stack_n == 0 {
                Vec::new()
            } else {
                interp
                    .stack
                    .data()
                    .iter()
                    .rev()
                    .take(self.stack_n)
                    .rev()
                    .map(|v| format!("{v:#x}"))
                    .collect()
            };
            self.push(Step {
                pc: interp.bytecode.pc(),
                opcode: interp.bytecode.opcode(),
                stack_len: interp.stack.len(),
                gas_remaining: interp.gas.remaining(),
                stack_top,
            });
        }
    }

    use revm::interpreter::interpreter_types::{Jumps, StackTr};

    // Clone the EVM (including DB state) for tracing so we don't disturb the caller's state.
    let ctx = evm.ctx.clone();
    let mut trace_evm =
        ctx.build_mainnet_with_inspector(RingTrace::new(options.keep_steps, options.stack_n));

    let result = trace_evm.inspect_tx_commit(tx);
    let formatted = format!(
        "{}\ntrace result: {result:?}\n",
        trace_evm.inspector.format()
    );
    if let Some(path) = &options.out_path {
        match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .and_then(|mut f| f.write_all(formatted.as_bytes()))
        {
            Ok(()) => {
                if options.write_stderr {
                    tracing::debug!("{formatted}");
                }
            }
            Err(err) => {
                tracing::error!(
                    "EVM trace output: failed to write `{}`: {err}",
                    path.display()
                );
                tracing::debug!("{formatted}");
            }
        }
    } else if options.write_stderr {
        tracing::debug!("{formatted}");
    }
}

// ---------------------------------------------------------------------------
// Call-trace inspector: captures CALL/CREATE events at contract boundaries
// ---------------------------------------------------------------------------

/// Normalized address in a call trace (sequential ID, not raw address).
type AddrId = usize;

/// A single event in a call trace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallTraceEvent {
    Call {
        target: AddrId,
        calldata: Vec<u8>,
        output: Vec<u8>,
        success: bool,
    },
    Create {
        scheme: &'static str,
        address: AddrId,
        success: bool,
    },
}

/// Address-normalized call trace from a single test execution.
#[derive(Debug, Clone, Default)]
pub struct CallTrace {
    pub events: Vec<CallTraceEvent>,
    addr_map: HashMap<Address, AddrId>,
}

impl CallTrace {
    /// Replaces known addresses in a hex-encoded byte string with their `$N` IDs.
    ///
    /// EVM addresses are 20 bytes. In ABI-encoded return data, they appear as
    /// 32-byte words with 12 zero bytes followed by the 20-byte address.
    /// We scan for both raw 20-byte occurrences and zero-padded 32-byte words.
    fn normalize_hex(hex_str: &str, addr_map: &HashMap<Address, AddrId>) -> String {
        if addr_map.is_empty() || hex_str.is_empty() {
            return hex_str.to_string();
        }

        let mut result = hex_str.to_string();
        // Sort by longest hex representation first to avoid partial replacements
        let mut entries: Vec<_> = addr_map.iter().collect();
        entries.sort_by_key(|(addr, _)| Reverse(addr.to_string().len()));

        for (addr, id) in entries {
            let addr_hex = hex::encode(addr.as_slice()); // 40 hex chars
            // Replace zero-padded 32-byte ABI word (24 zeros + 40 hex chars)
            let padded = format!("000000000000000000000000{addr_hex}");
            result = result.replace(&padded, &format!("${id}"));
            // Also replace bare 20-byte address
            result = result.replace(&addr_hex, &format!("${id}"));
        }
        result
    }
}

impl fmt::Display for CallTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for event in &self.events {
            match event {
                CallTraceEvent::Call {
                    target,
                    calldata,
                    output,
                    success,
                } => {
                    let status = if *success { "ok" } else { "revert" };
                    let ret_hex = CallTrace::normalize_hex(&hex::encode(output), &self.addr_map);
                    let data_hex = CallTrace::normalize_hex(&hex::encode(calldata), &self.addr_map);
                    writeln!(
                        f,
                        "CALL ${target} data={data_hex} -> {status} ret={ret_hex}",
                    )?;
                }
                CallTraceEvent::Create {
                    scheme,
                    address,
                    success,
                } => {
                    let status = if *success { "ok" } else { "fail" };
                    writeln!(f, "{scheme} {status} -> ${address}")?;
                }
            }
        }
        Ok(())
    }
}

/// Tracks whether we are inside a CALL or CREATE frame.
#[derive(Debug, Clone)]
enum PendingFrame {
    Call { target: AddrId, calldata: Vec<u8> },
    Create { scheme: &'static str },
}

/// Inspector that records every CALL/CREATE at contract boundaries.
///
/// Addresses are normalized to sequential IDs so that traces from different
/// backends (which produce different bytecode and therefore different
/// CREATE-derived addresses) can be compared directly.
#[derive(Debug)]
pub struct CallTracer {
    addr_map: HashMap<Address, AddrId>,
    next_id: AddrId,
    stack: Vec<PendingFrame>,
    events: Vec<CallTraceEvent>,
}

impl Default for CallTracer {
    fn default() -> Self {
        Self::new()
    }
}

impl CallTracer {
    pub fn new() -> Self {
        Self {
            addr_map: HashMap::new(),
            next_id: 0,
            stack: Vec::new(),
            events: Vec::new(),
        }
    }

    fn resolve_addr(&mut self, addr: Address) -> AddrId {
        let next = self.next_id;
        *self.addr_map.entry(addr).or_insert_with(|| {
            self.next_id = next + 1;
            next
        })
    }

    fn assign_new_addr(&mut self, addr: Address) -> AddrId {
        let id = self.next_id;
        self.next_id += 1;
        self.addr_map.insert(addr, id);
        id
    }

    pub fn into_trace(self) -> CallTrace {
        CallTrace {
            events: self.events,
            addr_map: self.addr_map,
        }
    }
}

impl<CTX: revm::context_interface::ContextTr, INTR: revm::interpreter::InterpreterTypes>
    revm::Inspector<CTX, INTR> for CallTracer
{
    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut revm::interpreter::CallInputs,
    ) -> Option<revm::interpreter::CallOutcome> {
        let target_id = self.resolve_addr(inputs.target_address);
        let calldata = inputs.input.bytes(context).to_vec();
        self.stack.push(PendingFrame::Call {
            target: target_id,
            calldata,
        });
        None
    }

    fn call_end(
        &mut self,
        _context: &mut CTX,
        _inputs: &revm::interpreter::CallInputs,
        outcome: &mut revm::interpreter::CallOutcome,
    ) {
        let Some(frame) = self.stack.pop() else {
            return;
        };
        if let PendingFrame::Call { target, calldata } = frame {
            self.events.push(CallTraceEvent::Call {
                target,
                calldata,
                output: outcome.result.output.to_vec(),
                success: outcome.result.result.is_ok(),
            });
        }
    }

    fn create(
        &mut self,
        _context: &mut CTX,
        inputs: &mut revm::interpreter::CreateInputs,
    ) -> Option<revm::interpreter::CreateOutcome> {
        let scheme = match inputs.scheme {
            revm::context_interface::CreateScheme::Create => "CREATE",
            revm::context_interface::CreateScheme::Create2 { .. } => "CREATE2",
            revm::context_interface::CreateScheme::Custom { .. } => "CREATE_CUSTOM",
        };
        self.stack.push(PendingFrame::Create { scheme });
        None
    }

    fn create_end(
        &mut self,
        _context: &mut CTX,
        _inputs: &revm::interpreter::CreateInputs,
        outcome: &mut revm::interpreter::CreateOutcome,
    ) {
        let Some(frame) = self.stack.pop() else {
            return;
        };
        if let PendingFrame::Create { scheme } = frame {
            let success = outcome.result.result.is_ok();
            let addr_id = if let Some(addr) = outcome.address {
                self.assign_new_addr(addr)
            } else {
                // Failed create — assign a placeholder ID
                let id = self.next_id;
                self.next_id += 1;
                id
            };
            self.events.push(CallTraceEvent::Create {
                scheme,
                address: addr_id,
                success,
            });
        }
    }
}

/// Formats raw EVM logs into debug strings for display.
///
/// * `logs` - Logs emitted by the EVM execution.
///
/// Returns a vector of formatted log strings.
fn format_logs(logs: &[Log]) -> Vec<String> {
    logs.iter().map(|log| format!("{log:?}")).collect()
}

/// Stateful runtime instance backed by a persistent in-memory database.
pub struct RuntimeInstance {
    evm: MainnetEvm<MainnetContext<InMemoryDB>>,
    address: Address,
    next_nonce_by_caller: HashMap<Address, u64>,
    trace_options: Option<EvmTraceOptions>,
}

impl RuntimeInstance {
    /// Instantiates a runtime instance from raw bytecode, inserting it into an `InMemoryDB`.
    pub fn new(runtime_bytecode_hex: &str) -> Result<Self, HarnessError> {
        let (bytecode, address, mut db) = prepare_account(runtime_bytecode_hex)?;
        let code_hash = bytecode.hash_slow();
        db.insert_account_info(
            address,
            AccountInfo::new(U256::ZERO, 0, code_hash, bytecode),
        );
        let ctx = Context::mainnet().with_db(db).modify_cfg_chained(|cfg| {
            cfg.limit_contract_code_size = Some(TEST_CONTRACT_CODE_SIZE_LIMIT);
            cfg.limit_contract_initcode_size = Some(TEST_CONTRACT_INITCODE_SIZE_LIMIT);
        });
        let evm = ctx.build_mainnet();
        Ok(Self {
            evm,
            address,
            next_nonce_by_caller: HashMap::new(),
            trace_options: None,
        })
    }

    /// Deploys a contract by executing its init bytecode and using the returned runtime code.
    /// This properly runs any initialization logic in the constructor.
    pub fn deploy(init_bytecode_hex: &str) -> Result<Self, HarnessError> {
        Self::deploy_tracked(init_bytecode_hex).map(|(instance, _)| instance)
    }

    /// Deploys a contract and returns the runtime instance plus deployment gas.
    pub fn deploy_tracked(init_bytecode_hex: &str) -> Result<(Self, u64), HarnessError> {
        Self::deploy_with_constructor_args_tracked(init_bytecode_hex, &[])
    }

    /// Deploys a contract by executing its init bytecode with ABI-encoded constructor args.
    pub fn deploy_with_constructor_args(
        init_bytecode_hex: &str,
        constructor_args: &[u8],
    ) -> Result<Self, HarnessError> {
        Self::deploy_with_constructor_args_tracked(init_bytecode_hex, constructor_args)
            .map(|(instance, _)| instance)
    }

    /// Deploys a contract with constructor args and returns deployment gas.
    pub fn deploy_with_constructor_args_tracked(
        init_bytecode_hex: &str,
        constructor_args: &[u8],
    ) -> Result<(Self, u64), HarnessError> {
        let mut init_code = hex_to_bytes(init_bytecode_hex)?;
        init_code.extend_from_slice(constructor_args);
        let caller = Address::ZERO;

        let mut db = InMemoryDB::default();
        // Give the caller some balance for deployment
        db.insert_account_info(
            caller,
            AccountInfo::new(
                U256::from(1_000_000_000u64),
                0,
                Default::default(),
                Bytecode::default(),
            ),
        );

        let ctx = Context::mainnet().with_db(db).modify_cfg_chained(|cfg| {
            cfg.limit_contract_code_size = Some(TEST_CONTRACT_CODE_SIZE_LIMIT);
            cfg.limit_contract_initcode_size = Some(TEST_CONTRACT_INITCODE_SIZE_LIMIT);
        });
        let mut evm = ctx.build_mainnet();

        // Create deployment transaction (TxKind::Create means contract creation)
        let tx = TxEnv::builder()
            .caller(caller)
            .gas_limit(TEST_GAS_LIMIT)
            .gas_price(0)
            .kind(TxKind::Create)
            .data(EvmBytes::from(init_code))
            .nonce(0)
            .build()
            .map_err(|err| HarnessError::Execution(format!("{err:?}")))?;

        let result = evm
            .transact_commit(tx)
            .map_err(|err| HarnessError::Execution(err.to_string()))?;

        match result {
            ExecutionResult::Success {
                output: Output::Create(_, Some(deployed_address)),
                gas_used,
                ..
            } => {
                // The contract was deployed successfully; revm has already inserted the account
                let mut next_nonce_by_caller = HashMap::new();
                next_nonce_by_caller.insert(caller, 1);
                Ok((
                    Self {
                        evm,
                        address: deployed_address,
                        next_nonce_by_caller,
                        trace_options: None,
                    },
                    gas_used,
                ))
            }
            ExecutionResult::Success { output, .. } => Err(HarnessError::Execution(format!(
                "deployment returned unexpected output: {output:?}"
            ))),
            ExecutionResult::Revert { output, .. } => {
                Err(HarnessError::Revert(RevertData(output.to_vec())))
            }
            ExecutionResult::Halt { reason, gas_used } => {
                Err(HarnessError::Halted { reason, gas_used })
            }
        }
    }

    /// Deploys another contract into the same in-memory EVM context.
    pub fn deploy_sidecar(
        &mut self,
        init_bytecode_hex: &str,
        constructor_args: &[u8],
    ) -> Result<Address, HarnessError> {
        let mut init_code = hex_to_bytes(init_bytecode_hex)?;
        init_code.extend_from_slice(constructor_args);

        let caller = Address::ZERO;
        let nonce = self.effective_nonce(ExecutionOptions {
            caller,
            ..ExecutionOptions::default()
        });

        let tx = TxEnv::builder()
            .caller(caller)
            .gas_limit(10_000_000)
            .gas_price(0)
            .kind(TxKind::Create)
            .data(EvmBytes::from(init_code))
            .nonce(nonce)
            .build()
            .map_err(|err| HarnessError::Execution(format!("{err:?}")))?;

        let result = self
            .evm
            .transact_commit(tx)
            .map_err(|err| HarnessError::Execution(err.to_string()))?;

        match result {
            ExecutionResult::Success {
                output: Output::Create(_, Some(deployed_address)),
                ..
            } => Ok(deployed_address),
            ExecutionResult::Success { output, .. } => Err(HarnessError::Execution(format!(
                "deployment returned unexpected output: {output:?}"
            ))),
            ExecutionResult::Revert { output, .. } => {
                Err(HarnessError::Revert(RevertData(output.to_vec())))
            }
            ExecutionResult::Halt { reason, gas_used } => {
                Err(HarnessError::Halted { reason, gas_used })
            }
        }
    }

    /// Gives the deployed contract the specified balance (in wei).
    ///
    /// This is useful for tests that need the contract to send ETH
    /// via internal calls (e.g. `evm.call(value: 1, ...)`).
    pub fn fund_contract(&mut self, amount: U256) {
        let address = self.address;
        self.fund_account(address, amount);
    }

    /// Adds `amount` wei to the given account's balance.
    ///
    /// Useful for tests where the caller needs to send ETH along with a call
    /// (e.g. payable deposits). Default `ExecutionOptions` uses `Address::ZERO`
    /// as the caller; fund that address to enable value transfers from it.
    pub fn fund_account(&mut self, address: Address, amount: U256) {
        let js = &mut self.evm.ctx.journaled_state;

        // Update the underlying DB cache so future loads see the balance.
        let mut info = js
            .database
            .cache
            .accounts
            .get(&address)
            .map(|a| a.info.clone())
            .unwrap_or_default();
        info.balance = info.balance.saturating_add(amount);
        js.database.insert_account_info(address, info.clone());

        // Also update the live journal state — after deploy the account is
        // already loaded there, and value transfers read from the journal
        // rather than reloading from the DB cache.
        if let Some(account) = js.state.get_mut(&address) {
            account.info.balance = info.balance;
        }
    }

    fn effective_nonce(&mut self, options: ExecutionOptions) -> u64 {
        if let Some(nonce) = options.nonce {
            let entry = self.next_nonce_by_caller.entry(options.caller).or_insert(0);
            *entry = (*entry).max(nonce + 1);
            return nonce;
        }

        let entry = self.next_nonce_by_caller.entry(options.caller).or_insert(0);
        let current = *entry;
        *entry += 1;
        current
    }

    /// Executes the runtime with arbitrary calldata.
    pub fn call_raw(
        &mut self,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        let nonce = self.effective_nonce(options);
        transact(
            &mut self.evm,
            self.address,
            calldata,
            options,
            nonce,
            self.trace_options.as_ref(),
        )
    }

    /// Executes the runtime with arbitrary calldata, returning execution logs.
    pub fn call_raw_with_logs(
        &mut self,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> Result<CallResultWithLogs, HarnessError> {
        let nonce = self.effective_nonce(options);
        transact_with_logs(
            &mut self.evm,
            self.address,
            calldata,
            options,
            nonce,
            self.trace_options.as_ref(),
        )
    }

    /// Executes the runtime at an arbitrary address using the same underlying EVM state.
    pub fn call_raw_at(
        &mut self,
        address: Address,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        let nonce = self.effective_nonce(options);
        transact(
            &mut self.evm,
            address,
            calldata,
            options,
            nonce,
            self.trace_options.as_ref(),
        )
    }

    /// Configures optional step-by-step EVM tracing for subsequent calls.
    pub fn set_trace_options(&mut self, trace_options: Option<EvmTraceOptions>) {
        self.trace_options = trace_options;
    }

    /// Executes a strongly-typed function call using ABI encoding.
    pub fn call_function(
        &mut self,
        signature: &str,
        args: &[Token],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        let calldata = encode_function_call(signature, args)?;
        self.call_raw(&calldata, options)
    }

    /// Re-executes the last transaction on a **cloned** EVM context with the
    /// `CallTracer` inspector attached, producing a normalized call trace.
    ///
    /// Uses `&self` because it clones the context — does not mutate real state.
    pub fn call_raw_traced(&self, calldata: &[u8], options: ExecutionOptions) -> CallTrace {
        let ctx = self.evm.ctx.clone();
        let mut tracer = CallTracer::new();
        let mut trace_evm = ctx.build_mainnet_with_inspector(&mut tracer);

        // Honor explicit nonce when set, otherwise use stored nonce for this caller.
        let nonce = options.nonce.unwrap_or_else(|| {
            self.next_nonce_by_caller
                .get(&options.caller)
                .copied()
                .unwrap_or(0)
        });

        let tx = TxEnv::builder()
            .caller(options.caller)
            .gas_limit(options.gas_limit)
            .gas_price(options.gas_price)
            .to(self.address)
            .value(options.value)
            .data(EvmBytes::copy_from_slice(calldata))
            .nonce(nonce)
            .build()
            .expect("tx builder is valid");

        let _ = trace_evm.inspect_tx_commit(tx);
        tracer.into_trace()
    }

    /// Re-executes the call on a cloned EVM context and returns total EVM steps.
    ///
    /// Uses `&self` because this is a read-only replay that does not mutate the
    /// runtime state used by real executions.
    pub fn call_raw_step_count(&self, calldata: &[u8], options: ExecutionOptions) -> u64 {
        self.call_raw_gas_profile(calldata, options).step_count
    }

    /// Re-executes the call on a cloned EVM context and returns full-step gas attribution.
    ///
    /// Uses `&self` because this is a read-only replay that does not mutate the
    /// runtime state used by real executions.
    pub fn call_raw_gas_profile(
        &self,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> CallGasProfile {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum InvocationKind {
            Call,
            Create,
        }

        #[derive(Debug, Clone, Copy)]
        struct PendingInvocation {
            kind: InvocationKind,
            started_interp: bool,
        }

        #[derive(Debug, Clone, Copy)]
        struct FrameState {
            in_constructor: bool,
            pending_gas_remaining: u64,
            pending_opcode: u8,
        }

        impl FrameState {
            fn new(in_constructor: bool, gas_limit: u64) -> Self {
                Self {
                    in_constructor,
                    pending_gas_remaining: gas_limit,
                    pending_opcode: 0,
                }
            }
        }

        #[derive(Debug, Default)]
        struct GasAttributionInspector {
            frame_stack: Vec<FrameState>,
            pending_invocations: Vec<PendingInvocation>,
            profile: CallGasProfile,
        }

        impl GasAttributionInspector {
            fn record_root_step_delta(&mut self, opcode: u8, delta: u64) {
                self.profile.total_step_gas = self.profile.total_step_gas.saturating_add(delta);
                match opcode {
                    0xf0 => {
                        self.profile.create_opcode_steps += 1;
                        self.profile.create_opcode_gas =
                            self.profile.create_opcode_gas.saturating_add(delta);
                    }
                    0xf5 => {
                        self.profile.create2_opcode_steps += 1;
                        self.profile.create2_opcode_gas =
                            self.profile.create2_opcode_gas.saturating_add(delta);
                    }
                    _ => {}
                }
            }

            fn complete_invocation(&mut self, kind: InvocationKind) -> Option<PendingInvocation> {
                self.pending_invocations
                    .iter()
                    .rposition(|invocation| invocation.kind == kind)
                    .map(|index| self.pending_invocations.remove(index))
            }
        }

        impl<CTX, INTR: revm::interpreter::InterpreterTypes> revm::Inspector<CTX, INTR>
            for GasAttributionInspector
        {
            fn initialize_interp(
                &mut self,
                interp: &mut revm::interpreter::Interpreter<INTR>,
                _context: &mut CTX,
            ) {
                let in_constructor = if self.frame_stack.is_empty() {
                    false
                } else {
                    let parent_in_constructor = self
                        .frame_stack
                        .last()
                        .map(|frame| frame.in_constructor)
                        .unwrap_or(false);
                    let child_kind = self
                        .pending_invocations
                        .iter_mut()
                        .rev()
                        .find(|invocation| !invocation.started_interp)
                        .map(|invocation| {
                            invocation.started_interp = true;
                            invocation.kind
                        });
                    parent_in_constructor || matches!(child_kind, Some(InvocationKind::Create))
                };
                self.frame_stack
                    .push(FrameState::new(in_constructor, interp.gas.limit()));
            }

            fn step(
                &mut self,
                interp: &mut revm::interpreter::Interpreter<INTR>,
                _context: &mut CTX,
            ) {
                if let Some(frame) = self.frame_stack.last_mut() {
                    frame.pending_gas_remaining = interp.gas.remaining();
                    frame.pending_opcode = interp.bytecode.opcode();
                }
                self.profile.step_count += 1;
            }

            fn step_end(
                &mut self,
                interp: &mut revm::interpreter::Interpreter<INTR>,
                _context: &mut CTX,
            ) {
                let frame_depth = self.frame_stack.len();
                let Some(frame) = self.frame_stack.last_mut() else {
                    return;
                };
                let remaining = interp.gas.remaining();
                let delta = frame.pending_gas_remaining.saturating_sub(remaining);
                let opcode = frame.pending_opcode;
                if frame_depth == 1 {
                    self.record_root_step_delta(opcode, delta);
                }
            }

            fn call(
                &mut self,
                _context: &mut CTX,
                _inputs: &mut revm::interpreter::CallInputs,
            ) -> Option<revm::interpreter::CallOutcome> {
                self.pending_invocations.push(PendingInvocation {
                    kind: InvocationKind::Call,
                    started_interp: false,
                });
                None
            }

            fn call_end(
                &mut self,
                _context: &mut CTX,
                _inputs: &revm::interpreter::CallInputs,
                _outcome: &mut revm::interpreter::CallOutcome,
            ) {
                if let Some(invocation) = self.complete_invocation(InvocationKind::Call)
                    && invocation.started_interp
                {
                    let _ = self.frame_stack.pop();
                }
            }

            fn create(
                &mut self,
                _context: &mut CTX,
                _inputs: &mut revm::interpreter::CreateInputs,
            ) -> Option<revm::interpreter::CreateOutcome> {
                self.pending_invocations.push(PendingInvocation {
                    kind: InvocationKind::Create,
                    started_interp: false,
                });
                None
            }

            fn create_end(
                &mut self,
                _context: &mut CTX,
                _inputs: &revm::interpreter::CreateInputs,
                outcome: &mut revm::interpreter::CreateOutcome,
            ) {
                if let Some(invocation) = self.complete_invocation(InvocationKind::Create)
                    && invocation.started_interp
                {
                    self.profile.constructor_frame_gas = self
                        .profile
                        .constructor_frame_gas
                        .saturating_add(outcome.result.gas.spent());
                    let _ = self.frame_stack.pop();
                }
            }
        }

        let ctx = self.evm.ctx.clone();
        let mut inspector = GasAttributionInspector::default();
        let mut trace_evm = ctx.build_mainnet_with_inspector(&mut inspector);

        // Honor explicit nonce when set, otherwise use stored nonce for this caller.
        let nonce = options.nonce.unwrap_or_else(|| {
            self.next_nonce_by_caller
                .get(&options.caller)
                .copied()
                .unwrap_or(0)
        });

        let tx = TxEnv::builder()
            .caller(options.caller)
            .gas_limit(options.gas_limit)
            .gas_price(options.gas_price)
            .to(self.address)
            .value(options.value)
            .data(EvmBytes::copy_from_slice(calldata))
            .nonce(nonce)
            .build()
            .expect("tx builder is valid");

        let _ = trace_evm.inspect_tx_commit(tx);
        let mut profile = trace_evm.inspector.profile;
        let create_total = profile
            .create_opcode_gas
            .saturating_add(profile.create2_opcode_gas);
        profile.non_create_opcode_gas = profile.total_step_gas.saturating_sub(create_total);
        profile.non_constructor_frame_gas = profile
            .total_step_gas
            .saturating_sub(profile.constructor_frame_gas);
        profile
    }

    /// Returns the contract address assigned to this runtime instance.
    pub fn address(&self) -> Address {
        self.address
    }
}

#[derive(Debug, Clone)]
pub struct ContractBytecode {
    pub bytecode: String,
    pub runtime_bytecode: String,
}

/// Harness that compiles Fe source code and executes the resulting contract runtime.
pub struct FeContractHarness {
    contract: ContractBytecode,
}

impl FeContractHarness {
    /// Convenience helper that uses default [`CompileOptions`].
    pub fn compile(contract_name: &str, source: &str) -> Result<Self, HarnessError> {
        Self::compile_from_source(contract_name, source, CompileOptions::default())
    }

    /// Compiles the provided Fe source into bytecode for the specified contract.
    pub fn compile_from_source(
        contract_name: &str,
        source: &str,
        options: CompileOptions,
    ) -> Result<Self, HarnessError> {
        let mut db = DriverDataBase::default();
        let url = Url::parse(MEMORY_SOURCE_URL).expect("static URL is valid");
        db.workspace()
            .touch(&mut db, url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &url)
            .expect("file should exist in workspace");
        let top_mod = db.top_mod(file);
        let diags = db.run_on_top_mod(top_mod);
        if !diags.is_empty() {
            return Err(HarnessError::CompilerDiagnostics(diags.format_diags(&db)));
        }
        let bytecode =
            emit_module_sonatina_bytecode(&db, top_mod, options.opt_level, Some(contract_name))
                .map_err(|err| HarnessError::EmitSonatina(err.to_string()))?;
        let bytecode = bytecode.get(contract_name).ok_or_else(|| {
            HarnessError::EmitSonatina(format!("missing bytecode for `{contract_name}`"))
        })?;
        let contract = ContractBytecode {
            bytecode: hex::encode(&bytecode.deploy),
            runtime_bytecode: hex::encode(&bytecode.runtime),
        };
        Ok(Self { contract })
    }

    /// Reads a source file from disk and compiles the specified contract.
    pub fn compile_from_file(
        contract_name: &str,
        path: impl AsRef<Path>,
        options: CompileOptions,
    ) -> Result<Self, HarnessError> {
        let source = std::fs::read_to_string(path)?;
        Self::compile_from_source(contract_name, &source, options)
    }

    /// Returns the compiled raw runtime bytecode.
    pub fn runtime_bytecode(&self) -> &str {
        &self.contract.runtime_bytecode
    }

    /// Returns the compiled init bytecode.
    pub fn init_bytecode(&self) -> &str {
        &self.contract.bytecode
    }

    /// Executes the compiled runtime with arbitrary calldata.
    pub fn call_raw(
        &self,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        execute_runtime(&self.contract.runtime_bytecode, calldata, options)
    }

    /// ABI-encodes the provided arguments and executes the runtime.
    pub fn call_function(
        &self,
        signature: &str,
        args: &[Token],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        let calldata = encode_function_call(signature, args)?;
        self.call_raw(&calldata, options)
    }

    /// Creates a persistent runtime instance that can serve multiple calls.
    pub fn deploy_instance(&self) -> Result<RuntimeInstance, HarnessError> {
        RuntimeInstance::new(&self.contract.runtime_bytecode)
    }

    /// Deploys a contract by running the init bytecode, initializing storage.
    /// Use this when your contract has initialization logic (e.g., storage setup).
    pub fn deploy_with_init(&self) -> Result<RuntimeInstance, HarnessError> {
        RuntimeInstance::deploy(&self.contract.bytecode)
    }

    /// Deploys a contract by running the init bytecode with ABI-encoded constructor args.
    pub fn deploy_with_init_args(
        &self,
        constructor_args: &[Token],
    ) -> Result<RuntimeInstance, HarnessError> {
        let args = ethers_core::abi::encode(constructor_args);
        RuntimeInstance::deploy_with_constructor_args(&self.contract.bytecode, &args)
    }
}

/// Compiles the provided Fe source to Sonatina-generated runtime bytecode (hex-encoded).
pub fn compile_runtime_sonatina_from_source(source: &str) -> Result<String, HarnessError> {
    let mut db = DriverDataBase::default();
    let url = Url::parse(MEMORY_SOURCE_URL).expect("static URL is valid");
    db.workspace()
        .touch(&mut db, url.clone(), Some(source.to_string()));
    let file = db
        .workspace()
        .get(&db, &url)
        .expect("file should exist in workspace");
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    if !diags.is_empty() {
        return Err(HarnessError::CompilerDiagnostics(diags.format_diags(&db)));
    }

    let bytecode = emit_module_sonatina_bytecode(&db, top_mod, OptLevel::default(), None)
        .map_err(|err| HarnessError::EmitSonatina(err.to_string()))?;
    let contract = bytecode
        .values()
        .next()
        .ok_or_else(|| HarnessError::EmitSonatina("no runtime bytecode emitted".into()))?;
    Ok(hex::encode(&contract.runtime))
}

/// ABI-encodes a function call according to the provided signature.
pub fn encode_function_call(signature: &str, args: &[Token]) -> Result<Vec<u8>, HarnessError> {
    let function = AbiParser::default().parse_function(signature)?;
    let encoded = function.encode_input(args)?;
    Ok(encoded)
}

/// Executes the provided runtime bytecode within `revm`.
pub fn execute_runtime(
    runtime_bytecode_hex: &str,
    calldata: &[u8],
    options: ExecutionOptions,
) -> Result<CallResult, HarnessError> {
    let mut instance = RuntimeInstance::new(runtime_bytecode_hex)?;
    instance.call_raw(calldata, options)
}

/// Parses a hex string (with or without `0x` prefix) into raw bytes.
pub fn hex_to_bytes(hex: &str) -> Result<Vec<u8>, HarnessError> {
    let trimmed = hex.trim().strip_prefix("0x").unwrap_or(hex.trim());
    Vec::from_hex(trimmed).map_err(HarnessError::Hex)
}

/// Interprets exactly 32 return bytes as a big-endian `U256`.
pub fn bytes_to_u256(bytes: &[u8]) -> Result<U256, HarnessError> {
    if bytes.len() != 32 {
        return Err(HarnessError::Execution(format!(
            "expected 32 bytes of return data, found {}",
            bytes.len()
        )));
    }
    let mut buf = [0u8; 32];
    buf.copy_from_slice(bytes);
    Ok(U256::from_be_bytes(buf))
}

/// Decodes ABI-encoded string return data.
pub fn bytes_to_string(bytes: &[u8]) -> Result<String, HarnessError> {
    let mut tokens = decode(&[ParamType::String], bytes)?;
    match tokens.pop() {
        Some(Token::String(value)) => Ok(value),
        _ => Err(HarnessError::Execution(
            "expected ABI string return data".to_string(),
        )),
    }
}

#[cfg(test)]
#[allow(clippy::print_stderr)]
mod tests {
    use super::*;
    use ethers_core::{
        abi::{AbiParser, Function, Param, ParamType, StateMutability, Token, decode},
        types::U256 as AbiU256,
    };

    fn compile_calldata_decode_contract() -> Option<FeContractHarness> {
        let source = r#"
use std::abi::sol
use std::abi::{decode_input, decode_input_at}
use std::evm::{CallData, Evm}

msg DecodeMsg {
    #[selector = sol("raw(uint256)")]
    Raw { value: u256 } -> u256,
    #[selector = sol("read(uint256)")]
    Read { value: u256 } -> u256,
    #[selector = sol("selector()")]
    Selector -> u256,
    #[selector = sol("args(uint256)")]
    Args { value: u256 } -> u256,
    #[selector = sol("tuple(uint64,bool)")]
    Tuple { a: u64, flag: bool } -> u256,
    #[selector = sol("generic(uint256)")]
    Generic { value: u256 } -> u256,
    #[selector = sol("bad()")]
    Bad -> u256,
    #[selector = sol("bad_view()")]
    BadView -> u256,
}

pub contract DecodeHarness {
    recv DecodeMsg {
        Raw { value: _ } -> u256 {
            CallData::new().decode<u256>()
        }

        Read { value } -> u256 {
            let decoded = CallData::with_base(4).decode<u256>()
            assert!(decoded == value)
            decoded
        }

        Selector -> u256 uses (evm: Evm) {
            evm.selector() as u256
        }

        Args { value } -> u256 uses (evm: mut Evm) {
            let decoded = evm.decode_args<u256>()
            assert!(decoded == value)
            decoded
        }

        Tuple { a, flag } -> u256 uses (evm: mut Evm) {
            let decoded: (u64, bool) = evm.decode_args<(u64, bool)>()
            assert!(decoded.0 == a)
            assert!(decoded.1 == flag)
            if flag {
                a as u256
            } else {
                0
            }
        }

        Generic { value } -> u256 {
            let input = CallData::with_base(4)
            let decoded: u256 = decode_input(input)
            let decoded_at: u256 = decode_input_at(CallData::new(), 4)
            assert!(decoded == value)
            assert!(decoded_at == value)
            decoded
        }

        Bad -> u256 {
            decode_input_at(CallData::new(), 5)
        }

        BadView -> u256 {
            CallData::with_base(5).decode<u256>()
        }
    }
}
"#;

        Some(
            FeContractHarness::compile_from_source(
                "DecodeHarness",
                source,
                CompileOptions::default(),
            )
            .expect("calldata decode contract should compile"),
        )
    }

    fn compile_canonical_decode_contract() -> Option<FeContractHarness> {
        let source = r#"
use std::abi::sol
use std::evm::Address

msg CanonicalMsg {
    #[selector = sol("readBool(bool)")]
    ReadBool { value: bool } -> u256,
    #[selector = sol("readU8(uint8)")]
    ReadU8 { value: u8 } -> u256,
    #[selector = sol("readI8(int8)")]
    ReadI8 { value: i8 } -> u256,
    #[selector = sol("readAddress(address)")]
    ReadAddress { value: Address } -> u256,
}

pub contract CanonicalHarness {
    recv CanonicalMsg {
        ReadBool { value } -> u256 {
            if value { 1 } else { 0 }
        }

        ReadU8 { value } -> u256 {
            value as u256
        }

        ReadI8 { value } -> u256 {
            if value == 127 { 1 } else { 0 }
        }

        ReadAddress { value } -> u256 {
            value.inner
        }
    }
}
"#;

        Some(
            FeContractHarness::compile_from_source(
                "CanonicalHarness",
                source,
                CompileOptions::default(),
            )
            .expect("canonical decode contract should compile"),
        )
    }

    fn compile_dynamic_view_contract() -> Option<FeContractHarness> {
        let source = r#"
use std::abi::sol
use std::abi::sol::{decode_bytes_view, decode_bytes_view_at, decode_string_view}
use core::ptr
use std::evm::{CallData, Evm}

const BYTES_LEN_SELECTOR: u32 = sol("bytesLen(bytes)")
const SECOND_BYTES_LEN_SELECTOR: u32 = sol("secondBytesLen(bytes,bytes)")
const STRING_FIRST_SELECTOR: u32 = sol("stringFirst(string)")
const STRING_LEN_SELECTOR: u32 = sol("stringLen(string)")

#[contract_init(ViewHarness)]
fn init() uses (evm: mut Evm) {
    evm.create_contract(runtime)
}

#[contract_runtime(ViewHarness)]
fn runtime() uses (evm: mut Evm) {
    let sel = evm.selector()

    if sel == BYTES_LEN_SELECTOR {
        let view = decode_bytes_view(CallData::with_base(4))
        let out = ptr::MemBuffer::alloc(32)
        evm.mstore(addr: out.ptr, value: view.len())
        evm.return_data(data: out)
    }

    if sel == SECOND_BYTES_LEN_SELECTOR {
        let view = decode_bytes_view_at(CallData::with_base(4), base: 0, head_pos: 32)
        let out = ptr::MemBuffer::alloc(32)
        evm.mstore(addr: out.ptr, value: view.len())
        evm.return_data(data: out)
    }

    if sel == STRING_FIRST_SELECTOR {
        let view = decode_string_view(CallData::with_base(4))
        let first: u256 = if view.is_empty() { 0 } else { view.byte_at(0) as u256 }
        let out = ptr::MemBuffer::alloc(32)
        evm.mstore(addr: out.ptr, value: first)
        evm.return_data(data: out)
    }

    if sel == STRING_LEN_SELECTOR {
        let view = decode_string_view(CallData::with_base(4))
        let out = ptr::MemBuffer::alloc(32)
        evm.mstore(addr: out.ptr, value: view.len())
        evm.return_data(data: out)
    }

    evm.revert(data: ptr::MemBuffer::empty())
}
"#;

        Some(
            FeContractHarness::compile_from_source(
                "ViewHarness",
                source,
                CompileOptions::default(),
            )
            .expect("dynamic view contract should compile"),
        )
    }

    fn compile_storage_bytes_contract() -> Option<FeContractHarness> {
        let source = r#"
use std::abi::sol
use std::abi::sol::{decode_bytes_view, decode_bytes_view_at}
use core::ptr
use std::evm::{CallData, Evm, RawStorage, StorageBytes, emit_bytes_event_view}

const SET_SELECTOR: u32 = sol("set(bytes)")
const GET_SELECTOR: u32 = sol("get()")
const CLEAR_SELECTOR: u32 = sol("clear()")
const EMIT_SELECTOR: u32 = sol("emit(bytes)")
const TOPIC0: u256 = 0x1234

type Blobs = StorageBytes<u256, 0, 1>

#[contract_init(StorageBytesHarness)]
fn init() uses (evm: mut Evm) {
    evm.create_contract(runtime)
}

#[contract_runtime(StorageBytesHarness)]
fn runtime() uses (evm: mut Evm) {
    let sel = evm.selector()
    with (RawStorage = evm) {
        let mut blobs: Blobs = Blobs::new()

        if sel == SET_SELECTOR {
            let view = decode_bytes_view(CallData::with_base(4))
            with (mut blobs) {
                blobs.store_view(key: 0, view: view)
            }
            evm.return_data(data: ptr::MemBuffer::empty())
        }

        if sel == GET_SELECTOR {
            blobs.encode_return(key: 0)
        }

        if sel == CLEAR_SELECTOR {
            with (mut blobs) {
                blobs.clear(key: 0)
            }
            evm.return_data(data: ptr::MemBuffer::empty())
        }

        if sel == EMIT_SELECTOR {
            let view = decode_bytes_view_at(CallData::new(), base: 4, head_pos: 0)
            emit_bytes_event_view(topic0: TOPIC0, view: view)
            evm.return_data(data: ptr::MemBuffer::empty())
        }
    }

    evm.revert(data: ptr::MemBuffer::empty())
}
"#;

        Some(
            FeContractHarness::compile_from_source(
                "StorageBytesHarness",
                source,
                CompileOptions::default(),
            )
            .expect("storage bytes contract should compile"),
        )
    }

    fn emit_then_text_contract_source() -> &'static str {
        r#"
use std::abi::{sol, Bytes}
use std::evm::emit_bytes_event_view

const TOPIC0: u256 = 0x1234

msg EmitThenTextMsg {
    #[selector = sol("emitAndReturn(bytes)")]
    EmitAndReturn { data: Bytes } -> Text,
}

pub contract EmitThenText {
    recv EmitThenTextMsg {
        EmitAndReturn { data } -> Text {
            emit_bytes_event_view(topic0: TOPIC0, view: data.view())
            "emit-and-return-abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789"
        }
    }
}
"#
    }

    fn compile_emit_then_text_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "EmitThenText",
                emit_then_text_contract_source(),
                CompileOptions::default(),
            )
            .expect("emit-then-text contract should compile"),
        )
    }

    fn raw_static_target_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg RawStaticTargetMsg {
    #[selector = sol("word()")]
    Word -> u256,

    #[selector = sol("flag()")]
    Flag -> bool,
}

pub contract RawStaticTarget {
    recv RawStaticTargetMsg {
        Word -> u256 {
            7
        }

        Flag -> bool {
            true
        }
    }
}
"#
    }

    fn compile_raw_static_target_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "RawStaticTarget",
                raw_static_target_contract_source(),
                CompileOptions::default(),
            )
            .expect("raw static target contract should compile"),
        )
    }

    fn raw_static_caller_contract_source() -> &'static str {
        r#"
use std::abi::sol
use core::ptr
use std::evm::{Evm, Address, Ctx, RawMem, staticcall_decode}

const WORD_SELECTOR: u32 = sol("word()")
const FLAG_SELECTOR: u32 = sol("flag()")

msg RawStaticCallerMsg {
    #[selector = sol("callWord(address)")]
    CallWord { target: Address } -> u256,

    #[selector = sol("callFlag(address)")]
    CallFlag { target: Address } -> bool,
}

pub contract RawStaticCaller {
    recv RawStaticCallerMsg {
        CallWord { target } -> u256 uses (evm: mut Evm) {
            let args = ptr::MemBuffer::alloc(4)
            evm.mstore(addr: args.ptr, value: (WORD_SELECTOR as u256) << 224)
            staticcall_decode(addr: target, gas: evm.gas(), args: args)
        }
        CallFlag { target } -> bool uses (evm: mut Evm) {
            let args = ptr::MemBuffer::alloc(4)
            evm.mstore(addr: args.ptr, value: (FLAG_SELECTOR as u256) << 224)
            staticcall_decode(addr: target, gas: evm.gas(), args: args)
        }
    }
}
"#
    }

    fn compile_raw_static_caller_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "RawStaticCaller",
                raw_static_caller_contract_source(),
                CompileOptions::default(),
            )
            .expect("raw static caller contract should compile"),
        )
    }

    fn bad_bool_target_contract_source() -> &'static str {
        r#"
use std::abi::sol
use core::ptr
use std::evm::Evm

const FLAG_SELECTOR: u32 = sol("flag()")

#[contract_init(BadBoolTarget)]
fn init() uses (evm: mut Evm) {
    evm.create_contract(runtime)
}

#[contract_runtime(BadBoolTarget)]
fn runtime() uses (evm: mut Evm) {
    if evm.selector() == FLAG_SELECTOR {
        let out = ptr::MemBuffer::alloc(32)
        evm.mstore(addr: out.ptr, value: 2)
        evm.return_data(data: out)
    }

    evm.revert(data: ptr::MemBuffer::empty())
}
"#
    }

    fn compile_bad_bool_target_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "BadBoolTarget",
                bad_bool_target_contract_source(),
                CompileOptions::default(),
            )
            .expect("bad bool target contract should compile"),
        )
    }

    fn string_echo_contract_source() -> &'static str {
        r#"
use std::abi::sol
use std::evm::effects::Log

msg EchoMsg {
    #[selector = sol("echo(string)")]
    Echo { text: Text } -> Text,

    #[selector = sol("emit(string)")]
    Emit { text: Text } -> bool,
}

#[event]
struct Echoed {
    text: Text,
}

pub contract StringEcho uses (log: mut Log) {
    recv EchoMsg {
        Echo { text } -> Text {
            text
        }

        Emit { text } -> bool uses (mut log) {
            log.emit(Echoed { text })
            true
        }
    }
}
"#
    }

    fn string_caller_contract_source() -> &'static str {
        r#"
use std::abi::sol
use std::evm::{Address, Call}

msg EchoMsg {
    #[selector = sol("echo(string)")]
    Echo { text: Text } -> Text,
}

msg CallerMsg {
    #[selector = sol("callEcho(address,string)")]
    CallEcho { target: Address, text: Text } -> Text,
}

pub contract StringCaller uses (call: mut Call) {
    recv CallerMsg {
        CallEcho { target, text } -> Text uses (mut call) {
            target.call(EchoMsg::Echo { text })
        }
    }
}
"#
    }

    fn string_literal_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg LiteralMsg {
    #[selector = sol("literal()")]
    Literal -> Text,
}

pub contract StringLiteral {
    recv LiteralMsg {
        Literal -> Text {
            let text = "literal-abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789"
            text
        }
    }
}
"#
    }

    fn compile_string_echo_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "StringEcho",
                string_echo_contract_source(),
                CompileOptions::default(),
            )
            .expect("string echo contract should compile"),
        )
    }

    fn compile_string_caller_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "StringCaller",
                string_caller_contract_source(),
                CompileOptions::default(),
            )
            .expect("string caller contract should compile"),
        )
    }

    fn compile_string_literal_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "StringLiteral",
                string_literal_contract_source(),
                CompileOptions::default(),
            )
            .expect("string literal contract should compile"),
        )
    }

    fn string_view_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg ViewMsg {
    #[selector = sol("head(string)")]
    Head { text: Text } -> u8,
}

pub contract StringViewHead {
    recv ViewMsg {
        Head { text } -> u8 {
            text.view().byte_at(0)
        }
    }
}
"#
    }

    fn compile_string_view_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "StringViewHead",
                string_view_contract_source(),
                CompileOptions::default(),
            )
            .expect("string view contract should compile"),
        )
    }

    fn vec_echo_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg VecMsg {
    #[selector = sol("echo(uint256[])")]
    Echo { values: Vec<u256> } -> Vec<u256>,
}

pub contract VecEcho {
    recv VecMsg {
        Echo { values } -> Vec<u256> {
            values
        }
    }
}
"#
    }

    fn compile_vec_echo_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "VecEcho",
                vec_echo_contract_source(),
                CompileOptions::default(),
            )
            .expect("vec echo contract should compile"),
        )
    }

    fn nested_tuple_echo_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg NestedEchoMsg {
    #[selector = sol("echo((string,uint64))")]
    Echo { pair: (Text, u64) } -> (Text, u64),
}

pub contract NestedTupleEcho {
    recv NestedEchoMsg {
        Echo { pair } -> (Text, u64) {
            pair
        }
    }
}
"#
    }

    fn nested_tuple_caller_contract_source() -> &'static str {
        r#"
use std::abi::sol
use std::evm::{Address, Call}

msg NestedEchoMsg {
    #[selector = sol("echo((string,uint64))")]
    Echo { pair: (Text, u64) } -> (Text, u64),
}

msg NestedCallerMsg {
    #[selector = sol("callEcho(address,(string,uint64))")]
    CallEcho { target: Address, pair: (Text, u64) } -> (Text, u64),
}

pub contract NestedTupleCaller uses (call: mut Call) {
    recv NestedCallerMsg {
        CallEcho { target, pair } -> (Text, u64) uses (mut call) {
            target.call(NestedEchoMsg::Echo { pair })
        }
    }
}
"#
    }

    fn nested_tuple_init_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg StoredPairMsg {
    #[selector = sol("getTextLen()")]
    GetTextLen -> u256,
    #[selector = sol("getFirstByte()")]
    GetFirstByte -> u8,
    #[selector = sol("getCount()")]
    GetCount -> u64,
}

struct PairStore {
    text_len: u256,
    first_byte: u8,
    count: u64,
}

pub contract NestedTupleInit {
    mut store: PairStore

    init(text: Text, count: u64) uses (mut store) {
        store.text_len = text.len()
        store.first_byte = if text.is_empty() { 0 } else { text.as_bytes().byte_at(0) }
        store.count = count
    }

    recv StoredPairMsg {
        GetTextLen -> u256 uses (store) {
            store.text_len
        }

        GetFirstByte -> u8 uses (store) {
            store.first_byte
        }

        GetCount -> u64 uses (store) {
            store.count
        }
    }
}
"#
    }

    fn tuple_head_guard_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg TupleHeadGuardMsg {
    #[selector = sol("read((uint64,string))")]
    Read { pair: (u64, Text) } -> u256,
}

pub contract TupleHeadGuard {
    recv TupleHeadGuardMsg {
        Read { pair } -> u256 {
            (pair.0 as u256) + pair.1.len()
        }
    }
}
"#
    }

    fn compile_nested_tuple_echo_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "NestedTupleEcho",
                nested_tuple_echo_contract_source(),
                CompileOptions::default(),
            )
            .expect("nested tuple echo contract should compile"),
        )
    }

    fn compile_nested_tuple_caller_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "NestedTupleCaller",
                nested_tuple_caller_contract_source(),
                CompileOptions::default(),
            )
            .expect("nested tuple caller contract should compile"),
        )
    }

    fn compile_nested_tuple_init_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "NestedTupleInit",
                nested_tuple_init_contract_source(),
                CompileOptions::default(),
            )
            .expect("nested tuple init contract should compile"),
        )
    }

    fn compile_tuple_head_guard_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "TupleHeadGuard",
                tuple_head_guard_contract_source(),
                CompileOptions::default(),
            )
            .expect("tuple head guard contract should compile"),
        )
    }

    fn fixed_string_decode_contract_source() -> &'static str {
        r#"
use std::abi::sol

msg FixedStringDecodeMsg {
    #[selector = sol("ok(string)")]
    Ok { text: String<31> } -> u256,
}

pub contract FixedStringDecode {
    recv FixedStringDecodeMsg {
        Ok { text } -> u256 {
            1
        }
    }
}
"#
    }

    fn compile_fixed_string_decode_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "FixedStringDecode",
                fixed_string_decode_contract_source(),
                CompileOptions::default(),
            )
            .expect("fixed string decode contract should compile"),
        )
    }

    fn fixed_string_return_caller_contract_source() -> &'static str {
        r#"
use std::abi::sol
use std::evm::{Address, Call}

msg FixedStringEchoMsg {
    #[selector = sol("echo(string)")]
    Echo { text: Text } -> String<31>,
}

msg FixedStringCallerMsg {
    #[selector = sol("forward(address,string)")]
    Forward { target: Address, text: Text } -> String<31>,
}

pub contract FixedStringCaller uses (call: mut Call) {
    recv FixedStringCallerMsg {
        Forward { target, text } -> String<31> uses (mut call) {
            target.call(FixedStringEchoMsg::Echo { text })
        }
    }
}
"#
    }

    fn compile_fixed_string_return_caller_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "FixedStringCaller",
                fixed_string_return_caller_contract_source(),
                CompileOptions::default(),
            )
            .expect("fixed string return caller contract should compile"),
        )
    }

    fn create2_init_args_contract_source() -> &'static str {
        r#"
use std::abi::sol
use std::evm::Create

msg StaticChildMsg {
    #[selector = sol("getByte()")]
    GetByte -> u8,
}

msg DynamicChildMsg {
    #[selector = sol("getTextLen()")]
    GetTextLen -> u256,
    #[selector = sol("getFirstByte()")]
    GetFirstByte -> u8,
    #[selector = sol("getCount()")]
    GetCount -> u64,
}

msg ParentMsg {
    #[selector = sol("deployStatic()")]
    DeployStatic -> u256,
    #[selector = sol("deployDynamic(string,uint64)")]
    DeployDynamic { text: Text, count: u64 } -> u256,
}

pub contract StaticChild {
    mut stored: u8

    init(value: u8) uses (mut stored) {
        stored = value
    }

    recv StaticChildMsg {
        GetByte -> u8 uses (stored) {
            stored
        }
    }
}

struct DynamicStore {
    text_len: u256,
    first_byte: u8,
    count: u64,
}

pub contract DynamicChild {
    mut store: DynamicStore

    init(text: Text, count: u64) uses (mut store) {
        store.text_len = text.len()
        store.first_byte = if text.is_empty() { 0 } else { text.as_bytes().byte_at(0) }
        store.count = count
    }

    recv DynamicChildMsg {
        GetTextLen -> u256 uses (store) {
            store.text_len
        }

        GetFirstByte -> u8 uses (store) {
            store.first_byte
        }

        GetCount -> u64 uses (store) {
            store.count
        }
    }
}

pub contract Create2Parent uses (create: mut Create) {
    recv ParentMsg {
        DeployStatic -> u256 uses (mut create) {
            create.create2<StaticChild>(value: 0, args: (7,), salt: 1).inner
        }

        DeployDynamic { text, count } -> u256 uses (mut create) {
            create.create2<DynamicChild>(value: 0, args: (text, count), salt: 2).inner
        }
    }
}
"#
    }

    fn compile_create2_init_args_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "Create2Parent",
                create2_init_args_contract_source(),
                CompileOptions::default(),
            )
            .expect("create2 init arg contract should compile"),
        )
    }

    fn custom_width_encode_contract_source() -> &'static str {
        r#"
use std::abi::sol::{self, Int40, Int136, Uint24, Uint160}
use std::evm::effects::Log

msg CustomWidthMsg {
    #[selector = sol("goodUint24()")]
    GoodUint24 -> Uint24,
    #[selector = sol("badUint24()")]
    BadUint24 -> Uint24,
    #[selector = sol("goodUint160()")]
    GoodUint160 -> Uint160,
    #[selector = sol("badUint160()")]
    BadUint160 -> Uint160,
    #[selector = sol("goodInt40()")]
    GoodInt40 -> Int40,
    #[selector = sol("badInt40()")]
    BadInt40 -> Int40,
    #[selector = sol("goodInt136()")]
    GoodInt136 -> Int136,
    #[selector = sol("badInt136()")]
    BadInt136 -> Int136,
    #[selector = sol("emitGoodUint160()")]
    EmitGoodUint160 -> bool,
    #[selector = sol("emitBadUint160()")]
    EmitBadUint160 -> bool,
}

#[event]
struct SeenUint160 {
    #[indexed]
    value: Uint160,
}

pub contract CustomWidthBoundary uses (log: mut Log) {
    recv CustomWidthMsg {
        GoodUint24 -> Uint24 {
            Uint24 { val: 7 }
        }

        BadUint24 -> Uint24 {
            Uint24 { val: (1 as u32) << 31 }
        }

        GoodUint160 -> Uint160 {
            Uint160 { val: 1 }
        }

        BadUint160 -> Uint160 {
            Uint160 { val: (1 as u256) << 200 }
        }

        GoodInt40 -> Int40 {
            Int40 { val: 5 }
        }

        BadInt40 -> Int40 {
            Int40 { val: (1 as i64) << 60 }
        }

        GoodInt136 -> Int136 {
            Int136 { val: 5 }
        }

        BadInt136 -> Int136 {
            Int136 { val: (1 as i256) << 180 }
        }

        EmitGoodUint160 -> bool uses (mut log) {
            log.emit(SeenUint160 { value: Uint160 { val: 1 } })
            true
        }

        EmitBadUint160 -> bool uses (mut log) {
            log.emit(SeenUint160 { value: Uint160 { val: (1 as u256) << 200 } })
            true
        }
    }
}
"#
    }

    fn compile_custom_width_encode_contract() -> Option<FeContractHarness> {
        Some(
            FeContractHarness::compile_from_source(
                "CustomWidthBoundary",
                custom_width_encode_contract_source(),
                CompileOptions::default(),
            )
            .expect("custom width encode contract should compile"),
        )
    }

    fn nested_tuple_param_type() -> ParamType {
        ParamType::Tuple(vec![ParamType::String, ParamType::Uint(64)])
    }

    fn nested_tuple_token(text: &str, count: u64) -> Token {
        Token::Tuple(vec![
            Token::String(text.to_string()),
            Token::Uint(AbiU256::from(count)),
        ])
    }

    fn long_string_value(tag: &str) -> String {
        format!("{tag}-abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789")
    }

    fn dyn_uint_array_param_type() -> ParamType {
        ParamType::Array(Box::new(ParamType::Uint(256)))
    }

    fn dyn_uint_array_token(values: &[u64]) -> Token {
        Token::Array(
            values
                .iter()
                .map(|value| Token::Uint(AbiU256::from(*value)))
                .collect::<Vec<_>>(),
        )
    }

    fn fixed_bool_array_param_type(len: usize) -> ParamType {
        ParamType::FixedArray(Box::new(ParamType::Bool), len)
    }

    fn fixed_bool_array_token(len: usize) -> Token {
        Token::FixedArray(
            (0..len)
                .map(|i| Token::Bool(i % 2 == 0))
                .collect::<Vec<_>>(),
        )
    }

    fn fixed_string_array_param_type(len: usize) -> ParamType {
        ParamType::FixedArray(Box::new(ParamType::String), len)
    }

    fn fixed_string_array_token(len: usize) -> Token {
        Token::FixedArray(
            (0..len)
                .map(|i| Token::String(long_string_value(&format!("str-{i:02}"))))
                .collect::<Vec<_>>(),
        )
    }

    fn fixed_bytes_array_param_type(len: usize) -> ParamType {
        ParamType::FixedArray(Box::new(ParamType::Bytes), len)
    }

    fn fixed_bytes_array_token(len: usize) -> Token {
        Token::FixedArray(
            (0..len)
                .map(|i| Token::Bytes(vec![i as u8, (i as u8) ^ 0x5a, 0xff]))
                .collect::<Vec<_>>(),
        )
    }

    fn fixed_bool_array_contract_source(len: usize) -> String {
        format!(
            r#"
use std::abi::sol

msg FixedBoolArrayMsg {{
    #[selector = sol("echo(bool[{len}])")]
    Echo {{ value: [bool; {len}] }} -> [bool; {len}],
}}

pub contract FixedBoolArrayBoundary {{
    recv FixedBoolArrayMsg {{
        Echo {{ value }} -> [bool; {len}] {{
            value
        }}
    }}
}}
"#
        )
    }

    fn fixed_dynamic_array_contract_source(len: usize) -> String {
        format!(
            r#"
use std::abi::{{sol, Bytes}}

msg FixedDynamicArrayMsg {{
    #[selector = sol("echoString(string[{len}])")]
    EchoString {{ value: [Text; {len}] }} -> [Text; {len}],
    #[selector = sol("echoBytes(bytes[{len}])")]
    EchoBytes {{ value: [Bytes; {len}] }} -> [Bytes; {len}],
}}

pub contract FixedDynamicArrayBoundary {{
    recv FixedDynamicArrayMsg {{
        EchoString {{ value }} -> [Text; {len}] {{
            value
        }}

        EchoBytes {{ value }} -> [Bytes; {len}] {{
            value
        }}
    }}
}}
"#
        )
    }

    #[allow(deprecated)]
    fn encode_typed_function_call(
        name: &str,
        inputs: Vec<ParamType>,
        args: &[Token],
    ) -> Result<Vec<u8>, HarnessError> {
        let function = Function {
            name: name.to_string(),
            inputs: inputs
                .into_iter()
                .map(|kind| Param {
                    name: String::new(),
                    kind,
                    internal_type: None,
                })
                .collect(),
            outputs: Vec::new(),
            constant: None,
            state_mutability: StateMutability::NonPayable,
        };
        Ok(function.encode_input(args)?)
    }

    fn assert_empty_revert(err: HarnessError) {
        match err {
            HarnessError::Revert(data) => {
                assert!(data.0.is_empty(), "expected empty revert data, got {data}");
            }
            other => panic!("expected revert, got {other:?}"),
        }
    }

    fn raw_single_word_call(signature: &str, word: [u8; 32]) -> Vec<u8> {
        let function = AbiParser::default()
            .parse_function(signature)
            .expect("signature should parse");
        let mut calldata = function.short_signature().to_vec();
        calldata.extend_from_slice(&word);
        calldata
    }

    fn word_from_u256(value: AbiU256) -> [u8; 32] {
        let mut word = [0u8; 32];
        value.to_big_endian(&mut word);
        word
    }

    fn address_word(bytes: [u8; 20]) -> [u8; 32] {
        let mut word = [0u8; 32];
        word[12..].copy_from_slice(&bytes);
        word
    }

    #[test]
    fn erc20_contract_test() {
        let source_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/erc20.fe"
        );
        let harness = FeContractHarness::compile_from_file(
            "CoolCoin",
            source_path,
            CompileOptions::default(),
        )
        .expect("compilation should succeed");

        let owner = Address::with_last_byte(0x01);
        let alice = Address::with_last_byte(0x02);
        let bob = Address::with_last_byte(0x03);

        let owner_abi = ethers_core::types::Address::from_low_u64_be(1);
        let alice_abi = ethers_core::types::Address::from_low_u64_be(2);
        let bob_abi = ethers_core::types::Address::from_low_u64_be(3);

        let initial_supply = AbiU256::from(1_000u64);
        let mut instance = harness
            .deploy_with_init_args(&[Token::Uint(initial_supply), Token::Address(owner_abi)])
            .expect("deployment succeeds");

        let owner_opts = ExecutionOptions {
            caller: owner,
            ..ExecutionOptions::default()
        };

        let name_call = encode_function_call("name()", &[]).unwrap();
        let name_res = instance
            .call_raw(&name_call, owner_opts)
            .expect("name() should succeed");
        assert_eq!(
            bytes_to_string(&name_res.return_data).unwrap(),
            "CoolCoin",
            "name() should return CoolCoin"
        );

        let symbol_call = encode_function_call("symbol()", &[]).unwrap();
        let symbol_res = instance
            .call_raw(&symbol_call, owner_opts)
            .expect("symbol() should succeed");
        assert_eq!(
            bytes_to_string(&symbol_res.return_data).unwrap(),
            "COOL",
            "symbol() should return COOL"
        );

        let decimals_call = encode_function_call("decimals()", &[]).unwrap();
        let decimals_res = instance
            .call_raw(&decimals_call, owner_opts)
            .expect("decimals() should succeed");
        assert_eq!(
            bytes_to_u256(&decimals_res.return_data).unwrap(),
            U256::from(18u64),
            "decimals() should return 18"
        );

        let total_supply_call = encode_function_call("totalSupply()", &[]).unwrap();
        let total_supply_res = instance
            .call_raw(&total_supply_call, owner_opts)
            .expect("totalSupply() should succeed");
        assert_eq!(
            bytes_to_u256(&total_supply_res.return_data).unwrap(),
            U256::from(1_000u64),
            "totalSupply() should match constructor mint"
        );

        let bal_owner_call =
            encode_function_call("balanceOf(address)", &[Token::Address(owner_abi)]).unwrap();
        let bal_owner = instance
            .call_raw(&bal_owner_call, owner_opts)
            .expect("balanceOf(owner) should succeed");
        assert_eq!(
            bytes_to_u256(&bal_owner.return_data).unwrap(),
            U256::from(1_000u64),
            "owner should receive initial supply"
        );

        // transfer 250 from owner -> alice
        let transfer_call = encode_function_call(
            "transfer(address,uint256)",
            &[
                Token::Address(alice_abi),
                Token::Uint(AbiU256::from(250u64)),
            ],
        )
        .unwrap();
        let transfer_res = instance
            .call_raw(&transfer_call, owner_opts)
            .expect("transfer should succeed");
        assert_eq!(
            bytes_to_u256(&transfer_res.return_data).unwrap(),
            U256::from(1u64),
            "transfer should return true"
        );

        let bal_owner = instance
            .call_raw(&bal_owner_call, owner_opts)
            .expect("balanceOf(owner) after transfer should succeed");
        assert_eq!(
            bytes_to_u256(&bal_owner.return_data).unwrap(),
            U256::from(750u64),
            "owner balance should decrease after transfer"
        );

        let bal_alice_call =
            encode_function_call("balanceOf(address)", &[Token::Address(alice_abi)]).unwrap();
        let bal_alice = instance
            .call_raw(&bal_alice_call, owner_opts)
            .expect("balanceOf(alice) after transfer should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(250u64),
            "alice balance should increase after transfer"
        );

        // approve bob to spend 100 from owner
        let approve_call = encode_function_call(
            "approve(address,uint256)",
            &[Token::Address(bob_abi), Token::Uint(AbiU256::from(100u64))],
        )
        .unwrap();
        let approve_res = instance
            .call_raw(&approve_call, owner_opts)
            .expect("approve should succeed");
        assert_eq!(
            bytes_to_u256(&approve_res.return_data).unwrap(),
            U256::from(1u64),
            "approve should return true"
        );

        let allowance_call = encode_function_call(
            "allowance(address,address)",
            &[Token::Address(owner_abi), Token::Address(bob_abi)],
        )
        .unwrap();
        let allowance_res = instance
            .call_raw(&allowance_call, owner_opts)
            .expect("allowance should succeed");
        assert_eq!(
            bytes_to_u256(&allowance_res.return_data).unwrap(),
            U256::from(100u64),
            "allowance should match approve"
        );

        // transferFrom by bob: owner -> alice, 60
        let transfer_from_call = encode_function_call(
            "transferFrom(address,address,uint256)",
            &[
                Token::Address(owner_abi),
                Token::Address(alice_abi),
                Token::Uint(AbiU256::from(60u64)),
            ],
        )
        .unwrap();
        let bob_opts = ExecutionOptions {
            caller: bob,
            ..ExecutionOptions::default()
        };
        let transfer_from_res = instance
            .call_raw(&transfer_from_call, bob_opts)
            .expect("transferFrom should succeed");
        assert_eq!(
            bytes_to_u256(&transfer_from_res.return_data).unwrap(),
            U256::from(1u64),
            "transferFrom should return true"
        );

        let allowance_res = instance
            .call_raw(&allowance_call, owner_opts)
            .expect("allowance after transferFrom should succeed");
        assert_eq!(
            bytes_to_u256(&allowance_res.return_data).unwrap(),
            U256::from(40u64),
            "allowance should decrease after transferFrom"
        );

        // mint 10 to alice (owner is MINTER)
        let mint_call = encode_function_call(
            "mint(address,uint256)",
            &[Token::Address(alice_abi), Token::Uint(AbiU256::from(10u64))],
        )
        .unwrap();
        let mint_res = instance
            .call_raw(&mint_call, owner_opts)
            .expect("mint should succeed");
        assert_eq!(
            bytes_to_u256(&mint_res.return_data).unwrap(),
            U256::from(1u64),
            "mint should return true"
        );

        let total_supply_res = instance
            .call_raw(&total_supply_call, owner_opts)
            .expect("totalSupply after mint should succeed");
        assert_eq!(
            bytes_to_u256(&total_supply_res.return_data).unwrap(),
            U256::from(1_010u64),
            "totalSupply should increase after mint"
        );

        // burn 5 from alice
        let burn_call =
            encode_function_call("burn(uint256)", &[Token::Uint(AbiU256::from(5u64))]).unwrap();
        let alice_opts = ExecutionOptions {
            caller: alice,
            ..ExecutionOptions::default()
        };
        let burn_res = instance
            .call_raw(&burn_call, alice_opts)
            .expect("burn should succeed");
        assert_eq!(
            bytes_to_u256(&burn_res.return_data).unwrap(),
            U256::from(1u64),
            "burn should return true"
        );

        let total_supply_res = instance
            .call_raw(&total_supply_call, owner_opts)
            .expect("totalSupply after burn should succeed");
        assert_eq!(
            bytes_to_u256(&total_supply_res.return_data).unwrap(),
            U256::from(1_005u64),
            "totalSupply should decrease after burn"
        );
    }

    #[test]
    fn calldata_rebased_view_reads_after_selector() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("read(uint256)", &[Token::Uint(AbiU256::from(42u64))])
            .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("read(uint256) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(42u64),
            "CallData::with_base(4).decode() should read the ABI word after the selector"
        );
    }

    #[test]
    fn calldata_decode_raw_starts_at_byte_zero() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("raw(uint256)", &[Token::Uint(AbiU256::from(42u64))])
            .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("raw(uint256) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            bytes_to_u256(&call[..32]).unwrap(),
            "CallData::new().decode() should read the first 32 calldata bytes including the selector prefix"
        );
    }

    #[test]
    fn evm_decode_args_reads_after_selector() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("args(uint256)", &[Token::Uint(AbiU256::from(77u64))])
            .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("args(uint256) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(77u64),
            "evm.decode_args() should decode the ABI payload after the selector"
        );
    }

    #[test]
    fn evm_selector_matches_current_call_selector() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("selector()", &[]).expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("selector() should succeed");
        let expected = u32::from_be_bytes([call[0], call[1], call[2], call[3]]);

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(expected),
            "evm.selector() should return the current 4-byte selector"
        );
    }

    #[test]
    fn evm_decode_args_tuple_round_trip() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call(
            "tuple(uint64,bool)",
            &[Token::Uint(AbiU256::from(7u64)), Token::Bool(true)],
        )
        .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("tuple(uint64,bool) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(7u64),
            "evm.selector() and evm.decode_args() should round-trip tuple arguments"
        );
    }

    #[test]
    fn calldata_decode_input_over_rebased_view_matches_decode_input_at() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("generic(uint256)", &[Token::Uint(AbiU256::from(99u64))])
            .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("generic(uint256) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(99u64),
            "decode_input(CallData::with_base(4)) should match decode_input_at(CallData::new(), 4)"
        );
    }

    #[test]
    fn calldata_decode_input_at_reverts_when_base_is_past_end() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("bad()", &[]).expect("calldata should encode");
        let err = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect_err("bad() should revert when decode_input_at base exceeds calldata len");

        assert_empty_revert(err);
    }

    #[test]
    fn calldata_view_decode_reverts_when_base_is_past_end() {
        let Some(harness) = compile_calldata_decode_contract() else {
            return;
        };

        let call = encode_function_call("bad_view()", &[]).expect("calldata should encode");
        let err = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect_err("bad_view() should revert when the rebased calldata view is past the end");

        assert_empty_revert(err);
    }

    #[test]
    fn canonical_bool_decode_accepts_only_zero_or_one() {
        let Some(harness) = compile_canonical_decode_contract() else {
            return;
        };

        let ok = harness
            .call_function(
                "readBool(bool)",
                &[Token::Bool(true)],
                ExecutionOptions::default(),
            )
            .expect("canonical bool should decode");
        assert_eq!(bytes_to_u256(&ok.return_data).unwrap(), U256::from(1u64));

        let invalid = raw_single_word_call("readBool(bool)", word_from_u256(AbiU256::from(2u64)));
        let err = harness
            .call_raw(&invalid, ExecutionOptions::default())
            .expect_err("bool=2 should revert");
        assert_empty_revert(err);
    }

    #[test]
    fn canonical_u8_decode_rejects_nonzero_high_bits() {
        let Some(harness) = compile_canonical_decode_contract() else {
            return;
        };

        let ok = harness
            .call_function(
                "readU8(uint8)",
                &[Token::Uint(AbiU256::from(42u64))],
                ExecutionOptions::default(),
            )
            .expect("canonical uint8 should decode");
        assert_eq!(bytes_to_u256(&ok.return_data).unwrap(), U256::from(42u64));

        let invalid =
            raw_single_word_call("readU8(uint8)", word_from_u256(AbiU256::from(0x100u64)));
        let err = harness
            .call_raw(&invalid, ExecutionOptions::default())
            .expect_err("uint8 with nonzero high bits should revert");
        assert_empty_revert(err);
    }

    #[test]
    fn canonical_i8_decode_requires_sign_extension() {
        let Some(harness) = compile_canonical_decode_contract() else {
            return;
        };

        let ok = raw_single_word_call("readI8(int8)", word_from_u256(AbiU256::from(127u64)));
        let result = harness
            .call_raw(&ok, ExecutionOptions::default())
            .expect("canonical int8=127 should decode");
        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(1u64)
        );

        let mut invalid_word = [0u8; 32];
        invalid_word[31] = 0xff;
        let invalid = raw_single_word_call("readI8(int8)", invalid_word);
        let err = harness
            .call_raw(&invalid, ExecutionOptions::default())
            .expect_err("non-sign-extended int8 should revert");
        assert_empty_revert(err);
    }

    #[test]
    fn canonical_address_decode_rejects_nonzero_high_bits() {
        let Some(harness) = compile_canonical_decode_contract() else {
            return;
        };

        let raw = [0x11u8; 20];
        let ok_word = address_word(raw);
        let ok = harness
            .call_raw(
                &raw_single_word_call("readAddress(address)", ok_word),
                ExecutionOptions::default(),
            )
            .expect("canonical address should decode");
        assert_eq!(
            bytes_to_u256(&ok.return_data).unwrap(),
            bytes_to_u256(&ok_word).unwrap(),
            "address decode should preserve the low 160 bits"
        );

        let mut invalid_word = ok_word;
        invalid_word[0] = 1;
        let invalid = raw_single_word_call("readAddress(address)", invalid_word);
        let err = harness
            .call_raw(&invalid, ExecutionOptions::default())
            .expect_err("address with nonzero high bits should revert");
        assert_empty_revert(err);
    }

    #[test]
    fn decode_bytes_view_reads_dynamic_arg_length() {
        let Some(harness) = compile_dynamic_view_contract() else {
            return;
        };

        let call = encode_function_call(
            "bytesLen(bytes)",
            &[Token::Bytes(vec![0xaa, 0xbb, 0xcc, 0xdd, 0xee])],
        )
        .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("bytesLen(bytes) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(5u64),
            "decode_bytes_view should expose the dynamic byte length"
        );
    }

    #[test]
    fn decode_bytes_view_at_can_target_second_dynamic_arg() {
        let Some(harness) = compile_dynamic_view_contract() else {
            return;
        };

        let call = encode_function_call(
            "secondBytesLen(bytes,bytes)",
            &[
                Token::Bytes(vec![0x01, 0x02]),
                Token::Bytes(vec![0xaa, 0xbb, 0xcc, 0xdd]),
            ],
        )
        .expect("calldata should encode");
        let result = harness
            .call_raw(&call, ExecutionOptions::default())
            .expect("secondBytesLen(bytes,bytes) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(4u64),
            "decode_bytes_view_at should decode the selected dynamic head"
        );
    }

    #[test]
    fn decode_string_view_exposes_string_bytes() {
        let Some(harness) = compile_dynamic_view_contract() else {
            return;
        };

        let first_call =
            encode_function_call("stringFirst(string)", &[Token::String("hello".to_string())])
                .expect("calldata should encode");
        let first = harness
            .call_raw(&first_call, ExecutionOptions::default())
            .expect("stringFirst(string) should succeed");
        assert_eq!(
            bytes_to_u256(&first.return_data).unwrap(),
            U256::from(b'h' as u64),
            "decode_string_view should expose the underlying UTF-8 bytes"
        );

        let len_call =
            encode_function_call("stringLen(string)", &[Token::String("hello".to_string())])
                .expect("calldata should encode");
        let len = harness
            .call_raw(&len_call, ExecutionOptions::default())
            .expect("stringLen(string) should succeed");
        assert_eq!(
            bytes_to_u256(&len.return_data).unwrap(),
            U256::from(5u64),
            "decode_string_view should expose the dynamic string length"
        );
    }

    #[test]
    fn storage_bytes_round_trip_and_clear() {
        let Some(harness) = compile_storage_bytes_contract() else {
            return;
        };

        let mut instance = harness
            .deploy_with_init()
            .expect("storage bytes contract should deploy");
        let payload = vec![0xaa, 0xbb, 0xcc, 0xdd, 0xee];

        instance
            .call_function(
                "set(bytes)",
                &[Token::Bytes(payload.clone())],
                ExecutionOptions::default(),
            )
            .expect("set(bytes) should succeed");

        let stored = instance
            .call_function("get()", &[], ExecutionOptions::default())
            .expect("get() should succeed");
        let decoded = decode(&[ParamType::Bytes], &stored.return_data)
            .expect("get() should return ABI-encoded bytes");
        assert_eq!(decoded, vec![Token::Bytes(payload.clone())]);

        instance
            .call_function("clear()", &[], ExecutionOptions::default())
            .expect("clear() should succeed");

        let cleared = instance
            .call_function("get()", &[], ExecutionOptions::default())
            .expect("get() after clear should succeed");
        let decoded = decode(&[ParamType::Bytes], &cleared.return_data)
            .expect("cleared get() should return ABI-encoded bytes");
        assert_eq!(decoded, vec![Token::Bytes(Vec::new())]);
    }

    #[test]
    fn emit_bytes_event_emits_a_log() {
        let Some(harness) = compile_storage_bytes_contract() else {
            return;
        };

        let mut instance = harness
            .deploy_with_init()
            .expect("storage bytes contract should deploy");
        let call = encode_function_call("emit(bytes)", &[Token::Bytes(vec![0xaa, 0xbb, 0xcc])])
            .expect("calldata should encode");
        let result = instance
            .call_raw_with_logs(&call, ExecutionOptions::default())
            .expect("emit(bytes) should succeed");

        assert_eq!(result.logs.len(), 1, "emit_bytes_event should emit one log");
        assert!(
            result.logs[0].contains("aabbcc"),
            "log output should contain the event payload bytes"
        );
    }

    #[test]
    fn emit_bytes_event_does_not_clobber_subsequent_dynamic_return() {
        let Some(harness) = compile_emit_then_text_contract() else {
            return;
        };

        let payload = vec![0xaa, 0xbb, 0xcc, 0xdd];
        let mut instance = harness
            .deploy_with_init()
            .expect("emit-then-text contract should deploy");
        let call = encode_function_call("emitAndReturn(bytes)", &[Token::Bytes(payload.clone())])
            .expect("calldata should encode");
        let result = instance
            .call_raw_with_logs(&call, ExecutionOptions::default())
            .expect("emitAndReturn(bytes) should succeed");

        assert_eq!(result.logs.len(), 1, "emit_bytes_event should emit one log");
        assert!(
            result.logs[0].contains("aabbccdd"),
            "log output should contain the event payload bytes"
        );

        let decoded = decode(&[ParamType::String], &result.result.return_data)
            .expect("emitAndReturn(bytes) should return ABI-encoded string");
        assert_eq!(
            decoded,
            vec![Token::String(long_string_value("emit-and-return"))]
        );
    }

    #[test]
    fn raw_staticcall_decode_round_trips_word() {
        let Some(target_harness) = compile_raw_static_target_contract() else {
            return;
        };
        let Some(caller_harness) = compile_raw_static_caller_contract() else {
            return;
        };

        let mut caller = caller_harness
            .deploy_with_init()
            .expect("raw static caller contract should deploy");
        let target_addr = caller
            .deploy_sidecar(target_harness.init_bytecode(), &[])
            .expect("raw static target sidecar should deploy");
        let target_abi = ethers_core::types::Address::from_slice(target_addr.as_slice());

        let result = caller
            .call_function(
                "callWord(address)",
                &[Token::Address(target_abi)],
                ExecutionOptions::default(),
            )
            .expect("callWord(address) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(7u64),
            "staticcall_decode should ABI-decode the returned word"
        );
    }

    #[test]
    fn raw_staticcall_decode_rejects_empty_returndata() {
        let Some(harness) = compile_raw_static_caller_contract() else {
            return;
        };

        let mut caller = harness
            .deploy_with_init()
            .expect("raw static caller contract should deploy");
        let eoa = ethers_core::types::Address::from_low_u64_be(0x1234);
        let err = caller
            .call_function(
                "callWord(address)",
                &[Token::Address(eoa)],
                ExecutionOptions::default(),
            )
            .expect_err("callWord(address) should revert on empty returndata");

        assert_empty_revert(err);
    }

    #[test]
    fn raw_staticcall_decode_rejects_noncanonical_bool() {
        let Some(target_harness) = compile_bad_bool_target_contract() else {
            return;
        };
        let Some(caller_harness) = compile_raw_static_caller_contract() else {
            return;
        };

        let mut caller = caller_harness
            .deploy_with_init()
            .expect("raw static caller contract should deploy");
        let target_addr = caller
            .deploy_sidecar(target_harness.init_bytecode(), &[])
            .expect("bad bool target sidecar should deploy");
        let target_abi = ethers_core::types::Address::from_slice(target_addr.as_slice());
        let err = caller
            .call_function(
                "callFlag(address)",
                &[Token::Address(target_abi)],
                ExecutionOptions::default(),
            )
            .expect_err("callFlag(address) should revert on non-canonical bool returndata");

        assert_empty_revert(err);
    }

    #[test]
    fn dynamic_string_unannotated_literal_binding_round_trips() {
        let Some(harness) = compile_string_literal_contract() else {
            return;
        };

        let expected = long_string_value("literal");
        let mut instance = harness
            .deploy_with_init()
            .expect("string literal contract should deploy");
        let result = instance
            .call_function("literal()", &[], ExecutionOptions::default())
            .expect("literal() should succeed");

        let decoded = decode(&[ParamType::String], &result.return_data)
            .expect("literal() should return ABI-encoded string");
        assert_eq!(decoded, vec![Token::String(expected)]);
    }

    #[test]
    fn dynamic_string_view_returns_first_byte() {
        let Some(harness) = compile_string_view_contract() else {
            return;
        };

        let payload = long_string_value("view");
        let mut instance = harness
            .deploy_with_init()
            .expect("string view contract should deploy");
        let result = instance
            .call_function(
                "head(string)",
                &[Token::String(payload.clone())],
                ExecutionOptions::default(),
            )
            .expect("head(string) should succeed");

        assert_eq!(
            bytes_to_u256(&result.return_data).unwrap(),
            U256::from(payload.as_bytes()[0]),
            "head(string) should read through Text.view()"
        );
    }

    #[test]
    fn dynamic_string_round_trip_supports_long_payloads() {
        let Some(harness) = compile_string_echo_contract() else {
            return;
        };

        let payload = long_string_value("echo");
        let mut instance = harness
            .deploy_with_init()
            .expect("string echo contract should deploy");
        let result = instance
            .call_function(
                "echo(string)",
                &[Token::String(payload.clone())],
                ExecutionOptions::default(),
            )
            .expect("echo(string) should succeed");

        let decoded = decode(&[ParamType::String], &result.return_data)
            .expect("echo(string) should return ABI-encoded string");
        assert_eq!(decoded, vec![Token::String(payload)]);
    }

    #[test]
    fn dynamic_string_decode_and_return_round_trip() {
        let Some(harness) = compile_string_echo_contract() else {
            return;
        };

        let payload = long_string_value("roundtrip");
        let mut instance = harness
            .deploy_with_init()
            .expect("string echo contract should deploy");
        let result = instance
            .call_function(
                "echo(string)",
                &[Token::String(payload.clone())],
                ExecutionOptions::default(),
            )
            .expect("echo(string) should succeed");

        let decoded = decode(&[ParamType::String], &result.return_data)
            .expect("echo(string) should return ABI-encoded string");
        assert_eq!(decoded, vec![Token::String(payload)]);
    }

    #[test]
    fn dynamic_vec_alias_round_trips() {
        let Some(harness) = compile_vec_echo_contract() else {
            return;
        };

        let value = dyn_uint_array_token(&[3, 5, 8, 13, 21]);
        let mut instance = harness
            .deploy_with_init()
            .expect("vec echo contract should deploy");
        let call = encode_typed_function_call(
            "echo",
            vec![dyn_uint_array_param_type()],
            std::slice::from_ref(&value),
        )
        .expect("typed calldata should encode");
        let result = instance
            .call_raw(&call, ExecutionOptions::default())
            .expect("echo(uint256[]) should succeed");
        let decoded = decode(&[dyn_uint_array_param_type()], &result.return_data)
            .expect("echo(uint256[]) should return ABI-encoded array");

        assert_eq!(decoded, vec![value]);
    }

    #[test]
    fn dynamic_string_msg_call_round_trips() {
        let Some(echo_harness) = compile_string_echo_contract() else {
            return;
        };
        let Some(caller_harness) = compile_string_caller_contract() else {
            return;
        };

        let payload = long_string_value("caller");
        let mut caller = caller_harness
            .deploy_with_init()
            .expect("string caller contract should deploy");
        let echo_addr = caller
            .deploy_sidecar(echo_harness.init_bytecode(), &[])
            .expect("string echo sidecar should deploy");
        let echo_abi = ethers_core::types::Address::from_slice(echo_addr.as_slice());

        let result = caller
            .call_function(
                "callEcho(address,string)",
                &[Token::Address(echo_abi), Token::String(payload.clone())],
                ExecutionOptions::default(),
            )
            .expect("callEcho(address,string) should succeed");

        let decoded = decode(&[ParamType::String], &result.return_data)
            .expect("callEcho(address,string) should return ABI-encoded string");
        assert_eq!(decoded, vec![Token::String(payload)]);
    }

    #[test]
    fn dynamic_string_event_payload_is_abi_encoded() {
        let Some(harness) = compile_string_echo_contract() else {
            return;
        };

        let payload = long_string_value("event");
        let mut instance = harness
            .deploy_with_init()
            .expect("string echo contract should deploy");
        let call = encode_function_call("emit(string)", &[Token::String(payload.clone())])
            .expect("calldata should encode");
        let result = instance
            .call_raw_with_logs(&call, ExecutionOptions::default())
            .expect("emit(string) should succeed");

        assert_eq!(result.logs.len(), 1, "emit(string) should emit one log");
        assert!(
            result.logs[0].contains(&hex::encode(payload.as_bytes())),
            "log output should contain the ABI-encoded string payload"
        );
    }

    #[test]
    fn dynamic_tuple_return_is_solidity_compatible() {
        let Some(harness) = compile_nested_tuple_echo_contract() else {
            return;
        };

        let text = long_string_value("tuple-return");
        let mut instance = harness
            .deploy_with_init()
            .expect("nested tuple echo contract should deploy");
        let call = encode_typed_function_call(
            "echo",
            vec![nested_tuple_param_type()],
            &[nested_tuple_token(&text, 7)],
        )
        .expect("typed calldata should encode");
        let result = instance
            .call_raw(&call, ExecutionOptions::default())
            .expect("echo((string,uint64)) should succeed");

        let decoded = decode(&[nested_tuple_param_type()], &result.return_data)
            .expect("echo((string,uint64)) should return ABI-encoded outputs");
        assert_eq!(decoded, vec![nested_tuple_token(&text, 7)]);
    }

    #[test]
    fn dynamic_tuple_msg_call_round_trips() {
        let Some(echo_harness) = compile_nested_tuple_echo_contract() else {
            return;
        };
        let Some(caller_harness) = compile_nested_tuple_caller_contract() else {
            return;
        };

        let text = long_string_value("tuple-call");
        let mut caller = caller_harness
            .deploy_with_init()
            .expect("nested tuple caller contract should deploy");
        let echo_addr = caller
            .deploy_sidecar(echo_harness.init_bytecode(), &[])
            .expect("nested tuple echo sidecar should deploy");
        let echo_abi = ethers_core::types::Address::from_slice(echo_addr.as_slice());

        let call = encode_typed_function_call(
            "callEcho",
            vec![ParamType::Address, nested_tuple_param_type()],
            &[Token::Address(echo_abi), nested_tuple_token(&text, 9)],
        )
        .expect("typed calldata should encode");
        let result = caller
            .call_raw(&call, ExecutionOptions::default())
            .expect("callEcho(address,(string,uint64)) should succeed");

        let decoded = decode(&[nested_tuple_param_type()], &result.return_data)
            .expect("callEcho(address,(string,uint64)) should return ABI-encoded outputs");
        assert_eq!(decoded, vec![nested_tuple_token(&text, 9)]);
    }

    #[test]
    fn truncated_dynamic_tuple_head_reverts_before_padded_offset_read() {
        let Some(harness) = compile_tuple_head_guard_contract() else {
            return;
        };

        let mut instance = harness
            .deploy_with_init()
            .expect("tuple head guard contract should deploy");
        let mut call = encode_typed_function_call(
            "read",
            vec![ParamType::Tuple(vec![
                ParamType::Uint(64),
                ParamType::String,
            ])],
            &[Token::Tuple(vec![
                Token::Uint(AbiU256::from(0u64)),
                Token::String("missing-head".to_string()),
            ])],
        )
        .expect("typed calldata should encode");

        call.truncate(4 + 32 + 32);
        let err = instance
            .call_raw(&call, ExecutionOptions::default())
            .expect_err("truncated tuple head should revert");
        assert_empty_revert(err);
    }

    #[test]
    fn dynamic_tuple_constructor_args_round_trip() {
        let Some(harness) = compile_nested_tuple_init_contract() else {
            return;
        };

        let text = long_string_value("ctor");
        let mut instance = harness
            .deploy_with_init_args(&[
                Token::String(text.clone()),
                Token::Uint(AbiU256::from(11u64)),
            ])
            .expect("nested tuple init contract should deploy");
        let text_len = instance
            .call_function("getTextLen()", &[], ExecutionOptions::default())
            .expect("getTextLen() should succeed");
        let first_byte = instance
            .call_function("getFirstByte()", &[], ExecutionOptions::default())
            .expect("getFirstByte() should succeed");
        let count = instance
            .call_function("getCount()", &[], ExecutionOptions::default())
            .expect("getCount() should succeed");

        let decoded_text_len = decode(&[ParamType::Uint(256)], &text_len.return_data)
            .expect("getTextLen() should return ABI-encoded length");
        assert_eq!(
            decoded_text_len,
            vec![Token::Uint(AbiU256::from(text.len()))],
            "constructor should observe the full decoded string length"
        );
        let decoded_first_byte = decode(&[ParamType::Uint(8)], &first_byte.return_data)
            .expect("getFirstByte() should return ABI-encoded byte");
        assert_eq!(
            decoded_first_byte,
            vec![Token::Uint(AbiU256::from(text.as_bytes()[0]))],
            "constructor should observe the decoded string payload"
        );
        assert_eq!(
            bytes_to_u256(&count.return_data).unwrap(),
            U256::from(11u64),
            "constructor should store the tuple's scalar tail value"
        );
    }

    #[test]
    fn create2_static_constructor_args_round_trip() {
        let Some(harness) = compile_create2_init_args_contract() else {
            return;
        };

        let mut parent = harness
            .deploy_with_init()
            .expect("create2 parent contract should deploy");
        let deployed = parent
            .call_function("deployStatic()", &[], ExecutionOptions::default())
            .expect("deployStatic() should succeed");
        let child_address = Address::from_slice(&deployed.return_data[12..]);

        let get_byte = encode_function_call("getByte()", &[]).expect("calldata should encode");
        let child = parent
            .call_raw_at(child_address, &get_byte, ExecutionOptions::default())
            .expect("static child runtime should succeed");
        let decoded = decode(&[ParamType::Uint(8)], &child.return_data)
            .expect("getByte() should return ABI-encoded u8");
        assert_eq!(
            decoded,
            vec![Token::Uint(AbiU256::from(7u64))],
            "create2 should append direct static init args using ABI width, not memory size"
        );
    }

    #[test]
    fn create2_dynamic_constructor_args_round_trip() {
        let Some(harness) = compile_create2_init_args_contract() else {
            return;
        };

        let text = long_string_value("create2-ctor");
        let mut parent = harness
            .deploy_with_init()
            .expect("create2 parent contract should deploy");
        let deployed = parent
            .call_function(
                "deployDynamic(string,uint64)",
                &[
                    Token::String(text.clone()),
                    Token::Uint(AbiU256::from(13u64)),
                ],
                ExecutionOptions::default(),
            )
            .expect("deployDynamic(string,uint64) should succeed");
        let child_address = Address::from_slice(&deployed.return_data[12..]);

        let text_len = parent
            .call_raw_at(
                child_address,
                &encode_function_call("getTextLen()", &[]).expect("calldata should encode"),
                ExecutionOptions::default(),
            )
            .expect("getTextLen() should succeed");
        let first_byte = parent
            .call_raw_at(
                child_address,
                &encode_function_call("getFirstByte()", &[]).expect("calldata should encode"),
                ExecutionOptions::default(),
            )
            .expect("getFirstByte() should succeed");
        let count = parent
            .call_raw_at(
                child_address,
                &encode_function_call("getCount()", &[]).expect("calldata should encode"),
                ExecutionOptions::default(),
            )
            .expect("getCount() should succeed");

        assert_eq!(
            bytes_to_u256(&text_len.return_data).unwrap(),
            U256::from(text.len() as u64),
            "create2 should encode dynamic init args without an extra root wrapper"
        );
        let decoded_first_byte = decode(&[ParamType::Uint(8)], &first_byte.return_data)
            .expect("getFirstByte() should return ABI-encoded u8");
        assert_eq!(
            decoded_first_byte,
            vec![Token::Uint(AbiU256::from(text.as_bytes()[0]))],
            "constructor should observe the decoded string payload"
        );
        assert_eq!(
            bytes_to_u256(&count.return_data).unwrap(),
            U256::from(13u64),
            "constructor should observe the scalar tail argument"
        );
    }

    #[test]
    fn fixed_string_calldata_decode_rejects_oversized_payloads() {
        let Some(harness) = compile_fixed_string_decode_contract() else {
            return;
        };

        harness
            .call_function(
                "ok(string)",
                &[Token::String("short-string".to_string())],
                ExecutionOptions::default(),
            )
            .expect("short string should decode into String<31>");

        let err = harness
            .call_function(
                "ok(string)",
                &[Token::String(long_string_value("fixed-string-overflow"))],
                ExecutionOptions::default(),
            )
            .expect_err("32+ byte string should not silently truncate into String<31>");
        assert_empty_revert(err);

        let mut truncated =
            encode_function_call("ok(string)", &[Token::String("hello".to_string())])
                .expect("calldata should encode");
        truncated.truncate(truncated.len() - 1);
        let err = harness
            .call_raw(&truncated, ExecutionOptions::default())
            .expect_err("truncated string tail should revert during decode");
        assert_empty_revert(err);
    }

    #[test]
    fn fixed_string_returndata_decode_rejects_oversized_payloads() {
        let Some(target_harness) = compile_string_echo_contract() else {
            return;
        };
        let Some(caller_harness) = compile_fixed_string_return_caller_contract() else {
            return;
        };

        let mut caller = caller_harness
            .deploy_with_init()
            .expect("fixed string return caller contract should deploy");
        let target_addr = caller
            .deploy_sidecar(target_harness.init_bytecode(), &[])
            .expect("string echo sidecar should deploy");
        let target_abi = ethers_core::types::Address::from_slice(target_addr.as_slice());

        let short_text = "short-return".to_string();
        let short_call = encode_typed_function_call(
            "forward",
            vec![ParamType::Address, ParamType::String],
            &[
                Token::Address(target_abi),
                Token::String(short_text.clone()),
            ],
        )
        .expect("typed calldata should encode");
        let short_result = caller
            .call_raw(&short_call, ExecutionOptions::default())
            .expect("short return string should decode into String<31>");
        let short_decoded = decode(&[ParamType::String], &short_result.return_data)
            .expect("forward(address,string) should return ABI-encoded string");
        assert_eq!(short_decoded, vec![Token::String(short_text)]);

        let long_text = long_string_value("fixed-string-return-overflow");
        let long_call = encode_typed_function_call(
            "forward",
            vec![ParamType::Address, ParamType::String],
            &[Token::Address(target_abi), Token::String(long_text)],
        )
        .expect("typed calldata should encode");
        let err = caller
            .call_raw(&long_call, ExecutionOptions::default())
            .expect_err("32+ byte returndata should not silently truncate into String<31>");
        assert_empty_revert(err);
    }

    #[test]
    fn custom_width_encode_rejects_non_canonical_values() {
        let Some(harness) = compile_custom_width_encode_contract() else {
            return;
        };

        let mut instance = harness
            .deploy_with_init()
            .expect("custom width encode contract should deploy");

        let good_uint24 = instance
            .call_function("goodUint24()", &[], ExecutionOptions::default())
            .expect("goodUint24() should succeed");
        assert_eq!(
            decode(&[ParamType::Uint(24)], &good_uint24.return_data)
                .expect("goodUint24() should return ABI-encoded uint24"),
            vec![Token::Uint(AbiU256::from(7u64))]
        );

        let err = instance
            .call_function("badUint24()", &[], ExecutionOptions::default())
            .expect_err("out-of-range Uint24 should revert during ABI encode");
        assert_empty_revert(err);

        let good_uint160 = instance
            .call_function("goodUint160()", &[], ExecutionOptions::default())
            .expect("goodUint160() should succeed");
        assert_eq!(
            decode(&[ParamType::Uint(160)], &good_uint160.return_data)
                .expect("goodUint160() should return ABI-encoded uint160"),
            vec![Token::Uint(AbiU256::from(1u64))]
        );

        let err = instance
            .call_function("badUint160()", &[], ExecutionOptions::default())
            .expect_err("out-of-range Uint160 should revert during ABI encode");
        assert_empty_revert(err);

        let good_int40 = instance
            .call_function("goodInt40()", &[], ExecutionOptions::default())
            .expect("goodInt40() should succeed");
        assert_eq!(
            decode(&[ParamType::Int(40)], &good_int40.return_data)
                .expect("goodInt40() should return ABI-encoded int40"),
            vec![Token::Int(AbiU256::from(5u64))]
        );

        let err = instance
            .call_function("badInt40()", &[], ExecutionOptions::default())
            .expect_err("out-of-range Int40 should revert during ABI encode");
        assert_empty_revert(err);

        let good_int136 = instance
            .call_function("goodInt136()", &[], ExecutionOptions::default())
            .expect("goodInt136() should succeed");
        assert_eq!(
            decode(&[ParamType::Int(136)], &good_int136.return_data)
                .expect("goodInt136() should return ABI-encoded int136"),
            vec![Token::Int(AbiU256::from(5u64))]
        );

        let err = instance
            .call_function("badInt136()", &[], ExecutionOptions::default())
            .expect_err("out-of-range Int136 should revert during ABI encode");
        assert_empty_revert(err);
    }

    #[test]
    fn custom_width_topics_reject_non_canonical_values() {
        let Some(harness) = compile_custom_width_encode_contract() else {
            return;
        };

        let mut instance = harness
            .deploy_with_init()
            .expect("custom width encode contract should deploy");

        let ok = instance
            .call_raw_with_logs(
                &encode_function_call("emitGoodUint160()", &[]).expect("calldata should encode"),
                ExecutionOptions::default(),
            )
            .expect("canonical Uint160 topic should emit successfully");
        assert_eq!(ok.logs.len(), 1, "expected a single emitted event");

        let err = instance
            .call_raw(
                &encode_function_call("emitBadUint160()", &[]).expect("calldata should encode"),
                ExecutionOptions::default(),
            )
            .expect_err("out-of-range Uint160 topic should revert during event emission");
        assert_empty_revert(err);
    }

    #[test]
    fn fixed_array_contract_round_trips_bool_array_64() {
        let source = fixed_bool_array_contract_source(64);
        let harness = FeContractHarness::compile_from_source(
            "FixedBoolArrayBoundary",
            &source,
            CompileOptions::default(),
        )
        .expect("fixed bool[64] contract should compile");
        let mut instance = harness
            .deploy_with_init()
            .expect("fixed bool[64] contract should deploy");
        let value = fixed_bool_array_token(64);
        let call = encode_typed_function_call(
            "echo",
            vec![fixed_bool_array_param_type(64)],
            std::slice::from_ref(&value),
        )
        .expect("typed calldata should encode");
        let result = instance
            .call_raw(&call, ExecutionOptions::default())
            .expect("echo(bool[64]) should succeed");
        let decoded = decode(&[fixed_bool_array_param_type(64)], &result.return_data)
            .expect("echo(bool[64]) should return ABI-encoded fixed array");

        assert_eq!(decoded, vec![value]);
    }

    #[test]
    fn fixed_array_contract_round_trips_bool_array_65() {
        let source = fixed_bool_array_contract_source(65);
        let harness = FeContractHarness::compile_from_source(
            "FixedBoolArrayBoundary",
            &source,
            CompileOptions::default(),
        )
        .expect("fixed bool[65] contract should compile");
        let mut instance = harness
            .deploy_with_init()
            .expect("fixed bool[65] contract should deploy");
        let value = fixed_bool_array_token(65);
        let call = encode_typed_function_call(
            "echo",
            vec![fixed_bool_array_param_type(65)],
            std::slice::from_ref(&value),
        )
        .expect("typed calldata should encode");
        let result = instance
            .call_raw(&call, ExecutionOptions::default())
            .expect("echo(bool[65]) should succeed");
        let decoded = decode(&[fixed_bool_array_param_type(65)], &result.return_data)
            .expect("echo(bool[65]) should return ABI-encoded fixed array");

        assert_eq!(decoded, vec![value]);
    }

    #[test]
    fn fixed_array_contract_round_trips_string_and_bytes_array_65() {
        let source = fixed_dynamic_array_contract_source(65);
        let harness = FeContractHarness::compile_from_source(
            "FixedDynamicArrayBoundary",
            &source,
            CompileOptions::default(),
        )
        .expect("fixed string[65]/bytes[65] contract should compile");
        let mut instance = harness
            .deploy_with_init()
            .expect("fixed string[65]/bytes[65] contract should deploy");

        let string_value = fixed_string_array_token(65);
        let string_call = encode_typed_function_call(
            "echoString",
            vec![fixed_string_array_param_type(65)],
            std::slice::from_ref(&string_value),
        )
        .expect("typed string calldata should encode");
        let string_result = instance
            .call_raw(&string_call, ExecutionOptions::default())
            .expect("echoString(string[65]) should succeed");
        let string_decoded = decode(
            &[fixed_string_array_param_type(65)],
            &string_result.return_data,
        )
        .expect("echoString(string[65]) should return ABI-encoded fixed array");
        assert_eq!(string_decoded, vec![string_value]);

        let bytes_value = fixed_bytes_array_token(65);
        let bytes_call = encode_typed_function_call(
            "echoBytes",
            vec![fixed_bytes_array_param_type(65)],
            std::slice::from_ref(&bytes_value),
        )
        .expect("typed bytes calldata should encode");
        let bytes_result = instance
            .call_raw(&bytes_call, ExecutionOptions::default())
            .expect("echoBytes(bytes[65]) should succeed");
        let bytes_decoded = decode(
            &[fixed_bytes_array_param_type(65)],
            &bytes_result.return_data,
        )
        .expect("echoBytes(bytes[65]) should return ABI-encoded fixed array");
        assert_eq!(bytes_decoded, vec![bytes_value]);
    }

    #[test]
    fn runtime_constructs_contract() {
        let fixture_dir = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/runtime_constructs"
        );
        let ingot_url = Url::from_directory_path(fixture_dir).expect("fixture dir is valid");

        let mut db = DriverDataBase::default();
        let had_init_diagnostics = driver::init_ingot(&mut db, &ingot_url);
        assert!(
            !had_init_diagnostics,
            "ingot resolution should succeed for `{ingot_url}`"
        );

        let ingot = db
            .workspace()
            .containing_ingot(&db, ingot_url.clone())
            .expect("ingot should be registered in workspace");
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            panic!("compiler diagnostics:\n{}", diags.format_diags(&db));
        }

        let root_file = ingot.root_file(&db).expect("ingot should have root file");
        let top_mod = db.top_mod(root_file);
        let bytecode =
            emit_module_sonatina_bytecode(&db, top_mod, OptLevel::default(), Some("Parent"))
                .expect("Sonatina bytecode should compile");
        let contract = bytecode
            .get("Parent")
            .expect("Parent bytecode should exist");
        let init_bytecode = hex::encode(&contract.deploy);

        let mut instance =
            RuntimeInstance::deploy(&init_bytecode).expect("parent deployment should succeed");
        let parent_res = instance
            .call_raw(&[], ExecutionOptions::default())
            .expect("parent runtime should succeed");
        assert_eq!(
            parent_res.return_data.len(),
            32,
            "parent should return a u256 word containing the deployed child address"
        );

        let child_address = Address::from_slice(&parent_res.return_data[12..]);
        assert_ne!(
            child_address,
            Address::ZERO,
            "parent should return a nonzero child address"
        );

        let child_res = instance
            .call_raw_at(child_address, &[], ExecutionOptions::default())
            .expect("child runtime should succeed");
        assert_eq!(
            bytes_to_u256(&child_res.return_data).unwrap(),
            U256::from(0xbeefu64),
            "child runtime should return expected value"
        );
    }
}
