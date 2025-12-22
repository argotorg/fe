//! Test harness utilities for compiling Fe contracts and exercising their runtimes with `revm`.
use codegen::emit_module_yul;
use common::InputDb;
use driver::DriverDataBase;
use ethers_core::abi::{AbiParser, ParseError as AbiParseError, Token};
use hex::FromHex;
pub use revm::primitives::U256;
use revm::{
    bytecode::Bytecode,
    context::{
        Context, TxEnv,
        result::{ExecutionResult, HaltReason, Output},
    },
    database::InMemoryDB,
    handler::{ExecuteCommitEvm, MainBuilder, MainContext, MainnetContext, MainnetEvm},
    primitives::{Address, Bytes as EvmBytes, TxKind},
    state::AccountInfo,
};
use solc_runner::{ContractBytecode, YulcError, compile_single_contract};
use std::{fmt, path::Path};
use thiserror::Error;
use url::Url;

/// Default in-memory file path used when compiling inline Fe sources.
const MEMORY_SOURCE_URL: &str = "file:///contract.fe";

/// Error type returned by the harness.
#[derive(Debug, Error)]
pub enum HarnessError {
    #[error("fe compiler diagnostics:\n{0}")]
    CompilerDiagnostics(String),
    #[error("failed to emit Yul: {0}")]
    EmitYul(#[from] codegen::EmitModuleError),
    #[error("solc error: {0}")]
    Solc(String),
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

impl From<YulcError> for HarnessError {
    fn from(value: YulcError) -> Self {
        Self::Solc(value.0)
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
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Toggle solc optimizer.
    pub optimize: bool,
    /// Verify that solc produced runtime bytecode.
    pub verify_runtime: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            optimize: false,
            verify_runtime: true,
        }
    }
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

impl Default for ExecutionOptions {
    fn default() -> Self {
        Self {
            caller: Address::ZERO,
            gas_limit: 1_000_000,
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
) -> Result<CallResult, HarnessError> {
    let tx = TxEnv::builder()
        .caller(options.caller)
        .gas_limit(options.gas_limit)
        .gas_price(options.gas_price)
        .to(address)
        .value(options.value)
        .data(EvmBytes::copy_from_slice(calldata))
        .nonce(options.nonce.unwrap_or(nonce))
        .build()
        .map_err(|err| HarnessError::Execution(format!("{err:?}")))?;

    let result = evm
        .transact_commit(tx)
        .map_err(|err| HarnessError::Execution(err.to_string()))?;
    match result {
        ExecutionResult::Success {
            output: Output::Call(bytes),
            gas_used,
            ..
        } => Ok(CallResult {
            return_data: bytes.to_vec(),
            gas_used,
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

/// Stateful runtime instance backed by a persistent in-memory database.
pub struct RuntimeInstance {
    evm: MainnetEvm<MainnetContext<InMemoryDB>>,
    address: Address,
    next_nonce: u64,
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
        let ctx = Context::mainnet().with_db(db);
        let evm = ctx.build_mainnet();
        Ok(Self {
            evm,
            address,
            next_nonce: 0,
        })
    }

    /// Deploys a contract by executing its init bytecode and using the returned runtime code.
    /// This properly runs any initialization logic in the constructor.
    pub fn deploy(init_bytecode_hex: &str) -> Result<Self, HarnessError> {
        let init_code = hex_to_bytes(init_bytecode_hex)?;
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

        let ctx = Context::mainnet().with_db(db);
        let mut evm = ctx.build_mainnet();

        // Create deployment transaction (TxKind::Create means contract creation)
        let tx = TxEnv::builder()
            .caller(caller)
            .gas_limit(10_000_000)
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
                ..
            } => {
                // The contract was deployed successfully; revm has already inserted the account
                Ok(Self {
                    evm,
                    address: deployed_address,
                    next_nonce: 1,
                })
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

    /// Executes the runtime with arbitrary calldata.
    pub fn call_raw(
        &mut self,
        calldata: &[u8],
        options: ExecutionOptions,
    ) -> Result<CallResult, HarnessError> {
        let nonce = options.nonce.unwrap_or_else(|| {
            let current = self.next_nonce;
            self.next_nonce += 1;
            current
        });
        transact(&mut self.evm, self.address, calldata, options, nonce)
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

    /// Returns the contract address assigned to this runtime instance.
    pub fn address(&self) -> Address {
        self.address
    }
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
        let yul = emit_module_yul(&db, top_mod)?;
        let contract = compile_single_contract(
            contract_name,
            &yul,
            options.optimize,
            options.verify_runtime,
        )?;
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

    /// Returns the raw runtime bytecode emitted by `solc`.
    pub fn runtime_bytecode(&self) -> &str {
        &self.contract.runtime_bytecode
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

    /// Returns the raw init bytecode emitted by `solc`.
    pub fn init_bytecode(&self) -> &str {
        &self.contract.bytecode
    }
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

#[cfg(test)]
#[allow(clippy::print_stderr)]
mod tests {
    use super::*;
    use ethers_core::{abi::Token, types::U256 as AbiU256};
    use std::process::Command;

    fn solc_available() -> bool {
        let solc_path = std::env::var("FE_SOLC_PATH").unwrap_or_else(|_| "solc".to_string());
        Command::new(solc_path)
            .arg("--version")
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    }

    #[test]
    fn runtime_instance_persists_state() {
        if !solc_available() {
            eprintln!("skipping runtime_instance_persists_state because solc is missing");
            return;
        }
        let yul = r#"
object "Counter" {
    code {
        datacopy(0, dataoffset("runtime"), datasize("runtime"))
        return(0, datasize("runtime"))
    }
    object "runtime" {
        code {
            let current := sload(0)
            let next := add(current, 1)
            sstore(0, next)
            mstore(0x00, next)
            return(0x00, 0x20)
        }
    }
}
"#;
        let contract =
            compile_single_contract("Counter", yul, false, true).expect("yul compilation succeeds");
        let mut instance =
            RuntimeInstance::new(&contract.runtime_bytecode).expect("runtime instantiation");
        let options = ExecutionOptions::default();
        let first = instance
            .call_raw(&[0u8; 0], options)
            .expect("first call succeeds");
        assert_eq!(bytes_to_u256(&first.return_data).unwrap(), U256::from(1));
        let second = instance
            .call_raw(&[0u8; 0], options)
            .expect("second call succeeds");
        assert_eq!(bytes_to_u256(&second.return_data).unwrap(), U256::from(2));
    }

    #[test]
    fn full_contract_test() {
        if !solc_available() {
            eprintln!("skipping full_contract_test because solc is missing");
            return;
        }
        let source_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/full_contract.fe"
        );
        let harness = FeContractHarness::compile_from_file(
            "ShapeDispatcher",
            source_path,
            CompileOptions::default(),
        )
        .expect("compilation should succeed");
        let mut instance = harness.deploy_instance().expect("deployment succeeds");
        let options = ExecutionOptions::default();
        let point_call = encode_function_call(
            "point(uint256,uint256)",
            &[
                Token::Uint(AbiU256::from(3u64)),
                Token::Uint(AbiU256::from(4u64)),
            ],
        )
        .unwrap();
        let point_result = instance
            .call_raw(&point_call, options)
            .expect("point selector should succeed");
        assert_eq!(
            bytes_to_u256(&point_result.return_data).unwrap(),
            U256::from(24u64)
        );

        let square_call =
            encode_function_call("square(uint256)", &[Token::Uint(AbiU256::from(5u64))]).unwrap();
        let square_result = instance
            .call_raw(&square_call, options)
            .expect("square selector should succeed");
        assert_eq!(
            bytes_to_u256(&square_result.return_data).unwrap(),
            U256::from(64u64)
        );
    }

    #[test]
    fn storage_contract_test() {
        if !solc_available() {
            eprintln!("skipping storage_contract_test because solc is missing");
            return;
        }
        let source_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/storage.fe"
        );
        let harness =
            FeContractHarness::compile_from_file("Coin", source_path, CompileOptions::default())
                .expect("compilation should succeed");

        let mut instance = harness.deploy_instance().expect("deployment succeeds");
        let options = ExecutionOptions::default();

        // Helper discriminants: 0 = Alice, 1 = Bob
        let alice = Token::Uint(AbiU256::from(0u64));
        let bob = Token::Uint(AbiU256::from(1u64));

        // credit Alice with 10
        let credit_alice = encode_function_call(
            "credit(uint256,uint256)",
            &[alice.clone(), Token::Uint(AbiU256::from(10u64))],
        )
        .unwrap();
        let credit_alice_res = instance
            .call_raw(&credit_alice, options)
            .expect("credit alice should succeed");
        assert_eq!(
            bytes_to_u256(&credit_alice_res.return_data).unwrap(),
            U256::from(10u64)
        );

        // credit Bob with 5
        let credit_bob = encode_function_call(
            "credit(uint256,uint256)",
            &[bob.clone(), Token::Uint(AbiU256::from(5u64))],
        )
        .unwrap();
        let credit_bob_res = instance
            .call_raw(&credit_bob, options)
            .expect("credit bob should succeed");
        assert_eq!(
            bytes_to_u256(&credit_bob_res.return_data).unwrap(),
            U256::from(5u64)
        );

        // transfer 3 from Alice -> Bob (should succeed, return code 0)
        let transfer_alice = encode_function_call(
            "transfer(uint256,uint256)",
            &[alice.clone(), Token::Uint(AbiU256::from(3u64))],
        )
        .unwrap();
        let transfer_alice_res = instance
            .call_raw(&transfer_alice, options)
            .expect("transfer from alice should succeed");
        assert_eq!(
            bytes_to_u256(&transfer_alice_res.return_data).unwrap(),
            U256::from(0u64),
            "successful transfer returns code 0"
        );

        // balances after transfer
        let bal_alice_call =
            encode_function_call("balance_of(uint256)", std::slice::from_ref(&alice)).unwrap();
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balance_of alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(7u64)
        );

        let bal_bob_call =
            encode_function_call("balance_of(uint256)", std::slice::from_ref(&bob)).unwrap();
        let bal_bob = instance
            .call_raw(&bal_bob_call, options)
            .expect("balance_of bob should succeed");
        assert_eq!(
            bytes_to_u256(&bal_bob.return_data).unwrap(),
            U256::from(8u64)
        );

        // transfer too much from Bob -> Alice should fail with code 1
        let transfer_bob = encode_function_call(
            "transfer(uint256,uint256)",
            &[bob, Token::Uint(AbiU256::from(20u64))],
        )
        .unwrap();
        let transfer_bob_res = instance
            .call_raw(&transfer_bob, options)
            .expect("transfer from bob should run");
        assert_eq!(
            bytes_to_u256(&transfer_bob_res.return_data).unwrap(),
            U256::from(1u64),
            "insufficient funds should return code 1"
        );

        // total_supply should equal alice + bob (10 + 5 = 15)
        let total_supply_call = encode_function_call("total_supply()", &[]).unwrap();
        let total_supply_res = instance
            .call_raw(&total_supply_call, options)
            .expect("total_supply should succeed");
        let total_supply = bytes_to_u256(&total_supply_res.return_data)
            .expect("total_supply should return a u256");
        assert_eq!(total_supply, U256::from(15u64));
    }

    #[test]
    fn enum_variant_construction_test() {
        if !solc_available() {
            eprintln!("skipping enum_variant_construction_test because solc is missing");
            return;
        }
        let source_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/enum_variant_contract.fe"
        );
        let harness = FeContractHarness::compile_from_file(
            "EnumContract",
            source_path,
            CompileOptions::default(),
        )
        .expect("compilation should succeed");
        let mut instance = harness.deploy_instance().expect("deployment succeeds");
        let options = ExecutionOptions::default();

        // Test make_some(42) - should return 42 (unwrapped value)
        let make_some_call =
            encode_function_call("make_some(uint256)", &[Token::Uint(AbiU256::from(42u64))])
                .unwrap();
        let make_some_result = instance
            .call_raw(&make_some_call, options)
            .expect("make_some selector should succeed");
        assert_eq!(
            bytes_to_u256(&make_some_result.return_data).unwrap(),
            U256::from(42u64),
            "make_some(42) should return 42"
        );

        // Test is_some_check(99) - should return 1 (true)
        let is_some_call = encode_function_call(
            "is_some_check(uint256)",
            &[Token::Uint(AbiU256::from(99u64))],
        )
        .unwrap();
        let is_some_result = instance
            .call_raw(&is_some_call, options)
            .expect("is_some_check selector should succeed");
        assert_eq!(
            bytes_to_u256(&is_some_result.return_data).unwrap(),
            U256::from(1u64),
            "is_some_check should return 1 for Some variant"
        );

        // Test make_none() - should return 0 (is_some returns 0 for None)
        let make_none_call = encode_function_call("make_none()", &[]).unwrap();
        let make_none_result = instance
            .call_raw(&make_none_call, options)
            .expect("make_none selector should succeed");
        assert_eq!(
            bytes_to_u256(&make_none_result.return_data).unwrap(),
            U256::from(0u64),
            "make_none() should return 0 (is_some of None)"
        );
    }

    #[test]
    fn storage_map_contract_test() {
        if !solc_available() {
            eprintln!("skipping storage_map_contract_test because solc is missing");
            return;
        }
        let source_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../codegen/tests/fixtures/storage_map_contract.fe"
        );
        let harness = FeContractHarness::compile_from_file(
            "BalanceMap",
            source_path,
            CompileOptions::default(),
        )
        .expect("compilation should succeed");

        // Use deploy_with_init to properly run init code that initializes storage
        let mut instance = harness.deploy_with_init().expect("deployment succeeds");
        let options = ExecutionOptions::default();

        // Use address-like values for accounts
        let alice = Token::Uint(AbiU256::from(0x1111u64));
        let bob = Token::Uint(AbiU256::from(0x2222u64));

        // Initially, balances should be zero
        let bal_alice_call =
            encode_function_call("balanceOf(uint256)", std::slice::from_ref(&alice)).unwrap();
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balanceOf alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(0u64),
            "initial alice balance should be 0"
        );

        // Set Alice's balance to 100
        let set_alice = encode_function_call(
            "setBalance(uint256,uint256)",
            &[alice.clone(), Token::Uint(AbiU256::from(100u64))],
        )
        .unwrap();
        instance
            .call_raw(&set_alice, options)
            .expect("setBalance alice should succeed");

        // Verify Alice's balance is now 100
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balanceOf alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(100u64),
            "alice balance should be 100 after set"
        );

        // Set Bob's balance to 50
        let set_bob = encode_function_call(
            "setBalance(uint256,uint256)",
            &[bob.clone(), Token::Uint(AbiU256::from(50u64))],
        )
        .unwrap();
        instance
            .call_raw(&set_bob, options)
            .expect("setBalance bob should succeed");

        // Transfer 30 from Alice to Bob (should succeed, return 0)
        let transfer_call = encode_function_call(
            "transfer(uint256,uint256,uint256)",
            &[
                alice.clone(),
                bob.clone(),
                Token::Uint(AbiU256::from(30u64)),
            ],
        )
        .unwrap();
        let transfer_result = instance
            .call_raw(&transfer_call, options)
            .expect("transfer should succeed");
        assert_eq!(
            bytes_to_u256(&transfer_result.return_data).unwrap(),
            U256::from(0u64),
            "transfer should return 0 (success)"
        );

        // Verify balances after transfer: Alice = 70, Bob = 80
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balanceOf alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(70u64),
            "alice balance should be 70 after transfer"
        );

        let bal_bob_call =
            encode_function_call("balanceOf(uint256)", std::slice::from_ref(&bob)).unwrap();
        let bal_bob = instance
            .call_raw(&bal_bob_call, options)
            .expect("balanceOf bob should succeed");
        assert_eq!(
            bytes_to_u256(&bal_bob.return_data).unwrap(),
            U256::from(80u64),
            "bob balance should be 80 after transfer"
        );

        // Try to transfer more than Alice has (should fail, return 1)
        let transfer_fail = encode_function_call(
            "transfer(uint256,uint256,uint256)",
            &[
                alice.clone(),
                bob.clone(),
                Token::Uint(AbiU256::from(1000u64)),
            ],
        )
        .unwrap();
        let transfer_fail_result = instance
            .call_raw(&transfer_fail, options)
            .expect("transfer should execute");
        assert_eq!(
            bytes_to_u256(&transfer_fail_result.return_data).unwrap(),
            U256::from(1u64),
            "transfer should return 1 (insufficient funds)"
        );

        // Verify balances unchanged after failed transfer
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balanceOf alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(70u64),
            "alice balance should still be 70 after failed transfer"
        );

        // ========== Test that allowances map is separate from balances ==========
        // Set Alice's allowance to 999
        let set_allowance_alice = encode_function_call(
            "setAllowance(uint256,uint256)",
            &[alice.clone(), Token::Uint(AbiU256::from(999u64))],
        )
        .unwrap();
        instance
            .call_raw(&set_allowance_alice, options)
            .expect("setAllowance alice should succeed");

        // Verify Alice's allowance is 999
        let get_allowance_alice =
            encode_function_call("getAllowance(uint256)", std::slice::from_ref(&alice)).unwrap();
        let allowance_alice = instance
            .call_raw(&get_allowance_alice, options)
            .expect("getAllowance alice should succeed");
        assert_eq!(
            bytes_to_u256(&allowance_alice.return_data).unwrap(),
            U256::from(999u64),
            "alice allowance should be 999"
        );

        // CRITICAL: Verify Alice's balance is STILL 70 (not affected by allowance)
        let bal_alice = instance
            .call_raw(&bal_alice_call, options)
            .expect("balanceOf alice should succeed");
        assert_eq!(
            bytes_to_u256(&bal_alice.return_data).unwrap(),
            U256::from(70u64),
            "alice balance should still be 70 after setting allowance - maps must be independent!"
        );

        // And verify Bob's allowance is 0 (default, never set)
        let get_allowance_bob =
            encode_function_call("getAllowance(uint256)", std::slice::from_ref(&bob)).unwrap();
        let allowance_bob = instance
            .call_raw(&get_allowance_bob, options)
            .expect("getAllowance bob should succeed");
        assert_eq!(
            bytes_to_u256(&allowance_bob.return_data).unwrap(),
            U256::from(0u64),
            "bob allowance should be 0 (never set)"
        );
    }
}
