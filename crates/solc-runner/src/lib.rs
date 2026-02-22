use indexmap::IndexMap;
use serde_json::{Value, json};
use std::{
    env,
    io::Write,
    process::{Command, Stdio},
};

const SOLC_ENV: &str = "FE_SOLC_PATH";

/// Error wrapper used throughout the Yul compilation pipeline.
#[derive(Debug, Clone)]
pub struct YulcError(pub String);

/// Represents the deployable and runtime bytecode for a compiled contract.
pub struct ContractBytecode {
    pub bytecode: String,
    pub runtime_bytecode: String,
}

/// Compiles an iterator of `(name, yul_source)` pairs using `solc`.
///
/// * `contracts` - Iterator of contract names and associated Yul source strings.
/// * `optimize` - Enables `solc`'s optimizer when `true`.
///
/// Returns a map containing each contract's compiled [`ContractBytecode`] keyed by name, or a
/// [`YulcError`] if compilation fails for any contract.
pub fn compile(
    contracts: impl Iterator<Item = (impl AsRef<str>, impl AsRef<str>)>,
    optimize: bool,
) -> Result<IndexMap<String, ContractBytecode>, YulcError> {
    contracts
        .map(|(name, yul_src)| {
            compile_single_contract(name.as_ref(), yul_src.as_ref(), optimize, true)
                .map(|bytecode| (name.as_ref().to_string(), bytecode))
        })
        .collect()
}

/// Compiles a single contract by forwarding the Yul source to `solc`.
///
/// * `name` - Contract identifier as it appears in the Yul source.
/// * `yul_src` - Yul source code for the contract.
/// * `optimize` - Enables the optimizer stage when `true`.
/// * `verify_runtime_bytecode` - Ensures runtime bytecode is present when set to `true`.
///
/// Returns the compiled [`ContractBytecode`] or a [`YulcError`] describing what went wrong.
pub fn compile_single_contract(
    name: &str,
    yul_src: &str,
    optimize: bool,
    verify_runtime_bytecode: bool,
) -> Result<ContractBytecode, YulcError> {
    compile_single_contract_with_solc(name, yul_src, optimize, verify_runtime_bytecode, None)
}

/// Compiles a single contract by forwarding the Yul source to a specific `solc` binary.
///
/// When `solc_path` is `None`, falls back to `FE_SOLC_PATH` and then `solc` on `PATH`.
pub fn compile_single_contract_with_solc(
    name: &str,
    yul_src: &str,
    optimize: bool,
    verify_runtime_bytecode: bool,
    solc_path: Option<&str>,
) -> Result<ContractBytecode, YulcError> {
    let input_json = build_standard_json(yul_src, optimize)?;
    let solc_output = run_solc_with_path(&input_json, solc_path)?;
    parse_contract_output(name, &solc_output, verify_runtime_bytecode)
}

/// Builds the standard JSON input description expected by `solc`.
///
/// * `yul_src` - Yul program fed into the compiler.
/// * `optimize` - Toggles optimizer support in the generated JSON.
///
/// Returns a serialized JSON string or a [`YulcError`] if serialization fails.
fn build_standard_json(yul_src: &str, optimize: bool) -> Result<String, YulcError> {
    let value = json!({
        "language": "Yul",
        "sources": {
            "input.yul": { "content": yul_src }
        },
        "settings": {
            "optimizer": {
                "enabled": optimize,
                "details": { "yul": true },
            },
            "outputSelection": {
                "*": {
                    "*": [
                        "evm.bytecode.object",
                        "evm.deployedBytecode.object",
                        "evm.bytecode.sourceMap",
                        "evm.deployedBytecode.sourceMap"
                    ]
                }
            }
        }
    });

    serde_json::to_string(&value).map_err(|err| YulcError(format!("failed to encode json: {err}")))
}

/// Invokes the `solc` binary with the provided standard JSON input.
///
/// * `input` - Serialized standard JSON payload describing the Yul compilation.
///
/// Returns the raw stdout emitted by `solc`, or a [`YulcError`] if the process fails or produces
/// invalid UTF-8.
fn run_solc_with_path(input: &str, solc_path: Option<&str>) -> Result<String, YulcError> {
    let solc_path = solc_path
        .map(str::to_string)
        .or_else(|| env::var(SOLC_ENV).ok())
        .unwrap_or_else(|| "solc".into());

    let mut child = Command::new(&solc_path)
        .arg("--standard-json")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| YulcError(format!("failed to spawn solc binary `{solc_path}`: {err}")))?;

    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or_else(|| YulcError("failed to open stdin for solc process".to_string()))?;
        stdin
            .write_all(input.as_bytes())
            .map_err(|err| YulcError(format!("failed to write solc stdin: {err}")))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| YulcError(format!("failed to read solc output: {err}")))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(YulcError(format!(
            "solc exited with status {}: {stderr}",
            output
                .status
                .code()
                .map(|code| code.to_string())
                .unwrap_or_else(|| "unknown".into())
        )));
    }

    String::from_utf8(output.stdout)
        .map_err(|err| YulcError(format!("solc emitted invalid utf-8 on stdout: {err}")))
}

/// Extracts the contract bytecode for `name` from the raw `solc` JSON output.
///
/// * `name` - Target contract identifier.
/// * `raw_output` - Raw JSON string written by `solc`.
/// * `verify_runtime_bytecode` - When `true`, enforces that deployed runtime bytecode is present.
///
/// Returns the parsed [`ContractBytecode`] or a [`YulcError`] describing why parsing failed.
fn parse_contract_output(
    name: &str,
    raw_output: &str,
    verify_runtime_bytecode: bool,
) -> Result<ContractBytecode, YulcError> {
    let value: Value =
        serde_json::from_str(raw_output).map_err(|err| YulcError(err.to_string()))?;

    // solc will return diagnostics in `errors`. Surface the first one with its
    // formatted message to help users locate the failure.
    if let Some(errors) = value.get("errors").and_then(Value::as_array)
        && let Some(error) = errors.iter().find(|err| {
            err.get("severity")
                .and_then(Value::as_str)
                .unwrap_or("warning")
                == "error"
        })
        && let Some(message) = error.get("formattedMessage").and_then(Value::as_str)
    {
        return Err(YulcError(message.to_string()));
    }

    let contracts = value
        .get("contracts")
        .and_then(|contracts| contracts.get("input.yul"))
        .ok_or_else(|| YulcError("solc output missing `contracts.input.yul`".into()))?;

    let contract = contracts
        .get(name)
        .ok_or_else(|| YulcError(format!("solc output missing contract `{name}`")))?;

    let bytecode = extract_object(contract, &["evm", "bytecode", "object"])
        .ok_or_else(|| YulcError("solc output missing deploy bytecode".into()))?;
    if bytecode == "null" || bytecode.is_empty() {
        return Err(YulcError("solc did not emit deploy bytecode".into()));
    }

    let runtime_bytecode = extract_object(contract, &["evm", "deployedBytecode", "object"])
        .unwrap_or_else(|| "null".into());

    if verify_runtime_bytecode && (runtime_bytecode == "null" || runtime_bytecode.is_empty()) {
        return Err(YulcError(
            "solc did not emit deployed runtime bytecode".into(),
        ));
    }

    Ok(ContractBytecode {
        bytecode,
        runtime_bytecode,
    })
}

/// Traverses a JSON value following `path` segments and returns the final object string.
///
/// * `value` - Root JSON value to inspect.
/// * `path` - Ordered keys representing nested lookups.
///
/// Returns the located string value (stripped of surrounding quotes for non-string JSON values) or
/// `None` if any segment is missing.
fn extract_object(value: &Value, path: &[&str]) -> Option<String> {
    let mut current = value;
    for segment in path {
        current = current.get(*segment)?;
    }
    match current {
        Value::String(s) => Some(s.clone()),
        Value::Null => None,
        other => Some(other.to_string().replace('"', "")),
    }
}

#[cfg(test)]
#[allow(clippy::print_stderr)]
mod tests {
    use super::*;
    use contract_harness::{ExecutionOptions, U256, bytes_to_u256, execute_runtime};
    use std::process::Command;

    fn solc_available() -> bool {
        let solc_path = std::env::var(super::SOLC_ENV).unwrap_or_else(|_| "solc".to_string());
        Command::new(solc_path)
            .arg("--version")
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    }
    #[test]
    fn build_standard_json_contains_fields() {
        let json_str = build_standard_json("{ sstore(0, 0) }", false).unwrap();
        let value: Value = serde_json::from_str(&json_str).unwrap();
        assert_eq!(value["language"], "Yul");
        assert_eq!(value["settings"]["optimizer"]["enabled"], false);
        assert_eq!(value["sources"]["input.yul"]["content"], "{ sstore(0, 0) }");
    }

    #[test]
    fn executes_contract_function() {
        if !solc_available() {
            eprintln!("skipping executes_contract_function because solc is missing");
            return;
        }
        let yul = r#"
object "Double" {
    code {
        datacopy(0, dataoffset("runtime"), datasize("runtime"))
        return(0, datasize("runtime"))
    }
    object "runtime" {
        code {
            let arg := calldataload(4)
            mstore(0x00, mul(arg, 2))
            return(0x00, 0x20)
        }
    }
}
"#;
        let contract = compile_single_contract("Double", yul, false, true)
            .expect("solc should compile handwritten contract");
        let calldata = encode_call_data(10u64);
        let result = execute_runtime(
            &contract.runtime_bytecode,
            &calldata,
            ExecutionOptions::default(),
        )
        .expect("runtime execution should succeed");
        assert_eq!(
            bytes_to_u256(&result.return_data).expect("return data should encode a u256"),
            U256::from(20u64)
        );
    }

    /// Builds calldata for the `Double` contract by ABI-encoding a single `u64`.
    ///
    /// * `value` - Input number to encode into calldata.
    ///
    /// Returns the ABI-encoded bytes prefixed with the function selector.
    fn encode_call_data(value: u64) -> Vec<u8> {
        let mut data = vec![0u8; 4 + 32];
        data[4 + 24..].copy_from_slice(&value.to_be_bytes());
        data
    }

    // execute_runtime and helpers are provided by the contract-harness crate.
}
