//! Gas benchmarking: compares Fe (Sonatina) against Solidity.
//!
//! Discovers paired `.fe` / `.sol` fixture files with a `.toml` manifest,
//! compiles each backend, deploys and calls them, and reports per-function
//! gas consumption.

use std::fmt::Write as _;
use std::fs;

use crate::bench_support::{
    SOL_VARIANTS, SolGasRow, compile_fe_sonatina, compile_solidity_pipeline, fmt_gas,
    print_sol_gas_table, sol_variant_label,
};
use camino::{Utf8Path, Utf8PathBuf};
use contract_harness::{ExecutionOptions, RuntimeInstance};
use ethers_core::abi::AbiParser;

/// A single benchmark fixture: paired Fe + Solidity sources with a call manifest.
struct BenchFixture {
    name: String,
    fe_source: String,
    sol_source: String,
    contract_name: String,
    calls: Vec<BenchCall>,
}

/// A function call to benchmark.
struct BenchCall {
    /// Solidity-style function signature, e.g. `"add(uint256,uint256)"`.
    signature: String,
    /// Hex-encoded argument values (no 0x prefix), each padded to 32 bytes.
    args: Vec<String>,
}

/// Gas measurement for a single function call across all backends.
struct BenchResult {
    fixture: String,
    function: String,
    fe_sonatina_gas: u64,
    /// Gas per Solidity variant, indexed by [`SOL_VARIANTS`].
    sol_gas: [u64; 4],
}

impl BenchResult {
    fn sol_best(&self) -> u64 {
        *self.sol_gas.iter().min().unwrap()
    }
}

/// TOML manifest deserialized from `<fixture>.toml`.
#[derive(serde::Deserialize)]
struct Manifest {
    contract: String,
    #[serde(default)]
    calls: Vec<ManifestCall>,
}

#[derive(serde::Deserialize)]
struct ManifestCall {
    function: String,
    #[serde(default)]
    args: Vec<String>,
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

#[allow(clippy::print_stdout)]
pub fn run_benchmarks(
    path: &Utf8Path,
    filter: Option<&str>,
    solc: Option<&str>,
    output: Option<&Utf8Path>,
) -> Result<(), String> {
    let path = resolve_fixtures_dir(path)?;
    let fixtures = discover_fixtures(&path, filter)?;
    if fixtures.is_empty() {
        return Err(format!("no benchmark fixtures found in {path}"));
    }

    println!("Found {} benchmark fixture(s)\n", fixtures.len());

    let mut all_results: Vec<BenchResult> = Vec::new();

    for fixture in &fixtures {
        println!("--- {} ---", fixture.name);

        let mut sol_bytecodes: Vec<String> = Vec::with_capacity(SOL_VARIANTS.len());
        for (pipeline, optimize) in SOL_VARIANTS {
            sol_bytecodes.push(
                compile_solidity_pipeline(
                    &fixture.sol_source,
                    &fixture.contract_name,
                    optimize,
                    pipeline,
                    solc,
                )
                .map_err(|e| format!("[{}] {e}", fixture.name))?,
            );
        }
        let fe_sonatina_bytecode =
            compile_fe_sonatina(&fixture.fe_source, &fixture.name, &fixture.contract_name)
                .map_err(|e| format!("[{}] {e}", fixture.name))?;

        for call in &fixture.calls {
            let calldata = encode_calldata(&call.signature, &call.args)
                .map_err(|e| format!("[{}] {}: {e}", fixture.name, call.signature))?;

            let fe_sonatina_gas = measure_call_bytes(&fe_sonatina_bytecode, &calldata)
                .map_err(|e| format!("[{}/fe] {}: {e}", fixture.name, call.signature))?;

            let mut sol_gas = [0u64; 4];
            for (idx, ((pipeline, optimize), bytecode)) in SOL_VARIANTS
                .iter()
                .copied()
                .zip(sol_bytecodes.iter())
                .enumerate()
            {
                let label = sol_variant_label(pipeline, optimize);
                sol_gas[idx] = measure_call(bytecode, &calldata)
                    .map_err(|e| format!("[{}/{label}] {}: {e}", fixture.name, call.signature))?;
            }

            let fn_name = call.signature.split('(').next().unwrap_or(&call.signature);
            all_results.push(BenchResult {
                fixture: fixture.name.clone(),
                function: fn_name.to_string(),
                fe_sonatina_gas,
                sol_gas,
            });
        }
    }

    print_sol_gas_table(
        &all_results
            .iter()
            .map(|r| SolGasRow {
                label: format!("{}/{}", r.fixture, r.function),
                fe: r.fe_sonatina_gas,
                sol_variants: r.sol_gas,
            })
            .collect::<Vec<_>>(),
        "fixture/fn",
        32,
    );

    if let Some(out_dir) = output {
        write_csv(&all_results, out_dir)?;
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Fixture discovery
// ---------------------------------------------------------------------------

/// If `path` doesn't exist, try common locations relative to the repo root.
fn resolve_fixtures_dir(path: &Utf8Path) -> Result<Utf8PathBuf, String> {
    if path.exists() {
        return Ok(path.to_path_buf());
    }

    // When running from the repo root, check crates/fe/<path>
    let under_crate = Utf8PathBuf::from("crates/fe").join(path);
    if under_crate.exists() {
        return Ok(under_crate);
    }

    Err(format!("fixtures directory does not exist: {path}"))
}

#[allow(clippy::print_stderr)]
fn discover_fixtures(dir: &Utf8Path, filter: Option<&str>) -> Result<Vec<BenchFixture>, String> {
    if !dir.exists() {
        return Err(format!("fixtures directory does not exist: {dir}"));
    }

    let mut fixtures = Vec::new();
    let mut toml_files: Vec<_> = fs::read_dir(dir.as_std_path())
        .map_err(|e| format!("failed to read {dir}: {e}"))?
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "toml"))
        .collect();
    toml_files.sort_by_key(|e| e.file_name());

    for entry in toml_files {
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|p| format!("non-utf8 path: {}", p.display()))?;
        let stem = path
            .file_stem()
            .ok_or_else(|| format!("no file stem: {path}"))?;

        if let Some(f) = filter
            && !stem.contains(f)
        {
            continue;
        }

        let fe_path = dir.join(format!("{stem}.fe"));
        let sol_path = dir.join(format!("{stem}.sol"));

        if !fe_path.exists() {
            eprintln!("warning: skipping {stem} — missing {fe_path}");
            continue;
        }
        if !sol_path.exists() {
            eprintln!("warning: skipping {stem} — missing {sol_path}");
            continue;
        }

        let manifest_str =
            fs::read_to_string(path.as_std_path()).map_err(|e| format!("read {path}: {e}"))?;
        let manifest: Manifest =
            toml::from_str(&manifest_str).map_err(|e| format!("parse {path}: {e}"))?;

        let fe_source = fs::read_to_string(fe_path.as_std_path())
            .map_err(|e| format!("read {fe_path}: {e}"))?;
        let sol_source = fs::read_to_string(sol_path.as_std_path())
            .map_err(|e| format!("read {sol_path}: {e}"))?;

        let calls = manifest
            .calls
            .into_iter()
            .map(|c| BenchCall {
                signature: c.function,
                args: c.args,
            })
            .collect();

        fixtures.push(BenchFixture {
            name: stem.to_string(),
            fe_source,
            sol_source,
            contract_name: manifest.contract,
            calls,
        });
    }

    Ok(fixtures)
}

// ---------------------------------------------------------------------------
// Execution / measurement
// ---------------------------------------------------------------------------

/// Encode calldata from a function signature and string arguments.
///
/// Argument format depends on the ABI type: integers are decimal, addresses
/// are hex (`0x…`), bools are `true`/`false`.
fn encode_calldata(signature: &str, args: &[String]) -> Result<Vec<u8>, String> {
    let function = AbiParser::default()
        .parse_function(signature)
        .map_err(|e| format!("bad signature `{signature}`: {e}"))?;

    if args.len() != function.inputs.len() {
        return Err(format!(
            "signature `{signature}` expects {} arg(s), got {}",
            function.inputs.len(),
            args.len(),
        ));
    }

    let tokens: Vec<ethers_core::abi::Token> = args
        .iter()
        .zip(function.inputs.iter())
        .map(|(val, param)| parse_arg(val, &param.kind))
        .collect::<Result<Vec<_>, _>>()?;

    let encoded = function
        .encode_input(&tokens)
        .map_err(|e| format!("encode error: {e}"))?;
    Ok(encoded)
}

/// Parse a string argument into an ABI token based on the expected param type.
fn parse_arg(
    val: &str,
    kind: &ethers_core::abi::ParamType,
) -> Result<ethers_core::abi::Token, String> {
    use ethers_core::abi::{ParamType, Token};
    match kind {
        ParamType::Uint(bits) => {
            let n = ethers_core::types::U256::from_dec_str(val)
                .map_err(|e| format!("cannot parse `{val}` as uint: {e}"))?;
            if n.bits() > *bits {
                return Err(format!("`{val}` does not fit in uint{bits}"));
            }
            Ok(Token::Uint(n))
        }
        ParamType::Int(bits) => {
            let n = ethers_core::types::I256::from_dec_str(val)
                .map_err(|e| format!("cannot parse `{val}` as int: {e}"))?;
            if n.bits() as usize > *bits {
                return Err(format!("`{val}` does not fit in int{bits}"));
            }
            Ok(Token::Int(n.into_raw()))
        }
        ParamType::Bool => {
            let b: bool = val
                .parse()
                .map_err(|e| format!("cannot parse `{val}` as bool: {e}"))?;
            Ok(Token::Bool(b))
        }
        ParamType::Address => {
            let addr: ethers_core::types::Address = val
                .parse()
                .map_err(|e| format!("cannot parse `{val}` as address: {e}"))?;
            Ok(Token::Address(addr))
        }
        _ => Err(format!("unsupported param type: {kind}")),
    }
}

/// Deploy a contract from hex-encoded init bytecode and call it.
fn measure_call(bytecode_hex: &str, calldata: &[u8]) -> Result<u64, String> {
    let mut instance =
        RuntimeInstance::deploy(bytecode_hex).map_err(|e| format!("deploy failed: {e}"))?;
    let result = instance
        .call_raw(calldata, ExecutionOptions::default())
        .map_err(|e| format!("call failed: {e}"))?;
    Ok(result.gas_used)
}

/// Deploy a contract from raw bytes and call it.
fn measure_call_bytes(bytecode: &[u8], calldata: &[u8]) -> Result<u64, String> {
    measure_call(&hex::encode(bytecode), calldata)
}

// ---------------------------------------------------------------------------
// Reporting
// ---------------------------------------------------------------------------

fn fmt_delta_pct_csv(lhs: u64, rhs: u64) -> String {
    if rhs == 0 {
        return String::new();
    }
    format!("{:.2}", ((lhs as f64 - rhs as f64) / rhs as f64) * 100.0)
}

#[allow(clippy::print_stdout)]
fn write_csv(results: &[BenchResult], out_dir: &Utf8Path) -> Result<(), String> {
    fs::create_dir_all(out_dir.as_std_path()).map_err(|e| format!("create dir {out_dir}: {e}"))?;

    let path = out_dir.join("gas_benchmark.csv");
    let mut csv = String::new();
    writeln!(
        csv,
        "fixture,function,fe_sonatina,sol,sol_opt,sol_ir,sol_ir_opt,sol_best,delta_fe_sonatina_vs_sol_best_pct"
    )
    .unwrap();

    for r in results {
        let best = r.sol_best();
        writeln!(
            csv,
            "{},{},{},{},{},{},{},{},{}",
            r.fixture,
            r.function,
            fmt_gas(r.fe_sonatina_gas),
            fmt_gas(r.sol_gas[0]),
            fmt_gas(r.sol_gas[1]),
            fmt_gas(r.sol_gas[2]),
            fmt_gas(r.sol_gas[3]),
            fmt_gas(best),
            fmt_delta_pct_csv(r.fe_sonatina_gas, best),
        )
        .unwrap();
    }

    fs::write(path.as_std_path(), &csv).map_err(|e| format!("write csv: {e}"))?;
    println!(
        "\nCSV report written to {}",
        out_dir.join("gas_benchmark.csv")
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::parse_arg;
    use ethers_core::{
        abi::{ParamType, Token},
        types::U256,
    };

    #[test]
    fn parse_signed_int_uses_full_word_twos_complement() {
        assert_eq!(
            parse_arg("-1", &ParamType::Int(256)).unwrap(),
            Token::Int(U256::MAX)
        );
        assert_eq!(
            parse_arg("-2", &ParamType::Int(256)).unwrap(),
            Token::Int(U256::MAX - U256::from(1u8))
        );
        assert_eq!(
            parse_arg("42", &ParamType::Int(256)).unwrap(),
            Token::Int(U256::from(42u8))
        );
    }

    #[test]
    fn parse_arg_rejects_values_outside_declared_width() {
        assert!(parse_arg("255", &ParamType::Uint(8)).is_ok());
        assert!(parse_arg("256", &ParamType::Uint(8)).is_err());
        assert!(parse_arg("127", &ParamType::Int(8)).is_ok());
        assert!(parse_arg("-128", &ParamType::Int(8)).is_ok());
        assert!(parse_arg("128", &ParamType::Int(8)).is_err());
        assert!(parse_arg("-129", &ParamType::Int(8)).is_err());
    }
}
