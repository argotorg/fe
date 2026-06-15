//! Differential test: Fe's `StoragePackedArray<8>` vs an inlined excerpt of
//! Solady's `LibMap.Uint8Map` (hand-tuned Yul).
//!
//! Both contracts live in `crates/fe/bench_fixtures/storage_packed_array.{fe,sol}`
//! and are the single source of truth shared with the `fe bench` CLI. For each
//! call we drive both backends with identical calldata, assert the return data
//! matches byte-for-byte, and record gas. The rendered report is snapshotted so
//! a compiler change that shifts gas (or breaks parity) surfaces in CI.

use std::path::PathBuf;

use contract_harness::{ExecutionOptions, RuntimeInstance};
use ethers_core::abi::{AbiParser, Token};
use ethers_core::types::U256 as EthU256;
use fe::bench_support::{
    SolOptGasRow, SolOptRuntime, call_sol_opt_variants, compile_fe_sonatina_bytecode,
    compile_solidity_opt_variants, deploy_solidity_opt_variants, render_sol_opt_call_gas_report,
    resolve_solc_path,
};
use test_utils::snap_test;

// ---------------------------------------------------------------------------
// Paths & sources
// ---------------------------------------------------------------------------

fn fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/differential_storage_packed_array")
}

fn bench_fixture(ext: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join(format!("bench_fixtures/storage_packed_array.{ext}"))
}

// ---------------------------------------------------------------------------
// Calldata
// ---------------------------------------------------------------------------

/// ABI-encode a call to `signature` with decimal-string uint256 arguments.
fn calldata(signature: &str, args: &[&str]) -> Vec<u8> {
    let function = AbiParser::default()
        .parse_function(signature)
        .unwrap_or_else(|e| panic!("parse `{signature}`: {e}"));
    let tokens: Vec<Token> = args
        .iter()
        .map(|a| Token::Uint(EthU256::from_dec_str(a).expect("parse uint arg")))
        .collect();
    function
        .encode_input(&tokens)
        .unwrap_or_else(|e| panic!("encode `{signature}`: {e}"))
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

/// One differential call: drive Fe + every Sol variant with the same calldata,
/// assert returns match byte-for-byte, and record gas.
fn drive(
    label: &str,
    fe: &mut RuntimeInstance,
    sols: &mut [SolOptRuntime],
    cd: &[u8],
) -> SolOptGasRow {
    let result = call_sol_opt_variants(label, fe, sols, cd, ExecutionOptions::default());
    for (variant, return_data) in &result.sol_returns {
        assert_eq!(
            &result.fe_return,
            return_data,
            "{label}: return mismatch (fe vs {})",
            variant.report_label()
        );
    }
    result.row
}

#[test]
#[allow(clippy::print_stdout, clippy::print_stderr)]
fn differential_storage_packed_array() {
    let solc_path = resolve_solc_path();
    if solc_path.is_none() {
        eprintln!("skipping: no solc found (set FE_SOLC_PATH or install solc on PATH)");
        return;
    }
    let solc_path_str = solc_path.as_deref();

    let fe_source = std::fs::read_to_string(bench_fixture("fe")).expect("read fe fixture");
    let sol_source = std::fs::read_to_string(bench_fixture("sol")).expect("read sol fixture");

    let fe_bytecode = compile_fe_sonatina_bytecode(&fe_source, "storage_packed_array", "Bench")
        .expect("fe -> sonatina compile");
    let fe_deploy_hex = hex::encode(&fe_bytecode.deploy);
    let mut fe = RuntimeInstance::deploy(&fe_deploy_hex).expect("deploy fe");

    let sol_bytecodes = compile_solidity_opt_variants(&sol_source, "Bench", solc_path_str)
        .unwrap_or_else(|e| panic!("compile Solidity variants: {e}"));
    let (mut sols, _) = deploy_solidity_opt_variants(&sol_bytecodes)
        .unwrap_or_else(|e| panic!("deploy Solidity variants: {e}"));

    // A sequence that exercises packing, lane isolation, read-modify-write, and
    // value truncation at 8 bits. Each call's return data must agree between Fe
    // and Solady; `drive` asserts that and records gas.
    let rows = vec![
        drive(
            "get(5) [empty]",
            &mut fe,
            &mut sols,
            &calldata("get(uint256)", &["5"]),
        ),
        drive(
            "set(5, 7)",
            &mut fe,
            &mut sols,
            &calldata("set(uint256,uint256)", &["5", "7"]),
        ),
        drive(
            "get(5)",
            &mut fe,
            &mut sols,
            &calldata("get(uint256)", &["5"]),
        ),
        drive(
            "setThenGet(10, 200)",
            &mut fe,
            &mut sols,
            &calldata("setThenGet(uint256,uint256)", &["10", "200"]),
        ),
        drive(
            "setTwoLanes(0,1,1,2)",
            &mut fe,
            &mut sols,
            &calldata(
                "setTwoLanes(uint256,uint256,uint256,uint256)",
                &["0", "1", "1", "2"],
            ),
        ),
        drive(
            "get(0)",
            &mut fe,
            &mut sols,
            &calldata("get(uint256)", &["0"]),
        ),
        drive(
            "get(1)",
            &mut fe,
            &mut sols,
            &calldata("get(uint256)", &["1"]),
        ),
        // 300 & 0xff == 44: both sides must truncate to 8 bits identically.
        drive(
            "setThenGet(20, 300) [trunc]",
            &mut fe,
            &mut sols,
            &calldata("setThenGet(uint256,uint256)", &["20", "300"]),
        ),
    ];

    let report = render_sol_opt_call_gas_report(&rows, "call", "fe-O2", 0);
    println!("Differential storage_packed_array — return parity OK.\n");
    println!("{report}");
    let snapshot_path = fixture_dir().join("gas_report");
    snap_test!(report, snapshot_path.to_str().unwrap());
}
