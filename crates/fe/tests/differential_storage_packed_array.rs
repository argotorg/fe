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
    SolidityPipeline, compile_fe_sonatina_bytecode, compile_solidity_pipeline_bytecode,
    sol_variant_label,
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

fn find_executable_in_path(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|dir| dir.join(name))
        .find(|candidate| candidate.is_file())
}

fn resolve_solc_path() -> Option<String> {
    std::env::var_os("FE_SOLC_PATH")
        .map(PathBuf::from)
        .filter(|p| p.is_file())
        .or_else(|| find_executable_in_path("solc"))
        .map(|p| p.to_string_lossy().into_owned())
}

// ---------------------------------------------------------------------------
// Solidity variants
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SolVariant {
    LegacyOpt,
    ViaIrOpt,
}

impl SolVariant {
    const ALL: [Self; 2] = [Self::LegacyOpt, Self::ViaIrOpt];

    fn report_label(self) -> &'static str {
        match self {
            Self::LegacyOpt => "sol",
            Self::ViaIrOpt => "sol-IR",
        }
    }

    fn compile_options(self) -> (SolidityPipeline, bool) {
        match self {
            Self::LegacyOpt => (SolidityPipeline::Legacy, true),
            Self::ViaIrOpt => (SolidityPipeline::ViaIR, true),
        }
    }
}

#[derive(Clone, Copy, Default)]
struct SolValues {
    legacy_opt: u64,
    via_ir_opt: u64,
}

impl SolValues {
    fn get(self, variant: SolVariant) -> u64 {
        match variant {
            SolVariant::LegacyOpt => self.legacy_opt,
            SolVariant::ViaIrOpt => self.via_ir_opt,
        }
    }

    fn set(&mut self, variant: SolVariant, value: u64) {
        match variant {
            SolVariant::LegacyOpt => self.legacy_opt = value,
            SolVariant::ViaIrOpt => self.via_ir_opt = value,
        }
    }
}

struct SolRuntime {
    variant: SolVariant,
    instance: RuntimeInstance,
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
// Report
// ---------------------------------------------------------------------------

struct ReportRow {
    label: String,
    fe: u64,
    sol: SolValues,
}

fn delta_pct(lhs: u64, rhs: u64) -> String {
    if rhs == 0 {
        return "-".into();
    }
    let pct = ((lhs as f64 - rhs as f64) / rhs as f64) * 100.0;
    format!("{pct:+.1}%")
}

/// Render `fe / sol / sol-IR / delta-vs-sol-IR` columns, plus a TOTAL row.
fn render(rows: &[ReportRow]) -> String {
    let total = ReportRow {
        label: "TOTAL".into(),
        fe: rows.iter().map(|r| r.fe).sum(),
        sol: rows.iter().fold(SolValues::default(), |mut acc, r| {
            acc.legacy_opt += r.sol.legacy_opt;
            acc.via_ir_opt += r.sol.via_ir_opt;
            acc
        }),
    };
    let all: Vec<&ReportRow> = rows.iter().chain(std::iter::once(&total)).collect();

    let label_width = all
        .iter()
        .map(|r| r.label.len())
        .chain(std::iter::once("call".len()))
        .max()
        .unwrap();
    let delta_width = all
        .iter()
        .map(|r| {
            let sol_ir = r.sol.get(SolVariant::ViaIrOpt);
            let abs = r.fe as i64 - sol_ir as i64;
            format!("{abs:+} ({})", delta_pct(r.fe, sol_ir)).len()
        })
        .chain(std::iter::once("fe vs sol-IR".len()))
        .max()
        .unwrap();

    let mut out = String::new();
    out.push_str("call gas\n");
    out.push_str(&format!(
        "{:<lw$} {:>10} {:>10} {:>10}  {:>dw$}\n",
        "call",
        "fe-O2",
        SolVariant::LegacyOpt.report_label(),
        SolVariant::ViaIrOpt.report_label(),
        "fe vs sol-IR",
        lw = label_width,
        dw = delta_width,
    ));
    out.push_str(&format!(
        "{}\n",
        "-".repeat(label_width + 1 + 10 * 3 + 2 + delta_width)
    ));
    for r in &all {
        let sol_ir = r.sol.get(SolVariant::ViaIrOpt);
        let abs = r.fe as i64 - sol_ir as i64;
        let delta = format!("{abs:+} ({})", delta_pct(r.fe, sol_ir));
        out.push_str(&format!(
            "{:<lw$} {:>10} {:>10} {:>10}  {:>dw$}\n",
            r.label,
            r.fe,
            r.sol.get(SolVariant::LegacyOpt),
            r.sol.get(SolVariant::ViaIrOpt),
            delta,
            lw = label_width,
            dw = delta_width,
        ));
    }
    out
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

/// One differential call: drive Fe + every Sol variant with the same calldata,
/// assert returns match byte-for-byte, and record gas.
fn drive(label: &str, fe: &mut RuntimeInstance, sols: &mut [SolRuntime], cd: &[u8]) -> ReportRow {
    let fe_res = fe
        .call_raw(cd, ExecutionOptions::default())
        .unwrap_or_else(|e| panic!("fe {label}: {e:?}"));

    let mut sol = SolValues::default();
    for s in sols {
        let res = s
            .instance
            .call_raw(cd, ExecutionOptions::default())
            .unwrap_or_else(|e| panic!("{} {label}: {e:?}", s.variant.report_label()));
        sol.set(s.variant, res.gas_used);
        assert_eq!(
            fe_res.return_data,
            res.return_data,
            "{label}: return mismatch (fe vs {})",
            s.variant.report_label()
        );
    }

    ReportRow {
        label: label.into(),
        fe: fe_res.gas_used,
        sol,
    }
}

#[test]
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

    let mut sols: Vec<SolRuntime> = SolVariant::ALL
        .iter()
        .map(|variant| {
            let (pipeline, optimize) = variant.compile_options();
            let bc = compile_solidity_pipeline_bytecode(
                &sol_source,
                "Bench",
                optimize,
                pipeline,
                solc_path_str,
            )
            .unwrap_or_else(|e| panic!("{} compile: {e}", sol_variant_label(pipeline, optimize)));
            let instance = RuntimeInstance::deploy(&bc.bytecode)
                .unwrap_or_else(|e| panic!("deploy {}: {e:?}", variant.report_label()));
            SolRuntime {
                variant: *variant,
                instance,
            }
        })
        .collect();

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

    let report = render(&rows);
    println!("Differential storage_packed_array — return parity OK.\n");
    println!("{report}");
    let snapshot_path = fixture_dir().join("gas_report");
    snap_test!(report, snapshot_path.to_str().unwrap());
}
