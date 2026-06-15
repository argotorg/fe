//! Shared helpers for benchmark-style Fe/Solidity comparisons.
//!
//! The `fe bench` CLI and heavier differential tests have different execution
//! models. Keep compilation and small reporting helpers here so they can share
//! infrastructure without forcing stateful scenarios into the simple bench
//! fixture format.

use codegen::OptLevel;
use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance};
use driver::DriverDataBase;
pub use solc_runner::SolidityPipeline;
use url::Url;

/// All four Solidity compile variants exercised by the bench/differential
/// pipeline: legacy ±opt and viaIR ±opt.
pub const SOL_VARIANTS: [(SolidityPipeline, bool); 4] = [
    (SolidityPipeline::Legacy, false),
    (SolidityPipeline::Legacy, true),
    (SolidityPipeline::ViaIR, false),
    (SolidityPipeline::ViaIR, true),
];

/// Index of the Solidity variant used as the correctness oracle in
/// differential tests (legacy + optimizer — the historical baseline).
pub const PRIMARY_SOL_IDX: usize = 1;

/// Short label for a Solidity compile variant (used in reports and error messages).
pub fn sol_variant_label(pipeline: SolidityPipeline, optimize: bool) -> &'static str {
    match (pipeline, optimize) {
        (SolidityPipeline::Legacy, false) => "sol",
        (SolidityPipeline::Legacy, true) => "sol+opt",
        (SolidityPipeline::ViaIR, false) => "sol-IR",
        (SolidityPipeline::ViaIR, true) => "sol-IR+opt",
    }
}

/// Compile Solidity source to deploy bytecode hex string.
///
/// Defaults to the legacy pipeline; call [`compile_solidity_pipeline`] to opt
/// into `viaIR`.
pub fn compile_solidity(
    source: &str,
    contract_name: &str,
    optimize: bool,
    solc_path: Option<&str>,
) -> Result<String, String> {
    compile_solidity_pipeline(
        source,
        contract_name,
        optimize,
        SolidityPipeline::Legacy,
        solc_path,
    )
}

/// Compile Solidity source with an explicit pipeline (`legacy` or `viaIR`).
pub fn compile_solidity_pipeline(
    source: &str,
    contract_name: &str,
    optimize: bool,
    pipeline: SolidityPipeline,
    solc_path: Option<&str>,
) -> Result<String, String> {
    compile_solidity_pipeline_bytecode(source, contract_name, optimize, pipeline, solc_path)
        .map(|bc| bc.bytecode)
}

/// Compile Solidity source with an explicit pipeline, returning deploy and
/// runtime bytecode.
pub fn compile_solidity_pipeline_bytecode(
    source: &str,
    contract_name: &str,
    optimize: bool,
    pipeline: SolidityPipeline,
    solc_path: Option<&str>,
) -> Result<solc_runner::ContractBytecode, String> {
    solc_runner::compile_solidity_with_pipeline(
        contract_name,
        source,
        optimize,
        pipeline,
        solc_path,
    )
    .map_err(|e| {
        format!(
            "{} compile error: {}",
            sol_variant_label(pipeline, optimize),
            e.0
        )
    })
}

/// Fe Sonatina deploy and runtime bytecode.
pub struct FeSonatinaBytecode {
    pub deploy: Vec<u8>,
    pub runtime: Vec<u8>,
}

/// Compile Fe source to bytecode via Sonatina backend. Returns deploy bytecode as raw bytes.
pub fn compile_fe_sonatina(
    fe_source: &str,
    name: &str,
    contract_name: &str,
) -> Result<Vec<u8>, String> {
    compile_fe_sonatina_bytecode(fe_source, name, contract_name).map(|bc| bc.deploy)
}

/// Compile Fe source to bytecode via Sonatina backend.
pub fn compile_fe_sonatina_bytecode(
    fe_source: &str,
    name: &str,
    contract_name: &str,
) -> Result<FeSonatinaBytecode, String> {
    let contract_name_owned = contract_name.to_string();
    let name_owned = name.to_string();
    with_fe_ingot(fe_source, name, move |db, ingot| {
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            diags.emit(db);
            return Err(format!("fe/sonatina diagnostics for {name_owned}"));
        }

        let mut map = codegen::emit_ingot_sonatina_bytecode(
            db,
            ingot,
            OptLevel::O2,
            Some(&contract_name_owned),
        )
        .map_err(|err| format!("fe/sonatina emit error for {name_owned}: {err}"))?;
        map.remove(&contract_name_owned)
            .map(|bc| FeSonatinaBytecode {
                deploy: bc.deploy,
                runtime: bc.runtime,
            })
            .ok_or_else(|| {
                format!("fe/sonatina: no bytecode emitted for contract `{contract_name_owned}`")
            })
    })?
}

/// Format a gas value for tabular reports.
pub fn fmt_gas(gas: u64) -> String {
    gas.to_string()
}

/// Format a percentage gas delta as `(lhs - rhs) / rhs`.
pub fn fmt_delta_pct(lhs: u64, rhs: u64) -> String {
    if rhs == 0 {
        return "-".to_string();
    }
    let pct = ((lhs as f64 - rhs as f64) / rhs as f64) * 100.0;
    format!("{pct:+.1}%")
}

/// One row of gas measurements: Fe vs every Solidity variant in [`SOL_VARIANTS`].
pub struct SolGasRow {
    pub label: String,
    pub fe: u64,
    pub sol_variants: [u64; 4],
}

impl SolGasRow {
    /// Lowest gas across all Solidity variants — the strongest baseline for Fe.
    pub fn sol_best(&self) -> u64 {
        *self.sol_variants.iter().min().unwrap()
    }
}

/// Optimized Solidity variants used by stateful differential gas tests.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SolOptVariant {
    LegacyOpt,
    ViaIrOpt,
}

impl SolOptVariant {
    pub const ALL: [Self; 2] = [Self::LegacyOpt, Self::ViaIrOpt];

    pub fn report_label(self) -> &'static str {
        match self {
            Self::LegacyOpt => "sol",
            Self::ViaIrOpt => "sol-IR",
        }
    }

    pub fn compile_label(self) -> &'static str {
        let (pipeline, optimize) = self.compile_options();
        sol_variant_label(pipeline, optimize)
    }

    pub fn compile_options(self) -> (SolidityPipeline, bool) {
        match self {
            Self::LegacyOpt => (SolidityPipeline::Legacy, true),
            Self::ViaIrOpt => (SolidityPipeline::ViaIR, true),
        }
    }

    pub fn is_primary(self) -> bool {
        self == Self::LegacyOpt
    }
}

pub struct SolOptBytecode {
    pub variant: SolOptVariant,
    pub bytecode: solc_runner::ContractBytecode,
}

impl SolOptBytecode {
    pub fn deploy_len(&self) -> usize {
        hex_byte_len(&self.bytecode.bytecode)
    }

    pub fn runtime_len(&self) -> usize {
        hex_byte_len(&self.bytecode.runtime_bytecode)
    }
}

pub fn compile_solidity_opt_variants(
    source: &str,
    contract_name: &str,
    solc_path: Option<&str>,
) -> Result<Vec<SolOptBytecode>, String> {
    SolOptVariant::ALL
        .iter()
        .map(|variant| {
            let (pipeline, optimize) = variant.compile_options();
            let bytecode = compile_solidity_pipeline_bytecode(
                source,
                contract_name,
                optimize,
                pipeline,
                solc_path,
            )?;
            Ok(SolOptBytecode {
                variant: *variant,
                bytecode,
            })
        })
        .collect()
}

#[derive(Clone, Copy, Default)]
pub struct SolOptValues {
    pub legacy_opt: u64,
    pub via_ir_opt: u64,
}

impl SolOptValues {
    pub fn get(self, variant: SolOptVariant) -> u64 {
        match variant {
            SolOptVariant::LegacyOpt => self.legacy_opt,
            SolOptVariant::ViaIrOpt => self.via_ir_opt,
        }
    }

    pub fn set(&mut self, variant: SolOptVariant, value: u64) {
        match variant {
            SolOptVariant::LegacyOpt => self.legacy_opt = value,
            SolOptVariant::ViaIrOpt => self.via_ir_opt = value,
        }
    }

    pub fn add_assign(&mut self, other: Self) {
        self.legacy_opt += other.legacy_opt;
        self.via_ir_opt += other.via_ir_opt;
    }
}

#[derive(Clone)]
pub struct SolOptGasRow {
    pub label: String,
    pub fe: u64,
    pub sol: SolOptValues,
}

impl SolOptGasRow {
    pub fn new(label: impl Into<String>, fe: u64, sol: SolOptValues) -> Self {
        Self {
            label: label.into(),
            fe,
            sol,
        }
    }
}

pub struct SolOptRuntime {
    pub variant: SolOptVariant,
    pub instance: RuntimeInstance,
}

pub fn deploy_solidity_opt_variants(
    bytecodes: &[SolOptBytecode],
) -> Result<(Vec<SolOptRuntime>, SolOptValues), String> {
    let mut gas = SolOptValues::default();
    let mut runtimes = Vec::with_capacity(bytecodes.len());
    for sol in bytecodes {
        let (instance, deploy_gas) = RuntimeInstance::deploy_tracked(&sol.bytecode.bytecode)
            .map_err(|e| format!("deploy {}: {e:?}", sol.variant.compile_label()))?;
        gas.set(sol.variant, deploy_gas);
        runtimes.push(SolOptRuntime {
            variant: sol.variant,
            instance,
        });
    }
    Ok((runtimes, gas))
}

pub fn primary_sol_runtime_mut(sols: &mut [SolOptRuntime]) -> &mut RuntimeInstance {
    sols.iter_mut()
        .find(|sol| sol.variant.is_primary())
        .map(|sol| &mut sol.instance)
        .expect("primary Solidity variant should be tested")
}

pub struct SolOptCallResult {
    pub fe_return: Vec<u8>,
    pub primary_sol_return: Vec<u8>,
    pub sol_returns: Vec<(SolOptVariant, Vec<u8>)>,
    pub row: SolOptGasRow,
}

pub fn call_sol_opt_variants(
    label: &str,
    fe: &mut RuntimeInstance,
    sols: &mut [SolOptRuntime],
    calldata: &[u8],
    options: ExecutionOptions,
) -> SolOptCallResult {
    let fe_res = fe
        .call_raw(calldata, options)
        .unwrap_or_else(|e| panic!("fe {label}: {e:?}"));
    let mut sol_gas = SolOptValues::default();
    let mut primary_sol_return = Vec::new();
    let mut sol_returns = Vec::with_capacity(sols.len());

    for sol in sols {
        let variant_label = sol.variant.compile_label();
        let res = sol
            .instance
            .call_raw(calldata, options)
            .unwrap_or_else(|e| panic!("{variant_label} {label}: {e:?}"));
        sol_gas.set(sol.variant, res.gas_used);
        if sol.variant.is_primary() {
            primary_sol_return = res.return_data.clone();
        }
        sol_returns.push((sol.variant, res.return_data));
    }

    SolOptCallResult {
        fe_return: fe_res.return_data,
        primary_sol_return,
        sol_returns,
        row: SolOptGasRow::new(label, fe_res.gas_used, sol_gas),
    }
}

pub fn render_sol_opt_call_gas_report(
    rows: &[SolOptGasRow],
    label_header: &str,
    fe_header: &str,
    separator_extra: usize,
) -> String {
    let mut out = String::new();
    out.push_str("call gas\n");
    let mut call_rows = rows.to_vec();
    let total_fe = call_rows.iter().map(|row| row.fe).sum();
    let total_sol = call_rows
        .iter()
        .fold(SolOptValues::default(), |mut total, row| {
            total.add_assign(row.sol);
            total
        });
    call_rows.push(SolOptGasRow::new("TOTAL", total_fe, total_sol));
    render_sol_opt_gas_rows(
        &mut out,
        &call_rows,
        label_header,
        fe_header,
        separator_extra,
    );
    out
}

pub fn render_sol_opt_gas_rows(
    out: &mut String,
    rows: &[SolOptGasRow],
    label_header: &str,
    fe_header: &str,
    separator_extra: usize,
) {
    let label_width = rows
        .iter()
        .map(|row| row.label.len())
        .chain(std::iter::once(label_header.len()))
        .max()
        .unwrap();
    let delta_width = rows
        .iter()
        .map(|row| {
            let sol_ir = row.sol.get(SolOptVariant::ViaIrOpt);
            let delta_abs = row.fe as i64 - sol_ir as i64;
            format!("{delta_abs:+} ({})", fmt_delta_pct(row.fe, sol_ir)).len()
        })
        .chain(std::iter::once("fe vs sol-IR".len()))
        .max()
        .unwrap();
    out.push_str(&format!(
        "{:<lw$} {:>10} {:>10} {:>10}  {:>dw$}\n",
        label_header,
        fe_header,
        SolOptVariant::LegacyOpt.report_label(),
        SolOptVariant::ViaIrOpt.report_label(),
        "fe vs sol-IR",
        lw = label_width,
        dw = delta_width,
    ));
    out.push_str(&format!(
        "{}\n",
        "-".repeat(label_width + 1 + 10 * 3 + 2 + delta_width + separator_extra)
    ));
    for row in rows {
        let sol_ir = row.sol.get(SolOptVariant::ViaIrOpt);
        let delta_abs = row.fe as i64 - sol_ir as i64;
        let delta = format!("{delta_abs:+} ({})", fmt_delta_pct(row.fe, sol_ir));
        out.push_str(&format!(
            "{:<lw$} {:>10} {:>10} {:>10}  {:>dw$}\n",
            row.label,
            row.fe,
            row.sol.get(SolOptVariant::LegacyOpt),
            row.sol.get(SolOptVariant::ViaIrOpt),
            delta,
            lw = label_width,
            dw = delta_width,
        ));
    }
}

pub fn resolve_solc_path() -> Option<String> {
    std::env::var_os("FE_SOLC_PATH")
        .map(std::path::PathBuf::from)
        .filter(|p| p.is_file())
        .or_else(|| find_executable_in_path("solc"))
        .map(|p| p.to_string_lossy().into_owned())
}

fn find_executable_in_path(name: &str) -> Option<std::path::PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|dir| dir.join(name))
        .find(|candidate| candidate.is_file())
}

fn hex_byte_len(hex: &str) -> usize {
    assert_eq!(hex.len() % 2, 0, "bytecode hex length should be even");
    hex.len() / 2
}

/// Print Fe vs all four Solidity variants with `best` and `fe vs best` columns.
///
/// `label_header` and `label_width` control the leftmost column (the rest is
/// fixed-width). Designed for shared use between the bench CLI and the
/// differential-deposit gas report.
#[allow(clippy::print_stdout)]
pub fn print_sol_gas_table(rows: &[SolGasRow], label_header: &str, label_width: usize) {
    let variant_headers: Vec<&'static str> = SOL_VARIANTS
        .iter()
        .map(|(p, o)| sol_variant_label(*p, *o))
        .collect();
    println!(
        "{:<lw$} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>14}",
        label_header,
        "fe gas",
        variant_headers[0],
        variant_headers[1],
        variant_headers[2],
        variant_headers[3],
        "best",
        "fe vs best",
        lw = label_width,
    );
    println!("{}", "-".repeat(label_width + 1 + 10 * 6 + 1 + 14 + 6));
    for row in rows {
        let best = row.sol_best();
        let delta_abs = row.fe as i64 - best as i64;
        let delta_pct = fmt_delta_pct(row.fe, best);
        println!(
            "{:<lw$} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}   {:>+6} ({:>5})",
            row.label,
            row.fe,
            row.sol_variants[0],
            row.sol_variants[1],
            row.sol_variants[2],
            row.sol_variants[3],
            best,
            delta_abs,
            delta_pct,
            lw = label_width,
        );
    }
}

/// Set up a temp ingot and run a callback with `(db, ingot)`.
///
/// Writes `fe.toml` + `src/lib.fe` to a temp directory, then calls `init_ingot`
/// so that `std` and `core` are resolved correctly.
fn with_fe_ingot<T>(
    fe_source: &str,
    name: &str,
    f: impl for<'db> FnOnce(&'db DriverDataBase, hir::Ingot<'db>) -> T,
) -> Result<T, String> {
    let tmp = tempfile::Builder::new()
        .prefix(&format!("fe_bench_{name}_"))
        .tempdir()
        .map_err(|e| format!("create temp ingot dir for {name}: {e}"))?;
    std::fs::create_dir_all(tmp.path().join("src"))
        .map_err(|e| format!("create temp ingot dir for {name}: {e}"))?;
    std::fs::write(
        tmp.path().join("fe.toml"),
        format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
    )
    .map_err(|e| format!("write fe.toml for {name}: {e}"))?;
    std::fs::write(tmp.path().join("src").join("lib.fe"), fe_source)
        .map_err(|e| format!("write lib.fe for {name}: {e}"))?;

    let ingot_url = Url::from_directory_path(tmp.path())
        .map_err(|_| format!("non-utf8 temp ingot path for {name}"))?;
    let mut db = DriverDataBase::default();
    let had_errors = driver::init_ingot(&mut db, &ingot_url);
    if had_errors {
        return Err(format!("fe ingot init errors for {name}"));
    }

    let ingot = db
        .workspace()
        .containing_ingot(&db, ingot_url)
        .ok_or_else(|| format!("no containing ingot for {name}"))?;
    Ok(f(&db, ingot))
}
