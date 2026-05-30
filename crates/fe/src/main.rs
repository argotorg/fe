#![allow(clippy::print_stderr, clippy::print_stdout)]
mod abi;
mod analyze;
mod build;
mod check;
mod cli;
mod debug_cli;
mod dependency_diagnostics;
mod doc;
#[cfg(feature = "doc-server")]
mod doc_serve;
mod report;
mod shape_cli;
mod test;
mod trace;
#[cfg(not(target_arch = "wasm32"))]
mod tree;
mod workspace_ingot;

use std::{fs, io::Read};

use build::build;
use camino::Utf8PathBuf;
use check::check;
use clap::{Args, CommandFactory, Parser, Subcommand, ValueEnum};
use colored::Colorize;
use fmt as fe_fmt;
use similar::{ChangeTag, TextDiff};
use walkdir::WalkDir;

use crate::test::TestDebugOptions;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum BuildEmit {
    Bytecode,
    RuntimeBytecode,
    Ir,
    Abi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum TestEmit {
    Ir,
    Rmir,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum AnalyzeFormat {
    Text,
    Json,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum TraceReportFormat {
    Text,
    Json,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum DatalogExportFormat {
    Csv,
    Jsonl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum DebugExportFormat {
    Dwarf,
    Ethdebug,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum RuntimeCaptureModeArg {
    Minimal,
    Standard,
    Full,
    DebugFull,
}

impl From<RuntimeCaptureModeArg> for trace_facts::RuntimeCaptureMode {
    fn from(value: RuntimeCaptureModeArg) -> Self {
        match value {
            RuntimeCaptureModeArg::Minimal => Self::Minimal,
            RuntimeCaptureModeArg::Standard => Self::Standard,
            RuntimeCaptureModeArg::Full => Self::Full,
            RuntimeCaptureModeArg::DebugFull => Self::DebugFull,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum RuntimeValuePolicyArg {
    Redacted,
    HashOnly,
    Full,
}

impl From<RuntimeValuePolicyArg> for trace_facts::RuntimeValuePolicy {
    fn from(value: RuntimeValuePolicyArg) -> Self {
        match value {
            RuntimeValuePolicyArg::Redacted => Self::Redacted,
            RuntimeValuePolicyArg::HashOnly => Self::HashOnly,
            RuntimeValuePolicyArg::Full => Self::Full,
        }
    }
}

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    /// Control colored output (auto, always, never).
    #[arg(long, global = true, value_enum, default_value = "auto")]
    pub color: ColorChoice,
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Args)]
pub struct OptimizeArgs {
    /// Optimization level.
    ///
    /// 0 = none
    /// 1 = fast compilation, usually close to `2` gas and bytecode size with Sonatina
    /// 2 = optimizes heavily for runtime gas
    /// s = size-oriented (currently similar to `2`)
    ///
    /// Defaults to `1`
    ///
    #[arg(
        long = "optimize",
        short = 'O',
        value_name = "LEVEL",
        value_parser = ["0", "1", "2", "s"],
        verbatim_doc_comment
    )]
    optimize: Option<String>,
}

impl OptimizeArgs {
    fn as_deref(&self) -> Option<&str> {
        self.optimize.as_deref()
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    /// Compile Fe code to EVM bytecode.
    Build {
        /// Path to an ingot/workspace directory (containing fe.toml), a workspace member name, or a .fe file.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Build artifacts for a single workspace ingot by member name.
        ///
        /// This requires targeting a workspace root path.
        #[arg(short = 'i', long = "ingot", value_name = "INGOT")]
        ingot: Option<String>,
        /// Treat a `.fe` file target as standalone, even if it is inside an ingot.
        #[arg(long)]
        standalone: bool,
        /// Build a specific contract by name (defaults to all contracts in the target).
        #[arg(long)]
        contract: Option<String>,
        #[command(flatten)]
        optimize: OptimizeArgs,
        /// Output directory for artifacts.
        #[arg(long)]
        out_dir: Option<Utf8PathBuf>,
        /// Compilation profile to use when resolving profile-aware config.
        #[arg(long, default_value = "release", value_name = "PROFILE")]
        profile: String,
        /// Comma-delimited artifacts to emit.
        #[arg(
            long,
            short = 'e',
            value_enum,
            value_delimiter = ',',
            default_value = "bytecode,runtime-bytecode,abi"
        )]
        emit: Vec<BuildEmit>,
        /// Write a debugging report as a `.tar.gz` file (includes sources, IR, backend output, and bytecode artifacts).
        #[arg(long)]
        report: bool,
        /// Output path for `--report` (must end with `.tar.gz`).
        #[arg(
            long,
            value_name = "OUT",
            default_value = "fe-build-report.tar.gz",
            requires = "report"
        )]
        report_out: Utf8PathBuf,
        /// Only write the report if `fe build` fails.
        #[arg(long, requires = "report")]
        report_failed_only: bool,
        /// Use recovery mode when parsing.
        #[arg(long, default_value = "false")]
        recovery_mode: bool,
    },
    Check {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Check a single workspace ingot by member name.
        ///
        /// This requires targeting a workspace root path.
        #[arg(short = 'i', long = "ingot", value_name = "INGOT")]
        ingot: Option<String>,
        /// Treat a `.fe` file target as standalone, even if it is inside an ingot.
        #[arg(long)]
        standalone: bool,
        /// Compilation profile to use when resolving profile-aware config.
        #[arg(long, default_value = "dev", value_name = "PROFILE")]
        profile: String,
        #[arg(long)]
        dump_mir: bool,
        /// Write a debugging report as a `.tar.gz` file (includes sources and diagnostics).
        #[arg(long)]
        report: bool,
        /// Output path for `--report` (must end with `.tar.gz`).
        #[arg(
            long,
            value_name = "OUT",
            default_value = "fe-check-report.tar.gz",
            requires = "report"
        )]
        report_out: Utf8PathBuf,
        /// Only write the report if `fe check` fails.
        #[arg(long, requires = "report")]
        report_failed_only: bool,
        /// Use recovery mode when parsing.
        #[arg(long, default_value = "false")]
        recovery_mode: bool,
    },
    /// Analyze a Fe target using normal compiler target resolution.
    Analyze {
        /// Path to an ingot/workspace directory, a workspace member name, or a .fe file.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Analyze a single workspace ingot by member name.
        ///
        /// This requires targeting a workspace root path.
        #[arg(short = 'i', long = "ingot", value_name = "INGOT")]
        ingot: Option<String>,
        /// Treat a `.fe` file target as standalone, even if it is inside an ingot.
        #[arg(long)]
        standalone: bool,
        /// Compilation profile to use when resolving profile-aware config.
        #[arg(long, default_value = "dev", value_name = "PROFILE")]
        profile: String,
        /// Output format.
        #[arg(long, value_enum, default_value = "text")]
        format: AnalyzeFormat,
        /// Analyze generated test entrypoints instead of runtime entrypoints.
        #[arg(long)]
        tests: bool,
        /// Include typed origin facts in the report.
        #[arg(long)]
        origin_facts: bool,
        /// Include relation-table projections generated from typed facts.
        #[arg(long, requires = "origin_facts")]
        fact_relation_tables: bool,
        /// Use recovery mode when parsing.
        #[arg(long, default_value = "false")]
        recovery_mode: bool,
    },
    /// Run unstable developer tooling.
    Dev {
        #[command(subcommand)]
        command: DevCommand,
    },
    /// Emit and compare derived content-addressed shape views.
    Shape {
        #[command(subcommand)]
        command: ShapeCommand,
    },
    /// Generate documentation for a Fe project
    Doc {
        /// Path to a .fe file or ingot directory
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Output directory for generated docs
        #[arg(short, long)]
        output: Option<Utf8PathBuf>,
        /// Include builtin ingots (core, std) in generated docs
        #[arg(long)]
        builtins: bool,
        /// Load core/std from a directory on disk instead of the embedded version.
        /// The directory should contain `core/` and `std/` subdirectories.
        #[arg(long)]
        stdlib_path: Option<Utf8PathBuf>,
        #[command(subcommand)]
        action: Option<DocAction>,
    },
    #[cfg(not(target_arch = "wasm32"))]
    Tree {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
    },
    /// Format Fe source code.
    Fmt {
        /// Path to a Fe source file or directory. If omitted, formats all .fe files in the current project.
        path: Option<Utf8PathBuf>,
        /// Check if files are formatted, but do not write changes.
        #[arg(long)]
        check: bool,
    },
    /// Run Fe tests in a file or directory.
    Test {
        /// Path(s) to .fe files or directories containing ingots with tests.
        ///
        /// Supports glob patterns (e.g. `crates/fe/tests/fixtures/fe_test/*.fe`).
        ///
        /// When omitted, defaults to the current project root (like `cargo test`).
        #[arg(value_name = "PATH", num_args = 0..)]
        paths: Vec<Utf8PathBuf>,
        /// Run tests for a single workspace ingot by member name
        ///
        /// This requires targeting a workspace root path.
        #[arg(short = 'i', long = "ingot", value_name = "INGOT")]
        ingot: Option<String>,
        /// Optional filter pattern for test names.
        #[arg(short, long)]
        filter: Option<String>,
        /// Number of suites to run in parallel (0 = auto).
        #[arg(long, default_value_t = 8, value_name = "N")]
        jobs: usize,
        /// Run suites as grouped jobs instead of splitting into per-test jobs.
        #[arg(long)]
        grouped: bool,
        /// Show event logs from test execution.
        #[arg(long)]
        show_logs: bool,
        /// Write test-module IR artifacts (`ir`, `rmir`) to the suite `out/` directory.
        #[arg(long, value_enum, value_delimiter = ',')]
        emit: Vec<TestEmit>,
        /// Compilation profile to use when resolving profile-aware config.
        #[arg(long, default_value = "test", value_name = "PROFILE")]
        profile: String,
        #[command(flatten)]
        optimize: OptimizeArgs,
        /// Trace executed EVM opcodes while running tests.
        #[arg(long)]
        trace_evm: bool,
        /// How many EVM steps to keep in the trace ring buffer.
        #[arg(long, default_value_t = 200)]
        trace_evm_keep: usize,
        /// How many stack items to print per EVM step in traces.
        #[arg(long, default_value_t = 16)]
        trace_evm_stack_n: usize,
        /// Directory to write debug outputs (traces) into.
        #[arg(long)]
        debug_dir: Option<Utf8PathBuf>,
        /// Write a debugging report as a `.tar.gz` file (includes sources, IR, bytecode, traces).
        #[arg(long)]
        report: bool,
        /// Output path for `--report` (must end with `.tar.gz`).
        #[arg(
            long,
            value_name = "OUT",
            default_value = "fe-test-report.tar.gz",
            requires = "report"
        )]
        report_out: Utf8PathBuf,
        /// Write one `.tar.gz` report per input suite into this directory.
        ///
        /// Useful when running a glob over many fixtures: each failing suite can be shared as a
        /// standalone artifact.
        #[arg(long, value_name = "DIR", conflicts_with = "report")]
        report_dir: Option<Utf8PathBuf>,
        /// When used with `--report-dir`, only write reports for suites that failed.
        #[arg(long, requires = "report_dir")]
        report_failed_only: bool,
        /// Print a normalized call trace for each test.
        #[arg(long)]
        call_trace: bool,
        /// Use recovery mode when parsing.
        #[arg(long, default_value = "false")]
        recovery_mode: bool,
    },
    /// Run gas benchmarks comparing Fe (Sonatina) against Solidity.
    Bench {
        /// Path to benchmark fixtures directory.
        #[arg(value_name = "PATH", default_value = "bench_fixtures")]
        path: Utf8PathBuf,
        /// Filter benchmarks by name.
        #[arg(short, long)]
        filter: Option<String>,
        /// solc binary to use (overrides FE_SOLC_PATH).
        #[arg(long)]
        solc: Option<String>,
        /// Output directory for CSV reports.
        #[arg(long, short)]
        output: Option<Utf8PathBuf>,
    },
    /// Create a new ingot or workspace.
    New {
        /// Path to create the ingot or workspace in.
        path: Utf8PathBuf,
        /// Create a workspace instead of a single ingot.
        #[arg(long)]
        workspace: bool,
        /// Override the default inferred name.
        #[arg(long)]
        name: Option<String>,
        /// Override the default version (default: 0.1.0).
        #[arg(long)]
        version: Option<String>,
    },
    /// Generate shell completion scripts.
    Completion {
        /// Shell to generate completions for
        #[arg(value_name = "shell")]
        shell: clap_complete::Shell,
    },
    /// Generate LSIF index for code navigation.
    Lsif {
        /// Path to the ingot directory.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Output file (defaults to stdout).
        #[arg(short, long)]
        output: Option<Utf8PathBuf>,
    },
    /// Find the workspace or ingot root for a given path.
    ///
    /// Walks up from the given path (or cwd) looking for fe.toml files.
    /// Prints the workspace root if found, otherwise the nearest ingot root.
    /// Useful for editor integrations that need to determine the project root.
    Root {
        /// Path to start searching from (default: current directory).
        path: Option<Utf8PathBuf>,
    },
    /// Start the Fe language server (LSP).
    #[cfg(feature = "lsp")]
    Lsp {
        /// Set the workspace root directory.
        ///
        /// Used as the server's working directory. When the LSP client doesn't
        /// send workspace folders, this directory is used as the fallback root
        /// for ingot/workspace discovery.
        #[arg(long)]
        root: Option<Utf8PathBuf>,
        /// Port for the combined doc+LSP server (default: auto-pick).
        #[arg(long)]
        port: Option<u16>,
        /// Communication mode (default: stdio).
        #[command(subcommand)]
        mode: Option<LspMode>,
    },
    /// Generate SCIP index for code navigation.
    Scip {
        /// Path to the ingot directory.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Output file (defaults to index.scip).
        #[arg(short, long, default_value = "index.scip")]
        output: Utf8PathBuf,
    },
}

#[derive(Debug, Clone, Subcommand)]
pub enum DocAction {
    /// Generate a documentation site (separate files by default).
    ///
    /// Default output: docs.json, index.html, fe-web.js, fe-highlight.css.
    /// Use --self-contained for a single index.html with everything inlined.
    Static {
        /// Output a single self-contained index.html instead of separate files
        #[arg(long)]
        self_contained: bool,
    },
    /// Produce docs.json (DocIndex + SCIP data) for web components.
    ///
    /// The JSON can be consumed by <fe-code-block src="docs.json">,
    /// <fe-doc-item src="docs.json">, and <fe-doc-viewer src="docs.json">.
    Json {
        /// Merge into an existing docs.json (deduplicates items, symbols, and files)
        #[arg(long)]
        merge: Option<Utf8PathBuf>,
    },
    /// Write the fe-web.js component bundle and fe-highlight.css.
    ///
    /// Does not require compiling a project — just outputs the reusable assets.
    Bundle {
        /// Also write fe-highlight.css alongside the bundle
        #[arg(long)]
        with_css: bool,
    },
    /// Generate Starlight-compatible markdown pages
    Pages {
        /// Base URL prefix for generated links
        #[arg(long, default_value = "/api")]
        base_url: String,
    },
    /// Start a live documentation server with hot reload
    Serve {
        /// Port for HTTP server
        #[arg(long, default_value = "8080")]
        port: u16,
    },
}

#[cfg(feature = "lsp")]
#[derive(Debug, Clone, Subcommand)]
pub enum LspMode {
    /// Show discovered live LSP endpoint status.
    Status,
    /// Diagnose stale or malformed LSP endpoint discovery files.
    Doctor,
    /// Ask the discovered LSP process to stop.
    Stop,
    /// Print effective shared tooling config and exit.
    Config {
        /// Print the fully merged config as JSON.
        #[arg(long)]
        effective: bool,
    },
    /// Start with TCP transport instead of stdio.
    Tcp {
        /// Port to listen on.
        #[arg(short, long, default_value_t = 4242)]
        port: u16,
        /// Timeout in seconds to shut down if no clients are connected.
        #[arg(short, long, default_value_t = 10)]
        timeout: u64,
    },
}

fn default_project_path() -> Utf8PathBuf {
    Utf8PathBuf::from(".")
}

fn unix_time_ms() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_millis())
        .unwrap_or_default()
}

fn lsp_auth_token(pid: u32) -> String {
    let mut bytes = [0u8; 32];
    if fs::File::open("/dev/urandom")
        .and_then(|mut file| file.read_exact(&mut bytes))
        .is_ok()
    {
        return hex::encode(bytes);
    }
    format!("{pid:x}{:x}", unix_time_ms())
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevCommand {
    /// Fixture-backed trace UX prototype; not compiler-derived.
    TraceFixture {
        #[command(subcommand)]
        command: TraceFixtureCommand,
    },
    /// Experimental debug export wrappers over validated trace bundles.
    Debug {
        #[command(subcommand)]
        command: DevDebugCommand,
    },
    /// Experimental trace queries and exports over validated trace JSONL.
    Trace {
        #[command(subcommand)]
        command: DevTraceCommand,
    },
}

#[derive(Debug, Clone, Subcommand)]
pub enum ShapeCommand {
    /// Emit graph and node hashes for an EVM bytecode shape.
    Emit(ShapeEmitArgs),
    /// Explain what dimensions mean for an EVM bytecode shape.
    Explain(ShapeExplainArgs),
    /// Compare two EVM bytecode shapes by dimension.
    Diff(ShapeDiffArgs),
    /// Bucket EVM bytecode variants by a selected shape dimension.
    Bucket(ShapeBucketArgs),
    /// Compare two loop-region bytecode shape candidates by dimension.
    LoopDiff(ShapeDiffArgs),
    /// Bucket loop-region bytecode shape candidates by a selected dimension.
    LoopBucket(ShapeBucketArgs),
}

#[derive(Debug, Clone, Args)]
pub struct ShapePolicyArgs {
    /// Shape level recorded in the hash policy.
    #[arg(long, default_value = "bytecode")]
    pub level: String,
    /// Bind hashes to OriginExportKeys or compare anonymous content shape.
    #[arg(long, value_enum, default_value = "identity-bound")]
    pub view_mode: ShapeViewModeArg,
    /// Dimensions to hash.
    #[arg(
        long,
        value_enum,
        value_delimiter = ',',
        default_value = "structure,names,constants,types,trace-events"
    )]
    pub dimensions: Vec<ShapeDimensionArg>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ShapeViewModeArg {
    IdentityBound,
    AnonymousShape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ShapeDimensionArg {
    Structure,
    Names,
    Constants,
    Types,
    TraceEvents,
}

#[derive(Debug, Clone, Args)]
pub struct ShapeBytecodeArgs {
    /// Owner part for generated bytecode OriginExportKeys.
    #[arg(long, default_value = "contract:shape-demo")]
    pub owner: String,
    /// Function local key for the generated bytecode function OriginExportKey.
    #[arg(long, default_value = "function:runtime")]
    pub function: String,
    /// EVM bytecode as hex, with or without 0x.
    #[arg(long = "bytecode-hex")]
    pub bytecode_hex: String,
    #[command(flatten)]
    pub policy: ShapePolicyArgs,
}

#[derive(Debug, Clone, Args)]
pub struct ShapeEmitArgs {
    #[command(flatten)]
    pub input: ShapeBytecodeArgs,
}

#[derive(Debug, Clone, Args)]
pub struct ShapeExplainArgs {
    #[command(flatten)]
    pub input: ShapeBytecodeArgs,
    /// Explain a specific bytecode PC node instead of the graph root.
    #[arg(long)]
    pub pc: Option<u32>,
}

#[derive(Debug, Clone, Args)]
pub struct ShapeDiffArgs {
    /// Owner part for generated bytecode OriginExportKeys.
    #[arg(long, default_value = "contract:shape-demo")]
    pub owner: String,
    /// Function local key for the generated bytecode function OriginExportKey.
    #[arg(long, default_value = "function:runtime")]
    pub function: String,
    /// Left EVM bytecode as hex, with or without 0x.
    #[arg(long = "left-bytecode-hex")]
    pub left_bytecode_hex: String,
    /// Right EVM bytecode as hex, with or without 0x.
    #[arg(long = "right-bytecode-hex")]
    pub right_bytecode_hex: String,
    #[command(flatten)]
    pub policy: ShapePolicyArgs,
}

#[derive(Debug, Clone, Args)]
pub struct ShapeBucketArgs {
    /// Owner part for generated bytecode OriginExportKeys.
    #[arg(long, default_value = "contract:shape-demo")]
    pub owner: String,
    /// Function local key for the generated bytecode function OriginExportKey.
    #[arg(long, default_value = "function:runtime")]
    pub function: String,
    /// EVM bytecode variant as hex. Repeat to compare variants.
    #[arg(long = "bytecode-hex", required = true)]
    pub bytecode_hex: Vec<String>,
    /// Dimension to bucket by.
    #[arg(long, value_enum, default_value = "structure")]
    pub dimension: ShapeDimensionArg,
    #[command(flatten)]
    pub policy: ShapePolicyArgs,
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevTraceCommand {
    /// Show trace fact/report availability and confidence boundaries.
    Status,
    /// Emit compiler-derived trace JSONL for a Fe target.
    Emit(DevTraceEmitArgs),
    /// Execute a compiled trace target in revm and append runtime facts.
    Run(DevTraceRunArgs),
    /// Validate a trace JSONL bundle before running reports.
    Validate(DevTraceInputArgs),
    /// Write a standalone browser demo for origin tracing through source, Sonatina, and bytecode.
    WebDemo(DevTraceWebDemoArgs),
    /// Run a report query against a validated trace snapshot.
    Query {
        #[command(subcommand)]
        command: DevTraceQueryCommand,
    },
    /// Export engine-agnostic Datalog base relations from a validated trace snapshot.
    ExportDatalog(DevTraceDatalogExportArgs),
    /// Experimental Datalog rulepack wrappers over validated trace snapshots.
    Datalog {
        #[command(subcommand)]
        command: DevTraceDatalogCommand,
    },
    /// Query a live LSP introspection endpoint discovered from .fe-lsp.json.
    Live {
        #[command(subcommand)]
        command: DevTraceLiveCommand,
    },
    /// Summarize static per-iteration loop cost from a validated trace JSONL bundle.
    LoopCost(DevTraceInputArgs),
    /// Show compiler-derived loop block and instruction membership.
    LoopContents(DevTraceInputArgs),
    /// Explain one local from a validated trace JSONL bundle.
    ExplainLocal(DevTraceExplainLocalArgs),
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevTraceQueryCommand {
    /// Summarize static per-iteration loop cost from a validated trace snapshot.
    LoopCost(DevTraceInputArgs),
    /// Show compiler-derived loop block and instruction membership.
    LoopContents(DevTraceInputArgs),
    /// Explain one local from a validated trace snapshot.
    ExplainLocal(DevTraceExplainLocalArgs),
    /// Summarize conservative static gas from opcode facts in a trace snapshot.
    GasBreakdown(DevTraceGasArgs),
    /// Explain the instruction and source attribution at a bytecode PC.
    ExplainPc(DevTracePcArgs),
    /// Summarize conservative static gas by source attribution.
    GasBySource(DevTraceGasArgs),
    /// Summarize emitted bytecode size by source attribution.
    BytecodeSizeBySource(DevTraceAttributionArgs),
    /// Summarize measured runtime gas by source attribution.
    DynamicGasBySource(DevTraceDynamicGasArgs),
    /// Combine static opcode gas and measured runtime gas by source attribution.
    GasToSource(DevTraceGasToSourceArgs),
    /// Report ambiguous, synthetic, and unmapped optimized-code attribution.
    OptimizedCodeHonesty(DevTraceInputArgs),
    /// Show variable locations active at a bytecode PC.
    VariablesAtPc(DevTracePcArgs),
    /// Summarize observed runtime step gas by source attribution.
    RuntimeGasBySource(DevTraceRuntimeAttributionArgs),
    /// Summarize observed runtime storage writes by source attribution.
    StorageWritesBySource(DevTraceRuntimeAttributionArgs),
    /// Group observed runtime storage accesses by storage slot.
    StorageAccessesBySlot(DevTraceStorageSlotArgs),
    /// Summarize observed runtime call-frame cost by callsite.
    CallCostByCallsite(DevTraceRuntimeArgs),
    /// Summarize observed runtime memory access extents by source attribution.
    MemoryGrowthBySource(DevTraceRuntimeAttributionArgs),
    /// Attribute observed runtime reverts back to source where possible.
    RevertAttribution(DevTraceRuntimeArgs),
    /// Summarize hot runtime PCs, scoped to loop membership when present.
    HotPathByIteration(DevTraceRuntimeAttributionArgs),
    /// Show runtime stack/storage/memory evidence at a bytecode PC.
    ValueFlowAtPc(DevTraceRuntimePcArgs),
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevTraceDatalogCommand {
    /// Run a built-in rulepack query or load a custom rulepack directory.
    Run(DevTraceDatalogRunArgs),
    /// Initialize a skeleton custom rulepack directory.
    InitRulepack(DevTraceDatalogInitArgs),
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevDebugCommand {
    /// Emit an experimental debug artifact from a validated trace snapshot.
    Emit(DevDebugEmitArgs),
    /// Validate an experimental debug artifact.
    Validate(DevDebugValidateArgs),
}

#[derive(Debug, Clone, Subcommand)]
pub enum DevTraceLiveCommand {
    /// Ask the live LSP endpoint for loop-cost status.
    LoopCost {
        /// Source file URI or path to query.
        #[arg(long)]
        uri: Option<String>,
        /// Output format.
        #[arg(long, value_enum, default_value = "text")]
        format: TraceReportFormat,
    },
    /// Ask the live LSP endpoint to explain a local.
    ExplainLocal {
        /// Source local to explain.
        #[arg(long)]
        local: String,
        /// Source file URI or path to query.
        #[arg(long)]
        uri: Option<String>,
        /// Output format.
        #[arg(long, value_enum, default_value = "text")]
        format: TraceReportFormat,
    },
    /// Ask the live LSP endpoint for static gas status.
    GasBreakdown {
        /// Source file URI or path to query.
        #[arg(long)]
        uri: Option<String>,
        /// Output format.
        #[arg(long, value_enum, default_value = "text")]
        format: TraceReportFormat,
    },
}

#[derive(Debug, Clone, Subcommand)]
pub enum TraceFixtureCommand {
    /// Emit fixture-backed Fibonacci facts as trace JSONL.
    Emit(TraceFixtureEmitArgs),
    /// Summarize static per-iteration loop cost using hard-coded Fibonacci fixture facts.
    LoopCost(TraceFixtureLoopCostArgs),
    /// Explain one local using hard-coded Fibonacci fixture facts.
    ExplainLocal(TraceFixtureExplainLocalArgs),
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceEmitArgs {
    /// Path to an ingot/workspace directory, a workspace member name, or a .fe file.
    #[arg(default_value_t = default_project_path())]
    pub path: Utf8PathBuf,
    /// Output trace JSONL bundle path.
    #[arg(long)]
    pub out: Utf8PathBuf,
    /// Treat a `.fe` file target as standalone, even if it is inside an ingot.
    #[arg(long)]
    pub standalone: bool,
    /// Compilation profile to use when resolving profile-aware config.
    #[arg(long, default_value = "dev", value_name = "PROFILE")]
    pub profile: String,
    /// Optimization level for emitted bytecode facts.
    #[arg(long = "optimize", short = 'O', default_value = "1", value_parser = ["0", "1", "2", "s"])]
    pub optimize: String,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceRunArgs {
    /// Static compiler trace JSONL bundle to extend with runtime facts.
    #[arg(long = "static", value_name = "TRACE_JSONL")]
    pub static_trace: Utf8PathBuf,
    /// Source file to compile for execution. Defaults to the static trace input path.
    #[arg(long)]
    pub source: Option<Utf8PathBuf>,
    /// Entry label in the form Contract or Contract::Message. Used only to select the contract.
    #[arg(long)]
    pub entry: String,
    /// Explicit 4-byte message selector as hex. This command does not infer ABI selectors.
    #[arg(long)]
    pub selector: String,
    /// ABI word arguments as decimal integers or 0x-prefixed hex. Repeat or comma-separate.
    #[arg(long = "args", value_delimiter = ',')]
    pub args: Vec<String>,
    /// Runtime capture detail level.
    #[arg(long = "runtime-capture", value_enum, default_value = "standard")]
    pub runtime_capture: RuntimeCaptureModeArg,
    /// Runtime value capture policy.
    #[arg(long = "value-policy", value_enum, default_value = "hash-only")]
    pub value_policy: RuntimeValuePolicyArg,
    /// Output combined trace JSONL bundle path.
    #[arg(long)]
    pub out: Utf8PathBuf,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceInputArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceWebDemoArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL", conflicts_with = "source")]
    pub from: Option<Utf8PathBuf>,
    /// Fe source file to compile with a long-lived salsa database.
    #[arg(long, value_name = "FE_FILE", conflicts_with = "from")]
    pub source: Option<Utf8PathBuf>,
    /// Output standalone HTML path.
    #[arg(long)]
    pub out: Utf8PathBuf,
    /// Serve the demo and live-reload when the source or JSONL input changes.
    #[arg(long)]
    pub serve: bool,
    /// HTTP port for --serve.
    #[arg(long, default_value_t = 5179)]
    pub port: u16,
    /// Treat --source as a standalone Fe file even if it lives under an ingot.
    #[arg(long)]
    pub standalone: bool,
    /// Compilation profile for --source.
    #[arg(long, default_value = "debug")]
    pub profile: String,
    /// Optimization level for --source: 0, 1, 2, or s.
    #[arg(long, default_value = "2")]
    pub optimize: String,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceDatalogExportArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Output directory for relation files and manifest.
    #[arg(long)]
    pub out: Utf8PathBuf,
    /// Relation row file format.
    #[arg(long, value_enum, default_value = "csv")]
    pub format: DatalogExportFormat,
    /// Export only typed base relations generated from TraceFact rows.
    #[arg(long)]
    pub base_only: bool,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceDatalogRunArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Built-in rulepack id or custom rulepack directory.
    #[arg(long)]
    pub rulepack: String,
    /// Rulepack query name.
    #[arg(long)]
    pub query: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceDatalogInitArgs {
    /// Directory to create for the rulepack skeleton.
    pub path: Utf8PathBuf,
}

#[derive(Debug, Clone, Args)]
pub struct DevDebugEmitArgs {
    /// Debug export format.
    #[arg(long, value_enum)]
    pub format: DebugExportFormat,
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Output artifact path.
    #[arg(long)]
    pub out: Utf8PathBuf,
    /// DWARF sections to emit, or ethdebug phase list.
    #[arg(long)]
    pub sections: Option<String>,
    /// Compilation unit display name for DWARF metadata.
    #[arg(long)]
    pub unit_name: Option<String>,
    /// Source language label for debug metadata.
    #[arg(long, default_value = "fe")]
    pub language: String,
    /// Minimum source confidence label for debug rows.
    #[arg(long, default_value = "compiler-emitted")]
    pub confidence: String,
    /// ethdebug schema version; use `pinned` for the vendored schema.
    #[arg(long, default_value = "pinned")]
    pub schema_version: String,
    /// ethdebug phase list.
    #[arg(long)]
    pub phase: Option<String>,
    /// Optional Fe origin/confidence sidecar output path for ethdebug.
    #[arg(long)]
    pub sidecar: Option<Utf8PathBuf>,
}

#[derive(Debug, Clone, Args)]
pub struct DevDebugValidateArgs {
    /// Debug export format.
    #[arg(long, value_enum)]
    pub format: DebugExportFormat,
    /// Input artifact path.
    #[arg(long)]
    pub input: Utf8PathBuf,
    /// ethdebug schema version; use `pinned` for the vendored schema.
    #[arg(long, default_value = "pinned")]
    pub schema_version: String,
    /// Optional Fe origin/confidence sidecar to validate alongside ethdebug.
    #[arg(long)]
    pub sidecar: Option<Utf8PathBuf>,
    /// Optional validation JSON output path.
    #[arg(long)]
    pub verify_json: Option<Utf8PathBuf>,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceExplainLocalArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Source local to explain.
    #[arg(long)]
    pub local: String,
    /// Exact origin key display label for ambiguous locals.
    #[arg(long)]
    pub local_key: Option<String>,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceGasArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Named EVM gas schedule.
    #[arg(long, default_value = "cancun")]
    pub schedule: String,
    /// Source-attribution policy.
    #[arg(long, default_value = "exclusive-primary")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceAttributionArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Source-attribution policy.
    #[arg(long, default_value = "exclusive-primary")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceDynamicGasArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Optional dynamic execution trace id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Source-attribution policy.
    #[arg(long, default_value = "exclusive-primary")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceGasToSourceArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Named EVM gas schedule for static opcode gas.
    #[arg(long, default_value = "cancun")]
    pub schedule: String,
    /// Optional dynamic execution trace id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Source-attribution policy.
    #[arg(long, default_value = "exclusive-primary")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceRuntimeArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Optional runtime execution trace/session id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceRuntimeAttributionArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Optional runtime execution trace/session id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Source-attribution policy.
    #[arg(long, default_value = "runtime-step-exclusive")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceStorageSlotArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Optional runtime execution trace/session id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Optional rendered storage slot label to filter.
    #[arg(long)]
    pub slot: Option<String>,
    /// Source-attribution policy.
    #[arg(long, default_value = "runtime-step-exclusive")]
    pub policy: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTraceRuntimePcArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Bytecode PC to inspect.
    #[arg(long)]
    pub pc: u32,
    /// Optional runtime execution trace/session id to filter.
    #[arg(long)]
    pub trace_id: Option<String>,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct DevTracePcArgs {
    /// Trace JSONL bundle to read.
    #[arg(long = "from", value_name = "TRACE_JSONL")]
    pub from: Utf8PathBuf,
    /// Bytecode PC to inspect.
    #[arg(long)]
    pub pc: u32,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct TraceFixtureEmitArgs {
    /// Path to fib_demo.fe.
    #[arg(default_value_t = default_project_path())]
    pub path: Utf8PathBuf,
    /// Output trace JSONL bundle path.
    #[arg(long)]
    pub out: Utf8PathBuf,
    /// Function label to record in trace metadata.
    #[arg(long, default_value = "Fib.recv Compute handler")]
    pub function: String,
}

#[derive(Debug, Clone, Args)]
pub struct TraceFixtureLoopCostArgs {
    /// Path to fib_demo.fe.
    #[arg(default_value_t = default_project_path())]
    pub path: Utf8PathBuf,
    /// Function label to display in the report.
    #[arg(long, default_value = "Fib.recv Compute handler")]
    pub function: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

#[derive(Debug, Clone, Args)]
pub struct TraceFixtureExplainLocalArgs {
    /// Path to fib_demo.fe.
    #[arg(default_value_t = default_project_path())]
    pub path: Utf8PathBuf,
    /// Source local to explain.
    #[arg(long)]
    pub local: String,
    /// Function label to display in the report.
    #[arg(long, default_value = "Fib.recv Compute handler")]
    pub function: String,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TraceReportFormat,
}

fn main() {
    let opts = Options::parse();
    run(&opts);
}
pub fn run(opts: &Options) {
    let preference = match opts.color {
        ColorChoice::Auto => common::color::ColorPreference::Auto,
        ColorChoice::Always => common::color::ColorPreference::Always,
        ColorChoice::Never => common::color::ColorPreference::Never,
    };
    common::color::set_color_preference(preference);
    match preference {
        common::color::ColorPreference::Auto => colored::control::unset_override(),
        common::color::ColorPreference::Always => colored::control::set_override(true),
        common::color::ColorPreference::Never => colored::control::set_override(false),
    }

    match &opts.command {
        Command::Build {
            path,
            ingot,
            standalone,
            contract,
            optimize,
            out_dir,
            profile,
            emit,
            report,
            report_out,
            report_failed_only,
            recovery_mode,
        } => {
            let opt_level = match effective_opt_level(optimize.as_deref()) {
                Ok(level) => level,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            build(
                path,
                ingot.as_deref(),
                *standalone,
                contract.as_deref(),
                opt_level,
                emit,
                out_dir.as_ref(),
                profile,
                (*report).then_some(report_out),
                *report_failed_only,
                *recovery_mode,
            )
        }
        Command::Check {
            path,
            ingot,
            standalone,
            profile,
            dump_mir,
            report,
            report_out,
            report_failed_only,
            recovery_mode,
        } => {
            match check(
                path,
                ingot.as_deref(),
                *standalone,
                profile,
                *dump_mir,
                (*report).then_some(report_out),
                *report_failed_only,
                *recovery_mode,
            ) {
                Ok(has_errors) => {
                    if has_errors {
                        std::process::exit(1);
                    }
                }
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            }
        }
        Command::Analyze {
            path,
            ingot,
            standalone,
            profile,
            format,
            tests,
            origin_facts,
            fact_relation_tables,
            recovery_mode,
        } => match analyze::analyze(
            path,
            ingot.as_deref(),
            *standalone,
            profile,
            *format,
            *tests,
            *origin_facts,
            *fact_relation_tables,
            *recovery_mode,
        ) {
            Ok(has_errors) => {
                if has_errors {
                    std::process::exit(1);
                }
            }
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        },
        Command::Dev { command } => match trace::run_dev_command(command) {
            Ok(output) => print!("{output}"),
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        },
        Command::Shape { command } => match shape_cli::run_shape_command(command) {
            Ok(output) => print!("{output}"),
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        },
        Command::Doc {
            path,
            output,
            builtins,
            stdlib_path,
            action,
        } => {
            if let Some(DocAction::Bundle { with_css }) = action {
                let output_dir = output.clone().unwrap_or_else(|| Utf8PathBuf::from("."));
                doc::write_bundle(&output_dir.join("fe-web.js"));
                if *with_css {
                    doc::write_highlight_css(&output_dir.join("fe-highlight.css"));
                    doc::write_styles_css(&output_dir.join("styles.css"));
                }
            } else {
                doc::generate_docs(
                    path,
                    output.as_ref(),
                    *builtins,
                    stdlib_path.as_ref(),
                    action.as_ref(),
                );
            }
        }
        #[cfg(not(target_arch = "wasm32"))]
        Command::Tree { path } => {
            if tree::print_tree(path) {
                std::process::exit(1);
            }
        }
        Command::Fmt { path, check } => {
            run_fmt(path.as_ref(), *check);
        }
        Command::Test {
            paths,
            ingot,
            filter,
            jobs,
            grouped,
            show_logs,
            emit,
            profile,
            optimize,
            trace_evm,
            trace_evm_keep,
            trace_evm_stack_n,
            debug_dir,
            report,
            report_out,
            report_dir,
            report_failed_only,
            call_trace,
            recovery_mode,
        } => {
            let opt_level = match effective_opt_level(optimize.as_deref()) {
                Ok(level) => level,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            let debug = TestDebugOptions {
                trace_evm: *trace_evm,
                trace_evm_keep: *trace_evm_keep,
                trace_evm_stack_n: *trace_evm_stack_n,
                debug_dir: debug_dir.clone(),
            };
            let paths = if paths.is_empty() {
                vec![default_project_path()]
            } else {
                paths.clone()
            };
            match test::run_tests(
                &paths,
                ingot.as_deref(),
                filter.as_deref(),
                *jobs,
                *grouped,
                *show_logs,
                profile,
                opt_level,
                emit,
                &debug,
                (*report).then_some(report_out),
                report_dir.as_ref(),
                *report_failed_only,
                *call_trace,
                *recovery_mode,
            ) {
                Ok(has_failures) => {
                    if has_failures {
                        std::process::exit(1);
                    }
                }
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            }
        }
        Command::Bench {
            path,
            filter,
            solc,
            output,
        } => match fe::bench::run_benchmarks(
            path.as_path(),
            filter.as_deref(),
            solc.as_deref(),
            output.as_ref().map(|p| p.as_path()),
        ) {
            Ok(()) => {}
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        },
        Command::New {
            path,
            workspace,
            name,
            version,
        } => {
            if let Err(err) = cli::new::run(path, *workspace, name.as_deref(), version.as_deref()) {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        }
        Command::Completion { shell } => {
            clap_complete::generate(
                *shell,
                &mut Options::command(),
                "fe",
                &mut std::io::stdout(),
            );
        }
        Command::Root { path } => {
            run_root(path.as_ref());
        }
        #[cfg(feature = "lsp")]
        Command::Lsp { root, port, mode } => {
            // If --root is explicit, use it. Otherwise, auto-discover from cwd.
            let resolved_root = match root {
                Some(r) => Some(r.canonicalize_utf8().unwrap_or_else(|e| {
                    eprintln!("Error: invalid --root path {r}: {e}");
                    std::process::exit(1);
                })),
                None => driver::files::find_project_root(),
            };
            if let Some(root) = &resolved_root {
                std::env::set_current_dir(root.as_std_path()).unwrap_or_else(|e| {
                    eprintln!("Error: cannot chdir to {root}: {e}");
                    std::process::exit(1);
                });
            }

            let rt = tokio::runtime::Runtime::new().unwrap_or_else(|e| {
                eprintln!("Error creating async runtime: {e}");
                std::process::exit(1);
            });
            rt.block_on(async {
                unsafe {
                    std::env::set_var("RUST_BACKTRACE", "full");
                }
                language_server::setup_panic_hook();
                match mode {
                    Some(LspMode::Status) => match lsp_status(resolved_root.as_ref()) {
                        Ok(output) => println!("{output}"),
                        Err(err) => {
                            eprintln!("Error: {err}");
                            std::process::exit(1);
                        }
                    },
                    Some(LspMode::Doctor) => match lsp_doctor(resolved_root.as_ref()) {
                        Ok(output) => println!("{output}"),
                        Err(err) => {
                            eprintln!("Error: {err}");
                            std::process::exit(1);
                        }
                    },
                    Some(LspMode::Stop) => match lsp_stop(resolved_root.as_ref()) {
                        Ok(output) => println!("{output}"),
                        Err(err) => {
                            eprintln!("Error: {err}");
                            std::process::exit(1);
                        }
                    },
                    Some(LspMode::Config { effective: _ }) => {
                        let root = resolved_root
                            .as_ref()
                            .map(|root| root.as_std_path())
                            .unwrap_or_else(|| std::path::Path::new("."));
                        let config =
                            introspection_config::FeToolingConfig::load_from_workspace(root)
                                .unwrap_or_else(|err| {
                                    eprintln!("Error: {err}");
                                    std::process::exit(1);
                                });
                        let output = serde_json::json!({
                            "config_hash": config.stable_hash(),
                            "config": config,
                        });
                        println!("{}", serde_json::to_string_pretty(&output).unwrap());
                    }
                    Some(LspMode::Tcp { port, timeout }) => {
                        language_server::run_tcp_server(
                            *port,
                            std::time::Duration::from_secs(*timeout),
                        )
                        .await;
                    }
                    None => {
                        run_lsp_with_combined_server(resolved_root, *port).await;
                    }
                }
            });
        }
        Command::Lsif { path, output } => {
            run_lsif(path, output.as_ref());
        }
        Command::Scip { path, output } => {
            run_scip(path, output);
        }
    }
}

#[cfg(feature = "lsp")]
async fn run_lsp_with_combined_server(resolved_root: Option<Utf8PathBuf>, port: Option<u16>) {
    use tokio::net::TcpListener;

    // Bind the combined server listener
    let addr = format!("127.0.0.1:{}", port.unwrap_or(0));
    let listener = match TcpListener::bind(&addr).await {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Warning: could not bind combined server: {e}");
            language_server::run_stdio_server(None).await;
            return;
        }
    };
    let actual_port = listener.local_addr().unwrap().port();

    // Generate doc HTML for the workspace (best-effort)
    let doc_html = generate_lsp_doc_html(resolved_root.as_ref());

    eprintln!("Documentation: http://127.0.0.1:{actual_port}");

    // Write .fe-lsp.json for discovery
    let workspace_root_path = resolved_root
        .as_ref()
        .map(|r| r.as_std_path().to_path_buf())
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    let tooling_config =
        match introspection_config::FeToolingConfig::load_from_workspace(&workspace_root_path) {
            Ok(config) => config,
            Err(err) => {
                eprintln!("Warning: could not load tooling config: {err}");
                introspection_config::FeToolingConfig::default()
            }
        };
    eprintln!("Tooling config hash: {}", tooling_config.stable_hash());
    let config_hash = tooling_config.stable_hash();
    let capabilities = vec![
        "trace.query".to_string(),
        "ir.view".to_string(),
        "gas.static".to_string(),
        "graph.origin".to_string(),
    ];

    // Inspect any existing .fe-lsp.json. This is purely diagnostic: we
    // always proceed with writing our own, since the file is a discovery
    // pointer, not a lock. The outcome is still important for triage:
    // stale files mean a previous crash, sibling-live means Zed respawned
    // us without shutting the old instance down, and a root-mismatch
    // means we're fighting with another instance over workspace root
    // detection.
    //
    // These diagnostics are emitted via `eprintln!` rather than `tracing`
    // because the tracing subscriber isn't installed until later, inside
    // `language_server::run_stdio_server` -> `setup_default_subscriber`.
    // Zed captures stderr for its LSP log panel, so users still see the
    // messages in the same place they'd see a `tracing::warn!`.
    let workspace_root_display = workspace_root_path.display();
    let our_pid = std::process::id();
    match doc::ExistingInstanceCheck::inspect(&workspace_root_path) {
        doc::ExistingInstanceCheck::None => {
            // Nothing to report in the common case; keep startup quiet.
        }
        doc::ExistingInstanceCheck::StaleFound {
            stale_pid,
            recorded_workspace_root,
        } => {
            eprintln!(
                "fe-language-server: removing stale .fe-lsp.json at {workspace_root_display} \
                 (previous pid {stale_pid} not alive; recorded workspace_root={recorded_workspace_root:?}). \
                 Previous instance likely crashed without cleanup."
            );
            doc::LspServerInfo::remove_from_workspace(&workspace_root_path);
        }
        doc::ExistingInstanceCheck::SiblingLive {
            sibling_pid,
            sibling_docs_url,
        } => {
            eprintln!(
                "fe-language-server: another fe lsp instance (pid {sibling_pid}) is already \
                 running for workspace {workspace_root_display} (sibling_docs_url={sibling_docs_url:?}); \
                 overwriting .fe-lsp.json with our info (pid {our_pid}). Zed may have respawned \
                 us before the previous instance finished shutting down."
            );
        }
        doc::ExistingInstanceCheck::RootMismatch {
            other_pid,
            other_workspace_root,
            our_workspace_root,
        } => {
            eprintln!(
                "fe-language-server: ROOT MISMATCH: existing .fe-lsp.json (pid {other_pid}) refers \
                 to workspace_root={other_workspace_root:?}, but we detected {our_workspace_root}. \
                 This usually means either a workspace-root detection bug or a pid-reuse false \
                 positive in is_alive(). To triage, check what the two processes think their \
                 workspace_folders are in the initialize params. Our pid is {our_pid}."
            );
        }
        doc::ExistingInstanceCheck::Malformed => {
            eprintln!(
                "fe-language-server: removing malformed .fe-lsp.json at {workspace_root_display} \
                 (parse failed; probably a leftover from an older fe version)."
            );
            doc::LspServerInfo::remove_from_workspace(&workspace_root_path);
        }
    }

    if tooling_config.lsp.live.write_server_info {
        let token_file = ".fe-lsp.token";
        let token = lsp_auth_token(our_pid);
        if let Err(err) = fs::write(workspace_root_path.join(token_file), token) {
            eprintln!("fe-language-server: could not write {token_file}: {err}");
        }
        let docs_url = format!("http://127.0.0.1:{actual_port}");
        let server_info = doc::LspServerInfo {
            schema_version: 1,
            pid: our_pid,
            started_at_ms: Some(unix_time_ms()),
            port: Some(actual_port),
            workspace_root: Some(workspace_root_path.display().to_string()),
            docs_url: Some(docs_url.clone()),
            lsp: Some(doc::LspEndpointInfo {
                transport: "websocket".to_string(),
                port: Some(actual_port),
                ws_url: Some(format!("ws://127.0.0.1:{actual_port}/lsp")),
            }),
            http: Some(doc::HttpEndpointInfo {
                base_url: docs_url.clone(),
                docs_url,
                trace_api_url: format!("http://127.0.0.1:{actual_port}/trace"),
            }),
            capabilities: capabilities.clone(),
            config_hash: Some(config_hash.clone()),
            auth: Some(doc::LspAuthInfo {
                mode: "localhost-token".to_string(),
                token_file: token_file.to_string(),
            }),
        };
        if let Err(e) = server_info.write_to_workspace(&workspace_root_path) {
            eprintln!(
                "fe-language-server: could not write .fe-lsp.json at {workspace_root_display}: {e}"
            );
        }
    }

    let config = language_server::CombinedServerConfig {
        listener,
        doc_html,
        docs_url: Some(format!("http://127.0.0.1:{actual_port}")),
        tooling_config,
        config_hash,
        workspace_root: Some(workspace_root_path.display().to_string()),
        capabilities,
    };

    language_server::run_stdio_server(Some(config)).await;

    // Cleanup on exit
    doc::LspServerInfo::remove_from_workspace(&workspace_root_path);
}

#[cfg(feature = "lsp")]
fn lsp_status(resolved_root: Option<&Utf8PathBuf>) -> Result<String, String> {
    let root = lsp_discovery_root(resolved_root)?;
    let info = doc::LspServerInfo::read_from_workspace(root.as_std_path())
        .ok_or_else(|| format!("no .fe-lsp.json found at {root}"))?;
    let check = doc::ExistingInstanceCheck::inspect(root.as_std_path());
    let mut out = String::new();
    out.push_str("Fe LSP status\n\n");
    out.push_str(&format!("status: {}\n", lsp_check_status(&check)));
    out.push_str(&format!("schema_version: {}\n", info.schema_version));
    if info.schema_version != 1 {
        out.push_str("schema_status: unsupported; restart fe lsp with the current binary\n");
    }
    out.push_str(&format!("pid: {}\n", info.pid));
    out.push_str(&format!("alive: {}\n", info.is_alive()));
    out.push_str(&format!(
        "workspace_root: {}\n",
        info.workspace_root.as_deref().unwrap_or("<unknown>")
    ));
    if let Some(hash) = &info.config_hash {
        out.push_str(&format!("config_hash: {hash}\n"));
    }
    if let Some(status) = lsp_config_hash_status(&root, &info) {
        out.push_str(&format!("{status}\n"));
    }
    if let Some(http) = &info.http {
        out.push_str(&format!("http: {}\n", http.base_url));
        if matches!(check, doc::ExistingInstanceCheck::SiblingLive { .. }) {
            match http_get_text(&format!("{}/health", http.base_url.trim_end_matches('/'))) {
                Ok(body) => out.push_str(&format!("health: {body}\n")),
                Err(err) => out.push_str(&format!("health: unavailable ({err})\n")),
            }
        } else {
            out.push_str("health: skipped because server info is not a live matching instance\n");
        }
    } else if let Some(docs_url) = &info.docs_url {
        out.push_str(&format!("docs_url: {docs_url}\n"));
    }
    Ok(out)
}

#[cfg(feature = "lsp")]
fn lsp_doctor(resolved_root: Option<&Utf8PathBuf>) -> Result<String, String> {
    let root = lsp_discovery_root(resolved_root)?;
    let check = doc::ExistingInstanceCheck::inspect(root.as_std_path());
    let mut out = String::new();
    out.push_str("Fe LSP doctor\n\n");
    out.push_str(&format!("workspace_root: {root}\n"));
    if let Some(info) = doc::LspServerInfo::read_from_workspace(root.as_std_path()) {
        out.push_str(&format!("schema_version: {}\n", info.schema_version));
        if info.schema_version != 1 {
            out.push_str("schema_status: unsupported; restart fe lsp with the current binary\n");
        }
        if let Some(status) = lsp_config_hash_status(&root, &info) {
            out.push_str(&format!("{status}\n"));
        }
    }
    match check {
        doc::ExistingInstanceCheck::None => out.push_str("status: no server info file found\n"),
        doc::ExistingInstanceCheck::StaleFound {
            stale_pid,
            recorded_workspace_root,
        } => out.push_str(&format!(
            "status: stale server info (pid {stale_pid}, recorded workspace_root={recorded_workspace_root:?})\n"
        )),
        doc::ExistingInstanceCheck::SiblingLive {
            sibling_pid,
            sibling_docs_url,
        } => out.push_str(&format!(
            "status: live server discovered (pid {sibling_pid}, docs_url={sibling_docs_url:?})\n"
        )),
        doc::ExistingInstanceCheck::RootMismatch {
            other_pid,
            other_workspace_root,
            our_workspace_root,
        } => out.push_str(&format!(
            "status: root mismatch (pid {other_pid}, recorded={other_workspace_root:?}, detected={our_workspace_root})\n"
        )),
        doc::ExistingInstanceCheck::Malformed => {
            out.push_str("status: malformed .fe-lsp.json\n")
        }
    }
    Ok(out)
}

#[cfg(feature = "lsp")]
fn lsp_check_status(check: &doc::ExistingInstanceCheck) -> String {
    match check {
        doc::ExistingInstanceCheck::None => "no server info file found".to_string(),
        doc::ExistingInstanceCheck::StaleFound {
            stale_pid,
            recorded_workspace_root,
        } => format!(
            "stale server info (pid {stale_pid}, recorded workspace_root={recorded_workspace_root:?})"
        ),
        doc::ExistingInstanceCheck::SiblingLive {
            sibling_pid,
            sibling_docs_url,
        } => format!("live matching server (pid {sibling_pid}, docs_url={sibling_docs_url:?})"),
        doc::ExistingInstanceCheck::RootMismatch {
            other_pid,
            other_workspace_root,
            our_workspace_root,
        } => format!(
            "root mismatch (pid {other_pid}, recorded={other_workspace_root:?}, detected={our_workspace_root})"
        ),
        doc::ExistingInstanceCheck::Malformed => "malformed .fe-lsp.json".to_string(),
    }
}

#[cfg(feature = "lsp")]
fn lsp_config_hash_status(root: &Utf8PathBuf, info: &doc::LspServerInfo) -> Option<String> {
    let current = introspection_config::FeToolingConfig::load_from_workspace(root.as_std_path())
        .ok()?
        .stable_hash();
    Some(match info.config_hash.as_deref() {
        Some(recorded) if recorded == current => "config_hash_status: match".to_string(),
        Some(recorded) => {
            format!("config_hash_status: mismatch (server={recorded}, current={current})")
        }
        None => format!("config_hash_status: missing (current={current})"),
    })
}

#[cfg(feature = "lsp")]
fn lsp_stop(resolved_root: Option<&Utf8PathBuf>) -> Result<String, String> {
    let root = lsp_discovery_root(resolved_root)?;
    let info = doc::LspServerInfo::read_from_workspace(root.as_std_path())
        .ok_or_else(|| format!("no .fe-lsp.json found at {root}"))?;
    if !info.is_alive() {
        doc::LspServerInfo::remove_from_workspace(root.as_std_path());
        return Ok(format!(
            "removed stale .fe-lsp.json for non-live pid {}\n",
            info.pid
        ));
    }
    terminate_process(info.pid)?;
    Ok(format!("sent stop signal to Fe LSP pid {}\n", info.pid))
}

#[cfg(feature = "lsp")]
fn lsp_discovery_root(resolved_root: Option<&Utf8PathBuf>) -> Result<Utf8PathBuf, String> {
    if let Some(root) = resolved_root {
        return Ok(root.clone());
    }
    Utf8PathBuf::from_path_buf(std::env::current_dir().map_err(|err| err.to_string())?)
        .map_err(|path| format!("current directory is not UTF-8: {}", path.display()))
}

#[cfg(feature = "lsp")]
fn terminate_process(pid: u32) -> Result<(), String> {
    #[cfg(unix)]
    {
        let status = std::process::Command::new("kill")
            .arg(pid.to_string())
            .status()
            .map_err(|err| format!("failed to run kill: {err}"))?;
        if status.success() {
            Ok(())
        } else {
            Err(format!("kill exited with status {status}"))
        }
    }
    #[cfg(windows)]
    {
        let status = std::process::Command::new("taskkill")
            .args(["/PID", &pid.to_string()])
            .status()
            .map_err(|err| format!("failed to run taskkill: {err}"))?;
        if status.success() {
            Ok(())
        } else {
            Err(format!("taskkill exited with status {status}"))
        }
    }
}

fn http_get_text(url: &str) -> Result<String, String> {
    http_request("GET", url, None)
}

pub(crate) fn http_post_json(url: &str, body: &serde_json::Value) -> Result<String, String> {
    http_request("POST", url, Some(body.to_string()))
}

fn http_request(method: &str, url: &str, body: Option<String>) -> Result<String, String> {
    use std::io::Write;
    use std::net::TcpStream;

    let url = url::Url::parse(url).map_err(|err| format!("invalid URL {url}: {err}"))?;
    if url.scheme() != "http" {
        return Err(format!("unsupported URL scheme: {}", url.scheme()));
    }
    let host = url
        .host_str()
        .ok_or_else(|| format!("URL has no host: {url}"))?;
    let port = url.port_or_known_default().unwrap_or(80);
    let path = if let Some(query) = url.query() {
        format!("{}?{query}", url.path())
    } else {
        url.path().to_string()
    };
    let mut stream = TcpStream::connect((host, port))
        .map_err(|err| format!("failed to connect to {host}:{port}: {err}"))?;
    let body = body.unwrap_or_default();
    let request = format!(
        "{method} {path} HTTP/1.1\r\nHost: {host}:{port}\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
        body.len()
    );
    stream
        .write_all(request.as_bytes())
        .map_err(|err| format!("failed to write HTTP request: {err}"))?;
    let mut response = String::new();
    stream
        .read_to_string(&mut response)
        .map_err(|err| format!("failed to read HTTP response: {err}"))?;
    let Some((headers, body)) = response.split_once("\r\n\r\n") else {
        return Err("malformed HTTP response".to_string());
    };
    if !headers.starts_with("HTTP/1.1 200") && !headers.starts_with("HTTP/1.0 200") {
        return Err(headers.lines().next().unwrap_or(headers).to_string());
    }
    Ok(body.to_string())
}

/// Initial doc data generation from a workspace root.
///
/// Discovers ingots via `discover_and_init` (requires `&mut`), then delegates
/// Generate doc + SCIP data for a workspace. Used at LSP startup.
/// Regenerate doc + SCIP data via salsa-tracked functions.
#[cfg(feature = "lsp")]
fn regenerate_doc_data(
    db: &mut driver::DriverDataBase,
    workspace_root: &camino::Utf8Path,
) -> (String, Option<String>) {
    let root_path = workspace_root
        .canonicalize_utf8()
        .unwrap_or_else(|_| workspace_root.to_owned());

    if let Ok(root_url) = url::Url::from_directory_path(&root_path) {
        let _discovered = driver::discover_and_init(db, &root_url);
        semantic_indexing::doc::regenerate(db)
    } else {
        let json = serde_json::to_string(&fe_web::model::DocIndex::new()).unwrap();
        (json, None)
    }
}

/// Generate the doc HTML for the combined server.
///
/// Uses `discover_context` (same discovery the LS uses) to find all ingots
/// under the workspace root, so it works for:
/// - Single ingots (directory with fe.toml)
/// - Workspaces (fe.toml with [workspace] members)
/// - Directories containing multiple ingots without a root fe.toml
/// - Sentinel workspaces with members=[] (discovers child ingots)
#[cfg(feature = "lsp")]
fn generate_lsp_doc_html(resolved_root: Option<&Utf8PathBuf>) -> String {
    let root_path = resolved_root
        .cloned()
        .unwrap_or_else(|| Utf8PathBuf::from("."));

    let mut db = driver::DriverDataBase::default();
    let (json, scip_json) = regenerate_doc_data(&mut db, &root_path);

    // Parse back the index to get the title
    let index: fe_web::model::DocIndex =
        serde_json::from_str(&json).unwrap_or_else(|_| fe_web::model::DocIndex::new());
    let title = if let Some(root) = index.modules.first() {
        format!("{} — Fe Documentation", root.name)
    } else {
        "Fe Documentation".to_string()
    };
    let mut html = fe_web::assets::html_shell_full(&title, &json, scip_json.as_deref(), None);

    // Append auto-connect script
    let connect_script =
        r#"<script>window.FE_LSP = connectLsp(`${location.protocol==='https:'?'wss:':'ws:'}://${location.host}/lsp`);</script>"#.to_string();
    if let Some(pos) = html.rfind("</body>") {
        html.insert_str(pos, &connect_script);
    }

    html
}

fn effective_opt_level(optimize: Option<&str>) -> Result<codegen::OptLevel, String> {
    optimize.unwrap_or("1").parse()
}

fn run_lsif(path: &Utf8PathBuf, output: Option<&Utf8PathBuf>) {
    use driver::DriverDataBase;

    let mut db = DriverDataBase::default();

    let canonical_path = match path.canonicalize_utf8() {
        Ok(p) => p,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {path}");
            std::process::exit(1);
        }
    };

    let ingot_url = match url::Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {path}");
            std::process::exit(1);
        }
    };

    let had_init_diagnostics = driver::init_ingot(&mut db, &ingot_url);
    if had_init_diagnostics {
        eprintln!("Warning: ingot had initialization diagnostics");
    }

    let result = if let Some(output_path) = output {
        let file = match std::fs::File::create(output_path.as_std_path()) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Error creating output file: {e}");
                std::process::exit(1);
            }
        };
        let writer = std::io::BufWriter::new(file);
        semantic_indexing::lsif::generate_lsif(&db, &ingot_url, writer)
    } else {
        let stdout = std::io::stdout().lock();
        let writer = std::io::BufWriter::new(stdout);
        semantic_indexing::lsif::generate_lsif(&db, &ingot_url, writer)
    };

    if let Err(e) = result {
        eprintln!("Error generating LSIF: {e}");
        std::process::exit(1);
    }
}

fn run_scip(path: &Utf8PathBuf, output: &Utf8PathBuf) {
    use driver::DriverDataBase;

    let mut db = DriverDataBase::default();

    let canonical_path = match path.canonicalize_utf8() {
        Ok(p) => p,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {path}");
            std::process::exit(1);
        }
    };

    let ingot_url = match url::Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {path}");
            std::process::exit(1);
        }
    };

    let had_init_diagnostics = driver::init_ingot(&mut db, &ingot_url);
    if had_init_diagnostics {
        eprintln!("Warning: ingot had initialization diagnostics");
    }

    let result =
        semantic_indexing::scip_batch::generate_scip(&db, &ingot_url).unwrap_or_else(|e| {
            eprintln!("Error generating SCIP: {e}");
            std::process::exit(1);
        });

    if let Err(e) = scip::write_message_to_file(output.as_std_path(), result.index) {
        eprintln!("Error writing SCIP file: {e}");
        std::process::exit(1);
    }
}

fn run_root(path: Option<&Utf8PathBuf>) {
    use resolver::workspace::discover_context;

    let start = match path {
        Some(p) => p.canonicalize_utf8().unwrap_or_else(|e| {
            eprintln!("Error: invalid path {p}: {e}");
            std::process::exit(1);
        }),
        None => Utf8PathBuf::from_path_buf(
            std::env::current_dir().expect("Unable to get current directory"),
        )
        .expect("Expected utf8 path"),
    };

    let start_url = url::Url::from_directory_path(start.as_str()).unwrap_or_else(|_| {
        // Maybe it's a file, try the parent directory
        let parent = start.parent().unwrap_or(&start);
        url::Url::from_directory_path(parent.as_str()).unwrap_or_else(|_| {
            eprintln!("Error: invalid directory path: {start}");
            std::process::exit(1);
        })
    });

    match discover_context(&start_url, false) {
        Ok(discovery) => {
            if let Some(workspace_root) = &discovery.workspace_root
                && let Ok(path) = workspace_root.to_file_path()
            {
                println!("{}", path.display());
                return;
            }
            if let Some(ingot_root) = discovery.ingot_roots.first()
                && let Ok(path) = ingot_root.to_file_path()
            {
                println!("{}", path.display());
                return;
            }
            eprintln!("No fe.toml found in {start} or any parent directory");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Error discovering project root: {e}");
            std::process::exit(1);
        }
    }
}

fn run_fmt(path: Option<&Utf8PathBuf>, check: bool) {
    let config = fe_fmt::Config::default();

    // Collect files to format
    let files: Vec<Utf8PathBuf> = match path {
        Some(p) if p.is_file() => vec![p.clone()],
        Some(p) if p.is_dir() => collect_fe_files(p),
        Some(p) => {
            eprintln!("Error: Path does not exist: {p}");
            std::process::exit(1);
        }
        None => {
            // Find project root and format all .fe files in src/
            match driver::files::find_project_root() {
                Some(root) => collect_fe_files(&root.join("src")),
                None => {
                    eprintln!(
                        "Error: No fe.toml found. Run from a Fe project directory or specify a path."
                    );
                    std::process::exit(1);
                }
            }
        }
    };

    if files.is_empty() {
        eprintln!("Error: No .fe files found");
        std::process::exit(1);
    }

    let mut unformatted_files = Vec::new();
    let mut error_count = 0;

    for file in &files {
        match format_single_file(file, &config, check) {
            FormatResult::Unchanged => {}
            FormatResult::Formatted {
                original,
                formatted,
            } => {
                if check {
                    print_diff(file, &original, &formatted);
                    unformatted_files.push(file.clone());
                }
            }
            FormatResult::ParseError(errs) => {
                eprintln!("Warning: Skipping {file} (parse errors):");
                for err in errs {
                    eprintln!("  {}", err.msg());
                }
            }
            FormatResult::IoError(err) => {
                eprintln!("Error: Failed to process {file}: {err}");
                error_count += 1;
            }
        }
    }

    if check && !unformatted_files.is_empty() {
        std::process::exit(1);
    }

    if error_count > 0 {
        std::process::exit(1);
    }
}

fn print_diff(path: &Utf8PathBuf, original: &str, formatted: &str) {
    let diff = TextDiff::from_lines(original, formatted);

    println!("{}", format!("Diff {}:", path).bold());
    for hunk in diff.unified_diff().context_radius(3).iter_hunks() {
        // Print hunk header
        println!("{}", format!("{}", hunk.header()).cyan());
        for change in hunk.iter_changes() {
            match change.tag() {
                ChangeTag::Delete => print!("{}", format!("-{}", change).red()),
                ChangeTag::Insert => print!("{}", format!("+{}", change).green()),
                ChangeTag::Equal => print!(" {}", change),
            };
        }
    }
    println!();
}

fn collect_fe_files(dir: &Utf8PathBuf) -> Vec<Utf8PathBuf> {
    if !dir.exists() {
        return Vec::new();
    }

    WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "fe"))
        .filter_map(|e| Utf8PathBuf::from_path_buf(e.into_path()).ok())
        .collect()
}

enum FormatResult {
    Unchanged,
    Formatted { original: String, formatted: String },
    ParseError(Vec<fe_fmt::ParseError>),
    IoError(std::io::Error),
}

fn format_single_file(path: &Utf8PathBuf, config: &fe_fmt::Config, check: bool) -> FormatResult {
    let original = match fs::read_to_string(path.as_std_path()) {
        Ok(s) => s,
        Err(e) => return FormatResult::IoError(e),
    };

    let formatted = match fe_fmt::format_str(&original, config) {
        Ok(f) => f,
        Err(fe_fmt::FormatError::ParseErrors(errs)) => return FormatResult::ParseError(errs),
        Err(fe_fmt::FormatError::Io(e)) => return FormatResult::IoError(e),
    };

    if formatted == original {
        return FormatResult::Unchanged;
    }

    if !check {
        if let Err(e) = fs::write(path.as_std_path(), &formatted) {
            return FormatResult::IoError(e);
        }
        println!("Formatted {}", path);
    }

    FormatResult::Formatted {
        original,
        formatted,
    }
}
