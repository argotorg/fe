#![allow(clippy::print_stderr, clippy::print_stdout)]
mod build;
mod check;
mod cli;
mod lsif;
mod report;
mod scip_index;
mod test;
#[cfg(not(target_arch = "wasm32"))]
mod tree;
mod workspace_ingot;

use std::fs;

use build::build;
use camino::Utf8PathBuf;
use check::check;
use clap::{CommandFactory, Parser, Subcommand, ValueEnum};
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
pub enum TestDebug {
    /// Print Yul output for any failing test.
    Failures,
    /// Print Yul output for all executed tests.
    All,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum BuildEmit {
    Bytecode,
    RuntimeBytecode,
    Ir,
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
        /// Code generation backend to use (yul or sonatina).
        #[arg(long, default_value = "sonatina")]
        backend: String,
        /// Optimization level (0 = none, 1 = balanced, 2 = aggressive).
        ///
        /// Defaults to `1`.
        ///
        /// Note: with `--backend yul`, opt levels `1` and `2` are currently equivalent.
        ///
        /// - Sonatina backend: controls the optimization pipeline.
        /// - Yul backend: controls whether solc optimization is enabled (0 = disabled, 1/2 = enabled).
        #[arg(long, default_value = "1", value_name = "LEVEL")]
        opt_level: String,
        /// Enable optimization.
        ///
        /// Shorthand for `--opt-level 1`.
        ///
        /// It is an error to pass `--optimize` with `--opt-level 0`.
        #[arg(long)]
        optimize: bool,
        /// solc binary to use (overrides FE_SOLC_PATH).
        ///
        /// Only used with `--backend yul` (ignored with a warning otherwise).
        #[arg(long)]
        solc: Option<String>,
        /// Output directory for artifacts.
        #[arg(long)]
        out_dir: Option<Utf8PathBuf>,
        /// Comma-delimited artifacts to emit.
        #[arg(
            long,
            short = 'e',
            value_enum,
            value_delimiter = ',',
            default_value = "bytecode,runtime-bytecode"
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
        /// Show event logs from test execution.
        #[arg(long)]
        show_logs: bool,
        /// Print Yul output (`failures` or `all`) when using the Yul backend.
        #[arg(
            long,
            value_enum,
            num_args = 0..=1,
            default_missing_value = "failures",
            require_equals = true
        )]
        debug: Option<TestDebug>,
        /// Backend to use for codegen (yul or sonatina).
        #[arg(long, default_value = "sonatina")]
        backend: String,
        /// solc binary to use (overrides FE_SOLC_PATH).
        ///
        /// Only used with `--backend yul` (ignored with a warning otherwise).
        #[arg(long)]
        solc: Option<String>,
        /// Optimization level (0 = none, 1 = balanced, 2 = aggressive).
        ///
        /// Defaults to `1`.
        ///
        /// Note: with `--backend yul`, opt levels `1` and `2` are currently equivalent.
        ///
        /// - Sonatina backend: controls the optimization pipeline.
        /// - Yul backend: controls whether solc optimization is enabled (0 = disabled, 1/2 = enabled).
        #[arg(long, default_value = "1", value_name = "LEVEL")]
        opt_level: String,
        /// Enable optimization.
        ///
        /// Shorthand for `--opt-level 1`.
        ///
        /// It is an error to pass `--optimize` with `--opt-level 0`.
        #[arg(long)]
        optimize: bool,
        /// Trace executed EVM opcodes while running tests.
        #[arg(long)]
        trace_evm: bool,
        /// How many EVM steps to keep in the trace ring buffer.
        #[arg(long, default_value_t = 200)]
        trace_evm_keep: usize,
        /// How many stack items to print per EVM step in traces.
        #[arg(long, default_value_t = 16)]
        trace_evm_stack_n: usize,
        /// Dump the Sonatina runtime symbol table (function offsets/sizes).
        #[arg(long)]
        sonatina_symtab: bool,
        /// Directory to write debug outputs (traces, symtabs) into.
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
        /// Print a normalized call trace for each test (for backend comparison).
        #[arg(long)]
        call_trace: bool,
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

#[cfg(feature = "lsp")]
#[derive(Debug, Clone, Subcommand)]
pub enum LspMode {
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
            backend,
            opt_level,
            optimize,
            solc,
            out_dir,
            emit,
            report,
            report_out,
            report_failed_only,
        } => {
            let backend_kind: codegen::BackendKind = match backend.parse() {
                Ok(kind) => kind,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            let opt_level = match effective_opt_level(backend_kind, opt_level, *optimize) {
                Ok(level) => level,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            if backend_kind != codegen::BackendKind::Yul && solc.is_some() {
                eprintln!("Warning: --solc is only used with --backend yul; ignoring --solc");
            }
            build(
                path,
                ingot.as_deref(),
                *standalone,
                contract.as_deref(),
                backend_kind,
                opt_level,
                emit,
                out_dir.as_ref(),
                solc.as_deref(),
                (*report).then_some(report_out),
                *report_failed_only,
            )
        }
        Command::Check {
            path,
            ingot,
            standalone,
            dump_mir,
            report,
            report_out,
            report_failed_only,
        } => {
            match check(
                path,
                ingot.as_deref(),
                *standalone,
                *dump_mir,
                (*report).then_some(report_out),
                *report_failed_only,
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
            show_logs,
            debug: test_debug,
            backend,
            solc,
            opt_level,
            optimize,
            trace_evm,
            trace_evm_keep,
            trace_evm_stack_n,
            sonatina_symtab,
            debug_dir,
            report,
            report_out,
            report_dir,
            report_failed_only,
            call_trace,
        } => {
            let backend_kind: codegen::BackendKind = match backend.parse() {
                Ok(kind) => kind,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            let opt_level = match effective_opt_level(backend_kind, opt_level, *optimize) {
                Ok(level) => level,
                Err(err) => {
                    eprintln!("Error: {err}");
                    std::process::exit(1);
                }
            };
            if backend_kind != codegen::BackendKind::Yul && solc.is_some() {
                eprintln!("Warning: --solc is only used with --backend yul; ignoring --solc");
            }
            let debug = TestDebugOptions {
                trace_evm: *trace_evm,
                trace_evm_keep: *trace_evm_keep,
                trace_evm_stack_n: *trace_evm_stack_n,
                sonatina_symtab: *sonatina_symtab,
                sonatina_evm_debug: false,
                sonatina_observability: false,
                dump_yul_on_failure: matches!(test_debug, Some(TestDebug::Failures)),
                dump_yul_for_all: matches!(test_debug, Some(TestDebug::All)),
                debug_dir: debug_dir.clone(),
            };
            let paths = if paths.is_empty() {
                vec![default_project_path()]
            } else {
                paths.clone()
            };
            let solc = if backend_kind == codegen::BackendKind::Yul {
                solc.as_deref()
            } else {
                None
            };
            let yul_optimize =
                backend_kind == codegen::BackendKind::Yul && opt_level.yul_optimize();
            match test::run_tests(
                &paths,
                ingot.as_deref(),
                filter.as_deref(),
                *jobs,
                *show_logs,
                backend,
                yul_optimize,
                solc,
                opt_level,
                &debug,
                (*report).then_some(report_out),
                report_dir.as_ref(),
                *report_failed_only,
                *call_trace,
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
        Command::Lsp { root, mode } => {
            // If --root is explicit, use it. Otherwise, auto-discover from cwd.
            let resolved_root = match root {
                Some(r) => Some(r.canonicalize_utf8().unwrap_or_else(|e| {
                    eprintln!("Error: invalid --root path {r}: {e}");
                    std::process::exit(1);
                })),
                None => driver::files::find_project_root(),
            };
            if let Some(root) = resolved_root {
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
                    Some(LspMode::Tcp { port, timeout }) => {
                        language_server::run_tcp_server(
                            *port,
                            std::time::Duration::from_secs(*timeout),
                        )
                        .await;
                    }
                    None => {
                        language_server::run_stdio_server().await;
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

fn effective_opt_level(
    backend_kind: codegen::BackendKind,
    opt_level: &str,
    optimize: bool,
) -> Result<codegen::OptLevel, String> {
    let level: codegen::OptLevel = opt_level.parse()?;

    if optimize && level == codegen::OptLevel::O0 {
        return Err(
            "--optimize is shorthand for `--opt-level 1` and cannot be used with `--opt-level 0`"
                .to_string(),
        );
    }

    if backend_kind == codegen::BackendKind::Yul && level == codegen::OptLevel::O2 {
        eprintln!("Warning: --opt-level 2 has no additional effect for --backend yul (same as 1)");
    }

    Ok(level)
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
        lsif::generate_lsif(&mut db, &ingot_url, writer)
    } else {
        let stdout = std::io::stdout().lock();
        let writer = std::io::BufWriter::new(stdout);
        lsif::generate_lsif(&mut db, &ingot_url, writer)
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

    let index = match scip_index::generate_scip(&mut db, &ingot_url) {
        Ok(index) => index,
        Err(e) => {
            eprintln!("Error generating SCIP: {e}");
            std::process::exit(1);
        }
    };

    if let Err(e) = scip::write_message_to_file(output.as_std_path(), index) {
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

    match discover_context(&start_url) {
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
