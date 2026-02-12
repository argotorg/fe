#![allow(clippy::print_stderr, clippy::print_stdout)]
mod build;
mod check;
mod cli;
mod test;
#[cfg(not(target_arch = "wasm32"))]
mod tree;

use std::fs;

use build::build;
use camino::Utf8PathBuf;
use check::check;
use clap::{CommandFactory, Parser, Subcommand};
use colored::Colorize;
use fmt as fe_fmt;
use similar::{ChangeTag, TextDiff};
use walkdir::WalkDir;

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    /// Show verbose resolver output.
    #[arg(short, long, global = true)]
    pub verbose: bool,
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    /// Compile Fe code to EVM bytecode using solc.
    Build {
        /// Path to an ingot/workspace directory (containing fe.toml), a workspace member name, or a .fe file.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Build a specific contract by name (defaults to all contracts in the target).
        #[arg(long)]
        contract: Option<String>,
        /// Enable solc optimizer.
        #[arg(long)]
        optimize: bool,
        /// solc binary to use (overrides FE_SOLC_PATH).
        #[arg(long)]
        solc: Option<String>,
        /// Output directory for artifacts.
        #[arg(long)]
        out_dir: Option<Utf8PathBuf>,
    },
    Check {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        #[arg(short, long)]
        core: Option<Utf8PathBuf>,
        #[arg(long)]
        dump_mir: bool,
        #[arg(long)]
        emit_yul_min: bool,
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
        /// Path to a .fe file or directory containing an ingot with tests.
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Optional filter pattern for test names.
        #[arg(short, long)]
        filter: Option<String>,
        /// Show event logs from test execution.
        #[arg(long)]
        show_logs: bool,
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
}

fn default_project_path() -> Utf8PathBuf {
    Utf8PathBuf::from(".")
}

fn main() {
    let opts = Options::parse();
    run(&opts);
}
pub fn run(opts: &Options) {
    driver::set_resolver_verbose(opts.verbose);
    match &opts.command {
        Command::Build {
            path,
            contract,
            optimize,
            solc,
            out_dir,
        } => build(
            path,
            contract.as_deref(),
            *optimize,
            out_dir.as_ref(),
            solc.as_deref(),
        ),
        Command::Check {
            path,
            core: _,
            dump_mir,
            emit_yul_min,
        } => {
            //: TODO readd custom core
            check(path, *dump_mir, *emit_yul_min);
        }
        #[cfg(not(target_arch = "wasm32"))]
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::Fmt { path, check } => {
            run_fmt(path.as_ref(), *check);
        }
        Command::Test {
            path,
            filter,
            show_logs,
        } => {
            test::run_tests(path, filter.as_deref(), *show_logs);
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
    }
}

fn run_fmt(path: Option<&Utf8PathBuf>, check: bool) {
    let config = fe_fmt::Config::default();

    // Collect files to format
    let files: Vec<Utf8PathBuf> = match path {
        Some(p) if p.is_file() => vec![p.clone()],
        Some(p) if p.is_dir() => collect_fe_files(p),
        Some(p) => {
            eprintln!("Path does not exist: {}", p);
            std::process::exit(1);
        }
        None => {
            // Find project root and format all .fe files in src/
            match driver::files::find_project_root() {
                Some(root) => collect_fe_files(&root.join("src")),
                None => {
                    eprintln!(
                        "No fe.toml found. Run from a Fe project directory or specify a path."
                    );
                    std::process::exit(1);
                }
            }
        }
    };

    if files.is_empty() {
        eprintln!("No .fe files found");
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
                eprintln!("Skipping {} (parse errors):", file);
                for err in errs {
                    eprintln!("  {}", err.msg());
                }
            }
            FormatResult::IoError(err) => {
                eprintln!("Error processing {}: {}", file, err);
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
