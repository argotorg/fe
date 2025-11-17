#![allow(clippy::print_stderr, clippy::print_stdout)]
mod check;
mod tree;

use std::fs;

use camino::Utf8PathBuf;
use check::check;
use clap::{Parser, Subcommand};
use fmt as fe_fmt;

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Build,
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
    Tree {
        path: Utf8PathBuf,
    },
    /// Format Fe source code.
    Fmt {
        /// Path to a Fe source file to format.
        path: Utf8PathBuf,
        /// Check if the file is formatted, but do not write changes.
        #[arg(long)]
        check: bool,
        /// Write the formatted code back to the file.
        #[arg(long)]
        write: bool,
    },
    New,
}

fn default_project_path() -> Utf8PathBuf {
    driver::files::find_project_root().unwrap_or(Utf8PathBuf::from("."))
}

fn main() {
    let opts = Options::parse();
    run(&opts);
}
pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check {
            path,
            core: _,
            dump_mir,
            emit_yul_min,
        } => {
            //: TODO readd custom core
            check(path, *dump_mir, *emit_yul_min);
        }
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::Fmt { path, check, write } => {
            if *check && *write {
                eprintln!("`fe fmt` does not support using --check and --write together");
                std::process::exit(1);
            }

            let config = fe_fmt::Config::default();
            let formatted = match fe_fmt::format_file(path.as_std_path(), &config) {
                Ok(formatted) => formatted,
                Err(err) => {
                    eprintln!("Failed to format {}: {err:?}", path);
                    std::process::exit(1);
                }
            };

            let original = fs::read_to_string(path.as_std_path()).unwrap_or_else(|err| {
                eprintln!("Failed to read {}: {err}", path);
                std::process::exit(1);
            });

            if *check {
                if formatted == original {
                    return;
                }
                eprintln!("{} is not formatted", path);
                std::process::exit(1);
            }

            if *write && formatted != original {
                if let Err(err) = fs::write(path.as_std_path(), formatted) {
                    eprintln!("Failed to write formatted output to {}: {err}", path);
                    std::process::exit(1);
                }
                return;
            }

            print!("{formatted}");
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}
