#![allow(clippy::print_stderr, clippy::print_stdout)]
mod check;
mod doc;
mod tree;

use camino::Utf8PathBuf;
use check::check;
use clap::{Parser, Subcommand};

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
    /// Generate documentation for a Fe project
    Doc {
        /// Path to a .fe file or ingot directory
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        /// Output path for generated docs
        #[arg(short, long)]
        output: Option<Utf8PathBuf>,
        /// Output raw JSON instead of summary
        #[arg(long)]
        json: bool,
        /// Start HTTP server to browse docs
        #[arg(long)]
        serve: bool,
        /// Port for HTTP server (default: 8080)
        #[arg(long, default_value = "8080")]
        port: u16,
    },
    Tree {
        path: Utf8PathBuf,
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
        Command::Doc {
            path,
            output,
            json,
            serve,
            port,
        } => {
            doc::generate_docs(path, output.as_ref(), *json, *serve, *port);
        }
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}
