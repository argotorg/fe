#![allow(clippy::print_stderr, clippy::print_stdout)]
mod check;
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
    Tree {
        path: Utf8PathBuf,
    },
    New,
}

fn default_project_path() -> Utf8PathBuf {
    // With search mode enabled in init_ingot, we can just use the current directory
    // The FilesResolver will walk up to find the ingot root
    Utf8PathBuf::from(".")
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
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}
