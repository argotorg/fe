#![allow(clippy::print_stderr, clippy::print_stdout)]
mod build;
mod check;
#[cfg(not(target_arch = "wasm32"))]
mod tree;

use build::build;
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
    Build {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        #[arg(long)]
        dump_mir: bool,
    },
    Check {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        #[arg(short, long)]
        core: Option<Utf8PathBuf>,
        #[arg(long, help = "Show detailed errors from downstream ingots")]
        show_downstream: bool,
    },
    #[cfg(not(target_arch = "wasm32"))]
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
        Command::Build { path, dump_mir } => {
            build(path, *dump_mir);
        }
        Command::Check {
            path,
            core: _,
            show_downstream,
        } => {
            //: TODO readd custom core
            check(path, *show_downstream);
        }
        #[cfg(not(target_arch = "wasm32"))]
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}
