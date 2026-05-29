mod trace_cli;
mod trace_datalog;
mod trace_emit;
mod trace_fixture;
mod trace_live;
mod trace_render;
mod trace_web_demo;

use std::{fs::File, io::BufReader};

use camino::Utf8PathBuf;
use trace_facts::{TraceDataSource, TraceMetadata, TraceSnapshot};

pub(crate) use trace_cli::run_dev_command;

pub(crate) fn read_trace_snapshot_jsonl_from_path(
    path: &Utf8PathBuf,
) -> Result<TraceSnapshot, String> {
    let file =
        File::open(path.as_std_path()).map_err(|err| format!("failed to open {path}: {err}"))?;
    TraceSnapshot::read_jsonl(BufReader::new(file))
        .map_err(|err| format!("failed to read validated trace JSONL {path}: {err}"))
}

fn compiler_commit() -> String {
    runtime_git_commit()
        .or_else(|| option_env!("FE_GIT_COMMIT").map(str::to_string))
        .unwrap_or_else(|| "unknown".to_string())
}

fn runtime_git_commit() -> Option<String> {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let output = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(repo_root)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let commit = String::from_utf8(output.stdout).ok()?.trim().to_string();
    (!commit.is_empty()).then_some(commit)
}

pub(crate) fn format_data_source(metadata: &TraceMetadata) -> String {
    match metadata.data_source {
        TraceDataSource::Fixture => {
            let marker = metadata.fixture_marker.as_deref().unwrap_or("unspecified");
            format!("fixture ({marker}; not compiler-derived)")
        }
        TraceDataSource::CompilerEmitted => "compiler_emitted".to_string(),
    }
}
