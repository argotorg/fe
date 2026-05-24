mod trace_cli;
mod trace_emit;
mod trace_fixture;
mod trace_live;
mod trace_render;

use trace_facts::{TraceDataSource, TraceMetadata};

pub(crate) use trace_cli::run_dev_command;

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

fn format_data_source(metadata: &TraceMetadata) -> String {
    match metadata.data_source {
        TraceDataSource::Fixture => {
            let marker = metadata.fixture_marker.as_deref().unwrap_or("unspecified");
            format!("fixture ({marker}; not compiler-derived)")
        }
        TraceDataSource::CompilerEmitted => "compiler_emitted".to_string(),
    }
}
