mod trace_cli;
mod trace_emit;

use trace_facts::{TraceDataSource, TraceMetadata};

pub(crate) use trace_cli::run_dev_command;

fn compiler_commit() -> String {
    // The commit baked in at build time is the only honest answer: asking git
    // at runtime reports whatever the build-directory checkout currently has,
    // not the compiler that produced these facts.
    option_env!("FE_GIT_COMMIT")
        .map(str::to_string)
        .unwrap_or_else(|| "unknown".to_string())
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
