use serde::Serialize;
use fe_trace_facts::TraceFactSpec;

#[derive(Serialize, TraceFactSpec)]
#[trace_fact(type = "bad", relation = "base_bad")]
struct SkipConflict {
    #[trace_skip]
    #[trace_col]
    name: String,
}

fn main() {}
