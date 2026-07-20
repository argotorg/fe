use fe_trace_facts::TraceFactSpec;
use serde::Serialize;

#[derive(Serialize, TraceFactSpec)]
#[trace_fact(type = "bad", relation = "base_bad")]
struct BadRef {
    #[trace_ref]
    key: String,
}

fn main() {}
