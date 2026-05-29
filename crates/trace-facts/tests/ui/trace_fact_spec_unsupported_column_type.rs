use serde::Serialize;
use fe_trace_facts::TraceFactSpec;

#[derive(Serialize, TraceFactSpec)]
#[trace_fact(type = "bad", relation = "base_bad")]
struct UnsupportedColumn {
    #[trace_col]
    value: (u8, u8),
}

fn main() {}
