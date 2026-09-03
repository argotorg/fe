use serde::Serialize;
use fe_trace_facts::{OriginExportKey, TraceFactSpec};

#[derive(Serialize, TraceFactSpec)]
#[trace_fact(type = "bad", relation = "base_bad")]
struct DuplicateKey {
    #[trace_key]
    first: OriginExportKey,
    #[trace_key]
    second: OriginExportKey,
}

fn main() {}
