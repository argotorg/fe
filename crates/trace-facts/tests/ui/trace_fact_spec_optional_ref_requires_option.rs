use fe_trace_facts::{OriginExportKey, TraceFactSpec};
use serde::Serialize;

#[derive(Serialize, TraceFactSpec)]
#[trace_fact(type = "bad", relation = "base_bad")]
struct BadOptionalRef {
    #[trace_ref(optional)]
    key: OriginExportKey,
}

fn main() {}
