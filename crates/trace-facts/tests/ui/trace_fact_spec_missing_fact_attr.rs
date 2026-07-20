use serde::Serialize;
use fe_trace_facts::{OriginExportKey, TraceFactSpec};

#[derive(Serialize, TraceFactSpec)]
struct MissingFactAttr {
    #[trace_key]
    key: OriginExportKey,
}

fn main() {}
