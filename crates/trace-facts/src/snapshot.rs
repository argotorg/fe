use std::fmt;
use std::io::BufRead;

use crate::{
    JsonlTraceReadError, TraceBundle, TraceFact, TraceMetadata, TraceValidationError,
    TraceValidationReport, TraceValidator, read_trace_bundle_jsonl,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraceSnapshot {
    metadata: TraceMetadata,
    facts: Vec<TraceFact>,
    validation: TraceValidationReport,
    trace_hash: String,
}

impl TraceSnapshot {
    pub fn new(bundle: TraceBundle) -> Result<Self, TraceValidationError> {
        let validation = TraceValidator::check(&bundle.facts);
        if let Some(error) = validation.first_error() {
            return Err(error.clone());
        }
        let trace_hash = snapshot_hash(&bundle);
        Ok(Self {
            metadata: bundle.metadata,
            facts: bundle.facts,
            validation,
            trace_hash,
        })
    }

    pub fn read_jsonl(reader: impl BufRead) -> Result<Self, TraceSnapshotReadError> {
        let bundle = read_trace_bundle_jsonl(reader)?;
        Self::new(bundle).map_err(TraceSnapshotReadError::Validation)
    }

    pub fn metadata(&self) -> &TraceMetadata {
        &self.metadata
    }

    pub fn facts(&self) -> &[TraceFact] {
        &self.facts
    }

    pub fn validation(&self) -> &TraceValidationReport {
        &self.validation
    }

    pub fn trace_hash(&self) -> &str {
        &self.trace_hash
    }

    pub fn into_bundle(self) -> TraceBundle {
        TraceBundle::new(self.metadata, self.facts)
    }
}

#[derive(Debug)]
pub enum TraceSnapshotReadError {
    Jsonl(JsonlTraceReadError),
    Validation(TraceValidationError),
}

impl fmt::Display for TraceSnapshotReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Jsonl(err) => write!(f, "{err}"),
            Self::Validation(err) => write!(f, "trace snapshot validation failed: {err}"),
        }
    }
}

impl std::error::Error for TraceSnapshotReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Jsonl(err) => Some(err),
            Self::Validation(err) => Some(err),
        }
    }
}

impl From<JsonlTraceReadError> for TraceSnapshotReadError {
    fn from(value: JsonlTraceReadError) -> Self {
        Self::Jsonl(value)
    }
}

fn snapshot_hash(bundle: &TraceBundle) -> String {
    let json = serde_json::to_vec(bundle).expect("trace bundle should serialize");
    format!("fnv64:{:016x}", fnv1a64(&json))
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use common::origin::OriginExportKey;

    use crate::{
        InstructionFact, JsonlTraceSink, OriginNodeFact, OriginNodeKind, TraceBundle,
        TraceDataSource, TraceFact, TraceMetadata, TraceSnapshot,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn snapshot_loads_valid_jsonl_with_metadata_and_hash() {
        let function = key("function", "demo", "main");
        let instruction = key("bytecode.inst", "demo", "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "demo.fe",
                vec!["profile=dev".to_string()],
            ),
            vec![
                TraceFact::OriginNode(OriginNodeFact::new(
                    function.clone(),
                    OriginNodeKind::new("function"),
                )),
                TraceFact::OriginNode(OriginNodeFact::new(
                    instruction.clone(),
                    OriginNodeKind::new("bytecode.inst"),
                )),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();

        let snapshot = TraceSnapshot::read_jsonl(Cursor::new(sink.into_inner())).unwrap();

        assert_eq!(
            snapshot.metadata().data_source,
            TraceDataSource::CompilerEmitted
        );
        assert_eq!(snapshot.validation().summary.instruction_count, 1);
        assert!(snapshot.trace_hash().starts_with("fnv64:"));
    }

    #[test]
    fn snapshot_rejects_invalid_trace_facts() {
        let instruction = key("bytecode.inst", "demo", "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![TraceFact::Instruction(InstructionFact::new(
                instruction,
                key("function", "demo", "main"),
                0,
                "STOP",
            ))],
        );

        assert!(TraceSnapshot::new(bundle).is_err());
    }
}
