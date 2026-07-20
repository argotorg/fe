use std::fmt;
use std::io::BufRead;

use crate::{
    JsonlTraceReadError, TraceBundle, TraceFact, TraceMetadata, TraceValidationError,
    TraceValidationReport, TraceValidator, read_trace_bundle_jsonl,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraceSnapshot {
    data: TraceSnapshotData,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraceSnapshotData {
    metadata: TraceMetadata,
    facts: Vec<TraceFact>,
    validation: TraceValidationReport,
    trace_hash: String,
}

impl TraceSnapshot {
    #[allow(clippy::result_large_err)]
    pub fn new(bundle: TraceBundle) -> Result<Self, TraceValidationError> {
        Ok(Self {
            data: TraceSnapshotData::from_bundle(bundle)?,
        })
    }

    #[allow(clippy::result_large_err)]
    pub fn from_data(data: TraceSnapshotData) -> Result<Self, TraceValidationError> {
        if let Some(error) = data.validation.first_error() {
            return Err(error.clone());
        }
        Ok(Self { data })
    }

    #[allow(clippy::result_large_err)]
    pub fn read_jsonl(reader: impl BufRead) -> Result<Self, TraceSnapshotReadError> {
        let bundle = read_trace_bundle_jsonl(reader)?;
        Self::new(bundle).map_err(TraceSnapshotReadError::Validation)
    }

    pub fn data(&self) -> &TraceSnapshotData {
        &self.data
    }

    pub fn metadata(&self) -> &TraceMetadata {
        self.data.metadata()
    }

    pub fn facts(&self) -> &[TraceFact] {
        self.data.facts()
    }

    pub fn validation(&self) -> &TraceValidationReport {
        self.data.validation()
    }

    pub fn trace_hash(&self) -> &str {
        self.data.trace_hash()
    }

    pub fn into_data(self) -> TraceSnapshotData {
        self.data
    }

    pub fn into_bundle(self) -> TraceBundle {
        self.data.into_bundle()
    }
}

impl TraceSnapshotData {
    #[allow(clippy::result_large_err)]
    pub fn from_bundle(bundle: TraceBundle) -> Result<Self, TraceValidationError> {
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
#[allow(clippy::large_enum_variant)]
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
    format!("blake3:{}", blake3::hash(&json).to_hex())
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
        assert_content_digest(snapshot.trace_hash());
    }

    fn assert_content_digest(value: &str) {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        assert_eq!(digest.len(), 64);
        assert!(digest.chars().all(|ch| ch.is_ascii_hexdigit()));
        assert!(!value.starts_with("fnv64:"));
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

    #[test]
    fn snapshot_data_is_typed_artifact_before_jsonl_export() {
        let function = key("function", "demo", "main");
        let instruction = key("bytecode.inst", "demo", "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                TraceFact::OriginNode(OriginNodeFact::from_key(function.clone())),
                TraceFact::OriginNode(OriginNodeFact::from_key(instruction.clone())),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        );

        let snapshot = TraceSnapshot::new(bundle).unwrap();
        let data = snapshot.clone().into_data();
        let rewrapped = TraceSnapshot::from_data(data.clone()).unwrap();

        assert_eq!(data.trace_hash(), snapshot.trace_hash());
        assert_eq!(rewrapped.trace_hash(), snapshot.trace_hash());
        assert_eq!(data.validation().summary.instruction_count, 1);
    }
}
