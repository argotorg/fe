use std::fmt;
use std::io::{self, BufRead, Write};

use crate::fact::TraceFact;
use serde::{Deserialize, Serialize};

pub const TRACE_SCHEMA_VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TraceBundle {
    pub metadata: TraceMetadata,
    pub facts: Vec<TraceFact>,
}

impl TraceBundle {
    pub fn new(metadata: TraceMetadata, facts: Vec<TraceFact>) -> Self {
        Self { metadata, facts }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TraceMetadata {
    pub schema_version: u32,
    pub compiler_commit: String,
    pub target: String,
    pub command: Vec<String>,
    pub input_path: String,
    pub flags: Vec<String>,
    pub data_source: TraceDataSource,
    pub fixture_marker: Option<String>,
}

impl TraceMetadata {
    pub fn fixture(
        compiler_commit: impl Into<String>,
        target: impl Into<String>,
        command: Vec<String>,
        input_path: impl Into<String>,
        flags: Vec<String>,
        fixture_marker: impl Into<String>,
    ) -> Self {
        Self {
            schema_version: TRACE_SCHEMA_VERSION,
            compiler_commit: compiler_commit.into(),
            target: target.into(),
            command,
            input_path: input_path.into(),
            flags,
            data_source: TraceDataSource::Fixture,
            fixture_marker: Some(fixture_marker.into()),
        }
    }

    pub fn compiler_emitted(
        compiler_commit: impl Into<String>,
        target: impl Into<String>,
        command: Vec<String>,
        input_path: impl Into<String>,
        flags: Vec<String>,
    ) -> Self {
        Self {
            schema_version: TRACE_SCHEMA_VERSION,
            compiler_commit: compiler_commit.into(),
            target: target.into(),
            command,
            input_path: input_path.into(),
            flags,
            data_source: TraceDataSource::CompilerEmitted,
            fixture_marker: None,
        }
    }

    pub fn validate(&self) -> Result<(), TraceMetadataError> {
        if self.schema_version != TRACE_SCHEMA_VERSION {
            return Err(TraceMetadataError::UnsupportedSchemaVersion {
                found: self.schema_version,
                expected: TRACE_SCHEMA_VERSION,
            });
        }
        validate_metadata_text("compiler_commit", &self.compiler_commit)?;
        validate_metadata_text("target", &self.target)?;
        validate_metadata_text("input_path", &self.input_path)?;
        for (index, arg) in self.command.iter().enumerate() {
            if arg.trim().is_empty() {
                return Err(TraceMetadataError::EmptyCommandArg { index });
            }
        }
        for (index, flag) in self.flags.iter().enumerate() {
            if flag.trim().is_empty() {
                return Err(TraceMetadataError::EmptyFlag { index });
            }
        }
        match (self.data_source, self.fixture_marker.as_deref()) {
            (TraceDataSource::Fixture, Some(marker)) if marker.trim().is_empty() => {
                Err(TraceMetadataError::EmptyFixtureMarker)
            }
            (TraceDataSource::Fixture, Some(_)) => Ok(()),
            (TraceDataSource::Fixture, None) => Err(TraceMetadataError::MissingFixtureMarker),
            (TraceDataSource::CompilerEmitted, Some(_)) => {
                Err(TraceMetadataError::UnexpectedFixtureMarker)
            }
            (TraceDataSource::CompilerEmitted, None) => Ok(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceDataSource {
    Fixture,
    CompilerEmitted,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceMetadataError {
    UnsupportedSchemaVersion { found: u32, expected: u32 },
    EmptyField { field: &'static str },
    EmptyCommandArg { index: usize },
    EmptyFlag { index: usize },
    MissingFixtureMarker,
    EmptyFixtureMarker,
    UnexpectedFixtureMarker,
}

impl fmt::Display for TraceMetadataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedSchemaVersion { found, expected } => write!(
                f,
                "unsupported trace schema version {found}; expected {expected}"
            ),
            Self::EmptyField { field } => write!(f, "trace metadata field {field} is empty"),
            Self::EmptyCommandArg { index } => {
                write!(f, "trace metadata command argument {index} is empty")
            }
            Self::EmptyFlag { index } => {
                write!(f, "trace metadata flag {index} is empty")
            }
            Self::MissingFixtureMarker => {
                write!(f, "fixture trace metadata is missing fixture_marker")
            }
            Self::EmptyFixtureMarker => {
                write!(f, "fixture trace metadata has an empty fixture_marker")
            }
            Self::UnexpectedFixtureMarker => write!(
                f,
                "compiler-emitted trace metadata must not include fixture_marker"
            ),
        }
    }
}

impl std::error::Error for TraceMetadataError {}

fn validate_metadata_text(field: &'static str, value: &str) -> Result<(), TraceMetadataError> {
    if value.trim().is_empty() {
        Err(TraceMetadataError::EmptyField { field })
    } else {
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum TraceJsonlRecord {
    Metadata(TraceMetadata),
    Fact(TraceFact),
}

pub struct JsonlTraceSink<W> {
    writer: W,
}

impl<W> JsonlTraceSink<W>
where
    W: Write,
{
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    pub fn write_bundle(&mut self, bundle: &TraceBundle) -> io::Result<()> {
        self.write_metadata(&bundle.metadata)?;
        for fact in &bundle.facts {
            self.write_fact(fact)?;
        }
        Ok(())
    }

    pub fn write_metadata(&mut self, metadata: &TraceMetadata) -> io::Result<()> {
        self.write_record(&TraceJsonlRecord::Metadata(metadata.clone()))
    }

    pub fn write_fact(&mut self, fact: &TraceFact) -> io::Result<()> {
        self.write_record(&TraceJsonlRecord::Fact(fact.clone()))
    }

    pub fn write_record(&mut self, record: &TraceJsonlRecord) -> io::Result<()> {
        serde_json::to_writer(&mut self.writer, record).map_err(io::Error::other)?;
        self.writer.write_all(b"\n")
    }

    pub fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }

    pub fn into_inner(self) -> W {
        self.writer
    }
}

#[derive(Debug)]
pub enum JsonlTraceReadError {
    Io(io::Error),
    Json {
        line: usize,
        source: serde_json::Error,
    },
    InvalidMetadata(TraceMetadataError),
    MissingMetadata,
    DuplicateMetadata,
}

impl fmt::Display for JsonlTraceReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => write!(f, "failed to read trace JSONL: {err}"),
            Self::Json { line, source } => {
                write!(f, "failed to parse trace JSONL line {line}: {source}")
            }
            Self::InvalidMetadata(err) => write!(f, "invalid trace metadata: {err}"),
            Self::MissingMetadata => write!(f, "trace JSONL is missing metadata record"),
            Self::DuplicateMetadata => write!(f, "trace JSONL contains multiple metadata records"),
        }
    }
}

impl std::error::Error for JsonlTraceReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(err) => Some(err),
            Self::Json { source, .. } => Some(source),
            Self::InvalidMetadata(err) => Some(err),
            Self::MissingMetadata | Self::DuplicateMetadata => None,
        }
    }
}

impl From<io::Error> for JsonlTraceReadError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

pub struct JsonlTraceReader<R> {
    reader: R,
}

impl<R> JsonlTraceReader<R>
where
    R: BufRead,
{
    pub fn new(reader: R) -> Self {
        Self { reader }
    }

    pub fn read_bundle(self) -> Result<TraceBundle, JsonlTraceReadError> {
        let mut metadata = None;
        let mut facts = Vec::new();
        for record in self.read_records()? {
            match record {
                TraceJsonlRecord::Metadata(next) => {
                    if metadata.replace(next).is_some() {
                        return Err(JsonlTraceReadError::DuplicateMetadata);
                    }
                }
                TraceJsonlRecord::Fact(fact) => facts.push(fact),
            }
        }
        let metadata = metadata.ok_or(JsonlTraceReadError::MissingMetadata)?;
        metadata
            .validate()
            .map_err(JsonlTraceReadError::InvalidMetadata)?;
        Ok(TraceBundle::new(metadata, facts))
    }

    pub fn read_facts(self) -> Result<Vec<TraceFact>, JsonlTraceReadError> {
        let mut facts = Vec::new();
        for record in self.read_records()? {
            if let TraceJsonlRecord::Fact(fact) = record {
                facts.push(fact);
            }
        }
        Ok(facts)
    }

    pub fn read_records(self) -> Result<Vec<TraceJsonlRecord>, JsonlTraceReadError> {
        let mut records = Vec::new();
        for (index, line) in self.reader.lines().enumerate() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            records.push(parse_trace_jsonl_record(&line, index + 1)?);
        }
        Ok(records)
    }
}

pub fn read_trace_facts_jsonl<R>(reader: R) -> Result<Vec<TraceFact>, JsonlTraceReadError>
where
    R: BufRead,
{
    JsonlTraceReader::new(reader).read_facts()
}

pub fn read_trace_bundle_jsonl<R>(reader: R) -> Result<TraceBundle, JsonlTraceReadError>
where
    R: BufRead,
{
    JsonlTraceReader::new(reader).read_bundle()
}

fn parse_trace_jsonl_record(
    line: &str,
    line_number: usize,
) -> Result<TraceJsonlRecord, JsonlTraceReadError> {
    match serde_json::from_str(line) {
        Ok(record) => Ok(record),
        Err(record_error) => match serde_json::from_str(line) {
            Ok(fact) => Ok(TraceJsonlRecord::Fact(fact)),
            Err(_) => Err(JsonlTraceReadError::Json {
                line: line_number,
                source: record_error,
            }),
        },
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use common::origin::OriginExportKey;

    use crate::{
        CompilerPhase, InstructionFact, JsonlTraceReader, JsonlTraceSink, OriginNodeFact,
        OriginNodeKind, TRACE_SCHEMA_VERSION, TraceBundle, TraceDataSource, TraceFact,
        TraceJsonlRecord, TraceMetadata, TraceMetadataError, read_trace_bundle_jsonl,
        read_trace_facts_jsonl,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn jsonl_sink_writes_one_fact_per_line_and_roundtrips() {
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_fact(&TraceFact::OriginNode(OriginNodeFact::new(
            key("function", "fib", "recv"),
            OriginNodeKind::new("function"),
        )))
        .unwrap();
        sink.write_fact(&TraceFact::Instruction(InstructionFact::new(
            key("asm.inst", "fib", "inst:0"),
            key("function", "fib", "recv"),
            0,
            "lw",
        )))
        .unwrap();

        let output = String::from_utf8(sink.into_inner()).unwrap();
        assert_eq!(output.lines().count(), 2);

        let facts = read_trace_facts_jsonl(Cursor::new(output)).unwrap();
        assert_eq!(facts.len(), 2);
        assert_eq!(facts[0].relation_name(), "origin_node");

        let _ = CompilerPhase::Backend;
    }

    #[test]
    fn jsonl_bundle_requires_metadata_and_roundtrips() {
        let metadata = TraceMetadata::fixture(
            "abc123",
            "riscv64-demo",
            vec![
                "fe".to_string(),
                "dev".to_string(),
                "trace-fixture".to_string(),
            ],
            "fib_demo.fe",
            vec!["function=Fib.recv".to_string()],
            "fib_demo_codegen_ux_v1",
        );
        let bundle = TraceBundle::new(
            metadata,
            vec![TraceFact::OriginNode(OriginNodeFact::new(
                key("function", "fib", "recv"),
                OriginNodeKind::new("function"),
            ))],
        );

        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        let output = String::from_utf8(sink.into_inner()).unwrap();

        let roundtripped = JsonlTraceReader::new(Cursor::new(&output))
            .read_bundle()
            .unwrap();
        assert_eq!(roundtripped.metadata.data_source, TraceDataSource::Fixture);
        assert_eq!(roundtripped.facts.len(), 1);

        assert_eq!(
            read_trace_bundle_jsonl(Cursor::new(output))
                .unwrap()
                .metadata
                .fixture_marker
                .as_deref(),
            Some("fib_demo_codegen_ux_v1")
        );
    }

    #[test]
    fn jsonl_bundle_rejects_unsupported_schema_version() {
        let mut metadata = TraceMetadata::compiler_emitted(
            "abc123",
            "evm/sonatina",
            vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
            "demo.fe",
            Vec::new(),
        );
        metadata.schema_version = TRACE_SCHEMA_VERSION + 1;
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_record(&TraceJsonlRecord::Metadata(metadata))
            .unwrap();
        let output = String::from_utf8(sink.into_inner()).unwrap();

        assert!(matches!(
            read_trace_bundle_jsonl(Cursor::new(output)),
            Err(super::JsonlTraceReadError::InvalidMetadata(
                TraceMetadataError::UnsupportedSchemaVersion { .. }
            ))
        ));
    }

    #[test]
    fn jsonl_bundle_rejects_inconsistent_fixture_metadata() {
        let metadata = TraceMetadata {
            schema_version: TRACE_SCHEMA_VERSION,
            compiler_commit: "abc123".to_string(),
            target: "evm/sonatina".to_string(),
            command: vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
            input_path: "demo.fe".to_string(),
            flags: Vec::new(),
            data_source: TraceDataSource::Fixture,
            fixture_marker: None,
        };
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_record(&TraceJsonlRecord::Metadata(metadata))
            .unwrap();
        let output = String::from_utf8(sink.into_inner()).unwrap();

        assert!(matches!(
            read_trace_bundle_jsonl(Cursor::new(output)),
            Err(super::JsonlTraceReadError::InvalidMetadata(
                TraceMetadataError::MissingFixtureMarker
            ))
        ));
    }
}
