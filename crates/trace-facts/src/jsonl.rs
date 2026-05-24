use std::fmt;
use std::io::{self, BufRead, Write};

use crate::fact::TraceFact;

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

    pub fn write_fact(&mut self, fact: &TraceFact) -> io::Result<()> {
        serde_json::to_writer(&mut self.writer, fact).map_err(io::Error::other)?;
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
}

impl fmt::Display for JsonlTraceReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => write!(f, "failed to read trace JSONL: {err}"),
            Self::Json { line, source } => {
                write!(f, "failed to parse trace JSONL line {line}: {source}")
            }
        }
    }
}

impl std::error::Error for JsonlTraceReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(err) => Some(err),
            Self::Json { source, .. } => Some(source),
        }
    }
}

impl From<io::Error> for JsonlTraceReadError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

pub fn read_trace_facts_jsonl<R>(reader: R) -> Result<Vec<TraceFact>, JsonlTraceReadError>
where
    R: BufRead,
{
    let mut facts = Vec::new();
    for (index, line) in reader.lines().enumerate() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let fact = serde_json::from_str(&line).map_err(|source| JsonlTraceReadError::Json {
            line: index + 1,
            source,
        })?;
        facts.push(fact);
    }
    Ok(facts)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use common::origin::OriginExportKey;

    use crate::{
        CompilerPhase, InstructionFact, JsonlTraceSink, OriginNodeFact, OriginNodeKind, TraceFact,
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
}
