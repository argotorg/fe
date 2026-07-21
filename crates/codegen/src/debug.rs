use std::fmt;

use common::{origin::OriginExportKey, source::SourceLocation};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytecodePcRange {
    pub start: u32,
    pub end: u32,
}

impl BytecodePcRange {
    pub fn try_new(start: u32, end: u32) -> Result<Self, BytecodeDebugError> {
        if start >= end {
            return Err(BytecodeDebugError::InvalidPcRange { start, end });
        }
        Ok(Self { start, end })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum BytecodeSourceMapEntryKind {
    Source { location: SourceLocation },
    NonSource { reason: String },
}

impl BytecodeSourceMapEntryKind {
    pub fn non_source(reason: impl Into<String>) -> Result<Self, BytecodeDebugError> {
        let reason = reason.into();
        if reason.is_empty() {
            return Err(BytecodeDebugError::EmptyNonSourceReason);
        }
        Ok(Self::NonSource { reason })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytecodeSourceMapEntry {
    pub origin: OriginExportKey,
    pub pc: BytecodePcRange,
    pub entry: BytecodeSourceMapEntryKind,
}

impl BytecodeSourceMapEntry {
    pub fn source(origin: OriginExportKey, pc: BytecodePcRange, location: SourceLocation) -> Self {
        Self {
            origin,
            pc,
            entry: BytecodeSourceMapEntryKind::Source { location },
        }
    }

    pub fn non_source(
        origin: OriginExportKey,
        pc: BytecodePcRange,
        reason: impl Into<String>,
    ) -> Result<Self, BytecodeDebugError> {
        Ok(Self {
            origin,
            pc,
            entry: BytecodeSourceMapEntryKind::non_source(reason)?,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BytecodeDebugError {
    InvalidPcRange { start: u32, end: u32 },
    EmptyNonSourceReason,
}

impl fmt::Display for BytecodeDebugError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPcRange { start, end } => {
                write!(f, "bytecode PC range {start}..{end} is invalid")
            }
            Self::EmptyNonSourceReason => write!(f, "non-source bytecode reason must not be empty"),
        }
    }
}

impl std::error::Error for BytecodeDebugError {}

#[cfg(test)]
mod tests {
    use super::{BytecodePcRange, BytecodeSourceMapEntry};
    use common::{origin::OriginExportKey, source::SourceLocation};

    fn origin_key() -> OriginExportKey {
        OriginExportKey::try_from_raw_parts("bytecode.pc", "runtime:main", "pc:0..2").unwrap()
    }

    #[test]
    fn source_map_entry_reuses_canonical_source_location_and_origin_key() {
        let location =
            SourceLocation::try_new("src/main.fe", 10, 14, 1, 2, 1, 6, Some("main".to_string()))
                .unwrap();
        let entry = BytecodeSourceMapEntry::source(
            origin_key(),
            BytecodePcRange::try_new(0, 2).unwrap(),
            location,
        );
        let json = serde_json::to_string(&entry).unwrap();
        let decoded: BytecodeSourceMapEntry = serde_json::from_str(&json).unwrap();

        assert_eq!(decoded.origin.kind(), "bytecode.pc");
        assert_eq!(decoded, entry);
    }

    #[test]
    fn bytecode_debug_records_reject_invalid_ranges_and_empty_reasons() {
        assert!(BytecodePcRange::try_new(4, 4).is_err());
        assert!(
            BytecodeSourceMapEntry::non_source(
                origin_key(),
                BytecodePcRange::try_new(0, 1).unwrap(),
                ""
            )
            .is_err()
        );
    }
}
