use std::fmt;

use serde::{Deserialize, Serialize};

/// Canonical exported source/display location.
///
/// Source locations are display metadata for humans. They must not be used as
/// compiler identity; trace/debug records should join through origin export
/// keys instead.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: String,
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
    pub snippet: Option<String>,
}

impl SourceLocation {
    #[allow(clippy::too_many_arguments)]
    pub fn try_new(
        file: impl Into<String>,
        start_byte: u32,
        end_byte: u32,
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
        snippet: Option<String>,
    ) -> Result<Self, SourceLocationError> {
        let file = file.into();
        if file.is_empty() {
            return Err(SourceLocationError::EmptyFile);
        }
        if start_byte > end_byte {
            return Err(SourceLocationError::InvalidByteRange {
                start: start_byte,
                end: end_byte,
            });
        }
        if (start_line, start_col) > (end_line, end_col) {
            return Err(SourceLocationError::InvalidLineRange {
                start_line,
                start_col,
                end_line,
                end_col,
            });
        }
        Ok(Self {
            file,
            start_byte,
            end_byte,
            start_line,
            start_col,
            end_line,
            end_col,
            snippet,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceLocationError {
    EmptyFile,
    InvalidByteRange {
        start: u32,
        end: u32,
    },
    InvalidLineRange {
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    },
}

impl fmt::Display for SourceLocationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyFile => write!(f, "source location file must not be empty"),
            Self::InvalidByteRange { start, end } => {
                write!(f, "source location byte range {start}..{end} is invalid")
            }
            Self::InvalidLineRange {
                start_line,
                start_col,
                end_line,
                end_col,
            } => write!(
                f,
                "source location line range {start_line}:{start_col}..{end_line}:{end_col} is invalid"
            ),
        }
    }
}

impl std::error::Error for SourceLocationError {}

#[cfg(test)]
mod tests {
    use super::{SourceLocation, SourceLocationError};

    #[test]
    fn source_location_validates_as_display_metadata() {
        let location =
            SourceLocation::try_new("src/main.fe", 10, 14, 1, 2, 1, 6, Some("main".to_string()))
                .unwrap();

        assert_eq!(location.file, "src/main.fe");
        assert_eq!(
            SourceLocation::try_new("", 0, 0, 1, 0, 1, 0, None),
            Err(SourceLocationError::EmptyFile)
        );
        assert!(SourceLocation::try_new("src/main.fe", 14, 10, 1, 2, 1, 6, None).is_err());
    }
}
