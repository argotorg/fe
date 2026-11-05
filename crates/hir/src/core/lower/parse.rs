use common::file::File;
use parser::{GreenNode, RecoveryMode};

use crate::{HirDb, hir_def::TopLevelMod};

/// The syntax tree of a file and the errors parsing it reported.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedFile {
    pub root: GreenNode,
    pub errors: Vec<ParserError>,
}

#[salsa::tracked]
pub fn parse_file_impl<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> ParsedFile {
    let file = top_mod.file(db);
    let text = file.text(db);
    let recovery_mode = RecoveryMode::new(db.compiler_options().recovery_mode(db));
    let (root, parse_errors) = parser::parse_source_file(text, recovery_mode);
    ParsedFile {
        root,
        errors: parse_errors
            .into_iter()
            .map(|error| ParserError { file, error })
            .collect(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    pub file: File,
    pub error: parser::ParseError,
}
