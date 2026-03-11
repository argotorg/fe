use common::file::File;
use parser::{GreenNode, RecoveryMode};
use salsa::Accumulator;

use crate::{HirDb, hir_def::TopLevelMod};

#[salsa::tracked]
pub fn parse_file_impl<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> GreenNode {
    let file = top_mod.file(db);
    let text = file.text(db);
    let recovery_mode = RecoveryMode::new(db.compiler_options().use_recovery_mode);
    let (node, parse_errors) = parser::parse_source_file(text, recovery_mode);

    for error in parse_errors {
        ParserError { file, error }.accumulate(db);
    }
    node
}

#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    pub file: File,
    pub error: parser::ParseError,
}
