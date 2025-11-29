use std::fs;
use std::path::Path;

use parser::{SyntaxNode, ast, ast::prelude::AstNode, parse_source_file};

use crate::{Config, Indent, Rewrite, RewriteContext, Shape};

/// Errors that can occur while formatting Fe source.
#[derive(Debug)]
pub enum FormatError {
    /// The input contained parse errors, so formatting was aborted.
    ParseErrors(Vec<parser::ParseError>),
    /// An underlying I/O error ocurred while reading a file.
    Io(std::io::Error),
}

impl From<std::io::Error> for FormatError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

/// Format a Fe source string according to the provided [`Config`].
pub fn format_str(source: &str, config: &Config) -> Result<String, FormatError> {
    let (green, parse_errors) = parse_source_file(source);
    if !parse_errors.is_empty() {
        return Err(FormatError::ParseErrors(parse_errors));
    }

    let root_syntax = SyntaxNode::new_root(green);
    let root =
        ast::Root::cast(root_syntax).expect("parser must always produce a root node for source");

    let context = RewriteContext { config, source };
    let shape = Shape::with_width(config.max_width, Indent::default());

    Ok(root
        .rewrite(&context, shape)
        // If formatting fails for any reason, fall back to the original text.
        .unwrap_or_else(|| source.to_owned()))
}

/// Format the Fe source file at the given `path`.
pub fn format_file(path: &Path, config: &Config) -> Result<String, FormatError> {
    let source = fs::read_to_string(path)?;
    format_str(&source, config)
}
