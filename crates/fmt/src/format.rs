use std::fs;
use std::path::Path;

use parser::{SyntaxNode, ast, ast::prelude::AstNode, parse_source_file};
use pretty::RcAllocator;

use crate::ast::ToDoc;
use crate::{Config, RewriteContext};

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

    let ctx = RewriteContext {
        config,
        source,
        alloc: RcAllocator,
    };

    let doc = root.to_doc(&ctx);
    let mut output = Vec::new();
    doc.into_doc()
        .render(config.max_width, &mut output)
        .expect("rendering to Vec should never fail");

    let formatted = String::from_utf8(output).unwrap_or_else(|_| source.to_owned());

    // Post-process to remove trailing whitespace from blank lines.
    // The pretty crate adds indentation after hardlines, which creates
    // trailing whitespace on intentional blank lines inside blocks.
    let mut cleaned: String = formatted
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n");

    // Preserve trailing newline if the original source had one
    if source.ends_with('\n') && !cleaned.ends_with('\n') {
        cleaned.push('\n');
    }

    Ok(cleaned)
}

/// Format the Fe source file at the given `path`.
pub fn format_file(path: &Path, config: &Config) -> Result<String, FormatError> {
    let source = fs::read_to_string(path)?;
    format_str(&source, config)
}
