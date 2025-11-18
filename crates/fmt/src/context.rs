use crate::Config;
use parser::{TextRange, ast::prelude::AstNode, syntax_node::NodeOrToken};

/// Shared, read-only context passed to [`Rewrite`] implementations.
#[derive(Debug)]
pub struct RewriteContext<'a> {
    /// Global formatting configuration.
    pub config: &'a Config,
    /// Original source text of the file being formatted.
    pub source: &'a str,
}

impl<'a> RewriteContext<'a> {
    /// Returns the exact source slice corresponding to `range`.
    pub fn snippet(&self, range: TextRange) -> &'a str {
        let start: usize = usize::from(range.start());
        let end: usize = usize::from(range.end());
        &self.source[start..end]
    }

    /// Returns the trimmed source text for the given AST node.
    pub fn snippet_trimmed(&self, node: &impl AstNode) -> String {
        self.snippet(node.syntax().text_range()).trim().to_owned()
    }

    /// Returns the trimmed source text for a NodeOrToken.
    pub fn snippet_node_or_token(&self, node_or_token: &NodeOrToken) -> String {
        match node_or_token {
            NodeOrToken::Node(node) => self.snippet(node.text_range()),
            NodeOrToken::Token(token) => self.snippet(token.text_range()),
        }
        .trim()
        .to_owned()
    }
}
