use crate::Config;
use parser::TextRange;

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
}
