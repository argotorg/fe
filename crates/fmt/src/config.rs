/// Controls when trailing commas are added to lists.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum TrailingComma {
    /// Always add trailing commas.
    Always,
    /// Never add trailing commas.
    Never,
    /// Add trailing commas only for multiline (vertical) lists.
    #[default]
    Multiline,
}

/// Global configuration for the Fe formatter.
#[derive(Clone, Debug)]
pub struct Config {
    /// Maximum width of a formatted line, in characters.
    pub max_width: usize,
    /// Width of a single indentation level, in spaces.
    pub indent_width: usize,
    /// Indentation for `where` and `uses` clauses, in spaces.
    pub clause_indent: usize,
    /// Whether `where` clauses should always start on a new line.
    pub where_new_line: bool,
    /// When to add trailing commas in lists.
    pub trailing_comma: TrailingComma,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_width: 100,
            indent_width: 4,
            clause_indent: 4,
            where_new_line: false,
            trailing_comma: TrailingComma::default(),
        }
    }
}
