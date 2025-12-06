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
    /// Maximum width of function call arguments before falling back to vertical formatting.
    pub fn_call_width: usize,
    /// Maximum width in the body of a struct literal before falling back to vertical formatting.
    /// A value of 0 results in struct literals always being broken into multiple lines.
    pub struct_lit_width: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_width: 100,
            indent_width: 4,
            clause_indent: 4,
            where_new_line: false,
            trailing_comma: TrailingComma::default(),
            fn_call_width: 60,
            struct_lit_width: 20,
        }
    }
}
