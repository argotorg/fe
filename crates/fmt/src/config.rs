/// Global configuration for the Fe formatter.
#[derive(Clone, Debug)]
pub struct Config {
    /// Maximum width of a formatted line, in characters.
    pub max_width: usize,
    /// Width of a single indentation level, in spaces.
    pub indent_width: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_width: 100,
            indent_width: 4,
        }
    }
}
