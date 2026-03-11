#[derive(Debug, Clone, PartialEq)]
pub struct CompilerOptions {
    /// Whether to use recovery mode when parsing.
    pub use_recovery_mode: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            use_recovery_mode: true,
        }
    }
}

impl CompilerOptions {
    pub fn set_recovery_mode(&mut self, use_recovery_mode: bool) {
        self.use_recovery_mode = use_recovery_mode;
    }
}
