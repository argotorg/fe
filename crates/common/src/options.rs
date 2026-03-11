use crate::InputDb;

#[salsa::input]
#[derive(Debug)]
pub struct CompilerOptions {
    /// Whether to use recovery mode when parsing.
    pub recovery_mode: bool,
}

#[salsa::tracked]
impl CompilerOptions {
    pub fn default(db: &dyn InputDb) -> Self {
        CompilerOptions::new(db, true)
    }
}
