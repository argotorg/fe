use crate::InputDb;

#[salsa::input]
#[derive(Debug)]
pub struct CompilerOptions {
    /// Whether to use recovery mode when parsing.
    pub recovery_mode: bool,
    /// Whether to track provenance (origin chains) through lowering.
    /// When enabled, each IR node records which input node it was derived from.
    pub emit_provenance: bool,
}

#[salsa::tracked]
impl CompilerOptions {
    pub fn default(db: &dyn InputDb) -> Self {
        CompilerOptions::new(db, true, false)
    }
}
