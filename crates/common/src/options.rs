use salsa::Durability;

use crate::InputDb;

/// Compiler options. They rarely change, so they have medium durability: a
/// query over the builtins that reads them stays verified without walking its
/// dependencies when the user's files change.
#[salsa::input]
#[derive(Debug)]
pub struct CompilerOptions {
    /// Whether to use recovery mode when parsing.
    #[returns(copy)]
    pub recovery_mode: bool,
}

#[salsa::tracked]
impl CompilerOptions {
    pub fn default(db: &dyn InputDb) -> Self {
        CompilerOptions::builder(true)
            .durability(Durability::MEDIUM)
            .new(db)
    }
}
