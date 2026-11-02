use smol_str::SmolStr;

use crate::InputDb;

/// The machine a compilation is for. Integer widths, layouts and destruction
/// depend on it, so a database compiles for one target at a time.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum Target {
    #[default]
    Evm,
    Native,
}

impl Target {
    pub const ALL: [Target; 2] = [Target::Evm, Target::Native];

    /// The width of `usize`, `isize` and memory addresses.
    pub fn usize_bits(self) -> usize {
        match self {
            Target::Evm => 256,
            Target::Native => 64,
        }
    }
}

#[salsa::input]
#[derive(Debug)]
pub struct CompilationSettings {
    pub profile: SmolStr,
    #[returns(copy)]
    pub target: Target,
}

#[salsa::tracked]
impl CompilationSettings {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, SmolStr::new("dev"), Target::Evm)
    }
}
