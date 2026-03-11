use smol_str::SmolStr;

use crate::InputDb;

#[salsa::input]
#[derive(Debug)]
pub struct CompilationSettings {
    pub profile: SmolStr,
}

#[salsa::tracked]
impl CompilationSettings {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, SmolStr::new("dev"))
    }
}
