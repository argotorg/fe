use super::{IdentId, Partial, PathId};

#[salsa::interned]
#[derive(Debug)]
pub struct EffectParamListId<'db> {
    #[return_ref]
    pub data: Vec<EffectParam<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectParam<'db> {
    pub name: Partial<IdentId<'db>>,    // None for unlabeled effects
    pub key_path: Partial<PathId<'db>>, // Unresolved path; resolve type/trait in analysis
    pub is_mut: bool,
}
