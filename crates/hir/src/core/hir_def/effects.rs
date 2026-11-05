use super::{IdentId, ModeMarker, Partial, TypeId};

#[salsa::interned]
#[derive(Debug)]
pub struct EffectParamListId<'db> {
    pub data: Vec<EffectParam<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct EffectParam<'db> {
    pub name: Option<IdentId<'db>>,
    pub key_ty: Partial<TypeId<'db>>,
    pub mode: ModeMarker,
}

impl EffectParam<'_> {
    /// Whether the effect is written `mut`.
    pub fn is_mut(&self) -> bool {
        self.mode == ModeMarker::Mut
    }
}
