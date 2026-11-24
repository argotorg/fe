use super::{IdentId, ModeMarker, Partial, TypeId};
use crate::HirDb;

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

impl<'db> EffectParam<'db> {
    /// Whether the effect is written `mut`.
    pub fn is_mut(&self) -> bool {
        self.mode == ModeMarker::Mut
    }

    /// The name a body binds the effect to: its label, or else its key's
    /// last segment.
    pub fn binding_name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.name
            .or_else(|| self.key_ty.to_opt()?.as_path(db)?.ident(db).to_opt())
    }
}
