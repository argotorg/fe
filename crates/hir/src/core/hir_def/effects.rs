use super::{IdentId, Partial, TypeId};

#[salsa::interned]
#[derive(Debug)]
pub struct EffectParamListId<'db> {
    #[return_ref]
    pub data: Vec<EffectParam<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectParam<'db> {
    pub name: Option<IdentId<'db>>,
    pub key_ty: Partial<TypeId<'db>>,
    pub is_mut: bool,
}
