use crate::HirDb;

use super::{IdentId, Partial, PathId, StringId};

#[salsa::interned]
#[derive(Debug)]
pub struct AttrListId<'db> {
    #[return_ref]
    pub data: Vec<Attr<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum Attr<'db> {
    Normal(NormalAttr<'db>),
    DocComment(DocCommentAttr<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalAttr<'db> {
    pub path: Partial<PathId<'db>>,
    pub args: Vec<AttrArg<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocCommentAttr<'db> {
    /// This is the text of the doc comment, excluding the `///` prefix.
    pub text: StringId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrArg<'db> {
    pub key: Partial<PathId<'db>>,
    pub value: Partial<AttrArgValue<'db>>,
}

impl<'db> AttrArg<'db> {
    pub fn key_str(&self, db: &'db dyn HirDb) -> Option<&str> {
        self.key
            .to_opt()
            .and_then(|p| p.as_ident(db))
            .map(|i| i.data(db).as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrArgValue<'db> {
    Ident(IdentId<'db>),
    Lit(super::LitKind<'db>),
}
