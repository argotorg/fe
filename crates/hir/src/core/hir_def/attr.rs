use super::{IdentId, Partial, StringId};

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
    pub name: Partial<IdentId<'db>>,
    pub args: Vec<AttrArg<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocCommentAttr<'db> {
    /// This is the text of the doc comment, excluding the `///` prefix.
    pub text: StringId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrArg<'db> {
    pub key: Partial<IdentId<'db>>,
    pub value: Partial<IdentId<'db>>,
}
