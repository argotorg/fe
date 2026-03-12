use crate::HirDb;

use super::{IdentId, Partial, PathId, StringId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithmeticMode {
    Checked,
    Unchecked,
}

impl ArithmeticMode {
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "checked" => Some(Self::Checked),
            "unchecked" => Some(Self::Unchecked),
            _ => None,
        }
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct AttrListId<'db> {
    #[return_ref]
    pub data: Vec<Attr<'db>>,
}

impl<'db> AttrListId<'db> {
    /// Returns true if this attribute list contains an attribute with the given name.
    ///
    /// Only checks simple identifier attributes (e.g., `#[msg]`), not path attributes.
    pub fn has_attr(self, db: &'db dyn HirDb, name: &str) -> bool {
        self.data(db).iter().any(|attr| {
            if let Attr::Normal(normal_attr) = attr
                && let Some(path) = normal_attr.path.to_opt()
                && let Some(ident) = path.as_ident(db)
            {
                ident.data(db) == name
            } else {
                false
            }
        })
    }

    /// Returns the attribute with the given name, if present.
    pub fn get_attr(self, db: &'db dyn HirDb, name: &str) -> Option<&'db NormalAttr<'db>> {
        self.data(db).iter().find_map(|attr| {
            if let Attr::Normal(normal_attr) = attr
                && let Some(path) = normal_attr.path.to_opt()
                && let Some(ident) = path.as_ident(db)
                && ident.data(db) == name
            {
                Some(normal_attr)
            } else {
                None
            }
        })
    }

    pub fn arithmetic_mode(self, db: &'db dyn HirDb) -> Option<ArithmeticMode> {
        self.data(db)
            .iter()
            .filter_map(|attr| {
                let Attr::Normal(normal_attr) = attr else {
                    return None;
                };
                let path = normal_attr.path.to_opt()?;
                let ident = path.as_ident(db)?;
                if ident.data(db) != "arithmetic" {
                    return None;
                }
                normal_attr.arithmetic_mode_arg(db)
            })
            .last()
    }
}

impl<'db> NormalAttr<'db> {
    /// Returns true if this attribute has an argument with the given key (no value).
    ///
    /// For example, `#[test(should_revert)]` has the argument `should_revert`.
    pub fn has_arg(&self, db: &'db dyn HirDb, key: &str) -> bool {
        self.args.iter().any(|arg| {
            arg.value.is_none()
                && arg
                    .key
                    .to_opt()
                    .and_then(|p| p.as_ident(db))
                    .is_some_and(|ident| ident.data(db) == key)
        })
    }

    pub fn arithmetic_mode_arg(&self, db: &'db dyn HirDb) -> Option<ArithmeticMode> {
        let [arg] = self.args.as_slice() else {
            return None;
        };
        if arg.value.is_some() {
            return None;
        }
        let mode = arg
            .key
            .to_opt()
            .and_then(|path| path.as_ident(db))
            .map(|ident| ident.data(db).as_str())?;
        ArithmeticMode::parse(mode)
    }
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
    /// The value after `=` in `#[attr(key = value)]`. None for `#[attr(key)]` form.
    pub value: Option<AttrArgValue<'db>>,
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
