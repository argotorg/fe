use cranelift_entity::entity_impl;

use super::{Body, IdentId, LitKind, Partial, PathId};
use crate::{HirDb, span::pat::LazyPatSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum Pat<'db> {
    WildCard,
    Rest,
    Lit(Partial<LitKind<'db>>),
    Tuple(Vec<PatId>),
    Path(Partial<PathId<'db>>, BindingMarker),
    PathTuple(Partial<PathId<'db>>, Vec<PatId>),
    Record(Partial<PathId<'db>>, Vec<RecordPatField<'db>>),
    Or(PatId, PatId),
}

/// The marker in front of a binding name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum BindingMarker {
    /// `x`: a value, or the access of a `mut` or `#[view]` component.
    Plain,
    /// `mut x`: a mutable access.
    Mut,
    /// `mut? x`: an access at the item's mode.
    MaybeMut,
    /// `ref x`: a read access.
    Ref,
    /// `var x`: a mutable value.
    Var,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct PatId(u32);
entity_impl!(PatId);

impl PatId {
    pub fn span(self, body: Body) -> LazyPatSpan {
        LazyPatSpan::new(body, self)
    }

    pub fn data<'db>(self, db: &'db dyn HirDb, body: Body<'db>) -> &'db Partial<Pat<'db>> {
        &body.pats(db)[self]
    }

    pub fn is_rest(self, db: &dyn HirDb, body: Body) -> bool {
        matches!(self.data(db, body), Partial::Present(Pat::Rest))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct RecordPatField<'db> {
    pub label: Partial<IdentId<'db>>,
    pub pat: PatId,
}

impl<'db> RecordPatField<'db> {
    pub fn label(&self, db: &'db dyn HirDb, body: Body<'db>) -> Option<IdentId<'db>> {
        if let Partial::Present(label) = self.label {
            return Some(label);
        }

        match self.pat.data(db, body) {
            Partial::Present(Pat::Path(Partial::Present(path), _)) => path.as_ident(db),
            _ => None,
        }
    }
}
