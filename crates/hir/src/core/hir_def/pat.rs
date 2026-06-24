use cranelift_entity::entity_impl;

use super::{Body, ExprId, IdentId, LitKind, Partial, PathId};
use crate::{HirDb, span::pat::LazyPatSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum Pat<'db> {
    WildCard,
    Rest,
    Lit(Partial<LitKind<'db>>),
    Tuple(Vec<PatId>),
    /// The second bool is `true` if the pat has `mut` in front of it.
    Path(Partial<PathId<'db>>, bool),
    PathTuple(Partial<PathId<'db>>, Vec<PatId>),
    Record(Partial<PathId<'db>>, Vec<RecordPatField<'db>>),
    Or(PatId, PatId),
    /// `${expr}(group)` — a splice hole in pattern position inside a quote
    /// body. The expression is evaluated by the provider executor when the
    /// quote is built (it must yield a reflected `Variant` handle); the
    /// binder pats name the group the variant's payload is bound under.
    QuoteHole(ExprId, Vec<PatId>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
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
