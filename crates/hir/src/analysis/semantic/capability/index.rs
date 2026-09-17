//! Lexically scoped symbolic indices. Binder numbers are lexical levels, never allocator IDs.
use std::collections::BTreeMap;

use crate::analysis::semantic::normalized::{NIndex, NValueId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IndexNamespace {
    Value,
    Loan,
    Result,
    InputSlot,
    Existential,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundIndex {
    namespace: IndexNamespace,
    level: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IndexExpr {
    Const(usize),
    Runtime(NValueId),
    FormalValue(u32),
    Bound(BoundIndex),
}

impl From<NIndex> for IndexExpr {
    fn from(index: NIndex) -> Self {
        match index {
            NIndex::Const(value) => Self::Const(value),
            NIndex::Value(value) => Self::Runtime(value),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinderScope {
    counts: [u32; 5],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IndexError {
    FreeBinder(IndexExpr),
    ConstantSubstitution,
    ConflictingSubstitution(IndexExpr),
    ScopeMismatch,
}

impl BinderScope {
    pub fn bind(&self, namespace: IndexNamespace) -> (Self, IndexExpr) {
        let mut nested = self.clone();
        let level = &mut nested.counts[namespace as usize];
        let index = IndexExpr::Bound(BoundIndex {
            namespace,
            level: *level,
        });
        *level = level
            .checked_add(1)
            .expect("symbolic binder depth overflow");
        (nested, index)
    }

    pub fn validate(&self, index: IndexExpr) -> Result<(), IndexError> {
        if let IndexExpr::Bound(bound) = index
            && bound.level >= self.counts[bound.namespace as usize]
        {
            return Err(IndexError::FreeBinder(index));
        }
        Ok(())
    }

    pub(crate) fn variables(&self) -> impl Iterator<Item = IndexExpr> + '_ {
        [
            IndexNamespace::Value,
            IndexNamespace::Loan,
            IndexNamespace::Result,
            IndexNamespace::InputSlot,
            IndexNamespace::Existential,
        ]
        .into_iter()
        .flat_map(|namespace| {
            (0..self.counts[namespace as usize])
                .map(move |level| IndexExpr::Bound(BoundIndex { namespace, level }))
        })
    }
}

/// A simultaneous, scope-checked substitution. Applying it never follows chains.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexSubst {
    source: BinderScope,
    destination: BinderScope,
    entries: BTreeMap<IndexExpr, IndexExpr>,
}

impl IndexSubst {
    pub fn new(
        source: &BinderScope,
        destination: &BinderScope,
        entries: impl IntoIterator<Item = (IndexExpr, IndexExpr)>,
    ) -> Result<Self, IndexError> {
        let mut map = BTreeMap::new();
        for (from, to) in entries {
            source.validate(from)?;
            destination.validate(to)?;
            if matches!(from, IndexExpr::Const(_)) {
                return Err(IndexError::ConstantSubstitution);
            }
            if map.insert(from, to).is_some_and(|previous| previous != to) {
                return Err(IndexError::ConflictingSubstitution(from));
            }
        }
        map.retain(|from, to| from != to);
        let substitution = Self {
            source: source.clone(),
            destination: destination.clone(),
            entries: map,
        };
        for variable in source.variables() {
            destination.validate(substitution.apply(variable))?;
        }
        Ok(substitution)
    }

    pub fn apply(&self, index: IndexExpr) -> IndexExpr {
        self.entries.get(&index).copied().unwrap_or(index)
    }

    pub fn source(&self) -> &BinderScope {
        &self.source
    }
    pub fn destination(&self) -> &BinderScope {
        &self.destination
    }

    pub fn then(&self, next: &Self) -> Result<Self, IndexError> {
        if self.destination != next.source {
            return Err(IndexError::ScopeMismatch);
        }
        let keys = self
            .entries
            .keys()
            .chain(next.entries.keys())
            .copied()
            .filter(|index| self.source.validate(*index).is_ok());
        Self::new(
            &self.source,
            &next.destination,
            keys.map(|index| (index, next.apply(self.apply(index)))),
        )
    }

    /// Lift through a new lexical binder without capturing destination variables.
    pub(crate) fn under_binder(&self, namespace: IndexNamespace) -> Self {
        let (source, from) = self.source.bind(namespace);
        let (destination, to) = self.destination.bind(namespace);
        Self::new(
            &source,
            &destination,
            self.entries
                .iter()
                .map(|(from, to)| (*from, *to))
                .chain([(from, to)]),
        )
        .expect("lifting a checked substitution preserves binder scope")
    }
}
