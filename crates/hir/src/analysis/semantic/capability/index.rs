//! Lexically scoped symbolic indices. Binder numbers are lexical levels, never allocator IDs.
use num_bigint::BigInt;
use std::collections::{BTreeMap, BTreeSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        int_const,
        normalized::{NBlockId, NIndex, NValueId},
    },
    ty::{
        const_ty::{ConstTyId, const_ty_from_sem_const, normalize_const_tys_for_comparison},
        fold::{TyFoldable, TyFolder},
        ty_def::{TyData, TyId},
    },
};

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
pub enum IndexExpr<'db> {
    Const(usize),
    Runtime(NValueId),
    Iteration(NBlockId),
    FormalValue(u32),
    /// A complete const-expression atom in comparison-normalized form.
    TypeConst(ConstTyId<'db>),
    Bound(BoundIndex),
}

impl IndexExpr<'_> {
    pub fn bound_namespace(self) -> Option<IndexNamespace> {
        match self {
            Self::Bound(index) => Some(index.namespace),
            _ => None,
        }
    }
}

impl<'db> From<NIndex> for IndexExpr<'db> {
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
pub enum IndexError<'db> {
    FreeBinder(IndexExpr<'db>),
    ConstantSubstitution,
    InvalidConstSubstitution,
    ConflictingSubstitution(IndexExpr<'db>),
    ScopeMismatch,
}

impl BinderScope {
    pub fn bind<'db>(&self, namespace: IndexNamespace) -> (Self, IndexExpr<'db>) {
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

    /// The lexical owner of clause-local existential binders.
    pub fn without_existentials(&self) -> Self {
        let mut owner = self.clone();
        owner.counts[IndexNamespace::Existential as usize] = 0;
        owner
    }

    /// Give an independently quantified occurrence fresh existential binders.
    pub fn freshening<'db>(&self, destination: &Self) -> IndexSubst<'db> {
        let mut destination = destination.clone();
        let entries: Vec<_> = self
            .variables()
            .map(|source| {
                let (nested, target) = destination.bind(IndexNamespace::Existential);
                destination = nested;
                (source, target)
            })
            .collect();
        IndexSubst::new(self, &destination, entries).expect("fresh binders are scoped")
    }

    /// Keep `owner`'s binders and give the binders local to this scope, such as
    /// an array leaf's element binders, fresh existential witnesses. `owner`
    /// must be a lexical ancestor; unrelated scopes are never matched by count.
    pub fn quantifying<'db>(&self, owner: &Self) -> IndexSubst<'db> {
        assert!(
            self.counts
                .iter()
                .zip(owner.counts)
                .all(|(count, owner)| owner <= *count),
            "quantified scope must extend its owner"
        );
        let mut destination = owner.clone();
        let entries: Vec<_> = self
            .variables()
            .map(|source| {
                if owner.validate(source).is_ok() {
                    return (source, source);
                }
                let (nested, target) = destination.bind(IndexNamespace::Existential);
                destination = nested;
                (source, target)
            })
            .collect();
        IndexSubst::new(self, &destination, entries).expect("quantified binders are scoped")
    }

    /// Extra existential variables are owned by a clause, not by its surrounding
    /// value or loan family. Other namespaces must match the lexical parent.
    pub fn existential_extension_of(&self, parent: &Self) -> Option<u32> {
        self.counts
            .iter()
            .zip(parent.counts)
            .enumerate()
            .all(|(namespace, (count, parent))| {
                namespace == IndexNamespace::Existential as usize || *count == parent
            })
            .then(|| {
                self.counts[IndexNamespace::Existential as usize]
                    .checked_sub(parent.counts[IndexNamespace::Existential as usize])
            })
            .flatten()
    }

    pub fn canonical_existentials<'db, I: IntoIterator<Item = IndexExpr<'db>>>(
        &self,
        parent: &Self,
        used: impl FnOnce() -> I,
    ) -> IndexSubst<'db> {
        let existentials = self
            .existential_extension_of(parent)
            .expect("clause scope must extend its owner");
        // Only clause-local binders are renamed, so only they need their uses.
        let used: BTreeSet<_> = if existentials == 0 {
            BTreeSet::new()
        } else {
            used().into_iter().collect()
        };
        let mut destination = parent.clone();
        let entries = self
            .variables()
            .filter(|index| parent.validate(*index).is_err())
            .map(|index| {
                let target = if used.contains(&index) {
                    let (scope, target) = destination.bind(IndexNamespace::Existential);
                    destination = scope;
                    target
                } else {
                    // This variable occurs in neither the guard nor the payload.
                    IndexExpr::Const(0)
                };
                (index, target)
            })
            .collect::<Vec<_>>();
        IndexSubst::new(self, &destination, entries).expect("canonical clause binders")
    }

    /// Open a clause with fresh witnesses while preserving all surrounding
    /// lexical variables. Independent clauses never share an existential by ID.
    pub fn open_existentials<'db>(&self, parent: &Self, destination: &Self) -> IndexSubst<'db> {
        self.existential_extension_of(parent)
            .expect("clause scope must extend its owner");
        let mut destination = destination.clone();
        let entries = self
            .variables()
            .filter(|index| parent.validate(*index).is_err())
            .map(|index| {
                let (scope, target) = destination.bind(IndexNamespace::Existential);
                destination = scope;
                (index, target)
            })
            .collect::<Vec<_>>();
        IndexSubst::new(self, &destination, entries).expect("fresh clause witnesses")
    }

    pub fn validate<'db>(&self, index: IndexExpr<'db>) -> Result<(), IndexError<'db>> {
        if let IndexExpr::Bound(bound) = index
            && bound.level >= self.counts[bound.namespace as usize]
        {
            return Err(IndexError::FreeBinder(index));
        }
        Ok(())
    }

    pub(crate) fn variables<'db>(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexSubst<'db> {
    source: BinderScope,
    destination: BinderScope,
    entries: BTreeMap<IndexExpr<'db>, IndexExpr<'db>>,
}

impl<'db> IndexSubst<'db> {
    pub fn new(
        source: &BinderScope,
        destination: &BinderScope,
        entries: impl IntoIterator<Item = (IndexExpr<'db>, IndexExpr<'db>)>,
    ) -> Result<Self, IndexError<'db>> {
        let mut map = BTreeMap::new();
        for (from, to) in entries {
            source.validate(from)?;
            destination.validate(to)?;
            if matches!(from, IndexExpr::Const(_)) {
                return Err(IndexError::ConstantSubstitution);
            }
            if matches!(from, IndexExpr::TypeConst(_))
                && !matches!(to, IndexExpr::Const(_) | IndexExpr::TypeConst(_))
            {
                return Err(IndexError::InvalidConstSubstitution);
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

    pub fn is_identity(&self) -> bool {
        self.source == self.destination && self.entries.is_empty()
    }

    pub fn apply(&self, index: IndexExpr<'db>) -> IndexExpr<'db> {
        self.entries.get(&index).copied().unwrap_or(index)
    }

    pub fn source(&self) -> &BinderScope {
        &self.source
    }
    pub fn destination(&self) -> &BinderScope {
        &self.destination
    }

    pub fn preserves_indices(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn then(&self, next: &Self) -> Result<Self, IndexError<'db>> {
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

    pub fn under_existentials(&self, scope: &BinderScope) -> Self {
        let count = scope
            .existential_extension_of(&self.source)
            .expect("owned existential scope");
        (0..count).fold(self.clone(), |substitution, _| {
            substitution.under_binder(IndexNamespace::Existential)
        })
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

impl<'db> From<usize> for IndexExpr<'db> {
    fn from(value: usize) -> Self {
        Self::Const(value)
    }
}

// Type-level expressions are atomic identities in this domain. Instantiation
// supplies a substitution for each complete expression, including derived bounds.
impl<'db> TyFolder<'db> for IndexSubst<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        // Capability types can retain deferred extents even though shape lengths
        // already carry canonical atoms. Look up the same specialization key.
        let normalized = normalize_const_tys_for_comparison(db, ty);
        if let TyData::ConstTy(value) = normalized.data(db) {
            let key = IndexExpr::TypeConst(*value);
            let replacement = self.apply(key);
            if replacement == key {
                return ty;
            }
            return match replacement {
                IndexExpr::Const(integer) => TyId::const_ty(
                    db,
                    const_ty_from_sem_const(db, int_const(db, value.ty(db), BigInt::from(integer))),
                ),
                IndexExpr::TypeConst(value) => TyId::const_ty(db, value),
                _ => unreachable!("checked const substitution"),
            };
        }
        ty.super_fold_with(db, self)
    }
}
