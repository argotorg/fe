//! External referents retain every load-and-dereference transition.
use std::collections::BTreeMap;

use crate::analysis::HirAnalysisDb;

use super::{
    external::{ExternalOrigin, ExternalSource},
    guard::Guard,
    index::{BinderScope, IndexExpr, IndexSubst},
    path::{RegionPath, StructuralPath},
    region::{RegionRoot, SymbolicPlace, path_alias_guard},
    repack::{ReferentRepackId, ReferentViews, RepackPayload},
    semantics::CapabilityClass,
    value::IndexPayload,
};

/// A structural result's source retains typed storage identity and conversion anchors.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceExpr<'db> {
    pub source: ExternalSource<'db>,
    pub path: RegionPath<IndexExpr<'db>>,
    pub views: ReferentViews<'db>,
}

impl<'db> SourceExpr<'db> {
    pub fn from_place(place: &SymbolicPlace<'db>) -> Option<Self> {
        let RegionRoot::External(source) = &place.root else {
            return None;
        };
        if matches!(source.origin, ExternalOrigin::Local(_)) {
            return None;
        }
        Some(Self {
            source: source.clone(),
            path: place.path.clone(),
            views: place.views.clone(),
        })
    }
}

impl<'db> IndexPayload<'db> for SourceExpr<'db> {
    fn accepts_class(&self, class: CapabilityClass) -> bool {
        matches!(
            class,
            CapabilityClass::Borrow(_) | CapabilityClass::View | CapabilityClass::Handle
        )
    }

    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> {
        self.source.indices().chain(self.path.indices())
    }

    fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self {
            source: self.source.substitute(db, subst),
            path: self.path.substitute(subst),
            views: self.views.substitute(db, subst),
        }
    }
}

impl<'db> RepackPayload<'db> for SourceExpr<'db> {
    fn repack_referent(&self, db: &'db dyn HirAnalysisDb, repack: ReferentRepackId<'db>) -> Self {
        let mut source = self.clone();
        source
            .views
            .append(db, source.path.as_slice().len(), repack);
        source
    }
}

/// The first region is either an input place or the target of a capability slot
/// in an input value. Following another stored capability is a separate step.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InputOrigin<'db> {
    Place(u32),
    Slot {
        param: u32,
        slot: StructuralPath<IndexExpr<'db>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InputSource<'db> {
    origin: InputOrigin<'db>,
    dereferences: Box<[RegionPath<IndexExpr<'db>>]>,
    reachable: bool,
}

impl<'db> InputSource<'db> {
    pub const MAX_DEREFERENCES: usize = 16;

    pub fn place(param: u32) -> Self {
        Self {
            origin: InputOrigin::Place(param),
            dereferences: Box::new([]),
            reachable: false,
        }
    }

    pub fn slot(param: u32, slot: StructuralPath<IndexExpr<'db>>) -> Self {
        Self {
            origin: InputOrigin::Slot { param, slot },
            dereferences: Box::new([]),
            reachable: false,
        }
    }

    /// An explicit conservative source, used by signature summaries and widening.
    pub fn reachable(param: u32) -> Self {
        Self {
            origin: InputOrigin::Place(param),
            dereferences: Box::new([]),
            reachable: true,
        }
    }

    pub fn origin(&self) -> &InputOrigin<'db> {
        &self.origin
    }
    pub fn dereferences(&self) -> &[RegionPath<IndexExpr<'db>>] {
        &self.dereferences
    }
    pub fn is_reachable(&self) -> bool {
        self.reachable
    }
    pub fn param(&self) -> u32 {
        match self.origin {
            InputOrigin::Place(param) | InputOrigin::Slot { param, .. } => param,
        }
    }

    /// `path` locates the next stored capability inside the current referent.
    /// This is not an ordinary projection: the result denotes its target.
    pub fn follow(&self, path: RegionPath<IndexExpr<'db>>) -> Self {
        if self.reachable || self.dereferences.len() == Self::MAX_DEREFERENCES {
            return Self::reachable(self.param());
        }
        let mut dereferences = self.dereferences.to_vec();
        dereferences.push(path);
        Self {
            origin: self.origin.clone(),
            dereferences: dereferences.into(),
            reachable: false,
        }
    }

    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        let slot = match &self.origin {
            InputOrigin::Slot { slot, .. } => Some(slot),
            InputOrigin::Place(_) => None,
        };
        slot.into_iter()
            .flat_map(StructuralPath::indices)
            .chain(self.dereferences.iter().flat_map(RegionPath::indices))
    }

    pub fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        let origin = match &self.origin {
            InputOrigin::Place(param) => InputOrigin::Place(*param),
            InputOrigin::Slot { param, slot } => InputOrigin::Slot {
                param: *param,
                slot: slot.substitute(subst),
            },
        };
        Self {
            origin,
            dereferences: self
                .dereferences
                .iter()
                .map(|path| path.substitute(subst))
                .collect(),
            reachable: self.reachable,
        }
    }

    /// Match one occurrence against an inventoried family. Family binders are
    /// instantiated; repeated indices and constant selectors remain guard facts.
    /// A reachable source is an overapproximation, never a typed storage cell.
    pub fn match_instance(
        &self,
        scope: &BinderScope,
        instance: &Self,
        instance_scope: &BinderScope,
    ) -> Option<(IndexSubst<'db>, Guard<'db>)> {
        if self.reachable || instance.reachable {
            return None;
        }
        let mut bindings = BTreeMap::new();
        for (formal, actual) in self.indices().zip(instance.indices()) {
            if matches!(formal, IndexExpr::Bound(_)) {
                bindings.entry(formal).or_insert(actual);
            }
        }
        let substitution = IndexSubst::new(scope, instance_scope, bindings).ok()?;
        let guard = self.substitute(&substitution).alias_guard(
            instance,
            Guard::always(instance_scope),
            false,
        )?;
        Some((substitution, guard))
    }

    pub(super) fn alias_guard(
        &self,
        other: &Self,
        mut guard: Guard<'db>,
        allow_unknown: bool,
    ) -> Option<Guard<'db>> {
        if self.param() != other.param() {
            return None;
        }
        if self.reachable || other.reachable {
            return allow_unknown.then_some(guard);
        }
        if self.dereferences.len() != other.dereferences.len() {
            return None;
        }
        match (&self.origin, &other.origin) {
            (InputOrigin::Place(_), InputOrigin::Place(_)) => {}
            (InputOrigin::Slot { slot: left, .. }, InputOrigin::Slot { slot: right, .. })
                if left.as_slice().len() == right.as_slice().len() =>
            {
                guard = path_alias_guard(left.as_slice(), right.as_slice(), guard, false)?;
            }
            _ => return None,
        }
        for (left, right) in self.dereferences.iter().zip(&other.dereferences) {
            if left.as_slice().len() != right.as_slice().len() {
                return None;
            }
            guard = path_alias_guard(left.as_slice(), right.as_slice(), guard, false)?;
        }
        Some(guard)
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::semantic::capability::test_roots;
    use crate::test_db::HirAnalysisTestDb;

    use super::*;
    use crate::analysis::semantic::{
        FieldIndex,
        capability::{
            index::{BinderScope, IndexNamespace},
            path::Projection,
            region::{OverlapResult, RegionRoot, RegionSet},
        },
        normalized::NValueId,
    };

    fn field<'db>(field: u16) -> RegionPath<IndexExpr<'db>> {
        RegionPath::new([Projection::Field(FieldIndex(field))])
    }

    fn region<'db>(
        db: &'db HirAnalysisTestDb,
        source: InputSource<'db>,
        path: RegionPath<IndexExpr<'db>>,
    ) -> RegionSet<'db> {
        RegionSet::singleton(
            &BinderScope::default(),
            RegionRoot::External(test_roots::input(db, source)),
            path,
        )
    }

    #[test]
    fn stored_handle_slots_are_distinct_from_each_followed_referent() {
        let db = HirAnalysisTestDb::default();
        let outer = InputSource::slot(0, StructuralPath::default());
        let inner = outer.follow(field(0));
        let value = inner.follow(field(1));
        let regions = [
            region(&db, outer.clone(), field(0)),
            region(&db, inner.clone(), field(1)),
            region(&db, value, RegionPath::default()),
            region(
                &db,
                outer.follow(field(0).concat(&field(1))),
                RegionPath::default(),
            ),
            region(&db, InputSource::place(0), field(0)),
        ];
        for (index, left) in regions.iter().enumerate() {
            assert!(left.provably_covers(left));
            for right in regions.iter().skip(index + 1) {
                assert_ne!(left, right);
                assert_eq!(left.overlap(right), OverlapResult::Disjoint);
            }
        }
    }

    #[test]
    fn substitution_reaches_every_slot_and_dereference_without_capturing_indices() {
        let (scope, binder) = BinderScope::default().bind(IndexNamespace::Result);
        let formal = IndexExpr::FormalValue(1);
        let runtime = IndexExpr::Runtime(NValueId::from_u32(8));
        let source = InputSource::slot(0, StructuralPath::new([Projection::Index(binder)]))
            .follow(RegionPath::new([Projection::Index(formal)]))
            .follow(RegionPath::new([Projection::Index(runtime)]));
        let subst = IndexSubst::new(
            &scope,
            &BinderScope::default(),
            [
                (binder, IndexExpr::Const(2)),
                (formal, runtime),
                (runtime, IndexExpr::Const(7)),
            ],
        )
        .unwrap();
        let instantiated = source.substitute(&subst);
        assert_eq!(
            instantiated.indices().collect::<Vec<_>>(),
            vec![IndexExpr::Const(2), runtime, IndexExpr::Const(7)]
        );
        let expected = InputSource::slot(
            0,
            StructuralPath::new([Projection::Index(IndexExpr::Const(2))]),
        )
        .follow(RegionPath::new([Projection::Index(runtime)]))
        .follow(RegionPath::new([Projection::Index(IndexExpr::Const(7))]));
        assert_eq!(instantiated, expected);
    }

    #[test]
    fn all_dereference_indices_share_the_region_guard_solver() {
        let db = HirAnalysisTestDb::default();
        let left_index = IndexExpr::Runtime(NValueId::from_u32(0));
        let right_index = IndexExpr::Runtime(NValueId::from_u32(1));
        let source = |first, second| {
            InputSource::slot(0, StructuralPath::new([Projection::Index(first)]))
                .follow(RegionPath::new([Projection::Index(second)]))
        };
        let left = region(&db, source(left_index, right_index), RegionPath::default());
        let right = region(
            &db,
            source(right_index, IndexExpr::Const(0)),
            RegionPath::default(),
        );
        let disjoint = Guard::always(&BinderScope::default())
            .with_disequality(left_index, IndexExpr::Const(0))
            .unwrap();
        assert!(left.with_guard(&disjoint).intersection(&right).is_empty());
        let overlap = left.intersection(&right);
        assert!(!overlap.is_empty());
        assert!(
            overlap
                .clauses()
                .iter()
                .all(|clause| clause.guard.proves_equal(left_index, IndexExpr::Const(0)))
        );
    }

    #[test]
    fn recursive_sources_widen_without_inventing_coverage_or_disjointness() {
        let db = HirAnalysisTestDb::default();
        let mut source = InputSource::slot(0, StructuralPath::default());
        let mut descendants = vec![source.clone()];
        for _ in 0..=InputSource::MAX_DEREFERENCES {
            source = source.follow(field(0));
            descendants.push(source.clone());
        }
        assert_eq!(source, InputSource::reachable(0));
        assert_eq!(source.follow(field(7)), source);
        let widened = region(&db, source, field(7));
        assert_eq!(widened.clauses()[0].payload.path, field(7));
        for descendant in descendants {
            let exact = region(&db, descendant, field(3));
            assert_eq!(widened.overlap(&exact), OverlapResult::Unknown);
            assert!(!widened.provably_covers(&exact));
            assert!(!exact.provably_covers(&widened));
            assert_eq!(exact.remove_covered(&widened), exact);
        }
        assert_eq!(
            widened.overlap(&region(&db, InputSource::place(1), field(0))),
            OverlapResult::Unknown
        );
    }
}
