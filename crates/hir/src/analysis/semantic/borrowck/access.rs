use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    semantic::{SLocalId, SemOrigin},
    ty::ty_def::BorrowKind,
};

use super::{
    canon::BorrowCanonCx,
    guard::{Guard, IndexParamId},
    loan::{LoanDef, LoanId, LoanRef},
    region::RegionSet,
    shape::{SlotPath, SlotProjection},
    transfer::BorrowState,
};

#[derive(Clone, Debug)]
pub(super) struct MoveSite<'db> {
    pub(super) origin: SemOrigin<'db>,
    pub(super) note: String,
}

pub(super) type MovedPlaces<'db> = FxHashMap<RegionSet<'db>, MoveSite<'db>>;

pub(super) struct CallAccess<'db> {
    group: usize,
    projection: Option<SlotPath<IndexParamId>>,
    kind: BorrowKind,
    region: RegionSet<'db>,
    origin: SemOrigin<'db>,
}

impl<'db> CallAccess<'db> {
    pub(super) fn new(
        group: usize,
        projection: Option<SlotPath<IndexParamId>>,
        kind: BorrowKind,
        region: RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self {
            group,
            projection,
            kind,
            region,
            origin,
        }
    }

    pub(super) fn conflicts_with(
        &self,
        group: usize,
        projection: Option<&SlotPath<IndexParamId>>,
        kind: BorrowKind,
        region: &RegionSet<'db>,
    ) -> bool {
        (self.group != group
            || self.projection.is_some()
                && projection.is_some()
                && !variant_slots_are_mutually_exclusive(self.projection.as_ref(), projection))
            && !matches!((self.kind, kind), (BorrowKind::Ref, BorrowKind::Ref))
            && self.region.may_overlap(region).is_some()
    }

    pub(super) fn origin(&self) -> SemOrigin<'db> {
        self.origin
    }
}

pub(super) struct ActiveLoan<'db> {
    reference: LoanRef,
    holder_guard: Guard,
    region: RegionSet<'db>,
    suspended: RegionSet<'db>,
}

impl<'db> ActiveLoan<'db> {
    pub(super) fn id(&self) -> LoanId {
        self.reference.id
    }

    pub(super) fn reference(&self) -> &LoanRef {
        &self.reference
    }

    pub(super) fn holder_guard(&self) -> &Guard {
        &self.holder_guard
    }

    pub(super) fn region(&self) -> &RegionSet<'db> {
        &self.region
    }

    pub(super) fn matches(&self, reference: &LoanRef, guard: &Guard) -> Option<Guard> {
        self.holder_guard
            .and(guard)
            .and_then(|guard| self.reference.unify(reference, &guard))
    }

    pub(super) fn overlaps(&self, region: &RegionSet<'db>) -> bool {
        let overlap = self.region.intersection(region);
        !overlap.is_empty() && !self.suspended.provably_covers(&overlap)
    }
}

pub(super) fn active_loans_in<'db>(
    canon: &BorrowCanonCx<'_, 'db>,
    state: &BorrowState<'db>,
    local: SLocalId,
) -> Vec<ActiveLoan<'db>> {
    state
        .leaves_in(local, super::guard::ValueScope::Local(local))
        .into_iter()
        .map(|leaf| ActiveLoan {
            reference: leaf.payload.clone(),
            holder_guard: leaf.guard.clone(),
            region: canon.active_region_for_held(&leaf.payload, &leaf.guard),
            suspended: RegionSet::empty(),
        })
        .collect()
}

pub(super) fn effective_loans<'db>(
    canon: &BorrowCanonCx<'_, 'db>,
    loans: &[LoanDef<'db>],
    state: &BorrowState<'db>,
    live: &FxHashSet<SLocalId>,
) -> Vec<ActiveLoan<'db>> {
    let mut active = state
        .locals()
        .filter(|local| live.contains(local))
        .flat_map(|local| active_loans_in(canon, state, local))
        .collect::<Vec<_>>();
    let mut suspended = vec![RegionSet::empty(); active.len()];
    let mut worklist = active
        .iter()
        .map(|loan| {
            (
                loan.reference.clone(),
                loan.holder_guard.clone(),
                loan.region.clone(),
            )
        })
        .collect::<Vec<_>>();
    let mut seen = FxHashSet::default();
    while let Some((reference, guard, region)) = worklist.pop() {
        if !seen.insert((reference.clone(), guard.clone(), region.clone())) {
            continue;
        }
        let parents = loans[reference.id.0 as usize].instantiate_parents(&reference, &guard);
        for parent in parents.iter() {
            let parent_region = region.with_guard(parent.guard());
            if parent_region.is_empty() {
                continue;
            }
            for (idx, active_parent) in active.iter().enumerate() {
                let Some(match_guard) = active_parent.matches(parent.reference(), parent.guard())
                else {
                    continue;
                };
                let matched = parent_region.with_guard(&match_guard);
                let joined = suspended[idx].union(&matched);
                if joined != suspended[idx] {
                    suspended[idx] = joined;
                }
            }
            worklist.push((
                parent.reference().clone(),
                parent.guard().clone(),
                parent_region,
            ));
        }
    }
    for (loan, suspended) in active.iter_mut().zip(suspended) {
        loan.suspended = suspended;
    }
    active.sort_by_key(|loan| loan.id().0);
    active
}

fn variant_slots_are_mutually_exclusive(
    lhs: Option<&SlotPath<IndexParamId>>,
    rhs: Option<&SlotPath<IndexParamId>>,
) -> bool {
    let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
        return false;
    };
    let Some((lhs, rhs)) = lhs
        .as_slice()
        .iter()
        .zip(rhs.as_slice())
        .find(|(lhs, rhs)| lhs != rhs)
    else {
        return false;
    };
    matches!(
        (lhs, rhs),
        (
            SlotProjection::VariantField { variant: lhs, .. },
            SlotProjection::VariantField { variant: rhs, .. }
        ) if lhs != rhs
    )
}
