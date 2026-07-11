use cranelift_entity::SecondaryMap;
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SBlockId, SLocalId, SemOrigin, SemanticInstance},
        ty::{
            provider::{ProviderAddressSpace, ProviderKind},
            ty_def::BorrowKind,
        },
    },
    projection::Aliasing,
};

use super::{
    diagnostics::normalized_body_internal_diag,
    ir::{
        NBorrowRoot, NBorrowRootId, NExpr, NSPlace, NSPlaceRoot, NSProjectionPath, NSStmt,
        NSStmtKind, NormalizedBindingLowering, NormalizedSemanticBody, SemanticBorrowDiagnostic,
    },
};

pub(super) fn address_space_for_borrow_root<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    root: &BorrowRoot<'db>,
    origin: SemOrigin<'db>,
) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
    match root {
        BorrowRoot::Param(_) | BorrowRoot::Local(_) => Ok(ProviderAddressSpace::Memory),
        BorrowRoot::Provider(binding) => match binding.semantics.address_space {
            Some(space) => Ok(space),
            None if matches!(binding.semantics.kind, ProviderKind::RootObject) => {
                Ok(ProviderAddressSpace::Memory)
            }
            None => Err(normalized_body_internal_diag(
                db,
                instance,
                body,
                origin,
                format!(
                    "provider `{}` has no address space",
                    binding.provider_ty.pretty_print(db)
                ),
            )),
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct LoanId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum BorrowRoot<'db> {
    Param(u32),
    Local(SLocalId),
    Provider(crate::semantic::ProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct CanonPlace<'db> {
    pub(super) root: BorrowRoot<'db>,
    pub(super) proj: NSProjectionPath<'db>,
}

#[derive(Clone, Debug)]
pub(super) struct Loan<'db> {
    pub(super) kind: BorrowKind,
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) parents: FxHashSet<LoanId>,
    pub(super) origin: SemOrigin<'db>,
}

#[derive(Clone, Debug)]
pub(super) struct MoveSite<'db> {
    pub(super) origin: SemOrigin<'db>,
    pub(super) note: String,
}

pub(super) type MovedPlaces<'db> = FxHashMap<CanonPlace<'db>, MoveSite<'db>>;
pub(super) type BlockAdjacency = SmallVec<SBlockId, 2>;
pub(super) type CfgAdjacency = SecondaryMap<SBlockId, BlockAdjacency>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct State {
    pub(super) local_loans: FxHashMap<SLocalId, FxHashSet<LoanId>>,
}

impl State {
    pub(super) fn loans_in(&self, local: SLocalId) -> FxHashSet<LoanId> {
        self.local_loans.get(&local).cloned().unwrap_or_default()
    }

    pub(super) fn assign_loans(&mut self, local: SLocalId, loans: FxHashSet<LoanId>) {
        if loans.is_empty() {
            self.local_loans.remove(&local);
        } else {
            self.local_loans.insert(local, loans);
        }
    }
}

impl JoinSemiLattice for State {
    fn join_into(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (local, loans) in &other.local_loans {
            let entry = self.local_loans.entry(*local).or_default();
            let before = entry.len();
            entry.extend(loans.iter().copied());
            changed |= before != entry.len();
        }
        changed
    }
}

pub(super) struct BorrowCanonCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    loans: &'a [Loan<'db>],
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        loans: &'a [Loan<'db>],
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            loans,
            loan_for_local,
        }
    }

    pub(super) fn apply_stmt_state(&self, state: &mut State, stmt: &NSStmt<'db>) {
        let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
            return;
        };
        let loans = match expr {
            NExpr::Use(src) => {
                let loans = state.loans_in(src.local);
                if loans.is_empty() {
                    self.loan_for_local
                        .get(dst)
                        .copied()
                        .map(|loan| FxHashSet::from_iter([loan]))
                        .unwrap_or_default()
                } else {
                    loans
                }
            }
            NExpr::Borrow { .. } | NExpr::Call { .. } => self
                .loan_for_local
                .get(dst)
                .copied()
                .map(|loan| FxHashSet::from_iter([loan]))
                .unwrap_or_default(),
            _ => FxHashSet::default(),
        };
        state.assign_loans(*dst, loans);
    }

    pub(super) fn canonicalize_value_base(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        if self
            .body
            .local(local)
            .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
        {
            return self.borrow_local_targets(state, local);
        }

        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        let root = match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root))),
            NormalizedBindingLowering::Erased => None,
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => unreachable!(),
        };
        root.into_iter()
            .map(|root| CanonPlace {
                root,
                proj: NSProjectionPath::default(),
            })
            .collect()
    }

    pub(super) fn borrow_local_targets(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        let mut out = FxHashSet::default();
        for loan in state.loans_in(local) {
            out.extend(self.loans[loan.0 as usize].targets.iter().cloned());
        }
        if !out.is_empty() {
            return out;
        }

        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root)))
                .into_iter()
                .map(|root| CanonPlace {
                    root,
                    proj: NSProjectionPath::default(),
                })
                .collect(),
            NormalizedBindingLowering::Erased => FxHashSet::default(),
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => FxHashSet::default(),
        }
    }

    pub(super) fn canonicalize_place(
        &self,
        state: &State,
        place: &NSPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let out = self.canonicalize_place_targets(state, place);
        if out.is_empty() {
            return Err(self.internal_diag(
                origin,
                "cannot canonicalize carrier-rooted place".to_string(),
            ));
        }
        Ok(out)
    }

    fn canonicalize_place_targets(
        &self,
        state: &State,
        place: &NSPlace<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        match place.root {
            NSPlaceRoot::Root(root) => FxHashSet::from_iter([CanonPlace {
                root: self
                    .root_to_borrow_root(root)
                    .expect("normalized borrow root"),
                proj: place.path.clone(),
            }]),
            NSPlaceRoot::CarrierDerefLocal(local) => {
                let suffix = place.path.clone();
                let mut out = FxHashSet::default();
                let mut resolved = false;
                for loan in state.loans_in(local) {
                    resolved = true;
                    for target in &self.loans[loan.0 as usize].targets {
                        out.insert(CanonPlace {
                            root: target.root.clone(),
                            proj: target.proj.concat(&suffix),
                        });
                    }
                }
                if !resolved
                    && let Some(NormalizedBindingLowering::CarrierLocal { root, provider, .. }) =
                        self.body.local(local).map(|local| &local.lowering)
                {
                    if let Some(provider) = provider {
                        out.insert(CanonPlace {
                            root: BorrowRoot::Provider(provider.clone()),
                            proj: suffix.clone(),
                        });
                    } else if let Some(root) = root.and_then(|root| self.root_to_borrow_root(root))
                    {
                        out.insert(CanonPlace { root, proj: suffix });
                    }
                }
                out
            }
        }
    }

    pub(super) fn root_to_borrow_root(&self, root: NBorrowRootId) -> Option<BorrowRoot<'db>> {
        match self.body.root(root)? {
            NBorrowRoot::Param { param_idx, .. } => Some(BorrowRoot::Param(*param_idx)),
            NBorrowRoot::LocalSlot { local } => Some(BorrowRoot::Local(*local)),
            NBorrowRoot::Provider { binding, .. } => Some(BorrowRoot::Provider(binding.clone())),
        }
    }

    pub(super) fn mut_loans_for_place(
        &self,
        state: &State,
        place: &NSPlace<'db>,
    ) -> FxHashSet<LoanId> {
        let active_loans = match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => state.loans_in(local),
            NSPlaceRoot::Root(_) => FxHashSet::default(),
        };
        active_loans
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn mut_loans_for_value(&self, state: &State, local: SLocalId) -> FxHashSet<LoanId> {
        state
            .loans_in(local)
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        normalized_body_internal_diag(self.db, self.instance, self.body, origin, message)
    }
}

pub(super) fn place_set_overlaps<'db>(
    lhs: &FxHashSet<CanonPlace<'db>>,
    rhs: &FxHashSet<CanonPlace<'db>>,
) -> bool {
    lhs.iter()
        .any(|lhs| rhs.iter().any(|rhs| places_overlap(lhs, rhs)))
}

pub(super) fn places_overlap<'db>(lhs: &CanonPlace<'db>, rhs: &CanonPlace<'db>) -> bool {
    lhs.root == rhs.root && !matches!(lhs.proj.may_alias(&rhs.proj), Aliasing::No)
}
