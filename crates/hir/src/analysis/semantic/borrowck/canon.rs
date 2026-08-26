use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SemOrigin, SemanticInstance,
        normalized::{
            NBlockId, NDataPath, NDataProjection, NExpr, NIndex, NPlace, NPlaceBase, NRootId,
            NRootKind, NStatement, NStatementKind, NValueDefinition, NValueId, NormalizedBody,
        },
    },
    ty::{provider::ProviderAddressSpace, ty_def::BorrowKind},
};

use super::{diagnostics::normalized_body_internal_diag, ir::SemanticBorrowDiagnostic};

pub(super) fn address_space_for_borrow_root<'db>(
    _db: &'db dyn HirAnalysisDb,
    _instance: SemanticInstance<'db>,
    _body: &NormalizedBody<'db>,
    root: &BorrowRoot<'db>,
    _origin: SemOrigin<'db>,
) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
    Ok(match root {
        BorrowRoot::Param(_) | BorrowRoot::Root(_) | BorrowRoot::Value(_) => {
            ProviderAddressSpace::Memory
        }
        BorrowRoot::Provider(binding) => binding
            .semantics
            .address_space
            .unwrap_or(ProviderAddressSpace::Memory),
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct LoanId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum BorrowRoot<'db> {
    Param(u32),
    Root(NRootId),
    Value(NValueId),
    Provider(crate::semantic::ProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct CanonPlace<'db> {
    pub(super) root: BorrowRoot<'db>,
    pub(super) proj: NDataPath,
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
pub(super) type BlockAdjacency = SmallVec<NBlockId, 2>;
pub(super) type CfgAdjacency = SecondaryMap<NBlockId, BlockAdjacency>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct State {
    pub(super) value_loans: FxHashMap<NValueId, FxHashSet<LoanId>>,
}

impl State {
    pub(super) fn loans_in(&self, value: NValueId) -> FxHashSet<LoanId> {
        self.value_loans.get(&value).cloned().unwrap_or_default()
    }

    pub(super) fn assign_loans(&mut self, value: NValueId, loans: FxHashSet<LoanId>) {
        if loans.is_empty() {
            self.value_loans.remove(&value);
        } else {
            self.value_loans.insert(value, loans);
        }
    }
}

impl JoinSemiLattice for State {
    fn join_into(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (value, loans) in &other.value_loans {
            let entry = self.value_loans.entry(*value).or_default();
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
    body: &'a NormalizedBody<'db>,
    loans: &'a [Loan<'db>],
    loan_for_value: &'a FxHashMap<NValueId, LoanId>,
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedBody<'db>,
        loans: &'a [Loan<'db>],
        loan_for_value: &'a FxHashMap<NValueId, LoanId>,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            loans,
            loan_for_value,
        }
    }

    pub(super) fn apply_statement_state(&self, state: &mut State, statement: &NStatement<'db>) {
        let NStatementKind::Define { result, expr } = &statement.kind else {
            return;
        };
        let loans = match expr {
            NExpr::Forward { src }
            | NExpr::ProjectValue { value: src, .. }
            | NExpr::StructuralRepack { value: src, .. } => {
                let loans = state.loans_in(src.value);
                if loans.is_empty() {
                    self.result_loan(*result)
                } else {
                    loans
                }
            }
            NExpr::Borrow { .. } | NExpr::Call { .. } => self.result_loan(*result),
            _ => FxHashSet::default(),
        };
        state.assign_loans(*result, loans);
    }

    pub(super) fn apply_successor_state(
        &self,
        state: &mut State,
        successor: &crate::analysis::semantic::normalized::NSuccessor,
    ) {
        for (param, arg) in self.body.blocks[successor.block.index()]
            .params
            .iter()
            .zip(successor.args.iter())
        {
            state.assign_loans(*param, state.loans_in(arg.value));
        }
    }

    fn result_loan(&self, value: NValueId) -> FxHashSet<LoanId> {
        self.loan_for_value
            .get(&value)
            .copied()
            .map(|loan| FxHashSet::from_iter([loan]))
            .unwrap_or_default()
    }

    pub(super) fn canonicalize_value_base(
        &self,
        state: &State,
        value: NValueId,
    ) -> FxHashSet<CanonPlace<'db>> {
        if self
            .body
            .value(value)
            .is_some_and(|value| value.ty.as_borrow(self.db).is_some())
        {
            let targets = self.borrow_value_targets(state, value);
            if !targets.is_empty() {
                return targets;
            }
        }
        self.canonicalize_value_definition(state, value)
            .unwrap_or_else(|| {
                FxHashSet::from_iter([CanonPlace {
                    root: BorrowRoot::Value(value),
                    proj: NDataPath::empty(),
                }])
            })
    }

    fn canonicalize_value_definition(
        &self,
        state: &State,
        value: NValueId,
    ) -> Option<FxHashSet<CanonPlace<'db>>> {
        match self.body.value(value)?.definition {
            NValueDefinition::EntryParam { param } => Some(FxHashSet::from_iter([CanonPlace {
                root: BorrowRoot::Param(param),
                proj: NDataPath::empty(),
            }])),
            NValueDefinition::BlockParam { .. } => None,
            NValueDefinition::Statement { block, statement } => {
                let NStatementKind::Define { expr, .. } = &self
                    .body
                    .block(block)?
                    .statements
                    .get(statement as usize)?
                    .kind
                else {
                    return None;
                };
                match expr {
                    NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => {
                        (src.mode != crate::analysis::semantic::normalized::ReadMode::Move)
                            .then(|| self.canonicalize_value_base(state, src.value))
                    }
                    NExpr::ProjectValue {
                        value: source,
                        path,
                    } => (source.mode != crate::analysis::semantic::normalized::ReadMode::Move
                        && self
                            .body
                            .value(value)
                            .is_some_and(|value| value.ty.as_capability(self.db).is_none()))
                    .then(|| {
                        self.canonicalize_value_base(state, source.value)
                            .into_iter()
                            .map(|base| CanonPlace {
                                root: base.root,
                                proj: base.proj.concat(&path.0),
                            })
                            .collect()
                    }),
                    NExpr::Load { place, mode }
                        if *mode != crate::analysis::semantic::normalized::ReadMode::Move
                            && self
                                .body
                                .value(value)
                                .is_some_and(|value| value.ty.as_capability(self.db).is_none()) =>
                    {
                        Some(self.canonicalize_place_targets(state, place))
                    }
                    NExpr::Load { .. } => None,
                    _ => None,
                }
            }
        }
    }

    pub(super) fn borrow_value_targets(
        &self,
        state: &State,
        value: NValueId,
    ) -> FxHashSet<CanonPlace<'db>> {
        let mut targets = FxHashSet::default();
        for loan in state.loans_in(value) {
            targets.extend(self.loans[loan.0 as usize].targets.iter().cloned());
        }
        targets
    }

    pub(super) fn canonicalize_place(
        &self,
        state: &State,
        place: &NPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let targets = self.canonicalize_place_targets(state, place);
        if targets.is_empty() {
            return Err(self.internal_diag(
                origin,
                "cannot canonicalize capability-target place".to_string(),
            ));
        }
        Ok(targets)
    }

    fn canonicalize_place_targets(
        &self,
        state: &State,
        place: &NPlace<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        match place.base {
            NPlaceBase::Root(root) => self
                .root_to_borrow_root(root)
                .map(|root| {
                    FxHashSet::from_iter([CanonPlace {
                        root,
                        proj: place.path.clone(),
                    }])
                })
                .unwrap_or_default(),
            NPlaceBase::CapabilityTarget { carrier } => self
                .canonicalize_value_base(state, carrier)
                .into_iter()
                .map(|target| CanonPlace {
                    root: target.root,
                    proj: target.proj.concat(&place.path),
                })
                .collect(),
        }
    }

    pub(super) fn root_to_borrow_root(&self, root: NRootId) -> Option<BorrowRoot<'db>> {
        match &self.body.root(root)?.kind {
            NRootKind::ParamPlace { param } => Some(BorrowRoot::Param(*param)),
            NRootKind::LocalSlot { .. } => Some(BorrowRoot::Root(root)),
            NRootKind::Provider { binding } => Some(BorrowRoot::Provider(binding.clone())),
            NRootKind::CapabilityRepresentation { carrier } => Some(BorrowRoot::Value(*carrier)),
        }
    }

    pub(super) fn mut_loans_for_place(
        &self,
        state: &State,
        place: &NPlace<'db>,
    ) -> FxHashSet<LoanId> {
        let active = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => state.loans_in(carrier),
            NPlaceBase::Root(_) => FxHashSet::default(),
        };
        active
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn mut_loans_for_value(&self, state: &State, value: NValueId) -> FxHashSet<LoanId> {
        state
            .loans_in(value)
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

pub(super) fn places_overlap(lhs: &CanonPlace<'_>, rhs: &CanonPlace<'_>) -> bool {
    lhs.root == rhs.root && paths_may_alias(&lhs.proj, &rhs.proj)
}

fn paths_may_alias(lhs: &NDataPath, rhs: &NDataPath) -> bool {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        match (lhs, rhs) {
            (NDataProjection::Field(lhs), NDataProjection::Field(rhs)) if lhs != rhs => {
                return false;
            }
            (
                NDataProjection::VariantField {
                    variant: lhs_variant,
                    field: lhs_field,
                },
                NDataProjection::VariantField {
                    variant: rhs_variant,
                    field: rhs_field,
                },
            ) if lhs_variant != rhs_variant || lhs_field != rhs_field => return false,
            (
                NDataProjection::Index(NIndex::Const(lhs)),
                NDataProjection::Index(NIndex::Const(rhs)),
            ) if lhs != rhs => return false,
            _ => {}
        }
    }
    true
}
