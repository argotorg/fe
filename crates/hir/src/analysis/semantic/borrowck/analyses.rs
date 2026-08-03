use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{BackwardCfgAnalysis, ForwardCfgAnalysis, JoinSemiLattice, SparseAnalysis};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SBlockId, SLocalId, SStmtId,
        borrowck::ir::{NExpr, NSStmtKind},
    },
};

use super::{
    canon::{
        BorrowCanonCx, CanonPlace, CfgAdjacency, Loan, LoanId, MovedPlaces, State,
        indexed_target_is_excluded,
    },
    check::Borrowck,
    ir::{BorrowResult, BorrowTransform, NormalizedSemanticBody, SemanticBorrowDiagnostic},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BorrowSummaryMode {
    FinalCheck,
    FinalSummary,
    Provisional,
}

pub(super) struct BorrowLoanTargetState<'a, 'db> {
    pub(super) loans: &'a mut [Loan<'db>],
}

pub(super) struct BorrowLoanTargetAnalysis<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedSemanticBody<'db>,
    entry_state: &'a SecondaryMap<SBlockId, State>,
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
    call_result_loans: &'a FxHashMap<SStmtId, Vec<(BorrowResult, LoanId)>>,
    call_loan_transforms: &'a FxHashMap<LoanId, Vec<BorrowTransform>>,
}

impl<'a, 'db> BorrowLoanTargetAnalysis<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        body: &'a NormalizedSemanticBody<'db>,
        entry_state: &'a SecondaryMap<SBlockId, State>,
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
        call_result_loans: &'a FxHashMap<SStmtId, Vec<(BorrowResult, LoanId)>>,
        call_loan_transforms: &'a FxHashMap<LoanId, Vec<BorrowTransform>>,
    ) -> Self {
        Self {
            db,
            body,
            entry_state,
            loan_for_local,
            constant_indices,
            call_result_loans,
            call_loan_transforms,
        }
    }

    fn canon<'b>(&'b self, loans: &'b [Loan<'db>]) -> BorrowCanonCx<'b, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.body.owner,
            self.body,
            loans,
            self.loan_for_local,
            self.constant_indices,
        )
    }

    fn extend_loan(
        &self,
        loans: &mut [Loan<'db>],
        loan_id: LoanId,
        targets: FxHashSet<CanonPlace<'db>>,
        unconditional_targets: FxHashSet<CanonPlace<'db>>,
        mut indexed_targets: Vec<super::canon::IndexedLoanTarget<'db>>,
        parents: FxHashSet<LoanId>,
    ) -> bool {
        let loan = &mut loans[loan_id.0 as usize];
        let has_structured_targets =
            !unconditional_targets.is_empty() || !indexed_targets.is_empty();
        indexed_targets
            .retain(|indexed| !indexed_target_is_excluded(indexed, &loan.result_exclusions));
        let targets = if has_structured_targets {
            unconditional_targets
                .iter()
                .cloned()
                .chain(indexed_targets.iter().map(|indexed| indexed.target.clone()))
                .collect()
        } else {
            targets
        };
        let before_targets = loan.targets.len();
        let before_unconditional_targets = loan.unconditional_targets.len();
        let before_indexed_targets = loan.indexed_targets.len();
        let before_parents = loan.parents.len();
        loan.targets.extend(targets);
        loan.unconditional_targets.extend(unconditional_targets);
        for indexed in indexed_targets {
            if !loan.indexed_targets.contains(&indexed) {
                loan.indexed_targets.push(indexed);
            }
        }
        loan.parents.extend(parents);
        before_targets != loan.targets.len()
            || before_unconditional_targets != loan.unconditional_targets.len()
            || before_indexed_targets != loan.indexed_targets.len()
            || before_parents != loan.parents.len()
    }

    fn update_loan_from_stmt(
        &self,
        loans: &mut [Loan<'db>],
        state: &State,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<bool, SemanticBorrowDiagnostic<'db>> {
        let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
            return Ok(false);
        };
        match expr {
            NExpr::Borrow { place, .. } | NExpr::ReadPlace { place, .. } => {
                let Some(&loan_id) = self.loan_for_local.get(dst) else {
                    return Ok(false);
                };
                let (targets, parents) = {
                    let canon = self.canon(loans);
                    (
                        canon.canonicalize_place(state, place, stmt.origin)?,
                        canon.mut_loans_for_place(state, place),
                    )
                };
                Ok(self.extend_loan(
                    loans,
                    loan_id,
                    targets.clone(),
                    targets,
                    Vec::new(),
                    parents,
                ))
            }
            NExpr::Call { args, .. } => {
                let mut loan_ids = self
                    .loan_for_local
                    .get(dst)
                    .copied()
                    .into_iter()
                    .collect::<Vec<_>>();
                loan_ids.extend(
                    self.call_result_loans
                        .get(&stmt.id)
                        .into_iter()
                        .flatten()
                        .map(|(_, loan)| *loan),
                );
                let mut changed = false;
                for loan_id in loan_ids {
                    let Some(transforms) = self.call_loan_transforms.get(&loan_id) else {
                        continue;
                    };
                    let mut targets = FxHashSet::default();
                    let mut unconditional_targets = FxHashSet::default();
                    let mut indexed_targets = Vec::new();
                    let mut parents = FxHashSet::default();
                    let canon = self.canon(loans);
                    for transform in transforms {
                        let param = transform.input.param();
                        let Some(arg) = args.get(param as usize) else {
                            continue;
                        };
                        let arg_targets = canon.canonicalize_call_input_with_families(
                            state,
                            arg.local,
                            &transform.input,
                        );
                        parents.extend(canon.mut_loans_for_value_targets(
                            state,
                            arg.local,
                            &arg_targets.targets,
                        ));
                        targets.extend(arg_targets.targets);
                        unconditional_targets.extend(arg_targets.unconditional_targets);
                        indexed_targets.extend(arg_targets.indexed_targets);
                    }
                    changed |= self.extend_loan(
                        loans,
                        loan_id,
                        targets,
                        unconditional_targets,
                        indexed_targets,
                        parents,
                    );
                }
                Ok(changed)
            }
            NExpr::Use(value) => {
                let Some(&loan_id) = self.loan_for_local.get(dst) else {
                    return Ok(false);
                };
                let canon = self.canon(loans);
                let targets = canon.borrow_local_targets(state, value.local);
                Ok(self.extend_loan(
                    loans,
                    loan_id,
                    targets.clone(),
                    targets,
                    Vec::new(),
                    canon.mut_loans_for_value(state, value.local),
                ))
            }
            _ => Ok(false),
        }
    }
}

impl<'a, 'db> SparseAnalysis for BorrowLoanTargetAnalysis<'a, 'db> {
    type Node = SBlockId;
    type State = BorrowLoanTargetState<'a, 'db>;
    type Error = SemanticBorrowDiagnostic<'db>;

    fn node_count(&self) -> usize {
        self.body.blocks.len()
    }

    fn seed_nodes(&self) -> Vec<Self::Node> {
        (0..self.body.blocks.len()).map(SBlockId::new).collect()
    }

    fn step(&mut self, node: Self::Node, state: &mut Self::State) -> Result<bool, Self::Error> {
        let mut local_state = self.entry_state[node].clone();
        let mut changed = false;
        for stmt in &self.body.blocks[node.index()].stmts {
            changed |= self.update_loan_from_stmt(&mut *state.loans, &local_state, stmt)?;
            self.canon(state.loans).apply_stmt_state_with_call_loans(
                &mut local_state,
                stmt,
                self.call_result_loans.get(&stmt.id).map(Vec::as_slice),
            );
        }
        Ok(changed)
    }

    fn dependents(&self, _node: Self::Node, out: &mut Vec<Self::Node>) {
        out.extend((0..self.body.blocks.len()).map(SBlockId::new));
    }
}

pub(super) struct BorrowEntryStateAnalysis<'a, 'db> {
    borrowck: &'a Borrowck<'db>,
    successors: CfgAdjacency,
}

impl<'a, 'db> BorrowEntryStateAnalysis<'a, 'db> {
    pub(super) fn new(borrowck: &'a Borrowck<'db>) -> Self {
        Self {
            borrowck,
            successors: borrowck.cfg_successor_indices(),
        }
    }
}

impl ForwardCfgAnalysis for BorrowEntryStateAnalysis<'_, '_> {
    type Block = SBlockId;
    type State = State;
    type Error = Infallible;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        (!self.borrowck.body.blocks.is_empty())
            .then_some(SBlockId::new(0))
            .into_iter()
            .collect()
    }

    fn bottom(&self) -> Self::State {
        State::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        if !self.borrowck.body.blocks.is_empty() {
            let entry = &mut entry_states[SBlockId::new(0)];
            for (&local, held) in &self.borrowck.param_loans_for_local {
                entry.assign_held_loans(local, held.clone());
            }
        }
        Ok(())
    }

    fn transfer(
        &mut self,
        block: Self::Block,
        in_state: &Self::State,
    ) -> Result<Self::State, Self::Error> {
        let mut state = in_state.clone();
        for stmt in &self.borrowck.body.blocks[block.index()].stmts {
            self.borrowck.apply_stmt_state(&mut state, stmt);
        }
        Ok(state)
    }

    fn successors(&self, block: Self::Block) -> &[Self::Block] {
        &self.successors[block]
    }
}

#[derive(Clone, Default)]
pub(super) struct MovedState<'db>(pub(super) MovedPlaces<'db>);

impl JoinSemiLattice for MovedState<'_> {
    fn join_into(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (place, site) in &other.0 {
            changed |= self.0.insert(place.clone(), site.clone()).is_none();
        }
        changed
    }
}

pub(super) struct BorrowMovedStateAnalysis<'a, 'db> {
    borrowck: &'a Borrowck<'db>,
    successors: CfgAdjacency,
}

impl<'a, 'db> BorrowMovedStateAnalysis<'a, 'db> {
    pub(super) fn new(borrowck: &'a Borrowck<'db>) -> Self {
        Self {
            borrowck,
            successors: borrowck.cfg_successor_indices(),
        }
    }
}

impl<'db> ForwardCfgAnalysis for BorrowMovedStateAnalysis<'_, 'db> {
    type Block = SBlockId;
    type State = MovedState<'db>;
    type Error = SemanticBorrowDiagnostic<'db>;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        (!self.borrowck.body.blocks.is_empty())
            .then_some(SBlockId::new(0))
            .into_iter()
            .collect()
    }

    fn bottom(&self) -> Self::State {
        MovedState::default()
    }

    fn transfer(
        &mut self,
        block: Self::Block,
        in_state: &Self::State,
    ) -> Result<Self::State, Self::Error> {
        let mut state = self.borrowck.entry_state[block].clone();
        let mut moved = in_state.0.clone();
        for stmt in &self.borrowck.body.blocks[block.index()].stmts {
            self.borrowck
                .update_moved_for_stmt(&state, &mut moved, stmt)?;
            self.borrowck.apply_stmt_state(&mut state, stmt);
        }
        Ok(MovedState(moved))
    }

    fn successors(&self, block: Self::Block) -> &[Self::Block] {
        &self.successors[block]
    }
}

#[derive(Clone, Default)]
pub(super) struct LiveSet(pub(super) FxHashSet<SLocalId>);

impl JoinSemiLattice for LiveSet {
    fn join_into(&mut self, other: &Self) -> bool {
        let before = self.0.len();
        self.0.extend(other.0.iter().copied());
        before != self.0.len()
    }
}

pub(super) struct BorrowLivenessAnalysis<'a, 'db> {
    borrowck: &'a Borrowck<'db>,
    predecessors: CfgAdjacency,
}

impl<'a, 'db> BorrowLivenessAnalysis<'a, 'db> {
    pub(super) fn new(borrowck: &'a Borrowck<'db>) -> Self {
        Self {
            borrowck,
            predecessors: borrowck.cfg_predecessor_indices(),
        }
    }
}

impl<'db> BackwardCfgAnalysis for BorrowLivenessAnalysis<'_, 'db> {
    type Block = SBlockId;
    type State = LiveSet;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        (0..self.borrowck.body.blocks.len())
            .map(SBlockId::new)
            .collect()
    }

    fn bottom(&self) -> Self::State {
        LiveSet::default()
    }

    fn initialize(&mut self, _exit_states: &mut SecondaryMap<Self::Block, Self::State>) {}

    fn transfer(&mut self, block: Self::Block, out_state: &Self::State) -> Self::State {
        let block_data = &self.borrowck.body.blocks[block.index()];
        let mut live = out_state.0.clone();
        live.extend(self.borrowck.facts.terminator_uses(block));
        for (stmt_idx, _) in block_data.stmts.iter().enumerate().rev() {
            live = self.borrowck.live_before_stmt(block, stmt_idx, &live);
        }
        LiveSet(live)
    }

    fn predecessors(&self, block: Self::Block) -> &[Self::Block] {
        &self.predecessors[block]
    }
}
