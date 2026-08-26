use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{BackwardCfgAnalysis, ForwardCfgAnalysis, JoinSemiLattice, SparseAnalysis};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SemanticInstance, get_or_build_semantic_instance,
        normalized::{NBlockId, NExpr, NStatement, NStatementKind, NValueId, NormalizedBody},
    },
};

use super::{
    canon::{BorrowCanonCx, CanonPlace, CfgAdjacency, Loan, LoanId, MovedPlaces, State},
    check::{Borrowck, provisional_borrow_summary_voucher, semantic_borrow_summary_voucher},
    ir::{BlockedSemanticBody, BorrowInputRef, SemanticBorrowDiagnostic},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BorrowSummaryMode {
    Final,
    Provisional,
}

pub(super) struct BorrowLoanTargetState<'a, 'db> {
    pub(super) loans: &'a mut [Loan<'db>],
}

pub(super) struct BorrowLoanTargetAnalysis<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedBody<'db>,
    entry_state: &'a SecondaryMap<NBlockId, State>,
    loan_for_value: &'a FxHashMap<NValueId, LoanId>,
    summary_mode: BorrowSummaryMode,
    blocked: Option<BlockedSemanticBody<'db>>,
}

impl<'a, 'db> BorrowLoanTargetAnalysis<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedBody<'db>,
        entry_state: &'a SecondaryMap<NBlockId, State>,
        loan_for_value: &'a FxHashMap<NValueId, LoanId>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            entry_state,
            loan_for_value,
            summary_mode,
            blocked: None,
        }
    }

    pub(super) fn blocked_body(&self) -> Option<BlockedSemanticBody<'db>> {
        self.blocked.clone()
    }

    fn canon<'b>(&'b self, loans: &'b [Loan<'db>]) -> BorrowCanonCx<'b, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.instance,
            self.body,
            loans,
            self.loan_for_value,
        )
    }

    fn extend_loan(
        &self,
        loans: &mut [Loan<'db>],
        loan_id: LoanId,
        targets: FxHashSet<CanonPlace<'db>>,
        parents: FxHashSet<LoanId>,
    ) -> bool {
        let loan = &mut loans[loan_id.0 as usize];
        let before_targets = loan.targets.len();
        let before_parents = loan.parents.len();
        loan.targets.extend(targets);
        loan.parents.extend(parents);
        before_targets != loan.targets.len() || before_parents != loan.parents.len()
    }

    fn update_loan_from_statement(
        &mut self,
        loans: &mut [Loan<'db>],
        state: &State,
        statement: &NStatement<'db>,
    ) -> Result<bool, SemanticBorrowDiagnostic<'db>> {
        let NStatementKind::Define { result, expr } = &statement.kind else {
            return Ok(false);
        };
        let Some(&loan_id) = self.loan_for_value.get(result) else {
            return Ok(false);
        };
        match expr {
            NExpr::Borrow { place, .. } => {
                let (targets, parents) = {
                    let canon = self.canon(loans);
                    (
                        canon.canonicalize_place(state, place, statement.origin)?,
                        canon.mut_loans_for_place(state, place),
                    )
                };
                Ok(self.extend_loan(loans, loan_id, targets, parents))
            }
            NExpr::Call { callee, args, .. } => {
                let callee_instance = get_or_build_semantic_instance(self.db, callee.key);
                let voucher = match self.summary_mode {
                    BorrowSummaryMode::Final => {
                        semantic_borrow_summary_voucher(self.db, callee_instance)
                    }
                    BorrowSummaryMode::Provisional => {
                        provisional_borrow_summary_voucher(self.db, callee_instance)
                    }
                }?;
                if self.blocked.is_none() {
                    self.blocked = voucher.blocked;
                }
                let summary = voucher.summary;
                let Some(summary) = summary else {
                    return Ok(false);
                };
                let (targets, parents) = {
                    let canon = self.canon(loans);
                    let mut targets = FxHashSet::default();
                    let mut parents = FxHashSet::default();
                    for transform in &summary {
                        let BorrowInputRef::Param(idx) = transform.input;
                        if let Some(arg) = args.get(idx as usize) {
                            for base in canon.canonicalize_value_base(state, arg.value) {
                                targets.insert(CanonPlace {
                                    root: base.root,
                                    proj: base.proj.concat(&transform.proj),
                                });
                            }
                            parents.extend(canon.mut_loans_for_value(state, arg.value));
                        }
                    }
                    (targets, parents)
                };
                Ok(self.extend_loan(loans, loan_id, targets, parents))
            }
            NExpr::ProjectValue { value, path } => {
                let canon = self.canon(loans);
                let targets = canon
                    .canonicalize_value_base(state, value.value)
                    .into_iter()
                    .map(|base| CanonPlace {
                        root: base.root,
                        proj: base.proj.concat(&path.0),
                    })
                    .collect();
                Ok(self.extend_loan(
                    loans,
                    loan_id,
                    targets,
                    canon.mut_loans_for_value(state, value.value),
                ))
            }
            _ => Ok(false),
        }
    }
}

impl<'a, 'db> SparseAnalysis for BorrowLoanTargetAnalysis<'a, 'db> {
    type Node = NBlockId;
    type State = BorrowLoanTargetState<'a, 'db>;
    type Error = SemanticBorrowDiagnostic<'db>;

    fn node_count(&self) -> usize {
        self.body.blocks.len()
    }

    fn seed_nodes(&self) -> Vec<Self::Node> {
        (0..self.body.blocks.len()).map(NBlockId::new).collect()
    }

    fn step(&mut self, node: Self::Node, state: &mut Self::State) -> Result<bool, Self::Error> {
        let mut local_state = self.entry_state[node].clone();
        let mut changed = false;
        for statement in &self.body.blocks[node.index()].statements {
            changed |=
                self.update_loan_from_statement(&mut *state.loans, &local_state, statement)?;
            self.canon(state.loans)
                .apply_statement_state(&mut local_state, statement);
        }
        Ok(changed)
    }

    fn dependents(&self, _node: Self::Node, out: &mut Vec<Self::Node>) {
        out.extend((0..self.body.blocks.len()).map(NBlockId::new));
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
    type Block = NBlockId;
    type State = State;
    type Error = Infallible;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        vec![self.borrowck.body.entry]
    }

    fn bottom(&self) -> Self::State {
        State::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        let entry = &mut entry_states[self.borrowck.body.entry];
        for (&value, &loan) in &self.borrowck.param_loan_for_value {
            entry.assign_loans(value, FxHashSet::from_iter([loan]));
        }
        Ok(())
    }

    fn transfer(
        &mut self,
        block: Self::Block,
        in_state: &Self::State,
    ) -> Result<Self::State, Self::Error> {
        let mut state = in_state.clone();
        let block_data = &self.borrowck.body.blocks[block.index()];
        for statement in &block_data.statements {
            self.borrowck
                .canon()
                .apply_statement_state(&mut state, statement);
        }
        for successor in block_data.terminator.kind.successors() {
            self.borrowck
                .canon()
                .apply_successor_state(&mut state, successor);
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
    type Block = NBlockId;
    type State = MovedState<'db>;
    type Error = SemanticBorrowDiagnostic<'db>;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        vec![self.borrowck.body.entry]
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
        for statement in &self.borrowck.body.blocks[block.index()].statements {
            self.borrowck
                .update_moved_for_statement(&state, &mut moved, statement)?;
            self.borrowck
                .canon()
                .apply_statement_state(&mut state, statement);
        }
        Ok(MovedState(moved))
    }

    fn successors(&self, block: Self::Block) -> &[Self::Block] {
        &self.successors[block]
    }
}

#[derive(Clone, Default)]
pub(super) struct LiveSet(pub(super) FxHashSet<NValueId>);

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
    type Block = NBlockId;
    type State = LiveSet;

    fn block_count(&self) -> usize {
        self.borrowck.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        (0..self.borrowck.body.blocks.len())
            .map(NBlockId::new)
            .collect()
    }

    fn bottom(&self) -> Self::State {
        LiveSet::default()
    }

    fn initialize(&mut self, _exit_states: &mut SecondaryMap<Self::Block, Self::State>) {}

    fn transfer(&mut self, block: Self::Block, out_state: &Self::State) -> Self::State {
        let block_data = &self.borrowck.body.blocks[block.index()];
        let mut live = out_state.0.clone();
        for successor in block_data.terminator.kind.successors() {
            for param in &self.borrowck.body.blocks[successor.block.index()].params {
                live.remove(param);
            }
        }
        live.extend(self.borrowck.facts.terminator_uses(block));
        for (statement, _) in block_data.statements.iter().enumerate().rev() {
            live = self.borrowck.live_before_statement(block, statement, &live);
        }
        LiveSet(live)
    }

    fn predecessors(&self, block: Self::Block) -> &[Self::Block] {
        &self.predecessors[block]
    }
}
