use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{BackwardCfgAnalysis, ForwardCfgAnalysis, JoinSemiLattice, SparseAnalysis};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SBlockId, SLocalId, SStmtId,
        borrowck::ir::{NExpr, NSStmtKind},
    },
};

use super::{
    access::MovedPlaces,
    canon::BorrowCanonCx,
    check::Borrowck,
    ir::{NormalizedSemanticBody, SemanticBorrowDiagnostic},
    loan::{LoanDef, LoanId, ParentSet},
    region::RegionSet,
    summary::{BorrowSourceClause, SummaryPath},
    transfer::{BorrowState, BorrowTransferCx},
};

pub(super) type BlockAdjacency = SmallVec<SBlockId, 2>;
pub(super) type CfgAdjacency = SecondaryMap<SBlockId, BlockAdjacency>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BorrowSummaryMode {
    FinalCheck,
    FinalSummary,
    Provisional,
}

pub(super) struct BorrowLoanTargetState<'a, 'db> {
    pub(super) loans: &'a mut [LoanDef<'db>],
}

pub(super) struct BorrowLoanTargetAnalysis<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedSemanticBody<'db>,
    entry_state: &'a SecondaryMap<SBlockId, BorrowState<'db>>,
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
    call_result_loans: &'a FxHashMap<SStmtId, Vec<(SummaryPath, LoanId)>>,
    call_loan_sources: &'a FxHashMap<LoanId, Vec<BorrowSourceClause>>,
}

impl<'a, 'db> BorrowLoanTargetAnalysis<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        body: &'a NormalizedSemanticBody<'db>,
        entry_state: &'a SecondaryMap<SBlockId, BorrowState<'db>>,
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
        call_result_loans: &'a FxHashMap<SStmtId, Vec<(SummaryPath, LoanId)>>,
        call_loan_sources: &'a FxHashMap<LoanId, Vec<BorrowSourceClause>>,
    ) -> Self {
        Self {
            db,
            body,
            entry_state,
            loan_for_local,
            constant_indices,
            call_result_loans,
            call_loan_sources,
        }
    }

    fn canon<'b>(&'b self, loans: &'b [LoanDef<'db>]) -> BorrowCanonCx<'b, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.body.owner,
            self.body,
            loans,
            self.constant_indices,
        )
    }

    fn extend_loan(
        &self,
        loans: &mut [LoanDef<'db>],
        loan_id: LoanId,
        region: RegionSet<'db>,
        parents: ParentSet,
    ) -> bool {
        loans[loan_id.0 as usize].extend(region, parents)
    }

    fn update_loan_from_stmt(
        &self,
        loans: &mut [LoanDef<'db>],
        state: &BorrowState<'db>,
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
                let (region, parents) = {
                    let canon = self.canon(loans);
                    let region = canon.resolve_place(state, place, stmt.origin)?;
                    (
                        region.clone(),
                        canon.mut_parent_refs_for_place(state, place, &region),
                    )
                };
                Ok(self.extend_loan(loans, loan_id, region, parents))
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
                    let Some(sources) = self.call_loan_sources.get(&loan_id) else {
                        continue;
                    };
                    let mut region = RegionSet::empty();
                    let mut parents = ParentSet::default();
                    let canon = self.canon(loans);
                    for source in sources {
                        let (source_region, source_parents) =
                            canon.instantiate_call_source(state, args, source);
                        region = region.union(&source_region);
                        parents.union(source_parents);
                    }
                    changed |= self.extend_loan(loans, loan_id, region, parents);
                }
                Ok(changed)
            }
            NExpr::Use(value) => {
                let Some(&loan_id) = self.loan_for_local.get(dst) else {
                    return Ok(false);
                };
                let canon = self.canon(loans);
                let region = canon.borrow_local_region(state, value.local);
                Ok(self.extend_loan(
                    loans,
                    loan_id,
                    region.clone(),
                    canon.mut_parent_refs_for_value(state, value.local, &region),
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
            BorrowTransferCx::new(
                self.db,
                self.body,
                self.loan_for_local,
                self.constant_indices,
            )
            .apply_stmt(
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

impl<'db> ForwardCfgAnalysis for BorrowEntryStateAnalysis<'_, 'db> {
    type Block = SBlockId;
    type State = BorrowState<'db>;
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
        BorrowState::new(self.borrowck.value_interner.clone())
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        if !self.borrowck.body.blocks.is_empty() {
            let entry = &mut entry_states[SBlockId::new(0)];
            for (&local, value) in &self.borrowck.param_values_for_local {
                entry.assign(local, *value);
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
