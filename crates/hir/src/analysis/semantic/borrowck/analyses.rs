use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{BackwardCfgAnalysis, ForwardCfgAnalysis, JoinSemiLattice, SparseAnalysis};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SBlockId, SLocalId, SemOrigin, SemanticInstance,
        borrowck::ir::{NExpr, NSStmtKind},
        get_or_build_semantic_instance,
    },
};

use super::{
    canon::{BorrowCanonCx, CanonPlace, CfgAdjacency, Loan, LoanId, MovedPlaces, State},
    check::{Borrowck, provisional_borrow_summary_voucher, semantic_borrow_summary_voucher},
    diagnostics::normalized_body_internal_diag,
    facts::NormalizedBodyFacts,
    ir::{BorrowInputRef, NormalizedSemanticBody, SemanticBorrowDiagnostic},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BorrowSummaryMode {
    Final,
    Provisional,
}

pub(super) struct BorrowLoanTargetState<'a, 'db> {
    pub(super) loans: &'a mut [Loan<'db>],
}

pub(super) struct BorrowLoanEffect<'db> {
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) parents: FxHashSet<LoanId>,
}

pub(super) struct BorrowLoanEffectCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    facts: &'a NormalizedBodyFacts,
    loans: &'a [Loan<'db>],
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    summary_mode: BorrowSummaryMode,
}

impl<'a, 'db> BorrowLoanEffectCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        facts: &'a NormalizedBodyFacts,
        loans: &'a [Loan<'db>],
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            facts,
            loans,
            loan_for_local,
            summary_mode,
        }
    }

    pub(super) fn for_expr(
        &self,
        state: &State<'db>,
        expr: &NExpr<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Option<BorrowLoanEffect<'db>>, SemanticBorrowDiagnostic<'db>> {
        let canon = BorrowCanonCx::new(
            self.db,
            self.instance,
            self.body,
            self.facts,
            self.loans,
            self.loan_for_local,
            self.summary_mode,
        );
        let effect = match expr {
            NExpr::Borrow { place, .. } => BorrowLoanEffect {
                targets: canon.canonicalize_place(state, place, origin)?,
                parents: canon.mut_loans_for_place(state, place),
            },
            NExpr::Call { callee, args, .. } => {
                let callee_instance = get_or_build_semantic_instance(self.db, callee.key);
                let summary = match self.summary_mode.for_callee(self.db, callee_instance) {
                    BorrowSummaryMode::Final => {
                        semantic_borrow_summary_voucher(self.db, callee_instance)
                    }
                    BorrowSummaryMode::Provisional => {
                        provisional_borrow_summary_voucher(self.db, callee_instance)
                    }
                }?;
                let Some(summary) = summary else {
                    return Err(normalized_body_internal_diag(
                        self.db,
                        self.instance,
                        self.body,
                        origin,
                        "borrow-returning call has no semantic borrow summary".to_string(),
                    ));
                };
                let mut targets = FxHashSet::default();
                let mut parents = FxHashSet::default();
                for transform in &summary {
                    let BorrowInputRef::Param(idx) = transform.input;
                    if let Some(arg) = args.get(idx as usize) {
                        targets.extend(canon.canonicalize_value_path(
                            state,
                            arg.local,
                            &transform.proj,
                            origin,
                        )?);
                        parents.extend(canon.mut_loans_for_value(state, arg.local));
                    }
                }
                BorrowLoanEffect { targets, parents }
            }
            NExpr::Use(value) => BorrowLoanEffect {
                targets: canon.canonicalize_value_base(state, value.local),
                parents: canon.mut_loans_for_value(state, value.local),
            },
            _ => return Ok(None),
        };
        Ok(Some(effect))
    }
}

impl BorrowSummaryMode {
    pub(super) fn for_callee<'db>(
        self,
        db: &'db dyn HirAnalysisDb,
        callee: SemanticInstance<'db>,
    ) -> Self {
        if self == Self::Provisional
            || callee
                .key(db)
                .subst(db)
                .generic_args(db)
                .iter()
                .any(|ty| ty.has_param(db) || ty.has_var(db))
        {
            Self::Provisional
        } else {
            Self::Final
        }
    }
}

pub(super) struct BorrowLoanTargetAnalysis<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    facts: &'a NormalizedBodyFacts,
    entry_state: &'a SecondaryMap<SBlockId, State<'db>>,
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    summary_mode: BorrowSummaryMode,
}

impl<'a, 'db> BorrowLoanTargetAnalysis<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        facts: &'a NormalizedBodyFacts,
        entry_state: &'a SecondaryMap<SBlockId, State<'db>>,
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            facts,
            entry_state,
            loan_for_local,
            summary_mode,
        }
    }

    fn canon<'b>(&'b self, loans: &'b [Loan<'db>]) -> BorrowCanonCx<'b, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.instance,
            self.body,
            self.facts,
            loans,
            self.loan_for_local,
            self.summary_mode,
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

    fn update_loan_from_stmt(
        &self,
        loans: &mut [Loan<'db>],
        state: &State<'db>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<bool, SemanticBorrowDiagnostic<'db>> {
        let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
            return Ok(false);
        };
        let Some(&loan_id) = self.loan_for_local.get(dst) else {
            return Ok(false);
        };
        let Some(effect) = BorrowLoanEffectCx::new(
            self.db,
            self.instance,
            self.body,
            self.facts,
            loans,
            self.loan_for_local,
            self.summary_mode,
        )
        .for_expr(state, expr, stmt.origin)?
        else {
            return Ok(false);
        };
        Ok(self.extend_loan(loans, loan_id, effect.targets, effect.parents))
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
            self.canon(state.loans)
                .apply_stmt_state(&mut local_state, stmt)?;
            if !local_state.is_reachable() {
                break;
            }
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
    type State = State<'db>;
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
        State::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        if !self.borrowck.body.blocks.is_empty() {
            let entry = &mut entry_states[SBlockId::new(0)];
            entry.mark_reachable();
            for (&local, &loan) in &self.borrowck.param_loan_for_local {
                entry.assign_loans(local, FxHashSet::from_iter([loan]));
            }
            for root in &self.borrowck.body.borrow_roots {
                if let super::ir::NBorrowRoot::Param { local, param_idx } = root {
                    self.borrowck
                        .canon()
                        .seed_param_pointer_targets(entry, *local, *param_idx);
                }
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
            self.borrowck.canon().apply_stmt_state(&mut state, stmt)?;
            if !state.is_reachable() {
                break;
            }
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
            self.borrowck.canon().apply_stmt_state(&mut state, stmt)?;
            if !state.is_reachable() {
                break;
            }
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
