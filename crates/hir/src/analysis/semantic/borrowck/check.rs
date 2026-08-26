use std::fmt;

use common::diagnostics::CompleteDiagnostic;
use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{solve_backward_cfg, solve_forward_cfg, try_solve_forward_cfg, try_solve_sparse};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SemOrigin, SemanticInstance, get_or_build_semantic_instance,
            identity_semantic_instance_key,
            normalized::{
                NBlockId, NDataPath, NDataProjection, NExpr, NIndex, NOperand, NPlace, NPlaceBase,
                NRootId, NRootKind, NStatement, NStatementKind, NTerminator, NTerminatorKind,
                NValueDefinition, NValueId, NormalizedBody, ReadMode, normalize_semantic_body,
                normalize_semantic_body_provisional,
            },
        },
        ty::{ty_check::BodyOwner, ty_def::BorrowKind},
    },
    hir_def::{Body, Expr, FuncParamMode, ItemKind, Partial, TopLevelMod},
};

use super::{
    analyses::{
        BorrowEntryStateAnalysis, BorrowLivenessAnalysis, BorrowLoanTargetAnalysis,
        BorrowLoanTargetState, BorrowMovedStateAnalysis, BorrowSummaryMode,
    },
    canon::{
        BlockAdjacency, BorrowCanonCx, BorrowRoot, CanonPlace, CfgAdjacency, Loan, LoanId,
        MoveSite, MovedPlaces, State, place_set_overlaps, places_overlap,
    },
    diagnostics::operand_origin,
    facts::NormalizedBodyFacts,
    ir::{
        BlockedSemanticBody, BorrowDiagnosticId, BorrowInputRef, BorrowSummary, BorrowSummaryId,
        BorrowTransform, SemanticBorrowCheckResult, SemanticBorrowDiagKind,
        SemanticBorrowDiagnostic, SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult,
        SemanticNormalizationFailure,
    },
};

#[salsa::tracked(
    cycle_fn=semantic_borrow_summary_cycle_recover,
    cycle_initial=semantic_borrow_summary_cycle_initial
)]
fn semantic_borrow_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    let borrowck = match Borrowck::new(db, instance) {
        Ok(borrowck) => borrowck,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return blocked_signature_borrow_summary_result(db, instance, blocked);
        }
        Err(SemanticNormalizationFailure::InternalFailure(diag)) => {
            return SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag));
        }
    };
    if !instance_returns_borrow(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    cached_borrow_summary_result(db, borrowck.borrow_summary())
}

#[salsa::tracked(
    cycle_fn=semantic_borrow_summary_cycle_recover,
    cycle_initial=semantic_borrow_summary_cycle_initial
)]
fn provisional_borrow_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    let body = match normalize_semantic_body_provisional(db, instance) {
        Ok(artifacts) => artifacts.body,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return blocked_signature_borrow_summary_result(db, instance, blocked);
        }
        Err(SemanticNormalizationFailure::InternalFailure(diag)) => {
            return SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag));
        }
    };
    let borrowck = match Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)
    {
        Ok(borrowck) => borrowck,
        Err(diag) => return SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    };
    if !instance_returns_borrow(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    cached_borrow_summary_result(db, borrowck.borrow_summary())
}

fn blocked_signature_borrow_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: BlockedSemanticBody<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    SemanticBorrowSummaryResult::Blocked {
        body,
        summary: instance_returns_borrow(db, instance)
            .then(|| BorrowSummaryId::new(db, conservative_signature_borrow_summary(db, instance))),
    }
}

fn cached_borrow_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    result: Result<BorrowSummaryComputation<'db>, SemanticBorrowDiagnostic<'db>>,
) -> SemanticBorrowSummaryResult<'db> {
    match result {
        Ok(BorrowSummaryComputation {
            summary,
            blocked: Some(body),
        }) => SemanticBorrowSummaryResult::Blocked {
            body,
            summary: summary.map(|summary| BorrowSummaryId::new(db, summary)),
        },
        Ok(BorrowSummaryComputation {
            summary,
            blocked: None,
        }) => SemanticBorrowSummaryResult::Ok(
            summary.map(|summary| BorrowSummaryId::new(db, summary)),
        ),
        Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

fn conservative_signature_borrow_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> BorrowSummary {
    let BodyOwner::Func(func) = instance.key(db).owner(db) else {
        return Vec::new();
    };
    func.params(db)
        .enumerate()
        .map(|(idx, _)| BorrowTransform {
            input: BorrowInputRef::Param(idx as u32),
            proj: NDataPath::default(),
        })
        .collect()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticAnalysisError<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Diagnostic(CompleteDiagnostic),
}

impl SemanticAnalysisError<'_> {
    pub fn diagnostic(&self) -> Option<&CompleteDiagnostic> {
        match self {
            Self::Blocked(_) => None,
            Self::Diagnostic(diag) => Some(diag),
        }
    }
}

impl fmt::Display for SemanticAnalysisError<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blocked(blocked) => write!(
                formatter,
                "semantic body is blocked by upstream causes: {:?}",
                blocked.causes
            ),
            Self::Diagnostic(diag) => formatter.write_str(&diag.message),
        }
    }
}

pub fn semantic_borrow_summary<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary>, SemanticAnalysisError<'db>> {
    match semantic_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => {
            Ok(summary.map(|summary| summary.items(db).clone()))
        }
        SemanticBorrowSummaryResult::Blocked { body, .. } => {
            Err(SemanticAnalysisError::Blocked(body))
        }
        SemanticBorrowSummaryResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

pub(super) struct BorrowSummaryVoucher<'db> {
    pub(super) summary: Option<BorrowSummary>,
    pub(super) blocked: Option<BlockedSemanticBody<'db>>,
}

pub(super) fn semantic_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<BorrowSummaryVoucher<'db>, SemanticBorrowDiagnostic<'db>> {
    match semantic_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
        }),
        SemanticBorrowSummaryResult::Blocked { body, summary } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: Some(body),
        }),
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub(super) fn provisional_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<BorrowSummaryVoucher<'db>, SemanticBorrowDiagnostic<'db>> {
    match provisional_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
        }),
        SemanticBorrowSummaryResult::Blocked { body, summary } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: Some(body),
        }),
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub fn check_semantic_borrows<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticAnalysisError<'db>> {
    match semantic_borrow_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => Ok(()),
        SemanticBorrowCheckResult::Blocked(body) => Err(SemanticAnalysisError::Blocked(body)),
        SemanticBorrowCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

#[salsa::tracked]
fn semantic_borrow_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    let borrowck = match Borrowck::new(db, instance) {
        Ok(borrowck) => borrowck,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return SemanticBorrowCheckResult::Blocked(blocked);
        }
        Err(SemanticNormalizationFailure::InternalFailure(diag)) => {
            return SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag));
        }
    };
    match borrowck.check() {
        Ok(Some(blocked)) => SemanticBorrowCheckResult::Blocked(blocked),
        Ok(None) => SemanticBorrowCheckResult::Ok,
        Err(diag) => SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

pub struct SemanticBorrowAnalysisPass;

impl ModuleAnalysisPass for SemanticBorrowAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        collect_semantic_borrow_diagnostic_vouchers(db, top_mod)
    }
}

pub fn collect_semantic_borrow_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    let mut diags = Vec::new();
    let mut seen_owners = FxHashSet::default();
    let mut seen_diags = FxHashSet::default();
    collect_top_mod_semantic_borrow_diagnostic_vouchers(
        db,
        top_mod,
        &mut seen_owners,
        &mut seen_diags,
        &mut diags,
    );
    diags
}

fn collect_top_mod_semantic_borrow_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    seen_owners: &mut FxHashSet<BodyOwner<'db>>,
    seen_diags: &mut FxHashSet<BorrowDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        match item {
            ItemKind::Func(func) => {
                collect_owner(db, BodyOwner::Func(*func), seen_owners, seen_diags, diags)
            }
            ItemKind::Const(const_) => collect_owner(
                db,
                BodyOwner::Const(*const_),
                seen_owners,
                seen_diags,
                diags,
            ),
            ItemKind::Contract(contract) => {
                collect_owner(
                    db,
                    BodyOwner::ContractInit {
                        contract: *contract,
                    },
                    seen_owners,
                    seen_diags,
                    diags,
                );
                for (recv_idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(db).len() {
                        collect_owner(
                            db,
                            BodyOwner::ContractRecvArm {
                                contract: *contract,
                                recv_idx: recv_idx as u32,
                                arm_idx: arm_idx as u32,
                            },
                            seen_owners,
                            seen_diags,
                            diags,
                        );
                    }
                }
            }
            ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }
}

fn collect_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    seen_owners: &mut FxHashSet<BodyOwner<'db>>,
    seen_diags: &mut FxHashSet<BorrowDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if !seen_owners.insert(owner) {
        return;
    }
    let key = identity_semantic_instance_key(db, owner);
    let instance = get_or_build_semantic_instance(db, key);
    match semantic_borrow_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => {}
        SemanticBorrowCheckResult::Blocked(_) => return,
        SemanticBorrowCheckResult::Err(diag) if seen_diags.insert(diag) => {
            diags.push(Box::new(diag));
        }
        SemanticBorrowCheckResult::Err(_) => {}
    }
    match super::noesc::semantic_noesc_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => {}
        SemanticBorrowCheckResult::Blocked(_) => {}
        SemanticBorrowCheckResult::Err(diag) if seen_diags.insert(diag) => {
            diags.push(Box::new(diag));
        }
        SemanticBorrowCheckResult::Err(_) => {}
    }
}

pub(super) struct Borrowck<'db> {
    pub(super) db: &'db dyn HirAnalysisDb,
    pub(super) instance: SemanticInstance<'db>,
    pub(super) body: NormalizedBody<'db>,
    pub(super) facts: NormalizedBodyFacts,
    pub(super) summary_mode: BorrowSummaryMode,
    hir_body: Option<Body<'db>>,
    param_modes: Vec<FuncParamMode>,
    pub(super) loan_for_value: FxHashMap<NValueId, LoanId>,
    pub(super) param_loan_for_value: FxHashMap<NValueId, LoanId>,
    loans: Vec<Loan<'db>>,
    pub(super) entry_state: SecondaryMap<NBlockId, State>,
    moved_entry: SecondaryMap<NBlockId, MovedPlaces<'db>>,
    live_before: Vec<Vec<FxHashSet<NValueId>>>,
    live_before_term: SecondaryMap<NBlockId, FxHashSet<NValueId>>,
}

struct BorrowSummaryComputation<'db> {
    summary: Option<BorrowSummary>,
    blocked: Option<BlockedSemanticBody<'db>>,
}

impl<'db> Borrowck<'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticNormalizationFailure<'db>> {
        let body = normalize_semantic_body(db, instance)?.body;
        Self::new_with_body(db, instance, body, BorrowSummaryMode::Final)
            .map_err(SemanticNormalizationFailure::InternalFailure)
    }

    pub(super) fn new_with_body(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: NormalizedBody<'db>,
        summary_mode: BorrowSummaryMode,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        let owner = instance.key(db).owner(db);
        let param_modes = match owner {
            BodyOwner::Func(func) => func.params(db).map(|param| param.mode(db)).collect(),
            _ => Vec::new(),
        };
        let facts = NormalizedBodyFacts::new(&body);
        let mut checker = Self {
            db,
            instance,
            hir_body: owner.body(db),
            body,
            facts,
            summary_mode,
            param_modes,
            loan_for_value: FxHashMap::default(),
            param_loan_for_value: FxHashMap::default(),
            loans: Vec::new(),
            entry_state: SecondaryMap::new(),
            moved_entry: SecondaryMap::new(),
            live_before: Vec::new(),
            live_before_term: SecondaryMap::new(),
        };
        checker.init_loans();
        Ok(checker)
    }

    pub(super) fn canon(&self) -> BorrowCanonCx<'_, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.instance,
            &self.body,
            &self.loans,
            &self.loan_for_value,
        )
    }

    fn borrow_summary(
        mut self,
    ) -> Result<BorrowSummaryComputation<'db>, SemanticBorrowDiagnostic<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        let typed_body = self.instance.key(self.db).instantiate_typed_body(self.db);
        if typed_body.result_ty().as_borrow(self.db).is_none() || owner.body(self.db).is_none() {
            return Ok(BorrowSummaryComputation {
                summary: None,
                blocked: None,
            });
        }
        self.compute_entry_states();
        if let Some(blocked) = self.compute_loan_targets()? {
            return Ok(BorrowSummaryComputation {
                summary: Some(conservative_signature_borrow_summary(
                    self.db,
                    self.instance,
                )),
                blocked: Some(blocked),
            });
        }
        Ok(BorrowSummaryComputation {
            summary: Some(self.compute_return_summary()?),
            blocked: None,
        })
    }

    fn check(mut self) -> Result<Option<BlockedSemanticBody<'db>>, SemanticBorrowDiagnostic<'db>> {
        self.compute_entry_states();
        if let Some(blocked) = self.compute_loan_targets()? {
            return Ok(Some(blocked));
        }
        self.compute_moved_states()?;
        self.compute_liveness();
        self.check_conflicts()?;
        if self
            .instance
            .key(self.db)
            .instantiate_typed_body(self.db)
            .result_ty()
            .as_borrow(self.db)
            .is_some()
        {
            let _ = self.compute_return_summary()?;
        }
        Ok(None)
    }

    fn compute_liveness(&mut self) {
        let live_out = solve_backward_cfg(&mut BorrowLivenessAnalysis::new(self));
        self.live_before = self
            .body
            .blocks
            .iter()
            .map(|block| vec![FxHashSet::default(); block.statements.len()])
            .collect();
        self.live_before_term = SecondaryMap::new();
        self.live_before_term.resize(self.body.blocks.len());

        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let bb = NBlockId::new(bb_idx);
            let mut live = live_out[bb].0.clone();
            live.extend(self.facts.terminator_uses(bb));
            self.live_before_term[bb] = live.clone();
            for (statement, _) in block.statements.iter().enumerate().rev() {
                live = self.live_before_statement(bb, statement, &live);
                self.live_before[bb_idx][statement] = live.clone();
            }
        }
    }

    pub(super) fn live_before_statement(
        &self,
        block: NBlockId,
        statement: usize,
        live_after: &FxHashSet<NValueId>,
    ) -> FxHashSet<NValueId> {
        let mut live = live_after.clone();
        let statement_data = &self.body.blocks[block.index()].statements[statement];
        match &statement_data.kind {
            NStatementKind::Define { result, .. } => {
                live.remove(result);
                live.extend(self.facts.statement_uses(block, statement));
            }
            NStatementKind::Store { .. } => {
                live.extend(self.facts.statement_uses(block, statement));
            }
        }
        live
    }

    fn init_loans(&mut self) {
        for (index, value) in self.body.values.iter().enumerate() {
            let value_id = NValueId::new(index);
            if let Some((kind, _)) = value.ty.as_borrow(self.db)
                && let NValueDefinition::EntryParam { param } = value.definition
            {
                let loan = LoanId(self.loans.len() as u32);
                let mut targets = FxHashSet::default();
                targets.insert(CanonPlace {
                    root: BorrowRoot::Param(param),
                    proj: NDataPath::default(),
                });
                self.loans.push(Loan {
                    kind,
                    targets,
                    parents: FxHashSet::default(),
                    origin: crate::analysis::semantic::SemOrigin::Body(self.body.template_owner),
                });
                self.param_loan_for_value.insert(value_id, loan);
            }
        }

        for block in &self.body.blocks {
            for statement in &block.statements {
                let NStatementKind::Define { result, expr } = &statement.kind else {
                    continue;
                };
                if self
                    .body
                    .value(*result)
                    .is_some_and(|value| value.ty.as_borrow(self.db).is_some())
                    && matches!(
                        expr,
                        NExpr::Borrow { .. } | NExpr::Call { .. } | NExpr::ProjectValue { .. }
                    )
                {
                    let kind = self
                        .body
                        .value(*result)
                        .and_then(|value| value.ty.as_borrow(self.db))
                        .map(|(kind, _)| kind)
                        .expect("borrow value");
                    let loan = LoanId(self.loans.len() as u32);
                    self.loan_for_value.insert(*result, loan);
                    self.loans.push(Loan {
                        kind,
                        targets: FxHashSet::default(),
                        parents: FxHashSet::default(),
                        origin: statement.origin,
                    });
                }
            }
        }
    }

    pub(super) fn compute_entry_states(&mut self) {
        self.entry_state = solve_forward_cfg(&mut BorrowEntryStateAnalysis::new(self));
    }

    pub(super) fn compute_loan_targets(
        &mut self,
    ) -> Result<Option<BlockedSemanticBody<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut analysis = BorrowLoanTargetAnalysis::new(
            self.db,
            self.instance,
            &self.body,
            &self.entry_state,
            &self.loan_for_value,
            self.summary_mode,
        );
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)?;
        Ok(analysis.blocked_body())
    }

    fn compute_moved_states(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.moved_entry = try_solve_forward_cfg(&mut BorrowMovedStateAnalysis::new(self))?
            .iter()
            .map(|(bb, state)| (bb, state.0.clone()))
            .collect();
        Ok(())
    }

    fn check_conflicts(&self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let bb = NBlockId::new(bb_idx);
            let mut state = self.entry_state[bb].clone();
            let mut moved = self.moved_entry[bb].clone();
            for (statement_index, statement) in block.statements.iter().enumerate() {
                self.check_statement(
                    &state,
                    &moved,
                    &self.live_before[bb_idx][statement_index],
                    statement,
                )?;
                self.update_moved_for_statement(&state, &mut moved, statement)?;
                self.canon().apply_statement_state(&mut state, statement);
            }
            self.check_terminator(
                &state,
                &moved,
                &self.live_before_term[bb],
                &block.terminator,
            )?;
        }
        Ok(())
    }

    fn check_statement(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<NValueId>,
        statement: &NStatement<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = self.effective_loans(state, live);
        match &statement.kind {
            NStatementKind::Define { result, expr } => match expr {
                NExpr::Load { place, mode } => {
                    let targets =
                        self.canon()
                            .canonicalize_place(state, place, statement.origin)?;
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        statement.origin,
                        "cannot use a value after it was moved",
                    )?;
                    if *mode == ReadMode::Move {
                        self.check_move_out(&active, place, &targets, statement.origin)?;
                    }
                }
                NExpr::Borrow { place, kind, .. } => {
                    let targets =
                        self.canon()
                            .canonicalize_place(state, place, statement.origin)?;
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        statement.origin,
                        "cannot borrow a moved value",
                    )?;
                    if let Some(conflict) = self.first_loan_conflict(
                        &active,
                        self.loan_for_value.get(result).copied(),
                        *kind,
                        &targets,
                    ) {
                        return Err(self.borrow_conflict_diag(
                            statement.origin,
                            self.overlapping_loans_msg(conflict, *kind),
                            conflict,
                        ));
                    }
                }
                NExpr::ProjectValue { value, path } => {
                    let targets = self.projected_move_targets(state, *value, &path.0);
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        statement.origin,
                        "cannot use a value after it was moved",
                    )?;
                    if value.mode == ReadMode::Move {
                        self.check_move_targets_out(&active, &targets, statement.origin)?;
                    }
                }
                _ => self.check_expr_operands(state, moved, statement.origin, expr)?,
            },
            NStatementKind::Store { destination, .. } => {
                let targets =
                    self.canon()
                        .canonicalize_place(state, destination, statement.origin)?;
                self.check_moved_parent(moved, &targets, statement.origin)?;
            }
        }
        Ok(())
    }

    fn check_terminator(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<NValueId>,
        term: &NTerminator<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &term.kind {
            NTerminatorKind::Goto(_) | NTerminatorKind::Assert { .. } => {}
            NTerminatorKind::Branch { cond, .. }
            | NTerminatorKind::MatchEnum { value: cond, .. }
            | NTerminatorKind::Return(Some(cond)) => {
                let _ = live;
                self.check_operand(
                    state,
                    moved,
                    *cond,
                    term.origin,
                    "cannot use a value after it was moved",
                )?;
            }
            NTerminatorKind::Return(None) => {}
        }
        if let NTerminatorKind::Return(Some(value)) = term.kind
            && self
                .body
                .value(value.value)
                .is_some_and(|value| value.ty.as_borrow(self.db).is_some())
            && self
                .canon()
                .borrow_value_targets(state, value.value)
                .is_empty()
        {
            return Err(self.internal_diag(
                term.origin,
                "borrow return local has no tracked loan targets".to_string(),
            ));
        }
        Ok(())
    }

    fn compute_return_summary(&self) -> Result<BorrowSummary, SemanticBorrowDiagnostic<'db>> {
        let mut out = Vec::new();
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let NTerminatorKind::Return(Some(value)) = block.terminator.kind else {
                continue;
            };
            let mut state = self.entry_state[NBlockId::new(bb_idx)].clone();
            for statement in &block.statements {
                self.canon().apply_statement_state(&mut state, statement);
            }
            for target in self.canon().borrow_value_targets(&state, value.value) {
                for projection in target.proj.iter() {
                    if matches!(projection, NDataProjection::Index(NIndex::Value(_))) {
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            "return borrows with dynamic indices are not supported".to_string(),
                        ));
                    }
                }
                match &target.root {
                    BorrowRoot::Param(idx) => {
                        let transform = BorrowTransform {
                            input: BorrowInputRef::Param(*idx),
                            proj: target.proj.clone(),
                        };
                        if !out.contains(&transform) {
                            out.push(transform);
                        }
                    }
                    BorrowRoot::Provider(_) => {
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            "cannot return a borrow derived from an effect parameter".to_string(),
                        ));
                    }
                    BorrowRoot::Root(root) => {
                        let name = self.pretty_root_name(*root);
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            format!("cannot return a borrow to local `{name}`"),
                        ));
                    }
                    BorrowRoot::Value(value) => {
                        let name = self.pretty_value_name(*value);
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            format!("cannot return a borrow to local `{name}`"),
                        ));
                    }
                }
            }
        }
        Ok(out)
    }

    fn effective_loans(&self, state: &State, live: &FxHashSet<NValueId>) -> Vec<LoanId> {
        let active = state
            .value_loans
            .iter()
            .filter(|(local, _)| live.contains(local))
            .flat_map(|(_, loans)| loans.iter().copied())
            .collect::<FxHashSet<_>>();
        let mut suspended = FxHashSet::default();
        let mut worklist: Vec<_> = active.iter().copied().collect();
        while let Some(loan) = worklist.pop() {
            for parent in &self.loans[loan.0 as usize].parents {
                if suspended.insert(*parent) {
                    worklist.push(*parent);
                }
            }
        }
        let mut active: Vec<_> = active
            .into_iter()
            .filter(|loan| !suspended.contains(loan))
            .collect();
        active.sort_by_key(|loan| loan.0);
        active
    }

    fn first_loan_conflict(
        &self,
        active: &[LoanId],
        new_loan: Option<LoanId>,
        new_kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<LoanId> {
        let reborrow_parents = new_loan.map(|loan| &self.loans[loan.0 as usize].parents);
        active
            .iter()
            .copied()
            .filter(|loan| reborrow_parents.is_none_or(|parents| !parents.contains(loan)))
            .find(|loan| {
                let loan = &self.loans[loan.0 as usize];
                !matches!((loan.kind, new_kind), (BorrowKind::Ref, BorrowKind::Ref))
                    && place_set_overlaps(&loan.targets, targets)
            })
    }

    fn check_move_out(
        &self,
        active: &[LoanId],
        place: &NPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let NPlaceBase::CapabilityTarget { carrier } = place.base {
            if self.body.values[carrier.index()]
                .source
                .is_some_and(|binding| {
                    matches!(
                        binding,
                        crate::analysis::ty::ty_check::LocalBinding::Param {
                            mode: FuncParamMode::View,
                            ..
                        }
                    )
                })
            {
                return Err(self.move_conflict_diag(
                    origin,
                    "cannot move out of a view parameter".to_string(),
                ));
            }
            return Err(self.move_conflict_diag(
                origin,
                "cannot move out through a borrow handle".to_string(),
            ));
        }
        self.check_move_targets_out(active, targets, origin)?;
        Ok(())
    }

    fn check_move_targets_out(
        &self,
        active: &[LoanId],
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for target in targets {
            if let BorrowRoot::Param(idx) = target.root
                && self
                    .param_modes
                    .get(idx as usize)
                    .copied()
                    .is_some_and(|mode| mode == FuncParamMode::View)
            {
                return Err(self.move_conflict_diag(
                    origin,
                    "cannot move out of a view parameter".to_string(),
                ));
            }
        }
        if let Some(loan) = active
            .iter()
            .copied()
            .find(|loan| place_set_overlaps(&self.loans[loan.0 as usize].targets, targets))
        {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot move out of a value while it is borrowed".to_string(),
                loan,
            ));
        }
        Ok(())
    }

    pub(super) fn update_moved_for_statement(
        &self,
        state: &State,
        moved: &mut MovedPlaces<'db>,
        statement: &NStatement<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &statement.kind {
            NStatementKind::Define { result, expr } => {
                moved.retain(|place, _| place.root != BorrowRoot::Value(*result));
                if let NExpr::Load {
                    place,
                    mode: ReadMode::Move,
                } = expr
                {
                    let site = MoveSite {
                        origin: statement.origin,
                        note: "value is moved here".to_string(),
                    };
                    for place in self
                        .canon()
                        .canonicalize_place(state, place, statement.origin)?
                    {
                        moved.insert(place, site.clone());
                    }
                }
                if let NExpr::ProjectValue { value, path } = expr {
                    if value.mode == ReadMode::Move {
                        let site = self.move_site(*value, operand_origin(*value, statement.origin));
                        for place in self.projected_move_targets(state, *value, &path.0) {
                            moved.insert(place, site.clone());
                        }
                    }
                } else {
                    self.record_expr_moves(state, moved, statement.origin, expr)?;
                }
            }
            NStatementKind::Store { destination, .. } => {
                let written =
                    self.canon()
                        .canonicalize_place(state, destination, statement.origin)?;
                moved.retain(|place, _| {
                    !written.iter().any(|written| {
                        written.root == place.root && written.proj.is_prefix_of(&place.proj)
                    })
                });
            }
        }
        Ok(())
    }

    fn projected_move_targets(
        &self,
        state: &State,
        source: NOperand,
        path: &NDataPath,
    ) -> FxHashSet<CanonPlace<'db>> {
        self.canon()
            .canonicalize_value_base(state, source.value)
            .into_iter()
            .map(|mut target| {
                target.proj = target.proj.concat(path);
                target
            })
            .collect()
    }

    fn check_expr_operands(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        expr.try_for_each_value_operand(|value| {
            self.check_operand(
                state,
                moved,
                value,
                origin,
                "cannot use a value after it was moved",
            )
        })
    }

    fn check_operand(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        operand: NOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        let targets = self.canon().canonicalize_value_base(state, operand.value);
        if targets.is_empty() {
            return Ok(());
        }
        self.check_moved_overlap(moved, &targets, origin, message)
    }

    fn record_expr_moves(
        &self,
        state: &State,
        moved: &mut MovedPlaces<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        expr.try_for_each_value_operand(|value| {
            self.record_operand_move(state, moved, value, origin)
        })
    }

    fn record_operand_move(
        &self,
        state: &State,
        moved: &mut MovedPlaces<'db>,
        operand: NOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        if operand.mode == ReadMode::Move && self.value_has_runtime_move_semantics(operand.value) {
            let site = self.move_site(operand, origin);
            for place in self.canon().canonicalize_value_base(state, operand.value) {
                moved.insert(place, site.clone());
            }
        }
        Ok(())
    }

    fn move_site(&self, operand: NOperand, origin: SemOrigin<'db>) -> MoveSite<'db> {
        MoveSite {
            origin,
            note: self.moved_operand_name(operand).map_or_else(
                || "value is moved here".to_string(),
                |name| format!("`{name}` is moved here"),
            ),
        }
    }

    fn moved_operand_name(&self, operand: NOperand) -> Option<String> {
        let expr = operand.origin?;
        let body = self.hir_body?;
        let Partial::Present(Expr::Path(Partial::Present(path))) = expr.data(self.db, body) else {
            return None;
        };
        path.as_ident(self.db)
            .map(|ident| ident.data(self.db).to_string())
    }

    fn value_has_runtime_move_semantics(&self, value: NValueId) -> bool {
        self.body
            .value(value)
            .is_some_and(|value| value.ty.as_capability(self.db).is_none())
    }

    fn check_moved_overlap(
        &self,
        moved: &MovedPlaces<'db>,
        accessed: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            accessed
                .iter()
                .any(|accessed| places_overlap(moved, accessed))
        }) {
            let mut diag = self.move_conflict_diag(origin, message.to_string());
            self.push_secondary_origin(&mut diag, site.origin, site.note.clone());
            return Err(diag);
        }
        Ok(())
    }

    fn check_moved_parent(
        &self,
        moved: &MovedPlaces<'db>,
        written: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            written.iter().any(|written| {
                written.root == moved.root
                    && moved.proj.is_prefix_of(&written.proj)
                    && moved.proj != written.proj
            })
        }) {
            let mut diag =
                self.move_conflict_diag(origin, "cannot write through a moved value".to_string());
            self.push_secondary_origin(&mut diag, site.origin, site.note.clone());
            return Err(diag);
        }
        Ok(())
    }

    fn successors(&self, terminator: &NTerminatorKind<'db>) -> BlockAdjacency {
        let mut out = BlockAdjacency::new();
        match terminator {
            NTerminatorKind::Goto(target) => out.push(target.block),
            NTerminatorKind::Branch {
                then_target,
                else_target,
                ..
            } => {
                out.push(then_target.block);
                out.push(else_target.block);
            }
            NTerminatorKind::MatchEnum { cases, default, .. } => {
                out.extend(cases.iter().map(|(_, target)| target.block));
                if let Some(default) = default {
                    out.push(default.block);
                }
            }
            NTerminatorKind::Assert { .. } | NTerminatorKind::Return(_) => {}
        }
        out
    }

    pub(super) fn cfg_successor_indices(&self) -> CfgAdjacency {
        let mut successors = CfgAdjacency::new();
        successors.resize(self.body.blocks.len());
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            successors[NBlockId::new(bb_idx)] = self.successors(&block.terminator.kind);
        }
        successors
    }

    pub(super) fn cfg_predecessor_indices(&self) -> CfgAdjacency {
        let mut predecessors = CfgAdjacency::new();
        predecessors.resize(self.body.blocks.len());
        for (bb, successors) in self.cfg_successor_indices().iter() {
            for succ in successors.iter().copied() {
                predecessors[succ].push(bb);
            }
        }
        predecessors
    }

    fn borrow_conflict_diag(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
        loan: LoanId,
    ) -> SemanticBorrowDiagnostic<'db> {
        let mut diag = self.diag(SemanticBorrowDiagKind::BorrowConflict, origin, message);
        self.push_secondary_origin(
            &mut diag,
            self.loans[loan.0 as usize].origin,
            "borrow created here".to_string(),
        );
        diag
    }

    fn move_conflict_diag(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        self.diag(SemanticBorrowDiagKind::MoveConflict, origin, message)
    }

    fn invalid_return_diag(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        self.diag(SemanticBorrowDiagKind::InvalidReturnBorrow, origin, message)
    }

    fn internal_diag(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        self.diag(SemanticBorrowDiagKind::Internal, origin, message)
    }

    pub(super) fn diag(
        &self,
        kind: SemanticBorrowDiagKind,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        SemanticBorrowDiagnostic::new(
            self.instance,
            kind,
            message,
            SemanticBorrowDiagnosticSpan::Origin {
                owner: self.instance.key(self.db).owner(self.db),
                origin,
            },
        )
    }

    fn push_secondary_origin(
        &self,
        diag: &mut SemanticBorrowDiagnostic<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: String,
    ) {
        diag.push_secondary(
            message,
            SemanticBorrowDiagnosticSpan::Origin {
                owner: self.instance.key(self.db).owner(self.db),
                origin,
            },
        );
    }

    fn overlapping_loans_msg(&self, loan: LoanId, new_kind: BorrowKind) -> String {
        match (new_kind, self.loans[loan.0 as usize].kind) {
            (BorrowKind::Mut, BorrowKind::Mut) => {
                "cannot mutably borrow this place while a mut borrow is active".to_string()
            }
            (BorrowKind::Mut, BorrowKind::Ref) => {
                "cannot mutably borrow this place while an immutable borrow is active".to_string()
            }
            (BorrowKind::Ref, BorrowKind::Mut) => {
                "cannot immutably borrow this place while a mutable borrow is active".to_string()
            }
            (BorrowKind::Ref, BorrowKind::Ref) => unreachable!(),
        }
    }

    fn pretty_value_name(&self, value: NValueId) -> String {
        self.hir_body
            .zip(self.body.value(value).and_then(|value| value.source))
            .map(|(body, source)| source.pretty_name_in_body(self.db, body))
            .unwrap_or_else(|| format!("%v{}", value.index()))
    }

    fn pretty_root_name(&self, root: NRootId) -> String {
        let source = match &self.body.root(root).map(|root| &root.kind) {
            Some(NRootKind::LocalSlot { binding }) => *binding,
            Some(NRootKind::ParamPlace { param }) => {
                return format!("parameter {param}");
            }
            Some(NRootKind::Provider { .. } | NRootKind::CapabilityRepresentation { .. })
            | None => None,
        };
        self.hir_body
            .zip(source)
            .map(|(body, source)| source.pretty_name_in_body(self.db, body))
            .unwrap_or_else(|| format!("%r{}", root.index()))
    }
}

fn semantic_borrow_summary_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    SemanticBorrowSummaryResult::Ok(
        instance_returns_borrow(db, instance).then(|| BorrowSummaryId::new(db, Vec::new())),
    )
}

fn instance_returns_borrow<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    let key = instance.key(db);
    key.owner(db).body(db).is_some() && key.typed_body(db).result_ty().as_borrow(db).is_some()
}

fn semantic_borrow_summary_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &SemanticBorrowSummaryResult<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}
