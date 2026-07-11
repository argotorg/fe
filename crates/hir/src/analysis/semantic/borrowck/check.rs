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
            SBlockId, SemOrigin, SemanticInstance, get_or_build_semantic_instance,
            identity_semantic_instance_key,
        },
        ty::{ty_check::BodyOwner, ty_def::BorrowKind},
    },
    hir_def::{Body, Expr, FuncParamMode, ItemKind, Partial, TopLevelMod},
    projection::{IndexSource, Projection},
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
        BorrowDiagnosticId, BorrowInputRef, BorrowSummary, BorrowSummaryId, BorrowTransform,
        NBorrowRoot, NBorrowRootId, NExpr, NOperand, NSPlace, NSPlaceRoot, NSProjectionPath,
        NSStmtKind, NSTerminatorKind, NormalizedBindingLowering, NormalizedSemanticBody, ReadMode,
        SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult,
        local_has_runtime_move_semantics,
    },
    normalize::{normalize_provisional_semantic_body, normalize_semantic_body},
    verify::verify_normalized_semantic_body,
};

#[salsa::tracked(
    cycle_fn=semantic_borrow_summary_cycle_recover,
    cycle_initial=semantic_borrow_summary_cycle_initial
)]
fn semantic_borrow_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    if !instance_returns_borrow(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    match Borrowck::new(db, instance).and_then(Borrowck::borrow_summary) {
        Ok(summary) => SemanticBorrowSummaryResult::Ok(
            summary.map(|summary| BorrowSummaryId::new(db, summary)),
        ),
        Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

#[salsa::tracked(
    cycle_fn=semantic_borrow_summary_cycle_recover,
    cycle_initial=semantic_borrow_summary_cycle_initial
)]
fn provisional_borrow_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    if !instance_returns_borrow(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    let body = match normalize_provisional_semantic_body(db, instance) {
        Ok(body) => body,
        Err(diag) => return SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    };
    match Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)
        .and_then(Borrowck::borrow_summary)
    {
        Ok(summary) => SemanticBorrowSummaryResult::Ok(
            summary.map(|summary| BorrowSummaryId::new(db, summary)),
        ),
        Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

pub fn semantic_borrow_summary<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary<'db>>, CompleteDiagnostic> {
    semantic_borrow_summary_voucher(db, instance).map_err(|diag| diag.to_complete(db))
}

pub(super) fn semantic_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary<'db>>, SemanticBorrowDiagnostic<'db>> {
    match semantic_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => {
            Ok(summary.map(|summary| summary.items(db).clone()))
        }
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub(super) fn provisional_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary<'db>>, SemanticBorrowDiagnostic<'db>> {
    match provisional_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => {
            Ok(summary.map(|summary| summary.items(db).clone()))
        }
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub fn check_semantic_borrows<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), CompleteDiagnostic> {
    match semantic_borrow_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => Ok(()),
        SemanticBorrowCheckResult::Err(diag) => Err(diag.to_complete(db)),
    }
}

#[salsa::tracked]
fn semantic_borrow_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    match Borrowck::new(db, instance).and_then(Borrowck::check) {
        Ok(()) => SemanticBorrowCheckResult::Ok,
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
    if let SemanticBorrowCheckResult::Err(diag) = semantic_borrow_check_query(db, instance)
        && seen_diags.insert(diag)
    {
        diags.push(Box::new(diag));
    }
    if let super::ir::SemanticBorrowCheckResult::Err(diag) =
        super::noesc::semantic_noesc_check_query(db, instance)
        && seen_diags.insert(diag)
    {
        diags.push(Box::new(diag));
    }
}

pub(super) struct Borrowck<'db> {
    pub(super) db: &'db dyn HirAnalysisDb,
    pub(super) instance: SemanticInstance<'db>,
    pub(super) body: NormalizedSemanticBody<'db>,
    pub(super) facts: NormalizedBodyFacts,
    pub(super) summary_mode: BorrowSummaryMode,
    hir_body: Option<Body<'db>>,
    param_modes: Vec<FuncParamMode>,
    param_index_of_local: FxHashMap<crate::analysis::semantic::SLocalId, u32>,
    pub(super) loan_for_local: FxHashMap<crate::analysis::semantic::SLocalId, LoanId>,
    pub(super) param_loan_for_local: FxHashMap<crate::analysis::semantic::SLocalId, LoanId>,
    loans: Vec<Loan<'db>>,
    pub(super) entry_state: SecondaryMap<SBlockId, State>,
    moved_entry: SecondaryMap<SBlockId, MovedPlaces<'db>>,
    live_before: Vec<Vec<FxHashSet<crate::analysis::semantic::SLocalId>>>,
    live_before_term: SecondaryMap<SBlockId, FxHashSet<crate::analysis::semantic::SLocalId>>,
}

impl<'db> Borrowck<'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        let body = normalize_semantic_body(db, instance)?;
        Self::new_with_body(db, instance, body, BorrowSummaryMode::Final)
    }

    pub(super) fn new_with_body(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: NormalizedSemanticBody<'db>,
        summary_mode: BorrowSummaryMode,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        verify_normalized_semantic_body(db, instance, &body)?;
        let owner = instance.key(db).owner(db);
        let param_modes = match owner {
            BodyOwner::Func(func) => func.params(db).map(|param| param.mode(db)).collect(),
            _ => Vec::new(),
        };
        let mut param_index_of_local = FxHashMap::default();
        for root_id in 0..body.borrow_roots.len() {
            let root_id = NBorrowRootId::from_u32(root_id as u32);
            match body.root(root_id).expect("borrow root") {
                NBorrowRoot::Param { local, param_idx } => {
                    param_index_of_local.insert(*local, *param_idx);
                }
                NBorrowRoot::Provider { .. } | NBorrowRoot::LocalSlot { .. } => {}
            }
        }
        let facts = NormalizedBodyFacts::new(&body);
        let mut checker = Self {
            db,
            instance,
            hir_body: owner.body(db),
            body,
            facts,
            summary_mode,
            param_modes,
            param_index_of_local,
            loan_for_local: FxHashMap::default(),
            param_loan_for_local: FxHashMap::default(),
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
            &self.loan_for_local,
        )
    }

    fn borrow_summary(
        mut self,
    ) -> Result<Option<BorrowSummary<'db>>, SemanticBorrowDiagnostic<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        let typed_body = self.instance.key(self.db).instantiate_typed_body(self.db);
        if typed_body.result_ty().as_borrow(self.db).is_none() || owner.body(self.db).is_none() {
            return Ok(None);
        }
        self.compute_entry_states();
        self.compute_loan_targets()?;
        self.compute_return_summary().map(Some)
    }

    fn check(mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.compute_entry_states();
        self.compute_loan_targets()?;
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
        Ok(())
    }

    fn compute_liveness(&mut self) {
        let live_out = solve_backward_cfg(&mut BorrowLivenessAnalysis::new(self));
        self.live_before = self
            .body
            .blocks
            .iter()
            .map(|block| vec![FxHashSet::default(); block.stmts.len()])
            .collect();
        self.live_before_term = SecondaryMap::new();
        self.live_before_term.resize(self.body.blocks.len());

        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let bb = SBlockId::new(bb_idx);
            let mut live = live_out[bb].0.clone();
            live.extend(self.facts.terminator_uses(bb));
            self.live_before_term[bb] = live.clone();
            for (stmt_idx, _) in block.stmts.iter().enumerate().rev() {
                live = self.live_before_stmt(bb, stmt_idx, &live);
                self.live_before[bb_idx][stmt_idx] = live.clone();
            }
        }
    }

    pub(super) fn live_before_stmt(
        &self,
        block: SBlockId,
        stmt_idx: usize,
        live_after: &FxHashSet<crate::analysis::semantic::SLocalId>,
    ) -> FxHashSet<crate::analysis::semantic::SLocalId> {
        let mut live = live_after.clone();
        let stmt = &self.body.blocks[block.index()].stmts[stmt_idx];
        match &stmt.kind {
            NSStmtKind::Assign { dst, .. } => {
                live.remove(dst);
                live.extend(self.facts.stmt_uses(block, stmt_idx));
            }
            NSStmtKind::Store { .. } => live.extend(self.facts.stmt_uses(block, stmt_idx)),
        }
        live
    }

    fn init_loans(&mut self) {
        for local_id in 0..self.body.locals.len() {
            let local_id = crate::analysis::semantic::SLocalId::from_u32(local_id as u32);
            let Some(local) = self.body.local(local_id) else {
                continue;
            };
            if let Some((kind, _)) = local.ty.as_borrow(self.db)
                && let Some(&param_idx) = self.param_index_of_local.get(&local_id)
                && !matches!(
                    local.lowering,
                    NormalizedBindingLowering::CarrierLocal { .. }
                )
            {
                let loan = LoanId(self.loans.len() as u32);
                let mut targets = FxHashSet::default();
                targets.insert(CanonPlace {
                    root: BorrowRoot::Param(param_idx),
                    proj: NSProjectionPath::default(),
                });
                self.loans.push(Loan {
                    kind,
                    targets,
                    parents: FxHashSet::default(),
                    origin: crate::analysis::semantic::SemOrigin::Body(self.body.template_owner),
                });
                self.param_loan_for_local.insert(local_id, loan);
            }
        }

        for block in &self.body.blocks {
            for stmt in &block.stmts {
                let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                    continue;
                };
                if self
                    .body
                    .local(*dst)
                    .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
                    && matches!(
                        expr,
                        NExpr::Borrow { .. } | NExpr::Call { .. } | NExpr::Use(_)
                    )
                {
                    let kind = self
                        .body
                        .local(*dst)
                        .and_then(|local| local.ty.as_borrow(self.db))
                        .map(|(kind, _)| kind)
                        .expect("borrow local");
                    let loan = LoanId(self.loans.len() as u32);
                    self.loan_for_local.insert(*dst, loan);
                    self.loans.push(Loan {
                        kind,
                        targets: FxHashSet::default(),
                        parents: FxHashSet::default(),
                        origin: stmt.origin,
                    });
                }
            }
        }
    }

    pub(super) fn compute_entry_states(&mut self) {
        self.entry_state = solve_forward_cfg(&mut BorrowEntryStateAnalysis::new(self));
    }

    pub(super) fn compute_loan_targets(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut analysis = BorrowLoanTargetAnalysis::new(
            self.db,
            self.instance,
            &self.body,
            &self.entry_state,
            &self.loan_for_local,
            self.summary_mode,
        );
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)
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
            let bb = SBlockId::new(bb_idx);
            let mut state = self.entry_state[bb].clone();
            let mut moved = self.moved_entry[bb].clone();
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                self.check_stmt(&state, &moved, &self.live_before[bb_idx][stmt_idx], stmt)?;
                self.update_moved_for_stmt(&state, &mut moved, stmt)?;
                self.canon().apply_stmt_state(&mut state, stmt);
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

    fn check_stmt(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = self.effective_loans(state, live);
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => match expr {
                NExpr::ReadPlace { place, mode } => {
                    let targets = self.canon().canonicalize_place(state, place, stmt.origin)?;
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        stmt.origin,
                        "cannot use a value after it was moved",
                    )?;
                    if *mode == ReadMode::Move {
                        self.check_move_out(&active, place, &targets, stmt.origin)?;
                    }
                }
                NExpr::Borrow { place, kind, .. } => {
                    let targets = self.canon().canonicalize_place(state, place, stmt.origin)?;
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        stmt.origin,
                        "cannot borrow a moved value",
                    )?;
                    if let Some(conflict) = self.first_loan_conflict(
                        &active,
                        self.loan_for_local.get(dst).copied(),
                        *kind,
                        &targets,
                    ) {
                        return Err(self.borrow_conflict_diag(
                            stmt.origin,
                            self.overlapping_loans_msg(conflict, *kind),
                            conflict,
                        ));
                    }
                }
                NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                } => {
                    let targets =
                        self.extract_enum_field_move_targets(state, *value, *variant, *field);
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        stmt.origin,
                        "cannot use a value after it was moved",
                    )?;
                    if value.mode == ReadMode::Move {
                        self.check_move_targets_out(&active, &targets, stmt.origin)?;
                    }
                }
                _ => self.check_expr_operands(state, moved, stmt.origin, expr)?,
            },
            NSStmtKind::Store { dst, .. } => {
                let targets = self.canon().canonicalize_place(state, dst, stmt.origin)?;
                self.check_moved_parent(moved, &targets, stmt.origin)?;
            }
        }
        Ok(())
    }

    fn check_terminator(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        term: &super::ir::NSTerminator<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &term.kind {
            NSTerminatorKind::Goto(_) | NSTerminatorKind::Assert { .. } => {}
            NSTerminatorKind::Branch { cond, .. }
            | NSTerminatorKind::MatchEnum { value: cond, .. }
            | NSTerminatorKind::Return(Some(cond)) => {
                let _ = live;
                self.check_operand(
                    state,
                    moved,
                    *cond,
                    term.origin,
                    "cannot use a value after it was moved",
                )?;
            }
            NSTerminatorKind::Return(None) => {}
        }
        if let NSTerminatorKind::Return(Some(value)) = term.kind
            && self
                .body
                .local(value.local)
                .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
            && self
                .canon()
                .borrow_local_targets(state, value.local)
                .is_empty()
        {
            return Err(self.internal_diag(
                term.origin,
                "borrow return local has no tracked loan targets".to_string(),
            ));
        }
        Ok(())
    }

    fn compute_return_summary(&self) -> Result<BorrowSummary<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut out = Vec::new();
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let NSTerminatorKind::Return(Some(value)) = block.terminator.kind else {
                continue;
            };
            let mut state = self.entry_state[SBlockId::new(bb_idx)].clone();
            for stmt in &block.stmts {
                self.canon().apply_stmt_state(&mut state, stmt);
            }
            for target in self.canon().borrow_local_targets(&state, value.local) {
                for proj in target.proj.iter() {
                    if matches!(proj, Projection::Index(IndexSource::Dynamic(_))) {
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
                    BorrowRoot::Local(local) => {
                        let name = self.pretty_local_name(*local);
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

    fn effective_loans(
        &self,
        state: &State,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
    ) -> Vec<LoanId> {
        let active = state
            .local_loans
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
        place: &NSPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let NSPlaceRoot::CarrierDerefLocal(local) = place.root {
            if self.body.locals[local.index()]
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

    pub(super) fn update_moved_for_stmt(
        &self,
        state: &State,
        moved: &mut MovedPlaces<'db>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                if let Some(root) = self
                    .local_root(*dst)
                    .and_then(|root| self.canon().root_to_borrow_root(root))
                {
                    moved.retain(|place, _| place.root != root);
                }
                if let NExpr::ReadPlace {
                    place,
                    mode: ReadMode::Move,
                } = expr
                {
                    let site = MoveSite {
                        origin: stmt.origin,
                        note: "value is moved here".to_string(),
                    };
                    for place in self.canon().canonicalize_place(state, place, stmt.origin)? {
                        moved.insert(place, site.clone());
                    }
                }
                if let NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                } = expr
                {
                    if value.mode == ReadMode::Move {
                        let site = self.move_site(*value, operand_origin(*value, stmt.origin));
                        for place in
                            self.extract_enum_field_move_targets(state, *value, *variant, *field)
                        {
                            moved.insert(place, site.clone());
                        }
                    }
                } else {
                    self.record_expr_moves(state, moved, stmt.origin, expr)?;
                }
            }
            NSStmtKind::Store { dst, .. } => {
                let written = self.canon().canonicalize_place(state, dst, stmt.origin)?;
                moved.retain(|place, _| {
                    !written.iter().any(|written| {
                        written.root == place.root && written.proj.is_prefix_of(&place.proj)
                    })
                });
            }
        }
        Ok(())
    }

    fn extract_enum_field_move_targets(
        &self,
        state: &State,
        source: NOperand,
        variant: crate::analysis::semantic::VariantIndex,
        field: crate::analysis::semantic::FieldIndex,
    ) -> FxHashSet<CanonPlace<'db>> {
        let Some(source_local) = self.body.local(source.local) else {
            return FxHashSet::default();
        };
        let projection = Projection::VariantField {
            variant,
            enum_ty: source_local.ty,
            field_idx: field.0 as usize,
        };
        self.canon()
            .canonicalize_value_base(state, source.local)
            .into_iter()
            .map(|mut target| {
                target.proj.push(projection.clone());
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
        let targets = self.canon().canonicalize_value_base(state, operand.local);
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
        if operand.mode == ReadMode::Move && self.local_has_runtime_move_semantics(operand.local) {
            let site = self.move_site(operand, origin);
            for place in self.canon().canonicalize_value_base(state, operand.local) {
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

    fn local_has_runtime_move_semantics(&self, local: crate::analysis::semantic::SLocalId) -> bool {
        self.body.local(local).is_some_and(|local| {
            local_has_runtime_move_semantics(self.db, local, &self.body.borrow_roots)
        })
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

    fn local_root(&self, local: crate::analysis::semantic::SLocalId) -> Option<NBorrowRootId> {
        self.body.local(local)?.lowering.root()
    }

    fn successors(&self, term: &NSTerminatorKind<'db>) -> BlockAdjacency {
        let mut out = BlockAdjacency::new();
        match term {
            NSTerminatorKind::Goto(bb) => out.push(*bb),
            NSTerminatorKind::Branch {
                then_bb, else_bb, ..
            } => {
                out.push(*then_bb);
                out.push(*else_bb);
            }
            NSTerminatorKind::MatchEnum { cases, default, .. } => {
                out.extend(cases.iter().map(|(_, bb)| *bb));
                if let Some(default) = default {
                    out.push(*default);
                }
            }
            NSTerminatorKind::Assert { .. } | NSTerminatorKind::Return(_) => {}
        }
        out
    }

    pub(super) fn cfg_successor_indices(&self) -> CfgAdjacency {
        let mut successors = CfgAdjacency::new();
        successors.resize(self.body.blocks.len());
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            successors[SBlockId::new(bb_idx)] = self.successors(&block.terminator.kind);
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

    fn pretty_local_name(&self, local: crate::analysis::semantic::SLocalId) -> String {
        self.hir_body
            .zip(self.body.local(local).and_then(|local| local.source))
            .map(|(body, source)| source.pretty_name_in_body(self.db, body))
            .unwrap_or_else(|| format!("%{}", local.index()))
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
