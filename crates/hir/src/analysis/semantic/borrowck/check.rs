use std::collections::{BTreeMap, VecDeque};

use common::diagnostics::CompleteDiagnostic;
use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{solve_backward_cfg, solve_forward_cfg, try_solve_forward_cfg, try_solve_sparse};
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            BorrowActivation, LayoutBackingProjection, SBlockId, SConst, SStmtId, SemConstScalar,
            SemConstValue, SemOrigin, SemanticInstance, SemanticInstanceKey,
            get_or_build_semantic_instance, identity_semantic_instance_key,
        },
        ty::{
            ty_check::{BodyOwner, EffectParamSite},
            ty_def::{BorrowKind, TyId},
            ty_is_borrow,
        },
    },
    core::semantic::EffectEnvView,
    hir_def::{Body, Expr, FuncParamMode, ItemKind, Partial, TopLevelMod},
};

use super::{
    access::{ActiveLoan, CallAccess, MoveSite, MovedPlaces, active_loans_in, effective_loans},
    analyses::{
        BlockAdjacency, BorrowEntryStateAnalysis, BorrowLivenessAnalysis, BorrowLoanTargetAnalysis,
        BorrowLoanTargetState, BorrowMovedStateAnalysis, BorrowSummaryMode, CfgAdjacency,
    },
    canon::BorrowCanonCx,
    diagnostics::operand_origin,
    facts::NormalizedBodyFacts,
    guard::{ExistentialId, Guard, IndexExpr, IndexParamId, IndexSubst, ResultIndexId},
    ir::{
        BorrowDiagnosticId, BorrowSummaryId, NBorrowRoot, NBorrowRootId, NEffectArgValue, NExpr,
        NOperand, NSPlace, NSPlaceRoot, NSStmtKind, NSTerminatorKind, NormalizedSemanticBody,
        ReadMode, SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult,
        local_has_runtime_move_semantics, semantic_projection_ty,
    },
    loan::{AuthoritySet, LoanDef, LoanId, LoanRef, ParentSet},
    normalize::{normalize_provisional_semantic_body, normalize_semantic_body},
    region::{RegionProjection, RegionRoot, RegionSet, SymbolicPlace},
    shape::{SlotPath, SlotProjection, capability_shape, capability_slots},
    summary::{
        BorrowSource, BorrowSourceClause, BorrowSummary, BorrowSummaryLeaf, SummaryPath,
        SummaryProjection, validate_borrow_summary,
    },
    transfer::{
        BorrowState, BorrowStateValueId, BorrowTransferCx, SharedBorrowValueInterner,
        shared_value_interner, slot_loan_value,
    },
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
    if !instance_returns_borrowing_value(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    if instance.key(db).owner(db).body(db).is_none() {
        return SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(
            db,
            conservative_signature_borrow_summary(db, instance),
        )));
    }
    match Borrowck::new_for_summary(db, instance).and_then(Borrowck::borrow_summary) {
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
    if !instance_returns_borrowing_value(db, instance) {
        return SemanticBorrowSummaryResult::Ok(None);
    }
    if instance.key(db).owner(db).body(db).is_none() {
        return SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(
            db,
            conservative_signature_borrow_summary(db, instance),
        )));
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
) -> Result<Option<BorrowSummary>, CompleteDiagnostic> {
    semantic_borrow_summary_voucher(db, instance).map_err(|diag| diag.to_complete(db))
}

pub(super) fn semantic_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
    match semantic_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => {
            Ok(summary.map(|summary| summary.summary(db).clone()))
        }
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub(super) fn provisional_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
    match provisional_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => {
            Ok(summary.map(|summary| summary.summary(db).clone()))
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
    let mut pending = VecDeque::new();
    let mut seen_diags = FxHashSet::default();
    collect_top_mod_semantic_borrow_diagnostic_vouchers(db, top_mod, &mut pending);
    let mut seen_instances = FxHashSet::default();
    while let Some(instance) = pending.pop_front() {
        if !seen_instances.insert(instance.key(db)) {
            continue;
        }
        collect_instance(db, instance, &mut pending, &mut seen_diags, &mut diags);
    }
    diags
}

fn collect_top_mod_semantic_borrow_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    pending: &mut VecDeque<SemanticInstance<'db>>,
) {
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        match item {
            ItemKind::Func(func) => pending.push_back(get_or_build_semantic_instance(
                db,
                identity_semantic_instance_key(db, BodyOwner::Func(*func)),
            )),
            ItemKind::Const(const_) => pending.push_back(get_or_build_semantic_instance(
                db,
                identity_semantic_instance_key(db, BodyOwner::Const(*const_)),
            )),
            ItemKind::Contract(contract) => {
                pending.push_back(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(
                        db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                ));
                for (recv_idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(db).len() {
                        pending.push_back(get_or_build_semantic_instance(
                            db,
                            identity_semantic_instance_key(
                                db,
                                BodyOwner::ContractRecvArm {
                                    contract: *contract,
                                    recv_idx: recv_idx as u32,
                                    arm_idx: arm_idx as u32,
                                },
                            ),
                        ));
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

fn collect_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    pending: &mut VecDeque<SemanticInstance<'db>>,
    seen_diags: &mut FxHashSet<BorrowDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
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
    // Address spaces supplied by effect handles exist only on finalized callee
    // instances. Walk through every reachable specialization because an ordinary
    // generic wrapper may sit between the root and a closure-bearing effect call.
    // Unrelated monomorphizations are left to their parametric identity-owner
    // check above.
    pending.extend(
        instance
            .callees(db)
            .iter()
            .filter(|callee| is_fully_instantiated_key(db, callee.key))
            .map(|callee| get_or_build_semantic_instance(db, callee.key)),
    );
}

fn is_fully_instantiated_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> bool {
    let args_are_concrete = key
        .subst(db)
        .generic_args(db)
        .iter()
        .all(|arg| !arg.has_param(db) && !arg.has_var(db));
    let providers = key.effect_providers(db).providers(db);
    let providers_are_concrete = providers.iter().all(|specialization| {
        let provider = &specialization.provider;
        [
            provider.provider_ty,
            provider.semantics.provider_ty,
            provider.effective_target_ty(),
        ]
        .into_iter()
        .all(|ty| !ty.has_param(db) && !ty.has_var(db))
    });
    let has_all_effect_providers = match key.owner(db) {
        BodyOwner::Func(func) => {
            EffectEnvView::new(EffectParamSite::Func(func))
                .requirements(db)
                .is_empty()
                || !providers.is_empty()
        }
        _ => true,
    };
    args_are_concrete && providers_are_concrete && has_all_effect_providers
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
    pub(super) param_values_for_local:
        FxHashMap<crate::analysis::semantic::SLocalId, BorrowStateValueId<'db>>,
    pub(super) value_interner: SharedBorrowValueInterner<'db>,
    loans: Vec<LoanDef<'db>>,
    pub(super) entry_state: SecondaryMap<SBlockId, BorrowState<'db>>,
    call_result_loans: FxHashMap<SStmtId, Vec<(SummaryPath, LoanId)>>,
    call_loan_sources: FxHashMap<LoanId, Vec<BorrowSourceClause>>,
    constant_indices: SecondaryMap<crate::analysis::semantic::SLocalId, Option<usize>>,
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
        Self::new_with_body(db, instance, body, BorrowSummaryMode::FinalCheck)
    }

    fn new_for_summary(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        let body = normalize_semantic_body(db, instance)?;
        Self::new_with_body(db, instance, body, BorrowSummaryMode::FinalSummary)
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
        let mut constant_candidates = FxHashMap::default();
        let mut stored_locals = FxHashSet::default();
        for stmt in body.blocks.iter().flat_map(|block| &block.stmts) {
            match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => {
                    let value = match expr {
                        NExpr::Const(SConst::Value(value)) => match value.value(db) {
                            SemConstValue::Scalar {
                                value: SemConstScalar::Int { value },
                                ..
                            } => value.to_usize(),
                            _ => None,
                        },
                        _ => None,
                    };
                    constant_candidates
                        .entry(*dst)
                        .and_modify(|candidate| {
                            if *candidate != value {
                                *candidate = None;
                            }
                        })
                        .or_insert(value);
                }
                NSStmtKind::Store {
                    dst:
                        NSPlace {
                            root: NSPlaceRoot::Root(root),
                            ..
                        },
                    ..
                } => match body.root(*root) {
                    Some(NBorrowRoot::Param { local, .. })
                    | Some(NBorrowRoot::LocalSlot { local }) => {
                        stored_locals.insert(*local);
                    }
                    Some(NBorrowRoot::Provider { .. }) | None => {}
                },
                NSStmtKind::Store { .. } => {}
            }
        }
        let mut constant_indices = SecondaryMap::new();
        constant_indices.resize(body.locals.len());
        for (local, value) in constant_candidates {
            if !stored_locals.contains(&local) {
                constant_indices[local] = value;
            }
        }
        let value_interner = shared_value_interner(db);
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
            param_values_for_local: FxHashMap::default(),
            value_interner: value_interner.clone(),
            loans: Vec::new(),
            entry_state: SecondaryMap::with_default(BorrowState::new(value_interner)),
            call_result_loans: FxHashMap::default(),
            call_loan_sources: FxHashMap::default(),
            constant_indices,
            moved_entry: SecondaryMap::new(),
            live_before: Vec::new(),
            live_before_term: SecondaryMap::new(),
        };
        checker.init_loans()?;
        Ok(checker)
    }

    pub(super) fn canon(&self) -> BorrowCanonCx<'_, 'db> {
        BorrowCanonCx::new(
            self.db,
            self.instance,
            &self.body,
            &self.loans,
            &self.constant_indices,
        )
    }

    fn borrow_summary(mut self) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
        let key = self.instance.key(self.db);
        if !instance_returns_borrowing_value(self.db, self.instance)
            || key.owner(self.db).body(self.db).is_none()
        {
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
        if instance_returns_borrowing_value(self.db, self.instance) {
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

    fn init_loans(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for local_id in 0..self.body.locals.len() {
            let local_id = crate::analysis::semantic::SLocalId::from_u32(local_id as u32);
            let Some(local) = self.body.local(local_id) else {
                continue;
            };
            let local_ty = local.ty;
            let Some(&param_idx) = self.param_index_of_local.get(&local_id) else {
                continue;
            };
            let shape = capability_shape(self.db, local_ty);
            let direct_place = match self.instance.key(self.db).owner(self.db) {
                BodyOwner::Func(func) => param_idx == 0 && func.receiver_ty(self.db).is_some(),
                _ => false,
            };
            let mut leaves = Vec::new();
            for slot in capability_slots(self.db, shape, false) {
                let slot_path = slot.path.map_indices(|param| IndexExpr::LoanParam(*param));
                let mut loan = LoanDef::for_slot(
                    slot.kind,
                    &slot.path,
                    BorrowActivation::Immediate,
                    crate::analysis::semantic::SemOrigin::Body(self.body.template_owner),
                );
                let root = if direct_place && slot.path.is_empty() {
                    RegionRoot::ParamPlace(param_idx)
                } else {
                    RegionRoot::ParamCapability {
                        param: param_idx,
                        slot: slot_path.clone(),
                    }
                };
                loan.extend(
                    RegionSet::singleton(SymbolicPlace::new(root, [])),
                    ParentSet::default(),
                );
                let loan = self.allocate_loan(loan);
                leaves.push((slot_path, LoanRef::for_slot(loan, &slot.path)));
            }
            if let Some(value) = slot_loan_value(&self.value_interner, shape, leaves)
                && !self.value_interner.borrow().is_empty(value)
            {
                self.param_values_for_local.insert(local_id, value);
            }
        }

        let stmts = self
            .body
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter().cloned())
            .collect::<Vec<_>>();
        for stmt in stmts {
            let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                continue;
            };
            let Some(result_ty) = self.body.local(*dst).map(|local| local.ty) else {
                continue;
            };
            if let NExpr::Call { callee, args, .. } = expr
                && capability_shape(self.db, result_ty).contains_borrow(self.db)
            {
                let Some(summary) = self.call_borrow_summary(callee.key)? else {
                    continue;
                };
                self.validate_call_borrow_summary(result_ty, args, &summary, stmt.origin)?;
                for leaf in summary.leaves() {
                    let loan = self.allocate_loan(LoanDef::for_summary(
                        leaf.kind,
                        &leaf.path,
                        BorrowActivation::Immediate,
                        stmt.origin,
                    ));
                    self.call_loan_sources.insert(loan, leaf.sources.clone());
                    if ty_is_borrow(self.db, result_ty).is_some() && leaf.path.is_empty() {
                        self.loan_for_local.insert(*dst, loan);
                    } else {
                        self.call_result_loans
                            .entry(stmt.id)
                            .or_default()
                            .push((leaf.path.clone(), loan));
                    }
                }
                continue;
            }

            let direct_loan = match expr {
                NExpr::Borrow {
                    kind, activation, ..
                } => Some((*kind, *activation)),
                NExpr::ReadPlace { .. } | NExpr::Use(_) => ty_is_borrow(self.db, result_ty)
                    .map(|(kind, _)| (kind, BorrowActivation::Immediate)),
                _ => None,
            };
            if let Some((kind, activation)) = direct_loan
                && !matches!(
                    expr,
                    NExpr::ReadPlace { place, .. }
                        if self.read_place_copies_capability(place)
                )
            {
                let loan = self.allocate_loan(LoanDef::plain(kind, activation, stmt.origin));
                self.loan_for_local.insert(*dst, loan);
            }
        }
        Ok(())
    }

    fn read_place_copies_capability(&self, place: &NSPlace<'db>) -> bool {
        self.body
            .place_root_ty(&place.root)
            .and_then(|ty| semantic_projection_ty(self.db, ty, &place.path))
            .is_some_and(|(ty, _)| ty.as_capability(self.db).is_some())
    }

    fn allocate_loan(&mut self, loan: LoanDef<'db>) -> LoanId {
        let id = LoanId(self.loans.len() as u32);
        self.loans.push(loan);
        id
    }

    fn call_borrow_summary(
        &self,
        key: SemanticInstanceKey<'db>,
    ) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
        let instance = get_or_build_semantic_instance(self.db, key);
        match self.summary_mode {
            BorrowSummaryMode::FinalCheck | BorrowSummaryMode::FinalSummary => {
                semantic_borrow_summary_voucher(self.db, instance)
            }
            BorrowSummaryMode::Provisional => provisional_borrow_summary_voucher(self.db, instance),
        }
    }

    fn validate_call_borrow_summary(
        &self,
        result_ty: crate::analysis::ty::ty_def::TyId<'db>,
        args: &[NOperand],
        summary: &BorrowSummary,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let argument_tys = args
            .iter()
            .map(|arg| self.body.local(arg.local).map(|local| local.ty))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| {
                self.internal_diag(
                    origin,
                    "callee borrow summary argument is missing".to_string(),
                )
            })?;
        validate_borrow_summary(self.db, result_ty, &argument_tys, summary)
            .map_err(|message| self.internal_diag(origin, message))
    }

    pub(super) fn compute_entry_states(&mut self) {
        self.entry_state = solve_forward_cfg(&mut BorrowEntryStateAnalysis::new(self));
    }

    pub(super) fn compute_loan_targets(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut analysis = BorrowLoanTargetAnalysis::new(
            self.db,
            &self.body,
            &self.entry_state,
            &self.loan_for_local,
            &self.constant_indices,
            &self.call_result_loans,
            &self.call_loan_sources,
        );
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)
    }

    pub(super) fn apply_stmt_state(
        &self,
        state: &mut BorrowState<'db>,
        stmt: &super::ir::NSStmt<'db>,
    ) {
        BorrowTransferCx::new(
            self.db,
            &self.body,
            &self.loan_for_local,
            &self.constant_indices,
        )
        .apply_stmt(
            state,
            stmt,
            self.call_result_loans.get(&stmt.id).map(Vec::as_slice),
        );
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
                self.apply_stmt_state(&mut state, stmt);
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
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = effective_loans(&self.canon(), &self.loans, state, live);
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                match expr {
                    NExpr::ReadPlace { place, mode } => {
                        self.check_place_read(state, moved, &active, place, *mode, stmt.origin)?;
                    }
                    NExpr::Borrow { place, kind, .. } => {
                        let targets = self.canon().resolve_place(state, place, stmt.origin)?;
                        let authorized = self.canon().authority_for_place(state, place);
                        self.check_moved_overlap(
                            moved,
                            &targets,
                            &authorized,
                            stmt.origin,
                            "cannot borrow a moved value",
                        )?;
                        let loan = self.loan_for_local.get(dst).copied();
                        if let Some(kind) =
                            loan.map_or(Some(*kind), |loan| self.loan_conflict_kind(loan))
                        {
                            let reference = loan.map(LoanRef::new);
                            self.check_loan_conflict(
                                &active,
                                reference.as_ref(),
                                kind,
                                &targets,
                                stmt.origin,
                            )?;
                        }
                    }
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => {
                        let targets =
                            self.extract_enum_field_move_region(state, *value, *variant, *field);
                        let authorized =
                            self.canon()
                                .authority_for_value_targets(state, value.local, &targets);
                        self.check_moved_overlap(
                            moved,
                            &targets,
                            &authorized,
                            stmt.origin,
                            "cannot use a value after it was moved",
                        )?;
                        if value.mode == ReadMode::Move {
                            self.check_move_targets_out(
                                &active,
                                &authorized,
                                &targets,
                                stmt.origin,
                            )?;
                        } else {
                            self.check_read_targets(&active, &authorized, &targets, stmt.origin)?;
                        }
                    }
                    _ => {
                        let expression_moved =
                            self.check_expr_operands(state, moved, &active, stmt.origin, expr)?;
                        let mut call_accesses =
                            self.check_call_argument_accesses(state, &active, stmt.origin, expr)?;
                        self.check_effect_place_accesses(
                            state,
                            &expression_moved,
                            &active,
                            stmt.origin,
                            expr,
                            &mut call_accesses,
                        )?;
                    }
                }
                if matches!(expr, NExpr::Call { .. } | NExpr::Use(_)) {
                    self.check_assigned_loan_conflicts(&active, stmt.id, *dst, stmt.origin)?;
                }
                self.check_assignment_write(state, &active, *dst, stmt.origin)?;
            }
            NSStmtKind::Store { dst, src } => {
                self.check_operand(
                    state,
                    moved,
                    &active,
                    *src,
                    stmt.origin,
                    "cannot use a value after it was moved",
                )?;
                let targets = self.canon().resolve_place(state, dst, stmt.origin)?;
                let mut authorized = self
                    .canon()
                    .mut_authority_for_place_targets(state, dst, &targets);
                if src.mode == ReadMode::Move {
                    authorized.union(
                        self.canon()
                            .mut_authority_for_value_targets(state, src.local, &targets),
                    );
                }
                self.check_moved_parent(moved, &targets, &authorized, stmt.origin)?;
                self.check_write_targets(&active, &authorized, &targets, stmt.origin)?;
            }
        }
        Ok(())
    }

    fn check_assignment_write(
        &self,
        state: &BorrowState<'db>,
        active: &[ActiveLoan<'db>],
        dst: crate::analysis::semantic::SLocalId,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(place) = self
            .body
            .local(dst)
            .filter(|local| local.source.is_some_and(|binding| binding.is_mut()))
            .and_then(|local| local.lowering.place())
        else {
            return Ok(());
        };
        let targets = self.canon().resolve_place(state, place, origin)?;
        self.check_write_targets(active, &AuthoritySet::default(), &targets, origin)
    }

    fn check_place_read(
        &self,
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        place: &NSPlace<'db>,
        mode: ReadMode,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let targets = self.canon().resolve_place(state, place, origin)?;
        let authorized = self.canon().authority_for_place(state, place);
        self.check_moved_overlap(
            moved,
            &targets,
            &authorized,
            origin,
            "cannot use a value after it was moved",
        )?;
        if mode == ReadMode::Move {
            self.check_move_out(active, &authorized, place, &targets, origin)
        } else {
            self.check_read_targets(active, &authorized, &targets, origin)
        }
    }

    fn check_effect_place_accesses(
        &self,
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        origin: SemOrigin<'db>,
        expr: &NExpr<'db>,
        accesses: &mut Vec<CallAccess<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let NExpr::Call {
            args, effect_args, ..
        } = expr
        else {
            return Ok(());
        };
        for (idx, effect_arg) in effect_args.iter().enumerate() {
            let group = args.len() + idx;
            let (targets, authorized, arg_origin) = match &effect_arg.arg {
                NEffectArgValue::Place(place) => {
                    let targets = self.canon().resolve_place(state, place, origin)?;
                    let authorized = if effect_arg.required_mut {
                        self.canon().mut_authority_for_place(state, place)
                    } else {
                        self.canon().authority_for_place(state, place)
                    };
                    self.check_moved_overlap(
                        moved,
                        &targets,
                        &authorized,
                        origin,
                        "cannot use a value after it was moved",
                    )?;
                    (targets, authorized, origin)
                }
                NEffectArgValue::Value(value) => {
                    let targets = self.canon().value_region(state, value.local);
                    let authorized = if effect_arg.required_mut {
                        self.canon()
                            .mut_authority_for_value_targets(state, value.local, &targets)
                    } else {
                        self.canon()
                            .authority_for_value_targets(state, value.local, &targets)
                    };
                    (targets, authorized, operand_origin(*value, origin))
                }
            };
            if effect_arg.required_mut {
                self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                self.record_call_access(
                    accesses,
                    group,
                    None,
                    BorrowKind::Mut,
                    targets,
                    arg_origin,
                )?;
            } else {
                self.check_read_targets(active, &authorized, &targets, arg_origin)?;
                self.record_call_access(
                    accesses,
                    group,
                    None,
                    BorrowKind::Ref,
                    targets,
                    arg_origin,
                )?;
            }

            let target_ty = effect_arg.target_ty.or_else(|| match &effect_arg.arg {
                NEffectArgValue::Value(value) => self.body.local(value.local).map(|local| local.ty),
                NEffectArgValue::Place(_) => None,
            });
            let Some(target_ty) = target_ty else {
                continue;
            };
            let shape = capability_shape(self.db, target_ty);
            for slot in capability_slots(self.db, shape, false) {
                let projection = layout_path_for_slot_template(&slot.path);
                let targets = match &effect_arg.arg {
                    NEffectArgValue::Place(place) => {
                        self.canon()
                            .place_layout_region(state, place, target_ty, &projection)
                    }
                    NEffectArgValue::Value(value) => {
                        self.canon()
                            .value_layout_region(state, value.local, &projection)
                    }
                };
                let authorized = match &effect_arg.arg {
                    NEffectArgValue::Place(place) if slot.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_authority_for_place_targets(state, place, &targets),
                    NEffectArgValue::Place(place) => self
                        .canon()
                        .authority_for_place_targets(state, place, &targets),
                    NEffectArgValue::Value(value) if slot.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_authority_for_value_targets(state, value.local, &targets),
                    NEffectArgValue::Value(value) => {
                        self.canon()
                            .authority_for_value_targets(state, value.local, &targets)
                    }
                };
                if slot.kind == BorrowKind::Mut {
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                } else {
                    self.check_read_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(
                    accesses,
                    group,
                    Some(&slot.path),
                    slot.kind,
                    targets,
                    arg_origin,
                )?;
            }
        }
        Ok(())
    }

    fn check_call_argument_accesses(
        &self,
        state: &BorrowState<'db>,
        active: &[ActiveLoan<'db>],
        origin: SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<Vec<CallAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let NExpr::Call { callee, args, .. } = expr else {
            return Ok(Vec::new());
        };
        let instance = get_or_build_semantic_instance(self.db, callee.key);
        let BodyOwner::Func(func) = callee.key.owner(self.db) else {
            return Ok(Vec::new());
        };
        let mut accesses = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().copied().enumerate() {
            let Some(param) = func.params(self.db).nth(idx) else {
                return Err(
                    self.internal_diag(origin, format!("callee is missing value parameter {idx}"))
                );
            };
            let ty = instance.normalized_ty(self.db, param.ty(self.db));
            let moves_value =
                arg.mode == ReadMode::Move && self.local_has_runtime_move_semantics(arg.local);
            let arg_origin = operand_origin(arg, origin);
            let mutably_passed_by_place =
                param.mode(self.db) != FuncParamMode::Own && param.is_mut(self.db);
            if ty.as_borrow(self.db).is_none()
                && (arg.mode != ReadMode::Copy || mutably_passed_by_place)
            {
                let kind = if mutably_passed_by_place || moves_value {
                    BorrowKind::Mut
                } else {
                    BorrowKind::Ref
                };
                let targets = self.canon().value_region(state, arg.local);
                if kind == BorrowKind::Mut && !moves_value {
                    let authorized = self
                        .canon()
                        .mut_authority_for_value_targets(state, arg.local, &targets);
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(&mut accesses, idx, None, kind, targets, arg_origin)?;
            }
            let shape = capability_shape(self.db, ty);
            for slot in capability_slots(self.db, shape, false) {
                let projection = layout_path_for_slot_template(&slot.path);
                let targets = self
                    .canon()
                    .value_layout_region(state, arg.local, &projection);
                let authorized = if slot.kind == BorrowKind::Mut {
                    self.canon()
                        .mut_authority_for_value_targets(state, arg.local, &targets)
                } else {
                    self.canon()
                        .authority_for_value_targets(state, arg.local, &targets)
                };
                if slot.kind == BorrowKind::Mut {
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                } else {
                    self.check_read_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(
                    &mut accesses,
                    idx,
                    Some(&slot.path),
                    slot.kind,
                    targets,
                    arg_origin,
                )?;
            }
        }
        Ok(accesses)
    }

    fn record_call_access(
        &self,
        accesses: &mut Vec<CallAccess<'db>>,
        group: usize,
        projection: Option<&SlotPath<IndexParamId>>,
        kind: BorrowKind,
        targets: RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        // An argument owns its container and the capability slots stored inside
        // it, but distinct capability slots must still obey aliasing rules.
        let conflict = accesses
            .iter()
            .find(|access| access.conflicts_with(group, projection, kind, &targets));
        if let Some(conflict) = conflict {
            let mut diag = SemanticBorrowDiagnostic::new(
                self.instance,
                SemanticBorrowDiagKind::BorrowConflict,
                "call arguments require conflicting access to the same place".to_string(),
                SemanticBorrowDiagnosticSpan::Origin {
                    owner: self.instance.key(self.db).owner(self.db),
                    origin,
                },
            );
            self.push_secondary_origin(
                &mut diag,
                conflict.origin(),
                "overlapping argument access occurs here".to_string(),
            );
            return Err(diag);
        }
        if !targets.is_empty() {
            accesses.push(CallAccess::new(
                group,
                projection.cloned(),
                kind,
                targets,
                origin,
            ));
        }
        Ok(())
    }

    fn check_assigned_loan_conflicts(
        &self,
        active: &[ActiveLoan<'db>],
        stmt: SStmtId,
        local: crate::analysis::semantic::SLocalId,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut references = self
            .loan_for_local
            .get(&local)
            .copied()
            .map(LoanRef::new)
            .into_iter()
            .collect::<Vec<_>>();
        references.extend(
            self.call_result_loans
                .get(&stmt)
                .into_iter()
                .flatten()
                .map(|(path, loan)| LoanRef::for_summary(*loan, path)),
        );
        for reference in references {
            let loan = &self.loans[reference.id.0 as usize];
            let targets = self
                .canon()
                .active_region_for_held(&reference, &Guard::always());
            self.check_loan_conflict(active, Some(&reference), loan.kind(), &targets, origin)?;
        }
        Ok(())
    }

    fn check_loan_conflict(
        &self,
        active: &[ActiveLoan<'db>],
        new_loan: Option<&LoanRef>,
        kind: BorrowKind,
        targets: &RegionSet<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some(conflict) = self.first_loan_conflict(active, new_loan, kind, targets) {
            return Err(self.borrow_conflict_diag(
                origin,
                self.overlapping_loans_msg(conflict, kind),
                conflict,
            ));
        }
        Ok(())
    }

    fn check_read_targets(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &AuthoritySet,
        targets: &RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().find(|loan| {
            !authorized.matches(loan.reference(), loan.holder_guard())
                && self.loan_conflict_kind(loan.id()) == Some(BorrowKind::Mut)
                && loan.overlaps(targets)
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot read this place while a mutable borrow is active".to_string(),
                conflict.id(),
            ));
        }
        Ok(())
    }

    fn check_write_targets(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &AuthoritySet,
        targets: &RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().find(|loan| {
            !authorized.matches(loan.reference(), loan.holder_guard())
                && self.loan_conflict_kind(loan.id()).is_some()
                && loan.overlaps(targets)
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot write to this place while it is borrowed".to_string(),
                conflict.id(),
            ));
        }
        Ok(())
    }

    fn check_terminator(
        &self,
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        term: &super::ir::NSTerminator<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &term.kind {
            NSTerminatorKind::Goto(_) | NSTerminatorKind::Assert { .. } => {}
            NSTerminatorKind::Branch { cond, .. } | NSTerminatorKind::Return(Some(cond)) => {
                let active = effective_loans(&self.canon(), &self.loans, state, live);
                self.check_operand(
                    state,
                    moved,
                    &active,
                    *cond,
                    term.origin,
                    "cannot use a value after it was moved",
                )?;
            }
            NSTerminatorKind::MatchEnum { value, .. } => {
                let active = effective_loans(&self.canon(), &self.loans, state, live);
                self.check_operand(
                    state,
                    moved,
                    &active,
                    NOperand {
                        mode: ReadMode::Read,
                        ..*value
                    },
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
                .is_some_and(|local| ty_is_borrow(self.db, local.ty).is_some())
            && self
                .canon()
                .borrow_local_region(state, value.local)
                .is_empty()
        {
            return Err(self.internal_diag(
                term.origin,
                "borrow return local has no tracked loan targets".to_string(),
            ));
        }
        if let NSTerminatorKind::Return(Some(value)) = term.kind
            && self
                .body
                .local(value.local)
                .is_some_and(|local| ty_is_borrow(self.db, local.ty).is_none())
        {
            for loan in active_loans_in(&self.canon(), state, value.local) {
                let region = self.resolve_return_region(state, loan.region(), term.origin)?;
                if let Some(local) = region.guarded_places().find_map(|(_, target)| {
                    if let RegionRoot::Local(local) = target.root() {
                        Some(*local)
                    } else {
                        None
                    }
                }) {
                    let name = self.pretty_local_name(local);
                    let mut diag = self.invalid_return_diag(
                        term.origin,
                        format!("cannot return a value that holds a borrow of local `{name}`"),
                    );
                    self.push_secondary_origin(
                        &mut diag,
                        self.loan_origin(loan.id()),
                        "borrow created here".to_string(),
                    );
                    return Err(diag);
                }
            }
        }
        Ok(())
    }

    fn resolve_return_region(
        &self,
        state: &BorrowState<'db>,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<RegionSet<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut pending = region.clauses().collect::<VecDeque<_>>();
        let mut seen = FxHashSet::default();
        let mut resolved = RegionSet::empty();
        while let Some(target) = pending.pop_front() {
            if !seen.insert(target.clone()) {
                continue;
            }
            let (guard, place) = target
                .guarded_places()
                .next()
                .expect("a split region contains one clause");
            let RegionRoot::Local(local) = place.root() else {
                resolved = resolved.union(&target);
                continue;
            };
            let Some(local) = self.body.local(*local) else {
                resolved = resolved.union(&target);
                continue;
            };
            // Snapshot provenance describes a value's physical source. Layout
            // backing is deliberately not used here: it can point at the
            // argument that supplied a fresh aggregate field's layout without
            // making that freshly allocated field an alias of the argument.
            let Some(source) = local.snapshot_source_place() else {
                resolved = resolved.union(&target);
                continue;
            };
            let source = self
                .canon()
                .resolve_place(state, source, origin)?
                .project(place.projection())
                .with_guard(guard);
            let mut advanced = false;
            for source in source.clauses() {
                if source == target {
                    continue;
                }
                advanced = true;
                pending.push_back(source);
            }
            if !advanced {
                resolved = resolved.union(&target);
            }
        }
        Ok(resolved)
    }

    fn compute_return_summary(&self) -> Result<BorrowSummary, SemanticBorrowDiagnostic<'db>> {
        let mut out = BTreeMap::<(BorrowKind, SummaryPath), Vec<BorrowSourceClause>>::new();
        let mut families = BTreeMap::new();
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let NSTerminatorKind::Return(Some(value)) = block.terminator.kind else {
                continue;
            };
            let mut state = self.entry_state[SBlockId::new(bb_idx)].clone();
            for stmt in &block.stmts {
                self.apply_stmt_state(&mut state, stmt);
            }
            let origin = block.terminator.origin;
            for leaf in state.leaves_in(value.local, super::guard::ValueScope::Summary) {
                let kind = self.loans[leaf.payload.id.0 as usize].kind();
                let (path, subst) = summary_path_for_leaf(&leaf.path, &mut families);
                let held = leaf.payload.substitute(&subst);
                let Some(guard) = leaf.payload_guard.substitute(&subst) else {
                    continue;
                };
                let region = self.canon().active_region_for_held(&held, &guard);
                if region.is_empty() {
                    if self.summary_mode != BorrowSummaryMode::FinalCheck {
                        out.entry((kind, path)).or_default();
                        continue;
                    }
                    return Err(self.internal_diag(
                        origin,
                        format!("borrow result slot {:?} has no tracked source", path),
                    ));
                }
                let region = self.resolve_return_region(&state, &region, origin)?;
                for (source_guard, target) in region.guarded_places() {
                    let (source_guard, target) =
                        self.normalize_summary_source(source_guard, target, origin)?;
                    match target.root() {
                        RegionRoot::ParamPlace(idx) => {
                            let source_path =
                                summary_path_for_region_projection(target.projection());
                            out.entry((kind, path.clone()))
                                .or_default()
                                .push(BorrowSourceClause {
                                    guard: source_guard.clone(),
                                    source: BorrowSource::ParamPlace {
                                        param: *idx,
                                        path: source_path,
                                    },
                                });
                        }
                        RegionRoot::ParamCapability { param, slot } => {
                            out.entry((kind, path.clone()))
                                .or_default()
                                .push(BorrowSourceClause {
                                    guard: source_guard.clone(),
                                    source: BorrowSource::ParamCapability {
                                        param: *param,
                                        slot: summary_path_for_slot(slot),
                                    },
                                });
                        }
                        RegionRoot::Provider(_) => {
                            return Err(self.invalid_return_diag(
                                origin,
                                "cannot return a borrow derived from an effect parameter"
                                    .to_string(),
                            ));
                        }
                        RegionRoot::Local(local) => {
                            let name = self.pretty_local_name(*local);
                            return Err(self.invalid_return_diag(
                                origin,
                                format!("cannot return a borrow to local `{name}`"),
                            ));
                        }
                    }
                }
            }
        }
        Ok(BorrowSummary::new(
            out.into_iter()
                .map(|((kind, path), sources)| BorrowSummaryLeaf::new(kind, path, sources))
                .collect(),
        ))
    }

    fn normalize_summary_source(
        &self,
        guard: &Guard,
        place: &SymbolicPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(Guard, SymbolicPlace<'db>), SemanticBorrowDiagnostic<'db>> {
        let mut expressions = guard
            .index_exprs()
            .into_iter()
            .chain(place.index_exprs())
            .collect::<Vec<_>>();
        expressions.sort_unstable();
        expressions.dedup();
        let mut next_existential = expressions
            .iter()
            .filter_map(|expr| match expr {
                IndexExpr::Existential(id) => Some(id.0),
                _ => None,
            })
            .max()
            .and_then(|id| id.checked_add(1))
            .unwrap_or(0);
        let mut subst = IndexSubst::new();
        for expression in expressions {
            match expression {
                IndexExpr::Runtime(local) => {
                    let replacement = self.param_index_of_local.get(&local).copied().map_or_else(
                        || {
                            let existential = ExistentialId(next_existential);
                            next_existential = next_existential
                                .checked_add(1)
                                .expect("summary existential space exhausted");
                            IndexExpr::Existential(existential)
                        },
                        IndexExpr::InputParam,
                    );
                    subst.insert(expression, replacement);
                }
                IndexExpr::ValueParam(_) | IndexExpr::LoanParam(_) => {
                    return Err(self.internal_diag(
                        origin,
                        "borrow summary contains an unbound internal index".to_string(),
                    ));
                }
                IndexExpr::Const(_)
                | IndexExpr::ResultParam(_)
                | IndexExpr::InputParam(_)
                | IndexExpr::Existential(_) => {}
            }
        }
        let guard = guard.substitute(&subst).ok_or_else(|| {
            self.internal_diag(
                origin,
                "borrow summary source has contradictory index constraints".to_string(),
            )
        })?;
        Ok((guard, place.substitute(&subst)))
    }

    fn first_loan_conflict(
        &self,
        active: &[ActiveLoan<'db>],
        new_loan: Option<&LoanRef>,
        new_kind: BorrowKind,
        targets: &RegionSet<'db>,
    ) -> Option<LoanId> {
        let reborrow_parents = new_loan.map(|reference| {
            self.loans[reference.id.0 as usize].instantiate_parents(reference, &Guard::always())
        });
        active
            .iter()
            .filter(|loan| {
                reborrow_parents.as_ref().is_none_or(|parents| {
                    !parents
                        .iter()
                        .any(|parent| loan.matches(parent.reference(), parent.guard()).is_some())
                })
            })
            .find(|loan| {
                self.loan_conflict_kind(loan.id()).is_some_and(|kind| {
                    !matches!((kind, new_kind), (BorrowKind::Ref, BorrowKind::Ref))
                }) && loan.overlaps(targets)
            })
            .map(ActiveLoan::id)
    }

    fn loan_conflict_kind(&self, loan: LoanId) -> Option<BorrowKind> {
        let loan = &self.loans[loan.0 as usize];
        // Receiver reservations remain dormant while later arguments are
        // evaluated. The call-access checks perform their activation.
        if loan.activation() == BorrowActivation::AtCall {
            None
        } else {
            Some(loan.kind())
        }
    }

    fn check_move_out(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &AuthoritySet,
        place: &NSPlace<'db>,
        targets: &RegionSet<'db>,
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
        self.check_move_targets_out(active, authorized, targets, origin)?;
        Ok(())
    }

    fn check_move_targets_out(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &AuthoritySet,
        targets: &RegionSet<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for (_, target) in targets.guarded_places() {
            if let RegionRoot::ParamPlace(idx) = target.root()
                && self
                    .param_modes
                    .get(*idx as usize)
                    .copied()
                    .is_some_and(|mode| mode == FuncParamMode::View)
            {
                return Err(self.move_conflict_diag(
                    origin,
                    "cannot move out of a view parameter".to_string(),
                ));
            }
        }
        if let Some(loan) = active.iter().find(|loan| {
            !authorized.matches(loan.reference(), loan.holder_guard()) && loan.overlaps(targets)
        }) {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot move out of a value while it is borrowed".to_string(),
                loan.id(),
            ));
        }
        Ok(())
    }

    pub(super) fn update_moved_for_stmt(
        &self,
        state: &BorrowState<'db>,
        moved: &mut MovedPlaces<'db>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                if let Some(root) = self
                    .local_root(*dst)
                    .and_then(|root| self.canon().root_to_region_root(root))
                {
                    moved.retain(|region, _| !region.has_root(&root));
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
                    self.record_move_region(
                        moved,
                        self.canon().resolve_place(state, place, stmt.origin)?,
                        site,
                    );
                }
                if let NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                } = expr
                {
                    if value.mode == ReadMode::Move {
                        let site = self.move_site(*value, operand_origin(*value, stmt.origin));
                        self.record_move_region(
                            moved,
                            self.extract_enum_field_move_region(state, *value, *variant, *field),
                            site,
                        );
                    }
                } else {
                    self.record_expr_moves(state, moved, stmt.origin, expr)?;
                }
            }
            NSStmtKind::Store { dst, src } => {
                self.record_operand_move(state, moved, *src, stmt.origin)?;
                let written = self.canon().resolve_place(state, dst, stmt.origin)?;
                moved.retain(|region, _| !written.provably_covers(region));
            }
        }
        Ok(())
    }

    fn extract_enum_field_move_region(
        &self,
        state: &BorrowState<'db>,
        source: NOperand,
        variant: crate::analysis::semantic::VariantIndex,
        field: crate::analysis::semantic::FieldIndex,
    ) -> RegionSet<'db> {
        self.canon()
            .value_region(state, source.local)
            .project(&[super::region::RegionProjection::VariantField { variant, field }])
    }

    fn check_expr_operands(
        &self,
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        origin: crate::analysis::semantic::SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<MovedPlaces<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut moved = moved.clone();
        expr.try_for_each_value_operand(|value| {
            self.check_operand(
                state,
                &moved,
                active,
                value,
                origin,
                "cannot use a value after it was moved",
            )?;
            self.record_operand_move(state, &mut moved, value, origin)
        })?;
        Ok(moved)
    }

    fn check_operand(
        &self,
        state: &BorrowState<'db>,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        operand: NOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        let targets = self.canon().value_region(state, operand.local);
        if targets.is_empty() {
            return Ok(());
        }
        let authorized = self
            .canon()
            .authority_for_value_targets(state, operand.local, &targets);
        self.check_moved_overlap(moved, &targets, &authorized, origin, message)?;
        if operand.mode == ReadMode::Move && self.local_has_runtime_move_semantics(operand.local) {
            self.check_move_targets_out(active, &authorized, &targets, origin)
        } else {
            self.check_read_targets(active, &authorized, &targets, origin)
        }
    }

    fn record_expr_moves(
        &self,
        state: &BorrowState<'db>,
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
        state: &BorrowState<'db>,
        moved: &mut MovedPlaces<'db>,
        operand: NOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        if operand.mode == ReadMode::Move && self.local_has_runtime_move_semantics(operand.local) {
            let site = self.move_site(operand, origin);
            self.record_move_region(moved, self.canon().value_region(state, operand.local), site);
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
        accessed: &RegionSet<'db>,
        authorized: &AuthoritySet,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            accessed.clauses().any(|accessed| {
                moved.may_overlap(&accessed).is_some()
                    && !self.loan_authorizes_access(authorized, &accessed)
            })
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
        written: &RegionSet<'db>,
        authorized: &AuthoritySet,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            written.clauses().any(|written| {
                moved.provably_covers(&written)
                    && !written.provably_covers(moved)
                    && !self.loan_authorizes_access(authorized, &written)
            })
        }) {
            let mut diag =
                self.move_conflict_diag(origin, "cannot write through a moved value".to_string());
            self.push_secondary_origin(&mut diag, site.origin, site.note.clone());
            return Err(diag);
        }
        Ok(())
    }

    fn loan_authorizes_access(&self, authorized: &AuthoritySet, accessed: &RegionSet<'db>) -> bool {
        authorized.iter().any(|authority| {
            self.loans[authority.reference().id.0 as usize]
                .instantiate(authority.reference())
                .with_guard(authority.guard())
                .provably_covers(accessed)
        })
    }

    fn record_move_region(
        &self,
        moved: &mut MovedPlaces<'db>,
        region: RegionSet<'db>,
        site: MoveSite<'db>,
    ) {
        for clause in region.clauses() {
            moved.insert(clause, site.clone());
        }
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
            self.loan_origin(loan),
            "borrow created here".to_string(),
        );
        diag
    }

    fn loan_origin(&self, mut loan: LoanId) -> SemOrigin<'db> {
        let mut seen = FxHashSet::default();
        while seen.insert(loan) {
            let data = &self.loans[loan.0 as usize];
            let Some(parent) = data.parents().iter().next() else {
                return data.origin();
            };
            if data.parents().iter().nth(1).is_some() {
                return data.origin();
            }
            loan = parent.reference().id;
        }
        self.loans[loan.0 as usize].origin()
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
        match (
            new_kind,
            self.loan_conflict_kind(loan)
                .expect("dormant loans do not produce conflicts"),
        ) {
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
        instance_returns_borrowing_value(db, instance)
            .then(|| BorrowSummaryId::new(db, empty_signature_borrow_summary(db, instance))),
    )
}

fn instance_returns_borrowing_value<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    !capability_slots(
        db,
        capability_shape(db, instance.normalized_result_ty(db)),
        true,
    )
    .is_empty()
}

fn summary_path_for_leaf(
    path: &SlotPath<IndexExpr>,
    families: &mut BTreeMap<IndexExpr, ResultIndexId>,
) -> (SummaryPath, IndexSubst) {
    let mut subst = IndexSubst::new();
    let projection = path
        .as_slice()
        .iter()
        .map(|step| match step {
            SlotProjection::Field(field) => SummaryProjection::Field(field.index()),
            SlotProjection::VariantField { variant, field } => SummaryProjection::VariantField {
                variant: *variant,
                field: *field,
            },
            SlotProjection::Index(IndexExpr::Const(index)) => {
                SummaryProjection::Index(IndexExpr::Const(*index))
            }
            SlotProjection::Index(index) => {
                let next = ResultIndexId(
                    u32::try_from(families.len()).expect("borrow result family space exhausted"),
                );
                let family = *families.entry(*index).or_insert(next);
                subst.insert(*index, IndexExpr::ResultParam(family));
                SummaryProjection::Index(IndexExpr::ResultParam(family))
            }
        })
        .collect::<Vec<_>>();
    (SummaryPath::from_steps(projection), subst)
}

fn summary_path_for_slot(path: &SlotPath<IndexExpr>) -> SummaryPath {
    SummaryPath::from_steps(path.as_slice().iter().map(|projection| match projection {
        SlotProjection::Field(field) => SummaryProjection::Field(field.index()),
        SlotProjection::VariantField { variant, field } => SummaryProjection::VariantField {
            variant: *variant,
            field: *field,
        },
        SlotProjection::Index(index) => SummaryProjection::Index(*index),
    }))
}

fn summary_path_for_slot_template(
    path: &SlotPath<super::guard::IndexParamId>,
    mut index: impl FnMut(super::guard::IndexParamId) -> IndexExpr,
) -> SummaryPath {
    SummaryPath::from_steps(path.as_slice().iter().map(|projection| match projection {
        SlotProjection::Field(field) => SummaryProjection::Field(field.index()),
        SlotProjection::VariantField { variant, field } => SummaryProjection::VariantField {
            variant: *variant,
            field: *field,
        },
        SlotProjection::Index(param) => SummaryProjection::Index(index(*param)),
    }))
}

fn layout_path_for_slot_template(path: &SlotPath<IndexParamId>) -> Vec<LayoutBackingProjection> {
    path.as_slice()
        .iter()
        .map(|projection| match projection {
            SlotProjection::Field(field) => LayoutBackingProjection::Field(field.index()),
            SlotProjection::VariantField { variant, field } => {
                LayoutBackingProjection::VariantField {
                    variant: *variant,
                    field: *field,
                }
            }
            SlotProjection::Index(_) => LayoutBackingProjection::Index(None),
        })
        .collect()
}

fn summary_path_for_region_projection(path: &[RegionProjection]) -> SummaryPath {
    SummaryPath::from_steps(path.iter().map(|projection| match projection {
        RegionProjection::Field(field) => SummaryProjection::Field(*field),
        RegionProjection::VariantField { variant, field } => SummaryProjection::VariantField {
            variant: *variant,
            field: *field,
        },
        RegionProjection::Index(index) => SummaryProjection::Index(*index),
    }))
}

fn empty_signature_borrow_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> BorrowSummary {
    let shape = capability_shape(db, instance.normalized_result_ty(db));
    BorrowSummary::new(
        capability_slots(db, shape, true)
            .into_iter()
            .map(|slot| {
                BorrowSummaryLeaf::new(
                    slot.kind,
                    summary_path_for_slot_template(&slot.path, |param| {
                        IndexExpr::ResultParam(ResultIndexId(param.0))
                    }),
                    Vec::new(),
                )
            })
            .collect(),
    )
}

fn conservative_signature_borrow_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> BorrowSummary {
    let results = capability_slots(
        db,
        capability_shape(db, instance.normalized_result_ty(db)),
        true,
    );
    let inputs = match instance.key(db).owner(db) {
        BodyOwner::Func(func) => func
            .params(db)
            .filter_map(|param| {
                u32::try_from(param.index()).ok().map(|idx| {
                    let ty = instance.normalized_ty(db, param.ty(db));
                    let slots = capability_slots(db, capability_shape(db, ty), false);
                    (idx, ty, param.is_mut(db), slots)
                })
            })
            .collect::<Vec<_>>(),
        _ => Vec::new(),
    };
    let mut next_existential = 0_u32;
    let mut summary = Vec::new();
    for result in results {
        let result_path = summary_path_for_slot_template(&result.path, |param| {
            IndexExpr::ResultParam(ResultIndexId(param.0))
        });
        let mut sources = Vec::new();
        for (idx, ty, is_mut, input_results) in &inputs {
            if signature_input_is_unresolved(db, *ty) {
                sources.push(BorrowSourceClause {
                    guard: super::guard::Guard::always(),
                    source: BorrowSource::AnyAccessible {
                        param: *idx,
                        class: match result.kind {
                            BorrowKind::Ref => super::summary::AccessClass::Shared,
                            BorrowKind::Mut => super::summary::AccessClass::Mutable,
                        },
                    },
                });
                continue;
            }
            if result.kind == BorrowKind::Ref || *is_mut {
                sources.push(BorrowSourceClause {
                    guard: super::guard::Guard::always(),
                    source: BorrowSource::ParamPlace {
                        param: *idx,
                        path: SummaryPath::new(),
                    },
                });
            }
            for input in input_results {
                if result.kind == BorrowKind::Ref || input.kind == BorrowKind::Mut {
                    let slot = summary_path_for_slot_template(&input.path, |_| {
                        let existential = ExistentialId(next_existential);
                        next_existential = next_existential
                            .checked_add(1)
                            .expect("summary existential space exhausted");
                        IndexExpr::Existential(existential)
                    });
                    sources.push(BorrowSourceClause {
                        guard: super::guard::Guard::always(),
                        source: BorrowSource::ParamCapability { param: *idx, slot },
                    });
                }
            }
        }
        summary.push(BorrowSummaryLeaf::new(result.kind, result_path, sources));
    }
    BorrowSummary::new(summary)
}

fn signature_input_is_unresolved(db: &dyn HirAnalysisDb, input_ty: TyId<'_>) -> bool {
    input_ty.has_param(db)
        || input_ty.has_var(db)
        || input_ty.has_projection(db)
        || input_ty.has_invalid(db)
}

fn semantic_borrow_summary_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &SemanticBorrowSummaryResult<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}
