use std::collections::VecDeque;

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
            BorrowActivation, LayoutBackingProjection, SBlockId, SConst, SLocalId, SStmtId,
            SemConstScalar, SemConstValue, SemOrigin, SemanticInstance, SemanticInstanceKey,
            get_or_build_semantic_instance, identity_semantic_instance_key,
        },
        ty::{
            ty_check::{BodyOwner, EffectParamSite, LocalBinding},
            ty_def::{BorrowKind, TyId},
            ty_is_borrow,
        },
    },
    core::semantic::EffectEnvView,
    hir_def::{Body, Expr, FuncParamMode, ItemKind, Partial, TopLevelMod},
    projection::Projection,
};

use super::{
    analyses::{
        BorrowEntryStateAnalysis, BorrowLivenessAnalysis, BorrowLoanTargetAnalysis,
        BorrowLoanTargetState, BorrowMovedStateAnalysis, BorrowSummaryMode,
    },
    canon::{
        BlockAdjacency, BorrowCanonCx, BorrowRoot, CanonPlace, CfgAdjacency, HeldLoan, HeldLoans,
        Loan, LoanId, LoanRegion, MoveSite, MovedPlaces, State, canon_projection_for_layout_path,
        place_set_overlaps, places_overlap,
    },
    diagnostics::operand_origin,
    facts::NormalizedBodyFacts,
    ir::{
        BorrowDiagnosticId, BorrowInput, BorrowResult, BorrowSlotFamilyIds, BorrowSummary,
        BorrowSummaryId, BorrowTransform, NBorrowRoot, NBorrowRootId, NEffectArgValue, NExpr,
        NOperand, NSPlace, NSPlaceRoot, NSStmtKind, NSTerminatorKind, NormalizedBindingLowering,
        NormalizedSemanticBody, ReadMode, SemanticBorrowCheckResult, SemanticBorrowDiagKind,
        SemanticBorrowDiagnostic, SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult,
        borrow_results_in_ty, borrow_results_in_ty_with_family_ids,
        local_has_runtime_move_semantics, return_borrow_results_in_ty,
        return_borrow_results_in_ty_with_family_ids, semantic_projection_ty,
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
            Ok(summary.map(|summary| summary.items(db).clone()))
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
    pub(super) param_loans_for_local: FxHashMap<crate::analysis::semantic::SLocalId, HeldLoans>,
    loans: Vec<Loan<'db>>,
    pub(super) entry_state: SecondaryMap<SBlockId, State>,
    call_result_loans: FxHashMap<SStmtId, Vec<(BorrowResult, LoanId)>>,
    call_loan_transforms: FxHashMap<LoanId, Vec<BorrowTransform>>,
    constant_indices: SecondaryMap<crate::analysis::semantic::SLocalId, Option<usize>>,
    moved_entry: SecondaryMap<SBlockId, MovedPlaces<'db>>,
    live_before: Vec<Vec<FxHashSet<crate::analysis::semantic::SLocalId>>>,
    live_before_term: SecondaryMap<SBlockId, FxHashSet<crate::analysis::semantic::SLocalId>>,
}

struct CallAccess<'db> {
    group: usize,
    projection: Option<Vec<LayoutBackingProjection>>,
    kind: BorrowKind,
    targets: FxHashSet<CanonPlace<'db>>,
    origin: SemOrigin<'db>,
}

struct ActiveLoan<'db> {
    id: LoanId,
    region: LoanRegion<'db>,
    suspended: Vec<LoanRegion<'db>>,
}

impl ActiveLoan<'_> {
    fn overlaps(&self, targets: &FxHashSet<CanonPlace<'_>>) -> bool {
        targets.iter().any(|target| {
            self.region.overlaps(target)
                && !self.suspended.iter().any(|region| region.covers(target))
        })
    }
}

fn variant_slots_are_mutually_exclusive(
    lhs: Option<&[LayoutBackingProjection]>,
    rhs: Option<&[LayoutBackingProjection]>,
) -> bool {
    let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
        return false;
    };
    let Some((lhs, rhs)) = lhs.iter().zip(rhs).find(|(lhs, rhs)| lhs != rhs) else {
        return false;
    };
    matches!(
        (lhs, rhs),
        (
            LayoutBackingProjection::VariantField { variant: lhs, .. },
            LayoutBackingProjection::VariantField { variant: rhs, .. }
        ) if lhs != rhs
    )
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
            param_loans_for_local: FxHashMap::default(),
            loans: Vec::new(),
            entry_state: SecondaryMap::new(),
            call_result_loans: FxHashMap::default(),
            call_loan_transforms: FxHashMap::default(),
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
            &self.loan_for_local,
            &self.constant_indices,
        )
    }

    fn borrow_summary(mut self) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
        let key = self.instance.key(self.db);
        if return_borrow_results_in_ty(self.db, key.instantiate_typed_body(self.db).result_ty())
            .is_empty()
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
        let key = self.instance.key(self.db);
        if !return_borrow_results_in_ty(self.db, key.instantiate_typed_body(self.db).result_ty())
            .is_empty()
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
            if ty_is_borrow(self.db, local_ty).is_some()
                && matches!(
                    local.lowering,
                    NormalizedBindingLowering::CarrierLocal { .. }
                )
            {
                continue;
            }
            let mut held = HeldLoans::default();
            for result in borrow_results_in_ty(self.db, local_ty) {
                let proj = canon_projection_for_layout_path(self.db, local_ty, &result.projection)
                    .expect("borrow result projection must match its parameter type");
                let targets = FxHashSet::from_iter([CanonPlace {
                    root: BorrowRoot::Param(param_idx),
                    proj,
                }]);
                let loan = self.allocate_loan(Loan {
                    kind: result.kind,
                    activation: BorrowActivation::Immediate,
                    unconditional_targets: targets.clone(),
                    targets,
                    indexed_targets: Vec::new(),
                    result_exclusions: Vec::new(),
                    parents: FxHashSet::default(),
                    origin: crate::analysis::semantic::SemOrigin::Body(self.body.template_owner),
                });
                held.entry(result.projection)
                    .or_default()
                    .insert(HeldLoan::new(loan));
            }
            if !held.is_empty() {
                self.param_loans_for_local.insert(local_id, held);
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
            if let Some((kind, _)) = ty_is_borrow(self.db, result_ty)
                && matches!(
                    expr,
                    NExpr::Borrow { .. }
                        | NExpr::Call { .. }
                        | NExpr::ReadPlace { .. }
                        | NExpr::Use(_)
                )
                && !matches!(
                    expr,
                    NExpr::ReadPlace { place, .. }
                        if self.read_place_copies_capability(place)
                )
            {
                let loan = self.allocate_loan(Loan {
                    kind,
                    activation: match expr {
                        NExpr::Borrow { activation, .. } => *activation,
                        NExpr::Call { .. } | NExpr::ReadPlace { .. } | NExpr::Use(_) => {
                            BorrowActivation::Immediate
                        }
                        _ => unreachable!(),
                    },
                    targets: FxHashSet::default(),
                    unconditional_targets: FxHashSet::default(),
                    indexed_targets: Vec::new(),
                    result_exclusions: Vec::new(),
                    parents: FxHashSet::default(),
                    origin: stmt.origin,
                });
                self.loan_for_local.insert(*dst, loan);
                if let NExpr::Call { callee, args, .. } = expr
                    && let Some(summary) = self.call_borrow_summary(callee.key)?
                {
                    self.validate_call_borrow_summary(result_ty, args, &summary, stmt.origin)?;
                    self.call_loan_transforms.insert(loan, summary);
                }
                continue;
            }

            let NExpr::Call { callee, args, .. } = expr else {
                continue;
            };
            if return_borrow_results_in_ty(self.db, result_ty).is_empty() {
                continue;
            }
            let Some(summary) = self.call_borrow_summary(callee.key)? else {
                continue;
            };
            self.validate_call_borrow_summary(result_ty, args, &summary, stmt.origin)?;
            self.call_result_loans.entry(stmt.id).or_default();
            let mut groups = Vec::<(BorrowResult, Vec<BorrowTransform>)>::new();
            for transform in summary {
                if let Some((_, transforms)) = groups
                    .iter_mut()
                    .find(|(result, _)| *result == transform.result)
                {
                    transforms.push(transform);
                } else {
                    groups.push((transform.result.clone(), vec![transform]));
                }
            }
            let result_slots = groups
                .iter()
                .map(|(result, _)| result.clone())
                .collect::<Vec<_>>();
            for (result, transforms) in groups {
                let result_exclusions = result_slots
                    .iter()
                    .filter_map(|candidate| borrow_result_refinement_bindings(&result, candidate))
                    .collect();
                let loan = self.allocate_loan(Loan {
                    kind: result.kind,
                    activation: BorrowActivation::Immediate,
                    targets: FxHashSet::default(),
                    unconditional_targets: FxHashSet::default(),
                    indexed_targets: Vec::new(),
                    result_exclusions,
                    parents: FxHashSet::default(),
                    origin: stmt.origin,
                });
                self.call_result_loans
                    .get_mut(&stmt.id)
                    .expect("call loan entry")
                    .push((result, loan));
                self.call_loan_transforms.insert(loan, transforms);
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

    fn allocate_loan(&mut self, loan: Loan<'db>) -> LoanId {
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
        summary: &[BorrowTransform],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let results = return_borrow_results_in_ty(self.db, result_ty);
        if let Some(transform) = summary.iter().find(|transform| {
            !results
                .iter()
                .any(|result| borrow_result_slot_matches(result, &transform.result))
        }) {
            return Err(self.internal_diag(
                origin,
                format!(
                    "callee borrow summary contains invalid result slot {:?}",
                    transform.result
                ),
            ));
        }
        if let Some(transform) = summary
            .iter()
            .find(|transform| transform.input.param() as usize >= args.len())
        {
            return Err(self.internal_diag(
                origin,
                format!(
                    "callee borrow summary references missing input {}",
                    transform.input.param()
                ),
            ));
        }
        if self.summary_mode == BorrowSummaryMode::FinalCheck
            && let Some(result) = results.iter().find(|result| {
                !result.projection.iter().any(|projection| {
                    matches!(projection, LayoutBackingProjection::VariantField { .. })
                }) && !summary
                    .iter()
                    .any(|transform| borrow_result_slot_matches(result, &transform.result))
            })
        {
            return Err(self.internal_diag(
                origin,
                format!(
                    "callee borrow summary is missing result slot {:?}",
                    result.projection
                ),
            ));
        }
        Ok(())
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
            &self.call_loan_transforms,
        );
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)
    }

    pub(super) fn apply_stmt_state(&self, state: &mut State, stmt: &super::ir::NSStmt<'db>) {
        self.canon().apply_stmt_state_with_call_loans(
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
        state: &State,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = self.effective_loans(state, live);
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                match expr {
                    NExpr::ReadPlace { place, mode } => {
                        self.check_place_read(state, moved, &active, place, *mode, stmt.origin)?;
                    }
                    NExpr::Borrow { place, kind, .. } => {
                        let targets = self.canon().canonicalize_place(state, place, stmt.origin)?;
                        let authorized = self.canon().loans_for_place(state, place);
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
                            self.check_loan_conflict(&active, loan, kind, &targets, stmt.origin)?;
                        }
                    }
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => {
                        let targets =
                            self.extract_enum_field_move_targets(state, *value, *variant, *field);
                        let authorized =
                            self.canon()
                                .loans_for_value_targets(state, value.local, &targets);
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
                let targets = self.canon().canonicalize_place(state, dst, stmt.origin)?;
                let authorized = self
                    .canon()
                    .mut_loans_for_place_targets(state, dst, &targets);
                self.check_moved_parent(moved, &targets, &authorized, stmt.origin)?;
                self.check_write_targets(&active, &authorized, &targets, stmt.origin)?;
            }
        }
        Ok(())
    }

    fn check_assignment_write(
        &self,
        state: &State,
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
        let targets = self.canon().canonicalize_place(state, place, origin)?;
        self.check_write_targets(active, &FxHashSet::default(), &targets, origin)
    }

    fn check_place_read(
        &self,
        state: &State,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        place: &NSPlace<'db>,
        mode: ReadMode,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let targets = self.canon().canonicalize_place(state, place, origin)?;
        let authorized = self.canon().loans_for_place(state, place);
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
        state: &State,
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
                    let targets = self.canon().canonicalize_place(state, place, origin)?;
                    let authorized = if effect_arg.required_mut {
                        self.canon().mut_loans_for_place(state, place)
                    } else {
                        self.canon().loans_for_place(state, place)
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
                    let targets = self.canon().canonicalize_value_base(state, value.local);
                    let authorized = if effect_arg.required_mut {
                        self.canon()
                            .mut_loans_for_value_targets(state, value.local, &targets)
                    } else {
                        self.canon()
                            .loans_for_value_targets(state, value.local, &targets)
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
            for result in borrow_results_in_ty(self.db, target_ty) {
                let targets = match &effect_arg.arg {
                    NEffectArgValue::Place(place) => {
                        self.canon().canonicalize_place_layout_projection(
                            state,
                            place,
                            target_ty,
                            &result.projection,
                        )
                    }
                    NEffectArgValue::Value(value) => {
                        self.canon().canonicalize_value_layout_projection(
                            state,
                            value.local,
                            &result.projection,
                        )
                    }
                };
                let authorized = match &effect_arg.arg {
                    NEffectArgValue::Place(place) if result.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_loans_for_place_targets(state, place, &targets),
                    NEffectArgValue::Place(place) => {
                        self.canon().loans_for_place_targets(state, place, &targets)
                    }
                    NEffectArgValue::Value(value) if result.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_loans_for_value_targets(state, value.local, &targets),
                    NEffectArgValue::Value(value) => {
                        self.canon()
                            .loans_for_value_targets(state, value.local, &targets)
                    }
                };
                if result.kind == BorrowKind::Mut {
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                } else {
                    self.check_read_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(
                    accesses,
                    group,
                    Some(&result.projection),
                    result.kind,
                    targets,
                    arg_origin,
                )?;
            }
        }
        Ok(())
    }

    fn check_call_argument_accesses(
        &self,
        state: &State,
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
                let targets = self.canon().canonicalize_value_base(state, arg.local);
                if kind == BorrowKind::Mut && !moves_value {
                    let authorized = self
                        .canon()
                        .mut_loans_for_value_targets(state, arg.local, &targets);
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(&mut accesses, idx, None, kind, targets, arg_origin)?;
            }
            for result in borrow_results_in_ty(self.db, ty) {
                let targets = self.canon().canonicalize_value_layout_projection(
                    state,
                    arg.local,
                    &result.projection,
                );
                let authorized = if result.kind == BorrowKind::Mut {
                    self.canon()
                        .mut_loans_for_value_targets(state, arg.local, &targets)
                } else {
                    self.canon()
                        .loans_for_value_targets(state, arg.local, &targets)
                };
                if result.kind == BorrowKind::Mut {
                    self.check_write_targets(active, &authorized, &targets, arg_origin)?;
                } else {
                    self.check_read_targets(active, &authorized, &targets, arg_origin)?;
                }
                self.record_call_access(
                    &mut accesses,
                    idx,
                    Some(&result.projection),
                    result.kind,
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
        projection: Option<&[LayoutBackingProjection]>,
        kind: BorrowKind,
        targets: FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        // An argument owns its container and the capability slots stored inside
        // it, but distinct capability slots must still obey aliasing rules.
        let conflict = accesses.iter().find(|access| {
            (access.group != group
                || access.projection.is_some()
                    && projection.is_some()
                    && !variant_slots_are_mutually_exclusive(
                        access.projection.as_deref(),
                        projection,
                    ))
                && !matches!((access.kind, kind), (BorrowKind::Ref, BorrowKind::Ref))
                && place_set_overlaps(&access.targets, &targets)
        });
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
                conflict.origin,
                "overlapping argument access occurs here".to_string(),
            );
            return Err(diag);
        }
        if !targets.is_empty() {
            accesses.push(CallAccess {
                group,
                projection: projection.map(<[_]>::to_vec),
                kind,
                targets,
                origin,
            });
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
        let direct = self.loan_for_local.get(&local).copied().into_iter();
        let aggregate = self
            .call_result_loans
            .get(&stmt)
            .into_iter()
            .flatten()
            .map(|(_, loan)| *loan);
        for loan_id in direct.chain(aggregate) {
            let loan = &self.loans[loan_id.0 as usize];
            self.check_loan_conflict(active, Some(loan_id), loan.kind, &loan.targets, origin)?;
        }
        Ok(())
    }

    fn check_loan_conflict(
        &self,
        active: &[ActiveLoan<'db>],
        new_loan: Option<LoanId>,
        kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
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
        authorized: &FxHashSet<LoanId>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().find(|loan| {
            !authorized.contains(&loan.id)
                && self.loan_conflict_kind(loan.id) == Some(BorrowKind::Mut)
                && loan.overlaps(targets)
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot read this place while a mutable borrow is active".to_string(),
                conflict.id,
            ));
        }
        Ok(())
    }

    fn check_write_targets(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &FxHashSet<LoanId>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().find(|loan| {
            !authorized.contains(&loan.id)
                && self.loan_conflict_kind(loan.id).is_some()
                && loan.overlaps(targets)
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot write to this place while it is borrowed".to_string(),
                conflict.id,
            ));
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
            NSTerminatorKind::Branch { cond, .. } | NSTerminatorKind::Return(Some(cond)) => {
                let active = self.effective_loans(state, live);
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
                let active = self.effective_loans(state, live);
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
                .borrow_local_targets(state, value.local)
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
            for loan in self.active_loans_in(state, value.local) {
                let targets =
                    self.resolve_return_targets(state, &loan.region.active_targets(), term.origin)?;
                if let Some(local) = targets.iter().find_map(|target| match target.root {
                    BorrowRoot::Local(local) => Some(local),
                    BorrowRoot::Param(_) | BorrowRoot::Provider(_) => None,
                }) {
                    let name = self.pretty_local_name(local);
                    let mut diag = self.invalid_return_diag(
                        term.origin,
                        format!("cannot return a value that holds a borrow of local `{name}`"),
                    );
                    self.push_secondary_origin(
                        &mut diag,
                        self.loan_origin(loan.id),
                        "borrow created here".to_string(),
                    );
                    return Err(diag);
                }
            }
        }
        Ok(())
    }

    fn resolve_return_targets(
        &self,
        state: &State,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut pending = targets.iter().cloned().collect::<VecDeque<_>>();
        let mut seen = FxHashSet::default();
        let mut resolved = FxHashSet::default();
        while let Some(target) = pending.pop_front() {
            if !seen.insert(target.clone()) {
                continue;
            }
            let BorrowRoot::Local(local) = &target.root else {
                resolved.insert(target);
                continue;
            };
            let Some(local) = self.body.local(*local) else {
                resolved.insert(target);
                continue;
            };
            // Snapshot provenance describes a value's physical source. Layout
            // backing is deliberately not used here: it can point at the
            // argument that supplied a fresh aggregate field's layout without
            // making that freshly allocated field an alias of the argument.
            let Some(mut source) = local.snapshot_source_place().cloned() else {
                resolved.insert(target);
                continue;
            };
            let Some(target_projection) =
                super::canon::semantic_projection_from_canon(&target.proj)
            else {
                resolved.insert(target);
                continue;
            };
            source.path = source.path.concat(&target_projection);
            let mut advanced = false;
            for source in self.canon().canonicalize_place(state, &source, origin)? {
                if source == target {
                    continue;
                }
                advanced = true;
                pending.push_back(source);
            }
            if !advanced {
                resolved.insert(target);
            }
        }
        Ok(resolved)
    }

    fn compute_return_summary(&self) -> Result<BorrowSummary, SemanticBorrowDiagnostic<'db>> {
        let mut out = Vec::new();
        let mut materialized_results = Vec::new();
        let mut returns = Vec::new();
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let NSTerminatorKind::Return(Some(value)) = block.terminator.kind else {
                continue;
            };
            let mut state = self.entry_state[SBlockId::new(bb_idx)].clone();
            for stmt in &block.stmts {
                self.apply_stmt_state(&mut state, stmt);
            }
            let Some(return_local) = self.body.local(value.local) else {
                continue;
            };
            let materialized = return_borrow_results_in_ty(self.db, return_local.ty)
                .into_iter()
                .flat_map(|result| {
                    state.materialize_borrow_result(
                        value.local,
                        &result,
                        &self.loans,
                        return_local.layout_backing_sources(),
                        matches!(
                            return_local.source,
                            Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
                        ) && return_local.ty.as_borrow(self.db).is_none(),
                    )
                })
                .collect::<Vec<_>>();
            materialized_results.extend(materialized);
            returns.push((state, value.local, block.terminator.origin));
        }
        materialized_results.sort_unstable();
        materialized_results.dedup();

        for (state, return_local, origin) in returns {
            for result in &materialized_results {
                let mut targets = self.canon().canonicalize_value_layout_projection(
                    &state,
                    return_local,
                    &result.projection,
                );
                let had_targets = !targets.is_empty();
                if result
                    .projection
                    .iter()
                    .any(|step| matches!(step, LayoutBackingProjection::IndexFamily(_)))
                {
                    let shadowing_targets = materialized_results
                        .iter()
                        .filter(|candidate| borrow_result_strictly_refines(result, candidate))
                        .flat_map(|candidate| {
                            self.canon().canonicalize_value_layout_projection(
                                &state,
                                return_local,
                                &candidate.projection,
                            )
                        })
                        .collect::<FxHashSet<_>>();
                    targets.retain(|target| !shadowing_targets.contains(target));
                    if had_targets && targets.is_empty() {
                        continue;
                    }
                }
                if targets.is_empty() {
                    if self.summary_mode != BorrowSummaryMode::FinalCheck
                        || result.projection.iter().any(|projection| {
                            matches!(projection, LayoutBackingProjection::VariantField { .. })
                        })
                    {
                        continue;
                    }
                    return Err(self.internal_diag(
                        origin,
                        format!(
                            "borrow result slot {:?} has no tracked source",
                            result.projection
                        ),
                    ));
                }
                let targets = self.resolve_return_targets(&state, &targets, origin)?;
                for target in targets {
                    match &target.root {
                        BorrowRoot::Param(idx) => {
                            let Some(input_projection) =
                                super::canon::layout_path_for_canon_projection(&target.proj)
                            else {
                                return Err(self.internal_diag(
                                    origin,
                                    "cannot summarize borrow source projection".to_string(),
                                ));
                            };
                            out.push(BorrowTransform {
                                result: result.clone(),
                                input: BorrowInput::Place {
                                    param: *idx,
                                    projection: input_projection,
                                },
                            });
                        }
                        BorrowRoot::Provider(_) => {
                            return Err(self.invalid_return_diag(
                                origin,
                                "cannot return a borrow derived from an effect parameter"
                                    .to_string(),
                            ));
                        }
                        BorrowRoot::Local(local) => {
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
        out.sort_unstable();
        out.dedup();
        Ok(out)
    }

    fn effective_loans(
        &self,
        state: &State,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
    ) -> Vec<ActiveLoan<'db>> {
        let mut active = state
            .local_loans
            .iter()
            .filter(|(local, _)| live.contains(local))
            .flat_map(|(local, _)| self.active_loans_in(state, *local))
            .collect::<Vec<_>>();
        let mut suspended = FxHashMap::<LoanId, Vec<LoanRegion<'db>>>::default();
        let mut worklist = active
            .iter()
            .map(|loan| (loan.id, loan.region.clone()))
            .collect::<Vec<_>>();
        while let Some((loan, region)) = worklist.pop() {
            for parent in &self.loans[loan.0 as usize].parents {
                let parent_regions = suspended.entry(*parent).or_default();
                if !parent_regions.contains(&region) {
                    parent_regions.push(region.clone());
                    worklist.push((*parent, region.clone()));
                }
            }
        }
        for loan in &mut active {
            loan.suspended = suspended.get(&loan.id).cloned().unwrap_or_default();
        }
        active.sort_by_key(|loan| loan.id.0);
        active
    }

    fn active_loans_in(&self, state: &State, local: SLocalId) -> Vec<ActiveLoan<'db>> {
        let canon = self.canon();
        state
            .local_loans
            .get(&local)
            .into_iter()
            .flatten()
            .flat_map(|(path, loans)| {
                loans.iter().map(|held| ActiveLoan {
                    id: held.id,
                    region: canon.active_targets_for_held(state, local, path, held),
                    suspended: Vec::new(),
                })
            })
            .collect()
    }

    fn first_loan_conflict(
        &self,
        active: &[ActiveLoan<'db>],
        new_loan: Option<LoanId>,
        new_kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<LoanId> {
        let reborrow_parents = new_loan.map(|loan| &self.loans[loan.0 as usize].parents);
        active
            .iter()
            .filter(|loan| reborrow_parents.is_none_or(|parents| !parents.contains(&loan.id)))
            .find(|loan| {
                self.loan_conflict_kind(loan.id).is_some_and(|kind| {
                    !matches!((kind, new_kind), (BorrowKind::Ref, BorrowKind::Ref))
                }) && loan.overlaps(targets)
            })
            .map(|loan| loan.id)
    }

    fn loan_conflict_kind(&self, loan: LoanId) -> Option<BorrowKind> {
        let loan = &self.loans[loan.0 as usize];
        // Receiver reservations remain dormant while later arguments are
        // evaluated. The call-access checks perform their activation.
        if loan.activation == BorrowActivation::AtCall {
            None
        } else {
            Some(loan.kind)
        }
    }

    fn check_move_out(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &FxHashSet<LoanId>,
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
        self.check_move_targets_out(active, authorized, targets, origin)?;
        Ok(())
    }

    fn check_move_targets_out(
        &self,
        active: &[ActiveLoan<'db>],
        authorized: &FxHashSet<LoanId>,
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
            .find(|loan| !authorized.contains(&loan.id) && loan.overlaps(targets))
        {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot move out of a value while it is borrowed".to_string(),
                loan.id,
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
            NSStmtKind::Store { dst, src } => {
                self.record_operand_move(state, moved, *src, stmt.origin)?;
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
        state: &State,
        moved: &MovedPlaces<'db>,
        active: &[ActiveLoan<'db>],
        operand: NOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        let targets = self.canon().canonicalize_value_base(state, operand.local);
        if targets.is_empty() {
            return Ok(());
        }
        let authorized = self
            .canon()
            .loans_for_value_targets(state, operand.local, &targets);
        self.check_moved_overlap(moved, &targets, &authorized, origin, message)?;
        if operand.mode == ReadMode::Move && self.local_has_runtime_move_semantics(operand.local) {
            self.check_move_targets_out(active, &authorized, &targets, origin)
        } else {
            self.check_read_targets(active, &authorized, &targets, origin)
        }
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
        authorized: &FxHashSet<LoanId>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            accessed.iter().any(|accessed| {
                places_overlap(moved, accessed)
                    && !self.loan_authorizes_access(authorized, accessed)
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
        written: &FxHashSet<CanonPlace<'db>>,
        authorized: &FxHashSet<LoanId>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some((_, site)) = moved.iter().find(|(moved, _)| {
            written.iter().any(|written| {
                written.root == moved.root
                    && moved.proj.is_prefix_of(&written.proj)
                    && moved.proj != written.proj
                    && !self.loan_authorizes_access(authorized, written)
            })
        }) {
            let mut diag =
                self.move_conflict_diag(origin, "cannot write through a moved value".to_string());
            self.push_secondary_origin(&mut diag, site.origin, site.note.clone());
            return Err(diag);
        }
        Ok(())
    }

    fn loan_authorizes_access(
        &self,
        authorized: &FxHashSet<LoanId>,
        accessed: &CanonPlace<'db>,
    ) -> bool {
        authorized.iter().any(|loan| {
            self.loans[loan.0 as usize].targets.iter().any(|target| {
                target.root == accessed.root && target.proj.is_prefix_of(&accessed.proj)
            })
        })
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
            let Some(parent) = data.parents.iter().copied().next() else {
                return data.origin;
            };
            if data.parents.len() != 1 {
                return data.origin;
            }
            loan = parent;
        }
        self.loans[loan.0 as usize].origin
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
            .then(|| BorrowSummaryId::new(db, Vec::new())),
    )
}

fn instance_returns_borrowing_value<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    !return_borrow_results_in_ty(db, instance.normalized_result_ty(db)).is_empty()
}

fn borrow_result_slot_matches(template: &BorrowResult, candidate: &BorrowResult) -> bool {
    template.kind == candidate.kind
        && template.projection.len() == candidate.projection.len()
        && template
            .projection
            .iter()
            .copied()
            .zip(candidate.projection.iter().copied())
            .all(|(template, candidate)| {
                template == candidate
                    || matches!(
                        (template, candidate),
                        (
                            LayoutBackingProjection::IndexFamily(_),
                            LayoutBackingProjection::Index(_)
                                | LayoutBackingProjection::IndexFamily(_)
                        )
                    )
            })
}

fn borrow_result_strictly_refines(fallback: &BorrowResult, candidate: &BorrowResult) -> bool {
    fallback.kind == candidate.kind
        && fallback.projection.len() == candidate.projection.len()
        && fallback.projection != candidate.projection
        && fallback
            .projection
            .iter()
            .copied()
            .zip(candidate.projection.iter().copied())
            .all(|(fallback, candidate)| {
                fallback == candidate
                    || matches!(
                        (fallback, candidate),
                        (
                            LayoutBackingProjection::IndexFamily(_),
                            LayoutBackingProjection::Index(Some(_))
                        )
                    )
            })
}

fn borrow_result_refinement_bindings(
    fallback: &BorrowResult,
    candidate: &BorrowResult,
) -> Option<super::canon::FamilyBindings> {
    borrow_result_strictly_refines(fallback, candidate).then(|| {
        fallback
            .projection
            .iter()
            .copied()
            .zip(candidate.projection.iter().copied())
            .filter_map(|(fallback, candidate)| match (fallback, candidate) {
                (
                    LayoutBackingProjection::IndexFamily(family),
                    candidate @ LayoutBackingProjection::Index(Some(_)),
                ) => Some((family, candidate)),
                _ => None,
            })
            .collect()
    })
}

fn conservative_signature_borrow_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> BorrowSummary {
    let mut family_ids = BorrowSlotFamilyIds::default();
    let results = return_borrow_results_in_ty_with_family_ids(
        db,
        instance.normalized_result_ty(db),
        &mut family_ids,
    );
    let inputs = match instance.key(db).owner(db) {
        BodyOwner::Func(func) => func
            .params(db)
            .filter_map(|param| {
                u32::try_from(param.index()).ok().map(|idx| {
                    let ty = instance.normalized_ty(db, param.ty(db));
                    let borrow_results =
                        borrow_results_in_ty_with_family_ids(db, ty, &mut family_ids);
                    (idx, ty, param.is_mut(db), borrow_results)
                })
            })
            .collect::<Vec<_>>(),
        _ => Vec::new(),
    };
    let mut summary = Vec::new();
    for result in results {
        for (idx, ty, is_mut, input_results) in &inputs {
            if signature_input_is_unresolved(db, *ty) {
                summary.push(BorrowTransform {
                    result: result.clone(),
                    input: BorrowInput::AnyInParam(*idx),
                });
                continue;
            }
            if result.kind == BorrowKind::Ref || *is_mut {
                summary.push(BorrowTransform {
                    result: result.clone(),
                    input: BorrowInput::Place {
                        param: *idx,
                        projection: Vec::new(),
                    },
                });
            }
            for input in input_results {
                if result.kind == BorrowKind::Ref || input.kind == BorrowKind::Mut {
                    summary.push(BorrowTransform {
                        result: result.clone(),
                        input: BorrowInput::Place {
                            param: *idx,
                            projection: input.projection.clone(),
                        },
                    });
                }
            }
        }
    }
    summary.sort_unstable();
    summary.dedup();
    summary
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
