use common::diagnostics::CompleteDiagnostic;
use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{solve_backward_cfg, try_solve_forward_cfg, try_solve_sparse};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SBlockId, SLocalId, SemOrigin, SemanticInstance, effect_param_site,
            get_or_build_semantic_instance, identity_semantic_instance_key,
        },
        ty::{
            corelib::{
                IntrinsicMemoryContract, IntrinsicMemoryProjection, IntrinsicPointerReturn,
                intrinsic_contract, is_std_evm_effect_method,
            },
            provider::ProviderAddressSpace,
            ty_check::{BodyOwner, LocalBinding, ParamSite},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::{Body, Expr, FuncParamMode, ItemKind, Partial, TopLevelMod},
    projection::{IndexSource, Projection},
    semantic::ProviderSource,
};

use super::{
    address_space_rank,
    analyses::{
        BorrowEntryStateAnalysis, BorrowLivenessAnalysis, BorrowLoanEffectCx,
        BorrowLoanTargetAnalysis, BorrowLoanTargetState, BorrowMovedStateAnalysis,
        BorrowSummaryMode,
    },
    canon::{
        BlockAdjacency, BorrowCanonCx, BorrowRoot, BorrowStateTransferCx, BorrowSummaryResolver,
        CanonPlace, CfgAdjacency, Loan, LoanId, MoveSite, MovedPlaces, State, WritePrecision,
        address_spaces_for_borrow_root, places_overlap as canonical_places_overlap,
    },
    diagnostics::operand_origin,
    facts::NormalizedBodyFacts,
    ir::{
        BorrowDiagnosticId, BorrowInputRef, BorrowSummary, BorrowSummaryId, BorrowTransform,
        FreshAllocSite, MemoryAccessKind, MemorySummary, MemorySummaryId, MemorySummaryItem,
        MemorySummaryTarget, NBorrowRoot, NBorrowRootId, NExpr, NOperand, NSPlace, NSPlaceRoot,
        NSProjectionPath, NSStmtKind, NSTerminatorKind, NormalizedBindingLowering,
        NormalizedSemanticBody, PointerAddressSpaces, ReadMode, SemanticBorrowCheckResult,
        SemanticBorrowDiagKind, SemanticBorrowDiagnostic, SemanticBorrowDiagnosticSpan,
        SemanticBorrowSummaryResult, SemanticMemorySummaryResult, local_has_runtime_move_semantics,
    },
    normalize::{normalize_provisional_semantic_body, normalize_semantic_body},
    pointer::{is_pointer_bearing_type, pointer_reachability, pointer_slots},
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
    if instance_has_smir_lowering_blocking_diagnostics(db, instance) {
        return unlowerable_borrow_summary_result(db, instance);
    }
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
    if instance_has_smir_lowering_blocking_diagnostics(db, instance) {
        return unlowerable_borrow_summary_result(db, instance);
    }
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

#[salsa::tracked(
    cycle_fn=semantic_memory_summary_cycle_recover,
    cycle_initial=semantic_memory_summary_cycle_initial
)]
fn semantic_memory_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticMemorySummaryResult<'db> {
    if instance_has_smir_lowering_blocking_diagnostics(db, instance) {
        return unlowerable_memory_summary_result(db, instance);
    }
    match Borrowck::new(db, instance).and_then(Borrowck::memory_summary) {
        Ok(summary) => SemanticMemorySummaryResult::Ok(summary),
        Err(diag) => SemanticMemorySummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

#[salsa::tracked(
    cycle_fn=semantic_memory_summary_cycle_recover,
    cycle_initial=semantic_memory_summary_cycle_initial
)]
fn provisional_memory_summary_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticMemorySummaryResult<'db> {
    if instance_has_smir_lowering_blocking_diagnostics(db, instance) {
        return unlowerable_memory_summary_result(db, instance);
    }
    let body = match normalize_provisional_semantic_body(db, instance) {
        Ok(body) => body,
        Err(diag) => {
            return SemanticMemorySummaryResult::Err(BorrowDiagnosticId::new(db, diag));
        }
    };
    match Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)
        .and_then(Borrowck::memory_summary)
    {
        Ok(summary) => SemanticMemorySummaryResult::Ok(summary),
        Err(diag) => SemanticMemorySummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

pub fn semantic_borrow_summary<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<BorrowSummary<'db>>, CompleteDiagnostic> {
    semantic_borrow_summary_voucher(db, instance).map_err(|diag| diag.to_complete(db))
}

pub fn semantic_memory_summary<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<MemorySummary<'db>, CompleteDiagnostic> {
    let summary =
        semantic_memory_summary_voucher(db, instance).map_err(|diag| diag.to_complete(db))?;
    Ok(MemorySummary {
        items: summary.items(db).clone(),
        may_return: summary.may_return(db),
    })
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

pub(super) fn semantic_memory_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
    memory_summary_voucher(db, semantic_memory_summary_query(db, instance))
}

pub(super) fn provisional_memory_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
    memory_summary_voucher(db, provisional_memory_summary_query(db, instance))
}

fn memory_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    result: SemanticMemorySummaryResult<'db>,
) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
    match result {
        SemanticMemorySummaryResult::Ok(summary) => Ok(summary),
        SemanticMemorySummaryResult::Err(diag) => Err(diag.diag(db).clone()),
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
    if instance_has_smir_lowering_blocking_diagnostics(db, instance) {
        return SemanticBorrowCheckResult::Ok;
    }
    match Borrowck::new(db, instance).and_then(Borrowck::check) {
        Ok(()) => SemanticBorrowCheckResult::Ok,
        Err(diag) => SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

pub(super) fn instance_has_smir_lowering_blocking_diagnostics<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    instance
        .key(db)
        .typed_body(db)
        .has_smir_lowering_blocking_diagnostics(db)
}

fn unlowerable_memory_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticMemorySummaryResult<'db> {
    let mut items = if instance_returns_pointer_bearing(db, instance) {
        pointer_slots(db, instance.normalized_result_ty(db))
            .into_iter()
            .map(|slot| MemorySummaryItem::ReturnPointer {
                output: slot.path,
                targets: vec![MemorySummaryTarget::Unknown {
                    address_spaces: PointerAddressSpaces::one(ProviderAddressSpace::Memory),
                }],
            })
            .collect()
    } else {
        Vec::new()
    };
    normalize_memory_summary(db, &mut items);
    SemanticMemorySummaryResult::Ok(MemorySummaryId::new(db, items, true))
}

fn unlowerable_borrow_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    SemanticBorrowSummaryResult::Ok(
        instance_returns_borrow(db, instance).then(|| BorrowSummaryId::new(db, Vec::new())),
    )
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

#[derive(Clone, Copy)]
struct ActiveLoan {
    id: LoanId,
    kind: BorrowKind,
    reservation: bool,
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
    two_phase_activations: FxHashMap<LoanId, (SBlockId, usize)>,
    pub(super) entry_state: SecondaryMap<SBlockId, State<'db>>,
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
            two_phase_activations: FxHashMap::default(),
            entry_state: SecondaryMap::new(),
            moved_entry: SecondaryMap::new(),
            live_before: Vec::new(),
            live_before_term: SecondaryMap::new(),
        };
        checker.init_loans();
        checker.init_two_phase_activations();
        Ok(checker)
    }

    pub(super) fn canon(&self) -> BorrowCanonCx<'_, 'db> {
        BorrowCanonCx::new(self.db, self.instance, &self.body, &self.facts, &self.loans)
    }

    pub(super) fn state_transfer(&self) -> BorrowStateTransferCx<'_, 'db> {
        BorrowStateTransferCx::new(self.canon(), &self.loan_for_local, self.summary_mode)
    }

    fn summaries(&self) -> BorrowSummaryResolver<'_, 'db> {
        BorrowSummaryResolver::new(self.canon(), self.summary_mode)
    }

    fn borrow_summary(
        mut self,
    ) -> Result<Option<BorrowSummary<'db>>, SemanticBorrowDiagnostic<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        let typed_body = self.instance.key(self.db).instantiate_typed_body(self.db);
        if typed_body.result_ty().as_borrow(self.db).is_none() || owner.body(self.db).is_none() {
            return Ok(None);
        }
        self.compute_entry_states_and_loan_targets()?;
        self.compute_return_summary().map(Some)
    }

    fn memory_summary(mut self) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        let result_ty = self.instance.normalized_result_ty(self.db);
        let evm_receiver = match owner {
            BodyOwner::Func(func) if is_std_evm_effect_method(self.db, func) => {
                Some(BorrowInputRef::Param(0))
            }
            _ => None,
        };
        let intrinsic = match owner {
            BodyOwner::Func(func) => intrinsic_contract(self.db, func),
            _ => None,
        };
        let pointer_returns = intrinsic
            .and_then(|contract| contract.pointer_return)
            .map(|contract| self.intrinsic_pointer_return_items(result_ty, contract));
        let memory_contract = intrinsic
            .and_then(|contract| contract.memory)
            .map(|contract| self.intrinsic_memory_items(contract));
        let has_body = owner.body(self.db).is_some();
        let mut may_return = !self.instance.is_intrinsically_never_returning(self.db);
        let mut items = if has_body && memory_contract.is_none() {
            self.compute_entry_states_and_loan_targets()?;
            let (items, body_may_return) =
                self.compute_body_memory_summary(result_ty, pointer_returns.is_none())?;
            may_return = body_may_return;
            items
        } else {
            memory_contract.unwrap_or_else(|| self.bodyless_memory_items(evm_receiver))
        };
        items.extend(pointer_returns.unwrap_or_else(|| {
            if !has_body && is_pointer_bearing_type(self.db, result_ty) {
                self.unknown_pointer_return_items(result_ty)
            } else {
                Vec::new()
            }
        }));
        if let Some(input) = evm_receiver {
            let authorizer = MemorySummaryTarget::Input {
                input,
                proj: NSProjectionPath::default(),
            };
            for item in &mut items {
                if let MemorySummaryItem::Access { authorizers, .. } = item {
                    authorizers.push(authorizer.clone());
                }
            }
        }
        normalize_memory_summary(self.db, &mut items);
        Ok(MemorySummaryId::new(self.db, items, may_return))
    }

    fn check(mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.compute_entry_states_and_loan_targets()?;
        self.compute_moved_states()?;
        self.compute_liveness();
        self.check_conflicts()?;
        let result_ty = self.instance.normalized_result_ty(self.db);
        if result_ty.as_borrow(self.db).is_some() {
            let _ = self.compute_return_summary()?;
        }
        if is_pointer_bearing_type(self.db, result_ty) {
            let _ = self.compute_body_memory_summary(result_ty, true)?;
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

    fn init_two_phase_activations(&mut self) {
        for (block_idx, block) in self.body.blocks.iter().enumerate() {
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                let NSStmtKind::Assign {
                    expr: NExpr::Call { callee, args, .. },
                    ..
                } = &stmt.kind
                else {
                    continue;
                };
                let BodyOwner::Func(func) = callee.key.owner(self.db) else {
                    continue;
                };
                let Some(receiver) = func.receiver_ty(self.db).and(args.first()) else {
                    continue;
                };
                let Some(&loan) = self.loan_for_local.get(&receiver.local) else {
                    continue;
                };
                if self.loans[loan.0 as usize].kind == BorrowKind::Mut
                    && self.loans[loan.0 as usize].origin == stmt.origin
                {
                    self.two_phase_activations
                        .insert(loan, (SBlockId::new(block_idx), stmt_idx));
                }
            }
        }
    }

    pub(super) fn compute_entry_states(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.entry_state = try_solve_forward_cfg(&mut BorrowEntryStateAnalysis::new(self))?;
        Ok(())
    }

    pub(super) fn compute_loan_targets(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut analysis = BorrowLoanTargetAnalysis::new(
            self.db,
            self.instance,
            &self.body,
            &self.facts,
            &self.entry_state,
            &self.loan_for_local,
            self.summary_mode,
        );
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)
    }

    pub(super) fn compute_entry_states_and_loan_targets(
        &mut self,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        loop {
            let before = self.loan_target_snapshot();
            self.compute_entry_states()?;
            self.reset_derived_loan_targets();
            self.compute_loan_targets()?;
            if self.loan_target_snapshot() == before {
                return Ok(());
            }
        }
    }

    fn reset_derived_loan_targets(&mut self) {
        for loan in self.loan_for_local.values().copied() {
            let loan = &mut self.loans[loan.0 as usize];
            loan.targets.clear();
            loan.parents.clear();
        }
    }

    fn loan_target_snapshot(&self) -> Vec<(FxHashSet<CanonPlace<'db>>, FxHashSet<LoanId>)> {
        self.loans
            .iter()
            .map(|loan| (loan.targets.clone(), loan.parents.clone()))
            .collect()
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
            if !state.is_reachable() {
                continue;
            }
            let mut moved = self.moved_entry[bb].clone();
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                self.check_stmt(
                    &state,
                    &moved,
                    &self.live_before[bb_idx][stmt_idx],
                    bb,
                    stmt_idx,
                    stmt,
                )?;
                self.update_moved_for_stmt(&state, &mut moved, stmt)?;
                self.state_transfer().apply_stmt(&mut state, stmt)?;
                if !state.is_reachable() {
                    break;
                }
            }
            if !state.is_reachable() {
                continue;
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
        state: &State<'db>,
        moved: &MovedPlaces<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        block: SBlockId,
        stmt_idx: usize,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = self.effective_loans(state, live, (block, stmt_idx));
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                self.check_assign_borrow_conflict(&active, state, *dst, stmt.origin)?;
                match expr {
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
                        } else {
                            self.check_place_access_borrow_conflict(
                                &active,
                                state,
                                place,
                                BorrowKind::Ref,
                                &targets,
                                stmt.origin,
                            )?;
                        }
                    }
                    NExpr::Borrow { place, .. } => {
                        let targets = self.canon().canonicalize_place(state, place, stmt.origin)?;
                        self.check_moved_overlap(
                            moved,
                            &targets,
                            stmt.origin,
                            "cannot borrow a moved value",
                        )?;
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
                    NExpr::Call { .. } => {
                        self.check_expr_operands(state, moved, stmt.origin, expr)?;
                        self.check_call_memory_accesses(&active, state, moved, expr, stmt.origin)?;
                    }
                    _ => self.check_expr_operands(state, moved, stmt.origin, expr)?,
                }
                self.check_created_borrow_conflict(
                    &active,
                    state,
                    *dst,
                    expr,
                    (block, stmt_idx),
                    stmt.origin,
                )?;
            }
            NSStmtKind::Store { dst, src } => {
                self.check_operand(
                    state,
                    moved,
                    *src,
                    stmt.origin,
                    "cannot use a value after it was moved",
                )?;
                let targets = self.canon().canonicalize_place(state, dst, stmt.origin)?;
                self.check_moved_parent(moved, &targets, stmt.origin)?;
                self.check_store_borrow_conflict(&active, state, dst, &targets, stmt.origin)?;
            }
        }
        Ok(())
    }

    fn check_call_memory_accesses(
        &self,
        active: &[ActiveLoan],
        state: &State<'db>,
        moved: &MovedPlaces<'db>,
        expr: &NExpr<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } = expr
        else {
            unreachable!("call memory accesses require a call expression")
        };
        for access in self.summaries().resolve_call_memory_accesses(
            state,
            callee,
            args,
            effect_args,
            origin,
        )? {
            let active = active
                .iter()
                .copied()
                .filter(|loan| !access.authorized.contains(&loan.id))
                .collect::<Vec<_>>();
            match access.kind {
                MemoryAccessKind::Read | MemoryAccessKind::MutAccess => {
                    self.check_moved_overlap(
                        moved,
                        &access.targets,
                        origin,
                        "cannot access a value after it was moved",
                    )?;
                }
                MemoryAccessKind::Write => {
                    self.check_moved_parent(moved, &access.targets, origin)?
                }
                MemoryAccessKind::Move => {
                    self.check_moved_overlap(
                        moved,
                        &access.targets,
                        origin,
                        "cannot move a value after it was moved",
                    )?;
                    self.check_move_targets_out(&active, &access.targets, origin)?;
                    continue;
                }
            }
            self.check_access_borrow_conflict(
                &active,
                access.kind.borrow_kind(),
                &access.targets,
                origin,
            )?;
        }
        Ok(())
    }

    fn check_place_access_borrow_conflict(
        &self,
        active: &[ActiveLoan],
        state: &State<'db>,
        place: &NSPlace<'db>,
        kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let authorized = self.canon().mut_loans_for_place(state, place);
        let active = active
            .iter()
            .copied()
            .filter(|loan| !authorized.contains(&loan.id))
            .collect::<Vec<_>>();
        self.check_access_borrow_conflict(&active, kind, targets, origin)
    }

    fn check_access_borrow_conflict(
        &self,
        active: &[ActiveLoan],
        kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some(conflict) = self.first_loan_conflict(active, kind, targets) {
            return Err(self.borrow_conflict_diag(
                origin,
                self.overlapping_loans_msg(conflict, kind),
                conflict.id,
            ));
        }
        Ok(())
    }

    fn check_assign_borrow_conflict(
        &self,
        active: &[ActiveLoan],
        state: &State<'db>,
        dst: crate::analysis::semantic::SLocalId,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if self
            .body
            .local(dst)
            .is_none_or(|local| local.ty.as_borrow(self.db).is_some())
        {
            return Ok(());
        }
        let targets = self.canon().canonicalize_value_base(state, dst);
        if !targets.is_empty()
            && let Some(conflict) = self.first_loan_conflict(active, BorrowKind::Mut, &targets)
        {
            return Err(self.borrow_conflict_diag(
                origin,
                self.overlapping_loans_msg(conflict, BorrowKind::Mut),
                conflict.id,
            ));
        }
        Ok(())
    }

    fn check_store_borrow_conflict(
        &self,
        active: &[ActiveLoan],
        state: &State<'db>,
        dst: &NSPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let authorized = self.canon().mut_loans_for_place(state, dst);
        let active = active
            .iter()
            .copied()
            .filter(|loan| !authorized.contains(&loan.id))
            .collect::<Vec<_>>();
        if let Some(conflict) = self.first_loan_conflict(&active, BorrowKind::Mut, targets) {
            return Err(self.borrow_conflict_diag(
                origin,
                self.overlapping_loans_msg(conflict, BorrowKind::Mut),
                conflict.id,
            ));
        }
        Ok(())
    }

    fn check_created_borrow_conflict(
        &self,
        active: &[ActiveLoan],
        state: &State<'db>,
        dst: crate::analysis::semantic::SLocalId,
        expr: &NExpr<'db>,
        point: (SBlockId, usize),
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let kind = match expr {
            NExpr::Borrow { kind, .. } => *kind,
            _ => {
                let Some((kind, _)) = self
                    .body
                    .local(dst)
                    .and_then(|local| local.ty.as_borrow(self.db))
                else {
                    return Ok(());
                };
                kind
            }
        };
        let Some(effect) = BorrowLoanEffectCx::new(
            self.db,
            self.instance,
            &self.body,
            &self.facts,
            &self.loans,
            self.summary_mode,
        )
        .for_expr(state, expr, origin)?
        else {
            return Ok(());
        };
        let active = active
            .iter()
            .copied()
            .filter(|loan| !effect.parents.contains(&loan.id))
            .collect::<Vec<_>>();
        let kind = self
            .loan_for_local
            .get(&dst)
            .map_or(kind, |loan| self.loan_kind_at(*loan, point));
        if let Some(conflict) = self.first_loan_conflict(&active, kind, &effect.targets) {
            return Err(self.borrow_conflict_diag(
                origin,
                self.overlapping_loans_msg(conflict, kind),
                conflict.id,
            ));
        }
        Ok(())
    }

    fn check_terminator(
        &self,
        state: &State<'db>,
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
            if !state.is_reachable() {
                continue;
            }
            for stmt in &block.stmts {
                self.state_transfer().apply_stmt(&mut state, stmt)?;
                if !state.is_reachable() {
                    break;
                }
            }
            if !state.is_reachable() {
                continue;
            }
            let targets = self.canon().borrow_local_targets(&state, value.local);
            if targets.is_empty() {
                return Err(self.internal_diag(
                    block.terminator.origin,
                    format!(
                        "borrow return local has no tracked loan targets while computing summary: local=%{}",
                        value.local.index()
                    ),
                ));
            }
            for target in targets {
                match &target.root {
                    BorrowRoot::Param(idx) => {
                        let transform = BorrowTransform {
                            input: BorrowInputRef::Param(*idx),
                            proj: self.input_summary_projection(&target.proj),
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
                    BorrowRoot::FreshAllocation { .. } => {
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            "cannot return a borrow derived from a fresh allocation".to_string(),
                        ));
                    }
                    BorrowRoot::UnknownMemory(_) => {
                        return Err(self.invalid_return_diag(
                            block.terminator.origin,
                            "cannot return a borrow derived from an unknown pointer".to_string(),
                        ));
                    }
                }
            }
        }
        Ok(out)
    }

    fn compute_body_memory_summary(
        &self,
        result_ty: TyId<'db>,
        include_pointer_returns: bool,
    ) -> Result<(Vec<MemorySummaryItem<'db>>, bool), SemanticBorrowDiagnostic<'db>> {
        let mut items = Vec::new();
        let no_authorizers = FxHashSet::default();
        let no_authorizer_targets = FxHashSet::default();
        let initial = self.initial_pointer_state();
        let mut returns_seen = FxHashMap::default();
        let mut return_count = 0usize;
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let mut state = self.entry_state[SBlockId::new(bb_idx)].clone();
            if !state.is_reachable() {
                continue;
            }
            for stmt in &block.stmts {
                match &stmt.kind {
                    NSStmtKind::Assign { expr, .. } => match expr {
                        NExpr::ReadPlace { place, mode } => {
                            let kind = if *mode == ReadMode::Move {
                                MemoryAccessKind::Move
                            } else {
                                MemoryAccessKind::Read
                            };
                            let targets =
                                self.canon()
                                    .canonicalize_place(&state, place, stmt.origin)?;
                            self.extend_memory_accesses(
                                &mut items,
                                kind,
                                targets,
                                &no_authorizers,
                                &no_authorizer_targets,
                                stmt.origin,
                            )?;
                        }
                        NExpr::Borrow { place, kind, .. } => {
                            let kind = if *kind == BorrowKind::Mut {
                                MemoryAccessKind::MutAccess
                            } else {
                                MemoryAccessKind::Read
                            };
                            let targets =
                                self.canon()
                                    .canonicalize_place(&state, place, stmt.origin)?;
                            self.extend_memory_accesses(
                                &mut items,
                                kind,
                                targets,
                                &no_authorizers,
                                &no_authorizer_targets,
                                stmt.origin,
                            )?;
                        }
                        NExpr::Call {
                            callee,
                            args,
                            effect_args,
                            ..
                        } => {
                            for access in self.summaries().resolve_call_memory_accesses(
                                &state,
                                callee,
                                args,
                                effect_args,
                                stmt.origin,
                            )? {
                                self.extend_memory_accesses(
                                    &mut items,
                                    access.kind,
                                    access.targets,
                                    &access.authorized,
                                    &access.authorizer_targets,
                                    stmt.origin,
                                )?;
                            }
                        }
                        _ => {}
                    },
                    NSStmtKind::Store { dst, .. } => {
                        let targets = self.canon().canonicalize_place(&state, dst, stmt.origin)?;
                        self.extend_memory_accesses(
                            &mut items,
                            MemoryAccessKind::Write,
                            targets,
                            &no_authorizers,
                            &no_authorizer_targets,
                            stmt.origin,
                        )?;
                    }
                }
                self.state_transfer().apply_stmt(&mut state, stmt)?;
                if !state.is_reachable() {
                    break;
                }
            }
            if !state.is_reachable()
                || !matches!(block.terminator.kind, NSTerminatorKind::Return(_))
            {
                continue;
            }
            return_count += 1;
            if include_pointer_returns
                && let NSTerminatorKind::Return(Some(value)) = block.terminator.kind
            {
                // For capability returns such as `mut *T`, `pointer_slots`
                // strips the capability. The summary describes the pointee
                // provenance exposed through the returned capability.
                for slot in pointer_slots(self.db, result_ty) {
                    let targets = self.canon().pointer_targets_for_value_path(
                        &state,
                        value.local,
                        &slot.path,
                        block.terminator.origin,
                    )?;
                    self.extend_return_pointer_items(
                        &mut items,
                        slot.path,
                        targets.places(),
                        block.terminator.origin,
                    )?;
                }
            }
            let mut seen = FxHashSet::default();
            for (written, targets, precision) in state.pointer_writes_since(&initial) {
                let Some(target) = self.memory_summary_target(written, block.terminator.origin)?
                else {
                    continue;
                };
                let targets = targets
                    .places()
                    .into_iter()
                    .map(|target| {
                        self.pointer_value_summary_target(target, block.terminator.origin)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                extend_pointer_store_items(
                    &mut items,
                    target.clone(),
                    targets,
                    precision == WritePrecision::Weak,
                );
                if seen.insert(target.clone()) {
                    *returns_seen.entry(target).or_insert(0usize) += 1;
                }
            }
        }
        for item in &mut items {
            let MemorySummaryItem::StorePointer { target, weak, .. } = item else {
                continue;
            };
            if returns_seen.get(target).copied().unwrap_or(0) < return_count {
                *weak = true;
            }
        }
        Ok((items, return_count > 0))
    }

    fn extend_memory_accesses(
        &self,
        items: &mut Vec<MemorySummaryItem<'db>>,
        kind: MemoryAccessKind,
        targets: FxHashSet<CanonPlace<'db>>,
        authorized: &FxHashSet<LoanId>,
        authorizer_targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let authorizers = self.memory_access_authorizers(authorized, authorizer_targets, origin)?;
        for target in targets {
            let Some(target) = self.memory_summary_target(target, origin)? else {
                continue;
            };
            items.push(MemorySummaryItem::Access {
                target,
                kind,
                authorizers: authorizers.clone(),
            });
        }
        Ok(())
    }

    fn memory_access_authorizers(
        &self,
        loans: &FxHashSet<LoanId>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<MemorySummaryTarget<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut out = Vec::new();
        let mut targets = targets.clone();
        let mut seen = FxHashSet::default();
        let mut pending = loans.iter().copied().collect::<Vec<_>>();
        while let Some(loan) = pending.pop() {
            if !seen.insert(loan) {
                continue;
            }
            let loan = &self.loans[loan.0 as usize];
            pending.extend(loan.parents.iter().copied());
            targets.extend(loan.targets.iter().cloned());
        }
        for target in targets {
            let Some(target) = self.memory_summary_target(target, origin)? else {
                continue;
            };
            match target {
                MemorySummaryTarget::Input { .. } | MemorySummaryTarget::Effect { .. } => {
                    out.push(target);
                }
                MemorySummaryTarget::FreshAllocation { .. }
                | MemorySummaryTarget::Unknown { .. } => {}
            }
        }
        Ok(out)
    }

    fn initial_pointer_state(&self) -> State<'db> {
        let mut state = State::default();
        for root in &self.body.borrow_roots {
            if let NBorrowRoot::Param { local, param_idx } = root {
                self.state_transfer()
                    .seed_param_pointer_targets(&mut state, *local, *param_idx);
            }
        }
        state
    }

    fn pointer_value_summary_target(
        &self,
        target: CanonPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<MemorySummaryTarget<'db>, SemanticBorrowDiagnostic<'db>> {
        Ok(match target.root {
            BorrowRoot::Param(idx) => self.input_memory_summary_target(idx, &target.proj),
            BorrowRoot::FreshAllocation {
                site,
                address_space,
            } => MemorySummaryTarget::FreshAllocation {
                site,
                address_space,
            },
            BorrowRoot::UnknownMemory(address_spaces) => {
                MemorySummaryTarget::Unknown { address_spaces }
            }
            BorrowRoot::Provider(_) => {
                return Err(self.invalid_return_diag(
                    origin,
                    "cannot write a pointer derived from an effect parameter through a pointer input"
                        .to_string(),
                ));
            }
            BorrowRoot::Local(local) => {
                let name = self.pretty_local_name(local);
                return Err(self.invalid_return_diag(
                    origin,
                    format!("cannot write a pointer to local `{name}` through a pointer input"),
                ));
            }
        })
    }

    fn memory_summary_target(
        &self,
        target: CanonPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Option<MemorySummaryTarget<'db>>, SemanticBorrowDiagnostic<'db>> {
        Ok(match target.root {
            BorrowRoot::UnknownMemory(address_spaces) => {
                Some(MemorySummaryTarget::Unknown { address_spaces })
            }
            BorrowRoot::Provider(binding) => {
                let proj = self.input_summary_projection(&target.proj);
                if let Some(target) = self.body.locals.iter().find_map(|local| {
                    local
                        .facts
                        .origin
                        .root_provider()
                        .is_some_and(|provider| provider == &binding)
                        .then_some(local.source)
                        .flatten()
                        .and_then(|source| match source {
                            LocalBinding::EffectParam { idx, .. }
                            | LocalBinding::Param {
                                site: ParamSite::EffectField(_),
                                idx,
                                ..
                            } => Some(MemorySummaryTarget::Effect {
                                binding_idx: idx as u32,
                                proj: proj.clone(),
                            }),
                            LocalBinding::Param { idx, .. } => Some(MemorySummaryTarget::Input {
                                input: BorrowInputRef::Param(idx as u32),
                                proj: proj.clone(),
                            }),
                            LocalBinding::Local { .. } => None,
                        })
                }) {
                    return Ok(Some(target));
                }
                if let ProviderSource::UsesParam {
                    site,
                    requirement_idx,
                } = &binding.source
                    && effect_param_site(self.body.template_owner) == Some(*site)
                {
                    return Ok(Some(MemorySummaryTarget::Effect {
                        binding_idx: *requirement_idx,
                        proj,
                    }));
                }
                let spaces = address_spaces_for_borrow_root(
                    self.db,
                    self.instance,
                    &self.body,
                    &BorrowRoot::Provider(binding),
                    origin,
                )?;
                Some(MemorySummaryTarget::Unknown {
                    address_spaces: match spaces.as_slice() {
                        [space] => PointerAddressSpaces::one(*space),
                        _ => PointerAddressSpaces::Any,
                    },
                })
            }
            BorrowRoot::Param(idx)
                if self
                    .canon()
                    .param_local(idx)
                    .and_then(|local| self.body.local(local))
                    .is_some_and(|local| local.ty.as_borrow(self.db).is_some()) =>
            {
                Some(MemorySummaryTarget::Input {
                    input: BorrowInputRef::Param(idx),
                    proj: self.input_summary_projection(&target.proj),
                })
            }
            _ if !target
                .proj
                .iter()
                .any(|projection| projection == &Projection::Deref) =>
            {
                None
            }
            BorrowRoot::Param(idx) => Some(MemorySummaryTarget::Input {
                input: BorrowInputRef::Param(idx),
                proj: self.input_summary_projection(&target.proj),
            }),
            BorrowRoot::FreshAllocation { .. } | BorrowRoot::Local(_) => None,
        })
    }

    fn extend_return_pointer_items(
        &self,
        items: &mut Vec<MemorySummaryItem<'db>>,
        output: NSProjectionPath<'db>,
        targets: FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for target in targets {
            let summary_target = match target.root {
                BorrowRoot::Param(idx) => self.input_memory_summary_target(idx, &target.proj),
                BorrowRoot::FreshAllocation {
                    site,
                    address_space,
                } => MemorySummaryTarget::FreshAllocation {
                    site,
                    address_space,
                },
                BorrowRoot::UnknownMemory(address_spaces) => {
                    MemorySummaryTarget::Unknown { address_spaces }
                }
                BorrowRoot::Provider(_) => {
                    return Err(self.invalid_return_diag(
                        origin,
                        "cannot return a pointer derived from an effect parameter".to_string(),
                    ));
                }
                BorrowRoot::Local(local) => {
                    let name = self.pretty_local_name(local);
                    return Err(self.invalid_return_diag(
                        origin,
                        format!("cannot return a pointer to local `{name}`"),
                    ));
                }
            };
            push_return_pointer_target(items, output.clone(), summary_target);
        }
        Ok(())
    }

    fn input_memory_summary_target(
        &self,
        idx: u32,
        proj: &NSProjectionPath<'db>,
    ) -> MemorySummaryTarget<'db> {
        MemorySummaryTarget::Input {
            input: BorrowInputRef::Param(idx),
            proj: self.input_summary_projection(proj),
        }
    }

    fn input_summary_projection(&self, proj: &NSProjectionPath<'db>) -> NSProjectionPath<'db> {
        let mut out = NSProjectionPath::default();
        for projection in proj.iter() {
            out.push(match projection {
                Projection::Index(IndexSource::Dynamic(_)) => Projection::Index(IndexSource::Any),
                projection => projection.clone(),
            });
        }
        out
    }

    fn intrinsic_pointer_return_items(
        &self,
        result_ty: TyId<'db>,
        contract: IntrinsicPointerReturn,
    ) -> Vec<MemorySummaryItem<'db>> {
        let input0_array_elem = || {
            let mut proj = NSProjectionPath::from_projection(Projection::Deref);
            proj.push(Projection::Index(IndexSource::Any));
            MemorySummaryTarget::Input {
                input: BorrowInputRef::Param(0),
                proj,
            }
        };
        let input0_pointee = || MemorySummaryTarget::Input {
            input: BorrowInputRef::Param(0),
            proj: NSProjectionPath::from_projection(Projection::Deref),
        };
        let input0_mem_array_elem = || {
            let mut proj = NSProjectionPath::from_projection(Projection::Field(0));
            proj.push(Projection::Deref);
            proj.push(Projection::Index(IndexSource::Any));
            MemorySummaryTarget::Input {
                input: BorrowInputRef::Param(0),
                proj,
            }
        };
        let target = match contract {
            IntrinsicPointerReturn::FreshMemory => MemorySummaryTarget::FreshAllocation {
                site: FreshAllocSite::Direct(SemOrigin::Synthetic),
                address_space: ProviderAddressSpace::Memory,
            },
            IntrinsicPointerReturn::InputPointee => input0_pointee(),
            IntrinsicPointerReturn::InputArrayElem => input0_array_elem(),
            IntrinsicPointerReturn::InputMemArrayElem => input0_mem_array_elem(),
        };
        pointer_slots(self.db, result_ty)
            .into_iter()
            .map(|slot| MemorySummaryItem::ReturnPointer {
                output: slot.path,
                targets: vec![target.clone()],
            })
            .collect()
    }

    fn intrinsic_memory_items(
        &self,
        contract: IntrinsicMemoryContract,
    ) -> Vec<MemorySummaryItem<'db>> {
        let mut items = Vec::with_capacity(contract.len() * 2);
        for access in contract {
            let proj = match access.projection {
                IntrinsicMemoryProjection::Value => NSProjectionPath::default(),
                IntrinsicMemoryProjection::Pointee => {
                    NSProjectionPath::from_projection(Projection::Deref)
                }
            };
            let target = MemorySummaryTarget::Input {
                input: BorrowInputRef::Param(access.input),
                proj,
            };
            items.push(MemorySummaryItem::Access {
                target: target.clone(),
                kind: access.kind,
                authorizers: Vec::new(),
            });
            if access.kind == MemoryAccessKind::Write {
                items.push(MemorySummaryItem::StorePointer {
                    target,
                    targets: vec![MemorySummaryTarget::Unknown {
                        address_spaces: PointerAddressSpaces::one(ProviderAddressSpace::Memory),
                    }],
                    weak: true,
                });
            }
        }
        items
    }

    fn unknown_pointer_return_items(&self, result_ty: TyId<'db>) -> Vec<MemorySummaryItem<'db>> {
        pointer_slots(self.db, result_ty)
            .into_iter()
            .map(|slot| MemorySummaryItem::ReturnPointer {
                output: slot.path,
                targets: vec![MemorySummaryTarget::Unknown {
                    address_spaces: PointerAddressSpaces::one(ProviderAddressSpace::Memory),
                }],
            })
            .collect()
    }

    fn bodyless_memory_items(
        &self,
        evm_receiver: Option<BorrowInputRef>,
    ) -> Vec<MemorySummaryItem<'db>> {
        let mut items = Vec::new();
        let address_spaces = PointerAddressSpaces::one(ProviderAddressSpace::Memory);
        for root in &self.body.borrow_roots {
            let NBorrowRoot::Param { local, param_idx } = root else {
                continue;
            };
            let Some(ty) = self.body.local(*local).map(|local| local.ty) else {
                continue;
            };
            let input = BorrowInputRef::Param(*param_idx);
            let borrow_kind = ty.as_borrow(self.db).map(|(kind, _)| kind);
            if let Some(kind) = borrow_kind {
                items.push(MemorySummaryItem::Access {
                    target: MemorySummaryTarget::Input {
                        input,
                        proj: NSProjectionPath::default(),
                    },
                    kind: if kind == BorrowKind::Mut {
                        MemoryAccessKind::MutAccess
                    } else {
                        MemoryAccessKind::Read
                    },
                    authorizers: Vec::new(),
                });
            }
            if evm_receiver == Some(input) {
                continue;
            }
            let reachable = pointer_reachability(self.db, ty, borrow_kind == Some(BorrowKind::Mut));
            items.extend(
                reachable
                    .targets
                    .into_iter()
                    .map(|proj| MemorySummaryItem::Access {
                        target: MemorySummaryTarget::Input { input, proj },
                        kind: MemoryAccessKind::MutAccess,
                        authorizers: Vec::new(),
                    }),
            );
            items.extend(reachable.writable_slots.into_iter().map(|proj| {
                MemorySummaryItem::StorePointer {
                    target: MemorySummaryTarget::Input { input, proj },
                    targets: vec![MemorySummaryTarget::Unknown { address_spaces }],
                    weak: true,
                }
            }));
            if reachable.unbounded {
                items.push(MemorySummaryItem::Access {
                    target: MemorySummaryTarget::Unknown { address_spaces },
                    kind: MemoryAccessKind::MutAccess,
                    authorizers: vec![MemorySummaryTarget::Input {
                        input,
                        proj: NSProjectionPath::default(),
                    }],
                });
                items.push(MemorySummaryItem::StorePointer {
                    target: MemorySummaryTarget::Unknown { address_spaces },
                    targets: vec![MemorySummaryTarget::Unknown { address_spaces }],
                    weak: true,
                });
            }
        }
        items
    }

    fn effective_loans(
        &self,
        state: &State<'db>,
        live: &FxHashSet<crate::analysis::semantic::SLocalId>,
        point: (SBlockId, usize),
    ) -> Vec<ActiveLoan> {
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
        let mut active: Vec<ActiveLoan> = active
            .into_iter()
            .filter(|loan| !suspended.contains(loan))
            .map(|id| ActiveLoan {
                id,
                kind: self.loan_kind_at(id, point),
                reservation: self.is_two_phase_reservation_at(id, point),
            })
            .collect();
        active.sort_by_key(|loan| loan.id.0);
        active
    }

    fn loan_kind_at(&self, loan: LoanId, point: (SBlockId, usize)) -> BorrowKind {
        if self.is_two_phase_reservation_at(loan, point) {
            BorrowKind::Ref
        } else {
            self.loans[loan.0 as usize].kind
        }
    }

    fn is_two_phase_reservation_at(&self, loan: LoanId, point: (SBlockId, usize)) -> bool {
        self.two_phase_activations
            .get(&loan)
            .is_some_and(|activation| *activation != point)
    }

    fn first_loan_conflict(
        &self,
        active: &[ActiveLoan],
        new_kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<ActiveLoan> {
        active.iter().copied().find(|loan| {
            if matches!((loan.kind, new_kind), (BorrowKind::Ref, BorrowKind::Ref)) {
                return false;
            }
            let loan_targets = &self.loans[loan.id.0 as usize].targets;
            if loan.reservation {
                if loan_targets.iter().all(|target| {
                    matches!(target.root, BorrowRoot::Provider(_)) && target.proj.is_empty()
                }) {
                    return false;
                }
                loan_targets.iter().any(|loan_target| {
                    targets.iter().any(|target| {
                        !matches!(
                            (&loan_target.root, &target.root),
                            (BorrowRoot::UnknownMemory(_), _) | (_, BorrowRoot::UnknownMemory(_))
                        ) && self.places_overlap(loan_target, target)
                    })
                })
            } else {
                self.place_sets_overlap(loan_targets, targets)
            }
        })
    }

    fn place_sets_overlap(
        &self,
        lhs: &FxHashSet<CanonPlace<'db>>,
        rhs: &FxHashSet<CanonPlace<'db>>,
    ) -> bool {
        lhs.iter()
            .any(|lhs| rhs.iter().any(|rhs| self.places_overlap(lhs, rhs)))
    }

    fn places_overlap(&self, lhs: &CanonPlace<'db>, rhs: &CanonPlace<'db>) -> bool {
        if matches!(lhs.root, BorrowRoot::UnknownMemory(_)) && self.place_root_is_zero_sized(rhs)
            || matches!(rhs.root, BorrowRoot::UnknownMemory(_))
                && self.place_root_is_zero_sized(lhs)
        {
            return false;
        }
        canonical_places_overlap(lhs, rhs)
    }

    fn place_root_is_zero_sized(&self, place: &CanonPlace<'db>) -> bool {
        let local = match place.root {
            BorrowRoot::Local(local) => Some(local),
            BorrowRoot::Param(idx) => self.canon().param_local(idx),
            BorrowRoot::Provider(_)
            | BorrowRoot::FreshAllocation { .. }
            | BorrowRoot::UnknownMemory(_) => None,
        };
        local
            .and_then(|local| self.body.local(local))
            .map(|local| {
                local
                    .ty
                    .as_capability(self.db)
                    .map_or(local.ty, |(_, target)| target)
            })
            .is_some_and(|ty| ty.is_zero_sized(self.db))
    }

    fn check_move_out(
        &self,
        active: &[ActiveLoan],
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
        active: &[ActiveLoan],
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for target in targets {
            if let BorrowRoot::Param(idx) = target.root
                && !target
                    .proj
                    .iter()
                    .any(|proj| matches!(proj, Projection::Deref))
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
            .find(|loan| self.place_sets_overlap(&self.loans[loan.id.0 as usize].targets, targets))
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
        state: &State<'db>,
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
                    if let NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } = expr
                    {
                        let site = MoveSite {
                            origin: stmt.origin,
                            note: "value is moved by this call".to_string(),
                        };
                        for access in self.summaries().resolve_call_memory_accesses(
                            state,
                            callee,
                            args,
                            effect_args,
                            stmt.origin,
                        )? {
                            if access.kind == MemoryAccessKind::Move {
                                for target in access.targets {
                                    moved.insert(target, site.clone());
                                }
                            }
                        }
                    }
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
        state: &State<'db>,
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
        state: &State<'db>,
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
        state: &State<'db>,
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
        state: &State<'db>,
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
        state: &State<'db>,
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
                .any(|accessed| places_overlap_for_move(moved, accessed))
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
                    && canonical_places_overlap(moved, written)
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

    fn overlapping_loans_msg(&self, loan: ActiveLoan, new_kind: BorrowKind) -> String {
        match (new_kind, loan.kind) {
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

fn places_overlap_for_move<'db>(moved: &CanonPlace<'db>, accessed: &CanonPlace<'db>) -> bool {
    if matches!(moved.root, BorrowRoot::UnknownMemory(_))
        || matches!(accessed.root, BorrowRoot::UnknownMemory(_))
    {
        return false;
    }
    canonical_places_overlap(moved, accessed)
}

fn push_return_pointer_target<'db>(
    items: &mut Vec<MemorySummaryItem<'db>>,
    output: NSProjectionPath<'db>,
    target: MemorySummaryTarget<'db>,
) {
    if let Some(targets) = items.iter_mut().find_map(|item| match item {
        MemorySummaryItem::ReturnPointer {
            output: item_output,
            targets,
        } if item_output == &output => Some(targets),
        _ => None,
    }) {
        if !targets.contains(&target) {
            targets.push(target);
        }
        return;
    }
    items.push(MemorySummaryItem::ReturnPointer {
        output,
        targets: vec![target],
    });
}

fn extend_pointer_store_items<'db>(
    items: &mut Vec<MemorySummaryItem<'db>>,
    target: MemorySummaryTarget<'db>,
    mut targets: Vec<MemorySummaryTarget<'db>>,
    weak: bool,
) {
    if let Some((item_targets, item_weak)) = items.iter_mut().find_map(|item| match item {
        MemorySummaryItem::StorePointer {
            target: item_target,
            targets,
            weak,
        } if item_target == &target => Some((targets, weak)),
        _ => None,
    }) {
        item_targets.append(&mut targets);
        *item_weak |= weak;
    } else {
        items.push(MemorySummaryItem::StorePointer {
            target,
            targets,
            weak,
        });
    }
}

fn normalize_memory_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    items: &mut Vec<MemorySummaryItem<'db>>,
) {
    let mut normalized = Vec::with_capacity(items.len());
    for item in std::mem::take(items) {
        match item {
            MemorySummaryItem::ReturnPointer {
                output,
                mut targets,
            } => {
                normalize_memory_summary_targets(db, &mut targets);
                if let Some(existing) = normalized.iter_mut().find_map(|item| match item {
                    MemorySummaryItem::ReturnPointer {
                        output: item_output,
                        targets,
                    } if item_output == &output => Some(targets),
                    _ => None,
                }) {
                    existing.extend(targets);
                    normalize_memory_summary_targets(db, existing);
                } else {
                    normalized.push(MemorySummaryItem::ReturnPointer { output, targets });
                }
            }
            MemorySummaryItem::Access {
                target,
                kind,
                mut authorizers,
            } => {
                normalize_memory_summary_targets(db, &mut authorizers);
                let item = MemorySummaryItem::Access {
                    target,
                    kind,
                    authorizers,
                };
                if !normalized.contains(&item) {
                    normalized.push(item);
                }
            }
            MemorySummaryItem::StorePointer {
                target,
                mut targets,
                weak,
            } => {
                normalize_memory_summary_targets(db, &mut targets);
                if let Some((existing_targets, existing_weak)) =
                    normalized.iter_mut().find_map(|item| match item {
                        MemorySummaryItem::StorePointer {
                            target: item_target,
                            targets,
                            weak,
                        } if item_target == &target => Some((targets, weak)),
                        _ => None,
                    })
                {
                    existing_targets.extend(targets);
                    normalize_memory_summary_targets(db, existing_targets);
                    *existing_weak |= weak;
                } else {
                    normalized.push(MemorySummaryItem::StorePointer {
                        target,
                        targets,
                        weak,
                    });
                }
            }
        }
    }
    normalized.sort_by_key(|item| memory_summary_item_sort_key(db, item));
    *items = normalized;
}

fn memory_summary_target_sort_key<'db>(
    db: &'db dyn HirAnalysisDb,
    target: &MemorySummaryTarget<'db>,
) -> String {
    match target {
        MemorySummaryTarget::Input { input, proj } => format!(
            "0:{input:?}:{}",
            projection_path_sort_key(db, proj).join("/")
        ),
        MemorySummaryTarget::Effect { binding_idx, proj } => format!(
            "1:{binding_idx}:{}",
            projection_path_sort_key(db, proj).join("/")
        ),
        MemorySummaryTarget::FreshAllocation {
            site,
            address_space,
        } => {
            format!("2:{}:{site:?}", address_space_rank(*address_space))
        }
        MemorySummaryTarget::Unknown { address_spaces } => {
            format!("3:{}", address_spaces_sort_key(*address_spaces))
        }
    }
}

fn memory_summary_item_sort_key<'db>(
    db: &'db dyn HirAnalysisDb,
    item: &MemorySummaryItem<'db>,
) -> String {
    match item {
        MemorySummaryItem::ReturnPointer { output, .. } => {
            format!("0:{}", projection_path_sort_key(db, output).join("/"))
        }
        MemorySummaryItem::Access {
            target,
            kind,
            authorizers,
        } => format!(
            "1:{}:{}:{}",
            memory_summary_target_sort_key(db, target),
            memory_access_kind_rank(*kind),
            authorizers
                .iter()
                .map(|target| memory_summary_target_sort_key(db, target))
                .collect::<Vec<_>>()
                .join(",")
        ),
        MemorySummaryItem::StorePointer { target, .. } => {
            format!("2:{}", memory_summary_target_sort_key(db, target))
        }
    }
}

fn memory_access_kind_rank(kind: MemoryAccessKind) -> u8 {
    match kind {
        MemoryAccessKind::Read => 0,
        MemoryAccessKind::MutAccess => 1,
        MemoryAccessKind::Write => 2,
        MemoryAccessKind::Move => 3,
    }
}

fn normalize_memory_summary_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    targets: &mut Vec<MemorySummaryTarget<'db>>,
) {
    targets.sort_by_key(|target| memory_summary_target_sort_key(db, target));
    targets.dedup();
}

fn projection_path_sort_key<'db>(
    db: &'db dyn HirAnalysisDb,
    path: &NSProjectionPath<'db>,
) -> Vec<String> {
    path.iter()
        .map(|projection| projection_sort_key(db, projection))
        .collect()
}

fn projection_sort_key<'db>(
    db: &'db dyn HirAnalysisDb,
    projection: &Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
) -> String {
    match projection {
        Projection::Field(idx) => format!("0:{idx}"),
        Projection::VariantField {
            enum_ty,
            variant,
            field_idx,
        } => {
            format!("1:{}:{field_idx}:{}", variant.0, enum_ty.pretty_print(db))
        }
        Projection::Discriminant => "2".to_string(),
        Projection::Index(IndexSource::Constant(idx)) => format!("3:0:{idx}"),
        Projection::Index(IndexSource::Dynamic(local)) => format!("3:1:{}", local.index()),
        Projection::Index(IndexSource::Any) => "3:2".to_string(),
        Projection::Deref => "4".to_string(),
    }
}

fn address_spaces_sort_key(spaces: PointerAddressSpaces) -> String {
    match spaces {
        PointerAddressSpaces::One(space) => address_space_rank(space).to_string(),
        PointerAddressSpaces::Any => "any".to_string(),
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

fn semantic_memory_summary_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticMemorySummaryResult<'db> {
    let result_ty = instance.normalized_result_ty(db);
    let mut items = if instance_returns_pointer_bearing(db, instance) {
        pointer_slots(db, result_ty)
            .into_iter()
            .map(|slot| MemorySummaryItem::ReturnPointer {
                output: slot.path,
                targets: vec![MemorySummaryTarget::Unknown {
                    address_spaces: PointerAddressSpaces::one(ProviderAddressSpace::Memory),
                }],
            })
            .collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    normalize_memory_summary(db, &mut items);
    SemanticMemorySummaryResult::Ok(MemorySummaryId::new(db, items, false))
}

fn instance_returns_pointer_bearing<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    is_pointer_bearing_type(db, instance.normalized_result_ty(db))
}

fn semantic_borrow_summary_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &SemanticBorrowSummaryResult<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

fn semantic_memory_summary_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &SemanticMemorySummaryResult<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticMemorySummaryResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}
