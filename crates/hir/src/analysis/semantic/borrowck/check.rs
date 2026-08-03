use std::{
    collections::{BTreeSet, VecDeque},
    mem,
};

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
            BorrowActivation, FieldIndex, LayoutBackingProjection, Mutability, SBlockId,
            SCallReturnProjectionStep, SConst, SLocalId, SStmtId, SemConstScalar, SemConstValue,
            SemOrigin, SemanticInstance, SemanticInstanceCompleteness, SemanticInstanceKey,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            non_regular_recursive_call_diagnostic,
            semantic_instance_is_in_non_regular_recursive_component,
        },
        ty::{
            ProviderAddressSpace,
            const_ty::CallableInputLayoutHoleOrigin,
            corelib::{PrimitiveWrapperCallKind, core_primitive_wrapper_call_kind},
            ty_check::{
                BodyOwner, CLOSURE_ARGS_PARAM_IDX, LocalBinding, ParamSite, ReturnProjectionStep,
                ReturnProvenance,
            },
            ty_def::{BorrowKind, CapabilityKind, ClosureParamMode, TyId},
            ty_is_borrow,
        },
    },
    hir_def::{
        BinOp, Body, ClosureDef, CompBinOp, Expr, ExprId, FuncParamMode, ItemKind, LogicalBinOp,
        Partial, StmtId, TopLevelMod, UnOp,
    },
    projection::{IndexSource, Projection, ProjectionPath},
};

use super::{
    analyses::{
        BorrowEntryStateAnalysis, BorrowLivenessAnalysis, BorrowLoanTargetAnalysis,
        BorrowLoanTargetInputs, BorrowLoanTargetState, BorrowMovedStateAnalysis, BorrowSummaryMode,
    },
    canon::{
        BlockAdjacency, BorrowCanonCx, BorrowRoot, CanonPlace, CanonProjectionPath, CfgAdjacency,
        FamilyBindings, Loan, LoanId, MoveSite, MovedPlaces, State, address_space_rank,
        known_address_space_for_borrow_root, layout_path_for_canon_projection, place_set_overlaps,
        places_overlap,
    },
    definite_init::check_definite_initialization,
    diagnostics::operand_origin,
    facts::NormalizedBodyFacts,
    ir::{
        BorrowDiagnosticId, BorrowInput, BorrowResult, BorrowSlotFamilyIds, BorrowSummary,
        BorrowSummaryId, BorrowTransform, NBorrowRoot, NBorrowRootId, NCallReturnSources,
        NEffectArg, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot, NSProjectionPath,
        NSStmtKind, NSTerminatorKind, NormalizedBindingLowering, NormalizedSemanticBody, ReadMode,
        SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult, borrow_results_in_ty,
        borrow_results_in_ty_with_family_ids, local_has_runtime_move_semantics,
        return_borrow_results_in_ty, return_borrow_results_in_ty_with_family_ids,
        return_source_borrow_input_reaches_capability, semantic_projection_for_layout_path,
        semantic_projection_ty, store_rebinds_capability,
    },
    normalize::{normalize_provisional_semantic_body, normalize_semantic_body_for_analysis},
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
    if instance.key(db).completeness(db) == SemanticInstanceCompleteness::Partial {
        return provisional_borrow_summary_query(db, instance);
    }
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
    match Borrowck::new_for_check(db, instance).and_then(Borrowck::check) {
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
    let mut seen_non_regular_sites = FxHashSet::default();
    let mut seen_uninitialized_sites = FxHashSet::default();
    collect_top_mod_semantic_borrow_diagnostic_vouchers(db, top_mod, &mut pending);
    let mut seen_instances = FxHashSet::default();
    while let Some(instance) = pending.pop_front() {
        if !seen_instances.insert(instance.key(db)) {
            continue;
        }
        collect_instance(
            db,
            instance,
            &mut pending,
            &mut seen_diags,
            &mut seen_non_regular_sites,
            &mut seen_uninitialized_sites,
            &mut diags,
        );
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum BorrowDiagnosticOwnerSite<'db> {
    Body(Body<'db>),
    Closure(ClosureDef<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum BorrowDiagnosticOriginSite<'db> {
    Expr(ExprId),
    Stmt(StmtId),
    Body(BorrowDiagnosticOwnerSite<'db>),
    Synthetic,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum UninitializedLocalSite<'db> {
    Binding(LocalBinding<'db>),
    Synthetic {
        owner: BorrowDiagnosticOwnerSite<'db>,
        local: usize,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct UninitializedDiagnosticSite<'db> {
    owner: BorrowDiagnosticOwnerSite<'db>,
    origin: BorrowDiagnosticOriginSite<'db>,
    local: UninitializedLocalSite<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct NonRegularDiagnosticSite<'db> {
    owner: BorrowDiagnosticOwnerSite<'db>,
    origin: BorrowDiagnosticOriginSite<'db>,
}

fn diagnostic_owner_site<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<BorrowDiagnosticOwnerSite<'db>> {
    match owner {
        BodyOwner::Closure { def, .. } => Some(BorrowDiagnosticOwnerSite::Closure(def)),
        owner => owner.body(db).map(BorrowDiagnosticOwnerSite::Body),
    }
}

fn diagnostic_origin_site<'db>(
    db: &'db dyn HirAnalysisDb,
    origin: SemOrigin<'db>,
) -> Option<BorrowDiagnosticOriginSite<'db>> {
    match origin {
        SemOrigin::Expr(expr) => Some(BorrowDiagnosticOriginSite::Expr(expr)),
        SemOrigin::Stmt(stmt) => Some(BorrowDiagnosticOriginSite::Stmt(stmt)),
        SemOrigin::Body(owner) => {
            diagnostic_owner_site(db, owner).map(BorrowDiagnosticOriginSite::Body)
        }
        SemOrigin::Synthetic => Some(BorrowDiagnosticOriginSite::Synthetic),
    }
}

fn uninitialized_diagnostic_site<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: BorrowDiagnosticId<'db>,
) -> Option<UninitializedDiagnosticSite<'db>> {
    let diagnostic = diagnostic.diag(db);
    if diagnostic.kind != SemanticBorrowDiagKind::UninitializedLocal {
        return None;
    }
    let (owner, origin) = match &diagnostic.primary.span {
        SemanticBorrowDiagnosticSpan::Origin { owner, origin }
        | SemanticBorrowDiagnosticSpan::OriginWithTemplateFallback { owner, origin, .. } => (
            diagnostic_owner_site(db, *owner)?,
            diagnostic_origin_site(db, *origin)?,
        ),
        SemanticBorrowDiagnosticSpan::LocalSourceOrBody { .. } => return None,
    };
    let local = diagnostic.secondaries.iter().find_map(|secondary| {
        let SemanticBorrowDiagnosticSpan::LocalSourceOrBody { instance, local } = &secondary.span
        else {
            return None;
        };
        let instance_owner = diagnostic_owner_site(db, instance.key(db).owner(db))?;
        let body = if instance.key(db).completeness(db) == SemanticInstanceCompleteness::Partial {
            instance.provisional_body(db)
        } else {
            instance.body(db)
        };
        Some(
            body.local(*local)
                .and_then(|local| local.source)
                .map_or_else(
                    || UninitializedLocalSite::Synthetic {
                        owner: instance_owner,
                        local: local.index(),
                    },
                    UninitializedLocalSite::Binding,
                ),
        )
    })?;
    Some(UninitializedDiagnosticSite {
        owner,
        origin,
        local,
    })
}

fn non_regular_diagnostic_site<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: BorrowDiagnosticId<'db>,
) -> Option<NonRegularDiagnosticSite<'db>> {
    let diagnostic = diagnostic.diag(db);
    if diagnostic.kind != SemanticBorrowDiagKind::NonRegularPolymorphicRecursion {
        return None;
    }
    let (owner, origin) = match &diagnostic.primary.span {
        SemanticBorrowDiagnosticSpan::Origin { owner, origin }
        | SemanticBorrowDiagnosticSpan::OriginWithTemplateFallback { owner, origin, .. } => (
            diagnostic_owner_site(db, *owner)?,
            diagnostic_origin_site(db, *origin)?,
        ),
        SemanticBorrowDiagnosticSpan::LocalSourceOrBody { .. } => return None,
    };
    Some(NonRegularDiagnosticSite { owner, origin })
}

fn push_distinct_borrow_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: BorrowDiagnosticId<'db>,
    seen_diags: &mut FxHashSet<BorrowDiagnosticId<'db>>,
    seen_non_regular_sites: &mut FxHashSet<NonRegularDiagnosticSite<'db>>,
    seen_uninitialized_sites: &mut FxHashSet<UninitializedDiagnosticSite<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if !seen_diags.insert(diagnostic) {
        return;
    }
    if let Some(site) = uninitialized_diagnostic_site(db, diagnostic)
        && !seen_uninitialized_sites.insert(site)
    {
        return;
    }
    if let Some(site) = non_regular_diagnostic_site(db, diagnostic)
        && !seen_non_regular_sites.insert(site)
    {
        return;
    }
    diags.push(Box::new(diagnostic));
}

fn collect_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    pending: &mut VecDeque<SemanticInstance<'db>>,
    seen_diags: &mut FxHashSet<BorrowDiagnosticId<'db>>,
    seen_non_regular_sites: &mut FxHashSet<NonRegularDiagnosticSite<'db>>,
    seen_uninitialized_sites: &mut FxHashSet<UninitializedDiagnosticSite<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    let key = instance.key(db);
    let owner = key.owner(db);
    let typed_body = key.typed_body(db);
    if typed_body.has_smir_lowering_blocker(db) {
        return;
    }
    let is_identity = key == identity_semantic_instance_key(db, owner);
    let is_non_regular = semantic_instance_is_in_non_regular_recursive_component(db, instance);
    if (is_identity || is_non_regular)
        && let Some(diag) = non_regular_recursive_call_diagnostic(db, instance)
    {
        push_distinct_borrow_diagnostic(
            db,
            diag,
            seen_diags,
            seen_non_regular_sites,
            seen_uninitialized_sites,
            diags,
        );
    }
    if is_non_regular {
        return;
    }
    let has_closures = typed_body.closure_infos().next().is_some();
    let has_concrete_provider_specialization = !key.effect_providers(db).providers(db).is_empty()
        && key.completeness(db) == SemanticInstanceCompleteness::Complete;
    if is_identity
        || has_closures
        || matches!(owner, BodyOwner::Closure { .. })
        || has_concrete_provider_specialization
    {
        if let SemanticBorrowCheckResult::Err(diag) = semantic_borrow_check_query(db, instance) {
            push_distinct_borrow_diagnostic(
                db,
                diag,
                seen_diags,
                seen_non_regular_sites,
                seen_uninitialized_sites,
                diags,
            );
        }
        if let super::ir::SemanticBorrowCheckResult::Err(diag) =
            super::noesc::semantic_noesc_check_query(db, instance)
        {
            push_distinct_borrow_diagnostic(
                db,
                diag,
                seen_diags,
                seen_non_regular_sites,
                seen_uninitialized_sites,
                diags,
            );
        }
    }
    if has_closures && !matches!(owner, BodyOwner::Closure { .. }) && owner.body(db).is_some() {
        for (expr, _) in typed_body.closure_infos() {
            if let Some(closure_ty) = typed_body.expr_ty(db, expr).as_closure(db) {
                pending.push_back(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(db, BodyOwner::closure(db, closure_ty)),
                ));
            }
        }
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
            .filter(|callee| callee.key.completeness(db) == SemanticInstanceCompleteness::Complete)
            .map(|callee| get_or_build_semantic_instance(db, callee.key)),
    );
}

pub(super) struct Borrowck<'db> {
    pub(super) db: &'db dyn HirAnalysisDb,
    pub(super) instance: SemanticInstance<'db>,
    pub(super) body: NormalizedSemanticBody<'db>,
    pub(super) facts: NormalizedBodyFacts,
    pub(super) summary_mode: BorrowSummaryMode,
    cfg_successors: CfgAdjacency,
    hir_body: Option<Body<'db>>,
    param_modes: Vec<FuncParamMode>,
    param_index_of_local: FxHashMap<crate::analysis::semantic::SLocalId, u32>,
    pub(super) loan_for_local: FxHashMap<crate::analysis::semantic::SLocalId, LoanId>,
    pub(super) param_loan_for_local: FxHashMap<crate::analysis::semantic::SLocalId, LoanId>,
    loans: Vec<Loan<'db>>,
    pub(super) entry_state: SecondaryMap<SBlockId, State>,
    call_result_loans: FxHashMap<SStmtId, Vec<(BorrowResult, LoanId)>>,
    call_loan_transforms: FxHashMap<LoanId, Vec<BorrowTransform>>,
    fresh_call_args: FxHashSet<(SStmtId, SLocalId)>,
    fresh_local_allocations: FxHashSet<(SStmtId, SLocalId)>,
    fresh_local_sites: FxHashMap<SLocalId, Vec<SStmtId>>,
    fresh_status_at_stmt: FreshStatusAtStmt<'db>,
    constant_indices: SecondaryMap<crate::analysis::semantic::SLocalId, Option<usize>>,
    index_value_identities: SecondaryMap<crate::analysis::semantic::SLocalId, Option<SLocalId>>,
    index_phi_edge_substitutions: IndexPhiEdgeSubstitutions,
    moved_entry: SecondaryMap<SBlockId, MovedPlaces<'db>>,
    live_before: Vec<Vec<FxHashSet<crate::analysis::semantic::SLocalId>>>,
    live_before_term: SecondaryMap<SBlockId, FxHashSet<crate::analysis::semantic::SLocalId>>,
}

struct CallAccess<'db> {
    group: usize,
    role: CallAccessRole,
    kind: BorrowKind,
    targets: FxHashSet<CanonPlace<'db>>,
    origin: SemOrigin<'db>,
}

#[derive(Clone, Copy)]
struct ConflictCheckCx<'a, 'db> {
    state: &'a State,
    moved: &'a MovedPlaces<'db>,
    active: &'a [LoanId],
}

type FreshRootStates<'db> = FxHashMap<FreshStorageRoot, Vec<FreshRootStatus<'db>>>;
type FreshLocalStatus<'db> = FxHashMap<SLocalId, FreshValueStatus<'db>>;
type FreshStatusAtStmt<'db> = FxHashMap<(SStmtId, SLocalId), FreshValueStatus<'db>>;
type FreshCarrierPath = Vec<LayoutBackingProjection>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct FreshValueStatus<'db> {
    roots: FreshRootStates<'db>,
    /// Whether `roots` accounts for every dynamically allocated root which
    /// this value can carry. Missing roots are impossible only when this is
    /// true.
    complete: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FreshRootStatus<'db> {
    generation: FreshGeneration,
    /// The latest static retention sites through which this dynamic instance
    /// reached storage that can outlive the allocation's current loop
    /// iteration.
    ///
    /// Intersecting sets conservatively denote possibly identical instances.
    /// Disjoint sets let the checker retain separation between instances from
    /// one allocation site that were retained by mutually exclusive
    /// statements. A later retention renames every current alias together, so
    /// copying one instance through multiple statements cannot make it appear
    /// disjoint from itself.
    retention_sites: FxHashSet<SStmtId>,
    /// Locations within the containing value which carry this instance.
    carriers: FxHashSet<FreshCarrierPath>,
    claimed: FxHashSet<CanonProjectionPath<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum FreshStorageRoot {
    Call {
        stmt: SStmtId,
        source: SLocalId,
    },
    Local {
        definition: SStmtId,
        local: SLocalId,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FreshGeneration {
    /// Produced by the current dynamic execution of its allocation site.
    Current,
    /// May have crossed a backedge since its allocation site executed.
    Stale,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallAccessRole {
    Container,
    BorrowSlot,
}

impl<'db> Borrowck<'db> {
    /// Final checks consume finalized provider roots. Hybrid instances instead
    /// use the provisional body and summaries so caller-local parameters stay
    /// parametric until a complete specialization is reached.
    pub(super) fn new_for_check(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        match instance.key(db).completeness(db) {
            SemanticInstanceCompleteness::Partial => {
                let body = normalize_provisional_semantic_body(db, instance)?;
                Self::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)
            }
            SemanticInstanceCompleteness::Complete | SemanticInstanceCompleteness::Parametric => {
                Self::new(db, instance)
            }
        }
    }

    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        let body = normalize_semantic_body_for_analysis(db, instance)?;
        Self::new_with_body(db, instance, body, BorrowSummaryMode::FinalCheck)
    }

    fn new_for_summary(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticBorrowDiagnostic<'db>> {
        let body = normalize_semantic_body_for_analysis(db, instance)?;
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
        let param_modes = instance
            .key(db)
            .callable_body(db)
            .param_bindings(db)
            .into_iter()
            .map(|binding| match binding {
                crate::analysis::ty::ty_check::LocalBinding::Param { mode, .. } => mode,
                crate::analysis::ty::ty_check::LocalBinding::Local { .. }
                | crate::analysis::ty::ty_check::LocalBinding::EffectParam { .. } => {
                    FuncParamMode::Own
                }
            })
            .collect();
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
        let cfg_successors = normalized_cfg_successor_indices(db, &body);
        let reachable_blocks = cfg_reachable_blocks(&cfg_successors);
        let assignment_is_reachable = |assignment| {
            facts
                .assignment(assignment)
                .is_some_and(|assignment| reachable_blocks.contains(&assignment.block))
        };
        let mut local_use_sites = vec![Vec::<Option<SStmtId>>::new(); body.locals.len()];
        for (block_idx, block) in body.blocks.iter().enumerate() {
            let block_id = SBlockId::new(block_idx);
            if !reachable_blocks.contains(&block_id) {
                continue;
            }
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                for &local in facts.stmt_uses(block_id, stmt_idx) {
                    local_use_sites[local.index()].push(Some(stmt.id));
                }
            }
            for &local in facts.terminator_uses(block_id) {
                local_use_sites[local.index()].push(None);
            }
        }
        let mut fresh_call_args = FxHashSet::default();
        for (block_idx, block) in body.blocks.iter().enumerate() {
            let block_id = SBlockId::new(block_idx);
            if !reachable_blocks.contains(&block_id) {
                continue;
            }
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                let NSStmtKind::Assign {
                    dst: call_result,
                    expr: NExpr::Call { callee, args, .. },
                } = &stmt.kind
                else {
                    continue;
                };
                let Some(assignment) = facts.stmt_assignment(block_id, stmt_idx) else {
                    continue;
                };
                let params = callee.key.callable_body(db).param_bindings(db);
                for (arg_idx, arg) in args.iter().enumerate() {
                    let local = arg.local;
                    let is_owned_param = matches!(
                        params.get(arg_idx),
                        Some(LocalBinding::Param {
                            mode: FuncParamMode::Own,
                            ..
                        })
                    );
                    let is_single_use_temporary = is_owned_param
                        && body.local(local).is_some_and(|local_data| {
                            local_data.source.is_none()
                                && local_data.lowering.root().is_some_and(|root| {
                                    matches!(
                                        body.root(root),
                                        Some(NBorrowRoot::LocalSlot { local: root_local })
                                            if *root_local == local
                                    )
                                })
                        })
                        && facts
                            .defs_by_local(local)
                            .iter()
                            .copied()
                            .filter(|assignment| assignment_is_reachable(*assignment))
                            .count()
                            == 1
                        && facts
                            .assignments_using_local(local)
                            .iter()
                            .copied()
                            .filter(|assignment| assignment_is_reachable(*assignment))
                            .eq([assignment])
                        && facts
                            .dynamic_dependents(local)
                            .iter()
                            .filter(|dependent| {
                                **dependent == local
                                    || facts
                                        .defs_by_local(**dependent)
                                        .iter()
                                        .copied()
                                        .any(assignment_is_reachable)
                                    || !local_use_sites[dependent.index()].is_empty()
                            })
                            .all(|dependent| *dependent == local || dependent == call_result)
                        && local_use_sites[local.index()] == [Some(stmt.id)]
                        && args
                            .iter()
                            .filter(|candidate| candidate.local == local)
                            .count()
                            == 1;
                    if is_single_use_temporary {
                        fresh_call_args.insert((stmt.id, local));
                    }
                }
            }
        }
        let mut fresh_local_allocations = FxHashSet::default();
        let mut fresh_local_sites = FxHashMap::<SLocalId, Vec<SStmtId>>::default();
        for (idx, local_data) in body.locals.iter().enumerate() {
            let local = SLocalId::new(idx);
            let demand = local_data.facts.root_demand;
            let storage_can_escape = demand.borrowed_or_addr_taken
                || demand.mut_borrowed_or_addr_taken
                || demand.passed_by_place;
            let is_self_rooted = local_data.lowering.root().is_some_and(|root| {
                matches!(
                    body.root(root),
                    Some(NBorrowRoot::LocalSlot { local: root_local }) if *root_local == local
                )
            });
            if !storage_can_escape || !is_self_rooted {
                continue;
            }
            let definitions = facts
                .defs_by_local(local)
                .iter()
                .copied()
                .filter(|assignment| assignment_is_reachable(*assignment));
            let allocation_definitions = if local_data.source.is_some() {
                definitions.take(1).collect::<Vec<_>>()
            } else {
                definitions.collect()
            };
            for definition in allocation_definitions {
                let Some(assignment) = facts.assignment(definition) else {
                    continue;
                };
                let stmt = body.blocks[assignment.block.index()].stmts[assignment.stmt_idx].id;
                fresh_local_allocations.insert((stmt, local));
                fresh_local_sites.entry(local).or_default().push(stmt);
            }
        }
        let index_facts = reachable_index_facts(db, &body, &cfg_successors);
        let mut checker = Self {
            db,
            instance,
            hir_body: owner.body(db),
            body,
            facts,
            summary_mode,
            cfg_successors,
            param_modes,
            param_index_of_local,
            loan_for_local: FxHashMap::default(),
            param_loan_for_local: FxHashMap::default(),
            loans: Vec::new(),
            entry_state: SecondaryMap::new(),
            call_result_loans: FxHashMap::default(),
            call_loan_transforms: FxHashMap::default(),
            fresh_call_args,
            fresh_local_allocations,
            fresh_local_sites,
            fresh_status_at_stmt: FxHashMap::default(),
            constant_indices: index_facts.constant_indices,
            index_value_identities: index_facts.value_identities,
            index_phi_edge_substitutions: index_facts.phi_edge_substitutions,
            moved_entry: SecondaryMap::new(),
            live_before: Vec::new(),
            live_before_term: SecondaryMap::new(),
        };
        let repeating_blocks = cfg_cycle_blocks(&checker.cfg_successor_indices());
        let repeating_stmts = checker
            .body
            .blocks
            .iter()
            .enumerate()
            .filter(|(idx, _)| repeating_blocks.contains(&SBlockId::new(*idx)))
            .flat_map(|(_, block)| block.stmts.iter().map(|stmt| stmt.id))
            .collect::<FxHashSet<_>>();
        checker
            .fresh_local_allocations
            .retain(|(stmt, _)| repeating_stmts.contains(stmt));
        for sites in checker.fresh_local_sites.values_mut() {
            sites.retain(|stmt| repeating_stmts.contains(stmt));
        }
        checker
            .fresh_local_sites
            .retain(|_, sites| !sites.is_empty());
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
            &self.index_value_identities,
        )
    }

    fn borrow_summary(mut self) -> Result<Option<BorrowSummary>, SemanticBorrowDiagnostic<'db>> {
        let key = self.instance.key(self.db);
        if return_borrow_results_in_ty(self.db, key.callable_body(self.db).result_ty(self.db))
            .is_empty()
            || key.owner(self.db).body(self.db).is_none()
        {
            return Ok(None);
        }
        self.compute_entry_states();
        self.compute_loan_targets()?;
        self.check_provider_provenance()?;
        self.compute_return_summary().map(Some)
    }

    fn check(mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        check_definite_initialization(&self)?;
        self.compute_entry_states();
        self.compute_loan_targets()?;
        self.compute_fresh_value_roots()?;
        self.check_provider_provenance()?;
        self.compute_moved_states()?;
        self.compute_liveness();
        self.check_conflicts()?;
        let key = self.instance.key(self.db);
        if !return_borrow_results_in_ty(self.db, key.callable_body(self.db).result_ty(self.db))
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
            if let Some((kind, _)) = ty_is_borrow(self.db, local.ty)
                && let Some(&param_idx) = self.param_index_of_local.get(&local_id)
                && (!matches!(
                    local.lowering,
                    NormalizedBindingLowering::CarrierLocal { .. }
                ) || self
                    .param_modes
                    .get(param_idx as usize)
                    .is_some_and(|mode| *mode != FuncParamMode::Own)
                    && !matches!(
                        local.source,
                        Some(LocalBinding::Param {
                            site: ParamSite::ClosureEnv(_),
                            ..
                        })
                    ))
            {
                let kind = if matches!(local.source, Some(LocalBinding::Param { is_mut: true, .. }))
                {
                    BorrowKind::Mut
                } else {
                    kind
                };
                let mut targets = FxHashSet::default();
                targets.insert(CanonPlace {
                    root: BorrowRoot::Param(param_idx),
                    proj: CanonProjectionPath::default(),
                });
                let loan = self.allocate_loan(Loan {
                    kind,
                    activation: BorrowActivation::Immediate,
                    unconditional_targets: targets.clone(),
                    targets,
                    indexed_targets: Vec::new(),
                    result_exclusions: Vec::new(),
                    parents: FxHashSet::default(),
                    origin: crate::analysis::semantic::SemOrigin::Body(self.body.template_owner),
                });
                self.param_loan_for_local.insert(local_id, loan);
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
        let mut analysis = BorrowLoanTargetAnalysis::new(BorrowLoanTargetInputs {
            db: self.db,
            body: &self.body,
            entry_state: &self.entry_state,
            loan_for_local: &self.loan_for_local,
            constant_indices: &self.constant_indices,
            index_value_identities: &self.index_value_identities,
            call_result_loans: &self.call_result_loans,
            call_loan_transforms: &self.call_loan_transforms,
            fresh_call_args: &self.fresh_call_args,
        });
        let mut state = BorrowLoanTargetState {
            loans: &mut self.loans,
        };
        try_solve_sparse(&mut analysis, &mut state)
    }

    fn compute_fresh_value_roots(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let successors = self.cfg_successor_indices();
        let predecessors = self.cfg_predecessor_indices();
        let backedges = cfg_backedges(&successors);

        let mut entry = vec![FreshLocalStatus::default(); self.body.blocks.len()];
        let mut exit = entry.clone();
        loop {
            let mut changed = false;
            for block_idx in 0..self.body.blocks.len() {
                let block = SBlockId::new(block_idx);
                if !self.entry_state[block].is_reachable() {
                    continue;
                }
                let mut ignored = FreshStatusAtStmt::default();
                let new_exit =
                    self.transfer_fresh_value_roots(block, &entry[block_idx], &mut ignored)?;
                if new_exit != exit[block_idx] {
                    exit[block_idx] = new_exit;
                    changed = true;
                }
            }
            for (block_idx, block_entry) in entry.iter_mut().enumerate().skip(1) {
                let block = SBlockId::new(block_idx);
                if !self.entry_state[block].is_reachable() {
                    continue;
                }
                let reachable_pred_count = predecessors[block]
                    .iter()
                    .filter(|pred| self.entry_state[**pred].is_reachable())
                    .count();
                let mut candidates = FreshLocalStatus::default();
                let mut predecessor_counts = FxHashMap::<SLocalId, usize>::default();
                for pred in predecessors[block]
                    .iter()
                    .copied()
                    .filter(|pred| self.entry_state[*pred].is_reachable())
                {
                    let crosses_backedge = backedges.contains(&(pred, block));
                    for (&local, status) in &exit[pred.index()] {
                        *predecessor_counts.entry(local).or_default() += 1;
                        let candidate =
                            candidates.entry(local).or_insert_with(|| FreshValueStatus {
                                roots: FreshRootStates::default(),
                                complete: true,
                            });
                        candidate.complete &= status.complete;
                        for (&root, root_statuses) in &status.roots {
                            for root_status in root_statuses {
                                let mut root_status = root_status.clone();
                                if crosses_backedge {
                                    root_status.generation = FreshGeneration::Stale;
                                }
                                merge_fresh_root_status(&mut candidate.roots, root, root_status);
                            }
                        }
                    }
                }
                for (&local, status) in &mut candidates {
                    status.complete &=
                        predecessor_counts.get(&local) == Some(&reachable_pred_count);
                }
                if candidates != *block_entry {
                    *block_entry = candidates;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        let mut status_at_stmt = FreshStatusAtStmt::default();
        for (block_idx, block_entry) in entry.iter().enumerate() {
            if !self.entry_state[SBlockId::new(block_idx)].is_reachable() {
                continue;
            }
            self.transfer_fresh_value_roots(
                SBlockId::new(block_idx),
                block_entry,
                &mut status_at_stmt,
            )?;
        }
        self.fresh_status_at_stmt = status_at_stmt;
        Ok(())
    }

    fn transfer_fresh_value_roots(
        &self,
        block: SBlockId,
        entry: &FreshLocalStatus<'db>,
        status_at_stmt: &mut FreshStatusAtStmt<'db>,
    ) -> Result<FreshLocalStatus<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut current = entry.clone();
        let mut state = self.entry_state[block].clone();
        for stmt in &self.body.blocks[block.index()].stmts {
            let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                let NSStmtKind::Store { dst, src } = &stmt.kind else {
                    unreachable!()
                };
                if let Some(status) = current.get(&src.local) {
                    status_at_stmt.insert((stmt.id, src.local), status.clone());
                }
                if let NSPlaceRoot::CarrierDerefLocal(local) = dst.root
                    && let Some(status) = current.get(&local)
                {
                    status_at_stmt.insert((stmt.id, local), status.clone());
                }
                let Some(base) = dst
                    .root
                    .borrow_root()
                    .and_then(|root| self.canon().root_base_local(root))
                else {
                    self.apply_stmt_state(&mut state, stmt);
                    continue;
                };
                let Some(projection) = self.canon().layout_path(&dst.path) else {
                    self.apply_stmt_state(&mut state, stmt);
                    current.remove(&base);
                    continue;
                };
                if self
                    .body
                    .place_root_ty(&dst.root)
                    .and_then(|ty| semantic_projection_ty(self.db, ty, &dst.path))
                    .is_some_and(|(_, traverses_capability)| traverses_capability)
                    && !super::ir::store_rebinds_capability(self.db, &self.body, dst, *src)
                {
                    self.apply_stmt_state(&mut state, stmt);
                    continue;
                }

                let source = current
                    .get(&src.local)
                    .cloned()
                    .unwrap_or_else(|| self.unknown_fresh_status(&state, src.local));
                self.mark_fresh_retention_site(&mut current, &source, stmt.id);
                let replacement = current
                    .get(&src.local)
                    .cloned()
                    .unwrap_or_else(|| self.unknown_fresh_status(&state, src.local));
                let mut status = current
                    .remove(&base)
                    .unwrap_or_else(|| self.unknown_fresh_status(&state, base));
                replace_fresh_value_projection(&mut status, &projection, replacement);
                self.apply_stmt_state(&mut state, stmt);
                self.filter_fresh_status(&state, base, &mut status);
                if !status.roots.is_empty() {
                    current.insert(base, status);
                }
                continue;
            };
            expr.for_each_value_operand(|value| {
                if let Some(status) = current.get(&value.local) {
                    status_at_stmt.insert((stmt.id, value.local), status.clone());
                }
            });
            let retains_operands = matches!(
                expr,
                NExpr::AggregateMake { .. } | NExpr::EnumMake { .. } | NExpr::ArrayRepeat { .. }
            ) || self
                .body
                .local(*dst)
                .is_some_and(|local| local.source.is_some());
            if retains_operands {
                let mut operands = Vec::new();
                expr.for_each_value_operand(|value| operands.push(value.local));
                for local in operands {
                    let source = current
                        .get(&local)
                        .cloned()
                        .unwrap_or_else(|| self.unknown_fresh_status(&state, local));
                    self.mark_fresh_retention_site(&mut current, &source, stmt.id);
                }
            }
            let previous_dst_status = current.get(dst).cloned();

            let mut status = match expr {
                NExpr::Use(value) | NExpr::Cast { value, .. } => {
                    current.get(&value.local).cloned().unwrap_or_default()
                }
                NExpr::ArrayRepeat { value, .. } => prefix_fresh_value_status(
                    current.get(&value.local).cloned().unwrap_or_default(),
                    &[LayoutBackingProjection::Index(None)],
                ),
                NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                } => current
                    .get(&value.local)
                    .map(|status| {
                        project_fresh_value_status(
                            status,
                            &[LayoutBackingProjection::VariantField {
                                variant: *variant,
                                field: *field,
                            }],
                        )
                    })
                    .unwrap_or_default(),
                NExpr::ReadPlace { place, .. } | NExpr::Borrow { place, .. } => {
                    let base = self.canon().place_base_local(place);
                    let projection = self.canon().layout_path(&place.path).unwrap_or_default();
                    let mut status = base
                        .and_then(|local| current.get(&local))
                        .map(|status| project_fresh_value_status(status, &projection))
                        .unwrap_or_default();
                    if status.roots.is_empty()
                        && let Some(local) = base.and_then(|local| self.body.local(local))
                    {
                        for source in local.layout_backing_sources() {
                            let Some(source_status) = self
                                .canon()
                                .place_base_local(&source.source)
                                .and_then(|local| current.get(&local))
                            else {
                                continue;
                            };
                            merge_fresh_value_status(&mut status, source_status.clone());
                        }
                    }
                    status
                }
                NExpr::AggregateMake { ty, fields } => {
                    let mut status = FreshValueStatus {
                        roots: FreshRootStates::default(),
                        complete: true,
                    };
                    for (idx, field) in fields.iter().enumerate() {
                        if let Some(field_status) = current.get(&field.local) {
                            let projection = if ty.is_array(self.db) {
                                LayoutBackingProjection::Index(Some(idx))
                            } else {
                                let Ok(field) = u16::try_from(idx) else {
                                    status.complete = false;
                                    continue;
                                };
                                LayoutBackingProjection::Field(FieldIndex(field))
                            };
                            merge_fresh_value_status(
                                &mut status,
                                prefix_fresh_value_status(field_status.clone(), &[projection]),
                            );
                        } else if !self
                            .fresh_storage_roots_for_value(&state, field.local)
                            .is_empty()
                        {
                            status.complete = false;
                        }
                    }
                    status
                }
                NExpr::EnumMake {
                    variant, fields, ..
                } => {
                    let mut status = FreshValueStatus {
                        roots: FreshRootStates::default(),
                        complete: true,
                    };
                    for (idx, field) in fields.iter().enumerate() {
                        if let Some(field_status) = current.get(&field.local) {
                            let Ok(field) = u16::try_from(idx) else {
                                status.complete = false;
                                continue;
                            };
                            merge_fresh_value_status(
                                &mut status,
                                prefix_fresh_value_status(
                                    field_status.clone(),
                                    &[LayoutBackingProjection::VariantField {
                                        variant: *variant,
                                        field: FieldIndex(field),
                                    }],
                                ),
                            );
                        } else if !self
                            .fresh_storage_roots_for_value(&state, field.local)
                            .is_empty()
                        {
                            status.complete = false;
                        }
                    }
                    status
                }
                NExpr::Call { args, .. } => {
                    let direct = self
                        .loan_for_local
                        .get(dst)
                        .copied()
                        .map(|loan| (Vec::new(), loan))
                        .into_iter();
                    let aggregate = self
                        .call_result_loans
                        .get(&stmt.id)
                        .into_iter()
                        .flatten()
                        .map(|(result, loan)| (result.projection.clone(), *loan));
                    let mut status = FreshValueStatus {
                        roots: FreshRootStates::default(),
                        complete: true,
                    };
                    for (result_projection, loan_id) in direct.chain(aggregate) {
                        let Some(transforms) = self.call_loan_transforms.get(&loan_id) else {
                            status.complete = false;
                            continue;
                        };
                        for transform in transforms {
                            let Some(arg) = args.get(transform.input.param() as usize) else {
                                status.complete = false;
                                continue;
                            };
                            let targets = self.canon().canonicalize_call_input(
                                &state,
                                stmt.id,
                                arg.local,
                                &transform.input,
                                self.fresh_call_args.contains(&(stmt.id, arg.local)),
                            );
                            for target in targets {
                                let arg_status = match &transform.input {
                                    BorrowInput::Place { projection, .. } => current
                                        .get(&arg.local)
                                        .map(|status| {
                                            project_fresh_value_status(status, projection)
                                        })
                                        .unwrap_or_else(|| {
                                            self.unknown_fresh_status(&state, arg.local)
                                        }),
                                    BorrowInput::AnyInParam(_) => {
                                        current.get(&arg.local).cloned().unwrap_or_else(|| {
                                            self.unknown_fresh_status(&state, arg.local)
                                        })
                                    }
                                };
                                let fresh_call_root = FreshStorageRoot::Call {
                                    stmt: stmt.id,
                                    source: arg.local,
                                };
                                if matches!(
                                    target.root,
                                    BorrowRoot::FreshCall {
                                        stmt: root_stmt,
                                        source,
                                    } if root_stmt == stmt.id && source == arg.local
                                ) {
                                    merge_fresh_root_status(
                                        &mut status.roots,
                                        fresh_call_root,
                                        FreshRootStatus {
                                            generation: FreshGeneration::Current,
                                            retention_sites: FxHashSet::default(),
                                            carriers: FxHashSet::from_iter([
                                                result_projection.clone()
                                            ]),
                                            claimed: FxHashSet::default(),
                                        },
                                    );
                                    continue;
                                }

                                let mut found = false;
                                for (&root, root_statuses) in &arg_status.roots {
                                    if !fresh_storage_root_matches(&root, &target.root) {
                                        continue;
                                    }
                                    found = true;
                                    for root_status in root_statuses {
                                        merge_fresh_root_status(
                                            &mut status.roots,
                                            root,
                                            prefix_fresh_root_status(
                                                root_status.clone(),
                                                &result_projection,
                                            ),
                                        );
                                    }
                                }
                                if !found && !arg_status.complete {
                                    status.complete = false;
                                }
                            }
                        }
                    }
                    status
                }
                NExpr::CodeRegionRef { .. }
                | NExpr::Const(_)
                | NExpr::Unary { .. }
                | NExpr::Binary { .. }
                | NExpr::GetEnumTag { .. }
                | NExpr::IsEnumVariant { .. }
                | NExpr::CodeRegionOffset { .. }
                | NExpr::CodeRegionLen { .. } => FreshValueStatus {
                    roots: FreshRootStates::default(),
                    complete: true,
                },
            };
            if self.fresh_local_allocations.contains(&(stmt.id, *dst)) {
                merge_fresh_root_status(
                    &mut status.roots,
                    FreshStorageRoot::Local {
                        definition: stmt.id,
                        local: *dst,
                    },
                    FreshRootStatus {
                        generation: FreshGeneration::Current,
                        retention_sites: FxHashSet::default(),
                        carriers: FxHashSet::from_iter([FreshCarrierPath::new()]),
                        claimed: FxHashSet::default(),
                    },
                );
            } else if self.fresh_local_sites.contains_key(dst)
                && let Some(previous_dst_status) = previous_dst_status
            {
                for (root, root_statuses) in previous_dst_status.roots {
                    if matches!(
                        root,
                        FreshStorageRoot::Local {
                            local: root_local,
                            ..
                        } if root_local == *dst
                    ) {
                        for root_status in root_statuses {
                            merge_fresh_root_status(&mut status.roots, root, root_status);
                        }
                    }
                }
            }
            current.remove(dst);
            self.apply_stmt_state(&mut state, stmt);
            self.filter_fresh_status(&state, *dst, &mut status);
            status_at_stmt.insert((stmt.id, *dst), status.clone());
            let direct = self.loan_for_local.get(dst).copied().into_iter();
            let aggregate = self
                .call_result_loans
                .get(&stmt.id)
                .into_iter()
                .flatten()
                .map(|(_, loan)| *loan);
            let claimed_targets = direct
                .chain(aggregate)
                .flat_map(|loan| self.loans[loan.0 as usize].targets.iter())
                .collect::<Vec<_>>();
            for value_status in current.values_mut() {
                claim_fresh_value_status(value_status, &status, &claimed_targets);
            }
            if !status.roots.is_empty() {
                current.insert(*dst, status);
            }
        }
        Ok(current)
    }

    fn unknown_fresh_status(&self, state: &State, local: SLocalId) -> FreshValueStatus<'db> {
        FreshValueStatus {
            roots: FreshRootStates::default(),
            complete: self.fresh_storage_roots_for_value(state, local).is_empty(),
        }
    }

    fn filter_fresh_status(
        &self,
        state: &State,
        local: SLocalId,
        status: &mut FreshValueStatus<'db>,
    ) {
        let possible_roots = self.fresh_storage_roots_for_value(state, local);
        status
            .roots
            .retain(|root, statuses| possible_roots.contains(root) && !statuses.is_empty());
        if possible_roots
            .iter()
            .all(|root| status.roots.contains_key(root))
        {
            status.complete = true;
        }
    }

    fn mark_fresh_retention_site(
        &self,
        current: &mut FreshLocalStatus<'db>,
        source: &FreshValueStatus<'db>,
        stmt: SStmtId,
    ) {
        for value_status in current.values_mut() {
            for (&root, source_statuses) in &source.roots {
                let Some(statuses) = value_status.roots.get_mut(&root) else {
                    continue;
                };
                for status in &mut *statuses {
                    if status.generation == FreshGeneration::Current
                        && source_statuses.iter().any(|source_status| {
                            fresh_root_statuses_may_alias(status, source_status)
                        })
                    {
                        // Rename every alias of the current allocation
                        // together. Sequential copies therefore keep one
                        // identity, while mutually exclusive retention sites
                        // can distinguish values retained by later loop
                        // iterations.
                        status.retention_sites.clear();
                        status.retention_sites.insert(stmt);
                    }
                }
                normalize_fresh_root_statuses(statuses);
            }
        }
    }

    fn fresh_storage_roots_for_value(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<FreshStorageRoot> {
        let mut roots = FxHashSet::default();
        for target in self.canon().all_value_targets(state, local) {
            self.extend_fresh_storage_roots(&mut roots, &target.root);
        }
        roots
    }

    fn extend_fresh_storage_roots(
        &self,
        roots: &mut FxHashSet<FreshStorageRoot>,
        root: &BorrowRoot<'db>,
    ) {
        match root {
            BorrowRoot::FreshCall { stmt, source } => {
                roots.insert(FreshStorageRoot::Call {
                    stmt: *stmt,
                    source: *source,
                });
            }
            BorrowRoot::Local(local) => {
                roots.extend(self.fresh_local_sites.get(local).into_iter().flatten().map(
                    |definition| FreshStorageRoot::Local {
                        definition: *definition,
                        local: *local,
                    },
                ));
            }
            BorrowRoot::Param(_) | BorrowRoot::Provider(_) => {}
        }
    }

    pub(super) fn apply_stmt_state(&self, state: &mut State, stmt: &super::ir::NSStmt<'db>) {
        self.canon().apply_stmt_state_with_call_loans(
            state,
            stmt,
            self.call_result_loans.get(&stmt.id).map(Vec::as_slice),
        );
    }

    fn check_provider_provenance(&self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut observed = FxHashMap::<
            (
                crate::analysis::semantic::SLocalId,
                Vec<LayoutBackingProjection>,
            ),
            (ProviderAddressSpace, SemOrigin<'db>),
        >::default();
        for (bb_idx, block) in self.body.blocks.iter().enumerate() {
            let mut state = self.entry_state[SBlockId::new(bb_idx)].clone();
            if !state.is_reachable() {
                continue;
            }
            self.observe_provider_slots(&state, &mut observed)?;
            for stmt in &block.stmts {
                self.apply_stmt_state(&mut state, stmt);
                self.observe_provider_slots(&state, &mut observed)?;
            }
        }
        Ok(())
    }

    fn observe_provider_slots(
        &self,
        state: &State,
        observed: &mut FxHashMap<
            (
                crate::analysis::semantic::SLocalId,
                Vec<LayoutBackingProjection>,
            ),
            (ProviderAddressSpace, SemOrigin<'db>),
        >,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for (&local, held) in &state.local_loans {
            for (path, loans) in held {
                let slot = path
                    .iter()
                    .copied()
                    .map(|projection| match projection {
                        LayoutBackingProjection::Index(_)
                        | LayoutBackingProjection::IndexFamily(_) => {
                            LayoutBackingProjection::Index(None)
                        }
                        projection => projection,
                    })
                    .collect::<Vec<_>>();
                let key = (local, slot);
                for loan in loans {
                    let origin = self.loan_origin(loan.id);
                    for target in &self.loans[loan.id.0 as usize].targets {
                        let Some(space) = known_address_space_for_borrow_root(&target.root) else {
                            continue;
                        };
                        let Some((previous_space, previous_origin)) = observed.get(&key) else {
                            observed.insert(key.clone(), (space, origin));
                            continue;
                        };
                        if *previous_space == space {
                            continue;
                        }
                        let mut spaces = [*previous_space, space];
                        spaces.sort_by_key(|space| address_space_rank(*space));
                        let mut diag = self.diag(
                            SemanticBorrowDiagKind::ProviderProvenanceConflict,
                            origin,
                            format!(
                                "borrow slot may come from multiple address spaces: {}, {}",
                                spaces[0].pretty(),
                                spaces[1].pretty(),
                            ),
                        );
                        self.push_secondary_origin(
                            &mut diag,
                            *previous_origin,
                            format!(
                                "the same borrow slot is {}-backed here",
                                previous_space.pretty()
                            ),
                        );
                        return Err(diag);
                    }
                }
            }
        }
        Ok(())
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
                let live_after = self.live_before[bb_idx]
                    .get(stmt_idx + 1)
                    .unwrap_or(&self.live_before_term[bb]);
                self.check_stmt(
                    &state,
                    &moved,
                    &self.live_before[bb_idx][stmt_idx],
                    live_after,
                    stmt,
                )?;
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
        live_before: &FxHashSet<crate::analysis::semantic::SLocalId>,
        live_after: &FxHashSet<crate::analysis::semantic::SLocalId>,
        stmt: &super::ir::NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let active = self.effective_loans(state, live_before);
        let mut state_after = state.clone();
        self.apply_stmt_state(&mut state_after, stmt);
        let mut live_at_write = live_after.clone();
        match &stmt.kind {
            NSStmtKind::Assign { dst, .. } => {
                live_at_write.insert(*dst);
            }
            NSStmtKind::Store { dst, .. } => {
                if let Some(base) = self.canon().place_base_local(dst) {
                    live_at_write.insert(base);
                }
            }
        }
        let active_at_write = self.effective_loans(&state_after, &live_at_write);
        let check = ConflictCheckCx {
            state,
            moved,
            active: &active,
        };
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                match expr {
                    NExpr::ReadPlace { place, mode } => {
                        self.check_place_read(
                            check,
                            place,
                            *mode,
                            stmt.origin,
                            self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                        )?;
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
                            self.check_loan_conflict(
                                &active,
                                loan,
                                kind,
                                &targets,
                                stmt.origin,
                                self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                            )?;
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
                                &targets,
                                stmt.origin,
                                self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                            )?;
                        } else {
                            self.check_read_targets(
                                &active,
                                &authorized,
                                &targets,
                                stmt.origin,
                                self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                            )?;
                        }
                    }
                    _ => {
                        let expression_moved = self.check_expr_operands(
                            state,
                            moved,
                            &active,
                            stmt.id,
                            stmt.origin,
                            expr,
                        )?;
                        let mut call_accesses = self.check_call_argument_accesses(
                            state,
                            &active,
                            stmt.id,
                            stmt.origin,
                            expr,
                        )?;
                        self.check_effect_place_accesses(
                            ConflictCheckCx {
                                moved: &expression_moved,
                                ..check
                            },
                            stmt.id,
                            stmt.origin,
                            expr,
                            &mut call_accesses,
                        )?;
                    }
                }
                if matches!(expr, NExpr::Call { .. } | NExpr::Use(_)) {
                    self.check_assigned_loan_conflicts(
                        &active,
                        stmt.id,
                        *dst,
                        stmt.origin,
                        self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                    )?;
                }
                self.check_assignment_write(
                    state,
                    &active_at_write,
                    *dst,
                    stmt.origin,
                    self.fresh_status_at_stmt.get(&(stmt.id, *dst)),
                )?;
            }
            NSStmtKind::Store { dst, src } => {
                self.check_operand(
                    check,
                    *src,
                    Some(stmt.id),
                    stmt.origin,
                    "cannot use a value after it was moved",
                )?;
                let targets = self.canon().canonicalize_place(state, dst, stmt.origin)?;
                let authorized = self
                    .canon()
                    .mut_loans_for_place_targets(state, dst, &targets);
                self.check_moved_parent(moved, &targets, &authorized, stmt.origin)?;
                let fresh_status = match dst.root {
                    NSPlaceRoot::CarrierDerefLocal(local) => {
                        self.fresh_status_at_stmt.get(&(stmt.id, local))
                    }
                    NSPlaceRoot::Root(_) => None,
                };
                self.check_write_targets(
                    &active_at_write,
                    &authorized,
                    &targets,
                    stmt.origin,
                    fresh_status,
                )?;
            }
        }
        Ok(())
    }

    fn check_assignment_write(
        &self,
        state: &State,
        active: &[LoanId],
        dst: crate::analysis::semantic::SLocalId,
        origin: SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(local) = self
            .body
            .local(dst)
            .filter(|local| local.source.is_some_and(|binding| binding.is_mut()))
        else {
            return Ok(());
        };
        if local.is_derived_place_bound_alias() {
            return Ok(());
        }
        let Some(place) = local.lowering.place() else {
            return Ok(());
        };
        let targets = self.canon().canonicalize_place(state, place, origin)?;
        self.check_write_targets(
            active,
            &FxHashSet::default(),
            &targets,
            origin,
            fresh_status,
        )
    }

    fn check_place_read(
        &self,
        check: ConflictCheckCx<'_, 'db>,
        place: &NSPlace<'db>,
        mode: ReadMode,
        origin: SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let targets = self
            .canon()
            .canonicalize_place(check.state, place, origin)?;
        let authorized = self.canon().loans_for_place(check.state, place);
        self.check_moved_overlap(
            check.moved,
            &targets,
            &authorized,
            origin,
            "cannot use a value after it was moved",
        )?;
        if mode == ReadMode::Move {
            self.check_move_out(check.active, place, &targets, origin, fresh_status)
        } else {
            self.check_read_targets(check.active, &authorized, &targets, origin, fresh_status)
        }
    }

    fn check_effect_place_accesses(
        &self,
        check: ConflictCheckCx<'_, 'db>,
        stmt: SStmtId,
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
                    let targets = self
                        .canon()
                        .canonicalize_place(check.state, place, origin)?;
                    let authorized = if effect_arg.required_mut {
                        self.canon().mut_loans_for_place(check.state, place)
                    } else {
                        self.canon().loans_for_place(check.state, place)
                    };
                    self.check_moved_overlap(
                        check.moved,
                        &targets,
                        &authorized,
                        origin,
                        "cannot use a value after it was moved",
                    )?;
                    (targets, authorized, origin)
                }
                NEffectArgValue::Value(value) => {
                    let targets = self
                        .canon()
                        .canonicalize_value_base(check.state, value.local);
                    let authorized = if effect_arg.required_mut {
                        self.canon()
                            .mut_loans_for_value_targets(check.state, value.local, &targets)
                    } else {
                        self.canon()
                            .loans_for_value_targets(check.state, value.local, &targets)
                    };
                    (targets, authorized, operand_origin(*value, origin))
                }
            };
            let fresh_status = match &effect_arg.arg {
                NEffectArgValue::Value(value) => {
                    self.fresh_status_at_stmt.get(&(stmt, value.local))
                }
                NEffectArgValue::Place(_) => None,
            };
            if effect_arg.required_mut {
                self.check_write_targets(
                    check.active,
                    &authorized,
                    &targets,
                    arg_origin,
                    fresh_status,
                )?;
                self.record_call_access(
                    accesses,
                    group,
                    CallAccessRole::Container,
                    BorrowKind::Mut,
                    targets,
                    arg_origin,
                )?;
            } else {
                self.check_read_targets(
                    check.active,
                    &authorized,
                    &targets,
                    arg_origin,
                    fresh_status,
                )?;
                self.record_call_access(
                    accesses,
                    group,
                    CallAccessRole::Container,
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
                            check.state,
                            place,
                            target_ty,
                            &result.projection,
                        )
                    }
                    NEffectArgValue::Value(value) => {
                        self.canon().canonicalize_value_layout_projection(
                            check.state,
                            value.local,
                            &result.projection,
                        )
                    }
                };
                let authorized = match &effect_arg.arg {
                    NEffectArgValue::Place(place) if result.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_loans_for_place_targets(check.state, place, &targets),
                    NEffectArgValue::Place(place) => {
                        self.canon()
                            .loans_for_place_targets(check.state, place, &targets)
                    }
                    NEffectArgValue::Value(value) if result.kind == BorrowKind::Mut => self
                        .canon()
                        .mut_loans_for_value_targets(check.state, value.local, &targets),
                    NEffectArgValue::Value(value) => {
                        self.canon()
                            .loans_for_value_targets(check.state, value.local, &targets)
                    }
                };
                if result.kind == BorrowKind::Mut {
                    self.check_write_targets(
                        check.active,
                        &authorized,
                        &targets,
                        arg_origin,
                        fresh_status,
                    )?;
                } else {
                    self.check_read_targets(
                        check.active,
                        &authorized,
                        &targets,
                        arg_origin,
                        fresh_status,
                    )?;
                }
                self.record_call_access(
                    accesses,
                    group,
                    CallAccessRole::BorrowSlot,
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
        active: &[LoanId],
        stmt: SStmtId,
        origin: SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<Vec<CallAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let NExpr::Call { callee, args, .. } = expr else {
            return Ok(Vec::new());
        };
        let instance = get_or_build_semantic_instance(self.db, callee.key);
        let callable = callee.key.callable_body(self.db);
        let mut accesses = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().copied().enumerate() {
            let Some(binding @ LocalBinding::Param { mode, .. }) =
                callable.param_binding(self.db, idx)
            else {
                return Err(
                    self.internal_diag(origin, format!("callee is missing value parameter {idx}"))
                );
            };
            let ty = instance.normalized_binding_ty(self.db, binding);
            let moves_value =
                arg.mode == ReadMode::Move && self.local_has_runtime_move_semantics(arg.local);
            let arg_origin = operand_origin(arg, origin);
            let fresh_status = self.fresh_status_at_stmt.get(&(stmt, arg.local));
            let mutably_passed_by_place = mode != FuncParamMode::Own && binding.is_mut();
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
                    self.check_write_targets(
                        active,
                        &authorized,
                        &targets,
                        arg_origin,
                        fresh_status,
                    )?;
                }
                self.record_call_access(
                    &mut accesses,
                    idx,
                    CallAccessRole::Container,
                    kind,
                    targets,
                    arg_origin,
                )?;
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
                    self.check_write_targets(
                        active,
                        &authorized,
                        &targets,
                        arg_origin,
                        fresh_status,
                    )?;
                } else {
                    self.check_read_targets(
                        active,
                        &authorized,
                        &targets,
                        arg_origin,
                        fresh_status,
                    )?;
                }
                self.record_call_access(
                    &mut accesses,
                    idx,
                    CallAccessRole::BorrowSlot,
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
        role: CallAccessRole,
        kind: BorrowKind,
        targets: FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        // An argument owns its container and the capability slots stored inside
        // it, but distinct capability slots must still obey aliasing rules.
        let conflict = accesses.iter().find(|access| {
            (access.group != group
                || access.role == CallAccessRole::BorrowSlot && role == CallAccessRole::BorrowSlot)
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
                role,
                kind,
                targets,
                origin,
            });
        }
        Ok(())
    }

    fn check_assigned_loan_conflicts(
        &self,
        active: &[LoanId],
        stmt: SStmtId,
        local: crate::analysis::semantic::SLocalId,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
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
            self.check_loan_conflict(
                active,
                Some(loan_id),
                loan.kind,
                &loan.targets,
                origin,
                fresh_status,
            )?;
        }
        Ok(())
    }

    fn check_loan_conflict(
        &self,
        active: &[LoanId],
        new_loan: Option<LoanId>,
        kind: BorrowKind,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some(conflict) =
            self.first_loan_conflict(active, new_loan, kind, targets, fresh_status)
        {
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
        active: &[LoanId],
        authorized: &FxHashSet<LoanId>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().copied().find(|loan| {
            !authorized.contains(loan)
                && self.loan_conflict_kind(*loan) == Some(BorrowKind::Mut)
                && place_sets_conflict(
                    &self.loans[loan.0 as usize].targets,
                    targets,
                    fresh_status,
                    &self.fresh_local_sites,
                )
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot read this place while a mutable borrow is active".to_string(),
                conflict,
            ));
        }
        Ok(())
    }

    fn check_write_targets(
        &self,
        active: &[LoanId],
        authorized: &FxHashSet<LoanId>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let conflict = active.iter().copied().find(|loan| {
            !authorized.contains(loan)
                && !authorized
                    .iter()
                    .copied()
                    .any(|authorized| self.loans_share_reborrow_authority(authorized, *loan))
                && self.loan_conflict_kind(*loan).is_some()
                && place_sets_conflict(
                    &self.loans[loan.0 as usize].targets,
                    targets,
                    fresh_status,
                    &self.fresh_local_sites,
                )
        });
        if let Some(conflict) = conflict {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot write to this place while it is borrowed".to_string(),
                conflict,
            ));
        }
        Ok(())
    }

    /// Copies of one incoming mutable authority may be retained in multiple
    /// returned slots. Local liveness is aggregate-grained, so sibling slots
    /// remain active while either slot is used; treat those sibling reborrows
    /// as the same authority for an access through one of them. Independent
    /// borrows (with distinct roots) still conflict, as do simultaneous call
    /// argument accesses checked by `record_call_access`.
    fn loans_share_reborrow_authority(&self, lhs: LoanId, rhs: LoanId) -> bool {
        if self.loans[lhs.0 as usize].parents.is_empty()
            || self.loans[rhs.0 as usize].parents.is_empty()
        {
            return false;
        }
        let roots = |start: LoanId| {
            let mut pending = vec![start];
            let mut seen = FxHashSet::default();
            let mut roots = FxHashSet::default();
            while let Some(loan) = pending.pop() {
                if !seen.insert(loan) {
                    continue;
                }
                let parents = &self.loans[loan.0 as usize].parents;
                if parents.is_empty() {
                    roots.insert(loan);
                } else {
                    pending.extend(parents.iter().copied());
                }
            }
            roots
        };
        !roots(lhs).is_disjoint(&roots(rhs))
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
                    ConflictCheckCx {
                        state,
                        moved,
                        active: &active,
                    },
                    *cond,
                    None,
                    term.origin,
                    "cannot use a value after it was moved",
                )?;
            }
            NSTerminatorKind::MatchEnum { value, .. } => {
                let active = self.effective_loans(state, live);
                self.check_operand(
                    ConflictCheckCx {
                        state,
                        moved,
                        active: &active,
                    },
                    NOperand {
                        mode: ReadMode::Read,
                        ..*value
                    },
                    None,
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
            for loan_id in state.loans_in(value.local) {
                let loan = &self.loans[loan_id.0 as usize];
                let targets = self.resolve_return_targets(state, &loan.targets, term.origin)?;
                if let Some(target) = targets.iter().find(|target| {
                    matches!(
                        target.root,
                        BorrowRoot::Local(_) | BorrowRoot::FreshCall { .. }
                    )
                }) {
                    let message = match target.root {
                        BorrowRoot::Local(local) => {
                            let name = self.pretty_local_name(local);
                            format!("cannot return a value that holds a borrow of local `{name}`")
                        }
                        BorrowRoot::FreshCall { .. } => {
                            "cannot return a value that holds a borrow of a temporary created in this function"
                                .to_string()
                        }
                        BorrowRoot::Param(_) | BorrowRoot::Provider(_) => unreachable!(),
                    };
                    let mut diag = self.invalid_return_diag(term.origin, message);
                    self.push_secondary_origin(
                        &mut diag,
                        self.loan_origin(loan_id),
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
            let Some(source) = local.snapshot_source_place().cloned() else {
                resolved.insert(target);
                continue;
            };
            let mut advanced = false;
            for mut source in self.canon().canonicalize_place(state, &source, origin)? {
                source.proj = source.proj.concat(&target.proj);
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
            if !state.is_reachable() {
                continue;
            }
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
                                layout_path_for_canon_projection(&target.proj)
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
                        BorrowRoot::FreshCall { .. } => {
                            return Err(self.invalid_return_diag(
                                origin,
                                "cannot return a borrow to a temporary created in this function"
                                    .to_string(),
                            ));
                        }
                    }
                }
            }
        }
        out.extend(self.closure_return_source_borrow_summary(&materialized_results));
        out.sort_unstable();
        out.dedup();
        Ok(out)
    }

    fn closure_return_source_borrow_summary(
        &self,
        materialized_results: &[BorrowResult],
    ) -> BorrowSummary {
        let key = self.instance.key(self.db);
        if !matches!(key.owner(self.db), BodyOwner::Closure { .. }) {
            return Vec::new();
        }
        let callable_body = key.callable_body(self.db);
        if matches!(
            callable_body.return_provenance(self.db),
            ReturnProvenance::Unknown
        ) {
            return Vec::new();
        }

        let fallback_results = materialized_results.is_empty().then(|| {
            return_borrow_results_in_ty(self.db, self.instance.normalized_result_ty(self.db))
        });
        let results = fallback_results.as_deref().unwrap_or(materialized_results);
        let mut out = Vec::new();
        for source in callable_body.forwarded_return_sources(self.db) {
            let Some(input_ty) = callable_body
                .param_bindings(self.db)
                .into_iter()
                .find(|binding| binding.callable_input_origin(self.db) == Some(source.origin))
                .map(|binding| self.instance.normalized_binding_ty(self.db, binding))
            else {
                continue;
            };
            let param = match source.origin {
                CallableInputLayoutHoleOrigin::Receiver => 0,
                CallableInputLayoutHoleOrigin::ValueParam(param) => {
                    let Ok(param) = u32::try_from(param) else {
                        continue;
                    };
                    param
                }
                CallableInputLayoutHoleOrigin::Effect(_) => continue,
            };
            let input_prefix = return_projection_to_layout_path(&source.projection);
            for result in results {
                if !return_source_borrow_input_reaches_capability(
                    self.db,
                    input_ty,
                    &source,
                    &result.projection,
                )
                .unwrap_or(false)
                {
                    continue;
                }
                let Some(suffix) =
                    return_projection_result_suffix(&source.result_projection, &result.projection)
                else {
                    continue;
                };
                // Control-flow joins retain only facts common to every branch.
                // Reintroduce the closure's conservative may-sources for the
                // matching result slot so a fresh alternative cannot erase a
                // forwarded borrow.
                let mut input_projection = input_prefix.clone();
                input_projection.extend_from_slice(&suffix);
                out.push(BorrowTransform {
                    result: result.clone(),
                    input: BorrowInput::Place {
                        param,
                        projection: input_projection,
                    },
                });
            }
        }
        out
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
            .flat_map(|(_, held)| held.values())
            .flatten()
            .map(|loan| loan.id)
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
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Option<LoanId> {
        let reborrow_parents = new_loan.map(|loan| &self.loans[loan.0 as usize].parents);
        active
            .iter()
            .copied()
            .filter(|loan| reborrow_parents.is_none_or(|parents| !parents.contains(loan)))
            .find(|loan_id| {
                let loan = &self.loans[loan_id.0 as usize];
                self.loan_conflict_kind(*loan_id).is_some_and(|kind| {
                    !matches!((kind, new_kind), (BorrowKind::Ref, BorrowKind::Ref))
                }) && place_sets_conflict(
                    &loan.targets,
                    targets,
                    fresh_status,
                    &self.fresh_local_sites,
                )
            })
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
        active: &[LoanId],
        place: &NSPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let NSPlaceRoot::CarrierDerefLocal(local) = place.root {
            if let Some(message) = self
                .logical_closure_param_mode_for_local(local)
                .and_then(Self::logical_param_move_message)
            {
                return Err(self.move_conflict_diag(origin, message.to_string()));
            }
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
        self.check_move_targets_out(active, targets, origin, fresh_status)?;
        Ok(())
    }

    fn check_move_targets_out(
        &self,
        active: &[LoanId],
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        fresh_status: Option<&FreshValueStatus<'db>>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for target in targets {
            if let Some(message) = self
                .logical_closure_param_mode_for_target(target)
                .and_then(Self::logical_param_move_message)
            {
                return Err(self.move_conflict_diag(origin, message.to_string()));
            }
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
        if let Some(loan) = active.iter().copied().find(|loan| {
            place_sets_conflict(
                &self.loans[loan.0 as usize].targets,
                targets,
                fresh_status,
                &self.fresh_local_sites,
            )
        }) {
            return Err(self.borrow_conflict_diag(
                origin,
                "cannot move out of a value while it is borrowed".to_string(),
                loan,
            ));
        }
        Ok(())
    }

    fn logical_closure_param_mode_for_local(
        &self,
        local: crate::analysis::semantic::SLocalId,
    ) -> Option<ClosureParamMode> {
        let source = self
            .body
            .local(local)?
            .facts
            .snapshot_source_place
            .as_ref()?;
        let NSPlaceRoot::Root(root) = source.root else {
            return None;
        };
        let NBorrowRoot::Param { param_idx, .. } = self.body.root(root)? else {
            return None;
        };
        self.logical_closure_param_mode(*param_idx, &source.path)
    }

    fn logical_closure_param_mode_for_target(
        &self,
        target: &CanonPlace<'db>,
    ) -> Option<ClosureParamMode> {
        let BorrowRoot::Param(param_idx) = target.root else {
            return None;
        };
        self.logical_closure_param_mode(param_idx, &target.proj)
    }

    fn logical_closure_param_mode<Idx>(
        &self,
        param_idx: u32,
        projection: &ProjectionPath<TyId<'db>, crate::analysis::semantic::VariantIndex, Idx>,
    ) -> Option<ClosureParamMode> {
        let BodyOwner::Closure { ty, .. } = self.instance.key(self.db).owner(self.db) else {
            return None;
        };
        if usize::try_from(param_idx).ok()? != CLOSURE_ARGS_PARAM_IDX {
            return None;
        }
        let Projection::Field(field_idx) = projection.iter().next()? else {
            return None;
        };
        ty.param_modes(self.db).get(*field_idx).copied()
    }

    fn logical_param_move_message(mode: ClosureParamMode) -> Option<&'static str> {
        match mode {
            ClosureParamMode::Own => None,
            ClosureParamMode::View => Some("cannot move out of a view parameter"),
            ClosureParamMode::Ref | ClosureParamMode::Mut => {
                Some("cannot move out through a borrow handle")
            }
        }
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
        active: &[LoanId],
        stmt: SStmtId,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        expr: &NExpr<'db>,
    ) -> Result<MovedPlaces<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut moved = moved.clone();
        expr.try_for_each_value_operand(|value| {
            self.check_operand(
                ConflictCheckCx {
                    state,
                    moved: &moved,
                    active,
                },
                value,
                Some(stmt),
                origin,
                "cannot use a value after it was moved",
            )?;
            self.record_operand_move(state, &mut moved, value, origin)
        })?;
        Ok(moved)
    }

    fn check_operand(
        &self,
        check: ConflictCheckCx<'_, 'db>,
        operand: NOperand,
        stmt: Option<SStmtId>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        message: &str,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let origin = operand_origin(operand, origin);
        let targets = self
            .canon()
            .canonicalize_value_base(check.state, operand.local);
        if targets.is_empty() {
            return Ok(());
        }
        let authorized = self
            .canon()
            .loans_for_value_targets(check.state, operand.local, &targets);
        self.check_moved_overlap(check.moved, &targets, &authorized, origin, message)?;
        if operand.mode == ReadMode::Move && self.local_has_runtime_move_semantics(operand.local) {
            self.check_move_targets_out(
                check.active,
                &targets,
                origin,
                stmt.and_then(|stmt| self.fresh_status_at_stmt.get(&(stmt, operand.local))),
            )
        } else {
            self.check_read_targets(
                check.active,
                &authorized,
                &targets,
                origin,
                stmt.and_then(|stmt| self.fresh_status_at_stmt.get(&(stmt, operand.local))),
            )
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

    pub(super) fn cfg_successor_indices(&self) -> CfgAdjacency {
        self.cfg_successors.clone()
    }

    pub(super) fn constant_index(&self, local: SLocalId) -> Option<usize> {
        self.constant_indices[local]
    }

    pub(super) fn index_value_identity(&self, local: SLocalId) -> Option<SLocalId> {
        self.index_value_identities[local]
    }

    pub(super) fn index_phi_substitutions(
        &self,
        predecessor: SBlockId,
        block: SBlockId,
    ) -> Option<&IndexPhiEdgeSubstitution> {
        self.index_phi_edge_substitutions.get(&(predecessor, block))
    }

    pub(super) fn cfg_predecessor_indices(&self) -> CfgAdjacency {
        cfg_predecessor_indices(&self.cfg_successors)
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

fn place_sets_conflict<'db>(
    active: &FxHashSet<CanonPlace<'db>>,
    new: &FxHashSet<CanonPlace<'db>>,
    fresh_status: Option<&FreshValueStatus<'db>>,
    fresh_local_sites: &FxHashMap<SLocalId, Vec<SStmtId>>,
) -> bool {
    active.iter().any(|active_target| {
        new.iter().any(|new_target| {
            let current_root_conflicts = |status: &FreshRootStatus<'db>| {
                (status.generation == FreshGeneration::Stale && status.retention_sites.is_empty())
                    || status.claimed.iter().any(|claimed| {
                        places_overlap(
                            &CanonPlace {
                                root: new_target.root.clone(),
                                proj: claimed.clone(),
                            },
                            new_target,
                        )
                    })
            };
            let needs_conflict_check = match (&new_target.root, fresh_status) {
                (
                    BorrowRoot::FreshCall { stmt, source },
                    Some(FreshValueStatus { roots, complete }),
                ) => match roots.get(&FreshStorageRoot::Call {
                    stmt: *stmt,
                    source: *source,
                }) {
                    // The allocation site just executed, so active loans with
                    // the same static root refer to earlier dynamic instances
                    // unless this path has already loaned the same projection.
                    Some(statuses) => statuses.iter().any(current_root_conflicts),
                    // Canonical loan targets are path-unioned. A root omitted
                    // by a complete value state is unreachable on this path.
                    None => !complete,
                },
                (BorrowRoot::Local(local), Some(FreshValueStatus { roots, complete }))
                    if fresh_local_sites.contains_key(local) =>
                {
                    let mut statuses = roots.iter().filter_map(|(root, status)| {
                        matches!(
                            root,
                            FreshStorageRoot::Local {
                                local: root_local,
                                ..
                            } if root_local == local
                        )
                        .then_some(status.iter())
                    });
                    let first = statuses.next();
                    first.is_none() && !complete
                        || first
                            .into_iter()
                            .chain(statuses)
                            .flatten()
                            .any(current_root_conflicts)
                }
                _ => true,
            };
            needs_conflict_check && places_overlap(active_target, new_target)
        })
    })
}

fn fresh_storage_root_matches<'db>(root: &FreshStorageRoot, target: &BorrowRoot<'db>) -> bool {
    matches!(
        (root, target),
        (
            FreshStorageRoot::Call { stmt, source },
            BorrowRoot::FreshCall {
                stmt: target_stmt,
                source: target_source,
            },
        ) if stmt == target_stmt && source == target_source
    ) || matches!(
        (root, target),
        (
            FreshStorageRoot::Local { local, .. },
            BorrowRoot::Local(target_local),
        ) if local == target_local
    )
}

fn prefix_fresh_root_status<'db>(
    mut status: FreshRootStatus<'db>,
    prefix: &[LayoutBackingProjection],
) -> FreshRootStatus<'db> {
    status.carriers = status
        .carriers
        .iter()
        .map(|carrier| {
            let mut prefixed = prefix.to_vec();
            prefixed.extend_from_slice(carrier);
            prefixed
        })
        .collect();
    status
}

fn claim_fresh_value_status<'db>(
    candidate: &mut FreshValueStatus<'db>,
    source: &FreshValueStatus<'db>,
    targets: &[&CanonPlace<'db>],
) {
    for target in targets {
        let matching_source = source
            .roots
            .iter()
            .filter(|(root, _)| fresh_storage_root_matches(root, &target.root))
            .flat_map(|(_, statuses)| statuses)
            .collect::<Vec<_>>();
        for (root, statuses) in &mut candidate.roots {
            if !fresh_storage_root_matches(root, &target.root) {
                continue;
            }
            for status in &mut *statuses {
                if matching_source.is_empty() && !source.complete
                    || matching_source
                        .iter()
                        .any(|source| fresh_root_statuses_may_alias(status, source))
                {
                    status.claimed.insert(target.proj.clone());
                }
            }
            normalize_fresh_root_statuses(statuses);
        }
    }
}

fn normalize_fresh_root_statuses<'db>(statuses: &mut Vec<FreshRootStatus<'db>>) {
    let previous = mem::take(statuses);
    for status in previous {
        merge_fresh_root_statuses(statuses, status);
    }
}

fn merge_fresh_root_status<'db>(
    roots: &mut FreshRootStates<'db>,
    root: FreshStorageRoot,
    status: FreshRootStatus<'db>,
) {
    let statuses = roots.entry(root).or_default();
    merge_fresh_root_statuses(statuses, status);
}

fn merge_fresh_root_statuses<'db>(
    statuses: &mut Vec<FreshRootStatus<'db>>,
    status: FreshRootStatus<'db>,
) {
    let mut merged = status;
    let mut idx = 0;
    while idx < statuses.len() {
        if fresh_root_statuses_may_alias(&statuses[idx], &merged) {
            let candidate = statuses.swap_remove(idx);
            merged.retention_sites.extend(candidate.retention_sites);
            merged.carriers.extend(candidate.carriers);
            merged.claimed.extend(candidate.claimed);
        } else {
            idx += 1;
        }
    }
    statuses.push(merged);
}

fn fresh_root_statuses_may_alias(lhs: &FreshRootStatus<'_>, rhs: &FreshRootStatus<'_>) -> bool {
    lhs.generation == rhs.generation
        && (lhs.retention_sites.is_empty() && rhs.retention_sites.is_empty()
            || !lhs.retention_sites.is_disjoint(&rhs.retention_sites))
}

fn merge_fresh_value_status<'db>(into: &mut FreshValueStatus<'db>, from: FreshValueStatus<'db>) {
    into.complete &= from.complete;
    for (root, statuses) in from.roots {
        for status in statuses {
            merge_fresh_root_status(&mut into.roots, root, status);
        }
    }
}

fn fresh_layout_projection_matches(
    lhs: LayoutBackingProjection,
    rhs: LayoutBackingProjection,
) -> bool {
    let is_index = |projection| {
        matches!(
            projection,
            LayoutBackingProjection::Index(_) | LayoutBackingProjection::IndexFamily(_)
        )
    };
    let is_symbolic = |projection| {
        matches!(
            projection,
            LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_)
        )
    };
    lhs == rhs || is_index(lhs) && is_index(rhs) && (is_symbolic(lhs) || is_symbolic(rhs))
}

fn fresh_layout_path_is_prefix(
    prefix: &[LayoutBackingProjection],
    path: &[LayoutBackingProjection],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(lhs, rhs)| fresh_layout_projection_matches(lhs, rhs))
}

fn prefix_fresh_value_status<'db>(
    mut status: FreshValueStatus<'db>,
    prefix: &[LayoutBackingProjection],
) -> FreshValueStatus<'db> {
    for statuses in status.roots.values_mut() {
        for root_status in statuses {
            root_status.carriers = root_status
                .carriers
                .iter()
                .map(|carrier| {
                    let mut prefixed = prefix.to_vec();
                    prefixed.extend_from_slice(carrier);
                    prefixed
                })
                .collect();
        }
    }
    status
}

fn project_fresh_value_status<'db>(
    status: &FreshValueStatus<'db>,
    projection: &[LayoutBackingProjection],
) -> FreshValueStatus<'db> {
    let mut projected = FreshValueStatus {
        roots: FreshRootStates::default(),
        complete: status.complete,
    };
    for (&root, statuses) in &status.roots {
        for root_status in statuses {
            let mut root_status = root_status.clone();
            root_status.carriers = root_status
                .carriers
                .iter()
                .filter_map(|carrier| {
                    if fresh_layout_path_is_prefix(projection, carrier) {
                        Some(carrier[projection.len()..].to_vec())
                    } else if fresh_layout_path_is_prefix(carrier, projection) {
                        Some(FreshCarrierPath::new())
                    } else {
                        None
                    }
                })
                .collect();
            if !root_status.carriers.is_empty() {
                merge_fresh_root_status(&mut projected.roots, root, root_status);
            }
        }
    }
    projected
}

fn replace_fresh_value_projection<'db>(
    status: &mut FreshValueStatus<'db>,
    projection: &[LayoutBackingProjection],
    replacement: FreshValueStatus<'db>,
) {
    if !projection.contains(&LayoutBackingProjection::Index(None)) {
        status.roots.retain(|_, statuses| {
            statuses.retain_mut(|root_status| {
                root_status.carriers.retain(|carrier| {
                    !fresh_layout_path_is_prefix(projection, carrier)
                        || projection.iter().copied().zip(carrier.iter().copied()).any(
                            |(written, held)| {
                                matches!(
                                    (written, held),
                                    (
                                        LayoutBackingProjection::Index(Some(_)),
                                        LayoutBackingProjection::Index(None)
                                            | LayoutBackingProjection::IndexFamily(_)
                                    )
                                )
                            },
                        )
                });
                !root_status.carriers.is_empty()
            });
            !statuses.is_empty()
        });
    }
    status.complete &= replacement.complete;
    merge_fresh_value_status(status, prefix_fresh_value_status(replacement, projection));
}

pub(crate) fn normalized_cfg_successor_indices(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
) -> CfgAdjacency {
    fn all_successors(term: &NSTerminatorKind<'_>) -> BlockAdjacency {
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

    let mut raw_successors = CfgAdjacency::new();
    raw_successors.resize(body.blocks.len());
    for (idx, block) in body.blocks.iter().enumerate() {
        raw_successors[SBlockId::new(idx)] = all_successors(&block.terminator.kind);
    }
    let mut successors = CfgAdjacency::new();
    successors.resize(body.blocks.len());
    if body.blocks.is_empty() {
        return successors;
    }
    let cyclic = cfg_cycle_blocks(&raw_successors);
    let raw_predecessors = cfg_predecessor_indices(&raw_successors);
    let facts = NormalizedBodyFacts::new(body);
    let constants = ProgramPointConstantAnalysis::new(db, body);
    let liveness = program_point_liveness(
        body,
        &raw_successors,
        &facts,
        &constants,
        ProgramPointLivenessGoal::ControlFlow,
    );

    // Discover executable edges while propagating constants only across edges
    // already proven executable. Starting with the raw graph and deleting
    // edges after a whole-graph solve is insufficient for loops: an
    // infeasible backedge can poison the header's constant before the branch
    // that excludes that backedge is evaluated.
    let mut entry_states = SecondaryMap::new();
    entry_states.resize(body.blocks.len());
    let entry = SBlockId::new(0);
    let initial = project_program_point_values(
        &initial_program_point_state(db, body),
        &liveness.live_in[entry],
    );
    entry_states[entry] = initial.clone();

    let mut phis = ProgramPointPhiInterner::default();
    let mut edge_states = FxHashMap::<(SBlockId, SBlockId), ProgramPointConstantState>::default();
    let mut pending = VecDeque::from([entry]);
    let mut queued = vec![false; body.blocks.len()];
    queued[entry.index()] = true;
    while let Some(block_id) = pending.pop_front() {
        queued[block_id.index()] = false;
        let exit = constants.transfer_state(
            block_id,
            &entry_states[block_id],
            &liveness.track_assign_result[block_id.index()],
            &liveness.track_store_effect[block_id.index()],
        );
        let block = &body.blocks[block_id.index()];
        let feasible = match &block.terminator.kind {
            NSTerminatorKind::Branch {
                cond,
                then_bb,
                else_bb,
            } => match exit
                .values
                .get(&cond.local)
                .copied()
                .and_then(ProgramPointValue::as_constant)
            {
                Some(ProgramPointConstant::Bool(value)) => {
                    vec![if value { *then_bb } else { *else_bb }]
                        .into_iter()
                        .collect()
                }
                _ => all_successors(&block.terminator.kind),
            },
            NSTerminatorKind::MatchEnum {
                value,
                cases,
                default,
                ..
            } => match exit
                .values
                .get(&value.local)
                .copied()
                .and_then(ProgramPointValue::as_constant)
            {
                Some(ProgramPointConstant::EnumVariant(variant)) => cases
                    .iter()
                    .find_map(|(candidate, target)| (*candidate == variant).then_some(*target))
                    .or(*default)
                    .into_iter()
                    .collect(),
                _ => all_successors(&block.terminator.kind),
            },
            _ => all_successors(&block.terminator.kind),
        };

        for successor in feasible {
            if !successors[block_id].contains(&successor) {
                successors[block_id].push(successor);
            }
            let edge = project_program_point_values(&exit, &liveness.live_in[successor]);
            if edge_states.get(&(block_id, successor)) == Some(&edge) {
                continue;
            }
            edge_states.insert((block_id, successor), edge);
            let mut inputs = Vec::new();
            if successor == entry {
                inputs.push((None, &initial));
            }
            for predecessor in raw_predecessors[successor].iter().copied() {
                if let Some(state) = edge_states.get(&(predecessor, successor)) {
                    inputs.push((Some(predecessor), state));
                }
            }
            let merged = merge_program_point_inputs(
                &constants,
                successor,
                &inputs,
                &entry_states[successor],
                &liveness.live_in[successor],
                cyclic.contains(&successor),
                &mut phis,
            );
            if entry_states[successor] != merged {
                entry_states[successor] = merged;
                if !queued[successor.index()] {
                    pending.push_back(successor);
                    queued[successor.index()] = true;
                }
            }
        }
    }
    successors
}

/// The semantically reachable successors of each normalized block after
/// program-point constant refinement.
///
/// Runtime lowering consumes this public, representation-neutral form so it
/// cannot reintroduce branches that front-end analyses proved unreachable.
pub fn normalized_cfg_successors(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
) -> Vec<Vec<SBlockId>> {
    normalized_cfg_successors_and_reachable(db, body).0
}

/// Executable successors and their entry-reachability bitmap, computed in one
/// pass for consumers that need both.
pub fn normalized_cfg_successors_and_reachable(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
) -> (Vec<Vec<SBlockId>>, Vec<bool>) {
    let successors = normalized_cfg_successor_indices(db, body);
    let reachable = cfg_reachable_blocks(&successors);
    let successors = successors
        .iter()
        .map(|(_, successors)| successors.iter().copied().collect())
        .collect();
    let reachable = (0..body.blocks.len())
        .map(|idx| reachable.contains(&SBlockId::new(idx)))
        .collect();
    (successors, reachable)
}

/// Reachability bitmap derived from the same executable-edge graph consumed
/// by semantic analysis and runtime lowering.
pub fn normalized_cfg_reachable_blocks(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
) -> Vec<bool> {
    normalized_cfg_successors_and_reachable(db, body).1
}

pub(crate) fn cfg_reachable_blocks(successors: &CfgAdjacency) -> FxHashSet<SBlockId> {
    let mut reachable = FxHashSet::default();
    let mut pending = (!successors.is_empty())
        .then_some(SBlockId::new(0))
        .into_iter()
        .collect::<VecDeque<_>>();
    while let Some(block) = pending.pop_front() {
        if reachable.insert(block) {
            pending.extend(successors[block].iter().copied());
        }
    }
    reachable
}

fn cfg_predecessor_indices(successors: &CfgAdjacency) -> CfgAdjacency {
    let mut predecessors = CfgAdjacency::new();
    predecessors.resize(successors.iter().count());
    for (block, successors) in successors.iter() {
        for successor in successors.iter().copied() {
            predecessors[successor].push(block);
        }
    }
    predecessors
}

/// Values needed by the program-point identity solve.
///
/// Normalized bodies use SSA-like temporaries, but mutable storage locals may
/// be redefined. Retaining every definition on every later edge makes loop
/// convergence proportional to all historical temporaries rather than the
/// values a successor can observe. This ordinary backward liveness solve keeps
/// only cross-block values that can still be read.
///
/// Local metadata may refer to places and dynamic indices that are not direct
/// expression operands. Closing each live set over those dependencies keeps
/// projected-place, ownership, and layout-backing identities available.
struct ProgramPointLiveness {
    live_in: SecondaryMap<SBlockId, Vec<SLocalId>>,
    track_assign_result: Vec<Vec<bool>>,
    track_store_effect: Vec<Vec<bool>>,
}

#[derive(Clone, Copy)]
enum ProgramPointLivenessGoal {
    ControlFlow,
    IndexIdentity,
}

fn program_point_liveness(
    body: &NormalizedSemanticBody<'_>,
    successors: &CfgAdjacency,
    facts: &NormalizedBodyFacts,
    analysis: &ProgramPointConstantAnalysis<'_, '_>,
    goal: ProgramPointLivenessGoal,
) -> ProgramPointLiveness {
    fn close_dependencies(
        live: &mut FxHashSet<SLocalId>,
        facts: &NormalizedBodyFacts,
        seeds: impl IntoIterator<Item = SLocalId>,
    ) {
        let mut pending = seeds.into_iter().collect::<Vec<_>>();
        while let Some(local) = pending.pop() {
            if !live.insert(local) {
                continue;
            }
            pending.extend(facts.local_dependency_uses(local).iter().copied());
        }
    }

    fn terminator_seed(term: &NSTerminatorKind<'_>) -> Option<SLocalId> {
        match term {
            NSTerminatorKind::Branch { cond, .. } => Some(cond.local),
            NSTerminatorKind::MatchEnum { value, .. } => Some(value.local),
            NSTerminatorKind::Goto(_)
            | NSTerminatorKind::Assert { .. }
            | NSTerminatorKind::Return(_) => None,
        }
    }

    let projection_index_uses = body
        .blocks
        .iter()
        .map(|block| {
            block
                .stmts
                .iter()
                .map(|stmt| {
                    if matches!(goal, ProgramPointLivenessGoal::ControlFlow) {
                        return Vec::new();
                    }
                    let mut indices = Vec::new();
                    let mut add_place_indices = |place: &NSPlace<'_>| {
                        indices.extend(place.dynamic_index_locals());
                    };
                    match &stmt.kind {
                        NSStmtKind::Assign { expr, .. } => {
                            expr.for_each_place_operand(&mut add_place_indices);
                        }
                        NSStmtKind::Store { dst, .. } => add_place_indices(dst),
                    }
                    indices.sort_unstable_by_key(|local| local.index());
                    indices.dedup();
                    indices
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let predecessors = cfg_predecessor_indices(successors);
    let mut live_in = SecondaryMap::<SBlockId, FxHashSet<SLocalId>>::new();
    live_in.resize(body.blocks.len());
    let mut pending = (0..body.blocks.len())
        .map(SBlockId::new)
        .collect::<VecDeque<_>>();
    let mut queued = vec![true; body.blocks.len()];

    while let Some(block) = pending.pop_front() {
        queued[block.index()] = false;
        let mut live = FxHashSet::default();
        for successor in successors[block].iter().copied() {
            live.extend(live_in[successor].iter().copied());
        }
        close_dependencies(
            &mut live,
            facts,
            terminator_seed(&body.blocks[block.index()].terminator.kind),
        );
        for stmt_idx in (0..body.blocks[block.index()].stmts.len()).rev() {
            let stmt = &body.blocks[block.index()].stmts[stmt_idx];
            close_dependencies(
                &mut live,
                facts,
                projection_index_uses[block.index()][stmt_idx]
                    .iter()
                    .copied(),
            );
            let uses_are_observable = match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => {
                    let result_is_live = live.remove(dst);
                    result_is_live
                        || analysis.dead_assignment_may_have_program_point_effects(stmt.id, expr)
                }
                NSStmtKind::Store { .. } => {
                    analysis.store_may_affect_live_program_point_state(stmt.id, &live)
                }
            };
            if uses_are_observable {
                close_dependencies(
                    &mut live,
                    facts,
                    facts.stmt_uses(block, stmt_idx).iter().copied(),
                );
            }
        }
        if live_in[block] == live {
            continue;
        }
        live_in[block] = live;
        for predecessor in predecessors[block].iter().copied() {
            if !queued[predecessor.index()] {
                pending.push_back(predecessor);
                queued[predecessor.index()] = true;
            }
        }
    }

    let mut track_assign_result = body
        .blocks
        .iter()
        .map(|block| vec![false; block.stmts.len()])
        .collect::<Vec<_>>();
    let mut track_store_effect = track_assign_result.clone();
    for (block_idx, block) in body.blocks.iter().enumerate() {
        let block_id = SBlockId::new(block_idx);
        let mut live = FxHashSet::default();
        for successor in successors[block_id].iter().copied() {
            live.extend(live_in[successor].iter().copied());
        }
        close_dependencies(
            &mut live,
            facts,
            terminator_seed(&body.blocks[block_id.index()].terminator.kind),
        );
        for stmt_idx in (0..block.stmts.len()).rev() {
            let stmt = &block.stmts[stmt_idx];
            close_dependencies(
                &mut live,
                facts,
                projection_index_uses[block_idx][stmt_idx].iter().copied(),
            );
            let uses_are_observable = match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => {
                    let result_is_live = live.remove(dst);
                    track_assign_result[block_idx][stmt_idx] = result_is_live;
                    result_is_live
                        || analysis.dead_assignment_may_have_program_point_effects(stmt.id, expr)
                }
                NSStmtKind::Store { .. } => {
                    let observable =
                        analysis.store_may_affect_live_program_point_state(stmt.id, &live);
                    track_store_effect[block_idx][stmt_idx] = observable;
                    observable
                }
            };
            if uses_are_observable {
                close_dependencies(
                    &mut live,
                    facts,
                    facts.stmt_uses(block_id, stmt_idx).iter().copied(),
                );
            }
        }
    }
    let mut ordered_live_in = SecondaryMap::<SBlockId, Vec<SLocalId>>::new();
    ordered_live_in.resize(body.blocks.len());
    for (block, locals) in live_in.iter() {
        let mut locals = locals.iter().copied().collect::<Vec<_>>();
        locals.sort_unstable_by_key(|local| local.index());
        ordered_live_in[block] = locals;
    }
    ProgramPointLiveness {
        live_in: ordered_live_in,
        track_assign_result,
        track_store_effect,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ProgramPointConstant {
    Bool(bool),
    EnumVariant(crate::analysis::semantic::VariantIndex),
    Index(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ProgramPointValue {
    Constant(ProgramPointConstant),
    Entry(SLocalId),
    Definition(SStmtId),
    ImmutablePlace(SStmtId),
    Mutation {
        stmt: SStmtId,
        local: SLocalId,
    },
    Phi(u32),
    /// A value selected by control flow within one or more nested cycles.
    ///
    /// The interned key retains the set of cyclic merge points together with
    /// the representative local. Unioning those points makes nested-loop
    /// identities monotone instead of alternately relabeling a value with the
    /// inner and outer loop headers.
    LoopPhi(u32),
}

impl ProgramPointValue {
    fn as_constant(self) -> Option<ProgramPointConstant> {
        let Self::Constant(constant) = self else {
            return None;
        };
        Some(constant)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
enum ProgramPointAliasProjection {
    #[default]
    Any,
    Field(usize),
    VariantField {
        variant: crate::analysis::semantic::VariantIndex,
        field: usize,
    },
    ConstantIndex(usize),
    DynamicIndex(SLocalId),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct ProgramPointAliasRoots {
    roots: FxHashSet<SLocalId>,
    unknown: bool,
    children: FxHashMap<ProgramPointAliasProjection, ProgramPointAliasRoots>,
}

impl ProgramPointAliasRoots {
    fn from_root(root: SLocalId) -> Self {
        Self {
            roots: FxHashSet::from_iter([root]),
            unknown: false,
            children: FxHashMap::default(),
        }
    }

    fn unknown() -> Self {
        Self {
            roots: FxHashSet::default(),
            unknown: true,
            children: FxHashMap::default(),
        }
    }

    fn extend(&mut self, other: &Self) {
        self.extend_summary(other);
        for (projection, child) in &other.children {
            self.children
                .entry(projection.clone())
                .or_default()
                .extend(child);
        }
    }

    fn extend_summary(&mut self, other: &Self) {
        self.roots.extend(other.roots.iter().copied());
        self.unknown |= other.unknown;
    }

    fn is_empty(&self) -> bool {
        self.roots.is_empty() && !self.unknown && self.children.is_empty()
    }

    fn collect_referenced_roots(&self, out: &mut Vec<SLocalId>) {
        out.extend(self.roots.iter().copied());
        for child in self.children.values() {
            child.collect_referenced_roots(out);
        }
    }
}

/// Select every array child that may overlap `index`.
///
/// `None` denotes an unknown/dynamic index. For a constant selection an
/// `Any` child and a dynamic child whose value is not known unequal must be
/// joined with the exact constant child; preferring the exact child would
/// under-approximate provenance after control-flow/source merges.
fn projected_index_alias_roots(
    state: &ProgramPointConstantState,
    roots: &ProgramPointAliasRoots,
    index: Option<usize>,
) -> Option<ProgramPointAliasRoots> {
    let mut selected = ProgramPointAliasRoots {
        unknown: roots.unknown,
        ..ProgramPointAliasRoots::default()
    };
    for (candidate, child) in &roots.children {
        let overlaps = match candidate {
            ProgramPointAliasProjection::Any => true,
            ProgramPointAliasProjection::ConstantIndex(candidate) => {
                index.is_none_or(|index| *candidate == index)
            }
            ProgramPointAliasProjection::DynamicIndex(candidate) => index.is_none_or(|index| {
                !matches!(
                    state
                        .values
                        .get(candidate)
                        .copied()
                        .and_then(ProgramPointValue::as_constant),
                    Some(ProgramPointConstant::Index(candidate)) if candidate != index
                )
            }),
            ProgramPointAliasProjection::Field(_)
            | ProgramPointAliasProjection::VariantField { .. } => false,
        };
        if overlaps {
            selected.extend(child);
        }
    }
    (!selected.is_empty()).then_some(selected)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct ProgramPointConstantState {
    reached: bool,
    values: FxHashMap<SLocalId, ProgramPointValue>,
    /// Mutable storage roots reachable from each carrier or aggregate local.
    /// Assignments overwrite one local's provenance while copies, projections,
    /// aggregates, closures, and call results transport it.
    alias_roots: FxHashMap<SLocalId, ProgramPointAliasRoots>,
    /// Monotone fallback universe for the rare carrier whose exact provenance
    /// cannot be reconstructed. Exact carrier provenance never consults this
    /// set, so unrelated aliases remain independent.
    escaped_mut_roots: FxHashSet<SLocalId>,
}

struct ProgramPointConstantAnalysis<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedSemanticBody<'db>,
    mut_alias_carriers: ProgramPointMutAliasCache,
    primitive_wrapper_calls: FxHashMap<SStmtId, PrimitiveWrapperCallKind>,
    store_facts: FxHashMap<SStmtId, ProgramPointStoreFacts<'db>>,
    unqualified_local_tys: Vec<TyId<'db>>,
}

#[derive(Clone, Copy)]
enum ProgramPointStoreRoot {
    Direct(SLocalId),
    DynamicPlace,
    Unknown,
}

#[derive(Clone, Copy)]
struct ProgramPointStoreFacts<'db> {
    root: ProgramPointStoreRoot,
    target_ty: Option<TyId<'db>>,
}

fn program_point_unqualified_value_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
) -> TyId<'db> {
    while let Some((_, inner)) = ty.as_capability(db) {
        ty = inner;
    }
    ty
}

/// Immutable local classification shared by every transfer and merge in one
/// program-point solve.
///
/// `borrow_results_in_ty` recursively walks aggregate types. Calling it from
/// `merge_program_point_inputs` made loop convergence repeat that structural
/// walk for every local on every iteration, even though local types cannot
/// change during a solve.
struct ProgramPointMutAliasCache {
    carries: Vec<bool>,
}

impl ProgramPointMutAliasCache {
    fn new(
        local_count: usize,
        mut classify: impl FnMut(SLocalId) -> bool,
    ) -> ProgramPointMutAliasCache {
        let mut carries = Vec::with_capacity(local_count);
        for idx in 0..local_count {
            let local = SLocalId::new(idx);
            let carries_mut_alias = classify(local);
            carries.push(carries_mut_alias);
        }
        Self { carries }
    }

    fn carries(&self, local: SLocalId) -> bool {
        self.carries.get(local.index()).copied().unwrap_or(false)
    }
}

impl<'a, 'db> ProgramPointConstantAnalysis<'a, 'db> {
    fn new(db: &'db dyn HirAnalysisDb, body: &'a NormalizedSemanticBody<'db>) -> Self {
        let mut_alias_carriers = ProgramPointMutAliasCache::new(body.locals.len(), |local| {
            body.local(local).is_some_and(|local| {
                borrow_results_in_ty(db, local.ty)
                    .iter()
                    .any(|result| result.kind == BorrowKind::Mut)
            })
        });
        let primitive_wrapper_calls = body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .filter_map(|stmt| {
                let NSStmtKind::Assign {
                    dst,
                    expr: NExpr::Call { callee, .. },
                } = &stmt.kind
                else {
                    return None;
                };
                let BodyOwner::Func(func) = callee.key.owner(db) else {
                    return None;
                };
                let result_ty = body.local(*dst)?.ty;
                core_primitive_wrapper_call_kind(db, func, result_ty).map(|kind| (stmt.id, kind))
            })
            .collect();
        let unqualified_local_tys = body
            .locals
            .iter()
            .map(|local| program_point_unqualified_value_ty(db, local.layout_ty()))
            .collect::<Vec<_>>();
        let store_facts = body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .filter_map(|stmt| {
                let NSStmtKind::Store { dst, src } = &stmt.kind else {
                    return None;
                };
                let rebinds_capability = store_rebinds_capability(db, body, dst, *src);
                let root = match dst.root {
                    NSPlaceRoot::Root(root) => match body.root(root) {
                        Some(
                            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local },
                        ) => {
                            if dst.path.is_empty()
                                || rebinds_capability
                                || projection_uses_only_immutable_capabilities(
                                    db,
                                    body.local(*local)
                                        .expect("verified normalized local root")
                                        .layout_ty(),
                                    &dst.path,
                                ) == Some(true)
                            {
                                ProgramPointStoreRoot::Direct(*local)
                            } else {
                                ProgramPointStoreRoot::DynamicPlace
                            }
                        }
                        Some(NBorrowRoot::Provider { .. }) | None => ProgramPointStoreRoot::Unknown,
                    },
                    NSPlaceRoot::CarrierDerefLocal(_) => ProgramPointStoreRoot::DynamicPlace,
                };
                let target_ty = body
                    .place_ty(db, dst)
                    .map(|ty| program_point_unqualified_value_ty(db, ty));
                Some((stmt.id, ProgramPointStoreFacts { root, target_ty }))
            })
            .collect();
        Self {
            db,
            body,
            mut_alias_carriers,
            primitive_wrapper_calls,
            store_facts,
            unqualified_local_tys,
        }
    }

    fn local_carries_mut_alias(&self, local: SLocalId) -> bool {
        self.mut_alias_carriers.carries(local)
    }

    fn dead_assignment_may_have_program_point_effects(
        &self,
        stmt: SStmtId,
        expr: &NExpr<'_>,
    ) -> bool {
        match expr {
            NExpr::Call { .. } => !matches!(
                self.primitive_wrapper_calls.get(&stmt),
                Some(PrimitiveWrapperCallKind::Unary(_) | PrimitiveWrapperCallKind::Binary(_))
            ),
            _ => false,
        }
    }

    fn store_may_affect_live_program_point_state(
        &self,
        stmt: SStmtId,
        live: &FxHashSet<SLocalId>,
    ) -> bool {
        match self
            .store_facts
            .get(&stmt)
            .expect("program-point store facts precomputed")
            .root
        {
            ProgramPointStoreRoot::Direct(local) => {
                live.contains(&local) || self.local_carries_mut_alias(local)
            }
            ProgramPointStoreRoot::DynamicPlace | ProgramPointStoreRoot::Unknown => true,
        }
    }

    fn alias_roots_for_local(
        &self,
        state: &ProgramPointConstantState,
        local: SLocalId,
    ) -> ProgramPointAliasRoots {
        state.alias_roots.get(&local).cloned().unwrap_or_else(|| {
            if self.local_carries_mut_alias(local) {
                ProgramPointAliasRoots::unknown()
            } else {
                ProgramPointAliasRoots::default()
            }
        })
    }

    fn carried_alias_roots_for_place(
        &self,
        state: &ProgramPointConstantState,
        place: &NSPlace<'db>,
    ) -> ProgramPointAliasRoots {
        let roots = match place.root {
            NSPlaceRoot::Root(root) => match self.body.root(root) {
                Some(NBorrowRoot::Param { local, .. }) | Some(NBorrowRoot::LocalSlot { local }) => {
                    self.alias_roots_for_local(state, *local)
                }
                Some(NBorrowRoot::Provider { .. }) | None => ProgramPointAliasRoots::unknown(),
            },
            NSPlaceRoot::CarrierDerefLocal(local) => self.alias_roots_for_local(state, local),
        };
        self.projected_alias_roots(state, &roots, &place.path)
    }

    fn projected_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
        path: &NSProjectionPath<'db>,
    ) -> ProgramPointAliasRoots {
        let mut selected = roots.clone();
        let mut matched = false;
        for projection in path.iter() {
            let projection = match projection {
                Projection::Field(field) => ProgramPointAliasProjection::Field(*field),
                Projection::VariantField {
                    variant, field_idx, ..
                } => ProgramPointAliasProjection::VariantField {
                    variant: *variant,
                    field: *field_idx,
                },
                Projection::Index(IndexSource::Constant(index)) => {
                    ProgramPointAliasProjection::ConstantIndex(*index)
                }
                Projection::Index(IndexSource::Dynamic(index)) => {
                    match state
                        .values
                        .get(index)
                        .copied()
                        .and_then(ProgramPointValue::as_constant)
                    {
                        Some(ProgramPointConstant::Index(index)) => {
                            ProgramPointAliasProjection::ConstantIndex(index)
                        }
                        _ => ProgramPointAliasProjection::DynamicIndex(*index),
                    }
                }
                Projection::Deref => break,
                Projection::Discriminant => return ProgramPointAliasRoots::default(),
            };
            let child = match projection {
                ProgramPointAliasProjection::ConstantIndex(index) => {
                    projected_index_alias_roots(state, &selected, Some(index))
                }
                ProgramPointAliasProjection::DynamicIndex(index) => projected_index_alias_roots(
                    state,
                    &selected,
                    state
                        .values
                        .get(&index)
                        .copied()
                        .and_then(ProgramPointValue::as_constant)
                        .and_then(|value| match value {
                            ProgramPointConstant::Index(index) => Some(index),
                            ProgramPointConstant::Bool(_)
                            | ProgramPointConstant::EnumVariant(_) => None,
                        }),
                ),
                ProgramPointAliasProjection::Any => {
                    projected_index_alias_roots(state, &selected, None)
                }
                projection => selected.children.get(&projection).cloned(),
            };
            let Some(mut child) = child else {
                // Missing structural detail is conservative, not proof that
                // the projection carries no mutable root.
                return selected;
            };
            child.unknown |= selected.unknown;
            selected = child;
            matched = true;
        }
        if matched { selected } else { roots.clone() }
    }

    fn projected_layout_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
        path: &[LayoutBackingProjection],
    ) -> ProgramPointAliasRoots {
        let mut selected = roots.clone();
        for projection in path {
            let child = match projection {
                LayoutBackingProjection::Field(field) => selected
                    .children
                    .get(&ProgramPointAliasProjection::Field(usize::from(field.0)))
                    .cloned(),
                LayoutBackingProjection::VariantField { variant, field } => selected
                    .children
                    .get(&ProgramPointAliasProjection::VariantField {
                        variant: *variant,
                        field: usize::from(field.0),
                    })
                    .cloned(),
                LayoutBackingProjection::Index(Some(index)) => {
                    projected_index_alias_roots(state, &selected, Some(*index))
                }
                LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_) => {
                    projected_index_alias_roots(state, &selected, None)
                }
            };
            let Some(mut child) = child else {
                return selected;
            };
            child.unknown |= selected.unknown;
            selected = child;
        }
        selected
    }

    fn projected_call_return_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
        path: &[SCallReturnProjectionStep],
    ) -> ProgramPointAliasRoots {
        let mut selected = roots.clone();
        for step in path {
            let projection = match *step {
                SCallReturnProjectionStep::Field(field) => {
                    ProgramPointAliasProjection::Field(usize::from(field))
                }
                SCallReturnProjectionStep::VariantField { variant, field } => {
                    ProgramPointAliasProjection::VariantField {
                        variant: crate::analysis::semantic::VariantIndex(variant),
                        field: usize::from(field),
                    }
                }
                SCallReturnProjectionStep::ConstantIndex(index) => {
                    ProgramPointAliasProjection::ConstantIndex(index)
                }
                SCallReturnProjectionStep::DynamicIndex(index) => match state
                    .values
                    .get(&index)
                    .copied()
                    .and_then(ProgramPointValue::as_constant)
                {
                    Some(ProgramPointConstant::Index(index)) => {
                        ProgramPointAliasProjection::ConstantIndex(index)
                    }
                    _ => ProgramPointAliasProjection::DynamicIndex(index),
                },
                SCallReturnProjectionStep::AnyIndex => ProgramPointAliasProjection::Any,
            };
            let child = match projection {
                ProgramPointAliasProjection::ConstantIndex(index) => {
                    projected_index_alias_roots(state, &selected, Some(index))
                }
                ProgramPointAliasProjection::DynamicIndex(index) => projected_index_alias_roots(
                    state,
                    &selected,
                    state
                        .values
                        .get(&index)
                        .copied()
                        .and_then(ProgramPointValue::as_constant)
                        .and_then(|value| match value {
                            ProgramPointConstant::Index(index) => Some(index),
                            ProgramPointConstant::Bool(_)
                            | ProgramPointConstant::EnumVariant(_) => None,
                        }),
                ),
                ProgramPointAliasProjection::Any => {
                    projected_index_alias_roots(state, &selected, None)
                }
                projection => selected.children.get(&projection).cloned(),
            };
            let Some(mut child) = child else {
                return selected;
            };
            child.unknown |= selected.unknown;
            selected = child;
        }
        selected
    }

    fn prefixed_layout_alias_roots(
        &self,
        roots: &ProgramPointAliasRoots,
        path: &[LayoutBackingProjection],
    ) -> ProgramPointAliasRoots {
        let mut nested = roots.clone();
        for projection in path.iter().rev() {
            let projection = match projection {
                LayoutBackingProjection::Field(field) => {
                    ProgramPointAliasProjection::Field(usize::from(field.0))
                }
                LayoutBackingProjection::VariantField { variant, field } => {
                    ProgramPointAliasProjection::VariantField {
                        variant: *variant,
                        field: usize::from(field.0),
                    }
                }
                LayoutBackingProjection::Index(Some(index)) => {
                    ProgramPointAliasProjection::ConstantIndex(*index)
                }
                LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_) => {
                    ProgramPointAliasProjection::Any
                }
            };
            let mut parent = ProgramPointAliasRoots::default();
            parent.extend_summary(&nested);
            parent.children.insert(projection, nested);
            nested = parent;
        }
        nested
    }

    fn prefixed_call_return_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
        path: &[SCallReturnProjectionStep],
    ) -> ProgramPointAliasRoots {
        let mut nested = roots.clone();
        for step in path.iter().rev() {
            let projection = match *step {
                SCallReturnProjectionStep::Field(field) => {
                    ProgramPointAliasProjection::Field(usize::from(field))
                }
                SCallReturnProjectionStep::VariantField { variant, field } => {
                    ProgramPointAliasProjection::VariantField {
                        variant: crate::analysis::semantic::VariantIndex(variant),
                        field: usize::from(field),
                    }
                }
                SCallReturnProjectionStep::ConstantIndex(index) => {
                    ProgramPointAliasProjection::ConstantIndex(index)
                }
                SCallReturnProjectionStep::DynamicIndex(index) => match state
                    .values
                    .get(&index)
                    .copied()
                    .and_then(ProgramPointValue::as_constant)
                {
                    Some(ProgramPointConstant::Index(index)) => {
                        ProgramPointAliasProjection::ConstantIndex(index)
                    }
                    _ => ProgramPointAliasProjection::DynamicIndex(index),
                },
                SCallReturnProjectionStep::AnyIndex => ProgramPointAliasProjection::Any,
            };
            let mut parent = ProgramPointAliasRoots::default();
            parent.extend_summary(&nested);
            parent.children.insert(projection, nested);
            nested = parent;
        }
        nested
    }

    fn call_arg_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        callee: SemanticInstanceKey<'db>,
        idx: usize,
        arg: &NOperand,
    ) -> ProgramPointAliasRoots {
        let mut roots = self.alias_roots_for_local(state, arg.local);
        if roots.is_empty()
            && callee
                .callable_body(self.db)
                .param_binding(self.db, idx)
                .is_some_and(|binding| {
                    matches!(
                        binding,
                        LocalBinding::Param { mode, .. } if mode != FuncParamMode::Own
                    ) && binding.is_mut()
                })
        {
            roots = ProgramPointAliasRoots::from_root(arg.local);
        }
        roots
    }

    fn call_result_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        callee: SemanticInstanceKey<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        return_sources: NCallReturnSources<'_>,
        result_local: SLocalId,
    ) -> Option<ProgramPointAliasRoots> {
        let NCallReturnSources { sources, complete } = return_sources;
        let result_ty = self.body.local(result_local)?.ty;
        let mutable_results = borrow_results_in_ty(self.db, result_ty)
            .into_iter()
            .filter(|result| result.kind == BorrowKind::Mut)
            .collect::<Vec<_>>();
        if sources.is_empty() {
            return None;
        }
        let mut result = ProgramPointAliasRoots::default();
        let mut mapped_mut_result = false;
        for source in sources {
            let roots = match source.origin {
                CallableInputLayoutHoleOrigin::Receiver => {
                    let arg = args.first()?;
                    self.call_arg_alias_roots(state, callee, 0, arg)
                }
                CallableInputLayoutHoleOrigin::ValueParam(param) => {
                    let arg = args.get(param)?;
                    self.call_arg_alias_roots(state, callee, param, arg)
                }
                CallableInputLayoutHoleOrigin::Effect(effect) => {
                    let effect_arg = effect_args
                        .iter()
                        .find(|arg| arg.binding_idx as usize == effect)?;
                    match &effect_arg.arg {
                        NEffectArgValue::Place(place) => {
                            self.carried_alias_roots_for_place(state, place)
                        }
                        NEffectArgValue::Value(value) => {
                            self.alias_roots_for_local(state, value.local)
                        }
                    }
                }
            };
            for mutable_result in &mutable_results {
                let Some(suffix) = call_return_projection_result_suffix(
                    &source.result_projection,
                    &mutable_result.projection,
                ) else {
                    continue;
                };
                let roots =
                    self.projected_call_return_alias_roots(state, &roots, &source.projection);
                let roots = self.projected_layout_alias_roots(state, &roots, &suffix);
                let roots = self.prefixed_layout_alias_roots(&roots, &suffix);
                result.extend(&self.prefixed_call_return_alias_roots(
                    state,
                    &roots,
                    &source.result_projection,
                ));
                mapped_mut_result = true;
            }
        }
        if !complete {
            // Forwarded sources are a may-set. A partial set is still useful
            // for retaining known roots, but it cannot exclude an additional
            // escaped mutable root.
            result.unknown = true;
        }
        mapped_mut_result.then_some(result)
    }

    fn borrowed_alias_roots_for_place(
        &self,
        state: &ProgramPointConstantState,
        place: &NSPlace<'db>,
        kind: BorrowKind,
    ) -> ProgramPointAliasRoots {
        if kind == BorrowKind::Mut
            && let NSPlaceRoot::Root(root) = place.root
            && let Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) =
                self.body.root(root)
        {
            // Keep direct storage edges rather than flattening the current
            // contents of the borrowed root. A carrier pointing at an
            // aggregate must observe later replacements of nested handles.
            return ProgramPointAliasRoots::from_root(*local);
        }
        self.carried_alias_roots_for_place(state, place)
    }

    fn alias_roots_for_expr(
        &self,
        state: &ProgramPointConstantState,
        expr: &NExpr<'db>,
        result_local: SLocalId,
    ) -> ProgramPointAliasRoots {
        if !self.local_carries_mut_alias(result_local) {
            return ProgramPointAliasRoots::default();
        }
        let operand_roots = |operand: &NOperand| self.alias_roots_for_local(state, operand.local);
        match expr {
            NExpr::Use(value) | NExpr::Cast { value, .. } => operand_roots(value),
            NExpr::ArrayRepeat { value, .. } => {
                let value_roots = operand_roots(value);
                let mut roots = ProgramPointAliasRoots::default();
                roots.extend_summary(&value_roots);
                roots
                    .children
                    .entry(ProgramPointAliasProjection::Any)
                    .or_default()
                    .extend(&value_roots);
                roots
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                let roots = operand_roots(value);
                roots
                    .children
                    .get(&ProgramPointAliasProjection::VariantField {
                        variant: *variant,
                        field: usize::from(field.0),
                    })
                    .cloned()
                    .unwrap_or(roots)
            }
            NExpr::ReadPlace { place, .. } => self.carried_alias_roots_for_place(state, place),
            NExpr::Borrow { place, kind, .. } => {
                self.borrowed_alias_roots_for_place(state, place, *kind)
            }
            NExpr::AggregateMake { ty, fields } => {
                let mut roots = ProgramPointAliasRoots::default();
                for (field, operand) in fields.iter().enumerate() {
                    let field_roots = operand_roots(operand);
                    roots.extend_summary(&field_roots);
                    let projection = if ty.is_array(self.db) {
                        ProgramPointAliasProjection::ConstantIndex(field)
                    } else {
                        ProgramPointAliasProjection::Field(field)
                    };
                    roots
                        .children
                        .entry(projection)
                        .or_default()
                        .extend(&field_roots);
                }
                roots
            }
            NExpr::EnumMake {
                variant, fields, ..
            } => {
                let mut roots = ProgramPointAliasRoots::default();
                for (field, operand) in fields.iter().enumerate() {
                    let field_roots = operand_roots(operand);
                    roots.extend_summary(&field_roots);
                    roots
                        .children
                        .entry(ProgramPointAliasProjection::VariantField {
                            variant: *variant,
                            field,
                        })
                        .or_default()
                        .extend(&field_roots);
                }
                roots
            }
            NExpr::Call {
                callee,
                args,
                effect_args,
                return_sources,
                return_sources_complete,
                ..
            } => {
                if let Some(roots) = self.call_result_alias_roots(
                    state,
                    callee.key,
                    args,
                    effect_args,
                    NCallReturnSources {
                        sources: return_sources,
                        complete: *return_sources_complete,
                    },
                    result_local,
                ) {
                    return roots;
                }
                let mut roots = ProgramPointAliasRoots::default();
                for (idx, arg) in args.iter().enumerate() {
                    let arg_roots = self.call_arg_alias_roots(state, callee.key, idx, arg);
                    roots.extend(&arg_roots);
                }
                for effect_arg in effect_args {
                    let effect_roots = match &effect_arg.arg {
                        NEffectArgValue::Place(place) => {
                            self.carried_alias_roots_for_place(state, place)
                        }
                        NEffectArgValue::Value(value) => operand_roots(value),
                    };
                    roots.extend(&effect_roots);
                }
                if roots.is_empty() {
                    ProgramPointAliasRoots::unknown()
                } else {
                    roots
                }
            }
            NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionRef { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => ProgramPointAliasRoots::unknown(),
        }
    }

    fn set_alias_roots(
        &self,
        state: &mut ProgramPointConstantState,
        local: SLocalId,
        roots: ProgramPointAliasRoots,
    ) {
        state.alias_roots.remove(&local);
        if roots.is_empty() {
            return;
        }
        state.escaped_mut_roots.extend(roots.roots.iter().copied());
        state.alias_roots.insert(local, roots);
    }

    fn expanded_alias_roots(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
    ) -> FxHashSet<SLocalId> {
        let mut expanded = FxHashSet::default();
        let mut pending = roots.roots.iter().copied().collect::<Vec<_>>();
        if roots.unknown {
            pending.extend(state.escaped_mut_roots.iter().copied());
        }
        while let Some(root) = pending.pop() {
            if !expanded.insert(root) {
                continue;
            }
            if let Some(nested) = state.alias_roots.get(&root) {
                pending.extend(nested.roots.iter().copied());
                if nested.unknown {
                    pending.extend(state.escaped_mut_roots.iter().copied());
                }
            }
        }
        expanded
    }

    fn alias_roots_have_unknown(
        &self,
        state: &ProgramPointConstantState,
        roots: &ProgramPointAliasRoots,
    ) -> bool {
        if roots.unknown {
            return true;
        }
        let mut visited = FxHashSet::default();
        let mut pending = roots.roots.iter().copied().collect::<Vec<_>>();
        while let Some(root) = pending.pop() {
            if !visited.insert(root) {
                continue;
            }
            if let Some(nested) = state.alias_roots.get(&root) {
                if nested.unknown {
                    return true;
                }
                pending.extend(nested.roots.iter().copied());
            }
        }
        false
    }

    /// Storage roots whose whole payload is selected by this store.
    ///
    /// Direct provenance edges are expanded dynamically, then filtered by the
    /// destination payload type. This distinguishes replacing an aggregate
    /// reached through `mut Aggregate` from writing through a nested scalar
    /// handle held by that aggregate.
    fn store_value_targets(
        &self,
        state: &ProgramPointConstantState,
        stmt: SStmtId,
        dst: &NSPlace<'db>,
    ) -> (FxHashSet<SLocalId>, bool) {
        let facts = self
            .store_facts
            .get(&stmt)
            .expect("program-point store facts precomputed");
        let direct = match facts.root {
            ProgramPointStoreRoot::Direct(local) => ProgramPointAliasRoots::from_root(local),
            ProgramPointStoreRoot::DynamicPlace => self.carried_alias_roots_for_place(state, dst),
            ProgramPointStoreRoot::Unknown => ProgramPointAliasRoots::unknown(),
        };
        let unknown = self.alias_roots_have_unknown(state, &direct);
        let expanded = self.expanded_alias_roots(state, &direct);
        let matching = facts
            .target_ty
            .map(|target_ty| {
                expanded
                    .iter()
                    .copied()
                    .filter(|root| {
                        self.unqualified_local_tys.get(root.index()).copied() == Some(target_ty)
                    })
                    .collect::<FxHashSet<_>>()
            })
            .unwrap_or_default();
        let whole_targets = !matching.is_empty();
        let targets = if whole_targets { matching } else { expanded };
        let exact_whole_target = whole_targets && targets.len() == 1 && !unknown;
        (targets, exact_whole_target)
    }

    fn place_mutation_targets(
        &self,
        state: &ProgramPointConstantState,
        place: &NSPlace<'db>,
        include_carried_roots: bool,
    ) -> FxHashSet<SLocalId> {
        let mut targets = FxHashSet::default();
        match place.root {
            NSPlaceRoot::Root(root) => {
                if let Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) =
                    self.body.root(root)
                {
                    targets.insert(*local);
                    if include_carried_roots {
                        targets.extend(self.expanded_alias_roots(
                            state,
                            &self.carried_alias_roots_for_place(state, place),
                        ));
                    }
                } else if include_carried_roots {
                    targets.extend(state.escaped_mut_roots.iter().copied());
                }
            }
            NSPlaceRoot::CarrierDerefLocal(local) => {
                targets.insert(local);
                targets.extend(self.expanded_alias_roots(
                    state,
                    &self.carried_alias_roots_for_place(state, place),
                ));
            }
        }
        targets
    }

    fn expr_mutation_targets(
        &self,
        state: &ProgramPointConstantState,
        expr: &NExpr<'db>,
    ) -> FxHashSet<SLocalId> {
        let NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } = expr
        else {
            return FxHashSet::default();
        };
        let callable = callee.key.callable_body(self.db);
        let mut targets = FxHashSet::default();
        for (idx, arg) in args.iter().enumerate() {
            let mutably_passed_by_place =
                callable.param_binding(self.db, idx).is_some_and(|binding| {
                    matches!(
                        binding,
                        LocalBinding::Param { mode, .. } if mode != FuncParamMode::Own
                    ) && binding.is_mut()
                });
            if mutably_passed_by_place {
                targets.insert(arg.local);
            }
            let Some(local) = self.body.local(arg.local) else {
                continue;
            };
            let roots = self.alias_roots_for_local(state, arg.local);
            for result in borrow_results_in_ty(self.db, local.ty)
                .into_iter()
                .filter(|result| result.kind == BorrowKind::Mut)
            {
                let projected =
                    self.projected_layout_alias_roots(state, &roots, &result.projection);
                targets.extend(self.expanded_alias_roots(state, &projected));
            }
        }
        for arg in effect_args {
            if arg.required_mut {
                match &arg.arg {
                    NEffectArgValue::Place(place) => {
                        targets.extend(self.place_mutation_targets(state, place, true));
                    }
                    NEffectArgValue::Value(value) => {
                        targets.insert(value.local);
                        let roots = self.alias_roots_for_local(state, value.local);
                        if roots.is_empty() {
                            targets.insert(value.local);
                        } else {
                            targets.extend(self.expanded_alias_roots(state, &roots));
                        }
                    }
                }
            }

            // A readonly effect container may still expose mutable borrow
            // slots nested inside it. The callee cannot replace the container,
            // but it can mutate every referent reachable through those slots.
            let target_ty = arg.target_ty.or_else(|| match &arg.arg {
                NEffectArgValue::Value(value) => self.body.local(value.local).map(|local| local.ty),
                NEffectArgValue::Place(place) => self.body.place_ty(self.db, place),
            });
            let Some(target_ty) = target_ty else {
                continue;
            };
            let roots = match &arg.arg {
                NEffectArgValue::Place(place) => self.carried_alias_roots_for_place(state, place),
                NEffectArgValue::Value(value) => self.alias_roots_for_local(state, value.local),
            };
            for result in borrow_results_in_ty(self.db, target_ty)
                .into_iter()
                .filter(|result| result.kind == BorrowKind::Mut)
            {
                let projected =
                    self.projected_layout_alias_roots(state, &roots, &result.projection);
                targets.extend(self.expanded_alias_roots(state, &projected));
            }
        }
        targets
    }

    /// Aggregate carriers whose nested mutable-root provenance a call may
    /// replace.
    ///
    /// Passing an aggregate through a top-level `mut` capability (or another
    /// mutable by-place binding) lets the callee rebind mutable capabilities
    /// stored inside it. A readonly or owned outer wrapper cannot itself be
    /// rebound, but a nested `mut Aggregate` slot still lets the callee
    /// replace capabilities inside that shared aggregate referent.
    fn expr_rebound_carrier_targets(
        &self,
        state: &ProgramPointConstantState,
        expr: &NExpr<'db>,
    ) -> FxHashSet<SLocalId> {
        let NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } = expr
        else {
            return FxHashSet::default();
        };
        let payload_may_carry_mut_alias = |ty| {
            let ty = program_point_unqualified_value_ty(self.db, ty);
            signature_input_is_unresolved(self.db, ty)
                || borrow_results_in_ty(self.db, ty)
                    .iter()
                    .any(|result| result.kind == BorrowKind::Mut)
        };
        let mutable_result_may_rebind_carrier = |target_ty, result: &BorrowResult| {
            semantic_projection_for_layout_path(self.db, target_ty, &result.projection)
                .and_then(|projection| semantic_projection_ty(self.db, target_ty, &projection))
                .and_then(|(ty, _)| ty.as_capability(self.db))
                .is_none_or(|(kind, payload)| {
                    kind == CapabilityKind::Mut && payload_may_carry_mut_alias(payload)
                })
        };
        let callable = callee.key.callable_body(self.db);
        let mut targets = FxHashSet::default();
        for (idx, arg) in args.iter().enumerate() {
            let Some(local) = self.body.local(arg.local) else {
                continue;
            };
            let Some(binding @ LocalBinding::Param { mode, .. }) =
                callable.param_binding(self.db, idx)
            else {
                continue;
            };
            let mutably_borrowed_payload = local
                .ty
                .as_capability(self.db)
                .is_some_and(|(kind, _)| kind == CapabilityKind::Mut);
            if mode != FuncParamMode::Own
                && (binding.is_mut() || mutably_borrowed_payload)
                && payload_may_carry_mut_alias(local.ty)
            {
                targets.insert(arg.local);
                let roots = if self.local_carries_mut_alias(arg.local) {
                    self.alias_roots_for_local(state, arg.local)
                } else {
                    ProgramPointAliasRoots::from_root(arg.local)
                };
                targets.extend(self.expanded_alias_roots(state, &roots));
            }

            // Moving or viewing an outer aggregate does not copy the storage
            // reached through its nested mutable capabilities. A nested
            // `mut Aggregate` referent can therefore have its own capability
            // fields rebound even when the outer argument is Own or readonly.
            let roots = self.alias_roots_for_local(state, arg.local);
            for result in borrow_results_in_ty(self.db, local.ty)
                .into_iter()
                .filter(|result| {
                    result.kind == BorrowKind::Mut
                        && mutable_result_may_rebind_carrier(local.ty, result)
                })
            {
                let projected =
                    self.projected_layout_alias_roots(state, &roots, &result.projection);
                targets.extend(self.expanded_alias_roots(state, &projected));
            }
        }
        for arg in effect_args {
            if arg.required_mut {
                match &arg.arg {
                    NEffectArgValue::Place(place) => {
                        if self
                            .body
                            .place_ty(self.db, place)
                            .is_some_and(payload_may_carry_mut_alias)
                        {
                            targets.extend(self.place_mutation_targets(state, place, true));
                        }
                    }
                    NEffectArgValue::Value(value) => {
                        if self.body.local(value.local).is_some_and(|local| {
                            local
                                .ty
                                .as_capability(self.db)
                                .is_some_and(|(kind, _)| kind == CapabilityKind::Mut)
                                && payload_may_carry_mut_alias(local.ty)
                        }) {
                            targets.insert(value.local);
                            targets.extend(self.expanded_alias_roots(
                                state,
                                &self.alias_roots_for_local(state, value.local),
                            ));
                        }
                    }
                }
            }

            // A readonly wrapper cannot itself be rebound, but a nested
            // `mut Aggregate` slot gives the callee authority to replace
            // mutable capabilities inside that aggregate referent. Invalidate
            // only those referent carrier roots, leaving the wrapper's own
            // field-to-root projection intact.
            let target_ty = arg.target_ty.or_else(|| match &arg.arg {
                NEffectArgValue::Value(value) => self.body.local(value.local).map(|local| local.ty),
                NEffectArgValue::Place(place) => self.body.place_ty(self.db, place),
            });
            let Some(target_ty) = target_ty else {
                continue;
            };
            let roots = match &arg.arg {
                NEffectArgValue::Place(place) => self.carried_alias_roots_for_place(state, place),
                NEffectArgValue::Value(value) => self.alias_roots_for_local(state, value.local),
            };
            for result in borrow_results_in_ty(self.db, target_ty)
                .into_iter()
                .filter(|result| result.kind == BorrowKind::Mut)
            {
                if !mutable_result_may_rebind_carrier(target_ty, &result) {
                    continue;
                }
                let projected =
                    self.projected_layout_alias_roots(state, &roots, &result.projection);
                targets.extend(self.expanded_alias_roots(state, &projected));
            }
        }
        targets.retain(|local| self.local_carries_mut_alias(*local));
        targets
    }

    fn immutable_place_value(&self, place: &NSPlace<'db>) -> Option<ProgramPointValue> {
        let source_local = mutated_place_root_local(self.body, place)?;
        let source = self.body.local(source_local)?;
        let root_is_immutable = match place.root {
            NSPlaceRoot::Root(_) => source.mutability == Mutability::Immutable,
            NSPlaceRoot::CarrierDerefLocal(_) => {
                source.mutability == Mutability::Immutable
                    && source.ty.as_capability(self.db).is_some_and(|(kind, _)| {
                        matches!(kind, CapabilityKind::View | CapabilityKind::Ref)
                    })
            }
        };
        if !root_is_immutable
            || !projection_uses_only_immutable_capabilities(
                self.db,
                source.layout_ty(),
                &place.path,
            )?
        {
            return None;
        }
        self.body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|stmt| match &stmt.kind {
                NSStmtKind::Assign {
                    expr:
                        NExpr::ReadPlace {
                            place: candidate, ..
                        },
                    ..
                } if candidate == place => Some(ProgramPointValue::ImmutablePlace(stmt.id)),
                _ => None,
            })
    }

    fn expr_value(
        &self,
        state: &ProgramPointConstantState,
        stmt: SStmtId,
        expr: &NExpr<'db>,
    ) -> Option<ProgramPointValue> {
        let operand_constant = |operand: &NOperand| {
            state
                .values
                .get(&operand.local)
                .copied()
                .and_then(ProgramPointValue::as_constant)
        };
        match expr {
            NExpr::Const(SConst::Value(value)) => match value.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(value),
                    ..
                } => Some(ProgramPointValue::Constant(ProgramPointConstant::Bool(
                    value,
                ))),
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } => value
                    .to_usize()
                    .map(|value| ProgramPointValue::Constant(ProgramPointConstant::Index(value))),
                SemConstValue::Enum { variant, .. } => Some(ProgramPointValue::Constant(
                    ProgramPointConstant::EnumVariant(variant),
                )),
                _ => None,
            },
            NExpr::EnumMake { variant, .. } => Some(ProgramPointValue::Constant(
                ProgramPointConstant::EnumVariant(*variant),
            )),
            NExpr::Unary {
                op: UnOp::Not,
                value,
            } => match operand_constant(value) {
                Some(ProgramPointConstant::Bool(value)) => Some(ProgramPointValue::Constant(
                    ProgramPointConstant::Bool(!value),
                )),
                _ => None,
            },
            NExpr::Binary { op, lhs, rhs } => self.binary_value(state, *op, lhs, rhs),
            NExpr::GetEnumTag { value } => match operand_constant(value) {
                Some(ProgramPointConstant::EnumVariant(variant)) => {
                    Some(ProgramPointValue::Constant(ProgramPointConstant::Index(
                        usize::from(variant.0),
                    )))
                }
                _ => None,
            },
            NExpr::IsEnumVariant { value, variant } => match operand_constant(value) {
                Some(ProgramPointConstant::EnumVariant(actual)) => Some(
                    ProgramPointValue::Constant(ProgramPointConstant::Bool(actual == *variant)),
                ),
                _ => None,
            },
            NExpr::Use(value) => state.values.get(&value.local).cloned(),
            NExpr::ReadPlace { place, .. } if place.path.is_empty() => match place.root {
                NSPlaceRoot::Root(root) => match self.body.root(root) {
                    Some(NBorrowRoot::Param { local, .. })
                    | Some(NBorrowRoot::LocalSlot { local }) => state.values.get(local).cloned(),
                    Some(NBorrowRoot::Provider { .. }) | None => None,
                },
                NSPlaceRoot::CarrierDerefLocal(local)
                    if self.body.local(local).is_some_and(|local| {
                        local.mutability == Mutability::Immutable
                            && local.ty.as_capability(self.db).is_some_and(|(kind, _)| {
                                matches!(kind, CapabilityKind::View | CapabilityKind::Ref)
                            })
                    }) =>
                {
                    state.values.get(&local).copied()
                }
                NSPlaceRoot::CarrierDerefLocal(_) => None,
            },
            NExpr::ReadPlace { place, .. } => self.immutable_place_value(place),
            NExpr::Call { args, .. } => match self.primitive_wrapper_calls.get(&stmt).copied()? {
                PrimitiveWrapperCallKind::Unary(UnOp::Not) => {
                    let [value] = args.as_ref() else {
                        return None;
                    };
                    match operand_constant(value) {
                        Some(ProgramPointConstant::Bool(value)) => Some(
                            ProgramPointValue::Constant(ProgramPointConstant::Bool(!value)),
                        ),
                        _ => None,
                    }
                }
                PrimitiveWrapperCallKind::Binary(op) => {
                    let [lhs, rhs] = args.as_ref() else {
                        return None;
                    };
                    self.binary_value(state, op, lhs, rhs)
                }
                PrimitiveWrapperCallKind::Unary(_) | PrimitiveWrapperCallKind::Assign(_) => None,
            },
            _ => None,
        }
    }

    fn binary_value(
        &self,
        state: &ProgramPointConstantState,
        op: BinOp,
        lhs: &NOperand,
        rhs: &NOperand,
    ) -> Option<ProgramPointValue> {
        let lhs_value = state.values.get(&lhs.local).copied()?;
        let rhs_value = state.values.get(&rhs.local).copied()?;
        if lhs_value == rhs_value && matches!(op, BinOp::Comp(CompBinOp::Eq | CompBinOp::NotEq)) {
            return Some(ProgramPointValue::Constant(ProgramPointConstant::Bool(
                matches!(op, BinOp::Comp(CompBinOp::Eq)),
            )));
        }
        let lhs = lhs_value.as_constant()?;
        let rhs = rhs_value.as_constant()?;
        let value = match op {
            BinOp::Comp(CompBinOp::Eq) => lhs == rhs,
            BinOp::Comp(CompBinOp::NotEq) => lhs != rhs,
            BinOp::Comp(
                op @ (CompBinOp::Lt | CompBinOp::LtEq | CompBinOp::Gt | CompBinOp::GtEq),
            ) => {
                let (ProgramPointConstant::Index(lhs), ProgramPointConstant::Index(rhs)) =
                    (lhs, rhs)
                else {
                    return None;
                };
                match op {
                    CompBinOp::Lt => lhs < rhs,
                    CompBinOp::LtEq => lhs <= rhs,
                    CompBinOp::Gt => lhs > rhs,
                    CompBinOp::GtEq => lhs >= rhs,
                    CompBinOp::Eq | CompBinOp::NotEq => unreachable!(),
                }
            }
            BinOp::Logical(LogicalBinOp::And | LogicalBinOp::Or) => {
                let (ProgramPointConstant::Bool(lhs), ProgramPointConstant::Bool(rhs)) = (lhs, rhs)
                else {
                    return None;
                };
                match op {
                    BinOp::Logical(LogicalBinOp::And) => lhs && rhs,
                    BinOp::Logical(LogicalBinOp::Or) => lhs || rhs,
                    _ => unreachable!(),
                }
            }
            BinOp::Arith(_) | BinOp::Index => return None,
        };
        Some(ProgramPointValue::Constant(ProgramPointConstant::Bool(
            value,
        )))
    }

    fn apply_expr_mutations(
        &self,
        state: &mut ProgramPointConstantState,
        expr: &NExpr<'db>,
        stmt: SStmtId,
    ) {
        let mutation_targets = self.expr_mutation_targets(state, expr);
        let rebound_carriers = self.expr_rebound_carrier_targets(state, expr);
        for local in mutation_targets {
            state
                .values
                .insert(local, ProgramPointValue::Mutation { stmt, local });
        }
        for local in rebound_carriers {
            // The replacement may select any mutable root available to the
            // callee. Drop stale projected children as well as the old summary:
            // retaining either could make a later store look exact again.
            self.set_alias_roots(state, local, ProgramPointAliasRoots::unknown());
        }
    }

    fn apply_stmt(&self, state: &mut ProgramPointConstantState, stmt: &super::ir::NSStmt<'db>) {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                let value = self
                    .expr_value(state, stmt.id, expr)
                    .unwrap_or(ProgramPointValue::Definition(stmt.id));
                self.apply_expr_mutations(state, expr, stmt.id);
                // Calls may replace mutable capabilities inside by-place
                // arguments. Resolve a call result against the post-call
                // carrier state instead of copying its stale input projection.
                let alias_roots = self.alias_roots_for_expr(state, expr, *dst);
                state.values.remove(dst);
                state.values.insert(*dst, value);
                self.set_alias_roots(state, *dst, alias_roots);
            }
            NSStmtKind::Store { dst, src } => {
                let value = state.values.get(&src.local).cloned();
                let source_alias_roots = self.alias_roots_for_local(state, src.local);
                let (targets, exact_whole_target) = self.store_value_targets(state, stmt.id, dst);
                for local in targets.iter().copied() {
                    state.values.insert(
                        local,
                        ProgramPointValue::Mutation {
                            stmt: stmt.id,
                            local,
                        },
                    );
                }
                if exact_whole_target {
                    let target = *targets
                        .iter()
                        .next()
                        .expect("an exact whole store has one target");
                    if let Some(value) = value {
                        state.values.insert(target, value);
                    }
                    self.set_alias_roots(state, target, source_alias_roots);
                } else if !source_alias_roots.is_empty() {
                    for target in targets {
                        if self.local_carries_mut_alias(target) {
                            let mut combined = self.alias_roots_for_local(state, target);
                            combined.extend(&source_alias_roots);
                            self.set_alias_roots(state, target, combined);
                        }
                    }
                }
            }
        }
    }

    fn transfer_state(
        &self,
        block: SBlockId,
        in_state: &ProgramPointConstantState,
        track_assign_result: &[bool],
        track_store_effect: &[bool],
    ) -> ProgramPointConstantState {
        let mut state = in_state.clone();
        for (stmt_idx, stmt) in self.body.blocks[block.index()].stmts.iter().enumerate() {
            if matches!(stmt.kind, NSStmtKind::Store { .. })
                && !track_store_effect.get(stmt_idx).copied().unwrap_or(true)
            {
                continue;
            }
            if matches!(stmt.kind, NSStmtKind::Assign { .. })
                && !track_assign_result.get(stmt_idx).copied().unwrap_or(true)
            {
                let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                    unreachable!()
                };
                // A dead result cannot transport alias provenance. Calls still
                // execute and may mutate their arguments or effect places.
                self.apply_expr_mutations(&mut state, expr, stmt.id);
                state.values.remove(dst);
                state.alias_roots.remove(dst);
            } else {
                self.apply_stmt(&mut state, stmt);
            }
        }
        state
    }
}

fn initial_program_point_state(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
) -> ProgramPointConstantState {
    let mut state = ProgramPointConstantState {
        reached: true,
        ..ProgramPointConstantState::default()
    };
    for local in body.entry_locals.iter().copied() {
        state.values.insert(local, ProgramPointValue::Entry(local));
        if body.local(local).is_some_and(|local| {
            borrow_results_in_ty(db, local.ty)
                .iter()
                .any(|result| result.kind == BorrowKind::Mut)
        }) {
            state
                .alias_roots
                .insert(local, ProgramPointAliasRoots::from_root(local));
            state.escaped_mut_roots.insert(local);
        }
    }
    state
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ProgramPointPhiKey {
    block: SBlockId,
    inputs: Vec<(Option<SBlockId>, ProgramPointValue)>,
}

#[derive(Default)]
struct ProgramPointPhiInterner {
    keys: Vec<ProgramPointPhiKey>,
    ids: FxHashMap<ProgramPointPhiKey, u32>,
    loop_keys: Vec<ProgramPointLoopPhiKey>,
    loop_ids: FxHashMap<ProgramPointLoopPhiKey, u32>,
}

impl ProgramPointPhiInterner {
    fn intern(&mut self, key: ProgramPointPhiKey) -> ProgramPointValue {
        if let Some(id) = self.ids.get(&key).copied() {
            return ProgramPointValue::Phi(id);
        }
        let id = u32::try_from(self.keys.len()).expect("program-point phi identity overflow");
        self.keys.push(key.clone());
        self.ids.insert(key, id);
        ProgramPointValue::Phi(id)
    }

    fn intern_loop(
        &mut self,
        block: Option<SBlockId>,
        local: SLocalId,
        inputs: impl IntoIterator<Item = ProgramPointValue>,
    ) -> ProgramPointValue {
        let mut components = BTreeSet::new();
        components.extend(block);
        for input in inputs {
            if let ProgramPointValue::LoopPhi(id) = input {
                components.extend(self.loop_keys[id as usize].components.iter().copied());
            }
        }
        debug_assert!(!components.is_empty());
        let key = ProgramPointLoopPhiKey { components, local };
        if let Some(id) = self.loop_ids.get(&key).copied() {
            return ProgramPointValue::LoopPhi(id);
        }
        let id =
            u32::try_from(self.loop_keys.len()).expect("program-point loop phi identity overflow");
        self.loop_keys.push(key.clone());
        self.loop_ids.insert(key, id);
        ProgramPointValue::LoopPhi(id)
    }

    fn loop_key(&self, value: ProgramPointValue) -> Option<&ProgramPointLoopPhiKey> {
        let ProgramPointValue::LoopPhi(id) = value else {
            return None;
        };
        self.loop_keys.get(id as usize)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ProgramPointLoopPhiKey {
    components: BTreeSet<SBlockId>,
    local: SLocalId,
}

struct ProgramPointValueSolution {
    entry_states: SecondaryMap<SBlockId, ProgramPointConstantState>,
    phi_keys: Vec<ProgramPointPhiKey>,
    loop_phi_keys: Vec<(ProgramPointValue, ProgramPointPhiKey)>,
}

fn project_program_point_values(
    state: &ProgramPointConstantState,
    live: &[SLocalId],
) -> ProgramPointConstantState {
    // Alias provenance is sparse, but one live carrier can reach otherwise
    // value-dead carrier state through storage edges. Unknown aliases can also
    // expand through every root that previously escaped. Retain exactly that
    // transitive alias graph instead of cloning all historical carrier temps
    // onto every edge.
    let mut retained_alias_roots = Vec::new();
    let mut pending = live
        .iter()
        .copied()
        .chain(state.escaped_mut_roots.iter().copied())
        .collect::<Vec<_>>();
    let mut visited = FxHashSet::default();
    while let Some(local) = pending.pop() {
        if !visited.insert(local) {
            continue;
        }
        let Some(roots) = state.alias_roots.get(&local) else {
            continue;
        };
        roots.collect_referenced_roots(&mut pending);
        retained_alias_roots.push((local, roots.clone()));
    }
    let mut alias_roots =
        FxHashMap::with_capacity_and_hasher(retained_alias_roots.len(), Default::default());
    alias_roots.extend(retained_alias_roots);
    let retained_values = live
        .iter()
        .filter_map(|local| {
            state
                .values
                .get(local)
                .copied()
                .map(|value| (*local, value))
        })
        .collect::<Vec<_>>();
    let mut values = FxHashMap::with_capacity_and_hasher(retained_values.len(), Default::default());
    values.extend(retained_values);
    ProgramPointConstantState {
        reached: state.reached,
        values,
        alias_roots,
        escaped_mut_roots: state.escaped_mut_roots.clone(),
    }
}

fn merge_program_point_inputs(
    analysis: &ProgramPointConstantAnalysis<'_, '_>,
    block: SBlockId,
    inputs: &[(Option<SBlockId>, &ProgramPointConstantState)],
    previous: &ProgramPointConstantState,
    live_locals: &[SLocalId],
    cyclic: bool,
    phis: &mut ProgramPointPhiInterner,
) -> ProgramPointConstantState {
    let Some((_, first)) = inputs.first() else {
        return ProgramPointConstantState::default();
    };
    let mut merged = ProgramPointConstantState {
        reached: true,
        values: first.values.clone(),
        alias_roots: FxHashMap::default(),
        escaped_mut_roots: FxHashSet::default(),
    };
    for (_, input) in inputs {
        merged
            .escaped_mut_roots
            .extend(input.escaped_mut_roots.iter().copied());
    }
    let mut alias_locals = FxHashSet::default();
    for (_, input) in inputs {
        alias_locals.extend(input.alias_roots.keys().copied());
        alias_locals.extend(
            input
                .values
                .keys()
                .copied()
                .filter(|local| analysis.local_carries_mut_alias(*local)),
        );
    }
    for local in alias_locals {
        if !analysis.local_carries_mut_alias(local) {
            continue;
        }
        let mut roots = ProgramPointAliasRoots::default();
        for (_, input) in inputs {
            if let Some(input_roots) = input.alias_roots.get(&local) {
                roots.extend(input_roots);
            } else {
                roots.unknown = true;
            }
        }
        if !roots.is_empty() {
            merged.alias_roots.insert(local, roots);
        }
    }
    merge_program_point_values(
        &mut merged.values,
        &previous.values,
        live_locals,
        block,
        inputs,
        cyclic,
        phis,
    );
    merged
}

fn merge_program_point_values(
    values: &mut FxHashMap<SLocalId, ProgramPointValue>,
    previous: &FxHashMap<SLocalId, ProgramPointValue>,
    live_locals: &[SLocalId],
    block: SBlockId,
    inputs: &[(Option<SBlockId>, &ProgramPointConstantState)],
    cyclic: bool,
    phis: &mut ProgramPointPhiInterner,
) {
    let mut cyclic_representatives = FxHashMap::<
        (
            Vec<(Option<SBlockId>, ProgramPointValue)>,
            Option<ProgramPointValue>,
        ),
        SLocalId,
    >::default();
    // Liveness already provides deterministic local order. Iterating it avoids
    // scanning all historical normalized temporaries at every merge while
    // preserving stable representatives for equal cyclic phi signatures.
    for local in live_locals.iter().copied() {
        if !values.contains_key(&local) {
            continue;
        }
        let value = values[&local];
        let mut complete = true;
        let mut all_equal = true;
        for (_, state) in inputs {
            let Some(incoming) = state.values.get(&local).copied() else {
                complete = false;
                break;
            };
            all_equal &= incoming == value;
        }
        if !complete {
            values.remove(&local);
            continue;
        }
        let previous_loop = previous
            .get(&local)
            .copied()
            .filter(|value| phis.loop_key(*value).is_some());
        if all_equal && (!cyclic || previous_loop.is_none() || previous_loop == Some(value)) {
            continue;
        }
        let incoming = inputs
            .iter()
            .map(|(predecessor, state)| (*predecessor, state.values[&local]))
            .collect::<Vec<_>>();
        let value = if cyclic {
            let representative = *cyclic_representatives
                .entry((incoming.clone(), previous_loop))
                .or_insert(local);
            phis.intern_loop(
                (!all_equal).then_some(block),
                representative,
                incoming
                    .iter()
                    .map(|(_, value)| *value)
                    .chain(previous_loop),
            )
        } else {
            phis.intern(ProgramPointPhiKey {
                block,
                inputs: incoming,
            })
        };
        values.insert(local, value);
    }
}

fn program_point_value_solution(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
    successors: &CfgAdjacency,
) -> ProgramPointValueSolution {
    let mut entry_states = SecondaryMap::new();
    entry_states.resize(body.blocks.len());
    if body.blocks.is_empty() {
        return ProgramPointValueSolution {
            entry_states,
            phi_keys: Vec::new(),
            loop_phi_keys: Vec::new(),
        };
    }
    let entry = SBlockId::new(0);
    let facts = NormalizedBodyFacts::new(body);
    let analysis = ProgramPointConstantAnalysis::new(db, body);
    let liveness = program_point_liveness(
        body,
        successors,
        &facts,
        &analysis,
        ProgramPointLivenessGoal::IndexIdentity,
    );
    let initial = project_program_point_values(
        &initial_program_point_state(db, body),
        &liveness.live_in[entry],
    );
    entry_states[entry] = initial.clone();
    let cyclic = cfg_cycle_blocks(successors);
    let predecessors = cfg_predecessor_indices(successors);
    let mut phis = ProgramPointPhiInterner::default();
    let mut edge_states = FxHashMap::<(SBlockId, SBlockId), ProgramPointConstantState>::default();
    let mut pending = VecDeque::from([entry]);
    let mut queued = vec![false; body.blocks.len()];
    queued[entry.index()] = true;
    while let Some(block) = pending.pop_front() {
        queued[block.index()] = false;
        let exit = analysis.transfer_state(
            block,
            &entry_states[block],
            &liveness.track_assign_result[block.index()],
            &liveness.track_store_effect[block.index()],
        );
        for successor in successors[block].iter().copied() {
            let edge = project_program_point_values(&exit, &liveness.live_in[successor]);
            let edge_changed = edge_states.get(&(block, successor)) != Some(&edge);
            if !edge_changed {
                continue;
            }
            edge_states.insert((block, successor), edge);
            let mut inputs = Vec::new();
            if successor == entry {
                inputs.push((None, &initial));
            }
            for predecessor in predecessors[successor].iter().copied() {
                if let Some(state) = edge_states.get(&(predecessor, successor)) {
                    inputs.push((Some(predecessor), state));
                }
            }
            let merged = merge_program_point_inputs(
                &analysis,
                successor,
                &inputs,
                &entry_states[successor],
                &liveness.live_in[successor],
                cyclic.contains(&successor),
                &mut phis,
            );
            if entry_states[successor] != merged && !queued[successor.index()] {
                entry_states[successor] = merged;
                pending.push_back(successor);
                queued[successor.index()] = true;
            } else if entry_states[successor] != merged {
                entry_states[successor] = merged;
            }
        }
    }
    let mut loop_phi_keys = Vec::new();
    let mut recorded_loop_phis = FxHashSet::default();
    for block in cyclic.iter().copied() {
        let mut loop_phis = entry_states[block]
            .values
            .values()
            .copied()
            .filter(|value| {
                phis.loop_key(*value)
                    .is_some_and(|key| key.components.contains(&block))
            })
            .collect::<Vec<_>>();
        loop_phis.sort_unstable_by_key(|value| {
            phis.loop_key(*value)
                .expect("filtered loop phi")
                .local
                .index()
        });
        for result in loop_phis {
            if !recorded_loop_phis.insert((block, result)) {
                continue;
            }
            let representative = phis.loop_key(result).expect("loop phi key").local;
            let mut inputs = Vec::new();
            if block == entry
                && let Some(value) = initial.values.get(&representative).copied()
            {
                inputs.push((None, value));
            }
            for predecessor_idx in 0..body.blocks.len() {
                let predecessor = SBlockId::new(predecessor_idx);
                if let Some(value) = edge_states
                    .get(&(predecessor, block))
                    .and_then(|state| state.values.get(&representative))
                    .copied()
                {
                    inputs.push((Some(predecessor), value));
                }
            }
            if !inputs.is_empty() {
                loop_phi_keys.push((result, ProgramPointPhiKey { block, inputs }));
            }
        }
    }
    ProgramPointValueSolution {
        entry_states,
        phi_keys: phis.keys,
        loop_phi_keys,
    }
}

fn mutated_place_root_local(
    body: &NormalizedSemanticBody<'_>,
    place: &NSPlace<'_>,
) -> Option<SLocalId> {
    match place.root {
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { local, .. }) | Some(NBorrowRoot::LocalSlot { local }) => {
                Some(*local)
            }
            Some(NBorrowRoot::Provider { .. }) | None => None,
        },
        NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
    }
}

fn projection_uses_only_immutable_capabilities(
    db: &dyn HirAnalysisDb,
    root_ty: TyId<'_>,
    path: &ProjectionPath<TyId<'_>, crate::analysis::semantic::VariantIndex, SLocalId>,
) -> Option<bool> {
    let mut prefix = ProjectionPath::new();
    for projection in path.iter() {
        let (mut ty, _) = semantic_projection_ty(db, root_ty, &prefix)?;
        while let Some((kind, inner)) = ty.as_capability(db) {
            if kind == CapabilityKind::Mut {
                return Some(false);
            }
            ty = inner;
        }
        prefix.push(projection.clone());
    }
    let (mut ty, _) = semantic_projection_ty(db, root_ty, &prefix)?;
    while let Some((kind, inner)) = ty.as_capability(db) {
        if kind == CapabilityKind::Mut {
            return Some(false);
        }
        ty = inner;
    }
    Some(true)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
enum StableIndexValueState {
    #[default]
    Unresolved,
    Known(ProgramPointValue),
    NotStable,
}

struct ReachableIndexFacts {
    constant_indices: SecondaryMap<SLocalId, Option<usize>>,
    value_identities: SecondaryMap<SLocalId, Option<SLocalId>>,
    phi_edge_substitutions: IndexPhiEdgeSubstitutions,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum IndexPhiSource {
    Constant(usize),
    Dynamic(SLocalId),
}

#[derive(Default)]
pub(super) struct IndexPhiEdgeSubstitution {
    pub(super) results: FxHashSet<SLocalId>,
    pub(super) replacements: FxHashMap<IndexPhiSource, FxHashSet<SLocalId>>,
}

type IndexPhiEdgeSubstitutions = FxHashMap<(SBlockId, SBlockId), IndexPhiEdgeSubstitution>;

fn reachable_index_facts<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    successors: &CfgAdjacency,
) -> ReachableIndexFacts {
    let analysis = ProgramPointConstantAnalysis::new(db, body);
    let solution = program_point_value_solution(db, body, successors);
    let entry_states = &solution.entry_states;
    let mut candidates = SecondaryMap::new();
    candidates.resize(body.locals.len());
    for local in body.entry_locals.iter().copied() {
        candidates[local] = StableIndexValueState::Known(ProgramPointValue::Entry(local));
    }

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let block_id = SBlockId::new(block_idx);
        let mut state = entry_states[block_id].clone();
        if !state.reached {
            continue;
        }
        for stmt in &block.stmts {
            match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => {
                    let value = analysis
                        .expr_value(&state, stmt.id, expr)
                        .unwrap_or(ProgramPointValue::Definition(stmt.id));
                    candidates[*dst] = merge_stable_index_value(
                        std::mem::take(&mut candidates[*dst]),
                        Some(value),
                    );
                    for local in analysis.expr_mutation_targets(&state, expr) {
                        candidates[local] = StableIndexValueState::NotStable;
                    }
                }
                NSStmtKind::Store { dst, src } => {
                    let (targets, exact_whole_target) =
                        analysis.store_value_targets(&state, stmt.id, dst);
                    if exact_whole_target {
                        let target = *targets
                            .iter()
                            .next()
                            .expect("an exact whole store has one target");
                        candidates[target] = merge_stable_index_value(
                            std::mem::take(&mut candidates[target]),
                            state.values.get(&src.local).cloned(),
                        );
                    } else {
                        for local in targets {
                            candidates[local] = StableIndexValueState::NotStable;
                        }
                    }
                }
            }
            analysis.apply_stmt(&mut state, stmt);
        }
    }

    let mut constant_indices = SecondaryMap::new();
    constant_indices.resize(body.locals.len());
    let mut value_identities = SecondaryMap::new();
    value_identities.resize(body.locals.len());
    let mut representatives = FxHashMap::<ProgramPointValue, SLocalId>::default();
    for (local, candidate) in candidates.iter() {
        if let StableIndexValueState::Known(value) = candidate {
            if let Some(ProgramPointConstant::Index(index)) = (*value).as_constant() {
                constant_indices[local] = Some(index);
            }
            let representative = *representatives.entry(*value).or_insert(local);
            value_identities[local] = Some(representative);
        }
    }
    let mut phi_edge_substitutions = IndexPhiEdgeSubstitutions::default();
    let mut record_phi_substitutions = |result: ProgramPointValue, phi: &ProgramPointPhiKey| {
        let Some(&result_local) = representatives.get(&result) else {
            return;
        };
        for (predecessor, input) in &phi.inputs {
            let Some(predecessor) = *predecessor else {
                continue;
            };
            let substitutions = phi_edge_substitutions
                .entry((predecessor, phi.block))
                .or_default();
            substitutions.results.insert(result_local);
            let input = match input {
                ProgramPointValue::Constant(ProgramPointConstant::Index(index)) => {
                    IndexPhiSource::Constant(*index)
                }
                _ => {
                    let Some(&input_local) = representatives.get(input) else {
                        continue;
                    };
                    IndexPhiSource::Dynamic(input_local)
                }
            };
            substitutions
                .replacements
                .entry(input)
                .or_default()
                .insert(result_local);
        }
    };
    for (id, phi) in solution.phi_keys.iter().enumerate() {
        let result =
            ProgramPointValue::Phi(u32::try_from(id).expect("program-point phi identity overflow"));
        record_phi_substitutions(result, phi);
    }
    for (result, phi) in &solution.loop_phi_keys {
        record_phi_substitutions(*result, phi);
    }
    ReachableIndexFacts {
        constant_indices,
        value_identities,
        phi_edge_substitutions,
    }
}

fn merge_stable_index_value(
    current: StableIndexValueState,
    candidate: Option<ProgramPointValue>,
) -> StableIndexValueState {
    let Some(candidate) = candidate else {
        return StableIndexValueState::NotStable;
    };
    match current {
        StableIndexValueState::Unresolved => StableIndexValueState::Known(candidate),
        StableIndexValueState::Known(current) if current == candidate => {
            StableIndexValueState::Known(current)
        }
        StableIndexValueState::Known(_) | StableIndexValueState::NotStable => {
            StableIndexValueState::NotStable
        }
    }
}

fn cfg_backedges(successors: &CfgAdjacency) -> FxHashSet<(SBlockId, SBlockId)> {
    fn visit(
        block: SBlockId,
        successors: &CfgAdjacency,
        colors: &mut [u8],
        backedges: &mut FxHashSet<(SBlockId, SBlockId)>,
    ) {
        colors[block.index()] = 1;
        for &successor in &successors[block] {
            match colors[successor.index()] {
                0 => visit(successor, successors, colors, backedges),
                1 => {
                    backedges.insert((block, successor));
                }
                2 => {}
                _ => unreachable!(),
            }
        }
        colors[block.index()] = 2;
    }

    let block_count = successors.iter().count();
    let mut colors = vec![0; block_count];
    let mut backedges = FxHashSet::default();
    for idx in 0..block_count {
        if colors[idx] == 0 {
            visit(SBlockId::new(idx), successors, &mut colors, &mut backedges);
        }
    }
    backedges
}

fn cfg_cycle_blocks(successors: &CfgAdjacency) -> FxHashSet<SBlockId> {
    let mut cyclic = FxHashSet::default();
    for (start, _) in successors.iter() {
        let mut seen = FxHashSet::default();
        let mut pending = successors[start].iter().copied().collect::<VecDeque<_>>();
        while let Some(block) = pending.pop_front() {
            if block == start {
                cyclic.insert(start);
                break;
            }
            if seen.insert(block) {
                pending.extend(successors[block].iter().copied());
            }
        }
    }
    cyclic
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
) -> Option<FamilyBindings> {
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

fn return_projection_to_layout_path(
    projection: &[ReturnProjectionStep],
) -> Vec<LayoutBackingProjection> {
    projection
        .iter()
        .map(|step| match step {
            ReturnProjectionStep::Field(field) => {
                LayoutBackingProjection::Field(FieldIndex(*field))
            }
            ReturnProjectionStep::VariantField { variant, field } => {
                LayoutBackingProjection::VariantField {
                    variant: crate::analysis::semantic::VariantIndex(*variant),
                    field: FieldIndex(*field),
                }
            }
            ReturnProjectionStep::ConstantIndex(index) => {
                LayoutBackingProjection::Index(Some(*index))
            }
            ReturnProjectionStep::DynamicIndex(_) | ReturnProjectionStep::AnyIndex => {
                LayoutBackingProjection::Index(None)
            }
        })
        .collect()
}

fn return_projection_result_suffix(
    source: &[ReturnProjectionStep],
    result: &[LayoutBackingProjection],
) -> Option<Vec<LayoutBackingProjection>> {
    if source.len() > result.len()
        || !source
            .iter()
            .zip(result)
            .all(|(source, result)| match (source, result) {
                (ReturnProjectionStep::Field(source), LayoutBackingProjection::Field(result)) => {
                    *source == result.0
                }
                (
                    ReturnProjectionStep::VariantField {
                        variant: source_variant,
                        field: source_field,
                    },
                    LayoutBackingProjection::VariantField {
                        variant: result_variant,
                        field: result_field,
                    },
                ) => *source_variant == result_variant.0 && *source_field == result_field.0,
                (
                    ReturnProjectionStep::ConstantIndex(source),
                    LayoutBackingProjection::Index(Some(result)),
                ) => source == result,
                (
                    ReturnProjectionStep::ConstantIndex(_)
                    | ReturnProjectionStep::DynamicIndex(_)
                    | ReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_),
                )
                | (
                    ReturnProjectionStep::DynamicIndex(_) | ReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(Some(_)),
                ) => true,
                _ => false,
            })
    {
        return None;
    }
    Some(result[source.len()..].to_vec())
}

fn call_return_projection_result_suffix(
    source: &[SCallReturnProjectionStep],
    result: &[LayoutBackingProjection],
) -> Option<Vec<LayoutBackingProjection>> {
    if source.len() > result.len()
        || !source
            .iter()
            .zip(result)
            .all(|(source, result)| match (source, result) {
                (
                    SCallReturnProjectionStep::Field(source),
                    LayoutBackingProjection::Field(result),
                ) => *source == result.0,
                (
                    SCallReturnProjectionStep::VariantField {
                        variant: source_variant,
                        field: source_field,
                    },
                    LayoutBackingProjection::VariantField {
                        variant: result_variant,
                        field: result_field,
                    },
                ) => *source_variant == result_variant.0 && *source_field == result_field.0,
                (
                    SCallReturnProjectionStep::ConstantIndex(source),
                    LayoutBackingProjection::Index(Some(result)),
                ) => source == result,
                (
                    SCallReturnProjectionStep::ConstantIndex(_)
                    | SCallReturnProjectionStep::DynamicIndex(_)
                    | SCallReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_),
                )
                | (
                    SCallReturnProjectionStep::DynamicIndex(_)
                    | SCallReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(Some(_)),
                ) => true,
                _ => false,
            })
    {
        return None;
    }
    Some(result[source.len()..].to_vec())
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
    let inputs = instance
        .key(db)
        .callable_body(db)
        .param_bindings(db)
        .into_iter()
        .filter_map(|binding| match binding {
            LocalBinding::Param {
                idx, ty, is_mut, ..
            } => u32::try_from(idx).ok().map(|idx| {
                let ty = instance.normalized_ty(db, ty);
                let borrow_results = borrow_results_in_ty_with_family_ids(db, ty, &mut family_ids);
                (idx, ty, is_mut, borrow_results)
            }),
            LocalBinding::Local { .. } | LocalBinding::EffectParam { .. } => None,
        })
        .collect::<Vec<_>>();
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

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use cranelift_entity::EntityRef;
    use rustc_hash::FxHashMap;

    use super::{
        ProgramPointAliasProjection, ProgramPointAliasRoots, ProgramPointConstant,
        ProgramPointConstantState, ProgramPointMutAliasCache, ProgramPointPhiInterner,
        ProgramPointValue, SBlockId, SLocalId, SStmtId, merge_program_point_values,
        project_program_point_values, projected_index_alias_roots,
    };

    #[test]
    fn program_point_mut_alias_classification_is_computed_once_per_local() {
        let classifications = Cell::new(0);
        let cache = ProgramPointMutAliasCache::new(4, |local| {
            classifications.set(classifications.get() + 1);
            local.index() % 2 == 0
        });

        assert_eq!(classifications.get(), 4);
        for _ in 0..100 {
            assert!(cache.carries(SLocalId::new(0)));
            assert!(!cache.carries(SLocalId::new(1)));
            assert!(cache.carries(SLocalId::new(2)));
            assert!(!cache.carries(SLocalId::new(3)));
        }
        assert_eq!(classifications.get(), 4);
    }

    #[test]
    fn streamed_program_point_value_merge_preserves_phi_identity_and_missing_inputs() {
        let first_predecessor = SBlockId::new(0);
        let second_predecessor = SBlockId::new(1);
        let join = SBlockId::new(2);
        let first = SLocalId::new(0);
        let second = SLocalId::new(1);
        let missing = SLocalId::new(2);
        let entry_value = ProgramPointValue::Entry(SLocalId::new(3));
        let definition_value = ProgramPointValue::Definition(SStmtId::new(0));
        let first_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([
                (first, entry_value),
                (second, entry_value),
                (missing, entry_value),
            ]),
            ..ProgramPointConstantState::default()
        };
        let second_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(first, definition_value), (second, definition_value)]),
            ..ProgramPointConstantState::default()
        };
        let inputs = [
            (Some(first_predecessor), &first_state),
            (Some(second_predecessor), &second_state),
        ];

        let mut phis = ProgramPointPhiInterner::default();
        let mut cyclic_values = first_state.values.clone();
        merge_program_point_values(
            &mut cyclic_values,
            &FxHashMap::default(),
            &[first, second, missing],
            join,
            &inputs,
            true,
            &mut phis,
        );
        let loop_phi = ProgramPointValue::LoopPhi(0);
        assert_eq!(cyclic_values.get(&first), Some(&loop_phi));
        assert_eq!(cyclic_values.get(&second), Some(&loop_phi));
        assert!(!cyclic_values.contains_key(&missing));
        let loop_key = phis.loop_key(loop_phi).expect("interned loop phi");
        assert_eq!(
            loop_key.components.iter().copied().collect::<Vec<_>>(),
            [join]
        );
        assert_eq!(loop_key.local, first);

        let mut acyclic_values = first_state.values.clone();
        merge_program_point_values(
            &mut acyclic_values,
            &FxHashMap::default(),
            &[first, second, missing],
            join,
            &inputs,
            false,
            &mut ProgramPointPhiInterner::default(),
        );
        assert_eq!(
            acyclic_values.get(&first),
            acyclic_values.get(&second),
            "locals with the same incoming signature must share one phi"
        );
        assert!(matches!(
            acyclic_values.get(&first),
            Some(ProgramPointValue::Phi(_))
        ));
        assert!(!acyclic_values.contains_key(&missing));
    }

    #[test]
    fn program_point_nested_loop_phi_components_converge_without_false_identity_merges() {
        let outer = SBlockId::new(4);
        let inner = SBlockId::new(13);
        let first_predecessor = SBlockId::new(0);
        let second_predecessor = SBlockId::new(1);
        let local = SLocalId::new(0);
        let initial = ProgramPointValue::Entry(SLocalId::new(10));
        let changed = ProgramPointValue::Definition(SStmtId::new(0));
        let mut phis = ProgramPointPhiInterner::default();

        let initial_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(local, initial)]),
            ..ProgramPointConstantState::default()
        };
        let changed_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(local, changed)]),
            ..ProgramPointConstantState::default()
        };
        let mut outer_values = initial_state.values.clone();
        merge_program_point_values(
            &mut outer_values,
            &FxHashMap::default(),
            &[local],
            outer,
            &[
                (Some(first_predecessor), &initial_state),
                (Some(second_predecessor), &changed_state),
            ],
            true,
            &mut phis,
        );
        let outer_phi = outer_values[&local];
        assert_eq!(
            phis.loop_key(outer_phi)
                .expect("outer loop phi")
                .components
                .iter()
                .copied()
                .collect::<Vec<_>>(),
            [outer]
        );

        let outer_phi_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(local, outer_phi)]),
            ..ProgramPointConstantState::default()
        };
        let mut nested_values = outer_phi_state.values.clone();
        merge_program_point_values(
            &mut nested_values,
            &FxHashMap::default(),
            &[local],
            inner,
            &[
                (Some(first_predecessor), &outer_phi_state),
                (Some(second_predecessor), &changed_state),
            ],
            true,
            &mut phis,
        );
        let nested_phi = nested_values[&local];
        let nested_key = phis.loop_key(nested_phi).expect("nested loop phi");
        assert_eq!(
            nested_key.components.iter().copied().collect::<Vec<_>>(),
            [outer, inner]
        );

        let nested_phi_state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(local, nested_phi)]),
            ..ProgramPointConstantState::default()
        };
        let mut staged_values = outer_phi_state.values.clone();
        merge_program_point_values(
            &mut staged_values,
            &nested_phi_state.values,
            &[local],
            inner,
            &[
                (Some(first_predecessor), &outer_phi_state),
                (Some(second_predecessor), &outer_phi_state),
            ],
            true,
            &mut phis,
        );
        assert_eq!(
            staged_values[&local], nested_phi,
            "temporarily agreeing predecessor states must not retract a widened loop identity"
        );

        let loop_key_count = phis.loop_keys.len();
        let mut agreeing_values = outer_phi_state.values.clone();
        merge_program_point_values(
            &mut agreeing_values,
            &FxHashMap::default(),
            &[local],
            SBlockId::new(20),
            &[
                (Some(first_predecessor), &outer_phi_state),
                (Some(second_predecessor), &outer_phi_state),
            ],
            true,
            &mut phis,
        );
        assert_eq!(agreeing_values[&local], outer_phi);
        assert_eq!(
            phis.loop_keys.len(),
            loop_key_count,
            "genuinely agreeing inputs must not acquire a spurious loop component"
        );

        let mut outer_again = initial_state.values.clone();
        merge_program_point_values(
            &mut outer_again,
            &FxHashMap::default(),
            &[local],
            outer,
            &[
                (Some(first_predecessor), &initial_state),
                (Some(second_predecessor), &nested_phi_state),
            ],
            true,
            &mut phis,
        );
        assert_eq!(outer_again[&local], nested_phi);

        let mut inner_again = outer_again.clone();
        merge_program_point_values(
            &mut inner_again,
            &FxHashMap::default(),
            &[local],
            inner,
            &[
                (Some(first_predecessor), &nested_phi_state),
                (Some(second_predecessor), &changed_state),
            ],
            true,
            &mut phis,
        );
        assert_eq!(inner_again[&local], nested_phi);

        let left = SLocalId::new(1);
        let right = SLocalId::new(2);
        let left_entry = ProgramPointValue::Entry(left);
        let right_entry = ProgramPointValue::Entry(right);
        let distinct_first = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(left, left_entry), (right, right_entry)]),
            ..ProgramPointConstantState::default()
        };
        let distinct_second = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([
                (left, ProgramPointValue::Definition(SStmtId::new(1))),
                (right, ProgramPointValue::Definition(SStmtId::new(2))),
            ]),
            ..ProgramPointConstantState::default()
        };
        let mut distinct_values = distinct_first.values.clone();
        merge_program_point_values(
            &mut distinct_values,
            &FxHashMap::default(),
            &[left, right],
            inner,
            &[
                (Some(first_predecessor), &distinct_first),
                (Some(second_predecessor), &distinct_second),
            ],
            true,
            &mut phis,
        );
        assert_ne!(distinct_values[&left], distinct_values[&right]);

        let swapped_second = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([(left, right_entry), (right, left_entry)]),
            ..ProgramPointConstantState::default()
        };
        let mut swapped_values = distinct_first.values.clone();
        merge_program_point_values(
            &mut swapped_values,
            &FxHashMap::default(),
            &[left, right],
            inner,
            &[
                (Some(first_predecessor), &distinct_first),
                (Some(second_predecessor), &swapped_second),
            ],
            true,
            &mut phis,
        );
        assert_ne!(swapped_values[&left], swapped_values[&right]);
    }

    #[test]
    fn program_point_edge_projection_keeps_only_live_and_escaped_alias_closure() {
        let live = SLocalId::new(0);
        let dead_alias = SLocalId::new(1);
        let escaped = SLocalId::new(2);
        let nested = SLocalId::new(3);
        let root = SLocalId::new(4);
        let state = ProgramPointConstantState {
            reached: true,
            values: FxHashMap::from_iter([
                (live, ProgramPointValue::Entry(live)),
                (dead_alias, ProgramPointValue::Entry(dead_alias)),
            ]),
            alias_roots: FxHashMap::from_iter([
                (live, ProgramPointAliasRoots::from_root(root)),
                (dead_alias, ProgramPointAliasRoots::from_root(root)),
                (escaped, ProgramPointAliasRoots::from_root(nested)),
                (nested, ProgramPointAliasRoots::from_root(root)),
            ]),
            escaped_mut_roots: [escaped].into_iter().collect(),
        };

        let projected = project_program_point_values(&state, &[live]);
        assert_eq!(
            projected.values,
            FxHashMap::from_iter([(live, ProgramPointValue::Entry(live))])
        );
        assert_eq!(
            projected.alias_roots,
            FxHashMap::from_iter([
                (live, ProgramPointAliasRoots::from_root(root)),
                (escaped, ProgramPointAliasRoots::from_root(nested)),
                (nested, ProgramPointAliasRoots::from_root(root)),
            ])
        );
        assert!(!projected.alias_roots.contains_key(&dead_alias));
        assert_eq!(projected.escaped_mut_roots, state.escaped_mut_roots);
    }

    #[test]
    fn program_point_index_projection_joins_every_overlapping_alias_child() {
        let exact_root = SLocalId::new(0);
        let dynamic_root = SLocalId::new(1);
        let any_root = SLocalId::new(2);
        let other_root = SLocalId::new(3);
        let dynamic_index = SLocalId::new(4);
        let mut roots = ProgramPointAliasRoots::unknown();
        roots.children.insert(
            ProgramPointAliasProjection::ConstantIndex(0),
            ProgramPointAliasRoots::from_root(exact_root),
        );
        roots.children.insert(
            ProgramPointAliasProjection::DynamicIndex(dynamic_index),
            ProgramPointAliasRoots::from_root(dynamic_root),
        );
        roots.children.insert(
            ProgramPointAliasProjection::Any,
            ProgramPointAliasRoots::from_root(any_root),
        );
        roots.children.insert(
            ProgramPointAliasProjection::ConstantIndex(1),
            ProgramPointAliasRoots::from_root(other_root),
        );

        let unknown_index_state = ProgramPointConstantState {
            values: FxHashMap::from_iter([(
                dynamic_index,
                ProgramPointValue::Definition(SStmtId::new(0)),
            )]),
            ..ProgramPointConstantState::default()
        };
        let selected = projected_index_alias_roots(&unknown_index_state, &roots, Some(0)).unwrap();
        assert!(
            selected.unknown,
            "parent uncertainty must survive projection"
        );
        assert_eq!(
            selected.roots,
            [exact_root, dynamic_root, any_root].into_iter().collect()
        );

        let known_other_state = ProgramPointConstantState {
            values: FxHashMap::from_iter([(
                dynamic_index,
                ProgramPointValue::Constant(ProgramPointConstant::Index(1)),
            )]),
            ..ProgramPointConstantState::default()
        };
        let selected = projected_index_alias_roots(&known_other_state, &roots, Some(0)).unwrap();
        assert_eq!(
            selected.roots,
            [exact_root, any_root].into_iter().collect(),
            "a dynamic child may be excluded only when its index is known unequal"
        );

        let selected = projected_index_alias_roots(&known_other_state, &roots, None).unwrap();
        assert_eq!(
            selected.roots,
            [exact_root, dynamic_root, any_root, other_root]
                .into_iter()
                .collect()
        );
    }
}
