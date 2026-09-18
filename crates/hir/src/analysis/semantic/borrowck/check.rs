use std::fmt;

use super::{
    ir::{
        BlockedSemanticBody, BorrowDiagnosticId, BorrowSummary, BorrowSummaryId,
        SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, SemanticBorrowSummaryResult, SemanticNormalizationFailure,
    },
    solver::{BorrowSummaryMode, Borrowck},
    summary::signature_summary,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SemOrigin, SemanticInstance, get_or_build_semantic_instance,
            identity_semantic_instance_key, normalized::normalize_semantic_body_provisional,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::{ItemKind, TopLevelMod},
};
use common::diagnostics::CompleteDiagnostic;
use rustc_hash::FxHashSet;

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
    cached_borrow_summary_result(db, borrowck.borrow_summary())
}

#[salsa::tracked(
    cycle_fn=provisional_borrow_summary_cycle_recover,
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
    cached_borrow_summary_result(db, borrowck.borrow_summary())
}

fn blocked_signature_borrow_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: BlockedSemanticBody<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    match signature_summary(db, instance, true) {
        Ok(summary) => SemanticBorrowSummaryResult::Blocked {
            body,
            summary: Some(BorrowSummaryId::new(db, summary)),
        },
        Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
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
) -> Result<Option<BorrowSummary<'db>>, SemanticAnalysisError<'db>> {
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
    pub(super) summary: Option<BorrowSummary<'db>>,
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
    match super::boundary::semantic_boundary_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => {}
        SemanticBorrowCheckResult::Blocked(_) => {}
        SemanticBorrowCheckResult::Err(diag) if seen_diags.insert(diag) => {
            diags.push(Box::new(diag));
        }
        SemanticBorrowCheckResult::Err(_) => {}
    }
}

pub(super) struct BorrowSummaryComputation<'db> {
    pub summary: Option<BorrowSummary<'db>>,
    pub blocked: Option<BlockedSemanticBody<'db>>,
}

fn semantic_borrow_summary_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    match signature_summary(db, instance, false) {
        Ok(summary) => SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(db, summary))),
        Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

fn semantic_borrow_summary_cycle_recover<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &SemanticBorrowSummaryResult<'db>,
    count: u32,
    instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    if count >= 16 && !matches!(value, SemanticBorrowSummaryResult::Err(_)) {
        // A signature-only fallback cannot describe body-dependent boundary
        // requirements. Do not silently discharge those proofs on a cycle.
        let owner = instance.key(db).owner(db);
        let diagnostic = SemanticBorrowDiagnostic::new(
            instance,
            SemanticBorrowDiagKind::TransportViolation,
            "recursive boundary requirements did not converge".into(),
            SemanticBorrowDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        );
        return salsa::CycleRecoveryAction::Fallback(SemanticBorrowSummaryResult::Err(
            BorrowDiagnosticId::new(db, diagnostic),
        ));
    }
    provisional_borrow_summary_cycle_recover(db, value, count, instance)
}

fn provisional_borrow_summary_cycle_recover<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &SemanticBorrowSummaryResult<'db>,
    count: u32,
    instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    if matches!(value, SemanticBorrowSummaryResult::Err(_)) {
        return salsa::CycleRecoveryAction::Fallback(value.clone());
    }
    if count >= 16 {
        let result = match signature_summary(db, instance, true) {
            Ok(summary) => match value {
                SemanticBorrowSummaryResult::Blocked { body, .. } => {
                    SemanticBorrowSummaryResult::Blocked {
                        body: body.clone(),
                        summary: Some(BorrowSummaryId::new(db, summary)),
                    }
                }
                _ => SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(db, summary))),
            },
            Err(diag) => SemanticBorrowSummaryResult::Err(BorrowDiagnosticId::new(db, diag)),
        };
        return salsa::CycleRecoveryAction::Fallback(result);
    }
    salsa::CycleRecoveryAction::Iterate
}
