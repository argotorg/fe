use crate::analysis::semantic::diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
    SemanticDiagnosticSpan, SemanticNormalizationFailure,
};
use std::fmt;

use super::{
    ir::{
        BorrowSummary, BorrowSummaryId, PendingSemanticValidation, SemanticBorrowCheckResult,
        SemanticBorrowSummaryResult,
    },
    solver::{BorrowSummaryMode, Borrowck},
    summary::signature_summary,
    validation::can_specialize,
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
            return SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag));
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
            return SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag));
        }
    };
    let borrowck = match Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)
    {
        Ok(borrowck) => borrowck,
        Err(diag) => return SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag)),
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
        Err(diag) => SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag)),
    }
}

fn cached_borrow_summary_result<'db>(
    db: &'db dyn HirAnalysisDb,
    result: Result<BorrowSummaryComputation<'db>, SemanticDiagnostic<'db>>,
) -> SemanticBorrowSummaryResult<'db> {
    match result {
        Ok(BorrowSummaryComputation {
            summary,
            blocked: Some(body),
            ..
        }) => SemanticBorrowSummaryResult::Blocked {
            body,
            summary: summary.map(|summary| BorrowSummaryId::new(db, summary)),
        },
        Ok(BorrowSummaryComputation {
            summary,
            blocked: None,
            pending,
        }) if !pending.callees.is_empty() => SemanticBorrowSummaryResult::Pending {
            validation: pending,
            summary: summary.map(|summary| BorrowSummaryId::new(db, summary)),
        },
        Ok(BorrowSummaryComputation {
            summary,
            blocked: None,
            ..
        }) => SemanticBorrowSummaryResult::Ok(
            summary.map(|summary| BorrowSummaryId::new(db, summary)),
        ),
        Err(diag) => SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag)),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticAnalysisError<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Pending(PendingSemanticValidation<'db>),
    Diagnostic(CompleteDiagnostic),
}

impl SemanticAnalysisError<'_> {
    pub fn diagnostic(&self) -> Option<&CompleteDiagnostic> {
        match self {
            Self::Blocked(_) | Self::Pending(_) => None,
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
            Self::Pending(validation) => write!(
                formatter,
                "semantic validation requires concrete implementations for {} opaque callees",
                validation.callees.len()
            ),
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
        SemanticBorrowSummaryResult::Pending { validation, .. } => {
            Err(SemanticAnalysisError::Pending(validation))
        }
        SemanticBorrowSummaryResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

pub(super) struct BorrowSummaryVoucher<'db> {
    pub(super) summary: Option<BorrowSummary<'db>>,
    pub(super) blocked: Option<BlockedSemanticBody<'db>>,
    pub(super) pending: PendingSemanticValidation<'db>,
}

pub(super) fn semantic_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<BorrowSummaryVoucher<'db>, SemanticDiagnostic<'db>> {
    match semantic_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
            pending: Default::default(),
        }),
        SemanticBorrowSummaryResult::Blocked { body, summary } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: Some(body),
            pending: Default::default(),
        }),
        SemanticBorrowSummaryResult::Pending {
            validation,
            summary,
        } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
            pending: validation,
        }),
        SemanticBorrowSummaryResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub(super) fn provisional_borrow_summary_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<BorrowSummaryVoucher<'db>, SemanticDiagnostic<'db>> {
    match provisional_borrow_summary_query(db, instance) {
        SemanticBorrowSummaryResult::Ok(summary) => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
            pending: Default::default(),
        }),
        SemanticBorrowSummaryResult::Blocked { body, summary } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: Some(body),
            pending: Default::default(),
        }),
        SemanticBorrowSummaryResult::Pending {
            validation,
            summary,
        } => Ok(BorrowSummaryVoucher {
            summary: summary.map(|summary| summary.items(db).clone()),
            blocked: None,
            pending: validation,
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
        SemanticBorrowCheckResult::Pending(validation) => {
            Err(SemanticAnalysisError::Pending(validation))
        }
        SemanticBorrowCheckResult::Blocked(body) => Err(SemanticAnalysisError::Blocked(body)),
        SemanticBorrowCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

#[salsa::tracked(
    cycle_fn=semantic_borrow_check_cycle_recover,
    cycle_initial=semantic_borrow_check_cycle_initial
)]
fn semantic_borrow_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    let mut borrowck = match Borrowck::new(db, instance) {
        Ok(borrowck) => borrowck,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return SemanticBorrowCheckResult::Blocked(blocked);
        }
        Err(SemanticNormalizationFailure::InternalFailure(diag)) => {
            return SemanticBorrowCheckResult::Err(SemanticDiagnosticId::new(db, diag));
        }
    };
    match borrowck.check() {
        Ok(Some(blocked)) => return SemanticBorrowCheckResult::Blocked(blocked),
        Err(diag) => return SemanticBorrowCheckResult::Err(SemanticDiagnosticId::new(db, diag)),
        Ok(None) => {}
    }
    // Summaries describe effects and boundary requirements. Local loan conflicts
    // are a separate validation: check every resolved implementation transitively
    // without making a boundary-only query depend on borrow diagnostics.
    for call in borrowck.calls.values().filter(|call| !call.pending) {
        match semantic_borrow_check_query(db, call.instance) {
            SemanticBorrowCheckResult::Ok => {}
            SemanticBorrowCheckResult::Pending(validation) => {
                borrowck.pending.callees.extend(validation.callees);
            }
            result
            @ (SemanticBorrowCheckResult::Blocked(_) | SemanticBorrowCheckResult::Err(_)) => {
                return result;
            }
        }
    }
    if borrowck.pending.callees.is_empty() {
        SemanticBorrowCheckResult::Ok
    } else {
        SemanticBorrowCheckResult::Pending(borrowck.pending)
    }
}

fn semantic_borrow_check_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    SemanticBorrowCheckResult::Ok
}

fn semantic_borrow_check_cycle_recover<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &SemanticBorrowCheckResult<'db>,
    count: u32,
    instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowCheckResult<'db>> {
    if matches!(
        value,
        SemanticBorrowCheckResult::Err(_) | SemanticBorrowCheckResult::Blocked(_)
    ) {
        return salsa::CycleRecoveryAction::Fallback(value.clone());
    }
    if count >= 16 {
        let diagnostic = SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::BorrowConflict,
            "recursive borrow validation did not converge".into(),
            SemanticDiagnosticSpan::Origin {
                owner: instance.key(db).owner(db),
                origin: SemOrigin::Body(instance.key(db).owner(db)),
            },
        );
        return salsa::CycleRecoveryAction::Fallback(SemanticBorrowCheckResult::Err(
            SemanticDiagnosticId::new(db, diagnostic),
        ));
    }
    salsa::CycleRecoveryAction::Iterate
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
    seen_diags: &mut FxHashSet<SemanticDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        if let Some(owner) = crate::hir_def::WhereClauseOwner::from_item_opt(*item) {
            for &body in owner.where_clause(db).const_predicates(db) {
                collect_owner(
                    db,
                    BodyOwner::AnonConstBody {
                        body,
                        expected: crate::analysis::ty::ty_def::TyId::bool(db),
                    },
                    seen_owners,
                    seen_diags,
                    diags,
                );
            }
        }
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
    seen_diags: &mut FxHashSet<SemanticDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if !seen_owners.insert(owner) {
        return;
    }
    let key = identity_semantic_instance_key(db, owner);
    let instance = get_or_build_semantic_instance(db, key);
    match semantic_borrow_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => {}
        SemanticBorrowCheckResult::Pending(validation) => {
            collect_pending_validation(db, instance, validation, seen_diags, diags);
        }
        SemanticBorrowCheckResult::Blocked(_) => return,
        SemanticBorrowCheckResult::Err(diag) if seen_diags.insert(diag) => {
            diags.push(Box::new(diag));
        }
        SemanticBorrowCheckResult::Err(_) => {}
    }
    match super::boundary::semantic_boundary_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => {}
        SemanticBorrowCheckResult::Pending(validation) => {
            collect_pending_validation(db, instance, validation, seen_diags, diags)
        }
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
    pub pending: PendingSemanticValidation<'db>,
}

fn collect_pending_validation<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    validation: PendingSemanticValidation<'db>,
    seen_diags: &mut FxHashSet<SemanticDiagnosticId<'db>>,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if instance.key(db).owner(db).body(db).is_some() && !can_specialize(db, instance) {
        let diagnostic = SemanticDiagnosticId::new(db, validation.diagnostic(db, instance));
        if seen_diags.insert(diagnostic) {
            diags.push(Box::new(diagnostic));
        }
    }
}

fn semantic_borrow_summary_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowSummaryResult<'db> {
    match signature_summary(db, instance, false) {
        Ok(summary) => SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(db, summary))),
        Err(diag) => SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag)),
    }
}

fn semantic_borrow_summary_cycle_recover<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &SemanticBorrowSummaryResult<'db>,
    count: u32,
    instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticBorrowSummaryResult<'db>> {
    if count >= 16
        && !matches!(
            value,
            SemanticBorrowSummaryResult::Err(_)
                | SemanticBorrowSummaryResult::Pending { .. }
                | SemanticBorrowSummaryResult::Blocked { .. }
        )
    {
        // A signature-only fallback cannot describe body-dependent boundary
        // requirements. Do not silently discharge those proofs on a cycle.
        let owner = instance.key(db).owner(db);
        let diagnostic = SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::TransportViolation,
            "recursive boundary requirements did not converge".into(),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        );
        return salsa::CycleRecoveryAction::Fallback(SemanticBorrowSummaryResult::Err(
            SemanticDiagnosticId::new(db, diagnostic),
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
                SemanticBorrowSummaryResult::Pending { validation, .. } => {
                    SemanticBorrowSummaryResult::Pending {
                        validation: validation.clone(),
                        summary: Some(BorrowSummaryId::new(db, summary)),
                    }
                }
                _ => SemanticBorrowSummaryResult::Ok(Some(BorrowSummaryId::new(db, summary))),
            },
            Err(diag) => SemanticBorrowSummaryResult::Err(SemanticDiagnosticId::new(db, diag)),
        };
        return salsa::CycleRecoveryAction::Fallback(result);
    }
    salsa::CycleRecoveryAction::Iterate
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                capability::{
                    external::ExternalOrigin,
                    guard::ValueOccurrence,
                    handle::HandleAddressSpace,
                    source::SourceExpr,
                    value::{ValueInterner, ValueLimits},
                },
                check_semantic_boundaries,
            },
            ty::{ProviderAddressSpace, corelib::MemoryAccessKind},
        },
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn opaque_native_summaries_preserve_query_order_and_provisional_recovery() {
        for summary_first in [false, true] {
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone(
                "opaque_recovery.fe".into(),
                "extern { fn lend(pointer: *u256) -> mut u256 }\nfn forward(pointer: *u256) -> mut u256 { lend(pointer) }",
            );
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            for name in ["lend", "forward"] {
                let instance = get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(
                        &db,
                        BodyOwner::Func(find_func(&db, module, name)),
                    ),
                );
                let initial = semantic_borrow_summary_cycle_initial(&db, instance);
                let SemanticBorrowSummaryResult::Ok(Some(bottom)) = &initial else {
                    panic!("{initial:?}");
                };
                assert!(!bottom.items(&db).may_return);
                assert!(bottom.items(&db).result.is_empty());
                let salsa::CycleRecoveryAction::Fallback(recovered) =
                    provisional_borrow_summary_cycle_recover(&db, &initial, 16, instance)
                else {
                    panic!("missing recovery");
                };
                let SemanticBorrowSummaryResult::Ok(Some(unknown)) = &recovered else {
                    panic!("{recovered:?}");
                };
                assert!(unknown.items(&db).may_return);
                assert!(!unknown.items(&db).result.has_missing_native_result(&db));
                if summary_first {
                    assert!(matches!(
                        semantic_borrow_summary(&db, instance),
                        Err(SemanticAnalysisError::Pending(_))
                    ));
                }
                assert!(matches!(
                    check_semantic_borrows(&db, instance),
                    Err(SemanticAnalysisError::Pending(_))
                ));
                assert!(matches!(
                    check_semantic_boundaries(&db, instance),
                    Err(SemanticAnalysisError::Pending(_))
                ));
                let pending = semantic_borrow_summary_query(&db, instance);
                let SemanticBorrowSummaryResult::Pending {
                    validation,
                    summary: Some(summary),
                } = &pending
                else {
                    panic!("{pending:?}");
                };
                assert_eq!(validation.callees.len(), 1);
                let summary = summary.items(&db);
                assert!(summary.may_return);
                assert!(!summary.result.has_missing_native_result(&db));
                assert_eq!(pending, semantic_borrow_summary_query(&db, instance));
                let salsa::CycleRecoveryAction::Fallback(replayed) =
                    provisional_borrow_summary_cycle_recover(&db, &initial, 16, instance)
                else {
                    panic!("missing replayed recovery");
                };
                assert_eq!(recovered, replayed);
                for recovery in [
                    provisional_borrow_summary_cycle_recover,
                    semantic_borrow_summary_cycle_recover,
                ] {
                    let salsa::CycleRecoveryAction::Fallback(recovered) =
                        recovery(&db, &pending, 16, instance)
                    else {
                        panic!("missing pending recovery");
                    };
                    let SemanticBorrowSummaryResult::Pending {
                        validation: retained,
                        ..
                    } = recovered
                    else {
                        panic!("pending validation was discharged by cycle recovery");
                    };
                    assert_eq!(&retained, validation);
                }
            }
        }
    }
    fn with_pending_summary(
        src: &str,
        name: &str,
        check: impl for<'db> FnOnce(&'db HirAnalysisTestDb, BorrowSummary<'db>),
    ) {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pending_summary.fe".into(), src);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(
                &db,
                BodyOwner::Func(
                    module
                        .all_funcs(&db)
                        .iter()
                        .copied()
                        .find(|func| {
                            func.name(&db)
                                .to_opt()
                                .is_some_and(|ident| ident.data(&db) == name)
                        })
                        .expect("fixture function"),
                ),
            ),
        );
        let voucher = provisional_borrow_summary_voucher(&db, instance).unwrap();
        assert!(!voucher.pending.callees.is_empty());
        check(&db, voucher.summary.unwrap());
        assert!(
            check_semantic_borrows(&db, instance).is_err(),
            "provisional facts cannot authorize execution"
        );
    }
    #[test]
    fn caller_enum_guards_use_actual_occurrences_in_parameterless_summaries() {
        with_pending_summary(
            r#"
extern { fn pointer() -> *u256 }
enum Maybe { Empty, Full(*u256) }
fn read(value: own Maybe) -> u256 {
    match value {
        Maybe::Empty => 0,
        Maybe::Full(pointer) => *pointer,
    }
}
fn root() -> u256 { read(value: Maybe::Full(pointer())) }
"#,
            "root",
            |_, summary| {
                assert!(!summary.availability.incoming.is_empty());
                assert!(
                    summary
                        .availability
                        .incoming
                        .iter()
                        .flat_map(|requirement| requirement.region.clauses())
                        .flat_map(|clause| clause.guard.occurrences())
                        .all(|occurrence| !matches!(occurrence, ValueOccurrence::Argument(_)))
                );
            },
        );
    }

    #[test]
    fn recursive_opaque_memory_effects_have_finite_summaries() {
        with_pending_summary(
            r#"
trait RecursiveRead {
    fn leaf(self, key: u256) -> u256
    fn recursive(self, depth: u256, key: u256) -> u256 {
        if depth == 0 { return self.leaf(key) }
        self.recursive(depth: depth - 1, key)
    }
}
"#,
            "recursive",
            |_, summary| {
                assert!(
                    summary
                        .accesses
                        .iter()
                        .any(|access| access.kind == MemoryAccessKind::Write
                            && !access.region.is_empty()),
                    "opaque calls must retain their possible writes: {summary:#?}"
                );
            },
        );
    }

    #[test]
    fn opaque_memory_effects_preserve_returned_address_identity() {
        with_pending_summary(
            r#"
extern { fn unknown() -> *u256 }
fn both() -> *u256 {
    let returned = unknown()
    let scratch = unknown()
    *scratch = 1
    *returned = 2
    returned
}
"#,
            "both",
            |db, summary| {
                let values = ValueInterner::new(db, ValueLimits::default());
                let returned = values.leaves(&summary.result, ValueOccurrence::Summary);
                assert_eq!(returned.len(), 1);
                let targets: Vec<_> = summary
                    .accesses
                    .iter()
                    .filter(|access| access.kind == MemoryAccessKind::Write)
                    .flat_map(|access| access.region.clauses())
                    .map(|clause| SourceExpr::from_place(&clause.payload).unwrap().source)
                    .collect();
                assert!(targets.contains(&returned[0].payload.source));
                assert!(
                    targets
                        .iter()
                        .any(|target| target != &returned[0].payload.source)
                );
            },
        );
    }

    #[test]
    fn generic_opaque_handles_preserve_declared_address_spaces() {
        let source = r#"
use core::{AddressSpace, EffectHandle}
struct Ptr<const SP: AddressSpace> { raw: u256 }
impl<const SP: AddressSpace> EffectHandle for Ptr<SP> {
    type Target = u256
    const SPACE: AddressSpace = SP
    type Raw = u256
    fn raw(self) -> u256 { self.raw }
}
extern { fn assumed<H: EffectHandle>(_ raw: u256) -> H }
fn declared<const SP: AddressSpace>(_ raw: u256) -> Ptr<SP> { Ptr { raw } }
fn memory(_ raw: u256) -> Ptr<AddressSpace::Memory> {
    declared<AddressSpace::Memory>(raw)
}
fn storage(_ raw: u256) -> Ptr<AddressSpace::Storage> {
    assumed<Ptr<AddressSpace::Storage>>(raw)
}
"#;
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_trusted_effect_handle_module("generic_opaque_spaces.fe".into(), source);
        let (top_mod, _) = db.top_mod(file);
        for (name, expected) in [
            ("assumed", None),
            ("declared", None),
            ("memory", Some(ProviderAddressSpace::Memory)),
            ("storage", Some(ProviderAddressSpace::Storage)),
        ] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
            );
            let voucher = provisional_borrow_summary_voucher(&db, instance).unwrap();
            assert_eq!(
                voucher.pending.callees.is_empty(),
                matches!(name, "declared" | "memory")
            );
            let summary = voucher.summary.unwrap();
            let values = ValueInterner::new(&db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert!(!leaves.is_empty(), "missing handle source in {name}");
            for leaf in leaves {
                let ExternalOrigin::OpaqueHandle(source) = leaf.payload.source.origin else {
                    panic!("missing opaque source in {name}")
                };
                let space = source.contract.address_space;
                assert_eq!(space.known(), expected, "{name}");
                if expected.is_none() {
                    assert!(matches!(space, HandleAddressSpace::Declared { .. }));
                    assert!(
                        space.may_alias(HandleAddressSpace::Known(ProviderAddressSpace::Memory))
                    );
                    assert!(
                        space.may_alias(HandleAddressSpace::Known(ProviderAddressSpace::Storage))
                    );
                }
            }
        }
    }
}
