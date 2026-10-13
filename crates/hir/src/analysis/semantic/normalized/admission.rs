use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BlockedSemanticBody, SemanticBody, SemanticDiagnostic, SemanticDiagnosticId,
        SemanticInstance, SemanticNormalizationFailure,
        ctfe::{canonicalize_semantic_const_refs, canonicalize_semantic_consts_for_runtime},
        diagnostics::{
            normalized_body_error_to_diag, normalized_body_verify_error_to_diag,
            normalized_layout_plan_verify_error_to_diag, smir_lowering_admission_diag,
        },
        instance::SemanticBodyAdmissionError,
        semantic_instance_base_assumptions_for_key,
    },
    ty::{trait_resolution::PredicateListId, ty_check::BodyOwner},
};

use super::{
    NLayoutPlan, NormalizedArtifacts, NormalizedBody, normalize_raw_body, verify_normalized_body,
    verify_normalized_layout_plan,
};

#[salsa::interned]
#[derive(Debug)]
pub struct AdmittedSemanticBodyId<'db> {
    #[return_ref]
    pub body: NormalizedBody<'db>,
    #[return_ref]
    pub layout_plan: NLayoutPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBodyAdmission<'db> {
    Ready(AdmittedSemanticBodyId<'db>),
    Blocked(BlockedSemanticBody<'db>),
    Rejected(SemanticDiagnosticId<'db>),
    InternalFailure(SemanticDiagnosticId<'db>),
}

/// The verified operation-preserving body. Const references are resolved, but
/// no operation is folded: borrow checking and definite assignment see every
/// move, borrow, read, and call, whether or not its operands are constant.
pub fn semantic_body_admission<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    admitted_semantic_body_query(db, instance)
}

pub fn normalize_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedArtifacts<'db>, SemanticNormalizationFailure<'db>> {
    artifacts_from_admission(db, semantic_body_admission(db, instance))
}

/// The verified body with constant-evaluable operations folded. Layout evidence
/// and runtime lowering share it; ownership is checked on
/// [`normalize_semantic_body`] instead.
pub fn normalize_runtime_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedArtifacts<'db>, SemanticNormalizationFailure<'db>> {
    artifacts_from_admission(db, runtime_admitted_semantic_body_query(db, instance))
}

pub(crate) fn normalize_semantic_body_provisional<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedArtifacts<'db>, SemanticNormalizationFailure<'db>> {
    artifacts_from_admission(db, provisional_admitted_semantic_body_query(db, instance))
}

fn artifacts_from_admission<'db>(
    db: &'db dyn HirAnalysisDb,
    admission: SemanticBodyAdmission<'db>,
) -> Result<NormalizedArtifacts<'db>, SemanticNormalizationFailure<'db>> {
    match admission {
        SemanticBodyAdmission::Ready(admitted) => Ok(NormalizedArtifacts {
            body: admitted.body(db).clone(),
            layout_plan: admitted.layout_plan(db).clone(),
        }),
        SemanticBodyAdmission::Blocked(blocked) => {
            Err(SemanticNormalizationFailure::Blocked(blocked))
        }
        SemanticBodyAdmission::Rejected(diag) => Err(SemanticNormalizationFailure::Rejected(
            diag.diag(db).clone(),
        )),
        SemanticBodyAdmission::InternalFailure(diag) => Err(
            SemanticNormalizationFailure::InternalFailure(diag.diag(db).clone()),
        ),
    }
}

#[salsa::tracked]
fn admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    let raw = canonicalize_semantic_const_refs(db, instance, raw);
    normalize_and_verify(db, instance, &raw, instance.assumptions(db))
}

/// Reject an actual runtime call whose scalar-selected evidence cannot be
/// recovered from checked structural sources. Definition validation keeps the
/// operation; this adapter reuses the same recovery outcomes as CTFE/checking.
#[salsa::tracked]
pub(crate) fn runtime_choice_call_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<SemanticDiagnostic<'db>> {
    let raw = instance.admitted_body(db).ok()?;
    use crate::analysis::semantic::{
        CallSiteId, SExpr, SStmtKind, get_or_build_semantic_instance,
        instance::recover_known_operand, operand_recovery::KnownOperandRecovery,
    };
    for statement in raw.blocks.iter().flat_map(|block| &block.stmts) {
        let SStmtKind::Assign {
            expr:
                SExpr::Call {
                    call_site: CallSiteId::Expr(expr),
                    callee,
                    ..
                },
            ..
        } = &statement.kind
        else {
            continue;
        };
        let BodyOwner::Func(func) = callee.key.owner(db) else {
            continue;
        };
        if !func.is_const(db) || func.body(db).is_none() {
            continue;
        }
        let callee = get_or_build_semantic_instance(db, callee.key);
        let body = callee.provisional_body(db);
        let pending = body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|statement| {
                if let SStmtKind::Assign {
                    expr: SExpr::OpenedConst { evidence, .. },
                    ..
                } = &statement.kind
                {
                    Some(evidence.sem_origin(statement.origin))
                } else {
                    None
                }
            });
        let Some(origin) = pending else {
            continue;
        };
        match recover_known_operand(db, instance, *expr) {
            KnownOperandRecovery::NotKnown => {
                return Some(normalized_body_error_to_diag(
                    db,
                    callee,
                    super::NormalizeError::EvidenceChoiceNotCompileTime(origin),
                ));
            }
            // Fault and blocked outcomes retain their existing evaluation path;
            // neither is converted into absence or a different implementation.
            KnownOperandRecovery::Value(_)
            | KnownOperandRecovery::Failed { .. }
            | KnownOperandRecovery::Blocked { .. } => {}
        }
    }
    None
}

#[salsa::tracked]
fn runtime_admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    if let Some(diagnostic) = runtime_choice_call_diagnostic(db, instance) {
        return SemanticBodyAdmission::Rejected(SemanticDiagnosticId::new(db, diagnostic));
    }
    let folded = canonicalize_semantic_consts_for_runtime(db, instance, raw);
    for block in &folded.blocks {
        for statement in &block.stmts {
            if let crate::analysis::semantic::SStmtKind::Assign {
                expr: crate::analysis::semantic::SExpr::OpenedConst { evidence, .. },
                ..
            } = &statement.kind
            {
                return SemanticBodyAdmission::Rejected(SemanticDiagnosticId::new(
                    db,
                    normalized_body_error_to_diag(
                        db,
                        instance,
                        super::NormalizeError::EvidenceChoiceNotCompileTime(
                            evidence.sem_origin(statement.origin),
                        ),
                    ),
                ));
            }
        }
    }
    if folded == canonicalize_semantic_const_refs(db, instance, raw) {
        return admitted_semantic_body_query(db, instance);
    }
    normalize_and_verify(db, instance, &folded, instance.assumptions(db))
}

#[salsa::tracked]
fn provisional_admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_provisional_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    let raw = canonicalize_semantic_const_refs(db, instance, raw);
    normalize_and_verify(
        db,
        instance,
        &raw,
        semantic_instance_base_assumptions_for_key(db, instance.key(db)),
    )
}

fn normalize_and_verify<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: &SemanticBody<'db>,
    assumptions: PredicateListId<'db>,
) -> SemanticBodyAdmission<'db> {
    match normalize_raw_body(db, instance, raw, assumptions) {
        Ok(artifacts) => {
            if let Err(error) = verify_normalized_body(db, &artifacts.body) {
                return internal_failure(
                    db,
                    normalized_body_verify_error_to_diag(db, instance, error),
                );
            }
            if let Err(error) =
                verify_normalized_layout_plan(db, &artifacts.body, raw, &artifacts.layout_plan)
            {
                return internal_failure(
                    db,
                    normalized_layout_plan_verify_error_to_diag(db, instance, error),
                );
            }
            SemanticBodyAdmission::Ready(AdmittedSemanticBodyId::new(
                db,
                artifacts.body,
                artifacts.layout_plan,
            ))
        }
        Err(error @ super::NormalizeError::EvidenceChoiceNotCompileTime(_)) => {
            SemanticBodyAdmission::Rejected(SemanticDiagnosticId::new(
                db,
                normalized_body_error_to_diag(db, instance, error),
            ))
        }
        Err(error) => internal_failure(db, normalized_body_error_to_diag(db, instance, error)),
    }
}

fn admission_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: SemanticBodyAdmissionError<'db>,
) -> SemanticBodyAdmission<'db> {
    match error {
        SemanticBodyAdmissionError::BlockedByUpstreamDiagnostics(causes) => {
            SemanticBodyAdmission::Blocked(BlockedSemanticBody { instance, causes })
        }
        SemanticBodyAdmissionError::IncompleteLoweringPlan(causes) => {
            internal_failure(db, smir_lowering_admission_diag(db, instance, &causes))
        }
        SemanticBodyAdmissionError::CallSiteFinalization(diag) => {
            SemanticBodyAdmission::InternalFailure(diag)
        }
        SemanticBodyAdmissionError::InvalidConcreteType(diag) => {
            SemanticBodyAdmission::Rejected(diag)
        }
    }
}

fn internal_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: SemanticDiagnostic<'db>,
) -> SemanticBodyAdmission<'db> {
    SemanticBodyAdmission::InternalFailure(SemanticDiagnosticId::new(db, diagnostic))
}
