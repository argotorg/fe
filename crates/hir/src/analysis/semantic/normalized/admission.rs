use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BlockedSemanticBody, BorrowDiagnosticId, SemanticBody, SemanticBorrowDiagnostic,
        SemanticInstance, SemanticNormalizationFailure,
        borrowck::{
            normalized_body_error_to_diag, normalized_body_verify_error_to_diag,
            normalized_layout_plan_verify_error_to_diag, smir_lowering_admission_diag,
        },
        ctfe::canonicalize_semantic_consts_for_admission,
        instance::SemanticBodyAdmissionError,
        semantic_instance_base_assumptions_for_key,
    },
    ty::trait_resolution::PredicateListId,
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
    InternalFailure(BorrowDiagnosticId<'db>),
}

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
    let raw = canonicalize_semantic_consts_for_admission(db, instance, raw);
    normalize_and_verify(db, instance, &raw, instance.assumptions(db))
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
    let raw = canonicalize_semantic_consts_for_admission(db, instance, raw);
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
    }
}

fn internal_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: SemanticBorrowDiagnostic<'db>,
) -> SemanticBodyAdmission<'db> {
    SemanticBodyAdmission::InternalFailure(BorrowDiagnosticId::new(db, diagnostic))
}
