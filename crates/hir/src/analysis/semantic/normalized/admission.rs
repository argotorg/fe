// Diagnostics and CTFE stops are returned on the slow path; salsa's 8-byte ids
// put them just past clippy's 128-byte `Err` threshold.
#![allow(clippy::result_large_err)]

use cranelift_entity::EntityRef;
use salsa::SalsaValue;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BlockedSemanticBody, SStmtKind, SemOrigin, SemanticBody, SemanticDiagnostic,
        SemanticDiagnosticId, SemanticInstance, SemanticLocalRole, SemanticNormalizationFailure,
        ctfe::{canonicalize_semantic_const_refs, canonicalize_semantic_consts_for_runtime},
        diagnostics::{
            SemanticDiagnosticKind, SemanticDiagnosticSpan, normalized_body_error_to_diag,
            normalized_body_verify_error_to_diag, normalized_layout_plan_verify_error_to_diag,
            smir_lowering_admission_diag,
        },
        instance::{LayoutDemandBody, SemanticBodyAdmissionError},
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
    pub body: NormalizedBody<'db>,
    pub layout_plan: NLayoutPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
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

/// The admission of the body runtime lowering uses: the checked one's, with
/// constant-evaluable operations folded.
pub fn runtime_semantic_body_admission<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    runtime_admitted_semantic_body_query(db, instance)
}

/// The verified body with constant-evaluable operations folded, which runtime
/// lowering uses; ownership is checked on
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

#[salsa::tracked(returns(clone))]
fn admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    if let Some(diag) = instance.concrete_layout_diagnostic(db, LayoutDemandBody::Admitted) {
        return SemanticBodyAdmission::Rejected(diag);
    }
    let raw = canonicalize_semantic_const_refs(db, instance, raw);
    normalize_and_verify(db, instance, &raw, instance.assumptions(db))
}

#[salsa::tracked(returns(clone))]
fn runtime_admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    // Folding evaluates values of the body's types, so they must be valid.
    if let Some(diag) = instance.concrete_layout_diagnostic(db, LayoutDemandBody::Admitted) {
        return SemanticBodyAdmission::Rejected(diag);
    }
    let folded = canonicalize_semantic_consts_for_runtime(db, instance, raw);
    if folded == canonicalize_semantic_const_refs(db, instance, raw) {
        return admitted_semantic_body_query(db, instance);
    }
    normalize_and_verify(db, instance, &folded, instance.assumptions(db))
}

#[salsa::tracked(returns(clone))]
fn provisional_admitted_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBodyAdmission<'db> {
    let raw = match instance.admitted_provisional_body(db) {
        Ok(body) => body,
        Err(error) => return admission_failure(db, instance, error),
    };
    if let Some(diag) = instance.concrete_layout_diagnostic(db, LayoutDemandBody::Provisional) {
        return SemanticBodyAdmission::Rejected(diag);
    }
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
    if let Some(diag) = state_only_memory_value(db, instance, raw) {
        return SemanticBodyAdmission::Rejected(diag);
    }
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

/// A memory value in `raw` whose type is state-only in `instance`. A state-only
/// type lives only at places in storage or transient storage: a template may
/// hold its type parameters as memory values, and an instance whose arguments
/// make one of those state-only would hold it in memory. The locals the
/// boundary initializes, parameters and effects, name the caller's values,
/// which the caller made and is checked for.
fn state_only_memory_value<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: &SemanticBody<'db>,
) -> Option<SemanticDiagnosticId<'db>> {
    let (idx, ty, part) = raw.locals.iter().enumerate().find_map(|(idx, local)| {
        if !matches!(local.role, SemanticLocalRole::DirectValue { .. })
            || raw.entry_locals.iter().any(|entry| entry.index() == idx)
        {
            return None;
        }
        let ty = instance.normalized_ty(db, local.ty);
        Some((idx, ty, ty.pinned_part(db)?))
    })?;
    let origin = raw
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| match stmt.kind {
            SStmtKind::Assign { dst, .. } if dst.index() == idx => Some(stmt.origin),
            _ => None,
        })
        .unwrap_or(SemOrigin::Body(raw.template_owner));
    Some(SemanticDiagnosticId::new(
        db,
        SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::InvalidConcreteType,
            format!(
                "here `{}` is a value in memory, but it holds `{}`, which is pinned to a state space",
                ty.pretty_print(db),
                part.pretty_print(db)
            ),
            SemanticDiagnosticSpan::Origin {
                owner: instance.key(db).owner(db),
                origin,
            },
        ),
    ))
}

fn admission_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: SemanticBodyAdmissionError<'db>,
) -> SemanticBodyAdmission<'db> {
    match error {
        // Type checking leaves a faulting specialized extent to concrete
        // demand, so an expression it invalidates has no upstream report.
        SemanticBodyAdmissionError::BlockedByUpstreamDiagnostics(causes) => instance
            .concrete_layout_diagnostic(db, LayoutDemandBody::Expressions)
            .map_or_else(
                || SemanticBodyAdmission::Blocked(BlockedSemanticBody { instance, causes }),
                SemanticBodyAdmission::Rejected,
            ),
        SemanticBodyAdmissionError::IncompleteLoweringPlan(causes) => {
            internal_failure(db, smir_lowering_admission_diag(db, instance, &causes))
        }
        SemanticBodyAdmissionError::CallSiteFinalization(diag) => {
            SemanticBodyAdmission::InternalFailure(diag)
        }
        SemanticBodyAdmissionError::InvalidConcreteType(diag)
        | SemanticBodyAdmissionError::InstanceLimit(diag) => SemanticBodyAdmission::Rejected(diag),
    }
}

fn internal_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    diagnostic: SemanticDiagnostic<'db>,
) -> SemanticBodyAdmission<'db> {
    SemanticBodyAdmission::InternalFailure(SemanticDiagnosticId::new(db, diagnostic))
}
