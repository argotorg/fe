use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, Span, SubDiagnostic,
};
use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::DiagnosticVoucher,
        diagnostics::SpannedHirAnalysisDb,
        semantic::{NOperand, SLocalId, SemOrigin, SemanticInstance},
        ty::ty_check::BodyOwner,
    },
    hir_def::{Body, Partial},
    span::LazySpan,
};

use super::ir::{
    BorrowDiagnosticId, NormalizedSemanticBody, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
    SemanticBorrowDiagnosticLabel, SemanticBorrowDiagnosticSpan, SemanticNormalizeError,
};

pub(super) fn operand_origin<'db>(operand: NOperand, fallback: SemOrigin<'db>) -> SemOrigin<'db> {
    operand.origin.map_or(fallback, SemOrigin::Expr)
}

pub(super) fn normalized_body_internal_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    origin: SemOrigin<'db>,
    message: String,
) -> SemanticBorrowDiagnostic<'db> {
    SemanticBorrowDiagnostic::new(
        instance,
        SemanticBorrowDiagKind::Internal,
        message,
        SemanticBorrowDiagnosticSpan::OriginWithTemplateFallback {
            owner: instance.key(db).owner(db),
            template_owner: body.template_owner,
            origin,
        },
    )
}

pub(super) fn normalize_error_to_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    err: SemanticNormalizeError<'db>,
) -> SemanticBorrowDiagnostic<'db> {
    let owner = instance.key(db).owner(db);
    let hir_body = owner.body(db);
    let (message, span) = match err {
        SemanticNormalizeError::MissingBorrowRoot { local } => {
            let message = if let Some(body) = hir_body
                && let Some(raw_local) = instance.body(db).local(local)
                && let Some(source) = raw_local.source
            {
                format!(
                    "cannot normalize borrow roots for `{}`",
                    source.pretty_name_in_body(db, body)
                )
            } else {
                format!("cannot normalize borrow roots for `%{}`", local.index())
            };
            (
                message,
                SemanticBorrowDiagnosticSpan::LocalSourceOrBody { instance, local },
            )
        }
        SemanticNormalizeError::LocalProvenanceCycle { local, .. } => (
            format!(
                "detected a cycle while normalizing derived-place provenance for `%{}`",
                local.index()
            ),
            SemanticBorrowDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        SemanticNormalizeError::NonPlaceDerivedValue { local, base, .. } => (
            format!(
                "cannot normalize derived-place provenance for `%{}` from non-place base `%{}`",
                local.index(),
                base.index()
            ),
            SemanticBorrowDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
    };
    SemanticBorrowDiagnostic::new(instance, SemanticBorrowDiagKind::Internal, message, span)
}

impl<'db> SemanticBorrowDiagnostic<'db> {
    pub(super) fn new(
        instance: SemanticInstance<'db>,
        kind: SemanticBorrowDiagKind,
        message: String,
        span: SemanticBorrowDiagnosticSpan<'db>,
    ) -> Self {
        Self {
            kind,
            instance,
            primary: SemanticBorrowDiagnosticLabel { message, span },
            secondaries: Vec::new(),
        }
    }

    pub(super) fn push_secondary(
        &mut self,
        message: String,
        span: SemanticBorrowDiagnosticSpan<'db>,
    ) {
        self.secondaries
            .push(SemanticBorrowDiagnosticLabel { message, span });
    }
}

impl DiagnosticVoucher for BorrowDiagnosticId<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        self.diag(db).to_complete(db)
    }
}

impl DiagnosticVoucher for SemanticBorrowDiagnostic<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let local_code = match self.kind {
            SemanticBorrowDiagKind::BorrowConflict => 1,
            SemanticBorrowDiagKind::MoveConflict => 2,
            SemanticBorrowDiagKind::InvalidReturnBorrow => 3,
            SemanticBorrowDiagKind::Internal => 4,
            SemanticBorrowDiagKind::NoEscViolation => 5,
            SemanticBorrowDiagKind::ProviderProvenanceConflict => 6,
        };
        CompleteDiagnostic::new(
            Severity::Error,
            self.kind.header(db, self.instance),
            std::iter::once(SubDiagnostic::new(
                LabelStyle::Primary,
                self.primary.message.clone(),
                self.primary.span.resolve(db),
            ))
            .chain(self.secondaries.iter().map(|secondary| {
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    secondary.message.clone(),
                    secondary.span.resolve(db),
                )
            }))
            .collect(),
            Vec::new(),
            GlobalErrorCode::new(DiagnosticPass::SemanticBorrowck, local_code),
        )
    }
}

impl SemanticBorrowDiagKind {
    fn header<'db>(self, db: &'db dyn HirAnalysisDb, instance: SemanticInstance<'db>) -> String {
        match self {
            Self::BorrowConflict => {
                format!("borrow conflict in `fn {}`", checker_name(db, instance))
            }
            Self::MoveConflict => format!("move conflict in `fn {}`", checker_name(db, instance)),
            Self::InvalidReturnBorrow => {
                format!(
                    "invalid return borrow in `fn {}`",
                    checker_name(db, instance)
                )
            }
            Self::Internal => {
                format!(
                    "internal borrow checking error in `fn {}`",
                    checker_name(db, instance)
                )
            }
            Self::NoEscViolation => {
                format!("noesc violation in `fn {}`", checker_name(db, instance))
            }
            Self::ProviderProvenanceConflict => {
                format!(
                    "provider provenance conflict in `fn {}`",
                    checker_name(db, instance)
                )
            }
        }
    }
}

impl<'db> SemanticBorrowDiagnosticSpan<'db> {
    fn resolve(&self, db: &dyn SpannedHirAnalysisDb) -> Option<Span> {
        match *self {
            Self::Origin { owner, origin } => span_for_origin_from_body(db, owner.body(db), origin),
            Self::OriginWithTemplateFallback {
                owner,
                template_owner,
                origin,
            } => span_for_origin_from_body(db, owner.body(db), origin).or_else(|| {
                template_owner
                    .body(db)
                    .and_then(|hir_body| hir_body.span().resolve(db))
            }),
            Self::LocalSourceOrBody { instance, local } => {
                resolve_local_source_span(db, instance, local)
            }
        }
    }
}

pub(crate) fn resolve_local_source_span<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
    local: SLocalId,
) -> Option<Span> {
    let owner = instance.key(db).owner(db);
    let hir_body = owner.body(db);
    hir_body
        .and_then(|body| {
            instance
                .body(db)
                .local(local)
                .and_then(|local| local.source)
                .and_then(|source| source.def_span_in_body(body).resolve(db))
        })
        .or_else(|| hir_body.and_then(|body| body.span().resolve(db)))
}

pub(crate) fn span_for_origin_from_body<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    body: Option<Body<'db>>,
    origin: SemOrigin<'db>,
) -> Option<Span> {
    let body = body?;
    match origin {
        SemOrigin::Expr(expr) => expr.span(body).resolve(db),
        SemOrigin::Stmt(stmt) => stmt.span(body).resolve(db),
        SemOrigin::Body(owner) => owner.body(db).and_then(|body| body.span().resolve(db)),
        SemOrigin::Synthetic => None,
    }
}

pub(crate) fn checker_name<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> String {
    match instance.key(db).owner(db) {
        BodyOwner::Func(func) => match func.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<fn>".to_string(),
        },
        BodyOwner::Const(const_) => match const_.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<const>".to_string(),
        },
        BodyOwner::AnonConstBody { .. } => "<anon const>".to_string(),
        BodyOwner::ContractInit { contract } => format!(
            "{}::__init__",
            match contract.name(db) {
                Partial::Present(name) => name.data(db).to_string(),
                Partial::Absent => "<contract>".to_string(),
            }
        ),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "{}::recv[{recv_idx}][{arm_idx}]",
            match contract.name(db) {
                Partial::Present(name) => name.data(db).to_string(),
                Partial::Absent => "<contract>".to_string(),
            }
        ),
    }
}
