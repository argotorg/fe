//! Shared diagnostics and admission failures for semantic consumers.
use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, Span, SubDiagnostic,
};
use cranelift_entity::EntityRef;
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::DiagnosticVoucher,
        diagnostics::SpannedHirAnalysisDb,
        semantic::{
            SLocalId, SemOrigin, SemanticInstance,
            normalized::{
                NOperand, NormalizeError, NormalizedBody, NormalizedBodyVerifyError,
                NormalizedLayoutPlanVerifyError,
            },
        },
        ty::ty_check::{BodyOwner, SmirLoweringIssue},
    },
    hir_def::{Body, ExprId, Partial},
    span::LazySpan,
};

pub(crate) fn operand_origin<'db>(operand: NOperand, fallback: SemOrigin<'db>) -> SemOrigin<'db> {
    operand.origin.map_or(fallback, SemOrigin::Expr)
}

pub(crate) fn normalized_body_internal_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
    origin: SemOrigin<'db>,
    message: String,
) -> SemanticDiagnostic<'db> {
    SemanticDiagnostic::new(
        instance,
        SemanticDiagnosticKind::Internal,
        message,
        SemanticDiagnosticSpan::OriginWithTemplateFallback {
            owner: instance.key(db).owner(db),
            template_owner: body.template_owner,
            origin,
        },
    )
}

pub(crate) fn smir_lowering_admission_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    causes: &[SmirLoweringIssue],
) -> SemanticDiagnostic<'db> {
    let owner = instance.key(db).owner(db);
    SemanticDiagnostic::new(
        instance,
        SemanticDiagnosticKind::Internal,
        format!(
            "semantic body has {} unresolved lowering plan entr{} despite valid typed input",
            causes.len(),
            if causes.len() == 1 { "y" } else { "ies" },
        ),
        SemanticDiagnosticSpan::Origin {
            owner,
            origin: SemOrigin::Body(owner),
        },
    )
}

pub(crate) fn normalized_body_error_to_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: NormalizeError<'db>,
) -> SemanticDiagnostic<'db> {
    let owner = instance.key(db).owner(db);
    let (message, span) = match error {
        NormalizeError::MissingValue(local) => (
            format!(
                "normalized body is missing a value for raw local `%{}`",
                local.index()
            ),
            SemanticDiagnosticSpan::LocalSourceOrBody { instance, local },
        ),
        NormalizeError::MissingRoot(local) => (
            format!(
                "normalized body is missing a root for raw local `%{}`",
                local.index()
            ),
            SemanticDiagnosticSpan::LocalSourceOrBody { instance, local },
        ),
        NormalizeError::MissingProviderAddressSpace(provider) => (
            format!("normalized provider has no address space: {provider:?}"),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        NormalizeError::UnresolvedHandleOrigin(ty) => (
            format!(
                "normalized handle has no resolved origin contract: {}",
                ty.pretty_print(db)
            ),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        NormalizeError::InvalidProjection => (
            "normalized body contains an invalid projection".to_string(),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        NormalizeError::UnsupportedPlaceProjection => (
            "normalized body contains a non-data place projection".to_string(),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        NormalizeError::UnsupportedCapabilityCast { from, to } => (
            format!(
                "normalized body cannot structurally repack `{}` as `{}`",
                from.pretty_print(db),
                to.pretty_print(db),
            ),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
        NormalizeError::InvalidControlFlow => (
            "normalized body contains invalid control flow".to_string(),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        ),
    };
    SemanticDiagnostic::new(instance, SemanticDiagnosticKind::Internal, message, span)
}

pub(crate) fn normalized_body_verify_error_to_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: NormalizedBodyVerifyError,
) -> SemanticDiagnostic<'db> {
    let owner = instance.key(db).owner(db);
    SemanticDiagnostic::new(
        instance,
        SemanticDiagnosticKind::Internal,
        format!("normalized body verification failed: {error:?}"),
        SemanticDiagnosticSpan::Origin {
            owner,
            origin: SemOrigin::Body(owner),
        },
    )
}

pub(crate) fn normalized_layout_plan_verify_error_to_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: NormalizedLayoutPlanVerifyError,
) -> SemanticDiagnostic<'db> {
    let owner = instance.key(db).owner(db);
    SemanticDiagnostic::new(
        instance,
        SemanticDiagnosticKind::Internal,
        format!("normalized layout plan verification failed: {error:?}"),
        SemanticDiagnosticSpan::Origin {
            owner,
            origin: SemOrigin::Body(owner),
        },
    )
}

impl<'db> SemanticDiagnostic<'db> {
    pub(crate) fn new(
        instance: SemanticInstance<'db>,
        kind: SemanticDiagnosticKind,
        message: String,
        span: SemanticDiagnosticSpan<'db>,
    ) -> Self {
        Self {
            kind,
            instance,
            primary: SemanticDiagnosticLabel { message, span },
            secondaries: Vec::new(),
        }
    }

    pub(crate) fn push_secondary(&mut self, message: String, span: SemanticDiagnosticSpan<'db>) {
        self.secondaries
            .push(SemanticDiagnosticLabel { message, span });
    }
}

impl DiagnosticVoucher for SemanticDiagnosticId<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        self.diag(db).to_complete(db)
    }
}

impl DiagnosticVoucher for SemanticDiagnostic<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let local_code = match self.kind {
            SemanticDiagnosticKind::BorrowConflict => 1,
            SemanticDiagnosticKind::MoveConflict => 2,
            SemanticDiagnosticKind::InvalidReturnBorrow => 3,
            SemanticDiagnosticKind::Internal => 4,
            SemanticDiagnosticKind::NoEscViolation => 5,
            SemanticDiagnosticKind::ProviderProvenanceConflict => 6,
            SemanticDiagnosticKind::TransportViolation => 7,
            SemanticDiagnosticKind::StorageViolation => 8,
            SemanticDiagnosticKind::UnresolvedCall => 9,
            SemanticDiagnosticKind::InvalidConcreteType => 10,
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

impl SemanticDiagnosticKind {
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
            Self::TransportViolation => {
                format!("transport violation in `fn {}`", checker_name(db, instance))
            }
            Self::StorageViolation => {
                format!("storage violation in `fn {}`", checker_name(db, instance))
            }
            Self::ProviderProvenanceConflict => {
                format!(
                    "provider provenance conflict in `fn {}`",
                    checker_name(db, instance)
                )
            }
            Self::UnresolvedCall => format!(
                "pending borrow validation in `fn {}`",
                checker_name(db, instance)
            ),
            Self::InvalidConcreteType => format!(
                "invalid concrete type in `fn {}`",
                checker_name(db, instance)
            ),
        }
    }
}

impl<'db> SemanticDiagnosticSpan<'db> {
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
            Self::HirExpr { body, expr } => expr.span(body).resolve(db),
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
                .admitted_body(db)
                .ok()?
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticDiagnostic<'db> {
    pub kind: SemanticDiagnosticKind,
    pub instance: SemanticInstance<'db>,
    pub primary: SemanticDiagnosticLabel<'db>,
    pub secondaries: Vec<SemanticDiagnosticLabel<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticDiagnosticLabel<'db> {
    pub message: String,
    pub span: SemanticDiagnosticSpan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticDiagnosticSpan<'db> {
    Origin {
        owner: BodyOwner<'db>,
        origin: SemOrigin<'db>,
    },
    OriginWithTemplateFallback {
        owner: BodyOwner<'db>,
        template_owner: BodyOwner<'db>,
        origin: SemOrigin<'db>,
    },
    LocalSourceOrBody {
        instance: SemanticInstance<'db>,
        local: SLocalId,
    },
    HirExpr {
        body: Body<'db>,
        expr: ExprId,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticDiagnosticId<'db> {
    pub diag: SemanticDiagnostic<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct BlockedSemanticBody<'db> {
    pub instance: SemanticInstance<'db>,
    pub causes: Box<[SmirLoweringIssue]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SemanticNormalizationFailure<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Rejected(SemanticDiagnostic<'db>),
    InternalFailure(SemanticDiagnostic<'db>),
}

impl<'db> SemanticNormalizationFailure<'db> {
    pub fn diagnostic(&self) -> Option<&SemanticDiagnostic<'db>> {
        match self {
            Self::Blocked(_) => None,
            Self::Rejected(diag) | Self::InternalFailure(diag) => Some(diag),
        }
    }
}

impl<'db> From<SemanticDiagnostic<'db>> for SemanticNormalizationFailure<'db> {
    fn from(diag: SemanticDiagnostic<'db>) -> Self {
        Self::InternalFailure(diag)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticDiagnosticKind {
    BorrowConflict,
    MoveConflict,
    InvalidReturnBorrow,
    Internal,
    NoEscViolation,
    TransportViolation,
    StorageViolation,
    ProviderProvenanceConflict,
    UnresolvedCall,
    InvalidConcreteType,
}
