use salsa::Update;

use crate::analysis::{
    semantic::{SemOrigin, normalized::NDataPath},
    ty::ty_check::{BodyOwner, SmirLoweringIssue},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BorrowInputRef {
    Param(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BorrowTransform {
    pub input: BorrowInputRef,
    pub proj: NDataPath,
}

pub type BorrowSummary = Vec<BorrowTransform>;

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub items: Vec<BorrowTransform>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBorrowDiagnostic<'db> {
    pub kind: SemanticBorrowDiagKind,
    pub instance: crate::analysis::semantic::SemanticInstance<'db>,
    pub primary: SemanticBorrowDiagnosticLabel<'db>,
    pub secondaries: Vec<SemanticBorrowDiagnosticLabel<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBorrowDiagnosticLabel<'db> {
    pub message: String,
    pub span: SemanticBorrowDiagnosticSpan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowDiagnosticSpan<'db> {
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
        instance: crate::analysis::semantic::SemanticInstance<'db>,
        local: crate::analysis::semantic::SLocalId,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowDiagnosticId<'db> {
    pub diag: SemanticBorrowDiagnostic<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct BlockedSemanticBody<'db> {
    pub instance: crate::analysis::semantic::SemanticInstance<'db>,
    pub causes: Box<[SmirLoweringIssue]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowSummaryResult<'db> {
    Ok(Option<BorrowSummaryId<'db>>),
    Blocked {
        body: BlockedSemanticBody<'db>,
        summary: Option<BorrowSummaryId<'db>>,
    },
    Err(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowCheckResult<'db> {
    Ok,
    Blocked(BlockedSemanticBody<'db>),
    Err(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SemanticNormalizationFailure<'db> {
    Blocked(BlockedSemanticBody<'db>),
    InternalFailure(SemanticBorrowDiagnostic<'db>),
}

impl<'db> SemanticNormalizationFailure<'db> {
    pub fn diagnostic(&self) -> Option<&SemanticBorrowDiagnostic<'db>> {
        match self {
            Self::Blocked(_) => None,
            Self::InternalFailure(diag) => Some(diag),
        }
    }
}

impl<'db> From<SemanticBorrowDiagnostic<'db>> for SemanticNormalizationFailure<'db> {
    fn from(diag: SemanticBorrowDiagnostic<'db>) -> Self {
        Self::InternalFailure(diag)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowDiagKind {
    BorrowConflict,
    MoveConflict,
    InvalidReturnBorrow,
    Internal,
    NoEscViolation,
    ProviderProvenanceConflict,
}
