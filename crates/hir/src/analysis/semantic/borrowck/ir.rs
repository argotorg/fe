use salsa::Update;

use crate::analysis::{
    semantic::{
        SemOrigin, SemanticInstance,
        capability::{region::RegionSet, source::SourceExpr, value::ValueId},
    },
    ty::{
        corelib::MemoryAccessKind,
        ty_check::{BodyOwner, SmirLoweringIssue},
        ty_def::TyId,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BorrowSummary<'db> {
    /// Whether any admitted path reaches a return.
    pub may_return: bool,
    pub result: ValueId<'db, SourceExpr<'db>>,
    pub mutable_inputs: Vec<InputPoststate<'db>>,
    /// Preconditions on the actual regions supplied by callers. They are
    /// independent of the callee's return value and mutable-input poststates.
    pub requirements: Vec<BoundaryRequirement<'db>>,
    /// Reads and writes through addresses, including effects of transitive calls.
    pub accesses: Vec<MemoryAccess<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemoryAccess<'db> {
    pub kind: MemoryAccessKind,
    pub region: RegionSet<'db>,
    /// Explicit receiver authority for an otherwise unknown memory effect.
    pub authorizers: RegionSet<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BoundaryRule<'db> {
    MemoryTransport(TyId<'db>),
    Writable,
    BorrowedStore(TyId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BoundaryRequirement<'db> {
    pub rule: BoundaryRule<'db>,
    pub instance: SemanticInstance<'db>,
    pub origin: SemOrigin<'db>,
    pub region: RegionSet<'db>,
    /// A storage rule applies only if this supplied capability is populated.
    /// This retains empty-variant precision across forwarding calls.
    pub populated: Option<RegionSet<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InputPoststate<'db> {
    pub destination: SourceExpr<'db>,
    pub value: ValueId<'db, SourceExpr<'db>>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub items: BorrowSummary<'db>,
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
    TransportViolation,
    StorageViolation,
    ProviderProvenanceConflict,
}
