use crate::analysis::semantic::diagnostics::{BlockedSemanticBody, SemanticDiagnosticId};
use salsa::Update;
use std::collections::BTreeSet;

use crate::analysis::{
    semantic::{
        CallSiteProviderRefinement, SStmtId, SemOrigin, SemanticInstance, SemanticInstanceKey,
        capability::{
            footprint::{AccessExtent, AccessFootprint},
            guard::Guard,
            index::BinderScope,
            region::RegionSet,
            separation::SeparationSet,
            source::SourceExpr,
            value::ValueId,
        },
    },
    ty::{corelib::MemoryAccessKind, ty_check::BodyOwner, ty_def::TyId},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BorrowSummary<'db> {
    /// Whether any admitted path reaches a return.
    pub may_return: bool,
    pub result: ValueId<'db, SourceExpr<'db>>,
    /// Scalar facts shared by all normal returns. The Result binder names an
    /// integral return and the Summary choice a boolean return; formal values
    /// and Argument choices name immutable argument SSA values.
    pub scalar_result: Option<Guard<'db>>,
    /// Parameters whose scalar facts this body can read or export, so a caller
    /// keeps the relations of the values it passes for them. `None` is every
    /// parameter, for a summary not derived from the body.
    pub observed_params: Option<ObservedParams>,
    pub mutable_inputs: Vec<InputPoststate<'db>>,
    /// A normal-return must range and its structural contents. The member
    /// binder, guarded coverage, and value share one source-witness namespace.
    pub certified_ranges: Vec<CertifiedRangePoststate<'db>>,
    /// Definite scalar values written to addressable inputs on every normal return.
    pub scalar_inputs: Vec<ScalarInputPoststate<'db>>,
    /// Preconditions on the actual regions supplied by callers. They are
    /// independent of the callee's return value and mutable-input poststates.
    pub requirements: Vec<BoundaryRequirement<'db>>,
    /// Reads and writes through addresses, including effects of transitive calls.
    pub accesses: Vec<MemoryAccess<'db>>,
    pub availability: AvailabilitySummary<'db>,
    /// Conditional overwrites that must be disjoint before a native capability is used.
    pub native_requirements: RegionSet<'db>,
    /// Separation between accesses and borrows live across them that this body
    /// could not prove. Callers establish it physically, never by authority.
    pub loan_requirements: SeparationSet<'db>,
    /// Native validity of the accesses `loan_requirements` relate. Callers
    /// resolve these, like the relations, with only physical separation.
    pub separation_validity: RegionSet<'db>,
}

/// The parameters a body observes, split by whether the observation needs its
/// scalar result: a caller that forgets a dead result's relation leaves the
/// arguments only that relation named unread.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ObservedParams {
    pub unconditional: BTreeSet<u32>,
    pub through_result: BTreeSet<u32>,
}

/// Where a separation requirement arose: a borrow a body held and the access it
/// could not separate from it. Diagnostics resolve both spans from the owning
/// body, so this never enters a summary's semantic equality.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct SeparationOrigin<'db> {
    pub owner: BodyOwner<'db>,
    pub template_owner: BodyOwner<'db>,
    pub borrow: SemOrigin<'db>,
    pub access: SemOrigin<'db>,
}

/// A normal-return ownership transformer, separate from unordered access history.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AvailabilitySummary<'db> {
    pub incoming: Vec<AvailabilityRequirement<'db>>,
    /// Each clause is a guaranteed write, rather than one possible destination.
    pub reinitialized: RegionSet<'db>,
    pub unavailable: RegionSet<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AvailabilityRequirement<'db> {
    /// Write requires an available parent; other accesses require the contents.
    pub kind: MemoryAccessKind,
    pub region: RegionSet<'db>,
    pub extent: AccessExtent<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemoryAccess<'db> {
    pub kind: MemoryAccessKind,
    pub region: RegionSet<'db>,
    pub extent: AccessExtent<'db>,
    /// Explicit receiver authority for an otherwise unknown memory effect.
    pub authorizers: RegionSet<'db>,
}

impl<'db> MemoryAccess<'db> {
    pub fn footprint(&self) -> AccessFootprint<'_, 'db> {
        AccessFootprint {
            region: &self.region,
            extent: self.extent,
        }
    }
}

impl<'db> AvailabilityRequirement<'db> {
    pub fn footprint(&self) -> AccessFootprint<'_, 'db> {
        AccessFootprint {
            region: &self.region,
            extent: self.extent,
        }
    }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CertifiedRangePoststate<'db> {
    pub destination: SourceExpr<'db>,
    pub scope: BinderScope,
    pub coverage: Guard<'db>,
    pub contents: ValueId<'db, SourceExpr<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ScalarInputPoststate<'db> {
    pub destination: SourceExpr<'db>,
    pub value: usize,
}

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub items: BorrowSummary<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowSummaryResult<'db> {
    Ok(Option<BorrowSummaryId<'db>>),
    Pending {
        validation: PendingSemanticValidation<'db>,
        summary: Option<BorrowSummaryId<'db>>,
    },
    Blocked {
        body: BlockedSemanticBody<'db>,
        summary: Option<BorrowSummaryId<'db>>,
    },
    Err(SemanticDiagnosticId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowCheckResult<'db> {
    Ok,
    Pending(PendingSemanticValidation<'db>),
    Blocked(BlockedSemanticBody<'db>),
    Err(SemanticDiagnosticId<'db>),
}

/// The provisional summary of a body together with the provider refinements
/// of its call sites, both read from one provisional solve.
#[derive(Clone, Debug, PartialEq, Eq, Update)]
pub(crate) struct ProvisionalBorrowAnalysis<'db> {
    pub summary: SemanticBorrowSummaryResult<'db>,
    /// `None` when the summary did not come from solving the body; call-site
    /// finalization then solves the body itself.
    pub refinements: Option<CallSiteRefinements<'db>>,
}

/// The provider address spaces a solved provisional body passes to its
/// callees' effect parameters.
#[derive(Clone, Debug, PartialEq, Eq, Update)]
pub(crate) enum CallSiteRefinements<'db> {
    Refined(Vec<CallSiteProviderRefinement>),
    /// Upstream causes block the provisional body.
    Blocked,
    Rejected(SemanticDiagnosticId<'db>),
}

/// The final summary of a body together with the local validation of the same
/// solved body, so the summary and the borrow check share one fixed point.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBorrowAnalysis<'db> {
    pub summary: SemanticBorrowSummaryResult<'db>,
    /// Origins of the summary's loan requirements, in clause order.
    pub provenance: Vec<SeparationOrigin<'db>>,
    /// `None` when the summary did not come from solving the body, e.g. for
    /// intrinsic contracts; the borrow check then solves the body itself.
    pub check: Option<LocalBorrowCheck<'db>>,
}

/// Loan-conflict and availability validation of one solved body. Resolved
/// callees are validated by their own checks.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LocalBorrowCheck<'db> {
    pub result: SemanticBorrowCheckResult<'db>,
    pub callees: Vec<SemanticInstance<'db>>,
    /// `None` when solving the body failed.
    pub executable: Option<ExecutableControlFlow>,
}

/// The control flow a solved body can execute: the solver's divergence and
/// infeasible-edge facts. Block indices and successor positions are those of
/// the raw semantic body, which every normalization of an instance preserves;
/// a diverging call is named by its raw statement.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ExecutableControlFlow(pub Box<[ExecutableBlock]>);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ExecutableBlock {
    Unreachable,
    /// Execution ends at this call, whose callee does not return.
    Diverges(SStmtId),
    /// The terminator executes; each successor edge is feasible or not.
    Continues(Box<[bool]>),
}

/// Obligations that must be discharged by rebuilding the concrete semantic
/// instance. A conservative template summary is not a successful validation.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Update)]
pub struct PendingSemanticValidation<'db> {
    pub callees: BTreeSet<SemanticInstanceKey<'db>>,
}
