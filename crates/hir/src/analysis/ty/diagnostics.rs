use super::{
    adt_def::AdtCycleMember,
    effects::rows::RowKey,
    provider::AddressSpace,
    trait_def::TraitInstId,
    ty_check::{RecordLike, TraitOps},
    ty_def::{BorrowKind, Kind, TyId},
};
use crate::visitor::prelude::*;
use crate::{analysis::HirAnalysisDb, hir_def::Trait};
use crate::{analysis::diagnostics::DiagnosticVoucher, hir_def::PathId};
use crate::{analysis::name_resolution::diagnostics::PathResDiag, hir_def::ItemKind};
use crate::{analysis::ty::ty_check::EffectParamOwner, span::DynLazySpan};
use crate::{
    core::hir_def::{
        CallableDef, CompBinOp, Enum, FieldIndex, FieldParent, Func, GenericParamOwner, IdentId,
        ImplTrait, IntegerId, TypeId as HirTypeId,
    },
    hir_def::TypeAlias,
};
use either::Either;
use salsa::Update;
use smallvec1::SmallVec;
use thin_vec::ThinVec;

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From, Update)]
pub enum FuncBodyDiag<'db> {
    Ty(TyDiagCollection<'db>),
    Body(BodyDiag<'db>),
    NameRes(PathResDiag<'db>),
}

impl<'db> FuncBodyDiag<'db> {
    pub(super) fn to_voucher(&self) -> Box<dyn DiagnosticVoucher + 'db> {
        match self {
            Self::Ty(diag) => diag.to_voucher(),
            Self::Body(diag) => Box::new(diag.clone()) as _,
            Self::NameRes(diag) => Box::new(diag.clone()) as _,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From, Update)]
pub enum TyDiagCollection<'db> {
    Ty(TyLowerDiag<'db>),
    PathRes(PathResDiag<'db>),
    Satisfiability(TraitConstraintDiag<'db>),
    TraitLower(TraitLowerDiag<'db>),
    Impl(ImplDiag<'db>),
}

impl<'db> TyDiagCollection<'db> {
    pub(super) fn to_voucher(&self) -> Box<dyn DiagnosticVoucher + 'db> {
        match self.clone() {
            TyDiagCollection::Ty(diag) => Box::new(diag) as _,
            TyDiagCollection::PathRes(diag) => Box::new(diag) as _,
            TyDiagCollection::Satisfiability(diag) => Box::new(diag) as _,
            TyDiagCollection::TraitLower(diag) => Box::new(diag) as _,
            TyDiagCollection::Impl(diag) => Box::new(diag) as _,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum TyLowerDiag<'db> {
    ExpectedStarKind(DynLazySpan<'db>),
    /// `ref T`/`mut T` outside a parameter or projection return.
    ModeNotType(DynLazySpan<'db>),
    /// A result-space annotation, or an implementation's associated space,
    /// that names no space.
    UnknownResultSpace(DynLazySpan<'db>),
    /// A result-space contract's path, `@p.f[_]`, that names no part of its
    /// root's type, or follows a root that is not a parameter or effect.
    InvalidResultSpacePath(DynLazySpan<'db>),
    /// An `own` parameter or a `mut` yield of a `#[view]` type.
    ViewTypeMode {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        mode: &'static str,
    },
    /// A state-only type, holding the pinned type `collection`, in a
    /// position that would make it a value: `position`.
    StateOnlyValue {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        collection: TyId<'db>,
        position: &'static str,
    },
    InvalidTypeArgKind {
        span: DynLazySpan<'db>,
        expected: Option<Kind>,
        given: TyId<'db>,
    },
    TooManyGenericArgs {
        span: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    StringTooLarge {
        span: DynLazySpan<'db>,
        max: usize,
        given: usize,
    },

    RecursiveType(Vec<AdtCycleMember<'db>>),
    GrowingRecursiveType(Vec<AdtCycleMember<'db>>),

    UnboundTypeAliasParam {
        span: DynLazySpan<'db>,
        alias: TypeAlias<'db>,
        n_given_args: usize,
    },

    TypeAliasCycle {
        cycle: Vec<TypeAlias<'db>>,
    },

    InconsistentKindBound {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        bound: Kind,
    },

    KindBoundNotAllowed(DynLazySpan<'db>),

    GenericParamAlreadyDefinedInParent {
        span: LazyGenericParamSpan<'db>,
        conflict_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    DuplicateArgName(Func<'db>, SmallVec<[u16; 4]>),
    DuplicateFieldName(FieldParent<'db>, SmallVec<[u16; 4]>),
    DuplicateVariantName(Enum<'db>, SmallVec<[u16; 4]>),
    DuplicateGenericParamName(GenericParamOwner<'db>, SmallVec<[u16; 4]>),

    InvalidConstParamTy(DynLazySpan<'db>),
    RecursiveConstParamTy(DynLazySpan<'db>),

    ConstTyMismatch {
        span: DynLazySpan<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },

    ConstTyExpected {
        span: DynLazySpan<'db>,
        expected: TyId<'db>,
    },

    NormalTypeExpected {
        span: DynLazySpan<'db>,
        given: TyId<'db>,
    },

    /// `self: ref Self` is written `self`.
    RefSelfType {
        span: DynLazySpan<'db>,
    },

    /// `var` on a parameter that is not `own`.
    VarParamNotOwned {
        span: DynLazySpan<'db>,
    },

    MixedRefSelfPrefixWithExplicitType {
        span: DynLazySpan<'db>,
    },
    MixedOwnSelfPrefixWithExplicitType {
        span: DynLazySpan<'db>,
    },
    MixedMutSelfPrefixWithExplicitType {
        span: DynLazySpan<'db>,
    },
    /// `var self: own Self`, which the shorthand `var own self` writes.
    InvalidVarSelfWithExplicitType {
        span: DynLazySpan<'db>,
    },

    InvalidConstTyExpr(DynLazySpan<'db>),

    ConstEvalUnsupported(DynLazySpan<'db>),
    ConstEvalAssertionFailed {
        span: DynLazySpan<'db>,
        message: Option<String>,
    },
    ConstEvalNonConstCall(DynLazySpan<'db>),
    ConstEvalDivisionByZero(DynLazySpan<'db>),
    ConstEvalOutOfBounds(DynLazySpan<'db>),
    ConstEvalInvalidOperation {
        span: DynLazySpan<'db>,
        message: String,
    },
    ConstEvalInvalidBorrow(DynLazySpan<'db>),
    ConstEvalInvalidProviderUse(DynLazySpan<'db>),
    ConstEvalVariantMismatch(DynLazySpan<'db>),
    ConstEvalUninitializedLocal(DynLazySpan<'db>),
    ConstEvalInvariant {
        span: DynLazySpan<'db>,
        message: String,
    },
    ConstEvalArithmeticOverflow(DynLazySpan<'db>),
    ConstEvalNegativeExponent(DynLazySpan<'db>),
    ConstEvalStepLimitExceeded(DynLazySpan<'db>),
    ConstEvalRecursionLimitExceeded(DynLazySpan<'db>),
    ConstEvalRecursiveConst(DynLazySpan<'db>),
    TypeLoweringCycle(DynLazySpan<'db>),
    TypeNormalizationLimit(DynLazySpan<'db>),

    NonTrailingDefaultGenericParam(LazyGenericParamSpan<'db>),

    // Default generic parameter diagnostics
    GenericDefaultForwardRef {
        span: LazyGenericParamSpan<'db>,
        name: IdentId<'db>,
    },

    /// A contract field in storage or transient storage holding a memory
    /// pointer, which no later transaction can follow.
    ContractFieldCallPointer {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        pointer: TyId<'db>,
    },

    /// A persistent collection inside a transient one's contents.
    ContractFieldPersistentUnderTransient {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        collection: TyId<'db>,
    },

    /// A type zero is no value of, held in contract state, which starts
    /// zeroed.
    ContractFieldZeroInvalid {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        invalid: TyId<'db>,
    },

    /// A collection entry type in contract state with no storage layout.
    ContractFieldEntryLayout {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        entry: TyId<'db>,
        reason: &'static str,
    },

    ContractFieldLayoutInvariant {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        issue: ContractFieldLayoutIssue,
    },

    /// `#[slot(e)]` whose `e` is not a constant `u256`.
    ContractFieldSlotNotConst {
        span: DynLazySpan<'db>,
    },

    /// `#[slot(e)]` on a field that lives in code, which has no slots.
    ContractFieldSlotInCode {
        span: DynLazySpan<'db>,
    },

    /// Two fields placed with `#[slot]` in one address space share slots.
    ContractFieldSlotOverlap {
        span: DynLazySpan<'db>,
        field: IdentId<'db>,
        other: IdentId<'db>,
    },
    /// A computation written elsewhere faults only for what `site` gives
    /// it, as a valid default a trait reference applies or a function a
    /// constant evaluates.
    RevealedConstFault {
        site: DynLazySpan<'db>,
        revealed_by: RevealedBy,
        fault: Box<TyDiagCollection<'db>>,
    },
}

/// What reveals a fault in a computation written elsewhere.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RevealedBy {
    AppliedDefault,
    Evaluation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractFieldLayoutIssue {
    ExtentOverflow,
    IncompleteProjection,
    InconsistentArrayLength,
}

impl TyLowerDiag<'_> {
    pub(crate) fn local_code(&self) -> u16 {
        match self {
            Self::ExpectedStarKind(_) => 0,
            Self::ModeNotType(_) => 58,
            Self::ViewTypeMode { .. } => 59,
            Self::StateOnlyValue { .. } => 61,
            Self::UnknownResultSpace(_) => 60,
            Self::InvalidResultSpacePath(_) => 67,
            Self::InvalidTypeArgKind { .. } => 1,
            Self::RecursiveType { .. } => 2,
            Self::GrowingRecursiveType(_) => 57,
            Self::UnboundTypeAliasParam { .. } => 3,
            Self::TypeAliasCycle { .. } => 4,
            Self::InconsistentKindBound { .. } => 5,
            Self::KindBoundNotAllowed(_) => 6,
            Self::GenericParamAlreadyDefinedInParent { .. } => 7,
            Self::DuplicateArgName { .. } => 8,
            Self::InvalidConstParamTy { .. } => 9,
            Self::RecursiveConstParamTy { .. } => 10,
            Self::ConstTyMismatch { .. } => 11,
            Self::ConstTyExpected { .. } => 12,
            Self::NormalTypeExpected { .. } => 13,
            Self::RefSelfType { .. } => 14,
            Self::VarParamNotOwned { .. } => 31,
            Self::InvalidConstTyExpr(_) => 15,
            Self::ConstEvalUnsupported(_) => 23,
            Self::ConstEvalAssertionFailed { .. } => 36,
            Self::ConstEvalNonConstCall(_) => 24,
            Self::ConstEvalDivisionByZero(_) => 25,
            Self::ConstEvalOutOfBounds(_) => 50,
            Self::ConstEvalInvalidOperation { .. } => 52,
            Self::ConstEvalInvalidBorrow(_) => 53,
            Self::ConstEvalInvalidProviderUse(_) => 54,
            Self::ConstEvalVariantMismatch(_) => 55,
            Self::ConstEvalUninitializedLocal(_) => 56,
            Self::ConstEvalInvariant { .. } => 51,
            Self::ConstEvalArithmeticOverflow(_) => 34,
            Self::ConstEvalNegativeExponent(_) => 35,
            Self::ConstEvalStepLimitExceeded(_) => 26,
            Self::ConstEvalRecursionLimitExceeded(_) => 27,
            Self::ConstEvalRecursiveConst(_) => 37,
            Self::TypeLoweringCycle(_) => 38,
            Self::TypeNormalizationLimit(_) => 58,
            Self::MixedRefSelfPrefixWithExplicitType { .. } => 28,
            Self::MixedOwnSelfPrefixWithExplicitType { .. } => 29,
            Self::InvalidVarSelfWithExplicitType { .. } => 30,
            Self::MixedMutSelfPrefixWithExplicitType { .. } => 68,
            Self::TooManyGenericArgs { .. } => 16,
            Self::StringTooLarge { .. } => 33,
            Self::DuplicateFieldName(..) => 17,
            Self::DuplicateVariantName(..) => 18,
            Self::DuplicateGenericParamName(..) => 19,
            Self::NonTrailingDefaultGenericParam(_) => 21,
            Self::GenericDefaultForwardRef { .. } => 22,
            Self::ContractFieldLayoutInvariant { .. } => 46,
            Self::ContractFieldSlotNotConst { .. } => 62,
            Self::ContractFieldSlotInCode { .. } => 63,
            Self::ContractFieldSlotOverlap { .. } => 64,
            Self::ContractFieldCallPointer { .. } => 65,
            Self::ContractFieldPersistentUnderTransient { .. } => 66,
            Self::ContractFieldZeroInvalid { .. } => 69,
            Self::ContractFieldEntryLayout { .. } => 70,
            Self::RevealedConstFault { .. } => 71,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct CallConstraintDiagInfo<'db> {
    pub callable_def: CallableDef<'db>,
    pub bound_span: DynLazySpan<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct StaticAssertComparisonValues {
    pub op: CompBinOp,
    pub lhs: String,
    pub rhs: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum MustUseSubject<'db> {
    Type(TyId<'db>),
    Function(CallableDef<'db>),
}

/// How to make writable a binding that is assigned through.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum MutabilityFix {
    /// A value binding: `var x`.
    Var,
    /// A plain name taking a value from a mutable place or grant: `mut x`
    /// writes through to it.
    MutComponent,
    /// A read access: the mutable one writes.
    ReadAccess,
    /// An owned parameter: `var x: own T`.
    OwnParam,
    /// A view parameter: `x: mut T`, a mutable borrow.
    ViewParam,
    /// A view receiver: `mut self`.
    ViewSelf,
    /// An owned receiver: `var own self`.
    OwnSelf,
    /// An effect binding: `uses (mut x)`.
    Effect,
    /// A calldata pointer: calldata is read-only input.
    Calldata,
}

/// Where an access marker stands in place of the names it belongs on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum MarkerPosition {
    /// A `match`, `if let` or `while let` scrutinee.
    Scrutinee,
    /// The initializer of a destructuring `let`.
    Initializer,
    /// A `for` loop's base.
    LoopBase,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum BodyDiag<'db> {
    TypeMismatch {
        span: DynLazySpan<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },
    InfiniteOccurrence(DynLazySpan<'db>),

    DuplicatedBinding {
        primary: DynLazySpan<'db>,
        conflicat_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    },
    BindingsInOrPat(DynLazySpan<'db>),
    DuplicatedRestPat(DynLazySpan<'db>),
    UnexpectedRestPat(DynLazySpan<'db>),

    InvalidPathDomainInPat {
        primary: DynLazySpan<'db>,
        resolved: Option<DynLazySpan<'db>>,
    },

    UnitVariantExpected {
        primary: DynLazySpan<'db>,
        kind_name: String,
        hint: Option<String>,
    },

    TupleVariantExpected {
        primary: DynLazySpan<'db>,
        kind_name: Option<String>,
        hint: Option<String>,
    },

    RecordExpected {
        primary: DynLazySpan<'db>,
        kind_name: Option<String>,
        hint: Option<String>,
    },

    MismatchedFieldCount {
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    DuplicatedRecordFieldBind {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    // TODO: capture type
    RecordFieldNotFound {
        span: DynLazySpan<'db>,
        label: IdentId<'db>,
    },

    ExplicitLabelExpectedInRecord {
        primary: DynLazySpan<'db>,
        hint: Option<String>,
    },

    MissingRecordFields {
        primary: DynLazySpan<'db>,
        missing_fields: Vec<IdentId<'db>>,
        hint: Option<String>,
    },

    /// The third field is set when the enclosing `impl` or `trait` defines a
    /// function of the same name: the container kind (`"impl"` or `"trait"`)
    /// and whether that function takes `self`.
    UndefinedVariable(DynLazySpan<'db>, IdentId<'db>, Option<(&'static str, bool)>),

    /// A `recv` arm names a contract field its `uses` clause does not.
    ContractFieldNotInUses {
        primary: DynLazySpan<'db>,
        field: IdentId<'db>,
    },

    InvalidEffectKey {
        owner: EffectParamOwner<'db>,
        key: HirTypeId<'db>,
        idx: usize,
    },

    ContractRootEffectTraitNotImplemented {
        owner: EffectParamOwner<'db>,
        idx: usize,
        root_ty: TyId<'db>,
        trait_req: TraitInstId<'db>,
    },

    ContractRootEffectTypeNotZeroSized {
        owner: EffectParamOwner<'db>,
        key: HirTypeId<'db>,
        idx: usize,
        given: TyId<'db>,
    },

    MissingEffect {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
    },

    /// A call needs an abstract row, which only the caller's own can supply.
    MissingRow {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        row: RowKey<'db>,
    },

    AmbiguousEffect {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
    },

    EffectMutabilityMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectTypeMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectProviderMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectTraitUnsatisfied {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTypeId<'db>,
        trait_req: TraitInstId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    WithEffectTraitUnsatisfied {
        primary: DynLazySpan<'db>,
        key: PathId<'db>,
        trait_req: TraitInstId<'db>,
        given: TyId<'db>,
    },

    WithEffectTypeUnsatisfied {
        primary: DynLazySpan<'db>,
        key: PathId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },

    ReturnedTypeMismatch {
        primary: DynLazySpan<'db>,
        actual: TyId<'db>,
        expected: TyId<'db>,
        func: Option<CallableDef<'db>>,
    },

    TypeMustBeKnown(DynLazySpan<'db>),
    TypeSizeOverflow {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },
    /// `storage_slots` of a type with no storage layout, for `reason`.
    NoStorageLayout {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
        reason: &'static str,
    },
    ConstValueMustBeKnown(DynLazySpan<'db>),
    ConstDependencyMustBeKnown {
        primary: DynLazySpan<'db>,
        dependency: String,
    },
    ConstEvaluationFailed {
        primary: DynLazySpan<'db>,
        const_name: String,
        origin: String,
        reason: String,
    },
    /// A constant in a callee's return type fails to evaluate for the generic
    /// arguments of this call, as `S<{N - 5}>` does at `N = 2`. `fault` is the
    /// evaluation failure, reported at the constant.
    CallReturnTypeConstFault {
        call: DynLazySpan<'db>,
        fault: Box<TyDiagCollection<'db>>,
    },
    WhereConstPredicateFailed(DynLazySpan<'db>),
    WhereTypeBoundMissing(DynLazySpan<'db>),
    GenericConstPredicateUnsupported(DynLazySpan<'db>),
    RecursiveConstRequirement(DynLazySpan<'db>),
    /// A use whose const requirement does not hold, reported by the
    /// requirement checks after inference for calls and types alike.
    ConstRequirementNotSatisfied {
        primary: DynLazySpan<'db>,
        predicate: DynLazySpan<'db>,
        reason: crate::analysis::ty::ty_check::RequirementFailure,
        /// The failing type, when only inference gives it at `primary`, so
        /// that nothing written there shows it.
        inferred: Option<TyId<'db>>,
    },
    StaticAssertFailed {
        primary: DynLazySpan<'db>,
        comparison: Option<StaticAssertComparisonValues>,
    },

    InvalidCast {
        primary: DynLazySpan<'db>,
        from: TyId<'db>,
        to: TyId<'db>,
        hint: Option<String>,
    },

    AccessedFieldNotFound {
        primary: DynLazySpan<'db>,
        given_ty: TyId<'db>,
        index: FieldIndex<'db>,
    },

    OpsTraitNotImplemented {
        span: DynLazySpan<'db>,
        ty: String,
        op: IdentId<'db>,
        trait_path: PathId<'db>,
    },
    UnsupportedUnaryPlus(DynLazySpan<'db>),
    UnsupportedMacroCall(DynLazySpan<'db>),
    IntLiteralOutOfRange {
        primary: DynLazySpan<'db>,
        literal: String,
        ty: TyId<'db>,
    },

    BorrowFromNonPlace {
        primary: DynLazySpan<'db>,
    },

    CannotBorrowMut {
        primary: DynLazySpan<'db>,
        binding: Option<(IdentId<'db>, DynLazySpan<'db>)>,
    },

    /// A call argument is not a place, but the callee requires a borrow handle (`mut`/`ref`).
    BorrowArgMustBePlace {
        primary: DynLazySpan<'db>,
        kind: BorrowKind,
    },

    /// A call argument is a place, but the callee requires an explicit borrow handle (`mut`/`ref`).
    ExplicitBorrowRequired {
        primary: DynLazySpan<'db>,
        kind: BorrowKind,
        suggestion: Option<String>,
    },

    /// A plain binding of a projection's read result that is neither `Copy`
    /// nor a `#[view]` type: it has no value reading, so it needs `ref`.
    RefBindingRequired {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    /// A plain `let` of a projection's whole `mut` result of a type with
    /// values: a plain binding takes a value, and a value would discard the
    /// writes the call asks for, so the access is written `let x = mut …`.
    MutBindingRequired {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    /// A `ref` or `mut` binding of an owned value, which has no access to
    /// hold. `marker` is `true` for `mut`.
    MarkedBindingWithoutAccess {
        primary: DynLazySpan<'db>,
        marker: bool,
    },

    /// `var x` copying a projection's `mut` result: writes to the copy do not
    /// reach the place.
    MutAccessCopied {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    /// `var x = ref p` or `var x = mut p`: `var` binds a value, never an
    /// access.
    VarOfAccess {
        primary: DynLazySpan<'db>,
    },

    /// `mut x` on a read component, which has no mutable place behind it.
    MutBindingOfRead {
        primary: DynLazySpan<'db>,
    },

    /// `var x` binding a type with no value reading (`#[view]` or
    /// state-only), which has no value to hold.
    VarOfNoValue {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    /// `ref p` or `mut p` as the scrutinee or initializer of a pattern with
    /// components, or as a loop's base: the marker belongs on the plain
    /// `names` the pattern binds. `marker` is `true` for `mut`.
    InitializerMarker {
        primary: DynLazySpan<'db>,
        marker: bool,
        position: MarkerPosition,
        names: Vec<(IdentId<'db>, DynLazySpan<'db>)>,
    },

    /// `ref p`, `mut p`, or a tuple or sum yield shape used as a value.
    AccessNotValue {
        primary: DynLazySpan<'db>,
    },

    /// An `if` or `match` outside a projection's yield whose branches end in
    /// accesses: a runtime choice between accesses.
    AccessChoiceNeedsProjection {
        primary: DynLazySpan<'db>,
    },

    /// A tuple of projection grants outside an unsafe split or a yield.
    TupleOfAccesses {
        primary: DynLazySpan<'db>,
    },

    /// An unsafe split component calling a projection not declared
    /// `#[raw_place]`.
    UnsafeSplitOperand {
        primary: DynLazySpan<'db>,
        callee: IdentId<'db>,
    },

    /// A tuple or sum yield shape bound to a single name.
    ShapeNotDestructured {
        primary: DynLazySpan<'db>,
    },

    /// A projection's yield site does not grant its return shape.
    InvalidYield {
        primary: DynLazySpan<'db>,
        shape: String,
    },

    /// `yield` in a function that is not a projection.
    YieldOutsideProjection {
        primary: DynLazySpan<'db>,
    },

    /// A `let` without `else` whose pattern may not match.
    RefutableLetPattern {
        primary: DynLazySpan<'db>,
    },

    /// The `else` block of a `let … else` can complete.
    LetElseMustDiverge {
        primary: DynLazySpan<'db>,
    },

    /// `?` on a value that is neither an `Option` nor a `Result`.
    TryOnNonSum {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    /// `?` in a function whose return cannot carry the operand's `None` or
    /// `Err`.
    TryReturnMismatch {
        primary: DynLazySpan<'db>,
        operand: TyId<'db>,
        ret: TyId<'db>,
    },

    /// A call instantiates its callee at a `#[view]` type in a position a
    /// view type cannot take.
    ViewInstantiation {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
        position: &'static str,
    },

    /// Array repetition literals (`[x; N]`) duplicate the element value.
    ///
    /// Duplicating a value requires that the element type implement `core::marker::Copy`.
    ArrayRepeatRequiresCopy {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    },

    ArrayIndexOutOfBounds {
        primary: DynLazySpan<'db>,
        index: IntegerId<'db>,
        len: usize,
    },

    NonAssignableExpr(DynLazySpan<'db>),

    ImmutableAssignment {
        primary: DynLazySpan<'db>,
        binding: Option<(IdentId<'db>, DynLazySpan<'db>, MutabilityFix)>,
    },

    ImmutableContractFieldNotInitialized {
        primary: DynLazySpan<'db>,
        field: IdentId<'db>,
        init: Option<DynLazySpan<'db>>,
    },
    UnsupportedContractFieldAddressSpace {
        primary: DynLazySpan<'db>,
        field: IdentId<'db>,
        space: AddressSpace,
    },
    ImmutableContractFieldMutBinding {
        primary: DynLazySpan<'db>,
        field: IdentId<'db>,
        field_span: DynLazySpan<'db>,
    },

    LoopControlOutsideOfLoop {
        primary: DynLazySpan<'db>,
        is_break: bool,
    },

    TraitNotImplemented {
        primary: DynLazySpan<'db>,
        ty: String,
        trait_name: IdentId<'db>,
    },

    NotCallable(DynLazySpan<'db>, TyId<'db>),
    /// A method call `h.f(..)` whose receiver has no method `f` but a field
    /// `f` holding a callable value, which `(h.f)(..)` calls. `receiver` is
    /// the receiver's name when it is one.
    CallableFieldCalledAsMethod {
        primary: DynLazySpan<'db>,
        field: IdentId<'db>,
        receiver_ty: TyId<'db>,
        receiver: Option<IdentId<'db>>,
    },

    NotAMethod {
        span: LazyMethodCallExprSpan<'db>,
        receiver_ty: TyId<'db>,
        func_name: IdentId<'db>,
        func_ty: TyId<'db>,
    },

    CallGenericArgNumMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: usize,
        expected: usize,
    },

    CallArgNumMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: usize,
        expected: usize,
    },

    AssertArgNumMismatch {
        primary: DynLazySpan<'db>,
        given: usize,
    },

    AssertMessageMustBeStringLiteral {
        primary: DynLazySpan<'db>,
    },

    CallArgLabelMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: Option<IdentId<'db>>,
        expected: IdentId<'db>,
    },

    AmbiguousInherentMethodCall {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        candidates: ThinVec<CallableDef<'db>>,
    },

    AmbiguousTrait {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        traits: ThinVec<TraitInstId<'db>>,
    },

    AmbiguousTraitInst {
        primary: DynLazySpan<'db>,
        cands: ThinVec<TraitInstId<'db>>,
        required_by: Option<CallConstraintDiagInfo<'db>>,
    },

    InvisibleAmbiguousTrait {
        primary: DynLazySpan<'db>,
        traits: ThinVec<Trait<'db>>,
    },

    NotValue {
        primary: DynLazySpan<'db>,
        given: Either<ItemKind<'db>, TyId<'db>>,
    },

    TypeAnnotationNeeded {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
    },
    UnusedMustUse {
        primary: DynLazySpan<'db>,
        subject: MustUseSubject<'db>,
    },

    NonExhaustiveMatch {
        primary: DynLazySpan<'db>,
        scrutinee_ty: TyId<'db>,
        missing_patterns: Vec<String>, // Text representation of missing patterns
    },
    UnreachablePattern {
        primary: DynLazySpan<'db>,
    },
    PatternAnalysisInconclusive {
        primary: DynLazySpan<'db>,
        reason: String,
    },

    /// The root path of a recv block doesn't refer to a msg type
    RecvExpectedMsgType {
        primary: DynLazySpan<'db>,
        given: TyId<'db>,
    },

    /// The root path of a recv block names a module-like scope that isn't a msg module
    RecvExpectedMsgModule {
        primary: DynLazySpan<'db>,
        given: IdentId<'db>,
        given_kind: &'static str, // can be "file module", "function", "constant", etc...
    },

    /// A recv arm pattern is not a variant of the expected msg type
    RecvArmNotMsgVariant {
        primary: DynLazySpan<'db>,
        msg_name: IdentId<'db>,
    },

    /// A recv arm return type annotation is required and must match the msg variant
    RecvArmRetTypeMissing {
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
    },

    /// A recv arm pattern is duplicated for the same msg variant
    RecvArmDuplicateVariant {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        variant: IdentId<'db>,
    },

    /// Some msg variants are not covered in the recv block
    RecvMissingMsgVariants {
        primary: DynLazySpan<'db>,
        variants: Vec<IdentId<'db>>,
    },

    /// Duplicate recv blocks for the same msg type
    RecvDuplicateMsgBlock {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        msg_name: IdentId<'db>,
    },

    /// Multiple msg variants across recv blocks have the same selector value
    RecvDuplicateSelector {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        selector: u32,
        first_variant: IdentId<'db>,
        second_variant: IdentId<'db>,
    },

    /// A recv arm pattern resolves to a type that implements MsgVariant,
    /// but is not a variant of the specified msg module
    RecvArmNotVariantOfMsg {
        primary: DynLazySpan<'db>,
        variant_ty: TyId<'db>,
        msg_name: IdentId<'db>,
    },

    /// A recv arm pattern resolves to a type that does not implement MsgVariant
    RecvArmNotMsgVariantTrait {
        primary: DynLazySpan<'db>,
        given_ty: TyId<'db>,
    },

    /// The same message handler type is handled multiple times across recv blocks
    RecvDuplicateHandler {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        handler_ty: TyId<'db>,
    },

    /// A fallback arm `_` is only allowed in a bare recv block.
    RecvFallbackOnlyInBareBlock {
        primary: DynLazySpan<'db>,
    },

    /// At most one fallback arm may exist in a contract.
    RecvDuplicateFallback {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
    },

    /// Fallback arms are unit-returning and may not declare `-> T`.
    RecvFallbackReturnTypeNotAllowed {
        primary: DynLazySpan<'db>,
    },

    // Const fn / const-check diagnostics -----------------------------------
    ConstFnEffectsNotAllowed(DynLazySpan<'db>),
    ConstFnNonConstCall {
        primary: DynLazySpan<'db>,
        callee: CallableDef<'db>,
    },
    ConstFnEffectfulCall {
        primary: DynLazySpan<'db>,
        callee: CallableDef<'db>,
    },

    // Unsafe-check diagnostics ---------------------------------------------
    UnsafeDerefRequiresUnsafe {
        primary: DynLazySpan<'db>,
    },
    UnsafeCallRequiresUnsafe {
        primary: DynLazySpan<'db>,
        callee: CallableDef<'db>,
    },
    /// Initializing (`write` false) or writing an `unsafe` field outside an
    /// unsafe context.
    UnsafeFieldRequiresUnsafe {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
        write: bool,
    },
    /// A `with` provider naming existing storage or transient state that the
    /// function's own effects do not cover.
    /// An access through a state pointer the body's own effects do not
    /// cover.
    UncoveredStateAccess {
        primary: DynLazySpan<'db>,
        is_mut: bool,
        in_closure: bool,
    },

    /// A closure in a constant context, which cannot evaluate it.
    ClosureInConstContext {
        primary: DynLazySpan<'db>,
    },

    /// A closure whose parameter count differs from the callable shape it
    /// is passed as.
    ClosureArityMismatch {
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    /// A write to, or a `mut` access of, a binding a closure captures: the
    /// closure has its own copy, read-only.
    WriteToCapture {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    /// A closure capturing an access (a view, `ref` or `mut` binding or
    /// parameter) of a non-`Copy` value, which it can neither copy nor move.
    AccessCapture {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    /// A two-base loop binding one item `mut` and the other not.
    MixedLoopBases {
        primary: DynLazySpan<'db>,
    },
}

impl<'db> BodyDiag<'db> {
    pub(super) fn unit_variant_expected(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: RecordLike<'db>,
    ) -> Self {
        let kind_name = record_like.kind_name(db);
        let hint = record_like.initializer_hint(db);

        Self::UnitVariantExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn tuple_variant_expected(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: Option<RecordLike<'db>>,
    ) -> Self {
        let (kind_name, hint) = if let Some(record_like) = record_like {
            (
                Some(record_like.kind_name(db)),
                record_like.initializer_hint(db),
            )
        } else {
            (None, None)
        };

        Self::TupleVariantExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn record_expected(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: Option<RecordLike<'db>>,
    ) -> Self {
        let (kind_name, hint) = if let Some(record_like) = record_like {
            (
                Some(record_like.kind_name(db)),
                record_like.initializer_hint(db),
            )
        } else {
            (None, None)
        };

        Self::RecordExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn ops_trait_not_implemented(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        ops: &dyn TraitOps,
    ) -> Self {
        let ty = ty.pretty_print(db).to_string();
        let op = ops.op_symbol(db);
        let trait_path = ops.core_trait_path(db);
        Self::OpsTraitNotImplemented {
            span,
            ty,
            op,
            trait_path,
        }
    }

    pub(crate) fn local_code(&self) -> u16 {
        match self {
            Self::TypeMismatch { .. } => 0,
            Self::InfiniteOccurrence(..) => 1,
            Self::DuplicatedRestPat(..) => 2,
            Self::UnexpectedRestPat(..) => 75,
            Self::BindingsInOrPat(..) => 76,
            Self::InvalidPathDomainInPat { .. } => 3,
            Self::UnitVariantExpected { .. } => 4,
            Self::TupleVariantExpected { .. } => 5,
            Self::RecordExpected { .. } => 6,
            Self::MismatchedFieldCount { .. } => 7,
            Self::DuplicatedRecordFieldBind { .. } => 8,
            Self::RecordFieldNotFound { .. } => 9,
            Self::ExplicitLabelExpectedInRecord { .. } => 10,
            Self::MissingRecordFields { .. } => 11,
            Self::UndefinedVariable(..) => 12,
            Self::InvalidEffectKey { .. } => 51,
            Self::ContractRootEffectTraitNotImplemented { .. } => 53,
            Self::ContractRootEffectTypeNotZeroSized { .. } => 54,
            Self::MissingEffect { .. } => 36,
            Self::EffectMutabilityMismatch { .. } => 37,
            Self::EffectTypeMismatch { .. } => 38,
            Self::EffectProviderMismatch { .. } => 52,
            Self::EffectTraitUnsatisfied { .. } => 39,
            Self::WithEffectTraitUnsatisfied { .. } => 75,
            Self::WithEffectTypeUnsatisfied { .. } => 76,
            Self::AmbiguousEffect { .. } => 40,
            Self::ReturnedTypeMismatch { .. } => 13,
            Self::TypeMustBeKnown(..) => 14,
            Self::TypeSizeOverflow { .. } => 88,
            Self::NoStorageLayout { .. } => 128,
            Self::InvalidCast { .. } => 55,
            Self::ConstValueMustBeKnown(..) => 64,
            Self::ConstDependencyMustBeKnown { .. } => 90,
            Self::ConstEvaluationFailed { .. } => 89,
            Self::CallReturnTypeConstFault { .. } => 96,
            Self::WhereConstPredicateFailed(..) => 91,
            Self::GenericConstPredicateUnsupported(..) => 92,
            Self::RecursiveConstRequirement(..) => 94,
            Self::ConstRequirementNotSatisfied { .. } => 93,
            Self::WhereTypeBoundMissing(..) => 95,
            Self::StaticAssertFailed { .. } => 81,
            Self::AccessedFieldNotFound { .. } => 15,
            Self::OpsTraitNotImplemented { .. } => 16,
            Self::UnsupportedUnaryPlus(..) => 52,
            Self::UnsupportedMacroCall(..) => 89,
            Self::IntLiteralOutOfRange { .. } => 74,
            Self::BorrowFromNonPlace { .. } => 65,
            Self::CannotBorrowMut { .. } => 66,
            Self::BorrowArgMustBePlace { .. } => 68,
            Self::ExplicitBorrowRequired { .. } => 69,
            Self::AccessNotValue { .. } => 70,
            Self::ShapeNotDestructured { .. } => 72,
            Self::InvalidYield { .. } => 99,
            Self::YieldOutsideProjection { .. } => 100,
            Self::RefutableLetPattern { .. } => 101,
            Self::LetElseMustDiverge { .. } => 102,
            Self::TryOnNonSum { .. } => 103,
            Self::TryReturnMismatch { .. } => 104,
            Self::ViewInstantiation { .. } => 105,
            Self::MissingRow { .. } => 106,
            Self::UncoveredStateAccess { .. } => 107,
            Self::ClosureInConstContext { .. } => 108,
            Self::ClosureArityMismatch { .. } => 109,
            Self::WriteToCapture { .. } => 110,
            Self::AccessCapture { .. } => 111,
            Self::MixedLoopBases { .. } => 114,
            Self::TupleOfAccesses { .. } => 115,
            Self::UnsafeSplitOperand { .. } => 118,
            Self::RefBindingRequired { .. } => 119,
            Self::MutBindingRequired { .. } => 129,
            Self::MarkedBindingWithoutAccess { .. } => 120,
            Self::MutAccessCopied { .. } => 121,
            Self::VarOfAccess { .. } => 122,
            Self::MutBindingOfRead { .. } => 123,
            Self::InitializerMarker { .. } => 124,
            Self::VarOfNoValue { .. } => 125,
            Self::UnsafeFieldRequiresUnsafe { .. } => 126,
            Self::CallableFieldCalledAsMethod { .. } => 127,
            Self::ContractFieldNotInUses { .. } => 116,
            Self::AccessChoiceNeedsProjection { .. } => 117,
            Self::ArrayRepeatRequiresCopy { .. } => 71,
            Self::ArrayIndexOutOfBounds { .. } => 84,
            Self::NonAssignableExpr(..) => 17,
            Self::ImmutableAssignment { .. } => 18,
            Self::ImmutableContractFieldNotInitialized { .. } => 86,
            Self::UnsupportedContractFieldAddressSpace { .. } => 84,
            Self::ImmutableContractFieldMutBinding { .. } => 85,
            Self::LoopControlOutsideOfLoop { .. } => 19,
            Self::TraitNotImplemented { .. } => 20,
            Self::NotCallable(..) => 21,
            Self::CallGenericArgNumMismatch { .. } => 22,
            Self::CallArgNumMismatch { .. } => 23,
            Self::AssertArgNumMismatch { .. } => 82,
            Self::AssertMessageMustBeStringLiteral { .. } => 83,
            Self::CallArgLabelMismatch { .. } => 24,
            Self::AmbiguousInherentMethodCall { .. } => 25,
            Self::AmbiguousTrait { .. } => 26,
            Self::AmbiguousTraitInst { .. } => 27,
            Self::InvisibleAmbiguousTrait { .. } => 28,
            Self::NotValue { .. } => 30,
            Self::TypeAnnotationNeeded { .. } => 31,
            Self::UnusedMustUse { .. } => 82,
            Self::DuplicatedBinding { .. } => 32,
            Self::NotAMethod { .. } => 33,
            Self::NonExhaustiveMatch { .. } => 34,
            Self::UnreachablePattern { .. } => 35,
            Self::PatternAnalysisInconclusive { .. } => 87,
            Self::RecvExpectedMsgType { .. } => 41,
            Self::RecvExpectedMsgModule { .. } => 90,
            Self::RecvArmNotMsgVariant { .. } => 42,
            Self::RecvArmRetTypeMissing { .. } => 43,
            Self::RecvArmDuplicateVariant { .. } => 44,
            Self::RecvMissingMsgVariants { .. } => 45,
            Self::RecvDuplicateMsgBlock { .. } => 46,
            Self::RecvDuplicateSelector { .. } => 47,
            Self::RecvArmNotVariantOfMsg { .. } => 48,
            Self::RecvArmNotMsgVariantTrait { .. } => 49,
            Self::RecvDuplicateHandler { .. } => 50,
            Self::RecvFallbackOnlyInBareBlock { .. } => 78,
            Self::RecvDuplicateFallback { .. } => 79,
            Self::RecvFallbackReturnTypeNotAllowed { .. } => 80,
            Self::ConstFnEffectsNotAllowed(_) => 55,
            Self::ConstFnNonConstCall { .. } => 62,
            Self::ConstFnEffectfulCall { .. } => 63,
            Self::UnsafeDerefRequiresUnsafe { .. } => 96,
            Self::UnsafeCallRequiresUnsafe { .. } => 97,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum TraitLowerDiag<'db> {
    ConflictTraitImpl {
        primary: ImplTrait<'db>,
        conflict_with: ImplTrait<'db>,
    },
    ExternalTraitForExternalType(ImplTrait<'db>),
    UnsafeLocalBoundBlanketImpl(ImplTrait<'db>),
    CyclicTraitRef(ImplTrait<'db>),
    CyclicSuperTraits(Vec<Trait<'db>>),
    /// A trait's effect row or result space with a value: each
    /// implementation gives its own.
    AccessItemDefault {
        primary: DynLazySpan<'db>,
        kind: &'static str,
        name: IdentId<'db>,
    },
    /// An impl defines its associated types in its body, so its header has
    /// nothing to bind.
    ImplHeaderAssocTypeBinding {
        span: DynLazySpan<'db>,
        name: IdentId<'db>,
    },
}

impl TraitLowerDiag<'_> {
    pub fn local_code(&self) -> u16 {
        match self {
            Self::ExternalTraitForExternalType(_) => 0,
            Self::ConflictTraitImpl { .. } => 1,
            Self::CyclicSuperTraits { .. } => 2,
            Self::CyclicTraitRef(_) => 3,
            Self::UnsafeLocalBoundBlanketImpl(_) => 4,
            Self::AccessItemDefault { .. } => 5,
            Self::ImplHeaderAssocTypeBinding { .. } => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum TraitConstraintDiag<'db> {
    KindMismatch {
        primary: LazyTraitRefSpan<'db>,
        trait_def: Trait<'db>,
    },

    TraitArgNumMismatch {
        span: LazyTraitRefSpan<'db>,
        expected: usize,
        given: usize,
    },

    TraitArgKindMismatch {
        span: LazyTraitRefSpan<'db>,
        expected: Kind,
        actual: TyId<'db>,
    },

    TraitBoundNotSat {
        span: DynLazySpan<'db>,
        primary_goal: TraitInstId<'db>,
        unsat_subgoal: Option<TraitInstId<'db>>,
        required_by: Option<CallConstraintDiagInfo<'db>>,
    },

    InfiniteBoundRecursion(DynLazySpan<'db>, String),

    ConcreteTypeBound(DynLazySpan<'db>, TyId<'db>),

    ConstTyBound(DynLazySpan<'db>, TyId<'db>),
}

impl TraitConstraintDiag<'_> {
    pub fn local_code(&self) -> u16 {
        match self {
            Self::KindMismatch { .. } => 0,
            Self::TraitArgNumMismatch { .. } => 1,
            Self::TraitArgKindMismatch { .. } => 2,
            Self::TraitBoundNotSat { .. } => 3,
            Self::InfiniteBoundRecursion(..) => 4,
            Self::ConcreteTypeBound(..) => 5,
            Self::ConstTyBound(..) => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ImplDiag<'db> {
    ConflictMethodImpl {
        primary: CallableDef<'db>,
        conflict_with: CallableDef<'db>,
    },

    MethodNotDefinedInTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        method_name: IdentId<'db>,
    },

    NotAllTraitItemsImplemented {
        primary: DynLazySpan<'db>,
        not_implemented: ThinVec<IdentId<'db>>,
    },

    MethodTypeParamNumMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
    },

    MethodTypeParamKindMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
        param_idx: usize,
    },

    MethodArgNumMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
    },

    MethodArgLabelMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
        param_idx: usize,
    },

    MethodArgTyMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
        trait_m_ty: TyId<'db>,
        impl_m_ty: TyId<'db>,
        param_idx: usize,
    },

    MethodRetTyMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
        trait_ty: TyId<'db>,
        impl_ty: TyId<'db>,
    },

    MethodEffectMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
    },

    MethodUnsafeMismatch {
        trait_m: CallableDef<'db>,
        impl_m: CallableDef<'db>,
    },

    MethodStricterBound {
        span: DynLazySpan<'db>,
        stricter_bounds: ThinVec<TraitInstId<'db>>,
    },

    InvalidSelfType {
        span: DynLazySpan<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },

    InherentImplIsNotAllowed {
        primary: DynLazySpan<'db>,
        ty: String,
        is_nominal: bool,
    },

    TypeNotDefinedInTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        type_name: IdentId<'db>,
    },

    MissingAssociatedType {
        primary: DynLazySpan<'db>,
        type_name: IdentId<'db>,
        trait_: Trait<'db>,
    },

    MissingAssociatedConstValue {
        primary: DynLazySpan<'db>,
        const_name: IdentId<'db>,
        trait_: Trait<'db>,
    },

    ConstNotDefinedInTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        const_name: IdentId<'db>,
    },

    MissingAssociatedConst {
        primary: DynLazySpan<'db>,
        const_name: IdentId<'db>,
        trait_: Trait<'db>,
    },

    ConstTyMismatchWithTrait {
        primary: DynLazySpan<'db>,
        trait_decl_span: DynLazySpan<'db>,
        const_name: IdentId<'db>,
        trait_ty: TyId<'db>,
        impl_ty: TyId<'db>,
    },

    RecursiveAssocConst {
        primary: DynLazySpan<'db>,
        const_name: IdentId<'db>,
    },

    InherentConstMissingValue {
        primary: DynLazySpan<'db>,
        const_name: IdentId<'db>,
    },

    InherentConstConflict {
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
        const_name: IdentId<'db>,
    },

    InherentConstShadowsVariant {
        primary: DynLazySpan<'db>,
        variant_span: DynLazySpan<'db>,
        const_name: IdentId<'db>,
    },

    InherentConstShadowsFn {
        primary: DynLazySpan<'db>,
        fn_span: DynLazySpan<'db>,
        const_name: IdentId<'db>,
    },

    /// An effect row or result space the trait does not declare.
    AccessItemNotDefinedInTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        kind: &'static str,
        name: IdentId<'db>,
    },

    MissingAssociatedSpace {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        name: IdentId<'db>,
    },

    /// An `impl` of a trait only the compiler implements.
    CompilerImplementedTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
    },

    /// A `Pinned` implementation whose space is not a state space, nor
    /// another pinned type's.
    PinNotStateSpace { primary: DynLazySpan<'db> },

    /// An implementation of an `unsafe trait` without `unsafe impl`, or an
    /// `unsafe impl` of a safe trait.
    UnsafeImplMismatch {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
    },

    /// A `Copy` implementation for a type with a field that is not `Copy`
    /// under the implementation's bounds.
    CopyFieldNotCopy {
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
        field_ty: TyId<'db>,
    },

    /// A `Copy` implementation for a bare type parameter, which covers types
    /// whose fields, views and state the checks of a `Copy` type never see.
    BlanketCopy { primary: DynLazySpan<'db> },

    /// An associated space whose value, followed through the spaces it names,
    /// comes back to itself.
    CyclicAssociatedSpace {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    /// An effect row whose components, expanded through the rows they name,
    /// come back to it.
    CyclicEffectRow {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
    },
}

impl ImplDiag<'_> {
    pub fn local_code(&self) -> u16 {
        match self {
            Self::ConflictMethodImpl { .. } => 0,
            Self::MethodNotDefinedInTrait { .. } => 1,
            Self::NotAllTraitItemsImplemented { .. } => 2,
            Self::MethodTypeParamNumMismatch { .. } => 3,
            Self::MethodTypeParamKindMismatch { .. } => 4,
            Self::MethodArgNumMismatch { .. } => 5,
            Self::MethodArgLabelMismatch { .. } => 6,
            Self::MethodArgTyMismatch { .. } => 7,
            Self::MethodRetTyMismatch { .. } => 8,
            Self::MethodStricterBound { .. } => 9,
            Self::MethodEffectMismatch { .. } => 24,
            Self::MethodUnsafeMismatch { .. } => 25,
            Self::InvalidSelfType { .. } => 10,
            Self::InherentImplIsNotAllowed { .. } => 11,
            Self::MissingAssociatedType { .. } => 12,
            Self::MissingAssociatedConstValue { .. } => 13,
            Self::ConstNotDefinedInTrait { .. } => 14,
            Self::MissingAssociatedConst { .. } => 15,
            Self::ConstTyMismatchWithTrait { .. } => 16,
            Self::RecursiveAssocConst { .. } => 17,
            Self::InherentConstMissingValue { .. } => 18,
            Self::InherentConstConflict { .. } => 19,
            Self::InherentConstShadowsVariant { .. } => 20,
            Self::InherentConstShadowsFn { .. } => 21,
            Self::TypeNotDefinedInTrait { .. } => 23,
            Self::AccessItemNotDefinedInTrait { .. } => 26,
            Self::MissingAssociatedSpace { .. } => 27,
            Self::CompilerImplementedTrait { .. } => 28,
            Self::PinNotStateSpace { .. } => 29,
            Self::UnsafeImplMismatch { .. } => 30,
            Self::CopyFieldNotCopy { .. } => 31,
            Self::BlanketCopy { .. } => 32,
            Self::CyclicAssociatedSpace { .. } => 33,
            Self::CyclicEffectRow { .. } => 34,
        }
    }
}

pub struct DefConflictError<'db>(pub SmallVec<[ItemKind<'db>; 2]>);
