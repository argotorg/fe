use super::{
    adt_def::AdtCycleMember,
    trait_def::TraitInstId,
    ty_check::{RecordLike, TraitOps},
    ty_def::{Kind, TyId},
};
use crate::span::DynLazySpan;
use crate::visitor::prelude::*;
use crate::{analysis::HirAnalysisDb, hir_def::Trait};
use crate::{analysis::diagnostics::DiagnosticVoucher, hir_def::PathId};
use crate::{analysis::name_resolution::diagnostics::PathResDiag, hir_def::ItemKind};
use crate::{
    core::hir_def::{
        CallableDef, Enum, FieldIndex, FieldParent, Func, GenericParamOwner, IdentId, ImplTrait,
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

    RecursiveType(Vec<AdtCycleMember<'db>>),

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
    DuplicateArgLabel(Func<'db>, SmallVec<[u16; 4]>),
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

    InvalidConstTyExpr(DynLazySpan<'db>),

    NonTrailingDefaultGenericParam(LazyGenericParamSpan<'db>),

    // Default generic parameter diagnostics
    GenericDefaultForwardRef {
        span: LazyGenericParamSpan<'db>,
        name: IdentId<'db>,
    },
}

impl TyLowerDiag<'_> {
    pub(crate) fn local_code(&self) -> u16 {
        match self {
            Self::ExpectedStarKind(_) => 0,
            Self::InvalidTypeArgKind { .. } => 1,
            Self::RecursiveType { .. } => 2,
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
            Self::InvalidConstTyExpr(_) => 15,
            Self::TooManyGenericArgs { .. } => 16,
            Self::DuplicateFieldName(..) => 17,
            Self::DuplicateVariantName(..) => 18,
            Self::DuplicateGenericParamName(..) => 19,
            Self::DuplicateArgLabel(..) => 20,
            Self::NonTrailingDefaultGenericParam(_) => 21,
            Self::GenericDefaultForwardRef { .. } => 22,
        }
    }
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
    DuplicatedRestPat(DynLazySpan<'db>),

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

    UndefinedVariable(DynLazySpan<'db>, IdentId<'db>),

    InvalidEffectKey {
        owner: crate::analysis::ty::ty_check::EffectParamOwner<'db>,
        key: PathId<'db>,
        idx: usize,
    },

    MissingEffect {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
    },

    AmbiguousEffect {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
    },

    EffectMutabilityMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectTypeMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectProviderMismatch {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    EffectTraitUnsatisfied {
        primary: DynLazySpan<'db>,
        func: Func<'db>,
        key: PathId<'db>,
        trait_req: TraitInstId<'db>,
        given: TyId<'db>,
        provided_span: Option<DynLazySpan<'db>>,
    },

    ReturnedTypeMismatch {
        primary: DynLazySpan<'db>,
        actual: TyId<'db>,
        expected: TyId<'db>,
        func: Option<CallableDef<'db>>,
    },

    TypeMustBeKnown(DynLazySpan<'db>),

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

    NonAssignableExpr(DynLazySpan<'db>),

    ImmutableAssignment {
        primary: DynLazySpan<'db>,
        binding: Option<(IdentId<'db>, DynLazySpan<'db>)>,
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

    NonExhaustiveMatch {
        primary: DynLazySpan<'db>,
        scrutinee_ty: TyId<'db>,
        missing_patterns: Vec<String>, // Text representation of missing patterns
    },
    UnreachablePattern {
        primary: DynLazySpan<'db>,
    },

    /// The root path of a recv block doesn't refer to a msg type
    RecvExpectedMsgType {
        primary: DynLazySpan<'db>,
        given: TyId<'db>,
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
            Self::MissingEffect { .. } => 36,
            Self::EffectMutabilityMismatch { .. } => 37,
            Self::EffectTypeMismatch { .. } => 38,
            Self::EffectProviderMismatch { .. } => 52,
            Self::EffectTraitUnsatisfied { .. } => 39,
            Self::AmbiguousEffect { .. } => 40,
            Self::ReturnedTypeMismatch { .. } => 13,
            Self::TypeMustBeKnown(..) => 14,
            Self::AccessedFieldNotFound { .. } => 15,
            Self::OpsTraitNotImplemented { .. } => 16,
            Self::NonAssignableExpr(..) => 17,
            Self::ImmutableAssignment { .. } => 18,
            Self::LoopControlOutsideOfLoop { .. } => 19,
            Self::TraitNotImplemented { .. } => 20,
            Self::NotCallable(..) => 21,
            Self::CallGenericArgNumMismatch { .. } => 22,
            Self::CallArgNumMismatch { .. } => 23,
            Self::CallArgLabelMismatch { .. } => 24,
            Self::AmbiguousInherentMethodCall { .. } => 25,
            Self::AmbiguousTrait { .. } => 26,
            Self::AmbiguousTraitInst { .. } => 27,
            Self::InvisibleAmbiguousTrait { .. } => 28,
            Self::NotValue { .. } => 30,
            Self::TypeAnnotationNeeded { .. } => 31,
            Self::DuplicatedBinding { .. } => 32,
            Self::NotAMethod { .. } => 33,
            Self::NonExhaustiveMatch { .. } => 34,
            Self::UnreachablePattern { .. } => 35,
            Self::RecvExpectedMsgType { .. } => 41,
            Self::RecvArmNotMsgVariant { .. } => 42,
            Self::RecvArmRetTypeMissing { .. } => 43,
            Self::RecvArmDuplicateVariant { .. } => 44,
            Self::RecvMissingMsgVariants { .. } => 45,
            Self::RecvDuplicateMsgBlock { .. } => 46,
            Self::RecvDuplicateSelector { .. } => 47,
            Self::RecvArmNotVariantOfMsg { .. } => 48,
            Self::RecvArmNotMsgVariantTrait { .. } => 49,
            Self::RecvDuplicateHandler { .. } => 50,
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
    CyclicSuperTraits(Vec<Trait<'db>>),
}

impl TraitLowerDiag<'_> {
    pub fn local_code(&self) -> u16 {
        match self {
            Self::ExternalTraitForExternalType(_) => 0,
            Self::ConflictTraitImpl { .. } => 1,
            Self::CyclicSuperTraits { .. } => 2,
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
            Self::InvalidSelfType { .. } => 10,
            Self::InherentImplIsNotAllowed { .. } => 11,
            Self::MissingAssociatedType { .. } => 12,
            Self::MissingAssociatedConstValue { .. } => 13,
            Self::ConstNotDefinedInTrait { .. } => 14,
            Self::MissingAssociatedConst { .. } => 15,
        }
    }
}

pub struct DefConflictError<'db>(pub SmallVec<[ItemKind<'db>; 2]>);
