use crate::{
    hir_def::{IdentId, PathId, TopLevelMod, Trait},
    span::DynLazySpan,
};
use either::Either;
use salsa::Update;
use thin_vec::ThinVec;

use super::NameRes;
use crate::analysis::{
    HirAnalysisDb,
    ty::ty_def::Kind,
    ty::{trait_def::TraitInstId, ty_def::TyId},
};
use crate::hir_def::CallableDef;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum PathResDiag<'db> {
    /// The definition conflicts with other definitions.
    Conflict(IdentId<'db>, ThinVec<DynLazySpan<'db>>),

    /// The name is not found.
    NotFound(DynLazySpan<'db>, IdentId<'db>),

    MethodNotFound {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        receiver: Either<TyId<'db>, TraitInstId<'db>>,
    },

    /// The resolved name is not visible.
    Invisible(DynLazySpan<'db>, IdentId<'db>, Option<DynLazySpan<'db>>),

    /// The resolved name is ambiguous.
    Ambiguous(
        DynLazySpan<'db>,
        IdentId<'db>,
        Vec<(DynLazySpan<'db>, bool)>,
    ),

    /// The associated type is ambiguous.
    AmbiguousAssociatedType {
        span: DynLazySpan<'db>,
        name: IdentId<'db>,
        candidates: ThinVec<(TraitInstId<'db>, TyId<'db>)>,
    },

    AmbiguousAssociatedConst {
        primary: DynLazySpan<'db>,
        name: IdentId<'db>,
        trait_insts: ThinVec<TraitInstId<'db>>,
    },

    /// The name is found, but it can't be used as a middle segment of a path.
    InvalidPathSegment {
        span: DynLazySpan<'db>,
        segment: PathId<'db>,
        defined_at: Option<DynLazySpan<'db>>,
    },

    /// The name is found but belongs to a different name domain other than the
    /// Type.
    ExpectedType(DynLazySpan<'db>, IdentId<'db>, &'static str),

    /// The name is found but belongs to a different name domain other than the
    /// trait.
    ExpectedTrait(DynLazySpan<'db>, IdentId<'db>, &'static str),

    /// The name is found but belongs to a different name domain other than the
    /// value.
    ExpectedValue(DynLazySpan<'db>, IdentId<'db>, &'static str),

    ArgNumMismatch {
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        expected: usize,
        given: usize,
    },
    ArgKindMismatch {
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        expected: Kind,
        given: TyId<'db>,
    },
    ArgTypeMismatch {
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },

    // Method selection related diagnostics
    TypeMustBeKnown(DynLazySpan<'db>),
    AmbiguousInherentMethod {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        candidates: ThinVec<CallableDef<'db>>,
    },
    AmbiguousTrait {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        trait_insts: ThinVec<TraitInstId<'db>>,
    },
    InvisibleAmbiguousTrait {
        primary: DynLazySpan<'db>,
        traits: ThinVec<Trait<'db>>,
    },
}

impl<'db> PathResDiag<'db> {
    /// Returns the top-level module where the diagnostic is located.
    pub fn top_mod(&self, db: &'db dyn HirAnalysisDb) -> TopLevelMod<'db> {
        match self {
            Self::Conflict(_, conflicts) => conflicts
                .iter()
                .filter_map(|span| span.top_mod(db))
                .min()
                .unwrap(),
            Self::NotFound(span, _) => span.top_mod(db).unwrap(),
            Self::MethodNotFound { primary, .. } => primary.top_mod(db).unwrap(),
            Self::Invisible(span, _, _) => span.top_mod(db).unwrap(),
            Self::Ambiguous(span, _, _) => span.top_mod(db).unwrap(),
            Self::AmbiguousAssociatedType { span, .. } => span.top_mod(db).unwrap(),
            Self::InvalidPathSegment { span, .. } => span.top_mod(db).unwrap(),
            Self::ExpectedType(span, _, _) => span.top_mod(db).unwrap(),
            Self::ExpectedTrait(span, _, _) => span.top_mod(db).unwrap(),
            Self::ExpectedValue(span, _, _) => span.top_mod(db).unwrap(),
            Self::ArgNumMismatch { span, .. } => span.top_mod(db).unwrap(),
            Self::ArgKindMismatch { span, .. } => span.top_mod(db).unwrap(),
            Self::ArgTypeMismatch { span, .. } => span.top_mod(db).unwrap(),
            Self::TypeMustBeKnown(span) => span.top_mod(db).unwrap(),
            Self::AmbiguousInherentMethod { primary, .. } => primary.top_mod(db).unwrap(),
            Self::AmbiguousTrait { primary, .. } => primary.top_mod(db).unwrap(),
            Self::AmbiguousAssociatedConst { primary, .. } => primary.top_mod(db).unwrap(),
            Self::InvisibleAmbiguousTrait { primary, .. } => primary.top_mod(db).unwrap(),
        }
    }

    pub(super) fn ambiguous(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        cands: ThinVec<NameRes<'db>>,
    ) -> Self {
        let cands = cands
            .into_iter()
            .filter_map(|name| {
                let span = name.kind.name_span(db)?;
                let from_implicit = name
                    .derivation
                    .use_stmt()
                    .map(|use_| use_.is_synthetic_use(db))
                    .unwrap_or(false);
                Some((span, from_implicit))
            })
            .collect();
        Self::Ambiguous(span, ident, cands)
    }

    pub fn local_code(&self) -> u16 {
        match self {
            Self::Conflict(..) => 1,
            Self::NotFound(..) => 2,
            Self::Invisible(..) => 3,
            Self::Ambiguous(..) => 4,
            Self::InvalidPathSegment { .. } => 5,
            Self::ExpectedType(..) => 6,
            Self::ExpectedTrait(..) => 7,
            Self::ExpectedValue(..) => 8,
            Self::AmbiguousAssociatedType { .. } => 9,
            Self::AmbiguousAssociatedConst { .. } => 18,
            Self::MethodNotFound { .. } => 10,
            Self::ArgNumMismatch { .. } => 11,
            Self::ArgKindMismatch { .. } => 12,
            Self::ArgTypeMismatch { .. } => 13,
            Self::TypeMustBeKnown(..) => 14,
            Self::AmbiguousInherentMethod { .. } => 15,
            Self::AmbiguousTrait { .. } => 16,
            Self::InvisibleAmbiguousTrait { .. } => 17,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ImportDiag<'db> {
    Conflict(IdentId<'db>, ThinVec<DynLazySpan<'db>>),
    NotFound(DynLazySpan<'db>, IdentId<'db>),
    Invisible(DynLazySpan<'db>, IdentId<'db>, Option<DynLazySpan<'db>>),
    Ambiguous(DynLazySpan<'db>, IdentId<'db>, Vec<DynLazySpan<'db>>),
    InvalidPathSegment(DynLazySpan<'db>, IdentId<'db>, Option<DynLazySpan<'db>>),
}

impl<'db> ImportDiag<'db> {
    pub fn top_mod(&self, db: &'db dyn HirAnalysisDb) -> TopLevelMod<'db> {
        match self {
            Self::Conflict(_, conflicts) => conflicts
                .iter()
                .filter_map(|span| span.top_mod(db))
                .min()
                .unwrap(),
            Self::NotFound(span, _) => span.top_mod(db).unwrap(),
            Self::Invisible(span, _, _) => span.top_mod(db).unwrap(),
            Self::Ambiguous(span, _, _) => span.top_mod(db).unwrap(),
            Self::InvalidPathSegment(span, _, _) => span.top_mod(db).unwrap(),
        }
    }

    pub(super) fn ambiguous(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        cands: ThinVec<NameRes<'db>>,
    ) -> Self {
        let cands = cands
            .into_iter()
            .filter_map(|name| name.kind.name_span(db))
            .collect();
        Self::Ambiguous(span, ident, cands)
    }
}
