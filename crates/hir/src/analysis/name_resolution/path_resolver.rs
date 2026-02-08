use crate::core::hir_def::GenericArg;
use crate::hir_def::{CallableDef, Func};
use crate::{
    core::hir_def::{
        Const, Enum, EnumVariant, GenericParamOwner, IdentId, ImplTrait, ItemKind, PathId,
        PathKind, Trait, TypeBound, TypeKind, VariantKind, scope_graph::ScopeId,
    },
    span::{DynLazySpan, path::LazyPathSpan},
};
use common::indexmap::{IndexMap, IndexSet};
use either::Either;
use smallvec::{SmallVec, smallvec};
use thin_vec::ThinVec;

use super::{
    EarlyNameQueryId, ExpectedPathKind, NameDomain,
    diagnostics::PathResDiag,
    is_scope_visible_from,
    method_selection::{MethodCandidate, MethodSelectionError, select_method_candidate},
    name_resolver::{NameRes, NameResBucket, NameResKind, NameResolutionError},
    resolve_query,
    visibility_checker::is_ty_visible_from,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::QueryDirective,
    ty::{
        adt_def::AdtRef,
        binder::Binder,
        canonical::{Canonical, Canonicalized},
        fold::TyFoldable,
        normalize::normalize_ty,
        trait_def::{TraitInstId, impls_for_ty_with_constraints},
        trait_lower::{TraitArgError, TraitRefLowerError, lower_trait_ref, lower_trait_ref_impl},
        trait_resolution::PredicateListId,
        ty_def::{InvalidCause, Kind, TyData, TyId},
        ty_lower::{
            TyAlias, collect_generic_params, lower_generic_arg_list, lower_hir_ty, lower_type_alias,
        },
        unify::UnificationTable,
    },
};

pub type PathResolutionResult<'db, T> = Result<T, PathResError<'db>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct PathResError<'db> {
    pub kind: PathResErrorKind<'db>,
    pub failed_at: PathId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum PathResErrorKind<'db> {
    /// The name is not found.
    NotFound {
        parent: Option<PathRes<'db>>,
        bucket: NameResBucket<'db>,
    },

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    ParseError,

    /// The name is found, but it's ambiguous.
    Ambiguous(ThinVec<NameRes<'db>>),

    /// The associated type is ambiguous.
    AmbiguousAssociatedType {
        name: IdentId<'db>,
        candidates: ThinVec<(TraitInstId<'db>, TyId<'db>)>,
    },

    AmbiguousAssociatedConst {
        name: IdentId<'db>,
        trait_insts: ThinVec<TraitInstId<'db>>,
    },

    /// The name is found, but it can't be used in the middle of a use path.
    InvalidPathSegment(PathRes<'db>),

    /// Type component of a qualified path failed to resolve.
    QualifiedTypeType(Box<PathResolutionResult<'db, PathRes<'db>>>),

    /// Trait component of a qualified path failed to resolve.
    QualifiedTypeTrait(Box<PathResolutionResult<'db, PathRes<'db>>>),

    /// The definition conflicts with other definitions.
    Conflict(ThinVec<DynLazySpan<'db>>),

    ArgNumMismatch {
        expected: usize,
        given: usize,
    },
    ArgKindMisMatch {
        expected: Kind,
        given: TyId<'db>,
    },
    ArgTypeMismatch {
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },

    /// Trait path generic argument expected a type; wrong domain was found.
    /// Carries the argument index and offending ident/kind for precise diagnostics.
    TraitGenericArgType {
        arg_idx: usize,
        ident: IdentId<'db>,
        given_kind: &'static str,
    },

    MethodSelection(MethodSelectionError<'db>),
}

impl<'db> PathResError<'db> {
    pub fn new(kind: PathResErrorKind<'db>, failed_at: PathId<'db>) -> Self {
        Self { kind, failed_at }
    }
    pub fn parse_err(path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::ParseError, path)
    }

    pub fn method_selection(err: MethodSelectionError<'db>, path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::MethodSelection(err), path)
    }

    pub fn from_name_res_error(err: NameResolutionError<'db>, path: PathId<'db>) -> Self {
        let kind = match err {
            NameResolutionError::NotFound => PathResErrorKind::NotFound {
                parent: None,
                bucket: NameResBucket::default(),
            },
            NameResolutionError::Invalid => PathResErrorKind::ParseError,
            NameResolutionError::Ambiguous(vec) => PathResErrorKind::Ambiguous(vec),
            NameResolutionError::Conflict(_ident, vec) => PathResErrorKind::Conflict(vec),
            NameResolutionError::Invisible(_) => unreachable!(),
            NameResolutionError::InvalidPathSegment(_) => unreachable!(),
        };
        Self::new(kind, path)
    }

    pub fn print(&self) -> String {
        match &self.kind {
            PathResErrorKind::NotFound { .. } => "Not found".to_string(),
            PathResErrorKind::ParseError => "Parse error".to_string(),
            PathResErrorKind::Ambiguous(v) => format!("Ambiguous; {} options.", v.len()),
            PathResErrorKind::AmbiguousAssociatedType {
                name: _,
                candidates,
            } => {
                format!("Ambiguous associated type; {} options.", candidates.len())
            }
            PathResErrorKind::AmbiguousAssociatedConst {
                name: _,
                trait_insts,
            } => {
                format!(
                    "Ambiguous associated const; {} candidates.",
                    trait_insts.len()
                )
            }
            PathResErrorKind::InvalidPathSegment(_) => "Invalid path segment".to_string(),
            PathResErrorKind::QualifiedTypeType(res) => match res.as_ref() {
                Ok(res) => format!(
                    "Expected type in qualified path, but found {}",
                    res.kind_name()
                ),
                Err(err) => err.print(),
            },
            PathResErrorKind::QualifiedTypeTrait(res) => match res.as_ref() {
                Ok(res) => format!(
                    "Expected trait qualifier in qualified path, but found {}",
                    res.kind_name()
                ),
                Err(err) => err.print(),
            },
            PathResErrorKind::Conflict(..) => "Conflicting definitions".to_string(),
            PathResErrorKind::ArgNumMismatch { expected, given } => {
                format!("Incorrect number of generic args; expected {expected}, given {given}.")
            }
            PathResErrorKind::ArgKindMisMatch { .. } => {
                "Generic argument kind mismatch".to_string()
            }
            PathResErrorKind::ArgTypeMismatch { .. } => {
                "Generic const argument type mismatch".to_string()
            }
            PathResErrorKind::TraitGenericArgType { .. } => {
                "Trait generic argument expects a type".to_string()
            }
            PathResErrorKind::MethodSelection(err) => match err {
                MethodSelectionError::AmbiguousInherentMethod(cands) => {
                    format!("Ambiguous method; {} inherent candidates.", cands.len())
                }
                MethodSelectionError::AmbiguousTraitMethod(traits) => {
                    format!("Ambiguous method; {} trait candidates.", traits.len())
                }
                MethodSelectionError::NotFound => "Method not found".to_string(),
                MethodSelectionError::InvisibleInherentMethod(_) => {
                    "Inherent method is not visible".to_string()
                }
                MethodSelectionError::InvisibleTraitMethod(traits) => {
                    format!("Trait is not in scope; {} candidate(s).", traits.len())
                }
                MethodSelectionError::ReceiverTypeMustBeKnown => {
                    "Receiver type must be known".to_string()
                }
            },
        }
    }

    pub fn into_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        path_span: LazyPathSpan<'db>,
        expected: ExpectedPathKind,
    ) -> Option<PathResDiag<'db>> {
        let kind = self.kind;
        let failed_idx = self.failed_at.segment_index(db);
        let seg_span = path_span.clone().segment(failed_idx);
        let seg_path = path.segment(db, failed_idx).unwrap_or(self.failed_at);

        let (span, ident) = if matches!(seg_path.kind(db), PathKind::QualifiedType { .. }) {
            (seg_span.clone().into_atom().into(), IdentId::new(db, "")) // ident is unused in this case
        } else {
            (
                seg_span.clone().ident().into(),
                seg_path.ident(db).to_opt()?,
            )
        };

        let diag = match kind {
            PathResErrorKind::ParseError => return None,
            PathResErrorKind::NotFound { parent, bucket } => {
                if let Some(nr) = bucket.iter_ok().next() {
                    if path != self.failed_at {
                        PathResDiag::InvalidPathSegment {
                            span,
                            segment: self.failed_at,
                            defined_at: nr.kind.name_span(db),
                        }
                    } else {
                        match expected {
                            ExpectedPathKind::Record | ExpectedPathKind::Type => {
                                PathResDiag::ExpectedType(span, ident, nr.kind_name())
                            }
                            ExpectedPathKind::Trait => {
                                PathResDiag::ExpectedTrait(span, ident, nr.kind_name())
                            }
                            ExpectedPathKind::Value => {
                                PathResDiag::ExpectedValue(span, ident, nr.kind_name())
                            }
                            ExpectedPathKind::Function => func_not_found_err(span, ident, parent),
                            _ => PathResDiag::NotFound(span, ident),
                        }
                    }
                } else if expected == ExpectedPathKind::Function {
                    func_not_found_err(span, ident, parent)
                } else {
                    PathResDiag::NotFound(span, ident)
                }
            }

            PathResErrorKind::Ambiguous(cands) => PathResDiag::ambiguous(db, span, ident, cands),

            PathResErrorKind::ArgNumMismatch { expected, given } => PathResDiag::ArgNumMismatch {
                span,
                ident,
                expected,
                given,
            },

            PathResErrorKind::ArgKindMisMatch { expected, given } => PathResDiag::ArgKindMismatch {
                span,
                ident,
                expected,
                given,
            },

            PathResErrorKind::ArgTypeMismatch { expected, given } => PathResDiag::ArgTypeMismatch {
                span,
                ident,
                expected,
                given,
            },

            PathResErrorKind::InvalidPathSegment(res) => PathResDiag::InvalidPathSegment {
                span,
                segment: seg_path,
                defined_at: res.name_span(db),
            },

            PathResErrorKind::Conflict(spans) => PathResDiag::Conflict(ident, spans),

            PathResErrorKind::AmbiguousAssociatedType { name, candidates } => {
                PathResDiag::AmbiguousAssociatedType {
                    span,
                    name,
                    candidates,
                }
            }

            PathResErrorKind::AmbiguousAssociatedConst { name, trait_insts } => {
                PathResDiag::AmbiguousAssociatedConst {
                    primary: span,
                    name,
                    trait_insts,
                }
            }

            PathResErrorKind::QualifiedTypeType(result) => match *result {
                Ok(res) => {
                    if let PathKind::QualifiedType { type_, .. } = seg_path.kind(db)
                        && let TypeKind::Path(type_path) = type_.data(db)
                    {
                        let type_ident = type_path.unwrap().ident(db).unwrap();
                        let ty_span = seg_span.qualified_type().ty().into_path_type().path();
                        PathResDiag::ExpectedType(ty_span.into(), type_ident, res.kind_name())
                    } else {
                        unreachable!()
                    }
                }
                Err(inner) => {
                    let failed = inner.failed_at;
                    let ty_span = seg_span.qualified_type().ty().into_path_type().path();
                    inner.into_diag(db, failed, ty_span, ExpectedPathKind::Type)?
                }
            },
            PathResErrorKind::QualifiedTypeTrait(result) => match *result {
                Ok(res) => {
                    if let PathKind::QualifiedType { trait_, .. } = seg_path.kind(db) {
                        let trait_ident = trait_.path(db).unwrap().ident(db).unwrap();
                        let trait_span = seg_span.qualified_type().trait_qualifier().name().into();
                        PathResDiag::ExpectedTrait(trait_span, trait_ident, res.kind_name())
                    } else {
                        unreachable!()
                    }
                }
                Err(inner) => {
                    let failed = inner.failed_at;
                    let trait_span = seg_span.qualified_type().trait_qualifier().path();
                    inner.into_diag(db, failed, trait_span, ExpectedPathKind::Trait)?
                }
            },

            PathResErrorKind::MethodSelection(err) => match err {
                MethodSelectionError::ReceiverTypeMustBeKnown => PathResDiag::TypeMustBeKnown(span),
                MethodSelectionError::AmbiguousInherentMethod(candidates) => {
                    PathResDiag::AmbiguousInherentMethod {
                        primary: span,
                        method_name: ident,
                        candidates,
                    }
                }
                MethodSelectionError::AmbiguousTraitMethod(trait_insts) => {
                    PathResDiag::AmbiguousTrait {
                        primary: span,
                        method_name: ident,
                        trait_insts,
                    }
                }
                MethodSelectionError::InvisibleInherentMethod(func) => {
                    PathResDiag::Invisible(span, ident, func.name_span().into())
                }
                MethodSelectionError::InvisibleTraitMethod(traits) => {
                    PathResDiag::InvisibleAmbiguousTrait {
                        primary: span,
                        traits,
                    }
                }
                MethodSelectionError::NotFound => PathResDiag::NotFound(span, ident),
            },

            // Force a type-expected diagnostic at the specific generic arg span.
            PathResErrorKind::TraitGenericArgType {
                arg_idx,
                ident,
                given_kind,
            } => {
                let ty_span = path_span
                    .clone()
                    .segment(failed_idx)
                    .generic_args()
                    .arg(arg_idx)
                    .into_type_arg()
                    .ty();
                PathResDiag::ExpectedType(ty_span.into(), ident, given_kind)
            }
        };
        Some(diag)
    }
}

fn func_not_found_err<'db>(
    span: DynLazySpan<'db>,
    ident: IdentId<'db>,
    parent: Option<PathRes<'db>>,
) -> PathResDiag<'db> {
    match parent {
        Some(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => PathResDiag::MethodNotFound {
            primary: span,
            method_name: ident,
            receiver: Either::Left(ty),
        },
        Some(PathRes::Trait(t)) => PathResDiag::MethodNotFound {
            primary: span,
            method_name: ident,
            receiver: Either::Right(t),
        },
        _ => PathResDiag::NotFound(span, ident),
    }
}

/// Panics if `path` has more than one segment.
pub fn resolve_ident_to_bucket<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> &'db NameResBucket<'db> {
    assert!(path.parent(db).is_none());
    let directive = QueryDirective::for_scope(db, scope);
    let query = make_query(db, path, scope, directive);
    resolve_query(db, query)
}

/// Panics if path.ident is `Absent`
fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    base_directive: QueryDirective,
) -> EarlyNameQueryId<'db> {
    let mut directive = base_directive;

    if path.segment_index(db) != 0 {
        directive = directive.disallow_external();
        directive = directive.disallow_lex();
    }

    let name = path.ident(db).unwrap();
    EarlyNameQueryId::new(db, name, scope, directive)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum PathRes<'db> {
    Ty(TyId<'db>),
    TyAlias(TyAlias<'db>, TyId<'db>),
    Func(TyId<'db>),
    FuncParam(ItemKind<'db>, u16),
    Trait(TraitInstId<'db>),
    /// A trait-associated function resolved via a trait path, e.g. `T::make`.
    ///
    /// Carries the trait reference as written (including generic args and assoc-type bindings),
    /// with `Self` still bound to the trait's `Self` type parameter. The type checker is
    /// responsible for instantiating `Self` to an inference variable and later confirming that
    /// an impl exists.
    TraitMethod(TraitInstId<'db>, Func<'db>),
    EnumVariant(ResolvedVariant<'db>),
    Const(Const<'db>, TyId<'db>),
    Mod(ScopeId<'db>),
    Method(TyId<'db>, MethodCandidate<'db>),
    TraitConst(TyId<'db>, TraitInstId<'db>, IdentId<'db>),
}

impl<'db> PathRes<'db> {
    pub fn map_over_ty<F>(self, mut f: F) -> Self
    where
        F: FnMut(TyId<'db>) -> TyId<'db>,
    {
        match self {
            PathRes::Ty(ty) => PathRes::Ty(f(ty)),
            PathRes::TyAlias(alias, ty) => PathRes::TyAlias(alias, f(ty)),
            PathRes::Func(ty) => PathRes::Func(f(ty)),
            PathRes::Const(const_, ty) => PathRes::Const(const_, f(ty)),
            PathRes::EnumVariant(v) => PathRes::EnumVariant(ResolvedVariant { ty: f(v.ty), ..v }),
            // TODO: map over candidate ty?
            PathRes::Method(ty, candidate) => PathRes::Method(f(ty), candidate),
            PathRes::TraitConst(ty, inst, name) => PathRes::TraitConst(f(ty), inst, name),
            r @ (PathRes::Trait(_)
            | PathRes::TraitMethod(..)
            | PathRes::Mod(_)
            | PathRes::FuncParam(..)) => r,
        }
    }

    pub fn as_scope(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) => ty.as_scope(db),
            PathRes::Const(const_, _) => Some(const_.scope()),
            PathRes::TraitConst(_ty, inst, name) => {
                let trait_ = inst.def(db);
                let idx = trait_.const_index(db, *name).unwrap() as u16;
                Some(ScopeId::TraitConst(trait_, idx))
            }
            PathRes::TyAlias(alias, _) => Some(alias.alias.scope()),
            PathRes::Trait(trait_) => Some(trait_.def(db).scope()),
            PathRes::TraitMethod(_inst, method) => Some(method.scope()),
            PathRes::EnumVariant(variant) => Some(ScopeId::Variant(variant.variant)),
            PathRes::FuncParam(item, idx) => Some(ScopeId::FuncParam(*item, *idx)),
            PathRes::Mod(scope) => Some(*scope),
            PathRes::Method(ty, _) => ty.as_scope(db),
        }
    }

    pub fn is_visible_from(&self, db: &'db dyn HirAnalysisDb, from_scope: ScopeId<'db>) -> bool {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) => is_ty_visible_from(db, *ty, from_scope),
            PathRes::Const(const_, _) => is_scope_visible_from(db, const_.scope(), from_scope),
            PathRes::TraitConst(_, inst, _) => {
                // Associated consts behave like trait methods: the trait does not
                // need to be imported as long as it's otherwise visible.
                is_scope_visible_from(db, inst.def(db).scope(), from_scope)
            }
            PathRes::TraitMethod(_inst, method) => {
                // Trait method visibility depends on the method's defining scope,
                // not on trait imports (the trait is explicitly referenced).
                is_scope_visible_from(db, method.scope(), from_scope)
            }
            PathRes::Method(_, cand) => {
                // Method visibility depends on the method's defining scope
                // (function or trait method), not the receiver type.
                let method_scope = match cand {
                    MethodCandidate::InherentMethod(func_def) => func_def.scope(),
                    MethodCandidate::TraitMethod(c) | MethodCandidate::NeedsConfirmation(c) => {
                        c.method.scope()
                    }
                };
                is_scope_visible_from(db, method_scope, from_scope)
            }
            r => is_scope_visible_from(db, r.as_scope(db).unwrap(), from_scope),
        }
    }

    pub fn name_span(&self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        self.as_scope(db)?.name_span(db)
    }

    pub fn pretty_path(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let ty_path = |ty: TyId<'db>| {
            if let Some(scope) = ty.as_scope(db) {
                scope.pretty_path(db)
            } else {
                Some(ty.pretty_print(db).to_string())
            }
        };

        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) => ty_path(*ty),
            PathRes::TyAlias(alias, _) => alias.alias.scope().pretty_path(db),
            PathRes::EnumVariant(v) => Some(format!(
                "{}::{}",
                ty_path(v.ty).unwrap_or_else(|| "<missing>".into()),
                v.variant.name(db)?
            )),
            PathRes::Const(const_, _) => const_.scope().pretty_path(db),
            PathRes::TraitConst(ty, _inst, name) => Some(format!(
                "{}::{}",
                ty_path(*ty).unwrap_or_else(|| "<missing>".into()),
                name.data(db)
            )),
            PathRes::TraitMethod(..) => self.as_scope(db)?.pretty_path(db),
            r @ (PathRes::Trait(..) | PathRes::Mod(..) | PathRes::FuncParam(..)) => {
                r.as_scope(db).unwrap().pretty_path(db)
            }

            PathRes::Method(ty, cand) => Some(format!(
                "{}::{}",
                ty_path(*ty).unwrap_or_else(|| "<missing>".into()),
                cand.name(db).data(db)
            )),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            PathRes::Ty(_) => "type",
            PathRes::TyAlias(..) => "type alias",
            PathRes::Func(_) => "function",
            PathRes::FuncParam(..) => "function parameter",
            PathRes::Trait(_) => "trait",
            PathRes::TraitMethod(..) => "trait method",
            PathRes::EnumVariant(_) => "enum variant",
            PathRes::Const(..) => "constant",
            PathRes::TraitConst(..) => "constant",
            PathRes::Mod(_) => "module",
            PathRes::Method(..) => "method",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct ResolvedVariant<'db> {
    pub ty: TyId<'db>,
    pub variant: EnumVariant<'db>,
    pub path: PathId<'db>,
}

impl<'db> ResolvedVariant<'db> {
    pub fn enum_(&self, db: &'db dyn HirAnalysisDb) -> Enum<'db> {
        self.ty.as_enum(db).unwrap()
    }

    pub fn kind(&self, db: &'db dyn HirAnalysisDb) -> VariantKind<'db> {
        self.variant.kind(db)
    }

    pub fn iter_field_types(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> {
        self.ty
            .adt_def(db)
            .unwrap()
            .fields(db)
            .get(self.variant.idx as usize)
            .unwrap()
            .iter_types(db)
    }

    pub fn constructor_func_ty(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let mut ty = TyId::func(db, self.to_callable(db)?);

        for &arg in self.ty.generic_args(db) {
            if ty.applicable_ty(db).is_some() {
                ty = TyId::app(db, ty, arg);
            }
        }
        Some(ty)
    }

    pub fn to_callable(&self, db: &'db dyn HirAnalysisDb) -> Option<CallableDef<'db>> {
        if !matches!(self.variant.kind(db), VariantKind::Tuple(_)) {
            return None;
        }

        Some(CallableDef::VariantCtor(self.variant))
    }
}

pub fn resolve_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
    let directive = QueryDirective::for_scope(db, scope);
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        directive,
        true,
        &mut |_, _| {},
    )
}

pub fn resolve_path_with_observer<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let directive = QueryDirective::for_scope(db, scope);
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        directive,
        true,
        observer,
    )
}

#[allow(clippy::too_many_arguments)]
fn resolve_path_impl<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    base_directive: QueryDirective,
    is_tail: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let parent_res = path
        .parent(db)
        .map(|path| {
            resolve_path_impl(
                db,
                path,
                scope,
                assumptions,
                resolve_tail_as_value,
                base_directive,
                false,
                observer,
            )
        })
        .transpose()?;

    if let PathKind::QualifiedType { type_, trait_ } = path.kind(db) {
        if path.parent(db).is_some() {
            return Err(PathResError::new(
                PathResErrorKind::InvalidPathSegment(PathRes::Ty(TyId::invalid(
                    db,
                    InvalidCause::Other,
                ))),
                path,
            ));
        }
        let ty = lower_hir_ty(db, type_, scope, assumptions);
        if let Some(cause) = ty.invalid_cause(db) {
            match cause {
                InvalidCause::NotAType(res) => {
                    return Err(PathResError::new(
                        PathResErrorKind::QualifiedTypeType(Box::new(Ok(res))),
                        path,
                    ));
                }
                InvalidCause::PathResolutionFailed { path: ty_path } => {
                    if let Err(inner) = resolve_path(db, ty_path, scope, assumptions, false) {
                        return Err(PathResError {
                            kind: PathResErrorKind::QualifiedTypeType(Box::new(Err(inner))),
                            failed_at: path,
                        });
                    }
                }
                _ => {}
            }
        }
        let trait_inst = match lower_trait_ref(db, ty, trait_, scope, assumptions, None) {
            Ok(inst) => inst,
            Err(err) => {
                let trait_path = trait_.path(db).to_opt().unwrap_or(path);
                let err = match err {
                    TraitRefLowerError::PathResError(e) => PathResError {
                        kind: PathResErrorKind::QualifiedTypeTrait(Box::new(Err(e))),
                        failed_at: path,
                    },
                    TraitRefLowerError::InvalidDomain(res) => PathResError::new(
                        PathResErrorKind::QualifiedTypeTrait(Box::new(Ok(res))),
                        trait_path,
                    ),
                    TraitRefLowerError::Ignored => PathResError::parse_err(trait_path),
                };
                return Err(err);
            }
        };

        let qualified_ty = TyId::qualified_ty(db, trait_inst);
        let r = PathRes::Ty(qualified_ty);
        observer(path, &r);
        return Ok(r);
    }

    let Some(ident) = path.ident(db).to_opt() else {
        return Err(PathResError::parse_err(path));
    };

    let parent_scope = parent_res
        .as_ref()
        .and_then(|r| r.as_scope(db))
        .unwrap_or(scope);

    match parent_res {
        Some(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
            // Fast paths for qualified types `<A as Trait>::...`.
            //
            // NOTE: This must run before generic associated-const probing, otherwise
            // `<A as Trait>::CONST` can be mis-resolved with `recv_ty` set to the
            // *qualified type* instead of `A`, which then breaks downstream trait-const
            // evaluation/CTFE.
            if let TyData::QualifiedTy(trait_inst) = ty.data(db) {
                // Associated type projection
                if let Some(assoc_ty) = trait_inst.assoc_ty(db, ident) {
                    let r = PathRes::Ty(assoc_ty);
                    observer(path, &r);
                    return Ok(r);
                }

                // Associated function on a specific trait instance
                if is_tail
                    && resolve_tail_as_value
                    && let Some(&method) = trait_inst.def(db).method_defs(db).get(&ident)
                {
                    let r = PathRes::TraitMethod(*trait_inst, method);
                    observer(path, &r);
                    return Ok(r);
                }

                // Associated const on a specific trait instance
                if resolve_tail_as_value && trait_inst.def(db).const_(db, ident).is_some() {
                    let r = PathRes::TraitConst(trait_inst.self_ty(db), *trait_inst, ident);
                    observer(path, &r);
                    return Ok(r);
                }
            }

            // Try to resolve as an associated const on the receiver type
            if is_tail && resolve_tail_as_value {
                // Probe impls across both the call-site scope and the receiver type's ingot so
                // `OtherIngotType::CONST` and `ExternalType::LOCAL_TRAIT_CONST` both resolve.
                match select_assoc_const_candidate(db, ty, ident, scope, assumptions) {
                    AssocConstSelection::Found(inst) => {
                        let r = PathRes::TraitConst(ty, inst, ident);
                        observer(path, &r);
                        return Ok(r);
                    }
                    AssocConstSelection::Ambiguous(traits) => {
                        return Err(PathResError::new(
                            PathResErrorKind::AmbiguousAssociatedConst {
                                name: ident,
                                trait_insts: traits,
                            },
                            path,
                        ));
                    }
                    AssocConstSelection::NotFound => {}
                }
            }

            // Try to resolve as an enum variant
            if let Some(enum_) = ty.as_enum(db) {
                // We need to use the concrete enum scope instead of
                // parent_scope to resolve the variants in all cases,
                // eg when parent is `Self`
                let directive = QueryDirective::for_scope(db, enum_.scope());
                let query = make_query(db, path, enum_.scope(), directive);
                let bucket = resolve_query(db, query);

                if let Ok(res) = bucket.pick(NameDomain::VALUE)
                    && let Some(var) = res.enum_variant()
                {
                    let reso = PathRes::EnumVariant(ResolvedVariant {
                        ty,
                        variant: var,
                        path,
                    });
                    observer(path, &reso);
                    return Ok(reso);
                }
            }

            if is_tail && resolve_tail_as_value {
                let receiver_ty = Canonicalized::new(db, ty);
                match select_method_candidate(
                    db,
                    receiver_ty.value,
                    ident,
                    parent_scope,
                    assumptions,
                    None,
                ) {
                    Ok(cand) => {
                        let r = PathRes::Method(ty, cand);
                        observer(path, &r);
                        return Ok(r);
                    }
                    Err(MethodSelectionError::NotFound) => {}
                    Err(err) => {
                        return Err(PathResError::method_selection(err, path));
                    }
                }
            }

            // Find raw associated types, then dedup by normalized result here.
            let assoc_tys =
                find_associated_type(db, scope, Canonical::new(db, ty), ident, assumptions);

            if assoc_tys.is_empty() {
                return Err(PathResError::new(
                    PathResErrorKind::NotFound {
                        parent: parent_res,
                        bucket: NameResBucket::default(),
                    },
                    path,
                ));
            }

            // Deduplicate by normalized type, but preserve and return the original
            // (unnormalized) candidate to avoid prematurely collapsing projections
            // like `T::IntoIter::Item` into `T::Item`.
            let seg_args = lower_generic_arg_list(db, path.generic_args(db), scope, assumptions);
            let mut dedup: IndexMap<TyId<'db>, (TraitInstId<'db>, TyId<'db>)> = IndexMap::new();
            for (inst, ty_candidate) in assoc_tys.iter().copied() {
                let applied = if seg_args.is_empty() {
                    ty_candidate
                } else {
                    TyId::foldl(db, ty_candidate, &seg_args)
                };
                if let TyData::Invalid(InvalidCause::TooManyGenericArgs { expected, given }) =
                    applied.data(db)
                {
                    return Err(PathResError::new(
                        PathResErrorKind::ArgNumMismatch {
                            expected: *expected,
                            given: *given,
                        },
                        path,
                    ));
                }

                let norm = normalize_ty(db, applied, scope, assumptions);
                dedup.entry(norm).or_insert((inst, applied));
            }

            match dedup.len() {
                0 => unreachable!(),
                1 => {
                    let (_, (_, original_ty)) = dedup.first().unwrap();
                    let r = PathRes::Ty(*original_ty);
                    observer(path, &r);
                    return Ok(r);
                }
                _ => {
                    // Build candidate list from deduped set for diagnostics
                    let candidates = dedup
                        .into_iter()
                        .map(|(_norm, (inst, original_ty))| (inst, original_ty))
                        .collect();
                    return Err(PathResError::new(
                        PathResErrorKind::AmbiguousAssociatedType {
                            name: ident,
                            candidates,
                        },
                        path,
                    ));
                }
            }
        }

        Some(
            PathRes::Func(_)
            | PathRes::EnumVariant(..)
            | PathRes::TraitConst(..)
            | PathRes::TraitMethod(..),
        ) => {
            return Err(PathResError::new(
                PathResErrorKind::InvalidPathSegment(parent_res.unwrap()),
                path,
            ));
        }
        Some(PathRes::FuncParam(..) | PathRes::Method(..)) => unreachable!(),
        Some(PathRes::Trait(trait_inst)) => {
            if is_tail
                && resolve_tail_as_value
                && let Some(&method) = trait_inst.def(db).method_defs(db).get(&ident)
            {
                let r = PathRes::TraitMethod(trait_inst, method);
                observer(path, &r);
                return Ok(r);
            }
        }
        Some(PathRes::Const(..) | PathRes::Mod(_)) | None => {}
    };

    let query = make_query(db, path, parent_scope, base_directive);
    let bucket = resolve_query(db, query);

    let parent_ty = parent_res.as_ref().and_then(|res| match res {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(*ty),
        _ => None,
    });

    let res = if is_tail
        && resolve_tail_as_value
        && let Ok(res) = bucket.pick(NameDomain::VALUE)
    {
        res.clone()
    } else {
        pick_type_domain_from_bucket(parent_res, bucket, path, path.parent(db))?
    };

    let r = resolve_name_res(db, &res, parent_ty, path, scope, assumptions)?;
    observer(path, &r);
    Ok(r)
}

enum AssocConstSelection<'db> {
    Found(TraitInstId<'db>),
    Ambiguous(ThinVec<TraitInstId<'db>>),
    NotFound,
}

fn select_assoc_const_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> AssocConstSelection<'db> {
    // Qualified type: `<A as T>::C` must resolve against the explicit trait instance.
    if let TyData::QualifiedTy(trait_inst) = receiver_ty.data(db) {
        return if trait_inst.def(db).const_(db, name).is_some() {
            AssocConstSelection::Found(*trait_inst)
        } else {
            AssocConstSelection::NotFound
        };
    }

    // When the receiver is a type parameter (or otherwise projection-like),
    // we don't know its concrete type yet, so probing impls would pull in many
    // unrelated candidates and frequently lead to spurious ambiguity.
    //
    // In that case, rely on in-scope bounds (`assumptions`) to provide candidates.
    let receiver_is_ty_param = matches!(
        receiver_ty.base_ty(db).data(db),
        TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
    );
    if receiver_is_ty_param {
        let mut matches: IndexSet<TraitInstId<'db>> = IndexSet::default();

        let mut table = UnificationTable::new(db);
        let receiver = Canonical::new(db, receiver_ty);
        let extracted_receiver_ty = receiver.extract_identity(&mut table);

        for &pred in assumptions.list(db) {
            let snapshot = table.snapshot();
            let self_ty = table.instantiate_to_term(pred.self_ty(db));

            if table.unify(extracted_receiver_ty, self_ty).is_ok() {
                if pred.def(db).const_(db, name).is_some() {
                    matches.insert(pred);
                }

                // Include super-trait consts so `T::CONST` works through a `T: SubTrait` bound
                // even when `assumptions` hasn't been transitively expanded.
                for super_trait in pred.def(db).super_traits(db) {
                    let super_inst = super_trait.instantiate(db, pred.args(db));
                    if super_inst.def(db).const_(db, name).is_some() {
                        matches.insert(super_inst);
                    }
                }
            }

            table.rollback_to(snapshot);
        }

        return match matches.len() {
            0 => AssocConstSelection::NotFound,
            1 => AssocConstSelection::Found(*matches.iter().next().unwrap()),
            _ => AssocConstSelection::Ambiguous(matches.into_iter().collect()),
        };
    }

    let canonical_receiver = Canonicalized::new(db, receiver_ty).value;
    let scope_ingot = scope.ingot(db);

    // Find trait impls for the receiver type that define the associated const, searching both:
    // - the call-site ingot (for local traits implemented for external types), and
    // - the receiver type's ingot (for external traits implemented in the receiver ingot).
    let search_ingots = [
        Some(scope_ingot),
        receiver_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    let mut matches: IndexSet<TraitInstId<'db>> = IndexSet::default();
    for ingot in search_ingots.into_iter().flatten() {
        for cand in impls_for_ty_with_constraints(db, ingot, canonical_receiver, assumptions) {
            let inst = cand.skip_binder().trait_(db);
            let trait_ = inst.def(db);
            if trait_.const_(db, name).is_some() {
                matches.insert(inst);
            }
        }
    }

    match matches.len() {
        0 => AssocConstSelection::NotFound,
        1 => AssocConstSelection::Found(*matches.iter().next().unwrap()),
        _ => AssocConstSelection::Ambiguous(matches.into_iter().collect()),
    }
}

pub fn find_associated_type<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
) -> SmallVec<(TraitInstId<'db>, TyId<'db>), 4> {
    // Qualified type: `<A as T>::B`. Always construct the associated type projection
    // against the qualified trait instance; bindings (if any) will be handled downstream.
    if let TyData::QualifiedTy(trait_inst) = ty.value.data(db) {
        let proj = TyId::assoc_ty(db, *trait_inst, name);
        return smallvec![(*trait_inst, proj)];
    }

    let scope_ingot = scope.ingot(db);
    // Use a single unification table and snapshots to preserve outer
    // substitutions while isolating per-candidate attempts.
    let mut table = UnificationTable::new(db);
    let lhs_ty = ty.extract_identity(&mut table);

    if let TyData::TyParam(param) = ty.value.data(db) {
        // Trait self, in trait or impl trait. Associated type must be in this trait.
        if param.is_trait_self() {
            if let Some(trait_) = param.owner.resolve_to::<Trait>(db) {
                if trait_.assoc_ty(db, name).is_some() {
                    let trait_inst = TraitInstId::new(db, trait_, vec![ty.value], IndexMap::new());
                    let assoc_ty = TyId::assoc_ty(db, trait_inst, name);
                    return smallvec![(trait_inst, assoc_ty)];
                }
            } else if let Some(impl_trait) = param.owner.resolve_to::<ImplTrait>(db)
                && let Some(trait_inst) = impl_trait.trait_inst(db)
                && let Some(assoc_ty) = trait_inst.assoc_ty(db, name)
            {
                return smallvec![(trait_inst, assoc_ty)];
            }
        }
    }

    let mut candidates = SmallVec::new();
    // Check explicit bounds in assumptions that match `ty` only when `ty` is a type
    // parameter (to avoid spurious ambiguities for concrete types that already have impls).
    if let TyData::TyParam(_) = ty.value.data(db) {
        for &trait_inst in assumptions.list(db) {
            // `trait_inst` is a specific trait bound, e.g., `A: Abi` or `S<A>: SomeTrait`.
            let snapshot = table.snapshot();
            let pred_self_ty =
                table.instantiate_with_fresh_vars(Binder::bind(trait_inst.self_ty(db)));

            if table.unify(lhs_ty, pred_self_ty).is_ok()
                && let Some(assoc_ty) = trait_inst.assoc_ty(db, name)
            {
                let folded = assoc_ty.fold_with(db, &mut table);
                candidates.push((trait_inst, folded));
            }
            table.rollback_to(snapshot);
        }
    }

    let search_ingots = [
        Some(scope_ingot),
        ty.value.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    // Check impls for `ty` across both the call-site ingot and `ty`'s defining ingot.
    for ingot in search_ingots.into_iter().flatten() {
        for impl_ in impls_for_ty_with_constraints(db, ingot, ty, assumptions) {
            let snapshot = table.snapshot();
            let impl_ = table.instantiate_with_fresh_vars(impl_);

            if table.unify(lhs_ty, impl_.self_ty(db)).is_ok()
                && let Some(ty) = impl_.assoc_ty(db, name)
            {
                let folded = ty.fold_with(db, &mut table);
                candidates.push((impl_.trait_(db), folded));
            }
            table.rollback_to(snapshot);
        }
    }

    // Case 3: The LHS `ty` is an associated type (e.g., `T::Encoder` in `T::Encoder::Output`).
    // We need to look at the trait bound on the associated type.
    if let TyData::AssocTy(assoc_ty) = ty.value.data(db) {
        // Extract the canonical type's substitutions into the unification table
        // This ensures we maintain any type parameter bindings from the outer context
        let ty_with_subst = ty.extract_identity(&mut table);

        // First, check if there are trait bounds on this associated type in the assumptions
        // (e.g., from where clauses like `T::Assoc: Level1`).
        for &trait_inst in assumptions.list(db) {
            let snapshot = table.snapshot();
            // Allow unification to account for type variables in either side
            if table.unify(ty_with_subst, trait_inst.self_ty(db)).is_ok()
                && let Some(assoc_ty) = trait_inst.assoc_ty(db, name)
            {
                let folded = assoc_ty.fold_with(db, &mut table);
                candidates.push((trait_inst, folded));
            }
            table.rollback_to(snapshot);
        }

        // Also check bounds defined on the associated type in the trait definition.
        // We need to use the calling context's scope/assumptions (not the trait's) so that
        // path resolution works correctly.
        let trait_ = assoc_ty.trait_.def(db);
        let assoc_name = assoc_ty.name;
        if let Some(decl) = trait_.assoc_ty(db, assoc_name) {
            let subject = ty_with_subst.fold_with(db, &mut table);
            // owner_self is used to substitute `Self` in bounds like `type Assoc: Encode<Self>`
            let owner_self = assoc_ty.trait_.self_ty(db);
            for bound in &decl.bounds {
                if let TypeBound::Trait(trait_ref) = *bound
                    && let Ok(inst) = crate::analysis::ty::trait_lower::lower_trait_ref(
                        db,
                        subject,
                        trait_ref,
                        scope,
                        assumptions,
                        Some(owner_self),
                    )
                    && inst.def(db).assoc_ty(db, name).is_some()
                {
                    let assoc_ty = TyId::assoc_ty(db, inst, name);
                    let folded = assoc_ty.fold_with(db, &mut table);
                    candidates.push((inst, folded));
                }
            }
        }
    }

    candidates
}

pub fn resolve_name_res<'db>(
    db: &'db dyn HirAnalysisDb,
    nameres: &NameRes<'db>,
    parent_ty: Option<TyId<'db>>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    let args = &lower_generic_arg_list(db, path.generic_args(db), scope, assumptions);

    let res = match nameres.kind {
        NameResKind::Prim(prim) => {
            let ty = TyId::from_hir_prim_ty(db, prim);
            PathRes::Ty(TyId::foldl(db, ty, args))
        }
        NameResKind::Scope(scope_id) => match scope_id {
            ScopeId::Item(item) => match item {
                ItemKind::Struct(_) | ItemKind::Enum(_) => {
                    let adt_ref = AdtRef::try_from_item(item).unwrap();
                    PathRes::Ty(ty_from_adtref(db, path, adt_ref, args, assumptions)?)
                }
                ItemKind::Contract(contract) => {
                    // Contracts have no generic parameters
                    if !args.is_empty() {
                        return Err(PathResError::new(
                            PathResErrorKind::ArgNumMismatch {
                                expected: 0,
                                given: args.len(),
                            },
                            path,
                        ));
                    }
                    PathRes::Ty(TyId::contract(db, contract))
                }

                ItemKind::Mod(_) | ItemKind::TopMod(_) => PathRes::Mod(scope_id),

                ItemKind::Func(func) => {
                    let func_def = func.as_callable(db).unwrap();
                    let ty = TyId::func(db, func_def);
                    // Generic args for callables are unified in the type-checker (for both call
                    // expressions and callable values), which knows how to align explicit args
                    // after implicit params like effect-provider generics).
                    //
                    // Applying them eagerly here via `foldl` would bind user generics to the wrong
                    // parameter slots once implicit params are prepended.
                    PathRes::Func(ty)
                }
                ItemKind::Const(const_) => {
                    if !args.is_empty() {
                        return Err(PathResError::new(
                            PathResErrorKind::ArgNumMismatch {
                                expected: 0,
                                given: args.len(),
                            },
                            path,
                        ));
                    }
                    // Use semantic const type.
                    let ty = const_.ty(db);
                    PathRes::Const(const_, ty)
                }

                ItemKind::TypeAlias(type_alias) => {
                    let alias = lower_type_alias(db, type_alias);
                    let expected = alias.params(db).len();
                    if args.len() > expected {
                        return Err(PathResError::new(
                            PathResErrorKind::ArgNumMismatch {
                                expected,
                                given: args.len(),
                            },
                            path,
                        ));
                    } else {
                        let completed = alias.param_set.complete_explicit_args_with_defaults(
                            db,
                            None,
                            args,
                            assumptions,
                        );
                        if completed.len() < expected {
                            return Ok(PathRes::TyAlias(
                                alias.clone(),
                                TyId::invalid(
                                    db,
                                    InvalidCause::UnboundTypeAliasParam {
                                        alias: type_alias,
                                        n_given_args: args.len(),
                                    },
                                ),
                            ));
                        }

                        let instantiated = alias.alias_to.instantiate(db, &completed);
                        PathRes::TyAlias(alias.clone(), instantiated)
                    }
                }

                ItemKind::Impl(impl_) => {
                    let base = impl_.ty(db);
                    PathRes::Ty(TyId::foldl(db, base, args))
                }
                ItemKind::ImplTrait(impl_) => {
                    let base = impl_.ty(db);
                    PathRes::Ty(TyId::foldl(db, base, args))
                }

                ItemKind::Trait(t) => {
                    if path.is_self_ty(db) {
                        let params = collect_generic_params(db, t.into());
                        let ty = params.trait_self(db).unwrap();
                        let ty = TyId::foldl(db, ty, args);
                        PathRes::Ty(ty)
                    } else {
                        // Pre-validate type generic arguments of the trait path to surface
                        // domain errors (e.g., trait or value used where a type is expected)
                        // with precise spans in the name-resolution phase.
                        if !path.generic_args(db).is_empty(db) {
                            let gen_args = path.generic_args(db).data(db);
                            for (idx, ga) in gen_args.iter().enumerate() {
                                if let GenericArg::Type(ty_arg) = ga
                                    && let Some(hir_ty) = ty_arg.ty.to_opt()
                                    && let TypeKind::Path(p) = hir_ty.data(db)
                                    && let Some(arg_path) = p.to_opt()
                                {
                                    match resolve_path(db, arg_path, scope, assumptions, false) {
                                        Ok(res)
                                            if !matches!(
                                                res,
                                                PathRes::Ty(_) | PathRes::TyAlias(..)
                                            ) =>
                                        {
                                            let ident = arg_path.ident(db).unwrap();
                                            let kind = res.kind_name();
                                            return Err(PathResError::new(
                                                PathResErrorKind::TraitGenericArgType {
                                                    arg_idx: idx,
                                                    ident,
                                                    given_kind: kind,
                                                },
                                                path,
                                            ));
                                        }
                                        Ok(_) => {}
                                        Err(inner) => {
                                            // Bubble up inner error; caller will render
                                            return Err(inner);
                                        }
                                    }
                                }
                            }
                        }
                        match lower_trait_ref_impl(db, path, scope, assumptions, t) {
                            Ok(t) => PathRes::Trait(t),
                            Err(err) => {
                                let kind = match err {
                                    TraitArgError::ArgNumMismatch { expected, given } => {
                                        PathResErrorKind::ArgNumMismatch { expected, given }
                                    }
                                    TraitArgError::ArgKindMisMatch { expected, given } => {
                                        PathResErrorKind::ArgKindMisMatch { expected, given }
                                    }
                                    TraitArgError::ArgTypeMismatch { expected, given } => {
                                        PathResErrorKind::ArgTypeMismatch { expected, given }
                                    }
                                    TraitArgError::Ignored => PathResErrorKind::ParseError,
                                };
                                return Err(PathResError {
                                    kind,
                                    failed_at: path,
                                });
                            }
                        }
                    }
                }

                ItemKind::Use(_) | ItemKind::Body(_) => unreachable!(),
            },
            ScopeId::GenericParam(parent, idx) => {
                let owner = GenericParamOwner::from_item_opt(parent).unwrap();
                let param_set = collect_generic_params(db, owner);
                let ty = param_set.param_by_original_idx(db, idx as usize).unwrap();
                let ty = TyId::foldl(db, ty, args);
                PathRes::Ty(ty)
            }

            ScopeId::TraitType(t, idx) => {
                let trait_def = t;
                let trait_type = t.assoc_ty_by_index(db, idx as usize);

                let params = collect_generic_params(db, t.into());
                let self_ty = params.trait_self(db).unwrap();

                let mut trait_args = vec![self_ty];
                trait_args.extend_from_slice(args);
                let trait_inst = TraitInstId::new(db, trait_def, &trait_args, IndexMap::new());

                // Create an associated type reference
                let assoc_ty_name = trait_type.name.unwrap();
                let assoc_ty = TyId::assoc_ty(db, trait_inst, assoc_ty_name);

                PathRes::Ty(assoc_ty)
            }

            ScopeId::TraitConst(t, idx) => {
                let params = collect_generic_params(db, t.into());
                let self_ty = params.trait_self(db).unwrap();

                let mut trait_args = vec![self_ty];
                trait_args.extend_from_slice(args);
                let trait_inst = TraitInstId::new(db, t, trait_args, IndexMap::new());

                let const_name = t.const_by_index(idx as usize).name(db).unwrap();
                PathRes::TraitConst(self_ty, trait_inst, const_name)
            }

            ScopeId::Variant(var) => {
                let enum_ty = if let Some(ty) = parent_ty {
                    ty
                } else {
                    // The variant was imported via `use`.
                    debug_assert!(path.parent(db).is_none());
                    ty_from_adtref(db, path, var.enum_.into(), &[], assumptions)?
                };
                // TODO report error if args isn't empty
                PathRes::EnumVariant(ResolvedVariant {
                    ty: enum_ty,
                    variant: var,
                    path,
                })
            }
            ScopeId::FuncParam(item, idx) => {
                if !args.is_empty() {
                    return Err(PathResError::new(
                        PathResErrorKind::ArgNumMismatch {
                            expected: 0,
                            given: args.len(),
                        },
                        path,
                    ));
                }
                PathRes::FuncParam(item, idx)
            }
            ScopeId::Field(..) => unreachable!(),
            ScopeId::Block(..) => unreachable!(),
        },
    };
    Ok(res)
}

fn ty_from_adtref<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    adt_ref: AdtRef<'db>,
    args: &[TyId<'db>],
    assumptions: PredicateListId<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    let adt = adt_ref.as_adt(db);
    let ty = TyId::adt(db, adt);
    // Fill trailing defaults (if any)
    let completed_args =
        adt.param_set(db)
            .complete_explicit_args_with_defaults(db, None, args, assumptions);
    let applied = TyId::foldl(db, ty, &completed_args);
    if let TyData::Invalid(InvalidCause::TooManyGenericArgs { expected, given }) = applied.data(db)
    {
        Err(PathResError::new(
            PathResErrorKind::ArgNumMismatch {
                expected: *expected,
                given: *given,
            },
            path,
        ))
    } else {
        Ok(applied)
    }
}

fn pick_type_domain_from_bucket<'db>(
    parent: Option<PathRes<'db>>,
    bucket: &NameResBucket<'db>,
    path: PathId<'db>,
    parent_path: Option<PathId<'db>>,
) -> PathResolutionResult<'db, NameRes<'db>> {
    bucket
        .pick(NameDomain::TYPE)
        .clone()
        .map_err(|err| match err {
            NameResolutionError::NotFound => {
                // If something was found in a different domain, mark the failure at
                // the parent segment to surface an InvalidPathSegment diagnostic.
                let failed_at = if bucket.iter_ok().next().is_some() {
                    parent_path.unwrap_or(path)
                } else {
                    path
                };
                PathResError::new(
                    PathResErrorKind::NotFound {
                        parent: parent.clone(),
                        bucket: bucket.clone(),
                    },
                    failed_at,
                )
            }
            err => PathResError::from_name_res_error(err, path),
        })
}
