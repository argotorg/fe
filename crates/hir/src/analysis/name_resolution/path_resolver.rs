use crate::core::hir_def::{GenericArg, TypeMode};
use crate::hir_def::{CallableDef, Func};
use crate::{
    core::hir_def::{
        Const, Enum, EnumVariant, GenericArgListId, GenericParamOwner, HirIngot, IdentId, Impl,
        ImplTrait, ItemKind, Partial, PathId, PathKind, Trait, TypeBound, TypeId, TypeKind,
        VariantKind, scope_graph::ScopeId,
    },
    core::semantic::trait_self_predicate,
    span::{DynLazySpan, path::LazyPathSpan, types::LazyTySpan},
};
use common::indexmap::{IndexMap, IndexSet};
use either::Either;
use rustc_hash::FxHashMap;
use smallvec::{SmallVec, smallvec};
use std::iter::successors;
use thin_vec::ThinVec;

use super::{
    EarlyNameQueryId, ExpectedPathKind, NameDomain,
    diagnostics::PathResDiag,
    is_scope_visible_from,
    method_selection::{MethodCandidate, MethodSelectionError, select_method_candidate},
    name_resolver::{NameRes, NameResBucket, NameResKind, NameResolutionError},
    resolve_query,
    traits_in_scope::traits_available_for_selection,
    visibility_checker::is_ty_visible_from,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::QueryDirective,
    ty::{
        adt_def::AdtRef,
        binder::Binder,
        canonical::Canonicalized,
        const_ty::{ConstBodyLowering, LoweringContext},
        diagnostics::{RevealedBy, TyDiagCollection, TyLowerDiag},
        fold::TyFoldable as _,
        generic_defaults::DefaultApplication,
        method_table::{MethodProbe, probe_method},
        normalize::normalize_ty,
        trait_def::{
            AssocBinding, ImplementorId, ImplementorOrigin, TraitInstId,
            impls_for_ty_with_satisfied_constraints,
        },
        trait_lower::{
            TraitRefLowerError, complete_candidate_impl_assoc_ty, complete_impl_assoc_ty,
            lower_candidate_impl_assoc_ty, lower_checked_impl_assoc_ty, lower_trait_ref,
            lower_trait_ref_impl_with_minter, lower_trait_ref_with_minter,
        },
        trait_resolution::{
            GoalSatisfiability, PredicateListId, TraitSolveCx, constraint::collect_constraints,
            is_goal_satisfiable,
        },
        ty_def::{InvalidCause, Kind, ShapeMode, TyBase, TyData, TyId},
        ty_error::emit_invalid_ty_error,
        ty_lower::{
            TyAlias, collect_generic_params, lower_generic_arg_list, lower_hir_ty_with_minter,
            lower_type_alias, lower_type_alias_deferred, lower_type_position_path,
        },
        unify::UnificationTable,
    },
};

pub type PathResolutionResult<'db, T> = Result<T, PathResError<'db>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct PathResError<'db> {
    pub kind: PathResErrorKind<'db>,
    pub failed_at: PathId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
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

    InfiniteBoundRecursion {
        context: &'static str,
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
    /// A trait argument failed compile-time evaluation; it is reported as
    /// that fault, from the reference when only an applied default reveals it.
    InvalidTraitArg {
        arg: TyId<'db>,
        applied_default: bool,
    },

    /// A path written in a type argument or associated type binding of a
    /// trait path, the `nth` that `find_written_path` visits there, names
    /// no type where it is written: `Ok` with what it names instead, `Err`
    /// with why it does not resolve.
    TraitGenericArgType {
        arg_idx: usize,
        nth: usize,
        result: Box<PathResolutionResult<'db, PathRes<'db>>>,
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
            PathResErrorKind::InvalidTraitArg { .. } => {
                "Trait argument failed compile-time evaluation".to_string()
            }
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
            PathResErrorKind::InfiniteBoundRecursion { .. } => {
                "Infinite trait bound recursion".to_string()
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
            PathResErrorKind::TraitGenericArgType { result, .. } => match result.as_ref() {
                Ok(_) => "Trait generic argument expects a type".to_string(),
                Err(err) => err.print(),
            },
            PathResErrorKind::MethodSelection(err) => match err {
                MethodSelectionError::AmbiguousInherentMethod(cands) => {
                    format!("Ambiguous method; {} inherent candidates.", cands.len())
                }
                MethodSelectionError::AmbiguousTraitMethod(ambiguous) => {
                    format!(
                        "Ambiguous method; {} trait candidates.",
                        ambiguous.diagnostic_traits.len()
                    )
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

    fn is_infinite_bound_recursion(&self) -> bool {
        match &self.kind {
            PathResErrorKind::InfiniteBoundRecursion { .. } => true,
            PathResErrorKind::QualifiedTypeType(result)
            | PathResErrorKind::QualifiedTypeTrait(result)
            | PathResErrorKind::TraitGenericArgType { result, .. } => {
                matches!(result.as_ref(), Err(inner) if inner.is_infinite_bound_recursion())
            }
            _ => false,
        }
    }

    /// The diagnostic for an error in a path written where `expected` is. A
    /// fault in a trait argument, which a path resolution diagnostic cannot
    /// describe, is reported as that fault at the trait path.
    pub fn into_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        path_span: LazyPathSpan<'db>,
        expected: ExpectedPathKind,
    ) -> Option<TyDiagCollection<'db>> {
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
            PathResErrorKind::InvalidTraitArg {
                arg,
                applied_default,
            } => {
                let fault = emit_invalid_ty_error(db, arg, path_span.clone().into())?;
                return Some(if applied_default {
                    TyLowerDiag::RevealedConstFault {
                        site: path_span.into(),
                        revealed_by: RevealedBy::AppliedDefault,
                        fault: Box::new(fault),
                    }
                    .into()
                } else {
                    fault
                });
            }
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

            PathResErrorKind::InfiniteBoundRecursion { context } => {
                PathResDiag::InfiniteBoundRecursion(
                    span,
                    format!("cyclic trait reference prevented lowering this {context}"),
                )
            }

            PathResErrorKind::QualifiedTypeType(result) => {
                let ty_span = seg_span.qualified_type().ty().into_path_type().path();
                match *result {
                    Ok(res) => {
                        let type_ident = if let PathKind::QualifiedType { type_, .. } =
                            seg_path.kind(db)
                            && let TypeKind::Path(type_path) = type_.data(db)
                        {
                            type_path.unwrap().ident(db).unwrap()
                        } else {
                            ident
                        };
                        PathResDiag::ExpectedType(ty_span.into(), type_ident, res.kind_name())
                    }
                    Err(inner) => {
                        let failed = inner.failed_at;
                        return inner.into_diag(db, failed, ty_span, ExpectedPathKind::Type);
                    }
                }
            }
            PathResErrorKind::QualifiedTypeTrait(result) => {
                let trait_span = seg_span.qualified_type().trait_qualifier();
                match *result {
                    Ok(res) => {
                        let trait_ident = match seg_path.kind(db) {
                            PathKind::QualifiedType { trait_, .. } => {
                                trait_.path(db).unwrap().ident(db).unwrap()
                            }
                            PathKind::Ident { .. } => ident,
                        };
                        let span = trait_span.name().into();
                        PathResDiag::ExpectedTrait(span, trait_ident, res.kind_name())
                    }
                    Err(inner) => {
                        let failed = inner.failed_at;
                        let span = trait_span.path();
                        return inner.into_diag(db, failed, span, ExpectedPathKind::Trait);
                    }
                }
            }

            PathResErrorKind::MethodSelection(err) => match err {
                MethodSelectionError::ReceiverTypeMustBeKnown => PathResDiag::TypeMustBeKnown(span),
                MethodSelectionError::AmbiguousInherentMethod(candidates) => {
                    PathResDiag::AmbiguousInherentMethod {
                        primary: span,
                        method_name: ident,
                        candidates,
                    }
                }
                MethodSelectionError::AmbiguousTraitMethod(ambiguous) => {
                    PathResDiag::AmbiguousTrait {
                        primary: span,
                        method_name: ident,
                        trait_insts: ambiguous.diagnostic_traits,
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

            // The path's own diagnostic, where the argument writes it.
            PathResErrorKind::TraitGenericArgType {
                arg_idx,
                nth,
                result,
            } => {
                let arg = path_span
                    .clone()
                    .segment(failed_idx)
                    .generic_args()
                    .arg(arg_idx);
                let (ty, ty_span, position) =
                    match seg_path.generic_args(db).data(db).get(arg_idx)? {
                        GenericArg::Type(ty_arg) => (
                            ty_arg.ty,
                            arg.into_type_arg().ty(),
                            TypePosition::GenericArg,
                        ),
                        GenericArg::AssocType(binding) => (
                            binding.ty,
                            arg.into_assoc_type_arg().ty(),
                            TypePosition::Type,
                        ),
                        GenericArg::Const(_) | GenericArg::Mode(_) => return None,
                    };
                let mut visited = 0;
                let (arg_path, span) = find_written_path(
                    db,
                    ty.to_opt()?,
                    position,
                    Some(ty_span),
                    &mut |arg_path, _, span| {
                        visited += 1;
                        (visited > nth).then_some((arg_path, span))
                    },
                )?;
                let span = span?;
                match *result {
                    Ok(res) => PathResDiag::ExpectedType(
                        span.into(),
                        arg_path.ident(db).to_opt()?,
                        res.kind_name(),
                    ),
                    Err(inner) => {
                        return inner.into_diag(db, arg_path, span, ExpectedPathKind::Type);
                    }
                }
            }
        };
        Some(diag.into())
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
    resolve_query(db, make_query(db, path, scope))
}

/// Resolves only the definition named by a type-domain path, without lowering
/// any generic arguments attached to its segments.
///
/// Raw trait-impl collection uses this to build candidate indexes before the
/// trait environment exists. Full path resolution is intentionally unsuitable
/// there because const generic expressions can type-check operators through
/// that same environment.
pub(crate) fn resolve_type_path_definition<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<NameResKind<'db>> {
    let mut parent_scope = None;
    for segment_idx in 0..path.len(db) {
        let segment = path.segment(db, segment_idx)?;
        if !matches!(segment.kind(db), PathKind::Ident { .. }) {
            return None;
        }
        let query = make_query(db, segment, parent_scope.unwrap_or(scope));
        let resolved = resolve_query(db, query)
            .pick(NameDomain::TYPE)
            .as_ref()
            .ok()?;
        if segment_idx + 1 == path.len(db) {
            return Some(resolved.kind);
        }
        parent_scope = resolved.scope();
    }
    None
}

/// Makes the query for the last segment of `path` in `scope`. Only a path's
/// first segment is looked up in lexical parents and external ingots.
fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> EarlyNameQueryId<'db> {
    let directive = if path.segment_index(db) == 0 {
        QueryDirective::new()
    } else {
        QueryDirective::new().disallow_external().disallow_lex()
    };

    let name = path
        .ident(db)
        .to_opt()
        .unwrap_or_else(|| IdentId::new(db, "_".to_string()));
    EarlyNameQueryId::new(db, name, scope, directive)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
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
    /// An associated const defined in an inherent `impl` block,
    /// e.g. `Foo::SIZE` where `impl Foo { const SIZE: u256 = 32 }`.
    InherentConst(TyId<'db>, Impl<'db>, IdentId<'db>),
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
            PathRes::InherentConst(ty, impl_, name) => PathRes::InherentConst(f(ty), impl_, name),
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
                let idx = trait_.const_index(db, *name)? as u16;
                Some(ScopeId::TraitConst(trait_, idx))
            }
            PathRes::InherentConst(_ty, impl_, name) => {
                let idx = impl_.const_index(db, *name)? as u16;
                Some(ScopeId::ImplConst(*impl_, idx))
            }
            PathRes::TyAlias(alias, _) => Some(alias.alias.scope()),
            PathRes::Trait(trait_) => Some(trait_.def(db).scope()),
            PathRes::TraitMethod(_inst, method) => Some(method.scope()),
            PathRes::EnumVariant(variant) => Some(ScopeId::Variant(variant.variant)),
            PathRes::FuncParam(item, idx) => Some(ScopeId::FuncParam(*item, *idx)),
            PathRes::Mod(scope) => Some(*scope),
            PathRes::Method(_, cand) => {
                let scope = match cand {
                    MethodCandidate::InherentMethod(cand) => cand.def.scope(),
                    MethodCandidate::TraitMethod(c) | MethodCandidate::NeedsConfirmation(c) => {
                        c.method.scope()
                    }
                };
                Some(scope)
            }
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
            // Anything else is as visible as its own scope, which for a method is
            // its definition, not its receiver type or trait. An inherent const
            // whose impl no longer has the name has no scope and isn't visible.
            r => r
                .as_scope(db)
                .is_some_and(|scope| is_scope_visible_from(db, scope, from_scope)),
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
            PathRes::EnumVariant(v) => Some(format!(
                "{}::{}",
                ty_path(v.ty).unwrap_or_else(|| "<missing>".into()),
                v.variant.name(db)?
            )),
            PathRes::TraitConst(ty, _, name) | PathRes::InherentConst(ty, _, name) => {
                Some(format!(
                    "{}::{}",
                    ty_path(*ty).unwrap_or_else(|| "<missing>".into()),
                    name.data(db)
                ))
            }
            PathRes::TyAlias(..)
            | PathRes::Const(..)
            | PathRes::TraitMethod(..)
            | PathRes::Trait(..)
            | PathRes::Mod(..)
            | PathRes::FuncParam(..) => self.as_scope(db)?.pretty_path(db),

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
            PathRes::InherentConst(..) => "constant",
            PathRes::Mod(_) => "module",
            PathRes::Method(..) => "method",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
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
    ) -> impl Iterator<Item = Binder<'db, TyId<'db>>> {
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
    let lowering_cx = LoweringContext::new();
    resolve_path_with_minter(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        &lowering_cx,
    )
}

/// Like [`resolve_path`], but resolves with the caller's lowering context, so
/// const bodies follow its mode. Each resolved segment is also reported to
/// the context, which keeps it when its lowering records resolutions.
pub(crate) fn resolve_path_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    resolve_path_with_observer_and_minter(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        &mut |_, _| {},
        lowering_cx,
    )
}

pub(crate) fn resolve_path_with_observer_and_minter<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    observer: &mut F,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        true,
        &mut |path, res| {
            lowering_cx.record_resolution(path, res);
            observer(path, res)
        },
        lowering_cx,
    )
}

/// The trait that the bare ident `path`, written where a trait is expected in
/// `scope`, names although it resolves to `definition`, an associated type of
/// an enclosing trait that lexically shadows a same-named trait
/// (`trait Abi { type Decoder: Decoder<Self> }`). It is looked up from outside
/// the shadowing trait.
pub(crate) fn shadowed_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    definition: NameResKind<'db>,
) -> Option<&'db NameRes<'db>> {
    if let NameResKind::Scope(ScopeId::TraitType(shadowing_trait, _)) = definition
        && path.parent(db).is_none()
        // The shadow can only come from a trait that encloses the use site.
        && scope.is_transitive_child_of(db, shadowing_trait.scope())
        && let Some(outer_scope) = shadowing_trait.scope().parent(db)
        && let Ok(outer) = resolve_ident_to_bucket(db, path, outer_scope).pick(NameDomain::TYPE)
        && matches!(outer.kind, NameResKind::Scope(ScopeId::Item(ItemKind::Trait(_))))
    {
        Some(outer)
    } else {
        None
    }
}

/// Resolves `path` written where a trait is expected.
///
/// What the path names is looked up before any generic args are lowered
/// against it. A trait path can never name an associated type, but an
/// associated type of an enclosing trait lexically shadows a same-named trait
/// (`trait Abi { type Decoder: Decoder<Self> }`). A bare ident naming such an
/// associated type is looked up from outside the shadowing trait instead,
/// while its generic args are still lowered in `scope`, so they can reference
/// the trait's own generic params. A path naming anything else that isn't a
/// trait is resolved without its generic args, so it's reported as not being
/// a trait rather than for the args that thing doesn't take.
pub(crate) fn resolve_trait_path_with_observer_and_minter<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    observer: &mut F,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let is_trait = |kind| matches!(kind, NameResKind::Scope(ScopeId::Item(ItemKind::Trait(_))));
    let definition = resolve_type_path_definition(db, path, scope);
    if let Some(outer) =
        definition.and_then(|definition| shadowed_trait(db, path, scope, definition))
    {
        let res = resolve_name_res_with_minter(db, outer, path, scope, assumptions, lowering_cx)?;
        lowering_cx.record_resolution(path, &res);
        observer(path, &res);
        return Ok(res);
    }

    let path = match definition {
        Some(kind) if !is_trait(kind) => PathId::new(
            db,
            PathKind::Ident {
                ident: path.ident(db),
                generic_args: GenericArgListId::none(db),
            },
            path.parent(db),
        ),
        _ => path,
    };
    resolve_path_with_observer_and_minter(
        db,
        path,
        scope,
        assumptions,
        false,
        observer,
        lowering_cx,
    )
}

/// How a path written where a type is expected chooses between the type and
/// the value its last segment can name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypePosition {
    /// A type. The path names a value only when it names no type.
    Type,
    /// A generic argument, which is syntactically ambiguous: `N` in
    /// `String<N>` is the constant it names, and otherwise a type.
    GenericArg,
}

/// Resolves `path`, written in a type `position`, once. Its parent segments
/// are resolved a single time and only its last segment is tried in each
/// namespace: resolving the whole path again would lower every generic
/// argument nested in it again, doubling the work at each nesting level.
pub(crate) fn resolve_type_position_path_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    position: TypePosition,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    let mut observer =
        |path: PathId<'db>, res: &PathRes<'db>| lowering_cx.record_resolution(path, res);
    let parent_res = path
        .parent(db)
        .map(|parent| {
            resolve_path_impl(
                db,
                parent,
                scope,
                assumptions,
                false,
                false,
                &mut observer,
                lowering_cx,
            )
        })
        .transpose()?;
    let resolve_tail = |as_value, decided_by_value: &mut bool| {
        resolve_segment(
            db,
            path,
            parent_res.clone(),
            scope,
            assumptions,
            as_value,
            true,
            lowering_cx,
            decided_by_value,
        )
        .inspect(|res| lowering_cx.record_resolution(path, res))
    };
    let not_found = |res: &PathResolutionResult<'db, PathRes<'db>>| {
        res.as_ref()
            .is_err_and(|err| matches!(err.kind, PathResErrorKind::NotFound { .. }))
    };
    match position {
        TypePosition::Type => {
            let ty = resolve_tail(false, &mut false);
            // When neither is found, the type's error is the one to report.
            if not_found(&ty)
                && let Ok(value) = resolve_tail(true, &mut false)
            {
                Ok(value)
            } else {
                ty
            }
        }
        TypePosition::GenericArg => {
            let mut decided_by_value = false;
            let value = resolve_tail(true, &mut decided_by_value);
            // An outcome no value-only branch decided is the type's as well. A
            // value that is not a constant, such as a function, gives way to a
            // type of the same name; every value-only branch precedes the type
            // namespace's lowering, so resolving the segment as a type then
            // repeats none of it.
            let settled = !decided_by_value
                || value.as_ref().is_ok_and(|res| match res {
                    PathRes::Ty(_)
                    | PathRes::Const(..)
                    | PathRes::TraitConst(..)
                    | PathRes::InherentConst(..) => true,
                    PathRes::EnumVariant(variant) => variant.ty.is_unit_variant_only_enum(db),
                    _ => false,
                });
            if settled {
                value
            } else {
                let ty = resolve_tail(false, &mut false);
                if not_found(&ty) { value } else { ty }
            }
        }
    }
}

/// Visits the paths written in `ty`, which is written at `position`, each
/// before the types written in it, until `visit` returns a value. Each is
/// visited with its position and, when `span` is `ty`'s span, its own.
fn find_written_path<'db, R>(
    db: &'db dyn HirAnalysisDb,
    ty: TypeId<'db>,
    position: TypePosition,
    span: Option<LazyTySpan<'db>>,
    visit: &mut impl FnMut(PathId<'db>, TypePosition, Option<LazyPathSpan<'db>>) -> Option<R>,
) -> Option<R> {
    match ty.data(db) {
        TypeKind::Ptr(inner) => find_written_path(
            db,
            inner.to_opt()?,
            TypePosition::Type,
            span.map(|span| span.into_ptr_type().pointee()),
            visit,
        ),
        TypeKind::Mode(_, inner) => find_written_path(
            db,
            inner.to_opt()?,
            TypePosition::Type,
            span.map(|span| span.into_mode_type().inner()),
            visit,
        ),
        TypeKind::Array(elem, _) => find_written_path(
            db,
            elem.to_opt()?,
            TypePosition::Type,
            span.map(|span| span.into_array_type().elem()),
            visit,
        ),
        TypeKind::Tuple(elems) => elems.data(db).iter().enumerate().find_map(|(idx, elem)| {
            find_written_path(
                db,
                elem.to_opt()?,
                TypePosition::Type,
                span.clone().map(|span| span.into_tuple_type().elem_ty(idx)),
                visit,
            )
        }),
        TypeKind::Path(path) => {
            let path = path.to_opt()?;
            let path_span = span.map(|span| span.into_path_type().path());
            if let Some(found) = visit(path, position, path_span.clone()) {
                return Some(found);
            }
            successors(Some(path), |segment| segment.parent(db)).find_map(|segment| {
                let segment_span = path_span
                    .clone()
                    .map(|span| span.segment(segment.segment_index(db)));
                match segment.kind(db) {
                    PathKind::QualifiedType { type_, .. } => find_written_path(
                        db,
                        type_,
                        TypePosition::Type,
                        segment_span.map(|span| span.qualified_type().ty()),
                        visit,
                    ),
                    PathKind::Ident { generic_args, .. } => generic_args
                        .data(db)
                        .iter()
                        .enumerate()
                        .find_map(|(idx, arg)| match arg {
                            GenericArg::Type(ty_arg) => find_written_path(
                                db,
                                ty_arg.ty.to_opt()?,
                                TypePosition::GenericArg,
                                segment_span
                                    .clone()
                                    .map(|span| span.generic_args().arg(idx).into_type_arg().ty()),
                                visit,
                            ),
                            GenericArg::Const(_)
                            | GenericArg::AssocType(_)
                            | GenericArg::Mode(_) => None,
                        }),
                }
            })
        }
        TypeKind::Never => None,
    }
}

/// The first path written in `arg`, a type argument or associated type
/// binding of a trait path, that names no type where it is written: the
/// `nth` that `find_written_path` visits there, with what it names instead or
/// why it does not resolve. An argument, unlike a binding, may name a
/// constant, as a type's generic argument may.
fn trait_arg_path_error<'db>(
    db: &'db dyn HirAnalysisDb,
    arg: &GenericArg<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> Option<(usize, PathResolutionResult<'db, PathRes<'db>>)> {
    let (ty, position) = match arg {
        GenericArg::Type(ty_arg) => (ty_arg.ty, TypePosition::GenericArg),
        GenericArg::AssocType(binding) => (binding.ty, TypePosition::Type),
        GenericArg::Const(_) | GenericArg::Mode(_) => return None,
    };
    let mut visited = 0;
    find_written_path(
        db,
        ty.to_opt()?,
        position,
        None,
        &mut |arg_path, position, _| {
            let nth = visited;
            visited += 1;
            match resolve_path_with_minter(db, arg_path, scope, assumptions, false, lowering_cx) {
                Ok(PathRes::Ty(_) | PathRes::TyAlias(..)) => None,
                _ if position == TypePosition::GenericArg
                    && lower_type_position_path(
                        db,
                        arg_path,
                        scope,
                        assumptions,
                        position,
                        lowering_cx,
                    )
                    .is_ok_and(|ty| ty.is_const_ty(db)) =>
                {
                    None
                }
                result => Some((nth, result)),
            }
        },
    )
}

/// The diagnostic for the first path written in the `arg_idx`th generic
/// argument of trait path `path` that names no type, as resolving the trait
/// path reports it.
pub(crate) fn trait_arg_path_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    path_span: LazyPathSpan<'db>,
    arg_idx: usize,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyDiagCollection<'db>> {
    let arg = path.generic_args(db).data(db).get(arg_idx)?;
    let (nth, result) = trait_arg_path_error(db, arg, scope, assumptions, &LoweringContext::new())?;
    PathResError::new(
        PathResErrorKind::TraitGenericArgType {
            arg_idx,
            nth,
            result: Box::new(result),
        },
        path,
    )
    .into_diag(db, path, path_span, ExpectedPathKind::Trait)
}

#[allow(clippy::too_many_arguments)]
fn resolve_path_impl<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    is_tail: bool,
    observer: &mut F,
    lowering_cx: &LoweringContext<'db>,
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
                false,
                observer,
                lowering_cx,
            )
        })
        .transpose()?;
    let res = resolve_segment(
        db,
        path,
        parent_res,
        scope,
        assumptions,
        resolve_tail_as_value,
        is_tail,
        lowering_cx,
        &mut false,
    )?;
    observer(path, &res);
    Ok(res)
}

/// Resolves the last segment of `path`, given its parent's resolution. Sets
/// `decided_by_value` when a branch that only the value namespace takes
/// decides the outcome; otherwise the outcome is the type namespace's too.
#[allow(clippy::too_many_arguments)]
fn resolve_segment<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    parent_res: Option<PathRes<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    is_tail: bool,
    lowering_cx: &LoweringContext<'db>,
    decided_by_value: &mut bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
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
        // A path keeps the error that its single resolution reports, rather
        // than being resolved again to recover it.
        let ty = match type_.data(db) {
            TypeKind::Path(Partial::Present(ty_path)) => lower_type_position_path(
                db,
                *ty_path,
                scope,
                assumptions,
                TypePosition::Type,
                lowering_cx,
            )
            .map_err(|inner| PathResError {
                kind: PathResErrorKind::QualifiedTypeType(Box::new(Err(inner))),
                failed_at: path,
            })?,
            _ => lower_hir_ty_with_minter(db, type_, scope, assumptions, lowering_cx),
        };
        if let Some(InvalidCause::NotAType(res)) = ty.invalid_cause(db) {
            return Err(PathResError::new(
                PathResErrorKind::QualifiedTypeType(Box::new(Ok(res))),
                path,
            ));
        }
        let trait_inst_result = match lowering_cx.const_bodies() {
            ConstBodyLowering::Eager => lower_trait_ref(db, ty, trait_, scope, assumptions, None),
            ConstBodyLowering::Deferred => {
                lower_trait_ref_with_minter(db, ty, trait_, scope, assumptions, None, lowering_cx)
            }
        };
        let trait_inst = match trait_inst_result {
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
                    TraitRefLowerError::Cycle => PathResError::new(
                        PathResErrorKind::InfiniteBoundRecursion {
                            context: "qualified trait reference",
                        },
                        path,
                    ),
                    TraitRefLowerError::UnsafeLocalBoundBlanketImpl
                    | TraitRefLowerError::Ignored => PathResError::parse_err(trait_path),
                };
                return Err(err);
            }
        };

        let qualified_ty = TyId::qualified_ty(db, trait_inst);
        return Ok(PathRes::Ty(qualified_ty));
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
                if let Some(assoc_ty) = trait_inst.project_assoc_ty(db, ident) {
                    return Ok(PathRes::Ty(match assoc_at_mode_arg(db, assoc_ty, path) {
                        Some(res) => res?,
                        None => assoc_ty,
                    }));
                }

                // Associated function on a specific trait instance
                if is_tail
                    && resolve_tail_as_value
                    && let Some(&method) = trait_inst.def(db).method_defs(db).get(&ident)
                {
                    *decided_by_value = true;
                    return Ok(PathRes::TraitMethod(*trait_inst, method));
                }

                // Associated const on a specific trait instance
                if resolve_tail_as_value && trait_inst.def(db).const_(db, ident).is_some() {
                    *decided_by_value = true;
                    reject_generic_args(db, path)?;
                    return Ok(PathRes::TraitConst(
                        trait_inst.self_ty(db),
                        *trait_inst,
                        ident,
                    ));
                }
            }

            // Try to resolve as an enum variant. Variants take precedence over
            // associated consts so that a const sharing a variant's name can
            // never change the meaning of `E::Variant` (the collision itself is
            // reported at the const definition).
            if let Some(enum_) = ty.as_enum(db) {
                // We need to use the concrete enum scope instead of
                // parent_scope to resolve the variants in all cases,
                // eg when parent is `Self`
                let bucket = resolve_query(db, make_query(db, path, enum_.scope()));

                if let Ok(res) = bucket.pick(NameDomain::VALUE)
                    && let Some(var) = res.enum_variant()
                {
                    // Generic arguments belong on the enum (`E<T>::V`).
                    reject_generic_args(db, path)?;
                    return Ok(PathRes::EnumVariant(ResolvedVariant {
                        ty,
                        variant: var,
                        path,
                    }));
                }
            }

            // Try to resolve as an associated const on the receiver type
            if is_tail && resolve_tail_as_value {
                // Inherent impl consts take precedence over trait impl consts.
                // Conflicting inherent impls are rejected at their definition,
                // so resolve to the first applicable impl here.
                if let Some(impl_) =
                    select_inherent_const_candidate(db, ty, ident, scope, assumptions)
                {
                    *decided_by_value = true;
                    reject_generic_args(db, path)?;
                    return Ok(PathRes::InherentConst(ty, impl_, ident));
                }

                // Probe impls across both the call-site scope and the receiver type's ingot so
                // `OtherIngotType::CONST` and `ExternalType::LOCAL_TRAIT_CONST` both resolve.
                match select_assoc_const_candidate(db, ty, ident, scope, assumptions) {
                    AssocConstSelection::Found(inst) => {
                        *decided_by_value = true;
                        reject_generic_args(db, path)?;
                        return Ok(PathRes::TraitConst(ty, inst, ident));
                    }
                    AssocConstSelection::Ambiguous(traits) => {
                        *decided_by_value = true;
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

            // Deferred signature lowering resolves type/const shapes, not callable
            // values. Method discovery checks signatures and their hidden parameters,
            // so entering it here would make shape discovery depend on validation.
            if is_tail
                && resolve_tail_as_value
                && lowering_cx.const_bodies() == ConstBodyLowering::Eager
            {
                let receiver_ty = Canonicalized::new(db, ty);
                match select_method_candidate(
                    db,
                    &receiver_ty,
                    ident,
                    parent_scope,
                    assumptions,
                    None,
                ) {
                    Ok(cand) => {
                        *decided_by_value = true;
                        return Ok(PathRes::Method(ty, cand));
                    }
                    Err(MethodSelectionError::NotFound) => {}
                    Err(err) => {
                        *decided_by_value = true;
                        return Err(PathResError::method_selection(err, path));
                    }
                }
            }

            // `Self::Assoc` inside an impl is always selected lexically from
            // the owning trait. Signatures preserve that projection until
            // comparison or instantiation. An associated-type definition is
            // different: its unique anchor asks us to resolve the owning
            // impl's sibling binding through the per-binding cycle query.
            let impl_self_assoc = if path.parent(db).is_some_and(|path| path.is_self_ty(db)) {
                let impl_trait = match scope {
                    ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => Some(impl_trait),
                    _ => match scope.parent_item(db) {
                        Some(ItemKind::ImplTrait(impl_trait)) => Some(impl_trait),
                        _ => None,
                    },
                };
                impl_trait.and_then(|impl_trait| {
                    let trait_inst = match lowering_cx.const_bodies() {
                        ConstBodyLowering::Eager => impl_trait.trait_inst_result(db).ok()?,
                        ConstBodyLowering::Deferred => {
                            impl_trait.candidate_trait_inst_result(db).ok()?
                        }
                    };
                    if lowering_cx.impl_assoc_type() == Some(impl_trait) {
                        match lowering_cx.const_bodies() {
                            ConstBodyLowering::Eager => {
                                lower_checked_impl_assoc_ty(db, impl_trait, ident)
                            }
                            ConstBodyLowering::Deferred => {
                                lower_candidate_impl_assoc_ty(db, impl_trait, ident)
                            }
                        }
                        .or_else(|| trait_inst.project_assoc_ty(db, ident))
                    } else {
                        trait_inst.project_assoc_ty(db, ident)
                    }
                })
            } else {
                None
            };

            if let Some(assoc_ty) = impl_self_assoc {
                if let Some(res) = assoc_at_mode_arg(db, assoc_ty, path) {
                    return Ok(PathRes::Ty(res?));
                }
                let seg_args = lower_generic_arg_list(
                    db,
                    path.generic_args(db),
                    scope,
                    assumptions,
                    lowering_cx,
                );
                let assoc_ty = TyId::foldl(db, assoc_ty, &seg_args);
                if let TyData::Invalid(InvalidCause::TooManyGenericArgs { expected, given }) =
                    assoc_ty.data(db)
                {
                    return Err(PathResError::new(
                        PathResErrorKind::ArgNumMismatch {
                            expected: *expected,
                            given: *given,
                        },
                        path,
                    ));
                }
                return Ok(PathRes::Ty(assoc_ty));
            }

            // Find raw associated types, then dedup by normalized result here.
            let assoc_tys = match find_associated_type_in_mode(
                db,
                scope,
                Canonicalized::new(db, ty),
                ident,
                assumptions,
                lowering_cx.const_bodies(),
            ) {
                Ok(assoc_tys) => assoc_tys,
                Err(FindAssociatedTypeError::InfiniteBoundRecursion) => {
                    return Err(PathResError::new(
                        PathResErrorKind::InfiniteBoundRecursion {
                            context: "associated type",
                        },
                        path,
                    ));
                }
            };

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
            let seg_args =
                lower_generic_arg_list(db, path.generic_args(db), scope, assumptions, lowering_cx);
            let evidence = assoc_ty_candidate_evidence(db, ty, assumptions);
            let mut dedup: IndexMap<TyId<'db>, (TraitInstId<'db>, TyId<'db>, TyId<'db>)> =
                IndexMap::new();
            for (inst, ty_candidate) in assoc_tys.iter().copied() {
                // A concrete type's associated shape is its projection, as
                // `<T as Tr>::Step`'s is, which its mode applies to; the
                // impl's value it normalizes to has none.
                let ty_candidate = match inst.project_assoc_ty(db, ident) {
                    Some(projection)
                        if *ty_candidate.kind(db) == Kind::Shape
                            && !matches!(ty_candidate.data(db), TyData::AssocTy(_)) =>
                    {
                        projection
                    }
                    _ => ty_candidate,
                };
                let applied = match assoc_at_mode_arg(db, ty_candidate, path) {
                    Some(res) => res?,
                    None if seg_args.is_empty() => ty_candidate,
                    None => TyId::foldl(db, ty_candidate, &seg_args),
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

                // Interpret each candidate's own equality when comparing it
                // with other candidates. Normalizing the binding-free
                // projection under all assumptions can leave conflicting
                // equalities unresolved and incorrectly merge them.
                // A shape named at a mode takes the equality at that mode.
                let mode = match applied.data(db) {
                    TyData::AssocTy(assoc) => assoc.mode,
                    _ => None,
                };
                let candidate_ty = match inst.bound_assoc_ty(db, AssocBinding { name: ident, mode })
                {
                    Some(bound) if mode.is_some() => bound,
                    Some(bound) => TyId::foldl(db, bound, &seg_args),
                    None => applied,
                };
                let norm = normalize_ty(db, candidate_ty, scope, evidence);
                dedup.entry(norm).or_insert((inst, applied, norm));
            }

            match dedup.len() {
                0 => unreachable!(),
                1 => {
                    let (_, (_, original_ty, _)) = dedup.first().unwrap();
                    return Ok(PathRes::Ty(*original_ty));
                }
                _ => {
                    // Build candidate list from deduped set for diagnostics
                    let candidates = dedup
                        .into_iter()
                        .map(|(_norm, (inst, _, candidate_ty))| (inst, candidate_ty))
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
            | PathRes::InherentConst(..)
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
                *decided_by_value = true;
                return Ok(PathRes::TraitMethod(trait_inst, method));
            }
        }
        Some(PathRes::Const(..) | PathRes::Mod(_)) | None => {}
    };

    let bucket = resolve_query(db, make_query(db, path, parent_scope));
    // A value that is found decides a value tail. One that is ambiguous gives
    // way to a type of the same name, but is the error when there's none.
    let res = match bucket.pick(NameDomain::VALUE) {
        Ok(res) if is_tail && resolve_tail_as_value => {
            *decided_by_value = true;
            res.clone()
        }
        Err(err @ NameResolutionError::Ambiguous(_)) if is_tail && resolve_tail_as_value => {
            pick_type_domain_from_bucket(parent_res, bucket, path).map_err(|_| {
                *decided_by_value = true;
                PathResError::from_name_res_error(err.clone(), path)
            })?
        }
        _ => pick_type_domain_from_bucket(parent_res, bucket, path)?,
    };
    resolve_name_res_with_minter(db, &res, path, scope, assumptions, lowering_cx)
}

enum AssocConstSelection<'db> {
    Found(TraitInstId<'db>),
    Ambiguous(ThinVec<TraitInstId<'db>>),
    NotFound,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FindAssociatedTypeError {
    InfiniteBoundRecursion,
}

/// Maps `(impl target base type, const name)` to the inherent impls of this
/// ingot that define an associated const with that name, so const path
/// resolution doesn't have to scan every impl.
#[salsa::tracked]
pub(crate) fn ingot_impl_const_map<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
) -> FxHashMap<(TyBase<'db>, IdentId<'db>), Vec<Impl<'db>>> {
    let mut map: FxHashMap<(TyBase<'db>, IdentId<'db>), Vec<Impl<'db>>> = FxHashMap::default();
    for &impl_ in ingot.all_impls(db) {
        if impl_.hir_consts(db).is_empty() {
            continue;
        }
        let Some(impl_ty) = impl_.admissible_inherent_impl_ty(db) else {
            continue;
        };
        let TyData::TyBase(base) = impl_ty.base_ty(db).data(db) else {
            continue;
        };
        for c in impl_.hir_consts(db) {
            if let Some(name) = c.name.to_opt() {
                map.entry((*base, name)).or_default().push(impl_);
            }
        }
    }
    map
}

fn reject_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
) -> Result<(), PathResError<'db>> {
    let args = path.generic_args(db);
    if !args.is_empty(db) {
        return Err(PathResError::new(
            PathResErrorKind::ArgNumMismatch {
                expected: 0,
                given: args.data(db).len(),
            },
            path,
        ));
    }
    Ok(())
}

/// Searches inherent `impl` blocks of the receiver type for an associated
/// const named `name`. Returns the first impl whose self type unifies with the
/// receiver and whose `where` constraints hold for it.
///
/// Two inherent impls that define the same const for the same type are a
/// definition-site conflict (see [`earliest_conflicting_inherent_const_impl`]),
/// exactly like conflicting inherent methods, so here we simply pick the first
/// applicable impl; a conflicting program is already rejected at its
/// definition.
fn select_inherent_const_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<Impl<'db>> {
    // Inherent impls can only be probed for concrete receiver types.
    let TyData::TyBase(receiver_base) = receiver_ty.base_ty(db).data(db) else {
        return None;
    };

    // Search the call-site ingot, its resolved dependencies, and the receiver
    // type's own ingot. Dependencies matter for primitives: `receiver_ty.ingot`
    // is `None` for `u256` etc., yet core/std may define `impl u256 { const X }`,
    // so a downstream `u256::X` must look into those dependency ingots. This
    // mirrors how the inherent-method table merges external ingots.
    let scope_ingot = scope.ingot(db);
    // `IndexSet` dedupes while preserving the deliberate search order
    // (call-site ingot, then dependencies, then the receiver's own ingot).
    let mut search_ingots: IndexSet<common::ingot::Ingot<'db>> = IndexSet::default();
    search_ingots.insert(scope_ingot);
    for (_, external) in scope_ingot.resolved_external_ingots(db) {
        search_ingots.insert(*external);
    }
    if let Some(recv_ingot) = receiver_ty.ingot(db) {
        search_ingots.insert(recv_ingot);
    }

    // Cheap name-indexed lookup first; the canonical receiver and solve
    // context are only built when there is at least one candidate.
    let candidates: Vec<(common::ingot::Ingot<'db>, &Vec<Impl<'db>>)> = search_ingots
        .into_iter()
        .filter_map(|ingot| {
            ingot_impl_const_map(db, ingot)
                .get(&(*receiver_base, name))
                .map(|impls| (ingot, impls))
        })
        .collect();
    if candidates.is_empty() {
        return None;
    }

    let canonical_receiver = Canonicalized::new(db, receiver_ty).canonical();

    for (ingot, impls) in candidates {
        let solve_cx =
            TraitSolveCx::new(db, ingot.root_mod(db).scope()).with_assumptions(assumptions);
        for &impl_ in impls {
            let Some(impl_ty) = impl_.admissible_inherent_impl_ty(db) else {
                continue;
            };

            let mut table = UnificationTable::new(db);
            let receiver = canonical_receiver.extract_identity(&mut table);
            let receiver = table.instantiate_to_term(receiver);

            // Instantiate the impl's params once so the target type and the
            // impl's `where` constraints share the same inference vars.
            let impl_params = collect_generic_params(db, impl_.into()).params(db);
            let fresh_args = table.instantiate_with_fresh_vars(impl_params.to_vec());
            let impl_ty = Binder::bind(impl_.into(), impl_ty).instantiate(db, &fresh_args);
            let impl_ty = table.instantiate_to_term(impl_ty);
            if table.unify(impl_ty, receiver).is_err() {
                continue;
            }

            // Conditional inherent impls (`impl<T> Foo<T> where T: Default`)
            // only provide the const when their constraints hold for the
            // receiver. `NeedsConfirmation` is rejected: nothing downstream
            // registers the obligation for a later recheck.
            let constraints = collect_constraints(db, impl_.into())
                .instantiate(db, &fresh_args)
                .fold_with(db, &mut table);
            let satisfied = constraints.list(db).iter().all(|&constraint| {
                matches!(
                    is_goal_satisfiable(db, solve_cx, constraint),
                    GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid
                )
            });
            if satisfied {
                return Some(impl_);
            }
        }
    }

    None
}

/// Definition-site conflict detection for inherent associated consts: finds the
/// earliest other inherent impl that defines a const named `name` for the same
/// type, so an unreferenced conflict is still diagnosed.
///
/// Conflict is purely structural (the impls' self types unify), exactly like
/// the inherent-method conflict check. Two impls of the same type defining the
/// same const collide regardless of their `where` clauses.
pub(crate) fn earliest_conflicting_inherent_const_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: Impl<'db>,
    name: IdentId<'db>,
) -> Option<Impl<'db>> {
    let self_ty = impl_.admissible_inherent_impl_ty(db)?;
    let TyData::TyBase(base) = self_ty.base_ty(db).data(db) else {
        return None;
    };
    let own_ingot = impl_.top_mod(db).ingot(db);

    // Search dependency ingots before the impl's own ingot, mirroring
    // `select_inherent_const_candidate`. This matters for primitives: core and
    // std are both allowed to `impl u256`, so a duplicate const across that
    // dependency edge must be detected. Dependencies come first so the conflict
    // is reported once, anchored at the dependency's definition.
    let mut search_ingots: IndexSet<common::ingot::Ingot<'db>> = IndexSet::default();
    for (_, ext) in own_ingot.resolved_external_ingots(db) {
        search_ingots.insert(*ext);
    }
    search_ingots.insert(own_ingot);

    // `ingot_impl_const_map` lists an impl once per matching const, so dedupe
    // while preserving the stable order used for anchoring.
    let mut ordered: IndexSet<Impl<'db>> = IndexSet::default();
    for ingot in search_ingots {
        if let Some(cands) = ingot_impl_const_map(db, ingot).get(&(*base, name)) {
            ordered.extend(cands.iter().copied());
        }
    }
    let self_idx = ordered.get_index_of(&impl_)?;

    // Only report against an *earlier* impl so each conflicting pair yields a
    // single diagnostic, anchored at the first definition.
    ordered
        .iter()
        .take(self_idx)
        .copied()
        .find(|&other| inherent_impl_self_types_unify(db, impl_, other))
}

/// Finds an inherent associated function that an inherent const named `name`
/// would shadow: path resolution resolves the const before method selection,
/// so the function becomes unreachable. Returns the function's name span for a
/// definition-site diagnostic.
pub(crate) fn shadowed_inherent_fn_for_const<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: Impl<'db>,
    name: IdentId<'db>,
) -> Option<DynLazySpan<'db>> {
    let self_ty = impl_.admissible_inherent_impl_ty(db)?;
    let ingot = impl_.top_mod(db).ingot(db);
    for cand in probe_method(
        db,
        ingot,
        MethodProbe {
            receiver: self_ty,
            assumptions: collect_constraints(db, impl_.into()).instantiate_identity(),
        },
        impl_.scope(),
        name,
    ) {
        let CallableDef::Func(func) = cand.def else {
            continue;
        };
        let Some(ItemKind::Impl(fn_impl)) = func.scope().parent_item(db) else {
            continue;
        };
        if fn_impl == impl_ || inherent_impl_self_types_unify(db, impl_, fn_impl) {
            return Some(cand.def.name_span());
        }
    }
    None
}

/// `true` if the two inherent impls' self types unify (with each impl's params
/// instantiated as fresh inference vars). This is the structural conflict
/// notion used for inherent methods, applied to consts.
fn inherent_impl_self_types_unify<'db>(
    db: &'db dyn HirAnalysisDb,
    a: Impl<'db>,
    b: Impl<'db>,
) -> bool {
    let (Some(a_ty), Some(b_ty)) = (
        a.admissible_inherent_impl_ty(db),
        b.admissible_inherent_impl_ty(db),
    ) else {
        return false;
    };
    let mut table = UnificationTable::new(db);
    let instantiate = |table: &mut UnificationTable<'db>, impl_: Impl<'db>, ty: TyId<'db>| {
        let args = table.instantiate_with_fresh_vars(
            collect_generic_params(db, impl_.into()).params(db).to_vec(),
        );
        let self_ty = Binder::bind(impl_.into(), ty).instantiate(db, &args);
        table.instantiate_to_term(self_ty)
    };
    let a_self = instantiate(&mut table, a, a_ty);
    let b_self = instantiate(&mut table, b, b_ty);
    table.unify(a_self, b_self).is_ok()
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
    // In that case, rely on in-scope bounds (`assumptions`) to provide
    // candidates. The list is elaborated so constants inherited through
    // transitive super-trait bounds are visible.
    let receiver_is_ty_param = matches!(
        receiver_ty.base_ty(db).data(db),
        TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
    );
    if receiver_is_ty_param {
        let assumptions = assumptions.extend_all_bounds(db);
        let mut matches: IndexSet<TraitInstId<'db>> = IndexSet::default();
        let receiver = Canonicalized::new(db, receiver_ty);
        receiver.with_materialized(db, |cx| {
            let receiver = cx.query();
            for &pred in assumptions.list(db) {
                let snapshot = cx.snapshot();
                let self_ty = cx.materialize_to_term(pred.self_ty(db));

                let pred = cx.materialize(pred);
                if cx.unify::<TyId<'db>>(receiver, self_ty).is_ok()
                    && let Some(pred) = cx.try_extract::<TraitInstId<'db>>(pred)
                    && pred.def(db).const_(db, name).is_some()
                {
                    // Constants inherited through super-trait bounds are
                    // covered by the elaborated assumption list.
                    matches.insert(pred);
                }

                cx.rollback_to(snapshot);
            }

            if let TyData::AssocTy(assoc_ty) = receiver_ty.data(db) {
                let trait_ = assoc_ty.trait_.def(db);
                let assoc_name = assoc_ty.name;
                if let Some(decl) = trait_.assoc_ty(db, assoc_name) {
                    // Bounds on the associated type are interpreted in the
                    // owner trait's `Self` context, not the projected subject.
                    let owner_self = cx.materialize(assoc_ty.trait_.self_ty(db));
                    for bound in &decl.bounds {
                        if let TypeBound::Trait(trait_ref, _) = *bound
                            && let Ok(inst) = cx.lower_trait_ref(
                                receiver,
                                trait_ref,
                                scope,
                                assumptions,
                                Some(owner_self),
                            )
                            && let Some(inst) = cx.try_extract::<TraitInstId<'db>>(inst)
                        {
                            if inst.def(db).const_(db, name).is_some() {
                                matches.insert(inst);
                            }

                            for super_trait in inst.def(db).super_traits(db) {
                                let super_inst = super_trait.instantiate(db, inst.args(db));
                                if super_inst.def(db).const_(db, name).is_some() {
                                    matches.insert(super_inst);
                                }
                            }
                        }
                    }
                }
            }
        });

        return match matches.len() {
            0 => AssocConstSelection::NotFound,
            1 => AssocConstSelection::Found(*matches.iter().next().unwrap()),
            _ => AssocConstSelection::Ambiguous(matches.into_iter().collect()),
        };
    }

    let receiver = Canonicalized::new(db, receiver_ty);
    let scope_ingot = scope.ingot(db);
    let search_ingots = [
        Some(scope_ingot),
        receiver_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    let mut matches: IndexSet<TraitInstId<'db>> = IndexSet::default();
    // Candidates whose trait arguments this receiver doesn't determine.
    let mut unresolved: IndexSet<TraitInstId<'db>> = IndexSet::default();
    let mut enclosing: IndexSet<TraitInstId<'db>> = IndexSet::default();
    receiver.with_materialized(db, |cx| {
        let receiver_ty = cx.query();
        for ingot in search_ingots.into_iter().flatten() {
            for candidate in impls_for_ty_with_satisfied_constraints(
                db,
                ingot,
                receiver.canonical(),
                assumptions,
            ) {
                let declared = candidate.trait_(db);
                if declared.def(db).const_(db, name).is_none() {
                    continue;
                }
                // Candidate discovery proves applicability but returns the impl's
                // binder. Recover its arguments from this receiver before the
                // selected trait instance leaves the inference context.
                let snapshot = cx.snapshot();
                let inst = cx.instantiate_with_fresh_vars(declared);
                if cx.unify::<TyId<'db>>(receiver_ty, inst.self_ty(db)).is_ok() {
                    let inst = cx.try_extract::<TraitInstId<'db>>(inst).unwrap_or_else(|| {
                        unresolved.insert(declared);
                        declared
                    });
                    matches.insert(inst);
                    if is_enclosing_impl(db, candidate, scope) {
                        enclosing.insert(inst);
                    }
                }
                cx.rollback_to(snapshot);
            }
        }
    });

    // Like method selection: a sole applicable candidate is used, but among
    // several only traits in scope and the impl `scope` is in count, so an
    // unrelated trait elsewhere (even in another ingot) can't make the path
    // ambiguous.
    let candidates = if matches.len() > 1 {
        let available = traits_available_for_selection(
            db,
            scope,
            assumptions.list(db).iter().map(|pred| pred.def(db)),
        );
        let visible: IndexSet<TraitInstId<'db>> = matches
            .iter()
            .copied()
            .filter(|inst| enclosing.contains(inst) || available.contains(&inst.def(db)))
            .collect();
        if visible.is_empty() { matches } else { visible }
    } else {
        matches
    };
    // A candidate whose trait arguments are unknown can't be selected.
    match candidates.first() {
        None => AssocConstSelection::NotFound,
        Some(&inst) if candidates.len() == 1 && !unresolved.contains(&inst) => {
            AssocConstSelection::Found(inst)
        }
        Some(_) => AssocConstSelection::Ambiguous(candidates.into_iter().collect()),
    }
}

/// Whether `implementor` is the trait impl that `scope` is in. Its items are
/// selected by paths such as `Self::SIZE` in it although its trait needn't be
/// in scope, as in `impl api::Local for u8`.
fn is_enclosing_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
    scope: ScopeId<'db>,
) -> bool {
    matches!(implementor.origin(db), ImplementorOrigin::Hir(impl_trait)
        if scope.is_transitive_child_of(db, impl_trait.scope()))
}

pub(crate) fn find_associated_type<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonicalized<'db, TyId<'db>>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<SmallVec<(TraitInstId<'db>, TyId<'db>), 4>, FindAssociatedTypeError> {
    find_associated_type_in_mode(db, scope, ty, name, assumptions, ConstBodyLowering::Eager)
}

/// The bounds implied by a trait's own `Self: Trait` predicate.
///
/// Header positions such as a trait method signature deliberately withhold the
/// self-predicate from `assumptions`, because assuming it while the trait's
/// interface is still being lowered can recurse through the in-progress
/// definition (see `header_constraints_for`). Its implied bounds are still
/// sound for naming and comparing associated types, which never discharges a
/// goal, so they are reconstructed locally where they are needed.
fn trait_self_implied_bounds<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> PredicateListId<'db> {
    PredicateListId::new(db, vec![trait_self_predicate(db, trait_)]).extend_all_bounds(db)
}

/// Evidence for comparing associated-type candidates with each other: the
/// caller's assumptions and everything they imply, plus the enclosing trait's
/// implied bounds when the receiver is that trait's `Self`.
///
/// Without the implied bounds a supertrait equality such as
/// `Left: Base<Item = u256>` is invisible while normalizing, so a binding-free
/// projection reached through a second supertrait cannot be recognized as the
/// same type, and two paths to one declaration are reported as an ambiguity.
fn assoc_ty_candidate_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> PredicateListId<'db> {
    let evidence = assumptions.extend_all_bounds(db);
    if let TyData::TyParam(param) = ty.data(db)
        && param.is_trait_self()
        && let Some(trait_) = param.owner.resolve_to::<Trait>(db)
    {
        let mut list = evidence.list(db).to_vec();
        for &bound in trait_self_implied_bounds(db, trait_).list(db) {
            if !list.contains(&bound) {
                list.push(bound);
            }
        }
        return PredicateListId::new(db, list);
    }
    evidence
}

/// What `path`'s generic arguments, a mode among them, make of `assoc`, the
/// associated type or shape it names: a shape at the mode its last segment
/// gives it alone, `D::Step<ref>`. A type takes no mode (3-0083), and a
/// shape takes `ref` or `mut` alone. `None` for a path without a mode
/// argument.
fn assoc_at_mode_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: TyId<'db>,
    path: PathId<'db>,
) -> Option<Result<TyId<'db>, PathResError<'db>>> {
    let args = path.generic_args(db).data(db);
    if !args.iter().any(|arg| matches!(arg, GenericArg::Mode(_))) {
        return None;
    }
    let is_shape = matches!(assoc.data(db), TyData::AssocTy(projection) if projection.is_shape(db));
    Some(match args.as_slice() {
        [GenericArg::Mode(TypeMode::Ref)] if is_shape => {
            Ok(assoc.with_shape_mode(db, ShapeMode::Ref))
        }
        [GenericArg::Mode(TypeMode::Mut)] if is_shape => {
            Ok(assoc.with_shape_mode(db, ShapeMode::Mut))
        }
        [_, _, ..] if is_shape => Err(PathResError::new(
            PathResErrorKind::ArgNumMismatch {
                expected: 1,
                given: args.len(),
            },
            path,
        )),
        _ => Ok(TyId::invalid(db, InvalidCause::ModeArgNotShape)),
    })
}

fn find_associated_type_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonicalized<'db, TyId<'db>>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
) -> Result<SmallVec<(TraitInstId<'db>, TyId<'db>), 4>, FindAssociatedTypeError> {
    let canonical_ty = ty.canonical();
    let original_ty = ty.original();

    // Candidate discovery must see implied bounds: `T: Sub` with
    // `trait Sub: Super { .. }` makes `Super`'s associated types reachable
    // through `T::Item`.
    let assumptions = assumptions.extend_all_bounds(db);

    // Qualified type: `<A as T>::B`. Always construct the associated type projection
    // against the qualified trait instance; bindings (if any) will be handled downstream.
    if let TyData::QualifiedTy(trait_inst) = original_ty.data(db) {
        return Ok(trait_inst
            .project_assoc_ty(db, name)
            .map_or_else(SmallVec::new, |projection| {
                smallvec![(*trait_inst, projection)]
            }));
    }

    let scope_ingot = scope.ingot(db);
    let mut candidates = SmallVec::new();
    let mut enclosing: IndexSet<TraitInstId<'db>> = IndexSet::default();

    if let TyData::TyParam(param) = original_ty.data(db) {
        // Trait self, in trait or impl trait. Associated type must be in this trait
        // or, inside a trait, in one of its supertraits.
        if param.is_trait_self() {
            if let Some(trait_) = param.owner.resolve_to::<Trait>(db) {
                let trait_inst = trait_self_predicate(db, trait_);
                if trait_.assoc_ty(db, name).is_some() {
                    let assoc_ty = TyId::assoc_ty(db, trait_inst.trait_ref(db), name);
                    return Ok(smallvec![(trait_inst, assoc_ty)]);
                }

                // The trait's `Self` also satisfies the trait's declared supertraits,
                // so their associated types are reachable as `Self::Name` too. Bounds
                // on the trait's own associated types have another self type and are
                // skipped. In a header position such as a trait method signature
                // `assumptions` does not carry the enclosing trait's own `Self`
                // bounds, so these are merged with the contextual candidates
                // collected below rather than returned here: a method-level
                // `where Self: Extra` is an equally valid source and must
                // participate in ambiguity resolution.
                for &bound in trait_self_implied_bounds(db, trait_).list(db) {
                    if bound.def(db) != trait_
                        && bound.self_ty(db) == original_ty
                        && let Some(assoc_ty) = bound.project_assoc_ty(db, name)
                    {
                        candidates.push((bound, assoc_ty));
                    }
                }
            } else if let Some(impl_trait) = param.owner.resolve_to::<ImplTrait>(db)
                && let Some(trait_inst) = impl_trait.trait_inst(db)
                && let Some(assoc_ty) = trait_inst.project_assoc_ty(db, name)
            {
                return Ok(smallvec![(trait_inst, assoc_ty)]);
            }
        }
    }

    let search_ingots = [
        Some(scope_ingot),
        original_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    ty.with_materialized(db, |cx| -> Result<(), FindAssociatedTypeError> {
        let lhs_ty = cx.query();

        // Only consult explicit bounds for type-parameter receivers; concrete
        // receivers get their candidates from impl lookup to avoid spurious
        // ambiguity between bounds and implementations. The parameters in
        // these bounds are the ones in scope, so they are matched as they
        // are: freshening them would let a bound on `T` match `U` or `Self`.
        if let TyData::TyParam(_) = original_ty.data(db) {
            for &trait_inst in assumptions.list(db) {
                let snapshot = cx.snapshot();
                let pred_self_ty = cx.materialize(trait_inst.self_ty(db));

                if cx.unify::<TyId<'db>>(lhs_ty, pred_self_ty).is_ok() {
                    let trait_inst = cx.materialize(trait_inst);
                    if let Some(assoc_ty) = trait_inst.project_assoc_ty(db, name)
                        && let (Some(inst), Some(assoc_ty)) = (
                            cx.try_extract::<TraitInstId<'db>>(trait_inst),
                            cx.try_extract::<TyId<'db>>(assoc_ty),
                        )
                    {
                        candidates.push((inst, assoc_ty));
                    }
                }
                cx.rollback_to(snapshot);
            }
        }

        // Search both the call-site ingot and the receiver's ingot so local
        // traits on external types and external traits on local types are both visible.
        if !matches!(original_ty.data(db), TyData::TyParam(_)) {
            for ingot in search_ingots.into_iter().flatten() {
                for impl_ in
                    impls_for_ty_with_satisfied_constraints(db, ingot, canonical_ty, assumptions)
                {
                    let impl_ = match const_bodies {
                        ConstBodyLowering::Eager => {
                            let Some(impl_) = complete_impl_assoc_ty(db, impl_, name) else {
                                continue;
                            };
                            impl_
                        }
                        ConstBodyLowering::Deferred => {
                            let Some(impl_) = complete_candidate_impl_assoc_ty(db, impl_, name)
                            else {
                                continue;
                            };
                            impl_
                        }
                    };
                    if let Some(Some((inst, assoc_ty))) =
                        cx.with_impl_assoc_ty(impl_, lhs_ty, name, |cx, inst, assoc_ty| {
                            Some((
                                cx.try_extract::<TraitInstId<'db>>(inst)?,
                                cx.try_extract::<TyId<'db>>(assoc_ty)?,
                            ))
                        })
                    {
                        candidates.push((inst, assoc_ty));
                        if is_enclosing_impl(db, impl_, scope) {
                            enclosing.insert(inst);
                        }
                    }
                }
            }
        }

        // Projections such as `T::Assoc::Item` can be resolved either from an
        // explicit bound on `T::Assoc` or from the bounds declared on the
        // associated type itself.
        if let TyData::AssocTy(assoc_ty) = original_ty.data(db) {
            for &trait_inst in assumptions.list(db) {
                let snapshot = cx.snapshot();
                let trait_inst = cx.materialize(trait_inst);
                if cx
                    .unify::<TyId<'db>>(lhs_ty, trait_inst.self_ty(db))
                    .is_ok()
                    && let Some(assoc_ty) = trait_inst.project_assoc_ty(db, name)
                    && let (Some(inst), Some(assoc_ty)) = (
                        cx.try_extract::<TraitInstId<'db>>(trait_inst),
                        cx.try_extract::<TyId<'db>>(assoc_ty),
                    )
                {
                    candidates.push((inst, assoc_ty));
                }
                cx.rollback_to(snapshot);
            }

            let trait_ = assoc_ty.trait_.def(db);
            let assoc_name = assoc_ty.name;
            if let Some(decl) = trait_.assoc_ty(db, assoc_name) {
                // Bounds like `type Assoc: Encode<Self>` are lowered in the
                // owner trait's `Self` environment.
                let owner_self = cx.materialize(assoc_ty.trait_.self_ty(db));
                for bound in &decl.bounds {
                    let TypeBound::Trait(trait_ref, _) = *bound else {
                        continue;
                    };

                    let inst = match cx.lower_trait_ref(
                        lhs_ty,
                        trait_ref,
                        scope,
                        assumptions,
                        Some(owner_self),
                    ) {
                        Ok(inst) => inst,
                        Err(TraitRefLowerError::Cycle) => {
                            return Err(FindAssociatedTypeError::InfiniteBoundRecursion);
                        }
                        Err(TraitRefLowerError::PathResError(err))
                            if err.is_infinite_bound_recursion() =>
                        {
                            return Err(FindAssociatedTypeError::InfiniteBoundRecursion);
                        }
                        Err(_) => continue,
                    };

                    if inst.def(db).assoc_ty(db, name).is_some()
                        && let Some(inst) = cx.try_extract::<TraitInstId<'db>>(inst)
                    {
                        candidates.push((inst, TyId::assoc_ty(db, inst.trait_ref(db), name)));
                    }
                }
            }
        }

        Ok(())
    })?;

    // Supertrait and contextual bounds can reach the same projection, e.g. a
    // method that restates a bound the enclosing trait already implies. Two
    // paths to one declaration are not an ambiguity.
    let mut seen = IndexSet::new();
    candidates.retain(|candidate| seen.insert(*candidate));

    // Implementations of a concrete type: like method selection and associated
    // consts, only traits in scope and the impl `scope` is in count among
    // several candidates, so an unrelated trait elsewhere can't make the path
    // ambiguous.
    if candidates.len() > 1
        && !matches!(
            original_ty.data(db),
            TyData::TyParam(_) | TyData::AssocTy(_)
        )
    {
        let available = traits_available_for_selection(
            db,
            scope,
            assumptions.list(db).iter().map(|pred| pred.def(db)),
        );
        let visible =
            |inst: &TraitInstId<'db>| enclosing.contains(inst) || available.contains(&inst.def(db));
        if candidates.iter().any(|(inst, _)| visible(inst)) {
            candidates.retain(|(inst, _)| visible(inst));
        }
    }

    Ok(candidates)
}

pub(crate) fn resolve_name_res_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    nameres: &NameRes<'db>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    // Lowered only by the resolutions that apply them: lowering arguments that
    // are then rejected or ignored repeats the whole nested lowering whenever a
    // caller resolves the path again in another namespace.
    let args =
        || lower_generic_arg_list(db, path.generic_args(db), scope, assumptions, lowering_cx);
    let res = match nameres.kind {
        NameResKind::Prim(prim) => {
            let ty = TyId::from_hir_prim_ty(db, prim);
            PathRes::Ty(TyId::foldl(db, ty, &args()))
        }
        NameResKind::Scope(scope_id) => match scope_id {
            ScopeId::Item(item) => match item {
                ItemKind::Struct(_) | ItemKind::Enum(_) => {
                    let adt_ref = AdtRef::try_from_item(item).unwrap();
                    PathRes::Ty(ty_from_adtref(db, path, adt_ref, &args(), lowering_cx)?)
                }
                ItemKind::Contract(contract) => {
                    // Contracts have no generic parameters
                    reject_generic_args(db, path)?;
                    PathRes::Ty(TyId::contract(db, contract))
                }

                ItemKind::Mod(_) | ItemKind::TopMod(_) => {
                    reject_generic_args(db, path)?;
                    PathRes::Mod(scope_id)
                }

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
                    reject_generic_args(db, path)?;
                    // Use semantic const type.
                    let ty = const_.ty(db);
                    PathRes::Const(const_, ty)
                }

                ItemKind::TypeAlias(type_alias) => {
                    let alias = match lowering_cx.const_bodies() {
                        ConstBodyLowering::Eager => lower_type_alias(db, type_alias),
                        ConstBodyLowering::Deferred => lower_type_alias_deferred(db, type_alias),
                    };
                    let expected = alias.params(db).len();
                    let given = path.generic_args(db).len(db);
                    if given > expected {
                        return Err(PathResError::new(
                            PathResErrorKind::ArgNumMismatch { expected, given },
                            path,
                        ));
                    }
                    PathRes::TyAlias(alias.clone(), alias.instantiate(db, &args(), lowering_cx))
                }

                ItemKind::Impl(impl_) => {
                    let base = impl_.ty(db);
                    PathRes::Ty(TyId::foldl(db, base, &args()))
                }
                ItemKind::ImplTrait(impl_) => {
                    let base = impl_.ty(db);
                    PathRes::Ty(TyId::foldl(db, base, &args()))
                }

                ItemKind::Trait(t) => {
                    if path.is_self_ty(db) {
                        let params = collect_generic_params(db, t.into());
                        let ty = params.trait_self(db).unwrap();
                        let ty = TyId::foldl(db, ty, &args());
                        PathRes::Ty(ty)
                    } else {
                        // Pre-validate the paths written in the type arguments and associated
                        // type bindings of the trait path, at any depth, to surface domain
                        // errors (e.g., trait or value used where a type is expected) with
                        // precise spans in the name-resolution phase. A path that does not
                        // resolve yet, such as `Q::T` in `Option<Q::T>` before `Q: Ty` is
                        // collected, fails the trait path, so its bound waits for the others.
                        for (arg_idx, arg) in path.generic_args(db).data(db).iter().enumerate() {
                            if let Some((nth, result)) =
                                trait_arg_path_error(db, arg, scope, assumptions, lowering_cx)
                            {
                                return Err(PathResError::new(
                                    PathResErrorKind::TraitGenericArgType {
                                        arg_idx,
                                        nth,
                                        result: Box::new(result),
                                    },
                                    path,
                                ));
                            }
                        }
                        let lowered = lower_trait_ref_impl_with_minter(
                            db,
                            path,
                            scope,
                            assumptions,
                            t,
                            lowering_cx,
                        );
                        match lowered {
                            Ok(t) => PathRes::Trait(t),
                            Err(err) => return Err(PathResError::new(err.into(), path)),
                        }
                    }
                }

                ItemKind::StaticAssert(_) | ItemKind::Use(_) | ItemKind::Body(_) => unreachable!(),
            },
            ScopeId::GenericParam(parent, idx) => {
                let owner = GenericParamOwner::from_item_opt(parent).unwrap();
                let param_set = collect_generic_params(db, owner);
                let ty = param_set
                    .param_by_original_idx(db, idx as usize)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                let ty = TyId::foldl(db, ty, &args());
                PathRes::Ty(ty)
            }

            // A bare associated name inside its trait means `<Self as Trait<..>>`
            // with the trait's own parameters; the name itself takes no arguments.
            ScopeId::TraitType(t, idx) => {
                reject_generic_args(db, path)?;
                let trait_inst = trait_self_predicate(db, t);
                let assoc_ty_name = t.assoc_ty_by_index(db, idx as usize).name.unwrap();
                PathRes::Ty(TyId::assoc_ty(db, trait_inst.trait_ref(db), assoc_ty_name))
            }

            ScopeId::TraitConst(t, idx) => {
                reject_generic_args(db, path)?;
                let trait_inst = trait_self_predicate(db, t);
                let const_name = t.const_by_index(idx as usize).name(db).unwrap();
                PathRes::TraitConst(trait_inst.self_ty(db), trait_inst, const_name)
            }

            ScopeId::ImplConst(impl_, idx) => {
                let const_name = impl_.const_by_index(idx as usize).name(db).unwrap();
                let self_ty = impl_
                    .admissible_inherent_impl_ty(db)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                PathRes::InherentConst(self_ty, impl_, const_name)
            }

            // A variant named by its enum type is resolved with that type;
            // here it was imported, so its enum takes default arguments.
            ScopeId::Variant(var) => {
                reject_generic_args(db, path)?;
                PathRes::EnumVariant(ResolvedVariant {
                    ty: ty_from_adtref(db, path, var.enum_.into(), &[], lowering_cx)?,
                    variant: var,
                    path,
                })
            }
            ScopeId::FuncParam(item, idx) => {
                reject_generic_args(db, path)?;
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
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    let adt = adt_ref.as_adt(db);
    let ty = TyId::adt(db, adt);
    let completed_args = adt.param_set(db).complete_args(
        db,
        &[],
        args,
        DefaultApplication::StructuralMetadata(lowering_cx),
    );
    let applied = match completed_args {
        Ok(args) => TyId::foldl(db, ty, &args),
        Err(error) => TyId::invalid(db, error.cause),
    };
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
) -> PathResolutionResult<'db, NameRes<'db>> {
    bucket
        .pick(NameDomain::TYPE)
        .clone()
        .map_err(|err| match err {
            NameResolutionError::NotFound => PathResError::new(
                PathResErrorKind::NotFound {
                    parent: parent.clone(),
                    bucket: bucket.clone(),
                },
                path,
            ),
            err => PathResError::from_name_res_error(err, path),
        })
}
