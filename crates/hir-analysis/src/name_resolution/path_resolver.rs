use common::indexmap::IndexMap;
use either::Either;
use hir::{
    hir_def::{
        Enum, EnumVariant, GenericParamOwner, IdentId, ImplTrait, ItemKind, Partial, PathId,
        PathKind, Trait, TypeBound, TypeId, VariantKind, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};
use if_chain::if_chain;
use smallvec::{SmallVec, smallvec};
use thin_vec::ThinVec;

use super::{
    EarlyNameQueryId, ExpectedPathKind, NameDomain,
    diagnostics::PathResDiag,
    is_scope_visible_from,
    method_selection::{MethodCandidate, MethodSelectionError, select_method_candidate},
    name_resolver::{NameRes, NameResBucket, NameResolutionError},
    resolve_query,
    visibility_checker::is_ty_visible_from,
};
use crate::{
    HirAnalysisDb,
    name_resolution::{NameResKind, QueryDirective},
    ty::{
        adt_def::{AdtRef, lower_adt},
        binder::Binder,
        canonical::{Canonical, Canonicalized},
        fold::TyFoldable,
        func_def::{FuncDef, HirFuncDefKind, lower_func},
        normalize::normalize_ty,
        trait_def::{TraitInstId, impls_for_ty_with_constraints},
        trait_lower::{
            TraitArgError, TraitRefLowerError, lower_trait, lower_trait_ref, lower_trait_ref_impl,
        },
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

    /// The name is found, but it can't be used in the middle of a use path.
    InvalidPathSegment(PathRes<'db>),

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
            PathResErrorKind::InvalidPathSegment(_) => "Invalid path segment".to_string(),
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
        span: DynLazySpan<'db>,
        expected: ExpectedPathKind,
    ) -> Option<PathResDiag<'db>> {
        let failed_at = self.failed_at;
        let ident = failed_at.ident(db).to_opt()?; // TODO: handle PathKind::QualifiedType

        let diag = match self.kind {
            PathResErrorKind::ParseError => return None,
            PathResErrorKind::NotFound { parent, bucket } => {
                if let Some(nr) = bucket.iter_ok().next() {
                    if path != self.failed_at {
                        PathResDiag::InvalidPathSegment(span, ident, nr.kind.name_span(db))
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

            PathResErrorKind::InvalidPathSegment(res) => {
                PathResDiag::InvalidPathSegment(span, ident, res.name_span(db))
            }

            PathResErrorKind::Conflict(spans) => PathResDiag::Conflict(ident, spans),

            PathResErrorKind::AmbiguousAssociatedType { name, candidates } => {
                PathResDiag::AmbiguousAssociatedType {
                    span,
                    name,
                    candidates,
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
                MethodSelectionError::AmbiguousTraitMethod(trait_insts) => {
                    PathResDiag::AmbiguousTrait {
                        primary: span,
                        method_name: ident,
                        trait_insts,
                    }
                }
                MethodSelectionError::InvisibleInherentMethod(func) => {
                    PathResDiag::Invisible(span, ident, func.name_span(db).into())
                }
                MethodSelectionError::InvisibleTraitMethod(traits) => {
                    PathResDiag::InvisibleAmbiguousTrait {
                        primary: span,
                        traits,
                    }
                }
                MethodSelectionError::NotFound => PathResDiag::NotFound(span, ident),
            },
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
    let query = make_query(db, path, scope);
    resolve_query(db, query)
}

/// Panics if path.ident is `Absent`
fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> EarlyNameQueryId<'db> {
    let mut directive = QueryDirective::new();

    if path.segment_index(db) != 0 {
        directive = directive.disallow_external();
        directive = directive.disallow_lex();
    }

    let name = *path.ident(db).unwrap();
    EarlyNameQueryId::new(db, name, scope, directive)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum PathRes<'db> {
    Ty(TyId<'db>),
    TyAlias(TyAlias<'db>, TyId<'db>),
    Func(TyId<'db>),
    FuncParam(ItemKind<'db>, u16),
    Trait(TraitInstId<'db>),
    EnumVariant(ResolvedVariant<'db>),
    Const(TyId<'db>),
    Mod(ScopeId<'db>),
    Method(TyId<'db>, MethodCandidate<'db>),
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
            PathRes::Const(ty) => PathRes::Const(f(ty)),
            PathRes::EnumVariant(v) => PathRes::EnumVariant(ResolvedVariant { ty: f(v.ty), ..v }),
            // TODO: map over candidate ty?
            PathRes::Method(ty, candidate) => PathRes::Method(f(ty), candidate),
            r @ (PathRes::Trait(_) | PathRes::Mod(_) | PathRes::FuncParam(..)) => r,
        }
    }

    pub fn as_scope(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty.as_scope(db),
            PathRes::TyAlias(alias, _) => Some(alias.alias.scope()),
            PathRes::Trait(trait_) => Some(trait_.def(db).trait_(db).scope()),
            PathRes::EnumVariant(variant) => Some(variant.enum_(db).scope()),
            PathRes::FuncParam(item, idx) => Some(ScopeId::FuncParam(*item, *idx)),
            PathRes::Mod(scope) => Some(*scope),
            PathRes::Method(ty, _) => ty.as_scope(db),
        }
    }

    pub fn is_visible_from(&self, db: &'db dyn HirAnalysisDb, from_scope: ScopeId<'db>) -> bool {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => {
                is_ty_visible_from(db, *ty, from_scope)
            }
            PathRes::Method(_, cand) => {
                // Method visibility depends on the method's defining scope
                // (function or trait method), not the receiver type.
                let method_scope = match cand {
                    MethodCandidate::InherentMethod(func_def) => func_def.scope(db),
                    MethodCandidate::TraitMethod(c) | MethodCandidate::NeedsConfirmation(c) => {
                        c.method.0.scope(db)
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
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty_path(*ty),
            PathRes::TyAlias(alias, _) => alias.alias.scope().pretty_path(db),
            PathRes::EnumVariant(v) => Some(format!(
                "{}::{}",
                ty_path(v.ty).unwrap_or_else(|| "<missing>".into()),
                v.variant.def(db).name.to_opt()?.data(db)
            )),
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
            PathRes::EnumVariant(_) => "enum variant",
            PathRes::Const(_) => "constant",
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
        let mut ty = TyId::func(db, self.to_funcdef(db)?);

        for &arg in self.ty.generic_args(db) {
            if ty.applicable_ty(db).is_some() {
                ty = TyId::app(db, ty, arg);
            }
        }
        Some(ty)
    }

    pub fn to_funcdef(&self, db: &'db dyn HirAnalysisDb) -> Option<FuncDef<'db>> {
        if !matches!(self.variant.kind(db), VariantKind::Tuple(_)) {
            return None;
        }

        let arg_tys = self.iter_field_types(db).collect();
        let adt = self.ty.adt_def(db).unwrap();

        let mut ret_ty = TyId::adt(db, adt);
        ret_ty = TyId::foldl(db, ret_ty, adt.param_set(db).params(db));

        Some(FuncDef::new(
            db,
            HirFuncDefKind::VariantCtor(self.variant),
            *self.variant.def(db).name.unwrap(),
            *adt.param_set(db),
            arg_tys,
            Binder::bind(ret_ty),
        ))
    }
}

pub fn resolve_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
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
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        true,
        observer,
    )
}

fn resolve_path_impl<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
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
        let trait_inst = match lower_trait_ref(db, ty, trait_, scope, assumptions) {
            Ok(inst) => inst,
            Err(err) => {
                let path = trait_.path(db).to_opt().unwrap_or(path);
                let err = match err {
                    TraitRefLowerError::PathResError(e) => e,
                    TraitRefLowerError::InvalidDomain(res) => {
                        // TODO: better error ("expected trait ref")
                        PathResError::new(PathResErrorKind::InvalidPathSegment(res), path)
                    }
                    TraitRefLowerError::Ignored => PathResError::parse_err(path),
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
            // Fast path: `<A as Trait>::Assoc` — return the projection directly.
            if let TyData::QualifiedTy(trait_inst) = ty.data(db)
                && let Some(assoc_ty) = trait_inst.assoc_ty(db, ident)
            {
                let r = PathRes::Ty(assoc_ty);
                observer(path, &r);
                return Ok(r);
            }

            // Try to resolve as an enum variant
            if let Some(enum_) = ty.as_enum(db) {
                // We need to use the concrete enum scope instead of
                // parent_scope to resolve the variants in all cases,
                // eg when parent is `Self`
                let query = make_query(db, path, enum_.scope());
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

            // Deduplicate by normalized type
            let mut dedup: IndexMap<TyId<'db>, TraitInstId<'db>> = IndexMap::new();
            for (inst, ty_candidate) in assoc_tys.iter().copied() {
                let norm = normalize_ty(db, ty_candidate, scope, assumptions);
                dedup.entry(norm).or_insert(inst);
            }

            match dedup.len() {
                0 => unreachable!(),
                1 => {
                    let (assoc_ty, _) = dedup.first().unwrap();
                    let r = PathRes::Ty(*assoc_ty);
                    observer(path, &r);
                    return Ok(r);
                }
                _ => {
                    // Build candidate list from deduped set for diagnostics
                    let candidates = dedup.into_iter().map(|(ty, inst)| (inst, ty)).collect();
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

        Some(PathRes::Func(_) | PathRes::EnumVariant(..)) => {
            return Err(PathResError::new(
                PathResErrorKind::InvalidPathSegment(parent_res.unwrap()),
                path,
            ));
        }
        Some(PathRes::FuncParam(..) | PathRes::Method(..)) => unreachable!(),
        Some(PathRes::Const(_) | PathRes::Mod(_) | PathRes::Trait(_)) | None => {}
    };

    let query = make_query(db, path, parent_scope);
    let bucket = resolve_query(db, query);

    let parent_ty = parent_res.as_ref().and_then(|res| match res {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(*ty),
        _ => None,
    });

    let res = if_chain! {
        if is_tail && resolve_tail_as_value;
        if let Ok(res) = bucket.pick(NameDomain::VALUE);
        then {
            res.clone()
        } else {
            pick_type_domain_from_bucket(parent_res, bucket, path)?
        }
    };

    let r = resolve_name_res(db, &res, parent_ty, path, scope, assumptions)?;
    observer(path, &r);
    Ok(r)
}

pub fn find_associated_type<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
) -> SmallVec<(TraitInstId<'db>, TyId<'db>), 4> {
    // Qualified type: `<A as T>::B`. B must be an associated type of trait T.
    if let TyData::QualifiedTy(trait_inst) = ty.value.data(db) {
        return if let Some(assoc_ty) = trait_inst.assoc_ty(db, name) {
            smallvec![(*trait_inst, assoc_ty)]
        } else {
            smallvec![]
        };
    }

    let ingot = scope.ingot(db);
    // Use a single unification table and snapshots to preserve outer
    // substitutions while isolating per-candidate attempts.
    let mut table = UnificationTable::new(db);
    let lhs_ty = ty.extract_identity(&mut table);

    if let TyData::TyParam(param) = ty.value.data(db) {
        // Trait self, in trait or impl trait. Associated type must be in this trait.
        if param.is_trait_self() {
            if let Some(trait_) = param.owner.resolve_to::<Trait>(db) {
                if trait_.assoc_ty(db, name).is_some() {
                    let trait_inst = TraitInstId::new(
                        db,
                        lower_trait(db, trait_),
                        vec![ty.value],
                        IndexMap::new(),
                    );
                    let assoc_ty = TyId::assoc_ty(db, trait_inst, name);
                    return smallvec![(trait_inst, assoc_ty)];
                }
            } else if let Some(impl_trait) = param.owner.resolve_to::<ImplTrait>(db)
                && let Some(trait_ref) = impl_trait.trait_ref(db).to_opt()
                && let Ok(trait_inst) =
                    lower_trait_ref(db, ty.value, trait_ref, impl_trait.scope(), assumptions)
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
                let folded = assoc_ty.fold_with(&mut table);
                candidates.push((trait_inst, folded));
            }
            table.rollback_to(snapshot);
        }
    }

    // check all impls for ty
    for impl_ in impls_for_ty_with_constraints(db, ingot, ty, assumptions) {
        let snapshot = table.snapshot();
        let impl_ = table.instantiate_with_fresh_vars(impl_);

        if table.unify(lhs_ty, impl_.self_ty(db)).is_ok()
            && let Some(ty) = impl_.assoc_ty(db, name)
        {
            let folded = ty.fold_with(&mut table);
            candidates.push((impl_.trait_(db), folded));
        }
        table.rollback_to(snapshot);
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
                let folded = assoc_ty.fold_with(&mut table);
                candidates.push((trait_inst, folded));
            }
            table.rollback_to(snapshot);
        }

        // Also check bounds defined on the associated type in the trait definition
        let trait_def = assoc_ty.trait_.def(db);
        let trait_ = trait_def.trait_(db);

        if let Some(assoc_ty_decl) = trait_.assoc_ty(db, assoc_ty.name) {
            for bound in &assoc_ty_decl.bounds {
                let TypeBound::Trait(trait_ref) = bound else {
                    todo!("assoc ty kind bounds")
                };
                let self_ty = ty_with_subst.fold_with(&mut table);

                if let Ok(inst) = lower_trait_ref(db, self_ty, *trait_ref, scope, assumptions)
                    && let Some(assoc_ty) = inst.assoc_ty(db, name)
                {
                    let folded = assoc_ty.fold_with(&mut table);
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
                ItemKind::Struct(_) | ItemKind::Contract(_) | ItemKind::Enum(_) => {
                    let adt_ref = AdtRef::try_from_item(item).unwrap();
                    PathRes::Ty(ty_from_adtref(db, path, adt_ref, args, assumptions)?)
                }

                ItemKind::TopMod(_) | ItemKind::Mod(_) => PathRes::Mod(scope_id),

                ItemKind::Func(func) => {
                    let func_def = lower_func(db, func).unwrap();
                    let ty = TyId::func(db, func_def);
                    PathRes::Func(TyId::foldl(db, ty, args))
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
                    let ty = if let Some(ty) = const_.ty(db).to_opt() {
                        lower_hir_ty(db, ty, scope, assumptions)
                    } else {
                        TyId::invalid(db, InvalidCause::ParseError)
                    };
                    PathRes::Const(ty)
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

                ItemKind::Impl(impl_) => PathRes::Ty(impl_typeid_to_ty(
                    db,
                    path,
                    impl_.ty(db),
                    scope,
                    args,
                    assumptions,
                )?),
                ItemKind::ImplTrait(impl_) => PathRes::Ty(impl_typeid_to_ty(
                    db,
                    path,
                    impl_.ty(db),
                    scope,
                    args,
                    assumptions,
                )?),

                ItemKind::Trait(t) => {
                    if path.is_self_ty(db) {
                        let params = collect_generic_params(db, t.into());
                        let ty = params.trait_self(db).unwrap();
                        let ty = TyId::foldl(db, ty, args);
                        PathRes::Ty(ty)
                    } else {
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
                let trait_def = lower_trait(db, t);
                let trait_type = &t.types(db)[idx as usize];

                let params = collect_generic_params(db, t.into());
                let self_ty = params.trait_self(db).unwrap();

                let mut trait_args = vec![self_ty];
                trait_args.extend_from_slice(args);
                let trait_inst = TraitInstId::new(db, trait_def, &trait_args, IndexMap::new());

                // Create an associated type reference
                let assoc_ty_name = trait_type.name.to_opt().unwrap();
                let assoc_ty = TyId::assoc_ty(db, trait_inst, assoc_ty_name);

                PathRes::Ty(assoc_ty)
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
            ScopeId::FuncParam(item, idx) => PathRes::FuncParam(item, idx),
            ScopeId::Field(..) => unreachable!(),
            ScopeId::Block(..) => unreachable!(),
        },
    };
    Ok(res)
}

fn impl_typeid_to_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    hir_ty: Partial<TypeId<'db>>,
    scope: ScopeId<'db>,
    args: &[TyId<'db>],
    assumptions: PredicateListId<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    if let Some(hir_ty) = hir_ty.to_opt() {
        let ty = lower_hir_ty(db, hir_ty, scope, assumptions); // root scope!
        Ok(TyId::foldl(db, ty, args))
    } else {
        Err(PathResError::parse_err(path))
    }
}

fn ty_from_adtref<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    adt_ref: AdtRef<'db>,
    args: &[TyId<'db>],
    assumptions: PredicateListId<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    let adt = lower_adt(db, adt_ref);
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
