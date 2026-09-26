//! Declaration-owned generic defaults and their application-site instantiation.

use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::{
        ConstBodyLowering, ConstCanonEnv, ConstCanonMode, ConstCaptureEnv, ConstTyId, HoleAnchor,
        HoleId, LayoutIntroSite, LoweringContext, StructuralHoleId, StructuralHoleOrigin,
        UnevaluatedConstPolicy, canonicalize_ty_for_mode,
    },
    diagnostics::{TyDiagCollection, TyLowerDiag},
    layout_holes::rewrite_structural_holes,
    subst::substitute_complete,
    trait_resolution::{PredicateListId, constraint::collect_candidate_constraints},
    ty_check::check_generic_default_body_types,
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
    ty_error::{collect_hir_ty_diags_deferred, emit_invalid_ty_error, first_invalid_ty_cause},
    ty_lower::{
        CompleteSubst, GenericParamTypeSet, ParamBasis, SourceParamIndex, collect_generic_params,
        lower_hir_ty_with_minter, param_schema,
    },
    visitor::{TyVisitable, TyVisitor},
};

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{EarlyNameQueryId, NameResKind, QueryDirective, resolve_query},
    },
    hir_def::{
        ConstGenericArgValue, GenericParam, GenericParamOwner, IdentId, ItemKind, Partial, PathId,
        scope_graph::ScopeId,
    },
    semantic::trait_self_predicate,
    span::path::LazyPathSpan,
    visitor::{Visitor, VisitorCtxt, walk_path},
};

/// Unelaborated declaration predicates. No caller or effect-provider predicates
/// are allowed to influence name resolution within a default.
pub(crate) fn default_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> PredicateListId<'db> {
    let declared = collect_candidate_constraints(db, owner).instantiate_identity();
    let trait_ = match owner {
        GenericParamOwner::Trait(trait_) => Some(trait_),
        GenericParamOwner::Func(func) => match func.scope().parent_item(db) {
            Some(ItemKind::Trait(trait_)) => Some(trait_),
            _ => None,
        },
        _ => None,
    };
    trait_.map_or(declared, |trait_| {
        declared.merge(
            db,
            PredicateListId::new(db, vec![trait_self_predicate(db, trait_)]),
        )
    })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum GenericDefault<'db> {
    Type(Binder<'db, TyId<'db>>),
    Const {
        value: ConstGenericArgValue<'db>,
        expected: Binder<'db, TyId<'db>>,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct DefaultLowerError<'db> {
    #[return_ref]
    pub cause: InvalidCause<'db>,
    pub forward_ref: Option<IdentId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub(crate) struct DefaultDiagnosticOwner<'db> {
    pub(crate) owner: GenericParamOwner<'db>,
    pub(crate) param: SourceParamIndex,
}

/// `Checked` certifies declaration-owned name and type checking; deferred
/// execution remains in the template's const nodes or in types reached at
/// concrete demand. `Invalid` has a declaration diagnostic owned by the
/// `(owner, param)` query key.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum DefaultValidation<'db> {
    Checked(GenericDefault<'db>),
    Invalid(DefaultLowerError<'db>),
}

/// Structural discovery uses `generic_default`; consumers requiring a checked
/// declaration use this result. Const execution remains an application or
/// declaration-diagnostic obligation, never a prerequisite for shape discovery.
#[salsa::tracked(return_ref)]
pub(crate) fn checked_generic_default<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: SourceParamIndex,
) -> Option<DefaultValidation<'db>> {
    let template = match generic_default(db, owner, param_idx.0) {
        Ok(Some(template)) => template,
        Ok(None) => return None,
        Err(error) => return Some(DefaultValidation::Invalid(*error)),
    };
    let checks = check_generic_default_body_types(db, owner, param_idx.0);
    if checks.iter().any(|check| !check.diagnostics.is_empty()) {
        return Some(DefaultValidation::Invalid(DefaultLowerError::new(
            db,
            InvalidCause::Other,
            None,
        )));
    }
    Some(DefaultValidation::Checked(template.clone()))
}

/// This is a deferred semantic template, not proof that its const bodies have
/// been validated. Declaration diagnostics check bodies independently of use.
#[salsa::tracked(return_ref, cycle_initial=default_cycle_initial, cycle_fn=default_cycle_recover)]
pub(crate) fn generic_default<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>> {
    if has_forward_source_dependency(db, owner, param_idx) {
        // Diagnosed from HIR without entering type/constraint lowering.
        return Err(DefaultLowerError::new(db, InvalidCause::Other, None));
    }
    let view = owner.param_view(db, param_idx);
    match view.param {
        GenericParam::Type(param) => {
            let Some(hir_ty) = param.default_ty else {
                return Ok(None);
            };
            let minter = LoweringContext::deferred(HoleAnchor::GenericDefault { owner, param_idx })
                .with_default_capture(owner, SourceParamIndex(param_idx));
            let ty = lower_hir_ty_with_minter(
                db,
                hir_ty,
                owner.scope(),
                default_assumptions(db, owner),
                &minter,
            );
            let set = collect_generic_params(db, owner);
            let formal = set
                .param_by_original_idx(db, param_idx)
                .ok_or_else(|| DefaultLowerError::new(db, InvalidCause::TypeLoweringCycle, None))?;
            reject_forward_dependency(db, set, param_idx, ty)?;
            let ty = check_argument(db, ty, formal.kind(db), None, false)
                .map_err(|cause| DefaultLowerError::new(db, cause, None))?;
            Ok(Some(GenericDefault::Type(Binder::bind(owner, ty))))
        }
        GenericParam::Const(param) => {
            let Some(value) = param.default else {
                return Ok(None);
            };
            let set = collect_generic_params(db, owner);
            let expected = set
                .param_by_original_idx(db, param_idx)
                .and_then(|ty| match ty.data(db) {
                    TyData::ConstTy(ty) => Some(ty.ty(db)),
                    _ => None,
                })
                .ok_or_else(|| DefaultLowerError::new(db, InvalidCause::TypeLoweringCycle, None))?;
            reject_forward_dependency(db, set, param_idx, expected)?;
            Ok(Some(GenericDefault::Const {
                value,
                expected: Binder::bind(owner, expected),
            }))
        }
    }
}

/// Whether a default names its own or a later parameter in source.
fn has_forward_source_dependency<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> bool {
    default_dependencies(db, owner, param_idx)
        .iter()
        .any(|&idx| idx >= param_idx)
}

/// Inspect the construction recipe after member identity and aliases have
/// been resolved. Equality predicates on the declaration are not recipe data.
fn reject_forward_dependency<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
    param_idx: usize,
    ty: TyId<'db>,
) -> Result<(), DefaultLowerError<'db>> {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        owner: ScopeId<'db>,
        first_forbidden_slot: usize,
        found: Option<IdentId<'db>>,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_param(&mut self, param: &TyParam<'db>) {
            if param.owner == self.owner && param.idx >= self.first_forbidden_slot {
                self.found.get_or_insert(param.name);
            }
        }

        fn visit_const_param(&mut self, param: &TyParam<'db>, const_ty_ty: TyId<'db>) {
            self.visit_param(param);
            self.visit_ty(const_ty_ty);
        }
    }

    let mut finder = Finder {
        db,
        owner: set.scope(db),
        first_forbidden_slot: set.offset_to_explicit_params_position(db) + param_idx,
        found: None,
    };
    ty.visit_with(&mut finder);
    match finder.found {
        Some(name) => Err(DefaultLowerError::new(db, InvalidCause::Other, Some(name))),
        None => Ok(()),
    }
}

fn default_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _owner: GenericParamOwner<'db>,
    _param_idx: usize,
) -> Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>> {
    Err(DefaultLowerError::new(
        db,
        InvalidCause::TypeLoweringCycle,
        None,
    ))
}

fn default_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>>,
    _count: u32,
    _owner: GenericParamOwner<'db>,
    _param_idx: usize,
) -> salsa::CycleRecoveryAction<Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) fn type_default_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut diags = Vec::new();
    for view in owner.params(db) {
        let GenericParam::Type(param) = view.param else {
            continue;
        };
        let Some(hir_ty) = param.default_ty else {
            continue;
        };
        if has_forward_source_dependency(db, owner, view.idx) {
            continue;
        }
        let span = view.span().into_type_param().default_ty();
        let mut errors = collect_hir_ty_diags_deferred(
            db,
            owner.scope(),
            hir_ty,
            span.clone(),
            default_assumptions(db, owner),
        );
        if errors.is_empty()
            && let Err(error) = generic_default(db, owner, view.idx)
        {
            if let Some(name) = error.forward_ref(db) {
                errors.push(
                    TyLowerDiag::GenericDefaultForwardRef {
                        span: view.span(),
                        name,
                    }
                    .into(),
                );
            } else {
                errors.extend(emit_invalid_ty_error(
                    db,
                    TyId::invalid(db, error.cause(db).clone()),
                    span.into(),
                ));
            }
        }
        diags.extend(errors);
    }
    diags
}

/// Applications own fresh source identities; identity normalization does not.
#[derive(Clone, Copy)]
pub(crate) enum DefaultApplication<'a, 'db> {
    StructuralMetadata(&'a LoweringContext<'db>),
    CheckedMetadata(&'a LoweringContext<'db>),
    Evaluate(&'a LoweringContext<'db>),
    Identity,
}

impl<'a, 'db> DefaultApplication<'a, 'db> {
    fn minter(self) -> Option<&'a LoweringContext<'db>> {
        match self {
            Self::StructuralMetadata(minter)
            | Self::CheckedMetadata(minter)
            | Self::Evaluate(minter) => Some(minter),
            Self::Identity => None,
        }
    }

    fn evaluates(self) -> bool {
        matches!(self, Self::Evaluate(_) | Self::Identity)
    }

    fn checks_default(self) -> bool {
        matches!(self, Self::CheckedMetadata(_))
    }

    fn const_policy(self) -> UnevaluatedConstPolicy {
        if self
            .minter()
            .is_some_and(|minter| minter.const_bodies() == ConstBodyLowering::Deferred)
        {
            UnevaluatedConstPolicy::DeferValidation
        } else if self.evaluates() {
            UnevaluatedConstPolicy::Evaluate
        } else {
            UnevaluatedConstPolicy::Preserve
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GenericArgError<'db> {
    pub index: usize,
    pub from_default: bool,
    pub cause: InvalidCause<'db>,
    pub diagnostic_owner: Option<DefaultDiagnosticOwner<'db>>,
}

fn check_argument<'db>(
    db: &'db dyn HirAnalysisDb,
    arg: TyId<'db>,
    kind: &Kind,
    expected: Option<TyId<'db>>,
    evaluate: bool,
) -> Result<TyId<'db>, InvalidCause<'db>> {
    if let Some(cause) = arg.invalid_cause(db) {
        return Err(cause);
    }
    let arg = if evaluate {
        arg.evaluate_const_ty(db, expected)?
    } else {
        arg.check_const_ty_without_eval(db, expected)?
    };
    if !kind.does_match(arg.kind(db)) {
        return Err(InvalidCause::KindMismatch {
            expected: Some(kind.clone()),
            given: arg,
        });
    }
    if let Some(cause) = first_invalid_ty_cause(db, arg) {
        return Err(cause);
    }
    Ok(arg)
}

impl<'db> GenericParamTypeSet<'db> {
    pub(crate) fn complete_args(
        self,
        db: &'db dyn HirAnalysisDb,
        implicit: &[TyId<'db>],
        provided: &[TyId<'db>],
        application: DefaultApplication<'_, 'db>,
    ) -> Result<Vec<TyId<'db>>, GenericArgError<'db>> {
        let offset = self.offset_to_explicit_params_position(db);
        let params = self.params(db);
        let owner = GenericParamOwner::from_item_opt(self.scope(db).item())
            .expect("generic parameter owner");
        assert!(
            !application.checks_default() || self.basis(db) == ParamBasis::Full,
            "checked default completion requires the full parameter schema"
        );
        if implicit.len() != offset {
            return Err(GenericArgError {
                index: 0,
                from_default: false,
                cause: InvalidCause::TypeLoweringCycle,
                diagnostic_owner: None,
            });
        }
        if provided.len() > self.explicit_param_count(db) {
            return Err(GenericArgError {
                index: self.explicit_param_count(db),
                from_default: false,
                cause: InvalidCause::TooManyGenericArgs {
                    expected: self.explicit_param_count(db),
                    given: provided.len(),
                },
                diagnostic_owner: None,
            });
        }
        let mut args = implicit.to_vec();
        for (source_param_idx, &formal) in params.iter().skip(offset).enumerate() {
            let index = SourceParamIndex(source_param_idx);
            let domain = param_schema(db, owner, self.basis(db))
                .allowed_default_dependencies(db, index)
                .expect("default prefix must name a source parameter");
            let subst = CompleteSubst::new(domain, db, args.clone())
                .unwrap_or_else(|error| panic!("invalid default prefix for {owner:?}: {error:?}"));
            let expected = match formal.data(db) {
                TyData::ConstTy(ty) => Some(
                    substitute_complete(db, ty.ty(db), &subst).unwrap_or_else(|error| {
                        panic!("invalid const parameter type for {owner:?}: {error:?}")
                    }),
                ),
                _ => None,
            };
            let from_default = source_param_idx >= provided.len();
            let checked = (from_default && application.checks_default())
                .then(|| checked_generic_default(db, owner, index).clone())
                .flatten();
            let arg = match provided.get(source_param_idx) {
                Some(&arg) => Ok(Some(arg)),
                None => default_arg(db, owner, index, checked.as_ref(), &subst, application),
            }
            .and_then(|arg| {
                let Some(arg) = arg else {
                    return Ok(None);
                };
                // Explicit eager const arguments must have the same identity
                // here as after TyId::app; later defaults capture this prefix.
                // Deferred metadata and omitted defaults stay unevaluated.
                let evaluate = application.evaluates()
                    || (!from_default && !arg.preserves_const_arg_metadata(db));
                check_argument(db, arg, formal.kind(db), expected, evaluate).map(Some)
            })
            .map_err(|cause| GenericArgError {
                index: source_param_idx,
                from_default,
                cause,
                diagnostic_owner: matches!(checked, Some(DefaultValidation::Invalid(_))).then_some(
                    DefaultDiagnosticOwner {
                        owner,
                        param: index,
                    },
                ),
            })?;
            let Some(arg) = arg else { break };
            args.push(arg);
        }
        Ok(args.split_off(offset))
    }
}

/// Instantiates `owner`'s default for `index` under `subst`, the substitution
/// of the arguments preceding it. `checked` is the declaration's validation
/// when the application requires one.
fn default_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    index: SourceParamIndex,
    checked: Option<&DefaultValidation<'db>>,
    subst: &CompleteSubst<'db>,
    application: DefaultApplication<'_, 'db>,
) -> Result<Option<TyId<'db>>, InvalidCause<'db>> {
    let default = match checked {
        Some(DefaultValidation::Checked(template)) => Some(template),
        Some(DefaultValidation::Invalid(error)) => return Err(error.cause(db).clone()),
        None => generic_default(db, owner, index.0)
            .as_ref()
            .map_err(|error| error.cause(db).clone())?
            .as_ref(),
    };
    Ok(default.map(|default| match default {
        GenericDefault::Type(template) => {
            instantiate_type_default(db, owner, *template, subst, application)
        }
        GenericDefault::Const { value, expected } => {
            instantiate_const_default(db, owner, index, value, *expected, subst, application)
        }
    }))
}

fn instantiate_type_default<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    template: Binder<'db, TyId<'db>>,
    subst: &CompleteSubst<'db>,
    application: DefaultApplication<'_, 'db>,
) -> TyId<'db> {
    // Freshen only template-owned roots before introducing caller values.
    let mut roots = FxHashMap::default();
    let fresh = rewrite_structural_holes(db, template.instantiate_identity(), |hole, ty| {
        let hole = match application.minter() {
            Some(minter) => {
                let root = *roots
                    .entry(hole.root(db))
                    .or_insert_with(|| minter.holes().mint(db));
                ConstTyId::hole_with_id(
                    db,
                    ty,
                    HoleId::Structural(StructuralHoleId::with_intro(
                        db,
                        ty,
                        root,
                        hole.origin(db),
                        hole.introduced_at(db).clone(),
                    )),
                )
            }
            None => ConstTyId::hole_with_ty(db, ty),
        };
        Some(TyId::const_ty(db, hole))
    });
    let applied = Binder::bind(owner, fresh)
        .instantiate_subst(db, subst)
        .unwrap_or_else(|error| panic!("invalid default template for {owner:?}: {error:?}"));
    if matches!(application, DefaultApplication::Evaluate(_)) {
        canonicalize_ty_for_mode(
            db,
            applied,
            ConstCanonEnv::new(owner.scope(), PredicateListId::empty_list(db), None),
            ConstCanonMode::Identity,
        )
    } else {
        applied
    }
}

fn instantiate_const_default<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    index: SourceParamIndex,
    value: &ConstGenericArgValue<'db>,
    expected: Binder<'db, TyId<'db>>,
    subst: &CompleteSubst<'db>,
    application: DefaultApplication<'_, 'db>,
) -> TyId<'db> {
    let template_ty = expected.instantiate_identity();
    let expected = expected
        .instantiate_subst(db, subst)
        .unwrap_or_else(|error| panic!("invalid const default type for {owner:?}: {error:?}"));
    let value = match value {
        ConstGenericArgValue::Expr(body) => ConstTyId::unevaluated(
            db,
            *body,
            Some(template_ty),
            Some(expected),
            ConstCaptureEnv::bound(db, owner, Some(index), subst.values().to_vec()),
            application.const_policy(),
        ),
        ConstGenericArgValue::Hole => match application.minter() {
            Some(minter) => ConstTyId::structural_hole(
                db,
                expected,
                StructuralHoleOrigin::DefaultHoleParam {
                    owner,
                    param_idx: index.0,
                },
                LayoutIntroSite::definition(owner, index.0),
                minter.holes().mint(db),
            ),
            None => ConstTyId::hole_with_ty(db, expected),
        },
    };
    TyId::const_ty(db, value)
}

/// Parameter dependencies are a property of the source, not of successful type
/// lowering. In particular, an unresolved `T::Item` still refers to `T`.
#[salsa::tracked(return_ref)]
pub(crate) fn default_dependencies<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> Vec<usize> {
    let view = owner.param_view(db, param_idx);
    let mut collector = DefaultDependencies {
        db,
        owner,
        indices: Vec::new(),
    };
    match view.param {
        GenericParam::Type(param) => {
            if let Some(ty) = param.default_ty {
                let mut ctxt = VisitorCtxt::new(
                    db,
                    owner.scope(),
                    view.span().into_type_param().default_ty(),
                );
                collector.visit_ty(&mut ctxt, ty);
            }
        }
        GenericParam::Const(param) => {
            if let Some(ConstGenericArgValue::Expr(Partial::Present(body))) = param.default {
                collector.visit_body(&mut VisitorCtxt::with_body(db, body), body);
            }
        }
    }
    collector.indices.sort_unstable();
    collector.indices.dedup();
    collector.indices
}

struct DefaultDependencies<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    indices: Vec<usize>,
}

impl<'db> Visitor<'db> for DefaultDependencies<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(name) = path.root_ident(self.db) {
            let query = EarlyNameQueryId::new(self.db, name, ctxt.scope(), QueryDirective::new());
            for res in resolve_query(self.db, query).iter_ok() {
                if let NameResKind::Scope(ScopeId::GenericParam(item, idx)) = res.kind
                    && GenericParamOwner::from_item_opt(item) == Some(self.owner)
                {
                    self.indices.push(usize::from(idx));
                }
            }
        }
        // Includes qualified receivers and generic arguments on every segment,
        // as well as const bodies nested inside a type.
        walk_path(self, ctxt, path);
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use crate::analysis::ty::ty_lower::{ParamSchemaId, SubstError};

    use super::*;
    use crate::{
        analysis::semantic::runtime_size_bytes,
        analysis::ty::{
            const_ty::{
                BoundHoleId, CallableLayoutOwner, ConstTyData, LayoutIntroRoot, LayoutIntroStep,
            },
            ty_lower::{collect_source_generic_params, func_implicit_param_plan},
        },
        hir_def::IdentId,
        test_db::{HirAnalysisTestDb, find_func},
    };

    fn assert_default_hole_provenance<'db>(
        db: &'db HirAnalysisTestDb,
        ty: TyId<'db>,
        owner: GenericParamOwner<'db>,
        source_param_idx: usize,
        name: &str,
    ) -> StructuralHoleId<'db> {
        let TyData::ConstTy(value) = ty.data(db) else {
            panic!("expected const argument");
        };
        let ConstTyData::Hole(_, HoleId::Structural(hole)) = value.data(db) else {
            panic!("expected structural default hole");
        };
        assert_eq!(
            hole.origin(db),
            StructuralHoleOrigin::DefaultHoleParam {
                owner,
                param_idx: source_param_idx,
            }
        );
        assert_eq!(
            owner
                .param_view(db, source_param_idx)
                .name()
                .to_opt()
                .unwrap()
                .data(db),
            name
        );
        assert_eq!(
            hole.introduced_at(db),
            LayoutIntroSite {
                root: LayoutIntroRoot::Definition { owner },
                path: vec![LayoutIntroStep::ConstParam(source_param_idx as u32)],
            }
        );
        *hole
    }

    #[test]
    fn default_hole_provenance_uses_source_indices_with_implicit_prefixes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("default_hole_provenance.fe"),
            r#"
struct Record<V = u256, const ROOT: u256 = _, const OTHER: u256 = _> {}
type Alias<V = u256, const ROOT: u256 = _, const OTHER: u256 = _> = Record<V, ROOT, OTHER>
trait Cap<V = u256, const ROOT: u256 = _, const OTHER: u256 = _> {}
fn plain<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>() {}

struct Slot<const BASE: u256 = _> {}
fn layout<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>(_ slot: Slot) {}
fn provider<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>() uses (env: u256) {}
fn combined<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>(_ slot: Slot) uses (env: u256) {}

struct Container<T> {}
impl<T> Container<T> {
    fn inherited<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>() {}
    fn inherited_combined<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>(_ slot: Slot) uses (env: u256) {}
}
trait Parent<T> {
    fn trait_method<V = u256, const ROOT: u256 = _, const OTHER: u256 = _>()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        for (name, expected_prefix, provider_idx) in [
            ("Record", 0, None),
            ("Alias", 0, None),
            ("Cap", 1, None),
            ("plain", 0, None),
            ("layout", 1, None),
            ("provider", 1, Some(0)),
            ("combined", 2, Some(1)),
            ("inherited", 1, None),
            ("inherited_combined", 3, Some(2)),
            ("trait_method", 2, None),
        ] {
            let owner = top_mod
                .children_nested(&db)
                .find(|item| item.name(&db).is_some_and(|ident| ident.data(&db) == name))
                .and_then(GenericParamOwner::from_item_opt)
                .unwrap_or_else(|| panic!("missing owner {name}"));
            let set = collect_generic_params(&db, owner);
            assert_eq!(
                set.offset_to_explicit_params_position(&db),
                expected_prefix,
                "{name}"
            );
            if let GenericParamOwner::Func(func) = owner {
                assert_eq!(
                    func_implicit_param_plan(&db, func).provider_param_index_by_effect,
                    provider_idx.into_iter().map(Some).collect::<Vec<_>>(),
                    "{name}"
                );
            }
            let implicit = &set.params(&db)[..expected_prefix];
            let minter = LoweringContext::new(HoleAnchor::TemplatePath {
                path: PathId::from_ident(&db, IdentId::new(&db, name)),
                scope: owner.scope(),
                assumptions: PredicateListId::empty_list(&db),
            });
            for application in [
                DefaultApplication::StructuralMetadata(&minter),
                DefaultApplication::Evaluate(&minter),
            ] {
                let first = set.complete_args(&db, implicit, &[], application).unwrap();
                let second = set.complete_args(&db, implicit, &[], application).unwrap();
                assert_eq!(first.len(), 3);
                assert_eq!(second.len(), 3);
                assert_eq!(first[0], TyId::u256(&db));
                let mut roots = Vec::new();
                for args in [&first, &second] {
                    for (source_param_idx, param_name) in [(1, "ROOT"), (2, "OTHER")] {
                        let hole = assert_default_hole_provenance(
                            &db,
                            args[source_param_idx],
                            owner,
                            source_param_idx,
                            param_name,
                        );
                        assert!(
                            !roots.contains(&hole.root(&db)),
                            "{name}: applications and parameters need distinct roots"
                        );
                        roots.push(hole.root(&db));
                    }
                }
            }
            let identity = set
                .complete_args(&db, implicit, &[], DefaultApplication::Identity)
                .unwrap();
            assert_eq!(identity.len(), 3);
            for arg in &identity[1..] {
                let TyData::ConstTy(value) = arg.data(&db) else {
                    panic!("expected identity const argument");
                };
                assert!(matches!(
                    value.data(&db),
                    ConstTyData::Hole(_, HoleId::Bound(BoundHoleId::Opaque))
                ));
            }
        }
    }

    #[test]
    fn default_applications_freshen_owned_roots_but_preserve_substituted_roots() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("default_roots.fe"),
            r#"
struct Slot<const N: u256 = _> {}
fn defaults<T = Slot, U = Slot, V = T>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "defaults");
        let slot_owner = top_mod
            .children_non_nested(&db)
            .find(|item| item.name(&db).is_some_and(|name| name.data(&db) == "Slot"))
            .and_then(GenericParamOwner::from_item_opt)
            .unwrap();
        let set = collect_generic_params(&db, func.into());
        let minter = LoweringContext::new(HoleAnchor::CallableOutput {
            owner: CallableLayoutOwner::Func(func),
        });
        for application in [
            DefaultApplication::StructuralMetadata(&minter),
            DefaultApplication::Evaluate(&minter),
        ] {
            let first = set.complete_args(&db, &[], &[], application).unwrap();
            let second = set.complete_args(&db, &[], &[], application).unwrap();
            let [first, second] = [first, second].map(|args| {
                args.into_iter()
                    .map(|ty| {
                        assert_default_hole_provenance(
                            &db,
                            ty.generic_args(&db)[0],
                            slot_owner,
                            0,
                            "N",
                        )
                        .root(&db)
                    })
                    .collect::<Vec<_>>()
            });
            assert_ne!(first[0], first[1]);
            assert_eq!(first[0], first[2]);
            assert_eq!(second[0], second[2]);
            assert!(first.iter().all(|root| !second.contains(root)));
        }
    }

    #[test]
    fn default_templates_do_not_evaluate_nested_const_bodies() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("default_nested_const.fe"),
            r#"
fn array<const N: usize, T = [u8; { N + 1 }]>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let func = find_func(&db, top_mod, "array");
        let default = generic_default(&db, func.into(), 1)
            .as_ref()
            .unwrap()
            .as_ref()
            .unwrap();
        let GenericDefault::Type(template) = default else {
            panic!("type default")
        };
        let ty = template.instantiate_identity();
        let TyData::ConstTy(length) = ty.generic_args(&db)[1].data(&db) else {
            panic!("array length")
        };
        assert!(
            matches!(
                length.data(&db),
                ConstTyData::UnEvaluated {
                    policy: UnevaluatedConstPolicy::DeferValidation,
                    capture: ConstCaptureEnv::Identity(_),
                    ..
                }
            ),
            "actual length: {:?}",
            length.data(&db)
        );
        // The symbolic length has no concrete size until an application binds N.
        assert_eq!(runtime_size_bytes(&db, ty), Ok(None));
        let set = collect_generic_params(&db, func.into());
        let minter = LoweringContext::deferred(HoleAnchor::CallableOutput {
            owner: CallableLayoutOwner::Func(func),
        });
        let arg = set.explicit_params(&db)[0];
        let args = set
            .complete_args(
                &db,
                &[],
                &[arg],
                DefaultApplication::StructuralMetadata(&minter),
            )
            .unwrap();
        let TyData::ConstTy(length) = args[1].generic_args(&db)[1].data(&db) else {
            panic!("array length")
        };
        let ConstTyData::UnEvaluated {
            capture, policy, ..
        } = length.data(&db)
        else {
            panic!("deferred length")
        };
        assert_eq!(*policy, UnevaluatedConstPolicy::DeferValidation);
        assert_eq!(capture.complete(&db).unwrap().values(), &[arg]);
    }

    #[test]
    fn completion_reports_invalid_defaults_and_explicit_arguments() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("invalid_defaults.fe"),
            r#"
struct Wrap<T> {}
fn invalid<T = Wrap>() {}
fn valid<T = u256>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let invalid = find_func(&db, top_mod, "invalid");
        let valid = find_func(&db, top_mod, "valid");
        let error = collect_generic_params(&db, invalid.into())
            .complete_args(&db, &[], &[], DefaultApplication::Identity)
            .unwrap_err();
        assert!(error.from_default);
        assert_eq!(error.index, 0);
        assert!(matches!(error.cause, InvalidCause::KindMismatch { .. }));
        let error = collect_generic_params(&db, valid.into())
            .complete_args(
                &db,
                &[],
                &[TyId::invalid(&db, InvalidCause::ParseError)],
                DefaultApplication::Identity,
            )
            .unwrap_err();
        assert!(!error.from_default);
        assert_eq!(error.cause, InvalidCause::ParseError);
    }

    #[test]
    fn checked_default_keeps_prefix_and_validation_in_both_query_orders() {
        for checked_first in [true, false] {
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone(
                Utf8PathBuf::from("checked_default_query_order.fe"),
                "fn f<const N: usize, T = [u8; { N + 1 }]>() {}",
            );
            let (module, _) = db.top_mod(file);
            let func = find_func(&db, module, "f");
            let owner = func.into();
            let validation = if checked_first {
                let checked = checked_generic_default(&db, owner, SourceParamIndex(1)).clone();
                collect_source_generic_params(&db, owner);
                checked
            } else {
                collect_source_generic_params(&db, owner);
                checked_generic_default(&db, owner, SourceParamIndex(1)).clone()
            };
            assert!(
                matches!(
                    validation,
                    Some(DefaultValidation::Checked(GenericDefault::Type(_)))
                ),
                "expected checked symbolic default: {validation:?}"
            );
            db.assert_no_diags(module);
        }
    }

    #[test]
    fn binder_rejects_equal_length_foreign_schema() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("default_template_schema.fe"),
            "fn f<T, U = T>() {}\nfn g<X, Y = X>() {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let f = find_func(&db, module, "f");
        let g = find_func(&db, module, "g");
        let Some(GenericDefault::Type(template)) =
            generic_default(&db, f.into(), 1).as_ref().unwrap().as_ref()
        else {
            panic!("type default")
        };
        let domain = ParamSchemaId::full(&db, f.into())
            .allowed_default_dependencies(&db, SourceParamIndex(1))
            .unwrap();
        let valid = CompleteSubst::new(domain, &db, vec![TyId::bool(&db)]).unwrap();
        assert_eq!(
            template.instantiate_subst(&db, &valid).unwrap(),
            TyId::bool(&db)
        );

        let foreign_domain = ParamSchemaId::full(&db, g.into())
            .allowed_default_dependencies(&db, SourceParamIndex(1))
            .unwrap();
        let foreign = CompleteSubst::new(foreign_domain, &db, vec![TyId::bool(&db)]).unwrap();
        assert!(matches!(
            template.instantiate_subst(&db, &foreign),
            Err(SubstError::InvalidDomain(domain)) if domain == foreign_domain
        ));
    }

    #[test]
    fn invalid_default_body_has_a_declaration_diagnostic_owner() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("checked_invalid_default.fe"),
            "fn f<T = [u8; missing()]>() {}",
        );
        let (module, _) = db.top_mod(file);
        let func = find_func(&db, module, "f");
        assert!(matches!(generic_default(&db, func.into(), 0), Ok(Some(_))));
        assert!(
            matches!(
                checked_generic_default(&db, func.into(), SourceParamIndex(0)),
                Some(DefaultValidation::Invalid(_))
            ),
            "nested body was treated as checked"
        );
        assert!(!check_generic_default_body_types(&db, func.into(), 0).is_empty());
        let context = LoweringContext::new(HoleAnchor::CallableOutput {
            owner: CallableLayoutOwner::Func(func),
        });
        let error = collect_generic_params(&db, func.into())
            .complete_args(&db, &[], &[], DefaultApplication::CheckedMetadata(&context))
            .unwrap_err();
        assert_eq!(
            error.diagnostic_owner,
            Some(DefaultDiagnosticOwner {
                owner: func.into(),
                param: SourceParamIndex(0),
            })
        );
        let diags = crate::test_db::format_diagnostics(&db, &db.run_on_top_mod(module));
        assert!(
            diags.contains("missing"),
            "missing declaration diagnostic: {diags}"
        );
    }
}
