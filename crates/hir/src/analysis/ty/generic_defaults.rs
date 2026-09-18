//! Declaration-owned generic defaults and their application-site instantiation.

use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::{Binder, backfill_unevaluated_const_generic_args},
    const_ty::{
        ConstBodyLowering, ConstTyData, ConstTyId, HoleAnchor, HoleId, HoleMinter, LayoutIntroSite,
        StructuralHoleId, StructuralHoleOrigin,
    },
    diagnostics::TyDiagCollection,
    fold::{TyFoldable, TyFolder},
    layout_holes::rewrite_structural_holes,
    trait_resolution::{PredicateListId, constraint::collect_candidate_constraints},
    ty_def::{InvalidCause, Kind, TyData, TyId},
    ty_error::{collect_hir_ty_diags, emit_invalid_ty_error},
    ty_lower::{GenericParamTypeSet, collect_generic_params, lower_hir_ty_with_minter},
};

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{EarlyNameQueryId, NameResKind, QueryDirective, resolve_query},
    },
    hir_def::{
        ConstGenericArgValue, GenericParam, GenericParamOwner, ItemKind, Partial, PathId,
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
    Type(Binder<TyId<'db>>),
    Const {
        value: ConstGenericArgValue<'db>,
        expected: Binder<TyId<'db>>,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct DefaultLowerError<'db> {
    #[return_ref]
    pub cause: InvalidCause<'db>,
}

/// This is a deferred semantic template, not proof that its const bodies have
/// been validated. Declaration diagnostics check bodies independently of use.
#[salsa::tracked(return_ref, cycle_initial=default_cycle_initial, cycle_fn=default_cycle_recover)]
pub(crate) fn generic_default<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>> {
    if default_dependencies(db, owner, param_idx)
        .iter()
        .any(|&idx| idx >= param_idx)
    {
        // Diagnosed from HIR without entering type/constraint lowering.
        return Err(DefaultLowerError::new(db, InvalidCause::Other));
    }
    let view = owner.param_view(db, param_idx);
    match view.param {
        GenericParam::Type(param) => {
            let Some(hir_ty) = param.default_ty else {
                return Ok(None);
            };
            let minter = HoleMinter::deferred(HoleAnchor::GenericDefault { owner, param_idx });
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
                .ok_or_else(|| DefaultLowerError::new(db, InvalidCause::TypeLoweringCycle))?;
            let ty = check_argument(db, ty, formal.kind(db), None, false)
                .map_err(|cause| DefaultLowerError::new(db, cause))?;
            Ok(Some(GenericDefault::Type(Binder::bind(ty))))
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
                .ok_or_else(|| DefaultLowerError::new(db, InvalidCause::TypeLoweringCycle))?;
            Ok(Some(GenericDefault::Const {
                value,
                expected: Binder::bind(expected),
            }))
        }
    }
}

fn default_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _owner: GenericParamOwner<'db>,
    _param_idx: usize,
) -> Result<Option<GenericDefault<'db>>, DefaultLowerError<'db>> {
    Err(DefaultLowerError::new(db, InvalidCause::TypeLoweringCycle))
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
        if default_dependencies(db, owner, view.idx)
            .iter()
            .any(|&idx| idx >= view.idx)
        {
            continue;
        }
        let span = view.span().into_type_param().default_ty();
        let mut errors = collect_hir_ty_diags(
            db,
            owner.scope(),
            hir_ty,
            span.clone(),
            default_assumptions(db, owner),
        );
        if errors.is_empty()
            && let Err(cause) = generic_default(db, owner, view.idx)
        {
            errors.extend(emit_invalid_ty_error(
                db,
                TyId::invalid(db, cause.cause(db).clone()),
                span.into(),
            ));
        }
        diags.extend(errors);
    }
    diags
}

/// Applications own fresh source identities; identity normalization does not.
#[derive(Clone, Copy)]
pub(crate) enum DefaultApplication<'a, 'db> {
    Metadata(&'a HoleMinter<'db>),
    Evaluate(&'a HoleMinter<'db>),
    Identity,
}

impl<'a, 'db> DefaultApplication<'a, 'db> {
    fn minter(self) -> Option<&'a HoleMinter<'db>> {
        match self {
            Self::Metadata(minter) | Self::Evaluate(minter) => Some(minter),
            Self::Identity => None,
        }
    }

    fn evaluates(self) -> bool {
        !matches!(self, Self::Metadata(_))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GenericArgError<'db> {
    pub index: usize,
    pub from_default: bool,
    pub cause: InvalidCause<'db>,
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
    if arg.has_invalid(db) {
        return Err(arg.invalid_cause(db).unwrap_or(InvalidCause::Other));
    }
    Ok(arg)
}

/// One owner-aware traversal. Never fold a replacement again: even recursive
/// callers may bind a parameter to another parameter of the same owner.
struct DefaultSubst<'a, 'db> {
    owner: ScopeId<'db>,
    inherited: Option<ScopeId<'db>>,
    args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for DefaultSubst<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let param = match ty.data(db) {
            TyData::TyParam(param) => Some(param),
            TyData::ConstTy(const_ty) => match const_ty.data(db) {
                ConstTyData::TyParam(param, _) => Some(param),
                _ => None,
            },
            _ => None,
        };
        if let Some(param) = param
            && (param.owner == self.owner || Some(param.owner) == self.inherited)
            && let Some(&arg) = self.args.get(param.idx)
        {
            return arg;
        }
        // Anonymous consts in type defaults also need their declaration's bound
        // prefix when there were no explicit captures during deferred lowering.
        let folded = ty.super_fold_with(db, self);
        if let TyData::ConstTy(const_ty) = folded.data(db)
            && let Some(const_ty) =
                [Some(self.owner), self.inherited]
                    .into_iter()
                    .find_map(|owner| {
                        backfill_unevaluated_const_generic_args(db, *const_ty, self.args, owner)
                    })
        {
            return TyId::const_ty(db, const_ty);
        }
        folded
    }
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
        if implicit.len() != offset {
            return Err(GenericArgError {
                index: 0,
                from_default: false,
                cause: InvalidCause::TypeLoweringCycle,
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
            });
        }
        let mut args = implicit.to_vec();
        for (index, &formal) in params.iter().skip(offset).enumerate() {
            let mut subst = DefaultSubst {
                owner: self.scope(db),
                inherited: self.scope(db).parent(db).filter(|scope| {
                    matches!(
                        scope.item(),
                        ItemKind::Impl(_) | ItemKind::ImplTrait(_) | ItemKind::Trait(_)
                    )
                }),
                args: &args,
            };
            let expected = match formal.data(db) {
                TyData::ConstTy(ty) => Some(ty.ty(db).fold_with(db, &mut subst)),
                _ => None,
            };
            let from_default = index >= provided.len();
            let result = (|| {
                let arg = if let Some(&arg) = provided.get(index) {
                    arg
                } else {
                    let Some(default) = generic_default(db, owner, index)
                        .as_ref()
                        .map_err(|error| error.cause(db).clone())?
                    else {
                        return Ok(None);
                    };
                    match default {
                        GenericDefault::Type(template) => {
                            // Freshen only template-owned roots before introducing caller values.
                            let mut roots = FxHashMap::default();
                            let fresh = rewrite_structural_holes(
                                db,
                                template.instantiate_identity(),
                                |hole, ty| {
                                    Some(TyId::const_ty(
                                        db,
                                        if let Some(minter) = application.minter() {
                                            let root = *roots
                                                .entry(hole.root(db))
                                                .or_insert_with(|| minter.mint(db));
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
                                        } else {
                                            ConstTyId::hole_with_ty(db, ty)
                                        },
                                    ))
                                },
                            );
                            fresh.fold_with(db, &mut subst)
                        }
                        GenericDefault::Const { value, expected } => {
                            let expected =
                                expected.instantiate_identity().fold_with(db, &mut subst);
                            match value {
                                ConstGenericArgValue::Expr(body) => {
                                    let captures = args.clone();
                                    let ct = if application.minter().is_some_and(|minter| {
                                        minter.const_bodies() == ConstBodyLowering::Deferred
                                    }) {
                                        ConstTyId::from_opt_body_deferred(
                                            db,
                                            *body,
                                            Some(expected),
                                            captures,
                                        )
                                    } else {
                                        ConstTyId::from_opt_body_with_ty_and_generic_args(
                                            db,
                                            *body,
                                            Some(expected),
                                            captures,
                                            !application.evaluates(),
                                        )
                                    };
                                    TyId::const_ty(db, ct)
                                }
                                ConstGenericArgValue::Hole => TyId::const_ty(
                                    db,
                                    if let Some(minter) = application.minter() {
                                        ConstTyId::structural_hole(
                                            db,
                                            expected,
                                            StructuralHoleOrigin::DefaultHoleParam {
                                                owner,
                                                param_idx: offset + index,
                                            },
                                            LayoutIntroSite::definition(owner, offset + index),
                                            minter.mint(db),
                                        )
                                    } else {
                                        ConstTyId::hole_with_ty(db, expected)
                                    },
                                ),
                            }
                        }
                    }
                };
                // Explicit eager const arguments must have the same identity
                // here as after TyId::app; later defaults capture this prefix.
                // Deferred metadata and omitted defaults stay unevaluated.
                let evaluate = application.evaluates()
                    || (!from_default && !arg.preserves_const_arg_metadata(db));
                check_argument(db, arg, formal.kind(db), expected, evaluate).map(Some)
            })()
            .map_err(|cause| GenericArgError {
                index,
                from_default,
                cause,
            })?;
            let Some(arg) = result else { break };
            args.push(arg);
        }
        Ok(args.split_off(offset))
    }
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

    use super::*;
    use crate::{
        analysis::ty::layout_holes::layout_root_id,
        test_db::{HirAnalysisTestDb, find_func},
    };

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
        let set = collect_generic_params(&db, func.into());
        let minter = HoleMinter::new(HoleAnchor::CallableOutput { func });
        let first = set
            .complete_args(&db, &[], &[], DefaultApplication::Metadata(&minter))
            .unwrap();
        let second = set
            .complete_args(&db, &[], &[], DefaultApplication::Metadata(&minter))
            .unwrap();
        let [first, second] = [first, second].map(|args| {
            args.into_iter()
                .map(|ty| layout_root_id(&db, ty.generic_args(&db)[0]).unwrap())
                .collect::<Vec<_>>()
        });
        assert_ne!(first[0], first[1]);
        assert_eq!(first[0], first[2]);
        assert_ne!(first[0], second[0]);
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
        assert!(matches!(
            length.data(&db),
            ConstTyData::UnEvaluated {
                defer_validation: true,
                ..
            }
        ));
        let set = collect_generic_params(&db, func.into());
        let minter = HoleMinter::deferred(HoleAnchor::CallableOutput { func });
        let arg = set.explicit_params(&db)[0];
        let args = set
            .complete_args(&db, &[], &[arg], DefaultApplication::Metadata(&minter))
            .unwrap();
        let TyData::ConstTy(length) = args[1].generic_args(&db)[1].data(&db) else {
            panic!("array length")
        };
        let ConstTyData::UnEvaluated {
            generic_args,
            defer_validation,
            ..
        } = length.data(&db)
        else {
            panic!("deferred length")
        };
        assert!(*defer_validation);
        assert_eq!(generic_args.as_slice(), &[arg]);
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
}
