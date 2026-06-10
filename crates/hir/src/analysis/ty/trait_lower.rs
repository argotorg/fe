//! This module implements the trait and impl trait lowering process.

use crate::{
    core::hir_def::{
        AssocTypeGenericArg, ConstGenericArgValue, HirIngot, IdentId, ImplTrait, ItemKind, Partial,
        PathId, Trait, TraitRefId, params::GenericArg, scope_graph::ScopeId,
    },
    hir_def::Func,
};
use common::{indexmap::IndexMap, ingot::Ingot};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::{ConstTyId, InstSite, ProvenanceSite},
    fold::{TyFoldable, TyFolder},
    layout_holes::push_provenance,
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, TyId},
    ty_lower::{ConstDefaultCompletion, lower_hir_ty, lower_opt_hir_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        NameDomain, NameResKind, PathRes, PathResError, resolve_ident_to_bucket, resolve_name_res,
    },
    ty::ty_def::{Kind, TyData},
};

type TraitImplTable<'db> = FxHashMap<Trait<'db>, Vec<Binder<ImplementorId<'db>>>>;

/// Collect all local trait implementors in the ingot.
///
/// This query is intentionally a raw lowering pass:
/// - it only lowers impls defined in `ingot`
/// - it does not pull in external ingot impls
/// - it does not perform overlap/conflict checking
///
/// Keeping impl collection free of trait solving avoids a query cycle between
/// trait-environment construction and overlap checking. Visible trait
/// environments are assembled later by [`TraitEnv`](super::trait_def::TraitEnv).
#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitImplTable<'db> {
    let mut impl_table = TraitImplTable::default();
    for &impl_ in ingot.all_impl_traits(db).iter() {
        let Some(implementor) = lower_impl_trait(db, impl_) else {
            continue;
        };
        impl_table
            .entry(implementor.instantiate_identity().trait_def(db))
            .or_default()
            .push(implementor);
    }

    impl_table
}

/// Returns the corresponding implementors for the given [`ImplTrait`].
/// If the implementor type or the trait reference is ill-formed, returns
/// `None`.
pub(crate) fn lower_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<ImplementorId<'db>>> {
    // Delegate trait-ref lowering and ingot checks to the semantic helper on
    // `ImplTrait`. If lowering fails or the ingot rule is violated, this
    // returns `None`.
    let trait_inst = impl_trait.trait_inst_result(db).ok()?;

    // Semantic generic parameters for this impl-trait block.
    let params = impl_trait.impl_params(db);

    // Semantic associated-type bindings for this impl-trait block, including
    // merged trait defaults.
    let types = impl_trait.assoc_type_bindings_for_trait_inst(db, trait_inst);

    let implementor = ImplementorId::new(
        db,
        trait_inst,
        params,
        types,
        ImplementorOrigin::Hir(impl_trait),
    );

    Some(Binder::bind(implementor))
}

/// Lower a trait reference to a trait instance.
///
/// When `owner_self` is provided, it is used for substituting `Self` references in generic
/// arguments and associated type bindings, while `self_ty` is used as the implementor (args[0]).
/// This is needed for associated type bounds like `type Assoc: Encode<Self>` where `Self`
/// refers to the owner trait's Self, not the associated type.
#[salsa::tracked(
    cycle_fn=lower_trait_ref_cycle_recover,
    cycle_initial=lower_trait_ref_cycle_initial
)]
pub(crate) fn lower_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    lower_trait_ref_inner(db, self_ty, trait_ref, scope, assumptions, owner_self)
}

fn lower_trait_ref_inner<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let Partial::Present(path) = trait_ref.path(db) else {
        return Err(TraitRefLowerError::Ignored);
    };

    let self_subst = owner_self.unwrap_or(self_ty);

    let resolved = match crate::analysis::name_resolution::resolve_path(
        db,
        path,
        scope,
        assumptions,
        false,
    ) {
        Ok(res @ PathRes::Ty(_)) => {
            match resolve_shadowed_trait_ref(db, &res, path, scope, assumptions) {
                Some(trait_res) => Ok(trait_res),
                None => Ok(res),
            }
        }
        other => other,
    };

    match resolved {
        Ok(PathRes::Trait(t)) => {
            let mut args = t.args(db).clone();

            // Substitute all occurrences of `Self` with `self_subst`
            // TODO: this shouldn't be necessary; Self should resolve to self_ty in a later stage,
            //  but something seems to be broken.
            struct SelfSubst<'db> {
                db: &'db dyn HirAnalysisDb,
                self_subst: TyId<'db>,
            }
            impl<'db> TyFolder<'db> for SelfSubst<'db> {
                fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                    match ty.data(self.db) {
                        TyData::TyParam(p) if p.is_trait_self() => self.self_subst,
                        _ => ty.super_fold_with(db, self),
                    }
                }
            }

            let mut folder = SelfSubst { db, self_subst };
            args[0] = self_ty;
            args.iter_mut()
                .skip(1)
                .for_each(|a| *a = a.fold_with(db, &mut folder));

            let mut assoc_bindings = t.assoc_type_bindings(db).clone();
            assoc_bindings
                .iter_mut()
                .for_each(|(_, ty)| *ty = (*ty).fold_with(db, &mut folder));

            Ok(TraitInstId::new(db, t.key(db), args, assoc_bindings))
        }
        Ok(res) => Err(TraitRefLowerError::InvalidDomain(res)),
        Err(e) => Err(TraitRefLowerError::PathResError(e)),
    }
}

/// A trait reference can never resolve to an associated type, but an
/// associated type of an enclosing trait lexically shadows a same-named trait
/// (`trait Abi { type Encoder: Encoder }`). When a bare trait-ref ident
/// resolves to such an associated type, retry the ident lookup from outside
/// the shadowing trait's scope, while still lowering the path's generic args
/// in the original scope so they can reference the trait's generic params.
fn resolve_shadowed_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    res: &PathRes<'db>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<PathRes<'db>> {
    if path.parent(db).is_some() {
        return None;
    }
    let ident = path.ident(db).to_opt()?;
    let PathRes::Ty(ty) = res else {
        return None;
    };
    let TyData::AssocTy(assoc) = ty.data(db) else {
        return None;
    };
    if assoc.name != ident {
        return None;
    }

    // The shadow can only come from a trait that encloses the use site.
    let shadowing_trait = assoc.trait_.def(db);
    let mut item = Some(scope.item());
    let enclosing = loop {
        match item {
            Some(ItemKind::Trait(trait_)) if trait_ == shadowing_trait => break trait_,
            Some(current) => item = current.scope().parent_item(db),
            None => return None,
        }
    };

    let retry_scope = enclosing.scope().parent_item(db)?.scope();
    let bucket = resolve_ident_to_bucket(db, path, retry_scope);
    let nameres = bucket.pick(NameDomain::TYPE).as_ref().ok()?;
    if !matches!(
        nameres.kind,
        NameResKind::Scope(ScopeId::Item(ItemKind::Trait(_)))
    ) {
        return None;
    }

    resolve_name_res(db, nameres, None, path, scope, assumptions).ok()
}

fn lower_trait_ref_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _self_ty: TyId<'db>,
    _trait_ref: TraitRefId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
    _owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    Err(TraitRefLowerError::Cycle)
}

#[allow(clippy::too_many_arguments)]
fn lower_trait_ref_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<TraitInstId<'db>, TraitRefLowerError<'db>>,
    _count: u32,
    _self_ty: TyId<'db>,
    _trait_ref: TraitRefId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
    _owner_self: Option<TyId<'db>>,
) -> salsa::CycleRecoveryAction<Result<TraitInstId<'db>, TraitRefLowerError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) enum TraitArgError<'db> {
    ArgNumMismatch {
        expected: usize,
        given: usize,
    },
    ArgKindMisMatch {
        // TODO: add index, improve diag display
        expected: Kind,
        given: TyId<'db>,
    },
    ArgTypeMismatch {
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },
    ConstHoleNotAllowed {
        arg_idx: usize,
    },
    Ignored,
}

pub(crate) fn lower_trait_ref_impl<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    lower_trait_ref_impl_inner(db, path, scope, assumptions, t)
}

fn lower_trait_ref_impl_inner<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    let trait_params: &[TyId<'db>] = t.params(db);
    let args = path.generic_args(db).data(db);
    let arg_list = path.generic_args(db);

    // Lower provided explicit args (excluding Self)
    let mut provided_explicit = Vec::new();
    let mut assoc_bindings = IndexMap::new();
    for (arg_idx, arg) in args.iter().enumerate() {
        match arg {
            GenericArg::Type(ty_arg) => {
                let hole_sites = ty_arg.ty.to_opt().map(|hir_ty| {
                    [
                        ProvenanceSite::Inst(InstSite::TypeComponent {
                            ty: hir_ty,
                            slot: arg_idx,
                        }),
                        ProvenanceSite::Inst(InstSite::GenericArgList(arg_list)),
                    ]
                });
                let ty = lower_opt_hir_ty(db, ty_arg.ty, scope, assumptions);
                let ty = hole_sites.map_or(ty, |sites| push_provenance(db, ty, &sites));
                provided_explicit.push(ty);
            }
            GenericArg::Const(const_arg) => match const_arg.value {
                ConstGenericArgValue::Expr(body) => {
                    provided_explicit.push(TyId::const_ty(db, ConstTyId::from_opt_body(db, body)));
                }
                ConstGenericArgValue::Hole => {
                    return Err(TraitArgError::ConstHoleNotAllowed { arg_idx });
                }
            },
            GenericArg::AssocType(AssocTypeGenericArg { name, ty }) => {
                if let (Some(name), Some(ty)) = (name.to_opt(), ty.to_opt()) {
                    let ty = lower_hir_ty(db, ty, scope, assumptions);
                    assoc_bindings.insert(name, ty);
                }
            }
        }
    }

    // Fill trailing defaults using the trait's param set. Bind Self (idx 0).
    let non_self_completed = t.param_set(db).complete_explicit_args(
        db,
        Some(t.self_param(db)),
        &provided_explicit,
        assumptions,
        ConstDefaultCompletion::evaluate(Some(path)),
    );

    if non_self_completed.len() != trait_params.len() - 1 {
        return Err(TraitArgError::ArgNumMismatch {
            expected: trait_params.len() - 1,
            given: non_self_completed.len(),
        });
    }

    let mut final_args: Vec<TyId<'db>> = Vec::with_capacity(trait_params.len());
    final_args.push(t.self_param(db));
    final_args.extend(non_self_completed);

    for (expected_ty, actual_ty) in trait_params.iter().zip(final_args.iter_mut()).skip(1) {
        if !expected_ty.kind(db).does_match(actual_ty.kind(db)) {
            return Err(TraitArgError::ArgKindMisMatch {
                expected: expected_ty.kind(db).clone(),
                given: *actual_ty,
            });
        }

        let expected_const_ty = match expected_ty.data(db) {
            TyData::ConstTy(expected_ty) => expected_ty.ty(db).into(),
            _ => None,
        };

        match actual_ty.evaluate_const_ty(db, expected_const_ty) {
            Ok(evaluated_ty) => *actual_ty = evaluated_ty,
            Err(InvalidCause::ConstTyMismatch { expected, given }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: Some(given),
                });
            }
            Err(InvalidCause::ConstTyExpected { expected }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: None,
                });
            }
            Err(InvalidCause::NormalTypeExpected { given }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: None,
                    given: Some(given),
                });
            }
            _ => return Err(TraitArgError::Ignored),
        }
    }

    Ok(TraitInstId::new(db, t, final_args, assoc_bindings))
}

#[cfg(test)]
mod layout_hole_tests {
    use camino::Utf8PathBuf;

    use super::lower_trait_ref_impl;
    use crate::analysis::ty::{
        const_ty::{ConstTyData, HoleId},
        trait_resolution::PredicateListId,
        ty_def::TyData,
    };
    use crate::hir_def::{ItemKind, PathId};
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn omitted_trait_hole_defaults_keep_distinct_path_arg_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("omitted_trait_hole_defaults_keep_distinct_path_arg_identity.fe"),
            r#"
trait Cap<const LEFT: u256 = _, const RIGHT: u256 = _> {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Cap") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Cap` trait");
        let name = trait_.name(&db).to_opt().expect("trait must have a name");
        let path = PathId::from_ident(&db, name);
        let inst = match lower_trait_ref_impl(
            &db,
            path,
            trait_.scope(),
            PredicateListId::empty_list(&db),
            trait_,
        ) {
            Ok(inst) => inst,
            Err(_) => panic!("failed to lower trait ref"),
        };
        let args = inst.args(&db);

        assert_eq!(args.len(), 3);
        let left = args[1];
        let right = args[2];
        assert_ne!(left, right);

        let TyData::ConstTy(left) = left.data(&db) else {
            panic!("expected left arg to be a const hole");
        };
        let TyData::ConstTy(right) = right.data(&db) else {
            panic!("expected right arg to be a const hole");
        };

        assert!(matches!(
            left.data(&db),
            ConstTyData::Hole(_, HoleId::Structural(_),)
        ));
        assert!(matches!(
            right.data(&db),
            ConstTyData::Hole(_, HoleId::Structural(_),)
        ));
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_implementor_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> IndexMap<IdentId<'db>, Func<'db>> {
    let mut methods = IndexMap::default();
    let impl_trait = match implementor.origin(db) {
        super::trait_def::ImplementorOrigin::Hir(impl_trait) => impl_trait,
        super::trait_def::ImplementorOrigin::VirtualContract(_)
        | super::trait_def::ImplementorOrigin::Assumption => return methods,
    };
    let scope = impl_trait.scope();
    let graph = scope.scope_graph(db);
    for method in graph.child_items(scope).filter_map(|item| match item {
        ItemKind::Func(func) => Some(func),
        _ => None,
    }) {
        let Some(name) = method.name(db).to_opt() else {
            continue;
        };
        methods.insert(name, method);
    }

    methods
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum TraitRefLowerError<'db> {
    PathResError(PathResError<'db>),
    InvalidDomain(PathRes<'db>),
    Cycle,
    UnsafeLocalBoundBlanketImpl,
    /// Error is expected to be reported elsewhere.
    Ignored,
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::*;
    use crate::{
        analysis::ty::ty_def::InvalidCause, core::hir_def::Partial, test_db::HirAnalysisTestDb,
    };

    #[test]
    fn lower_trait_ref_cycle_initial_returns_cycle() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("cycle_initial.fe"), "");
        let (top_mod, _) = db.top_mod(file);
        let scope = ScopeId::from_item(top_mod.into());

        let result = lower_trait_ref_cycle_initial(
            &db,
            TyId::invalid(&db, InvalidCause::Other),
            TraitRefId::new(&db, Partial::Absent),
            scope,
            PredicateListId::empty_list(&db),
            None,
        );

        assert_eq!(result, Err(TraitRefLowerError::Cycle));
    }
}
