//! This module implements the trait and impl trait lowering process.

use crate::core::hir_def::{
    AssocTypeGenericArg, HirIngot, IdentId, ImplTrait, Partial, PathId, Trait, TraitRefId,
    params::GenericArg, scope_graph::ScopeId,
};
use common::{indexmap::IndexMap, ingot::Ingot};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::ConstTyId,
    fold::{TyFoldable, TyFolder},
    func_def::FuncDef,
    trait_def::{Implementor, TraitDef, TraitInstId, does_impl_trait_conflict},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, TyId},
    ty_lower::{collect_generic_params, lower_hir_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, PathResError, resolve_path},
    ty::{
        func_def::lower_func,
        trait_resolution::constraint::collect_constraints,
        ty_def::{Kind, TyData},
        ty_lower::lower_opt_hir_ty,
    },
};

type TraitImplTable<'db> = FxHashMap<TraitDef<'db>, Vec<Binder<Implementor<'db>>>>;

#[salsa::tracked]
pub(crate) fn lower_trait<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> TraitDef<'db> {
    TraitDef::new(db, trait_)
}

/// Collect all trait implementors in the ingot.
/// The returned table doesn't contain the const(external) ingot
/// implementors. If you need to obtain the environment that contains all
/// available implementors in the ingot, please use
/// [`TraitEnv`](super::trait_def::TraitEnv).
#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitImplTable<'db> {
    let const_impls = ingot
        .resolved_external_ingots(db)
        .iter()
        .map(|(_, external)| collect_trait_impls(db, *external))
        .collect();

    let impl_traits = ingot.all_impl_traits(db);
    ImplementorCollector::new(db, const_impls).collect(impl_traits)
}

/// Returns the corresponding implementors for the given [`ImplTrait`].
/// If the implementor type or the trait reference is ill-formed, returns
/// `None`.
#[salsa::tracked]
pub(crate) fn lower_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<Implementor<'db>>> {
    let scope = impl_trait.scope();

    let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();

    let hir_ty = impl_trait.type_ref___tmp(db).to_opt()?;
    let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
    if ty.has_invalid(db) {
        return None;
    }

    let trait_ = lower_trait_ref(
        db,
        ty,
        impl_trait.trait_ref(db).to_opt()?,
        impl_trait.scope(),
        assumptions,
    )
    .ok()?;

    let impl_trait_ingot = impl_trait.top_mod(db).ingot(db);

    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_.def(db).ingot(db) {
        return None;
    }

    let params = collect_generic_params(db, impl_trait.into())
        .params(db)
        .to_vec();

    let mut types: IndexMap<_, _> = impl_trait
        .types(db)
        .iter()
        .filter_map(|t| match (t.name.to_opt(), t.type_ref___tmp().to_opt()) {
            (Some(name), Some(ty)) => Some((name, lower_hir_ty(db, ty, scope, assumptions))),
            _ => None,
        })
        .collect();

    // Merge trait associated type defaults into the implementor, but evaluated in
    // the trait's own scope and then instantiated with this impl's concrete args
    // (including Self). This ensures defaults like `type Output = Self` resolve
    // to the implementor's concrete self type rather than remaining as `Self`.
    let trait_scope = trait_.def(db).trait_(db).scope();
    for t in trait_.def(db).trait_(db).assoc_types(db) {
        let (Some(name), Some(default)) = (t.name(db), t.default_ty(db)) else {
            continue;
        };

        types.entry(name).or_insert_with(|| {
            // Lower the default in the trait scope so `Self` and trait params are visible
            let lowered = default;
            // Instantiate all trait parameters (including `Self` at idx 0) with
            // this implementor's concrete arguments so `Self` becomes the impl's
            // self type and other generics (if used) are substituted too.
            Binder::bind(lowered).instantiate(db, trait_.args(db))
        });
    }

    let implementor = Implementor::new(db, trait_, params, types, impl_trait);

    Some(Binder::bind(implementor))
}

/// Lower a trait reference to a trait instance.
#[salsa::tracked]
pub(crate) fn lower_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let Partial::Present(path) = trait_ref.path(db) else {
        return Err(TraitRefLowerError::Ignored);
    };

    match resolve_path(db, path, scope, assumptions, false) {
        Ok(PathRes::Trait(t)) => {
            let mut args = t.args(db).clone();

            // Substitute all occurrences of `Self` with `self_ty`
            // TODO: this shouldn't be necessary; Self should resolve to self_ty in a later stage,
            //  but something seems to be broken.
            struct SelfSubst<'db> {
                db: &'db dyn HirAnalysisDb,
                self_ty: TyId<'db>,
            }
            impl<'db> TyFolder<'db> for SelfSubst<'db> {
                fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                    match ty.data(self.db) {
                        TyData::TyParam(p) if p.is_trait_self() => self.self_ty,
                        _ => ty.super_fold_with(db, self),
                    }
                }
            }

            let mut folder = SelfSubst { db, self_ty };
            args[0] = self_ty;
            args.iter_mut()
                .skip(1)
                .for_each(|a| *a = a.fold_with(db, &mut folder));
            let mut assoc_bindings = t.assoc_type_bindings(db).clone();
            assoc_bindings
                .iter_mut()
                .for_each(|(_, ty)| *ty = (*ty).fold_with(db, &mut folder));

            Ok(TraitInstId::new(db, t.def(db), args, assoc_bindings))
        }
        Ok(res) => Err(TraitRefLowerError::InvalidDomain(res)),
        Err(e) => Err(TraitRefLowerError::PathResError(e)),
    }
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
    Ignored,
}

pub(crate) fn lower_trait_ref_impl<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    let trait_def = lower_trait(db, t);
    let trait_params: &[TyId<'db>] = trait_def.params(db);
    let args = path.generic_args(db).data(db);

    // Lower provided explicit args (excluding Self)
    let provided_explicit: Vec<TyId<'db>> = args
        .iter()
        .filter_map(|arg| match arg {
            GenericArg::Type(ty_arg) => Some(lower_opt_hir_ty(db, ty_arg.ty, scope, assumptions)),
            GenericArg::Const(const_arg) => {
                let const_ty_id = ConstTyId::from_opt_body(db, const_arg.body);
                Some(TyId::const_ty(db, const_ty_id))
            }
            _ => None,
        })
        .collect();

    // Fill trailing defaults using the trait's param set. Bind Self (idx 0).
    let non_self_completed = trait_def
        .param_set(db)
        .complete_explicit_args_with_defaults(
            db,
            Some(trait_def.self_param(db)),
            &provided_explicit,
            assumptions,
        );

    if non_self_completed.len() != trait_params.len() - 1 {
        return Err(TraitArgError::ArgNumMismatch {
            expected: trait_params.len() - 1,
            given: non_self_completed.len(),
        });
    }

    let mut final_args: Vec<TyId<'db>> = Vec::with_capacity(trait_params.len());
    final_args.push(trait_def.self_param(db));
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

    let assoc_bindings: IndexMap<IdentId<'db>, TyId<'db>> = args
        .iter()
        .filter_map(|arg| match arg {
            GenericArg::AssocType(AssocTypeGenericArg { name, ty }) => {
                let (Some(name), Some(ty)) = (name.to_opt(), ty.to_opt()) else {
                    return None;
                };
                Some((name, lower_hir_ty(db, ty, scope, assumptions)))
            }
            _ => None,
        })
        .collect();

    Ok(TraitInstId::new(db, trait_def, final_args, assoc_bindings))
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_implementor_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Implementor<'db>,
) -> IndexMap<IdentId<'db>, FuncDef<'db>> {
    let mut methods = IndexMap::default();
    for method in implementor.hir_impl_trait(db).methods(db) {
        if let Some(func) = lower_func(db, method) {
            methods.insert(func.name(db), func);
        }
    }

    methods
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum TraitRefLowerError<'db> {
    PathResError(PathResError<'db>),
    InvalidDomain(PathRes<'db>),
    /// Error is expected to be reported elsewhere.
    Ignored,
}

/// Collect all implementors in an ingot.
struct ImplementorCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    impl_table: TraitImplTable<'db>,
    const_impl_maps: Vec<&'db TraitImplTable<'db>>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, const_impl_maps: Vec<&'db TraitImplTable>) -> Self {
        Self {
            db,
            impl_table: TraitImplTable::default(),
            const_impl_maps,
        }
    }

    fn collect(mut self, impl_traits: &[ImplTrait<'db>]) -> TraitImplTable<'db> {
        for &impl_ in impl_traits {
            let Some(implementor) = lower_impl_trait(self.db, impl_) else {
                continue;
            };

            if !self.does_conflict(implementor) {
                self.impl_table
                    .entry(implementor.instantiate_identity().trait_def(self.db))
                    .or_default()
                    .push(implementor);
            }
        }

        self.impl_table
    }

    /// Returns `true` if `implementor` conflicts with any existing implementor.
    fn does_conflict(&mut self, implementor: Binder<Implementor>) -> bool {
        let def = implementor.instantiate_identity().trait_def(self.db);
        for impl_map in self
            .const_impl_maps
            .iter()
            .chain(std::iter::once(&&self.impl_table))
        {
            let Some(impls) = impl_map.get(&def) else {
                continue;
            };
            for already_implemented in impls {
                if does_impl_trait_conflict(self.db, *already_implemented, implementor) {
                    return true;
                }
            }
        }

        false
    }
}
