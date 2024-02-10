//! This module implements the trait and impl trait lowering process.

use std::collections::BTreeMap;

use hir::hir_def::{
    scope_graph::ScopeId, IdentId, ImplTrait, IngotId, ItemKind, Partial, PathId, Trait, TraitRefId,
};
use rustc_hash::FxHashMap;

use super::{
    trait_def::{Implementor, TraitDef, TraitInstId, TraitMethod},
    ty_def::{FuncDef, InvalidCause, Kind, TyId},
    ty_lower::{
        collect_generic_params, lower_generic_arg_list, GenericParamOwnerId, GenericParamTypeSet,
    },
    unify::UnificationTable,
};
use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    ty::{
        ty_def::TyData,
        ty_lower::{lower_func, lower_hir_ty},
    },
    HirAnalysisDb,
};

type TraitImplTable = FxHashMap<TraitDef, Vec<Implementor>>;

#[salsa::tracked]
pub(crate) fn lower_trait(db: &dyn HirAnalysisDb, trait_: Trait) -> TraitDef {
    TraitBuilder::new(db, trait_).build()
}

/// Collect all trait implementors in the ingot.
/// The returned table doesn't contain the const(external) ingot
/// implementors. If you need to obtain the environment that contains all
/// available implementors in the ingot, please use
/// [`TraitEnv`](super::trait_def::TraitEnv).
#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitImplTable {
    let const_impls = ingot
        .external_ingots(db.as_hir_db())
        .iter()
        .map(|(_, external)| collect_trait_impls(db, *external))
        .collect();

    let impl_traits = ingot.all_impl_traits(db.as_hir_db());
    ImplementorCollector::new(db, const_impls).collect(impl_traits)
}

/// Returns the corresponding implementors for the given [`ImplTrait`].
/// If the implementor type or the trait reference is ill-formed, returns
/// `None`.
#[salsa::tracked]
pub(crate) fn lower_impl_trait(
    db: &dyn HirAnalysisDb,
    impl_trait: ImplTrait,
) -> Option<Implementor> {
    let hir_db = db.as_hir_db();
    let scope = impl_trait.scope();

    let hir_ty = impl_trait.ty(hir_db).to_opt()?;
    let ty = lower_hir_ty(db, hir_ty, scope);
    if ty.contains_invalid(db) {
        return None;
    }

    let trait_ = lower_trait_ref(
        db,
        impl_trait.trait_ref(hir_db).to_opt()?,
        impl_trait.scope(),
    )
    .ok()?;

    let impl_trait_ingot = impl_trait.top_mod(hir_db).ingot(hir_db);

    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_.def(db).ingot(db) {
        return None;
    }

    let param_owner = GenericParamOwnerId::new(db, impl_trait.into());
    let params = collect_generic_params(db, param_owner).params(db).to_vec();

    Some(Implementor::new(db, trait_, ty, params, impl_trait))
}

/// Lower a trait reference to a trait instance.
#[salsa::tracked]
pub(crate) fn lower_trait_ref(
    db: &dyn HirAnalysisDb,
    trait_ref: TraitRefId,
    scope: ScopeId,
) -> Result<TraitInstId, TraitRefLowerError> {
    let hir_db = db.as_hir_db();
    let mut args = if let Some(args) = trait_ref.generic_args(hir_db) {
        lower_generic_arg_list(db, args, scope)
    } else {
        vec![]
    };

    let Partial::Present(path) = trait_ref.path(hir_db) else {
        return Err(TraitRefLowerError::Other);
    };

    let trait_def = match resolve_path_early(db, path, scope) {
        EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
            Ok(res) => {
                let NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) = res.kind else {
                    return Err(TraitRefLowerError::Other);
                };
                lower_trait(db, trait_)
            }

            Err(_) => return Err(TraitRefLowerError::Other),
        },

        EarlyResolvedPath::Partial { .. } => {
            return Err(TraitRefLowerError::AssocTy(path));
        }
    };

    if trait_def.params(db).len() != args.len() {
        return Err(TraitRefLowerError::ArgNumMismatch {
            expected: trait_def.params(db).len(),
            given: args.len(),
        });
    }

    for (param, arg) in trait_def.params(db).iter().zip(args.iter_mut()) {
        if !param.kind(db).does_match(arg.kind(db)) {
            return Err(TraitRefLowerError::ArgKindMisMatch {
                expected: param.kind(db).clone(),
                given: *arg,
            });
        }

        let expected_const_ty = match param.data(db) {
            TyData::ConstTy(expected_ty) => expected_ty.ty(db).into(),
            _ => None,
        };

        match arg.evaluate_const_ty(db, expected_const_ty) {
            Ok(ty) => *arg = ty,

            Err(InvalidCause::ConstTyMismatch { expected, given }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: Some(given),
                });
            }

            Err(InvalidCause::ConstTyExpected { expected }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: None,
                });
            }

            Err(InvalidCause::NormalTypeExpected { given }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: None,
                    given: Some(given),
                })
            }

            _ => return Err(TraitRefLowerError::Other),
        }
    }

    let ingot = scope.ingot(hir_db);
    Ok(TraitInstId::new(db, trait_def, args, ingot))
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_implementor_methods(
    db: &dyn HirAnalysisDb,
    implementor: Implementor,
) -> BTreeMap<IdentId, FuncDef> {
    let mut methods = BTreeMap::default();

    for method in implementor.hir_impl_trait(db).methods(db.as_hir_db()) {
        if let Some(func) = lower_func(db, method) {
            methods.insert(func.name(db), func);
        }
    }

    methods
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TraitRefLowerError {
    /// The trait reference contains an associated type, which is not supported
    /// yet.
    AssocTy(PathId),

    /// The number of arguments doesn't match the number of parameters.
    ArgNumMismatch { expected: usize, given: usize },

    /// The kind of the argument doesn't match the kind of the parameter of the
    /// trait.
    ArgKindMisMatch { expected: Kind, given: TyId },

    /// The argument type doesn't match the const parameter type.
    ArgTypeMismatch {
        expected: Option<TyId>,
        given: Option<TyId>,
    },

    /// Other errors, which is reported by another pass. So we don't need to
    /// report this error kind.
    Other,
}

struct TraitBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: Trait,
    param_set: GenericParamTypeSet,
    methods: BTreeMap<IdentId, TraitMethod>,
}

impl<'db> TraitBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: Trait) -> Self {
        let params_owner_id = GenericParamOwnerId::new(db, trait_.into());
        let param_set = collect_generic_params(db, params_owner_id);

        Self {
            db,
            trait_,
            param_set,
            methods: BTreeMap::default(),
        }
    }

    fn build(mut self) -> TraitDef {
        self.collect_params();
        self.collect_methods();

        TraitDef::new(self.db, self.trait_, self.param_set, self.methods)
    }

    fn collect_params(&mut self) {
        let params_owner_id = GenericParamOwnerId::new(self.db, self.trait_.into());
        self.param_set = collect_generic_params(self.db, params_owner_id);
    }

    fn collect_methods(&mut self) {
        let hir_db = self.db.as_hir_db();
        for method in self.trait_.methods(hir_db) {
            let Some(func) = lower_func(self.db, method) else {
                continue;
            };

            let name = func.name(self.db);
            let trait_method = TraitMethod(func);
            // We can simply ignore the conflict here because it's already handled by the
            // name resolution.
            self.methods.entry(name).or_insert(trait_method);
        }
    }
}

/// Collect all implementors in an ingot.
struct ImplementorCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    impl_table: TraitImplTable,
    const_impl_maps: Vec<&'db TraitImplTable>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, const_impl_maps: Vec<&'db TraitImplTable>) -> Self {
        Self {
            db,
            impl_table: TraitImplTable::default(),
            const_impl_maps,
        }
    }

    fn collect(mut self, impl_traits: &[ImplTrait]) -> TraitImplTable {
        for &impl_ in impl_traits {
            let Some(implementor) = lower_impl_trait(self.db, impl_) else {
                continue;
            };

            if !self.does_conflict(implementor) {
                self.impl_table
                    .entry(implementor.trait_def(self.db))
                    .or_default()
                    .push(implementor);
            }
        }

        self.impl_table
    }

    /// Returns `true` if `implementor` conflicts with any existing implementor.
    fn does_conflict(&mut self, implementor: Implementor) -> bool {
        let def = implementor.trait_def(self.db);
        for impl_map in self
            .const_impl_maps
            .iter()
            .chain(std::iter::once(&&self.impl_table))
        {
            let Some(impls) = impl_map.get(&def) else {
                continue;
            };
            for already_implemented in impls {
                let mut table = UnificationTable::new(self.db);
                if already_implemented.does_conflict(self.db, implementor, &mut table) {
                    return true;
                }
            }
        }

        false
    }
}
