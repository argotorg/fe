use std::collections::hash_map::Entry;

use rustc_hash::FxHashMap;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    trait_def::TraitInstId,
    ty_def::{AssocTy, TyData, TyId},
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{GenericParamOwner, ItemKind, scope_graph::ScopeId};

/// A `Binder` is a type constructor that binds a type variable within its
/// scope.
///
/// # Type Parameters
/// - `T`: The type being bound within the `Binder`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<T> {
    value: T,
}
unsafe impl<T> salsa::Update for Binder<T>
where
    T: salsa::Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe {
            let old_value = &mut *old_pointer;
            T::maybe_update(&mut old_value.value, new_value.value)
        }
    }
}

impl<T> Binder<T> {
    pub const fn bind(value: T) -> Self {
        Binder { value }
    }
}

impl<'db, T> Binder<T>
where
    T: TyFoldable<'db>,
{
    /// Instantiates the binder with an identity function.
    ///
    /// This method essentially returns the value within the binder without any
    /// modifications.
    ///
    /// # Returns
    /// The value contained within the `Binder`.
    ///
    /// # Note
    /// This function is useful when you want to retrieve the value inside the
    /// binder without applying any transformations.
    pub fn instantiate_identity(self) -> T {
        self.value
    }

    /// Retrieves a reference to the value within the binder.
    ///
    /// This function is useful when you want to access some data that you know
    /// doesn't depend on bounded variables in the binder.
    pub fn skip_binder(&self) -> &T {
        &self.value
    }

    /// Instantiates the binder with the provided arguments.
    ///
    /// This method takes a reference to a `HirAnalysisDb` and a slice of `TyId`
    /// arguments, and returns a new instance of the type contained within
    /// the binder with the arguments applied.
    ///
    /// # Parameters
    /// - `db`: A reference to the `HirAnalysisDb`.
    /// - `args`: A slice of `TyId` that will be used to instantiate the type.
    ///
    /// # Returns
    /// A new instance of the type contained within the binder with the
    /// arguments applied.
    pub fn instantiate(self, db: &'db dyn HirAnalysisDb, args: &[TyId<'db>]) -> T {
        let mut folder = InstantiateFolder {
            owner: bound_value_owner(db, &self.value),
            args,
        };
        self.value.fold_with(db, &mut folder)
    }

    /// Instantiates the binder with the provided arguments, substituting only
    /// params owned by `owner`.
    pub fn instantiate_scoped(
        self,
        db: &'db dyn HirAnalysisDb,
        owner: ScopeId<'db>,
        args: &[TyId<'db>],
    ) -> T {
        let mut folder = InstantiateScopedFolder { owner, args };
        self.value.fold_with(db, &mut folder)
    }

    /// Instantiates the binder with a custom function.
    ///
    /// This method takes a reference to a `HirAnalysisDb` and a closure that
    /// maps a bound variable to `TyId`, and returns a new instance of the
    /// type contained within the binder with the custom function applied.
    ///
    /// # Parameters
    /// - `db`: A reference to the `HirAnalysisDb`.
    /// - `f`: A function that map a bouded variable to a type.
    ///
    /// # Returns
    /// A new instance of the type contained within the binder with the custom
    /// function applied.
    pub fn instantiate_with<F>(self, db: &'db dyn HirAnalysisDb, f: F) -> T
    where
        F: FnMut(TyId<'db>) -> TyId<'db>,
    {
        let mut folder = InstantiateWithFolder {
            f,
            params: FxHashMap::default(),
        };
        self.value.fold_with(db, &mut folder)
    }
}

struct InstantiateFolder<'db, 'a> {
    owner: Option<ScopeId<'db>>,
    args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for InstantiateFolder<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) if !param.is_effect() => {
                return self.args[param.idx];
            }
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db) {
                    return self.args[param.idx];
                }

                let folded = ty.super_fold_with(db, self);
                if let TyData::ConstTy(const_ty) = folded.data(db)
                    && let Some(const_ty) = backfill_unevaluated_const_generic_args(
                        db, *const_ty, self.args, self.owner,
                    )
                {
                    return TyId::const_ty(db, const_ty);
                }
                return folded;
            }

            TyData::AssocTy(assoc_ty) => {
                // When substituting type parameters in associated types,
                // we need to fold the trait instance to substitute its generic parameters
                let trait_inst = assoc_ty.trait_;

                // Fold the self type and generic arguments of the trait instance
                let mut folded_generic_args = vec![];
                for &arg in trait_inst.args(db) {
                    folded_generic_args.push(self.fold_ty(db, arg));
                }

                // If any types changed, create a new trait instance with substituted types
                if folded_generic_args != *trait_inst.args(db) {
                    // If we couldn't resolve to a concrete type, create a new trait instance
                    let new_trait_inst = TraitInstId::new(
                        db,
                        trait_inst.def(db),
                        folded_generic_args,
                        trait_inst.assoc_type_bindings(db).clone(),
                    );

                    // Return a new associated type with the updated trait instance
                    return TyId::new(
                        db,
                        TyData::AssocTy(AssocTy {
                            trait_: new_trait_inst,
                            name: assoc_ty.name,
                        }),
                    );
                }
            }

            _ => {}
        }

        ty.super_fold_with(db, self)
    }
}

struct InstantiateScopedFolder<'db, 'a> {
    owner: ScopeId<'db>,
    args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for InstantiateScopedFolder<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) if param.owner == self.owner && !param.is_effect() => {
                return self.args[param.idx];
            }
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                    && param.owner == self.owner
                {
                    return self.args[param.idx];
                }

                let folded = ty.super_fold_with(db, self);
                if let TyData::ConstTy(const_ty) = folded.data(db)
                    && let Some(const_ty) = backfill_unevaluated_const_generic_args(
                        db,
                        *const_ty,
                        self.args,
                        Some(self.owner),
                    )
                {
                    return TyId::const_ty(db, const_ty);
                }
                return folded;
            }

            TyData::AssocTy(assoc_ty) => {
                let trait_inst = assoc_ty.trait_;

                let mut folded_generic_args = vec![];
                for &arg in trait_inst.args(db) {
                    folded_generic_args.push(self.fold_ty(db, arg));
                }

                if folded_generic_args != *trait_inst.args(db) {
                    let new_trait_inst = TraitInstId::new(
                        db,
                        trait_inst.def(db),
                        folded_generic_args,
                        trait_inst.assoc_type_bindings(db).clone(),
                    );

                    return TyId::new(
                        db,
                        TyData::AssocTy(AssocTy {
                            trait_: new_trait_inst,
                            name: assoc_ty.name,
                        }),
                    );
                }
            }

            _ => {}
        }

        ty.super_fold_with(db, self)
    }
}

fn backfill_unevaluated_const_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    args: &[TyId<'db>],
    owner: Option<ScopeId<'db>>,
) -> Option<ConstTyId<'db>> {
    let ConstTyData::UnEvaluated {
        body,
        ty,
        const_def,
        generic_args,
        preserve_unevaluated,
    } = const_ty.data(db)
    else {
        return None;
    };
    let owner = owner?;
    if !generic_args.is_empty()
        || args.is_empty()
        || unevaluated_const_owner_scope(db, *body) != Some(owner)
    {
        return None;
    }

    Some(ConstTyId::new(
        db,
        ConstTyData::UnEvaluated {
            body: *body,
            ty: *ty,
            const_def: *const_def,
            generic_args: args.to_vec(),
            preserve_unevaluated: *preserve_unevaluated,
        },
    ))
}

fn bound_value_owner<'db, T>(db: &'db dyn HirAnalysisDb, value: &T) -> Option<ScopeId<'db>>
where
    T: TyVisitable<'db>,
{
    struct OwnerCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        owner: Option<ScopeId<'db>>,
        ambiguous: bool,
    }

    impl<'db> OwnerCollector<'db> {
        fn record_owner(&mut self, owner: ScopeId<'db>) {
            match self.owner {
                Some(current) if current != owner => self.ambiguous = true,
                Some(_) => {}
                None => self.owner = Some(owner),
            }
        }
    }

    impl<'db> TyVisitor<'db> for OwnerCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.ambiguous {
                return;
            }

            match ty.data(self.db) {
                TyData::TyParam(param) if !param.is_effect() => self.record_owner(param.owner),
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(self.db) {
                        self.record_owner(param.owner);
                        return;
                    }
                    walk_ty(self, ty);
                }
                _ => walk_ty(self, ty),
            }
        }
    }

    let mut collector = OwnerCollector {
        db,
        owner: None,
        ambiguous: false,
    };
    value.visit_with(&mut collector);
    (!collector.ambiguous).then_some(collector.owner).flatten()
}

fn unevaluated_const_owner_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
) -> Option<ScopeId<'db>> {
    let mut owner = body.scope().parent_item(db)?;
    while let ItemKind::Body(parent) = owner {
        owner = parent.scope().parent_item(db)?;
    }
    GenericParamOwner::from_item_opt(owner).map(GenericParamOwner::scope)
}

struct InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    f: F,
    // Cache by full param identity (TyId), not by param.idx.
    //
    // Different generic-param owners can legally reuse the same idx; caching by idx
    // conflates distinct params and makes instantiate_with unsound for values that
    // contain params from multiple owners.
    params: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db, F> TyFolder<'db> for InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) if !param.is_effect() => {
                match self.params.entry(ty) {
                    Entry::Occupied(entry) => return *entry.get(),
                    Entry::Vacant(entry) => {
                        let ty = (self.f)(ty);
                        entry.insert(ty);
                        return ty;
                    }
                };
            }
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db) {
                    let _ = param;
                    match self.params.entry(ty) {
                        Entry::Occupied(entry) => return *entry.get(),
                        Entry::Vacant(entry) => {
                            let ty = (self.f)(ty);
                            entry.insert(ty);
                            return ty;
                        }
                    };
                }
            }

            _ => {}
        }

        ty.super_fold_with(db, self)
    }
}
