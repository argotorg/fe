use std::collections::hash_map::Entry;

use rustc_hash::FxHashMap;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    trait_def::TraitInstId,
    ty_def::{AssocTy, TyData, TyId},
};
use crate::analysis::HirAnalysisDb;

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
        let mut folder = InstantiateFolder { args };
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
                    && let ConstTyData::UnEvaluated {
                        body,
                        ty,
                        const_def,
                        generic_args,
                    } = const_ty.data(db)
                    && generic_args.is_empty()
                    && !self.args.is_empty()
                {
                    let const_ty = ConstTyId::new(
                        db,
                        ConstTyData::UnEvaluated {
                            body: *body,
                            ty: *ty,
                            const_def: *const_def,
                            generic_args: self.args.to_vec(),
                        },
                    );
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

struct InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    f: F,
    params: FxHashMap<usize, TyId<'db>>,
}

impl<'db, F> TyFolder<'db> for InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) if !param.is_effect() => {
                match self.params.entry(param.idx) {
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
                    match self.params.entry(param.idx) {
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
