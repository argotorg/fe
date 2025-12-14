use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId, TyVar},
    unify::{InferenceKey, UnificationStore, UnificationTableBase},
};
use crate::analysis::{HirAnalysisDb, ty::ty_def::collect_variables};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Canonical<T> {
    pub value: T,
}

impl<'db, T> Canonical<T>
where
    T: TyFoldable<'db>,
{
    pub fn new(db: &'db dyn HirAnalysisDb, value: T) -> Self {
        let mut c = Canonicalizer::default();
        let value = value.fold_with(db, &mut c);
        Canonical { value }
    }

    /// Extracts the identity from the canonical value.
    ///
    /// This method initializes the unification table with new variables
    /// based on the canonical value and then returns the canonical value
    /// itself.
    ///
    /// # Parameters
    /// - `table`: The unification table to be initialized with new variables.
    ///
    /// # Returns
    /// The canonical value after initializing the unification table.
    ///
    /// # Panics
    /// This function will panic if the `table` is not empty.
    pub fn extract_identity<S>(self, table: &mut UnificationTableBase<'db, S>) -> T
    where
        S: UnificationStore<'db>,
    {
        assert!(table.is_empty());

        for var in collect_variables(table.db, &self.value) {
            table.new_var(var.sort, &var.kind);
        }

        self.value
    }

    /// Canonicalize a new solution that corresponds to the canonical query.
    /// This function creates a new solution for a canonical query by folding
    /// the provided solution with the unification table. It then constructs
    /// a substitution map from probed type variables to canonical type
    /// variables, and uses this map to canonicalize the solution.
    ///
    /// # Parameters
    /// - `db`: The database reference.
    /// - `table`: The unification table must be from the same environment as
    ///   the solution.
    /// - `solution`: The solution to be canonicalized.
    ///
    /// # Returns
    /// A `Solution<U>` where `U` is the type of the provided solution,
    /// canonicalized to the context of the canonical query.
    pub fn canonicalize_solution<S, U>(
        &self,
        db: &'db dyn HirAnalysisDb,
        table: &mut UnificationTableBase<'db, S>,
        solution: U,
    ) -> Solution<U>
    where
        T: Copy,
        S: UnificationStore<'db>,
        U: TyFoldable<'db> + Clone + Update,
    {
        let solution = solution.fold_with(db, table);

        // Make the substitution so that it maps back from probed type variable to
        // canonical type variables.
        // `Probed type variable -> Canonical type variable`.
        let canonical_vars = collect_variables(db, &self.value)
            .into_iter()
            .filter_map(|var| {
                let ty = TyId::ty_var(db, var.sort, var.kind, var.key);
                let probed = ty.fold_with(db, table);
                if probed.is_ty_var(db) {
                    Some((probed, ty))
                } else {
                    None
                }
            });
        let mut canonicalizer = Canonicalizer {
            subst: canonical_vars.collect(),
        };

        Solution {
            value: solution.fold_with(db, &mut canonicalizer),
        }
    }
}

/// This type contains [`Canonical`] type and auxiliary information to map back
/// [`Solution`] that corresponds to [`Canonical`] query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Canonicalized<'db, T> {
    pub value: Canonical<T>,
    // A substitution from canonical type variables to original type variables.
    subst: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db, T> Canonicalized<'db, T>
where
    T: TyFoldable<'db>,
{
    pub fn new(db: &'db dyn HirAnalysisDb, value: T) -> Self {
        let mut canonicalizer = Canonicalizer::default();
        let value = value.fold_with(db, &mut canonicalizer);
        let map = canonicalizer
            .subst
            .into_iter()
            .map(|(orig_var, canonical_var)| (canonical_var, orig_var))
            .collect();
        Canonicalized {
            value: Canonical { value },
            subst: map,
        }
    }

    /// Extracts the solution from the canonicalized query.
    ///
    /// This method takes a unification table and a solution, and returns the
    /// solution in the context of the original query environment.
    ///
    /// # Parameters
    /// - `table`: The unification table in the original query environement.
    /// - `solution`: The solution to extract.
    ///
    /// # Returns
    /// The extracted solution in the context of the original query environment.
    pub fn extract_solution<U, S>(
        &self,
        table: &mut UnificationTableBase<'db, S>,
        solution: Solution<U>,
    ) -> U
    where
        U: TyFoldable<'db> + Update,
        S: UnificationStore<'db>,
    {
        let map = self.subst.clone();
        let db = table.db;
        let mut extractor = SolutionExtractor::new(table, map);
        solution.value.fold_with(db, &mut extractor)
    }

    /// Substitute canonical variables back to the original variables.
    ///
    /// This is useful when you want to run unification in a canonicalized
    /// environment (e.g. to avoid mixing inference keys from different tables)
    /// but need to return a result in the original environment.
    pub fn decanonicalize<U>(&self, db: &'db dyn HirAnalysisDb, value: U) -> U
    where
        U: TyFoldable<'db>,
    {
        struct Decanonicalizer<'a, 'db> {
            subst: &'a FxHashMap<TyId<'db>, TyId<'db>>,
        }

        impl<'db> TyFolder<'db> for Decanonicalizer<'_, 'db> {
            fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                if let Some(&ty) = self.subst.get(&ty) {
                    return ty;
                }
                ty.super_fold_with(db, self)
            }
        }

        let mut folder = Decanonicalizer { subst: &self.subst };
        value.fold_with(db, &mut folder)
    }
}

/// Represents a solution to a [`Canonical`] query.
///
/// This type guarantees:
/// 1. Any type variable in the solution that is unifiable with a type variable
///    from the [`Canonical`] query will be canonicalized to that variable.
/// 2. All other type variables are canonicalized in a consistent manner with
///    the [`Canonical`] type.
///
/// To extract the internal value into the environment where the query was
/// created, use [`Canonicalized::extract_solution`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct Solution<T>
where
    T: Update,
{
    pub(super) value: T,
}

/// A struct that helps in converting types to their canonical form.
/// It maintains a mapping from original type variables to canonical variables.
#[derive(Default)]
struct Canonicalizer<'db> {
    // A substitution from original type variables to canonical variables.
    subst: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db> Canonicalizer<'db> {
    fn canonical_var(&mut self, var: &TyVar<'db>) -> TyVar<'db> {
        let key = self.subst.len() as u32;
        TyVar {
            sort: var.sort,
            kind: var.kind.clone(),
            key: InferenceKey(key, Default::default()),
        }
    }
}

impl<'db> TyFolder<'db> for Canonicalizer<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(&canonical) = self.subst.get(&ty) {
            return canonical;
        }

        match ty.data(db) {
            TyData::TyVar(var) => {
                let canonical_var = self.canonical_var(var);
                let canonical_ty = TyId::new(db, TyData::TyVar(canonical_var));

                self.subst.insert(ty, canonical_ty);
                canonical_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(db) {
                    let canonical_var = self.canonical_var(var);
                    let const_ty =
                        ConstTyId::new(db, ConstTyData::TyVar(canonical_var, *const_ty_ty));
                    let canonical_ty = TyId::const_ty(db, const_ty);

                    self.subst.insert(ty, canonical_ty);
                    canonical_ty
                } else {
                    ty.super_fold_with(db, self)
                }
            }

            _ => ty.super_fold_with(db, self),
        }
    }
}

struct SolutionExtractor<'a, 'db, S>
where
    S: UnificationStore<'db>,
{
    table: &'a mut UnificationTableBase<'db, S>,
    /// A subst from canonical type variables to the variables in the current
    /// env.
    subst: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'a, 'db, S> SolutionExtractor<'a, 'db, S>
where
    S: UnificationStore<'db>,
{
    fn new(
        table: &'a mut UnificationTableBase<'db, S>,
        subst: FxHashMap<TyId<'db>, TyId<'db>>,
    ) -> Self {
        SolutionExtractor { table, subst }
    }
}

impl<'db, S> TyFolder<'db> for SolutionExtractor<'_, 'db, S>
where
    S: UnificationStore<'db>,
{
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(&ty) = self.subst.get(&ty) {
            return ty;
        }

        match ty.data(db) {
            TyData::TyVar(var) => {
                let new_ty = self.table.new_var(var.sort, &var.kind);
                self.subst.insert(ty, new_ty);
                new_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(db) {
                    let new_key = self.table.new_key(&var.kind, var.sort);
                    let new_ty = TyId::const_ty_var(db, *const_ty_ty, new_key);
                    self.subst.insert(ty, new_ty);
                    new_ty
                } else {
                    ty.super_fold_with(db, self)
                }
            }

            _ => ty.super_fold_with(db, self),
        }
    }
}
