use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyFlags, TyId, TyVar},
    unify::{InferenceKey, UnificationStore, UnificationTableBase},
    visitor::{TyVisitable, collect_flags},
};
use crate::analysis::{HirAnalysisDb, ty::ty_def::collect_variables};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Canonical<T> {
    value: T,
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

    /// Materializes the canonical value into the provided table's inference
    /// universe.
    ///
    /// # Parameters
    /// - `table`: The unification table that receives fresh vars corresponding
    ///   to the canonical vars in `self`.
    ///
    /// # Returns
    /// A copy of the canonical value where each canonical var has been
    /// re-materialized as a fresh var in `table`.
    pub fn extract_identity<S>(self, table: &mut UnificationTableBase<'db, S>) -> T
    where
        S: UnificationStore<'db>,
    {
        // Re-materialize canonical vars through the current table instead of
        // assuming canonical keys are contiguous and start from zero.
        let db = table.db;
        let mut extractor = SolutionExtractor::new(table, FxHashMap::default());
        self.value.fold_with(db, &mut extractor)
    }

    pub fn flags(self, db: &'db dyn HirAnalysisDb) -> TyFlags
    where
        T: TyVisitable<'db>,
    {
        collect_flags(db, self.value)
    }

    pub(crate) fn value(self) -> T
    where
        T: Copy,
    {
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
        let query_vars = collect_variables(db, &self.value);
        // Fresh canonical vars for solution-only variables must be numbered
        // past every query var: keying them from the count of *unsolved*
        // query vars would collide with (and be conflated into) a live query
        // var whenever another query var was solved during probing.
        let next_key = query_vars.len() as u32;
        let canonical_vars = query_vars.into_iter().filter_map(|var| {
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
            next_key,
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
    original: T,
    canonical: Canonical<T>,
    // A substitution from canonical type variables to original type variables.
    subst: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db, T> Canonicalized<'db, T>
where
    T: TyFoldable<'db> + Copy,
{
    pub fn new(db: &'db dyn HirAnalysisDb, value: T) -> Self {
        let mut canonicalizer = Canonicalizer::default();
        let canonical = value.fold_with(db, &mut canonicalizer);
        let map = canonicalizer
            .subst
            .into_iter()
            .map(|(orig_var, canonical_var)| (canonical_var, orig_var))
            .collect();
        Canonicalized {
            original: value,
            canonical: Canonical { value: canonical },
            subst: map,
        }
    }

    pub fn original(&self) -> T {
        self.original
    }

    pub fn canonical(&self) -> Canonical<T> {
        self.canonical
    }

    pub fn canonicalize_solution<S, U>(
        &self,
        db: &'db dyn HirAnalysisDb,
        table: &mut UnificationTableBase<'db, S>,
        solution: U,
    ) -> Solution<U>
    where
        S: UnificationStore<'db>,
        U: TyFoldable<'db> + Clone + Update,
    {
        self.canonical.canonicalize_solution(db, table, solution)
    }

    pub(crate) fn subst(&self) -> &FxHashMap<TyId<'db>, TyId<'db>> {
        &self.subst
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
    // The key for the next canonical var this folder mints.
    next_key: u32,
}

impl<'db> Canonicalizer<'db> {
    fn canonical_var(&mut self, var: &TyVar<'db>) -> TyVar<'db> {
        let key = self.next_key;
        self.next_key += 1;
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
        if !ty.has_var(db) {
            return ty;
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

pub(super) struct SolutionExtractor<'a, 'db, S>
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
    pub(super) fn new(
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

#[cfg(test)]
mod tests {
    use super::{Canonical, Canonicalized};
    use crate::analysis::ty::{
        ty_def::{Kind, TyId, TyVarSort},
        unify::UnificationTable,
    };
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn canonical_extract_identity_handles_preseeded_tables() {
        let db = HirAnalysisTestDb::default();
        let mut table = UnificationTable::new(&db);
        let original = table.new_var(TyVarSort::General, &Kind::Star);
        let canonical = Canonical::new(&db, original);

        let mut scratch = UnificationTable::new(&db);
        let _ = scratch.new_var(TyVarSort::General, &Kind::Star);

        assert!(canonical.extract_identity(&mut scratch).is_ty_var(&db));
    }

    /// When a query variable is solved during probing, fresh solution-only
    /// variables must not reuse a live query variable's canonical key —
    /// otherwise extraction conflates the fresh variable with the query
    /// variable.
    #[test]
    fn canonicalize_solution_fresh_vars_do_not_collide_with_query_vars() {
        let db = HirAnalysisTestDb::default();
        let mut table = UnificationTable::new(&db);
        let v0 = table.new_var(TyVarSort::General, &Kind::Star);
        let v1 = table.new_var(TyVarSort::General, &Kind::Star);
        let query = TyId::tuple_with_elems(&db, &[v0, v1]);
        let canonicalized = Canonicalized::new(&db, query);

        // Probe environment, seeded the way method selection seeds it.
        let mut probe = UnificationTable::new(&db);
        let extracted = canonicalized.canonical().extract_identity(&mut probe);
        let (_, probe_elems) = extracted.decompose_ty_app(&db);
        let (p0, p1) = (probe_elems[0], probe_elems[1]);

        // Solve the first query var; the second stays live.
        probe.unify(p0, TyId::bool(&db)).unwrap();
        // The solution mentions the live query var and a fresh probe var.
        let fresh = probe.new_var(TyVarSort::General, &Kind::Star);
        let solution_value = TyId::tuple_with_elems(&db, &[p1, fresh]);
        let solution = canonicalized.canonicalize_solution(&db, &mut probe, solution_value);

        let out = canonicalized.extract_solution(&mut table, solution);
        let (_, out_elems) = out.decompose_ty_app(&db);
        assert_eq!(out_elems[0], v1, "live query var must map back to itself");
        assert!(out_elems[1].is_ty_var(&db));
        assert_ne!(
            out_elems[1], v1,
            "fresh solution var must not be conflated with a query var"
        );
    }
}
