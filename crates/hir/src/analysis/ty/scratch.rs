use std::marker::PhantomData;

use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    canonical::{Canonicalized, Solution},
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    trait_def::{ImplementorId, TraitInstId},
    trait_lower::{TraitRefLowerError, lower_trait_ref},
    trait_resolution::PredicateListId,
    ty_def::{TyData, TyId},
    unify::{Snapshot, Unifiable, UnificationResult, UnificationStore, UnificationTable},
    visitor::{TyVisitable, TyVisitor, walk_const_ty, walk_ty},
};
use crate::{
    analysis::HirAnalysisDb,
    core::hir_def::{IdentId, TraitRefId, scope_graph::ScopeId},
};

pub(crate) trait ScratchRepr<'db>: Sized {
    type Branded<'q>: Copy;
}

mod private {
    use super::ScratchRepr;

    pub(crate) trait ScratchOps<'db>: ScratchRepr<'db> {
        fn brand<'q>(raw: Self) -> Self::Branded<'q>;
        fn unbrand<'q>(value: Self::Branded<'q>) -> Self;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ScratchTy<'q, 'db> {
    raw: TyId<'db>,
    _brand: PhantomData<fn(&'q ()) -> &'q ()>,
}

impl<'q, 'db> ScratchTy<'q, 'db> {
    pub(crate) fn is_star_kind(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.raw.is_star_kind(db)
    }

    const fn new(raw: TyId<'db>) -> Self {
        Self {
            raw,
            _brand: PhantomData,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ScratchTraitInst<'q, 'db> {
    raw: TraitInstId<'db>,
    _brand: PhantomData<fn(&'q ()) -> &'q ()>,
}

impl<'q, 'db> ScratchTraitInst<'q, 'db> {
    pub(crate) fn def(self, db: &'db dyn HirAnalysisDb) -> crate::hir_def::Trait<'db> {
        self.raw.def(db)
    }

    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> ScratchTy<'q, 'db> {
        ScratchTy::new(self.raw.self_ty(db))
    }

    pub(crate) fn project_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScratchTy<'q, 'db>> {
        self.raw.project_assoc_ty(db, name).map(ScratchTy::new)
    }

    const fn new(raw: TraitInstId<'db>) -> Self {
        Self {
            raw,
            _brand: PhantomData,
        }
    }
}

#[must_use]
pub(crate) struct ScratchSnapshot<'q, 'db> {
    raw: Snapshot<ena::unify::InPlace<super::unify::InferenceKey<'db>>>,
    _brand: PhantomData<fn(&'q ()) -> &'q ()>,
}

pub(crate) struct MaterializedCx<'q, 'db, Q>
where
    Q: TyFoldable<'db> + Copy + ScratchRepr<'db>,
{
    db: &'db dyn HirAnalysisDb,
    table: UnificationTable<'db>,
    query_raw: Q,
    local_to_canonical: FxHashMap<TyId<'db>, TyId<'db>>,
    original_to_local: FxHashMap<TyId<'db>, TyId<'db>>,
    canonical_to_original: FxHashMap<TyId<'db>, TyId<'db>>,
    _brand: PhantomData<fn(&'q ()) -> &'q ()>,
}

impl<'q, 'db, Q> MaterializedCx<'q, 'db, Q>
where
    Q: TyFoldable<'db> + Copy + ScratchRepr<'db> + private::ScratchOps<'db>,
{
    pub(crate) fn query(&self) -> Q::Branded<'q> {
        Q::brand(self.query_raw)
    }

    pub(crate) fn snapshot(&mut self) -> ScratchSnapshot<'q, 'db> {
        ScratchSnapshot {
            raw: self.table.snapshot(),
            _brand: PhantomData,
        }
    }

    pub(crate) fn rollback_to(&mut self, snapshot: ScratchSnapshot<'q, 'db>) {
        self.table.rollback_to(snapshot.raw);
    }

    pub(crate) fn materialize<U>(&mut self, value: U) -> U::Branded<'q>
    where
        U: TyFoldable<'db> + ScratchRepr<'db> + private::ScratchOps<'db>,
    {
        let mut materializer =
            ScratchMaterializer::new(&mut self.table, std::mem::take(&mut self.original_to_local));
        let raw = value.fold_with(self.db, &mut materializer);
        self.original_to_local = materializer.subst;
        U::brand(raw)
    }

    pub(crate) fn resolve<U>(&mut self, value: U::Branded<'q>) -> U::Branded<'q>
    where
        U: TyFoldable<'db> + ScratchRepr<'db> + private::ScratchOps<'db>,
    {
        U::brand(U::unbrand(value).fold_with(self.db, &mut self.table))
    }

    pub(crate) fn instantiate_with_fresh_vars<U>(&mut self, value: U) -> U::Branded<'q>
    where
        U: TyFoldable<'db> + ScratchRepr<'db> + private::ScratchOps<'db>,
    {
        U::brand(self.table.instantiate_with_fresh_vars(value))
    }

    pub(crate) fn materialize_to_term(&mut self, ty: TyId<'db>) -> ScratchTy<'q, 'db> {
        let ty = self.materialize(ty);
        self.instantiate_to_term(ty)
    }

    pub(crate) fn instantiate_to_term(&mut self, ty: ScratchTy<'q, 'db>) -> ScratchTy<'q, 'db> {
        ScratchTy::new(self.table.instantiate_to_term(ty.raw))
    }

    pub(crate) fn unify<U>(&mut self, lhs: U::Branded<'q>, rhs: U::Branded<'q>) -> UnificationResult
    where
        U: Unifiable<'db> + ScratchRepr<'db> + private::ScratchOps<'db>,
    {
        self.table.unify(U::unbrand(lhs), U::unbrand(rhs))
    }

    pub(crate) fn try_extract<U>(&mut self, value: U::Branded<'q>) -> Option<U>
    where
        U: TyFoldable<'db>
            + TyVisitable<'db>
            + Update
            + ScratchRepr<'db>
            + private::ScratchOps<'db>,
    {
        let value = U::unbrand(value).fold_with(self.db, &mut self.table);
        let mut remapper = RemapToCanonical {
            db: self.db,
            subst: &self.local_to_canonical,
        };
        let value = value.fold_with(self.db, &mut remapper);
        uses_only_query_canonical_vars(self.db, &self.canonical_to_original, &value).then(|| {
            let solution = Solution { value };
            let mut extractor = super::canonical::SolutionExtractor::new(
                &mut self.table,
                self.canonical_to_original.clone(),
            );
            solution.value.fold_with(self.db, &mut extractor)
        })
    }

    pub(crate) fn lower_trait_ref(
        &mut self,
        self_ty: ScratchTy<'q, 'db>,
        trait_ref: TraitRefId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        owner_self: Option<ScratchTy<'q, 'db>>,
    ) -> Result<ScratchTraitInst<'q, 'db>, TraitRefLowerError<'db>> {
        lower_trait_ref(
            self.db,
            self_ty.raw,
            trait_ref,
            scope,
            assumptions,
            owner_self.map(|owner| owner.raw),
        )
        .map(ScratchTraitInst::new)
    }

    pub(crate) fn with_impl_assoc_ty<R>(
        &mut self,
        implementor: ImplementorId<'db>,
        receiver: ScratchTy<'q, 'db>,
        name: IdentId<'db>,
        f: impl FnOnce(&mut Self, ScratchTraitInst<'q, 'db>, ScratchTy<'q, 'db>) -> R,
    ) -> Option<R> {
        let snapshot = self.snapshot();
        let implementor = self.table.instantiate_with_fresh_vars(implementor);
        let result = if self
            .table
            .unify(receiver.raw, implementor.self_ty(self.db))
            .is_ok()
        {
            implementor.assoc_ty(self.db, name).map(|assoc_ty| {
                let inst = ScratchTraitInst::new(
                    implementor
                        .trait_(self.db)
                        .fold_with(self.db, &mut self.table),
                );
                let assoc_ty = ScratchTy::new(assoc_ty.fold_with(self.db, &mut self.table));
                f(self, inst, assoc_ty)
            })
        } else {
            None
        };
        self.rollback_to(snapshot);
        result
    }
}

#[allow(private_bounds)]
impl<'db, Q> Canonicalized<'db, Q>
where
    Q: TyFoldable<'db> + Copy + ScratchRepr<'db> + private::ScratchOps<'db>,
{
    pub(crate) fn with_materialized<R>(
        &self,
        db: &'db dyn HirAnalysisDb,
        f: impl for<'q> FnOnce(&mut MaterializedCx<'q, 'db, Q>) -> R,
    ) -> R {
        let mut table = UnificationTable::new(db);
        let mut materializer = ScratchMaterializer::new(&mut table, FxHashMap::default());
        let query_raw = self.canonical().value().fold_with(db, &mut materializer);
        let canonical_to_local = materializer.subst;
        let local_to_canonical = materializer.reverse_subst;
        let mut original_to_local = FxHashMap::default();

        for (canonical, original) in self.subst() {
            if let Some(&local) = canonical_to_local.get(canonical) {
                original_to_local.insert(*original, local);
            }
        }

        let mut cx = MaterializedCx {
            db,
            table,
            query_raw,
            local_to_canonical,
            original_to_local,
            canonical_to_original: self.subst().clone(),
            _brand: PhantomData,
        };
        f(&mut cx)
    }
}

impl<'db> ScratchRepr<'db> for TyId<'db> {
    type Branded<'q> = ScratchTy<'q, 'db>;
}

impl<'db> private::ScratchOps<'db> for TyId<'db> {
    fn brand<'q>(raw: Self) -> Self::Branded<'q> {
        ScratchTy::new(raw)
    }

    fn unbrand<'q>(value: Self::Branded<'q>) -> Self {
        value.raw
    }
}

impl<'db> ScratchRepr<'db> for TraitInstId<'db> {
    type Branded<'q> = ScratchTraitInst<'q, 'db>;
}

impl<'db> private::ScratchOps<'db> for TraitInstId<'db> {
    fn brand<'q>(raw: Self) -> Self::Branded<'q> {
        ScratchTraitInst::new(raw)
    }

    fn unbrand<'q>(value: Self::Branded<'q>) -> Self {
        value.raw
    }
}

struct ScratchMaterializer<'a, 'db, S>
where
    S: UnificationStore<'db>,
{
    table: &'a mut super::unify::UnificationTableBase<'db, S>,
    subst: FxHashMap<TyId<'db>, TyId<'db>>,
    reverse_subst: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'a, 'db, S> ScratchMaterializer<'a, 'db, S>
where
    S: UnificationStore<'db>,
{
    fn new(
        table: &'a mut super::unify::UnificationTableBase<'db, S>,
        subst: FxHashMap<TyId<'db>, TyId<'db>>,
    ) -> Self {
        Self {
            table,
            subst,
            reverse_subst: FxHashMap::default(),
        }
    }
}

impl<'db, S> TyFolder<'db> for ScratchMaterializer<'_, 'db, S>
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
                self.reverse_subst.insert(new_ty, ty);
                new_ty
            }
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(db) {
                    let new_ty = TyId::const_ty_var(
                        db,
                        *const_ty_ty,
                        self.table.new_key(const_ty_ty.kind(db), var.sort),
                    );
                    self.subst.insert(ty, new_ty);
                    self.reverse_subst.insert(new_ty, ty);
                    new_ty
                } else {
                    ty.super_fold_with(db, self)
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

struct RemapToCanonical<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    subst: &'a FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db> TyFolder<'db> for RemapToCanonical<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(&canonical) = self.subst.get(&ty) {
            return canonical;
        }

        match ty.data(self.db) {
            TyData::TyVar(_) => ty,
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(..) = const_ty.data(self.db) {
                    ty
                } else {
                    ty.super_fold_with(db, self)
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

fn uses_only_query_canonical_vars<'db, V>(
    db: &'db dyn HirAnalysisDb,
    subst: &FxHashMap<TyId<'db>, TyId<'db>>,
    value: &V,
) -> bool
where
    V: TyVisitable<'db>,
{
    struct QueryVarChecker<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        subst: &'a FxHashMap<TyId<'db>, TyId<'db>>,
        is_valid: bool,
    }

    impl<'db> TyVisitor<'db> for QueryVarChecker<'_, 'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if !self.is_valid {
                return;
            }

            match ty.data(self.db) {
                TyData::TyVar(_) => self.is_valid &= self.subst.contains_key(&ty),
                _ => walk_ty(self, ty),
            }
        }

        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            if !self.is_valid {
                return;
            }

            match const_ty.data(self.db) {
                ConstTyData::TyVar(var, const_ty_ty) => {
                    let ty = TyId::const_ty_var(self.db, *const_ty_ty, var.key);
                    self.is_valid &= self.subst.contains_key(&ty);
                }
                _ => walk_const_ty(self, const_ty),
            }
        }
    }

    let mut checker = QueryVarChecker {
        db,
        subst,
        is_valid: true,
    };
    value.visit_with(&mut checker);
    checker.is_valid
}

#[cfg(test)]
mod tests {
    use salsa::Update;

    use super::{Canonicalized, ScratchRepr, private};
    use crate::{
        analysis::HirAnalysisDb,
        analysis::ty::{
            fold::{TyFoldable, TyFolder},
            ty_def::{Kind, TyId, TyVarSort},
            visitor::{TyVisitable, TyVisitor},
        },
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn with_materialized_extracts_query_back_to_original() {
        let db = HirAnalysisTestDb::default();
        let mut table = super::UnificationTable::new(&db);
        let original = table.new_var(TyVarSort::General, &Kind::Star);
        let canonicalized = Canonicalized::new(&db, original);

        canonicalized.with_materialized(&db, |cx| {
            assert_eq!(cx.try_extract::<TyId<'_>>(cx.query()), Some(original));
        });
    }

    #[test]
    fn with_materialized_rejects_scratch_only_vars() {
        let db = HirAnalysisTestDb::default();
        let mut table = super::UnificationTable::new(&db);
        let original = table.new_var(TyVarSort::General, &Kind::Star);
        let canonicalized = Canonicalized::new(&db, original);

        canonicalized.with_materialized(&db, |cx| {
            let extra = super::ScratchTy::new(cx.table.new_var(TyVarSort::General, &Kind::Star));
            assert_eq!(cx.try_extract::<TyId<'_>>(extra), None);
        });
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
    struct RepeatedAndConstVars<'db> {
        lhs: TyId<'db>,
        rhs: TyId<'db>,
        const_arg: TyId<'db>,
    }

    impl<'db> TyFoldable<'db> for RepeatedAndConstVars<'db> {
        fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
        where
            F: TyFolder<'db>,
        {
            Self {
                lhs: self.lhs.fold_with(db, folder),
                rhs: self.rhs.fold_with(db, folder),
                const_arg: self.const_arg.fold_with(db, folder),
            }
        }
    }

    impl<'db> TyVisitable<'db> for RepeatedAndConstVars<'db> {
        fn visit_with<V>(&self, visitor: &mut V)
        where
            V: TyVisitor<'db> + ?Sized,
        {
            self.lhs.visit_with(visitor);
            self.rhs.visit_with(visitor);
            self.const_arg.visit_with(visitor);
        }
    }

    impl<'db> ScratchRepr<'db> for RepeatedAndConstVars<'db> {
        type Branded<'q> = RepeatedAndConstVars<'db>;
    }

    impl<'db> private::ScratchOps<'db> for RepeatedAndConstVars<'db> {
        fn brand<'q>(raw: Self) -> <Self as ScratchRepr<'db>>::Branded<'q> {
            raw
        }

        fn unbrand<'q>(value: <Self as ScratchRepr<'db>>::Branded<'q>) -> Self {
            value
        }
    }

    #[test]
    fn with_materialized_extracts_values_derived_from_query() {
        let db = HirAnalysisTestDb::default();
        let mut table = super::UnificationTable::new(&db);
        let repeated = table.new_var(TyVarSort::General, &Kind::Star);
        let const_arg = TyId::const_ty_var(
            &db,
            TyId::u256(&db),
            table.new_key(&Kind::Star, TyVarSort::General),
        );
        let original = RepeatedAndConstVars {
            lhs: repeated,
            rhs: repeated,
            const_arg,
        };
        let canonicalized = Canonicalized::new(&db, original);

        canonicalized.with_materialized(&db, |cx| {
            let query = cx.query();
            assert_eq!(
                cx.try_extract::<RepeatedAndConstVars<'_>>(query),
                Some(original)
            );
        });
    }
}
