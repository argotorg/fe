//! This module contains the unification table for type inference and trait
//! satisfiability checking.

use std::marker::PhantomData;

use either::Either;
use ena::unify::{InPlace, UnifyKey, UnifyValue};
use num_bigint::BigUint;

use super::{
    binder::Binder,
    fold::{TyFoldable, TyFolder},
    trait_def::{ImplementorId, TraitInstId},
    ty_def::{ApplicableTyProp, Kind, TyData, TyId, TyVar, TyVarSort, inference_keys},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::const_ty::{ConstTyData, EvaluatedConstTy},
};

pub(crate) type UnificationTable<'db> = UnificationTableBase<'db, InPlace<InferenceKey<'db>>>;

/// This table should only be used in the trait resolution where the performance
/// of `clone` is the critical. This table provides the very cheap clone
/// operation at the cost of update operations.
///
/// [`UnificationTable`] is probably the one that you need to use for other
/// components.
pub(crate) type PersistentUnificationTable<'db> =
    UnificationTableBase<'db, ena::unify::Persistent<InferenceKey<'db>>>;

pub type Snapshot<U> = ena::unify::Snapshot<U>;
pub type UnificationResult = Result<(), UnificationError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnificationError {
    OccursCheckFailed,
    TypeMismatch,
}

pub trait UnificationStore<'db>:
    Default
    + ena::unify::UnificationStoreBase<Key = InferenceKey<'db>, Value = InferenceValue<'db>>
    + ena::unify::UnificationStore
    + ena::unify::UnificationStoreMut
{
}

impl<'db, U> UnificationStore<'db> for U where
    U: Default
        + ena::unify::UnificationStoreBase<Key = InferenceKey<'db>, Value = InferenceValue<'db>>
        + ena::unify::UnificationStoreBase
        + ena::unify::UnificationStore
        + ena::unify::UnificationStoreMut
{
}

#[derive(Clone)]
pub struct UnificationTableBase<'db, U>
where
    U: ena::unify::UnificationStoreBase,
{
    pub db: &'db dyn HirAnalysisDb,
    table: ena::unify::UnificationTable<U>,
}

impl<'db, U> UnificationTableBase<'db, U>
where
    U: UnificationStore<'db>,
{
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            table: ena::unify::UnificationTable::new(),
        }
    }

    /// Returns the number of the created keys.
    pub fn len(&self) -> usize {
        self.table.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn rollback_to(&mut self, snapshot: Snapshot<U>) {
        self.table.rollback_to(snapshot);
    }

    pub fn snapshot(&mut self) -> Snapshot<U> {
        self.table.snapshot()
    }

    pub fn commit(&mut self, snapshot: Snapshot<U>) {
        self.table.commit(snapshot);
    }

    pub fn unify<T>(&mut self, lhs: T, rhs: T) -> UnificationResult
    where
        T: Unifiable<'db>,
    {
        let snapshot = self.snapshot();
        match lhs.unify(self, rhs) {
            Ok(()) => {
                self.table.commit(snapshot);
                Ok(())
            }
            Err(err) => {
                self.rollback_to(snapshot);
                Err(err)
            }
        }
    }

    /// Returns `Ok()` if the two types were unified, otherwise returns an
    /// error. This method doesn't roll back the unification table. Please
    /// refer to `unify`[Self::unify] if you need to roll back the table
    /// automatically when unification fails.
    fn unify_ty(&mut self, ty1: TyId<'db>, ty2: TyId<'db>) -> UnificationResult {
        if !ty1.kind(self.db).does_match(ty2.kind(self.db)) {
            return Err(UnificationError::TypeMismatch);
        }

        let ty1 = ty1.fold_with(self.db, self);
        let ty2 = ty2.fold_with(self.db, self);

        match (ty1.data(self.db), ty2.data(self.db)) {
            (TyData::TyVar(_), TyData::TyVar(_)) => self.unify_var_var(ty1, ty2),

            (TyData::TyVar(var), _) => self.unify_var_value(var, ty2),

            (_, TyData::TyVar(var)) => self.unify_var_value(var, ty1),

            (TyData::TyApp(ty1_1, ty1_2), TyData::TyApp(ty2_1, ty2_2)) => {
                self.unify_ty(*ty1_1, *ty2_1)?;
                self.unify_ty(*ty1_2, *ty2_2)
            }

            (TyData::TyParam(_), TyData::TyParam(_)) | (TyData::TyBase(_), TyData::TyBase(_)) => {
                if ty1 == ty2 {
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }

            (TyData::Invalid(_), _)
            | (_, TyData::Invalid(_))
            | (TyData::Never, _)
            | (_, TyData::Never) => Ok(()),

            (TyData::ConstTy(const_ty1), TyData::ConstTy(const_ty2)) => {
                self.unify_ty(const_ty1.ty(self.db), const_ty2.ty(self.db))?;

                match (const_ty1.data(self.db), const_ty2.data(self.db)) {
                    (ConstTyData::TyVar(..), ConstTyData::TyVar(..)) => {
                        self.unify_var_var(ty1, ty2)
                    }

                    (ConstTyData::TyVar(var, _), _) => self.unify_var_value(var, ty2),

                    (_, ConstTyData::TyVar(var, _)) => self.unify_var_value(var, ty1),

                    (ConstTyData::TyParam(..), ConstTyData::TyParam(..))
                    | (ConstTyData::Evaluated(..), ConstTyData::Evaluated(..))
                    | (ConstTyData::Abstract(..), ConstTyData::Abstract(..)) => {
                        if const_ty1 == const_ty2 {
                            Ok(())
                        } else {
                            Err(UnificationError::TypeMismatch)
                        }
                    }

                    _ => Err(UnificationError::TypeMismatch),
                }
            }
            (TyData::AssocTy(a1), TyData::AssocTy(a2)) => {
                if a1 == a2 {
                    Ok(())
                } else if a1.name == a2.name {
                    self.unify(a1.trait_, a2.trait_)
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }
            (TyData::AssocTy(_), _) | (_, TyData::AssocTy(_)) => {
                // Associated types should be resolved before unification
                Err(UnificationError::TypeMismatch)
            }

            _ => Err(UnificationError::TypeMismatch),
        }
    }

    pub fn new_var(&mut self, sort: TyVarSort, kind: &Kind) -> TyId<'db> {
        let key = self.new_key(kind, sort);
        TyId::ty_var(self.db, sort, kind.clone(), key)
    }

    pub fn new_var_from_param(&mut self, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(param) => {
                let sort = TyVarSort::General;
                let key = self.new_key(&param.kind, sort);
                TyId::ty_var(self.db, sort, param.kind.clone(), key)
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(_, ty) = const_ty.data(self.db) {
                    let key = self.new_key(ty.kind(self.db), TyVarSort::General);
                    TyId::const_ty_var(self.db, *ty, key)
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    }

    pub(super) fn new_var_for(&mut self, ty_prop: ApplicableTyProp<'db>) -> TyId<'db> {
        let kind = ty_prop.kind;
        let sort = TyVarSort::General;
        let key = self.new_key(&kind, sort);

        match ty_prop.const_ty {
            Some(const_ty) => TyId::const_ty_var(self.db, const_ty, key),
            None => TyId::ty_var(self.db, TyVarSort::General, kind, key),
        }
    }

    pub fn instantiate_with_fresh_vars<T>(&mut self, value: Binder<T>) -> T
    where
        T: TyFoldable<'db>,
    {
        value.instantiate_with(self.db, |ty| self.new_var_from_param(ty))
    }

    pub fn instantiate_to_term(&mut self, mut ty: TyId<'db>) -> TyId<'db> {
        if ty.has_invalid(self.db) {
            return ty;
        };

        while let Some(prop) = ty.applicable_ty(self.db) {
            let arg = self.new_var_for(prop);
            ty = TyId::app(self.db, ty, arg);
        }

        ty
    }

    pub fn new_key(&mut self, kind: &Kind, sort: TyVarSort) -> InferenceKey<'db> {
        self.table
            .new_key(InferenceValue::Unbound(kind.clone(), sort))
    }

    fn probe_impl(&mut self, key: InferenceKey<'db>) -> Either<TyId<'db>, TyVar<'db>> {
        let root_key = self.table.find(key);
        match self.table.probe_value(key) {
            InferenceValue::Bound(ty) => Either::Left(ty),
            InferenceValue::Unbound(kind, sort) => Either::Right(TyVar {
                key: root_key,
                kind,
                sort,
            }),
        }
    }

    /// Try to unify two type variables.
    ///
    /// When the two variables are in the same sort, we can just unify them.
    ///
    /// When the two variables are *NOT* in the same sort, a type variable
    /// that has a broader sort are narrowed down to the narrower one.
    ///
    /// NOTE: This method assumes that we have only two sorts: General and Int.
    fn unify_var_var(&mut self, ty_var1: TyId<'db>, ty_var2: TyId<'db>) -> UnificationResult {
        let (var1, var2) = match (ty_var1.data(self.db), ty_var2.data(self.db)) {
            (TyData::TyVar(var1), TyData::TyVar(var2)) => (var1, var2),
            (TyData::ConstTy(const_ty1), TyData::ConstTy(const_ty2)) => {
                match (const_ty1.data(self.db), const_ty2.data(self.db)) {
                    (ConstTyData::TyVar(var1, _), ConstTyData::TyVar(var2, _)) => (var1, var2),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        };

        match (var1.sort, var2.sort) {
            (sort1, sort2) if sort1 == sort2 => self.table.unify_var_var(var1.key, var2.key),

            (TyVarSort::General, _) | (_, TyVarSort::General) => {
                self.table.unify_var_var(var1.key, var2.key)
            }

            (TyVarSort::String(_), TyVarSort::String(_)) => {
                self.table.unify_var_var(var1.key, var2.key)
            }

            (_, _) => Err(UnificationError::TypeMismatch),
        }
    }

    /// Try to unify a type variable to a type.
    /// We perform the following checks:
    /// 1. Occurrence check: The same type variable must not occur in the type.
    /// 2. Universe check: The sort of the type variable must match the sort of
    ///    the type.
    fn unify_var_value(&mut self, var: &TyVar<'db>, value: TyId<'db>) -> UnificationResult {
        if inference_keys(self.db, &value).contains(&var.key) {
            return Err(UnificationError::OccursCheckFailed);
        }

        if value.has_invalid(self.db) {
            return self
                .table
                .unify_var_value(var.key, InferenceValue::Bound(value));
        }

        let root_key = self.table.find(var.key);
        let root_value = self.table.probe_value(root_key);
        let root_var = match root_value {
            InferenceValue::Unbound(kind, sort) => TyVar {
                key: root_key,
                sort,
                kind,
            },

            InferenceValue::Bound(ty) => {
                if ty == value {
                    return Ok(());
                } else {
                    return Err(UnificationError::TypeMismatch);
                }
            }
        };

        match root_var.sort {
            TyVarSort::General => self
                .table
                .unify_var_value(root_var.key, InferenceValue::Bound(value)),

            TyVarSort::Integral => {
                if value.is_integral(self.db) {
                    self.table
                        .unify_var_value(root_var.key, InferenceValue::Bound(value))
                } else if value.is_never(self.db) {
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }

            TyVarSort::String(n_var) => {
                let (base, args) = value.decompose_ty_app(self.db);

                if base.is_never(self.db) {
                    return Ok(());
                }

                if !base.is_string(self.db) || args.len() != 1 {
                    return Err(UnificationError::TypeMismatch);
                }

                let TyData::ConstTy(const_ty) = args[0].data(self.db) else {
                    return Ok(());
                };

                let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(n_value), _) =
                    const_ty.data(self.db)
                else {
                    return Ok(());
                };

                if &BigUint::from(n_var) <= n_value.data(self.db) {
                    self.table
                        .unify_var_value(root_var.key, InferenceValue::Bound(value))
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferenceKey<'db>(pub(super) u32, pub(super) PhantomData<&'db ()>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceValue<'db> {
    Bound(TyId<'db>),
    Unbound(Kind, TyVarSort),
}

impl<'db> UnifyKey for InferenceKey<'db> {
    type Value = InferenceValue<'db>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(idx: u32) -> Self {
        Self(idx, Default::default())
    }

    fn tag() -> &'static str {
        "InferenceKey"
    }
}

impl UnifyValue for InferenceValue<'_> {
    type Error = UnificationError;

    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        match (v1, v2) {
            (InferenceValue::Unbound(k1, sort1), InferenceValue::Unbound(k2, sort2)) => {
                assert!(k1.does_match(k2));
                if sort1 < sort2 {
                    Ok(InferenceValue::Unbound(k2.clone(), *sort2))
                } else {
                    Ok(InferenceValue::Unbound(k1.clone(), *sort1))
                }
            }

            (InferenceValue::Unbound(_, _), InferenceValue::Bound(ty))
            | (InferenceValue::Bound(ty), InferenceValue::Unbound(_, _)) => {
                Ok(InferenceValue::Bound(*ty))
            }

            (InferenceValue::Bound(ty1), InferenceValue::Bound(ty2)) => {
                if ty1 == ty2 {
                    Ok(InferenceValue::Bound(*ty1))
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }
        }
    }
}

pub trait Unifiable<'db> {
    fn unify<U: UnificationStore<'db>>(
        self,
        table: &mut UnificationTableBase<'db, U>,
        other: Self,
    ) -> UnificationResult;
}

impl<'db> Unifiable<'db> for TyId<'db> {
    fn unify<U: UnificationStore<'db>>(
        self,
        table: &mut UnificationTableBase<'db, U>,
        other: Self,
    ) -> UnificationResult {
        table.unify_ty(self, other)
    }
}

impl<'db> Unifiable<'db> for TraitInstId<'db> {
    fn unify<U: UnificationStore<'db>>(
        self,
        table: &mut UnificationTableBase<'db, U>,
        other: Self,
    ) -> UnificationResult {
        let db = table.db;
        if self.def(db) != other.def(db) {
            return Err(UnificationError::TypeMismatch);
        }

        for (&self_arg, &other_arg) in self.args(db).iter().zip(other.args(db)) {
            table.unify_ty(self_arg, other_arg)?;
        }

        // Unify associated type bindings
        // We need to handle the case where one trait instance has more specific
        // associated type bindings than the other. A more specific constraint
        // (with bindings) should be able to satisfy a less specific constraint
        // (without bindings).

        // First, unify all bindings that are present in both
        for (name, &self_assoc_ty) in self.assoc_type_bindings(db) {
            if let Some(&other_assoc_ty) = other.assoc_type_bindings(db).get(name) {
                table.unify_ty(self_assoc_ty, other_assoc_ty)?;
            }
        }

        // For bindings that are only in other, we need to check if self can satisfy them
        for (name, &_other_assoc_ty) in other.assoc_type_bindings(db) {
            if !self.assoc_type_bindings(db).contains_key(name) {
                // other has a binding that self doesn't have
                // This means other is more specific than self, so unification fails
                return Err(UnificationError::TypeMismatch);
            }
        }

        Ok(())
    }
}

impl<'db> Unifiable<'db> for ImplementorId<'db> {
    fn unify<U: UnificationStore<'db>>(
        self,
        table: &mut UnificationTableBase<'db, U>,
        other: Self,
    ) -> UnificationResult {
        let db = table.db;
        table.unify(self.trait_(db), other.trait_(db))
    }
}

impl<'db, U> TyFolder<'db> for UnificationTableBase<'db, U>
where
    U: UnificationStore<'db>,
{
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let mut resolver = TyVarResolver {
            table: self,
            var_stack: vec![],
        };

        ty.fold_with(db, &mut resolver)
    }
}

struct TyVarResolver<'a, 'db, U>
where
    U: UnificationStore<'db>,
{
    table: &'a mut UnificationTableBase<'db, U>,
    var_stack: Vec<InferenceKey<'db>>,
}

impl<'db, U> TyFolder<'db> for TyVarResolver<'_, 'db, U>
where
    U: UnificationStore<'db>,
{
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let (shallow_resolved, key) = match ty.data(db) {
            TyData::TyVar(var) if !self.var_stack.contains(&var.key) => {
                match self.table.probe_impl(var.key) {
                    Either::Left(ty) => (ty, var.key),
                    Either::Right(var) => return TyId::ty_var(db, var.sort, var.kind, var.key),
                }
            }

            TyData::ConstTy(cty) => match cty.data(db) {
                ConstTyData::TyVar(var, ty) if !self.var_stack.contains(&var.key) => {
                    match self.table.probe_impl(var.key) {
                        Either::Left(ty) => (ty, var.key),
                        Either::Right(var) => {
                            return TyId::const_ty_var(db, *ty, var.key);
                        }
                    }
                }
                _ => {
                    return ty.super_fold_with(db, self);
                }
            },
            TyData::AssocTy(_) => {
                // Associated types should be resolved before unification
                // by the normalization process
                return ty.super_fold_with(db, self);
            }
            _ => {
                return ty.super_fold_with(db, self);
            }
        };

        self.var_stack.push(key);
        let resolved = shallow_resolved.fold_with(self.table.db, self);
        self.var_stack.pop();
        resolved
    }
}
