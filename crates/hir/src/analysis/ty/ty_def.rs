//! This module contains the type definitions for the Fe type system.

use std::fmt;

use crate::{
    hir_def::{
        Body, Enum, ExprId, GenericParamOwner, IdentId, IntegerId, ItemKind, PathId,
        TypeAlias as HirTypeAlias,
        prim_ty::{IntTy as HirIntTy, PrimTy as HirPrimTy, UintTy as HirUintTy},
        scope_graph::ScopeId,
    },
    span::DynLazySpan,
};
use bitflags::bitflags;
use common::{
    indexmap::IndexSet,
    ingot::{Ingot, IngotKind},
};
use num_bigint::BigUint;
use rustc_hash::FxHashSet;
use salsa::Update;
use smallvec::SmallVec;

use super::{
    adt_def::AdtDef,
    const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    trait_def::TraitInstId,
    trait_resolution::{PredicateListId, WellFormedness},
    ty_lower::collect_generic_params,
    unify::InferenceKey,
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::PathRes,
    ty::{adt_def::AdtRef, trait_resolution::check_ty_wf, ty_error::emit_invalid_ty_error},
};
use crate::hir_def::CallableDef;

#[salsa::interned]
#[derive(Debug)]
pub struct TyId<'db> {
    #[return_ref]
    pub data: TyData<'db>,
}

#[salsa::tracked]
impl<'db> TyId<'db> {
    /// Returns the kind of the type.
    #[salsa::tracked(return_ref)]
    pub fn kind(self, db: &'db dyn HirAnalysisDb) -> Kind {
        self.data(db).kind(db)
    }

    /// Returns the current arguments of the type.
    /// ## Example
    /// Calling this method for `TyApp<TyApp<Adt, T>, U>` returns `[T, U]`.
    pub fn generic_args(self, db: &'db dyn HirAnalysisDb) -> &'db [Self] {
        let (_, args) = self.decompose_ty_app(db);
        args
    }

    /// Returns teh base type of this type.
    /// ## Example
    /// `TyApp<Adt, i32>` returns `Adt`.
    /// `TyApp<TyParam<T>, i32>` returns `TyParam<T>`.
    pub fn base_ty(self, db: &'db dyn HirAnalysisDb) -> Self {
        self.decompose_ty_app(db).0
    }

    /// Returns the type of const type if the type is a const type.
    pub fn const_ty_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        match self.data(db) {
            TyData::ConstTy(const_ty) => Some(const_ty.ty(db)),
            _ => None,
        }
    }

    /// Returns `true` is the type has `*` kind.
    pub fn is_star_kind(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.kind(db), Kind::Star | Kind::Any)
    }

    /// Returns `true` if the type is an integral type(like `u32`, `i32` etc.)
    pub fn is_integral(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(ty_base) => ty_base.is_integral(),
            TyData::TyVar(var) => {
                matches!(var.sort, TyVarSort::Integral)
            }
            _ => false,
        }
    }

    pub fn is_integral_var(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyVar(var) => {
                matches!(var.sort, TyVarSort::Integral)
            }
            _ => false,
        }
    }

    /// Returns `true` if the type is a bool type.
    pub fn is_bool(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(ty_base) => ty_base.is_bool(),
            _ => false,
        }
    }

    /// Returns `true` if the type is a never type.
    pub fn is_never(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::Never)
    }

    /// Returns an ingot associated with this type.
    pub fn ingot(self, db: &'db dyn HirAnalysisDb) -> Option<Ingot<'db>> {
        fn ingot_from_non_projection<'db>(
            db: &'db dyn HirAnalysisDb,
            mut ty: TyId<'db>,
        ) -> Option<Ingot<'db>> {
            loop {
                match ty.data(db) {
                    TyData::TyBase(TyBase::Adt(adt)) => return adt.ingot(db).into(),
                    TyData::TyBase(TyBase::Contract(contract)) => {
                        return contract.top_mod(db).ingot(db).into();
                    }
                    TyData::TyBase(TyBase::Func(def)) => return def.ingot(db).into(),
                    TyData::TyApp(lhs, _) => {
                        ty = *lhs;
                    }
                    _ => return None,
                }
            }
        }

        match self.data(db) {
            TyData::TyBase(TyBase::Adt(adt)) => adt.ingot(db).into(),
            TyData::TyBase(TyBase::Contract(contract)) => contract.top_mod(db).ingot(db).into(),
            TyData::TyBase(TyBase::Func(def)) => def.ingot(db).into(),
            TyData::TyApp(lhs, _) => lhs.ingot(db),
            // Projection types don't have a single defining ingot, but we still want an ingot
            // that can be used to search for relevant trait impls. Using an ingot that is
            // referenced by the underlying trait arguments generally yields the best results.
            TyData::AssocTy(assoc_ty) => assoc_ty
                .trait_
                .args(db)
                .iter()
                .copied()
                .find_map(|arg| ingot_from_non_projection(db, arg)),
            TyData::QualifiedTy(trait_inst) => trait_inst
                .args(db)
                .iter()
                .copied()
                .find_map(|arg| ingot_from_non_projection(db, arg)),
            _ => None,
        }
    }

    pub fn invalid_cause(self, db: &'db dyn HirAnalysisDb) -> Option<InvalidCause<'db>> {
        match self.data(db) {
            TyData::Invalid(cause) => Some(cause.clone()),
            _ => None,
        }
    }

    pub fn flags(self, db: &dyn HirAnalysisDb) -> TyFlags {
        ty_flags(db, self)
    }

    pub fn has_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_INVALID)
    }

    pub fn has_param(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_PARAM)
    }

    pub fn has_var(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_VAR)
    }

    /// Returns `true` if the type has a `*` kind.
    pub fn has_star_kind(self, db: &dyn HirAnalysisDb) -> bool {
        !matches!(self.kind(db), Kind::Abs(..))
    }

    #[salsa::tracked(return_ref)]
    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.data(db) {
            TyData::TyVar(var) => var.pretty_print(),
            TyData::TyParam(param) => param.pretty_print(db),
            TyData::AssocTy(assoc_ty) => {
                let self_ty = assoc_ty.trait_.self_ty(db);
                format!("{}::{}", self_ty.pretty_print(db), assoc_ty.name.data(db))
            }
            TyData::QualifiedTy(trait_inst) => {
                format!(
                    "<{} as {}>",
                    trait_inst.self_ty(db).pretty_print(db),
                    trait_inst.pretty_print(db, false)
                )
            }
            TyData::TyApp(_, _) => pretty_print_ty_app(db, self),
            TyData::TyBase(base) => base.pretty_print(db),
            TyData::ConstTy(const_ty) => const_ty.pretty_print(db),
            TyData::Never => "!".to_string(),
            TyData::Invalid(cause) => format!("invalid({})", cause.pretty_print(db)),
        }
    }

    pub fn is_inherent_impl_allowed(self, db: &dyn HirAnalysisDb, ingot: Ingot) -> bool {
        if self.is_param(db) {
            return false;
        };

        let ty_ingot = self.ingot(db);
        match ingot.kind(db) {
            IngotKind::Core | IngotKind::Std => ty_ingot.is_none() || ty_ingot == Some(ingot),
            _ => ty_ingot == Some(ingot),
        }
    }

    /// Decompose type application into the base type and type arguments, this
    /// doesn't perform deconstruction recursively. e.g.,
    /// `App(App(T, U), App(V, W))` -> `(T, [U, App(V, W)])`
    pub fn decompose_ty_app(self, db: &'db dyn HirAnalysisDb) -> (TyId<'db>, &'db [TyId<'db>]) {
        let (base, args) = decompose_ty_app(db, self);
        (*base, args)
    }

    pub(super) fn ptr(db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Ptr)))
    }

    pub(super) fn tuple(db: &'db dyn HirAnalysisDb, n: usize) -> Self {
        Self::new(db, TyData::TyBase(TyBase::tuple(n)))
    }

    pub(super) fn tuple_with_elems(db: &'db dyn HirAnalysisDb, elems: &[TyId<'db>]) -> Self {
        let base = TyBase::tuple(elems.len());
        let mut ty = Self::new(db, TyData::TyBase(base));
        for &elem in elems {
            ty = Self::app(db, ty, elem);
        }
        ty
    }

    pub fn bool(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Bool)))
    }

    pub fn u256(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)))
    }

    pub(super) fn array(db: &'db dyn HirAnalysisDb, elem: TyId<'db>) -> Self {
        let base = TyBase::Prim(PrimTy::Array);
        let array = Self::new(db, TyData::TyBase(base));
        Self::app(db, array, elem)
    }

    pub(super) fn array_with_len(db: &'db dyn HirAnalysisDb, elem: TyId<'db>, len: usize) -> Self {
        let array = Self::array(db, elem);

        let len = EvaluatedConstTy::LitInt(IntegerId::new(db, BigUint::from(len)));
        let len = ConstTyData::Evaluated(len, array.applicable_ty(db).unwrap().const_ty.unwrap());
        let len = TyId::const_ty(db, ConstTyId::new(db, len));

        TyId::app(db, array, len)
    }

    pub fn unit(db: &'db dyn HirAnalysisDb) -> Self {
        Self::tuple(db, 0)
    }

    pub fn never(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::Never)
    }

    pub(super) fn const_ty(db: &'db dyn HirAnalysisDb, const_ty: ConstTyId<'db>) -> Self {
        Self::new(db, TyData::ConstTy(const_ty))
    }

    pub fn assoc_ty(
        db: &'db dyn HirAnalysisDb,
        trait_: TraitInstId<'db>,
        name: IdentId<'db>,
    ) -> Self {
        let assoc_ty = AssocTy { trait_, name };
        Self::new(db, TyData::AssocTy(assoc_ty))
    }

    pub(crate) fn qualified_ty(db: &'db dyn HirAnalysisDb, trait_: TraitInstId<'db>) -> Self {
        Self::new(db, TyData::QualifiedTy(trait_))
    }

    pub(crate) fn adt(db: &'db dyn HirAnalysisDb, adt: AdtDef<'db>) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Adt(adt)))
    }

    pub(crate) fn contract(
        db: &'db dyn HirAnalysisDb,
        contract: crate::hir_def::Contract<'db>,
    ) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Contract(contract)))
    }

    // TODO: Add semantic view and restrict visibility
    pub fn func(db: &'db dyn HirAnalysisDb, func: CallableDef<'db>) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Func(func)))
    }

    pub fn is_func(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyBase(TyBase::Func(_)))
    }

    pub(crate) fn is_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyParam(ty_param) if ty_param.is_trait_self())
    }

    pub(crate) fn is_ty_var(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyVar(_))
    }

    pub fn is_const_ty(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::ConstTy(_))
    }

    /// Returns the contract if this type is a contract type.
    pub fn as_contract(self, db: &'db dyn HirAnalysisDb) -> Option<crate::hir_def::Contract<'db>> {
        match self.base_ty(db).data(db) {
            TyData::TyBase(base) => base.contract(),
            _ => None,
        }
    }

    pub fn is_tuple(self, db: &dyn HirAnalysisDb) -> bool {
        // Check if this is directly a tuple type
        if matches!(
            self.data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_)))
        ) {
            return true;
        }

        // Check if the base type is a tuple (for TyApp cases)
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_)))
        )
    }

    pub fn is_array(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Array))
        )
    }

    /// Returns `true` if this type is known to have no runtime representation.
    ///
    /// This is a structural check (not based on byte-size calculation):
    /// - `()` and empty structs are zero-sized
    /// - tuples/structs/arrays are zero-sized iff all elements/fields are zero-sized
    pub fn is_zero_sized(self, db: &'db dyn HirAnalysisDb) -> bool {
        fn inner<'db>(
            db: &'db dyn HirAnalysisDb,
            ty: TyId<'db>,
            visiting: &mut FxHashSet<TyId<'db>>,
        ) -> bool {
            if !visiting.insert(ty) {
                return false;
            }

            let result = if ty.is_never(db)
                || matches!(ty.base_ty(db).data(db), TyData::TyBase(TyBase::Func(_)))
            {
                true
            } else if ty.is_tuple(db) {
                ty.field_types(db)
                    .into_iter()
                    .all(|field_ty| inner(db, field_ty, visiting))
            } else if ty.is_array(db) {
                let (_, args) = ty.decompose_ty_app(db);
                match args.first().copied() {
                    Some(elem_ty) => inner(db, elem_ty, visiting),
                    None => false,
                }
            } else if let Some(adt_def) = ty.adt_def(db)
                && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
            {
                ty.field_types(db)
                    .into_iter()
                    .all(|field_ty| inner(db, field_ty, visiting))
            } else {
                false
            };

            visiting.remove(&ty);
            result
        }

        inner(db, self, &mut FxHashSet::default())
    }

    pub fn is_string(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::String))
        )
    }

    pub(crate) fn is_param(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyParam(_))
    }

    /// Returns `true` if the base type is a user defined `struct` type.
    pub fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        let base_ty = self.base_ty(db);
        match base_ty.data(db) {
            TyData::TyBase(TyBase::Adt(adt)) => adt.is_struct(db),
            _ => false,
        }
    }

    pub fn is_prim(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyBase(TyBase::Prim(_)))
    }

    pub fn as_enum(self, db: &'db dyn HirAnalysisDb) -> Option<Enum<'db>> {
        let base_ty = self.base_ty(db);
        if let Some(adt_ref) = base_ty.adt_ref(db)
            && let AdtRef::Enum(enum_) = adt_ref
        {
            Some(enum_)
        } else {
            None
        }
    }

    pub(crate) fn as_scope(self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self.base_ty(db).data(db) {
            TyData::TyParam(param) => Some(param.scope(db)),
            TyData::AssocTy(assoc_ty) => Some(assoc_ty.scope(db)),
            TyData::QualifiedTy(trait_inst) => Some(trait_inst.def(db).scope()),
            TyData::TyBase(TyBase::Adt(adt)) => Some(adt.scope(db)),
            TyData::TyBase(TyBase::Contract(c)) => Some(c.scope()),
            TyData::TyBase(TyBase::Func(func)) => Some(func.scope()),
            TyData::TyBase(TyBase::Prim(..)) => None,
            TyData::ConstTy(const_ty) => match const_ty.data(db) {
                ConstTyData::TyVar(..) => None,
                ConstTyData::TyParam(ty_param, _) => Some(ty_param.scope(db)),
                ConstTyData::Evaluated(..) => None,
                ConstTyData::Abstract(..) => None,
                ConstTyData::UnEvaluated { body, .. } => Some(body.scope()),
            },

            TyData::Never | TyData::Invalid(_) | TyData::TyVar(_) => None,
            TyData::TyApp(..) => unreachable!(),
        }
    }

    /// Returns the span of the name of the type, at its definition site
    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        match self.base_ty(db).data(db) {
            TyData::TyVar(_) => None,
            TyData::TyParam(param) => param.scope(db).name_span(db),
            TyData::AssocTy(assoc_ty) => assoc_ty.scope(db).name_span(db),
            TyData::QualifiedTy(trait_inst) => trait_inst.def(db).scope().name_span(db),

            TyData::TyBase(TyBase::Adt(adt)) => Some(adt.name_span(db)),
            TyData::TyBase(TyBase::Contract(c)) => c.scope().name_span(db),
            TyData::TyBase(TyBase::Func(func)) => Some(func.name_span()),
            TyData::TyBase(TyBase::Prim(_)) => None,

            TyData::ConstTy(ty) => match ty.data(db) {
                ConstTyData::TyParam(param, _) => param.scope(db).name_span(db),
                _ => None,
            },

            TyData::Never | TyData::Invalid(_) => None,
            TyData::TyApp(..) => unreachable!(),
        }
    }

    /// Emit diagnostics for the type if the type contains invalid types.
    pub(crate) fn emit_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        emit_invalid_ty_error(db, self, span)
    }

    pub(super) fn emit_wf_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        ingot: Ingot<'db>,
        assumptions: PredicateListId<'db>,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_ty_wf(db, ingot, self, assumptions)
        {
            Some(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            )
        } else {
            None
        }
    }

    pub(super) fn ty_var(
        db: &'db dyn HirAnalysisDb,
        sort: TyVarSort,
        kind: Kind,
        key: InferenceKey<'db>,
    ) -> Self {
        Self::new(db, TyData::TyVar(TyVar { sort, kind, key }))
    }

    pub(super) fn const_ty_var(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        key: InferenceKey<'db>,
    ) -> Self {
        let ty_var = TyVar {
            sort: TyVarSort::General,
            kind: ty.kind(db).clone(),
            key,
        };

        let data = ConstTyData::TyVar(ty_var, ty);
        Self::new(db, TyData::ConstTy(ConstTyId::new(db, data)))
    }

    /// Perform type level application.
    pub fn app(db: &'db dyn HirAnalysisDb, lhs: Self, rhs: Self) -> TyId<'db> {
        let Some(applicable_ty) = lhs.applicable_ty(db) else {
            return Self::invalid(
                db,
                InvalidCause::KindMismatch {
                    expected: None,
                    given: rhs,
                },
            );
        };

        let rhs = rhs
            .evaluate_const_ty(db, applicable_ty.const_ty)
            .unwrap_or_else(|cause| Self::invalid(db, cause));

        let applicable_kind = applicable_ty.kind;
        if !applicable_kind.does_match(rhs.kind(db)) {
            return Self::invalid(
                db,
                InvalidCause::KindMismatch {
                    expected: Some(applicable_kind),
                    given: rhs,
                },
            );
        };

        Self::new(db, TyData::TyApp(lhs, rhs))
    }

    /// Check if this type contains an associated type of a type parameter
    pub fn contains_assoc_ty_of_param(self, db: &'db dyn HirAnalysisDb) -> bool {
        use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_ty};

        struct AssocTyOfParamChecker<'db> {
            db: &'db dyn HirAnalysisDb,
            found: bool,
        }

        impl<'db> TyVisitor<'db> for AssocTyOfParamChecker<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_ty(&mut self, ty: TyId<'db>) {
                if self.found {
                    return;
                }
                if let TyData::AssocTy(assoc_ty) = ty.data(self.db) {
                    // Check if the trait instance's self type is a type parameter
                    if matches!(
                        assoc_ty.trait_.self_ty(self.db).data(self.db),
                        TyData::TyParam(_)
                    ) {
                        self.found = true;
                        return;
                    }
                }

                walk_ty(self, ty);
            }
        }

        let mut checker = AssocTyOfParamChecker { db, found: false };
        self.visit_with(&mut checker);
        checker.found
    }

    /// Folds over a series of type applications from left to right.
    ///
    /// For example, given base type B and arg types [A1, A2, A3],
    /// foldl would produce ((B A1) A2) A3).
    pub fn foldl(db: &'db dyn HirAnalysisDb, mut base: Self, args: &[Self]) -> Self {
        for (i, arg) in args.iter().enumerate() {
            if base.applicable_ty(db).is_some() {
                base = Self::app(db, base, *arg);
            } else {
                return Self::invalid(
                    db,
                    InvalidCause::TooManyGenericArgs {
                        expected: i,
                        given: args.len(),
                    },
                );
            }
        }
        base
    }

    pub fn invalid(db: &'db dyn HirAnalysisDb, cause: InvalidCause<'db>) -> Self {
        Self::new(db, TyData::Invalid(cause))
    }

    pub(crate) fn from_hir_prim_ty(db: &'db dyn HirAnalysisDb, hir_prim: HirPrimTy) -> Self {
        Self::new(db, TyData::TyBase(hir_prim.into()))
    }

    pub(crate) fn evaluate_const_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        expected_ty: Option<TyId<'db>>,
    ) -> Result<TyId<'db>, InvalidCause<'db>> {
        match (expected_ty, self.data(db)) {
            (Some(expected_const_ty), TyData::ConstTy(const_ty)) => {
                if expected_const_ty.has_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    let evaluated_const_ty = const_ty.evaluate(db, expected_const_ty.into());
                    let evaluated_const_ty_ty = evaluated_const_ty.ty(db);
                    if let Some(cause) = evaluated_const_ty_ty.invalid_cause(db) {
                        Err(cause)
                    } else {
                        Ok(TyId::const_ty(db, evaluated_const_ty))
                    }
                }
            }

            (Some(expected_const_ty), _) => {
                if expected_const_ty.has_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    Err(InvalidCause::ConstTyExpected {
                        expected: expected_const_ty,
                    })
                }
            }

            (None, TyData::ConstTy(const_ty)) => Err(InvalidCause::NormalTypeExpected {
                given: TyId::const_ty(db, *const_ty),
            }),

            (None, _) => Ok(self),
        }
    }

    /// Returns the property of the type that can be applied to the `self`.
    pub fn applicable_ty(self, db: &'db dyn HirAnalysisDb) -> Option<ApplicableTyProp<'db>> {
        let applicable_kind = match self.kind(db) {
            Kind::Star => return None,
            Kind::Abs(inner) => inner.0.clone(),
            Kind::Any => Kind::Any,
        };

        let (base, args) = self.decompose_ty_app(db);
        let TyData::TyBase(base) = base.data(db) else {
            return Some(ApplicableTyProp {
                kind: applicable_kind.clone(),
                const_ty: None,
            });
        };

        let const_ty = match base {
            TyBase::Adt(adt_def) => {
                let params = adt_def.params(db);
                let param = params.get(args.len()).copied();
                param.and_then(|ty| ty.const_ty_ty(db))
            }

            TyBase::Func(func_def) => {
                let params = func_def.params(db);
                let param = params.get(args.len()).copied();
                param.and_then(|ty| ty.const_ty_ty(db))
            }

            TyBase::Prim(PrimTy::Array) => {
                if args.len() == 1 {
                    Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize))))
                } else {
                    None
                }
            }

            TyBase::Prim(PrimTy::String) => {
                if args.is_empty() {
                    Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize))))
                } else {
                    None
                }
            }

            _ => None,
        };

        Some(ApplicableTyProp {
            kind: applicable_kind.clone(),
            const_ty,
        })
    }

    /// Returns the number of fields for tuple types and structs
    pub fn field_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        if self.is_tuple(db) {
            let (_, elems) = self.decompose_ty_app(db);
            elems.len()
        } else if let Some(adt_def) = self.adt_def(db) {
            match adt_def.adt_ref(db) {
                AdtRef::Struct(_) => adt_def.fields(db)[0].num_types(),
                _ => 0,
            }
        } else {
            0
        }
    }

    /// Returns the field types for tuple types and structs
    pub fn field_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        if self.is_tuple(db) {
            let (_, elems) = self.decompose_ty_app(db);
            elems.to_vec()
        } else if let Some(adt_def) = self.adt_def(db) {
            match adt_def.adt_ref(db) {
                AdtRef::Struct(_) => {
                    let args = self.generic_args(db);
                    (0..adt_def.fields(db)[0].num_types())
                        .map(|idx| adt_def.fields(db)[0].ty(db, idx).instantiate(db, args))
                        .collect()
                }
                _ => vec![],
            }
        } else {
            vec![]
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ApplicableTyProp<'db> {
    /// A kind of the applicable type.
    pub kind: Kind,
    /// An expected type of const type if the applicable type is a const type.
    pub const_ty: Option<TyId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData<'db> {
    /// Type variable.
    TyVar(TyVar<'db>),

    /// Type Parameter.
    TyParam(TyParam<'db>),

    AssocTy(AssocTy<'db>),

    /// Qualified type, e.g., `<T as Iterator>`.
    QualifiedTy(TraitInstId<'db>),

    // Type application,
    // e.g., `Option<i32>` is represented as `TApp(TyConst(Option), TyConst(i32))`.
    TyApp(TyId<'db>, TyId<'db>),

    /// A concrete type, e.g., `i32`, `u32`, `bool`, `String`, `Result` etc.
    TyBase(TyBase<'db>),

    ConstTy(ConstTyId<'db>),

    /// A never(bottom) type.
    Never,

    // Invalid type which means the type is ill-formed.
    // This type can be unified with any other types.
    // NOTE: For type soundness check in this level, we don't consider trait satisfiability.
    Invalid(InvalidCause<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidCause<'db> {
    /// Type is not fully applied where it is required.
    NotFullyApplied,

    /// Kind mismatch between two types.
    KindMismatch {
        expected: Option<Kind>,
        given: TyId<'db>,
    },

    TooManyGenericArgs {
        expected: usize,
        given: usize,
    },

    InvalidConstParamTy,

    RecursiveConstParamTy,

    /// The given type doesn't match the expected const type.
    ConstTyMismatch {
        expected: TyId<'db>,
        given: TyId<'db>,
    },

    /// The given type is not a const type where it is required.
    ConstTyExpected {
        expected: TyId<'db>,
    },

    /// The given type is const type where it is *NOT* required.
    NormalTypeExpected {
        given: TyId<'db>,
    },

    /// Type alias parameter is not bound.
    /// NOTE: In our type system, type alias is a macro, so we can't perform
    /// partial application to type alias.
    UnboundTypeAliasParam {
        alias: HirTypeAlias<'db>,
        n_given_args: usize,
    },

    AliasCycle(SmallVec<HirTypeAlias<'db>, 4>),

    // The given expression is not supported yet in the const type context.
    // TODO: Remove this error kind and introduce a new error kind for more specific cause when
    // type inference is implemented.
    InvalidConstTyExpr {
        body: Body<'db>,
    },

    ConstEvalUnsupported {
        body: Body<'db>,
        expr: ExprId,
    },

    ConstEvalNonConstCall {
        body: Body<'db>,
        expr: ExprId,
    },

    ConstEvalDivisionByZero {
        body: Body<'db>,
        expr: ExprId,
    },

    ConstEvalStepLimitExceeded {
        body: Body<'db>,
        expr: ExprId,
    },

    ConstEvalRecursionLimitExceeded {
        body: Body<'db>,
        expr: ExprId,
    },

    // TraitConstraintNotSat(PredicateId),
    ParseError,

    /// Path resolution failed during type lowering
    PathResolutionFailed {
        path: PathId<'db>,
    },

    NotAType(PathRes<'db>),

    /// `Other` indicates the cause is already reported in other analysis
    /// passes, e.g., parser or name resolution.
    Other,
}

impl InvalidCause<'_> {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            InvalidCause::KindMismatch { expected, given } => format!(
                "KindMismatch {{ expected: {:?}, given: {} }}",
                expected.clone().map(|k| format!("{k}")),
                given.pretty_print(db)
            ),
            InvalidCause::ConstTyMismatch { expected, given } => format!(
                "ConstTyMismatch {{ expected: {}, given: {} }}",
                expected.pretty_print(db),
                given.pretty_print(db)
            ),
            InvalidCause::ConstTyExpected { expected } => {
                format!("ConstTyExpected({})", expected.pretty_print(db))
            }
            InvalidCause::NormalTypeExpected { given } => {
                format!("NormallTyExpected({})", given.pretty_print(db))
            }
            InvalidCause::UnboundTypeAliasParam {
                alias,
                n_given_args,
            } => {
                format!(
                    "UnboundTypeAliasParam {{ alias: {:?},  given: {n_given_args} }}",
                    alias.name(db).to_opt().map(|i| i.data(db)),
                )
            }
            InvalidCause::AliasCycle(v) => format!("AliasCycle(len={})", v.len()),
            InvalidCause::PathResolutionFailed { path } => {
                format!("PathResolutionFailed({})", path.pretty_print(db))
            }
            InvalidCause::NotAType(res) => format!(
                "NotAType({})",
                res.pretty_path(db)
                    .unwrap_or_else(|| res.kind_name().into())
            ),
            InvalidCause::NotFullyApplied
            | InvalidCause::TooManyGenericArgs { .. }
            | InvalidCause::InvalidConstParamTy
            | InvalidCause::RecursiveConstParamTy
            | InvalidCause::ParseError
            | InvalidCause::Other => format!("{self:?}"),

            InvalidCause::InvalidConstTyExpr { body: _ } => "InvalidConstTyExpr".into(),
            InvalidCause::ConstEvalUnsupported { .. } => "ConstEvalUnsupported".into(),
            InvalidCause::ConstEvalNonConstCall { .. } => "ConstEvalNonConstCall".into(),
            InvalidCause::ConstEvalDivisionByZero { .. } => "ConstEvalDivisionByZero".into(),
            InvalidCause::ConstEvalStepLimitExceeded { .. } => "ConstEvalStepLimitExceeded".into(),
            InvalidCause::ConstEvalRecursionLimitExceeded { .. } => {
                "ConstEvalRecursionLimitExceeded".into()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Represents star kind, i.e., `*` kind.
    Star,

    /// Represents higher kinded types.
    /// e.g.,
    /// `* -> *`, `(* -> *) -> *` or `* -> (* -> *) -> *`
    Abs(Box<(Kind, Kind)>),

    /// `Any` kind is set to the type iff the type is `Invalid`.
    Any,
}

impl Kind {
    fn abs(lhs: Kind, rhs: Kind) -> Self {
        Kind::Abs(Box::new((lhs, rhs)))
    }

    pub fn does_match(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Star, Self::Star) => true,
            (Self::Abs(a), Self::Abs(b)) => a.0.does_match(&b.0) && a.1.does_match(&b.1),
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Abs(inner) => write!(f, "({} -> {})", inner.0, inner.1),
            Self::Any => write!(f, "Any"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar<'db> {
    pub sort: TyVarSort,
    pub kind: Kind,
    pub(super) key: InferenceKey<'db>,
}

impl std::cmp::PartialOrd for TyVar<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for TyVar<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return std::cmp::Ordering::Equal;
        }
        self.key.cmp(&other.key)
    }
}

/// Represents the sort of a type variable that indicates what type domain
/// can be unified with the type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVarSort {
    /// Type variable that can be unified with any other types.
    General,

    /// Type variable that can be unified with only string types that has at
    /// least the given length.
    String(usize),

    /// Type variable that can be unified with only integral types.
    Integral,
}

impl PartialOrd for TyVarSort {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::General, Self::General) => Some(std::cmp::Ordering::Equal),
            (Self::General, _) => Some(std::cmp::Ordering::Less),
            (_, Self::General) => Some(std::cmp::Ordering::Greater),
            (Self::String(n1), Self::String(n2)) => n1.partial_cmp(n2),
            (Self::String(_), _) | (_, Self::String(_)) => None,
            (Self::Integral, Self::Integral) => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl TyVar<'_> {
    pub(super) fn pretty_print(&self) -> String {
        match self.sort {
            TyVarSort::General => ("_").to_string(),
            TyVarSort::Integral => "{integer}".to_string(),
            TyVarSort::String(n) => format!("String<{n}>").to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct AssocTy<'db> {
    pub trait_: TraitInstId<'db>,
    pub name: IdentId<'db>,
}

impl<'db> AssocTy<'db> {
    pub fn scope(&self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        // Find the index of this associated type in the trait's type list
        let trait_def = self.trait_.def(db);
        let idx = trait_def
            .assoc_types(db)
            .enumerate()
            .find(|(_, t)| t.name(db) == Some(self.name))
            .map(|(i, _)| i)
            .unwrap();
        ScopeId::TraitType(trait_def, idx as u16)
    }
}

/// Type generics parameter. We also treat `Self` type in a trait definition as
/// a special type parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam<'db> {
    pub name: IdentId<'db>,
    // The index points to the lowered type parameter list, which means that the idx doesn't
    // correspond to the index of the type parameter in the original source code.
    // E.g.,
    // ```fe
    // impl Foo<T, U> {
    //     fn foo<V>(v: V) {}
    // ```
    // The `foo`'s type parameter list is lowered to [`T`, `U`, `V`], so the index of `V` is 2.
    pub idx: usize,
    pub kind: Kind,
    variant: Variant,
    pub owner: ScopeId<'db>,
}

impl<'db> TyParam<'db> {
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        TyId::new(db, TyData::TyParam(self))
    }

    pub(super) fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        // For effect parameters, show `name: Trait` if possible
        if self.is_effect() {
            let name_str = self.name.data(db).to_string();
            // Attempt to get owning function and retrieve effect key path at this index
            if let ItemKind::Func(func) = self.owner.item()
                && let Some(view) = func.effect_params(db).nth(self.idx)
                && let Some(path) = view.key_path(db)
                && let Some(trait_ident) = path.ident(db).to_opt()
            {
                return format!("{}: {}", name_str, trait_ident.data(db));
            }
            return name_str;
        }
        self.name.data(db).to_string()
    }

    pub fn is_trait_self(&self) -> bool {
        matches!(self.variant, Variant::TraitSelf)
    }

    pub fn is_normal(&self) -> bool {
        matches!(self.variant, Variant::Normal)
    }

    pub fn is_effect(&self) -> bool {
        matches!(self.variant, Variant::Effect)
    }

    pub fn is_effect_provider(&self) -> bool {
        matches!(self.variant, Variant::EffectProvider)
    }

    pub(super) fn normal_param(
        name: IdentId<'db>,
        idx: usize,
        kind: Kind,
        scope: ScopeId<'db>,
    ) -> Self {
        Self {
            name,
            idx,
            kind,
            variant: Variant::Normal,
            owner: scope,
        }
    }

    pub fn trait_self(db: &'db dyn HirAnalysisDb, kind: Kind, scope: ScopeId<'db>) -> Self {
        Self {
            name: IdentId::make_self_ty(db),
            idx: 0,
            kind,
            variant: Variant::TraitSelf,
            owner: scope,
        }
    }

    /// Create an effect parameter TyParam local to a function body.
    pub fn effect_param(name: IdentId<'db>, idx: usize, scope: ScopeId<'db>) -> Self {
        Self {
            name,
            idx,
            kind: Kind::Star,
            variant: Variant::Effect,
            owner: scope,
        }
    }

    /// Create a synthetic generic parameter that carries the "provider type" for a type effect.
    pub fn effect_provider_param(name: IdentId<'db>, idx: usize, scope: ScopeId<'db>) -> Self {
        Self {
            name,
            idx,
            kind: Kind::Star,
            variant: Variant::EffectProvider,
            owner: scope,
        }
    }

    pub fn original_idx(&self, db: &'db dyn HirAnalysisDb) -> usize {
        match self.variant {
            Variant::Normal | Variant::TraitSelf => {
                let owner = GenericParamOwner::from_item_opt(self.owner.item()).unwrap();
                let param_set = collect_generic_params(db, owner);
                let offset = param_set.offset_to_explicit_params_position(db);

                // TyParam.idx includes implicit params, subtract offset to get original idx
                self.idx - offset
            }
            Variant::Effect => self.idx,
            Variant::EffectProvider => self.idx,
        }
    }

    pub fn scope(&self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        match self.variant {
            Variant::TraitSelf => self.owner,
            Variant::Normal => {
                ScopeId::GenericParam(self.owner.item(), self.original_idx(db) as u16)
            }
            Variant::Effect => ScopeId::FuncParam(self.owner.item(), self.idx as u16),
            Variant::EffectProvider => self.owner,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Variant {
    Normal,
    TraitSelf,
    /// Effect parameter local to a function `uses` list
    Effect,
    /// Synthetic generic parameter used to encode type-effect provider domains.
    ///
    /// These are inserted by type lowering for functions that have type effects so that
    /// monomorphization can treat effect domains as ordinary generic arguments.
    EffectProvider,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, Update)]
pub enum TyBase<'db> {
    Prim(PrimTy),
    Adt(AdtDef<'db>),
    Contract(crate::hir_def::Contract<'db>),
    Func(CallableDef<'db>),
}

impl<'db> TyBase<'db> {
    pub fn is_integral(self) -> bool {
        match self {
            Self::Prim(prim) => prim.is_integral(),
            _ => false,
        }
    }

    pub fn is_bool(self) -> bool {
        match self {
            Self::Prim(prim) => prim.is_bool(),
            _ => false,
        }
    }

    pub(super) fn tuple(n: usize) -> Self {
        Self::Prim(PrimTy::Tuple(n))
    }

    fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            Self::Prim(prim) => match prim {
                PrimTy::Bool => "bool",
                PrimTy::U8 => "u8",
                PrimTy::U16 => "u16",
                PrimTy::U32 => "u32",
                PrimTy::U64 => "u64",
                PrimTy::U128 => "u128",
                PrimTy::U256 => "u256",
                PrimTy::Usize => "usize",
                PrimTy::I8 => "i8",
                PrimTy::I16 => "i16",
                PrimTy::I32 => "i32",
                PrimTy::I64 => "i64",
                PrimTy::I128 => "i128",
                PrimTy::I256 => "i256",
                PrimTy::Isize => "isize",
                PrimTy::String => "String",
                PrimTy::Array => "[]",
                PrimTy::Tuple(_) => "()",
                PrimTy::Ptr => "*",
            }
            .to_string(),

            Self::Adt(adt) => adt
                .name(db)
                .map(|i| i.data(db).to_string())
                .unwrap_or_else(|| "<unknown>".to_string()),

            Self::Contract(contract) => contract
                .name(db)
                .to_opt()
                .map(|i| i.data(db).to_string())
                .unwrap_or_else(|| "<unknown>".to_string()),

            Self::Func(func) => format!(
                "fn {}",
                func.name(db)
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string())
            ),
        }
    }

    pub(super) fn adt(self) -> Option<AdtDef<'db>> {
        match self {
            Self::Adt(adt) => Some(adt),
            _ => None,
        }
    }

    pub fn contract(self) -> Option<crate::hir_def::Contract<'db>> {
        match self {
            Self::Contract(c) => Some(c),
            _ => None,
        }
    }
}

impl From<HirPrimTy> for TyBase<'_> {
    fn from(hir_prim: HirPrimTy) -> Self {
        match hir_prim {
            HirPrimTy::Bool => Self::Prim(PrimTy::Bool),

            HirPrimTy::Int(int_ty) => match int_ty {
                HirIntTy::I8 => Self::Prim(PrimTy::I8),
                HirIntTy::I16 => Self::Prim(PrimTy::I16),
                HirIntTy::I32 => Self::Prim(PrimTy::I32),
                HirIntTy::I64 => Self::Prim(PrimTy::I64),
                HirIntTy::I128 => Self::Prim(PrimTy::I128),
                HirIntTy::I256 => Self::Prim(PrimTy::I256),
                HirIntTy::Isize => Self::Prim(PrimTy::Isize),
            },

            HirPrimTy::Uint(uint_ty) => match uint_ty {
                HirUintTy::U8 => Self::Prim(PrimTy::U8),
                HirUintTy::U16 => Self::Prim(PrimTy::U16),
                HirUintTy::U32 => Self::Prim(PrimTy::U32),
                HirUintTy::U64 => Self::Prim(PrimTy::U64),
                HirUintTy::U128 => Self::Prim(PrimTy::U128),
                HirUintTy::U256 => Self::Prim(PrimTy::U256),
                HirUintTy::Usize => Self::Prim(PrimTy::Usize),
            },

            HirPrimTy::String => Self::Prim(PrimTy::String),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub enum PrimTy {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    Isize,
    String,
    Array,
    Tuple(usize),
    Ptr,
}

impl PrimTy {
    pub fn is_integral(self) -> bool {
        matches!(
            self,
            Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::U128
                | Self::U256
                | Self::Usize
                | Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::I128
                | Self::I256
                | Self::Isize
        )
    }

    pub fn is_bool(self) -> bool {
        matches!(self, Self::Bool)
    }
}

/// Returns the width (in bits) for the given primitive integer type, or `None` when unknown.
pub fn prim_int_bits(prim: PrimTy) -> Option<usize> {
    use PrimTy::*;
    match prim {
        U8 | I8 => Some(8),
        U16 | I16 => Some(16),
        U32 | I32 => Some(32),
        U64 | I64 => Some(64),
        U128 | I128 => Some(128),
        U256 | I256 | Usize | Isize => Some(256),
        _ => None,
    }
}

pub(super) trait HasKind {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind;
}

impl HasKind for TyData<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyData::TyVar(ty_var) => ty_var.kind(db),
            TyData::TyParam(ty_param) => ty_param.kind.clone(),
            TyData::AssocTy(assoc) => assoc
                .trait_
                .def(db)
                .assoc_ty(db, assoc.name)
                .and_then(|decl| super::ty_lower::lower_kind_in_bounds(&decl.bounds))
                .unwrap_or(Kind::Star),
            TyData::QualifiedTy(_) => Kind::Star,
            TyData::TyBase(base) => base.kind(db),
            TyData::TyApp(abs, _) => match abs.kind(db) {
                // `TyId::app` method handles the kind mismatch, so we don't need to verify it again
                // here.
                Kind::Abs(inner) => inner.1.clone(),
                _ => Kind::Any,
            },

            TyData::ConstTy(const_ty) => const_ty.ty(db).kind(db).clone(),

            TyData::Never => Kind::Any,

            TyData::Invalid(_) => Kind::Any,
        }
    }
}

impl HasKind for TyVar<'_> {
    fn kind(&self, _db: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

impl HasKind for TyBase<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyBase::Prim(prim) => prim.kind(db),
            TyBase::Adt(adt) => adt.kind(db),
            TyBase::Contract(_) => Kind::Star, // Contracts have no generic params
            TyBase::Func(func) => func.kind(db),
        }
    }
}

impl HasKind for PrimTy {
    fn kind(&self, _: &dyn HirAnalysisDb) -> Kind {
        match self {
            Self::Array => (0..2).fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Tuple(n) => (0..*n).fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Ptr => Kind::abs(Kind::Star, Kind::Star),
            Self::String => Kind::abs(Kind::Star, Kind::Star),
            _ => Kind::Star,
        }
    }
}

impl HasKind for AdtDef<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(param.kind(db).clone(), kind);
        }

        kind
    }
}

impl HasKind for CallableDef<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(param.kind(db).clone(), kind);
        }

        kind
    }
}

pub(crate) fn collect_variables<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: &V,
) -> IndexSet<TyVar<'db>>
where
    V: TyVisitable<'db>,
{
    struct TyVarCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        vars: IndexSet<TyVar<'db>>,
    }

    impl<'db> TyVisitor<'db> for TyVarCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, var: &TyVar<'db>) {
            self.vars.insert(var.clone());
        }
    }
    let mut collector = TyVarCollector {
        db,
        vars: IndexSet::default(),
    };

    visitable.visit_with(&mut collector);

    collector.vars
}

pub(crate) fn inference_keys<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: &V,
) -> FxHashSet<InferenceKey<'db>>
where
    V: TyVisitable<'db>,
{
    struct FreeInferenceKeyCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        keys: FxHashSet<InferenceKey<'db>>,
    }

    impl<'db> TyVisitor<'db> for FreeInferenceKeyCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, var: &TyVar<'db>) {
            self.keys.insert(var.key);
        }
    }

    let mut collector = FreeInferenceKeyCollector {
        db,
        keys: FxHashSet::default(),
    };

    visitable.visit_with(&mut collector);
    collector.keys
}

fn pretty_print_ty_app<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> String {
    use PrimTy::*;
    use TyBase::*;

    let (base, args) = decompose_ty_app(db, ty);
    match base.data(db) {
        TyData::TyBase(Prim(Array)) => {
            let elem_ty = args[0].pretty_print(db);
            let len = args[1].pretty_print(db);
            format!("[{elem_ty}; {len}]")
        }

        TyData::TyBase(Prim(Tuple(_))) => {
            let mut args = args.iter();
            let mut s = ("(").to_string();
            if let Some(first) = args.next() {
                s.push_str(first.pretty_print(db));
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg.pretty_print(db));
                }
            }
            s.push(')');
            s
        }

        _ => {
            let mut s = (base.pretty_print(db)).to_string();

            let args_to_print: Vec<TyId<'db>> = match base.data(db) {
                TyData::TyBase(Func(func_def)) => {
                    let params = func_def.params(db);
                    args.iter()
                        .copied()
                        .enumerate()
                        .filter_map(|(idx, arg)| {
                            let is_provider = params.get(idx).is_some_and(|param_ty| {
                                matches!(param_ty.data(db), TyData::TyParam(p) if p.is_effect_provider())
                            });
                            (!is_provider).then_some(arg)
                        })
                        .collect()
                }
                _ => args.clone(),
            };

            let mut args_iter = args_to_print.iter();
            if let Some(first) = args_iter.next() {
                s.push('<');
                s.push_str(first.pretty_print(db));
                for arg in args_iter {
                    s.push_str(", ");
                    s.push_str(arg.pretty_print(db));
                }
                s.push('>');
            }
            s
        }
    }
}

/// Decompose type application into the base type and type arguments.
/// e.g., `App(App(T, U), App(V, W))` -> `(T, [U, App(V, W)])`
#[salsa::tracked(return_ref)]
pub(crate) fn decompose_ty_app<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> (TyId<'db>, Vec<TyId<'db>>) {
    struct TyAppDecomposer<'db> {
        db: &'db dyn HirAnalysisDb,
        base: Option<TyId<'db>>,
        args: Vec<TyId<'db>>,
    }

    impl<'db> TyVisitor<'db> for TyAppDecomposer<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            let db = self.db;

            match ty.data(db) {
                TyData::TyApp(lhs, rhs) => {
                    self.visit_ty(*lhs);
                    self.args.push(*rhs);
                }
                _ => self.base = Some(ty),
            }
        }
    }

    let mut decomposer = TyAppDecomposer {
        db,
        base: None,
        args: Vec::new(),
    };

    ty.visit_with(&mut decomposer);
    (decomposer.base.unwrap(), decomposer.args)
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TyFlags: u8 {
        const HAS_INVALID = 0b0000_0001;
        const HAS_VAR = 0b0000_0010;
        const HAS_PARAM = 0b0000_0100;
    }
}

#[salsa::tracked]
pub(crate) fn ty_flags<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyFlags {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        flags: TyFlags,
    }

    impl<'db> TyVisitor<'db> for Collector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, _: &TyVar) {
            self.flags.insert(TyFlags::HAS_VAR);
        }

        fn visit_param(&mut self, _: &TyParam) {
            self.flags.insert(TyFlags::HAS_PARAM)
        }

        fn visit_const_param(&mut self, _: &TyParam<'db>, _: TyId<'db>) {
            self.flags.insert(TyFlags::HAS_PARAM)
        }

        fn visit_invalid(&mut self, _: &InvalidCause) {
            self.flags.insert(TyFlags::HAS_INVALID);
        }
    }

    let mut collector = Collector {
        db,
        flags: TyFlags::empty(),
    };

    ty.visit_with(&mut collector);
    collector.flags
}
