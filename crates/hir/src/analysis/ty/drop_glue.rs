//! Which values the compiler destroys: a type's drop glue.

use super::{
    adt_def::AdtRef,
    corelib::resolve_core_trait,
    trait_def::{ImplementorId, impls_for_trait_def},
    trait_lower::lower_impl_trait,
    ty_def::{PrimTy, ShapeCon, TyBase, TyData, TyId},
};
use crate::{analysis::HirAnalysisDb, hir_def::Func};

/// Whether `func` is an implementation's `Drop::drop`.
pub fn implements_drop<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> bool {
    func.containing_impl_trait(db).is_some_and(|impl_trait| {
        lower_impl_trait(db, impl_trait).is_some_and(|implementor| {
            Some(implementor.trait_def(db))
                == resolve_core_trait(db, func.scope(), &["ops", "Drop"])
        })
    })
}

/// The `Drop` implementation of a struct or enum. It covers every value of
/// the type, so it lies in the type's own ingot.
#[salsa::tracked(returns(copy))]
pub fn adt_drop_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtRef<'db>,
) -> Option<ImplementorId<'db>> {
    let drop = resolve_core_trait(db, adt.scope(), &["ops", "Drop"])?;
    impls_for_trait_def(db, adt.scope().ingot(db), drop)
        .iter()
        .copied()
        .find(|implementor| {
            implementor
                .self_ty(db)
                .adt_def(db)
                .is_some_and(|def| def.adt_ref(db) == adt)
        })
}

fn ty_has_drop_glue_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: TyId<'db>,
) -> bool {
    false
}

/// Whether destroying a value of `ty` may run code: the value is, or holds, a
/// value whose type implements `Drop`. A type not yet known, such as a type
/// parameter, may. A view or a state-only type is never a value to destroy.
#[salsa::tracked(returns(copy), cycle_initial = ty_has_drop_glue_cycle_initial)]
pub fn ty_has_drop_glue<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if ty.is_view(db) || ty.pinned_part(db).is_some() {
        return false;
    }
    let any = |tys: &[TyId<'db>]| tys.iter().any(|&ty| ty_has_drop_glue(db, ty));
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => {
            let args = ty.generic_args(db);
            adt_drop_impl(db, adt.adt_ref(db)).is_some()
                || adt
                    .fields(db)
                    .iter()
                    .flat_map(|variant| variant.iter_types(db))
                    .any(|field| ty_has_drop_glue(db, field.instantiate(db, args)))
        }
        TyData::TyBase(TyBase::Prim(PrimTy::Array)) => any(&ty.generic_args(db)[..1]),
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_))) => any(ty.generic_args(db)),
        TyData::TyBase(TyBase::Closure(closure)) => any(closure.captures(db)),
        TyData::TyBase(TyBase::Prim(_) | TyBase::Contract(_) | TyBase::Func(_)) => false,
        // A value of a shape not yet known may hold anything, as a type
        // parameter may; the other shape constructors are not value types.
        TyData::TyBase(TyBase::Shape(ShapeCon::Value | ShapeCon::Carrier)) => true,
        TyData::TyBase(TyBase::Shape(_)) => false,
        TyData::TyVar(_) | TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_) => true,
        TyData::TyApp(..) | TyData::ConstTy(_) | TyData::Never | TyData::Invalid(_) => false,
    }
}
