use super::array::ArrayLength;
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            normalize::normalize_ty,
            trait_resolution::PredicateListId,
            ty_def::{ShapeCon, TyBase, TyData, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
};

/// Whether a value of `ty` holds a capability carrier, without following one
/// to its target. A pointer is data.
#[salsa::tracked(returns(copy), cycle_initial=contains_capability_cycle_initial)]
pub fn contains_capability<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> bool {
    let ty = normalize_ty(db, ty, scope, assumptions);
    if ty.as_capability(db).is_some() {
        return true;
    }
    if ty.is_array(db) {
        ArrayLength::from_ty(db, ty.generic_args(db)[1]) != Some(ArrayLength::Known(0))
            && contains_capability(db, scope, assumptions, ty.generic_args(db)[0])
    } else if ty.is_product(db) {
        ty.field_types(db)
            .into_iter()
            .any(|field| contains_capability(db, scope, assumptions, field))
    } else if let Some(adt) = ty.adt_def(db)
        && matches!(adt.adt_ref(db), AdtRef::Enum(_))
    {
        adt.fields(db).iter().enumerate().any(|(variant, fields)| {
            (0..fields.num_types()).any(|field| {
                let field_ty =
                    instantiate_adt_field_shape(db, adt, variant, field, ty.generic_args(db));
                contains_capability(db, scope, assumptions, field_ty)
            })
        })
    } else {
        false
    }
}

fn contains_capability_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: ScopeId<'db>,
    _: PredicateListId<'db>,
    _: TyId<'db>,
) -> bool {
    false
}

/// Whether a semantic-IR value of `ty` carries accesses: a carrier, a
/// projection's tuple or sum shape holding one, or the carrier of a shape
/// not known here, which may hold any.
pub fn holds_carrier<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let (base, args) = ty.decompose_ty_app(db);
    ty.as_capability(db).is_some()
        || matches!(
            base.data(db),
            TyData::TyBase(TyBase::Shape(ShapeCon::Carrier))
        )
        || args.iter().any(|arg| holds_carrier(db, *arg))
}
