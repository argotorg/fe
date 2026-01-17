use num_bigint::BigUint;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, const_ty_from_trait_const},
        ty_check::ConstRef,
        ty_def::TyId,
    },
};
use crate::core::hir_def::Body;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    Int(BigUint),
    Bool(bool),
}

pub fn try_eval_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstValue> {
    let const_ty = ConstTyId::from_body(db, body, Some(expected_ty), None);
    try_eval_const_ty(db, const_ty, Some(expected_ty))
}

pub fn try_eval_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: ConstRef<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstValue> {
    let const_ty = match cref {
        ConstRef::Const(const_def) => {
            let body = const_def.body(db).to_opt()?;
            ConstTyId::from_body(db, body, None, Some(const_def))
        }
        ConstRef::TraitConst { inst, name } => const_ty_from_trait_const(db, inst, name)?,
    };
    try_eval_const_ty(db, const_ty, Some(expected_ty))
}

fn try_eval_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> Option<ConstValue> {
    let const_ty = const_ty.evaluate(db, expected_ty);
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(i), _) => {
            Some(ConstValue::Int(i.data(db).clone()))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(b), _) => Some(ConstValue::Bool(*b)),
        _ => None,
    }
}
