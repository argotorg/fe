use num_bigint::BigUint;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, const_ty_from_trait_const},
        ctfe::{CtfeConfig, CtfeInterpreter},
        ty_check::ConstRef,
        ty_check::TypedBody,
        ty_def::{InvalidCause, TyId},
    },
};
use crate::core::hir_def::Body;
use crate::hir_def::ExprId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    Int(BigUint),
    Bool(bool),
    Bytes(Vec<u8>),
}

pub fn try_eval_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstValue> {
    let const_ty = ConstTyId::from_body(db, body, Some(expected_ty), None);
    eval_const_ty(db, const_ty, Some(expected_ty))
        .ok()
        .flatten()
}

pub fn try_eval_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: ConstRef<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstValue> {
    eval_const_ref(db, cref, expected_ty).ok().flatten()
}

pub fn eval_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: ConstRef<'db>,
    expected_ty: TyId<'db>,
) -> Result<Option<ConstValue>, InvalidCause<'db>> {
    let const_ty = match cref {
        ConstRef::Const(const_def) => {
            let body = const_def
                .body(db)
                .to_opt()
                .ok_or(InvalidCause::ParseError)?;
            ConstTyId::from_body(db, body, None, Some(const_def))
        }
        ConstRef::TraitConst { inst, name } => {
            const_ty_from_trait_const(db, inst, name).ok_or(InvalidCause::Other)?
        }
    };
    eval_const_ty(db, const_ty, Some(expected_ty))
}

pub fn eval_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &TypedBody<'db>,
    generic_args: &[TyId<'db>],
    expr: ExprId,
) -> Result<Option<ConstValue>, InvalidCause<'db>> {
    let mut interp = CtfeInterpreter::new(db, CtfeConfig::default());
    let const_ty =
        interp.eval_expr_in_body(body, typed_body.clone(), generic_args.to_vec(), expr)?;

    Ok(match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(i), _) => {
            Some(ConstValue::Int(i.data(db).clone()))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(b), _) => Some(ConstValue::Bool(*b)),
        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => {
            Some(ConstValue::Bytes(bytes.clone()))
        }
        _ => None,
    })
}

fn eval_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> Result<Option<ConstValue>, InvalidCause<'db>> {
    let const_ty = const_ty.evaluate(db, expected_ty);
    if let Some(cause) = const_ty.ty(db).invalid_cause(db) {
        return Err(cause);
    }

    Ok(match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(i), _) => {
            Some(ConstValue::Int(i.data(db).clone()))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(b), _) => Some(ConstValue::Bool(*b)),
        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => {
            Some(ConstValue::Bytes(bytes.clone()))
        }
        _ => None,
    })
}
