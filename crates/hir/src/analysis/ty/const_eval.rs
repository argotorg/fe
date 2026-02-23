use num_bigint::BigUint;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, const_ty_from_trait_const},
        ctfe::{CtfeConfig, CtfeInterpreter},
        trait_resolution::TraitSolveCx,
        ty_check::ConstRef,
        ty_check::TypedBody,
        ty_def::{InvalidCause, TyData, TyId},
    },
};
use crate::core::hir_def::Body;
use crate::hir_def::ExprId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    Int(BigUint),
    Bool(bool),
    Bytes(Vec<u8>),
    EnumVariant(u16),
    /// A compile-time constant array whose elements are themselves `ConstValue`s.
    /// Byte serialization is deferred to MIR lowering where layout info is available.
    ConstArray(Vec<ConstValue>),
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
    mut expected_ty: TyId<'db>,
) -> Result<Option<ConstValue>, InvalidCause<'db>> {
    if let Some((_, inner)) = expected_ty.as_capability(db) {
        expected_ty = inner;
    }

    let const_ty = match cref {
        ConstRef::Const(const_def) => {
            let body = const_def
                .body(db)
                .to_opt()
                .ok_or(InvalidCause::ParseError)?;
            ConstTyId::from_body(db, body, None, Some(const_def))
        }
        ConstRef::TraitConst { inst, name } => {
            let solve_cx = TraitSolveCx::new(db, inst.def(db).top_mod(db).scope());
            const_ty_from_trait_const(db, solve_cx, inst, name).ok_or(InvalidCause::Other)?
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

    Ok(evaluated_const_to_value(db, const_ty))
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

    Ok(evaluated_const_to_value(db, const_ty))
}

/// Recursively converts an evaluated const type into a `ConstValue`.
pub fn evaluated_const_to_value<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
) -> Option<ConstValue> {
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(i), _) => {
            Some(ConstValue::Int(i.data(db).clone()))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(b), _) => Some(ConstValue::Bool(*b)),
        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => {
            Some(ConstValue::Bytes(bytes.clone()))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::EnumVariant(variant), _) => {
            Some(ConstValue::EnumVariant(variant.idx))
        }
        ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _) => {
            let values: Option<Vec<_>> = elems
                .iter()
                .map(|elem_ty| {
                    let TyData::ConstTy(elem_const) = elem_ty.data(db) else {
                        return None;
                    };
                    evaluated_const_to_value(db, *elem_const)
                })
                .collect();
            Some(ConstValue::ConstArray(values?))
        }
        _ => None,
    }
}
