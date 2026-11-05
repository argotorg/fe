use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SemConstId, SemConstScalar, SemConstValue, bool_const, bytes_const,
            consts::fixed_string_capacity_bytes, int_const, int_ty_shape, normalize_int_to_shape,
        },
        ty::ty_def::TyId,
    },
    hir_def::{ArithBinOp, UnOp, attr::ArithmeticMode},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PrimitiveFault {
    ArithmeticOverflow,
    DivisionByZero,
    NegativeExponent,
    InvalidPowerExponent,
    OutsideSupportedSubset,
    InvalidCast,
    UnsupportedCast,
}

pub(crate) fn int_in_range(db: &dyn HirAnalysisDb, result_ty: TyId<'_>, value: &BigInt) -> bool {
    let Some((bits, signed)) = int_ty_shape(db, result_ty) else {
        return true;
    };
    if signed {
        let half = BigInt::one() << (usize::from(bits) - 1);
        value >= &-half.clone() && value <= &(half - BigInt::one())
    } else {
        value >= &BigInt::zero() && value < &(BigInt::one() << usize::from(bits))
    }
}

pub(crate) fn execute_source_int_binary(
    db: &dyn HirAnalysisDb,
    result_ty: TyId<'_>,
    mode: ArithmeticMode,
    op: ArithBinOp,
    lhs: BigInt,
    rhs: BigInt,
) -> Result<BigInt, PrimitiveFault> {
    let value = match op {
        ArithBinOp::Add => lhs + rhs,
        ArithBinOp::Sub => lhs - rhs,
        ArithBinOp::Mul => lhs * rhs,
        ArithBinOp::Div => {
            if rhs.is_zero() {
                return Err(PrimitiveFault::DivisionByZero);
            }
            lhs / rhs
        }
        ArithBinOp::Rem => {
            if rhs.is_zero() {
                return Err(PrimitiveFault::DivisionByZero);
            }
            lhs % rhs
        }
        ArithBinOp::Pow => {
            if rhs.sign() == Sign::Minus {
                return Err(PrimitiveFault::NegativeExponent);
            }
            if mode == ArithmeticMode::Unchecked {
                lhs.pow(rhs.to_u32().ok_or(PrimitiveFault::InvalidPowerExponent)?)
            } else {
                let Some(mut exp) = rhs.to_biguint() else {
                    return Err(PrimitiveFault::NegativeExponent);
                };
                let mut acc = BigInt::one();
                let mut base = lhs;
                while !exp.is_zero() {
                    if (&exp & BigUint::one()) == BigUint::one() {
                        acc *= base.clone();
                        if !int_in_range(db, result_ty, &acc) {
                            return Err(PrimitiveFault::ArithmeticOverflow);
                        }
                    }
                    exp >>= 1usize;
                    if exp.is_zero() {
                        break;
                    }
                    base = base.clone() * base;
                    if !int_in_range(db, result_ty, &base) {
                        return Err(PrimitiveFault::ArithmeticOverflow);
                    }
                }
                acc
            }
        }
        ArithBinOp::Range
        | ArithBinOp::LShift
        | ArithBinOp::RShift
        | ArithBinOp::BitAnd
        | ArithBinOp::BitOr
        | ArithBinOp::BitXor => return Err(PrimitiveFault::OutsideSupportedSubset),
    };
    if mode == ArithmeticMode::Checked && !int_in_range(db, result_ty, &value) {
        Err(PrimitiveFault::ArithmeticOverflow)
    } else {
        Ok(value)
    }
}

pub(crate) fn execute_source_int_unary(
    db: &dyn HirAnalysisDb,
    result_ty: TyId<'_>,
    mode: ArithmeticMode,
    op: UnOp,
    value: BigInt,
) -> Result<BigInt, PrimitiveFault> {
    let value = match op {
        UnOp::Plus => value,
        UnOp::Minus => -value,
        UnOp::Not | UnOp::BitNot | UnOp::Mut | UnOp::MaybeMut | UnOp::Ref | UnOp::Deref => {
            return Err(PrimitiveFault::OutsideSupportedSubset);
        }
    };
    if mode == ArithmeticMode::Checked && !int_in_range(db, result_ty, &value) {
        Err(PrimitiveFault::ArithmeticOverflow)
    } else {
        Ok(value)
    }
}

/// Cast an immutable scalar with its source interpretation preserved. Both
/// ordered term forcing and the concrete machine use this operation.
pub(crate) fn execute_scalar_cast<'db>(
    db: &'db dyn HirAnalysisDb,
    result_ty: TyId<'db>,
    value: SemConstId<'db>,
) -> Result<SemConstId<'db>, PrimitiveFault> {
    match value.value(db) {
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(value),
            ..
        } if int_ty_shape(db, result_ty).is_some() => {
            Ok(int_const(db, result_ty, BigInt::from(u8::from(*value))))
        }
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } if result_ty == TyId::bool(db) => Ok(bool_const(db, !value.is_zero())),
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } if int_ty_shape(db, result_ty).is_some() => Ok(int_const(db, result_ty, value.clone())),
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } if result_ty.is_string(db) => {
            fixed_string_capacity_bytes(db, result_ty).ok_or(PrimitiveFault::UnsupportedCast)?;
            let word = normalize_int_to_shape(value.clone(), 256, false);
            let (_, bytes) = word.to_bytes_be();
            let mut padded = vec![0; 32 - bytes.len()];
            padded.extend(bytes);
            Ok(bytes_const(db, result_ty, padded))
        }
        SemConstValue::Scalar {
            value: SemConstScalar::Bytes(bytes),
            ..
        } if matches!(int_ty_shape(db, result_ty), Some((_, false))) => {
            let Some((bits, false)) = int_ty_shape(db, result_ty) else {
                unreachable!("unsigned cast must have an unsigned integer shape")
            };
            let width = usize::from(bits / 8);
            if bytes.len() > width && bytes[..bytes.len() - width].iter().any(|byte| *byte != 0) {
                return Err(PrimitiveFault::UnsupportedCast);
            }
            let suffix = &bytes[bytes.len().saturating_sub(width)..];
            Ok(int_const(
                db,
                result_ty,
                BigInt::from(BigUint::from_bytes_be(suffix)),
            ))
        }
        SemConstValue::Scalar {
            value: SemConstScalar::Bytes(bytes),
            ..
        } => Ok(bytes_const(db, result_ty, bytes.clone())),
        _ => Err(PrimitiveFault::InvalidCast),
    }
}
