use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use rustc_hash::FxHashSet;
use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{SemanticInstance, instantiate_with_generic_args},
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, evaluate_type_level_int_const_expr},
        ty_def::{PrimTy, TyBase, TyData, TyId, TyVarSort, prim_int_bits},
    },
};

use super::ir::VariantIndex;

#[salsa::interned]
#[derive(Debug)]
pub struct SemConstId<'db> {
    pub value: SemConstValue<'db>,
}

impl<'db> SemConstId<'db> {
    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.value(db) {
            SemConstValue::Unit => "()".to_string(),
            SemConstValue::Scalar { value, .. } => match value {
                SemConstScalar::Bool(value) => value.to_string(),
                SemConstScalar::Int { value } => value.to_string(),
                SemConstScalar::Bytes(bytes) => {
                    let hex = bytes
                        .iter()
                        .map(|byte| format!("{byte:02x}"))
                        .collect::<String>();
                    format!("0x{hex}")
                }
            },
            SemConstValue::TypeLevel { const_ty, .. } => const_ty.pretty_print(db).to_string(),
            SemConstValue::Tuple { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.pretty_print(db))
                    .collect::<Vec<_>>();
                if elems.len() == 1 {
                    format!("({},)", elems[0])
                } else {
                    format!("({})", elems.join(", "))
                }
            }
            SemConstValue::Struct { ty, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| field.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({fields})", ty.pretty_print(db))
            }
            SemConstValue::Array { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{elems}]")
            }
            SemConstValue::Enum {
                ty,
                variant,
                fields,
            } => {
                let variant_name = ty
                    .as_enum(db)
                    .and_then(|enum_| enum_.variants(db).nth(variant.0 as usize))
                    .and_then(|variant| variant.name(db))
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let fields = fields
                    .iter()
                    .map(|field| field.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(", ");
                if fields.is_empty() {
                    format!("{}::{variant_name}", ty.pretty_print(db))
                } else {
                    format!("{}::{variant_name}({fields})", ty.pretty_print(db))
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemConstValue<'db> {
    Unit,
    Scalar {
        ty: TyId<'db>,
        value: SemConstScalar,
    },
    TypeLevel {
        ty: TyId<'db>,
        const_ty: TyId<'db>,
    },
    Tuple {
        ty: TyId<'db>,
        elems: Box<[SemConstId<'db>]>,
    },
    Struct {
        ty: TyId<'db>,
        fields: Box<[SemConstId<'db>]>,
    },
    Array {
        ty: TyId<'db>,
        elems: Box<[SemConstId<'db>]>,
    },
    Enum {
        ty: TyId<'db>,
        variant: VariantIndex,
        fields: Box<[SemConstId<'db>]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemConstScalar {
    Bool(bool),
    Int { value: BigInt },
    Bytes(Vec<u8>),
}

pub fn sem_const_ty<'db>(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> TyId<'db> {
    match value.value(db) {
        SemConstValue::Unit => TyId::unit(db),
        SemConstValue::Scalar { ty, .. }
        | SemConstValue::TypeLevel { ty, .. }
        | SemConstValue::Tuple { ty, .. }
        | SemConstValue::Struct { ty, .. }
        | SemConstValue::Array { ty, .. }
        | SemConstValue::Enum { ty, .. } => ty,
    }
}

pub fn sem_const_eq<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: SemConstId<'db>,
    rhs: SemConstId<'db>,
) -> bool {
    fn fixed_string_capacity_bytes<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Option<usize> {
        if !ty.is_string(db) {
            return None;
        }
        let (_, args) = ty.decompose_ty_app(db);
        let len_ty = args.first().copied()?;
        let TyData::ConstTy(const_ty) = len_ty.data(db) else {
            return None;
        };
        match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                int_id.data(db).to_usize()
            }
            _ => None,
        }
    }

    fn fixed_string_runtime_bytes<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        bytes: &[u8],
    ) -> Vec<u8> {
        let Some(len) = fixed_string_capacity_bytes(db, ty) else {
            return bytes.to_vec();
        };
        let suffix = if bytes.len() > len {
            &bytes[bytes.len() - len..]
        } else {
            bytes
        };
        let mut out = vec![0u8; len];
        let offset = len - suffix.len();
        out[offset..].copy_from_slice(suffix);
        out
    }

    fn fixed_string_bytes_eq<'db>(
        db: &'db dyn HirAnalysisDb,
        lhs_ty: TyId<'db>,
        lhs_bytes: &[u8],
        rhs_ty: TyId<'db>,
        rhs_bytes: &[u8],
    ) -> bool {
        fn trim_leading_zeros(bytes: &[u8]) -> &[u8] {
            let mut idx = 0usize;
            while idx < bytes.len() && bytes[idx] == 0 {
                idx += 1;
            }
            &bytes[idx..]
        }

        let lhs_bytes = fixed_string_runtime_bytes(db, lhs_ty, lhs_bytes);
        let rhs_bytes = fixed_string_runtime_bytes(db, rhs_ty, rhs_bytes);
        trim_leading_zeros(&lhs_bytes) == trim_leading_zeros(&rhs_bytes)
    }

    fn is_string_like<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
        let ty = ty.as_capability(db).map(|(_, inner)| inner).unwrap_or(ty);
        if ty.is_string(db) {
            return true;
        }

        let base = ty.base_ty(db);
        matches!(
            base.data(db),
            TyData::TyVar(var) if matches!(var.sort, TyVarSort::String { .. })
        )
    }

    if lhs == rhs {
        return true;
    }
    match (lhs.value(db), rhs.value(db)) {
        (SemConstValue::Unit, SemConstValue::Unit) => true,
        (
            SemConstValue::Scalar {
                ty: lhs_ty,
                value: lhs_value,
            },
            SemConstValue::Scalar {
                ty: rhs_ty,
                value: rhs_value,
            },
        ) => {
            if is_string_like(db, lhs_ty)
                && is_string_like(db, rhs_ty)
                && let (SemConstScalar::Bytes(lhs_bytes), SemConstScalar::Bytes(rhs_bytes)) =
                    (&lhs_value, &rhs_value)
            {
                return fixed_string_bytes_eq(
                    db,
                    lhs_ty,
                    lhs_bytes.as_slice(),
                    rhs_ty,
                    rhs_bytes.as_slice(),
                );
            }
            if lhs_ty != rhs_ty {
                return false;
            }
            lhs_value == rhs_value
        }
        (
            SemConstValue::TypeLevel {
                ty: lhs_ty,
                const_ty: lhs_const_ty,
            },
            SemConstValue::TypeLevel {
                ty: rhs_ty,
                const_ty: rhs_const_ty,
            },
        ) => {
            lhs_ty == rhs_ty
                && if lhs_const_ty == rhs_const_ty {
                    true
                } else {
                    let lhs_value = sem_const_from_ty(db, lhs_const_ty);
                    let rhs_value = sem_const_from_ty(db, rhs_const_ty);
                    match (lhs_value, rhs_value) {
                        (Some(lhs_value), Some(rhs_value))
                            if !matches!(lhs_value.value(db), SemConstValue::TypeLevel { .. })
                                && !matches!(
                                    rhs_value.value(db),
                                    SemConstValue::TypeLevel { .. }
                                ) =>
                        {
                            sem_const_eq(db, lhs_value, rhs_value)
                        }
                        _ => false,
                    }
                }
        }
        (
            SemConstValue::Tuple {
                ty: lhs_ty,
                elems: lhs_elems,
            },
            SemConstValue::Tuple {
                ty: rhs_ty,
                elems: rhs_elems,
            },
        ) => {
            lhs_ty == rhs_ty
                && lhs_elems.len() == rhs_elems.len()
                && lhs_elems
                    .iter()
                    .copied()
                    .zip(rhs_elems.iter().copied())
                    .all(|(lhs, rhs)| sem_const_eq(db, lhs, rhs))
        }
        (
            SemConstValue::Struct {
                ty: lhs_ty,
                fields: lhs_fields,
            },
            SemConstValue::Struct {
                ty: rhs_ty,
                fields: rhs_fields,
            },
        )
        | (
            SemConstValue::Array {
                ty: lhs_ty,
                elems: lhs_fields,
            },
            SemConstValue::Array {
                ty: rhs_ty,
                elems: rhs_fields,
            },
        ) => {
            lhs_ty == rhs_ty
                && lhs_fields.len() == rhs_fields.len()
                && lhs_fields
                    .iter()
                    .copied()
                    .zip(rhs_fields.iter().copied())
                    .all(|(lhs, rhs)| sem_const_eq(db, lhs, rhs))
        }
        (
            SemConstValue::Enum {
                ty: lhs_ty,
                variant: lhs_variant,
                fields: lhs_fields,
            },
            SemConstValue::Enum {
                ty: rhs_ty,
                variant: rhs_variant,
                fields: rhs_fields,
            },
        ) => {
            lhs_ty == rhs_ty
                && lhs_variant == rhs_variant
                && lhs_fields.len() == rhs_fields.len()
                && lhs_fields
                    .iter()
                    .copied()
                    .zip(rhs_fields.iter().copied())
                    .all(|(lhs, rhs)| sem_const_eq(db, lhs, rhs))
        }
        _ => false,
    }
}

/// Demands the most concrete form of a type-level (symbolic) const value
/// under an instance's generic arguments: instantiates the carried const
/// type with the args, evaluates it at the value's expected type, and folds
/// integer const expressions. A second round covers structure exposed by the
/// first evaluation (e.g. a trait const that resolved to another symbolic
/// form mentioning instantiable params).
///
/// This is the single demand point for turning a `SemConstValue::TypeLevel`
/// payload concrete; const canonicalization, runtime reification, and the
/// CTFE machine's scalar reads all go through it. Returns `None` if the
/// payload is not a const type.
pub(crate) fn demand_concrete_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: TyId<'db>,
    expected: TyId<'db>,
    generic_args: &[TyId<'db>],
) -> Option<ConstTyId<'db>> {
    fn evaluate_and_fold<'db>(
        db: &'db dyn HirAnalysisDb,
        const_ty: ConstTyId<'db>,
        expected: TyId<'db>,
    ) -> ConstTyId<'db> {
        let evaluated = const_ty.evaluate(db, Some(expected));
        if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db)
            && let Some(concrete) = evaluate_type_level_int_const_expr(db, *expr, *expected_ty)
        {
            concrete
        } else {
            evaluated
        }
    }

    let instantiated = instantiate_with_generic_args(db, const_ty, generic_args);
    let TyData::ConstTy(const_ty) = instantiated.data(db) else {
        return None;
    };
    let mut evaluated = evaluate_and_fold(db, *const_ty, expected);
    if matches!(evaluated.data(db), ConstTyData::Abstract(..)) {
        let reinstantiated =
            instantiate_with_generic_args(db, TyId::const_ty(db, evaluated), generic_args);
        let TyData::ConstTy(reinstantiated) = reinstantiated.data(db) else {
            unreachable!("instantiating a const ty must yield a const ty");
        };
        evaluated = evaluate_and_fold(db, *reinstantiated, expected);
    }
    Some(evaluated)
}

pub fn sem_const_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<SemConstId<'db>> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return None;
    };

    match const_ty.data(db) {
        ConstTyData::Evaluated(value, expected_ty) => match value {
            EvaluatedConstTy::LitInt(int) => Some(int_const(
                db,
                *expected_ty,
                BigInt::from(int.data(db).clone()),
            )),
            EvaluatedConstTy::LitBool(value) => Some(bool_const(db, *value)),
            EvaluatedConstTy::Unit => Some(unit_const(db)),
            EvaluatedConstTy::Tuple(elems) => Some(tuple_const(
                db,
                *expected_ty,
                elems
                    .iter()
                    .map(|elem| sem_const_from_ty(db, *elem))
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )),
            EvaluatedConstTy::Array(elems) => Some(array_const(
                db,
                *expected_ty,
                elems
                    .iter()
                    .map(|elem| sem_const_from_ty(db, *elem))
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )),
            EvaluatedConstTy::Bytes(bytes) => Some(bytes_const(db, *expected_ty, bytes.clone())),
            EvaluatedConstTy::Record(fields) => Some(struct_const(
                db,
                *expected_ty,
                fields
                    .iter()
                    .map(|field| sem_const_from_ty(db, *field))
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )),
            EvaluatedConstTy::EnumVariant(variant) => Some(enum_const(
                db,
                *expected_ty,
                VariantIndex(variant.idx),
                Box::new([]),
            )),
            EvaluatedConstTy::Invalid => None,
        },
        ConstTyData::TyVar(_, value_ty)
        | ConstTyData::TyParam(_, value_ty)
        | ConstTyData::Hole(value_ty, _)
        | ConstTyData::Abstract(_, value_ty)
        | ConstTyData::UnEvaluated {
            ty: Some(value_ty), ..
        } => Some(SemConstId::new(
            db,
            SemConstValue::TypeLevel {
                ty: *value_ty,
                const_ty: ty,
            },
        )),
        ConstTyData::UnEvaluated { ty: None, .. } => None,
    }
}

#[salsa::tracked]
pub fn reify_runtime_const<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    value: SemConstId<'db>,
) -> Option<SemConstId<'db>> {
    reify_runtime_const_for_ty(db, instance, sem_const_ty(db, value), value)
}

#[salsa::tracked]
pub fn reify_runtime_const_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    expected_ty: TyId<'db>,
    value: SemConstId<'db>,
) -> Option<SemConstId<'db>> {
    reify_runtime_const_impl(db, instance, value, expected_ty)
}

fn reify_runtime_const_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    value: SemConstId<'db>,
    expected_ty: TyId<'db>,
) -> Option<SemConstId<'db>> {
    Some(match value.value(db) {
        SemConstValue::Unit => unit_const(db),
        SemConstValue::Scalar { ty, value } => {
            let ty = if ty.pretty_print(db) == "{integer}" {
                expected_ty
            } else {
                ty
            };
            match value {
                SemConstScalar::Bool(value) => bool_const(db, value),
                SemConstScalar::Int { value } => int_const(db, ty, value.clone()),
                SemConstScalar::Bytes(bytes) => bytes_const(db, ty, bytes.clone()),
            }
        }
        SemConstValue::TypeLevel { ty, const_ty } => {
            let ty = if ty.pretty_print(db) == "{integer}" {
                expected_ty
            } else {
                ty
            };
            let evaluated = demand_concrete_const_ty(
                db,
                const_ty,
                ty,
                instance.key(db).subst(db).generic_args(db),
            )?;
            let value = sem_const_from_ty(db, TyId::const_ty(db, evaluated))?;
            if matches!(value.value(db), SemConstValue::TypeLevel { .. }) {
                return None;
            }
            reify_runtime_const_impl(db, instance, value, ty)?
        }
        SemConstValue::Tuple { ty: _, elems } => {
            let ty = expected_ty;
            let field_tys = ty.field_types(db);
            if field_tys.len() != elems.len() {
                return None;
            }
            tuple_const(
                db,
                ty,
                elems
                    .iter()
                    .copied()
                    .zip(field_tys)
                    .map(|(elem, field_ty)| reify_runtime_const_impl(db, instance, elem, field_ty))
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )
        }
        SemConstValue::Struct { ty: _, fields } => {
            let ty = expected_ty;
            let field_tys = ty.field_types(db);
            if field_tys.len() != fields.len() {
                return None;
            }
            struct_const(
                db,
                ty,
                fields
                    .iter()
                    .copied()
                    .zip(field_tys)
                    .map(|(field, field_ty)| {
                        reify_runtime_const_impl(db, instance, field, field_ty)
                    })
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )
        }
        SemConstValue::Array { ty: _, elems } => {
            let ty = expected_ty;
            let (_, args) = ty.decompose_ty_app(db);
            let elem_ty = args.first().copied()?;
            array_const(
                db,
                ty,
                elems
                    .iter()
                    .copied()
                    .map(|elem| reify_runtime_const_impl(db, instance, elem, elem_ty))
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )
        }
        SemConstValue::Enum {
            ty: _,
            variant,
            fields,
        } => {
            let ty = expected_ty;
            let enum_ = ty.as_enum(db)?;
            let args = ty.generic_args(db);
            let field_tys = enum_
                .variants(db)
                .nth(variant.0 as usize)?
                .field_tys(db)
                .into_iter()
                .map(|field| field.instantiate(db, args))
                .collect::<Vec<_>>();
            if field_tys.len() != fields.len() {
                return None;
            }
            enum_const(
                db,
                ty,
                variant,
                fields
                    .iter()
                    .copied()
                    .zip(field_tys)
                    .map(|(field, field_ty)| {
                        reify_runtime_const_impl(db, instance, field, field_ty)
                    })
                    .collect::<Option<Vec<_>>>()?
                    .into_boxed_slice(),
            )
        }
    })
}

pub fn unit_const<'db>(db: &'db dyn HirAnalysisDb) -> SemConstId<'db> {
    SemConstId::new(db, SemConstValue::Unit)
}

pub fn bool_const<'db>(db: &'db dyn HirAnalysisDb, value: bool) -> SemConstId<'db> {
    SemConstId::new(
        db,
        SemConstValue::Scalar {
            ty: TyId::bool(db),
            value: SemConstScalar::Bool(value),
        },
    )
}

pub fn int_const<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> SemConstId<'db> {
    SemConstId::new(
        db,
        SemConstValue::Scalar {
            ty,
            value: SemConstScalar::Int {
                value: normalize_int(db, ty, value),
            },
        },
    )
}

pub fn bytes_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    bytes: Vec<u8>,
) -> SemConstId<'db> {
    SemConstId::new(
        db,
        SemConstValue::Scalar {
            ty,
            value: SemConstScalar::Bytes(bytes),
        },
    )
}

pub fn tuple_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    elems: Box<[SemConstId<'db>]>,
) -> SemConstId<'db> {
    SemConstId::new(db, SemConstValue::Tuple { ty, elems })
}

pub fn struct_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    fields: Box<[SemConstId<'db>]>,
) -> SemConstId<'db> {
    SemConstId::new(db, SemConstValue::Struct { ty, fields })
}

pub fn array_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    elems: Box<[SemConstId<'db>]>,
) -> SemConstId<'db> {
    SemConstId::new(db, SemConstValue::Array { ty, elems })
}

pub fn enum_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    variant: VariantIndex,
    fields: Box<[SemConstId<'db>]>,
) -> SemConstId<'db> {
    SemConstId::new(
        db,
        SemConstValue::Enum {
            ty,
            variant,
            fields,
        },
    )
}

pub fn scalar_bool(value: SemConstScalar) -> Option<bool> {
    let SemConstScalar::Bool(value) = value else {
        return None;
    };
    Some(value)
}

pub fn scalar_int(value: &SemConstScalar) -> Option<&BigInt> {
    let SemConstScalar::Int { value } = value else {
        return None;
    };
    Some(value)
}

pub fn normalize_int<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> BigInt {
    let Some((bits, signed)) = int_ty_shape(db, ty) else {
        return value;
    };
    normalize_int_to_shape(value, bits, signed)
}

pub fn normalize_int_to_shape(value: BigInt, bits: u16, signed: bool) -> BigInt {
    if bits == 0 {
        return BigInt::default();
    }

    let modulus = BigInt::from(1u8) << usize::from(bits);
    let mut normalized = value % &modulus;
    if normalized.sign() == Sign::Minus {
        normalized += &modulus;
    }
    if signed {
        let sign_bit = BigInt::from(1u8) << usize::from(bits - 1);
        if normalized >= sign_bit {
            normalized -= modulus;
        }
    }
    normalized
}

pub fn int_ty_shape<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<(u16, bool)> {
    let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) else {
        return None;
    };
    Some(match prim {
        PrimTy::U8 => (8, false),
        PrimTy::U16 => (16, false),
        PrimTy::U32 => (32, false),
        PrimTy::U64 => (64, false),
        PrimTy::U128 => (128, false),
        PrimTy::U256 | PrimTy::Usize => (256, false),
        PrimTy::I8 => (8, true),
        PrimTy::I16 => (16, true),
        PrimTy::I32 => (32, true),
        PrimTy::I64 => (64, true),
        PrimTy::I128 => (128, true),
        PrimTy::I256 | PrimTy::Isize => (256, true),
        PrimTy::Bool
        | PrimTy::String
        | PrimTy::Array
        | PrimTy::Tuple(_)
        | PrimTy::Ptr
        | PrimTy::View
        | PrimTy::BorrowMut
        | PrimTy::BorrowRef => return None,
    })
}

pub fn runtime_size_bytes<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    const WORD_SIZE_BYTES: usize = 32;
    const ENUM_TAG_SIZE_BYTES: usize = 1;

    fn array_len<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
        let (_, args) = ty.decompose_ty_app(db);
        let TyData::ConstTy(const_ty) = args.get(1)?.data(db) else {
            return None;
        };
        match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                int_id.data(db).to_usize()
            }
            _ => None,
        }
    }

    fn inner<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        visiting: &mut FxHashSet<TyId<'db>>,
    ) -> Option<usize> {
        if !visiting.insert(ty) {
            return None;
        }

        let result = if ty.has_invalid(db) || ty.has_var(db) {
            None
        } else if let TyData::TyParam(param) = ty.data(db)
            && (param.is_effect() || param.is_effect_provider() || param.is_trait_self())
        {
            Some(0)
        } else if ty.has_param(db) {
            None
        } else if ty.is_tuple(db) {
            ty.field_types(db)
                .into_iter()
                .try_fold(0usize, |size, field| {
                    Some(size + inner(db, field, visiting)?)
                })
        } else if matches!(
            ty.base_ty(db).data(db),
            TyData::TyBase(TyBase::Func(_) | TyBase::Contract(_))
        ) {
            Some(0)
        } else if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) {
            match prim {
                PrimTy::Bool => Some(1),
                PrimTy::String
                | PrimTy::Ptr
                | PrimTy::View
                | PrimTy::BorrowMut
                | PrimTy::BorrowRef => Some(WORD_SIZE_BYTES),
                PrimTy::Array | PrimTy::Tuple(_) => None,
                _ => prim_int_bits(*prim).map(|bits| bits / 8),
            }
        } else if ty.is_array(db) {
            let (_, args) = ty.decompose_ty_app(db);
            let elem = args.first().copied()?;
            let stride = inner(db, elem, visiting).unwrap_or(WORD_SIZE_BYTES);
            Some(array_len(db, ty)? * stride)
        } else if ty.is_struct(db) {
            ty.field_types(db)
                .into_iter()
                .try_fold(0usize, |size, field| {
                    Some(size + inner(db, field, visiting)?)
                })
        } else if let Some(enum_) = ty.as_enum(db) {
            let args = ty.generic_args(db);
            let max_payload = enum_
                .variants(db)
                .map(|variant| {
                    variant
                        .field_tys(db)
                        .into_iter()
                        .try_fold(0usize, |size, field| {
                            Some(size + inner(db, field.instantiate(db, args), visiting)?)
                        })
                })
                .collect::<Option<Vec<_>>>()?
                .into_iter()
                .max()
                .unwrap_or(0);
            Some(ENUM_TAG_SIZE_BYTES + max_payload)
        } else {
            None
        };

        visiting.remove(&ty);
        result
    }

    inner(db, ty, &mut FxHashSet::default())
}
