use std::collections::HashSet;

use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        NExpr, SConst, SemConstId, SemConstScalar, SemConstValue, SemanticConstRef,
        SemanticInstance, VariantIndex,
        borrowck::{NSStmtKind, NormalizedSemanticBody},
        eval_const_ref, normalize_int_to_shape, reify_runtime_const_for_ty, sem_const_ty,
    },
    ty::const_ty::{ConstTyData, EvaluatedConstTy, evaluate_type_level_int_const_expr},
    ty::ty_def::{TyData, TyId},
};

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ConstNode, ConstRegionId, ConstScalar, Layout, LayoutId, RuntimeClass,
        ScalarClass, ScalarRepr,
    },
};

use super::{
    layout::layout_for_ty_in_env,
    type_info::{
        RuntimeTypeEnv, runtime_zero_sized_ty, scalar_class_for_ty_in_env,
        top_level_class_for_ty_in_env,
    },
};

pub(super) fn evaluated_const_ref_value<'db>(
    db: &'db dyn MirDb,
    cref: SemanticConstRef<'db>,
) -> SemConstId<'db> {
    eval_const_ref(db, cref).unwrap_or_else(|err| panic!("CTFE failed for {cref:?}: {err:?}"))
}

pub(super) fn reified_const_ref_value_for_ty<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    cref: SemanticConstRef<'db>,
    expected_ty: TyId<'db>,
) -> SemConstId<'db> {
    let value = evaluated_const_ref_value(db, cref);
    reify_runtime_const_for_ty(db, semantic, expected_ty, value).unwrap_or(value)
}

pub(super) fn collect_const_ref_regions<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    body: &NormalizedSemanticBody<'db>,
) -> HashSet<ConstRegionId<'db>> {
    body.blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .filter_map(|stmt| {
            let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                return None;
            };
            let NExpr::Const(SConst::Ref(cref)) = expr else {
                return None;
            };
            aggregate_const_ref_region(
                db,
                env,
                reified_const_ref_value_for_ty(db, body.owner, *cref, body.locals[dst.index()].ty),
            )
        })
        .collect()
}

pub(super) fn runtime_const_value_class<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
    allow_const_ref_storage: bool,
) -> Option<RuntimeClass<'db>> {
    let ty = sem_const_ty(db, value);
    if ty == TyId::unit(db) {
        return None;
    }
    allow_const_ref_storage
        .then(|| aggregate_const_ref_class(db, env, value))
        .flatten()
        .or_else(|| top_level_class_for_ty_in_env(db, env, ty, AddressSpaceKind::Memory))
}

pub(super) fn const_scalar_from_value<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
) -> Option<ConstScalar> {
    let ty = sem_const_ty(db, value);
    match value.value(db) {
        SemConstValue::Unit
        | SemConstValue::Tuple { .. }
        | SemConstValue::Struct { .. }
        | SemConstValue::Array { .. }
        | SemConstValue::Enum { .. } => None,
        SemConstValue::Scalar { value, .. } => match value {
            SemConstScalar::Bool(value) => Some(ConstScalar::Bool(value)),
            SemConstScalar::Int { value } => {
                let scalar = scalar_class_for_ty_in_env(db, env, ty)?;
                match scalar.repr {
                    ScalarRepr::Bool => None,
                    ScalarRepr::Int { bits, signed } => Some(ConstScalar::Int {
                        bits,
                        signed,
                        words: encode_int_words(&value, bits, signed),
                    }),
                    ScalarRepr::FixedBytes { .. } => None,
                    ScalarRepr::Address { bits } => Some(ConstScalar::Address {
                        bits,
                        bytes: encode_int_words(&value, bits, false),
                    }),
                }
            }
            SemConstScalar::Bytes(bytes) => {
                scalar_class_for_ty_in_env(db, env, ty).and_then(|scalar| {
                    matches!(scalar.repr, ScalarRepr::FixedBytes { .. })
                        .then(|| ConstScalar::FixedBytes(bytes.clone()))
                })
            }
        },
        SemConstValue::TypeLevel { ty, const_ty } => {
            let TyData::ConstTy(const_ty) = const_ty.data(db) else {
                return None;
            };
            let evaluated = const_ty.evaluate(db, Some(ty));
            let evaluated = if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db) {
                evaluate_type_level_int_const_expr(db, *expr, *expected_ty).unwrap_or(evaluated)
            } else {
                evaluated
            };
            match evaluated.data(db) {
                ConstTyData::Evaluated(EvaluatedConstTy::LitBool(value), _) => {
                    Some(ConstScalar::Bool(*value))
                }
                ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                    let value = num_bigint::BigInt::from(int_id.data(db).clone());
                    let scalar = scalar_class_for_ty_in_env(db, env, ty)?;
                    match scalar.repr {
                        ScalarRepr::Bool => None,
                        ScalarRepr::Int { bits, signed } => Some(ConstScalar::Int {
                            bits,
                            signed,
                            words: encode_int_words(&value, bits, signed),
                        }),
                        ScalarRepr::FixedBytes { .. } => None,
                        ScalarRepr::Address { bits } => Some(ConstScalar::Address {
                            bits,
                            bytes: encode_int_words(&value, bits, false),
                        }),
                    }
                }
                ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => {
                    scalar_class_for_ty_in_env(db, env, ty).and_then(|scalar| {
                        matches!(scalar.repr, ScalarRepr::FixedBytes { .. })
                            .then(|| ConstScalar::FixedBytes(bytes.clone()))
                    })
                }
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Unit
                    | EvaluatedConstTy::Tuple(_)
                    | EvaluatedConstTy::Array(_)
                    | EvaluatedConstTy::Record(_)
                    | EvaluatedConstTy::EnumVariant(_)
                    | EvaluatedConstTy::Invalid,
                    _,
                )
                | ConstTyData::TyVar(_, _)
                | ConstTyData::TyParam(_, _)
                | ConstTyData::Hole(_, _)
                | ConstTyData::Abstract(_, _)
                | ConstTyData::UnEvaluated { .. } => None,
            }
        }
    }
}

pub(super) fn const_scalar_for_class(
    value: &SemConstScalar,
    class: &ScalarClass<'_>,
) -> Option<ConstScalar> {
    match value {
        SemConstScalar::Bool(value) => {
            matches!(class.repr, ScalarRepr::Bool).then_some(ConstScalar::Bool(*value))
        }
        SemConstScalar::Int { value } => match class.repr {
            ScalarRepr::Bool => None,
            ScalarRepr::Int { bits, signed } => Some(ConstScalar::Int {
                bits,
                signed,
                words: encode_int_words(value, bits, signed),
            }),
            ScalarRepr::FixedBytes { .. } => None,
            ScalarRepr::Address { bits } => Some(ConstScalar::Address {
                bits,
                bytes: encode_int_words(value, bits, false),
            }),
        },
        SemConstScalar::Bytes(bytes) => matches!(class.repr, ScalarRepr::FixedBytes { .. })
            .then(|| ConstScalar::FixedBytes(bytes.clone())),
    }
}

fn encode_int_words(value: &num_bigint::BigInt, bits: u16, signed: bool) -> Vec<u8> {
    let normalized = normalize_int_to_shape(value.clone(), bits, signed);
    let unsigned = normalize_int_to_shape(normalized, bits, false);
    let (_, bytes) = unsigned.to_bytes_be();
    bytes
}

pub(super) fn lower_const_region<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
) -> Option<ConstRegionId<'db>> {
    let ty = sem_const_ty(db, value);
    if runtime_zero_sized_ty(db, ty, env.scope, env.assumptions) {
        return None;
    }
    let layout = layout_for_ty_in_env(db, env, ty);
    let value = lower_const_node(db, env, value)?;
    Some(ConstRegionId::new(db, layout, value))
}

pub(super) fn aggregate_const_ref_class<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
) -> Option<RuntimeClass<'db>> {
    let ty = sem_const_ty(db, value);
    if runtime_zero_sized_ty(db, ty, env.scope, env.assumptions)
        || !matches!(
            value.value(db),
            SemConstValue::Tuple { .. }
                | SemConstValue::Struct { .. }
                | SemConstValue::Array { .. }
        )
    {
        return None;
    }
    let node = lower_const_node(db, env, value)?;
    if !const_node_supports_data_ref(db, &node) {
        return None;
    }
    Some(RuntimeClass::const_ref(layout_for_ty_in_env(db, env, ty)))
}

pub(super) fn aggregate_const_ref_region<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
) -> Option<ConstRegionId<'db>> {
    aggregate_const_ref_class(db, env, value)?;
    lower_const_region(db, env, value)
}

fn const_node_supports_data_ref<'db>(db: &'db dyn MirDb, node: &ConstNode<'db>) -> bool {
    match node {
        ConstNode::Scalar(_) => true,
        ConstNode::Aggregate { layout, fields } => {
            !matches!(layout.data(db), Layout::Enum(_))
                && fields
                    .iter()
                    .all(|field| const_node_supports_data_ref(db, field))
        }
    }
}

fn lower_const_node<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    value: SemConstId<'db>,
) -> Option<ConstNode<'db>> {
    if let Some(scalar) = const_scalar_from_value(db, env, value) {
        return Some(ConstNode::Scalar(scalar));
    }
    let ty = sem_const_ty(db, value);
    if let SemConstValue::Scalar {
        value: SemConstScalar::Bytes(bytes),
        ..
    } = value.value(db)
        && ty.is_array(db)
    {
        return Some(ConstNode::Aggregate {
            layout: layout_for_ty_in_env(db, env, ty),
            fields: bytes
                .iter()
                .map(|byte| {
                    ConstNode::Scalar(ConstScalar::Int {
                        bits: 8,
                        signed: false,
                        words: if *byte == 0 { Vec::new() } else { vec![*byte] },
                    })
                })
                .collect(),
        });
    }
    match value.value(db) {
        SemConstValue::Unit | SemConstValue::Scalar { .. } | SemConstValue::TypeLevel { .. } => {
            None
        }
        SemConstValue::Tuple { elems, .. }
        | SemConstValue::Struct { fields: elems, .. }
        | SemConstValue::Array { elems, .. } => Some(ConstNode::Aggregate {
            layout: layout_for_ty_in_env(db, env, ty),
            fields: elems
                .iter()
                .copied()
                .map(|value| lower_const_node(db, env, value))
                .collect::<Option<Vec<_>>>()?
                .into_boxed_slice(),
        }),
        SemConstValue::Enum {
            variant, fields, ..
        } => {
            let layout = layout_for_ty_in_env(db, env, ty);
            let mut nodes = Vec::with_capacity(fields.len() + 1);
            nodes.push(ConstNode::Scalar(enum_tag_scalar(db, layout, variant)?));
            nodes.extend(
                fields
                    .iter()
                    .copied()
                    .map(|field| lower_const_node(db, env, field))
                    .collect::<Option<Vec<_>>>()?,
            );
            Some(ConstNode::Aggregate {
                layout,
                fields: nodes.into_boxed_slice(),
            })
        }
    }
}

pub(super) fn enum_tag_scalar<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    variant: VariantIndex,
) -> Option<ConstScalar> {
    let crate::runtime::Layout::Enum(layout_data) = layout.data(db) else {
        return None;
    };
    let ScalarRepr::Int { bits, signed } = layout_data.tag.repr else {
        return None;
    };
    Some(ConstScalar::Int {
        bits,
        signed,
        words: if variant.0 == 0 {
            Vec::new()
        } else {
            vec![variant.0 as u8]
        },
    })
}
