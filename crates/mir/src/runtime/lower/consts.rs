use hir::analysis::ty::AddressSpace;

use hir::analysis::{
    semantic::{
        EvalOutcome, SemConstId, SemConstScalar, SemConstValue, SemanticConstRef, SemanticInstance,
        VariantIndex, eval_const_ref, normalize_int_to_shape, reify_runtime_const_for_ty,
        sem_const_ty,
    },
    ty::ty_def::TyId,
};

use crate::{
    db::MirDb,
    runtime::{
        ConstNode, ConstRegionId, ConstScalar, Layout, LayoutId, RuntimeClass, ScalarClass,
        ScalarRepr, synthetic::uint_scalar,
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
    match eval_const_ref(db, cref) {
        EvalOutcome::Ready(value) => value,
        outcome => {
            panic!("runtime constant was not validated before lowering: {cref:?}: {outcome:?}")
        }
    }
}

pub(super) fn reified_const_ref_value_for_ty<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    cref: SemanticConstRef<'db>,
    expected_ty: TyId<'db>,
) -> SemConstId<'db> {
    let value = evaluated_const_ref_value(db, cref);
    reify_runtime_const_for_ty(db, semantic, expected_ty, value)
        .expect("runtime constant reification was validated before lowering")
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
        .or_else(|| top_level_class_for_ty_in_env(db, env, ty, AddressSpace::Memory))
}

pub(crate) fn const_scalar_from_value<'db>(
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
        | SemConstValue::Enum { .. }
        | SemConstValue::Description(..) => None,
        SemConstValue::Scalar { value, .. } => match value {
            SemConstScalar::Bool(value) => Some(ConstScalar::Bool(*value)),
            SemConstScalar::Int { value } => {
                let scalar = scalar_class_for_ty_in_env(db, env, ty)?;
                match scalar.repr {
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
                }
            }
            SemConstScalar::Bytes(bytes) => {
                scalar_class_for_ty_in_env(db, env, ty).and_then(|scalar| {
                    matches!(scalar.repr, ScalarRepr::FixedBytes { .. })
                        .then(|| ConstScalar::FixedBytes(bytes.clone()))
                })
            }
        },
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
    let aggregate = match value.value(db) {
        SemConstValue::Tuple { .. }
        | SemConstValue::Struct { .. }
        | SemConstValue::Array { .. } => true,
        SemConstValue::Scalar {
            value: SemConstScalar::Bytes(_),
            ..
        } => ty.is_array(db),
        _ => false,
    };
    if !aggregate || runtime_zero_sized_ty(db, ty, env.scope, env.assumptions) {
        return None;
    }
    let node = lower_const_node(db, env, value)?;
    if !const_node_supports_data_ref(db, &node) || const_node_scalars(&node) == 1 {
        return None;
    }
    Some(RuntimeClass::const_ref(layout_for_ty_in_env(db, env, ty)))
}

/// The scalars a constant holds. A constant of one scalar, such as
/// `Id { v: 1 }`, `Outer { id: Id { v: 1 } }` or `(true,)`, is one word and as
/// cheap as the scalar itself. A const region would make it a pointer that
/// each non-inlined callee takes, in an instance of its own beside the one
/// that takes the value.
fn const_node_scalars(node: &ConstNode<'_>) -> usize {
    match node {
        ConstNode::Scalar(_) => 1,
        ConstNode::Aggregate { fields, .. } => fields.iter().map(const_node_scalars).sum(),
    }
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
        SemConstValue::Unit | SemConstValue::Scalar { .. } | SemConstValue::Description(..) => None,
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
            nodes.push(ConstNode::Scalar(enum_tag_scalar(db, layout, *variant)?));
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
    let ScalarRepr::Int {
        bits,
        signed: false,
    } = layout_data.tag.repr
    else {
        return None;
    };
    Some(uint_scalar(bits, u64::from(variant.0)))
}
