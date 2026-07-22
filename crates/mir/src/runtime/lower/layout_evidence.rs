use hir::analysis::ty::{LayoutMapTy, ty_def::TyId};

use crate::{
    db::MirDb,
    runtime::{ConstScalar, RuntimeLayoutMap, ScalarRepr},
};

use super::type_info::{RuntimeTypeEnv, scalar_class_for_ty_in_env};

pub(crate) fn runtime_layout_map_for_shape<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    scalar_ty: TyId<'db>,
    dimensions: &[usize],
) -> RuntimeLayoutMap<'db> {
    let scalar = scalar_class_for_ty_in_env(db, env, scalar_ty)
        .unwrap_or_else(|| panic!("layout evidence must have a scalar const type: {scalar_ty:?}"));
    RuntimeLayoutMap::new(scalar_ty, scalar, dimensions.to_vec())
}

pub(crate) fn runtime_layout_map_for_map_ty<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    map_ty: &LayoutMapTy<'db>,
) -> RuntimeLayoutMap<'db> {
    runtime_layout_map_for_shape(db, env, map_ty.scalar_ty, &map_ty.dimensions)
}

pub(crate) fn runtime_layout_scalar_const(map: &RuntimeLayoutMap<'_>, value: usize) -> ConstScalar {
    let ScalarRepr::Int { bits, signed } = map.scalar().repr else {
        panic!("layout-map scalar must be an integer")
    };
    assert!(!signed, "layout-map scalar must be unsigned");
    assert!(
        usize::BITS - value.leading_zeros() <= u32::from(bits),
        "layout-map scalar cannot represent {value}"
    );
    ConstScalar::Int {
        bits,
        signed,
        words: if value == 0 {
            Vec::new()
        } else {
            value
                .to_be_bytes()
                .into_iter()
                .skip_while(|byte| *byte == 0)
                .collect()
        },
    }
}
