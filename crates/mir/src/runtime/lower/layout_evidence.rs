use hir::analysis::ty::ty_def::TyId;

use crate::{
    db::MirDb,
    runtime::{ConstScalar, ScalarClass, ScalarRepr},
};

use super::type_info::{RuntimeTypeEnv, scalar_class_for_ty_in_env};

/// The runtime scalar class of a layout root whose const type is `ty`.
pub(crate) fn layout_root_scalar_class<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> ScalarClass<'db> {
    scalar_class_for_ty_in_env(db, env, ty)
        .unwrap_or_else(|| panic!("layout root must have a scalar const type: {ty:?}"))
}

pub(crate) fn layout_root_scalar_const(scalar: &ScalarClass<'_>, value: usize) -> ConstScalar {
    let ScalarRepr::Int { bits, signed } = scalar.repr else {
        panic!("layout root scalar must be an integer")
    };
    assert!(!signed, "layout root scalar must be unsigned");
    assert!(
        usize::BITS - value.leading_zeros() <= u32::from(bits),
        "layout root scalar cannot represent {value}"
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
