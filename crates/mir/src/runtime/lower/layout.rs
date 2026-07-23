use hir::{
    analysis::{semantic::FieldIndex, ty::ty_def::TyId},
    projection::IndexSource,
};

use crate::{
    db::MirDb,
    runtime::{
        ArrayLayout, EnumLayoutKey, EnumVariantLayout, Layout, LayoutId, LayoutKey, PlaceElem,
        RuntimeClass, StructLayout,
    },
};

use super::type_info::{RuntimeTypeEnv, runtime_repr_ty_in_env, stored_class_for_ty_in_env};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct AggregateCtorElem<'db> {
    pub(crate) elem: PlaceElem<'db>,
    pub(crate) class: RuntimeClass<'db>,
}

pub(crate) fn layout_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> LayoutId<'db> {
    let ty = runtime_repr_ty_in_env(db, env, ty);
    if ty.as_enum(db).is_some() {
        return LayoutId::new(db, LayoutKey::Enum(enum_layout_key(db, env, ty)));
    }
    if ty.is_array(db) {
        let (_, args) = ty.decompose_ty_app(db);
        let elem = args.first().copied().expect("array element type");
        return LayoutId::new(
            db,
            LayoutKey::Array(ArrayLayout {
                elem: stored_class_for_ty_in_env(db, env, elem),
                len: ty.array_len(db).expect("array length") as u64,
            }),
        );
    }
    LayoutId::new(
        db,
        LayoutKey::Struct(StructLayout {
            fields: ty
                .field_types(db)
                .into_iter()
                .map(|field| stored_class_for_ty_in_env(db, env, field))
                .collect(),
        }),
    )
}

pub(crate) fn layout_for_aggregate_instance_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
    field_classes: &[RuntimeClass<'db>],
) -> LayoutId<'db> {
    let ty = runtime_repr_ty_in_env(db, env, ty);
    if ty.as_enum(db).is_some() {
        panic!("aggregate instance layout requested for enum type");
    }
    if ty.is_array(db) {
        let (_, args) = ty.decompose_ty_app(db);
        let elem_ty = args.first().copied().expect("array element type");
        let len = ty.array_len(db).expect("array length");
        assert_eq!(
            len,
            field_classes.len(),
            "aggregate instance arity mismatch for array type {}",
            ty.pretty_print(db),
        );
        let elem = field_classes
            .first()
            .cloned()
            .unwrap_or_else(|| stored_class_for_ty_in_env(db, env, elem_ty));
        assert!(
            field_classes.iter().all(|class| class == &elem),
            "array aggregate instance requires homogeneous runtime element classes: ty={} field_classes={field_classes:?}",
            ty.pretty_print(db),
        );
        return LayoutId::new(
            db,
            LayoutKey::Array(ArrayLayout {
                elem,
                len: len as u64,
            }),
        );
    }
    assert_eq!(
        ty.field_types(db).len(),
        field_classes.len(),
        "aggregate instance arity mismatch for struct/tuple type {}",
        ty.pretty_print(db),
    );
    LayoutId::new(
        db,
        LayoutKey::Struct(StructLayout {
            fields: field_classes.to_vec().into_boxed_slice(),
        }),
    )
}

pub(crate) fn layout_for_enum_variant_instance_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    enum_ty: TyId<'db>,
    variant: usize,
    field_classes: &[RuntimeClass<'db>],
) -> LayoutId<'db> {
    let enum_ty = runtime_repr_ty_in_env(db, env, enum_ty);
    let enum_ = enum_ty
        .as_enum(db)
        .unwrap_or_else(|| panic!("enum instance layout requested for non-enum type"));
    let args = enum_ty.generic_args(db);
    LayoutId::new(
        db,
        LayoutKey::Enum(EnumLayoutKey {
            variants: enum_
                .variants(db)
                .enumerate()
                .map(|(idx, enum_variant)| {
                    let default_fields = enum_variant
                        .field_tys(db)
                        .into_iter()
                        .map(|field| {
                            stored_class_for_ty_in_env(db, env, field.instantiate(db, args))
                        })
                        .collect::<Vec<_>>();
                    let fields = if idx == variant {
                        assert_eq!(
                            default_fields.len(),
                            field_classes.len(),
                            "enum variant layout arity mismatch for {}::{}",
                            enum_ty.pretty_print(db),
                            enum_variant
                                .name(db)
                                .map(|name| name.data(db).to_string())
                                .unwrap_or_else(|| format!("variant_{idx}")),
                        );
                        field_classes.to_vec()
                    } else {
                        default_fields
                    };
                    EnumVariantLayout {
                        fields: fields.into(),
                    }
                })
                .collect(),
        }),
    )
}

pub(crate) fn aggregate_ctor_elems_for_layout<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    arity: usize,
) -> Box<[AggregateCtorElem<'db>]> {
    match layout.data(db) {
        Layout::Struct(layout) => {
            assert_eq!(
                layout.fields.len(),
                arity,
                "aggregate constructor arity mismatch for struct layout {layout:?}"
            );
            layout
                .fields
                .iter()
                .enumerate()
                .map(|(idx, class)| AggregateCtorElem {
                    elem: PlaceElem::Field(FieldIndex(idx as u16)),
                    class: class.clone(),
                })
                .collect()
        }
        Layout::Array(layout) => {
            assert_eq!(
                layout.len as usize, arity,
                "aggregate constructor arity mismatch for array layout {layout:?}"
            );
            (0..arity)
                .map(|idx| AggregateCtorElem {
                    elem: PlaceElem::Index(IndexSource::Constant(idx)),
                    class: layout.elem.clone(),
                })
                .collect()
        }
        Layout::Enum(_) => panic!("AggregateMake must not lower enum layouts"),
    }
}

fn enum_layout_key<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> EnumLayoutKey<'db> {
    let enum_ = ty.as_enum(db).expect("enum layout requested for non-enum");
    let args = ty.generic_args(db);
    EnumLayoutKey {
        variants: enum_
            .variants(db)
            .map(|variant| EnumVariantLayout {
                fields: variant
                    .field_tys(db)
                    .into_iter()
                    .map(|field| stored_class_for_ty_in_env(db, env, field.instantiate(db, args)))
                    .collect(),
            })
            .collect(),
    }
}
