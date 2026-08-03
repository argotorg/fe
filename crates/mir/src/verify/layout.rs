use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{Layout, LayoutId, RefView, RuntimeClass, RuntimeProgramView, ScalarRole},
    verify::VerifyError,
};

pub(super) fn verify_class_layouts<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    class: &RuntimeClass<'db>,
    visited: &mut FxHashSet<LayoutId<'db>>,
) -> Result<(), VerifyError<'db>> {
    match class {
        RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => Ok(()),
        RuntimeClass::AggregateValue { layout } => verify_layout(db, program, *layout, visited),
        RuntimeClass::Ref { pointee, view, .. } => {
            if let RefView::EnumVariant(variant) = view {
                let Some(layout) = pointee.aggregate_layout() else {
                    return Err(VerifyError::InvalidPlace(class.clone()));
                };
                let Layout::Enum(enum_layout) = program.layout(layout) else {
                    return Err(VerifyError::InvalidLayoutRefView(layout));
                };
                if variant.enum_layout != layout {
                    return Err(VerifyError::InvalidLayoutRefView(layout));
                }
                if enum_layout
                    .variants
                    .get(usize::from(variant.index))
                    .is_none()
                {
                    return Err(VerifyError::InvalidVariant(layout, variant.index));
                }
            }
            if let Some(layout) = pointee.aggregate_layout() {
                verify_layout(db, program, layout, visited)?;
            }
            verify_class_layouts(db, program, pointee, visited)
        }
    }
}

pub(super) fn verify_layout<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    layout_id: LayoutId<'db>,
    visited: &mut FxHashSet<LayoutId<'db>>,
) -> Result<(), VerifyError<'db>> {
    if !visited.insert(layout_id) {
        return Ok(());
    }

    let result = match program.layout(layout_id) {
        Layout::Struct(layout) => layout
            .fields
            .iter()
            .try_for_each(|field| verify_stored_class(db, program, field, visited)),
        Layout::Array(layout) => verify_stored_class(db, program, &layout.elem, visited),
        Layout::Enum(layout) => {
            if !matches!(
                layout.tag.role,
                ScalarRole::EnumTag {
                    enum_layout: tag_layout
                } if tag_layout == layout_id
            ) {
                return Err(VerifyError::InvalidEnumTag(layout_id));
            }
            for variant in layout.variants.iter() {
                for field in variant.fields.iter() {
                    verify_stored_class(db, program, field, visited)?;
                }
            }
            Ok(())
        }
    };

    visited.remove(&layout_id);
    result
}

fn verify_stored_class<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    class: &RuntimeClass<'db>,
    visited: &mut FxHashSet<LayoutId<'db>>,
) -> Result<(), VerifyError<'db>> {
    match class {
        RuntimeClass::Ref {
            pointee,
            view: RefView::EnumVariant(_),
            ..
        } => {
            let Some(layout) = pointee.aggregate_layout() else {
                return Err(VerifyError::InvalidPlace(class.clone()));
            };
            return Err(VerifyError::InvalidLayoutRefView(layout));
        }
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => {}
    }
    verify_class_layouts(db, program, class, visited)
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;
    use rustc_hash::FxHashSet;

    use super::*;
    use crate::{
        db::MirDb,
        runtime::{
            EnumLayoutKey, EnumVariantLayout, LayoutKey, RefKind, ScalarClass, ScalarRepr,
            ScalarRole, StructLayout, VariantId,
        },
    };

    fn word_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    fn enum_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: vec![EnumVariantLayout {
                    fields: vec![word_class()].into(),
                }]
                .into(),
            }),
        )
    }

    fn struct_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                fields: vec![word_class()].into(),
            }),
        )
    }

    #[test]
    fn enum_variant_ref_view_must_name_its_pointee_layout() {
        let db = DriverDataBase::default();
        let pointee_layout = enum_layout(&db);
        let other_layout = struct_layout(&db);
        let class = RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue {
                layout: pointee_layout,
            }),
            kind: RefKind::Object,
            view: RefView::EnumVariant(VariantId {
                enum_layout: other_layout,
                index: 0,
            }),
        };
        let program: &dyn MirDb = &db;

        assert_eq!(
            verify_class_layouts(&db, &program, &class, &mut FxHashSet::default()),
            Err(VerifyError::InvalidLayoutRefView(pointee_layout)),
        );
    }

    #[test]
    fn enum_variant_ref_view_must_name_an_existing_enum_variant() {
        let db = DriverDataBase::default();
        let layout = enum_layout(&db);
        let invalid_index = 1;
        let class = RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue { layout }),
            kind: RefKind::Object,
            view: RefView::EnumVariant(VariantId {
                enum_layout: layout,
                index: invalid_index,
            }),
        };
        let program: &dyn MirDb = &db;

        assert_eq!(
            verify_class_layouts(&db, &program, &class, &mut FxHashSet::default()),
            Err(VerifyError::InvalidVariant(layout, invalid_index)),
        );

        let struct_layout = struct_layout(&db);
        let class = RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue {
                layout: struct_layout,
            }),
            kind: RefKind::Object,
            view: RefView::EnumVariant(VariantId {
                enum_layout: struct_layout,
                index: 0,
            }),
        };
        assert_eq!(
            verify_class_layouts(&db, &program, &class, &mut FxHashSet::default()),
            Err(VerifyError::InvalidLayoutRefView(struct_layout)),
        );
    }

    #[test]
    fn stored_enum_variant_ref_view_with_scalar_pointee_is_rejected_without_panicking() {
        let db = DriverDataBase::default();
        let enum_layout = enum_layout(&db);
        let invalid_field = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Object,
            view: RefView::EnumVariant(VariantId {
                enum_layout,
                index: 0,
            }),
        };
        let layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: vec![invalid_field.clone()].into(),
            }),
        );
        let program: &dyn MirDb = &db;

        assert_eq!(
            verify_layout(&db, &program, layout, &mut FxHashSet::default()),
            Err(VerifyError::InvalidPlace(invalid_field)),
        );
    }
}
