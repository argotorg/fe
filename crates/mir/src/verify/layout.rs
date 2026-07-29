use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, Layout, LayoutId, RefKind, RefView, RuntimeClass, RuntimeProgramView,
        ScalarRole,
    },
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
        RuntimeClass::Ref {
            pointee,
            kind,
            view,
        } => {
            match view {
                RefView::Whole => {}
                RefView::EnumVariant(_) if pointee.aggregate_layout().is_some() => {}
                RefView::StorageLane(lane)
                    if matches!(
                        kind,
                        RefKind::Provider {
                            space: AddressSpaceKind::Storage | AddressSpaceKind::Transient,
                            ..
                        }
                    ) && matches!(
                        pointee.as_ref(),
                        RuntimeClass::Scalar(scalar)
                            if scalar.storage_bit_width() == lane.bit_width
                    ) && lane.bit_width > 0
                        && u32::from(lane.bit_offset) + u32::from(lane.bit_width)
                            <= u32::from(common::layout::WORD_SIZE_BITS) => {}
                RefView::EnumVariant(_) | RefView::StorageLane(_) => {
                    return Err(VerifyError::InvalidPlace(class.clone()));
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
            return Err(VerifyError::InvalidLayoutRefView(
                pointee.aggregate_layout().unwrap_or_else(|| {
                    panic!("variant ref view requires aggregate pointee: {pointee:?}")
                }),
            ));
        }
        RuntimeClass::Ref {
            view: RefView::StorageLane(_),
            ..
        } => return Err(VerifyError::InvalidPlace(class.clone())),
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => {}
    }
    verify_class_layouts(db, program, class, visited)
}
