use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        Layout, LayoutId, RawPointeeKey, RefKind, RefView, RuntimeClass, RuntimeProgramView,
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
        RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { pointee: None, .. } => Ok(()),
        // A stored target is a deferred source descriptor: validating it here
        // would unfold its whole referent closure. Dereferences validate the
        // one level they demand. Exact targets carry supplied structure that
        // used to sit inline, so they are still checked.
        RuntimeClass::RawAddr {
            pointee: Some(pointee),
            ..
        } => match pointee.key(db) {
            RawPointeeKey::Stored(_) => Ok(()),
            RawPointeeKey::Exact(target) => verify_class_layouts(db, program, target, visited),
        },
        RuntimeClass::AggregateValue { layout } => verify_layout(db, program, *layout, visited),
        RuntimeClass::Ref {
            pointee,
            view: RefView::StorageLane(_),
            kind,
        } => {
            // A storage lane is part of one word: only a provider reference to
            // a scalar can view one.
            if !matches!(**pointee, RuntimeClass::Scalar(_))
                || !matches!(kind, RefKind::Provider { .. })
            {
                return Err(VerifyError::InvalidPlace(class.clone()));
            }
            Ok(())
        }
        RuntimeClass::Ref { pointee, view, .. } => {
            if !matches!(view, RefView::Whole | RefView::EnumVariant(_)) {
                return Err(VerifyError::InvalidLayoutRefView(
                    pointee.aggregate_layout().unwrap_or_else(|| {
                        panic!("ref view should only appear on aggregate pointees: {pointee:?}")
                    }),
                ));
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
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => {}
    }
    verify_class_layouts(db, program, class, visited)
}
