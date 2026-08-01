use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        ConstNode, ConstRegion, ConstRegionId, Layout, LayoutId, RuntimeClass, RuntimeProgramView,
    },
    verify::{VerifyError, layout::verify_layout},
};

pub fn verify_const_region<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    region: ConstRegion<'db>,
) -> Result<(), VerifyError<'db>> {
    verify_layout(db, program, region.layout, &mut FxHashSet::default())?;
    verify_const_node(db, program, region.layout, &region.value)
}

fn verify_const_node<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    expected_layout: LayoutId<'db>,
    node: &ConstNode<'db>,
) -> Result<(), VerifyError<'db>> {
    match node {
        ConstNode::Scalar(_) => Ok(()),
        ConstNode::Aggregate { layout, fields } => {
            if *layout != expected_layout {
                return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                    db,
                    expected_layout,
                    node.clone(),
                )));
            }
            match program.layout(*layout) {
                Layout::Struct(layout) => {
                    verify_const_fields(db, program, expected_layout, node, fields, &layout.fields)?
                }
                Layout::Array(layout) => {
                    if layout.len as usize != fields.len() {
                        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                            db,
                            expected_layout,
                            node.clone(),
                        )));
                    }
                    for field in fields {
                        verify_const_field(
                            db,
                            program,
                            expected_layout,
                            node,
                            field,
                            &layout.elem,
                        )?;
                    }
                }
                Layout::Enum(layout) => {
                    if layout.variants.is_empty() {
                        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                            db,
                            expected_layout,
                            node.clone(),
                        )));
                    }
                    let Some((tag, payload)) = fields.split_first() else {
                        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                            db,
                            expected_layout,
                            node.clone(),
                        )));
                    };
                    let Some(variant_idx) = enum_variant_index(tag) else {
                        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                            db,
                            expected_layout,
                            node.clone(),
                        )));
                    };
                    let Some(variant) = layout.variants.get(variant_idx) else {
                        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
                            db,
                            expected_layout,
                            node.clone(),
                        )));
                    };
                    verify_const_fields(
                        db,
                        program,
                        expected_layout,
                        node,
                        payload,
                        &variant.fields,
                    )?;
                }
            }
            Ok(())
        }
    }
}

fn verify_const_fields<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    expected_layout: LayoutId<'db>,
    node: &ConstNode<'db>,
    values: &[ConstNode<'db>],
    classes: &[RuntimeClass<'db>],
) -> Result<(), VerifyError<'db>> {
    if values.len() != classes.len() {
        return Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
            db,
            expected_layout,
            node.clone(),
        )));
    }
    for (value, class) in values.iter().zip(classes.iter()) {
        verify_const_field(db, program, expected_layout, node, value, class)?;
    }
    Ok(())
}

fn verify_const_field<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    expected_layout: LayoutId<'db>,
    node: &ConstNode<'db>,
    value: &ConstNode<'db>,
    class: &RuntimeClass<'db>,
) -> Result<(), VerifyError<'db>> {
    match class {
        RuntimeClass::Scalar(class) if matches!(value, ConstNode::Scalar(scalar) if scalar.fits_repr(class.repr)) => {
            Ok(())
        }
        RuntimeClass::Scalar(_) => Err(VerifyError::InvalidConstRegion(ConstRegionId::new(
            db,
            expected_layout,
            node.clone(),
        ))),
        RuntimeClass::AggregateValue { layout } => verify_const_node(db, program, *layout, value),
        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => Err(
            VerifyError::InvalidConstRegion(ConstRegionId::new(db, expected_layout, node.clone())),
        ),
    }
}

fn enum_variant_index(node: &ConstNode<'_>) -> Option<usize> {
    let ConstNode::Scalar(crate::runtime::ConstScalar::Int { words, .. }) = node else {
        return None;
    };
    Some(
        words
            .iter()
            .fold(0usize, |acc, byte| (acc << 8) | usize::from(*byte)),
    )
}
