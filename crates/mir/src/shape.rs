use common::origin::OriginExportKey;
use shape_address::{ShapeDimension, ShapeError, ShapeGraph, ShapeGraphKey, ShapeNodeKey};

use crate::origin::{
    RUNTIME_LOCAL_EXPORT_KIND, RUNTIME_STMT_EXPORT_KIND, RUNTIME_TERMINATOR_EXPORT_KIND,
    RuntimeInstanceOwnerKey,
};

pub const RUNTIME_INSTANCE_SHAPE_KIND: &str = "runtime.instance";

pub fn describe_runtime_export_shape(
    stable_instance_key: &RuntimeInstanceOwnerKey,
    locals: impl IntoIterator<Item = OriginExportKey>,
    stmts: impl IntoIterator<Item = OriginExportKey>,
    terminators: impl IntoIterator<Item = OriginExportKey>,
) -> Result<ShapeGraph, ShapeError> {
    let instance_key = OriginExportKey::try_from_raw_parts(
        RUNTIME_INSTANCE_SHAPE_KIND,
        stable_instance_key.as_str(),
        "instance",
    )
    .expect("runtime instance shape key must be valid");
    let instance_node = ShapeNodeKey::entity(instance_key.clone());
    let mut graph = ShapeGraph::new(ShapeGraphKey::new(instance_key, "mir-runtime-shape")?);
    graph.add_node(instance_node.clone(), RUNTIME_INSTANCE_SHAPE_KIND)?;
    graph.add_field(&instance_node, ShapeDimension::Structure, "phase", "mir")?;

    for (ordinal, key) in locals.into_iter().enumerate() {
        let node = ShapeNodeKey::entity(key);
        graph.add_node(node.clone(), RUNTIME_LOCAL_EXPORT_KIND)?;
        graph.add_child(&instance_node, "local", ordinal as u32, &node)?;
    }
    for (ordinal, key) in stmts.into_iter().enumerate() {
        let node = ShapeNodeKey::entity(key);
        graph.add_node(node.clone(), RUNTIME_STMT_EXPORT_KIND)?;
        graph.add_child(&instance_node, "stmt", ordinal as u32, &node)?;
    }
    for (ordinal, key) in terminators.into_iter().enumerate() {
        let node = ShapeNodeKey::entity(key);
        graph.add_node(node.clone(), RUNTIME_TERMINATOR_EXPORT_KIND)?;
        graph.add_child(&instance_node, "terminator", ordinal as u32, &node)?;
    }
    Ok(graph)
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use super::*;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn runtime_shape_keeps_same_local_ids_separate_by_kind() {
        let owner = RuntimeInstanceOwnerKey::new("runtime-instance:demo");
        let graph = describe_runtime_export_shape(
            &owner,
            [key(RUNTIME_LOCAL_EXPORT_KIND, owner.as_str(), "local:0")],
            [key(
                RUNTIME_STMT_EXPORT_KIND,
                owner.as_str(),
                "block:0:stmt:0",
            )],
            [key(
                RUNTIME_TERMINATOR_EXPORT_KIND,
                owner.as_str(),
                "block:0:terminator",
            )],
        )
        .unwrap();

        assert_eq!(graph.nodes.len(), 4);
        assert_eq!(graph.children.len(), 3);
    }
}
