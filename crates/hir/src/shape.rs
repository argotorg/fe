use common::origin::OriginExportKey;
use shape_address::{ShapeDimension, ShapeError, ShapeGraph, ShapeGraphKey, ShapeNodeKey};

use crate::origin::{
    HIR_EXPR_EXPORT_KIND, HIR_STMT_EXPORT_KIND, HirExprOrigin, HirOriginBodyOwnerKey, HirStmtOrigin,
};

pub const HIR_BODY_SHAPE_KIND: &str = "hir.body";

pub fn describe_hir_origin_shape<'db>(
    stable_body_key: &HirOriginBodyOwnerKey,
    exprs: impl IntoIterator<Item = HirExprOrigin<'db>>,
    stmts: impl IntoIterator<Item = HirStmtOrigin<'db>>,
) -> Result<ShapeGraph, ShapeError> {
    describe_hir_export_shape(
        stable_body_key,
        exprs
            .into_iter()
            .map(|origin| origin.export_key(stable_body_key)),
        stmts
            .into_iter()
            .map(|origin| origin.export_key(stable_body_key)),
    )
}

pub fn describe_hir_export_shape(
    stable_body_key: &HirOriginBodyOwnerKey,
    exprs: impl IntoIterator<Item = OriginExportKey>,
    stmts: impl IntoIterator<Item = OriginExportKey>,
) -> Result<ShapeGraph, ShapeError> {
    let body_key =
        OriginExportKey::try_from_raw_parts(HIR_BODY_SHAPE_KIND, stable_body_key.as_str(), "body")
            .expect("HIR body shape key must be valid");
    let body_node = ShapeNodeKey::entity(body_key.clone());
    let mut graph = ShapeGraph::new(ShapeGraphKey::new(body_key, "hir-origin-shape")?);
    graph.add_node(body_node.clone(), HIR_BODY_SHAPE_KIND)?;
    graph.add_field(&body_node, ShapeDimension::Structure, "phase", "hir")?;

    for (ordinal, key) in exprs.into_iter().enumerate() {
        let node = ShapeNodeKey::entity(key);
        graph.add_node(node.clone(), HIR_EXPR_EXPORT_KIND)?;
        graph.add_child(&body_node, "expr", ordinal as u32, &node)?;
    }
    for (ordinal, key) in stmts.into_iter().enumerate() {
        let node = ShapeNodeKey::entity(key);
        graph.add_node(node.clone(), HIR_STMT_EXPORT_KIND)?;
        graph.add_child(&body_node, "stmt", ordinal as u32, &node)?;
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
    fn hir_shape_uses_typed_expr_and_stmt_export_keys() {
        let owner = HirOriginBodyOwnerKey::new("pkg:demo:body:0");
        let graph = describe_hir_export_shape(
            &owner,
            [key(HIR_EXPR_EXPORT_KIND, owner.as_str(), "0")],
            [key(HIR_STMT_EXPORT_KIND, owner.as_str(), "0")],
        )
        .unwrap();

        assert_eq!(graph.nodes.len(), 3);
        assert!(graph.nodes.contains_key(&ShapeNodeKey::entity(key(
            HIR_EXPR_EXPORT_KIND,
            owner.as_str(),
            "0"
        ))));
        assert!(graph.nodes.contains_key(&ShapeNodeKey::entity(key(
            HIR_STMT_EXPORT_KIND,
            owner.as_str(),
            "0"
        ))));
    }
}
