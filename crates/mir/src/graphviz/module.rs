use dot2::{label::Text, GraphWalk, Id, Kind, Labeller};
use fe_analyzer::namespace::items::ModuleId;

use crate::{
    db::MirDb,
    ir::{inst::BranchInfo, FunctionId},
    pretty_print::PrettyPrint,
};

use super::{block::BlockNode, function::FunctionNode};

pub(super) struct ModuleGraph<'db> {
    db: &'db dyn MirDb,
    module: ModuleId,
}

impl<'db> ModuleGraph<'db> {
    pub(super) fn new(db: &'db dyn MirDb, module: ModuleId) -> Self {
        Self { db, module }
    }
}

impl<'db> GraphWalk<'db> for ModuleGraph<'db> {
    type Node = BlockNode;
    type Edge = ModuleGraphEdge;
    type Subgraph = FunctionNode;

    fn nodes(&self) -> dot2::Nodes<'db, Self::Node> {
        let mut nodes = Vec::new();

        // Collect function nodes.
        for func in self
            .db
            .mir_lower_module_all_functions(self.module)
            .iter()
            .map(|id| FunctionNode::new(*id))
        {
            nodes.extend(func.blocks(self.db).into_iter())
        }

        nodes.into()
    }

    fn edges(&self) -> dot2::Edges<'db, Self::Edge> {
        let mut edges = vec![];
        for func in self.db.mir_lower_module_all_functions(self.module).iter() {
            for block in FunctionNode::new(*func).blocks(self.db) {
                for succ in block.succs(self.db) {
                    let edge = ModuleGraphEdge {
                        from: block,
                        to: succ,
                        func: *func,
                    };
                    edges.push(edge);
                }
            }
        }

        edges.into()
    }

    fn source(&self, edge: &Self::Edge) -> Self::Node {
        edge.from
    }

    fn target(&self, edge: &Self::Edge) -> Self::Node {
        edge.to
    }

    fn subgraphs(&self) -> dot2::Subgraphs<'db, Self::Subgraph> {
        self.db
            .mir_lower_module_all_functions(self.module)
            .iter()
            .map(|id| FunctionNode::new(*id))
            .collect::<Vec<_>>()
            .into()
    }

    fn subgraph_nodes(&self, s: &Self::Subgraph) -> dot2::Nodes<'db, Self::Node> {
        s.blocks(self.db).into_iter().collect::<Vec<_>>().into()
    }
}

impl<'db> Labeller<'db> for ModuleGraph<'db> {
    type Node = BlockNode;
    type Edge = ModuleGraphEdge;
    type Subgraph = FunctionNode;

    fn graph_id(&self) -> dot2::Result<Id<'db>> {
        let module_name = self.module.name(self.db.upcast());
        dot2::Id::new(module_name.to_string())
    }

    fn node_id(&self, n: &Self::Node) -> dot2::Result<Id<'db>> {
        n.id()
    }

    fn node_shape(&self, _n: &Self::Node) -> Option<Text<'db>> {
        Some(Text::LabelStr("none".into()))
    }

    fn node_label(&self, n: &Self::Node) -> dot2::Result<Text<'db>> {
        Ok(n.label(self.db))
    }

    fn edge_label<'a>(&self, e: &Self::Edge) -> Text<'db> {
        Text::LabelStr(e.label(self.db).into())
    }

    fn subgraph_id(&self, s: &Self::Subgraph) -> Option<Id<'db>> {
        s.subgraph_id()
    }

    fn subgraph_label(&self, s: &Self::Subgraph) -> Text<'db> {
        s.label(self.db)
    }

    fn kind(&self) -> Kind {
        Kind::Digraph
    }
}

#[derive(Debug, Clone)]
pub(super) struct ModuleGraphEdge {
    from: BlockNode,
    to: BlockNode,
    func: FunctionId,
}

impl ModuleGraphEdge {
    fn label(&self, db: &dyn MirDb) -> String {
        let body = self.func.body(db);
        let terminator = body.order.terminator(&body.store, self.from.block).unwrap();
        let to = self.to.block;
        match body.store.branch_info(terminator) {
            BranchInfo::NotBranch => unreachable!(),
            BranchInfo::Jump(_) => String::new(),
            BranchInfo::Branch(_, true_bb, _) => {
                format! {"{}", true_bb == to}
            }
            BranchInfo::Switch(_, table, default) => {
                if default == Some(to) {
                    return "*".to_string();
                }

                for (value, bb) in table.iter() {
                    if bb == to {
                        let mut s = String::new();
                        value.pretty_print(db, &body.store, &mut s).unwrap();
                        return s;
                    }
                }

                unreachable!()
            }
        }
    }
}
