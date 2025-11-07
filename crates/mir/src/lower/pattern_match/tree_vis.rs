use std::fmt::Write;

use dot2::{label::Text, Id};
use fe_analyzer::{pattern_analysis::ConstructorKind, AnalyzerDb};
use fxhash::FxHashMap;
use indexmap::IndexMap;
use smol_str::SmolStr;

use super::decision_tree::{Case, DecisionTree, LeafNode, Occurrence, SwitchNode};

pub(super) struct TreeRenderer<'db> {
    nodes: Vec<Node>,
    edges: FxHashMap<(usize, usize), Case>,
    db: &'db dyn AnalyzerDb,
}

impl<'db> TreeRenderer<'db> {
    #[allow(unused)]
    pub(super) fn new(db: &'db dyn AnalyzerDb, tree: &DecisionTree) -> Self {
        let mut renderer = Self {
            nodes: Vec::new(),
            edges: FxHashMap::default(),
            db,
        };

        match tree {
            DecisionTree::Leaf(leaf) => {
                renderer.nodes.push(Node::from(leaf));
            }
            DecisionTree::Switch(switch) => {
                renderer.nodes.push(Node::from(switch));
                let node_id = renderer.nodes.len() - 1;
                for arm in &switch.arms {
                    renderer.switch_from(&arm.1, node_id, arm.0);
                }
            }
        }
        renderer
    }

    fn switch_from(&mut self, tree: &DecisionTree, node_id: usize, case: Case) {
        match tree {
            DecisionTree::Leaf(leaf) => {
                self.nodes.push(Node::from(leaf));
                self.edges.insert((node_id, self.nodes.len() - 1), case);
            }

            DecisionTree::Switch(switch) => {
                self.nodes.push(Node::from(switch));
                let switch_id = self.nodes.len() - 1;
                self.edges.insert((node_id, switch_id), case);
                for arm in &switch.arms {
                    self.switch_from(&arm.1, switch_id, arm.0);
                }
            }
        }
    }
}

impl<'db> dot2::Labeller<'db> for TreeRenderer<'db> {
    type Node = usize;
    type Edge = (Self::Node, Self::Node);
    type Subgraph = ();

    fn graph_id(&self) -> dot2::Result<Id<'db>> {
        dot2::Id::new("DecisionTree")
    }

    fn node_id(&self, n: &Self::Node) -> dot2::Result<Id<'db>> {
        dot2::Id::new(format!("N{}", *n))
    }

    fn node_label(&self, n: &Self::Node) -> dot2::Result<Text<'db>> {
        let node = &self.nodes[*n];
        let label = match node {
            Node::Leaf { arm_idx, .. } => {
                format!("arm_idx: {arm_idx}")
            }
            Node::Switch(occurrence) => {
                let mut s = "expr".to_string();
                for num in occurrence.iter() {
                    write!(&mut s, ".{num}").unwrap();
                }
                s
            }
        };

        Ok(Text::LabelStr(label.into()))
    }

    fn edge_label(&self, e: &Self::Edge) -> Text<'db> {
        let label = match &self.edges[e] {
            Case::Ctor(ConstructorKind::Enum(variant)) => {
                variant.name_with_parent(self.db).to_string()
            }
            Case::Ctor(ConstructorKind::Tuple(_)) => "()".to_string(),
            Case::Ctor(ConstructorKind::Struct(sid)) => sid.name(self.db).into(),
            Case::Ctor(ConstructorKind::Literal((lit, _))) => lit.to_string(),
            Case::Default => "_".into(),
        };

        Text::LabelStr(label.into())
    }
}

impl<'db> dot2::GraphWalk<'db> for TreeRenderer<'db> {
    type Node = usize;
    type Edge = (Self::Node, Self::Node);
    type Subgraph = ();

    fn nodes(&self) -> dot2::Nodes<'db, Self::Node> {
        (0..self.nodes.len()).collect()
    }

    fn edges(&self) -> dot2::Edges<'db, Self::Edge> {
        self.edges.keys().cloned().collect::<Vec<_>>().into()
    }

    fn source(&self, e: &Self::Edge) -> Self::Node {
        e.0
    }

    fn target(&self, e: &Self::Edge) -> Self::Node {
        e.1
    }
}

enum Node {
    Leaf {
        arm_idx: usize,
        #[allow(unused)]
        binds: IndexMap<(SmolStr, usize), Occurrence>,
    },
    Switch(Occurrence),
}

impl From<&LeafNode> for Node {
    fn from(node: &LeafNode) -> Self {
        Node::Leaf {
            arm_idx: node.arm_idx,
            binds: node.binds.clone(),
        }
    }
}

impl From<&SwitchNode> for Node {
    fn from(node: &SwitchNode) -> Self {
        Node::Switch(node.occurrence.clone())
    }
}
