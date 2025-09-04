use std::{
    collections::{VecDeque, hash_map::Entry},
    io,
};

use cranelift_entity::{PrimaryMap, entity_impl};
use dot2::label::Text;
use rustc_hash::{FxHashMap, FxHashSet};

use super::scope_graph::{EdgeKind, ScopeGraph, ScopeId};
use crate::{HirDb, hir_def::ItemKind};

type NodeId = usize;

pub(super) struct ScopeGraphFormatter<'db> {
    db: &'db dyn HirDb,
    edges: PrimaryMap<EdgeId, Edge<'db>>,
    nodes: Vec<ScopeId<'db>>,
}

impl<'db> ScopeGraphFormatter<'db> {
    fn build_formatter(&mut self, s_graph: &ScopeGraph<'db>) {
        let mut visited = FxHashSet::default();
        let mut nodes_map = FxHashMap::default();

        let mut worklist = VecDeque::new();
        let root = s_graph.top_mod.scope();
        worklist.push_back(root);
        while let Some(scope) = worklist.pop_front() {
            if !visited.insert(scope) {
                continue;
            }
            let source = self.node_id(scope, &mut nodes_map);

            for edge in s_graph.edges(scope) {
                let target = self.node_id(edge.dest, &mut nodes_map);

                self.edges.push(Edge {
                    kind: edge.kind,
                    target,
                    source,
                });

                if !visited.contains(&edge.dest) {
                    worklist.push_back(edge.dest);
                }
            }
        }
    }

    fn node_id(
        &mut self,
        scope: ScopeId<'db>,
        nodes_map: &mut FxHashMap<ScopeId<'db>, usize>,
    ) -> NodeId {
        match nodes_map.entry(scope) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let id = self.nodes.len();
                self.nodes.push(scope);
                entry.insert(id);
                id
            }
        }
    }
}

impl<'db> ScopeGraphFormatter<'db> {
    pub(super) fn new(db: &'db dyn HirDb, s_graph: &ScopeGraph<'db>) -> Self {
        let nodes = Vec::new();
        let edges = PrimaryMap::new();
        let mut formatter = Self { db, edges, nodes };

        formatter.build_formatter(s_graph);
        formatter
    }

    pub(super) fn render(&self, w: &mut impl io::Write) -> io::Result<()> {
        dot2::render(self, w).map_err(|err| match err {
            dot2::Error::Io(err) => err,
            dot2::Error::InvalidId => unreachable!(),
        })
    }
}

impl<'db, 'a> dot2::Labeller<'a> for ScopeGraphFormatter<'db> {
    type Node = NodeId;
    type Edge = EdgeId;
    type Subgraph = ();

    fn graph_id(&'a self) -> dot2::Result<dot2::Id<'a>> {
        dot2::Id::new("example1")
    }

    fn node_id(&'a self, n: &Self::Node) -> dot2::Result<dot2::Id<'a>> {
        dot2::Id::new(format!("N{n}"))
    }

    fn node_label<'b>(&'a self, node: &Self::Node) -> dot2::Result<Text<'db>> {
        let label = match &self.nodes[*node] {
            ScopeId::Item(item) => {
                let item_name = match item {
                    ItemKind::Use(use_) => use_.pretty_path(self.db),
                    _ => item
                        .name(self.db)
                        .map_or(" ", |name| name.data(self.db))
                        .to_string(),
                };

                format!(
                    r#" <font color="{kw_color}">{kind_name}</font><font color="{item_color}">&nbsp;{item_name}</font> "#,
                    kw_color = "#7B2D80",
                    kind_name = item.kind_name(),
                    item_color = "#1B458D",
                )
            }

            ScopeId::Block(body, expr) => {
                let idx = body.iter_block(self.db)[expr];
                format!(
                    r#" <font color="{block_color}">{{block{block_number}}}</font> "#,
                    block_color = "#383A42",
                    block_number = idx
                )
            }

            scope => {
                format!(
                    r#" <font color="{param_color}">{name}</font> "#,
                    param_color = "#3A793A",
                    name = scope
                        .name(self.db)
                        .map_or(String::new(), |name| name.data(self.db).to_string()),
                )
            }
        };
        Ok(Text::HtmlStr(label.into()))
    }

    fn edge_label(&'a self, e: &Self::Edge) -> Text<'a> {
        let edge = &self.edges[*e];

        let label = match edge.kind {
            EdgeKind::Lex(_) => "lex",
            EdgeKind::Mod(_) => "mod",
            EdgeKind::Type(_) => "type",
            EdgeKind::Trait(_) => "trait",
            EdgeKind::TraitType(_) => "associated_type",
            EdgeKind::GenericParam(_) => "generic_param",
            EdgeKind::Value(_) => "value",
            EdgeKind::Field(_) => "field",
            EdgeKind::Variant(_) => "variant",
            EdgeKind::Super(_) => "super",
            EdgeKind::Ingot(_) => "ingot",
            EdgeKind::Self_(_) => "self",
            EdgeKind::SelfTy(_) => "self_ty",
            EdgeKind::Anon(_) => "anon",
        };
        let color = edge.color();
        let colored_label = format!(r#" <font color="{color}">{label}</font> "#);
        Text::HtmlStr(colored_label.into())
    }

    fn edge_color(&'a self, e: &Self::Edge) -> Option<Text<'a>> {
        let edge = &self.edges[*e];
        Some(Text::LabelStr(edge.color().into()))
    }

    fn node_shape(&self, _n: &Self::Node) -> Option<Text<'db>> {
        Some(Text::LabelStr("box".into()))
    }
}

impl<'a> dot2::GraphWalk<'a> for ScopeGraphFormatter<'_> {
    type Node = NodeId;
    type Edge = EdgeId;
    type Subgraph = ();

    fn nodes(&'a self) -> dot2::Nodes<'a, Self::Node> {
        (0..self.nodes.len()).collect()
    }

    fn edges(&'a self) -> dot2::Edges<'a, Self::Edge> {
        self.edges.keys().collect()
    }

    fn source(&self, e: &Self::Edge) -> Self::Node {
        self.edges[*e].source
    }

    fn target(&self, e: &Self::Edge) -> Self::Node {
        self.edges[*e].target
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct EdgeId(u32);
entity_impl!(EdgeId);

#[derive(Debug)]
struct Edge<'db> {
    kind: EdgeKind<'db>,
    target: NodeId,
    source: NodeId,
}

impl Edge<'_> {
    fn color(&self) -> &'static str {
        match self.kind {
            EdgeKind::Lex(_) => "#F94144",
            EdgeKind::Mod(_) => "#F3722C",
            EdgeKind::Type(_) => "#F8961E",
            EdgeKind::Trait(_) => "#F9C74F",
            EdgeKind::TraitType(_) => "#90BE6D",
            EdgeKind::GenericParam(_) => "#90BE6D",
            EdgeKind::Value(_) => "#43AA8B",
            EdgeKind::Field(_) => "#577590",
            EdgeKind::Variant(_) => "#6D597A",
            EdgeKind::Super(_) => "#B56576",
            EdgeKind::Ingot(_) => "#E56B6F",
            EdgeKind::Self_(_) => "#FFBA49",
            EdgeKind::SelfTy(_) => "#3A6351",
            EdgeKind::Anon(_) => "#788475",
        }
    }
}
