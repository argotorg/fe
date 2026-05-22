use crate::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};
use crate::provenance::ProvenanceNodeId;

#[derive(Clone, Debug, PartialEq)]
pub enum Fact {
    NodeHash {
        node_id: u32,
        kind: String,
        parent_id: Option<u32>,
        structure_hash: u128,
        names_hash: u128,
    },
    Origin {
        node_id: u32,
        origin: ProvenanceNodeId,
    },
    SourceSpan {
        node_id: u32,
        file: String,
        line: u32,
        col: u32,
        end_line: u32,
        end_col: u32,
    },
    GraphEdge {
        source_id: u32,
        label: String,
        target_id: u32,
    },
    NodeEffect {
        node_id: u32,
        effect: String,
    },
    DataFlowEdge {
        from_id: u32,
        to_id: u32,
    },
}

pub struct FactConsumer {
    facts: Vec<Fact>,
    node_stack: Vec<(u32, usize)>, // (node_id, facts_vec_index for NodeHash placeholder)
    next_id: u32,
    pending_origin: Option<ProvenanceNodeId>,
    pending_source: Option<(String, u32, u32, u32, u32)>,
    // Inline hash state per node for producing NodeHash facts
    hash_stack: Vec<[xxhash_rust::xxh3::Xxh3; Dim::COUNT]>,
}

impl Default for FactConsumer {
    fn default() -> Self {
        Self::new()
    }
}

impl FactConsumer {
    pub fn new() -> Self {
        Self::with_starting_id(0)
    }

    pub fn with_starting_id(start_id: u32) -> Self {
        Self {
            facts: Vec::new(),
            node_stack: Vec::new(),
            next_id: start_id,
            pending_origin: None,
            pending_source: None,
            hash_stack: Vec::new(),
        }
    }

    pub fn facts(&self) -> &[Fact] {
        &self.facts
    }

    pub fn into_facts(self) -> Vec<Fact> {
        self.facts
    }

    pub fn next_id(&self) -> u32 {
        self.next_id
    }

    fn alloc_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn current_id(&self) -> Option<u32> {
        self.node_stack.last().map(|(id, _)| *id)
    }

    #[allow(dead_code)]
    fn parent_id(&self) -> Option<u32> {
        if self.node_stack.len() >= 2 {
            Some(self.node_stack[self.node_stack.len() - 2].0)
        } else {
            None
        }
    }
}

impl IrConsumer for FactConsumer {
    fn enter_node(&mut self, kind: &str) {
        let id = self.alloc_id();
        let parent = self.current_id();

        let mut hashers = std::array::from_fn(|_| xxhash_rust::xxh3::Xxh3::new());
        for h in &mut hashers {
            h.update(kind.as_bytes());
        }
        self.hash_stack.push(hashers);

        // Emit pending origin/source for this node
        if let Some(origin) = self.pending_origin.take() {
            self.facts.push(Fact::Origin {
                node_id: id,
                origin,
            });
        }
        if let Some((file, line, col, end_line, end_col)) = self.pending_source.take() {
            self.facts.push(Fact::SourceSpan {
                node_id: id,
                file,
                line,
                col,
                end_line,
                end_col,
            });
        }

        // Store the index of this NodeHash fact for O(1) backpatch on exit
        let fact_idx = self.facts.len();
        self.node_stack.push((id, fact_idx));

        self.facts.push(Fact::NodeHash {
            node_id: id,
            kind: kind.to_string(),
            parent_id: parent,
            structure_hash: 0, // placeholder — filled on exit
            names_hash: 0,
        });
    }

    fn exit_node(&mut self) {
        let (_id, fact_idx) = self.node_stack.pop().expect("exit without enter");
        let hashers = self.hash_stack.pop().expect("hash stack mismatch");

        let dim_hashes: [u128; Dim::COUNT] = std::array::from_fn(|i| hashers[i].digest128());

        if let Fact::NodeHash {
            structure_hash: sh,
            names_hash: nh,
            ..
        } = &mut self.facts[fact_idx]
        {
            *sh = dim_hashes[Dim::Structure as usize];
            *nh = dim_hashes[Dim::Names as usize];
        }

        if let Some(parent_hashers) = self.hash_stack.last_mut() {
            for dim in Dim::ALL {
                parent_hashers[dim as usize].update(&dim_hashes[dim as usize].to_le_bytes());
            }
        }
    }

    fn field_u64(&mut self, dim: Dim, value: u64) {
        if let Some(hashers) = self.hash_stack.last_mut() {
            hashers[dim as usize].update(&value.to_le_bytes());
        }
    }

    fn field_bytes(&mut self, dim: Dim, value: &[u8]) {
        if let Some(hashers) = self.hash_stack.last_mut() {
            hashers[dim as usize].update(value);
        }
    }

    fn field_str(&mut self, dim: Dim, value: &str) {
        if let Some(hashers) = self.hash_stack.last_mut() {
            hashers[dim as usize].update(value.as_bytes());
        }
    }

    fn field_bool(&mut self, dim: Dim, value: bool) {
        if let Some(hashers) = self.hash_stack.last_mut() {
            hashers[dim as usize].update(&[value as u8]);
        }
    }

    fn child<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, child: &T) {
        child.describe(cx, self);
    }

    fn children_ordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        if let Some(hashers) = self.hash_stack.last_mut() {
            hashers[Dim::Structure as usize].update(&(children.len() as u64).to_le_bytes());
        }
        for child in children {
            child.describe(cx, self);
        }
    }

    fn children_unordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        self.children_ordered(cx, children);
    }

    fn graph_edge(&mut self, label: &str, target_id: u64) {
        if let Some(&(source_id, _)) = self.node_stack.last() {
            self.facts.push(Fact::GraphEdge {
                source_id,
                label: label.to_string(),
                target_id: target_id as u32,
            });
        }
    }

    fn origin(&mut self, origin: &ProvenanceNodeId) {
        self.pending_origin = Some(*origin);
    }

    fn source_span(&mut self, file: &str, line: u32, col: u32, end_line: u32, end_col: u32) {
        self.pending_source = Some((file.to_string(), line, col, end_line, end_col));
    }

    fn effect(&mut self, effect: &str) {
        if let Some(&(node_id, _)) = self.node_stack.last() {
            self.facts.push(Fact::NodeEffect {
                node_id,
                effect: effect.to_string(),
            });
        }
    }

    fn data_flow(&mut self, from_id: u64, to_id: u64) {
        self.facts.push(Fact::DataFlowEdge {
            from_id: from_id as u32,
            to_id: to_id as u32,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fact_consumer_produces_node_hash_facts() {
        let mut c = FactConsumer::new();

        c.enter_node("Root");
        c.field_u64(Dim::Structure, 42);

        c.enter_node("Child");
        c.field_str(Dim::Names, "x");
        c.exit_node();

        c.exit_node();

        let facts = c.into_facts();

        let node_hashes: Vec<_> = facts
            .iter()
            .filter(|f| matches!(f, Fact::NodeHash { .. }))
            .collect();

        assert_eq!(
            node_hashes.len(),
            2,
            "should have 2 NodeHash facts (Root + Child)"
        );

        // Verify hashes are non-zero (computed, not placeholders)
        for f in &node_hashes {
            if let Fact::NodeHash { structure_hash, .. } = f {
                assert_ne!(
                    *structure_hash, 0,
                    "hash should be computed, not placeholder"
                );
            }
        }

        // Verify parent-child relationship
        if let (
            Fact::NodeHash {
                node_id: root_id,
                parent_id: root_parent,
                ..
            },
            Fact::NodeHash {
                node_id: _child_id,
                parent_id: child_parent,
                ..
            },
        ) = (&node_hashes[0], &node_hashes[1])
        {
            assert_eq!(*root_parent, None);
            assert_eq!(*child_parent, Some(*root_id));
        }
    }

    #[test]
    fn fact_consumer_records_origins_and_spans() {
        let mut c = FactConsumer::new();

        c.origin(&ProvenanceNodeId::new(
            crate::provenance::IrLevel::Smir,
            7,
            crate::provenance::TransformTag::SmirToMir,
        ));
        c.source_span("test.fe", 42, 5, 42, 20);
        c.enter_node("Assign");
        c.exit_node();

        let facts = c.into_facts();

        let origins: Vec<_> = facts
            .iter()
            .filter(|f| matches!(f, Fact::Origin { .. }))
            .collect();
        assert_eq!(origins.len(), 1);

        let spans: Vec<_> = facts
            .iter()
            .filter(|f| matches!(f, Fact::SourceSpan { .. }))
            .collect();
        assert_eq!(spans.len(), 1);

        if let Fact::SourceSpan { line, .. } = &spans[0] {
            assert_eq!(*line, 42);
        }
    }

    #[test]
    fn fact_consumer_records_graph_edges() {
        let mut c = FactConsumer::new();

        c.enter_node("Body");
        c.enter_node("Block");
        c.exit_node();
        c.graph_edge("successor", 3);
        c.exit_node();

        let edges: Vec<_> = c
            .facts()
            .iter()
            .filter(|f| matches!(f, Fact::GraphEdge { .. }))
            .collect();
        assert_eq!(edges.len(), 1);
    }

    #[test]
    fn triple_composite_consumer() {
        use crate::debug_consumer::DebugConsumer;
        use crate::hash_consumer::HashConsumer;

        let mut composite = (
            HashConsumer::new(),
            (DebugConsumer::new(), FactConsumer::new()),
        );

        composite.origin(&ProvenanceNodeId::new(
            crate::provenance::IrLevel::Smir,
            1,
            crate::provenance::TransformTag::SmirToMir,
        ));
        composite.source_span("test.fe", 10, 1, 10, 20);
        composite.enter_node("Stmt");
        composite.field_u64(Dim::Structure, 99);
        composite.exit_node();

        // HashConsumer got hash
        assert!(composite.0.result().is_some());
        // DebugConsumer got entry with origin and source
        assert_eq!(composite.1.0.entries().len(), 1);
        assert!(composite.1.0.entries()[0].origin.is_some());
        assert!(composite.1.0.entries()[0].source.is_some());
        // FactConsumer got NodeHash + Origin + SourceSpan facts
        let facts = &composite.1.1.facts();
        assert!(facts.iter().any(|f| matches!(f, Fact::NodeHash { .. })));
        assert!(facts.iter().any(|f| matches!(f, Fact::Origin { .. })));
        assert!(facts.iter().any(|f| matches!(f, Fact::SourceSpan { .. })));
    }
}
