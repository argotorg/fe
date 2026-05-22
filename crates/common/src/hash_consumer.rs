use std::collections::BTreeMap;

use xxhash_rust::xxh3::Xxh3;

use crate::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};
use crate::provenance::ProvenanceNodeId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DimHashes {
    pub values: [u128; Dim::COUNT],
}

impl DimHashes {
    pub fn structure(&self) -> u128 { self.values[Dim::Structure as usize] }
    pub fn names(&self) -> u128 { self.values[Dim::Names as usize] }
    pub fn constants(&self) -> u128 { self.values[Dim::Constants as usize] }
    pub fn types(&self) -> u128 { self.values[Dim::Types as usize] }

    pub fn projected(&self, dims: crate::ir_describe::DimSet) -> u128 {
        use xxhash_rust::xxh3::xxh3_128;
        let mut buf = Vec::new();
        for dim in Dim::ALL {
            if dims.contains(dim) {
                buf.extend_from_slice(&self.values[dim as usize].to_le_bytes());
            }
        }
        xxh3_128(&buf)
    }
}

struct NodeState {
    id: u64,
    hashers: [Xxh3; Dim::COUNT],
}

impl NodeState {
    fn new(id: u64, kind: &str) -> Self {
        let mut hashers = std::array::from_fn(|_| Xxh3::new());
        for h in &mut hashers {
            h.update(kind.as_bytes());
        }
        Self { id, hashers }
    }

    fn feed(&mut self, dim: Dim, bytes: &[u8]) {
        self.hashers[dim as usize].update(bytes);
    }

    fn feed_child_hash(&mut self, child: &DimHashes) {
        for dim in Dim::ALL {
            self.hashers[dim as usize].update(&child.values[dim as usize].to_le_bytes());
        }
    }

    fn finish(self) -> DimHashes {
        DimHashes {
            values: std::array::from_fn(|i| self.hashers[i].digest128()),
        }
    }
}

pub struct HashConsumer {
    stack: Vec<NodeState>,
    next_id: u64,
    pending_node_id: Option<u64>,
    external_to_internal: BTreeMap<u64, u64>,
    pending_edges: Vec<(u64, String, u64)>,
    node_hashes: BTreeMap<u64, DimHashes>,
    result: Option<DimHashes>,
    has_graph_edges: bool,
}

impl HashConsumer {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            next_id: 0,
            pending_node_id: None,
            external_to_internal: BTreeMap::new(),
            pending_edges: Vec::new(),
            node_hashes: BTreeMap::new(),
            result: None,
            has_graph_edges: false,
        }
    }

    pub fn result(&self) -> Option<&DimHashes> {
        self.result.as_ref()
    }

    pub fn into_result(self) -> Option<DimHashes> {
        self.result
    }

    pub fn node_hashes(&self) -> &BTreeMap<u64, DimHashes> {
        &self.node_hashes
    }

    fn alloc_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

impl IrConsumer for HashConsumer {
    fn set_node_id(&mut self, external_id: u64) {
        self.pending_node_id = Some(external_id);
    }

    fn enter_node(&mut self, kind: &str) {
        let id = self.alloc_id();
        if let Some(ext_id) = self.pending_node_id.take() {
            self.external_to_internal.insert(ext_id, id);
        }
        self.stack.push(NodeState::new(id, kind));
    }

    fn exit_node(&mut self) {
        let state = self.stack.pop().expect("exit_node without matching enter_node");
        let id = state.id;
        let hashes = state.finish();

        self.node_hashes.insert(id, hashes.clone());

        if self.stack.is_empty() {
            if !self.has_graph_edges {
                self.result = Some(hashes);
            } else {
                self.result = Some(self.run_helbling());
            }
        } else if !self.has_graph_edges {
            self.stack.last_mut().unwrap().feed_child_hash(&hashes);
        }
        // In graph mode, skip feed_child_hash — Helbling canonicalizes order
    }

    fn field_u64(&mut self, dim: Dim, value: u64) {
        if let Some(state) = self.stack.last_mut() {
            state.feed(dim, &value.to_le_bytes());
        }
    }

    fn field_bytes(&mut self, dim: Dim, value: &[u8]) {
        if let Some(state) = self.stack.last_mut() {
            state.feed(dim, value);
        }
    }

    fn field_str(&mut self, dim: Dim, value: &str) {
        if let Some(state) = self.stack.last_mut() {
            state.feed(dim, value.as_bytes());
        }
    }

    fn field_bool(&mut self, dim: Dim, value: bool) {
        if let Some(state) = self.stack.last_mut() {
            state.feed(dim, &[value as u8]);
        }
    }

    fn child<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, child: &T) {
        child.describe(cx, self);
    }

    fn children_ordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        if let Some(state) = self.stack.last_mut() {
            state.feed(Dim::Structure, &(children.len() as u64).to_le_bytes());
        }
        for child in children {
            child.describe(cx, self);
        }
    }

    fn children_unordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        // For unordered children, we hash each child independently,
        // then sort by hash to get canonical order before feeding into parent
        if let Some(state) = self.stack.last_mut() {
            state.feed(Dim::Structure, &(children.len() as u64).to_le_bytes());
        }
        let mut child_hashes: Vec<DimHashes> = Vec::new();
        for child in children {
            let mut sub = HashConsumer::new();
            child.describe(cx, &mut sub);
            if let Some(h) = sub.into_result() {
                child_hashes.push(h);
            }
        }
        child_hashes.sort_by_key(|h| h.values);
        for h in &child_hashes {
            if let Some(state) = self.stack.last_mut() {
                state.feed_child_hash(h);
            }
        }
    }

    fn graph_edge(&mut self, label: &str, target_external_id: u64) {
        self.has_graph_edges = true;
        let source_internal = self.stack.last().map(|s| s.id).unwrap_or(0);
        // Store source internal ID + target EXTERNAL ID — resolve in run_helbling
        self.pending_edges.push((source_internal, label.to_string(), target_external_id));
    }

    fn origin(&mut self, _origin: &ProvenanceNodeId) {
        // HashConsumer ignores provenance — it's not what the code computes
    }

    fn source_span(&mut self, _file: &str, _line: u32, _col: u32, _end_line: u32, _end_col: u32) {
        // HashConsumer ignores source spans
    }
}

impl HashConsumer {
    fn run_helbling(&self) -> DimHashes {
        use petgraph::algo::tarjan_scc;
        use petgraph::graph::{DiGraph, NodeIndex};
        use petgraph::visit::EdgeRef;
        use petgraph::Direction;

        // Build petgraph from collected nodes + edges
        let mut graph = DiGraph::new();
        let mut id_to_idx: BTreeMap<u64, NodeIndex> = BTreeMap::new();

        for (&id, hashes) in &self.node_hashes {
            let idx = graph.add_node(hashes.clone());
            id_to_idx.insert(id, idx);
        }

        for (src_internal, label, dst_external) in &self.pending_edges {
            let dst_internal = self.external_to_internal
                .get(dst_external)
                .copied()
                .unwrap_or(*dst_external);
            if let (Some(&src_idx), Some(&dst_idx)) = (id_to_idx.get(src_internal), id_to_idx.get(&dst_internal)) {
                let mut label_hasher = Xxh3::new();
                label_hasher.update(label.as_bytes());
                let edge_label = (label_hasher.digest128() & 0xFF) as u8;
                graph.add_edge(src_idx, dst_idx, edge_label);
            }
        }

        // Tarjan SCC
        let sccs = tarjan_scc(&graph);
        let mut node_to_scc: BTreeMap<NodeIndex, usize> = BTreeMap::new();
        for (scc_idx, scc) in sccs.iter().enumerate() {
            for &node in scc {
                node_to_scc.insert(node, scc_idx);
            }
        }

        let mut final_node_hashes: BTreeMap<NodeIndex, DimHashes> = BTreeMap::new();
        let mut scc_hashes: Vec<DimHashes> = Vec::new();

        // Process SCCs in Tarjan order (reverse topological)
        for (scc_idx, scc) in sccs.iter().enumerate() {
            let mut augmented: Vec<(NodeIndex, DimHashes)> = Vec::new();

            for &node in scc {
                let content = &graph[node];
                let my_scc = scc_idx;

                let mut ext_edges: Vec<(u8, [u128; Dim::COUNT])> = Vec::new();
                for edge in graph.edges_directed(node, Direction::Outgoing) {
                    let target_scc = node_to_scc[&edge.target()];
                    if target_scc != my_scc {
                        if let Some(h) = final_node_hashes.get(&edge.target()) {
                            ext_edges.push((*edge.weight(), h.values));
                        }
                    }
                }
                ext_edges.sort();

                let mut hashers: [Xxh3; Dim::COUNT] = std::array::from_fn(|_| Xxh3::new());
                for dim in Dim::ALL {
                    hashers[dim as usize].update(&content.values[dim as usize].to_le_bytes());
                    for (label, ext_hash) in &ext_edges {
                        hashers[dim as usize].update(&[*label]);
                        hashers[dim as usize].update(&ext_hash[dim as usize].to_le_bytes());
                    }
                }
                let aug = DimHashes { values: std::array::from_fn(|i| hashers[i].digest128()) };
                augmented.push((node, aug));
            }

            augmented.sort_by_key(|(_, h)| h.values);

            let canon_pos: BTreeMap<NodeIndex, usize> = augmented.iter()
                .enumerate()
                .map(|(pos, (node, _))| (*node, pos))
                .collect();

            let mut canon_edges: Vec<(usize, u8, usize)> = Vec::new();
            for &node in scc {
                let from = canon_pos[&node];
                for edge in graph.edges_directed(node, Direction::Outgoing) {
                    if let Some(&to) = canon_pos.get(&edge.target()) {
                        canon_edges.push((from, *edge.weight(), to));
                    }
                }
            }
            canon_edges.sort();

            let mut scc_hashers: [Xxh3; Dim::COUNT] = std::array::from_fn(|_| Xxh3::new());
            for dim in Dim::ALL {
                for (_, h) in &augmented {
                    scc_hashers[dim as usize].update(&h.values[dim as usize].to_le_bytes());
                }
                for (from, label, to) in &canon_edges {
                    scc_hashers[dim as usize].update(&from.to_le_bytes());
                    scc_hashers[dim as usize].update(&[*label]);
                    scc_hashers[dim as usize].update(&to.to_le_bytes());
                }
            }
            let scc_hash = DimHashes { values: std::array::from_fn(|i| scc_hashers[i].digest128()) };
            scc_hashes.push(scc_hash.clone());

            for (pos, (node, _)) in augmented.iter().enumerate() {
                let mut nh: [Xxh3; Dim::COUNT] = std::array::from_fn(|_| Xxh3::new());
                for dim in Dim::ALL {
                    nh[dim as usize].update(&pos.to_le_bytes());
                    nh[dim as usize].update(&scc_hash.values[dim as usize].to_le_bytes());
                }
                let node_hash = DimHashes { values: std::array::from_fn(|i| nh[i].digest128()) };
                final_node_hashes.insert(*node, node_hash);
            }
        }

        // Graph hash = H(all SCC hashes in topological order)
        let mut final_hashers: [Xxh3; Dim::COUNT] = std::array::from_fn(|_| Xxh3::new());
        for scc_hash in scc_hashes.iter().rev() {
            for dim in Dim::ALL {
                final_hashers[dim as usize].update(&scc_hash.values[dim as usize].to_le_bytes());
            }
        }
        DimHashes { values: std::array::from_fn(|i| final_hashers[i].digest128()) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};

    struct Leaf { value: u64 }
    impl IrDescribe for Leaf {
        fn describe<C: IrConsumer>(&self, _cx: &DescribeCtx<'_>, c: &mut C) {
            c.enter_node("Leaf");
            c.field_u64(Dim::Structure, self.value);
            c.exit_node();
        }
    }

    struct NamedLeaf { name: &'static str, value: u64 }
    impl IrDescribe for NamedLeaf {
        fn describe<C: IrConsumer>(&self, _cx: &DescribeCtx<'_>, c: &mut C) {
            c.enter_node("NamedLeaf");
            c.field_str(Dim::Names, self.name);
            c.field_u64(Dim::Structure, self.value);
            c.exit_node();
        }
    }

    struct Tree { kind: &'static str, children: Vec<Tree> }
    impl IrDescribe for Tree {
        fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
            c.enter_node(self.kind);
            c.children_ordered(cx, &self.children);
            c.exit_node();
        }
    }

    // Can't easily create a DescribeCtx without a real db in tests,
    // so test the consumer directly via manual calls

    #[test]
    fn identical_nodes_produce_identical_hashes() {
        let mut c1 = HashConsumer::new();
        c1.enter_node("Leaf");
        c1.field_u64(Dim::Structure, 42);
        c1.exit_node();

        let mut c2 = HashConsumer::new();
        c2.enter_node("Leaf");
        c2.field_u64(Dim::Structure, 42);
        c2.exit_node();

        assert_eq!(c1.result(), c2.result());
    }

    #[test]
    fn different_values_produce_different_hashes() {
        let mut c1 = HashConsumer::new();
        c1.enter_node("Leaf");
        c1.field_u64(Dim::Structure, 42);
        c1.exit_node();

        let mut c2 = HashConsumer::new();
        c2.enter_node("Leaf");
        c2.field_u64(Dim::Structure, 99);
        c2.exit_node();

        assert_ne!(c1.result().unwrap().structure(), c2.result().unwrap().structure());
    }

    #[test]
    fn names_dimension_independent() {
        let mut c1 = HashConsumer::new();
        c1.enter_node("Leaf");
        c1.field_str(Dim::Names, "x");
        c1.field_u64(Dim::Structure, 42);
        c1.exit_node();

        let mut c2 = HashConsumer::new();
        c2.enter_node("Leaf");
        c2.field_str(Dim::Names, "y");
        c2.field_u64(Dim::Structure, 42);
        c2.exit_node();

        let h1 = c1.result().unwrap();
        let h2 = c2.result().unwrap();

        assert_eq!(h1.structure(), h2.structure(), "structure unchanged by rename");
        assert_ne!(h1.names(), h2.names(), "names changed by rename");
    }

    #[test]
    fn nested_nodes_hash_correctly() {
        let mut c = HashConsumer::new();
        c.enter_node("Parent");
        c.enter_node("Child");
        c.field_u64(Dim::Structure, 1);
        c.exit_node();
        c.enter_node("Child");
        c.field_u64(Dim::Structure, 2);
        c.exit_node();
        c.exit_node();

        assert!(c.result().is_some());
    }

    #[test]
    fn graph_edges_trigger_helbling() {
        let mut c = HashConsumer::new();
        c.enter_node("Body");

        // Two blocks
        c.enter_node("Block0");
        c.field_u64(Dim::Structure, 10);
        c.exit_node();

        c.enter_node("Block1");
        c.field_u64(Dim::Structure, 20);
        c.exit_node();

        // CFG edge: Block0 → Block1
        c.graph_edge("successor", 1);

        c.exit_node();

        assert!(c.result().is_some());
    }

    #[test]
    fn cycle_handled_via_helbling() {
        let mut c = HashConsumer::new();
        c.enter_node("Body");

        c.enter_node("Block0");
        c.field_u64(Dim::Structure, 10);
        c.exit_node();

        c.enter_node("Block1");
        c.field_u64(Dim::Structure, 20);
        c.exit_node();

        // Cycle: Block0 → Block1, Block1 → Block0
        c.graph_edge("goto", 1);      // 0 → 1
        c.graph_edge("back", 0);      // 1 → 0 (back-edge)

        c.exit_node();

        assert!(c.result().is_some(), "cycle must be handled without panic");
    }

    #[test]
    fn different_cycle_wiring_produces_different_hashes() {
        // Graph A: 3 blocks in a cycle A→B→C→A
        let mut c1 = HashConsumer::new();
        c1.enter_node("Body");
        c1.set_node_id(0); c1.enter_node("Block"); c1.field_u64(Dim::Structure, 1); c1.graph_edge("goto", 1); c1.exit_node();
        c1.set_node_id(1); c1.enter_node("Block"); c1.field_u64(Dim::Structure, 2); c1.graph_edge("goto", 2); c1.exit_node();
        c1.set_node_id(2); c1.enter_node("Block"); c1.field_u64(Dim::Structure, 3); c1.graph_edge("goto", 0); c1.exit_node();
        c1.exit_node();

        // Graph B: same 3 blocks, but cycle A→C→B→A (different wiring, same nodes)
        let mut c2 = HashConsumer::new();
        c2.enter_node("Body");
        c2.set_node_id(0); c2.enter_node("Block"); c2.field_u64(Dim::Structure, 1); c2.graph_edge("goto", 2); c2.exit_node();
        c2.set_node_id(1); c2.enter_node("Block"); c2.field_u64(Dim::Structure, 2); c2.graph_edge("goto", 0); c2.exit_node();
        c2.set_node_id(2); c2.enter_node("Block"); c2.field_u64(Dim::Structure, 3); c2.graph_edge("goto", 1); c2.exit_node();
        c2.exit_node();

        let h1 = c1.into_result().expect("graph A");
        let h2 = c2.into_result().expect("graph B");
        assert_ne!(
            h1.structure(), h2.structure(),
            "cycles with same nodes but different wiring must produce different hashes"
        );
    }

    #[test]
    fn same_cycle_different_order_same_hash() {
        // Graph A: blocks entered in order 0,1 — cycle 0→1→0
        let mut c1 = HashConsumer::new();
        c1.enter_node("Body");
        c1.set_node_id(0); c1.enter_node("Block"); c1.field_u64(Dim::Structure, 10); c1.graph_edge("goto", 1); c1.exit_node();
        c1.set_node_id(1); c1.enter_node("Block"); c1.field_u64(Dim::Structure, 20); c1.graph_edge("goto", 0); c1.exit_node();
        c1.exit_node();

        // Graph B: same cycle, blocks entered in order 1,0
        let mut c2 = HashConsumer::new();
        c2.enter_node("Body");
        c2.set_node_id(1); c2.enter_node("Block"); c2.field_u64(Dim::Structure, 20); c2.graph_edge("goto", 0); c2.exit_node();
        c2.set_node_id(0); c2.enter_node("Block"); c2.field_u64(Dim::Structure, 10); c2.graph_edge("goto", 1); c2.exit_node();
        c2.exit_node();

        let h1 = c1.into_result().expect("order A");
        let h2 = c2.into_result().expect("order B");
        assert_eq!(
            h1.structure(), h2.structure(),
            "same cycle with blocks in different traversal order must produce identical hashes"
        );
    }

    #[test]
    fn deterministic_hashing() {
        let hash = || {
            let mut c = HashConsumer::new();
            c.enter_node("Root");
            c.field_u64(Dim::Structure, 42);
            c.field_str(Dim::Names, "hello");
            c.exit_node();
            c.into_result().unwrap()
        };

        assert_eq!(hash(), hash());
    }
}
