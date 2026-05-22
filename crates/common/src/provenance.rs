use indexmap::IndexSet;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ProvenanceNodeId {
    pub level: IrLevel,
    pub node: u32,
    pub transform: TransformTag,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(u16)]
pub enum IrLevel {
    Ast = 0,
    Hir = 1,
    Smir = 2,
    Mir = 3,
    Sonatina = 4,
    Bytecode = 5,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(u16)]
pub enum TransformTag {
    AstToHir = 0,
    HirDesugar = 1,
    HirToSmir = 2,
    SmirToMir = 3,
    MirToSonatina = 4,
    SonatinaPass = 5,
    SonatinaToBytecode = 6,
    Identity = 7,
    Synthetic = 8,
    SonatinaOptNew = 9,
}

impl ProvenanceNodeId {
    pub fn new(level: IrLevel, node: u32, transform: TransformTag) -> Self {
        Self { level, node, transform }
    }

    pub fn mir(node: u32, transform: TransformTag) -> Self {
        Self { level: IrLevel::Mir, node, transform }
    }

    pub fn sonatina(node: u32, transform: TransformTag) -> Self {
        Self { level: IrLevel::Sonatina, node, transform }
    }

    pub fn hir(node: u32, transform: TransformTag) -> Self {
        Self { level: IrLevel::Hir, node, transform }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ProvenanceDag {
    edges: IndexSet<(ProvenanceNodeId, ProvenanceNodeId)>,
}

impl ProvenanceDag {
    pub fn new() -> Self {
        Self { edges: IndexSet::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self { edges: IndexSet::with_capacity(cap) }
    }

    pub fn add_edge(&mut self, from: ProvenanceNodeId, to: ProvenanceNodeId) {
        self.edges.insert((from, to));
    }

    pub fn merge(&mut self, other: &ProvenanceDag) {
        self.edges.extend(other.edges.iter().copied());
    }

    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    pub fn edges(&self) -> impl Iterator<Item = &(ProvenanceNodeId, ProvenanceNodeId)> {
        self.edges.iter()
    }

    pub fn sources_of(&self, target: ProvenanceNodeId) -> Vec<ProvenanceNodeId> {
        self.edges
            .iter()
            .filter(|(_, t)| *t == target)
            .map(|(s, _)| *s)
            .collect()
    }

    pub fn targets_of(&self, source: ProvenanceNodeId) -> Vec<ProvenanceNodeId> {
        self.edges
            .iter()
            .filter(|(s, _)| *s == source)
            .map(|(_, t)| *t)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn provenance_node_id_is_8_bytes() {
        assert_eq!(std::mem::size_of::<ProvenanceNodeId>(), 8);
    }

    #[test]
    fn dag_deduplicates_edges() {
        let mut dag = ProvenanceDag::new();
        let a = ProvenanceNodeId::hir(1, TransformTag::AstToHir);
        let b = ProvenanceNodeId::mir(1, TransformTag::SmirToMir);

        dag.add_edge(a, b);
        dag.add_edge(a, b); // duplicate
        assert_eq!(dag.edge_count(), 1);
    }

    #[test]
    fn dag_merge_deduplicates() {
        let a = ProvenanceNodeId::hir(1, TransformTag::AstToHir);
        let b = ProvenanceNodeId::mir(1, TransformTag::SmirToMir);
        let c = ProvenanceNodeId::mir(2, TransformTag::SmirToMir);

        let mut dag1 = ProvenanceDag::new();
        dag1.add_edge(a, b);

        let mut dag2 = ProvenanceDag::new();
        dag2.add_edge(a, b); // overlap with dag1
        dag2.add_edge(a, c); // new

        dag1.merge(&dag2);
        assert_eq!(dag1.edge_count(), 2);
    }

    #[test]
    fn sources_and_targets() {
        let mut dag = ProvenanceDag::new();
        let h1 = ProvenanceNodeId::hir(1, TransformTag::AstToHir);
        let h2 = ProvenanceNodeId::hir(2, TransformTag::AstToHir);
        let m1 = ProvenanceNodeId::mir(1, TransformTag::SmirToMir);

        dag.add_edge(h1, m1);
        dag.add_edge(h2, m1);

        assert_eq!(dag.sources_of(m1).len(), 2);
        assert_eq!(dag.targets_of(h1), vec![m1]);
    }
}
