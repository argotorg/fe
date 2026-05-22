use crate::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};
use crate::provenance::ProvenanceNodeId;

#[derive(Clone, Debug)]
pub struct DebugEntry {
    pub node_kind: String,
    pub depth: usize,
    pub origin: Option<ProvenanceNodeId>,
    pub source: Option<SourceLocation>,
}

#[derive(Clone, Debug)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
    pub col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

pub struct DebugConsumer {
    entries: Vec<DebugEntry>,
    depth: usize,
    current_origin: Option<ProvenanceNodeId>,
    current_source: Option<SourceLocation>,
}

impl DebugConsumer {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            depth: 0,
            current_origin: None,
            current_source: None,
        }
    }

    pub fn entries(&self) -> &[DebugEntry] {
        &self.entries
    }

    pub fn into_entries(self) -> Vec<DebugEntry> {
        self.entries
    }

    pub fn entries_with_source(&self) -> impl Iterator<Item = &DebugEntry> {
        self.entries.iter().filter(|e| e.source.is_some())
    }

    pub fn entries_with_origin(&self) -> impl Iterator<Item = &DebugEntry> {
        self.entries.iter().filter(|e| e.origin.is_some())
    }
}

impl IrConsumer for DebugConsumer {
    fn enter_node(&mut self, kind: &str) {
        let entry = DebugEntry {
            node_kind: kind.to_string(),
            depth: self.depth,
            origin: self.current_origin.take(),
            source: self.current_source.take(),
        };
        self.entries.push(entry);
        self.depth += 1;
    }

    fn exit_node(&mut self) {
        self.depth = self.depth.saturating_sub(1);
        self.current_origin = None;
        self.current_source = None;
    }

    fn field_u64(&mut self, _dim: Dim, _value: u64) {}
    fn field_bytes(&mut self, _dim: Dim, _value: &[u8]) {}
    fn field_str(&mut self, _dim: Dim, _value: &str) {}
    fn field_bool(&mut self, _dim: Dim, _value: bool) {}

    fn child<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, child: &T) {
        child.describe(cx, self);
    }

    fn children_ordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        for child in children {
            child.describe(cx, self);
        }
    }

    fn children_unordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        self.children_ordered(cx, children);
    }

    fn graph_edge(&mut self, _label: &str, _target_id: u64) {}

    fn origin(&mut self, origin: &ProvenanceNodeId) {
        self.current_origin = Some(*origin);
    }

    fn source_span(&mut self, file: &str, line: u32, col: u32, end_line: u32, end_col: u32) {
        self.current_source = Some(SourceLocation {
            file: file.to_string(),
            line,
            col,
            end_line,
            end_col,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_consumer_collects_entries() {
        let mut c = DebugConsumer::new();

        c.origin(&ProvenanceNodeId::new(
            crate::provenance::IrLevel::Smir, 5,
            crate::provenance::TransformTag::SmirToMir,
        ));
        c.source_span("test.fe", 10, 5, 10, 15);
        c.enter_node("Assign");
        c.field_u64(Dim::Structure, 42);
        c.exit_node();

        assert_eq!(c.entries().len(), 1);
        let entry = &c.entries()[0];
        assert_eq!(entry.node_kind, "Assign");
        assert!(entry.origin.is_some());
        assert!(entry.source.is_some());
        assert_eq!(entry.source.as_ref().unwrap().line, 10);
    }

    #[test]
    fn composite_with_hash_consumer() {
        use crate::hash_consumer::HashConsumer;

        let mut composite = (HashConsumer::new(), DebugConsumer::new());

        composite.origin(&ProvenanceNodeId::new(
            crate::provenance::IrLevel::Smir, 1,
            crate::provenance::TransformTag::SmirToMir,
        ));
        composite.enter_node("Stmt");
        composite.field_u64(Dim::Structure, 99);
        composite.exit_node();

        // Hash consumer got the hash
        assert!(composite.0.result().is_some());
        // Debug consumer got the entry with origin
        assert_eq!(composite.1.entries().len(), 1);
        assert!(composite.1.entries()[0].origin.is_some());
    }
}
