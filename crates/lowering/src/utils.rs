use fe_common::files::SourceFileId;
use fe_common::Span;
use fe_parser::node::{Node, NodeId};

/// A trait to turn any sized type into a `Node` with a span of zero.
pub trait ZeroSpanNode: Sized {
    /// Wrap the value in a `Node` with a span of zero.
    fn into_node(self) -> Node<Self> {
        Node::new(self, Span::zero(SourceFileId::default()))
    }

    fn into_traceable_node(self, original_id: NodeId) -> Node<Self> {
        Node::with_original_id(self, Span::zero(SourceFileId::default()), original_id)
    }

    /// Wrap the value in a boxed `Node` with a span of zero
    fn into_boxed_node(self) -> Box<Node<Self>> {
        Box::new(self.into_node())
    }
}

impl<T> ZeroSpanNode for T {}
