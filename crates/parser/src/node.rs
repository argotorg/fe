pub use fe_common::{Span, Spanned};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq, Default, PartialOrd, Ord)]
pub struct NodeId(Uuid);

impl NodeId {
    pub fn create() -> Self {
        Self(Uuid::new_v4())
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Node<T> {
    pub kind: T,
    #[serde(skip_serializing, skip_deserializing)]
    pub id: NodeId,
    #[serde(skip_serializing, skip_deserializing)]
    pub original_id: NodeId,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        let id = NodeId::create();
        Self {
            kind,
            id,
            original_id: id,
            span,
        }
    }

    pub fn with_original_id(kind: T, span: Span, original_id: NodeId) -> Self {
        let id = NodeId::create();
        Self {
            kind,
            id,
            original_id,
            span,
        }
    }
}

impl<T> Spanned for Node<T> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T> From<&Node<T>> for Span {
    fn from(node: &Node<T>) -> Self {
        node.span
    }
}

impl<T> From<&Box<Node<T>>> for Span {
    fn from(node: &Box<Node<T>>) -> Self {
        node.span
    }
}

impl<T> From<&Node<T>> for NodeId {
    fn from(node: &Node<T>) -> Self {
        node.id
    }
}

impl<T> From<&Box<Node<T>>> for NodeId {
    fn from(node: &Box<Node<T>>) -> Self {
        node.id
    }
}
