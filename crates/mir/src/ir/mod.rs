use fe_common::Span;
use fe_parser::node::{Node, NodeId};

pub mod basic_block;
pub mod body_builder;
pub mod body_cursor;
pub mod body_order;
pub mod constant;
pub mod function;
pub mod inst;
pub mod types;
pub mod value;

pub use basic_block::{BasicBlock, BasicBlockId};
pub use constant::{Constant, ConstantId};
pub use function::{FunctionBody, FunctionId, FunctionParam, FunctionSignature};
pub use inst::{Inst, InstId};
pub use types::{Type, TypeId, TypeKind};
pub use value::{Value, ValueId};

/// An original source information that indicates where `mir` entities derive
/// from. `SourceInfo` is mainly used for diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceInfo {
    pub span: Span,
    pub id: NodeId,
}

impl SourceInfo {
    pub fn dummy() -> Self {
        Self {
            span: Span::dummy(),
            id: NodeId::dummy(),
        }
    }

    pub fn is_dummy(&self) -> bool {
        self == &Self::dummy()
    }
}

impl<T> From<&Node<T>> for SourceInfo {
    fn from(node: &Node<T>) -> Self {
        Self {
            span: node.span,
            id: node.id,
        }
    }
}
