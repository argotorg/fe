pub mod attr;
pub mod expr;
pub mod item;
pub mod lit;
pub mod param;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod types;
pub mod use_tree;

pub use attr::*;
pub use expr::*;
pub use item::*;
pub use lit::*;
pub use param::*;
pub use pat::*;
pub use path::*;
pub use stmt::*;
pub use types::*;
pub use use_tree::*;

pub type AstChildren<T> = rowan::ast::AstChildren<T>;
pub type SyntaxText = rowan::SyntaxText;
pub type AstPtr<T> = rowan::ast::AstPtr<T>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<FeLang>;

/// Child lookups that scan a node's green children and build a syntax node
/// only for the one found, where `rowan::ast::support` builds one for every
/// child it passes.
mod support {
    use rowan::{NodeOrToken, ast::AstNode};

    use crate::{FeLang, SyntaxKind, SyntaxNode, SyntaxToken};

    pub(super) fn child<N: AstNode<Language = FeLang>>(parent: &SyntaxNode) -> Option<N> {
        parent.first_child_by_kind(&N::can_cast).and_then(N::cast)
    }

    pub(super) use rowan::ast::support::children;

    pub(super) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .first_child_or_token_by_kind(&|found| found == kind)
            .and_then(NodeOrToken::into_token)
    }
}

pub mod prelude {
    pub use super::{
        AttrListOwner, GenericArgsOwner, GenericParamsOwner, ItemModifierOwner, WhereClauseOwner,
    };
    pub use rowan::ast::AstNode;
}

macro_rules! ast_node {
    (
        $(#[$attrs: meta])*
        $visibility: vis struct $name: ident $({
             $($field_vis: vis $field: ident: $ty: ty),*
        })?,
        $kind: pat $(,)?
    ) => {
        $(#[$attrs])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $visibility struct $name {
            __syntax: crate::SyntaxNode,
            $($($field: $ty),*)?
        }

        impl rowan::ast::AstNode for $name {
            type Language = $crate::FeLang;
            fn can_cast(node: crate::SyntaxKind) -> bool {
                matches!(node, $kind)
            }
            fn cast(node: crate::SyntaxNode) -> Option<Self> {
                Self::can_cast(node.kind()).then(|| Self{
                    __syntax: node.into(),
                    $($($field: Default::default(),)*)?
                })
            }
            fn syntax(&self) -> &crate::SyntaxNode {
                &self.__syntax
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(rowan::ast::AstNode::syntax(self), f)
            }
        }
    };
    (
        $(#[$attrs: meta])*
        $visibility: vis struct $name: ident $({
             $($field_vis: vis $field: ident: $ty: ty),*
        })?,
        $kind: pat,
        IntoIterator<Item=$item_ty:ty> $(,)?
    ) => {
        ast_node!{
            $(#[$attrs])*
            $visibility struct $name $({
                $($field_vis $field: $ty),*
            })?,
            $kind
        }
        impl IntoIterator for $name {
            type Item = $item_ty;
            type IntoIter = crate::ast::AstChildren<$item_ty>;

            fn into_iter(self) -> Self::IntoIter {
                rowan::ast::support::children(rowan::ast::AstNode::syntax(&self))
            }
        }
        impl IntoIterator for &$name {
            type Item = $item_ty;
            type IntoIter = crate::ast::AstChildren<$item_ty>;

            fn into_iter(self) -> Self::IntoIter {
                rowan::ast::support::children(rowan::ast::AstNode::syntax(self))
            }
        }

        impl $name {
            /// Returns an iterator over the children of this node.
            pub fn iter(&self) -> crate::ast::AstChildren<$item_ty> {
                self.into_iter()
            }
        }
    };
}

use ast_node;

use crate::FeLang;
