use crate::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FeLang {}

impl rowan::Language for FeLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::try_from(raw.0).expect("invalid SyntaxKind from raw u16")
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<FeLang>;
pub type SyntaxToken = rowan::SyntaxToken<FeLang>;
pub type GreenNode = rowan::GreenNode;
pub type TextRange = rowan::TextRange;
pub type NodeOrToken = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
