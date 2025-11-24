use rowan::ast::{AstNode, support};

use super::{AstChildren, GenericArgsOwner, ast_node};
use crate::{SyntaxKind as SK, syntax_node::SyntaxToken};

ast_node! {
    /// A path.
    /// `foo::bar::baz`
    pub struct Path,
    SK::Path,
    IntoIterator<Item=PathSegment>,
}
impl Path {
    pub fn text(&self) -> rowan::SyntaxText {
        self.syntax().text()
    }

    /// Returns the segments of the path.
    pub fn segments(&self) -> AstChildren<PathSegment> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// A path segment.
    pub struct PathSegment,
    SK::PathSegment
}
impl GenericArgsOwner for PathSegment {}
impl PathSegment {
    pub fn kind(&self) -> Option<PathSegmentKind> {
        match self.syntax().first_child_or_token() {
            Some(node) => match node.kind() {
                SK::IngotKw => Some(PathSegmentKind::Ingot(node.into_token().unwrap())),
                SK::SuperKw => Some(PathSegmentKind::Super(node.into_token().unwrap())),
                SK::SelfTypeKw => Some(PathSegmentKind::SelfTy(node.into_token().unwrap())),
                SK::SelfKw => Some(PathSegmentKind::Self_(node.into_token().unwrap())),
                SK::Ident => Some(PathSegmentKind::Ident(node.into_token().unwrap())),
                SK::QualifiedType => Some(PathSegmentKind::QualifiedType(
                    QualifiedType::cast(node.into_node().unwrap()).unwrap(),
                )),
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns the qualified type node if this segment is a qualified type
    /// like `<T as Trait>`.
    pub fn qualified_type(&self) -> Option<QualifiedType> {
        match self.kind()? {
            PathSegmentKind::QualifiedType(q) => Some(q),
            _ => None,
        }
    }
    /// Returns the identifier of the segment.
    pub fn ident(&self) -> Option<SyntaxToken> {
        match self.kind()? {
            PathSegmentKind::Ingot(token) => Some(token),
            PathSegmentKind::Super(token) => Some(token),
            PathSegmentKind::SelfTy(token) => Some(token),
            PathSegmentKind::Self_(token) => Some(token),
            PathSegmentKind::Ident(token) => Some(token),
            PathSegmentKind::QualifiedType(_) => None,
        }
    }

    /// Returns `true` if the segment  is a `self` keyword.
    pub fn is_self(&self) -> bool {
        support::token(self.syntax(), SK::SelfKw).is_some()
    }

    /// Returns `true` if the segment is a `Self` keyword.
    pub fn is_self_ty(&self) -> bool {
        support::token(self.syntax(), SK::SelfTypeKw).is_some()
    }
}

/// A path segment kind.
pub enum PathSegmentKind {
    /// `ingot`
    Ingot(SyntaxToken),
    /// `super`
    Super(SyntaxToken),
    /// `Self`
    SelfTy(SyntaxToken),
    /// `self`
    Self_(SyntaxToken),
    /// `foo`
    Ident(SyntaxToken),
    /// `<Foo as Bar>`
    QualifiedType(QualifiedType),
}

ast_node! {
    pub struct QualifiedType,
    SK::QualifiedType
}
impl QualifiedType {
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    pub fn trait_qualifier(&self) -> Option<super::TraitRef> {
        support::child(self.syntax())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::Lexer,
        parser::{Parser, path::PathScope},
    };

    use wasm_bindgen_test::wasm_bindgen_test;

    fn parse_path(source: &str) -> Path {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(PathScope::default()).unwrap();
        Path::cast(parser.finish_to_node().0).unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_ast() {
        let source = r#"self::Foo"#;
        let path = parse_path(source);
        let mut segments = path.segments();

        assert!(segments.next().unwrap().is_self());
        assert_eq!(segments.next().unwrap().ident().unwrap().text(), "Foo");
        assert!(segments.next().is_none());
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_ast2() {
        let source = r#"Self::Dep"#;
        let path = parse_path(source);
        let mut segments = path.segments();

        let self_ = segments.next().unwrap();
        assert!(self_.is_self_ty());
        assert_eq!(self_.ident().unwrap().text(), "Self");
        assert_eq!(segments.next().unwrap().ident().unwrap().text(), "Dep");
        assert!(segments.next().is_none());
    }
}
