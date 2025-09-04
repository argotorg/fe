use rowan::ast::{AstNode, support};

use super::ast_node;
use crate::{FeLang, SyntaxKind as SK, SyntaxToken};

ast_node! {
    pub struct AttrList,
    SK::AttrList,
    IntoIterator<Item=Attr>,
}
impl AttrList {
    /// Returns only normal attributes in the attribute list.
    pub fn normal_attrs(&self) -> impl Iterator<Item = NormalAttr> {
        self.iter().filter_map(|attr| match attr.kind() {
            AttrKind::Normal(attr) => Some(attr),
            AttrKind::DocComment(_) => None,
        })
    }

    /// Returns only doc comment attributes in the attribute list.
    pub fn doc_attrs(&self) -> impl Iterator<Item = DocCommentAttr> {
        self.iter().filter_map(|attr| match attr.kind() {
            AttrKind::Normal(_) => None,
            AttrKind::DocComment(attr) => Some(attr),
        })
    }
}

ast_node! {
    /// An attribute, which can be either a normal attribute or a doc comment attribute.
    pub struct Attr,
    SK::Attr | SK::DocCommentAttr,
}
impl Attr {
    /// Returns the kind of the attribute.
    pub fn kind(&self) -> AttrKind {
        match self.syntax().kind() {
            SK::Attr => AttrKind::Normal(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::DocCommentAttr => {
                AttrKind::DocComment(AstNode::cast(self.syntax().clone()).unwrap())
            }
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// A normal attribute.
    /// `#attr(arg1: Arg, arg2: Arg)`
    pub struct NormalAttr,
    SK::Attr,
}
impl NormalAttr {
    /// Returns the name of the attribute.
    /// `foo` in `#foo(..)`
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn args(&self) -> Option<AttrArgList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// An attribute argument list.
    /// `(arg1: Arg, arg2: Arg)` in `#foo(arg1: Arg, arg2: Arg)`
    pub struct AttrArgList,
    SK::AttrArgList,
    IntoIterator<Item=AttrArg>,
}

ast_node! {
    /// An Attribute argument.
    /// `arg1: Arg` in `#foo(arg1: Arg, arg2: Arg)`
    pub struct AttrArg,
    SK::AttrArg
}
impl AttrArg {
    /// Returns the key of the attribute argument.
    /// `arg1` in `arg1: Arg`.
    pub fn key(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the value of the attribute argument.
    /// `Arg` in `arg1: Arg`.
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|c| match c.into_token() {
                Some(c) if c.kind() == SK::Ident => Some(c),
                _ => None,
            })
            .nth(1)
    }
}

ast_node! {
    pub struct DocCommentAttr,
    SK::DocCommentAttr,
}
impl DocCommentAttr {
    /// Returns the underlying token of the doc comment, which includes `///`.
    pub fn doc(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::DocComment)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum AttrKind {
    /// A normal attribute.
    Normal(NormalAttr),
    /// A doc comment attribute.
    DocComment(DocCommentAttr),
}

/// A trait for AST nodes that can have an attributes.
pub trait AttrListOwner: AstNode<Language = FeLang> {
    /// Returns the attribute list of the node.
    fn attr_list(&self) -> Option<AttrList> {
        support::child(self.syntax())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{Parser, attr::AttrListScope},
    };

    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;

    fn parse_attr_list(source: &str) -> AttrList {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(AttrListScope::default()).unwrap();
        AttrList::cast(parser.finish_to_node().0).unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn attr_list() {
        let source = r#"
            #foo
            /// Doc1
            #cfg(target: evm, abi: solidity)
            /// Doc2
        "#;
        let attr_list = parse_attr_list(source);
        for (i, attr) in attr_list.doc_attrs().enumerate() {
            match i {
                0 => assert_eq!(attr.doc().unwrap().text(), "/// Doc1"),
                1 => assert_eq!(attr.doc().unwrap().text(), "/// Doc2"),
                _ => unreachable!(),
            }
        }

        for (i, attr) in attr_list.normal_attrs().enumerate() {
            match i {
                0 => {
                    assert_eq!(attr.name().unwrap().text(), "foo");
                    assert!(attr.args().is_none());
                }

                1 => {
                    assert_eq!(attr.name().unwrap().text(), "cfg");
                    for (i, arg) in attr.args().unwrap().iter().enumerate() {
                        match i {
                            0 => {
                                assert_eq!(arg.key().unwrap().text(), "target");
                                assert_eq!(arg.value().unwrap().text(), "evm");
                            }
                            1 => {
                                assert_eq!(arg.key().unwrap().text(), "abi");
                                assert_eq!(arg.value().unwrap().text(), "solidity");
                            }
                            _ => unreachable!(),
                        }
                    }
                }

                _ => unreachable!(),
            }
        }
    }
}
