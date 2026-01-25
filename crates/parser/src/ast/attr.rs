use rowan::ast::{AstNode, support};

use super::{Expr, Path, ast_node, lit::Lit};
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
    /// `#[attr(arg1 = Arg, arg2)]` or `#[path::to::attr]` or `#[foo = "..."]` or `#[x = 10]`
    pub struct NormalAttr,
    SK::Attr,
}
impl NormalAttr {
    pub fn path(&self) -> Option<Path> {
        support::child(self.syntax())
    }

    pub fn args(&self) -> Option<AttrArgList> {
        support::child(self.syntax())
    }

    /// Returns the direct value for the `#[attr = value]` form.
    /// This is distinct from args() which returns the argument list `(arg1, arg2)`.
    pub fn value(&self) -> Option<AttrArgValueKind> {
        let node = support::child::<AttrArgValue>(self.syntax())?;
        if let Some(expr) = support::child::<Expr>(node.syntax()) {
            return Some(expr.into());
        }
        if let Some(lit) = support::child::<Lit>(node.syntax()) {
            return Some(lit.into());
        }
        Some(support::token(node.syntax(), SK::Ident)?.into())
    }
}

ast_node! {
    /// An attribute argument list.
    /// `(arg1 = Arg, arg2 = Arg)` in `#[foo(arg1 = Arg, arg2 = Arg)]`
    pub struct AttrArgList,
    SK::AttrArgList,
    IntoIterator<Item=AttrArg>,
}

ast_node! {
    /// An Attribute argument.
    /// `arg1` or `arg2 = Arg` in `#[foo(arg1, arg2 = Arg)]`
    pub struct AttrArg,
    SK::AttrArg
}
impl AttrArg {
    pub fn key(&self) -> Option<Path> {
        support::child(self.syntax())
    }

    pub fn value(&self) -> Option<AttrArgValueKind> {
        let node = support::child::<AttrArgValue>(self.syntax())?;
        if let Some(expr) = support::child::<Expr>(node.syntax()) {
            return Some(expr.into());
        }
        if let Some(lit) = support::child::<Lit>(node.syntax()) {
            return Some(lit.into());
        }
        Some(support::token(node.syntax(), SK::Ident)?.into())
    }

    /// Returns the value node of the attribute argument.
    pub fn value_node(&self) -> Option<AttrArgValue> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// Attribute argument value wrapper
    pub struct AttrArgValue,
    SK::AttrArgValue
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum AttrArgValueKind {
    Ident(SyntaxToken),
    Lit(Lit),
    Expr(Expr),
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
            #[foo]
            /// Doc1
            #[cfg(target = "evm", abi = "solidity")]
            #[feat(foo::bar = 1, baz = false, name = Foo)]
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
                    assert_eq!(attr.path().unwrap().text(), "foo");
                    assert!(attr.args().is_none());
                }

                1 => {
                    assert_eq!(attr.path().unwrap().text(), "cfg");
                    for (i, arg) in attr.args().unwrap().iter().enumerate() {
                        match i {
                            0 => {
                                assert_eq!(arg.key().unwrap().text(), "target");
                                let val = arg.value().unwrap();
                                match val {
                                    AttrArgValueKind::Ident(tok) => {
                                        panic!("expected string literal, got ident {}", tok.text())
                                    }
                                    AttrArgValueKind::Lit(lit) => match lit.kind() {
                                        crate::ast::lit::LitKind::String(s) => {
                                            assert_eq!(s.token().text(), "\"evm\"")
                                        }
                                        _ => panic!("expected string literal"),
                                    },
                                    AttrArgValueKind::Expr(_) => {
                                        panic!("expected string literal, got expr")
                                    }
                                }
                            }
                            1 => {
                                assert_eq!(arg.key().unwrap().text(), "abi");
                                let val = arg.value().unwrap();
                                match val {
                                    AttrArgValueKind::Ident(tok) => {
                                        panic!("expected string literal, got ident {}", tok.text())
                                    }
                                    AttrArgValueKind::Lit(lit) => match lit.kind() {
                                        crate::ast::lit::LitKind::String(s) => {
                                            assert_eq!(s.token().text(), "\"solidity\"")
                                        }
                                        _ => panic!("expected string literal"),
                                    },
                                    AttrArgValueKind::Expr(_) => {
                                        panic!("expected string literal, got expr")
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }

                2 => {
                    assert_eq!(attr.path().unwrap().text(), "feat");
                    let mut args = attr.args().unwrap().into_iter();

                    // foo::bar = 1
                    let arg = args.next().unwrap();
                    assert_eq!(arg.key().unwrap().text(), "foo::bar");
                    match arg.value().unwrap() {
                        AttrArgValueKind::Lit(l) => match l.kind() {
                            crate::ast::lit::LitKind::Int(i) => {
                                assert_eq!(i.token().text(), "1")
                            }
                            _ => panic!("expected int literal"),
                        },
                        AttrArgValueKind::Expr(_) => panic!("expected literal, got expr"),
                        _ => panic!("expected literal"),
                    }

                    // baz = false
                    let arg = args.next().unwrap();
                    assert_eq!(arg.key().unwrap().text(), "baz");
                    match arg.value().unwrap() {
                        AttrArgValueKind::Lit(l) => match l.kind() {
                            crate::ast::lit::LitKind::Bool(b) => {
                                assert_eq!(b.token().text(), "false")
                            }
                            _ => panic!("expected bool literal"),
                        },
                        AttrArgValueKind::Expr(_) => panic!("expected literal, got expr"),
                        _ => panic!("expected literal"),
                    }

                    // name = Foo (ident)
                    let arg = args.next().unwrap();
                    assert_eq!(arg.key().unwrap().text(), "name");
                    match arg.value().unwrap() {
                        AttrArgValueKind::Ident(tok) => {
                            assert_eq!(tok.text(), "Foo")
                        }
                        AttrArgValueKind::Expr(_) => panic!("expected ident, got expr"),
                        _ => panic!("expected ident"),
                    }

                    assert!(args.next().is_none());
                }

                _ => unreachable!(),
            }
        }
    }
}
