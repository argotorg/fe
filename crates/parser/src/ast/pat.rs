use rowan::ast::{AstNode, support};

use super::ast_node;
use crate::{SyntaxKind as SK, SyntaxToken};

ast_node! {
    /// A pattern.
    /// Use [`Self::kind`] to get the specific kind of the pattern.
    pub struct Pat,
    SK::WildCardPat
    | SK::RestPat
    | SK::LitPat
    | SK::TuplePat
    | SK::PathPat
    | SK::PathTuplePat
    | SK::RecordPat
    | SK::OrPat
}
impl Pat {
    /// Returns the specific kind of the pattern.
    pub fn kind(&self) -> PatKind {
        match self.syntax().kind() {
            SK::WildCardPat => PatKind::WildCard(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::RestPat => PatKind::Rest(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::LitPat => PatKind::Lit(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::TuplePat => PatKind::Tuple(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::PathPat => PatKind::Path(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::PathTuplePat => {
                PatKind::PathTuple(PathTuplePat::cast(self.syntax().clone()).unwrap())
            }
            SK::RecordPat => PatKind::Record(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::OrPat => PatKind::Or(AstNode::cast(self.syntax().clone()).unwrap()),
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// `_`
    pub struct WildCardPat,
    SK::WildCardPat,
}

ast_node! {
    /// `..`
    pub struct RestPat,
    SK::RestPat,
}

ast_node! {
    /// `1`
    pub struct LitPat,
    SK::LitPat,
}
impl LitPat {
    /// Returns the underlying literal.
    pub fn lit(&self) -> Option<super::Lit> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `(Foo::Bar, 1, ..)`
    pub struct TuplePat,
    SK::TuplePat,
}
impl TuplePat {
    pub fn elems(&self) -> Option<TuplePatElemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `(Foo::Bar, 1, ..)`
    pub struct TuplePatElemList,
    SK::TuplePatElemList,
    IntoIterator<Item=Pat>
}

ast_node! {
    /// `Foo::Bar`
    pub struct PathPat,
    SK::PathPat,
}
impl PathPat {
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    /// Returns the `mut` keyword if the patter is mutable.
    pub fn mut_token(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::MutKw)
    }
}

ast_node! {
    /// `Foo::Bar(1, 2)`
    pub struct PathTuplePat,
    SK::PathTuplePat,
}
impl PathTuplePat {
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }
    pub fn elems(&self) -> Option<TuplePatElemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `Foo::Bar{a: 1, b: Foo::baz, c}
    pub struct RecordPat,
    SK::RecordPat,
}
impl RecordPat {
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    pub fn fields(&self) -> Option<RecordPatFieldList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `{a: 1, b: Foo::baz, c}`
    pub struct RecordPatFieldList,
    SK::RecordPatFieldList,
    IntoIterator<Item=RecordPatField>
}

ast_node! {
    /// `a: 1`
    pub struct RecordPatField,
    SK::RecordPatField,
}
impl RecordPatField {
    /// Returns the field name.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the field pattern.
    pub fn pat(&self) -> Option<Pat> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `Foo::Bar | 1`
    pub struct OrPat,
    SK::OrPat,
}
impl OrPat {
    pub fn lhs(&self) -> Option<Pat> {
        support::child(self.syntax())
    }
    pub fn rhs(&self) -> Option<Pat> {
        support::children(self.syntax()).nth(1)
    }
}

/// A specific pattern kind.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum PatKind {
    WildCard(WildCardPat),
    Rest(RestPat),
    Lit(LitPat),
    Tuple(TuplePat),
    Path(PathPat),
    PathTuple(PathTuplePat),
    Record(RecordPat),
    Or(OrPat),
}

#[cfg(test)]
mod tests {
    use derive_more::TryIntoError;
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    fn parse_pat<T>(source: &str) -> T
    where
        T: TryFrom<PatKind, Error = TryIntoError<PatKind>>,
    {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        crate::parser::pat::parse_pat(&mut parser).unwrap();
        Pat::cast(parser.finish_to_node().0)
            .unwrap()
            .kind()
            .try_into()
            .unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn wildcard() {
        let _: WildCardPat = parse_pat("_");
    }

    #[test]
    #[wasm_bindgen_test]
    fn rest() {
        let _: RestPat = parse_pat("..");
    }

    #[test]
    #[wasm_bindgen_test]
    fn lit() {
        let _: LitPat = parse_pat("0x1");
        let _: LitPat = parse_pat("true");
        let _: LitPat = parse_pat(r#""foo""#);
    }

    #[test]
    #[wasm_bindgen_test]
    fn tuple() {
        let source = r#"(Foo::Bar, true, ..)"#;
        let tuple_pat: TuplePat = parse_pat(source);

        for (i, pat) in tuple_pat.elems().unwrap().iter().enumerate() {
            match i {
                0 => assert!(matches!(pat.kind(), PatKind::Path(_))),
                1 => assert!(matches!(pat.kind(), PatKind::Lit(_))),
                2 => assert!(matches!(pat.kind(), PatKind::Rest(_))),
                _ => panic!("unexpected tuple pat"),
            }
        }

        let tuple_pat: TuplePat = parse_pat("()");
        assert!(tuple_pat.elems().unwrap().iter().next().is_none());
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_tuple() {
        let source = r#"Self::Bar(1, Foo::Bar)"#;
        let path_tuple_pat: PathTuplePat = parse_pat(source);

        for (i, seg) in path_tuple_pat.path().unwrap().segments().enumerate() {
            match i {
                0 => assert!(seg.is_self_ty()),
                1 => assert_eq!(seg.ident().unwrap().text(), "Bar"),
                _ => panic!("unexpected path tuple pat"),
            }
        }

        for (i, pat) in path_tuple_pat.elems().unwrap().iter().enumerate() {
            match i {
                0 => assert!(matches!(pat.kind(), PatKind::Lit(_))),
                1 => assert!(matches!(pat.kind(), PatKind::Path(_))),
                _ => panic!("unexpected path tuple pat"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn record() {
        let source = r#"Foo::Bar{a: 1, b: Foo::baz, mut c}"#;
        let record_pat: RecordPat = parse_pat(source);

        for (i, seg) in record_pat.path().unwrap().segments().enumerate() {
            match i {
                0 => assert_eq!(seg.ident().unwrap().text(), "Foo"),
                1 => assert_eq!(seg.ident().unwrap().text(), "Bar"),
                _ => panic!("unexpected record pat"),
            }
        }

        for (i, field) in record_pat.fields().unwrap().iter().enumerate() {
            match i {
                0 => {
                    assert_eq!(field.name().unwrap().text(), "a");
                    assert!(matches!(field.pat().unwrap().kind(), PatKind::Lit(_)));
                }
                1 => {
                    assert_eq!(field.name().unwrap().text(), "b");
                    assert!(matches!(field.pat().unwrap().kind(), PatKind::Path(_)));
                }
                2 => {
                    let PatKind::Path(pat) = field.pat().unwrap().kind() else {
                        panic!("unexpected record pat");
                    };

                    assert!(field.name().is_none());
                    assert!(matches!(field.pat().unwrap().kind(), PatKind::Path(_)));
                    assert!(pat.mut_token().is_some());
                }
                _ => panic!("unexpected record pat"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn or() {
        let source = r#"Foo::Int | Foo::Float | Foo::Str "#;
        let or_pat: OrPat = parse_pat(source);

        assert!(matches!(or_pat.lhs().unwrap().kind(), PatKind::Path(_)));
        assert!(matches!(or_pat.rhs().unwrap().kind(), PatKind::Or(_)));
    }
}
