use rowan::ast::{AstNode, support};

use super::ast_node;
use crate::{FeLang, SyntaxKind as SK, SyntaxToken, ast::Type};

ast_node! {
    /// A list of parameters.
    /// `(self, a: u256, b: u256)`
    pub struct FuncParamList,
    SK::FuncParamList,
    IntoIterator<Item=FuncParam>,
}

ast_node! {
    /// A single parameter.
    /// `self`
    /// `label a: u256`
    pub struct FuncParam,
    SK::FnParam,
}
impl FuncParam {
    /// Returns the `mut` keyword if the parameter is mutable.
    pub fn mut_token(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::MutKw)
    }

    /// Returns the `label` if the parameter is labeled.
    /// `label` in `label a: u256`.
    pub fn label(&self) -> Option<FuncParamLabel> {
        self.syntax()
            .children_with_tokens()
            .find_map(|child| match child {
                rowan::NodeOrToken::Token(token) => FuncParamLabel::from_token(token),
                _ => None,
            })
    }

    /// Returns the name of the parameter.
    /// `a` in `label a: u256`.
    pub fn name(&self) -> Option<FuncParamName> {
        let mut param_names = self.syntax().children_with_tokens().filter_map(|child| {
            if let rowan::NodeOrToken::Token(token) = child {
                FuncParamName::from_token(token)
            } else {
                None
            }
        });

        let first = param_names.next();
        match param_names.next() {
            Some(second) => Some(second),
            None => first,
        }
    }

    /// Returns the type of the parameter.
    /// `u256` in `a: u256`.
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A list of generic parameters.
    /// `<T: Trait, U>`
    pub struct GenericParamList,
    SK::GenericParamList,
    IntoIterator<Item=GenericParam>,

}

ast_node! {
    /// A generic parameter.
    /// `T`
    /// `T: Trait`
    /// `const N: usize`
    pub struct GenericParam,
    SK::TypeGenericParam | SK::ConstGenericParam,
}
impl GenericParam {
    /// Returns the specific kind of the generic parameter.
    pub fn kind(&self) -> GenericParamKind {
        match self.syntax().kind() {
            SK::TypeGenericParam => {
                GenericParamKind::Type(AstNode::cast(self.syntax().clone()).unwrap())
            }
            SK::ConstGenericParam => {
                GenericParamKind::Const(AstNode::cast(self.syntax().clone()).unwrap())
            }
            _ => unreachable!(),
        }
    }
}

/// A generic parameter kind.
/// `Type` is either `T` or `T: Trait`.
/// `Const` is `const N: usize`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum GenericParamKind {
    Type(TypeGenericParam),
    Const(ConstGenericParam),
}

impl GenericParamKind {
    pub fn syntax(&self) -> &rowan::SyntaxNode<FeLang> {
        match self {
            GenericParamKind::Type(param) => param.syntax(),
            GenericParamKind::Const(param) => param.syntax(),
        }
    }
}

ast_node! {
    /// `(label1: arg1, arg2, ..)`
    pub struct CallArgList,
    SK::CallArgList,
    IntoIterator<Item=CallArg>,
}

ast_node! {
    /// `label1: arg1`
    pub struct CallArg,
    SK::CallArg,
}
impl CallArg {
    /// Returns the label of the argument.
    /// `label1` in `label1: arg1`.
    pub fn label(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the expression of the argument.
    /// `arg1` in `label1: arg1`.
    pub fn expr(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A type generic parameter.
    /// `T`
    /// `T: Trait`
    pub struct TypeGenericParam,
    SK::TypeGenericParam,
}
impl TypeGenericParam {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn bounds(&self) -> Option<TypeBoundList> {
        support::child(self.syntax())
    }

    pub fn default_ty(&self) -> Option<Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A const generic parameter.
    /// `const N: usize`.
    pub struct ConstGenericParam,
    SK::ConstGenericParam,
}
impl ConstGenericParam {
    /// Returns the name of the const generic parameter.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn const_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::ConstKw)
    }

    /// Returns the type of the const generic parameter.
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A list of generic arguments.
    /// `<T,
    pub struct GenericArgList,
    SK::GenericArgList,
    IntoIterator<Item=GenericArg>,

}

ast_node! {
    /// A generic argument.
    /// `T`
    /// `T: Trait`
    /// `{expr}`
    /// `lit`
    /// `Output = u64`
    pub struct GenericArg,
    SK::TypeGenericArg | SK::ConstGenericArg | SK::AssocTypeGenericArg,
}
impl GenericArg {
    pub fn kind(&self) -> GenericArgKind {
        match self.syntax().kind() {
            SK::TypeGenericArg => {
                GenericArgKind::Type(AstNode::cast(self.syntax().clone()).unwrap())
            }
            SK::ConstGenericArg => {
                GenericArgKind::Const(AstNode::cast(self.syntax().clone()).unwrap())
            }
            SK::AssocTypeGenericArg => {
                GenericArgKind::AssocType(AstNode::cast(self.syntax().clone()).unwrap())
            }
            _ => unreachable!(),
        }
    }
}

ast_node! {
    pub struct TypeGenericArg,
    SK::TypeGenericArg,
}
impl TypeGenericArg {
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct ConstGenericArg,
    SK::ConstGenericArg,
}
impl ConstGenericArg {
    pub fn expr(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct AssocTypeGenericArg,
    SK::AssocTypeGenericArg,
}
impl AssocTypeGenericArg {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `where T: Trait`
    pub struct WhereClause,
    SK::WhereClause,
    IntoIterator<Item=WherePredicate>,
}
impl WhereClause {
    pub fn where_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::WhereKw)
    }
}

ast_node! {
    /// `T: Trait`
    pub struct WherePredicate,
    SK::WherePredicate,
}
impl WherePredicate {
    /// Returns `T` in `T: Trait`.
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns `Trait` in `T: Trait`.
    pub fn bounds(&self) -> Option<TypeBoundList> {
        support::child(self.syntax())
    }
}

/// A generic argument kind.
/// `Type` is either `Type` or `T: Trait`.
/// `Const` is either `{expr}` or `lit`.
/// `AssocType` is `Output = u64`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum GenericArgKind {
    Type(TypeGenericArg),
    Const(ConstGenericArg),
    AssocType(AssocTypeGenericArg),
}

ast_node! {
    /// A type bound list.
    /// `: Trait + Trait2`
    pub struct TypeBoundList,
    SK::TypeBoundList,
    IntoIterator<Item=TypeBound>,
}

ast_node! {
    /// A type bound.
    /// `Trait`
    /// `Trait<T, U>`
    /// `(* -> *) -> *`
    pub struct TypeBound,
    SK::TypeBound,
}
impl TypeBound {
    /// A path of the type bound.
    pub fn trait_bound(&self) -> Option<TraitRef> {
        support::child(self.syntax())
    }

    pub fn kind_bound(&self) -> Option<KindBound> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct TraitRef,
    SK::TraitRef
}
impl TraitRef {
    /// A path to the trait.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    /// A generic argument list for the trait.
    pub fn generic_args(&self) -> Option<GenericArgList> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct KindBound,
     SK::KindBoundAbs | SK::KindBoundMono
}
impl KindBound {
    pub fn mono(&self) -> Option<KindBoundMono> {
        match self.syntax().kind() {
            SK::KindBoundMono => Some(KindBoundMono::cast(self.syntax().clone()).unwrap()),
            _ => None,
        }
    }

    pub fn abs(&self) -> Option<KindBoundAbs> {
        match self.syntax().kind() {
            SK::KindBoundAbs => Some(KindBoundAbs::cast(self.syntax().clone()).unwrap()),
            _ => None,
        }
    }
}

ast_node! {
    pub struct KindBoundMono,
    SK::KindBoundMono,
}

ast_node! {
    pub struct KindBoundAbs,
    SK::KindBoundAbs,
}
impl KindBoundAbs {
    pub fn lhs(&self) -> Option<KindBound> {
        support::child(self.syntax())
    }

    pub fn rhs(&self) -> Option<KindBound> {
        support::children(self.syntax()).nth(1)
    }

    pub fn arrow(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Arrow)
    }
}

#[derive(Debug, Clone)]
pub enum KindBoundVariant {
    /// `*`
    Mono(KindBoundMono),
    /// `KindBound -> KindBound`
    Abs(KindBoundAbs),
}

/// A trait for AST nodes that can have generic parameters.
pub trait GenericParamsOwner: AstNode<Language = FeLang> {
    /// Returns the generic parameter list of the node.
    fn generic_params(&self) -> Option<GenericParamList> {
        support::child(self.syntax())
    }
}

/// A trait for AST nodes that can have generic arguments.
pub trait GenericArgsOwner: AstNode<Language = FeLang> {
    /// Returns the generic argument list of the node.
    fn generic_args(&self) -> Option<GenericArgList> {
        support::child(self.syntax())
    }
}

/// A trait for AST nodes that can have a where clause.
pub trait WhereClauseOwner: AstNode<Language = FeLang> {
    /// Returns the where clause of the node.
    fn where_clause(&self) -> Option<WhereClause> {
        support::child(self.syntax())
    }
}

pub enum FuncParamLabel {
    /// `label` in `label a: u256`
    Ident(SyntaxToken),
    /// `_` in `_ a: u256`.
    Underscore(SyntaxToken),
}
impl FuncParamLabel {
    pub fn syntax(&self) -> SyntaxToken {
        match self {
            FuncParamLabel::Ident(token) => token,
            FuncParamLabel::Underscore(token) => token,
        }
        .clone()
    }

    fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Ident => Some(FuncParamLabel::Ident(token)),
            SK::Underscore => Some(FuncParamLabel::Underscore(token)),
            _ => None,
        }
    }
}

pub enum FuncParamName {
    /// `a` in `label a: u256`
    Ident(SyntaxToken),
    /// `self` parameter.
    SelfParam(SyntaxToken),
    /// `_` parameter.
    Underscore(SyntaxToken),
}
impl FuncParamName {
    pub fn syntax(&self) -> SyntaxToken {
        match self {
            FuncParamName::Ident(token) => token,
            FuncParamName::SelfParam(token) => token,
            FuncParamName::Underscore(token) => token,
        }
        .clone()
    }

    fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Ident => Some(FuncParamName::Ident(token)),
            SK::SelfKw => Some(FuncParamName::SelfParam(token)),
            SK::Underscore => Some(FuncParamName::Underscore(token)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::TypeKind,
        lexer::Lexer,
        parser::{
            Parser,
            param::{GenericArgListScope, GenericParamListScope, WhereClauseScope},
        },
    };

    use wasm_bindgen_test::wasm_bindgen_test;

    fn parse_generic_params(source: &str) -> GenericParamList {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(GenericParamListScope::default()).unwrap();
        GenericParamList::cast(parser.finish_to_node().0).unwrap()
    }

    fn parse_generic_arg(source: &str) -> GenericArgList {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(GenericArgListScope::default()).unwrap();
        GenericArgList::cast(parser.finish_to_node().0).unwrap()
    }

    fn parse_where_clause(source: &str) -> WhereClause {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(WhereClauseScope::default()).unwrap();
        WhereClause::cast(parser.finish_to_node().0).unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn generic_param() {
        let source = r#"<T: Trait + Trait2<X, Y>, U, const N: usize>"#;
        let gp = parse_generic_params(source);
        let mut params = gp.into_iter();

        let GenericParamKind::Type(p1) = params.next().unwrap().kind() else {
            panic!("expected type param");
        };
        assert_eq!(p1.name().unwrap().text(), "T");
        let p1_bounds = p1.bounds().unwrap();
        let mut p1_bounds = p1_bounds.iter();

        assert_eq!(
            p1_bounds
                .next()
                .unwrap()
                .trait_bound()
                .unwrap()
                .path()
                .unwrap()
                .segments()
                .next()
                .unwrap()
                .ident()
                .unwrap()
                .text(),
            "Trait"
        );
        let p1_bounds_trait2 = p1_bounds.next().unwrap();

        assert_eq!(
            p1_bounds_trait2
                .trait_bound()
                .unwrap()
                .path()
                .unwrap()
                .segments()
                .next()
                .unwrap()
                .ident()
                .unwrap()
                .text(),
            "Trait2"
        );

        let GenericParamKind::Type(p2) = params.next().unwrap().kind() else {
            panic!("expected type param");
        };
        assert_eq!(p2.name().unwrap().text(), "U");

        let GenericParamKind::Const(p3) = params.next().unwrap().kind() else {
            panic!("expected const param");
        };
        assert_eq!(p3.name().unwrap().text(), "N");
        assert!(p3.ty().is_some());
    }

    #[test]
    #[wasm_bindgen_test]
    fn generic_arg() {
        let source = r#"<T, "foo">"#;
        let ga = parse_generic_arg(source);
        let mut args = ga.iter();

        let GenericArgKind::Type(_) = args.next().unwrap().kind() else {
            panic!("expected type arg");
        };
        let GenericArgKind::Const(a2) = args.next().unwrap().kind() else {
            panic!("expected const arg");
        };
        assert!(a2.expr().is_some());
    }

    #[test]
    #[wasm_bindgen_test]
    fn generic_arg_with_assoc_type() {
        let source = r#"<T, Output = u64>"#;
        let ga = parse_generic_arg(source);
        let mut args = ga.into_iter();

        let GenericArgKind::Type(_) = args.next().unwrap().kind() else {
            panic!("expected type arg");
        };
        let GenericArgKind::AssocType(a2) = args.next().unwrap().kind() else {
            panic!("expected associated type arg");
        };
        assert_eq!(a2.name().unwrap().text(), "Output");
        assert!(a2.ty().is_some());
    }

    #[test]
    #[wasm_bindgen_test]
    fn where_clause() {
        let source = r#"where
            T: Trait + Trait2<X, Y>
            *U: Trait3
            (T, U): Trait4 + Trait5
        "#;
        let wc = parse_where_clause(source);
        let mut count = 0;
        for pred in wc {
            match count {
                0 => {
                    assert!(matches!(pred.ty().unwrap().kind(), TypeKind::Path(_)));
                    assert_eq!(pred.bounds().unwrap().iter().count(), 2);
                }
                1 => {
                    assert!(matches!(pred.ty().unwrap().kind(), TypeKind::Ptr(_)));
                    assert_eq!(pred.bounds().unwrap().iter().count(), 1);
                }
                2 => {
                    assert!(matches!(pred.ty().unwrap().kind(), TypeKind::Tuple(_)));
                    assert_eq!(pred.bounds().unwrap().iter().count(), 2);
                }
                _ => panic!("unexpected predicate"),
            }
            count += 1;
        }
        assert!(count == 3);
    }

    #[test]
    #[wasm_bindgen_test]
    fn generic_param_with_assoc_type() {
        let source = r#"<T: Iterator<Item = i32>>"#;
        let gp = parse_generic_params(source);
        let mut params = gp.into_iter();

        let GenericParamKind::Type(p1) = params.next().unwrap().kind() else {
            panic!("expected type param");
        };
        assert_eq!(p1.name().unwrap().text(), "T");

        let p1_bounds = p1.bounds().unwrap();
        let mut p1_bounds = p1_bounds.iter();

        let bound = p1_bounds.next().unwrap();
        let trait_ref = bound.trait_bound().unwrap();
        let trait_path = trait_ref.path().unwrap();
        assert_eq!(
            trait_path
                .segments()
                .next()
                .unwrap()
                .ident()
                .unwrap()
                .text(),
            "Iterator"
        );

        // Check generic args on the trait path's last segment
        let last_segment = trait_path.segments().next().unwrap();
        let generic_args = last_segment.generic_args().unwrap();
        let mut args = generic_args.into_iter();

        let GenericArgKind::AssocType(assoc_arg) = args.next().unwrap().kind() else {
            panic!("expected associated type arg");
        };
        assert_eq!(assoc_arg.name().unwrap().text(), "Item");
        assert!(assoc_arg.ty().is_some());
    }
}
