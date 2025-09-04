use rowan::ast::{AstNode, support};

use super::{AstChildren, ast_node};
use crate::{SyntaxKind as SK, SyntaxToken};

ast_node! {
    /// A type node.
    /// If you want to match a specific kind of type, use `[Type::kind]`.
    pub struct Type,
    SK::PtrType
    | SK::PathType
    | SK::SelfType
    | SK::TupleType
    | SK::ArrayType
    | SK::NeverType
}
impl Type {
    pub fn kind(&self) -> TypeKind {
        match self.syntax().kind() {
            SK::PtrType => TypeKind::Ptr(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::PathType => TypeKind::Path(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::TupleType => TypeKind::Tuple(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ArrayType => TypeKind::Array(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::NeverType => TypeKind::Never(AstNode::cast(self.syntax().clone()).unwrap()),
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// A pointer type.
    /// `*i32`
    pub struct PtrType,
    SK::PtrType,
}
impl PtrType {
    /// Returns the `*` token.
    pub fn star(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Star)
    }

    /// Returns the type pointed to.
    pub fn inner(&self) -> Option<Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A path type.
    /// `foo::Type<T, U + 2>`
    pub struct PathType,
    SK::PathType
}
impl PathType {
    /// Returns the path of the type.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }
}
impl super::GenericArgsOwner for PathType {}

ast_node! {
    /// A tuple type.
    /// `(i32, foo::Bar)`
    pub struct TupleType,
    SK::TupleType,
    IntoIterator<Item=Type>,
}
impl TupleType {
    pub fn l_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::LParen)
    }

    pub fn r_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::RParen)
    }

    /// Returns the types in the tuple.
    pub fn elem_tys(&self) -> AstChildren<Type> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// An array type.
    /// `[i32; 4]`
    pub struct ArrayType,
    SK::ArrayType,
}
impl ArrayType {
    pub fn l_bracket(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::LBracket)
    }

    pub fn r_bracket(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::LBracket)
    }
    /// Returns the type of the array elements.
    pub fn elem_ty(&self) -> Option<Type> {
        support::child(self.syntax())
    }

    /// Returns the length of the array.
    pub fn len(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct NeverType,
    SK::NeverType,
}

/// A specific kind of type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum TypeKind {
    Ptr(PtrType),
    Path(PathType),
    Tuple(TupleType),
    Array(ArrayType),
    Never(NeverType),
}

#[cfg(test)]
mod tests {
    use derive_more::TryIntoError;
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;
    use crate::{ast::prelude::*, lexer::Lexer, parser};

    fn parse_type<T>(source: &str) -> T
    where
        T: TryFrom<TypeKind, Error = TryIntoError<TypeKind>> + std::fmt::Debug,
    {
        let lexer = Lexer::new(source);
        let mut parser = parser::Parser::new(lexer);
        let _ = parser::type_::parse_type(&mut parser, None);
        Type::cast(parser.finish_to_node().0)
            .unwrap()
            .kind()
            .try_into()
            .unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn ptr_type() {
        let ptr_ty: PtrType = parse_type("*i32");

        assert_eq!(ptr_ty.star().unwrap().text(), "*");
        assert!(matches!(ptr_ty.inner().unwrap().kind(), TypeKind::Path(_)));
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_type() {
        let path_ty: PathType = parse_type("Foo::Bar<T, {U + 2}>");

        for (i, segment) in path_ty.path().unwrap().segments().enumerate() {
            match i {
                0 => assert_eq!(segment.ident().unwrap().text(), "Foo"),
                1 => {
                    assert_eq!(segment.ident().unwrap().text(), "Bar");
                    let generic_args = segment.generic_args().unwrap();
                    for (i, arg) in generic_args.iter().enumerate() {
                        match i {
                            0 => assert!(matches!(arg.kind(), crate::ast::GenericArgKind::Type(_))),
                            1 => {
                                assert!(matches!(arg.kind(), crate::ast::GenericArgKind::Const(_)))
                            }
                            _ => panic!(),
                        }
                    }
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn tuple_type() {
        let tuple_ty: TupleType = parse_type("((i32, u32), foo::Bar, *usize");

        for (i, ty) in tuple_ty.elem_tys().enumerate() {
            match i {
                0 => assert!(matches!(ty.kind(), TypeKind::Tuple(_))),
                1 => assert!(matches!(ty.kind(), TypeKind::Path(_))),
                2 => assert!(matches!(ty.kind(), TypeKind::Ptr(_))),
                _ => panic!(),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn array_type() {
        let array_ty: ArrayType = parse_type("[(i32, u32); 1]");

        assert!(matches!(
            array_ty.elem_ty().unwrap().kind(),
            TypeKind::Tuple(_)
        ));
        assert!(array_ty.len().is_some());
    }
}
