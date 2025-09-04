use rowan::ast::AstNode;

use crate::{SyntaxToken, syntax_kind::SyntaxKind as SK};

use super::ast_node;

ast_node! {
    pub struct Lit,
    SK::Lit
}
impl Lit {
    pub fn kind(&self) -> LitKind {
        let token = self.syntax().first_token().unwrap();
        match token.kind() {
            SK::Int => LitKind::Int(LitInt { token }),
            SK::TrueKw | SK::FalseKw => LitKind::Bool(LitBool { token }),
            SK::String => LitKind::String(LitString { token }),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitInt {
    pub(super) token: SyntaxToken,
}
impl LitInt {
    pub fn token(&self) -> &SyntaxToken {
        &self.token
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitBool {
    token: SyntaxToken,
}
impl LitBool {
    pub fn token(&self) -> &SyntaxToken {
        &self.token
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitString {
    token: SyntaxToken,
}
impl LitString {
    pub fn token(&self) -> &SyntaxToken {
        &self.token
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum LitKind {
    Int(LitInt),
    Bool(LitBool),
    String(LitString),
}
