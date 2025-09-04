use std::convert::Infallible;

use crate::SyntaxKind;

use super::{Parser, define_scope, token_stream::TokenStream};

define_scope! { pub(crate) LitScope, Lit }
impl super::Parse for LitScope {
    type Error = Infallible;

    /// Caller is expected to verify that the next token is a literal.
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        assert!(is_lit(parser.current_kind().unwrap()));
        parser.bump();
        Ok(())
    }
}

pub fn is_lit(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Int | SyntaxKind::TrueKw | SyntaxKind::FalseKw | SyntaxKind::String
    )
}
