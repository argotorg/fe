use std::convert::Infallible;

use crate::{ParseError, SyntaxKind, TextRange, ast::decode_string_literal};

use super::{
    Parser, define_scope,
    token_stream::{LexicalToken, TokenStream},
};

define_scope! { pub(crate) LitScope, Lit }
impl super::Parse for LitScope {
    type Error = Infallible;

    /// Caller is expected to verify that the next token is a literal.
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        let token = parser.current_token().expect("literal token");
        assert!(is_lit(token.syntax_kind()));
        if token.syntax_kind() == SyntaxKind::String
            && let Err(range) = decode_string_literal(token.text())
        {
            parser.bump_trivias();
            let start = parser.current_pos;
            parser.add_error(ParseError::Msg(
                "invalid string escape".to_string(),
                TextRange::new(start + range.start(), start + range.end()),
            ));
        }
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
