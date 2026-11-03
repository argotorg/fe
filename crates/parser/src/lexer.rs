use crate::{
    SyntaxKind,
    parser::token_stream::{LexicalToken, TokenStream},
};

pub struct Lexer<'s> {
    inner: logos::Lexer<'s, SyntaxKind>,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            inner: logos::Lexer::new(text),
        }
    }
}

impl<'s> TokenStream for Lexer<'s> {
    type Token = Token<'s>;

    fn next(&mut self) -> Option<Self::Token> {
        let syntax_kind = self.inner.next()?.unwrap_or(SyntaxKind::InvalidToken);

        Some(Token {
            syntax_kind,
            text: self.inner.slice(),
        })
    }
}

#[derive(Clone)]
pub struct Token<'s> {
    syntax_kind: SyntaxKind,
    text: &'s str,
}

impl LexicalToken for Token<'_> {
    fn syntax_kind(&self) -> SyntaxKind {
        self.syntax_kind
    }

    fn text(&self) -> &str {
        self.text
    }
}
