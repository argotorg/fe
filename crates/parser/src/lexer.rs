use crate::{
    SyntaxKind,
    parser::token_stream::{LexicalToken, TokenStream},
};

pub struct Lexer<'s> {
    peek: Option<Token<'s>>,
    inner: logos::Lexer<'s, SyntaxKind>,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            peek: None,
            inner: logos::Lexer::new(text),
        }
    }
}

impl<'s> TokenStream for Lexer<'s> {
    type Token = Token<'s>;

    fn next(&mut self) -> Option<Self::Token> {
        if let Some(token) = self.peek.take() {
            return Some(token);
        }

        let syntax_kind = self.inner.next()?.unwrap_or(SyntaxKind::InvalidToken);

        Some(Token {
            syntax_kind,
            text: self.inner.slice(),
        })
    }

    fn peek(&mut self) -> Option<&Self::Token> {
        if self.peek.is_none() {
            self.peek = self.next();
        }
        self.peek.as_ref()
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
