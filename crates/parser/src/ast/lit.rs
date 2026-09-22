use rowan::ast::AstNode;

use crate::{SyntaxToken, TextRange, TextSize, syntax_kind::SyntaxKind as SK};

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

    /// Decodes a lexed string literal. Error ranges are relative to its token.
    pub fn value(&self) -> Result<String, TextRange> {
        decode_string_literal(self.token.text())
    }
}

/// The lexer retains source spelling; all semantic consumers use decoded text.
/// Keep the documented quote, backslash and control escapes in one place.
pub(crate) fn decode_string_literal(text: &str) -> Result<String, TextRange> {
    let contents = &text[1..text.len() - 1];
    let mut decoded = String::with_capacity(contents.len());
    let mut chars = contents.char_indices();
    while let Some((start, ch)) = chars.next() {
        if ch != '\\' {
            decoded.push(ch);
            continue;
        }
        let start = TextSize::try_from(start + 1).expect("token offset fits TextSize");
        let (end, escape) = chars
            .next()
            .ok_or(TextRange::at(start, TextSize::from(1)))?;
        let value = match escape {
            '"' => '"',
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            _ => {
                let end = TextSize::try_from(end + 1 + escape.len_utf8())
                    .expect("token offset fits TextSize");
                return Err(TextRange::new(start, end));
            }
        };
        decoded.push(value);
    }
    Ok(decoded)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum LitKind {
    Int(LitInt),
    Bool(LitBool),
    String(LitString),
}
