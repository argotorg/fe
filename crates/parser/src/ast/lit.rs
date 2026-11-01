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
            SK::Byte => LitKind::Byte(LitByte { token }),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitByte {
    token: SyntaxToken,
}
impl LitByte {
    pub fn token(&self) -> &SyntaxToken {
        &self.token
    }

    /// Decodes a lexed byte literal. Error ranges are relative to its token.
    pub fn value(&self) -> Result<u8, (&'static str, TextRange)> {
        decode_byte_literal(self.token.text())
    }
}

/// Decodes a byte literal `b'..'`: one ASCII character, or an escape for a
/// control character, a quote, a backslash or any byte (`\xNN`).
pub(crate) fn decode_byte_literal(text: &str) -> Result<u8, (&'static str, TextRange)> {
    // Offsets within the literal's contents, which follow `b'`.
    let range = |start: usize, end: usize| {
        TextRange::new(
            TextSize::try_from(start + 2).expect("token offset fits TextSize"),
            TextSize::try_from(end + 2).expect("token offset fits TextSize"),
        )
    };
    let contents = &text[2..text.len() - 1];
    let mut chars = contents.char_indices().peekable();
    let byte = match chars.next() {
        None => {
            let len = TextSize::try_from(text.len()).expect("token length fits TextSize");
            return Err(("empty byte literal", TextRange::up_to(len)));
        }
        Some((_, '\\')) => {
            // The lexer ends a literal only after an escape's character.
            let (_, escape) = chars.next().expect("escaped character");
            match escape {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => 0,
                '\\' => b'\\',
                '\'' => b'\'',
                '"' => b'"',
                'x' => {
                    let digits: String = chars.by_ref().take(2).map(|(_, ch)| ch).collect();
                    let end = chars.peek().map_or(contents.len(), |&(end, _)| end);
                    if digits.len() != 2 || !digits.bytes().all(|b| b.is_ascii_hexdigit()) {
                        return Err(("invalid byte escape", range(0, end)));
                    }
                    u8::from_str_radix(&digits, 16).expect("two hex digits")
                }
                _ => return Err(("invalid byte escape", range(0, 1 + escape.len_utf8()))),
            }
        }
        Some((_, ch)) if ch.is_ascii() => ch as u8,
        Some((_, ch)) => {
            return Err((
                "non-ASCII character in byte literal; use a `\\x` escape",
                range(0, ch.len_utf8()),
            ));
        }
    };
    if chars.peek().is_some() {
        return Err((
            "byte literal holds more than one character",
            range(0, contents.len()),
        ));
    }
    Ok(byte)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum LitKind {
    Int(LitInt),
    Bool(LitBool),
    String(LitString),
    Byte(LitByte),
}
