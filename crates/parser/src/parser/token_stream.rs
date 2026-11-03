use crate::SyntaxKind;

/// This trait works as an abstraction layer to encapsulate the differences
/// between input sources. There are mainly two types of input sources,
/// 1. text in source file
/// 2. tokens stream produced by procedural macros.
pub trait TokenStream {
    type Token: LexicalToken;

    /// Returns the next token in the stream.
    fn next(&mut self) -> Option<Self::Token>;
}

/// This trait represents a single token in the token stream.
pub trait LexicalToken: Clone {
    /// Returns `SyntaxKind` of the token.
    fn syntax_kind(&self) -> SyntaxKind;

    /// Returns raw text of the token.
    fn text(&self) -> &str;
}
