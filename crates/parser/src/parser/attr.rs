use std::convert::Infallible;
use unwrap_infallible::UnwrapInfallible;

use super::path::PathScope;
use super::{
    Checkpoint, ErrProof, Parser, Recovery, define_scope, parse_list, token_stream::TokenStream,
};
use crate::{ExpectedKind, SyntaxKind};

pub(super) fn parse_attr_list<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<Option<Checkpoint>, Recovery<ErrProof>> {
    if let Some(SyntaxKind::DocComment) | Some(SyntaxKind::Pound) = parser.current_kind() {
        parser.parse_cp(AttrListScope::default(), None).map(Some)
    } else {
        Ok(None)
    }
}

define_scope! { pub(crate) AttrListScope, AttrList, (Newline) }
impl super::Parse for AttrListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(SyntaxKind::Pound) => {
                    parser.parse(AttrScope::default())?;
                }
                Some(SyntaxKind::DocComment) => parser
                    .parse(DocCommentAttrScope::default())
                    .unwrap_infallible(),
                _ => break,
            };
            parser.set_newline_as_trivia(false);
            if parser.find(
                SyntaxKind::Newline,
                ExpectedKind::Separator {
                    separator: SyntaxKind::Newline,
                    element: SyntaxKind::Attr,
                },
            )? {
                parser.bump();
            }
        }
        Ok(())
    }
}

define_scope! { AttrScope, Attr, (RBracket) }
impl super::Parse for AttrScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Pound);

        // Expect the opening bracket for a Rust-style outer attribute: #[ ... ]
        parser.bump_or_recover(SyntaxKind::LBracket, "expected `[` after `#`")?;

        // Parse the attribute path (e.g., foo, foo::bar). Recover on failure.
        parser.parse_or_recover(PathScope::default())?;

        // After the path, support either a meta list `(...)` or a name-value `= <expr>`.
        match parser.current_kind() {
            Some(SyntaxKind::LParen) => {
                parser.parse(AttrArgListScope::default())?;
            }
            Some(SyntaxKind::Eq) => {
                // Bump '=' then parse an expression value (e.g. `#[selector = sol_sig("...")]`).
                parser.bump();
                parser.parse(AttrValueExprScope::default())?;
            }
            _ => {}
        }

        // Expect the closing bracket of the attribute.
        parser.bump_or_recover(SyntaxKind::RBracket, "expected `]` to close attribute")?;
        Ok(())
    }
}

define_scope! { AttrArgListScope, AttrArgList, (Comma, RParen) }
impl super::Parse for AttrArgListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::AttrArgList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| parser.parse(AttrArgScope::default()),
        )
    }
}

define_scope! { AttrArgScope, AttrArg }
impl super::Parse for AttrArgScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // Parse the key as a path
        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Eq]);

        // TODO: this should be a "SimplePath" that doesn't allow generic args
        parser.parse_or_recover(PathScope::default())?;
        // Optional `= value`
        if parser.current_kind() == Some(SyntaxKind::Eq) {
            parser.bump();
            parser.parse(AttrArgValueScope::default())?;
        }
        Ok(())
    }
}

define_scope! { AttrArgValueScope, AttrArgValue }
impl super::Parse for AttrArgValueScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        use crate::parser::lit::{LitScope, is_lit};

        match parser.current_kind() {
            Some(kind) if is_lit(kind) => {
                // Parse a literal as a nested `Lit` node under `AttrArgValue`.
                parser.parse(LitScope::default()).unwrap_infallible();
                Ok(())
            }
            Some(SyntaxKind::Ident) => {
                parser.bump();
                Ok(())
            }
            _ => parser.error_and_recover("attribute value must be an ident or literal value"),
        }
    }
}

// Parses an expression value for the `#[attr = <expr>]` form.
//
// This is distinct from `AttrArgValueScope` (used in `#[attr(key = value)]`),
// which intentionally keeps values restricted to ident/literal for now.
define_scope! { AttrValueExprScope, AttrArgValue }
impl super::Parse for AttrValueExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        super::expr::parse_expr(parser)
    }
}

define_scope! { DocCommentAttrScope, DocCommentAttr }
impl super::Parse for DocCommentAttrScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::DocComment);
        parser.bump_if(SyntaxKind::Newline);
        Ok(())
    }
}
