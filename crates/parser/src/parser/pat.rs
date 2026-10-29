use std::convert::Infallible;

use super::{
    Checkpoint, ErrProof, Parser, Recovery, define_scope, path::PathScope,
    token_stream::TokenStream,
};
use crate::{
    ParseError, SyntaxKind,
    parser::{
        lit::{LitScope, is_lit},
        parse_list,
        token_stream::LexicalToken,
    },
};

pub fn parse_pat<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    let checkpoint = parse_pat_operand(parser)?;
    if parser.current_kind() == Some(SyntaxKind::Pipe) {
        parser.parse_cp(OrPatScope::default(), Some(checkpoint))?;
    }
    Ok(())
}

/// A pattern with no `|` at its top: a parameter's, where a closure's `|`
/// ends the list. Returns where it starts.
pub(crate) fn parse_pat_operand<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    use SyntaxKind::*;
    parser.bump_trivias();
    let checkpoint = parser.checkpoint();
    bump_binding_marker(parser);

    match parser.current_kind() {
        Some(Underscore) => parser
            .parse_cp(WildCardPatScope::default(), Some(checkpoint))
            .unwrap(),
        Some(Dot2) => parser
            .parse_cp(RestPatScope::default(), Some(checkpoint))
            .unwrap(),
        Some(LParen) => parser.parse_cp(TuplePatScope::default(), Some(checkpoint))?,
        Some(kind) if is_lit(kind) => parser
            .parse_cp(LitPatScope::default(), Some(checkpoint))
            .unwrap(),
        _ => parser.parse_cp(PathPatScope::default(), Some(checkpoint))?,
    };
    Ok(checkpoint)
}

/// Bumps a binding's `mut`, `mut?`, `ref` or `var` marker, reporting one in
/// front of a pattern that binds nothing or a second marker.
fn bump_binding_marker<S: TokenStream>(parser: &mut Parser<S>) {
    use SyntaxKind::*;
    let Some(marker) = [MutKw, MaybeMutKw, RefKw, VarKw]
        .into_iter()
        .find(|kind| parser.bump_if(*kind))
    else {
        return;
    };
    let Some(token) = parser.current_token() else {
        return;
    };
    match token.syntax_kind() {
        Underscore | Dot2 | LParen => {}
        kind if is_lit(kind) => {}
        MutKw | MaybeMutKw | RefKw | VarKw => {
            parser.error_msg_on_current_token(&format!(
                "a binding takes one marker, not {} and {}",
                marker.describe(),
                token.syntax_kind().describe()
            ));
            return;
        }
        _ => return,
    }
    parser.error_msg_on_current_token(&format!(
        "{} is not allowed on `{}`",
        marker.describe(),
        token.text()
    ));
}

define_scope! { WildCardPatScope, WildCardPat, (Pipe) }
impl super::Parse for WildCardPatScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Underscore);
        Ok(())
    }
}

define_scope! { RestPatScope, RestPat }
impl super::Parse for RestPatScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Dot2);
        Ok(())
    }
}

define_scope! { LitPatScope, LitPat, (Pipe) }
impl super::Parse for LitPatScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.parse(LitScope::default())
    }
}

define_scope! { TuplePatScope, TuplePat }
impl super::Parse for TuplePatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.parse(TuplePatElemListScope::default())
    }
}

define_scope! { TuplePatElemListScope, TuplePatElemList, (RParen, Comma) }
impl super::Parse for TuplePatElemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::TuplePatElemList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            parse_pat,
        )
    }
}

define_scope! { PathPatScope, PathPat, (Pipe) }
impl super::Parse for PathPatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.or_recover(|p| {
            let pos = p.current_pos;
            p.parse(PathScope::default())
                .map_err(|_| ParseError::expected(&[SyntaxKind::PathPat], None, pos))
        })?;

        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.set_scope_kind(SyntaxKind::PathTuplePat);
            parser.parse(TuplePatElemListScope::default())
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.set_scope_kind(SyntaxKind::RecordPat);
            parser.parse(RecordPatFieldListScope::default())
        } else {
            Ok(())
        }
    }
}

define_scope! { RecordPatFieldListScope, RecordPatFieldList, (Comma, RBrace) }
impl super::Parse for RecordPatFieldListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::RecordPatFieldList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(RecordPatFieldScope::default()),
        )
    }
}

define_scope! { RecordPatFieldScope, RecordPatField }
impl super::Parse for RecordPatFieldScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        let has_label = matches!(
            parser.peek_n_non_trivia(2).as_slice(),
            [SyntaxKind::Ident, SyntaxKind::Colon]
        );
        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_pat(parser)
    }
}

define_scope! { OrPatScope, OrPat, (Pipe) }
impl super::Parse for OrPatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Pipe);
        parse_pat(parser)
    }
}

/// Parses a restricted pattern for recv arm patterns.
/// Only allows:
/// - Path patterns (e.g., `Baz`, `Foo::Bar`)
/// - Record patterns with path (e.g., `Bar { a, b, .. }`, `Foo::Bar { x, y: _, .. }`)
///
/// Tuple patterns like `Foo(a, b)` are rejected at parse time. A record
/// pattern's fields may be any patterns; type checking rejects an arm whose
/// pattern may not match every value of its variant.
pub fn parse_recv_arm_pat<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    parser.bump_trivias();
    parser.parse(RecvArmPatScope::default())
}

define_scope! { RecvArmPatScope, PathPat, (Arrow, LBrace, UsesKw) }
impl super::Parse for RecvArmPatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        if parser.current_kind() == Some(SyntaxKind::Underscore) {
            parser.set_scope_kind(SyntaxKind::WildCardPat);
            parser
                .parse(WildCardPatScope::default())
                .expect("wildcard recv arm pattern parsing should be infallible");
            return Ok(());
        }

        parser.or_recover(|p| {
            let pos = p.current_pos;
            p.parse(PathScope::default())
                .map_err(|_| ParseError::expected(&[SyntaxKind::PathPat], None, pos))
        })?;

        parser.set_newline_as_trivia(false);
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                parser.set_scope_kind(SyntaxKind::RecordPat);
                parser.parse(RecordPatFieldListScope::default())
            }
            Some(SyntaxKind::LParen) => {
                // Tuple patterns are not allowed in recv arms - emit error but parse for recovery
                parser.error_msg_on_current_token(
                    "tuple patterns are not allowed in recv arms; use record pattern syntax `{ field, .. }` instead",
                );
                parser.set_scope_kind(SyntaxKind::PathTuplePat);
                parser.parse(TuplePatElemListScope::default())
            }
            _ => Ok(()),
        }
    }
}
