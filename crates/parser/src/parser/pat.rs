use std::convert::Infallible;

use super::{ErrProof, Parser, Recovery, define_scope, path::PathScope, token_stream::TokenStream};
use crate::{
    ParseError, SyntaxKind,
    parser::{
        lit::{LitScope, is_lit},
        parse_list,
        token_stream::LexicalToken,
    },
};

pub fn parse_pat<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    use SyntaxKind::*;
    parser.bump_trivias();
    let checkpoint = parser.checkpoint();
    let has_mut = parser.bump_if(SyntaxKind::MutKw);

    let token = parser.current_token();
    if has_mut {
        match token.as_ref().map(|t| t.syntax_kind()) {
            Some(Underscore | Dot2 | LParen) => {
                parser.error_msg_on_current_token(&format!(
                    "`mut` is not allowed on `{}`",
                    token.unwrap().text()
                ));
            }

            Some(kind) if is_lit(kind) => {
                parser.error_msg_on_current_token(&format!(
                    "`mut` is not allowed on `{}`",
                    token.unwrap().text()
                ));
            }

            _ => {}
        }
    }

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

    if parser.current_kind() == Some(SyntaxKind::Pipe) {
        parser.parse_cp(OrPatScope::default(), Some(checkpoint))?;
    }
    Ok(())
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
            self.set_kind(SyntaxKind::PathTuplePat);
            parser.parse(TuplePatElemListScope::default())
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            self.set_kind(SyntaxKind::RecordPat);
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
        let has_label = parser.dry_run(|parser| {
            //
            parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
        });
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
/// Tuple patterns like `Foo(a, b)` are rejected at parse time.
/// Other restrictions (e.g., on record field patterns) are validated semantically.
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
        parser.or_recover(|p| {
            let pos = p.current_pos;
            p.parse(PathScope::default())
                .map_err(|_| ParseError::expected(&[SyntaxKind::PathPat], None, pos))
        })?;

        parser.set_newline_as_trivia(false);
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                self.set_kind(SyntaxKind::RecordPat);
                // Use restricted record field list parsing for recv arms
                parser.parse(RecvArmRecordPatFieldListScope::default())
            }
            Some(SyntaxKind::LParen) => {
                // Tuple patterns are not allowed in recv arms - emit error but parse for recovery
                parser.error_msg_on_current_token(
                    "tuple patterns are not allowed in recv arms; use record pattern syntax `{ field, .. }` instead",
                );
                self.set_kind(SyntaxKind::PathTuplePat);
                parser.parse(TuplePatElemListScope::default())
            }
            _ => Ok(()),
        }
    }
}

// Restricted record pattern field list for recv arms.
// Allows destructuring patterns but rejects literal patterns (constraints).
define_scope! { RecvArmRecordPatFieldListScope, RecordPatFieldList, (Comma, RBrace) }
impl super::Parse for RecvArmRecordPatFieldListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::RecordPatFieldList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(RecvArmRecordPatFieldScope::default()),
        )
    }
}

// Restricted record pattern field for recv arms.
// The pattern inside can be:
// - An identifier (binding): `name` or `name: alias`
// - A wildcard: `name: _`
// - A rest pattern: `..`
// - A nested record pattern: `name: Foo { a, b }`
// - A nested tuple pattern: `name: (a, b)`
// But NOT:
// - A literal pattern: `name: 42` (pattern constraints)
define_scope! { RecvArmRecordPatFieldScope, RecordPatField }
impl super::Parse for RecvArmRecordPatFieldScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        let has_label = parser.dry_run(|parser| {
            parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
        });
        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_recv_arm_field_pat(parser)
    }
}

/// Parses a pattern for recv arm record fields with restrictions.
/// Allows destructuring patterns but rejects literal patterns (constraints).
fn parse_recv_arm_field_pat<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    use SyntaxKind::*;
    parser.bump_trivias();
    let checkpoint = parser.checkpoint();
    let has_mut = parser.bump_if(SyntaxKind::MutKw);

    let token = parser.current_token();
    if has_mut {
        match token.as_ref().map(|t| t.syntax_kind()) {
            Some(Underscore | Dot2 | LParen) => {
                parser.error_msg_on_current_token(&format!(
                    "`mut` is not allowed on `{}`",
                    token.unwrap().text()
                ));
            }

            Some(kind) if is_lit(kind) => {
                parser.error_msg_on_current_token(&format!(
                    "`mut` is not allowed on `{}`",
                    token.unwrap().text()
                ));
            }

            _ => {}
        }
    }

    match parser.current_kind() {
        Some(Underscore) => parser
            .parse_cp(WildCardPatScope::default(), Some(checkpoint))
            .unwrap(),
        Some(Dot2) => parser
            .parse_cp(RestPatScope::default(), Some(checkpoint))
            .unwrap(),
        Some(LParen) => parser.parse_cp(RecvArmTuplePatScope::default(), Some(checkpoint))?,
        Some(kind) if is_lit(kind) => {
            // Reject literal patterns in recv arm fields
            parser.error_msg_on_current_token(
                "literal patterns are not allowed in recv arm patterns; use a binding pattern instead",
            );
            // Still parse for error recovery
            parser
                .parse_cp(LitPatScope::default(), Some(checkpoint))
                .unwrap()
        }
        _ => parser.parse_cp(RecvArmFieldPathPatScope::default(), Some(checkpoint))?,
    };

    // Note: Or patterns are not allowed in recv arm fields - they would only make sense
    // with pattern constraints which are not supported
    Ok(())
}

// Path pattern scope for recv arm fields that recursively uses restricted parsing
define_scope! { RecvArmFieldPathPatScope, PathPat, (Pipe) }
impl super::Parse for RecvArmFieldPathPatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.or_recover(|p| {
            let pos = p.current_pos;
            p.parse(PathScope::default())
                .map_err(|_| ParseError::expected(&[SyntaxKind::PathPat], None, pos))
        })?;

        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            self.set_kind(SyntaxKind::PathTuplePat);
            // Allow tuple destructuring in nested patterns
            parser.parse(RecvArmTuplePatElemListScope::default())
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            self.set_kind(SyntaxKind::RecordPat);
            // Recursively use restricted record field parsing
            parser.parse(RecvArmRecordPatFieldListScope::default())
        } else {
            Ok(())
        }
    }
}

// Bare tuple pattern for recv arm fields (no path prefix)
define_scope! { RecvArmTuplePatScope, TuplePat }
impl super::Parse for RecvArmTuplePatScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.parse(RecvArmTuplePatElemListScope::default())
    }
}

// Tuple pattern element list for recv arm fields (allows nested patterns but not literals)
define_scope! { RecvArmTuplePatElemListScope, TuplePatElemList, (RParen, Comma) }
impl super::Parse for RecvArmTuplePatElemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::TuplePatElemList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            parse_recv_arm_field_pat,
        )
    }
}
