use std::convert::Infallible;

use super::{
    Checkpoint, ErrProof, Parser, Recovery, define_scope,
    expr::parse_expr,
    param::GenericArgListScope,
    parse_list,
    path::{PathScope, is_path_segment},
    token_stream::TokenStream,
};
use crate::{ExpectedKind, ParseError, SyntaxKind};

pub fn parse_type<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: Option<Checkpoint>,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    match parser.current_kind() {
        Some(SyntaxKind::Star) => parser.parse_cp(PtrTypeScope::default(), checkpoint),
        Some(SyntaxKind::LParen) => parser.parse_cp(TupleTypeScope::default(), checkpoint),
        Some(SyntaxKind::LBracket) => parser.parse_cp(ArrayTypeScope::default(), checkpoint),
        Some(SyntaxKind::Not) => parser
            .parse_cp(NeverTypeScope::default(), checkpoint)
            .map_err(|e| e.into()),
        _ => parser.parse_cp(PathTypeScope::default(), checkpoint),
    }
}

pub(crate) fn is_type_start(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::Star | SyntaxKind::SelfTypeKw | SyntaxKind::LParen | SyntaxKind::LBracket => {
            true
        }
        kind if is_path_segment(kind) => true,
        _ => false,
    }
}

define_scope!(PtrTypeScope, PtrType);
impl super::Parse for PtrTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Star);
        parse_type(parser, None).map(|_| ())
    }
}

define_scope!(pub(crate) PathTypeScope , PathType);
impl super::Parse for PathTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        parser.or_recover(|p| {
            p.parse(PathScope::default()).map_err(|_| {
                ParseError::expected(&[SyntaxKind::PathType], None, p.end_of_prev_token)
            })
        })?;

        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { pub(crate) TupleTypeScope, TupleType, (RParen, Comma) }
impl super::Parse for TupleTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::TupleType,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| {
                parse_type(parser, None)?;
                Ok(())
            },
        )
    }
}

define_scope! { ArrayTypeScope, ArrayType }
impl super::Parse for ArrayTypeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);

        parser.set_scope_recovery_stack(&[SyntaxKind::SemiColon, SyntaxKind::RBracket]);

        parse_type(parser, None)?;

        if parser.find_and_pop(SyntaxKind::SemiColon, ExpectedKind::Unspecified)? {
            parser.bump();
        }

        parse_expr(parser)?;

        if parser.find_and_pop(
            SyntaxKind::RBracket,
            ExpectedKind::ClosingBracket {
                bracket: SyntaxKind::RBracket,
                parent: SyntaxKind::ArrayType,
            },
        )? {
            parser.bump();
        }
        Ok(())
    }
}

define_scope! {NeverTypeScope, NeverType}
impl super::Parse for NeverTypeScope {
    type Error = Recovery<Infallible>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Not);
        Ok(())
    }
}
