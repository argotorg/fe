use std::convert::Infallible;

use super::{
    Checkpoint, ErrProof, Parser, Recovery, define_scope,
    expr::parse_expr,
    param::GenericArgListScope,
    parse_list,
    path::{PathScope, is_path_segment, is_qualified_type},
    token_stream::TokenStream,
};
use crate::{ExpectedKind, ParseError, SyntaxKind};

pub fn parse_type<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: Option<Checkpoint>,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    if starts_view_mode_type(parser) {
        return parser.parse_cp(ModeTypeScope::default(), checkpoint);
    }
    match parser.current_kind() {
        Some(SyntaxKind::Star) => parser.parse_cp(PtrTypeScope::default(), checkpoint),
        Some(SyntaxKind::MutKw | SyntaxKind::RefKw | SyntaxKind::OwnKw) => {
            parser.parse_cp(ModeTypeScope::default(), checkpoint)
        }
        Some(SyntaxKind::LParen) => parser.parse_cp(TupleTypeScope::default(), checkpoint),
        Some(SyntaxKind::LBracket) => parser.parse_cp(ArrayTypeScope::default(), checkpoint),
        Some(SyntaxKind::Not) => parser
            .parse_cp(NeverTypeScope::default(), checkpoint)
            .map_err(|e| e.into()),
        _ => parser.parse_cp(PathTypeScope::default(), checkpoint),
    }
}

pub(crate) fn parse_closure_param_type<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    let bare_view_mode = parser.is_ident("view")
        && matches!(
            parser.peek_n_non_trivia(2).get(1),
            Some(SyntaxKind::Comma | SyntaxKind::Pipe)
        );
    if starts_view_mode_type(parser)
        || bare_view_mode
        || matches!(
            parser.current_kind(),
            Some(SyntaxKind::MutKw | SyntaxKind::RefKw | SyntaxKind::OwnKw)
        )
    {
        parser.parse(ModeTypeScope::new(true))
    } else {
        parse_type(parser, None).map(|_| ())
    }
}

fn starts_view_mode_type<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    if !parser.is_ident("view") {
        return false;
    }
    let Some(next) = parser.peek_n_non_trivia(2).get(1).copied() else {
        return false;
    };
    if next == SyntaxKind::Lt {
        return parser.dry_run(|parser| {
            parser.bump();
            is_qualified_type(parser)
        });
    }
    is_type_start(next)
}

pub(crate) fn is_type_start(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::Star
        | SyntaxKind::Not
        | SyntaxKind::SelfTypeKw
        | SyntaxKind::LParen
        | SyntaxKind::LBracket => true,
        SyntaxKind::MutKw | SyntaxKind::RefKw | SyntaxKind::OwnKw => true,
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

define_scope!(
    ModeTypeScope {
        allow_missing_inner: bool
    },
    ModeType
);
impl super::Parse for ModeTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        if !parser.is_ident("view") {
            parser.expect(
                &[SyntaxKind::MutKw, SyntaxKind::RefKw, SyntaxKind::OwnKw],
                None,
            )?;
        }
        parser.bump();
        if self.allow_missing_inner {
            let newline_as_trivia = parser.set_newline_as_trivia(true);
            let missing_inner = matches!(
                parser.current_kind(),
                Some(SyntaxKind::Comma | SyntaxKind::Pipe)
            );
            parser.set_newline_as_trivia(newline_as_trivia);
            if missing_inner {
                return Ok(());
            }
        }
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
