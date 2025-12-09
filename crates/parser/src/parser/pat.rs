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
            p.parse(PathScope::default())
                .map_err(|e| ParseError::expected(&[SyntaxKind::PathPat], None, e.range().start()))
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
