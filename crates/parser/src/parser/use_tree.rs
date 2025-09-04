use std::{cell::Cell, convert::identity, rc::Rc};

use crate::{ParseError, SyntaxKind, TextRange, parser::path::is_path_segment};

use super::{ErrProof, Parser, Recovery, define_scope, parse_list, token_stream::TokenStream};

define_scope! { pub(crate) UseTreeScope, UseTree }
impl super::Parse for UseTreeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        if let Some(SyntaxKind::LBrace) = parser.current_kind() {
            return parser.parse(UseTreeListScope::default());
        }

        let use_path_scope = UsePathScope::default();
        parser.parse_or_recover(use_path_scope.clone())?;
        let is_glob = use_path_scope.is_glob.get();

        if parser.current_kind() == Some(SyntaxKind::AsKw) {
            if is_glob {
                parser.error_msg_on_current_token("can't use `as` with `*`");
            }
            if parser.current_kind() == Some(SyntaxKind::AsKw) {
                parser.or_recover(|p| p.parse(UseTreeAliasScope::default()))?;
            }
            return Ok(());
        }

        if !parser.bump_if(SyntaxKind::Colon2) {
            return Ok(());
        }
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            if is_glob {
                parser.error_msg_on_current_token("can't use `*` with `{}`");
            }
            parser.parse(UseTreeListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { UseTreeListScope, UseTreeList, (Comma, RBrace) }
impl super::Parse for UseTreeListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::UseTreeList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(UseTreeScope::default()),
        )
    }
}

define_scope! {
    UsePathScope{ is_glob: Rc<Cell<bool>>},
    UsePath,
    (Colon2)
}
impl super::Parse for UsePathScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.parse(UsePathSegmentScope::default())?;

        loop {
            let is_path_segment = parser.dry_run(|parser| {
                parser.bump_if(SyntaxKind::Colon2)
                    && parser
                        .parse_ok(UsePathSegmentScope::default())
                        .is_ok_and(identity)
            });
            if is_path_segment {
                if self.is_glob.get() {
                    parser.error_msg_on_current_token("can't specify path after `*`");
                }
                parser.bump_expected(SyntaxKind::Colon2);
                self.is_glob
                    .set(parser.current_kind() == Some(SyntaxKind::Star));
                parser.parse(UsePathSegmentScope::default())?;
            } else {
                break;
            }
        }
        Ok(())
    }
}

define_scope! { UsePathSegmentScope, UsePathSegment }
impl super::Parse for UsePathSegmentScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        match parser.current_kind() {
            Some(kind) if is_use_path_segment(kind) => {
                parser.bump();
            }
            _ => {
                return Err(ParseError::Msg(
                    "expected identifier or `self`".into(),
                    TextRange::empty(parser.end_of_prev_token),
                ));
            }
        }
        Ok(())
    }
}

define_scope! { UseTreeAliasScope, UseTreeRename }
impl super::Parse for UseTreeAliasScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::AsKw);

        match parser.current_kind() {
            Some(SyntaxKind::Ident) => parser.bump_expected(SyntaxKind::Ident),
            Some(SyntaxKind::Underscore) => parser.bump_expected(SyntaxKind::Underscore),
            _ => {
                return Err(ParseError::Msg(
                    "expected identifier or `_`".into(),
                    TextRange::empty(parser.current_pos),
                ));
            }
        };
        Ok(())
    }
}

fn is_use_path_segment(kind: SyntaxKind) -> bool {
    is_path_segment(kind) || matches!(kind, SyntaxKind::Star)
}
