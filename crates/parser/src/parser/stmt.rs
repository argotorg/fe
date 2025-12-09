use std::convert::Infallible;

use unwrap_infallible::UnwrapInfallible;

use crate::{ExpectedKind, SyntaxKind};

use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr::{parse_expr, parse_expr_no_struct},
    expr_atom::BlockExprScope,
    pat::parse_pat,
    token_stream::TokenStream,
    type_::parse_type,
};

pub fn parse_stmt<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    use SyntaxKind::*;

    match parser.current_kind() {
        Some(LetKw) => parser.parse(LetStmtScope::default()),
        Some(ForKw) => parser.parse(ForStmtScope::default()),
        Some(WhileKw) => parser.parse(WhileStmtScope::default()),
        Some(ContinueKw) => {
            parser
                .parse(ContinueStmtScope::default())
                .unwrap_infallible();
            Ok(())
        }
        Some(BreakKw) => {
            parser.parse(BreakStmtScope::default()).unwrap_infallible();
            Ok(())
        }
        Some(ReturnKw) => parser.parse(ReturnStmtScope::default()),
        _ => parser.parse(ExprStmtScope::default()),
    }
}

define_scope! { LetStmtScope, LetStmt }
impl super::Parse for LetStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LetKw);
        parser.set_newline_as_trivia(false);
        parse_pat(parser)?;

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.bump_expected(SyntaxKind::Colon);
            parse_type(parser, None)?;
        }

        if parser.bump_if(SyntaxKind::Eq) {
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ForStmtScope, ForStmt }
impl super::Parse for ForStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ForKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::InKw, SyntaxKind::Ident, SyntaxKind::LBrace]);
        parse_pat(parser)?;

        if parser.find_and_pop(SyntaxKind::InKw, ExpectedKind::Unspecified)? {
            parser.bump();
        }
        parse_expr_no_struct(parser)?;

        // pop `Ident` recovery token, which is only included because it solves a contrived test case
        parser.pop_recovery_stack();

        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::ForStmt))? {
            parser.parse(BlockExprScope::default())?;
        }
        Ok(())
    }
}

define_scope! { WhileStmtScope, WhileStmt }
impl super::Parse for WhileStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::WhileKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::LBrace]);
        parse_expr_no_struct(parser)?;

        if parser.find_and_pop(
            SyntaxKind::LBrace,
            ExpectedKind::Body(SyntaxKind::WhileStmt),
        )? {
            parser.parse(BlockExprScope::default())?;
        }
        Ok(())
    }
}

define_scope! { ContinueStmtScope, ContinueStmt }
impl super::Parse for ContinueStmtScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ContinueKw);
        Ok(())
    }
}

define_scope! { BreakStmtScope, BreakStmt }
impl super::Parse for BreakStmtScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::BreakKw);
        Ok(())
    }
}

define_scope! { ReturnStmtScope, ReturnStmt }
impl super::Parse for ReturnStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ReturnKw);
        parser.set_newline_as_trivia(false);

        if !matches!(
            parser.current_kind(),
            None | Some(SyntaxKind::Newline | SyntaxKind::RBrace)
        ) {
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ExprStmtScope, ExprStmt }
impl super::Parse for ExprStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_expr(parser)
    }
}
