use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr_atom::BlockExprScope,
    param::{FuncParamListScope, parse_generic_params_opt, parse_where_clause_opt},
    token_stream::TokenStream,
    type_::parse_type,
};
use crate::{ExpectedKind, SyntaxKind};

define_scope! {
    pub(crate) FuncScope {
        fn_def_scope: FuncDefScope
    },
    Func
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum FuncDefScope {
    Normal,
    Impl,
    TraitDef,
    Extern,
}
impl Default for FuncDefScope {
    fn default() -> Self {
        Self::Normal
    }
}

impl super::Parse for FuncScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::FnKw);

        match self.fn_def_scope {
            FuncDefScope::Normal => parse_normal_fn_def_impl(parser, false),
            FuncDefScope::Impl => parse_normal_fn_def_impl(parser, true),
            FuncDefScope::TraitDef => parse_trait_fn_def_impl(parser),
            FuncDefScope::Extern => parse_extern_fn_def_impl(parser),
        }
    }
}

fn parse_normal_fn_def_impl<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_self: bool,
) -> Result<(), Recovery<ErrProof>> {
    parser.set_scope_recovery_stack(&[
        SyntaxKind::Ident,
        SyntaxKind::Lt,
        SyntaxKind::LParen,
        SyntaxKind::Arrow,
        SyntaxKind::WhereKw,
        SyntaxKind::LBrace,
    ]);

    if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Func))? {
        parser.bump();
    }

    parser.expect_and_pop_recovery_stack()?;
    parse_generic_params_opt(parser, false)?;

    if parser.find_and_pop(
        SyntaxKind::LParen,
        ExpectedKind::Syntax(SyntaxKind::FuncParamList),
    )? {
        parser.parse(FuncParamListScope::new(allow_self))?;
    }

    parser.expect_and_pop_recovery_stack()?;
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    parser.expect_and_pop_recovery_stack()?;
    parse_where_clause_opt(parser)?;

    if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Func))? {
        parser.parse(BlockExprScope::default())?;
    }
    Ok(())
}

fn parse_trait_fn_def_impl<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    parser.set_scope_recovery_stack(&[
        SyntaxKind::Ident,
        SyntaxKind::Lt,
        SyntaxKind::LParen,
        SyntaxKind::Arrow,
        SyntaxKind::WhereKw,
        SyntaxKind::LBrace,
    ]);

    if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Func))? {
        parser.bump();
    }

    parser.expect_and_pop_recovery_stack()?;
    parse_generic_params_opt(parser, false)?;

    if parser.find_and_pop(
        SyntaxKind::LParen,
        ExpectedKind::Syntax(SyntaxKind::FuncParamList),
    )? {
        parser.parse(FuncParamListScope::new(true))?;
    }

    parser.pop_recovery_stack();
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    parser.pop_recovery_stack();
    parse_where_clause_opt(parser)?;

    if parser.current_kind() == Some(SyntaxKind::LBrace) {
        parser.parse(BlockExprScope::default())?;
    }
    Ok(())
}

fn parse_extern_fn_def_impl<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::LParen, SyntaxKind::Arrow]);

    if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Func))? {
        parser.bump();
    }

    if parser.find_and_pop(
        SyntaxKind::LParen,
        ExpectedKind::Syntax(SyntaxKind::FuncParamList),
    )? {
        parser.parse(FuncParamListScope::new(true))?;
    }

    parser.pop_recovery_stack();
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    Ok(())
}
