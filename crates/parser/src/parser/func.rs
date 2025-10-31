use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr_atom::BlockExprScope,
    param::{parse_generic_params_opt, parse_where_clause_opt},
    parse_list,
    path::PathScope,
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

#[derive(Clone, Copy, Debug, Default)]
pub(crate) enum FuncDefScope {
    #[default]
    Normal,
    Impl,
    TraitDef,
    Extern,
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
        SyntaxKind::UsesKw,
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
        // function parameter list (signature parameters)
        parser.parse(super::param::FuncParamListScope::new(allow_self))?;
    }

    parser.expect_and_pop_recovery_stack()?;
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    parser.expect_and_pop_recovery_stack()?;
    parse_uses_clause_opt(parser)?;

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
        SyntaxKind::UsesKw,
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
        // trait method parameter list
        parser.parse(super::param::FuncParamListScope::new(true))?;
    }

    parser.pop_recovery_stack();
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    parser.pop_recovery_stack();
    parse_uses_clause_opt(parser)?;

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
        parser.parse(super::param::FuncParamListScope::new(true))?;
    }

    parser.pop_recovery_stack();
    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None)?;
    }

    Ok(())
}

/// Optionally parse a `uses` clause after the function parameter list and optional return type.
///
/// Supports two forms:
/// - `uses (ctx: Ctx, mut st: Storage)`
/// - `uses TypePath`
fn parse_uses_clause_opt<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    // Allow `uses` to appear on a new line after the signature
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    let r = if parser.current_kind() == Some(SyntaxKind::UsesKw) {
        parser.parse(UsesClauseScope::default())
    } else {
        Ok(())
    };
    parser.set_newline_as_trivia(newline_as_trivia);
    r
}

define_scope! { UsesClauseScope, SyntaxKind::UsesClause }
impl super::Parse for UsesClauseScope {
    type Error = Recovery<ErrProof>;

    fn parse<TS: TokenStream>(&mut self, parser: &mut Parser<TS>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::UsesKw);

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(UsesParamListScope::default())?
        } else {
            // Single bare param using same rules as list items (supports `mut Type`)
            parser.parse(UsesParamScope::default())?;
        }
        Ok(())
    }
}

define_scope! { UsesParamListScope, SyntaxKind::UsesParamList, (RParen, Comma) }
impl super::Parse for UsesParamListScope {
    type Error = Recovery<ErrProof>;

    fn parse<TS: TokenStream>(&mut self, parser: &mut Parser<TS>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::UsesParamList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| parser.parse(UsesParamScope::default()),
        )
    }
}

define_scope! { UsesParamScope, SyntaxKind::UsesParam }
impl super::Parse for UsesParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<TS: TokenStream>(&mut self, parser: &mut Parser<TS>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        // Cases to support inside parens:
        // - `Ctx`
        // - `mut Storage`
        // - `c: Ctx`
        // - `mut f: Foo`

        // Detect labeled form (optional leading `mut`, then ident/underscore, then `:`)
        let is_labeled = parser.dry_run(|p| {
            p.bump_if(SyntaxKind::MutKw);
            (p.current_kind() == Some(SyntaxKind::Ident)
                || p.current_kind() == Some(SyntaxKind::Underscore))
                && {
                    p.bump();
                    p.current_kind() == Some(SyntaxKind::Colon)
                }
        });

        if is_labeled {
            // optional `mut`
            parser.bump_if(SyntaxKind::MutKw);
            // name
            parser.expect(&[SyntaxKind::Ident, SyntaxKind::Underscore], None)?;
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.bump_expected(SyntaxKind::Underscore);
            }
            parser.bump_expected(SyntaxKind::Colon);
            parser.or_recover(|p| p.parse(PathScope::default()))?;
            return Ok(());
        }

        // Unlabeled form: optional `mut` followed by a Path key
        parser.bump_if(SyntaxKind::MutKw);
        parser.or_recover(|p| p.parse(PathScope::default()))?;
        Ok(())
    }
}
