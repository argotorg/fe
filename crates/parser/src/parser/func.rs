use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr_atom::BlockExprScope,
    param::{ItemBlock, parse_generic_params_opt, parse_where_clause_opt},
    parse_list,
    token_stream::TokenStream,
    type_::parse_type,
};
use crate::{ExpectedKind, ParseError, SyntaxKind, TextRange};

define_scope! {
    pub(crate) FuncScope {
        fn_def_scope: FuncDefScope
    },
    Func
}

define_scope! {
    pub(crate) FuncSignatureScope {
        fn_def_scope: FuncDefScope
    },
    SyntaxKind::FuncSignature
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum FuncDefScope {
    #[default]
    Normal,
    Impl,
    TraitDef,
    Extern,
}

impl FuncDefScope {
    /// Whether the function can take `self`.
    fn allows_self(self) -> bool {
        !matches!(self, Self::Normal)
    }

    /// Whether the function's body follows its signature.
    fn body(self) -> ItemBlock {
        match self {
            Self::Normal | Self::Impl => ItemBlock::Required,
            Self::TraitDef => ItemBlock::Optional,
            Self::Extern => ItemBlock::Absent,
        }
    }
}

impl super::Parse for FuncScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_if(SyntaxKind::ConstKw);
        parser.bump_expected(SyntaxKind::FnKw);

        parser.parse(FuncSignatureScope::new(self.fn_def_scope))?;
        match self.fn_def_scope.body() {
            ItemBlock::Required => {
                parser.set_scope_recovery_stack(&[SyntaxKind::LBrace]);
                if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Func))? {
                    parser.parse(BlockExprScope::default())?;
                }
            }
            ItemBlock::Optional => {
                if parser.current_kind() == Some(SyntaxKind::LBrace) {
                    parser.parse(BlockExprScope::default())?;
                }
            }
            ItemBlock::Absent => {}
        }
        Ok(())
    }
}

impl super::Parse for FuncSignatureScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // Tokens that can reasonably appear after each portion of a function
        // signature and therefore serve as recovery anchors.
        let mut recovery_tokens = vec![
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::LParen,
            SyntaxKind::Arrow,
            SyntaxKind::UsesKw,
            SyntaxKind::WhereKw,
            SyntaxKind::FnKw,
            SyntaxKind::ConstKw,
            SyntaxKind::PubKw,
            SyntaxKind::UnsafeKw,
            SyntaxKind::Pound,
            SyntaxKind::DocComment,
            SyntaxKind::DocCommentAttr,
            SyntaxKind::Newline,
            SyntaxKind::RBrace,
        ];
        let body = self.fn_def_scope.body();
        if body != ItemBlock::Absent {
            recovery_tokens.push(SyntaxKind::LBrace);
        }
        // A bodyless trait method also ends where an associated type begins.
        if self.fn_def_scope == FuncDefScope::TraitDef {
            recovery_tokens.push(SyntaxKind::TypeKw);
        }
        parser.set_scope_recovery_stack(&recovery_tokens);

        if parser.at_quote_hole() {
            parser.pop_recovery_stack();
            parser.parse(super::expr_atom::QuoteHoleScope::default())?;
        } else if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Func))? {
            parser.bump();
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_generic_params_opt(parser, false)?;

        if parser.find_and_pop(
            SyntaxKind::LParen,
            ExpectedKind::Syntax(SyntaxKind::FuncParamList),
        )? {
            parser.parse(super::param::FuncParamListScope::new(
                self.fn_def_scope.allows_self(),
            ))?;
        }

        parser.expect_and_pop_recovery_stack()?;
        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None)?;
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_uses_clause_opt(parser, self.fn_def_scope)?;

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser, body)?;

        Ok(())
    }
}

/// Optionally parse a `uses` clause after the function parameter list and optional return type.
///
/// Supports two forms:
/// - `uses (ctx: Ctx, st: mut Storage)`
/// - `uses TypePath`
///
/// The clause may start on a new line, except where a trait or trait
/// implementation item could: a line `uses E` in a trait, or `uses E = ..`
/// in an implementation, is an associated effect row item.
fn parse_uses_clause_opt<S: TokenStream>(
    parser: &mut Parser<S>,
    fn_def_scope: FuncDefScope,
) -> Result<(), Recovery<ErrProof>> {
    let newline_as_trivia = parser.set_newline_as_trivia(false);
    let starts_line = parser.current_kind() == Some(SyntaxKind::Newline);
    parser.set_newline_as_trivia(true);
    let row_item = starts_line
        && matches!(fn_def_scope, FuncDefScope::TraitDef | FuncDefScope::Impl)
        && parser.current_kind() == Some(SyntaxKind::UsesKw)
        && parser.dry_run(|parser| {
            parser.bump_expected(SyntaxKind::UsesKw);
            let newline_as_trivia = parser.set_newline_as_trivia(false);
            let row_item = parser.bump_if(SyntaxKind::Ident)
                && match parser.current_kind() {
                    Some(SyntaxKind::Eq) => true,
                    Some(SyntaxKind::Newline | SyntaxKind::RBrace) | None => {
                        fn_def_scope == FuncDefScope::TraitDef
                    }
                    _ => false,
                };
            parser.set_newline_as_trivia(newline_as_trivia);
            row_item
        });
    let r = if parser.current_kind() == Some(SyntaxKind::UsesKw) && !row_item {
        parser.parse(UsesClauseScope::default())
    } else {
        Ok(())
    };
    parser.set_newline_as_trivia(newline_as_trivia);
    r
}

/// The effects of a row: `(a: A, b: mut B)`, `A` or `mut A`.
pub(crate) fn parse_uses_row<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    if parser.current_kind() == Some(SyntaxKind::LParen) {
        parser.parse(UsesParamListScope::default())
    } else {
        parser.parse(UsesParamScope::default())
    }
}

define_scope! { pub(crate) UsesClauseScope, SyntaxKind::UsesClause }
impl super::Parse for UsesClauseScope {
    type Error = Recovery<ErrProof>;

    fn parse<TS: TokenStream>(&mut self, parser: &mut Parser<TS>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::UsesKw);
        parse_uses_row(parser)
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
        // - `mut Storage`, `mut? Storage`
        // - `c: Ctx`
        // - `f: mut Foo`, `f: mut? Foo`
        //
        // Legacy typed form `mut f: Foo` is rejected with a targeted parse error.
        let lookahead = parser.peek_n_non_trivia(3);
        let is_legacy_labeled = matches!(
            lookahead.as_slice(),
            [
                SyntaxKind::MutKw,
                SyntaxKind::Ident | SyntaxKind::Underscore,
                SyntaxKind::Colon
            ]
        );

        // Detect labeled form (ident/underscore, then `:`)
        let is_labeled = matches!(
            lookahead.as_slice(),
            [
                SyntaxKind::Ident | SyntaxKind::Underscore,
                SyntaxKind::Colon,
                ..
            ]
        );

        if is_legacy_labeled {
            let pos = parser.current_pos;
            parser.bump_expected(SyntaxKind::MutKw);
            parser.expect(&[SyntaxKind::Ident, SyntaxKind::Underscore], None)?;
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.bump_expected(SyntaxKind::Underscore);
            }
            parser.bump_expected(SyntaxKind::Colon);
            parse_typed_uses_key(parser)?;
            parser.add_error(ParseError::Msg(
                "`uses` typed parameters use `name: mut Type`, not `mut name: Type`".to_string(),
                TextRange::empty(pos),
            ));
            return Ok(());
        }

        if is_labeled {
            // name
            parser.expect(&[SyntaxKind::Ident, SyntaxKind::Underscore], None)?;
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.bump_expected(SyntaxKind::Underscore);
            }
            parser.bump_expected(SyntaxKind::Colon);
            parse_typed_uses_key(parser)?;
            return Ok(());
        }

        parse_typed_uses_key(parser)
    }
}

fn parse_typed_uses_key<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    if parser.bump_if(SyntaxKind::MutKw) || parser.bump_if(SyntaxKind::MaybeMutKw) {
        parse_type(parser, None)?;
        return Ok(());
    }

    if let Some(kind @ (SyntaxKind::RefKw | SyntaxKind::OwnKw)) = parser.current_kind() {
        let pos = parser.current_pos;
        parser.bump();
        parse_type(parser, None)?;
        let mode = match kind {
            SyntaxKind::RefKw => "ref",
            SyntaxKind::OwnKw => "own",
            _ => unreachable!(),
        };
        parser.add_error(ParseError::Msg(
            format!(
                "typed `uses` parameters only support `mut` and `mut?`; remove `{mode}` or use `mut`"
            ),
            TextRange::empty(pos),
        ));
        return Ok(());
    }

    parse_type(parser, None)?;
    Ok(())
}
