use super::{
    ErrProof, Parser, ParsingScope, Recovery, define_scope,
    expr_atom::BlockExprScope,
    item::{ExternItemListScope, TraitItemListScope},
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

#[derive(Clone, Copy, Debug, Default)]
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
        // A signature whose body may be omitted ends wherever its item list
        // continues.
        let item_list_anchors = match self.fn_def_scope {
            FuncDefScope::TraitDef => TraitItemListScope::default().recovery_tokens().to_vec(),
            FuncDefScope::Extern => ExternItemListScope::default().recovery_tokens().to_vec(),
            FuncDefScope::Normal | FuncDefScope::Impl => Vec::new(),
        };
        for anchor in item_list_anchors {
            if !recovery_tokens.contains(&anchor) {
                recovery_tokens.push(anchor);
            }
        }
        parser.set_scope_recovery_stack(&recovery_tokens);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Func))? {
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
        parse_uses_clause_opt(parser)?;

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

define_scope! { pub(crate) UsesClauseScope, SyntaxKind::UsesClause }
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
        // - `f: mut Foo`
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
    if parser.bump_if(SyntaxKind::MutKw) {
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
            format!("typed `uses` parameters only support `mut`; remove `{mode}` or use `mut`"),
            TextRange::empty(pos),
        ));
        return Ok(());
    }

    parse_type(parser, None)?;
    Ok(())
}
