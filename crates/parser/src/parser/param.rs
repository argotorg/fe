use std::convert::Infallible;

use unwrap_infallible::UnwrapInfallible;

use crate::{ExpectedKind, ParseError, SyntaxKind};

use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr::parse_expr,
    expr_atom::{BlockExprScope, LitExprScope},
    parse_list,
    path::PathScope,
    token_stream::TokenStream,
    type_::{is_type_start, parse_type},
};

define_scope! {
    pub(crate) FuncParamListScope{ allow_self: bool},
    FuncParamList,
    (RParen, Comma)
}
impl super::Parse for FuncParamListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::FuncParamList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| parser.parse(FnParamScope::new(self.allow_self)),
        )
    }
}

define_scope! { FnParamScope{allow_self: bool}, FnParam }
impl super::Parse for FnParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_if(SyntaxKind::MutKw);
        parser.expect(
            &[
                SyntaxKind::SelfKw,
                SyntaxKind::Ident,
                SyntaxKind::Underscore,
            ],
            None,
        )?;

        match parser.current_kind() {
            Some(SyntaxKind::SelfKw) => {
                if !self.allow_self {
                    parser.error_msg_on_current_token("`self` is not allowed here");
                }
                parser.bump_expected(SyntaxKind::SelfKw);
                if parser.bump_if(SyntaxKind::Colon) {
                    parse_type(parser, None)?;
                }
            }
            Some(SyntaxKind::Ident) => {
                parser.bump();

                if matches!(
                    parser.current_kind(),
                    Some(SyntaxKind::Ident | SyntaxKind::Underscore)
                ) {
                    parser.error_msg_on_current_token(
                        "parameter label renaming is not supported; use the parameter name as the label",
                    );
                    parser.bump();
                }
                if parser.find(
                    SyntaxKind::Colon,
                    ExpectedKind::TypeSpecifier(SyntaxKind::FnParam),
                )? {
                    parser.bump();
                    parse_type(parser, None)?;
                }
            }
            Some(SyntaxKind::Underscore) => {
                parser.bump();

                parser.expect(
                    &[SyntaxKind::Ident, SyntaxKind::Underscore, SyntaxKind::Colon],
                    None,
                )?;
                if !parser.bump_if(SyntaxKind::Ident) {
                    parser.bump_if(SyntaxKind::Underscore);
                }
                if parser.find(
                    SyntaxKind::Colon,
                    ExpectedKind::TypeSpecifier(SyntaxKind::FnParam),
                )? {
                    parser.bump();
                    parse_type(parser, None)?;
                }
            }
            _ => unreachable!(), // only reachable if a recovery token is added
        };
        Ok(())
    }
}

define_scope! {
    pub(crate) GenericParamListScope {disallow_trait_bound: bool},
    GenericParamList,
    (Comma, Gt)
}
impl super::Parse for GenericParamListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::GenericParamList,
            (SyntaxKind::Lt, SyntaxKind::Gt),
            |parser| {
                parser.expect(
                    &[SyntaxKind::Ident, SyntaxKind::ConstKw, SyntaxKind::Gt],
                    None,
                )?;
                match parser.current_kind() {
                    Some(SyntaxKind::ConstKw) => parser.parse(ConstGenericParamScope::default()),
                    Some(SyntaxKind::Ident) => {
                        parser.parse(TypeGenericParamScope::new(self.disallow_trait_bound))
                    }
                    Some(SyntaxKind::Gt) => Ok(()),
                    _ => unreachable!(),
                }
            },
        )
    }
}

define_scope! { ConstGenericParamScope, ConstGenericParam }
impl super::Parse for ConstGenericParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::ConstKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Colon]);
        if parser.find_and_pop(
            SyntaxKind::Ident,
            ExpectedKind::Name(SyntaxKind::ConstGenericParam),
        )? {
            parser.bump();
        }
        if parser.find_and_pop(
            SyntaxKind::Colon,
            ExpectedKind::TypeSpecifier(SyntaxKind::ConstGenericParam),
        )? {
            parser.bump();
            parse_type(parser, None)?;
        }

        // parse trait bound even though it's not allowed (checked in hir)
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::new(true))?;
        }
        Ok(())
    }
}

define_scope! {
    TypeGenericParamScope {disallow_trait_bound: bool},
    TypeGenericParam
}
impl super::Parse for TypeGenericParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Ident);

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::new(self.disallow_trait_bound))?;
        }
        if parser.bump_if(SyntaxKind::Eq) {
            parse_type(parser, None)?;
        }
        Ok(())
    }
}

define_scope! {
    pub TypeBoundListScope{disallow_trait_bound: bool},
    TypeBoundList,
    (Plus)
}
impl super::Parse for TypeBoundListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Colon);

        parser.parse(TypeBoundScope::new(self.disallow_trait_bound))?;
        while parser.current_kind() == Some(SyntaxKind::Plus) {
            parser.bump_expected(SyntaxKind::Plus);
            parser.parse(TypeBoundScope::new(self.disallow_trait_bound))?;
        }
        Ok(())
    }
}

define_scope! {
    TypeBoundScope{disallow_trait_bound: bool},
    TypeBound
}
impl super::Parse for TypeBoundScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        let is_type_kind = matches!(
            parser.current_kind(),
            Some(SyntaxKind::LParen | SyntaxKind::Star)
        );

        if is_type_kind {
            parse_kind_bound(parser)
        } else {
            if self.disallow_trait_bound {
                return parser.error_and_recover("trait bounds are not allowed here");
            }
            parser.parse_or_recover(TraitRefScope::default())
        }
    }
}

fn parse_kind_bound<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    let checkpoint = parser.checkpoint();
    let is_newline_trivia = parser.set_newline_as_trivia(false);

    parser.expect(&[SyntaxKind::Star, SyntaxKind::LParen], None)?;

    if parser.bump_if(SyntaxKind::LParen) {
        parse_kind_bound(parser)?;
        if parser.find(
            SyntaxKind::RParen,
            ExpectedKind::ClosingBracket {
                bracket: SyntaxKind::RParen,
                parent: SyntaxKind::TypeBound,
            },
        )? {
            parser.bump();
        }
    } else if parser.current_kind() == Some(SyntaxKind::Star) {
        parser
            .parse(KindBoundMonoScope::default())
            .unwrap_infallible();
    } else {
        // guaranteed by `expected`, unless other recovery
        // other tokens are added to the current scope
        unreachable!();
    }

    if parser.current_kind() == Some(SyntaxKind::Arrow) {
        parser.parse_cp(KindBoundAbsScope::default(), checkpoint.into())?;
    }
    parser.set_newline_as_trivia(is_newline_trivia);
    Ok(())
}

define_scope! { KindBoundMonoScope, KindBoundMono }
impl super::Parse for KindBoundMonoScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Star);
        Ok(())
    }
}

define_scope! { KindBoundAbsScope, KindBoundAbs }
impl super::Parse for KindBoundAbsScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Arrow);
        parse_kind_bound(parser)
    }
}

define_scope! { pub(super) TraitRefScope, TraitRef }
impl super::Parse for TraitRefScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.parse(PathScope::default()).map_err(|_| {
            ParseError::expected(&[SyntaxKind::TraitRef], None, parser.end_of_prev_token)
        })
    }
}

define_scope! {
    pub(crate) GenericArgListScope { is_expr: bool },
    GenericArgList,
    (Gt, Comma)
}
impl super::Parse for GenericArgListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Lt);

        let err_kind = Some(ExpectedKind::ClosingBracket {
            bracket: SyntaxKind::Gt,
            parent: SyntaxKind::GenericArgList,
        });
        let mut has_seen_comma = false;
        loop {
            if parser.bump_if(SyntaxKind::Gt) {
                return Ok(());
            }

            parser.parse(GenericArgScope::default())?;

            // If we're parsing an expr, recover less aggressively.
            if self.is_expr
                && !matches!(
                    parser.current_kind(),
                    Some(SyntaxKind::Gt | SyntaxKind::Comma)
                )
                && !has_seen_comma
            {
                let p = parser.add_error(ParseError::expected(
                    &[SyntaxKind::Gt, SyntaxKind::Comma],
                    err_kind,
                    parser.current_pos,
                ));
                return Err(Recovery(None, p));
            }
            parser.expect(&[SyntaxKind::Gt, SyntaxKind::Comma], err_kind)?;
            if !parser.bump_if(SyntaxKind::Comma) {
                break;
            }
            has_seen_comma = true;
        }
        parser.bump_expected(SyntaxKind::Gt);

        Ok(())
    }
}

define_scope! { GenericArgScope, TypeGenericArg }
impl super::Parse for GenericArgScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        // Check if this is an associated type argument (Ident = Type)
        let is_assoc_type = parser.dry_run(|parser| {
            parser.current_kind() == Some(SyntaxKind::Ident) && {
                parser.bump();
                parser.current_kind() == Some(SyntaxKind::Eq)
            }
        });

        if is_assoc_type {
            self.set_kind(SyntaxKind::AssocTypeGenericArg);
            // Parse the identifier name
            parser.bump_expected(SyntaxKind::Ident);

            // Parse the equals sign
            parser.bump_expected(SyntaxKind::Eq);

            // Parse the type
            parse_type(parser, None)?;
        } else {
            match parser.current_kind() {
                Some(SyntaxKind::LBrace) => {
                    self.set_kind(SyntaxKind::ConstGenericArg);
                    parser.parse(BlockExprScope::default())?;
                }

                Some(kind) if kind.is_literal_leaf() => {
                    self.set_kind(SyntaxKind::ConstGenericArg);
                    parser.parse(LitExprScope::default()).unwrap_infallible();
                }

                _ => {
                    parse_type(parser, None)?;
                    if parser.current_kind() == Some(SyntaxKind::Colon) {
                        parser.error_and_recover("type bounds are not allowed here")?;
                    }
                }
            }
        }
        Ok(())
    }
}

define_scope! { pub(crate) CallArgListScope, CallArgList, (RParen, Comma) }
impl super::Parse for CallArgListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::CallArgList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| parser.parse(CallArgScope::default()),
        )
    }
}

define_scope! { CallArgScope, CallArg, (Comma, RParen) }
impl super::Parse for CallArgScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        let has_label = parser.dry_run(|parser| {
            parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
        });

        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_expr(parser)?;
        Ok(())
    }
}

define_scope! { pub(crate) WhereClauseScope, WhereClause, (Newline) }
impl super::Parse for WhereClauseScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::WhereKw);

        let mut pred_count = 0;

        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(kind) if is_type_start(kind) => {
                    parser.parse(WherePredicateScope::default())?;
                    pred_count += 1;
                }
                _ => break,
            }

            if !parser.bump_if(SyntaxKind::Comma)
                && parser.current_kind().is_some()
                && is_type_start(parser.current_kind().unwrap())
            {
                parser.set_newline_as_trivia(false);
                let newline = parser.current_kind() == Some(SyntaxKind::Newline);
                parser.set_newline_as_trivia(true);

                if newline {
                    parser.add_error(ParseError::expected(
                        &[SyntaxKind::Comma],
                        None,
                        parser.current_pos,
                    ));
                } else if parser.find(
                    SyntaxKind::Comma,
                    ExpectedKind::Separator {
                        separator: SyntaxKind::Comma,
                        element: SyntaxKind::WherePredicate,
                    },
                )? {
                    parser.bump();
                } else {
                    break;
                }
            }
        }

        if pred_count == 0 {
            parser.error("`where` clause requires one or more type constraints");
        }
        Ok(())
    }
}

define_scope! { pub(crate) WherePredicateScope, WherePredicate }
impl super::Parse for WherePredicateScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_type(parser, None)?;

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::default())?;
        } else {
            parser.add_error(ParseError::expected(
                &[SyntaxKind::Colon],
                Some(ExpectedKind::TypeSpecifier(SyntaxKind::WherePredicate)),
                parser.end_of_prev_token,
            ));
        }
        Ok(())
    }
}

pub(crate) fn parse_where_clause_opt<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    let r = if parser.current_kind() == Some(SyntaxKind::WhereKw) {
        parser.parse(WhereClauseScope::default())
    } else {
        Ok(())
    };
    parser.set_newline_as_trivia(newline_as_trivia);
    r
}

pub(crate) fn parse_generic_params_opt<S: TokenStream>(
    parser: &mut Parser<S>,
    disallow_trait_bound: bool,
) -> Result<(), Recovery<ErrProof>> {
    if parser.current_kind() == Some(SyntaxKind::Lt) {
        parser.parse(GenericParamListScope::new(disallow_trait_bound))
    } else {
        Ok(())
    }
}
