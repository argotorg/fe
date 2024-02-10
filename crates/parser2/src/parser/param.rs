use crate::SyntaxKind;

use super::{
    define_scope,
    expr::parse_expr,
    expr_atom::{BlockExprScope, LitExprScope},
    path::PathScope,
    token_stream::TokenStream,
    type_::{is_type_start, parse_type},
    Parser,
};

define_scope! {
    pub(crate) FuncParamListScope{ allow_self: bool},
    FuncParamList,
    Override(RParen, Comma)
}
impl super::Parse for FuncParamListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.with_next_expected_tokens(
            |parser| parser.parse(FnParamScope::new(self.allow_self), None),
            &[SyntaxKind::Comma, SyntaxKind::RParen],
        );
        while parser.bump_if(SyntaxKind::Comma) {
            parser.with_next_expected_tokens(
                |parser| parser.parse(FnParamScope::new(false), None),
                &[SyntaxKind::Comma, SyntaxKind::RParen],
            );
        }

        parser.bump_or_recover(SyntaxKind::RParen, "expected closing `)`", None);
    }
}

define_scope! {
    FnParamScope{allow_self: bool},
    FnParam,
    Inheritance
}
impl super::Parse for FnParamScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_if(SyntaxKind::MutKw);

        if !self.allow_self && parser.current_kind() == Some(SyntaxKind::SelfKw) {
            parser.error_and_recover("self is not allowed here", None);
            return;
        }

        let is_self = parser.with_recovery_tokens(
            |parser| match parser.current_kind() {
                Some(SyntaxKind::SelfKw) => {
                    if self.allow_self {
                        parser.bump_expected(SyntaxKind::SelfKw);
                        true
                    } else {
                        unreachable!()
                    }
                }
                Some(SyntaxKind::Ident | SyntaxKind::Underscore) => parser
                    .with_next_expected_tokens(
                        |parser| {
                            parser.bump();
                            if !parser.bump_if(SyntaxKind::Ident) {
                                parser.bump_if(SyntaxKind::Underscore);
                            }
                            false
                        },
                        &[SyntaxKind::Colon],
                    ),
                _ => {
                    parser.error_and_recover("expected identifier for argument name", None);
                    false
                }
            },
            &[SyntaxKind::Colon],
        );

        if is_self {
            if parser.bump_if(SyntaxKind::Colon) {
                parse_type(parser, None);
            }
        } else {
            parser.bump_or_recover(SyntaxKind::Colon, "expected `:` after argument name", None);
            parse_type(parser, None);
        }
    }
}

define_scope! {
    pub(crate) GenericParamListScope {disallow_trait_bound: bool},
    GenericParamList,
    Override(Gt)
}
impl super::Parse for GenericParamListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);
        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericParamScope::new(self.disallow_trait_bound), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(GenericParamScope::new(self.disallow_trait_bound), None);
        }

        parser.bump_or_recover(SyntaxKind::Gt, "expected closing `>`", None);
    }
}

define_scope! {
    GenericParamScope {disallow_trait_bound: bool},
    TypeGenericParam,
    Inheritance(Comma)
}
impl super::Parse for GenericParamScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let is_const = parser.bump_if(SyntaxKind::ConstKw);
        if is_const {
            self.set_kind(SyntaxKind::ConstGenericParam);
        }
        parser.with_next_expected_tokens(
            |parser| {
                if is_const {
                    parser.with_next_expected_tokens(
                        |parser| {
                            if !parser.bump_if(SyntaxKind::Ident) {
                                parser.error_and_recover("expected const parameter", None);
                            }
                        },
                        &[SyntaxKind::Colon],
                    );

                    if !parser.bump_if(SyntaxKind::Colon) {
                        parser.error_and_recover("expected `:` after const parameter", None);
                        return;
                    }
                    parse_type(parser, None);

                    parser.set_newline_as_trivia(true);
                } else {
                    if !parser.bump_if(SyntaxKind::Ident) {
                        parser.error_and_recover("expected type parameter", None);
                    }

                    if parser.current_kind() == Some(SyntaxKind::Colon) {
                        parser.parse(TypeBoundListScope::new(self.disallow_trait_bound), None);
                    }

                    parser.set_newline_as_trivia(true);
                }
            },
            &[SyntaxKind::Comma, SyntaxKind::Gt],
        );
    }
}

define_scope! {
    TypeBoundListScope{disallow_trait_bound: bool},
    TypeBoundList,
    Inheritance(Plus)
}
impl super::Parse for TypeBoundListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Colon);

        parser.parse(TypeBoundScope::new(self.disallow_trait_bound), None);
        while parser.current_kind() == Some(SyntaxKind::Plus) {
            parser.bump_expected(SyntaxKind::Plus);
            parser.parse(TypeBoundScope::new(self.disallow_trait_bound), None);
        }
    }
}

define_scope! {
    TypeBoundScope{disallow_trait_bound: bool},
    TypeBound,
    Inheritance
}
impl super::Parse for TypeBoundScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        let is_type_kind = matches!(
            parser.current_kind(),
            Some(SyntaxKind::LParen | SyntaxKind::Star)
        );

        if is_type_kind {
            parse_kind_bound(parser);
        } else {
            if self.disallow_trait_bound {
                parser.error_and_recover("trait bounds are not allowed here", None);
                return;
            }
            parser.parse(TraitRefScope::default(), None);
        }
    }
}

fn parse_kind_bound<S: TokenStream>(parser: &mut Parser<S>) {
    let checkpoint = parser.checkpoint();
    let is_newline_trivia = parser.set_newline_as_trivia(false);

    if parser.bump_if(SyntaxKind::LParen) {
        parse_kind_bound(parser);
        parser.bump_or_recover(SyntaxKind::RParen, "expected closing `)`", None);
    } else if parser.current_kind() == Some(SyntaxKind::Star) {
        parser.parse(KindBoundMonoScope::default(), None);
    } else {
        parser.error_and_recover("expected `*` or `(`", None);
    }

    if parser.current_kind() == Some(SyntaxKind::Arrow) {
        parser.parse(KindBoundAbsScope::default(), checkpoint.into());
    }
    parser.set_newline_as_trivia(is_newline_trivia);
}

define_scope! {
    KindBoundMonoScope,
    KindBoundMono,
    Inheritance
}
impl super::Parse for KindBoundMonoScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Star);
    }
}

define_scope! {
    KindBoundAbsScope,
    KindBoundAbs,
    Inheritance
}
impl super::Parse for KindBoundAbsScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Arrow);
        parse_kind_bound(parser);
    }
}

define_scope! {
    pub(super) TraitRefScope,
    TraitRef,
    Inheritance
}
impl super::Parse for TraitRefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(PathScope::default(), None);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
        }
    }
}

define_scope! {
    pub(crate) GenericArgListScope,
    GenericArgList,
    Override(Gt, Comma)
}
impl super::Parse for GenericArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);

        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericArgScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(GenericArgScope::default(), None);
        }

        parser.bump_or_recover(SyntaxKind::Gt, "expected closing `>`", None);
    }
}

define_scope! {
    GenericArgScope,
    TypeGenericArg,
    Inheritance
}
impl super::Parse for GenericArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.with_next_expected_tokens(
            |parser| {
                match parser.current_kind() {
                    Some(SyntaxKind::LBrace) => {
                        self.set_kind(SyntaxKind::ConstGenericArg);
                        parser.parse(BlockExprScope::default(), None);
                    }

                    Some(kind) if kind.is_literal_leaf() => {
                        self.set_kind(SyntaxKind::ConstGenericArg);
                        parser.parse(LitExprScope::default(), None);
                    }

                    _ => {
                        parse_type(parser, None);
                        if parser.current_kind() == Some(SyntaxKind::Colon) {
                            parser.error_and_recover("type bounds are not allowed here", None);
                        }
                    }
                }
                parser.set_newline_as_trivia(true);
            },
            &[SyntaxKind::Comma, SyntaxKind::Gt],
        );
    }
}

define_scope! { pub(crate) CallArgListScope, CallArgList, Override(RParen, Comma) }
impl super::Parse for CallArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);

        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(CallArgScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(CallArgScope::default(), None);
        }

        parser.bump_or_recover(SyntaxKind::RParen, "expected closing `)`", None);
    }
}

define_scope! { CallArgScope, CallArg, Inheritance }
impl super::Parse for CallArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.with_next_expected_tokens(
            |parser| {
                parser.set_newline_as_trivia(false);
                let has_label = parser.dry_run(|parser| {
                    parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
                });

                if has_label {
                    parser.bump_expected(SyntaxKind::Ident);
                    parser.bump_expected(SyntaxKind::Colon);
                }
                parse_expr(parser);
                parser.set_newline_as_trivia(true);
            },
            &[SyntaxKind::Comma, SyntaxKind::RParen],
        );
    }
}

define_scope! { pub(crate) WhereClauseScope, WhereClause, Inheritance(Newline) }
impl super::Parse for WhereClauseScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::WhereKw);

        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(kind) if is_type_start(kind) => {
                    parser.parse(WherePredicateScope::default(), None);
                }
                _ => break,
            }
        }
    }
}

define_scope! { pub(crate) WherePredicateScope, WherePredicate, Inheritance }
impl super::Parse for WherePredicateScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_type(parser, None);
        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::default(), None);
            if !parser.bump_if(SyntaxKind::Newline) {
                parser.error_and_recover("expected newline after type bounds", None);
            }
        } else {
            parser.error_and_recover("expected `:` for type bounds", None);
        }
    }
}

pub(crate) fn parse_where_clause_opt<S: TokenStream>(parser: &mut Parser<S>) {
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    if parser.current_kind() == Some(SyntaxKind::WhereKw) {
        parser.parse(WhereClauseScope::default(), None);
    }
    parser.set_newline_as_trivia(newline_as_trivia);
}

pub(crate) fn parse_generic_params_opt<S: TokenStream>(
    parser: &mut Parser<S>,
    disallow_trait_bound: bool,
) {
    if parser.current_kind() == Some(SyntaxKind::Lt) {
        parser.parse(GenericParamListScope::new(disallow_trait_bound), None);
    }
}
