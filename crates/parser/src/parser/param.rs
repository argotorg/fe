use std::convert::Infallible;

use unwrap_infallible::UnwrapInfallible;

use crate::{ExpectedKind, ParseError, SyntaxKind};

use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr::{is_lshift, is_lt_eq, parse_const_generic_expr, parse_expr, parse_expr_no_struct},
    expr_atom::{BlockExprScope, LitExprScope},
    parse_list,
    path::{PathScope, is_path_segment},
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
        let lookahead = parser.peek_n_non_trivia(2);
        let allow_ref_self_shorthand = matches!(
            lookahead.as_slice(),
            [SyntaxKind::RefKw, SyntaxKind::SelfKw]
        );
        let allow_own_self_shorthand = matches!(
            lookahead.as_slice(),
            [SyntaxKind::OwnKw, SyntaxKind::SelfKw]
        );
        if allow_ref_self_shorthand {
            parser.bump_expected(SyntaxKind::RefKw);
        }
        if allow_own_self_shorthand {
            parser.bump_expected(SyntaxKind::OwnKw);
        }
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
                    // Recovery may land on a list separator or unexpected token;
                    // treat as empty parameter and let parse_list handle it.
                    _ => Ok(()),
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

        if parser.bump_if(SyntaxKind::Eq) {
            if parser.current_kind() == Some(SyntaxKind::Underscore) {
                parser.bump_expected(SyntaxKind::Underscore);
            } else {
                parse_const_generic_expr(parser)?;
            }
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
        ) || parser.is_ident("Constraint");

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

    if parser.is_ident("Constraint") {
        parser
            .parse(KindBoundConstraintScope::default())
            .unwrap_infallible();
    } else {
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

define_scope! { KindBoundConstraintScope, KindBoundConstraint }
impl super::Parse for KindBoundConstraintScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Ident);
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
        let is_assoc_type = matches!(
            parser.peek_n_non_trivia(2).as_slice(),
            [SyntaxKind::Ident, SyntaxKind::Eq]
        );

        if is_assoc_type {
            self.set_kind(SyntaxKind::AssocTypeGenericArg);
            // Parse the identifier name
            parser.bump_expected(SyntaxKind::Ident);

            // Parse the equals sign
            parser.bump_expected(SyntaxKind::Eq);

            // Parse the type
            parse_type(parser, None)?;
        } else {
            let is_const_call = parser.dry_run(|parser| {
                parser
                    .parse(PathScope::default())
                    .is_ok_and(|()| parser.current_kind() == Some(SyntaxKind::LParen))
            });

            if is_const_call {
                self.set_kind(SyntaxKind::ConstGenericArg);
                parse_const_generic_expr(parser)?;
                return Ok(());
            }

            match parser.current_kind() {
                Some(SyntaxKind::Underscore) => {
                    self.set_kind(SyntaxKind::ConstGenericArg);
                    parser.bump_expected(SyntaxKind::Underscore);
                }
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
        let has_label = matches!(
            parser.peek_n_non_trivia(2).as_slice(),
            [SyntaxKind::Ident, SyntaxKind::Colon]
        );

        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_expr(parser)?;
        Ok(())
    }
}

/// How a `{` encountered at a predicate-start position inside a `where`
/// clause is disambiguated between a brace-delimited const predicate
/// (`where { T::SIZE < 100 }`) and the delimiter that ends the item header
/// (a function body, field list, variant list, or item list).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum WhereBracePolicy {
    /// Dry-run the block as an expression and peek ONE non-trivia token
    /// behind it: if that token continues the item header (`,`, another
    /// predicate start, or a `{` — i.e. the header's own mandatory block
    /// still follows), the block is a predicate; otherwise it is left in
    /// place as the item's own block. Used everywhere a block is mandatory
    /// after the where clause (fn defs, struct/enum/trait/impl headers).
    #[default]
    Lookahead,
    /// The block is always a predicate. Used for fn declarations whose
    /// trailing block is optional or absent (trait fn decls, extern fns):
    /// no mandatory body exists to claim the block, and a `where` clause
    /// demands at least one predicate.
    AlwaysPredicate,
}

define_scope! {
    pub(crate) WhereClauseScope { brace_policy: WhereBracePolicy },
    WhereClause,
    (Newline)
}
impl super::Parse for WhereClauseScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::WhereKw);

        let mut pred_count = 0;

        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(kind) if is_type_start(kind) => {
                    if is_type_bound_predicate(parser)
                        || is_constraint_application_predicate(parser)
                    {
                        // `T: Bounds` (colon present) or the boundless
                        // constraint-application form `where Eq<T>` (colon
                        // absent); `WherePredicateScope` accepts both.
                        parser.parse(WherePredicateScope::default())?;
                    } else {
                        parser.parse(WhereConstPredicateScope::default())?;
                    }
                    pred_count += 1;
                }
                Some(kind) if is_const_predicate_start(kind) => {
                    parser.parse(WhereConstPredicateScope::default())?;
                    pred_count += 1;
                }
                Some(SyntaxKind::LBrace) if self.takes_brace_as_predicate(parser) => {
                    parser.parse(WhereConstPredicateScope::default())?;
                    pred_count += 1;
                }
                _ => break,
            }

            parser.set_newline_as_trivia(true);
            if !parser.bump_if(SyntaxKind::Comma) {
                let next_could_be_predicate = parser
                    .current_kind()
                    .is_some_and(|k| is_type_start(k) || is_const_predicate_start(k));
                if next_could_be_predicate {
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
                } else {
                    break;
                }
            }
        }

        if pred_count == 0 {
            parser.error("`where` clause requires one or more predicates");
        }
        Ok(())
    }
}

impl WhereClauseScope {
    /// Decides whether a `{` at a predicate-start position opens a const
    /// predicate (see [`WhereBracePolicy`]). One dry-run plus a one-token
    /// lookahead; no new delimiters.
    fn takes_brace_as_predicate<S: TokenStream>(&self, parser: &mut Parser<S>) -> bool {
        match self.brace_policy {
            WhereBracePolicy::AlwaysPredicate => true,
            WhereBracePolicy::Lookahead => parser.dry_run(|parser| {
                if !parser.parses_without_error(BlockExprScope::default()) {
                    return false;
                }
                match parser.current_kind() {
                    Some(SyntaxKind::Comma | SyntaxKind::LBrace) => true,
                    Some(kind) => is_type_start(kind) || is_const_predicate_start(kind),
                    None => false,
                }
            }),
        }
    }
}

define_scope! { pub(crate) WherePredicateScope, WherePredicate }
impl super::Parse for WherePredicateScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_type(parser, None)?;

        // A colon introduces a type-bound predicate (`T: Bounds`). With no
        // colon the predicate is the boundless constraint-application form
        // (`where Eq<T>`): the type alone is the predicate, lowered to a
        // `TraitInstId`. The router only reaches this scope without a colon
        // when `is_constraint_application_predicate` already matched.
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { WhereConstPredicateScope, WhereConstPredicate }
impl super::Parse for WhereConstPredicateScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // Record-init expressions are excluded so that in `where FLAG { .. }`
        // the predicate is `FLAG` and the block stays with the item — the
        // same mechanism `if`/`while` conditions use.
        parse_expr_no_struct(parser)?;
        Ok(())
    }
}

/// Dry-run: does this position start a type-bound predicate (`Type: Bounds`)?
fn is_type_bound_predicate<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|p| parse_type(p, None).is_ok() && p.current_kind() == Some(SyntaxKind::Colon))
}

/// Dry-run: does this position start a *constraint-application* predicate — a
/// trait path applied to type args with no bound (`where Eq<T>`), as opposed to
/// a type-bound predicate (`T: Bounds`) or a const-expression predicate
/// (`FLAG`, `T::SIZE >= 50`, `above_threshold<T>()`)?
///
/// Discriminator: the path's FINAL segment carries generic args **and** the
/// token after the path does not continue a const expression. This excludes
/// bare-ident const predicates (`where FLAG` — no generic args) and the generic
/// const-expr forms (`above_threshold<T>()` — `(` continues the expression;
/// `Foo<T>::C >= 5` — an operator continues). The head is resolved later: an
/// abstract `where P<T>` (`P` a `* -> Constraint` param) is syntactically
/// indistinguishable from `Eq<T>` and also matches here; it is rejected by name
/// in the analysis pass.
fn is_constraint_application_predicate<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        loop {
            match parser.current_kind() {
                // A simple path segment (not a `<T as Trait>` qualified type).
                Some(kind) if is_path_segment(kind) && kind != SyntaxKind::Lt => {
                    parser.bump();
                }
                _ => return false,
            }

            // Optional generic args on this segment (mirrors `PathSegmentScope`:
            // test with a nested dry-run, then consume for real).
            let has_args = parser.current_kind_same_line() == Some(SyntaxKind::Lt)
                && !(is_lt_eq(parser) || is_lshift(parser))
                && parser.dry_run(|p| p.parses_without_error(GenericArgListScope::new(false)));
            if has_args {
                parser
                    .parse(GenericArgListScope::new(false))
                    .expect("dry_run suggests this will succeed");
            }

            // A `::` means more segments follow; the constraint head is the
            // generic args on the FINAL segment, so keep scanning.
            if parser.bump_if(SyntaxKind::Colon2) {
                continue;
            }

            return has_args && !continues_const_expr(parser.current_kind());
        }
    })
}

/// Tokens that continue a const-expression after a primary path expression:
/// binary operators, `as`, and call/index/access postfixes. If one of these
/// follows a path-with-generic-args the predicate is a const expression
/// (`above_threshold<T>()`, `Foo<T>::C >= 5`), not a constraint application.
fn continues_const_expr(kind: Option<SyntaxKind>) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        Some(
            Plus | Minus | Star | Star2 | Slash | Percent | Amp | Amp2 | Pipe | Pipe2 | Hat | Lt
                | Gt | LtEq | GtEq | Eq2 | NotEq | LShift | RShift | AsKw | LParen | LBracket
                | Dot | Dot2 | Colon2
        )
    )
}

/// Tokens that unambiguously start expressions but never types.
///
/// `LBrace` is deliberately absent: a block in where-position is ambiguous
/// with the item's own block and is resolved by [`WhereBracePolicy`].
fn is_const_predicate_start(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        Not | Minus | Tilde | Plus | IfKw | MatchKw | Int | String | TrueKw | FalseKw
    )
}

pub(crate) fn parse_where_clause_opt<S: TokenStream>(
    parser: &mut Parser<S>,
    brace_policy: WhereBracePolicy,
) -> Result<(), Recovery<ErrProof>> {
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    let r = if parser.current_kind() == Some(SyntaxKind::WhereKw) {
        parser.parse(WhereClauseScope::new(brace_policy))
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
