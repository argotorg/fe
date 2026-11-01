use std::convert::Infallible;

use unwrap_infallible::UnwrapInfallible;

use crate::{ExpectedKind, ParseError, SyntaxKind};

use super::{
    ErrProof, Parser, ProbeKind, Recovery, define_scope,
    expr::{is_expr_start, parse_const_generic_expr, parse_expr, parse_expr_no_struct},
    expr_atom::{BlockExprScope, LitExprScope},
    parse_list,
    pat::parse_pat_operand,
    path::{PathScope, is_path_segment},
    token_stream::TokenStream,
    type_::{TupleTypeScope, is_type_start, parse_closure_param_type, parse_type},
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
            |parser| parser.parse(FnParamScope::new(self.allow_self, false)),
        )
    }
}

define_scope! {
    pub(crate) ClosureParamListScope,
    FuncParamList,
    (Pipe, Comma)
}
impl super::Parse for ClosureParamListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::FuncParamList,
            (SyntaxKind::Pipe, SyntaxKind::Pipe),
            |parser| parser.parse(FnParamScope::new(false, true)),
        )
    }
}

define_scope! {
    FnParamScope {
        allow_self: bool,
        allow_inferred_type: bool
    },
    FnParam
}
impl FnParamScope {
    /// Parses the `: Type` suffix. A closure parameter may omit it entirely, or
    /// omit the payload type after a mode keyword (`own`, `ref`, `mut`).
    fn parse_param_ty<S: TokenStream>(
        &self,
        parser: &mut Parser<S>,
    ) -> Result<(), Recovery<ErrProof>> {
        if self.allow_inferred_type {
            if parser.bump_if(SyntaxKind::Colon) {
                parse_closure_param_type(parser)?;
            }
        } else if parser.find(
            SyntaxKind::Colon,
            ExpectedKind::TypeSpecifier(SyntaxKind::FnParam),
        )? {
            parser.bump();
            parse_type(parser, None)?;
        }
        Ok(())
    }
}
/// `mut` before a parameter's name, where a mutable owned parameter writes
/// `var`.
fn mut_param_name_error<S: TokenStream>(parser: &mut Parser<S>) {
    parser.error_msg_on_current_token(
        "a mutable owned parameter is written `var x: own T`; `x: mut T` is a mutable borrow",
    );
}

/// `mut?` before a parameter's name, which goes on its type.
fn maybe_mut_param_name_error<S: TokenStream>(parser: &mut Parser<S>) {
    parser.error_msg_on_current_token("a parameter's mode goes on its type: `x: mut? T`");
}

/// Whether a parameter pattern starts here, after any binding marker, which
/// the pattern parser reports: a tuple pattern, or a path to a tuple struct,
/// record or variant. A path starts as any path does (`Self`, `super`,
/// `ingot`, a qualified type, `self::` or a name), and only a name, or
/// `self` alone, may be the parameter's name instead.
fn is_param_pat_start<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    let tokens = parser.peek_n_non_trivia(3);
    let tokens = match tokens.as_slice() {
        [MutKw | MaybeMutKw | RefKw | VarKw, rest @ ..] => rest,
        tokens => tokens,
    };
    match tokens {
        [LParen, ..] => true,
        [Ident, next, ..] => matches!(next, LParen | LBrace | Colon2 | Lt),
        [SelfKw, next, ..] => *next == Colon2,
        [Ident | SelfKw, ..] | [] => false,
        [first, ..] => is_path_segment(*first),
    }
}

impl super::Parse for FnParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // `..A`: the parameters a shape lists.
        if parser.bump_if(SyntaxKind::Dot2) {
            parse_type(parser, None)?;
            return Ok(());
        }
        let has_mut = parser.bump_if(SyntaxKind::MutKw);
        let has_maybe_mut = !has_mut && parser.bump_if(SyntaxKind::MaybeMutKw);
        let has_var = !has_mut && !has_maybe_mut && parser.bump_if(SyntaxKind::VarKw);
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
        // A closure parameter may be a pattern; a function's takes `_` first,
        // having no label.
        if !allow_ref_self_shorthand && !allow_own_self_shorthand && is_param_pat_start(parser) {
            if has_mut || has_maybe_mut || has_var {
                parser.error_msg_on_current_token(
                    "a parameter pattern's markers go on the names it binds: `(var a, ref b)`",
                );
            }
            if !self.allow_inferred_type {
                parser.error_msg_on_current_token(
                    "a parameter pattern has no label; write `_` before it: `_ (a, b): T`",
                );
            }
            parse_pat_operand(parser)?;
            return self.parse_param_ty(parser);
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
                let typed = matches!(
                    parser.peek_n_non_trivia(2).as_slice(),
                    [_, SyntaxKind::Colon]
                );
                if has_mut && allow_own_self_shorthand {
                    parser.error_msg_on_current_token(
                        "a mutable owned receiver is written `var own self`; `mut self` is a mutable borrow",
                    );
                } else if has_var && !allow_own_self_shorthand && !typed {
                    parser.error_msg_on_current_token(
                        "`var` makes an owned receiver mutable: `var own self`",
                    );
                }
                parser.bump_expected(SyntaxKind::SelfKw);
                if parser.bump_if(SyntaxKind::Colon) {
                    if has_mut && parser.current_kind() == Some(SyntaxKind::OwnKw) {
                        parser.error_msg_on_current_token(
                            "a mutable owned receiver is written `var self: own T`; `mut self` is a mutable borrow",
                        );
                    }
                    parse_type(parser, None)?;
                }
            }
            Some(SyntaxKind::Ident) => {
                if has_mut {
                    mut_param_name_error(parser);
                } else if has_maybe_mut {
                    maybe_mut_param_name_error(parser);
                }
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
                self.parse_param_ty(parser)?;
            }
            Some(SyntaxKind::Underscore) => {
                if has_mut {
                    mut_param_name_error(parser);
                } else if has_maybe_mut {
                    maybe_mut_param_name_error(parser);
                }
                parser.bump();

                // A closure parameter may be a bare `_` with an inferred type.
                if is_param_pat_start(parser) {
                    parse_pat_operand(parser)?;
                } else if !self.allow_inferred_type
                    || !matches!(
                        parser.current_kind(),
                        Some(SyntaxKind::Comma | SyntaxKind::Pipe)
                    )
                {
                    parser.expect(
                        &[SyntaxKind::Ident, SyntaxKind::Underscore, SyntaxKind::Colon],
                        None,
                    )?;
                    if !parser.bump_if(SyntaxKind::Ident) {
                        parser.bump_if(SyntaxKind::Underscore);
                    }
                }
                self.parse_param_ty(parser)?;
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
                    // `shape` is a keyword only before a parameter's name.
                    Some(SyntaxKind::Ident)
                        if parser.is_ident("shape")
                            && parser.peek_n_non_trivia(2).as_slice()
                                == [SyntaxKind::Ident, SyntaxKind::Ident] =>
                    {
                        parser.parse(ShapeGenericParamScope::default())
                    }
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
            parse_const_generic_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ShapeGenericParamScope, ShapeGenericParam }
impl super::Parse for ShapeGenericParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Ident);
        parser.bump_expected(SyntaxKind::Ident);
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
            // The modes the bound grants a trait's `mut?` methods at:
            // `mut Index<usize>`, or the item's mode with `mut? Collection`.
            if !parser.bump_if(SyntaxKind::MutKw) {
                parser.bump_if(SyntaxKind::MaybeMutKw);
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
        let expected = |parser: &Parser<S>| {
            ParseError::expected(&[SyntaxKind::TraitRef], None, parser.end_of_prev_token)
        };
        // `Fn` and `FnMut` are keywords only before a parameter list.
        if (parser.is_ident("Fn") || parser.is_ident("FnMut"))
            && parser.peek_n_non_trivia(2).as_slice() == [SyntaxKind::Ident, SyntaxKind::LParen]
        {
            return parser
                .parse(FnShapeScope::default())
                .map_err(|_| expected(parser));
        }
        parser
            .parse(PathScope::default())
            .map_err(|_| expected(parser))
    }
}

define_scope! { FnShapeScope, FnShape }
impl super::Parse for FnShapeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Ident);
        parser.parse(TupleTypeScope::default())?;
        // `-> _` leaves the result open.
        if parser.bump_if(SyntaxKind::Arrow) && !parser.bump_if(SyntaxKind::Underscore) {
            parse_type(parser, None)?;
        }
        Ok(())
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

        // `ref` or `mut` alone is the mode of an associated shape.
        if matches!(
            parser.peek_n_non_trivia(2).as_slice(),
            [
                SyntaxKind::RefKw | SyntaxKind::MutKw,
                SyntaxKind::Gt | SyntaxKind::Comma
            ]
        ) {
            parser.set_scope_kind(SyntaxKind::ModeGenericArg);
            parser.bump();
            return Ok(());
        }

        // Check if this is an associated type argument (`Ident = Type`), or
        // an associated shape's at a mode (`Ident<ref> = Shape`).
        let is_assoc_type = matches!(
            parser.peek_n_non_trivia(2).as_slice(),
            [SyntaxKind::Ident, SyntaxKind::Eq]
        ) || matches!(
            parser.peek_n_non_trivia(5).as_slice(),
            [
                SyntaxKind::Ident,
                SyntaxKind::Lt,
                SyntaxKind::RefKw | SyntaxKind::MutKw,
                SyntaxKind::Gt,
                SyntaxKind::Eq
            ]
        );

        if is_assoc_type {
            parser.set_scope_kind(SyntaxKind::AssocTypeGenericArg);
            // Parse the identifier name
            parser.bump_expected(SyntaxKind::Ident);

            // The mode an associated shape is bound at
            if parser.current_kind() == Some(SyntaxKind::Lt) {
                parser.parse(GenericArgListScope::default())?;
            }

            // Parse the equals sign
            parser.bump_expected(SyntaxKind::Eq);

            // Parse the type
            parse_type(parser, None)?;
        } else {
            let is_const_call = parser.probe(ProbeKind::ConstCall, |parser| {
                parser
                    .parse(PathScope::default())
                    .is_ok_and(|()| parser.current_kind() == Some(SyntaxKind::LParen))
            });

            if is_const_call {
                parser.set_scope_kind(SyntaxKind::ConstGenericArg);
                parse_const_generic_expr(parser)?;
                return Ok(());
            }

            match parser.current_kind() {
                Some(SyntaxKind::LBrace) => {
                    parser.set_scope_kind(SyntaxKind::ConstGenericArg);
                    parser.parse(BlockExprScope::default())?;
                }

                Some(kind) if kind.is_literal_leaf() => {
                    parser.set_scope_kind(SyntaxKind::ConstGenericArg);
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

/// Whether an item's own block can follow its `where` clause. This decides
/// how a `{` that starts a predicate is read: as a braced condition, as in
/// `where { N > 0 }`, or as the item's body, field list or item list (see
/// [`WhereClauseScope::brace_opens_predicate`]).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum ItemBlock {
    /// A block must follow: function definitions and struct, enum, trait
    /// and impl headers.
    #[default]
    Required,
    /// A block may follow: a trait function's default body.
    Optional,
    /// No block follows: an extern function.
    Absent,
}

define_scope! {
    pub(crate) WhereClauseScope { item_block: ItemBlock },
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
                // `where ${w}` splices predicates; `where ${t}: Tr` bounds a type.
                Some(SyntaxKind::Dollar)
                    if !parser.dry_run(|p| {
                        parse_type(p, None).is_ok() && p.current_kind() == Some(SyntaxKind::Colon)
                    }) =>
                {
                    parser.parse(super::expr_atom::QuoteHoleScope::default())?;
                }
                Some(SyntaxKind::LBrace) if self.brace_opens_predicate(parser, pred_count == 0) => {
                    parser.parse(WhereConstPredicateScope::default())?;
                }
                Some(kind) if starts_predicate(kind) => {
                    let type_bound = is_type_start(kind)
                        && parser.dry_run(|p| {
                            parse_type(p, None).is_ok()
                                && p.current_kind() == Some(SyntaxKind::Colon)
                        });
                    if type_bound {
                        parser.parse(WherePredicateScope::default())?;
                    } else {
                        parser.parse(WhereConstPredicateScope::default())?;
                    }
                }
                _ => break,
            }
            pred_count += 1;

            if parser.bump_if(SyntaxKind::Comma) {
                continue;
            }
            if !parser.current_kind().is_some_and(starts_predicate) {
                break;
            }
            // Another predicate follows without a `,`.
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

        if pred_count == 0 {
            parser.error("`where` clause requires one or more predicates");
        }
        Ok(())
    }
}

impl WhereClauseScope {
    /// Whether a `{` that starts a predicate, right after `where` (`first`)
    /// or after a `,`, opens a braced condition rather than the item's own
    /// block.
    ///
    /// Where no block can follow the clause, it always does. So it does
    /// right after a trait function's `where`: read as the function's body,
    /// it would leave the clause empty. Otherwise it does when what follows
    /// its matching `}` continues the clause: a `,`, another predicate, or
    /// the item's own `{`. So `where T: Copy, { body }` keeps its body. Only
    /// the braces are matched, the block is not parsed, so a syntax error
    /// inside a braced condition does not turn it into the item's block.
    fn brace_opens_predicate<S: TokenStream>(&self, parser: &mut Parser<S>, first: bool) -> bool {
        match (self.item_block, first) {
            (ItemBlock::Absent, _) | (ItemBlock::Optional, true) => true,
            (ItemBlock::Required | ItemBlock::Optional, _) => parser.dry_run(|parser| {
                skip_braces(parser)
                    && parser.current_kind().is_some_and(|kind| {
                        matches!(kind, SyntaxKind::Comma | SyntaxKind::LBrace)
                            || starts_predicate(kind)
                    })
            }),
        }
    }
}

/// Bumps a `{` and every token up to its matching `}`. Returns `false` when
/// the input ends first.
fn skip_braces<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    let mut depth = 0usize;
    while let Some(kind) = parser.current_kind() {
        match kind {
            SyntaxKind::LBrace => depth += 1,
            SyntaxKind::RBrace => depth -= 1,
            _ => {}
        }
        parser.bump();
        if depth == 0 {
            return true;
        }
    }
    false
}

/// Whether a `where` predicate can start with `kind`: a type bound starts
/// with a type, and a const condition with an expression. A `{` is left out:
/// whether it opens a condition or the item's block is decided by
/// [`WhereClauseScope::brace_opens_predicate`]. An `unsafe` is left out too:
/// it starts the next `unsafe fn` of a trait or `extern` block.
fn starts_predicate(kind: SyntaxKind) -> bool {
    !matches!(kind, SyntaxKind::LBrace | SyntaxKind::UnsafeKw)
        && (is_type_start(kind) || is_expr_start(kind))
}

define_scope! { WhereConstPredicateScope, WhereConstPredicate }
impl super::Parse for WhereConstPredicateScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_expr_no_struct(parser)?;
        Ok(())
    }
}

define_scope! { pub(crate) WherePredicateScope, WherePredicate }
impl super::Parse for WherePredicateScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // The clause parses a type bound only after finding the `:`.
        parse_type(parser, None)?;
        parser.parse(TypeBoundListScope::default())?;
        Ok(())
    }
}

pub(crate) fn parse_where_clause_opt<S: TokenStream>(
    parser: &mut Parser<S>,
    item_block: ItemBlock,
) -> Result<(), Recovery<ErrProof>> {
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    let r = if parser.current_kind() == Some(SyntaxKind::WhereKw) {
        parser.parse(WhereClauseScope::new(item_block))
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
