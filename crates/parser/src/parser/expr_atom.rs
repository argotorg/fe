use std::convert::{Infallible, identity};

use rowan::Checkpoint;
use unwrap_infallible::UnwrapInfallible;

use super::token_stream::LexicalToken;
use super::{
    ErrProof, Parser, Recovery, define_scope,
    expr::{parse_condition_expr, parse_expr, parse_expr_no_struct},
    item::ItemScope,
    param::{ClosureParamListScope, ItemBlock, parse_where_clause_opt},
    parse_list, parse_pat,
    stmt::parse_stmt,
    token_stream::TokenStream,
    type_::parse_type,
};
use crate::{
    ExpectedKind, SyntaxKind, TextRange,
    parser::{lit, path},
};

// Must be kept in sync with `parse_expr_atom`
pub(super) fn is_expr_atom_head(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    match kind {
        IfKw | MatchKw | UnsafeKw | LBrace | LParen | LBracket | Pipe | Pipe2 | Dollar => true,
        kind if lit::is_lit(kind) => true,
        kind if path::is_path_segment(kind) => true,
        _ => false,
    }
}

/// Panics if `!is_expr_atom_head(parser.current_kind())`
pub(super) fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_record_init: bool,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    use SyntaxKind::*;
    match parser.current_kind() {
        Some(IfKw) => parser.parse_cp(IfExprScope::default(), None),
        Some(MatchKw) => parser.parse_cp(MatchExprScope::default(), None),
        Some(UnsafeKw) => parser.parse_cp(UnsafeExprScope::default(), None),
        Some(Pipe | Pipe2) => parser.parse_cp(ClosureExprScope::default(), None),
        Some(SyntaxKind::Dollar) => parser.parse_cp(QuoteHoleExprScope::default(), None),
        Some(SyntaxKind::Ident) => {
            // `quote` starts a quote only when its body opens on the same
            // line; otherwise it is an ordinary name, as on master.
            let is_quote = allow_record_init
                && parser
                    .current_token()
                    .is_some_and(|token| token.text() == "quote")
                && parser.dry_run(|p| {
                    let newline_as_trivia = p.set_newline_as_trivia(false);
                    p.bump_expected(SyntaxKind::Ident);
                    let opens_body = match p.current_kind() {
                        Some(SyntaxKind::LBrace) => true,
                        Some(SyntaxKind::LParen) => {
                            let mut depth = 0usize;
                            while let Some(kind) = p.current_kind() {
                                match kind {
                                    SyntaxKind::LParen => depth += 1,
                                    SyntaxKind::RParen => {
                                        depth -= 1;
                                        if depth == 0 {
                                            p.bump();
                                            break;
                                        }
                                    }
                                    SyntaxKind::Newline => break,
                                    _ => {}
                                }
                                p.bump();
                            }
                            depth == 0 && p.current_kind() == Some(SyntaxKind::LBrace)
                        }
                        _ => false,
                    };
                    p.set_newline_as_trivia(newline_as_trivia);
                    opens_body
                });
            if is_quote {
                return parser.parse_cp(QuoteExprScope::default(), None);
            }

            // Contextual 'with': only treat as with-block when:
            // ident text is "with" AND we can parse a WithParamList AND next is '{'
            let is_with = parser.current_token().is_some_and(|t| t.text() == "with")
                && matches!(
                    parser.peek_n_non_trivia(2).as_slice(),
                    [SyntaxKind::Ident, SyntaxKind::LParen]
                )
                && parser.dry_run(|p| {
                    p.bump_expected(SyntaxKind::Ident);
                    // Try parse the with param list body and require a block right after.
                    if !p
                        .parse_ok(WithParamListScope::default())
                        .is_ok_and(identity)
                    {
                        return false;
                    }
                    p.current_kind() == Some(SyntaxKind::LBrace)
                });
            if is_with {
                parser.parse_cp(WithExprScope::default(), None)
            } else {
                parser.parse_cp(PathExprScope::new(allow_record_init), None)
            }
        }
        Some(LBrace) => parser.parse_cp(BlockExprScope::default(), None),
        Some(LParen) => parser.parse_cp(ParenScope::default(), None),
        Some(LBracket) => parser.parse_cp(ArrayScope::default(), None),
        Some(kind) if lit::is_lit(kind) => Ok(parser
            .parse_cp(LitExprScope::default(), None)
            .unwrap_infallible()),
        Some(kind) if path::is_path_segment(kind) => {
            parser.parse_cp(PathExprScope::new(allow_record_init), None)
        }
        _ => unreachable!(),
    }
}

define_scope! { ClosureExprScope, ClosureExpr }
impl super::Parse for ClosureExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // `||` is an empty parameter list.
        if !parser.bump_if(SyntaxKind::Pipe2) {
            parser.set_scope_recovery_stack(&[SyntaxKind::Pipe, SyntaxKind::LBrace]);
            parser.parse(ClosureParamListScope::default())?;
            parser.pop_recovery_stack();
        }

        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None)?;
        }

        parse_expr(parser)?;
        Ok(())
    }
}

define_scope! {
    pub(crate) BlockExprScope,
    BlockExpr,
    (
        RBrace,
        Newline,
        LetKw,
        ForKw,
        WhileKw,
        ContinueKw,
        BreakKw,
        ReturnKw,
        YieldKw
    )
}
impl super::Parse for BlockExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }

            let in_quote_body = parser
                .parents
                .iter()
                .rev()
                .nth(1)
                .is_some_and(|entry| entry.kind == SyntaxKind::QuoteExpr);
            // `unsafe {` starts an `unsafe` block, not an `unsafe fn` item.
            if (parser.current_kind().is_some_and(SyntaxKind::is_item_head)
                || (in_quote_body
                    && matches!(
                        parser.current_kind(),
                        Some(SyntaxKind::Pound | SyntaxKind::DocComment)
                    )))
                && !matches!(
                    parser.peek_n_non_trivia(2).as_slice(),
                    [SyntaxKind::UnsafeKw, SyntaxKind::LBrace]
                )
            {
                parser.parse(ItemScope::default())?;
                continue;
            }

            parse_stmt(parser)?;

            parser.set_newline_as_trivia(false);
            parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            parser.bump_if(SyntaxKind::Newline);
        }

        if parser.find(
            SyntaxKind::RBrace,
            crate::ExpectedKind::ClosingBracket {
                bracket: SyntaxKind::RBrace,
                parent: SyntaxKind::BlockExpr,
            },
        )? {
            parser.bump();
        }
        Ok(())
    }
}

define_scope! { IfExprScope, IfExpr }
impl super::Parse for IfExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::IfKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::LBrace, SyntaxKind::ElseKw]);
        parse_condition_expr(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::IfExpr))? {
            parser.parse(BlockExprScope::default())?;
        }

        if parser.current_kind() == Some(SyntaxKind::ElseKw) {
            parser.bump();
            parser.pop_recovery_stack();

            parser.expect(&[SyntaxKind::LBrace, SyntaxKind::IfKw], None)?;
            // Only a block or nested conditional belongs to the else arm.
            // The enclosing expression parser handles any following operators.
            parse_expr_atom(parser, true)?;
        }
        Ok(())
    }
}

define_scope! { WithExprScope, WithExpr }
impl super::Parse for WithExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // Expect `with` identifier
        let is_with = parser.current_token().is_some_and(|t| t.text() == "with");
        if !is_with {
            return parser.error_and_recover("expected `with`");
        }
        parser.bump();

        parser.set_scope_recovery_stack(&[SyntaxKind::LParen, SyntaxKind::LBrace]);
        // Parse with parameter list: (Effect = value, ...)
        parser.parse(WithParamListScope::default())?;
        // Done with LParen recovery token
        parser.pop_recovery_stack();

        // Parse block body (required)
        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            return parser.error_and_recover("`with` block requires a body `{ ... }`");
        }
        parser.parse(BlockExprScope::default())?;
        Ok(())
    }
}

define_scope! { UnsafeExprScope, UnsafeExpr }
impl super::Parse for UnsafeExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::UnsafeKw);
        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            return parser.error_and_recover("expected `{` after `unsafe`");
        }
        parser.parse(BlockExprScope::default())
    }
}

define_scope! { WithParamListScope, WithParamList, (RParen, Comma) }
impl super::Parse for WithParamListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::WithParamList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| parser.parse(WithParamScope::default()),
        )
    }
}

define_scope! { WithParamScope, WithParam }
impl super::Parse for WithParamScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        // `with` parameter supports either:
        // - `Key = value` (legacy)
        // - `value` (shorthand; key inferred from value type / usage)
        let is_keyed = matches!(
            parser.peek_n_non_trivia(1).as_slice(),
            [kind] if path::is_path_segment(*kind)
        ) && parser.dry_run(|p| {
            if !p.parse_ok(path::PathScope::default()).is_ok_and(identity) {
                return false;
            }
            p.current_kind() == Some(SyntaxKind::Eq)
        });

        if is_keyed {
            // effect key path
            parser.or_recover(|p| p.parse(path::PathScope::default()))?;
            parser.bump_expected(SyntaxKind::Eq);
            parse_expr(parser)
        } else {
            // shorthand value expression
            parse_expr(parser)
        }
    }
}

define_scope! { MatchExprScope, MatchExpr }
impl super::Parse for MatchExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::MatchKw);

        parse_expr_no_struct(parser)?;
        if parser.find(
            SyntaxKind::LBrace,
            ExpectedKind::Body(SyntaxKind::MatchExpr),
        )? {
            parser.parse(MatchArmListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { MatchArmListScope, MatchArmList, (SyntaxKind::Newline, SyntaxKind::RBrace, SyntaxKind::Comma) }
impl super::Parse for MatchArmListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) {
                break;
            }

            if starts_arm_hole(parser) {
                parser.parse(QuoteHoleArmScope::default())?;
            } else {
                parser.parse(MatchArmScope::default())?;
            }
            parser.set_newline_as_trivia(false);

            parser.expect(
                &[SyntaxKind::Comma, SyntaxKind::Newline, SyntaxKind::RBrace],
                None,
            )?;
            let comma = parser.bump_if(SyntaxKind::Comma);
            let nl = parser.bump_if(SyntaxKind::Newline);
            if !(comma || nl) {
                break;
            }
        }
        parser.bump_expected(SyntaxKind::RBrace);
        Ok(())
    }
}

define_scope! { MatchArmScope, MatchArm }
impl super::Parse for MatchArmScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        parser.set_scope_recovery_stack(&[SyntaxKind::FatArrow]);
        parse_pat(parser)?;

        if parser.find_and_pop(SyntaxKind::FatArrow, ExpectedKind::Unspecified)? {
            parser.bump();
        }

        // Allow statement-like keywords (return, break, continue) in match
        // arms by wrapping them in an implicit block expression.
        if matches!(
            parser.current_kind(),
            Some(
                SyntaxKind::ReturnKw
                    | SyntaxKind::YieldKw
                    | SyntaxKind::BreakKw
                    | SyntaxKind::ContinueKw
            )
        ) {
            parser.parse(ImplicitBlockExprScope::default())
        } else {
            parse_expr(parser)
        }
    }
}

// A block expression without braces, used to wrap a single statement
// (e.g. `return`) in match arm bodies where only expressions are expected.
define_scope! { ImplicitBlockExprScope, BlockExpr }
impl super::Parse for ImplicitBlockExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_stmt(parser)
    }
}

define_scope! { pub(crate) LitExprScope, LitExpr }
impl super::Parse for LitExprScope {
    type Error = Infallible;

    /// Caller is expected to verify that the next token is a literal.
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.parse(lit::LitScope::default())
    }
}

define_scope! { PathExprScope{ allow_record_init: bool }, PathExpr }
impl super::Parse for PathExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.or_recover(|p| {
            p.parse(path::PathScope::new(true)).map_err(|_| {
                crate::ParseError::Msg(
                    "expected an expression".into(),
                    TextRange::empty(p.end_of_prev_token),
                )
            })
        })?;

        if parser.current_kind() == Some(SyntaxKind::LBrace) && self.allow_record_init {
            parser.set_scope_kind(SyntaxKind::RecordInitExpr);
            parser.parse(RecordFieldListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { RecordFieldListScope, RecordFieldList, (RBrace, Comma) }
impl super::Parse for RecordFieldListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::RecordFieldList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(RecordFieldScope::default()),
        )
    }
}

define_scope! { RecordFieldScope, RecordField }
fn quote_field_label_ahead<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.at_quote_hole()
        && parser.dry_run(|p| skip_splice(p) && p.current_kind() == Some(SyntaxKind::Colon))
}

impl super::Parse for RecordFieldScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        if quote_field_label_ahead(parser) {
            parser.parse(QuoteHoleScope::default())?;
            parser.bump_expected(SyntaxKind::Colon);
        } else if matches!(
            parser.peek_two(),
            (Some(SyntaxKind::Ident), Some(SyntaxKind::Colon))
        ) {
            parser.bump_if(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }

        parse_expr(parser)
    }
}

define_scope! { ParenScope, ParenExpr, (RParen, Comma) }
impl super::Parse for ParenScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LParen);

        if parser.bump_if(SyntaxKind::RParen) {
            parser.set_scope_kind(SyntaxKind::TupleExpr);
            return Ok(());
        }

        loop {
            if parser.bump_if(SyntaxKind::RParen) {
                return Ok(());
            }
            parse_expr(parser)?;
            parser.expect(&[SyntaxKind::RParen, SyntaxKind::Comma], None)?;

            if parser.bump_if(SyntaxKind::Comma) {
                parser.set_scope_kind(SyntaxKind::TupleExpr);
                continue;
            }
            break;
        }
        parser.bump_expected(SyntaxKind::RParen);
        Ok(())
    }
}

define_scope! { ArrayScope, ArrayExpr, (RBracket, Comma, SemiColon) }
impl super::Parse for ArrayScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBracket);

        if parser.bump_if(SyntaxKind::RBracket) {
            return Ok(());
        }

        parse_expr(parser)?;
        parser.expect(
            &[
                SyntaxKind::SemiColon,
                SyntaxKind::Comma,
                SyntaxKind::RBracket,
            ],
            None,
        )?;

        if parser.bump_if(SyntaxKind::SemiColon) {
            parser.set_scope_kind(SyntaxKind::ArrayRepExpr);
            parse_expr(parser)?;
        } else {
            while parser.bump_if(SyntaxKind::Comma) {
                if parser.bump_if(SyntaxKind::RBracket) {
                    return Ok(());
                }

                parse_expr(parser)?;
                parser.expect(&[SyntaxKind::Comma, SyntaxKind::RBracket], None)?;
            }
        }

        if parser.find(
            SyntaxKind::RBracket,
            ExpectedKind::ClosingBracket {
                bracket: SyntaxKind::RBracket,
                parent: SyntaxKind::ArrayExpr,
            },
        )? {
            parser.bump();
        }
        Ok(())
    }
}

define_scope! { QuoteExprScope, QuoteExpr }
impl super::Parse for QuoteExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        if parser.quote_scope_depth() > 1 {
            parser.error_msg_on_current_token(
                "nested quote expressions are not supported by this bounded item quotation",
            );
        }
        parser.bump_expected(SyntaxKind::Ident);
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(QuoteOpenListScope::default())?;
        }
        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            return parser.error_and_recover("`quote` requires a body `{ ... }`");
        }
        match quote_body_kind(parser) {
            QuoteBodyKind::Items => parser.parse(QuoteItemListScope::default()),
            QuoteBodyKind::Arms => parser.parse(MatchArmListScope::default()),
            QuoteBodyKind::Where => parser.parse(QuoteWhereScope::default()),
            QuoteBodyKind::Expr => parser.parse(BlockExprScope::default()),
        }
    }
}

/// What a quote body holds, decided by its item/where head or top-level arm arrow.
enum QuoteBodyKind {
    Expr,
    Arms,
    Items,
    Where,
}

fn quote_body_kind<S: TokenStream>(parser: &mut Parser<S>) -> QuoteBodyKind {
    let newline_as_trivia = parser.set_newline_as_trivia(false);
    let kind = parser.dry_run(|p| {
        p.bump_expected(SyntaxKind::LBrace);
        loop {
            match p.current_kind() {
                Some(SyntaxKind::Newline) => p.bump(),
                Some(SyntaxKind::Dollar) => {
                    if !skip_splice(p) {
                        return QuoteBodyKind::Expr;
                    }
                }
                Some(SyntaxKind::WhereKw) => return QuoteBodyKind::Where,
                Some(kind) if kind.is_item_head() || kind == SyntaxKind::Pound => {
                    return QuoteBodyKind::Items;
                }
                _ => {
                    // Only an arrow at this body's top level introduces arm syntax.
                    // Arrows inside nested matches and record/tuple patterns do not.
                    let mut depth = 0usize;
                    loop {
                        match p.current_kind() {
                            Some(SyntaxKind::FatArrow) if depth == 0 => return QuoteBodyKind::Arms,
                            Some(SyntaxKind::RBrace) if depth == 0 => return QuoteBodyKind::Expr,
                            Some(
                                SyntaxKind::LBrace | SyntaxKind::LParen | SyntaxKind::LBracket,
                            ) => depth += 1,
                            Some(
                                SyntaxKind::RBrace | SyntaxKind::RParen | SyntaxKind::RBracket,
                            ) => depth = depth.saturating_sub(1),
                            None => return QuoteBodyKind::Expr,
                            _ => {}
                        }
                        p.bump();
                    }
                }
            }
        }
    });
    parser.set_newline_as_trivia(newline_as_trivia);
    kind
}

define_scope! { QuoteOpenListScope, QuoteOpenList, (RParen, Comma) }
impl super::Parse for QuoteOpenListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            SyntaxKind::QuoteOpenList,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| {
                if parser.current_kind() == Some(SyntaxKind::Ident) {
                    parser.bump();
                    Ok(())
                } else {
                    parser.error_and_recover("expected an open name")
                }
            },
        )
    }
}

define_scope! { QuoteItemListScope, QuoteItemList, (RBrace) }
impl super::Parse for QuoteItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);
        loop {
            parser.set_newline_as_trivia(true);
            if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
                break;
            }
            if parser.current_kind() == Some(SyntaxKind::Dollar) {
                parser.parse(QuoteHoleScope::default())?;
            } else {
                parser.parse(ItemScope::default())?;
            }
            parser.set_newline_as_trivia(false);
            parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            parser.bump_if(SyntaxKind::Newline);
        }
        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the quote")
    }
}

define_scope! { QuoteWhereScope, QuoteWhere, (RBrace) }
impl super::Parse for QuoteWhereScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.set_newline_as_trivia(true);
        parse_where_clause_opt(parser, ItemBlock::Absent)?;
        parser.set_newline_as_trivia(true);
        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the quote")
    }
}

/// Parses `${ expr }` after checking that it stands inside a quote.
fn parse_hole_body<S: TokenStream>(
    parser: &mut Parser<S>,
    parent: SyntaxKind,
) -> Result<(), Recovery<ErrProof>> {
    if parser.quote_scope_depth() == 0 {
        parser.error_msg_on_current_token("quotation holes are only allowed inside quote");
    }
    parser.bump_expected(SyntaxKind::Dollar);
    if parser.current_kind() != Some(SyntaxKind::LBrace) {
        return parser.error_and_recover("`$` must be followed by `{` to form a splice hole");
    }
    parser.bump_expected(SyntaxKind::LBrace);
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    let parsed = parse_expr(parser);
    parser.set_newline_as_trivia(newline_as_trivia);
    parsed?;
    if parser.find(
        SyntaxKind::RBrace,
        ExpectedKind::ClosingBracket {
            bracket: SyntaxKind::RBrace,
            parent,
        },
    )? {
        parser.bump();
    }
    Ok(())
}

define_scope! {
    /// A splice in expression position.
    pub(super) QuoteHoleExprScope,
    QuoteHoleExpr
}
impl super::Parse for QuoteHoleExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_hole_body(parser, SyntaxKind::QuoteHoleExpr)
    }
}

define_scope! {
    /// A splice in pattern position.
    pub(crate) QuoteHolePatScope,
    QuoteHolePat
}
impl super::Parse for QuoteHolePatScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_hole_body(parser, SyntaxKind::QuoteHolePat)
    }
}

define_scope! {
    /// A splice in type position.
    pub(crate) QuoteHoleTypeScope,
    QuoteHoleType
}
impl super::Parse for QuoteHoleTypeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_hole_body(parser, SyntaxKind::QuoteHoleType)
    }
}

define_scope! {
    /// A splice for a member, a declared name, items or where predicates.
    /// Its parent node says which.
    pub(crate) QuoteHoleScope,
    QuoteHole
}
impl super::Parse for QuoteHoleScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_hole_body(parser, SyntaxKind::QuoteHole)
    }
}

define_scope! { QuoteHoleArmScope, QuoteHoleArm }
impl super::Parse for QuoteHoleArmScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_hole_body(parser, SyntaxKind::QuoteHoleArm)
    }
}

fn starts_arm_hole<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    if parser.current_kind() != Some(SyntaxKind::Dollar) {
        return false;
    }
    parser.dry_run(|p| {
        skip_splice(p)
            && !matches!(
                p.current_kind(),
                Some(SyntaxKind::FatArrow | SyntaxKind::Pipe)
            )
    })
}

/// Bumps a splice `${..}` at the current `$`, with its balanced braces.
/// Returns false when the `$` does not open one. Callers look past a splice
/// inside a dry run.
fn skip_splice<S: TokenStream>(p: &mut Parser<S>) -> bool {
    p.bump_expected(SyntaxKind::Dollar);
    if p.current_kind() != Some(SyntaxKind::LBrace) {
        return false;
    }
    let mut depth = 0usize;
    loop {
        match p.current_kind() {
            Some(SyntaxKind::LBrace) => depth += 1,
            Some(SyntaxKind::RBrace) => depth -= 1,
            None => return false,
            _ => {}
        }
        p.bump();
        if depth == 0 {
            return true;
        }
    }
}
