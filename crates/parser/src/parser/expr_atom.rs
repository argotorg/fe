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
            let is_quote = allow_record_init && quote_opens_here(parser);
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
        LoopKw,
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
                    && match parser.current_kind() {
                        Some(SyntaxKind::DocComment) => true,
                        // An attribute opens a statement, such as a `for` loop,
                        // unless an item follows it.
                        Some(SyntaxKind::Pound) => attributes_precede_item(parser),
                        _ => false,
                    }))
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
pub(super) fn quote_field_label_ahead<S: TokenStream>(parser: &mut Parser<S>) -> bool {
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
            QuoteBodyKind::Pat => parser.parse(QuotePatScope::default()),
        }
    }
}

/// Whether the current token is a `quote` that opens a quotation: its body, or
/// its open list and then its body, starts on the same line. Otherwise `quote`
/// is an ordinary name, as on master.
fn quote_opens_here<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser
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
        })
}

/// What a quote body holds, decided once from its tokens: an item or `where`
/// head, an arm arrow at its top level, or pattern syntax (see
/// [`PatternShape`]). Everything else is an expression block.
enum QuoteBodyKind {
    Expr,
    Pat,
    Arms,
    Items,
    Where,
}

/// Reads the tokens of a quote body, without parsing it, to tell whether the
/// body is a pattern that no expression can spell: it holds a wildcard `_`, a
/// rest `..` or a nested `var` binding, and only tokens that patterns use, in
/// positions where patterns use them. A body that reads both ways, such as
/// `Some(v)` or `(a, b)`, is an expression; a pattern quotation lowers its
/// expression syntax as a pattern.
///
/// Splice contents are skipped, so a nested quote is seen only at the body's
/// own level, where it makes the body an expression.
#[derive(Default)]
struct PatternShape {
    /// The previous token ends an operand: a name, a literal, `_`, `..`, a
    /// splice or a closing delimiter.
    after_operand: bool,
    /// The previous token ends a path, which a `(` or `{` continues.
    after_path: bool,
    /// The body's top level broke a line after an operand, so a further
    /// token starts another statement.
    line_break: bool,
    pattern_only: bool,
    expression_only: bool,
    /// Generic argument lists open after a path: an `_` inside one is an
    /// inferred argument, not a wildcard.
    generic_depth: usize,
}

impl PatternShape {
    fn is_pattern(&self) -> bool {
        self.pattern_only && !self.expression_only
    }

    fn newline(&mut self, depth: usize) {
        if depth == 0 && self.after_operand {
            self.line_break = true;
        }
    }

    fn splice(&mut self) {
        self.next_token();
        self.after_operand = true;
        self.after_path = false;
    }

    fn next_token(&mut self) {
        if self.line_break {
            self.expression_only = true;
        }
    }

    /// `quote_head` is whether `kind` is a `quote` that opens a quotation.
    fn token(&mut self, kind: SyntaxKind, quote_head: bool) {
        use SyntaxKind::*;
        self.next_token();
        let (after_operand, after_path) = match kind {
            Ident if quote_head => {
                self.expression_only = true;
                (false, false)
            }
            Ident | SelfTypeKw | SelfKw | SuperKw | IngotKw => (true, true),
            Gt => {
                self.generic_depth = self.generic_depth.saturating_sub(1);
                (true, true)
            }
            Int | String | Byte | TrueKw | FalseKw | RParen | RBrace => (true, false),
            Underscore => {
                if self.generic_depth == 0 {
                    self.pattern_only = true;
                }
                (true, false)
            }
            // `..` after an operand is a range; elsewhere it is a rest pattern.
            Dot2 => {
                if self.after_operand {
                    self.expression_only = true;
                } else {
                    self.pattern_only = true;
                }
                (true, false)
            }
            // A `var` binding inside a pattern; `var` starts a statement only
            // at the head of a block, and a block makes the body an expression.
            VarKw => {
                if self.after_operand {
                    self.expression_only = true;
                } else {
                    self.pattern_only = true;
                }
                (false, false)
            }
            // A `{` continues a path into a record; elsewhere it opens a block.
            LBrace => {
                if !self.after_path {
                    self.expression_only = true;
                }
                (false, false)
            }
            // A `|` after an operand separates alternatives; elsewhere it
            // opens a closure.
            Pipe => {
                if !self.after_operand {
                    self.expression_only = true;
                }
                (false, false)
            }
            Lt => {
                if self.after_path {
                    self.generic_depth += 1;
                }
                (false, false)
            }
            MutKw | MaybeMutKw | RefKw | Colon2 | LParen | Comma | Colon => (false, false),
            _ => {
                self.expression_only = true;
                (false, false)
            }
        };
        self.after_operand = after_operand;
        self.after_path = after_path;
    }
}

fn quote_body_kind<S: TokenStream>(parser: &mut Parser<S>) -> QuoteBodyKind {
    let newline_as_trivia = parser.set_newline_as_trivia(false);
    let kind = parser.dry_run(|p| {
        p.bump_expected(SyntaxKind::LBrace);
        let mut shape = PatternShape::default();
        loop {
            match p.current_kind() {
                Some(SyntaxKind::Newline) => {
                    shape.newline(0);
                    p.bump();
                }
                Some(SyntaxKind::Dollar) => {
                    if !skip_splice(p) {
                        return QuoteBodyKind::Expr;
                    }
                    shape.splice();
                }
                Some(SyntaxKind::WhereKw) => return QuoteBodyKind::Where,
                // Attributes belong to the statement or item that follows them,
                // which decides the kind of the body. A `#` that does not open a
                // bracketed group is left to the item parser to report.
                Some(SyntaxKind::Pound) => {
                    if !skip_attribute_groups(p) {
                        return QuoteBodyKind::Items;
                    }
                }
                Some(kind)
                    if kind.is_item_head()
                        && !(kind == SyntaxKind::UnsafeKw
                            && p.dry_run(|p| {
                                p.bump();
                                while p.current_kind() == Some(SyntaxKind::Newline) {
                                    p.bump();
                                }
                                p.current_kind() == Some(SyntaxKind::LBrace)
                            })) =>
                {
                    return QuoteBodyKind::Items;
                }
                Some(SyntaxKind::Ident) if at_quoted_shape(p) => return QuoteBodyKind::Items,
                _ => {
                    // A body that starts with a binding marker, `_` or `..` is a
                    // pattern unless a later token makes it an expression, such
                    // as the `=` of `var x = 1`.
                    let marker_head = matches!(
                        p.current_kind(),
                        Some(
                            SyntaxKind::Underscore
                                | SyntaxKind::MutKw
                                | SyntaxKind::MaybeMutKw
                                | SyntaxKind::RefKw
                                | SyntaxKind::VarKw
                                | SyntaxKind::Dot2
                        )
                    );
                    // Only an arrow at this body's top level introduces arm syntax.
                    // Arrows inside nested matches and record/tuple patterns do not.
                    let mut depth = 0usize;
                    loop {
                        let Some(kind) = p.current_kind() else {
                            return QuoteBodyKind::Expr;
                        };
                        match kind {
                            SyntaxKind::FatArrow if depth == 0 => return QuoteBodyKind::Arms,
                            SyntaxKind::RBrace if depth == 0 => {
                                return if shape.is_pattern()
                                    || (marker_head && !shape.expression_only)
                                {
                                    QuoteBodyKind::Pat
                                } else {
                                    QuoteBodyKind::Expr
                                };
                            }
                            SyntaxKind::Dollar => {
                                if !skip_splice(p) {
                                    return QuoteBodyKind::Expr;
                                }
                                shape.splice();
                                continue;
                            }
                            SyntaxKind::Newline => shape.newline(depth),
                            SyntaxKind::LBrace | SyntaxKind::LParen | SyntaxKind::LBracket => {
                                shape.token(kind, false);
                                depth += 1;
                            }
                            SyntaxKind::RBrace | SyntaxKind::RParen | SyntaxKind::RBracket => {
                                depth = depth.saturating_sub(1);
                                shape.token(kind, false);
                            }
                            _ => {
                                let quote_head = kind == SyntaxKind::Ident && quote_opens_here(p);
                                shape.token(kind, quote_head);
                            }
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

/// Skips the attribute groups, `#[..]`, and the doc comments that start at the
/// current token, with the newlines after each. Returns `false` where a `#` does
/// not open a bracketed group or the group is not closed.
fn skip_attribute_groups<S: TokenStream>(p: &mut Parser<S>) -> bool {
    loop {
        match p.current_kind() {
            Some(SyntaxKind::Pound) => {
                p.bump();
                if p.current_kind() != Some(SyntaxKind::LBracket) {
                    return false;
                }
                let mut depth = 0usize;
                loop {
                    match p.current_kind() {
                        Some(SyntaxKind::LBracket) => depth += 1,
                        Some(SyntaxKind::RBracket) => depth -= 1,
                        None => return false,
                        _ => {}
                    }
                    p.bump();
                    if depth == 0 {
                        break;
                    }
                }
            }
            Some(SyntaxKind::DocComment) => p.bump(),
            _ => return true,
        }
        while p.current_kind() == Some(SyntaxKind::Newline) {
            p.bump();
        }
    }
}

/// Whether the attributes that start at the current token are followed by an
/// item, as opposed to a statement. A malformed attribute counts as one on an
/// item, so that the item parser reports it.
fn attributes_precede_item<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|p| {
        if !skip_attribute_groups(p) {
            return true;
        }
        p.current_kind().is_some_and(SyntaxKind::is_item_head)
            && !matches!(
                p.peek_n_non_trivia(2).as_slice(),
                [SyntaxKind::UnsafeKw, SyntaxKind::LBrace]
            )
    })
}

/// Whether the next tokens start an associated shape member, `shape Step ..`.
/// `shape` is an ordinary identifier outside this position.
fn at_quoted_shape<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.is_ident("shape")
        && parser.peek_n_non_trivia(2).as_slice() == [SyntaxKind::Ident, SyntaxKind::Ident]
}

define_scope! { QuotePatScope, QuotePat, (RBrace) }
impl super::Parse for QuotePatScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);
        parse_pat(parser)?;
        parser.expect(&[SyntaxKind::RBrace], None)?;
        parser.bump();
        Ok(())
    }
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
            } else if at_quoted_shape(parser) {
                // An associated shape member, `shape Step = (own u256, ref T)`: it
                // has no module item form, so only a member splice can lower it.
                parser.parse(super::item::TraitShapeItemScope::default())?;
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

#[cfg(test)]
mod quote_pattern_tests {
    use crate::{
        SyntaxKind, SyntaxNode,
        ast::{self, prelude::*},
        parser::RecoveryMode,
    };

    #[test]
    fn pattern_only_quotes_parse_without_reclassifying_match_arms() {
        for pattern in ["_", "mut x", "ref x", "var x", ".."] {
            let source = format!("const fn p() {{ let value = quote {{ {pattern} }} }}");
            let (green, errors) = crate::parse_source_file(&source, RecoveryMode::default());
            assert!(errors.is_empty(), "{source}: {errors:?}");
            let root = SyntaxNode::new_root(green);
            let quote = root.descendants().find_map(ast::QuoteExpr::cast).unwrap();
            assert!(quote.pattern().is_some(), "{source}");
        }
        let (green, errors) = crate::parse_source_file(
            "const fn p() { let arms = quote { _ => 7 } }",
            RecoveryMode::default(),
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(
            SyntaxNode::new_root(green)
                .descendants()
                .any(|node| node.kind() == SyntaxKind::MatchArmList)
        );
    }

    fn quote_body(body: &str) -> (Option<ast::QuoteExpr>, String) {
        let source = format!("const fn p() {{ let value = quote {body} }}");
        let (green, errors) = crate::parse_source_file(&source, RecoveryMode::default());
        let quote = SyntaxNode::new_root(green)
            .descendants()
            .find_map(ast::QuoteExpr::cast);
        (quote, format!("{errors:?}"))
    }

    #[test]
    fn compound_pattern_quotes_are_told_from_expressions_by_their_tokens() {
        for body in [
            "{ (_, mut y) }",
            "{ Pair { value: mut a, .. } }",
            "{ core::option::Option::Some((_, v)) }",
            "{ (.., x) }",
            "{ (var a, b) }",
            "{ Pair { ${f}: w, .. } }",
            "{ Some(_) | None }",
            "{ pick(_, 1) }",
        ] {
            let (quote, errors) = quote_body(body);
            assert_eq!(errors, "[]", "{body}");
            assert!(quote.unwrap().pattern().is_some(), "{body}");
        }
        for body in [
            "{ Some(v) }",
            "{ (a, b) }",
            "{ true | false }",
            "{ |_| 1 }",
            "{ 0..n }",
            "{ match x { _ => 1 } }",
            "{ let (_, y) = t\n y }",
            "{ ${a}\n ${b} }",
        ] {
            let (quote, errors) = quote_body(body);
            assert_eq!(errors, "[]", "{body}");
            let quote = quote.unwrap();
            assert!(
                quote.pattern().is_none() && quote.body().is_some(),
                "{body}"
            );
        }
        // A body that is not a pattern keeps its expression parse error, and
        // a nested quote stays refused whatever its body holds.
        for (body, error) in [
            ("{ pick(_, 1 + 2) }", "expected expression"),
            (
                "{ quote { 1 } }",
                "nested quote expressions are not supported",
            ),
            (
                "{ quote { _ } }",
                "nested quote expressions are not supported",
            ),
            (
                "{ quote(v) { (_, v) } }",
                "nested quote expressions are not supported",
            ),
        ] {
            let (_, errors) = quote_body(body);
            assert!(errors.contains(error), "{body}: {errors}");
        }
    }

    #[test]
    fn nested_quote_bodies_are_classified_without_reparsing() {
        // Deciding a body by parsing it first doubles the work at each level of
        // nesting, so forty levels would not finish.
        let depth = 40;
        let source = format!(
            "const fn p() {{ let q = {}1 + 1{} }}",
            "quote { ".repeat(depth),
            " }".repeat(depth)
        );
        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::Builder::new()
            .stack_size(64 << 20)
            .spawn(move || {
                let (_, errors) = crate::parse_source_file(&source, RecoveryMode::default());
                let _ = sender.send(format!("{errors:?}"));
            })
            .unwrap();
        let errors = receiver
            .recv_timeout(std::time::Duration::from_secs(30))
            .expect("parsing nested quotes must take time linear in their depth");
        assert!(
            errors.contains("nested quote expressions are not supported"),
            "{errors}"
        );
    }
}
