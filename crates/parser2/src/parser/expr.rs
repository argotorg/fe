use super::{
    define_scope, expr_atom,
    param::{CallArgListScope, GenericArgListScope},
    token_stream::{LexicalToken, TokenStream},
    Checkpoint, Parser,
};
use crate::SyntaxKind;

/// Parses expression.
pub fn parse_expr<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parse_expr_with_min_bp(parser, 0, true)
}

/// Parses expression except for `struct` initialization expression.
pub fn parse_expr_no_struct<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parse_expr_with_min_bp(parser, 0, false)
}

// Expressions are parsed in Pratt's top-down operator precedence style.
// <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
/// Parse an expression, stopping if/when we reach an operator that binds less
/// tightly than given binding power.
///
/// Returns `true` if parsing succeeded, `false` otherwise.
fn parse_expr_with_min_bp<S: TokenStream>(
    parser: &mut Parser<S>,
    min_bp: u8,
    allow_struct_init: bool,
) -> bool {
    let (ok, checkpoint) = parse_expr_atom(parser, allow_struct_init);
    if !ok {
        return false;
    }

    loop {
        let is_trivia = parser.set_newline_as_trivia(true);
        let Some(kind) = parser.current_kind() else {
            parser.set_newline_as_trivia(is_trivia);
            break;
        };
        parser.set_newline_as_trivia(is_trivia);

        // Parse postfix operators.
        match postfix_binding_power(parser) {
            Some(lbp) if lbp < min_bp => break,
            Some(_) => {
                match kind {
                    SyntaxKind::LBracket => {
                        parser.parse(IndexExprScope::default(), Some(checkpoint));
                        continue;
                    }

                    SyntaxKind::LParen => {
                        if parser.parse(CallExprScope::default(), Some(checkpoint)).0 {
                            continue;
                        }
                    }

                    // `expr<generic_param_args>()`.
                    SyntaxKind::Lt => {
                        if is_call_expr(parser) {
                            parser.parse(CallExprScope::default(), Some(checkpoint));
                            continue;
                        }
                    }

                    // `expr.method<T, i32>()`
                    SyntaxKind::Dot => {
                        if is_method_call(parser) {
                            parser.parse(MethodExprScope::default(), Some(checkpoint));
                            continue;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            None => {}
        }

        if let Some((lbp, _)) = infix_binding_power(parser) {
            if lbp < min_bp {
                break;
            }

            let (ok, _) = if kind == SyntaxKind::Dot {
                parser.parse(FieldExprScope::default(), Some(checkpoint))
            } else if is_assign(parser) {
                parser.parse(AssignExprScope::default(), Some(checkpoint))
            } else if is_aug_assign(parser) {
                parser.parse(AugAssignExprScope::default(), Some(checkpoint))
            } else {
                parser.parse(BinExprScope::default(), Some(checkpoint))
            };

            if !ok {
                return false;
            }

            continue;
        }
        break;
    }

    true
}

fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_struct_init: bool,
) -> (bool, Checkpoint) {
    match parser.current_kind() {
        Some(kind) if prefix_binding_power(kind).is_some() => {
            parser.parse(UnExprScope::default(), None)
        }
        Some(_) => expr_atom::parse_expr_atom(parser, allow_struct_init),
        None => {
            parser.error_and_recover("expected expression", None);
            (false, parser.checkpoint())
        }
    }
}

/// Specifies how tightly a prefix unary operator binds to its operand.
fn prefix_binding_power(kind: SyntaxKind) -> Option<u8> {
    use SyntaxKind::*;
    match kind {
        Not | Plus | Minus | Tilde => Some(145),
        _ => None,
    }
}

/// Specifies how tightly a postfix operator binds to its operand.
fn postfix_binding_power<S: TokenStream>(parser: &mut Parser<S>) -> Option<u8> {
    use SyntaxKind::*;

    let is_trivia = parser.set_newline_as_trivia(true);
    if let Some(Dot) = parser.current_kind() {
        parser.set_newline_as_trivia(is_trivia);
        return Some(151);
    }

    parser.set_newline_as_trivia(false);
    let power = match parser.current_kind() {
        Some(LBracket | LParen | Lt) => Some(147),
        _ => None,
    };

    parser.set_newline_as_trivia(is_trivia);
    power
}

/// Specifies how tightly does an infix operator bind to its left and right
/// operands.
fn infix_binding_power<S: TokenStream>(parser: &mut Parser<S>) -> Option<(u8, u8)> {
    use SyntaxKind::*;

    let is_trivia = parser.set_newline_as_trivia(true);
    if let Some(Dot) = parser.current_kind() {
        parser.set_newline_as_trivia(is_trivia);
        return Some((151, 150));
    }

    parser.set_newline_as_trivia(false);
    if is_aug_assign(parser) {
        parser.set_newline_as_trivia(is_trivia);
        return Some((11, 10));
    }

    let Some(kind) = parser.current_kind() else {
        parser.set_newline_as_trivia(is_trivia);
        return None;
    };

    let bp = match kind {
        Pipe2 => (50, 51),
        Amp2 => (60, 61),
        NotEq | Eq2 => (70, 71),
        Lt => {
            if is_lshift(parser) {
                (110, 111)
            } else {
                // `LT` and `LtEq` has the same binding power.
                (70, 71)
            }
        }
        Gt => {
            if is_rshift(parser) {
                (110, 111)
            } else {
                // `Gt` and `GtEq` has the same binding power.
                (70, 71)
            }
        }
        Pipe => (80, 81),
        Hat => (90, 91),
        Amp => (100, 101),
        LShift | RShift => (110, 111),
        Plus | Minus => (120, 121),
        Star | Slash | Percent => (130, 131),
        Star2 => (141, 140),
        Eq => {
            // `Assign` and `AugAssign` have the same binding power
            (11, 10)
        }
        _ => {
            return {
                parser.set_newline_as_trivia(is_trivia);
                None
            }
        }
    };

    parser.set_newline_as_trivia(is_trivia);
    Some(bp)
}

define_scope! { UnExprScope, UnExpr, Inheritance }
impl super::Parse for UnExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let kind = parser.current_kind().unwrap();
        let bp = prefix_binding_power(kind).unwrap();
        parser.bump();
        parse_expr_with_min_bp(parser, bp, true);
    }
}

define_scope! { BinExprScope, BinExpr, Inheritance }
impl super::Parse for BinExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        bump_bin_op(parser);
        parse_expr_with_min_bp(parser, rbp, false);
    }
}

define_scope! { AugAssignExprScope, AugAssignExpr, Inheritance }
impl super::Parse for AugAssignExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        bump_aug_assign_op(parser);
        parse_expr_with_min_bp(parser, rbp, false);
    }
}

define_scope! { AssignExprScope, AssignExpr, Inheritance }
impl super::Parse for AssignExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        parser.bump_expected(SyntaxKind::Eq);
        parse_expr_with_min_bp(parser, rbp, true);
    }
}

define_scope! { IndexExprScope, IndexExpr, Override(RBracket) }
impl super::Parse for IndexExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);
        parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RBracket]);
        parser.bump_or_recover(SyntaxKind::RBracket, "expected `]`", None);
    }
}

define_scope! { CallExprScope, CallExpr, Inheritance }
impl super::Parse for CallExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(GenericArgListScope::default(), None);
                },
                &[SyntaxKind::LParen],
            );
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            parser.error_and_recover("expected `(`", None);
            return;
        }
        parser.parse(CallArgListScope::default(), None);
    }
}

define_scope! { MethodExprScope, MethodCallExpr, Inheritance }
impl super::Parse for MethodExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        let is_trivia = parser.set_newline_as_trivia(true);
        parser.bump_expected(SyntaxKind::Dot);

        parser.bump_or_recover(SyntaxKind::Ident, "expected identifier", None);

        parser.with_next_expected_tokens(
            |parser| {
                if parser.current_kind() == Some(SyntaxKind::Lt) {
                    parser.parse(GenericArgListScope::default(), None);
                }
            },
            &[SyntaxKind::LParen],
        );

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            parser.error_and_recover("expected `(`", None);
            parser.set_newline_as_trivia(is_trivia);
            return;
        }

        parser.set_newline_as_trivia(is_trivia);
        parser.parse(CallArgListScope::default(), None);
    }
}

define_scope! { FieldExprScope, FieldExpr, Inheritance }
impl super::Parse for FieldExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        let is_trivia = parser.set_newline_as_trivia(true);
        parser.bump_expected(SyntaxKind::Dot);

        match parser.current_token() {
            Some(token) if token.syntax_kind() == SyntaxKind::Ident => {
                parser.bump();
            }
            Some(token) if token.syntax_kind() == SyntaxKind::Int => {
                let text = token.text();
                if !text.chars().all(|c| c.is_ascii_digit()) {
                    parser
                        .error_and_recover("expected integer decimal literal without prefix", None);
                    return;
                }
                parser.bump();
            }
            _ => {
                parser.error_and_recover("expected identifier or integer literal", None);
            }
        }

        parser.set_newline_as_trivia(is_trivia);
    }
}

define_scope! { pub(super) LShiftScope, LShift, Inheritance }
impl super::Parse for LShiftScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);
        parser.bump_expected(SyntaxKind::Lt);
    }
}

define_scope! { pub(super) RShiftScope, RShift, Inheritance }
impl super::Parse for RShiftScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Gt);
        parser.bump_expected(SyntaxKind::Gt);
    }
}

define_scope! { pub(super) LtEqScope, LtEq, Inheritance }
impl super::Parse for LtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);
        parser.bump_expected(SyntaxKind::Eq);
    }
}

define_scope! { pub(super) GtEqScope, GtEq, Inheritance }
impl super::Parse for GtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Gt);
        parser.bump_expected(SyntaxKind::Eq);
    }
}

pub(crate) fn is_lshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Lt), Some(SyntaxKind::Lt))
}

pub(crate) fn is_rshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Gt), Some(SyntaxKind::Gt))
}

fn is_lt_eq<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Lt), Some(SyntaxKind::Eq))
}

fn is_gt_eq<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Gt), Some(SyntaxKind::Eq))
}

fn is_aug_assign<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    matches!(
        parser.peek_three(),
        (
            Some(Pipe | Hat | Amp | Plus | Minus | Star | Slash | Percent | Star2),
            Some(Eq),
            _
        ) | (Some(Lt), Some(Lt), Some(Eq))
            | (Some(Gt), Some(Gt), Some(Eq))
    )
}

fn is_assign<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    let nt = parser.set_newline_as_trivia(false);
    let is_asn = parser.current_kind() == Some(SyntaxKind::Eq);
    parser.set_newline_as_trivia(nt);
    is_asn
}

fn bump_bin_op<S: TokenStream>(parser: &mut Parser<S>) {
    match parser.current_kind() {
        Some(SyntaxKind::Lt) => {
            if is_lshift(parser) {
                parser.parse(LShiftScope::default(), None);
            } else if is_lt_eq(parser) {
                parser.parse(LtEqScope::default(), None);
            } else {
                parser.bump();
            }
        }
        Some(SyntaxKind::Gt) => {
            if is_rshift(parser) {
                parser.parse(RShiftScope::default(), None);
            } else if is_gt_eq(parser) {
                parser.parse(GtEqScope::default(), None);
            } else {
                parser.bump();
            }
        }
        _ => {
            parser.bump();
        }
    }
}

fn bump_aug_assign_op<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    match parser.peek_three() {
        (Some(Pipe | Hat | Amp | Plus | Minus | Star | Slash | Percent | Star2), Some(Eq), _) => {
            parser.bump();
            parser.bump();
            true
        }
        (Some(Lt), Some(Lt), Some(Eq)) => {
            parser.parse(LShiftScope::default(), None);
            parser.bump_expected(SyntaxKind::Eq);
            true
        }
        (Some(Gt), Some(Gt), Some(Eq)) => {
            parser.parse(RShiftScope::default(), None);
            parser.bump_expected(SyntaxKind::Eq);
            true
        }
        _ => false,
    }
}

fn is_call_expr<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        parser.set_newline_as_trivia(false);

        let mut is_call = true;
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            is_call &= parser.parse(GenericArgListScope::default(), None).0;
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            false
        } else {
            is_call && parser.parse(CallArgListScope::default(), None).0
        }
    })
}

fn is_method_call<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        let is_trivia = parser.set_newline_as_trivia(true);
        if !parser.bump_if(SyntaxKind::Dot) {
            parser.set_newline_as_trivia(is_trivia);
            return false;
        }

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.set_newline_as_trivia(is_trivia);
            return false;
        }

        if parser.current_kind() == Some(SyntaxKind::Lt)
            && !parser.parse(GenericArgListScope::default(), None).0
        {
            parser.set_newline_as_trivia(is_trivia);
            return false;
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            parser.set_newline_as_trivia(is_trivia);
            false
        } else {
            parser.set_newline_as_trivia(is_trivia);
            parser.parse(CallArgListScope::default(), None).0
        }
    })
}
