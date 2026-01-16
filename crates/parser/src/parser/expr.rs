use std::convert::{Infallible, identity};
use unwrap_infallible::UnwrapInfallible;

use super::{
    Checkpoint, ErrProof, Parser, Recovery, define_scope,
    expr_atom::{self, is_expr_atom_head},
    param::{CallArgListScope, GenericArgListScope},
    token_stream::TokenStream,
};
use crate::{ExpectedKind, SyntaxKind};

/// Parses expression.
pub fn parse_expr<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    parse_expr_with_min_bp(parser, 0, true)
}

/// Parses expression except for `struct` initialization expression.
pub fn parse_expr_no_struct<S: TokenStream>(
    parser: &mut Parser<S>,
) -> Result<(), Recovery<ErrProof>> {
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
) -> Result<(), Recovery<ErrProof>> {
    let checkpoint = parse_expr_atom(parser, allow_struct_init)?;

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
                        parser.parse_cp(IndexExprScope::default(), Some(checkpoint))?;
                        continue;
                    }

                    SyntaxKind::LParen => {
                        if parser
                            .parse_cp(CallExprScope::default(), Some(checkpoint))
                            .is_ok()
                        {
                            continue;
                        }
                    }

                    // `expr.method<T, i32>()`
                    SyntaxKind::Dot => {
                        if is_method_call(parser) {
                            parser.parse_cp(MethodExprScope::default(), Some(checkpoint))?;
                            continue;
                        }
                    }
                    SyntaxKind::AsKw => {
                        parser.parse_cp(CastExprScope::default(), Some(checkpoint))?;
                        continue;
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

            if kind == SyntaxKind::Dot {
                parser.parse_cp(FieldExprScope::default(), Some(checkpoint))
            } else if is_assign(parser) {
                parser.parse_cp(AssignExprScope::default(), Some(checkpoint))
            } else if is_aug_assign(parser) {
                parser.parse_cp(AugAssignExprScope::default(), Some(checkpoint))
            } else {
                parser.parse_cp(BinExprScope::default(), Some(checkpoint))
            }?;
            continue;
        }
        break;
    }

    Ok(())
}

fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_struct_init: bool,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    match parser.current_kind() {
        Some(kind) if prefix_binding_power(kind).is_some() => {
            parser.parse_cp(UnExprScope::new(allow_struct_init), None)
        }
        Some(kind) if is_expr_atom_head(kind) => {
            expr_atom::parse_expr_atom(parser, allow_struct_init)
        }
        _ => parser
            .error_and_recover("expected expression")
            .map(|_| parser.checkpoint()),
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
        Some(LBracket | LParen) => Some(147),
        Some(AsKw) => Some(146),
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
            };
        }
    };

    parser.set_newline_as_trivia(is_trivia);
    Some(bp)
}

define_scope! { UnExprScope { allow_struct_init: bool }, UnExpr }
impl super::Parse for UnExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        let kind = parser.current_kind().unwrap();
        let bp = prefix_binding_power(kind).unwrap();
        parser.bump();
        parse_expr_with_min_bp(parser, bp, self.allow_struct_init)
    }
}

define_scope! { CastExprScope, CastExpr }
impl super::Parse for CastExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::AsKw);
        super::type_::parse_type(parser, None)?;
        Ok(())
    }
}

define_scope! { BinExprScope, BinExpr }
impl super::Parse for BinExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        bump_bin_op(parser);
        parse_expr_with_min_bp(parser, rbp, false)
    }
}

define_scope! { AugAssignExprScope, AugAssignExpr }
impl super::Parse for AugAssignExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        bump_aug_assign_op(parser);
        parse_expr_with_min_bp(parser, rbp, false)
    }
}

define_scope! { AssignExprScope, AssignExpr }
impl super::Parse for AssignExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        let (_, rbp) = infix_binding_power(parser).unwrap();
        parser.bump_expected(SyntaxKind::Eq);
        parse_expr_with_min_bp(parser, rbp, true)
    }
}

define_scope! { IndexExprScope, IndexExpr, (RBracket, Newline) }
impl super::Parse for IndexExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);
        parse_expr(parser)?;

        if parser.find(
            SyntaxKind::RBracket,
            ExpectedKind::ClosingBracket {
                bracket: SyntaxKind::RBracket,
                parent: SyntaxKind::IndexExpr,
            },
        )? {
            parser.bump();
        }
        Ok(())
    }
}

define_scope! { CallExprScope, CallExpr }
impl super::Parse for CallExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        parser.set_scope_recovery_stack(&[SyntaxKind::LParen]);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default())?;
        }

        if parser.find_and_pop(
            SyntaxKind::LParen,
            ExpectedKind::Syntax(SyntaxKind::CallArgList),
        )? {
            parser.parse(CallArgListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { MethodExprScope, MethodCallExpr }
impl super::Parse for MethodExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Dot);
        parser.set_newline_as_trivia(false);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Lt, SyntaxKind::LParen]);
        if parser.find_and_pop(
            SyntaxKind::Ident,
            ExpectedKind::Name(SyntaxKind::MethodCallExpr),
        )? {
            parser.bump();
        }

        parser.pop_recovery_stack();
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default())?;
        }

        if parser.find_and_pop(
            SyntaxKind::LParen,
            ExpectedKind::Syntax(SyntaxKind::CallArgList),
        )? {
            parser.parse(CallArgListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { FieldExprScope, FieldExpr }
impl super::Parse for FieldExprScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Dot);

        parser.expect(&[SyntaxKind::Ident, SyntaxKind::Int], None)?;
        parser.bump();
        Ok(())
    }
}

define_scope! { pub(super) LShiftScope, LShift }
impl super::Parse for LShiftScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Lt);
        parser.bump_expected(SyntaxKind::Lt);
        Ok(())
    }
}

define_scope! { pub(super) RShiftScope, RShift }
impl super::Parse for RShiftScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Gt);
        parser.bump_expected(SyntaxKind::Gt);
        Ok(())
    }
}

define_scope! { pub(super) LtEqScope, LtEq }
impl super::Parse for LtEqScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Lt);
        parser.bump_expected(SyntaxKind::Eq);
        Ok(())
    }
}

define_scope! { pub(super) GtEqScope, GtEq }
impl super::Parse for GtEqScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Gt);
        parser.bump_expected(SyntaxKind::Eq);
        Ok(())
    }
}

pub(crate) fn is_lshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Lt), Some(SyntaxKind::Lt))
}

pub(crate) fn is_rshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.peek_two() == (Some(SyntaxKind::Gt), Some(SyntaxKind::Gt))
}

pub(crate) fn is_lt_eq<S: TokenStream>(parser: &mut Parser<S>) -> bool {
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
                parser.parse(LShiftScope::default()).unwrap_infallible();
            } else if is_lt_eq(parser) {
                parser.parse(LtEqScope::default()).unwrap_infallible();
            } else {
                parser.bump();
            }
        }
        Some(SyntaxKind::Gt) => {
            if is_rshift(parser) {
                parser.parse(RShiftScope::default()).unwrap_infallible();
            } else if is_gt_eq(parser) {
                parser.parse(GtEqScope::default()).unwrap_infallible();
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
            parser.parse(LShiftScope::default()).unwrap_infallible();
            parser.bump_expected(SyntaxKind::Eq);
            true
        }
        (Some(Gt), Some(Gt), Some(Eq)) => {
            parser.parse(RShiftScope::default()).unwrap_infallible();
            parser.bump_expected(SyntaxKind::Eq);
            true
        }
        _ => false,
    }
}

fn is_method_call<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    let is_trivia = parser.set_newline_as_trivia(true);
    let res = parser.dry_run(|parser| {
        if !parser.bump_if(SyntaxKind::Dot) {
            return false;
        }

        if !parser.bump_if(SyntaxKind::Ident) {
            return false;
        }

        // After the identifier, require `<` or `(` to be on the same line
        parser.set_newline_as_trivia(false);

        if parser.current_kind() == Some(SyntaxKind::Lt)
            && (is_lt_eq(parser)
                || is_lshift(parser)
                || !parser.parses_without_error(GenericArgListScope::default()))
        {
            return false;
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            false
        } else {
            parser.set_newline_as_trivia(is_trivia);
            parser
                .parse_ok(CallArgListScope::default())
                .is_ok_and(identity)
        }
    });
    parser.set_newline_as_trivia(is_trivia);
    res
}
