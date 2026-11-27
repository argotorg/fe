use crate::{ParseError, SyntaxKind, TextRange, TextSize};

use super::{
    Parser, ProbeKind, define_scope,
    expr::{is_lshift, is_lt_eq},
    param::{GenericArgListScope, TraitRefScope},
    token_stream::TokenStream,
    type_::parse_type,
};

define_scope! {
    #[doc(hidden)]
    pub PathScope { is_expr: bool },
    Path,
    (Colon2)
}
impl super::Parse for PathScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.parse(PathSegmentScope::new(self.is_expr, self.is_expr))?;
        while parser.bump_if(SyntaxKind::Colon2) {
            parser.parse(PathSegmentScope::new(false, self.is_expr))?;
        }
        Ok(())
    }
}

define_scope! {
    /// `is_expr` marks the first segment of an expression's path, and
    /// `expr_path` every segment of it.
    PathSegmentScope { is_expr: bool, expr_path: bool },
    PathSegment
}
impl super::Parse for PathSegmentScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        match parser.current_kind() {
            Some(SyntaxKind::Lt) if is_qualified_type(parser) => {
                parser.parse(QualifiedTypeScope::default())
            }
            Some(kind) if is_path_segment(kind) => {
                parser.bump();

                if parser.current_kind_same_line() == Some(SyntaxKind::Lt) && is_lshift(parser) {
                    if lshift_opens_generic_args(parser) {
                        // Errors inside the arguments are reported as they are parsed.
                        let _ = parser.parse(GenericArgListScope::new(self.is_expr));
                    }
                    return Ok(());
                }

                let is_turbofish = parser.current_kind_same_line() == Some(SyntaxKind::Colon2)
                    && parser.peek_two() == (Some(SyntaxKind::Colon2), Some(SyntaxKind::Lt));

                if (is_turbofish
                    || (parser.current_kind_same_line() == Some(SyntaxKind::Lt)
                        && !is_lt_eq(parser)))
                    && parser.probe(
                        ProbeKind::GenericArgList {
                            is_expr: self.is_expr,
                            expr_path: self.expr_path,
                        },
                        |parser| {
                            parser.bump_if(SyntaxKind::Colon2);
                            parser.parses_without_error(GenericArgListScope::new(self.is_expr))
                                && !(self.expr_path
                                    && parser
                                        .current_kind_same_line()
                                        .is_some_and(cannot_follow_path_expr))
                        },
                    )
                {
                    if is_turbofish {
                        parser.bump_trivias();
                        parser.add_error(ParseError::Unexpected(
                            "unexpected turbofish syntax `::<`; remove the double colons".into(),
                            TextRange::at(parser.current_pos, TextSize::from(3)),
                        ));
                        parser.bump_expected(SyntaxKind::Colon2);
                    }
                    parser
                        .parse(GenericArgListScope::new(self.is_expr))
                        .expect("the probe suggests this will succeed");
                }
                Ok(())
            }
            _ => Err(ParseError::expected(
                &[SyntaxKind::PathSegment],
                None,
                parser.end_of_prev_token,
            )),
        }
    }
}

define_scope! { QualifiedTypeScope, QualifiedType }
impl super::Parse for QualifiedTypeScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Lt);

        match parse_type(parser, None) {
            Ok(_) => {}
            Err(_) => {
                return Err(ParseError::expected(
                    &[SyntaxKind::PathType],
                    None,
                    parser.end_of_prev_token,
                ));
            }
        }
        if parser.bump_if(SyntaxKind::AsKw) {
            parser.parse(TraitRefScope::default())?;
        } else if parser.current_kind() == Some(SyntaxKind::Gt) {
            // `<T>::item` is read as a qualified path that lacks its trait, so
            // the error is that, and the rest of the path still parses.
            parser.add_error(ParseError::Msg(
                "missing `as Trait` in qualified path; write `<T as Trait>::item`, or `T::item` if `T` is a named type"
                    .into(),
                TextRange::empty(parser.end_of_prev_token),
            ));
        } else {
            return Err(ParseError::expected(
                &[SyntaxKind::AsKw],
                None,
                parser.end_of_prev_token,
            ));
        }
        if parser.bump_if(SyntaxKind::Gt) {
            Ok(())
        } else {
            Err(ParseError::expected(
                &[SyntaxKind::Gt],
                None,
                parser.end_of_prev_token,
            ))
        }
    }
}

/// Whether `kind`, right after a `>` that would close an expression path's
/// generic arguments, shows that the `<` and `>` are comparisons instead. It
/// starts an operand and can never continue an expression after one, as the
/// `0` in `N < 8, M > 0`, or it is the `=` of a `>=`, as in `N < 8, M >= 1`.
/// So a `,` that separates call arguments, tuple elements or `where`
/// predicates does not end up inside generic arguments. A token that can
/// continue an expression, such as the `(` of `N < 8, M > (1)`, keeps the
/// generic arguments, as in `f<8, M>(1)`.
fn cannot_follow_path_expr(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        Ident
            | SelfKw
            | SelfTypeKw
            | IngotKw
            | SuperKw
            | Int
            | String
            | TrueKw
            | FalseKw
            | IfKw
            | MatchKw
            | Not
            | Tilde
            | MutKw
            | RefKw
            | Eq
    )
}

/// Whether the `<<` at the current position opens generic arguments whose first
/// argument is a qualified path, as in `Wrapped<<T as Trait>::Item>`, rather
/// than being a left shift.
///
/// No shift operand continues with `>::`, so that prefix settles it, and a cast
/// such as `value << bits as u256 >> 1` stays a shift. Path segments and
/// method calls must agree on this, or `Wrapped<<T as Trait>::Item>::new()`
/// parses while `receiver.take<<T as Trait>::Item>(x)` does not.
pub(super) fn lshift_opens_generic_args<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.probe(ProbeKind::LShiftOpensGenericArgs, |parser| {
        parser.bump();
        parser.parses_without_error(QualifiedTypeScope::default())
            && parser.current_kind() == Some(SyntaxKind::Colon2)
    })
}

/// Whether a qualified type starts here: `<T as Trait>`, or `<T>` and a `::`,
/// which lacks its trait and is reported where it is parsed.
pub(super) fn is_qualified_type<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.probe(ProbeKind::QualifiedType, |parser| {
        parser.bump_if(SyntaxKind::Lt)
            && parse_type(parser, None).is_ok()
            && (parser.current_kind() == Some(SyntaxKind::AsKw)
                || (parser.bump_if(SyntaxKind::Gt)
                    && parser.current_kind() == Some(SyntaxKind::Colon2)))
    })
}

pub(super) fn is_path_segment(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SelfTypeKw
            | SyntaxKind::SelfKw
            | SyntaxKind::IngotKw
            | SyntaxKind::SuperKw
            | SyntaxKind::Ident
            | SyntaxKind::Lt
    )
}
