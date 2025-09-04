use std::convert::identity;

use crate::{ParseError, SyntaxKind};

use super::{
    Parser, define_scope,
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
        parser.parse(PathSegmentScope::new(self.is_expr))?;
        while parser.bump_if(SyntaxKind::Colon2) {
            parser.parse(PathSegmentScope::default())?;
        }
        Ok(())
    }
}

define_scope! { PathSegmentScope { is_expr: bool }, PathSegment }
impl super::Parse for PathSegmentScope {
    type Error = ParseError;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        match parser.current_kind() {
            Some(SyntaxKind::Lt) if is_qualified_type(parser) => {
                parser.parse(QualifiedTypeScope::default())
            }
            Some(kind) if is_path_segment(kind) => {
                parser.bump();

                if parser.current_kind() == Some(SyntaxKind::Lt)
                    && !(is_lt_eq(parser) || is_lshift(parser))
                    && parser.dry_run(|parser| {
                        parser
                            .parse_ok(GenericArgListScope::new(self.is_expr))
                            .is_ok_and(identity)
                    })
                {
                    parser
                        .parse(GenericArgListScope::new(self.is_expr))
                        .expect("dry_run suggests this will succeed");
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
        parser.bump_expected(SyntaxKind::AsKw);
        parser.parse(TraitRefScope::default())?;
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

fn is_qualified_type<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser
        .dry_run(|parser| {
            parser.bump_expected(SyntaxKind::Lt);
            parse_type(parser, None).ok()?;
            (parser.current_kind() == Some(SyntaxKind::AsKw)).then_some(())
        })
        .is_some()
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
