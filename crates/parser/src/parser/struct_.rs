use crate::{ExpectedKind, SyntaxKind};

use super::{
    ErrProof, Parser, Recovery,
    attr::parse_attr_list,
    define_scope,
    func::FuncScope,
    param::{parse_generic_params_opt, parse_where_clause_opt},
    parse_list,
    token_stream::TokenStream,
    type_::parse_type,
};

define_scope! { pub(crate) StructScope, Struct }
impl super::Parse for StructScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::StructKw);

        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::WhereKw,
            SyntaxKind::LBrace,
        ]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Struct))? {
            parser.bump();
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_generic_params_opt(parser, false)?;

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Struct))? {
            parser.parse(RecordFieldDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! {
    pub(crate) RecordFieldDefListScope,
    RecordFieldDefList,
    (RBrace, Comma, Newline)
}
impl super::Parse for RecordFieldDefListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::RecordFieldDefList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(RecordFieldDefScope::default()),
        )
    }
}

define_scope! { RecordFieldDefScope, RecordFieldDef }
impl super::Parse for RecordFieldDefScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parse_attr_list(parser)?;

        parser.bump_if(SyntaxKind::PubKw);
        // Since the Fe-V2 doesn't support method definition in a struct, we add an
        // ad-hoc check for the method definition in a struct to avoid the confusing
        // error message.
        // The reason that justifies this ad-hoc check is
        // 1. This error is difficult to recover properly with the current parser
        //    design, and the emitted error message is confusing.
        // 2. We anticipate that this error would happen often in the transition period
        //    to Fe-V2.
        if parser.current_kind() == Some(SyntaxKind::FnKw) {
            parser.error_msg_on_current_token("function definition in struct is not allowed");
            let checkpoint = parser.enter(super::ErrorScope::new(), None);
            parser.parse(FuncScope::default())?;
            parser.leave(checkpoint);
            return Ok(());
        }

        parser.set_scope_recovery_stack(&[SyntaxKind::Colon]);

        if parser.find(
            SyntaxKind::Ident,
            ExpectedKind::Name(SyntaxKind::RecordField),
        )? {
            parser.bump();
        }

        if parser.find(
            SyntaxKind::Colon,
            ExpectedKind::TypeSpecifier(SyntaxKind::RecordField),
        )? {
            parser.bump();
            parse_type(parser, None).map(|_| ())?;
        }
        Ok(())
    }
}
