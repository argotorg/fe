use super::{
    Checkpoint, ErrProof, Parser, Recovery,
    attr::{self, parse_attr_list},
    define_scope,
    expr::parse_expr,
    expr_atom::BlockExprScope,
    func::FuncDefScope,
    param::{
        FuncParamListScope, TraitRefScope, TypeBoundListScope, parse_generic_params_opt,
        parse_where_clause_opt,
    },
    parse_list,
    path::PathScope,
    struct_::{RecordFieldDefListScope, RecordFieldDefScope},
    token_stream::{LexicalToken, TokenStream},
    type_::{TupleTypeScope, parse_type},
    use_tree::UseTreeScope,
};
use crate::{
    ExpectedKind, SyntaxKind,
    parser::{
        func::{FuncScope, UsesClauseScope},
        pat::parse_recv_arm_pat,
    },
};

define_scope! {
    #[doc(hidden)]
    pub ItemListScope {inside_mod: bool},
    ItemList,
    (
        ModKw,
        FnKw,
        StructKw,
        ContractKw,
        EnumKw,
        TraitKw,
        ImplKw,
        UseKw,
        ConstKw,
        ExternKw,
        TypeKw,
        PubKw,
        UnsafeKw,
        DocComment,
        Pound
    )
}
impl super::Parse for ItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        use crate::SyntaxKind::*;

        if self.inside_mod {
            parser.bump_expected(LBrace);
            parser.set_scope_recovery_stack(&[RBrace]);
        }

        loop {
            parser.set_newline_as_trivia(true);
            if self.inside_mod && parser.bump_if(RBrace) {
                break;
            }
            if parser.current_kind().is_none() {
                if self.inside_mod {
                    parser.add_error(crate::ParseError::expected(
                        &[RBrace],
                        Some(ExpectedKind::ClosingBracket {
                            bracket: RBrace,
                            parent: Mod,
                        }),
                        parser.current_pos,
                    ));
                }
                break;
            }

            let ok = parser.parse_ok(ItemScope::default())?;
            if parser.current_kind().is_none() || (self.inside_mod && parser.bump_if(RBrace)) {
                break;
            }
            if ok {
                parser.set_newline_as_trivia(false);
                if parser.find(
                    Newline,
                    ExpectedKind::Separator {
                        separator: Newline,
                        element: Item,
                    },
                )? {
                    parser.bump();
                }
            }
        }
        Ok(())
    }
}

define_scope! {
    #[doc(hidden)]
    pub(super) ItemScope,
    Item
}
impl super::Parse for ItemScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        use crate::SyntaxKind::*;

        let mut checkpoint = attr::parse_attr_list(parser)?;
        let modifiers = parse_item_modifiers(parser, &mut checkpoint);

        if modifiers.is_unsafe && !is_fn_item_head(parser) {
            parser.error("expected `fn` after `unsafe` keyword");
        } else if modifiers.is_pub && matches!(parser.current_kind(), Some(ImplKw | ExternKw)) {
            let error_msg = format!(
                "`pub` can't be used for `{}`",
                parser.current_token().unwrap().text()
            );
            parser.error(&error_msg);
        }

        parser.expect(
            &[
                ModKw, FnKw, StructKw, ContractKw, MsgKw, EnumKw, TraitKw, ImplKw, UseKw, ConstKw,
                ExternKw, TypeKw,
            ],
            Some(ExpectedKind::Syntax(SyntaxKind::Item)),
        )?;

        match parser.current_kind() {
            Some(ModKw) => parser.parse_cp(ModScope::default(), checkpoint),
            Some(FnKw) => parser.parse_cp(FuncScope::default(), checkpoint),
            Some(StructKw) => parser.parse_cp(super::struct_::StructScope::default(), checkpoint),
            Some(ContractKw) => parser.parse_cp(ContractScope::default(), checkpoint),
            Some(MsgKw) => parser.parse_cp(MsgScope::default(), checkpoint),
            Some(EnumKw) => parser.parse_cp(EnumScope::default(), checkpoint),
            Some(TraitKw) => parser.parse_cp(TraitScope::default(), checkpoint),
            Some(ImplKw) => parser.parse_cp(ImplScope::default(), checkpoint),
            Some(UseKw) => parser.parse_cp(UseScope::default(), checkpoint),
            Some(ConstKw) => {
                if is_fn_item_head(parser) {
                    parser.parse_cp(FuncScope::default(), checkpoint)
                } else {
                    parser.parse_cp(ConstScope::default(), checkpoint)
                }
            }
            Some(ExternKw) => parser.parse_cp(ExternScope::default(), checkpoint),
            Some(TypeKw) => parser.parse_cp(TypeAliasScope::default(), checkpoint),
            _ => unreachable!(),
        }?;

        Ok(())
    }
}

fn is_fn_item_head<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|p| {
        p.bump_if(SyntaxKind::ConstKw);
        p.current_kind() == Some(SyntaxKind::FnKw)
    })
}

#[derive(Debug, Clone, Copy, Default)]
struct ItemModifiers {
    is_pub: bool,
    is_unsafe: bool,
}

fn parse_item_modifiers<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: &mut Option<Checkpoint>,
) -> ItemModifiers {
    let mut modifiers = ItemModifiers::default();

    loop {
        match parser.current_kind() {
            Some(SyntaxKind::PubKw) => {
                if checkpoint.is_none() {
                    *checkpoint = Some(parser.checkpoint());
                }

                if modifiers.is_pub {
                    parser.unexpected_token_error(format!(
                        "duplicate {} modifier",
                        SyntaxKind::PubKw.describe(),
                    ));
                } else if modifiers.is_unsafe {
                    parser
                        .unexpected_token_error("`pub` modifier must come before `unsafe`".into());
                    modifiers.is_pub = true;
                } else {
                    parser.bump();
                    modifiers.is_pub = true;
                }
            }
            Some(SyntaxKind::UnsafeKw) => {
                if checkpoint.is_none() {
                    *checkpoint = Some(parser.checkpoint());
                }

                if modifiers.is_unsafe {
                    parser.unexpected_token_error(format!(
                        "duplicate {} modifier",
                        SyntaxKind::UnsafeKw.describe(),
                    ));
                } else {
                    parser.bump();
                    modifiers.is_unsafe = true;
                }
            }
            _ => break,
        }
    }

    modifiers
}

define_scope! { ModScope, Mod }
impl super::Parse for ModScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ModKw);

        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::LBrace,
            SyntaxKind::RBrace,
        ]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Mod))? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Mod))? {
            parser.parse(ItemListScope::new(true))?;
        }
        Ok(())
    }
}

define_scope! { ContractScope, Contract }
impl super::Parse for ContractScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ContractKw);

        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::UsesKw,
            SyntaxKind::LBrace,
        ]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Contract))? {
            parser.bump();
        }

        // Optional `uses` clause after the contract name
        if parser.current_kind() == Some(SyntaxKind::UsesKw) {
            parser.parse(UsesClauseScope::default())?;
        }
        parser.pop_recovery_stack(); // remove `UsesKw` from recovery stack

        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Contract))? {
            parser.bump_expected(SyntaxKind::LBrace);

            parser.parse(ContractFieldsScope::default())?;

            // Optional `init` block
            if parser.is_ident("init") {
                parser.parse(ContractInitScope::default())?;
            }

            // Zero or more `recv` blocks
            loop {
                if !parser.is_ident("recv") {
                    break;
                }
                parser.parse(ContractRecvScope::default())?;
            }

            parser.bump_or_recover(
                SyntaxKind::RBrace,
                "expected `}` to close the contract body",
            )?;
        }
        Ok(())
    }
}

// Parses the leading contract fields inside the contract body.
// Comma separators are optional; items can be delimited by commas or newlines.
define_scope! { ContractFieldsScope, SyntaxKind::ContractFields }
impl super::Parse for ContractFieldsScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // Keep consuming field definitions while they parse cleanly.
        // Stop when we reach `init`, `recv`, or `}`.
        loop {
            // Stop conditions
            match parser.current_kind() {
                Some(SyntaxKind::RBrace) | None => break,
                Some(SyntaxKind::Ident)
                    if matches!(parser.current_token().unwrap().text(), "init" | "recv") =>
                {
                    break;
                }
                _ => {}
            }

            parser.parse(RecordFieldDefScope::default())?;

            // Optional comma between fields
            let _ = parser.bump_if(SyntaxKind::Comma);
        }
        Ok(())
    }
}

// Parses the `init` block within a contract.
define_scope! { ContractInitScope, SyntaxKind::ContractInit }
impl super::Parse for ContractInitScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        debug_assert!(parser.is_ident("init"));
        // bump `init`
        parser.bump();

        // Parameter list
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(FuncParamListScope::new(false))?;
        }

        // Optional `uses` clause
        let nt = parser.set_newline_as_trivia(true);
        if parser.current_kind() == Some(SyntaxKind::UsesKw) {
            parser.parse(UsesClauseScope::default())?;
        }
        parser.set_newline_as_trivia(nt);

        // Body block
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(BlockExprScope::default())?;
        }
        Ok(())
    }
}

// Parses a `recv` block within a contract, in either form:
// - `recv Type { ... }`
// - `recv { ... }`
define_scope! { ContractRecvScope, SyntaxKind::ContractRecv }
impl super::Parse for ContractRecvScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        debug_assert!(parser.is_ident("recv"));
        parser.bump();

        // Optional message root path before the block
        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.or_recover(|p| p.parse(PathScope::default()))?;
        }

        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecvArmListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { RecvArmListScope, RecvArmList }
impl super::Parse for RecvArmListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LBrace);
        while parser.current_kind() != Some(SyntaxKind::RBrace) && parser.current_kind().is_some() {
            parser.parse(RecvArmScope::default())?;
        }
        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close recv block")?;
        Ok(())
    }
}

// Parses: `Pattern -> RetTy uses (...) { body }`
define_scope! { RecvArmScope, RecvArm }
impl super::Parse for RecvArmScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        parse_recv_arm_pat(parser)?;

        parser.set_newline_as_trivia(true);

        // Optional return type
        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None)?;
        }

        // Optional uses clause
        if parser.current_kind() == Some(SyntaxKind::UsesKw) {
            parser.parse(UsesClauseScope::default())?;
        }

        // Body block
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(BlockExprScope::default())?;
        }

        Ok(())
    }
}
define_scope! { MsgScope, Msg }
impl super::Parse for MsgScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::MsgKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::LBrace]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Msg))? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Msg))? {
            parser.parse(MsgVariantListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { MsgVariantListScope, MsgVariantList, (Comma, RBrace) }
impl super::Parse for MsgVariantListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::MsgVariantList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(MsgVariantScope::default()),
        )
    }
}

define_scope! { MsgVariantScope, MsgVariant }
impl super::Parse for MsgVariantScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        // Parse attribute list
        parse_attr_list(parser)?;

        // Parse variant name
        parser.bump_or_recover(SyntaxKind::Ident, "expected identifier for message variant")?;

        // Parse optional parameters
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(MsgVariantParamsScope::default())?;
        }

        // Parse optional return type
        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None)?;
        }

        Ok(())
    }
}

define_scope! { MsgVariantParamsScope, MsgVariantParams, (Comma, RBrace) }
impl super::Parse for MsgVariantParamsScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::MsgVariantParams,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(RecordFieldDefScope::default()),
        )
    }
}

define_scope! { EnumScope, Enum }
impl super::Parse for EnumScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::EnumKw);

        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::WhereKw,
            SyntaxKind::LBrace,
        ]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Enum))? {
            parser.bump();
        }

        parser.pop_recovery_stack();
        parse_generic_params_opt(parser, false)?;

        parser.pop_recovery_stack();
        parse_where_clause_opt(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Enum))? {
            parser.parse(VariantDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { VariantDefListScope, VariantDefList, (Comma, RBrace) }
impl super::Parse for VariantDefListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            SyntaxKind::VariantDefList,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(VariantDefScope::default()),
        )
    }
}

define_scope! { VariantDefScope, VariantDef }
impl super::Parse for VariantDefScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_attr_list(parser)?;
        parser.bump_or_recover(SyntaxKind::Ident, "expected ident for the variant name")?;

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(TupleTypeScope::default())?;
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecordFieldDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { TraitScope, Trait }
impl super::Parse for TraitScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::TraitKw);
        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::Colon,
            SyntaxKind::WhereKw,
            SyntaxKind::LBrace,
        ]);
        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Trait))? {
            parser.bump();
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_generic_params_opt(parser, false)?;

        parser.expect_and_pop_recovery_stack()?;
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(SuperTraitListScope::default())?;
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser)?;

        if parser.find(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Trait))? {
            parser.parse(TraitItemListScope::default())?;
        }
        Ok(())
    }
}

define_scope! {SuperTraitListScope, SuperTraitList, (Plus)}
impl super::Parse for SuperTraitListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Colon);
        loop {
            parser.parse_or_recover(TraitRefScope::default())?;
            if !parser.bump_if(SyntaxKind::Plus) {
                break;
            }
        }
        Ok(())
    }
}

define_scope! { TraitItemListScope, TraitItemList, (RBrace, Newline, FnKw, TypeKw, ConstKw) }
impl super::Parse for TraitItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_trait_item_block(parser, FuncDefScope::TraitDef)
    }
}

define_scope! { TraitTypeItemScope, TraitTypeItem }
impl super::Parse for TraitTypeItemScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Eq]);
        if parser.find_and_pop(
            SyntaxKind::Ident,
            ExpectedKind::Name(SyntaxKind::TraitTypeItem),
        )? {
            parser.bump();
        }

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::new(false))?;
        }

        if parser.current_kind() == Some(SyntaxKind::Eq) {
            parser.bump();
            parse_type(parser, None)?;
        }

        Ok(())
    }
}

define_scope! { TraitConstItemScope, TraitConstItem }
impl super::Parse for TraitConstItemScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::ConstKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Colon, SyntaxKind::Eq]);

        if parser.find_and_pop(
            SyntaxKind::Ident,
            ExpectedKind::Name(SyntaxKind::TraitConstItem),
        )? {
            parser.bump();
        }

        if parser.find_and_pop(
            SyntaxKind::Colon,
            ExpectedKind::TypeSpecifier(SyntaxKind::TraitConstItem),
        )? {
            parser.bump();
            parse_type(parser, None)?;
        }

        parser.set_newline_as_trivia(true);
        if parser.bump_if(SyntaxKind::Eq) {
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ImplScope, Impl, (ForKw, LBrace) }
impl super::Parse for ImplScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ImplKw);

        parse_generic_params_opt(parser, false)?;

        let is_impl_trait = parser.dry_run(|parser| {
            parser.parse(TraitRefScope::default()).is_ok()
                && parser
                    .find(SyntaxKind::ForKw, ExpectedKind::Unspecified)
                    .is_ok_and(|x| x)
        });

        if is_impl_trait {
            self.set_kind(SyntaxKind::ImplTrait);
            parser.set_scope_recovery_stack(&[
                SyntaxKind::ForKw,
                SyntaxKind::WhereKw,
                SyntaxKind::LBrace,
            ]);

            parser.parse_or_recover(TraitRefScope::default())?;
            if parser.find_and_pop(SyntaxKind::ForKw, ExpectedKind::Unspecified)? {
                parser.bump();
            }
        } else {
            parser.set_scope_recovery_stack(&[SyntaxKind::WhereKw, SyntaxKind::LBrace]);
        }

        parse_type(parser, None)?;

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser)?;

        if parser.find_and_pop(
            SyntaxKind::LBrace,
            ExpectedKind::Body(SyntaxKind::ImplTrait),
        )? {
            if is_impl_trait {
                parser.parse(ImplTraitItemListScope::default())?;
            } else {
                parser.parse(ImplItemListScope::default())?;
            }
        }
        Ok(())
    }
}

define_scope! { ImplTraitItemListScope, TraitItemList, (RBrace, FnKw, TypeKw, ConstKw) }
impl super::Parse for ImplTraitItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_trait_item_block(parser, FuncDefScope::Impl)
    }
}

define_scope! { ImplItemListScope, ImplItemList, (RBrace, ConstKw, FnKw) }
impl super::Parse for ImplItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, FuncDefScope::Impl)
    }
}

define_scope! { UseScope, Use }
impl super::Parse for UseScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::UseKw);
        parser.parse(UseTreeScope::default())
    }
}

define_scope! { ConstScope, Const }
impl super::Parse for ConstScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_attr_list(parser)?;

        parser.bump_expected(SyntaxKind::ConstKw);
        parser.set_newline_as_trivia(false);
        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Colon, SyntaxKind::Eq]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Const))? {
            parser.bump();
        }
        if parser.find_and_pop(
            SyntaxKind::Colon,
            ExpectedKind::TypeSpecifier(SyntaxKind::Const),
        )? {
            parser.bump();
            parse_type(parser, None)?;
        }

        parser.set_newline_as_trivia(true);
        if parser.find_and_pop(SyntaxKind::Eq, ExpectedKind::Unspecified)? {
            parser.bump();
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ExternScope, Extern }
impl super::Parse for ExternScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ExternKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::LBrace]);
        if parser.find(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Extern))? {
            parser.parse(ExternItemListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { ExternItemListScope, ExternItemList, (PubKw, UnsafeKw, ConstKw, FnKw) }
impl super::Parse for ExternItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, FuncDefScope::Extern)
    }
}

define_scope! { TypeAliasScope, TypeAlias }
impl super::Parse for TypeAliasScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Lt, SyntaxKind::Eq]);
        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::TypeAlias))? {
            parser.bump();
        }

        parser.pop_recovery_stack();
        parse_generic_params_opt(parser, true)?;

        if parser.find_and_pop(SyntaxKind::Eq, ExpectedKind::Unspecified)? {
            parser.bump();
            parse_type(parser, None)?;
        }
        Ok(())
    }
}

/// This function is used to parse items in `impl` and `extern` blocks,
/// which only allow `fn` definitions.
fn parse_fn_item_block<S: TokenStream>(
    parser: &mut Parser<S>,
    fn_def_scope: FuncDefScope,
) -> Result<(), Recovery<ErrProof>> {
    parser.bump_expected(SyntaxKind::LBrace);
    loop {
        parser.set_newline_as_trivia(true);
        if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            break;
        }

        let mut checkpoint = attr::parse_attr_list(parser)?;
        let modifiers = parse_item_modifiers(parser, &mut checkpoint);

        let is_fn_head = is_fn_item_head(parser);

        if modifiers.is_unsafe && !is_fn_head {
            parser.error("expected `fn` after `unsafe` keyword");
        }

        if is_fn_head {
            parser.parse_cp(FuncScope::new(fn_def_scope), checkpoint)?;

            parser.set_newline_as_trivia(false);
            parser.expect(
                &[
                    SyntaxKind::Newline,
                    SyntaxKind::RBrace,
                    SyntaxKind::DocComment,
                    SyntaxKind::DocCommentAttr,
                ],
                None,
            )?;
        } else {
            let proof = parser.error_msg_on_current_token("only `fn` is allowed in this block");
            parser.try_recover().map_err(|r| r.add_err_proof(proof))?;
        }
    }

    parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the block")
}

fn parse_trait_item_block<S: TokenStream>(
    parser: &mut Parser<S>,
    fn_def_scope: FuncDefScope,
) -> Result<(), Recovery<ErrProof>> {
    parser.bump_expected(SyntaxKind::LBrace);
    loop {
        parser.set_newline_as_trivia(true);
        if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            break;
        }

        let checkpoint = attr::parse_attr_list(parser)?;

        while parser.current_kind().is_some_and(|k| k.is_modifier_head()) {
            let kind = parser.current_kind().unwrap();
            parser.unexpected_token_error(format!(
                "{} modifier is not allowed in this block",
                kind.describe()
            ));
        }

        match parser.current_kind() {
            Some(SyntaxKind::FnKw) => {
                parser.parse_cp(FuncScope::new(fn_def_scope), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(
                    &[
                        SyntaxKind::Newline,
                        SyntaxKind::RBrace,
                        SyntaxKind::DocComment,
                        SyntaxKind::DocCommentAttr,
                    ],
                    None,
                )?;
            }
            Some(SyntaxKind::TypeKw) => {
                parser.parse_cp(TraitTypeItemScope::default(), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            }
            Some(SyntaxKind::ConstKw) if is_fn_item_head(parser) => {
                parser.parse_cp(FuncScope::new(fn_def_scope), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(
                    &[
                        SyntaxKind::Newline,
                        SyntaxKind::RBrace,
                        SyntaxKind::DocComment,
                        SyntaxKind::DocCommentAttr,
                    ],
                    None,
                )?;
            }
            Some(SyntaxKind::ConstKw) => {
                parser.parse_cp(TraitConstItemScope::default(), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            }
            _ => {
                let proof = parser.error_msg_on_current_token(
                    "only `fn`, `type`, or `const` is allowed in this block",
                );
                parser.try_recover().map_err(|r| r.add_err_proof(proof))?;
            }
        }
    }

    parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the block")
}
