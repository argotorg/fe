use std::{cell::Cell, convert::Infallible, rc::Rc};

use unwrap_infallible::UnwrapInfallible;

use super::{
    ErrProof, Parser, Recovery,
    attr::{self, parse_attr_list},
    define_scope,
    expr::parse_expr,
    func::FuncDefScope,
    param::{TraitRefScope, TypeBoundListScope, parse_generic_params_opt, parse_where_clause_opt},
    parse_list,
    struct_::RecordFieldDefListScope,
    token_stream::{LexicalToken, TokenStream},
    type_::{TupleTypeScope, parse_type},
    use_tree::UseTreeScope,
};
use crate::{ExpectedKind, SyntaxKind, parser::func::FuncScope};

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
        let modifier_scope = ItemModifierScope::default();
        let modifier = match parser.current_kind() {
            Some(kind) if kind.is_modifier_head() => {
                let modifier_checkpoint = parser.parse_cp(modifier_scope.clone(), None).unwrap();
                checkpoint.get_or_insert(modifier_checkpoint);
                modifier_scope.kind.get()
            }
            _ => ModifierKind::None,
        };

        if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
            parser.error("expected `fn` after `unsafe` keyword");
        } else if modifier.is_pub() && matches!(parser.current_kind(), Some(ImplKw | ExternKw)) {
            let error_msg = format!(
                "`pub` can't be used for `{}`",
                parser.current_token().unwrap().text()
            );
            parser.error(&error_msg);
        }

        parser.expect(
            &[
                ModKw, FnKw, StructKw, ContractKw, EnumKw, TraitKw, ImplKw, UseKw, ConstKw,
                ExternKw, TypeKw,
            ],
            Some(ExpectedKind::Syntax(SyntaxKind::Item)),
        )?;

        match parser.current_kind() {
            Some(ModKw) => parser.parse_cp(ModScope::default(), checkpoint),
            Some(FnKw) => parser.parse_cp(FuncScope::default(), checkpoint),
            Some(StructKw) => parser.parse_cp(super::struct_::StructScope::default(), checkpoint),
            Some(ContractKw) => parser.parse_cp(ContractScope::default(), checkpoint),
            Some(EnumKw) => parser.parse_cp(EnumScope::default(), checkpoint),
            Some(TraitKw) => parser.parse_cp(TraitScope::default(), checkpoint),
            Some(ImplKw) => parser.parse_cp(ImplScope::default(), checkpoint),
            Some(UseKw) => parser.parse_cp(UseScope::default(), checkpoint),
            Some(ConstKw) => parser.parse_cp(ConstScope::default(), checkpoint),
            Some(ExternKw) => parser.parse_cp(ExternScope::default(), checkpoint),
            Some(TypeKw) => parser.parse_cp(TypeAliasScope::default(), checkpoint),
            _ => unreachable!(),
        }?;

        Ok(())
    }
}

define_scope! {
    ItemModifierScope {kind: Rc<Cell<ModifierKind>>},
    ItemModifier
}
impl super::Parse for ItemModifierScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        let mut modifier_kind = ModifierKind::None;

        loop {
            match parser.current_kind() {
                Some(kind) if kind.is_modifier_head() => {
                    let new_kind = modifier_kind.union(kind);
                    if new_kind == modifier_kind {
                        parser.unexpected_token_error(format!(
                            "duplicate {} modifier",
                            kind.describe(),
                        ));
                    } else if kind == SyntaxKind::PubKw && modifier_kind.is_unsafe() {
                        parser.unexpected_token_error(
                            "`pub` modifier must come before `unsafe`".into(),
                        );
                    } else {
                        parser.bump();
                    }
                    modifier_kind = new_kind;
                }
                _ => break,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum ModifierKind {
    #[default]
    None,
    Pub,
    Unsafe,
    PubAndUnsafe,
}
impl ModifierKind {
    fn union(&self, kind: SyntaxKind) -> ModifierKind {
        match kind {
            SyntaxKind::PubKw => {
                if self.is_unsafe() {
                    Self::PubAndUnsafe
                } else {
                    Self::Pub
                }
            }
            SyntaxKind::UnsafeKw => {
                if self.is_pub() {
                    Self::PubAndUnsafe
                } else {
                    Self::Unsafe
                }
            }
            _ => unreachable!(),
        }
    }

    fn is_pub(&self) -> bool {
        matches!(self, Self::Pub | Self::PubAndUnsafe)
    }

    fn is_unsafe(&self) -> bool {
        matches!(self, Self::Unsafe | Self::PubAndUnsafe)
    }
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

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::LBrace]);

        if parser.find_and_pop(SyntaxKind::Ident, ExpectedKind::Name(SyntaxKind::Contract))? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::LBrace, ExpectedKind::Body(SyntaxKind::Contract))? {
            parser.parse(RecordFieldDefListScope::default())?;
        }
        Ok(())
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

define_scope! { TraitItemListScope, TraitItemList, (RBrace, Newline, FnKw) }
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

define_scope! { ImplTraitItemListScope, TraitItemList, (RBrace, FnKw, TypeKw) }
impl super::Parse for ImplTraitItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_trait_item_block(parser, FuncDefScope::Impl)
    }
}

define_scope! { ImplItemListScope, ImplItemList, (RBrace, FnKw) }
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

define_scope! { ExternItemListScope, ExternItemList, (PubKw, UnsafeKw, FnKw) }
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

        let is_modifier = |kind: Option<SyntaxKind>| kind.is_some_and(|k| k.is_modifier_head());

        if is_modifier(parser.current_kind()) {
            let modifier_checkpoint = parser
                .parse_cp(ItemModifierScope::default(), None)
                .unwrap_infallible();
            checkpoint.get_or_insert(modifier_checkpoint);
        }

        match parser.current_kind() {
            Some(SyntaxKind::FnKw) => {
                parser.parse_cp(FuncScope::new(fn_def_scope), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            }
            _ => {
                let proof = parser.error_msg_on_current_token("only `fn` is allowed in this block");
                parser.try_recover().map_err(|r| r.add_err_proof(proof))?;
            }
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
                parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            }
            Some(SyntaxKind::TypeKw) => {
                parser.parse_cp(TraitTypeItemScope::default(), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(&[SyntaxKind::Newline, SyntaxKind::RBrace], None)?;
            }
            _ => {
                let proof = parser
                    .error_msg_on_current_token("only `fn` or `type` is allowed in this block");
                parser.try_recover().map_err(|r| r.add_err_proof(proof))?;
            }
        }
    }

    parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the block")
}
