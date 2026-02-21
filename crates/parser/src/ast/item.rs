use rowan::ast::{AstNode, support};

use super::{TraitRef, TupleType, TypeBoundList, ast_node};
use crate::{FeLang, SyntaxKind as SK, SyntaxToken};

ast_node! {
    /// The top-level node of the AST tree.
    pub struct Root,
    SK::Root,
}
impl Root {
    pub fn items(&self) -> Option<ItemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A list of items in a module.
    pub struct ItemList,
    SK::ItemList,
    IntoIterator<Item=Item>
}

ast_node! {
    /// A single item in a module.
    /// Use `[Item::kind]` to get the specific type of item.
    pub struct Item,
    SK::Item
}
impl Item {
    pub fn kind(&self) -> Option<ItemKind> {
        support::child(self.syntax())
            .map(ItemKind::Mod)
            .or_else(|| support::child(self.syntax()).map(ItemKind::Func))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Struct))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Contract))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Msg))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Enum))
            .or_else(|| support::child(self.syntax()).map(ItemKind::TypeAlias))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Impl))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Trait))
            .or_else(|| support::child(self.syntax()).map(ItemKind::ImplTrait))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Const))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Use))
            .or_else(|| support::child(self.syntax()).map(ItemKind::Extern))
    }
}

ast_node! {
    pub struct Mod,
    SK::Mod,
}
impl super::AttrListOwner for Mod {}
impl super::ItemModifierOwner for Mod {}
impl Mod {
    /// Returns the name of the function.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the function's parameter list.
    pub fn items(&self) -> Option<ItemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `pub fn foo<T, U: Trait>(_ x: T, from u: U) -> T where T: Trait2 { ... }`
    pub struct Func,
    SK::Func,
}
ast_node! {
    /// `foo<T>(a: T) -> T where T: Clone`
    pub struct FuncSignature,
    SK::FuncSignature,
}
impl super::GenericParamsOwner for FuncSignature {}
impl super::WhereClauseOwner for FuncSignature {}
impl FuncSignature {
    /// Returns the name of the function.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the function's parameter list.
    pub fn params(&self) -> Option<super::FuncParamList> {
        support::child(self.syntax())
    }

    /// Returns the function's return type.
    pub fn ret_ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the optional `uses` clause of the function.
    pub fn uses_clause(&self) -> Option<super::UsesClause> {
        support::child(self.syntax())
    }

    /// Returns the where clause of the function.
    pub fn where_clause(&self) -> Option<super::WhereClause> {
        support::child(self.syntax())
    }
}
impl super::AttrListOwner for Func {}
impl super::ItemModifierOwner for Func {}
impl Func {
    pub fn const_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::ConstKw)
    }

    /// Returns the function's signature if present in the syntax tree.
    /// This is primarily for consumers (like lazy spans) that need to handle
    /// malformed code without panicking.
    pub fn signature_opt(&self) -> Option<FuncSignature> {
        support::child(self.syntax())
    }

    /// Returns the function's signature.
    pub fn sig(&self) -> FuncSignature {
        self.signature_opt()
            .expect("a function must always contain a signature node")
    }

    /// Returns the function's body.
    pub fn body(&self) -> Option<super::BlockExpr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct Struct,
    SK::Struct,
}
impl super::GenericParamsOwner for Struct {}
impl super::WhereClauseOwner for Struct {}
impl super::AttrListOwner for Struct {}
impl super::ItemModifierOwner for Struct {}
impl Struct {
    /// Returns the name of the struct.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the struct's field def list.
    pub fn fields(&self) -> Option<RecordFieldDefList> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct Contract,
    SK::Contract,
}

impl super::AttrListOwner for Contract {}
impl super::ItemModifierOwner for Contract {}
impl Contract {
    /// Returns the name of the contract.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the contract's leading fields section.
    pub fn fields(&self) -> Option<ContractFields> {
        support::child(self.syntax())
    }

    /// Returns the optional `uses` clause of the contract.
    pub fn uses_clause(&self) -> Option<super::UsesClause> {
        support::child(self.syntax())
    }

    /// Returns the optional `init` block of the contract.
    pub fn init_block(&self) -> Option<ContractInit> {
        support::child(self.syntax())
    }

    /// Returns all `recv` blocks declared in the contract.
    pub fn recvs(&self) -> rowan::ast::AstChildren<ContractRecv> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// A section containing the leading contract fields.
    pub struct ContractFields,
    SK::ContractFields,
    IntoIterator<Item=RecordFieldDef>
}

ast_node! {
    /// The contract initialization block: `init(...) uses (...) { ... }`.
    pub struct ContractInit,
    SK::ContractInit,
}
impl ContractInit {
    pub fn params(&self) -> Option<super::FuncParamList> {
        support::child(self.syntax())
    }

    pub fn uses_clause(&self) -> Option<super::UsesClause> {
        support::child(self.syntax())
    }

    pub fn body(&self) -> Option<super::BlockExpr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A `recv` block inside a contract. Supports both typed and untyped forms.
    pub struct ContractRecv,
    SK::ContractRecv,
}
impl ContractRecv {
    /// Optional root message type path (`recv Type { ... }`).
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    /// The list of arms in this recv block.
    pub fn arms(&self) -> Option<RecvArmList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// List of recv arms inside a recv block.
    pub struct RecvArmList,
    SK::RecvArmList,
    IntoIterator<Item=RecvArm>
}

ast_node! {
    /// A single recv arm: `Pattern -> RetTy uses (...) { body }`
    pub struct RecvArm,
    SK::RecvArm,
}
impl RecvArm {
    /// The pattern being matched (e.g., `Transfer { to, amount }`).
    pub fn pat(&self) -> Option<super::Pat> {
        support::child(self.syntax())
    }

    /// Optional return type.
    pub fn ret_ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Optional uses clause.
    pub fn uses_clause(&self) -> Option<super::UsesClause> {
        support::child(self.syntax())
    }

    /// The body block.
    pub fn body(&self) -> Option<super::BlockExpr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct Enum,
    SK::Enum,
}
impl super::GenericParamsOwner for Enum {}
impl super::WhereClauseOwner for Enum {}
impl super::AttrListOwner for Enum {}
impl super::ItemModifierOwner for Enum {}
impl Enum {
    /// Returns the name of the enum.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the enum's variant def list.
    pub fn variants(&self) -> Option<VariantDefList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `type Foo<T> = Bar<T>`
    pub struct TypeAlias,
    SK::TypeAlias,
}
impl super::GenericParamsOwner for TypeAlias {}
impl super::AttrListOwner for TypeAlias {}
impl super::ItemModifierOwner for TypeAlias {}
impl TypeAlias {
    /// Returns the name of the type alias.
    /// `Foo` in `type Foo<T> = Bar<T>`
    pub fn alias(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the type alias's type.
    /// `Bar<T>` in `type Foo<T> = Bar<T>`
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `trait Foo<..> where .. { .. }`
    pub struct Trait,
    SK::Trait,
}
impl super::GenericParamsOwner for Trait {}
impl super::WhereClauseOwner for Trait {}
impl super::AttrListOwner for Trait {}
impl super::ItemModifierOwner for Trait {}
impl Trait {
    /// Returns the name of the trait.
    /// `Foo` in `trait Foo<..> where .. { .. }`
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the trait's item list.
    /// `{ .. }` in `trait Foo<..> where .. { .. }`
    pub fn item_list(&self) -> Option<TraitItemList> {
        support::child(self.syntax())
    }

    pub fn super_trait_list(&self) -> Option<SuperTraitList> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct SuperTraitList,
    SK::SuperTraitList,
    IntoIterator<Item=TraitRef>
}
impl SuperTraitList {
    pub fn colon(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Colon)
    }
}

ast_node! {
    /// `type Bar` in trait definition
    /// or `type Bar = i32` in trait implementation
    pub struct TraitTypeItem,
    SK::TraitTypeItem,
}
impl super::AttrListOwner for TraitTypeItem {}
impl TraitTypeItem {
    /// Returns the name of the associated type
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    pub fn bounds(&self) -> Option<TypeBoundList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `const FOO: Ty` in trait definition
    /// or `const FOO: Ty = expr` in trait implementation
    pub struct TraitConstItem,
    SK::TraitConstItem,
}
impl super::AttrListOwner for TraitConstItem {}
impl TraitConstItem {
    /// Returns the name of the associated const
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the type of the associated const
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the optional default value of the associated const
    pub fn value(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct TraitItem,
    SK::Func | SK::TraitTypeItem | SK::TraitConstItem
}
impl TraitItem {
    pub fn kind(&self) -> TraitItemKind {
        match self.syntax().kind() {
            SK::Func => TraitItemKind::Func(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::TraitTypeItem => {
                TraitItemKind::Type(TraitTypeItem::cast(self.syntax().clone()).unwrap())
            }
            SK::TraitConstItem => {
                TraitItemKind::Const(TraitConstItem::cast(self.syntax().clone()).unwrap())
            }
            _ => unreachable!(),
        }
    }
}

pub enum TraitItemKind {
    Func(Func),
    Type(TraitTypeItem),
    Const(TraitConstItem),
}

ast_node! {
    /// `impl Foo::Bar<T> where .. { .. }`
    pub struct Impl,
    SK::Impl,
}
impl super::GenericParamsOwner for Impl {}
impl super::WhereClauseOwner for Impl {}
impl super::AttrListOwner for Impl {}
impl Impl {
    /// Returns the type of the impl.
    /// `Foo::Bar<T>` in `impl Foo::Bar<T> where .. { .. }`
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the impl item list.
    /// `{ .. }` in `impl Foo::Bar<T> where .. { .. }`
    /// NOTE: Currently only supports `fn` items.
    pub fn item_list(&self) -> Option<ImplItemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `impl<T> Foo for Bar<T> { .. }`
    pub struct ImplTrait,
    SK::ImplTrait,
}
impl super::GenericParamsOwner for ImplTrait {}
impl super::WhereClauseOwner for ImplTrait {}
impl super::AttrListOwner for ImplTrait {}
impl ImplTrait {
    /// Returns the trait of the impl.
    /// `Foo` in `impl<T> Foo for Bar<T> { .. }`
    pub fn trait_ref(&self) -> Option<TraitRef> {
        support::child(self.syntax())
    }

    /// Returns the type of the impl.
    /// `Bar<T>` in `impl<T> Foo for Bar<T> { .. }`
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the trait impl item list.
    /// `{ .. }` in `impl<T> Foo for Bar<T> { .. }`
    pub fn item_list(&self) -> Option<TraitItemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `const FOO: u32 = 42;`
    pub struct Const,
    SK::Const,
}
impl super::AttrListOwner for Const {}
impl ItemModifierOwner for Const {}
impl Const {
    /// Returns the name of the const.
    /// `FOO` in `const FOO: u32 = 42;`
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the type of the const.
    /// `u32` in `const FOO: u32 = 42;`
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the value of the const.
    /// `42` in `const FOO: u32 = 42;`
    pub fn value(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `use foo::{bar, Baz::*}`
    pub struct Use,
    SK::Use,
}
impl super::AttrListOwner for Use {}
impl ItemModifierOwner for Use {}
impl Use {
    /// Returns the use tree.
    /// `foo::{bar, Baz::*}` in `use foo::{bar, Baz::*}`
    pub fn use_tree(&self) -> Option<super::UseTree> {
        support::child(self.syntax())
    }

    pub fn has_sub_tree(&self) -> bool {
        self.use_tree().is_some_and(|it| it.has_subtree())
    }
}

ast_node! {
    /// `extern { .. }`
    pub struct Extern,
    SK::Extern,
}
impl super::AttrListOwner for Extern {}
impl Extern {
    /// Returns the item list.
    /// NOTE: Currently only supports `fn` items.
    pub fn extern_block(&self) -> Option<ExternItemList> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct RecordFieldDefList,
    SK::RecordFieldDefList,
    IntoIterator<Item=RecordFieldDef>
}
ast_node! {
    pub struct RecordFieldDef,
    SK::RecordFieldDef,
}

impl super::AttrListOwner for RecordFieldDef {}
impl RecordFieldDef {
    /// Returns the pub keyword if exists.
    pub fn pub_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::PubKw)
    }

    /// Returns the mut keyword if exists.
    pub fn mut_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::MutKw)
    }

    /// Returns the name of the field.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the type of the field.
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct VariantDefList,
    SK::VariantDefList,
    IntoIterator<Item=VariantDef>
}

ast_node! {
    /// `Foo(i32, u32)`
    pub struct VariantDef,
    SK::VariantDef,
}
impl super::AttrListOwner for VariantDef {}
impl VariantDef {
    /// Returns the name of the variant.
    /// `Foo` in `Foo(i32, u32)`
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the kind of the variant.
    pub fn kind(&self) -> VariantKind {
        support::child(self.syntax())
            .map(VariantKind::Tuple)
            .or_else(|| support::child(self.syntax()).map(VariantKind::Record))
            .unwrap_or(VariantKind::Unit)
    }

    /// Returns the variant's field def list.
    pub fn fields(&self) -> Option<RecordFieldDefList> {
        support::child(self.syntax())
    }

    pub fn tuple_type(&self) -> Option<TupleType> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariantKind {
    Unit,
    Tuple(TupleType),
    Record(RecordFieldDefList),
}

ast_node! {
    pub struct TraitItemList,
    SK::TraitItemList,
    IntoIterator<Item=TraitItem>,
}

ast_node! {
    pub struct ImplItemList,
    SK::ImplItemList,
    IntoIterator<Item=Func>, // TODO: ImplTraitItem
}

ast_node! {
    pub struct ExternItemList,
    SK::ExternItemList,
    IntoIterator<Item=Func>,
}

pub trait ItemModifierOwner: AstNode<Language = FeLang> {
    fn pub_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::PubKw)
    }

    fn unsafe_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::UnsafeKw)
    }
}

ast_node! {
    /// `msg Erc20Msg { ... }`
    pub struct Msg,
    SK::Msg,
}

impl super::AttrListOwner for Msg {}
impl super::ItemModifierOwner for Msg {}
impl Msg {
    /// Returns the name of the message interface.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the message variants.
    pub fn variants(&self) -> Option<MsgVariantList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A list of message variants.
    pub struct MsgVariantList,
    SK::MsgVariantList,
    IntoIterator<Item=MsgVariant>
}

ast_node! {
    /// A single message variant.
    /// `Transfer { to: Address, amount: u256 } -> bool`
    pub struct MsgVariant,
    SK::MsgVariant,
}
impl super::AttrListOwner for MsgVariant {}
impl MsgVariant {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    pub fn params(&self) -> Option<MsgVariantParams> {
        support::child(self.syntax())
    }

    pub fn ret_ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// Message variant parameters.
    /// `{ to: Address, amount: u256 }`
    pub struct MsgVariantParams,
    SK::MsgVariantParams,
    IntoIterator<Item=RecordFieldDef>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum ItemKind {
    Mod(Mod),
    Func(Func),
    Struct(Struct),
    Contract(Contract),
    Msg(Msg),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
    Const(Const),
    Use(Use),
    Extern(Extern),
}

#[cfg(test)]
mod tests {
    use derive_more::TryIntoError;
    use tracing::error;
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;
    use crate::{
        ast::{ExprKind, TypeKind, prelude::*},
        lexer::Lexer,
        parser::{ItemListScope, Parser},
    };

    fn parse_item<T>(source: &str) -> T
    where
        T: TryFrom<ItemKind, Error = TryIntoError<ItemKind>>,
    {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse(ItemListScope::default());
        let (node, errs) = parser.finish_to_node();
        for e in errs {
            error!("{:?}", e);
        }
        let item_list = ItemList::cast(node).unwrap();
        let mut items = item_list.into_iter().collect::<Vec<_>>();
        if items.len() > 1 {
            error!("expected one item, got: {:?}", &items);
        }
        assert_eq!(items.len(), 1);
        items.pop().unwrap().kind().unwrap().try_into().unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn mod_() {
        let source = r"
            pub mod foo {
                pub fn bar() {}
                pub struct Baz
            }
        ";
        let mod_: Mod = parse_item(source);
        assert_eq!(mod_.name().unwrap().text(), "foo");
        let mut i = 0;
        for item in mod_.items().unwrap().into_iter() {
            match i {
                0 => {
                    assert!(matches!(item.kind().unwrap(), ItemKind::Func(_)));
                    let func: Func = item.kind().unwrap().try_into().unwrap();
                    assert_eq!(func.sig().name().unwrap().text(), "bar");
                }
                1 => {
                    assert!(matches!(item.kind().unwrap(), ItemKind::Struct(_)));
                    let struct_: Struct = item.kind().unwrap().try_into().unwrap();
                    assert_eq!(struct_.name().unwrap().text(), "Baz");
                }
                _ => panic!(),
            }
            i += 1;
        }

        assert_eq!(i, 2);
    }

    #[test]
    #[wasm_bindgen_test]
    fn func() {
        let source = r#"
                /// This is doc comment
                #[evm]
                pub unsafe fn foo<T, U: Trait>(_ x: T, from u: U) -> (T, U) where T: Trait2 { return }
            "#;
        let func: Func = parse_item(source);

        assert_eq!(func.sig().name().unwrap().text(), "foo");
        assert_eq!(func.attr_list().unwrap().iter().count(), 2);
        assert_eq!(func.sig().generic_params().unwrap().iter().count(), 2);
        assert!(func.sig().where_clause().is_some());
        assert!(func.body().is_some());
        assert!(matches!(
            func.sig().ret_ty().unwrap().kind(),
            TypeKind::Tuple(_)
        ));
        assert!(func.pub_kw().is_some());
        assert!(func.unsafe_kw().is_some());
    }

    #[test]
    #[wasm_bindgen_test]
    fn struct_() {
        let source = r#"
                pub struct Foo<T, U: Trait> where T: Trait2 {
                    pub x: T,
                    y: (U, i32),
                }
            "#;
        let s: Struct = parse_item(source);
        assert_eq!(s.name().unwrap().text(), "Foo");
        let mut count = 0;
        for field in s.fields().unwrap() {
            match count {
                0 => {
                    assert!(field.pub_kw().is_some());
                    assert_eq!(field.name().unwrap().text(), "x");
                    assert!(matches!(field.ty().unwrap().kind(), TypeKind::Path(_)));
                }
                1 => {
                    assert!(field.pub_kw().is_none());
                    assert_eq!(field.name().unwrap().text(), "y");
                    assert!(matches!(field.ty().unwrap().kind(), TypeKind::Tuple(_)));
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 2);
    }

    #[test]
    #[wasm_bindgen_test]
    fn contract() {
        let source = r#"
                pub contract Foo {
                    pub x: u32,
                    y: (i32, u32),
                }
            "#;
        let c: Contract = parse_item(source);
        assert_eq!(c.name().unwrap().text(), "Foo");
        let mut count = 0;
        for field in c.fields().unwrap() {
            match count {
                0 => {
                    assert!(field.pub_kw().is_some());
                    assert_eq!(field.name().unwrap().text(), "x");
                    assert!(matches!(field.ty().unwrap().kind(), TypeKind::Path(_)));
                }
                1 => {
                    assert!(field.pub_kw().is_none());
                    assert_eq!(field.name().unwrap().text(), "y");
                    assert!(matches!(field.ty().unwrap().kind(), TypeKind::Tuple(_)));
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 2);
    }

    #[test]
    #[wasm_bindgen_test]
    fn enum_() {
        let source = r#"
                pub enum Foo<T, U: Trait> where T: Trait2 {
                    Bar
                    Baz(T, U)
                    Bux {
                        x: i8
                        y: i8
                    }
                }
            "#;
        let e: Enum = parse_item(source);
        assert_eq!(e.name().unwrap().text(), "Foo");

        let mut count = 0;
        for variant in e.variants().unwrap() {
            match count {
                0 => {
                    assert_eq!(variant.name().unwrap().text(), "Bar");
                    assert_eq!(variant.kind(), VariantKind::Unit);
                }
                1 => {
                    assert_eq!(variant.name().unwrap().text(), "Baz");
                    assert!(matches!(variant.kind(), VariantKind::Tuple(_)));
                }
                2 => {
                    assert_eq!(variant.name().unwrap().text(), "Bux");
                    assert!(matches!(variant.kind(), VariantKind::Record(_)));
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 3);
    }

    #[test]
    #[wasm_bindgen_test]
    fn type_() {
        let source = r#"
                type MyError<T> where T: Debug = Error<T, String>
            "#;
        let t: TypeAlias = parse_item(source);
        assert_eq!(t.alias().unwrap().text(), "MyError");
        assert!(matches!(t.ty().unwrap().kind(), TypeKind::Path(_)));
    }

    #[test]
    #[wasm_bindgen_test]
    fn impl_() {
        let source = r#"
                impl Foo {
                    pub fn foo<T>(self, t: T) -> T { return t }
                    pub fn bar(self) -> u32 { return 1 }
                    pub fn baz(mut self) { self.x = 1 }
                }
            "#;
        let i: Impl = parse_item(source);
        assert!(matches!(i.ty().unwrap().kind(), TypeKind::Path(_)));
        assert_eq!(i.item_list().unwrap().iter().count(), 3);
    }

    #[test]
    #[wasm_bindgen_test]
    fn trait_() {
        let source = r#"
                pub trait Foo {
                    type Bar: Encode
                    pub fn foo<T>(self, t: T) -> T
                    pub fn default(self) -> u32 { return 1 }
                }
            "#;
        let t: Trait = parse_item(source);
        assert_eq!(t.name().unwrap().text(), "Foo");

        let mut count = 0;
        for item in t.item_list().unwrap() {
            match count {
                0 => {
                    let TraitItemKind::Type(t) = item.kind() else {
                        panic!()
                    };
                    assert_eq!(t.name().unwrap().text(), "Bar");
                    assert_eq!(
                        t.bounds()
                            .unwrap()
                            .into_iter()
                            .next()
                            .unwrap()
                            .trait_bound()
                            .unwrap()
                            .path()
                            .unwrap()
                            .segments()
                            .next()
                            .unwrap()
                            .ident()
                            .unwrap()
                            .text(),
                        "Encode"
                    );
                }
                1 => {
                    let TraitItemKind::Func(func) = item.kind() else {
                        panic!()
                    };
                    assert!(func.body().is_none());
                }
                2 => {
                    let TraitItemKind::Func(func) = item.kind() else {
                        panic!()
                    };
                    assert!(func.body().is_some());
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 3);
    }

    #[test]
    #[wasm_bindgen_test]
    fn impl_trait() {
        let source = r#"
            impl Trait::Foo for (i32)  {
                type Foo = u32
                fn foo<T>(self, _t: T) -> u32 { return 1 };
            }"#;
        let i: ImplTrait = parse_item(source);
        assert!(i.generic_params().is_none());
        assert!(i.trait_ref().is_some());
        assert!(matches!(i.ty().unwrap().kind(), TypeKind::Tuple(_)));
        let mut items = i.item_list().unwrap().iter();
        assert!(matches!(
            items.next().unwrap().kind(),
            TraitItemKind::Type(_)
        ));
        assert!(matches!(
            items.next().unwrap().kind(),
            TraitItemKind::Func(_)
        ));
        assert_eq!(items.next(), None);
    }

    #[test]
    #[wasm_bindgen_test]
    fn const_() {
        let source = r#"
            pub const FOO: u32 = 1 + 1
        "#;
        let c: Const = parse_item(source);
        assert_eq!(c.name().unwrap().text(), "FOO");
        assert!(matches!(c.ty().unwrap().kind(), TypeKind::Path(_)));
        assert!(matches!(c.value().unwrap().kind(), ExprKind::Bin(_)));
    }

    #[test]
    #[wasm_bindgen_test]
    fn use_() {
        let source = r#"
            use foo::bar::{bar::*, baz::{Baz, Baz2}}
        "#;
        let u: Use = parse_item(source);
        let use_tree = u.use_tree().unwrap();
        let mut count = 0;
        for segment in use_tree.path().unwrap() {
            match count {
                0 => {
                    assert_eq!(segment.ident().unwrap().text(), "foo");
                }
                1 => {
                    assert_eq!(segment.ident().unwrap().text(), "bar");
                }
                _ => unreachable!(),
            }
            count += 1;
        }

        count = 0;
        let children = use_tree.children().unwrap();
        for child in children {
            match count {
                0 => {
                    let mut segments = child.path().unwrap().iter();
                    assert_eq!(segments.next().unwrap().ident().unwrap().text(), "bar");
                    assert!(segments.next().unwrap().glob().is_some());
                    assert!(segments.next().is_none());
                    assert!(child.children().is_none());
                }
                1 => {
                    let mut segments = child.path().unwrap().iter();
                    assert_eq!(segments.next().unwrap().ident().unwrap().text(), "baz");
                    assert!(child.children().unwrap().iter().count() == 2);
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 2);

        let source = r#"
            use {foo as _foo, bar::Baz as _}
        "#;
        let u: Use = parse_item(source);
        let use_tree = u.use_tree().unwrap();
        assert!(use_tree.path().is_none());
        let mut count = 0;
        for child in use_tree.children().unwrap() {
            match count {
                0 => {
                    let alias = child.alias().unwrap();
                    assert_eq!(alias.ident().unwrap().text(), "_foo");
                }
                1 => {
                    let alias = child.alias().unwrap();
                    assert!(alias.underscore().is_some());
                }
                _ => unreachable!(),
            }
            count += 1;
        }
        assert_eq!(count, 2);
    }

    #[test]
    #[wasm_bindgen_test]
    fn extern_() {
        let source = r#"
            extern {
                pub unsafe fn foo()
                pub unsafe fn bar()
            }"#;
        let e: Extern = parse_item(source);

        for f in e.extern_block().unwrap() {
            assert!(f.body().is_none());
        }
        assert_eq!(e.extern_block().unwrap().iter().count(), 2);
    }
    #[test]
    #[wasm_bindgen_test]
    fn msg_() {
        let source = r#"
            msg Erc20Msg {
                Transfer { to: Address, amount: u256 } -> bool,
                Balance { addr: Address } -> u256,
                TotalSupply -> u256,
            }
        "#;
        let m: Msg = parse_item(source);
        assert_eq!(m.name().unwrap().text(), "Erc20Msg");

        let mut count = 0;
        for variant in m.variants().unwrap() {
            match count {
                0 => {
                    assert_eq!(variant.name().unwrap().text(), "Transfer");
                    assert!(variant.params().is_some());
                    assert!(variant.ret_ty().is_some());
                    assert_eq!(variant.params().unwrap().into_iter().count(), 2);
                }
                1 => {
                    assert_eq!(variant.name().unwrap().text(), "Balance");
                    assert!(variant.params().is_some());
                    assert!(variant.ret_ty().is_some());
                    assert_eq!(variant.params().unwrap().into_iter().count(), 1);
                }
                2 => {
                    assert_eq!(variant.name().unwrap().text(), "TotalSupply");
                    assert!(variant.params().is_none());
                    assert!(variant.ret_ty().is_some());
                }
                _ => panic!("unexpected variant"),
            }
            count += 1;
        }
        assert_eq!(count, 3);
    }
}
