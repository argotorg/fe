// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use std::borrow::Cow;

use common::{file::File, ingot::Ingot};
use parser::ast;

use super::{
    AttrListId, Body, CompBinOp, EffectParamListId, FuncParamListId, FuncParamName,
    GenericParamListId, HirIngot, IdentId, InlineAttr, InlineAttrErrorKind, InlineHint,
    ManualContractRootAttr, Partial, Pat, PatId, TupleTypeId, TypeBound, TypeId, UseAlias,
    WhereClauseId,
    scope_graph::{ScopeGraph, ScopeId},
};
use crate::{
    HirDb,
    hir_def::{PathId, TraitRefId},
    lower,
    span::{
        DynLazySpan, HirOrigin,
        item::{
            LazyConstSpan, LazyContractSpan, LazyDeriveDeclSpan, LazyDeriveProviderScopeSpan,
            LazyEnumSpan, LazyFuncSpan, LazyImplSpan, LazyImplTraitSpan, LazyItemSpan, LazyModSpan,
            LazyStaticAssertSpan, LazyStructSpan, LazyTopModSpan, LazyTraitSpan, LazyTraitTypeSpan,
            LazyTypeAliasSpan, LazyUseSpan, LazyVariantDefSpan,
        },
        params::LazyGenericParamListSpan,
    },
};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    derive_more::From,
    derive_more::TryInto,
    salsa::Update,
)]
pub enum ItemKind<'db> {
    TopMod(TopLevelMod<'db>),
    Mod(Mod<'db>),
    Func(Func<'db>),
    Struct(Struct<'db>),
    Contract(Contract<'db>),
    Enum(Enum<'db>),
    TypeAlias(TypeAlias<'db>),
    Impl(Impl<'db>),
    Trait(Trait<'db>),
    ImplTrait(ImplTrait<'db>),
    DeriveProviderScope(DeriveProviderScope<'db>),
    DeriveDecl(DeriveDecl<'db>),
    Const(Const<'db>),
    StaticAssert(StaticAssert<'db>),
    Use(Use<'db>),
    /// Body is not an `Item`, but this makes it easier for analyzers to handle
    /// it.
    Body(Body<'db>),
}

impl<'db> ItemKind<'db> {
    pub fn span(self) -> LazyItemSpan<'db> {
        LazyItemSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self)
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        use ItemKind::*;
        match self {
            TopMod(top_mod) => Some(top_mod.name(db)),
            Mod(mod_) => mod_.name(db).to_opt(),
            Func(func_) => func_.name(db).to_opt(),
            Struct(struct_) => struct_.name(db).to_opt(),
            Contract(contract_) => contract_.name(db).to_opt(),
            Enum(enum_) => enum_.name(db).to_opt(),
            TypeAlias(alias) => alias.name(db).to_opt(),
            Trait(trait_) => trait_.name(db).to_opt(),
            Const(const_) => const_.name(db).to_opt(),
            Use(_)
            | StaticAssert(_)
            | Body(_)
            | Impl(_)
            | ImplTrait(_)
            | DeriveProviderScope(_)
            | DeriveDecl(_) => None,
        }
    }

    /// Returns attributes being applied to this item.
    pub fn attrs(self, db: &'db dyn HirDb) -> Option<AttrListId<'db>> {
        match self {
            Self::TopMod(top_mod) => top_mod.attributes(db),
            Self::Mod(mod_) => mod_.attributes(db),
            Self::Func(func) => func.attributes(db),
            Self::Struct(struct_) => struct_.attributes(db),
            Self::Contract(contract) => contract.attributes(db),
            Self::Enum(enum_) => enum_.attributes(db),
            Self::TypeAlias(alias) => alias.attributes(db),
            Self::Impl(impl_) => impl_.attributes(db),
            Self::Trait(trait_) => trait_.attributes(db),
            Self::ImplTrait(impl_trait) => impl_trait.attributes(db),
            Self::DeriveProviderScope(scope) => scope.attributes(db),
            Self::DeriveDecl(decl) => decl.attributes(db),
            Self::Const(const_) => const_.attributes(db),
            Self::StaticAssert(assert_) => assert_.attributes(db),
            Self::Use(use_) => use_.attributes(db),
            _ => return None,
        }
        .into()
    }

    pub fn kind_name(self) -> &'static str {
        use ItemKind::*;
        match self {
            TopMod(_) => "mod",
            Mod(_) => "mod",
            Func(_) => "fn",
            Struct(_) => "struct",
            Contract(_) => "contract",
            Enum(_) => "enum",
            TypeAlias(_) => "type",
            Trait(_) => "trait",
            Impl(_) => "impl",
            ImplTrait(_) => "impl trait",
            DeriveProviderScope(_) => "derive provider selection scope",
            DeriveDecl(_) => "derive",
            Const(_) => "const",
            StaticAssert(_) => "static_assert",
            Use(_) => "use",
            Body(_) => "body",
        }
    }

    pub fn name_span(self) -> Option<DynLazySpan<'db>> {
        use ItemKind::*;
        match self {
            Mod(mod_) => Some(mod_.span().name().into()),
            Func(func_) => Some(func_.span().name().into()),
            Struct(struct_) => Some(struct_.span().name().into()),
            Contract(contract_) => Some(contract_.span().name().into()),
            Enum(enum_) => Some(enum_.span().name().into()),
            TypeAlias(alias) => Some(alias.span().alias().into()),
            Trait(trait_) => Some(trait_.span().name().into()),
            Const(const_) => Some(const_.span().name().into()),
            TopMod(_)
            | StaticAssert(_)
            | Use(_)
            | Body(_)
            | Impl(_)
            | ImplTrait(_)
            | DeriveProviderScope(_)
            | DeriveDecl(_) => None,
        }
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        use ItemKind::*;
        match self {
            TopMod(top_mod) => top_mod.vis(db),
            Mod(mod_) => mod_.vis(db),
            Func(func) => func.vis(db),
            Struct(struct_) => struct_.vis(db),
            Contract(contract) => contract.vis(db),
            Enum(enum_) => enum_.vis(db),
            TypeAlias(type_) => type_.vis(db),
            Trait(trait_) => trait_.vis(db),
            Const(const_) => const_.vis(db),
            Use(use_) => use_.vis(db),
            StaticAssert(_)
            | Impl(_)
            | ImplTrait(_)
            | DeriveProviderScope(_)
            | DeriveDecl(_)
            | Body(_) => Visibility::Private,
        }
    }

    pub fn top_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        match self {
            ItemKind::TopMod(top_mod) => top_mod,
            ItemKind::Mod(mod_) => mod_.top_mod(db),
            ItemKind::Func(func) => func.top_mod(db),
            ItemKind::Struct(struct_) => struct_.top_mod(db),
            ItemKind::Contract(contract) => contract.top_mod(db),
            ItemKind::Enum(enum_) => enum_.top_mod(db),
            ItemKind::TypeAlias(type_) => type_.top_mod(db),
            ItemKind::Trait(trait_) => trait_.top_mod(db),
            ItemKind::Impl(impl_) => impl_.top_mod(db),
            ItemKind::ImplTrait(impl_trait) => impl_trait.top_mod(db),
            ItemKind::DeriveProviderScope(scope) => scope.top_mod(db),
            ItemKind::DeriveDecl(decl) => decl.top_mod(db),
            ItemKind::Const(const_) => const_.top_mod(db),
            ItemKind::StaticAssert(assert_) => assert_.top_mod(db),
            ItemKind::Use(use_) => use_.top_mod(db),
            ItemKind::Body(body) => body.top_mod(db),
        }
    }

    pub fn is_type(self) -> bool {
        matches!(
            self,
            Self::Struct(_) | Self::Enum(_) | Self::Contract(_) | Self::TypeAlias(_)
        )
    }

    pub fn is_trait(self) -> bool {
        matches!(self, Self::Trait(_))
    }
}

impl<'db> From<GenericParamOwner<'db>> for ItemKind<'db> {
    fn from(owner: GenericParamOwner<'db>) -> Self {
        match owner {
            GenericParamOwner::Func(func) => ItemKind::Func(func),
            GenericParamOwner::Struct(struct_) => ItemKind::Struct(struct_),
            GenericParamOwner::Enum(enum_) => ItemKind::Enum(enum_),
            GenericParamOwner::TypeAlias(type_alias) => ItemKind::TypeAlias(type_alias),
            GenericParamOwner::Impl(impl_) => ItemKind::Impl(impl_),
            GenericParamOwner::Trait(trait_) => ItemKind::Trait(trait_),
            GenericParamOwner::ImplTrait(impl_trait) => ItemKind::ImplTrait(impl_trait),
        }
    }
}

impl<'db> From<WhereClauseOwner<'db>> for ItemKind<'db> {
    fn from(owner: WhereClauseOwner<'db>) -> Self {
        match owner {
            WhereClauseOwner::Func(func) => ItemKind::Func(func),
            WhereClauseOwner::Struct(struct_) => ItemKind::Struct(struct_),
            WhereClauseOwner::Enum(enum_) => ItemKind::Enum(enum_),
            WhereClauseOwner::Impl(impl_) => ItemKind::Impl(impl_),
            WhereClauseOwner::Trait(trait_) => ItemKind::Trait(trait_),
            WhereClauseOwner::ImplTrait(impl_trait) => ItemKind::ImplTrait(impl_trait),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    derive_more::From,
    salsa::Supertype,
    salsa::Update,
)]
pub enum GenericParamOwner<'db> {
    Func(Func<'db>),
    Struct(Struct<'db>),
    Enum(Enum<'db>),
    TypeAlias(TypeAlias<'db>),
    Impl(Impl<'db>),
    Trait(Trait<'db>),
    ImplTrait(ImplTrait<'db>),
}

impl<'db> GenericParamOwner<'db> {
    pub fn top_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        ItemKind::from(self).top_mod(db)
    }

    pub(in crate::core) fn params_list(self, db: &'db dyn HirDb) -> GenericParamListId<'db> {
        match self {
            GenericParamOwner::Func(func) => func.generic_params(db),
            GenericParamOwner::Struct(struct_) => struct_.generic_params(db),
            GenericParamOwner::Enum(enum_) => enum_.generic_params(db),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.generic_params(db),
            GenericParamOwner::Impl(impl_) => impl_.generic_params(db),
            GenericParamOwner::Trait(trait_) => trait_.generic_params(db),
            GenericParamOwner::ImplTrait(impl_trait) => impl_trait.generic_params(db),
        }
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        match self {
            GenericParamOwner::Func(func) => func.name(db).to_opt(),
            GenericParamOwner::Struct(struct_) => struct_.name(db).to_opt(),
            GenericParamOwner::Enum(enum_) => enum_.name(db).to_opt(),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.name(db).to_opt(),
            GenericParamOwner::Impl(_) => None,
            GenericParamOwner::Trait(trait_) => trait_.name(db).to_opt(),
            GenericParamOwner::ImplTrait(_) => None,
        }
    }

    pub fn kind_name(self) -> &'static str {
        match self {
            GenericParamOwner::Func(_) => "fn",
            GenericParamOwner::Struct(_) => "struct",
            GenericParamOwner::Enum(_) => "enum",
            GenericParamOwner::TypeAlias(_) => "type",
            GenericParamOwner::Impl(_) => "impl",
            GenericParamOwner::Trait(_) => "trait",
            GenericParamOwner::ImplTrait(_) => "impl trait",
        }
    }

    pub(in crate::core) fn params_span(self) -> LazyGenericParamListSpan<'db> {
        match self {
            GenericParamOwner::Func(func) => func.span().generic_params(),
            GenericParamOwner::Struct(struct_) => struct_.span().generic_params(),
            GenericParamOwner::Enum(enum_) => enum_.span().generic_params(),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.span().generic_params(),
            GenericParamOwner::Impl(impl_) => impl_.span().generic_params(),
            GenericParamOwner::Trait(trait_) => trait_.span().generic_params(),
            GenericParamOwner::ImplTrait(impl_trait) => impl_trait.span().generic_params(),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        ItemKind::from(self).scope()
    }

    pub fn from_item_opt(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Func(func) => Some(GenericParamOwner::Func(func)),
            ItemKind::Struct(struct_) => Some(GenericParamOwner::Struct(struct_)),
            ItemKind::Enum(enum_) => Some(GenericParamOwner::Enum(enum_)),
            ItemKind::TypeAlias(type_alias) => Some(GenericParamOwner::TypeAlias(type_alias)),
            ItemKind::Impl(impl_) => Some(GenericParamOwner::Impl(impl_)),
            ItemKind::Trait(trait_) => Some(GenericParamOwner::Trait(trait_)),
            ItemKind::ImplTrait(impl_trait) => Some(GenericParamOwner::ImplTrait(impl_trait)),
            _ => None,
        }
    }

    pub fn parent(self, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::Item(item) = self.scope().parent(db)? else {
            return None;
        };

        match item {
            ItemKind::Func(func) => Some(GenericParamOwner::Func(func)),
            ItemKind::Struct(struct_) => Some(GenericParamOwner::Struct(struct_)),
            ItemKind::Enum(enum_) => Some(GenericParamOwner::Enum(enum_)),
            ItemKind::TypeAlias(type_alias) => Some(GenericParamOwner::TypeAlias(type_alias)),
            ItemKind::Impl(impl_) => Some(GenericParamOwner::Impl(impl_)),
            ItemKind::Trait(trait_) => Some(GenericParamOwner::Trait(trait_)),
            ItemKind::ImplTrait(impl_trait) => Some(GenericParamOwner::ImplTrait(impl_trait)),
            _ => None,
        }
    }

    pub fn where_clause_owner(self) -> Option<WhereClauseOwner<'db>> {
        let item = ItemKind::from(self);
        WhereClauseOwner::from_item_opt(item)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Update)]
pub enum CallableDef<'db> {
    Func(Func<'db>),
    VariantCtor(EnumVariant<'db>),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::From, salsa::Update,
)]
pub enum WhereClauseOwner<'db> {
    Func(Func<'db>),
    Struct(Struct<'db>),
    Enum(Enum<'db>),
    Impl(Impl<'db>),
    Trait(Trait<'db>),
    ImplTrait(ImplTrait<'db>),
}

impl<'db> WhereClauseOwner<'db> {
    pub fn top_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        ItemKind::from(self).top_mod(db)
    }

    pub fn where_clause(self, db: &'db dyn HirDb) -> WhereClauseId<'db> {
        match self {
            Self::Func(func) => func.where_clause(db),
            Self::Struct(struct_) => struct_.where_clause(db),
            Self::Enum(enum_) => enum_.where_clause(db),
            Self::Impl(impl_) => impl_.where_clause(db),
            Self::Trait(trait_) => trait_.where_clause(db),
            Self::ImplTrait(impl_trait) => impl_trait.where_clause(db),
        }
    }

    // pub(in crate::core) fn where_clause_span(self) -> LazyWhereClauseSpan<'db> {
    //     match self {
    //         Self::Func(func) => func.span().where_clause(),
    //         Self::Struct(struct_) => struct_.span().where_clause(),
    //         Self::Enum(enum_) => enum_.span().where_clause(),
    //         Self::Impl(impl_) => impl_.span().where_clause(),
    //         Self::Trait(trait_) => trait_.span().where_clause(),
    //         Self::ImplTrait(impl_trait) => impl_trait.span().where_clause(),
    //     }
    // }

    pub fn scope(self) -> ScopeId<'db> {
        ItemKind::from(self).scope()
    }

    pub fn from_item_opt(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Func(func) => Some(Self::Func(func)),
            ItemKind::Struct(struct_) => Some(Self::Struct(struct_)),
            ItemKind::Enum(enum_) => Some(Self::Enum(enum_)),
            ItemKind::Impl(impl_) => Some(Self::Impl(impl_)),
            ItemKind::Trait(trait_) => Some(Self::Trait(trait_)),
            ItemKind::ImplTrait(impl_trait) => Some(Self::ImplTrait(impl_trait)),
            _ => None,
        }
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct TopLevelMod<'db> {
    // No #[id] here, because `TopLevelMod` is always unique to a `InputFile` that is an argument
    // of `module_scope_graph`.
    pub name: IdentId<'db>,
    pub(crate) file: File,
}

#[salsa::tracked]
impl<'db> TopLevelMod<'db> {
    pub fn span(self) -> LazyTopModSpan<'db> {
        LazyTopModSpan::new(self)
    }
    pub fn ingot(self, db: &'db dyn HirDb) -> Ingot<'db> {
        self.file(db)
            .containing_ingot(db)
            .expect("top level mod should have an ingot")
    }

    pub fn lazy_span(self) -> LazyTopModSpan<'db> {
        LazyTopModSpan::new(self)
    }

    pub fn attributes(self, db: &'db dyn HirDb) -> AttrListId<'db> {
        lower::top_mod_attributes_impl(db, self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn scope_graph(self, db: &'db dyn HirDb) -> &'db ScopeGraph<'db> {
        lower::scope_graph_impl(db, self)
    }

    /// Returns the child top level modules of `self`.
    pub fn child_top_mods(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = TopLevelMod<'db>> + 'db {
        let module_tree = self.ingot(db).module_tree(db);
        // Error is logged by tree_node() if module not found
        module_tree.children(self)
    }

    /// Returns the top level children of this module.
    /// If you need all the children, use
    /// [`children_nested`](Self::children_nested) instead.
    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let s_graph = self.scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    /// Returns all the children of this module, including nested items.
    pub fn children_nested(self, db: &'db dyn HirDb) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let s_graph = self.scope_graph(db);
        s_graph.items_dfs(db)
    }

    pub fn parent(self, db: &'db dyn HirDb) -> Option<TopLevelMod<'db>> {
        let module_tree = self.ingot(db).module_tree(db);
        // Error is logged by tree_node() if module not found
        module_tree.parent(self)
    }

    pub fn vis(self, _db: &dyn HirDb) -> Visibility {
        // We don't have a way to specify visibility of a top level module.
        // Please change here if we introduce it.
        Visibility::Public
    }

    /// Returns all items in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_items(self, db: &'db dyn HirDb) -> Vec<ItemKind<'db>> {
        self.children_nested(db).collect()
    }

    /// Returns all structs in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_structs(self, db: &'db dyn HirDb) -> Vec<Struct<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Struct(struct_) => Some(*struct_),
                _ => None,
            })
            .collect()
    }

    /// Returns all enums in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_enums(self, db: &'db dyn HirDb) -> Vec<Enum<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Enum(enum_) => Some(*enum_),
                _ => None,
            })
            .collect()
    }

    /// Returns all contracts in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_contracts(self, db: &'db dyn HirDb) -> Vec<Contract<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Contract(contract) => Some(*contract),
                _ => None,
            })
            .collect()
    }

    /// Returns all type aliases in the top level module including ones in
    /// nested modules.
    #[salsa::tracked(return_ref)]
    pub fn all_type_aliases(self, db: &'db dyn HirDb) -> Vec<TypeAlias<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::TypeAlias(alias) => Some(*alias),
                _ => None,
            })
            .collect()
    }

    /// Returns all traits in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_traits(self, db: &'db dyn HirDb) -> Vec<Trait<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Trait(trait_) => Some(*trait_),
                _ => None,
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn all_funcs(self, db: &'db dyn HirDb) -> Vec<Func<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Func(func_) => Some(*func_),
                _ => None,
            })
            .collect()
    }

    /// Returns all traits in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_impl_traits(self, db: &'db dyn HirDb) -> Vec<ImplTrait<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::ImplTrait(impl_trait) => Some(*impl_trait),
                _ => None,
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn all_derive_provider_scopes(self, db: &'db dyn HirDb) -> Vec<DeriveProviderScope<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::DeriveProviderScope(scope) => Some(*scope),
                _ => None,
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn all_derive_decls(self, db: &'db dyn HirDb) -> Vec<DeriveDecl<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::DeriveDecl(decl) => Some(*decl),
                _ => None,
            })
            .collect()
    }

    /// Returns all impls in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_impls(self, db: &'db dyn HirDb) -> Vec<Impl<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Impl(impl_) => Some(*impl_),
                _ => None,
            })
            .collect()
    }

    /// Returns all mods in the top level module including ones in nested
    /// modules.
    #[salsa::tracked(return_ref)]
    pub fn all_mods(self, db: &'db dyn HirDb) -> Vec<Mod<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::Mod(mod_) => Some(*mod_),
                _ => None,
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn all_static_asserts(self, db: &'db dyn HirDb) -> Vec<StaticAssert<'db>> {
        self.all_items(db)
            .iter()
            .filter_map(|item| match item {
                ItemKind::StaticAssert(assert_) => Some(*assert_),
                _ => None,
            })
            .collect()
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Mod<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,

    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Mod>,
}
impl<'db> Mod<'db> {
    pub fn span(self) -> LazyModSpan<'db> {
        LazyModSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct FuncModifiers {
    pub vis: Visibility,
    pub is_unsafe: bool,
    pub is_const: bool,
    pub is_extern: bool,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Func<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    pub(in crate::core) params_list: Partial<FuncParamListId<'db>>,
    pub(crate) effects: EffectParamListId<'db>,
    pub(in crate::core) ret_type_ref: Option<TypeId<'db>>,
    pub(in crate::core) modifiers: FuncModifiers,
    pub body: Option<Body<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub origin: HirOrigin<ast::Func>,
}

impl<'db> Func<'db> {
    pub fn span(self) -> LazyFuncSpan<'db> {
        LazyFuncSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        self.modifiers(db).vis
    }

    pub fn is_unsafe(self, db: &dyn HirDb) -> bool {
        self.modifiers(db).is_unsafe
    }

    pub fn is_const(self, db: &dyn HirDb) -> bool {
        self.modifiers(db).is_const
    }

    pub fn is_must_use(self, db: &'db dyn HirDb) -> bool {
        self.attributes(db).is_must_use(db)
    }

    fn inline_attr(self, db: &'db dyn HirDb) -> Option<InlineAttr> {
        self.attributes(db).inline_attr(db)
    }

    pub fn inline_hint(self, db: &'db dyn HirDb) -> Option<InlineHint> {
        match self.inline_attr(db) {
            Some(InlineAttr::Hint(hint)) => Some(hint),
            Some(InlineAttr::Error(_)) | None => None,
        }
    }

    pub fn inline_attr_error(self, db: &'db dyn HirDb) -> Option<InlineAttrErrorKind> {
        match self.inline_attr(db) {
            Some(InlineAttr::Error(kind)) => Some(kind),
            Some(InlineAttr::Hint(_)) | None => None,
        }
    }

    pub fn manual_contract_root_attr(
        self,
        db: &'db dyn HirDb,
    ) -> Option<ManualContractRootAttr<'db>> {
        let attr = self.attributes(db).manual_contract_root_attr(db)?;
        if matches!(attr, ManualContractRootAttr::Error(_)) {
            return Some(attr);
        }
        if self.is_method(db)
            || self.is_associated_func(db)
            || self.is_extern(db)
            || self.params(db).next().is_some()
            || !self.generic_params(db).data(db).is_empty()
        {
            return Some(ManualContractRootAttr::Error(
                crate::hir_def::ManualContractRootAttrError::WrongTarget,
            ));
        }
        Some(attr)
    }

    pub fn is_extern(self, db: &dyn HirDb) -> bool {
        self.modifiers(db).is_extern
    }

    /// Whether this function is the `derive` function of a derive provider.
    /// Provider bodies are written in the compile-time command language and are
    /// checked by the provider executor in the expansion stage, so the ordinary
    /// signature/body analyses skip them.
    ///
    /// A derive provider is the ordinary `impl Derive<Goal> for Provider {
    /// const fn derive .. }`, whose parent is an [`ItemKind::ImplTrait`] whose
    /// trait ref resolves — base-graph-safely, by the same `core::derive::Derive`
    /// identity rule the provider engine uses — to the `Derive` provider trait.
    pub fn is_derive_provider_fn(self, db: &dyn HirDb) -> bool {
        match self.scope().parent_item(db) {
            Some(ItemKind::ImplTrait(impl_trait)) => {
                crate::lower::impl_trait_provider_goal_path(db, impl_trait).is_some()
            }
            _ => false,
        }
    }

    pub fn is_method(self, db: &dyn HirDb) -> bool {
        let Some(params) = self.params_list(db).to_opt() else {
            return false;
        };

        let Some(first_param) = params.data(db).first() else {
            return false;
        };

        first_param.is_self_param(db)
    }

    /// Returns `true` if the function is method or associated functions.
    pub fn is_associated_func(self, db: &dyn HirDb) -> bool {
        let item = match self.scope().parent(db) {
            Some(ScopeId::Item(item)) => item,
            _ => return false,
        };

        matches!(
            item,
            ItemKind::Trait(_) | ItemKind::Impl(_) | ItemKind::ImplTrait(_)
        )
    }

    pub fn param_label(self, db: &'db dyn HirDb, idx: usize) -> Option<IdentId<'db>> {
        self.params_list(db)
            .to_opt()?
            .data(db)
            .get(idx)?
            .label_eagerly()
    }

    pub fn param_label_or_name(self, db: &'db dyn HirDb, idx: usize) -> Option<FuncParamName<'db>> {
        let param = self.params_list(db).to_opt()?.data(db).get(idx)?;
        if param.is_label_suppressed {
            Some(FuncParamName::Underscore)
        } else {
            param.name.to_opt()
        }
    }

    /// View as a callable definition (if named).
    pub fn as_callable(self, db: &'db dyn HirDb) -> Option<CallableDef<'db>> {
        self.name(db).to_opt()?;
        Some(CallableDef::Func(self))
    }

    // Function semantic helpers live in `crate::semantic`.
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Struct<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    pub(in crate::core) fields: FieldDefListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Struct>,
}

impl<'db> Struct<'db> {
    pub fn span(self) -> LazyStructSpan<'db> {
        LazyStructSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn hir_fields(self, db: &'db dyn HirDb) -> FieldDefListId<'db> {
        self.fields(db)
    }

    pub fn is_must_use(self, db: &'db dyn HirDb) -> bool {
        self.attributes(db).is_must_use(db)
    }

    /// Returns the human readable string of the expected struct initializer.
    /// ## Example
    /// When `S` is a struct defined as below:
    /// ```fe
    /// struct S {
    ///    x: u64,
    ///    y: i32,
    /// }
    /// ```
    /// Then this method returns ` { x, y }`.
    pub fn format_initializer_args(self, db: &dyn HirDb) -> String {
        self.fields(db).format_initializer_args(db)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Contract<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub(in crate::core) hir_fields: FieldDefListId<'db>,
    /// `uses` clause attached to the contract header
    pub effects: EffectParamListId<'db>,
    /// Optional init block owned by the contract.
    pub init: Option<ContractInit<'db>>,
    /// Receive handlers declared in the contract
    pub recvs: ContractRecvListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Contract>,
}

impl<'db> Contract<'db> {
    pub fn span(self) -> LazyContractSpan<'db> {
        LazyContractSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn recv_arm(
        self,
        db: &'db dyn HirDb,
        recv_idx: usize,
        arm_idx: usize,
    ) -> Option<ContractRecvArm<'db>> {
        self.recvs(db)
            .data(db)
            .get(recv_idx)?
            .arms
            .data(db)
            .get(arm_idx)
            .copied()
    }
}

/// Contract-owned init block.
#[salsa::tracked]
#[derive(Debug)]
pub struct ContractInit<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub attributes: AttrListId<'db>,
    pub params: FuncParamListId<'db>,
    pub effects: EffectParamListId<'db>,
    pub body: Body<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::ContractInit>,
}

impl<'db> ContractInit<'db> {
    /// Returns `true` if this init block is marked `#[payable]`.
    pub fn is_payable(self, db: &'db dyn HirDb) -> bool {
        self.attributes(db).has_marker_attr(db, "payable")
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct ContractRecvListId<'db> {
    #[return_ref]
    pub data: Vec<ContractRecv<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractRecv<'db> {
    pub msg_path: Option<PathId<'db>>,
    pub arms: ContractRecvArmListId<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct ContractRecvArmListId<'db> {
    #[return_ref]
    pub data: Vec<ContractRecvArm<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContractRecvArm<'db> {
    pub pat: PatId,
    pub ret_ty: Option<TypeId<'db>>,
    pub effects: EffectParamListId<'db>,
    pub body: Body<'db>,
    pub attributes: AttrListId<'db>,
}

impl<'db> ContractRecvArm<'db> {
    /// Returns the path from the arm's pattern, if it has one.
    ///
    /// This extracts the path from patterns like `Foo::Bar`, `Foo::Bar(..)`,
    /// or `Foo::Bar { .. }`.
    pub fn variant_path(&self, db: &'db dyn HirDb) -> Option<PathId<'db>> {
        let Partial::Present(pat) = self.pat.data(db, self.body) else {
            return None;
        };

        match pat {
            Pat::Path(Partial::Present(path), ..)
            | Pat::PathTuple(Partial::Present(path), ..)
            | Pat::Record(Partial::Present(path), ..) => Some(*path),
            _ => None,
        }
    }

    pub fn is_fallback(&self, db: &'db dyn HirDb) -> bool {
        matches!(
            self.pat.data(db, self.body),
            Partial::Present(Pat::WildCard)
        )
    }

    /// Returns `true` if this recv arm is marked `#[payable]`.
    pub fn is_payable(&self, db: &'db dyn HirDb) -> bool {
        self.attributes.has_marker_attr(db, "payable")
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Enum<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    pub(in crate::core) variants_list: VariantDefListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Enum>,
}

impl<'db> Enum<'db> {
    pub fn span(self) -> LazyEnumSpan<'db> {
        LazyEnumSpan::new(self)
    }

    pub fn variant_span(self, idx: usize) -> LazyVariantDefSpan<'db> {
        self.span().variants().variant(idx)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn is_must_use(self, db: &'db dyn HirDb) -> bool {
        self.attributes(db).is_must_use(db)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct EnumVariant<'db> {
    pub enum_: Enum<'db>,
    pub idx: u16,
}

impl<'db> EnumVariant<'db> {
    pub fn new(enum_: Enum<'db>, idx: usize) -> Self {
        Self {
            enum_,
            idx: idx as u16,
        }
    }
    pub(in crate::core) fn def(self, db: &'db dyn HirDb) -> &'db VariantDef<'db> {
        &self.enum_.variants_list(db).data(db)[self.idx as usize]
    }

    pub fn kind(self, db: &'db dyn HirDb) -> VariantKind<'db> {
        self.def(db).kind
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<&'db str> {
        Some(self.def(db).name.to_opt()?.data(db))
    }

    pub fn ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.def(db).name.to_opt()
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::Variant(self)
    }

    pub fn span(self) -> LazyVariantDefSpan<'db> {
        self.enum_.variant_span(self.idx as usize)
    }

    /// Returns the human readable initializer args string for this variant
    /// (delegates to the underlying VariantDef helper).
    pub fn format_initializer_args(&self, db: &dyn HirDb) -> String {
        self.def(db).format_initializer_args(db)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct TypeAlias<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) type_ref: Partial<TypeId<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::TypeAlias>,
}

impl<'db> TypeAlias<'db> {
    pub fn span(self) -> LazyTypeAliasSpan<'db> {
        LazyTypeAliasSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    // raw type_ref access kept; shim exposes public ___tmp method.
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Impl<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) type_ref: super::Partial<TypeId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    #[return_ref]
    pub(in crate::core) consts: Vec<AssocConstDef<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Impl>,
}

impl<'db> Impl<'db> {
    pub fn span(self) -> LazyImplSpan<'db> {
        LazyImplSpan::new(self)
    }

    /// Returns the raw associated const definitions from the HIR.
    pub fn hir_consts(self, db: &'db dyn HirDb) -> &'db [AssocConstDef<'db>] {
        self.consts(db)
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope)
    }

    pub fn funcs(self, db: &'db dyn HirDb) -> impl Iterator<Item = Func<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    // raw type_ref access kept; shim exposes public ___tmp method.

    // Semantic `ty` lives in `crate::semantic`.
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Trait<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,

    pub(in crate::core) attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    #[return_ref]
    pub(in crate::core) super_traits_refs: Vec<TraitRefId<'db>>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    #[return_ref]
    pub(in crate::core) types: Vec<AssocTyDecl<'db>>,
    #[return_ref]
    pub(in crate::core) consts: Vec<AssocConstDecl<'db>>,

    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Trait>,
}

impl<'db> Trait<'db> {
    pub fn span(self) -> LazyTraitSpan<'db> {
        LazyTraitSpan::new(self)
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn methods(self, db: &'db dyn HirDb) -> impl Iterator<Item = Func<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }

    pub fn assoc_ty(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<&'db AssocTyDecl<'db>> {
        self.types(db)
            .iter()
            .find(|trait_type| trait_type.name.to_opt() == Some(name))
    }

    /// Returns the associated type declaration by index as used in scope graph.
    pub fn assoc_ty_by_index(self, db: &'db dyn HirDb, idx: usize) -> &'db AssocTyDecl<'db> {
        &self.types(db)[idx]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocTyDecl<'db> {
    pub attributes: AttrListId<'db>,
    pub name: Partial<IdentId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
    pub default: Option<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocConstDecl<'db> {
    pub attributes: AttrListId<'db>,
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
    pub default: Option<Partial<Body<'db>>>,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct ImplTrait<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) trait_ref: Partial<TraitRefId<'db>>,
    pub(crate) type_ref: Partial<TypeId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub(in crate::core) generic_params: GenericParamListId<'db>,
    pub(in crate::core) where_clause: WhereClauseId<'db>,
    #[return_ref]
    pub(crate) types: Vec<AssocTyDef<'db>>,
    #[return_ref]
    pub(in crate::core) consts: Vec<AssocConstDef<'db>>,
    /// The optional user-facing `as Name` alias on this trait impl (FCO
    /// T-Nway). `None` for elided/synthesized impls. Stored only — not yet
    /// bound into any scope nor used for selection (later increments).
    pub(in crate::core) alias: Option<Partial<IdentId<'db>>>,
    /// The optional user-facing `with <path>` permit on this trait impl (FCO
    /// T3). `None` for elided/synthesized impls. The path references a permit
    /// value and is stored UNRESOLVED — not yet bound into any scope, gated,
    /// nor used for selection (later increments).
    pub(in crate::core) impl_permit: Option<Partial<PathId<'db>>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}

impl<'db> ImplTrait<'db> {
    pub fn span(self) -> LazyImplTraitSpan<'db> {
        LazyImplTraitSpan::new(self)
    }

    /// Returns the trait reference for this impl.
    pub fn hir_trait_ref(self, db: &'db dyn HirDb) -> Partial<TraitRefId<'db>> {
        self.trait_ref(db)
    }

    /// Returns the optional user-facing `as Name` alias on this trait impl
    /// (FCO T-Nway). `None` when the impl was written without an `as` clause
    /// or is synthesized. Stored only — name binding/selection are later
    /// increments.
    pub fn hir_alias(self, db: &'db dyn HirDb) -> Option<Partial<IdentId<'db>>> {
        self.alias(db)
    }

    /// Returns the optional user-facing `with <path>` permit on this trait
    /// impl (FCO T3). `None` when the impl was written without a `with` clause
    /// or is synthesized. The path is stored UNRESOLVED — name resolution,
    /// gating, and selection are later increments.
    pub fn hir_impl_permit(self, db: &'db dyn HirDb) -> Option<Partial<PathId<'db>>> {
        self.impl_permit(db)
    }

    /// Returns the raw associated const definitions from the HIR.
    pub fn hir_consts(self, db: &'db dyn HirDb) -> &'db [AssocConstDef<'db>] {
        self.consts(db)
    }

    pub fn associated_type_span(
        self,
        db: &'db dyn HirDb,
        name: IdentId<'db>,
    ) -> Option<LazyTraitTypeSpan<'db>> {
        self.types(db)
            .iter()
            .position(|t| t.name.to_opt() == Some(name))
            .map(|idx| self.span().associated_type(idx))
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn methods(self, db: &'db dyn HirDb) -> impl Iterator<Item = Func<'db>> + 'db {
        self.children_non_nested(db).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }

    /// A `db`-resolved, salsa-index-independent rendering of this impl's
    /// interning identity (its [`TrackedItemId`]'s [`TrackedItemVariant`]).
    /// Exposes the FCO keystone's content key for cross-database comparison:
    /// a *generated* impl renders its `(goal, self_ty)` content, whereas a
    /// hand-written one renders its positional ordinal. See
    /// [`TrackedItemVariant::content_repr`].
    pub fn interning_identity_repr(self, db: &'db dyn HirDb) -> String {
        self.id(db).variant(db).content_repr(db)
    }

    // raw type_ref access kept; shim exposes public ___tmp method.

    // Semantic `ty` lives in `crate::semantic`.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocTyDef<'db> {
    pub attributes: AttrListId<'db>,
    pub name: Partial<IdentId<'db>>,
    pub(crate) type_ref: Partial<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocConstDef<'db> {
    pub attributes: AttrListId<'db>,
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
    pub value: Partial<Body<'db>>,
    /// Only meaningful for consts in inherent `impl` blocks; consts in trait
    /// impls inherit their visibility from the trait.
    pub vis: Visibility,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct DeriveProviderScope<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) attributes: AttrListId<'db>,
    pub provider_path: Partial<PathId<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::DeriveProviderScope>,
}

impl<'db> DeriveProviderScope<'db> {
    pub fn span(self) -> LazyDeriveProviderScopeSpan<'db> {
        LazyDeriveProviderScopeSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let scope = ScopeId::from_item(self.into());
        let s_graph = scope.scope_graph(db);
        s_graph.child_items(scope)
    }

    pub fn derive_decls(self, db: &'db dyn HirDb) -> impl Iterator<Item = DeriveDecl<'db>> + 'db {
        self.children_non_nested(db).filter_map(|item| match item {
            ItemKind::DeriveDecl(decl) => Some(decl),
            _ => None,
        })
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct DeriveDecl<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) attributes: AttrListId<'db>,
    pub head_path: Partial<PathId<'db>>,
    pub target_path: Partial<PathId<'db>>,
    pub selected_provider_path: Option<Partial<PathId<'db>>>,
    pub provider_scope: Option<DeriveProviderScope<'db>>,
    /// True when `selected_provider_path` came from an enclosing
    /// `with Provider { ... }` selector rather than an explicit `using`.
    pub selected_provider_from_scope: bool,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::DeriveDecl>,
}

impl<'db> DeriveDecl<'db> {
    pub fn span(self) -> LazyDeriveDeclSpan<'db> {
        LazyDeriveDeclSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Const<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) attributes: AttrListId<'db>,
    pub(in crate::core) type_ref: Partial<TypeId<'db>>,
    pub body: Partial<Body<'db>>,
    pub vis: Visibility,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Const>,
}

impl<'db> Const<'db> {
    pub fn span(self) -> LazyConstSpan<'db> {
        LazyConstSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    // Semantic `ty` lives in `crate::semantic`.

    // raw type_ref access kept; shim exposes public ___tmp method.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct StaticAssertComparison<'db> {
    pub op: CompBinOp,
    pub lhs: Body<'db>,
    pub rhs: Body<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct StaticAssert<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) attributes: AttrListId<'db>,
    pub condition: Body<'db>,
    pub comparison: Option<StaticAssertComparison<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::StaticAssert>,
}

impl<'db> StaticAssert<'db> {
    pub fn span(self) -> LazyStaticAssertSpan<'db> {
        LazyStaticAssertSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Use<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(in crate::core) attributes: AttrListId<'db>,
    pub path: Partial<super::UsePathId<'db>>,
    pub alias: Option<Partial<UseAlias<'db>>>,
    pub vis: Visibility,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Use>,
}

impl<'db> Use<'db> {
    pub fn span(self) -> LazyUseSpan<'db> {
        LazyUseSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn is_synthetic_use(self, db: &'db dyn HirDb) -> bool {
        matches!(self.origin(db), &HirOrigin::Synthetic)
    }

    /// Returns imported name if it is present and not a glob.
    pub fn imported_name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if let Some(alias) = self.alias(db) {
            return match alias {
                Partial::Present(UseAlias::Ident(name)) => Some(name),
                _ => None,
            };
        }

        self.path(db).to_opt()?.last_ident(db)
    }

    /// Returns the span of imported name span if the use is not a glob.
    /// The returned span is
    /// 1. If the use has an alias, the span of the alias.
    /// 2. If the use has no alias, the span of the last segment of the path.
    pub fn imported_name_span(&self, db: &'db dyn HirDb) -> Option<DynLazySpan<'db>> {
        if self.is_glob(db) {
            return None;
        }

        if self.alias(db).is_some() {
            Some(self.span().alias().into())
        } else {
            let segment_len = self.path(db).to_opt()?.segment_len(db);
            Some(self.span().path().segment(segment_len - 1).into())
        }
    }

    pub fn glob_span(&self, db: &dyn HirDb) -> Option<DynLazySpan<'db>> {
        if !self.is_glob(db) {
            return None;
        }

        let segment_len = self.path(db).to_opt()?.segment_len(db);
        Some(self.span().path().segment(segment_len - 1).into())
    }

    pub fn is_glob(&self, db: &dyn HirDb) -> bool {
        self.path(db).to_opt().is_some_and(|path| path.is_glob(db))
    }

    pub fn is_unnamed(&self, db: &dyn HirDb) -> bool {
        if let Some(alias) = self.alias(db) {
            !matches!(alias, Partial::Present(UseAlias::Ident(_)))
        } else {
            false
        }
    }

    pub(crate) fn pretty_path(&self, db: &dyn HirDb) -> String {
        self.path(db)
            .to_opt()
            .map_or_else(|| "{invalid}".to_string(), |path| path.pretty_path(db))
    }
}

impl FuncModifiers {
    pub fn new(vis: Visibility, is_unsafe: bool, is_const: bool, is_extern: bool) -> Self {
        Self {
            vis,
            is_unsafe,
            is_const,
            is_extern,
        }
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct FieldDefListId<'db> {
    #[return_ref]
    pub data: Vec<FieldDef<'db>>,
}

impl<'db> FieldDefListId<'db> {
    pub fn get_field(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<&'db FieldDef<'db>> {
        self.data(db)
            .iter()
            .find(|field| field.name.to_opt() == Some(name))
    }

    pub fn field_idx(self, db: &dyn HirDb, name: IdentId<'db>) -> Option<usize> {
        self.data(db)
            .iter()
            .position(|field| field.name.to_opt() == Some(name))
    }

    fn format_initializer_args(self, db: &dyn HirDb) -> String {
        let args = self
            .data(db)
            .iter()
            .map(|field| {
                field
                    .name
                    .to_opt()
                    .map_or_else(|| "_".to_string(), |name| name.data(db).to_string())
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!(" {{ {args} }}")
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Hash, salsa::Update)]
pub enum FieldParent<'db> {
    Struct(Struct<'db>),
    Contract(Contract<'db>),
    Variant(EnumVariant<'db>),
}

impl<'db> FieldParent<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<Cow<'db, str>> {
        match self {
            FieldParent::Struct(struct_) => Some(struct_.name(db).to_opt()?.data(db).into()),
            FieldParent::Contract(contract) => Some(contract.name(db).to_opt()?.data(db).into()),
            FieldParent::Variant(variant) => {
                let e = variant.enum_.name(db).to_opt()?.data(db);
                Some(format!("{e}::{}", variant.name(db)?).into())
            }
        }
    }

    pub fn kind_name(self) -> &'static str {
        match self {
            FieldParent::Struct(_) => "struct",
            FieldParent::Contract(_) => "contract",
            FieldParent::Variant(..) => "enum variant",
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            FieldParent::Struct(struct_) => struct_.scope(),
            FieldParent::Contract(contract) => contract.scope(),
            FieldParent::Variant(variant) => variant.scope(),
        }
    }

    pub(in crate::core) fn fields_list(self, db: &'db dyn HirDb) -> FieldDefListId<'db> {
        match self {
            FieldParent::Struct(struct_) => struct_.fields(db),
            FieldParent::Contract(contract) => contract.hir_fields(db),
            FieldParent::Variant(variant) => match variant.kind(db) {
                VariantKind::Record(fields) => fields,
                _ => unreachable!(),
            },
        }
    }

    pub fn top_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        match self {
            FieldParent::Struct(i) => i.top_mod(db),
            FieldParent::Contract(i) => i.top_mod(db),
            FieldParent::Variant(i) => i.enum_.top_mod(db),
        }
    }

    pub fn field_name_span(self, idx: usize) -> DynLazySpan<'db> {
        match self {
            FieldParent::Struct(s) => s.span().fields().field(idx).name().into(),
            FieldParent::Contract(c) => c.span().fields().field(idx).name().into(),
            FieldParent::Variant(v) => v.span().fields().field(idx).name().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef<'db> {
    pub attributes: AttrListId<'db>,
    pub is_event_indexed: bool,
    pub name: Partial<IdentId<'db>>,
    pub(in crate::core) type_ref: Partial<TypeId<'db>>,
    pub vis: Visibility,
    pub is_mut: bool,
}

impl<'db> FieldDef<'db> {
    pub(crate) fn new(
        attributes: AttrListId<'db>,
        name: Partial<IdentId<'db>>,
        type_ref: Partial<TypeId<'db>>,
        vis: Visibility,
        is_event_indexed: bool,
        is_mut: bool,
    ) -> Self {
        Self {
            attributes,
            is_event_indexed,
            name,
            type_ref,
            vis,
            is_mut,
        }
    }

    pub fn type_ref(&self) -> Partial<TypeId<'db>> {
        self.type_ref
    }

    pub fn is_mut(&self) -> bool {
        self.is_mut
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct VariantDefListId<'db> {
    #[return_ref]
    pub data: Vec<VariantDef<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDef<'db> {
    pub attributes: AttrListId<'db>,
    pub name: Partial<IdentId<'db>>,
    pub kind: VariantKind<'db>,
}

impl VariantDef<'_> {
    /// Returns the human readable string of the expected variant initializer.
    /// ## Example
    /// When enum `E` is an variant defined as below:
    /// ```fe
    /// enum E {
    ///     V(u64, i32),
    ///     S { x: u64, y: i32 },
    /// }
    /// ```
    ///
    /// Then the method returns `(_, _)` for the first variant and ` { x, y }`
    /// for the second variant.
    pub fn format_initializer_args(&self, db: &dyn HirDb) -> String {
        match self.kind {
            VariantKind::Unit => "".to_string(),
            VariantKind::Tuple(tup) => {
                let args = (0..tup.len(db)).map(|_| "_").collect::<Vec<_>>().join(", ");
                format!("({args})")
            }

            VariantKind::Record(fields) => fields.format_initializer_args(db),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariantKind<'db> {
    Unit,
    Tuple(TupleTypeId<'db>),
    Record(FieldDefListId<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// Unrestricted public visibility.
    Public,
    /// Private to the defining scope.
    Private,
    /// `pub(ingot)` — visible within the same ingot (like Rust's `pub(crate)`).
    PubIngot,
    /// `pub(super)` — visible within the parent module.
    PubSuper,
}

impl Visibility {
    /// Returns true only for unrestricted `pub`.
    pub fn is_pub(self) -> bool {
        self == Self::Public
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct TrackedItemId<'db> {
    variant: TrackedItemVariant<'db>,
}

impl<'db> TrackedItemId<'db> {
    pub(crate) fn join(self, db: &'db dyn HirDb, variant: TrackedItemVariant<'db>) -> Self {
        let old = self.variant(db);
        let joined = old.join(variant);
        Self::new(db, joined)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackedItemVariant<'db> {
    TopLevelMod(IdentId<'db>),
    Mod(Partial<IdentId<'db>>),
    Func(Partial<IdentId<'db>>),
    Struct(Partial<IdentId<'db>>),
    Contract(Partial<IdentId<'db>>),
    Enum(Partial<IdentId<'db>>),
    TypeAlias(Partial<IdentId<'db>>),
    Impl(u32),
    Trait(Partial<IdentId<'db>>),
    ImplTrait(u32),
    /// FCO KEYSTONE: the content-keyed identity of a *generated* `impl Trait
    /// for Ty` item (the `#[error]`/`#[event]`/`#[msg]` desugarings and the
    /// derive-provider replay). Unlike the positional [`Self::ImplTrait`]
    /// ordinal — which is reset to 0 in every lowering pass and renumbers all
    /// downstream ids when derive targets are reordered — this arm keys on the
    /// HIR-level content that actually distinguishes the impl: its goal trait
    /// reference and self type. Under coherence (at most one impl of a given
    /// trait for a given type) `(goal, self_ty)` is collision-free, so the
    /// interned `TrackedItemId` — and therefore the downstream `ImplementorId`
    /// / `SemanticInstanceKey` — is stable across reorderings of sibling
    /// derive targets. Hand-written `impl Trait` items stay on
    /// [`Self::ImplTrait`] (they are already reorder-stable via
    /// `HirOrigin::raw`).
    GeneratedImplTrait {
        goal: Partial<TraitRefId<'db>>,
        self_ty: Partial<TypeId<'db>>,
    },
    DeriveProviderScope(u32),
    DeriveDecl(u32),
    Const(Partial<IdentId<'db>>),
    StaticAssert(u32),
    ContractInit,
    ContractRecvArm {
        recv_idx: u32,
        arm_idx: u32,
    },
    Use(Partial<super::UsePathId<'db>>),
    /// Namespace marker for items synthesized by the post-lowering expansion
    /// stage (e.g. `#[derive(..)]` impls). Joining this onto the top-level
    /// module's id keeps expansion-generated `TrackedItemId`s disjoint from
    /// the ids of items created during base lowering, whose per-file
    /// `ImplTrait`/`Impl` counters restart from zero in the expansion pass.
    Expansion,
    FuncBody,
    NamelessBody,
    /// Anonymous `bool`-expected const body of a bare const-expression
    /// predicate in a `where` clause, indexed by its position among the
    /// clause's const predicates.
    WhereConstPredicate(u32),
    StaticAssertCondition,
    StaticAssertComparisonLhs,
    StaticAssertComparisonRhs,
    Joined(Box<Self>, Box<Self>),
}
impl<'db> TrackedItemVariant<'db> {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }

    /// A `db`-resolved, salsa-index-independent rendering of this variant,
    /// suitable for comparing interning identity *across databases* (idents are
    /// resolved to their text, paths/types to their source form). Used by the
    /// FCO keystone tripwire to observe that a *generated* impl's interning
    /// identity is stable under derive-target reordering: the
    /// [`Self::ImplTrait`] arm renders its positional ordinal (the instability),
    /// while [`Self::GeneratedImplTrait`] renders only its `(goal, self_ty)`
    /// content (the fix).
    pub fn content_repr(&self, db: &'db dyn crate::HirDb) -> String {
        fn ident(p: &Partial<IdentId>, db: &dyn crate::HirDb) -> String {
            p.to_opt()
                .map_or_else(|| "<missing>".into(), |i| i.data(db).to_string())
        }
        match self {
            Self::TopLevelMod(name) => format!("TopMod({})", name.data(db)),
            Self::Mod(name) => format!("Mod({})", ident(name, db)),
            Self::Func(name) => format!("Func({})", ident(name, db)),
            Self::Struct(name) => format!("Struct({})", ident(name, db)),
            Self::Contract(name) => format!("Contract({})", ident(name, db)),
            Self::Enum(name) => format!("Enum({})", ident(name, db)),
            Self::TypeAlias(name) => format!("TypeAlias({})", ident(name, db)),
            Self::Impl(idx) => format!("Impl(ord:{idx})"),
            Self::Trait(name) => format!("Trait({})", ident(name, db)),
            Self::ImplTrait(idx) => format!("ImplTrait(ord:{idx})"),
            Self::GeneratedImplTrait { goal, self_ty } => {
                let goal = goal
                    .to_opt()
                    .and_then(|t| t.path(db).to_opt())
                    .map_or_else(|| "<missing>".into(), |p| p.pretty_print(db));
                let self_ty = self_ty
                    .to_opt()
                    .map_or_else(|| "<missing>".into(), |t| t.pretty_print(db));
                format!("GeneratedImplTrait(goal:{goal},self:{self_ty})")
            }
            Self::DeriveProviderScope(idx) => format!("DeriveProviderScope(ord:{idx})"),
            Self::DeriveDecl(idx) => format!("DeriveDecl(ord:{idx})"),
            Self::Const(name) => format!("Const({})", ident(name, db)),
            Self::StaticAssert(idx) => format!("StaticAssert(ord:{idx})"),
            Self::ContractInit => "ContractInit".into(),
            Self::ContractRecvArm { recv_idx, arm_idx } => {
                format!("ContractRecvArm({recv_idx},{arm_idx})")
            }
            Self::Use(path) => format!(
                "Use({})",
                path.to_opt()
                    .map_or_else(|| "<missing>".into(), |p| p.pretty_path(db))
            ),
            Self::Expansion => "Expansion".into(),
            Self::FuncBody => "FuncBody".into(),
            Self::NamelessBody => "NamelessBody".into(),
            Self::WhereConstPredicate(idx) => format!("WhereConstPredicate({idx})"),
            Self::StaticAssertCondition => "StaticAssertCondition".into(),
            Self::StaticAssertComparisonLhs => "StaticAssertComparisonLhs".into(),
            Self::StaticAssertComparisonRhs => "StaticAssertComparisonRhs".into(),
            Self::Joined(lhs, rhs) => {
                format!("{}::{}", lhs.content_repr(db), rhs.content_repr(db))
            }
        }
    }
}

#[cfg(test)]
mod keystone_tripwire {
    //! FCO KEYSTONE IDENTITY TRIPWIRE (SGK rung 1a).
    //!
    //! These tests lock the invariant the staged-generation kernel depends on:
    //! a *generated* derive-provider `impl Trait for Ty` item's interning
    //! identity (its [`TrackedItemId`]'s [`TrackedItemVariant`], rendered via
    //! [`ImplTrait::interning_identity_repr`]) is
    //!
    //!   (i)  STABLE across two independent analyses of the same input, and
    //!   (ii) INVARIANT under reordering sibling `derive` targets in the source.
    //!
    //! Both hold because the generated impl keys on its content
    //! ([`TrackedItemVariant::GeneratedImplTrait`]'s `(goal, self_ty)`), not on
    //! a positional ordinal. If a future change regresses the keystone back to a
    //! positional id, (ii) trips: the repr would render `ImplTrait(ord:N)` and
    //! flip with source order.

    use crate::{
        hir_def::ItemKind,
        lower::{generated_hir_items, map_file_to_mod},
        test_db::TestDb,
    };

    /// The sorted interning-identity reprs of every generated `impl Trait`
    /// item for `text`, each produced in its own freshly-built database.
    fn generated_impl_reprs(text: &str) -> Vec<String> {
        let mut db = TestDb::default();
        let file = db.standalone_file(text);
        let top_mod = map_file_to_mod(&db, file);
        let mut reprs: Vec<String> = generated_hir_items(&db, top_mod)
            .iter()
            .filter_map(|item| match item {
                ItemKind::ImplTrait(impl_trait) => Some(impl_trait.interning_identity_repr(&db)),
                _ => None,
            })
            .collect();
        reprs.sort();
        reprs
    }

    /// (i) The generated impls' identities are identical across two independent
    /// analyses of the same source (no hidden per-run / per-database state).
    #[test]
    fn generated_impl_identity_stable_across_analyses() {
        let text = r#"
            #[derive(Eq, Default)]
            struct Point {
                x: u256,
                y: u256,
            }
        "#;

        let first = generated_impl_reprs(text);
        let second = generated_impl_reprs(text);

        assert_eq!(
            first, second,
            "generated impl identities must be stable across analyses"
        );
        // Two derived traits (`Eq`, `Default`), both content-keyed.
        assert_eq!(first.len(), 2);
        assert!(
            first
                .iter()
                .all(|repr| repr.contains("GeneratedImplTrait") && !repr.contains("ord:")),
            "generated impls must be content-keyed, not positional: {first:?}"
        );
    }

    /// (ii) Reordering sibling derive targets does not change any generated
    /// impl's identity: the set of reprs is byte-identical between the two
    /// orderings (content-keyed, not positional).
    #[test]
    fn generated_impl_identity_invariant_under_sibling_reorder() {
        let order_a = r#"
            #[derive(Eq)]
            struct A {
                x: u256,
            }

            #[derive(Eq)]
            struct B {
                y: u256,
            }
        "#;
        let order_b = r#"
            #[derive(Eq)]
            struct B {
                y: u256,
            }

            #[derive(Eq)]
            struct A {
                x: u256,
            }
        "#;

        let reprs_a = generated_impl_reprs(order_a);
        let reprs_b = generated_impl_reprs(order_b);

        assert_eq!(
            reprs_a, reprs_b,
            "generated impl identities must be invariant under sibling derive reordering"
        );
        assert_eq!(reprs_a.len(), 2);
        assert!(
            reprs_a
                .iter()
                .all(|repr| repr.contains("GeneratedImplTrait") && !repr.contains("ord:")),
            "reorder-invariance requires content keying, not positional ordinals: {reprs_a:?}"
        );
    }
}
