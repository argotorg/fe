// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use std::borrow::Cow;

use common::{file::File, ingot::Ingot};
use indexmap::IndexMap;
use parser::ast;

use super::{
    AttrListId, Body, FuncParamListId, FuncParamName, GenericParam, GenericParamListId, HirIngot,
    IdentId, Partial, TupleTypeId, TypeBound, TypeId, UseAlias, WhereClauseId,
    scope_graph::{ScopeGraph, ScopeId},
};
use crate::{
    HirDb,
    analysis::{
        HirAnalysisDb,
        ty::{
            adt_def::AdtRef,
            trait_def::TraitMethod,
            trait_lower::{TraitRefLowerError, lower_trait_ref},
            trait_resolution::{
                PredicateListId,
                constraint::{collect_constraints, collect_func_def_constraints},
            },
            ty_def::{InvalidCause, TyId},
            ty_lower::lower_hir_ty,
        },
    },
    hir_def::TraitRefId,
    lower,
    span::{
        DynLazySpan, HirOrigin,
        item::{
            LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyFuncSpan, LazyImplSpan,
            LazyImplTraitSpan, LazyItemSpan, LazyModSpan, LazyStructSpan, LazyTopModSpan,
            LazyTraitSpan, LazyTraitTypeSpan, LazyTypeAliasSpan, LazyUseSpan, LazyVariantDefSpan,
        },
        params::{LazyGenericParamListSpan, LazyWhereClauseSpan},
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
    Const(Const<'db>),
    Use(Use<'db>),
    /// Body is not an `Item`, but this makes it easier for analyzers to handle
    /// it.
    Body(Body<'db>),
}

impl<'db> ItemKind<'db> {
    fn constraints(&self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        use crate::analysis::ty::func_def::CallableDef;
        use crate::analysis::ty::trait_resolution::constraint::{
            collect_constraints, collect_func_def_constraints,
        };

        match self {
            Self::Func(func) => collect_func_def_constraints(db, CallableDef::Func(*func), true)
                .instantiate_identity(),
            Self::Struct(struct_) => collect_constraints(db, (*struct_).into()).instantiate_identity(),
            // Contracts have no generics and no where-clause constraints
            Self::Contract(_contract) => PredicateListId::empty_list(db),
            Self::Enum(enum_) => collect_constraints(db, (*enum_).into()).instantiate_identity(),
            Self::TypeAlias(alias) => collect_constraints(db, (*alias).into()).instantiate_identity(),
            Self::Impl(impl_) => collect_constraints(db, (*impl_).into()).instantiate_identity(),
            Self::Trait(trait_) => collect_constraints(db, (*trait_).into()).instantiate_identity(),
            Self::ImplTrait(impl_trait) => collect_constraints(db, (*impl_trait).into()).instantiate_identity(),
            // No constraints for these kinds
            Self::Const(_) | Self::Use(_) | Self::Body(_) | Self::TopMod(_) | Self::Mod(_) => {
                PredicateListId::empty_list(db)
            }
        }
    }

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
            Use(_) | Body(_) | Impl(_) | ImplTrait(_) => None,
        }
    }

    /// Returns attributes being applied to this item.
    pub fn attrs(self, db: &'db dyn HirDb) -> Option<AttrListId<'db>> {
        match self {
            Self::Mod(mod_) => mod_.attributes(db),
            Self::Func(func) => func.attributes(db),
            Self::Struct(struct_) => struct_.attributes(db),
            Self::Contract(contract) => contract.attributes(db),
            Self::Enum(enum_) => enum_.attributes(db),
            Self::TypeAlias(alias) => alias.attributes(db),
            Self::Impl(impl_) => impl_.attributes(db),
            Self::Trait(trait_) => trait_.attributes(db),
            Self::ImplTrait(impl_trait) => impl_trait.attributes(db),
            Self::Const(const_) => const_.attributes(db),
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
            Const(_) => "const",
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
            TopMod(_) | Use(_) | Body(_) | Impl(_) | ImplTrait(_) => None,
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
            Impl(_) | ImplTrait(_) | Body(_) => Visibility::Private,
        }
    }

    // pub fn ingot(self, db: &'db dyn HirDb) -> IngotDescription<'db> {
    //     let top_mod = self.top_mod(db);
    //     top_mod.ingot(db)
    // }

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
            ItemKind::Const(const_) => const_.top_mod(db),
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

    pub fn params(self, db: &'db dyn HirDb) -> GenericParamListId<'db> {
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

    pub fn param(self, db: &'db dyn HirDb, idx: usize) -> &'db GenericParam<'db> {
        &self.params(db).data(db)[idx]
    }

    pub fn params_span(self) -> LazyGenericParamListSpan<'db> {
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

    pub fn where_clause(self, db: &'db dyn HirDb) -> Option<WhereClauseId<'db>> {
        self.where_clause_owner()
            .map(|owner| owner.where_clause(db))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::From)]
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

    pub fn where_clause_span(self) -> LazyWhereClauseSpan<'db> {
        match self {
            Self::Func(func) => func.span().where_clause(),
            Self::Struct(struct_) => struct_.span().where_clause(),
            Self::Enum(enum_) => enum_.span().where_clause(),
            Self::Impl(impl_) => impl_.span().where_clause(),
            Self::Trait(trait_) => trait_.span().where_clause(),
            Self::ImplTrait(impl_trait) => impl_trait.span().where_clause(),
        }
    }

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
        // let ingot = self.index(db).containing_ingot(db, location)
        let module_tree = self.ingot(db).module_tree(db);
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
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Mod<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
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
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Func<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub generic_params: GenericParamListId<'db>,
    pub where_clause: WhereClauseId<'db>,
    pub params: Partial<FuncParamListId<'db>>,
    pub ret_ty: Option<TypeId<'db>>,
    pub modifier: ItemModifier,
    pub body: Option<Body<'db>>,
    pub is_extern: bool,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Func>,
}
#[salsa::tracked]
impl<'db> Func<'db> {
    pub fn span(self) -> LazyFuncSpan<'db> {
        LazyFuncSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        self.modifier(db).to_visibility()
    }

    pub fn is_method(self, db: &dyn HirDb) -> bool {
        let Some(params) = self.params(db).to_opt() else {
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
        self.params(db).to_opt()?.data(db).get(idx)?.label_eagerly()
    }

    pub fn param_label_or_name(self, db: &'db dyn HirDb, idx: usize) -> Option<FuncParamName<'db>> {
        let param = self.params(db).to_opt()?.data(db).get(idx)?;
        param.label.or(param.name.to_opt())
    }

    // Analysis methods (type lowering)

    #[salsa::tracked(return_ref)]
    pub fn arg_tys(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::{
            binder::Binder,
            trait_resolution::constraint::collect_func_def_constraints,
            ty_def::{InvalidCause, TyId},
            ty_lower::lower_hir_ty,
        };

        let assumptions =
            collect_func_def_constraints(db, self.into(), true).instantiate_identity();

        match self.params(db) {
            Partial::Present(params) => params
                .data(db)
                .iter()
                .map(|arg| {
                    let ty = arg
                        .ty
                        .to_opt()
                        .map(|ty| lower_hir_ty(db, ty, self.scope(), assumptions))
                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError));
                    Binder::bind(ty)
                })
                .collect(),
            Partial::Absent => vec![],
        }
    }

    #[salsa::tracked]
    pub fn return_ty(self, db: &'db dyn crate::analysis::HirAnalysisDb) -> TyId<'db> {
        let assumptions =
            collect_func_def_constraints(db, self.into(), true).instantiate_identity();
        let ty = self
            .ret_ty(db) // Access the field
            .map(|ty| lower_hir_ty(db, ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::unit(db));
        ty
    }

    #[salsa::tracked]
    pub fn param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        crate::analysis::ty::ty_lower::collect_generic_params(db, self.into())
    }

    pub fn receiver_ty(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Option<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        self.is_method(db)
            .then(|| self.arg_tys(db).first().copied().unwrap())
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Struct<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub generic_params: GenericParamListId<'db>,
    pub where_clause: WhereClauseId<'db>,
    pub fields: FieldDefListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Struct>,
}
#[salsa::tracked]
impl<'db> Struct<'db> {
    pub fn span(self) -> LazyStructSpan<'db> {
        LazyStructSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
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

    /// Returns the type parameter set for this struct.
    /// This method performs the same computation as `lower_adt().param_set()`,
    /// but with direct Salsa caching on the HIR item instead of manual materialization.
    #[salsa::tracked(return_ref)]
    pub fn ty_param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        use crate::analysis::ty::ty_lower::collect_generic_params;
        collect_generic_params(db, self.into())
    }

    /// Returns the field types for this struct.
    /// This method performs the same computation as `lower_adt().fields()`,
    /// but with direct Salsa caching on the HIR item instead of manual materialization.
    #[salsa::tracked(return_ref)]
    pub fn ty_fields(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::adt_def::AdtField<'db> {
        use crate::analysis::ty::adt_def::AdtField;
        let scope = self.scope();
        let fields_data = self
            .fields(db)
            .data(db)
            .iter()
            .map(|field| field.ty)
            .collect();
        AdtField::new(fields_data, scope)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Contract<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub fields: FieldDefListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Contract>,
}
#[salsa::tracked]
impl<'db> Contract<'db> {
    pub fn span(self) -> LazyContractSpan<'db> {
        LazyContractSpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    /// Returns the field types for this contract.
    /// This method performs the same computation as `lower_adt().fields()`,
    /// but with direct Salsa caching on the HIR item instead of manual materialization.
    #[salsa::tracked(return_ref)]
    pub fn ty_fields(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::adt_def::AdtField<'db> {
        use crate::analysis::ty::adt_def::AdtField;
        let scope = self.scope();
        let fields_data = self
            .fields(db)
            .data(db)
            .iter()
            .map(|field| field.ty)
            .collect();
        AdtField::new(fields_data, scope)
    }

    /// Returns an empty parameter set for contracts (they don't have generic parameters).
    /// This method exists to provide a uniform interface with Struct and Enum.
    #[salsa::tracked(return_ref)]
    pub fn ty_param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        use crate::analysis::ty::ty_lower::GenericParamTypeSet;
        GenericParamTypeSet::empty(db, self.scope())
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Enum<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub generic_params: GenericParamListId<'db>,
    pub where_clause: WhereClauseId<'db>,
    pub variants: VariantDefListId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Enum>,
}

#[salsa::tracked]
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

    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId {
        collect_constraints(db, self.into()).instantiate_identity()
    }

    /// Returns the argument types for a variant constructor.
    /// This is a salsa-tracked query to cache the Vec and return a reference.
    #[salsa::tracked(return_ref)]
    pub fn variant_arg_tys(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        idx: u16,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        let field_types = self.ty_fields(db).get(idx as usize).unwrap().iter_types(db);
        field_types.collect()
    }

    /// Returns the type parameter set for this enum.
    /// This method performs the same computation as `lower_adt().param_set()`,
    /// but with direct Salsa caching on the HIR item instead of manual materialization.
    #[salsa::tracked(return_ref)]
    pub fn ty_param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        use crate::analysis::ty::ty_lower::collect_generic_params;
        collect_generic_params(db, self.into())
    }

    /// Returns the field types for all variants of this enum.
    /// This method performs the same computation as `lower_adt().fields()`,
    /// but with direct Salsa caching on the HIR item instead of manual materialization.
    #[salsa::tracked(return_ref)]
    pub fn ty_fields(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::adt_def::AdtField<'db>> {
        use crate::analysis::ty::adt_def::AdtField;
        use crate::hir_def::VariantKind;

        let scope = self.scope();
        let variants = self.variants(db);

        variants
            .data(db)
            .iter()
            .map(|variant| {
                let tys = match variant.kind {
                    VariantKind::Tuple(tuple_id) => tuple_id.data(db).clone(),
                    VariantKind::Record(fields) => {
                        fields.data(db).iter().map(|field| field.ty).collect()
                    }
                    VariantKind::Unit => vec![],
                };
                AdtField::new(tys, scope)
            })
            .collect()
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
    pub fn def(self, db: &'db dyn HirDb) -> &'db VariantDef<'db> {
        &self.enum_.variants(db).data(db)[self.idx as usize]
    }

    pub fn kind(self, db: &'db dyn HirDb) -> VariantKind<'db> {
        self.def(db).kind
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<&'db str> {
        Some(self.def(db).name.to_opt()?.data(db))
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::Variant(self)
    }

    pub fn span(self) -> LazyVariantDefSpan<'db> {
        self.enum_.variant_span(self.idx as usize)
    }

    // Analysis methods (type lowering)

    /// Returns the argument types for this variant constructor.
    /// Returns `None` if this is a Unit variant (not callable).
    pub fn arg_tys_opt(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Option<Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>>>
    {
        use crate::analysis::ty::adt_def::AdtRef;

        // Only tuple variants are callable
        if !matches!(self.kind(db), VariantKind::Tuple(_)) {
            return None;
        }

        let adt_ref: AdtRef = self.enum_.into();
        let field_types = adt_ref
            .fields(db)
            .get(self.idx as usize)
            .unwrap()
            .iter_types(db);
        Some(field_types.collect())
    }

    /// Returns the argument types for this variant constructor.
    /// Should only be called for tuple variants (CallableDef::VariantCtor).
    pub fn arg_tys(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> &'db [crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>] {
        self.enum_.variant_arg_tys(db, self.idx)
    }

    /// Returns the return type for this variant constructor (the enum type with params).
    pub fn return_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let adt_ref: AdtRef = self.enum_.into();
        let mut ret_ty = TyId::adt(db, adt_ref);
        ret_ty = TyId::foldl(db, ret_ty, adt_ref.param_set(db).params(db));
        ret_ty
    }

    /// Returns the generic parameter set (inherited from the parent enum).
    pub fn param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        use crate::analysis::ty::adt_def::AdtRef;

        let adt_ref: AdtRef = self.enum_.into();
        *adt_ref.param_set(db)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct TypeAlias<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub generic_params: GenericParamListId<'db>,
    pub ty: Partial<TypeId<'db>>,
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
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Impl<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub(crate) type_ref: super::Partial<TypeId<'db>>,
    pub attributes: AttrListId<'db>,
    pub generic_params: GenericParamListId<'db>,
    pub where_clause: WhereClauseId<'db>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Impl>,
}
#[salsa::tracked]
impl<'db> Impl<'db> {
    fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId {
        collect_constraints(db, self.into()).instantiate_identity()
    }
    pub fn span(self) -> LazyImplSpan<'db> {
        LazyImplSpan::new(self)
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    pub fn funcs(self, db: &'db dyn HirDb) -> impl Iterator<Item = Func<'db>> + 'db {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }
}

// Analysis methods for Impl
#[salsa::tracked]
impl<'db> Impl<'db> {
    /// Returns the lowered type for this impl block.
    /// Returns an invalid type if the impl's type is malformed.
    #[salsa::tracked]
    pub fn ty(self, db: &'db dyn crate::analysis::HirAnalysisDb) -> TyId<'db> {
        let scope = self.scope();
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();

        self.type_ref(db)
            .to_opt()
            .map(|ty| lower_hir_ty(db, ty, scope, assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Trait<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,

    pub attributes: AttrListId<'db>,
    pub vis: Visibility,
    pub generic_params: GenericParamListId<'db>,
    #[return_ref]
    pub super_traits: Vec<TraitRefId<'db>>,
    pub where_clause: WhereClauseId<'db>,
    #[return_ref]
    pub types: Vec<AssocTyDecl<'db>>,

    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Trait>,
}
#[salsa::tracked]
impl<'db> Trait<'db> {
    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId {
        collect_constraints(db, self.into()).instantiate_identity()
    }
    pub fn span(self) -> LazyTraitSpan<'db> {
        LazyTraitSpan::new(self)
    }

    pub fn children_non_nested(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    pub(crate) fn child_funcs(self, db: &'db dyn HirDb) -> impl Iterator<Item = Func<'db>> + 'db {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
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
}

// Analysis methods for Trait
#[salsa::tracked]
impl<'db> Trait<'db> {
    #[salsa::tracked(return_ref)]
    pub fn methods(self, db: &'db dyn HirAnalysisDb) -> IndexMap<IdentId<'db>, TraitMethod<'db>> {
        let mut methods =
            IndexMap::<IdentId<'db>, crate::analysis::ty::trait_def::TraitMethod<'db>>::default();
        for method in self.child_funcs(db) {
            let Some(name) = method.name(db).to_opt() else {
                continue;
            };
            let callable = crate::analysis::ty::func_def::CallableDef::Func(method);
            let trait_method = crate::analysis::ty::trait_def::TraitMethod(callable);
            // We can simply ignore the conflict here because it's already
            // handled by the def analysis pass
            methods.entry(name).or_insert(trait_method);
        }
        methods
    }

    pub fn params(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> &'db [crate::analysis::ty::ty_def::TyId<'db>] {
        self.param_set(db).params(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn param_set(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_lower::GenericParamTypeSet<'db> {
        crate::analysis::ty::ty_lower::collect_generic_params(db, self.into())
    }

    pub fn self_param(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_def::TyId<'db> {
        self.param_set(db).trait_self(db).unwrap()
    }

    pub fn original_params(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> &'db [crate::analysis::ty::ty_def::TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }

    pub fn expected_implementor_kind(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> &'db crate::analysis::ty::ty_def::Kind {
        self.self_param(db).kind(db)
    }

    /// Lower an associated type's default type to its semantic representation.
    /// Returns None if the associated type has no default.
    pub fn assoc_type_default_ty(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        assoc_type: &AssocTyDecl<'db>,
    ) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
        use crate::analysis::ty::{
            trait_resolution::constraint::collect_constraints, ty_lower::lower_hir_ty,
        };

        let default_ty_hir = assoc_type.default?;
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        Some(lower_hir_ty(db, default_ty_hir, self.scope(), assumptions))
    }

    pub fn ingot(self, db: &'db dyn crate::analysis::HirAnalysisDb) -> Ingot<'db> {
        self.top_mod(db).ingot(db)
    }

    pub fn super_trait_insts(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> &'db common::indexmap::IndexSet<
        crate::analysis::ty::binder::Binder<crate::analysis::ty::trait_def::TraitInstId<'db>>,
    > {
        use crate::analysis::ty::trait_resolution::constraint::{
            collect_super_traits, super_trait_cycle,
        };
        use common::indexmap::IndexSet;
        use std::sync::OnceLock;

        static EMPTY: OnceLock<
            IndexSet<
                crate::analysis::ty::binder::Binder<crate::analysis::ty::trait_def::TraitInstId>,
            >,
        > = OnceLock::new();

        if super_trait_cycle(db, self).is_some() {
            EMPTY.get_or_init(IndexSet::new)
        } else {
            collect_super_traits(db, self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocTyDecl<'db> {
    pub name: Partial<IdentId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
    pub default: Option<TypeId<'db>>,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct ImplTrait<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub trait_ref: Partial<TraitRefId<'db>>,
    pub(crate) type_ref: Partial<TypeId<'db>>,
    pub attributes: AttrListId<'db>,
    pub generic_params: GenericParamListId<'db>,
    pub where_clause: WhereClauseId<'db>,
    #[return_ref]
    pub types: Vec<AssocTyDef<'db>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}
#[salsa::tracked]
impl<'db> ImplTrait<'db> {
    pub fn span(self) -> LazyImplTraitSpan<'db> {
        LazyImplTraitSpan::new(self)
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
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
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
}

// Semantic analysis methods for ImplTrait - these compute lowered trait implementation info
// These replace the need for the Implementor struct
#[salsa::tracked]
impl<'db> ImplTrait<'db> {
    /// (Internal) Lowers this syntactic `ImplTrait` into semantic representation.
    /// Returns the type being implemented (the self type of this impl).
    /// Returns an invalid type if the impl's type is malformed.
    #[salsa::tracked]
    pub fn ty(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> crate::analysis::ty::ty_def::TyId<'db> {
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        use crate::analysis::ty::ty_lower::lower_hir_ty;

        let scope = self.scope();

        self.type_ref(db)
            .to_opt()
            .map(|ty| lower_hir_ty(db, ty, scope, self.constraints(db)))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    /// Returns the lowered trait instance for this impl.
    /// Returns a Result with detailed error information.
    /// Most callers can use `.ok()` if they don't need error details.
    #[salsa::tracked]
    pub fn trait_inst(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Result<
        crate::analysis::ty::trait_def::TraitInstId<'db>,
        crate::analysis::ty::trait_lower::TraitRefLowerError<'db>,
    > {
        let self_ty = self.ty(db);
        let scope = self.scope();
        let assumptions = self.constraints(db);

        let trait_ref = self
            .trait_ref(db)
            .to_opt()
            .ok_or(TraitRefLowerError::Ignored)?;
        lower_trait_ref(db, self_ty, trait_ref, scope, assumptions)
    }

    /// Returns the trait being implemented.
    pub(crate) fn trait_def(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Option<crate::hir_def::Trait<'db>> {
        self.trait_inst(db).ok().map(|inst| inst.def(db))
    }

    /// Returns the generic parameters for this impl block.
    #[salsa::tracked(return_ref)]
    pub(crate) fn impl_params(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::ty_def::TyId<'db>> {
        use crate::analysis::ty::ty_lower::collect_generic_params;
        collect_generic_params(db, self.into()).params(db).to_vec()
    }

    /// Returns all associated types defined in this impl, including trait defaults.
    pub(crate) fn assoc_types(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
    ) -> common::indexmap::IndexMap<IdentId<'db>, crate::analysis::ty::ty_def::TyId<'db>> {
        use crate::analysis::ty::binder::Binder;
        use crate::analysis::ty::trait_resolution::constraint::collect_constraints;
        use crate::analysis::ty::ty_lower::lower_hir_ty;

        let scope = self.scope();
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();

        let mut types: common::indexmap::IndexMap<_, _> = self
            .types(db)
            .iter()
            .filter_map(|t| match (t.name.to_opt(), t.type_ref.to_opt()) {
                (Some(name), Some(ty)) => Some((name, lower_hir_ty(db, ty, scope, assumptions))),
                _ => None,
            })
            .collect();

        // Merge trait associated type defaults
        if let Some(trait_inst) = self.trait_inst(db).ok() {
            let trait_def = trait_inst.def(db);
            let trait_scope = trait_def.scope();

            for t in trait_def.types(db).iter() {
                let (Some(name), Some(default)) = (t.name.to_opt(), t.default) else {
                    continue;
                };

                types.entry(name).or_insert_with(|| {
                    let lowered = lower_hir_ty(db, default, trait_scope, assumptions);
                    Binder::bind(lowered).instantiate(db, trait_inst.args(db))
                });
            }
        }

        types
    }

    /// Returns a specific associated type by name.
    pub(crate) fn assoc_ty(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
        self.assoc_types(db).get(&name).copied()
    }

    /// Returns the constraints (where clauses) for this impl.
    #[salsa::tracked]
    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        collect_constraints(db, self.into()).instantiate_identity()
    }

    /// Creates fresh type variables for this impl's generic parameters.
    /// These fresh vars can then be used to instantiate the impl's types atomically.
    ///
    /// This ensures that all pieces (self_ty, trait_inst, constraints) use the
    /// SAME fresh vars, maintaining correctness.
    pub(crate) fn create_fresh_vars<U>(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        table: &mut crate::analysis::ty::unify::UnificationTableBase<'db, U>,
    ) -> Vec<crate::analysis::ty::ty_def::TyId<'db>>
    where
        U: crate::analysis::ty::unify::UnificationStore<'db>,
    {
        self.impl_params(db)
            .iter()
            .map(|p| table.new_var_from_param(*p))
            .collect()
    }

    /// Returns the self type instantiated with the given fresh vars.
    pub(crate) fn ty_instantiated(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        fresh_vars: &[crate::analysis::ty::ty_def::TyId<'db>],
    ) -> crate::analysis::ty::ty_def::TyId<'db> {
        use crate::analysis::ty::binder::Binder;
        Binder::bind(self.ty(db)).instantiate(db, fresh_vars)
    }

    /// Returns the trait instance instantiated with the given fresh vars.
    pub(crate) fn trait_inst_instantiated(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        fresh_vars: &[crate::analysis::ty::ty_def::TyId<'db>],
    ) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        use crate::analysis::ty::binder::Binder;
        let trait_inst = self.trait_inst(db).ok()?;
        Some(Binder::bind(trait_inst).instantiate(db, fresh_vars))
    }

    /// Returns the constraints instantiated with the given fresh vars.
    pub(crate) fn constraints_instantiated(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        fresh_vars: &[crate::analysis::ty::ty_def::TyId<'db>],
    ) -> crate::analysis::ty::trait_resolution::PredicateListId<'db> {
        use crate::analysis::ty::binder::Binder;
        Binder::bind(self.constraints(db)).instantiate(db, fresh_vars)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct AssocTyDef<'db> {
    pub name: Partial<IdentId<'db>>,
    pub type_ref: Partial<TypeId<'db>>,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Const<'db> {
    #[id]
    id: TrackedItemId<'db>,

    pub name: Partial<IdentId<'db>>,
    pub attributes: AttrListId<'db>,
    pub ty: Partial<TypeId<'db>>,
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
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Use<'db> {
    #[id]
    id: TrackedItemId<'db>,

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemModifier {
    Pub,
    Unsafe,
    PubAndUnsafe,
    None,
}

impl ItemModifier {
    pub fn to_visibility(self) -> Visibility {
        match self {
            ItemModifier::Pub | ItemModifier::PubAndUnsafe => Visibility::Public,
            ItemModifier::Unsafe | ItemModifier::None => Visibility::Private,
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

    pub fn fields(self, db: &'db dyn HirDb) -> FieldDefListId<'db> {
        match self {
            FieldParent::Struct(struct_) => struct_.fields(db),
            FieldParent::Contract(contract) => contract.fields(db),
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
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
    pub vis: Visibility,
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
    Public,
    Private,
}

impl Visibility {
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
    Impl(Partial<TypeId<'db>>),
    Trait(Partial<IdentId<'db>>),
    ImplTrait(Partial<TraitRefId<'db>>, Partial<TypeId<'db>>),
    Const(Partial<IdentId<'db>>),
    Use(Partial<super::UsePathId<'db>>),
    FuncBody,
    NamelessBody,
    Joined(Box<Self>, Box<Self>),
}
impl TrackedItemVariant<'_> {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }
}
