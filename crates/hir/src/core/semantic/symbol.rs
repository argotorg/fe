//! Symbol types for LSP features and semantic analysis.

use crate::HirDb;
use crate::hir_def::{
    Const, Contract, Enum, FieldParent, Func, GenericParam, GenericParamOwner, IdentId, Impl,
    ImplTrait, ItemKind, Mod, Struct, TopLevelMod, Trait, TypeAlias, Use,
};
use crate::span::DynLazySpan;
use enum_dispatch::enum_dispatch;

use super::{
    FieldView, FuncParamView, GenericParamView, ImplAssocConstView, ImplAssocTypeView,
    TraitAssocConstView, TraitAssocTypeView, VariantView,
};

#[enum_dispatch]
pub trait SymbolInfo<'db> {
    fn display_name(&self) -> &'static str;
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>>;
    fn name_span(&self) -> Option<DynLazySpan<'db>>;
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db>;
}

// Item implementations

impl<'db> SymbolInfo<'db> for TopLevelMod<'db> {
    fn display_name(&self) -> &'static str {
        "module"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Some(TopLevelMod::name(*self, db))
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        None
    }
    fn top_mod(&self, _db: &'db dyn HirDb) -> TopLevelMod<'db> {
        *self
    }
}

impl<'db> SymbolInfo<'db> for Mod<'db> {
    fn display_name(&self) -> &'static str {
        "module"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Mod::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Mod::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Func<'db> {
    fn display_name(&self) -> &'static str {
        "function"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Func::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Func::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Struct<'db> {
    fn display_name(&self) -> &'static str {
        "struct"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Struct::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Struct::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Contract<'db> {
    fn display_name(&self) -> &'static str {
        "contract"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Contract::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Contract::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Enum<'db> {
    fn display_name(&self) -> &'static str {
        "enum"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Enum::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Enum::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for TypeAlias<'db> {
    fn display_name(&self) -> &'static str {
        "type alias"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        TypeAlias::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().alias().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        TypeAlias::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Impl<'db> {
    fn display_name(&self) -> &'static str {
        "impl"
    }
    fn name(&self, _db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        None
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        None
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Impl::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Trait<'db> {
    fn display_name(&self) -> &'static str {
        "trait"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Trait::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Trait::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for ImplTrait<'db> {
    fn display_name(&self) -> &'static str {
        "impl trait"
    }
    fn name(&self, _db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        None
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        None
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        ImplTrait::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Const<'db> {
    fn display_name(&self) -> &'static str {
        "constant"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        Const::name(*self, db).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Const::top_mod(*self, db)
    }
}

impl<'db> SymbolInfo<'db> for Use<'db> {
    fn display_name(&self) -> &'static str {
        "use"
    }
    fn name(&self, _db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        None
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        None
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        Use::top_mod(*self, db)
    }
}

// View implementations

impl<'db> SymbolInfo<'db> for FieldView<'db> {
    fn display_name(&self) -> &'static str {
        "field"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.parent.fields_list(db).data(db)[self.idx].name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(match self.parent {
            FieldParent::Struct(s) => s.span().fields().field(self.idx).name().into(),
            FieldParent::Contract(c) => c.span().fields().field(self.idx).name().into(),
            FieldParent::Variant(v) => v.span().fields().field(self.idx).name().into(),
        })
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner_item().top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for VariantView<'db> {
    fn display_name(&self) -> &'static str {
        "variant"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.owner.variants_list(db).data(db)[self.idx].name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner.top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for GenericParamView<'db> {
    fn display_name(&self) -> &'static str {
        match self.param {
            GenericParam::Type(_) => "type parameter",
            GenericParam::Const(_) => "const parameter",
        }
    }
    fn name(&self, _db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        GenericParamView::name(self).to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(GenericParamView::name_span(self).into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        ItemKind::from(self.owner).top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for FuncParamView<'db> {
    fn display_name(&self) -> &'static str {
        "parameter"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.func
            .params_list(db)
            .to_opt()
            .and_then(|p| p.data(db).get(self.idx))
            .and_then(|p| p.name())
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.func.top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for TraitAssocTypeView<'db> {
    fn display_name(&self) -> &'static str {
        "associated type"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.decl(db).name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner.top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for TraitAssocConstView<'db> {
    fn display_name(&self) -> &'static str {
        "associated constant"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.decl(db).name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner.top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for ImplAssocTypeView<'db> {
    fn display_name(&self) -> &'static str {
        "associated type"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.owner.types(db)[self.idx].name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner.top_mod(db)
    }
}

impl<'db> SymbolInfo<'db> for ImplAssocConstView<'db> {
    fn display_name(&self) -> &'static str {
        "associated constant"
    }
    fn name(&self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.def(db).name.to_opt()
    }
    fn name_span(&self) -> Option<DynLazySpan<'db>> {
        Some(self.span().name().into())
    }
    fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.owner.top_mod(db)
    }
}

/// Unified symbol handle - delegation auto-generated by enum_dispatch.
#[enum_dispatch(SymbolInfo)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolKind<'db> {
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
    Field(FieldView<'db>),
    Variant(VariantView<'db>),
    GenericParam(GenericParamView<'db>),
    FuncParam(FuncParamView<'db>),
    TraitAssocType(TraitAssocTypeView<'db>),
    TraitAssocConst(TraitAssocConstView<'db>),
    ImplAssocType(ImplAssocTypeView<'db>),
    ImplAssocConst(ImplAssocConstView<'db>),
}

impl<'db> From<ItemKind<'db>> for SymbolKind<'db> {
    fn from(item: ItemKind<'db>) -> Self {
        match item {
            ItemKind::TopMod(m) => Self::TopMod(m),
            ItemKind::Mod(m) => Self::Mod(m),
            ItemKind::Func(f) => Self::Func(f),
            ItemKind::Struct(s) => Self::Struct(s),
            ItemKind::Contract(c) => Self::Contract(c),
            ItemKind::Enum(e) => Self::Enum(e),
            ItemKind::TypeAlias(t) => Self::TypeAlias(t),
            ItemKind::Impl(i) => Self::Impl(i),
            ItemKind::Trait(t) => Self::Trait(t),
            ItemKind::ImplTrait(i) => Self::ImplTrait(i),
            ItemKind::Const(c) => Self::Const(c),
            ItemKind::Use(u) => Self::Use(u),
            ItemKind::Body(_) => panic!("Body is not a named symbol"),
        }
    }
}

impl<'db> SymbolKind<'db> {
    /// Returns the child symbols of this symbol for traversal.
    pub fn children(&self, db: &'db dyn HirDb) -> Vec<SymbolKind<'db>> {
        match self {
            // Modules contain items
            Self::TopMod(m) => m.children_non_nested(db).map(SymbolKind::from).collect(),
            Self::Mod(m) => m.children_non_nested(db).map(SymbolKind::from).collect(),

            // Func has params + generic params
            Self::Func(f) => f
                .params(db)
                .map(SymbolKind::from)
                .chain(GenericParamOwner::Func(*f).params(db).map(SymbolKind::from))
                .collect(),

            // Struct has fields + generic params
            Self::Struct(s) => FieldParent::Struct(*s)
                .fields(db)
                .map(SymbolKind::from)
                .chain(GenericParamOwner::Struct(*s).params(db).map(SymbolKind::from))
                .collect(),

            // Contract has fields + child funcs (no generic params)
            Self::Contract(c) => {
                let s_graph = c.top_mod(db).scope_graph(db);
                let funcs = s_graph.child_items(c.scope()).filter_map(|item| match item {
                    ItemKind::Func(f) => Some(SymbolKind::from(f)),
                    _ => None,
                });
                FieldParent::Contract(*c)
                    .fields(db)
                    .map(SymbolKind::from)
                    .chain(funcs)
                    .collect()
            }

            // Enum has variants + generic params
            Self::Enum(e) => e
                .variants(db)
                .map(SymbolKind::from)
                .chain(GenericParamOwner::Enum(*e).params(db).map(SymbolKind::from))
                .collect(),

            // TypeAlias has generic params only
            Self::TypeAlias(t) => GenericParamOwner::TypeAlias(*t)
                .params(db)
                .map(SymbolKind::from)
                .collect(),

            // Impl has funcs + generic params
            Self::Impl(i) => i
                .funcs(db)
                .map(SymbolKind::from)
                .chain(GenericParamOwner::Impl(*i).params(db).map(SymbolKind::from))
                .collect(),

            // Trait has methods + assoc types + assoc consts + generic params
            Self::Trait(t) => t
                .methods(db)
                .map(SymbolKind::from)
                .chain(t.assoc_types(db).map(SymbolKind::from))
                .chain(t.assoc_consts(db).map(SymbolKind::from))
                .chain(GenericParamOwner::Trait(*t).params(db).map(SymbolKind::from))
                .collect(),

            // ImplTrait has methods + assoc types + assoc consts + generic params
            Self::ImplTrait(i) => i
                .methods(db)
                .map(SymbolKind::from)
                .chain(i.assoc_types(db).map(SymbolKind::from))
                .chain(i.assoc_consts(db).map(SymbolKind::from))
                .chain(GenericParamOwner::ImplTrait(*i).params(db).map(SymbolKind::from))
                .collect(),

            // Const has no children
            Self::Const(_) => Vec::new(),

            // Use has no children
            Self::Use(_) => Vec::new(),

            // Variant has fields
            Self::Variant(v) => v.fields(db).map(SymbolKind::from).collect(),

            // Leaf symbols with no children
            Self::Field(_)
            | Self::GenericParam(_)
            | Self::FuncParam(_)
            | Self::TraitAssocType(_)
            | Self::TraitAssocConst(_)
            | Self::ImplAssocType(_)
            | Self::ImplAssocConst(_) => Vec::new(),
        }
    }
}

/// Role of a reference in its context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RefRole {
    Use,
    Write,
    Call,
    Import,
    TypeAnnotation,
    Inherit,
}

/// A use-site reference to a symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRef<'db> {
    pub span: DynLazySpan<'db>,
    pub target: SymbolKind<'db>,
    pub role: RefRole,
}

impl<'db> SymbolRef<'db> {
    pub fn new(span: DynLazySpan<'db>, target: SymbolKind<'db>, role: RefRole) -> Self {
        Self { span, target, role }
    }
}
