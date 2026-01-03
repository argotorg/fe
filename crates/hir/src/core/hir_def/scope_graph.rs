use std::io;

use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::Ingot,
};
use rustc_hash::FxHashSet;
use salsa::Update;

use super::{
    AssocConstDecl, AssocTyDecl, AttrListId, Body, Const, Contract, Enum, EnumVariant, ExprId,
    FieldDef, FieldParent, Func, FuncParam, FuncParamName, GenericParam, IdentId, Impl, ImplTrait,
    ItemKind, Mod, Struct, TopLevelMod, Trait, TypeAlias, Use, VariantDef, VariantKind, Visibility,
    scope_graph_viz::ScopeGraphFormatter,
};
use crate::{
    HirDb,
    hir_def::{BodyKind, GenericParamOwner},
    span::DynLazySpan,
};

/// Represents a scope relation graph in a top-level module.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ScopeGraph<'db> {
    /// The top-level module containing the scope graph.
    pub top_mod: TopLevelMod<'db>,
    /// The scopes in the graph.
    pub scopes: IndexMap<ScopeId<'db>, Scope<'db>>,
    /// The all unresolved uses in the graph, this is used in name resolution.
    pub unresolved_uses: FxHashSet<Use<'db>>,
}

impl<'db> ScopeGraph<'db> {
    /// Represents all item scopes in a top-level module in depth-first order.
    pub fn items_dfs<'a>(&'a self, db: &'a dyn HirDb) -> impl Iterator<Item = ItemKind<'a>> + 'a {
        ScopeGraphItemIterDfs {
            db,
            graph: self,
            visited: Default::default(),
            stack: vec![self.top_mod.scope()],
        }
    }

    /// Returns the direct child items of the given `scope`.
    pub fn child_items(&self, scope: ScopeId<'db>) -> impl Iterator<Item = ItemKind<'db>> + '_ {
        self.children(scope).filter_map(|child| child.to_item())
    }

    /// Returns the direct child scopes of the given `scope`
    pub fn children(&self, scope: ScopeId<'db>) -> impl Iterator<Item = ScopeId<'db>> + '_ {
        self.edges(scope).iter().filter_map(|edge| match edge.kind {
            EdgeKind::Lex(_)
            | EdgeKind::Super(_)
            | EdgeKind::Ingot(_)
            | EdgeKind::SelfTy(_)
            | EdgeKind::Self_(_) => None,

            _ => Some(edge.dest),
        })
    }

    /// Returns the all edges outgoing from the given `scope`.
    pub fn edges(&self, scope: ScopeId<'db>) -> &IndexSet<ScopeEdge<'db>> {
        let scope_data = self
            .scopes
            .get(&scope)
            .unwrap_or_else(|| panic!("no scope entry for key: {scope:?}"));
        &scope_data.edges
    }

    /// Write a scope graph as a dot file format to given `w`.
    pub fn write_as_dot(&self, db: &dyn HirDb, w: &mut impl io::Write) -> io::Result<()> {
        ScopeGraphFormatter::new(db, self).render(w)
    }

    pub fn scope_data(&self, scope: &ScopeId<'db>) -> &Scope<'_> {
        &self.scopes[scope]
    }
}

/// An reference to a `[ScopeData]` in a `ScopeGraph`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub enum ScopeId<'db> {
    /// An item scope.
    Item(ItemKind<'db>),

    /// A generic parameter scope.
    GenericParam(ItemKind<'db>, u16),

    /// Trait associated type scope.
    TraitType(Trait<'db>, u16),
    /// Trait associated const scope.
    TraitConst(Trait<'db>, u16),

    /// A function parameter scope.
    FuncParam(ItemKind<'db>, u16),

    /// A field scope.
    Field(FieldParent<'db>, u16),

    /// A variant scope.
    Variant(EnumVariant<'db>),

    /// A block scope.
    Block(Body<'db>, ExprId),
}
impl<'db> ScopeId<'db> {
    /// Returns the top level module containing this scope.
    pub fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        match self {
            ScopeId::Item(item) => item.top_mod(db),
            ScopeId::GenericParam(item, _) => item.top_mod(db),
            ScopeId::TraitType(t, _) => t.top_mod(db),
            ScopeId::TraitConst(t, _) => t.top_mod(db),
            ScopeId::FuncParam(item, _) => item.top_mod(db),
            ScopeId::Field(p, _) => p.top_mod(db),
            ScopeId::Variant(v) => v.enum_.top_mod(db),
            ScopeId::Block(body, _) => body.top_mod(db),
        }
    }

    /// Convert an item to a scope id.
    pub fn from_item(item: ItemKind<'db>) -> Self {
        Self::Item(item)
    }

    /// Convert a scope id to an item if the scope is an item.
    pub fn to_item(self) -> Option<ItemKind<'db>> {
        match self {
            ScopeId::Item(item) => Some(item),
            _ => None,
        }
    }

    /// Returns the body if this is a block scope or body item scope.
    ///
    /// Block scopes are created for code blocks within function bodies.
    /// Returns `None` for other scope types.
    pub fn body(self) -> Option<Body<'db>> {
        match self {
            ScopeId::Block(body, _) => Some(body),
            ScopeId::Item(ItemKind::Body(body)) => Some(body),
            _ => None,
        }
    }

    /// Returns the nearest item that contains this scope.
    /// If the scope is item itself, returns the item.
    pub fn item(self) -> ItemKind<'db> {
        match self {
            ScopeId::Item(item) => item,
            ScopeId::GenericParam(item, _) => item,
            ScopeId::FuncParam(item, _) => item,
            ScopeId::TraitType(t, _) => t.into(),
            ScopeId::TraitConst(t, _) => t.into(),
            ScopeId::Field(FieldParent::Struct(s), _) => s.into(),
            ScopeId::Field(FieldParent::Contract(c), _) => c.into(),
            ScopeId::Field(FieldParent::Variant(v), _) | ScopeId::Variant(v) => v.enum_.into(),
            ScopeId::Block(body, _) => body.into(),
        }
    }

    /// Resolves the `ScopeId` to `T`.
    /// Returns `None` if the resolution is not defined for this scope.
    pub fn resolve_to<T>(self, db: &'db dyn HirDb) -> Option<T>
    where
        T: FromScope<'db>,
    {
        T::from_scope(self, db)
    }

    /// Returns attributes being applied to the scope.
    pub fn attrs(self, db: &'db dyn HirDb) -> Option<AttrListId<'db>> {
        match self {
            ScopeId::Item(item) => item.attrs(db),
            ScopeId::Field(..) => {
                let def: &FieldDef = self.resolve_to(db).unwrap();
                Some(def.attributes)
            }
            ScopeId::Variant(..) => {
                let def: &VariantDef = self.resolve_to(db).unwrap();
                Some(def.attributes)
            }
            ScopeId::TraitType(t, idx) => t.types(db).get(idx as usize).map(|d| d.attributes),
            ScopeId::TraitConst(t, idx) => t.consts(db).get(idx as usize).map(|d| d.attributes),
            _ => None,
        }
    }

    /// Returns the scope graph containing this scope.
    pub fn scope_graph(self, db: &'db dyn HirDb) -> &'db ScopeGraph<'db> {
        self.top_mod(db).scope_graph(db)
    }

    pub fn edges(self, db: &'db dyn HirDb) -> &'db IndexSet<ScopeEdge<'db>> {
        self.scope_graph(db).edges(self)
    }

    /// Returns `true` if `scope` is reachable from `self` by following only
    /// lexical edges.
    pub fn is_lex_child(self, db: &dyn HirDb, scope: &ScopeId) -> bool {
        if self.top_mod(db) != scope.top_mod(db) {
            return false;
        }

        match self.lex_parent(db) {
            Some(lex_parent) => {
                if &lex_parent == scope {
                    return true;
                }
                lex_parent.is_lex_child(db, scope)
            }
            None => false,
        }
    }

    /// Returns `true` if `self` is a transitive reflexive child of `of`.
    pub fn is_transitive_child_of(self, db: &dyn HirDb, of: ScopeId) -> bool {
        let mut current = Some(self);

        while let Some(scope) = current {
            if scope == of {
                return true;
            }
            current = scope.parent(db);
        }

        false
    }

    /// Return the `IngotDescription` containing the scope.
    pub fn ingot(self, db: &'db dyn HirDb) -> Ingot<'db> {
        self.top_mod(db).ingot(db)
    }

    /// Returns the `Scope` data for this scope.
    pub fn data(self, db: &'db dyn HirDb) -> &'db Scope<'db> {
        self.scope_graph(db).scope_data(&self)
    }

    /// Returns the parent scope of this scope.
    /// The parent scope is
    /// 1. the lexical parent if it exists
    /// 2. the parent module if 1. does not exist
    pub fn parent(self, db: &'db dyn HirDb) -> Option<Self> {
        let mut super_dest = None;
        for edge in self.edges(db) {
            if let EdgeKind::Lex(_) = edge.kind {
                return Some(edge.dest);
            }
            if let EdgeKind::Super(_) = edge.kind {
                super_dest = Some(edge.dest);
            }
        }
        super_dest
    }

    /// Returns the lexical parent scope of this scope.
    pub fn lex_parent(self, db: &'db dyn HirDb) -> Option<Self> {
        self.data(db)
            .edges
            .iter()
            .find(|e| matches!(e.kind, EdgeKind::Lex(_)))
            .map(|e| e.dest)
    }

    /// Returns the parent module of this scope.
    pub fn parent_module(self, db: &'db dyn HirDb) -> Option<Self> {
        let parent_item = self.parent_item(db)?;
        match parent_item {
            ItemKind::Mod(_) | ItemKind::TopMod(_) => Some(Self::Item(parent_item)),
            _ => {
                let parent_id = Self::from_item(parent_item);
                parent_id.parent_module(db)
            }
        }
    }

    /// Returns the direct child items of the given `scope`.
    pub fn child_items(self, db: &'db dyn HirDb) -> impl Iterator<Item = ItemKind<'db>> + 'db {
        self.scope_graph(db).child_items(self)
    }

    /// Returns the direct child scopes of the given `scope`
    pub fn children(self, db: &'db dyn HirDb) -> impl Iterator<Item = ScopeId<'db>> + 'db {
        self.scope_graph(db).children(self)
    }

    /// Returns all items visible in this scope for the specified domain(s).
    ///
    /// This includes items from imports, direct child items, and parent scopes.
    /// Requires `HirAnalysisDb` for name resolution.
    ///
    /// # Example
    /// ```ignore
    /// use hir::analysis::name_resolution::NameDomain;
    ///
    /// // Get all values and types visible in this scope
    /// let items = scope.items_in_scope(db, NameDomain::VALUE | NameDomain::TYPE);
    /// ```
    pub fn items_in_scope(
        self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        domain: crate::analysis::name_resolution::NameDomain,
    ) -> &'db common::indexmap::IndexMap<String, crate::analysis::name_resolution::NameRes<'db>>
    {
        crate::analysis::name_resolution::items_in_scope(db, self, domain)
    }

    /// Returns `true` if the scope is a type.
    pub fn is_type(self) -> bool {
        match self {
            ScopeId::Item(item) => item.is_type(),
            ScopeId::GenericParam(..) => true,
            _ => false,
        }
    }

    /// Returns the item that contains this scope.
    pub fn parent_item(self, db: &'db dyn HirDb) -> Option<ItemKind<'db>> {
        let mut parent = self.parent(db)?;
        loop {
            match parent {
                ScopeId::Item(item) => return Some(item),
                _ => {
                    parent = parent.parent(db)?;
                }
            }
        }
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        match self {
            ScopeId::Item(item) => item.name(db),

            ScopeId::Variant(..) => self.resolve_to::<&VariantDef>(db).unwrap().name.to_opt(),

            ScopeId::Field(..) => self.resolve_to::<&FieldDef>(db).unwrap().name.to_opt(),

            ScopeId::FuncParam(..) => {
                let param: &FuncParam = self.resolve_to(db).unwrap();
                if let Some(FuncParamName::Ident(ident)) = param.label {
                    Some(ident)
                } else {
                    param.name()
                }
            }

            ScopeId::GenericParam(..) => {
                let param: &GenericParam = self.resolve_to(db).unwrap();
                param.name().to_opt()
            }

            ScopeId::TraitType(t, idx) => t.assoc_ty_by_index(db, idx as usize).name.to_opt(),
            ScopeId::TraitConst(t, idx) => t.const_by_index(idx as usize).name(db),

            ScopeId::Block(..) => None,
        }
    }

    pub fn name_span(self, db: &'db dyn HirDb) -> Option<DynLazySpan<'db>> {
        match self {
            ScopeId::Item(item) => item.name_span(),

            ScopeId::Variant(v) => Some(v.span().name().into()),

            ScopeId::Field(p, idx) => Some(p.field_name_span(idx as usize)),

            ScopeId::FuncParam(parent, idx) => {
                let func: Func = parent.try_into().unwrap();
                let param = &func.params_list(db).to_opt()?.data(db)[idx as usize];
                let param_span = func.span().params().param(idx as usize);
                if let Some(FuncParamName::Ident(_)) = param.label {
                    Some(param_span.label().into())
                } else {
                    Some(param_span.name().into())
                }
            }

            ScopeId::GenericParam(parent, idx) => {
                let parent = GenericParamOwner::from_item_opt(parent).unwrap();

                Some(parent.params_span().param(idx as usize).into())
            }

            ScopeId::TraitType(t, idx) => {
                Some(t.span().item_list().assoc_type(idx as usize).into())
            }
            ScopeId::TraitConst(t, idx) => {
                Some(t.span().item_list().assoc_const(idx as usize).into())
            }

            ScopeId::Block(..) => None,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            ScopeId::Item(item) => item.kind_name(),
            ScopeId::GenericParam(_, _) => "type",
            ScopeId::TraitType(..) => "associated type",
            ScopeId::TraitConst(..) => "associated const",
            ScopeId::FuncParam(_, _) => "value",
            ScopeId::Field(_, _) => "field",
            ScopeId::Variant(..) => "value",
            ScopeId::Block(_, _) => "block",
        }
    }

    pub fn pretty_path(self, db: &dyn HirDb) -> Option<String> {
        let name = match self {
            ScopeId::Block(body, expr) => format!("{{block{}}}", body.iter_block(db)[&expr]),
            ScopeId::Item(ItemKind::Body(body)) => match body.body_kind(db) {
                BodyKind::FuncBody => "{fn_body}".to_string(),
                BodyKind::Anonymous => "{anonymous_body}".to_string(),
            },
            _ => self.name(db)?.data(db).clone(),
        };

        if let Some(parent) = self.parent(db) {
            let parent_path = parent.pretty_path(db)?;
            Some(format!("{parent_path}::{name}"))
        } else {
            Some(name)
        }
    }
}

struct ScopeGraphItemIterDfs<'db, 'a> {
    db: &'db dyn HirDb,
    graph: &'a ScopeGraph<'db>,
    visited: FxHashSet<ScopeId<'db>>,
    stack: Vec<ScopeId<'db>>,
}

impl<'db> std::iter::Iterator for ScopeGraphItemIterDfs<'db, '_> {
    type Item = ItemKind<'db>;

    fn next(&mut self) -> Option<ItemKind<'db>> {
        while let Some(scope) = self.stack.pop() {
            self.visited.insert(scope);
            for edge in self.graph.edges(scope).iter().rev() {
                let dest = edge.dest;
                let top_mod = dest.top_mod(self.db);
                if top_mod != self.graph.top_mod || self.visited.contains(&dest) {
                    continue;
                }

                match dest {
                    ScopeId::Item(_) | ScopeId::Block(..) => {
                        self.stack.push(dest);
                    }

                    _ => {}
                }
            }

            if let Some(item) = scope.to_item() {
                return Some(item);
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct Scope<'db> {
    pub id: ScopeId<'db>,
    pub edges: IndexSet<ScopeEdge<'db>>,
    pub vis: Visibility,
}

impl<'db> Scope<'db> {
    pub fn new(kind: ScopeId<'db>, vis: Visibility) -> Self {
        Self {
            id: kind,
            edges: Default::default(),
            vis,
        }
    }
}

/// An edge of the scope graph.
/// The edge contains the destination of the edge and the kind of the edge.
/// [`EdgeKind`] is contains supplementary information about the destination
/// scope, which is used for name resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct ScopeEdge<'db> {
    pub dest: ScopeId<'db>,
    pub kind: EdgeKind<'db>,
}

/// A specific edge property definitions.
///
/// NOTE: The internal types of each variants contains very small amount of
/// information, the reason why we need to prepare each internal types is to
/// allow us to implement traits to each edges directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub enum EdgeKind<'db> {
    /// An edge to a lexical parent scope.
    Lex(LexEdge),
    /// An edge to a module.
    Mod(ModEdge<'db>),
    /// An edge to a type.
    Type(TypeEdge<'db>),
    /// An edge to a trait.
    Trait(TraitEdge<'db>),
    /// An edge from a trait to an associated type.
    TraitType(TraitTypeEdge<'db>),
    /// An edge from a scope to a generic parameter.
    GenericParam(GenericParamEdge<'db>),
    /// An edge to a value. The value is either a function or a
    /// constant.
    Value(ValueEdge<'db>),
    /// An edge to a field definition scope.
    Field(FieldEdge<'db>),
    /// An edge to a enum variant definition scope.
    Variant(VariantEdge<'db>),
    /// An edge to a module that is referenced by a `super` keyword.
    Super(SuperEdge),
    /// An edge to an ingot that is referenced by a `ingot` keyword.
    Ingot(IngotEdge),
    /// An edge to a scope that is referenced by a `self` keyword.
    Self_(SelfEdge),
    /// An edge to a scope that is referenced by a `Self` keyword.
    SelfTy(SelfTyEdge),
    /// An edge to an anonymous scope, e.g., `impl` or function body.
    Anon(AnonEdge),
}

impl<'db> EdgeKind<'db> {
    pub fn lex() -> Self {
        EdgeKind::Lex(LexEdge())
    }

    pub fn mod_(ident: IdentId<'db>) -> Self {
        EdgeKind::Mod(ident.into())
    }

    pub fn type_(ident: IdentId<'db>) -> Self {
        EdgeKind::Type(ident.into())
    }

    pub fn trait_(ident: IdentId<'db>) -> Self {
        EdgeKind::Trait(ident.into())
    }

    pub fn generic_param(ident: IdentId<'db>) -> Self {
        EdgeKind::GenericParam(ident.into())
    }

    pub fn value(ident: IdentId<'db>) -> Self {
        EdgeKind::Value(ident.into())
    }

    pub fn field(ident: IdentId<'db>) -> Self {
        EdgeKind::Field(ident.into())
    }

    pub fn variant(ident: IdentId<'db>) -> Self {
        EdgeKind::Variant(ident.into())
    }

    pub fn super_() -> Self {
        EdgeKind::Super(SuperEdge())
    }

    pub fn ingot() -> Self {
        EdgeKind::Ingot(IngotEdge())
    }

    pub fn self_ty() -> Self {
        EdgeKind::SelfTy(SelfTyEdge())
    }

    pub fn self_() -> Self {
        EdgeKind::Self_(SelfEdge())
    }

    pub fn anon() -> Self {
        EdgeKind::Anon(AnonEdge())
    }

    pub fn trait_type(ident: IdentId<'db>) -> Self {
        EdgeKind::TraitType(ident.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LexEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct ModEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct TypeEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct TraitEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct TraitTypeEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct ValueEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct GenericParamEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct FieldEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Update)]
pub struct VariantEdge<'db>(pub IdentId<'db>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SuperEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IngotEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct SelfTyEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct SelfEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnonEdge();

pub trait FromScope<'db>: Sized {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self>;
}

impl<'db> FromScope<'db> for ItemKind<'db> {
    fn from_scope(scope: ScopeId<'db>, _db: &'db dyn HirDb) -> Option<Self> {
        match scope {
            ScopeId::Item(item) => Some(item),
            _ => None,
        }
    }
}

macro_rules! item_from_scope {
    ($($item_ty: ty,)*) => {
        $(
        impl<'db> FromScope<'db> for $item_ty {
            fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
                scope.resolve_to::<ItemKind>(db).and_then(|item| item.try_into().ok())
            }
        }
        )*
    };
}

item_from_scope! {
    TopLevelMod<'db>,
    Mod<'db>,
    Func<'db>,
    Struct<'db>,
    Contract<'db>,
    Enum<'db>,
    TypeAlias<'db>,
    Impl<'db>,
    Trait<'db>,
    ImplTrait<'db>,
    Const<'db>,
    Use<'db>,
    Body<'db>,
}

impl<'db> FromScope<'db> for &'db FieldDef<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::Field(parent, idx) = scope else {
            return None;
        };
        let idx = idx as usize;

        match parent {
            FieldParent::Struct(s) => Some(&s.fields(db).data(db)[idx]),
            FieldParent::Contract(c) => Some(&c.fields(db).data(db)[idx]),
            FieldParent::Variant(v) => match v.kind(db) {
                VariantKind::Record(fields) => Some(&fields.data(db)[idx]),
                _ => unreachable!(),
            },
        }
    }
}

impl<'db> FromScope<'db> for &'db VariantDef<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::Variant(v) = scope else {
            return None;
        };
        Some(v.def(db))
    }
}

impl<'db> FromScope<'db> for &'db FuncParam<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::FuncParam(parent, idx) = scope else {
            return None;
        };

        let func: Func = parent.try_into().unwrap();
        func.params_list(db)
            .to_opt()
            .map(|params| &params.data(db)[idx as usize])
    }
}

impl<'db> FromScope<'db> for &'db GenericParam<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::GenericParam(parent, idx) = scope else {
            return None;
        };

        let parent = GenericParamOwner::from_item_opt(parent).unwrap();
        Some(parent.param_view(db, idx as usize).param)
    }
}

impl<'db> FromScope<'db> for &'db AssocTyDecl<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::TraitType(t, idx) = scope else {
            return None;
        };
        Some(t.assoc_ty_by_index(db, idx as usize))
    }
}

impl<'db> FromScope<'db> for &'db AssocConstDecl<'db> {
    fn from_scope(scope: ScopeId<'db>, db: &'db dyn HirDb) -> Option<Self> {
        let ScopeId::TraitConst(t, idx) = scope else {
            return None;
        };
        Some(&t.consts(db)[idx as usize])
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        hir_def::{
            ItemKind,
            scope_graph::{FieldParent, ScopeId},
        },
        test_db::TestDb,
    };

    #[test]
    fn item_tree() {
        let mut db = TestDb::default();

        let text = r#"
            mod foo {
                fn bar() {}
                extern {
                    fn baz()
                }
            }

            enum MyEnum {}

            mod baz {
                struct MyS {}
            }
        "#;

        let file = db.standalone_file(text);
        let scope_graph = db.parse_source(file);
        let items: Vec<_> = scope_graph
            .items_dfs(&db)
            .filter(|item| !matches!(item, ItemKind::Use(use_) if use_.is_prelude_use(&db)))
            .collect();
        assert_eq!(items.len(), 8);

        for (i, item) in items.into_iter().enumerate() {
            match i {
                0 => assert!(matches!(item, ItemKind::TopMod(_))),
                1 => assert!(matches!(item, ItemKind::Mod(_))),
                2 => assert!(matches!(item, ItemKind::Func(_))),
                3 => assert!(matches!(item, ItemKind::Body(_))),
                4 => assert!(matches!(item, ItemKind::Func(_))),
                5 => assert!(matches!(item, ItemKind::Enum(_))),
                6 => assert!(matches!(item, ItemKind::Mod(_))),
                7 => assert!(matches!(item, ItemKind::Struct(_))),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn enum_record_fields() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo {
                X { a: i8, b: i8 },
            }
        "#;

        let file = db.standalone_file(text);
        let scope_graph = db.parse_source(file);
        let root = scope_graph.top_mod.scope();
        let enum_ = scope_graph
            .children(root)
            .find(|scope| !matches!(scope.item(), ItemKind::Use(use_) if use_.is_prelude_use(&db)))
            .unwrap();
        assert!(matches!(enum_.item(), ItemKind::Enum(_)));

        let variant = scope_graph.children(enum_).next().unwrap();
        assert!(matches!(variant, ScopeId::Variant(..)));

        let field = scope_graph.children(variant).next().unwrap();
        assert!(matches!(field, ScopeId::Field(FieldParent::Variant(..), _)));
    }

    #[test]
    fn msg_variant_fields() {
        let mut db = TestDb::default();

        // msg blocks are desugared into modules containing structs and impl MsgVariant
        let text = r#"
            msg Foo {
                Bar { a: u8, b: u8 } -> bool,
            }
        "#;

        let file = db.standalone_file(text);
        let scope_graph = db.parse_source(file);
        let root = scope_graph.top_mod.scope();

        // The msg block becomes a module with #[msg] attribute
        let msg_mod = scope_graph
            .children(root)
            .find(|scope| matches!(scope.item(), ItemKind::Mod(_)))
            .unwrap();
        assert!(matches!(msg_mod.item(), ItemKind::Mod(_)));

        // Skip the Use item (prelude) and find the struct
        let variant_struct = scope_graph
            .children(msg_mod)
            .find(|scope| matches!(scope.item(), ItemKind::Struct(_)))
            .unwrap();
        assert!(matches!(variant_struct.item(), ItemKind::Struct(_)));

        // Check that there's also an ImplTrait for MsgVariant
        let impl_trait = scope_graph
            .children(msg_mod)
            .find(|scope| matches!(scope.item(), ItemKind::ImplTrait(_)))
            .unwrap();
        assert!(matches!(impl_trait.item(), ItemKind::ImplTrait(_)));

        // Fields are now struct fields, not msg variant fields
        let field = scope_graph.children(variant_struct).next().unwrap();
        assert!(matches!(field, ScopeId::Field(FieldParent::Struct(..), _)));
    }
}
