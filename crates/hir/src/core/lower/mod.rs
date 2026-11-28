use common::{
    file::File,
    generation::ScopedDeriveDemand,
    ingot::{Ingot, IngotKind},
};
use num_bigint::BigUint;
use num_traits::Num;
use parser::{
    SyntaxNode, SyntaxToken,
    ast::{self, prelude::*},
};

use self::{item::lower_module_items, scope_builder::ScopeGraphBuilder};
use crate::{
    HirDb, LowerHirDb,
    hir_def::{
        AttrListId, AuthoredLocator, AuthoredScope, Body, ExprId, Func, HirIngot, IdentId,
        ImplTrait, IntegerId, ItemKind, LitKind, ModuleTree, Partial, PathId, StringId,
        TopLevelMod, TrackedItemId, TrackedItemVariant, Use, Visibility,
        authored::ScopedDeriveDemandId,
        module_tree_impl,
        scope_graph::{ScopeGraph, ScopeId},
        use_tree::{UsePathId, UsePathSegment},
    },
    span::HirOrigin,
};
pub use abi_field::{AbiFieldContext, AbiFieldDiagnostic, AbiFieldDiagnosticKind};
pub use abi_struct::{AbiStructDiagnostic, AbiStructDiagnosticKind};
pub use attr::{AttrMisuseError, AttrMisuseErrorKind};
pub use error::{ErrorDiagnostic, ErrorDiagnosticKind};
pub use event::{EventError, EventErrorKind};
pub use generated::GenerationLoweringError;
pub use item::{FieldModifierError, FieldModifierErrorKind, MsgDiagnostic, MsgDiagnosticKind};
pub use params::{VariadicParamError, VariadicParamErrorKind};
pub use parse::{ParsedFile, parse_file_impl};

pub(crate) mod parse;

mod abi_field;
mod abi_struct;
mod attr;
mod body;
mod contract;
mod error;
mod event;
mod expr;
mod generated;
mod hir_builder;
mod item;
mod msg;
mod params;
mod pat;
mod path;
mod payable;
mod scope_builder;
mod short_form;
mod stmt;
mod types;
mod use_tree;

// Signature names can exceed the supported inline-string width. Hashing accepts
// tuples of fragments, so split names without changing their concatenated bytes.
fn signature_name_chunks(name: &str) -> Vec<&str> {
    let mut chunks = Vec::new();
    let mut start = 0;
    for (offset, ch) in name.char_indices() {
        if offset + ch.len_utf8() - start > crate::analysis::ty::ty_def::MAX_INLINE_STRING_BYTES {
            chunks.push(&name[start..offset]);
            start = offset;
        }
    }
    chunks.push(&name[start..]);
    chunks
}

pub(super) fn lower_visibility(owner: &impl ItemModifierOwner) -> Visibility {
    if owner.pub_kw().is_none() {
        return Visibility::Private;
    }
    lower_vis_restriction(owner.vis_restriction())
}

/// Lowers visibility for a record field definition.
///
/// `RecordFieldDef` has `pub_kw()` and `vis_restriction()` as inherent methods
/// (it does not implement `ItemModifierOwner`), so this is a separate helper
/// from `lower_visibility`.
pub(super) fn lower_field_visibility(field: &ast::RecordFieldDef) -> Visibility {
    if field.pub_kw().is_none() {
        return Visibility::Private;
    }
    lower_vis_restriction(field.vis_restriction())
}

fn lower_vis_restriction(restriction: Option<ast::VisRestriction>) -> Visibility {
    match restriction {
        None => Visibility::Public,
        Some(restriction) => {
            if restriction.ingot_kw().is_some() {
                Visibility::PubIngot
            } else if restriction.super_kw().is_some() {
                Visibility::PubSuper
            } else {
                // pub(in path) — parsed but not yet supported.
                // Treat as private for safety (deny access rather than allow).
                Visibility::Private
            }
        }
    }
}

pub(super) fn lower_is_unsafe(owner: &impl ItemModifierOwner) -> bool {
    owner.unsafe_kw().is_some()
}

/// Maps the given file to a top-level module.
/// This function just maps the file to a top-level module, and doesn't perform
/// any parsing or lowering.
/// To perform the actual lowering, use [`scope_graph`] instead.
pub fn map_file_to_mod(db: &dyn LowerHirDb, file: File) -> TopLevelMod<'_> {
    map_file_to_mod_impl(db, file)
}

/// Returns the scope graph of the given top-level module.
pub fn scope_graph<'db>(
    db: &'db dyn LowerHirDb,
    top_mod: TopLevelMod<'db>,
) -> &'db ScopeGraph<'db> {
    linked_scope_graph(db, top_mod)
}

/// The scope graph of `top_mod` with every generated item's lexical edge to a
/// producer in another module. Lowering leaves those edges out, so that no
/// module's lowering reads another module's lowered graph; linking reads the
/// producer modules' lowered graphs only, which never wait for this module.
pub(crate) fn linked_scope_graph<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> &'db ScopeGraph<'db> {
    linked_scope_graph_impl(db, top_mod)
        .as_ref()
        .unwrap_or_else(|| scope_graph_impl(db, top_mod))
}

#[salsa::tracked]
fn linked_scope_graph_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> Option<ScopeGraph<'db>> {
    let lowered = scope_graph_impl(db, top_mod);
    if lowered.foreign_origins.is_empty() {
        return None;
    }
    let mut linked = lowered.clone();
    for (item, producer) in std::mem::take(&mut linked.foreign_origins) {
        let edge = crate::hir_def::scope_graph::ScopeEdge {
            dest: resolve_authored_scope(db, producer),
            kind: crate::hir_def::scope_graph::EdgeKind::lex(),
        };
        let scope = linked
            .scopes
            .get_mut(&item)
            .expect("a generated item has a scope in its module");
        // External edges precede the item's own edges, as lowering orders them.
        scope.edges = std::iter::once(edge)
            .chain(std::mem::take(&mut scope.edges))
            .collect();
    }
    Some(linked)
}

/// Bind an authored reference in its module's lowered scope graph. The
/// referenced declaration was validated when its request was frozen and its
/// source is unchanged in this view.
#[salsa::tracked(returns(copy))]
pub(crate) fn resolve_authored_scope<'db>(
    db: &'db dyn HirDb,
    reference: AuthoredScope<'db>,
) -> ScopeId<'db> {
    let top_mod = reference.top_mod(db);
    let graph = scope_graph_impl(db, top_mod);
    match reference.locator(db) {
        AuthoredLocator::Producer { module, block } => scope_builder::find_producer_scope(
            db,
            top_mod,
            graph.scopes.keys().copied(),
            module,
            block,
        ),
        AuthoredLocator::Item(range) => {
            let written_at = |origin: Option<ast::SyntaxNodePtr>| {
                origin.is_some_and(|ptr| ptr.text_range() == range)
            };
            let root = top_mod_ast(db, top_mod);
            graph
                .scopes
                .keys()
                .copied()
                .find(|scope| match *scope {
                    ScopeId::Item(ItemKind::Struct(s)) => {
                        s.origin(db).authored_range(root.syntax()) == Some(range)
                    }
                    ScopeId::Item(ItemKind::Enum(e)) => written_at(raw_origin(e.origin(db))),
                    ScopeId::Item(ItemKind::Trait(t)) => written_at(raw_origin(t.origin(db))),
                    _ => false,
                })
                .expect("validated authored declaration exists in unchanged source")
        }
        AuthoredLocator::GenericParam { item, index } => {
            let header = graph
                .scopes
                .keys()
                .find_map(|scope| match *scope {
                    ScopeId::Item(header @ ItemKind::ImplTrait(implementation))
                        if raw_origin(implementation.origin(db))
                            .is_some_and(|ptr| ptr.text_range() == item) =>
                    {
                        Some(header)
                    }
                    ScopeId::Item(header @ ItemKind::Impl(implementation))
                        if raw_origin(implementation.origin(db))
                            .is_some_and(|ptr| ptr.text_range() == item) =>
                    {
                        Some(header)
                    }
                    _ => None,
                })
                .expect("the derive header being lowered is authored in its module");
            ScopeId::GenericParam(header, index)
        }
    }
}

/// The declared type of an authored record's field as written: names bound in
/// the record's scope, its generic parameters left as bare names. Type lowering
/// does not read this type; it answers questions about the written syntax.
#[salsa::tracked(returns(copy))]
pub(crate) fn authored_field_type<'db>(
    db: &'db dyn HirDb,
    field: crate::hir_def::AuthoredFieldType<'db>,
) -> Option<crate::hir_def::TypeId<'db>> {
    use crate::hir_def::VariantKind;
    let record = field.record(db);
    let item = record.resolve(db).to_item()?;
    let index = field.index(db);
    let ty = match (item, field.variant(db)) {
        (ItemKind::Struct(s), None) => s.fields(db).data(db).get(index)?.type_ref.to_opt(),
        (ItemKind::Enum(e), Some(variant)) => {
            match &e.variants_list(db).data(db).get(variant)?.kind {
                VariantKind::Tuple(elems) => elems.data(db).get(index)?.to_opt(),
                VariantKind::Record(fields) => fields.data(db).get(index)?.type_ref.to_opt(),
                VariantKind::Unit => None,
            }
        }
        _ => None,
    }?;
    Some(types::bind_type_to_scope(db, ty, record, item))
}

fn raw_origin<T: AstNode<Language = parser::FeLang>>(
    origin: &HirOrigin<T>,
) -> Option<ast::SyntaxNodePtr> {
    match origin {
        HirOrigin::Raw(ptr) => Some(ptr.syntax_node_ptr()),
        _ => None,
    }
}

pub fn generation_lowering_errors<'db>(
    db: &'db dyn LowerHirDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<GenerationLoweringError> {
    scope_graph_impl(db, top_mod)
        .lower_diagnostics
        .iter()
        .filter_map(|diagnostic| match diagnostic {
            LowerDiagnostic::Generation(error) => Some(error.clone()),
            _ => None,
        })
        .collect()
}

/// Finds the one prepared item installed for this frozen demand.
pub fn prepared_scoped_item<'db>(
    db: &'db dyn HirDb,
    demand: &ScopedDeriveDemand,
) -> Option<ImplTrait<'db>> {
    let demand = ScopedDeriveDemandId::new(db, demand.clone());
    prepared_scoped_item_impl(db, demand)
}

#[salsa::tracked(returns(copy))]
fn prepared_scoped_item_impl<'db>(
    db: &'db dyn HirDb,
    demand: ScopedDeriveDemandId<'db>,
) -> Option<ImplTrait<'db>> {
    let file = db.workspace().get(db, &demand.demand(db).opening.file)?;
    let module = map_file_to_mod_impl(db, file);
    let mut items = scope_graph_impl(db, module)
        .scoped_implementations
        .iter()
        .filter_map(|(key, item)| (*key == demand).then_some(*item));
    let item = items.next()?;
    items.next().is_none().then_some(item)
}

/// Returns the ingot module tree of the given ingot.
pub fn module_tree<'db>(db: &'db dyn LowerHirDb, ingot: Ingot<'db>) -> &'db ModuleTree<'db> {
    module_tree_impl(db, ingot)
}

#[salsa::tracked(returns(copy))]
pub(crate) fn map_file_to_mod_impl<'db>(db: &'db dyn HirDb, file: File) -> TopLevelMod<'db> {
    let path = file.path(db);
    let path = path.as_ref().unwrap_or_else(|| {
        panic!(
            "File path is not valid: {:?}, containing ingot: {:?}, file url: {:?}",
            path,
            file.containing_ingot(db),
            // .expect("should have ingot")
            file.url(db)
        )
    });
    let name = path.file_stem().unwrap();
    let mod_name = IdentId::new(db, name.to_string());
    TopLevelMod::new(db, mod_name, file)
}

#[salsa::tracked]
pub(crate) fn scope_graph_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> ScopeGraph<'db> {
    let ast = top_mod_ast(db, top_mod);
    let mut ctxt = FileLowerCtxt::enter_top_mod(db, top_mod);

    ctxt.insert_synthetic_prelude_use();

    if let Some(items) = ast.items() {
        item::validate_module_inner_attrs(&mut ctxt, items.inner_attr_list());
        if enclosing_cfg_holds_impl(db, top_mod) {
            lower_module_items(&mut ctxt, items);
        }
    }
    generated::append_functions(&mut ctxt);
    ctxt.builder.apply_quoted_function_origins();
    ctxt.leave_item_scope(top_mod);

    ctxt.build()
}

/// Whether the `#![cfg(..)]` attributes of every file module enclosing
/// `top_mod` hold, so that a module's `cfg` drops the modules in its
/// directory along with its own items.
#[salsa::tracked(returns(copy))]
pub(crate) fn enclosing_cfg_holds_impl<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> bool {
    top_mod.parent(db).is_none_or(|parent| {
        enclosing_cfg_holds_impl(db, parent)
            && attr::named_attr_specs(
                top_mod_ast(db, parent)
                    .items()
                    .and_then(|items| items.inner_attr_list()),
                "cfg",
            )
            .iter()
            .all(|spec| attr::cfg_spec_holds(db, spec) == Some(true))
    })
}

/// Whether lowering keeps the item whose syntax is `item`, a node of the file
/// of `top_mod`: the `cfg` attributes of the item and of every item around
/// it, the `#![cfg]` attributes of every item list around it, and those of
/// the file modules around `top_mod` hold.
pub fn syntax_item_cfg_holds(db: &dyn HirDb, top_mod: TopLevelMod<'_>, item: &SyntaxNode) -> bool {
    let holds = |attrs: Option<ast::AttrList>| {
        attr::named_attr_specs(attrs, "cfg")
            .iter()
            .all(|spec| attr::cfg_spec_holds(db, spec) == Some(true))
    };
    enclosing_cfg_holds_impl(db, top_mod)
        && item.ancestors().all(|node| {
            if let Some(item) = ast::Item::cast(node.clone()) {
                holds(item.attr_list())
            } else if let Some(items) = ast::ItemList::cast(node) {
                holds(items.inner_attr_list())
            } else {
                true
            }
        })
}

/// Returns the scope graph of the body of `func`, lowered on its own, or `None`
/// if the function has no body in source or its body was lowered with the
/// module because it declares items.
#[salsa::tracked]
pub(crate) fn func_body_graph_impl<'db>(
    db: &'db dyn HirDb,
    func: Func<'db>,
) -> Option<ScopeGraph<'db>> {
    let (None, HirOrigin::Raw(ptr)) = (func.eager_body(db), func.origin(db)) else {
        return None;
    };
    let ast = ptr.to_node(top_mod_ast(db, func.top_mod(db)).syntax());
    let body = ast.body()?;
    let mut ctxt = FileLowerCtxt::enter_func_body(db, func);
    Body::lower_func_body(&mut ctxt, &ast, body, Vec::new());
    Some(ctxt.build())
}

#[salsa::tracked(returns(copy))]
pub(crate) fn top_mod_attributes_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> AttrListId<'db> {
    let ast = top_mod_ast(db, top_mod);
    let mut ctxt = FileLowerCtxt::enter_top_mod(db, top_mod);
    AttrListId::lower_ast_opt(
        &mut ctxt,
        ast.items().and_then(|items| items.inner_attr_list()),
    )
}

pub(crate) fn top_mod_ast(db: &dyn HirDb, top_mod: TopLevelMod) -> ast::Root {
    let node = SyntaxNode::new_root(parse_file_impl(db, top_mod).root.clone());
    // This cast never fails even if the file content is empty.
    ast::Root::cast(node).unwrap()
}

/// A diagnostic reported while lowering a module or a function body.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum LowerDiagnostic {
    Msg(MsgDiagnostic),
    FieldModifier(FieldModifierError),
    Event(EventError),
    Error(ErrorDiagnostic),
    AbiStruct(AbiStructDiagnostic),
    AbiField(AbiFieldDiagnostic),
    AttrMisuse(AttrMisuseError),
    VariadicParam(VariadicParamError),
    Generation(GenerationLoweringError),
}

pub(super) struct FileLowerCtxt<'db> {
    builder: ScopeGraphBuilder<'db>,
    next_impl_idx: u32,
    next_impl_trait_idx: u32,
    next_static_assert_idx: u32,
    next_with_idx: u32,
    diagnostics: Vec<LowerDiagnostic>,
    next_quote_frame_id: usize,
    pub(super) quote_frames: Vec<generated::ActiveQuoteFrame<'db>>,
    /// The derive header, trait or inherent, whose members are being lowered:
    /// the only binders a frozen type may name.
    pub(super) derive_header: Option<parser::TextRange>,
}

/// An authored record named by a frozen reference: its reference, bound by
/// ordinary resolution, and the syntax lowering reads names and shapes from.
pub(super) struct AuthoredRecord<'db> {
    pub(super) scope: AuthoredScope<'db>,
    pub(super) syntax: RecordSyntax,
}

pub(super) enum RecordSyntax {
    Struct(ast::Struct),
    Enum(ast::Enum),
    /// A `msg` variant, whose struct has the variant's parameters as fields.
    MsgVariant(ast::MsgVariant),
}

/// The fields of a struct or of one enum variant, named as lowering names
/// them: tuple struct fields by position, tuple variant fields by count.
pub(super) enum AuthoredFields<'db> {
    Unit,
    Tuple(usize),
    Named(Vec<Partial<IdentId<'db>>>),
}

impl<'db> AuthoredRecord<'db> {
    pub(super) fn name(&self, db: &'db dyn HirDb) -> Partial<IdentId<'db>> {
        let name = match &self.syntax {
            RecordSyntax::Struct(s) => s.name(),
            RecordSyntax::Enum(e) => e.name(),
            RecordSyntax::MsgVariant(v) => v.name(),
        };
        name.map(|name| IdentId::new(db, name.text().to_string()))
            .into()
    }

    pub(super) fn generic_params(&self) -> Option<ast::GenericParamList> {
        match &self.syntax {
            RecordSyntax::Struct(s) => s.generic_params(),
            RecordSyntax::Enum(e) => e.generic_params(),
            RecordSyntax::MsgVariant(_) => None,
        }
    }

    pub(super) fn variant(&self, index: usize) -> Option<ast::VariantDef> {
        let RecordSyntax::Enum(e) = &self.syntax else {
            return None;
        };
        e.variants()?.into_iter().nth(index)
    }

    /// Whether the record is a struct declared with positional fields.
    pub(super) fn is_tuple_struct(&self) -> bool {
        matches!(&self.syntax, RecordSyntax::Struct(s)
            if s.fields().is_some_and(|fields| fields.is_tuple()))
    }

    /// The struct's fields, or variant `variant`'s; `None` when the reference
    /// does not name fields of this record.
    pub(super) fn fields(
        &self,
        db: &'db dyn HirDb,
        variant: Option<usize>,
    ) -> Option<AuthoredFields<'db>> {
        let labels = |fields: Vec<ast::RecordFieldDef>, is_tuple: bool| {
            AuthoredFields::Named(
                fields
                    .into_iter()
                    .enumerate()
                    .map(|(index, field)| {
                        if is_tuple {
                            Partial::Present(IdentId::new(db, index.to_string()))
                        } else {
                            field
                                .name()
                                .map(|name| IdentId::new(db, name.text().to_string()))
                                .into()
                        }
                    })
                    .collect(),
            )
        };
        let named = |list: Option<ast::RecordFieldDefList>| {
            let Some(list) = list else {
                return AuthoredFields::Named(Vec::new());
            };
            let is_tuple = list.is_tuple();
            labels(list.into_iter().collect(), is_tuple)
        };
        match (&self.syntax, variant) {
            (RecordSyntax::Struct(s), None) => Some(named(s.fields())),
            (RecordSyntax::MsgVariant(v), None) => {
                Some(labels(v.params().into_iter().flatten().collect(), false))
            }
            (RecordSyntax::Enum(_), Some(index)) => Some(match self.variant(index)?.kind() {
                ast::VariantKind::Unit => AuthoredFields::Unit,
                ast::VariantKind::Tuple(tuple) => AuthoredFields::Tuple(tuple.into_iter().count()),
                ast::VariantKind::Record(fields) => named(Some(fields)),
            }),
            _ => None,
        }
    }

    /// Whether field `index` of the struct, or of variant `variant`, declares
    /// a type.
    pub(super) fn field_has_type(&self, variant: Option<usize>, index: usize) -> bool {
        match (&self.syntax, variant) {
            (RecordSyntax::Struct(s), None) => s
                .fields()
                .and_then(|fields| fields.into_iter().nth(index))
                .is_some_and(|field| field.ty().is_some()),
            (RecordSyntax::MsgVariant(v), None) => v
                .params()
                .and_then(|params| params.into_iter().nth(index))
                .is_some_and(|field| field.ty().is_some()),
            (RecordSyntax::Enum(_), Some(v)) => match self.variant(v).map(|v| v.kind()) {
                Some(ast::VariantKind::Tuple(tuple)) => tuple.into_iter().nth(index).is_some(),
                Some(ast::VariantKind::Record(fields)) => fields
                    .into_iter()
                    .nth(index)
                    .is_some_and(|field| field.ty().is_some()),
                _ => false,
            },
            _ => false,
        }
    }

    /// A path naming the record itself, bound when it is resolved.
    pub(super) fn path(
        &self,
        db: &'db dyn HirDb,
        generic_args: crate::hir_def::GenericArgListId<'db>,
    ) -> PathId<'db> {
        let display = PathId::new(
            db,
            crate::hir_def::PathKind::Ident {
                ident: self.name(db),
                generic_args,
            },
            None,
        );
        PathId::new(
            db,
            crate::hir_def::PathKind::BoundItem {
                item: self.scope,
                display,
            },
            None,
        )
    }
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_top_mod(db, top_mod),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_static_assert_idx: 0,
            next_with_idx: 0,
            diagnostics: Vec::new(),
            next_quote_frame_id: 0,
            quote_frames: Vec::new(),
            derive_header: None,
        }
    }

    fn enter_func_body(db: &'db dyn HirDb, func: Func<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_func_body(db, func),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_static_assert_idx: 0,
            next_with_idx: 0,
            diagnostics: Vec::new(),
            next_quote_frame_id: 0,
            quote_frames: Vec::new(),
            derive_header: None,
        }
    }

    pub(super) fn next_quote_frame_id(&mut self) -> usize {
        let id = self.next_quote_frame_id;
        self.next_quote_frame_id += 1;
        id
    }

    pub(super) fn resolve_sub_quote_local_ident(&self, text: &str) -> IdentId<'db> {
        let db = self.db();
        if let Some(frame) = self.quote_frames.last()
            && frame.is_sub_quote
            && !frame.open_names.iter().any(|(name, _)| name == text)
        {
            return IdentId::new(db, format!("\0q{}_{text}", frame.frame_id));
        }
        IdentId::new(db, text.to_string())
    }

    pub(super) fn record_bound_name(&mut self, name: &str) {
        for frame in self.quote_frames.iter_mut().rev() {
            frame.bound_names.insert(name.to_string());
            if !frame.is_sub_quote {
                break;
            }
        }
    }

    pub(super) fn pop_quote_frame(&mut self) {
        let Some(frame) = self.quote_frames.pop() else {
            return;
        };
        if !frame.is_sub_quote {
            let db = self.db();
            let file = frame.quote_top_mod.file(db);
            for (name, range) in frame.open_names {
                if !frame.bound_names.contains(&name) {
                    self.file_scoped_error(
                        file,
                        range,
                        format!("quoted open name `{name}` has no enclosing binder"),
                    );
                }
            }
        }
    }

    pub(super) fn current_hole(
        &self,
        range: parser::TextRange,
    ) -> Option<common::generation::FrozenHoleValue> {
        self.quote_frames
            .last()?
            .holes
            .iter()
            .find(|(r, _)| *r == range)
            .map(|(_, value)| value.clone())
    }

    /// Mark an item generated into an authored declaration as generated, like
    /// every other generated item: its quote's producer is its lexical origin
    /// and the enclosing declaration its publication parent.
    pub(super) fn defer_generated_member_origin(&mut self, item: ItemKind<'db>) {
        let producer = self
            .quote_frames
            .last()
            .expect("a generated member is lowered inside its quote")
            .producer;
        self.builder.defer_quoted_item_origin(item, producer);
    }

    /// The file and line of the active quotation, for messages about what it
    /// wrote; this file's start outside generation.
    pub(super) fn current_quote_location(&self) -> String {
        let db = self.db();
        let (file, range) = self.quote_frames.last().map_or_else(
            || (self.top_mod().file(db), parser::TextRange::empty(0.into())),
            |frame| (frame.quote_top_mod.file(db), frame.quote_range),
        );
        source_location(db, file, range)
    }

    /// The module whose syntax is being lowered: the active quotation's, or
    /// this file's outside generation.
    pub(super) fn syntax_top_mod(&self) -> TopLevelMod<'db> {
        self.quote_frames
            .last()
            .map_or_else(|| self.top_mod(), |frame| frame.quote_top_mod)
    }

    /// The modules of the active quotations, innermost first: the one that
    /// wrote the code being lowered, then each one that spliced a fragment
    /// around it, the outermost last. Empty outside generation.
    pub(super) fn quote_chain(&self) -> Vec<TopLevelMod<'db>> {
        self.quote_frames
            .iter()
            .rev()
            .map(|frame| frame.quote_top_mod)
            .collect()
    }

    /// Where a member spliced from the active quotation was written.
    pub(super) fn generated_member_syntax(
        &self,
        node: &parser::SyntaxNode,
    ) -> crate::hir_def::GeneratedMemberSyntax<'db> {
        crate::hir_def::GeneratedMemberSyntax {
            top_mod: self.syntax_top_mod(),
            node: parser::ast::SyntaxNodePtr::new(node),
        }
    }

    /// Whether the free names of the active quotation resolve in the scope
    /// where the quotation was written, as they do for a fragment spliced into
    /// a body and for the members of a spliced template.
    pub(super) fn quote_names_resolve_at_producer(&self) -> bool {
        self.quote_frames
            .last()
            .is_some_and(|frame| frame.is_sub_quote || frame.is_member_quote)
    }

    pub(super) fn quote_producer_in_other_ingot(
        &self,
    ) -> Option<crate::hir_def::AuthoredScope<'db>> {
        let frame = self.quote_frames.last()?;
        let db = self.db();
        let quote_ingot = frame.producer.ingot(db);
        (quote_ingot != self.top_mod().ingot(db)).then_some(frame.producer)
    }

    /// The module and the syntax of the quotation that a frozen template names.
    pub(super) fn quote_template_syntax(
        &self,
        quote_file: &url::Url,
        quote_range: parser::TextRange,
    ) -> (TopLevelMod<'db>, ast::QuoteExpr) {
        let db = self.db();
        let file = db
            .workspace()
            .get(db, quote_file)
            .expect("validated frozen quote file exists in workspace");
        let quote_top_mod = map_file_to_mod_impl(db, file);
        let quote = top_mod_ast(db, quote_top_mod)
            .syntax()
            .descendants()
            .filter_map(ast::QuoteExpr::cast)
            .find(|quote| quote.syntax().text_range() == quote_range)
            .expect("validated frozen quote belongs to unchanged input source");
        (quote_top_mod, quote)
    }

    pub(super) fn resolve_quote_template(
        &mut self,
        quote_file: &url::Url,
        quote_range: parser::TextRange,
        producer_module: Option<parser::TextRange>,
        producer_block: Option<parser::TextRange>,
        is_sub_quote: bool,
        holes: Vec<common::generation::FrozenHoleValue>,
    ) -> (ast::QuoteExpr, generated::ActiveQuoteFrame<'db>) {
        let db = self.db();
        let (quote_top_mod, quote) = self.quote_template_syntax(quote_file, quote_range);
        let holes = crate::core::meta::quote_holes(&quote)
            .expect("checked frozen quote retains valid bounded syntax")
            .into_iter()
            .map(|hole| hole.syntax().text_range())
            .zip(holes)
            .collect();
        let open_names = quote
            .open_list()
            .map(|list| {
                list.names()
                    .map(|n| (n.text().to_string(), n.text_range()))
                    .collect()
            })
            .unwrap_or_default();
        let producer = AuthoredScope::producer(db, quote_top_mod, producer_module, producer_block);
        let frame_id = self.next_quote_frame_id();
        (
            quote,
            generated::ActiveQuoteFrame {
                frame_id,
                quote_top_mod,
                quote_range,
                producer,
                is_sub_quote,
                is_member_quote: false,
                open_names,
                bound_names: rustc_hash::FxHashSet::default(),
                destination_names: rustc_hash::FxHashSet::default(),
                holes,
            },
        )
    }

    /// The authored record a frozen reference names, read from its file's
    /// syntax. Lowering takes the record's names and shapes from here, not
    /// from its lowered HIR, which may depend on this module's own output.
    pub(super) fn authored_record(
        &self,
        record_ref: &common::generation::FrozenRecordRef,
    ) -> AuthoredRecord<'db> {
        self.authored_record_at(&record_ref.target_file, record_ref.target_range)
    }

    fn authored_record_at(&self, file: &url::Url, range: parser::TextRange) -> AuthoredRecord<'db> {
        let db = self.db();
        let file = db
            .workspace()
            .get(db, file)
            .expect("validated frozen record file exists in workspace");
        let top_mod = map_file_to_mod_impl(db, file);
        let syntax = top_mod_ast(db, top_mod)
            .syntax()
            .descendants()
            .filter(|node| node.text_range() == range)
            .find_map(|node| {
                ast::Struct::cast(node.clone())
                    .map(RecordSyntax::Struct)
                    .or_else(|| ast::Enum::cast(node.clone()).map(RecordSyntax::Enum))
                    .or_else(|| ast::MsgVariant::cast(node).map(RecordSyntax::MsgVariant))
            })
            .expect("validated authored record item exists in target module");
        AuthoredRecord {
            scope: AuthoredScope::item(db, top_mod, range),
            syntax,
        }
    }

    /// A path naming the authored type declaration `decl`, bound when it is
    /// resolved.
    pub(super) fn authored_decl_path(
        &self,
        decl: &common::generation::FrozenDeclRef,
        generic_args: crate::hir_def::GenericArgListId<'db>,
    ) -> PathId<'db> {
        self.authored_record_at(&decl.file, decl.range)
            .path(self.db(), generic_args)
    }

    /// A path naming parameter `index` of the derive header (trait or inherent)
    /// being lowered, bound when it is resolved. Generated members resolve free
    /// names in their producer, so a header binder is never named by spelling.
    pub(super) fn header_param_path(&mut self, index: usize) -> Partial<PathId<'db>> {
        let db = self.db();
        let Some(header) = self.derive_header else {
            self.record_generation_error(
                parser::TextRange::empty(0.into()),
                "a frozen type names a derive header parameter outside its header",
            );
            return Partial::Absent;
        };
        let name = top_mod_ast(db, self.top_mod())
            .syntax()
            .descendants()
            .filter(|node| node.text_range() == header)
            .find_map(|node| {
                ast::ImplTrait::cast(node.clone())
                    .map(|header| header.generic_params())
                    .or_else(|| ast::Impl::cast(node).map(|header| header.generic_params()))
            })
            .flatten()
            .and_then(|params| params.into_iter().nth(index))
            .and_then(|param| match param.kind() {
                ast::GenericParamKind::Type(p) => p.name(),
                ast::GenericParamKind::Const(p) => p.name(),
                ast::GenericParamKind::Shape(p) => p.name(),
            })
            .map(|name| IdentId::new(db, name.text().to_string()));
        let Some(name) = name else {
            self.record_generation_error(
                header,
                "invalid derive header parameter in a frozen type",
            );
            return Partial::Absent;
        };
        let scope = AuthoredScope::new(
            db,
            self.top_mod(),
            AuthoredLocator::GenericParam {
                item: header,
                index: index as u16,
            },
        );
        Partial::Present(PathId::new(
            db,
            crate::hir_def::PathKind::BoundItem {
                item: scope,
                display: PathId::from_ident(db, name),
            },
            None,
        ))
    }

    /// The record's lowered item, when it is declared in this module and was
    /// lowered already: its generic header is copied, not only named.
    pub(super) fn local_record_item(
        &self,
        record_ref: &common::generation::FrozenRecordRef,
    ) -> Option<ItemKind<'db>> {
        let db = self.db();
        let file = db.workspace().get(db, &record_ref.target_file)?;
        (map_file_to_mod_impl(db, file) == self.top_mod())
            .then(|| self.builder.find_item_by_range(record_ref.target_range))
            .flatten()
    }

    /// The authored trait a frozen reference names, and its name.
    pub(super) fn authored_trait(
        &self,
        reference: &common::generation::FrozenTraitRef,
    ) -> (AuthoredScope<'db>, Partial<IdentId<'db>>) {
        let db = self.db();
        let file = db
            .workspace()
            .get(db, &reference.file)
            .expect("validated frozen trait file exists in workspace");
        let top_mod = map_file_to_mod_impl(db, file);
        let trait_ = top_mod_ast(db, top_mod)
            .syntax()
            .descendants()
            .filter_map(ast::Trait::cast)
            .find(|trait_| trait_.syntax().text_range() == reference.range)
            .expect("validated trait exists in unchanged source");
        (
            AuthoredScope::item(db, top_mod, reference.range),
            trait_
                .name()
                .map(|name| IdentId::new(db, name.text().to_string()))
                .into(),
        )
    }

    pub(super) fn lower_field_hole_ident(
        &mut self,
        range: parser::TextRange,
    ) -> Partial<IdentId<'db>> {
        let Some(common::generation::FrozenHoleValue::Field(field_ref)) = self.current_hole(range)
        else {
            self.record_generation_error(
                range,
                "record field label hole requires a Field<T, F> splice",
            );
            return Partial::Absent;
        };
        let db = self.db();
        let record = self.authored_record(&field_ref.record);
        match record.fields(db, field_ref.record.variant_index) {
            Some(AuthoredFields::Named(names)) => match names.get(field_ref.index) {
                Some(name) => *name,
                None => {
                    self.record_generation_error(
                        range,
                        "invalid field splice in record field label",
                    );
                    Partial::Absent
                }
            },
            Some(AuthoredFields::Tuple(_)) => {
                self.record_generation_error(
                    range,
                    "tuple variant field cannot be used as a named record field label",
                );
                Partial::Absent
            }
            _ => {
                self.record_generation_error(range, "invalid field splice in record field label");
                Partial::Absent
            }
        }
    }

    pub(super) fn lower_field_hole_index(
        &mut self,
        range: parser::TextRange,
    ) -> Partial<crate::hir_def::expr::FieldIndex<'db>> {
        use crate::hir_def::expr::FieldIndex;
        let Some(common::generation::FrozenHoleValue::Field(field_ref)) = self.current_hole(range)
        else {
            self.record_generation_error(range, "field access hole requires a Field<T, F> splice");
            return Partial::Absent;
        };
        let db = self.db();
        let record = self.authored_record(&field_ref.record);
        let res: Partial<FieldIndex<'db>> =
            match record.fields(db, field_ref.record.variant_index) {
                Some(AuthoredFields::Named(names)) => names
                    .get(field_ref.index)
                    .and_then(|name| name.to_opt())
                    .map(FieldIndex::Ident)
                    .into(),
                Some(AuthoredFields::Tuple(_)) => Partial::Present(FieldIndex::Index(
                    IntegerId::new(db, BigUint::from(field_ref.index)),
                )),
                _ => Partial::Absent,
            };
        if res.to_opt().is_none() {
            self.record_generation_error(range, "invalid field splice in field projection");
        }
        res
    }

    pub(super) fn record_generation_error(
        &mut self,
        range: parser::TextRange,
        message: impl Into<String>,
    ) {
        let db = self.db();
        let file = self.quote_frames.last().map_or_else(
            || self.top_mod().file(db),
            |frame| frame.quote_top_mod.file(db),
        );
        self.file_scoped_error(file, range, message);
    }

    pub(super) fn file_scoped_error(
        &mut self,
        file: common::file::File,
        range: parser::TextRange,
        message: impl Into<String>,
    ) {
        self.report(GenerationLoweringError {
            file,
            primary_range: range,
            message: message.into(),
        });
    }
}

pub(crate) fn source_location(
    db: &dyn HirDb,
    file: common::file::File,
    range: parser::TextRange,
) -> String {
    let text = file.text(db);
    let offset = usize::from(range.start()).min(text.len());
    let line = text[..offset].bytes().filter(|byte| *byte == b'\n').count() + 1;
    let path = file
        .path(db)
        .as_ref()
        .map(ToString::to_string)
        .or_else(|| file.url(db).map(|url| url.to_string()))
        .unwrap_or_else(|| "<source>".to_string());
    format!("{path}:{line}")
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn build(self) -> ScopeGraph<'db> {
        self.builder.build(self.diagnostics)
    }

    pub(super) fn report(&mut self, diagnostic: impl Into<LowerDiagnostic>) {
        self.diagnostics.push(diagnostic.into());
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.builder.db
    }

    pub(super) fn top_mod(&self) -> TopLevelMod<'db> {
        self.builder.top_mod
    }

    pub(super) fn lazy_func(&self) -> Option<Func<'db>> {
        self.builder.lazy_func
    }

    /// The path naming the builtin ingot `name` of `kind`, `core` or `std`,
    /// from the file: `ingot` within that ingot, and otherwise the root of
    /// the dependency of that kind, bound where the path is written. Bound,
    /// it does not depend on what a module of the file declares: a user
    /// `mod std` cannot take the place of the library. Without such a
    /// dependency the path is the plain name, which then fails to resolve.
    pub(super) fn builtin_ingot_path(&self, kind: IngotKind, name: &str) -> PathId<'db> {
        let db = self.db();
        let own = self.top_mod().ingot(db);
        if own.kind(db) == kind {
            return PathId::from_ident(db, IdentId::make_ingot(db));
        }
        let display = IdentId::new(db, name.to_string());
        let library = own
            .resolved_external_ingots(db)
            .iter()
            .find_map(|(dependency, ingot)| {
                (*dependency == display && ingot.kind(db) == kind).then_some(*ingot)
            });
        match library {
            Some(library) => PathId::new(
                db,
                crate::hir_def::PathKind::BoundIngot {
                    scope: AuthoredScope::producer(db, library.root_mod(db), None, None),
                    display,
                },
                None,
            ),
            None => PathId::from_ident(db, display),
        }
    }

    pub(super) fn insert_synthetic_prelude_use(&mut self) {
        let db = self.db();
        let top_mod = self.top_mod();
        let ingot = top_mod.ingot(db);
        let kind = ingot.kind(db);

        if kind == IngotKind::Core {
            return;
        }

        // Always inject core::prelude::* as the baseline — this is guaranteed
        // to exist and provides Option, Result, Clone, Copy, etc.
        let core = IdentId::new(db, "core".to_string());
        let prelude = IdentId::new(db, "prelude".to_string());
        self.insert_synthetic_use(vec![core, prelude]);

        // For non-std ingots, additionally inject std::prelude::* which adds
        // EVM/ABI items (Evm, Address, Call, Sol, revert, etc.) on top of
        // core::prelude. If std lacks a prelude module (e.g. a user-defined
        // package aliased as "std"), the synthetic import fails silently and
        // we still have core::prelude as fallback.
        if kind != IngotKind::Std {
            let std = IdentId::new(db, "std".to_string());
            let prelude = IdentId::new(db, "prelude".to_string());
            self.insert_synthetic_use(vec![std, prelude]);
        }
    }

    /// Inserts `use super::*` to re-export parent module items into current scope.
    pub(super) fn insert_synthetic_super_use(&mut self) {
        let db = self.db();

        let super_ident = IdentId::new(db, "super".to_string());

        let segs = vec![
            Partial::Present(UsePathSegment::Ident(super_ident)),
            Partial::Present(UsePathSegment::Glob),
        ];
        let path = Partial::Present(UsePathId::new(db, segs, None, None));

        let id = self.joined_id(TrackedItemVariant::Use(path));
        self.enter_item_scope(id, false);

        let top_mod = self.top_mod();
        let origin = HirOrigin::synthetic();
        let attrs = AttrListId::new(db, vec![]);
        let use_ = Use::new(
            db,
            id,
            attrs,
            path,
            None,
            Visibility::Private,
            top_mod,
            origin,
        );
        self.leave_item_scope(use_);
    }

    fn insert_synthetic_use(&mut self, segments: Vec<IdentId<'db>>) {
        let db = self.db();
        let segs: Vec<Partial<UsePathSegment<'db>>> = segments
            .into_iter()
            .map(|ident| Partial::Present(UsePathSegment::Ident(ident)))
            .chain(std::iter::once(Partial::Present(UsePathSegment::Glob)))
            .collect();
        let path = Partial::Present(UsePathId::new(db, segs, None, None));

        let id = self.joined_id(TrackedItemVariant::Use(path));
        self.enter_item_scope(id, false);

        let top_mod = self.top_mod();
        let origin = HirOrigin::synthetic();
        let attrs = AttrListId::new(db, vec![]);
        let use_ = Use::new(
            db,
            id,
            attrs,
            path,
            None,
            Visibility::Private,
            top_mod,
            origin,
        );
        self.leave_item_scope(use_);
    }

    pub(super) fn enter_block_scope(&mut self) {
        self.builder.enter_block_scope();
    }

    pub(super) fn leave_block_scope(&mut self, block: ExprId) {
        self.builder.leave_block_scope(block);
    }

    pub(super) fn joined_id(&self, id: TrackedItemVariant<'db>) -> TrackedItemId<'db> {
        self.builder.joined_id(id)
    }

    pub(super) fn next_impl_idx(&mut self) -> u32 {
        let idx = self.next_impl_idx;
        self.next_impl_idx += 1;
        idx
    }

    pub(super) fn next_impl_trait_idx(&mut self) -> u32 {
        let idx = self.next_impl_trait_idx;
        self.next_impl_trait_idx += 1;
        idx
    }

    pub(super) fn next_static_assert_idx(&mut self) -> u32 {
        let idx = self.next_static_assert_idx;
        self.next_static_assert_idx += 1;
        idx
    }

    pub(super) fn next_with_idx(&mut self) -> u32 {
        let idx = self.next_with_idx;
        self.next_with_idx += 1;
        idx
    }

    pub(super) fn current_quote_top_mod(&self) -> Option<TopLevelMod<'db>> {
        let frame = self.quote_frames.last()?;
        (frame.quote_top_mod != self.top_mod()).then_some(frame.quote_top_mod)
    }

    pub(super) fn current_quote_mod_idx(&mut self) -> Option<u16> {
        let quote_top_mod = self.current_quote_top_mod()?;
        Some(self.builder.intern_quote_mod(quote_top_mod))
    }

    /// Creates a new scope for an item.
    fn enter_item_scope(&mut self, id: TrackedItemId<'db>, is_mod: bool) {
        let quote_top_mod = self.current_quote_top_mod();
        self.builder.enter_item_scope(id, is_mod, quote_top_mod);
    }

    fn enter_body_scope(&mut self, id: TrackedItemId<'db>) {
        let quote_top_mod = self.current_quote_top_mod();
        self.builder.enter_body_scope(id, quote_top_mod);
    }

    pub(super) fn enter_body_scope_with_top_mod(
        &mut self,
        id: TrackedItemId<'db>,
        quote_top_mod: Option<TopLevelMod<'db>>,
    ) {
        self.builder.enter_body_scope(id, quote_top_mod);
    }

    /// Leaves the current scope, `item` should be the generated item which owns
    /// the scope.
    pub(super) fn leave_item_scope<I>(&mut self, item: I) -> I
    where
        I: Into<ItemKind<'db>> + Copy,
    {
        self.builder.leave_item_scope(item.into());
        item
    }
}

impl<'db> IdentId<'db> {
    fn lower_token(ctxt: &mut FileLowerCtxt<'db>, token: SyntaxToken) -> Self {
        Self::new(ctxt.db(), token.text())
    }

    fn lower_token_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        token: Option<SyntaxToken>,
    ) -> Partial<Self> {
        token.map(|token| Self::lower_token(ctxt, token)).into()
    }
}

impl<'db> LitKind<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Lit) -> Option<Self> {
        Some(match ast.kind() {
            ast::LitKind::Int(int) => Self::Int(IntegerId::lower_ast(ctxt, int)),
            ast::LitKind::String(string) => {
                // Parsing diagnosed invalid escapes; recovery must not intern
                // their source spelling as a valid semantic string.
                Self::String(StringId::new(ctxt.db(), string.value().ok()?))
            }
            // Parsing diagnosed invalid byte literals.
            ast::LitKind::Byte(byte) => Self::Byte(byte.value().ok()?),
            ast::LitKind::Bool(bool) => match bool.token().text() {
                "true" => Self::Bool(true),
                "false" => Self::Bool(false),
                _ => unreachable!(),
            },
        })
    }
}

impl<'db> IntegerId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::LitInt) -> Self {
        let text = ast.token().text();
        // Parser ensures that the text is valid pair with a radix and a number.
        if text.len() < 2 {
            return Self::new(ctxt.db(), BigUint::from_str_radix(text, 10).unwrap());
        }

        let int = match &text[0..2] {
            "0x" | "0X" => BigUint::from_str_radix(&text[2..], 16).unwrap(),
            "0o" | "0O" => BigUint::from_str_radix(&text[2..], 8).unwrap(),
            "0b" | "0B" => BigUint::from_str_radix(&text[2..], 2).unwrap(),
            _ => BigUint::from_str_radix(text, 10).unwrap(),
        };

        Self::new(ctxt.db(), int)
    }
}
