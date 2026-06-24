use common::{
    file::File,
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
        AttrListId, ExprId, IdentId, IntegerId, ItemKind, LitKind, ModuleTree, Partial, StringId,
        TopLevelMod, TrackedItemId, TrackedItemVariant, Use, Visibility, module_tree_impl,
        scope_graph::ScopeGraph,
        use_tree::{UsePathId, UsePathSegment},
    },
    span::HirOrigin,
};
pub use abi_field::{AbiFieldContext, AbiFieldDiagnostic};
pub use attr::{AttrMisuseError, AttrMisuseErrorKind};
pub use derive::{DeriveError, DeriveErrorKind, DeriveSecondarySpan};
pub use provider::{DerivedImplProvenance, derived_impl_provenance};
pub(crate) use provider::CoreDeriveItem;
pub(crate) use provider::impl_trait_provider_goal_path;
pub use error::{ErrorDiagnostic, ErrorDiagnosticKind};
pub use event::{EventError, EventErrorKind};
pub(crate) use expansion::expanded_items_impl;
pub use expansion::generated_hir_items;
pub use item::{FieldModifierError, FieldModifierErrorKind, SelectorError, SelectorErrorKind};
pub use parse::parse_file_impl;

pub(crate) mod parse;

mod abi_field;
mod attr;
mod body;
mod contract;
mod derive;
mod error;
mod event;
mod expansion;
mod expr;
mod hir_builder;
mod item;
mod msg;
mod params;
mod pat;
mod path;
mod payable;
mod provider;
mod provider_executor;
mod provider_synthesis;
mod scope_builder;
mod stmt;
mod types;
mod use_tree;

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
    scope_graph_impl(db, top_mod)
}

/// Returns the ingot module tree of the given ingot.
pub fn module_tree<'db>(db: &'db dyn LowerHirDb, ingot: Ingot<'db>) -> &'db ModuleTree<'db> {
    module_tree_impl(db, ingot)
}

#[salsa::tracked]
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

/// Lowers the AST of `top_mod` into the *base* scope graph: every item
/// written in (or desugared from) the source file, but none of the items
/// synthesized by the post-lowering expansion stage (see [`expansion`]).
///
/// The expansion stage consumes this query, so it must never read
/// [`scope_graph_impl`] (directly or transitively) — that would be a query
/// cycle.
#[salsa::tracked(return_ref)]
pub(crate) fn base_scope_graph_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> ScopeGraph<'db> {
    let ast = top_mod_ast(db, top_mod);
    let mut ctxt = FileLowerCtxt::enter_top_mod(db, top_mod);

    ctxt.insert_synthetic_prelude_use();

    if let Some(items) = ast.items() {
        item::validate_module_inner_attrs(&mut ctxt, items.inner_attr_list());
        lower_module_items(&mut ctxt, items);
    }
    ctxt.leave_item_scope(top_mod);

    ctxt.build()
}

/// The complete scope graph of `top_mod`: the base graph produced by AST
/// lowering, plus the items synthesized by the post-lowering expansion stage
/// (the provider-generated `#[derive(..)]` / `derive ..` impls).
///
/// Generated items are merged in as ordinary scopes — children of the same
/// parent scope as the item they were derived from — so every downstream
/// consumer (name resolution, `all_items`/`all_funcs`/`all_impl_traits`,
/// type checking, MIR lowering) sees them exactly like hand-written items
/// without any special casing.
///
/// For modules where the expansion stage generates nothing (the common
/// case), this returns the base graph directly instead of storing a merged
/// copy.
pub(crate) fn scope_graph_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> &'db ScopeGraph<'db> {
    if expansion::expanded_items_impl(db, top_mod)
        .graph
        .scopes
        .is_empty()
    {
        base_scope_graph_impl(db, top_mod)
    } else {
        merged_scope_graph_impl(db, top_mod)
    }
}

/// The base scope graph with the expansion stage's partial graph merged in.
/// Only consulted (via [`scope_graph_impl`]) for modules where the expansion
/// stage generated items.
#[salsa::tracked(return_ref)]
pub(crate) fn merged_scope_graph_impl<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
) -> ScopeGraph<'db> {
    let base = base_scope_graph_impl(db, top_mod);
    let generated = &expansion::expanded_items_impl(db, top_mod).graph;

    let mut merged = base.clone();
    for (scope_id, scope) in &generated.scopes {
        if let Some(existing) = merged.scopes.get_mut(scope_id) {
            // A shim scope mirroring an existing scope of the base graph:
            // union the generated parent-to-child edges into it.
            existing.edges.extend(scope.edges.iter().cloned());
        } else {
            merged.scopes.insert(*scope_id, scope.clone());
        }
    }
    merged
        .unresolved_uses
        .extend(generated.unresolved_uses.iter().copied());
    merged
}

#[salsa::tracked]
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
    let node = SyntaxNode::new_root(parse_file_impl(db, top_mod));
    // This cast never fails even if the file content is empty.
    ast::Root::cast(node).unwrap()
}

pub(super) struct FileLowerCtxt<'db> {
    builder: ScopeGraphBuilder<'db>,
    next_impl_idx: u32,
    next_impl_trait_idx: u32,
    next_derive_provider_scope_idx: u32,
    next_derive_decl_idx: u32,
    next_static_assert_idx: u32,
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_top_mod(db, top_mod),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_derive_provider_scope_idx: 0,
            next_derive_decl_idx: 0,
            next_static_assert_idx: 0,
        }
    }

    /// Creates a lowering context for the post-lowering expansion stage of
    /// `top_mod`. Items built in this context get `TrackedItemId`s namespaced
    /// under [`TrackedItemVariant::Expansion`], so the per-file impl counters
    /// restarting from zero cannot collide with base items.
    pub(super) fn enter_expansion(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_expansion(db, top_mod),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_derive_provider_scope_idx: 0,
            next_derive_decl_idx: 0,
            next_static_assert_idx: 0,
        }
    }

    /// Enters a shim for an *existing* scope of the base graph; generated
    /// items built before the matching [`Self::leave_shim_scope`] become
    /// children of that scope once the partial graph is merged.
    pub(super) fn enter_shim_scope(
        &mut self,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        vis: Visibility,
    ) {
        self.builder.enter_shim_scope(scope, vis);
    }

    pub(super) fn leave_shim_scope(&mut self) {
        self.builder.leave_shim_scope();
    }

    pub(super) fn build(self) -> ScopeGraph<'db> {
        self.builder.build()
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.builder.db
    }

    pub(super) fn top_mod(&self) -> TopLevelMod<'db> {
        self.builder.top_mod
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
        let path = Partial::Present(UsePathId::new(db, segs));

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
        let path = Partial::Present(UsePathId::new(db, segs));

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

    pub(super) fn next_derive_decl_idx(&mut self) -> u32 {
        let idx = self.next_derive_decl_idx;
        self.next_derive_decl_idx += 1;
        idx
    }

    pub(super) fn next_derive_provider_scope_idx(&mut self) -> u32 {
        let idx = self.next_derive_provider_scope_idx;
        self.next_derive_provider_scope_idx += 1;
        idx
    }

    /// Creates a new scope for an item.
    fn enter_item_scope(&mut self, id: TrackedItemId<'db>, is_mod: bool) {
        self.builder.enter_item_scope(id, is_mod);
    }

    fn enter_body_scope(&mut self, id: TrackedItemId<'db>) {
        self.builder.enter_body_scope(id);
    }

    /// Leaves the current scope, `item` should be the generated item which owns
    /// the scope.
    fn leave_item_scope<I>(&mut self, item: I) -> I
    where
        I: Into<ItemKind<'db>> + Copy,
    {
        self.builder.leave_item_scope(item.into());
        item
    }
}

impl<'db> IdentId<'db> {
    fn lower_token(ctxt: &mut FileLowerCtxt<'db>, token: SyntaxToken) -> Self {
        Self::new(ctxt.db(), token.text().to_string())
    }

    fn lower_token_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        token: Option<SyntaxToken>,
    ) -> Partial<Self> {
        token.map(|token| Self::lower_token(ctxt, token)).into()
    }
}

impl<'db> LitKind<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Lit) -> Self {
        match ast.kind() {
            ast::LitKind::Int(int) => Self::Int(IntegerId::lower_ast(ctxt, int)),
            ast::LitKind::String(string) => {
                let text = string.token().text();
                Self::String(StringId::new(
                    ctxt.db(),
                    text[1..text.len() - 1].to_string(),
                ))
            }
            ast::LitKind::Bool(bool) => match bool.token().text() {
                "true" => Self::Bool(true),
                "false" => Self::Bool(false),
                _ => unreachable!(),
            },
        }
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
