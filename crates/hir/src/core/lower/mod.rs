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
        AttrListId, Body, ExprId, Func, IdentId, IntegerId, ItemKind, LitKind, ModuleTree, Partial,
        PathId, StringId, TopLevelMod, TrackedItemId, TrackedItemVariant, Use, Visibility,
        module_tree_impl,
        scope_graph::ScopeGraph,
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
    scope_graph_impl(db, top_mod)
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
        lower_module_items(&mut ctxt, items);
    }
    generated::append_functions(&mut ctxt);
    ctxt.builder.apply_quoted_function_origins();
    ctxt.leave_item_scope(top_mod);

    ctxt.build()
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
    Generation(GenerationLoweringError),
}

pub(super) struct FileLowerCtxt<'db> {
    builder: ScopeGraphBuilder<'db>,
    next_impl_idx: u32,
    next_impl_trait_idx: u32,
    next_static_assert_idx: u32,
    diagnostics: Vec<LowerDiagnostic>,
    next_quote_frame_id: usize,
    pub(super) quote_frames: Vec<generated::ActiveQuoteFrame<'db>>,
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_top_mod(db, top_mod),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_static_assert_idx: 0,
            diagnostics: Vec::new(),
            next_quote_frame_id: 0,
            quote_frames: Vec::new(),
        }
    }

    fn enter_func_body(db: &'db dyn HirDb, func: Func<'db>) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_func_body(db, func),
            next_impl_idx: 0,
            next_impl_trait_idx: 0,
            next_static_assert_idx: 0,
            diagnostics: Vec::new(),
            next_quote_frame_id: 0,
            quote_frames: Vec::new(),
        }
    }

    pub(super) fn next_quote_frame_id(&mut self) -> usize {
        let id = self.next_quote_frame_id;
        self.next_quote_frame_id += 1;
        id
    }

    pub(super) fn resolve_sub_quote_local_ident(&self, text: &str) -> IdentId<'db> {
        let db = self.db();
        for frame in self.quote_frames.iter().rev() {
            if !frame.is_sub_quote {
                break;
            }
            if !frame.open_names.iter().any(|(name, _)| name == text) {
                return IdentId::new(db, format!("\0q{}_{text}", frame.frame_id));
            }
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

    pub(super) fn current_quote_ingot(&self) -> Option<common::ingot::Ingot<'db>> {
        let frame = self.quote_frames.last()?;
        let db = self.db();
        let quote_ingot = frame.quote_top_mod.ingot(db);
        (quote_ingot != self.top_mod().ingot(db)).then_some(quote_ingot)
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
        let file = db
            .workspace()
            .get(db, quote_file)
            .expect("validated frozen quote file exists in workspace");
        let quote_top_mod = map_file_to_mod_impl(db, file);
        let root = top_mod_ast(db, quote_top_mod);
        let quote = root
            .syntax()
            .descendants()
            .filter_map(ast::QuoteExpr::cast)
            .find(|quote| quote.syntax().text_range() == quote_range)
            .expect("validated frozen quote belongs to unchanged input source");
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
        let producer_scope =
            self.builder
                .resolve_producer_scope(quote_top_mod, producer_module, producer_block);
        let frame_id = self.next_quote_frame_id();
        (
            quote,
            generated::ActiveQuoteFrame {
                frame_id,
                quote_top_mod,
                quote_range,
                producer_scope,
                is_sub_quote,
                is_member_quote: false,
                open_names,
                bound_names: rustc_hash::FxHashSet::default(),
                holes,
            },
        )
    }

    pub(super) fn resolve_record_item(
        &self,
        record_ref: &common::generation::FrozenRecordRef,
    ) -> ItemKind<'db> {
        let db = self.db();
        let file = db
            .workspace()
            .get(db, &record_ref.target_file)
            .expect("validated frozen record file exists in workspace");
        let record_top_mod = map_file_to_mod_impl(db, file);
        if record_top_mod == self.top_mod() {
            return self
                .builder
                .find_item_by_range(record_ref.target_range)
                .expect("validated authored record item exists in module");
        }
        let root = top_mod_ast(db, record_top_mod);
        record_top_mod
            .scope_graph(db)
            .scopes
            .keys()
            .find_map(|scope| match *scope {
                crate::hir_def::scope_graph::ScopeId::Item(item @ ItemKind::Struct(s)) => {
                    let HirOrigin::Raw(ptr) = s.origin(db) else {
                        return None;
                    };
                    (ptr.to_node(root.syntax()).syntax().text_range() == record_ref.target_range)
                        .then_some(item)
                }
                crate::hir_def::scope_graph::ScopeId::Item(item @ ItemKind::Enum(e)) => {
                    let HirOrigin::Raw(ptr) = e.origin(db) else {
                        return None;
                    };
                    (ptr.to_node(root.syntax()).syntax().text_range() == record_ref.target_range)
                        .then_some(item)
                }
                _ => None,
            })
            .expect("validated authored record item exists in target module")
    }

    pub(super) fn resolve_trait_item(
        &self,
        reference: &common::generation::FrozenTraitRef,
    ) -> crate::hir_def::Trait<'db> {
        let db = self.db();
        let file = db
            .workspace()
            .get(db, &reference.file)
            .expect("validated frozen trait file exists in workspace");
        let module = map_file_to_mod_impl(db, file);
        if module == self.top_mod() {
            return self
                .builder
                .find_trait_by_range(reference.range)
                .expect("validated trait exists in unchanged source");
        }
        let root = top_mod_ast(db, module);
        module
            .scope_graph(db)
            .scopes
            .keys()
            .find_map(|scope| {
                let crate::hir_def::scope_graph::ScopeId::Item(ItemKind::Trait(trait_)) = *scope
                else {
                    return None;
                };
                let HirOrigin::Raw(ptr) = trait_.origin(db) else {
                    return None;
                };
                (ptr.to_node(root.syntax()).syntax().text_range() == reference.range)
                    .then_some(trait_)
            })
            .expect("validated trait exists in unchanged source")
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
        match (
            self.resolve_record_item(&field_ref.record),
            field_ref.record.variant_index,
        ) {
            (ItemKind::Struct(s), None) => match s.fields(db).data(db).get(field_ref.index) {
                Some(f) => f.name,
                None => {
                    self.record_generation_error(
                        range,
                        "invalid field splice in record field label",
                    );
                    Partial::Absent
                }
            },
            (ItemKind::Enum(e), Some(v)) => {
                match e.variants_list(db).data(db).get(v).map(|v| &v.kind) {
                    Some(crate::hir_def::VariantKind::Record(fields)) => {
                        match fields.data(db).get(field_ref.index) {
                            Some(f) => f.name,
                            None => {
                                self.record_generation_error(
                                    range,
                                    "invalid field splice in record field label",
                                );
                                Partial::Absent
                            }
                        }
                    }
                    Some(crate::hir_def::VariantKind::Tuple(_)) => {
                        self.record_generation_error(
                            range,
                            "tuple variant field cannot be used as a named record field label",
                        );
                        Partial::Absent
                    }
                    _ => {
                        self.record_generation_error(
                            range,
                            "invalid field splice in record field label",
                        );
                        Partial::Absent
                    }
                }
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
        let res: Partial<FieldIndex<'db>> = match (
            self.resolve_record_item(&field_ref.record),
            field_ref.record.variant_index,
        ) {
            (ItemKind::Struct(s), None) => s
                .fields(db)
                .data(db)
                .get(field_ref.index)
                .and_then(|f| f.name.to_opt())
                .map(FieldIndex::Ident)
                .into(),
            (ItemKind::Enum(e), Some(v)) => {
                match e.variants_list(db).data(db).get(v).map(|v| &v.kind) {
                    Some(crate::hir_def::VariantKind::Record(fields)) => fields
                        .data(db)
                        .get(field_ref.index)
                        .and_then(|f| f.name.to_opt())
                        .map(FieldIndex::Ident)
                        .into(),
                    Some(crate::hir_def::VariantKind::Tuple(_)) => Partial::Present(
                        FieldIndex::Index(IntegerId::new(db, BigUint::from(field_ref.index))),
                    ),
                    _ => Partial::Absent,
                }
            }
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

    /// The path naming the core ingot from the file: `ingot` within core.
    pub(super) fn core_path(&self) -> PathId<'db> {
        let db = self.db();
        let root = if self.top_mod().ingot(db).kind(db) == IngotKind::Core {
            IdentId::make_ingot(db)
        } else {
            IdentId::new(db, "core".to_string())
        };
        PathId::from_ident(db, root)
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
