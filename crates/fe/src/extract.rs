//! Documentation extraction from HIR
//!
//! This module traverses the Fe HIR (High-level Intermediate Representation) and extracts
//! documentation into the `DocItem` model suitable for rendering.

use common::ingot::Ingot;
use hir::{
    SpannedHirDb,
    core::semantic::SymbolView,
    hir_def::{
        Contract, Enum, FieldParent, HirIngot, Impl, ImplTrait, ItemKind, Struct, TopLevelMod,
        Trait, VariantKind, Visibility, scope_graph::ScopeId,
    },
    semantic::{FieldView, qualify_path_with_ingot_name},
    span::{self, DesugaredOrigin, HirOrigin, LazySpan},
};

use fe_web::model::{
    DocChild, DocChildKind, DocContent, DocGenericParam, DocImplMethod, DocIndex, DocItem,
    DocItemKind, DocModuleItem, DocModuleTree, DocSourceLoc, DocTraitImpl, DocVisibility,
    SignatureSpanData,
};

/// Extracts documentation from a Fe HIR database
pub struct DocExtractor<'db> {
    db: &'db dyn SpannedHirDb,
    /// Root path for computing relative display paths
    root_path: Option<std::path::PathBuf>,
    /// Include `#[test]` functions in the output.
    /// Off by default — test fns pollute the public API sidebar.
    include_tests: bool,
}

impl<'db> DocExtractor<'db> {
    pub fn new(db: &'db dyn SpannedHirDb) -> Self {
        Self {
            db,
            root_path: None,
            include_tests: false,
        }
    }

    /// Set the root path for computing relative display paths.
    /// Source links use display_file relative to this root.
    pub fn with_root_path(mut self, root: std::path::PathBuf) -> Self {
        self.root_path = Some(root);
        self
    }

    /// Include `#[test]` functions in the extracted output.
    pub fn with_include_tests(mut self, include: bool) -> Self {
        self.include_tests = include;
        self
    }

    /// Returns true if `item` is a `#[test(...)]` function that should be
    /// filtered out unless `include_tests` is enabled.
    fn is_filtered_test_item(&self, item: ItemKind<'db>) -> bool {
        if self.include_tests {
            return false;
        }
        matches!(item, ItemKind::Func(_))
            && item
                .attrs(self.db)
                .is_some_and(|attrs| attrs.has_attr(self.db, "test"))
    }

    /// Rewrite a path to use the ingot's config name instead of "lib".
    /// Delegates to the shared `qualify_path_with_ingot_name` function.
    fn qualify_path_with_ingot(&self, path: &str, ingot: Ingot<'db>) -> String {
        qualify_path_with_ingot_name(self.db, path, ingot)
    }

    /// Extract documentation for a single item with ingot-qualified paths
    pub fn extract_item_for_ingot(
        &self,
        item: ItemKind<'db>,
        ingot: Ingot<'db>,
    ) -> Option<DocItem> {
        let mut doc_item = self.extract_item(item)?;
        doc_item.path = self.qualify_path_with_ingot(&doc_item.path, ingot);

        // If this is the ingot root module (name is literally "lib" from
        // `lib.fe`), substitute the ingot's configured name so pages don't
        // render `<h1>lib</h1>`. Match on the qualified path being exactly
        // the ingot name so a nested `foo::lib` submodule is left alone.
        if matches!(doc_item.kind, DocItemKind::Module)
            && doc_item.name == "lib"
            && let Some(cfg_name) = ingot
                .config(self.db)
                .and_then(|c| c.metadata.name.clone())
            && doc_item.path == cfg_name.as_str()
        {
            doc_item.name = cfg_name.to_string();
        }

        // The display_file from get_source_location is already relative to workspace root,
        // which includes the ingot directory. No need to prepend ingot name again.

        Some(doc_item)
    }

    /// Extract documentation for an entire top-level module and its children
    pub fn extract_module(&self, top_mod: TopLevelMod<'db>) -> DocIndex {
        let mut index = DocIndex::new();

        // Extract all items recursively
        for item in top_mod.children_nested(self.db) {
            if let Some(doc_item) = self.extract_item(item) {
                index.add_item(doc_item);
            }
        }

        // Build module tree for navigation
        index.modules = self.build_module_tree(top_mod);

        index
    }

    /// Extract trait implementation links from an ingot.
    /// Returns a list of (target_type_name, DocTraitImpl) that should be
    /// added to the corresponding type's trait_impls field.
    /// Includes both `impl Trait for Type` and `impl Type` blocks.
    pub fn extract_trait_impl_links(&self, ingot: Ingot<'db>) -> Vec<(String, DocTraitImpl)> {
        let mut links = Vec::new();

        // Extract `impl Trait for Type` blocks
        for &impl_trait in ingot.all_impl_traits(self.db) {
            let Some(trait_name) = impl_trait.trait_name(self.db) else {
                continue;
            };
            let Some(target_type) = impl_trait.target_type_name(self.db) else {
                continue;
            };

            // Construct impl path from parent module + target type + trait
            // e.g., "hoverable::Numbers::impl_Calculatable"
            let parent_path = impl_trait
                .scope()
                .parent_module(self.db)
                .and_then(|m| m.pretty_path(self.db))
                .unwrap_or_default();
            let parent_path = self.qualify_path_with_ingot(&parent_path, ingot);

            // Create a unique impl identifier: Type::impl_Trait
            let simple_type = extract_simple_name(&target_type);
            let simple_trait = extract_simple_name(&trait_name);
            let impl_path = if parent_path.is_empty() {
                format!("{}::impl_{}", simple_type, simple_trait)
            } else {
                format!("{}::{}::impl_{}", parent_path, simple_type, simple_trait)
            };

            let (signature, signature_span) = self.get_signature_with_span(impl_trait.into());
            let methods = self.extract_impl_trait_methods(impl_trait);

            links.push((
                target_type,
                DocTraitImpl {
                    trait_name,
                    impl_url: format!("{}/impl", impl_path),
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    methods,
                },
            ));
        }

        // Extract `impl Type` blocks (inherent impls)
        // Track index per type for multiple inherent impls
        let mut inherent_impl_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for &impl_ in ingot.all_impls(self.db) {
            let Some(target_type) = impl_.target_type_name(self.db) else {
                continue;
            };

            let parent_path = impl_
                .scope()
                .parent_module(self.db)
                .and_then(|m| m.pretty_path(self.db))
                .unwrap_or_default();
            let parent_path = self.qualify_path_with_ingot(&parent_path, ingot);

            // Create a unique impl identifier: Type::impl or Type::impl_1, etc.
            let simple_type = extract_simple_name(&target_type);
            let count = inherent_impl_counts.entry(simple_type.clone()).or_insert(0);
            let impl_suffix = if *count == 0 {
                "impl".to_string()
            } else {
                format!("impl_{}", count)
            };
            *count += 1;

            let impl_path = if parent_path.is_empty() {
                format!("{}::{}", simple_type, impl_suffix)
            } else {
                format!("{}::{}::{}", parent_path, simple_type, impl_suffix)
            };

            let (signature, signature_span) = self.get_signature_with_span(impl_.into());
            let methods = self.extract_impl_methods_for_type(impl_);

            links.push((
                target_type,
                DocTraitImpl {
                    trait_name: String::new(), // Empty = inherent impl
                    impl_url: format!("{}/impl", impl_path),
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    methods,
                },
            ));
        }

        links
    }

    /// Extract methods from an `impl Trait for Type` block as DocImplMethod
    fn extract_impl_trait_methods(&self, it: ImplTrait<'db>) -> Vec<DocImplMethod> {
        it.methods(self.db)
            .filter_map(|func| {
                let name = func.name(self.db).to_opt()?.data(self.db).to_string();
                let (signature, signature_span) = self.get_signature_with_span(func.into());
                let docs = self
                    .get_docstring(func.scope())
                    .map(|s| DocContent::from_raw(&s));

                Some(DocImplMethod {
                    name,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    docs,
                })
            })
            .collect()
    }

    /// Extract methods from an `impl Type` block as DocImplMethod
    fn extract_impl_methods_for_type(&self, i: Impl<'db>) -> Vec<DocImplMethod> {
        i.funcs(self.db)
            .filter_map(|func| {
                let name = func.name(self.db).to_opt()?.data(self.db).to_string();
                let (signature, signature_span) = self.get_signature_with_span(func.into());
                let docs = self
                    .get_docstring(func.scope())
                    .map(|s| DocContent::from_raw(&s));

                Some(DocImplMethod {
                    name,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    docs,
                })
            })
            .collect()
    }

    /// Extract documentation for a single item
    pub fn extract_item(&self, item: ItemKind<'db>) -> Option<DocItem> {
        // Skip items that shouldn't be documented
        match item {
            ItemKind::StaticAssert(_) | ItemKind::Use(_) | ItemKind::Body(_) => return None,
            _ => {}
        }

        // Skip `#[test]` fns unless explicitly requested. Doc pages should
        // focus on the public API surface by default.
        if self.is_filtered_test_item(item) {
            return None;
        }

        // Skip the synthetic pieces produced by `msg` desugaring — the
        // per-variant struct and its accompanying impl blocks. The `msg` Mod
        // itself is the sole DocItem for that block; variants appear inline
        // on its page (see `extract_children` for `ItemKind::Mod(_)`).
        match item {
            ItemKind::Struct(s) if is_desugared_msg_variant_struct(self.db, s) => return None,
            ItemKind::ImplTrait(it) if is_desugared_msg_impl_trait(self.db, it) => return None,
            ItemKind::Impl(i)
                if matches!(
                    span::impl_ast(self.db, i),
                    HirOrigin::Desugared(DesugaredOrigin::Msg(_))
                ) =>
            {
                return None;
            }
            _ => {}
        }

        // Skip items nested inside containers (traits, structs, enums, impls) —
        // they are already captured as children of their parent DocItem.
        // For a `msg` Mod the variants are also rendered as children, so
        // treat that Mod as a container too.
        if let Some(parent) = item.scope().parent_item(self.db) {
            if crate::index_util::is_container_item(parent) {
                return None;
            }
            if let ItemKind::Mod(m) = parent
                && is_desugared_msg_mod(self.db, m)
            {
                return None;
            }
        }

        let scope = item.scope();
        let path = scope.pretty_path(self.db)?;
        let name = item.name(self.db)?.data(self.db).to_string();
        let kind = self.item_kind_to_doc_kind(item)?;
        let visibility = self.convert_visibility(item.vis(self.db));
        let docs = self.get_docstring(scope).map(|s| DocContent::from_raw(&s));
        let (signature, signature_span) = self.get_signature_with_span(item);
        let generics = self.extract_generics(item);
        let where_bounds = self.extract_where_bounds(item);
        let children = self.extract_children(item);
        let source = self.get_source_location(item);
        let source_text = self.get_source_text(item);

        Some(DocItem {
            path,
            name,
            kind,
            visibility,
            docs,
            signature,
            rich_signature: vec![],
            signature_span,
            sig_scope: None,
            generics,
            where_bounds,
            children,
            source,
            source_text,
            trait_impls: Vec::new(),  // Populated later by link_trait_impls
            implementors: Vec::new(), // Populated later by link_trait_impls (for traits)
        })
    }

    fn item_kind_to_doc_kind(&self, item: ItemKind<'db>) -> Option<DocItemKind> {
        match item {
            ItemKind::TopMod(_) => Some(DocItemKind::Module),
            ItemKind::Mod(m) => {
                if is_desugared_msg_mod(self.db, m) {
                    Some(DocItemKind::Msg)
                } else {
                    Some(DocItemKind::Module)
                }
            }
            ItemKind::Func(_) => Some(DocItemKind::Function),
            ItemKind::Struct(s) => {
                if is_desugared_msg_variant_struct(self.db, s) {
                    Some(DocItemKind::MsgVariant)
                } else {
                    Some(DocItemKind::Struct)
                }
            }
            ItemKind::Enum(_) => Some(DocItemKind::Enum),
            ItemKind::Trait(_) => Some(DocItemKind::Trait),
            ItemKind::Contract(_) => Some(DocItemKind::Contract),
            ItemKind::TypeAlias(_) => Some(DocItemKind::TypeAlias),
            ItemKind::Const(_) => Some(DocItemKind::Const),
            ItemKind::Impl(_) => Some(DocItemKind::Impl),
            ItemKind::ImplTrait(_) => Some(DocItemKind::ImplTrait),
            ItemKind::StaticAssert(_) | ItemKind::Use(_) | ItemKind::Body(_) => None,
        }
    }

    fn convert_visibility(&self, vis: Visibility) -> DocVisibility {
        match vis {
            Visibility::Public | Visibility::PubIngot | Visibility::PubSuper => {
                DocVisibility::Public
            }
            Visibility::Private => DocVisibility::Private,
        }
    }

    /// Extract doc comments from a scope's attributes.
    /// Delegates to SymbolView::docs().
    fn get_docstring(&self, scope: ScopeId<'db>) -> Option<String> {
        SymbolView::new(scope).docs(self.db)
    }

    /// Get first sentence of documentation as a summary
    fn get_summary(&self, scope: ScopeId<'db>) -> Option<String> {
        let docs = self.get_docstring(scope)?;
        let trimmed = docs.trim();
        // Find first sentence ending or paragraph break.
        // find() returns byte offsets that are always on char boundaries
        // since `. `, `.\n`, and `\n\n` are all ASCII, so `i + 1` lands
        // on the byte after `.` or `\n` which is also a valid boundary.
        let end = trimmed
            .find(". ")
            .or_else(|| trimmed.find(".\n"))
            .or_else(|| trimmed.find("\n\n"))
            .map(|i| i + 1)
            .unwrap_or(trimmed.len());
        let summary = trimmed.get(..end).unwrap_or(trimmed).trim();
        if summary.is_empty() {
            None
        } else {
            Some(summary.to_string())
        }
    }

    /// Get the item's signature and its source span data for SCIP overlay.
    fn get_signature_with_span(&self, item: ItemKind<'db>) -> (String, Option<SignatureSpanData>) {
        let sym = SymbolView::from_item(item);
        match sym.signature_with_span(self.db) {
            Some(sig_span) => {
                let span_data = sig_span.file.url(self.db).map(|u| SignatureSpanData {
                    file_url: u.to_string(),
                    byte_start: sig_span.byte_start,
                    byte_end: sig_span.byte_end,
                });
                (sig_span.text.replace("\r\n", "\n"), span_data)
            }
            None => (sym.signature(self.db).unwrap_or_default(), None),
        }
    }

    /// Build `SignatureSpanData` from a resolved `Span`.
    fn span_to_sig_data(&self, span: &common::diagnostics::Span) -> Option<SignatureSpanData> {
        let file_url = span.file.url(self.db)?;
        Some(SignatureSpanData {
            file_url: file_url.to_string(),
            byte_start: span.range.start().into(),
            byte_end: span.range.end().into(),
        })
    }

    /// Build a signature span covering name..type for a field.
    ///
    /// The full field AST node includes visibility keywords (`pub`), but the
    /// signature text is `"name: type"`. Using the full node span would shift
    /// SCIP occurrence positions in virtual documents, causing wrong symbols
    /// to be resolved. Instead we span from the name token start to the type
    /// node end, which exactly matches the signature text layout.
    fn field_sig_span(&self, field_view: FieldView<'db>) -> Option<SignatureSpanData> {
        // Desugared msg variant struct fields: the HIR field spans inherit
        // the variant block's DesugaredOrigin, so `.fields().field(i).name()`
        // collapses to the entire variant body. Skip the signature span so
        // the renderer shows the plain signature text (`name: Type`) without
        // overlaying SCIP occurrences on a span that would shift positions.
        if let hir::hir_def::FieldParent::Struct(s) = field_view.parent
            && is_desugared_msg_variant_struct(self.db, s)
        {
            return None;
        }

        let name_span = match field_view.parent {
            hir::hir_def::FieldParent::Struct(s) => s
                .span()
                .fields()
                .field(field_view.idx)
                .name()
                .resolve(self.db),
            hir::hir_def::FieldParent::Contract(c) => c
                .span()
                .fields()
                .field(field_view.idx)
                .name()
                .resolve(self.db),
            hir::hir_def::FieldParent::Variant(v) => v
                .span()
                .fields()
                .field(field_view.idx)
                .name()
                .resolve(self.db),
        }?;
        let ty_span = field_view.ty_span().resolve(self.db)?;
        let file_url = name_span.file.url(self.db)?;
        Some(SignatureSpanData {
            file_url: file_url.to_string(),
            byte_start: name_span.range.start().into(),
            byte_end: ty_span.range.end().into(),
        })
    }

    /// Extract the type text from a field's type span
    fn get_field_type_text(&self, field_view: FieldView<'db>) -> Option<String> {
        let ty_span = field_view.ty_span().resolve(self.db)?;
        let start: usize = ty_span.range.start().into();
        let end: usize = ty_span.range.end().into();
        let file_text = ty_span.file.text(self.db).as_str();

        Some(file_text.get(start..end)?.trim().to_string())
    }

    /// Extract generic parameters from an item
    fn extract_generics(&self, item: ItemKind<'db>) -> Vec<DocGenericParam> {
        // Get generics from scope's children
        let scope = item.scope();
        let mut params = Vec::new();

        for child_scope in scope.children(self.db) {
            if let ScopeId::GenericParam(_, _) = child_scope
                && let Some(name) = child_scope.name(self.db)
            {
                params.push(DocGenericParam {
                    name: name.data(self.db).to_string(),
                    bounds: Vec::new(),
                    default: None,
                });
            }
        }

        params
    }

    /// Extract where clause bounds
    fn extract_where_bounds(&self, _item: ItemKind<'db>) -> Vec<String> {
        // TODO: implement where clause extraction
        Vec::new()
    }

    /// Extract child items (fields, variants, methods, etc.)
    fn extract_children(&self, item: ItemKind<'db>) -> Vec<DocChild> {
        match item {
            ItemKind::Struct(s) => self.extract_struct_fields(s),
            ItemKind::Contract(c) => {
                let mut children = self.extract_contract_fields(c);
                // init block (if any)
                if let Some(init_child) = self.extract_contract_init(c) {
                    children.push(init_child);
                }
                // recv handler arms
                children.extend(self.extract_contract_recv_handlers(c));
                children
            }
            ItemKind::Enum(e) => self.extract_enum_variants(e),
            ItemKind::Trait(t) => self.extract_trait_members(t),
            ItemKind::Impl(i) => self.extract_impl_members(i),
            ItemKind::ImplTrait(it) => self.extract_impl_trait_members(it),
            ItemKind::Mod(m) if is_desugared_msg_mod(self.db, m) => {
                self.extract_msg_variants(m)
            }
            _ => Vec::new(),
        }
    }

    /// Extract the contract `init(...)` block as a `DocChild` of kind `Init`.
    ///
    /// The signature is the source text spanning the `init` keyword through
    /// the closing paren of the params (and `uses (...)` clause if present),
    /// i.e. everything up to the opening `{` of the body.
    fn extract_contract_init(&self, c: Contract<'db>) -> Option<DocChild> {
        let _init = c.init(self.db)?;

        // Full span of the init block AST (includes body). We'll trim to the
        // header by cutting at the first `{` so the signature is stable even
        // if the body uses nested braces.
        let init_span = c.span().init_block().resolve(self.db)?;
        let start: usize = init_span.range.start().into();
        let end: usize = init_span.range.end().into();
        let file_text = init_span.file.text(self.db);
        let slice = file_text.get(start..end)?;
        // Cut at the first `{` to get the header.
        let header_end_rel = slice.find('{').unwrap_or(slice.len());
        let header = slice[..header_end_rel].trim_end().to_string();

        let file_url = init_span.file.url(self.db)?;
        let signature_span = Some(SignatureSpanData {
            file_url: file_url.to_string(),
            byte_start: start,
            byte_end: start + header_end_rel,
        });

        Some(DocChild {
            kind: DocChildKind::Init,
            name: "init".to_string(),
            docs: None,
            signature: header,
            rich_signature: vec![],
            signature_span,
            sig_scope: None,
            visibility: DocVisibility::Public,
        })
    }

    /// Extract each arm of every `recv` block in the contract as a
    /// `DocChild` of kind `RecvHandler`.
    ///
    /// Signature is the source text of the arm header (pattern through the
    /// optional return type and `uses (...)` clause), stopping at the body's
    /// opening brace.
    fn extract_contract_recv_handlers(&self, c: Contract<'db>) -> Vec<DocChild> {
        let mut out = Vec::new();
        let recvs = c.recvs(self.db);
        for (recv_idx, recv) in recvs.data(self.db).iter().enumerate() {
            let msg_type_name = recv
                .msg_path
                .and_then(|p| p.ident(self.db).to_opt())
                .map(|id| id.data(self.db).to_string());

            for (arm_idx, arm) in recv.arms.data(self.db).iter().enumerate() {
                let arm_lazy = c.span().recv(recv_idx).arms().arm(arm_idx);
                let Some(arm_span) = arm_lazy.clone().resolve(self.db) else {
                    continue;
                };

                let start: usize = arm_span.range.start().into();
                let end: usize = arm_span.range.end().into();
                let file_text = arm_span.file.text(self.db);
                let Some(slice) = file_text.get(start..end) else {
                    continue;
                };

                // Header goes up to the body's opening `{`. Use the body
                // span's start (if resolvable) to find it precisely — a
                // naive `slice.find('{')` would stop at the first record
                // pattern brace like `Lock { challenge }`.
                let header_end_abs = arm_lazy
                    .body()
                    .resolve(self.db)
                    .map(|bs| usize::from(bs.range.start()))
                    .unwrap_or(end);
                let header_end_rel = header_end_abs.saturating_sub(start).min(slice.len());
                let header = slice[..header_end_rel].trim_end().to_string();

                // Name: variant identifier when available; fallback to "_"
                // for wildcard arms.
                let name = arm
                    .variant_path(self.db)
                    .and_then(|p| p.ident(self.db).to_opt())
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| {
                        if arm.is_fallback(self.db) {
                            "_".to_string()
                        } else {
                            format!("arm{}_{}", recv_idx, arm_idx)
                        }
                    });

                // Disambiguate duplicate arm names across multiple recv blocks
                // (rare, but possible if the same msg type is handled twice).
                let qualified_name = if let Some(ref msg) = msg_type_name {
                    format!("{}::{}", msg, name)
                } else {
                    name.clone()
                };

                let file_url = arm_span.file.url(self.db);
                let signature_span = file_url.map(|u| SignatureSpanData {
                    file_url: u.to_string(),
                    byte_start: start,
                    byte_end: start + header_end_rel,
                });

                out.push(DocChild {
                    kind: DocChildKind::RecvHandler,
                    name: qualified_name,
                    docs: None,
                    signature: header,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                });
            }
        }
        out
    }

    /// Extract variants from a desugared `msg` Mod as `DocChild::Variant`.
    ///
    /// A `msg` block desugars into a `Mod` whose children are the per-variant
    /// structs (plus internal trait impls). For docs we surface each variant
    /// struct as a child of the msg DocItem so the `msg` page renders like an
    /// `enum` page — variants listed inline rather than as separate items.
    fn extract_msg_variants(&self, m: hir::hir_def::Mod<'db>) -> Vec<DocChild> {
        let mut out = Vec::new();
        for child in m.children_non_nested(self.db) {
            let ItemKind::Struct(s) = child else { continue };
            if !is_desugared_msg_variant_struct(self.db, s) {
                continue;
            }
            let Some(name_ident) = s.name(self.db).to_opt() else {
                continue;
            };
            let name = name_ident.data(self.db).to_string();
            let docs = self
                .get_docstring(s.scope())
                .map(|s| DocContent::from_raw(&s));
            let (signature, signature_span) = self.get_signature_with_span(child);

            out.push(DocChild {
                kind: DocChildKind::Variant,
                name,
                docs,
                signature,
                rich_signature: vec![],
                signature_span,
                sig_scope: None,
                visibility: DocVisibility::Public,
            });
        }
        out
    }

    fn extract_struct_fields(&self, s: Struct<'db>) -> Vec<DocChild> {
        // Use FieldParent to access fields through the public API
        let parent = FieldParent::Struct(s);
        parent
            .fields(self.db)
            .filter_map(|field_view| {
                let name = field_view.name(self.db)?.data(self.db).to_string();
                let scope = ScopeId::Field(parent, field_view.idx as u16);
                let docs = self.get_docstring(scope).map(|s| DocContent::from_raw(&s));
                let type_text = self
                    .get_field_type_text(field_view)
                    .unwrap_or_else(|| "?".to_string());
                let signature = format!("{}: {}", name, type_text);
                // Get visibility from the scope's data
                let visibility = scope.data(self.db).vis;

                let signature_span = self.field_sig_span(field_view);

                Some(DocChild {
                    kind: DocChildKind::Field,
                    name,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility: self.convert_visibility(visibility),
                })
            })
            .collect()
    }

    fn extract_contract_fields(&self, c: Contract<'db>) -> Vec<DocChild> {
        let parent = FieldParent::Contract(c);
        parent
            .fields(self.db)
            .filter_map(|field_view| {
                let name = field_view.name(self.db)?.data(self.db).to_string();
                let scope = ScopeId::Field(parent, field_view.idx as u16);
                let docs = self.get_docstring(scope).map(|s| DocContent::from_raw(&s));
                let type_text = self
                    .get_field_type_text(field_view)
                    .unwrap_or_else(|| "?".to_string());
                let signature = format!("{}: {}", name, type_text);
                let visibility = scope.data(self.db).vis;

                let signature_span = self.field_sig_span(field_view);

                Some(DocChild {
                    kind: DocChildKind::Field,
                    name,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility: self.convert_visibility(visibility),
                })
            })
            .collect()
    }

    fn extract_enum_variants(&self, e: Enum<'db>) -> Vec<DocChild> {
        // Use the public variants() method that returns VariantView
        e.variants(self.db)
            .filter_map(|variant_view| {
                let name = variant_view.name(self.db)?.data(self.db).to_string();
                let enum_variant = hir::hir_def::EnumVariant::new(e, variant_view.idx);
                let docs = self
                    .get_docstring(enum_variant.scope())
                    .map(|s| DocContent::from_raw(&s));

                let signature = match variant_view.kind(self.db) {
                    VariantKind::Unit => name.clone(),
                    VariantKind::Tuple(tuple_id) => {
                        // Extract actual type text from the source
                        let type_texts: Vec<String> = (0..tuple_id.len(self.db))
                            .filter_map(|i| self.get_tuple_elem_type_text(&enum_variant, i))
                            .collect();
                        if type_texts.is_empty() {
                            format!("{}(...)", name)
                        } else {
                            format!("{}({})", name, type_texts.join(", "))
                        }
                    }
                    VariantKind::Record(_) => format!("{} {{ ... }}", name),
                };

                let signature_span = enum_variant
                    .span()
                    .resolve(self.db)
                    .and_then(|s| self.span_to_sig_data(&s));

                Some(DocChild {
                    kind: DocChildKind::Variant,
                    name,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                })
            })
            .collect()
    }

    /// Extract the type text from a tuple variant element's type span
    fn get_tuple_elem_type_text(
        &self,
        enum_variant: &hir::hir_def::EnumVariant<'db>,
        idx: usize,
    ) -> Option<String> {
        let ty_span = enum_variant
            .span()
            .tuple_type()
            .elem_ty(idx)
            .resolve(self.db)?;
        let start: usize = ty_span.range.start().into();
        let end: usize = ty_span.range.end().into();
        let file_text = ty_span.file.text(self.db).as_str();

        Some(file_text.get(start..end)?.trim().to_string())
    }

    fn extract_trait_members(&self, t: Trait<'db>) -> Vec<DocChild> {
        let mut children = Vec::new();

        // Methods
        for func in t.methods(self.db) {
            if let Some(name) = func.name(self.db).to_opt() {
                let name_str = name.data(self.db).to_string();
                let docs = self
                    .get_docstring(func.scope())
                    .map(|s| DocContent::from_raw(&s));
                let (signature, signature_span) = self.get_signature_with_span(func.into());

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                });
            }
        }

        // Associated types - use the public assoc_types() method
        for (idx, assoc_ty) in t.assoc_types(self.db).enumerate() {
            if let Some(name) = assoc_ty.name(self.db) {
                let name_str = name.data(self.db).to_string();
                let scope = ScopeId::TraitType(t, idx as u16);
                let docs = self.get_docstring(scope).map(|s| DocContent::from_raw(&s));

                children.push(DocChild {
                    kind: DocChildKind::AssocType,
                    name: name_str.clone(),
                    docs,
                    signature: format!("type {}", name_str),
                    rich_signature: vec![],
                    signature_span: None,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                });
            }
        }

        children
    }

    fn extract_impl_members(&self, i: Impl<'db>) -> Vec<DocChild> {
        let mut children = Vec::new();

        for func in i.funcs(self.db) {
            if let Some(name) = func.name(self.db).to_opt() {
                let name_str = name.data(self.db).to_string();
                let docs = self
                    .get_docstring(func.scope())
                    .map(|s| DocContent::from_raw(&s));
                let (signature, signature_span) = self.get_signature_with_span(func.into());
                let visibility = self.convert_visibility(func.vis(self.db));

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility,
                });
            }
        }

        children
    }

    fn extract_impl_trait_members(&self, it: ImplTrait<'db>) -> Vec<DocChild> {
        let mut children = Vec::new();

        for func in it.methods(self.db) {
            if let Some(name) = func.name(self.db).to_opt() {
                let name_str = name.data(self.db).to_string();
                let docs = self
                    .get_docstring(func.scope())
                    .map(|s| DocContent::from_raw(&s));
                let (signature, signature_span) = self.get_signature_with_span(func.into());
                let visibility = self.convert_visibility(func.vis(self.db));

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
                    rich_signature: vec![],
                    signature_span,
                    sig_scope: None,
                    visibility,
                });
            }
        }

        children
    }

    fn get_source_location(&self, item: ItemKind<'db>) -> Option<DocSourceLoc> {
        let span = item.span().resolve(self.db)?;
        let byte_offset: usize = span.range.start().into();
        let file_text = span.file.text(self.db);

        // Get absolute file path from URL (not the relative path from file.path())
        let file_url = span.file.url(self.db)?;
        let file_path = file_url.to_file_path().ok()?;
        let file_str = file_path.to_string_lossy().to_string();

        // Compute relative display path
        let display_file = if let Some(ref root) = self.root_path {
            file_path
                .strip_prefix(root)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| file_str.clone())
        } else {
            // Fallback: just use the filename
            file_path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_else(|| file_str.clone())
        };

        // Convert byte offset to 1-based line number
        let clamped = byte_offset.min(file_text.len());
        let prefix = file_text.get(..clamped).unwrap_or(file_text);
        let line = prefix.chars().filter(|&c| c == '\n').count() as u32 + 1;

        // Calculate column (bytes from start of line)
        let line_start = prefix.rfind('\n').map(|pos| pos + 1).unwrap_or(0);
        let column = (byte_offset - line_start) as u32;

        Some(DocSourceLoc {
            file: file_str,
            display_file,
            line,
            column,
        })
    }

    /// Get the full source text of an item definition.
    fn get_source_text(&self, item: ItemKind<'db>) -> Option<String> {
        let span = item.span().resolve(self.db)?;
        let start: usize = span.range.start().into();
        let end: usize = span.range.end().into();
        let text = span.file.text(self.db);
        Some(text.get(start..end)?.replace("\r\n", "\n"))
    }

    /// Build module tree for navigation sidebar (single module, no file-based children)
    fn build_module_tree(&self, top_mod: TopLevelMod<'db>) -> Vec<DocModuleTree> {
        vec![self.build_module_node(top_mod.into())]
    }

    /// Build module tree for a standalone .fe file (no ingot context).
    pub fn build_standalone_module_tree(&self, top_mod: TopLevelMod<'db>) -> DocModuleTree {
        self.build_module_node(top_mod.into())
    }

    /// Build module tree for an entire ingot (includes file-based child modules)
    pub fn build_module_tree_for_ingot(
        &self,
        ingot: Ingot<'db>,
        root_mod: TopLevelMod<'db>,
    ) -> Vec<DocModuleTree> {
        vec![self.build_module_node_for_ingot(ingot, root_mod)]
    }

    /// Should this child be skipped when populating a parent module's item
    /// list in the nav tree? Excludes msg-desugared synthetic impls and the
    /// per-variant structs (which are siblings of the msg Mod in the HIR but
    /// should only appear as children of the Msg item, not as top-level
    /// entries in the enclosing module's sidebar).
    fn should_skip_module_item(&self, item: ItemKind<'db>) -> bool {
        // Filter out `#[test]` functions by default so the nav tree is not
        // dominated by test_* entries.
        if self.is_filtered_test_item(item) {
            return true;
        }
        match item {
            ItemKind::Struct(s) if is_desugared_msg_variant_struct(self.db, s) => true,
            ItemKind::ImplTrait(it) if is_desugared_msg_impl_trait(self.db, it) => true,
            ItemKind::Impl(i) if matches!(
                span::impl_ast(self.db, i),
                HirOrigin::Desugared(DesugaredOrigin::Msg(_))
            ) =>
            {
                true
            }
            _ => false,
        }
    }

    /// Emit a DocModuleItem entry into `items` for `child`.
    fn push_item_entry(
        &self,
        ingot: Ingot<'db>,
        child: ItemKind<'db>,
        items: &mut Vec<DocModuleItem>,
    ) {
        if let (Some(name), Some(kind)) =
            (child.name(self.db), self.item_kind_to_doc_kind(child))
        {
            let raw_child_path = child.scope().pretty_path(self.db).unwrap_or_default();
            let child_path = self.qualify_path_with_ingot(&raw_child_path, ingot);
            let summary = self.get_summary(child.scope());
            items.push(DocModuleItem {
                name: name.data(self.db).to_string(),
                path: child_path,
                kind,
                summary,
            });
        }
    }

    /// Build a module node including file-based children from the ingot's module tree
    fn build_module_node_for_ingot(
        &self,
        ingot: Ingot<'db>,
        top_mod: TopLevelMod<'db>,
    ) -> DocModuleTree {
        let scope = top_mod.scope();
        let module_tree = ingot.module_tree(self.db);
        let is_root = module_tree.parent(top_mod).is_none();

        // For root module, use ingot config name instead of "lib"
        let name = if is_root {
            ingot
                .config(self.db)
                .and_then(|c| c.metadata.name)
                .map(|s| s.to_string())
                .unwrap_or_else(|| top_mod.name(self.db).data(self.db).to_string())
        } else {
            top_mod.name(self.db).data(self.db).to_string()
        };

        // Qualify path with ingot name
        let raw_path = scope.pretty_path(self.db).unwrap_or_else(|| name.clone());
        let path = self.qualify_path_with_ingot(&raw_path, ingot);

        let mut children = Vec::new();
        let mut items = Vec::new();

        // Get inline children (defined in this file)
        for child in top_mod.children_non_nested(self.db) {
            match child {
                ItemKind::Mod(m) if is_desugared_msg_mod(self.db, m) => {
                    // A msg block desugars to a Mod; surface it as an item
                    // (kind: msg) in the parent rather than a sub-module, so
                    // nav links resolve to the msg DocItem instead of a
                    // non-existent `.../mod` URL.
                    self.push_item_entry(ingot, child, &mut items);
                }
                ItemKind::Mod(_) => {
                    // Use ingot-aware builder for inline modules too
                    children.push(self.build_module_node_for_ingot_inline(ingot, child));
                }
                ItemKind::StaticAssert(_)
                | ItemKind::Use(_)
                | ItemKind::Body(_)
                | ItemKind::TopMod(_) => {}
                _ => {
                    if self.should_skip_module_item(child) {
                        continue;
                    }
                    self.push_item_entry(ingot, child, &mut items);
                }
            }
        }

        // Get file-based children from ingot's module tree
        let module_tree = ingot.module_tree(self.db);
        for child_mod in module_tree.children(top_mod) {
            children.push(self.build_module_node_for_ingot(ingot, child_mod));
        }

        // Sort for consistent ordering
        children.sort_by(|a, b| a.name.cmp(&b.name));
        items.sort_by(|a, b| {
            a.kind
                .as_str()
                .cmp(b.kind.as_str())
                .then_with(|| a.name.cmp(&b.name))
        });

        DocModuleTree {
            name,
            path,
            children,
            items,
        }
    }

    /// Build a module node for an inline module with ingot-qualified paths
    fn build_module_node_for_ingot_inline(
        &self,
        ingot: Ingot<'db>,
        item: ItemKind<'db>,
    ) -> DocModuleTree {
        let scope = item.scope();
        let name = item
            .name(self.db)
            .map(|n| n.data(self.db).to_string())
            .unwrap_or_else(|| "root".to_string());
        let raw_path = scope.pretty_path(self.db).unwrap_or_else(|| name.clone());
        let path = self.qualify_path_with_ingot(&raw_path, ingot);

        let mut children = Vec::new();
        let mut items = Vec::new();

        // Get direct children
        let direct_children: Vec<_> = match item {
            ItemKind::TopMod(tm) => tm.children_non_nested(self.db).collect(),
            ItemKind::Mod(m) => m.children_non_nested(self.db).collect(),
            _ => Vec::new(),
        };

        for child in direct_children {
            match child {
                ItemKind::Mod(m) if is_desugared_msg_mod(self.db, m) => {
                    self.push_item_entry(ingot, child, &mut items);
                }
                ItemKind::Mod(_) | ItemKind::TopMod(_) => {
                    children.push(self.build_module_node_for_ingot_inline(ingot, child));
                }
                ItemKind::StaticAssert(_) | ItemKind::Use(_) | ItemKind::Body(_) => {}
                _ => {
                    if self.should_skip_module_item(child) {
                        continue;
                    }
                    self.push_item_entry(ingot, child, &mut items);
                }
            }
        }

        // Sort for consistent ordering
        children.sort_by(|a, b| a.name.cmp(&b.name));
        items.sort_by(|a, b| {
            a.kind
                .as_str()
                .cmp(b.kind.as_str())
                .then_with(|| a.name.cmp(&b.name))
        });

        DocModuleTree {
            name,
            path,
            children,
            items,
        }
    }

    fn build_module_node(&self, item: ItemKind<'db>) -> DocModuleTree {
        let scope = item.scope();
        let name = item
            .name(self.db)
            .map(|n| n.data(self.db).to_string())
            .unwrap_or_else(|| "root".to_string());
        let path = scope.pretty_path(self.db).unwrap_or_else(|| name.clone());

        let mut children = Vec::new();
        let mut items = Vec::new();

        // Get direct children
        let direct_children: Vec<_> = match item {
            ItemKind::TopMod(tm) => tm.children_non_nested(self.db).collect(),
            ItemKind::Mod(m) => m.children_non_nested(self.db).collect(),
            _ => Vec::new(),
        };

        for child in direct_children {
            match child {
                ItemKind::Mod(m) if is_desugared_msg_mod(self.db, m) => {
                    if let (Some(name), Some(kind)) =
                        (child.name(self.db), self.item_kind_to_doc_kind(child))
                    {
                        let child_path = child.scope().pretty_path(self.db).unwrap_or_default();
                        let summary = self.get_summary(child.scope());
                        items.push(DocModuleItem {
                            name: name.data(self.db).to_string(),
                            path: child_path,
                            kind,
                            summary,
                        });
                    }
                }
                ItemKind::Mod(_) | ItemKind::TopMod(_) => {
                    children.push(self.build_module_node(child));
                }
                ItemKind::StaticAssert(_) | ItemKind::Use(_) | ItemKind::Body(_) => {}
                _ => {
                    if self.should_skip_module_item(child) {
                        continue;
                    }
                    if let (Some(name), Some(kind)) =
                        (child.name(self.db), self.item_kind_to_doc_kind(child))
                    {
                        let child_path = child.scope().pretty_path(self.db).unwrap_or_default();
                        let summary = self.get_summary(child.scope());
                        items.push(DocModuleItem {
                            name: name.data(self.db).to_string(),
                            path: child_path,
                            kind,
                            summary,
                        });
                    }
                }
            }
        }

        // Sort for consistent ordering
        children.sort_by(|a, b| a.name.cmp(&b.name));
        items.sort_by(|a, b| {
            // Sort by kind first, then by name
            a.kind
                .as_str()
                .cmp(b.kind.as_str())
                .then_with(|| a.name.cmp(&b.name))
        });

        DocModuleTree {
            name,
            path,
            children,
            items,
        }
    }
}

/// Returns true if `m` is a `Mod` synthesized from a `msg` block.
///
/// A `msg` block desugars into a `Mod` containing per-variant structs and
/// their `impl MsgVariant` blocks. We treat that `Mod` as a single
/// `DocItemKind::Msg` item (not as a navigable sub-module) so the user sees
/// one "Message" entry in the sidebar rather than a phantom sub-module.
fn is_desugared_msg_mod<'db>(db: &'db dyn SpannedHirDb, m: hir::hir_def::Mod<'db>) -> bool {
    matches!(
        span::mod_ast(db, m),
        HirOrigin::Desugared(DesugaredOrigin::Msg(_))
    )
}

/// Returns true if `s` is a `Struct` synthesized from one variant of a `msg`
/// block (the per-variant payload struct).
fn is_desugared_msg_variant_struct<'db>(db: &'db dyn SpannedHirDb, s: Struct<'db>) -> bool {
    matches!(
        span::struct_ast(db, s),
        HirOrigin::Desugared(DesugaredOrigin::Msg(msg)) if msg.variant_idx.is_some()
    )
}

/// Returns true if `it` is an `ImplTrait` synthesized by the msg desugaring
/// (the per-variant `impl MsgVariant for V` block). These are internal and
/// should not appear in docs.
fn is_desugared_msg_impl_trait<'db>(db: &'db dyn SpannedHirDb, it: ImplTrait<'db>) -> bool {
    matches!(
        span::impl_trait_ast(db, it),
        HirOrigin::Desugared(DesugaredOrigin::Msg(_))
    )
}

/// Extract the simple name from a potentially qualified/generic type.
/// "mod::MyStruct<T>" -> "MyStruct"
/// "Option<T>" -> "Option"
fn extract_simple_name(s: &str) -> String {
    // Remove generic params first
    let without_generics = s.split('<').next().unwrap_or(s);
    // Get last path segment
    without_generics
        .rsplit("::")
        .next()
        .unwrap_or(without_generics)
        .trim()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use dir_test::{Fixture, dir_test};
    use driver::DriverDataBase;
    use fe_web::model::DocIndex;
    use test_utils::snap_test;

    fn normalize_paths(output: &str) -> String {
        let fixtures_dir =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/doc_fixtures");
        output.replace(&fixtures_dir.to_string_lossy().to_string(), "<fixtures>")
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/tests/doc_fixtures",
        glob: "*.fe",
    )]
    fn doc_extract(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let url = url::Url::from_file_path(fixture.path()).expect("path should be absolute");
        let file = db
            .workspace()
            .touch(&mut db, url, Some(fixture.content().to_string()));
        let top_mod = db.top_mod(file);

        let extractor = DocExtractor::new(&db);
        let index = extractor.extract_module(top_mod);

        let json = serde_json::to_string_pretty(&index).expect("serialize DocIndex");
        let output = normalize_paths(&json);
        snap_test!(output, fixture.path());
    }

    /// Integration test: extract → static site generation
    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/tests/doc_fixtures",
        glob: "*.fe",
    )]
    fn doc_static_site(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let url = url::Url::from_file_path(fixture.path()).expect("path should be absolute");
        let file = db
            .workspace()
            .touch(&mut db, url, Some(fixture.content().to_string()));
        let top_mod = db.top_mod(file);

        let extractor = DocExtractor::new(&db);
        let index = extractor.extract_module(top_mod);

        let test_name = std::path::Path::new(fixture.path())
            .file_stem()
            .unwrap()
            .to_string_lossy();
        let dir = std::env::temp_dir().join(format!("fe_doc_static_{}", test_name));
        let _ = std::fs::remove_dir_all(&dir);

        fe_web::static_site::StaticSiteGenerator::generate(&index, &dir)
            .expect("static site generation failed");

        let html_path = dir.join("index.html");
        assert!(html_path.exists(), "index.html should exist");

        let html = std::fs::read_to_string(&html_path).unwrap();
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("FE_DOC_INDEX"));
        assert!(html.contains("fe-doc-viewer"));

        for item in &index.items {
            assert!(
                html.contains(&item.path),
                "HTML should contain item path: {}",
                item.path
            );
        }

        if index.items.iter().any(|i| i.docs.is_some()) {
            assert!(
                html.contains("html_body"),
                "should contain pre-rendered html_body"
            );
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    /// Integration test: extract → JSON → deserialize round-trip
    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/tests/doc_fixtures",
        glob: "*.fe",
    )]
    fn doc_round_trip(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let url = url::Url::from_file_path(fixture.path()).expect("path should be absolute");
        let file = db
            .workspace()
            .touch(&mut db, url, Some(fixture.content().to_string()));
        let top_mod = db.top_mod(file);

        let extractor = DocExtractor::new(&db);
        let original = extractor.extract_module(top_mod);

        let json = serde_json::to_string(&original).expect("serialize DocIndex");
        let restored: DocIndex = serde_json::from_str(&json).expect("deserialize DocIndex");

        assert_eq!(
            original.items.len(),
            restored.items.len(),
            "item count mismatch for {}",
            fixture.path()
        );

        for (a, b) in original.items.iter().zip(restored.items.iter()) {
            assert_eq!(a.path, b.path, "path mismatch");
            assert_eq!(a.name, b.name, "name mismatch");
            assert_eq!(a.kind, b.kind, "kind mismatch");
            assert_eq!(
                a.children.len(),
                b.children.len(),
                "children count mismatch for {}",
                a.path
            );
        }

        assert_eq!(
            original.modules.len(),
            restored.modules.len(),
            "module tree mismatch"
        );
    }
}
