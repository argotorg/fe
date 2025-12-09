//! Documentation extraction from HIR
//!
//! This module traverses the Fe HIR (High-level Intermediate Representation) and extracts
//! documentation into the `DocItem` model suitable for rendering.

use common::ingot::Ingot;
use hir::{
    hir_def::{
        scope_graph::ScopeId, Attr, Contract, Enum, FieldParent, HirIngot, Impl, ImplTrait,
        ItemKind, Struct, TopLevelMod, Trait, VariantKind, Visibility,
    },
    span::LazySpan,
    SpannedHirDb,
};

use crate::model::{
    DocChild, DocChildKind, DocContent, DocGenericParam, DocIndex, DocItem, DocItemKind,
    DocModuleItem, DocModuleTree, DocSourceLoc, DocTraitImpl, DocVisibility,
};

/// Extracts documentation from a Fe HIR database
pub struct DocExtractor<'db> {
    db: &'db dyn SpannedHirDb,
    /// Root path for computing relative display paths
    root_path: Option<std::path::PathBuf>,
}

impl<'db> DocExtractor<'db> {
    pub fn new(db: &'db dyn SpannedHirDb) -> Self {
        Self { db, root_path: None }
    }

    /// Set the root path for computing relative display paths
    pub fn with_root_path(mut self, root: impl Into<std::path::PathBuf>) -> Self {
        self.root_path = Some(root.into());
        self
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
    pub fn extract_trait_impl_links(
        &self,
        ingot: Ingot<'db>,
    ) -> Vec<(String, DocTraitImpl)> {
        let mut links = Vec::new();

        for &impl_trait in ingot.all_impl_traits(self.db) {
            let Some(trait_name) = impl_trait.trait_name(self.db) else {
                continue;
            };
            let Some(target_type) = impl_trait.target_type_name(self.db) else {
                continue;
            };

            // Get the impl's path for linking
            let impl_path = impl_trait
                .scope()
                .pretty_path(self.db)
                .unwrap_or_default();

            let signature = self.get_signature(impl_trait.into());

            links.push((
                target_type,
                DocTraitImpl {
                    trait_name,
                    impl_url: format!("{}/impl", impl_path),
                    signature,
                },
            ));
        }

        links
    }

    /// Extract documentation for a single item
    pub fn extract_item(&self, item: ItemKind<'db>) -> Option<DocItem> {
        // Skip items that shouldn't be documented
        match item {
            ItemKind::Use(_) | ItemKind::Body(_) => return None,
            _ => {}
        }

        let scope = item.scope();
        let path = scope.pretty_path(self.db)?;
        let name = item.name(self.db)?.data(self.db).to_string();
        let kind = self.item_kind_to_doc_kind(item)?;
        let visibility = self.convert_visibility(item.vis(self.db));
        let docs = self.get_docstring(scope).map(|s| DocContent::from_raw(&s));
        let signature = self.get_signature(item);
        let generics = self.extract_generics(item);
        let where_bounds = self.extract_where_bounds(item);
        let children = self.extract_children(item);
        let source = self.get_source_location(item);

        Some(DocItem {
            path,
            name,
            kind,
            visibility,
            docs,
            signature,
            generics,
            where_bounds,
            children,
            source,
            trait_impls: Vec::new(), // Populated later by link_trait_impls
        })
    }

    fn item_kind_to_doc_kind(&self, item: ItemKind<'db>) -> Option<DocItemKind> {
        match item {
            ItemKind::TopMod(_) | ItemKind::Mod(_) => Some(DocItemKind::Module),
            ItemKind::Func(_) => Some(DocItemKind::Function),
            ItemKind::Struct(_) => Some(DocItemKind::Struct),
            ItemKind::Enum(_) => Some(DocItemKind::Enum),
            ItemKind::Trait(_) => Some(DocItemKind::Trait),
            ItemKind::Contract(_) => Some(DocItemKind::Contract),
            ItemKind::TypeAlias(_) => Some(DocItemKind::TypeAlias),
            ItemKind::Const(_) => Some(DocItemKind::Const),
            ItemKind::Impl(_) => Some(DocItemKind::Impl),
            ItemKind::ImplTrait(_) => Some(DocItemKind::ImplTrait),
            ItemKind::Use(_) | ItemKind::Body(_) => None,
        }
    }

    fn convert_visibility(&self, vis: Visibility) -> DocVisibility {
        match vis {
            Visibility::Public => DocVisibility::Public,
            Visibility::Private => DocVisibility::Private,
        }
    }

    /// Extract doc comments from a scope's attributes
    fn get_docstring(&self, scope: ScopeId<'db>) -> Option<String> {
        scope
            .attrs(self.db)?
            .data(self.db)
            .iter()
            .filter_map(|attr| {
                if let Attr::DocComment(doc) = attr {
                    Some(doc.text.data(self.db).clone())
                } else {
                    None
                }
            })
            .reduce(|a, b| a + "\n" + &b)
    }

    /// Get the item's signature (definition without body)
    fn get_signature(&self, item: ItemKind<'db>) -> String {
        let span = match item.span().resolve(self.db) {
            Some(s) => s,
            None => return String::new(),
        };

        let mut start: usize = span.range.start().into();
        let mut end: usize = span.range.end().into();

        // Trim body for items that have one
        let body_start = match item {
            ItemKind::Func(func) => func
                .body(self.db)
                .and_then(|b| b.span().resolve(self.db))
                .map(|s| s.range.start()),
            ItemKind::Mod(module) => module
                .scope()
                .name_span(self.db)
                .and_then(|s| s.resolve(self.db))
                .map(|s| s.range.end()),
            _ => None,
        };

        if let Some(body_start) = body_start {
            end = body_start.into();
        }

        // Start at beginning of the line where name is defined
        if let Some(name_span) = item.name_span().and_then(|s| s.resolve(self.db)) {
            let mut name_line_start: usize = name_span.range.start().into();
            let file_text = span.file.text(self.db).as_str();
            while name_line_start > 0
                && file_text.chars().nth(name_line_start - 1) != Some('\n')
            {
                name_line_start -= 1;
            }
            start = name_line_start;
        }

        let file_text = span.file.text(self.db).as_str();
        if end > file_text.len() {
            end = file_text.len();
        }
        if start > end {
            start = end;
        }

        file_text[start..end].trim().to_string()
    }

    /// Extract generic parameters from an item
    fn extract_generics(&self, item: ItemKind<'db>) -> Vec<DocGenericParam> {
        // Get generics from scope's children
        let scope = item.scope();
        let mut params = Vec::new();

        for child_scope in scope.children(self.db) {
            if let ScopeId::GenericParam(_, _) = child_scope {
                if let Some(name) = child_scope.name(self.db) {
                    params.push(DocGenericParam {
                        name: name.data(self.db).to_string(),
                        bounds: Vec::new(),
                        default: None,
                    });
                }
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
            ItemKind::Contract(c) => self.extract_contract_fields(c),
            ItemKind::Enum(e) => self.extract_enum_variants(e),
            ItemKind::Trait(t) => self.extract_trait_members(t),
            ItemKind::Impl(i) => self.extract_impl_members(i),
            ItemKind::ImplTrait(it) => self.extract_impl_trait_members(it),
            _ => Vec::new(),
        }
    }

    fn extract_struct_fields(&self, s: Struct<'db>) -> Vec<DocChild> {
        // Use FieldParent to access fields through the public API
        let parent = FieldParent::Struct(s);
        parent
            .fields(self.db)
            .filter_map(|field_view| {
                let name = field_view.name(self.db)?.data(self.db).to_string();
                let scope = ScopeId::Field(parent, field_view.idx as u16);
                let docs = self.get_docstring(scope);
                let signature = format!("{}: /* type */", name); // TODO: render type
                // Get visibility from the scope's data
                let visibility = scope.data(self.db).vis;

                Some(DocChild {
                    kind: DocChildKind::Field,
                    name,
                    docs,
                    signature,
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
                let docs = self.get_docstring(scope);
                let signature = format!("{}: /* type */", name);
                let visibility = scope.data(self.db).vis;

                Some(DocChild {
                    kind: DocChildKind::Field,
                    name,
                    docs,
                    signature,
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
                let docs = self.get_docstring(enum_variant.scope());

                let signature = match variant_view.kind(self.db) {
                    VariantKind::Unit => name.clone(),
                    VariantKind::Tuple(_) => format!("{}(...)", name),
                    VariantKind::Record(_) => format!("{} {{ ... }}", name),
                };

                Some(DocChild {
                    kind: DocChildKind::Variant,
                    name,
                    docs,
                    signature,
                    visibility: DocVisibility::Public,
                })
            })
            .collect()
    }

    fn extract_trait_members(&self, t: Trait<'db>) -> Vec<DocChild> {
        let mut children = Vec::new();

        // Methods
        for func in t.methods(self.db) {
            if let Some(name) = func.name(self.db).to_opt() {
                let name_str = name.data(self.db).to_string();
                let docs = self.get_docstring(func.scope());
                let signature = self.get_signature(func.into());

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
                    visibility: DocVisibility::Public,
                });
            }
        }

        // Associated types - use the public assoc_types() method
        for (idx, assoc_ty) in t.assoc_types(self.db).enumerate() {
            if let Some(name) = assoc_ty.name(self.db) {
                let name_str = name.data(self.db).to_string();
                let scope = ScopeId::TraitType(t, idx as u16);
                let docs = self.get_docstring(scope);

                children.push(DocChild {
                    kind: DocChildKind::AssocType,
                    name: name_str.clone(),
                    docs,
                    signature: format!("type {}", name_str),
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
                let docs = self.get_docstring(func.scope());
                let signature = self.get_signature(func.into());
                let visibility = self.convert_visibility(func.vis(self.db));

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
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
                let docs = self.get_docstring(func.scope());
                let signature = self.get_signature(func.into());
                let visibility = self.convert_visibility(func.vis(self.db));

                children.push(DocChild {
                    kind: DocChildKind::Method,
                    name: name_str,
                    docs,
                    signature,
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
        let line = file_text[..byte_offset.min(file_text.len())]
            .chars()
            .filter(|&c| c == '\n')
            .count() as u32
            + 1;

        // Calculate column (bytes from start of line)
        let line_start = file_text[..byte_offset]
            .rfind('\n')
            .map(|pos| pos + 1)
            .unwrap_or(0);
        let column = (byte_offset - line_start) as u32;

        Some(DocSourceLoc {
            file: file_str,
            display_file,
            line,
            column,
        })
    }

    /// Build module tree for navigation sidebar (single module, no file-based children)
    fn build_module_tree(&self, top_mod: TopLevelMod<'db>) -> Vec<DocModuleTree> {
        vec![self.build_module_node(top_mod.into())]
    }

    /// Build module tree for an entire ingot (includes file-based child modules)
    pub fn build_module_tree_for_ingot(
        &self,
        ingot: Ingot<'db>,
        root_mod: TopLevelMod<'db>,
    ) -> Vec<DocModuleTree> {
        vec![self.build_module_node_for_ingot(ingot, root_mod)]
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
        let path = scope.pretty_path(self.db).unwrap_or_else(|| name.clone());

        let mut children = Vec::new();
        let mut items = Vec::new();

        // Get inline children (defined in this file)
        for child in top_mod.children_non_nested(self.db) {
            match child {
                ItemKind::Mod(_) => {
                    children.push(self.build_module_node(child));
                }
                ItemKind::Use(_) | ItemKind::Body(_) | ItemKind::TopMod(_) => {}
                _ => {
                    if let (Some(name), Some(kind)) =
                        (child.name(self.db), self.item_kind_to_doc_kind(child))
                    {
                        let child_path = child.scope().pretty_path(self.db).unwrap_or_default();
                        items.push(DocModuleItem {
                            name: name.data(self.db).to_string(),
                            path: child_path,
                            kind,
                        });
                    }
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

    fn build_module_node(&self, item: ItemKind<'db>) -> DocModuleTree {
        let scope = item.scope();
        let name = item
            .name(self.db)
            .map(|n| n.data(self.db).to_string())
            .unwrap_or_else(|| "root".to_string());
        let path = scope
            .pretty_path(self.db)
            .unwrap_or_else(|| name.clone());

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
                ItemKind::Mod(_) | ItemKind::TopMod(_) => {
                    children.push(self.build_module_node(child));
                }
                ItemKind::Use(_) | ItemKind::Body(_) => {}
                _ => {
                    if let (Some(name), Some(kind)) =
                        (child.name(self.db), self.item_kind_to_doc_kind(child))
                    {
                        let child_path = child
                            .scope()
                            .pretty_path(self.db)
                            .unwrap_or_default();
                        items.push(DocModuleItem {
                            name: name.data(self.db).to_string(),
                            path: child_path,
                            kind,
                        });
                    }
                }
            }
        }

        // Sort for consistent ordering
        children.sort_by(|a, b| a.name.cmp(&b.name));
        items.sort_by(|a, b| {
            // Sort by kind first, then by name
            a.kind.as_str().cmp(b.kind.as_str())
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
