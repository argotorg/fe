//! Documentation data model
//!
//! These types represent extracted documentation in a format suitable for rendering.
//! They are designed to be serializable for static site generation and cacheable
//! for dynamic serving.

use serde::{Deserialize, Serialize};

// ============================================================================
// Rich Signature Types (for rendering signatures with embedded links)
// ============================================================================

/// A part of a signature - either plain text or a linkable reference
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SignaturePart {
    /// The display text
    pub text: String,
    /// If Some, render as a link to this doc path (e.g., "hoverable::Numbers/struct")
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub link: Option<String>,
}

impl SignaturePart {
    /// Create a plain text part
    pub fn text(s: impl Into<String>) -> Self {
        Self {
            text: s.into(),
            link: None,
        }
    }

    /// Create a linked part
    pub fn link(text: impl Into<String>, path: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            link: Some(path.into()),
        }
    }
}

/// A rich signature with embedded links
pub type RichSignature = Vec<SignaturePart>;

/// Helper to create a RichSignature from a plain string (no links)
pub fn plain_signature(s: impl Into<String>) -> RichSignature {
    vec![SignaturePart::text(s)]
}

// ============================================================================
// Core Documentation Types
// ============================================================================

/// A documented item in the Fe codebase
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocItem {
    /// Unique path identifier (e.g., "std::option::Option")
    pub path: String,
    /// Short name of the item
    pub name: String,
    /// What kind of item this is
    pub kind: DocItemKind,
    /// The item's visibility
    pub visibility: DocVisibility,
    /// Parsed documentation content
    pub docs: Option<DocContent>,
    /// The item's signature/definition (plain text for backward compat)
    pub signature: String,
    /// Rich signature with embedded links (for rendering)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rich_signature: RichSignature,
    /// Generic parameters, if any
    pub generics: Vec<DocGenericParam>,
    /// Where clause bounds, if any
    pub where_bounds: Vec<String>,
    /// Child items (methods, fields, variants, etc.)
    pub children: Vec<DocChild>,
    /// Source location for "view source" links
    pub source: Option<DocSourceLoc>,
    /// Trait implementations for this type (structs, enums, contracts)
    #[serde(default)]
    pub trait_impls: Vec<DocTraitImpl>,
    /// Types that implement this trait (for trait pages)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub implementors: Vec<DocImplementor>,
}

/// A type that implements a trait
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocImplementor {
    /// The implementing type name
    pub type_name: String,
    /// Path to the type's documentation
    pub type_url: String,
    /// The trait name (for linking to the impl block)
    pub trait_name: String,
    /// The full impl signature (plain text)
    pub signature: String,
    /// Rich signature with embedded links
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rich_signature: RichSignature,
}

impl DocItem {
    /// Get the URL path for this item (includes kind suffix)
    pub fn url_path(&self) -> String {
        format!("{}/{}", self.path, self.kind.as_str())
    }
}

/// The kind of documented item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DocItemKind {
    Module,
    Function,
    Struct,
    Enum,
    Trait,
    Contract,
    TypeAlias,
    Const,
    Impl,
    ImplTrait,
}

impl DocItemKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            DocItemKind::Module => "mod",
            DocItemKind::Function => "fn",
            DocItemKind::Struct => "struct",
            DocItemKind::Enum => "enum",
            DocItemKind::Trait => "trait",
            DocItemKind::Contract => "contract",
            DocItemKind::TypeAlias => "type",
            DocItemKind::Const => "const",
            DocItemKind::Impl => "impl",
            DocItemKind::ImplTrait => "impl",
        }
    }

    /// Parse kind from URL suffix string
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "mod" | "module" => Some(DocItemKind::Module),
            "fn" | "function" => Some(DocItemKind::Function),
            "struct" => Some(DocItemKind::Struct),
            "enum" => Some(DocItemKind::Enum),
            "trait" => Some(DocItemKind::Trait),
            "contract" => Some(DocItemKind::Contract),
            "type" => Some(DocItemKind::TypeAlias),
            "const" => Some(DocItemKind::Const),
            "impl" => Some(DocItemKind::Impl),
            _ => None,
        }
    }

    pub fn display_name(&self) -> &'static str {
        match self {
            DocItemKind::Module => "Module",
            DocItemKind::Function => "Function",
            DocItemKind::Struct => "Struct",
            DocItemKind::Enum => "Enum",
            DocItemKind::Trait => "Trait",
            DocItemKind::Contract => "Contract",
            DocItemKind::TypeAlias => "Type Alias",
            DocItemKind::Const => "Constant",
            DocItemKind::Impl => "Implementation",
            DocItemKind::ImplTrait => "Trait Implementation",
        }
    }

    /// Plural display name for section headers
    pub fn plural_name(&self) -> &'static str {
        match self {
            DocItemKind::Module => "Modules",
            DocItemKind::Function => "Functions",
            DocItemKind::Struct => "Structs",
            DocItemKind::Enum => "Enums",
            DocItemKind::Trait => "Traits",
            DocItemKind::Contract => "Contracts",
            DocItemKind::TypeAlias => "Type Aliases",
            DocItemKind::Const => "Constants",
            DocItemKind::Impl => "Implementations",
            DocItemKind::ImplTrait => "Trait Implementations",
        }
    }

    /// Display order for sidebar grouping (lower = first)
    pub fn display_order(&self) -> u8 {
        match self {
            DocItemKind::Module => 0,
            DocItemKind::Trait => 1,
            DocItemKind::Contract => 2,
            DocItemKind::Struct => 3,
            DocItemKind::Enum => 4,
            DocItemKind::TypeAlias => 5,
            DocItemKind::Function => 6,
            DocItemKind::Const => 7,
            DocItemKind::Impl => 8,
            DocItemKind::ImplTrait => 9,
        }
    }
}

/// Visibility of a documented item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DocVisibility {
    Public,
    Private,
}

/// Parsed documentation content with sections
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocContent {
    /// The main summary (first paragraph)
    pub summary: String,
    /// Full documentation body (markdown)
    pub body: String,
    /// Extracted sections like # Examples, # Panics, etc.
    pub sections: Vec<DocSection>,
}

impl DocContent {
    pub fn from_raw(raw: &str) -> Self {
        let trimmed = raw.trim();

        // Split into summary (first paragraph) and body
        let (summary, body) = if let Some(idx) = trimmed.find("\n\n") {
            (trimmed[..idx].to_string(), trimmed.to_string())
        } else {
            (trimmed.to_string(), trimmed.to_string())
        };

        // Extract known sections
        let sections = Self::extract_sections(trimmed);

        DocContent {
            summary,
            body,
            sections,
        }
    }

    fn extract_sections(text: &str) -> Vec<DocSection> {
        let mut sections = Vec::new();
        let mut current_section: Option<(String, String)> = None;

        for line in text.lines() {
            if let Some(header) = line.strip_prefix("# ") {
                // Save previous section if any
                if let Some((name, content)) = current_section.take() {
                    sections.push(DocSection {
                        name,
                        content: content.trim().to_string(),
                    });
                }
                // Start new section
                let name = header.trim().to_string();
                current_section = Some((name, String::new()));
            } else if let Some((_, ref mut content)) = current_section {
                content.push_str(line);
                content.push('\n');
            }
        }

        // Save final section
        if let Some((name, content)) = current_section {
            sections.push(DocSection {
                name,
                content: content.trim().to_string(),
            });
        }

        sections
    }
}

/// A named section within documentation (e.g., "Examples", "Panics")
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocSection {
    pub name: String,
    pub content: String,
}

/// A generic parameter with its bounds
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocGenericParam {
    pub name: String,
    pub bounds: Vec<String>,
    pub default: Option<String>,
}

/// A child of a documented item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocChild {
    pub kind: DocChildKind,
    pub name: String,
    pub docs: Option<String>,
    pub signature: String,
    /// Rich signature with embedded links
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rich_signature: RichSignature,
    pub visibility: DocVisibility,
}

/// Kind of child item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DocChildKind {
    Field,
    Variant,
    Method,
    AssocType,
    AssocConst,
}

impl DocChildKind {
    pub fn display_name(&self) -> &'static str {
        match self {
            DocChildKind::Field => "Field",
            DocChildKind::Variant => "Variant",
            DocChildKind::Method => "Method",
            DocChildKind::AssocType => "Associated Type",
            DocChildKind::AssocConst => "Associated Constant",
        }
    }

    /// Plural display name for section headers
    pub fn plural_name(&self) -> &'static str {
        match self {
            DocChildKind::Field => "Fields",
            DocChildKind::Variant => "Variants",
            DocChildKind::Method => "Methods",
            DocChildKind::AssocType => "Associated Types",
            DocChildKind::AssocConst => "Associated Constants",
        }
    }

    /// Display order for grouping (lower = first)
    pub fn display_order(&self) -> u8 {
        match self {
            DocChildKind::Variant => 0,
            DocChildKind::Field => 1,
            DocChildKind::AssocType => 2,
            DocChildKind::AssocConst => 3,
            DocChildKind::Method => 4,
        }
    }

    /// Anchor prefix for linking (rustdoc-style)
    pub fn anchor_prefix(&self) -> &'static str {
        match self {
            DocChildKind::Field => "field",
            DocChildKind::Variant => "variant",
            DocChildKind::Method => "tymethod",
            DocChildKind::AssocType => "associatedtype",
            DocChildKind::AssocConst => "associatedconstant",
        }
    }
}

/// Source location for linking to source code
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocSourceLoc {
    /// Absolute file path (used for navigation)
    pub file: String,
    /// Relative display path (shown in UI)
    pub display_file: String,
    pub line: u32,
    pub column: u32,
}

/// A trait implementation reference (shown on type pages)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocTraitImpl {
    /// The name of the trait being implemented (e.g., "Clone"). Empty for inherent impls.
    pub trait_name: String,
    /// URL path to the impl item's documentation
    pub impl_url: String,
    /// The full signature of the impl (e.g., "impl Clone for MyStruct")
    pub signature: String,
    /// Rich signature with embedded links
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rich_signature: RichSignature,
    /// Methods defined in this impl block (displayed inline on type pages)
    #[serde(default)]
    pub methods: Vec<DocImplMethod>,
}

/// A method in an impl block (for inline display on type pages)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DocImplMethod {
    /// Method name
    pub name: String,
    /// Method signature (e.g., "pub fn foo(&self) -> u32")
    pub signature: String,
    /// Rich signature with embedded links
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rich_signature: RichSignature,
    /// Documentation (first paragraph only for summary)
    pub docs: Option<String>,
}

/// A collection of documented items forming a documentation index
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DocIndex {
    /// All documented items, keyed by path
    pub items: Vec<DocItem>,
    /// Module hierarchy for navigation
    pub modules: Vec<DocModuleTree>,
}

impl DocIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_item(&mut self, item: DocItem) {
        self.items.push(item);
    }

    /// Find an item by its path (without kind suffix)
    pub fn find_by_path(&self, path: &str) -> Option<&DocItem> {
        self.items.iter().find(|item| item.path == path)
    }

    /// Find an item by path and kind
    pub fn find_by_path_and_kind(&self, path: &str, kind: DocItemKind) -> Option<&DocItem> {
        self.items
            .iter()
            .find(|item| item.path == path && item.kind == kind)
    }

    /// Parse a URL path (potentially with kind suffix) and find the item.
    /// URL format: "path::to::item" or "path::to::item/kind"
    /// Returns the item if found, handling both formats.
    pub fn find_by_url(&self, url_path: &str) -> Option<&DocItem> {
        // Try to parse kind suffix (e.g., "lib::foo/fn" -> path="lib::foo", kind="fn")
        if let Some((path, kind_str)) = url_path.rsplit_once('/')
            && let Some(kind) = DocItemKind::parse(kind_str)
        {
            // URL has valid kind suffix - find by path and kind
            return self.find_by_path_and_kind(path, kind);
        }
        // No valid kind suffix - find by path alone (may be ambiguous)
        self.find_by_path(url_path)
    }

    /// Find all items with a given path (for disambiguation)
    pub fn find_all_by_path(&self, path: &str) -> Vec<&DocItem> {
        self.items.iter().filter(|item| item.path == path).collect()
    }

    /// Build a searchable index of items
    pub fn search(&self, query: &str) -> Vec<&DocItem> {
        let query_lower = query.to_lowercase();
        self.items
            .iter()
            .filter(|item| {
                item.name.to_lowercase().contains(&query_lower)
                    || item.path.to_lowercase().contains(&query_lower)
            })
            .collect()
    }

    /// Link trait implementations to their target types and implementors to traits.
    /// `links` is a list of (target_type_path, DocTraitImpl) pairs extracted
    /// from the HIR using semantic helpers.
    pub fn link_trait_impls(&mut self, links: Vec<(String, DocTraitImpl)>) {
        // Build a lookup map of type items by simple name for URL resolution
        let type_items: std::collections::HashMap<String, (&str, &DocItemKind)> = self
            .items
            .iter()
            .filter(|item| {
                matches!(
                    item.kind,
                    DocItemKind::Struct | DocItemKind::Enum | DocItemKind::Contract
                )
            })
            .map(|item| {
                let simple_name = extract_simple_type_name(&item.name);
                (simple_name, (item.path.as_str(), &item.kind))
            })
            .collect();

        // Build a lookup map of trait items by simple name for URL resolution
        let trait_items: std::collections::HashMap<String, String> = self
            .items
            .iter()
            .filter(|item| item.kind == DocItemKind::Trait)
            .map(|item| {
                let simple_name = extract_simple_type_name(&item.name);
                (simple_name, item.path.clone())
            })
            .collect();

        // First pass: collect implementors for each trait
        let mut trait_implementors: std::collections::HashMap<String, Vec<DocImplementor>> =
            std::collections::HashMap::new();

        for (target_type, trait_impl) in &links {
            // Skip inherent impls (empty trait_name)
            if trait_impl.trait_name.is_empty() {
                continue;
            }

            let trait_simple_name = extract_simple_type_name(&trait_impl.trait_name);
            let type_simple_name = extract_simple_type_name(target_type);

            // Look up the actual type item to get the correct path and kind
            let (type_path, type_kind_suffix) =
                if let Some(&(path, kind)) = type_items.get(&type_simple_name) {
                    (path, kind.as_str())
                } else {
                    // Fallback to the target_type path with struct suffix
                    (target_type.as_str(), "struct")
                };

            // Look up the actual trait to get the correct path
            let trait_path = trait_items
                .get(&trait_simple_name)
                .cloned()
                .unwrap_or_else(|| trait_impl.trait_name.clone());

            // Build rich signature: "impl Trait for Type"
            let rich_signature = vec![
                SignaturePart::text("impl "),
                SignaturePart::link(&trait_simple_name, format!("{}/trait", trait_path)),
                SignaturePart::text(" for "),
                SignaturePart::link(
                    &type_simple_name,
                    format!("{}/{}", type_path, type_kind_suffix),
                ),
            ];

            // Create implementor entry with correct URL and rich signature
            let implementor = DocImplementor {
                type_name: type_simple_name.clone(),
                type_url: format!("{}/{}", type_path, type_kind_suffix),
                trait_name: trait_simple_name.clone(),
                signature: trait_impl.signature.clone(),
                rich_signature,
            };

            trait_implementors
                .entry(trait_simple_name)
                .or_default()
                .push(implementor);
        }

        // Second pass: link trait impls to types and implementors to traits
        for (target_type, mut trait_impl) in links {
            let target_simple_name = extract_simple_type_name(&target_type);
            let trait_simple_name = extract_simple_type_name(&trait_impl.trait_name);

            for item in &mut self.items {
                // Link trait impls to types (structs, enums, contracts)
                let is_type = matches!(
                    item.kind,
                    DocItemKind::Struct | DocItemKind::Enum | DocItemKind::Contract
                );
                if is_type {
                    let matches = item.path == target_type
                        || item.name == target_simple_name
                        || item.path.ends_with(&format!("::{}", target_simple_name));

                    if matches {
                        // Build rich signature for this trait impl if it's a trait impl (not inherent)
                        if !trait_impl.trait_name.is_empty() && trait_impl.rich_signature.is_empty()
                        {
                            // Look up the trait URL
                            let trait_url = trait_items
                                .get(&trait_simple_name)
                                .map(|p| format!("{}/trait", p))
                                .unwrap_or_else(|| format!("{}/trait", &trait_impl.trait_name));

                            // Use the target item's path and kind for the type URL
                            let type_url = format!("{}/{}", item.path, item.kind.as_str());

                            trait_impl.rich_signature = vec![
                                SignaturePart::text("impl "),
                                SignaturePart::link(&trait_simple_name, trait_url),
                                SignaturePart::text(" for "),
                                SignaturePart::link(&target_simple_name, type_url),
                            ];
                        }
                        item.trait_impls.push(trait_impl.clone());
                    }
                }

                // Link implementors to traits
                if item.kind == DocItemKind::Trait && !trait_impl.trait_name.is_empty() {
                    let trait_matches = item.name == trait_simple_name
                        || item.path.ends_with(&format!("::{}", trait_simple_name));

                    if trait_matches && let Some(impls) = trait_implementors.get(&trait_simple_name)
                    {
                        // Only add if not already present
                        for imp in impls {
                            if !item
                                .implementors
                                .iter()
                                .any(|i| i.type_name == imp.type_name)
                            {
                                item.implementors.push(imp.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    /// Link types in signatures to their documentation pages.
    /// This scans all function/method signatures and field types to find
    /// type references that can be linked.
    pub fn link_signature_types(&mut self) {
        // Build a lookup map of linkable types by simple name
        // Maps simple name -> (path, kind_suffix)
        let linkable_types: std::collections::HashMap<String, (String, &'static str)> = self
            .items
            .iter()
            .filter(|item| {
                matches!(
                    item.kind,
                    DocItemKind::Struct
                        | DocItemKind::Enum
                        | DocItemKind::Contract
                        | DocItemKind::Trait
                        | DocItemKind::TypeAlias
                )
            })
            .map(|item| {
                let simple_name = extract_simple_type_name(&item.name);
                (simple_name, (item.path.clone(), item.kind.as_str()))
            })
            .collect();

        // Process all items with signatures
        for item in &mut self.items {
            // Link types in the item's main signature
            if !item.signature.is_empty() && item.rich_signature.is_empty() {
                item.rich_signature = build_rich_signature(&item.signature, &linkable_types);
            }

            // Link types in children (fields, methods, variants)
            for child in &mut item.children {
                if !child.signature.is_empty() && child.rich_signature.is_empty() {
                    child.rich_signature = build_rich_signature(&child.signature, &linkable_types);
                }
            }

            // Link types in trait impl method signatures
            for trait_impl in &mut item.trait_impls {
                for method in &mut trait_impl.methods {
                    if !method.signature.is_empty() && method.rich_signature.is_empty() {
                        method.rich_signature =
                            build_rich_signature(&method.signature, &linkable_types);
                    }
                }
            }
        }
    }
}

/// Build a rich signature from a plain signature string by linking type references.
/// Scans the signature for identifiers that match known types and creates links.
fn build_rich_signature(
    signature: &str,
    linkable_types: &std::collections::HashMap<String, (String, &'static str)>,
) -> RichSignature {
    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut chars = signature.chars().peekable();

    while let Some(c) = chars.next() {
        // Check if this could be the start of an identifier
        if c.is_ascii_alphabetic() || c == '_' {
            // Collect the full identifier
            let mut ident = String::new();
            ident.push(c);
            while let Some(&next) = chars.peek() {
                if next.is_ascii_alphanumeric() || next == '_' {
                    ident.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            // Check if this identifier is a linkable type
            // Only link identifiers that start with uppercase (type convention)
            if ident.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                && let Some((path, kind)) = linkable_types.get(&ident)
            {
                // Flush any pending text
                if !current_text.is_empty() {
                    parts.push(SignaturePart::text(&current_text));
                    current_text.clear();
                }
                // Add the linked type
                parts.push(SignaturePart::link(&ident, format!("{}/{}", path, kind)));
                continue;
            }

            // Not linkable, just add to current text
            current_text.push_str(&ident);
        } else {
            current_text.push(c);
        }
    }

    // Flush any remaining text
    if !current_text.is_empty() {
        parts.push(SignaturePart::text(&current_text));
    }

    parts
}

/// Extract the simple type name from a potentially qualified/generic path.
/// "mod::MyStruct<T>" -> "MyStruct"
/// "MyStruct" -> "MyStruct"
fn extract_simple_type_name(type_str: &str) -> String {
    // Remove generic params first
    let without_generics = type_str.split('<').next().unwrap_or(type_str);
    // Get last path segment
    without_generics
        .rsplit("::")
        .next()
        .unwrap_or(without_generics)
        .trim()
        .to_string()
}

/// Module tree for navigation sidebar
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocModuleTree {
    pub name: String,
    pub path: String,
    pub children: Vec<DocModuleTree>,
    /// Direct items in this module (non-module children)
    pub items: Vec<DocModuleItem>,
}

impl DocModuleTree {
    /// Get the URL path for this module (includes kind suffix)
    pub fn url_path(&self) -> String {
        format!("{}/mod", self.path)
    }
}

/// A reference to an item within a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocModuleItem {
    pub name: String,
    pub path: String,
    pub kind: DocItemKind,
    /// Brief summary (first sentence of docs)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
}

impl DocModuleItem {
    /// Get the URL path for this item (includes kind suffix)
    pub fn url_path(&self) -> String {
        format!("{}/{}", self.path, self.kind.as_str())
    }
}
