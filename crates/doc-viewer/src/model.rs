//! Documentation data model
//!
//! These types represent extracted documentation in a format suitable for rendering.
//! They are designed to be serializable for static site generation and cacheable
//! for dynamic serving.

use serde::{Deserialize, Serialize};

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
    /// The item's signature/definition (rendered as code)
    pub signature: String,
    /// Generic parameters, if any
    pub generics: Vec<DocGenericParam>,
    /// Where clause bounds, if any
    pub where_bounds: Vec<String>,
    /// Child items (methods, fields, variants, etc.)
    pub children: Vec<DocChild>,
    /// Source location for "view source" links
    pub source: Option<DocSourceLoc>,
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
    pub fn from_str(s: &str) -> Option<Self> {
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
            if line.starts_with("# ") {
                // Save previous section if any
                if let Some((name, content)) = current_section.take() {
                    sections.push(DocSection {
                        name,
                        content: content.trim().to_string(),
                    });
                }
                // Start new section
                let name = line[2..].trim().to_string();
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
        if let Some((path, kind_str)) = url_path.rsplit_once('/') {
            if let Some(kind) = DocItemKind::from_str(kind_str) {
                // URL has valid kind suffix - find by path and kind
                return self.find_by_path_and_kind(path, kind);
            }
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
}

impl DocModuleItem {
    /// Get the URL path for this item (includes kind suffix)
    pub fn url_path(&self) -> String {
        format!("{}/{}", self.path, self.kind.as_str())
    }
}
