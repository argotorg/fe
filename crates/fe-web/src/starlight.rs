//! Starlight-compatible markdown page generator.
//!
//! Generates a directory of `.md` files from a [`DocIndex`] that Astro Starlight
//! can render as native documentation pages with sidebar, search, and badges.

use std::collections::HashMap;
use std::fmt::Write as _;
use std::io;
use std::path::Path;

use crate::escape::escape_html;
use crate::model::*;

/// Generate Starlight-compatible markdown pages from a [`DocIndex`].
///
/// Each module becomes a directory with an `index.md`, and each item becomes
/// an individual `.md` file with Starlight frontmatter (title, description,
/// sidebar badge, etc.).
pub fn generate(index: &DocIndex, output_dir: &Path, base_url: &str) -> io::Result<()> {
    std::fs::create_dir_all(output_dir)?;

    let collisions = detect_collisions(index);

    // Root index page
    write_root_index(index, output_dir, base_url)?;

    // Module pages
    for module in &index.modules {
        write_module_tree(module, index, output_dir, base_url, &collisions)?;
    }

    // Item pages (skip Module/Impl/ImplTrait — they're rendered elsewhere)
    for item in &index.items {
        if matches!(
            item.kind,
            DocItemKind::Module | DocItemKind::Impl | DocItemKind::ImplTrait
        ) {
            continue;
        }
        write_item_page(item, index, output_dir, base_url, &collisions)?;
    }

    // Write web component assets for Starlight to import
    write_component_assets(output_dir)?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Path helpers
// ---------------------------------------------------------------------------

/// Convert a doc path like `mylib::Greeter` to a filesystem path like `mylib/greeter.md`.
fn item_fs_path(
    path: &str,
    kind: DocItemKind,
    collisions: &HashMap<String, Vec<DocItemKind>>,
) -> std::path::PathBuf {
    let segments: Vec<&str> = path.split("::").collect();
    let mut p = std::path::PathBuf::new();

    if kind == DocItemKind::Module {
        for seg in &segments {
            p.push(slug(seg));
        }
        p.push("index.md");
        return p;
    }

    // Parent module segments → directories
    for seg in &segments[..segments.len().saturating_sub(1)] {
        p.push(slug(seg));
    }

    // Item filename, with kind suffix if there's a name collision
    let name = slug(segments.last().unwrap_or(&"unknown"));
    if collisions.contains_key(path) {
        p.push(format!("{}-{}.md", name, kind.as_str()));
    } else {
        p.push(format!("{name}.md"));
    }

    p
}

/// Convert a name to a URL-safe slug (lowercase).
fn slug(name: &str) -> String {
    name.to_lowercase()
}

/// Build an absolute URL for a doc item within the Starlight site.
fn item_url(
    path: &str,
    kind: DocItemKind,
    base_url: &str,
    collisions: &HashMap<String, Vec<DocItemKind>>,
) -> String {
    let fs = item_fs_path(path, kind, collisions);
    let route = fs
        .with_extension("")
        .to_string_lossy()
        .replace('\\', "/");
    format!("{base_url}/{route}/")
}

/// Detect doc paths that appear with multiple different kinds (rare, but possible).
fn detect_collisions(index: &DocIndex) -> HashMap<String, Vec<DocItemKind>> {
    let mut path_kinds: HashMap<String, Vec<DocItemKind>> = HashMap::new();
    for item in &index.items {
        path_kinds
            .entry(item.path.clone())
            .or_default()
            .push(item.kind);
    }
    path_kinds.retain(|_, kinds| kinds.len() > 1);
    path_kinds
}

// ---------------------------------------------------------------------------
// Starlight badge mapping
// ---------------------------------------------------------------------------

fn kind_badge(kind: DocItemKind) -> (&'static str, &'static str) {
    match kind {
        DocItemKind::Struct => ("struct", "note"),
        DocItemKind::Enum => ("enum", "caution"),
        DocItemKind::Trait => ("trait", "tip"),
        DocItemKind::Contract => ("contract", "danger"),
        DocItemKind::Function => ("fn", "note"),
        DocItemKind::TypeAlias => ("type", "note"),
        DocItemKind::Const => ("const", "note"),
        DocItemKind::Module => ("mod", "note"),
        DocItemKind::Impl | DocItemKind::ImplTrait => ("impl", "note"),
    }
}

// ---------------------------------------------------------------------------
// Page writers
// ---------------------------------------------------------------------------

/// Render a signature as an HTML web component.
///
/// If `rich_signature` is non-empty, emits `<fe-signature data='...'>` with the
/// rich signature serialized as JSON. Otherwise falls back to `<fe-code-block>`.
fn render_signature_html(signature: &str, rich_signature: &[SignaturePart]) -> String {
    if !rich_signature.is_empty() {
        let json = serde_json::to_string(rich_signature).unwrap_or_default();
        format!(
            "<fe-signature data='{}'>{}</fe-signature>",
            escape_html(&json),
            escape_html(signature),
        )
    } else {
        format!(
            "<fe-code-block lang=\"fe\">{}</fe-code-block>",
            escape_html(signature),
        )
    }
}

/// Write web component JS/CSS assets alongside the generated markdown.
fn write_component_assets(output_dir: &Path) -> io::Result<()> {
    use crate::assets::{FE_CODE_BLOCK_JS, FE_HIGHLIGHT_CSS, FE_SIGNATURE_JS};

    let dir = output_dir.join("_components");
    std::fs::create_dir_all(&dir)?;
    std::fs::write(dir.join("fe-code-block.js"), FE_CODE_BLOCK_JS)?;
    std::fs::write(dir.join("fe-signature.js"), FE_SIGNATURE_JS)?;
    std::fs::write(dir.join("fe-highlight.css"), FE_HIGHLIGHT_CSS)?;
    Ok(())
}

fn write_root_index(index: &DocIndex, output_dir: &Path, base_url: &str) -> io::Result<()> {
    let mut md = String::new();
    writeln!(md, "---").unwrap();
    writeln!(md, "title: API Reference").unwrap();
    writeln!(md, "description: Fe API documentation").unwrap();
    writeln!(md, "---").unwrap();
    writeln!(md).unwrap();

    if !index.modules.is_empty() {
        writeln!(md, "## Modules\n").unwrap();
        writeln!(md, "| Module | Description |").unwrap();
        writeln!(md, "|--------|-------------|").unwrap();
        for module in &index.modules {
            let url = format!("{base_url}/{}/", slug(&module.name));
            let summary = module
                .items
                .first()
                .and_then(|i| i.summary.as_deref())
                .unwrap_or("");
            writeln!(md, "| [{}]({url}) | {summary} |", module.name).unwrap();
        }
        writeln!(md).unwrap();
    }

    std::fs::write(output_dir.join("index.md"), md)
}

fn write_module_tree(
    module: &DocModuleTree,
    index: &DocIndex,
    output_dir: &Path,
    base_url: &str,
    collisions: &HashMap<String, Vec<DocItemKind>>,
) -> io::Result<()> {
    let dir = output_dir.join(slug(&module.name));
    std::fs::create_dir_all(&dir)?;

    let mut md = String::new();
    writeln!(md, "---").unwrap();
    writeln!(md, "title: \"{}\"", module.name).unwrap();
    writeln!(md, "description: \"Module {}\"", module.path).unwrap();
    writeln!(md, "sidebar:").unwrap();
    writeln!(md, "  label: {}", module.name).unwrap();
    writeln!(md, "---").unwrap();
    writeln!(md).unwrap();

    // Module docs (if the module has a DocItem)
    if let Some(item) = index.items.iter().find(|i| {
        i.path == module.path && i.kind == DocItemKind::Module
    }) {
        if let Some(docs) = &item.docs {
            writeln!(md, "{}\n", docs.body).unwrap();
        }
    }

    // Group items by kind, sorted by display order
    let mut by_kind: HashMap<DocItemKind, Vec<&DocModuleItem>> = HashMap::new();
    for item in &module.items {
        by_kind.entry(item.kind).or_default().push(item);
    }
    let mut kinds: Vec<_> = by_kind.keys().copied().collect();
    kinds.sort_by_key(|k| k.display_order());

    for kind in kinds {
        let items = &by_kind[&kind];
        writeln!(md, "## {}\n", kind.plural_name()).unwrap();
        writeln!(md, "| Name | Description |").unwrap();
        writeln!(md, "|------|-------------|").unwrap();
        for item in items {
            let url = item_url(&item.path, item.kind, base_url, collisions);
            let summary = item.summary.as_deref().unwrap_or("");
            writeln!(md, "| [{}]({url}) | {summary} |", item.name).unwrap();
        }
        writeln!(md).unwrap();
    }

    std::fs::write(dir.join("index.md"), md)?;

    // Recurse into child modules
    for child in &module.children {
        write_module_tree(child, index, &dir, base_url, collisions)?;
    }

    Ok(())
}

fn write_item_page(
    item: &DocItem,
    _index: &DocIndex,
    output_dir: &Path,
    base_url: &str,
    collisions: &HashMap<String, Vec<DocItemKind>>,
) -> io::Result<()> {
    let fs_path = item_fs_path(&item.path, item.kind, collisions);
    let full_path = output_dir.join(&fs_path);

    if let Some(parent) = full_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let (badge_text, badge_variant) = kind_badge(item.kind);

    let mut md = String::new();

    // Frontmatter
    writeln!(md, "---").unwrap();
    writeln!(md, "title: \"{}\"", item.name).unwrap();
    writeln!(
        md,
        "description: \"{} {} in {}\"",
        item.kind.display_name(),
        item.name,
        parent_module(&item.path)
    )
    .unwrap();
    writeln!(md, "sidebar:").unwrap();
    writeln!(md, "  label: {}", item.name).unwrap();
    writeln!(md, "  badge:").unwrap();
    writeln!(md, "    text: {badge_text}").unwrap();
    writeln!(md, "    variant: {badge_variant}").unwrap();
    writeln!(md, "---").unwrap();
    writeln!(md).unwrap();

    // Signature
    if !item.signature.is_empty() {
        writeln!(md, "{}\n", render_signature_html(&item.signature, &item.rich_signature)).unwrap();
    }

    // Documentation body
    if let Some(docs) = &item.docs {
        writeln!(md, "{}", docs.body).unwrap();
        writeln!(md).unwrap();
    }

    // Children grouped by kind
    render_children(&mut md, &item.children);

    // Trait implementations
    render_trait_impls(&mut md, &item.trait_impls, base_url, collisions);

    // Implementors (for trait pages)
    render_implementors(&mut md, &item.implementors, base_url, collisions);

    std::fs::write(full_path, md)
}

// ---------------------------------------------------------------------------
// Section renderers
// ---------------------------------------------------------------------------

fn render_children(md: &mut String, children: &[DocChild]) {
    if children.is_empty() {
        return;
    }

    let mut by_kind: HashMap<DocChildKind, Vec<&DocChild>> = HashMap::new();
    for child in children {
        by_kind.entry(child.kind).or_default().push(child);
    }
    let mut kinds: Vec<_> = by_kind.keys().copied().collect();
    kinds.sort_by_key(|k| k.display_order());

    for kind in kinds {
        let items = &by_kind[&kind];
        writeln!(md, "## {}\n", kind.plural_name()).unwrap();

        for child in items {
            writeln!(md, "### `{}`\n", child.name).unwrap();
            if !child.signature.is_empty() {
                writeln!(md, "{}\n", render_signature_html(&child.signature, &child.rich_signature)).unwrap();
            }
            if let Some(docs) = &child.docs {
                writeln!(md, "{docs}\n").unwrap();
            }
        }
    }
}

fn render_trait_impls(
    md: &mut String,
    trait_impls: &[DocTraitImpl],
    _base_url: &str,
    _collisions: &HashMap<String, Vec<DocItemKind>>,
) {
    if trait_impls.is_empty() {
        return;
    }

    writeln!(md, "## Trait Implementations\n").unwrap();

    for ti in trait_impls {
        let heading = if ti.trait_name.is_empty() {
            "Methods".to_string()
        } else {
            format!("impl {}", ti.trait_name)
        };
        writeln!(md, "### {heading}\n").unwrap();

        if !ti.signature.is_empty() {
            writeln!(md, "{}\n", render_signature_html(&ti.signature, &ti.rich_signature)).unwrap();
        }

        for method in &ti.methods {
            writeln!(md, "#### `{}`\n", method.name).unwrap();
            if !method.signature.is_empty() {
                writeln!(md, "{}\n", render_signature_html(&method.signature, &method.rich_signature)).unwrap();
            }
            if let Some(docs) = &method.docs {
                writeln!(md, "{docs}\n").unwrap();
            }
        }
    }
}

fn render_implementors(
    md: &mut String,
    implementors: &[DocImplementor],
    base_url: &str,
    collisions: &HashMap<String, Vec<DocItemKind>>,
) {
    if implementors.is_empty() {
        return;
    }

    writeln!(md, "## Implementors\n").unwrap();
    writeln!(md, "| Type | Signature |").unwrap();
    writeln!(md, "|------|-----------|").unwrap();
    for imp in implementors {
        let url = item_url(&imp.type_url, DocItemKind::Struct, base_url, collisions);
        let sig_cell = if !imp.rich_signature.is_empty() {
            let json = serde_json::to_string(&imp.rich_signature).unwrap_or_default();
            format!(
                "<fe-signature data='{}'>{}</fe-signature>",
                escape_html(&json),
                escape_html(&imp.signature),
            )
        } else {
            format!("<code>{}</code>", escape_html(&imp.signature))
        };
        writeln!(md, "| [{}]({url}) | {sig_cell} |", imp.type_name).unwrap();
    }
    writeln!(md).unwrap();
}

/// Extract the parent module path from a fully qualified path.
fn parent_module(path: &str) -> &str {
    path.rsplit_once("::").map_or(path, |(parent, _)| parent)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_index() -> DocIndex {
        let mut index = DocIndex::new();
        index.add_item(DocItem {
            path: "mylib::Greeter".into(),
            name: "Greeter".into(),
            kind: DocItemKind::Struct,
            visibility: DocVisibility::Public,
            docs: Some(DocContent::from_raw("A **friendly** greeter.")),
            signature: "pub struct Greeter".into(),
            rich_signature: vec![],
            signature_span: None,
            sig_scope: None,
            generics: vec![],
            where_bounds: vec![],
            children: vec![
                DocChild {
                    kind: DocChildKind::Field,
                    name: "name".into(),
                    docs: Some("The greeter's name.".into()),
                    signature: "name: String".into(),
                    rich_signature: vec![],
                    signature_span: None,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                },
                DocChild {
                    kind: DocChildKind::Method,
                    name: "greet".into(),
                    docs: Some("Say hello.".into()),
                    signature: "pub fn greet(self)".into(),
                    rich_signature: vec![],
                    signature_span: None,
                    sig_scope: None,
                    visibility: DocVisibility::Public,
                },
            ],
            source: None,
            trait_impls: vec![],
            implementors: vec![],
        });
        index.add_item(DocItem {
            path: "mylib::hello".into(),
            name: "hello".into(),
            kind: DocItemKind::Function,
            visibility: DocVisibility::Public,
            docs: Some(DocContent::from_raw("A hello function.")),
            signature: "pub fn hello()".into(),
            rich_signature: vec![],
            signature_span: None,
            sig_scope: None,
            generics: vec![],
            where_bounds: vec![],
            children: vec![],
            source: None,
            trait_impls: vec![],
            implementors: vec![],
        });
        index.modules = vec![DocModuleTree {
            name: "mylib".into(),
            path: "mylib".into(),
            children: vec![],
            items: vec![
                DocModuleItem {
                    name: "Greeter".into(),
                    path: "mylib::Greeter".into(),
                    kind: DocItemKind::Struct,
                    summary: Some("A friendly greeter.".into()),
                },
                DocModuleItem {
                    name: "hello".into(),
                    path: "mylib::hello".into(),
                    kind: DocItemKind::Function,
                    summary: Some("A hello function.".into()),
                },
            ],
        }];
        index
    }

    #[test]
    fn generates_directory_structure() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_starlight_test");
        let _ = std::fs::remove_dir_all(&dir);

        generate(&index, &dir, "/api").unwrap();

        assert!(dir.join("index.md").exists(), "root index");
        assert!(dir.join("mylib/index.md").exists(), "module index");
        assert!(dir.join("mylib/greeter.md").exists(), "struct page");
        assert!(dir.join("mylib/hello.md").exists(), "function page");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn root_index_has_frontmatter() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_starlight_root");
        let _ = std::fs::remove_dir_all(&dir);

        generate(&index, &dir, "/api").unwrap();

        let content = std::fs::read_to_string(dir.join("index.md")).unwrap();
        assert!(content.starts_with("---\n"));
        assert!(content.contains("title: API Reference"));
        assert!(content.contains("[mylib]"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn item_page_has_badge_and_signature() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_starlight_item");
        let _ = std::fs::remove_dir_all(&dir);

        generate(&index, &dir, "/api").unwrap();

        let content = std::fs::read_to_string(dir.join("mylib/greeter.md")).unwrap();
        assert!(content.contains("text: struct"), "should have struct badge");
        assert!(
            content.contains("<fe-code-block") || content.contains("<fe-signature"),
            "should use web component for signature, got:\n{content}"
        );
        assert!(content.contains("pub struct Greeter"), "should have signature text");
        assert!(!content.contains("```fe"), "should not have fenced code blocks");
        assert!(content.contains("## Fields"), "should group fields");
        assert!(content.contains("### `name`"), "should have field heading");
        assert!(content.contains("## Methods"), "should group methods");
        assert!(content.contains("### `greet`"), "should have method heading");
        assert!(content.contains("**friendly**"), "should have docs body");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn module_page_groups_by_kind() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_starlight_mod");
        let _ = std::fs::remove_dir_all(&dir);

        generate(&index, &dir, "/api").unwrap();

        let content = std::fs::read_to_string(dir.join("mylib/index.md")).unwrap();
        assert!(content.contains("## Structs"), "should have Structs section");
        assert!(content.contains("## Functions"), "should have Functions section");
        assert!(content.contains("[Greeter]"), "should link to Greeter");
        assert!(content.contains("[hello]"), "should link to hello");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn writes_component_assets() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_starlight_assets");
        let _ = std::fs::remove_dir_all(&dir);

        generate(&index, &dir, "/api").unwrap();

        let comp = dir.join("_components");
        assert!(comp.join("fe-code-block.js").exists(), "fe-code-block.js");
        assert!(comp.join("fe-signature.js").exists(), "fe-signature.js");
        assert!(comp.join("fe-highlight.css").exists(), "fe-highlight.css");

        // Verify content is non-empty
        let cb = std::fs::read_to_string(comp.join("fe-code-block.js")).unwrap();
        assert!(cb.contains("fe-code-block"), "should contain element name");
        let sig = std::fs::read_to_string(comp.join("fe-signature.js")).unwrap();
        assert!(sig.contains("fe-signature"), "should contain element name");
        let css = std::fs::read_to_string(comp.join("fe-highlight.css")).unwrap();
        assert!(!css.is_empty(), "CSS should be non-empty");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn item_fs_path_basic() {
        let no_collisions = HashMap::new();
        assert_eq!(
            item_fs_path("mylib::Foo", DocItemKind::Struct, &no_collisions),
            std::path::PathBuf::from("mylib/foo.md")
        );
        assert_eq!(
            item_fs_path("mylib", DocItemKind::Module, &no_collisions),
            std::path::PathBuf::from("mylib/index.md")
        );
    }

    #[test]
    fn item_fs_path_collision() {
        let mut collisions = HashMap::new();
        collisions.insert(
            "mylib::Foo".to_string(),
            vec![DocItemKind::Struct, DocItemKind::Function],
        );
        assert_eq!(
            item_fs_path("mylib::Foo", DocItemKind::Struct, &collisions),
            std::path::PathBuf::from("mylib/foo-struct.md")
        );
        assert_eq!(
            item_fs_path("mylib::Foo", DocItemKind::Function, &collisions),
            std::path::PathBuf::from("mylib/foo-fn.md")
        );
    }
}
