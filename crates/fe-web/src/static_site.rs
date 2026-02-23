//! Static documentation site generator
//!
//! Produces a single `index.html` that works with `file://` — no server needed.

use std::collections::HashMap;
use std::path::Path;

use crate::assets;
use crate::highlight::highlight_fe;
use crate::markdown::render_markdown;
use crate::model::{DocIndex, DocItemKind};

pub struct StaticSiteGenerator;

impl StaticSiteGenerator {
    /// Generate a static documentation site in `output_dir`.
    ///
    /// Produces a single `index.html` file with inlined CSS, JS, and JSON.
    /// Markdown doc bodies are pre-rendered to HTML and injected as `html_body`
    /// fields in the JSON (the Rust types are never modified).
    pub fn generate(index: &DocIndex, output_dir: &Path) -> std::io::Result<()> {
        Self::generate_with_scip(index, output_dir, None)
    }

    /// Generate a static documentation site with optional embedded SCIP data.
    ///
    /// When `scip_json` is provided, the pre-processed SCIP JSON is embedded
    /// inline so the browser can build a ScipStore for interactive symbol
    /// resolution (progressive enhancement over the pre-rendered DocIndex).
    pub fn generate_with_scip(
        index: &DocIndex,
        output_dir: &Path,
        scip_json: Option<&str>,
    ) -> std::io::Result<()> {
        std::fs::create_dir_all(output_dir)?;

        // Serialize to a JSON Value so we can inject html_body fields
        let mut value = serde_json::to_value(index)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        // Pre-render markdown bodies, syntax-highlight signatures, and link types
        let type_links = build_type_links(index);
        inject_html_bodies(&mut value);
        inject_highlighted_signatures(&mut value);
        inject_type_links(&mut value, &type_links);

        let json = serde_json::to_string(&value)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        let title = index_title(index);
        let html = assets::html_shell_with_scip(&title, &json, scip_json);

        std::fs::write(output_dir.join("index.html"), html)?;

        Ok(())
    }
}

/// Walk the JSON and inject `html_body` next to every `docs.body` field.
pub fn inject_html_bodies(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::Object(map) => {
            // If this object has a "docs" field with a "body", inject "html_body"
            if let Some(docs) = map.get_mut("docs") {
                if let Some(docs_obj) = docs.as_object_mut() {
                    if let Some(body) = docs_obj.get("body").and_then(|b| b.as_str()) {
                        let html = render_markdown(body);
                        docs_obj.insert(
                            "html_body".to_string(),
                            serde_json::Value::String(html),
                        );
                    }
                }
            }
            // Recurse into all values
            for v in map.values_mut() {
                inject_html_bodies(v);
            }
        }
        serde_json::Value::Array(arr) => {
            for v in arr {
                inject_html_bodies(v);
            }
        }
        _ => {}
    }
}

/// Walk the JSON and inject `highlighted_signature` next to every `signature`
/// field by running the tree-sitter Fe highlighter.
pub fn inject_highlighted_signatures(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::Object(map) => {
            // If this object has a "signature" string, highlight it
            if let Some(sig) = map.get("signature").and_then(|s| s.as_str()) {
                if !sig.is_empty() {
                    let html = highlight_fe(sig);
                    map.insert(
                        "highlighted_signature".to_string(),
                        serde_json::Value::String(html),
                    );
                }
            }
            for v in map.values_mut() {
                inject_highlighted_signatures(v);
            }
        }
        serde_json::Value::Array(arr) => {
            for v in arr {
                inject_highlighted_signatures(v);
            }
        }
        _ => {}
    }
}

/// Build a map of type names to their doc URL paths from the index.
///
/// Only includes linkable types (structs, enums, traits, contracts, type aliases).
pub fn build_type_links(index: &DocIndex) -> HashMap<String, String> {
    let mut links = HashMap::new();
    for item in &index.items {
        match item.kind {
            DocItemKind::Struct
            | DocItemKind::Enum
            | DocItemKind::Trait
            | DocItemKind::Contract
            | DocItemKind::TypeAlias => {
                links.insert(item.name.clone(), item.url_path());
            }
            _ => {}
        }
    }
    links
}

/// Walk the JSON and replace `<span class="hl-type">Name</span>` with
/// `<a href="#url" class="hl-type type-link">Name</a>` for known types.
///
/// Processes `highlighted_signature` and `html_body` fields.
pub fn inject_type_links(value: &mut serde_json::Value, type_links: &HashMap<String, String>) {
    match value {
        serde_json::Value::Object(map) => {
            for key in ["highlighted_signature", "html_body"] {
                if let Some(html) = map.get(key).and_then(|v| v.as_str()) {
                    let linked = link_types_in_html(html, type_links);
                    if linked != html {
                        map.insert(key.to_string(), serde_json::Value::String(linked));
                    }
                }
            }
            for v in map.values_mut() {
                inject_type_links(v, type_links);
            }
        }
        serde_json::Value::Array(arr) => {
            for v in arr {
                inject_type_links(v, type_links);
            }
        }
        _ => {}
    }
}

/// Replace type-highlighted spans with anchor links for known types.
fn link_types_in_html(html: &str, type_links: &HashMap<String, String>) -> String {
    // tree-sitter HtmlRenderer emits a double space: `<span  class=`
    const PREFIX: &str = "<span  class=\"hl-type\">";
    const SUFFIX: &str = "</span>";

    let mut result = String::with_capacity(html.len());
    let mut pos = 0;
    let bytes = html.as_bytes();

    while pos < html.len() {
        if let Some(start) = html[pos..].find(PREFIX) {
            let abs_start = pos + start;
            let text_start = abs_start + PREFIX.len();

            if let Some(end_offset) = html[text_start..].find(SUFFIX) {
                let text = &html[text_start..text_start + end_offset];
                let span_end = text_start + end_offset + SUFFIX.len();

                // Only link plain identifiers (no nested HTML)
                if !text.contains('<') {
                    if let Some(url) = type_links.get(text) {
                        result.push_str(&html[pos..abs_start]);
                        result.push_str("<a href=\"#");
                        result.push_str(url);
                        result.push_str("\" class=\"hl-type type-link\">");
                        result.push_str(text);
                        result.push_str("</a>");
                        pos = span_end;
                        continue;
                    }
                }

                // Not a known type — keep original span
                result.push_str(&html[pos..span_end]);
                pos = span_end;
            } else {
                // Unclosed span, copy rest
                result.push_str(&html[pos..]);
                return result;
            }
        } else {
            result.push_str(&html[pos..]);
            return result;
        }
    }

    let _ = bytes; // suppress unused warning
    result
}

/// Derive a title from the index (use the root module name if available).
fn index_title(index: &DocIndex) -> String {
    if let Some(root) = index.modules.first() {
        format!("{} — Fe Documentation", root.name)
    } else {
        "Fe Documentation".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::*;

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
            items: vec![DocModuleItem {
                name: "Greeter".into(),
                path: "mylib::Greeter".into(),
                kind: DocItemKind::Struct,
                summary: Some("A friendly greeter.".into()),
            }],
        }];
        index
    }

    #[test]
    fn generates_index_html() {
        let index = sample_index();
        let dir = std::env::temp_dir().join("fe_web_static_test");
        let _ = std::fs::remove_dir_all(&dir);

        StaticSiteGenerator::generate(&index, &dir).expect("generate failed");

        let html_path = dir.join("index.html");
        assert!(html_path.exists(), "index.html should exist");

        let html = std::fs::read_to_string(&html_path).unwrap();

        // Contains inlined CSS
        assert!(html.contains(":root"), "should contain CSS");
        // Contains inlined JS
        assert!(html.contains("renderDocItem"), "should contain JS");
        // Contains the JSON data
        assert!(html.contains("mylib::Greeter"), "should contain item path");
        // Contains pre-rendered markdown (html_body with <strong>)
        assert!(html.contains("html_body"), "should contain html_body key");
        // In the <script> tag, </ is escaped to <\/ for XSS safety
        assert!(html.contains(r"<strong>friendly<\/strong>"), "markdown should be pre-rendered");

        // Cleanup
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn inject_html_bodies_works() {
        let index = sample_index();
        let mut value = serde_json::to_value(&index).unwrap();
        inject_html_bodies(&mut value);

        let json = serde_json::to_string_pretty(&value).unwrap();
        assert!(json.contains("html_body"));
        assert!(json.contains("<strong>friendly</strong>"));
    }

    #[test]
    fn inject_highlighted_signatures_works() {
        let index = sample_index();
        let mut value = serde_json::to_value(&index).unwrap();
        inject_highlighted_signatures(&mut value);

        let json = serde_json::to_string_pretty(&value).unwrap();
        assert!(json.contains("highlighted_signature"), "should inject highlighted_signature");
        // "pub struct Greeter" should produce a keyword span for "struct"
        assert!(json.contains("hl-keyword"), "should have keyword highlight: {json}");
    }

    #[test]
    fn link_types_in_html_replaces_known_types() {
        let mut links = HashMap::new();
        links.insert("Greeter".to_string(), "mylib::Greeter/struct".to_string());

        // tree-sitter HtmlRenderer uses double space between <span and class
        let html = "<span  class=\"hl-keyword\">pub</span> <span  class=\"hl-keyword\">struct</span> <span  class=\"hl-type\">Greeter</span>";
        let result = link_types_in_html(html, &links);

        assert!(result.contains("type-link"), "should have type-link class: {result}");
        assert!(result.contains("<a href="), "should have anchor tag: {result}");
        assert!(result.contains("Greeter</a>"), "should wrap Greeter in link: {result}");
        assert!(result.contains("hl-keyword"), "should preserve non-type spans: {result}");
    }

    #[test]
    fn link_types_skips_unknown_types() {
        let links = HashMap::new();
        let html = "<span  class=\"hl-type\">Unknown</span>";
        let result = link_types_in_html(html, &links);
        assert_eq!(result, html, "should not modify unknown types");
    }

    #[test]
    fn inject_type_links_processes_highlighted_signature() {
        let mut index = sample_index();
        // Add a second item so the first can reference it
        index.add_item(DocItem {
            path: "mylib::Name".into(),
            name: "Name".into(),
            kind: DocItemKind::Struct,
            visibility: DocVisibility::Public,
            docs: None,
            signature: "pub struct Name".into(),
            rich_signature: vec![],
            generics: vec![],
            where_bounds: vec![],
            children: vec![],
            source: None,
            trait_impls: vec![],
            implementors: vec![],
        });

        let type_links = build_type_links(&index);
        let mut value = serde_json::to_value(&index).unwrap();
        inject_highlighted_signatures(&mut value);
        inject_type_links(&mut value, &type_links);

        let json = serde_json::to_string(&value).unwrap();
        // "Greeter" appears as hl-type in "pub struct Greeter" → should be linked
        assert!(json.contains("type-link"), "should have type links in highlighted signatures: {json}");
    }

    #[test]
    fn title_uses_root_module_name() {
        let index = sample_index();
        assert_eq!(index_title(&index), "mylib \u{2014} Fe Documentation");
    }

    #[test]
    fn title_fallback_when_no_modules() {
        let index = DocIndex::new();
        assert_eq!(index_title(&index), "Fe Documentation");
    }
}
