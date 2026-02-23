//! Static documentation site generator
//!
//! Produces a single `index.html` that works with `file://` — no server needed.

use std::path::Path;

use crate::assets;
use crate::markdown::render_markdown;
use crate::model::DocIndex;

pub struct StaticSiteGenerator;

impl StaticSiteGenerator {
    /// Generate a static documentation site in `output_dir`.
    ///
    /// Produces a single `index.html` file with inlined CSS, JS, and JSON.
    /// Markdown doc bodies are pre-rendered to HTML and injected as `html_body`
    /// fields in the JSON (the Rust types are never modified).
    pub fn generate(index: &DocIndex, output_dir: &Path) -> std::io::Result<()> {
        std::fs::create_dir_all(output_dir)?;

        // Serialize to a JSON Value so we can inject html_body fields
        let mut value = serde_json::to_value(index)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        // Pre-render markdown bodies
        inject_html_bodies(&mut value);

        let json = serde_json::to_string(&value)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        let title = index_title(index);
        let html = assets::html_shell(&title, &json);

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
