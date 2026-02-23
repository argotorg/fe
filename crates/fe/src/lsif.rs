use std::collections::HashMap;
use std::io::{self, Write};

use common::InputDb;
use common::diagnostics::Span;
use hir::{
    hir_def::{HirIngot, ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};

use crate::index_util::{self, LineIndex};

/// Position in LSIF (0-based line and character).
#[derive(Clone, Copy)]
struct LsifPos {
    line: u32,
    character: u32,
}

/// Range in LSIF.
#[derive(Clone, Copy)]
struct LsifRange {
    start: LsifPos,
    end: LsifPos,
}

fn utf16_column(text: &str, line_start: usize, offset: usize) -> Option<u32> {
    if line_start > offset || offset > text.len() {
        return None;
    }
    let prefix = text.get(line_start..offset)?;
    Some(prefix.encode_utf16().count() as u32)
}

/// Convert a Span to an LsifRange.
fn span_to_range(span: &Span, db: &dyn InputDb) -> Option<LsifRange> {
    let text = span.file.text(db);
    let line_index = LineIndex::new(text);

    let start = line_index.position(span.range.start().into());
    let end = line_index.position(span.range.end().into());

    let start_character = utf16_column(text, start.line_start_offset, start.byte_offset)?;
    let end_character = utf16_column(text, end.line_start_offset, end.byte_offset)?;

    Some(LsifRange {
        start: LsifPos {
            line: start.line as u32,
            character: start_character,
        },
        end: LsifPos {
            line: end.line as u32,
            character: end_character,
        },
    })
}

/// The LSIF emitter.
struct LsifEmitter<W: Write> {
    writer: W,
    next_id: u64,
}

impl<W: Write> LsifEmitter<W> {
    fn new(writer: W) -> Self {
        Self { writer, next_id: 1 }
    }

    fn next_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn emit_vertex(&mut self, label: &str, data: serde_json::Value) -> io::Result<u64> {
        let id = self.next_id();
        let mut obj = serde_json::json!({
            "id": id,
            "type": "vertex",
            "label": label,
        });
        if let serde_json::Value::Object(map) = data {
            for (k, v) in map {
                obj[&k] = v;
            }
        }
        writeln!(self.writer, "{}", obj)?;
        Ok(id)
    }

    fn emit_edge(&mut self, label: &str, out_v: u64, in_v: u64) -> io::Result<u64> {
        let id = self.next_id();
        let obj = serde_json::json!({
            "id": id,
            "type": "edge",
            "label": label,
            "outV": out_v,
            "inV": in_v,
        });
        writeln!(self.writer, "{}", obj)?;
        Ok(id)
    }

    fn emit_edge_many(
        &mut self,
        label: &str,
        out_v: u64,
        in_vs: &[u64],
        document: Option<u64>,
    ) -> io::Result<u64> {
        let id = self.next_id();
        let mut obj = serde_json::json!({
            "id": id,
            "type": "edge",
            "label": label,
            "outV": out_v,
            "inVs": in_vs,
        });
        if let Some(doc) = document {
            obj["document"] = serde_json::json!(doc);
        }
        writeln!(self.writer, "{}", obj)?;
        Ok(id)
    }

    fn emit_metadata(&mut self) -> io::Result<u64> {
        self.emit_vertex(
            "metaData",
            serde_json::json!({
                "version": "0.4.0",
                "positionEncoding": "utf-16",
                "toolInfo": {
                    "name": "fe-lsif",
                    "version": env!("CARGO_PKG_VERSION"),
                }
            }),
        )
    }

    fn emit_project(&mut self) -> io::Result<u64> {
        self.emit_vertex("project", serde_json::json!({"kind": "fe"}))
    }

    fn emit_document(&mut self, uri: &str) -> io::Result<u64> {
        self.emit_vertex(
            "document",
            serde_json::json!({
                "uri": uri,
                "languageId": "fe",
            }),
        )
    }

    fn emit_range(&mut self, range: LsifRange) -> io::Result<u64> {
        self.emit_vertex(
            "range",
            serde_json::json!({
                "start": {"line": range.start.line, "character": range.start.character},
                "end": {"line": range.end.line, "character": range.end.character},
            }),
        )
    }

    fn emit_result_set(&mut self) -> io::Result<u64> {
        self.emit_vertex("resultSet", serde_json::json!({}))
    }

    fn emit_definition_result(&mut self) -> io::Result<u64> {
        self.emit_vertex("definitionResult", serde_json::json!({}))
    }

    fn emit_reference_result(&mut self) -> io::Result<u64> {
        self.emit_vertex("referenceResult", serde_json::json!({}))
    }

    fn emit_hover_result(&mut self, contents: &str) -> io::Result<u64> {
        self.emit_vertex(
            "hoverResult",
            serde_json::json!({
                "result": {
                    "contents": [
                        {"language": "fe", "value": contents}
                    ]
                }
            }),
        )
    }

    fn emit_moniker(&mut self, scheme: &str, identifier: &str) -> io::Result<u64> {
        self.emit_vertex(
            "moniker",
            serde_json::json!({
                "scheme": scheme,
                "identifier": identifier,
                "kind": "export",
            }),
        )
    }
}

/// Build hover content for an item (definition + docstring) using SymbolView.
fn build_hover_content(db: &driver::DriverDataBase, item: ItemKind) -> Option<String> {
    let docs = index_util::item_docs(db, item);
    let signature = docs.signature?;

    if let Some(doc) = docs.docstring {
        Some(format!("{signature}\n\n{doc}"))
    } else {
        Some(signature)
    }
}

/// Run LSIF generation on a project.
pub fn generate_lsif(
    db: &mut driver::DriverDataBase,
    ingot_url: &url::Url,
    writer: impl Write,
) -> io::Result<()> {
    let mut emitter = LsifEmitter::new(writer);

    // Metadata and project
    emitter.emit_metadata()?;
    let project_id = emitter.emit_project()?;

    let ctx = index_util::IngotContext::resolve(db, ingot_url)?;

    // Track documents: url -> (vertex_id, range_ids)
    let mut documents: HashMap<String, (u64, Vec<u64>)> = HashMap::new();

    // Pre-emit document vertices for each module
    for top_mod in ctx.ingot.all_modules(db) {
        let doc_span = top_mod.span().resolve(db);
        let doc_url = match &doc_span {
            Some(span) => match span.file.url(db) {
                Some(url) => url.to_string(),
                None => continue,
            },
            None => continue,
        };
        if let std::collections::hash_map::Entry::Vacant(entry) = documents.entry(doc_url) {
            let doc_id = emitter.emit_document(entry.key())?;
            entry.insert((doc_id, Vec::new()));
        }
    }

    // Process each module's items
    for top_mod in ctx.ingot.all_modules(db) {
        let scope_graph = top_mod.scope_graph(db);

        let doc_span = top_mod.span().resolve(db);
        let doc_url = match &doc_span {
            Some(span) => match span.file.url(db) {
                Some(url) => url.to_string(),
                None => continue,
            },
            None => continue,
        };

        let doc_id = documents[&doc_url].0;

        for item in scope_graph.items_dfs(db) {
            let scope = ScopeId::from_item(item);

            let name_span = match item.name_span() {
                Some(ns) => ns,
                None => continue,
            };
            let resolved_name_span = match name_span.resolve(db) {
                Some(s) => s,
                None => continue,
            };
            let name_range = match span_to_range(&resolved_name_span, db) {
                Some(r) => r,
                None => continue,
            };

            // Emit range + resultSet
            let range_id = emitter.emit_range(name_range)?;
            documents.get_mut(&doc_url).unwrap().1.push(range_id);

            let result_set_id = emitter.emit_result_set()?;
            emitter.emit_edge("next", range_id, result_set_id)?;

            // Definition result
            let def_result_id = emitter.emit_definition_result()?;
            emitter.emit_edge("textDocument/definition", result_set_id, def_result_id)?;
            emitter.emit_edge_many("item", def_result_id, &[range_id], Some(doc_id))?;

            // Hover result
            if let Some(hover_content) = build_hover_content(db, item) {
                let hover_id = emitter.emit_hover_result(&hover_content)?;
                emitter.emit_edge("textDocument/hover", result_set_id, hover_id)?;
            }

            // Reference result using ReferenceIndex
            let ref_result_id = emitter.emit_reference_result()?;
            emitter.emit_edge("textDocument/references", result_set_id, ref_result_id)?;

            // Definition is also a reference
            emitter.emit_edge_many("item", ref_result_id, &[range_id], Some(doc_id))?;

            // Collect references from the pre-built index
            // Group by document URL so we can emit per-document item edges
            let mut refs_by_doc: HashMap<String, Vec<u64>> = HashMap::new();
            for indexed_ref in ctx.ref_index.references_to(&scope) {
                if let Some(resolved) = indexed_ref.span.resolve(db)
                    && let Some(r) = span_to_range(&resolved, db)
                {
                    let ref_doc_url = match resolved.file.url(db) {
                        Some(url) => url.to_string(),
                        None => continue,
                    };
                    let ref_range_id = emitter.emit_range(r)?;
                    refs_by_doc
                        .entry(ref_doc_url)
                        .or_default()
                        .push(ref_range_id);
                }
            }

            for (ref_doc_url, ref_range_ids) in &refs_by_doc {
                if !ref_range_ids.is_empty() {
                    let ref_doc_id = documents[ref_doc_url].0;
                    documents
                        .get_mut(ref_doc_url)
                        .unwrap()
                        .1
                        .extend_from_slice(ref_range_ids);
                    emitter.emit_edge_many(
                        "item",
                        ref_result_id,
                        ref_range_ids,
                        Some(ref_doc_id),
                    )?;
                }
            }

            // Moniker
            if let Some(pretty_path) = scope.pretty_path(db) {
                let identifier = format!("{}:{}:{pretty_path}", ctx.name, ctx.version);
                let moniker_id = emitter.emit_moniker("fe", &identifier)?;
                emitter.emit_edge("moniker/attach", result_set_id, moniker_id)?;
            }
        }
    }

    // Emit contains edges for all documents
    let document_ids: Vec<u64> = documents.values().map(|(id, _)| *id).collect();
    for (doc_id, range_ids) in documents.values() {
        if !range_ids.is_empty() {
            emitter.emit_edge_many("contains", *doc_id, range_ids, None)?;
        }
    }

    // Project -> document edges
    if !document_ids.is_empty() {
        emitter.emit_edge_many("contains", project_id, &document_ids, None)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use std::collections::{HashMap as StdHashMap, HashSet};

    /// Parse LSIF output into a list of JSON values, one per line.
    fn parse_lsif(output: &str) -> Vec<serde_json::Value> {
        output
            .lines()
            .filter(|l| !l.is_empty())
            .map(|l| serde_json::from_str(l).expect("valid JSON line"))
            .collect()
    }

    /// Validate structural correctness of LSIF output.
    fn validate_lsif(elements: &[serde_json::Value]) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        let mut seen_ids: HashSet<u64> = HashSet::new();
        let mut vertex_labels: StdHashMap<u64, String> = StdHashMap::new();

        // Check first element is metaData
        if let Some(first) = elements.first() {
            if first.get("label").and_then(|l| l.as_str()) != Some("metaData") {
                errors.push("first element must be metaData vertex".into());
            }
        } else {
            errors.push("empty LSIF output".into());
            return Err(errors);
        }

        // Validate each element
        for (i, el) in elements.iter().enumerate() {
            let id = el.get("id").and_then(|v| v.as_u64());
            let el_type = el.get("type").and_then(|v| v.as_str());
            let label = el.get("label").and_then(|v| v.as_str());

            // Every element needs id, type, label
            if id.is_none() {
                errors.push(format!("element {i} missing 'id'"));
            }
            if el_type.is_none() {
                errors.push(format!("element {i} missing 'type'"));
            }
            if label.is_none() {
                errors.push(format!("element {i} missing 'label'"));
            }

            if let Some(id) = id {
                if !seen_ids.insert(id) {
                    errors.push(format!("duplicate id {id}"));
                }
                if let (Some(t), Some(l)) = (el_type, label)
                    && t == "vertex"
                {
                    vertex_labels.insert(id, l.to_string());
                }
            }
        }

        // Validate edges reference existing vertices
        for el in elements {
            if el.get("type").and_then(|v| v.as_str()) != Some("edge") {
                continue;
            }
            if let Some(out_v) = el.get("outV").and_then(|v| v.as_u64()) {
                if !seen_ids.contains(&out_v) {
                    errors.push(format!("edge references non-existent outV {out_v}"));
                }
            } else {
                let id = el.get("id").and_then(|v| v.as_u64()).unwrap_or(0);
                errors.push(format!("edge {id} missing 'outV'"));
            }

            // Check inV or inVs
            let has_in_v = el.get("inV").and_then(|v| v.as_u64()).is_some();
            let has_in_vs = el.get("inVs").and_then(|v| v.as_array()).is_some();
            if !has_in_v && !has_in_vs {
                let id = el.get("id").and_then(|v| v.as_u64()).unwrap_or(0);
                errors.push(format!("edge {id} missing both 'inV' and 'inVs'"));
            }

            if let Some(in_v) = el.get("inV").and_then(|v| v.as_u64())
                && !seen_ids.contains(&in_v)
            {
                errors.push(format!("edge references non-existent inV {in_v}"));
            }
            if let Some(in_vs) = el.get("inVs").and_then(|v| v.as_array()) {
                for v in in_vs {
                    if let Some(id) = v.as_u64()
                        && !seen_ids.contains(&id)
                    {
                        errors.push(format!("edge references non-existent inVs member {id}"));
                    }
                }
            }
        }

        // Check required vertex types exist
        let labels: HashSet<&str> = vertex_labels.values().map(|s| s.as_str()).collect();
        for required in ["metaData", "project"] {
            if !labels.contains(required) {
                errors.push(format!("missing required vertex type: {required}"));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn generate_test_lsif(code: &str) -> String {
        let mut db = DriverDataBase::default();
        let url = url::Url::parse("file:///test.fe").unwrap();
        db.workspace()
            .touch(&mut db, url.clone(), Some(code.to_string()));

        let ingot_url = url::Url::parse("file:///").unwrap();
        let mut output = Vec::new();
        let _ = generate_lsif(&mut db, &ingot_url, &mut output);
        String::from_utf8(output).unwrap()
    }

    #[test]
    fn test_lsif_basic_structure() {
        let output = generate_test_lsif("struct Foo {\n    x: i32\n}\n");
        let elements = parse_lsif(&output);

        assert!(!elements.is_empty(), "should produce LSIF output");
        match validate_lsif(&elements) {
            Ok(()) => {}
            Err(errors) => panic!("LSIF validation errors:\n{}", errors.join("\n")),
        }
    }

    #[test]
    fn test_lsif_has_document_vertex() {
        let output = generate_test_lsif("fn hello() -> i32 {\n    42\n}\n");
        let elements = parse_lsif(&output);

        let docs: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("document"))
            .collect();
        assert!(!docs.is_empty(), "should have at least one document vertex");

        let doc = &docs[0];
        assert_eq!(
            doc.get("languageId").and_then(|l| l.as_str()),
            Some("fe"),
            "document languageId should be 'fe'"
        );
    }

    #[test]
    fn test_lsif_definitions_and_references() {
        let code = r#"struct Point {
    x: i32
    y: i32
}

fn make_point() -> Point {
    Point { x: 1, y: 2 }
}
"#;
        let output = generate_test_lsif(code);
        let elements = parse_lsif(&output);

        match validate_lsif(&elements) {
            Ok(()) => {}
            Err(errors) => panic!("validation errors:\n{}", errors.join("\n")),
        }

        // Should have definitionResult vertices
        let def_results: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("definitionResult"))
            .collect();
        assert!(
            def_results.len() >= 2,
            "should have definition results for Point and make_point, got {}",
            def_results.len()
        );

        // Should have referenceResult vertices
        let ref_results: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("referenceResult"))
            .collect();
        assert!(
            ref_results.len() >= 2,
            "should have reference results for both items"
        );

        // Should have hover results
        let hovers: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("hoverResult"))
            .collect();
        assert!(!hovers.is_empty(), "should have hover results");
    }

    #[test]
    fn test_lsif_monikers() {
        let output = generate_test_lsif("fn greet() -> i32 {\n    1\n}\n");
        let elements = parse_lsif(&output);

        let monikers: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("moniker"))
            .collect();
        assert!(!monikers.is_empty(), "should have moniker vertices");

        let moniker = &monikers[0];
        assert_eq!(
            moniker.get("scheme").and_then(|s| s.as_str()),
            Some("fe"),
            "moniker scheme should be 'fe'"
        );
        assert!(
            moniker.get("identifier").and_then(|s| s.as_str()).is_some(),
            "moniker should have identifier"
        );
    }

    #[test]
    fn test_lsif_contains_edges() {
        let output = generate_test_lsif("fn foo() -> i32 {\n    1\n}\n");
        let elements = parse_lsif(&output);

        let contains_edges: Vec<_> = elements
            .iter()
            .filter(|e| {
                e.get("type").and_then(|t| t.as_str()) == Some("edge")
                    && e.get("label").and_then(|l| l.as_str()) == Some("contains")
            })
            .collect();
        assert!(
            contains_edges.len() >= 2,
            "should have contains edges for document->ranges and project->documents, got {}",
            contains_edges.len()
        );
    }

    #[test]
    fn test_lsif_no_duplicate_document_ids() {
        let output = generate_test_lsif("struct A {\n    x: i32\n}\nstruct B {\n    y: i32\n}\n");
        let elements = parse_lsif(&output);

        let doc_ids: Vec<u64> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("document"))
            .filter_map(|e| e.get("id").and_then(|v| v.as_u64()))
            .collect();
        let unique: HashSet<u64> = doc_ids.iter().copied().collect();
        assert_eq!(
            doc_ids.len(),
            unique.len(),
            "should have no duplicate document IDs"
        );
    }

    #[test]
    fn test_lsif_hover_content() {
        let code = "struct Widget {\n    count: i32\n}\n";
        let output = generate_test_lsif(code);
        let elements = parse_lsif(&output);

        let hovers: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("hoverResult"))
            .collect();

        assert!(!hovers.is_empty(), "should have hover results");
        let hover = &hovers[0];
        let contents = hover
            .get("result")
            .and_then(|r| r.get("contents"))
            .and_then(|c| c.as_array());
        assert!(contents.is_some(), "hover should have contents array");

        let first_content = &contents.unwrap()[0];
        assert_eq!(
            first_content.get("language").and_then(|l| l.as_str()),
            Some("fe")
        );
        let value = first_content.get("value").and_then(|v| v.as_str()).unwrap();
        assert!(
            value.contains("Widget"),
            "hover for Widget should contain 'Widget', got: {value}"
        );
    }

    #[test]
    fn test_lsif_range_positions() {
        let output = generate_test_lsif("fn test_fn() -> i32 {\n    42\n}\n");
        let elements = parse_lsif(&output);

        let ranges: Vec<_> = elements
            .iter()
            .filter(|e| e.get("label").and_then(|l| l.as_str()) == Some("range"))
            .collect();
        assert!(!ranges.is_empty(), "should have range vertices");

        for range in &ranges {
            let start = range.get("start").expect("range must have start");
            let end = range.get("end").expect("range must have end");
            assert!(start.get("line").is_some(), "start must have line");
            assert!(
                start.get("character").is_some(),
                "start must have character"
            );
            assert!(end.get("line").is_some(), "end must have line");
            assert!(end.get("character").is_some(), "end must have character");

            let start_line = start["line"].as_u64().unwrap();
            let end_line = end["line"].as_u64().unwrap();
            assert!(end_line >= start_line, "end line should be >= start line");
        }
    }

    #[test]
    fn test_lsif_metadata() {
        let output = generate_test_lsif("fn x() -> i32 { 1 }\n");
        let elements = parse_lsif(&output);

        let meta = &elements[0];
        assert_eq!(meta["label"].as_str(), Some("metaData"));
        assert_eq!(meta["version"].as_str(), Some("0.4.0"));
        assert_eq!(meta["positionEncoding"].as_str(), Some("utf-16"));
        assert!(meta.get("toolInfo").is_some());
        assert_eq!(meta["toolInfo"]["name"].as_str(), Some("fe-lsif"));
    }

    #[test]
    fn test_utf16_column_counts_surrogate_pairs() {
        let text = "a😀b";
        // byte offsets: a=0..1, 😀=1..5, b=5..6
        assert_eq!(utf16_column(text, 0, 1), Some(1));
        assert_eq!(utf16_column(text, 0, 5), Some(3));
        assert_eq!(utf16_column(text, 0, 6), Some(4));
    }
}
