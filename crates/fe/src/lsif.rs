use std::io::{self, Write};

use common::InputDb;
use common::diagnostics::Span;
use hir::{
    HirDb, SpannedHirDb,
    hir_def::{
        Attr, ItemKind,
        scope_graph::ScopeId,
        HirIngot,
    },
    span::LazySpan,
};

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

/// Compute line offsets for a text string.
fn calculate_line_offsets(text: &str) -> Vec<usize> {
    text.lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect()
}

/// Convert a Span to an LsifRange.
fn span_to_range(span: &Span, db: &dyn InputDb) -> Option<LsifRange> {
    let text = span.file.text(db);
    let line_offsets = calculate_line_offsets(text);
    if line_offsets.is_empty() {
        return None;
    }

    let start: usize = span.range.start().into();
    let end: usize = span.range.end().into();

    let start_line = line_offsets
        .binary_search(&start)
        .unwrap_or_else(|x| x.saturating_sub(1));
    let end_line = line_offsets
        .binary_search(&end)
        .unwrap_or_else(|x| x.saturating_sub(1));

    let start_character = start.saturating_sub(line_offsets[start_line]);
    let end_character = end.saturating_sub(line_offsets[end_line]);

    Some(LsifRange {
        start: LsifPos {
            line: start_line as u32,
            character: start_character as u32,
        },
        end: LsifPos {
            line: end_line as u32,
            character: end_character as u32,
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
        Self {
            writer,
            next_id: 1,
        }
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

/// Get the docstring for a scope.
fn get_docstring(db: &dyn HirDb, scope: ScopeId) -> Option<String> {
    scope
        .attrs(db)?
        .data(db)
        .iter()
        .filter_map(|attr| {
            if let Attr::DocComment(doc) = attr {
                Some(doc.text.data(db).clone())
            } else {
                None
            }
        })
        .reduce(|a, b| a + "\n" + &b)
}

/// Get a simple definition string for an item (used for hover).
fn get_item_definition(db: &dyn SpannedHirDb, item: ItemKind) -> Option<String> {
    let span = item.span().resolve(db)?;

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    // Trim body for functions, modules
    let body_start = match item {
        ItemKind::Func(func) => Some(func.body(db)?.span().resolve(db)?.range.start()),
        ItemKind::Mod(module) => Some(module.scope().name_span(db)?.resolve(db)?.range.end()),
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = body_start.into();
    }

    // Start at the beginning of the name line
    let name_span = item.name_span()?.resolve(db);
    if let Some(name_span) = name_span {
        let file_text = span.file.text(db).as_str();
        let mut name_line_start: usize = name_span.range.start().into();
        while name_line_start > 0 && file_text.as_bytes().get(name_line_start - 1) != Some(&b'\n')
        {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let item_definition = span.file.text(db).as_str()[start..end].to_string();
    Some(item_definition.trim().to_string())
}

/// Build hover content for an item (definition + docstring).
fn build_hover_content(db: &driver::DriverDataBase, item: ItemKind) -> Option<String> {
    let definition = get_item_definition(db, item)?;
    let docstring = get_docstring(db, item.scope());

    if let Some(doc) = docstring {
        Some(format!("{definition}\n\n{doc}"))
    } else {
        Some(definition)
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

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "Could not resolve ingot",
        ));
    };

    // Get ingot name and version for monikers
    let ingot_name = ingot
        .config(db)
        .and_then(|c| c.metadata.name)
        .map(|n| n.to_string())
        .unwrap_or_else(|| "unknown".to_string());
    let ingot_version = ingot
        .version(db)
        .map(|v| v.to_string())
        .unwrap_or_else(|| "0.0.0".to_string());

    let mut document_ids = Vec::new();

    // Process each module
    for top_mod in ingot.all_modules(db) {
        let scope_graph = top_mod.scope_graph(db);

        // Determine document URI from the top-level module span
        let doc_span = top_mod.span().resolve(db);
        let doc_url = match &doc_span {
            Some(span) => match span.file.url(db) {
                Some(url) => url.to_string(),
                None => continue,
            },
            None => continue,
        };

        let doc_id = emitter.emit_document(&doc_url)?;
        document_ids.push(doc_id);

        let mut doc_range_ids = Vec::new();

        // Iterate all items in this module
        for item in scope_graph.items_dfs(db) {
            let scope = ScopeId::from_item(item);

            // Get name span for the item
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
            doc_range_ids.push(range_id);

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

            // Reference result: find all references across the ingot
            let target = hir::core::semantic::reference::Target::Scope(scope);
            let ref_result_id = emitter.emit_reference_result()?;
            emitter.emit_edge("textDocument/references", result_set_id, ref_result_id)?;

            // Definition is also a reference
            emitter.emit_edge_many("item", ref_result_id, &[range_id], Some(doc_id))?;

            // Collect references from all modules in the ingot
            for ref_mod in ingot.all_modules(db) {
                let refs = ref_mod.references_to_target(db, &target);
                if refs.is_empty() {
                    continue;
                }

                // Determine the document for these references
                let ref_doc_span = ref_mod.span().resolve(db);
                let ref_doc_url = match &ref_doc_span {
                    Some(span) => match span.file.url(db) {
                        Some(url) => url.to_string(),
                        None => continue,
                    },
                    None => continue,
                };

                // We need a document ID for each referenced file
                // For simplicity, emit ref ranges and item edges inline
                let mut ref_range_ids = Vec::new();
                for reference in refs {
                    let ref_span = reference.span();
                    if let Some(resolved) = ref_span.resolve(db) {
                        if let Some(r) = span_to_range(&resolved, db) {
                            let ref_range_id = emitter.emit_range(r)?;
                            ref_range_ids.push(ref_range_id);
                        }
                    }
                }

                if !ref_range_ids.is_empty() {
                    // Find or create the document for this reference file
                    let ref_doc_id = if ref_doc_url == doc_url {
                        doc_range_ids.extend_from_slice(&ref_range_ids);
                        doc_id
                    } else {
                        // We need a separate document vertex for cross-file references
                        // TODO: cache document IDs to avoid duplicates across items
                        let rd = emitter.emit_document(&ref_doc_url)?;
                        document_ids.push(rd);
                        // We'd need to track ranges per document for contains edges
                        // For now, emit a contains edge for this batch
                        emitter.emit_edge_many("contains", rd, &ref_range_ids, None)?;
                        rd
                    };
                    emitter.emit_edge_many(
                        "item",
                        ref_result_id,
                        &ref_range_ids,
                        Some(ref_doc_id),
                    )?;
                }
            }

            // Moniker
            if let Some(pretty_path) = scope.pretty_path(db) {
                let identifier = format!("{ingot_name}:{ingot_version}:{pretty_path}");
                let moniker_id = emitter.emit_moniker("fe", &identifier)?;
                emitter.emit_edge("moniker/attach", result_set_id, moniker_id)?;
            }
        }

        // Contains edge: document -> all ranges in this document
        if !doc_range_ids.is_empty() {
            emitter.emit_edge_many("contains", doc_id, &doc_range_ids, None)?;
        }
    }

    // Project -> document edges
    if !document_ids.is_empty() {
        emitter.emit_edge_many("contains", project_id, &document_ids, None)?;
    }

    Ok(())
}
