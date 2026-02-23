use std::collections::{HashMap, HashSet};
use std::io;

use camino::{Utf8Path, Utf8PathBuf};
use common::InputDb;
use common::diagnostics::Span;
use hir::{
    hir_def::{HirIngot, ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};

use crate::index_util::{self, LineIndex};
use fe_web::model::DocIndex;
use scip::{
    symbol::format_symbol,
    types::{self, descriptor, symbol_information},
};

#[derive(Default)]
struct ScipDocumentBuilder {
    relative_path: String,
    occurrences: Vec<types::Occurrence>,
    symbols: Vec<types::SymbolInformation>,
    seen_symbols: HashSet<String>,
}

impl ScipDocumentBuilder {
    fn new(relative_path: String) -> Self {
        Self {
            relative_path,
            ..Default::default()
        }
    }

    fn into_document(self) -> types::Document {
        types::Document {
            language: "fe".to_string(),
            relative_path: self.relative_path,
            occurrences: self.occurrences,
            symbols: self.symbols,
            text: String::new(),
            position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart.into(),
            special_fields: Default::default(),
        }
    }
}

fn span_to_scip_range(span: &Span, db: &dyn InputDb) -> Option<Vec<i32>> {
    let text = span.file.text(db);
    let line_index = LineIndex::new(text);

    let start = line_index.position(span.range.start().into());
    let end = line_index.position(span.range.end().into());

    let start_col = start.byte_offset.checked_sub(start.line_start_offset)?;
    let end_col = end.byte_offset.checked_sub(end.line_start_offset)?;

    Some(if start.line == end.line {
        vec![start.line as i32, start_col as i32, end_col as i32]
    } else {
        vec![
            start.line as i32,
            start_col as i32,
            end.line as i32,
            end_col as i32,
        ]
    })
}

fn top_mod_url(
    db: &driver::DriverDataBase,
    top_mod: &hir::hir_def::TopLevelMod<'_>,
) -> Option<url::Url> {
    top_mod.span().resolve(db)?.file.url(db)
}

fn relative_path(project_root: &Utf8Path, doc_url: &url::Url) -> Option<String> {
    let file_path = doc_url.to_file_path().ok()?;
    let file_path = Utf8PathBuf::from_path_buf(file_path).ok()?;
    let relative = file_path.strip_prefix(project_root).ok()?;
    Some(relative.to_string())
}

fn item_descriptor_suffix(item: ItemKind<'_>) -> descriptor::Suffix {
    match item {
        ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Trait(_) => descriptor::Suffix::Type,
        ItemKind::Func(_) => descriptor::Suffix::Term,
        _ => descriptor::Suffix::Meta,
    }
}

fn item_symbol_kind(item: ItemKind<'_>) -> symbol_information::Kind {
    match item {
        ItemKind::Struct(_) => symbol_information::Kind::Struct,
        ItemKind::Enum(_) => symbol_information::Kind::Enum,
        ItemKind::Trait(_) => symbol_information::Kind::Trait,
        ItemKind::Func(_) => symbol_information::Kind::Function,
        _ => symbol_information::Kind::UnspecifiedKind,
    }
}

fn item_symbol<'db>(
    db: &driver::DriverDataBase,
    item: ItemKind<'db>,
    package_name: &str,
    package_version: &str,
) -> Option<(String, String)> {
    let pretty_path = ScopeId::from_item(item).pretty_path(db)?.to_string();
    let mut descriptors = Vec::new();
    let mut parts = pretty_path.split("::").peekable();
    while let Some(part) = parts.next() {
        let suffix = if parts.peek().is_some() {
            descriptor::Suffix::Namespace
        } else {
            item_descriptor_suffix(item)
        };
        descriptors.push(types::Descriptor {
            name: part.to_string(),
            disambiguator: String::new(),
            suffix: suffix.into(),
            special_fields: Default::default(),
        });
    }

    let symbol = types::Symbol {
        scheme: "fe".to_string(),
        package: Some(types::Package {
            manager: "fe".to_string(),
            name: package_name.to_string(),
            version: package_version.to_string(),
            special_fields: Default::default(),
        })
        .into(),
        descriptors,
        special_fields: Default::default(),
    };
    let display_name = pretty_path
        .rsplit("::")
        .next()
        .unwrap_or(pretty_path.as_str())
        .to_string();
    Some((format_symbol(symbol), display_name))
}

fn push_occurrence(
    doc: &mut ScipDocumentBuilder,
    range: Vec<i32>,
    symbol: String,
    symbol_roles: i32,
) {
    doc.occurrences.push(types::Occurrence {
        range,
        symbol,
        symbol_roles,
        override_documentation: Vec::new(),
        syntax_kind: types::SyntaxKind::Identifier.into(),
        diagnostics: Vec::new(),
        enclosing_range: Vec::new(),
        special_fields: Default::default(),
    });
}

pub fn generate_scip(
    db: &mut driver::DriverDataBase,
    ingot_url: &url::Url,
) -> io::Result<types::Index> {
    let ctx = index_util::IngotContext::resolve(db, ingot_url)?;

    let project_root_path = ingot_url
        .to_file_path()
        .ok()
        .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "ingot URL must be file://"))?;

    let mut documents: HashMap<String, ScipDocumentBuilder> = HashMap::new();

    for top_mod in ctx.ingot.all_modules(db) {
        let Some(doc_url) = top_mod_url(db, top_mod) else {
            continue;
        };
        let Some(relative) = relative_path(&project_root_path, &doc_url) else {
            continue;
        };
        documents
            .entry(doc_url.to_string())
            .or_insert_with(|| ScipDocumentBuilder::new(relative));
    }

    for top_mod in ctx.ingot.all_modules(db) {
        let scope_graph = top_mod.scope_graph(db);
        let Some(doc_url) = top_mod_url(db, top_mod).map(|u| u.to_string()) else {
            continue;
        };

        for item in scope_graph.items_dfs(db) {
            let Some((symbol, display_name)) = item_symbol(db, item, &ctx.name, &ctx.version)
            else {
                continue;
            };

            if let Some(doc) = documents.get_mut(&doc_url) {
                if doc.seen_symbols.insert(symbol.clone()) {
                    doc.symbols.push(types::SymbolInformation {
                        symbol: symbol.clone(),
                        documentation: index_util::hover_parts(db, item).to_scip_documentation(),
                        relationships: Vec::new(),
                        kind: item_symbol_kind(item).into(),
                        display_name,
                        signature_documentation: None.into(),
                        enclosing_symbol: String::new(),
                        special_fields: Default::default(),
                    });
                }

                if let Some(name_span) = item.name_span().and_then(|span| span.resolve(db))
                    && let Some(range) = span_to_scip_range(&name_span, db)
                {
                    push_occurrence(
                        doc,
                        range,
                        symbol.clone(),
                        types::SymbolRole::Definition as i32,
                    );
                }
            }

            // Use ReferenceIndex instead of scanning all modules
            let scope = ScopeId::from_item(item);
            for indexed_ref in ctx.ref_index.references_to(&scope) {
                if let Some(resolved) = indexed_ref.span.resolve(db) {
                    let ref_doc_url = match resolved.file.url(db) {
                        Some(url) => url.to_string(),
                        None => continue,
                    };
                    if let Some(ref_doc) = documents.get_mut(&ref_doc_url)
                        && let Some(range) = span_to_scip_range(&resolved, db)
                    {
                        push_occurrence(ref_doc, range, symbol.clone(), 0);
                    }
                }
            }
        }
    }

    let mut index = types::Index::new();
    index.metadata = Some(types::Metadata {
        version: types::ProtocolVersion::UnspecifiedProtocolVersion.into(),
        tool_info: Some(types::ToolInfo {
            name: "fe-scip".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            arguments: Vec::new(),
            special_fields: Default::default(),
        })
        .into(),
        project_root: ingot_url.to_string(),
        text_document_encoding: types::TextEncoding::UTF8.into(),
        special_fields: Default::default(),
    })
    .into();

    let mut docs: Vec<_> = documents
        .into_values()
        .map(ScipDocumentBuilder::into_document)
        .collect();
    docs.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    index.documents = docs;

    Ok(index)
}

/// Convert a SCIP Index into a compact JSON string for browser embedding.
///
/// The JSON has two top-level keys:
/// - `symbols`: map from SCIP symbol string to metadata
/// - `files`: map from relative file path to sorted occurrence arrays
pub fn scip_to_json_data(index: &types::Index) -> String {
    use serde_json::{Map, Value, json};

    let mut symbols = Map::new();
    let mut files: HashMap<String, Vec<Value>> = HashMap::new();

    for doc in &index.documents {
        // Collect symbol info from this document
        for si in &doc.symbols {
            if symbols.contains_key(&si.symbol) {
                continue;
            }
            let kind = si.kind.value();
            let docs: Vec<Value> = si
                .documentation
                .iter()
                .map(|d| Value::String(d.clone()))
                .collect();
            let mut entry = Map::new();
            entry.insert("name".into(), Value::String(si.display_name.clone()));
            entry.insert("kind".into(), Value::Number(kind.into()));
            if !docs.is_empty() {
                entry.insert("docs".into(), Value::Array(docs));
            }
            if !si.enclosing_symbol.is_empty() {
                entry.insert(
                    "enclosing".into(),
                    Value::String(si.enclosing_symbol.clone()),
                );
            }
            symbols.insert(si.symbol.clone(), Value::Object(entry));
        }

        // Collect occurrences
        let file_occs = files.entry(doc.relative_path.clone()).or_default();
        for occ in &doc.occurrences {
            if occ.range.is_empty() || occ.symbol.is_empty() {
                continue;
            }
            let line = occ.range[0];
            let cs = occ.range[1];
            let ce = if occ.range.len() == 3 {
                occ.range[2]
            } else if occ.range.len() >= 4 {
                occ.range[3]
            } else {
                continue;
            };
            let is_def = (occ.symbol_roles & (types::SymbolRole::Definition as i32)) != 0;
            let mut obj = json!({
                "line": line,
                "cs": cs,
                "ce": ce,
                "sym": occ.symbol,
            });
            if is_def {
                obj.as_object_mut()
                    .unwrap()
                    .insert("def".into(), Value::Bool(true));
            }
            file_occs.push(obj);
        }
    }

    // Sort occurrences by line, then column
    for occs in files.values_mut() {
        occs.sort_by(|a, b| {
            let al = a["line"].as_i64().unwrap_or(0);
            let bl = b["line"].as_i64().unwrap_or(0);
            al.cmp(&bl)
                .then(a["cs"].as_i64().unwrap_or(0).cmp(&b["cs"].as_i64().unwrap_or(0)))
        });
    }

    let mut root = Map::new();
    root.insert("symbols".into(), Value::Object(symbols));
    root.insert(
        "files".into(),
        serde_json::to_value(files).unwrap_or(Value::Object(Map::new())),
    );
    Value::Object(root).to_string()
}

/// Inject `doc_url` fields into a SCIP JSON string by matching SCIP display names
/// against items in the DocIndex.
pub fn inject_doc_urls(scip_json: &str, doc_index: &DocIndex) -> String {
    let mut root: serde_json::Value = match serde_json::from_str(scip_json) {
        Ok(v) => v,
        Err(_) => return scip_json.to_string(),
    };

    // Build name → url_path lookup from DocIndex items
    let mut name_to_url: HashMap<&str, String> = HashMap::new();
    for item in &doc_index.items {
        name_to_url.insert(&item.name, item.url_path());
    }

    if let Some(symbols) = root.get_mut("symbols").and_then(|s| s.as_object_mut()) {
        for (_sym_str, entry) in symbols.iter_mut() {
            if let Some(obj) = entry.as_object_mut() {
                if let Some(name) = obj.get("name").and_then(|n| n.as_str()) {
                    if let Some(url) = name_to_url.get(name) {
                        obj.insert(
                            "doc_url".to_string(),
                            serde_json::Value::String(url.clone()),
                        );
                    }
                }
            }
        }
    }

    root.to_string()
}

/// Enrich `rich_signature` fields in a DocIndex using the SCIP symbol table.
///
/// Tokenizes each item's `signature` string and links type-like identifiers
/// to their doc page URLs when they appear in the SCIP index.
pub fn enrich_signatures(index: &mut DocIndex, scip_index: &types::Index) {
    use fe_web::model::SignaturePart;

    // Build display_name → doc_url map from SCIP symbols
    let mut type_urls: HashMap<String, String> = HashMap::new();
    for doc in &scip_index.documents {
        for si in &doc.symbols {
            let kind = si.kind.value();
            // Only link types: Struct(49), Enum(11), Trait(53), TypeAlias(54/55), Contract(7/Class)
            if matches!(kind, 49 | 11 | 53 | 54 | 55 | 7) {
                if !si.display_name.is_empty() && !type_urls.contains_key(&si.display_name) {
                    type_urls.insert(si.display_name.clone(), si.symbol.clone());
                }
            }
        }
    }

    // Build SCIP symbol → DocIndex url_path map
    let mut name_to_url: HashMap<&str, String> = HashMap::new();
    for item in &index.items {
        name_to_url.insert(&item.name, item.url_path());
    }

    // Resolve type_urls to actual doc URLs
    let mut type_doc_urls: HashMap<String, String> = HashMap::new();
    for (display_name, _scip_symbol) in &type_urls {
        if let Some(url) = name_to_url.get(display_name.as_str()) {
            type_doc_urls.insert(display_name.clone(), url.clone());
        }
    }

    if type_doc_urls.is_empty() {
        return;
    }

    fn tokenize_signature(sig: &str, type_urls: &HashMap<String, String>) -> Vec<SignaturePart> {
        let mut parts = Vec::new();
        let mut text_buf = String::new();
        let mut ident_buf = String::new();
        let chars: Vec<char> = sig.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let ch = chars[i];
            if ch.is_alphanumeric() || ch == '_' {
                // Start or continue identifier
                if ident_buf.is_empty() && !text_buf.is_empty() {
                    // Flush text buffer before starting ident
                }
                ident_buf.push(ch);
            } else {
                // End of identifier
                if !ident_buf.is_empty() {
                    if let Some(url) = type_urls.get(&ident_buf) {
                        if !text_buf.is_empty() {
                            parts.push(SignaturePart::text(&text_buf));
                            text_buf.clear();
                        }
                        parts.push(SignaturePart::link(&ident_buf, url));
                    } else {
                        text_buf.push_str(&ident_buf);
                    }
                    ident_buf.clear();
                }
                text_buf.push(ch);
            }
            i += 1;
        }
        // Flush remaining
        if !ident_buf.is_empty() {
            if let Some(url) = type_urls.get(&ident_buf) {
                if !text_buf.is_empty() {
                    parts.push(SignaturePart::text(&text_buf));
                    text_buf.clear();
                }
                parts.push(SignaturePart::link(&ident_buf, url));
            } else {
                text_buf.push_str(&ident_buf);
            }
        }
        if !text_buf.is_empty() {
            parts.push(SignaturePart::text(&text_buf));
        }
        parts
    }

    // Check if a rich signature actually has any links
    fn has_links(parts: &[SignaturePart]) -> bool {
        parts.iter().any(|p| p.link.is_some())
    }

    // Enrich top-level item signatures
    for item in &mut index.items {
        if !item.signature.is_empty() && item.rich_signature.is_empty() {
            let parts = tokenize_signature(&item.signature, &type_doc_urls);
            if has_links(&parts) {
                item.rich_signature = parts;
            }
        }

        // Enrich children (fields, variants, methods)
        for child in &mut item.children {
            if !child.signature.is_empty() && child.rich_signature.is_empty() {
                let parts = tokenize_signature(&child.signature, &type_doc_urls);
                if has_links(&parts) {
                    child.rich_signature = parts;
                }
            }
        }

        // Enrich trait impl signatures
        for trait_impl in &mut item.trait_impls {
            // Enrich impl methods
            for method in &mut trait_impl.methods {
                if !method.signature.is_empty() && method.rich_signature.is_empty() {
                    let parts = tokenize_signature(&method.signature, &type_doc_urls);
                    if has_links(&parts) {
                        method.rich_signature = parts;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_test_scip(code: &str) -> types::Index {
        let mut db = driver::DriverDataBase::default();
        let url = url::Url::parse("file:///test.fe").unwrap();
        db.workspace()
            .touch(&mut db, url.clone(), Some(code.to_string()));
        let ingot_url = url::Url::parse("file:///").unwrap();
        generate_scip(&mut db, &ingot_url).expect("generate scip index")
    }

    #[test]
    fn test_scip_basic_structure() {
        let index = generate_test_scip("fn hello() -> i32 {\n    42\n}\n");

        let metadata = index.metadata.as_ref().expect("metadata");
        assert_eq!(
            metadata.tool_info.as_ref().expect("tool info").name,
            "fe-scip"
        );
        assert_eq!(
            metadata.text_document_encoding.value(),
            types::TextEncoding::UTF8 as i32
        );
        assert!(!index.documents.is_empty(), "should contain documents");
    }

    #[test]
    fn test_scip_contains_symbols_and_occurrences() {
        let code = r#"struct Point {
    x: i32
    y: i32
}

fn make_point() -> Point {
    Point { x: 1, y: 2 }
}
"#;
        let index = generate_test_scip(code);
        let doc = index
            .documents
            .iter()
            .find(|d| d.relative_path == "test.fe")
            .expect("document");

        assert!(
            doc.symbols.iter().any(|s| s.display_name == "Point"),
            "expected Point symbol"
        );
        assert!(
            doc.occurrences
                .iter()
                .any(|o| (o.symbol_roles & (types::SymbolRole::Definition as i32)) != 0),
            "expected at least one definition occurrence"
        );
    }
}
