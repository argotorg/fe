use std::collections::HashMap;

use ::scip::types;

use crate::index::{SemanticIndex, IndexSymbol};

pub struct ScipResult {
    pub index: types::Index,
    pub doc_urls: HashMap<String, String>,
}

/// Generate SCIP output by reading from a pre-populated SemanticIndex.
///
/// This is the incremental path: no HIR walking needed.
pub fn generate_from_index(
    db: &dyn salsa::Database,
    semantic_index: SemanticIndex,
    ingot_name: &str,
    ingot_version: &str,
    ingot_url: &url::Url,
) -> ScipResult {
    let prefix = format!("fe fe {} {} ", ingot_name, ingot_version);
    let view = semantic_index.symbols_for_prefix(db, prefix);

    let mut documents: HashMap<String, DocumentBuilder> = HashMap::new();
    let mut all_doc_urls: HashMap<String, String> = HashMap::new();

    for (_key, sym) in view.iter() {
        let doc = documents
            .entry(sym.def_location.file_url.clone())
            .or_insert_with(|| DocumentBuilder::new(sym.def_location.relative_path.clone()));

        if doc.seen_symbols.insert(sym.symbol.clone()) {
            doc.symbols.push(types::SymbolInformation {
                symbol: sym.symbol.clone(),
                documentation: sym.documentation.clone(),
                relationships: Vec::new(),
                kind: protobuf::EnumOrUnknown::from_i32(sym.kind),
                display_name: sym.display_name.clone(),
                signature_documentation: None.into(),
                enclosing_symbol: sym.enclosing_symbol.clone(),
                special_fields: Default::default(),
            });
        }

        if sym.def_location.range != vec![0, 0, 0] {
            push_occurrence(
                doc,
                sym.def_location.range.clone(),
                sym.symbol.clone(),
                types::SymbolRole::Definition as i32,
            );
        }

        if let Some(url) = &sym.doc_url {
            all_doc_urls.insert(sym.symbol.clone(), url.clone());
        }

        if let Some(refs) = semantic_index.references_to(db, &sym.symbol) {
            for r in &refs {
                let ref_doc = documents
                    .entry(r.location.file_url.clone())
                    .or_insert_with(|| DocumentBuilder::new(r.location.relative_path.clone()));
                push_occurrence(ref_doc, r.location.range.clone(), r.symbol.clone(), r.role);
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
        .map(DocumentBuilder::into_document)
        .collect();
    docs.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    index.documents = docs;

    ScipResult {
        index,
        doc_urls: all_doc_urls,
    }
}

// --- Internal helpers ---

use std::collections::HashSet;

#[derive(Default)]
struct DocumentBuilder {
    relative_path: String,
    occurrences: Vec<types::Occurrence>,
    symbols: Vec<types::SymbolInformation>,
    seen_symbols: HashSet<String>,
    seen_occurrences: HashSet<(Vec<i32>, String)>,
}

impl DocumentBuilder {
    fn new(relative_path: String) -> Self {
        Self { relative_path, ..Default::default() }
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

fn push_occurrence(
    doc: &mut DocumentBuilder,
    range: Vec<i32>,
    symbol: String,
    symbol_roles: i32,
) {
    let key = (range.clone(), symbol.clone());
    if !doc.seen_occurrences.insert(key) {
        return;
    }
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

/// Convert a SCIP Index into a compact JSON string for browser embedding.
pub fn scip_to_json_data(index: &types::Index, doc_urls: &HashMap<String, String>) -> String {
    use serde_json::{Map, Value, json};

    let mut symbols = Map::new();
    let mut files: HashMap<String, Vec<Value>> = HashMap::new();

    for doc in &index.documents {
        for si in &doc.symbols {
            if symbols.contains_key(&si.symbol) {
                continue;
            }
            let kind = si.kind.value();
            let docs: Vec<Value> = si.documentation.iter().map(|d| Value::String(d.clone())).collect();
            let mut entry = Map::new();
            entry.insert("name".into(), Value::String(si.display_name.clone()));
            entry.insert("kind".into(), Value::Number(kind.into()));
            if !docs.is_empty() {
                entry.insert("docs".into(), Value::Array(docs));
            }
            if !si.enclosing_symbol.is_empty() {
                entry.insert("enclosing".into(), Value::String(si.enclosing_symbol.clone()));
            }
            if let Some(url) = doc_urls.get(&si.symbol) {
                entry.insert("doc_url".into(), Value::String(url.clone()));
            }
            symbols.insert(si.symbol.clone(), Value::Object(entry));
        }

        let file_key = if doc.relative_path.is_empty() {
            doc.symbols.first()
                .and_then(|si| {
                    let parts: Vec<&str> = si.symbol.split(' ').collect();
                    parts.iter().rev()
                        .find(|p| !p.is_empty() && **p != "/")
                        .map(|s| {
                            let name = s.trim_end_matches('/').trim_end_matches('#');
                            format!("{}.fe", name)
                        })
                })
                .unwrap_or_else(|| "input.fe".to_string())
        } else {
            doc.relative_path.clone()
        };
        let file_occs = files.entry(file_key).or_default();
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
            let mut obj = json!({ "line": line, "cs": cs, "ce": ce, "sym": occ.symbol });
            if is_def {
                obj.as_object_mut().unwrap().insert("def".into(), Value::Bool(true));
            }
            file_occs.push(obj);
        }
    }

    for occs in files.values_mut() {
        occs.sort_by(|a, b| {
            let al = a["line"].as_i64().unwrap_or(0);
            let bl = b["line"].as_i64().unwrap_or(0);
            al.cmp(&bl).then(a["cs"].as_i64().unwrap_or(0).cmp(&b["cs"].as_i64().unwrap_or(0)))
        });
    }

    let mut root = Map::new();
    root.insert("symbols".into(), Value::Object(symbols));
    root.insert("files".into(), serde_json::to_value(files).unwrap_or(Value::Object(Map::new())));
    Value::Object(root).to_string()
}
