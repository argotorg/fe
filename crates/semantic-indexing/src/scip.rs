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
