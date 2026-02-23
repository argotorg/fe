//! WASM query module for browser-side SCIP symbol resolution
//!
//! Provides `ScipStore` — a WASM-friendly wrapper around a SCIP index
//! that can be constructed from protobuf bytes and queried from JavaScript.
//! Replaces the earlier `DocStore` which operated on `DocIndex` JSON.

use std::collections::HashMap;

use wasm_bindgen::prelude::*;

/// A reference to an occurrence within the parsed SCIP index.
struct OccRef {
    file_idx: u32,
    #[allow(dead_code)]
    occ_idx: u32,
}

/// A single occurrence sorted by position within a file.
struct SortedOcc {
    line: u32,
    col_start: u32,
    col_end: u32,
    symbol: String,
    roles: i32,
}

/// Per-file index of sorted occurrences for position lookups.
struct FileIndex {
    #[allow(dead_code)]
    path: String,
    occurrences: Vec<SortedOcc>,
}

/// Compact symbol info extracted from SCIP SymbolInformation.
struct SymInfo {
    symbol: String,
    display_name: String,
    kind: i32,
    documentation: Vec<String>,
    enclosing_symbol: String,
    relationships: Vec<SymRelationship>,
}

struct SymRelationship {
    symbol: String,
    is_implementation: bool,
    #[allow(dead_code)]
    is_reference: bool,
    #[allow(dead_code)]
    is_type_definition: bool,
}

/// SCIP-powered symbol store for the browser.
///
/// Loads a SCIP protobuf index and builds in-memory lookup structures
/// for symbol resolution, search, and navigation.
#[wasm_bindgen]
pub struct ScipStore {
    /// symbol string → SymInfo
    sym_info: HashMap<String, SymInfo>,
    /// symbol string → all occurrences across files
    sym_occurrences: HashMap<String, Vec<OccRef>>,
    /// file relative path → sorted occurrences by (line, col)
    file_index: HashMap<String, FileIndex>,
    /// All symbol strings for search
    all_symbols: Vec<String>,
}

#[wasm_bindgen]
impl ScipStore {
    /// Create a ScipStore from raw SCIP protobuf bytes.
    #[wasm_bindgen(constructor)]
    pub fn new(scip_bytes: &[u8]) -> Result<ScipStore, JsError> {
        use protobuf::Message;
        let index = scip::types::Index::parse_from_bytes(scip_bytes)
            .map_err(|e| JsError::new(&format!("Failed to parse SCIP: {e}")))?;

        let mut sym_info: HashMap<String, SymInfo> = HashMap::new();
        let mut sym_occurrences: HashMap<String, Vec<OccRef>> = HashMap::new();
        let mut file_index: HashMap<String, FileIndex> = HashMap::new();

        // Process external symbols
        for si in &index.external_symbols {
            if !si.symbol.is_empty() {
                sym_info.entry(si.symbol.clone()).or_insert_with(|| SymInfo {
                    symbol: si.symbol.clone(),
                    display_name: si.display_name.clone(),
                    kind: si.kind.value(),
                    documentation: si.documentation.clone(),
                    enclosing_symbol: si.enclosing_symbol.clone(),
                    relationships: si
                        .relationships
                        .iter()
                        .map(|r| SymRelationship {
                            symbol: r.symbol.clone(),
                            is_implementation: r.is_implementation,
                            is_reference: r.is_reference,
                            is_type_definition: r.is_type_definition,
                        })
                        .collect(),
                });
            }
        }

        // Process each document
        for (file_idx, doc) in index.documents.iter().enumerate() {
            let file_idx = file_idx as u32;

            // Index symbol information from this document
            for si in &doc.symbols {
                if !si.symbol.is_empty() {
                    sym_info.entry(si.symbol.clone()).or_insert_with(|| SymInfo {
                        symbol: si.symbol.clone(),
                        display_name: si.display_name.clone(),
                        kind: si.kind.value(),
                        documentation: si.documentation.clone(),
                        enclosing_symbol: si.enclosing_symbol.clone(),
                        relationships: si
                            .relationships
                            .iter()
                            .map(|r| SymRelationship {
                                symbol: r.symbol.clone(),
                                is_implementation: r.is_implementation,
                                is_reference: r.is_reference,
                                is_type_definition: r.is_type_definition,
                            })
                            .collect(),
                    });
                }
            }

            // Build sorted occurrence list for this file
            let mut sorted_occs = Vec::with_capacity(doc.occurrences.len());
            for (occ_idx, occ) in doc.occurrences.iter().enumerate() {
                let (line, col_start, col_end) = parse_range(&occ.range);

                // Track symbol → occurrence mapping
                if !occ.symbol.is_empty() {
                    sym_occurrences
                        .entry(occ.symbol.clone())
                        .or_default()
                        .push(OccRef {
                            file_idx,
                            occ_idx: occ_idx as u32,
                        });
                }

                sorted_occs.push(SortedOcc {
                    line,
                    col_start,
                    col_end,
                    symbol: occ.symbol.clone(),
                    roles: occ.symbol_roles,
                });
            }

            // Sort by (line, col_start) for binary search
            sorted_occs.sort_by(|a, b| a.line.cmp(&b.line).then(a.col_start.cmp(&b.col_start)));

            file_index.insert(
                doc.relative_path.clone(),
                FileIndex {
                    path: doc.relative_path.clone(),
                    occurrences: sorted_occs,
                },
            );
        }

        // Collect all symbol strings for search
        let all_symbols: Vec<String> = sym_info.keys().cloned().collect();

        Ok(ScipStore {
            sym_info,
            sym_occurrences,
            file_index,
            all_symbols,
        })
    }

    /// Resolve the symbol at a file position (click-to-navigate).
    ///
    /// Returns the SCIP symbol string, or null if nothing at that position.
    #[wasm_bindgen(js_name = "resolveSymbol")]
    pub fn resolve_symbol(&self, file: &str, line: u32, col: u32) -> Option<String> {
        let fi = self.file_index.get(file)?;
        // Binary search for the line, then scan for column overlap
        let start = fi
            .occurrences
            .partition_point(|o| o.line < line);

        for occ in &fi.occurrences[start..] {
            if occ.line > line {
                break;
            }
            if occ.line == line && col >= occ.col_start && col < occ.col_end {
                if !occ.symbol.is_empty() {
                    return Some(occ.symbol.clone());
                }
            }
        }
        None
    }

    /// Find all occurrences of a symbol across files.
    ///
    /// Returns a JSON array of `{file, line, col_start, col_end, is_def}`.
    #[wasm_bindgen(js_name = "findReferences")]
    pub fn find_references(&self, symbol: &str) -> String {
        let Some(occs) = self.sym_occurrences.get(symbol) else {
            return "[]".to_string();
        };

        let mut results = Vec::new();

        for occ_ref in occs {
            // Look up the file path by index (documents are inserted in order)
            let file_path = self
                .file_index
                .iter()
                .nth(occ_ref.file_idx as usize)
                .map(|(k, _)| k.as_str())
                .unwrap_or("");

            if let Some(fi) = self.file_index.get(file_path) {
                if let Some(occ) = fi
                    .occurrences
                    .iter()
                    .find(|o| o.symbol == symbol)
                {
                    let is_def = (occ.roles & (scip::types::SymbolRole::Definition as i32)) != 0;
                    results.push(format!(
                        r#"{{"file":"{}","line":{},"col_start":{},"col_end":{},"is_def":{}}}"#,
                        escape_json_string(file_path),
                        occ.line,
                        occ.col_start,
                        occ.col_end,
                        is_def
                    ));
                }
            }
        }
        // Deduplicate since we may find the same occurrence multiple times
        results.dedup();

        format!("[{}]", results.join(","))
    }

    /// Get symbol documentation and metadata (for hover).
    ///
    /// Returns JSON: `{symbol, display_name, kind, documentation, enclosing_symbol}`
    /// or null if unknown.
    #[wasm_bindgen(js_name = "symbolInfo")]
    pub fn symbol_info(&self, symbol: &str) -> Option<String> {
        let info = self.sym_info.get(symbol)?;
        let docs_json: Vec<String> = info
            .documentation
            .iter()
            .map(|d| format!("\"{}\"", escape_json_string(d)))
            .collect();
        Some(format!(
            r#"{{"symbol":"{}","display_name":"{}","kind":{},"documentation":[{}],"enclosing_symbol":"{}"}}"#,
            escape_json_string(&info.symbol),
            escape_json_string(&info.display_name),
            info.kind,
            docs_json.join(","),
            escape_json_string(&info.enclosing_symbol),
        ))
    }

    /// Search symbols by name substring.
    ///
    /// Returns a JSON array of `{symbol, display_name, kind}` (max 30).
    pub fn search(&self, query: &str) -> String {
        let query_lower = query.to_lowercase();
        let mut results = Vec::new();

        for sym_str in &self.all_symbols {
            if results.len() >= 30 {
                break;
            }
            if let Some(info) = self.sym_info.get(sym_str) {
                if info.display_name.to_lowercase().contains(&query_lower)
                    || sym_str.to_lowercase().contains(&query_lower)
                {
                    results.push(format!(
                        r#"{{"symbol":"{}","display_name":"{}","kind":{}}}"#,
                        escape_json_string(sym_str),
                        escape_json_string(&info.display_name),
                        info.kind,
                    ));
                }
            }
        }

        format!("[{}]", results.join(","))
    }

    /// Build the module tree from enclosing_symbol relationships.
    ///
    /// Returns a JSON tree of `{name, symbol, children: [...]}`.
    #[wasm_bindgen(js_name = "moduleTree")]
    pub fn module_tree(&self) -> String {
        // Group symbols by their enclosing_symbol (parent)
        let mut children_of: HashMap<&str, Vec<&str>> = HashMap::new();
        let mut roots = Vec::new();

        for (sym, info) in &self.sym_info {
            if info.enclosing_symbol.is_empty() {
                roots.push(sym.as_str());
            } else {
                children_of
                    .entry(info.enclosing_symbol.as_str())
                    .or_default()
                    .push(sym.as_str());
            }
        }

        fn build_node(
            sym: &str,
            sym_info: &HashMap<String, SymInfo>,
            children_of: &HashMap<&str, Vec<&str>>,
        ) -> String {
            let display_name = sym_info
                .get(sym)
                .map(|i| i.display_name.as_str())
                .unwrap_or("");
            let kind = sym_info.get(sym).map(|i| i.kind).unwrap_or(0);

            let children = children_of
                .get(sym)
                .map(|kids| {
                    kids.iter()
                        .map(|k| build_node(k, sym_info, children_of))
                        .collect::<Vec<_>>()
                        .join(",")
                })
                .unwrap_or_default();

            format!(
                r#"{{"name":"{}","symbol":"{}","kind":{},"children":[{}]}}"#,
                escape_json_string(display_name),
                escape_json_string(sym),
                kind,
                children,
            )
        }

        let nodes: Vec<String> = roots
            .iter()
            .map(|r| build_node(r, &self.sym_info, &children_of))
            .collect();

        format!("[{}]", nodes.join(","))
    }

    /// Get children of a symbol grouped by kind.
    ///
    /// Returns JSON: `{fields: [...], methods: [...], variants: [...]}`.
    #[wasm_bindgen(js_name = "symbolChildren")]
    pub fn symbol_children(&self, symbol: &str) -> String {
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut variants = Vec::new();
        let mut types = Vec::new();
        let mut other = Vec::new();

        for (sym, info) in &self.sym_info {
            if info.enclosing_symbol == symbol {
                let entry = format!(
                    r#"{{"symbol":"{}","display_name":"{}","kind":{}}}"#,
                    escape_json_string(sym),
                    escape_json_string(&info.display_name),
                    info.kind,
                );
                // Use SCIP symbol_information::Kind values
                match info.kind {
                    15 => fields.push(entry),         // Field
                    12 => variants.push(entry),       // EnumMember
                    26 | 17 => methods.push(entry),   // Method | Function
                    54 | 55 => types.push(entry),     // Type | TypeAlias
                    _ => other.push(entry),
                }
            }
        }

        format!(
            r#"{{"fields":[{}],"methods":[{}],"variants":[{}],"types":[{}],"other":[{}]}}"#,
            fields.join(","),
            methods.join(","),
            variants.join(","),
            types.join(","),
            other.join(","),
        )
    }

    /// Types that implement a given trait (via SCIP relationships).
    ///
    /// Returns a JSON array of `{symbol, display_name}`.
    pub fn implementors(&self, trait_symbol: &str) -> String {
        let mut results = Vec::new();

        for (sym, info) in &self.sym_info {
            for rel in &info.relationships {
                if rel.is_implementation && rel.symbol == trait_symbol {
                    results.push(format!(
                        r#"{{"symbol":"{}","display_name":"{}"}}"#,
                        escape_json_string(sym),
                        escape_json_string(&info.display_name),
                    ));
                }
            }
        }

        format!("[{}]", results.join(","))
    }

    /// Total number of symbols in the index.
    #[wasm_bindgen(js_name = "symbolCount")]
    pub fn symbol_count(&self) -> usize {
        self.sym_info.len()
    }

    /// List all files in the index.
    ///
    /// Returns a JSON array of file paths.
    pub fn files(&self) -> String {
        let paths: Vec<String> = self
            .file_index
            .keys()
            .map(|p| format!("\"{}\"", escape_json_string(p)))
            .collect();
        format!("[{}]", paths.join(","))
    }
}

/// Parse a SCIP range (3 or 4 elements) into (line, col_start, col_end).
fn parse_range(range: &[i32]) -> (u32, u32, u32) {
    match range.len() {
        3 => (range[0] as u32, range[1] as u32, range[2] as u32),
        4 => {
            // Multi-line range: use start line, start col, and end col
            // For position lookups we only match the start line
            (range[0] as u32, range[1] as u32, range[3] as u32)
        }
        _ => (0, 0, 0),
    }
}

/// Minimal JSON string escaping for manual JSON construction.
fn escape_json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            _ => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal SCIP index in-memory for testing.
    fn make_test_scip() -> Vec<u8> {
        use protobuf::Message;

        let mut index = scip::types::Index::new();
        index.metadata = Some(scip::types::Metadata {
            version: scip::types::ProtocolVersion::UnspecifiedProtocolVersion.into(),
            tool_info: Some(scip::types::ToolInfo {
                name: "test".to_string(),
                version: "0.1".to_string(),
                ..Default::default()
            })
            .into(),
            project_root: "file:///test/".to_string(),
            ..Default::default()
        })
        .into();

        let mut doc = scip::types::Document::new();
        doc.relative_path = "test.fe".to_string();
        doc.language = "fe".to_string();

        // Symbol: Point struct definition
        doc.symbols.push(scip::types::SymbolInformation {
            symbol: "fe fe test 0.1 Point#".to_string(),
            display_name: "Point".to_string(),
            kind: scip::types::symbol_information::Kind::Struct.into(),
            documentation: vec!["```fe\nstruct Point\n```\n\nA 2D point.".to_string()],
            enclosing_symbol: String::new(),
            ..Default::default()
        });

        // Symbol: make_point function
        doc.symbols.push(scip::types::SymbolInformation {
            symbol: "fe fe test 0.1 make_point.".to_string(),
            display_name: "make_point".to_string(),
            kind: scip::types::symbol_information::Kind::Function.into(),
            documentation: vec!["```fe\nfn make_point() -> Point\n```".to_string()],
            enclosing_symbol: String::new(),
            ..Default::default()
        });

        // Occurrence: Point definition at line 0, col 7-12
        doc.occurrences.push(scip::types::Occurrence {
            range: vec![0, 7, 12],
            symbol: "fe fe test 0.1 Point#".to_string(),
            symbol_roles: scip::types::SymbolRole::Definition as i32,
            ..Default::default()
        });

        // Occurrence: Point reference at line 5, col 20-25
        doc.occurrences.push(scip::types::Occurrence {
            range: vec![5, 20, 25],
            symbol: "fe fe test 0.1 Point#".to_string(),
            symbol_roles: 0,
            ..Default::default()
        });

        // Occurrence: make_point definition at line 5, col 3-13
        doc.occurrences.push(scip::types::Occurrence {
            range: vec![5, 3, 13],
            symbol: "fe fe test 0.1 make_point.".to_string(),
            symbol_roles: scip::types::SymbolRole::Definition as i32,
            ..Default::default()
        });

        index.documents.push(doc);
        index.write_to_bytes().expect("serialize SCIP index")
    }

    #[test]
    fn load_and_count() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");
        assert_eq!(store.symbol_count(), 2);
    }

    #[test]
    fn resolve_symbol_at_position() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        // Definition site
        let sym = store.resolve_symbol("test.fe", 0, 8);
        assert_eq!(sym.as_deref(), Some("fe fe test 0.1 Point#"));

        // Reference site
        let sym = store.resolve_symbol("test.fe", 5, 21);
        assert_eq!(sym.as_deref(), Some("fe fe test 0.1 Point#"));

        // No symbol at this position
        let sym = store.resolve_symbol("test.fe", 0, 0);
        assert!(sym.is_none());
    }

    #[test]
    fn symbol_info_returns_json() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        let info = store.symbol_info("fe fe test 0.1 Point#");
        assert!(info.is_some());
        let json = info.unwrap();
        assert!(json.contains("\"display_name\":\"Point\""));
        assert!(json.contains("\"kind\":49")); // Struct = 49
    }

    #[test]
    fn symbol_info_not_found() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");
        assert!(store.symbol_info("nonexistent").is_none());
    }

    #[test]
    fn search_by_name() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        // "Point" matches display_name "Point" and also "make_point" via symbol string
        let results = store.search("Point");
        assert!(results.contains("Point"));

        // Case-insensitive: finds both
        let results = store.search("point");
        assert!(results.contains("Point"));
        assert!(results.contains("make_point"));

        // Exact display_name search
        let results = store.search("make_point");
        assert!(results.contains("make_point"));
        assert!(!results.contains("\"display_name\":\"Point\""));
    }

    #[test]
    fn find_references_returns_json() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        let refs = store.find_references("fe fe test 0.1 Point#");
        assert!(refs.contains("test.fe"));
        assert!(refs.contains("\"is_def\":true") || refs.contains("\"is_def\":false"));
    }

    #[test]
    fn files_list() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        let files = store.files();
        assert!(files.contains("test.fe"));
    }

    #[test]
    fn module_tree_structure() {
        let bytes = make_test_scip();
        let store = ScipStore::new(&bytes).expect("load scip");

        let tree = store.module_tree();
        // Should be valid JSON array
        assert!(tree.starts_with('['));
        assert!(tree.ends_with(']'));
        // Should contain our symbols at root level (no enclosing_symbol)
        assert!(tree.contains("Point"));
    }

    #[test]
    fn escape_json_string_handles_special_chars() {
        assert_eq!(escape_json_string("hello"), "hello");
        assert_eq!(escape_json_string("he\"llo"), "he\\\"llo");
        assert_eq!(escape_json_string("line\nnew"), "line\\nnew");
        assert_eq!(escape_json_string("back\\slash"), "back\\\\slash");
    }

    #[test]
    fn empty_index() {
        use protobuf::Message;
        let index = scip::types::Index::new();
        let bytes = index.write_to_bytes().expect("serialize");
        let store = ScipStore::new(&bytes).expect("load");
        assert_eq!(store.symbol_count(), 0);
        assert_eq!(store.search("anything"), "[]");
        assert_eq!(store.module_tree(), "[]");
    }
}
