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
