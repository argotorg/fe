use std::collections::{HashMap, HashSet};
use std::io;

use camino::{Utf8Path, Utf8PathBuf};
use common::InputDb;
use common::diagnostics::Span;
use hir::{
    HirDb, SpannedHirDb,
    core::semantic::reference::Target,
    hir_def::{Attr, HirIngot, ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};
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

fn calculate_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, b) in text.bytes().enumerate() {
        if b == b'\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn span_to_scip_range(span: &Span, db: &dyn InputDb) -> Option<Vec<i32>> {
    let text = span.file.text(db);
    let line_offsets = calculate_line_offsets(text);

    let start: usize = span.range.start().into();
    let end: usize = span.range.end().into();

    let start_line = line_offsets
        .partition_point(|&line_start| line_start <= start)
        .saturating_sub(1);
    let end_line = line_offsets
        .partition_point(|&line_start| line_start <= end)
        .saturating_sub(1);

    let start_col = start.checked_sub(line_offsets[start_line])?;
    let end_col = end.checked_sub(line_offsets[end_line])?;

    Some(if start_line == end_line {
        vec![start_line as i32, start_col as i32, end_col as i32]
    } else {
        vec![
            start_line as i32,
            start_col as i32,
            end_line as i32,
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

fn get_item_definition(db: &dyn SpannedHirDb, item: ItemKind) -> Option<String> {
    let span = item.span().resolve(db)?;

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    let body_start = match item {
        ItemKind::Func(func) => Some(func.body(db)?.span().resolve(db)?.range.start()),
        ItemKind::Mod(module) => Some(module.scope().name_span(db)?.resolve(db)?.range.end()),
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = body_start.into();
    }

    let name_span = item.name_span()?.resolve(db);
    if let Some(name_span) = name_span {
        let file_text = span.file.text(db).as_str();
        let mut name_line_start: usize = name_span.range.start().into();
        while name_line_start > 0 && file_text.as_bytes().get(name_line_start - 1) != Some(&b'\n') {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let item_definition = span.file.text(db).as_str()[start..end].to_string();
    Some(item_definition.trim().to_string())
}

fn build_symbol_documentation(db: &driver::DriverDataBase, item: ItemKind) -> Vec<String> {
    let mut parts = Vec::new();
    if let Some(definition) = get_item_definition(db, item) {
        parts.push(format!("```fe\n{definition}\n```"));
    }
    if let Some(doc) = get_docstring(db, item.scope()) {
        parts.push(doc);
    }
    if parts.is_empty() {
        Vec::new()
    } else {
        vec![parts.join("\n\n")]
    }
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
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "Could not resolve ingot",
        ));
    };

    let project_root_path = ingot_url
        .to_file_path()
        .ok()
        .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "ingot URL must be file://"))?;

    let ingot_name = ingot
        .config(db)
        .and_then(|c| c.metadata.name)
        .map(|n| n.to_string())
        .unwrap_or_else(|| "unknown".to_string());
    let ingot_version = ingot
        .version(db)
        .map(|v| v.to_string())
        .unwrap_or_else(|| "0.0.0".to_string());

    let mut documents: HashMap<String, ScipDocumentBuilder> = HashMap::new();

    for top_mod in ingot.all_modules(db) {
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

    for top_mod in ingot.all_modules(db) {
        let scope_graph = top_mod.scope_graph(db);
        let Some(doc_url) = top_mod_url(db, top_mod).map(|u| u.to_string()) else {
            continue;
        };

        for item in scope_graph.items_dfs(db) {
            let Some((symbol, display_name)) = item_symbol(db, item, &ingot_name, &ingot_version)
            else {
                continue;
            };

            if let Some(doc) = documents.get_mut(&doc_url) {
                if doc.seen_symbols.insert(symbol.clone()) {
                    doc.symbols.push(types::SymbolInformation {
                        symbol: symbol.clone(),
                        documentation: build_symbol_documentation(db, item),
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

            let target = Target::Scope(ScopeId::from_item(item));
            for ref_mod in ingot.all_modules(db) {
                let refs = ref_mod.references_to_target(db, &target);
                if refs.is_empty() {
                    continue;
                }
                let Some(ref_doc_url) = top_mod_url(db, ref_mod).map(|u| u.to_string()) else {
                    continue;
                };
                let Some(ref_doc) = documents.get_mut(&ref_doc_url) else {
                    continue;
                };

                for reference in refs {
                    if let Some(resolved) = reference.span().resolve(db)
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
