use async_lsp::ResponseError;
use async_lsp::lsp_types::{Location, ReferenceParams};
use common::InputDb;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};

use crate::{backend::Backend, util::to_offset_from_position};

pub async fn handle_references(
    backend: &Backend,
    params: ReferenceParams,
    
) -> Result<Option<Vec<Location>>, ResponseError> {
    // Locate file and module and convert position to offset using workspace content
    // Use the URI directly to avoid path/encoding issues
    let url = params.text_document_position.text_document.uri.clone();
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("File not found: {url}")))?;
    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.text_document_position.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Use unified SemanticQuery API
    let query = SemanticQuery::at_cursor(&backend.db, top_mod, cursor);
    let mut found = query.find_references()
        .into_iter()
        .filter_map(|r| r.span.resolve(&backend.db))
        .filter_map(|sp| crate::util::to_lsp_range_from_span(sp.clone(), &backend.db).ok().map(|range| (sp, range)))
        .map(|(sp, range)| Location { uri: sp.file.url(&backend.db).expect("url"), range })
        .collect::<Vec<_>>();

    // TODO: Honor includeDeclaration: if false, remove the def location when present
    // This would require exposing definition lookup on SemanticQuery
    // Deduplicate identical locations
    found.sort_by_key(|l| (l.uri.clone(), l.range.start, l.range.end));
    found.dedup_by(|a, b| a.uri == b.uri && a.range == b.range);

    Ok(Some(found))
}

#[cfg(test)]
mod tests {
    use super::*;
    // use common::ingot::IngotKind;
    use url::Url;
    use driver::DriverDataBase;

    #[test]
    fn basic_references_in_hoverable() {
        // Load the hoverable ingot and open lib.fe
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir = std::path::Path::new(&cargo_manifest_dir).join("test_files/hoverable");
        let mut db = DriverDataBase::default();
        crate::test_utils::load_ingot_from_directory(&mut db, &ingot_base_dir);

        let lib_path = ingot_base_dir.join("src/lib.fe");
        let file = db.workspace().touch(&mut db, Url::from_file_path(&lib_path).unwrap(), None);
        let top_mod = map_file_to_mod(&db, file);

        // Cursor on return_three() call inside return_seven()
        let content = std::fs::read_to_string(&lib_path).unwrap();
        let call_off = content.find("return_three()").unwrap() as u32;
        let cursor = parser::TextSize::from(call_off);

        let refs = SemanticQuery::at_cursor(&db, top_mod, cursor).find_references();
        assert!(!refs.is_empty(), "expected at least one reference at call site");
        // Ensure we can convert at least one to an LSP location
        let any_loc = refs
            .into_iter()
            .filter_map(|r| r.span.resolve(&db))
            .filter_map(|sp| crate::util::to_lsp_range_from_span(sp.clone(), &db).ok().map(|range| (sp, range)))
            .map(|(sp, range)| Location { uri: sp.file.url(&db).expect("url"), range })
            .next();
        assert!(any_loc.is_some());
    }
}
