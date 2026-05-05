use common::InputDb;
use common::stdlib::{HasBuiltinCore, HasBuiltinStd};
use hir::hir_def::HirIngot;

use crate::extract::DocExtractor;
use crate::index::SemanticIndex;

/// Regenerate doc + SCIP JSON from a database snapshot.
///
/// This is the function that replaces `DocRegenerateFn`. The language-server
/// calls it directly — no closure plumbing needed.
///
/// Returns `(doc_json, scip_json)`.
pub fn regenerate(db: &driver::DriverDataBase, semantic_index: SemanticIndex) -> (String, Option<String>) {
    let builtin_core_url = url::Url::parse(common::stdlib::BUILTIN_CORE_BASE_URL).unwrap();
    let builtin_std_url = url::Url::parse(common::stdlib::BUILTIN_STD_BASE_URL).unwrap();

    let ingot_urls: Vec<url::Url> = db
        .dependency_graph()
        .petgraph(db)
        .node_weights()
        .filter(|u| *u != &builtin_core_url && *u != &builtin_std_url)
        .cloned()
        .collect();

    let standalone_files: Vec<url::Url> = db
        .workspace()
        .all_files(db)
        .iter()
        .filter_map(|(url, _file)| {
            if db.workspace().containing_ingot(db, url.clone()).is_none() {
                Some(url)
            } else {
                None
            }
        })
        .collect();

    build_doc_index(db, semantic_index, &ingot_urls, &standalone_files)
}

fn build_doc_index(
    db: &driver::DriverDataBase,
    semantic_index: SemanticIndex,
    ingot_urls: &[url::Url],
    standalone_file_urls: &[url::Url],
) -> (String, Option<String>) {
    let mut index = fe_web::model::DocIndex::new();
    let mut scip_json: Option<String> = None;

    let extractor = DocExtractor::new(db);

    for ingot_url in ingot_urls {
        let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
            continue;
        };
        for top_mod in ingot.all_modules(db) {
            for item in top_mod.children_nested(db) {
                if let Some(doc_item) = extractor.extract_item_for_ingot(item, ingot) {
                    index.items.push(doc_item);
                }
            }
        }
        let root_mod = ingot.root_mod(db);
        index
            .modules
            .extend(extractor.build_module_tree_for_ingot(ingot, root_mod));
        let trait_impl_links = extractor.extract_trait_impl_links(ingot);
        index.link_trait_impls(trait_impl_links);
    }

    for file_url in standalone_file_urls {
        if let Some(file) = db.workspace().get(db, file_url) {
            let top_mod = db.top_mod(file);
            for item in top_mod.children_nested(db) {
                if let Some(doc_item) = extractor.extract_item(item) {
                    index.items.push(doc_item);
                }
            }
            index
                .modules
                .push(extractor.build_standalone_module_tree(top_mod));
        }
    }

    let existing: std::collections::HashSet<_> =
        index.modules.iter().map(|m| m.name.clone()).collect();
    for (label, builtin) in [("core", db.builtin_core()), ("std", db.builtin_std())] {
        if existing.contains(label) {
            continue;
        }
        for top_mod in builtin.all_modules(db) {
            for item in top_mod.children_nested(db) {
                if let Some(doc_item) = extractor.extract_item_for_ingot(item, builtin) {
                    index.items.push(doc_item);
                }
            }
        }
        let root_mod = builtin.root_mod(db);
        index
            .builtin_modules
            .extend(extractor.build_module_tree_for_ingot(builtin, root_mod));
        let trait_impl_links = extractor.extract_trait_impl_links(builtin);
        index.link_trait_impls(trait_impl_links);
    }

    // Generate SCIP from the SemanticIndex (fast path — no HIR walking)
    let prefix = "fe fe ".to_string();
    let view = semantic_index.symbols_for_prefix(db, prefix);
    if !view.is_empty() {
        let result = crate::scip::generate_from_index(
            db,
            semantic_index,
            // Use first ingot name or "unknown"
            "unknown",
            "0.0.0",
            &url::Url::parse("file:///").unwrap(),
        );
        if !result.index.documents.is_empty() {
            scip_json = Some(crate::scip::scip_to_json_data(&result.index, &result.doc_urls));
        }
    }

    let mut value = serde_json::to_value(&index).expect("serialize DocIndex");
    fe_web::static_site::inject_html_bodies(&mut value);
    let json = serde_json::to_string(&value).expect("serialize JSON");

    (json, scip_json)
}
