use common::InputDb;
use common::stdlib::{HasBuiltinCore, HasBuiltinStd};

use crate::extract::DocExtractor;
use crate::scip_batch;
use crate::tracked;

/// Regenerate doc + SCIP JSON from a database snapshot.
///
/// Uses salsa-tracked per-ingot functions for doc extraction and SCIP generation.
/// Builtins never recompute (their inputs never change).
///
/// Returns `(doc_json, scip_json)`.
pub fn regenerate(db: &driver::DriverDataBase) -> (String, Option<String>) {
    let builtin_core_url = url::Url::parse(common::stdlib::BUILTIN_CORE_BASE_URL).unwrap();
    let builtin_std_url = url::Url::parse(common::stdlib::BUILTIN_STD_BASE_URL).unwrap();

    let ingot_urls: Vec<url::Url> = db
        .dependency_graph()
        .petgraph(db)
        .node_weights()
        .filter(|u| *u != &builtin_core_url && *u != &builtin_std_url)
        .cloned()
        .collect();

    let mut index = fe_web::model::DocIndex::new();

    // User ingots — salsa-cached per-ingot
    for ingot_url in &ingot_urls {
        let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
            continue;
        };
        index
            .items
            .extend(tracked::docs_for_ingot(db, ingot).clone());
        index
            .modules
            .extend(tracked::module_tree_for_ingot(db, ingot).clone());
        index.link_trait_impls(tracked::trait_impl_links_for_ingot(db, ingot).clone());
    }

    // Standalone files (not in any ingot)
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

    for file_url in &standalone_files {
        if let Some(file) = db.workspace().get(db, file_url) {
            let extractor = DocExtractor::new(db);
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

    // Builtins — salsa-cached, never recompute after first call
    let existing: std::collections::HashSet<_> =
        index.modules.iter().map(|m| m.name.clone()).collect();
    for (label, builtin) in [("core", db.builtin_core()), ("std", db.builtin_std())] {
        if existing.contains(label) {
            continue;
        }
        index
            .items
            .extend(tracked::docs_for_ingot(db, builtin).clone());
        index
            .builtin_modules
            .extend(tracked::module_tree_for_ingot(db, builtin).clone());
        index.link_trait_impls(tracked::trait_impl_links_for_ingot(db, builtin).clone());
    }

    // Generate SCIP via salsa-cached scip_batch path
    let builtin_urls = [builtin_core_url, builtin_std_url];
    let all_scip_urls: Vec<_> = ingot_urls.iter().chain(builtin_urls.iter()).collect();
    let mut combined_scip = scip::types::Index::default();
    let mut combined_doc_urls = std::collections::HashMap::new();
    let mut any_scip = false;

    for ingot_url in &all_scip_urls {
        if let Ok(mut result) = scip_batch::generate_scip(db, ingot_url)
            && !result.index.documents.is_empty()
        {
            let base_url = if ingot_url.to_file_path().is_ok() {
                None
            } else {
                Some(*ingot_url)
            };
            scip_batch::enrich_signatures_with_base(
                db,
                camino::Utf8Path::new("/"),
                base_url,
                &mut index,
                &mut result.index,
            );
            combined_scip.documents.extend(result.index.documents);
            combined_doc_urls.extend(result.doc_urls);
            any_scip = true;
        }
    }
    let scip_json = if any_scip {
        Some(scip_batch::scip_to_json_data(
            &combined_scip,
            &combined_doc_urls,
        ))
    } else {
        None
    };

    let mut value = serde_json::to_value(&index).expect("serialize DocIndex");
    fe_web::static_site::inject_html_bodies(&mut value);
    let json = serde_json::to_string(&value).expect("serialize JSON");

    (json, scip_json)
}

#[cfg(test)]
mod tests {
    #[test]
    fn regenerate_produces_scip_with_enriched_signatures() {
        let temp = tempfile::tempdir().expect("create temp dir");
        std::fs::write(
            temp.path().join("fe.toml"),
            "[ingot]\nname = \"test_ingot\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src_dir = temp.path().join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::write(
            src_dir.join("lib.fe"),
            "pub struct Foo {\n    pub x: i32\n}\n\npub fn make_foo() -> Foo {\n    Foo { x: 1 }\n}\n",
        )
        .unwrap();

        let mut db = driver::DriverDataBase::default();
        let ingot_url = url::Url::from_directory_path(temp.path()).expect("dir url");
        driver::init_ingot(&mut db, &ingot_url);

        let (doc_json, scip_json) = super::regenerate(&db);

        assert!(!doc_json.is_empty(), "doc JSON should not be empty");
        assert!(
            scip_json.is_some(),
            "SCIP JSON should be generated for an ingot with code"
        );

        let scip = scip_json.unwrap();
        assert!(
            scip.contains("symbols") || scip.contains("occurrences"),
            "SCIP JSON should contain symbol data"
        );
    }
}
