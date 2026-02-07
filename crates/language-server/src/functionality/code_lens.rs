use async_lsp::ResponseError;
use async_lsp::lsp_types::{CodeLens, CodeLensParams, Command};
use common::InputDb;
use hir::{core::semantic::reference::Target, hir_def::ItemKind, lower::map_file_to_mod};

use crate::{backend::Backend, util::to_lsp_location_from_scope};

/// Handle textDocument/codeLens.
pub async fn handle_code_lens(
    backend: &Backend,
    params: CodeLensParams,
) -> Result<Option<Vec<CodeLens>>, ResponseError> {
    let path_str = params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let top_mod = map_file_to_mod(&backend.db, file);
    let scope_graph = top_mod.scope_graph(&backend.db);
    let ingot = top_mod.ingot(&backend.db);

    let mut lenses = Vec::new();

    for item in scope_graph.items_dfs(&backend.db) {
        match item {
            ItemKind::Func(func) => {
                let target = Target::Scope(func.scope());
                let ref_count = count_references(&backend.db, ingot, &target);

                if let Ok(location) = to_lsp_location_from_scope(&backend.db, func.scope()) {
                    let title = if ref_count == 1 {
                        "1 reference".to_string()
                    } else {
                        format!("{ref_count} references")
                    };

                    lenses.push(CodeLens {
                        range: location.range,
                        command: Some(Command {
                            title,
                            command: "fe.showReferences".to_string(),
                            arguments: None,
                        }),
                        data: None,
                    });
                }
            }
            ItemKind::Trait(trait_) => {
                let impl_count = trait_.all_impl_traits(&backend.db).len();

                if let Ok(location) = to_lsp_location_from_scope(&backend.db, trait_.scope()) {
                    let title = if impl_count == 1 {
                        "1 implementation".to_string()
                    } else {
                        format!("{impl_count} implementations")
                    };

                    lenses.push(CodeLens {
                        range: location.range,
                        command: Some(Command {
                            title,
                            command: "fe.showImplementations".to_string(),
                            arguments: None,
                        }),
                        data: None,
                    });
                }
            }
            ItemKind::Struct(s) => {
                let target = Target::Scope(s.scope());
                let ref_count = count_references(&backend.db, ingot, &target);

                if let Ok(location) = to_lsp_location_from_scope(&backend.db, s.scope()) {
                    let title = if ref_count == 1 {
                        "1 reference".to_string()
                    } else {
                        format!("{ref_count} references")
                    };

                    lenses.push(CodeLens {
                        range: location.range,
                        command: Some(Command {
                            title,
                            command: "fe.showReferences".to_string(),
                            arguments: None,
                        }),
                        data: None,
                    });
                }
            }
            ItemKind::Enum(e) => {
                let target = Target::Scope(e.scope());
                let ref_count = count_references(&backend.db, ingot, &target);

                if let Ok(location) = to_lsp_location_from_scope(&backend.db, e.scope()) {
                    let title = if ref_count == 1 {
                        "1 reference".to_string()
                    } else {
                        format!("{ref_count} references")
                    };

                    lenses.push(CodeLens {
                        range: location.range,
                        command: Some(Command {
                            title,
                            command: "fe.showReferences".to_string(),
                            arguments: None,
                        }),
                        data: None,
                    });
                }
            }
            _ => {}
        }
    }

    // Add codegen view lenses (once per file, at line 0)
    let codegen_range = async_lsp::lsp_types::Range {
        start: async_lsp::lsp_types::Position {
            line: 0,
            character: 0,
        },
        end: async_lsp::lsp_types::Position {
            line: 0,
            character: 0,
        },
    };
    let uri_string = params.text_document.uri.to_string();
    for (label, cmd) in [
        ("MIR", "fe.viewMir"),
        ("Yul", "fe.viewYul"),
        ("Sonatina IR", "fe.viewSonatinaIr"),
    ] {
        lenses.push(CodeLens {
            range: codegen_range,
            command: Some(Command {
                title: label.to_string(),
                command: cmd.to_string(),
                arguments: Some(vec![serde_json::json!(uri_string)]),
            }),
            data: None,
        });
    }

    if lenses.is_empty() {
        Ok(None)
    } else {
        Ok(Some(lenses))
    }
}

fn count_references<'db>(
    db: &'db driver::DriverDataBase,
    ingot: common::ingot::Ingot<'db>,
    target: &Target<'db>,
) -> usize {
    let mut count = 0usize;
    for (url, file) in ingot.files(db).iter() {
        if !url.path().ends_with(".fe") {
            continue;
        }
        let mod_ = map_file_to_mod(db, file);
        count += mod_.references_to_target(db, target).len();
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    fn collect_lens_data<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
    ) -> Vec<(String, String, usize)> {
        let scope_graph = top_mod.scope_graph(db);
        let ingot = top_mod.ingot(db);
        let mut results = Vec::new();

        for item in scope_graph.items_dfs(db) {
            let (name, kind, count) = match item {
                ItemKind::Func(func) => {
                    let target = Target::Scope(func.scope());
                    let count = count_references(db, ingot, &target);
                    let name = func
                        .name(db)
                        .to_opt()
                        .map(|n| n.data(db).to_string())
                        .unwrap_or_default();
                    (name, "func".to_string(), count)
                }
                ItemKind::Trait(trait_) => {
                    let count = trait_.all_impl_traits(db).len();
                    let name = trait_
                        .name(db)
                        .to_opt()
                        .map(|n| n.data(db).to_string())
                        .unwrap_or_default();
                    (name, "trait".to_string(), count)
                }
                ItemKind::Struct(s) => {
                    let target = Target::Scope(s.scope());
                    let count = count_references(db, ingot, &target);
                    let name = s
                        .name(db)
                        .to_opt()
                        .map(|n| n.data(db).to_string())
                        .unwrap_or_default();
                    (name, "struct".to_string(), count)
                }
                ItemKind::Enum(e) => {
                    let target = Target::Scope(e.scope());
                    let count = count_references(db, ingot, &target);
                    let name = e
                        .name(db)
                        .to_opt()
                        .map(|n| n.data(db).to_string())
                        .unwrap_or_default();
                    (name, "enum".to_string(), count)
                }
                _ => continue,
            };
            results.push((name, kind, count));
        }

        results
    }

    #[test]
    fn test_code_lens_references() {
        let mut db = DriverDataBase::default();
        let code = r#"struct Point {
    x: i32
    y: i32
}

fn make_point() -> Point {
    Point { x: 1, y: 2 }
}

fn use_point() -> i32 {
    let p: Point = make_point()
    p.x
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let data = collect_lens_data(&db, top_mod);

        // Point struct should have references (used in make_point and use_point)
        let point_data = data.iter().find(|(n, _, _)| n == "Point");
        assert!(point_data.is_some(), "should find Point in lens data");
        let (_, kind, count) = point_data.unwrap();
        assert_eq!(kind, "struct");
        assert!(
            *count >= 2,
            "Point should have at least 2 references, got {count}"
        );

        // make_point should have a reference (called in use_point)
        let make_point = data.iter().find(|(n, _, _)| n == "make_point");
        assert!(make_point.is_some(), "should find make_point");
        let (_, kind, count) = make_point.unwrap();
        assert_eq!(kind, "func");
        assert!(
            *count >= 1,
            "make_point should have at least 1 reference, got {count}"
        );
    }

    #[test]
    fn test_code_lens_trait_implementations() {
        let mut db = DriverDataBase::default();
        let code = r#"trait Runnable {
    fn run(self) -> i32
}

struct Task {
    id: i32
}

impl Runnable for Task {
    fn run(self) -> i32 {
        self.id
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let data = collect_lens_data(&db, top_mod);

        let runnable = data.iter().find(|(n, _, _)| n == "Runnable");
        assert!(runnable.is_some(), "should find Runnable");
        let (_, kind, count) = runnable.unwrap();
        assert_eq!(kind, "trait");
        assert_eq!(*count, 1, "Runnable should have 1 implementation");
    }

    #[test]
    fn test_code_lens_no_references() {
        let mut db = DriverDataBase::default();
        let code = "fn unused() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let data = collect_lens_data(&db, top_mod);

        let unused = data.iter().find(|(n, _, _)| n == "unused");
        assert!(unused.is_some());
        let (_, _, count) = unused.unwrap();
        assert_eq!(*count, 0, "unused function should have 0 references");
    }
}
