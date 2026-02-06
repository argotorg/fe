use async_lsp::ResponseError;
use async_lsp::lsp_types::{CodeLens, CodeLensParams, Command};
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::ItemKind,
    lower::map_file_to_mod,
};

use crate::{
    backend::Backend,
    util::to_lsp_location_from_scope,
};

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
