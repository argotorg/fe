use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    core::semantic::reference::{Target, TargetResolution},
    hir_def::{ItemKind, TopLevelMod},
    lower::map_file_to_mod,
};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

/// Get goto target resolution for the cursor position.
///
/// Uses the unified target_at which handles references, definitions,
/// and bindings, preserving ambiguity information.
pub fn goto_target_at_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> TargetResolution<'db> {
    top_mod.target_at(db, cursor)
}

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    use tracing::{info, warn};

    // Convert the position to an offset in the file
    let params = params.text_document_position_params;
    info!(
        "goto_definition request for {:?} at {:?}",
        params.text_document.uri, params.position
    );

    let file_text = std::fs::read_to_string(params.text_document.uri.path()).ok();
    let cursor: Cursor = to_offset_from_position(params.position, file_text.unwrap().as_str());
    info!("cursor offset: {:?}", cursor);

    // Get the module and the goto info
    let file_path_str = params.text_document.uri.path();
    let url = url::Url::from_file_path(file_path_str).map_err(|()| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            format!("Invalid file path: {file_path_str}"),
        )
    })?;
    info!("looking up file: {}", url);

    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            warn!("File not found in workspace: {}", url);
            // List all files in workspace for debugging
            let all_files: Vec<_> = backend
                .db
                .workspace()
                .all_files(&backend.db)
                .iter()
                .map(|(u, _)| u.to_string())
                .collect();
            warn!(
                "Workspace contains {} files: {:?}",
                all_files.len(),
                all_files
            );
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in index: {url} (original path: {file_path_str})"),
            )
        })?;
    info!("found file in workspace");
    let top_mod = map_file_to_mod(&backend.db, file);

    // Get target resolution (may be ambiguous)
    let resolution = goto_target_at_cursor(&backend.db, top_mod, cursor);
    info!("goto resolution: {:?}", resolution);

    // Convert targets to LSP locations
    // Special case: if this is a method in an impl trait block, navigate to the trait method
    let locations: Vec<_> = resolution
        .as_slice()
        .iter()
        .filter_map(|target| match target {
            Target::Scope(scope) => {
                // If this is a method inside an impl trait block, go to the trait method definition
                if let ItemKind::Func(func) = scope.item()
                    && let Some(trait_method) = func.trait_method_def(&backend.db)
                {
                    return to_lsp_location_from_scope(&backend.db, trait_method.scope()).ok();
                }
                to_lsp_location_from_scope(&backend.db, *scope).ok()
            }
            Target::Local { span, .. } => {
                to_lsp_location_from_lazy_span(&backend.db, span.clone()).ok()
            }
        })
        .collect();

    match locations.len() {
        0 => Ok(None),
        1 => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Scalar(
            locations.into_iter().next().unwrap(),
        ))),
        _ => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locations,
        ))),
    }
}
// }
#[cfg(test)]
mod tests {
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term::{
            self,
            termcolor::{BufferWriter, ColorChoice},
        },
    };
    use common::ingot::IngotKind;
    use dir_test::{Fixture, dir_test};
    use hir::{
        analysis::{
            name_resolution::{PathResErrorKind, resolve_path},
            ty::trait_resolution::PredicateListId,
        },
        hir_def::{PathId, scope_graph::ScopeId},
        span::LazySpan,
        visitor::{Visitor, VisitorCtxt, prelude::LazyPathSpan},
    };
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;

    /// Test infrastructure: collects all paths for cursor testing.
    #[derive(Default)]
    struct PathSpanCollector<'db> {
        paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    }

    impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
        fn visit_path(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>,
            path: PathId<'db>,
        ) {
            let Some(span) = ctxt.span() else {
                return;
            };

            let scope = ctxt.scope();
            self.paths.push((path, scope, span));
        }
    }

    /// Test infrastructure: finds path surrounding cursor.
    fn find_path_surrounding_cursor<'db>(
        db: &'db DriverDataBase,
        cursor: Cursor,
        full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    ) -> Option<(PathId<'db>, bool, ScopeId<'db>)> {
        for (path, scope, lazy_span) in full_paths {
            let Some(span) = lazy_span.resolve(db) else {
                continue;
            };

            if !span.range.contains(cursor) {
                continue;
            }

            let last_idx = path.segment_index(db);
            for idx in 0..=last_idx {
                let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) else {
                    continue;
                };

                if seg_span.range.contains(cursor)
                    && let Some(seg_path) = path.segment(db, idx)
                {
                    return Some((seg_path, idx != last_idx, scope));
                }
            }
        }

        None
    }

    fn extract_multiple_cursor_positions_from_spans(
        db: &DriverDataBase,
        top_mod: TopLevelMod,
    ) -> Vec<parser::TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut path_collector = PathSpanCollector::default();
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let mut cursors = Vec::new();
        for (path, _, lazy_span) in path_collector.paths {
            for idx in 0..=path.segment_index(db) {
                if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                    cursors.push(seg_span.range.start());
                }
            }
        }

        cursors.sort();
        cursors.dedup();
        cursors
    }

    /// Collect all path segment spans with their goto targets.
    fn collect_goto_annotations<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<(parser::TextRange, String)> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut path_collector = PathSpanCollector::default();
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let mut annotations = Vec::new();

        for (path, _, lazy_span) in path_collector.paths {
            // For each segment of the path
            for idx in 0..=path.segment_index(db) {
                let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) else {
                    continue;
                };

                // Get cursor at start of segment and resolve target
                let cursor = seg_span.range.start();
                let resolution = goto_target_at_cursor(db, top_mod, cursor);

                if let Some(target) = resolution.first() {
                    let label = match target {
                        Target::Scope(scope) => {
                            scope.pretty_path(db).unwrap_or_else(|| "?".to_string())
                        }
                        Target::Local { ty, .. } => {
                            format!("local: {}", ty.pretty_print(db))
                        }
                    };
                    annotations.push((seg_span.range, format!("-> {}", label)));
                }
            }
        }

        // Sort by span start position for consistent output
        annotations.sort_by_key(|(range, _)| range.start());
        annotations
    }

    fn make_goto_cursors_snapshot(
        db: &DriverDataBase,
        fixture: &Fixture<&str>,
        top_mod: TopLevelMod,
    ) -> String {
        let annotations = collect_goto_annotations(db, top_mod);

        // Set up codespan files
        let mut files = SimpleFiles::new();
        // Use just the filename for cleaner output
        let filename = std::path::Path::new(fixture.path())
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| fixture.path().to_string());
        let file_id = files.add(filename, fixture.content().to_string());

        // Create diagnostics for each annotation
        let diags: BTreeMap<_, _> = annotations
            .into_iter()
            .map(|(range, label)| {
                let diag = Diagnostic::note()
                    .with_labels(vec![Label::primary(file_id, range).with_message(&label)]);
                ((range.start(), range.end()), diag)
            })
            .collect();

        // Render with codespan
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in diags.values() {
            term::emit(&mut buffer, &config, &files, diag).unwrap();
        }

        std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/single_ingot",
        glob: "**/lib.fe",
    )]
    fn test_goto_multiple_files(fixture: Fixture<&str>) {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir =
            std::path::Path::new(&cargo_manifest_dir).join("test_files/single_ingot");

        let mut db = DriverDataBase::default();

        // Load all files from the ingot directory
        load_ingot_from_directory(&mut db, &ingot_base_dir);

        // Get our specific test file
        let fe_source_path = fixture.path();
        let file_url = Url::from_file_path(fe_source_path).unwrap();

        // Get the containing ingot - should be Local now
        let ingot = db.workspace().containing_ingot(&db, file_url).unwrap();
        assert_eq!(ingot.kind(&db), IngotKind::Local);

        // Introduce a new scope to limit the lifetime of `top_mod`
        {
            // Get the file directly from the file index
            let file_url = Url::from_file_path(fe_source_path).unwrap();
            let file = db.workspace().get(&db, &file_url).unwrap();
            let top_mod = map_file_to_mod(&db, file);

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
            snap_test!(snapshot, fixture.path());
        }

        // Get the containing ingot for the file path
        let file_url = Url::from_file_path(fixture.path()).unwrap();
        let ingot = db.workspace().containing_ingot(&db, file_url);
        assert_eq!(ingot.unwrap().kind(&db), IngotKind::Local);
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "smallest_enclosing*.fe"
    )]
    fn test_find_path_surrounding_cursor(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut

        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);

        let mut cursor_paths: Vec<(Cursor, String)> = vec![];

        for cursor in &cursors {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
            let mut path_collector = PathSpanCollector::default();
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let full_paths = path_collector.paths;

            if let Some((path, _, scope)) = find_path_surrounding_cursor(&db, *cursor, full_paths) {
                let resolved_enclosing_path =
                    resolve_path(&db, path, scope, PredicateListId::empty_list(&db), false);

                let res = match resolved_enclosing_path {
                    Ok(res) => res.pretty_path(&db).unwrap(),
                    Err(err) => match err.kind {
                        PathResErrorKind::Ambiguous(vec) => vec
                            .iter()
                            .map(|r| r.pretty_path(&db).unwrap())
                            .collect::<Vec<_>>()
                            .join("\n"),
                        _ => "".into(),
                    },
                };
                cursor_paths.push((*cursor, res));
            }
        }

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_paths
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {cursor:?}, path: {path}") })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }
}
