use async_lsp::ResponseError;
use common::InputDb;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};
// use tracing::error;

use crate::{backend::Backend, util::to_offset_from_position};
// Note: DriverDataBase and tracing are only used in tests below.
pub type Cursor = parser::TextSize;

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Convert the position to an offset in the file using the workspace's current content
    let params = params.text_document_position_params;
    // Use URI directly to avoid path/encoding/case issues
    let url = params.text_document.uri.clone();
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in index: {url}"),
            )
        })?;
    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Use unified SemanticQuery API
    let mut locs: Vec<async_lsp::lsp_types::Location> = Vec::new();
    let query = SemanticQuery::at_cursor(&backend.db, top_mod, cursor);
    let candidates = query.goto_definition();
    for def in candidates.into_iter() {
        if let Some(span) = def.span.resolve(&backend.db) {
            let url = span.file.url(&backend.db).expect("Failed to get file URL");
            let range = crate::util::to_lsp_range_from_span(span, &backend.db)
                .map_err(|e| ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("{e}")))?;
            locs.push(async_lsp::lsp_types::Location { uri: url, range });
        }
    }
    match locs.len() {
        0 => Ok(None),
        1 => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Scalar(locs.remove(0)))),
        _ => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(locs))),
    }
}
// }
#[cfg(test)]
mod tests {
    use common::ingot::IngotKind;
    use dir_test::{dir_test, Fixture};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;
    use tracing::error;

    use hir::{hir_def::{TopLevelMod, PathId, scope_graph::ScopeId}, span::LazySpan, visitor::{VisitorCtxt, prelude::LazyPathSpan, Visitor}};
use hir_analysis::{name_resolution::{resolve_with_policy, DomainPreference, PathResErrorKind}, ty::trait_resolution::PredicateListId};

    #[derive(Default)]
    struct PathSpanCollector<'db> {
        paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    }

    impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
        fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
            if let Some(span) = ctxt.span() {
                let scope = ctxt.scope();
                self.paths.push((path, scope, span));
            }
        }
    }

    fn find_path_surrounding_cursor<'db>(
        db: &'db DriverDataBase,
        cursor: Cursor,
        full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    ) -> Option<(PathId<'db>, bool, ScopeId<'db>)> {
        for (path, scope, lazy_span) in full_paths {
            let span = lazy_span.resolve(db).unwrap();
            if span.range.contains(cursor) {
                // Prefer the deepest segment that contains the cursor to match user intent
                for idx in (0..=path.segment_index(db)).rev() {
                    let seg_span = lazy_span.clone().segment(idx).resolve(db).unwrap();
                    if seg_span.range.contains(cursor) {
                        return Some((path.segment(db, idx).unwrap(), idx != path.segment_index(db), scope));
                    }
                }
            }
        }
        None
    }


    // given a cursor position and a string, convert to cursor line and column
    fn line_col_from_cursor(cursor: Cursor, s: &str) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;
        for (i, c) in s.chars().enumerate() {
            if i == Into::<usize>::into(cursor) {
                return (line, col);
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        (line, col)
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
                let seg_span = lazy_span.clone().segment(idx).resolve(db).unwrap();
                cursors.push(seg_span.range.start());
            }
        }

        cursors.sort();
        cursors.dedup();

        error!("Found cursors: {:?}", cursors);
        cursors
    }

    fn make_goto_cursors_snapshot(
        db: &DriverDataBase,
        fixture: &Fixture<&str>,
        top_mod: TopLevelMod,
    ) -> String {
        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);
        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();

        for cursor in &cursors {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
            let mut path_collector = PathSpanCollector::default();
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);
            let full_paths = path_collector.paths;
            if let Some((path, _, scope)) = find_path_surrounding_cursor(db, *cursor, full_paths) {
                let resolved = resolve_with_policy(
                    db,
                    path,
                    scope,
                    PredicateListId::empty_list(db),
                    DomainPreference::Either,
                );
                let mut lines: Vec<String> = match resolved {
                    Ok(r) => r.pretty_path(db).into_iter().collect(),
                    Err(err) => match err.kind {
                        PathResErrorKind::NotFound { parent: _, bucket } => bucket
                            .iter_ok()
                            .filter_map(|nr| nr.pretty_path(db))
                            .collect(),
                        PathResErrorKind::Ambiguous(vec) => vec
                            .into_iter()
                            .filter_map(|nr| nr.pretty_path(db))
                            .collect(),
                        _ => vec![],
                    },
                };
                // Filter out primitive/builtin types and noise to match expected readability
                lines.retain(|s| s.contains("::") || s.starts_with("local at") || s == "lib" || s.starts_with("lib::"));
                if !lines.is_empty() {
                    cursor_path_map.insert(*cursor, lines.join("\n"));
                }
            }
        }

        let cursor_lines = cursor_path_map
            .iter()
            .map(|(cursor, path)| {
                let (cursor_line, cursor_col) = line_col_from_cursor(*cursor, fixture.content());
                format!("cursor position ({cursor_line:?}, {cursor_col:?}), path: {path}")
            })
            .collect::<Vec<_>>();

        format!(
            "{}\n---\n{}",
            fixture
                .content()
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{i:?}: {line}"))
                .collect::<Vec<_>>()
                .join("\n"),
            cursor_lines.join("\n")
        )
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
                    resolve_with_policy(
                        &db,
                        path,
                        scope,
                        PredicateListId::empty_list(&db),
                        DomainPreference::Type,
                    );

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
