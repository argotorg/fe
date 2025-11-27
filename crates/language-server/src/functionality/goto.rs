use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    analysis::{
        name_resolution::{PathResErrorKind, resolve_path},
        ty::trait_resolution::PredicateListId,
    },
    core::semantic::{CursorTarget, find_at_cursor},
    hir_def::{PathId, TopLevelMod, scope_graph::ScopeId},
    lower::map_file_to_mod,
    span::LazySpan,
    visitor::{Visitor, VisitorCtxt, prelude::LazyPathSpan},
};
use tracing::error;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_scope, to_offset_from_position},
};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

// Path collector using visitor - needed for type annotations which aren't in Reference yet
#[derive(Default)]
struct PathSpanCollector<'db> {
    paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else {
            return;
        };
        let scope = ctxt.scope();
        self.paths.push((path, scope, span));
    }
}

fn find_path_surrounding_cursor<'db>(
    db: &'db DriverDataBase,
    cursor: Cursor,
    full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
) -> Option<(PathId<'db>, ScopeId<'db>)> {
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
                return Some((seg_path, scope));
            }
        }
    }

    None
}

/// Gets goto-definition target scopes for a cursor position.
pub fn get_goto_target_scopes_for_cursor<'db>(
    db: &'db DriverDataBase,
    file: common::file::File,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    // First try the semantic layer's find_at_cursor for expression references
    if let Some(target) = find_at_cursor(db, file, cursor) {
        let scopes = match target {
            CursorTarget::Definition(scope) => vec![scope],
            CursorTarget::Reference(reference) => reference.resolve_to_scopes(db),
        };
        if !scopes.is_empty() {
            return Some(scopes);
        }
    }

    // Fall back to visitor-based path resolution for type annotations
    let top_mod = map_file_to_mod(db, file);
    let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    let mut path_collector = PathSpanCollector::default();
    path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

    let (path, scope) = find_path_surrounding_cursor(db, cursor, path_collector.paths)?;

    let resolved = resolve_path(db, path, scope, PredicateListId::empty_list(db), false);
    let scopes = match resolved {
        Ok(r) => r.as_scope(db).into_iter().collect::<Vec<_>>(),
        Err(err) => match err.kind {
            PathResErrorKind::NotFound { parent: _, bucket } => {
                bucket.iter_ok().flat_map(|r| r.scope()).collect()
            }
            PathResErrorKind::Ambiguous(vec) => vec.into_iter().flat_map(|r| r.scope()).collect(),
            _ => vec![],
        },
    };

    if scopes.is_empty() {
        None
    } else {
        Some(scopes)
    }
}

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    let params = params.text_document_position_params;
    let file_text = std::fs::read_to_string(params.text_document.uri.path()).ok();
    let cursor: Cursor = to_offset_from_position(params.position, file_text.unwrap().as_str());

    let file_path_str = params.text_document.uri.path();
    let url = url::Url::from_file_path(file_path_str).map_err(|()| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            format!("Invalid file path: {file_path_str}"),
        )
    })?;
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in index: {url} (original path: {file_path_str})"),
            )
        })?;

    let scopes = get_goto_target_scopes_for_cursor(&backend.db, file, cursor).unwrap_or_default();

    let locations = scopes
        .iter()
        .map(|scope| to_lsp_location_from_scope(&backend.db, *scope))
        .collect::<Vec<_>>();

    let result: Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ()> =
        Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locations
                .into_iter()
                .filter_map(std::result::Result::ok)
                .collect(),
        )));
    let response = match result {
        Ok(response) => response,
        Err(e) => {
            error!("Error handling goto definition: {:?}", e);
            None
        }
    };
    Ok(response)
}

#[cfg(test)]
mod tests {
    use common::ingot::IngotKind;
    use dir_test::{Fixture, dir_test};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    use crate::test_utils::load_ingot_from_directory;

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
                if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                    cursors.push(seg_span.range.start());
                }
            }
        }

        cursors.sort();
        cursors.dedup();
        cursors
    }

    fn make_goto_cursors_snapshot(
        db: &DriverDataBase,
        fixture: &Fixture<&str>,
        file: common::file::File,
    ) -> String {
        let top_mod = map_file_to_mod(db, file);
        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);
        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();

        for cursor in &cursors {
            let scopes = get_goto_target_scopes_for_cursor(db, file, *cursor).unwrap_or_default();

            if !scopes.is_empty() {
                cursor_path_map.insert(
                    *cursor,
                    scopes
                        .iter()
                        .flat_map(|x| x.pretty_path(db))
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
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
        load_ingot_from_directory(&mut db, &ingot_base_dir);

        let fe_source_path = fixture.path();
        let file_url = Url::from_file_path(fe_source_path).unwrap();

        let ingot = db.workspace().containing_ingot(&db, file_url).unwrap();
        assert_eq!(ingot.kind(&db), IngotKind::Local);

        {
            let file_url = Url::from_file_path(fe_source_path).unwrap();
            let file = db.workspace().get(&db, &file_url).unwrap();

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, file);
            snap_test!(snapshot, fixture.path());
        }

        let file_url = Url::from_file_path(fixture.path()).unwrap();
        let ingot = db.workspace().containing_ingot(&db, file_url);
        assert_eq!(ingot.unwrap().kind(&db), IngotKind::Local);
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, file);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "smallest_enclosing*.fe"
    )]
    fn test_find_path_surrounding_cursor(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();

        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );

        let top_mod = map_file_to_mod(&db, file);
        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);

        let mut cursor_paths: Vec<(Cursor, String)> = vec![];

        for cursor in &cursors {
            let scopes = get_goto_target_scopes_for_cursor(&db, file, *cursor).unwrap_or_default();
            if !scopes.is_empty() {
                let path = scopes
                    .iter()
                    .flat_map(|s| s.pretty_path(&db))
                    .collect::<Vec<_>>()
                    .join("\n");
                cursor_paths.push((*cursor, path));
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
