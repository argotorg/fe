use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    core::semantic::reference::{ReferenceView, Target, TargetResolution},
    hir_def::{ItemKind, TopLevelMod},
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::warn;

use crate::{
    backend::Backend,
    util::{
        to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_lsp_range_from_span,
        to_offset_from_position,
    },
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
    let params = params.text_document_position_params;
    warn!(
        "goto_definition: pos=({},{}), uri={}",
        params.position.line, params.position.character, params.text_document.uri
    );

    let internal_url = backend.map_client_uri_to_internal(params.text_document.uri.clone());
    let Some(file) = backend.db.workspace().get(&backend.db, &internal_url) else {
        warn!("goto_definition: file not found for uri={}", params.text_document.uri);
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = goto_target_at_cursor(&backend.db, top_mod, cursor);

    warn!(
        "goto_definition: cursor={:?}, targets={}, ref_found={}",
        cursor,
        resolution.as_slice().len(),
        top_mod.reference_at(&backend.db, cursor).is_some(),
    );

    // Compute origin_selection_range: the span of the identifier being clicked.
    // For paths like `ops::returndatasize`, this is the specific segment at the cursor.
    // This range is critical for Zed's hover link caching — without it, every pixel
    // of mouse movement fires a new definition request, causing a request storm.
    let origin_range = top_mod
        .reference_at(&backend.db, cursor)
        .and_then(|r| match r {
            ReferenceView::Path(pv) => {
                for idx in 0..=pv.path.segment_index(&backend.db) {
                    // Use full segment span to check cursor containment (includes generics)
                    let Some(seg_resolved) = pv.span.clone().segment(idx).resolve(&backend.db) else {
                        continue;
                    };
                    if seg_resolved.range.contains(cursor) {
                        // But return just the ident span (excludes generics like <C::InitArgs>)
                        // so Zed doesn't cache/underline too wide a region
                        if let Some(ident_resolved) = pv.span.clone().segment(idx).ident().resolve(&backend.db) {
                            return to_lsp_range_from_span(ident_resolved, &backend.db).ok();
                        }
                        return to_lsp_range_from_span(seg_resolved, &backend.db).ok();
                    }
                }
                None
            }
            _ => r
                .span()
                .resolve(&backend.db)
                .and_then(|s| to_lsp_range_from_span(s, &backend.db).ok()),
        })
        .or_else(|| {
            // Fallback: compute word boundaries at cursor from the source text.
            // Without an origin_selection_range, Zed fires a new request on every
            // mouse pixel movement, creating a request storm that prevents goto
            // from ever completing.
            let text = file_text.as_str();
            let offset = u32::from(cursor) as usize;
            if offset >= text.len() {
                return None;
            }
            let bytes = text.as_bytes();
            if !bytes[offset].is_ascii_alphanumeric() && bytes[offset] != b'_' {
                return None;
            }
            let start = text[..offset]
                .rfind(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                .map(|i| i + 1)
                .unwrap_or(0);
            let end = text[offset..]
                .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                .map(|i| i + offset)
                .unwrap_or(text.len());
            let line_offsets = crate::util::calculate_line_offsets(text);
            let to_pos = |off: usize| -> async_lsp::lsp_types::Position {
                let line = line_offsets.partition_point(|&o| o <= off).saturating_sub(1);
                let col = off - line_offsets[line];
                async_lsp::lsp_types::Position::new(line as u32, col as u32)
            };
            Some(async_lsp::lsp_types::Range::new(to_pos(start), to_pos(end)))
        });

    // Convert targets to LSP LocationLinks
    // Special case: if this is a method in an impl trait block, navigate to the trait method
    let links: Vec<_> = resolution
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
        .map(|mut location| {
            location.uri = backend.map_internal_uri_to_client(location.uri);
            async_lsp::lsp_types::LocationLink {
                origin_selection_range: origin_range,
                target_uri: location.uri,
                target_range: location.range,
                target_selection_range: location.range,
            }
        })
        .collect();

    if links.is_empty() {
        warn!("goto_definition: no links found at cursor {:?}", cursor);
        return Ok(None);
    }

    warn!(
        "goto_definition: returning {} links, origin_range={:?}",
        links.len(),
        origin_range,
    );

    Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Link(
        links,
    )))
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

    /// Annotation for a single goto target.
    struct GotoAnnotation {
        /// The ident-only span (what Zed caches as origin_selection_range).
        ident_range: parser::TextRange,
        /// The full segment span (may include generics like `<C::InitArgs>`).
        segment_range: parser::TextRange,
        /// The goto target label.
        label: String,
    }

    /// Collect all path segment spans with their goto targets.
    fn collect_goto_annotations<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<GotoAnnotation> {
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

                    // Use ident span if available (excludes generics)
                    let ident_range = lazy_span.clone().segment(idx).ident().resolve(db)
                        .map(|s| s.range)
                        .unwrap_or(seg_span.range);

                    annotations.push(GotoAnnotation {
                        ident_range,
                        segment_range: seg_span.range,
                        label: format!("-> {}", label),
                    });
                }
            }
        }

        // Sort by span start position for consistent output
        annotations.sort_by_key(|a| a.ident_range.start());
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
        let filename = std::path::Path::new(fixture.path())
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| fixture.path().to_string());
        let file_id = files.add(filename, fixture.content().to_string());

        // Create diagnostics for each annotation.
        // Primary label: ident span (= what Zed caches as origin_selection_range).
        // Secondary label: full segment span if it differs (includes generics).
        let diags: BTreeMap<_, _> = annotations
            .into_iter()
            .map(|ann| {
                let mut labels = vec![
                    Label::primary(file_id, ann.ident_range).with_message(&ann.label),
                ];

                // When full segment span is wider than ident (e.g. includes <C::InitArgs>),
                // show it so we can verify the handler won't over-cache.
                if ann.segment_range != ann.ident_range {
                    labels.push(
                        Label::secondary(file_id, ann.segment_range)
                            .with_message("full segment (includes generics)"),
                    );
                }

                let diag = Diagnostic::note().with_labels(labels);
                ((ann.ident_range.start(), ann.ident_range.end()), diag)
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

    /// Diagnostic test: traces the full semantic API chain for `C::static_method()`
    /// where C is a type param bound to a trait.
    #[test]
    fn test_goto_generic_static_method_trace() {
        let mut db = DriverDataBase::default();
        let code = r#"trait Contract {
    fn init_code_offset() -> i32
    fn init_code_len() -> i32
}

fn create<C: Contract>() -> i32 {
    let off = C::init_code_offset()
    off
}
"#;
        let url = Url::parse("file:///test_trace.fe").unwrap();
        let file = db.workspace().touch(&mut db, url, Some(code.to_string()));
        let top_mod = map_file_to_mod(&db, file);

        // cursor on "init_code_offset" in "C::init_code_offset()"
        // Line 6 (0-indexed): "    let off = C::init_code_offset()"
        // "C::init_code_offset" starts at col 14
        // "init_code_offset" starts at col 17
        let lines: Vec<&str> = code.lines().collect();
        let mut offset = 0u32;
        for (i, line) in lines.iter().enumerate() {
            if i == 6 {
                break;
            }
            offset += line.len() as u32 + 1; // +1 for newline
        }
        let cursor_on_init = parser::TextSize::from(offset + 17); // "init_code_offset"

        // Step 1: Does find_enclosing_items find the function?
        let items = top_mod.find_enclosing_items(&db, cursor_on_init);
        assert!(!items.is_empty(), "find_enclosing_items should find items at cursor");
        let item_kinds: Vec<_> = items.iter().map(|i| format!("{:?}", i)).collect();
        eprintln!("Step 1 - enclosing items: {:?}", item_kinds);

        // Step 2: Does reference_at find a reference?
        let reference = top_mod.reference_at(&db, cursor_on_init);
        assert!(reference.is_some(), "reference_at should find a reference at cursor on init_code_offset");
        let ref_view = reference.unwrap();
        eprintln!("Step 2 - reference kind: {:?}", std::mem::discriminant(ref_view));

        // Step 3: Does target_at resolve?
        let resolution = top_mod.target_at(&db, cursor_on_init);
        eprintln!("Step 3 - resolution: {:?}", resolution);
        assert!(
            resolution.first().is_some(),
            "target_at should resolve C::init_code_offset to a target"
        );
    }
}
