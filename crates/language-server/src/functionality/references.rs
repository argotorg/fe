use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    core::semantic::reference::Target, hir_def::TopLevelMod, lower::map_file_to_mod, span::LazySpan,
};

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

use super::goto::Cursor;

/// Find all references to the symbol at the cursor position.
fn find_references_at_cursor<'db>(
    db: &'db impl hir::SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Vec<async_lsp::lsp_types::Location> {
    // Use the simplified API to get the target at cursor
    let Some(target) = top_mod.target_at(db, cursor) else {
        return vec![];
    };

    let mut locations = vec![];

    match &target {
        Target::Scope(target_scope) => {
            // For scopes, search all modules in the ingot
            let ingot = top_mod.ingot(db);

            for (url, file) in ingot.files(db).iter() {
                if !url.path().ends_with(".fe") {
                    continue;
                }
                let mod_ = map_file_to_mod(db, file);
                for span in mod_.find_references(db, &target) {
                    if span.resolve(db).is_some()
                        && let Ok(location) = to_lsp_location_from_lazy_span(db, span)
                    {
                        locations.push(location);
                    }
                }
            }

            // Include the definition location first
            if let Ok(def_location) = to_lsp_location_from_scope(db, *target_scope) {
                locations.insert(0, def_location);
            }
        }
        Target::Local { span, .. } => {
            // For locals, find_references searches within the function body
            for ref_span in top_mod.find_references(db, &target) {
                if ref_span.resolve(db).is_some()
                    && let Ok(location) = to_lsp_location_from_lazy_span(db, ref_span)
                {
                    locations.push(location);
                }
            }

            // Include the definition location first
            if let Ok(def_location) = to_lsp_location_from_lazy_span(db, span.clone()) {
                locations.insert(0, def_location);
            }
        }
    }

    locations
}

pub async fn handle_references(
    backend: &Backend,
    params: async_lsp::lsp_types::ReferenceParams,
) -> Result<Option<Vec<async_lsp::lsp_types::Location>>, ResponseError> {
    let path_str = params.text_document_position.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor: Cursor =
        to_offset_from_position(params.text_document_position.position, file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);
    let locations = find_references_at_cursor(&backend.db, top_mod, cursor);

    if locations.is_empty() {
        Ok(None)
    } else {
        Ok(Some(locations))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term::{
            self,
            termcolor::{BufferWriter, ColorChoice},
        },
    };
    use common::indexmap::IndexMap;
    use common::ingot::IngotKind;
    use dir_test::{Fixture, dir_test};
    use driver::DriverDataBase;
    use hir::hir_def::{ItemKind, PathId, scope_graph::ScopeId};
    use hir::span::{DynLazySpan, LazySpan};
    use hir::visitor::{Visitor, VisitorCtxt, prelude::LazyPathSpan};
    use parser::TextSize;
    use rustc_hash::FxHashMap;
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

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

    /// Property formatter for annotating references in snapshots.
    struct ReferenceFormatter<'db> {
        properties: IndexMap<TopLevelMod<'db>, Vec<(String, DynLazySpan<'db>)>>,
        top_mod_to_file: FxHashMap<TopLevelMod<'db>, usize>,
        code_span_files: SimpleFiles<String, String>,
    }

    impl<'db> ReferenceFormatter<'db> {
        fn new() -> Self {
            Self {
                properties: Default::default(),
                top_mod_to_file: Default::default(),
                code_span_files: SimpleFiles::new(),
            }
        }

        fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod<'db>) {
            let file_id = self.code_span_files.add(path.to_string(), text.to_string());
            self.top_mod_to_file.insert(top_mod, file_id);
        }

        fn push_prop(&mut self, top_mod: TopLevelMod<'db>, span: DynLazySpan<'db>, prop: String) {
            self.properties
                .entry(top_mod)
                .or_default()
                .push((prop, span));
        }

        fn finish(&mut self, db: &'db dyn hir::SpannedHirDb) -> String {
            let writer = BufferWriter::stderr(ColorChoice::Never);
            let mut buffer = writer.buffer();
            let config = term::Config::default();

            for top_mod in self.top_mod_to_file.keys() {
                if !self.properties.contains_key(top_mod) {
                    continue;
                }

                let diags = self.properties[top_mod]
                    .iter()
                    .map(|(prop, span)| {
                        let resolved_span = span.resolve(db).unwrap();
                        let file_id = self.top_mod_to_file[top_mod];
                        let diag = Diagnostic::note().with_labels(vec![
                            Label::primary(file_id, resolved_span.range).with_message(prop),
                        ]);
                        (
                            (
                                resolved_span.file,
                                (resolved_span.range.start(), resolved_span.range.end()),
                            ),
                            diag,
                        )
                    })
                    .collect::<BTreeMap<_, _>>();

                for diag in diags.values() {
                    term::emit(&mut buffer, &config, &self.code_span_files, diag).unwrap();
                }
            }

            std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
        }
    }

    fn extract_cursor_positions_from_spans(
        db: &DriverDataBase,
        top_mod: TopLevelMod,
    ) -> Vec<TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut path_collector = PathSpanCollector::default();
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let mut cursors = Vec::new();

        // Collect cursors from path references
        for (path, _, lazy_span) in path_collector.paths {
            for idx in 0..=path.segment_index(db) {
                if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                    cursors.push(seg_span.range.start());
                }
            }
        }

        // Also collect cursors from item definition sites
        let scope_graph = top_mod.scope_graph(db);
        for item in scope_graph.items_dfs(db) {
            if let Some(name_span) = item.name_span()
                && let Some(span) = name_span.resolve(db)
            {
                cursors.push(span.range.start());
            }

            // Also collect cursors from function parameter names
            if let ItemKind::Func(func) = item {
                for (idx, _param) in func.params(db).enumerate() {
                    let param_span = func.span().params().param(idx);
                    if let Some(span) = param_span.name().resolve(db) {
                        cursors.push(span.range.start());
                    }
                }
            }
        }

        cursors.sort();
        cursors.dedup();
        cursors
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/hoverable",
        glob: "**/*.fe",
    )]
    fn test_references_multiple_files(fixture: Fixture<&str>) {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir = std::path::Path::new(&cargo_manifest_dir).join("test_files/hoverable");

        let mut db = DriverDataBase::default();
        load_ingot_from_directory(&mut db, &ingot_base_dir);

        let fe_source_path = fixture.path();
        let file_url = Url::from_file_path(fe_source_path).unwrap();

        let ingot = db
            .workspace()
            .containing_ingot(&db, file_url.clone())
            .unwrap();
        assert_eq!(ingot.kind(&db), IngotKind::Local);

        let mut formatter = ReferenceFormatter::new();

        // Register all files in the ingot and collect cursors from all of them
        let mut all_cursors: Vec<(TextSize, TopLevelMod<'_>)> = Vec::new();

        for (url, file) in ingot.files(&db).iter() {
            if !url.path().ends_with(".fe") {
                continue;
            }
            let top_mod = map_file_to_mod(&db, file);
            let text = file.text(&db);
            // Use just the filename for cleaner snapshots
            let path = std::path::Path::new(url.path())
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(url.path());
            formatter.register_top_mod(path, text.as_str(), top_mod);

            // Collect cursors from this file
            let cursors = extract_cursor_positions_from_spans(&db, top_mod);
            for cursor in cursors {
                all_cursors.push((cursor, top_mod));
            }
        }

        // For each cursor from any file, find all references and annotate them
        for (cursor, cursor_top_mod) in &all_cursors {
            let locations = find_references_at_cursor(&db, *cursor_top_mod, *cursor);
            if locations.is_empty() {
                continue;
            }

            // Group references by target symbol to show def + refs together
            let total_refs = locations.len();

            // Annotate each location (including definition which is first)
            for (idx, loc) in locations.iter().enumerate() {
                let ref_url = Url::parse(loc.uri.as_str()).unwrap();
                if let Some(ref_file) = db.workspace().get(&db, &ref_url) {
                    let ref_top_mod = map_file_to_mod(&db, ref_file);
                    let ref_text = ref_file.text(&db);
                    let ref_offset = lsp_position_to_offset(&loc.range.start, ref_text.as_str());

                    // For the definition (first location), annotate with the target's span
                    // For other locations, use reference_at
                    if idx == 0 {
                        // First location is the definition - use target_at to handle both
                        // item definitions and local/param bindings
                        if let Some(target) = ref_top_mod.target_at(&db, ref_offset) {
                            let annotation = format!(
                                "def: defined here @ {}:{} ({} refs)",
                                loc.range.start.line + 1,
                                loc.range.start.character,
                                total_refs
                            );
                            match target {
                                Target::Scope(scope) => {
                                    if let Some(name_span) = scope.item().name_span() {
                                        formatter.push_prop(ref_top_mod, name_span, annotation);
                                    }
                                }
                                Target::Local { span, .. } => {
                                    formatter.push_prop(ref_top_mod, span, annotation);
                                }
                            }
                        }
                    } else {
                        // Regular reference
                        if let Some(reference) = ref_top_mod.reference_at(&db, ref_offset) {
                            let annotation = format!(
                                "ref: {}:{}",
                                loc.range.start.line + 1,
                                loc.range.start.character
                            );
                            formatter.push_prop(ref_top_mod, reference.span(), annotation);
                        }
                    }
                }
            }
        }

        let snapshot = formatter.finish(&db);
        snap_test!(snapshot, fixture.path());
    }

    fn lsp_position_to_offset(position: &async_lsp::lsp_types::Position, text: &str) -> TextSize {
        let mut line = 0;
        let mut col = 0;
        for (offset, ch) in text.char_indices() {
            if line == position.line && col == position.character {
                return TextSize::from(offset as u32);
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        TextSize::from(text.len() as u32)
    }
}
