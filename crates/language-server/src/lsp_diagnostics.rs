use async_lsp::lsp_types::{Diagnostic, Url};
use camino::Utf8Path;
use codespan_reporting::files as cs_files;
use common::{diagnostics::CompleteDiagnostic, file::File};
use driver::DriverDataBase;
use hir::Ingot;
use hir_analysis::analysis_pass::{AnalysisPassManager, ParsingPass};
use hir_analysis::name_resolution::ImportAnalysisPass;
use hir_analysis::ty::{
    AdtDefAnalysisPass, BodyAnalysisPass, DefConflictAnalysisPass, FuncAnalysisPass,
    ImplAnalysisPass, ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
};
use rustc_hash::FxHashMap;

use crate::util::diag_to_lsp;

/// Wrapper type to implement codespan Files trait
#[allow(dead_code)]
pub struct LspDb<'a>(pub &'a DriverDataBase);

/// Extension trait for LSP-specific functionality on DriverDataBase
pub trait LspDiagnostics {
    fn diagnostics_for_ingot(&self, ingot: Ingot) -> FxHashMap<Url, Vec<Diagnostic>>;
    #[allow(dead_code)]
    fn file_line_starts(&self, file: File) -> Vec<usize>;
}

impl LspDiagnostics for DriverDataBase {
    fn diagnostics_for_ingot(&self, ingot: Ingot) -> FxHashMap<Url, Vec<Diagnostic>> {
        let mut result = FxHashMap::<Url, Vec<Diagnostic>>::default();
        let mut pass_manager = initialize_analysis_pass();
        let ingot_files = ingot.files(self);

        for (url, file) in ingot_files.iter() {
            // Only analyze source files; skip config and non-source entries
            if file.kind(self) != Some(common::file::IngotFileKind::Source) {
                continue;
            }

            // initialize an empty diagnostic list for this file
            // (to clear any previous diagnostics)
            result.entry(url.clone()).or_default();

            let top_mod = hir::lower::map_file_to_mod(self, file);
            let diagnostics = pass_manager.run_on_module(self, top_mod);
            let mut finalized_diags: Vec<CompleteDiagnostic> = diagnostics
                .iter()
                .map(|d| d.to_complete(self).clone())
                .collect();
            finalized_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
                std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
                ord => ord,
            });
            for diag in finalized_diags {
                let lsp_diags = diag_to_lsp(self, diag).clone();
                for (uri, more_diags) in lsp_diags {
                    let diags = result.entry(uri.clone()).or_insert_with(Vec::new);
                    diags.extend(more_diags);
                }
            }
        }

        result
    }

    fn file_line_starts(&self, file: File) -> Vec<usize> {
        cs_files::line_starts(file.text(self)).collect()
    }
}

impl<'a> cs_files::Files<'a> for LspDb<'a> {
    type FileId = File;
    type Name = &'a Utf8Path;
    type Source = &'a str;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        Ok(file_id
            .path(self.0)
            .as_ref()
            .expect("File path should be valid"))
    }

    fn source(&'a self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self.0))
    }

    fn line_index(
        &'a self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = self.0.file_line_starts(file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, cs_files::Error> {
        let line_starts = self.0.file_line_starts(file_id);

        let start = *line_starts
            .get(line_index)
            .ok_or(cs_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len() - 1,
            })?;

        let end = if line_index == line_starts.len() - 1 {
            file_id.text(self.0).len()
        } else {
            *line_starts
                .get(line_index + 1)
                .ok_or(cs_files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len() - 1,
                })?
        };

        Ok(std::ops::Range { start, end })
    }
}

fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass {}));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(BodyAnalysisPass {}));
    pass_manager
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use async_lsp::lsp_types::{Diagnostic, NumberOrString};
    use common::InputDb;
    use driver::DriverDataBase;
    use std::collections::BTreeMap;
    use std::path::PathBuf;
    use test_utils::snap_test;
    use url::Url;

    fn code_to_string(code: &Option<NumberOrString>) -> String {
        match code {
            Some(NumberOrString::String(s)) => s.clone(),
            Some(NumberOrString::Number(n)) => n.to_string(),
            None => String::new(),
        }
    }

    // Produce a stable, human-readable snapshot of diagnostics per file.
    fn format_diagnostics(map: &rustc_hash::FxHashMap<Url, Vec<Diagnostic>>) -> String {
        // Sort by URI for determinism
        let mut by_uri: BTreeMap<String, Vec<Diagnostic>> = BTreeMap::new();
        for (uri, diags) in map.iter() {
            by_uri
                .entry(uri.to_string())
                .or_insert_with(Vec::new)
                .extend(diags.iter().cloned());
        }

        let mut out = String::new();
        for (uri, mut diags) in by_uri {
            // Stable sort: code, start line/char, end line/char, message prefix
            diags.sort_by(|a, b| {
                let ac = code_to_string(&a.code);
                let bc = code_to_string(&b.code);
                (
                    ac,
                    a.range.start.line,
                    a.range.start.character,
                    a.range.end.line,
                    a.range.end.character,
                    a.message.clone(),
                )
                    .cmp(&(
                        bc,
                        b.range.start.line,
                        b.range.start.character,
                        b.range.end.line,
                        b.range.end.character,
                        b.message.clone(),
                    ))
            });

            out.push_str(&format!("File: {}\n", uri));
            for d in diags {
                let code = code_to_string(&d.code);
                let sev = d.severity.map(|s| format!("{:?}", s)).unwrap_or_default();
                out.push_str(&format!(
                    "  - code:{} severity:{} @ {}:{}..{}:{}\n    {}\n",
                    code,
                    sev,
                    d.range.start.line + 1,
                    d.range.start.character + 1,
                    d.range.end.line + 1,
                    d.range.end.character + 1,
                    d.message.trim()
                ));
            }
            out.push('\n');
        }
        out
    }

    #[test]
    fn diagnostics_snapshot_comprehensive_project() {
        let mut db = DriverDataBase::default();
        let project_dir: PathBuf =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_projects/comprehensive");

        // Load the test project into the DB (mirrors server initialization)
        load_ingot_from_directory(&mut db, &project_dir);

        // Find the ingot from the directory URL
        let ingot_url =
            Url::from_directory_path(&project_dir).expect("failed to convert project dir to URL");
        let ingot = db
            .workspace()
            .containing_ingot(&db, ingot_url)
            .expect("ingot should be discoverable");

        // Compute diagnostics per file using the server path
        let map = db.diagnostics_for_ingot(ingot);
        let snapshot = format_diagnostics(&map);

        // Write snapshot alongside the project, like other dir-test layouts
        let snap_path = project_dir.join("diagnostics.snap");
        snap_test!(snapshot, snap_path.to_str().unwrap());
    }
}
