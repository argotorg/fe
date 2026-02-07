use async_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};
use camino::Utf8Path;
use codespan_reporting::files as cs_files;
use common::{
    diagnostics::CompleteDiagnostic,
    file::{File, IngotFileKind},
    ingot::IngotKind,
};
use driver::DriverDataBase;
use hir::Ingot;
use hir::analysis::analysis_pass::{AnalysisPassManager, MsgLowerPass, ParsingPass};
use hir::analysis::name_resolution::ImportAnalysisPass;
use hir::analysis::ty::{
    AdtDefAnalysisPass, BodyAnalysisPass, DefConflictAnalysisPass, FuncAnalysisPass,
    ImplAnalysisPass, ImplTraitAnalysisPass, MsgSelectorAnalysisPass, TraitAnalysisPass,
    TypeAliasAnalysisPass,
};
use hir::lower::map_file_to_mod;
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
        let is_standalone = ingot.kind(self) == IngotKind::StandAlone;

        for (url, file) in ingot_files.iter() {
            if !matches!(file.kind(self), Some(IngotFileKind::Source)) {
                continue;
            }

            // initialize an empty diagnostic list for this file
            // (to clear any previous diagnostics)
            let file_diags = result.entry(url.clone()).or_default();

            // Add warning for standalone files (files outside of a proper ingot)
            if is_standalone {
                file_diags.push(standalone_file_warning());
            }

            let top_mod = map_file_to_mod(self, file);
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
    pass_manager.add_module_pass(Box::new(MsgLowerPass {}));
    pass_manager.add_module_pass(Box::new(MsgSelectorAnalysisPass {}));
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

/// Creates a warning diagnostic for standalone files (files outside of a proper ingot).
fn standalone_file_warning() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        },
        severity: Some(DiagnosticSeverity::WARNING),
        code: None,
        source: Some("fe".to_string()),
        message: "This file is not part of an ingot and should be considered isolated from other .fe files."
            .to_string(),
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }
}
