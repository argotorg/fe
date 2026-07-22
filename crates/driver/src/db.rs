use crate::diagnostics::CsDbWrapper;
use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use common::file::File;
use common::{
    define_input_db,
    diagnostics::{
        CompleteDiagnostic, Severity, cmp_complete_diagnostics, trim_trailing_line_whitespace,
    },
};
use hir::analysis::{
    analysis_pass::AnalysisPassManager,
    diagnostics::DiagnosticVoucher,
    initialize_analysis_pass,
    semantic::{LayoutEvidenceAnalysisPass, SemanticBorrowAnalysisPass},
};
use hir::{
    Ingot,
    hir_def::{HirIngot, TopLevelMod},
    lower::{map_file_to_mod, module_tree},
};

use crate::diagnostics::ToCsDiag;

define_input_db!(DriverDataBase);

impl DriverDataBase {
    // TODO: An temporary implementation for ui testing.
    pub fn run_on_top_mod<'db>(&'db self, top_mod: TopLevelMod<'db>) -> DiagnosticsCollection<'db> {
        self.run_on_file_with_pass_manager(top_mod, initialize_analysis_pass())
    }

    pub fn run_on_file_with_pass_manager<'db>(
        &'db self,
        top_mod: TopLevelMod<'db>,
        mut pass_manager: hir::analysis::analysis_pass::AnalysisPassManager,
    ) -> DiagnosticsCollection<'db> {
        DiagnosticsCollection(pass_manager.run_on_module(self, top_mod))
    }

    pub fn run_on_ingot<'db>(&'db self, ingot: Ingot<'db>) -> DiagnosticsCollection<'db> {
        self.run_on_ingot_with_pass_manager(ingot, initialize_analysis_pass())
    }

    pub fn run_on_ingot_with_pass_manager<'db>(
        &'db self,
        ingot: Ingot<'db>,
        mut pass_manager: hir::analysis::analysis_pass::AnalysisPassManager,
    ) -> DiagnosticsCollection<'db> {
        let tree = module_tree(self, ingot);
        DiagnosticsCollection(pass_manager.run_on_module_tree(self, tree))
    }

    pub fn top_mod(&self, input: File) -> TopLevelMod<'_> {
        map_file_to_mod(self, input)
    }

    pub fn mir_diagnostics_for_top_mod<'db>(
        &'db self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<CompleteDiagnostic> {
        let mut pass_manager = initialize_mir_diagnostics_pass();
        let mut diagnostics: Vec<_> = pass_manager
            .run_on_module(self, top_mod)
            .into_iter()
            .map(|diag| diag.to_complete(self))
            .collect();
        sort_and_dedup_complete_diagnostics(&mut diagnostics);
        diagnostics
    }

    pub fn mir_diagnostics_for_ingot<'db>(&'db self, ingot: Ingot<'db>) -> Vec<CompleteDiagnostic> {
        // Empty ingots (e.g. deleted during incremental workspace changes)
        // have no root module to analyze.
        if ingot.module_tree(self).root_data().is_none() {
            return Vec::new();
        };
        if self.run_on_ingot(ingot).has_errors(self) {
            return Vec::new();
        }
        let mut pass_manager = initialize_mir_diagnostics_pass();
        let mut diagnostics: Vec<_> = pass_manager
            .run_on_module_tree(self, ingot.module_tree(self))
            .into_iter()
            .map(|diag| diag.to_complete(self))
            .collect();
        sort_and_dedup_complete_diagnostics(&mut diagnostics);
        diagnostics
    }

    pub fn emit_complete_diagnostics(&self, diagnostics: &[CompleteDiagnostic]) {
        let writer = BufferWriter::stderr(ColorChoice::Auto);
        let mut buffer = writer.buffer();
        let config = term::Config::default();
        let mut diagnostics = diagnostics.to_vec();
        sort_and_dedup_complete_diagnostics(&mut diagnostics);

        for diag in diagnostics {
            term::emit(&mut buffer, &config, &CsDbWrapper(self), &diag.to_cs(self)).unwrap();
        }

        writer
            .print(&buffer)
            .expect("Failed to write diagnostics to stderr");
    }

    pub fn format_complete_diagnostics(&self, diagnostics: &[CompleteDiagnostic]) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();
        let mut diagnostics = diagnostics.to_vec();
        sort_and_dedup_complete_diagnostics(&mut diagnostics);

        for diag in diagnostics {
            term::emit(&mut buffer, &config, &CsDbWrapper(self), &diag.to_cs(self)).unwrap();
        }

        trim_trailing_line_whitespace(std::str::from_utf8(buffer.as_slice()).unwrap())
    }
}

fn initialize_mir_diagnostics_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass("SemanticBorrow", Box::new(SemanticBorrowAnalysisPass));
    pass_manager.add_module_pass("LayoutEvidence", Box::new(LayoutEvidenceAnalysisPass));
    pass_manager
}

pub struct DiagnosticsCollection<'db>(Vec<Box<dyn DiagnosticVoucher + 'db>>);
impl DiagnosticsCollection<'_> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn has_errors(&self, db: &DriverDataBase) -> bool {
        self.finalize(db)
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    pub fn emit(&self, db: &DriverDataBase) {
        let writer = BufferWriter::stderr(ColorChoice::Auto);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
        }

        writer
            .print(&buffer)
            .expect("Failed to write diagnostics to stderr");
    }

    /// Format the accumulated diagnostics to a string.
    pub fn format_diags(&self, db: &DriverDataBase) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
        }

        trim_trailing_line_whitespace(std::str::from_utf8(buffer.as_slice()).unwrap())
    }

    fn finalize(&self, db: &DriverDataBase) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = self.0.iter().map(|d| d.as_ref().to_complete(db)).collect();
        sort_and_dedup_complete_diagnostics(&mut diags);
        diags
    }
}

fn sort_complete_diagnostics(diags: &mut [CompleteDiagnostic]) {
    diags.sort_by(cmp_complete_diagnostics);
}

fn sort_and_dedup_complete_diagnostics(diags: &mut Vec<CompleteDiagnostic>) {
    sort_complete_diagnostics(diags);
    diags.dedup();
}
