use crate::diagnostics::CsDbWrapper;
use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use common::file::File;
use common::{
    define_input_db,
    diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics},
diagnostics::{CompleteDiagnostic, Severity},
};
use hir::analysis::{
    analysis_pass::{AnalysisPassManager, EventLowerPass, MsgLowerPass, ParsingPass},
    diagnostics::DiagnosticVoucher,
    name_resolution::ImportAnalysisPass,
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, ContractAnalysisPass, DefConflictAnalysisPass,
        FuncAnalysisPass, ImplAnalysisPass, ImplTraitAnalysisPass, MsgSelectorAnalysisPass,
        TraitAnalysisPass, TypeAliasAnalysisPass,
    },
};
use hir::{
    Ingot,
    hir_def::{HirIngot, TopLevelMod},
    lower::{map_file_to_mod, module_tree},
};
use mir::{MirDiagnosticsMode, collect_mir_diagnostics};

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
        mut pass_manager: AnalysisPassManager,
    ) -> DiagnosticsCollection<'db> {
        DiagnosticsCollection(pass_manager.run_on_module(self, top_mod))
    }

    pub fn run_on_ingot<'db>(&'db self, ingot: Ingot<'db>) -> DiagnosticsCollection<'db> {
        self.run_on_ingot_with_pass_manager(ingot, initialize_analysis_pass())
    }

    pub fn run_on_ingot_with_pass_manager<'db>(
        &'db self,
        ingot: Ingot<'db>,
        mut pass_manager: AnalysisPassManager,
    ) -> DiagnosticsCollection<'db> {
        let tree = module_tree(self, ingot);
        DiagnosticsCollection(pass_manager.run_on_module_tree(self, tree))
    }

    pub fn top_mod(&self, input: File) -> TopLevelMod<'_> {
        map_file_to_mod(self, input)
    }

    pub fn mir_diagnostics_for_ingot<'db>(
        &'db self,
        ingot: Ingot<'db>,
        mode: MirDiagnosticsMode,
    ) -> Vec<CompleteDiagnostic> {
        // Empty ingots (e.g. deleted during incremental workspace changes)
        // have no root module to analyze.
        let Some(root_data) = ingot.module_tree(self).root_data() else {
            return Vec::new();
        };
        let top_mod = root_data.top_mod;
        let mut output = collect_mir_diagnostics(self, top_mod, mode);
        for err in output.internal_errors {
            tracing::debug!(target: "lsp", "MIR diagnostics internal error: {err}");
        }
        sort_and_dedup_complete_diagnostics(&mut output.diagnostics);
        output.diagnostics
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

        std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
    }

    fn finalize(&self, db: &DriverDataBase) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = self.0.iter().map(|d| d.as_ref().to_complete(db)).collect();
        sort_complete_diagnostics(&mut diags);
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

fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass("Parsing", Box::new(ParsingPass {}));
    pass_manager.add_module_pass("MsgLower", Box::new(MsgLowerPass {}));
    pass_manager.add_module_pass("EventLower", Box::new(EventLowerPass {}));
    pass_manager.add_module_pass("MsgSelector", Box::new(MsgSelectorAnalysisPass {}));
    pass_manager.add_module_pass("DefConflict", Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass("Import", Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass("AdtDef", Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass("TypeAlias", Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass("Trait", Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass("Impl", Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass("ImplTrait", Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass("Func", Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass("Body", Box::new(BodyAnalysisPass {}));
    pass_manager.add_module_pass("Contract", Box::new(ContractAnalysisPass {}));
    pass_manager
}
