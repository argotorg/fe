use crate::{diagnostics::DiagnosticVoucher, HirAnalysisDb};
use hir::{
    hir_def::{ModuleTree, TopLevelMod},
    lower::parse_file_impl,
    ParserError,
};

/// All analysis passes that run analysis on the HIR top level module
/// granularity should implement this trait.
pub trait ModuleAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>>;
}

#[derive(Default)]
pub struct AnalysisPassManager {
    module_passes: Vec<Box<dyn ModuleAnalysisPass>>,
}

impl AnalysisPassManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module_pass(&mut self, pass: Box<dyn ModuleAnalysisPass>) {
        self.module_passes.push(pass);
    }

    pub fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        for pass in self.module_passes.iter_mut() {
            diags.extend(pass.run_on_module(db, top_mod));
        }
        diags
    }

    /// Stable alternative to run_on_module that uses File as the key.
    /// This prevents issues with stale TopLevelMod references during incremental recompilation.
    pub fn run_on_file<'db, DB>(
        &mut self,
        db: &'db DB,
        file: common::file::File,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>>
    where
        DB: HirAnalysisDb + hir::LowerHirDb,
    {
        // Convert File to fresh TopLevelMod using the stable API
        let top_mod = hir::lower::map_file_to_mod(db, file);
        // Use the existing analysis logic
        self.run_on_module(db, top_mod)
    }

    pub fn run_on_module_tree<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        tree: &'db ModuleTree<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        for module in tree.all_modules() {
            for pass in self.module_passes.iter_mut() {
                diags.extend(pass.run_on_module(db, module));
            }
        }
        diags
    }
}

#[derive(Clone, Copy)]
pub struct ParsingPass {}

impl ModuleAnalysisPass for ParsingPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        parse_file_impl::accumulated::<ParserError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}
