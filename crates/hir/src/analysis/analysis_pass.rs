use crate::analysis::{HirAnalysisDb, diagnostics::DiagnosticVoucher};
use crate::{
    EventError, ParserError, SelectorError,
    hir_def::{ModuleTree, TopLevelMod},
    lower::{parse_file_impl, scope_graph_impl},
};

fn diag_timing_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("FE_DIAG_TIMING").is_ok())
}

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
    module_passes: Vec<(&'static str, Box<dyn ModuleAnalysisPass>)>,
}

impl AnalysisPassManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module_pass(&mut self, name: &'static str, pass: Box<dyn ModuleAnalysisPass>) {
        self.module_passes.push((name, pass));
    }

    pub fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let timing = diag_timing_enabled();
        let mut diags = vec![];
        for (name, pass) in self.module_passes.iter_mut() {
            let t0 = std::time::Instant::now();
            diags.extend(pass.run_on_module(db, top_mod));
            if timing {
                let elapsed = t0.elapsed();
                if elapsed.as_micros() > 100 {
                    eprintln!("[fe:timing]   pass {name}: {elapsed:?}");
                }
            }
        }
        diags
    }

    pub fn run_on_module_tree<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        tree: &'db ModuleTree<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let timing = diag_timing_enabled();
        let mut diags = vec![];
        for module in tree.all_modules() {
            for (name, pass) in self.module_passes.iter_mut() {
                let t0 = std::time::Instant::now();
                diags.extend(pass.run_on_module(db, module));
                if timing {
                    let elapsed = t0.elapsed();
                    if elapsed.as_micros() > 100 {
                        eprintln!("[fe:timing]   pass {name}: {elapsed:?}");
                    }
                }
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

/// Analysis pass that collects selector errors from msg block lowering.
pub struct MsgLowerPass {}

impl ModuleAnalysisPass for MsgLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<SelectorError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects event lowering errors from `#[event]` struct desugaring.
pub struct EventLowerPass {}

impl ModuleAnalysisPass for EventLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<EventError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}
