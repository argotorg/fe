use crate::analysis::{HirAnalysisDb, diagnostics::DiagnosticVoucher};
use crate::{
    ArithmeticAttrError, DeriveError, ErrorDiagnostic, EventError, InlineAttrError,
    LoopUnrollAttrError, ParserError, PayableError, SelectorError,
    hir_def::{ModuleTree, TopLevelMod},
    lower::{parse_file_impl, scope_graph_impl},
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
        let mut diags = vec![];
        for (name, pass) in self.module_passes.iter_mut() {
            let t0 = std::time::Instant::now();
            diags.extend(pass.run_on_module(db, top_mod));
            let elapsed = t0.elapsed();
            if elapsed.as_micros() > 100 {
                tracing::debug!("[fe:timing]   pass {name}: {elapsed:?}");
            }
        }
        diags
    }

    pub fn run_on_module_tree<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        tree: &'db ModuleTree<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        for module in tree.all_modules() {
            for (name, pass) in self.module_passes.iter_mut() {
                let t0 = std::time::Instant::now();
                diags.extend(pass.run_on_module(db, module));
                let elapsed = t0.elapsed();
                if elapsed.as_micros() > 100 {
                    tracing::debug!("[fe:timing]   pass {name}: {elapsed:?}");
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

/// Analysis pass that collects error lowering diagnostics from `#[error]` struct desugaring.
pub struct ErrorLowerPass {}

impl ModuleAnalysisPass for ErrorLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<ErrorDiagnostic>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects derive lowering errors from `#[derive(...)]` struct desugaring.
pub struct DeriveLowerPass {}

impl ModuleAnalysisPass for DeriveLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<DeriveError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects arithmetic attribute validation errors.
pub struct ArithmeticAttrPass {}

impl ModuleAnalysisPass for ArithmeticAttrPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<ArithmeticAttrError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects payable attribute validation errors.
pub struct PayableAttrPass {}

impl ModuleAnalysisPass for PayableAttrPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<PayableError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects invalid `#[inline]` attributes from function lowering.
pub struct InlineAttrPass {}

impl ModuleAnalysisPass for InlineAttrPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<InlineAttrError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects invalid loop unroll attributes from `for` lowering.
pub struct LoopUnrollAttrPass {}

impl ModuleAnalysisPass for LoopUnrollAttrPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<LoopUnrollAttrError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}
