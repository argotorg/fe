pub mod diagnostics;

mod import_resolver;
pub(crate) mod method_selection;
mod name_resolver;
mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;

use crate::hir_def::TopLevelMod;
use common::ingot::Ingot;
pub use import_resolver::ResolvedImports;
pub use name_resolver::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind,
    NameResolutionError, QueryDirective,
};
pub use path_resolver::{
    PathRes, PathResError, PathResErrorKind, ResolvedVariant, find_associated_type,
    resolve_ident_to_bucket, resolve_name_res, resolve_path, resolve_path_with_observer,
};
use tracing::debug;
pub use traits_in_scope::available_traits_in_scope;
pub(crate) use visibility_checker::is_scope_visible_from;

use self::{diagnostics::ImportDiag, import_resolver::DefaultImporter};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};

#[salsa::tracked(return_ref)]
pub fn resolve_query<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
) -> NameResBucket<'db> {
    let importer = DefaultImporter;
    let mut name_resolver = name_resolver::NameResolver::new(db, &importer);
    name_resolver.resolve_query(query)
}

/// Performs import resolution analysis. This pass only checks correctness of
/// the imports and doesn't emit other name resolutions errors.
pub struct ImportAnalysisPass {}

impl ModuleAnalysisPass for ImportAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let ingot = top_mod.ingot(db);
        resolve_imports(db, ingot)
            .0
            .iter()
            .filter(|diag| diag.top_mod(db) == top_mod)
            .map(|diag| Box::new(diag.clone()) as _)
            .collect()
    }
}

#[salsa::tracked(return_ref, cycle_fn=resolve_imports_cycle_recover, cycle_initial=resolve_imports_cycle_initial)]
pub fn resolve_imports<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> (Vec<ImportDiag<'db>>, ResolvedImports<'db>) {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, diags) = resolver.resolve_imports();
    (diags, imports)
}
fn resolve_imports_cycle_recover<'db>(
    db: &'db dyn HirAnalysisDb,
    _value: &(Vec<ImportDiag<'db>>, ResolvedImports<'db>),
    count: u32,
    ingot: Ingot<'db>,
) -> salsa::CycleRecoveryAction<(Vec<ImportDiag<'db>>, ResolvedImports<'db>)> {
    // Log cycle information for debugging
    debug!(
        "[CYCLE DETECTED] resolve_imports cycle detected for ingot '{}' (kind={:?}), iteration #{}",
        ingot.base(db),
        ingot.kind(db),
        count
    );

    // Return the iterate strategy to allow for fixpoint iteration
    // This allows Salsa to iteratively converge to a fixpoint solution
    salsa::CycleRecoveryAction::Iterate
}

fn resolve_imports_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> (Vec<ImportDiag<'db>>, ResolvedImports<'db>) {
    // Log initial cycle value creation for debugging
    debug!(
        "[CYCLE INITIAL] Creating initial value for resolve_imports cycle in ingot '{}' (kind={:?})",
        ingot.base(db),
        ingot.kind(db)
    );

    // Return empty results for the initial value in case of a cycle
    // This provides a starting point for fixpoint iteration
    (Vec::new(), ResolvedImports::default())
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpectedPathKind {
    /// NameDomain::TYPE
    Type,
    Function,
    /// NameDomain::TYPE
    Trait,
    /// NameDomain::VALUE
    Value,
    /// NameDomain::VALUE | NameDomain::TYPE
    Record,
    /// NameDomain::VALUE | NameDomain::TYPE
    Pat,
}
