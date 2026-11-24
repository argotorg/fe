pub mod diagnostics;

mod import_resolver;
pub(crate) mod items_in_scope;
pub(crate) mod method_selection;
mod name_resolver;
pub mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;
pub use visibility_checker::is_scope_visible_from;

use crate::core::hir_def::TopLevelMod;
use common::ingot::Ingot;
pub use import_resolver::ResolvedImports;
pub use items_in_scope::items_in_scope;
pub use name_resolver::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind,
    NameResolutionError, QueryDirective,
};
pub(crate) use path_resolver::resolve_type_path_definition;
pub use path_resolver::{
    PathRes, PathResError, PathResErrorKind, ResolvedVariant, resolve_ident_to_bucket, resolve_path,
};
pub(crate) use path_resolver::{
    TypePosition, resolve_name_res_with_minter, resolve_path_with_minter,
    resolve_path_with_observer_and_minter, resolve_trait_path_with_observer_and_minter,
    resolve_type_position_path_with_minter, trait_arg_path_diag,
};
pub(crate) use path_resolver::{
    earliest_conflicting_inherent_const_impl, shadowed_inherent_fn_for_const,
};
use tracing::debug;
pub use traits_in_scope::available_traits_in_scope;

use self::{diagnostics::ImportDiag, import_resolver::DefaultImporter};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};

#[salsa::tracked]
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

#[salsa::tracked(cycle_initial=resolve_imports_cycle_initial)]
pub fn resolve_imports<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> (Vec<ImportDiag<'db>>, ResolvedImports<'db>) {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, diags) = resolver.resolve_imports();
    (diags, imports)
}
fn resolve_imports_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
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
