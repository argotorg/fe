pub mod diagnostics;

mod changed_choices;
mod hidden_items;
mod import_resolver;
pub(crate) mod items_in_scope;
pub(crate) mod method_selection;
mod name_resolver;
pub mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;
pub use visibility_checker::is_scope_visible_from;

use crate::core::hir_def::TopLevelMod;
pub(crate) use changed_choices::package_name;
pub use changed_choices::{ChangedChoice, Decision, DecisionSite, GeneratedItem, changed_choices};
use common::ingot::Ingot;
pub use hidden_items::{
    Binding, HiddenGeneratedItem, HiddenGeneratedMember, Located, MemberBinding, Via,
    hidden_generated_items, hidden_generated_members,
};
pub use import_resolver::ResolvedImports;
pub use items_in_scope::items_in_scope;
pub use name_resolver::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind,
    NameResolutionError, QueryDirective, Tier, ingot_has_generated_items, ingot_publishes_items,
};
pub(crate) use name_resolver::{authored_first, authored_first_filling, choose_in_tiers};
pub(crate) use path_resolver::resolve_type_path_definition;
pub(crate) use path_resolver::{FindAssociatedTypeError, find_associated_type};
pub use path_resolver::{
    PathRes, PathResError, PathResErrorKind, ResolvedVariant, resolve_ident_to_bucket, resolve_path,
};
pub(crate) use path_resolver::{
    TypePosition, resolve_ident_to_bucket_in, resolve_name_res_with_minter,
    resolve_path_with_minter, resolve_path_with_observer_and_minter,
    resolve_trait_path_with_observer_and_minter, resolve_type_position_path_with_minter,
    trait_arg_path_diag,
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
fn resolve_query_in_tier<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
    tier: Tier,
) -> NameResBucket<'db> {
    let hide = tier.hides(query.scope(db).ingot(db));
    let importer = DefaultImporter { hide };
    let mut name_resolver = name_resolver::NameResolver::new(db, &importer, hide);
    name_resolver.resolve_query(query)
}

/// Resolves a name query in `tier`. A step of a longer lookup procedure uses
/// this inside [`authored_first`], so that the procedure falls back to the
/// generated items as a whole.
pub(crate) fn resolve_query_in<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
    tier: Tier,
) -> &'db NameResBucket<'db> {
    resolve_query_in_tier(
        db,
        query,
        tier.in_ingot_for_names(db, query.scope(db).ingot(db)),
    )
}

/// Resolves a name query that is a lookup on its own: in the authored tier,
/// and in the full tier only if the authored tier answers nothing for the
/// name in any domain. An authored binding of the name in one domain thus
/// hides a generated item of the same name in another.
pub fn resolve_query<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
) -> &'db NameResBucket<'db> {
    authored_first(
        db,
        query.scope(db).ingot(db),
        |tier| resolve_query_in(db, query, tier),
        |bucket| bucket.answers(),
    )
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
        resolve_imports(db, ingot, Tier::All)
            .0
            .iter()
            .filter(|diag| diag.top_mod(db) == top_mod)
            .map(|diag| Box::new(diag.clone()) as _)
            .collect()
    }
}

/// The settled imports of `ingot` in `tier`, and their diagnostics. In the
/// authored tier, a use of an item that was generated in the ingot imports
/// nothing; the full tier is the program's meaning of its imports.
pub fn resolve_imports<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    tier: Tier,
) -> &'db (Vec<ImportDiag<'db>>, ResolvedImports<'db>) {
    resolve_imports_in_tier(db, ingot, tier.in_ingot_for_names(db, ingot))
}

#[salsa::tracked(cycle_initial=resolve_imports_cycle_initial)]
fn resolve_imports_in_tier<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    tier: Tier,
) -> (Vec<ImportDiag<'db>>, ResolvedImports<'db>) {
    let resolver = import_resolver::ImportResolver::new(db, ingot, tier);
    let (imports, diags) = resolver.resolve_imports();
    (diags, imports)
}
fn resolve_imports_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    ingot: Ingot<'db>,
    _tier: Tier,
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
