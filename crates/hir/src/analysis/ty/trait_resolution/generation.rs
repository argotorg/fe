//! A lexical offer and the preparation dependency it can introduce. Neither
//! is evidence: only an admitted generated implementation can produce proof.
use common::generation::{ScopedDeriveDemand, ScopedDeriveOpeningKey};
use salsa::Accumulator;

use crate::{
    analysis::HirAnalysisDb,
    hir_def::{Trait, scope_graph::ScopeId},
};

use super::super::{
    fold::{TyFoldable, TyFolder},
    trait_def::TraitInstId,
    ty_def::TyId,
    visitor::{TyVisitable, TyVisitor},
};

#[salsa::interned]
#[derive(Debug)]
pub struct ProviderOpening<'db> {
    pub key: ScopedDeriveOpeningKey,
    #[returns(copy)]
    pub provider_ty: TyId<'db>,
    #[returns(copy)]
    pub offered_trait: Trait<'db>,
    #[returns(copy)]
    pub scope: ScopeId<'db>,
}

impl<'db> TyFoldable<'db> for ProviderOpening<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self::new(
            db,
            self.key(db).clone(),
            self.provider_ty(db).fold_with(db, folder),
            self.offered_trait(db),
            self.scope(db),
        )
    }
}

impl<'db> TyVisitable<'db> for ProviderOpening<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.provider_ty(visitor.db()).visit_with(visitor);
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenerationRequest<'db> {
    #[returns(copy)]
    pub opening: ProviderOpening<'db>,
    #[returns(copy)]
    pub goal: TraitInstId<'db>,
}

/// Query-tracked requests are replayed with cached checking results. Only
/// frozen authored identities cross preparation views; no proof IDs do.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenerationDemand {
    pub opening: ScopedDeriveOpeningKey,
    pub demand: Result<ScopedDeriveDemand, String>,
}

pub(super) fn record_request<'db>(db: &'db dyn HirAnalysisDb, request: GenerationRequest<'db>) {
    GenerationDemand {
        opening: request.opening(db).key(db).clone(),
        demand: crate::core::meta::freeze_generation_request(db, request),
    }
    .accumulate(db);
}

/// Header discovery advertises an offer even when its prerequisites will fail.
/// Selection and prerequisite proofs remain owned by the ordinary solver.
pub(crate) fn offered_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    provider_ty: TyId<'db>,
) -> Vec<Trait<'db>> {
    use super::super::{
        canonical::Canonical,
        corelib::resolve_core_trait,
        trait_def::impls_for_trait_in_ingots,
        ty_def::{TyBase, TyData},
        unify::UnificationTable,
    };
    if super::super::visitor::collect_flags(db, provider_ty).intersects(
        super::super::ty_def::TyFlags::HAS_VAR | super::super::ty_def::TyFlags::HAS_INVALID,
    ) {
        return Vec::new();
    }
    let Some(derive) = resolve_core_trait(db, scope, &["derive", "Derive"]) else {
        return Vec::new();
    };
    // Only the trait definition and ingots are used by the inventory query.
    let inventory_goal = TraitInstId::new_simple(db, derive, vec![provider_ty, TyId::unit(db)]);
    let (primary, secondary) =
        super::TraitSolveCx::new(db, scope).search_ingots_for_trait_inst(db, inventory_goal);
    let mut offers = Vec::new();
    for &header in
        impls_for_trait_in_ingots(db, primary, secondary, Canonical::new(db, inventory_goal))
    {
        let mut table = UnificationTable::new(db);
        let candidate = table.instantiate_with_fresh_vars(header);
        if table
            .unify(candidate.trait_inst(db).self_ty(db), provider_ty)
            .is_err()
        {
            continue;
        }
        let Some(&descriptor) = candidate.trait_inst(db).args(db).get(1) else {
            continue;
        };
        let descriptor = descriptor.fold_with(db, &mut table);
        if let TyData::TyBase(TyBase::TraitDescriptor(trait_)) = descriptor.data(db)
            && !offers.contains(trait_)
        {
            offers.push(*trait_);
        }
    }
    offers
}

/// Retrieve checked provider identity; no source path or provider selection is
/// reconstructed by the preparation driver.
pub fn provider_opening_at<'db>(
    db: &'db dyn HirAnalysisDb,
    key: &ScopedDeriveOpeningKey,
    offered_trait: Trait<'db>,
) -> Option<ProviderOpening<'db>> {
    super::super::ty_check::provider_opening_at(db, key, offered_trait)
}

#[salsa::tracked]
fn check_preparation_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
) {
    use crate::analysis::analysis_pass::ModuleAnalysisPass;
    // These are the existing checking passes, not another obligation walker.
    let _ = super::super::BodyAnalysisPass {}.run_on_module(db, module);
    let _ = super::super::ContractAnalysisPass {}.run_on_module(db, module);
}

/// Pending requests take precedence over provisional diagnostics. A caller
/// publishes a view only after this list is empty and ordinary checking passes.
pub fn preparation_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
) -> Vec<GenerationDemand> {
    check_preparation_bodies::accumulated::<GenerationDemand>(db, module)
        .into_iter()
        .cloned()
        .collect()
}
