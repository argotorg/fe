//! A lexical offer and the preparation dependency it can introduce. Neither
//! is evidence: only an admitted generated implementation can produce proof.
use common::generation::{ScopedDeriveDemand, ScopedDeriveOpeningKey};
use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemanticInstance, SemanticInstanceKey},
    },
    hir_def::{Trait, scope_graph::ScopeId},
};

use super::super::{
    fold::{TyFoldable, TyFolder},
    trait_def::{ImplementorOrigin, TraitInstId},
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

/// What preparation must settle before a checked body can be trusted: either
/// a request for a generated implementation or a refusal of an opening that
/// can never be served. Demands travel in query results, never beside them,
/// so a cached result carries exactly the demands that produced it. Only
/// frozen authored identities cross preparation views; no proof IDs do.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct GenerationDemand {
    pub opening: ScopedDeriveOpeningKey,
    pub demand: Result<ScopedDeriveDemand, String>,
}

impl GenerationDemand {
    /// The demand a pending request makes. Freezing keeps only authored
    /// identities; a request that cannot be frozen becomes a refusal.
    pub(crate) fn pending<'db>(
        db: &'db dyn HirAnalysisDb,
        request: GenerationRequest<'db>,
    ) -> Self {
        Self {
            opening: request.opening(db).key(db).clone(),
            demand: crate::core::meta::freeze_generation_request(db, request),
        }
    }

    pub(crate) fn refused(opening: ScopedDeriveOpeningKey, cause: impl Into<String>) -> Self {
        Self {
            opening,
            demand: Err(cause.into()),
        }
    }
}

/// The demands of one checked body, in the order they were made and without
/// repeats. Order carries no authority: consumers must treat the set as a
/// whole.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, SalsaValue)]
pub struct GenerationDemands {
    demands: Vec<GenerationDemand>,
}

impl GenerationDemands {
    pub fn is_empty(&self) -> bool {
        self.demands.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, GenerationDemand> {
        self.demands.iter()
    }

    pub fn into_vec(self) -> Vec<GenerationDemand> {
        self.demands
    }

    pub(crate) fn push(&mut self, demand: GenerationDemand) {
        if !self.demands.contains(&demand) {
            self.demands.push(demand);
        }
    }

    pub(crate) fn refuse(&mut self, opening: ScopedDeriveOpeningKey, cause: impl Into<String>) {
        self.push(GenerationDemand::refused(opening, cause));
    }

    pub(crate) fn request<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        request: GenerationRequest<'db>,
    ) {
        self.push(GenerationDemand::pending(db, request));
    }

    pub(crate) fn requests<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        requests: impl IntoIterator<Item = GenerationRequest<'db>>,
    ) {
        for request in requests {
            self.request(db, request);
        }
    }

    pub(crate) fn extend(&mut self, other: &GenerationDemands) {
        for demand in &other.demands {
            self.push(demand.clone());
        }
    }
}

impl<'a> IntoIterator for &'a GenerationDemands {
    type Item = &'a GenerationDemand;
    type IntoIter = std::slice::Iter<'a, GenerationDemand>;

    fn into_iter(self) -> Self::IntoIter {
        self.demands.iter()
    }
}

/// Header discovery advertises an offer even when its prerequisites will fail.
/// Selection and prerequisite proofs remain owned by the ordinary solver.
pub(crate) fn offered_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    provider_ty: TyId<'db>,
) -> Vec<Trait<'db>> {
    let mut offers = Vec::new();
    for (offer, _) in provider_headers(db, scope, provider_ty) {
        if !offers.contains(&offer) {
            offers.push(offer);
        }
    }
    offers
}

/// The `Derive` headers that apply to `provider_ty`, each with the trait its
/// descriptor names and the implementation that wrote it.
fn provider_headers<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    provider_ty: TyId<'db>,
) -> Vec<(Trait<'db>, ImplementorOrigin<'db>)> {
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
    let mut headers = Vec::new();
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
        if let TyData::TyBase(TyBase::TraitDescriptor(trait_)) = descriptor.data(db) {
            headers.push((*trait_, candidate.origin(db)));
        }
    }
    headers
}

/// The demands met by running the instance `key`: those of its own body and,
/// transitively, of the bodies of the calls it makes and of the constants it
/// reads. Each of these bodies runs, so an evaluation whose result depends on
/// a generated implementation is not ready while any of them waits for one.
pub fn evaluation_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> GenerationDemands {
    use super::super::ty_check::checked_body_demands;
    let mut demands = GenerationDemands::default();
    let mut pending = std::collections::VecDeque::from([key]);
    let mut seen = std::collections::HashSet::new();
    while let Some(key) = pending.pop_front() {
        if !seen.insert(key) {
            continue;
        }
        demands.extend(&checked_body_demands(db, key.owner(db)));
        let instance = SemanticInstance::new(db, key);
        for callee in instance.callees(db) {
            pending.push_back(callee.key);
        }
        pending.extend(instance.implicit_dependencies(db));
    }
    demands
}

/// The demands a provider's admission reads before any provider code can run:
/// those of the bodies that decide whether the provider applies. They are the
/// const predicates of the implementation that offers the request's trait and
/// of the nominal types it is admitted for, and the implementation's method
/// bodies; and, for each predicate that admission evaluates, those of the
/// calls and constants its evaluation runs. A request whose admission has such
/// a demand is not ready; the demand is served first.
pub fn admission_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    request: GenerationRequest<'db>,
) -> GenerationDemands {
    use super::super::ty_check::{
        BodyOwner, checked_body_demands, evaluated_const_predicates, type_admission_instances,
    };
    use crate::{
        analysis::semantic::{GenericSubst, const_body_instance},
        hir_def::WhereClauseOwner,
    };
    let opening = request.opening(db);
    let goal = request.goal(db);
    let provider_ty = opening.provider_ty(db);
    // The bodies themselves: a predicate that is not well formed yet, because
    // it waits for a generated implementation, is evaluated by no instance.
    let mut owners = Vec::new();
    let mut instances = Vec::new();
    for (offered, origin) in provider_headers(db, opening.scope(db), provider_ty) {
        if offered != goal.def(db) {
            continue;
        }
        if let ImplementorOrigin::Hir(implementation) = origin {
            owners.extend(BodyOwner::const_predicates_of(db, implementation.into()));
            for method in implementation.methods(db) {
                owners.extend(BodyOwner::func_bodies(db, method));
            }
            // Declaration checking evaluates these predicates with the empty
            // substitution.
            for predicate in
                evaluated_const_predicates(db, WhereClauseOwner::ImplTrait(implementation))
            {
                let owner = BodyOwner::const_predicate(db, predicate);
                instances.push(const_body_instance(db, owner, GenericSubst::none(db)));
            }
        }
    }
    for ty in goal
        .args(db)
        .first()
        .copied()
        .into_iter()
        .chain([provider_ty])
    {
        if let Some(adt) = ty.adt_ref(db) {
            owners.extend(BodyOwner::const_predicates_of(db, adt.as_item()));
        }
        instances.extend(type_admission_instances(db, ty));
    }
    let mut demands = GenerationDemands::default();
    for owner in owners {
        demands.extend(&checked_body_demands(db, owner));
    }
    for instance in instances {
        demands.extend(&evaluation_demands(db, instance.key(db)));
    }
    demands
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

/// Pending requests take precedence over provisional diagnostics. A caller
/// publishes a view only after this list is empty and ordinary checking passes.
/// The list is the union of the demands of every body checking `module`
/// checks; each body reports its own, so none is read from another body's
/// result.
pub fn preparation_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
) -> Vec<GenerationDemand> {
    let mut demands = GenerationDemands::default();
    for owner in super::super::ty_check::module_body_owners(db, module) {
        demands.extend(&super::super::ty_check::checked_body_demands(db, owner));
    }
    demands.into_vec()
}
