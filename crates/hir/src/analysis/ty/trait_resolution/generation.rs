//! A lexical offer and the preparation dependency it can introduce. Neither
//! is evidence: only an admitted generated implementation can produce proof.
use crate::analysis::name_resolution::Tier;
use common::{
    generation::{ScopedDeriveDemand, ScopedDeriveOpeningKey},
    indexmap::IndexSet,
};
use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemanticInstance, SemanticInstanceKey, root_semantic_instance_key},
    },
    hir_def::{Trait, scope_graph::ScopeId},
};

use super::super::{
    canonical::Canonical,
    fold::{TyFoldable, TyFolder},
    trait_def::{ImplementorOrigin, TraitInstId},
    ty_def::TyId,
    visitor::{TyVisitable, TyVisitor},
};
use super::PredicateListId;

#[salsa::interned]
#[derive(Debug)]
pub struct ProviderOpening<'db> {
    pub key: ScopedDeriveOpeningKey,
    #[returns(copy)]
    pub provider_ty: TyId<'db>,
    pub hints: ProviderHints<'db>,
    #[returns(copy)]
    pub scope: ScopeId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct ProviderHints<'db> {
    pub(crate) traits: Vec<Trait<'db>>,
    pub(crate) open: bool,
}

impl<'db> ProviderOpening<'db> {
    pub(crate) fn may_offer(self, db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> bool {
        let hints = self.hints(db);
        hints.traits.contains(&trait_)
            || (hints.open && trait_.ingot(db) == self.scope(db).ingot(db))
    }

    pub(crate) fn traits_for_lookup(
        self,
        db: &'db dyn HirAnalysisDb,
        visible: impl IntoIterator<Item = Trait<'db>>,
    ) -> impl Iterator<Item = Trait<'db>> {
        let hints = self.hints(db);
        hints
            .traits
            .iter()
            .copied()
            .chain(visible.into_iter().filter(move |&trait_| {
                hints.open && self.may_offer(db, trait_) && !hints.traits.contains(&trait_)
            }))
    }
}

impl<'db> TyFoldable<'db> for ProviderOpening<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self::new(
            db,
            self.key(db).clone(),
            self.provider_ty(db).fold_with(db, folder),
            self.hints(db).clone(),
            self.scope(db),
        )
    }
}

impl<'db> TyVisitable<'db> for ProviderOpening<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.provider_ty(visitor.db()).visit_with(visitor);
    }
}

#[salsa::interned(constructor = intern)]
#[derive(Debug)]
pub struct GenerationRequest<'db> {
    #[returns(copy)]
    pub opening: ProviderOpening<'db>,
    #[returns(copy)]
    pub goal: TraitInstId<'db>,
    #[returns(copy)]
    pub(crate) refusal: Option<&'static str>,
}

impl<'db> GenerationRequest<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        opening: ProviderOpening<'db>,
        goal: TraitInstId<'db>,
    ) -> Self {
        Self::intern(db, opening, goal, None)
    }

    pub(crate) fn refused(
        db: &'db dyn HirAnalysisDb,
        opening: ProviderOpening<'db>,
        goal: TraitInstId<'db>,
        reason: &'static str,
    ) -> Self {
        Self::intern(db, opening, goal, Some(reason))
    }
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

/// The head information available before a goal is known. A closed family
/// can be compared eagerly. An open head keeps only a name hint.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum OfferHead<'db> {
    Family(TyId<'db>),
    Trait(Trait<'db>),
    Any,
}

impl<'db> OfferHead<'db> {
    fn trait_hint(self, db: &'db dyn HirAnalysisDb) -> Option<Trait<'db>> {
        match self {
            Self::Family(ty) => match ty.data(db) {
                super::super::ty_def::TyData::TraitFamily(family) => Some(family.def(db)),
                _ => unreachable!("a family offer always contains a trait family"),
            },
            Self::Trait(trait_) => Some(trait_),
            Self::Any => None,
        }
    }

    fn intersect(self, db: &'db dyn HirAnalysisDb, rhs: Self) -> Option<Self> {
        match (self, rhs) {
            (Self::Any, other) | (other, Self::Any) => Some(other),
            (lhs, rhs) if lhs == rhs => Some(lhs),
            (Self::Family(_), Self::Family(_)) => None,
            (lhs, rhs) if lhs.trait_hint(db) == rhs.trait_hint(db) => {
                lhs.trait_hint(db).map(Self::Trait)
            }
            _ => None,
        }
    }
}

/// Discover names for lexical method lookup. Actual eligibility is checked on
/// the requested family, before the ordinary solver discharges prerequisites.
pub(crate) fn provider_hints<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    provider_ty: TyId<'db>,
) -> Option<ProviderHints<'db>> {
    if provider_ty.has_invalid(db) || provider_ty.has_var(db) {
        return None;
    }
    let (offers, problem) = provider_headers(db, scope, provider_ty);
    let mut traits = IndexSet::default();
    let mut open = problem.is_some();
    for (head, _) in offers {
        if let Some(trait_) = head.trait_hint(db) {
            traits.insert(trait_);
        } else {
            open = true;
        }
    }
    (open || !traits.is_empty()).then(|| ProviderHints {
        traits: traits.into_iter().collect(),
        open,
    })
}

pub(crate) fn closed_offers<'db>(
    db: &'db dyn HirAnalysisDb,
    opening: ProviderOpening<'db>,
) -> Vec<TyId<'db>> {
    let (offers, _) = provider_headers(db, opening.scope(db), opening.provider_ty(db));
    offers
        .into_iter()
        .filter_map(|(head, _)| match head {
            OfferHead::Family(family) => Some(family),
            _ => None,
        })
        .collect::<common::indexmap::IndexSet<_>>()
        .into_iter()
        .collect()
}

pub(crate) fn provider_offers_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    opening: ProviderOpening<'db>,
    goal: TraitInstId<'db>,
) -> Result<bool, &'static str> {
    if !opening.may_offer(db, goal.def(db)) {
        return Ok(false);
    }
    let scope = opening.scope(db);
    let assumptions = PredicateListId::empty_list(db);
    let families = crate::core::meta::derive_families(db, goal, scope, assumptions);
    let (offers, problem) =
        matching_provider_headers(db, scope, assumptions, opening.provider_ty(db), &families);
    if !offers.is_empty() {
        Ok(true)
    } else if let Some(problem) = problem {
        Err(problem)
    } else {
        Ok(false)
    }
}

type OfferedHeader<'db> = (OfferHead<'db>, ImplementorOrigin<'db>);

fn provider_headers<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    provider_ty: TyId<'db>,
) -> (Vec<OfferedHeader<'db>>, Option<&'static str>) {
    let Some(mut search) = ProviderSearch::new(db, scope, PredicateListId::empty_list(db)) else {
        return (Vec::new(), None);
    };
    (search.headers(provider_ty), search.problem)
}

/// Discover the concrete protocol keys for one elaborated goal. Every key
/// uses the same inventory and the search budget spans the whole request.
fn matching_provider_headers<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
    families: &[TyId<'db>],
) -> (
    Vec<(TyId<'db>, ImplementorOrigin<'db>)>,
    Option<&'static str>,
) {
    let Some(mut search) = ProviderSearch::new(db, scope, assumptions) else {
        return (Vec::new(), None);
    };
    let mut offers = Vec::new();
    for &family in families {
        search.wanted = Some(Canonical::new(db, family));
        offers.extend(
            search
                .headers(provider_ty)
                .into_iter()
                .map(|(_, origin)| (family, origin)),
        );
    }
    (offers, search.problem)
}

/// A defaulted goal can fit distinct ordinary protocol keys. Resolve this
/// choice from the matching headers before proving any provider prerequisite.
/// Once the key is chosen, the ordinary call supplies its proof and body.
pub(crate) fn select_derive_family<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
    goal: TraitInstId<'db>,
) -> Result<TyId<'db>, String> {
    let families = crate::core::meta::derive_families(db, goal, scope, assumptions);
    if let [family] = families.as_slice() {
        return Ok(*family);
    }
    let (offers, problem) =
        matching_provider_headers(db, scope, assumptions, provider_ty, &families);
    if let Some(problem) = problem {
        return Err(problem.into());
    }
    let matching = offers
        .into_iter()
        .map(|(family, _)| family)
        .collect::<IndexSet<_>>();
    super::derive::choose_family(db, provider_ty, &families, matching.into_iter().collect())
}

/// Structural domain matching for the ordinary Derive protocol. Only a bare
/// family variable can be restricted by same-family provider bounds. All other
/// prerequisites are obligations of the chosen application.
struct ProviderSearch<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    derive: Trait<'db>,
    assumptions: PredicateListId<'db>,
    wanted: Option<Canonical<TyId<'db>>>,
    active: Vec<(TyId<'db>, ImplementorOrigin<'db>)>,
    remaining: usize,
    problem: Option<&'static str>,
}

impl<'db> ProviderSearch<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<Self> {
        Some(Self {
            db,
            scope,
            derive: super::super::corelib::resolve_core_trait(db, scope, &["derive", "Derive"])?,
            assumptions,
            wanted: None,
            active: Vec::new(),
            remaining: 128,
            problem: None,
        })
    }

    fn headers(&mut self, receiver: TyId<'db>) -> Vec<OfferedHeader<'db>> {
        use super::super::{
            normalize::normalize_ty,
            trait_def::impls_for_trait_in_ingots,
            ty_def::{TyData, TyFlags},
            unify::UnificationTable,
        };

        let db = self.db;
        let receiver = if receiver.flags(db).contains(TyFlags::HAS_PROJECTION) {
            normalize_ty(db, receiver, self.scope, self.assumptions)
        } else {
            receiver
        };
        if receiver
            .flags(db)
            .intersects(TyFlags::HAS_INVALID | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION)
        {
            self.problem = Some(
                "scoped derive could not determine the type of a provider required by its forwarding bounds",
            );
            return Vec::new();
        }
        // The index uses only the Derive declaration and the receiver's ingot.
        let inventory = TraitInstId::new_simple(db, self.derive, vec![receiver, TyId::unit(db)]);
        let (primary, secondary) = super::TraitSolveCx::new(db, self.scope, Tier::All)
            .search_ingots_for_trait_inst(db, inventory);
        let mut headers = Vec::new();
        for &header in impls_for_trait_in_ingots(
            db,
            primary,
            secondary,
            Canonical::new(db, inventory),
            Tier::All,
            self.scope.ingot(db),
        ) {
            let mut table = UnificationTable::new(db);
            let candidate = table.instantiate_with_fresh_vars(header);
            if table
                .unify_header(candidate.trait_inst(db).self_ty(db), receiver)
                .is_err()
            {
                continue;
            }
            if self.remaining == 0 {
                self.problem = Some("scoped derive provider search exceeded its work limit");
                break;
            }
            self.remaining -= 1;
            let Some(&family) = candidate.trait_inst(db).args(db).get(1) else {
                continue;
            };
            let family = family.fold_with(db, &mut table);
            let symbolic = matches!(family.data(db), TyData::TyVar(_));
            let restrictors: Vec<_> = if symbolic {
                candidate
                    .constraints(db)
                    .list(db)
                    .iter()
                    .copied()
                    .filter(|condition| {
                        condition.def(db) == self.derive
                            && condition
                                .args(db)
                                .get(1)
                                .is_some_and(|key| key.fold_with(db, &mut table) == family)
                    })
                    .collect()
            } else {
                Vec::new()
            };
            if let Some(wanted) = self.wanted {
                let wanted = wanted.extract_identity(&mut table);
                let requested = TraitInstId::new_simple(db, self.derive, vec![receiver, wanted]);
                if super::table_solver::match_header_args(
                    db,
                    &mut table,
                    candidate.trait_inst(db),
                    requested,
                    self.scope,
                    [self.assumptions; 2],
                )
                .is_err()
                {
                    continue;
                }
            }
            let family = family.fold_with(db, &mut table);
            let family = if family.has_projection(db) {
                normalize_ty(db, family, self.scope, self.assumptions)
            } else {
                family
            };
            let head = match family.data(db) {
                TyData::TraitFamily(family_id)
                    if family.has_var(db) || family.has_param(db) || family.has_projection(db) =>
                {
                    OfferHead::Trait(family_id.def(db))
                }
                TyData::TraitFamily(_) => OfferHead::Family(family),
                TyData::TyVar(_) => OfferHead::Any,
                _ => continue,
            };
            let origin = candidate.origin(db);
            let visit = (receiver, origin);
            if self.active.contains(&visit) {
                continue;
            }
            self.active.push(visit);
            let mut domain = IndexSet::from_iter([head]);
            for restriction in restrictors {
                let other = restriction.self_ty(db).fold_with(db, &mut table);
                let offered = self.headers(other);
                domain = domain
                    .into_iter()
                    .flat_map(|lhs| {
                        offered
                            .iter()
                            .filter_map(move |(rhs, _)| lhs.intersect(db, *rhs))
                    })
                    .collect();
                if domain.is_empty() {
                    break;
                }
            }
            self.active.pop();
            headers.extend(domain.into_iter().map(|head| (head, origin)));
        }
        headers
    }
}

/// The demands met by running the instance `key`: those of its own body and,
/// transitively, of the bodies of the calls it makes and of the constants it
/// reads. Each of these bodies runs, so an evaluation whose result depends on
/// a generated implementation is not ready while any of them waits for one.
/// An instance past the instance limits contributes its own body's demands,
/// and its calls are not followed.
pub fn evaluation_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> GenerationDemands {
    instance_demands(db, [key])
}

fn instance_demands<'db>(
    db: &'db dyn HirAnalysisDb,
    roots: impl IntoIterator<Item = SemanticInstanceKey<'db>>,
) -> GenerationDemands {
    use super::super::ty_check::checked_body_demands;
    let mut demands = GenerationDemands::default();
    let mut pending: std::collections::VecDeque<_> = roots.into_iter().collect();
    let mut seen = std::collections::HashSet::new();
    while let Some(key) = pending.pop_front() {
        if !seen.insert(key) {
            continue;
        }
        demands.extend(&checked_body_demands(db, key.owner(db)));
        let instance = SemanticInstance::new(db, key);
        // An instance past the instance limits is rejected as itself where
        // it is checked; its calls need not end, so they are not followed.
        if crate::analysis::semantic::instance::instance_limit_violation(db, instance).is_some() {
            continue;
        }
        demands.requests(db, instance.codec_generation_requests(db));
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
        written_const_bodies_of,
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
    let scope = opening.scope(db);
    let assumptions = PredicateListId::empty_list(db);
    let families = crate::core::meta::derive_families(db, goal, scope, assumptions);
    let (headers, _) = matching_provider_headers(db, scope, assumptions, provider_ty, &families);
    let origins: IndexSet<_> = headers.into_iter().map(|(_, origin)| origin).collect();
    for origin in origins {
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
            owners.extend(
                written_const_bodies_of(db, adt.as_item())
                    .into_iter()
                    .map(|(body, expected)| BodyOwner::AnonConstBody { body, expected }),
            );
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
    preparation_demands_for_modules(db, [module])
}

pub fn preparation_demands_for_modules<'db>(
    db: &'db dyn HirAnalysisDb,
    modules: impl IntoIterator<Item = crate::hir_def::TopLevelMod<'db>>,
) -> Vec<GenerationDemand> {
    preparation_demands_with_dependencies(db, modules, [])
}

/// A concrete call can require a scoped provider opened in a dependency.
/// The driver supplies only modules in this ingot's dependency closure.
pub fn preparation_demands_with_dependencies<'db>(
    db: &'db dyn HirAnalysisDb,
    modules: impl IntoIterator<Item = crate::hir_def::TopLevelMod<'db>>,
    dependencies: impl IntoIterator<Item = crate::hir_def::TopLevelMod<'db>>,
) -> Vec<GenerationDemand> {
    use super::super::ty_check::{BodyOwner, checked_body_demands, infer_body, module_body_owners};

    let modules: Vec<_> = modules.into_iter().collect();
    let dependencies: Vec<_> = dependencies.into_iter().collect();
    let owners: Vec<_> = modules
        .iter()
        .flat_map(|&module| module_body_owners(db, module))
        .collect();
    let mut demands = GenerationDemands::default();
    for &owner in &owners {
        demands.extend(&checked_body_demands(db, owner));
    }
    if owners
        .iter()
        .copied()
        .chain(
            dependencies
                .iter()
                .flat_map(|&module| module_body_owners(db, module)),
        )
        .any(|owner| infer_body(db, owner).1.has_scoped_provider_openings())
    {
        let roots = owners.into_iter().filter_map(|owner| {
            if owner.body(db).is_none()
                || !matches!(
                    owner,
                    BodyOwner::Func(..)
                        | BodyOwner::ContractInit { .. }
                        | BodyOwner::ContractRecvArm { .. }
                )
            {
                return None;
            }
            root_semantic_instance_key(db, owner).ok()
        });
        let files: std::collections::HashSet<_> = modules
            .iter()
            .chain(&dependencies)
            .filter_map(|module| module.file(db).url(db))
            .collect();
        // An unrelated workspace root contributes no openings.
        for demand in instance_demands(db, roots).into_vec() {
            if files.contains(&demand.opening.file) {
                demands.push(demand);
            }
        }
    }
    demands.into_vec()
}
