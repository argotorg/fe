use common::indexmap::IndexSet;

use crate::{
    analysis::{HirAnalysisDb, name_resolution::Tier},
    hir_def::{Trait, scope_graph::ScopeId},
};

use super::{
    super::{
        canonical::Canonical,
        fold::TyFoldable,
        normalize::normalize_ty,
        trait_def::{ImplementorOrigin, TraitInstId, impls_for_trait_in_ingots},
        ty_def::{TyData, TyFlags, TyId},
        unify::UnificationTable,
    },
    PredicateListId,
    table_solver::match_header_type,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProviderSearchBudget {
    Written,
    Scoped,
}

/// A defaulted goal can fit distinct ordinary provider keys. Determine
/// which keys the provider offers before proving its ordinary prerequisites.
pub(crate) fn select_family<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    derive: Trait<'db>,
    provider: TyId<'db>,
    goal: TraitInstId<'db>,
    budget: ProviderSearchBudget,
) -> Result<TyId<'db>, String> {
    let families = crate::core::meta::derive_families(db, goal, scope, assumptions);
    if let [family] = families.as_slice() {
        return Ok(*family);
    }
    let matching = ProviderSearch::new(db, scope, assumptions, derive, budget)
        .matching_headers(provider, &families)?
        .into_iter()
        .map(|(family, _)| family)
        .collect::<IndexSet<_>>();
    choose_family(db, provider, &families, matching.into_iter().collect())
}

fn choose_family<'db>(
    db: &'db dyn HirAnalysisDb,
    provider: TyId<'db>,
    families: &[TyId<'db>],
    matching: Vec<TyId<'db>>,
) -> Result<TyId<'db>, String> {
    if matching.len() == 1 {
        return Ok(matching[0]);
    }
    let provider = provider.pretty_print(db);
    if matching.is_empty() {
        let family = families[0].pretty_print(db);
        return Err(format!(
            "`{provider}` has no `impl Derive<{family}>` that applies"
        ));
    }
    let families = matching
        .into_iter()
        .map(|family| format!("`Derive<{}>`", family.pretty_print(db)))
        .collect::<Vec<_>>()
        .join(", ");
    Err(format!(
        "`{provider}` has multiple derive families for the same trait application: {families}"
    ))
}

/// The head information available before a goal is known. A closed family
/// can be compared eagerly. An open head keeps only a name hint.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum OfferHead<'db> {
    Family(TyId<'db>),
    Trait(Trait<'db>),
    Any,
}

impl<'db> OfferHead<'db> {
    pub(super) fn trait_hint(self, db: &'db dyn HirAnalysisDb) -> Option<Trait<'db>> {
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

type OfferedHeader<'db> = (OfferHead<'db>, ImplementorOrigin<'db>);

/// Structural domain matching for the ordinary Derive protocol. Only a bare
/// family variable can be restricted by same-family provider bounds. All other
/// prerequisites are obligations of the chosen application.
pub(super) struct ProviderSearch<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    derive: Trait<'db>,
    assumptions: PredicateListId<'db>,
    wanted: Option<Canonical<TyId<'db>>>,
    active: Vec<(TyId<'db>, ImplementorOrigin<'db>)>,
    remaining: usize,
    budget: ProviderSearchBudget,
}

impl<'db> ProviderSearch<'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        derive: Trait<'db>,
        budget: ProviderSearchBudget,
    ) -> Self {
        Self {
            db,
            scope,
            derive,
            assumptions,
            wanted: None,
            active: Vec::new(),
            remaining: 128,
            budget,
        }
    }

    fn spend(&mut self) -> Result<(), &'static str> {
        self.remaining = self
            .remaining
            .checked_sub(1)
            .ok_or("derive provider search exceeded its work limit")?;
        Ok(())
    }

    pub(super) fn matching_headers(
        &mut self,
        receiver: TyId<'db>,
        families: &[TyId<'db>],
    ) -> Result<Vec<(TyId<'db>, ImplementorOrigin<'db>)>, &'static str> {
        let mut offers = Vec::new();
        for &family in families {
            self.wanted = Some(Canonical::new(self.db, family));
            offers.extend(
                self.headers(receiver)?
                    .into_iter()
                    .map(|(_, origin)| (family, origin)),
            );
        }
        Ok(offers)
    }

    pub(super) fn headers(
        &mut self,
        receiver: TyId<'db>,
    ) -> Result<Vec<OfferedHeader<'db>>, &'static str> {
        let db = self.db;
        let receiver = if receiver.flags(db).contains(TyFlags::HAS_PROJECTION) {
            normalize_ty(db, receiver, self.scope, self.assumptions)
        } else {
            receiver
        };
        let flags = receiver.flags(db);
        let undetermined = || {
            Err(
                "derive could not determine the type of a provider required by its forwarding bounds",
            )
        };
        if flags.contains(TyFlags::HAS_INVALID) {
            return undetermined();
        }
        // The index uses only the Derive declaration and the receiver's ingot.
        let inventory = TraitInstId::new_simple(db, self.derive, vec![receiver, TyId::unit(db)]);
        let (primary, secondary) = super::TraitSolveCx::new(db, self.scope, Tier::All)
            .search_ingots_for_trait_inst(db, inventory);
        let inventory_headers = impls_for_trait_in_ingots(
            db,
            primary,
            secondary,
            Canonical::new(db, inventory),
            Tier::All,
            self.scope.ingot(db),
        );
        if flags.intersects(TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION) {
            // A receiver that is not settled is a provider only if some
            // header's receiver could match it once settled. When none can,
            // the receiver is no provider and offers nothing.
            let may_match = inventory_headers.iter().any(|&header| {
                let mut table = UnificationTable::new(db);
                let candidate = table.instantiate_with_fresh_vars(header);
                let header_base = candidate.trait_inst(db).self_ty(db).base_ty(db);
                let receiver_base = receiver.base_ty(db);
                header_base.is_ty_var(db)
                    || receiver_base.is_ty_var(db)
                    || receiver_base.has_projection(db)
                    || header_base == receiver_base
            });
            return if may_match {
                undetermined()
            } else {
                Ok(Vec::new())
            };
        }
        let mut headers = Vec::new();
        for &header in inventory_headers {
            let mut table = UnificationTable::new(db);
            let mut candidate = table.instantiate_with_fresh_vars(header);
            let candidate_receiver = candidate.trait_inst(db).self_ty(db);
            let receiver_matches = match_header_type(
                db,
                &mut table,
                candidate_receiver,
                receiver,
                self.scope,
                self.assumptions,
            )
            .is_ok();
            if !receiver_matches {
                // A known family may bind an argument needed by a receiver
                // projection. Retry with the whole head in a fresh table.
                if self.wanted.is_none() || !candidate_receiver.has_projection(db) {
                    continue;
                }
                table = UnificationTable::new(db);
                candidate = table.instantiate_with_fresh_vars(header);
            }
            if receiver_matches && self.budget == ProviderSearchBudget::Scoped {
                self.spend()?;
            }
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
            if !receiver_matches && self.budget == ProviderSearchBudget::Scoped {
                self.spend()?;
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
                if self.budget == ProviderSearchBudget::Written {
                    self.spend()?;
                }
                let offered = self.headers(other)?;
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
        Ok(headers)
    }
}
