//! Resource domains: the storage effects confer authority over.
//!
//! Each provider source (a contract field, an effect parameter of the body, a
//! root provider) is a domain, and a handle whose provider is unknown names a
//! dynamic domain. The storage layout makes distinct contract fields
//! disjoint. A function's or closure's effects are disjoint as its
//! parameters are, since its callers prove them so (`check_call`), unless
//! one provider may satisfy both their keys: `uses (a: T, b: U)` where `T`
//! may be `U`, or two trait keys a type may implement both of. Those are the
//! same place, whole, or disjoint, never a part of one another. A dynamic
//! domain, or one whose effect is unknown, may share storage with any other
//! of its space.
use std::cell::RefCell;

use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::SemanticInstance,
        ty::{
            instantiate_trait_self,
            provider::AddressSpace,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::BodyOwner,
            ty_def::{TyData, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
    semantic::{EffectRequirement, EffectRequirementKey, ProviderBinding, ProviderSource},
};

/// How two distinct domains of one space may share storage.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Alias {
    Disjoint,
    /// The same place, whole, or disjoint: places at overlapping paths of
    /// the two overlap.
    Whole,
    /// Any place of one may overlap any of the other.
    Any,
}

#[derive(Clone, Debug)]
struct Domain<'db> {
    /// The provider the domain is, or `None` for a dynamic domain.
    source: Option<ProviderSource<'db>>,
    space: Option<AddressSpace>,
    /// The keys of the body's effects the domain provides.
    keys: SmallVec<EffectRequirementKey<'db>, 1>,
}

pub(super) struct Domains<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    /// The body's effects, if they are a function's or a closure's, whose
    /// callers prove them disjoint.
    effects: Option<&'db [EffectRequirement<'db>]>,
    domains: Vec<Domain<'db>>,
    aliases: RefCell<FxHashMap<(u32, u32), Alias>>,
}

impl<'db> Domains<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, instance: SemanticInstance<'db>) -> Self {
        let effects = matches!(
            instance.key(db).owner(db),
            BodyOwner::Func(..) | BodyOwner::Closure { .. }
        )
        .then(|| instance.effect_requirements(db));
        Self {
            db,
            scope: instance.key(db).impl_env(db).normalization_scope(db),
            assumptions: instance.assumptions(db),
            effects,
            domains: Vec::new(),
            aliases: RefCell::default(),
        }
    }

    /// The domain of `source`, which provides the body's effect `effect`,
    /// if any.
    fn intern(
        &mut self,
        source: Option<ProviderSource<'db>>,
        space: Option<AddressSpace>,
        effect: Option<u32>,
    ) -> u32 {
        let key = effect.and_then(|effect| {
            self.effects?
                .iter()
                .find(|requirement| requirement.binding_idx == effect)
                .map(|requirement| requirement.key.clone())
        });
        let index = self
            .domains
            .iter()
            .position(|known| known.source == source && (source.is_some() || known.space == space))
            .unwrap_or_else(|| {
                self.domains.push(Domain {
                    source,
                    space,
                    keys: SmallVec::new(),
                });
                self.domains.len() - 1
            });
        if let Some(key) = key
            && !self.domains[index].keys.contains(&key)
        {
            self.domains[index].keys.push(key);
            self.aliases.get_mut().clear();
        }
        index as u32
    }

    /// The domain of a provider root, which provides the body's effect of
    /// the same slot.
    pub fn provider(&mut self, binding: &ProviderBinding<'db>) -> u32 {
        self.intern(
            Some(binding.source.clone()),
            Some(binding.semantics.address_space),
            Some(binding.provider_idx),
        )
    }

    /// The domain of the body's effect `effect`, provided by `source`.
    pub fn effect(
        &mut self,
        source: ProviderSource<'db>,
        space: Option<AddressSpace>,
        effect: u32,
    ) -> u32 {
        self.intern(Some(source), space, Some(effect))
    }

    pub fn dynamic(&mut self, space: Option<AddressSpace>) -> u32 {
        self.intern(None, space, None)
    }

    pub fn space(&self, domain: u32) -> Option<AddressSpace> {
        self.domains[domain as usize].space
    }

    /// How two distinct domains may share storage when their places lie in
    /// one space.
    pub fn alias(&self, lhs: u32, rhs: u32) -> Alias {
        let pair = (lhs.min(rhs), lhs.max(rhs));
        if let Some(&alias) = self.aliases.borrow().get(&pair) {
            return alias;
        }
        let (lhs, rhs) = (&self.domains[lhs as usize], &self.domains[rhs as usize]);
        let alias = match (&lhs.source, &rhs.source) {
            (None, _) | (_, None) => Alias::Any,
            (
                Some(ProviderSource::ContractField { .. }),
                Some(ProviderSource::ContractField { .. }),
            ) => Alias::Disjoint,
            _ if lhs.keys.is_empty() || rhs.keys.is_empty() => Alias::Any,
            _ if lhs.keys.iter().any(|lhs| {
                rhs.keys
                    .iter()
                    .any(|rhs| keys_may_share(self.db, self.scope, self.assumptions, lhs, rhs))
            }) =>
            {
                Alias::Whole
            }
            _ => Alias::Disjoint,
        };
        self.aliases.borrow_mut().insert(pair, alias);
        alias
    }
}

/// Whether one provider may satisfy effects of keys `lhs` and `rhs`: a type
/// key is provided by a place of its type, a trait key by one of a type
/// implementing it.
pub(crate) fn keys_may_share<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lhs: &EffectRequirementKey<'db>,
    rhs: &EffectRequirementKey<'db>,
) -> bool {
    match (lhs, rhs) {
        (EffectRequirementKey::Type(lhs), EffectRequirementKey::Type(rhs)) => {
            tys_may_be_equal(db, *lhs, *rhs)
        }
        (EffectRequirementKey::Type(ty), EffectRequirementKey::Trait(inst))
        | (EffectRequirementKey::Trait(inst), EffectRequirementKey::Type(ty)) => !matches!(
            is_goal_satisfiable(
                db,
                TraitSolveCx::new(db, scope).with_assumptions(assumptions),
                instantiate_trait_self(db, *inst, *ty),
            ),
            GoalSatisfiability::UnSat(_)
        ),
        // A type may implement any two traits.
        _ => true,
    }
}

/// Whether some substitution of their parameters, associated types and
/// other unknowns makes `lhs` and `rhs` one type.
fn tys_may_be_equal<'db>(db: &'db dyn HirAnalysisDb, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
    match (lhs.data(db), rhs.data(db)) {
        (TyData::TyApp(lhs_abs, lhs_arg), TyData::TyApp(rhs_abs, rhs_arg)) => {
            tys_may_be_equal(db, *lhs_abs, *rhs_abs) && tys_may_be_equal(db, *lhs_arg, *rhs_arg)
        }
        (TyData::TyBase(_) | TyData::TyApp(..), TyData::TyBase(_) | TyData::TyApp(..)) => {
            lhs == rhs
        }
        _ => true,
    }
}
