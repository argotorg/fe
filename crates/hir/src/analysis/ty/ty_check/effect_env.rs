use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;
use std::iter;

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::effects::{
    BarrierReason, EffectFamily, EffectForwarder, EffectQuery, EffectQueryMode, EffectWitness,
    ForwardedEffectKey, ForwardedTraitKey, KeyedEffectEntry, StoredEffectKey, StoredTraitKey,
    effect_family_for_trait,
    elaborate::{contains_projection_or_invalid_query_state, query_contains_unresolved_inference},
    forwarded_trait_key_is_well_formed, forwarded_type_key_is_well_formed,
    match_::{KeyMatchCommit, patterns_overlap, query_matches_forwarder, query_matches_witness},
    normalize_effect_identity_trait, stored_trait_key_is_rigid, stored_type_key_is_rigid,
};

use super::{TyChecker, env::ProvidedEffect};
use crate::analysis::ty::{trait_def::TraitInstId, trait_resolution::PredicateListId};
use crate::hir_def::{ModeMarker, scope_graph::ScopeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrameLookupResult<'db> {
    Unkeyed {
        providers: SmallVec<[ProvidedEffect<'db>; 2]>,
    },
    KeyedMatched {
        entries: Box<SmallVec<[MatchedKeyedEntry<'db>; 2]>>,
        blocked_by_barrier: bool,
        barrier_reason: Option<BarrierReason<'db>>,
    },
    KeyedFamily {
        entries: Box<SmallVec<[MatchedKeyedEntry<'db>; 2]>>,
        providers: Box<SmallVec<[ProvidedEffect<'db>; 2]>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchedWitness<'db> {
    pub witness: EffectWitness<'db, ProvidedEffect<'db>>,
    pub key_commit: KeyMatchCommit<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchedForwarder<'db> {
    pub forwarder: EffectForwarder<'db, ProvidedEffect<'db>>,
    pub key_commit: KeyMatchCommit<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchedKeyedEntry<'db> {
    Witness(MatchedWitness<'db>),
    Forwarder(MatchedForwarder<'db>),
}

type KeyedEntries<'db> =
    FxHashMap<EffectFamily<'db>, SmallVec<[KeyedEffectEntry<'db, ProvidedEffect<'db>>; 2]>>;

#[derive(Debug, Clone, Default)]
pub struct EffectFrame<'db> {
    keyed_by_family: KeyedEntries<'db>,
    /// The supertraits of the frame's trait effects, which each provides
    /// from its own provider: `uses (io: mut Stdio)` provides `Stdout`,
    /// `Stderr` and `Stdin`. A frame's own entry of a trait shadows them.
    implied_by_family: KeyedEntries<'db>,
    unkeyed: SmallVec<[ProvidedEffect<'db>; 2]>,
}

impl<'db> EffectFrame<'db> {
    /// The frame's keyed entries that match `query`, and the reason of a
    /// barrier among them that blocks it. The frame's own entries shadow the
    /// supertraits it implies: those match only when none of its own does.
    fn match_keyed(
        &self,
        query: &EffectQuery<'db>,
        tc: &mut TyChecker<'db>,
    ) -> (
        SmallVec<[MatchedKeyedEntry<'db>; 2]>,
        Option<BarrierReason<'db>>,
    ) {
        let family = query.key.clone().family();
        let mut entries = SmallVec::new();
        let mut barrier_reason = None;
        for family_entries in [&self.keyed_by_family, &self.implied_by_family] {
            for entry in family_entries.get(&family).into_iter().flatten().cloned() {
                match entry {
                    KeyedEffectEntry::Witness(witness) => {
                        if let Some(key_commit) =
                            query_matches_witness(tc, &query.key, &witness.key, witness.provider.ty)
                        {
                            entries.push(MatchedKeyedEntry::Witness(MatchedWitness {
                                witness,
                                key_commit,
                            }));
                        }
                    }
                    KeyedEffectEntry::Forwarder(forwarder) => {
                        if let Some(key_commit) =
                            query_matches_forwarder(tc, &query.key, &forwarder.key)
                        {
                            entries.push(MatchedKeyedEntry::Forwarder(MatchedForwarder {
                                forwarder,
                                key_commit,
                            }));
                        }
                    }
                    KeyedEffectEntry::Barrier(barrier) => {
                        if patterns_overlap(tc.db, &query.key, &barrier.pattern) {
                            barrier_reason = Some(barrier.reason);
                        }
                    }
                }
            }
            if !entries.is_empty() || barrier_reason.is_some() {
                break;
            }
        }
        (entries, barrier_reason)
    }

    /// Adds `entry` and, for a trait key, the supertraits the trait implies
    /// of its provider, as the trait solver elaborates them. `scope` and
    /// `assumptions` are those the key was resolved in, which normalize the
    /// supertraits' keys: with `trait Derived<T: HasItem>: Base<T::Item>`,
    /// `Derived<Concrete>` implies `Base<u8>`, not `Base<Concrete::Item>`.
    fn insert(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        entry: KeyedEffectEntry<'db, ProvidedEffect<'db>>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) {
        let (family, trait_key) = match &entry {
            KeyedEffectEntry::Witness(witness) => (
                witness.key.clone().family(),
                match &witness.key {
                    StoredEffectKey::Trait(key) => Some((
                        key.def,
                        &key.args_no_self,
                        &key.assoc_bindings,
                        witness.provider,
                    )),
                    StoredEffectKey::Type(_) => None,
                },
            ),
            KeyedEffectEntry::Forwarder(forwarder) => (
                forwarder.key.clone().family(),
                match &forwarder.key {
                    ForwardedEffectKey::Trait(key) => Some((
                        key.def,
                        &key.args_no_self,
                        &key.assoc_bindings,
                        forwarder.provider,
                    )),
                    ForwardedEffectKey::Type(_) => None,
                },
            ),
            KeyedEffectEntry::Barrier(barrier) => (barrier.pattern.clone().family(), None),
        };
        if let Some((def, args_no_self, assoc_bindings, provider)) = trait_key {
            let args: Vec<_> = iter::once(provider.ty)
                .chain(args_no_self.iter().copied())
                .collect();
            let bindings: IndexMap<_, _> = assoc_bindings.iter().copied().collect();
            let key = TraitInstId::new(db, def, args, bindings, ModeMarker::None);
            let implied = PredicateListId::new(db, vec![key])
                .extend_all_bounds(db)
                .list(db)
                .iter()
                .filter(|inst| **inst != key && inst.self_ty(db) == provider.ty)
                .map(|inst| {
                    let inst = normalize_effect_identity_trait(db, *inst, scope, assumptions, None);
                    let def = inst.def(db);
                    let args_no_self = inst.args(db)[1..].iter().copied().collect();
                    let assoc_bindings = inst.assoc_ty_bindings(db).into_iter().collect();
                    let family = effect_family_for_trait(def);
                    let entry = match &entry {
                        KeyedEffectEntry::Witness(_) => KeyedEffectEntry::Witness(EffectWitness {
                            key: StoredEffectKey::Trait(StoredTraitKey {
                                def,
                                args_no_self,
                                assoc_bindings,
                                family,
                            }),
                            provider,
                        }),
                        _ => KeyedEffectEntry::Forwarder(EffectForwarder {
                            key: ForwardedEffectKey::Trait(ForwardedTraitKey {
                                def,
                                args_no_self,
                                assoc_bindings,
                                family,
                            }),
                            provider,
                        }),
                    };
                    (family, entry)
                })
                .collect::<Vec<_>>();
            for (family, entry) in implied {
                self.implied_by_family
                    .entry(family)
                    .or_default()
                    .push(entry);
            }
        }
        self.keyed_by_family.entry(family).or_default().push(entry);
    }
}

#[derive(Clone)]
pub struct EffectEnv<'db> {
    frames: Vec<EffectFrame<'db>>,
}

impl<'db> EffectEnv<'db> {
    pub fn new() -> Self {
        Self {
            frames: vec![EffectFrame::default()],
        }
    }

    pub fn push_frame(&mut self) {
        self.frames.push(EffectFrame::default());
    }

    pub fn pop_frame(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    /// The frames of the `with` blocks being checked, innermost last.
    pub fn with_frames(&self) -> Vec<EffectFrame<'db>> {
        self.frames[1..].to_vec()
    }

    /// Reopens `frames`, which `with_frames` took, over the outermost frame,
    /// to resolve a call deferred from under them.
    pub fn reopen_with_frames(&mut self, frames: Vec<EffectFrame<'db>>) {
        debug_assert_eq!(self.frames.len(), 1, "no `with` block is open");
        self.frames.extend(frames);
    }

    pub fn close_with_frames(&mut self) {
        self.frames.truncate(1);
    }

    /// Adds `forwarder` to the outermost frame: an effect of the whole body
    /// (a closure's row component), below the `with` blocks it opens.
    pub fn insert_base_forwarder(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        forwarder: EffectForwarder<'db, ProvidedEffect<'db>>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) {
        self.frames[0].insert(
            db,
            KeyedEffectEntry::Forwarder(forwarder),
            scope,
            assumptions,
        );
    }

    fn current_frame_mut(&mut self) -> &mut EffectFrame<'db> {
        self.frames
            .last_mut()
            .expect("effect env must have at least one frame")
    }

    /// Adds `witness` to the innermost frame. `scope` and `assumptions` are
    /// those its key was resolved in; they normalize the supertraits it
    /// implies.
    pub fn insert_witness(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        witness: EffectWitness<'db, ProvidedEffect<'db>>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) {
        match &witness.key {
            StoredEffectKey::Type(key) => debug_assert!(stored_type_key_is_rigid(db, *key)),
            StoredEffectKey::Trait(key) => {
                debug_assert!(stored_trait_key_is_rigid(db, key.clone()))
            }
        }
        self.current_frame_mut()
            .insert(db, KeyedEffectEntry::Witness(witness), scope, assumptions);
    }

    /// Adds `forwarder` to the innermost frame. `scope` and `assumptions`
    /// are those its key was resolved in; they normalize the supertraits it
    /// implies.
    pub fn insert_forwarder(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        forwarder: EffectForwarder<'db, ProvidedEffect<'db>>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) {
        match &forwarder.key {
            ForwardedEffectKey::Type(key) => {
                debug_assert!(forwarded_type_key_is_well_formed(db, *key))
            }
            ForwardedEffectKey::Trait(key) => {
                debug_assert!(forwarded_trait_key_is_well_formed(db, key.clone()))
            }
        }
        self.current_frame_mut().insert(
            db,
            KeyedEffectEntry::Forwarder(forwarder),
            scope,
            assumptions,
        );
    }

    pub fn insert_barrier(
        &mut self,
        family: EffectFamily<'db>,
        barrier: crate::analysis::ty::effects::EffectBarrier<'db>,
    ) {
        self.current_frame_mut()
            .keyed_by_family
            .entry(family)
            .or_default()
            .push(KeyedEffectEntry::Barrier(barrier));
    }

    pub fn insert_unkeyed(&mut self, binding: ProvidedEffect<'db>) {
        self.current_frame_mut().unkeyed.push(binding);
    }

    pub fn lookup_effect_frames(
        &self,
        query: &EffectQuery<'db>,
        tc: &mut TyChecker<'db>,
    ) -> Vec<FrameLookupResult<'db>> {
        match query.mode {
            EffectQueryMode::Precise => self.lookup_precise(query, tc),
            EffectQueryMode::FamilyFallback => self.lookup_family_fallback(query, tc),
        }
    }

    fn lookup_precise(
        &self,
        query: &EffectQuery<'db>,
        tc: &mut TyChecker<'db>,
    ) -> Vec<FrameLookupResult<'db>> {
        debug_assert!(
            !query_contains_unresolved_inference(tc.db, &query.key)
                && !contains_projection_or_invalid_query_state(tc.db, &query.key),
            "precise effect lookup requires a fully resolved query",
        );
        let mut out = Vec::new();
        for frame in self.frames.iter().rev() {
            let (entries, barrier_reason) = frame.match_keyed(query, tc);
            if !entries.is_empty() || barrier_reason.is_some() {
                out.push(FrameLookupResult::KeyedMatched {
                    entries: Box::new(entries),
                    blocked_by_barrier: barrier_reason.is_some(),
                    barrier_reason,
                });
                break;
            }

            if !frame.unkeyed.is_empty() {
                out.push(FrameLookupResult::Unkeyed {
                    providers: frame.unkeyed.clone(),
                });
            }
        }
        out
    }

    /// Each frame's keyed entries that match `query`, which is not fully
    /// resolved, and its unkeyed providers. Unlike `lookup_precise`, a
    /// barrier does not end the lookup.
    fn lookup_family_fallback(
        &self,
        query: &EffectQuery<'db>,
        tc: &mut TyChecker<'db>,
    ) -> Vec<FrameLookupResult<'db>> {
        let mut out = Vec::new();
        for frame in self.frames.iter().rev() {
            let (entries, _) = frame.match_keyed(query, tc);
            let providers = frame.unkeyed.clone();
            if !entries.is_empty() || !providers.is_empty() {
                out.push(FrameLookupResult::KeyedFamily {
                    entries: Box::new(entries),
                    providers: Box::new(providers),
                });
            }
        }
        out
    }
}
