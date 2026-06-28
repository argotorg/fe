use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::effects::{
    BarrierReason, EffectFamily, EffectForwarder, EffectQuery, EffectQueryMode, EffectWitness,
    ForwardedEffectKey, KeyedEffectEntry, StoredEffectKey,
    elaborate::{contains_projection_or_invalid_query_state, query_contains_unresolved_inference},
    forwarded_trait_key_is_well_formed, forwarded_type_key_is_well_formed,
    match_::{KeyMatchCommit, patterns_overlap, query_matches_forwarder, query_matches_witness},
    stored_trait_key_is_rigid, stored_type_key_is_rigid,
};

use super::{TyChecker, env::ProvidedEffect};

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
        entries: Box<SmallVec<[FamilyKeyedEntry<'db>; 2]>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FamilyKeyedEntry<'db> {
    Witness(EffectWitness<'db, ProvidedEffect<'db>>),
    Forwarder(EffectForwarder<'db, ProvidedEffect<'db>>),
}

#[derive(Clone, Default)]
pub struct EffectFrame<'db> {
    keyed_by_family:
        FxHashMap<EffectFamily<'db>, SmallVec<[KeyedEffectEntry<'db, ProvidedEffect<'db>>; 2]>>,
    unkeyed: SmallVec<[ProvidedEffect<'db>; 2]>,
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

    fn current_frame_mut(&mut self) -> &mut EffectFrame<'db> {
        self.frames
            .last_mut()
            .expect("effect env must have at least one frame")
    }

    fn insert_keyed_entry(
        &mut self,
        family: EffectFamily<'db>,
        entry: KeyedEffectEntry<'db, ProvidedEffect<'db>>,
    ) {
        self.current_frame_mut()
            .keyed_by_family
            .entry(family)
            .or_default()
            .push(entry);
    }

    pub fn insert_witness(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        witness: EffectWitness<'db, ProvidedEffect<'db>>,
    ) {
        match &witness.key {
            StoredEffectKey::Type(key) => debug_assert!(stored_type_key_is_rigid(db, *key)),
            StoredEffectKey::Trait(key) => {
                debug_assert!(stored_trait_key_is_rigid(db, key.clone()))
            }
        }
        self.insert_keyed_entry(
            witness.key.clone().family(),
            KeyedEffectEntry::Witness(witness),
        );
    }

    pub fn insert_forwarder(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        forwarder: EffectForwarder<'db, ProvidedEffect<'db>>,
    ) {
        match &forwarder.key {
            ForwardedEffectKey::Type(key) => {
                debug_assert!(forwarded_type_key_is_well_formed(db, *key))
            }
            ForwardedEffectKey::Trait(key) => {
                debug_assert!(forwarded_trait_key_is_well_formed(db, key.clone()))
            }
        }
        self.insert_keyed_entry(
            forwarder.key.clone().family(),
            KeyedEffectEntry::Forwarder(forwarder),
        );
    }

    pub fn insert_barrier(
        &mut self,
        family: EffectFamily<'db>,
        barrier: crate::analysis::ty::effects::EffectBarrier<'db>,
    ) {
        self.insert_keyed_entry(family, KeyedEffectEntry::Barrier(barrier));
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
            EffectQueryMode::FamilyFallback => self.lookup_family_fallback(query),
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
            let mut entries = SmallVec::new();
            let mut blocked_by_barrier = false;
            let mut barrier_reason = None;
            if let Some(family_entries) = frame.keyed_by_family.get(&query.key.clone().family()) {
                for entry in family_entries.iter().cloned() {
                    match entry {
                        KeyedEffectEntry::Witness(witness) => {
                            if let Some(key_commit) =
                                query_matches_witness(tc, &query.key, &witness.key)
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
                                blocked_by_barrier = true;
                                barrier_reason = Some(barrier.reason.clone());
                            }
                        }
                    }
                }
            }

            if !entries.is_empty() || blocked_by_barrier {
                out.push(FrameLookupResult::KeyedMatched {
                    entries: Box::new(entries),
                    blocked_by_barrier,
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

    fn lookup_family_fallback(&self, query: &EffectQuery<'db>) -> Vec<FrameLookupResult<'db>> {
        let mut out = Vec::new();
        for frame in self.frames.iter().rev() {
            let entries = frame
                .keyed_by_family
                .get(&query.key.clone().family())
                .into_iter()
                .flat_map(|entries| entries.iter().cloned())
                .filter_map(|entry| match entry {
                    KeyedEffectEntry::Witness(witness) => Some(FamilyKeyedEntry::Witness(witness)),
                    KeyedEffectEntry::Forwarder(forwarder) => {
                        Some(FamilyKeyedEntry::Forwarder(forwarder))
                    }
                    KeyedEffectEntry::Barrier(_) => None,
                })
                .collect::<SmallVec<[FamilyKeyedEntry<'db>; 2]>>();
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

    /// An innermost-first snapshot of every in-scope provision's
    /// [`ProvidedEffect`], across all live frames. Frames are walked from the
    /// innermost (most recently pushed) outward; within a frame the unkeyed
    /// providers are followed by the providers backing the keyed witness /
    /// forwarder entries.
    ///
    /// This is the raw provision list captured onto a deferred
    /// [`TraitObligation`](super::env::TraitObligation) at enqueue time, so the
    /// obligation processor can consult the provisions that were in scope at the
    /// raise site even after every `with` / `uses` frame has been popped. It is
    /// a snapshot of provider *bindings* only — no key matching, no barriers, no
    /// effect-query semantics — because the obligation processor recognizes the
    /// `Evidence`-typed entries itself, by resolved identity.
    pub fn snapshot_provisions(&self) -> SmallVec<[ProvidedEffect<'db>; 4]> {
        let mut out = SmallVec::new();
        for frame in self.frames.iter().rev() {
            out.extend(frame.unkeyed.iter().copied());
            for entries in frame.keyed_by_family.values() {
                for entry in entries {
                    match entry {
                        KeyedEffectEntry::Witness(witness) => out.push(witness.provider),
                        KeyedEffectEntry::Forwarder(forwarder) => out.push(forwarder.provider),
                        KeyedEffectEntry::Barrier(_) => {}
                    }
                }
            }
        }
        out
    }
}
