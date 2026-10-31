use common::indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;

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
use crate::analysis::ty::{
    trait_def::{AssocBinding, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::TyId,
};
use crate::hir_def::{ModeMarker, Trait, scope_graph::ScopeId};

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
    fn insert_forwarder(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        forwarder: EffectForwarder<'db, ProvidedEffect<'db>>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) {
        if let ForwardedEffectKey::Trait(key) = &forwarder.key {
            for inst in super_trait_insts(
                db,
                key.def,
                &key.args_no_self,
                &key.assoc_bindings,
                forwarder.provider.ty,
                scope,
                assumptions,
            ) {
                let def = inst.def(db);
                self.implied_by_family
                    .entry(effect_family_for_trait(def))
                    .or_default()
                    .push(KeyedEffectEntry::Forwarder(EffectForwarder {
                        key: ForwardedEffectKey::Trait(ForwardedTraitKey {
                            def,
                            args_no_self: inst.args(db)[1..].iter().copied().collect(),
                            assoc_bindings: inst.assoc_ty_bindings(db).into_iter().collect(),
                            family: effect_family_for_trait(def),
                        }),
                        provider: forwarder.provider,
                    }));
            }
        }
        self.keyed_by_family
            .entry(forwarder.key.clone().family())
            .or_default()
            .push(KeyedEffectEntry::Forwarder(forwarder));
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
        self.frames[0].insert_forwarder(db, forwarder, scope, assumptions);
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
        let frame = self.current_frame_mut();
        if let StoredEffectKey::Trait(key) = &witness.key {
            for inst in super_trait_insts(
                db,
                key.def,
                &key.args_no_self,
                &key.assoc_bindings,
                witness.provider.ty,
                scope,
                assumptions,
            ) {
                let def = inst.def(db);
                frame
                    .implied_by_family
                    .entry(effect_family_for_trait(def))
                    .or_default()
                    .push(KeyedEffectEntry::Witness(EffectWitness {
                        key: StoredEffectKey::Trait(StoredTraitKey {
                            def,
                            args_no_self: inst.args(db)[1..].iter().copied().collect(),
                            assoc_bindings: inst.assoc_ty_bindings(db).into_iter().collect(),
                            family: effect_family_for_trait(def),
                        }),
                        provider: witness.provider,
                    }));
            }
        }
        frame
            .keyed_by_family
            .entry(witness.key.clone().family())
            .or_default()
            .push(KeyedEffectEntry::Witness(witness));
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
        self.current_frame_mut()
            .insert_forwarder(db, forwarder, scope, assumptions);
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
        let family = query.key.clone().family();
        for frame in self.frames.iter().rev() {
            let mut entries = SmallVec::new();
            let mut blocked_by_barrier = false;
            let mut barrier_reason = None;
            for family_entries in [&frame.keyed_by_family, &frame.implied_by_family] {
                for entry in family_entries.get(&family).into_iter().flatten().cloned() {
                    match entry {
                        KeyedEffectEntry::Witness(witness) => {
                            if let Some(key_commit) = query_matches_witness(
                                tc,
                                &query.key,
                                &witness.key,
                                witness.provider.ty,
                            ) {
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
                // The frame's own entries shadow the supertraits it implies.
                if !entries.is_empty() || blocked_by_barrier {
                    break;
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
        let family = query.key.clone().family();
        for frame in self.frames.iter().rev() {
            let family_entries = |entries: &KeyedEntries<'db>| {
                entries
                    .get(&family)
                    .into_iter()
                    .flatten()
                    .cloned()
                    .filter_map(|entry| match entry {
                        KeyedEffectEntry::Witness(witness) => {
                            Some(FamilyKeyedEntry::Witness(witness))
                        }
                        KeyedEffectEntry::Forwarder(forwarder) => {
                            Some(FamilyKeyedEntry::Forwarder(forwarder))
                        }
                        KeyedEffectEntry::Barrier(_) => None,
                    })
                    .collect::<SmallVec<[FamilyKeyedEntry<'db>; 2]>>()
            };
            // The frame's own entries shadow the supertraits it implies.
            let mut entries = family_entries(&frame.keyed_by_family);
            if entries.is_empty() {
                entries = family_entries(&frame.implied_by_family);
            }
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

/// The supertraits of the trait key `def` with `args_no_self` and
/// `assoc_bindings`, which `provider_ty` provides, and theirs in turn. Each
/// is normalized as an effect key in `scope` under `assumptions`, with its
/// subtrait as evidence: with `trait Derived<T: HasItem>: Base<T::Item>`,
/// `Derived<Concrete>` implies `Base<u8>`, not `Base<Concrete::Item>`.
fn super_trait_insts<'db>(
    db: &'db dyn HirAnalysisDb,
    def: Trait<'db>,
    args_no_self: &[TyId<'db>],
    assoc_bindings: &[(AssocBinding<'db>, TyId<'db>)],
    provider_ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> FxHashSet<TraitInstId<'db>> {
    let args: Vec<_> = std::iter::once(provider_ty)
        .chain(args_no_self.iter().copied())
        .collect();
    let bindings: IndexMap<_, _> = assoc_bindings.iter().copied().collect();
    let mut worklist = vec![TraitInstId::new(db, def, args, bindings, ModeMarker::None)];
    let mut supers = FxHashSet::default();
    while let Some(inst) = worklist.pop() {
        for super_trait in inst.def(db).super_traits(db) {
            let super_inst = normalize_effect_identity_trait(
                db,
                super_trait.instantiate(db, inst.args(db)),
                scope,
                assumptions,
                Some(inst),
            );
            if supers.insert(super_inst) {
                worklist.push(super_inst);
            }
        }
    }
    supers
}
