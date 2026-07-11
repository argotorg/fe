use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::ConstTyData,
        effects::{
            EffectPatternKey, ForwardedEffectKey, ForwardedTraitKey, ForwardedTypeKey, PatternSlot,
            StoredEffectKey, StoredTraitKey, StoredTypeKey, TraitPatternKey, TypePatternKey,
            stored_trait_key_is_rigid, stored_type_key_is_rigid,
        },
        fold::{TyFoldable, TyFolder},
        layout_holes::layout_hole_fallback_ty,
        method_cmp::trait_effect_key_matches_with,
        trait_def::TraitInstId,
        ty_def::{InvalidCause, Kind, TyBase, TyData, TyId, TyVarSort},
        ty_lower::collect_generic_params,
        unify::UnificationTable,
    },
};
use crate::hir_def::{CallableDef, IdentId, Trait};
use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeyMatchCommit<'db> {
    QueryToType {
        query: TypePatternKey<'db>,
        actual: TyId<'db>,
    },
    WitnessType {
        query: TypePatternKey<'db>,
        witness: StoredTypeKey<'db>,
    },
    WitnessTrait {
        query: TraitPatternKey<'db>,
        witness: StoredTraitKey<'db>,
    },
    ForwarderType {
        query: TypePatternKey<'db>,
        forwarder: ForwardedTypeKey<'db>,
    },
    ForwarderTrait {
        query: TraitPatternKey<'db>,
        forwarder: ForwardedTraitKey<'db>,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum PatternSide {
    Left,
    Right,
}

pub fn query_matches_witness<'db>(
    tc: &mut crate::analysis::ty::ty_check::TyChecker<'db>,
    query: &EffectPatternKey<'db>,
    witness: &StoredEffectKey<'db>,
) -> Option<KeyMatchCommit<'db>> {
    // Witnesses are rigid authoritative providers. Matching is directional from the
    // query pattern into that stored key and must not persist body-local specialization.
    match witness {
        StoredEffectKey::Type(witness) => debug_assert!(stored_type_key_is_rigid(tc.db, *witness)),
        StoredEffectKey::Trait(witness) => {
            debug_assert!(stored_trait_key_is_rigid(tc.db, witness.clone()))
        }
    }
    match (query.clone(), witness.clone()) {
        (EffectPatternKey::Type(query), StoredEffectKey::Type(witness)) => {
            query_matches_stored_type(tc.db, query.clone(), witness)
                .then_some(KeyMatchCommit::WitnessType { query, witness })
        }
        (EffectPatternKey::Trait(query), StoredEffectKey::Trait(witness)) => {
            query_matches_stored_trait(tc.db, query.clone(), witness.clone())
                .then_some(KeyMatchCommit::WitnessTrait { query, witness })
        }
        _ => None,
    }
}

pub fn query_matches_forwarder<'db>(
    tc: &mut crate::analysis::ty::ty_check::TyChecker<'db>,
    query: &EffectPatternKey<'db>,
    forwarder: &ForwardedEffectKey<'db>,
) -> Option<KeyMatchCommit<'db>> {
    // Forwarders are body-local requirement schemas with persistent specialization.
    // Probe under a snapshot, then commit the chosen specialization into the real table later.
    let commit = match (query.clone(), forwarder.clone()) {
        (EffectPatternKey::Type(query), ForwardedEffectKey::Type(forwarder)) => {
            KeyMatchCommit::ForwarderType { query, forwarder }
        }
        (EffectPatternKey::Trait(query), ForwardedEffectKey::Trait(forwarder)) => {
            KeyMatchCommit::ForwarderTrait { query, forwarder }
        }
        _ => return None,
    };
    let snapshot = tc.snapshot_state();
    let ok = apply_key_match_commit(tc, commit.clone());
    tc.rollback_state(snapshot);
    ok.then_some(commit)
}

pub fn patterns_overlap<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: &EffectPatternKey<'db>,
    rhs: &EffectPatternKey<'db>,
) -> bool {
    // Barriers are shadow-only invalid keyed bindings, not providers. Overlap asks whether
    // the two patterns have any common instantiation, with each side owning its own slots.
    match (lhs.clone(), rhs.clone()) {
        (EffectPatternKey::Type(lhs), EffectPatternKey::Type(rhs)) => {
            type_patterns_overlap(db, lhs, rhs)
        }
        (EffectPatternKey::Trait(lhs), EffectPatternKey::Trait(rhs)) => {
            trait_patterns_overlap(db, lhs, rhs)
        }
        _ => false,
    }
}

pub fn apply_key_match_commit<'db>(
    tc: &mut crate::analysis::ty::ty_check::TyChecker<'db>,
    commit: KeyMatchCommit<'db>,
) -> bool {
    match commit {
        KeyMatchCommit::QueryToType { query, actual } => {
            query_type_matches_ty(tc.db, &mut tc.table, query, actual)
        }
        KeyMatchCommit::WitnessType { query, witness } => {
            let query = instantiate_type_pattern_in(tc.db, &mut tc.table, query);
            RigidStoredTypeMatcher::new(tc.db, &mut tc.table).matches_type(
                query,
                witness.carrier,
                true,
            )
        }
        KeyMatchCommit::WitnessTrait { query, witness } => {
            let query = instantiate_trait_pattern_in(tc.db, &mut tc.table, query);
            query_instantiated_trait_matches_stored(tc.db, &mut tc.table, query, witness)
        }
        KeyMatchCommit::ForwarderType { query, forwarder } => {
            query_type_matches_ty(tc.db, &mut tc.table, query, forwarder.carrier)
        }
        KeyMatchCommit::ForwarderTrait { query, forwarder } => {
            let query = instantiate_trait_pattern_in(tc.db, &mut tc.table, query);
            query_instantiated_trait_matches_forwarded(tc.db, &mut tc.table, query, forwarder)
        }
    }
}

pub fn instantiate_type_pattern_in<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    pattern: TypePatternKey<'db>,
) -> TyId<'db> {
    instantiate_type_pattern_in_with_side(db, table, pattern, PatternSide::Left)
}

fn instantiate_type_pattern_in_with_side<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    pattern: TypePatternKey<'db>,
    side: PatternSide,
) -> TyId<'db> {
    instantiate_slots_in_with_side(
        db,
        table,
        pattern.carrier,
        pattern.slots.entries.as_slice(),
        side,
    )
}

pub fn instantiate_trait_pattern_in<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    pattern: TraitPatternKey<'db>,
) -> TraitInstId<'db> {
    instantiate_trait_pattern_in_with_side(db, table, pattern, PatternSide::Left)
}

pub fn instantiate_trait_pattern_in_with_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    pattern: TraitPatternKey<'db>,
) -> (TraitInstId<'db>, FxHashMap<TyId<'db>, TyId<'db>>) {
    let replacements = slot_replacements_for_side(
        db,
        table,
        pattern.slots.entries.as_slice(),
        PatternSide::Left,
    );
    let reverse_bindings = replacements
        .iter()
        .map(|(placeholder, instantiated)| (*instantiated, *placeholder))
        .collect();
    (
        instantiate_trait_pattern_with_replacements(db, pattern, &replacements),
        reverse_bindings,
    )
}

fn instantiate_trait_pattern_in_with_side<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    pattern: TraitPatternKey<'db>,
    side: PatternSide,
) -> TraitInstId<'db> {
    let replacements =
        slot_replacements_for_side(db, table, pattern.slots.entries.as_slice(), side);
    instantiate_trait_pattern_with_replacements(db, pattern, &replacements)
}

fn instantiate_trait_pattern_with_replacements<'db>(
    db: &'db dyn HirAnalysisDb,
    pattern: TraitPatternKey<'db>,
    replacements: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> TraitInstId<'db> {
    let self_ty = TyId::invalid(db, InvalidCause::Other);
    let mut args = vec![self_ty];
    args.extend(
        pattern
            .args_no_self
            .iter()
            .map(|ty| apply_slot_replacements(db, *ty, replacements)),
    );
    let assoc = pattern
        .assoc_bindings
        .iter()
        .map(|(name, ty)| (*name, apply_slot_replacements(db, *ty, replacements)))
        .collect::<IndexMap<_, _>>();
    TraitInstId::new(db, pattern.def, args, assoc)
}

fn type_patterns_overlap<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: TypePatternKey<'db>,
    rhs: TypePatternKey<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let lhs = instantiate_type_pattern_in_with_side(db, &mut table, lhs, PatternSide::Left);
    let lhs = instantiate_pattern_existentials_in(db, &mut table, lhs, true);
    let rhs = instantiate_type_pattern_in_with_side(db, &mut table, rhs, PatternSide::Right);
    let rhs = instantiate_pattern_existentials_in(db, &mut table, rhs, true);
    table.unify(lhs, rhs).is_ok()
}

fn trait_patterns_overlap<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: TraitPatternKey<'db>,
    rhs: TraitPatternKey<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let lhs = instantiate_trait_pattern_in_with_side(db, &mut table, lhs, PatternSide::Left);
    let lhs = instantiate_pattern_existentials_in(db, &mut table, lhs, true);
    let rhs = instantiate_trait_pattern_in_with_side(db, &mut table, rhs, PatternSide::Right);
    let rhs = instantiate_pattern_existentials_in(db, &mut table, rhs, true);
    trait_effect_key_matches_with(db, lhs, rhs, |lhs, rhs| table.unify(lhs, rhs).is_ok())
}

fn query_type_matches_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    query: TypePatternKey<'db>,
    actual: TyId<'db>,
) -> bool {
    let instantiated = instantiate_type_pattern_in(db, table, query);
    table.unify(instantiated, actual).is_ok()
}

fn query_matches_stored_type<'db>(
    db: &'db dyn HirAnalysisDb,
    query: TypePatternKey<'db>,
    witness: StoredTypeKey<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let query = instantiate_type_pattern_in(db, &mut table, query);
    let query = instantiate_pattern_existentials_in(db, &mut table, query, true);
    RigidStoredTypeMatcher::new(db, &mut table).matches_type(query, witness.carrier, true)
}

fn query_instantiated_trait_matches_stored<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    query: TraitInstId<'db>,
    witness: StoredTraitKey<'db>,
) -> bool {
    let witness = trait_inst_from_parts(
        db,
        witness.def,
        witness.args_no_self,
        witness.assoc_bindings,
    );
    let mut matcher = RigidStoredTypeMatcher::new(db, table);
    trait_effect_key_matches_with(db, query, witness, |lhs, rhs| {
        matcher.matches_type(lhs, rhs, true)
    })
}

fn query_instantiated_trait_matches_forwarded<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    query: TraitInstId<'db>,
    forwarder: ForwardedTraitKey<'db>,
) -> bool {
    let forwarder = trait_inst_from_parts(
        db,
        forwarder.def,
        forwarder.args_no_self,
        forwarder.assoc_bindings,
    );
    trait_effect_key_matches_with(db, query, forwarder, |lhs, rhs| {
        table.unify(lhs, rhs).is_ok()
    })
}

fn trait_inst_from_parts<'db>(
    db: &'db dyn HirAnalysisDb,
    def: Trait<'db>,
    args_no_self: impl IntoIterator<Item = TyId<'db>>,
    assoc_bindings: impl IntoIterator<Item = (IdentId<'db>, TyId<'db>)>,
) -> TraitInstId<'db> {
    let self_ty = TyId::invalid(db, InvalidCause::Other);
    TraitInstId::new(
        db,
        def,
        std::iter::once(self_ty)
            .chain(args_no_self)
            .collect::<Vec<_>>(),
        assoc_bindings.into_iter().collect::<IndexMap<_, _>>(),
    )
}

fn query_matches_stored_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    query: TraitPatternKey<'db>,
    witness: StoredTraitKey<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let instantiated = instantiate_trait_pattern_in(db, &mut table, query);
    let instantiated = instantiate_pattern_existentials_in(db, &mut table, instantiated, true);
    query_instantiated_trait_matches_stored(db, &mut table, instantiated, witness)
}

// Witness-only matcher: rigid stored keys may reuse hidden provider placeholders across matches,
// but that specialization must stay local to the probe/commit using this matcher.
struct RigidStoredTypeMatcher<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    table: &'a mut UnificationTable<'db>,
    hidden_actual_bindings: FxHashMap<TyId<'db>, TyId<'db>>,
    expected_to_hidden_actual: FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'a, 'db> RigidStoredTypeMatcher<'a, 'db> {
    fn new(db: &'db dyn HirAnalysisDb, table: &'a mut UnificationTable<'db>) -> Self {
        Self {
            db,
            table,
            hidden_actual_bindings: FxHashMap::default(),
            expected_to_hidden_actual: FxHashMap::default(),
        }
    }

    fn matches_type(
        &mut self,
        expected: TyId<'db>,
        actual: TyId<'db>,
        allow_actual_omitted_explicit_args: bool,
    ) -> bool {
        if let Some(actual_placeholder) = hidden_stored_placeholder(actual, self.db) {
            return self.bind_hidden_actual(expected, actual_placeholder);
        }
        if expected == actual {
            return true;
        }

        let (expected_base, expected_args) = expected.decompose_ty_app(self.db);
        let (actual_base, actual_args) = actual.decompose_ty_app(self.db);
        if expected_base != actual_base {
            return self.table.unify(expected, actual).is_ok();
        }

        let explicit_param_count =
            explicit_param_count_for_effect_identity_base(self.db, expected_base);
        let shared_explicit_args = expected_args
            .len()
            .min(actual_args.len())
            .min(explicit_param_count);
        for (lhs, rhs) in expected_args
            .iter()
            .copied()
            .zip(actual_args.iter().copied())
            .take(shared_explicit_args)
        {
            if !self.matches_type(lhs, rhs, allow_actual_omitted_explicit_args) {
                return false;
            }
        }

        if expected_args.len() < explicit_param_count {
            return true;
        }
        if actual_args.len() < explicit_param_count {
            return allow_actual_omitted_explicit_args;
        }

        expected_args.len() == actual_args.len()
            && expected_args
                .iter()
                .copied()
                .skip(explicit_param_count)
                .zip(actual_args.iter().copied().skip(explicit_param_count))
                .all(|(lhs, rhs)| self.matches_type(lhs, rhs, allow_actual_omitted_explicit_args))
    }

    fn bind_hidden_actual(&mut self, expected: TyId<'db>, actual: TyId<'db>) -> bool {
        if let Some(bound_expected) = self.hidden_actual_bindings.get(&actual).copied() {
            return self.table.unify(expected, bound_expected).is_ok();
        }
        if let Some(bound_actual) = self.expected_to_hidden_actual.get(&expected).copied() {
            return bound_actual == actual;
        }

        self.hidden_actual_bindings.insert(actual, expected);
        self.expected_to_hidden_actual.insert(expected, actual);
        true
    }
}

fn hidden_stored_placeholder<'db>(ty: TyId<'db>, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
    match ty.data(db) {
        TyData::TyParam(param) if param.is_implicit() => Some(ty),
        TyData::ConstTy(const_ty) if matches!(const_ty.data(db), ConstTyData::TyParam(param, _) if param.is_implicit()) => {
            Some(ty)
        }
        _ => None,
    }
}

fn instantiate_slots_in_with_side<'db, T>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    value: T,
    slots: &[PatternSlot<'db>],
    side: PatternSide,
) -> T
where
    T: TyFoldable<'db>,
{
    let replacements = slot_replacements_for_side(db, table, slots, side);
    apply_slot_replacements(db, value, &replacements)
}

fn slot_replacements_for_side<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    slots: &[PatternSlot<'db>],
    side: PatternSide,
) -> FxHashMap<TyId<'db>, TyId<'db>> {
    let replacements_by_slot = slots
        .iter()
        .map(|slot| ((side, slot.id), fresh_slot_value(db, table, *slot)))
        .collect::<FxHashMap<_, _>>();
    slots
        .iter()
        .map(|slot| (slot.placeholder, replacements_by_slot[&(side, slot.id)]))
        .collect()
}

fn apply_slot_replacements<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    replacements: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> T
where
    T: TyFoldable<'db>,
{
    if replacements.is_empty() {
        return value;
    }
    struct SlotReplacer<'a, 'db> {
        replacements: &'a FxHashMap<TyId<'db>, TyId<'db>>,
    }

    impl<'db> TyFolder<'db> for SlotReplacer<'_, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            self.replacements
                .get(&ty)
                .copied()
                .unwrap_or_else(|| ty.super_fold_with(db, self))
        }
    }

    value.fold_with(db, &mut SlotReplacer { replacements })
}

fn fresh_slot_value<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    slot: PatternSlot<'db>,
) -> TyId<'db> {
    let is_const_slot = matches!(slot.placeholder.data(db), TyData::ConstTy(..));
    let key = table.new_key(slot.fallback_ty.kind(db), TyVarSort::General);
    if is_const_slot {
        TyId::const_ty_var(db, slot.fallback_ty, key)
    } else {
        TyId::ty_var(db, TyVarSort::General, Kind::Star, key)
    }
}

fn instantiate_pattern_existentials_in<'db, T>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    value: T,
    replace_ty_vars: bool,
) -> T
where
    T: TyFoldable<'db>,
{
    instantiate_effect_key_identity_with_policy_in(
        db,
        table,
        value,
        EffectKeyIdentityInstantiationPolicy {
            replace_ty_vars,
            replace_holes: true,
            replace_implicit_const_params: true,
        },
    )
}

fn instantiate_effect_key_identity_with_policy_in<'db, T>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    value: T,
    policy: EffectKeyIdentityInstantiationPolicy,
) -> T
where
    T: TyFoldable<'db>,
{
    value.fold_with(
        db,
        &mut EffectKeyIdentityInstantiator {
            db,
            table,
            replacements: FxHashMap::default(),
            policy,
        },
    )
}

#[derive(Clone, Copy)]
struct EffectKeyIdentityInstantiationPolicy {
    replace_ty_vars: bool,
    replace_holes: bool,
    replace_implicit_const_params: bool,
}

struct EffectKeyIdentityInstantiator<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    table: &'a mut UnificationTable<'db>,
    replacements: FxHashMap<TyId<'db>, TyId<'db>>,
    policy: EffectKeyIdentityInstantiationPolicy,
}

impl<'a, 'db> EffectKeyIdentityInstantiator<'a, 'db> {
    fn fresh_var_for(&mut self, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyVar(var) => self.table.new_var(var.sort, &var.kind),
            TyData::ConstTy(const_ty) => {
                let fallback_ty = match const_ty.data(self.db) {
                    ConstTyData::TyVar(_, const_ty_ty) => *const_ty_ty,
                    ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(self.db, *hole_ty),
                    ConstTyData::TyParam(param, fallback_ty) if param.is_implicit() => *fallback_ty,
                    _ => unreachable!("unexpected non-placeholder in effect-key identity matcher"),
                };
                let key = self
                    .table
                    .new_key(fallback_ty.kind(self.db), TyVarSort::General);
                TyId::const_ty_var(self.db, fallback_ty, key)
            }
            _ => unreachable!("unexpected non-inference type in effect-key identity matcher"),
        }
    }
}

impl<'a, 'db> TyFolder<'db> for EffectKeyIdentityInstantiator<'a, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(expected) = effect_key_identity_const_expected_ty(self.db, ty) {
            let key = self
                .table
                .new_key(expected.kind(self.db), TyVarSort::General);
            return TyId::const_ty_var(self.db, expected, key);
        }

        if let Some(replacement) = self.replacements.get(&ty).copied() {
            return replacement;
        }

        let needs_replacement = match ty.data(self.db) {
            TyData::TyVar(_) => self.policy.replace_ty_vars,
            TyData::ConstTy(const_ty) => match const_ty.data(self.db) {
                ConstTyData::TyVar(..) => self.policy.replace_ty_vars,
                ConstTyData::Hole(..) => self.policy.replace_holes,
                ConstTyData::TyParam(param, _) => {
                    self.policy.replace_implicit_const_params && param.is_implicit()
                }
                _ => false,
            },
            _ => false,
        };
        if needs_replacement {
            let replacement = self.fresh_var_for(ty);
            self.replacements.insert(ty, replacement);
            return replacement;
        }

        ty.super_fold_with(db, self)
    }

    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        TyId::new(db, TyData::TyApp(abs, arg))
    }
}

fn explicit_param_count_for_effect_identity_base<'db>(
    db: &'db dyn HirAnalysisDb,
    base: TyId<'db>,
) -> usize {
    match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => adt.param_set(db).explicit_param_count(db),
        TyData::TyBase(TyBase::Func(func)) => match func {
            CallableDef::Func(def) => {
                collect_generic_params(db, (*def).into()).explicit_param_count(db)
            }
            CallableDef::VariantCtor(_) => 0,
        },
        _ => 0,
    }
}

fn effect_key_identity_const_expected_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    ty.invalid_cause(db)
        .and_then(|cause| match cause {
            InvalidCause::ConstTyExpected { expected } => Some(expected),
            _ => None,
        })
        .or_else(|| {
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return None;
            };
            match const_ty.ty(db).invalid_cause(db) {
                Some(InvalidCause::ConstTyExpected { expected }) => Some(expected),
                _ => None,
            }
        })
}
