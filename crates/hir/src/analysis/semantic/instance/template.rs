use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::ConstTyData,
            fold::{TyFoldable, TyFolder},
            trait_def::{ImplementorId, TraitInstId},
            trait_resolution::PredicateListId,
            ty_check::{
                BodyOwner, EffectProviderSpecialization, TypedBody, check_anon_const_body,
                check_const_body, check_contract_init_body, check_contract_recv_arm_body,
                check_func_body,
            },
            ty_def::{TyData, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
};
use salsa::Update;
use salsa::plumbing::AsId;

#[derive(Clone, Debug)]
pub struct TypedBodyTemplate<'db> {
    pub owner: BodyOwner<'db>,
    pub body: TypedBody<'db>,
}

pub fn typed_body_template<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> TypedBodyTemplate<'db> {
    let typed_body = match owner {
        BodyOwner::Func(func) => check_func_body(db, func).1.clone(),
        BodyOwner::Const(const_) => check_const_body(db, const_).1.clone(),
        BodyOwner::AnonConstBody { body, expected } => {
            check_anon_const_body(db, body, expected).1.clone()
        }
        BodyOwner::ContractInit { contract } => check_contract_init_body(db, contract).1.clone(),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => check_contract_recv_arm_body(db, contract, recv_idx, arm_idx)
            .1
            .clone(),
    };

    TypedBodyTemplate {
        owner,
        body: typed_body,
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericSubst<'db> {
    #[return_ref]
    pub generic_args: Vec<TyId<'db>>,
}

impl<'db> GenericSubst<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }
}

// SEMANTIC-INSTANCE-KEY IDENTITY INVARIANT (rung 3.2 + cascade C3d/M3): `ImplEnv`
// is a field of the `#[salsa::interned]` `SemanticInstanceKey`, so its
// `Hash`/`Eq` form part of that key's interning identity (and salsa interns over
// ALL fields of an embedded value — there is no per-field "exclude from
// identity" attribute). `selected_implementors` is the per-goal map of impl
// overrides the typeck solver committed to at instantiation time, recorded so
// the MIR C1 rail consumes them as the resolution source and rung 3.3 can assert
// MIR re-resolution agrees.
//
// M3 WIDENING: the carrier was a single `Option<ImplementorId>` (one scope-
// selected override per instance). It is now a per-goal `Vec<(TraitInstId,
// ImplementorId)>` so a generic helper instance can carry the override for EACH
// goal a `with (<T as Trait>)` scope selected at the helper CALL SITE, letting
// the helper body's inner trait calls each look their override up BY GOAL. The
// single-Option behavior is preserved as the 0-or-1-entry case.
//
// EMPTY-ONLY CASCADE EXCEPTION (the byte-identity floor): the carrier folds into
// identity ONLY when NON-EMPTY. An EMPTY carrier hashes/compares/updates
// BYTE-IDENTICALLY to the pre-M3 `None` behavior (where the field was excluded
// entirely), so every instance that did not scope-select an impl — which today
// is every instance, since `const_ref.rs` carries an empty carrier for non-
// scope-selected calls — interns EXACTLY as before. A non-empty carrier DOES
// fold its `(goal, impl)` entries into identity: this is what makes the cascade
// observable, because a `with (<T as Trait>)` scoped call carries a non-empty
// carrier while the same call outside carries an empty one, and those two
// instances MUST mint distinct `SemanticInstanceKey`s (and distinct codegen
// symbols — see the matching empty==None discriminator in `stable_key.rs`) so
// they lower against different impls. Without folding the entries in, the
// override and the default would collide on one key and one symbol (a
// miscompile). An EMPTY carrier remains observationally identical to one
// differing only by an empty carrier — that is the byte-identity floor the
// manual `PartialEq`/`Eq`/`Hash`/`Update` below preserve (hence a plain struct
// with manual impls instead of `#[salsa::interned]`/`#[derive]`, which would
// always fold the field). The carrier is CANONICALLY ORDERED (sorted by goal —
// see `with_selected_implementors`) so two equal selection sets mint equal keys.
// `stable_key.rs` carries the SAME empty==None discriminator so the codegen-
// symbol identity tracks the interning identity exactly.
#[derive(Debug, Clone)]
pub struct ImplEnv<'db> {
    normalization_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    witnesses: Vec<TraitInstId<'db>>,
    /// The per-goal impl overrides the typeck solver committed to at
    /// instantiation time, when this `ImplEnv` belongs to an instance whose call
    /// was SCOPE-SELECTED (`with (<T as Trait>)`). EMPTY for any call that was
    /// not scope-selected (today every non-cascade call). Each entry is
    /// `(goal, override)`; the MIR C1 rail looks an override up BY GOAL. Folds
    /// into identity ONLY when NON-EMPTY (the empty-only cascade exception — see
    /// invariant above): an empty carrier is byte-identical to the pre-M3
    /// behavior; a non-empty carrier makes the scope selection observable by
    /// minting a distinct key/symbol. CANONICALLY ORDERED (sorted by goal) so
    /// equal selection sets mint equal keys. Consumed by the MIR C1 rail
    /// (`classify.rs`) as the resolution source.
    selected_implementors: Vec<(TraitInstId<'db>, ImplementorId<'db>)>,
}

impl<'db> PartialEq for ImplEnv<'db> {
    fn eq(&self, other: &Self) -> bool {
        // `selected_implementors` folds into equality ONLY when NON-EMPTY — see
        // the EMPTY-ONLY CASCADE EXCEPTION in the IDENTITY INVARIANT above. The
        // carrier is canonically ordered at construction, so direct `Vec`
        // equality gives exactly that: `[] == []` (the byte-identity floor — two
        // non-scope-selected envs are equal), two non-empty carriers equal iff
        // they carry the SAME `(goal, impl)` entries in the SAME canonical order,
        // and `[..] != []` (a scope-selected override is a distinct instance from
        // the unscoped default).
        self.normalization_scope == other.normalization_scope
            && self.assumptions == other.assumptions
            && self.witnesses == other.witnesses
            && self.selected_implementors == other.selected_implementors
    }
}

impl<'db> Eq for ImplEnv<'db> {}

impl<'db> std::hash::Hash for ImplEnv<'db> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.normalization_scope.hash(state);
        self.assumptions.hash(state);
        self.witnesses.hash(state);
        // EMPTY-ONLY: hash `selected_implementors` ONLY when NON-EMPTY, so an
        // EMPTY carrier hashes BYTE-IDENTICALLY to the pre-M3 behavior (where the
        // field was excluded entirely). A non-empty carrier mixes its
        // canonically-ordered `(goal, impl)` entries in so it lands on a distinct
        // bucket from the empty default. Consistent with `PartialEq` (`[]`
        // skip-vs-skip; equal non-empty carriers hash equally because they share
        // the same canonical order).
        if !self.selected_implementors.is_empty() {
            self.selected_implementors.hash(state);
        }
    }
}

unsafe impl<'db> Update for ImplEnv<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        // EMPTY-ONLY (consistent with `Eq`): an `ImplEnv` whose
        // `selected_implementors` is unchanged under the empty-only rule is NOT a
        // salsa change. `Vec` equality over the canonically-ordered carrier
        // encodes that exactly — `[]`→`[]` is not a change (byte-identity floor),
        // but `[]`→`[..]` or any change of entries IS a real change (the cascade
        // selection became observable / shifted). On no-change we still refresh
        // the stored value (it is already equal, so this is a no-op write) and
        // report "unchanged" so downstream memoized results are not invalidated.
        if old_value.normalization_scope == new_value.normalization_scope
            && old_value.assumptions == new_value.assumptions
            && old_value.witnesses == new_value.witnesses
            && old_value.selected_implementors == new_value.selected_implementors
        {
            old_value.selected_implementors = new_value.selected_implementors;
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

impl<'db> ImplEnv<'db> {
    pub fn new(
        _db: &'db dyn HirAnalysisDb,
        normalization_scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        witnesses: Vec<TraitInstId<'db>>,
    ) -> Self {
        Self {
            normalization_scope,
            assumptions,
            witnesses,
            selected_implementors: Vec::new(),
        }
    }

    pub fn empty(db: &'db dyn HirAnalysisDb, normalization_scope: ScopeId<'db>) -> Self {
        Self::new(
            db,
            normalization_scope,
            PredicateListId::empty_list(db),
            Vec::new(),
        )
    }

    /// The lexical scope used to normalize types within this instance.
    pub fn normalization_scope(&self, _db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.normalization_scope
    }

    /// The `where`-clause / param-env assumptions in force for this instance.
    pub fn assumptions(&self, _db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        self.assumptions
    }

    /// The trait-instance witnesses carried by this instance.
    pub fn witnesses(&self, _db: &'db dyn HirAnalysisDb) -> &Vec<TraitInstId<'db>> {
        &self.witnesses
    }

    /// Records a SINGLE per-goal impl override the typeck solver committed to at
    /// instantiation time. `None` carries an EMPTY carrier (byte-identical to the
    /// pre-M3 behavior — see the IDENTITY INVARIANT above); `Some((goal, impl))`
    /// carries a one-entry per-goal carrier. The MIR C1 rail consumes it as the
    /// resolution source; rung 3.3 asserts MIR re-resolution agrees.
    pub fn with_selected_implementor(
        self,
        selected_implementor: Option<(TraitInstId<'db>, ImplementorId<'db>)>,
    ) -> Self {
        self.with_selected_implementors(selected_implementor.into_iter().collect())
    }

    /// Records the per-goal impl overrides the typeck solver committed to at
    /// instantiation time. The carrier is CANONICALLY ORDERED here (sorted by
    /// goal's interned id, then deduplicated keeping the first override per goal)
    /// so two equal selection SETS mint EQUAL `ImplEnv`s — and therefore equal
    /// `SemanticInstanceKey`s and equal codegen symbols. An empty input leaves
    /// the carrier empty (the byte-identity floor — see the IDENTITY INVARIANT).
    pub fn with_selected_implementors(
        mut self,
        mut selected_implementors: Vec<(TraitInstId<'db>, ImplementorId<'db>)>,
    ) -> Self {
        // Canonical order: sort by the goal's interned id (deterministic within a
        // db; equal selection sets sort identically), then drop later duplicates
        // of the same goal so a goal maps to exactly one override.
        selected_implementors.sort_by_key(|(goal, _)| goal.as_id().as_u32());
        selected_implementors.dedup_by_key(|(goal, _)| *goal);
        self.selected_implementors = selected_implementors;
        self
    }

    /// The SINGLE impl override the typeck committed to, when this `ImplEnv`
    /// carries exactly one (the trait-method-callee / Gap-C nested-carry case).
    /// `None` for an empty carrier, AND for a multi-goal carrier (a generic
    /// helper boundary), where the MIR C1 rail must look up BY GOAL instead — see
    /// [`Self::selected_implementor_for_goal`]. Consumed by the rung-3.3 MIR
    /// re-resolution determinism assertion (`classify.rs::resolve_runtime_call_key`).
    pub fn selected_implementor(&self, _db: &'db dyn HirAnalysisDb) -> Option<ImplementorId<'db>> {
        match self.selected_implementors.as_slice() {
            [(_, implementor)] => Some(*implementor),
            _ => None,
        }
    }

    /// The impl override the typeck committed to FOR `goal`, if this `ImplEnv`
    /// carries one. This is the per-goal lookup the MIR C1 rail uses to propagate
    /// a scoped selection through a generic helper boundary (M3): the helper
    /// instance carries one `(goal, override)` per goal a `with (<T as Trait>)`
    /// scope selected at the helper call site, and the helper body's inner trait
    /// call resolves its override by matching its goal here.
    pub fn selected_implementor_for_goal(
        &self,
        _db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> Option<ImplementorId<'db>> {
        self.selected_implementors
            .iter()
            .find(|(carried_goal, _)| *carried_goal == goal)
            .map(|(_, implementor)| *implementor)
    }

    /// All per-goal impl overrides carried by this `ImplEnv`, in canonical order.
    /// Empty for any non-scope-selected instance (the byte-identity floor).
    /// Consumed by `stable_key.rs` to mirror the carrier into codegen-symbol
    /// identity in lockstep with interning identity.
    pub fn selected_implementors(
        &self,
        _db: &'db dyn HirAnalysisDb,
    ) -> &[(TraitInstId<'db>, ImplementorId<'db>)] {
        &self.selected_implementors
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct EffectProviderSubst<'db> {
    #[return_ref]
    pub providers: Vec<EffectProviderSpecialization<'db>>,
}

impl<'db> EffectProviderSubst<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }
}

pub fn instantiate_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TypedBodyTemplate<'db>,
    subst: GenericSubst<'db>,
) -> TypedBody<'db> {
    instantiate_with_generic_args(db, template.body, subst.generic_args(db))
}

pub fn instantiate_with_generic_args<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    generic_args: &[TyId<'db>],
) -> T
where
    T: TyFoldable<'db>,
{
    let mut folder = GenericInstantiator { generic_args };
    value.fold_with(db, &mut folder)
}

struct GenericInstantiator<'a, 'db> {
    generic_args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for GenericInstantiator<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => self.generic_args.get(param.idx).copied().unwrap_or(ty),
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                    && let Some(replacement) = self.generic_args.get(param.idx).copied()
                {
                    replacement
                } else {
                    ty.super_fold_with(db, self)
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

#[cfg(test)]
mod impl_env_identity_tests {
    //! FCO M3 IDENTITY-FLOOR tripwire (value layer).
    //!
    //! The `selected_implementors` per-goal carrier MUST fold into `ImplEnv`
    //! identity ONLY when NON-EMPTY: an EMPTY carrier hashes/compares EXACTLY as
    //! the pre-M3 `None` did (the byte-identity floor), and a NON-EMPTY carrier
    //! makes the scope selection observable (distinct `ImplEnv`, distinct
    //! `SemanticInstanceKey`). Canonical ordering makes equal selection SETS mint
    //! equal envs/keys regardless of insertion order. These assertions guard the
    //! manual `PartialEq`/`Eq`/`Hash`/`Update` above; the symbol-layer twin lives
    //! in `crates/mir/tests/m3_empty_carrier_floor.rs`.

    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use camino::Utf8PathBuf;

    use super::{EffectProviderSubst, GenericSubst, ImplEnv};
    use crate::{
        analysis::{
            semantic::SemanticInstanceKey,
            ty::{
                trait_def::{ImplementorId, TraitInstId, impls_for_trait_def},
                ty_check::BodyOwner,
            },
        },
        hir_def::TopLevelMod,
        test_db::HirAnalysisTestDb,
    };

    // Two derive-`Eq` structs → two REAL, distinct `impl Eq for _` implementors
    // (the carrier entries), plus a plain `host` func to own the instance whose
    // `ImplEnv` identity we probe.
    const FIXTURE: &str = r#"
#[derive(Eq)]
struct Alpha { x: u8 }

#[derive(Eq)]
struct Beta { y: u8 }

fn host() {}
"#;

    fn hash_of(env: &ImplEnv<'_>) -> u64 {
        let mut hasher = DefaultHasher::new();
        env.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn empty_carrier_is_byte_identical_to_none_and_nonempty_is_observable() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("m3_impl_env_floor.fe"), FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let host = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|f| f.name(&db).to_opt().is_some_and(|n| n.data(&db) == "host"))
            .expect("missing `host` function");
        let scope = host.scope();

        // Two real, distinct implementors + their goals.
        let (alpha_goal, alpha_impl) = real_eq_entry(&db, top_mod, "Alpha");
        let (beta_goal, beta_impl) = real_eq_entry(&db, top_mod, "Beta");
        assert_ne!(alpha_impl, beta_impl, "Alpha/Beta impls must be distinct");

        let base = ImplEnv::empty(&db, scope);

        // FLOOR: `with_selected_implementors(vec![])` is byte-identical to the
        // empty `ImplEnv::new` — equal under `Eq` AND `Hash`.
        let empty_via_setter = ImplEnv::empty(&db, scope).with_selected_implementors(vec![]);
        assert_eq!(base, empty_via_setter, "empty carrier must equal `None` floor");
        assert_eq!(
            hash_of(&base),
            hash_of(&empty_via_setter),
            "empty carrier must HASH identically to the `None` floor",
        );

        // FLOOR (interning): both mint the SAME `SemanticInstanceKey`.
        let key_none = key_for(&db, host, &base);
        let key_empty = key_for(&db, host, &empty_via_setter);
        assert_eq!(
            key_none, key_empty,
            "empty carrier must INTERN to the same key as the `None` floor",
        );

        // OBSERVABLE: a NON-EMPTY carrier is a DISTINCT env, hash, and key.
        let one = ImplEnv::empty(&db, scope)
            .with_selected_implementors(vec![(alpha_goal, alpha_impl)]);
        assert_ne!(base, one, "a non-empty carrier must differ from the floor");
        assert_ne!(
            hash_of(&base),
            hash_of(&one),
            "a non-empty carrier must hash distinctly from the floor",
        );
        assert_ne!(
            key_none,
            key_for(&db, host, &one),
            "a non-empty carrier must mint a distinct key",
        );

        // DISTINCTNESS: two different single overrides → different envs/keys.
        let other = ImplEnv::empty(&db, scope)
            .with_selected_implementors(vec![(beta_goal, beta_impl)]);
        assert_ne!(one, other, "different overrides must produce different envs");
        assert_ne!(
            key_for(&db, host, &one),
            key_for(&db, host, &other),
            "different overrides must mint different keys",
        );

        // CANONICAL ORDER: the SAME two entries in OPPOSITE insertion order
        // produce EQUAL envs and EQUAL keys (sorted-by-goal canonicalization).
        let ab = ImplEnv::empty(&db, scope)
            .with_selected_implementors(vec![(alpha_goal, alpha_impl), (beta_goal, beta_impl)]);
        let ba = ImplEnv::empty(&db, scope)
            .with_selected_implementors(vec![(beta_goal, beta_impl), (alpha_goal, alpha_impl)]);
        assert_eq!(ab, ba, "equal selection SETS must be equal regardless of order");
        assert_eq!(
            hash_of(&ab),
            hash_of(&ba),
            "equal selection SETS must hash equally regardless of order",
        );
        assert_eq!(
            key_for(&db, host, &ab),
            key_for(&db, host, &ba),
            "equal selection SETS must mint the same key regardless of order",
        );

        // BY-GOAL lookup resolves each entry; an unrelated goal misses.
        assert_eq!(ab.selected_implementor_for_goal(&db, alpha_goal), Some(alpha_impl));
        assert_eq!(ab.selected_implementor_for_goal(&db, beta_goal), Some(beta_impl));
    }

    /// A real `(goal, implementor)` for `impl Eq for <self_name>` — the derived
    /// `Eq` implementor whose self type is named `self_name`, paired with the
    /// trait inst it realizes (its goal).
    fn real_eq_entry<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        self_name: &str,
    ) -> (TraitInstId<'db>, ImplementorId<'db>) {
        use crate::analysis::name_resolution::{PathRes, resolve_path};
        use crate::analysis::ty::trait_resolution::PredicateListId;
        use crate::hir_def::PathId;

        let path = PathId::from_segments(db, &["core", "ops", "Eq"]);
        let PathRes::Trait(eq_inst) = resolve_path(
            db,
            path,
            top_mod.scope(),
            PredicateListId::empty_list(db),
            false,
        )
        .expect("core::ops::Eq must resolve") else {
            panic!("core::ops::Eq must resolve to a trait");
        };

        let implementor = impls_for_trait_def(db, top_mod.ingot(db), eq_inst.def(db))
            .iter()
            .map(|binder| binder.instantiate_identity())
            .find(|implementor| implementor.self_ty(db).pretty_print(db).contains(self_name))
            .unwrap_or_else(|| panic!("missing `impl Eq for {self_name}`"));
        (implementor.trait_inst(db), implementor)
    }

    fn key_for<'db>(
        db: &'db HirAnalysisTestDb,
        host: crate::hir_def::Func<'db>,
        impl_env: &ImplEnv<'db>,
    ) -> SemanticInstanceKey<'db> {
        SemanticInstanceKey::new(
            db,
            BodyOwner::Func(host),
            GenericSubst::empty(db),
            EffectProviderSubst::empty(db),
            impl_env.clone(),
        )
    }
}
