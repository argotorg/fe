use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::{ConstTyData, ConstTyId},
            layout_holes::{
                LayoutPlaceholderPolicy, collect_unique_layout_placeholders_in_order_with_policy,
                layout_hole_fallback_ty,
            },
            trait_def::TraitInstId,
            ty_def::{InvalidCause, TyData, TyId},
            visitor::{TyVisitable, TyVisitor, walk_const_ty, walk_ty},
        },
    },
    hir_def::{IdentId, PathId, Trait, TypeId as HirTypeId, scope_graph::ScopeId},
    span::DynLazySpan,
};
use common::indexmap::IndexMap;
use smallvec1::SmallVec;

use crate::core::semantic::{
    EffectRequirement, EffectRequirementKey as SemanticEffectRequirementKey,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectRequirementDecl<'db> {
    pub binding_idx: u32,
    pub required_mut: bool,
    pub name: Option<IdentId<'db>>,
    pub key_ty: HirTypeId<'db>,
    pub key: EffectRequirementKey<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EffectRequirementKey<'db> {
    Type(TypeKeySchema<'db>),
    Trait(TraitKeySchema<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeKeySchema<'db> {
    pub carrier: TyId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitKeySchema<'db> {
    pub def: Trait<'db>,
    pub args_no_self: SmallVec<[TyId<'db>; 2]>,
    pub assoc_bindings: SmallVec<[(IdentId<'db>, TyId<'db>); 2]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EffectPatternKey<'db> {
    Type(TypePatternKey<'db>),
    Trait(TraitPatternKey<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePatternKey<'db> {
    pub carrier: TyId<'db>,
    pub family: EffectFamily<'db>,
    pub slots: PatternSlots<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitPatternKey<'db> {
    pub def: Trait<'db>,
    pub args_no_self: SmallVec<[TyId<'db>; 2]>,
    pub assoc_bindings: SmallVec<[(IdentId<'db>, TyId<'db>); 2]>,
    pub family: EffectFamily<'db>,
    pub slots: PatternSlots<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternSlots<'db> {
    pub entries: SmallVec<[PatternSlot<'db>; 4]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatternSlot<'db> {
    pub id: PatternSlotId,
    pub kind: PatternSlotKind,
    pub placeholder: TyId<'db>,
    pub fallback_ty: TyId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatternSlotId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternSlotKind {
    LayoutPlaceholder,
    OmittedExplicitArg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectQueryMode {
    Precise,
    FamilyFallback,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectQuery<'db> {
    pub binding_idx: u32,
    pub required_mut: bool,
    pub mode: EffectQueryMode,
    pub key: EffectPatternKey<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StoredEffectKey<'db> {
    Type(StoredTypeKey<'db>),
    Trait(StoredTraitKey<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ForwardedEffectKey<'db> {
    Type(ForwardedTypeKey<'db>),
    Trait(ForwardedTraitKey<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WitnessTransport {
    Direct,
    ByValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StoredTypeKey<'db> {
    pub carrier: TyId<'db>,
    pub family: EffectFamily<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ForwardedTypeKey<'db> {
    pub carrier: TyId<'db>,
    pub family: EffectFamily<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StoredTraitKey<'db> {
    pub def: Trait<'db>,
    pub args_no_self: SmallVec<[TyId<'db>; 2]>,
    pub assoc_bindings: SmallVec<[(IdentId<'db>, TyId<'db>); 2]>,
    pub family: EffectFamily<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ForwardedTraitKey<'db> {
    pub def: Trait<'db>,
    pub args_no_self: SmallVec<[TyId<'db>; 2]>,
    pub assoc_bindings: SmallVec<[(IdentId<'db>, TyId<'db>); 2]>,
    pub family: EffectFamily<'db>,
}

/// A validated keyed provider entry with a rigid stored key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectWitness<'db, P> {
    pub key: StoredEffectKey<'db>,
    pub provider: P,
    pub transport: WitnessTransport,
}

/// A body-local forwarded requirement whose hidden vars must specialize consistently.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectForwarder<'db, P> {
    pub key: ForwardedEffectKey<'db>,
    pub provider: P,
    pub transport: WitnessTransport,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectBarrier<'db> {
    pub pattern: EffectPatternKey<'db>,
    pub reason: BarrierReason<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeyedEffectEntry<'db, P> {
    Witness(EffectWitness<'db, P>),
    Forwarder(EffectForwarder<'db, P>),
    Barrier(EffectBarrier<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectFamily<'db> {
    Type(TyId<'db>),
    Trait(Trait<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BarrierReason<'db> {
    InvalidExplicitTypeKey {
        span: DynLazySpan<'db>,
        key_path: PathId<'db>,
    },
    InvalidExplicitTraitKey {
        span: DynLazySpan<'db>,
        key_path: PathId<'db>,
    },
    UnstableExplicitKeyedProvider {
        span: DynLazySpan<'db>,
        key_path: PathId<'db>,
    },
}

impl<'db> EffectRequirementDecl<'db> {
    pub fn from_effect_requirement(
        db: &'db dyn HirAnalysisDb,
        requirement: &EffectRequirement<'db>,
    ) -> Option<Self> {
        let key = match requirement.key {
            SemanticEffectRequirementKey::Type(carrier) => {
                EffectRequirementKey::Type(TypeKeySchema { carrier })
            }
            SemanticEffectRequirementKey::Trait(trait_inst) => EffectRequirementKey::Trait(
                TraitKeySchema::from_canonical_trait_binding(db, trait_inst),
            ),
            SemanticEffectRequirementKey::Other => return None,
        };

        Some(Self {
            binding_idx: requirement.binding_idx,
            required_mut: requirement.is_mut,
            name: Some(requirement.binding_name),
            key_ty: requirement.binding_ty,
            key,
        })
    }
}

impl<'db> TraitKeySchema<'db> {
    pub fn from_canonical_trait_binding(
        db: &'db dyn HirAnalysisDb,
        trait_inst: TraitInstId<'db>,
    ) -> Self {
        let mut assoc_bindings: SmallVec<[(IdentId<'db>, TyId<'db>); 2]> =
            trait_inst.assoc_ty_bindings(db).into();
        assoc_bindings.sort_by_key(|(lhs, _)| *lhs);
        Self {
            def: trait_inst.def(db),
            args_no_self: trait_inst.args(db)[1..].iter().copied().collect(),
            assoc_bindings,
        }
    }

    pub fn into_trait_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        let self_ty = TyId::invalid(db, InvalidCause::Other);
        let args = std::iter::once(self_ty)
            .chain(self.args_no_self)
            .collect::<Vec<_>>();
        let assoc = self.assoc_bindings.into_iter().collect::<IndexMap<_, _>>();
        TraitInstId::new(db, self.def, args, assoc)
    }
}

impl<'db> PatternSlots<'db> {
    pub fn empty() -> Self {
        Self {
            entries: SmallVec::new(),
        }
    }

    pub(crate) fn from_value_with_extra<T>(
        db: &'db dyn HirAnalysisDb,
        value: T,
        policy: LayoutPlaceholderPolicy,
        extra_slots: impl IntoIterator<Item = PatternSlot<'db>>,
    ) -> Self
    where
        T: TyVisitable<'db>,
    {
        let mut extra_slots = extra_slots
            .into_iter()
            .collect::<SmallVec<[PatternSlot<'db>; 4]>>();
        let mut entries =
            collect_unique_layout_placeholders_in_order_with_policy(db, value, policy)
                .into_iter()
                .map(|placeholder| {
                    if let Some(idx) = extra_slots
                        .iter()
                        .position(|slot| slot.placeholder == placeholder)
                    {
                        return extra_slots.swap_remove(idx);
                    }

                    let fallback_ty = placeholder_fallback_ty(db, placeholder);
                    if let Some(idx) = extra_slots.iter().position(|slot| {
                        slot.kind != PatternSlotKind::LayoutPlaceholder
                            && slot.fallback_ty == fallback_ty
                    }) {
                        let mut slot = extra_slots.swap_remove(idx);
                        slot.placeholder = placeholder;
                        return slot;
                    }

                    PatternSlot {
                        id: PatternSlotId(0),
                        kind: PatternSlotKind::LayoutPlaceholder,
                        fallback_ty,
                        placeholder,
                    }
                })
                .collect::<SmallVec<[PatternSlot<'db>; 4]>>();
        entries.extend(extra_slots);
        for (idx, slot) in entries.iter_mut().enumerate() {
            slot.id = PatternSlotId(idx as u32);
        }
        Self { entries }
    }
}

impl<'db> EffectPatternKey<'db> {
    pub fn family(self) -> EffectFamily<'db> {
        match self {
            Self::Type(key) => key.family,
            Self::Trait(key) => key.family,
        }
    }

    pub fn slots(self) -> PatternSlots<'db> {
        match self {
            Self::Type(key) => key.slots,
            Self::Trait(key) => key.slots,
        }
    }
}

impl<'db> StoredEffectKey<'db> {
    pub fn family(self) -> EffectFamily<'db> {
        match self {
            Self::Type(key) => key.family,
            Self::Trait(key) => key.family,
        }
    }
}

impl<'db> ForwardedEffectKey<'db> {
    pub fn family(self) -> EffectFamily<'db> {
        match self {
            Self::Type(key) => key.family,
            Self::Trait(key) => key.family,
        }
    }

    pub fn is_well_formed(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            Self::Type(key) => forwarded_type_key_is_well_formed(db, *key),
            Self::Trait(key) => forwarded_trait_key_is_well_formed(db, key.clone()),
        }
    }
}

pub fn effect_family_for_type<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> EffectFamily<'db> {
    EffectFamily::Type(ty.decompose_ty_app(db).0)
}

pub fn effect_family_for_trait<'db>(trait_: Trait<'db>) -> EffectFamily<'db> {
    EffectFamily::Trait(trait_)
}

pub fn stored_type_key_is_rigid<'db>(db: &'db dyn HirAnalysisDb, key: StoredTypeKey<'db>) -> bool {
    stored_value_is_storage_rigid(db, key.carrier)
}

pub fn stored_trait_key_is_rigid<'db>(
    db: &'db dyn HirAnalysisDb,
    key: StoredTraitKey<'db>,
) -> bool {
    key.args_no_self
        .iter()
        .copied()
        .all(|ty| stored_value_is_storage_rigid(db, ty))
        && key
            .assoc_bindings
            .iter()
            .all(|(_, ty)| stored_value_is_storage_rigid(db, *ty))
}

pub fn type_key_schema_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    key: TypeKeySchema<'db>,
) -> bool {
    schema_value_is_well_formed(db, key.carrier)
}

pub fn trait_key_schema_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    key: &TraitKeySchema<'db>,
) -> bool {
    key.args_no_self
        .iter()
        .copied()
        .all(|ty| schema_value_is_well_formed(db, ty))
        && key
            .assoc_bindings
            .iter()
            .all(|(_, ty)| schema_value_is_well_formed(db, *ty))
}

pub fn forwarded_type_key_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    key: ForwardedTypeKey<'db>,
) -> bool {
    forwarded_value_is_well_formed(db, key.carrier)
}

pub fn forwarded_trait_key_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    key: ForwardedTraitKey<'db>,
) -> bool {
    key.args_no_self
        .iter()
        .copied()
        .all(|ty| forwarded_value_is_well_formed(db, ty))
        && key
            .assoc_bindings
            .iter()
            .all(|(_, ty)| forwarded_value_is_well_formed(db, *ty))
}

pub fn stored_value_is_storage_rigid<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
) -> bool {
    value_is_well_formed_with(db, value, false, false, false)
}

fn schema_value_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
) -> bool {
    value_is_well_formed_with(db, value, true, true, true)
}

fn forwarded_value_is_well_formed<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
) -> bool {
    value_is_well_formed_with(db, value, true, false, true)
}

fn value_is_well_formed_with<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
    allow_ty_vars: bool,
    allow_layout_holes: bool,
    allow_untyped_const_exprs: bool,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        allow_ty_vars: bool,
        allow_layout_holes: bool,
        allow_untyped_const_exprs: bool,
        invalid: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, _: &crate::analysis::ty::ty_def::TyVar<'db>) {
            self.invalid = !self.allow_ty_vars;
        }

        fn visit_assoc_ty(&mut self, _: &crate::analysis::ty::ty_def::AssocTy<'db>) {
            if !self.allow_ty_vars {
                self.invalid = true;
            }
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.invalid {
                return;
            }
            if let Some(cause) = ty.invalid_cause(self.db) {
                // Generic const-fn calls can be non-evaluable until effect-key
                // substitution gives them concrete const args.
                if matches!(cause, InvalidCause::ConstTyExpected { .. })
                    || (self.allow_untyped_const_exprs
                        && matches!(cause, InvalidCause::ConstEvalNonConstCall { .. }))
                {
                    return;
                }
                self.invalid = true;
                return;
            }
            walk_ty(self, ty);
        }

        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            match const_ty.data(self.db) {
                ConstTyData::Hole(..) if !self.allow_layout_holes => self.invalid = true,
                ConstTyData::UnEvaluated {
                    ty: None,
                    generic_args,
                    ..
                } if self.allow_untyped_const_exprs => {
                    generic_args.visit_with(self);
                }
                _ => walk_const_ty(self, const_ty),
            }
        }
    }

    let mut finder = Finder {
        db,
        allow_ty_vars,
        allow_layout_holes,
        allow_untyped_const_exprs,
        invalid: false,
    };
    value.visit_with(&mut finder);
    !finder.invalid
}

pub fn stored_value_contains_implicit_layout_params<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(const_ty.data(self.db), ConstTyData::TyParam(param, _) if param.is_implicit())
            {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = Finder { db, found: false };
    value.visit_with(&mut finder);
    finder.found
}

pub fn stored_value_contains_out_of_scope_params<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    value: impl TyVisitable<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        found: bool,
    }

    impl<'db> Finder<'db> {
        fn param_is_out_of_scope(&self, param: crate::analysis::ty::ty_def::TyParam<'db>) -> bool {
            !param.is_implicit()
                && !param.is_effect()
                && !param.is_effect_provider()
                && !param.is_trait_self()
                && !self.scope.is_transitive_child_of(self.db, param.owner)
        }
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            match ty.data(self.db) {
                TyData::TyParam(param) if self.param_is_out_of_scope(param.clone()) => {
                    self.found = true;
                }
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(self.db)
                        && self.param_is_out_of_scope(param.clone())
                    {
                        self.found = true;
                        return;
                    }
                    walk_ty(self, ty);
                }
                _ => walk_ty(self, ty),
            }
        }
    }

    let mut finder = Finder {
        db,
        scope,
        found: false,
    };
    value.visit_with(&mut finder);
    finder.found
}

fn placeholder_fallback_ty<'db>(db: &'db dyn HirAnalysisDb, placeholder: TyId<'db>) -> TyId<'db> {
    let TyData::ConstTy(const_ty) = placeholder.data(db) else {
        return placeholder;
    };
    match const_ty.data(db) {
        ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(db, *hole_ty),
        ConstTyData::TyParam(_, fallback_ty) => *fallback_ty,
        ConstTyData::TyVar(_, const_ty_ty) => *const_ty_ty,
        _ => placeholder,
    }
}
