//! Instances that the compiler runs for a body without a call written in it,
//! which the trust walk (see `trust`) follows like any call.
//!
//! - A contract's dispatcher decodes the arguments of a `recv` arm and encodes
//!   its return value, and the entry of a contract decodes the arguments of
//!   `init`. The code that does so is the `Decode` and `Encode` implementation
//!   of the types, which a dependency may have written, although no body
//!   names it, and, for an arm that reads its message in place, the reads of
//!   `std::abi`.
//! - Drop elaboration inserts a call of `Drop::drop` where a value that owns
//!   drop glue goes out of scope, and of the fields of a value that a `drop`
//!   destroys.
use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::Tier,
        semantic::{
            DropStep, SemOrigin, SemanticInstance, SemanticInstanceKey, VariantIndex, drop_callee,
            drop_elaboration,
            normalized::{
                NDropTarget, NPlaceBase, NRootKind, NStatementId, NValueDefinition, NormalizedBody,
            },
        },
        ty::{
            adt_def::AdtRef,
            drop_glue::{adt_drop_impl, implements_drop, ty_has_drop_glue},
            trait_resolution::TraitSolveCx,
            ty_check::BodyOwner,
            ty_def::TyId,
        },
    },
    semantic::{RecvArmInput, UnmetGoal, contract_entry_plan},
};
/// The instances the dispatcher runs for `owner`, which is an `init` or a
/// `recv` arm of a contract: the decode of the arguments, and for an arm the
/// read of the message and the encode of its return value. They are the steps
/// of the contract's entry plan, which the lowering of the contract and the
/// contract's check read too. A step without an instance is reported by the
/// check, and the walk has nothing to follow for it.
pub fn dispatcher_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<SemanticInstanceKey<'db>> {
    match owner {
        BodyOwner::ContractInit { contract } => contract_entry_plan(db, contract, None)
            .init
            .iter()
            .flatten()
            .copied()
            .collect(),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => {
            let Some(arm) = contract_entry_plan(db, contract, None).arm(recv_idx, arm_idx) else {
                return Vec::new();
            };
            let mut callees = Vec::new();
            match &arm.input {
                Ok(RecvArmInput::Whole { decode }) => callees.push(*decode),
                Ok(RecvArmInput::InPlace { args, reads }) => {
                    callees.push(*args);
                    callees.extend(reads.iter().map(|(_, read)| *read));
                }
                Ok(RecvArmInput::None) | Err(_) => {}
            }
            callees.extend(arm.ret.iter().flatten().copied());
            callees
        }
        _ => Vec::new(),
    }
}

/// The `Drop::drop` implementations that drop elaboration inserts for the
/// drop `statement` of `instance`'s `body`: for each part of the target that
/// is initialized there, the `drop` of its type if it has one, else those of
/// the parts it owns. A `drop` destroys its own `self` field by field. A
/// type whose `Drop` implementation has no proof is reported in `unproved`.
pub(super) fn drop_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
    statement: NStatementId,
    target: &NDropTarget<'db>,
) -> DropCallees<'db> {
    let Some(elaboration) = drop_elaboration(db, instance) else {
        return DropCallees::default();
    };
    let ty = match target {
        NDropTarget::Value(value) => body.values[value.index()].ty,
        NDropTarget::Place(place) => place.ty,
    };
    let mut walk = DropWalk {
        db,
        instance,
        seen: FxHashSet::default(),
        callees: Vec::new(),
        unproved: Vec::new(),
    };
    let own_self = drops_own_self(db, instance, body, target);
    for part in elaboration.parts(statement) {
        let mut part_ty = ty;
        let mut possible = true;
        for step in &part.path {
            // A part below a variant that cannot be constructed is never
            // destroyed.
            if let DropStep::Variant { variant, .. } = step
                && walk.variant_uninhabited(part_ty, *variant)
            {
                possible = false;
                break;
            }
            part_ty = walk.part_ty(part_ty, *step);
        }
        if possible {
            walk.destroy(part_ty, own_self && part.path.is_empty());
        }
    }
    DropCallees {
        keys: walk.callees,
        unproved: walk.unproved,
    }
}

/// A type that is destroyed whose `Drop` implementation has no ordinary proof,
/// and the goal that the implementation lacks, when known.
pub(super) struct UnprovedDrop<'db> {
    pub(super) ty: TyId<'db>,
    pub(super) unmet: Option<UnmetGoal>,
}

/// The result of `drop_callees`.
#[derive(Default)]
pub(super) struct DropCallees<'db> {
    pub(super) keys: Vec<SemanticInstanceKey<'db>>,
    pub(super) unproved: Vec<UnprovedDrop<'db>>,
}

/// Whether `target` is the `self` of the `Drop::drop` that `instance` runs.
fn drops_own_self<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
    target: &NDropTarget<'db>,
) -> bool {
    let self_param = match target {
        NDropTarget::Value(value) => matches!(
            body.values[value.index()].definition,
            NValueDefinition::EntryParam { param: 0 }
        ),
        NDropTarget::Place(place) => {
            place.path.is_empty()
                && matches!(
                    place.base,
                    NPlaceBase::Root(root)
                        if matches!(
                            body.roots[root.index()].kind,
                            NRootKind::ParamPlace { param: 0 }
                        )
                )
        }
    };
    let BodyOwner::Func(func, _) = instance.key(db).owner(db) else {
        return false;
    };
    self_param && implements_drop(db, func)
}

struct DropWalk<'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    seen: FxHashSet<TyId<'db>>,
    callees: Vec<SemanticInstanceKey<'db>>,
    /// The types whose `Drop` implementation has no proof, in the order met.
    unproved: Vec<UnprovedDrop<'db>>,
}

impl<'db> DropWalk<'db> {
    /// Whether no value of `ty` can exist in the body's instance: the
    /// predicate that runtime lowering asks before it destroys a value
    /// (`TyId::is_uninhabited_in`), with the scope and assumptions of the
    /// instance.
    fn uninhabited(&self, ty: TyId<'db>) -> bool {
        ty.is_uninhabited_in(
            self.db,
            Some(self.instance.key(self.db).owner(self.db).scope()),
            self.instance.assumptions(self.db),
        )
    }

    /// Whether `variant` of the enum type `ty` cannot be constructed, as
    /// runtime lowering asks before it destroys the fields of a variant.
    fn variant_uninhabited(&self, ty: TyId<'db>, variant: u16) -> bool {
        ty.is_enum_variant_uninhabited_in(
            self.db,
            usize::from(variant),
            Some(self.instance.key(self.db).owner(self.db).scope()),
            self.instance.assumptions(self.db),
        )
    }

    /// The type at `step` from a value of `ty`.
    fn part_ty(&self, ty: TyId<'db>, step: DropStep) -> TyId<'db> {
        match step {
            DropStep::Field(field) => {
                self.instance.normalized_field_types(self.db, ty)[field as usize]
            }
            DropStep::Variant { variant, field } => self
                .instance
                .normalized_enum_variant_field_tys(self.db, ty, VariantIndex(variant))
                [field as usize],
            DropStep::Index(_) => self
                .instance
                .normalized_ty(self.db, ty.generic_args(self.db)[0]),
        }
    }

    /// Destroys a value of `ty`, as the runtime does: a type with a `Drop`
    /// implementation is passed to its `drop`, which destroys the fields; any
    /// other type destroys its components. `field_wise` is the `self` of a
    /// `drop`, whose fields are destroyed without calling it again.
    fn destroy(&mut self, ty: TyId<'db>, field_wise: bool) {
        let db = self.db;
        if (!field_wise && !self.seen.insert(ty))
            || !ty_has_drop_glue(db, ty)
            || self.uninhabited(ty)
        {
            return;
        }
        let adt = ty.adt_def(db);
        let drop_impl = adt
            .as_ref()
            .and_then(|adt| adt_drop_impl(db, adt.adt_ref(db)));
        if !field_wise && let Some(drop_impl) = drop_impl {
            match drop_callee(db, ty) {
                Ok(key) => self.callees.push(key),
                // A type with parameters is proved where it is instantiated.
                Err(_) if ty.has_param(db) => {}
                Err(_) => {
                    // The type's `Drop` has no proof in its own scope: name
                    // the first bound of the implementation that it lacks.
                    let goal = drop_impl.trait_inst(db).with_self_ty(db, ty);
                    let unmet = ty.as_scope(db).and_then(|scope| {
                        UnmetGoal::of(db, TraitSolveCx::new(db, scope, Tier::All), goal)
                    });
                    self.unproved.push(UnprovedDrop { ty, unmet });
                }
            }
            return;
        }
        if let Some(adt) = adt
            && matches!(adt.adt_ref(db), AdtRef::Enum(_))
        {
            for variant in 0..adt.fields(db).len() as u16 {
                if self.variant_uninhabited(ty, variant) {
                    continue;
                }
                for field in
                    self.instance
                        .normalized_enum_variant_field_tys(db, ty, VariantIndex(variant))
                {
                    self.destroy(*field, false);
                }
            }
            return;
        }
        if let Some(len) = ty.array_len(db) {
            // An empty array has no element to destroy.
            if len > 0 {
                let element = self.instance.normalized_ty(db, ty.generic_args(db)[0]);
                self.destroy(element, false);
            }
            return;
        }
        for component in self.instance.normalized_field_types(db, ty) {
            self.destroy(*component, false);
        }
    }
}

/// What a drop of `target` is attributed to: the statement, or for a drop at
/// the end of a scope, which has no syntax of its own, where the dropped value
/// was defined, or else the body.
pub(super) fn drop_origin<'db>(
    owner: BodyOwner<'db>,
    body: &NormalizedBody<'db>,
    statement: SemOrigin<'db>,
    target: &NDropTarget<'db>,
) -> SemOrigin<'db> {
    let defined = match target {
        NDropTarget::Value(value) => body.values[value.index()].origin,
        NDropTarget::Place(place) => match place.base {
            NPlaceBase::Root(root) => body.roots[root.index()].origin,
            NPlaceBase::CapabilityTarget { .. } | NPlaceBase::Deref { .. } => place.origin,
        },
    };
    [statement, defined]
        .into_iter()
        .find(|origin| !matches!(origin, SemOrigin::Synthetic))
        .unwrap_or(SemOrigin::Body(owner))
}
