//! Instances that the compiler runs for a body without a call written in it,
//! which the trust walk (see `trust`) follows like any call.
//!
//! - A contract's dispatcher decodes the arguments of a `recv` arm and encodes
//!   its return value, and the entry of a contract decodes the arguments of
//!   `init`. The code that does so is the `Decode` and `Encode` implementation
//!   of the types, which a dependency may have written, although no body
//!   names it.
//! - Drop elaboration inserts a call of `Drop::drop` where a value that owns
//!   drop glue goes out of scope, and of the fields of a value that a `drop`
//!   destroys.
use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            DropStep, SemOrigin, SemanticInstance, SemanticInstanceKey, VariantIndex,
            core_method_callee_key, drop_elaboration, generated_callee_key,
            normalized::{
                NDropTarget, NPlaceBase, NRootKind, NStatementId, NValueDefinition, NormalizedBody,
            },
        },
        ty::{
            adt_def::AdtRef,
            corelib::{resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path},
            drop_glue::{adt_drop_impl, implements_drop, ty_has_drop_glue},
            provider::registered_root_providers,
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, EffectParamSite},
            ty_def::TyId,
        },
    },
    hir_def::{Contract, Func, IdentId},
    semantic::RecvView,
};

/// The instance of `trait_inst`'s method `name` that a compiler-generated call
/// reaches, for the trait's own arguments `own_args`.
fn trait_method_callee<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    trait_inst: TraitInstId<'db>,
    name: &str,
    own_args: &[TyId<'db>],
) -> Option<SemanticInstanceKey<'db>> {
    let func = *trait_inst
        .def(db)
        .method_defs(db)
        .get(&IdentId::new(db, name.to_string()))?;
    generated_callee_key(
        db,
        contract.scope(),
        PredicateListId::empty_list(db),
        func,
        Some(trait_inst),
        own_args,
    )
    .ok()
}

fn free_function_callee<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    func: Func<'db>,
    args: &[TyId<'db>],
) -> Option<SemanticInstanceKey<'db>> {
    generated_callee_key(
        db,
        contract.scope(),
        PredicateListId::empty_list(db),
        func,
        None,
        args,
    )
    .ok()
}

/// The instances the dispatcher runs for `owner`, which is an `init` or a
/// `recv` arm of a contract: the decode of the arguments, and for an arm the
/// encode of its return value. The arm and the entry resolve the same
/// instances when the contract is lowered.
pub(super) fn dispatcher_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<SemanticInstanceKey<'db>> {
    match owner {
        BodyOwner::ContractInit { contract } => init_callees(db, contract),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => recv_arm_callees(db, contract, recv_idx, arm_idx),
        _ => Vec::new(),
    }
}

fn init_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<SemanticInstanceKey<'db>> {
    let args_ty = contract.init_args_ty(db);
    if contract.init(db).is_none() || args_ty == TyId::unit(db) {
        return Vec::new();
    }
    let scope = contract.scope();
    let (Some(abi_ty), Some(decoder_ty), Some(input_ty), Some(decode)) = (
        resolve_lib_type_path(db, scope, "std::abi::Sol"),
        resolve_lib_type_path(db, scope, "std::abi::sol::SolDecoder"),
        resolve_lib_type_path(db, scope, "core::ptr::MemSpan"),
        resolve_core_trait(db, scope, &["abi", "Decode"]),
    ) else {
        return Vec::new();
    };
    let inst = TraitInstId::new_simple(db, decode, vec![args_ty, abi_ty]);
    trait_method_callee(
        db,
        contract,
        inst,
        "decode_payload",
        &[decoder_ty, input_ty],
    )
    .into_iter()
    .collect()
}

fn recv_arm_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
    arm_idx: u32,
) -> Vec<SemanticInstanceKey<'db>> {
    let scope = contract.scope();
    let Some(abi_ty) = resolve_lib_type_path(db, scope, "std::abi::Sol") else {
        return Vec::new();
    };
    let Some(arm) = RecvView::new(db, contract, recv_idx).arm(db, arm_idx) else {
        return Vec::new();
    };
    let info = arm.abi_info(db, abi_ty);
    let Some(host_ty) = registered_root_providers(
        db,
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        },
    )
    .first()
    .map(|registration| registration.provider_ty) else {
        return Vec::new();
    };
    let mut callees = Vec::new();
    if info.args_ty != TyId::unit(db)
        && let Some(decode) =
            resolve_lib_func_path(db, scope, "core::contracts::decode_runtime_args")
    {
        callees.extend(free_function_callee(
            db,
            contract,
            decode,
            &[host_ty, abi_ty, info.args_ty],
        ));
    }
    if let Some(ret_ty) = info.ret_ty
        && let Some(host) = resolve_core_trait(db, scope, &["contracts", "ContractHost"])
    {
        let inst = TraitInstId::new_simple(db, host, vec![host_ty]);
        callees.extend(trait_method_callee(
            db,
            contract,
            inst,
            "return_value",
            &[abi_ty, ret_ty],
        ));
    }
    callees
}
/// The `Drop::drop` implementations that drop elaboration inserts for the
/// drop `statement` of `instance`'s `body`: for each part of the target that
/// is initialized there, the `drop` of its type if it has one, else those of
/// the parts it owns. A `drop` destroys its own `self` field by field.
pub(super) fn drop_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
    statement: NStatementId,
    target: &NDropTarget<'db>,
) -> Vec<SemanticInstanceKey<'db>> {
    let Some(elaboration) = drop_elaboration(db, instance) else {
        return Vec::new();
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
    };
    let own_self = drops_own_self(db, instance, body, target);
    for part in elaboration.parts(statement) {
        let part_ty = part
            .path
            .iter()
            .fold(ty, |ty, step| walk.part_ty(ty, *step));
        walk.destroy(part_ty, own_self && part.path.is_empty());
    }
    walk.callees
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
}

impl<'db> DropWalk<'db> {
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
        if (!field_wise && !self.seen.insert(ty)) || !ty_has_drop_glue(db, ty) {
            return;
        }
        let adt = ty.adt_def(db);
        if !field_wise
            && adt
                .as_ref()
                .is_some_and(|adt| adt_drop_impl(db, adt.adt_ref(db)).is_some())
        {
            if let Some(scope) = ty.as_scope(db)
                && let Ok(key) = core_method_callee_key(
                    db,
                    scope,
                    PredicateListId::empty_list(db),
                    &["ops", "Drop"],
                    "drop",
                    vec![ty],
                )
            {
                self.callees.push(key);
            }
            return;
        }
        if let Some(adt) = adt
            && matches!(adt.adt_ref(db), AdtRef::Enum(_))
        {
            for variant in 0..adt.fields(db).len() as u16 {
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
