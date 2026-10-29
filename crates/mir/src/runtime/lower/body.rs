use hir::analysis::ty::AddressSpace;
use std::mem::size_of;

use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        BinaryOp, DropFlagPoint, EvalOutcome, FieldIndex, Overflow, RuntimeSizeError, SConst,
        SLocalId, SStmtId, SemConstId, SemConstScalar, SemConstValue, SemOrigin, SemanticCalleeRef,
        SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticConstRef, SemanticInstance,
        SemanticInstanceKey, SemanticLocalRole, VariantIndex, core_method_callee_key,
        eval_const_ref, generated_callee_key,
        normalized::{
            NBlockId, NDataPath, NDataProjection, NEffectArg, NEffectArgValue, NExpr, NIndex,
            NOperand, NPlace, NPlaceBase, NRootKind, NStatement, NStatementId, NStatementKind,
            NSuccessor, NTerminator, NTerminatorKind, NValueDefinition, NValueId, ReadMode,
        },
        reify_runtime_const_for_ty, runtime_size_bytes, sem_const_ty,
    },
    ty::{
        const_expr::ConstExpr,
        const_ty::ConstTyData,
        corelib::{
            RuntimeBuiltinFuncKind, concrete_usize, resolve_core_trait, resolve_lib_func_path,
            resolve_lib_type_path, runtime_builtin_func_kind,
        },
        trait_def::TraitInstId,
        trait_resolution::{
            GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
        },
        ty_check::{BodyOwner, LocalBinding},
        ty_def::{BorrowKind, TyId},
    },
};
use hir::hir_def::{ArithBinOp, BinOp, CompBinOp, Expr, Func, Partial, UnOp, scope_graph::ScopeId};
use hir::projection::IndexSource;
use rustc_hash::FxHashMap;

use crate::{
    db::MirDb,
    instance::{RuntimeInstance, RuntimeInstanceKey, RuntimeTarget, get_or_build_runtime_instance},
    resolve_runtime_place_address_class,
    runtime::place::{
        project_field_class, project_index_class, project_variant_field_class, runtime_place_lane,
    },
    runtime::{
        ConstScalar, IntrinsicArithBinOp, Layout, LayoutId, PlaceElem, PlaceRoot, RBlock, RBlockId,
        RExpr, RLocal, RLocalId, RStmt, RTerminator, RefKind, RefView, RuntimeBody, RuntimeBuiltin,
        RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeExitBehavior,
        RuntimeInterfaceSignature, RuntimeLocalRoot, RuntimePlace, RuntimeProviderBinding,
        RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole, VariantId,
        code_region::runtime_code_region_for_semantic_ref,
        layout_utils::RuntimeMemoryLayout,
        package::{LowerError, generated_call_error, runtime_instance_for_semantic},
        synthetic::uint_scalar,
    },
};

use super::{
    abi::{runtime_body_signature, runtime_declaration_signature},
    arg_selector::RuntimeArgSelector,
    boundary::{BoundarySiteAllocator, RuntimeValueUsePlan, boundary_spec_for_ty_in_env},
    call_input::{CompiledCallInputPlan, compile_call_input_plan_for_semantic},
    classify::{
        BodyEnv, BodyStaticFacts, ContractMetadataBuiltin, GenericNumericIntrinsicKind,
        InferClassCache, RuntimeBodyCx, contract_metadata_builtin, generic_numeric_intrinsic_kind,
        is_immutable_reference, semantic_return_ty,
    },
    consts::{
        aggregate_const_ref_class, const_scalar_for_class, const_scalar_from_value,
        enum_tag_scalar, evaluated_const_ref_value, lower_const_region,
    },
    conversion::{
        RuntimeConversionEmitter, RuntimeConversionError, RuntimeConversionPlanner,
        emit_runtime_coercion,
    },
    infer::{InferenceResult, LocalStateInferer, RuntimeLocalLowering, merge_runtime_class},
    interface::runtime_visible_binding_plans,
    layout::{
        AggregateCtorElem, aggregate_ctor_elems_for_layout, layout_for_aggregate_instance_in_env,
        layout_for_enum_variant_instance_in_env, layout_for_ty_in_env,
    },
    realize::{
        RuntimeArgSource, RuntimeValueUseEmitter, SelectedRuntimeArg, emit_runtime_value_use_plan,
    },
    returns::{
        declaration_runtime_return_class, runtime_return_class_for_body, semantic_never_returns,
    },
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
    source::{
        RuntimeSourceMode, RuntimeSourceQuery, SemanticPlaceValueSource,
        alias_source_place_for_local as source_alias_source_place_for_local,
        data_path_index_bounds, nonself_alias_source_place_for_local,
    },
    tuple::RuntimeTupleFieldEmitter,
    type_info::{
        RuntimeTypeEnv, deref_transport_class_in_env, provider_class_for_target_in_env,
        runtime_array_len, runtime_interface_ty_in_env, stored_class_for_ty_in_env,
        top_level_class_for_ty_in_env, validate_runtime_array_extents_in_env,
    },
};

mod drops;

pub fn lower_to_rmir<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> Result<RuntimeBody<'db>, LowerError> {
    let key = instance.key(db);
    let semantic = key
        .semantic(db)
        .expect("semantic lowering only applies to semantic runtime instances");
    let normalized_body = RuntimeSemanticBody::admitted(db, semantic)?;
    let type_env = RuntimeTypeEnv::for_semantic(db, semantic);
    for ty in normalized_body
        .normalized
        .values
        .iter()
        .map(|value| value.ty)
        .chain(normalized_body.normalized.roots.iter().map(|root| root.ty))
        .chain(normalized_body.locals.iter().map(|local| local.ty))
    {
        validate_runtime_array_extents_in_env(db, type_env, ty)?;
    }
    check_runtime_body_supported(db, semantic.key(db), &normalized_body)?;
    let facts = BodyStaticFacts::new(db, &normalized_body);
    let returned = runtime_return_class_for_body(db, key, &normalized_body);
    let signature = runtime_body_signature(db, key, &normalized_body, returned.as_ref());
    let param_locals = crate::runtime::lower::interface::runtime_param_locals(
        db,
        semantic,
        &normalized_body.source,
        key.params(db),
    );
    let mut inferer = LocalStateInferer::new(
        BodyEnv::new(db, &normalized_body, &facts),
        key.params(db),
        &param_locals,
    );
    // Return locals keep the body's own class; return lowering adapts it to
    // the declaration contract.
    if let Some(ret_class) = returned.filter(|class| class.contains_transport(db)) {
        let return_locals = normalized_body
            .normalized
            .blocks
            .iter()
            .filter_map(|block| normalized_body.operand_local(block.terminator.kind.returned()?))
            .collect::<Vec<_>>();
        inferer.seed_return_class(&return_locals, ret_class);
    }
    let inferred = inferer.run();
    let mut emitter = RmirEmitter::new(db, instance, normalized_body, facts, inferred, signature)?;
    emitter.lower_blocks();
    Ok(emitter.finish())
}

pub(super) fn check_runtime_body_supported<'db>(
    db: &'db dyn MirDb,
    key: SemanticInstanceKey<'db>,
    body: &RuntimeSemanticBody<'db>,
) -> Result<(), LowerError> {
    for block in &body.normalized.blocks {
        for stmt in &block.statements {
            if let NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } = &stmt.kind
                && let BodyOwner::Func(func, _) = callee.key.owner(db)
                && func.containing_trait(db).is_some()
                && func.body(db).is_none()
                && contract_metadata_builtin(db, SemanticInstance::new(db, callee.key)).is_none()
            {
                return Err(LowerError::Unsupported(format!(
                    "runtime call requires a selected implementation before layout planning: caller={key:?} callee={:?}",
                    callee.key,
                )));
            }
            if let NStatementKind::Define {
                result: dst,
                expr: NExpr::Const(constant),
            } = &stmt.kind
                && !matches!(constant, SConst::Ref(..))
            {
                let value = match constant {
                    SConst::Value(value) => value.value(),
                    SConst::Description(value) | SConst::Evidence(value) => *value,
                    SConst::Invalid(value) => {
                        return Err(LowerError::Unsupported(format!(
                            "invalid semantic constant from {:?}: {value:?}",
                            stmt.origin
                        )));
                    }
                    SConst::Ref(..) => unreachable!(),
                };
                if let Some(ty) = oversized_size_of_ty(db, key, value) {
                    return Err(LowerError::Unsupported(format!(
                        "type `{}` exceeds the supported 64-bit raw-memory layout size",
                        ty.pretty_print(db)
                    )));
                }
                let expected_ty = body.normalized.values[dst.index()].ty;
                if reify_runtime_const_for_ty(db, body.owner(), expected_ty, value).is_none() {
                    return Err(LowerError::Unsupported(format!(
                        "semantic value from {:?} cannot be reified as `{}` for runtime lowering",
                        stmt.origin,
                        expected_ty.pretty_print(db)
                    )));
                }
            }
            if let NStatementKind::Define {
                result: dst,
                expr: NExpr::Const(SConst::Ref(cref)),
            } = &stmt.kind
            {
                let const_name = semantic_const_ref_name(db, key, *cref);
                let value = match eval_const_ref(db, *cref) {
                    EvalOutcome::Ready(value) => value,
                    EvalOutcome::Blocked(info) => {
                        return Err(LowerError::Unsupported(format!(
                            "semantic constant `{const_name}` referenced from {:?} requires {:?}: {:?}",
                            cref.origin(db),
                            info.demand,
                            info.first_dependency
                        )));
                    }
                    EvalOutcome::Failed(failure) => {
                        return Err(LowerError::Unsupported(format!(
                            "semantic constant `{const_name}` referenced from {:?} failed CTFE: {failure:?}",
                            cref.origin(db)
                        )));
                    }
                };
                let expected_ty = body.normalized.values[dst.index()].ty;
                if reify_runtime_const_for_ty(db, body.owner(), expected_ty, value).is_none() {
                    return Err(LowerError::Unsupported(format!(
                        "semantic constant `{const_name}` referenced from {:?} failed to reify as `{}` for runtime lowering",
                        cref.origin(db),
                        expected_ty.pretty_print(db)
                    )));
                }
            }

            if let NStatementKind::Define {
                expr: NExpr::Call { callee, args, .. },
                ..
            } = &stmt.kind
                && let Some(value_ty) = builtin_arg_ty(
                    db,
                    body,
                    *callee,
                    args,
                    RuntimeBuiltinFuncKind::PanicWithValue,
                )
            {
                let impl_env = key.impl_env(db);
                ensure_panic_payload_encodable(
                    db,
                    impl_env.normalization_scope(db),
                    impl_env.assumptions(db),
                    value_ty,
                )?;
            }
            if let NStatementKind::Define {
                expr: NExpr::Call { callee, args, .. },
                ..
            } = &stmt.kind
                && let Some(value_ty) =
                    builtin_arg_ty(db, body, *callee, args, RuntimeBuiltinFuncKind::BlackBox)
            {
                let impl_env = key.impl_env(db);
                let env = RuntimeTypeEnv::new(
                    Some(impl_env.normalization_scope(db)),
                    impl_env.assumptions(db),
                );
                let value_ty = runtime_interface_ty_in_env(db, env, value_ty);
                // Pointer types become integer words in the runtime model.
                // Reject them while their source-level distinction is available.
                if value_ty.as_ptr(db).is_some()
                    || !matches!(
                        top_level_class_for_ty_in_env(db, env, value_ty, AddressSpace::Memory),
                        Some(RuntimeClass::Scalar(_))
                    )
                {
                    return Err(LowerError::Unsupported(format!(
                        "`core::hint::black_box` requires a scalar argument, such as an integer \
                         or `bool`, but found `{}`",
                        value_ty.pretty_print(db)
                    )));
                }
            }
        }
    }
    Ok(())
}

fn oversized_size_of_ty<'db>(
    db: &'db dyn MirDb,
    key: SemanticInstanceKey<'db>,
    value: SemConstId<'db>,
) -> Option<TyId<'db>> {
    let SemConstValue::Description(term) = value.value(db) else {
        return None;
    };
    let ConstTyData::Abstract(expr, _) = term.data(db) else {
        return None;
    };
    let ConstExpr::Invocation(invocation) = expr.data(db) else {
        return None;
    };
    let size_of = resolve_lib_func_path(db, key.owner(db).scope(), "core::size_of")?;
    if invocation.key.owner(db) != BodyOwner::Func(size_of, None) {
        return None;
    }
    let ty = *invocation.key.subst(db).generic_args(db).first()?;
    matches!(runtime_size_bytes(db, ty), Err(RuntimeSizeError::Overflow)).then_some(ty)
}

fn semantic_const_ref_name<'db>(
    db: &'db dyn MirDb,
    caller: SemanticInstanceKey<'db>,
    cref: SemanticConstRef<'db>,
) -> String {
    if let BodyOwner::Const(const_) = cref.instance(db).owner(db)
        && let Some(name) = const_.name(db).to_opt()
    {
        return name.data(db).clone();
    }

    if let hir::analysis::semantic::SemOrigin::Expr(expr) = cref.origin(db)
        && let Some(caller_body) = caller.owner(db).body(db)
        && let Partial::Present(Expr::Path(Partial::Present(path))) = expr.data(db, caller_body)
        && let Some(name) = path.ident(db).to_opt()
    {
        return name.data(db).clone();
    }

    "<associated const>".to_string()
}

/// Returns the argument type of a call to the single-argument builtin `kind`.
fn builtin_arg_ty<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    callee: SemanticCalleeRef<'db>,
    args: &[NOperand],
    kind: RuntimeBuiltinFuncKind,
) -> Option<TyId<'db>> {
    let BodyOwner::Func(func, _) = callee.key.owner(db) else {
        return None;
    };
    if runtime_builtin_func_kind(db, func) != Some(kind) {
        return None;
    }
    let [value] = args else {
        return None;
    };
    Some(body.normalized.value(value.value)?.ty)
}

fn ensure_panic_payload_encodable<'db>(
    db: &'db dyn MirDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    value_ty: TyId<'db>,
) -> Result<(), LowerError> {
    let abi_ty = resolve_lib_type_path(db, scope, "std::abi::Sol")
        .ok_or_else(|| LowerError::Unsupported("missing std::abi::Sol".to_string()))?;
    let encode_trait = resolve_core_trait(db, scope, &["abi", "Encode"])
        .ok_or_else(|| LowerError::Unsupported("missing core::abi::Encode".to_string()))?;
    let abi_size_trait = resolve_core_trait(db, scope, &["abi", "AbiSize"])
        .ok_or_else(|| LowerError::Unsupported("missing core::abi::AbiSize".to_string()))?;
    let encode = TraitInstId::new_simple(db, encode_trait, vec![value_ty, abi_ty]);
    let abi_size = TraitInstId::new_simple(db, abi_size_trait, vec![value_ty]);
    if trait_goal_satisfied(db, scope, assumptions, encode)
        && trait_goal_satisfied(db, scope, assumptions, abi_size)
    {
        return Ok(());
    }
    Err(LowerError::Unsupported(format!(
        "`unwrap()` requires the error type `{}` to implement `Encode<Sol>` and `AbiSize`",
        value_ty.pretty_print(db)
    )))
}

fn panic_payload_is_error_variant<'db>(
    db: &'db dyn MirDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    value_ty: TyId<'db>,
) -> bool {
    let Some(abi_ty) = resolve_lib_type_path(db, scope, "std::abi::Sol") else {
        return false;
    };
    let Some(error_variant_trait) = resolve_core_trait(db, scope, &["error", "ErrorVariant"])
    else {
        return false;
    };
    let error_variant = TraitInstId::new_simple(db, error_variant_trait, vec![value_ty, abi_ty]);
    trait_goal_satisfied(db, scope, assumptions, error_variant)
}

fn trait_goal_satisfied<'db>(
    db: &'db dyn MirDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    inst: TraitInstId<'db>,
) -> bool {
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    matches!(
        is_goal_satisfiable(db, solve_cx, inst),
        GoalSatisfiability::Satisfied(_)
    )
}

fn expr_requires_runtime_eval_when_erased(expr: &NExpr<'_>) -> bool {
    match expr {
        NExpr::ArrayRepeat { .. } => false,
        // An operation that can panic runs even when its result is unused.
        NExpr::Unary { op, .. } => op.overflow() == Some(Overflow::Checked),
        NExpr::Binary { op, .. } => {
            matches!(op, BinaryOp::Div(_) | BinaryOp::Rem)
                || op.overflow() == Some(Overflow::Checked)
        }
        NExpr::Call { .. } => true,
        NExpr::Forward { .. }
        | NExpr::ProjectValue { .. }
        | NExpr::StructuralRepack { .. }
        | NExpr::CodeRegionRef { .. }
        | NExpr::Load { .. }
        | NExpr::Const(_)
        | NExpr::PointerCast { .. }
        | NExpr::ScalarCast { .. }
        | NExpr::AggregateMake { .. }
        | NExpr::EnumMake { .. }
        | NExpr::Borrow { .. }
        | NExpr::MakeView { .. }
        | NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. } => false,
    }
}

pub(super) struct RmirEmitter<'db> {
    pub(super) db: &'db dyn MirDb,
    pub(super) instance: RuntimeInstance<'db>,
    pub(super) key: RuntimeInstanceKey<'db>,
    pub(super) semantic_body: RuntimeSemanticBody<'db>,
    pub(super) facts: BodyStaticFacts<'db>,
    pub(super) signature: RuntimeInterfaceSignature<'db>,
    pub(super) env: RuntimeTypeEnv<'db>,
    pub(super) semantic_carriers: Vec<RuntimeCarrier<'db>>,
    pub(super) semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    pub(super) provider_bindings: Vec<RuntimeProviderBinding<'db>>,
    normalized_value_temps: Vec<Option<RLocalId>>,
    /// The local each projection call assigns, by its normalized statement:
    /// the session an `End` of the call's result finishes.
    sessions: FxHashMap<NStatementId, RLocalId>,
    /// The memory copies of lanes, by local, with the lane each stores back
    /// into.
    lane_copies: FxHashMap<RLocalId, LaneSite<'db>>,
    /// The lane copies held for each extent, whose end stores them back.
    lane_extents: FxHashMap<LaneExtent, Vec<RLocalId>>,
    /// The lane copies the arguments of the call being lowered pass by
    /// address, with the places they copy.
    call_lane_copies: Vec<(NPlace<'db>, RLocalId)>,
    pub(super) locals: Vec<RLocal<'db>>,
    pub(super) blocks: Vec<RBlock<'db>>,
    pub(super) stmt_origins: Vec<Vec<SemOrigin<'db>>>,
    pub(super) terminator_origins: Vec<SemOrigin<'db>>,
    pub(super) current_origin: SemOrigin<'db>,
    pub(super) terminated_blocks: Vec<bool>,
    /// The local of each drop flag the body consults on its target
    /// (`drops`).
    drop_flags: Vec<Option<RLocalId>>,
    /// The flags each point of the body sets.
    drop_flag_updates: FxHashMap<DropFlagPoint, Vec<(u32, bool)>>,
}

/// An element a collection packs into a lane (`StateIndex::Lanes`): the
/// word at `slot` holds it `offset` bytes above its low end, encoded by
/// `codec`.
#[derive(Clone)]
struct Lane<'db> {
    slot: RLocalId,
    offset: RLocalId,
    space: AddressSpace,
    codec: TyId<'db>,
    ty: TyId<'db>,
}

/// A lane: a value packed into part of a storage word, which has no
/// address. Every construct that consumes a place lowers a lane the same
/// way (`resolve_place`): a read extracts it (`read_lane`), an instantaneous
/// write is a masked store (`write_lane`), and an access held for an extent
/// (a binding, an argument, a provided effect, a grant) works on a memory
/// copy (`hold_lane`) that the end of the extent stores back
/// (`end_lane_extent`).
#[derive(Clone)]
enum LaneSite<'db> {
    /// An element a collection packs into a lane.
    Entry(Lane<'db>),
    /// A field packed into a storage word, which a store masks into it.
    Field {
        place: RuntimePlace<'db>,
        ty: TyId<'db>,
    },
}

/// A place resolved for the construct that consumes it.
enum ResolvedPlace<'db> {
    Lane(LaneSite<'db>),
    /// A place with an address, which may lie in a lane's memory copy.
    Place(RuntimePlace<'db>),
}

/// What a lane's memory copy is held for: its end stores the copy back.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum LaneExtent {
    /// A `mut` access, a binding or an argument, which its `End` ends.
    Access(NValueId),
    /// A projection call's session, given the copy as a `mut` effect.
    Session(RLocalId),
}

/// The element `lower_entry` addresses.
enum EntryPlace<'db> {
    Place(RuntimePlace<'db>),
    Lane(Lane<'db>),
}

enum LoweredBuiltinCall<'db> {
    Expr {
        builtin: crate::runtime::RuntimeBuiltin<'db>,
        class: Option<RuntimeClass<'db>>,
    },
    Binary {
        op: BinOp,
        lhs: RLocalId,
        rhs: RLocalId,
    },
    Terminator(RTerminator<'db>),
}

enum ValueExtractStep<'db> {
    Aggregate {
        index: u32,
        class: RuntimeClass<'db>,
    },
    EnumField {
        variant: VariantId<'db>,
        field: FieldIndex,
        class: RuntimeClass<'db>,
    },
}

impl<'db> RuntimeConversionEmitter<'db> for RmirEmitter<'db> {
    fn alloc_conversion_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> RLocalId {
        self.alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(class))
    }

    fn push_conversion_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        self.push_stmt(bb, stmt);
    }
}

impl<'db> RuntimeValueUseEmitter<'db> for RmirEmitter<'db> {
    fn value_class_for_use(&self, value: RLocalId) -> Option<RuntimeClass<'db>> {
        self.value_class(value).cloned()
    }

    fn coerce_value_for_use(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        let _ = semantic_ty;
        self.coerce_value(bb, src, target)
    }

    fn emit_addr_of_place_for_use(
        &mut self,
        bb: RBlockId,
        place: RuntimePlace<'db>,
        class: RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        self.lower_place_addr_of_for_class(semantic_ty, bb, place, class)
    }

    fn alloc_value_slot(&mut self, semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> RLocalId {
        let slot = self.alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(class.clone()));
        self.locals[slot.index()].root = RuntimeLocalRoot::Slot(class);
        slot
    }

    fn push_value_use(&mut self, bb: RBlockId, dst: RLocalId, src: RLocalId) {
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(src),
            },
        );
    }
}

impl<'db> RuntimeTupleFieldEmitter<'db> for RmirEmitter<'db> {
    fn db(&self) -> &'db dyn MirDb {
        self.db
    }

    fn tuple_value_class(&self, tuple: RLocalId) -> Option<RuntimeClass<'db>> {
        self.value_class(tuple).cloned()
    }

    fn tuple_local_root(&self, tuple: RLocalId) -> RuntimeLocalRoot<'db> {
        self.locals[tuple.index()].root.clone()
    }

    fn alloc_tuple_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        carrier: RuntimeCarrier<'db>,
    ) -> RLocalId {
        self.alloc_runtime_temp(semantic_ty, carrier)
    }

    fn push_tuple_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        self.push_stmt(bb, stmt);
    }
}

impl<'db> crate::runtime::place::PlaceClassEnv<'db> for RmirEmitter<'db> {
    fn place_local_root(&self, local: RLocalId) -> Option<&RuntimeLocalRoot<'db>> {
        self.locals.get(local.index()).map(|local| &local.root)
    }

    fn place_value_class(&self, value: RLocalId) -> Option<&RuntimeClass<'db>> {
        self.value_class(value)
    }

    fn place_provider_binding(
        &self,
        binding: RuntimeProviderBindingId,
    ) -> Option<&RuntimeProviderBinding<'db>> {
        self.provider_bindings.get(binding.index())
    }
}

impl<'db> RmirEmitter<'db> {
    fn new(
        db: &'db dyn MirDb,
        instance: RuntimeInstance<'db>,
        semantic_body: RuntimeSemanticBody<'db>,
        facts: BodyStaticFacts<'db>,
        inferred: InferenceResult<'db>,
        signature: RuntimeInterfaceSignature<'db>,
    ) -> Result<Self, LowerError> {
        let key = instance.key(db);
        let semantic = key
            .semantic(db)
            .expect("semantic lowering only applies to semantic runtime instances");
        let InferenceResult {
            carriers,
            roots,
            semantic_locals,
            provider_bindings,
        } = inferred;
        let semantic_carriers = carriers.clone();
        let env = RuntimeTypeEnv::for_semantic(db, semantic);
        let terminated_blocks = vec![false; semantic_body.normalized.blocks.len()];
        let stmt_origins = vec![Vec::new(); semantic_body.normalized.blocks.len()];
        let terminator_origins = vec![SemOrigin::Synthetic; semantic_body.normalized.blocks.len()];
        let locals = semantic_body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, local)| RLocal {
                semantic_ty: local.ty,
                carrier: carriers.get(idx).cloned().unwrap_or(RuntimeCarrier::Erased),
                root: roots.get(idx).cloned().unwrap_or(RuntimeLocalRoot::None),
            })
            .collect::<Vec<_>>();
        let blocks = Vec::with_capacity(semantic_body.normalized.blocks.len());
        let normalized_value_temps = vec![None; semantic_body.normalized.values.len()];
        Ok(Self {
            db,
            instance,
            key,
            semantic_body,
            facts,
            signature,
            env,
            semantic_carriers,
            semantic_locals,
            provider_bindings,
            normalized_value_temps,
            sessions: FxHashMap::default(),
            lane_copies: FxHashMap::default(),
            lane_extents: FxHashMap::default(),
            call_lane_copies: Vec::new(),
            locals,
            blocks,
            stmt_origins,
            terminator_origins,
            current_origin: SemOrigin::Synthetic,
            terminated_blocks,
            drop_flags: Vec::new(),
            drop_flag_updates: FxHashMap::default(),
        })
    }

    fn finish(mut self) -> RuntimeBody<'db> {
        while self.blocks.len() < self.semantic_body.normalized.blocks.len() {
            self.blocks.push(RBlock {
                stmts: Vec::new(),
                terminator: RTerminator::Return(None),
            });
            self.stmt_origins.push(Vec::new());
            self.terminator_origins.push(SemOrigin::Synthetic);
        }
        RuntimeBody {
            owner: self.instance,
            key: self.key,
            signature: self.signature,
            provider_bindings: self.provider_bindings,
            locals: self.locals,
            blocks: self.blocks,
            stmt_origins: self.stmt_origins,
            terminator_origins: self.terminator_origins,
        }
    }

    fn layout_for_ty(&self, ty: TyId<'db>) -> LayoutId<'db> {
        layout_for_ty_in_env(self.db, self.env, ty)
    }

    fn target(&self) -> RuntimeTarget {
        self.instance.target(self.db)
    }

    fn current_semantic_key(&self) -> SemanticInstanceKey<'db> {
        self.key
            .semantic(self.db)
            .expect("runtime lowering requires a semantic instance")
            .key(self.db)
    }

    fn top_level_class_for_ty(
        &self,
        ty: TyId<'db>,
        default_space: AddressSpace,
    ) -> Option<RuntimeClass<'db>> {
        top_level_class_for_ty_in_env(self.db, self.env, ty, default_space)
    }

    fn lower_blocks(&mut self) {
        self.blocks = (0..self.semantic_body.normalized.blocks.len())
            .map(|_| RBlock {
                stmts: Vec::new(),
                terminator: RTerminator::Return(None),
            })
            .collect();
        self.stmt_origins = vec![Vec::new(); self.semantic_body.normalized.blocks.len()];
        self.terminator_origins =
            vec![SemOrigin::Synthetic; self.semantic_body.normalized.blocks.len()];
        self.terminated_blocks = vec![false; self.semantic_body.normalized.blocks.len()];
        let blocks = self.semantic_body.normalized.blocks.clone();
        self.prepare_drop_flags();
        for (idx, block) in blocks.iter().enumerate() {
            let block_id = NBlockId::new(idx);
            let mut bb = RBlockId::from_u32(idx as u32);
            self.update_drop_flags(bb, DropFlagPoint::BlockStart(block_id));
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                bb = self.with_current_origin(stmt.origin, |this| {
                    this.lower_stmt(bb, block_id, stmt_idx, stmt)
                });
                self.update_drop_flags(bb, DropFlagPoint::After(stmt.id));
                if self.terminated_blocks[bb.index()] {
                    break;
                }
            }
            self.update_drop_flags(bb, DropFlagPoint::BeforeTerminator(block_id));
            if !self.terminated_blocks[bb.index()] {
                self.with_current_origin(block.terminator.origin, |this| {
                    this.blocks[bb.index()].terminator =
                        this.lower_terminator(bb, &block.terminator);
                    this.terminator_origins[bb.index()] = this.current_origin;
                });
            }
        }
    }

    fn set_terminator(&mut self, bb: RBlockId, terminator: RTerminator<'db>) {
        self.blocks[bb.index()].terminator = terminator;
        self.terminator_origins[bb.index()] = self.current_origin;
        self.terminated_blocks[bb.index()] = true;
    }

    fn with_current_origin<T>(
        &mut self,
        origin: SemOrigin<'db>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let previous = self.current_origin;
        self.current_origin = origin;
        let result = f(self);
        self.current_origin = previous;
        result
    }

    /// Lowers `stmt`, the statement at `stmt_idx` of `block`, into `bb`;
    /// lowering goes on in the block it returns. A drop splits blocks, so
    /// `bb` need not be `block`'s own.
    fn lower_stmt(
        &mut self,
        bb: RBlockId,
        block: NBlockId,
        stmt_idx: usize,
        stmt: &NStatement<'db>,
    ) -> RBlockId {
        self.lower_stmt_index_checks(bb, &stmt.kind);
        if self.terminated_blocks[bb.index()] {
            return bb;
        }
        match &stmt.kind {
            NStatementKind::Define { result, expr } => {
                self.lower_assign(bb, block, stmt_idx, stmt.source, *result, expr)
            }
            NStatementKind::Drop { target } => return self.lower_drop(bb, stmt.id, target),
            NStatementKind::End { access } => {
                if !self.end_lane_extent(bb, LaneExtent::Access(*access))
                    && let NValueDefinition::Statement { block, statement } =
                        self.semantic_body.normalized.values[access.index()].definition
                    && let NStatementKind::Define {
                        expr:
                            NExpr::Borrow {
                                place,
                                kind: BorrowKind::Mut,
                                ..
                            },
                        ..
                    } = &self.semantic_body.normalized.blocks[block.index()].statements
                        [statement as usize]
                        .kind
                {
                    assert!(
                        !self.with_current_body_cx(|cx| cx
                            .env
                            .place_entry_root(place)
                            .flatten()
                            .is_some_and(|entry| entry.lane))
                            || !self.semantic_body.normalized.access_is_used(*access),
                        "a lane borrow ends before it is lowered"
                    );
                }
                if let NValueDefinition::Statement { block, statement } =
                    self.semantic_body.normalized.values[access.index()].definition
                    && let Some(&session) = self.sessions.get(
                        &self.semantic_body.normalized.blocks[block.index()].statements
                            [statement as usize]
                            .id,
                    )
                {
                    self.push_stmt(bb, RStmt::End { session });
                    self.end_lane_extent(bb, LaneExtent::Session(session));
                }
            }
            NStatementKind::Store { destination, value } => {
                if !(stmt.source.is_none()
                    && self.lower_local_root_assignment(bb, destination, *value))
                {
                    let place_class = self.with_current_body_cx(|cx| {
                        cx.env.normalized_place_class(cx.carriers, destination)
                    });
                    if place_class.is_some() {
                        match self.resolve_place(bb, destination) {
                            ResolvedPlace::Lane(lane) => {
                                let class = self.lane_class(&lane);
                                let value =
                                    self.lower_semantic_operand_for_class(bb, *value, &class);
                                self.write_lane(bb, &lane, value);
                            }
                            ResolvedPlace::Place(place) => {
                                let target = self.project_place_class(&place);
                                // A native-reference slot stores its carrier,
                                // whereas a scalar destination needs the
                                // referent's value.
                                let value =
                                    self.lower_semantic_operand_for_class(bb, *value, &target);
                                let copy = self.lane_copy_root(&place);
                                self.write_value_to_place(bb, place, value, &target);
                                // A write into a lane's copy is a write of the lane.
                                if let Some(copy) = copy {
                                    self.store_lane_copy(bb, copy);
                                }
                            }
                        }
                    }
                }
            }
        }
        bb
    }

    fn lower_local_root_assignment(
        &mut self,
        bb: RBlockId,
        destination: &NPlace<'db>,
        value: NOperand,
    ) -> bool {
        let NPlaceBase::Root(root) = destination.base else {
            return false;
        };
        if !destination.path.is_empty()
            || !matches!(
                self.semantic_body
                    .normalized
                    .root(root)
                    .map(|root| &root.kind),
                Some(NRootKind::LocalSlot { .. })
            )
        {
            return false;
        }
        let Some(local) = self.semantic_body.root_local(root) else {
            return false;
        };
        let runtime_local = self.runtime_value(local);
        let class = match &self.locals[runtime_local.index()].root {
            RuntimeLocalRoot::Ref(class) => class.clone(),
            RuntimeLocalRoot::None => {
                let Some(class) = self.value_class(runtime_local).cloned() else {
                    return true;
                };
                class
            }
            RuntimeLocalRoot::Slot(_) | RuntimeLocalRoot::Ptr { .. } => return false,
        };
        // A normalized assignment initializes the root's representation. Its
        // source span may name an expression; it does not make this a write
        // through the reference (which may point into immutable constant data).
        let source = self.lower_semantic_operand_for_class(bb, value, &class);
        let source = self.coerce_value(bb, source, &class);
        if source != runtime_local {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: runtime_local,
                    expr: RExpr::Use(source),
                },
            );
        }
        true
    }

    fn lower_assign(
        &mut self,
        bb: RBlockId,
        block: NBlockId,
        stmt_idx: usize,
        stmt_id: Option<SStmtId>,
        result: NValueId,
        expr: &NExpr<'db>,
    ) {
        let dst = self
            .semantic_body
            .value_local(result)
            .unwrap_or_else(|| panic!("normalized value {result:?} has no runtime representation"));
        let result_ty = self
            .semantic_body
            .normalized
            .value(result)
            .expect("normalized assignment result must exist")
            .ty;
        let normalized_id =
            self.semantic_body.normalized.blocks[block.index()].statements[stmt_idx].id;
        // A call that never returns ends its executable block and produces no
        // value, so carrier inference gives its result no class.
        if let NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } = expr
            && semantic_never_returns(self.db, SemanticInstance::new(self.db, callee.key))
        {
            self.lower_call(bb, normalized_id, *callee, args, effect_args);
            return;
        }
        let direct_class = self.current_expr_direct_class(block.index(), stmt_idx, expr);
        if self.root_provider_value_load_is_lazy(dst, expr)
            || ((matches!(expr, NExpr::MakeView { .. })
                || (stmt_id.is_none()
                    && matches!(expr, NExpr::Load { place, .. } if place.path.is_empty())))
                && self.runtime_value_is_unused(result))
            || self.is_dead_lane_access(result, expr)
        {
            self.normalized_value_temps[result.index()] = None;
            return;
        }
        if let RuntimeLocalRoot::Slot(slot_class) =
            self.locals[self.runtime_value(dst).index()].root.clone()
        {
            let carrier = direct_class
                .clone()
                .or_else(|| self.semantic_value_class(dst))
                .map(RuntimeCarrier::Value)
                .unwrap_or(RuntimeCarrier::Erased);
            let temp = self.alloc_runtime_temp(result_ty, carrier);
            self.normalized_value_temps[result.index()] = Some(temp);
            self.lower_expr_into(bb, normalized_id, result, temp, expr);
            let stored_by_normalized_root = self
                .semantic_body
                .normalized
                .blocks
                .get(block.index())
                .and_then(|block| block.statements.get(stmt_idx + 1))
                .is_some_and(|statement| {
                    let NStatementKind::Store { destination, value } = &statement.kind else {
                        return false;
                    };
                    value.value == result
                        && destination.path.is_empty()
                        && matches!(
                            destination.base,
                            NPlaceBase::Root(root)
                                if self.semantic_body.root_local(root) == Some(dst)
                        )
                });
            let read_from_current_slot = match expr {
                NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => {
                    self.semantic_body.operand_local(*src) == Some(dst)
                        && self.normalized_value_temps[src.value.index()].is_none()
                }
                NExpr::Load { place, .. } => {
                    place.path.is_empty()
                        && matches!(
                            place.base,
                            NPlaceBase::Root(root)
                                if self.semantic_body.root_local(root) == Some(dst)
                        )
                }
                NExpr::ProjectValue { .. }
                | NExpr::CodeRegionRef { .. }
                | NExpr::Const(_)
                | NExpr::Unary { .. }
                | NExpr::Binary { .. }
                | NExpr::PointerCast { .. }
                | NExpr::ScalarCast { .. }
                | NExpr::ArrayRepeat { .. }
                | NExpr::AggregateMake { .. }
                | NExpr::EnumMake { .. }
                | NExpr::Borrow { .. }
                | NExpr::MakeView { .. }
                | NExpr::GetEnumTag { .. }
                | NExpr::IsEnumVariant { .. }
                | NExpr::Call { .. }
                | NExpr::CodeRegionOffset { .. }
                | NExpr::CodeRegionLen { .. } => false,
            };
            if self.value_class(temp) == Some(&slot_class)
                && !stored_by_normalized_root
                && !read_from_current_slot
            {
                self.write_semantic_value(bb, dst, temp);
            }
            return;
        }
        if result_ty != self.semantic_body.locals[dst.index()].ty
            || direct_class
                .as_ref()
                .is_some_and(|class| Some(class) != self.local_class(dst))
        {
            let syncs_direct_carrier = result_ty == self.semantic_body.locals[dst.index()].ty
                && self.semantic_local_is_direct(dst)
                && direct_class
                    .as_ref()
                    .zip(self.local_class(dst))
                    .is_some_and(|(source, target)| {
                        RuntimeConversionPlanner::plan(self.db, source.clone(), target.clone())
                            .is_ok()
                    });
            let lowers_into_merged_class = matches!(
                expr,
                NExpr::Const(_)
                    | NExpr::ArrayRepeat { .. }
                    | NExpr::AggregateMake { .. }
                    | NExpr::EnumMake { .. }
            ) && direct_class
                .as_ref()
                .zip(self.local_class(dst))
                .is_some_and(|(direct, target)| {
                    merge_runtime_class(self.db, direct, target).as_ref() == Some(target)
                });
            if result_ty == self.semantic_body.locals[dst.index()].ty
                && self.semantic_local_is_direct(dst)
                && (direct_class.as_ref() == self.local_class(dst) || lowers_into_merged_class)
            {
                let dst = self.runtime_value(dst);
                self.normalized_value_temps[result.index()] = Some(dst);
                self.lower_expr_into(bb, normalized_id, result, dst, expr);
                return;
            }
            let carrier = direct_class
                .map(RuntimeCarrier::Value)
                .unwrap_or(RuntimeCarrier::Erased);
            let temp = self.alloc_runtime_temp(result_ty, carrier);
            self.normalized_value_temps[result.index()] = Some(temp);
            self.lower_expr_into(bb, normalized_id, result, temp, expr);
            if syncs_direct_carrier {
                self.write_semantic_value(bb, dst, temp);
            }
            return;
        }
        self.normalized_value_temps[result.index()] = None;
        let desired = self.semantic_value_class(dst);
        match desired {
            None => {
                if !expr_requires_runtime_eval_when_erased(expr) {
                    return;
                }
                let carrier = self
                    .current_expr_direct_class(block.index(), stmt_idx, expr)
                    .map(RuntimeCarrier::Value)
                    .unwrap_or(RuntimeCarrier::Erased);
                let sink = self.alloc_runtime_temp(result_ty, RuntimeCarrier::Erased);
                self.locals[sink.index()].carrier = carrier;
                self.lower_expr_into(bb, normalized_id, result, sink, expr);
            }
            Some(desired) => {
                if self.semantic_local_is_derived_place_bound_alias(dst) {
                    self.lower_derived_place_bound_alias_assign(dst, expr);
                    return;
                }
                if self.semantic_local_is_direct(dst) {
                    let dst = self.runtime_value(dst);
                    self.normalized_value_temps[result.index()] = Some(dst);
                    self.lower_expr_into(bb, normalized_id, result, dst, expr);
                    return;
                }
                let temp = self.alloc_runtime_temp(
                    self.locals[dst.index()].semantic_ty,
                    RuntimeCarrier::Value(desired),
                );
                self.normalized_value_temps[result.index()] = Some(temp);
                self.lower_expr_into(bb, normalized_id, result, temp, expr);
                self.write_semantic_value(bb, dst, temp);
            }
        }
    }

    fn with_current_body_cx<T>(&self, f: impl FnOnce(RuntimeBodyCx<'_, '_, 'db>) -> T) -> T {
        f(BodyEnv::new(self.db, &self.semantic_body, &self.facts)
            .with_carriers(&self.semantic_carriers))
    }

    fn current_expr_direct_class(
        &mut self,
        block_idx: usize,
        stmt_idx: usize,
        expr: &NExpr<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let mut lookup_return_class = |key| declaration_runtime_return_class(self.db, key);
        BodyEnv::new(self.db, &self.semantic_body, &self.facts).expr_direct_class(
            &self.semantic_carriers,
            block_idx,
            stmt_idx,
            expr,
            None,
            &mut lookup_return_class,
        )
    }

    fn lower_expr_into(
        &mut self,
        bb: RBlockId,
        stmt_id: NStatementId,
        result: NValueId,
        dst: RLocalId,
        expr: &NExpr<'db>,
    ) {
        let Some(dst_class) = self.value_class(dst).cloned() else {
            if let NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } = expr
            {
                let _ = self.lower_call(bb, stmt_id, *callee, args, effect_args);
            }
            return;
        };

        match expr {
            NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => {
                let value = self.lower_semantic_operand_for_class(bb, *src, &dst_class);
                let value = self.coerce_value_if_needed(bb, value, &dst_class);
                // An object ref can be the runtime representation of an owned
                // aggregate. Copying that value must not alias its backing object.
                // Borrow/capability types, on the other hand, copy the handle.
                let copies_aggregate = src.mode == ReadMode::Copy
                    && self
                        .semantic_body
                        .operand_local(*src)
                        .and_then(|local| self.semantic_value_class(local))
                        .as_ref()
                        == Some(&dst_class)
                    && matches!(
                        dst_class,
                        RuntimeClass::Ref {
                            kind: RefKind::Object,
                            view: RefView::Whole,
                            ..
                        }
                    )
                    && matches!(
                        stored_class_for_ty_in_env(
                            self.db,
                            self.env,
                            self.locals[dst.index()].semantic_ty,
                        ),
                        RuntimeClass::AggregateValue { .. }
                    );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: if copies_aggregate {
                            RExpr::MaterializePlaceToObject {
                                place: RuntimePlace {
                                    root: PlaceRoot::Ref(value),
                                    path: Box::default(),
                                },
                            }
                        } else {
                            RExpr::Use(value)
                        },
                    },
                );
            }
            NExpr::ProjectValue { value, path } => {
                let source_class = self.with_current_body_cx(|cx| {
                    cx.env
                        .normalized_value_structural_class(cx.carriers, value.value)
                        .expect("verified structural projection source must have a runtime class")
                });
                let source = self.lower_semantic_operand_for_class(bb, *value, &source_class);
                let class = self
                    .value_class(source)
                    .cloned()
                    .expect("normalized projected value must have a runtime class");
                if !self.lower_value_extract_from_value(
                    bb,
                    dst,
                    source,
                    class.clone(),
                    &path.0.iter().copied().collect::<Vec<_>>(),
                ) {
                    panic!(
                        "verified normalized structural projection must be runtime-lowerable: owner={:?}; source={source:?}; class={class:?}; path={path:?}; dst={dst:?}; dst_class={:?}",
                        self.current_semantic_key().owner(self.db),
                        self.value_class(dst),
                    );
                }
            }
            NExpr::Load { place, .. } => {
                self.lower_semantic_place_read_into(bb, dst, place, &dst_class);
            }
            NExpr::Const(const_) => {
                self.lower_const_into(bb, dst, const_);
            }
            NExpr::Unary { op, value } => {
                let value = self.read_semantic_operand(bb, *value);
                let RuntimeClass::Scalar(class) = &dst_class else {
                    panic!("a unary operation has a scalar result");
                };
                let expr = self
                    .lower_unary_expr_for_mode(
                        bb,
                        op.un_op(),
                        op.overflow() == Some(Overflow::Checked),
                        value,
                        self.locals[dst.index()].semantic_ty,
                        class,
                    )
                    .expect("a built-in unary operation lowers");
                self.push_stmt(bb, RStmt::Assign { dst, expr });
            }
            NExpr::Binary { op, lhs, rhs } => {
                let lhs = self.read_semantic_operand(bb, *lhs);
                let rhs = self.read_semantic_operand(bb, *rhs);
                let expr = match op.bin_op() {
                    BinOp::Arith(arith) => {
                        let RuntimeClass::Scalar(class) = &dst_class else {
                            panic!("an arithmetic operation has a scalar result");
                        };
                        self.lower_arith_expr_for_mode(
                            bb,
                            arith,
                            op.overflow() == Some(Overflow::Checked),
                            lhs,
                            rhs,
                            class,
                        )
                        .expect("a built-in arithmetic operation lowers")
                    }
                    op => RExpr::Binary { op, lhs, rhs },
                };
                self.push_stmt(bb, RStmt::Assign { dst, expr });
            }
            NExpr::ScalarCast { value, .. } | NExpr::PointerCast { value, .. } => {
                let value = self.read_semantic_operand(bb, *value);
                if let RuntimeClass::Scalar(to) = dst_class {
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::Cast { value, to },
                        },
                    );
                } else {
                    let copied = self.coerce_value(bb, value, &dst_class);
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::Use(copied),
                        },
                    );
                }
            }
            NExpr::ArrayRepeat { ty, value } => self.lower_array_repeat(bb, dst, *ty, *value),
            NExpr::AggregateMake { ty, fields } => self.lower_aggregate_make(bb, dst, *ty, fields),
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => self.lower_enum_make(bb, dst, *enum_ty, *variant, fields),
            NExpr::Borrow { place, .. } | NExpr::MakeView { place, .. } => {
                if matches!(expr, NExpr::MakeView { .. }) && !dst_class.is_transport() {
                    self.lower_semantic_place_read_into(bb, dst, place, &dst_class);
                    return;
                }
                let (value, copy) = self.lower_place_address(
                    self.locals[dst.index()].semantic_ty,
                    bb,
                    place,
                    dst_class,
                );
                if let NExpr::Borrow {
                    kind: BorrowKind::Mut,
                    ..
                } = expr
                    && let Some(copy) = copy
                {
                    self.hold_until(LaneExtent::Access(result), copy);
                }
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(value),
                    },
                );
            }
            NExpr::CodeRegionRef { .. } => {
                panic!(
                    "code-region ref reached runtime lowering as a runtime value: owner={:?}; dst={dst:?}; expr={expr:?}",
                    self.current_semantic_key(),
                );
            }
            NExpr::GetEnumTag { value } => {
                self.lower_enum_tag(bb, dst, self.runtime_operand(*value))
            }
            NExpr::IsEnumVariant { value, variant } => {
                self.lower_is_enum_variant(bb, dst, self.runtime_operand(*value), *variant);
            }
            NExpr::CodeRegionOffset { target } => {
                let region = self.lower_code_region_target(target);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::CodeRegionOffset {
                            region,
                        }),
                    },
                );
            }
            NExpr::CodeRegionLen { target } => {
                let region = self.lower_code_region_target(target);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::CodeRegionLen {
                            region,
                        }),
                    },
                );
            }
            NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                let value = self.lower_call(bb, stmt_id, *callee, args, effect_args);
                if self.terminated_blocks[bb.index()] {
                    return;
                }
                let value = self.coerce_value_if_needed(bb, value, &dst_class);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(value),
                    },
                );
            }
        }
    }

    fn lower_code_region_ref(&self, region: &SemanticCodeRegionRef<'db>) -> RuntimeCodeRegion<'db> {
        runtime_code_region_for_semantic_ref(self.db, region)
    }

    fn lower_code_region_target(
        &self,
        target: &SemanticCodeRegionTarget<'db>,
    ) -> RuntimeCodeRegion<'db> {
        match target {
            SemanticCodeRegionTarget::Resolved(region) => self.lower_code_region_ref(region),
            SemanticCodeRegionTarget::Deferred { arg, ty } => {
                panic!(
                    "deferred generic code-region target reached runtime lowering: owner={:?}; arg={arg:?}; ty={ty:?}",
                    self.current_semantic_key(),
                )
            }
        }
    }

    fn lower_const_into(&mut self, bb: RBlockId, dst: RLocalId, const_: &SConst<'db>) {
        let value = match const_ {
            SConst::Value(value) => value.value(),
            SConst::Description(value) | SConst::Evidence(value) => *value,
            SConst::Invalid(_) => unreachable!("invalid constant passed runtime admission"),
            SConst::Ref(cref) => evaluated_const_ref_value(self.db, *cref),
        };
        if sem_const_ty(self.db, value) == TyId::unit(self.db) {
            return;
        }
        let target = self
            .value_class(dst)
            .cloned()
            .expect("const destination should have a runtime class");
        let expected_ty = self.const_lowering_ty(self.locals[dst.index()].semantic_ty, &target);
        self.lower_sem_const_for_class(bb, dst, value, expected_ty, &target);
    }

    fn lower_sem_const_for_class(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
        target: &RuntimeClass<'db>,
    ) {
        let src = self.lower_sem_const_as_class(bb, value, expected_ty, target);
        let value = self.coerce_value_if_needed(bb, src, target);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(value),
            },
        );
    }

    fn lower_sem_const_as_class(
        &mut self,
        bb: RBlockId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
        target: &RuntimeClass<'db>,
    ) -> RLocalId {
        let expected_ty = self.const_lowering_ty(expected_ty, target);
        let value = self.reify_runtime_const(expected_ty, value);
        let ty = expected_ty;
        if matches!(
            target,
            RuntimeClass::Ref {
                kind: RefKind::Const,
                ..
            }
        ) {
            return self.lower_sem_const_as_const_handle(bb, value, ty);
        }
        if let Some(value) = self.try_lower_dyn_string_literal(bb, ty, value) {
            return value;
        }
        if let Some(value) = self.try_lower_const_data(bb, ty, value, target) {
            return value;
        }
        if let Some(value) = self.try_lower_bytes_array_const_as_class(bb, ty, value, target) {
            return value;
        }
        if let Some(scalar) = const_scalar_from_value(self.db, self.env, value) {
            return self.lower_sem_const_scalar(bb, ty, scalar);
        }
        if let RuntimeClass::RawAddr { .. } = target
            && let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(value, &word_scalar_class())
        {
            return self.lower_sem_const_scalar_with_class(bb, ty, target.clone(), scalar);
        }
        if let RuntimeClass::Scalar(class) = target
            && let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(value, class)
        {
            return self.lower_sem_const_scalar(bb, ty, scalar);
        }
        match target {
            RuntimeClass::Scalar(_) => {
                panic!(
                    "non-scalar semantic const {value:?} cannot lower to scalar class {target:?}"
                )
            }
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Object,
                view: RefView::Whole,
            } => {
                assert!(
                    pointee.aggregate_layout().is_some(),
                    "object ref const target should have aggregate layout"
                );
                self.lower_non_scalar_const_as_class(bb, value, ty, target)
            }
            RuntimeClass::Ref {
                kind: RefKind::Provider { .. },
                ..
            } => self.lower_non_scalar_const_as_class(bb, value, ty, target),
            RuntimeClass::Ref { .. } => {
                panic!(
                    "non-scalar semantic const {value:?} cannot lower directly to ref class {target:?}"
                )
            }
            RuntimeClass::AggregateValue { layout: _ } => {
                let src = self.lower_non_scalar_const_as_class(bb, value, expected_ty, target);
                let actual = self.value_class(src).cloned();
                if self.value_class(src) == Some(target)
                    || actual.as_ref().is_some_and(|actual| {
                        self.should_preserve_const_source_class(actual, target)
                    })
                {
                    src
                } else {
                    self.coerce_value(bb, src, target)
                }
            }
            RuntimeClass::RawAddr { .. } => {
                self.lower_non_scalar_const_as_class(bb, value, ty, target)
            }
        }
    }

    fn should_preserve_const_source_class(
        &self,
        actual: &RuntimeClass<'db>,
        target: &RuntimeClass<'db>,
    ) -> bool {
        merge_runtime_class(self.db, actual, target).is_some_and(|merged| &merged == actual)
    }

    fn target_accepts_const_ref_source(&self, target: &RuntimeClass<'db>) -> bool {
        !matches!(
            target,
            RuntimeClass::RawAddr { .. }
                | RuntimeClass::Ref {
                    kind: RefKind::Provider { .. },
                    ..
                }
        )
    }

    fn lower_sem_const_as_value(
        &mut self,
        bb: RBlockId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
    ) -> RLocalId {
        let value = self.reify_runtime_const(expected_ty, value);
        let ty = expected_ty;
        if let Some(value) = self.try_lower_dyn_string_literal(bb, ty, value) {
            return value;
        }
        let aggregate = RuntimeClass::AggregateValue {
            layout: self.layout_for_ty(ty),
        };
        if let Some(value) = self.try_lower_const_data(bb, ty, value, &aggregate) {
            return value;
        }
        if let Some(value) = self.try_lower_bytes_array_const_as_class(bb, ty, value, &aggregate) {
            return value;
        }
        if let Some(scalar) = const_scalar_from_value(self.db, self.env, value) {
            return self.lower_sem_const_scalar(bb, ty, scalar);
        }
        if let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(value, &word_scalar_class())
        {
            return self.lower_sem_const_scalar_with_class(
                bb,
                ty,
                RuntimeClass::Scalar(word_scalar_class()),
                scalar,
            );
        }
        match value.value(self.db) {
            SemConstValue::Tuple { .. }
            | SemConstValue::Struct { .. }
            | SemConstValue::Array { .. }
            | SemConstValue::Enum { .. } => {
                self.lower_non_scalar_const_as_class(bb, value, ty, &aggregate)
            }
            SemConstValue::Unit | SemConstValue::Scalar { .. } | SemConstValue::Description(..) => {
                panic!("semantic const should lower as a natural runtime value: {value:?}")
            }
        }
    }

    fn try_lower_dyn_string_literal(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        value: SemConstId<'db>,
    ) -> Option<RLocalId> {
        let SemConstValue::Scalar {
            value: SemConstScalar::Bytes(bytes),
            ..
        } = value.value(self.db)
        else {
            return None;
        };
        ty.is_core_dyn_string(self.db)
            .then(|| self.lower_dyn_string_literal(bb, ty, bytes))
    }

    /// An aggregate constant with const data reads it from its region.
    fn try_lower_const_data(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        value: SemConstId<'db>,
        target: &RuntimeClass<'db>,
    ) -> Option<RLocalId> {
        if aggregate_const_ref_class(self.db, self.env, value).is_none()
            || !self.target_accepts_const_ref_source(target)
        {
            return None;
        }
        let handle = self.lower_sem_const_as_const_handle(bb, value, ty);
        Some(if self.value_class(handle) == Some(target) {
            handle
        } else {
            self.coerce_value(bb, handle, target)
        })
    }

    fn try_lower_bytes_array_const_as_class(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        value: SemConstId<'db>,
        target: &RuntimeClass<'db>,
    ) -> Option<RLocalId> {
        let SemConstValue::Scalar {
            value: SemConstScalar::Bytes(bytes),
            ..
        } = value.value(self.db)
        else {
            return None;
        };
        if !ty.is_array(self.db) {
            return None;
        }

        let field_tys = self.aggregate_field_tys(ty, bytes.len());
        let mut field_values = Vec::with_capacity(bytes.len());
        let mut field_classes = Vec::with_capacity(bytes.len());
        for (byte, field_ty) in bytes.iter().copied().zip(field_tys) {
            let field_class = self
                .top_level_class_for_ty(field_ty, AddressSpace::Memory)
                .unwrap_or_else(|| {
                    panic!(
                        "bytes array element should have a runtime class: {}",
                        field_ty.pretty_print(self.db)
                    )
                });
            let RuntimeClass::Scalar(ScalarClass {
                repr:
                    ScalarRepr::Int {
                        bits: 8,
                        signed: false,
                    },
                ..
            }) = &field_class
            else {
                panic!("bytes array const requires u8 element class, found {field_class:?}");
            };
            let value = self.lower_sem_const_scalar_with_class(
                bb,
                field_ty,
                field_class.clone(),
                ConstScalar::Int {
                    bits: 8,
                    signed: false,
                    words: if byte == 0 { Vec::new() } else { vec![byte] },
                },
            );
            field_values.push(value);
            field_classes.push(field_class);
        }
        Some(self.lower_aggregate_const_fields_as_class(
            bb,
            ty,
            target,
            &field_values,
            &field_classes,
        ))
    }

    fn lower_dyn_string_literal(&mut self, bb: RBlockId, ty: TyId<'db>, bytes: &[u8]) -> RLocalId {
        let len = self.alloc_u256_const(bb, bytes.len());
        let payload_size = 32 + bytes.len().next_multiple_of(32);
        let size = self.alloc_u256_const(bb, payload_size);
        let ptr = self.malloc_bytes(bb, size);

        let layout = self.layout_for_ty(ty);
        let dst = self.alloc_runtime_temp(
            ty,
            RuntimeCarrier::Value(RuntimeClass::AggregateValue { layout }),
        );
        let ctor_elems = aggregate_ctor_elems_for_layout(self.db, layout, 3);
        self.lower_aggregate_values(bb, dst, layout, &ctor_elems, &[ptr, len, size]);
        // The length is stored apart from the bytes, as a word the backend can
        // forward to later loads of it.
        self.push_ignored_builtin(
            bb,
            crate::runtime::RuntimeBuiltin::Mstore {
                addr: ptr,
                value: len,
            },
        );
        if !bytes.is_empty() {
            let thirty_two = self.alloc_u256_const(bb, 32);
            let data =
                self.alloc_runtime_temp(TyId::u256(self.db), RuntimeCarrier::Value(word_class()));
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: data,
                    expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::PtrOffsetBytes {
                        ptr,
                        offset: thirty_two,
                    }),
                },
            );
            self.push_ignored_builtin(
                bb,
                crate::runtime::RuntimeBuiltin::StoreConstBytes {
                    dst: data,
                    bytes: bytes.into(),
                },
            );
        }
        dst
    }

    fn malloc_bytes(&mut self, bb: RBlockId, size: RLocalId) -> RLocalId {
        let ptr = self.alloc_runtime_temp(
            TyId::ptr_to(self.db, TyId::u8(self.db)),
            RuntimeCarrier::Value(word_class()),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: ptr,
                expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::Malloc { size }),
            },
        );
        ptr
    }

    fn lower_assert_terminator(
        &mut self,
        bb: RBlockId,
        message: Option<hir::hir_def::StringId<'db>>,
    ) -> RTerminator<'db> {
        let payload = if let Some(message) = message {
            solidity_error_string_payload(message.data(self.db).as_bytes())
        } else {
            solidity_panic_payload(0x01)
        };
        let payload_len = self.alloc_u256_const(bb, payload.len());
        let allocated_len = self.alloc_u256_const(bb, payload.len().next_multiple_of(32));
        let ptr = self.malloc_bytes(bb, allocated_len);
        self.push_ignored_builtin(
            bb,
            crate::runtime::RuntimeBuiltin::StoreConstBytes {
                dst: ptr,
                bytes: payload.into(),
            },
        );

        RTerminator::Revert {
            offset: ptr,
            len: payload_len,
        }
    }

    fn alloc_u256_const(&mut self, bb: RBlockId, value: usize) -> RLocalId {
        let local = self.alloc_runtime_temp(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: local,
                expr: RExpr::ConstScalar(ConstScalar::Int {
                    bits: 256,
                    signed: false,
                    words: usize_word_bytes(value),
                }),
            },
        );
        local
    }

    fn push_ignored_builtin(&mut self, bb: RBlockId, builtin: crate::runtime::RuntimeBuiltin<'db>) {
        let sink = self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: sink,
                expr: RExpr::Builtin(builtin),
            },
        );
    }

    fn lower_sem_const_as_const_handle(
        &mut self,
        bb: RBlockId,
        value: SemConstId<'db>,
        ty: TyId<'db>,
    ) -> RLocalId {
        let value = self.reify_runtime_const(ty, value);
        let region = lower_const_region(self.db, self.env, value).unwrap_or_else(|| {
            panic!("const-backed handle should lower to a const region: {value:?}")
        });
        let layout = region.layout(self.db);
        let local =
            self.alloc_runtime_temp(ty, RuntimeCarrier::Value(RuntimeClass::const_ref(layout)));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: local,
                expr: RExpr::ConstRef { region, layout },
            },
        );
        local
    }

    fn lower_sem_const_scalar(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        scalar: ConstScalar,
    ) -> RLocalId {
        let class = self
            .top_level_class_for_ty(ty, AddressSpace::Memory)
            .unwrap_or_else(|| panic!("scalar const should have a runtime class: {ty:?}"));
        self.lower_sem_const_scalar_with_class(bb, ty, class, scalar)
    }

    fn lower_sem_const_scalar_with_class(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        class: RuntimeClass<'db>,
        scalar: ConstScalar,
    ) -> RLocalId {
        let local = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(class));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: local,
                expr: RExpr::ConstScalar(scalar),
            },
        );
        local
    }

    fn lower_non_scalar_const_as_class(
        &mut self,
        bb: RBlockId,
        value: SemConstId<'db>,
        ty: TyId<'db>,
        target: &RuntimeClass<'db>,
    ) -> RLocalId {
        debug_assert!(
            !matches!(value.value(self.db), SemConstValue::Scalar { .. }),
            "scalar const {value:?} with ty {} cannot lower as non-scalar class {target:?}",
            ty.pretty_print(self.db)
        );
        let ty = self.const_lowering_ty(ty, target);
        match value.value(self.db) {
            SemConstValue::Tuple { elems, .. }
            | SemConstValue::Struct { fields: elems, .. }
            | SemConstValue::Array { elems, .. } => {
                let field_tys = self.aggregate_field_tys(ty, elems.len());
                let (field_values, field_classes): (Vec<_>, Vec<_>) = elems
                    .iter()
                    .copied()
                    .zip(field_tys)
                    .map(|(field, field_ty)| self.lower_const_value_field(bb, field, field_ty))
                    .unzip();
                self.lower_aggregate_const_fields_as_class(
                    bb,
                    ty,
                    target,
                    &field_values,
                    &field_classes,
                )
            }
            SemConstValue::Enum {
                variant, fields, ..
            } => {
                let Some(layout) = target.aggregate_layout() else {
                    panic!("enum constant requires an aggregate target: {target:?}");
                };
                let crate::runtime::Layout::Enum(layout_data) = layout.data(self.db) else {
                    panic!("enum constant requires an enum layout");
                };
                let field_tys = ty
                    .as_enum(self.db)
                    .expect("enum constant should have an enum type")
                    .variants(self.db)
                    .nth(variant.0 as usize)
                    .expect("enum variant index should resolve")
                    .field_tys(self.db)
                    .into_iter()
                    .map(|field| field.instantiate(self.db, ty.generic_args(self.db)))
                    .collect::<Vec<_>>();
                let field_values = fields
                    .iter()
                    .copied()
                    .zip(field_tys)
                    .zip(layout_data.variants[variant.0 as usize].fields.iter())
                    .map(|((field, field_ty), field_class)| {
                        self.lower_sem_const_as_class(bb, field, field_ty, field_class)
                    })
                    .collect::<Vec<_>>();
                let dst_class = match target {
                    RuntimeClass::AggregateValue { .. } => RuntimeClass::AggregateValue { layout },
                    RuntimeClass::Ref {
                        kind: RefKind::Object,
                        view: RefView::Whole,
                        ..
                    } => RuntimeClass::object_ref(layout),
                    RuntimeClass::Scalar(_)
                    | RuntimeClass::RawAddr { .. }
                    | RuntimeClass::Ref { .. } => {
                        panic!("enum const cannot lower directly to class {target:?}")
                    }
                };
                let dst = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(dst_class));
                self.lower_enum_values(bb, dst, layout, *variant, &field_values);
                dst
            }
            SemConstValue::Unit | SemConstValue::Scalar { .. } | SemConstValue::Description(..) => {
                panic!("expected non-scalar semantic const, found {value:?}")
            }
        }
    }

    fn lower_aggregate_const_fields_as_class(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        target: &RuntimeClass<'db>,
        field_values: &[RLocalId],
        field_classes: &[RuntimeClass<'db>],
    ) -> RLocalId {
        let layout = layout_for_aggregate_instance_in_env(self.db, self.env, ty, field_classes);
        let ctor_elems = aggregate_ctor_elems_for_layout(self.db, layout, field_values.len());
        let dst_class = match target {
            RuntimeClass::AggregateValue { .. } => RuntimeClass::AggregateValue { layout },
            RuntimeClass::Ref {
                kind: RefKind::Object,
                view: RefView::Whole,
                ..
            } => RuntimeClass::object_ref(layout),
            RuntimeClass::Ref {
                kind: RefKind::Provider { .. },
                ..
            }
            | RuntimeClass::RawAddr { .. } => target.clone(),
            RuntimeClass::Scalar(_)
            | RuntimeClass::Ref {
                kind: RefKind::Const,
                ..
            }
            | RuntimeClass::Ref { .. } => {
                panic!("aggregate const cannot lower directly to class {target:?}")
            }
        };
        let dst = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(dst_class));
        self.lower_aggregate_values(bb, dst, layout, &ctor_elems, field_values);
        dst
    }

    fn aggregate_field_tys(&self, ty: TyId<'db>, arity: usize) -> Vec<TyId<'db>> {
        let instance = self.semantic_body.owner();
        let ty = instance.normalized_ty(self.db, ty);
        let field_tys = if ty.is_array(self.db) {
            let (_, args) = ty.decompose_ty_app(self.db);
            let elem_ty = args.first().copied().expect("array element type");
            vec![elem_ty; arity]
        } else {
            instance.normalized_field_types(self.db, ty).to_vec()
        };
        assert_eq!(
            field_tys.len(),
            arity,
            "aggregate constructor arity mismatch for {}",
            ty.pretty_print(self.db),
        );
        field_tys
    }

    fn lower_aggregate_make(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        ty: TyId<'db>,
        fields: &[NOperand],
    ) {
        let field_tys = self.aggregate_field_tys(ty, fields.len());
        let mut field_values = Vec::with_capacity(fields.len());
        let mut field_classes = Vec::with_capacity(fields.len());
        for (field, field_ty) in fields.iter().copied().zip(field_tys.iter().copied()) {
            let (value, class) = self.lower_ctor_field(bb, field, field_ty);
            field_values.push(value);
            field_classes.push(class);
        }
        let layout = layout_for_aggregate_instance_in_env(self.db, self.env, ty, &field_classes);
        let ctor_elems = aggregate_ctor_elems_for_layout(self.db, layout, fields.len());
        self.lower_aggregate_values(bb, dst, layout, &ctor_elems, &field_values);
    }

    fn lower_array_repeat(&mut self, bb: RBlockId, dst: RLocalId, ty: TyId<'db>, value: NOperand) {
        let len = runtime_array_len(self.db, ty)
            .expect("valid runtime array length")
            .unwrap_or_else(|| {
                panic!(
                    "array repeat with non-concrete length reached runtime lowering: {}",
                    ty.pretty_print(self.db)
                )
            });
        self.lower_aggregate_make(bb, dst, ty, &vec![value; len]);
    }

    fn lower_aggregate_values(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        layout: LayoutId<'db>,
        ctor_elems: &[AggregateCtorElem<'db>],
        field_values: &[RLocalId],
    ) {
        let dst_class = self
            .value_class(dst)
            .cloned()
            .expect("aggregate destination must have a runtime class");
        let dst_class = match dst_class {
            RuntimeClass::AggregateValue { .. } => RuntimeClass::AggregateValue { layout },
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            } => RuntimeClass::object_ref(layout),
            class @ (RuntimeClass::Ref {
                kind: RefKind::Provider { .. } | RefKind::Const | RefKind::Native,
                ..
            }
            | RuntimeClass::RawAddr { .. }) => class,
            RuntimeClass::Scalar(_) => panic!("aggregate destination must not be scalar"),
        };
        match dst_class {
            RuntimeClass::AggregateValue { .. } => {
                let fields = field_values
                    .iter()
                    .copied()
                    .zip(ctor_elems.iter())
                    .map(|(value, elem)| {
                        if self.class_is_runtime_zst(&elem.class) {
                            value
                        } else {
                            self.coerce_value(bb, value, &elem.class)
                        }
                    })
                    .collect();
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::AggregateMake { layout, fields },
                    },
                );
            }
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            } => {
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::AllocObject { layout },
                    },
                );
                for (value, elem) in field_values.iter().copied().zip(ctor_elems.iter()) {
                    if self.value_class(value).is_none() {
                        continue;
                    }
                    let place = RuntimePlace {
                        root: PlaceRoot::Ref(dst),
                        path: vec![elem.elem.clone()].into_boxed_slice(),
                    };
                    self.write_value_to_place(bb, place, value, &elem.class);
                }
            }
            class @ RuntimeClass::Ref {
                kind: RefKind::Provider { .. },
                ..
            }
            | class @ RuntimeClass::RawAddr { .. } => panic!(
                "aggregate construction must produce an aggregate representation, found {class:?} for {}",
                self.locals[dst.index()].semantic_ty.pretty_print(self.db),
            ),
            RuntimeClass::Ref {
                kind: RefKind::Const,
                ..
            } => {
                panic!("aggregate construction must not target const refs");
            }
            class => {
                panic!(
                    "aggregate construction requires object/provider/raw destination, found {class:?}"
                )
            }
        }
    }

    /// The field types of `variant` of `enum_ty`, a constructor of `arity`
    /// fields.
    fn enum_variant_field_tys(
        &self,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        arity: usize,
    ) -> Vec<TyId<'db>> {
        let field_tys = self
            .semantic_body
            .owner()
            .normalized_enum_variant_field_tys(self.db, enum_ty, variant)
            .to_vec();
        assert_eq!(
            field_tys.len(),
            arity,
            "enum constructor arity mismatch for {}::{variant:?}",
            enum_ty.pretty_print(self.db),
        );
        field_tys
    }

    fn lower_enum_make(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: &[NOperand],
    ) {
        let field_tys = self.enum_variant_field_tys(enum_ty, variant, fields.len());
        let mut field_values = Vec::with_capacity(fields.len());
        let mut field_classes = Vec::with_capacity(fields.len());
        for (field, field_ty) in fields.iter().copied().zip(field_tys.iter().copied()) {
            let (value, class) = self.lower_ctor_field(bb, field, field_ty);
            field_values.push(value);
            field_classes.push(class);
        }
        let layout = layout_for_enum_variant_instance_in_env(
            self.db,
            self.env,
            enum_ty,
            variant.0 as usize,
            &field_classes,
        );
        self.lower_enum_values(bb, dst, layout, variant, &field_values);
    }

    fn lower_ctor_field(
        &mut self,
        bb: RBlockId,
        field: NOperand,
        field_ty: TyId<'db>,
    ) -> (RLocalId, RuntimeClass<'db>) {
        let stored = stored_class_for_ty_in_env(self.db, self.env, field_ty);
        if self.class_is_runtime_zst(&stored) {
            let value = self.lower_zst_value_placeholder(bb, field_ty, stored.clone());
            return (value, stored);
        }
        let boundary =
            boundary_spec_for_ty_in_env(self.db, self.env, field_ty, AddressSpace::Memory);
        let runtime_field = self.runtime_operand(field);
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        // The selection inference classified the field by.
        let selected = self.with_current_body_cx(|cx| {
            let mut class_cache = InferClassCache::new(self.semantic_body.locals.len());
            RuntimeArgSelector::new(cx.env, cx.carriers, Some(&mut class_cache))
                .with_concrete_roots(&roots)
                .with_concrete_value_classes(&value_classes)
                .selected_ctor_field(runtime_field, boundary.as_ref())
        });
        // With no source selected, inference gave the field its stored class.
        let value = match selected {
            Some(selected) => self.lower_selected_runtime_arg(bb, field, &selected),
            None => self.lower_semantic_operand_for_class(bb, field, &stored),
        };
        let class = self.value_class(value).cloned().unwrap_or(stored);
        (value, class)
    }

    fn lower_const_value_field(
        &mut self,
        bb: RBlockId,
        field: SemConstId<'db>,
        field_ty: TyId<'db>,
    ) -> (RLocalId, RuntimeClass<'db>) {
        let stored = stored_class_for_ty_in_env(self.db, self.env, field_ty);
        if self.class_is_runtime_zst(&stored) {
            let value = self.lower_zst_value_placeholder(bb, field_ty, stored.clone());
            return (value, stored);
        }
        let value = self.lower_sem_const_as_value(bb, field, field_ty);
        let class = self.value_class(value).cloned().unwrap_or(stored);
        (value, class)
    }

    fn lower_zst_value_placeholder(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> RLocalId {
        let value = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(class.clone()));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: value,
                expr: RExpr::Placeholder { class },
            },
        );
        value
    }

    fn lower_enum_values(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        ctor_layout: LayoutId<'db>,
        variant: VariantIndex,
        field_values: &[RLocalId],
    ) {
        let dst_class = self
            .value_class(dst)
            .cloned()
            .expect("enum destination must have a runtime class");
        let layout = match &dst_class {
            RuntimeClass::AggregateValue { layout } => *layout,
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Object,
                ..
            } => pointee
                .aggregate_layout()
                .expect("object enum destination must have aggregate layout"),
            _ => ctor_layout,
        };
        let variant = VariantId {
            enum_layout: layout,
            index: variant.0,
        };
        let field_values = match layout.data(self.db) {
            crate::runtime::Layout::Enum(layout_data) => field_values
                .iter()
                .copied()
                .zip(layout_data.variants[variant.index as usize].fields.iter())
                .map(|(value, class)| {
                    if self.value_class(value) == Some(class) {
                        value
                    } else {
                        self.coerce_value(bb, value, class)
                    }
                })
                .collect::<Vec<_>>(),
            crate::runtime::Layout::Struct(_) | crate::runtime::Layout::Array(_) => {
                panic!("enum destination must use an enum layout")
            }
        };
        match dst_class {
            RuntimeClass::AggregateValue { .. } => {
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumMake {
                            layout,
                            variant,
                            fields: field_values.into_boxed_slice(),
                        },
                    },
                );
            }
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            } => {
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::AllocObject { layout },
                    },
                );
                self.push_stmt(
                    bb,
                    if field_values.is_empty() {
                        RStmt::EnumSetTag { root: dst, variant }
                    } else {
                        RStmt::EnumWriteVariant {
                            root: dst,
                            variant,
                            fields: field_values.into_boxed_slice(),
                        }
                    },
                );
            }
            class => panic!(
                "enum construction requires aggregate or object-ref destination, found {class:?}"
            ),
        }
    }

    fn lower_semantic_place_read_into(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        place: &NPlace<'db>,
        class: &RuntimeClass<'db>,
    ) {
        if self.lower_value_extract_place_read(bb, dst, place) {
            return;
        }
        match self.resolve_place(bb, place) {
            ResolvedPlace::Lane(lane) => {
                let value = self.read_lane(bb, &lane);
                let value = self.coerce_value(bb, value, class);
                self.push_value_use(bb, dst, value);
            }
            ResolvedPlace::Place(place) => {
                self.lower_runtime_place_read_into(bb, dst, place, class)
            }
        }
    }

    fn lower_value_extract_place_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        place: &NPlace<'db>,
    ) -> bool {
        let base = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => {
                let Some(local) = self.semantic_body.value_local(carrier) else {
                    return false;
                };
                let value = self.normalized_value_temps[carrier.index()]
                    .unwrap_or_else(|| self.runtime_value(local));
                let Some(class) = self.value_class(value).cloned() else {
                    return false;
                };
                // Capability targets load through their emitted transport. A
                // normalized call result may not share its source local's home.
                if class.is_transport()
                    || !matches!(self.locals[value.index()].root, RuntimeLocalRoot::None)
                {
                    return false;
                }
                return self.lower_value_extract_from_value(
                    bb,
                    dst,
                    value,
                    class,
                    &place.path.iter().copied().collect::<Vec<_>>(),
                );
            }
            NPlaceBase::Deref { .. } => return false,
            NPlaceBase::Root(root_id) => match self.semantic_body.normalized.root(root_id) {
                Some(root) => match &root.kind {
                    NRootKind::LocalSlot { .. }
                    | NRootKind::Temporary { .. }
                    | NRootKind::ParamPlace { .. } => {
                        let Some(local) = self.semantic_body.root_local(root_id) else {
                            return false;
                        };
                        local
                    }
                    NRootKind::Provider { .. } => return false,
                    NRootKind::CapabilityRepresentation { carrier } => {
                        let Some(local) = self.semantic_body.value_local(*carrier) else {
                            return false;
                        };
                        local
                    }
                },
                None => return false,
            },
        };
        let path = place.path.iter().copied().collect::<Vec<_>>();
        self.lower_value_extract_path_read(bb, dst, base, &path)
    }

    fn lower_value_extract_path_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        base: SLocalId,
        path: &[NDataProjection],
    ) -> bool {
        if !matches!(self.locals[base.index()].root, RuntimeLocalRoot::None) {
            return false;
        }
        let value = self.runtime_value(base);
        let Some(class) = self.value_class(value).cloned() else {
            return false;
        };
        self.lower_value_extract_from_value(bb, dst, value, class, path)
    }

    fn lower_value_extract_from_value(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        mut value: RLocalId,
        mut class: RuntimeClass<'db>,
        path: &[NDataProjection],
    ) -> bool {
        if path.is_empty() {
            let target = self
                .value_class(dst)
                .cloned()
                .expect("value read result must have class");
            let value = self.coerce_value(bb, value, &target);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Use(value),
                },
            );
            return true;
        }
        if let Some(mut place) = self.runtime_place_from_addr_value(value) {
            let mut current = self.project_place_class(&place);
            let mut projected = Vec::with_capacity(path.len());
            for projection in path {
                match projection {
                    NDataProjection::Field(field) => {
                        projected.push(PlaceElem::Field(*field));
                        current = project_field_class(self.db, current, *field);
                    }
                    NDataProjection::VariantField { variant, field } => {
                        let variant = VariantId {
                            enum_layout: current
                                .aggregate_layout()
                                .expect("variant-field value projection must use an enum layout"),
                            index: variant.0,
                        };
                        projected.push(PlaceElem::VariantField {
                            variant,
                            field: *field,
                        });
                        current = project_variant_field_class(self.db, current, variant, *field);
                    }
                    NDataProjection::Index(NIndex::Const(index)) => {
                        projected.push(PlaceElem::Index(IndexSource::Constant(*index)));
                        current = project_index_class(self.db, current);
                    }
                    NDataProjection::Index(NIndex::Value(index)) => {
                        projected.push(PlaceElem::Index(IndexSource::Dynamic(
                            self.read_normalized_value(bb, *index),
                        )));
                        current = project_index_class(self.db, current);
                    }
                    NDataProjection::Entry(_) => {
                        unreachable!("an entry is a place step, never a value projection")
                    }
                }
            }
            place.path = projected.into_boxed_slice();
            let target = self
                .value_class(dst)
                .cloned()
                .expect("value extract result must have class");
            self.lower_runtime_place_read_into(bb, dst, place, &target);
            return true;
        }
        if path
            .iter()
            .any(|projection| matches!(projection, NDataProjection::Index(NIndex::Value(_))))
            && let RuntimeClass::AggregateValue { layout } = class
        {
            let materialized = RuntimeClass::object_ref(layout);
            value = self.coerce_value(bb, value, &materialized);
            return self.lower_value_extract_from_value(bb, dst, value, materialized, path);
        }
        if !matches!(class, RuntimeClass::AggregateValue { .. }) {
            return false;
        }
        for (path_idx, elem) in path.iter().enumerate() {
            let step = match elem {
                NDataProjection::Field(field) => {
                    let field = *field;
                    ValueExtractStep::Aggregate {
                        index: u32::from(field.0),
                        class: project_field_class(self.db, class, field),
                    }
                }
                NDataProjection::Index(NIndex::Const(index)) => {
                    let index = (*index).try_into().expect("constant index fits in u32");
                    ValueExtractStep::Aggregate {
                        index,
                        class: project_index_class(self.db, class),
                    }
                }
                NDataProjection::VariantField { variant, field } => {
                    let field = *field;
                    let variant = VariantId {
                        enum_layout: class
                            .aggregate_layout()
                            .expect("variant-field value extract should project from enum layout"),
                        index: variant.0,
                    };
                    ValueExtractStep::EnumField {
                        variant,
                        field,
                        class: project_variant_field_class(self.db, class, variant, field),
                    }
                }
                NDataProjection::Index(NIndex::Value(_)) => return false,
                NDataProjection::Entry(_) => {
                    unreachable!("an entry is a place step, never a value projection")
                }
            };
            let extracted_class = match &step {
                ValueExtractStep::Aggregate { class, .. }
                | ValueExtractStep::EnumField { class, .. } => class.clone(),
            };
            let is_last = path_idx + 1 == path.len();
            if is_last {
                let target = self
                    .value_class(dst)
                    .cloned()
                    .expect("value extract result must have class");
                if extracted_class == target {
                    self.push_value_extract_step(bb, dst, value, step);
                } else {
                    let temp = self.alloc_runtime_temp(
                        self.locals[dst.index()].semantic_ty,
                        RuntimeCarrier::Value(extracted_class),
                    );
                    self.push_value_extract_step(bb, temp, value, step);
                    let copied = self.coerce_value(bb, temp, &target);
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::Use(copied),
                        },
                    );
                }
                return true;
            }
            let temp = self.alloc_runtime_temp(
                self.locals[dst.index()].semantic_ty,
                RuntimeCarrier::Value(extracted_class.clone()),
            );
            self.push_value_extract_step(bb, temp, value, step);
            value = temp;
            class = extracted_class;
        }
        false
    }

    fn lower_runtime_place_read_into(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        place: RuntimePlace<'db>,
        target: &RuntimeClass<'db>,
    ) {
        let projected = self.project_place_class(&place);
        if matches!(projected, RuntimeClass::AggregateValue { .. })
            && is_immutable_reference(target)
            && self.place_addr_class(&place) == *target
        {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::AddrOf { place },
                },
            );
        } else if let (
            RuntimeClass::AggregateValue { layout },
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Object,
                view: RefView::Whole,
            },
        ) = (&projected, target)
            && **pointee == (RuntimeClass::AggregateValue { layout: *layout })
        {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::MaterializePlaceToObject { place },
                },
            );
        } else if &projected == target {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Load { place },
                },
            );
        } else {
            let source = self.alloc_runtime_temp(
                self.locals[dst.index()].semantic_ty,
                RuntimeCarrier::Value(projected),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: source,
                    expr: RExpr::Load { place },
                },
            );
            let copied = self.coerce_value(bb, source, target);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Use(copied),
                },
            );
        }
    }

    fn push_value_extract_step(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: RLocalId,
        step: ValueExtractStep<'db>,
    ) {
        let expr = match step {
            ValueExtractStep::Aggregate { index, .. } => RExpr::AggregateExtract { value, index },
            ValueExtractStep::EnumField { variant, field, .. } => {
                self.push_stmt(bb, RStmt::EnumAssertVariant { value, variant });
                RExpr::EnumExtract {
                    value,
                    variant,
                    field,
                }
            }
        };
        self.push_stmt(bb, RStmt::Assign { dst, expr });
    }

    fn enum_tag_expr(&mut self, bb: RBlockId, value: RuntimeOperand) -> RExpr<'db> {
        if matches!(
            self.semantic_local_lowering(value.local),
            RuntimeLocalLowering::PlaceCarrier {
                place_class: RuntimeClass::AggregateValue { .. },
            } | RuntimeLocalLowering::PlaceBoundValue {
                place_class: RuntimeClass::AggregateValue { .. },
                ..
            }
        ) {
            // Query the same place a value read would use, without copying its payload.
            // Value extracts still read the captured value, rather than its source place.
            let place = match self.semantic_place_value_source(value.local) {
                Some(SemanticPlaceValueSource::PlaceValue { place, .. }) => {
                    Some(self.lower_place(bb, &place))
                }
                Some(SemanticPlaceValueSource::ValueExtract { .. }) => None,
                None => Some(self.semantic_place(bb, value.local)),
            };
            if let Some(place) = place {
                return RExpr::EnumGetTag { place };
            }
        }
        let value = self.read_semantic_value(bb, value.local);
        if matches!(self.value_class(value), Some(RuntimeClass::Ref { .. })) {
            RExpr::EnumGetTag {
                place: RuntimePlace {
                    root: PlaceRoot::Ref(value),
                    path: Box::default(),
                },
            }
        } else {
            RExpr::EnumTagOfValue { value }
        }
    }

    fn lower_enum_tag(&mut self, bb: RBlockId, dst: RLocalId, value: RuntimeOperand) {
        let expr = self.enum_tag_expr(bb, value);
        self.push_stmt(bb, RStmt::Assign { dst, expr });
    }

    fn lower_is_enum_variant(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: RuntimeOperand,
        variant: VariantIndex,
    ) {
        let semantic_ty = self.locals[value.local.index()].semantic_ty;
        let enum_layout = self.enum_layout_for_local(value.local);
        let variant_id = self.enum_variant_for_local(value.local, variant);
        let expr = self.enum_tag_expr(bb, value);
        if matches!(expr, RExpr::EnumGetTag { .. }) {
            let tag_class = RuntimeClass::Scalar(ScalarClass {
                repr: match enum_layout.data(self.db) {
                    crate::runtime::Layout::Enum(layout) => layout.tag.repr,
                    _ => unreachable!(),
                },
                role: ScalarRole::EnumTag { enum_layout },
            });
            let tag = self.alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(tag_class));
            self.push_stmt(bb, RStmt::Assign { dst: tag, expr });
            let expected = self.alloc_runtime_temp(
                semantic_ty,
                RuntimeCarrier::Value(
                    self.value_class(tag)
                        .cloned()
                        .expect("enum tag temp should have a class"),
                ),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: expected,
                    expr: RExpr::ConstScalar(
                        enum_tag_scalar(self.db, enum_layout, variant)
                            .expect("enum variant should lower to a tag scalar"),
                    ),
                },
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Binary {
                        op: hir::hir_def::BinOp::Comp(hir::hir_def::CompBinOp::Eq),
                        lhs: tag,
                        rhs: expected,
                    },
                },
            );
        } else {
            let RExpr::EnumTagOfValue { value } = expr else {
                unreachable!("enum tag query must read a value or a place");
            };
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::EnumIsVariant {
                        value,
                        variant: variant_id,
                    },
                },
            );
        }
    }

    fn alloc_zero_scalar(
        &mut self,
        bb: RBlockId,
        semantic_ty: TyId<'db>,
        class: &ScalarClass<'db>,
    ) -> RLocalId {
        let zero = self.alloc_runtime_temp(
            semantic_ty,
            RuntimeCarrier::Value(RuntimeClass::Scalar(class.clone())),
        );
        let ScalarRepr::Int { bits, signed } = class.repr else {
            panic!("checked neg requires an integer scalar class");
        };
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: zero,
                expr: RExpr::ConstScalar(ConstScalar::Int {
                    bits,
                    signed,
                    words: Vec::new(),
                }),
            },
        );
        zero
    }

    fn lower_intrinsic_arith_expr(
        &mut self,
        op: IntrinsicArithBinOp,
        checked: bool,
        lhs: RLocalId,
        rhs: RLocalId,
        class: &ScalarClass<'db>,
    ) -> RExpr<'db> {
        RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
            op,
            checked,
            lhs,
            rhs,
            class: class.clone(),
        })
    }

    fn lower_arith_expr_for_mode(
        &mut self,
        bb: RBlockId,
        op: ArithBinOp,
        checked: bool,
        lhs: RLocalId,
        rhs: RLocalId,
        class: &ScalarClass<'db>,
    ) -> Option<RExpr<'db>> {
        let _ = bb;
        Some(match op {
            ArithBinOp::Add => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Add, checked, lhs, rhs, class)
            }
            ArithBinOp::Sub => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Sub, checked, lhs, rhs, class)
            }
            ArithBinOp::Mul => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Mul, checked, lhs, rhs, class)
            }
            ArithBinOp::Div => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Div, checked, lhs, rhs, class)
            }
            ArithBinOp::Rem => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Rem, checked, lhs, rhs, class)
            }
            ArithBinOp::Pow => {
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Pow, checked, lhs, rhs, class)
            }
            ArithBinOp::BitAnd
            | ArithBinOp::BitOr
            | ArithBinOp::BitXor
            | ArithBinOp::LShift
            | ArithBinOp::RShift => RExpr::Binary {
                op: BinOp::Arith(op),
                lhs,
                rhs,
            },
            ArithBinOp::Range => return None,
        })
    }

    fn lower_unary_expr_for_mode(
        &mut self,
        bb: RBlockId,
        op: UnOp,
        checked: bool,
        value: RLocalId,
        semantic_ty: TyId<'db>,
        class: &ScalarClass<'db>,
    ) -> Option<RExpr<'db>> {
        Some(match op {
            UnOp::Minus if checked => {
                let zero = self.alloc_zero_scalar(bb, semantic_ty, class);
                self.lower_intrinsic_arith_expr(IntrinsicArithBinOp::Sub, true, zero, value, class)
            }
            UnOp::Minus | UnOp::BitNot | UnOp::Not => RExpr::Unary { op, value },
            UnOp::Plus | UnOp::Mut | UnOp::MaybeMut | UnOp::Ref | UnOp::Deref => return None,
        })
    }

    fn runtime_place_from_addr_value(&self, value: RLocalId) -> Option<RuntimePlace<'db>> {
        match self.value_class(value)? {
            RuntimeClass::Ref { .. } => Some(RuntimePlace {
                root: PlaceRoot::Ref(value),
                path: Box::default(),
            }),
            RuntimeClass::RawAddr {
                space,
                pointee: Some(pointee),
            } => Some(RuntimePlace {
                root: PlaceRoot::Ptr {
                    addr: value,
                    space: *space,
                    class: pointee.target(self.db).clone(),
                },
                path: Box::default(),
            }),
            RuntimeClass::RawAddr { pointee: None, .. } => None,
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => None,
        }
    }

    fn lower_call(
        &mut self,
        bb: RBlockId,
        stmt_id: NStatementId,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> RLocalId {
        let semantic = SemanticInstance::new(self.db, callee.key);
        if let Some(ret) = self.lower_extern_builtin_call(bb, semantic, args, effect_args) {
            return ret;
        }
        let mut boundary_sites = BoundarySiteAllocator::default();
        let call_input_plan = compile_call_input_plan_for_semantic(
            self.db,
            &self.semantic_body,
            semantic,
            self.env,
            effect_args,
            &mut boundary_sites,
        );
        let (runtime_args, runtime_classes) =
            self.lower_runtime_call_inputs(bb, args, effect_args, &call_input_plan);
        // A lane a `mut` effect argument provides is passed as a copy, which
        // the callee may update: it is stored back when the call returns, or
        // when a projection's session ends.
        let lane_copies: Vec<_> = std::mem::take(&mut self.call_lane_copies)
            .into_iter()
            .filter(|(place, _)| {
                effect_args.iter().any(|effect| {
                    effect.required_mut
                        && matches!(&effect.arg, NEffectArgValue::Place(arg) if arg == place)
                })
            })
            .map(|(_, copy)| copy)
            .collect();
        let runtime_key = RuntimeInstanceKey::new(
            self.db,
            crate::instance::RuntimeInstanceSource::Semantic(semantic),
            runtime_classes,
        );
        let signature = runtime_declaration_signature(self.db, runtime_key);
        let callee = get_or_build_runtime_instance(self.db, runtime_key, self.target());
        if callee.exit_behavior(self.db) == RuntimeExitBehavior::NeverReturns {
            self.set_terminator(
                bb,
                RTerminator::TerminalCall {
                    callee,
                    args: runtime_args.into_boxed_slice(),
                },
            );
            return self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased);
        }
        let ret_ty = semantic_return_ty(self.db, semantic);
        let call_result = self.alloc_runtime_temp(
            ret_ty,
            signature
                .ret
                .map_or(RuntimeCarrier::Erased, RuntimeCarrier::Value),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: call_result,
                expr: RExpr::Call {
                    callee,
                    args: runtime_args.into_boxed_slice(),
                },
            },
        );
        if semantic.is_projection(self.db) {
            self.sessions.insert(stmt_id, call_result);
            for copy in lane_copies {
                self.hold_until(LaneExtent::Session(call_result), copy);
            }
        } else {
            for copy in lane_copies {
                self.store_lane_copy(bb, copy);
            }
        }
        call_result
    }

    fn lower_stmt_index_checks(&mut self, bb: RBlockId, stmt: &NStatementKind<'db>) {
        match stmt {
            NStatementKind::Define { expr, .. } => {
                expr.for_each_place_operand(|place| self.lower_place_index_checks(bb, place));
                if let NExpr::ProjectValue { value, path } = expr {
                    let ty = self
                        .semantic_body
                        .normalized
                        .value(value.value)
                        .expect("verified structural projection source exists")
                        .ty;
                    self.lower_data_path_index_checks(bb, ty, &path.0);
                }
            }
            NStatementKind::Store { destination, .. } => {
                self.lower_place_index_checks(bb, destination)
            }
            NStatementKind::End { .. } | NStatementKind::Drop { .. } => {}
        }
    }

    fn lower_place_index_checks(&mut self, bb: RBlockId, place: &NPlace<'db>) {
        if !place
            .path
            .iter()
            .any(|projection| matches!(projection, NDataProjection::Index(_)))
        {
            return;
        }
        let Some(ty) = self.semantic_place_root_ty(place) else {
            return;
        };
        self.lower_data_path_index_checks(bb, ty, &place.path);
    }

    fn lower_data_path_index_checks(&mut self, bb: RBlockId, ty: TyId<'db>, path: &NDataPath) {
        if self.terminated_blocks[bb.index()] {
            return;
        }
        for (index, len) in data_path_index_bounds(self.db, self.semantic_body.owner(), ty, path) {
            let index = match index {
                NIndex::Const(index) => IndexSource::Constant(index),
                NIndex::Value(index) => IndexSource::Dynamic(self.read_normalized_value(bb, index)),
            };
            self.push_stmt(
                bb,
                RStmt::AssertIndexInBounds {
                    index,
                    len: len
                        .try_into()
                        .expect("array length must fit the runtime index representation"),
                },
            );
            if len == 0 {
                self.set_terminator(bb, RTerminator::Trap);
                return;
            }
        }
    }

    fn semantic_place_root_ty(&self, place: &NPlace<'db>) -> Option<TyId<'db>> {
        self.semantic_body
            .normalized
            .place_base_ty(self.db, place.base)
    }

    fn lower_extern_builtin_call(
        &mut self,
        bb: RBlockId,
        semantic: SemanticInstance<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Option<RLocalId> {
        let BodyOwner::Func(func, _) = semantic.key(self.db).owner(self.db) else {
            return None;
        };

        if let Some(ret) = self.lower_intrinsic_keccak256_call(bb, semantic, func, args) {
            return Some(ret);
        }
        if let Some(ret) = self.lower_intrinsic_keccak_words_call(bb, func, args) {
            return Some(ret);
        }
        if let Some(builtin) = contract_metadata_builtin(self.db, semantic) {
            let ret_ty = semantic_return_ty(self.db, semantic);
            let class = RuntimeClass::Scalar(ScalarClass {
                repr: ScalarRepr::Int {
                    bits: 256,
                    signed: false,
                },
                role: ScalarRole::Plain,
            });
            let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(class.clone()));
            let builtin = match builtin {
                ContractMetadataBuiltin::InitCodeOffset(region) => {
                    crate::runtime::RuntimeBuiltin::CodeRegionOffset { region }
                }
                ContractMetadataBuiltin::InitCodeLen(region) => {
                    crate::runtime::RuntimeBuiltin::CodeRegionLen { region }
                }
            };
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: ret,
                    expr: RExpr::Builtin(builtin),
                },
            );
            return Some(ret);
        }
        if let Some(ret) = self.lower_numeric_intrinsic_call(bb, semantic, args) {
            return Some(ret);
        }
        let kind = runtime_builtin_func_kind(self.db, func)?;

        let mut boundary_sites = BoundarySiteAllocator::default();
        let call_input_plan = compile_call_input_plan_for_semantic(
            self.db,
            &self.semantic_body,
            semantic,
            self.env,
            &[],
            &mut boundary_sites,
        );
        let (args, _) = self.lower_visible_call_args(bb, args, &call_input_plan);
        if kind == RuntimeBuiltinFuncKind::PanicWithValue {
            let [value] = args.as_slice() else {
                return None;
            };
            return Some(self.lower_panic_with_value(bb, *value));
        }
        if kind == RuntimeBuiltinFuncKind::PanicCode {
            let [code] = args.as_slice() else {
                return None;
            };
            return Some(self.lower_panic_code(bb, *code));
        }
        // The argument moved into the call, which destroys nothing; EVM
        // memory is never reclaimed.
        if kind == RuntimeBuiltinFuncKind::Forget
            || kind == RuntimeBuiltinFuncKind::Free && self.target() == RuntimeTarget::Evm
        {
            return Some(self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased));
        }
        // EVM memory is big-endian, so there a word is its own big-endian
        // representation.
        if matches!(
            kind,
            RuntimeBuiltinFuncKind::ToBigEndian | RuntimeBuiltinFuncKind::FromBigEndian
        ) && self.target() == RuntimeTarget::Evm
            && let [value] = args.as_slice()
        {
            let class = self.value_class(*value)?.clone();
            let ret = self.alloc_runtime_temp(
                semantic_return_ty(self.db, semantic),
                RuntimeCarrier::Value(class),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: ret,
                    expr: RExpr::Use(*value),
                },
            );
            return Some(ret);
        }
        if kind == RuntimeBuiltinFuncKind::NeedsDrop {
            let ty = semantic.key(self.db).subst(self.db).generic_args(self.db)[0];
            let needs_drop = self.needs_drop(ty);
            let ret = self.alloc_bool_temp();
            self.assign_bool(bb, ret, needs_drop);
            return Some(ret);
        }
        if kind == RuntimeBuiltinFuncKind::Clear {
            // A zero-sized place is erased: clearing it stores nothing.
            if let [place] = args.as_slice() {
                self.lower_clear(bb, *place);
            }
            return Some(self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased));
        }
        let lowered = self.lower_extern_builtin(semantic, &args)?;
        let ret_ty = semantic_return_ty(self.db, semantic);
        let _ = effect_args;
        Some(match lowered {
            LoweredBuiltinCall::Expr { builtin, class } => {
                let ret = class
                    .clone()
                    .map(|class| self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(class)))
                    .unwrap_or_else(|| {
                        self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased)
                    });
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: ret,
                        expr: RExpr::Builtin(builtin),
                    },
                );
                ret
            }
            LoweredBuiltinCall::Binary { op, lhs, rhs } => {
                let class = self.top_level_class_for_ty(ret_ty, AddressSpace::Memory)?;
                let word = RuntimeClass::Scalar(word_scalar_class());
                let lhs = self.coerce_value(bb, lhs, &word);
                let rhs = self.coerce_value(bb, rhs, &word);
                let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(class));
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: ret,
                        expr: RExpr::Binary { op, lhs, rhs },
                    },
                );
                ret
            }
            LoweredBuiltinCall::Terminator(terminator) => {
                self.set_terminator(bb, terminator);
                self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased)
            }
        })
    }

    /// Reverts with Solidity `Panic(uint256)` data for a runtime `code`.
    fn lower_panic_code(&mut self, bb: RBlockId, code: RLocalId) -> RLocalId {
        // The call reverts, so the payload can use scratch memory at 0 like
        // the checked-arithmetic panics do.
        let word = RuntimeClass::Scalar(word_scalar_class());
        let code = self.coerce_value(bb, code, &word);
        let ptr = self.alloc_u256_const(bb, 0);
        let selector = self.alloc_runtime_temp(TyId::u256(self.db), RuntimeCarrier::Value(word));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: selector,
                expr: RExpr::ConstScalar(ConstScalar::Int {
                    bits: 256,
                    signed: false,
                    words: padded_word_bytes(&solidity_panic_payload(0)[..4]),
                }),
            },
        );
        self.push_ignored_builtin(
            bb,
            crate::runtime::RuntimeBuiltin::Mstore {
                addr: ptr,
                value: selector,
            },
        );
        let code_addr = self.alloc_u256_const(bb, 4);
        self.push_ignored_builtin(
            bb,
            crate::runtime::RuntimeBuiltin::Mstore {
                addr: code_addr,
                value: code,
            },
        );
        let len = self.alloc_u256_const(bb, 36);
        self.set_terminator(bb, RTerminator::Revert { offset: ptr, len });
        self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased)
    }

    fn lower_panic_with_value(&mut self, bb: RBlockId, value: RLocalId) -> RLocalId {
        let value_ty = self.locals[value.index()].semantic_ty;
        let revert = self.resolve_panic_payload_revert(value_ty);
        let [param_class] = revert.key(self.db).params(self.db).as_slice() else {
            panic!("panic payload revert should have one runtime parameter");
        };
        let value = self.coerce_value_if_needed(bb, value, param_class);
        self.set_terminator(
            bb,
            RTerminator::TerminalCall {
                callee: revert,
                args: Box::new([value]),
            },
        );
        self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased)
    }

    fn resolve_panic_payload_revert(&self, value_ty: TyId<'db>) -> RuntimeInstance<'db> {
        let key = self.current_semantic_key();
        let impl_env = key.impl_env(self.db);
        let scope = impl_env.normalization_scope(self.db);
        let assumptions = impl_env.assumptions(self.db);
        self.assert_panic_payload_encodable(scope, assumptions, value_ty);
        let func_path = if panic_payload_is_error_variant(self.db, scope, assumptions, value_ty) {
            "std::evm::revert_error"
        } else {
            "std::evm::revert"
        };
        let func = resolve_lib_func_path(self.db, scope, func_path)
            .unwrap_or_else(|| panic!("missing {func_path}"));
        let semantic_key =
            generated_callee_key(self.db, scope, assumptions, func, None, &[value_ty])
                .unwrap_or_else(|error| panic!("{}", generated_call_error(self.db, func, error)));
        runtime_instance_for_semantic(
            self.db,
            SemanticInstance::new(self.db, semantic_key),
            self.target(),
        )
    }

    fn assert_panic_payload_encodable(
        &self,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        value_ty: TyId<'db>,
    ) {
        ensure_panic_payload_encodable(self.db, scope, assumptions, value_ty)
            .expect("panic payload support should be checked before rMIR emission");
    }

    fn lower_numeric_intrinsic_call(
        &mut self,
        bb: RBlockId,
        semantic: SemanticInstance<'db>,
        args: &[NOperand],
    ) -> Option<RLocalId> {
        let BodyOwner::Func(func, _) = semantic.key(self.db).owner(self.db) else {
            return None;
        };
        if func.body(self.db).is_some() {
            return None;
        }
        let name = func.name(self.db).to_opt()?.data(self.db);
        let mut boundary_sites = BoundarySiteAllocator::default();
        let call_input_plan = compile_call_input_plan_for_semantic(
            self.db,
            &self.semantic_body,
            semantic,
            self.env,
            &[],
            &mut boundary_sites,
        );
        let (args, _) = self.lower_visible_call_args(bb, args, &call_input_plan);
        let ret_ty = semantic_return_ty(self.db, semantic);
        let ret_class = self.top_level_class_for_ty(ret_ty, AddressSpace::Memory)?;
        let scalar = match &ret_class {
            RuntimeClass::Scalar(scalar) => scalar.clone(),
            RuntimeClass::Ref { .. } => return None,
            RuntimeClass::AggregateValue { .. } | RuntimeClass::RawAddr { .. } => return None,
        };
        let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(ret_class.clone()));
        let expr = match generic_numeric_intrinsic_kind(name.as_str()) {
            Some(GenericNumericIntrinsicKind::Saturating(op)) => {
                let [lhs, rhs] = args.as_slice() else {
                    return None;
                };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::Saturating {
                    op,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar,
                })
            }
            Some(GenericNumericIntrinsicKind::Bitcast) => {
                let [value] = args.as_slice() else {
                    return None;
                };
                RExpr::Cast {
                    value: *value,
                    to: scalar,
                }
            }
            Some(GenericNumericIntrinsicKind::CheckedBinary(op)) => {
                let [lhs, rhs] = args.as_slice() else {
                    return None;
                };
                self.lower_arith_expr_for_mode(bb, op, true, *lhs, *rhs, &scalar)?
            }
            Some(GenericNumericIntrinsicKind::CheckedNeg) => {
                let [value] = args.as_slice() else {
                    return None;
                };
                self.lower_unary_expr_for_mode(bb, UnOp::Minus, true, *value, ret_ty, &scalar)?
            }
            _ => self.lower_numeric_intrinsic_expr(name.as_str(), &args, &scalar)?,
        };
        self.push_stmt(bb, RStmt::Assign { dst: ret, expr });
        Some(ret)
    }

    fn lower_numeric_intrinsic_expr(
        &self,
        name: &str,
        args: &[RLocalId],
        scalar: &ScalarClass<'db>,
    ) -> Option<RExpr<'db>> {
        let (op, _) = intrinsic_numeric_name_parts(name)?;
        Some(match op {
            "add" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Add,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "sub" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Sub,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "mul" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Mul,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "div" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Div,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "rem" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Rem,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "pow" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::IntrinsicArith {
                    op: IntrinsicArithBinOp::Pow,
                    checked: false,
                    lhs: *lhs,
                    rhs: *rhs,
                    class: scalar.clone(),
                })
            }
            "shl" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::LShift),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "shr" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::RShift),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "bitand" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::BitAnd),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "bitor" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::BitOr),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "bitxor" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::BitXor),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "eq" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::Eq),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "ne" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::NotEq),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "lt" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::Lt),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "le" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::LtEq),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "gt" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::Gt),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "ge" => {
                let [lhs, rhs] = args else { return None };
                RExpr::Binary {
                    op: BinOp::Comp(CompBinOp::GtEq),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            "bitnot" => {
                let [value] = args else { return None };
                RExpr::Unary {
                    op: UnOp::BitNot,
                    value: *value,
                }
            }
            "not" => {
                let [value] = args else { return None };
                RExpr::Unary {
                    op: UnOp::Not,
                    value: *value,
                }
            }
            _ => return None,
        })
    }

    fn lower_visible_call_args(
        &mut self,
        bb: RBlockId,
        args: &[NOperand],
        plan: &CompiledCallInputPlan<'db>,
    ) -> (Vec<RLocalId>, Vec<RuntimeClass<'db>>) {
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        let selected = self.with_current_body_cx(|cx| {
            let mut class_cache = InferClassCache::new(self.semantic_body.locals.len());
            RuntimeArgSelector::new(cx.env, cx.carriers, Some(&mut class_cache))
                .with_concrete_roots(&roots)
                .with_concrete_value_classes(&value_classes)
                .selected_param_inputs(args, &plan.param_plans)
        });
        RuntimeArgLowerer::new(self, bb).lower_all(&selected)
    }

    fn lower_runtime_call_inputs(
        &mut self,
        bb: RBlockId,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        plan: &CompiledCallInputPlan<'db>,
    ) -> (Vec<RLocalId>, Vec<RuntimeClass<'db>>) {
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        let selected = self.with_current_body_cx(|cx| {
            let mut class_cache = InferClassCache::new(self.semantic_body.locals.len());
            RuntimeArgSelector::new(cx.env, cx.carriers, Some(&mut class_cache))
                .with_concrete_roots(&roots)
                .with_concrete_value_classes(&value_classes)
                .selected_call_inputs(args, effect_args, plan)
        });
        RuntimeArgLowerer::new(self, bb).lower_all(&selected)
    }

    fn lower_extern_builtin(
        &self,
        semantic: SemanticInstance<'db>,
        args: &[RLocalId],
    ) -> Option<LoweredBuiltinCall<'db>> {
        let BodyOwner::Func(func, _) = semantic.key(self.db).owner(self.db) else {
            return None;
        };
        let kind = runtime_builtin_func_kind(self.db, func)?;
        let word = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let builtin = |builtin, class| LoweredBuiltinCall::Expr { builtin, class };
        Some(match kind {
            RuntimeBuiltinFuncKind::Malloc | RuntimeBuiltinFuncKind::TryMalloc => {
                let [size] = args else { return None };
                let size = *size;
                builtin(
                    if kind == RuntimeBuiltinFuncKind::Malloc {
                        crate::runtime::RuntimeBuiltin::Malloc { size }
                    } else {
                        crate::runtime::RuntimeBuiltin::TryMalloc { size }
                    },
                    self.top_level_class_for_ty(
                        semantic_return_ty(self.db, semantic),
                        AddressSpace::Memory,
                    ),
                )
            }
            RuntimeBuiltinFuncKind::Free => {
                let [ptr] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Free { ptr: *ptr }, None)
            }
            RuntimeBuiltinFuncKind::PtrOffsetBytes => {
                let [ptr, offset] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::PtrOffsetBytes {
                        ptr: *ptr,
                        offset: *offset,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::PtrEq => {
                let [lhs, rhs] = args else { return None };
                LoweredBuiltinCall::Binary {
                    op: BinOp::Comp(CompBinOp::Eq),
                    lhs: *lhs,
                    rhs: *rhs,
                }
            }
            RuntimeBuiltinFuncKind::Mload => {
                let [addr] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Mload { addr: *addr },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Mstore => {
                let [addr, value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Mstore {
                        addr: *addr,
                        value: *value,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Mstore8 => {
                let [addr, value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Mstore8 {
                        addr: *addr,
                        value: *value,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Mcopy => {
                let [dst, src, len] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Mcopy {
                        dst: *dst,
                        src: *src,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::ZeroMem => {
                let [dst, len] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::ZeroMem {
                        dst: *dst,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Msize => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Msize, Some(word.clone()))
            }
            // Hashed-slot provenance only informs borrow checking.
            RuntimeBuiltinFuncKind::Sload | RuntimeBuiltinFuncKind::SloadHashed => {
                let [slot] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Sload { slot: *slot },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Sstore | RuntimeBuiltinFuncKind::SstoreHashed => {
                let [slot, value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Sstore {
                        slot: *slot,
                        value: *value,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::CallDataLoad => {
                let [offset] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::CallDataLoad { offset: *offset },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::CallDataCopy => {
                let [dst, offset, len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::CallDataCopy {
                        dst: *dst,
                        offset: *offset,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::CallDataSize => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::CallDataSize,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::ReturnDataCopy => {
                let [dst, offset, len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::ReturnDataCopy {
                        dst: *dst,
                        offset: *offset,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::ReturnDataSize => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::ReturnDataSize,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::CodeCopy => {
                let [dst, offset, len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::CodeCopy {
                        dst: *dst,
                        offset: *offset,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::CodeSize => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::CodeSize, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::ExtCodeSize => {
                let [addr] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::ExtCodeSize { addr: *addr },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::ExtCodeCopy => {
                let [addr, dst, offset, len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::ExtCodeCopy {
                        addr: *addr,
                        dst: *dst,
                        offset: *offset,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::ExtCodeHash => {
                let [addr] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::ExtCodeHash { addr: *addr },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Keccak256 => {
                let [offset, len] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Keccak256 {
                        offset: *offset,
                        len: *len,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::AddMod => {
                let [lhs, rhs, modulus] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::AddMod {
                        lhs: *lhs,
                        rhs: *rhs,
                        modulus: *modulus,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::MulMod => {
                let [lhs, rhs, modulus] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::MulMod {
                        lhs: *lhs,
                        rhs: *rhs,
                        modulus: *modulus,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::LeadingZeros => {
                let [value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::LeadingZeros { value: *value },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::ToBigEndian | RuntimeBuiltinFuncKind::FromBigEndian => {
                let [value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::SwapBytes { value: *value },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::BlackBox => {
                let [value] = args else { return None };
                let class @ RuntimeClass::Scalar(_) = self.value_class(*value)? else {
                    return None;
                };
                builtin(
                    RuntimeBuiltin::BlackBox { value: *value },
                    Some(class.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Byte => {
                let [pos, value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Byte {
                        pos: *pos,
                        value: *value,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::SignExtend => {
                let [byte, value] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::SignExtend {
                        byte: *byte,
                        value: *value,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Address => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Address, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::Caller => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Caller, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::CallValue => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::CallValue,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Origin => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Origin, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::GasPrice => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::GasPrice, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::CoinBase => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::CoinBase, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::Balance => {
                let [addr] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Balance { addr: *addr },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Timestamp => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Timestamp,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Number => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Number, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::PrevRandao => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::PrevRandao,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::GasLimit => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::GasLimit, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::ChainId => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::ChainId, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::BaseFee => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::BaseFee, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::SelfBalance => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::SelfBalance,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::BlockHash => {
                let [block] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::BlockHash { block: *block },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::BlobHash => {
                let [index] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::BlobHash { index: *index },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::BlobBaseFee => {
                let [] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::BlobBaseFee,
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Gas => {
                let [] = args else { return None };
                builtin(crate::runtime::RuntimeBuiltin::Gas, Some(word.clone()))
            }
            RuntimeBuiltinFuncKind::Call => {
                let [gas, addr, value, args_offset, args_len, ret_offset, ret_len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Call {
                        gas: *gas,
                        addr: *addr,
                        value: *value,
                        args_offset: *args_offset,
                        args_len: *args_len,
                        ret_offset: *ret_offset,
                        ret_len: *ret_len,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::StaticCall | RuntimeBuiltinFuncKind::StaticCallPrecompile => {
                let [gas, addr, args_offset, args_len, ret_offset, ret_len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::StaticCall {
                        gas: *gas,
                        addr: *addr,
                        args_offset: *args_offset,
                        args_len: *args_len,
                        ret_offset: *ret_offset,
                        ret_len: *ret_len,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::DelegateCall => {
                let [gas, addr, args_offset, args_len, ret_offset, ret_len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::DelegateCall {
                        gas: *gas,
                        addr: *addr,
                        args_offset: *args_offset,
                        args_len: *args_len,
                        ret_offset: *ret_offset,
                        ret_len: *ret_len,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Create => {
                let [value, offset, len] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Create {
                        value: *value,
                        offset: *offset,
                        len: *len,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Create2 => {
                let [value, offset, len, salt] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Create2 {
                        value: *value,
                        offset: *offset,
                        len: *len,
                        salt: *salt,
                    },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Log0 => {
                let [offset, len] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Log0 {
                        offset: *offset,
                        len: *len,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Log1 => {
                let [offset, len, topic0] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Log1 {
                        offset: *offset,
                        len: *len,
                        topic0: *topic0,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Log2 => {
                let [offset, len, topic0, topic1] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Log2 {
                        offset: *offset,
                        len: *len,
                        topic0: *topic0,
                        topic1: *topic1,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Log3 => {
                let [offset, len, topic0, topic1, topic2] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Log3 {
                        offset: *offset,
                        len: *len,
                        topic0: *topic0,
                        topic1: *topic1,
                        topic2: *topic2,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Log4 => {
                let [offset, len, topic0, topic1, topic2, topic3] = args else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::Log4 {
                        offset: *offset,
                        len: *len,
                        topic0: *topic0,
                        topic1: *topic1,
                        topic2: *topic2,
                        topic3: *topic3,
                    },
                    None,
                )
            }
            RuntimeBuiltinFuncKind::Revert => {
                let [offset, len] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::Revert {
                    offset: *offset,
                    len: *len,
                })
            }
            RuntimeBuiltinFuncKind::RevertEmpty => {
                let [] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::RevertEmpty)
            }
            RuntimeBuiltinFuncKind::ReturnData => {
                let [offset, len] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::ReturnData {
                    offset: *offset,
                    len: *len,
                })
            }
            RuntimeBuiltinFuncKind::SelfDestruct => {
                let [beneficiary] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::SelfDestruct {
                    beneficiary: *beneficiary,
                })
            }
            RuntimeBuiltinFuncKind::Stop => {
                let [] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::Stop)
            }
            RuntimeBuiltinFuncKind::Panic | RuntimeBuiltinFuncKind::Todo => {
                let [] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::Trap)
            }
            RuntimeBuiltinFuncKind::PanicWithValue => {
                let [_value] = args else { return None };
                LoweredBuiltinCall::Terminator(RTerminator::Trap)
            }
            // Lowered with its payload in `lower_panic_code`, by
            // `lower_clear`, as nothing, and as a constant.
            RuntimeBuiltinFuncKind::PanicCode
            | RuntimeBuiltinFuncKind::Clear
            | RuntimeBuiltinFuncKind::Forget
            | RuntimeBuiltinFuncKind::NeedsDrop => return None,
            RuntimeBuiltinFuncKind::IntrinsicKeccak256
            | RuntimeBuiltinFuncKind::IntrinsicKeccakWords => return None,
        })
    }

    /// Hashes the words of `__keccak_words`'s array as values: each element
    /// is read into a scalar, so the digest depends on no memory.
    fn lower_intrinsic_keccak_words_call(
        &mut self,
        bb: RBlockId,
        func: Func<'db>,
        args: &[NOperand],
    ) -> Option<RLocalId> {
        if runtime_builtin_func_kind(self.db, func)
            != Some(RuntimeBuiltinFuncKind::IntrinsicKeccakWords)
        {
            return None;
        }
        let [words] = args else {
            return None;
        };
        let words_ty = self.semantic_body.normalized.value(words.value)?.ty;
        let layout = self.layout_for_ty(words_ty);
        let crate::runtime::Layout::Array(array_layout) = layout.data(self.db) else {
            panic!(
                "__keccak_words expects a word-array argument, found {}",
                words_ty.pretty_print(self.db)
            );
        };
        let len = u32::try_from(array_layout.len).expect("too many hashed words");
        let word_ty = TyId::u256(self.db);
        // An empty array is erased; there is nothing to read.
        let words = if len == 0 {
            Box::default()
        } else {
            let array = self.read_semantic_operand(bb, *words);
            let array = self.coerce_value(bb, array, &RuntimeClass::AggregateValue { layout });
            (0..len)
                .map(|index| {
                    let word =
                        self.alloc_runtime_temp(word_ty, RuntimeCarrier::Value(word_class()));
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst: word,
                            expr: RExpr::AggregateExtract {
                                value: array,
                                index,
                            },
                        },
                    );
                    word
                })
                .collect()
        };
        let ret = self.alloc_runtime_temp(word_ty, RuntimeCarrier::Value(word_class()));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: ret,
                expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::KeccakWords { words }),
            },
        );
        Some(ret)
    }

    fn lower_intrinsic_keccak256_call(
        &mut self,
        bb: RBlockId,
        semantic: SemanticInstance<'db>,
        func: Func<'db>,
        args: &[NOperand],
    ) -> Option<RLocalId> {
        if runtime_builtin_func_kind(self.db, func)
            != Some(RuntimeBuiltinFuncKind::IntrinsicKeccak256)
        {
            return None;
        }
        let [bytes] = args else { return None };
        let bytes_ty = self.semantic_body.normalized.value(bytes.value)?.ty;
        let width = *semantic
            .key(self.db)
            .subst(self.db)
            .generic_args(self.db)
            .first()?;
        let width = runtime_interface_ty_in_env(
            self.db,
            RuntimeTypeEnv::for_semantic(self.db, semantic),
            width,
        );
        let len = concrete_usize(self.db, width).unwrap_or_else(|| {
            panic!(
                "__keccak256 requires a concrete N: {}",
                width.pretty_print(self.db)
            )
        });
        let layout = self.layout_for_ty(bytes_ty);
        let size = RuntimeMemoryLayout::raw(self.db)
            .layout_size(layout)
            .expect("packed hash input has an admitted raw-memory layout");
        let size = usize::try_from(size).expect("raw-memory layout size fits usize");
        assert!(size >= len);
        let size = self.alloc_u256_const(bb, size);
        let offset = self.malloc_bytes(bb, size);
        let class = RuntimeClass::AggregateValue { layout };
        if !self.class_is_runtime_zst(&class) {
            let raw_ptr = self.coerce_value(
                bb,
                offset,
                &RuntimeClass::raw_addr(self.db, AddressSpace::Memory, class.clone()),
            );
            let value = self.read_semantic_operand(bb, *bytes);
            // Export numeric words into fresh raw memory. Memory providers hold
            // objects, whose addresses and element layouts are not raw pointers.
            self.write_value_to_place(
                bb,
                RuntimePlace {
                    root: PlaceRoot::Ptr {
                        addr: raw_ptr,
                        space: AddressSpace::Memory,
                        class: class.clone(),
                    },
                    path: Box::default(),
                },
                value,
                &class,
            );
        }
        let len = self.alloc_u256_const(bb, len);
        let ret = self.alloc_runtime_temp(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: ret,
                expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::Keccak256 { offset, len }),
            },
        );
        Some(ret)
    }

    fn lower_terminator(
        &mut self,
        bb: RBlockId,
        terminator: &NTerminator<'db>,
    ) -> RTerminator<'db> {
        match &terminator.kind {
            NTerminatorKind::Goto(target) => RTerminator::Goto(self.lower_successor(target)),
            NTerminatorKind::Branch {
                cond,
                then_target,
                else_target,
            } => RTerminator::Branch {
                cond: self.read_semantic_operand(bb, *cond),
                then_bb: self.lower_successor(then_target),
                else_bb: self.lower_successor(else_target),
            },
            NTerminatorKind::MatchEnum {
                value,
                enum_ty,
                cases,
                default,
            } => {
                let value = self.runtime_operand(*value);
                let enum_layout = self.enum_layout_for_local(value.local);
                let tag_class = RuntimeClass::Scalar(ScalarClass {
                    repr: match enum_layout.data(self.db) {
                        crate::runtime::Layout::Enum(layout) => layout.tag.repr,
                        _ => unreachable!(),
                    },
                    role: ScalarRole::EnumTag { enum_layout },
                });
                let tag = self.alloc_runtime_temp(*enum_ty, RuntimeCarrier::Value(tag_class));
                self.lower_enum_tag(bb, tag, value);
                RTerminator::MatchEnumTag {
                    tag,
                    enum_layout,
                    cases: cases
                        .iter()
                        .map(|(variant, target)| {
                            (
                                VariantId {
                                    enum_layout,
                                    index: variant.0,
                                },
                                self.lower_successor(target),
                            )
                        })
                        .collect(),
                    default: default.as_ref().map(|target| self.lower_successor(target)),
                }
            }
            NTerminatorKind::Assert { message } => self.lower_assert_terminator(bb, *message),
            // A projection's slide returns nothing: its caller inlines it.
            NTerminatorKind::Return(None)
                if self
                    .key
                    .semantic(self.db)
                    .is_some_and(|semantic| semantic.is_projection(self.db)) =>
            {
                RTerminator::Return(None)
            }
            NTerminatorKind::Return(value) => self.lower_return(bb, *value),
            NTerminatorKind::Yield { value, resume } => {
                let RTerminator::Return(value) = self.lower_return(bb, Some(*value)) else {
                    unreachable!("a yielded grant lowers to a returned value")
                };
                RTerminator::Yield {
                    value,
                    resume: self.lower_successor(resume),
                }
            }
        }
    }

    fn lower_return(&mut self, bb: RBlockId, value: Option<NOperand>) -> RTerminator<'db> {
        let ret = self.signature.ret.clone();
        RTerminator::Return(ret.and_then(|class| {
            value.map(|value| {
                self.lower_built_for_class(bb, value, &class)
                    .unwrap_or_else(|| self.lower_semantic_operand_for_class(bb, value, &class))
            })
        }))
    }

    /// `operand`, built here as an enum variant or a struct or tuple, built
    /// again as `class`. An enum value has no payload in its other variants,
    /// which `class` may give other fields: the join of the values a body
    /// returns takes each variant's fields from the values built as it,
    /// wherever the enum lies in the value.
    fn lower_built_for_class(
        &mut self,
        bb: RBlockId,
        operand: NOperand,
        class: &RuntimeClass<'db>,
    ) -> Option<RLocalId> {
        let RuntimeClass::AggregateValue { layout } = class else {
            return None;
        };
        let (_, expr) = self.semantic_body.normalized.defining_expr(operand.value)?;
        let (variant, field_tys, fields, classes) = match (expr, layout.data(self.db)) {
            (
                NExpr::EnumMake {
                    enum_ty,
                    variant,
                    fields,
                },
                Layout::Enum(data),
            ) if data.variants[variant.0 as usize].fields.len() == fields.len() => (
                Some(*variant),
                self.enum_variant_field_tys(*enum_ty, *variant, fields.len()),
                fields.clone(),
                data.variants[variant.0 as usize].fields.clone(),
            ),
            (NExpr::AggregateMake { ty, fields }, Layout::Struct(data))
                if !ty.is_array(self.db) && data.fields.len() == fields.len() =>
            {
                (
                    None,
                    self.aggregate_field_tys(*ty, fields.len()),
                    fields.clone(),
                    data.fields.clone(),
                )
            }
            _ => return None,
        };
        let value = self.read_semantic_operand(bb, operand);
        if self.value_class(value) == Some(class) {
            return Some(value);
        }
        let values = fields
            .iter()
            .zip(field_tys)
            .zip(classes.iter())
            .map(|((&field, field_ty), class)| {
                let built = if self.class_is_runtime_zst(class) {
                    None
                } else {
                    self.lower_built_for_class(bb, field, class)
                };
                built.unwrap_or_else(|| self.lower_ctor_field(bb, field, field_ty).0)
            })
            .collect::<Vec<_>>();
        let dst = self.alloc_runtime_temp(
            self.locals[value.index()].semantic_ty,
            RuntimeCarrier::Value(class.clone()),
        );
        match variant {
            Some(variant) => self.lower_enum_values(bb, dst, *layout, variant, &values),
            None => {
                let elems = aggregate_ctor_elems_for_layout(self.db, *layout, values.len());
                self.lower_aggregate_values(bb, dst, *layout, &elems, &values);
            }
        }
        Some(dst)
    }

    fn lower_successor(&mut self, successor: &NSuccessor) -> RBlockId {
        let target = self.runtime_block(successor.block);
        let params = self.semantic_body.normalized.blocks[successor.block.index()]
            .params
            .iter()
            .copied()
            .zip(successor.args.iter().copied())
            .filter_map(|(param, arg)| {
                let dst = self.semantic_body.value_local(param)?;
                (self.semantic_body.operand_local(arg) != Some(dst)).then_some((dst, arg))
            })
            .collect::<Vec<_>>();
        if params.is_empty() {
            return target;
        }
        let edge = self.new_block();
        self.blocks[edge.index()].terminator = RTerminator::Goto(target);
        let values = params
            .iter()
            .map(|(_, arg)| {
                let value = self.read_semantic_operand(edge, *arg);
                let class = self.value_class(value).cloned()?;
                let temp = self.alloc_runtime_temp(
                    self.semantic_operand_ty(*arg),
                    RuntimeCarrier::Value(class),
                );
                self.push_stmt(
                    edge,
                    RStmt::Assign {
                        dst: temp,
                        expr: RExpr::Use(value),
                    },
                );
                Some(temp)
            })
            .collect::<Vec<_>>();
        for ((dst, _), value) in params.into_iter().zip(values) {
            if let Some(value) = value {
                self.write_semantic_value(edge, dst, value);
            }
        }
        edge
    }

    fn write_value_to_place(
        &mut self,
        bb: RBlockId,
        dst: RuntimePlace<'db>,
        src: RLocalId,
        target: &RuntimeClass<'db>,
    ) {
        if self.class_is_runtime_zst(target) {
            return;
        }
        match target {
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                let src = self.coerce_value(bb, src, target);
                self.push_stmt(bb, RStmt::Store { dst, src });
            }
            RuntimeClass::AggregateValue { .. } => {
                let src = self.coerce_value(bb, src, target);
                self.push_stmt(bb, RStmt::CopyInto { dst, src });
            }
        }
    }

    fn coerce_value(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
    ) -> RLocalId {
        let semantic_ty = self.locals[src.index()].semantic_ty;
        let source = self
            .value_class(src)
            .cloned()
            .unwrap_or_else(|| panic!("cannot coerce erased value {src:?} to {target:?}"));
        self.emit_plain_runtime_coercion(bb, src, source, target, semantic_ty)
    }

    fn emit_plain_runtime_coercion(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        source: RuntimeClass<'db>,
        target: &RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        let db = self.db;
        emit_runtime_coercion(self, db, bb, src, source, target, semantic_ty)
            .unwrap_or_else(|err| self.panic_unsupported_conversion(src, err))
    }

    fn panic_unsupported_conversion(&self, src: RLocalId, err: RuntimeConversionError<'db>) -> ! {
        let (kind, source, target) = match err {
            RuntimeConversionError::Unsupported { source, target } => {
                ("unsupported", source, target)
            }
            RuntimeConversionError::Cycle { source, target } => ("cyclic", source, target),
        };
        let layout_kind = |layout: LayoutId<'db>| match layout.data(self.db) {
            crate::runtime::Layout::Struct(data) => format!("struct/{}", data.fields.len()),
            crate::runtime::Layout::Array(data) => format!("array/{}", data.len),
            crate::runtime::Layout::Enum(data) => format!("enum/{}", data.variants.len()),
        };
        let class_layout = |class: &RuntimeClass<'db>| {
            class
                .aggregate_layout()
                .map(|layout| (layout, layout.data(self.db), layout_kind(layout)))
        };
        let source_layout = class_layout(&source);
        let target_layout = class_layout(&target);
        let owner = self
            .key
            .semantic(self.db)
            .map(|semantic| semantic.key(self.db).owner(self.db));
        let owner_name = owner.and_then(|owner| match owner {
            BodyOwner::Func(func, _) => func
                .name(self.db)
                .to_opt()
                .map(|name| name.data(self.db).to_string()),
            _ => None,
        });
        panic!(
            "{kind} runtime class coercion in {:?} owner={owner_name:?} {owner:?} from {source:?} to {target:?}; source_layout={source_layout:?}; target_layout={target_layout:?}; src={src:?}; src_ty={}; locals={:?}",
            self.key.source(self.db),
            self.locals[src.index()].semantic_ty.pretty_print(self.db),
            self.locals,
        )
    }

    fn semantic_local_lowering(&self, local: SLocalId) -> &RuntimeLocalLowering<'db> {
        self.semantic_locals.get(local.index()).unwrap_or_else(|| {
            panic!(
                "missing semantic local lowering for {local:?}; semantic_locals={:?}",
                self.semantic_locals,
            )
        })
    }

    fn semantic_local_is_direct(&self, local: SLocalId) -> bool {
        matches!(
            self.semantic_local_lowering(local),
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::PlaceCarrier { .. }
        )
    }

    fn semantic_local_is_derived_place_bound_alias(&self, local: SLocalId) -> bool {
        self.semantic_body
            .locals
            .get(local.index())
            .is_some_and(|local| {
                matches!(
                    local.role,
                    SemanticLocalRole::PlaceBoundValue {
                        provenance: hir::analysis::semantic::PlaceProvenance::Derived(_),
                        ..
                    }
                )
            })
    }

    fn runtime_value_is_unused(&self, value: NValueId) -> bool {
        // A boundary can consume the defining place directly. Preserve the SSA
        // home, but omit a snapshot or view address that no runtime consumer uses.
        let local = self
            .semantic_body
            .value_local(value)
            .expect("normalized value home");
        if self.semantic_body.normalized.roots.iter().any(|root| {
            matches!(root.kind, NRootKind::CapabilityRepresentation { carrier } if carrier == value)
        }) {
            return false;
        }
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        !self.semantic_body.normalized.value_is_used_with(value, |expr| {
            let mut used = false;
            expr.for_each_value_operand(|operand| used |= operand.value == value);
            if !used {
                return false;
            }
            let NExpr::Call { callee, args, effect_args, .. } = expr else {
                return true;
            };
            let semantic = SemanticInstance::new(self.db, callee.key);
            let mut sites = BoundarySiteAllocator::default();
            let plan = compile_call_input_plan_for_semantic(
                self.db, &self.semantic_body, semantic, self.env, effect_args, &mut sites,
            );
            self.with_current_body_cx(|cx| {
                RuntimeArgSelector::new(cx.env, cx.carriers, None)
                    .with_concrete_roots(&roots)
                    .with_concrete_value_classes(&value_classes)
                    .selected_call_inputs(args, effect_args, &plan)
                    .iter()
                    .any(|selected| match &selected.source {
                        RuntimeArgSource::SemanticOperand(operand)
                        | RuntimeArgSource::AggregateFromRuntimeSource(operand) => {
                            operand.value == Some(value) || operand.local == local
                        }
                        RuntimeArgSource::DirectValueMaterialization { local: source, .. }
                        | RuntimeArgSource::RuntimeValue(source)
                        | RuntimeArgSource::TransportValue(source)
                        | RuntimeArgSource::SemanticPlaceAddress(source, _) => *source == local,
                        RuntimeArgSource::PlaceAddress(place, _)
                        | RuntimeArgSource::PlaceValue(place, _)
                        | RuntimeArgSource::ValueExtract { place, .. } => {
                            matches!(place.base, NPlaceBase::CapabilityTarget { carrier } | NPlaceBase::Deref { pointer: carrier } if carrier == value)
                                || matches!(place.base, NPlaceBase::Root(root) if self.semantic_body.root_local(root) == Some(local))
                                || place.path.iter().any(|projection| matches!(projection,
                                    NDataProjection::Index(NIndex::Value(index)) if *index == value))
                        }
                        RuntimeArgSource::Placeholder(_) => false,
                    })
            })
        })
    }

    fn root_provider_value_load_is_lazy(&self, local: SLocalId, expr: &NExpr<'db>) -> bool {
        let SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::RootProvider(provider),
            ..
        } = &self.semantic_body.locals[local.index()].role
        else {
            return false;
        };
        let NExpr::Load { place, .. } = expr else {
            return false;
        };
        if !place.path.is_empty() {
            return false;
        }
        let NPlaceBase::Root(root) = place.base else {
            return false;
        };
        matches!(
            self.semantic_body.normalized.root(root).map(|root| &root.kind),
            Some(NRootKind::Provider { binding }) if binding == provider
        )
    }

    fn lower_derived_place_bound_alias_assign(&self, local: SLocalId, expr: &NExpr<'db>) {
        if self.derived_place_bound_alias_expr_matches_place(local, expr) {
            return;
        }
        panic!(
            "derived place-bound local assigned from non-alias expression: owner={:?}; local={local:?}; local_data={:?}; expr={expr:?}",
            self.current_semantic_key().owner(self.db),
            self.semantic_body.local(local),
        );
    }

    fn derived_place_bound_alias_expr_matches_place(
        &self,
        local: SLocalId,
        expr: &NExpr<'db>,
    ) -> bool {
        let Some(dst_place) = self.alias_source_place_for_local(local) else {
            return false;
        };
        match expr {
            NExpr::Load { place, .. }
            | NExpr::Borrow { place, .. }
            | NExpr::MakeView { place, .. } => place == &dst_place,
            NExpr::Forward { src } => {
                self.semantic_body
                    .operand_local(*src)
                    .and_then(|local| self.alias_source_place_for_local(local))
                    == Some(dst_place)
            }
            NExpr::ProjectValue { value, path } => {
                let Some(source) = self.semantic_body.operand_local(*value) else {
                    return false;
                };
                let Some(mut place) = self.alias_source_place_for_local(source) else {
                    return false;
                };
                place.path = place.path.concat(&path.0);
                place.ty = dst_place.ty;
                place == dst_place
            }
            NExpr::StructuralRepack { .. }
            | NExpr::CodeRegionRef { .. }
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::PointerCast { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::ArrayRepeat { .. }
            | NExpr::AggregateMake { .. }
            | NExpr::EnumMake { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::Call { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => false,
        }
    }

    fn alias_source_place_for_local(&self, local: SLocalId) -> Option<NPlace<'db>> {
        source_alias_source_place_for_local(self.db, &self.semantic_body, local)
    }

    fn semantic_value_class(&self, local: SLocalId) -> Option<RuntimeClass<'db>> {
        match self.semantic_local_lowering(local) {
            RuntimeLocalLowering::Erased => None,
            RuntimeLocalLowering::DirectValue => self.local_class(local).cloned(),
            RuntimeLocalLowering::PlaceCarrier { place_class }
            | RuntimeLocalLowering::PlaceBoundValue { place_class, .. } => {
                Some(place_class.clone())
            }
        }
    }

    fn provider_binding(&self, id: RuntimeProviderBindingId) -> &RuntimeProviderBinding<'db> {
        self.provider_bindings
            .get(id.index())
            .unwrap_or_else(|| panic!("missing runtime provider binding for {id:?}"))
    }

    fn provider_binding_value(&self, id: RuntimeProviderBindingId) -> RLocalId {
        self.provider_binding(id).value
    }

    fn provider_binding_id_for_semantic(
        &self,
        provider: &hir::semantic::ProviderBinding<'db>,
    ) -> Option<RuntimeProviderBindingId> {
        self.provider_bindings
            .iter()
            .enumerate()
            .find_map(|(idx, binding)| {
                (binding.provider == *provider)
                    .then(|| RuntimeProviderBindingId::from_u32(idx as u32))
            })
    }

    fn provider_place_root(
        &self,
        provider: &hir::semantic::ProviderBinding<'db>,
    ) -> Option<PlaceRoot<'db>> {
        self.provider_binding_id_for_semantic(provider)
            .map(PlaceRoot::Provider)
    }

    fn semantic_place_root(&self, local: SLocalId) -> Option<PlaceRoot<'db>> {
        if let RuntimeLocalLowering::PlaceBoundValue {
            provider: Some(provider),
            ..
        } = self.semantic_local_lowering(local)
        {
            return Some(PlaceRoot::Provider(*provider));
        }
        self.local_root(local)
    }

    fn try_semantic_place(&mut self, bb: RBlockId, local: SLocalId) -> Option<RuntimePlace<'db>> {
        if let Some(place) =
            nonself_alias_source_place_for_local(self.db, &self.semantic_body, local)
            && let Some(place) = self.try_lower_place(bb, &place)
        {
            return Some(place);
        }
        if matches!(
            self.semantic_local_lowering(local),
            RuntimeLocalLowering::PlaceCarrier { .. }
        ) && let Some(place) = self.runtime_place_from_addr_value(self.runtime_value(local))
        {
            return Some(place);
        }
        let root = self.semantic_place_root(local)?;
        Some(RuntimePlace {
            root,
            path: Box::default(),
        })
    }

    fn semantic_place(&mut self, bb: RBlockId, local: SLocalId) -> RuntimePlace<'db> {
        self.try_semantic_place(bb, local).unwrap_or_else(|| {
            panic!(
                "cannot lower erased local as a runtime place root: source={:?}; owner={:?}; local={local:?}; ty={}; source_binding={:?}; lowering={:?}; root={:?}; carrier={:?}",
                self.key.source(self.db),
                self.key
                    .semantic(self.db)
                    .map(|semantic| semantic.key(self.db).owner(self.db)),
                self.locals[local.index()].semantic_ty.pretty_print(self.db),
                self.semantic_body.locals[local.index()].source,
                self.semantic_local_lowering(local),
                self.locals[self.runtime_value(local).index()].root,
                self.locals[self.runtime_value(local).index()].carrier,
            )
        })
    }

    fn try_lower_place(&mut self, bb: RBlockId, place: &NPlace<'db>) -> Option<RuntimePlace<'db>> {
        let mut runtime_place = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => {
                let local = self.semantic_body.value_local(carrier)?;
                let value = self.normalized_value_temps[carrier.index()]
                    .unwrap_or_else(|| self.runtime_value(local));
                self.runtime_place_from_addr_value(value)
                    .or_else(|| self.try_semantic_place(bb, local))?
            }
            // A pointer is an address word; deref is its conversion to the
            // pointee's transport.
            NPlaceBase::Deref { pointer } => {
                let transport = deref_transport_class_in_env(
                    self.db,
                    self.env,
                    self.semantic_body.normalized.value(pointer)?.ty,
                )?;
                let word = self.read_normalized_value(bb, pointer);
                let addr = self.coerce_value(bb, word, &transport);
                self.runtime_place_from_addr_value(addr)?
            }
            NPlaceBase::Root(root) => match &self.semantic_body.normalized.root(root)?.kind {
                NRootKind::LocalSlot { .. }
                | NRootKind::Temporary { .. }
                | NRootKind::ParamPlace { .. } => {
                    let root = self.semantic_place_root(self.semantic_body.root_local(root)?)?;
                    RuntimePlace {
                        root,
                        path: Box::default(),
                    }
                }
                NRootKind::Provider { binding } => RuntimePlace {
                    root: self.provider_place_root(binding)?,
                    path: Box::default(),
                },
                NRootKind::CapabilityRepresentation { carrier } => {
                    self.try_semantic_place(bb, self.semantic_body.value_local(*carrier)?)?
                }
            },
        };
        let root_ty = self
            .semantic_body
            .normalized
            .place_prefix_ty(self.db, place, 0)?;
        if let Some(pinned) = self.pinned_place(bb, &runtime_place, root_ty) {
            runtime_place = pinned;
        }
        let mut current = self.project_place_class(&runtime_place);
        let mut projected = runtime_place.path.into_vec();
        for (idx, elem) in place.path.iter().enumerate() {
            match elem {
                NDataProjection::Field(field) => {
                    let field = *field;
                    projected.push(PlaceElem::Field(field));
                    current = project_field_class(self.db, current, field);
                }
                NDataProjection::VariantField { variant, field } => {
                    let field = *field;
                    let variant = VariantId {
                        enum_layout: current
                            .aggregate_layout()
                            .expect("variant-field places should project from enum layouts"),
                        index: variant.0,
                    };
                    projected.push(PlaceElem::VariantField { variant, field });
                    current = project_variant_field_class(self.db, current, variant, field);
                }
                NDataProjection::Index(NIndex::Value(index)) => {
                    projected.push(PlaceElem::Index(IndexSource::Dynamic(
                        self.read_normalized_value(bb, *index),
                    )));
                    current = project_index_class(self.db, current);
                }
                NDataProjection::Index(NIndex::Const(index)) => {
                    projected.push(PlaceElem::Index(IndexSource::Constant(*index)));
                    current = project_index_class(self.db, current);
                }
                NDataProjection::Entry(key) => {
                    runtime_place.path = std::mem::take(&mut projected).into_boxed_slice();
                    let normalized = &self.semantic_body.normalized;
                    let collection_ty = normalized.place_prefix_ty(self.db, place, idx)?;
                    let element_ty = normalized.place_prefix_ty(self.db, place, idx + 1)?;
                    runtime_place = match self.lower_entry(
                        bb,
                        runtime_place,
                        collection_ty,
                        element_ty,
                        *key,
                    ) {
                        EntryPlace::Place(place) => place,
                        EntryPlace::Lane(lane) => RuntimePlace {
                            root: PlaceRoot::Slot(self.hold_lane(bb, LaneSite::Entry(lane))),
                            path: Box::default(),
                        },
                    };
                    current = self.project_place_class(&runtime_place);
                }
            }
            if let Some(ty) = self
                .semantic_body
                .normalized
                .place_prefix_ty(self.db, place, idx + 1)
                && self
                    .semantic_body
                    .owner()
                    .pinned_space(self.db, ty)
                    .is_some()
            {
                runtime_place.path = std::mem::take(&mut projected).into_boxed_slice();
                if let Some(pinned) = self.pinned_place(bb, &runtime_place, ty) {
                    runtime_place = pinned;
                    current = self.project_place_class(&runtime_place);
                }
                projected = std::mem::take(&mut runtime_place.path).into_vec();
            }
            if idx + 1 < place.path.len()
                && let Some(target) = current.deref_target(self.db)
            {
                projected.push(PlaceElem::Deref);
                current = target;
            }
        }
        runtime_place.path = projected.into_boxed_slice();
        Some(runtime_place)
    }

    /// `place`, of type `ty`, rooted in the space `ty` is pinned to when it
    /// lies in another: a place of a pinned type lies in the pin's space at
    /// the same number, so a `TSlot` field of a storage struct is transient.
    /// `None` when `ty` is unpinned or the place already lies there.
    fn pinned_place(
        &mut self,
        bb: RBlockId,
        place: &RuntimePlace<'db>,
        ty: TyId<'db>,
    ) -> Option<RuntimePlace<'db>> {
        let space = self.semantic_body.owner().pinned_space(self.db, ty)?;
        let addr_class = self.place_addr_class(place);
        let from = addr_class.address_space();
        if from == Some(space) {
            return None;
        }
        // Admission rejects a pinned type outside state, so only a place in
        // the other state space moves, as a `TSlot` in a storage struct.
        assert!(
            !matches!(
                from,
                Some(AddressSpace::Memory | AddressSpace::Calldata | AddressSpace::Code)
            ),
            "a place of `{}`, pinned to {space:?}, lies in {from:?}",
            ty.pretty_print(self.db)
        );
        let addr = self.lower_place_addr_of_for_class(ty, bb, place.clone(), addr_class);
        let slot = self.coerce_value(bb, addr, &word_class());
        let class = provider_class_for_target_in_env(self.db, self.env, Some(ty), space);
        Some(RuntimePlace {
            root: PlaceRoot::Ref(self.coerce_value(bb, slot, &class)),
            path: Box::default(),
        })
    }

    /// The element at `key` of the collection at `collection`, of type
    /// `collection_ty`: at the slot and bit offset the collection's
    /// `StateIndex::locate` computes from the collection's own slot, in the
    /// space the collection keeps its elements in. An element the collection
    /// packs into a lane is that lane; any other is a reference at its slot.
    fn lower_entry(
        &mut self,
        bb: RBlockId,
        collection: RuntimePlace<'db>,
        collection_ty: TyId<'db>,
        element_ty: TyId<'db>,
        key: NValueId,
    ) -> EntryPlace<'db> {
        let owner = self.semantic_body.owner();
        let space = owner
            .pinned_space(self.db, collection_ty)
            .expect("a collection whose elements are places is pinned to their space");
        let lanes = owner.state_index_lanes(self.db, collection_ty);
        let word_ty = TyId::u256(self.db);
        let word = word_class();
        let receiver_class = self.place_addr_class(&collection);
        assert!(
            matches!(
                receiver_class.address_space(),
                Some(AddressSpace::Storage | AddressSpace::Transient)
            ),
            "the collection `{}` lies outside state",
            collection_ty.pretty_print(self.db)
        );
        let receiver =
            self.lower_place_addr_of_for_class(collection_ty, bb, collection, receiver_class);
        let base = self.coerce_value(bb, receiver, &word);
        let key = self.read_normalized_value(bb, key);
        let locate = self.resolve_core_method(
            &["ops", "StateIndex"],
            "locate",
            vec![collection_ty],
            collection_ty,
        );
        let location = self.call_generated(bb, locate, &[receiver, base, key]);
        let mut coordinate = |index| {
            let value = self.alloc_runtime_temp(word_ty, RuntimeCarrier::Value(word.clone()));
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: value,
                    expr: RExpr::AggregateExtract {
                        value: location,
                        index,
                    },
                },
            );
            value
        };
        let slot = coordinate(0);
        if let Some(lanes) = lanes {
            return EntryPlace::Lane(Lane {
                slot,
                offset: coordinate(1),
                space,
                codec: lanes.codec,
                ty: element_ty,
            });
        }
        let element_class =
            provider_class_for_target_in_env(self.db, self.env, Some(element_ty), space);
        let element = self.coerce_value(bb, slot, &element_class);
        EntryPlace::Place(RuntimePlace {
            root: PlaceRoot::Ref(element),
            path: Box::default(),
        })
    }

    /// The lane `place` names when it ends at an element its collection
    /// packs into a lane.
    fn entry_lane(&mut self, bb: RBlockId, place: &NPlace<'db>) -> Option<Lane<'db>> {
        let (NDataProjection::Entry(key), prefix) = place.path.as_slice().split_last()? else {
            return None;
        };
        let collection_ty =
            self.semantic_body
                .normalized
                .place_prefix_ty(self.db, place, prefix.len())?;
        self.semantic_body
            .owner()
            .state_index_lanes(self.db, collection_ty)?;
        let collection = self.try_lower_place(
            bb,
            &NPlace {
                path: NDataPath::new(prefix),
                ty: collection_ty,
                ..place.clone()
            },
        )?;
        match self.lower_entry(bb, collection, collection_ty, place.ty, *key) {
            EntryPlace::Lane(lane) => Some(lane),
            EntryPlace::Place(_) => unreachable!("the collection packs its elements"),
        }
    }

    /// `place` resolved for a construct that consumes it: the lane it ends
    /// at, or the place, in which a lane on its path is a memory copy.
    fn resolve_place(&mut self, bb: RBlockId, place: &NPlace<'db>) -> ResolvedPlace<'db> {
        if let Some(lane) = self.entry_lane(bb, place) {
            return ResolvedPlace::Lane(LaneSite::Entry(lane));
        }
        let ty = place.ty;
        let place = self.lower_place(bb, place);
        let program = self.db as &dyn MirDb;
        if runtime_place_lane(self.db, &program, self, &place)
            .unwrap_or_else(|err| panic!("invalid runtime place: {err:?}; {place:?}"))
            .is_some()
        {
            ResolvedPlace::Lane(LaneSite::Field { place, ty })
        } else {
            ResolvedPlace::Place(place)
        }
    }

    /// The word holding the element `lane`.
    fn lane_word(&mut self, bb: RBlockId, lane: &Lane<'db>) -> RuntimePlace<'db> {
        let class = provider_class_for_target_in_env(
            self.db,
            self.env,
            Some(TyId::u256(self.db)),
            lane.space,
        );
        RuntimePlace {
            root: PlaceRoot::Ref(self.coerce_value(bb, lane.slot, &class)),
            path: Box::default(),
        }
    }

    /// The class of the value in `lane`.
    fn lane_class(&self, lane: &LaneSite<'db>) -> RuntimeClass<'db> {
        match lane {
            LaneSite::Entry(lane) => stored_class_for_ty_in_env(self.db, self.env, lane.ty),
            LaneSite::Field { place, .. } => self.project_place_class(place),
        }
    }

    /// The value in `lane`, extracted from its word.
    fn read_lane(&mut self, bb: RBlockId, lane: &LaneSite<'db>) -> RLocalId {
        match lane {
            LaneSite::Entry(lane) => {
                let place = self.lane_word(bb, lane);
                let word = self.load_runtime_place_value(bb, place, TyId::u256(self.db));
                let get = self.resolve_core_method(
                    &["ops", "LaneCodec"],
                    "get",
                    vec![lane.codec, lane.ty],
                    lane.codec,
                );
                self.call_generated(bb, get, &[word, lane.offset])
            }
            LaneSite::Field { place, ty } => self.load_runtime_place_value(bb, place.clone(), *ty),
        }
    }

    /// Stores `value` into `lane`, keeping the rest of its word.
    fn write_lane(&mut self, bb: RBlockId, lane: &LaneSite<'db>, value: RLocalId) {
        match lane {
            LaneSite::Entry(lane) => {
                let place = self.lane_word(bb, lane);
                let word = self.load_runtime_place_value(bb, place.clone(), TyId::u256(self.db));
                let set = self.resolve_core_method(
                    &["ops", "LaneCodec"],
                    "set",
                    vec![lane.codec, lane.ty],
                    lane.codec,
                );
                let word = self.call_generated(bb, set, &[word, lane.offset, value]);
                self.write_value_to_place(bb, place, word, &word_class());
            }
            LaneSite::Field { place, .. } => {
                let class = self.project_place_class(place);
                self.write_value_to_place(bb, place.clone(), value, &class);
            }
        }
    }

    /// A memory copy of the value in `lane`, which `store_lane_copy` stores
    /// back.
    fn hold_lane(&mut self, bb: RBlockId, lane: LaneSite<'db>) -> RLocalId {
        let class = self.lane_class(&lane);
        let ty = match &lane {
            LaneSite::Entry(lane) => lane.ty,
            LaneSite::Field { ty, .. } => *ty,
        };
        let value = self.read_lane(bb, &lane);
        let value = self.coerce_value(bb, value, &class);
        let copy = self.alloc_value_slot(ty, class);
        self.push_value_use(bb, copy, value);
        self.lane_copies.insert(copy, lane);
        copy
    }

    /// Holds the lane copy `copy` until `extent` ends.
    fn hold_until(&mut self, extent: LaneExtent, copy: RLocalId) {
        self.lane_extents.entry(extent).or_default().push(copy);
    }

    /// Stores back the lane copies held for `extent`, which ends here; an
    /// extent can end on several paths. Whether it held any.
    fn end_lane_extent(&mut self, bb: RBlockId, extent: LaneExtent) -> bool {
        let Some(copies) = self.lane_extents.get(&extent).cloned() else {
            return false;
        };
        for copy in copies {
            self.store_lane_copy(bb, copy);
        }
        true
    }

    /// Whether `expr`, defining `value`, accesses a lane that nothing reads
    /// through the access: it needs no copy.
    fn is_dead_lane_access(&self, value: NValueId, expr: &NExpr<'db>) -> bool {
        let (NExpr::Borrow { place, .. } | NExpr::MakeView { place, .. }) = expr else {
            return false;
        };
        !self.semantic_body.normalized.access_is_used(value)
            && self.with_current_body_cx(|cx| {
                cx.env
                    .normalized_place_is_lane(cx.carriers, place)
                    .unwrap_or(false)
            })
    }

    /// The lane copy `place` lies in.
    fn lane_copy_root(&self, place: &RuntimePlace<'db>) -> Option<RLocalId> {
        match place.root {
            PlaceRoot::Slot(copy) if self.lane_copies.contains_key(&copy) => Some(copy),
            _ => None,
        }
    }

    /// Stores the lane copy `copy` back into its lane.
    fn store_lane_copy(&mut self, bb: RBlockId, copy: RLocalId) {
        let value = self.load_runtime_place_value(
            bb,
            RuntimePlace {
                root: PlaceRoot::Slot(copy),
                path: Box::default(),
            },
            self.locals[copy.index()].semantic_ty,
        );
        let lane = self.lane_copies[&copy].clone();
        self.write_lane(bb, &lane, value);
    }

    /// Zeroes the representation of the place `place` refers to
    /// (`core::intrinsic::clear`): its words in storage or transient storage,
    /// its bytes in memory. A lane arrives as a memory copy, whose write-back
    /// masks the zeros into the lane's word.
    fn lower_clear(&mut self, bb: RBlockId, place: RLocalId) {
        let Some(RuntimeClass::Ref { pointee, kind, .. }) = self.value_class(place).cloned() else {
            panic!("`clear` takes a reference to its place");
        };
        let space = match kind {
            RefKind::Provider { space, .. } => space,
            RefKind::Object | RefKind::Native => AddressSpace::Memory,
            RefKind::Const => unreachable!("a `mut` place is not a constant"),
        };
        let ref_ty = self.locals[place.index()].semantic_ty;
        let ty = ref_ty
            .as_capability(self.db)
            .map_or(ref_ty, |(_, inner)| inner);
        let target = RuntimePlace {
            root: PlaceRoot::Ref(place),
            path: Box::default(),
        };
        if space.is_byte_addressed() {
            let zero = self.alloc_zero_value(bb, ty, &pointee);
            self.write_value_to_place(bb, target, zero, &pointee);
            return;
        }
        let size = RuntimeMemoryLayout::for_space(self.db, space)
            .class_size(&pointee)
            .expect("a cleared place has a size");
        let addr = self.lower_place_addr_of_for_class(
            ref_ty,
            bb,
            target,
            RuntimeClass::RawAddr {
                space,
                pointee: None,
            },
        );
        let addr = self.coerce_value(bb, addr, &word_class());
        let word_ref =
            provider_class_for_target_in_env(self.db, self.env, Some(TyId::u256(self.db)), space);
        let zero = self.alloc_word_const(bb, 0);
        for offset in 0..size {
            let slot = if offset == 0 {
                addr
            } else {
                let offset = self.alloc_word_const(bb, offset);
                let slot = self
                    .alloc_runtime_temp(TyId::u256(self.db), RuntimeCarrier::Value(word_class()));
                let expr = self.lower_intrinsic_arith_expr(
                    IntrinsicArithBinOp::Add,
                    false,
                    addr,
                    offset,
                    &word_scalar_class(),
                );
                self.push_stmt(bb, RStmt::Assign { dst: slot, expr });
                slot
            };
            let word = RuntimePlace {
                root: PlaceRoot::Ref(self.coerce_value(bb, slot, &word_ref)),
                path: Box::default(),
            };
            self.write_value_to_place(bb, word, zero, &word_class());
        }
    }

    /// The value of `class` whose representation is all zeros, as a state
    /// space holds before anything is written: zero scalars and words, an
    /// aggregate of zero fields, an enum's first variant with zero fields. A
    /// fresh object is not this: its contents are undefined.
    fn alloc_zero_value(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        class: &RuntimeClass<'db>,
    ) -> RLocalId {
        let expr = match class {
            RuntimeClass::Scalar(scalar) => RExpr::ConstScalar(match scalar.repr {
                ScalarRepr::Bool => ConstScalar::Bool(false),
                ScalarRepr::Int { bits, signed } => ConstScalar::Int {
                    bits,
                    signed,
                    words: Vec::new(),
                },
                ScalarRepr::Address { bits } => ConstScalar::Address {
                    bits,
                    bytes: Vec::new(),
                },
                ScalarRepr::FixedBytes { len } => {
                    ConstScalar::FixedBytes(vec![0; usize::from(len)])
                }
            }),
            // An address or a storage handle is a word.
            RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                let zero = self.alloc_word_const(bb, 0);
                return self.coerce_value(bb, zero, class);
            }
            RuntimeClass::AggregateValue { layout } => match layout.data(self.db) {
                Layout::Struct(data) => {
                    let field_tys = self.aggregate_field_tys(ty, data.fields.len());
                    let fields = data
                        .fields
                        .iter()
                        .zip(field_tys)
                        .map(|(field, field_ty)| self.alloc_zero_value(bb, field_ty, field))
                        .collect();
                    RExpr::AggregateMake {
                        layout: *layout,
                        fields,
                    }
                }
                Layout::Array(data) => {
                    let elem_ty = self.aggregate_field_tys(ty, 1)[0];
                    let elem = self.alloc_zero_value(bb, elem_ty, &data.elem);
                    RExpr::AggregateMake {
                        layout: *layout,
                        fields: vec![elem; data.len as usize].into_boxed_slice(),
                    }
                }
                Layout::Enum(data) => {
                    let classes = &data.variants[0].fields;
                    let field_tys = self.enum_variant_field_tys(ty, VariantIndex(0), classes.len());
                    let fields = classes
                        .iter()
                        .zip(field_tys)
                        .map(|(field, field_ty)| self.alloc_zero_value(bb, field_ty, field))
                        .collect();
                    RExpr::EnumMake {
                        layout: *layout,
                        variant: VariantId {
                            enum_layout: *layout,
                            index: 0,
                        },
                        fields,
                    }
                }
            },
        };
        let zero = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(class.clone()));
        self.push_stmt(bb, RStmt::Assign { dst: zero, expr });
        zero
    }

    /// The address of `place` as a `target`, and the lane copy it lies in:
    /// a lane has no address, so it is referenced as a memory copy, which
    /// the access's users store back.
    fn lower_place_address(
        &mut self,
        semantic_ty: TyId<'db>,
        bb: RBlockId,
        place: &NPlace<'db>,
        target: RuntimeClass<'db>,
    ) -> (RLocalId, Option<RLocalId>) {
        let (place, copy) = match self.resolve_place(bb, place) {
            ResolvedPlace::Lane(lane) => {
                let copy = self.hold_lane(bb, lane);
                let place = RuntimePlace {
                    root: PlaceRoot::Slot(copy),
                    path: Box::default(),
                };
                (place, Some(copy))
            }
            ResolvedPlace::Place(place) => {
                let copy = self.lane_copy_root(&place);
                (place, copy)
            }
        };
        let value = self.lower_place_addr_of_for_class(semantic_ty, bb, place, target);
        (value, copy)
    }

    fn alloc_word_const(&mut self, bb: RBlockId, value: u64) -> RLocalId {
        let dst = self.alloc_runtime_temp(TyId::u256(self.db), RuntimeCarrier::Value(word_class()));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::ConstScalar(uint_scalar(256, value)),
            },
        );
        dst
    }

    /// Calls `semantic`, a method the compiler inserts, with `inputs` for
    /// its parameters in order, passing those it keeps at runtime.
    fn call_generated(
        &mut self,
        bb: RBlockId,
        semantic: SemanticInstance<'db>,
        inputs: &[RLocalId],
    ) -> RLocalId {
        let (args, params): (Vec<_>, Vec<_>) = runtime_visible_binding_plans(self.db, semantic)
            .iter()
            .map(|plan| {
                let LocalBinding::Param { idx, .. } = plan.binding else {
                    panic!("a compiler-inserted call takes no effects")
                };
                let arg = inputs[idx];
                let class = self
                    .value_class(arg)
                    .cloned()
                    .expect("a visible argument has a runtime class");
                (arg, class)
            })
            .unzip();
        let callee_key = RuntimeInstanceKey::new(
            self.db,
            crate::instance::RuntimeInstanceSource::Semantic(semantic),
            params,
        );
        let callee = get_or_build_runtime_instance(self.db, callee_key, self.target());
        let carrier = runtime_declaration_signature(self.db, callee_key)
            .ret
            .map_or(RuntimeCarrier::Erased, RuntimeCarrier::Value);
        let result = self.alloc_runtime_temp(semantic_return_ty(self.db, semantic), carrier);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::Call {
                    callee,
                    args: args.into_boxed_slice(),
                },
            },
        );
        result
    }

    /// The instance of core trait `trait_path`'s method `name` for the
    /// trait arguments `args`, resolved in the scope of `scope_ty` when the
    /// body has none.
    fn resolve_core_method(
        &self,
        trait_path: &[&str],
        name: &str,
        args: Vec<TyId<'db>>,
        scope_ty: TyId<'db>,
    ) -> SemanticInstance<'db> {
        let scope = self
            .env
            .scope
            .or_else(|| scope_ty.as_scope(self.db))
            .expect("core trait resolution requires a scope");
        let key =
            core_method_callee_key(self.db, scope, self.env.assumptions, trait_path, name, args)
                .unwrap_or_else(|error| {
                    panic!("failed to finalize generated call to `{name}`: {error:?}")
                });
        SemanticInstance::new(self.db, key)
    }

    fn read_semantic_value(&mut self, bb: RBlockId, local: SLocalId) -> RLocalId {
        match self.semantic_local_lowering(local) {
            RuntimeLocalLowering::Erased => self.runtime_value(local),
            RuntimeLocalLowering::DirectValue => self.runtime_value(local),
            RuntimeLocalLowering::PlaceCarrier { .. }
            | RuntimeLocalLowering::PlaceBoundValue { .. } => {
                if let Some(source) = self.semantic_place_value_source(local) {
                    match source {
                        SemanticPlaceValueSource::PlaceValue { place, semantic_ty } => {
                            let place = self.lower_place(bb, &place);
                            return self.load_runtime_place_value(bb, place, semantic_ty);
                        }
                        SemanticPlaceValueSource::ValueExtract { place, semantic_ty } => {
                            if let Some(class) = self.semantic_value_class(local) {
                                let temp = self
                                    .alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(class));
                                if self.lower_value_extract_place_read(bb, temp, &place) {
                                    return temp;
                                }
                            }
                        }
                    }
                }
                let place = self.semantic_place(bb, local);
                self.load_runtime_place_value(bb, place, self.locals[local.index()].semantic_ty)
            }
        }
    }

    fn semantic_place_value_source(
        &self,
        local: SLocalId,
    ) -> Option<SemanticPlaceValueSource<'db>> {
        let roots = self.runtime_roots();
        self.with_current_body_cx(|cx| {
            RuntimeSourceQuery::new(cx.env, cx.carriers, RuntimeSourceMode::Concrete(&roots))
                .semantic_place_value_source(RuntimeOperand {
                    local,
                    value: None,
                    origin: None,
                    mode: ReadMode::Copy,
                })
        })
    }

    fn read_semantic_operand(&mut self, bb: RBlockId, operand: NOperand) -> RLocalId {
        if let Some(value) = self.normalized_value_temps[operand.value.index()] {
            return value;
        }
        self.read_semantic_value(bb, self.runtime_operand(operand).local)
    }

    fn read_runtime_operand(&mut self, bb: RBlockId, operand: RuntimeOperand) -> RLocalId {
        if let Some(value) = operand.value
            && let Some(temp) = self.normalized_value_temps[value.index()]
        {
            return temp;
        }
        self.read_semantic_value(bb, operand.local)
    }

    fn read_normalized_value(&mut self, bb: RBlockId, value: NValueId) -> RLocalId {
        if let Some(value) = self.normalized_value_temps[value.index()] {
            return value;
        }
        let local = self
            .semantic_body
            .value_local(value)
            .unwrap_or_else(|| panic!("normalized value {value:?} has no runtime representation"));
        self.read_semantic_value(bb, local)
    }

    fn runtime_operand(&self, operand: NOperand) -> RuntimeOperand {
        self.semantic_body
            .runtime_operand(operand)
            .unwrap_or_else(|| {
                panic!("normalized operand {operand:?} has no runtime representation")
            })
    }

    fn semantic_operand_ty(&self, operand: NOperand) -> TyId<'db> {
        self.semantic_body
            .normalized
            .value(operand.value)
            .expect("normalized operand value must exist")
            .ty
    }

    fn coerce_value_if_needed(
        &mut self,
        bb: RBlockId,
        value: RLocalId,
        target: &RuntimeClass<'db>,
    ) -> RLocalId {
        if self.value_class(value).is_none() || self.value_class(value) == Some(target) {
            value
        } else {
            self.coerce_value(bb, value, target)
        }
    }

    fn lower_semantic_operand_for_class(
        &mut self,
        bb: RBlockId,
        operand: NOperand,
        target: &RuntimeClass<'db>,
    ) -> RLocalId {
        let runtime_operand = self.runtime_operand(operand);
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        let selected = self.with_current_body_cx(|cx| {
            RuntimeArgSelector::new(cx.env, cx.carriers, None)
                .with_concrete_roots(&roots)
                .with_concrete_value_classes(&value_classes)
                .selected_semantic_operand_for_class(runtime_operand, target)
        });
        self.lower_selected_runtime_arg(bb, operand, &selected)
    }

    fn actual_aggregate_value_from_runtime_source(
        &mut self,
        bb: RBlockId,
        operand: RuntimeOperand,
    ) -> Option<RLocalId> {
        let value = self.read_runtime_operand(bb, operand);
        let class = self.value_class(value).cloned()?;
        let actual = class.aggregate_value_class()?;
        Some(self.coerce_value_if_needed(bb, value, &actual))
    }

    fn materialize_direct_value(
        &mut self,
        bb: RBlockId,
        local: SLocalId,
        materialized_class: &RuntimeClass<'db>,
    ) -> Option<RLocalId> {
        let place = self
            .try_semantic_place(bb, local)
            .or_else(|| self.place_from_direct_value_transport(local, materialized_class))?;
        let place_class = self.project_place_class(&place);
        let temp = self.alloc_runtime_temp(
            self.locals[local.index()].semantic_ty,
            RuntimeCarrier::Value(place_class.clone()),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: temp,
                expr: RExpr::Load { place },
            },
        );
        Some(self.coerce_value_if_needed(bb, temp, materialized_class))
    }

    fn place_from_direct_value_transport(
        &self,
        local: SLocalId,
        target: &RuntimeClass<'db>,
    ) -> Option<RuntimePlace<'db>> {
        let value = self.runtime_value(local);
        match self.value_class(value)? {
            RuntimeClass::Ref { .. } => Some(RuntimePlace {
                root: PlaceRoot::Ref(value),
                path: Box::default(),
            }),
            RuntimeClass::RawAddr {
                pointee: Some(pointee),
                space,
            } => Some(RuntimePlace {
                root: PlaceRoot::Ptr {
                    addr: value,
                    space: *space,
                    class: pointee.target(self.db).clone(),
                },
                path: Box::default(),
            }),
            RuntimeClass::RawAddr {
                pointee: None,
                space,
            } if matches!(
                target,
                RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. }
            ) =>
            {
                Some(RuntimePlace {
                    root: PlaceRoot::Ptr {
                        addr: value,
                        space: *space,
                        class: target.clone(),
                    },
                    path: Box::default(),
                })
            }
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => None,
            RuntimeClass::RawAddr { pointee: None, .. } => None,
        }
    }

    fn lower_selected_runtime_arg(
        &mut self,
        bb: RBlockId,
        operand: NOperand,
        selected: &SelectedRuntimeArg<'db>,
    ) -> RLocalId {
        let value = RuntimeArgLowerer::new(self, bb).lower_one(selected);
        let Some(class) = self.value_class(value).cloned() else {
            panic!(
                "selected runtime argument lowered without a runtime class: owner={:?}; operand={operand:?}; selected={selected:?}; value={value:?}",
                self.current_semantic_key(),
            );
        };
        assert_eq!(
            class,
            selected.class,
            "selected runtime argument class mismatch: owner={:?}; operand={operand:?}; selected={selected:?}; value={value:?}",
            self.current_semantic_key(),
        );
        value
    }

    fn transport_semantic_value(&self, local: SLocalId) -> Option<RLocalId> {
        let value = match self.semantic_local_lowering(local) {
            RuntimeLocalLowering::Erased => None,
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::PlaceCarrier { .. } => {
                Some(self.runtime_value(local))
            }
            RuntimeLocalLowering::PlaceBoundValue { provider, .. } => {
                provider.map(|provider| self.provider_binding_value(provider))
            }
        }?;
        self.value_class(value)?.is_transport().then_some(value)
    }

    fn load_runtime_place_value(
        &mut self,
        bb: RBlockId,
        place: RuntimePlace<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        let place_class = self.project_place_class(&place);
        let value = self.alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(place_class));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: value,
                expr: RExpr::Load { place },
            },
        );
        value
    }

    fn lower_place_addr_of_for_class(
        &mut self,
        semantic_ty: TyId<'db>,
        bb: RBlockId,
        place: RuntimePlace<'db>,
        target: RuntimeClass<'db>,
    ) -> RLocalId {
        let actual = self.place_addr_class(&place);
        let temp = self.alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(actual.clone()));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: temp,
                expr: RExpr::AddrOf { place },
            },
        );
        if actual == target {
            temp
        } else {
            self.coerce_value(bb, temp, &target)
        }
    }

    fn place_addr_class(&self, place: &RuntimePlace<'db>) -> RuntimeClass<'db> {
        let program = self.db as &dyn MirDb;
        resolve_runtime_place_address_class(self.db, &program, self, place)
            .unwrap_or_else(|err| panic!("invalid runtime place address class: {err:?}; {place:?}"))
    }

    fn write_semantic_value(&mut self, bb: RBlockId, local: SLocalId, src: RLocalId) {
        match self.semantic_local_lowering(local).clone() {
            RuntimeLocalLowering::Erased => {}
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::PlaceCarrier { .. } => {
                let dst = self.runtime_value(local);
                let Some(target) = self.value_class(dst).cloned() else {
                    return;
                };
                let src = self.coerce_value(bb, src, &target);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(src),
                    },
                );
            }
            RuntimeLocalLowering::PlaceBoundValue { .. } => {
                let place = self.semantic_place(bb, local);
                let target = self.project_place_class(&place);
                self.write_value_to_place(bb, place, src, &target);
            }
        }
    }

    fn lower_place(&mut self, bb: RBlockId, place: &NPlace<'db>) -> RuntimePlace<'db> {
        self.try_lower_place(bb, place).unwrap_or_else(|| {
            let root_info = match place.base {
                NPlaceBase::CapabilityTarget { carrier } => {
                    format!(
                        "capability target carrier={carrier:?} source={:?}",
                        self.semantic_body.value_local(carrier)
                    )
                }
                NPlaceBase::Deref { pointer } => format!(
                    "deref pointer={pointer:?} source={:?}",
                    self.semantic_body.value_local(pointer)
                ),
                NPlaceBase::Root(root) => format!(
                    "root={root:?} data={:?} source={:?}",
                    self.semantic_body.normalized.root(root),
                    self.semantic_body.root_local(root),
                ),
            };
            panic!(
                "cannot lower erased place root: place={place:?}; root_info={root_info}; locals={:?}",
                self.locals,
            )
        })
    }

    fn project_place_class(&self, place: &RuntimePlace<'db>) -> RuntimeClass<'db> {
        let program = self.db as &dyn MirDb;
        crate::runtime::place::project_place(self.db, &program, self, place)
            .unwrap_or_else(|err| panic!("invalid runtime place class: {err:?} place={place:?}"))
    }

    fn class_is_runtime_zst(&self, class: &RuntimeClass<'db>) -> bool {
        class.is_runtime_zst(self.db)
    }

    fn enum_variant_for_local(&self, value: SLocalId, variant: VariantIndex) -> VariantId<'db> {
        VariantId {
            enum_layout: self.enum_layout_for_local(value),
            index: variant.0,
        }
    }

    fn enum_layout_for_local(&self, value: SLocalId) -> LayoutId<'db> {
        let class = self
            .semantic_value_class(value)
            .expect("enum value should have a runtime class");
        match class {
            RuntimeClass::AggregateValue { layout } => layout,
            RuntimeClass::Ref { ref pointee, .. } => {
                pointee.aggregate_layout().unwrap_or_else(|| {
                    panic!("enum values should lower as aggregate values or refs, found {class:?}")
                })
            }
            class => {
                panic!("enum values should lower as aggregate values or refs, found {class:?}")
            }
        }
    }

    fn alloc_runtime_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        carrier: RuntimeCarrier<'db>,
    ) -> RLocalId {
        let id = RLocalId::from_u32(self.locals.len() as u32);
        self.locals.push(RLocal {
            semantic_ty,
            carrier,
            root: RuntimeLocalRoot::None,
        });
        id
    }

    fn push_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        if !self.terminated_blocks[bb.index()] {
            self.blocks[bb.index()].stmts.push(stmt);
            self.stmt_origins[bb.index()].push(self.current_origin);
        }
    }

    fn runtime_value(&self, local: SLocalId) -> RLocalId {
        RLocalId::from_u32(local.as_u32())
    }

    fn runtime_block(&self, block: NBlockId) -> RBlockId {
        RBlockId::from_u32(block.as_u32())
    }

    fn local_class(&self, local: SLocalId) -> Option<&RuntimeClass<'db>> {
        self.value_class(self.runtime_value(local))
    }

    fn local_root(&self, local: SLocalId) -> Option<PlaceRoot<'db>> {
        self.local_root_r(self.runtime_value(local))
    }

    fn local_root_r(&self, local: RLocalId) -> Option<PlaceRoot<'db>> {
        match self.locals.get(local.index())?.root.clone() {
            RuntimeLocalRoot::None => None,
            RuntimeLocalRoot::Slot(_) => Some(PlaceRoot::Slot(local)),
            RuntimeLocalRoot::Ref(_) => Some(PlaceRoot::Ref(local)),
            RuntimeLocalRoot::Ptr { space, class } => Some(PlaceRoot::Ptr {
                addr: local,
                space,
                class,
            }),
        }
    }

    fn runtime_roots(&self) -> Vec<RuntimeLocalRoot<'db>> {
        self.locals.iter().map(|local| local.root.clone()).collect()
    }

    fn normalized_value_classes(&self) -> Vec<Option<RuntimeClass<'db>>> {
        self.normalized_value_temps
            .iter()
            .map(|value| value.and_then(|value| self.value_class(value).cloned()))
            .collect()
    }

    fn value_class(&self, local: RLocalId) -> Option<&RuntimeClass<'db>> {
        match self.locals.get(local.index())?.carrier {
            RuntimeCarrier::Erased => None,
            RuntimeCarrier::Value(ref class) => Some(class),
        }
    }

    fn reify_runtime_const(
        &self,
        expected_ty: TyId<'db>,
        value: SemConstId<'db>,
    ) -> SemConstId<'db> {
        let semantic = self
            .key
            .semantic(self.db)
            .expect("runtime const reification requires a semantic instance");
        reify_runtime_const_for_ty(self.db, semantic, expected_ty, value).unwrap_or_else(|| {
            let owner = semantic.key(self.db).owner(self.db);
            let owner_name = match owner {
                BodyOwner::Func(func, _) => func
                    .name(self.db)
                    .to_opt()
                    .map(|name| {
                        format!(
                            "{} on {}",
                            name.data(self.db),
                            func.expected_self_ty(self.db)
                                .map(|ty| ty.pretty_print(self.db).to_string())
                                .unwrap_or_else(|| "<free function>".to_string()),
                        )
                    })
                    .unwrap_or_else(|| "<anonymous>".to_string()),
                _ => format!("{owner:?}"),
            };
            panic!(
                "semantic const should reify for runtime lowering: value={} ({value:?}), \
                 expected={}, owner={owner_name} ({owner:?}), subst={:?}. This is a compiler \
                 bug: the const failed to evaluate but no diagnostic was reported during type \
                 checking.",
                value.pretty_print(self.db),
                expected_ty.pretty_print(self.db),
                semantic
                    .key(self.db)
                    .subst(self.db)
                    .generic_args(self.db)
                    .iter()
                    .map(|arg| arg.pretty_print(self.db).to_string())
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn const_lowering_ty(&self, fallback: TyId<'db>, target: &RuntimeClass<'db>) -> TyId<'db> {
        if target.aggregate_layout().is_none() {
            return fallback;
        }
        // Recover the underlying aggregate value type by peeling any
        // view/borrow wrappers off the declared type.
        let mut ty = fallback;
        loop {
            if let Some(inner) = ty.as_view(self.db) {
                ty = inner;
            } else if let Some((_, inner)) = ty.as_borrow(self.db) {
                ty = inner;
            } else {
                return ty;
            }
        }
    }
}

struct RuntimeArgLowerer<'emitter, 'db> {
    emitter: &'emitter mut RmirEmitter<'db>,
    bb: RBlockId,
}

impl<'emitter, 'db> RuntimeArgLowerer<'emitter, 'db> {
    fn new(emitter: &'emitter mut RmirEmitter<'db>, bb: RBlockId) -> Self {
        Self { emitter, bb }
    }

    fn lower_all(
        mut self,
        selected: &[SelectedRuntimeArg<'db>],
    ) -> (Vec<RLocalId>, Vec<RuntimeClass<'db>>) {
        let mut runtime_args = Vec::with_capacity(selected.len());
        let mut runtime_classes = Vec::with_capacity(selected.len());
        for selected in selected {
            let value = self.lower_one(selected);
            let Some(class) = self.emitter.value_class(value).cloned() else {
                panic!(
                    "selected runtime call arg lowered without a runtime class: caller={:?}; selected={selected:?}; value={value:?}",
                    self.emitter.current_semantic_key(),
                );
            };
            assert_eq!(
                class,
                selected.class,
                "selected runtime call arg class mismatch: caller={:?}; selected={selected:?}; value={value:?}",
                self.emitter.current_semantic_key(),
            );
            runtime_args.push(value);
            runtime_classes.push(class);
        }
        (runtime_args, runtime_classes)
    }

    fn lower_one(&mut self, selected: &SelectedRuntimeArg<'db>) -> RLocalId {
        match (&selected.source, &selected.use_plan) {
            (RuntimeArgSource::SemanticOperand(operand), use_plan) => {
                let value = self.emitter.read_runtime_operand(self.bb, *operand);
                self.apply_use_plan(value, use_plan.clone(), self.semantic_ty(*operand))
            }
            (
                RuntimeArgSource::DirectValueMaterialization {
                    local,
                    materialized_class,
                },
                use_plan,
            ) => {
                let Some(value) =
                    self.emitter
                        .materialize_direct_value(self.bb, *local, materialized_class)
                else {
                    panic!(
                        "selected direct-value materialization was not lowerable: caller={:?}; local={local:?}; selected={selected:?}",
                        self.emitter.current_semantic_key(),
                    )
                };
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::RuntimeValue(local), use_plan) => {
                let value = self.emitter.runtime_value(*local);
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::TransportValue(local), use_plan) => {
                let value = self
                    .emitter
                    .transport_semantic_value(*local)
                    .unwrap_or_else(|| self.emitter.read_semantic_value(self.bb, *local));
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::PlaceAddress(source, semantic_ty), use_plan) => {
                let (value, copy) = self.emitter.lower_place_address(
                    *semantic_ty,
                    self.bb,
                    source,
                    selected.class.clone(),
                );
                if let Some(copy) = copy {
                    self.emitter.call_lane_copies.push((source.clone(), copy));
                }
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (RuntimeArgSource::PlaceValue(place, semantic_ty), use_plan) => {
                let place = self.emitter.lower_place(self.bb, place);
                let value = self
                    .emitter
                    .load_runtime_place_value(self.bb, place, *semantic_ty);
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (
                RuntimeArgSource::ValueExtract {
                    place,
                    semantic_ty,
                    value_class,
                },
                use_plan,
            ) => {
                let value = self
                    .emitter
                    .alloc_runtime_temp(*semantic_ty, RuntimeCarrier::Value(value_class.clone()));
                if !self
                    .emitter
                    .lower_value_extract_place_read(self.bb, value, place)
                {
                    panic!(
                        "selected value-extract runtime source was not lowerable: caller={:?}; selected={selected:?}",
                        self.emitter.current_semantic_key(),
                    )
                }
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (RuntimeArgSource::SemanticPlaceAddress(local, semantic_ty), use_plan) => {
                let place = self.emitter.semantic_place(self.bb, *local);
                let value = self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                );
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (RuntimeArgSource::AggregateFromRuntimeSource(operand), use_plan) => {
                let Some(value) = self
                    .emitter
                    .actual_aggregate_value_from_runtime_source(self.bb, *operand)
                else {
                    panic!(
                        "selected aggregate runtime source was not lowerable: caller={:?}; operand={operand:?}; selected={selected:?}",
                        self.emitter.current_semantic_key(),
                    )
                };
                self.apply_use_plan(value, use_plan.clone(), self.semantic_ty(*operand))
            }
            (RuntimeArgSource::Placeholder(semantic_ty), use_plan) => {
                let value = self.lower_placeholder(*semantic_ty, selected.class.clone());
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
        }
    }

    fn apply_use_plan(
        &mut self,
        value: RLocalId,
        use_plan: RuntimeValueUsePlan<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        emit_runtime_value_use_plan(self.emitter, self.bb, value, use_plan, semantic_ty)
    }

    fn semantic_ty(&self, operand: RuntimeOperand) -> TyId<'db> {
        if let Some(value) = operand.value {
            return self
                .emitter
                .semantic_body
                .normalized
                .value(value)
                .expect("normalized runtime operand value must exist")
                .ty;
        }
        self.semantic_local_ty(operand.local)
    }

    fn semantic_local_ty(&self, local: SLocalId) -> TyId<'db> {
        self.emitter.semantic_body.locals[local.index()].ty
    }

    fn lower_placeholder(&mut self, semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> RLocalId {
        let value = self
            .emitter
            .alloc_runtime_temp(semantic_ty, RuntimeCarrier::Value(class.clone()));
        self.emitter.push_stmt(
            self.bb,
            RStmt::Assign {
                dst: value,
                expr: RExpr::Placeholder { class },
            },
        );
        value
    }
}

fn word_class<'db>() -> RuntimeClass<'db> {
    RuntimeClass::Scalar(word_scalar_class())
}

fn word_scalar_class<'db>() -> ScalarClass<'db> {
    ScalarClass {
        repr: ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        role: ScalarRole::Plain,
    }
}

fn solidity_error_string_payload(bytes: &[u8]) -> Vec<u8> {
    let padded_len = bytes.len().next_multiple_of(32);
    let mut payload = Vec::with_capacity(4 + 64 + padded_len);
    payload.extend([0x08, 0xc3, 0x79, 0xa0]);
    push_abi_word_usize(&mut payload, 32);
    push_abi_word_usize(&mut payload, bytes.len());
    payload.extend(bytes);
    payload.resize(4 + 64 + padded_len, 0);
    payload
}

fn solidity_panic_payload(code: usize) -> Vec<u8> {
    let mut payload = Vec::with_capacity(36);
    payload.extend([0x4e, 0x48, 0x7b, 0x71]);
    push_abi_word_usize(&mut payload, code);
    payload
}

fn push_abi_word_usize(out: &mut Vec<u8>, value: usize) {
    let mut word = [0; 32];
    word[32 - size_of::<usize>()..].copy_from_slice(&value.to_be_bytes());
    out.extend(word);
}

fn padded_word_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut word = [0; 32];
    word[..bytes.len()].copy_from_slice(bytes);
    trim_leading_zero_bytes(&word)
}

fn usize_word_bytes(value: usize) -> Vec<u8> {
    trim_leading_zero_bytes(&value.to_be_bytes())
}

fn trim_leading_zero_bytes(bytes: &[u8]) -> Vec<u8> {
    bytes
        .iter()
        .copied()
        .skip_while(|byte| *byte == 0)
        .collect()
}

fn intrinsic_numeric_name_parts(name: &str) -> Option<(&str, &str)> {
    let op = name.strip_prefix("__")?;
    [
        "_u8", "_u16", "_u32", "_u64", "_u128", "_u256", "_usize", "_i8", "_i16", "_i32", "_i64",
        "_i128", "_i256", "_isize", "_bool",
    ]
    .iter()
    .find_map(|suffix| op.strip_suffix(suffix).map(|prefix| (prefix, *suffix)))
}

#[cfg(test)]
mod tests {
    use crate::RuntimeTarget;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::{
        semantic::{SemanticInstance, identity_semantic_instance_key},
        ty::ty_check::BodyOwner,
    };
    use url::Url;

    use crate::{build_test_runtime_package, runtime::package::runtime_instance_for_semantic};

    #[test]
    fn const_generic_zero_sized_fields_lower_through_calls() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(
            std::env::temp_dir().join("const_generic_zero_sized_fields_runtime_lowering.fe"),
        )
        .expect("fixture path should be absolute");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Marker<const ROOT: u256> {}

struct Mixed<const ROOT: u256> {
    fixed: Marker<7>,
    generic: Marker<ROOT>,
    value: u256,
}

fn pass<const ROOT: u256>(value: own Mixed<ROOT>) -> Mixed<ROOT> {
    value
}

fn forward<const ROOT: u256>(value: own Mixed<ROOT>) -> Mixed<ROOT> {
    pass(value: value)
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let diags = db.run_on_top_mod(top_mod);
        assert!(diags.is_empty(), "{}", diags.format_diags(&db));
        for name in ["pass", "forward"] {
            let func = top_mod
                .all_funcs(&db)
                .iter()
                .copied()
                .find(|func| {
                    func.name(&db)
                        .to_opt()
                        .is_some_and(|func_name| func_name.data(&db) == name)
                })
                .unwrap_or_else(|| panic!("missing function `{name}`"));
            let semantic = SemanticInstance::new(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
            );
            let body = runtime_instance_for_semantic(&db, semantic, RuntimeTarget::Evm).body(&db);
            assert_eq!(body.signature.params.len(), 1);
            assert_eq!(
                body.signature.ret.as_ref(),
                Some(&body.signature.params[0].class)
            );
        }
    }

    #[test]
    fn poseidon_mock_range_consts_with_zst_fields_lower_into_runtime_package() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(
            std::env::temp_dir().join("poseidon_mock_range_const_runtime_lowering.fe"),
        )
        .expect("fixture path should be absolute");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../fe/tests/fixtures/fe_test/poseidon_mock.fe").to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_test_runtime_package(&db, top_mod, Some("test_deterministic"));
        assert!(
            package.is_ok(),
            "poseidon_mock should lower through range consts with runtime-zst fields: {package:#?}"
        );
    }
}
