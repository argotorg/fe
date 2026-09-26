use std::{collections::HashSet, mem::size_of};

use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        EvalOutcome, FieldIndex, LayoutEvidenceBase, LayoutEvidenceBody,
        LayoutEvidenceComponentValue, LayoutEvidenceConstBinding, LayoutEvidenceConstant,
        LayoutEvidenceExpr, LayoutEvidenceIndex, LayoutEvidenceOperand, RuntimeSizeError, SBlockId,
        SConst, SLocalId, SStmtId, SemConstId, SemConstScalar, SemConstValue, SemOrigin,
        SemanticCalleeRef, SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticConstRef,
        SemanticInstance, SemanticInstanceKey, SemanticLocalRole, VariantIndex, eval_const_ref,
        generated_callee_key, get_or_build_semantic_instance, layout_evidence_body,
        normalized::{
            NBlockId, NDataPath, NDataProjection, NEffectArg, NExpr, NIndex, NOperand, NPlace,
            NPlaceBase, NRootKind, NStatement, NStatementId, NStatementKind, NSuccessor,
            NTerminator, NTerminatorKind, NValueId, ReadMode,
        },
        reify_runtime_const_for_ty, runtime_size_bytes, sem_const_ty,
        verify_layout_evidence_runtime_compatibility,
    },
    ty::{
        CallableLayoutParamPort,
        const_expr::ConstExpr,
        const_ty::ConstTyData,
        corelib::{
            PrimitiveWrapperCallKind, RuntimeBuiltinFuncKind, core_primitive_wrapper_call_kind,
            resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path,
            runtime_builtin_func_kind,
        },
        trait_def::TraitInstId,
        trait_resolution::{
            GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
        },
        ty_check::BodyOwner,
        ty_def::{TyData, TyId},
    },
};
use hir::hir_def::{
    ArithBinOp, BinOp, CompBinOp, Expr, Func, IdentId, Partial, UnOp, attr::ArithmeticMode,
    scope_graph::ScopeId,
};
use hir::projection::IndexSource;
use hir::semantic::ProviderBinding;

use crate::{
    db::MirDb,
    instance::{RuntimeInstance, RuntimeInstanceKey, get_or_build_runtime_instance},
    resolve_runtime_place_address_class,
    runtime::place::{project_field_class, project_index_class, project_variant_field_class},
    runtime::{
        AddressSpaceKind, ConstRegionId, ConstScalar, IntrinsicArithBinOp, LayoutId, PlaceElem,
        PlaceRoot, RBlock, RBlockId, RExpr, RLocal, RLocalId, RStmt, RTerminator, RefKind, RefView,
        RuntimeBody, RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeExitBehavior,
        RuntimeLayoutMap, RuntimeLocalRoot, RuntimePlace, RuntimeProviderBinding,
        RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole, VariantId,
        code_region::runtime_code_region_for_semantic_ref,
        package::{LowerError, generated_call_error, runtime_instance_for_semantic},
    },
};

use super::{
    abi::{RuntimeAbiPlan, runtime_body_abi_plan, runtime_declaration_abi_plan},
    arg_selector::RuntimeArgSelector,
    boundary::{BoundarySiteAllocator, RuntimeValueUsePlan, boundary_spec_for_ty_in_env},
    call_input::{CompiledCallInputPlan, compile_call_input_plan_for_semantic},
    classify::{
        BodyEnv, BodyStaticFacts, ContractMetadataBuiltin, GenericNumericIntrinsicKind,
        InferClassCache, RuntimeBodyCx, contract_metadata_builtin, generic_numeric_intrinsic_kind,
        semantic_return_ty,
    },
    consts::{
        aggregate_const_ref_class, aggregate_const_ref_region, collect_const_ref_regions,
        const_scalar_for_class, const_scalar_from_value, enum_tag_scalar,
        evaluated_const_ref_value, lower_const_region,
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
    layout_evidence::{
        runtime_layout_map_for_map_ty, runtime_layout_map_for_shape, runtime_layout_scalar_const,
    },
    realize::{
        RuntimeArgSource, RuntimeValueUseEmitter, SelectedRuntimeArg, emit_runtime_value_use_plan,
    },
    returns::declaration_runtime_return_class,
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
    source::{
        RuntimeSourceMode, RuntimeSourceQuery, SemanticPlaceValueSource,
        alias_source_place_for_local as source_alias_source_place_for_local,
        data_path_index_bounds, nonself_alias_source_place_for_local,
    },
    tuple::RuntimeTupleFieldEmitter,
    type_info::{
        RuntimeTypeEnv, effect_handle_transport_class_for_ty_in_env,
        provider_class_for_target_in_env, runtime_array_len, runtime_effect_handle_info,
        stored_class_for_ty_in_env, top_level_class_for_ty_in_env,
        validate_runtime_array_extents_in_env,
    },
};

pub fn lower_to_rmir<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> Result<RuntimeBody<'db>, LowerError> {
    let key = instance.key(db);
    let semantic = key
        .semantic(db)
        .expect("semantic lowering only applies to semantic runtime instances");
    let normalized_body = RuntimeSemanticBody::admitted(db, semantic).map_err(|error| {
        LowerError::Unsupported(format!(
            "semantic normalization failed for {:?}: {error:?}",
            semantic.key(db)
        ))
    })?;
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
    let abi = runtime_body_abi_plan(db, key, &normalized_body);
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
    let visible_ret_class = abi.returns.visible.clone();
    if let Some(ret_class) = visible_ret_class
        .clone()
        .filter(|class| class.contains_transport(db))
    {
        let return_locals = normalized_body
            .normalized
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator.kind {
                NTerminatorKind::Return(Some(value)) => normalized_body.operand_local(*value),
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Branch { .. }
                | NTerminatorKind::MatchEnum { .. }
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => None,
            })
            .collect::<Vec<_>>();
        inferer.seed_return_class(&return_locals, ret_class);
    }
    let inferred = inferer.run();
    let mut emitter = RmirEmitter::new(db, instance, normalized_body, facts, inferred, abi)?;
    emitter.lower_blocks();
    Ok(emitter.finish())
}

pub(super) fn check_runtime_body_supported<'db>(
    db: &'db dyn MirDb,
    key: SemanticInstanceKey<'db>,
    body: &RuntimeSemanticBody<'db>,
) -> Result<(), LowerError> {
    let evidence = layout_evidence_body(db, body.owner()).map_err(|error| {
        LowerError::Unsupported(format!(
            "layout evidence lowering failed for {key:?}: {error:?}"
        ))
    })?;
    for block in &body.normalized.blocks {
        for stmt in &block.statements {
            if let NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } = &stmt.kind
                && let BodyOwner::Func(func) = callee.key.owner(db)
                && func.containing_trait(db).is_some()
                && func.body(db).is_none()
                && contract_metadata_builtin(db, get_or_build_semantic_instance(db, callee.key))
                    .is_none()
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
                let bindings = evidence
                    .constant_bindings
                    .get(dst.index())
                    .map_or(&[][..], AsRef::as_ref);
                let expected_ty = body.normalized.values[dst.index()].ty;
                if formal_runtime_layout_root_binding(db, value, bindings).is_none()
                    && reify_runtime_const_for_ty(db, body.owner(), expected_ty, value).is_none()
                {
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
                let bindings = evidence
                    .constant_bindings
                    .get(dst.index())
                    .map_or(&[][..], AsRef::as_ref);
                if formal_runtime_layout_root_binding(db, value, bindings).is_none()
                    && reify_runtime_const_for_ty(db, body.owner(), expected_ty, value).is_none()
                {
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
                && let Some(value_ty) = panic_payload_ty(db, body, *callee, args)
            {
                let impl_env = key.impl_env(db);
                ensure_panic_payload_encodable(
                    db,
                    impl_env.normalization_scope(db),
                    impl_env.assumptions(db),
                    value_ty,
                )?;
            }
        }
    }
    Ok(())
}

fn formal_runtime_layout_root_binding<'a, 'db>(
    db: &'db dyn MirDb,
    value: SemConstId<'db>,
    bindings: &'a [LayoutEvidenceConstBinding<'db>],
) -> Option<&'a LayoutEvidenceConstBinding<'db>> {
    let SemConstValue::Description(term) = value.value(db) else {
        return None;
    };
    if !matches!(term.data(db), ConstTyData::TyParam(_, _)) {
        return None;
    }
    let param_ty = TyId::new(db, TyData::ConstTy(term));
    bindings.iter().find(|binding| binding.param == param_ty)
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
    if invocation.key.owner(db) != BodyOwner::Func(size_of) {
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

fn panic_payload_ty<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    callee: SemanticCalleeRef<'db>,
    args: &[NOperand],
) -> Option<TyId<'db>> {
    let BodyOwner::Func(func) = callee.key.owner(db) else {
        return None;
    };
    if runtime_builtin_func_kind(db, func) != Some(RuntimeBuiltinFuncKind::PanicWithValue) {
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
        NExpr::Unary {
            op: UnOp::Minus, ..
        }
        | NExpr::Binary {
            op:
                BinOp::Arith(
                    ArithBinOp::Add
                    | ArithBinOp::Sub
                    | ArithBinOp::Mul
                    | ArithBinOp::Div
                    | ArithBinOp::Rem
                    | ArithBinOp::Pow,
                ),
            ..
        }
        | NExpr::Call { .. } => true,
        NExpr::Forward { .. }
        | NExpr::ProjectValue { .. }
        | NExpr::StructuralRepack { .. }
        | NExpr::CodeRegionRef { .. }
        | NExpr::Load { .. }
        | NExpr::Const(_)
        | NExpr::Unary { .. }
        | NExpr::Binary { .. }
        | NExpr::PointerCast { .. }
        | NExpr::ScalarCast { .. }
        | NExpr::AggregateMake { .. }
        | NExpr::MakeHandle { .. }
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
    pub(super) layout_evidence: &'db LayoutEvidenceBody<'db>,
    pub(super) facts: BodyStaticFacts<'db>,
    pub(super) abi: RuntimeAbiPlan<'db>,
    pub(super) env: RuntimeTypeEnv<'db>,
    const_ref_regions: HashSet<ConstRegionId<'db>>,
    pub(super) semantic_carriers: Vec<RuntimeCarrier<'db>>,
    pub(super) semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    pub(super) provider_bindings: Vec<RuntimeProviderBinding<'db>>,
    layout_evidence_locals: Vec<RLocalId>,
    normalized_value_temps: Vec<Option<RLocalId>>,
    pub(super) locals: Vec<RLocal<'db>>,
    pub(super) blocks: Vec<RBlock<'db>>,
    pub(super) stmt_origins: Vec<Vec<SemOrigin<'db>>>,
    pub(super) terminator_origins: Vec<SemOrigin<'db>>,
    pub(super) current_origin: SemOrigin<'db>,
    pub(super) terminated_blocks: Vec<bool>,
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
        abi: RuntimeAbiPlan<'db>,
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
        let layout_evidence = layout_evidence_body(db, semantic).map_err(|error| {
            LowerError::Unsupported(format!(
                "layout evidence lowering failed for {:?}: {error:?}",
                semantic.key(db)
            ))
        })?;
        verify_layout_evidence_runtime_compatibility(
            db,
            &semantic_body.normalized,
            &semantic_body.layout_plan,
            &semantic_body.source,
            layout_evidence,
        )
        .map_err(|error| {
            LowerError::Unsupported(format!(
                "layout evidence is incompatible with runtime semantic body for {:?}: {error:?}",
                semantic.key(db)
            ))
        })?;
        let const_ref_regions = collect_const_ref_regions(db, env, &semantic_body);
        let terminated_blocks = vec![false; semantic_body.normalized.blocks.len()];
        let stmt_origins = vec![Vec::new(); semantic_body.normalized.blocks.len()];
        let terminator_origins = vec![SemOrigin::Synthetic; semantic_body.normalized.blocks.len()];
        let mut locals = semantic_body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, local)| RLocal {
                semantic_ty: local.ty,
                carrier: carriers.get(idx).cloned().unwrap_or(RuntimeCarrier::Erased),
                root: roots.get(idx).cloned().unwrap_or(RuntimeLocalRoot::None),
            })
            .collect::<Vec<_>>();
        if abi.evidence_params.len() != layout_evidence.params.len() {
            return Err(LowerError::Unsupported(
                "layout evidence ABI parameter count does not match its body".into(),
            ));
        }
        let mut layout_evidence_locals = vec![None; layout_evidence.locals.len()];
        for (abi_param, evidence_local) in abi
            .evidence_params
            .iter()
            .zip(layout_evidence.params.iter().copied())
        {
            if layout_evidence.locals[evidence_local.index()]
                .param
                .as_ref()
                != Some(&abi_param.source)
            {
                return Err(LowerError::Unsupported(format!(
                    "layout evidence ABI parameter does not match its body local: {:?}",
                    abi_param.source
                )));
            }
            let runtime_local = RLocalId::from_u32(locals.len() as u32);
            let map = runtime_layout_map_for_map_ty(db, env, &abi_param.map_ty);
            if abi_param.param.local != runtime_local || abi_param.param.class != map.class() {
                return Err(LowerError::Unsupported(format!(
                    "layout evidence ABI parameter does not match its body local: {:?}",
                    abi_param.source
                )));
            }
            locals.push(RLocal {
                semantic_ty: abi_param.map_ty.scalar_ty,
                carrier: RuntimeCarrier::Value(map.class()),
                root: RuntimeLocalRoot::None,
            });
            layout_evidence_locals[evidence_local.index()] = Some(runtime_local);
        }
        for (index, metadata) in layout_evidence.locals.iter().enumerate() {
            if layout_evidence_locals[index].is_some() {
                continue;
            }
            if metadata.param.is_some() {
                return Err(LowerError::Unsupported(format!(
                    "layout evidence body parameter is absent from its ABI: {:?}",
                    metadata.param
                )));
            }
            let runtime_local = RLocalId::from_u32(locals.len() as u32);
            locals.push(RLocal {
                semantic_ty: metadata.map_ty.scalar_ty,
                carrier: RuntimeCarrier::Value(
                    runtime_layout_map_for_map_ty(db, env, &metadata.map_ty).class(),
                ),
                root: RuntimeLocalRoot::None,
            });
            layout_evidence_locals[index] = Some(runtime_local);
        }
        let layout_evidence_locals = layout_evidence_locals
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| {
                LowerError::Unsupported(
                    "layout evidence body contains an unmapped local".to_string(),
                )
            })?;
        let blocks = Vec::with_capacity(semantic_body.normalized.blocks.len());
        let normalized_value_temps = vec![None; semantic_body.normalized.values.len()];
        Ok(Self {
            db,
            instance,
            key,
            semantic_body,
            layout_evidence,
            facts,
            abi,
            env,
            const_ref_regions,
            semantic_carriers,
            semantic_locals,
            provider_bindings,
            layout_evidence_locals,
            normalized_value_temps,
            locals,
            blocks,
            stmt_origins,
            terminator_origins,
            current_origin: SemOrigin::Synthetic,
            terminated_blocks,
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
            signature: self.abi.signature(),
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

    fn runtime_layout_map_for_evidence_local(
        &self,
        local: hir::analysis::semantic::LayoutEvidenceLocalId,
    ) -> RuntimeLayoutMap<'db> {
        runtime_layout_map_for_map_ty(
            self.db,
            self.env,
            &self.layout_evidence.locals[local.index()].map_ty,
        )
    }

    fn layout_evidence_runtime_local(
        &self,
        local: hir::analysis::semantic::LayoutEvidenceLocalId,
    ) -> RLocalId {
        self.layout_evidence_locals[local.index()]
    }

    fn alloc_layout_map_temp(&mut self, map: &RuntimeLayoutMap<'db>) -> RLocalId {
        self.alloc_runtime_temp(map.scalar_ty(), RuntimeCarrier::Value(map.class()))
    }

    fn lower_layout_base(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        base: LayoutEvidenceBase<'db>,
    ) -> RLocalId {
        match base {
            LayoutEvidenceBase::Slot(slot) => self.alloc_layout_scalar(bb, map, slot),
            LayoutEvidenceBase::Root(root) => {
                let TyData::ConstTy(const_ty) = root.data(self.db) else {
                    panic!("static layout root must be a const value: {root:?}")
                };
                let ty = const_ty.ty(self.db);
                let value = SemConstId::new(self.db, SemConstValue::Description(*const_ty));
                self.lower_sem_const_as_class(
                    bb,
                    value,
                    ty,
                    &RuntimeClass::Scalar(map.scalar().clone()),
                    &[],
                )
            }
        }
    }

    fn lower_layout_constant(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        value: &LayoutEvidenceConstant<'db>,
    ) -> RLocalId {
        assert_eq!(map.rank(), value.strides.len());
        let base = self.lower_layout_base(bb, map, value.base);
        if map.rank() == 0 {
            return base;
        }
        let strides = value
            .strides
            .iter()
            .copied()
            .map(|stride| self.alloc_layout_scalar(bb, map, stride))
            .collect::<Vec<_>>();
        let result = self.alloc_layout_map_temp(map);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::LayoutMapAffine {
                    map: map.clone(),
                    base,
                    strides: strides.into_boxed_slice(),
                },
            },
        );
        result
    }

    fn alloc_layout_scalar(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        value: usize,
    ) -> RLocalId {
        let local = self.alloc_runtime_temp(
            map.scalar_ty(),
            RuntimeCarrier::Value(RuntimeClass::Scalar(map.scalar().clone())),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: local,
                expr: RExpr::ConstScalar(runtime_layout_scalar_const(map, value)),
            },
        );
        local
    }

    fn lower_layout_operand(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        operand: &LayoutEvidenceOperand<'db>,
    ) -> RLocalId {
        match operand {
            LayoutEvidenceOperand::Local(local) => self.layout_evidence_runtime_local(*local),
            LayoutEvidenceOperand::Constant(value) => self.lower_layout_constant(bb, map, value),
        }
    }

    fn lower_layout_index(&mut self, bb: RBlockId, index: LayoutEvidenceIndex) -> RLocalId {
        match index {
            LayoutEvidenceIndex::Constant(index) => self.alloc_u256_const(bb, index),
            LayoutEvidenceIndex::Dynamic(index) => self.read_semantic_value(bb, index),
        }
    }

    fn project_layout_map_once(
        &mut self,
        bb: RBlockId,
        source_map: &RuntimeLayoutMap<'db>,
        source: RLocalId,
        index: RLocalId,
    ) -> RLocalId {
        let child_map = source_map
            .projected()
            .expect("layout-map projection requires a ranked source");
        let child = self.alloc_layout_map_temp(&child_map);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: child,
                expr: RExpr::LayoutMapProject {
                    map: source_map.clone(),
                    source,
                    index,
                },
            },
        );
        child
    }

    fn lower_layout_array(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        elements: &[LayoutEvidenceExpr<'db>],
    ) -> RLocalId {
        assert_eq!(map.dimensions().first().copied(), Some(elements.len()));
        let child_map = map
            .projected()
            .expect("layout-map array construction requires a ranked map");
        let result = self.alloc_layout_map_temp(map);
        if let Some(first) = elements.first()
            && elements.iter().all(|element| element == first)
        {
            let element = self.lower_layout_expr(bb, &child_map, first);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: result,
                    expr: RExpr::LayoutMapRepeat {
                        map: map.clone(),
                        element,
                    },
                },
            );
            return result;
        }
        let elements = elements
            .iter()
            .map(|element| self.lower_layout_expr(bb, &child_map, element))
            .collect::<Vec<_>>();
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::LayoutMapDense {
                    map: map.clone(),
                    elements: elements.into_boxed_slice(),
                },
            },
        );
        result
    }

    fn lower_layout_repeat(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        element: &LayoutEvidenceExpr<'db>,
    ) -> RLocalId {
        let child_map = map
            .projected()
            .expect("layout-map repeat construction requires a ranked map");
        let element = self.lower_layout_expr(bb, &child_map, element);
        let result = self.alloc_layout_map_temp(map);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::LayoutMapRepeat {
                    map: map.clone(),
                    element,
                },
            },
        );
        result
    }

    fn update_layout_map(
        &mut self,
        bb: RBlockId,
        source_map: &RuntimeLayoutMap<'db>,
        source: RLocalId,
        indices: &[LayoutEvidenceIndex],
        replacement_map: &RuntimeLayoutMap<'db>,
        replacement: RLocalId,
    ) -> RLocalId {
        let Some((index, remaining)) = indices.split_first() else {
            assert_eq!(source_map, replacement_map);
            return replacement;
        };
        let child_map = source_map
            .projected()
            .expect("layout-map update requires a ranked source");
        let index = self.lower_layout_index(bb, *index);
        let replacement = if remaining.is_empty() {
            assert_eq!(&child_map, replacement_map);
            replacement
        } else {
            let original = self.project_layout_map_once(bb, source_map, source, index);
            self.update_layout_map(
                bb,
                &child_map,
                original,
                remaining,
                replacement_map,
                replacement,
            )
        };
        let result = self.alloc_layout_map_temp(source_map);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::LayoutMapPatch {
                    map: source_map.clone(),
                    source,
                    index,
                    replacement,
                },
            },
        );
        result
    }

    fn lower_layout_expr(
        &mut self,
        bb: RBlockId,
        map: &RuntimeLayoutMap<'db>,
        expr: &LayoutEvidenceExpr<'db>,
    ) -> RLocalId {
        match expr {
            LayoutEvidenceExpr::Use(operand) => self.lower_layout_operand(bb, map, operand),
            LayoutEvidenceExpr::Project { source, indices } => {
                let mut source_map = match source {
                    LayoutEvidenceOperand::Local(local) => {
                        self.runtime_layout_map_for_evidence_local(*local)
                    }
                    LayoutEvidenceOperand::Constant(value) => {
                        runtime_layout_map_for_map_ty(self.db, self.env, &value.map_ty)
                    }
                };
                let mut value = self.lower_layout_operand(bb, &source_map, source);
                for index in indices {
                    let index = self.lower_layout_index(bb, *index);
                    value = self.project_layout_map_once(bb, &source_map, value, index);
                    source_map = source_map
                        .projected()
                        .expect("layout-map projection consumed too many axes");
                }
                assert_eq!(&source_map, map);
                value
            }
            LayoutEvidenceExpr::Array { elements } => self.lower_layout_array(bb, map, elements),
            LayoutEvidenceExpr::Repeat { len, element } => {
                assert_eq!(map.dimensions().first(), Some(len));
                self.lower_layout_repeat(bb, map, element)
            }
            LayoutEvidenceExpr::Update {
                source,
                indices,
                value,
            } => {
                assert!(indices.len() <= map.rank());
                let replacement_map = runtime_layout_map_for_shape(
                    self.db,
                    self.env,
                    map.scalar_ty(),
                    &map.dimensions()[indices.len()..],
                );
                let replacement = self.lower_layout_expr(bb, &replacement_map, value);
                let source = self.lower_layout_operand(bb, map, source);
                self.update_layout_map(bb, map, source, indices, &replacement_map, replacement)
            }
            LayoutEvidenceExpr::CallResult { .. } => {
                panic!("call-result layout evidence must be unpacked by call lowering")
            }
        }
    }

    fn lower_layout_value_args(
        &mut self,
        bb: RBlockId,
        local: SLocalId,
        semantic: SemanticInstance<'db>,
        abi: &RuntimeAbiPlan<'db>,
    ) -> Vec<RLocalId> {
        let callee_env = RuntimeTypeEnv::for_semantic(self.db, semantic);
        let value = self.layout_evidence.semantic_values[local.index()].clone();
        let mut args = Vec::new();
        for expected in &abi.evidence_params {
            let port = match &expected.source {
                CallableLayoutParamPort::Input(port) => &port.component,
                CallableLayoutParamPort::OutputWitness(port) => port,
            };
            let (component_id, actual) = value.schema.component_by_port(port).unwrap_or_else(|| {
                panic!(
                    "layout evidence source does not match call parameter: local={local:?}, expected={expected:?}, actual={:?}",
                    value.schema.components,
                )
            });
            let operand = match &value.components[component_id.index()] {
                LayoutEvidenceComponentValue::Known(value) => {
                    LayoutEvidenceOperand::Constant(value.clone())
                }
                LayoutEvidenceComponentValue::Dynamic(local) => {
                    LayoutEvidenceOperand::Local(*local)
                }
            };
            assert_eq!(actual.map_ty(), expected.map_ty);
            let expr = LayoutEvidenceExpr::Use(operand);
            let map = runtime_layout_map_for_map_ty(self.db, callee_env, &expected.map_ty);
            assert_eq!(map.class(), expected.param.class);
            args.push(self.lower_layout_expr(bb, &map, &expr));
        }
        args
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
        default_space: crate::runtime::AddressSpaceKind,
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
        if !self.blocks.is_empty() {
            self.lower_effect_handle_provider_transports(RBlockId::from_u32(0));
        }
        let blocks = self.semantic_body.normalized.blocks.clone();
        for (idx, block) in blocks.iter().enumerate() {
            let bb = RBlockId::from_u32(idx as u32);
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                self.with_current_origin(stmt.origin, |this| this.lower_stmt(bb, stmt_idx, stmt));
                if self.terminated_blocks[bb.index()] {
                    break;
                }
            }
            if !self.terminated_blocks[bb.index()] {
                self.with_current_origin(block.terminator.origin, |this| {
                    this.blocks[bb.index()].terminator =
                        this.lower_terminator(bb, &block.terminator);
                    this.terminator_origins[bb.index()] = this.current_origin;
                });
            }
        }
    }

    fn lower_effect_handle_provider_transports(&mut self, bb: RBlockId) {
        for index in 0..self.provider_bindings.len() {
            let binding = self.provider_bindings[index].clone();
            if self.value_class(binding.value) == Some(&binding.provider_class) {
                continue;
            }
            assert!(
                binding.value.index() < self.semantic_body.locals.len(),
                "effect-handle provider source must be a semantic local"
            );
            let source = SLocalId::from_u32(binding.value.as_u32());
            let handle_ty = self.semantic_body.locals[source.index()].ty;
            let transport = effect_handle_transport_class_for_ty_in_env(
                self.db, self.env, handle_ty,
            )
            .unwrap_or_else(|| {
                panic!(
                    "provider source class differs from its transport for non-handle type {}",
                    handle_ty.pretty_print(self.db),
                )
            });
            assert_eq!(
                transport, binding.provider_class,
                "effect-handle provider binding must use its canonical transport class"
            );
            let value = self.lower_effect_handle_to_transport(
                bb,
                binding.value,
                &binding.provider_class,
                handle_ty,
                Some(source),
            );
            self.provider_bindings[index].value = value;
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

    fn lower_stmt(&mut self, bb: RBlockId, stmt_idx: usize, stmt: &NStatement<'db>) {
        self.lower_stmt_index_checks(bb, &stmt.kind);
        if self.terminated_blocks[bb.index()] {
            return;
        }
        match &stmt.kind {
            NStatementKind::Define { result, expr } => {
                self.lower_assign(bb, stmt_idx, stmt.source, *result, expr)
            }
            NStatementKind::Store { destination, value } => {
                if !(stmt.source.is_none()
                    && self.lower_local_root_assignment(bb, destination, *value))
                {
                    let place_class = self.with_current_body_cx(|cx| {
                        cx.env.normalized_place_class(cx.carriers, destination)
                    });
                    if place_class.is_some() {
                        let place = self.lower_place(bb, destination);
                        let target = self.project_place_class(&place);
                        // A native-reference slot stores its carrier, whereas
                        // a scalar destination needs the referent's value.
                        let value = self.lower_semantic_operand_for_class(bb, *value, &target);
                        self.write_value_to_place(bb, place, value, &target);
                    }
                }
            }
        }
        if !matches!(
            &stmt.kind,
            NStatementKind::Define {
                expr: NExpr::Call { .. },
                ..
            }
        ) && !self.terminated_blocks[bb.index()]
        {
            self.lower_layout_evidence_statement(bb, stmt.id);
        }
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

    fn lower_layout_evidence_statement(&mut self, bb: RBlockId, stmt_id: NStatementId) {
        let statement = self
            .layout_evidence
            .statement(stmt_id)
            .expect("verified layout evidence must contain every normalized statement")
            .clone();
        debug_assert!(statement.call.is_none());
        for assignment in statement.assignments {
            self.lower_layout_evidence_assignment(bb, &assignment);
        }
    }

    fn lower_layout_evidence_assignment(
        &mut self,
        bb: RBlockId,
        assignment: &hir::analysis::semantic::LayoutEvidenceAssignment<'db>,
    ) {
        let map = self.runtime_layout_map_for_evidence_local(assignment.dst);
        let value = self.lower_layout_expr(bb, &map, &assignment.expr);
        let dst = self.layout_evidence_runtime_local(assignment.dst);
        if value != dst {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Use(value),
                },
            );
        }
    }

    fn lower_assign(
        &mut self,
        bb: RBlockId,
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
            self.semantic_body.normalized.blocks[bb.index()].statements[stmt_idx].id;
        let direct_class = self.current_expr_direct_class(bb.index(), stmt_idx, expr);
        if self.root_provider_value_load_is_lazy(dst, expr)
            || ((matches!(expr, NExpr::MakeView { .. })
                || (stmt_id.is_none()
                    && matches!(expr, NExpr::Load { place, .. } if place.path.is_empty())))
                && self.runtime_value_is_unused(result))
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
                .get(bb.index())
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
                | NExpr::MakeHandle { .. }
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
                    | NExpr::MakeHandle { .. }
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
                    .current_expr_direct_class(bb.index(), stmt_idx, expr)
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
                let bindings = self.layout_evidence.constant_bindings[result.index()].clone();
                self.lower_const_into(bb, dst, const_, &bindings);
            }
            NExpr::Unary { op, value } => {
                let value = self.read_semantic_operand(bb, *value);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Unary { op: *op, value },
                    },
                );
            }
            NExpr::Binary { op, lhs, rhs } => {
                let lhs = self.read_semantic_operand(bb, *lhs);
                let rhs = self.read_semantic_operand(bb, *rhs);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Binary { op: *op, lhs, rhs },
                    },
                );
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
            NExpr::AggregateMake { ty, fields }
            | NExpr::MakeHandle {
                ty,
                variant: None,
                fields,
                ..
            } => self.lower_aggregate_make(bb, dst, *ty, fields),
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            }
            | NExpr::MakeHandle {
                ty: enum_ty,
                variant: Some(variant),
                fields,
                ..
            } => self.lower_enum_make(bb, dst, *enum_ty, *variant, fields),
            NExpr::Borrow { place, .. } | NExpr::MakeView { place, .. } => {
                if matches!(expr, NExpr::MakeView { .. }) && !dst_class.is_transport() {
                    self.lower_semantic_place_read_into(bb, dst, place, &dst_class);
                    return;
                }
                let place = self.lower_place(bb, place);
                let value = self.lower_place_addr_of_for_class(
                    self.locals[dst.index()].semantic_ty,
                    bb,
                    place,
                    dst_class,
                );
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

    fn lower_const_into(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        const_: &SConst<'db>,
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) {
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
        if matches!(const_, SConst::Ref(..)) {
            self.lower_const_ref_for_class(bb, dst, value, expected_ty, &target, bindings);
        } else {
            self.lower_sem_const_for_class(bb, dst, value, expected_ty, &target, bindings);
        }
    }

    fn lower_const_ref_for_class(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
        target: &RuntimeClass<'db>,
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) {
        let value = self.reify_runtime_const(expected_ty, value);
        if aggregate_const_ref_class(self.db, self.env, value).is_some()
            && self.target_accepts_const_ref_source(target)
        {
            let src = self.lower_sem_const_as_const_handle(bb, value, expected_ty);
            let value = if self.value_class(src) == Some(target) {
                src
            } else {
                self.coerce_value(bb, src, target)
            };
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Use(value),
                },
            );
        } else {
            self.lower_sem_const_for_class(bb, dst, value, expected_ty, target, bindings);
        }
    }

    fn lower_sem_const_for_class(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
        target: &RuntimeClass<'db>,
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) {
        let src = self.lower_sem_const_as_class(bb, value, expected_ty, target, bindings);
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
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) -> RLocalId {
        if let Some(value) = self.runtime_layout_root_value_for_const(bb, value, bindings) {
            return value;
        }
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
        if let Some(value) = self.try_lower_bytes_array_const_as_class(bb, ty, value, target) {
            return value;
        }
        if self.known_const_ref_region(value).is_some() {
            let value = self.lower_sem_const_as_const_handle(bb, value, ty);
            return if self.value_class(value) == Some(target) {
                value
            } else {
                self.coerce_value(bb, value, target)
            };
        }
        if let Some(scalar) = const_scalar_from_value(self.db, self.env, value) {
            return self.lower_sem_const_scalar(bb, ty, scalar);
        }
        if let RuntimeClass::RawAddr { .. } = target
            && let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(&value, &word_scalar_class())
        {
            return self.lower_sem_const_scalar_with_class(bb, ty, target.clone(), scalar);
        }
        if let RuntimeClass::Scalar(class) = target
            && let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(&value, class)
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
                self.lower_non_scalar_const_as_class(bb, value, ty, target, bindings)
            }
            RuntimeClass::Ref {
                kind: RefKind::Provider { .. },
                ..
            } => self.lower_non_scalar_const_as_class(bb, value, ty, target, bindings),
            RuntimeClass::Ref { .. } => {
                panic!(
                    "non-scalar semantic const {value:?} cannot lower directly to ref class {target:?}"
                )
            }
            RuntimeClass::AggregateValue { layout: _ } => {
                let src =
                    self.lower_non_scalar_const_as_class(bb, value, expected_ty, target, bindings);
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
                self.lower_non_scalar_const_as_class(bb, value, ty, target, bindings)
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
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) -> RLocalId {
        if let Some(value) = self.runtime_layout_root_value_for_const(bb, value, bindings) {
            return value;
        }
        let value = self.reify_runtime_const(expected_ty, value);
        let ty = expected_ty;
        if let Some(value) = self.try_lower_dyn_string_literal(bb, ty, value) {
            return value;
        }
        let layout = self.layout_for_ty(ty);
        if let Some(value) = self.try_lower_bytes_array_const_as_class(
            bb,
            ty,
            value,
            &RuntimeClass::AggregateValue { layout },
        ) {
            return value;
        }
        if let Some(scalar) = const_scalar_from_value(self.db, self.env, value) {
            return self.lower_sem_const_scalar(bb, ty, scalar);
        }
        if let SemConstValue::Scalar { value, .. } = value.value(self.db)
            && let Some(scalar) = const_scalar_for_class(&value, &word_scalar_class())
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
            | SemConstValue::Enum { .. } => self.lower_non_scalar_const_as_class(
                bb,
                value,
                ty,
                &RuntimeClass::AggregateValue { layout },
                bindings,
            ),
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
            .then(|| self.lower_dyn_string_literal(bb, ty, &bytes))
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
                .top_level_class_for_ty(field_ty, AddressSpaceKind::Memory)
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
        let (raw_ptr, ptr) = self.malloc_bytes(bb, size);

        let layout = self.layout_for_ty(ty);
        let dst = self.alloc_runtime_temp(
            ty,
            RuntimeCarrier::Value(RuntimeClass::AggregateValue { layout }),
        );
        let ctor_elems = aggregate_ctor_elems_for_layout(self.db, layout, 3);
        self.lower_aggregate_values(bb, dst, layout, &ctor_elems, &[raw_ptr, len, size]);
        self.push_ignored_builtin(
            bb,
            crate::runtime::RuntimeBuiltin::Mstore {
                addr: ptr,
                value: len,
            },
        );
        for (idx, chunk) in bytes.chunks(32).enumerate() {
            let addr = self.alloc_runtime_temp(
                TyId::u256(self.db),
                RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
            );
            let offset = self.alloc_u256_const(bb, 32 * (idx + 1));
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: addr,
                    expr: RExpr::Binary {
                        op: BinOp::Arith(ArithBinOp::Add),
                        lhs: ptr,
                        rhs: offset,
                    },
                },
            );
            let word = self.alloc_runtime_temp(
                TyId::u256(self.db),
                RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: word,
                    expr: RExpr::ConstScalar(ConstScalar::Int {
                        bits: 256,
                        signed: false,
                        words: padded_word_bytes(chunk),
                    }),
                },
            );
            self.push_ignored_builtin(
                bb,
                crate::runtime::RuntimeBuiltin::Mstore { addr, value: word },
            );
        }
        dst
    }

    fn malloc_bytes(&mut self, bb: RBlockId, size: RLocalId) -> (RLocalId, RLocalId) {
        let ptr_ty = TyId::ptr_to(self.db, TyId::u8(self.db));
        let ptr_class = self
            .top_level_class_for_ty(ptr_ty, AddressSpaceKind::Memory)
            .expect("u8 pointer should have a runtime class");
        let RuntimeClass::RawAddr { .. } = ptr_class.clone() else {
            panic!("u8 pointer should lower as a raw memory address");
        };
        let raw_ptr = self.alloc_runtime_temp(ptr_ty, RuntimeCarrier::Value(ptr_class));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: raw_ptr,
                expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::Malloc { size }),
            },
        );
        let ptr = self.alloc_runtime_temp(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: ptr,
                expr: RExpr::Cast {
                    value: raw_ptr,
                    to: word_scalar_class(),
                },
            },
        );
        (raw_ptr, ptr)
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
        let (_, ptr) = self.malloc_bytes(bb, allocated_len);

        for (idx, chunk) in payload.chunks(32).enumerate() {
            let addr = if idx == 0 {
                ptr
            } else {
                let addr = self.alloc_runtime_temp(
                    TyId::u256(self.db),
                    RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
                );
                let offset = self.alloc_u256_const(bb, idx * 32);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: addr,
                        expr: RExpr::Binary {
                            op: BinOp::Arith(ArithBinOp::Add),
                            lhs: ptr,
                            rhs: offset,
                        },
                    },
                );
                addr
            };
            let word = self.alloc_runtime_temp(
                TyId::u256(self.db),
                RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: word,
                    expr: RExpr::ConstScalar(ConstScalar::Int {
                        bits: 256,
                        signed: false,
                        words: padded_word_bytes(chunk),
                    }),
                },
            );
            self.push_ignored_builtin(
                bb,
                crate::runtime::RuntimeBuiltin::Mstore { addr, value: word },
            );
        }

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

    fn known_const_ref_region(&self, value: SemConstId<'db>) -> Option<ConstRegionId<'db>> {
        let region = aggregate_const_ref_region(self.db, self.env, value)?;
        self.const_ref_regions.contains(&region).then_some(region)
    }

    fn lower_sem_const_scalar(
        &mut self,
        bb: RBlockId,
        ty: TyId<'db>,
        scalar: ConstScalar,
    ) -> RLocalId {
        let class = self
            .top_level_class_for_ty(ty, AddressSpaceKind::Memory)
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
        bindings: &[LayoutEvidenceConstBinding<'db>],
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
                    .map(|(field, field_ty)| {
                        self.lower_const_value_field(bb, field, field_ty, bindings)
                    })
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
                        self.lower_sem_const_as_class(bb, field, field_ty, field_class, bindings)
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
                self.lower_enum_values(bb, dst, layout, variant, &field_values);
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
        let field_tys = if ty.is_array(self.db) {
            let (_, args) = ty.decompose_ty_app(self.db);
            let elem_ty = args.first().copied().expect("array element type");
            vec![elem_ty; arity]
        } else {
            ty.field_types(self.db)
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

    fn lower_enum_make(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: &[NOperand],
    ) {
        let enum_ = enum_ty
            .as_enum(self.db)
            .unwrap_or_else(|| panic!("enum construction reached non-enum type"));
        let args = enum_ty.generic_args(self.db);
        let Some(enum_variant) = enum_.variants(self.db).nth(variant.0 as usize) else {
            panic!("missing enum variant for {variant:?}");
        };
        let field_tys = enum_variant
            .field_tys(self.db)
            .into_iter()
            .map(|field| field.instantiate(self.db, args))
            .collect::<Vec<_>>();
        assert_eq!(
            field_tys.len(),
            fields.len(),
            "enum constructor arity mismatch for {}::{variant:?}",
            enum_ty.pretty_print(self.db),
        );
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
        if matches!(
            stored,
            RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            }
        ) {
            let value = self.lower_semantic_operand_for_class(bb, field, &stored);
            return (value, stored);
        }
        let value = match boundary_spec_for_ty_in_env(
            self.db,
            self.env,
            field_ty,
            AddressSpaceKind::Memory,
        ) {
            Some(boundary) => self.lower_semantic_operand_for_boundary(bb, field, &boundary),
            None => {
                let class = self
                    .top_level_class_for_ty(field_ty, AddressSpaceKind::Memory)
                    .unwrap_or_else(|| {
                        panic!(
                            "non-zst constructor field should have a runtime class: owner={:?} field_ty={} stored={stored:#?}",
                            self.semantic_body.owner().key(self.db).owner(self.db),
                            field_ty.pretty_print(self.db),
                        )
                    });
                self.lower_semantic_operand_for_class(bb, field, &class)
            }
        };
        let class = self.value_class(value).cloned().unwrap_or(stored);
        (value, class)
    }

    fn lower_const_value_field(
        &mut self,
        bb: RBlockId,
        field: SemConstId<'db>,
        field_ty: TyId<'db>,
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) -> (RLocalId, RuntimeClass<'db>) {
        let stored = stored_class_for_ty_in_env(self.db, self.env, field_ty);
        if self.class_is_runtime_zst(&stored) {
            let value = self.lower_zst_value_placeholder(bb, field_ty, stored.clone());
            return (value, stored);
        }
        let value = self.lower_sem_const_as_value(bb, field, field_ty, bindings);
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
        let place = self.lower_place(bb, place);
        self.lower_runtime_place_read_into(bb, dst, place, class);
    }

    fn lower_value_extract_place_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        place: &NPlace<'db>,
    ) -> bool {
        let base = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => {
                if self.semantic_body.normalized.values[carrier.index()]
                    .ty
                    .as_ptr(self.db)
                    .is_some()
                {
                    return false;
                }
                let Some(local) = self.semantic_body.value_local(carrier) else {
                    return false;
                };
                local
            }
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
                    NRootKind::Provider { binding } => {
                        let binding = binding.clone();
                        let value_ty = root.ty;
                        let path = place.path.iter().cloned().collect::<Vec<_>>();
                        return self.lower_provider_handle_value_extract_path_read(
                            bb, dst, &binding, value_ty, &path,
                        );
                    }
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
        let path = place.path.iter().cloned().collect::<Vec<_>>();
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

    fn lower_provider_handle_value_extract_path_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        binding: &ProviderBinding<'db>,
        value_ty: TyId<'db>,
        path: &[NDataProjection],
    ) -> bool {
        // Only representation roots reconstruct a handle from its transport.
        // Target roots load the referent through the provider's address.
        if value_ty != binding.provider_ty {
            return false;
        }
        let Some(local) = self.facts.root_provider_local(binding) else {
            return false;
        };
        let value = self.runtime_value(local);
        let Some(actual) = self.value_class(value).cloned() else {
            return false;
        };
        let value_ty = self.semantic_body.locals[local.index()].ty;
        let class = stored_class_for_ty_in_env(self.db, self.env, value_ty);
        if !actual.shares_runtime_rep_with(self.db, &class) {
            return false;
        }
        let value = self.emit_plain_runtime_coercion(bb, value, actual, &class, value_ty);
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
        if let (
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

    fn lower_enum_tag(&mut self, bb: RBlockId, dst: RLocalId, value: RuntimeOperand) {
        if self.semantic_local_is_place_bound(value.local) {
            let value = self.read_semantic_value(bb, value.local);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::EnumTagOfValue { value },
                },
            );
            return;
        }
        match self.local_class(value.local) {
            Some(RuntimeClass::Ref { .. }) => {
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumGetTag {
                            root: self.runtime_value(value.local),
                        },
                    },
                );
            }
            _ => {
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumTagOfValue {
                            value: self.runtime_value(value.local),
                        },
                    },
                );
            }
        }
    }

    fn lower_is_enum_variant(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: RuntimeOperand,
        variant: VariantIndex,
    ) {
        if self.semantic_local_is_place_bound(value.local) {
            let variant = self.enum_variant_for_local(value.local, variant);
            let value = self.read_semantic_value(bb, value.local);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::EnumIsVariant { value, variant },
                },
            );
            return;
        }
        match self.local_class(value.local) {
            Some(RuntimeClass::Ref { .. }) => {
                let enum_layout = self.enum_layout_for_local(value.local);
                let tag_class = RuntimeClass::Scalar(ScalarClass {
                    repr: match enum_layout.data(self.db) {
                        crate::runtime::Layout::Enum(layout) => layout.tag.repr,
                        _ => unreachable!(),
                    },
                    role: ScalarRole::EnumTag { enum_layout },
                });
                let tag = self.alloc_runtime_temp(
                    self.locals[value.local.index()].semantic_ty,
                    RuntimeCarrier::Value(tag_class),
                );
                self.lower_enum_tag(bb, tag, value);
                let expected = self.alloc_runtime_temp(
                    self.locals[value.local.index()].semantic_ty,
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
            }
            _ => {
                let variant = self.enum_variant_for_local(value.local, variant);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumIsVariant {
                            value: self.runtime_value(value.local),
                            variant,
                        },
                    },
                );
            }
        }
    }

    fn current_arithmetic_mode(&self) -> ArithmeticMode {
        self.current_semantic_key()
            .owner(self.db)
            .arithmetic_mode(self.db)
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
            UnOp::Plus | UnOp::Mut | UnOp::Ref | UnOp::Deref => return None,
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
                    class: pointee.as_ref().clone(),
                },
                path: Box::default(),
            }),
            RuntimeClass::RawAddr { pointee: None, .. } => None,
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => None,
        }
    }

    fn lower_core_primitive_wrapper_call(
        &mut self,
        bb: RBlockId,
        semantic: SemanticInstance<'db>,
        args: &[NOperand],
    ) -> Option<RLocalId> {
        let BodyOwner::Func(func) = semantic.key(self.db).owner(self.db) else {
            return None;
        };
        let ret_ty = semantic_return_ty(self.db, semantic);
        let kind = core_primitive_wrapper_call_kind(self.db, func, ret_ty)?;
        let checked = self.current_arithmetic_mode() == ArithmeticMode::Checked;
        let mut boundary_sites = BoundarySiteAllocator::default();
        let call_input_plan = compile_call_input_plan_for_semantic(
            self.db,
            &self.semantic_body,
            semantic,
            self.env,
            &[],
            &mut boundary_sites,
        );
        let (runtime_args, _) = self.lower_visible_call_args(bb, args, &call_input_plan);

        match kind {
            PrimitiveWrapperCallKind::Unary(op) => {
                let [value] = runtime_args.as_slice() else {
                    return None;
                };
                let RuntimeClass::Scalar(class) =
                    self.top_level_class_for_ty(ret_ty, AddressSpaceKind::Memory)?
                else {
                    return None;
                };
                let ret = self.alloc_runtime_temp(
                    ret_ty,
                    RuntimeCarrier::Value(RuntimeClass::Scalar(class.clone())),
                );
                let expr =
                    self.lower_unary_expr_for_mode(bb, op, checked, *value, ret_ty, &class)?;
                self.push_stmt(bb, RStmt::Assign { dst: ret, expr });
                Some(ret)
            }
            PrimitiveWrapperCallKind::Binary(op) => {
                let [lhs, rhs] = runtime_args.as_slice() else {
                    return None;
                };
                let ret_class = self.top_level_class_for_ty(ret_ty, AddressSpaceKind::Memory)?;
                if matches!(op, BinOp::Comp(_))
                    && runtime_args.iter().any(|arg| {
                        matches!(self.value_class(*arg), Some(RuntimeClass::RawAddr { .. }))
                    })
                {
                    let word = RuntimeClass::Scalar(word_scalar_class());
                    let lhs = self.coerce_value(bb, *lhs, &word);
                    let rhs = self.coerce_value(bb, *rhs, &word);
                    let ret =
                        self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(ret_class.clone()));
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst: ret,
                            expr: RExpr::Binary { op, lhs, rhs },
                        },
                    );
                    return Some(ret);
                }
                if let BinOp::Arith(op @ (ArithBinOp::Add | ArithBinOp::Sub)) = op
                    && runtime_args.iter().any(|arg| {
                        matches!(self.value_class(*arg), Some(RuntimeClass::RawAddr { .. }))
                    })
                {
                    let word = RuntimeClass::Scalar(word_scalar_class());
                    let lhs = self.coerce_value(bb, *lhs, &word);
                    let rhs = self.coerce_value(bb, *rhs, &word);
                    let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(word.clone()));
                    let RuntimeClass::Scalar(class) = &word else {
                        unreachable!("word runtime class should be scalar")
                    };
                    let expr = self.lower_arith_expr_for_mode(bb, op, checked, lhs, rhs, class)?;
                    self.push_stmt(bb, RStmt::Assign { dst: ret, expr });
                    return Some(if ret_class == word {
                        ret
                    } else {
                        self.coerce_value(bb, ret, &ret_class)
                    });
                }
                let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(ret_class.clone()));
                let expr = match op {
                    BinOp::Arith(op) => {
                        let RuntimeClass::Scalar(class) = &ret_class else {
                            return None;
                        };
                        self.lower_arith_expr_for_mode(bb, op, checked, *lhs, *rhs, class)?
                    }
                    BinOp::Comp(_) | BinOp::Logical(_) => RExpr::Binary {
                        op,
                        lhs: *lhs,
                        rhs: *rhs,
                    },
                    BinOp::Index => return None,
                };
                self.push_stmt(bb, RStmt::Assign { dst: ret, expr });
                Some(ret)
            }
            PrimitiveWrapperCallKind::Assign(op) => {
                let [dst_addr, rhs] = runtime_args.as_slice() else {
                    return None;
                };
                let place = self.runtime_place_from_addr_value(*dst_addr)?;
                let target = self.project_place_class(&place);
                let target_ty = self.semantic_operand_ty(args[0]);
                let lhs = self.alloc_runtime_temp(target_ty, RuntimeCarrier::Value(target.clone()));
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: lhs,
                        expr: RExpr::Load {
                            place: place.clone(),
                        },
                    },
                );
                let result = match target.clone() {
                    RuntimeClass::Scalar(class) => {
                        let result = self
                            .alloc_runtime_temp(target_ty, RuntimeCarrier::Value(target.clone()));
                        let expr = match op {
                            BinOp::Arith(op) => {
                                self.lower_arith_expr_for_mode(bb, op, checked, lhs, *rhs, &class)?
                            }
                            BinOp::Comp(_) | BinOp::Logical(_) | BinOp::Index => return None,
                        };
                        self.push_stmt(bb, RStmt::Assign { dst: result, expr });
                        result
                    }
                    RuntimeClass::RawAddr { .. } => {
                        let BinOp::Arith(op @ (ArithBinOp::Add | ArithBinOp::Sub)) = op else {
                            return None;
                        };
                        let word = RuntimeClass::Scalar(word_scalar_class());
                        let lhs = self.coerce_value(bb, lhs, &word);
                        let rhs = self.coerce_value(bb, *rhs, &word);
                        let result_word =
                            self.alloc_runtime_temp(target_ty, RuntimeCarrier::Value(word.clone()));
                        let RuntimeClass::Scalar(class) = &word else {
                            unreachable!("word runtime class should be scalar")
                        };
                        let expr =
                            self.lower_arith_expr_for_mode(bb, op, checked, lhs, rhs, class)?;
                        self.push_stmt(
                            bb,
                            RStmt::Assign {
                                dst: result_word,
                                expr,
                            },
                        );
                        self.coerce_value(bb, result_word, &target)
                    }
                    RuntimeClass::Ref { .. } | RuntimeClass::AggregateValue { .. } => return None,
                };
                self.write_value_to_place(bb, place, result, &target);
                Some(self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased))
            }
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
        let layout_statement = self
            .layout_evidence
            .statement(stmt_id)
            .expect("verified layout evidence must contain every normalized statement")
            .clone();
        let layout_call = layout_statement.call.as_ref();
        for assignment in &layout_statement.assignments {
            if !matches!(assignment.expr, LayoutEvidenceExpr::CallResult { .. }) {
                self.lower_layout_evidence_assignment(bb, assignment);
            }
        }
        let semantic = get_or_build_semantic_instance(self.db, callee.key);
        if layout_call.is_none() {
            if let Some(ret) = self.lower_core_primitive_wrapper_call(bb, semantic, args) {
                return ret;
            }
            if let Some(ret) = self.lower_extern_builtin_call(bb, semantic, args, effect_args) {
                return ret;
            }
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
        let (mut runtime_args, runtime_classes) =
            self.lower_runtime_call_inputs(bb, args, effect_args, &call_input_plan);
        let runtime_key = RuntimeInstanceKey::new(
            self.db,
            crate::instance::RuntimeInstanceSource::Semantic(semantic),
            runtime_classes,
        );
        let abi = runtime_declaration_abi_plan(self.db, runtime_key);
        let callee_env = RuntimeTypeEnv::for_semantic(self.db, semantic);
        let layout_args = layout_call.map_or(&[][..], |call| call.args.as_ref());
        assert_eq!(
            abi.evidence_params.len(),
            layout_args.len(),
            "runtime layout-evidence argument count must match the canonical ABI",
        );
        for (param, arg) in abi.evidence_params.iter().zip(layout_args) {
            assert_eq!(arg.target, param.source);
            let map = runtime_layout_map_for_map_ty(self.db, callee_env, &param.map_ty);
            assert_eq!(map.class(), param.param.class);
            runtime_args.push(self.lower_layout_expr(bb, &map, &arg.value));
        }
        let callee = get_or_build_runtime_instance(self.db, runtime_key);
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
        let Some(call_class) = abi.returns.class.clone() else {
            let ret = self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: ret,
                    expr: RExpr::Call {
                        callee,
                        args: runtime_args.into_boxed_slice(),
                    },
                },
            );
            return ret;
        };
        let call_result = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(call_class));
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
        let Some(envelope_layout) = abi.returns.layout else {
            return call_result;
        };
        let mut field = 0u32;
        let visible_result = abi.returns.visible.clone().map(|class| {
            let result = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(class));
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: result,
                    expr: RExpr::AggregateExtract {
                        value: call_result,
                        index: field,
                    },
                },
            );
            field += 1;
            result
        });
        for result in &abi.returns.evidence {
            let assignment = layout_statement.assignments.iter().find(|assignment| {
                matches!(
                    assignment.expr,
                    LayoutEvidenceExpr::CallResult { component }
                        if component == result.component_id
                )
            });
            if let Some(assignment) = assignment {
                assert_eq!(
                    self.layout_evidence.locals[assignment.dst.index()].map_ty,
                    result.map_ty,
                );
                let extracted = self.alloc_runtime_temp(
                    self.layout_evidence.locals[assignment.dst.index()]
                        .map_ty
                        .scalar_ty,
                    RuntimeCarrier::Value(result.class.clone()),
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: extracted,
                        expr: RExpr::AggregateExtract {
                            value: call_result,
                            index: field,
                        },
                    },
                );
                let dst = self.layout_evidence_runtime_local(assignment.dst);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(extracted),
                    },
                );
            }
            field += 1;
        }
        let _ = envelope_layout;
        debug_assert_eq!(
            field as usize,
            abi.returns.evidence.len() + usize::from(visible_result.is_some())
        );
        visible_result
            .unwrap_or_else(|| self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased))
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
        for (index, len) in data_path_index_bounds(self.db, ty, path) {
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
        let BodyOwner::Func(func) = semantic.key(self.db).owner(self.db) else {
            return None;
        };

        if let Some(ret) = self.lower_intrinsic_keccak256_call(bb, func, args) {
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
                let class = self.top_level_class_for_ty(ret_ty, AddressSpaceKind::Memory)?;
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
            get_or_build_semantic_instance(self.db, semantic_key),
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
        let BodyOwner::Func(func) = semantic.key(self.db).owner(self.db) else {
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
        let ret_class = self.top_level_class_for_ty(ret_ty, AddressSpaceKind::Memory)?;
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
            "neg" => {
                let [value] = args else { return None };
                RExpr::Unary {
                    op: UnOp::Minus,
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
        let BodyOwner::Func(func) = semantic.key(self.db).owner(self.db) else {
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
            RuntimeBuiltinFuncKind::Malloc => {
                let [size] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Malloc { size: *size },
                    self.top_level_class_for_ty(
                        semantic_return_ty(self.db, semantic),
                        AddressSpaceKind::Memory,
                    ),
                )
            }
            RuntimeBuiltinFuncKind::NativePtrIsNull => {
                let [ptr] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::NativePtrIsNull { ptr: *ptr },
                    self.top_level_class_for_ty(
                        semantic_return_ty(self.db, semantic),
                        AddressSpaceKind::Memory,
                    ),
                )
            }
            RuntimeBuiltinFuncKind::PtrOffsetBytes => {
                let [ptr, offset] = args else { return None };
                let RuntimeClass::RawAddr { space, pointee } = self.value_class(*ptr)? else {
                    return None;
                };
                builtin(
                    crate::runtime::RuntimeBuiltin::PtrOffsetBytes {
                        ptr: *ptr,
                        offset: *offset,
                    },
                    Some(RuntimeClass::RawAddr {
                        space: *space,
                        pointee: pointee.clone(),
                    }),
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
            RuntimeBuiltinFuncKind::Sload => {
                let [slot] = args else { return None };
                builtin(
                    crate::runtime::RuntimeBuiltin::Sload { slot: *slot },
                    Some(word.clone()),
                )
            }
            RuntimeBuiltinFuncKind::Sstore => {
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
            RuntimeBuiltinFuncKind::StaticCall => {
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
            // Lowered with its payload in `lower_panic_code`.
            RuntimeBuiltinFuncKind::PanicCode => return None,
            RuntimeBuiltinFuncKind::IntrinsicKeccak256 => return None,
        })
    }

    fn lower_intrinsic_keccak256_call(
        &mut self,
        bb: RBlockId,
        func: Func<'db>,
        args: &[NOperand],
    ) -> Option<RLocalId> {
        if runtime_builtin_func_kind(self.db, func)
            != Some(RuntimeBuiltinFuncKind::IntrinsicKeccak256)
        {
            return None;
        }

        let [bytes] = args else {
            return None;
        };
        let bytes_ty = self.semantic_body.normalized.value(bytes.value)?.ty;
        let layout = self.layout_for_ty(bytes_ty);
        let crate::runtime::Layout::Array(array_layout) = layout.data(self.db) else {
            panic!(
                "__keccak256 expects a byte-array argument, found {}",
                bytes_ty.pretty_print(self.db)
            );
        };

        let word_class = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let len = self.alloc_runtime_temp(
            TyId::u256(self.db),
            RuntimeCarrier::Value(word_class.clone()),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: len,
                expr: RExpr::ConstScalar(ConstScalar::Int {
                    bits: 256,
                    signed: false,
                    words: if array_layout.len == 0 {
                        Vec::new()
                    } else {
                        let bytes = array_layout.len.to_be_bytes();
                        bytes
                            .into_iter()
                            .skip_while(|byte| *byte == 0)
                            .collect::<Vec<_>>()
                    },
                }),
            },
        );

        let value = self.read_semantic_operand(bb, *bytes);
        let provider_class = provider_class_for_target_in_env(
            self.db,
            self.env,
            Some(bytes_ty),
            AddressSpaceKind::Memory,
        );
        let provider = match self.value_class(value) {
            Some(
                RuntimeClass::Ref {
                    kind:
                        RefKind::Provider {
                            space: AddressSpaceKind::Memory,
                            ..
                        },
                    ..
                }
                | RuntimeClass::RawAddr {
                    space: AddressSpaceKind::Memory,
                    ..
                },
            ) => value,
            Some(_) => self.coerce_value(bb, value, &provider_class),
            None => panic!(
                "__keccak256 argument should have a runtime class: key={:?}; local={bytes:?}",
                self.key
            ),
        };
        let offset = self.coerce_value(
            bb,
            provider,
            &RuntimeClass::raw_addr(
                AddressSpaceKind::Memory,
                RuntimeClass::AggregateValue { layout },
            ),
        );
        let ret = self.alloc_runtime_temp(TyId::u256(self.db), RuntimeCarrier::Value(word_class));
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
            NTerminatorKind::Return(value) => {
                let semantic = self
                    .key
                    .semantic(self.db)
                    .expect("runtime body must have a semantic owner");
                let returns = self.abi.returns.clone();
                let Some(layout) = returns.layout else {
                    return RTerminator::Return(match returns.visible {
                        Some(class) => value
                            .map(|value| self.lower_semantic_operand_for_class(bb, value, &class)),
                        None => None,
                    });
                };
                let mut fields = Vec::with_capacity(
                    returns.evidence.len() + usize::from(returns.visible.is_some()),
                );
                if let Some(class) = &returns.visible {
                    fields.push(
                        value
                            .map(|value| self.lower_semantic_operand_for_class(bb, value, class))
                            .expect("visible runtime return must have a semantic value"),
                    );
                }
                let operands = self
                    .layout_evidence
                    .terminator(SBlockId::from_u32(bb.as_u32()))
                    .expect("verified layout evidence must contain every semantic terminator")
                    .returns
                    .clone();
                assert_eq!(operands.len(), returns.evidence.len());
                for (result, returned) in returns.evidence.iter().zip(operands) {
                    assert_eq!(returned.component, result.component_id);
                    let map = runtime_layout_map_for_map_ty(self.db, self.env, &result.map_ty);
                    assert_eq!(map.class(), result.class);
                    fields.push(self.lower_layout_operand(bb, &map, &returned.value));
                }
                let result = self.alloc_runtime_temp(
                    semantic_return_ty(self.db, semantic),
                    RuntimeCarrier::Value(RuntimeClass::AggregateValue { layout }),
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: result,
                        expr: RExpr::AggregateMake {
                            layout,
                            fields: fields.into_boxed_slice(),
                        },
                    },
                );
                RTerminator::Return(Some(result))
            }
        }
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
        let edge = RBlockId::from_u32(self.blocks.len() as u32);
        self.blocks.push(RBlock {
            stmts: Vec::new(),
            terminator: RTerminator::Goto(target),
        });
        self.terminated_blocks.push(false);
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
        if self.value_class(src).is_none()
            && let Some(value) =
                self.lower_erased_effect_handle_coercion(bb, src, target, semantic_ty)
        {
            return value;
        }
        let source = self
            .value_class(src)
            .cloned()
            .unwrap_or_else(|| panic!("cannot coerce erased value {src:?} to {target:?}"));
        if let Some(value) =
            self.lower_effect_handle_coercion(bb, src, &source, target, semantic_ty)
        {
            return value;
        }
        self.emit_plain_runtime_coercion(bb, src, source, target, semantic_ty)
    }

    fn lower_erased_effect_handle_coercion(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
        handle_ty: TyId<'db>,
    ) -> Option<RLocalId> {
        let transport = effect_handle_transport_class_for_ty_in_env(self.db, self.env, handle_ty)?;
        let ordinary = stored_class_for_ty_in_env(self.db, self.env, handle_ty);
        if target != &transport || !ordinary.is_zero_sized(self.db) {
            return None;
        }
        Some(self.lower_effect_handle_to_transport(
            bb,
            src,
            target,
            handle_ty,
            self.semantic_source_local_for_runtime_value(src),
        ))
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

    fn lower_effect_handle_coercion(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        source: &RuntimeClass<'db>,
        target: &RuntimeClass<'db>,
        handle_ty: TyId<'db>,
    ) -> Option<RLocalId> {
        let transport = effect_handle_transport_class_for_ty_in_env(self.db, self.env, handle_ty)?;
        let ordinary = stored_class_for_ty_in_env(self.db, self.env, handle_ty);
        let source_local = self.semantic_source_local_for_runtime_value(src);
        if target == &transport
            && source
                .aggregate_value_class()
                .is_some_and(|source| source.shares_runtime_rep_with(self.db, &ordinary))
        {
            let source =
                self.emit_plain_runtime_coercion(bb, src, source.clone(), &ordinary, handle_ty);
            return Some(self.lower_effect_handle_to_transport(
                bb,
                source,
                target,
                handle_ty,
                source_local,
            ));
        }

        None
    }

    fn lower_effect_handle_to_transport(
        &mut self,
        bb: RBlockId,
        handle: RLocalId,
        target: &RuntimeClass<'db>,
        handle_ty: TyId<'db>,
        layout_source: Option<SLocalId>,
    ) -> RLocalId {
        let raw = self.lower_effect_handle_raw_call(bb, handle_ty, handle, layout_source);
        let raw_class = self
            .value_class(raw)
            .cloned()
            .expect("EffectHandle::raw must return a runtime value");
        let raw_ty = self.locals[raw.index()].semantic_ty;
        self.emit_plain_runtime_coercion(bb, raw, raw_class, target, raw_ty)
    }

    fn semantic_source_local_for_runtime_value(&self, value: RLocalId) -> Option<SLocalId> {
        if value.index() < self.semantic_body.source.locals.len() {
            return Some(SLocalId::from_u32(value.as_u32()));
        }
        self.normalized_value_temps
            .iter()
            .enumerate()
            .find_map(|(index, temp)| {
                (*temp == Some(value)).then(|| {
                    self.semantic_body
                        .layout_plan
                        .value_source(NValueId::from_u32(index as u32))
                })
            })?
    }

    fn lower_effect_handle_raw_call(
        &mut self,
        bb: RBlockId,
        handle_ty: TyId<'db>,
        arg: RLocalId,
        layout_source: Option<SLocalId>,
    ) -> RLocalId {
        let semantic = self.resolve_effect_handle_raw_method(handle_ty);
        let visible_bindings = runtime_visible_binding_plans(self.db, semantic);
        let (mut args, params) = match visible_bindings.as_slice() {
            [] => (Vec::new(), Vec::new()),
            [_] => {
                let class = self
                    .value_class(arg)
                    .cloned()
                    .expect("EffectHandle method argument must have a runtime class");
                (vec![arg], vec![class])
            }
            _ => panic!(
                "EffectHandle::raw must have exactly one semantic argument and at most one runtime argument"
            ),
        };
        let callee_key = RuntimeInstanceKey::new(
            self.db,
            crate::instance::RuntimeInstanceSource::Semantic(semantic),
            params,
        );
        let callee = get_or_build_runtime_instance(self.db, callee_key);
        let abi = runtime_declaration_abi_plan(self.db, callee_key);
        if !abi.evidence_params.is_empty() {
            let source = layout_source.unwrap_or_else(|| {
                panic!(
                    "EffectHandle::raw requires runtime layout evidence without a semantic source"
                )
            });
            args.extend(self.lower_layout_value_args(bb, source, semantic, &abi));
        }
        let signature = abi.signature();
        assert_eq!(
            args.len(),
            signature.params.len(),
            "EffectHandle::raw runtime argument count mismatch"
        );
        let ret_ty = semantic_return_ty(self.db, semantic);
        let Some(call_class) = abi.returns.class.clone() else {
            let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Erased);
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: ret,
                    expr: RExpr::Call {
                        callee,
                        args: args.into_boxed_slice(),
                    },
                },
            );
            return ret;
        };
        let call_result = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(call_class));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: call_result,
                expr: RExpr::Call {
                    callee,
                    args: args.into_boxed_slice(),
                },
            },
        );
        let Some(_) = abi.returns.layout else {
            return call_result;
        };
        let Some(visible) = abi.returns.visible else {
            return self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Erased);
        };
        let result = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(visible));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: result,
                expr: RExpr::AggregateExtract {
                    value: call_result,
                    index: 0,
                },
            },
        );
        result
    }

    fn resolve_effect_handle_raw_method(&self, handle_ty: TyId<'db>) -> SemanticInstance<'db> {
        let scope = self
            .env
            .scope
            .or_else(|| handle_ty.as_scope(self.db))
            .expect("EffectHandle method resolution requires a scope");
        let impl_instance =
            runtime_effect_handle_info(self.db, handle_ty, Some(scope), self.env.assumptions)
                .unwrap_or_else(|| {
                    panic!(
                        "failed to resolve EffectHandle implementation for {}",
                        handle_ty.pretty_print(self.db),
                    )
                })
                .impl_instance;
        let trait_inst = impl_instance.trait_inst();
        let func = trait_inst
            .def(self.db)
            .method_defs(self.db)
            .get(&IdentId::new(self.db, "raw".to_string()))
            .copied()
            .expect("EffectHandle declares raw");
        let key = generated_callee_key(
            self.db,
            scope,
            self.env.assumptions,
            func,
            Some(trait_inst),
            &[],
        )
        .unwrap_or_else(|error| panic!("{}", generated_call_error(self.db, func, error)));
        get_or_build_semantic_instance(self.db, key)
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
            BodyOwner::Func(func) => func
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
            RuntimeLocalLowering::DirectValue
                | RuntimeLocalLowering::PlaceCarrier { .. }
                | RuntimeLocalLowering::DirectCarrier { .. }
        )
    }

    fn semantic_local_is_place_bound(&self, local: SLocalId) -> bool {
        matches!(
            self.semantic_local_lowering(local),
            RuntimeLocalLowering::PlaceCarrier { .. }
                | RuntimeLocalLowering::PlaceBoundValue { .. }
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
        if effect_handle_transport_class_for_ty_in_env(
            self.db, self.env, self.semantic_body.locals[local.index()].ty,
        ).is_some()
            || self.semantic_body.normalized.roots.iter().any(|root| {
                matches!(root.kind, NRootKind::CapabilityRepresentation { carrier } if carrier == value)
            })
        {
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
            let semantic = get_or_build_semantic_instance(self.db, callee.key);
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
                        | RuntimeArgSource::HandleLikeValue(source)
                        | RuntimeArgSource::SemanticPlaceAddress(source, _) => *source == local,
                        RuntimeArgSource::PlaceAddress(place, _)
                        | RuntimeArgSource::PlaceValue(place, _)
                        | RuntimeArgSource::ValueExtract { place, .. } => {
                            matches!(place.base, NPlaceBase::CapabilityTarget { carrier } if carrier == value)
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
            | NExpr::MakeHandle { .. }
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
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::DirectCarrier { .. } => {
                self.local_class(local).cloned()
            }
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
            }
            if idx + 1 < place.path.len()
                && let Some(target) = current.deref_target()
            {
                projected.push(PlaceElem::Deref);
                current = target;
            }
        }
        runtime_place.path = projected.into_boxed_slice();
        Some(runtime_place)
    }

    fn read_semantic_value(&mut self, bb: RBlockId, local: SLocalId) -> RLocalId {
        match self.semantic_local_lowering(local) {
            RuntimeLocalLowering::Erased => self.runtime_value(local),
            RuntimeLocalLowering::DirectValue => self.runtime_value(local),
            RuntimeLocalLowering::DirectCarrier { .. } => self.runtime_value(local),
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
                    class: pointee.as_ref().clone(),
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

    fn lower_semantic_operand_for_boundary(
        &mut self,
        bb: RBlockId,
        operand: NOperand,
        boundary: &crate::runtime::RuntimeBoundarySpec<'db>,
    ) -> RLocalId {
        let runtime_operand = self.runtime_operand(operand);
        let roots = self.runtime_roots();
        let value_classes = self.normalized_value_classes();
        let selected = self.with_current_body_cx(|cx| {
            RuntimeArgSelector::new(cx.env, cx.carriers, None)
                .with_concrete_roots(&roots)
                .with_concrete_value_classes(&value_classes)
                .selected_semantic_operand_for_boundary(runtime_operand, boundary)
        });
        self.lower_selected_runtime_arg(bb, operand, &selected)
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

    fn handle_like_semantic_value(&self, local: SLocalId) -> Option<RLocalId> {
        let value = match self.semantic_local_lowering(local) {
            RuntimeLocalLowering::Erased => None,
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::PlaceCarrier { .. } => {
                Some(self.runtime_value(local))
            }
            RuntimeLocalLowering::PlaceBoundValue { provider, .. } => {
                provider.map(|provider| self.provider_binding_value(provider))
            }
            RuntimeLocalLowering::DirectCarrier { .. } => Some(self.runtime_value(local)),
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
            RuntimeLocalLowering::DirectCarrier { .. } => {
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
                BodyOwner::Func(func) => func
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

    fn runtime_layout_root_value_for_const(
        &mut self,
        bb: RBlockId,
        value: SemConstId<'db>,
        bindings: &[LayoutEvidenceConstBinding<'db>],
    ) -> Option<RLocalId> {
        let binding = formal_runtime_layout_root_binding(self.db, value, bindings)?;
        let map_ty = match &binding.value {
            LayoutEvidenceOperand::Local(local) => {
                &self.layout_evidence.locals[local.index()].map_ty
            }
            LayoutEvidenceOperand::Constant(value) => &value.map_ty,
        };
        let map = runtime_layout_map_for_map_ty(self.db, self.env, map_ty);
        if map.rank() != 0 {
            panic!("ranked layout map cannot bind a scalar const: {map_ty:?}");
        }
        let value = self.lower_layout_operand(bb, &map, &binding.value);
        Some(value)
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
            (RuntimeArgSource::HandleLikeValue(local), use_plan) => {
                let value = self
                    .emitter
                    .handle_like_semantic_value(*local)
                    .unwrap_or_else(|| self.emitter.read_semantic_value(self.bb, *local));
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::PlaceAddress(place, semantic_ty), use_plan) => {
                let place = self.emitter.lower_place(self.bb, place);
                let value = self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                );
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
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::{
        semantic::{get_or_build_semantic_instance, identity_semantic_instance_key},
        ty::ty_check::BodyOwner,
    };
    use url::Url;

    use crate::{build_test_runtime_package, runtime::package::runtime_instance_for_semantic};

    #[test]
    fn mixed_compile_time_and_runtime_layout_components_lower_through_calls() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(
            std::env::temp_dir().join("mixed_layout_component_runtime_lowering.fe"),
        )
        .expect("fixture path should be absolute");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
use std::evm::StorageMap

struct Mixed<const ROOT: u256> {
    fixed: StorageMap<u256, u256, 7>,
    dynamic: StorageMap<u256, u256, ROOT>,
}

fn pass<const ROOT: u256>(value: Mixed<ROOT>) -> Mixed<ROOT> {
    value
}

fn forward<const ROOT: u256>(value: Mixed<ROOT>) -> Mixed<ROOT> {
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
            let semantic = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func)),
            );
            let _ = runtime_instance_for_semantic(&db, semantic).body(&db);
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
