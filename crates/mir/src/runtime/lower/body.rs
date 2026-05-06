use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        EffectProviderSubst, FieldIndex, GenericSubst, ImplEnv, SBlockId, SConst, SLocalId,
        SemConstId, SemConstScalar, SemConstValue, SemanticCalleeRef, SemanticCodeRegionRef,
        SemanticInstance, SemanticInstanceKey, SemanticLocalKind, VariantIndex,
        borrowck::{
            NBorrowRoot, NBorrowRootId, NEffectArg, NExpr, NLocalOrigin, NOperand, NSPlace,
            NSPlaceRoot, NSStmt, NSStmtKind, NSTerminator, NSTerminatorKind,
            NormalizedBindingLowering, NormalizedSemanticBody, normalize_semantic_body,
        },
        get_or_build_semantic_instance, reify_runtime_const_for_ty, sem_const_ty,
    },
    ty::{
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
        ty_def::TyId,
    },
};
use hir::hir_def::{
    ArithBinOp, BinOp, CompBinOp, Func, UnOp, attr::ArithmeticMode, scope_graph::ScopeId,
};
use hir::projection::{IndexSource, Projection};

use crate::{
    db::MirDb,
    instance::{RuntimeInstance, RuntimeInstanceKey, get_or_build_runtime_instance},
    resolve_runtime_place_address_class,
    runtime::{
        AddressSpaceKind, ConstScalar, IntrinsicArithBinOp, LayoutId, PlaceElem, PlaceRoot, RBlock,
        RBlockId, RExpr, RLocal, RLocalId, RStmt, RTerminator, RefKind, RefView, RuntimeBody,
        RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeExitBehavior,
        RuntimeInterfaceSignature, RuntimeLocalLowering, RuntimeLocalRoot, RuntimePlace,
        RuntimeProviderBinding, RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole,
        VariantId,
        code_region::runtime_code_region_for_semantic_ref,
        package::{LowerError, runtime_instance_for_semantic},
    },
};

use super::{
    arg_selector::RuntimeArgSelector,
    boundary::{BoundarySiteAllocator, RuntimeValueUsePlan, boundary_spec_for_ty_in_env},
    call_input::{CompiledCallInputPlan, compile_call_input_plan_for_semantic},
    classify::{
        BodyEnv, BodyStaticFacts, ContractMetadataBuiltin, GenericNumericIntrinsicKind,
        InferClassCache, RuntimeBodyCx, actual_aggregate_class_from_runtime_source,
        contract_metadata_builtin, generic_numeric_intrinsic_kind, nonself_backing_value_place,
        resolve_runtime_call_key, semantic_return_ty, snapshot_source_place,
    },
    consts::{
        const_scalar_for_class, const_scalar_from_value, enum_tag_scalar, lower_const_region,
    },
    conversion::{RuntimeConversionEmitter, RuntimeConversionError, emit_runtime_coercion},
    infer::{InferenceResult, LocalStateInferer, merge_runtime_class},
    layout::{
        AggregateCtorElem, aggregate_ctor_elems_for_layout, layout_for_aggregate_instance_in_env,
        layout_for_enum_variant_instance_in_env, layout_for_ty_in_env,
    },
    place::{project_field_class, project_index_class, project_variant_field_class},
    realize::{
        RuntimeArgSource, RuntimeValueUseEmitter, SelectedRuntimeArg, emit_runtime_value_use_plan,
    },
    returns::runtime_return_class,
    tuple::RuntimeTupleFieldEmitter,
    type_info::{
        RuntimeTypeEnv, effect_handle_class_for_ty_in_context, provider_class_for_target_in_env,
        stored_class_for_ty_in_context, top_level_class_for_ty_in_env,
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
    let normalized_body = normalize_semantic_body(db, semantic).unwrap_or_else(|err| {
        panic!(
            "semantic normalization failed for {:?}: {err:?}",
            semantic.key(db)
        )
    });
    check_runtime_body_supported(db, semantic.key(db), &normalized_body)?;
    let facts = BodyStaticFacts::new(db, &normalized_body);
    let param_locals =
        crate::runtime::lower::interface::runtime_param_locals(db, semantic, key.params(db));
    let inferred = LocalStateInferer::new(
        BodyEnv::new(db, &normalized_body, &facts),
        key.params(db),
        &param_locals,
    )
    .run();
    let signature = instance.interface_signature(db);
    let mut emitter = RmirEmitter::new(
        db,
        instance,
        normalized_body,
        facts,
        inferred,
        signature.clone(),
    );
    emitter.lower_blocks();
    Ok(emitter.finish(signature))
}

fn check_runtime_body_supported<'db>(
    db: &'db dyn MirDb,
    key: SemanticInstanceKey<'db>,
    body: &NormalizedSemanticBody<'db>,
) -> Result<(), LowerError> {
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let NSStmtKind::Assign {
                expr:
                    NExpr::Call {
                        callee,
                        args,
                        effect_args: _,
                        ..
                    },
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

fn panic_payload_ty<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
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
    Some(body.locals[value.local.index()].ty)
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
        NExpr::ArrayRepeat { .. } => {
            panic!("array repeat with non-concrete length reached runtime lowering: {expr:?}")
        }
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
        NExpr::Use(_)
        | NExpr::CodeRegionRef { .. }
        | NExpr::ReadPlace { .. }
        | NExpr::Const(_)
        | NExpr::Unary { .. }
        | NExpr::Binary { .. }
        | NExpr::Cast { .. }
        | NExpr::AggregateMake { .. }
        | NExpr::EnumMake { .. }
        | NExpr::Borrow { .. }
        | NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::ExtractEnumField { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. } => false,
    }
}

pub(super) struct RmirEmitter<'db> {
    pub(super) db: &'db dyn MirDb,
    pub(super) instance: RuntimeInstance<'db>,
    pub(super) key: RuntimeInstanceKey<'db>,
    pub(super) semantic_body: NormalizedSemanticBody<'db>,
    pub(super) facts: BodyStaticFacts<'db>,
    pub(super) ret_class: Option<RuntimeClass<'db>>,
    pub(super) env: RuntimeTypeEnv<'db>,
    pub(super) semantic_carriers: Vec<RuntimeCarrier<'db>>,
    pub(super) semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    pub(super) provider_bindings: Vec<RuntimeProviderBinding<'db>>,
    pub(super) locals: Vec<RLocal<'db>>,
    pub(super) blocks: Vec<RBlock<'db>>,
    pub(super) terminated_blocks: Vec<bool>,
}

enum LoweredBuiltinCall<'db> {
    Expr {
        builtin: crate::runtime::RuntimeBuiltin<'db>,
        class: Option<RuntimeClass<'db>>,
    },
    Terminator(RTerminator<'db>),
}

struct LoweredSemanticLocals<'db> {
    carriers: Vec<RuntimeCarrier<'db>>,
    roots: Vec<RuntimeLocalRoot<'db>>,
    semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    provider_bindings: Vec<RuntimeProviderBinding<'db>>,
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

impl<'db> RmirEmitter<'db> {
    fn new(
        db: &'db dyn MirDb,
        instance: RuntimeInstance<'db>,
        semantic_body: NormalizedSemanticBody<'db>,
        facts: BodyStaticFacts<'db>,
        inferred: InferenceResult<'db>,
        signature: RuntimeInterfaceSignature<'db>,
    ) -> Self {
        let key = instance.key(db);
        let semantic = key
            .semantic(db)
            .expect("semantic lowering only applies to semantic runtime instances");
        let LoweredSemanticLocals {
            carriers,
            roots,
            semantic_locals,
            provider_bindings,
        } = LoweredSemanticLocals {
            carriers: inferred.carriers,
            roots: inferred.roots,
            semantic_locals: inferred.semantic_locals,
            provider_bindings: inferred.provider_bindings,
        };
        let semantic_carriers = carriers.clone();
        let env = RuntimeTypeEnv::for_semantic(db, semantic);
        let terminated_blocks = vec![false; semantic_body.blocks.len()];
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
        let blocks = Vec::with_capacity(semantic_body.blocks.len());
        Self {
            db,
            instance,
            key,
            semantic_body,
            facts,
            ret_class: signature.ret.clone(),
            env,
            semantic_carriers,
            semantic_locals,
            provider_bindings,
            locals,
            blocks,
            terminated_blocks,
        }
    }

    fn finish(mut self, signature: RuntimeInterfaceSignature<'db>) -> RuntimeBody<'db> {
        while self.blocks.len() < self.semantic_body.blocks.len() {
            self.blocks.push(RBlock {
                stmts: Vec::new(),
                terminator: RTerminator::Return(None),
            });
        }
        RuntimeBody {
            owner: self.instance,
            key: self.key,
            signature,
            semantic_locals: self.semantic_locals,
            provider_bindings: self.provider_bindings,
            locals: self.locals,
            blocks: self.blocks,
        }
    }

    fn layout_for_ty(&self, ty: TyId<'db>) -> LayoutId<'db> {
        layout_for_ty_in_env(self.db, self.env, ty)
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
        self.blocks = (0..self.semantic_body.blocks.len())
            .map(|_| RBlock {
                stmts: Vec::new(),
                terminator: RTerminator::Return(None),
            })
            .collect();
        self.terminated_blocks = vec![false; self.semantic_body.blocks.len()];
        let blocks = self.semantic_body.blocks.clone();
        for (idx, block) in blocks.iter().enumerate() {
            let bb = RBlockId::from_u32(idx as u32);
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                self.lower_stmt(bb, stmt_idx, stmt);
                if self.terminated_blocks[bb.index()] {
                    break;
                }
            }
            if !self.terminated_blocks[bb.index()] {
                self.blocks[bb.index()].terminator = self.lower_terminator(bb, &block.terminator);
            }
        }
    }

    fn set_terminator(&mut self, bb: RBlockId, terminator: RTerminator<'db>) {
        self.blocks[bb.index()].terminator = terminator;
        self.terminated_blocks[bb.index()] = true;
    }

    fn lower_stmt(&mut self, bb: RBlockId, stmt_idx: usize, stmt: &NSStmt<'db>) {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => self.lower_assign(bb, stmt_idx, *dst, expr),
            NSStmtKind::Store { dst, src } => {
                let place = self.lower_place(bb, dst);
                let target = self.project_place_class(&place);
                let value = self.read_semantic_value(bb, src.local);
                self.write_value_to_place(bb, place, value, &target);
            }
        }
    }

    fn lower_assign(&mut self, bb: RBlockId, stmt_idx: usize, dst: SLocalId, expr: &NExpr<'db>) {
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
                let sink = self.alloc_runtime_temp(
                    self.semantic_body.locals[dst.index()].ty,
                    RuntimeCarrier::Erased,
                );
                self.locals[sink.index()].carrier = carrier;
                self.lower_expr_into(bb, sink, expr);
            }
            Some(desired) => {
                if self.semantic_local_is_derived_place_bound_alias(dst) {
                    self.lower_derived_place_bound_alias_assign(dst, expr);
                    return;
                }
                if self.semantic_local_is_direct(dst) {
                    self.specialize_direct_assign_target_from_expr(dst, expr);
                    self.lower_expr_into(bb, self.runtime_value(dst), expr);
                    return;
                }
                let temp = self.alloc_runtime_temp(
                    self.locals[dst.index()].semantic_ty,
                    RuntimeCarrier::Value(desired),
                );
                self.lower_expr_into(bb, temp, expr);
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
        let mut lookup_return_class = |key| runtime_return_class(self.db, key);
        BodyEnv::new(self.db, &self.semantic_body, &self.facts).expr_direct_class(
            &self.semantic_carriers,
            block_idx,
            stmt_idx,
            expr,
            None,
            &mut lookup_return_class,
        )
    }

    fn specialize_direct_assign_target_from_expr(&mut self, dst: SLocalId, expr: &NExpr<'db>) {
        let dst_value = self.runtime_value(dst);
        let Some(target) = self.value_class(dst_value).cloned() else {
            return;
        };
        if matches!(
            self.semantic_local_lowering(dst),
            RuntimeLocalLowering::PlaceCarrier { .. }
        ) {
            let actual = self.with_current_body_cx(|cx| match expr {
                NExpr::Use(value) => cx
                    .selected_materialized_value(value.local)
                    .map(|selected| selected.class),
                NExpr::Borrow { place, .. } => cx.normalized_place_address_class(place),
                NExpr::ReadPlace { place, .. } => cx.normalized_place_class(place),
                NExpr::ArrayRepeat { .. } => {
                    panic!(
                        "array repeat with non-concrete length reached runtime lowering: {expr:?}"
                    )
                }
                NExpr::Const(_)
                | NExpr::CodeRegionRef { .. }
                | NExpr::Unary { .. }
                | NExpr::Binary { .. }
                | NExpr::Cast { .. }
                | NExpr::AggregateMake { .. }
                | NExpr::EnumMake { .. }
                | NExpr::GetEnumTag { .. }
                | NExpr::IsEnumVariant { .. }
                | NExpr::ExtractEnumField { .. }
                | NExpr::CodeRegionOffset { .. }
                | NExpr::CodeRegionLen { .. }
                | NExpr::Call { .. } => None,
            });
            if let Some(actual) = actual
                && actual.is_transport()
            {
                self.refine_local_runtime_class(dst_value, actual);
                return;
            }
        }
        if !self.runtime_local_uses_source_transport(dst_value) {
            return;
        }
        if !matches!(
            target,
            RuntimeClass::AggregateValue { .. }
                | RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                }
        ) {
            return;
        }
        let source = match expr {
            NExpr::Use(value) => {
                self.with_current_body_cx(|cx| cx.actual_aggregate_class_for_source(value.local))
            }
            _ => None,
        };
        let Some(actual) = source else {
            return;
        };
        let target = match target {
            RuntimeClass::AggregateValue { .. } => actual,
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            } => RuntimeClass::object_ref(actual.aggregate_layout().expect("aggregate ref layout")),
            RuntimeClass::Ref { .. } | RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => {
                unreachable!()
            }
        };
        self.refine_local_runtime_class(dst_value, target);
    }

    fn lower_expr_into(&mut self, bb: RBlockId, dst: RLocalId, expr: &NExpr<'db>) {
        let Some(dst_class) = self.value_class(dst).cloned() else {
            if let NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } = expr
            {
                let _ = self.lower_call(bb, *callee, args, effect_args);
            }
            return;
        };

        match expr {
            NExpr::Use(src) => {
                let dst_class = self.specialize_runtime_target_from_operand(dst, *src, &dst_class);
                let value = self.lower_semantic_operand_for_class(bb, *src, &dst_class);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(value),
                    },
                );
            }
            NExpr::ReadPlace { place, .. } => {
                if self.lower_single_field_transport_place_read(bb, dst, place) {
                    return;
                }
                let place = self.lower_place(bb, place);
                let projected = self.project_place_class(&place);
                let dst_class =
                    if self.runtime_local_uses_source_transport(dst) && projected.is_transport() {
                        self.refine_local_runtime_class(dst, projected.clone());
                        projected.clone()
                    } else {
                        dst_class
                    };
                if let (
                    RuntimeClass::AggregateValue { layout },
                    RuntimeClass::Ref {
                        pointee,
                        kind: RefKind::Object,
                        view: RefView::Whole,
                    },
                ) = (&projected, &dst_class)
                    && **pointee == (RuntimeClass::AggregateValue { layout: *layout })
                {
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::MaterializePlaceToObject { place },
                        },
                    );
                    return;
                }
                if projected == dst_class {
                    self.push_stmt(
                        bb,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::Load { place },
                        },
                    );
                    return;
                }

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
                let copied = self.coerce_value(bb, source, &dst_class);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(copied),
                    },
                );
            }
            NExpr::Const(const_) => self.lower_const_into(bb, dst, const_),
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
            NExpr::Cast { value, .. } => {
                let RuntimeClass::Scalar(to) = dst_class else {
                    panic!("casts must lower to scalar carriers");
                };
                let value = self.read_semantic_operand(bb, *value);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Cast { value, to },
                    },
                );
            }
            NExpr::ArrayRepeat { .. } => {
                panic!("array repeat with non-concrete length reached runtime lowering: {expr:?}")
            }
            NExpr::AggregateMake { ty, fields } => self.lower_aggregate_make(bb, dst, *ty, fields),
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => self.lower_enum_make(bb, dst, *enum_ty, *variant, fields),
            NExpr::Borrow { place, .. } => {
                let place = self.lower_place(bb, place);
                let actual = self.place_addr_class(&place);
                let dst_class =
                    if self.runtime_local_uses_source_transport(dst) && actual.is_transport() {
                        let target =
                            merge_runtime_class(self.db, &dst_class, &actual).unwrap_or(actual);
                        self.refine_local_runtime_class(dst, target.clone());
                        target
                    } else {
                        dst_class
                    };
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
            NExpr::GetEnumTag { value } => self.lower_enum_tag(bb, dst, *value),
            NExpr::IsEnumVariant { value, variant } => {
                self.lower_is_enum_variant(bb, dst, *value, *variant);
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                self.lower_extract_enum_field(bb, dst, *value, *variant, *field);
            }
            NExpr::CodeRegionOffset { region } => self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::CodeRegionOffset {
                        region: self.lower_code_region_ref(region),
                    }),
                },
            ),
            NExpr::CodeRegionLen { region } => self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::CodeRegionLen {
                        region: self.lower_code_region_ref(region),
                    }),
                },
            ),
            NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                let value = self.lower_call(bb, *callee, args, effect_args);
                if self.terminated_blocks[bb.index()] {
                    return;
                }
                let actual = self.value_class(value).cloned();
                let value = if self.value_class(value) == Some(&dst_class) {
                    value
                } else if actual.as_ref().is_some_and(|actual| {
                    self.should_preserve_const_source_class(actual, &dst_class)
                }) {
                    self.refine_local_runtime_class(
                        dst,
                        actual.expect("actual runtime class should be present"),
                    );
                    value
                } else {
                    self.coerce_value(bb, value, &dst_class)
                };
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

    fn lower_const_into(&mut self, bb: RBlockId, dst: RLocalId, const_: &SConst<'db>) {
        match const_ {
            SConst::Value(value) => {
                if sem_const_ty(self.db, *value) == TyId::unit(self.db) {
                    return;
                }
                let target = self
                    .value_class(dst)
                    .cloned()
                    .expect("const destination should have a runtime class");
                let expected_ty =
                    self.const_lowering_ty(self.locals[dst.index()].semantic_ty, &target);
                self.lower_sem_const_for_class(bb, dst, *value, expected_ty, &target);
            }
            SConst::Ref(cref) => panic!("unresolved const ref reached rMIR lowering: {cref:?}"),
        }
    }

    fn lower_sem_const_for_class(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: SemConstId<'db>,
        expected_ty: TyId<'db>,
        target: &RuntimeClass<'db>,
    ) {
        let value = self.reify_runtime_const(expected_ty, value);
        let src = self.lower_sem_const_as_class(bb, value, expected_ty, target);
        let actual = self.value_class(src).cloned();
        let value = if self.value_class(src) == Some(target) {
            src
        } else if actual
            .as_ref()
            .is_some_and(|actual| self.should_preserve_const_source_class(actual, target))
        {
            self.refine_local_runtime_class(
                dst,
                actual.expect("actual runtime class should be present"),
            );
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
        if let Some(value) = self.try_lower_bytes_array_const_as_class(bb, ty, value, target) {
            return value;
        }
        if let Some(scalar) = const_scalar_from_value(self.db, self.env, value) {
            return self.lower_sem_const_scalar(bb, ty, scalar);
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
            ),
            SemConstValue::Unit
            | SemConstValue::Scalar { .. }
            | SemConstValue::TypeLevel { .. } => {
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
        let ptr = self.alloc_runtime_temp(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: ptr,
                expr: RExpr::Builtin(crate::runtime::RuntimeBuiltin::Malloc { size }),
            },
        );
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

        let layout = self.layout_for_ty(ty);
        let dst = self.alloc_runtime_temp(
            ty,
            RuntimeCarrier::Value(RuntimeClass::AggregateValue { layout }),
        );
        let ctor_elems = aggregate_ctor_elems_for_layout(self.db, layout, 3);
        self.lower_aggregate_values(bb, dst, ty, layout, &ctor_elems, &[ptr, len, size]);
        dst
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
    ) -> RLocalId {
        debug_assert!(!matches!(
            value.value(self.db),
            SemConstValue::Scalar { .. }
        ));
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
                self.lower_enum_values(bb, dst, layout, variant, &field_values);
                dst
            }
            SemConstValue::Unit
            | SemConstValue::Scalar { .. }
            | SemConstValue::TypeLevel { .. } => {
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
        self.lower_aggregate_values(bb, dst, ty, layout, &ctor_elems, field_values);
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
        self.lower_aggregate_values(bb, dst, ty, layout, &ctor_elems, &field_values);
    }

    fn lower_aggregate_values(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        ty: TyId<'db>,
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
                kind: RefKind::Provider { .. } | RefKind::Const,
                ..
            }
            | RuntimeClass::RawAddr { .. }) => class,
            RuntimeClass::Scalar(_) => panic!("aggregate destination must not be scalar"),
        };
        self.refine_local_runtime_class(dst, dst_class.clone());
        match dst_class {
            RuntimeClass::AggregateValue { .. } => {
                let temp = self.alloc_runtime_temp(
                    ty,
                    RuntimeCarrier::Value(RuntimeClass::object_ref(layout)),
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: temp,
                        expr: RExpr::AllocObject { layout },
                    },
                );
                for (value, elem) in field_values.iter().copied().zip(ctor_elems.iter()) {
                    if self.value_class(value).is_none() {
                        continue;
                    }
                    let place = RuntimePlace {
                        root: PlaceRoot::Ref(temp),
                        path: vec![elem.elem.clone()].into_boxed_slice(),
                    };
                    self.write_value_to_place(bb, place, value, &elem.class);
                }
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Load {
                            place: RuntimePlace {
                                root: PlaceRoot::Ref(temp),
                                path: Box::default(),
                            },
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
            provider @ RuntimeClass::Ref {
                kind: RefKind::Provider { .. },
                ..
            } => {
                self.lower_single_field_transport_ctor(bb, dst, provider, field_values, "provider")
            }
            raw_addr @ RuntimeClass::RawAddr { .. } => self.lower_single_field_transport_ctor(
                bb,
                dst,
                raw_addr,
                field_values,
                "raw-address",
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

    fn lower_single_field_transport_ctor(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        dst_class: RuntimeClass<'db>,
        field_values: &[RLocalId],
        kind_name: &str,
    ) {
        let [value] = field_values else {
            panic!("{kind_name} aggregate construction requires exactly one field");
        };
        let value = self.coerce_value(bb, *value, &dst_class);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(value),
            },
        );
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
        let stored =
            stored_class_for_ty_in_context(self.db, field_ty, self.env.scope, self.env.assumptions);
        if self.class_is_runtime_zst(&stored) {
            return (
                self.alloc_runtime_temp(field_ty, RuntimeCarrier::Erased),
                stored,
            );
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
                            self.semantic_body.owner.key(self.db).owner(self.db),
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
    ) -> (RLocalId, RuntimeClass<'db>) {
        let stored =
            stored_class_for_ty_in_context(self.db, field_ty, self.env.scope, self.env.assumptions);
        if self.class_is_runtime_zst(&stored) {
            return (
                self.alloc_runtime_temp(field_ty, RuntimeCarrier::Erased),
                stored,
            );
        }
        let value = self.lower_sem_const_as_value(bb, field, field_ty);
        let class = self.value_class(value).cloned().unwrap_or(stored);
        (value, class)
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

    fn lower_field_like(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        base: SLocalId,
        elem: PlaceElem<'db>,
    ) {
        if let PlaceElem::Field(FieldIndex(field_idx)) = &elem
            && self.lower_single_field_transport_local_read(bb, dst, base, *field_idx as usize)
        {
            return;
        }
        let mut place = self.semantic_place(bb, base);
        place.path = vec![elem].into_boxed_slice();
        let projected = self.project_place_class(&place);
        let target = self
            .value_class(dst)
            .cloned()
            .expect("field result must have class");
        if projected == target {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Load { place },
                },
            );
            return;
        }

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
        let copied = self.coerce_value(bb, source, &target);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(copied),
            },
        );
    }

    fn lower_single_field_transport_place_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        place: &NSPlace<'db>,
    ) -> bool {
        let Some(Projection::Field(field_idx)) = place.path.iter().next() else {
            return false;
        };
        if place.path.len() != 1 {
            return false;
        }
        let base = match place.root {
            NSPlaceRoot::CarrierDerefLocal(base) => base,
            NSPlaceRoot::Root(root) => match self.semantic_body.root(root) {
                Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) => *local,
                Some(NBorrowRoot::Provider { .. }) => return false,
                None => return false,
            },
        };
        self.lower_single_field_transport_local_read(bb, dst, base, *field_idx)
    }

    fn lower_single_field_transport_local_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        base: SLocalId,
        field_idx: usize,
    ) -> bool {
        if field_idx != 0 {
            return false;
        }
        let base_ty = self.locals[base.index()].semantic_ty;
        self.lower_single_field_transport_value_read(bb, dst, self.runtime_value(base), base_ty)
    }

    fn lower_single_field_transport_value_read(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: RLocalId,
        value_ty: TyId<'db>,
    ) -> bool {
        if effect_handle_class_for_ty_in_context(
            self.db,
            value_ty,
            self.env.scope,
            self.env.assumptions,
        )
        .is_none()
        {
            return false;
        }
        let Some(base_class) = self.value_class(value) else {
            return false;
        };
        let Some(target) = self.value_class(dst).cloned() else {
            return false;
        };
        if !base_class.is_transport() || !matches!(target, RuntimeClass::Scalar(_)) {
            return false;
        }
        let raw = self.coerce_value(bb, value, &target);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(raw),
            },
        );
        true
    }

    fn lower_enum_tag(&mut self, bb: RBlockId, dst: RLocalId, value: NOperand) {
        if self.semantic_local_is_place_bound(value.local) {
            let value = self.read_semantic_operand(bb, value);
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
        value: NOperand,
        variant: VariantIndex,
    ) {
        if self.semantic_local_is_place_bound(value.local) {
            let variant = self.enum_variant_for_local(value.local, variant);
            let value = self.read_semantic_operand(bb, value);
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

    fn lower_extract_enum_field(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: NOperand,
        variant: VariantIndex,
        field: FieldIndex,
    ) {
        let value_class = self
            .semantic_value_class(value.local)
            .expect("enum value should have a runtime class");
        let variant_id = self.enum_variant_for_local(value.local, variant);
        if self.semantic_local_is_place_bound(value.local) {
            let value = self.read_semantic_operand(bb, value);
            self.push_stmt(
                bb,
                RStmt::EnumAssertVariant {
                    value,
                    variant: variant_id,
                },
            );
            self.lower_enum_extract_value(
                bb,
                dst,
                value,
                variant_id,
                field,
                project_variant_field_class(self.db, value_class.clone(), variant_id, field),
            );
            return;
        }
        match self.local_class(value.local) {
            Some(RuntimeClass::Ref { .. }) => {
                let refined = self.enum_variant_ref(bb, value.local, variant);
                self.lower_field_like(
                    bb,
                    dst,
                    refined,
                    PlaceElem::VariantField {
                        variant: variant_id,
                        field,
                    },
                );
            }
            _ => {
                self.push_stmt(
                    bb,
                    RStmt::EnumAssertVariant {
                        value: self.runtime_value(value.local),
                        variant: variant_id,
                    },
                );
                self.lower_enum_extract_value(
                    bb,
                    dst,
                    self.runtime_value(value.local),
                    variant_id,
                    field,
                    project_variant_field_class(self.db, value_class, variant_id, field),
                );
            }
        }
    }

    fn lower_enum_extract_value(
        &mut self,
        bb: RBlockId,
        dst: RLocalId,
        value: RLocalId,
        variant: VariantId<'db>,
        field: FieldIndex,
        field_class: RuntimeClass<'db>,
    ) {
        let target = self
            .value_class(dst)
            .cloned()
            .expect("enum extract result must have a runtime class");
        if field_class == target {
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::EnumExtract {
                        value,
                        variant,
                        field,
                    },
                },
            );
            return;
        }
        let source = self.alloc_runtime_temp(
            self.locals[dst.index()].semantic_ty,
            RuntimeCarrier::Value(field_class),
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: source,
                expr: RExpr::EnumExtract {
                    value,
                    variant,
                    field,
                },
            },
        );
        let copied = self.coerce_value(bb, source, &target);
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Use(copied),
            },
        );
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
            UnOp::Plus | UnOp::Mut | UnOp::Ref => return None,
        })
    }

    fn runtime_place_from_addr_value(&self, value: RLocalId) -> Option<RuntimePlace<'db>> {
        match self.value_class(value)? {
            RuntimeClass::Ref { .. } => Some(RuntimePlace {
                root: PlaceRoot::Ref(value),
                path: Box::default(),
            }),
            RuntimeClass::RawAddr { space, target } => {
                let pointee = if let Some(layout) = target {
                    RuntimeClass::AggregateValue { layout: *layout }
                } else {
                    let (_, inner) = self.locals[value.index()].semantic_ty.as_borrow(self.db)?;
                    self.top_level_class_for_ty(inner, *space)?
                };
                Some(RuntimePlace {
                    root: PlaceRoot::Ptr {
                        addr: value,
                        space: *space,
                        class: pointee,
                    },
                    path: Box::default(),
                })
            }
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
                let RuntimeClass::Scalar(class) = target.clone() else {
                    return None;
                };
                let lhs = self.alloc_runtime_temp(
                    self.locals[args[0].local.index()].semantic_ty,
                    RuntimeCarrier::Value(target.clone()),
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: lhs,
                        expr: RExpr::Load {
                            place: place.clone(),
                        },
                    },
                );
                let result = self.alloc_runtime_temp(
                    self.locals[args[0].local.index()].semantic_ty,
                    RuntimeCarrier::Value(target.clone()),
                );
                let expr = match op {
                    BinOp::Arith(op) => {
                        self.lower_arith_expr_for_mode(bb, op, checked, lhs, *rhs, &class)?
                    }
                    BinOp::Comp(_) | BinOp::Logical(_) | BinOp::Index => return None,
                };
                self.push_stmt(bb, RStmt::Assign { dst: result, expr });
                self.write_value_to_place(bb, place, result, &target);
                Some(self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased))
            }
        }
    }

    fn lower_call(
        &mut self,
        bb: RBlockId,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> RLocalId {
        let caller_key = self.current_semantic_key();
        let caller_typed_body = caller_key.instantiate_typed_body(self.db);
        let callee_key = resolve_runtime_call_key(
            self.db,
            caller_key,
            &caller_typed_body,
            &self.semantic_body,
            callee,
            args,
        )
        .unwrap_or_else(|err| {
            panic!(
                "runtime call resolution failed while lowering {:?}: {err}",
                self.key
                    .semantic(self.db)
                    .map(|semantic| semantic.key(self.db)),
            )
        });
        let semantic = get_or_build_semantic_instance(self.db, callee_key);
        if let Some(ret) = self.lower_core_primitive_wrapper_call(bb, semantic, args) {
            return ret;
        }
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
        let callee_key = RuntimeInstanceKey::new(
            self.db,
            crate::instance::RuntimeInstanceSource::Semantic(semantic),
            runtime_classes,
        );
        let callee = get_or_build_runtime_instance(self.db, callee_key);
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

        let Some(ret_class) = runtime_return_class(self.db, callee_key) else {
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
        let ret = self.alloc_runtime_temp(ret_ty, RuntimeCarrier::Value(ret_class));
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
        ret
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
        let lowered = self.lower_extern_builtin(func, &args)?;
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
            LoweredBuiltinCall::Terminator(terminator) => {
                self.set_terminator(bb, terminator);
                self.alloc_runtime_temp(TyId::unit(self.db), RuntimeCarrier::Erased)
            }
        })
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
        let semantic_key = SemanticInstanceKey::new(
            self.db,
            BodyOwner::Func(func),
            GenericSubst::new(self.db, vec![value_ty]),
            EffectProviderSubst::empty(self.db),
            ImplEnv::new(self.db, scope, assumptions, Vec::new()),
        );
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
        let selected = self.with_current_body_cx(|cx| {
            let mut class_cache = InferClassCache::new(self.semantic_body.locals.len());
            RuntimeArgSelector::new(cx.env, cx.carriers, Some(&mut class_cache))
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
        let selected = self.with_current_body_cx(|cx| {
            let mut class_cache = InferClassCache::new(self.semantic_body.locals.len());
            RuntimeArgSelector::new(cx.env, cx.carriers, Some(&mut class_cache))
                .selected_call_inputs(args, effect_args, plan)
        });
        RuntimeArgLowerer::new(self, bb).lower_all(&selected)
    }

    fn lower_extern_builtin(
        &self,
        func: Func<'db>,
        args: &[RLocalId],
    ) -> Option<LoweredBuiltinCall<'db>> {
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
                    Some(word.clone()),
                )
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
        let bytes_ty = self.semantic_body.local(bytes.local)?.ty;
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
            &RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                target: Some(layout),
            },
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
        terminator: &NSTerminator<'db>,
    ) -> RTerminator<'db> {
        match &terminator.kind {
            NSTerminatorKind::Goto(block) => RTerminator::Goto(self.runtime_block(*block)),
            NSTerminatorKind::Branch {
                cond,
                then_bb,
                else_bb,
            } => RTerminator::Branch {
                cond: self.read_semantic_operand(bb, *cond),
                then_bb: self.runtime_block(*then_bb),
                else_bb: self.runtime_block(*else_bb),
            },
            NSTerminatorKind::MatchEnum {
                value,
                enum_ty,
                cases,
                default,
            } => {
                let enum_layout = self.enum_layout_for_local(value.local);
                let tag_class = RuntimeClass::Scalar(ScalarClass {
                    repr: match enum_layout.data(self.db) {
                        crate::runtime::Layout::Enum(layout) => layout.tag.repr,
                        _ => unreachable!(),
                    },
                    role: ScalarRole::EnumTag { enum_layout },
                });
                let tag = self.alloc_runtime_temp(*enum_ty, RuntimeCarrier::Value(tag_class));
                self.lower_enum_tag(bb, tag, *value);
                RTerminator::MatchEnumTag {
                    tag,
                    enum_layout,
                    cases: cases
                        .iter()
                        .map(|(variant, block)| {
                            (
                                VariantId {
                                    enum_layout,
                                    index: variant.0,
                                },
                                self.runtime_block(*block),
                            )
                        })
                        .collect(),
                    default: default.map(|block| self.runtime_block(block)),
                }
            }
            NSTerminatorKind::Return(value) => {
                let ret_class = self.ret_class.clone();
                RTerminator::Return(match ret_class {
                    Some(class) => {
                        value.map(|value| self.lower_semantic_operand_for_class(bb, value, &class))
                    }
                    None => None,
                })
            }
        }
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
        let source = self
            .value_class(src)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "cannot coerce erased value {src:?} to {target:?}; owner={:?}; src_ty={}; locals={:?}",
                    self.key
                        .semantic(self.db)
                        .map(|semantic| semantic.key(self.db).owner(self.db)),
                    self.locals[src.index()].semantic_ty.pretty_print(self.db),
                    self.locals,
                )
            });
        let semantic_ty = self.locals[src.index()].semantic_ty;
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
        let layout_source_ty = |layout: LayoutId<'db>| match layout.data(self.db) {
            crate::runtime::Layout::Struct(data) => {
                data.source_ty.pretty_print(self.db).to_string()
            }
            crate::runtime::Layout::Array(data) => data.source_ty.pretty_print(self.db).to_string(),
            crate::runtime::Layout::Enum(data) => data.source_ty.pretty_print(self.db).to_string(),
        };
        let source_layout = match &source {
            RuntimeClass::Ref { .. } => source
                .aggregate_layout()
                .map(|layout| (layout, layout.data(self.db), layout_source_ty(layout))),
            RuntimeClass::AggregateValue { layout }
            | RuntimeClass::RawAddr {
                target: Some(layout),
                ..
            } => Some((*layout, layout.data(self.db), layout_source_ty(*layout))),
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { target: None, .. } => None,
        };
        let target_layout = match &target {
            RuntimeClass::Ref { .. } => target
                .aggregate_layout()
                .map(|layout| (layout, layout.data(self.db), layout_source_ty(layout))),
            RuntimeClass::AggregateValue { layout }
            | RuntimeClass::RawAddr {
                target: Some(layout),
                ..
            } => Some((*layout, layout.data(self.db), layout_source_ty(*layout))),
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { target: None, .. } => None,
        };
        let owner = self
            .key
            .semantic(self.db)
            .map(|semantic| semantic.key(self.db).owner(self.db));
        panic!(
            "{kind} runtime class coercion in {:?} owner={:?} from {source:?} to {target:?}; source_layout={source_layout:?}; target_layout={target_layout:?}; src={src:?}; src_ty={}; locals={:?}",
            self.key.source(self.db),
            owner,
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
                    (&local.facts.interface, &local.facts.origin),
                    (
                        SemanticLocalKind::PlaceBoundValue,
                        NLocalOrigin::AliasedPlace
                    )
                )
            })
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
        let Some(dst_place) = self
            .semantic_body
            .local(local)
            .and_then(|local| local.backing_place())
        else {
            return false;
        };
        match expr {
            NExpr::ReadPlace { place, .. } => place == dst_place,
            NExpr::Use(operand) => {
                self.semantic_body
                    .local(operand.local)
                    .and_then(|local| local.backing_place())
                    == Some(dst_place)
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                let Some(mut place) = self.alias_source_place_for_local(value.local) else {
                    return false;
                };
                let Some(value_local) = self.semantic_body.local(value.local) else {
                    return false;
                };
                place.path.push(Projection::VariantField {
                    variant: *variant,
                    enum_ty: value_local.ty,
                    field_idx: field.0 as usize,
                });
                place == *dst_place
            }
            NExpr::CodeRegionRef { .. }
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::ArrayRepeat { .. }
            | NExpr::AggregateMake { .. }
            | NExpr::EnumMake { .. }
            | NExpr::Borrow { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::Call { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => false,
        }
    }

    fn alias_source_place_for_local(&self, local: SLocalId) -> Option<NSPlace<'db>> {
        let local_data = self.semantic_body.local(local)?;
        if let Some(place) = local_data.backing_place() {
            return Some(place.clone());
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { provider, .. } => {
                let root = if let Some(provider) = provider {
                    NSPlaceRoot::Root(self.provider_borrow_root(provider)?)
                } else {
                    NSPlaceRoot::CarrierDerefLocal(local)
                };
                Some(NSPlace {
                    root,
                    path: Default::default(),
                })
            }
            NormalizedBindingLowering::Erased
            | NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => None,
        }
    }

    fn provider_borrow_root(
        &self,
        provider: &hir::semantic::ProviderBinding<'db>,
    ) -> Option<NBorrowRootId> {
        self.semantic_body
            .borrow_roots
            .iter()
            .enumerate()
            .find_map(|(idx, root)| match root {
                NBorrowRoot::Provider { binding } if binding == provider => {
                    Some(NBorrowRootId::from_u32(idx as u32))
                }
                NBorrowRoot::Param { .. }
                | NBorrowRoot::LocalSlot { .. }
                | NBorrowRoot::Provider { .. } => None,
            })
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

    fn runtime_local_uses_source_transport(&self, local: RLocalId) -> bool {
        local.index() < self.semantic_locals.len()
            && (matches!(
                self.semantic_locals[local.index()],
                RuntimeLocalLowering::PlaceCarrier { .. }
            ) || self
                .facts
                .boundary_source_transport_sensitive(SLocalId::from_u32(local.index() as u32)))
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
        if let RuntimeLocalLowering::DirectCarrier {
            provider: Some(provider),
            ..
        } = self.semantic_local_lowering(local)
        {
            return Some(PlaceRoot::Provider(*provider));
        }
        self.local_root(local)
    }

    fn try_semantic_place(&mut self, bb: RBlockId, local: SLocalId) -> Option<RuntimePlace<'db>> {
        if let Some(place) = nonself_backing_value_place(&self.semantic_body, local).cloned() {
            return self.try_lower_place(bb, &place);
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

    fn try_lower_place(&mut self, bb: RBlockId, place: &NSPlace<'db>) -> Option<RuntimePlace<'db>> {
        let mut runtime_place = match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => self.try_semantic_place(bb, local)?,
            NSPlaceRoot::Root(root) => match self.semantic_body.root(root)? {
                NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                    self.try_semantic_place(bb, *local)?
                }
                NBorrowRoot::Provider { binding } => RuntimePlace {
                    root: self.provider_place_root(binding)?,
                    path: Box::default(),
                },
            },
        };
        let mut current = self.project_place_class(&runtime_place);
        let mut projected = Vec::new();
        for (idx, elem) in place.path.iter().enumerate() {
            match elem {
                Projection::Deref => {
                    panic!("unexpected deref in normalized runtime place: {place:?}")
                }
                Projection::Field(field) => {
                    let field = FieldIndex((*field).try_into().expect("field index fits in u16"));
                    projected.push(PlaceElem::Field(field));
                    current = project_field_class(self.db, current, field);
                }
                Projection::VariantField {
                    variant, field_idx, ..
                } => {
                    let field =
                        FieldIndex((*field_idx).try_into().expect("field index fits in u16"));
                    let variant = VariantId {
                        enum_layout: current
                            .aggregate_layout()
                            .expect("variant-field places should project from enum layouts"),
                        index: variant.0,
                    };
                    projected.push(PlaceElem::VariantField { variant, field });
                    current = project_variant_field_class(self.db, current, variant, field);
                }
                Projection::Index(IndexSource::Dynamic(index)) => {
                    projected.push(PlaceElem::Index(IndexSource::Dynamic(
                        self.read_semantic_value(bb, *index),
                    )));
                    current = project_index_class(self.db, current);
                }
                Projection::Index(IndexSource::Constant(index)) => {
                    projected.push(PlaceElem::Index(IndexSource::Constant(*index)));
                    current = project_index_class(self.db, current);
                }
                Projection::Discriminant => {
                    panic!("discriminant projections are not valid runtime places: {place:?}");
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
            RuntimeLocalLowering::DirectCarrier { provider, .. } => provider.map_or_else(
                || self.runtime_value(local),
                |provider| self.provider_binding_value(provider),
            ),
            RuntimeLocalLowering::PlaceCarrier { .. }
            | RuntimeLocalLowering::PlaceBoundValue { .. } => {
                let place = self.semantic_place(bb, local);
                let place_class = self.project_place_class(&place);
                let temp = self.alloc_runtime_temp(
                    self.locals[local.index()].semantic_ty,
                    RuntimeCarrier::Value(place_class),
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: temp,
                        expr: RExpr::Load { place },
                    },
                );
                temp
            }
        }
    }

    fn read_semantic_operand(&mut self, bb: RBlockId, operand: NOperand) -> RLocalId {
        self.read_semantic_value(bb, operand.local)
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
        let selected = self.with_current_body_cx(|cx| {
            RuntimeArgSelector::new(cx.env, cx.carriers, None)
                .selected_semantic_operand_for_class(operand, target)
        });
        self.lower_selected_runtime_arg(bb, operand, &selected)
    }

    fn actual_aggregate_value_from_runtime_source(
        &mut self,
        bb: RBlockId,
        local: SLocalId,
    ) -> Option<RLocalId> {
        if let Some(place) = snapshot_source_place(&self.semantic_body, local).cloned()
            && let Some(place) = self.try_lower_place(bb, &place)
        {
            let class = self.project_place_class(&place);
            let actual = actual_aggregate_class_from_runtime_source(&class)?;
            let value = self.alloc_runtime_temp(
                self.locals[local.index()].semantic_ty,
                RuntimeCarrier::Value(class.clone()),
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: value,
                    expr: RExpr::Load { place },
                },
            );
            return Some(self.coerce_value_if_needed(bb, value, &actual));
        }
        let value = self.read_semantic_value(bb, local);
        let class = self.value_class(value).cloned()?;
        let actual = actual_aggregate_class_from_runtime_source(&class)?;
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
                target: Some(layout),
                space,
            } => Some(RuntimePlace {
                root: PlaceRoot::Ptr {
                    addr: value,
                    space: *space,
                    class: RuntimeClass::AggregateValue { layout: *layout },
                },
                path: Box::default(),
            }),
            RuntimeClass::RawAddr {
                target: None,
                space,
            } if matches!(target, RuntimeClass::Scalar(_)) => Some(RuntimePlace {
                root: PlaceRoot::Ptr {
                    addr: value,
                    space: *space,
                    class: target.clone(),
                },
                path: Box::default(),
            }),
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => None,
            RuntimeClass::RawAddr { target: None, .. } => None,
        }
    }

    fn lower_semantic_operand_for_boundary(
        &mut self,
        bb: RBlockId,
        operand: NOperand,
        boundary: &crate::runtime::RuntimeBoundarySpec<'db>,
    ) -> RLocalId {
        let selected = self.with_current_body_cx(|cx| {
            RuntimeArgSelector::new(cx.env, cx.carriers, None)
                .selected_semantic_operand_for_boundary(operand, boundary)
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
            RuntimeLocalLowering::PlaceBoundValue {
                provider: Some(provider),
                ..
            } => Some(self.provider_binding_value(*provider)),
            RuntimeLocalLowering::PlaceBoundValue { provider: None, .. } => None,
            RuntimeLocalLowering::DirectCarrier { provider, .. } => Some(provider.map_or_else(
                || self.runtime_value(local),
                |provider| self.provider_binding_value(provider),
            )),
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
        let body = RuntimeBody {
            owner: self.instance,
            key: self.key,
            signature: RuntimeInterfaceSignature {
                params: Vec::new(),
                ret: None,
            },
            semantic_locals: self.semantic_locals.clone(),
            provider_bindings: self.provider_bindings.clone(),
            locals: self.locals.clone(),
            blocks: Vec::new(),
        };
        resolve_runtime_place_address_class(self.db, &program, &body, place)
            .unwrap_or_else(|err| panic!("invalid runtime place address class: {err:?}"))
    }

    fn write_semantic_value(&mut self, bb: RBlockId, local: SLocalId, src: RLocalId) {
        match self.semantic_local_lowering(local).clone() {
            RuntimeLocalLowering::Erased => {}
            RuntimeLocalLowering::DirectValue | RuntimeLocalLowering::PlaceCarrier { .. } => {
                let dst = self.runtime_value(local);
                let Some(mut target) = self.value_class(dst).cloned() else {
                    return;
                };
                if self.runtime_local_uses_source_transport(dst)
                    && matches!(
                        target,
                        RuntimeClass::AggregateValue { .. }
                            | RuntimeClass::Ref {
                                kind: RefKind::Object,
                                ..
                            }
                    )
                    && let Some(actual) =
                        self.with_current_body_cx(|cx| cx.actual_aggregate_class_for_source(local))
                {
                    target = match target {
                        RuntimeClass::AggregateValue { .. } => actual,
                        RuntimeClass::Ref {
                            kind: RefKind::Object,
                            ..
                        } => RuntimeClass::object_ref(
                            actual.aggregate_layout().expect("aggregate ref layout"),
                        ),
                        RuntimeClass::Ref { .. }
                        | RuntimeClass::Scalar(_)
                        | RuntimeClass::RawAddr { .. } => unreachable!(),
                    };
                    self.refine_local_runtime_class(dst, target.clone());
                }
                let src = self.coerce_value(bb, src, &target);
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Use(src),
                    },
                );
            }
            RuntimeLocalLowering::DirectCarrier { provider, .. } => {
                let dst = provider
                    .map(|provider| self.provider_binding_value(provider))
                    .unwrap_or_else(|| self.runtime_value(local));
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

    fn specialize_runtime_target_from_operand(
        &mut self,
        dst: RLocalId,
        src: NOperand,
        target: &RuntimeClass<'db>,
    ) -> RuntimeClass<'db> {
        if !self.runtime_local_uses_source_transport(dst)
            || !matches!(
                target,
                RuntimeClass::AggregateValue { .. }
                    | RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    }
            )
        {
            return target.clone();
        }
        let Some(actual) =
            self.with_current_body_cx(|cx| cx.actual_aggregate_class_for_source(src.local))
        else {
            return target.clone();
        };
        let target = match target {
            RuntimeClass::AggregateValue { .. } => actual,
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            } => RuntimeClass::object_ref(actual.aggregate_layout().expect("aggregate ref layout")),
            RuntimeClass::Ref { .. } | RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => {
                unreachable!()
            }
        };
        self.refine_local_runtime_class(dst, target.clone());
        target
    }

    fn lower_place(&mut self, bb: RBlockId, place: &NSPlace<'db>) -> RuntimePlace<'db> {
        self.try_lower_place(bb, place).unwrap_or_else(|| {
            let root_info = match place.root {
                NSPlaceRoot::CarrierDerefLocal(local) => format!(
                    "carrier_deref local={local:?} lowering={:?} root={:?}",
                    self.semantic_local_lowering(local),
                    self.locals[self.runtime_value(local).index()].root,
                ),
                NSPlaceRoot::Root(root) => match self.semantic_body.root(root) {
                    Some(NBorrowRoot::Param { local, param_idx }) => format!(
                        "param root={root:?} local={local:?} param_idx={param_idx} lowering={:?} runtime_root={:?}",
                        self.semantic_local_lowering(*local),
                        self.locals[self.runtime_value(*local).index()].root,
                    ),
                    Some(NBorrowRoot::LocalSlot { local }) => format!(
                        "local root={root:?} local={local:?} lowering={:?} runtime_root={:?}",
                        self.semantic_local_lowering(*local),
                        self.locals[self.runtime_value(*local).index()].root,
                    ),
                    Some(NBorrowRoot::Provider { binding }) => {
                        format!("provider root={root:?} binding={binding:?} provider_place_root={:?}", self.provider_place_root(binding))
                    }
                    None => format!("missing root {root:?}"),
                },
            };
            panic!(
                "cannot lower erased place root: place={place:?}; root_info={root_info}; locals={:?}",
                self.locals,
            )
        })
    }

    fn project_place_class(&self, place: &RuntimePlace<'db>) -> RuntimeClass<'db> {
        let mut current = match &place.root {
            PlaceRoot::Slot(local) => self
                .local_root_class_r(*local)
                .expect("projected places should have runtime root classes"),
            PlaceRoot::Ref(local) => match self
                .value_class(*local)
                .cloned()
                .expect("projected ref places should have runtime classes")
            {
                RuntimeClass::Ref { pointee, .. } => *pointee,
                class => class,
            },
            PlaceRoot::Provider(binding) => self.provider_binding(*binding).place_class.clone(),
            PlaceRoot::Ptr { class, .. } => class.clone(),
        };
        for elem in place.path.iter() {
            current = match elem {
                PlaceElem::Field(field) => project_field_class(self.db, current, *field),
                PlaceElem::Index(_) => project_index_class(self.db, current),
                PlaceElem::VariantField { variant, field } => {
                    project_variant_field_class(self.db, current, *variant, *field)
                }
                PlaceElem::Deref => current
                    .deref_target()
                    .unwrap_or_else(|| panic!("invalid runtime place deref class: {current:?}")),
            };
        }
        current
    }

    fn class_is_runtime_zst(&self, class: &RuntimeClass<'db>) -> bool {
        match class {
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                false
            }
            RuntimeClass::AggregateValue { layout } => self.layout_is_runtime_zst(*layout),
        }
    }

    fn layout_is_runtime_zst(&self, layout: LayoutId<'db>) -> bool {
        match layout.data(self.db) {
            crate::runtime::Layout::Struct(layout) => layout
                .fields
                .iter()
                .all(|field| self.class_is_runtime_zst(field)),
            crate::runtime::Layout::Array(layout) => {
                layout.len == 0 || self.class_is_runtime_zst(&layout.elem)
            }
            crate::runtime::Layout::Enum(_) => false,
        }
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

    fn enum_variant_ref(
        &mut self,
        bb: RBlockId,
        value: SLocalId,
        variant: VariantIndex,
    ) -> SLocalId {
        let class = self
            .local_class(value)
            .cloned()
            .expect("enum ref should have a class");
        let RuntimeClass::Ref { pointee, kind, .. } = class else {
            panic!("enum variant refs require ref-form enums");
        };
        let layout = pointee.aggregate_layout().expect("enum ref pointee layout");
        let variant_id = VariantId {
            enum_layout: layout,
            index: variant.0,
        };
        let temp = self.alloc_runtime_temp(
            self.locals[value.index()].semantic_ty,
            RuntimeCarrier::Value(RuntimeClass::Ref {
                pointee: Box::new(RuntimeClass::AggregateValue { layout }),
                kind: kind.clone(),
                view: RefView::EnumVariant(variant_id),
            }),
        );
        self.locals[temp.index()].root = RuntimeLocalRoot::Ref(RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue { layout }),
            kind,
            view: RefView::EnumVariant(variant_id),
        });
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: temp,
                expr: RExpr::EnumAssertVariantRef {
                    root: self.runtime_value(value),
                    variant: variant_id,
                },
            },
        );
        SLocalId::from_u32(temp.as_u32())
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
        }
    }

    fn runtime_value(&self, local: SLocalId) -> RLocalId {
        RLocalId::from_u32(local.as_u32())
    }

    fn runtime_block(&self, block: SBlockId) -> RBlockId {
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

    fn local_root_class_r(&self, local: RLocalId) -> Option<RuntimeClass<'db>> {
        match &self.locals.get(local.index())?.root {
            RuntimeLocalRoot::None => None,
            RuntimeLocalRoot::Slot(class)
            | RuntimeLocalRoot::Ref(class)
            | RuntimeLocalRoot::Ptr { class, .. } => Some(class.clone()),
        }
    }

    fn value_class(&self, local: RLocalId) -> Option<&RuntimeClass<'db>> {
        match self.locals.get(local.index())?.carrier {
            RuntimeCarrier::Erased => None,
            RuntimeCarrier::Value(ref class) => Some(class),
        }
    }

    fn refine_local_runtime_class(&mut self, local: RLocalId, class: RuntimeClass<'db>) {
        let class = self
            .value_class(local)
            .and_then(|current| merge_runtime_class(self.db, current, &class))
            .unwrap_or(class);
        self.locals[local.index()].carrier = RuntimeCarrier::Value(class.clone());
        if let Some(carrier) = self.semantic_carriers.get_mut(local.index()) {
            *carrier = RuntimeCarrier::Value(class.clone());
        }
        match &mut self.locals[local.index()].root {
            RuntimeLocalRoot::Slot(root) => *root = class,
            RuntimeLocalRoot::Ref(root) => *root = class,
            RuntimeLocalRoot::Ptr { .. } | RuntimeLocalRoot::None => {}
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
            panic!("semantic const should reify for runtime lowering: {value:?}")
        })
    }

    fn const_lowering_ty(&self, fallback: TyId<'db>, target: &RuntimeClass<'db>) -> TyId<'db> {
        target
            .aggregate_layout()
            .map_or(fallback, |layout| match layout.data(self.db) {
                crate::runtime::Layout::Struct(layout) => layout.source_ty,
                crate::runtime::Layout::Array(layout) => layout.source_ty,
                crate::runtime::Layout::Enum(layout) => layout.source_ty,
            })
    }

    fn should_preserve_const_source_class(
        &self,
        actual: &RuntimeClass<'db>,
        target: &RuntimeClass<'db>,
    ) -> bool {
        merge_runtime_class(self.db, actual, target).is_some_and(|merged| &merged == actual)
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
                let value = self.emitter.read_semantic_operand(self.bb, *operand);
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
            (RuntimeArgSource::RuntimeValue { local }, use_plan) => {
                let value = self.emitter.runtime_value(*local);
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::HandleLikeValue { local }, use_plan) => {
                let value = self
                    .emitter
                    .handle_like_semantic_value(*local)
                    .unwrap_or_else(|| self.emitter.read_semantic_value(self.bb, *local));
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (
                RuntimeArgSource::PlaceAddress { place, semantic_ty },
                RuntimeValueUsePlan::UseValue,
            ) => {
                let place = self.emitter.lower_place(self.bb, place);
                self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                )
            }
            (RuntimeArgSource::PlaceAddress { place, semantic_ty }, use_plan) => {
                let place = self.emitter.lower_place(self.bb, place);
                let value = self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                );
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (RuntimeArgSource::PlaceValue { place, semantic_ty }, use_plan) => {
                let place = self.emitter.lower_place(self.bb, place);
                let value = self
                    .emitter
                    .load_runtime_place_value(self.bb, place, *semantic_ty);
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (
                RuntimeArgSource::SemanticPlaceAddress { local, semantic_ty },
                RuntimeValueUsePlan::UseValue,
            ) => {
                let place = self.emitter.semantic_place(self.bb, *local);
                self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                )
            }
            (RuntimeArgSource::SemanticPlaceAddress { local, semantic_ty }, use_plan) => {
                let place = self.emitter.semantic_place(self.bb, *local);
                let value = self.emitter.lower_place_addr_of_for_class(
                    *semantic_ty,
                    self.bb,
                    place,
                    selected.class.clone(),
                );
                self.apply_use_plan(value, use_plan.clone(), *semantic_ty)
            }
            (RuntimeArgSource::AggregateFromRuntimeSource { local }, use_plan) => {
                let Some(value) = self
                    .emitter
                    .actual_aggregate_value_from_runtime_source(self.bb, *local)
                else {
                    panic!(
                        "selected aggregate runtime source was not lowerable: caller={:?}; local={local:?}; selected={selected:?}",
                        self.emitter.current_semantic_key(),
                    )
                };
                self.apply_use_plan(value, use_plan.clone(), self.semantic_local_ty(*local))
            }
            (RuntimeArgSource::Placeholder { semantic_ty }, RuntimeValueUsePlan::UseValue) => {
                self.lower_placeholder(*semantic_ty, selected.class.clone())
            }
            (RuntimeArgSource::Placeholder { semantic_ty }, use_plan) => {
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

    fn semantic_ty(&self, operand: NOperand) -> TyId<'db> {
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
    use url::Url;

    use crate::build_test_runtime_package;

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
