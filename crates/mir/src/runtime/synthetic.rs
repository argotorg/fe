use common::layout::EVM_LAYOUT;
use cranelift_entity::EntityRef;
use hir::{
    analysis::{
        semantic::{GenericSubst, ImplEnv, SemanticInstanceKey, get_or_build_semantic_instance},
        ty::{
            corelib::{resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path},
            trait_def::{TraitInstId, resolve_trait_method_instance},
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::BodyOwner,
            ty_def::{InvalidCause, TyId},
        },
    },
    hir_def::{
        IdentId,
        expr::{ArithBinOp, BinOp, CompBinOp},
    },
};

use crate::{
    db::MirDb,
    instance::{
        RuntimeInstance, RuntimeInstanceKey, RuntimeInstanceSource, get_or_build_runtime_instance,
    },
    layout_size_bytes,
    runtime::{
        AddressSpaceKind, BorrowAccess, ConstScalar, ContractInitAbiPlan, ContractRecvAbiPlan,
        DispatchDefault, EntryEffectArgPlan, InitArgsPlan, PlaceElem, PlaceRoot, RBlock, RBlockId,
        RExpr, RLocal, RLocalId, RStmt, RTerminator, RefKind, RefView, RuntimeBody,
        RuntimeBoundarySpec, RuntimeBuiltin, RuntimeCarrier, RuntimeClass, RuntimeExitBehavior,
        RuntimeInputPlan, RuntimeInterfaceSignature, RuntimeLocalRoot, RuntimeParamPlan,
        RuntimePlace, RuntimeReturnPlan, RuntimeSyntheticSpec, ScalarClass, ScalarRepr, ScalarRole,
        TargetRootProviderBinding, TargetRootProviderMaterialization,
        lower::{
            boundary::{RuntimeValueAddress, RuntimeValueSource},
            classify::{ref_class_for_place_result, semantic_return_ty},
            conversion::{RuntimeConversionEmitter, emit_runtime_coercion},
            interface::runtime_visible_binding_plans,
            realize::{
                RuntimeValueArgSelectionCx, RuntimeValueArgSelector, RuntimeValueUseEmitter,
                SelectedRuntimeValueArg, emit_selected_runtime_value_args,
            },
            tuple::{
                RuntimeTupleFieldEmitter, extract_runtime_tuple_fields, memory_fallback_class,
            },
            type_info::{RuntimeTypeEnv, top_level_class_for_ty_in_env},
        },
        package::runtime_instance_for_semantic,
    },
};

pub(crate) fn runtime_synthetic_exit_behavior<'db>(
    spec: RuntimeSyntheticSpec<'db>,
) -> RuntimeExitBehavior {
    match spec {
        RuntimeSyntheticSpec::ContractInitAbi { .. } => RuntimeExitBehavior::MayReturn,
        RuntimeSyntheticSpec::MainRoot { .. }
        | RuntimeSyntheticSpec::TestRoot { .. }
        | RuntimeSyntheticSpec::ManualContractRoot { .. }
        | RuntimeSyntheticSpec::ContractRecvAbi { .. }
        | RuntimeSyntheticSpec::ContractInitRoot { .. }
        | RuntimeSyntheticSpec::ContractRuntimeRoot { .. }
        | RuntimeSyntheticSpec::CodeRegionRoot { .. } => RuntimeExitBehavior::NeverReturns,
    }
}

pub(crate) fn runtime_synthetic_interface_signature<'db>(
    _spec: RuntimeSyntheticSpec<'db>,
) -> RuntimeInterfaceSignature<'db> {
    RuntimeInterfaceSignature {
        params: Vec::new(),
        ret: None,
    }
}

pub(crate) fn lower_synthetic_runtime_body<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    spec: RuntimeSyntheticSpec<'db>,
) -> RuntimeBody<'db> {
    let mut builder = SyntheticBodyBuilder::new(db, instance);
    match spec {
        RuntimeSyntheticSpec::MainRoot {
            callee,
            entry_effect_args,
        }
        | RuntimeSyntheticSpec::TestRoot {
            callee,
            entry_effect_args,
            ..
        }
        | RuntimeSyntheticSpec::ManualContractRoot {
            callee,
            entry_effect_args,
            ..
        } => builder.build_entry_root(callee, &entry_effect_args),
        RuntimeSyntheticSpec::CodeRegionRoot { callee, .. } => {
            builder.build_passthrough_root(callee)
        }
        RuntimeSyntheticSpec::ContractInitAbi { plan } => builder.build_contract_init_abi(plan),
        RuntimeSyntheticSpec::ContractRecvAbi { plan } => builder.build_contract_recv_abi(plan),
        RuntimeSyntheticSpec::ContractInitRoot {
            init_abi,
            runtime_region,
            ..
        } => builder.build_contract_init_root(init_abi, runtime_region),
        RuntimeSyntheticSpec::ContractRuntimeRoot {
            dispatch, default, ..
        } => builder.build_contract_runtime_root(&dispatch, default),
    }
    builder.finish()
}

struct SyntheticBodyBuilder<'db> {
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    locals: Vec<RLocal<'db>>,
    blocks: Vec<RBlock<'db>>,
}

impl<'db> RuntimeConversionEmitter<'db> for SyntheticBodyBuilder<'db> {
    fn alloc_conversion_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> RLocalId {
        self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(class),
            RuntimeLocalRoot::None,
        )
    }

    fn push_conversion_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        self.push_stmt(bb, stmt);
    }
}

impl<'db> RuntimeValueUseEmitter<'db> for SyntheticBodyBuilder<'db> {
    fn value_class_for_use(&self, value: RLocalId) -> Option<RuntimeClass<'db>> {
        self.locals
            .get(value.index())?
            .carrier
            .value_class()
            .cloned()
    }

    fn coerce_value_for_use(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        self.coerce_runtime_value(bb, src, target, semantic_ty)
    }

    fn emit_addr_of_place_for_use(
        &mut self,
        bb: RBlockId,
        place: RuntimePlace<'db>,
        class: RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        let dst = self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(class.clone()),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::AddrOf { place },
            },
        );
        dst
    }

    fn alloc_value_slot(&mut self, semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> RLocalId {
        self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(class.clone()),
            RuntimeLocalRoot::Slot(class),
        )
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

impl<'db> RuntimeTupleFieldEmitter<'db> for SyntheticBodyBuilder<'db> {
    fn db(&self) -> &'db dyn MirDb {
        self.db
    }

    fn tuple_value_class(&self, tuple: RLocalId) -> Option<RuntimeClass<'db>> {
        self.locals
            .get(tuple.index())?
            .carrier
            .value_class()
            .cloned()
    }

    fn tuple_local_root(&self, tuple: RLocalId) -> RuntimeLocalRoot<'db> {
        self.locals[tuple.index()].root.clone()
    }

    fn alloc_tuple_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        carrier: RuntimeCarrier<'db>,
    ) -> RLocalId {
        self.push_local(semantic_ty, carrier, RuntimeLocalRoot::None)
    }

    fn push_tuple_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        self.push_stmt(bb, stmt);
    }
}

impl<'db> RuntimeValueArgSelectionCx<'db> for SyntheticBodyBuilder<'db> {
    fn runtime_value_class(&self, value: RLocalId) -> Option<RuntimeClass<'db>> {
        self.locals
            .get(value.index())?
            .carrier
            .value_class()
            .cloned()
    }

    fn runtime_value_source(&self, value: RLocalId) -> Option<RuntimeValueSource<'db>> {
        Some(RuntimeValueSource {
            value: self.runtime_value_class(value)?,
            address: self.runtime_value_address(value),
        })
    }

    fn promote_runtime_value_address(
        &mut self,
        value: RLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeValueAddress<'db>> {
        if matches!(
            boundary,
            RuntimeBoundarySpec::BorrowLike {
                access: BorrowAccess::ReadWrite,
                allow,
                ..
            } if allow.allow_object
        ) {
            return self.promote_runtime_aggregate_local_place(value);
        }
        None
    }
}

impl<'db> SyntheticBodyBuilder<'db> {
    fn new(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> Self {
        Self {
            db,
            instance,
            locals: Vec::new(),
            blocks: vec![RBlock {
                stmts: Vec::new(),
                terminator: RTerminator::Stop,
            }],
        }
    }

    fn finish(self) -> RuntimeBody<'db> {
        let signature = self.instance.interface_signature(self.db);
        RuntimeBody {
            owner: self.instance,
            key: self.instance.key(self.db),
            signature,
            semantic_locals: Vec::new(),
            provider_bindings: Vec::new(),
            locals: self.locals,
            blocks: self.blocks,
        }
    }

    fn runtime_interface_signature(
        &self,
        callee: RuntimeInstance<'db>,
    ) -> RuntimeInterfaceSignature<'db> {
        callee.interface_signature(self.db)
    }

    fn build_entry_root(
        &mut self,
        callee: RuntimeInstance<'db>,
        entry_effect_args: &[EntryEffectArgPlan<'db>],
    ) {
        let args = self.emit_entry_effect_args(RBlockId::from_u32(0), entry_effect_args);
        self.build_root_call(callee, args);
    }

    fn build_passthrough_root(&mut self, callee: RuntimeInstance<'db>) {
        let signature = self.runtime_interface_signature(callee);
        let semantic = callee.key(self.db).semantic(self.db);
        let param_entries =
            semantic.map(|semantic| runtime_visible_binding_plans(self.db, semantic));
        if let Some(entries) = param_entries {
            assert_eq!(
                entries.len(),
                signature.params.len(),
                "synthetic passthrough arg count mismatch for {callee:?}"
            );
        }
        let mut args = Vec::with_capacity(signature.params.len());
        for (idx, param) in signature.params.iter().enumerate() {
            let semantic_ty = param_entries
                .and_then(|entries| entries.get(idx))
                .map(|entry| entry.semantic_ty)
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
            args.push(self.push_synthetic_default_value(
                RBlockId::from_u32(0),
                semantic_ty,
                &param.class,
            ));
        }

        self.build_root_call(callee, args);
    }

    fn build_root_call(&mut self, callee: RuntimeInstance<'db>, args: Vec<RLocalId>) {
        let signature = self.runtime_interface_signature(callee);
        assert_eq!(
            args.len(),
            signature.params.len(),
            "synthetic root arg count mismatch for {callee:?}"
        );
        let semantic = callee.key(self.db).semantic(self.db);
        if let RuntimeInstanceSource::Synthetic(synthetic) = callee.key(self.db).source(self.db)
            && runtime_synthetic_exit_behavior(synthetic.spec(self.db).clone())
                == RuntimeExitBehavior::NeverReturns
        {
            self.blocks[0].terminator = RTerminator::TerminalCall {
                callee,
                args: args.into_boxed_slice(),
            };
            return;
        }

        if let Some(class) = signature.ret.clone() {
            let dst = self.push_local(
                semantic
                    .map(|semantic| semantic_return_ty(self.db, semantic))
                    .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),
                RuntimeCarrier::Value(class),
                RuntimeLocalRoot::None,
            );
            self.push_stmt(
                RBlockId::from_u32(0),
                RStmt::Assign {
                    dst,
                    expr: RExpr::Call {
                        callee,
                        args: args.into_boxed_slice(),
                    },
                },
            );
        } else {
            let dst = self.push_erased_local(TyId::unit(self.db));
            self.push_stmt(
                RBlockId::from_u32(0),
                RStmt::Assign {
                    dst,
                    expr: RExpr::Call {
                        callee,
                        args: args.into_boxed_slice(),
                    },
                },
            );
        }
        self.blocks[0].terminator = RTerminator::Stop;
    }

    fn build_contract_init_abi(&mut self, plan: ContractInitAbiPlan<'db>) {
        let zero = self.push_const_word(RBlockId::from_u32(0), 0);
        let mut cont_bb = if plan.payable {
            RBlockId::from_u32(0)
        } else {
            self.emit_nonpayable_guard(zero)
        };

        let mut call_args = Vec::new();
        if let InitArgsPlan::DecodeInitTail {
            tuple_ty,
            decode_fn,
            projected_fields,
        } = plan.init_args
        {
            let self_len = self.push_builtin_value(
                cont_bb,
                TyId::u256(self.db),
                RuntimeClass::Scalar(word_scalar_class()),
                RuntimeBuiltin::CurrentCodeRegionLen,
            );
            let code_size = self.push_builtin_value(
                cont_bb,
                TyId::u256(self.db),
                RuntimeClass::Scalar(word_scalar_class()),
                RuntimeBuiltin::CodeSize,
            );
            let short_code = self.push_bool_binary(cont_bb, CompBinOp::Lt, code_size, self_len);
            let revert_bb = self.new_block();
            let decode_bb = self.new_block();
            self.blocks[cont_bb.index()].terminator = RTerminator::Branch {
                cond: short_code,
                then_bb: revert_bb,
                else_bb: decode_bb,
            };
            self.blocks[revert_bb.index()].terminator = RTerminator::Revert {
                offset: zero,
                len: zero,
            };
            cont_bb = decode_bb;
            let tail_len = self.push_binary_word(decode_bb, ArithBinOp::Sub, code_size, self_len);
            let tail_ptr = self.push_builtin_value(
                decode_bb,
                TyId::u256(self.db),
                RuntimeClass::Scalar(word_scalar_class()),
                RuntimeBuiltin::Malloc { size: tail_len },
            );
            self.push_side_effect_builtin(
                decode_bb,
                RuntimeBuiltin::CodeCopy {
                    dst: tail_ptr,
                    offset: self_len,
                    len: tail_len,
                },
            );
            let input =
                self.push_memory_bytes_value(decode_bb, plan.contract.scope(), tail_ptr, tail_len);
            let decoder_new =
                resolve_sol_decoder_new(self.db, plan.contract.scope()).expect("decoder_new");
            let decoder = self.push_call_result(decode_bb, decoder_new, vec![input]);
            if let Some(decoded) = self.push_call(decode_bb, decode_fn, vec![decoder]) {
                call_args.extend(self.extract_selected_tuple_fields(
                    decode_bb,
                    decoded,
                    tuple_ty,
                    plan.contract.scope(),
                    &projected_fields,
                ));
            }
        }

        if let Some(user_init) = plan.user_init {
            call_args.extend(self.owner_effect_call_args(
                cont_bb,
                user_init,
                call_args.len(),
                &plan.entry_effect_args,
            ));
            let _ = self.push_ignored_call(cont_bb, user_init, call_args);
        }
        self.blocks[cont_bb.index()].terminator = RTerminator::Return(None);
    }

    fn build_contract_recv_abi(&mut self, plan: ContractRecvAbiPlan<'db>) {
        let zero = self.push_const_word(RBlockId::from_u32(0), 0);
        let four = self.push_const_word(RBlockId::from_u32(0), 4);
        let cont_bb = if plan.payable {
            RBlockId::from_u32(0)
        } else {
            self.emit_nonpayable_guard(zero)
        };

        if plan.passthrough
            && matches!(plan.ret, RuntimeReturnPlan::Value { .. })
            && let RuntimeInputPlan::DecodeCalldataPayload { msg_ty, .. } = &plan.input
        {
            self.build_contract_recv_passthrough(
                plan.contract.scope(),
                *msg_ty,
                cont_bb,
                zero,
                four,
            );
            return;
        }

        let mut call_args = Vec::new();
        if let RuntimeInputPlan::DecodeCalldataPayload {
            msg_ty,
            decode_fn,
            projected_fields,
        } = plan.input
        {
            let size = self.push_builtin_value(
                cont_bb,
                TyId::u256(self.db),
                RuntimeClass::Scalar(word_scalar_class()),
                RuntimeBuiltin::CallDataSize,
            );
            let payload_len = self.push_binary_word(cont_bb, ArithBinOp::Sub, size, four);
            let payload_ptr = self.push_builtin_value(
                cont_bb,
                TyId::u256(self.db),
                RuntimeClass::Scalar(word_scalar_class()),
                RuntimeBuiltin::Malloc { size: payload_len },
            );
            self.push_side_effect_builtin(
                cont_bb,
                RuntimeBuiltin::CallDataCopy {
                    dst: payload_ptr,
                    offset: four,
                    len: payload_len,
                },
            );
            let input = self.push_memory_bytes_value(
                cont_bb,
                plan.contract.scope(),
                payload_ptr,
                payload_len,
            );
            let decoder_new =
                resolve_sol_decoder_new(self.db, plan.contract.scope()).expect("decoder_new");
            let decoder = self.push_call_result(cont_bb, decoder_new, vec![input]);
            if let Some(decoded) = self.push_call(cont_bb, decode_fn, vec![decoder]) {
                call_args.extend(self.extract_selected_tuple_fields(
                    cont_bb,
                    decoded,
                    msg_ty,
                    plan.contract.scope(),
                    &projected_fields,
                ));
            }
        }
        call_args.extend(self.owner_effect_call_args(
            cont_bb,
            plan.user_recv,
            call_args.len(),
            &plan.entry_effect_args,
        ));

        let ret = self.push_call(cont_bb, plan.user_recv, call_args);
        match plan.ret {
            RuntimeReturnPlan::Unit => {
                self.blocks[cont_bb.index()].terminator = RTerminator::ReturnData {
                    offset: zero,
                    len: zero,
                };
            }
            RuntimeReturnPlan::Value { .. } => {
                let ret_value = ret.expect("value-returning recv wrapper should produce a value");
                let scope = plan.contract.scope();
                let encode_alloc = resolve_sol_encode_single_root_alloc(
                    self.db,
                    scope,
                    self.locals[ret_value.index()].semantic_ty,
                )
                .expect("encode_single_root_alloc");
                let encoded = self.push_call_result(cont_bb, encode_alloc, vec![ret_value]);
                let fields = self.extract_tuple_fields(
                    cont_bb,
                    encoded,
                    self.locals[encoded.index()].semantic_ty,
                    scope,
                );
                let [offset, len]: [RLocalId; 2] = fields
                    .try_into()
                    .expect("encoded return should expose ptr/len");
                self.blocks[cont_bb.index()].terminator = RTerminator::ReturnData { offset, len };
            }
        }
    }

    fn build_contract_recv_passthrough(
        &mut self,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
        msg_ty: TyId<'db>,
        cont_bb: RBlockId,
        zero: RLocalId,
        four: RLocalId,
    ) {
        let input = self.push_calldata_value(cont_bb, scope, zero);
        let input_ty = self.locals[input.index()].semantic_ty;
        let validate_payload_end = if has_abi_validate(self.db, scope, msg_ty) {
            resolve_sol_validate_payload_end(self.db, scope, msg_ty, input_ty, true)
        } else {
            resolve_sol_validate_payload_end(self.db, scope, msg_ty, input_ty, false)
        }
        .expect("validate_payload_end");
        let payload_end = self.push_call_result(cont_bb, validate_payload_end, vec![input, four]);
        let payload_len = self.push_binary_word(cont_bb, ArithBinOp::Sub, payload_end, four);
        self.push_side_effect_builtin(
            cont_bb,
            RuntimeBuiltin::CallDataCopy {
                dst: zero,
                offset: four,
                len: payload_len,
            },
        );
        self.blocks[cont_bb.index()].terminator = RTerminator::ReturnData {
            offset: zero,
            len: payload_len,
        };
    }

    fn build_contract_init_root(
        &mut self,
        init_abi: RuntimeInstance<'db>,
        runtime_region: crate::runtime::RuntimeCodeRegion<'db>,
    ) {
        let zero = self.push_const_word(RBlockId::from_u32(0), 0);
        let _ = self.push_ignored_call(RBlockId::from_u32(0), init_abi, Vec::new());
        let runtime_offset = self.push_builtin_value(
            RBlockId::from_u32(0),
            TyId::u256(self.db),
            RuntimeClass::Scalar(word_scalar_class()),
            RuntimeBuiltin::CodeRegionOffset {
                region: runtime_region,
            },
        );
        let runtime_len = self.push_builtin_value(
            RBlockId::from_u32(0),
            TyId::u256(self.db),
            RuntimeClass::Scalar(word_scalar_class()),
            RuntimeBuiltin::CodeRegionLen {
                region: runtime_region,
            },
        );
        self.push_side_effect_builtin(
            RBlockId::from_u32(0),
            RuntimeBuiltin::CodeCopy {
                dst: zero,
                offset: runtime_offset,
                len: runtime_len,
            },
        );
        self.blocks[0].terminator = RTerminator::ReturnData {
            offset: zero,
            len: runtime_len,
        };
    }

    fn build_contract_runtime_root(
        &mut self,
        dispatch: &[crate::runtime::DispatchArm<'db>],
        default: DispatchDefault<'db>,
    ) {
        let zero = self.push_const_word(RBlockId::from_u32(0), 0);
        let selector = self.push_builtin_value(
            RBlockId::from_u32(0),
            TyId::u256(self.db),
            RuntimeClass::Scalar(selector_scalar_class()),
            RuntimeBuiltin::CallDataSelector,
        );
        let default_bb = self.new_block();
        let mut cases = Vec::with_capacity(dispatch.len());
        for arm in dispatch {
            let block = self.new_block();
            cases.push((u32_scalar(arm.selector), block));
            self.blocks[block.index()].terminator = RTerminator::TerminalCall {
                callee: arm.wrapper,
                args: Box::default(),
            };
        }
        self.blocks[default_bb.index()].terminator = match default {
            DispatchDefault::RevertEmpty => RTerminator::Revert {
                offset: zero,
                len: zero,
            },
            DispatchDefault::Call { wrapper } => RTerminator::TerminalCall {
                callee: wrapper,
                args: Box::default(),
            },
        };
        self.emit_dispatch_tree(selector, RBlockId::from_u32(0), &cases, default_bb);
    }

    fn emit_dispatch_tree(
        &mut self,
        selector: RLocalId,
        into_block: RBlockId,
        arms: &[(ConstScalar, RBlockId)],
        default_block: RBlockId,
    ) {
        let terminator = match arms.len() {
            0 => RTerminator::Goto(default_block),
            1 => {
                let selector_match = self.push_selector_const(into_block, arms[0].0.clone());
                let cond =
                    self.push_bool_binary(into_block, CompBinOp::Eq, selector, selector_match);
                RTerminator::Branch {
                    cond,
                    then_bb: arms[0].1,
                    else_bb: default_block,
                }
            }
            _ => {
                let pivot_idx = (arms.len() - 1) / 2;
                let pivot = arms[pivot_idx].0.clone();
                let left_block = self.new_block();
                let right_block = self.new_block();
                let pivot_value = self.push_selector_const(into_block, pivot);
                let cond =
                    self.push_bool_binary(into_block, CompBinOp::LtEq, selector, pivot_value);
                self.emit_dispatch_tree(selector, left_block, &arms[..=pivot_idx], default_block);
                self.emit_dispatch_tree(
                    selector,
                    right_block,
                    &arms[pivot_idx + 1..],
                    default_block,
                );
                RTerminator::Branch {
                    cond,
                    then_bb: left_block,
                    else_bb: right_block,
                }
            }
        };
        self.blocks[into_block.index()].terminator = terminator;
    }

    fn emit_nonpayable_guard(&mut self, zero: RLocalId) -> RBlockId {
        let entry = RBlockId::from_u32(0);
        let revert_bb = self.new_block();
        let cont_bb = self.new_block();
        let callvalue = self.push_builtin_value(
            entry,
            TyId::u256(self.db),
            RuntimeClass::Scalar(word_scalar_class()),
            RuntimeBuiltin::CallValue,
        );
        let cond = self.push_bool_binary(entry, CompBinOp::NotEq, callvalue, zero);
        self.blocks[entry.index()].terminator = RTerminator::Branch {
            cond,
            then_bb: revert_bb,
            else_bb: cont_bb,
        };
        self.blocks[revert_bb.index()].terminator = RTerminator::Revert {
            offset: zero,
            len: zero,
        };
        cont_bb
    }

    fn emit_entry_effect_args(
        &mut self,
        bb: RBlockId,
        bindings: &[EntryEffectArgPlan<'db>],
    ) -> Vec<RLocalId> {
        bindings
            .iter()
            .map(|binding| match binding {
                EntryEffectArgPlan::ContractField(binding) => self.push_builtin_value(
                    bb,
                    binding.declared_ty,
                    binding.class.clone(),
                    RuntimeBuiltin::MakeContractFieldRef {
                        slot: binding.slot,
                        class: binding.class.clone(),
                        kind: binding.kind.clone(),
                    },
                ),
                EntryEffectArgPlan::TargetRootProvider(binding) => {
                    self.emit_target_root_provider(bb, binding)
                }
            })
            .collect()
    }

    fn emit_target_root_provider(
        &mut self,
        bb: RBlockId,
        binding: &TargetRootProviderBinding<'db>,
    ) -> RLocalId {
        let root = match binding.materialization {
            TargetRootProviderMaterialization::MemoryObject { layout } => {
                self.push_zeroed_memory_object_ref(bb, binding.declared_ty, layout)
            }
            TargetRootProviderMaterialization::MemoryRawAddr { layout } => {
                self.push_zeroed_memory_raw_root(bb, binding.declared_ty, layout)
            }
        };
        if self.locals[root.index()].carrier.value_class() == Some(&binding.class) {
            root
        } else {
            self.coerce_runtime_value(bb, root, &binding.class, binding.declared_ty)
        }
    }

    fn owner_effect_call_args(
        &mut self,
        bb: RBlockId,
        callee: RuntimeInstance<'db>,
        provided_prefix: usize,
        bindings: &[EntryEffectArgPlan<'db>],
    ) -> Vec<RLocalId> {
        let needed = self
            .runtime_interface_signature(callee)
            .params
            .len()
            .saturating_sub(provided_prefix);
        let args = self.emit_entry_effect_args(bb, bindings);
        assert_eq!(
            args.len(),
            needed,
            "synthetic owner-effect arg count mismatch for {callee:?}"
        );
        args
    }

    fn runtime_type_env(
        &self,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ) -> RuntimeTypeEnv<'db> {
        RuntimeTypeEnv::new(Some(scope), PredicateListId::empty_list(self.db))
    }

    fn extract_selected_tuple_fields(
        &mut self,
        bb: RBlockId,
        tuple: RLocalId,
        tuple_ty: TyId<'db>,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
        field_indices: &[u32],
    ) -> Vec<RLocalId> {
        let env = self.runtime_type_env(scope);
        let field_indices = field_indices.iter().map(|idx| *idx as usize);
        extract_runtime_tuple_fields(
            self,
            bb,
            tuple,
            tuple_ty,
            field_indices,
            |emitter, field_ty| {
                top_level_class_for_ty_in_env(emitter.db(), env, field_ty, AddressSpaceKind::Memory)
                    .unwrap_or_else(memory_fallback_class)
            },
        )
    }

    fn extract_tuple_fields(
        &mut self,
        bb: RBlockId,
        tuple: RLocalId,
        tuple_ty: TyId<'db>,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ) -> Vec<RLocalId> {
        let field_count = tuple_ty.field_types(self.db).len();
        let env = self.runtime_type_env(scope);
        extract_runtime_tuple_fields(self, bb, tuple, tuple_ty, 0..field_count, |emitter, ty| {
            top_level_class_for_ty_in_env(emitter.db(), env, ty, AddressSpaceKind::Memory)
                .unwrap_or_else(memory_fallback_class)
        })
    }

    fn push_call(
        &mut self,
        bb: RBlockId,
        callee: RuntimeInstance<'db>,
        args: Vec<RLocalId>,
    ) -> Option<RLocalId> {
        let (callee, sig, args) = self.prepare_call(bb, callee, args);
        let dst = if let Some(class) = sig.ret {
            let semantic_ty = callee
                .key(self.db)
                .semantic(self.db)
                .map(|semantic| semantic_return_ty(self.db, semantic))
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
            Some(self.push_local(
                semantic_ty,
                RuntimeCarrier::Value(class),
                RuntimeLocalRoot::None,
            ))
        } else {
            let dst = self.push_erased_local(TyId::unit(self.db));
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst,
                    expr: RExpr::Call {
                        callee,
                        args: args.into_boxed_slice(),
                    },
                },
            );
            return None;
        };
        let dst = dst.expect("value-returning call should allocate a destination");
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Call {
                    callee,
                    args: args.into_boxed_slice(),
                },
            },
        );
        Some(dst)
    }

    fn push_ignored_call(
        &mut self,
        bb: RBlockId,
        callee: RuntimeInstance<'db>,
        args: Vec<RLocalId>,
    ) -> RLocalId {
        let (callee, _, args) = self.prepare_call(bb, callee, args);
        let dst = self.push_erased_local(TyId::unit(self.db));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Call {
                    callee,
                    args: args.into_boxed_slice(),
                },
            },
        );
        dst
    }

    fn prepare_call(
        &mut self,
        bb: RBlockId,
        callee: RuntimeInstance<'db>,
        args: Vec<RLocalId>,
    ) -> (
        RuntimeInstance<'db>,
        RuntimeInterfaceSignature<'db>,
        Vec<RLocalId>,
    ) {
        let selected = self.select_call_args(callee, &args);
        let callee = self.specialize_callee_for_selected_args(callee, &selected);
        let signature = self.runtime_interface_signature(callee);
        self.assert_selected_args_match_signature(callee, &selected, &signature);
        let args = self.lower_selected_call_args(bb, &selected);
        (callee, signature, args)
    }

    fn push_call_result(
        &mut self,
        bb: RBlockId,
        callee: RuntimeInstance<'db>,
        args: Vec<RLocalId>,
    ) -> RLocalId {
        self.push_call(bb, callee, args)
            .expect("synthetic helper call should produce a value")
    }

    fn select_call_args(
        &mut self,
        callee: RuntimeInstance<'db>,
        args: &[RLocalId],
    ) -> Vec<SelectedRuntimeValueArg<'db>> {
        let signature = self.runtime_interface_signature(callee);
        if args.is_empty() && signature.params.is_empty() {
            return Vec::new();
        }
        let param_entries = callee
            .key(self.db)
            .semantic(self.db)
            .map(|semantic| runtime_visible_binding_plans(self.db, semantic));
        if let Some(entries) = param_entries {
            assert_eq!(
                entries.len(),
                signature.params.len(),
                "synthetic semantic/runtime param metadata mismatch for {callee:?}"
            );
        }
        assert_eq!(
            args.len(),
            signature.params.len(),
            "synthetic call arg count mismatch for {callee:?}"
        );
        let arg_plans = args
            .iter()
            .zip(signature.params.iter().enumerate())
            .map(|(arg, (idx, param))| {
                let (semantic_ty, plan) = param_entries
                    .and_then(|entries| entries.get(idx))
                    .map(|entry| (entry.semantic_ty, entry.plan.clone()))
                    .unwrap_or_else(|| {
                        (
                            self.locals[arg.index()].semantic_ty,
                            RuntimeParamPlan::Boundary(RuntimeBoundarySpec::ExactTransport(
                                param.class.clone(),
                            )),
                        )
                    });
                (*arg, semantic_ty, plan)
            })
            .collect::<Vec<_>>();
        let mut selector = RuntimeValueArgSelector::new(self);
        arg_plans
            .into_iter()
            .map(|(arg, semantic_ty, plan)| {
                selector.selected_arg_for_param_plan(arg, &plan, semantic_ty)
            })
            .collect()
    }

    fn specialize_callee_for_selected_args(
        &mut self,
        callee: RuntimeInstance<'db>,
        args: &[SelectedRuntimeValueArg<'db>],
    ) -> RuntimeInstance<'db> {
        if args.is_empty() {
            return callee;
        }
        let RuntimeInstanceSource::Semantic(semantic) = callee.key(self.db).source(self.db) else {
            return callee;
        };
        let signature = self.runtime_interface_signature(callee);
        let param_entries = runtime_visible_binding_plans(self.db, semantic);
        assert_eq!(
            param_entries.len(),
            signature.params.len(),
            "synthetic specialized callee metadata mismatch for {callee:?}"
        );
        let params: Vec<RuntimeClass<'db>> = args.iter().map(|arg| arg.class.clone()).collect();
        let key =
            RuntimeInstanceKey::new(self.db, RuntimeInstanceSource::Semantic(semantic), params);
        if key == callee.key(self.db) {
            callee
        } else {
            get_or_build_runtime_instance(self.db, key)
        }
    }

    fn assert_selected_args_match_signature(
        &self,
        callee: RuntimeInstance<'db>,
        selected: &[SelectedRuntimeValueArg<'db>],
        signature: &RuntimeInterfaceSignature<'db>,
    ) {
        assert_eq!(
            selected.len(),
            signature.params.len(),
            "synthetic selected arg count mismatch for {callee:?}"
        );
        for (idx, (selected, param)) in selected.iter().zip(signature.params.iter()).enumerate() {
            assert_eq!(
                selected.class, param.class,
                "synthetic selected arg class mismatch for {callee:?} param {idx}"
            );
        }
    }

    fn lower_selected_call_args(
        &mut self,
        bb: RBlockId,
        selected: &[SelectedRuntimeValueArg<'db>],
    ) -> Vec<RLocalId> {
        emit_selected_runtime_value_args(self, bb, selected)
    }

    fn runtime_value_address(&self, source: RLocalId) -> Option<RuntimeValueAddress<'db>> {
        Some(RuntimeValueAddress {
            place: self.runtime_place_for_local(source)?,
            class: self.runtime_place_addr_class(source)?,
        })
    }

    fn promote_runtime_aggregate_local_place(
        &mut self,
        local: RLocalId,
    ) -> Option<RuntimeValueAddress<'db>> {
        let class = self
            .locals
            .get(local.index())?
            .carrier
            .value_class()?
            .clone();
        let RuntimeClass::AggregateValue { .. } = class else {
            return None;
        };
        match &self.locals[local.index()].root {
            RuntimeLocalRoot::None => {
                self.locals[local.index()].root = RuntimeLocalRoot::Slot(class.clone());
            }
            RuntimeLocalRoot::Slot(_) => {}
            RuntimeLocalRoot::Ref(_) | RuntimeLocalRoot::Ptr { .. } => return None,
        }
        let place = self
            .runtime_place_for_local(local)
            .expect("promoted aggregate local should have a place root");
        let addr_class = self
            .runtime_place_addr_class(local)
            .expect("promoted aggregate local should have a place address class");
        Some(RuntimeValueAddress {
            place,
            class: addr_class,
        })
    }

    fn runtime_place_for_local(&self, local: RLocalId) -> Option<RuntimePlace<'db>> {
        let root = match self.locals.get(local.index())?.root.clone() {
            RuntimeLocalRoot::None => return None,
            RuntimeLocalRoot::Slot(_) => PlaceRoot::Slot(local),
            RuntimeLocalRoot::Ref(_) => PlaceRoot::Ref(local),
            RuntimeLocalRoot::Ptr { space, class } => PlaceRoot::Ptr {
                addr: local,
                space,
                class,
            },
        };
        Some(RuntimePlace {
            root,
            path: Box::default(),
        })
    }

    fn runtime_place_addr_class(&self, local: RLocalId) -> Option<RuntimeClass<'db>> {
        let place = self.runtime_place_for_local(local)?;
        let value_class = self
            .locals
            .get(local.index())?
            .carrier
            .value_class()?
            .clone();
        let (root_class, root_space, force_raw) = match &place.root {
            PlaceRoot::Slot(_) => (
                match &self.locals.get(local.index())?.root {
                    RuntimeLocalRoot::Slot(class) => class.clone(),
                    RuntimeLocalRoot::None
                    | RuntimeLocalRoot::Ref(_)
                    | RuntimeLocalRoot::Ptr { .. } => unreachable!(),
                },
                AddressSpaceKind::Memory,
                false,
            ),
            PlaceRoot::Ref(_) => (
                match &self.locals.get(local.index())?.root {
                    RuntimeLocalRoot::Ref(class) => class.clone(),
                    RuntimeLocalRoot::None
                    | RuntimeLocalRoot::Slot(_)
                    | RuntimeLocalRoot::Ptr { .. } => unreachable!(),
                },
                AddressSpaceKind::Memory,
                false,
            ),
            PlaceRoot::Ptr { space, class, .. } => (
                RuntimeClass::RawAddr {
                    space: *space,
                    target: class.aggregate_layout(),
                },
                *space,
                true,
            ),
            PlaceRoot::Provider(_) => {
                unreachable!("synthetic runtime locals do not use provider roots")
            }
        };
        Some(ref_class_for_place_result(
            &root_class,
            &value_class,
            root_class.address_space().unwrap_or(root_space),
            force_raw,
        ))
    }

    fn coerce_runtime_value(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId {
        let source = self.locals[src.index()]
            .carrier
            .value_class()
            .cloned()
            .unwrap_or_else(|| panic!("cannot coerce erased runtime value {src:?} to {target:?}"));
        let db = self.db;
        emit_runtime_coercion(self, db, bb, src, source, target, semantic_ty)
            .unwrap_or_else(|err| panic!("unsupported synthetic runtime coercion: {err:?}"))
    }

    fn push_builtin_value(
        &mut self,
        bb: RBlockId,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
        builtin: RuntimeBuiltin<'db>,
    ) -> RLocalId {
        let dst = self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(class),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Builtin(builtin),
            },
        );
        dst
    }

    fn push_side_effect_builtin(&mut self, bb: RBlockId, builtin: RuntimeBuiltin<'db>) -> RLocalId {
        let dst = self.push_erased_local(TyId::unit(self.db));
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Builtin(builtin),
            },
        );
        dst
    }

    fn push_const_word(&mut self, bb: RBlockId, value: u32) -> RLocalId {
        self.push_const_scalar(
            bb,
            ConstScalar::Int {
                bits: 256,
                signed: false,
                words: if value == 0 {
                    Vec::new()
                } else {
                    value
                        .to_be_bytes()
                        .into_iter()
                        .skip_while(|byte| *byte == 0)
                        .collect()
                },
            },
        )
    }

    fn push_selector_const(&mut self, bb: RBlockId, value: ConstScalar) -> RLocalId {
        let dst = self.push_local(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(selector_scalar_class())),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::ConstScalar(value),
            },
        );
        dst
    }

    fn push_const_scalar(&mut self, bb: RBlockId, value: ConstScalar) -> RLocalId {
        let dst = self.push_local(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::ConstScalar(value),
            },
        );
        dst
    }

    fn push_memory_bytes_value(
        &mut self,
        bb: RBlockId,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
        ptr: RLocalId,
        len: RLocalId,
    ) -> RLocalId {
        let ptr = if matches!(
            self.locals[ptr.index()].carrier.value_class(),
            Some(RuntimeClass::RawAddr { .. })
        ) {
            let casted = self.push_local(
                TyId::u256(self.db),
                RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
                RuntimeLocalRoot::None,
            );
            self.push_stmt(
                bb,
                RStmt::Assign {
                    dst: casted,
                    expr: RExpr::Cast {
                        value: ptr,
                        to: word_scalar_class(),
                    },
                },
            );
            casted
        } else {
            ptr
        };
        let ty = memory_bytes_ty(self.db, scope).expect("MemoryBytes");
        let class = top_level_class_for_ty_in_env(
            self.db,
            self.runtime_type_env(scope),
            ty,
            AddressSpaceKind::Memory,
        )
        .expect("memory bytes runtime class");
        let root = match &class {
            RuntimeClass::Ref { .. } => RuntimeLocalRoot::Ref(class.clone()),
            _ => RuntimeLocalRoot::Slot(class.clone()),
        };
        let local = self.push_local(ty, RuntimeCarrier::Value(class.clone()), root);
        let root = match class {
            RuntimeClass::Ref { .. } => PlaceRoot::Ref(local),
            RuntimeClass::Scalar(_)
            | RuntimeClass::RawAddr { .. }
            | RuntimeClass::AggregateValue { .. } => PlaceRoot::Slot(local),
        };
        self.push_stmt(
            bb,
            RStmt::Store {
                dst: RuntimePlace {
                    root: root.clone(),
                    path: vec![PlaceElem::Field(hir::analysis::semantic::FieldIndex(0))]
                        .into_boxed_slice(),
                },
                src: ptr,
            },
        );
        self.push_stmt(
            bb,
            RStmt::Store {
                dst: RuntimePlace {
                    root: root.clone(),
                    path: vec![PlaceElem::Field(hir::analysis::semantic::FieldIndex(1))]
                        .into_boxed_slice(),
                },
                src: len,
            },
        );
        local
    }

    fn push_calldata_value(
        &mut self,
        bb: RBlockId,
        scope: hir::hir_def::scope_graph::ScopeId<'db>,
        base: RLocalId,
    ) -> RLocalId {
        let ty = calldata_ty(self.db, scope).expect("CallData");
        let class = top_level_class_for_ty_in_env(
            self.db,
            self.runtime_type_env(scope),
            ty,
            AddressSpaceKind::Memory,
        )
        .expect("calldata runtime class");
        let root = match &class {
            RuntimeClass::Ref { .. } => RuntimeLocalRoot::Ref(class.clone()),
            _ => RuntimeLocalRoot::Slot(class.clone()),
        };
        let local = self.push_local(ty, RuntimeCarrier::Value(class.clone()), root);
        let root = match class {
            RuntimeClass::Ref { .. } => PlaceRoot::Ref(local),
            RuntimeClass::Scalar(_)
            | RuntimeClass::RawAddr { .. }
            | RuntimeClass::AggregateValue { .. } => PlaceRoot::Slot(local),
        };
        self.push_stmt(
            bb,
            RStmt::Store {
                dst: RuntimePlace {
                    root,
                    path: vec![PlaceElem::Field(hir::analysis::semantic::FieldIndex(0))]
                        .into_boxed_slice(),
                },
                src: base,
            },
        );
        local
    }

    fn push_synthetic_default_value(
        &mut self,
        bb: RBlockId,
        semantic_ty: TyId<'db>,
        class: &RuntimeClass<'db>,
    ) -> RLocalId {
        match class {
            RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                target: Some(layout),
            } => self.push_zeroed_memory_raw_root(bb, semantic_ty, *layout),
            RuntimeClass::Ref {
                pointee,
                kind:
                    RefKind::Object
                    | RefKind::Provider {
                        space: AddressSpaceKind::Memory,
                        ..
                    },
                view: RefView::Whole,
            } if pointee.aggregate_layout().is_some() => {
                let object = self.push_zeroed_memory_object_ref(
                    bb,
                    semantic_ty,
                    pointee.aggregate_layout().expect("aggregate ref layout"),
                );
                if self.locals[object.index()].carrier.value_class() == Some(class) {
                    object
                } else {
                    self.coerce_runtime_value(bb, object, class, semantic_ty)
                }
            }
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { .. }
            | RuntimeClass::Ref { .. } => {
                let local = self.push_local(
                    semantic_ty,
                    RuntimeCarrier::Value(class.clone()),
                    RuntimeLocalRoot::None,
                );
                self.push_stmt(
                    bb,
                    RStmt::Assign {
                        dst: local,
                        expr: RExpr::Placeholder {
                            class: class.clone(),
                        },
                    },
                );
                local
            }
        }
    }

    fn push_zeroed_memory_raw_root(
        &mut self,
        bb: RBlockId,
        semantic_ty: TyId<'db>,
        layout: crate::LayoutId<'db>,
    ) -> RLocalId {
        let size = self.push_const_scalar(
            bb,
            ConstScalar::Int {
                bits: 256,
                signed: false,
                words: layout_size_bytes(self.db, layout, EVM_LAYOUT)
                    .to_be_bytes()
                    .into_iter()
                    .skip_while(|byte| *byte == 0)
                    .collect(),
            },
        );
        let ptr = self.push_builtin_value(
            bb,
            TyId::u256(self.db),
            RuntimeClass::Scalar(word_scalar_class()),
            RuntimeBuiltin::Malloc { size },
        );
        let raw_class = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: Some(layout),
        };
        let raw = self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(raw_class.clone()),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst: raw,
                expr: RExpr::WordToRawAddr {
                    value: ptr,
                    space: AddressSpaceKind::Memory,
                    target: Some(layout),
                },
            },
        );
        raw
    }

    fn push_zeroed_memory_object_ref(
        &mut self,
        bb: RBlockId,
        semantic_ty: TyId<'db>,
        layout: crate::LayoutId<'db>,
    ) -> RLocalId {
        let dst = self.push_local(
            semantic_ty,
            RuntimeCarrier::Value(RuntimeClass::object_ref(layout)),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::AllocObject { layout },
            },
        );
        dst
    }

    fn push_binary_word(
        &mut self,
        bb: RBlockId,
        op: ArithBinOp,
        lhs: RLocalId,
        rhs: RLocalId,
    ) -> RLocalId {
        let dst = self.push_local(
            TyId::u256(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(word_scalar_class())),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Binary {
                    op: BinOp::Arith(op),
                    lhs,
                    rhs,
                },
            },
        );
        dst
    }

    fn push_bool_binary(
        &mut self,
        bb: RBlockId,
        op: CompBinOp,
        lhs: RLocalId,
        rhs: RLocalId,
    ) -> RLocalId {
        let dst = self.push_local(
            TyId::bool(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(bool_scalar_class())),
            RuntimeLocalRoot::None,
        );
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::Binary {
                    op: BinOp::Comp(op),
                    lhs,
                    rhs,
                },
            },
        );
        dst
    }

    fn push_local(
        &mut self,
        semantic_ty: TyId<'db>,
        carrier: RuntimeCarrier<'db>,
        root: RuntimeLocalRoot<'db>,
    ) -> RLocalId {
        let id = RLocalId::from_u32(self.locals.len() as u32);
        self.locals.push(RLocal {
            semantic_ty,
            carrier,
            root,
        });
        id
    }

    fn push_erased_local(&mut self, semantic_ty: TyId<'db>) -> RLocalId {
        self.push_local(semantic_ty, RuntimeCarrier::Erased, RuntimeLocalRoot::None)
    }

    fn push_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>) {
        self.blocks[bb.index()].stmts.push(stmt);
    }

    fn new_block(&mut self) -> RBlockId {
        let id = RBlockId::from_u32(self.blocks.len() as u32);
        self.blocks.push(RBlock {
            stmts: Vec::new(),
            terminator: RTerminator::Trap,
        });
        id
    }
}

trait RuntimeCarrierExt<'db> {
    fn value_class(&self) -> Option<&RuntimeClass<'db>>;
}

impl<'db> RuntimeCarrierExt<'db> for RuntimeCarrier<'db> {
    fn value_class(&self) -> Option<&RuntimeClass<'db>> {
        match self {
            RuntimeCarrier::Erased => None,
            RuntimeCarrier::Value(class) => Some(class),
        }
    }
}

fn bool_scalar_class<'db>() -> ScalarClass<'db> {
    ScalarClass {
        repr: ScalarRepr::Bool,
        role: ScalarRole::Plain,
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

fn selector_scalar_class<'db>() -> ScalarClass<'db> {
    ScalarClass {
        repr: ScalarRepr::Int {
            bits: 32,
            signed: false,
        },
        role: ScalarRole::Plain,
    }
}

fn u32_scalar(value: u32) -> ConstScalar {
    ConstScalar::Int {
        bits: 32,
        signed: false,
        words: if value == 0 {
            Vec::new()
        } else {
            value
                .to_be_bytes()
                .into_iter()
                .skip_while(|byte| *byte == 0)
                .collect()
        },
    }
}

fn resolve_sol_validate_payload_end<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ty: TyId<'db>,
    input_ty: TyId<'db>,
    fast: bool,
) -> Option<RuntimeInstance<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let path = if fast {
        "std::abi::sol::validate_payload_end_fast"
    } else {
        "std::abi::sol::validate_payload_end"
    };
    let func = resolve_lib_func_path(db, scope, path)?;
    let key = SemanticInstanceKey::new(
        db,
        BodyOwner::Func(func),
        GenericSubst::new(db, vec![ty, input_ty]),
        hir::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::new(db, scope, assumptions, vec![]),
    );
    Some(runtime_instance_for_semantic(
        db,
        get_or_build_semantic_instance(db, key),
    ))
}

fn has_abi_validate<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ty: TyId<'db>,
) -> bool {
    let Some(abi_ty) = sol_abi_ty(db, scope) else {
        return false;
    };
    let Some(validate_trait) = resolve_core_trait(db, scope, &["abi", "AbiValidate"]) else {
        return false;
    };
    let assumptions = PredicateListId::empty_list(db);
    let inst = TraitInstId::new_simple(db, validate_trait, vec![ty, abi_ty]);
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    matches!(
        is_goal_satisfiable(db, solve_cx, inst),
        GoalSatisfiability::Satisfied(_)
    )
}

fn resolve_sol_decoder_new<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Option<RuntimeInstance<'db>> {
    let abi_ty = sol_abi_ty(db, scope)?;
    let input_ty = memory_bytes_ty(db, scope)?;
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])?;
    let inst = TraitInstId::new_simple(db, abi_trait, vec![abi_ty]);
    resolve_trait_runtime_instance(db, scope, inst, "decoder_new", vec![input_ty]).ok()
}

fn resolve_sol_encode_single_root_alloc<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<RuntimeInstance<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let func = resolve_lib_func_path(db, scope, "core::abi::encode_single_root_alloc")?;
    let abi_ty = sol_abi_ty(db, scope)?;
    let key = SemanticInstanceKey::new(
        db,
        BodyOwner::Func(func),
        GenericSubst::new(db, vec![abi_ty, ty]),
        hir::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::new(db, scope, assumptions, vec![]),
    );
    Some(runtime_instance_for_semantic(
        db,
        get_or_build_semantic_instance(db, key),
    ))
}

fn resolve_trait_runtime_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    inst: TraitInstId<'db>,
    method: &str,
    extra_generic_args: Vec<TyId<'db>>,
) -> Result<RuntimeInstance<'db>, ()> {
    let assumptions = PredicateListId::empty_list(db);
    let (func, mut impl_args) = resolve_trait_method_instance(
        db,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
        inst,
        IdentId::new(db, method.to_string()),
    )
    .ok_or(())?;
    impl_args.extend(extra_generic_args);
    let key = SemanticInstanceKey::new(
        db,
        BodyOwner::Func(func),
        GenericSubst::new(db, impl_args),
        hir::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::new(db, scope, assumptions, vec![inst]),
    );
    Ok(runtime_instance_for_semantic(
        db,
        get_or_build_semantic_instance(db, key),
    ))
}

fn sol_abi_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Option<TyId<'db>> {
    resolve_lib_type_path(db, scope, "std::abi::Sol")
}

fn calldata_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Option<TyId<'db>> {
    resolve_lib_type_path(db, scope, "std::evm::calldata::CallData")
        .or_else(|| resolve_lib_type_path(db, scope, "std::evm::CallData"))
}

fn memory_bytes_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Option<TyId<'db>> {
    resolve_lib_type_path(db, scope, "std::evm::memory_input::MemoryBytes")
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use url::Url;

    use crate::{
        build_test_runtime_package,
        runtime::{RuntimeBuiltin, RuntimeFunctionOwner, RuntimeSyntheticSpec},
    };

    #[test]
    fn test_roots_materialize_target_root_providers() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(
            std::env::temp_dir().join("synthetic_target_root_provider_test_root.fe"),
        )
        .expect("fixture path should be absolute");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
use std::evm::RawMem

#[test]
fn test_raw_mem_root() uses (mem: mut RawMem) {
    mem.mstore(addr: 0x80, value: 1)
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
        let package = build_test_runtime_package(&db, top_mod, Some("test_raw_mem_root"))
            .expect("test runtime package should build");
        let functions = package.functions(&db);
        let root = functions
            .iter()
            .find(|function| {
                matches!(
                    function.owner(&db),
                    RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::TestRoot { ref name, .. })
                        if name == "test_raw_mem_root"
                )
            })
            .expect("expected synthetic test root");
        let body = root.instance(&db).body(&db);
        assert!(
            body.blocks[0].stmts.iter().any(|stmt| matches!(
                stmt,
                RStmt::Assign {
                    expr: RExpr::Builtin(RuntimeBuiltin::Malloc { .. }) | RExpr::AllocObject { .. },
                    ..
                }
            )),
            "expected synthetic test root to materialize target root providers: {body:#?}"
        );
        assert!(
            !body.blocks[0].stmts.iter().any(|stmt| matches!(
                stmt,
                RStmt::Assign {
                    expr: RExpr::Placeholder {
                        class: RuntimeClass::RawAddr {
                            space: AddressSpaceKind::Memory,
                            target: Some(_),
                        }
                    },
                    ..
                }
            )),
            "synthetic test root should not seed aggregate memory effect params with a null raw pointer: {body:#?}"
        );
    }
}
