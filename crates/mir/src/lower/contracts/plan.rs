use hir::{
    analysis::{
        diagnostics::SpannedHirAnalysisDb,
        ty::{ty_check, ty_def::TyId},
    },
    hir_def::{Contract, ContractInit},
    semantic::{ArgBinding, EffectBinding, EffectSource},
};
use num_bigint::BigUint;

use crate::ir::SyntheticId;

use super::{
    ContractLoweringConfig, MirLowerError, MirLowerResult, diagnostics, target::TargetContext,
};

#[derive(Clone)]
pub struct ContractPlan<'db> {
    pub contract: Contract<'db>,
    pub display_name: String,
    pub fields: Vec<FieldPlan<'db>>,
    pub functions: Vec<SyntheticFnPlan<'db>>,
}

#[derive(Clone)]
pub struct FieldPlan<'db> {
    pub index: usize,
    pub slot: BigUint,
    pub declared_ty: TyId<'db>,
    #[allow(dead_code)]
    pub target_ty: TyId<'db>,
    pub is_provider: bool,
}

#[derive(Clone)]
pub enum SyntheticFnPlan<'db> {
    InitHandler(InitHandlerPlan<'db>),
    RecvHandler(RecvHandlerPlan<'db>),
    InitEntrypoint(InitEntrypointPlan<'db>),
    RuntimeEntrypoint(RuntimeEntrypointPlan<'db>),
    CodeRegionQuery(CodeRegionQueryPlan<'db>),
}

impl<'db> SyntheticFnPlan<'db> {
    pub fn id(&self) -> SyntheticId<'db> {
        match self {
            Self::InitHandler(plan) => plan.id,
            Self::RecvHandler(plan) => plan.id,
            Self::InitEntrypoint(plan) => plan.id,
            Self::RuntimeEntrypoint(plan) => plan.id,
            Self::CodeRegionQuery(plan) => plan.id,
        }
    }

    pub fn always_defer_root(&self) -> bool {
        matches!(self, Self::CodeRegionQuery(_))
    }
}

#[derive(Clone)]
pub struct InitHandlerPlan<'db> {
    pub id: SyntheticId<'db>,
    pub init: ContractInit<'db>,
    pub body: hir::hir_def::Body<'db>,
    pub typed_body: ty_check::TypedBody<'db>,
    pub source_param_tys: Vec<TyId<'db>>,
    pub effect_bindings: Vec<EffectBinding<'db>>,
}

#[derive(Clone)]
pub struct RecvHandlerPlan<'db> {
    pub id: SyntheticId<'db>,
    pub body: hir::hir_def::Body<'db>,
    pub typed_body: ty_check::TypedBody<'db>,
    pub args_ty: TyId<'db>,
    pub ret_ty: TyId<'db>,
    pub arg_bindings: Vec<ArgBinding<'db>>,
    pub effect_bindings: Vec<EffectBinding<'db>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FieldBindingMode {
    Init,
    Runtime,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DefaultAction {
    Abort,
}

#[derive(Clone, Copy)]
pub enum InitArgsPlan<'db> {
    Empty,
    DecodeInitTailTupleElements { tuple_ty: TyId<'db> },
}

#[derive(Clone, Copy)]
pub enum RuntimeArgsPlan<'db> {
    Empty,
    DecodeRuntimeInput { ty: TyId<'db> },
}

#[derive(Clone, Copy)]
pub enum RuntimeReturnPlan<'db> {
    Unit,
    Value { ty: TyId<'db> },
}

#[derive(Clone, Copy)]
pub enum InitFinishPlan<'db> {
    ReturnCodeRegion { target: SyntheticId<'db> },
}

#[derive(Clone)]
pub struct InitCallPlan<'db> {
    pub callee: SyntheticId<'db>,
    pub args: InitArgsPlan<'db>,
    pub effects: Vec<EffectSource>,
}

#[derive(Clone)]
pub struct RuntimeCallPlan<'db> {
    pub callee: SyntheticId<'db>,
    pub args: RuntimeArgsPlan<'db>,
    pub effects: Vec<EffectSource>,
}

#[derive(Clone)]
pub struct InitEntrypointPlan<'db> {
    pub id: SyntheticId<'db>,
    pub field_mode: FieldBindingMode,
    pub is_payable: bool,
    pub init_call: Option<InitCallPlan<'db>>,
    pub finish: InitFinishPlan<'db>,
}

#[derive(Clone)]
pub struct RuntimeDispatchArmPlan<'db> {
    #[allow(dead_code)]
    pub recv_idx: u32,
    #[allow(dead_code)]
    pub arm_idx: u32,
    pub is_payable: bool,
    pub selector: u32,
    pub call: RuntimeCallPlan<'db>,
    pub ret: RuntimeReturnPlan<'db>,
}

#[derive(Clone)]
pub struct RuntimeEntrypointPlan<'db> {
    pub id: SyntheticId<'db>,
    pub field_mode: FieldBindingMode,
    pub arms: Vec<RuntimeDispatchArmPlan<'db>>,
    pub default: DefaultAction,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CodeRegionQueryKind {
    Offset,
    Len,
}

#[derive(Clone, Copy)]
pub struct CodeRegionQueryPlan<'db> {
    pub id: SyntheticId<'db>,
    pub target: SyntheticId<'db>,
    pub kind: CodeRegionQueryKind,
}

impl<'db> ContractPlan<'db> {
    pub(super) fn build(
        db: &'db dyn SpannedHirAnalysisDb,
        target: &TargetContext<'db>,
        contract: Contract<'db>,
        config: &ContractLoweringConfig<'_>,
    ) -> MirLowerResult<Self> {
        let display_name = contract_display_name(db, contract, config.ingot_prefix);
        let fields = contract
            .field_layout(db)
            .values()
            .map(|field| FieldPlan {
                index: field.index as usize,
                slot: BigUint::from(field.slot_offset),
                declared_ty: field.declared_ty,
                target_ty: field.target_ty,
                is_provider: field.is_provider,
            })
            .collect::<Vec<_>>();

        let mut functions = Vec::new();
        if let Some(init) = contract.init(db) {
            let body = init.body(db);
            let (diags, typed_body) = ty_check::check_contract_init_body(db, contract);
            if !diags.is_empty() {
                return Err(MirLowerError::AnalysisDiagnostics {
                    func_name: format!("contract `{display_name}` init"),
                    diagnostics: diagnostics::format_func_body_diags(db, diags),
                });
            }
            let effect_bindings = contract
                .init_effect_env(db)
                .map(|env| env.bindings(db).to_vec())
                .unwrap_or_default();
            functions.push(SyntheticFnPlan::InitHandler(InitHandlerPlan {
                id: SyntheticId::ContractInitHandler(contract),
                init,
                body,
                typed_body: typed_body.clone(),
                source_param_tys: contract.init_args_ty(db).field_types(db).to_vec(),
                effect_bindings,
            }));
        }

        for recv in contract.recv_views(db) {
            for arm in recv.arms(db) {
                let recv_idx = recv.index(db);
                let arm_idx = arm.index(db);
                let Some(hir_arm) = arm.arm(db) else {
                    return Err(MirLowerError::Unsupported {
                        func_name: "<contract lowering>".into(),
                        message: format!(
                            "missing recv arm body for contract `{display_name}` recv={recv_idx} arm={arm_idx}"
                        ),
                    });
                };
                let (diags, typed_body) =
                    ty_check::check_contract_recv_arm_body(db, contract, recv_idx, arm_idx);
                if !diags.is_empty() {
                    return Err(MirLowerError::AnalysisDiagnostics {
                        func_name: format!(
                            "contract `{display_name}` recv arm {recv_idx}:{arm_idx}"
                        ),
                        diagnostics: diagnostics::format_func_body_diags(db, diags),
                    });
                }

                let abi_info = arm.abi_info(db, target.abi.abi_ty);
                functions.push(SyntheticFnPlan::RecvHandler(RecvHandlerPlan {
                    id: SyntheticId::ContractRecvArmHandler {
                        contract,
                        recv_idx,
                        arm_idx,
                    },
                    body: hir_arm.body,
                    typed_body: typed_body.clone(),
                    args_ty: abi_info.args_ty,
                    ret_ty: abi_info.ret_ty.unwrap_or_else(|| TyId::unit(db)),
                    arg_bindings: arm.arg_bindings(db).clone(),
                    effect_bindings: arm.effective_effect_env(db).bindings(db).to_vec(),
                }));
            }
        }

        functions.push(SyntheticFnPlan::InitEntrypoint(InitEntrypointPlan {
            id: SyntheticId::ContractInitEntrypoint(contract),
            field_mode: FieldBindingMode::Init,
            is_payable: contract.init(db).is_some_and(|init| init.is_payable(db)),
            init_call: contract.init_effect_env(db).map(|env| InitCallPlan {
                callee: SyntheticId::ContractInitHandler(contract),
                args: if abi_payload_is_empty(db, contract.init_args_ty(db)) {
                    InitArgsPlan::Empty
                } else {
                    InitArgsPlan::DecodeInitTailTupleElements {
                        tuple_ty: contract.init_args_ty(db),
                    }
                },
                effects: env
                    .bindings(db)
                    .iter()
                    .map(|binding| binding.source)
                    .collect(),
            }),
            finish: InitFinishPlan::ReturnCodeRegion {
                target: SyntheticId::ContractRuntimeEntrypoint(contract),
            },
        }));

        let mut arms = Vec::new();
        for recv in contract.recv_views(db) {
            for arm in recv.arms(db) {
                let abi_info = arm.abi_info(db, target.abi.abi_ty);
                let selector =
                    abi_info
                        .selector_value
                        .ok_or_else(|| MirLowerError::Unsupported {
                            func_name: "<contract lowering>".into(),
                            message: format!(
                                "contract `{display_name}` recv arm {}:{} has no resolved selector",
                                recv.index(db),
                                arm.index(db)
                            ),
                        })?;
                arms.push(RuntimeDispatchArmPlan {
                    recv_idx: recv.index(db),
                    arm_idx: arm.index(db),
                    is_payable: arm.arm(db).is_some_and(|a| a.is_payable(db)),
                    selector,
                    call: RuntimeCallPlan {
                        callee: SyntheticId::ContractRecvArmHandler {
                            contract,
                            recv_idx: recv.index(db),
                            arm_idx: arm.index(db),
                        },
                        args: if abi_payload_is_empty(db, abi_info.args_ty) {
                            RuntimeArgsPlan::Empty
                        } else {
                            RuntimeArgsPlan::DecodeRuntimeInput {
                                ty: abi_info.args_ty,
                            }
                        },
                        effects: arm
                            .effective_effect_env(db)
                            .bindings(db)
                            .iter()
                            .map(|binding| binding.source)
                            .collect(),
                    },
                    ret: abi_info.ret_ty.map_or(RuntimeReturnPlan::Unit, |ty| {
                        RuntimeReturnPlan::Value { ty }
                    }),
                });
            }
        }
        functions.push(SyntheticFnPlan::RuntimeEntrypoint(RuntimeEntrypointPlan {
            id: SyntheticId::ContractRuntimeEntrypoint(contract),
            field_mode: FieldBindingMode::Runtime,
            arms,
            default: DefaultAction::Abort,
        }));
        functions.push(SyntheticFnPlan::CodeRegionQuery(CodeRegionQueryPlan {
            id: SyntheticId::ContractInitCodeOffset(contract),
            target: SyntheticId::ContractInitEntrypoint(contract),
            kind: CodeRegionQueryKind::Offset,
        }));
        functions.push(SyntheticFnPlan::CodeRegionQuery(CodeRegionQueryPlan {
            id: SyntheticId::ContractInitCodeLen(contract),
            target: SyntheticId::ContractInitEntrypoint(contract),
            kind: CodeRegionQueryKind::Len,
        }));

        Ok(Self {
            contract,
            display_name,
            fields,
            functions,
        })
    }
}

fn abi_payload_is_empty(db: &dyn hir::analysis::HirAnalysisDb, ty: TyId<'_>) -> bool {
    crate::layout::is_zero_sized_ty(db, ty)
}

fn contract_display_name(
    db: &dyn hir::analysis::HirAnalysisDb,
    contract: Contract<'_>,
    ingot_prefix: Option<&str>,
) -> String {
    let bare_name = contract
        .name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous_contract>".to_string());
    match ingot_prefix {
        Some(prefix) => format!("{prefix}__{bare_name}"),
        None => bare_name,
    }
}
