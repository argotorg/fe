//! Contract lowering to MIR-synthetic entrypoints and handlers.
//!
//! This is the implementation of `contract_lowering_target_architecture.md`'s contract lowering
//! pipeline: HIR remains purely syntactic, while typed contract elaboration drives MIR generation.

use common::{indexmap::IndexMap, ingot::IngotKind};
use hir::{
    analysis::ty::{
        ty_check::{LocalBinding, ParamSite},
        ty_def::{TyBase, TyData},
    },
    hir_def::{
        CallableDef, Contract, IdentId, Partial, PathId, TopLevelMod,
        expr::{ArithBinOp, BinOp, CompBinOp},
    },
};
use hir::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{PathRes, resolve_path},
        ty::{
            corelib::resolve_core_trait, normalize::normalize_ty, trait_def::TraitInstId,
            trait_resolution::PredicateListId, ty_def::TyId,
        },
    },
    hir_def::{Func, Trait, scope_graph::ScopeId},
    semantic::{ContractFieldInfo, EffectBinding, EffectSource, RecvArmView},
};
use num_bigint::BigUint;
use rustc_hash::FxHashMap;

use crate::{
    core_lib::CoreLib,
    ir::{
        AddressSpaceKind, BodyBuilder, CallOrigin, CodeRegionRoot, ContractFunction,
        ContractFunctionKind, HirCallTarget, IntrinsicOp, MirFunction, MirFunctionOrigin, Rvalue,
        SwitchTarget, SwitchValue, SyntheticId, TerminatingCall, Terminator, ValueId, ValueOrigin,
        ValueRepr,
    },
    layout, repr,
};

use super::{MirBuilder, MirLowerError, MirLowerResult, diagnostics};

pub(super) fn lower_contract_templates<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> MirLowerResult<Vec<MirFunction<'db>>> {
    let mut out = Vec::new();

    let target = TargetContext::new(db, top_mod)?;

    for &contract in top_mod.all_contracts(db) {
        let symbols = ContractSymbols::new(db, contract);

        let core_lib = CoreLib::new(db, contract.scope());

        let fields = contract.field_infos(db);
        let slot_offsets = compute_field_slot_offsets(db, &core_lib, fields)?;

        // User-body handlers first (so entrypoints can call them by symbol).
        if contract.init(db).is_some() {
            out.push(lower_init_handler(
                db, contract, fields, &symbols, &core_lib,
            )?);
        }
        for recv in contract.recv_views(db) {
            for arm in recv.arms(db) {
                out.push(lower_recv_arm_handler(
                    db,
                    contract,
                    fields,
                    arm,
                    target.abi.abi_ty,
                    &symbols,
                    &core_lib,
                )?);
            }
        }

        // Entrypoints + metadata hooks.
        out.push(lower_init_entrypoint(
            db,
            contract,
            fields,
            &target,
            &symbols,
            &slot_offsets,
        )?);
        out.push(lower_runtime_entrypoint(
            db,
            contract,
            fields,
            &target,
            &symbols,
            &slot_offsets,
        )?);
        out.push(lower_init_code_offset(db, contract, &symbols)?);
        out.push(lower_init_code_len(db, contract, &symbols)?);
    }

    Ok(out)
}

struct ContractSymbols {
    contract_name: String,
    init_entrypoint: String,
    runtime_entrypoint: String,
    init_handler: String,
    init_code_offset: String,
    init_code_len: String,
}

impl ContractSymbols {
    fn new(db: &dyn HirAnalysisDb, contract: Contract<'_>) -> Self {
        let contract_name = contract
            .name(db)
            .to_opt()
            .map(|id| id.data(db).to_string())
            .unwrap_or_else(|| "<anonymous_contract>".to_string());
        let init_entrypoint = format!("__{contract_name}_init");
        let runtime_entrypoint = format!("__{contract_name}_runtime");
        let init_handler = format!("__{contract_name}_init_contract");
        let init_code_offset = format!("__{contract_name}_init_code_offset");
        let init_code_len = format!("__{contract_name}_init_code_len");
        Self {
            contract_name,
            init_entrypoint,
            runtime_entrypoint,
            init_handler,
            init_code_offset,
            init_code_len,
        }
    }

    fn recv_handler(&self, recv_idx: u32, arm_idx: u32) -> String {
        format!("__{}_recv_{}_{}", self.contract_name, recv_idx, arm_idx)
    }
}

struct TargetContext<'db> {
    host: TargetHostContext<'db>,
    abi: AbiContext<'db>,
}

struct TargetHostContext<'db> {
    root_effect_ty: TyId<'db>,
    contract_host_inst: TraitInstId<'db>,
    init_input_ty: TyId<'db>,
    input_ty: TyId<'db>,
    alloc_fn: Func<'db>,
    effect_ref_trait: Trait<'db>,
    effect_ref_from_raw_fn: Func<'db>,
    field_fn: Func<'db>,
    init_field_fn: Func<'db>,
    runtime_selector_fn: Func<'db>,
    runtime_decoder_fn: Func<'db>,
    return_value_fn: Func<'db>,
    return_unit_fn: Func<'db>,
    abort_fn: Func<'db>,
}

struct AbiContext<'db> {
    abi_ty: TyId<'db>,
    abi_inst: TraitInstId<'db>,
    selector_ty: TyId<'db>,
    init_decoder_ty: TyId<'db>,
    runtime_decoder_ty: TyId<'db>,
    abi_decoder_new: Func<'db>,
    decode_trait: Trait<'db>,
    decode_decode: Func<'db>,
}

const EVM_TARGET_TY_PATH: &[&str] = &["std", "evm", "EvmTarget"];
const EVM_ALLOC_FUNC_PATH: &[&str] = &["std", "evm", "mem", "alloc"];

#[derive(Clone, Copy)]
struct TargetSpec {
    target_ty_path: &'static [&'static str],
    alloc_func_path: &'static [&'static str],
}

impl TargetSpec {
    const EVM: Self = Self {
        target_ty_path: EVM_TARGET_TY_PATH,
        alloc_func_path: EVM_ALLOC_FUNC_PATH,
    };
}

impl<'db> TargetHostContext<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        spec: TargetSpec,
    ) -> MirLowerResult<(Self, TyId<'db>)> {
        let target_ty = resolve_ty_path(db, top_mod, scope, spec.target_ty_path)?;
        let target_trait = resolve_core_trait(db, scope, &["contracts", "Target"]);
        let inst_target = TraitInstId::new(db, target_trait, vec![target_ty], IndexMap::new());
        let root_effect_ty = resolve_assoc_ty(db, inst_target, scope, assumptions, "RootEffect");
        let default_abi_ty = resolve_assoc_ty(db, inst_target, scope, assumptions, "DefaultAbi");

        let contract_host_trait = resolve_core_trait(db, scope, &["contracts", "ContractHost"]);
        let contract_host_inst = TraitInstId::new(
            db,
            contract_host_trait,
            vec![root_effect_ty],
            IndexMap::new(),
        );

        let init_input_ty =
            resolve_assoc_ty(db, contract_host_inst, scope, assumptions, "InitInput");
        let input_ty = resolve_assoc_ty(db, contract_host_inst, scope, assumptions, "Input");

        let effect_ref_trait = resolve_core_trait(db, scope, &["effect_ref", "EffectRef"]);
        let effect_ref_from_raw = require_trait_method(db, effect_ref_trait, "from_raw")?;

        let alloc_func = resolve_value_func_path(db, top_mod, scope, spec.alloc_func_path)?;

        let host_field = require_trait_method(db, contract_host_trait, "field")?;
        let host_init_field = require_trait_method(db, contract_host_trait, "init_field")?;
        let host_runtime_selector =
            require_trait_method(db, contract_host_trait, "runtime_selector")?;
        let host_runtime_decoder =
            require_trait_method(db, contract_host_trait, "runtime_decoder")?;
        let host_return_value = require_trait_method(db, contract_host_trait, "return_value")?;
        let host_return_unit = require_trait_method(db, contract_host_trait, "return_unit")?;
        let host_abort = require_trait_method(db, contract_host_trait, "abort")?;

        let host = Self {
            root_effect_ty,
            contract_host_inst,
            init_input_ty,
            input_ty,
            alloc_fn: alloc_func,
            effect_ref_trait,
            effect_ref_from_raw_fn: effect_ref_from_raw,
            field_fn: host_field,
            init_field_fn: host_init_field,
            runtime_selector_fn: host_runtime_selector,
            runtime_decoder_fn: host_runtime_decoder,
            return_value_fn: host_return_value,
            return_unit_fn: host_return_unit,
            abort_fn: host_abort,
        };

        Ok((host, default_abi_ty))
    }
}

impl<'db> TargetContext<'db> {
    fn new(db: &'db dyn HirAnalysisDb, top_mod: TopLevelMod<'db>) -> MirLowerResult<Self> {
        Self::for_target(db, top_mod, TargetSpec::EVM)
    }

    fn for_target(
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        spec: TargetSpec,
    ) -> MirLowerResult<Self> {
        let scope = top_mod.scope();
        let assumptions = PredicateListId::empty_list(db);

        let (host, default_abi_ty) = TargetHostContext::new(db, top_mod, scope, assumptions, spec)?;

        let abi = AbiContext::new(db, scope, assumptions, default_abi_ty, &host)?;

        Ok(Self { host, abi })
    }
}

impl<'db> AbiContext<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        abi_ty: TyId<'db>,
        host: &TargetHostContext<'db>,
    ) -> MirLowerResult<Self> {
        let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"]);
        let abi_inst = TraitInstId::new(db, abi_trait, vec![abi_ty], IndexMap::new());
        let selector_ty = resolve_assoc_ty(db, abi_inst, scope, assumptions, "Selector");

        let decoder_ctor = resolve_assoc_ty(db, abi_inst, scope, assumptions, "Decoder");
        let init_decoder_ty = normalize_ty(
            db,
            TyId::app(db, decoder_ctor, host.init_input_ty),
            scope,
            assumptions,
        );
        let runtime_decoder_ty = normalize_ty(
            db,
            TyId::app(db, decoder_ctor, host.input_ty),
            scope,
            assumptions,
        );

        let abi_decoder_new = require_trait_method(db, abi_trait, "decoder_new")?;
        let decode_trait = resolve_core_trait(db, scope, &["abi", "Decode"]);
        let decode_decode = require_trait_method(db, decode_trait, "decode")?;

        Ok(Self {
            abi_ty,
            abi_inst,
            selector_ty,
            init_decoder_ty,
            runtime_decoder_ty,
            abi_decoder_new,
            decode_trait,
            decode_decode,
        })
    }
}

struct ContractMirCx<'db, 'a> {
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    host: &'a TargetHostContext<'db>,
    abi: &'a AbiContext<'db>,
}

impl<'db, 'a> ContractMirCx<'db, 'a> {
    fn new(
        db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
        target: &'a TargetContext<'db>,
    ) -> Self {
        Self::new_with_abi(db, &target.host, &target.abi)
    }

    fn new_with_abi(
        db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
        host: &'a TargetHostContext<'db>,
        abi: &'a AbiContext<'db>,
    ) -> Self {
        Self { db, host, abi }
    }

    fn call_hir(
        &self,
        callable_def: CallableDef<'db>,
        generic_args: Vec<TyId<'db>>,
        trait_inst: Option<TraitInstId<'db>>,
        args: Vec<ValueId>,
    ) -> CallOrigin<'db> {
        CallOrigin {
            expr: None,
            hir_target: Some(HirCallTarget {
                callable_def,
                generic_args,
                trait_inst,
            }),
            args,
            effect_args: Vec::new(),
            resolved_name: None,
            receiver_space: None,
        }
    }

    fn call_symbol(
        &self,
        symbol_name: impl Into<String>,
        args: Vec<ValueId>,
        effect_args: Vec<ValueId>,
    ) -> CallOrigin<'db> {
        CallOrigin {
            expr: None,
            hir_target: None,
            args,
            effect_args,
            resolved_name: Some(symbol_name.into()),
            receiver_space: None,
        }
    }

    fn host_abort(&self, root_value: ValueId) -> CallOrigin<'db> {
        self.call_hir(
            CallableDef::Func(self.host.abort_fn),
            self.host.contract_host_inst.args(self.db).to_vec(),
            Some(self.host.contract_host_inst),
            vec![root_value],
        )
    }

    fn host_return_unit(&self, root_value: ValueId) -> CallOrigin<'db> {
        self.call_hir(
            CallableDef::Func(self.host.return_unit_fn),
            self.host.contract_host_inst.args(self.db).to_vec(),
            Some(self.host.contract_host_inst),
            vec![root_value],
        )
    }

    fn host_return_value(
        &self,
        root_value: ValueId,
        result_value: ValueId,
        ret_ty: TyId<'db>,
    ) -> CallOrigin<'db> {
        let mut generic_args = self.host.contract_host_inst.args(self.db).to_vec();
        generic_args.push(self.abi.abi_ty);
        generic_args.push(ret_ty);
        self.call_hir(
            CallableDef::Func(self.host.return_value_fn),
            generic_args,
            Some(self.host.contract_host_inst),
            vec![root_value, result_value],
        )
    }

    fn host_runtime_selector(&self, root_value: ValueId) -> CallOrigin<'db> {
        let mut generic_args = self.host.contract_host_inst.args(self.db).to_vec();
        generic_args.push(self.abi.abi_ty);
        self.call_hir(
            CallableDef::Func(self.host.runtime_selector_fn),
            generic_args,
            Some(self.host.contract_host_inst),
            vec![root_value],
        )
    }

    fn host_runtime_decoder(&self, root_value: ValueId) -> CallOrigin<'db> {
        let mut generic_args = self.host.contract_host_inst.args(self.db).to_vec();
        generic_args.push(self.abi.abi_ty);
        self.call_hir(
            CallableDef::Func(self.host.runtime_decoder_fn),
            generic_args,
            Some(self.host.contract_host_inst),
            vec![root_value],
        )
    }

    fn alloc(&self, len: ValueId) -> CallOrigin<'db> {
        self.call_hir(
            CallableDef::Func(self.host.alloc_fn),
            Vec::new(),
            None,
            vec![len],
        )
    }

    fn abi_decoder_new(&self, input_value: ValueId, input_ty: TyId<'db>) -> CallOrigin<'db> {
        let mut generic_args = self.abi.abi_inst.args(self.db).to_vec();
        generic_args.push(input_ty);
        self.call_hir(
            CallableDef::Func(self.abi.abi_decoder_new),
            generic_args,
            Some(self.abi.abi_inst),
            vec![input_value],
        )
    }

    fn decode_decode(
        &self,
        decoder_value: ValueId,
        decoder_ty: TyId<'db>,
        target_ty: TyId<'db>,
    ) -> CallOrigin<'db> {
        let inst = TraitInstId::new(
            self.db,
            self.abi.decode_trait,
            vec![target_ty, self.abi.abi_ty],
            IndexMap::new(),
        );
        let mut generic_args = inst.args(self.db).to_vec();
        generic_args.push(decoder_ty);
        self.call_hir(
            CallableDef::Func(self.abi.decode_decode),
            generic_args,
            Some(inst),
            vec![decoder_value],
        )
    }

    fn field_value_call(
        &self,
        host_field_func: Func<'db>,
        root_value: ValueId,
        slot_value: ValueId,
        declared_ty: TyId<'db>,
        is_provider: bool,
    ) -> CallOrigin<'db> {
        if is_provider {
            let inst = TraitInstId::new(
                self.db,
                self.host.effect_ref_trait,
                vec![declared_ty],
                IndexMap::new(),
            );
            return self.call_hir(
                CallableDef::Func(self.host.effect_ref_from_raw_fn),
                inst.args(self.db).to_vec(),
                Some(inst),
                vec![slot_value],
            );
        }

        let mut generic_args = self.host.contract_host_inst.args(self.db).to_vec();
        generic_args.push(declared_ty);
        self.call_hir(
            CallableDef::Func(host_field_func),
            generic_args,
            Some(self.host.contract_host_inst),
            vec![root_value, slot_value],
        )
    }

    fn emit_field_providers(
        &self,
        builder: &mut BodyBuilder<'db>,
        fields: &[ContractFieldInfo<'db>],
        slot_offsets: &[BigUint],
        root_value: ValueId,
        host_field_func: Func<'db>,
    ) -> Vec<ValueId> {
        let u256_ty = TyId::u256(self.db);
        let mut field_values = Vec::with_capacity(fields.len());
        for (idx, field) in fields.iter().enumerate() {
            let slot = slot_offsets
                .get(idx)
                .cloned()
                .unwrap_or_else(|| BigUint::from(0u8));
            let slot_value = builder.const_int_value(u256_ty, slot);
            let call = self.field_value_call(
                host_field_func,
                root_value,
                slot_value,
                field.declared_ty,
                field.is_provider,
            );
            let field_value = builder
                .assign_to_new_local(
                    format!("field{idx}"),
                    u256_ty,
                    true,
                    AddressSpaceKind::Memory,
                    Rvalue::Call(call),
                )
                .value;
            field_values.push(field_value);
        }
        field_values
    }

    fn effect_args_from_sources(
        &self,
        effects: &[EffectBinding<'db>],
        zero_u256: ValueId,
        field_values: &[ValueId],
    ) -> Vec<ValueId> {
        let mut out = Vec::with_capacity(effects.len());
        for effect in effects {
            match effect.source {
                EffectSource::Root => out.push(zero_u256),
                EffectSource::Field(field_idx) => out.push(
                    field_values
                        .get(field_idx as usize)
                        .copied()
                        .unwrap_or(zero_u256),
                ),
            }
        }
        out
    }

    fn emit_decode_or_unit(
        &self,
        builder: &mut BodyBuilder<'db>,
        decoder_value: ValueId,
        decoder_ty: TyId<'db>,
        target_ty: TyId<'db>,
        name_hint: impl Into<String>,
    ) -> ValueId {
        if layout::is_zero_sized_ty(self.db, target_ty) {
            builder.assign(
                None,
                Rvalue::Call(self.decode_decode(decoder_value, decoder_ty, target_ty)),
            );
            return builder.unit_value(target_ty);
        }

        builder
            .assign_to_new_local(
                name_hint,
                target_ty,
                false,
                AddressSpaceKind::Memory,
                Rvalue::Call(self.decode_decode(decoder_value, decoder_ty, target_ty)),
            )
            .value
    }
}

fn resolve_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    name: &str,
) -> TyId<'db> {
    let ident = IdentId::new(db, name.to_owned());
    normalize_ty(db, TyId::assoc_ty(db, inst, ident), scope, assumptions)
}

fn require_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_def: Trait<'db>,
    name: &str,
) -> MirLowerResult<Func<'db>> {
    let name_id = IdentId::new(db, name.to_string());
    trait_def
        .methods(db)
        .find(|func| func.name(db).to_opt() == Some(name_id))
        .ok_or_else(|| MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "missing core trait method `{}` on `{}`",
                name,
                trait_def
                    .name(db)
                    .to_opt()
                    .map(|id| id.data(db).to_string())
                    .unwrap_or_else(|| "<anonymous trait>".into())
            ),
        })
}

fn resolve_ty_path<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    scope: ScopeId<'db>,
    segments: &[&str],
) -> MirLowerResult<TyId<'db>> {
    let path = path_from_segments(db, top_mod, segments);
    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => Ok(ty),
        Ok(other) => Err(MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "expected type at path `{}` but resolved to `{}`",
                path.pretty_print(db),
                other.kind_name()
            ),
        }),
        Err(err) => Err(MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "failed to resolve type path `{}`: {err:?}",
                path.pretty_print(db)
            ),
        }),
    }
}

fn resolve_value_func_path<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    scope: ScopeId<'db>,
    segments: &[&str],
) -> MirLowerResult<Func<'db>> {
    let path = path_from_segments(db, top_mod, segments);
    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, true) {
        Ok(PathRes::Func(ty)) => match ty.base_ty(db).data(db) {
            TyData::TyBase(TyBase::Func(CallableDef::Func(func))) => Ok(*func),
            _ => Err(MirLowerError::Unsupported {
                func_name: "<contract lowering>".into(),
                message: format!(
                    "expected function at path `{}` but got `{}`",
                    path.pretty_print(db),
                    ty.pretty_print(db)
                ),
            }),
        },
        Ok(other) => Err(MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "expected function at path `{}` but resolved to `{}`",
                path.pretty_print(db),
                other.kind_name()
            ),
        }),
        Err(err) => Err(MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "failed to resolve function path `{}`: {err:?}",
                path.pretty_print(db)
            ),
        }),
    }
}

fn path_from_segments<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    segments: &[&str],
) -> PathId<'db> {
    let ingot_kind = top_mod.ingot(db).kind(db);
    let mut iter = segments.iter();
    let root = iter
        .next()
        .unwrap_or_else(|| panic!("expected non-empty path segments"));
    let root_ident = match (*root, ingot_kind) {
        ("core", IngotKind::Core) => IdentId::make_ingot(db),
        ("core", _) => IdentId::make_core(db),
        ("std", IngotKind::Std) => IdentId::make_ingot(db),
        ("std", _) => IdentId::new(db, "std".to_string()),
        (other, _) => IdentId::new(db, other.to_string()),
    };
    let mut path = PathId::from_ident(db, root_ident);
    for seg in iter {
        path = path.push_ident(db, IdentId::new(db, (*seg).to_string()));
    }
    path
}

fn compute_field_slot_offsets<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    fields: &[ContractFieldInfo<'db>],
) -> MirLowerResult<Vec<BigUint>> {
    let mut next_offset: FxHashMap<AddressSpaceKind, usize> = FxHashMap::default();
    let mut out = Vec::with_capacity(fields.len());

    for field in fields {
        let space = if field.is_provider {
            repr::effect_provider_space_for_ty(db, core, field.declared_ty)
                .unwrap_or(AddressSpaceKind::Storage)
        } else {
            AddressSpaceKind::Storage
        };

        let offset = *next_offset.get(&space).unwrap_or(&0);
        out.push(BigUint::from(offset));

        let slots = crate::layout::ty_storage_slots(db, field.target_ty).ok_or_else(|| {
            MirLowerError::Unsupported {
                func_name: "<contract lowering>".into(),
                message: format!(
                    "failed to compute storage slots for `{}`",
                    field.target_ty.pretty_print(db)
                ),
            }
        })?;
        *next_offset.entry(space).or_insert(0) += slots;
    }

    Ok(out)
}

fn lower_init_handler<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    fields: &[ContractFieldInfo<'db>],
    symbols: &ContractSymbols,
    core: &CoreLib<'db>,
) -> MirLowerResult<MirFunction<'db>> {
    let init = contract
        .init(db)
        .expect("contract init handler requested without init");
    let body = init.body(db);
    let (diags, typed_body) = hir::analysis::ty::ty_check::check_contract_init_body(db, contract);
    if !diags.is_empty() {
        let rendered = diagnostics::format_func_body_diags(db, diags);
        return Err(MirLowerError::AnalysisDiagnostics {
            func_name: format!("contract `{}` init", symbols.contract_name),
            diagnostics: rendered,
        });
    }

    let mut builder = MirBuilder::new_for_body_owner(
        db,
        hir::analysis::ty::ty_check::BodyOwner::ContractInit { contract },
        body,
        typed_body,
        &[],
        TyId::unit(db),
    )?;

    // Seed explicit value params.
    for (idx, param) in init.params(db).data(db).iter().enumerate() {
        let binding = builder
            .typed_body
            .param_binding(idx)
            .unwrap_or(LocalBinding::Param {
                site: ParamSite::ContractInit(contract),
                idx,
                ty: TyId::invalid(db, hir::analysis::ty::ty_def::InvalidCause::Other),
                is_mut: param.is_mut,
            });
        let name = param
            .name()
            .map(|ident| ident.data(db).to_string())
            .unwrap_or_else(|| format!("arg{idx}"));
        let ty = match binding {
            LocalBinding::Param { ty, .. } => ty,
            _ => TyId::invalid(db, hir::analysis::ty::ty_def::InvalidCause::Other),
        };
        builder.seed_synthetic_param_local(name, ty, binding.is_mut(), Some(binding));
    }

    let init_env = contract
        .init_effect_env(db)
        .expect("contract init handler requested without init");
    seed_effect_param_locals(db, &mut builder, fields, init_env.bindings(db), core);

    let entry = builder.builder.entry_block();
    builder.move_to_block(entry);
    builder.lower_root(body.expr(db));
    builder.ensure_const_expr_values();
    if let Some(block) = builder.current_block() {
        builder.set_terminator(block, Terminator::Return(None));
    }
    let mir_body = builder.finish();

    if let Some(expr) = super::first_unlowered_expr_used_by_mir(&mir_body) {
        let expr_context = super::format_hir_expr_context(db, body, expr);
        return Err(MirLowerError::UnloweredHirExpr {
            func_name: symbols.init_handler.clone(),
            expr: expr_context,
        });
    }

    Ok(MirFunction {
        origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractInitHandler(contract)),
        body: mir_body,
        typed_body: Some(typed_body.clone()),
        generic_args: Vec::new(),
        ret_ty: TyId::unit(db),
        returns_value: false,
        contract_function: None,
        symbol_name: symbols.init_handler.clone(),
        receiver_space: None,
    })
}

fn lower_recv_arm_handler<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    fields: &[ContractFieldInfo<'db>],
    arm: RecvArmView<'db>,
    abi: TyId<'db>,
    symbols: &ContractSymbols,
    core: &CoreLib<'db>,
) -> MirLowerResult<MirFunction<'db>> {
    let recv_idx = arm.recv(db).index(db);
    let arm_idx = arm.index(db);

    let Some(hir_arm) = arm.arm(db) else {
        return Err(MirLowerError::Unsupported {
            func_name: "<contract lowering>".into(),
            message: format!(
                "missing recv arm body for contract `{}` recv={recv_idx} arm={arm_idx}",
                symbols.contract_name
            ),
        });
    };
    let body = hir_arm.body;

    let (diags, typed_body) =
        hir::analysis::ty::ty_check::check_contract_recv_arm_body(db, contract, recv_idx, arm_idx);
    if !diags.is_empty() {
        let rendered = diagnostics::format_func_body_diags(db, diags);
        return Err(MirLowerError::AnalysisDiagnostics {
            func_name: format!(
                "contract `{}` recv arm {recv_idx}:{arm_idx}",
                symbols.contract_name
            ),
            diagnostics: rendered,
        });
    }

    let abi_info = arm.abi_info(db, abi);
    let args_ty = abi_info.args_ty;
    let ret_ty = abi_info.ret_ty.unwrap_or_else(|| TyId::unit(db));

    let mut builder = MirBuilder::new_for_body_owner(
        db,
        hir::analysis::ty::ty_check::BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        },
        body,
        typed_body,
        &[],
        ret_ty,
    )?;

    let args_local = builder.seed_synthetic_param_local("args".to_string(), args_ty, false, None);

    let effects = arm.effective_effect_env(db).bindings(db);
    seed_effect_param_locals(db, &mut builder, fields, effects, core);

    // Prologue: destructure decoded args tuple into pattern bindings.
    let arg_bindings = arm.arg_bindings(db);
    let args_value = builder.alloc_value(
        args_ty,
        ValueOrigin::Local(args_local),
        builder.value_repr_for_ty(args_ty, AddressSpaceKind::Memory),
    );
    let entry = builder.builder.entry_block();
    builder.move_to_block(entry);
    for binding in arg_bindings {
        let tuple_index = binding.tuple_index as usize;
        let elem_value =
            project_tuple_elem_value(&mut builder, args_value, args_ty, tuple_index, binding.ty);
        bind_pat_value(&mut builder, binding.pat, elem_value);
        if builder.current_block().is_none() {
            break;
        }
    }

    builder.lower_root(body.expr(db));
    builder.ensure_const_expr_values();
    if let Some(block) = builder.current_block() {
        let returns_value = !builder.is_unit_ty(ret_ty)
            && !ret_ty.is_never(db)
            && !layout::is_zero_sized_ty(db, ret_ty);
        if returns_value {
            let ret_val = builder.ensure_value(body.expr(db));
            builder.set_terminator(block, Terminator::Return(Some(ret_val)));
        } else {
            builder.set_terminator(block, Terminator::Return(None));
        }
    }
    let mir_body = builder.finish();

    if let Some(expr) = super::first_unlowered_expr_used_by_mir(&mir_body) {
        let expr_context = super::format_hir_expr_context(db, body, expr);
        return Err(MirLowerError::UnloweredHirExpr {
            func_name: symbols.recv_handler(recv_idx, arm_idx),
            expr: expr_context,
        });
    }

    Ok(MirFunction {
        origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractRecvArmHandler {
            contract,
            recv_idx,
            arm_idx,
        }),
        body: mir_body,
        typed_body: Some(typed_body.clone()),
        generic_args: Vec::new(),
        ret_ty,
        returns_value: !layout::is_zero_sized_ty(db, ret_ty),
        contract_function: None,
        symbol_name: symbols.recv_handler(recv_idx, arm_idx),
        receiver_space: None,
    })
}

fn seed_effect_param_locals<'db>(
    db: &'db dyn HirAnalysisDb,
    builder: &mut MirBuilder<'db, '_>,
    fields: &[ContractFieldInfo<'db>],
    effects: &[EffectBinding<'db>],
    core: &CoreLib<'db>,
) {
    for effect in effects {
        let name = effect.binding_name.data(db).to_string();
        let binding = match effect.source {
            EffectSource::Root => LocalBinding::EffectParam {
                site: effect.binding_site,
                idx: effect.binding_idx as usize,
                key_path: effect.binding_key_path,
                is_mut: effect.is_mut,
            },
            EffectSource::Field(field_idx) => {
                let ty = fields
                    .get(field_idx as usize)
                    .map(|f| f.target_ty)
                    .unwrap_or_else(|| {
                        TyId::invalid(db, hir::analysis::ty::ty_def::InvalidCause::Other)
                    });
                LocalBinding::Param {
                    site: ParamSite::EffectField(effect.binding_site),
                    idx: effect.binding_idx as usize,
                    ty,
                    is_mut: effect.is_mut,
                }
            }
        };

        let addr_space = match effect.source {
            EffectSource::Root => AddressSpaceKind::Storage,
            EffectSource::Field(field_idx) => match fields.get(field_idx as usize) {
                Some(field) if field.is_provider => {
                    repr::effect_provider_space_for_ty(db, core, field.declared_ty)
                        .unwrap_or(AddressSpaceKind::Storage)
                }
                _ => AddressSpaceKind::Storage,
            },
        };

        builder.seed_synthetic_effect_param_local(name, binding, addr_space);
    }
}

fn project_tuple_elem_value<'db>(
    builder: &mut MirBuilder<'db, '_>,
    tuple_value: ValueId,
    tuple_ty: TyId<'db>,
    field_idx: usize,
    field_ty: TyId<'db>,
) -> ValueId {
    // Transparent newtype access: field 0 is a representation-preserving cast.
    if field_idx == 0 && repr::transparent_newtype_field_ty(builder.db, tuple_ty).is_some() {
        let base_repr = builder.builder.body.value(tuple_value).repr;
        if !base_repr.is_ref() {
            let space = base_repr
                .address_space()
                .unwrap_or(AddressSpaceKind::Memory);
            return builder.alloc_value(
                field_ty,
                ValueOrigin::TransparentCast { value: tuple_value },
                builder.value_repr_for_ty(field_ty, space),
            );
        }
    }

    let base_space = builder.value_address_space(tuple_value);
    let place = crate::ir::Place::new(
        tuple_value,
        crate::ir::MirProjectionPath::from_projection(hir::projection::Projection::Field(
            field_idx,
        )),
    );
    if builder.is_by_ref_ty(field_ty) {
        return builder.alloc_value(
            field_ty,
            ValueOrigin::PlaceRef(place),
            ValueRepr::Ref(base_space),
        );
    }
    let dest = builder.alloc_temp_local(field_ty, false, "arg");
    builder.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
    builder.assign(None, Some(dest), Rvalue::Load { place });
    builder.alloc_value(field_ty, ValueOrigin::Local(dest), ValueRepr::Word)
}

fn bind_pat_value<'db>(
    builder: &mut MirBuilder<'db, '_>,
    pat: hir::hir_def::PatId,
    value: ValueId,
) {
    let Some(block) = builder.current_block() else {
        return;
    };
    let Partial::Present(pat_data) = pat.data(builder.db, builder.body) else {
        return;
    };

    match pat_data {
        hir::hir_def::Pat::WildCard | hir::hir_def::Pat::Rest => {}
        hir::hir_def::Pat::Path(_, is_mut) => {
            let binding = builder
                .typed_body
                .pat_binding(pat)
                .unwrap_or(LocalBinding::Local {
                    pat,
                    is_mut: *is_mut,
                });
            let Some(local) = builder.local_for_binding(binding) else {
                return;
            };
            builder.move_to_block(block);
            builder.assign(None, Some(local), Rvalue::Value(value));
            let pat_ty = builder.typed_body.pat_ty(builder.db, pat);
            if builder
                .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                .address_space()
                .is_some()
            {
                let space = builder.value_address_space(value);
                builder.set_pat_address_space(pat, space);
            }
        }
        hir::hir_def::Pat::Tuple(pats) | hir::hir_def::Pat::PathTuple(_, pats) => {
            let base_ty = builder.typed_body.pat_ty(builder.db, pat);
            for (idx, field_pat) in pats.iter().enumerate() {
                let Partial::Present(field_pat_data) = field_pat.data(builder.db, builder.body)
                else {
                    continue;
                };
                if matches!(
                    field_pat_data,
                    hir::hir_def::Pat::WildCard | hir::hir_def::Pat::Rest
                ) {
                    continue;
                }
                let field_ty = builder.typed_body.pat_ty(builder.db, *field_pat);
                let field_value = project_tuple_elem_value(builder, value, base_ty, idx, field_ty);
                bind_pat_value(builder, *field_pat, field_value);
                if builder.current_block().is_none() {
                    break;
                }
            }
        }
        hir::hir_def::Pat::Record(_, fields) => {
            let base_ty = builder.typed_body.pat_ty(builder.db, pat);
            for field in fields {
                let Some(label) = field.label(builder.db, builder.body) else {
                    continue;
                };
                let Some(info) =
                    builder.field_access_info(base_ty, hir::hir_def::FieldIndex::Ident(label))
                else {
                    continue;
                };
                let field_ty = builder.typed_body.pat_ty(builder.db, field.pat);
                let field_value =
                    project_tuple_elem_value(builder, value, base_ty, info.field_idx, field_ty);
                bind_pat_value(builder, field.pat, field_value);
                if builder.current_block().is_none() {
                    break;
                }
            }
        }
        _ => {}
    }
}

fn lower_init_entrypoint<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    fields: &[ContractFieldInfo<'db>],
    target: &TargetContext<'db>,
    symbols: &ContractSymbols,
    slot_offsets: &[BigUint],
) -> MirLowerResult<MirFunction<'db>> {
    let contract_fn = ContractFunction {
        contract_name: symbols.contract_name.clone(),
        kind: ContractFunctionKind::Init,
    };

    let cx = ContractMirCx::new(db, target);
    let mut builder = BodyBuilder::new();
    let entry = builder.entry_block();
    builder.move_to_block(entry);

    let root_value = builder.unit_value(target.host.root_effect_ty);
    let zero_u256 = builder.const_int_value(TyId::u256(db), BigUint::from(0u8));

    let field_values = cx.emit_field_providers(
        &mut builder,
        fields,
        slot_offsets,
        root_value,
        target.host.init_field_fn,
    );

    // Code region queries for the runtime entrypoint are shared by init-input decoding and
    // contract creation.
    let runtime_func_item = builder.func_item_value(
        TyId::unit(db),
        CodeRegionRoot {
            origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractRuntimeEntrypoint(contract)),
            generic_args: Vec::new(),
            symbol: None,
        },
    );
    let runtime_offset = builder
        .assign_to_new_local(
            "runtime_offset",
            TyId::u256(db),
            false,
            AddressSpaceKind::Memory,
            Rvalue::Intrinsic {
                op: IntrinsicOp::CodeRegionOffset,
                args: vec![runtime_func_item],
            },
        )
        .value;
    let runtime_len = builder
        .assign_to_new_local(
            "runtime_len",
            TyId::u256(db),
            false,
            AddressSpaceKind::Memory,
            Rvalue::Intrinsic {
                op: IntrinsicOp::CodeRegionLen,
                args: vec![runtime_func_item],
            },
        )
        .value;

    if contract.init(db).is_some()
        && let Some(init_env) = contract.init_effect_env(db)
    {
        let init_args_ty = contract.init_args_ty(db);
        // Inline `ContractHost::init_input` semantics (avoids needing a synthetic HIR function-item type).
        let args_offset_value = builder.alloc_value(
            TyId::u256(db),
            ValueOrigin::Binary {
                op: BinOp::Arith(ArithBinOp::Add),
                lhs: runtime_offset,
                rhs: runtime_len,
            },
            ValueRepr::Word,
        );
        let code_size = builder
            .assign_to_new_local(
                "code_size",
                TyId::u256(db),
                false,
                AddressSpaceKind::Memory,
                Rvalue::Intrinsic {
                    op: IntrinsicOp::Codesize,
                    args: Vec::new(),
                },
            )
            .value;
        let cond_value = builder.alloc_value(
            TyId::bool(db),
            ValueOrigin::Binary {
                op: BinOp::Comp(CompBinOp::Lt),
                lhs: code_size,
                rhs: args_offset_value,
            },
            ValueRepr::Word,
        );

        let abort_block = builder.make_block();
        let cont_block = builder.make_block();
        builder.branch(cond_value, abort_block, cont_block);

        // abort: `root.abort()`
        builder.move_to_block(abort_block);
        builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Call(
            cx.host_abort(root_value),
        )));

        // continue block builds the init-input decoder, calls init handler, then returns the runtime region.
        builder.move_to_block(cont_block);
        let args_len_value = builder.alloc_value(
            TyId::u256(db),
            ValueOrigin::Binary {
                op: BinOp::Arith(ArithBinOp::Sub),
                lhs: code_size,
                rhs: args_offset_value,
            },
            ValueRepr::Word,
        );
        let args_ptr_value = builder
            .assign_to_new_local(
                "args_ptr",
                TyId::u256(db),
                false,
                AddressSpaceKind::Memory,
                Rvalue::Call(cx.alloc(args_len_value)),
            )
            .value;
        builder.assign(
            None,
            Rvalue::Intrinsic {
                op: IntrinsicOp::Codecopy,
                args: vec![args_ptr_value, args_offset_value, args_len_value],
            },
        );

        // Construct `InitInput` (MemoryBytes) with `{ base: args_ptr, len: args_len }`.
        let input_value = builder
            .assign_to_new_local(
                "input",
                target.host.init_input_ty,
                false,
                AddressSpaceKind::Memory,
                Rvalue::Alloc {
                    address_space: AddressSpaceKind::Memory,
                },
            )
            .value;
        builder.store_field(input_value, 0, args_ptr_value);
        builder.store_field(input_value, 1, args_len_value);

        // Decoder: `A::decoder_new(input)`.
        let decoder_value = builder
            .assign_to_new_local(
                "decoder",
                target.abi.init_decoder_ty,
                false,
                AddressSpaceKind::Memory,
                Rvalue::Call(cx.abi_decoder_new(input_value, target.host.init_input_ty)),
            )
            .value;

        // Decode init params in order.
        let mut decoded_params = Vec::new();
        for (idx, param_ty) in init_args_ty.field_types(db).iter().copied().enumerate() {
            decoded_params.push(cx.emit_decode_or_unit(
                &mut builder,
                decoder_value,
                target.abi.init_decoder_ty,
                param_ty,
                format!("init_arg{idx}"),
            ));
        }

        // Call init handler with decoded args + effects.
        let effect_args =
            cx.effect_args_from_sources(init_env.bindings(db), zero_u256, &field_values);
        builder.assign(
            None,
            Rvalue::Call(cx.call_symbol(symbols.init_handler.clone(), decoded_params, effect_args)),
        );

        // Inline `ContractHost::create_contract` semantics.
        builder.assign(
            None,
            Rvalue::Intrinsic {
                op: IntrinsicOp::Codecopy,
                args: vec![zero_u256, runtime_offset, runtime_len],
            },
        );
        builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Intrinsic {
            op: IntrinsicOp::ReturnData,
            args: vec![zero_u256, runtime_len],
        }));
    } else {
        // No init block: just return the runtime region.
        builder.assign(
            None,
            Rvalue::Intrinsic {
                op: IntrinsicOp::Codecopy,
                args: vec![zero_u256, runtime_offset, runtime_len],
            },
        );
        builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Intrinsic {
            op: IntrinsicOp::ReturnData,
            args: vec![zero_u256, runtime_len],
        }));
    }

    Ok(MirFunction {
        origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractInitEntrypoint(contract)),
        body: builder.build(),
        typed_body: None,
        generic_args: Vec::new(),
        ret_ty: TyId::unit(db),
        returns_value: false,
        contract_function: Some(contract_fn),
        symbol_name: symbols.init_entrypoint.clone(),
        receiver_space: None,
    })
}

fn lower_runtime_entrypoint<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    fields: &[ContractFieldInfo<'db>],
    target: &TargetContext<'db>,
    symbols: &ContractSymbols,
    slot_offsets: &[BigUint],
) -> MirLowerResult<MirFunction<'db>> {
    let contract_fn = ContractFunction {
        contract_name: symbols.contract_name.clone(),
        kind: ContractFunctionKind::Runtime,
    };

    let cx = ContractMirCx::new(db, target);
    let mut builder = BodyBuilder::new();
    let entry = builder.entry_block();
    builder.move_to_block(entry);

    let root_value = builder.unit_value(target.host.root_effect_ty);
    let zero_u256 = builder.const_int_value(TyId::u256(db), BigUint::from(0u8));

    let field_values = cx.emit_field_providers(
        &mut builder,
        fields,
        slot_offsets,
        root_value,
        target.host.field_fn,
    );

    // Selector + decoder.
    let selector_value = builder
        .assign_to_new_local(
            "selector",
            target.abi.selector_ty,
            false,
            AddressSpaceKind::Memory,
            Rvalue::Call(cx.host_runtime_selector(root_value)),
        )
        .value;
    let decoder_value = builder
        .assign_to_new_local(
            "decoder",
            target.abi.runtime_decoder_ty,
            false,
            AddressSpaceKind::Memory,
            Rvalue::Call(cx.host_runtime_decoder(root_value)),
        )
        .value;

    // Dispatch switch.
    let mut targets = Vec::new();
    let default_block = builder.make_block();
    for recv in contract.recv_views(db) {
        for arm in recv.arms(db) {
            let recv_idx = recv.index(db);
            let arm_idx = arm.index(db);
            let abi_info = arm.abi_info(db, target.abi.abi_ty);

            let block = builder.make_block();
            targets.push(SwitchTarget {
                value: SwitchValue::Int(BigUint::from(abi_info.selector_value)),
                block,
            });

            builder.move_to_block(block);
            let args_value = cx.emit_decode_or_unit(
                &mut builder,
                decoder_value,
                target.abi.runtime_decoder_ty,
                abi_info.args_ty,
                format!("args_{recv_idx}_{arm_idx}"),
            );
            let effect_args = cx.effect_args_from_sources(
                arm.effective_effect_env(db).bindings(db),
                zero_u256,
                &field_values,
            );
            let handler_symbol = symbols.recv_handler(recv_idx, arm_idx);

            if let Some(ret_ty) = abi_info.ret_ty {
                let result_value = builder
                    .assign_to_new_local(
                        format!("result_{recv_idx}_{arm_idx}"),
                        ret_ty,
                        false,
                        AddressSpaceKind::Memory,
                        Rvalue::Call(cx.call_symbol(handler_symbol, vec![args_value], effect_args)),
                    )
                    .value;
                builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Call(
                    cx.host_return_value(root_value, result_value, ret_ty),
                )));
            } else {
                builder.assign(
                    None,
                    Rvalue::Call(cx.call_symbol(handler_symbol, vec![args_value], effect_args)),
                );
                builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Call(
                    cx.host_return_unit(root_value),
                )));
            }
        }
    }

    builder.move_to_block(default_block);
    builder.terminate_current(Terminator::TerminatingCall(TerminatingCall::Call(
        cx.host_abort(root_value),
    )));

    builder.move_to_block(entry);
    builder.switch(selector_value, targets, default_block);

    Ok(MirFunction {
        origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractRuntimeEntrypoint(contract)),
        body: builder.build(),
        typed_body: None,
        generic_args: Vec::new(),
        ret_ty: TyId::unit(db),
        returns_value: false,
        contract_function: Some(contract_fn),
        symbol_name: symbols.runtime_entrypoint.clone(),
        receiver_space: None,
    })
}

fn lower_init_code_offset<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    symbols: &ContractSymbols,
) -> MirLowerResult<MirFunction<'db>> {
    lower_code_region_query(
        db,
        contract,
        symbols.init_code_offset.clone(),
        SyntheticId::ContractInitCodeOffset(contract),
        IntrinsicOp::CodeRegionOffset,
    )
}

fn lower_init_code_len<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    symbols: &ContractSymbols,
) -> MirLowerResult<MirFunction<'db>> {
    lower_code_region_query(
        db,
        contract,
        symbols.init_code_len.clone(),
        SyntheticId::ContractInitCodeLen(contract),
        IntrinsicOp::CodeRegionLen,
    )
}

fn lower_code_region_query<'db>(
    db: &'db dyn hir::analysis::diagnostics::SpannedHirAnalysisDb,
    contract: Contract<'db>,
    symbol_name: String,
    id: SyntheticId<'db>,
    op: IntrinsicOp,
) -> MirLowerResult<MirFunction<'db>> {
    let mut builder = BodyBuilder::new();
    let entry = builder.entry_block();
    builder.move_to_block(entry);

    let func_item = builder.func_item_value(
        TyId::unit(db),
        CodeRegionRoot {
            origin: MirFunctionOrigin::Synthetic(SyntheticId::ContractInitEntrypoint(contract)),
            generic_args: Vec::new(),
            symbol: None,
        },
    );
    let value = builder
        .assign_to_new_local(
            "ret",
            TyId::u256(db),
            false,
            AddressSpaceKind::Memory,
            Rvalue::Intrinsic {
                op,
                args: vec![func_item],
            },
        )
        .value;
    builder.return_value(value);

    Ok(MirFunction {
        origin: MirFunctionOrigin::Synthetic(id),
        body: builder.build(),
        typed_body: None,
        generic_args: Vec::new(),
        ret_ty: TyId::u256(db),
        returns_value: true,
        contract_function: None,
        symbol_name,
        receiver_space: None,
    })
}
