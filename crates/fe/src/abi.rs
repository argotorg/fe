use std::collections::HashSet;

use common::ingot::IngotKind;
use driver::DriverDataBase;
use hir::{
    analysis::{
        name_resolution::{PathRes, resolve_path},
        ty::{
            abi_ty::{self, AbiComponent, AbiTypeDesc, ParsedFunctionSignature},
            adt_def::AdtRef,
            binder::Binder,
            corelib::{RuntimeBuiltinFuncKind, runtime_builtin_func_kind},
            normalize::normalize_from_assumptions,
            trait_def::{TraitInstId, resolve_trait_method_instance},
            trait_resolution::{PredicateListId, Selection, TraitSolveCx},
            ty_def::{TyBase, TyData, TyId},
        },
    },
    hir_def::{
        FieldDefListId, Func, GenericParamOwner, IdentId, PathId, Struct, TopLevelMod, Trait,
        scope_graph::ScopeId,
    },
};
use serde::Serialize;
use tiny_keccak::{Hasher, Keccak};

#[derive(Serialize)]
pub struct AbiEntry {
    #[serde(rename = "type")]
    pub entry_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inputs: Option<Vec<AbiParam>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outputs: Option<Vec<AbiParam>>,
    #[serde(rename = "stateMutability", skip_serializing_if = "Option::is_none")]
    pub state_mutability: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anonymous: Option<bool>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq)]
pub struct AbiParam {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Vec<AbiParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct NamedAbiParamDesc {
    name: String,
    indexed: Option<bool>,
    desc: AbiTypeDesc,
}

fn component_to_param(component: AbiComponent) -> AbiParam {
    AbiParam {
        name: component.name,
        ty: component.ty,
        indexed: None,
        components: component
            .components
            .map(|components| components.into_iter().map(component_to_param).collect()),
    }
}

impl NamedAbiParamDesc {
    fn into_param(self) -> AbiParam {
        AbiParam {
            name: self.name,
            ty: self.desc.abi_type,
            indexed: self.indexed,
            components: self
                .desc
                .components
                .map(|components| components.into_iter().map(component_to_param).collect()),
        }
    }
}

pub struct AbiResult {
    pub json: String,
    pub entry_count: usize,
    pub warnings: Vec<String>,
}

enum RecvArmAbiEmission {
    Emit(AbiEntry),
    Skip(String),
}

/// Generate an Ethereum-compatible JSON ABI for a given contract inside `top_mod`.
///
/// Returns `Ok(None)` if the contract is not found in this module (useful for
/// ingot builds where a contract may live in a different module).
pub fn generate_contract_abi(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    contract_name: &str,
) -> Result<Option<AbiResult>, String> {
    let Some(contract) = top_mod
        .all_contracts(db)
        .iter()
        .find(|c| {
            c.name(db)
                .to_opt()
                .map(|n| n.data(db).as_str() == contract_name)
                .unwrap_or(false)
        })
        .copied()
    else {
        return Ok(None);
    };

    let mut entries = Vec::new();
    let mut warnings = Vec::new();

    if let Some(init) = contract.init(db) {
        let inputs = init_params_to_abi_params(db, contract)?
            .into_iter()
            .map(NamedAbiParamDesc::into_param)
            .collect();
        let state_mutability = if init.is_payable(db) {
            "payable"
        } else {
            "nonpayable"
        };
        entries.push(AbiEntry {
            entry_type: "constructor".to_string(),
            name: None,
            inputs: Some(inputs),
            outputs: None,
            state_mutability: Some(state_mutability.to_string()),
            anonymous: None,
        });
    }

    let sol_ty = resolve_sol_abi_ty(db, contract.scope())?;
    for recv in contract.recv_views(db) {
        for arm_view in recv.arms(db) {
            match recv_arm_to_abi_entry(db, arm_view, sol_ty) {
                Ok(RecvArmAbiEmission::Emit(entry)) => entries.push(entry),
                Ok(RecvArmAbiEmission::Skip(warning)) => warnings.push(warning),
                Err(e) => return Err(e),
            }
        }
    }

    for struct_ in collect_contract_event_structs(db, contract, false) {
        entries.push(event_struct_to_abi_entry(db, struct_)?);
    }

    for struct_ in collect_contract_event_structs(db, contract, true) {
        let mut entry = event_struct_to_abi_entry(db, struct_)?;
        entry.entry_type = "error".to_string();
        entry.anonymous = None;
        for input in entry.inputs.iter_mut().flatten() {
            input.indexed = None;
        }
        entries.push(entry);
    }

    let entry_count = entries.len();
    let json = serde_json::to_string_pretty(&entries)
        .map_err(|e| format!("JSON serialization error: {e}"))?;
    Ok(Some(AbiResult {
        json,
        entry_count,
        warnings,
    }))
}

fn recv_arm_to_abi_entry(
    db: &DriverDataBase,
    arm_view: hir::semantic::RecvArmView<'_>,
    sol_ty: TyId<'_>,
) -> Result<RecvArmAbiEmission, String> {
    let arm = arm_view
        .arm(db)
        .ok_or_else(|| "missing recv arm during ABI generation".to_string())?;
    if arm_view.is_fallback(db) {
        return Ok(RecvArmAbiEmission::Emit(AbiEntry {
            entry_type: "fallback".to_string(),
            name: None,
            inputs: None,
            outputs: None,
            state_mutability: Some(if arm.is_payable(db) {
                "payable".to_string()
            } else {
                "nonpayable".to_string()
            }),
            anonymous: None,
        }));
    }

    let variant_ty = arm_view.variant_ty(db);
    let variant_struct = struct_from_ty(db, variant_ty).ok_or_else(|| {
        format!(
            "recv arm type `{}` is not a struct",
            variant_ty.pretty_print(db)
        )
    })?;
    let variant_name = variant_struct
        .name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "<unknown>".to_string());
    if !variant_has_canonical_json_abi_shape(db, variant_struct) {
        return Ok(RecvArmAbiEmission::Skip(format!(
            "skipping recv arm `{variant_name}`: ABI shape is not compiler-known for manual `MsgVariant` impls; only `msg`-generated variants are emitted"
        )));
    }
    let abi_info = arm_view.abi_info(db, sol_ty);
    let Some(selector_signature) = abi_info.selector_signature.as_deref() else {
        return Ok(RecvArmAbiEmission::Skip(format!(
            "skipping recv arm `{variant_name}`: selector signature is unknown; \
             use `#[selector = sol(\"name(types)\")]` to include it in the ABI"
        )));
    };

    let input_descs = struct_ty_to_abi_param_descs(db, variant_struct, variant_ty, |_| None)?;
    let outputs = match abi_info.ret_ty {
        Some(ret_ty) => {
            let desc = semantic_ty_to_abi_desc(db, ret_ty)?;
            vec![
                NamedAbiParamDesc {
                    name: String::new(),
                    indexed: None,
                    desc,
                }
                .into_param(),
            ]
        }
        None => Vec::new(),
    };

    let selector_value = abi_info.selector_value.ok_or_else(|| {
        format!(
            "cannot emit JSON ABI for `{selector_signature}`: selector value could not be resolved"
        )
    })?;
    let parsed_signature = parse_function_signature(selector_signature)?;
    ensure_selector_matches_signature(
        selector_signature,
        &parsed_signature,
        &input_descs,
        selector_value,
    )?;

    Ok(RecvArmAbiEmission::Emit(AbiEntry {
        entry_type: "function".to_string(),
        name: Some(parsed_signature.name),
        inputs: Some(
            input_descs
                .into_iter()
                .map(NamedAbiParamDesc::into_param)
                .collect(),
        ),
        outputs: Some(outputs),
        state_mutability: Some(derive_state_mutability(db, arm_view)),
        anonymous: None,
    }))
}

fn event_struct_to_abi_entry(db: &DriverDataBase, struct_: Struct<'_>) -> Result<AbiEntry, String> {
    let event_name = struct_
        .name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .ok_or_else(|| "event struct is missing a name".to_string())?;
    let field_tys: Vec<_> = struct_
        .field_tys(db)
        .into_iter()
        .map(|ty| ty.instantiate_identity())
        .collect();
    let inputs = named_field_param_descs(db, struct_.hir_fields(db), &field_tys, |field| {
        Some(field.is_event_indexed)
    })?
    .into_iter()
    .map(NamedAbiParamDesc::into_param)
    .collect();

    Ok(AbiEntry {
        entry_type: "event".to_string(),
        name: Some(event_name),
        inputs: Some(inputs),
        outputs: None,
        state_mutability: None,
        anonymous: Some(false),
    })
}

fn collect_contract_event_structs<'db>(
    db: &'db DriverDataBase,
    contract: hir::hir_def::Contract<'db>,
    errors: bool,
) -> Vec<Struct<'db>> {
    let mut events = Vec::new();
    let mut seen = HashSet::new();
    let mut visited_funcs = HashSet::new();
    let emit_traits = resolve_event_emit_traits(db, contract.scope());
    let error_func = hir::analysis::ty::corelib::resolve_lib_func_path(
        db,
        contract.scope(),
        "std::evm::effects::revert_error",
    );

    if contract.init(db).is_some() {
        let (_, typed_body) = hir::analysis::ty::ty_check::check_contract_init_body(db, contract);
        collect_typed_body_event_structs(
            db,
            typed_body,
            &emit_traits,
            errors,
            error_func,
            &mut events,
            &mut seen,
            &mut visited_funcs,
        );
    }

    for recv in contract.recv_views(db) {
        for arm in recv.arms(db) {
            let (_, typed_body) = hir::analysis::ty::ty_check::check_contract_recv_arm_body(
                db,
                contract,
                recv.index(db),
                arm.index(db),
            );
            collect_typed_body_event_structs(
                db,
                typed_body,
                &emit_traits,
                errors,
                error_func,
                &mut events,
                &mut seen,
                &mut visited_funcs,
            );
        }
    }

    events
}

#[allow(clippy::too_many_arguments)]
fn collect_typed_body_event_structs<'db>(
    db: &'db DriverDataBase,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    emit_traits: &EventEmitTraits<'db>,
    errors: bool,
    error_func: Option<Func<'db>>,
    out: &mut Vec<Struct<'db>>,
    seen: &mut HashSet<Struct<'db>>,
    visited_funcs: &mut HashSet<VisitedFuncBody<'db>>,
) {
    let Some(body) = typed_body.body() else {
        return;
    };

    for (expr_id, partial_expr) in body.exprs(db).iter() {
        let hir::hir_def::Partial::Present(_) = partial_expr else {
            continue;
        };
        if errors {
            if let Some(struct_) = reverted_error_struct(db, typed_body, body, expr_id, error_func)
                && seen.insert(struct_)
            {
                out.push(struct_);
            }
        } else if let Some(struct_) =
            emitted_event_struct(db, typed_body, body, expr_id, emit_traits)
        {
            push_event_struct(db, out, seen, struct_);
        }

        // Operators also have semantic callees, even without call syntax.
        if let Some(callable) = typed_body.callable_expr(expr_id)
            && let hir::hir_def::CallableDef::Func(func) = callable.callable_def
        {
            let mut target = VisitedFuncBody::from_callable(func, callable);
            if let Some(inst) = callable.trait_inst()
                && let Some(name) = func.name(db).to_opt()
                && let Selection::Unique(method) = resolve_trait_method_instance(
                    db,
                    TraitSolveCx::new(db, body.scope()).with_assumptions(typed_body.assumptions()),
                    inst,
                    name,
                )
                && let Some(impl_func) = method.body()
                && let Ok(body_args) = method.complete_body_args(
                    db,
                    callable.generic_args(),
                    callable.checked_input_tys(),
                    None,
                )
            {
                target.func = impl_func;
                target.generic_args = body_args.into_values();
            }
            if target.func.body(db).is_none() || !visited_funcs.insert(target.clone()) {
                continue;
            }
            let (_, func_typed_body) =
                hir::analysis::ty::ty_check::check_func_body(db, target.func);
            let func_typed_body =
                instantiate_callable_typed_body(db, func_typed_body.clone(), &target);
            collect_typed_body_event_structs(
                db,
                &func_typed_body,
                emit_traits,
                errors,
                error_func,
                out,
                seen,
                visited_funcs,
            );
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct VisitedFuncBody<'db> {
    func: Func<'db>,
    generic_args: Vec<TyId<'db>>,
    trait_inst: Option<TraitInstId<'db>>,
}

impl<'db> VisitedFuncBody<'db> {
    fn from_callable(
        func: Func<'db>,
        callable: &hir::analysis::ty::ty_check::Callable<'db>,
    ) -> Self {
        Self {
            func,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        }
    }
}

fn instantiate_callable_typed_body<'db>(
    db: &'db DriverDataBase,
    typed_body: hir::analysis::ty::ty_check::TypedBody<'db>,
    target: &VisitedFuncBody<'db>,
) -> hir::analysis::ty::ty_check::TypedBody<'db> {
    let mut typed_body = Binder::bind(GenericParamOwner::Func(target.func), typed_body)
        .instantiate(db, &target.generic_args);
    if let Some(trait_inst) = target.trait_inst {
        typed_body = normalize_from_assumptions(
            db,
            typed_body,
            target.func.scope(),
            PredicateListId::new(db, vec![trait_inst]),
        );
    }
    typed_body
}

struct EventEmitTraits<'db> {
    log_trait: Option<Trait<'db>>,
    event_trait: Option<Trait<'db>>,
}

fn resolve_event_emit_traits<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
) -> EventEmitTraits<'db> {
    EventEmitTraits {
        log_trait: resolve_lib_trait_path(db, scope, "std::evm::effects::Log"),
        event_trait: resolve_lib_trait_path(db, scope, "std::evm::event::Event"),
    }
}

fn emitted_event_struct<'db>(
    db: &'db DriverDataBase,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    body: hir::hir_def::Body<'db>,
    expr_id: hir::hir_def::ExprId,
    emit_traits: &EventEmitTraits<'db>,
) -> Option<Struct<'db>> {
    let hir::hir_def::Partial::Present(hir::hir_def::Expr::MethodCall(
        receiver,
        method_name,
        _,
        args,
    )) = expr_id.data(db, body)
    else {
        return None;
    };
    let method_name = method_name.to_opt()?;
    if method_name.data(db) != "emit" {
        return None;
    }

    let callable = typed_body.callable_expr(expr_id)?;
    let hir::hir_def::CallableDef::Func(func) = callable.callable_def else {
        return None;
    };
    if func.name(db).to_opt()? != method_name {
        return None;
    }

    let trait_def = callable.trait_inst()?.def(db);
    if emit_traits
        .log_trait
        .is_some_and(|log_trait| trait_def == log_trait)
    {
        let event_expr = args.first()?.expr;
        let struct_ = struct_from_ty(db, typed_body.expr_ty(db, event_expr))?;
        return is_event_struct(db, struct_).then_some(struct_);
    }
    if emit_traits
        .event_trait
        .is_some_and(|event_trait| trait_def == event_trait)
    {
        let struct_ = struct_from_ty(db, typed_body.expr_ty(db, *receiver))?;
        return is_event_struct(db, struct_).then_some(struct_);
    }

    None
}

fn reverted_error_struct<'db>(
    db: &'db DriverDataBase,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    body: hir::hir_def::Body<'db>,
    expr_id: hir::hir_def::ExprId,
    error_func: Option<Func<'db>>,
) -> Option<Struct<'db>> {
    let hir::hir_def::Partial::Present(hir::hir_def::Expr::Call(_, args)) = expr_id.data(db, body)
    else {
        return None;
    };
    let callable = typed_body.callable_expr(expr_id)?;
    let hir::hir_def::CallableDef::Func(func) = callable.callable_def else {
        return None;
    };
    if Some(func) != error_func
        && runtime_builtin_func_kind(db, func) != Some(RuntimeBuiltinFuncKind::PanicWithValue)
    {
        return None;
    }
    let struct_ = struct_from_ty(db, typed_body.expr_ty(db, args.first()?.expr))?;
    matches!(
        hir::span::struct_ast(db, struct_),
        hir::span::HirOrigin::Desugared(hir::span::DesugaredOrigin::Error(_))
    )
    .then_some(struct_)
}

fn push_event_struct<'db>(
    db: &'db DriverDataBase,
    out: &mut Vec<Struct<'db>>,
    seen: &mut HashSet<Struct<'db>>,
    struct_: Struct<'db>,
) {
    if !seen.insert(struct_) || !is_event_struct(db, struct_) {
        return;
    }
    out.push(struct_);
}

fn is_event_struct(db: &DriverDataBase, struct_: Struct<'_>) -> bool {
    matches!(
        hir::span::struct_ast(db, struct_),
        hir::span::HirOrigin::Desugared(hir::span::DesugaredOrigin::Event(_))
    )
}

fn variant_has_canonical_json_abi_shape(db: &DriverDataBase, struct_: Struct<'_>) -> bool {
    matches!(
        hir::span::struct_ast(db, struct_),
        hir::span::HirOrigin::Desugared(hir::span::DesugaredOrigin::Msg(_))
    )
}

fn resolve_lib_trait_path<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
    path: &str,
) -> Option<Trait<'db>> {
    let mut segments = path.split("::");
    let root = segments.next()?;

    let ingot = scope.ingot(db);
    let mut path = if (ingot.kind(db) == IngotKind::Std && root == "std")
        || (ingot.kind(db) == IngotKind::Core && root == "core")
    {
        PathId::from_ident(db, IdentId::make_ingot(db))
    } else {
        PathId::from_str(db, root)
    };

    for segment in segments {
        path = path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, true).ok()? {
        PathRes::Trait(inst) => Some(inst.def(db)),
        _ => None,
    }
}

fn init_params_to_abi_params(
    db: &DriverDataBase,
    contract: hir::hir_def::Contract<'_>,
) -> Result<Vec<NamedAbiParamDesc>, String> {
    let Some(init) = contract.init(db) else {
        return Ok(Vec::new());
    };

    let params: Vec<_> = init
        .params(db)
        .data(db)
        .iter()
        .filter(|param| !param.is_self_param(db))
        .collect();
    let param_tys = contract.init_args_ty(db).field_types(db);

    if params.len() != param_tys.len() {
        return Err(format!(
            "constructor parameter count mismatch: {} names vs {} semantic types",
            params.len(),
            param_tys.len()
        ));
    }

    params
        .into_iter()
        .zip(param_tys)
        .map(|(param, ty)| {
            let name = param
                .name()
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_default();
            Ok(NamedAbiParamDesc {
                name,
                indexed: None,
                desc: semantic_ty_to_abi_desc(db, ty)?,
            })
        })
        .collect()
}

fn struct_ty_to_abi_param_descs(
    db: &DriverDataBase,
    struct_: Struct<'_>,
    ty: TyId<'_>,
    indexed: impl Fn(&hir::hir_def::FieldDef<'_>) -> Option<bool>,
) -> Result<Vec<NamedAbiParamDesc>, String> {
    let field_tys = ty.field_types(db);
    named_field_param_descs(db, struct_.hir_fields(db), &field_tys, indexed)
}

fn named_field_param_descs(
    db: &DriverDataBase,
    fields: FieldDefListId<'_>,
    field_tys: &[TyId<'_>],
    indexed: impl Fn(&hir::hir_def::FieldDef<'_>) -> Option<bool>,
) -> Result<Vec<NamedAbiParamDesc>, String> {
    let hir_fields = fields.data(db);
    if hir_fields.len() != field_tys.len() {
        return Err(format!(
            "field count mismatch: {} HIR fields vs {} semantic fields",
            hir_fields.len(),
            field_tys.len()
        ));
    }

    hir_fields
        .iter()
        .zip(field_tys.iter().copied())
        .map(|(field, ty)| {
            let name = field
                .name
                .to_opt()
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_default();
            Ok(NamedAbiParamDesc {
                name,
                indexed: indexed(field),
                desc: semantic_ty_to_abi_desc(db, ty)?,
            })
        })
        .collect()
}

fn semantic_ty_to_abi_desc(db: &DriverDataBase, ty: TyId<'_>) -> Result<AbiTypeDesc, String> {
    abi_ty::semantic_ty_to_abi_desc(db, ty).map_err(|err| err.to_string())
}

fn struct_from_ty<'db>(db: &'db DriverDataBase, ty: TyId<'db>) -> Option<Struct<'db>> {
    if let Some((_, inner)) = ty.as_capability(db) {
        return struct_from_ty(db, inner);
    }
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => match adt.adt_ref(db) {
            AdtRef::Struct(struct_) => Some(struct_),
            AdtRef::Enum(_) => None,
        },
        TyData::QualifiedTy(trait_inst) => struct_from_ty(db, trait_inst.self_ty(db)),
        _ => None,
    }
}

fn parse_function_signature(signature: &str) -> Result<ParsedFunctionSignature, String> {
    abi_ty::parse_function_signature(signature)
        .map_err(|err| format!("cannot emit JSON ABI for `{signature}`: {err}"))
}

fn ensure_selector_matches_signature(
    source_signature: &str,
    parsed_signature: &ParsedFunctionSignature,
    inputs: &[NamedAbiParamDesc],
    actual_selector: u32,
) -> Result<(), String> {
    if parsed_signature.arg_types.len() != inputs.len() {
        return Err(format!(
            "cannot emit JSON ABI for `{source_signature}`: selector arity {} does not match semantic arity {}",
            parsed_signature.arg_types.len(),
            inputs.len()
        ));
    }

    for (selector_ty, input) in parsed_signature.arg_types.iter().zip(inputs) {
        if selector_ty != &input.desc.canonical_type {
            let mut message = format!(
                "cannot emit JSON ABI for `{source_signature}`: selector argument type `{selector_ty}` does not match semantic ABI type `{}`",
                input.desc.canonical_type
            );
            if let Some(suggestion) = abi_ty::suggested_fe_type_for_sol_type(selector_ty) {
                message.push_str(&format!(
                    "; `{suggestion}` decodes and encodes Solidity `{selector_ty}`"
                ));
            }
            return Err(message);
        }
    }

    let canonical_signature = canonical_function_signature(&parsed_signature.name, inputs);
    let expected_selector = selector_for_signature(&canonical_signature);
    if actual_selector != expected_selector {
        return Err(format!(
            "cannot emit JSON ABI for `{source_signature}`: non-canonical selector 0x{actual_selector:08x} does not match canonical signature `{canonical_signature}` (0x{expected_selector:08x})"
        ));
    }
    Ok(())
}

fn canonical_function_signature(fn_name: &str, inputs: &[NamedAbiParamDesc]) -> String {
    let mut signature = String::new();
    signature.push_str(fn_name);
    signature.push('(');
    for (idx, input) in inputs.iter().enumerate() {
        if idx > 0 {
            signature.push(',');
        }
        signature.push_str(&input.desc.canonical_type);
    }
    signature.push(')');
    signature
}

fn selector_for_signature(signature: &str) -> u32 {
    let mut hasher = Keccak::v256();
    let mut output = [0u8; 32];
    hasher.update(signature.as_bytes());
    hasher.finalize(&mut output);
    u32::from_be_bytes([output[0], output[1], output[2], output[3]])
}

fn resolve_sol_abi_ty<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
) -> Result<TyId<'db>, String> {
    let ingot = scope.ingot(db);
    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let sol_path = PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "abi".to_string()))
        .push_ident(db, IdentId::new(db, "Sol".to_string()));

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, sol_path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => Ok(ty),
        Ok(other) => Err(format!(
            "expected `std::abi::Sol` to resolve to a type, got `{other:?}`"
        )),
        Err(err) => Err(format!("failed to resolve `std::abi::Sol`: {err:?}")),
    }
}

/// Derive ABI state mutability from the effective recv-arm effect requirement set.
fn derive_state_mutability(
    db: &DriverDataBase,
    arm_view: hir::semantic::RecvArmView<'_>,
) -> String {
    let arm = arm_view
        .arm(db)
        .expect("recv arm should exist during ABI generation");
    if arm.is_payable(db) {
        return "payable".to_string();
    }

    // EVM memory is local to the call. Reading or writing RawMem cannot
    // observe or mutate chain state and must not change ABI mutability.
    let raw_mem = resolve_lib_trait_path(
        db,
        arm_view.contract(db).scope(),
        "std::evm::effects::RawMem",
    );
    let effects = arm_view.effective_effect_requirements(db);
    let effects: Vec<_> = effects
        .iter()
        .filter(|effect| {
            !effect
                .key
                .key_trait()
                .is_some_and(|inst| Some(inst.def(db)) == raw_mem)
        })
        .collect();
    if effects.is_empty() {
        "pure".to_string()
    } else if effects.iter().any(|effect| effect.is_mut) {
        "nonpayable".to_string()
    } else {
        "view".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use serde_json::Value;

    fn generate_test_abi_result(code: &str, contract_name: &str) -> Result<AbiResult, String> {
        let temp = tempfile::tempdir().expect("create temp dir");
        let file_path = temp.path().join("test.fe");
        let url = url::Url::from_file_path(&file_path).expect("file path to url");
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(&mut db, url, Some(code.to_string()));
        let top_mod = db.top_mod(file);
        generate_contract_abi(&db, top_mod, contract_name)?
            .ok_or_else(|| format!("contract `{contract_name}` not found"))
    }

    fn generate_test_abi(code: &str, contract_name: &str) -> Result<String, String> {
        generate_test_abi_result(code, contract_name).map(|r| r.json)
    }

    fn abi_entries(code: &str, contract_name: &str) -> Vec<Value> {
        let abi = generate_test_abi(code, contract_name).expect("generate abi");
        serde_json::from_str(&abi).expect("parse abi json")
    }

    #[test]
    fn hex_selector_skips_arm_with_warning() {
        let code = r#"
msg FooMsg {
    #[selector = 0x12345678]
    Ping -> u256,
}

pub contract Foo {
    recv FooMsg {
        Ping -> u256 {
            return 1
        }
    }
}
"#;

        let result = generate_test_abi_result(code, "Foo").expect("ABI generation should succeed");
        let entries: Vec<Value> = serde_json::from_str(&result.json).expect("parse abi json");
        assert!(
            entries.iter().all(|e| e["type"] != "function"),
            "hex-selector arm should be skipped"
        );
        assert_eq!(result.warnings.len(), 1);
        assert!(
            result.warnings[0].contains("selector signature is unknown"),
            "unexpected warning: {}",
            result.warnings[0]
        );
    }

    #[test]
    fn sol_selector_alias_uses_source_function_name() {
        let code = r#"
use std::abi::sol

msg FooMsg {
    #[selector = sol("foo(uint256)")]
    Bar { value: u256 } -> u256,
}

pub contract Foo {
    recv FooMsg {
        Bar { value } -> u256 {
            value
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "foo");
        assert_eq!(function["inputs"][0]["type"], "uint256");
    }

    #[test]
    fn sol_fixed_bytes_wrappers_emit_canonical_types() {
        let code = r#"
use std::abi::{FixedBytes, sol}

msg FooMsg {
    #[selector = sol("check(bytes4,bytes32)")]
    Check { interface_id: FixedBytes<4>, root: FixedBytes<32> } -> bool,
}

pub contract Foo {
    recv FooMsg {
        Check { interface_id, root } -> bool {
            interface_id.val == 0x01ffc9a7 && root.val != 0
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "check");
        assert_eq!(function["inputs"][0]["type"], "bytes4");
        assert_eq!(function["inputs"][1]["type"], "bytes32");
        assert_eq!(function["outputs"][0]["type"], "bool");
    }

    #[test]
    fn sol_fixed_bytes_aliases_emit_canonical_types() {
        let code = r#"
use std::abi::{Bytes1, Bytes4, Bytes32, sol}

msg FooMsg {
    #[selector = sol("check(bytes1,bytes4,bytes32)")]
    Check { prefix: Bytes1, interface_id: Bytes4, root: Bytes32 } -> bool,
}

pub contract Foo {
    recv FooMsg {
        Check { prefix, interface_id, root } -> bool {
            prefix.val == 0xab && interface_id.val == 0x01ffc9a7 && root.val != 0
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "check");
        assert_eq!(function["inputs"][0]["type"], "bytes1");
        assert_eq!(function["inputs"][1]["type"], "bytes4");
        assert_eq!(function["inputs"][2]["type"], "bytes32");
        assert_eq!(function["outputs"][0]["type"], "bool");
    }

    #[test]
    fn selector_const_alias_preserves_signature() {
        let code = r#"
use std::abi::sol

const PING: u32 = sol("ping()")

msg FooMsg {
    #[selector = PING]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn selector_const_block_preserves_signature() {
        let code = r#"
use std::abi::sol

const PING: u32 = { sol("ping()") }

msg FooMsg {
    #[selector = PING]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn selector_const_block_local_alias_preserves_signature() {
        let code = r#"
use std::abi::sol

const PING: u32 = {
    let x = sol("ping()");
    x
}

msg FooMsg {
    #[selector = PING]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn selector_const_block_local_alias_chain_preserves_signature() {
        let code = r#"
use std::abi::sol

const PING: u32 = {
    let x = sol("ping()");
    let y = x;
    y
}

msg FooMsg {
    #[selector = PING]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn selector_const_fn_preserves_signature() {
        let code = r#"
use std::abi::sol

const fn ping_selector() -> u32 { sol("ping()") }

msg FooMsg {
    #[selector = ping_selector()]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn sol_selector_preserves_source_casing() {
        let code = r#"
use std::abi::sol

msg FooMsg {
    #[selector = sol("urlValue()")]
    URLValue,
}

pub contract Foo {
    recv FooMsg {
        URLValue {
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "urlValue");
        assert_eq!(
            function["inputs"].as_array().expect("inputs array").len(),
            0
        );
    }

    #[test]
    fn manual_generic_recv_variants_are_skipped_with_warning() {
        let code = r#"
use std::abi::sol

struct GenericMsg<T> {
    pub value: T,
}

impl<T> core::abi::AbiSize for GenericMsg<T>
    where T: core::abi::AbiSize
{
    const HEAD_SIZE: u256 = T::HEAD_SIZE
    const IS_DYNAMIC: bool = T::IS_DYNAMIC

    fn payload_size(self) -> u256 {
        Self::HEAD_SIZE + core::abi::dynamic_payload_size(self.value)
    }
}

impl<T> core::abi::Encode<std::abi::Sol> for GenericMsg<T>
    where T: core::abi::Encode<std::abi::Sol>
{
    fn encode(own self, _ ptr: *u8) {
        let Self { value } = self
        value.encode(ptr)
    }
}

impl<T> core::abi::Decode<std::abi::Sol> for GenericMsg<T>
    where T: core::abi::Decode<std::abi::Sol>
{
    fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
        let value = T::decode_payload(d)
        Self { value }
    }
}

impl core::message::MsgVariant<std::abi::Sol> for GenericMsg<u8> {
    const SELECTOR: u32 = sol("genericMsg(uint8)")
    type Return = u8
}

impl core::message::MsgVariant<std::abi::Sol> for GenericMsg<u16> {
    const SELECTOR: u32 = sol("genericMsg(uint16)")
    type Return = u16
}

pub contract GenericRecvContract {
    recv {
        GenericMsg<u8> { value } -> u8 uses () {
            value
        }
        GenericMsg<u16> { value } -> u16 uses () {
            value
        }
    }
}
"#;

        let result = generate_test_abi_result(code, "GenericRecvContract")
            .expect("ABI generation should succeed");
        let entries: Vec<Value> = serde_json::from_str(&result.json).expect("parse abi json");
        assert!(
            entries.iter().all(|entry| entry["type"] != "function"),
            "manual generic MsgVariant impls should be skipped"
        );
        assert_eq!(result.warnings.len(), 2);
        assert!(
            result
                .warnings
                .iter()
                .all(|warning| warning.contains("manual `MsgVariant` impls")),
            "unexpected warnings: {:?}",
            result.warnings
        );
    }

    #[test]
    fn manual_msg_variant_impls_are_skipped_with_warning() {
        let code = r#"
use std::abi::sol

struct Weird {
    pub amount: u64,
    pub flag: bool,
}

impl core::abi::Encode<std::abi::Sol> for Weird {
    fn encode(own self, _ ptr: *u8) {
        self.flag.encode(ptr)
        self.amount.encode(core::ptr::offset_bytes(ptr, 32))
    }
}

impl core::abi::Decode<std::abi::Sol> for Weird {
    fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
        let flag = bool::decode_payload(d)
        let amount = u64::decode_payload(d)
        Self { amount, flag }
    }
}

impl core::message::MsgVariant<std::abi::Sol> for Weird {
    const SELECTOR: u32 = sol("foo(bool,uint64)")
    type Return = ()
}

pub contract Foo {
    recv {
        Weird { amount, flag } uses () {
            let _ = amount
            let _ = flag
        }
    }
}
"#;

        let result = generate_test_abi_result(code, "Foo").expect("ABI generation should succeed");
        let entries: Vec<Value> = serde_json::from_str(&result.json).expect("parse abi json");
        assert!(
            entries.iter().all(|e| e["type"] != "function"),
            "manual MsgVariant should be skipped"
        );
        assert_eq!(result.warnings.len(), 1);
        assert!(
            result.warnings[0].contains("manual `MsgVariant` impls"),
            "unexpected warning: {}",
            result.warnings[0]
        );
    }

    #[test]
    fn manual_module_msg_variants_are_skipped_with_warning() {
        let code = r#"
mod TokenMsg {
    use std::abi::sol

    pub struct Transfer {
        pub to: u64,
        pub amount: u64,
    }

    impl core::abi::AbiSize for Transfer {
        const HEAD_SIZE: u256 = 64
        const IS_DYNAMIC: bool = false
    }

    impl core::abi::Encode<std::abi::Sol> for Transfer {
        fn encode(own self, _ ptr: *u8) {
            self.to.encode(ptr)
            self.amount.encode(core::ptr::offset_bytes(ptr, 32))
        }
    }

    impl core::abi::Decode<std::abi::Sol> for Transfer {
        fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
            let to = u64::decode_payload(d)
            let amount = u64::decode_payload(d)
            Self { to, amount }
        }
    }

    impl core::message::MsgVariant<std::abi::Sol> for Transfer {
        const SELECTOR: u32 = sol("transfer(uint64,uint64)")
        type Return = bool
    }
}

pub contract Foo {
    recv TokenMsg {
        Transfer { to, amount } -> bool uses () {
            let _ = to
            let _ = amount
            true
        }
    }
}
"#;

        let result = generate_test_abi_result(code, "Foo").expect("ABI generation should succeed");
        let entries: Vec<Value> = serde_json::from_str(&result.json).expect("parse abi json");
        assert!(
            entries.iter().all(|e| e["type"] != "function"),
            "manual MsgVariant module should be skipped"
        );
        assert_eq!(result.warnings.len(), 1);
        assert!(
            result.warnings[0].contains("manual `MsgVariant` impls"),
            "unexpected warning: {}",
            result.warnings[0]
        );
    }

    #[test]
    fn constructed_events_without_emit_are_not_included() {
        let code = r#"
use std::abi::sol

#[event]
struct Transfer {
    value: u256,
}

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping uses () {
            helper()
        }
    }
}

fn helper() {
    let _ = Transfer { value: 1 }
}
"#;

        let entries = abi_entries(code, "Foo");
        assert!(
            entries
                .iter()
                .all(|entry| !(entry["type"] == "event" && entry["name"] == "Transfer")),
            "constructed-but-not-emitted event should be absent: {entries:?}"
        );
    }

    #[test]
    fn recv_variant_resolution_survives_same_name_event_structs() {
        let code = r#"
use std::abi::sol

msg Erc20 {
    #[selector = sol("transfer(uint256,uint256)")]
    Transfer { to: u256, amount: u256 } -> bool,
}

#[event]
struct Transfer {
    #[indexed]
    from: u256,
    #[indexed]
    to: u256,
    value: u256,
}

pub contract C {
    recv Erc20 {
        Transfer { to, amount } -> bool {
            let _ = to
            let _ = amount
            true
        }
    }
}
"#;

        let entries = abi_entries(code, "C");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");
        assert_eq!(function["name"], "transfer");
        assert_eq!(function["inputs"][0]["name"], "to");
        assert_eq!(function["inputs"][1]["name"], "amount");
    }

    #[test]
    fn event_abi_preserves_indexed_fields() {
        let code = r#"
use std::abi::sol
use std::evm::Log

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

#[event]
struct Transfer {
    #[indexed]
    from: u256,
    #[indexed]
    to: u256,
    value: u256,
}

pub contract C uses (log: mut Log) {
    recv FooMsg {
        Ping uses (mut log) {
            log.emit(Transfer { from: 1, to: 2, value: 3 })
        }
    }
}
"#;

        let entries = abi_entries(code, "C");
        let event = entries
            .iter()
            .find(|entry| entry["type"] == "event" && entry["name"] == "Transfer")
            .expect("event entry");

        assert_eq!(event["anonymous"], false);
        assert_eq!(event["inputs"][0]["name"], "from");
        assert_eq!(event["inputs"][0]["indexed"], true);
        assert_eq!(event["inputs"][1]["name"], "to");
        assert_eq!(event["inputs"][1]["indexed"], true);
        assert_eq!(event["inputs"][2]["name"], "value");
        assert_eq!(event["inputs"][2]["indexed"], false);
    }

    #[test]
    fn tuple_and_fixed_array_types_emit_components_and_lengths() {
        let code = r#"
use std::abi::sol

msg TupleMsg {
    #[selector = sol("setPair((uint64,bool),uint256[2])")]
    SetPair { pair: (u64, bool), values: [u256; 2] } -> (u64, bool),
}

pub contract TupleContract {
    recv TupleMsg {
        SetPair { pair, values } -> (u64, bool) uses () {
            let _ = values
            pair
        }
    }
}
"#;

        let entries = abi_entries(code, "TupleContract");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["inputs"][0]["type"], "tuple");
        assert_eq!(function["inputs"][0]["components"][0]["type"], "uint64");
        assert_eq!(function["inputs"][0]["components"][1]["type"], "bool");
        assert_eq!(function["inputs"][1]["type"], "uint256[2]");
        assert_eq!(function["outputs"][0]["type"], "tuple");
        assert_eq!(function["outputs"][0]["components"][0]["type"], "uint64");
        assert_eq!(function["outputs"][0]["components"][1]["type"], "bool");
    }

    #[test]
    fn recv_mutability_uses_effective_contract_scoped_bindings() {
        let code = r#"
use std::abi::sol
use std::evm::{Address, Call, Ctx}

msg FooMsg {
    #[selector = sol("who()")]
    Who -> u256,
    #[selector = sol("ping(address)")]
    Ping { b: Address },
}

msg BarMsg {
    #[selector = sol("pong()")]
    Pong,
}

pub contract Foo uses (ctx: Ctx, call: mut Call) {
    recv FooMsg {
        Who -> u256 uses (ctx) {
            ctx.caller().inner
        }

        Ping { b } uses (call) {
            call.call(addr: b, gas: 100000, value: 0, message: BarMsg::Pong {})
        }
    }
}

pub contract Bar {
    recv BarMsg {
        Pong {}
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let who = entries
            .iter()
            .find(|entry| entry["type"] == "function" && entry["name"] == "who")
            .expect("who entry");
        let ping = entries
            .iter()
            .find(|entry| entry["type"] == "function" && entry["name"] == "ping")
            .expect("ping entry");

        assert_eq!(who["stateMutability"], "view");
        assert_eq!(ping["stateMutability"], "nonpayable");
    }

    #[test]
    fn raw_staticcall_supports_view_abi_with_readonly_call_effect() {
        let code = r#"
use core::ptr::MemBuffer
use std::abi::sol
use std::evm::{Address, Call}
msg ProbeMsg {
    #[selector = sol("probe(address)")]
    Probe { target: Address } -> bool,
}
pub contract Probe {
    recv ProbeMsg {
        Probe { target } -> bool uses (call: Call) {
            let args = MemBuffer::alloc(0)
            let mut ret = MemBuffer::with_capacity(64)
            call.raw_staticcall(addr: target, gas: 30000, args: args.span(), ret: mut ret).success()
        }
    }
}
"#;
        let entries = abi_entries(code, "Probe");
        let probe = entries
            .iter()
            .find(|e| e["name"] == "probe")
            .expect("probe entry");
        assert_eq!(probe["stateMutability"], "view");
    }

    #[test]
    fn payable_constructor_and_recv_arms_preserve_abi_mutability_and_array_inputs() {
        let code = r#"
use std::abi::sol

msg WalletMsg {
    #[selector = sol("fund()")]
    Fund,

    #[selector = sol("peek()")]
    Peek -> u256,
}

pub contract Wallet {
    #[payable]
    init(seed: u256, values: [u256; 2]) {}

    recv WalletMsg {
        #[payable]
        Fund {} {}

        Peek -> u256 {
            7
        }
    }
}
"#;

        let entries = abi_entries(code, "Wallet");
        let constructor = entries
            .iter()
            .find(|entry| entry["type"] == "constructor")
            .expect("constructor entry");
        let fund = entries
            .iter()
            .find(|entry| entry["type"] == "function" && entry["name"] == "fund")
            .expect("fund entry");
        let peek = entries
            .iter()
            .find(|entry| entry["type"] == "function" && entry["name"] == "peek")
            .expect("peek entry");

        assert_eq!(constructor["stateMutability"], "payable");
        assert_eq!(constructor["inputs"][0]["name"], "seed");
        assert_eq!(constructor["inputs"][0]["type"], "uint256");
        assert_eq!(constructor["inputs"][1]["name"], "values");
        assert_eq!(constructor["inputs"][1]["type"], "uint256[2]");

        assert_eq!(fund["stateMutability"], "payable");
        assert_eq!(peek["stateMutability"], "pure");
        assert_eq!(peek["outputs"][0]["type"], "uint256");
    }

    #[test]
    fn fallback_recv_arm_emits_standard_fallback_abi_entry() {
        let code = r#"
use std::abi::sol

msg WalletMsg {
    #[selector = sol("peek()")]
    Peek -> u256,
}

pub contract Wallet {
    recv {
        WalletMsg::Peek {} -> u256 {
            7
        }

        #[payable]
        _ {}
    }
}
"#;

        let entries = abi_entries(code, "Wallet");
        let fallback = entries
            .iter()
            .find(|entry| entry["type"] == "fallback")
            .expect("fallback entry");

        assert_eq!(fallback["stateMutability"], "payable");
        assert!(fallback.get("name").is_none());
        assert!(fallback.get("inputs").is_none());
        assert!(fallback.get("outputs").is_none());
    }

    #[test]
    fn generic_event_helpers_preserve_concrete_event_types() {
        let code = r#"
use std::abi::sol
use std::evm::effects::Log
use std::evm::event::Event

#[event]
struct Transfer {
    value: u256,
}

#[event]
struct Approval {
    value: u256,
}

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

fn emit_event<E: Event>(event: E) uses (log: mut Log) {
    log.emit(event)
}

pub contract Foo uses (log: mut Log) {
    recv FooMsg {
        Ping uses (mut log) {
            emit_event(Transfer { value: 1 })
            emit_event(Approval { value: 2 })
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        assert!(
            entries
                .iter()
                .any(|entry| entry["type"] == "event" && entry["name"] == "Transfer"),
            "generic helper should preserve Transfer event: {entries:?}"
        );
        assert!(
            entries
                .iter()
                .any(|entry| entry["type"] == "event" && entry["name"] == "Approval"),
            "generic helper should preserve Approval event: {entries:?}"
        );
    }

    #[test]
    fn sol_compat_wrapper_types_emit_correct_abi_type() {
        let code = r#"
use std::abi::sol
use std::abi::sol::Uint160
use std::abi::sol::Int24

msg FooMsg {
    #[selector = sol("set(uint160,int24)")]
    Set { addr: Uint160, value: Int24 },
}

pub contract Foo {
    recv FooMsg {
        Set { addr, value } uses () {
            let _ = addr
            let _ = value
        }
    }
}
"#;

        let entries = abi_entries(code, "Foo");
        let function = entries
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");

        assert_eq!(function["name"], "set");
        assert_eq!(function["inputs"][0]["type"], "uint160");
        assert_eq!(function["inputs"][0]["name"], "addr");
        assert_eq!(function["inputs"][1]["type"], "int24");
        assert_eq!(function["inputs"][1]["name"], "value");
    }
    #[test]
    fn custom_errors_include_reachable_reverts_only() {
        let code = r#"
use std::abi::sol
use std::evm::revert_error
#[error]
struct Failure { code: u256 }
#[error]
struct InitFailure {}
#[error]
struct Unused { code: u256 }
fn fail() { revert_error(Failure { code: 7 }) }
msg M {
    #[selector = sol("fail()")]
    Fail {},
}
pub contract C {
    init(fail: bool) { if fail { revert_error(InitFailure {}) } }
    recv M {
        Fail {} {
            let _ = Unused { code: 3 }
            fail()
        }
    }
}
"#;
        let entries = abi_entries(code, "C");
        let errors: Vec<_> = entries
            .iter()
            .filter(|entry| entry["type"] == "error")
            .collect();
        assert_eq!(errors.len(), 2, "{entries:?}");
        let failure = errors
            .iter()
            .find(|entry| entry["name"] == "Failure")
            .unwrap();
        assert_eq!(
            failure["inputs"],
            serde_json::json!([{"name":"code","type":"uint256"}])
        );
        assert!(failure.get("anonymous").is_none());
        assert!(failure.get("stateMutability").is_none());
    }
    #[test]
    fn custom_errors_include_panic_with_value() {
        let entries = abi_entries(
            r#"
use std::abi::sol
#[error]
struct Failure { code: u256 }
msg M {
    #[selector = sol("fail()")]
    Fail {}
}
pub contract C {
    recv M { Fail {} { core::panic_with_value(Failure { code: 7 }) } }
}
"#,
            "C",
        );
        let errors: Vec<_> = entries
            .iter()
            .filter(|entry| entry["type"] == "error")
            .collect();
        assert_eq!(errors.len(), 1, "{entries:?}");
        assert_eq!(errors[0]["name"], "Failure");
        assert_eq!(
            errors[0]["inputs"],
            serde_json::json!([{"name":"code","type":"uint256"}])
        );
    }

    #[test]
    fn custom_errors_include_result_unwrap() {
        let entries = abi_entries(
            r#"
use core::Result
use std::abi::sol
#[error]
struct Failure { code: u256 }
fn result() -> Result<Failure, u256> { Result::Err(Failure { code: 7 }) }
msg M {
    #[selector = sol("fail()")]
    Fail {} -> u256
}
pub contract C {
    recv M { Fail {} -> u256 { result().unwrap() } }
}
"#,
            "C",
        );
        let errors: Vec<_> = entries
            .iter()
            .filter(|entry| entry["type"] == "error")
            .collect();
        assert_eq!(errors.len(), 1, "{entries:?}");
        assert_eq!(errors[0]["name"], "Failure");
        assert_eq!(
            errors[0]["inputs"],
            serde_json::json!([{"name":"code","type":"uint256"}])
        );
    }

    #[test]
    fn custom_errors_include_overloaded_operators() {
        let entries = abi_entries(
            r#"
use core::ops::Add
use std::abi::sol
use std::evm::revert_error
#[error]
struct AddFailure { code: u256 }
struct Number { value: u256 }
impl Add for Number {
    fn add(own self, _ other: own Number) -> Number {
        revert_error(AddFailure { code: self.value })
    }
}
msg M {
    #[selector = sol("fail()")]
    Fail {} -> u256
}
pub contract C {
    recv M {
        Fail {} -> u256 {
            let a = Number { value: 1 }
            let b = Number { value: 2 }
            (a + b).value
        }
    }
}
"#,
            "C",
        );
        let errors: Vec<_> = entries
            .iter()
            .filter(|entry| entry["type"] == "error")
            .collect();
        assert_eq!(errors.len(), 1, "{entries:?}");
        assert_eq!(errors[0]["name"], "AddFailure");
        assert_eq!(
            errors[0]["inputs"],
            serde_json::json!([{"name":"code","type":"uint256"}])
        );
    }

    #[test]
    fn custom_errors_include_generic_operator_helpers() {
        let entries = abi_entries(
            r#"
use core::ops::Add
use std::abi::sol
use std::evm::revert_error
#[error]
struct AddFailure { code: u256 }
struct Number<T> { value: u256, tag: T }
impl<T> Add for Number<T> {
    fn add(own self, _ other: own Number<T>) -> Number<T> {
        revert_error(AddFailure { code: self.value })
    }
}
fn add<T: Add>(_ lhs: own T, _ rhs: own T) -> T::Output { lhs + rhs }
msg M {
    #[selector = sol("fail()")]
    Fail {} -> u256
}
pub contract C {
    recv M {
        Fail {} -> u256 {
            let a = Number { value: 1, tag: true }
            let b = Number { value: 2, tag: false }
            add(a, b).value
        }
    }
}
"#,
            "C",
        );
        let errors: Vec<_> = entries
            .iter()
            .filter(|entry| entry["type"] == "error")
            .collect();
        assert_eq!(errors.len(), 1, "{entries:?}");
        assert_eq!(errors[0]["name"], "AddFailure");
        assert_eq!(
            errors[0]["inputs"],
            serde_json::json!([{"name":"code","type":"uint256"}])
        );
    }

    #[test]
    fn custom_errors_ignore_non_error_panics_and_user_named_helpers() {
        let entries = abi_entries(
            r#"
use std::abi::sol
#[error]
struct Unused {}
fn panic_with_value(_ value: own Unused) {}
msg M {
    #[selector = sol("fail()")]
    Fail {}
}
pub contract C {
    recv M {
        Fail {} {
            panic_with_value(Unused {})
            core::panic_with_value(7 as u256)
        }
    }
}
"#,
            "C",
        );
        assert!(
            !entries.iter().any(|entry| entry["type"] == "error"),
            "{entries:?}"
        );
    }
    #[test]
    fn memory_effects_do_not_change_abi_state_mutability() {
        let code = r#"
use std::abi::sol
use std::evm::{RawMem, RawStorage}
msg M {
    #[selector = sol("memoryOnly()")]
    MemoryOnly {},
    #[selector = sol("readMemory()")]
    ReadMemory {},
    #[selector = sol("readStorage()")]
    ReadStorage {},
    #[selector = sol("writeStorage()")]
    WriteStorage {},
}
pub contract C {
    recv M {
        MemoryOnly {} uses (mem: mut RawMem) {}
        ReadMemory {} uses (mem: RawMem) {}
        ReadStorage {} uses (mem: mut RawMem, storage: RawStorage) {}
        WriteStorage {} uses (mem: mut RawMem, storage: mut RawStorage) {}
    }
}
"#;
        let entries = abi_entries(code, "C");
        for (name, expected) in [
            ("memoryOnly", "pure"),
            ("readMemory", "pure"),
            ("readStorage", "view"),
            ("writeStorage", "nonpayable"),
        ] {
            let entry = entries.iter().find(|entry| entry["name"] == name).unwrap();
            assert_eq!(entry["stateMutability"], expected);
        }
    }

    #[test]
    fn abi_export_traverses_typed_calldata_encoding() {
        let entries = abi_entries(
            r#"// ABI-only export panics in instantiate_callable_typed_body on Fe aad737010.
use std::abi::sol
use std::evm::{Address, encode_msg_calldata}
msg TokenMsg {
    #[selector = sol("transfer(address,uint256)")]
    Transfer { to: Address, amount: u256 } -> bool,
}
msg ProbeMsg {
    #[selector = sol("probe()")]
    Probe {},
}
pub contract EncodeMsgAbiProbe {
    recv ProbeMsg {
        Probe {} {
            let encoded = encode_msg_calldata(
                TokenMsg::Transfer { to: Address { inner: 1 }, amount: 1 },
            )
        }
    }
}
"#,
            "EncodeMsgAbiProbe",
        );
        assert!(entries.iter().any(|e| e["name"] == "probe"));
    }
}
