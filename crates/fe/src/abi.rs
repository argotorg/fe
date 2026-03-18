use std::collections::HashSet;

use common::ingot::IngotKind;
use driver::DriverDataBase;
use hir::{
    analysis::{
        name_resolution::{PathRes, resolve_path},
        ty::{
            adt_def::AdtRef,
            binder::Binder,
            const_ty::{ConstTyData, EvaluatedConstTy},
            fold::{AssocTySubst, TyFoldable},
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        FieldDefListId, Func, IdentId, PathId, Struct, TopLevelMod, Trait, scope_graph::ScopeId,
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
struct AbiTypeDesc {
    abi_type: String,
    canonical_type: String,
    components: Option<Vec<AbiParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct NamedAbiParamDesc {
    name: String,
    indexed: Option<bool>,
    desc: AbiTypeDesc,
}

impl AbiTypeDesc {
    fn simple(ty: &str) -> Self {
        Self {
            abi_type: ty.to_string(),
            canonical_type: ty.to_string(),
            components: None,
        }
    }

    fn tuple(components: Vec<AbiParam>, canonical_type: String) -> Self {
        Self {
            abi_type: "tuple".to_string(),
            canonical_type,
            components: Some(components),
        }
    }

    fn array(self, len: &str) -> Self {
        Self {
            abi_type: format!("{}[{len}]", self.abi_type),
            canonical_type: format!("{}[{len}]", self.canonical_type),
            components: self.components,
        }
    }
}

impl NamedAbiParamDesc {
    fn into_param(self) -> AbiParam {
        AbiParam {
            name: self.name,
            ty: self.desc.abi_type,
            indexed: self.indexed,
            components: self.desc.components,
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

    for struct_ in collect_contract_event_structs(db, contract) {
        entries.push(event_struct_to_abi_entry(db, struct_)?);
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
    arm_view
        .arm(db)
        .ok_or_else(|| "missing recv arm during ABI generation".to_string())?;
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
        Some(field.attributes.has_attr(db, "indexed"))
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
        anonymous: None,
    })
}

fn collect_contract_event_structs<'db>(
    db: &'db DriverDataBase,
    contract: hir::hir_def::Contract<'db>,
) -> Vec<Struct<'db>> {
    let mut events = Vec::new();
    let mut seen = HashSet::new();
    let mut visited_funcs = HashSet::new();
    let emit_traits = resolve_event_emit_traits(db, contract.scope());

    if contract.init(db).is_some() {
        let (_, typed_body) = hir::analysis::ty::ty_check::check_contract_init_body(db, contract);
        collect_typed_body_event_structs(
            db,
            typed_body,
            &emit_traits,
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
                &mut events,
                &mut seen,
                &mut visited_funcs,
            );
        }
    }

    events
}

fn collect_typed_body_event_structs<'db>(
    db: &'db DriverDataBase,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    emit_traits: &EventEmitTraits<'db>,
    out: &mut Vec<Struct<'db>>,
    seen: &mut HashSet<Struct<'db>>,
    visited_funcs: &mut HashSet<VisitedFuncBody<'db>>,
) {
    let Some(body) = typed_body.body() else {
        return;
    };

    for (expr_id, partial_expr) in body.exprs(db).iter() {
        let hir::hir_def::Partial::Present(expr) = partial_expr else {
            continue;
        };
        if let Some(struct_) = emitted_event_struct(db, typed_body, body, expr_id, emit_traits) {
            push_event_struct(db, out, seen, struct_);
        }

        if matches!(
            expr,
            hir::hir_def::Expr::Call(..) | hir::hir_def::Expr::MethodCall(..)
        ) && let Some(callable) = typed_body.callable_expr(expr_id)
            && let hir::hir_def::CallableDef::Func(func) = callable.callable_def
            && visited_funcs.insert(VisitedFuncBody::from_callable(func, callable))
            && func.body(db).is_some()
        {
            let (_, func_typed_body) = hir::analysis::ty::ty_check::check_func_body(db, func);
            let func_typed_body =
                instantiate_callable_typed_body(db, func_typed_body.clone(), callable);
            collect_typed_body_event_structs(
                db,
                &func_typed_body,
                emit_traits,
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
    callable: &hir::analysis::ty::ty_check::Callable<'db>,
) -> hir::analysis::ty::ty_check::TypedBody<'db> {
    let mut typed_body = Binder::bind(typed_body).instantiate(db, callable.generic_args());
    if let Some(trait_inst) = callable.trait_inst() {
        let mut subst = AssocTySubst::new(trait_inst);
        typed_body = typed_body.fold_with(db, &mut subst);
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
    if let Some((_, inner)) = ty.as_capability(db) {
        return semantic_ty_to_abi_desc(db, inner);
    }

    if ty == TyId::unit(db) {
        return Err("unit type is not a valid external ABI parameter".to_string());
    }

    if ty.is_tuple(db) {
        let components = ty.field_types(db);
        let component_descs: Vec<_> = components
            .into_iter()
            .map(|field_ty| semantic_ty_to_abi_desc(db, field_ty))
            .collect::<Result<_, _>>()?;
        return Ok(AbiTypeDesc::tuple(
            component_descs
                .iter()
                .map(|desc| AbiParam {
                    name: String::new(),
                    ty: desc.abi_type.clone(),
                    indexed: None,
                    components: desc.components.clone(),
                })
                .collect(),
            canonical_tuple_type(&component_descs),
        ));
    }

    if ty.is_array(db) {
        let (_, args) = ty.decompose_ty_app(db);
        let elem_ty = args
            .first()
            .copied()
            .ok_or_else(|| "array type is missing its element type".to_string())?;
        let len_ty = args
            .get(1)
            .copied()
            .ok_or_else(|| "array type is missing its length".to_string())?;
        let elem_desc = semantic_ty_to_abi_desc(db, elem_ty)?;
        let len = array_len_to_string(db, len_ty)?;
        return Ok(elem_desc.array(&len));
    }

    if ty.is_string(db) {
        return Ok(AbiTypeDesc::simple("string"));
    }

    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            PrimTy::Bool => Ok(AbiTypeDesc::simple("bool")),
            PrimTy::U8 => Ok(AbiTypeDesc::simple("uint8")),
            PrimTy::U16 => Ok(AbiTypeDesc::simple("uint16")),
            PrimTy::U32 => Ok(AbiTypeDesc::simple("uint32")),
            PrimTy::U64 => Ok(AbiTypeDesc::simple("uint64")),
            PrimTy::U128 => Ok(AbiTypeDesc::simple("uint128")),
            PrimTy::U256 | PrimTy::Usize => Ok(AbiTypeDesc::simple("uint256")),
            PrimTy::I8 => Ok(AbiTypeDesc::simple("int8")),
            PrimTy::I16 => Ok(AbiTypeDesc::simple("int16")),
            PrimTy::I32 => Ok(AbiTypeDesc::simple("int32")),
            PrimTy::I64 => Ok(AbiTypeDesc::simple("int64")),
            PrimTy::I128 => Ok(AbiTypeDesc::simple("int128")),
            PrimTy::I256 | PrimTy::Isize => Ok(AbiTypeDesc::simple("int256")),
            PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) => unreachable!(),
            PrimTy::Ptr | PrimTy::View | PrimTy::BorrowMut | PrimTy::BorrowRef => {
                Err(format!("unsupported ABI type `{}`", ty.pretty_print(db)))
            }
        },
        TyData::TyBase(TyBase::Adt(adt)) => {
            let adt_ref = adt.adt_ref(db);
            if is_std_address_ty(db, ty, adt_ref) {
                return Ok(AbiTypeDesc::simple("address"));
            }
            if let Some(sol_type) = std_sol_compat_abi_type(db, ty, adt_ref) {
                return Ok(AbiTypeDesc::simple(&sol_type));
            }
            match adt_ref {
                AdtRef::Struct(struct_) => {
                    let component_descs = struct_ty_to_abi_param_descs(db, struct_, ty, |_| None)?;
                    let components: Vec<_> = component_descs
                        .iter()
                        .cloned()
                        .map(NamedAbiParamDesc::into_param)
                        .collect();
                    let canonical = canonical_tuple_type(
                        &component_descs
                            .iter()
                            .map(|param| param.desc.clone())
                            .collect::<Vec<_>>(),
                    );
                    Ok(AbiTypeDesc::tuple(components, canonical))
                }
                AdtRef::Enum(_) => Err(format!(
                    "unsupported ABI enum type `{}`",
                    ty.pretty_print(db)
                )),
            }
        }
        TyData::Invalid(_) => Err(format!("unresolved ABI type `{}`", ty.pretty_print(db))),
        _ => Err(format!("unsupported ABI type `{}`", ty.pretty_print(db))),
    }
}

fn array_len_to_string(db: &DriverDataBase, ty: TyId<'_>) -> Result<String, String> {
    match ty.data(db) {
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                Ok(value.data(db).to_string())
            }
            _ => Err(format!(
                "array length `{}` is not a concrete integer",
                ty.pretty_print(db)
            )),
        },
        _ => Err(format!(
            "array length `{}` is not represented as a const type",
            ty.pretty_print(db)
        )),
    }
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

fn is_std_address_ty(db: &DriverDataBase, ty: TyId<'_>, adt_ref: AdtRef<'_>) -> bool {
    let Some(name) = adt_ref.name(db) else {
        return false;
    };
    if name.data(db) != "Address" {
        return false;
    }
    ty.ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Std)
}

/// Recognise `std::abi::sol` SolCompat wrapper types like `Uint160` / `Int24`
/// and return their Solidity ABI type string (e.g. `"uint160"`, `"int24"`).
fn std_sol_compat_abi_type(
    db: &DriverDataBase,
    ty: TyId<'_>,
    adt_ref: AdtRef<'_>,
) -> Option<String> {
    if !ty
        .ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Std)
    {
        return None;
    }
    let name = adt_ref.name(db)?.data(db).to_string();

    // Match Uint{N} or Int{N} where N is a valid Solidity bit width (8..=256, multiple of 8)
    let (prefix, digits) = if let Some(rest) = name.strip_prefix("Uint") {
        ("uint", rest)
    } else if let Some(rest) = name.strip_prefix("Int") {
        ("int", rest)
    } else {
        return None;
    };

    let bits: u16 = digits.parse().ok()?;
    if (8..=256).contains(&bits) && bits.is_multiple_of(8) {
        Some(format!("{prefix}{bits}"))
    } else {
        None
    }
}

fn canonical_tuple_type(component_descs: &[AbiTypeDesc]) -> String {
    let mut out = String::from("(");
    for (idx, desc) in component_descs.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        out.push_str(&desc.canonical_type);
    }
    out.push(')');
    out
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParsedFunctionSignature {
    name: String,
    arg_types: Vec<String>,
}

fn parse_function_signature(signature: &str) -> Result<ParsedFunctionSignature, String> {
    let (name, args) = signature.split_once('(').ok_or_else(|| {
        format!(
            "cannot emit JSON ABI for `{signature}`: selector signature must be of the form `name(type,...)`"
        )
    })?;
    let args = args.strip_suffix(')').ok_or_else(|| {
        format!("cannot emit JSON ABI for `{signature}`: selector signature must end with `)`")
    })?;
    if name.is_empty() || name.trim() != name || name.chars().any(char::is_whitespace) {
        return Err(format!(
            "cannot emit JSON ABI for `{signature}`: selector function name must be a single identifier"
        ));
    }

    Ok(ParsedFunctionSignature {
        name: name.to_string(),
        arg_types: split_signature_args(signature, args)?,
    })
}

fn split_signature_args(signature: &str, args: &str) -> Result<Vec<String>, String> {
    if args.is_empty() {
        return Ok(Vec::new());
    }

    let mut arg_types = Vec::new();
    let mut start = 0;
    let mut tuple_depth = 0usize;
    let mut array_depth = 0usize;

    for (idx, ch) in args.char_indices() {
        match ch {
            '(' => tuple_depth += 1,
            ')' => {
                if tuple_depth == 0 {
                    return Err(format!(
                        "cannot emit JSON ABI for `{signature}`: unbalanced `)` in selector signature"
                    ));
                }
                tuple_depth -= 1;
            }
            '[' => array_depth += 1,
            ']' => {
                if array_depth == 0 {
                    return Err(format!(
                        "cannot emit JSON ABI for `{signature}`: unbalanced `]` in selector signature"
                    ));
                }
                array_depth -= 1;
            }
            ',' if tuple_depth == 0 && array_depth == 0 => {
                let arg = args[start..idx].trim();
                if arg.is_empty() {
                    return Err(format!(
                        "cannot emit JSON ABI for `{signature}`: empty selector argument type"
                    ));
                }
                arg_types.push(arg.to_string());
                start = idx + 1;
            }
            _ => {}
        }
    }

    if tuple_depth != 0 || array_depth != 0 {
        return Err(format!(
            "cannot emit JSON ABI for `{signature}`: unbalanced selector argument list"
        ));
    }

    let last = args[start..].trim();
    if last.is_empty() {
        return Err(format!(
            "cannot emit JSON ABI for `{signature}`: empty selector argument type"
        ));
    }
    arg_types.push(last.to_string());
    Ok(arg_types)
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
            return Err(format!(
                "cannot emit JSON ABI for `{source_signature}`: selector argument type `{selector_ty}` does not match semantic ABI type `{}`",
                input.desc.canonical_type
            ));
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

/// Derive ABI state mutability from the effective recv-arm effect set.
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

    let effects = arm_view.effective_effect_bindings(db);
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

impl<T> core::abi::Encode<std::abi::Sol> for GenericMsg<T>
    where T: core::abi::Encode<std::abi::Sol>
{
    fn encode<E: core::abi::AbiEncoder<std::abi::Sol>>(own self, _ e: mut E) {
        let Self { value } = self
        value.encode(e)
    }
}

impl<T> core::abi::Decode<std::abi::Sol> for GenericMsg<T>
    where T: core::abi::Decode<std::abi::Sol>
{
    fn decode<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
        let value = T::decode(d)
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
    fn encode<E: core::abi::AbiEncoder<std::abi::Sol>>(own self, _ e: mut E) {
        self.flag.encode(e)
        self.amount.encode(e)
    }
}

impl core::abi::Decode<std::abi::Sol> for Weird {
    fn decode<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
        let flag = bool::decode(d)
        let amount = u64::decode(d)
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
use std::abi::sol

mod TokenMsg {
    pub struct Transfer {
        pub to: u64,
        pub amount: u64,
    }

    impl core::abi::Encode<std::abi::Sol> for Transfer {
        fn encode<E: core::abi::AbiEncoder<std::abi::Sol>>(own self, _ e: mut E) {
            self.to.encode(e)
            self.amount.encode(e)
        }
    }

    impl core::abi::Decode<std::abi::Sol> for Transfer {
        fn decode<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
            let to = u64::decode(d)
            let amount = u64::decode(d)
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
}
