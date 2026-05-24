use common::ingot::{Ingot, IngotKind};
use hir::{
    analysis::{
        HirAnalysisDb,
        semantic::{EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance},
        ty::{
            trait_def::TraitInstId,
            ty_check::{
                BodyOwner, EffectParamSite, EffectProviderProvenance, EffectProviderSpecialization,
                LocalBinding,
            },
            ty_def::{TyBase, TyData, TyId},
        },
    },
    hir_def::{CallableDef, ExprId, ItemKind, PatId, TopLevelMod, scope_graph::ScopeId},
    semantic::{ProviderBinding, ProviderSource},
};

pub fn semantic_instance_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    let key = semantic.key(db);
    format!(
        "{}$subst${}$effects${}$impl${}",
        body_owner_identity(db, key.owner(db)),
        generic_subst_identity(db, key.subst(db)),
        effect_provider_subst_identity(db, key.effect_providers(db)),
        impl_env_identity(db, key.impl_env(db))
    )
}

pub fn semantic_instance_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    let key = semantic.key(db);
    format!(
        "{}$subst${}$effects${}$impl${}",
        body_owner_identity(db, key.owner(db)),
        generic_subst_identity(db, key.subst(db)),
        effect_provider_symbol_subst_identity(db, key.effect_providers(db)),
        impl_env_symbol_identity(db, key.impl_env(db))
    )
}

fn body_owner_identity<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> String {
    match owner {
        BodyOwner::Func(func) => {
            let owner_context = semantic_owner_context_identity(db, owner).unwrap_or_default();
            format!("func${}${owner_context}", item_identity(db, func.into()))
        }
        BodyOwner::Const(const_) => format!("const${}", item_identity(db, const_.into())),
        BodyOwner::AnonConstBody { body, expected } => format!(
            "anon_const${}${}",
            module_path_components_for_scope(db, body.scope()).join("$"),
            type_identity(db, expected)
        ),
        BodyOwner::ContractInit { contract } => {
            format!("contract_init${}", item_identity(db, contract.into()))
        }
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "contract_recv${}${recv_idx}${arm_idx}",
            item_identity(db, contract.into())
        ),
    }
}

pub fn semantic_owner_context_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<String> {
    let BodyOwner::Func(func) = owner else {
        return None;
    };
    if let Some(trait_) = func.containing_trait(db) {
        return Some(format!("trait${}", item_name_component(db, trait_.into())));
    }
    if let Some(impl_trait) = func.containing_impl_trait(db) {
        let trait_identity = impl_trait
            .trait_inst(db)
            .map(|trait_inst| trait_identity(db, trait_inst))
            .unwrap_or_else(|| {
                impl_trait
                    .hir_trait_ref(db)
                    .to_opt()
                    .map(|trait_ref| trait_ref.pretty_print(db))
                    .unwrap_or_else(|| "trait".to_string())
            });
        let impl_identity = format!("{trait_identity}${}", type_identity(db, impl_trait.ty(db)));
        return Some(format!(
            "impl_trait${}",
            stable_identity_fingerprint(&impl_identity)
        ));
    }
    func.containing_impl(db).map(|impl_| {
        format!(
            "impl${}",
            stable_identity_fingerprint(&type_identity(db, impl_.ty(db)))
        )
    })
}

fn generic_subst_identity<'db>(db: &'db dyn HirAnalysisDb, subst: GenericSubst<'db>) -> String {
    generic_args_identity(db, subst.generic_args(db))
}

pub fn generic_args_identity<'db>(db: &'db dyn HirAnalysisDb, args: &[TyId<'db>]) -> String {
    args.iter()
        .map(|ty| type_identity(db, *ty))
        .collect::<Vec<_>>()
        .join("$")
}

fn effect_provider_symbol_subst_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    subst: EffectProviderSubst<'db>,
) -> String {
    subst
        .providers(db)
        .iter()
        .map(|provider| effect_provider_symbol_specialization_identity(db, provider))
        .collect::<Vec<_>>()
        .join("$")
}

fn effect_provider_subst_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    subst: EffectProviderSubst<'db>,
) -> String {
    subst
        .providers(db)
        .iter()
        .map(|provider| effect_provider_specialization_identity(db, provider))
        .collect::<Vec<_>>()
        .join("$")
}

fn effect_provider_specialization_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    specialization: &EffectProviderSpecialization<'db>,
) -> String {
    format!(
        "provider${}$provenance${}",
        provider_binding_identity(db, &specialization.provider),
        effect_provider_provenance_identity(db, specialization.provenance)
    )
}

fn effect_provider_symbol_specialization_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    specialization: &EffectProviderSpecialization<'db>,
) -> String {
    provider_binding_symbol_identity(db, &specialization.provider)
}

fn provider_binding_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &ProviderBinding<'db>,
) -> String {
    format!(
        "idx${}$ty${}$mut${}$source${}$semantics${}",
        binding.provider_idx,
        type_identity(db, binding.provider_ty),
        binding.is_mut,
        provider_source_identity(db, &binding.source),
        provider_semantics_identity(db, &binding.semantics)
    )
}

fn provider_binding_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &ProviderBinding<'db>,
) -> String {
    format!(
        "idx${}$mut${}$source${}$semantics${}",
        binding.provider_idx,
        binding.is_mut,
        provider_source_symbol_identity(db, &binding.source),
        provider_semantics_symbol_identity(db, &binding.semantics)
    )
}

fn provider_source_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &ProviderSource<'db>,
) -> String {
    match source {
        ProviderSource::UsesParam {
            site,
            requirement_idx,
        } => format!(
            "uses_param${}${requirement_idx}",
            effect_param_site_identity(db, *site)
        ),
        ProviderSource::ContractField {
            contract,
            field_idx,
        } => format!(
            "contract_field${}${field_idx}",
            item_identity(db, (*contract).into())
        ),
        ProviderSource::RootProvider { site, registration } => format!(
            "root_provider${}$idx${}$kind${:?}$ty${}",
            effect_param_site_identity(db, *site),
            registration.idx,
            registration.site_kind,
            type_identity(db, registration.provider_ty)
        ),
    }
}

fn provider_source_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &ProviderSource<'db>,
) -> String {
    match source {
        ProviderSource::UsesParam {
            site,
            requirement_idx,
        } => format!(
            "uses_param${}${requirement_idx}",
            effect_param_site_identity(db, *site)
        ),
        ProviderSource::ContractField {
            contract,
            field_idx,
        } => format!(
            "contract_field${}${field_idx}",
            item_identity(db, (*contract).into())
        ),
        ProviderSource::RootProvider { site, registration } => format!(
            "root_provider${}$idx${}$kind${:?}",
            effect_param_site_identity(db, *site),
            registration.idx,
            registration.site_kind
        ),
    }
}

fn provider_semantics_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantics: &hir::analysis::ty::ProviderSemantics<'db>,
) -> String {
    let target = semantics
        .target_ty
        .map(|ty| type_identity(db, ty))
        .unwrap_or_default();
    format!(
        "ty${}$kind${:?}$space${:?}$target${target}$transport${:?}",
        type_identity(db, semantics.provider_ty),
        semantics.kind,
        semantics.address_space,
        semantics.transport
    )
}

fn provider_semantics_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantics: &hir::analysis::ty::ProviderSemantics<'db>,
) -> String {
    let target = semantics
        .target_ty
        .map(|ty| type_identity(db, ty))
        .unwrap_or_default();
    format!(
        "kind${:?}$space${:?}$target${target}$transport${:?}",
        semantics.kind, semantics.address_space, semantics.transport
    )
}

fn effect_provider_provenance_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    provenance: EffectProviderProvenance<'db>,
) -> String {
    match provenance {
        EffectProviderProvenance::Binding { owner, binding } => format!(
            "binding${}${}",
            body_owner_identity(db, owner),
            local_binding_identity(db, binding)
        ),
        EffectProviderProvenance::Expr { owner, expr } => {
            format!("expr${}${}", body_owner_identity(db, owner), expr_id(expr))
        }
    }
}

fn local_binding_identity<'db>(db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> String {
    match binding {
        LocalBinding::Local { pat, is_mut } => {
            format!("local${}${is_mut}", pat_id(pat))
        }
        LocalBinding::Param {
            site,
            idx,
            mode,
            ty,
            is_mut,
        } => format!(
            "param${:?}${idx}${mode:?}${}${is_mut}",
            site,
            type_identity(db, ty)
        ),
        LocalBinding::EffectParam {
            site,
            idx,
            binding_name,
            provider_idx,
            is_mut,
            ..
        } => format!(
            "effect_param${}${idx}${}${provider_idx}${is_mut}",
            effect_param_site_identity(db, site),
            binding_name.data(db)
        ),
    }
}

fn impl_env_identity<'db>(db: &'db dyn HirAnalysisDb, env: ImplEnv<'db>) -> String {
    let assumptions = env
        .assumptions(db)
        .list(db)
        .iter()
        .map(|inst| trait_identity(db, *inst))
        .collect::<Vec<_>>()
        .join("$");
    let witnesses = env
        .witnesses(db)
        .iter()
        .map(|inst| trait_identity(db, *inst))
        .collect::<Vec<_>>()
        .join("$");
    format!(
        "scope${}$assumptions${assumptions}$witnesses${witnesses}",
        module_path_components_for_scope(db, env.normalization_scope(db)).join("$")
    )
}

fn impl_env_symbol_identity<'db>(db: &'db dyn HirAnalysisDb, env: ImplEnv<'db>) -> String {
    let assumptions = trait_symbol_identities(db, env.assumptions(db).list(db).iter().copied());
    let witnesses = trait_symbol_identities(db, env.witnesses(db).iter().copied());
    format!(
        "scope${}$assumptions${assumptions}$witnesses${witnesses}",
        module_path_components_for_scope(db, env.normalization_scope(db)).join("$")
    )
}

fn trait_symbol_identities<'db>(
    db: &'db dyn HirAnalysisDb,
    insts: impl Iterator<Item = TraitInstId<'db>>,
) -> String {
    let mut identities = insts
        .filter(|inst| !trait_inst_is_internal_effect_provider_constraint(db, *inst))
        .map(|inst| trait_identity(db, inst))
        .collect::<Vec<_>>();
    identities.sort();
    identities.join("$")
}

fn trait_inst_is_internal_effect_provider_constraint<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    trait_inst_mentions_effect_provider_param(db, trait_inst)
        || trait_inst_is_effect_ref_marker(db, trait_inst)
}

fn trait_inst_is_effect_ref_marker<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    let trait_def = trait_inst.def(db);
    let Some(name) = trait_def.name(db).to_opt() else {
        return false;
    };
    trait_def.top_mod(db).ingot(db).kind(db) == IngotKind::Core
        && matches!(name.data(db).as_str(), "EffectRef" | "EffectRefMut")
        && module_path_components_for_scope(db, trait_def.scope())
            .iter()
            .any(|component| component == "effect_ref")
}

fn trait_inst_mentions_effect_provider_param<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    trait_inst
        .args(db)
        .iter()
        .any(|ty| ty_mentions_effect_provider_param(db, *ty))
        || trait_inst
            .assoc_type_bindings(db)
            .iter()
            .any(|(_, ty)| ty_mentions_effect_provider_param(db, *ty))
}

fn ty_mentions_effect_provider_param<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if matches!(ty.data(db), TyData::TyApp(..)) {
        return ty_mentions_effect_provider_param(db, ty.base_ty(db))
            || ty
                .generic_args(db)
                .iter()
                .any(|arg| ty_mentions_effect_provider_param(db, *arg));
    }

    match ty.data(db) {
        TyData::TyParam(param) => param.is_effect_provider(),
        TyData::AssocTy(assoc) => trait_inst_mentions_effect_provider_param(db, assoc.trait_),
        TyData::QualifiedTy(trait_inst) => {
            trait_inst_mentions_effect_provider_param(db, *trait_inst)
        }
        TyData::TyVar(_)
        | TyData::TyBase(_)
        | TyData::ConstTy(_)
        | TyData::Never
        | TyData::Invalid(_) => false,
        TyData::TyApp(..) => unreachable!("TyApp handled before data match"),
    }
}

fn effect_param_site_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> String {
    match site {
        EffectParamSite::Func(func) => format!("func${}", item_identity(db, func.into())),
        EffectParamSite::Contract(contract) => {
            format!("contract${}", item_identity(db, contract.into()))
        }
        EffectParamSite::ContractInit { contract } => {
            format!("contract_init${}", item_identity(db, contract.into()))
        }
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "contract_recv${}${recv_idx}${arm_idx}",
            item_identity(db, contract.into())
        ),
    }
}

fn expr_id(expr: ExprId) -> u32 {
    expr.as_u32()
}

fn pat_id(pat: PatId) -> u32 {
    pat.as_u32()
}

pub fn type_identity<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> String {
    if matches!(ty.data(db), TyData::TyApp(..)) {
        let base = type_identity(db, ty.base_ty(db));
        let args = ty
            .generic_args(db)
            .iter()
            .map(|arg| type_identity(db, *arg))
            .collect::<Vec<_>>()
            .join("$");
        return format!("app${base}${args}");
    }

    match ty.data(db) {
        TyData::TyBase(base) => match base {
            TyBase::Prim(prim) => format!("prim${prim:?}"),
            TyBase::Adt(adt) => format!("adt${}", item_identity(db, adt.adt_ref(db).as_item())),
            TyBase::Contract(contract) => {
                format!("contract${}", item_identity(db, (*contract).into()))
            }
            TyBase::Func(callable) => match callable {
                CallableDef::Func(func) => format!("func${}", item_identity(db, (*func).into())),
                CallableDef::VariantCtor(variant) => format!(
                    "variant_ctor${}${}",
                    item_identity(db, variant.enum_.into()),
                    variant.name(db).unwrap_or("variant")
                ),
            },
        },
        TyData::TyParam(param) => {
            format!(
                "param${}${}",
                item_identity(db, param.owner.item()),
                param.idx
            )
        }
        TyData::AssocTy(assoc) => format!(
            "assoc${}${}",
            trait_identity(db, assoc.trait_),
            assoc.name.data(db)
        ),
        TyData::QualifiedTy(trait_inst) => format!("qualified${}", trait_identity(db, *trait_inst)),
        TyData::ConstTy(const_ty) => {
            format!(
                "const_ty${}",
                stable_identity_fingerprint(&const_ty.pretty_print_concrete(db))
            )
        }
        TyData::Never => "never".to_string(),
        TyData::TyVar(_) => format!("var${}", stable_identity_fingerprint(ty.pretty_print(db))),
        TyData::Invalid(cause) => {
            format!(
                "invalid${}",
                stable_identity_fingerprint(&cause.pretty_print(db))
            )
        }
        TyData::TyApp(..) => unreachable!("TyApp handled before data match"),
    }
}

fn trait_identity<'db>(db: &'db dyn HirAnalysisDb, trait_inst: TraitInstId<'db>) -> String {
    let args = trait_inst
        .args(db)
        .iter()
        .map(|arg| type_identity(db, *arg))
        .collect::<Vec<_>>()
        .join("$");
    let assoc_types = trait_inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, ty)| format!("{}${}", name.data(db), type_identity(db, *ty)))
        .collect::<Vec<_>>()
        .join("$");
    format!(
        "{}$args${args}$assoc${assoc_types}",
        item_identity(db, trait_inst.def(db).into())
    )
}

pub fn item_identity<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> String {
    let top_mod = item.top_mod(db);
    format!(
        "{}${}${}${}",
        ingot_identity_for_top_mod(db, top_mod),
        module_path_components_for_scope(db, item.scope()).join("$"),
        item.kind_name(),
        item_name_component(db, item)
    )
}

fn item_name_component<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> String {
    item.name(db)
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| item.kind_name().replace(' ', "_"))
}

pub fn module_path_components_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Vec<String> {
    let mut modules = Vec::new();
    let mut current = scope.parent_module(db);
    while let Some(module_scope) = current {
        match module_scope.item() {
            ItemKind::TopMod(top_mod) => modules.push(top_mod.name(db).data(db).to_string()),
            ItemKind::Mod(module) => modules.push(
                module
                    .name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| "mod".to_string()),
            ),
            _ => {}
        }
        current = module_scope.parent_module(db);
    }
    if modules.is_empty() {
        modules.push(scope.top_mod(db).name(db).data(db).to_string());
    }
    modules.reverse();
    modules
}

pub fn ingot_component_for_scope<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> String {
    let top_mod = scope.top_mod(db);
    ingot_logical_name(db, top_mod.ingot(db), top_mod)
}

fn ingot_identity_for_top_mod<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> String {
    let ingot = top_mod.ingot(db);
    format!(
        "{:?}${}",
        ingot.kind(db),
        ingot_logical_name(db, ingot, top_mod)
    )
}

fn ingot_logical_name<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    top_mod: TopLevelMod<'db>,
) -> String {
    ingot
        .config(db)
        .and_then(|config| config.metadata.name.map(|name| name.to_string()))
        .unwrap_or_else(|| match ingot.kind(db) {
            IngotKind::Core => "core".to_string(),
            IngotKind::Std => "std".to_string(),
            IngotKind::StandAlone => format!("standalone${}", top_mod.name(db).data(db)),
            IngotKind::Local => format!("local${}", top_mod.name(db).data(db)),
            IngotKind::External => format!("external${}", top_mod.name(db).data(db)),
        })
}

pub fn stable_identity_fingerprint(value: &str) -> String {
    let mut fingerprint = 0xcbf29ce484222325u64;
    for byte in value.bytes() {
        fingerprint ^= u64::from(byte);
        fingerprint = fingerprint.wrapping_mul(0x100000001b3);
    }
    format!("{fingerprint:016x}")
}
