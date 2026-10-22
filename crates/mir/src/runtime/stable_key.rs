use common::ingot::{Ingot, IngotKind};
use hir::analysis::ty::evidence::{Evidence, EvidenceId, EvidenceList};
use hir::{
    analysis::{
        HirAnalysisDb,
        semantic::{EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance},
        ty::{
            RootProviderScope,
            trait_def::{TraitInstId, TraitRefId},
            ty_check::{BodyOwner, EffectParamSite},
            ty_def::{BorrowKind, ClosureDef, ClosureTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        Body, CallableDef, ItemKind, TopLevelMod, params::FuncParamMode, scope_graph::ScopeId,
    },
    semantic::{ProviderBinding, ProviderSource},
};

/// Selects how much detail an identity rendering includes.
///
/// `Sort` is the complete stable identity. `Symbol` deliberately omits detail
/// already represented by generic suffixes or disambiguation hashes and
/// filters incidental internal effect-provider constraints.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IdentityMode {
    Sort,
    Symbol,
}

pub fn semantic_instance_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    semantic_instance_identity_in_mode(db, semantic, IdentityMode::Sort)
}

pub fn semantic_instance_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    semantic_instance_identity_in_mode(db, semantic, IdentityMode::Symbol)
}

pub fn semantic_instance_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
    mode: IdentityMode,
) -> String {
    let key = semantic.key(db);
    format!(
        "{}$subst${}$effects${}$impl${}",
        body_owner_identity(db, key.owner(db)),
        generic_subst_identity(db, key.subst(db)),
        effect_provider_subst_identity(db, key.effect_providers(db), mode),
        impl_env_identity(db, key.impl_env(db), key.evidence(db), mode)
    )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OwnerContextRenderMode {
    Fingerprinted,
    Textual,
}

/// A `mut?` function's bodies, one per mode, are distinct.
fn mode_identity(mode: Option<BorrowKind>) -> &'static str {
    match mode {
        None => "",
        Some(BorrowKind::Ref) => "$ref",
        Some(BorrowKind::Mut) => "$mut",
    }
}

fn body_owner_identity<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> String {
    body_owner_identity_in_mode(db, owner, OwnerContextRenderMode::Fingerprinted)
}

fn body_owner_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    render_mode: OwnerContextRenderMode,
) -> String {
    match owner {
        BodyOwner::Func(func, mode) => {
            let owner_context =
                owner_context_identity_in_mode(db, owner, render_mode).unwrap_or_default();
            let mode = match render_mode {
                OwnerContextRenderMode::Textual => "",
                OwnerContextRenderMode::Fingerprinted => mode_identity(mode),
            };
            format!(
                "func${}${owner_context}{mode}",
                item_identity(db, func.into())
            )
        }
        BodyOwner::Const(const_) => format!("const${}", item_identity(db, const_.into())),
        BodyOwner::AnonConstBody { body, expected } => {
            anon_const_body_identity_in_mode(db, body, expected, render_mode)
        }
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
        BodyOwner::Closure { ty, receiver, .. } => format!(
            "closure_body${}${receiver:?}",
            type_identity(db, TyId::closure(db, ty))
        ),
    }
}

fn anon_const_body_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
    mode: OwnerContextRenderMode,
) -> String {
    let owner = BodyOwner::AnonConstBody { body, expected };
    let mut identity = format!(
        "anon_const${}${}",
        module_path_components_for_scope(db, body.scope()).join("$"),
        type_identity(db, expected)
    );
    if let Some(owner_context) = owner_context_identity_in_mode(db, owner, mode) {
        identity.push_str(&format!("${owner_context}"));
    } else if let Some(lexical) = lexical_scope_identity_in_mode(db, body.scope(), mode) {
        identity.push_str(&format!("$lexical${lexical}"));
    }
    if let Some(name) = assoc_const_name(db, body) {
        identity.push_str(&format!("$const${name}"));
    }
    if let Some(ordinal) = child_body_ordinal(db, body.scope()) {
        identity.push_str(&format!("$body{ordinal}"));
    }
    identity
}

fn assoc_const_name<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<&'db str> {
    let name = match body.scope().parent(db)? {
        ScopeId::Item(ItemKind::Trait(trait_)) => trait_
            .assoc_consts(db)
            .find_map(|const_| (const_.default_body(db) == Some(body)).then(|| const_.name(db))?),
        ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => impl_trait
            .assoc_consts(db)
            .find_map(|const_| (const_.value_body(db) == Some(body)).then(|| const_.name(db))?),
        ScopeId::Item(ItemKind::Impl(impl_)) => impl_
            .assoc_consts(db)
            .find_map(|const_| (const_.value_body(db) == Some(body)).then(|| const_.name(db))?),
        _ => None,
    }?;
    Some(name.data(db))
}

pub fn semantic_owner_context_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<String> {
    owner_context_identity_in_mode(db, owner, OwnerContextRenderMode::Fingerprinted)
}

/// Trace-facing variant of [`semantic_owner_context_identity`]: spells the
/// impl context out textually instead of compressing it to a 64-bit
/// fingerprint. HIR body identity on the trace wire must not rest on a hash
/// whose collision would merge two different bodies' facts.
pub fn semantic_owner_context_trace_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<String> {
    owner_context_identity_in_mode(db, owner, OwnerContextRenderMode::Textual)
}

fn impl_trait_header_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: hir::hir_def::ImplTrait<'db>,
) -> String {
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
    let ty_identity = type_identity(db, impl_trait.ty(db));
    match impl_trait.selection_name(db) {
        Some(selection_name) => format!(
            "{trait_identity}${ty_identity}$as${}${}",
            selection_name.data(db),
            item_identity(db, ItemKind::ImplTrait(impl_trait))
        ),
        None => format!("{trait_identity}${ty_identity}"),
    }
}

fn owner_context_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    mode: OwnerContextRenderMode,
) -> Option<String> {
    let scope = match owner {
        BodyOwner::Func(func, _) => func.scope(),
        BodyOwner::AnonConstBody { body, .. } => body.scope(),
        _ => return None,
    };
    match scope.parent(db)? {
        ScopeId::Item(ItemKind::Trait(trait_)) => {
            let name = item_name_component(db, trait_.into());
            let trait_identity = match lexical_scope_identity_in_mode(db, trait_.scope(), mode) {
                Some(lexical) => format!("{name}$lexical${lexical}"),
                None => name,
            };
            Some(format!("trait${trait_identity}"))
        }
        ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => {
            let header = impl_trait_header_identity(db, impl_trait);
            let impl_identity = match lexical_scope_identity_in_mode(db, impl_trait.scope(), mode) {
                Some(lexical) => format!("{header}$lexical${lexical}"),
                None => header,
            };
            Some(match mode {
                OwnerContextRenderMode::Fingerprinted => {
                    format!("impl_trait${}", stable_identity_fingerprint(&impl_identity))
                }
                OwnerContextRenderMode::Textual => format!("impl_trait${impl_identity}"),
            })
        }
        ScopeId::Item(ItemKind::Impl(impl_)) => {
            let ty_identity = type_identity(db, impl_.ty(db));
            let impl_identity = match lexical_scope_identity_in_mode(db, impl_.scope(), mode) {
                Some(lexical) => format!("{ty_identity}$lexical${lexical}"),
                None => ty_identity,
            };
            Some(match mode {
                OwnerContextRenderMode::Fingerprinted => {
                    format!("impl${}", stable_identity_fingerprint(&impl_identity))
                }
                OwnerContextRenderMode::Textual => format!("impl${impl_identity}"),
            })
        }
        _ => None,
    }
}

/// Generics-free identity of the HIR body itself: no substitution, effect, or
/// impl-environment components, and textual (fingerprint-free) impl context.
/// Every instantiation of the same body maps to the same identity.
pub fn body_owner_trace_identity<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> String {
    body_owner_identity_in_mode(db, owner, OwnerContextRenderMode::Textual)
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

fn effect_provider_subst_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    subst: EffectProviderSubst<'db>,
    mode: IdentityMode,
) -> String {
    subst
        .providers(db)
        .iter()
        .map(|provider| provider_binding_identity(db, provider, mode))
        .collect::<Vec<_>>()
        .join("$")
}

fn provider_binding_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &ProviderBinding<'db>,
    mode: IdentityMode,
) -> String {
    let ty = match mode {
        IdentityMode::Sort => format!("$ty${}", type_identity(db, binding.provider_ty)),
        IdentityMode::Symbol => String::new(),
    };
    format!(
        "idx${}{ty}$mut${}$source${}$semantics${}",
        binding.provider_idx,
        binding.is_mut,
        provider_source_identity(db, &binding.source, mode),
        provider_semantics_identity(db, &binding.semantics, mode)
    )
}

fn provider_source_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &ProviderSource<'db>,
    mode: IdentityMode,
) -> String {
    match source {
        ProviderSource::UsesParam {
            site,
            requirement_idx,
        } => format!(
            "uses_param${}${requirement_idx}",
            effect_param_site_identity(db, *site)
        ),
        ProviderSource::ContractField { field } => format!(
            "contract_field${}${}",
            item_identity(db, field.contract.into()),
            field.index
        ),
        ProviderSource::RootProvider {
            scope,
            registration,
        } => {
            let ty = match mode {
                IdentityMode::Sort => {
                    format!("$ty${}", type_identity(db, registration.provider_ty))
                }
                IdentityMode::Symbol => String::new(),
            };
            format!(
                "root_provider${}$idx${}$kind${:?}{ty}",
                root_provider_scope_identity(db, *scope),
                registration.idx,
                registration.site_kind
            )
        }
    }
}

fn provider_semantics_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantics: &hir::analysis::ty::ProviderSemantics<'db>,
    mode: IdentityMode,
) -> String {
    let target = semantics
        .target_ty
        .map(|ty| type_identity(db, ty))
        .unwrap_or_default();
    let ty = match mode {
        IdentityMode::Sort => format!("ty${}$", type_identity(db, semantics.provider_ty)),
        IdentityMode::Symbol => String::new(),
    };
    format!(
        "{ty}kind${:?}$space${:?}$target${target}$transport${:?}",
        semantics.kind, semantics.address_space, semantics.transport
    )
}

fn impl_env_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    env: ImplEnv<'db>,
    evidence: EvidenceList<'db>,
    mode: IdentityMode,
) -> String {
    let assumptions = trait_list_identity(db, env.assumptions(db).list(db).iter().copied(), mode);
    let witnesses = trait_list_identity(db, env.witnesses(db).iter().copied(), mode);
    // Callees normalize at an ingot root, and every ingot's root module has
    // the same name, so the scope must name its ingot.
    let scope = env.normalization_scope(db);
    let identity = format!(
        "scope${}${}$assumptions${assumptions}$witnesses${witnesses}",
        ingot_identity_for_top_mod(db, scope.top_mod(db)),
        module_path_components_for_scope(db, scope).join("$")
    );
    if !evidence
        .entries(db)
        .iter()
        .flatten()
        .any(|ev| ev.has_named_impl(db))
    {
        return identity;
    }
    // Instances that differ only in non-ordinary evidence or evidence parameters
    // run different code.
    let mut parts = Vec::new();
    for (idx, entry) in evidence.entries(db).iter().enumerate() {
        if let Some(ev) = entry
            && ev.has_named_impl(db)
        {
            parts.push(format!("bound{idx}={}", evidence_id_identity(db, *ev)));
        }
    }
    format!("{identity}$evidence${}", parts.join("$"))
}

fn evidence_id_identity<'db>(db: &'db dyn HirAnalysisDb, ev: EvidenceId<'db>) -> String {
    if !ev.has_named_impl(db) {
        return "ordinary".to_string();
    }
    use hir::analysis::ty::trait_def::ImplementorOrigin;
    match ev.data(db) {
        Evidence::Assumption(idx) => format!("assumption{idx}"),
        Evidence::DeferredSolCodec { .. } => "ordinary".to_string(),
        Evidence::Impl {
            selected,
            instantiated,
            conds,
        } => {
            let origin = match selected.origin(db) {
                ImplementorOrigin::Hir(impl_trait) => {
                    let name = impl_trait
                        .selection_name(db)
                        .map_or_else(|| "ordinary".to_string(), |name| name.data(db).to_string());
                    format!(
                        "impl${}${name}",
                        named_impl_evidence_identity(db, impl_trait)
                    )
                }
                ImplementorOrigin::Assumption => "assumption".to_string(),
                ImplementorOrigin::VirtualContract(_) => "virtual".to_string(),
                ImplementorOrigin::Callable => "callable".to_string(),
                ImplementorOrigin::Structural => "structural".to_string(),
            };
            // An implementation can capture or explicitly accept arguments
            // absent from its trait header. Ordinary condition proofs are
            // intentionally shared below, so they cannot recover these args.
            let instance = ev
                .answering(db, instantiated.trait_inst(db))
                .expect("implementation evidence has an instantiated implementation");
            let params = instance
                .impl_args(db)
                .iter()
                .map(|arg| type_identity(db, *arg))
                .collect::<Vec<_>>()
                .join("$");
            let base = format!(
                "{origin}${}$params$[{params}]",
                trait_identity(db, instantiated.trait_inst(db))
            );
            if conds.is_empty() {
                base
            } else {
                let conds = conds
                    .iter()
                    .map(|c| evidence_id_identity(db, *c))
                    .collect::<Vec<_>>()
                    .join("$");
                format!("{base}$conds$[{conds}]")
            }
        }
    }
}

/// Local choices with equal names still denote different declarations. Keep
/// module-level identities unchanged, and name lexical parents without Salsa IDs.
fn named_impl_evidence_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: hir::hir_def::ImplTrait<'db>,
) -> String {
    named_impl_identity_in_mode(db, impl_trait, OwnerContextRenderMode::Fingerprinted)
}

fn named_impl_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: hir::hir_def::ImplTrait<'db>,
    mode: OwnerContextRenderMode,
) -> String {
    let identity = item_identity(db, ItemKind::ImplTrait(impl_trait));
    if impl_trait.selection_name(db).is_none() {
        return identity;
    }
    match lexical_scope_identity_in_mode(db, impl_trait.scope(), mode) {
        Some(lexical) => format!("{identity}$lexical${lexical}"),
        None => identity,
    }
}

fn lexical_scope_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    mode: OwnerContextRenderMode,
) -> Option<String> {
    let mut parents = Vec::new();
    let mut current = scope.parent(db);
    while let Some(scope) = current {
        match scope {
            ScopeId::Item(ItemKind::TopMod(_)) => break,
            ScopeId::Item(ItemKind::Mod(_)) => {}
            ScopeId::Item(ItemKind::Body(body)) => {
                let ordinal =
                    child_body_ordinal(db, scope).expect("body is a child of its lexical parent");
                match assoc_const_name(db, body) {
                    Some(name) => parents.push(format!("const${name}$body{ordinal}")),
                    None => parents.push(format!("body{ordinal}")),
                }
            }
            ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => {
                let header = impl_trait_header_identity(db, impl_trait);
                parents.push(match mode {
                    OwnerContextRenderMode::Fingerprinted => {
                        format!("impl_trait${}", stable_identity_fingerprint(&header))
                    }
                    OwnerContextRenderMode::Textual => format!("impl_trait${header}"),
                });
            }
            ScopeId::Item(ItemKind::Impl(impl_)) => {
                let ty_identity = type_identity(db, impl_.ty(db));
                parents.push(match mode {
                    OwnerContextRenderMode::Fingerprinted => {
                        format!("impl${}", stable_identity_fingerprint(&ty_identity))
                    }
                    OwnerContextRenderMode::Textual => format!("impl${ty_identity}"),
                });
            }
            // Enclosing impl/trait headers are recorded by the same parent
            // walk; rendering them recursively here would duplicate the path.
            ScopeId::Item(ItemKind::Func(func)) => {
                parents.push(item_identity(db, func.into()));
            }
            ScopeId::Item(item) => parents.push(format!(
                "{}${}",
                item.kind_name(),
                item_name_component(db, item)
            )),
            ScopeId::Block(body, expression) => {
                parents.push(format!("block{}", body.iter_block(db)[&expression]));
            }
            _ => {}
        }
        current = scope.parent(db);
    }
    if parents.is_empty() {
        None
    } else {
        parents.reverse();
        Some(parents.join("$"))
    }
}

fn child_body_ordinal<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<usize> {
    let parent = scope.parent(db)?;
    scope
        .scope_graph(db)
        .children(parent)
        .filter(|child| matches!(child, ScopeId::Item(ItemKind::Body(_))))
        .position(|child| child == scope)
}

fn trait_list_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    insts: impl Iterator<Item = TraitInstId<'db>>,
    mode: IdentityMode,
) -> String {
    match mode {
        IdentityMode::Sort => insts
            .map(|inst| trait_identity(db, inst))
            .collect::<Vec<_>>()
            .join("$"),
        IdentityMode::Symbol => {
            let mut identities = insts
                .filter(|inst| !trait_inst_is_internal_effect_provider_constraint(db, *inst))
                .map(|inst| trait_identity(db, inst))
                .collect::<Vec<_>>();
            identities.sort();
            identities.join("$")
        }
    }
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
        && name.data(db) == "EffectRef"
        && module_path_components_for_scope(db, trait_def.scope())
            .iter()
            .any(|component| component == "effect_ref")
}

fn trait_inst_mentions_effect_provider_param<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    trait_ref_mentions_effect_provider_param(db, trait_inst.trait_ref(db))
        || trait_inst
            .assoc_type_bindings(db)
            .iter()
            .any(|(_, ty)| ty_mentions_effect_provider_param(db, *ty))
}

fn trait_ref_mentions_effect_provider_param<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_ref: TraitRefId<'db>,
) -> bool {
    trait_ref
        .args(db)
        .iter()
        .any(|ty| ty_mentions_effect_provider_param(db, *ty))
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
        TyData::AssocTy(assoc) => trait_ref_mentions_effect_provider_param(db, assoc.trait_),
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

fn root_provider_scope_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: RootProviderScope<'db>,
) -> String {
    match scope {
        RootProviderScope::Func(func) => format!("func${}", item_identity(db, func.into())),
        RootProviderScope::Contract(contract) => {
            format!("contract${}", item_identity(db, contract.into()))
        }
    }
}

fn effect_param_site_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> String {
    match site {
        EffectParamSite::Func(func, mode) => format!(
            "func${}{}",
            item_identity(db, func.into()),
            mode_identity(mode)
        ),
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
        EffectParamSite::Closure(def) => format!(
            "closure${}${}",
            closure_owner_identity(db, def),
            def.expr.as_u32()
        ),
    }
}

/// The owner of the body a closure is in: closures are nameless.
fn closure_owner_identity<'db>(db: &'db dyn HirAnalysisDb, def: ClosureDef<'db>) -> String {
    BodyOwner::closure_parent(db, def).map_or_else(
        || module_path_components_for_scope(db, def.body.scope()).join("$"),
        |owner| body_owner_identity(db, owner),
    )
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
            TyBase::Shape(con) => format!("shape${con:?}"),
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
                CallableDef::StructCtor(struct_) => {
                    format!("struct_ctor${}", item_identity(db, (*struct_).into()))
                }
            },
            TyBase::Closure(closure) => {
                // Closures are nameless: the owner of the body defining one
                // and its position there identify it.
                let def = closure.def(db);
                let owner = closure_owner_identity(db, def);
                let leaves: Vec<_> = closure
                    .params(db)
                    .iter()
                    .flat_map(|param| param.leaves(db))
                    .collect();
                let tys = closure
                    .parent_args(db)
                    .iter()
                    .chain(closure.captures(db))
                    .copied()
                    .chain(leaves.iter().map(|&(_, ty)| ty))
                    .chain([closure.ret_ty(db)])
                    .map(|ty| type_identity(db, ty))
                    .collect::<Vec<_>>()
                    .join("$");
                let shapes: Vec<_> = closure
                    .params(db)
                    .iter()
                    .map(|param| param.pretty_print_product(db))
                    .collect();
                format!("closure${owner}${}${shapes:?}${tys}", def.expr.as_u32(),)
            }
        },
        TyData::TyParam(param) => {
            format!(
                "param${}${}",
                item_identity(db, param.owner.item()),
                param.idx
            )
        }
        TyData::AssocTy(assoc) => format!(
            "assoc${}${}${:?}",
            trait_ref_identity(db, assoc.trait_),
            assoc.name.data(db),
            assoc.mode
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
    let trait_ref = trait_ref_identity(db, trait_inst.trait_ref(db));
    let assoc_types = trait_inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(binding, ty)| format!("{}${}", binding.pretty_print(db), type_identity(db, *ty)))
        .collect::<Vec<_>>()
        .join("$");
    format!("{trait_ref}$assoc${assoc_types}")
}

fn trait_ref_identity<'db>(db: &'db dyn HirAnalysisDb, trait_ref: TraitRefId<'db>) -> String {
    let args = trait_ref
        .args(db)
        .iter()
        .map(|arg| type_identity(db, *arg))
        .collect::<Vec<_>>()
        .join("$");
    format!(
        "{}$args${args}",
        item_identity(db, trait_ref.def(db).into())
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
        .clone()
        .and_then(|config| config.metadata.name.map(|name| name.to_string()))
        .unwrap_or_else(|| match ingot.kind(db) {
            IngotKind::Core => "core".to_string(),
            IngotKind::Std => "std".to_string(),
            IngotKind::StandAlone => format!("standalone${}", top_mod.name(db).data(db)),
            IngotKind::Local => format!("local${}", top_mod.name(db).data(db)),
            IngotKind::External => format!("external${}", top_mod.name(db).data(db)),
        })
}

/// The symbol name of a closure's body called with `receiver`.
pub fn closure_symbol_component<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
    receiver: FuncParamMode,
) -> String {
    let kind = match receiver {
        FuncParamMode::Mut => "closure_mut",
        FuncParamMode::View | FuncParamMode::Own => "closure",
    };
    format!(
        "__{kind}_{}",
        stable_identity_fingerprint(&type_identity(db, TyId::closure(db, closure)))
    )
}

pub fn stable_identity_fingerprint(value: &str) -> String {
    let mut fingerprint = 0xcbf29ce484222325u64;
    for byte in value.bytes() {
        fingerprint ^= u64::from(byte);
        fingerprint = fingerprint.wrapping_mul(0x100000001b3);
    }
    format!("{fingerprint:016x}")
}

#[cfg(test)]
mod tests {
    use super::{
        body_owner_trace_identity, semantic_instance_identity, semantic_instance_symbol_identity,
    };
    use hir::{
        analysis::{
            semantic::{SemanticInstance, root_semantic_instance_key},
            ty::ty_check::BodyOwner,
        },
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };

    fn assert_trace_has_no_fingerprint(trace: &str) {
        for segment in trace.split('$') {
            let is_hex_fingerprint =
                segment.len() == 16 && segment.bytes().all(|b| b.is_ascii_hexdigit());
            assert!(
                !is_hex_fingerprint,
                "trace identity must not contain a 64-bit hex fingerprint segment: {trace}"
            );
        }
    }

    fn assert_pairwise_distinct_instances(
        db: &HirAnalysisTestDb,
        instances: &[SemanticInstance<'_>],
    ) {
        for (index, instance) in instances.iter().enumerate() {
            let trace = body_owner_trace_identity(db, instance.key(db).owner(db));
            assert_trace_has_no_fingerprint(&trace);
            for other in &instances[index + 1..] {
                assert_ne!(
                    instance.key(db).owner(db),
                    other.key(db).owner(db),
                    "expected distinct BodyOwners"
                );
                assert_ne!(
                    semantic_instance_identity(db, *instance),
                    semantic_instance_identity(db, *other),
                    "expected distinct Sort identities"
                );
                assert_ne!(
                    semantic_instance_symbol_identity(db, *instance),
                    semantic_instance_symbol_identity(db, *other),
                    "expected distinct Symbol identities"
                );
                assert_ne!(
                    trace,
                    body_owner_trace_identity(db, other.key(db).owner(db)),
                    "expected distinct trace identities"
                );
            }
        }
    }

    #[test]
    fn stable_key_distinguishes_named_evidence_and_shares_ordinary_instances() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "stable_key_evidence_identity.fe".into(),
            r#"
trait Inner {
    const fn inner() -> u256
}
impl Inner for u256 {
    const fn inner() -> u256 { 10 }
}
impl Inner for u256 as AltInner20 {
    const fn inner() -> u256 { 20 }
}
impl Inner for u256 as AltInner30 {
    const fn inner() -> u256 { 30 }
}

struct Boxed<T> {
    pub x: T,
}
trait Outer {
    const fn outer(self) -> u256
}
impl<T: Inner> Outer for Boxed<T> {
    const fn outer(self) -> u256 { T::inner() }
}

const fn helper<T: Inner>(_ m: Boxed<T>) -> u256 {
    m.outer()
}

const fn call_before() -> u256 {
    helper(Boxed { x: 1 as u256 })
}
const fn call_alt20() -> u256 {
    with (AltInner20) {
        helper(Boxed { x: 1 as u256 })
    }
}
const fn call_alt30() -> u256 {
    with (AltInner30) {
        helper(Boxed { x: 1 as u256 })
    }
}
const fn call_after() -> u256 {
    helper(Boxed { x: 1 as u256 })
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let func = |name: &str| {
            top_mod
                .children_non_nested(&db)
                .find_map(|item| match item {
                    ItemKind::Func(f)
                        if f.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                    {
                        Some(f)
                    }
                    _ => None,
                })
                .unwrap()
        };
        let helper_fn = func("helper");
        let helper_key_in = |caller_name: &str| {
            let caller = func(caller_name);
            let inst = SemanticInstance::new(
                &db,
                root_semantic_instance_key(&db, BodyOwner::Func(caller, None)).unwrap(),
            );
            let key = inst
                .callees(&db)
                .iter()
                .find(|c| c.key.owner(&db) == BodyOwner::Func(helper_fn, None))
                .unwrap()
                .key;
            SemanticInstance::new(&db, key)
        };

        let helper_before = helper_key_in("call_before");
        let helper_alt20 = helper_key_in("call_alt20");
        let helper_alt30 = helper_key_in("call_alt30");
        let helper_after = helper_key_in("call_after");

        let id_before = semantic_instance_identity(&db, helper_before);
        let id_alt20 = semantic_instance_identity(&db, helper_alt20);
        let id_alt30 = semantic_instance_identity(&db, helper_alt30);
        let id_after = semantic_instance_identity(&db, helper_after);

        assert_eq!(id_before, id_after);
        assert_ne!(id_before, id_alt20);
        assert_ne!(id_before, id_alt30);
        assert_ne!(id_alt20, id_alt30);

        // Follow helper -> Outer::outer -> selected Inner::inner method body.
        fn selected_inner_in<'db>(
            db: &'db HirAnalysisTestDb,
            helper_inst: SemanticInstance<'db>,
        ) -> SemanticInstance<'db> {
            let outer_callee = helper_inst.callees(db)[0].key;
            let outer_inst = SemanticInstance::new(db, outer_callee);
            let inner_callee = outer_inst.callees(db)[0].key;
            SemanticInstance::new(db, inner_callee)
        }
        let inner_before = selected_inner_in(&db, helper_before);
        let inner_alt20 = selected_inner_in(&db, helper_alt20);
        let inner_alt30 = selected_inner_in(&db, helper_alt30);
        let inner_after = selected_inner_in(&db, helper_after);

        assert_eq!(
            semantic_instance_identity(&db, inner_before),
            semantic_instance_identity(&db, inner_after)
        );
        assert_eq!(
            semantic_instance_symbol_identity(&db, inner_before),
            semantic_instance_symbol_identity(&db, inner_after)
        );
        assert_eq!(
            body_owner_trace_identity(&db, inner_before.key(&db).owner(&db)),
            body_owner_trace_identity(&db, inner_after.key(&db).owner(&db))
        );
        assert_pairwise_distinct_instances(&db, &[inner_before, inner_alt20, inner_alt30]);
    }

    #[test]
    fn stable_keys_preserve_impl_arguments_absent_from_trait_headers() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "applied_choice_identity.fe".into(),
            r#"
struct Item {}
struct Arg {}
struct Other {}
trait Value { fn value() -> u256 }
impl Value for Arg { fn value() -> u256 { 7 } }
impl Value for Other { fn value() -> u256 { 9 } }
trait Pick { fn pick() -> u256 }
impl<T: Value> Pick for Item as Chosen {
    fn pick() -> u256 { T::value() }
}
impl<const N: u256> Pick for Item as Number {
    fn pick() -> u256 { N }
}
fn helper<T: Pick>() -> u256 { T::pick() }
fn explicit() -> u256 {
    let first = with (Chosen<Arg>) { helper<Item>() }
    let second = with (Chosen<Other>) { helper<Item>() }
    first + second
}
fn constants() -> u256 {
    let first = with (Number<7>) { helper<Item>() }
    let second = with (Number<9>) { helper<Item>() }
    first + second
}
fn captured<T: Value>() -> u256 {
    impl Pick for Item as Local where T: Value {
        fn pick() -> u256 { T::value() }
    }
    with (Local) { helper<Item>() }
}
fn captures() -> u256 { captured<Arg>() + captured<Other>() }
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let function = |name: &str| {
            top_mod
                .children_non_nested(&db)
                .find_map(|item| match item {
                    ItemKind::Func(func)
                        if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                    {
                        Some(func)
                    }
                    _ => None,
                })
                .unwrap()
        };
        let root = |name: &str| {
            SemanticInstance::new(
                &db,
                root_semantic_instance_key(&db, BodyOwner::Func(function(name), None)).unwrap(),
            )
        };
        fn helper_instances<'db>(
            db: &'db HirAnalysisTestDb,
            instance: SemanticInstance<'db>,
            helper: BodyOwner<'db>,
        ) -> Vec<SemanticInstance<'db>> {
            instance
                .callees(db)
                .iter()
                .filter(|callee| callee.key.owner(db) == helper)
                .map(|callee| SemanticInstance::new(db, callee.key))
                .collect::<Vec<_>>()
        }
        let helper = BodyOwner::Func(function("helper"), None);
        let helpers = |instance| helper_instances(&db, instance, helper);
        let captures = root("captures")
            .callees(&db)
            .iter()
            .flat_map(|callee| helpers(SemanticInstance::new(&db, callee.key)))
            .collect::<Vec<_>>();
        for (kind, instances) in [
            ("explicit type arguments", helpers(root("explicit"))),
            ("explicit constant arguments", helpers(root("constants"))),
            ("captured type arguments", captures),
        ] {
            assert_eq!(instances.len(), 2, "{kind}");
            let [first, second] = [instances[0], instances[1]];
            assert_eq!(first.key(&db).owner(&db), second.key(&db).owner(&db));
            assert_ne!(first.key(&db), second.key(&db), "distinct proofs: {kind}");
            assert_ne!(
                semantic_instance_identity(&db, first),
                semantic_instance_identity(&db, second),
                "sort identity must retain {kind}"
            );
            assert_ne!(
                semantic_instance_symbol_identity(&db, first),
                semantic_instance_symbol_identity(&db, second),
                "symbol identity must retain {kind}"
            );
        }
    }

    #[test]
    fn stable_keys_distinguish_same_named_choices_in_lexical_scopes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "local_choice_identity.fe".into(),
            r#"
trait Pick { fn pick(self) -> u256 }
impl Pick for u256 { fn pick(self) -> u256 { 1 } }
fn helper<T: Pick>(_ x: T) -> u256 { x.pick() }
fn first() -> u256 {
    impl Pick for u256 as Local { fn pick(self) -> u256 { 20 } }
    with (Local) { helper(0 as u256) }
}
fn second() -> u256 {
    impl Pick for u256 as Local { fn pick(self) -> u256 { 30 } }
    with (Local) { helper(0 as u256) }
}
fn sibling_blocks() -> u256 {
    let a = {
        impl Pick for u256 as Local { fn pick(self) -> u256 { 40 } }
        with (Local) { helper(0 as u256) }
    }
    let b = {
        impl Pick for u256 as Local { fn pick(self) -> u256 { 50 } }
        with (Local) { helper(0 as u256) }
    }
    a + b
}
fn ordinary() -> u256 { helper(0 as u256) + helper(1 as u256) }
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let function = |name: &str| {
            top_mod
                .children_non_nested(&db)
                .find_map(|item| match item {
                    ItemKind::Func(func)
                        if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                    {
                        Some(func)
                    }
                    _ => None,
                })
                .unwrap()
        };
        let helper = function("helper");
        let instances = |name: &str| {
            let root = SemanticInstance::new(
                &db,
                root_semantic_instance_key(&db, BodyOwner::Func(function(name), None)).unwrap(),
            );
            root.callees(&db)
                .iter()
                .filter(|callee| callee.key.owner(&db) == BodyOwner::Func(helper, None))
                .map(|callee| SemanticInstance::new(&db, callee.key))
                .collect::<Vec<_>>()
        };
        let mut choices = instances("first");
        assert_eq!(choices.len(), 1);
        let second = instances("second");
        assert_eq!(second.len(), 1);
        choices.extend(second);
        let siblings = instances("sibling_blocks");
        assert_eq!(siblings.len(), 2);
        choices.extend(siblings);
        for (index, choice) in choices.iter().enumerate() {
            for other in &choices[index + 1..] {
                assert_ne!(
                    semantic_instance_identity(&db, *choice),
                    semantic_instance_identity(&db, *other)
                );
                assert_ne!(
                    semantic_instance_symbol_identity(&db, *choice),
                    semantic_instance_symbol_identity(&db, *other)
                );
            }
        }
        let ordinary = instances("ordinary");
        assert!(!ordinary.is_empty());
        for instance in &ordinary {
            assert_eq!(
                semantic_instance_identity(&db, ordinary[0]),
                semantic_instance_identity(&db, *instance)
            );
            assert_eq!(
                semantic_instance_symbol_identity(&db, ordinary[0]),
                semantic_instance_symbol_identity(&db, *instance)
            );
        }

        // Descend from each helper instance to its selected `Pick::pick` method body.
        fn selected_pick<'db>(
            db: &'db HirAnalysisTestDb,
            helper_inst: SemanticInstance<'db>,
        ) -> SemanticInstance<'db> {
            let callees = helper_inst.callees(db);
            assert_eq!(callees.len(), 1);
            SemanticInstance::new(db, callees[0].key)
        }
        let mut selected_methods = choices
            .iter()
            .copied()
            .map(|instance| selected_pick(&db, instance))
            .collect::<Vec<_>>();
        let ordinary_pick = selected_pick(&db, ordinary[0]);
        for instance in &ordinary {
            let pick = selected_pick(&db, *instance);
            assert_eq!(pick.key(&db).owner(&db), ordinary_pick.key(&db).owner(&db));
            assert_eq!(
                semantic_instance_identity(&db, pick),
                semantic_instance_identity(&db, ordinary_pick)
            );
            assert_eq!(
                semantic_instance_symbol_identity(&db, pick),
                semantic_instance_symbol_identity(&db, ordinary_pick)
            );
            assert_eq!(
                body_owner_trace_identity(&db, pick.key(&db).owner(&db)),
                body_owner_trace_identity(&db, ordinary_pick.key(&db).owner(&db))
            );
        }
        selected_methods.push(ordinary_pick);
        assert_pairwise_distinct_instances(&db, &selected_methods);
    }
    #[test]
    fn nested_method_local_choices_and_allocation_order_independence() {
        let source = r#"
trait Outer {
    fn run(self) -> u256
}
trait Inner {
    fn inner(self) -> u256
}

impl Outer for u256 as Left {
    fn run(self) -> u256 {
        impl Inner for u256 as Local {
            fn inner(self) -> u256 { 20 }
        }
        with (Local) { self.inner() }
    }
}

impl Outer for u256 as Right {
    fn run(self) -> u256 {
        impl Inner for u256 as Local {
            fn inner(self) -> u256 { 30 }
        }
        with (Local) { self.inner() }
    }
}

fn call_left() -> u256 {
    with (Left) { (0 as u256).run() }
}

fn call_right() -> u256 {
    with (Right) { (0 as u256).run() }
}
"#;

        let collect_identities = |with_unrelated_prefix: bool| {
            let mut db = HirAnalysisTestDb::default();
            if with_unrelated_prefix {
                let prefix = db.new_stand_alone(
                    "unrelated_prefix.fe".into(),
                    r#"
trait Noise { fn noise(self) -> u256 }
impl Noise for u256 as Extra { fn noise(self) -> u256 { 99 } }
fn pre_intern() -> u256 {
    impl Noise for u256 as InnerExtra { fn noise(self) -> u256 { 100 } }
    with (InnerExtra) { (1 as u256).noise() }
}
"#,
                );
                let (prefix_mod, _) = db.top_mod(prefix);
                db.assert_no_diags(prefix_mod);
            }
            let file = db.new_stand_alone("nested_owner_identity.fe".into(), source);
            let (top_mod, _) = db.top_mod(file);
            db.assert_no_diags(top_mod);
            let function = |name: &str| {
                top_mod
                    .children_non_nested(&db)
                    .find_map(|item| match item {
                        ItemKind::Func(func)
                            if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                        {
                            Some(func)
                        }
                        _ => None,
                    })
                    .unwrap()
            };
            let chain_for = |caller_name: &str| {
                let root = SemanticInstance::new(
                    &db,
                    root_semantic_instance_key(&db, BodyOwner::Func(function(caller_name), None))
                        .unwrap(),
                );
                let outer_inst = SemanticInstance::new(&db, root.callees(&db)[0].key);
                let inner_inst = SemanticInstance::new(&db, outer_inst.callees(&db)[0].key);
                (outer_inst, inner_inst)
            };
            let (left_outer, left_inner) = chain_for("call_left");
            let (right_outer, right_inner) = chain_for("call_right");
            assert_pairwise_distinct_instances(&db, &[left_outer, right_outer]);
            assert_pairwise_distinct_instances(&db, &[left_inner, right_inner]);
            vec![left_outer, right_outer, left_inner, right_inner]
                .into_iter()
                .map(|inst| {
                    (
                        semantic_instance_identity(&db, inst),
                        semantic_instance_symbol_identity(&db, inst),
                        body_owner_trace_identity(&db, inst.key(&db).owner(&db)),
                    )
                })
                .collect::<Vec<_>>()
        };

        let baseline = collect_identities(false);
        let shifted = collect_identities(true);
        assert_eq!(baseline, shifted);
    }
}
