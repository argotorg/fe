use hir::semantic::{RecvArmAbiInfo, RecvArmView};
use hir::{
    analysis::{
        semantic::{
            GenericSubst, ImplEnv, ManualContractSection, RootSemanticInstanceError,
            SemConstScalar, SemConstValue, SemanticInstance, SemanticInstanceKey,
            eval_body_owner_const, get_or_build_semantic_instance, owner_effect_bindings,
            root_semantic_instance_key,
        },
        ty::{
            const_ty::ConstTyData,
            corelib::{resolve_core_trait, resolve_lib_type_path},
            normalize::normalize_ty,
            trait_def::{
                TraitInstId, assoc_const_body_for_trait_inst, resolve_trait_method_instance,
            },
            trait_resolution::{PredicateListId, TraitSolveCx},
            ty_check::{BodyOwner, LocalBinding},
            ty_def::{TyData, TyId},
        },
    },
    hir_def::{
        Contract, Func, IdentId, InlineHint, IntegerId, ItemKind, ManualContractRootAttr,
        TopLevelMod,
    },
};
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    db::MirDb,
    instance::runtime::runtime_instance_lowered_body,
    instance::{
        RuntimeInstance, RuntimeInstanceKey, RuntimeInstanceSource, RuntimeSyntheticInstance,
        get_or_build_runtime_instance,
    },
    runtime::code_region::{code_region_symbol, runtime_code_region_for_manual_root},
    runtime::lower::classify::{
        runtime_effect_binding_plan, runtime_param_class, runtime_visible_binding_class,
    },
    runtime::lower::interface::runtime_visible_binding_plans,
    runtime::lower::type_info::{RuntimeTypeEnv, top_level_class_for_ty_in_env},
    runtime::root_effects::{EntryEffectContext, entry_effect_arg_plans},
    runtime::stable_key::{
        item_identity, semantic_instance_identity, semantic_instance_symbol_identity,
        stable_identity_hash, type_identity,
    },
    runtime::{
        AddressSpaceKind, ConstRegionId, ContractAbiPlan, ContractInitAbiPlan, ContractRecvAbiPlan,
        DispatchArm, DispatchDefault, EntryEffectArgPlan, InitArgsPlan, LayoutId, LayoutKey,
        RefKind, RefView, ResolvedCodeRegion, RuntimeClass, RuntimeCodeRegion,
        RuntimeCodeRegionKey, RuntimeFunction, RuntimeFunctionOwner, RuntimeInlineHint,
        RuntimeInputPlan, RuntimeLinkage, RuntimeObject, RuntimePackage, RuntimePackagePlan,
        RuntimeReturnPlan, RuntimeSection, RuntimeSectionName, RuntimeSectionRef,
        RuntimeSyntheticSpec, ScalarClass, ScalarRepr, ScalarRole,
        TargetRootProviderMaterialization,
    },
    verify::verify_runtime_package,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum LowerError {
    Unsupported(String),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::Unsupported(message) => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for LowerError {}

#[derive(Clone, Copy)]
struct ManualContractRoot<'db> {
    func: Func<'db>,
    instance: RuntimeInstance<'db>,
    contract_name: &'db str,
    section: ManualContractSection,
}

type ManualContractObjectSpec<'db> = (
    String,
    Vec<(RuntimeSectionName, RuntimeInstance<'db>)>,
    Vec<RuntimeInstance<'db>>,
);

#[derive(Debug)]
enum RuntimeRootCandidate<'db> {
    Root(Func<'db>),
    NotRoot,
    Rejected(RuntimeRootRejection<'db>),
}

#[derive(Debug)]
struct RuntimeRootRejection<'db> {
    func: Func<'db>,
    reason: RuntimeRootRejectionReason<'db>,
}

#[derive(Debug)]
enum RuntimeRootRejectionReason<'db> {
    RootSemanticInstance(RootSemanticInstanceError<'db>),
    UnsupportedEntryEffect(String),
}

#[derive(Debug, Clone)]
struct RuntimeGraphNode<'db> {
    direct_callees: Vec<RuntimeInstance<'db>>,
    referenced_const_regions: Vec<ConstRegionId<'db>>,
    referenced_code_regions: Vec<RuntimeCodeRegion<'db>>,
}

struct RuntimeGraph<'db> {
    nodes: FxHashMap<RuntimeInstance<'db>, RuntimeGraphNode<'db>>,
    object_specs: Vec<(String, Vec<(RuntimeSectionName, RuntimeInstance<'db>)>)>,
    code_region_roots: Vec<(RuntimeCodeRegion<'db>, RuntimeInstance<'db>)>,
}

struct RuntimeGraphBuilder<'db> {
    db: &'db dyn MirDb,
    queue: Vec<RuntimeInstance<'db>>,
    queued: FxHashSet<RuntimeInstance<'db>>,
    nodes: FxHashMap<RuntimeInstance<'db>, RuntimeGraphNode<'db>>,
    object_specs: Vec<(String, Vec<(RuntimeSectionName, RuntimeInstance<'db>)>)>,
    discovered_contract_specs: Vec<(String, Vec<(RuntimeSectionName, RuntimeInstance<'db>)>)>,
    code_region_roots: Vec<(RuntimeCodeRegion<'db>, RuntimeInstance<'db>)>,
    seen_region_roots: FxHashSet<RuntimeCodeRegion<'db>>,
    materialized_contracts: FxHashSet<Contract<'db>>,
    materialized_object_names: FxHashSet<String>,
}

impl<'db> RuntimeGraphBuilder<'db> {
    fn new(
        db: &'db dyn MirDb,
        roots: Vec<RuntimeInstance<'db>>,
        object_specs: Vec<(String, Vec<(RuntimeSectionName, RuntimeInstance<'db>)>)>,
    ) -> Self {
        let materialized_contracts = materialized_contracts_for_roots(db, &roots);
        let materialized_object_names = object_specs
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<FxHashSet<_>>();
        let mut builder = Self {
            db,
            queue: Vec::new(),
            queued: FxHashSet::default(),
            nodes: FxHashMap::default(),
            object_specs,
            discovered_contract_specs: Vec::new(),
            code_region_roots: Vec::new(),
            seen_region_roots: FxHashSet::default(),
            materialized_contracts,
            materialized_object_names,
        };
        for root in roots {
            builder.enqueue(root);
        }
        builder
    }

    fn build(mut self) -> Result<RuntimeGraph<'db>, LowerError> {
        while let Some(instance) = self.queue.pop() {
            self.queued.remove(&instance);
            if self.nodes.contains_key(&instance) {
                continue;
            }

            let lowered = runtime_instance_lowered_body(self.db, instance)
                .map_err(|err| wrap_runtime_lowering_error(self.db, instance, err))?;
            let direct_callees = lowered
                .direct_callees(self.db)
                .into_iter()
                .map(|edge| edge.callee)
                .collect::<Vec<_>>();
            let referenced_const_regions = lowered.referenced_const_regions(self.db);
            let referenced_code_regions = lowered.referenced_code_regions(self.db);
            for callee in direct_callees.iter().copied() {
                self.enqueue(callee);
            }
            self.process_referenced_regions(&referenced_code_regions)?;
            self.nodes.insert(
                instance,
                RuntimeGraphNode {
                    direct_callees,
                    referenced_const_regions,
                    referenced_code_regions,
                },
            );
        }

        self.discovered_contract_specs
            .sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        self.object_specs.extend(self.discovered_contract_specs);
        self.code_region_roots
            .sort_by_key(|(region, _)| code_region_symbol(self.db, *region));
        Ok(RuntimeGraph {
            nodes: self.nodes,
            object_specs: self.object_specs,
            code_region_roots: self.code_region_roots,
        })
    }

    fn enqueue(&mut self, instance: RuntimeInstance<'db>) {
        if !self.nodes.contains_key(&instance) && self.queued.insert(instance) {
            self.queue.push(instance);
        }
    }

    fn process_referenced_regions(
        &mut self,
        regions: &[RuntimeCodeRegion<'db>],
    ) -> Result<(), LowerError> {
        let mut function_roots = Vec::new();
        let mut referenced_contracts = Vec::new();
        let mut referenced_manual_roots = Vec::new();
        for region in regions.iter().copied() {
            match region.key(self.db) {
                RuntimeCodeRegionKey::FunctionRoot { .. } => {
                    if self.seen_region_roots.insert(region) {
                        function_roots.push(region);
                    }
                }
                RuntimeCodeRegionKey::ContractInit { contract }
                | RuntimeCodeRegionKey::ContractRuntime { contract } => {
                    if self.materialized_contracts.insert(contract) {
                        referenced_contracts.push(contract);
                    }
                }
                RuntimeCodeRegionKey::ManualContractRoot { func } => {
                    referenced_manual_roots.push(func);
                }
            }
        }

        function_roots.sort_by_key(|region| code_region_symbol(self.db, *region));
        for region in function_roots {
            let RuntimeCodeRegionKey::FunctionRoot { symbol, callee } = region.key(self.db).clone()
            else {
                unreachable!();
            };
            let root = synthetic_instance(
                self.db,
                RuntimeSyntheticSpec::CodeRegionRoot { symbol, callee },
                Vec::new(),
            );
            self.code_region_roots.push((region, root));
            self.enqueue(root);
        }

        referenced_contracts.sort_by_key(|contract| contract_name(self.db, *contract));
        for contract in referenced_contracts {
            let (name, sections, section_roots) = contract_object_spec(self.db, contract)?;
            if !self.materialized_object_names.insert(name.clone()) {
                continue;
            }
            self.discovered_contract_specs.push((name, sections));
            for root in section_roots {
                self.enqueue(root);
            }
        }
        referenced_manual_roots.sort_by_key(|func| {
            func.name(self.db)
                .to_opt()
                .map(|name| name.data(self.db).to_string())
        });
        for func in referenced_manual_roots {
            let Some((name, sections, section_roots)) =
                manual_contract_object_for_root(self.db, func)?
            else {
                continue;
            };
            if !self.materialized_object_names.insert(name.clone()) {
                continue;
            }
            self.discovered_contract_specs.push((name, sections));
            for root in section_roots {
                self.enqueue(root);
            }
        }
        Ok(())
    }
}

pub fn build_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    if !top_mod.all_contracts(db).is_empty()
        || !discover_manual_contract_roots(db, top_mod)?.is_empty()
    {
        return build_contract_package(db, top_mod);
    }

    let funcs = top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .filter(|func| func.top_mod(db) == top_mod)
        .filter(|func| !func.is_extern(db) && !is_test_func(db, *func))
        .collect::<Vec<_>>();
    let mut funcs = funcs;
    funcs.sort_by_key(|func| {
        func.name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_default()
    });
    let mut entry_funcs = Vec::new();
    let mut rejections = Vec::new();
    for func in funcs.iter().copied() {
        match runtime_root_candidate(db, func)? {
            RuntimeRootCandidate::Root(func) => entry_funcs.push(func),
            RuntimeRootCandidate::NotRoot => {}
            RuntimeRootCandidate::Rejected(rejection) => rejections.push(rejection),
        }
    }
    if let Some(rejection) = rejections
        .iter()
        .find(|rejection| is_main_func(db, rejection.func))
    {
        return Err(LowerError::Unsupported(format_runtime_root_rejection(
            db, rejection,
        )));
    }
    if entry_funcs.is_empty() {
        if let Some(rejection) = rejections.first() {
            return Err(LowerError::Unsupported(format_runtime_root_rejection(
                db, rejection,
            )));
        }
        return Ok(RuntimePackage::new(
            db,
            top_mod,
            Vec::new(),
            RuntimePackagePlan::new(db, Vec::new(), Vec::new(), Vec::new(), Vec::new(), None),
        ));
    }

    let mut roots = Vec::new();
    for func in entry_funcs.iter().copied() {
        let semantic = semantic_instance_for_root_owner(db, BodyOwner::Func(func))?;
        let entry_effect_args =
            entry_effect_arg_plans(db, EntryEffectContext::StandaloneFunc { func }, semantic)?;
        roots.push((
            func,
            runtime_instance_for_semantic(db, semantic),
            entry_effect_args,
        ));
    }
    let entry = roots
        .iter()
        .find(|(func, _, _)| is_main_func(db, *func))
        .or_else(|| roots.first())
        .map(|(_, instance, entry_effect_args)| (*instance, entry_effect_args.clone()))
        .expect("entry root candidates should include the chosen entry function");
    let root = synthetic_instance(
        db,
        RuntimeSyntheticSpec::MainRoot {
            callee: entry.0,
            entry_effect_args: entry.1.into_boxed_slice(),
        },
        Vec::new(),
    );
    let mut package_roots = roots
        .into_iter()
        .map(|(_, instance, _)| instance)
        .collect::<Vec<_>>();
    package_roots.push(root);
    let package = build_non_contract_package(
        db,
        top_mod,
        package_roots,
        vec![(sanitize_object_name("main"), RuntimeSectionName::Main, root)],
        Some("main"),
    )?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

pub fn build_test_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    filter: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let mut roots = Vec::new();
    let mut objects = Vec::new();
    for &func in top_mod.all_funcs(db) {
        if func.top_mod(db) != top_mod {
            continue;
        }
        let Some(attrs) = ItemKind::from(func).attrs(db) else {
            continue;
        };
        if attrs.get_attr(db, "test").is_none() {
            continue;
        }

        let name = func
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "<anonymous>".to_string());
        if let Some(filter) = filter
            && !name.contains(filter)
        {
            continue;
        }

        let semantic = semantic_instance_for_root_owner(db, BodyOwner::Func(func))?;
        let entry_effect_args =
            entry_effect_arg_plans(db, EntryEffectContext::TestFunc { func }, semantic)?;
        let runtime_root = runtime_instance_for_semantic(db, semantic);
        let root = synthetic_instance(
            db,
            RuntimeSyntheticSpec::TestRoot {
                name: name.clone(),
                callee: runtime_root,
                entry_effect_args: entry_effect_args.into_boxed_slice(),
            },
            Vec::new(),
        );
        roots.push(root);
        objects.push((
            sanitize_object_name(&name),
            vec![(RuntimeSectionName::Test(name), root)],
        ));
    }

    let primary = (objects.len() == 1).then(|| objects[0].0.clone());
    let package = build_sectioned_package(db, top_mod, roots, objects, primary.as_deref())?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

fn build_contract_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let mut roots = Vec::new();
    let mut objects = Vec::new();
    let contracts = top_mod.all_contracts(db);
    for &contract in contracts {
        let (name, sections, section_roots) = contract_object_spec(db, contract)?;
        roots.extend(section_roots);
        objects.push((name, sections));
    }
    for (name, sections, section_roots) in manual_contract_objects(db, top_mod)? {
        roots.extend(section_roots);
        objects.push((name, sections));
    }

    let primary = (objects.len() == 1).then(|| objects[0].0.clone());
    let package = build_sectioned_package(db, top_mod, roots, objects, primary.as_deref())?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

fn manual_contract_objects<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<Vec<ManualContractObjectSpec<'db>>, LowerError> {
    let roots = discover_manual_contract_roots(db, top_mod)?;
    let mut by_contract =
        FxHashMap::<String, (Option<RuntimeInstance<'db>>, Option<RuntimeInstance<'db>>)>::default(
        );
    for root in roots {
        let entry = by_contract
            .entry(root.contract_name.to_string())
            .or_insert((None, None));
        match root.section {
            ManualContractSection::Init => {
                if entry.0.replace(root.instance).is_some() {
                    return Err(LowerError::Unsupported(format!(
                        "duplicate #[contract_init({})] root in package",
                        root.contract_name
                    )));
                }
            }
            ManualContractSection::Runtime => {
                if entry.1.replace(root.instance).is_some() {
                    return Err(LowerError::Unsupported(format!(
                        "duplicate #[contract_runtime({})] root in package",
                        root.contract_name
                    )));
                }
            }
        }
    }

    let high_level_names = top_mod
        .all_contracts(db)
        .iter()
        .filter_map(|contract| {
            contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
        })
        .collect::<FxHashSet<_>>();
    for contract_name in by_contract.keys() {
        if high_level_names.contains(contract_name) {
            return Err(LowerError::Unsupported(format!(
                "manual contract roots for `{contract_name}` conflict with a high-level contract of the same name"
            )));
        }
    }

    let mut objects = by_contract
        .into_iter()
        .map(|(contract_name, (init, runtime))| {
            let mut sections = Vec::new();
            let mut roots = Vec::new();
            if let Some(init) = init {
                sections.push((RuntimeSectionName::Init, init));
                roots.push(init);
            }
            if let Some(runtime) = runtime {
                sections.push((RuntimeSectionName::Runtime, runtime));
                roots.push(runtime);
            }
            (sanitize_object_name(&contract_name), sections, roots)
        })
        .collect::<Vec<_>>();
    objects.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
    Ok(objects)
}

fn manual_contract_object_for_root<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Result<Option<ManualContractObjectSpec<'db>>, LowerError> {
    let Some(attr) = func.manual_contract_root_attr(db) else {
        return Ok(None);
    };
    let contract_name = match attr {
        ManualContractRootAttr::Init { contract_name }
        | ManualContractRootAttr::Runtime { contract_name } => contract_name.data(db),
        ManualContractRootAttr::Error(err) => {
            return Err(LowerError::Unsupported(format!(
                "invalid manual contract root attr on `{}`: {err:?}",
                func.name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| "<anonymous>".to_string())
            )));
        }
    };
    let object_name = sanitize_object_name(contract_name);
    Ok(manual_contract_objects(db, func.top_mod(db))?
        .into_iter()
        .find(|(name, _, _)| name == &object_name))
}

fn discover_manual_contract_roots<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<Vec<ManualContractRoot<'db>>, LowerError> {
    let mut roots = Vec::new();
    for &func in top_mod.all_funcs(db) {
        if func.top_mod(db) != top_mod {
            continue;
        }
        let Some(attr) = func.manual_contract_root_attr(db) else {
            continue;
        };
        let (contract_name, section) = match attr {
            ManualContractRootAttr::Init { contract_name } => {
                (contract_name.data(db), ManualContractSection::Init)
            }
            ManualContractRootAttr::Runtime { contract_name } => {
                (contract_name.data(db), ManualContractSection::Runtime)
            }
            ManualContractRootAttr::Error(err) => {
                return Err(LowerError::Unsupported(format!(
                    "invalid manual contract root attr on `{}`: {err:?}",
                    func.name(db)
                        .to_opt()
                        .map(|name| name.data(db).to_string())
                        .unwrap_or_else(|| "<anonymous>".to_string())
                )));
            }
        };
        if !func.arg_tys(db).is_empty() || func.return_ty(db) != TyId::unit(db) {
            return Err(LowerError::Unsupported(format!(
                "manual contract root `{}` must be monomorphic, unit-returning, and take no ordinary value params",
                func.name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| "<anonymous>".to_string())
            )));
        }
        roots.push(ManualContractRoot {
            func,
            instance: manual_contract_root_instance(db, func)?,
            contract_name,
            section,
        });
    }
    roots.sort_by_key(|root| {
        (
            root.contract_name.to_string(),
            matches!(root.section, ManualContractSection::Runtime),
            root.func
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string()),
        )
    });
    Ok(roots)
}

pub(crate) fn manual_contract_root_instance<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let semantic = semantic_instance_for_root_owner(db, BodyOwner::Func(func))?;
    let callee = runtime_instance_for_semantic(db, semantic);
    let entry_effect_args = entry_effect_arg_plans(
        db,
        EntryEffectContext::ManualContractRoot { func },
        semantic,
    )?;
    Ok(synthetic_instance(
        db,
        RuntimeSyntheticSpec::ManualContractRoot {
            func,
            callee,
            entry_effect_args: entry_effect_args.into_boxed_slice(),
        },
        Vec::new(),
    ))
}

fn contract_runtime_root<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let mut dispatch = Vec::new();
    let mut default = DispatchDefault::RevertEmpty;
    for recv in contract.recv_views(db) {
        let abi = contract_abi_plan(db, contract.scope(), recv.abi_ty(db))?;
        for arm in recv.arms(db) {
            let (abi_info, wrapper) = contract_recv_wrapper(db, arm, abi.clone())?;
            if abi_info.is_fallback {
                if matches!(default, DispatchDefault::Call { .. }) {
                    return Err(LowerError::Unsupported(format!(
                        "contract `{}` has multiple fallback recv arms",
                        contract_name(db, contract)
                    )));
                }
                default = DispatchDefault::Call { wrapper };
                continue;
            }

            let selector = abi_info.selector_value.ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "recv arm in `{}` is missing a resolved selector",
                    contract_name(db, contract)
                ))
            })?;
            dispatch.push(DispatchArm {
                abi: abi.clone(),
                selector,
                wrapper,
            });
        }
    }
    dispatch.sort_by(|left, right| {
        left.abi
            .selector_size
            .cmp(&right.abi.selector_size)
            .then_with(|| left.selector.data(db).cmp(right.selector.data(db)))
    });
    check_dispatch_selector_conflicts(db, contract, &dispatch)?;
    Ok(synthetic_instance(
        db,
        RuntimeSyntheticSpec::ContractRuntimeRoot {
            contract,
            dispatch: dispatch.into_boxed_slice(),
            default,
        },
        Vec::new(),
    ))
}

fn check_dispatch_selector_conflicts<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
    dispatch: &[DispatchArm<'db>],
) -> Result<(), LowerError> {
    let mut seen = Vec::<(Vec<u8>, String)>::new();
    for arm in dispatch {
        let bytes = dispatch_selector_bytes(db, arm)?;
        let text = selector_hex(&bytes);
        for (first_bytes, first_text) in &seen {
            if selector_prefixes_overlap(first_bytes, &bytes) {
                return Err(LowerError::Unsupported(format!(
                    "contract `{}` has overlapping recv selectors: {first_text} overlaps {text}; \
                     each msg variant in a contract must have a unique, non-overlapping selector prefix",
                    contract_name(db, contract),
                )));
            }
        }
        seen.push((bytes, text));
    }
    Ok(())
}

fn dispatch_selector_bytes<'db>(
    db: &'db dyn MirDb,
    arm: &DispatchArm<'db>,
) -> Result<Vec<u8>, LowerError> {
    let raw = arm.selector.data(db).to_bytes_be();
    let selector_size = arm.abi.selector_size as usize;
    if raw.len() > selector_size {
        return Err(LowerError::Unsupported(format!(
            "selector {} does not fit in {} byte ABI selector",
            selector_hex(&raw),
            arm.abi.selector_size,
        )));
    }
    let mut out = vec![0; selector_size.saturating_sub(raw.len())];
    out.extend(raw);
    Ok(out)
}

fn selector_hex(bytes: &[u8]) -> String {
    let mut out = String::from("0x");
    for byte in bytes {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

fn selector_prefixes_overlap(left: &[u8], right: &[u8]) -> bool {
    left.iter().zip(right.iter()).all(|(lhs, rhs)| lhs == rhs)
}

fn contract_init_root<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let init_abi = contract_init_abi(db, contract)?;
    Ok(synthetic_instance(
        db,
        RuntimeSyntheticSpec::ContractInitRoot {
            contract,
            init_abi,
            runtime_region: RuntimeCodeRegion::new(
                db,
                RuntimeCodeRegionKey::ContractRuntime { contract },
            ),
        },
        Vec::new(),
    ))
}

fn contract_init_abi<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let plan = contract_init_abi_plan(db, contract)?;
    Ok(synthetic_instance(
        db,
        RuntimeSyntheticSpec::ContractInitAbi { plan },
        Vec::new(),
    ))
}

fn contract_init_abi_plan<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<ContractInitAbiPlan<'db>, LowerError> {
    let Some(init) = contract.init(db) else {
        return Ok(ContractInitAbiPlan {
            contract,
            payable: false,
            user_init: None,
            entry_effect_args: Box::new([]),
            init_args: InitArgsPlan::None,
        });
    };

    let semantic = semantic_instance_for_root_owner(db, BodyOwner::ContractInit { contract })?;
    let user_init = Some(runtime_instance_for_semantic(db, semantic));
    let entry_effect_args = entry_effect_arg_plans(
        db,
        EntryEffectContext::HighLevelContract { contract },
        semantic,
    )?;
    let projected_fields = visible_init_arg_fields(db, semantic);
    let init_args = if contract.init_args_ty(db) == TyId::unit(db) {
        InitArgsPlan::None
    } else {
        let abi_ty = sol_abi_ty(db, contract.scope())?;
        InitArgsPlan::DecodeInitTail {
            tuple_ty: contract.init_args_ty(db),
            decode_fn: resolve_decode_instance(
                db,
                contract.scope(),
                abi_ty,
                contract.init_args_ty(db),
            )?,
            projected_fields,
        }
    };
    Ok(ContractInitAbiPlan {
        contract,
        payable: init.is_payable(db),
        user_init,
        entry_effect_args: entry_effect_args.into_boxed_slice(),
        init_args,
    })
}

fn contract_recv_wrapper<'db>(
    db: &'db dyn MirDb,
    arm: RecvArmView<'db>,
    abi: ContractAbiPlan<'db>,
) -> Result<(RecvArmAbiInfo<'db>, RuntimeInstance<'db>), LowerError> {
    let contract = arm.contract(db);
    let abi_info = arm.abi_info(db, abi.abi_ty);
    let recv = arm.recv(db);
    let user_recv = runtime_instance_for_semantic(
        db,
        semantic_instance_for_root_owner(
            db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: recv.recv_idx(db),
                arm_idx: arm.arm_idx(db),
            },
        )?,
    );
    let entry_effect_args = entry_effect_arg_plans(
        db,
        EntryEffectContext::HighLevelContract { contract },
        semantic_instance_for_root_owner(
            db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: recv.recv_idx(db),
                arm_idx: arm.arm_idx(db),
            },
        )?,
    )?;
    let projected_fields = visible_recv_arg_fields(
        db,
        semantic_instance_for_root_owner(
            db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: recv.recv_idx(db),
                arm_idx: arm.arm_idx(db),
            },
        )?,
        arm,
    );
    let input = if abi_info.args_ty == TyId::unit(db) {
        RuntimeInputPlan::None
    } else {
        RuntimeInputPlan::DecodeCalldataPayload {
            msg_ty: abi_info.args_ty,
            decode_fn: resolve_decode_instance(db, contract.scope(), abi.abi_ty, abi_info.args_ty)?,
            projected_fields,
        }
    };
    let ret = if let Some(ret_ty) = abi_info.ret_ty {
        RuntimeReturnPlan::Value { ty: ret_ty }
    } else {
        RuntimeReturnPlan::Unit
    };
    let wrapper = synthetic_instance(
        db,
        RuntimeSyntheticSpec::ContractRecvAbi {
            plan: ContractRecvAbiPlan {
                contract,
                abi,
                selector: abi_info.selector_value,
                payable: match arm.arm(db) {
                    Some(recv_arm) => recv_arm.is_payable(db),
                    None => false,
                },
                user_recv,
                entry_effect_args: entry_effect_args.into_boxed_slice(),
                input,
                ret,
            },
        },
        Vec::new(),
    );
    Ok((abi_info, wrapper))
}

fn build_non_contract_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    roots: Vec<RuntimeInstance<'db>>,
    object_specs: Vec<(String, RuntimeSectionName, RuntimeInstance<'db>)>,
    primary_object_name: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    build_sectioned_package(
        db,
        top_mod,
        roots,
        object_specs
            .into_iter()
            .map(|(name, section, entry)| (name, vec![(section, entry)]))
            .collect(),
        primary_object_name,
    )
}

fn build_sectioned_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    roots: Vec<RuntimeInstance<'db>>,
    object_specs: Vec<(String, Vec<(RuntimeSectionName, RuntimeInstance<'db>)>)>,
    primary_object_name: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let root_object_names = object_specs
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<FxHashSet<_>>();
    let mut graph = RuntimeGraphBuilder::new(db, roots, object_specs).build()?;
    let functions = collect_runtime_functions(db, &graph);
    let functions_by_instance = functions
        .iter()
        .map(|function| (function.instance(db), *function))
        .collect::<FxHashMap<_, _>>();
    let const_regions = collect_const_regions(db, &graph);
    let mut reachable_cache = FxHashMap::default();

    let mut objects = std::mem::take(&mut graph.object_specs)
        .into_iter()
        .map(|(name, sections)| {
            make_runtime_object(
                db,
                name,
                sections
                    .into_iter()
                    .map(|(section_name, entry_instance)| {
                        let entry = *functions_by_instance
                            .get(&entry_instance)
                            .expect("section entry should be declared as a runtime function");
                        let reachable = collect_reachable_from_entry(
                            &graph,
                            entry_instance,
                            &mut reachable_cache,
                        );
                        RuntimeSection {
                            name: section_name,
                            entry,
                            embeds: Vec::new(),
                            const_regions: collect_const_regions_for_reachable(
                                db, &graph, &reachable,
                            ),
                        }
                    })
                    .collect(),
            )
        })
        .collect::<Vec<_>>();

    if !graph.code_region_roots.is_empty() {
        let code_regions_object =
            build_code_regions_object(db, &graph, &functions_by_instance, &mut reachable_cache);
        objects.push(code_regions_object);
    }

    let code_regions = resolve_code_regions(
        db,
        &objects,
        &functions_by_instance,
        &graph.code_region_roots,
    );
    let code_region_map = code_regions
        .iter()
        .map(|region| (region.region(db), *region))
        .collect::<FxHashMap<_, _>>();
    objects = objects
        .into_iter()
        .map(|object| {
            rewrite_object_embeds(db, &graph, object, &code_region_map, &mut reachable_cache)
        })
        .collect();
    objects = remap_object_section_refs(db, &objects);
    let code_regions = remap_resolved_code_regions(db, &objects, code_regions);

    let root_objects: Vec<_> = objects
        .iter()
        .filter(|object| root_object_names.contains(&object.name(db)))
        .copied()
        .collect();
    let primary_object = primary_object_name.and_then(|primary| {
        objects
            .iter()
            .find(|object| object.name(db) == primary)
            .copied()
    });

    Ok(RuntimePackage::new(
        db,
        top_mod,
        functions,
        RuntimePackagePlan::new(
            db,
            objects,
            const_regions,
            code_regions,
            root_objects,
            primary_object,
        ),
    ))
}

fn build_code_regions_object<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
    functions_by_instance: &FxHashMap<RuntimeInstance<'db>, RuntimeFunction<'db>>,
    reachable_cache: &mut FxHashMap<RuntimeInstance<'db>, FxHashSet<RuntimeInstance<'db>>>,
) -> RuntimeObject<'db> {
    let sections = graph
        .code_region_roots
        .iter()
        .map(|(region, instance)| {
            let RuntimeCodeRegionKey::FunctionRoot { symbol, .. } = region.key(db).clone() else {
                unreachable!();
            };
            let entry = *functions_by_instance
                .get(instance)
                .expect("code-region root should be declared as a runtime function");
            let reachable = collect_reachable_from_entry(graph, *instance, reachable_cache);
            RuntimeSection {
                name: RuntimeSectionName::CodeRegion(symbol),
                entry,
                embeds: Vec::new(),
                const_regions: collect_const_regions_for_reachable(db, graph, &reachable),
            }
        })
        .collect();
    make_runtime_object(db, "CodeRegions".to_string(), sections)
}

fn rewrite_object_embeds<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
    object: RuntimeObject<'db>,
    code_region_map: &FxHashMap<RuntimeCodeRegion<'db>, ResolvedCodeRegion<'db>>,
    reachable_cache: &mut FxHashMap<RuntimeInstance<'db>, FxHashSet<RuntimeInstance<'db>>>,
) -> RuntimeObject<'db> {
    let section_refs = code_region_map
        .iter()
        .map(|(region, resolved)| (*region, resolved.source(db).clone()))
        .collect::<FxHashMap<_, _>>();
    let sections = object
        .sections(db)
        .iter()
        .cloned()
        .map(|mut section| {
            let reachable =
                collect_reachable_from_entry(graph, section.entry.instance(db), reachable_cache);
            section.embeds = collect_region_embeds(
                db,
                graph,
                &reachable,
                &section_refs,
                RuntimeSectionRef::Local {
                    object,
                    section: section.name.clone(),
                },
            );
            section
        })
        .collect();
    make_runtime_object(db, object.name(db).clone(), sections)
}

fn remap_resolved_code_regions<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
    code_regions: Vec<ResolvedCodeRegion<'db>>,
) -> Vec<ResolvedCodeRegion<'db>> {
    code_regions
        .into_iter()
        .map(|region| {
            make_resolved_code_region(
                db,
                region.region(db),
                region.symbol(db).clone(),
                remap_section_ref(db, objects, region.source(db).clone()),
                region.root(db),
            )
        })
        .collect()
}

fn remap_object_section_refs<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
) -> Vec<RuntimeObject<'db>> {
    objects
        .iter()
        .map(|object| {
            let sections = object
                .sections(db)
                .iter()
                .cloned()
                .map(|mut section| {
                    section.embeds = section
                        .embeds
                        .into_iter()
                        .map(|embed| crate::runtime::RuntimeEmbed {
                            source: remap_section_ref(db, objects, embed.source),
                            as_symbol: embed.as_symbol,
                        })
                        .collect();
                    section
                })
                .collect();
            make_runtime_object(db, object.name(db).clone(), sections)
        })
        .collect()
}

fn remap_section_ref<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
    section_ref: RuntimeSectionRef<'db>,
) -> RuntimeSectionRef<'db> {
    let (old_object, section, is_local) = match section_ref {
        RuntimeSectionRef::Local { object, section } => (object, section, true),
        RuntimeSectionRef::External { object, section } => (object, section, false),
    };
    let object = objects
        .iter()
        .find(|candidate| candidate.name(db) == old_object.name(db))
        .copied()
        .unwrap_or_else(|| {
            panic!(
                "missing rewritten runtime object `{}` while remapping section ref",
                old_object.name(db)
            )
        });
    if is_local {
        RuntimeSectionRef::Local { object, section }
    } else {
        RuntimeSectionRef::External { object, section }
    }
}

fn resolve_code_regions<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
    functions_by_instance: &FxHashMap<RuntimeInstance<'db>, RuntimeFunction<'db>>,
    function_roots: &[(RuntimeCodeRegion<'db>, RuntimeInstance<'db>)],
) -> Vec<ResolvedCodeRegion<'db>> {
    let mut resolved = Vec::new();
    for object in objects {
        for section in object.sections(db) {
            let Some((region, symbol)) = resolved_section_region(db, &section) else {
                continue;
            };
            resolved.push(make_resolved_code_region(
                db,
                region,
                symbol,
                RuntimeSectionRef::Local {
                    object: *object,
                    section: section.name.clone(),
                },
                section.entry,
            ));
        }
    }

    if let Some(code_regions_object) = objects
        .iter()
        .find(|object| object.name(db) == "CodeRegions")
    {
        for (region, root_instance) in function_roots {
            let RuntimeCodeRegionKey::FunctionRoot { symbol, .. } = region.key(db).clone() else {
                continue;
            };
            let Some(_section) = code_regions_object
                .sections(db)
                .iter()
                .find(|section| section.name == RuntimeSectionName::CodeRegion(symbol.clone()))
            else {
                continue;
            };
            resolved.push(make_resolved_code_region(
                db,
                *region,
                symbol.clone(),
                RuntimeSectionRef::Local {
                    object: *code_regions_object,
                    section: RuntimeSectionName::CodeRegion(symbol),
                },
                *functions_by_instance
                    .get(root_instance)
                    .expect("code-region root should be declared as a runtime function"),
            ));
        }
    }

    resolved.sort_by_key(|region| region.symbol(db).clone());
    resolved
}

fn resolved_section_region<'db>(
    db: &'db dyn MirDb,
    section: &RuntimeSection<'db>,
) -> Option<(RuntimeCodeRegion<'db>, String)> {
    match section.entry.owner(db) {
        RuntimeFunctionOwner::Synthetic(
            RuntimeSyntheticSpec::ContractInitRoot { contract, .. }
            | RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. },
        ) => match section.name {
            RuntimeSectionName::Init => Some((
                RuntimeCodeRegion::new(db, RuntimeCodeRegionKey::ContractInit { contract }),
                format!("{}_init", contract_name(db, contract)),
            )),
            RuntimeSectionName::Runtime => Some((
                RuntimeCodeRegion::new(db, RuntimeCodeRegionKey::ContractRuntime { contract }),
                format!("{}_runtime", contract_name(db, contract)),
            )),
            RuntimeSectionName::Main
            | RuntimeSectionName::Test(_)
            | RuntimeSectionName::CodeRegion(_) => None,
        },
        RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ManualContractRoot {
            func, ..
        }) => {
            let region = runtime_code_region_for_manual_root(db, func)?;
            Some((region, code_region_symbol(db, region)))
        }
        RuntimeFunctionOwner::Semantic(semantic) => {
            let BodyOwner::Func(func) = semantic.key(db).owner(db) else {
                return None;
            };
            let region = runtime_code_region_for_manual_root(db, func)?;
            Some((region, code_region_symbol(db, region)))
        }
        RuntimeFunctionOwner::Synthetic(
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. }
            | RuntimeSyntheticSpec::ContractRecvAbi { .. }
            | RuntimeSyntheticSpec::CodeRegionRoot { .. },
        ) => None,
    }
}

fn collect_region_embeds<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
    reachable: &FxHashSet<RuntimeInstance<'db>>,
    section_refs: &FxHashMap<RuntimeCodeRegion<'db>, RuntimeSectionRef<'db>>,
    current_section: RuntimeSectionRef<'db>,
) -> Vec<crate::runtime::RuntimeEmbed<'db>> {
    let current_object = match &current_section {
        RuntimeSectionRef::Local { object, .. } | RuntimeSectionRef::External { object, .. } => {
            *object
        }
    };
    let mut seen = FxHashSet::default();
    let mut embeds = Vec::new();
    let mut instances = reachable.iter().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| runtime_instance_sort_key(db, *instance));
    for instance in instances {
        let Some(node) = graph.nodes.get(&instance) else {
            continue;
        };
        for region in node.referenced_code_regions.iter().copied() {
            let Some(source) = section_refs.get(&region) else {
                continue;
            };
            if *source == current_section || !seen.insert(region) {
                continue;
            }
            let source = match source {
                RuntimeSectionRef::Local { object, section }
                | RuntimeSectionRef::External { object, section }
                    if *object == current_object =>
                {
                    RuntimeSectionRef::Local {
                        object: *object,
                        section: section.clone(),
                    }
                }
                RuntimeSectionRef::Local { object, section }
                | RuntimeSectionRef::External { object, section } => RuntimeSectionRef::External {
                    object: *object,
                    section: section.clone(),
                },
            };
            embeds.push(crate::runtime::RuntimeEmbed {
                source,
                as_symbol: code_region_symbol(db, region),
            });
        }
    }
    embeds.sort_by(|lhs, rhs| lhs.as_symbol.cmp(&rhs.as_symbol));
    embeds
}

fn collect_reachable_from_entry<'db>(
    graph: &RuntimeGraph<'db>,
    entry: RuntimeInstance<'db>,
    cache: &mut FxHashMap<RuntimeInstance<'db>, FxHashSet<RuntimeInstance<'db>>>,
) -> FxHashSet<RuntimeInstance<'db>> {
    if let Some(reachable) = cache.get(&entry) {
        return reachable.clone();
    }
    let mut seen = FxHashSet::default();
    let mut stack = vec![entry];
    while let Some(instance) = stack.pop() {
        if !seen.insert(instance) {
            continue;
        }
        if let Some(node) = graph.nodes.get(&instance) {
            for callee in node.direct_callees.iter().copied() {
                stack.push(callee);
            }
        }
    }
    cache.insert(entry, seen.clone());
    seen
}

fn collect_const_regions_for_reachable<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
    reachable: &FxHashSet<RuntimeInstance<'db>>,
) -> Vec<ConstRegionId<'db>> {
    let mut seen = FxHashSet::default();
    let mut regions = Vec::new();
    let mut instances = reachable.iter().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| runtime_instance_sort_key(db, *instance));
    for instance in instances {
        let Some(node) = graph.nodes.get(&instance) else {
            continue;
        };
        for region in node.referenced_const_regions.iter().copied() {
            if seen.insert(region) {
                regions.push(region);
            }
        }
    }
    regions
}

fn materialized_contracts_for_roots<'db>(
    db: &'db dyn MirDb,
    roots: &[RuntimeInstance<'db>],
) -> FxHashSet<Contract<'db>> {
    roots
        .iter()
        .filter_map(|root| match root.key(db).source(db) {
            RuntimeInstanceSource::Synthetic(synthetic) => match synthetic.spec(db) {
                RuntimeSyntheticSpec::ContractInitRoot { contract, .. }
                | RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. } => Some(contract),
                RuntimeSyntheticSpec::MainRoot { .. }
                | RuntimeSyntheticSpec::TestRoot { .. }
                | RuntimeSyntheticSpec::ManualContractRoot { .. }
                | RuntimeSyntheticSpec::ContractInitAbi { .. }
                | RuntimeSyntheticSpec::ContractRecvAbi { .. }
                | RuntimeSyntheticSpec::CodeRegionRoot { .. } => None,
            },
            RuntimeInstanceSource::Semantic(_) => None,
        })
        .collect()
}

fn semantic_instance_for_root_owner<'db>(
    db: &'db dyn MirDb,
    owner: BodyOwner<'db>,
) -> Result<SemanticInstance<'db>, LowerError> {
    let key = root_semantic_instance_key(db, owner).map_err(|err| match err {
        RootSemanticInstanceError::UnsupportedGenericParam {
            owner,
            owner_scope,
            offending_ty,
            param_idx,
        } => LowerError::Unsupported(format!(
            "root semantic instance for {owner:?} has unsupported generic param {param_idx} in {owner_scope:?}: {}",
            offending_ty.pretty_print(db),
        )),
        RootSemanticInstanceError::MissingRootProvider { owner } => LowerError::Unsupported(
            format!("root semantic instance for {owner:?} is missing a root provider binding"),
        ),
        RootSemanticInstanceError::UnclosedEffectEnv(err) => LowerError::Unsupported(format!(
            "root semantic instance for {:?} is not closed under synthesized root substitution: owner_scope={:?} param_idx={} args_len={} offending_ty={}",
            err.owner,
            err.owner_scope,
            err.param_idx,
            err.args_len,
            err.offending_ty.pretty_print(db),
        )),
    })?;
    Ok(get_or_build_semantic_instance(db, key))
}

fn contract_object_spec<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<ManualContractObjectSpec<'db>, LowerError> {
    let runtime_root = contract_runtime_root(db, contract)?;
    let init_root = contract_init_root(db, contract)?;
    Ok((
        sanitize_object_name(&contract_name(db, contract)),
        vec![
            (RuntimeSectionName::Init, init_root),
            (RuntimeSectionName::Runtime, runtime_root),
        ],
        vec![init_root, runtime_root],
    ))
}

fn is_test_func<'db>(db: &'db dyn MirDb, func: Func<'db>) -> bool {
    ItemKind::from(func)
        .attrs(db)
        .is_some_and(|attrs| attrs.get_attr(db, "test").is_some())
}

fn runtime_root_candidate<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Result<RuntimeRootCandidate<'db>, LowerError> {
    if func.is_associated_func(db) || func.params(db).next().is_some() {
        return Ok(RuntimeRootCandidate::NotRoot);
    }
    let semantic = match root_semantic_instance_key(db, BodyOwner::Func(func)) {
        Ok(key) => get_or_build_semantic_instance(db, key),
        Err(err) => {
            return Ok(RuntimeRootCandidate::Rejected(RuntimeRootRejection {
                func,
                reason: RuntimeRootRejectionReason::RootSemanticInstance(err),
            }));
        }
    };
    if let Err(err) =
        entry_effect_arg_plans(db, EntryEffectContext::StandaloneFunc { func }, semantic)
    {
        return Ok(RuntimeRootCandidate::Rejected(RuntimeRootRejection {
            func,
            reason: RuntimeRootRejectionReason::UnsupportedEntryEffect(err.to_string()),
        }));
    }
    Ok(RuntimeRootCandidate::Root(func))
}

fn is_main_func<'db>(db: &'db dyn MirDb, func: Func<'db>) -> bool {
    func.name(db)
        .to_opt()
        .is_some_and(|name| name.data(db) == "main")
}

fn func_display_name<'db>(db: &'db dyn MirDb, func: Func<'db>) -> String {
    func.name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string())
}

fn format_runtime_root_rejection<'db>(
    db: &'db dyn MirDb,
    rejection: &RuntimeRootRejection<'db>,
) -> String {
    let name = func_display_name(db, rejection.func);
    match &rejection.reason {
        RuntimeRootRejectionReason::RootSemanticInstance(err) => {
            format_root_semantic_instance_rejection(db, &name, err)
        }
        RuntimeRootRejectionReason::UnsupportedEntryEffect(message) => message.clone(),
    }
}

fn format_root_semantic_instance_rejection<'db>(
    db: &'db dyn MirDb,
    func_name: &str,
    err: &RootSemanticInstanceError<'db>,
) -> String {
    match err {
        RootSemanticInstanceError::UnsupportedGenericParam {
            offending_ty,
            param_idx,
            ..
        } if is_implicit_layout_const_param(db, *offending_ty) => format!(
            "function `{func_name}` cannot be used as a standalone runtime root because an effect provider type contains an inferred layout const parameter `{}` at generic parameter {param_idx}; roots cannot declare wildcard effect providers because there is no caller to supply a concrete provider. Move the effectful logic into a helper and call it from `{func_name}` with a concrete provider using `with (...)`, or use a contract field/provider context",
            offending_ty.pretty_print(db),
        ),
        RootSemanticInstanceError::UnsupportedGenericParam {
            offending_ty,
            param_idx,
            ..
        } => format!(
            "function `{func_name}` cannot be used as a standalone runtime root because generic parameter {param_idx} is not supported for root instantiation: {}",
            offending_ty.pretty_print(db),
        ),
        RootSemanticInstanceError::MissingRootProvider { .. } => format!(
            "function `{func_name}` cannot be used as a standalone runtime root because an effect provider could not be synthesized"
        ),
        RootSemanticInstanceError::UnclosedEffectEnv(err) => format!(
            "function `{func_name}` cannot be used as a standalone runtime root because its effect environment is not fully concrete: parameter {} is missing while instantiating {}",
            err.param_idx,
            err.offending_ty.pretty_print(db),
        ),
    }
}

fn is_implicit_layout_const_param<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> bool {
    if let TyData::ConstTy(const_ty) = ty.data(db)
        && let ConstTyData::TyParam(param, _) = const_ty.data(db)
    {
        return param.is_implicit();
    }
    false
}

pub(crate) fn runtime_instance_for_semantic<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> RuntimeInstance<'db> {
    let typed_body = semantic.key(db).typed_body(db);
    let owner = semantic.key(db).owner(db);
    if let BodyOwner::Func(func) = owner
        && func.body(db).is_none()
    {
        panic!(
            "bodyless semantic function leaked into runtime instance construction: func={func:?} key={:?}",
            semantic.key(db)
        );
    }
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let mut params = Vec::new();
    let mut idx = 0;
    while let Some(binding) = typed_body.param_binding(idx) {
        if let Some(class) = runtime_visible_binding_class(db, semantic, binding)
            .map(|class| runtime_param_class(db, typed_body, binding, env, class))
        {
            params.push(class);
        }
        idx += 1;
    }
    if let BodyOwner::ContractRecvArm {
        contract,
        recv_idx,
        arm_idx,
    } = owner
    {
        let recv = hir::semantic::RecvView::new(db, contract, recv_idx);
        let arm = RecvArmView::new(db, recv, arm_idx);
        for arg_binding in arm.arg_bindings(db) {
            let Some(binding) = typed_body.pat_binding(arg_binding.pat) else {
                continue;
            };
            let ty = semantic.binding_ty(db, binding);
            if let Some(class) =
                top_level_class_for_ty_in_env(db, env, ty, AddressSpaceKind::Memory)
            {
                params.push(class);
            }
        }
    }
    for binding in owner_effect_bindings(db, owner) {
        if let Some(class) = owner_effect_binding_class(db, semantic, binding) {
            params.push(class);
        }
    }
    let key = RuntimeInstanceKey::new(db, RuntimeInstanceSource::Semantic(semantic), params);
    get_or_build_runtime_instance(db, key)
}

fn owner_effect_binding_class<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: hir::analysis::ty::ty_check::LocalBinding<'db>,
) -> Option<crate::runtime::RuntimeClass<'db>> {
    runtime_effect_binding_plan(db, semantic, binding).map(|plan| plan.class)
}

fn synthetic_instance<'db>(
    db: &'db dyn MirDb,
    spec: RuntimeSyntheticSpec<'db>,
    params: Vec<crate::runtime::RuntimeClass<'db>>,
) -> RuntimeInstance<'db> {
    let synthetic = RuntimeSyntheticInstance::new(db, spec);
    let key = RuntimeInstanceKey::new(db, RuntimeInstanceSource::Synthetic(synthetic), params);
    get_or_build_runtime_instance(db, key)
}

fn contract_abi_plan<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    abi_ty: TyId<'db>,
) -> Result<ContractAbiPlan<'db>, LowerError> {
    let assumptions = PredicateListId::empty_list(db);
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])
        .ok_or_else(|| LowerError::Unsupported("missing required core::abi::Abi".to_string()))?;
    let abi_inst = TraitInstId::new_simple(db, abi_trait, vec![abi_ty]);

    let selector_ty = normalize_ty(
        db,
        TyId::assoc_ty(db, abi_inst, IdentId::new(db, "Selector".to_string())),
        scope,
        assumptions,
    );
    let selector_class = crate::runtime::lower::type_info::scalar_class_for_ty_in_env(
        db,
        RuntimeTypeEnv::new(Some(scope), assumptions),
        selector_ty,
    )
    .ok_or_else(|| {
        LowerError::Unsupported(format!(
            "ABI `{}` selector type `{}` is not a scalar runtime type",
            abi_ty.pretty_print(db),
            selector_ty.pretty_print(db),
        ))
    })?;
    if !matches!(selector_class.repr, ScalarRepr::Int { signed: false, .. }) {
        return Err(LowerError::Unsupported(format!(
            "ABI `{}` selector type `{}` must lower to an unsigned integer",
            abi_ty.pretty_print(db),
            selector_ty.pretty_print(db),
        )));
    }

    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let selector_size_name = IdentId::new(db, "SELECTOR_SIZE".to_string());
    let selector_size_body =
        assoc_const_body_for_trait_inst(db, solve_cx, abi_inst, selector_size_name).ok_or_else(
            || {
                LowerError::Unsupported(format!(
                    "missing evaluable associated const `SELECTOR_SIZE` for ABI `{}`",
                    abi_ty.pretty_print(db),
                ))
            },
        )?;
    let selector_size = match eval_body_owner_const(
        db,
        BodyOwner::AnonConstBody {
            body: selector_size_body,
            expected: TyId::u256(db),
        },
        Vec::new(),
    ) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value.to_u64().ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "associated const `SELECTOR_SIZE` for ABI `{}` is too large",
                    abi_ty.pretty_print(db),
                ))
            })?,
            _ => {
                return Err(LowerError::Unsupported(format!(
                    "failed to evaluate associated const `SELECTOR_SIZE` for ABI `{}`",
                    abi_ty.pretty_print(db),
                )));
            }
        },
        Err(_) => {
            return Err(LowerError::Unsupported(format!(
                "failed to evaluate associated const `SELECTOR_SIZE` for ABI `{}`",
                abi_ty.pretty_print(db),
            )));
        }
    };
    if selector_size > 32 {
        return Err(LowerError::Unsupported(format!(
            "ABI `{}` selector size {selector_size} exceeds one EVM word",
            abi_ty.pretty_print(db),
        )));
    }

    Ok(ContractAbiPlan {
        abi_ty,
        selector_ty,
        selector_size,
        selector_class,
    })
}

fn resolve_decode_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    abi_ty: TyId<'db>,
    ty: TyId<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let decoder_ty = abi_decoder_ty(db, scope, abi_ty)?;
    let decode_trait = resolve_core_trait(db, scope, &["abi", "Decode"])
        .ok_or_else(|| LowerError::Unsupported("missing required core::abi::Decode".to_string()))?;
    let inst = TraitInstId::new_simple(db, decode_trait, vec![ty, abi_ty]);
    resolve_trait_runtime_instance(db, scope, inst, "decode_payload", vec![decoder_ty])
}

fn abi_decoder_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    abi_ty: TyId<'db>,
) -> Result<TyId<'db>, LowerError> {
    let assumptions = PredicateListId::empty_list(db);
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])
        .ok_or_else(|| LowerError::Unsupported("missing required core::abi::Abi".to_string()))?;
    let abi_inst = TraitInstId::new_simple(db, abi_trait, vec![abi_ty]);
    let decoder_ctor = normalize_ty(
        db,
        TyId::assoc_ty(db, abi_inst, IdentId::new(db, "Decoder".to_string())),
        scope,
        assumptions,
    );
    Ok(TyId::app(db, decoder_ctor, memory_bytes_ty(db, scope)?))
}

fn resolve_trait_runtime_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    inst: TraitInstId<'db>,
    method: &str,
    extra_generic_args: Vec<TyId<'db>>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let assumptions = hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db);
    let method = IdentId::new(db, method.to_string());
    let (func, mut impl_args) = resolve_trait_method_instance(
        db,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
        inst,
        method,
    )
    .ok_or_else(|| {
        LowerError::Unsupported(format!(
            "failed to resolve trait method `{}` for runtime package planning",
            method.data(db)
        ))
    })?;
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
) -> Result<TyId<'db>, LowerError> {
    resolve_lib_type_path(db, scope, "std::abi::Sol")
        .ok_or_else(|| LowerError::Unsupported("missing std::abi::Sol".to_string()))
}

fn visible_init_arg_fields<'db>(db: &'db dyn MirDb, semantic: SemanticInstance<'db>) -> Box<[u32]> {
    runtime_visible_binding_plans(db, semantic)
        .iter()
        .filter_map(|entry| match entry.binding {
            LocalBinding::Param { idx, .. } => Some(idx as u32),
            LocalBinding::Local { .. } | LocalBinding::EffectParam { .. } => None,
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn visible_recv_arg_fields<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    arm: RecvArmView<'db>,
) -> Box<[u32]> {
    let tuple_indices_by_pat = arm
        .arg_bindings(db)
        .iter()
        .map(|binding| (binding.pat, binding.tuple_index))
        .collect::<FxHashMap<_, _>>();
    runtime_visible_binding_plans(db, semantic)
        .iter()
        .filter_map(|entry| match entry.binding {
            LocalBinding::Local { pat, .. } => tuple_indices_by_pat.get(&pat).copied(),
            LocalBinding::Param { .. } | LocalBinding::EffectParam { .. } => None,
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn memory_bytes_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Result<TyId<'db>, LowerError> {
    resolve_lib_type_path(db, scope, "std::evm::memory_input::MemoryBytes").ok_or_else(|| {
        LowerError::Unsupported("missing std::evm::memory_input::MemoryBytes".to_string())
    })
}

fn make_runtime_function<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    symbol: String,
    linkage: RuntimeLinkage,
    inline_hint: RuntimeInlineHint,
    owner: RuntimeFunctionOwner<'db>,
    referenced_const_regions: Vec<ConstRegionId<'db>>,
) -> RuntimeFunction<'db> {
    RuntimeFunction::new(
        db,
        instance,
        symbol,
        linkage,
        inline_hint,
        owner,
        referenced_const_regions,
    )
}

fn make_runtime_object<'db>(
    db: &'db dyn MirDb,
    name: String,
    sections: Vec<RuntimeSection<'db>>,
) -> RuntimeObject<'db> {
    RuntimeObject::new(db, name, sections)
}

fn make_resolved_code_region<'db>(
    db: &'db dyn MirDb,
    region: RuntimeCodeRegion<'db>,
    symbol: String,
    source: RuntimeSectionRef<'db>,
    root: RuntimeFunction<'db>,
) -> ResolvedCodeRegion<'db> {
    ResolvedCodeRegion::new(db, region, symbol, source, root)
}

const RUNTIME_SYMBOL_DISAMBIG_HASH_LEN: usize = 4;

fn collect_runtime_functions<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
) -> Vec<RuntimeFunction<'db>> {
    let mut instances = graph.nodes.keys().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| runtime_instance_sort_key(db, *instance));
    let instance_symbols = instances
        .into_iter()
        .map(|instance| {
            (
                instance,
                runtime_instance_symbol_base(db, instance),
                runtime_instance_symbol_key(db, instance),
            )
        })
        .collect::<Vec<_>>();
    let duplicate_counts =
        instance_symbols
            .iter()
            .fold(FxHashMap::default(), |mut counts, (_, base, _)| {
                *counts.entry(base.clone()).or_insert(0usize) += 1;
                counts
            });
    let mut emitted_counts = FxHashMap::<String, usize>::default();
    let mut functions = instance_symbols
        .into_iter()
        .map(|(instance, base, symbol_key)| {
            let needs_disambiguator = duplicate_counts.get(&base).copied().unwrap_or_default() > 1;
            let symbol = runtime_instance_symbol(
                base,
                &symbol_key,
                needs_disambiguator,
                &mut emitted_counts,
            );
            runtime_function_for_instance(
                db,
                instance,
                symbol,
                graph
                    .nodes
                    .get(&instance)
                    .expect("runtime graph should contain every materialized instance")
                    .referenced_const_regions
                    .clone(),
            )
        })
        .collect::<Vec<_>>();
    functions.sort_by_key(|function| function.symbol(db));
    functions
}

fn runtime_instance_symbol(
    base: String,
    stable_key: &str,
    needs_disambiguator: bool,
    emitted_counts: &mut FxHashMap<String, usize>,
) -> String {
    let mut symbol = if needs_disambiguator {
        let hash = stable_identity_hash(stable_key);
        format!("{base}_{}", &hash[..RUNTIME_SYMBOL_DISAMBIG_HASH_LEN])
    } else {
        base
    };
    let ordinal = emitted_counts.entry(symbol.clone()).or_insert(0);
    if *ordinal > 0 {
        symbol = format!("{symbol}_{ordinal}");
    }
    *ordinal += 1;
    symbol
}

fn runtime_function_for_instance<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    symbol: String,
    referenced_const_regions: Vec<ConstRegionId<'db>>,
) -> RuntimeFunction<'db> {
    match instance.key(db).source(db) {
        RuntimeInstanceSource::Semantic(semantic) => make_runtime_function(
            db,
            instance,
            symbol,
            RuntimeLinkage::Private,
            inline_hint_for_semantic(db, semantic),
            RuntimeFunctionOwner::Semantic(semantic),
            referenced_const_regions,
        ),
        RuntimeInstanceSource::Synthetic(synthetic) => {
            let spec = synthetic.spec(db).clone();
            make_runtime_function(
                db,
                instance,
                symbol,
                RuntimeLinkage::Private,
                RuntimeInlineHint::Auto,
                RuntimeFunctionOwner::Synthetic(spec),
                referenced_const_regions,
            )
        }
    }
}

pub fn runtime_instance_stable_key<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    runtime_instance_sort_key(db, instance)
}

pub fn runtime_instance_symbol_key<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    runtime_instance_symbol_key_query(db, instance)
}

fn runtime_instance_sort_key<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> String {
    runtime_instance_sort_key_query(db, instance)
}

fn optional_selector_key<'db>(db: &'db dyn MirDb, selector: Option<IntegerId<'db>>) -> String {
    selector
        .map(|selector| selector_key(db, selector))
        .unwrap_or_else(|| "fallback".to_string())
}

fn selector_key<'db>(db: &'db dyn MirDb, selector: IntegerId<'db>) -> String {
    selector.data(db).to_str_radix(10)
}

#[salsa::tracked]
fn runtime_instance_symbol_key_query<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    let key = instance.key(db);
    let source = runtime_instance_source_symbol_key(db, key.source(db));
    let params = key
        .params(db)
        .iter()
        .map(|param| runtime_class_sort_key(db, param))
        .collect::<Vec<_>>()
        .join(",");
    format!("{source}:params[{params}]")
}

#[salsa::tracked]
fn runtime_instance_sort_key_query<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    let key = instance.key(db);
    let source = runtime_instance_source_sort_key(db, key.source(db));
    let params = key
        .params(db)
        .iter()
        .map(|param| runtime_class_sort_key(db, param))
        .collect::<Vec<_>>()
        .join(",");
    format!("{source}:params[{params}]")
}

fn runtime_instance_source_symbol_key<'db>(
    db: &'db dyn MirDb,
    source: RuntimeInstanceSource<'db>,
) -> String {
    match source {
        RuntimeInstanceSource::Semantic(semantic) => {
            format!(
                "semantic:{}",
                semantic_instance_symbol_identity(db, semantic)
            )
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_spec_symbol_key(db, synthetic.spec(db))
        }
    }
}

fn runtime_instance_source_sort_key<'db>(
    db: &'db dyn MirDb,
    source: RuntimeInstanceSource<'db>,
) -> String {
    match source {
        RuntimeInstanceSource::Semantic(semantic) => {
            format!("semantic:{}", semantic_instance_identity(db, semantic))
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_spec_sort_key(db, synthetic.spec(db))
        }
    }
}

fn runtime_synthetic_spec_symbol_key<'db>(
    db: &'db dyn MirDb,
    spec: RuntimeSyntheticSpec<'db>,
) -> String {
    match spec {
        RuntimeSyntheticSpec::MainRoot {
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:main_root:{}:{}",
            runtime_instance_symbol_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::TestRoot {
            name,
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:test_root:{name}:{}:{}",
            runtime_instance_symbol_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::ManualContractRoot {
            func,
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:manual_contract_root:{}:{}:{}",
            item_identity(db, func.into()),
            runtime_instance_symbol_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::ContractInitAbi { plan } => format!(
            "__synthetic:contract_init_abi:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            plan.payable,
            plan.user_init
                .map(|instance| runtime_instance_symbol_key(db, instance))
                .unwrap_or_default(),
            init_args_plan_symbol_key(db, &plan.init_args)
        ),
        RuntimeSyntheticSpec::ContractRecvAbi { plan } => format!(
            "__synthetic:contract_recv_abi:{}:{}:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            optional_selector_key(db, plan.selector),
            type_identity(db, plan.abi.abi_ty),
            plan.payable,
            runtime_instance_symbol_key(db, plan.user_recv),
            runtime_input_plan_symbol_key(db, &plan.input)
        ),
        RuntimeSyntheticSpec::ContractInitRoot {
            contract,
            init_abi,
            runtime_region,
        } => format!(
            "__synthetic:contract_init_root:{}:{}:{}",
            item_identity(db, contract.into()),
            runtime_instance_symbol_key(db, init_abi),
            runtime_code_region_symbol_key(db, runtime_region)
        ),
        RuntimeSyntheticSpec::ContractRuntimeRoot {
            contract,
            dispatch,
            default,
        } => format!(
            "__synthetic:contract_runtime_root:{}:{}:{}",
            item_identity(db, contract.into()),
            dispatch
                .iter()
                .map(|arm| format!(
                    "{}:{}:{}",
                    type_identity(db, arm.abi.abi_ty),
                    selector_key(db, arm.selector),
                    runtime_instance_symbol_key(db, arm.wrapper)
                ))
                .collect::<Vec<_>>()
                .join(","),
            dispatch_default_symbol_key(db, &default)
        ),
        RuntimeSyntheticSpec::CodeRegionRoot { symbol, callee } => {
            format!(
                "__synthetic:code_region_root:{symbol}:{}",
                runtime_instance_symbol_key(db, callee)
            )
        }
    }
}

fn runtime_synthetic_spec_sort_key<'db>(
    db: &'db dyn MirDb,
    spec: RuntimeSyntheticSpec<'db>,
) -> String {
    match spec {
        RuntimeSyntheticSpec::MainRoot {
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:main_root:{}:{}",
            runtime_instance_sort_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::TestRoot {
            name,
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:test_root:{name}:{}:{}",
            runtime_instance_sort_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::ManualContractRoot {
            func,
            callee,
            entry_effect_args,
        } => format!(
            "__synthetic:manual_contract_root:{}:{}:{}",
            item_identity(db, func.into()),
            runtime_instance_sort_key(db, callee),
            entry_effect_args_sort_key(db, entry_effect_args.as_ref())
        ),
        RuntimeSyntheticSpec::ContractInitAbi { plan } => format!(
            "__synthetic:contract_init_abi:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            plan.payable,
            plan.user_init
                .map(|instance| runtime_instance_sort_key(db, instance))
                .unwrap_or_default(),
            init_args_plan_sort_key(db, &plan.init_args)
        ),
        RuntimeSyntheticSpec::ContractRecvAbi { plan } => format!(
            "__synthetic:contract_recv_abi:{}:{}:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            optional_selector_key(db, plan.selector),
            type_identity(db, plan.abi.abi_ty),
            plan.payable,
            runtime_instance_sort_key(db, plan.user_recv),
            runtime_input_plan_sort_key(db, &plan.input)
        ),
        RuntimeSyntheticSpec::ContractInitRoot {
            contract,
            init_abi,
            runtime_region,
        } => format!(
            "__synthetic:contract_init_root:{}:{}:{}",
            item_identity(db, contract.into()),
            runtime_instance_sort_key(db, init_abi),
            runtime_code_region_sort_key(db, runtime_region)
        ),
        RuntimeSyntheticSpec::ContractRuntimeRoot {
            contract,
            dispatch,
            default,
        } => format!(
            "__synthetic:contract_runtime_root:{}:{}:{}",
            item_identity(db, contract.into()),
            dispatch
                .iter()
                .map(|arm| format!(
                    "{}:{}:{}",
                    type_identity(db, arm.abi.abi_ty),
                    selector_key(db, arm.selector),
                    runtime_instance_sort_key(db, arm.wrapper)
                ))
                .collect::<Vec<_>>()
                .join(","),
            dispatch_default_sort_key(db, &default)
        ),
        RuntimeSyntheticSpec::CodeRegionRoot { symbol, callee } => {
            format!(
                "__synthetic:code_region_root:{symbol}:{}",
                runtime_instance_sort_key(db, callee)
            )
        }
    }
}

fn entry_effect_args_sort_key<'db>(db: &'db dyn MirDb, args: &[EntryEffectArgPlan<'db>]) -> String {
    args.iter()
        .map(|arg| match arg {
            EntryEffectArgPlan::ContractField(binding) => format!(
                "field:{}:{}:{}:{}",
                binding.slot,
                type_identity(db, binding.declared_ty),
                runtime_class_sort_key(db, &binding.class),
                ref_kind_sort_key(db, &binding.kind)
            ),
            EntryEffectArgPlan::TargetRootProvider(binding) => format!(
                "root:{}:{}:{}",
                type_identity(db, binding.declared_ty),
                runtime_class_sort_key(db, &binding.class),
                target_root_provider_materialization_sort_key(db, binding.materialization)
            ),
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn init_args_plan_symbol_key<'db>(db: &'db dyn MirDb, plan: &InitArgsPlan<'db>) -> String {
    match plan {
        InitArgsPlan::None => "none".to_string(),
        InitArgsPlan::DecodeInitTail {
            tuple_ty,
            decode_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:{projected_fields:?}",
            type_identity(db, *tuple_ty),
            runtime_instance_symbol_key(db, *decode_fn)
        ),
    }
}

fn init_args_plan_sort_key<'db>(db: &'db dyn MirDb, plan: &InitArgsPlan<'db>) -> String {
    match plan {
        InitArgsPlan::None => "none".to_string(),
        InitArgsPlan::DecodeInitTail {
            tuple_ty,
            decode_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:{projected_fields:?}",
            type_identity(db, *tuple_ty),
            runtime_instance_sort_key(db, *decode_fn)
        ),
    }
}

fn runtime_input_plan_symbol_key<'db>(db: &'db dyn MirDb, plan: &RuntimeInputPlan<'db>) -> String {
    match plan {
        RuntimeInputPlan::None => "none".to_string(),
        RuntimeInputPlan::DecodeCalldataPayload {
            msg_ty,
            decode_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:{projected_fields:?}",
            type_identity(db, *msg_ty),
            runtime_instance_symbol_key(db, *decode_fn)
        ),
    }
}

fn runtime_input_plan_sort_key<'db>(db: &'db dyn MirDb, plan: &RuntimeInputPlan<'db>) -> String {
    match plan {
        RuntimeInputPlan::None => "none".to_string(),
        RuntimeInputPlan::DecodeCalldataPayload {
            msg_ty,
            decode_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:{projected_fields:?}",
            type_identity(db, *msg_ty),
            runtime_instance_sort_key(db, *decode_fn)
        ),
    }
}

fn dispatch_default_symbol_key<'db>(db: &'db dyn MirDb, default: &DispatchDefault<'db>) -> String {
    match default {
        DispatchDefault::RevertEmpty => "revert_empty".to_string(),
        DispatchDefault::Call { wrapper } => {
            format!("call:{}", runtime_instance_symbol_key(db, *wrapper))
        }
    }
}

fn dispatch_default_sort_key<'db>(db: &'db dyn MirDb, default: &DispatchDefault<'db>) -> String {
    match default {
        DispatchDefault::RevertEmpty => "revert_empty".to_string(),
        DispatchDefault::Call { wrapper } => {
            format!("call:{}", runtime_instance_sort_key(db, *wrapper))
        }
    }
}

fn runtime_code_region_symbol_key<'db>(
    db: &'db dyn MirDb,
    region: RuntimeCodeRegion<'db>,
) -> String {
    match region.key(db) {
        RuntimeCodeRegionKey::ContractInit { contract } => {
            format!("contract_init:{}", item_identity(db, contract.into()))
        }
        RuntimeCodeRegionKey::ContractRuntime { contract } => {
            format!("contract_runtime:{}", item_identity(db, contract.into()))
        }
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            format!("manual_root:{}", item_identity(db, func.into()))
        }
        RuntimeCodeRegionKey::FunctionRoot { symbol, callee } => {
            format!(
                "function_root:{symbol}:{}",
                runtime_instance_symbol_key(db, callee)
            )
        }
    }
}

fn runtime_code_region_sort_key<'db>(db: &'db dyn MirDb, region: RuntimeCodeRegion<'db>) -> String {
    match region.key(db) {
        RuntimeCodeRegionKey::ContractInit { contract } => {
            format!("contract_init:{}", item_identity(db, contract.into()))
        }
        RuntimeCodeRegionKey::ContractRuntime { contract } => {
            format!("contract_runtime:{}", item_identity(db, contract.into()))
        }
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            format!("manual_root:{}", item_identity(db, func.into()))
        }
        RuntimeCodeRegionKey::FunctionRoot { symbol, callee } => {
            format!(
                "function_root:{symbol}:{}",
                runtime_instance_sort_key(db, callee)
            )
        }
    }
}

fn runtime_class_sort_key<'db>(db: &'db dyn MirDb, class: &RuntimeClass<'db>) -> String {
    match class {
        RuntimeClass::Scalar(class) => scalar_class_sort_key(db, class),
        RuntimeClass::AggregateValue { layout } => {
            format!("agg:{}", layout_sort_key(db, *layout))
        }
        RuntimeClass::Ref {
            pointee,
            kind,
            view,
        } => format!(
            "ref:{}:{}:{}",
            ref_kind_sort_key(db, kind),
            ref_view_sort_key(db, view),
            runtime_class_sort_key(db, pointee)
        ),
        RuntimeClass::RawAddr { space, target } => format!(
            "raw:{}:{}",
            address_space_sort_key(*space),
            target
                .map(|layout| layout_sort_key(db, layout))
                .unwrap_or_default()
        ),
    }
}

fn scalar_class_sort_key<'db>(db: &'db dyn MirDb, class: &ScalarClass<'db>) -> String {
    format!(
        "{}:{}",
        scalar_repr_sort_key(class.repr),
        scalar_role_sort_key(db, &class.role)
    )
}

fn scalar_repr_sort_key(repr: ScalarRepr) -> String {
    match repr {
        ScalarRepr::Bool => "bool".to_string(),
        ScalarRepr::Int { bits, signed } => format!("int:{bits}:{signed}"),
        ScalarRepr::FixedBytes { len } => format!("bytes:{len}"),
        ScalarRepr::Address { bits } => format!("address:{bits}"),
    }
}

fn scalar_role_sort_key<'db>(db: &'db dyn MirDb, role: &ScalarRole<'db>) -> String {
    match role {
        ScalarRole::Plain => "plain".to_string(),
        ScalarRole::EnumTag { enum_layout } => {
            format!("enum_tag:{}", layout_sort_key(db, *enum_layout))
        }
    }
}

fn ref_kind_sort_key<'db>(db: &'db dyn MirDb, kind: &RefKind<'db>) -> String {
    match kind {
        RefKind::Const => "const".to_string(),
        RefKind::Object => "object".to_string(),
        RefKind::Provider { provider_ty, space } => {
            format!(
                "provider:{}:{}",
                type_identity(db, *provider_ty),
                address_space_sort_key(*space)
            )
        }
    }
}

fn ref_view_sort_key<'db>(db: &'db dyn MirDb, view: &RefView<'db>) -> String {
    match view {
        RefView::Whole => "whole".to_string(),
        RefView::EnumVariant(variant) => format!(
            "variant:{}:{}",
            layout_sort_key(db, variant.enum_layout),
            variant.index
        ),
    }
}

fn target_root_provider_materialization_sort_key<'db>(
    db: &'db dyn MirDb,
    materialization: TargetRootProviderMaterialization<'db>,
) -> String {
    match materialization {
        TargetRootProviderMaterialization::MemoryObject { layout } => {
            format!("memory_object:{}", layout_sort_key(db, layout))
        }
        TargetRootProviderMaterialization::MemoryRawAddr { layout } => {
            format!("memory_raw_addr:{}", layout_sort_key(db, layout))
        }
    }
}

fn layout_sort_key<'db>(db: &'db dyn MirDb, layout: LayoutId<'db>) -> String {
    layout_sort_key_query(db, layout)
}

#[salsa::tracked]
fn layout_sort_key_query<'db>(db: &'db dyn MirDb, layout: LayoutId<'db>) -> String {
    match layout.key(db) {
        LayoutKey::Struct(layout) => format!(
            "struct:{}:[{}]",
            type_identity(db, layout.source_ty),
            layout
                .fields
                .iter()
                .map(|field| runtime_class_sort_key(db, field))
                .collect::<Vec<_>>()
                .join(",")
        ),
        LayoutKey::Array(layout) => format!(
            "array:{}:{}:{}",
            type_identity(db, layout.source_ty),
            runtime_class_sort_key(db, &layout.elem),
            layout.len
        ),
        LayoutKey::Enum(layout) => format!(
            "enum:{}:[{}]",
            type_identity(db, layout.source_ty),
            layout
                .variants
                .iter()
                .map(|variant| format!(
                    "{}:[{}]",
                    variant.name,
                    variant
                        .fields
                        .iter()
                        .map(|field| runtime_class_sort_key(db, field))
                        .collect::<Vec<_>>()
                        .join(",")
                ))
                .collect::<Vec<_>>()
                .join(",")
        ),
    }
}

fn address_space_sort_key(space: AddressSpaceKind) -> &'static str {
    match space {
        AddressSpaceKind::Memory => "memory",
        AddressSpaceKind::Storage => "storage",
        AddressSpaceKind::Transient => "transient",
        AddressSpaceKind::Calldata => "calldata",
    }
}

fn runtime_instance_symbol_base<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> String {
    match instance.key(db).source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            symbol_base_for_semantic_instance(db, semantic)
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            symbol_base_for_runtime_instance(db, &synthetic.spec(db))
        }
    }
}

fn wrap_runtime_lowering_error<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    err: LowerError,
) -> LowerError {
    match err {
        LowerError::Unsupported(message) => LowerError::Unsupported(format!(
            "MIR lowering failed: unsupported while lowering `{}`: {message}",
            runtime_instance_symbol_base(db, instance)
        )),
    }
}

fn symbol_base_for_semantic_instance<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> String {
    let owner = semantic.key(db).owner(db);
    match owner {
        BodyOwner::Func(func) => func
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "__anon".to_string()),
        BodyOwner::ContractInit { contract } => format!(
            "__{}_init",
            contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "contract".to_string())
        ),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "__{}_recv_{}_{}",
            contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "contract".to_string()),
            recv_idx,
            arm_idx
        ),
        BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => "__const".to_string(),
    }
}

fn symbol_base_for_runtime_instance<'db>(
    db: &'db dyn MirDb,
    spec: &RuntimeSyntheticSpec<'db>,
) -> String {
    match spec {
        RuntimeSyntheticSpec::MainRoot { .. } => "main_root".to_string(),
        RuntimeSyntheticSpec::TestRoot { name, .. } => {
            format!("test_root_{}", sanitize_symbol(name))
        }
        RuntimeSyntheticSpec::ManualContractRoot { func, .. } => {
            let (contract_name, section) = match func.manual_contract_root_attr(db) {
                Some(ManualContractRootAttr::Init { contract_name }) => {
                    (contract_name.data(db), ManualContractSection::Init)
                }
                Some(ManualContractRootAttr::Runtime { contract_name }) => {
                    (contract_name.data(db), ManualContractSection::Runtime)
                }
                Some(ManualContractRootAttr::Error(_)) | None => {
                    return "manual_contract_root".to_string();
                }
            };
            let section = match section {
                ManualContractSection::Init => "init",
                ManualContractSection::Runtime => "runtime",
            };
            format!(
                "manual_contract_{section}_root_{}",
                sanitize_symbol(contract_name)
            )
        }
        RuntimeSyntheticSpec::ContractInitAbi { plan } => {
            format!("contract_init_abi_{}", contract_name(db, plan.contract))
        }
        RuntimeSyntheticSpec::ContractRecvAbi { plan } => format!(
            "contract_recv_abi_{}_{}",
            contract_name(db, plan.contract),
            optional_selector_key(db, plan.selector),
        ),
        RuntimeSyntheticSpec::ContractInitRoot { contract, .. } => {
            format!("contract_init_root_{}", contract_name(db, *contract))
        }
        RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. } => {
            format!("contract_runtime_root_{}", contract_name(db, *contract))
        }
        RuntimeSyntheticSpec::CodeRegionRoot { symbol, .. } => {
            format!("code_region_root_{}", sanitize_symbol(symbol))
        }
    }
}

fn inline_hint_for_semantic<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> RuntimeInlineHint {
    match semantic.key(db).owner(db) {
        BodyOwner::Func(func) => match func.inline_hint(db) {
            Some(InlineHint::Hint) => RuntimeInlineHint::Hint,
            Some(InlineHint::Always) => RuntimeInlineHint::Always,
            Some(InlineHint::Never) => RuntimeInlineHint::Never,
            None => RuntimeInlineHint::Auto,
        },
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. } => RuntimeInlineHint::Auto,
    }
}

fn collect_const_regions<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
) -> Vec<ConstRegionId<'db>> {
    let mut seen = FxHashSet::default();
    let mut regions = Vec::new();
    let mut instances = graph.nodes.keys().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| runtime_instance_sort_key(db, *instance));
    for instance in instances {
        for region in graph
            .nodes
            .get(&instance)
            .expect("runtime graph should contain every materialized instance")
            .referenced_const_regions
            .iter()
            .copied()
        {
            if seen.insert(region) {
                regions.push(region);
            }
        }
    }
    regions
}

fn contract_name<'db>(db: &'db dyn MirDb, contract: hir::hir_def::Contract<'db>) -> String {
    contract
        .name(db)
        .to_opt()
        .map(|name| sanitize_symbol(name.data(db)))
        .unwrap_or_else(|| "contract".to_string())
}

fn sanitize_symbol(value: &str) -> String {
    value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

fn sanitize_object_name(value: &str) -> String {
    let sanitized = sanitize_symbol(value);
    if sanitized.is_empty() {
        "object".to_string()
    } else {
        sanitized
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use url::Url;

    use super::*;

    fn recv_wrapper_plan<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        selector_sig: &str,
    ) -> ContractRecvAbiPlan<'db> {
        let contract = top_mod
            .all_contracts(db)
            .first()
            .copied()
            .expect("fixture should define a contract");
        let abi_ty = sol_abi_ty(db, contract.scope()).expect("Sol ABI type");
        let recv = hir::semantic::RecvView::new(db, contract, 0);
        let arm = recv
            .arms(db)
            .find(|arm| {
                arm.abi_info(db, abi_ty).selector_signature.as_deref() == Some(selector_sig)
            })
            .unwrap_or_else(|| panic!("missing recv arm `{selector_sig}`"));
        let abi = contract_abi_plan(db, contract.scope(), abi_ty).expect("Sol ABI plan");
        let (_, wrapper) = contract_recv_wrapper(db, arm, abi).expect("recv wrapper");
        let RuntimeInstanceSource::Synthetic(synthetic) = wrapper.key(db).source(db) else {
            panic!("recv wrapper should be synthetic");
        };
        match synthetic.spec(db) {
            RuntimeSyntheticSpec::ContractRecvAbi { plan } => plan.clone(),
            other => panic!("expected recv wrapper synthetic spec, got {other:?}"),
        }
    }

    fn with_test_runtime_package<T>(
        file_name: &str,
        source: &str,
        filter: Option<&str>,
        f: impl for<'db> FnOnce(&'db DriverDataBase, RuntimePackage<'db>) -> T,
    ) -> T {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{file_name}")).unwrap();
        let file = db
            .workspace()
            .touch(&mut db, file_url, Some(source.to_string()));
        let top_mod = db.top_mod(file);
        let package =
            build_test_runtime_package(&db, top_mod, filter).expect("test package should build");
        f(&db, package)
    }

    fn package_object_names(db: &DriverDataBase, package: RuntimePackage<'_>) -> Vec<String> {
        let mut names = package
            .objects(db)
            .into_iter()
            .map(|object| object.name(db).clone())
            .collect::<Vec<_>>();
        names.sort();
        names
    }

    fn package_root_object_names(db: &DriverDataBase, package: RuntimePackage<'_>) -> Vec<String> {
        let mut names = package
            .root_objects(db)
            .into_iter()
            .map(|object| object.name(db).clone())
            .collect::<Vec<_>>();
        names.sort();
        names
    }

    #[test]
    fn filtered_test_package_excludes_unreferenced_contract_roots() {
        with_test_runtime_package(
            "filtered_test_package_excludes_unreferenced_contract_roots.fe",
            r#"
use std::evm::assert

pub contract Unused {}

#[test]
fn selected() {
    assert(true)
}

#[test]
fn ignored() {
    assert(true)
}
"#,
            Some("selected"),
            |db, package| {
                assert_eq!(package_root_object_names(db, package), vec!["selected"]);
                assert_eq!(package_object_names(db, package), vec!["selected"]);
            },
        );
    }

    #[test]
    fn filtered_test_package_discovers_create2_contract_dependency() {
        with_test_runtime_package(
            "filtered_test_package_discovers_create2_contract_dependency.fe",
            r#"
use std::abi::sol
use std::evm::{Evm, assert}

pub msg ChildMsg {
    #[selector = sol("get()")]
    Get -> u256,
}

pub contract Child {
    recv ChildMsg {
        Get -> u256 {
            1
        }
    }
}

pub contract Unused {}

#[test]
fn selected() uses (evm: mut Evm) {
    let addr = evm.create2<Child>(value: 0, args: (), salt: 1)
    assert(addr.inner != 0)
}
"#,
            Some("selected"),
            |db, package| {
                let object_names = package_object_names(db, package);

                assert_eq!(package_root_object_names(db, package), vec!["selected"]);
                assert!(
                    object_names.contains(&"Child".to_string()),
                    "selected test package should discover create2 contract dependency: {object_names:?}"
                );
                assert!(
                    !object_names.contains(&"Unused".to_string()),
                    "selected test package should not include unreferenced contracts: {object_names:?}"
                );
            },
        );
    }

    #[test]
    fn filtered_test_package_without_matches_is_empty() {
        with_test_runtime_package(
            "filtered_test_package_without_matches_is_empty.fe",
            r#"
pub contract Unused {}

#[test]
fn selected() {}
"#,
            Some("missing"),
            |db, package| {
                assert!(package.root_objects(db).is_empty());
                assert!(package.objects(db).is_empty());
            },
        );
    }

    #[test]
    fn contract_recv_wrapper_projects_only_runtime_visible_fields_in_runtime_order() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///contract_recv_wrapper_projects_visible_fields.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
use std::abi::sol

msg DecodeMsg {
    #[selector = sol("raw(uint256)")]
    Raw { value: u256 } -> u256,
    #[selector = sol("swap(uint64,uint64)")]
    Swap { a: u64, b: u64 } -> u64,
}

pub contract DecodeHarness {
    recv DecodeMsg {
        Raw { value: _ } -> u256 { 0 }
        Swap { b, a } -> u64 { a }
    }
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

        let raw_plan = recv_wrapper_plan(&db, top_mod, "raw(uint256)");
        let RuntimeInputPlan::DecodeCalldataPayload {
            projected_fields, ..
        } = raw_plan.input
        else {
            panic!("raw(uint256) should decode calldata payload");
        };
        assert!(
            projected_fields.is_empty(),
            "ignored recv arm fields must not be forwarded to the runtime callee: {projected_fields:?}"
        );

        let swap_plan = recv_wrapper_plan(&db, top_mod, "swap(uint64,uint64)");
        let RuntimeInputPlan::DecodeCalldataPayload {
            projected_fields, ..
        } = swap_plan.input
        else {
            panic!("swap(uint64,uint64) should decode calldata payload");
        };
        assert_eq!(
            projected_fields.as_ref(),
            &[1, 0],
            "recv wrapper must forward decoded fields in runtime-visible binding order, not tuple order"
        );
    }

    #[test]
    fn contract_init_wrapper_is_synthesized_for_no_init_contracts() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///contract_init_wrapper_is_synthesized.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
pub contract NoInitBox {}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let contract = top_mod
            .all_contracts(&db)
            .first()
            .copied()
            .expect("fixture should define a contract");

        let init_abi = contract_init_abi(&db, contract).expect("init abi wrapper");
        let RuntimeInstanceSource::Synthetic(synthetic) = init_abi.key(&db).source(&db) else {
            panic!("init abi should be synthetic");
        };
        let RuntimeSyntheticSpec::ContractInitAbi { plan } = synthetic.spec(&db) else {
            panic!("expected synthetic contract init abi");
        };
        assert!(
            !plan.payable,
            "implicit constructor wrapper must reject deployment value"
        );
        assert!(
            plan.user_init.is_none(),
            "implicit constructor wrapper should not call a user init"
        );
        assert!(
            plan.entry_effect_args.is_empty(),
            "implicit constructor wrapper should not synthesize owner effect args"
        );
        assert!(
            matches!(plan.init_args, InitArgsPlan::None),
            "implicit constructor wrapper should not decode init args"
        );

        let root = contract_init_root(&db, contract).expect("init root");
        let RuntimeInstanceSource::Synthetic(synthetic) = root.key(&db).source(&db) else {
            panic!("init root should be synthetic");
        };
        let RuntimeSyntheticSpec::ContractInitRoot {
            init_abi: root_init_abi,
            ..
        } = synthetic.spec(&db)
        else {
            panic!("expected synthetic contract init root");
        };
        assert_eq!(
            root_init_abi, init_abi,
            "contract init root should always call the synthesized init abi wrapper"
        );
    }
}
