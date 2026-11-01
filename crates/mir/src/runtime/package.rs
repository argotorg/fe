use common::{compilation::Target, indexmap::IndexMap};
use hir::analysis::ty::AddressSpace;
use hir::semantic::{RecvArmAbiInfo, RecvArmView};
use hir::{
    analysis::{
        semantic::{
            ManualContractSection, RootSemanticInstanceError, SemanticInstance,
            generated_callee_key, root_semantic_instance_key, same_owner_effect_binding,
        },
        ty::{
            const_ty::ConstTyData,
            corelib::{resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path},
            trait_def::{MethodArgMapError, TraitInstId},
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, EffectParamSite, LocalBinding},
            ty_def::{TyData, TyId},
        },
    },
    hir_def::{
        Contract, Func, HirIngot, IdentId, InlineHint, ItemKind, ManualContractRootAttr,
        TopLevelMod, params::FuncParamMode,
    },
};
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
        RuntimeVisibleBindingPlan, runtime_effect_binding_plan, runtime_param_class,
        runtime_visible_binding_class,
    },
    runtime::lower::interface::runtime_visible_binding_plans,
    runtime::lower::type_info::{
        RuntimeTypeEnv, provider_class_for_target_in_env, top_level_class_for_ty_in_env,
        validate_runtime_array_extents_in_env,
    },
    runtime::object_name::{ObjectId, assign_object_names},
    runtime::root_effects::{
        EntryEffectContext, entry_semantic_args_plan, target_root_provider_materialization,
    },
    runtime::stable_key::{
        IdentityMode, closure_symbol_component, item_identity, semantic_instance_identity_in_mode,
        stable_identity_fingerprint, type_identity,
    },
    runtime::{
        ConstRegionId, ContractInitAbiPlan, ContractRecvAbiPlan, DispatchArm, DispatchDefault,
        EntryEffectArgPlan, EntrySemanticArgsPlan, InitArgsPlan, LayoutId, LayoutKey, RawPointeeId,
        RefKind, RefView, ResolvedCodeRegion, RuntimeClass, RuntimeCodeRegion,
        RuntimeCodeRegionKey, RuntimeFunction, RuntimeFunctionOwner, RuntimeInlineHint,
        RuntimeInputPlan, RuntimeInterfaceSignature, RuntimeLinkage, RuntimeObject, RuntimePackage,
        RuntimePackagePlan, RuntimeReturnPlan, RuntimeSection, RuntimeSectionName,
        RuntimeSectionRef, RuntimeSyntheticSpec, ScalarClass, ScalarRepr, ScalarRole,
        TargetRootProviderBinding, TargetRootProviderMaterialization,
    },
    verify::verify_runtime_package,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
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

type ObjectSections<'db> = Vec<(RuntimeSectionName, RuntimeInstance<'db>)>;

/// An object to build: what it is built from, its sections, and the roots that
/// keep those sections alive.
type ObjectSpecWithRoots<'db> = (
    ObjectId<'db>,
    ObjectSections<'db>,
    Vec<RuntimeInstance<'db>>,
);

/// An object to build, named once all objects of the package are known.
type ObjectSpec<'db> = (ObjectId<'db>, ObjectSections<'db>);

#[derive(Debug, Clone)]
struct RuntimeGraphNode<'db> {
    direct_callees: Vec<RuntimeInstance<'db>>,
    referenced_const_regions: Vec<ConstRegionId<'db>>,
    referenced_code_regions: Vec<RuntimeCodeRegion<'db>>,
}

struct RuntimeGraph<'db> {
    // Insertion-ordered: keys are Salsa tracked IDs whose hash order varies
    // across runs, which would make `_0`/`_1` suffix assignment for
    // content-identical duplicates non-deterministic.
    nodes: IndexMap<RuntimeInstance<'db>, RuntimeGraphNode<'db>>,
    object_specs: Vec<ObjectSpec<'db>>,
}

struct RuntimeGraphBuilder<'db> {
    db: &'db dyn MirDb,
    queue: Vec<RuntimeInstance<'db>>,
    queued: FxHashSet<RuntimeInstance<'db>>,
    nodes: IndexMap<RuntimeInstance<'db>, RuntimeGraphNode<'db>>,
    object_specs: Vec<ObjectSpec<'db>>,
    discovered_contract_specs: Vec<ObjectSpec<'db>>,
    materialized_contracts: FxHashSet<Contract<'db>>,
    materialized_objects: FxHashSet<ObjectId<'db>>,
}

impl<'db> RuntimeGraphBuilder<'db> {
    fn new(
        db: &'db dyn MirDb,
        roots: Vec<RuntimeInstance<'db>>,
        object_specs: Vec<ObjectSpec<'db>>,
    ) -> Self {
        let materialized_contracts = materialized_contracts_for_roots(db, &roots);
        let materialized_objects = object_specs
            .iter()
            .map(|(id, _)| id.clone())
            .collect::<FxHashSet<_>>();
        let mut builder = Self {
            db,
            queue: Vec::new(),
            queued: FxHashSet::default(),
            nodes: IndexMap::new(),
            object_specs,
            discovered_contract_specs: Vec::new(),
            materialized_contracts,
            materialized_objects,
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

            if instance.is_external_declaration(self.db) {
                self.nodes.insert(
                    instance,
                    RuntimeGraphNode {
                        direct_callees: Vec::new(),
                        referenced_const_regions: Vec::new(),
                        referenced_code_regions: Vec::new(),
                    },
                );
                continue;
            }

            let lowered = runtime_instance_lowered_body(self.db, instance)
                .map_err(|err| wrap_runtime_lowering_error(self.db, instance, err))?;
            let direct_callees = lowered
                .direct_callees(self.db)
                .iter()
                .map(|edge| edge.callee)
                .collect::<Vec<_>>();
            let referenced_const_regions = lowered.referenced_const_regions(self.db);
            let referenced_code_regions = lowered.referenced_code_regions(self.db);
            for callee in direct_callees.iter().copied() {
                self.enqueue(callee);
            }
            self.process_referenced_regions(referenced_code_regions)?;
            self.nodes.insert(
                instance,
                RuntimeGraphNode {
                    direct_callees,
                    referenced_const_regions: referenced_const_regions.clone(),
                    referenced_code_regions: referenced_code_regions.clone(),
                },
            );
        }

        let db = self.db;
        self.discovered_contract_specs
            .sort_by_cached_key(|(id, _)| id.sort_key(db));
        self.object_specs.extend(self.discovered_contract_specs);
        Ok(RuntimeGraph {
            nodes: self.nodes,
            object_specs: self.object_specs,
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
        let mut referenced_contracts = Vec::new();
        let mut referenced_manual_roots = Vec::new();
        for region in regions.iter().copied() {
            match region.key(self.db) {
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

        referenced_contracts
            .sort_by_cached_key(|contract| ObjectId::Contract(*contract).sort_key(self.db));
        for contract in referenced_contracts {
            let (id, sections, section_roots) = contract_object_spec(self.db, contract)?;
            if !self.materialized_objects.insert(id.clone()) {
                continue;
            }
            self.discovered_contract_specs.push((id, sections));
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
            let Some((id, sections, section_roots)) =
                manual_contract_object_for_root(self.db, func)?
            else {
                continue;
            };
            if !self.materialized_objects.insert(id.clone()) {
                continue;
            }
            self.discovered_contract_specs.push((id, sections));
            for root in section_roots {
                self.enqueue(root);
            }
        }
        Ok(())
    }
}

/// Build the runtime package of a single-module build: its contracts, or the
/// executable its `main` entry makes, if any.
pub fn build_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    match build_ingot_module_runtime_package(db, top_mod)? {
        Some(package) => Ok(package),
        None => Ok(RuntimePackage::new(
            db,
            top_mod,
            Vec::new(),
            RuntimePackagePlan::new(db, Vec::new(), Vec::new(), Vec::new(), Vec::new(), None),
        )),
    }
}

/// Build the executable `main` object entered at `main`, the module's entry,
/// with the target's root effect.
fn build_main_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    main: Func<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let semantic = root_semantic_instance_key(db, BodyOwner::Func(main, None))
        .map(|key| SemanticInstance::new(db, key))
        .map_err(|err| {
            let name = func_display_name(db, main);
            LowerError::Unsupported(format_root_semantic_instance_rejection(db, &name, &err))
        })?;
    let entry_args = entry_semantic_args_plan(
        db,
        EntryEffectContext::StandaloneFunc { func: main },
        semantic,
    )?;
    let instance = runtime_instance_for_semantic(db, semantic);
    let root = synthetic_instance(
        db,
        RuntimeSyntheticSpec::MainRoot {
            callee: instance,
            entry_args,
        },
        Vec::new(),
    );
    let package = build_non_contract_package(
        db,
        top_mod,
        vec![instance, root],
        vec![(ObjectId::Main, RuntimeSectionName::Main, root)],
    )?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

/// Build the runtime package contributed by `top_mod` as part of an *ingot*
/// build, or `None` when the module contributes nothing deployable.
///
/// Contracts (and manual contract roots) are deployable wherever they live.
/// The only other artifact is the executable of the ingot's entry, the root
/// module's `pub fn main()`. Other modules' free functions are not entries:
/// they are lowered as dependencies of what calls them.
pub fn build_ingot_module_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<Option<RuntimePackage<'db>>, LowerError> {
    if !top_mod.all_contracts(db).is_empty()
        || !discover_manual_contract_roots(db, top_mod)?.is_empty()
    {
        return Ok(Some(build_contract_package(db, top_mod)?));
    }
    top_mod
        .main_entry(db)
        .map(|main| build_main_package(db, top_mod, main))
        .transpose()
}

pub fn build_test_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    filter: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    build_test_package(db, top_mod, &collect_test_funcs(db, &[top_mod], filter))
}

/// Build one reachable runtime graph for every selected native test in a suite.
/// Object identities remain distinct when source modules use the same test name.
pub fn build_native_test_runtime_package<'db>(
    db: &'db dyn MirDb,
    top_mods: &[TopLevelMod<'db>],
    filter: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    require_target(db, Target::Native)?;
    let &top_mod = top_mods.first().ok_or_else(|| {
        LowerError::Unsupported("test runtime package needs a source module".into())
    })?;
    build_test_package(db, top_mod, &collect_test_funcs(db, top_mods, filter))
}

/// Rejects building a package for another machine than the compilation's:
/// layouts and destruction follow the compilation target.
fn require_target(db: &dyn MirDb, target: Target) -> Result<(), LowerError> {
    let actual = db.compilation_settings().target(db);
    if actual == target {
        Ok(())
    } else {
        Err(LowerError::Unsupported(format!(
            "a {target:?} package needs the {target:?} compilation target, not {actual:?}"
        )))
    }
}

/// A function marked `#[test]` that a run selected, with the name it is
/// reported under.
#[derive(Clone, Debug)]
pub struct TestFunc<'db> {
    pub func: Func<'db>,
    pub name: String,
}

/// The `#[test]` functions of `top_mods` whose names contain `filter`, in the
/// order of the modules and of the functions in each.
pub fn collect_test_funcs<'db>(
    db: &'db dyn MirDb,
    top_mods: &[TopLevelMod<'db>],
    filter: Option<&str>,
) -> Vec<TestFunc<'db>> {
    let mut tests = Vec::new();
    for &module in top_mods {
        for &func in module.all_funcs(db) {
            if func.top_mod(db) != module || !func.is_test(db) {
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
            tests.push(TestFunc { func, name });
        }
    }
    tests
}

/// Build the package of one test of a WebAssembly test run: a module with the
/// test and what it reaches, and nothing else.
pub fn build_wasm_test_runtime_package<'db>(
    db: &'db dyn MirDb,
    test: &TestFunc<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    require_target(db, Target::Wasm)?;
    build_test_package(db, test.func.top_mod(db), std::slice::from_ref(test))
}

fn build_test_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    tests: &[TestFunc<'db>],
) -> Result<RuntimePackage<'db>, LowerError> {
    let mut roots = Vec::new();
    let mut objects = Vec::new();
    for &TestFunc { func, ref name } in tests {
        let semantic = semantic_instance_for_root_owner(db, BodyOwner::Func(func, None))?;
        let entry_args =
            entry_semantic_args_plan(db, EntryEffectContext::TestFunc { func }, semantic)?;
        let runtime_root = runtime_instance_for_semantic(db, semantic);
        let root = synthetic_instance(
            db,
            RuntimeSyntheticSpec::TestRoot {
                name: name.clone(),
                callee: runtime_root,
                entry_args,
            },
            Vec::new(),
        );
        roots.push(root);
        objects.push((
            ObjectId::Test(func),
            vec![(RuntimeSectionName::Test(name.clone()), root)],
        ));
    }
    let package = build_sectioned_package(db, top_mod, roots, objects)?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

fn build_contract_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<RuntimePackage<'db>, LowerError> {
    require_target(db, Target::Evm)?;
    let mut roots = Vec::new();
    let mut objects = Vec::new();
    let contracts = top_mod.all_contracts(db);
    for &contract in contracts {
        let (id, sections, section_roots) = contract_object_spec(db, contract)?;
        roots.extend(section_roots);
        objects.push((id, sections));
    }
    for (id, sections, section_roots) in manual_contract_objects(db, top_mod)? {
        roots.extend(section_roots);
        objects.push((id, sections));
    }

    let package = build_sectioned_package(db, top_mod, roots, objects)?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid runtime package: {err:?}")))?;
    Ok(package)
}

/// The `#[export]` functions of `top_mod`'s ingot, which a native object
/// defines under their own names.
pub fn native_exports<'db>(db: &'db dyn MirDb, top_mod: TopLevelMod<'db>) -> Vec<Func<'db>> {
    top_mod
        .ingot(db)
        .all_funcs(db)
        .iter()
        .copied()
        .filter(|func| func.is_exported(db))
        .collect()
}

/// What a native package is built for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeOutputKind {
    /// An executable, entered at the public top-level `main`, that also
    /// defines the exported functions.
    Executable,
    /// A library of the exported functions.
    Library,
}

/// Build a target-neutral native package of one object, `main`, whose
/// sections are the exported functions and, for an executable, the public
/// top-level `main`. Each keeps its ordinary function signature.
pub fn build_native_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    kind: NativeOutputKind,
) -> Result<RuntimePackage<'db>, LowerError> {
    require_target(db, Target::Native)?;
    let root = |func| {
        semantic_instance_for_root_owner(db, BodyOwner::Func(func, None))
            .map(|semantic| runtime_instance_for_semantic(db, semantic))
    };
    let mut sections = Vec::new();
    // The function each exported symbol names. An executable's entry is
    // exported as `main`.
    let mut symbols = FxHashMap::default();
    if kind == NativeOutputKind::Executable {
        let main = top_mod.main_entry(db).ok_or_else(|| {
            LowerError::Unsupported(
                "native executable output requires `pub fn main() -> i32`".to_string(),
            )
        })?;
        symbols.insert("main".to_string(), main);
        sections.push((RuntimeSectionName::Main, root(main)?));
    }
    for func in native_exports(db, top_mod) {
        let symbol = func_display_name(db, func);
        match symbols.insert(symbol.clone(), func) {
            Some(other) if other == func => continue,
            Some(other) => {
                let path = |func: Func<'db>| func.scope().pretty_path(db).unwrap_or_default();
                return Err(LowerError::Unsupported(format!(
                    "`{}` and `{}` both define the native symbol `{symbol}`",
                    path(other),
                    path(func),
                )));
            }
            None => sections.push((RuntimeSectionName::Export(symbol), root(func)?)),
        }
    }
    if sections.is_empty() {
        return Err(LowerError::Unsupported(
            "native library output requires an `#[export]` function".to_string(),
        ));
    }
    let package = build_sectioned_package(
        db,
        top_mod,
        sections.iter().map(|(_, instance)| *instance).collect(),
        vec![(ObjectId::Main, sections)],
    )?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid native package: {err:?}")))?;
    Ok(package)
}

/// The name of the linear memory that every WebAssembly module exports. A
/// function cannot be exported under it.
pub const WASM_MEMORY_EXPORT: &str = "memory";

/// The names of the exports that give the host an allocator. A module has them
/// when an export takes text, and no function of the module may use them.
pub const WASM_ALLOCATOR_EXPORTS: [&str; 3] = ["alloc", "arena_mark", "arena_release"];

/// How a parameter of an export crosses the WebAssembly boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WasmParamShape {
    /// `bool` or an integer of at most 64 bits, by value.
    Scalar,
    /// `std::wasm::HostText`: an address and a length, two `i32` parameters.
    Text,
}

/// How the result of an export crosses the WebAssembly boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WasmResultShape {
    /// No result.
    Unit,
    /// `bool` or an integer of at most 64 bits, by value.
    Scalar,
    /// `std::wasm::Reply`: the tag in the low half and the payload in the high
    /// half of one `i64`.
    Reply,
}

/// A function the host can call by name.
#[derive(Clone, Debug)]
pub struct WasmExport<'db> {
    /// The name the host calls.
    pub name: String,
    pub func: Func<'db>,
    pub instance: RuntimeInstance<'db>,
    pub params: Vec<WasmParamShape>,
    pub result: WasmResultShape,
}

impl WasmExport<'_> {
    /// Whether the exported signature differs from the signature of the Fe
    /// function, so that a wrapper converts between them.
    pub fn needs_adapter(&self) -> bool {
        self.params.contains(&WasmParamShape::Text) || self.result == WasmResultShape::Reply
    }

    pub fn takes_text(&self) -> bool {
        self.params.contains(&WasmParamShape::Text)
    }
}

#[derive(Debug)]
pub struct WasmExportPackage<'db> {
    pub package: RuntimePackage<'db>,
    /// In name order.
    pub exports: Vec<WasmExport<'db>>,
}

/// Build the package of a WebAssembly module. Its exports are the `pub fn`
/// items of the root scope that are not generic, not `extern` and not tests.
/// Each is rooted with its own signature, which must consist of `bool` and
/// integers of at most 64 bits. An export that is not scalar is an error that
/// names it, and so is a module with no exports.
pub fn build_wasm_export_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<WasmExportPackage<'db>, LowerError> {
    require_target(db, Target::Wasm)?;
    let mut exports = Vec::new();
    for item in top_mod.children_non_nested(db) {
        let ItemKind::Func(func) = item else {
            continue;
        };
        if func.is_extern(db)
            || func.is_associated_func(db)
            || func.is_test(db)
            || !func.vis(db).is_pub()
        {
            continue;
        }
        if let Some(export) = wasm_export_for_func(db, func)? {
            exports.push(export);
        }
    }
    exports.sort_by_cached_key(|export| func_display_name(db, export.func));
    if exports.is_empty() {
        return Err(LowerError::Unsupported(
            "WebAssembly output needs at least one exported function: a non-generic `pub fn` in the root of the file".to_string(),
        ));
    }

    if exports.iter().any(WasmExport::takes_text) {
        for export in &exports {
            let name = func_display_name(db, export.func);
            if WASM_ALLOCATOR_EXPORTS.contains(&name.as_str()) {
                return Err(LowerError::Unsupported(format!(
                    "cannot export `{name}` to WebAssembly: the name is reserved for the allocator that a module with text parameters exports (`{}`)",
                    WASM_ALLOCATOR_EXPORTS.join("`, `")
                )));
            }
        }
    }

    let sections = exports
        .iter()
        .map(|export| {
            (
                RuntimeSectionName::Export(export.name.clone()),
                export.instance,
            )
        })
        .collect::<Vec<_>>();
    let package = build_sectioned_package(
        db,
        top_mod,
        sections.iter().map(|(_, instance)| *instance).collect(),
        vec![(ObjectId::Main, sections)],
    )?;
    verify_runtime_package(db, package)
        .map_err(|err| LowerError::Unsupported(format!("invalid WebAssembly package: {err:?}")))?;
    check_wasm_imports(db, &package)?;
    Ok(WasmExportPackage { package, exports })
}

/// The imports of a module are the `extern` functions that its exports reach.
/// Their signatures cross the boundary by value, so they follow the rule for
/// exports: `bool` and integers of at most 64 bits.
fn check_wasm_imports<'db>(
    db: &'db dyn MirDb,
    package: &RuntimePackage<'db>,
) -> Result<(), LowerError> {
    for function in package.functions(db) {
        let instance = function.instance(db);
        if function.linkage(db) != RuntimeLinkage::External || !instance.is_external_declaration(db)
        {
            continue;
        }
        let Some(func) =
            instance
                .key(db)
                .semantic(db)
                .and_then(|semantic| match semantic.key(db).owner(db) {
                    BodyOwner::Func(func, _) => Some(func),
                    _ => None,
                })
        else {
            continue;
        };
        let name = func_display_name(db, func);
        let signature = instance.interface_signature(db);
        let scalar = vec![WasmParamShape::Scalar; signature.params.len()];
        let result = if signature.ret.is_some() {
            WasmResultShape::Scalar
        } else {
            WasmResultShape::Unit
        };
        if let Some(reason) =
            wasm_signature_rejection(db, func, signature, WasmBoundary::Import, &scalar, result)
        {
            return Err(LowerError::Unsupported(format!(
                "cannot import `{name}` from the WebAssembly host: {reason}"
            )));
        }
    }
    Ok(())
}

/// Which side of the module a signature belongs to, for the message.
#[derive(Clone, Copy)]
enum WasmBoundary {
    Export,
    Import,
}

impl WasmBoundary {
    /// What the boundary accepts, for the end of a message.
    fn rule(self) -> String {
        match self {
            Self::Import => "imports take only `bool` and integers of at most 64 bits".to_string(),
            Self::Export => "exports take `bool` and integers of at most 64 bits, `std::wasm::HostText` parameters and a `std::wasm::Reply` result".to_string(),
        }
    }
}

/// The two types of `std::wasm` that cross the boundary besides numbers.
struct WasmBoundaryTypes<'db> {
    text: Option<TyId<'db>>,
    reply: Option<TyId<'db>>,
}

impl<'db> WasmBoundaryTypes<'db> {
    fn resolve(db: &'db dyn MirDb, func: Func<'db>) -> Self {
        let scope = func.scope();
        Self {
            text: resolve_lib_type_path(db, scope, "std::wasm::HostText"),
            reply: resolve_lib_type_path(db, scope, "std::wasm::Reply"),
        }
    }

    fn is_text(&self, ty: TyId<'db>) -> bool {
        self.text == Some(ty)
    }

    fn is_reply(&self, ty: TyId<'db>) -> bool {
        self.reply == Some(ty)
    }
}

/// Why the signature of `func` cannot cross the WebAssembly boundary, naming
/// the first parameter or the result that breaks the rule. On the export side
/// the parameters in `text_params` and a result that `reply_result` marks are
/// the shapes the rule allows besides numbers.
fn wasm_signature_rejection<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
    signature: &RuntimeInterfaceSignature<'db>,
    boundary: WasmBoundary,
    shapes: &[WasmParamShape],
    result: WasmResultShape,
) -> Option<String> {
    let hir_params = func.params(db).count();
    let types = WasmBoundaryTypes::resolve(db, func);
    // Fe holds a raw pointer in a 256-bit integer, which would be reported as
    // a wide integer. The host sees no Fe memory, so name it for what it is.
    let pointer_reason = || format!("is a pointer into Fe memory; {}", boundary.rule());
    for (idx, param) in signature.params.iter().enumerate() {
        let declared = (signature.params.len() == hir_params)
            .then(|| func.arg_ty(db, idx))
            .flatten()
            .map(|ty| ty.instantiate_identity());
        let rejection = if shapes.get(idx) == Some(&WasmParamShape::Text) {
            None
        } else if declared.is_some_and(|ty| ty.as_ptr(db).is_some()) {
            Some(pointer_reason())
        } else if declared.is_some_and(|ty| types.is_text(ty)) {
            Some(match boundary {
                WasmBoundary::Export => format!(
                    "is a `HostText` that is not passed by value (a `mut` parameter cannot be built from an address and a length); {}",
                    boundary.rule()
                ),
                WasmBoundary::Import => format!(
                    "is a `HostText`, which only an export can take (pass an address and a length as two `u32`); {}",
                    boundary.rule()
                ),
            })
        } else if declared.is_some_and(|ty| types.is_reply(ty)) {
            Some(format!(
                "is a `Reply`, which can only be the result of an export; {}",
                boundary.rule()
            ))
        } else {
            wasm_scalar_rejection(&param.class, boundary)
        };
        if let Some(reason) = rejection {
            let what = if signature.params.len() == hir_params {
                let param_name = func
                    .params(db)
                    .nth(idx)
                    .and_then(|view| view.name(db))
                    .map_or_else(|| format!("{idx}"), |ident| ident.data(db).to_string());
                let ty = func.arg_ty(db, idx).map_or_else(String::new, |ty| {
                    format!(": {}", ty.instantiate_identity().pretty_print(db))
                });
                format!("parameter `{param_name}{ty}`")
            } else {
                format!("parameter {idx}")
            };
            return Some(format!("{what} {reason}"));
        }
    }
    let ret_class = signature.ret.as_ref()?;
    let return_ty = func.return_ty(db);
    let reason = if result == WasmResultShape::Reply {
        return None;
    } else if return_ty.as_ptr(db).is_some() {
        pointer_reason()
    } else if types.is_text(return_ty) {
        format!(
            "is a `HostText`, which can only be a parameter of an export; {}",
            boundary.rule()
        )
    } else if types.is_reply(return_ty) {
        format!(
            "is a `Reply`, which only an export can return (an import returns the reply word as a `u64`); {}",
            boundary.rule()
        )
    } else {
        wasm_scalar_rejection(ret_class, boundary)?
    };
    Some(format!(
        "the result type `{}` {reason}",
        return_ty.pretty_print(db)
    ))
}

/// The shapes of the parameters and the result of an export, as far as the
/// types say. `HostText` and `Reply` are the types of `std::wasm`; a `HostText` must be
/// a plain view or owned parameter, which the wrapper can build.
fn wasm_export_shapes<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
    signature: &RuntimeInterfaceSignature<'db>,
) -> (Vec<WasmParamShape>, WasmResultShape) {
    let types = WasmBoundaryTypes::resolve(db, func);
    let by_hir = signature.params.len() == func.params(db).count();
    let params = signature
        .params
        .iter()
        .enumerate()
        .map(|(idx, param)| {
            let is_text = by_hir
                && func
                    .arg_ty(db, idx)
                    .is_some_and(|ty| types.is_text(ty.instantiate_identity()))
                && !func
                    .params(db)
                    .nth(idx)
                    .and_then(|view| view.hir(db))
                    .is_some_and(|param| param.mode == FuncParamMode::Mut);
            // An entry function reads a view of a struct through a reference
            // to memory; `own` takes the struct itself.
            let by_value = match &param.class {
                RuntimeClass::AggregateValue { .. } => true,
                RuntimeClass::Ref { pointee, .. } => {
                    matches!(**pointee, RuntimeClass::AggregateValue { .. })
                }
                _ => false,
            };
            if is_text && by_value {
                WasmParamShape::Text
            } else {
                WasmParamShape::Scalar
            }
        })
        .collect();
    let result = match &signature.ret {
        None => WasmResultShape::Unit,
        Some(RuntimeClass::AggregateValue { .. }) if types.is_reply(func.return_ty(db)) => {
            WasmResultShape::Reply
        }
        Some(_) => WasmResultShape::Scalar,
    };
    (params, result)
}

fn wasm_export_for_func<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Result<Option<WasmExport<'db>>, LowerError> {
    let name = func_display_name(db, func);
    let semantic = match root_semantic_instance_key(db, BodyOwner::Func(func, None)) {
        Ok(key) => SemanticInstance::new(db, key),
        // A generic function has no WebAssembly form until a caller picks
        // its arguments, so it is not an export.
        Err(RootSemanticInstanceError::UnsupportedGenericParam { offending_ty, .. })
            if !is_implicit_layout_const_param(db, offending_ty) =>
        {
            return Ok(None);
        }
        Err(err) => {
            return Err(LowerError::Unsupported(
                format_root_semantic_instance_rejection(db, &name, &err),
            ));
        }
    };
    if name == WASM_MEMORY_EXPORT {
        return Err(LowerError::Unsupported(format!(
            "cannot export `{name}` to WebAssembly: the name is reserved for the linear memory"
        )));
    }
    let entry_args =
        entry_semantic_args_plan(db, EntryEffectContext::StandaloneFunc { func }, semantic)
            .map_err(|err| {
                LowerError::Unsupported(format!("cannot export `{name}` to WebAssembly: {err}"))
            })?;
    // An export's effects come from the root effect, `std::wasm::Host`, which
    // a root provides before calling it.
    let callee = runtime_instance_for_semantic(db, semantic);
    let instance = if entry_args.effects.is_empty() {
        callee
    } else {
        synthetic_instance(
            db,
            RuntimeSyntheticSpec::ExportRoot { callee, entry_args },
            Vec::new(),
        )
    };
    let signature = instance.interface_signature(db);
    let (params, result) = wasm_export_shapes(db, func, signature);
    if let Some(reason) =
        wasm_signature_rejection(db, func, signature, WasmBoundary::Export, &params, result)
    {
        return Err(LowerError::Unsupported(format!(
            "cannot export `{name}` to WebAssembly: {reason}"
        )));
    }
    Ok(Some(WasmExport {
        name,
        func,
        instance,
        params,
        result,
    }))
}

/// Why `class` cannot cross the WebAssembly boundary by value, or `None` for
/// `bool` and the integers of at most 64 bits.
fn wasm_scalar_rejection(class: &RuntimeClass<'_>, boundary: WasmBoundary) -> Option<String> {
    let rule = boundary.rule();
    match class {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Bool,
            ..
        }) => None,
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int { bits, .. },
            ..
        }) if *bits <= 64 => None,
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int { bits, .. },
            ..
        }) => Some(format!("is a {bits}-bit integer; {rule}")),
        RuntimeClass::Scalar(_) => Some(format!("is a fixed-size byte value or address; {rule}")),
        RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => Some(format!(
            "is not a scalar (a struct, tuple, array, enum, reference or pointer); {rule}"
        )),
    }
}

fn manual_contract_objects<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
) -> Result<Vec<ObjectSpecWithRoots<'db>>, LowerError> {
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
        .filter_map(|contract| contract.name(db).to_opt().map(|name| name.data(db).clone()))
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
            (ObjectId::manual(top_mod, &contract_name), sections, roots)
        })
        .collect::<Vec<_>>();
    objects.sort_by_cached_key(|(id, _, _)| id.sort_key(db));
    Ok(objects)
}

fn manual_contract_object_for_root<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Result<Option<ObjectSpecWithRoots<'db>>, LowerError> {
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
    let id = ObjectId::manual(func.top_mod(db), contract_name);
    Ok(manual_contract_objects(db, func.top_mod(db))?
        .into_iter()
        .find(|(candidate, _, _)| *candidate == id))
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
    let semantic = semantic_instance_for_root_owner(db, BodyOwner::Func(func, None))?;
    let callee = runtime_instance_for_semantic(db, semantic);
    let entry_args = entry_semantic_args_plan(
        db,
        EntryEffectContext::ManualContractRoot { func },
        semantic,
    )?;
    Ok(synthetic_instance(
        db,
        RuntimeSyntheticSpec::ManualContractRoot {
            func,
            callee,
            entry_args,
        },
        Vec::new(),
    ))
}

fn contract_runtime_root<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let abi_ty = sol_abi_ty(db, contract.scope())?;
    let mut dispatch = Vec::new();
    let mut default = DispatchDefault::RevertEmpty;
    for arm in contract.recv_views(db).flat_map(|recv| recv.arms(db)) {
        let (abi_info, wrapper) = contract_recv_wrapper(db, arm, abi_ty)?;
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
        dispatch.push(DispatchArm { selector, wrapper });
    }
    dispatch.sort_by_key(|arm| arm.selector);
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
            entry_args: EntrySemanticArgsPlan {
                effects: Box::new([]),
            },
            init_args: InitArgsPlan::None,
        });
    };

    validate_runtime_array_extents_in_env(
        db,
        RuntimeTypeEnv::new(Some(contract.scope()), PredicateListId::empty_list(db)),
        contract.init_args_ty(db),
    )?;

    let semantic = semantic_instance_for_root_owner(db, BodyOwner::ContractInit { contract })?;
    let user_init = Some(runtime_instance_for_semantic(db, semantic));
    let entry_args = entry_semantic_args_plan(
        db,
        EntryEffectContext::HighLevelContract { contract },
        semantic,
    )?;
    let projected_fields = visible_init_arg_fields(db, semantic);
    let init_args = if contract.init_args_ty(db) == TyId::unit(db) {
        InitArgsPlan::None
    } else {
        InitArgsPlan::DecodeInitTail {
            tuple_ty: contract.init_args_ty(db),
            decode_fn: resolve_decode_instance(
                db,
                contract.scope(),
                contract.init_args_ty(db),
                memory_span_ty(db, contract.scope())?,
            )?,
            projected_fields,
        }
    };
    Ok(ContractInitAbiPlan {
        contract,
        payable: init.is_payable(db),
        user_init,
        entry_args,
        init_args,
    })
}

fn contract_recv_wrapper<'db>(
    db: &'db dyn MirDb,
    arm: RecvArmView<'db>,
    abi_ty: TyId<'db>,
) -> Result<(RecvArmAbiInfo<'db>, RuntimeInstance<'db>), LowerError> {
    let contract = arm.contract(db);
    let abi_info = arm.abi_info(db, abi_ty);
    let type_env = RuntimeTypeEnv::new(Some(contract.scope()), PredicateListId::empty_list(db));
    validate_runtime_array_extents_in_env(db, type_env, abi_info.args_ty)?;
    if let Some(ret_ty) = abi_info.ret_ty {
        validate_runtime_array_extents_in_env(db, type_env, ret_ty)?;
    }
    let recv = arm.recv(db);
    let owner = BodyOwner::ContractRecvArm {
        contract,
        recv_idx: recv.recv_idx(db),
        arm_idx: arm.arm_idx(db),
    };
    let semantic = semantic_instance_for_root_owner(db, owner)?;
    let user_recv = runtime_instance_for_semantic(db, semantic);
    let entry_args = entry_semantic_args_plan(
        db,
        EntryEffectContext::HighLevelContract { contract },
        semantic,
    )?;
    let projected_fields = visible_recv_arg_fields(db, semantic, arm);
    let host = if abi_info.args_ty != TyId::unit(db) || abi_info.ret_ty.is_some() {
        Some(contract_recv_host_binding(
            db,
            contract,
            recv.recv_idx(db),
            arm.arm_idx(db),
        )?)
    } else {
        None
    };
    let input = if abi_info.args_ty == TyId::unit(db) {
        RuntimeInputPlan::None
    } else {
        let host = host.clone().expect("non-unit recv args require a host");
        RuntimeInputPlan::DecodeHostPayload {
            msg_ty: abi_info.args_ty,
            decode_args_fn: resolve_decode_runtime_args_instance(
                db,
                contract.scope(),
                host.declared_ty,
                host.class.clone(),
                abi_info.args_ty,
            )?,
            host,
            projected_fields,
        }
    };
    let ret = if let Some(ret_ty) = abi_info.ret_ty {
        let host = host.expect("value-returning recv requires a host");
        let contract_host = resolve_core_trait(
            db,
            contract.scope(),
            &["contracts", "ContractHost"],
        )
        .ok_or_else(|| {
            LowerError::Unsupported("missing required core::contracts::ContractHost".to_string())
        })?;
        let host_inst = TraitInstId::new_simple(db, contract_host, vec![host.declared_ty]);
        let return_value = resolve_trait_runtime_instance(
            db,
            contract.scope(),
            host_inst,
            "return_value",
            vec![abi_ty, ret_ty],
        )?;
        RuntimeReturnPlan::Value {
            ty: ret_ty,
            host,
            return_value,
        }
    } else {
        RuntimeReturnPlan::Unit
    };
    let wrapper = synthetic_instance(
        db,
        RuntimeSyntheticSpec::ContractRecvAbi {
            plan: ContractRecvAbiPlan {
                contract,
                selector: abi_info.selector_value,
                payable: match arm.arm(db) {
                    Some(recv_arm) => recv_arm.is_payable(db),
                    None => false,
                },
                user_recv,
                entry_args,
                input,
                ret,
            },
        },
        Vec::new(),
    );
    Ok((abi_info, wrapper))
}

fn contract_recv_host_binding<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
    recv_idx: u32,
    arm_idx: u32,
) -> Result<TargetRootProviderBinding<'db>, LowerError> {
    let site = EffectParamSite::ContractRecvArm {
        contract,
        recv_idx,
        arm_idx,
    };
    let registration = hir::analysis::ty::registered_root_providers(db, site)
        .first()
        .ok_or_else(|| {
            LowerError::Unsupported(format!(
                "contract `{}` recv arm has no registered root provider",
                contract_name(db, contract)
            ))
        })?;
    let env = RuntimeTypeEnv::new(
        Some(contract.scope()),
        hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
    );
    let class = provider_class_for_target_in_env(
        db,
        env,
        Some(registration.provider_ty),
        AddressSpace::Memory,
    );
    let materialization = target_root_provider_materialization(db, &class).ok_or_else(|| {
        LowerError::Unsupported(format!(
            "contract `{}` recv root provider class `{:?}` has no supported entry materialization",
            contract_name(db, contract),
            class
        ))
    })?;
    Ok(TargetRootProviderBinding {
        declared_ty: registration.provider_ty,
        class,
        materialization,
    })
}

fn build_non_contract_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    roots: Vec<RuntimeInstance<'db>>,
    object_specs: Vec<(ObjectId<'db>, RuntimeSectionName, RuntimeInstance<'db>)>,
) -> Result<RuntimePackage<'db>, LowerError> {
    build_sectioned_package(
        db,
        top_mod,
        roots,
        object_specs
            .into_iter()
            .map(|(id, section, entry)| (id, vec![(section, entry)]))
            .collect(),
    )
}

fn build_sectioned_package<'db>(
    db: &'db dyn MirDb,
    top_mod: TopLevelMod<'db>,
    roots: Vec<RuntimeInstance<'db>>,
    object_specs: Vec<ObjectSpec<'db>>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let root_object_count = object_specs.len();
    let mut graph = RuntimeGraphBuilder::new(db, roots, object_specs).build()?;
    let functions = collect_runtime_functions(db, &graph);
    let functions_by_instance = functions
        .iter()
        .map(|function| (function.instance(db), *function))
        .collect::<FxHashMap<_, _>>();
    let const_regions = collect_const_regions(db, &graph);
    let mut reachable_cache = FxHashMap::default();

    // Objects are named once the contracts the roots reach are known, so that
    // no two objects of the package can share a name.
    let object_specs = std::mem::take(&mut graph.object_specs);
    let object_ids = object_specs
        .iter()
        .map(|(id, _)| id.clone())
        .collect::<Vec<_>>();
    let object_names = assign_object_names(db, top_mod.ingot(db), &object_ids);
    let mut objects = object_specs
        .into_iter()
        .zip(object_names)
        .map(|((_, sections), name)| {
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

    let code_regions = resolve_code_regions(db, &objects);
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
    // The roots come first; the contracts they reach follow.
    let root_objects: Vec<_> = objects[..root_object_count].to_vec();
    let primary_object = (root_object_count == 1).then(|| objects[0]);

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
                    object: object.name(db).clone(),
                    section: section.name.clone(),
                },
            );
            section
        })
        .collect();
    make_runtime_object(db, object.name(db).clone(), sections)
}

fn resolve_code_regions<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
) -> Vec<ResolvedCodeRegion<'db>> {
    let mut resolved = Vec::new();
    for object in objects {
        for section in object.sections(db) {
            let Some(region) = resolved_section_region(db, section) else {
                continue;
            };
            resolved.push(make_resolved_code_region(
                db,
                region,
                code_region_symbol(db, region, object.name(db)),
                RuntimeSectionRef::Local {
                    object: object.name(db).clone(),
                    section: section.name.clone(),
                },
                section.entry,
            ));
        }
    }

    resolved.sort_by_key(|region| region.symbol(db));
    resolved
}

fn resolved_section_region<'db>(
    db: &'db dyn MirDb,
    section: &RuntimeSection<'db>,
) -> Option<RuntimeCodeRegion<'db>> {
    match section.entry.owner(db) {
        RuntimeFunctionOwner::Synthetic(
            RuntimeSyntheticSpec::ContractInitRoot { contract, .. }
            | RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. },
        ) => match section.name {
            RuntimeSectionName::Init => Some(RuntimeCodeRegion::new(
                db,
                RuntimeCodeRegionKey::ContractInit {
                    contract: *contract,
                },
            )),
            RuntimeSectionName::Runtime => Some(RuntimeCodeRegion::new(
                db,
                RuntimeCodeRegionKey::ContractRuntime {
                    contract: *contract,
                },
            )),
            RuntimeSectionName::Main
            | RuntimeSectionName::Test(_)
            | RuntimeSectionName::Export(_) => None,
        },
        RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ManualContractRoot {
            func, ..
        }) => {
            let region = runtime_code_region_for_manual_root(db, *func)?;
            Some(region)
        }
        RuntimeFunctionOwner::Semantic(semantic) => {
            let BodyOwner::Func(func, _) = semantic.key(db).owner(db) else {
                return None;
            };
            runtime_code_region_for_manual_root(db, func)
        }
        RuntimeFunctionOwner::Synthetic(
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ExportRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. }
            | RuntimeSyntheticSpec::ContractRecvAbi { .. },
        ) => None,
    }
}

fn collect_region_embeds<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
    reachable: &FxHashSet<RuntimeInstance<'db>>,
    section_refs: &FxHashMap<RuntimeCodeRegion<'db>, RuntimeSectionRef>,
    current_section: RuntimeSectionRef,
) -> Vec<crate::runtime::RuntimeEmbed> {
    let current_object = current_section.object().to_string();
    let mut seen = FxHashSet::default();
    let mut embeds = Vec::new();
    let mut instances = reachable.iter().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| {
        (
            runtime_instance_sort_key(db, *instance),
            runtime_instance_symbol_key(db, *instance),
        )
    });
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
            let as_symbol = code_region_symbol(db, region, source.object());
            let order = match region.key(db) {
                RuntimeCodeRegionKey::ContractInit { contract } => {
                    (item_identity(db, contract.into()), 0)
                }
                RuntimeCodeRegionKey::ContractRuntime { contract } => {
                    (item_identity(db, contract.into()), 1)
                }
                RuntimeCodeRegionKey::ManualContractRoot { func } => {
                    (item_identity(db, func.into()), 0)
                }
            };
            let source = if source.object() == current_object {
                RuntimeSectionRef::Local {
                    object: source.object().to_string(),
                    section: source.section().clone(),
                }
            } else {
                RuntimeSectionRef::External {
                    object: source.object().to_string(),
                    section: source.section().clone(),
                }
            };
            embeds.push((order, crate::runtime::RuntimeEmbed { source, as_symbol }));
        }
    }
    // Embedded code is laid out in this order. Order it by what each region is
    // built from, so that object names, which another package may choose
    // differently for a dependency's contract, cannot change the bytes.
    embeds.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
    embeds.into_iter().map(|(_, embed)| embed).collect()
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
    instances.sort_by_cached_key(|instance| {
        (
            runtime_instance_sort_key(db, *instance),
            runtime_instance_symbol_key(db, *instance),
        )
    });
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
                | RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. } => Some(*contract),
                RuntimeSyntheticSpec::MainRoot { .. }
                | RuntimeSyntheticSpec::TestRoot { .. }
                | RuntimeSyntheticSpec::ExportRoot { .. }
                | RuntimeSyntheticSpec::ManualContractRoot { .. }
                | RuntimeSyntheticSpec::ContractInitAbi { .. }
                | RuntimeSyntheticSpec::ContractRecvAbi { .. } => None,
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
            "root semantic instance is not closed under synthesized root substitution: {err:?}"
        )),
    })?;
    Ok(SemanticInstance::new(db, key))
}

fn contract_object_spec<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
) -> Result<ObjectSpecWithRoots<'db>, LowerError> {
    let runtime_root = contract_runtime_root(db, contract)?;
    let init_root = contract_init_root(db, contract)?;
    Ok((
        ObjectId::Contract(contract),
        vec![
            (RuntimeSectionName::Init, init_root),
            (RuntimeSectionName::Runtime, runtime_root),
        ],
        vec![init_root, runtime_root],
    ))
}

fn func_display_name<'db>(db: &'db dyn MirDb, func: Func<'db>) -> String {
    func.name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string())
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
        RootSemanticInstanceError::UnclosedEffectEnv(_) => format!(
            "function `{func_name}` cannot be used as a standalone runtime root because its effect environment is not fully concrete"
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
    runtime_instance_for_semantic_with_visible_param_overrides(db, semantic, |_| None)
}

pub(crate) fn runtime_instance_for_semantic_with_visible_param_overrides<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    mut override_class: impl FnMut(&RuntimeVisibleBindingPlan<'db>) -> Option<RuntimeClass<'db>>,
) -> RuntimeInstance<'db> {
    let typed_body = semantic.key(db).typed_body(db);
    let owner = semantic.key(db).owner(db);
    if let BodyOwner::Func(func, _) = owner
        && func.body(db).is_none()
    {
        panic!(
            "bodyless semantic function leaked into runtime instance construction: func={func:?} key={:?}",
            semantic.key(db)
        );
    }
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let params: Vec<_> = runtime_visible_binding_plans(db, semantic)
        .iter()
        .map(|entry| {
            override_class(entry).unwrap_or_else(|| {
                runtime_class_for_visible_binding_entry(db, semantic, typed_body, env, entry)
            })
        })
        .collect();
    let key = RuntimeInstanceKey::new(db, RuntimeInstanceSource::Semantic(semantic), params);
    get_or_build_runtime_instance(db, key)
}

fn runtime_class_for_visible_binding_entry<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    env: RuntimeTypeEnv<'db>,
    entry: &RuntimeVisibleBindingPlan<'db>,
) -> RuntimeClass<'db> {
    if semantic
        .effect_bindings(db)
        .into_iter()
        .any(|binding| same_owner_effect_binding(binding, entry.binding))
    {
        return owner_effect_binding_class(db, semantic, entry.binding).unwrap_or_else(|| {
            panic!("runtime-visible owner effect binding has no runtime class: {entry:?}")
        });
    }
    if matches!(entry.binding, LocalBinding::Local { .. }) {
        return top_level_class_for_ty_in_env(db, env, entry.semantic_ty, AddressSpace::Memory)
            .unwrap_or_else(|| {
                panic!("runtime-visible recv arg binding has no top-level runtime class: {entry:?}")
            });
    }
    runtime_visible_binding_class(db, semantic, entry.binding)
        .map(|class| runtime_param_class(db, typed_body, entry.binding, env, class))
        .unwrap_or_else(|| panic!("runtime-visible typed binding has no runtime class: {entry:?}"))
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

fn resolve_decode_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    ty: TyId<'db>,
    input_ty: TyId<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let abi_ty = sol_abi_ty(db, scope)?;
    let decoder_ty = resolve_lib_type_path(db, scope, "std::abi::sol::SolDecoder")
        .ok_or_else(|| LowerError::Unsupported("missing std::abi::sol::SolDecoder".to_string()))?;
    let decode_trait = resolve_core_trait(db, scope, &["abi", "Decode"])
        .ok_or_else(|| LowerError::Unsupported("missing required core::abi::Decode".to_string()))?;
    let inst = TraitInstId::new_simple(db, decode_trait, vec![ty, abi_ty]);
    resolve_trait_runtime_instance(
        db,
        scope,
        inst,
        "decode_payload",
        vec![decoder_ty, input_ty],
    )
}

fn resolve_decode_runtime_args_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    host_ty: TyId<'db>,
    host_class: RuntimeClass<'db>,
    msg_ty: TyId<'db>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let func = resolve_lib_func_path(db, scope, "core::contracts::decode_runtime_args")
        .ok_or_else(|| {
            LowerError::Unsupported(
                "missing required core::contracts::decode_runtime_args".to_string(),
            )
        })?;
    let key = generated_callee_key(
        db,
        scope,
        PredicateListId::empty_list(db),
        func,
        None,
        &[host_ty, sol_abi_ty(db, scope)?, msg_ty],
    )
    .map_err(|error| generated_call_error(db, func, error))?;
    let semantic = SemanticInstance::new(db, key);
    Ok(runtime_instance_for_semantic_with_visible_param_overrides(
        db,
        semantic,
        |entry| {
            if matches!(entry.binding, LocalBinding::Param { idx: 0, .. }) {
                Some(host_class.clone())
            } else {
                None
            }
        },
    ))
}

/// The instance of `inst`'s `method` an EVM contract entry calls.
pub(crate) fn resolve_trait_runtime_instance<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    inst: TraitInstId<'db>,
    method: &str,
    own_args: Vec<TyId<'db>>,
) -> Result<RuntimeInstance<'db>, LowerError> {
    let name = IdentId::new(db, method.to_string());
    let func = inst
        .def(db)
        .method_defs(db)
        .get(&name)
        .copied()
        .ok_or_else(|| {
            LowerError::Unsupported(format!(
                "missing trait method `{method}` for runtime package"
            ))
        })?;
    let key = generated_callee_key(
        db,
        scope,
        PredicateListId::empty_list(db),
        func,
        Some(inst),
        &own_args,
    )
    .map_err(|error| generated_call_error(db, func, error))?;
    Ok(runtime_instance_for_semantic(
        db,
        SemanticInstance::new(db, key),
    ))
}

pub(crate) fn generated_call_error<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
    error: MethodArgMapError<'db>,
) -> LowerError {
    let name = func
        .name(db)
        .to_opt()
        .map_or("<unnamed>", |name| name.data(db));
    LowerError::Unsupported(format!(
        "failed to finalize generated call to `{name}`: {error:?}"
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
            LocalBinding::Local { .. }
            | LocalBinding::ParamPart { .. }
            | LocalBinding::EffectParam { .. } => None,
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
            LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. }
            | LocalBinding::EffectParam { .. } => None,
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn memory_span_ty<'db>(
    db: &'db dyn MirDb,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
) -> Result<TyId<'db>, LowerError> {
    resolve_lib_type_path(db, scope, "core::ptr::MemSpan")
        .ok_or_else(|| LowerError::Unsupported("missing core::ptr::MemSpan".to_string()))
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
    source: RuntimeSectionRef,
    root: RuntimeFunction<'db>,
) -> ResolvedCodeRegion<'db> {
    ResolvedCodeRegion::new(db, region, symbol, source, root)
}

const RUNTIME_SYMBOL_DISAMBIG_FINGERPRINT_LEN: usize = 4;

fn collect_runtime_functions<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
) -> Vec<RuntimeFunction<'db>> {
    let mut instances = graph.nodes.keys().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| {
        (
            runtime_instance_sort_key(db, *instance),
            runtime_instance_symbol_key(db, *instance),
        )
    });
    let instance_symbols = instances
        .into_iter()
        .map(|instance| {
            (
                instance,
                runtime_instance_display_name(db, instance),
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
        let fingerprint = stable_identity_fingerprint(stable_key);
        format!(
            "{base}_{}",
            &fingerprint[..RUNTIME_SYMBOL_DISAMBIG_FINGERPRINT_LEN]
        )
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
            if instance.is_external_declaration(db) {
                RuntimeLinkage::External
            } else {
                RuntimeLinkage::Private
            },
            inline_hint_for_semantic(db, semantic),
            RuntimeFunctionOwner::Semantic(semantic),
            referenced_const_regions,
        ),
        RuntimeInstanceSource::Synthetic(synthetic) => {
            let spec = synthetic.spec(db).clone();
            let inline_hint = match &spec {
                RuntimeSyntheticSpec::ContractInitAbi { .. } => RuntimeInlineHint::Always,
                // Let the backend budget receive-wrapper inlining. Forcing every
                // wrapper into the dispatcher inflates large contracts and their
                // shared memory footprint, including otherwise small call paths.
                _ => RuntimeInlineHint::Auto,
            };
            make_runtime_function(
                db,
                instance,
                symbol,
                RuntimeLinkage::Private,
                inline_hint,
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
    runtime_instance_symbol_key_query(db, instance).clone()
}

fn runtime_instance_sort_key<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> String {
    runtime_instance_sort_key_query(db, instance).clone()
}

#[salsa::tracked]
fn runtime_instance_symbol_key_query<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    runtime_instance_key_for_mode(db, instance, IdentityMode::Symbol)
}

#[salsa::tracked]
fn runtime_instance_sort_key_query<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    runtime_instance_key_for_mode(db, instance, IdentityMode::Sort)
}

/// Memoized per-mode entry point for recursive runtime-instance identities.
fn runtime_instance_key_in_mode<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    mode: IdentityMode,
) -> String {
    match mode {
        IdentityMode::Sort => runtime_instance_sort_key_query(db, instance).clone(),
        IdentityMode::Symbol => runtime_instance_symbol_key_query(db, instance).clone(),
    }
}

fn runtime_instance_key_for_mode<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
    mode: IdentityMode,
) -> String {
    let key = instance.key(db);
    let source = runtime_instance_source_key(db, key.source(db), mode);
    let params = key
        .params(db)
        .iter()
        .map(|param| runtime_class_sort_key(db, param))
        .collect::<Vec<_>>()
        .join(",");
    format!("{source}:params[{params}]")
}

fn runtime_instance_source_key<'db>(
    db: &'db dyn MirDb,
    source: RuntimeInstanceSource<'db>,
    mode: IdentityMode,
) -> String {
    match source {
        RuntimeInstanceSource::Semantic(semantic) => {
            format!(
                "semantic:{}",
                semantic_instance_identity_in_mode(db, semantic, mode)
            )
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_spec_key(db, synthetic.spec(db).clone(), mode)
        }
    }
}

fn runtime_synthetic_spec_key<'db>(
    db: &'db dyn MirDb,
    spec: RuntimeSyntheticSpec<'db>,
    mode: IdentityMode,
) -> String {
    let instance_key = |instance| runtime_instance_key_in_mode(db, instance, mode);
    match spec {
        RuntimeSyntheticSpec::MainRoot { callee, entry_args } => format!(
            "__synthetic:main_root:{}:{}",
            instance_key(callee),
            entry_semantic_args_sort_key(db, &entry_args)
        ),
        RuntimeSyntheticSpec::TestRoot {
            name,
            callee,
            entry_args,
        } => format!(
            "__synthetic:test_root:{name}:{}:{}",
            instance_key(callee),
            entry_semantic_args_sort_key(db, &entry_args)
        ),
        RuntimeSyntheticSpec::ExportRoot { callee, entry_args } => format!(
            "__synthetic:export_root:{}:{}",
            instance_key(callee),
            entry_semantic_args_sort_key(db, &entry_args)
        ),
        RuntimeSyntheticSpec::ManualContractRoot {
            func,
            callee,
            entry_args,
        } => format!(
            "__synthetic:manual_contract_root:{}:{}:{}",
            item_identity(db, func.into()),
            instance_key(callee),
            entry_semantic_args_sort_key(db, &entry_args)
        ),
        RuntimeSyntheticSpec::ContractInitAbi { plan } => format!(
            "__synthetic:contract_init_abi:{}:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            plan.payable,
            plan.user_init.map(instance_key).unwrap_or_default(),
            entry_semantic_args_sort_key(db, &plan.entry_args),
            init_args_plan_key(db, &plan.init_args, mode)
        ),
        RuntimeSyntheticSpec::ContractRecvAbi { plan } => format!(
            "__synthetic:contract_recv_abi:{}:{}:{}:{}:{}:{}:{}",
            item_identity(db, plan.contract.into()),
            plan.selector
                .map_or_else(|| "fallback".to_string(), |selector| selector.to_string()),
            plan.payable,
            instance_key(plan.user_recv),
            entry_semantic_args_sort_key(db, &plan.entry_args),
            runtime_input_plan_key(db, &plan.input, mode),
            runtime_return_plan_key(db, &plan.ret, mode),
        ),
        RuntimeSyntheticSpec::ContractInitRoot {
            contract,
            init_abi,
            runtime_region,
        } => format!(
            "__synthetic:contract_init_root:{}:{}:{}",
            item_identity(db, contract.into()),
            instance_key(init_abi),
            runtime_code_region_key(db, runtime_region)
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
                .map(|arm| format!("{}:{}", arm.selector, instance_key(arm.wrapper)))
                .collect::<Vec<_>>()
                .join(","),
            dispatch_default_key(db, &default, mode)
        ),
    }
}

fn entry_semantic_args_sort_key<'db>(
    db: &'db dyn MirDb,
    args: &EntrySemanticArgsPlan<'db>,
) -> String {
    format!("effects[{}]", entry_effect_args_sort_key(db, &args.effects))
}

fn entry_effect_args_sort_key<'db>(db: &'db dyn MirDb, args: &[EntryEffectArgPlan<'db>]) -> String {
    args.iter()
        .map(|arg| match arg {
            EntryEffectArgPlan::ContractField(binding) => format!(
                "field:{}:{}:{}:{}:{}:{}:{}",
                item_identity(db, binding.field.contract.into()),
                binding.field.index,
                binding.slot,
                type_identity(db, binding.declared_ty),
                runtime_class_sort_key(db, &binding.class),
                ref_kind_sort_key(db, &binding.kind),
                binding.init_immutable
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

fn projected_fields_sort_key(projected_fields: &[u32]) -> String {
    projected_fields
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(".")
}

fn init_args_plan_key<'db>(
    db: &'db dyn MirDb,
    plan: &InitArgsPlan<'db>,
    mode: IdentityMode,
) -> String {
    match plan {
        InitArgsPlan::None => "none".to_string(),
        InitArgsPlan::DecodeInitTail {
            tuple_ty,
            decode_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:[{}]",
            type_identity(db, *tuple_ty),
            runtime_instance_key_in_mode(db, *decode_fn, mode),
            projected_fields_sort_key(projected_fields),
        ),
    }
}

fn runtime_input_plan_key<'db>(
    db: &'db dyn MirDb,
    plan: &RuntimeInputPlan<'db>,
    mode: IdentityMode,
) -> String {
    match plan {
        RuntimeInputPlan::None => "none".to_string(),
        RuntimeInputPlan::DecodeHostPayload {
            msg_ty,
            host,
            decode_args_fn,
            projected_fields,
        } => format!(
            "decode:{}:{}:{}:[{}]",
            type_identity(db, *msg_ty),
            target_root_provider_binding_sort_key(db, host),
            runtime_instance_key_in_mode(db, *decode_args_fn, mode),
            projected_fields_sort_key(projected_fields),
        ),
    }
}

fn runtime_return_plan_key<'db>(
    db: &'db dyn MirDb,
    plan: &RuntimeReturnPlan<'db>,
    mode: IdentityMode,
) -> String {
    match plan {
        RuntimeReturnPlan::Unit => "unit".to_string(),
        RuntimeReturnPlan::Value {
            ty,
            host,
            return_value,
        } => format!(
            "value:{}:{}:{}",
            type_identity(db, *ty),
            target_root_provider_binding_sort_key(db, host),
            runtime_instance_key_in_mode(db, *return_value, mode),
        ),
    }
}

fn target_root_provider_binding_sort_key<'db>(
    db: &'db dyn MirDb,
    binding: &TargetRootProviderBinding<'db>,
) -> String {
    format!(
        "{}:{}:{}",
        type_identity(db, binding.declared_ty),
        runtime_class_sort_key(db, &binding.class),
        target_root_provider_materialization_sort_key(db, binding.materialization)
    )
}

fn dispatch_default_key<'db>(
    db: &'db dyn MirDb,
    default: &DispatchDefault<'db>,
    mode: IdentityMode,
) -> String {
    match default {
        DispatchDefault::RevertEmpty => "revert_empty".to_string(),
        DispatchDefault::Call { wrapper } => {
            format!("call:{}", runtime_instance_key_in_mode(db, *wrapper, mode))
        }
    }
}

fn runtime_code_region_key<'db>(db: &'db dyn MirDb, region: RuntimeCodeRegion<'db>) -> String {
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
        RuntimeClass::RawAddr { space, pointee } => format!(
            "raw:{}:{}",
            address_space_sort_key(*space),
            pointee
                .map(|pointee| raw_pointee_sort_key(db, pointee))
                .unwrap_or_default()
        ),
    }
}

/// Names a raw target by its class rendering. The result is a naming and
/// ordering string, not an identity.
pub(crate) fn raw_pointee_sort_key<'db>(db: &'db dyn MirDb, pointee: RawPointeeId<'db>) -> String {
    runtime_class_sort_key(db, pointee.target(db))
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
        ScalarRole::Pointer => "pointer".to_string(),
    }
}

fn ref_kind_sort_key<'db>(db: &'db dyn MirDb, kind: &RefKind<'db>) -> String {
    match kind {
        RefKind::Native => "native".to_string(),
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
    layout_sort_key_query(db, layout).clone()
}

#[salsa::tracked]
fn layout_sort_key_query<'db>(db: &'db dyn MirDb, layout: LayoutId<'db>) -> String {
    match layout.key(db) {
        LayoutKey::Struct(layout) => format!(
            "struct:[{}]",
            layout
                .fields
                .iter()
                .map(|field| runtime_class_sort_key(db, field))
                .collect::<Vec<_>>()
                .join(",")
        ),
        LayoutKey::Array(layout) => format!(
            "array:{}:{}",
            runtime_class_sort_key(db, &layout.elem),
            layout.len
        ),
        LayoutKey::Enum(layout) => format!(
            "enum:[{}]",
            layout
                .variants
                .iter()
                .enumerate()
                .map(|(idx, variant)| format!(
                    "{idx}:[{}]",
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

fn address_space_sort_key(space: AddressSpace) -> &'static str {
    match space {
        AddressSpace::Memory => "memory",
        AddressSpace::Storage => "storage",
        AddressSpace::Transient => "transient",
        AddressSpace::Calldata => "calldata",
        AddressSpace::Code => "code",
    }
}

pub(crate) fn runtime_instance_display_name<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> String {
    match instance.key(db).source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            symbol_base_for_semantic_instance(db, semantic)
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            symbol_base_for_runtime_instance(db, synthetic.spec(db))
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
            runtime_instance_display_name(db, instance)
        )),
    }
}

fn symbol_base_for_semantic_instance<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> String {
    let owner = semantic.key(db).owner(db);
    match owner {
        BodyOwner::Func(func, _) => func
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
        BodyOwner::Closure { ty, receiver, .. } => closure_symbol_component(db, ty, receiver),
        BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => "__const".to_string(),
    }
}

fn symbol_base_for_runtime_instance<'db>(
    db: &'db dyn MirDb,
    spec: &RuntimeSyntheticSpec<'db>,
) -> String {
    match spec {
        RuntimeSyntheticSpec::MainRoot { .. } => "main_root".to_string(),
        RuntimeSyntheticSpec::ExportRoot { callee, .. } => {
            format!("export_root_{}", runtime_instance_display_name(db, *callee))
        }
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
            plan.selector
                .map_or_else(|| "fallback".to_string(), |selector| selector.to_string()),
        ),
        RuntimeSyntheticSpec::ContractInitRoot { contract, .. } => {
            format!("contract_init_root_{}", contract_name(db, *contract))
        }
        RuntimeSyntheticSpec::ContractRuntimeRoot { contract, .. } => {
            format!("contract_runtime_root_{}", contract_name(db, *contract))
        }
    }
}

fn inline_hint_for_semantic<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> RuntimeInlineHint {
    match semantic.key(db).owner(db) {
        BodyOwner::Func(func, _) => match func.inline_hint(db) {
            Some(InlineHint::Hint) => RuntimeInlineHint::Hint,
            Some(InlineHint::Always) => RuntimeInlineHint::Always,
            Some(InlineHint::Never) => RuntimeInlineHint::Never,
            None => RuntimeInlineHint::Auto,
        },
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. }
        | BodyOwner::Closure { .. } => RuntimeInlineHint::Auto,
    }
}

fn collect_const_regions<'db>(
    db: &'db dyn MirDb,
    graph: &RuntimeGraph<'db>,
) -> Vec<ConstRegionId<'db>> {
    let mut seen = FxHashSet::default();
    let mut regions = Vec::new();
    let mut instances = graph.nodes.keys().copied().collect::<Vec<_>>();
    instances.sort_by_cached_key(|instance| {
        (
            runtime_instance_sort_key(db, *instance),
            runtime_instance_symbol_key(db, *instance),
        )
    });
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

pub(crate) fn contract_name<'db>(
    db: &'db dyn MirDb,
    contract: hir::hir_def::Contract<'db>,
) -> String {
    contract
        .name(db)
        .to_opt()
        .map(|name| sanitize_symbol(name.data(db)))
        .unwrap_or_else(|| "contract".to_string())
}

pub(crate) fn sanitize_symbol(value: &str) -> String {
    value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use salsa::Setter;
    use url::Url;

    use crate::runtime::RuntimeExitBehavior;

    use super::*;

    #[test]
    fn native_package_keeps_extern_calls_as_bodyless_declarations() {
        let mut db = DriverDataBase::default();
        db.compilation_settings()
            .set_target(&mut db)
            .to(Target::Native);
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///native_extern.fe").unwrap(),
            Some(
                r#"extern {
    fn putchar(_ c: i32) -> i32
}
pub fn main() -> i32 {
    let _ = unsafe { putchar(65) }
    0
}"#
                .to_string(),
            ),
        );
        let package =
            build_native_package(&db, db.top_mod(file), NativeOutputKind::Executable).unwrap();
        let imports = package
            .functions(&db)
            .iter()
            .filter(|function| function.linkage(&db) == RuntimeLinkage::External)
            .collect::<Vec<_>>();
        assert_eq!(imports.len(), 1);
        let imported = imports[0].instance(&db);
        assert!(imported.calls(&db).is_empty());
        assert!(imported.referenced_const_regions(&db).is_empty());
        assert!(imported.referenced_code_regions(&db).is_empty());
        assert_eq!(imported.interface_signature(&db).params.len(), 1);
        assert!(crate::format_runtime_package(&db, &package).contains("(external,"));
    }

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
        let (_, wrapper) = contract_recv_wrapper(db, arm, abi_ty).expect("recv wrapper");
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

    #[test]
    fn invalid_specialized_init_array_extent_returns_lowering_error() {
        for source in [
            "type Arr<const N: usize> = [u8; { 10 / N }]\npub contract C { init(value: Arr<0>) {} }\n",
            "struct S<const N: usize> { x: [u8; { 10 / N }] }\npub contract C { init(value: S<0>) {} }\n",
        ] {
            let mut db = DriverDataBase::default();
            let file_url = Url::parse("file:///invalid_specialized_init_array_extent.fe").unwrap();
            let file = db
                .workspace()
                .touch(&mut db, file_url, Some(source.to_string()));
            let top_mod = db.top_mod(file);
            let error = build_runtime_package(&db, top_mod)
                .expect_err("invalid specialized init array must not enter runtime layout");
            assert!(
                error.to_string().contains("array length"),
                "unexpected runtime error for {source}: {error}"
            );
        }
    }

    #[test]
    fn blocked_body_keeps_a_declaration_abi_but_cannot_be_lowered() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///blocked_semantic_body.fe").unwrap();
        let file = db.workspace().touch(
            &mut db,
            file_url,
            Some(
                r#"
fn invalid(result: mut u256) -> u256 {
    missing = 1
    result
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let func = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "invalid")
            })
            .expect("invalid function");
        let semantic = SemanticInstance::new(
            &db,
            hir::analysis::semantic::identity_semantic_instance_key(
                &db,
                BodyOwner::Func(func, None),
            ),
        );
        let runtime = runtime_instance_for_semantic(&db, semantic);

        let signature = runtime.interface_signature(&db);
        assert_eq!(signature.params.len(), 1);
        assert!(signature.ret.is_some());
        assert_eq!(runtime.exit_behavior(&db), RuntimeExitBehavior::MayReturn);

        let error = runtime_instance_lowered_body(&db, runtime)
            .expect_err("blocked semantic body must not reach runtime lowering");
        assert!(
            error.to_string().contains("semantic body is blocked by"),
            "{error}"
        );
    }

    fn package_object_names(db: &DriverDataBase, package: RuntimePackage<'_>) -> Vec<String> {
        let mut names = package
            .objects(db)
            .iter()
            .map(|object| object.name(db).clone())
            .collect::<Vec<_>>();
        names.sort();
        names
    }

    fn package_root_object_names(db: &DriverDataBase, package: RuntimePackage<'_>) -> Vec<String> {
        let mut names = package
            .root_objects(db)
            .iter()
            .map(|object| object.name(db).clone())
            .collect::<Vec<_>>();
        names.sort();
        names
    }

    #[test]
    fn same_named_contracts_in_nested_modules_get_distinct_objects() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///same_named_nested.fe").unwrap();
        let file = db.workspace().touch(
            &mut db,
            file_url,
            Some(
                r#"
pub contract C {}

pub mod inner {
    pub contract C {}
}

pub contract Other {}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("package should build");
        let mut names = package_root_object_names(&db, package);
        names.sort();
        // Only the clashing contracts are qualified.
        assert_eq!(
            names,
            vec![
                "Other",
                "same_named_nested__C",
                "same_named_nested__inner__C"
            ]
        );
        let mut symbols = package
            .code_regions(&db)
            .iter()
            .map(|region| region.symbol(&db).clone())
            .collect::<Vec<_>>();
        symbols.sort();
        assert_eq!(
            symbols,
            vec![
                "Other_init",
                "Other_runtime",
                "same_named_nested__C_init",
                "same_named_nested__C_runtime",
                "same_named_nested__inner__C_init",
                "same_named_nested__inner__C_runtime",
            ]
        );
    }

    #[test]
    fn a_test_that_deploys_same_named_contracts_gets_an_object_for_each() {
        with_test_runtime_package(
            "same_named_deployed.fe",
            r#"
pub contract C {}

pub mod inner {
    pub contract C {}
}

#[test]
fn deploys_both() uses (evm: mut Evm) {
    let first = evm.create2<C>(value: Wei::zero(), args: (), salt: 0)
    let second = evm.create2<inner::C>(value: Wei::zero(), args: (), salt: 1)
    assert!(first != second)
}
"#,
            None,
            |db, package| {
                assert_eq!(package_root_object_names(db, package), vec!["deploys_both"]);
                let mut names = package_object_names(db, package);
                names.sort();
                assert_eq!(
                    names,
                    vec![
                        "deploys_both",
                        "same_named_deployed__C",
                        "same_named_deployed__inner__C"
                    ]
                );
            },
        );
    }

    #[test]
    fn verifier_rejects_two_objects_with_one_name() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///duplicate_object_name.fe").unwrap();
        let file = db.workspace().touch(
            &mut db,
            file_url,
            Some("pub contract A {}\npub contract B {}\n".to_string()),
        );
        let package = build_runtime_package(&db, db.top_mod(file)).expect("package should build");
        let [first, second] = package.objects(&db) else {
            panic!("expected two objects");
        };
        let duplicate =
            RuntimeObject::new(&db, first.name(&db).clone(), second.sections(&db).clone());
        let objects = vec![*first, duplicate];
        let broken = RuntimePackage::new(
            &db,
            package.top_mod(&db),
            package.functions(&db).clone(),
            RuntimePackagePlan::new(&db, objects, Vec::new(), Vec::new(), Vec::new(), None),
        );
        assert_eq!(
            verify_runtime_package(&db, broken),
            Err(crate::VerifyError::DuplicateObjectName("A".to_string()))
        );
    }

    #[test]
    fn filtered_test_package_excludes_unreferenced_contract_roots() {
        with_test_runtime_package(
            "filtered_test_package_excludes_unreferenced_contract_roots.fe",
            r#"
pub contract Unused {}

#[test]
fn selected() {
    assert!(true)
}

#[test]
fn ignored() {
    assert!(true)
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
use std::evm::Evm

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
    let addr = evm.create2<Child>(value: Wei::zero(), args: (), salt: 1)
    assert!(addr.word() != 0)
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
        let RuntimeInputPlan::DecodeHostPayload {
            projected_fields, ..
        } = raw_plan.input
        else {
            panic!("raw(uint256) should decode host payload");
        };
        assert!(
            projected_fields.is_empty(),
            "ignored recv arm fields must not be forwarded to the runtime callee: {projected_fields:?}"
        );

        let swap_plan = recv_wrapper_plan(&db, top_mod, "swap(uint64,uint64)");
        let RuntimeInputPlan::DecodeHostPayload {
            projected_fields, ..
        } = swap_plan.input
        else {
            panic!("swap(uint64,uint64) should decode host payload");
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
            plan.entry_args.effects.is_empty(),
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
            *root_init_abi, init_abi,
            "contract init root should always call the synthesized init abi wrapper"
        );
    }
}
