mod lower_runtime;

use std::collections::{BTreeMap, VecDeque};

use common::ingot::Ingot;
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::runtime::ir::RuntimePackagePlan;
use mir::{RuntimePackage, build_runtime_package, build_test_runtime_package};
use rustc_hash::FxHashSet;
use sonatina_codegen::{EvmCompile, OptLevel as SonatinaOptLevel};
use sonatina_ir::{
    Module,
    ir_writer::{FuncWriter, ModuleWriter},
    isa::evm::Evm,
    module::{FuncRef, ModuleCtx},
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};
use sonatina_verifier::{
    Location, VerificationLevel, VerificationReport, VerifierConfig, verify_module,
};

use crate::{
    OptLevel, TargetDataLayout, TestMetadata, TestModuleOutput,
    runtime_package::ensure_runtime_package_has_roots,
    test_output::{TestRootMetadataError, runtime_test_root_metadata},
};

#[derive(Debug)]
pub enum LowerError {
    RuntimeLower(mir::LowerError),
    Unsupported(String),
    Internal(String),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::RuntimeLower(err) => write!(f, "{err}"),
            LowerError::Unsupported(message) => write!(f, "unsupported: {message}"),
            LowerError::Internal(message) => write!(f, "internal error: {message}"),
        }
    }
}

impl std::error::Error for LowerError {}

impl From<mir::LowerError> for LowerError {
    fn from(err: mir::LowerError) -> Self {
        LowerError::RuntimeLower(err)
    }
}

#[derive(Debug, Clone)]
pub struct SonatinaContractBytecode {
    pub deploy: Vec<u8>,
    pub runtime: Vec<u8>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SonatinaTestOptions {
    pub emit_observability: bool,
}

pub(crate) fn create_evm_isa() -> Evm {
    Evm::new(TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    ))
}

fn create_module_ctx() -> ModuleCtx {
    ModuleCtx::new(&create_evm_isa())
}

fn ensure_module_sonatina_ir_valid(module: &Module) -> Result<(), LowerError> {
    let report = verify_module(module, &VerifierConfig::for_level(VerificationLevel::Full));
    if report.has_errors() {
        return Err(LowerError::Internal(format_verification_report(
            module, &report,
        )));
    }
    Ok(())
}

fn format_verification_report(module: &Module, report: &VerificationReport) -> String {
    const MAX_FUNC_CONTEXTS: usize = 3;

    let mut out = report.to_string();
    let funcs = failing_function_contexts(module, report);
    if funcs.is_empty() {
        return out;
    }

    out.push_str("\n\nVerifier function IR context");
    if funcs.len() > MAX_FUNC_CONTEXTS {
        out.push_str(&format!(
            " (showing first {MAX_FUNC_CONTEXTS} of {})",
            funcs.len()
        ));
    }
    out.push_str(":\n");

    for (func_ref, func_name, func_ir) in funcs.into_iter().take(MAX_FUNC_CONTEXTS) {
        out.push_str(&format!(
            "\n---- func{} (%{func_name}) ----\n{func_ir}\n",
            func_ref.as_u32()
        ));
    }

    out
}

fn failing_function_contexts(
    module: &Module,
    report: &VerificationReport,
) -> Vec<(FuncRef, String, String)> {
    let mut funcs = Vec::new();
    for diagnostic in report.errors() {
        let Some(func_ref) = diagnostic_func_ref(&diagnostic.primary) else {
            continue;
        };
        if funcs.iter().any(|(existing, _, _)| *existing == func_ref)
            || !module.func_store.contains(func_ref)
        {
            continue;
        }
        let Some(func_name) = module
            .ctx
            .get_sig(func_ref)
            .map(|sig| sig.name().to_string())
        else {
            continue;
        };
        let func_ir = module.func_store.view(func_ref, |func| {
            FuncWriter::new(func_ref, func).dump_string()
        });
        funcs.push((func_ref, func_name, func_ir));
    }
    funcs
}

fn diagnostic_func_ref(location: &Location) -> Option<FuncRef> {
    match location {
        Location::Function(func)
        | Location::Block { func, .. }
        | Location::Inst { func, .. }
        | Location::Value { func, .. } => Some(*func),
        Location::Type {
            func: Some(func), ..
        } => Some(*func),
        Location::Module
        | Location::Global(_)
        | Location::Object { .. }
        | Location::Type { func: None, .. } => None,
    }
}

fn to_sonatina_opt_level(opt_level: OptLevel) -> SonatinaOptLevel {
    match opt_level {
        OptLevel::O0 => SonatinaOptLevel::O0,
        OptLevel::O1 => SonatinaOptLevel::O1,
        OptLevel::Os => SonatinaOptLevel::Os,
        OptLevel::O2 => SonatinaOptLevel::O2,
    }
}

fn evm_compile(module: Module, opt_level: OptLevel, emit_observability: bool) -> EvmCompile {
    EvmCompile::new(module)
        .with_opt_level(to_sonatina_opt_level(opt_level))
        .with_observability(emit_observability)
}

fn format_object_compile_errors(errors: &[sonatina_codegen::object::ObjectCompileError]) -> String {
    errors
        .iter()
        .map(|error| format!("{error:?}"))
        .collect::<Vec<_>>()
        .join("; ")
}

fn compile_runtime_objects(
    module: Module,
    opt_level: OptLevel,
    emit_observability: bool,
) -> Result<Vec<sonatina_codegen::object::ObjectArtifact>, LowerError> {
    let mut compile = evm_compile(module, opt_level, emit_observability);
    ensure_module_sonatina_ir_valid(compile.optimize())?;
    compile
        .compile()
        .map_err(|errors| LowerError::Internal(format_object_compile_errors(&errors)))
}

pub fn compile_to_artifacts(
    db: &driver::DriverDataBase,
    package: &mir::RuntimePackage<'_>,
    layout: crate::TargetDataLayout,
) -> Result<
    (
        Vec<sonatina_codegen::object::ObjectArtifact>,
        lower_runtime::SonatinaOriginMap,
    ),
    LowerError,
> {
    let (module, origins) = lower_runtime::compile_runtime_package_sonatina(db, package, layout)?;
    let artifacts = compile_runtime_objects(module, OptLevel::O1, true)?;
    Ok((artifacts, origins))
}

pub struct OptimizationProvenanceReport {
    pub pre_opt_origins: lower_runtime::SonatinaOriginMap,
    pub post_opt_origins: lower_runtime::SonatinaOriginMap,
    pub survived: usize,
    pub eliminated: usize,
    pub new_insts: usize,
}

pub fn compile_to_artifacts_with_opt_provenance(
    db: &driver::DriverDataBase,
    package: &mir::RuntimePackage<'_>,
    layout: crate::TargetDataLayout,
    opt_level: OptLevel,
) -> Result<
    (
        Vec<sonatina_codegen::object::ObjectArtifact>,
        OptimizationProvenanceReport,
    ),
    LowerError,
> {
    use common::provenance::{IrLevel, ProvenanceNodeId, TransformTag};
    use std::collections::{HashMap, HashSet};

    let (module, pre_origins) =
        lower_runtime::compile_runtime_package_sonatina(db, package, layout)?;

    // Build lookup: (FuncRef, InstId) → ProvenanceNodeId
    let origin_map: HashMap<(FuncRef, sonatina_ir::InstId), ProvenanceNodeId> =
        pre_origins.iter().map(|(f, i, o)| ((*f, *i), *o)).collect();

    // Snapshot pre-optimization live instructions per function
    let mut pre_live: HashMap<FuncRef, HashSet<sonatina_ir::InstId>> = HashMap::new();
    for func_ref in module.funcs() {
        let insts = module.func_store.view(func_ref, |func| {
            let mut set = HashSet::new();
            for block in func.layout.iter_block() {
                for inst in func.layout.iter_inst(block) {
                    set.insert(inst);
                }
            }
            set
        });
        pre_live.insert(func_ref, insts);
    }

    // Optimize
    let mut compile = evm_compile(module, opt_level, true);
    let optimized = compile.optimize();

    // Snapshot post-optimization live instructions
    let mut post_live: HashMap<FuncRef, HashSet<sonatina_ir::InstId>> = HashMap::new();
    for func_ref in optimized.funcs() {
        let insts = optimized.func_store.view(func_ref, |func| {
            let mut set = HashSet::new();
            for block in func.layout.iter_block() {
                for inst in func.layout.iter_inst(block) {
                    set.insert(inst);
                }
            }
            set
        });
        post_live.insert(func_ref, insts);
    }

    // Diff
    let mut survived = 0usize;
    let mut eliminated = 0usize;
    let mut new_insts = 0usize;
    let mut post_origins: lower_runtime::SonatinaOriginMap = Vec::new();

    for func_ref in optimized.funcs() {
        let post = post_live
            .get(&func_ref)
            .map(|s| s.iter().copied().collect::<Vec<_>>())
            .unwrap_or_default();
        let pre = pre_live.get(&func_ref);

        for inst_id in &post {
            let was_pre = pre.is_some_and(|s| s.contains(inst_id));
            if was_pre {
                survived += 1;
                if let Some(origin) = origin_map.get(&(func_ref, *inst_id)) {
                    post_origins.push((func_ref, *inst_id, *origin));
                }
            } else {
                new_insts += 1;
                post_origins.push((
                    func_ref,
                    *inst_id,
                    ProvenanceNodeId::new(
                        IrLevel::Sonatina,
                        inst_id.0,
                        TransformTag::SonatinaOptNew,
                    ),
                ));
            }
        }

        if let Some(pre_set) = pre {
            let post_set = post_live.get(&func_ref);
            for inst_id in pre_set {
                let still_live = post_set.is_some_and(|s| s.contains(inst_id));
                if !still_live {
                    eliminated += 1;
                }
            }
        }
    }

    let artifacts = compile
        .compile()
        .map_err(|errors| LowerError::Internal(format_object_compile_errors(&errors)))?;

    Ok((
        artifacts,
        OptimizationProvenanceReport {
            pre_opt_origins: pre_origins,
            post_opt_origins: post_origins,
            survived,
            eliminated,
            new_insts,
        },
    ))
}

fn section_name_for_runtime(name: &mir::RuntimeSectionName) -> sonatina_ir::SectionName {
    match name {
        mir::RuntimeSectionName::Init => "init".into(),
        mir::RuntimeSectionName::Runtime => "runtime".into(),
        mir::RuntimeSectionName::Main => "main".into(),
        mir::RuntimeSectionName::Test(name) => format!("test_{name}").into(),
        mir::RuntimeSectionName::CodeRegion(symbol) => format!("code_region_{symbol}").into(),
    }
}

fn wrap_as_init_code(runtime: &[u8]) -> Vec<u8> {
    fn push_u256(mut value: usize) -> Vec<u8> {
        let mut bytes = Vec::new();
        while value > 0 {
            bytes.push((value & 0xff) as u8);
            value >>= 8;
        }
        if bytes.is_empty() {
            bytes.push(0);
        }
        bytes.reverse();
        let mut out = Vec::with_capacity(1 + bytes.len());
        out.push(0x5f + bytes.len() as u8);
        out.extend(bytes);
        out
    }

    let len_push = push_u256(runtime.len());
    let mut init = Vec::with_capacity(32 + runtime.len());
    init.extend(len_push.clone());
    init.push(0x61);
    let off_pos = init.len();
    init.extend([0, 0]);
    init.extend([0x60, 0x00]);
    init.push(0x39);
    init.extend(len_push);
    init.extend([0x60, 0x00]);
    init.push(0xf3);
    let off = init.len();
    init[off_pos] = ((off >> 8) & 0xff) as u8;
    init[off_pos + 1] = (off & 0xff) as u8;
    init.extend_from_slice(runtime);
    init
}

pub fn compile_runtime_package_sonatina(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
) -> Result<Module, LowerError> {
    let (module, _origins) = lower_runtime::compile_runtime_package_sonatina(db, package, layout)?;
    Ok(module)
}

#[allow(dead_code)] // Public API — used by tests and external consumers
pub fn compile_runtime_package_sonatina_with_origins(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
) -> Result<(Module, lower_runtime::SonatinaOriginMap), LowerError> {
    lower_runtime::compile_runtime_package_sonatina(db, package, layout)
}

fn select_runtime_package_contract<'db>(
    db: &'db dyn mir::MirDb,
    package: RuntimePackage<'db>,
    contract: Option<&str>,
) -> Result<RuntimePackage<'db>, LowerError> {
    let Some(contract) = contract else {
        return Ok(package);
    };
    let matches = root_objects_named(db, package, contract);
    match matches.as_slice() {
        [] => Err(LowerError::Internal(format!(
            "root object `{contract}` not found in runtime package"
        ))),
        [root] => Ok(filter_runtime_package_to_root_objects(
            db,
            package,
            &[*root],
        )),
        _ => Err(LowerError::Internal(format!(
            "multiple root objects named `{contract}` in runtime package"
        ))),
    }
}

fn select_ingot_runtime_packages<'db>(
    db: &'db dyn mir::MirDb,
    ingot: Ingot<'db>,
    contract: Option<&str>,
) -> Result<Vec<RuntimePackage<'db>>, LowerError> {
    let mut packages = Vec::new();
    for &top_mod in ingot.all_modules(db) {
        let package = build_runtime_package(db, top_mod)?;
        if package.root_objects(db).is_empty() {
            continue;
        }
        let Some(contract) = contract else {
            packages.push(package);
            continue;
        };
        let matches = root_objects_named(db, package, contract);
        if matches.len() > 1 {
            return Err(LowerError::Internal(format!(
                "multiple root objects named `{contract}` in runtime package"
            )));
        }
        if let Some(root) = matches.first().copied() {
            packages.push(filter_runtime_package_to_root_objects(db, package, &[root]));
        }
    }
    if let Some(contract) = contract {
        if packages.is_empty() {
            return Err(LowerError::Internal(format!(
                "root object `{contract}` not found in ingot runtime packages"
            )));
        }
        if packages.len() > 1 {
            return Err(LowerError::Internal(format!(
                "duplicate root object `{contract}` across ingot modules"
            )));
        }
    }
    Ok(packages)
}

fn root_objects_named<'db>(
    db: &'db dyn mir::MirDb,
    package: RuntimePackage<'db>,
    name: &str,
) -> Vec<mir::RuntimeObject<'db>> {
    package
        .root_objects(db)
        .into_iter()
        .filter(|object| object.name(db) == name)
        .collect()
}

fn filter_runtime_package_to_root_objects<'db>(
    db: &'db dyn mir::MirDb,
    package: RuntimePackage<'db>,
    roots: &[mir::RuntimeObject<'db>],
) -> RuntimePackage<'db> {
    let root_names = roots
        .iter()
        .map(|object| object.name(db).clone())
        .collect::<FxHashSet<_>>();
    let package_objects = package.objects(db);
    let section_set = reachable_sections(db, &package_objects, roots);
    let objects = package
        .objects(db)
        .into_iter()
        .filter_map(|object| {
            let sections = object
                .sections(db)
                .into_iter()
                .filter(|section| {
                    section_set.contains(&runtime_section_key(db, object, &section.name))
                })
                .collect::<Vec<_>>();
            (!sections.is_empty())
                .then(|| mir::RuntimeObject::new(db, object.name(db).clone(), sections))
        })
        .collect::<Vec<_>>();
    let function_set = reachable_functions(db, &objects);
    let functions = package
        .functions(db)
        .into_iter()
        .filter(|function| function_set.contains(&function.instance(db)))
        .collect::<Vec<_>>();
    let const_region_set = reachable_const_regions(db, &objects, &functions);
    let const_regions = package
        .const_regions(db)
        .into_iter()
        .filter(|region| const_region_set.contains(region))
        .collect::<Vec<_>>();
    let code_regions = package
        .code_regions(db)
        .into_iter()
        .filter(|region| section_set.contains(&section_ref_key(db, region.source(db))))
        .collect::<Vec<_>>();
    let root_objects = package
        .objects(db)
        .into_iter()
        .filter(|object| root_names.contains(&object.name(db)))
        .filter_map(|object| {
            objects
                .iter()
                .find(|filtered| filtered.name(db) == object.name(db))
                .copied()
        })
        .collect::<Vec<_>>();
    let primary_object = package
        .primary_object(db)
        .filter(|object| root_names.contains(&object.name(db)))
        .and_then(|object| {
            objects
                .iter()
                .find(|filtered| filtered.name(db) == object.name(db))
                .copied()
        })
        .or_else(|| root_objects.first().copied());

    RuntimePackage::new(
        db,
        package.top_mod(db),
        functions,
        RuntimePackagePlan::new(
            db,
            objects,
            const_regions,
            code_regions,
            root_objects,
            primary_object,
        ),
    )
}

fn reachable_sections<'db>(
    db: &'db dyn mir::MirDb,
    objects: &[mir::RuntimeObject<'db>],
    roots: &[mir::RuntimeObject<'db>],
) -> FxHashSet<(String, mir::RuntimeSectionName)> {
    let mut seen = FxHashSet::default();
    let mut queue = roots
        .iter()
        .flat_map(|object| {
            object
                .sections(db)
                .into_iter()
                .map(|section| runtime_section_key(db, *object, &section.name))
        })
        .collect::<VecDeque<_>>();
    while let Some((object_name, section_name)) = queue.pop_front() {
        if !seen.insert((object_name.clone(), section_name.clone())) {
            continue;
        }
        for section in objects
            .iter()
            .flat_map(|object| {
                object
                    .sections(db)
                    .into_iter()
                    .map(move |section| (*object, section))
            })
            .filter(|(object, _)| object.name(db) == object_name)
            .filter(|(_, section)| section.name == section_name)
            .map(|(_, section)| section)
        {
            for embed in section.embeds {
                queue.push_back(section_ref_key(db, embed.source));
            }
        }
    }
    seen
}

fn runtime_section_key<'db>(
    db: &'db dyn mir::MirDb,
    object: mir::RuntimeObject<'db>,
    section: &mir::RuntimeSectionName,
) -> (String, mir::RuntimeSectionName) {
    (object.name(db).clone(), section.clone())
}

fn section_ref_key<'db>(
    db: &'db dyn mir::MirDb,
    section_ref: mir::RuntimeSectionRef<'db>,
) -> (String, mir::RuntimeSectionName) {
    match section_ref {
        mir::RuntimeSectionRef::Local { object, section }
        | mir::RuntimeSectionRef::External { object, section } => {
            runtime_section_key(db, object, &section)
        }
    }
}

fn reachable_functions<'db>(
    db: &'db dyn mir::MirDb,
    objects: &[mir::RuntimeObject<'db>],
) -> FxHashSet<mir::RuntimeInstance<'db>> {
    let mut seen = FxHashSet::default();
    let mut queue = objects
        .iter()
        .flat_map(|object| object.sections(db))
        .map(|section| section.entry.instance(db))
        .collect::<VecDeque<_>>();
    while let Some(instance) = queue.pop_front() {
        if !seen.insert(instance) {
            continue;
        }
        for call in instance.calls(db) {
            queue.push_back(call.callee);
        }
    }
    seen
}

fn reachable_const_regions<'db>(
    db: &'db dyn mir::MirDb,
    objects: &[mir::RuntimeObject<'db>],
    functions: &[mir::RuntimeFunction<'db>],
) -> FxHashSet<mir::ConstRegionId<'db>> {
    let mut seen = FxHashSet::default();
    for section in objects.iter().flat_map(|object| object.sections(db)) {
        seen.extend(section.const_regions);
    }
    for function in functions {
        seen.extend(function.referenced_const_regions(db));
    }
    seen
}

pub fn emit_runtime_package_sonatina_ir(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
) -> Result<String, LowerError> {
    ensure_runtime_package_has_roots(db, package, "Sonatina IR")?;
    let module = compile_runtime_package_sonatina(db, package, layout)?;
    let mut writer = ModuleWriter::new(&module);
    Ok(writer.dump_string())
}

pub fn emit_runtime_package_sonatina_ir_optimized(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
    opt_level: OptLevel,
) -> Result<String, LowerError> {
    ensure_runtime_package_has_roots(db, package, "Sonatina IR")?;
    let module = compile_runtime_package_sonatina(db, package, layout)?;
    ensure_module_sonatina_ir_valid(&module)?;
    let mut compile = evm_compile(module, opt_level, false);
    let optimized = compile.optimize();
    ensure_module_sonatina_ir_valid(optimized)?;
    let mut writer = ModuleWriter::new(optimized);
    Ok(writer.dump_string())
}

pub fn emit_runtime_package_sonatina_bytecode(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
    opt_level: OptLevel,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    ensure_runtime_package_has_roots(db, package, "Sonatina bytecode")?;
    let module = compile_runtime_package_sonatina(db, package, layout)?;
    ensure_module_sonatina_ir_valid(&module)?;
    let artifacts = compile_runtime_objects(module, opt_level, false)?;
    let artifacts_by_name = artifacts
        .iter()
        .map(|artifact| (artifact.object.0.as_str(), artifact))
        .collect::<std::collections::HashMap<_, _>>();

    let mut out = BTreeMap::new();
    for object in package.root_objects(db) {
        let object_name = object.name(db);
        let artifact = artifacts_by_name
            .get(object_name.as_str())
            .copied()
            .ok_or_else(|| {
                LowerError::Internal(format!("compiled object `{object_name}` not found"))
            })?;
        let init = artifact
            .sections
            .get(&section_name_for_runtime(&mir::RuntimeSectionName::Init));
        let runtime = artifact
            .sections
            .get(&section_name_for_runtime(&mir::RuntimeSectionName::Runtime));
        let (deploy, runtime) = match (init, runtime) {
            (Some(init), Some(runtime)) => (init.bytes.clone(), runtime.bytes.clone()),
            _ => {
                let sections = object.sections(db);
                let section = sections.first().ok_or_else(|| {
                    LowerError::Internal(format!("root object `{object_name}` has no sections"))
                })?;
                let runtime = artifact
                    .sections
                    .get(&section_name_for_runtime(&section.name))
                    .ok_or_else(|| {
                        LowerError::Internal(format!(
                            "compiled object `{object_name}` is missing section `{:?}`",
                            section.name
                        ))
                    })?
                    .bytes
                    .clone();
                (wrap_as_init_code(&runtime), runtime)
            }
        };
        out.insert(
            object_name.clone(),
            SonatinaContractBytecode { deploy, runtime },
        );
    }
    Ok(out)
}

pub fn emit_module_sonatina_ir(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, LowerError> {
    let package = build_runtime_package(db, top_mod)?;
    emit_runtime_package_sonatina_ir(db, &package, crate::EVM_LAYOUT)
}

pub fn emit_module_sonatina_ir_optimized(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<String, LowerError> {
    let package = build_runtime_package(db, top_mod)?;
    let package = select_runtime_package_contract(db, package, contract)?;
    emit_runtime_package_sonatina_ir_optimized(db, &package, crate::EVM_LAYOUT, opt_level)
}

pub fn emit_ingot_sonatina_ir(db: &DriverDataBase, ingot: Ingot<'_>) -> Result<String, LowerError> {
    let mut modules = Vec::new();
    for &top_mod in ingot.all_modules(db) {
        let package = build_runtime_package(db, top_mod)?;
        if package.root_objects(db).is_empty() {
            continue;
        }
        modules.push(emit_runtime_package_sonatina_ir(
            db,
            &package,
            crate::EVM_LAYOUT,
        )?);
    }
    if modules.is_empty() {
        return Err(mir::LowerError::Unsupported(
            "runtime package has no root objects; refusing to emit target-only Sonatina IR"
                .to_string(),
        )
        .into());
    }
    Ok(modules.join("\n\n"))
}

pub fn emit_ingot_sonatina_ir_optimized(
    db: &DriverDataBase,
    ingot: Ingot<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<String, LowerError> {
    let mut modules = Vec::new();
    for package in select_ingot_runtime_packages(db, ingot, contract)? {
        modules.push(emit_runtime_package_sonatina_ir_optimized(
            db,
            &package,
            crate::EVM_LAYOUT,
            opt_level,
        )?);
    }
    if modules.is_empty() {
        return Err(mir::LowerError::Unsupported(
            "runtime package has no root objects; refusing to emit target-only Sonatina IR"
                .to_string(),
        )
        .into());
    }
    Ok(modules.join("\n\n"))
}

pub fn validate_module_sonatina_ir(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, LowerError> {
    let package = build_runtime_package(db, top_mod)?;
    compile_runtime_package_sonatina(db, &package, crate::EVM_LAYOUT)?;
    Ok("ok\n".to_string())
}

pub fn emit_module_sonatina_bytecode(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    let package = build_runtime_package(db, top_mod)?;
    let package = select_runtime_package_contract(db, package, contract)?;
    emit_runtime_package_sonatina_bytecode(db, &package, crate::EVM_LAYOUT, opt_level)
}

pub fn emit_ingot_sonatina_bytecode(
    db: &DriverDataBase,
    ingot: Ingot<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    let mut outputs = BTreeMap::new();
    for package in select_ingot_runtime_packages(db, ingot, contract)? {
        for (name, bytecode) in
            emit_runtime_package_sonatina_bytecode(db, &package, crate::EVM_LAYOUT, opt_level)?
        {
            if outputs.insert(name.clone(), bytecode).is_some() {
                return Err(LowerError::Internal(format!(
                    "duplicate root object `{name}` across ingot modules"
                )));
            }
        }
    }
    if outputs.is_empty() {
        return Err(mir::LowerError::Unsupported(
            "runtime package has no root objects; refusing to emit target-only Sonatina bytecode"
                .to_string(),
        )
        .into());
    }
    Ok(outputs)
}

pub fn emit_test_module_sonatina(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    options: SonatinaTestOptions,
    filter: Option<&str>,
) -> Result<TestModuleOutput, LowerError> {
    let package = build_test_runtime_package(db, top_mod, filter)?;
    if package.root_objects(db).is_empty() {
        return Ok(TestModuleOutput { tests: Vec::new() });
    }
    let module = compile_runtime_package_sonatina(db, &package, crate::EVM_LAYOUT)?;
    ensure_module_sonatina_ir_valid(&module)?;
    let artifacts = compile_runtime_objects(module, opt_level, options.emit_observability)?;
    let artifacts_by_name = artifacts
        .iter()
        .map(|artifact| (artifact.object.0.as_str(), artifact))
        .collect::<std::collections::HashMap<_, _>>();

    let mut tests = Vec::new();
    for object in package.root_objects(db) {
        let sections = object.sections(db);
        let Some(section) = sections.first() else {
            continue;
        };
        let mir::RuntimeSectionName::Test(_) = &section.name else {
            continue;
        };
        let artifact = artifacts_by_name
            .get(object.name(db).as_str())
            .copied()
            .ok_or_else(|| {
                LowerError::Internal(format!("compiled object `{}` not found", object.name(db)))
            })?;
        let runtime = artifact
            .sections
            .get(&section_name_for_runtime(&section.name))
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "compiled object `{}` missing test section",
                    object.name(db)
                ))
            })?;
        let metadata = runtime_test_root_metadata(db, &section.entry.owner(db), &section.name)
            .map_err(|err| match err {
                TestRootMetadataError::InvalidPackage(message) => LowerError::Internal(message),
                TestRootMetadataError::Unsupported(message) => LowerError::Unsupported(message),
            })?;
        tests.push(TestMetadata {
            display_name: metadata.display_name,
            hir_name: metadata.hir_name,
            symbol_name: section.entry.symbol(db).clone(),
            object_name: object.name(db).clone(),
            bytecode: wrap_as_init_code(&runtime.bytes),
            sonatina_observability_json: artifact.observability_json(),
            value_param_count: 0,
            effect_param_count: 0,
            init_bytecode: Vec::new(),
            expected_revert: metadata.expected_revert,
            initial_balance: metadata.initial_balance,
        });
    }
    Ok(TestModuleOutput { tests })
}

pub fn emit_test_ingot_sonatina(
    db: &DriverDataBase,
    ingot: Ingot<'_>,
    opt_level: OptLevel,
    options: SonatinaTestOptions,
    filter: Option<&str>,
) -> Result<TestModuleOutput, LowerError> {
    let mut top_mods = ingot.all_modules(db).to_vec();
    top_mods.sort_by(|left, right| left.name(db).cmp(&right.name(db)));

    let mut output = TestModuleOutput { tests: Vec::new() };
    for top_mod in top_mods {
        output.extend(emit_test_module_sonatina(
            db, top_mod, opt_level, options, filter,
        )?);
    }
    output.sort_tests();
    Ok(output)
}

#[cfg(test)]
#[allow(clippy::print_stderr)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use std::{fs, path::PathBuf};
    use url::Url;

    fn temp_fixture_url(name: &str) -> Url {
        let fixture_path = std::env::temp_dir().join(name);
        Url::from_file_path(&fixture_path).expect("fixture path should be absolute")
    }

    #[test]
    fn fe_opt_levels_map_to_sonatina_opt_levels() {
        assert_eq!(to_sonatina_opt_level(OptLevel::O0), SonatinaOptLevel::O0);
        assert_eq!(to_sonatina_opt_level(OptLevel::O1), SonatinaOptLevel::O1);
        assert_eq!(to_sonatina_opt_level(OptLevel::Os), SonatinaOptLevel::Os);
        assert_eq!(to_sonatina_opt_level(OptLevel::O2), SonatinaOptLevel::O2);
    }

    #[test]
    fn module_sonatina_bytecode_respects_contract_filter() {
        let mut db = DriverDataBase::default();
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../fe/tests/fixtures/cli_output/build/multi_contract.fe");
        let fixture_source =
            fs::read_to_string(&fixture_path).expect("multi_contract fixture should be readable");
        let file_url = Url::from_file_path(&fixture_path).expect("fixture path should be absolute");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(fixture_source));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let bytecode = emit_module_sonatina_bytecode(&db, top_mod, OptLevel::O0, Some("Foo"))
            .expect("selected contract should compile");
        let keys = bytecode.keys().map(String::as_str).collect::<Vec<_>>();

        assert_eq!(
            keys,
            vec!["Foo"],
            "selected contract bytecode should exclude unselected roots"
        );
    }

    #[test]
    fn result_map_chain_test_runtime_package_retains_value_enum_asserts() {
        let mut db = DriverDataBase::default();
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../fe/tests/fixtures/fe_test/result_map_chain_infers_independently.fe");
        let fixture_source = fs::read_to_string(&fixture_path)
            .expect("result_map_chain_infers_independently fixture should be readable");
        let file_url = Url::from_file_path(&fixture_path).expect("fixture path should be absolute");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(fixture_source));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_test_runtime_package(&db, top_mod, None)
            .expect("test runtime package should build");

        let module = compile_runtime_package_sonatina(&db, &package, crate::EVM_LAYOUT)
            .expect("test runtime package should lower to Sonatina IR");
        let dumped = ModuleWriter::new(&module).dump_string();
        let map_helpers = dumped
            .lines()
            .filter(|line| line.starts_with("func private %map"))
            .collect::<Vec<_>>();
        assert_eq!(
            map_helpers.len(),
            2,
            "expected two map helpers in test runtime package:\n{dumped}"
        );
        assert!(
            map_helpers
                .iter()
                .all(|line| line.starts_with("func private %map__g")),
            "expected colliding map helpers to include generic discriminators:\n{dumped}"
        );
        assert!(
            dumped.contains("func private %unwrap"),
            "expected unwrap helper in test runtime package:\n{dumped}"
        );
        assert!(
            dumped.contains("enum.assert_variant"),
            "expected value enum proofs in test runtime package:\n{dumped}"
        );

        if let Err(err) = ensure_module_sonatina_ir_valid(&module) {
            panic!("pre-opt test module should verify: {err}\n\n{dumped}");
        }
        compile_runtime_objects(module, OptLevel::O0, false)
            .expect("test runtime package should compile");
    }

    #[test]
    fn int_downcast_test_runtime_package_verifies_with_enum_param_init_cfg() {
        let mut db = DriverDataBase::default();
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../fe/tests/fixtures/fe_test/int_downcast.fe");
        let fixture_source =
            fs::read_to_string(&fixture_path).expect("int_downcast fixture should be readable");
        let file_url = Url::from_file_path(&fixture_path).expect("fixture path should be absolute");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(fixture_source));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_test_runtime_package(&db, top_mod, None)
            .expect("test runtime package should build");

        let module = compile_runtime_package_sonatina(&db, &package, crate::EVM_LAYOUT)
            .expect("test runtime package should lower to Sonatina IR");
        let dumped = ModuleWriter::new(&module).dump_string();

        if let Err(err) = ensure_module_sonatina_ir_valid(&module) {
            panic!("pre-opt test module should verify: {err}\n\n{dumped}");
        }
        compile_runtime_objects(module, OptLevel::O0, false)
            .expect("test runtime package should compile");
    }

    #[test]
    fn enum_state_machine_test_runtime_package_supports_storage_enum_roundtrips() {
        let mut db = DriverDataBase::default();
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../fe/tests/fixtures/fe_test/enum_state_machine.fe");
        let fixture_source = fs::read_to_string(&fixture_path)
            .expect("enum_state_machine fixture should be readable");
        let file_url = Url::from_file_path(&fixture_path).expect("fixture path should be absolute");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(fixture_source));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_test_runtime_package(&db, top_mod, None)
            .expect("test runtime package should build");

        let module = compile_runtime_package_sonatina(&db, &package, crate::EVM_LAYOUT)
            .expect("test runtime package should lower to Sonatina IR");
        let dumped = ModuleWriter::new(&module).dump_string();

        if let Err(err) = ensure_module_sonatina_ir_valid(&module) {
            panic!("pre-opt test module should verify: {err}\n\n{dumped}");
        }
        compile_runtime_objects(module, OptLevel::O0, false)
            .expect("test runtime package should compile");
    }

    #[test]
    fn if_both_arms_return_test_runtime_package_has_no_empty_unreachable_blocks() {
        let mut db = DriverDataBase::default();
        let file_url = temp_fixture_url("if_both_arms_return_sonatina_runtime.fe");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn f(x: u256) -> u256 {
    if x == 0 {
        return 1
    } else {
        return 2
    }
}

#[test]
fn roundtrip() {
    assert(f(0) == 1)
    assert(f(1) == 2)
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

        emit_test_module_sonatina(
            &db,
            top_mod,
            OptLevel::O0,
            SonatinaTestOptions::default(),
            None,
        )
        .expect(
            "if branches that both return should lower without empty unreachable Sonatina blocks",
        );
    }

    #[test]
    fn sonatina_lowering_produces_origin_map() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(std::env::temp_dir().join("sonatina_origin_test.fe"))
            .expect("path");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
msg Msg {
    #[selector = 0x11111111]
    Compute { a: u256, b: u256 } -> u256,
}

pub contract C {
    recv Msg {
        Compute { a, b } -> u256 { a + b }
    }
}
"#
                .to_string(),
            ),
        );
        let file = db.workspace().get(&db, &file_url).expect("file");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("compile");

        let (_module, origins) =
            compile_runtime_package_sonatina_with_origins(&db, &package, crate::EVM_LAYOUT)
                .expect("sonatina compile");

        assert!(
            !origins.is_empty(),
            "origin map must contain entries mapping Sonatina insts to MIR origins"
        );

        let smir_origins = origins
            .iter()
            .filter(|(_, _, o)| o.level == common::provenance::IrLevel::Smir)
            .count();

        eprintln!(
            "Sonatina origin map: {} total entries, {} with SMIR origin",
            origins.len(),
            smir_origins
        );

        assert!(
            smir_origins > 0,
            "at least some Sonatina instructions should trace back through MIR to SMIR origins"
        );
    }

    #[test]
    fn erc20_sonatina_origin_coverage() {
        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(std::env::temp_dir().join("erc20_sonatina_origin.fe"))
            .expect("path");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(include_str!("../../../fe/tests/fixtures/fe_test/erc20.fe").to_string()),
        );
        let file = db.workspace().get(&db, &file_url).expect("file");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("compile");

        let (_module, origins) =
            compile_runtime_package_sonatina_with_origins(&db, &package, crate::EVM_LAYOUT)
                .expect("sonatina compile");

        assert!(
            origins.len() > 100,
            "ERC20 should produce many Sonatina instructions with origins, got {}",
            origins.len()
        );

        // Count distinct functions represented
        let distinct_funcs: std::collections::HashSet<_> =
            origins.iter().map(|(func_ref, _, _)| func_ref).collect();

        eprintln!(
            "ERC20 Sonatina origins: {} insts across {} functions",
            origins.len(),
            distinct_funcs.len()
        );

        assert!(
            distinct_funcs.len() > 5,
            "origins should span multiple functions"
        );
    }

    #[test]
    fn end_to_end_origin_chain_resolves_to_source() {
        use common::provenance::IrLevel;
        use hir::span::LazySpan;

        // Known source with a specific expression we can identify
        let source = r#"
msg Msg {
    #[selector = 0x11111111]
    Add { a: u256, b: u256 } -> u256,
}

pub contract Calculator {
    recv Msg {
        Add { a, b } -> u256 { a + b }
    }
}
"#;
        let mut db = DriverDataBase::default();
        let file_url =
            Url::from_file_path(std::env::temp_dir().join("e2e_origin_chain.fe")).expect("path");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db.workspace().get(&db, &file_url).expect("file");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("compile");

        // Get the Sonatina origin map
        let (_module, sonatina_origins) =
            compile_runtime_package_sonatina_with_origins(&db, &package, crate::EVM_LAYOUT)
                .expect("sonatina compile");

        // Collect all MIR functions and their origin chains — categorize failures
        let mut resolved_chains = Vec::new();
        let mut skip_synthetic_func = 0;
        let mut skip_no_hir_body = 0;
        let mut skip_synthetic_origin = 0;
        let mut skip_body_origin = 0;
        let mut skip_span_none = 0;
        let mut total_stmts = 0;

        for func in package.functions(&db) {
            let body = func.instance(&db).body(&db);
            let key = func.instance(&db).key(&db);
            let semantic = match key.semantic(&db) {
                Some(s) => s,
                None => {
                    for block in &body.blocks {
                        skip_synthetic_func += block.stmt_origins.len();
                        total_stmts += block.stmt_origins.len();
                    }
                    continue;
                }
            };

            let owner = semantic.key(&db).owner(&db);
            let hir_body = match owner.body(&db) {
                Some(b) => b,
                None => {
                    for block in &body.blocks {
                        skip_no_hir_body += block.stmt_origins.len();
                        total_stmts += block.stmt_origins.len();
                    }
                    continue;
                }
            };

            for (block_idx, block) in body.blocks.iter().enumerate() {
                for (stmt_idx, origin) in block.stmt_origins.iter().enumerate() {
                    total_stmts += 1;

                    if origin.transform == common::provenance::TransformTag::Synthetic {
                        skip_synthetic_origin += 1;
                        continue;
                    }

                    if origin.level != IrLevel::Smir {
                        skip_body_origin += 1;
                        continue;
                    }

                    let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
                    let lazy_span = expr_id.span(hir_body);
                    match lazy_span.resolve(&db) {
                        Some(span) => {
                            let source_text = span.file.text(&db);
                            let start: usize = span.range.start().into();
                            let end: usize = span.range.end().into();
                            let snippet = if end <= source_text.len() {
                                &source_text[start..end]
                            } else {
                                "<out of bounds>"
                            };
                            resolved_chains.push((
                                func.symbol(&db),
                                block_idx,
                                stmt_idx,
                                snippet.to_string(),
                            ));
                        }
                        None => {
                            skip_span_none += 1;
                        }
                    }
                }
            }
        }

        assert!(
            !resolved_chains.is_empty(),
            "at least some MIR statements should resolve through the full origin \
             chain back to source text"
        );

        eprintln!("\n=== End-to-End Origin Chain Resolution ===");
        for (func, block, stmt, snippet) in &resolved_chains {
            eprintln!(
                "  {func} bb{block}[{stmt}] → \"{}\"",
                snippet.chars().take(60).collect::<String>()
            );
        }
        eprintln!("Total MIR statements: {total_stmts}");
        eprintln!("  Resolved to source:   {}", resolved_chains.len());
        eprintln!("  Synthetic functions:  {skip_synthetic_func}");
        eprintln!("  No HIR body:          {skip_no_hir_body}");
        eprintln!("  Synthetic origin:     {skip_synthetic_origin}");
        eprintln!("  Body-level origin:    {skip_body_origin}");
        eprintln!("  LazySpan→None:        {skip_span_none}");
        let accounted = resolved_chains.len()
            + skip_synthetic_func
            + skip_no_hir_body
            + skip_synthetic_origin
            + skip_body_origin
            + skip_span_none;
        eprintln!("  Accounted:            {accounted}/{total_stmts}");
        eprintln!("==========================================\n");

        // Verify Sonatina origins also chain back
        let mut sonatina_to_source = 0;
        for (_func_ref, _inst_id, mir_origin) in &sonatina_origins {
            if mir_origin.level != IrLevel::Smir {
                continue;
            }
            // The mir_origin points to an SMIR ExprId — we already know
            // some of these resolve. Count them.
            sonatina_to_source += 1;
        }

        assert!(
            sonatina_to_source > 0,
            "at least some Sonatina instructions should trace back through \
             MIR to SMIR to HIR to source"
        );

        eprintln!("Sonatina→source chains: {sonatina_to_source}");
    }

    #[test]
    fn ir_describe_hash_consumer_on_real_mir() {
        use common::hash_consumer::HashConsumer;
        use common::ir_describe::{DescribeCtx, IrDescribe};

        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(std::env::temp_dir().join("ir_describe_hash_test.fe"))
            .expect("path");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
msg Msg {
    #[selector = 0x11111111]
    Compute { a: u256, b: u256 } -> u256,
}
pub contract C {
    recv Msg {
        Compute { a, b } -> u256 { a + b }
    }
}
"#
                .to_string(),
            ),
        );
        let file = db.workspace().get(&db, &file_url).expect("file");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("compile");

        let cx = DescribeCtx::new(&db);
        let mut function_hashes = Vec::new();

        for func in package.functions(&db) {
            let body = func.instance(&db).body(&db);
            let mut consumer = HashConsumer::new();
            body.describe(&cx, &mut consumer);
            if let Some(hash) = consumer.into_result() {
                function_hashes.push((func.symbol(&db), hash));
            }
        }

        assert!(
            !function_hashes.is_empty(),
            "should hash at least one function"
        );

        // Hash the same function twice — should be deterministic
        let first_func = &package.functions(&db)[0];
        let body = first_func.instance(&db).body(&db);

        let mut c1 = HashConsumer::new();
        body.describe(&cx, &mut c1);
        let mut c2 = HashConsumer::new();
        body.describe(&cx, &mut c2);

        assert_eq!(
            c1.result().unwrap().structure(),
            c2.result().unwrap().structure(),
            "same function should hash identically"
        );

        eprintln!(
            "IrDescribe → HashConsumer: {} functions hashed",
            function_hashes.len()
        );
        for (name, hash) in &function_hashes[..std::cmp::min(5, function_hashes.len())] {
            eprintln!("  {name}: structure={:#x}", hash.structure());
        }
    }

    #[test]
    fn ir_describe_hash_detects_code_change() {
        use common::hash_consumer::HashConsumer;
        use common::ir_describe::{DescribeCtx, IrDescribe};

        fn compile_and_hash(db: &mut DriverDataBase, source: &str) -> Vec<(String, u128)> {
            let file_url = Url::from_file_path(std::env::temp_dir().join("hash_change_test.fe"))
                .expect("path");
            db.workspace()
                .touch(db, file_url.clone(), Some(source.to_string()));
            let file = db.workspace().get(db, &file_url).expect("file");
            let top_mod = db.top_mod(file);
            let package = build_runtime_package(db, top_mod).expect("compile");
            let cx = DescribeCtx::new(db);

            package
                .functions(db)
                .iter()
                .map(|func| {
                    let body = func.instance(db).body(db);
                    let mut consumer = HashConsumer::new();
                    body.describe(&cx, &mut consumer);
                    (
                        func.symbol(db),
                        consumer.into_result().map(|h| h.structure()).unwrap_or(0),
                    )
                })
                .collect()
        }

        let mut db1 = DriverDataBase::default();
        let h1 = compile_and_hash(
            &mut db1,
            r#"
msg Msg {
    #[selector = 0x11111111]
    Compute { a: u256, b: u256 } -> u256,
}
pub contract C {
    recv Msg {
        Compute { a, b } -> u256 { a + b }
    }
}
"#,
        );

        let mut db2 = DriverDataBase::default();
        let h2 = compile_and_hash(
            &mut db2,
            r#"
msg Msg {
    #[selector = 0x11111111]
    Compute { a: u256, b: u256 } -> u256,
}
pub contract C {
    recv Msg {
        Compute { a, b } -> u256 { a - b }
    }
}
"#,
        );

        // Find the recv function in both
        let recv1 = h1.iter().find(|(n, _)| n.contains("recv_0_0"));
        let recv2 = h2.iter().find(|(n, _)| n.contains("recv_0_0"));

        assert!(
            recv1.is_some() && recv2.is_some(),
            "should find recv function"
        );

        assert_ne!(
            recv1.unwrap().1,
            recv2.unwrap().1,
            "changing + to - must change the structural hash"
        );

        // Non-recv functions should mostly be identical
        let matching: usize = h1
            .iter()
            .filter(|(n1, hash1)| h2.iter().any(|(n2, hash2)| n1 == n2 && hash1 == hash2))
            .count();

        assert!(
            matching > 0,
            "some functions should be unchanged between + and -"
        );

        eprintln!(
            "Hash change detection: {}/{} functions unchanged",
            matching,
            h1.len()
        );
    }

    #[test]
    fn erc20_end_to_end_origin_chain() {
        use common::provenance::IrLevel;
        use hir::span::LazySpan;

        let mut db = DriverDataBase::default();
        let file_url =
            Url::from_file_path(std::env::temp_dir().join("erc20_e2e_origin.fe")).expect("path");
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(include_str!("../../../fe/tests/fixtures/fe_test/erc20.fe").to_string()),
        );
        let file = db.workspace().get(&db, &file_url).expect("file");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("compile");

        let mut total = 0;
        let mut resolved = 0;
        let mut synthetic_func = 0;
        let mut synthetic_origin = 0;
        let mut span_none = 0;

        for func in package.functions(&db) {
            let body = func.instance(&db).body(&db);
            let key = func.instance(&db).key(&db);
            let semantic = match key.semantic(&db) {
                Some(s) => s,
                None => {
                    for b in &body.blocks {
                        synthetic_func += b.stmt_origins.len();
                        total += b.stmt_origins.len();
                    }
                    continue;
                }
            };
            let owner = semantic.key(&db).owner(&db);
            let hir_body = match owner.body(&db) {
                Some(b) => b,
                None => continue,
            };

            for block in &body.blocks {
                for origin in &block.stmt_origins {
                    total += 1;
                    if origin.transform == common::provenance::TransformTag::Synthetic {
                        synthetic_origin += 1;
                        continue;
                    }
                    if origin.level != IrLevel::Smir {
                        continue;
                    }

                    let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
                    match expr_id.span(hir_body).resolve(&db) {
                        Some(_) => resolved += 1,
                        None => span_none += 1,
                    }
                }
            }
        }

        eprintln!("\n=== ERC20 Origin Coverage ===");
        eprintln!(
            "Total: {total}, Resolved: {resolved}, Synthetic func: {synthetic_func}, Synthetic origin: {synthetic_origin}, Span None: {span_none}"
        );
        let coverage = resolved as f64 / total as f64 * 100.0;
        eprintln!("User-code coverage: {coverage:.0}%");
        eprintln!("============================\n");

        assert!(
            resolved > 500,
            "ERC20 should resolve many origins, got {resolved}"
        );
        assert_eq!(span_none, 0, "zero LazySpan failures expected");
    }
}
