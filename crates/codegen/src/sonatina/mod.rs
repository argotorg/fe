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
        let Some(package) = mir::build_ingot_module_runtime_package(db, top_mod)? else {
            continue;
        };
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
        let Some(package) = mir::build_ingot_module_runtime_package(db, top_mod)? else {
            continue;
        };
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
    assert!(f(0) == 1)
    assert!(f(1) == 2)
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
}
