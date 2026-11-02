use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::{build_native_test_runtime_package, format_runtime_package};
use sonatina_codegen::Compile;
use sonatina_cranelift::CraneliftObjectBackend;
use sonatina_ir::{Linkage, ir_writer::ModuleWriter};

use super::{
    LowerError, NativePanics, create_native_isa, debug_ensure_module_sonatina_ir_valid,
    format_cranelift_errors, lower_runtime, to_sonatina_opt_level,
};
use crate::{
    ExpectedRevert, OptLevel,
    test_output::{TestRootMetadataError, runtime_test_root_metadata},
};

#[derive(Debug)]
pub struct NativeTestEntry {
    pub name: String,
    pub symbol: String,
    pub expected_revert: Option<ExpectedRevert>,
}

/// One compiled object for every selected test in a suite.
#[derive(Debug)]
pub struct NativeTestSuite {
    pub tests: Vec<NativeTestEntry>,
    pub object: Vec<u8>,
    pub ir: Option<String>,
    pub rmir: Option<String>,
}

pub fn emit_test_suite_native(
    db: &DriverDataBase,
    top_mods: &[TopLevelMod<'_>],
    opt_level: OptLevel,
    filter: Option<&str>,
    emit_ir: bool,
    emit_rmir: bool,
) -> Result<Option<NativeTestSuite>, LowerError> {
    let package = build_native_test_runtime_package(db, top_mods, filter)?;
    if package.root_objects(db).is_empty() {
        return Ok(None);
    }
    let mut roots = Vec::new();
    for &object in package.root_objects(db) {
        for section in object.sections(db) {
            let metadata = runtime_test_root_metadata(db, section.entry.owner(db), &section.name)
                .map_err(|err| match err {
                TestRootMetadataError::InvalidPackage(message) => LowerError::Internal(message),
                TestRootMetadataError::Unsupported(message) => LowerError::Unsupported(message),
            })?;
            if metadata.initial_balance.is_some() {
                return Err(LowerError::Unsupported(format!(
                    "native test `{}` cannot use the EVM `balance` attribute",
                    metadata.display_name
                )));
            }
            roots.push((
                metadata.display_name,
                metadata.expected_revert,
                section.entry.instance(db),
            ));
        }
    }
    let isa = create_native_isa()?;
    let (module, functions) = lower_runtime::compile_runtime_package_sonatina_for_isa(
        db,
        &package,
        &isa,
        false,
        None,
        NativePanics::HostRevert,
    )?;
    let mut tests = Vec::with_capacity(roots.len());
    for (name, expected_revert, instance) in roots {
        let entry = functions[&instance];
        let symbol = module.ctx.func_sig(entry, |signature| {
            if !signature.args().is_empty() || !signature.ret_tys().is_empty() {
                return Err(LowerError::Internal(format!(
                    "native test wrapper `{name}` must have a void, parameterless signature"
                )));
            }
            Ok(signature.name().to_string())
        })?;
        module.ctx.update_func_linkage(entry, Linkage::Public);
        tests.push(NativeTestEntry {
            name,
            symbol,
            expected_revert,
        });
    }
    let mut compile = Compile::new(module, CraneliftObjectBackend::new())
        .with_opt_level(to_sonatina_opt_level(opt_level));
    compile.optimize();
    debug_ensure_module_sonatina_ir_valid(compile.module())?;
    let ir = emit_ir.then(|| ModuleWriter::new(compile.module()).dump_string());
    let object = compile
        .compile()
        .map(|artifact| artifact.into_bytes())
        .map_err(|errors| LowerError::Internal(format_cranelift_errors(&errors)))?;
    Ok(Some(NativeTestSuite {
        tests,
        object,
        ir,
        rmir: emit_rmir.then(|| format_runtime_package(db, &package)),
    }))
}
