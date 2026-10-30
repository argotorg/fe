//! One WebAssembly module for each `#[test]` function.
//!
//! A test is lowered like a native test: a synthetic root without parameters
//! calls the function and traps if it fails. The root is the only export of the
//! module (besides the memory), so a host runs the test by calling it. A module
//! holds one test and what that test reaches, which means a test that uses
//! something the WebAssembly backend does not support fails alone, with an
//! error that names it.

use driver::DriverDataBase;
use mir::{TestFunc, build_wasm_test_runtime_package};
use sonatina_codegen::Compile;
use sonatina_ir::{Linkage, ir_writer::ModuleWriter};
use sonatina_wasm::WasmBackend;

use super::{
    LowerError, ensure_module_sonatina_ir_valid, lower_runtime, to_sonatina_opt_level,
    wasm::{create_wasm32_isa, lower_wasm_errors},
};
use crate::{
    OptLevel,
    test_output::{TestRootMetadataError, runtime_test_root_metadata},
};

/// A compiled test.
#[derive(Debug)]
pub struct WasmTestModule {
    /// The export that runs the test.
    pub entry: String,
    pub wasm: Vec<u8>,
    pub ir: Option<String>,
}

/// Compiles one test to a module. An attribute that has no meaning outside the
/// EVM (`should_revert`, `balance`) is an error, as it is for native tests.
pub fn emit_test_wasm(
    db: &DriverDataBase,
    test: &TestFunc<'_>,
    opt_level: OptLevel,
    emit_ir: bool,
) -> Result<WasmTestModule, LowerError> {
    let package = build_wasm_test_runtime_package(db, test)?;
    let mut roots = Vec::new();
    for object in package.root_objects(db) {
        for section in object.sections(db) {
            let metadata = runtime_test_root_metadata(db, section.entry.owner(db), &section.name)
                .map_err(|err| match err {
                TestRootMetadataError::InvalidPackage(message) => LowerError::Internal(message),
                TestRootMetadataError::Unsupported(message) => LowerError::Unsupported(message),
            })?;
            if metadata.expected_revert.is_some() || metadata.initial_balance.is_some() {
                return Err(LowerError::Unsupported(format!(
                    "WebAssembly test `{}` cannot use EVM `should_revert` or `balance` attributes",
                    metadata.display_name
                )));
            }
            roots.push((metadata.display_name, section.entry.instance(db)));
        }
    }
    let [(name, instance)] = roots.as_slice() else {
        return Err(LowerError::Internal(format!(
            "the package of test `{}` has {} test roots instead of one",
            test.name,
            roots.len()
        )));
    };

    let isa = create_wasm32_isa();
    let (module, functions) = lower_runtime::compile_runtime_package_sonatina_for_isa(
        db,
        &package,
        &isa,
        false,
        &[],
        lower_runtime::NativePanics::Trap,
    )?;
    let entry = functions[instance];
    let symbol = module.ctx.func_sig(entry, |signature| {
        if !signature.args().is_empty() || !signature.ret_tys().is_empty() {
            return Err(LowerError::Internal(format!(
                "WebAssembly test wrapper `{name}` must have a void, parameterless signature"
            )));
        }
        Ok(signature.name().to_string())
    })?;
    module.ctx.update_func_linkage(entry, Linkage::Public);

    let mut compile =
        Compile::new(module, WasmBackend::new()).with_opt_level(to_sonatina_opt_level(opt_level));
    compile.optimize();
    ensure_module_sonatina_ir_valid(compile.module())?;
    let ir = emit_ir.then(|| ModuleWriter::new(compile.module()).dump_string());
    let wasm = compile.compile().map_err(lower_wasm_errors)?.into_bytes();
    Ok(WasmTestModule {
        entry: symbol,
        wasm,
        ir,
    })
}
