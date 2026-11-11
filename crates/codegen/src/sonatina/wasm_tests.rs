//! WebAssembly modules for `#[test]` functions.
//!
//! A test is lowered like a native test: a synthetic root without parameters
//! calls the function and traps if it fails. The roots are the only exports of
//! a module (besides the memory), so a host runs a test by calling its root. A
//! module holds the tests it was built for and what they reach.

use driver::DriverDataBase;
use mir::{TestFunc, build_wasm_test_runtime_package};
use sonatina_codegen::Compile;
use sonatina_ir::{Linkage, ir_writer::ModuleWriter};
use sonatina_wasm::WasmBackend;

use super::{
    LowerError, ensure_module_sonatina_ir_valid, lower_runtime, remove_unreachable_functions,
    to_sonatina_opt_level,
    wasm::{create_wasm32_isa, lower_wasm_errors},
};
use crate::{
    OptLevel,
    test_output::{ExpectedRevert, runtime_test_root_metadata},
};

/// Compiled tests.
#[derive(Debug)]
pub struct WasmTestModule {
    /// The export that runs each test and the revert it expects, in the order
    /// of the tests.
    pub entries: Vec<(String, Option<ExpectedRevert>)>,
    pub wasm: Vec<u8>,
    pub ir: Option<String>,
}

/// Compiles `tests` to one module. A panic or revert with data calls the test
/// host's `__fe_revert`, as in a native test. The `balance` attribute has no
/// meaning outside the EVM and is an error, as is anything else the backend
/// cannot build: the caller builds fewer tests at once to find the test at
/// fault.
pub fn emit_test_wasm(
    db: &DriverDataBase,
    tests: &[TestFunc<'_>],
    opt_level: OptLevel,
    emit_ir: bool,
) -> Result<WasmTestModule, LowerError> {
    let package = build_wasm_test_runtime_package(db, tests)?;
    let mut roots = Vec::new();
    for object in package.root_objects(db) {
        for section in object.sections(db) {
            let metadata = runtime_test_root_metadata(db, section.entry.owner(db))?;
            if metadata.initial_balance.is_some() {
                return Err(LowerError::Unsupported(format!(
                    "WebAssembly test `{}` cannot use the EVM `balance` attribute",
                    metadata.name
                )));
            }
            roots.push((
                metadata.name,
                metadata.expected_revert,
                section.entry.instance(db),
            ));
        }
    }
    if roots.len() != tests.len() {
        return Err(LowerError::Internal(format!(
            "the package of {} tests has {} test roots",
            tests.len(),
            roots.len()
        )));
    }

    let isa = create_wasm32_isa();
    let (module, functions) = lower_runtime::compile_runtime_package_sonatina_for_isa(
        db,
        &package,
        &isa,
        false,
        &[],
        lower_runtime::NativePanics::HostRevertOrTrap,
    )?;
    let mut entries = Vec::with_capacity(roots.len());
    for (name, expected_revert, instance) in roots {
        let entry = functions[&instance];
        let symbol = module.ctx.func_sig(entry, |signature| {
            if !signature.args().is_empty() || !signature.ret_tys().is_empty() {
                return Err(LowerError::Internal(format!(
                    "WebAssembly test wrapper `{name}` must have a void, parameterless signature"
                )));
            }
            Ok(signature.name().to_string())
        })?;
        module.ctx.update_func_linkage(entry, Linkage::Public);
        entries.push((symbol, expected_revert));
    }
    remove_unreachable_functions(&module);

    let mut compile =
        Compile::new(module, WasmBackend::new()).with_opt_level(to_sonatina_opt_level(opt_level));
    compile.optimize();
    ensure_module_sonatina_ir_valid(compile.module())?;
    let ir = emit_ir.then(|| ModuleWriter::new(compile.module()).dump_string());
    let wasm = compile.compile().map_err(lower_wasm_errors)?.into_bytes();
    Ok(WasmTestModule { entries, wasm, ir })
}
