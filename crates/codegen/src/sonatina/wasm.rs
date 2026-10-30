//! WebAssembly output for programs whose exports are functions of numbers and
//! text.
//!
//! The module is lowered by the same lowerer as host-native and SP1 output,
//! for the `wasm32-unknown-native` target: one linear address space and
//! 32-bit pointers. The exports are the `pub fn` items of the file; their
//! bodies may use aggregates, constants and dynamic memory, which the backend
//! keeps in the linear memory. An export may take `std::wasm::HostText` parameters
//! and return a `std::wasm::Reply`; a wrapper converts them to and from the
//! numbers of the boundary, and a module with a text parameter exports the
//! allocator through which the host places the text.

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::{
    WasmExport, WasmExportPackage, WasmParamShape, WasmResultShape, build_wasm_export_package,
};
use sonatina_codegen::Compile;
use sonatina_ir::{ir_writer::ModuleWriter, isa::wasm32::Wasm32};
use sonatina_triple::{Architecture, OperatingSystem, TargetTriple, Vendor};
use sonatina_wasm::{WasmBackend, WasmError};

use super::{LowerError, ensure_module_sonatina_ir_valid, lower_runtime, to_sonatina_opt_level};
use crate::OptLevel;

#[derive(Debug, Clone, Copy, Default)]
pub struct WasmOutputSelection {
    pub ir: bool,
    pub wasm: bool,
}

/// The version of the contract between a module and its host: the module
/// `env` for imports, the reply word, and the allocator protocol of a module
/// with text parameters (`std::wasm`). A host refuses a module that names a
/// version it does not know.
pub const WASM_HOST_CONTRACT_VERSION: u32 = 1;

/// The name that a manifest gives to `std::wasm::HostText`.
pub const WASM_TEXT_TYPE: &str = "HostText";
/// The name that a manifest gives to `std::wasm::Reply`.
pub const WASM_REPLY_TYPE: &str = "Reply";

/// A parameter of an export, as the manifest describes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmParamInfo {
    pub name: String,
    /// The Fe type, or `HostText`.
    pub ty: String,
    pub shape: WasmParamShape,
}

/// An exported Fe function, as the manifest describes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmExportInfo {
    pub name: String,
    pub params: Vec<WasmParamInfo>,
    /// The Fe type of the result, or `Reply`; `None` for no result.
    pub result: Option<(String, WasmResultShape)>,
}

#[derive(Debug)]
pub struct WasmArtifacts {
    pub ir: Option<String>,
    pub wasm: Option<Vec<u8>>,
    /// The exported Fe functions in name order. The module also exports its
    /// memory and, when an export takes text, the allocator.
    pub exports: Vec<WasmExportInfo>,
}

pub(super) fn create_wasm32_isa() -> Wasm32 {
    Wasm32::new(TargetTriple::new(
        Architecture::Wasm32,
        Vendor::Unknown,
        OperatingSystem::Native,
    ))
}

/// Joins the errors of the backend. Input that it does not translate is the
/// user's to change; any other failure is a compiler fault.
pub(super) fn lower_wasm_errors(errors: Vec<WasmError>) -> LowerError {
    let unsupported = errors
        .iter()
        .all(|error| matches!(error, WasmError::Unsupported(_)));
    if unsupported {
        let messages = errors.into_iter().map(|error| match error {
            WasmError::Unsupported(message) => message,
            other => other.to_string(),
        });
        LowerError::Unsupported(messages.collect::<Vec<_>>().join("; "))
    } else {
        let messages = errors.iter().map(ToString::to_string);
        LowerError::Internal(messages.collect::<Vec<_>>().join("; "))
    }
}

pub fn emit_module_wasm_artifacts(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    outputs: WasmOutputSelection,
) -> Result<WasmArtifacts, LowerError> {
    let WasmExportPackage { package, exports } = build_wasm_export_package(db, top_mod)?;
    let isa = create_wasm32_isa();
    let adapters = exports
        .iter()
        .filter(|export| export.needs_adapter())
        .map(|export| lower_runtime::WasmAdapter {
            instance: export.instance,
            name: export.name.clone(),
            params: export.params.clone(),
            result: export.result,
        })
        .collect::<Vec<_>>();
    let scalar_exports = exports
        .iter()
        .filter(|export| !export.needs_adapter())
        .map(|export| (export.instance, export.name.as_str()))
        .collect::<Vec<_>>();
    let module = lower_runtime::compile_wasm_package_sonatina(
        db,
        &package,
        &isa,
        &scalar_exports,
        &adapters,
    )?;
    let backend = if exports.iter().any(WasmExport::takes_text) {
        WasmBackend::new().with_host_allocator()
    } else {
        WasmBackend::new()
    };

    let mut compile =
        Compile::new(module, backend).with_opt_level(to_sonatina_opt_level(opt_level));
    compile.optimize();
    ensure_module_sonatina_ir_valid(compile.module())?;
    let ir = outputs
        .ir
        .then(|| ModuleWriter::new(compile.module()).dump_string());
    let wasm = if outputs.wasm {
        let artifact = compile.compile().map_err(lower_wasm_errors)?;
        Some(artifact.into_bytes())
    } else {
        None
    };
    let exports = export_infos(db, &exports);
    Ok(WasmArtifacts { ir, wasm, exports })
}

fn export_infos(db: &DriverDataBase, exports: &[WasmExport<'_>]) -> Vec<WasmExportInfo> {
    exports
        .iter()
        .map(|export| {
            let params = export
                .params
                .iter()
                .enumerate()
                .map(|(idx, &shape)| {
                    let name = export
                        .func
                        .params(db)
                        .nth(idx)
                        .and_then(|view| view.name(db))
                        .map_or_else(|| format!("arg{idx}"), |ident| ident.data(db).to_string());
                    let ty = match shape {
                        WasmParamShape::Text => WASM_TEXT_TYPE.to_string(),
                        WasmParamShape::Scalar => {
                            export.func.arg_ty(db, idx).map_or_else(String::new, |ty| {
                                ty.instantiate_identity().pretty_print(db).to_string()
                            })
                        }
                    };
                    WasmParamInfo { name, ty, shape }
                })
                .collect();
            let result = match export.result {
                WasmResultShape::Unit => None,
                WasmResultShape::Reply => Some((WASM_REPLY_TYPE.to_string(), export.result)),
                WasmResultShape::Scalar => Some((
                    export.func.return_ty(db).pretty_print(db).to_string(),
                    export.result,
                )),
            };
            WasmExportInfo {
                name: export.name.clone(),
                params,
                result,
            }
        })
        .collect()
}
