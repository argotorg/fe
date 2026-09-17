mod backend;
pub mod debug;
mod function_symbols;
mod runtime_package;
mod sonatina;
mod test_output;
pub mod trace;

pub use backend::OptLevel;
pub use debug::{
    BytecodeDebugError, BytecodePcRange, BytecodeSourceMapEntry, BytecodeSourceMapEntryKind,
};
pub use sonatina::{
    LowerError, SonatinaContractBytecode, SonatinaTestOptions, compile_runtime_package_sonatina,
    emit_ingot_sonatina_bytecode, emit_ingot_sonatina_ir, emit_ingot_sonatina_ir_optimized,
    emit_module_sonatina_bytecode, emit_module_sonatina_bytecode_with_observability,
    emit_module_sonatina_ir, emit_module_sonatina_ir_optimized, emit_runtime_package_sonatina_ir,
    emit_runtime_package_sonatina_ir_optimized, emit_test_ingot_sonatina,
    emit_test_module_sonatina, validate_module_sonatina_ir,
};
#[cfg(feature = "cranelift")]
pub use sonatina::{
    NativeArtifacts, NativeOutputSelection, NativeTestEntry, NativeTestModule,
    emit_module_native_artifacts, emit_module_native_ir, emit_module_native_object,
    emit_test_module_native,
};
pub use test_output::{ExpectedRevert, TestMetadata, TestModuleOutput, parse_expected_revert};
