mod backend;
pub mod debug;
mod function_symbols;
mod layout;
mod runtime_package;
mod sonatina;
mod test_output;

pub use backend::{Backend, BackendError, BackendKind, BackendOutput, OptLevel, SonatinaBackend};
pub use debug::{
    BytecodeDebugError, BytecodePcRange, BytecodeSourceMapEntry, BytecodeSourceMapEntryKind,
};
pub use layout::{DISCRIMINANT_SIZE_BYTES, EVM_LAYOUT, TargetDataLayout, WORD_SIZE_BYTES};
pub use sonatina::{
    LowerError, SonatinaContractBytecode, SonatinaTestOptions, emit_ingot_sonatina_bytecode,
    emit_ingot_sonatina_ir, emit_ingot_sonatina_ir_optimized, emit_module_sonatina_bytecode,
    emit_module_sonatina_ir, emit_module_sonatina_ir_optimized, emit_runtime_package_sonatina_ir,
    emit_runtime_package_sonatina_ir_optimized, emit_test_ingot_sonatina,
    emit_test_module_sonatina, validate_module_sonatina_ir,
};
pub use test_output::{ExpectedRevert, TestMetadata, TestModuleOutput, parse_expected_revert};
