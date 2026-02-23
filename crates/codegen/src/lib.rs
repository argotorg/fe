mod backend;
mod sonatina;
mod yul;

pub use backend::{
    Backend, BackendError, BackendKind, BackendOutput, OptLevel, SonatinaBackend, YulBackend,
};
pub use sonatina::{
    DebugOutputSink, LowerError, SonatinaContractBytecode, SonatinaTestDebugConfig,
    emit_ingot_sonatina_bytecode, emit_mir_module_sonatina_ir_optimized,
    emit_module_sonatina_bytecode, emit_module_sonatina_ir, emit_module_sonatina_ir_optimized,
    emit_test_module_sonatina, validate_module_sonatina_ir,
};
pub use yul::{
    EmitModuleError, ExpectedRevert, TestMetadata, TestModuleOutput, YulError, emit_ingot_yul,
    emit_ingot_yul_with_layout, emit_module_yul, emit_module_yul_with_layout, emit_test_module_yul,
    emit_test_module_yul_with_layout,
};
