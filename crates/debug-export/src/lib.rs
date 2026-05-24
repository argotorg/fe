pub mod dwarf;
pub mod ethdebug;
pub mod model;

pub use dwarf::{DwarfLineRow, DwarfLineTable, emit_dwarf_line_table};
pub use ethdebug::{
    ETHDEBUG_SCHEMA_VERSION, EthdebugArtifact, EthdebugByteRange, EthdebugCompilation,
    EthdebugCompiler, EthdebugContract, EthdebugEnvironment, EthdebugInstruction,
    EthdebugInstructionContext, EthdebugOperation, EthdebugProgram, EthdebugReference,
    EthdebugSourceMaterial, EthdebugSourceRange, emit_ethdebug_artifact, pinned_ethdebug_schema,
    validate_ethdebug_artifact,
};
pub use model::{
    AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugBundle, DebugCodeObject,
    DebugFunction, DebugGasRecord, DebugInstruction, DebugLocationRange, DebugScope,
    DebugSourceFile, DebugSourceSpan, DebugType, DebugVariable, InstructionClassification,
    build_debug_bundle,
};
