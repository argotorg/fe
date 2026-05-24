pub mod dwarf;
pub mod model;

pub use dwarf::{DwarfLineRow, DwarfLineTable, emit_dwarf_line_table};
pub use model::{
    AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugBundle, DebugCodeObject,
    DebugFunction, DebugGasRecord, DebugInstruction, DebugLocationRange, DebugScope,
    DebugSourceFile, DebugSourceSpan, DebugType, DebugVariable, InstructionClassification,
    build_debug_bundle,
};
