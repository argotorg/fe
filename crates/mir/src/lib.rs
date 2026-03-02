pub mod analysis;
mod capability_space;
mod core_lib;
mod dedup;
pub mod fmt;
mod hash;
pub mod ir;
pub mod layout;
mod lower;
mod monomorphize;
pub mod repr;
mod transform;
mod ty;

pub use core_lib::CoreLib;
pub use ir::{
    BasicBlockId, CallOrigin, ConstRegion, ConstRegionId, LocalData, LocalId, LoopInfo, MirBackend,
    MirBody, MirFunction, MirInst, MirModule, MirProjection, MirProjectionPath, MirStage, Rvalue,
    SwitchTarget, SwitchValue, TerminatingCall, Terminator, ValueData, ValueId, ValueOrigin,
    ValueRepr,
};
pub use lower::{
    MirDiagnosticsMode, MirDiagnosticsOutput, MirLowerError, MirLowerResult,
    collect_mir_diagnostics, lower_ingot, lower_module,
};
pub use transform::prepare_module_for_evm_yul_codegen;
