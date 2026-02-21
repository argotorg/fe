pub mod analysis;
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

pub use ir::{
    BasicBlockId, CallOrigin, LocalData, LocalId, LoopInfo, MirBackend, MirBody,
    MirFunction, MirInst, MirModule, MirProjection, MirProjectionPath, MirStage, Rvalue,
    SwitchTarget, SwitchValue, TerminatingCall, Terminator, ValueData, ValueId, ValueOrigin,
    ValueRepr,
};
pub use lower::{
    MirDiagnosticsMode, MirDiagnosticsOutput, MirLowerError, MirLowerResult,
    collect_mir_diagnostics, lower_ingot, lower_module,
};
