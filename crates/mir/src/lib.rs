mod core_lib;
mod dedup;
pub mod fmt;
mod hash;
pub mod ir;
pub mod layout;
mod lower;
mod monomorphize;
mod transform;

pub use ir::{
    BasicBlockId, CallOrigin, LocalData, LocalId, LoopInfo, MirBody, MirFunction, MirInst,
    MirModule, MirProjection, MirProjectionPath, Rvalue, SwitchTarget, SwitchValue,
    TerminatingCall, Terminator, ValueData, ValueId, ValueOrigin,
};
pub use lower::{MirLowerError, MirLowerResult, lower_module};
