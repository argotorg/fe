mod core_lib;
mod dedup;
mod hash;
pub mod ir;
pub mod layout;
mod lower;
mod monomorphize;
mod transform;

pub use ir::{
    BasicBlockId, CallOrigin, DecisionTreeBinding, LocalData, LocalId, LoopInfo, MatchArmLowering,
    MatchArmPattern, MatchLoweringInfo, MirBody, MirFunction, MirInst, MirModule, MirProjection,
    MirProjectionPath, SwitchOrigin, SwitchTarget, SwitchValue, Terminator, ValueData, ValueId,
    ValueOrigin,
};
pub use lower::{MirLowerError, MirLowerResult, lower_module};
