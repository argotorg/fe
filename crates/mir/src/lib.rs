mod core_lib;
mod dedup;
mod hash;
pub mod ir;
pub mod layout;
mod lower;
mod monomorphize;

pub use ir::{
    BasicBlockId, CallOrigin, DecisionTreeBinding, LoopInfo, MatchArmLowering, MatchArmPattern,
    MatchLoweringInfo, MirBody, MirFunction, MirInst, MirModule, MirProjection, MirProjectionPath,
    SwitchOrigin, SwitchTarget, SwitchValue, Terminator, ValueData, ValueId, ValueOrigin,
};
pub use lower::{MirLowerError, MirLowerResult, lower_module};
