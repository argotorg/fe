pub mod ir;
mod lower;
mod monomorphize;

pub use ir::{
    BasicBlockId, CallOrigin, LoopInfo, MatchArmLowering, MatchArmPattern, MatchLoweringInfo,
    MirBody, MirFunction, MirInst, MirModule, SwitchOrigin, SwitchTarget, SwitchValue, Terminator,
    ValueData, ValueId, ValueOrigin,
};
pub use lower::{MirLowerError, MirLowerResult, lower_module};
