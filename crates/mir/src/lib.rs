pub mod ir;
mod lower;

pub use ir::{
    BasicBlockId, CallOrigin, LoopInfo, MirBody, MirFunction, MirInst, MirModule, Terminator,
    ValueData, ValueId, ValueOrigin,
};
pub use lower::{MirLowerError, MirLowerResult, lower_module};
