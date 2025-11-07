pub mod ir;
mod lower;

pub use ir::{
    BasicBlockId, MirBody, MirFunction, MirInst, MirModule, Terminator, ValueData, ValueId,
    ValueOrigin,
};
pub use lower::{lower_module, MirLowerError, MirLowerResult};
