pub mod fact;
pub mod jsonl;
pub mod validate;

pub use fact::{
    CategorySource, CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason,
    InlineContextFact, InstructionCategory, InstructionCategoryFact, InstructionFact,
    LoopDerivation, LoopMembershipFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact,
    OriginNodeKind, StorageFact, StorageLocation, StorageReason, TraceFact, TraceFactTextError,
};
pub use jsonl::{JsonlTraceReadError, JsonlTraceSink, read_trace_facts_jsonl};
pub use validate::{TraceValidationError, TraceValidationSummary, TraceValidator};
