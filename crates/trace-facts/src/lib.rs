pub mod evm_trace;
pub mod fact;
pub mod jsonl;
pub mod relation;
pub mod snapshot;
pub mod validate;

extern crate self as trace_facts;

pub use common::origin::OriginExportKey;
pub use evm_trace::{
    EvmExecutionStep, EvmExecutionTrace, EvmExecutionTraceError, RuntimeImportConfig,
    dynamic_gas_facts_from_evm_trace, runtime_facts_from_debug_trace_json,
    runtime_facts_from_eip3155_jsonl, runtime_facts_from_evm_execution_trace,
};
pub use fact::{
    BlockFact, CallFact, CategorySource, CfgEdgeFact, CfgEdgeKind, CodeObjectFact, CodeObjectKind,
    CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason, DisplayNameFact,
    DisplayNameKind, DynamicGasKind, DynamicGasStepFact, EvmSchedule, ExecutionStepFact,
    ExecutionTraceSessionFact, FunctionFact, GasConfidence, GasCostFact, GasKind, GasSource,
    InlineContextFact, InstructionBlockFact, InstructionCategory, InstructionCategoryFact,
    InstructionExtentFact, InstructionFact, LexicalScopeFact, LocationConfidence, LocationExpr,
    LocationRangeFact, LogFact, LoopBlockFact, LoopBlockRole, LoopConfidence, LoopDerivation,
    LoopFact, LoopMembershipFact, MemoryAccessFact, MemoryAccessKind, OpcodeCategory, OpcodeFact,
    OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind, PcRange,
    PrecompileInvocationFact, ReturnDataFact, ReturnDataKind, RevertFact, RuntimeCallKind,
    RuntimeCaptureMode, RuntimeCodeObjectBindingFact, RuntimePcJoinConfidence,
    RuntimeTraceDataSource, RuntimeValue, RuntimeValuePolicy, SelfdestructFact,
    ShapeComponentHashFact, ShapeGraphHashFact, ShapeNodeHashFact, ShapePolicyFact, SourceFileFact,
    SourceSpanFact, StackSampleFact, StaticGasFact, StorageAccessFact, StorageAccessKind,
    StorageFact, StorageLocation, StorageReason, TraceFact, TraceFactTextError, TypeFact,
    TypeField, TypeKind, ValueLocation, ValueProperty, ValuePropertyFact, VariableFact,
    VariableStorageClass, shape_hash_facts,
};
pub use jsonl::{
    JsonlTraceReadError, JsonlTraceReader, JsonlTraceSink, TRACE_SCHEMA_VERSION, TraceBundle,
    TraceDataSource, TraceJsonlRecord, TraceMetadata, TraceMetadataError, read_trace_bundle_jsonl,
    read_trace_facts_jsonl,
};
pub use relation::{
    OriginRef, RelationColumn, RelationColumnKind, RelationRow, RelationSchema, TraceFactSpec,
    TraceRelation, ValidationIssue, ValidationSeverity,
};
pub use snapshot::{TraceSnapshot, TraceSnapshotReadError};
pub use trace_facts_macros::TraceFactSpec;
pub use validate::{
    TraceValidationDiagnostic, TraceValidationError, TraceValidationInfo, TraceValidationLevel,
    TraceValidationReport, TraceValidationSummary, TraceValidationWarning, TraceValidator,
};
