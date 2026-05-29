use std::collections::BTreeMap;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use shape_address::{DimensionDigests, ShapeDimension};

use crate::fact::{
    BlockFact, CallFact, CfgEdgeFact, CodeObjectFact, CompilerEventFact, DisplayNameFact,
    DynamicGasStepFact, ExecutionStepFact, ExecutionTraceSessionFact, FunctionFact, GasCostFact,
    InlineContextFact, InstructionBlockFact, InstructionCategoryFact, InstructionExtentFact,
    InstructionFact, LexicalScopeFact, LocationRangeFact, LogFact, LoopBlockFact, LoopFact,
    LoopMembershipFact, MemoryAccessFact, OpcodeFact, OriginEdgeFact, OriginNodeFact,
    PrecompileInvocationFact, ReturnDataFact, RevertFact, RuntimeCodeObjectBindingFact,
    SelfdestructFact, ShapeComponentHashFact, ShapeGraphHashFact, ShapeNodeHashFact,
    ShapePolicyFact, SourceFileFact, SourceSpanFact, StackSampleFact, StaticGasFact,
    StorageAccessFact, StorageFact, TraceFact, TypeFact, ValuePropertyFact, VariableFact,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RelationColumnKind {
    Key,
    OptionalKey,
    Text,
    OptionalText,
    U32,
    U64,
    I64,
    List,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelationColumn {
    pub name: &'static str,
    pub kind: RelationColumnKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelationSchema {
    pub name: &'static str,
    pub columns: Vec<RelationColumn>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelationRow {
    pub relation: &'static str,
    pub values: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct RelationBundleData {
    pub trace_hash: String,
    pub schemas: Vec<RelationSchema>,
    pub rows: Vec<RelationRow>,
}

impl RelationBundleData {
    pub fn from_snapshot(snapshot: &crate::snapshot::TraceSnapshot) -> Self {
        Self::from_facts(snapshot.trace_hash(), snapshot.facts())
    }

    pub fn from_facts(trace_hash: impl Into<String>, facts: &[TraceFact]) -> Self {
        let mut schemas = BTreeMap::new();
        let mut rows = Vec::with_capacity(facts.len());

        for fact in facts {
            let schema = fact.base_relation_schema();
            schemas.entry(schema.name).or_insert(schema);
            rows.push(fact.base_relation_row());
        }

        Self {
            trace_hash: trace_hash.into(),
            schemas: schemas.into_values().collect(),
            rows,
        }
    }
}

pub trait TraceRelation {
    const NAME: &'static str;

    fn schema() -> RelationSchema;
    fn row(&self) -> RelationRow;

    fn primary_key(&self) -> Option<&OriginExportKey> {
        None
    }

    fn origin_refs(&self) -> Vec<OriginRef<'_>> {
        Vec::new()
    }

    fn local_validation(&self) -> Vec<ValidationIssue> {
        Vec::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OriginRef<'a> {
    pub field: &'static str,
    pub key: &'a OriginExportKey,
    pub required: bool,
    pub expected_kind: Option<&'static str>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ValidationSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidationIssue {
    pub severity: ValidationSeverity,
    pub code: &'static str,
    pub fact_type: &'static str,
    pub field: Option<&'static str>,
    pub message: String,
}

pub trait TraceFactSpec: Serialize {
    const TYPE_NAME: &'static str;
    const RELATION_NAME: &'static str;

    fn primary_key(&self) -> Option<&OriginExportKey>;
    fn origin_refs(&self) -> Vec<OriginRef<'_>>;
    fn relation_schema() -> RelationSchema;
    fn relation_row(&self) -> RelationRow;

    fn local_validation(&self) -> Vec<ValidationIssue> {
        Vec::new()
    }
}

impl<T> TraceRelation for T
where
    T: TraceFactSpec,
{
    const NAME: &'static str = T::RELATION_NAME;

    fn schema() -> RelationSchema {
        T::relation_schema()
    }

    fn row(&self) -> RelationRow {
        self.relation_row()
    }

    fn primary_key(&self) -> Option<&OriginExportKey> {
        TraceFactSpec::primary_key(self)
    }

    fn origin_refs(&self) -> Vec<OriginRef<'_>> {
        TraceFactSpec::origin_refs(self)
    }

    fn local_validation(&self) -> Vec<ValidationIssue> {
        TraceFactSpec::local_validation(self)
    }
}

macro_rules! trace_fact_registry {
    ($($variant:ident($ty:ty)),+ $(,)?) => {
        $(
            impl From<$ty> for TraceFact {
                fn from(fact: $ty) -> Self {
                    Self::$variant(fact)
                }
            }
        )+

        impl TraceFact {
            pub fn base_relation_name(&self) -> &'static str {
                match self {
                    $(Self::$variant(_) => <$ty as TraceRelation>::NAME,)+
                }
            }

            pub fn base_relation_schema(&self) -> RelationSchema {
                match self {
                    $(Self::$variant(_) => <$ty as TraceRelation>::schema(),)+
                }
            }

            pub fn base_relation_row(&self) -> RelationRow {
                match self {
                    $(Self::$variant(fact) => TraceRelation::row(fact),)+
                }
            }

            pub fn primary_key(&self) -> Option<&OriginExportKey> {
                match self {
                    $(Self::$variant(fact) => TraceRelation::primary_key(fact),)+
                }
            }

            pub fn origin_refs(&self) -> Vec<OriginRef<'_>> {
                match self {
                    $(Self::$variant(fact) => TraceRelation::origin_refs(fact),)+
                }
            }

            pub fn local_validation(&self) -> Vec<ValidationIssue> {
                match self {
                    $(Self::$variant(fact) => TraceRelation::local_validation(fact),)+
                }
            }
        }
    };
}

trace_fact_registry! {
    OriginNode(OriginNodeFact),
    OriginEdge(OriginEdgeFact),
    CompilerEvent(CompilerEventFact),
    Storage(StorageFact),
    Instruction(InstructionFact),
    InstructionCategory(InstructionCategoryFact),
    Block(BlockFact),
    CfgEdge(CfgEdgeFact),
    Loop(LoopFact),
    LoopBlock(LoopBlockFact),
    InstructionBlock(InstructionBlockFact),
    InstructionExtent(InstructionExtentFact),
    LoopMembership(LoopMembershipFact),
    InlineContext(InlineContextFact),
    Opcode(OpcodeFact),
    GasCost(GasCostFact),
    DisplayName(DisplayNameFact),
    ValueProperty(ValuePropertyFact),
    SourceFile(SourceFileFact),
    SourceSpan(SourceSpanFact),
    CodeObject(CodeObjectFact),
    Function(FunctionFact),
    LexicalScope(LexicalScopeFact),
    Type(TypeFact),
    Variable(VariableFact),
    LocationRange(LocationRangeFact),
    StaticGas(StaticGasFact),
    DynamicGasStep(DynamicGasStepFact),
    ExecutionTraceSession(ExecutionTraceSessionFact),
    RuntimeCodeObjectBinding(RuntimeCodeObjectBindingFact),
    ExecutionStep(ExecutionStepFact),
    StackSample(StackSampleFact),
    StorageAccess(StorageAccessFact),
    MemoryAccess(MemoryAccessFact),
    Call(CallFact),
    Log(LogFact),
    ReturnData(ReturnDataFact),
    Revert(RevertFact),
    PrecompileInvocation(PrecompileInvocationFact),
    Selfdestruct(SelfdestructFact),
    ShapePolicy(ShapePolicyFact),
    ShapeNodeHash(ShapeNodeHashFact),
    ShapeComponentHash(ShapeComponentHashFact),
    ShapeGraphHash(ShapeGraphHashFact),
}

macro_rules! cols {
    ($($name:literal : $kind:ident),+ $(,)?) => {
        vec![
            $(RelationColumn {
                name: $name,
                kind: RelationColumnKind::$kind,
            }),+
        ]
    };
}

macro_rules! schema {
    ($name:expr, [$($col:literal : $kind:ident),+ $(,)?]) => {
        RelationSchema {
            name: $name,
            columns: cols![$($col : $kind),+],
        }
    };
}

macro_rules! row {
    ($name:expr, [$($value:expr),* $(,)?]) => {
        RelationRow {
            relation: $name,
            values: vec![$($value),*],
        }
    };
}

impl TraceRelation for CompilerEventFact {
    const NAME: &'static str = "base_compiler_event";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "event": Key,
                "phase": Text,
                "kind": Text,
                "inputs": List,
                "outputs": List,
                "reason": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.event),
                value(&self.phase),
                value(&self.kind),
                key_list(&self.inputs),
                key_list(&self.outputs),
                self.reason
                    .as_ref()
                    .map_or_else(String::new, ToString::to_string),
            ]
        )
    }
}

impl TraceRelation for StorageFact {
    const NAME: &'static str = "base_storage";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["subject": Key, "phase": Text, "location": Text, "reason": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.subject),
                value(&self.phase),
                value(&self.location),
                value(&self.reason),
            ]
        )
    }
}

impl TraceRelation for InstructionCategoryFact {
    const NAME: &'static str = "base_instruction_category";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["instruction": Key, "category": Text, "source": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.instruction),
                value(&self.category),
                value(&self.source),
            ]
        )
    }
}

impl TraceRelation for BlockFact {
    const NAME: &'static str = "base_block";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "block": Key,
                "function": Key,
                "phase": Text,
                "ordinal": U32,
                "name": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.block),
                key(&self.function),
                value(&self.phase),
                self.ordinal.to_string(),
                self.name.clone().unwrap_or_default(),
            ]
        )
    }
}

impl TraceRelation for CfgEdgeFact {
    const NAME: &'static str = "base_cfg_edge";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "function": Key,
                "from_block": Key,
                "to_block": Key,
                "kind": Text,
                "condition_origin": OptionalKey,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.function),
                key(&self.from_block),
                key(&self.to_block),
                value(&self.kind),
                opt_key(self.condition_origin.as_ref()),
            ]
        )
    }
}

impl TraceRelation for LoopFact {
    const NAME: &'static str = "base_loop";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "loop": Key,
                "function": Key,
                "phase": Text,
                "header_block": Key,
                "derivation": Text,
                "confidence": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.loop_key),
                key(&self.function),
                value(&self.phase),
                key(&self.header_block),
                value(&self.derivation),
                value(&self.confidence),
            ]
        )
    }
}

impl TraceRelation for LoopBlockFact {
    const NAME: &'static str = "base_loop_block";

    fn schema() -> RelationSchema {
        schema!(Self::NAME, ["loop": Key, "block": Key, "role": Text])
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [key(&self.loop_key), key(&self.block), value(&self.role),]
        )
    }
}

impl TraceRelation for InstructionBlockFact {
    const NAME: &'static str = "base_instruction_block";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["instruction": Key, "block": Key, "phase": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [key(&self.instruction), key(&self.block), value(&self.phase),]
        )
    }
}

impl TraceRelation for LoopMembershipFact {
    const NAME: &'static str = "base_loop_member";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["loop": Key, "instruction": Key, "derived_from": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.loop_key),
                key(&self.instruction),
                value(&self.derived_from),
            ]
        )
    }
}

impl TraceRelation for InlineContextFact {
    const NAME: &'static str = "base_inline_context";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "inline_instance": Key,
                "caller_function": Key,
                "callee_function": Key,
                "callsite": Key,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.inline_instance),
                key(&self.caller_function),
                key(&self.callee_function),
                key(&self.callsite),
            ]
        )
    }
}

impl TraceRelation for OpcodeFact {
    const NAME: &'static str = "base_opcode";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "pc": Key,
                "opcode": Text,
                "immediate": OptionalText,
                "category": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.pc),
                self.opcode.clone(),
                self.immediate.clone().unwrap_or_default(),
                value(&self.category),
            ]
        )
    }
}

impl TraceRelation for GasCostFact {
    const NAME: &'static str = "base_gas_cost";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "subject": Key,
                "gas_kind": Text,
                "gas": U64,
                "schedule": Text,
                "confidence": Text,
                "source": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.subject),
                value(&self.gas_kind),
                self.gas.to_string(),
                self.schedule.to_string(),
                value(&self.confidence),
                value(&self.source),
            ]
        )
    }
}

impl TraceRelation for DisplayNameFact {
    const NAME: &'static str = "base_display_name";

    fn schema() -> RelationSchema {
        schema!(Self::NAME, ["subject": Key, "kind": Text, "name": Text])
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [key(&self.subject), value(&self.kind), self.name.clone()]
        )
    }
}

impl TraceRelation for ValuePropertyFact {
    const NAME: &'static str = "base_value_property";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "subject": Key,
                "phase": Text,
                "property": Text,
                "reason": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.subject),
                value(&self.phase),
                value(&self.property),
                self.reason
                    .as_ref()
                    .map_or_else(String::new, ToString::to_string),
            ]
        )
    }
}

impl TraceRelation for SourceFileFact {
    const NAME: &'static str = "base_source_file";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "file": Key,
                "uri": Text,
                "display_name": Text,
                "content_hash": Text,
                "source_id": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.file_key),
                self.uri.clone(),
                self.display_name.clone(),
                self.content_hash.clone(),
                self.source_id.map_or_else(String::new, |id| id.to_string()),
            ]
        )
    }
}

impl TraceRelation for SourceSpanFact {
    const NAME: &'static str = "base_source_span";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "origin": Key,
                "file": Key,
                "start_byte": U32,
                "end_byte": U32,
                "start_line": U32,
                "start_column": U32,
                "end_line": U32,
                "end_column": U32,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.origin),
                key(&self.file),
                self.start_byte.to_string(),
                self.end_byte.to_string(),
                self.start_line.to_string(),
                self.start_column.to_string(),
                self.end_line.to_string(),
                self.end_column.to_string(),
            ]
        )
    }
}

impl TraceRelation for CodeObjectFact {
    const NAME: &'static str = "base_code_object";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "code_object": Key,
                "kind": Text,
                "owner": OptionalKey,
                "target": Text,
                "code_hash": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.code_object),
                value(&self.kind),
                opt_key(self.owner_function_or_contract.as_ref()),
                self.target.clone(),
                self.code_hash.clone().unwrap_or_default(),
            ]
        )
    }
}

impl TraceRelation for FunctionFact {
    const NAME: &'static str = "base_function";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "function": Key,
                "name": Text,
                "source_origin": OptionalKey,
                "code_object": OptionalKey,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.function),
                self.name.clone(),
                opt_key(self.source_origin.as_ref()),
                opt_key(self.code_object.as_ref()),
            ]
        )
    }
}

impl TraceRelation for LexicalScopeFact {
    const NAME: &'static str = "base_scope";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "scope": Key,
                "parent": OptionalKey,
                "function": Key,
                "source_origin": OptionalKey,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.scope),
                opt_key(self.parent.as_ref()),
                key(&self.function),
                opt_key(self.source_origin.as_ref()),
            ]
        )
    }
}

impl TraceRelation for TypeFact {
    const NAME: &'static str = "base_type";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "type": Key,
                "kind": Text,
                "name": OptionalText,
                "bit_width": OptionalText,
                "fields": List,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.ty),
                value(&self.kind),
                self.name.clone().unwrap_or_default(),
                self.bit_width
                    .map_or_else(String::new, |width| width.to_string()),
                value(&self.fields),
            ]
        )
    }
}

impl TraceRelation for VariableFact {
    const NAME: &'static str = "base_variable";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "variable": Key,
                "name": Text,
                "type": Key,
                "declaration_origin": Key,
                "scope": OptionalKey,
                "storage_class": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.variable),
                self.name.clone(),
                key(&self.ty),
                key(&self.declaration_origin),
                opt_key(self.scope.as_ref()),
                value(&self.storage_class),
            ]
        )
    }
}

impl TraceRelation for LocationRangeFact {
    const NAME: &'static str = "base_location_range";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "subject": Key,
                "code_object": Key,
                "pc_start": U32,
                "pc_end": U32,
                "location": Text,
                "reason": Text,
                "confidence": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.subject),
                key(&self.code_object),
                self.pc_range.start.to_string(),
                self.pc_range.end.to_string(),
                value(&self.location),
                value(&self.reason),
                value(&self.confidence),
            ]
        )
    }
}

impl TraceRelation for StaticGasFact {
    const NAME: &'static str = "base_static_gas";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "instruction": Key,
                "schedule": Text,
                "base_cost": U64,
                "dynamic_cost_kind": OptionalText,
                "confidence": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.instruction),
                self.schedule.to_string(),
                self.base_cost.to_string(),
                opt_value(self.dynamic_cost_kind.as_ref()),
                value(&self.confidence),
            ]
        )
    }
}

impl TraceRelation for DynamicGasStepFact {
    const NAME: &'static str = "base_dynamic_gas";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "trace_id": Text,
                "step_index": U64,
                "code_object": Key,
                "pc": U32,
                "instruction": OptionalKey,
                "gas_before": U64,
                "gas_after": U64,
                "gas_cost": U64,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                self.trace_id.clone(),
                self.step_index.to_string(),
                key(&self.code_object),
                self.pc.to_string(),
                opt_key(self.instruction.as_ref()),
                self.gas_before.to_string(),
                self.gas_after.to_string(),
                self.gas_cost.to_string(),
            ]
        )
    }
}

impl TraceRelation for ExecutionTraceSessionFact {
    const NAME: &'static str = "base_execution_trace_session";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "session": Key,
                "source": Text,
                "capture_mode": Text,
                "value_policy": Text,
                "transaction_hash": OptionalText,
                "chain_id": OptionalText,
                "block_number": OptionalText,
                "entry_code_object": OptionalKey,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.session),
                value(&self.source),
                value(&self.capture_mode),
                value(&self.value_policy),
                self.transaction_hash.clone().unwrap_or_default(),
                self.chain_id
                    .map_or_else(String::new, |value| value.to_string()),
                self.block_number
                    .map_or_else(String::new, |value| value.to_string()),
                opt_key(self.entry_code_object.as_ref()),
            ]
        )
    }
}

impl TraceRelation for RuntimeCodeObjectBindingFact {
    const NAME: &'static str = "base_runtime_code_object_binding";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "binding": Key,
                "session": Key,
                "code_object": Key,
                "runtime_code_hash": Text,
                "address": OptionalText,
                "confidence": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.binding),
                key(&self.session),
                key(&self.code_object),
                self.runtime_code_hash.clone(),
                self.address.clone().unwrap_or_default(),
                value(&self.confidence),
            ]
        )
    }
}

impl TraceRelation for StackSampleFact {
    const NAME: &'static str = "base_stack_sample";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["sample": Key, "step": Key, "policy": Text, "values_top_first": List]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.sample),
                key(&self.step),
                value(&self.policy),
                value(&self.values_top_first),
            ]
        )
    }
}

impl TraceRelation for ReturnDataFact {
    const NAME: &'static str = "base_return_data";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["event": Key, "step": Key, "kind": Text, "data": Text, "policy": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.event),
                key(&self.step),
                value(&self.kind),
                value(&self.data),
                value(&self.policy),
            ]
        )
    }
}

impl TraceRelation for PrecompileInvocationFact {
    const NAME: &'static str = "base_precompile_invocation";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "invocation": Key,
                "step": Key,
                "address": Text,
                "gas_used": U64,
                "input": Text,
                "output": OptionalText,
                "success": Text,
                "policy": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.invocation),
                key(&self.step),
                self.address.clone(),
                self.gas_used.to_string(),
                value(&self.input),
                opt_value(self.output.as_ref()),
                self.success.to_string(),
                value(&self.policy),
            ]
        )
    }
}

impl TraceRelation for SelfdestructFact {
    const NAME: &'static str = "base_selfdestruct";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "event": Key,
                "step": Key,
                "contract": OptionalText,
                "beneficiary": OptionalText,
                "balance": OptionalText,
                "policy": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.event),
                key(&self.step),
                self.contract.clone().unwrap_or_default(),
                self.beneficiary.clone().unwrap_or_default(),
                opt_value(self.balance.as_ref()),
                value(&self.policy),
            ]
        )
    }
}

impl TraceRelation for ShapePolicyFact {
    const NAME: &'static str = "base_shape_policy";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "policy": Text,
                "schema_version": U32,
                "algorithm": Text,
                "level": Text,
                "dimensions": List,
                "view_mode": Text,
                "cycle_policy": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                self.policy.as_str().to_string(),
                self.schema_version.to_string(),
                self.algorithm.as_str().to_string(),
                self.level.clone(),
                shape_dimensions(&self.dimensions),
                self.view_mode.as_str().to_string(),
                self.cycle_policy.as_str().to_string(),
            ]
        )
    }
}

impl TraceRelation for ShapeNodeHashFact {
    const NAME: &'static str = "base_shape_node_hash";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "node": Key,
                "graph_owner": Key,
                "graph_local": Text,
                "policy": Text,
                "local": Text,
                "tree": Text,
                "component": OptionalText,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.node),
                key(&self.graph.owner),
                self.graph.local.as_str().to_string(),
                self.policy.as_str().to_string(),
                dimension_digests(&self.local),
                dimension_digests(&self.tree),
                self.component
                    .as_ref()
                    .map_or_else(String::new, dimension_digests),
            ]
        )
    }
}

impl TraceRelation for ShapeComponentHashFact {
    const NAME: &'static str = "base_shape_component_hash";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "graph_owner": Key,
                "graph_local": Text,
                "policy": Text,
                "component_index": U32,
                "members": List,
                "digests": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.graph.owner),
                self.graph.local.as_str().to_string(),
                self.policy.as_str().to_string(),
                self.component_index.to_string(),
                key_list(&self.members),
                dimension_digests(&self.digests),
            ]
        )
    }
}

impl TraceRelation for ShapeGraphHashFact {
    const NAME: &'static str = "base_shape_graph_hash";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "graph_owner": Key,
                "graph_local": Text,
                "policy": Text,
                "digests": Text,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.graph.owner),
                self.graph.local.as_str().to_string(),
                self.policy.as_str().to_string(),
                dimension_digests(&self.digests),
            ]
        )
    }
}

pub fn encode_key(key: &OriginExportKey) -> String {
    key.canonical_storage_key()
}

pub fn encode_optional_key(key: Option<&OriginExportKey>) -> String {
    key.map_or_else(String::new, OriginExportKey::canonical_storage_key)
}

pub fn encode_key_list(keys: &[OriginExportKey]) -> String {
    keys.iter().map(key).collect::<Vec<_>>().join("|")
}

fn shape_dimensions(dimensions: &[ShapeDimension]) -> String {
    dimensions
        .iter()
        .map(|dimension| dimension.as_str())
        .collect::<Vec<_>>()
        .join("|")
}

fn dimension_digests(digests: &DimensionDigests) -> String {
    digests
        .iter()
        .map(|(dimension, digest)| format!("{}={}", dimension.as_str(), digest.as_str()))
        .collect::<Vec<_>>()
        .join("|")
}

pub fn encode_optional_value<T>(value: Option<&T>) -> String
where
    T: Serialize,
{
    value.map_or_else(String::new, self::value)
}

pub fn encode_value<T>(value: &T) -> String
where
    T: Serialize,
{
    match serde_json::to_value(value).expect("trace relation value should serialize") {
        serde_json::Value::String(value) => value,
        value => value.to_string(),
    }
}

fn key(key: &OriginExportKey) -> String {
    encode_key(key)
}

fn opt_key(key: Option<&OriginExportKey>) -> String {
    encode_optional_key(key)
}

fn key_list(keys: &[OriginExportKey]) -> String {
    encode_key_list(keys)
}

fn opt_value<T>(value: Option<&T>) -> String
where
    T: Serialize,
{
    encode_optional_value(value)
}

fn value<T>(value: &T) -> String
where
    T: Serialize,
{
    encode_value(value)
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use crate::{
        ExecutionStepFact, InstructionExtentFact, InstructionFact, OriginEdgeFact, OriginEdgeLabel,
        OriginNodeFact, PcRange, RelationBundleData, RuntimePcJoinConfidence, TraceBundle,
        TraceFact, TraceMetadata, TraceSnapshot,
    };

    use super::TraceFactSpec;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn typed_facts_define_base_relation_schema_and_row() {
        let from = key("bytecode.pc", "demo", "pc:0");
        let to = key("hir.expr", "demo", "expr:0");
        let fact = TraceFact::OriginEdge(OriginEdgeFact::new(
            from.clone(),
            to.clone(),
            OriginEdgeLabel::LoweredFrom,
            None,
        ));

        assert_eq!(fact.base_relation_name(), "base_origin_edge");
        assert_eq!(fact.base_relation_schema().columns.len(), 4);
        assert_eq!(
            fact.base_relation_row(),
            super::RelationRow {
                relation: "base_origin_edge",
                values: vec![
                    from.canonical_storage_key(),
                    to.canonical_storage_key(),
                    "lowered_from".to_string(),
                    String::new(),
                ],
            }
        );
    }

    #[test]
    fn origin_node_relation_kind_comes_from_export_key() {
        let key = key("runtime.local", "demo", "local:0");
        let fact = TraceFact::OriginNode(OriginNodeFact::from_key(key.clone()));

        assert_eq!(
            fact.base_relation_row().values,
            vec![key.canonical_storage_key(), "runtime.local".to_string()]
        );
    }

    #[test]
    fn instruction_relation_uses_instruction_owner_identity() {
        let function = key("function", "demo", "main");
        let instruction = key("asm.inst", "demo", "inst:0");
        let fact = TraceFact::Instruction(InstructionFact::new(
            instruction.clone(),
            function.clone(),
            7,
            "addi",
        ));

        assert_eq!(
            fact.base_relation_row().values,
            vec![
                instruction.canonical_storage_key(),
                function.canonical_storage_key(),
                "7".to_string(),
                "addi".to_string(),
            ]
        );
    }

    #[test]
    fn generated_instruction_extent_relation_matches_legacy_projection() {
        let instruction = key("bytecode.pc", "demo", "pc:4");
        let code_object = key("code.object", "demo", "runtime");
        let fact = InstructionExtentFact::new(
            instruction.clone(),
            code_object.clone(),
            PcRange::new(4, 7),
            3,
        );

        assert_eq!(fact.primary_key(), Some(&instruction));
        assert_eq!(
            fact.origin_refs()
                .iter()
                .map(|origin_ref| origin_ref.field)
                .collect::<Vec<_>>(),
            vec!["instruction", "code_object"]
        );
        assert_eq!(
            TraceFact::InstructionExtent(fact).base_relation_row(),
            super::RelationRow {
                relation: "base_instruction_extent",
                values: vec![
                    instruction.canonical_storage_key(),
                    code_object.canonical_storage_key(),
                    "4".to_string(),
                    "7".to_string(),
                    "3".to_string(),
                ],
            }
        );
    }

    #[test]
    fn generated_execution_step_relation_keeps_runtime_projection_shape() {
        let step = key("runtime.step", "demo", "step:0");
        let session = key("runtime.session", "demo", "session:0");
        let code_object = key("code.object", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:4");
        let fact = ExecutionStepFact {
            step: step.clone(),
            session: session.clone(),
            step_index: 9,
            code_object: code_object.clone(),
            pc: 4,
            opcode: "JUMPDEST".to_string(),
            instruction: Some(instruction.clone()),
            gas_before: 100,
            gas_after: 98,
            gas_cost: 2,
            depth: 1,
            join_confidence: RuntimePcJoinConfidence::ExactCodeObjectAndPc,
        };

        assert_eq!(fact.primary_key(), Some(&step));
        assert_eq!(
            fact.origin_refs()
                .iter()
                .map(|origin_ref| (origin_ref.field, origin_ref.required))
                .collect::<Vec<_>>(),
            vec![
                ("step", true),
                ("session", true),
                ("code_object", true),
                ("instruction", false),
            ]
        );
        assert_eq!(
            TraceFact::ExecutionStep(fact).base_relation_row(),
            super::RelationRow {
                relation: "base_execution_step",
                values: vec![
                    step.canonical_storage_key(),
                    session.canonical_storage_key(),
                    "9".to_string(),
                    code_object.canonical_storage_key(),
                    "4".to_string(),
                    "JUMPDEST".to_string(),
                    instruction.canonical_storage_key(),
                    "100".to_string(),
                    "98".to_string(),
                    "2".to_string(),
                    "1".to_string(),
                    "exact_code_object_and_pc".to_string(),
                ],
            }
        );
    }

    #[test]
    fn relation_bundle_data_is_derived_from_snapshot_facts() {
        let function = key("function", "demo", "main");
        let instruction = key("bytecode.inst", "demo", "pc:0");
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                TraceFact::OriginNode(OriginNodeFact::from_key(function.clone())),
                TraceFact::OriginNode(OriginNodeFact::from_key(instruction.clone())),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        ))
        .unwrap();

        let relation_bundle = RelationBundleData::from_snapshot(&snapshot);

        assert_eq!(relation_bundle.trace_hash, snapshot.trace_hash());
        assert_eq!(relation_bundle.rows.len(), 3);
        assert_eq!(
            relation_bundle
                .schemas
                .iter()
                .map(|schema| schema.name)
                .collect::<Vec<_>>(),
            vec!["base_instruction", "base_origin_node"]
        );
    }

    #[test]
    fn trace_emit_pushes_already_constructed_facts_only() {
        let function = key("function", "demo", "main");
        let instruction = key("bytecode.inst", "demo", "pc:0");
        let mut facts = Vec::new();

        crate::trace_emit!(
            facts,
            OriginNodeFact::from_key(function.clone()),
            OriginNodeFact::from_key(instruction.clone()),
            InstructionFact::new(instruction, function, 0, "STOP"),
        );

        assert_eq!(facts.len(), 3);
        assert!(matches!(facts[0], TraceFact::OriginNode(_)));
        assert!(matches!(facts[2], TraceFact::Instruction(_)));
    }
}
