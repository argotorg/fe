use std::fmt;

use common::origin::{OriginExportKey, validate_origin_key_text};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};
use shape_address::{
    DimensionDigests, ShapeCyclePolicy, ShapeDigestAlgorithm, ShapeDimension, ShapeGraph,
    ShapeGraphHashes, ShapeGraphKey, ShapeHashPolicy, ShapeNodeKey, ShapePolicyId, ShapeViewMode,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum TraceFact {
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
    ShapePolicy(ShapePolicyFact),
    ShapeNodeHash(ShapeNodeHashFact),
    ShapeComponentHash(ShapeComponentHashFact),
    ShapeGraphHash(ShapeGraphHashFact),
}

impl TraceFact {
    pub const fn relation_name(&self) -> &'static str {
        match self {
            Self::OriginNode(_) => "origin_node",
            Self::OriginEdge(_) => "origin_edge",
            Self::CompilerEvent(_) => "compiler_event",
            Self::Storage(_) => "storage",
            Self::Instruction(_) => "instruction",
            Self::InstructionCategory(_) => "instruction_category",
            Self::Block(_) => "block",
            Self::CfgEdge(_) => "cfg_edge",
            Self::Loop(_) => "loop",
            Self::LoopBlock(_) => "loop_block",
            Self::InstructionBlock(_) => "instruction_block",
            Self::InstructionExtent(_) => "instruction_extent",
            Self::LoopMembership(_) => "loop_membership",
            Self::InlineContext(_) => "inline_context",
            Self::Opcode(_) => "opcode",
            Self::GasCost(_) => "gas_cost",
            Self::DisplayName(_) => "display_name",
            Self::ValueProperty(_) => "value_property",
            Self::SourceFile(_) => "source_file",
            Self::SourceSpan(_) => "source_span",
            Self::CodeObject(_) => "code_object",
            Self::Function(_) => "function",
            Self::LexicalScope(_) => "lexical_scope",
            Self::Type(_) => "type",
            Self::Variable(_) => "variable",
            Self::LocationRange(_) => "location_range",
            Self::StaticGas(_) => "static_gas",
            Self::DynamicGasStep(_) => "dynamic_gas_step",
            Self::ShapePolicy(_) => "shape_policy",
            Self::ShapeNodeHash(_) => "shape_node_hash",
            Self::ShapeComponentHash(_) => "shape_component_hash",
            Self::ShapeGraphHash(_) => "shape_graph_hash",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OriginNodeKind(String);

impl OriginNodeKind {
    pub fn new(value: impl Into<String>) -> Self {
        Self::try_new(value).unwrap_or_else(|err| panic!("invalid origin node kind: {err}"))
    }

    pub fn try_new(value: impl Into<String>) -> Result<Self, TraceFactTextError> {
        let value = value.into();
        validate_trace_text("origin node kind", &value)?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for OriginNodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for OriginNodeKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for OriginNodeKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::try_new(String::deserialize(deserializer)?).map_err(de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompilerReason(String);

impl CompilerReason {
    pub fn new(value: impl Into<String>) -> Self {
        Self::try_new(value).unwrap_or_else(|err| panic!("invalid compiler reason: {err}"))
    }

    pub fn try_new(value: impl Into<String>) -> Result<Self, TraceFactTextError> {
        let value = value.into();
        validate_trace_text("compiler reason", &value)?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for CompilerReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for CompilerReason {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for CompilerReason {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::try_new(String::deserialize(deserializer)?).map_err(de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceFactTextError {
    Empty { kind: &'static str },
    ReservedStorageSeparator { kind: &'static str },
}

impl fmt::Display for TraceFactTextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty { kind } => write!(f, "{kind} must not be empty"),
            Self::ReservedStorageSeparator { kind } => {
                write!(
                    f,
                    "{kind} must not contain reserved origin storage separator"
                )
            }
        }
    }
}

impl std::error::Error for TraceFactTextError {}

fn validate_trace_text(kind: &'static str, value: &str) -> Result<(), TraceFactTextError> {
    validate_origin_key_text(kind, value).map_err(|err| match err {
        common::origin::OriginKeyTextError::Empty { kind } => TraceFactTextError::Empty { kind },
        common::origin::OriginKeyTextError::ReservedStorageSeparator { kind } => {
            TraceFactTextError::ReservedStorageSeparator { kind }
        }
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct OriginNodeFact {
    pub key: OriginExportKey,
}

impl OriginNodeFact {
    pub fn new(key: OriginExportKey, kind: OriginNodeKind) -> Self {
        assert_eq!(
            key.kind(),
            kind.as_str(),
            "origin node kind must match export key kind"
        );
        Self { key }
    }

    pub fn from_key(key: OriginExportKey) -> Self {
        Self { key }
    }

    pub fn kind(&self) -> &str {
        self.key.kind()
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct OriginNodeFactSerde {
    key: OriginExportKey,
    #[serde(default)]
    kind: Option<OriginNodeKind>,
}

impl<'de> Deserialize<'de> for OriginNodeFact {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = OriginNodeFactSerde::deserialize(deserializer)?;
        if let Some(kind) = raw.kind
            && raw.key.kind() != kind.as_str()
        {
            return Err(de::Error::custom(format!(
                "origin node kind {} does not match key kind {}",
                kind,
                raw.key.kind()
            )));
        }
        Ok(Self { key: raw.key })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OriginEdgeFact {
    pub from: OriginExportKey,
    pub to: OriginExportKey,
    pub label: OriginEdgeLabel,
    pub introduced_by: Option<CompilerPhase>,
}

impl OriginEdgeFact {
    pub fn new(
        from: OriginExportKey,
        to: OriginExportKey,
        label: OriginEdgeLabel,
        introduced_by: Option<CompilerPhase>,
    ) -> Self {
        Self {
            from,
            to,
            label,
            introduced_by,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OriginEdgeLabel {
    LoweredFrom,
    EmittedFrom,
    SyntheticFor,
    IntegerLegalizationFor,
    LoadOf,
    StoreOf,
    SpillOf,
    ReloadOf,
    InlinedFrom,
    CallsiteOf,
    PreservedSnapshotIdentity,
    BackendPrepared,
    Unmapped,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompilerPhase {
    Hir,
    Mir,
    SonatinaPreOpt,
    SonatinaPostOpt,
    Backend,
    BytecodeEmission,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CompilerEventFact {
    pub event: OriginExportKey,
    pub phase: CompilerPhase,
    pub kind: CompilerEventKind,
    pub inputs: Vec<OriginExportKey>,
    pub outputs: Vec<OriginExportKey>,
    pub reason: Option<CompilerReason>,
}

impl CompilerEventFact {
    pub fn new(
        event: OriginExportKey,
        phase: CompilerPhase,
        kind: CompilerEventKind,
        inputs: Vec<OriginExportKey>,
        outputs: Vec<OriginExportKey>,
        reason: Option<CompilerReason>,
    ) -> Self {
        Self {
            event,
            phase,
            kind,
            inputs,
            outputs,
            reason,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompilerEventKind {
    Lowering,
    InsertIntegerZeroExtend,
    InsertIntegerSignExtend,
    CreateStackSlot,
    Spill,
    Reload,
    RegisterMove,
    InlineFunction,
    OptimizerSnapshotJoin,
    OptimizerCreated,
    OptimizerElidedOrRewritten,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StorageFact {
    pub subject: OriginExportKey,
    pub phase: CompilerPhase,
    pub location: StorageLocation,
    pub reason: StorageReason,
}

impl StorageFact {
    pub fn new(
        subject: OriginExportKey,
        phase: CompilerPhase,
        location: StorageLocation,
        reason: StorageReason,
    ) -> Self {
        Self {
            subject,
            phase,
            location,
            reason,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StorageLocation {
    SsaValue,
    MemoryPlace,
    StackSlot { offset: i32 },
    VirtualRegister(String),
    PhysicalRegister(String),
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StorageReason {
    MutableLocalLowering,
    AddressTaken,
    FrameSlot,
    Spill,
    Reload,
    Abi,
    BackendPrepared,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InstructionFact {
    pub instruction: OriginExportKey,
    pub function: OriginExportKey,
    pub index: u32,
    pub mnemonic: String,
}

impl InstructionFact {
    pub fn new(
        instruction: OriginExportKey,
        function: OriginExportKey,
        index: u32,
        mnemonic: impl Into<String>,
    ) -> Self {
        Self {
            instruction,
            function,
            index,
            mnemonic: mnemonic.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InstructionCategoryFact {
    pub instruction: OriginExportKey,
    pub category: InstructionCategory,
    pub source: CategorySource,
}

impl InstructionCategoryFact {
    pub fn new(
        instruction: OriginExportKey,
        category: InstructionCategory,
        source: CategorySource,
    ) -> Self {
        Self {
            instruction,
            category,
            source,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InstructionCategory {
    Arithmetic,
    Branch,
    Jump,
    Load,
    Store,
    StackLoad,
    StackStore,
    ZeroExtend,
    SignExtend,
    Move,
    Call,
    FrameSetup,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CategorySource {
    BackendEmissionReason,
    PosthocClassifier { version: String },
    ManualAnnotation,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BlockFact {
    pub block: OriginExportKey,
    pub function: OriginExportKey,
    pub phase: CompilerPhase,
    pub ordinal: u32,
    pub name: Option<String>,
}

impl BlockFact {
    pub fn new(
        block: OriginExportKey,
        function: OriginExportKey,
        phase: CompilerPhase,
        ordinal: u32,
        name: Option<String>,
    ) -> Self {
        Self {
            block,
            function,
            phase,
            ordinal,
            name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CfgEdgeFact {
    pub function: OriginExportKey,
    pub from_block: OriginExportKey,
    pub to_block: OriginExportKey,
    pub kind: CfgEdgeKind,
    pub condition_origin: Option<OriginExportKey>,
}

impl CfgEdgeFact {
    pub fn new(
        function: OriginExportKey,
        from_block: OriginExportKey,
        to_block: OriginExportKey,
        kind: CfgEdgeKind,
        condition_origin: Option<OriginExportKey>,
    ) -> Self {
        Self {
            function,
            from_block,
            to_block,
            kind,
            condition_origin,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CfgEdgeKind {
    Fallthrough,
    BranchTrue,
    BranchFalse,
    Jump,
    Backedge,
    Return,
    Unwind,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LoopFact {
    pub loop_key: OriginExportKey,
    pub function: OriginExportKey,
    pub phase: CompilerPhase,
    pub header_block: OriginExportKey,
    pub derivation: LoopDerivation,
    pub confidence: LoopConfidence,
}

impl LoopFact {
    pub fn new(
        loop_key: OriginExportKey,
        function: OriginExportKey,
        phase: CompilerPhase,
        header_block: OriginExportKey,
        derivation: LoopDerivation,
        confidence: LoopConfidence,
    ) -> Self {
        Self {
            loop_key,
            function,
            phase,
            header_block,
            derivation,
            confidence,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoopConfidence {
    MirCfg,
    SonatinaCfg,
    BackendBlockMapping,
    CrossPhaseWitness,
    Fixture,
    PosthocDisassembly,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LoopBlockFact {
    pub loop_key: OriginExportKey,
    pub block: OriginExportKey,
    pub role: LoopBlockRole,
}

impl LoopBlockFact {
    pub fn new(loop_key: OriginExportKey, block: OriginExportKey, role: LoopBlockRole) -> Self {
        Self {
            loop_key,
            block,
            role,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoopBlockRole {
    Header,
    Body,
    Latch,
    Preheader,
    Exit,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InstructionBlockFact {
    pub instruction: OriginExportKey,
    pub block: OriginExportKey,
    pub phase: CompilerPhase,
}

impl InstructionBlockFact {
    pub fn new(instruction: OriginExportKey, block: OriginExportKey, phase: CompilerPhase) -> Self {
        Self {
            instruction,
            block,
            phase,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InstructionExtentFact {
    pub instruction: OriginExportKey,
    pub code_object: OriginExportKey,
    pub pc_range: PcRange,
    pub byte_len: u32,
}

impl InstructionExtentFact {
    pub fn new(
        instruction: OriginExportKey,
        code_object: OriginExportKey,
        pc_range: PcRange,
        byte_len: u32,
    ) -> Self {
        Self {
            instruction,
            code_object,
            pc_range,
            byte_len,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LoopMembershipFact {
    pub loop_key: OriginExportKey,
    pub instruction: OriginExportKey,
    pub derived_from: LoopDerivation,
}

impl LoopMembershipFact {
    pub fn new(
        loop_key: OriginExportKey,
        instruction: OriginExportKey,
        derived_from: LoopDerivation,
    ) -> Self {
        Self {
            loop_key,
            instruction,
            derived_from,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoopDerivation {
    NaturalLoopAnalysis { cfg_hash: String },
    BackendBlockMapping,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InlineContextFact {
    pub inline_instance: OriginExportKey,
    pub caller_function: OriginExportKey,
    pub callee_function: OriginExportKey,
    pub callsite: OriginExportKey,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OpcodeFact {
    pub pc: OriginExportKey,
    pub opcode: String,
    pub immediate: Option<String>,
    pub category: OpcodeCategory,
}

impl OpcodeFact {
    pub fn new(
        pc: OriginExportKey,
        opcode: impl Into<String>,
        immediate: Option<String>,
        category: OpcodeCategory,
    ) -> Self {
        Self {
            pc,
            opcode: opcode.into(),
            immediate,
            category,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OpcodeCategory {
    Arithmetic,
    Comparison,
    Memory,
    Storage,
    ControlFlow,
    Stack,
    Push,
    CallData,
    Return,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GasCostFact {
    pub subject: OriginExportKey,
    pub gas_kind: GasKind,
    pub gas: u64,
    pub schedule: EvmSchedule,
    pub confidence: GasConfidence,
    pub source: GasSource,
}

impl GasCostFact {
    pub fn new(
        subject: OriginExportKey,
        gas_kind: GasKind,
        gas: u64,
        schedule: EvmSchedule,
        confidence: GasConfidence,
        source: GasSource,
    ) -> Self {
        Self {
            subject,
            gas_kind,
            gas,
            schedule,
            confidence,
            source,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GasKind {
    OpcodeStatic,
    PcRangeStatic,
    FunctionStatic,
    RuntimeTrace,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EvmSchedule(String);

impl EvmSchedule {
    pub fn new(value: impl Into<String>) -> Self {
        Self::try_new(value).unwrap_or_else(|err| panic!("invalid EVM schedule: {err}"))
    }

    pub fn try_new(value: impl Into<String>) -> Result<Self, TraceFactTextError> {
        let value = value.into();
        validate_trace_text("EVM schedule", &value)?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for EvmSchedule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for EvmSchedule {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for EvmSchedule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::try_new(String::deserialize(deserializer)?).map_err(de::Error::custom)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GasConfidence {
    ExactStaticOpcode,
    ConservativeStatic,
    RuntimeMeasured,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GasSource {
    OpcodeTable,
    EvmTrace,
    ManualFixture,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DisplayNameFact {
    pub subject: OriginExportKey,
    pub kind: DisplayNameKind,
    pub name: String,
}

impl DisplayNameFact {
    pub fn new(subject: OriginExportKey, kind: DisplayNameKind, name: impl Into<String>) -> Self {
        Self {
            subject,
            kind,
            name: name.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DisplayNameKind {
    SourceLocal,
    RuntimeSymbol,
    BytecodeFunction,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ValuePropertyFact {
    pub subject: OriginExportKey,
    pub phase: CompilerPhase,
    pub property: ValueProperty,
    pub reason: Option<CompilerReason>,
}

impl ValuePropertyFact {
    pub fn new(
        subject: OriginExportKey,
        phase: CompilerPhase,
        property: ValueProperty,
        reason: Option<CompilerReason>,
    ) -> Self {
        Self {
            subject,
            phase,
            property,
            reason,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ValueProperty {
    SourceMutable,
    MemoryBacked,
    SsaValue,
    KnownUnsignedWidth { bits: u16 },
    ZeroExtended,
    LoopInvariantCandidate,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceFileFact {
    pub file_key: OriginExportKey,
    pub uri: String,
    pub display_name: String,
    pub content_hash: String,
    pub source_id: Option<u32>,
}

impl SourceFileFact {
    pub fn new(
        file_key: OriginExportKey,
        uri: impl Into<String>,
        display_name: impl Into<String>,
        content_hash: impl Into<String>,
        source_id: Option<u32>,
    ) -> Self {
        Self {
            file_key,
            uri: uri.into(),
            display_name: display_name.into(),
            content_hash: content_hash.into(),
            source_id,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceSpanFact {
    pub origin: OriginExportKey,
    pub file: OriginExportKey,
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

impl SourceSpanFact {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        origin: OriginExportKey,
        file: OriginExportKey,
        start_byte: u32,
        end_byte: u32,
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
    ) -> Self {
        Self {
            origin,
            file,
            start_byte,
            end_byte,
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CodeObjectFact {
    pub code_object: OriginExportKey,
    pub kind: CodeObjectKind,
    pub owner_function_or_contract: Option<OriginExportKey>,
    pub target: String,
    pub code_hash: Option<String>,
}

impl CodeObjectFact {
    pub fn new(
        code_object: OriginExportKey,
        kind: CodeObjectKind,
        owner_function_or_contract: Option<OriginExportKey>,
        target: impl Into<String>,
        code_hash: Option<String>,
    ) -> Self {
        Self {
            code_object,
            kind,
            owner_function_or_contract,
            target: target.into(),
            code_hash,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CodeObjectKind {
    EvmRuntimeBytecode,
    EvmCreationBytecode,
    NativeObject,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FunctionFact {
    pub function: OriginExportKey,
    pub name: String,
    pub source_origin: Option<OriginExportKey>,
    pub code_object: Option<OriginExportKey>,
}

impl FunctionFact {
    pub fn new(
        function: OriginExportKey,
        name: impl Into<String>,
        source_origin: Option<OriginExportKey>,
        code_object: Option<OriginExportKey>,
    ) -> Self {
        Self {
            function,
            name: name.into(),
            source_origin,
            code_object,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LexicalScopeFact {
    pub scope: OriginExportKey,
    pub parent: Option<OriginExportKey>,
    pub function: OriginExportKey,
    pub source_origin: Option<OriginExportKey>,
}

impl LexicalScopeFact {
    pub fn new(
        scope: OriginExportKey,
        parent: Option<OriginExportKey>,
        function: OriginExportKey,
        source_origin: Option<OriginExportKey>,
    ) -> Self {
        Self {
            scope,
            parent,
            function,
            source_origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TypeFact {
    pub ty: OriginExportKey,
    pub kind: TypeKind,
    pub name: Option<String>,
    pub bit_width: Option<u32>,
    pub fields: Vec<TypeField>,
}

impl TypeFact {
    pub fn new(
        ty: OriginExportKey,
        kind: TypeKind,
        name: Option<String>,
        bit_width: Option<u32>,
        fields: Vec<TypeField>,
    ) -> Self {
        Self {
            ty,
            kind,
            name,
            bit_width,
            fields,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TypeKind {
    Bool,
    UnsignedInteger,
    SignedInteger,
    Address,
    Unit,
    Tuple,
    Array,
    Struct,
    Contract,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TypeField {
    pub name: String,
    pub ty: OriginExportKey,
    pub offset_bits: Option<u32>,
    pub width_bits: Option<u32>,
}

impl TypeField {
    pub fn new(
        name: impl Into<String>,
        ty: OriginExportKey,
        offset_bits: Option<u32>,
        width_bits: Option<u32>,
    ) -> Self {
        Self {
            name: name.into(),
            ty,
            offset_bits,
            width_bits,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct VariableFact {
    pub variable: OriginExportKey,
    pub name: String,
    pub ty: OriginExportKey,
    pub declaration_origin: OriginExportKey,
    pub scope: Option<OriginExportKey>,
    pub storage_class: VariableStorageClass,
}

impl VariableFact {
    pub fn new(
        variable: OriginExportKey,
        name: impl Into<String>,
        ty: OriginExportKey,
        declaration_origin: OriginExportKey,
        scope: Option<OriginExportKey>,
        storage_class: VariableStorageClass,
    ) -> Self {
        Self {
            variable,
            name: name.into(),
            ty,
            declaration_origin,
            scope,
            storage_class,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableStorageClass {
    Local,
    Parameter,
    State,
    Temporary,
    ReturnValue,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LocationRangeFact {
    pub subject: OriginExportKey,
    pub code_object: OriginExportKey,
    pub pc_range: PcRange,
    pub location: ValueLocation,
    pub reason: StorageReason,
    pub confidence: LocationConfidence,
}

impl LocationRangeFact {
    pub fn new(
        subject: OriginExportKey,
        code_object: OriginExportKey,
        pc_range: PcRange,
        location: ValueLocation,
        reason: StorageReason,
        confidence: LocationConfidence,
    ) -> Self {
        Self {
            subject,
            code_object,
            pc_range,
            location,
            reason,
            confidence,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PcRange {
    pub start: u32,
    pub end: u32,
}

impl PcRange {
    pub const fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub const fn is_valid(self) -> bool {
        self.start < self.end
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ValueLocation {
    SsaValue {
        value: OriginExportKey,
    },
    MemoryPlace,
    StackSlot {
        offset: i64,
    },
    Register {
        name: String,
    },
    EvmStack {
        depth_from_top: u32,
    },
    EvmMemory {
        offset: LocationExpr,
        length: Option<LocationExpr>,
    },
    EvmStorage {
        slot: LocationExpr,
        offset_bits: Option<u16>,
        width_bits: Option<u16>,
    },
    EvmCalldata {
        offset: LocationExpr,
        length: Option<LocationExpr>,
    },
    Unknown {
        reason: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LocationExpr {
    Constant { value: i64 },
    Origin { origin: OriginExportKey },
    Unknown { reason: String },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LocationConfidence {
    Exact,
    Conservative,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StaticGasFact {
    pub instruction: OriginExportKey,
    pub schedule: EvmSchedule,
    pub base_cost: u64,
    pub dynamic_cost_kind: Option<DynamicGasKind>,
    #[serde(default = "default_static_gas_confidence")]
    pub confidence: GasConfidence,
}

impl StaticGasFact {
    pub fn new(
        instruction: OriginExportKey,
        schedule: EvmSchedule,
        base_cost: u64,
        dynamic_cost_kind: Option<DynamicGasKind>,
    ) -> Self {
        Self {
            instruction,
            schedule,
            base_cost,
            dynamic_cost_kind,
            confidence: GasConfidence::ConservativeStatic,
        }
    }

    pub fn with_confidence(
        instruction: OriginExportKey,
        schedule: EvmSchedule,
        base_cost: u64,
        dynamic_cost_kind: Option<DynamicGasKind>,
        confidence: GasConfidence,
    ) -> Self {
        Self {
            instruction,
            schedule,
            base_cost,
            dynamic_cost_kind,
            confidence,
        }
    }
}

fn default_static_gas_confidence() -> GasConfidence {
    GasConfidence::ConservativeStatic
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DynamicGasKind {
    MemoryExpansion,
    StorageAccess,
    Call,
    Copy,
    Keccak,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DynamicGasStepFact {
    pub trace_id: String,
    pub step_index: u64,
    pub code_object: OriginExportKey,
    pub pc: u32,
    pub instruction: Option<OriginExportKey>,
    pub gas_before: u64,
    pub gas_after: u64,
    pub gas_cost: u64,
}

impl DynamicGasStepFact {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        trace_id: impl Into<String>,
        step_index: u64,
        code_object: OriginExportKey,
        pc: u32,
        instruction: Option<OriginExportKey>,
        gas_before: u64,
        gas_after: u64,
        gas_cost: u64,
    ) -> Self {
        Self {
            trace_id: trace_id.into(),
            step_index,
            code_object,
            pc,
            instruction,
            gas_before,
            gas_after,
            gas_cost,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ShapePolicyFact {
    pub policy: ShapePolicyId,
    pub schema_version: u32,
    pub algorithm: ShapeDigestAlgorithm,
    pub level: String,
    pub dimensions: Vec<ShapeDimension>,
    pub view_mode: ShapeViewMode,
    pub cycle_policy: ShapeCyclePolicy,
}

impl ShapePolicyFact {
    pub fn new(
        policy: ShapePolicyId,
        schema_version: u32,
        algorithm: ShapeDigestAlgorithm,
        level: impl Into<String>,
        dimensions: Vec<ShapeDimension>,
        view_mode: ShapeViewMode,
        cycle_policy: ShapeCyclePolicy,
    ) -> Self {
        Self {
            policy,
            schema_version,
            algorithm,
            level: level.into(),
            dimensions,
            view_mode,
            cycle_policy,
        }
    }

    pub fn from_policy(policy: &ShapeHashPolicy) -> Self {
        Self {
            policy: policy.policy_id(),
            schema_version: policy.schema_version,
            algorithm: policy.algorithm,
            level: policy.level.as_str().to_string(),
            dimensions: policy.dimensions.iter().copied().collect(),
            view_mode: policy.view_mode,
            cycle_policy: policy.cycle_policy,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ShapeNodeHashFact {
    pub node: OriginExportKey,
    pub graph: ShapeGraphKey,
    pub policy: ShapePolicyId,
    pub local: DimensionDigests,
    pub tree: DimensionDigests,
    pub component: Option<DimensionDigests>,
}

impl ShapeNodeHashFact {
    pub fn new(
        node: OriginExportKey,
        graph: ShapeGraphKey,
        policy: ShapePolicyId,
        local: DimensionDigests,
        tree: DimensionDigests,
        component: Option<DimensionDigests>,
    ) -> Self {
        Self {
            node,
            graph,
            policy,
            local,
            tree,
            component,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ShapeComponentHashFact {
    pub graph: ShapeGraphKey,
    pub policy: ShapePolicyId,
    pub component_index: u32,
    pub members: Vec<OriginExportKey>,
    pub digests: DimensionDigests,
}

impl ShapeComponentHashFact {
    pub fn new(
        graph: ShapeGraphKey,
        policy: ShapePolicyId,
        component_index: u32,
        members: Vec<OriginExportKey>,
        digests: DimensionDigests,
    ) -> Self {
        Self {
            graph,
            policy,
            component_index,
            members,
            digests,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ShapeGraphHashFact {
    pub graph: ShapeGraphKey,
    pub policy: ShapePolicyId,
    pub digests: DimensionDigests,
}

impl ShapeGraphHashFact {
    pub fn new(graph: ShapeGraphKey, policy: ShapePolicyId, digests: DimensionDigests) -> Self {
        Self {
            graph,
            policy,
            digests,
        }
    }
}

pub fn shape_hash_facts(
    graph: &ShapeGraph,
    policy: &ShapeHashPolicy,
    hashes: &ShapeGraphHashes,
) -> Vec<TraceFact> {
    let mut facts = vec![
        TraceFact::ShapePolicy(ShapePolicyFact::from_policy(policy)),
        TraceFact::ShapeGraphHash(ShapeGraphHashFact::new(
            graph.graph_key.clone(),
            hashes.policy_id.clone(),
            hashes.graph.clone(),
        )),
    ];
    for (node, node_hashes) in &hashes.nodes {
        let ShapeNodeKey::Entity(node) = node else {
            continue;
        };
        facts.push(TraceFact::ShapeNodeHash(ShapeNodeHashFact::new(
            node.clone(),
            graph.graph_key.clone(),
            hashes.policy_id.clone(),
            node_hashes.local.clone(),
            node_hashes.tree.clone(),
            node_hashes.component.clone(),
        )));
    }
    for component in &hashes.components {
        let members = component
            .members
            .iter()
            .filter_map(|member| match member {
                ShapeNodeKey::Entity(node) => Some(node.clone()),
                ShapeNodeKey::Derived { .. } => None,
            })
            .collect::<Vec<_>>();
        if members.is_empty() {
            continue;
        }
        facts.push(TraceFact::ShapeComponentHash(ShapeComponentHashFact::new(
            graph.graph_key.clone(),
            hashes.policy_id.clone(),
            component.component_index,
            members,
            component.digests.clone(),
        )));
    }
    facts
}

impl InlineContextFact {
    pub fn new(
        inline_instance: OriginExportKey,
        caller_function: OriginExportKey,
        callee_function: OriginExportKey,
        callsite: OriginExportKey,
    ) -> Self {
        Self {
            inline_instance,
            caller_function,
            callee_function,
            callsite,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn trace_fact_enum_uses_typed_schema_names() {
        let fact = TraceFact::OriginEdge(OriginEdgeFact::new(
            key("bytecode.inst", "fib", "pc:4"),
            key("runtime.local", "fib", "local:b"),
            OriginEdgeLabel::LoadOf,
            Some(CompilerPhase::Backend),
        ));

        assert_eq!(fact.relation_name(), "origin_edge");
        assert!(
            serde_json::to_string(&fact)
                .unwrap()
                .contains("\"origin_edge\"")
        );
    }

    #[test]
    fn extensible_text_newtypes_reject_invalid_join_text() {
        assert!(OriginNodeKind::try_new("runtime.local").is_ok());
        assert_eq!(
            OriginNodeKind::try_new(""),
            Err(TraceFactTextError::Empty {
                kind: "origin node kind"
            })
        );
    }

    #[test]
    fn extensible_text_newtypes_validate_deserialized_text() {
        assert!(serde_json::from_str::<OriginNodeKind>("\"runtime.local\"").is_ok());
        assert!(serde_json::from_str::<OriginNodeKind>("\"\"").is_err());
        assert!(serde_json::from_str::<CompilerReason>("\"lowered integer width\"").is_ok());
        assert!(serde_json::from_str::<CompilerReason>("\"\"").is_err());
        assert!(serde_json::from_str::<EvmSchedule>("\"cancun\"").is_ok());
        assert!(serde_json::from_str::<EvmSchedule>("\"\"").is_err());
    }

    #[test]
    fn origin_node_fact_serializes_key_as_kind_authority() {
        let fact = OriginNodeFact::new(
            key("runtime.local", "fib", "local:b"),
            OriginNodeKind::new("runtime.local"),
        );
        let json = serde_json::to_value(&fact).unwrap();

        assert!(json.get("kind").is_none());
        assert_eq!(json["key"]["kind"], "runtime.local");
    }

    #[test]
    fn origin_node_fact_accepts_matching_legacy_kind_but_rejects_mismatch() {
        let matching = r#"{
            "key": {"kind": "runtime.local", "owner_key": "fib", "local_key": "local:b"},
            "kind": "runtime.local"
        }"#;
        let mismatched = r#"{
            "key": {"kind": "runtime.local", "owner_key": "fib", "local_key": "local:b"},
            "kind": "hir.expr"
        }"#;

        assert!(serde_json::from_str::<OriginNodeFact>(matching).is_ok());
        assert!(serde_json::from_str::<OriginNodeFact>(mismatched).is_err());
    }
}
