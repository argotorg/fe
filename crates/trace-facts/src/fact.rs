use std::fmt;

use common::origin::{OriginExportKey, validate_origin_key_text};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum TraceFact {
    OriginNode(OriginNodeFact),
    OriginEdge(OriginEdgeFact),
    CompilerEvent(CompilerEventFact),
    Storage(StorageFact),
    Instruction(InstructionFact),
    InstructionCategory(InstructionCategoryFact),
    LoopMembership(LoopMembershipFact),
    InlineContext(InlineContextFact),
    Opcode(OpcodeFact),
    GasCost(GasCostFact),
    DisplayName(DisplayNameFact),
    ValueProperty(ValuePropertyFact),
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
            Self::LoopMembership(_) => "loop_membership",
            Self::InlineContext(_) => "inline_context",
            Self::Opcode(_) => "opcode",
            Self::GasCost(_) => "gas_cost",
            Self::DisplayName(_) => "display_name",
            Self::ValueProperty(_) => "value_property",
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
