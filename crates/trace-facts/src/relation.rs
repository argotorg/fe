use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use shape_address::{DimensionDigests, ShapeDimension};

use crate::fact::{
    BlockFact, CfgEdgeFact, CodeObjectFact, CompilerEventFact, DisplayNameFact, DynamicGasStepFact,
    FunctionFact, GasCostFact, InlineContextFact, InstructionBlockFact, InstructionCategoryFact,
    InstructionExtentFact, InstructionFact, LexicalScopeFact, LocationRangeFact, LoopBlockFact,
    LoopFact, LoopMembershipFact, OpcodeFact, OriginEdgeFact, OriginNodeFact,
    ShapeComponentHashFact, ShapeGraphHashFact, ShapeNodeHashFact, ShapePolicyFact, SourceFileFact,
    SourceSpanFact, StaticGasFact, StorageFact, TraceFact, TypeFact, ValuePropertyFact,
    VariableFact,
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

pub trait TraceRelation {
    const NAME: &'static str;

    fn schema() -> RelationSchema;
    fn row(&self) -> RelationRow;
}

impl TraceFact {
    pub fn base_relation_name(&self) -> &'static str {
        match self {
            Self::OriginNode(_) => OriginNodeFact::NAME,
            Self::OriginEdge(_) => OriginEdgeFact::NAME,
            Self::CompilerEvent(_) => CompilerEventFact::NAME,
            Self::Storage(_) => StorageFact::NAME,
            Self::Instruction(_) => InstructionFact::NAME,
            Self::InstructionCategory(_) => InstructionCategoryFact::NAME,
            Self::Block(_) => BlockFact::NAME,
            Self::CfgEdge(_) => CfgEdgeFact::NAME,
            Self::Loop(_) => LoopFact::NAME,
            Self::LoopBlock(_) => LoopBlockFact::NAME,
            Self::InstructionBlock(_) => InstructionBlockFact::NAME,
            Self::InstructionExtent(_) => InstructionExtentFact::NAME,
            Self::LoopMembership(_) => LoopMembershipFact::NAME,
            Self::InlineContext(_) => InlineContextFact::NAME,
            Self::Opcode(_) => OpcodeFact::NAME,
            Self::GasCost(_) => GasCostFact::NAME,
            Self::DisplayName(_) => DisplayNameFact::NAME,
            Self::ValueProperty(_) => ValuePropertyFact::NAME,
            Self::SourceFile(_) => SourceFileFact::NAME,
            Self::SourceSpan(_) => SourceSpanFact::NAME,
            Self::CodeObject(_) => CodeObjectFact::NAME,
            Self::Function(_) => FunctionFact::NAME,
            Self::LexicalScope(_) => LexicalScopeFact::NAME,
            Self::Type(_) => TypeFact::NAME,
            Self::Variable(_) => VariableFact::NAME,
            Self::LocationRange(_) => LocationRangeFact::NAME,
            Self::StaticGas(_) => StaticGasFact::NAME,
            Self::DynamicGasStep(_) => DynamicGasStepFact::NAME,
            Self::ShapePolicy(_) => ShapePolicyFact::NAME,
            Self::ShapeNodeHash(_) => ShapeNodeHashFact::NAME,
            Self::ShapeComponentHash(_) => ShapeComponentHashFact::NAME,
            Self::ShapeGraphHash(_) => ShapeGraphHashFact::NAME,
        }
    }

    pub fn base_relation_schema(&self) -> RelationSchema {
        match self {
            Self::OriginNode(_) => OriginNodeFact::schema(),
            Self::OriginEdge(_) => OriginEdgeFact::schema(),
            Self::CompilerEvent(_) => CompilerEventFact::schema(),
            Self::Storage(_) => StorageFact::schema(),
            Self::Instruction(_) => InstructionFact::schema(),
            Self::InstructionCategory(_) => InstructionCategoryFact::schema(),
            Self::Block(_) => BlockFact::schema(),
            Self::CfgEdge(_) => CfgEdgeFact::schema(),
            Self::Loop(_) => LoopFact::schema(),
            Self::LoopBlock(_) => LoopBlockFact::schema(),
            Self::InstructionBlock(_) => InstructionBlockFact::schema(),
            Self::InstructionExtent(_) => InstructionExtentFact::schema(),
            Self::LoopMembership(_) => LoopMembershipFact::schema(),
            Self::InlineContext(_) => InlineContextFact::schema(),
            Self::Opcode(_) => OpcodeFact::schema(),
            Self::GasCost(_) => GasCostFact::schema(),
            Self::DisplayName(_) => DisplayNameFact::schema(),
            Self::ValueProperty(_) => ValuePropertyFact::schema(),
            Self::SourceFile(_) => SourceFileFact::schema(),
            Self::SourceSpan(_) => SourceSpanFact::schema(),
            Self::CodeObject(_) => CodeObjectFact::schema(),
            Self::Function(_) => FunctionFact::schema(),
            Self::LexicalScope(_) => LexicalScopeFact::schema(),
            Self::Type(_) => TypeFact::schema(),
            Self::Variable(_) => VariableFact::schema(),
            Self::LocationRange(_) => LocationRangeFact::schema(),
            Self::StaticGas(_) => StaticGasFact::schema(),
            Self::DynamicGasStep(_) => DynamicGasStepFact::schema(),
            Self::ShapePolicy(_) => ShapePolicyFact::schema(),
            Self::ShapeNodeHash(_) => ShapeNodeHashFact::schema(),
            Self::ShapeComponentHash(_) => ShapeComponentHashFact::schema(),
            Self::ShapeGraphHash(_) => ShapeGraphHashFact::schema(),
        }
    }

    pub fn base_relation_row(&self) -> RelationRow {
        match self {
            Self::OriginNode(fact) => fact.row(),
            Self::OriginEdge(fact) => fact.row(),
            Self::CompilerEvent(fact) => fact.row(),
            Self::Storage(fact) => fact.row(),
            Self::Instruction(fact) => fact.row(),
            Self::InstructionCategory(fact) => fact.row(),
            Self::Block(fact) => fact.row(),
            Self::CfgEdge(fact) => fact.row(),
            Self::Loop(fact) => fact.row(),
            Self::LoopBlock(fact) => fact.row(),
            Self::InstructionBlock(fact) => fact.row(),
            Self::InstructionExtent(fact) => fact.row(),
            Self::LoopMembership(fact) => fact.row(),
            Self::InlineContext(fact) => fact.row(),
            Self::Opcode(fact) => fact.row(),
            Self::GasCost(fact) => fact.row(),
            Self::DisplayName(fact) => fact.row(),
            Self::ValueProperty(fact) => fact.row(),
            Self::SourceFile(fact) => fact.row(),
            Self::SourceSpan(fact) => fact.row(),
            Self::CodeObject(fact) => fact.row(),
            Self::Function(fact) => fact.row(),
            Self::LexicalScope(fact) => fact.row(),
            Self::Type(fact) => fact.row(),
            Self::Variable(fact) => fact.row(),
            Self::LocationRange(fact) => fact.row(),
            Self::StaticGas(fact) => fact.row(),
            Self::DynamicGasStep(fact) => fact.row(),
            Self::ShapePolicy(fact) => fact.row(),
            Self::ShapeNodeHash(fact) => fact.row(),
            Self::ShapeComponentHash(fact) => fact.row(),
            Self::ShapeGraphHash(fact) => fact.row(),
        }
    }
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

impl TraceRelation for OriginNodeFact {
    const NAME: &'static str = "base_origin_node";

    fn schema() -> RelationSchema {
        schema!(Self::NAME, ["node": Key, "kind": Text])
    }

    fn row(&self) -> RelationRow {
        row!(Self::NAME, [key(&self.key), self.kind().to_string()])
    }
}

impl TraceRelation for OriginEdgeFact {
    const NAME: &'static str = "base_origin_edge";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["from": Key, "to": Key, "label": Text, "introduced_by": OptionalText]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.from),
                key(&self.to),
                value(&self.label),
                opt_value(self.introduced_by.as_ref()),
            ]
        )
    }
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

impl TraceRelation for InstructionFact {
    const NAME: &'static str = "base_instruction";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            ["instruction": Key, "function": Key, "index": U32, "mnemonic": Text]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.instruction),
                key(&self.function),
                self.index.to_string(),
                self.mnemonic.clone(),
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

impl TraceRelation for InstructionExtentFact {
    const NAME: &'static str = "base_instruction_extent";

    fn schema() -> RelationSchema {
        schema!(
            Self::NAME,
            [
                "instruction": Key,
                "code_object": Key,
                "pc_start": U32,
                "pc_end": U32,
                "byte_len": U32,
            ]
        )
    }

    fn row(&self) -> RelationRow {
        row!(
            Self::NAME,
            [
                key(&self.instruction),
                key(&self.code_object),
                self.pc_range.start.to_string(),
                self.pc_range.end.to_string(),
                self.byte_len.to_string(),
            ]
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

fn key(key: &OriginExportKey) -> String {
    key.canonical_storage_key()
}

fn opt_key(key: Option<&OriginExportKey>) -> String {
    key.map_or_else(String::new, OriginExportKey::canonical_storage_key)
}

fn key_list(keys: &[OriginExportKey]) -> String {
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

fn opt_value<T>(value: Option<&T>) -> String
where
    T: Serialize,
{
    value.map_or_else(String::new, self::value)
}

fn value<T>(value: &T) -> String
where
    T: Serialize,
{
    match serde_json::to_value(value).expect("trace relation value should serialize") {
        serde_json::Value::String(value) => value,
        value => value.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;

    use crate::{InstructionFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, TraceFact};

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
}
