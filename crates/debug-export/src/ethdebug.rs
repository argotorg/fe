use std::collections::{BTreeMap, BTreeSet};

use common::origin::OriginExportKey;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Value, json};

use crate::model::{
    AttributionConfidence, DebugBundle, DebugCodeObject, DebugInstruction, DebugSourceFile,
    DebugSourceSpan, InstructionClassification,
};

pub const ETHDEBUG_SCHEMA_VERSION: &str = "ethdebug/format/draft-2020-12+fe-instruction-source-v2";
pub const ETHDEBUG_FALLBACK_PROGRAM_ID: &str = "program:runtime";

const ORIGIN_ATTRIBUTION_HASH_DOMAIN: &[u8] = b"fe-ethdebug-origin-attribution-v1\0";
const EVM_BYTECODE_INSTRUCTION_KIND: &str = "bytecode.pc";

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugArtifact {
    pub schema_version: String,
    pub compilation: EthdebugCompilation,
    pub programs: Vec<EthdebugProgram>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugCompilation {
    pub id: String,
    pub compiler: EthdebugCompiler,
    pub sources: Vec<EthdebugSourceMaterial>,
    pub fe_origin_attribution_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugOriginAttribution {
    pub instruction_key: String,
    pub code_object: Option<String>,
    pub pc_start: u32,
    pub pc_end: u32,
    pub primary_source: Option<String>,
    pub all_origins: Vec<String>,
    pub classification: InstructionClassification,
    pub confidence: AttributionConfidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugCompiler {
    pub name: String,
    pub version: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugSourceMaterial {
    pub id: u32,
    pub path: String,
    pub uri: String,
    pub language: String,
    pub content_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugProgram {
    pub id: String,
    pub environment: EthdebugEnvironment,
    pub contract: EthdebugContract,
    #[serde(
        default,
        deserialize_with = "deserialize_present_option",
        skip_serializing_if = "Option::is_none"
    )]
    pub bytecode_hash: Option<String>,
    pub instructions: Vec<EthdebugInstruction>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EthdebugEnvironment {
    Call,
    Create,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugContract {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugInstruction {
    pub offset: u32,
    pub operation: EthdebugOperation,
    #[serde(
        default,
        deserialize_with = "deserialize_present_option",
        skip_serializing_if = "Option::is_none"
    )]
    pub context: Option<EthdebugInstructionContext>,
    pub fe_origin_key: String,
    pub confidence: AttributionConfidence,
    pub classification: InstructionClassification,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugOperation {
    pub mnemonic: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugInstructionContext {
    pub code: EthdebugSourceRange,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugSourceRange {
    pub source: EthdebugReference,
    pub range: EthdebugByteRange,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugReference {
    pub id: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EthdebugByteRange {
    pub offset: u32,
    pub length: u32,
}

/// Optional wire fields are omitted when absent. If they are present, require
/// the declared value type instead of accepting JSON `null`, which would be
/// outside the pinned schema while still deserializing to `None` by default.
fn deserialize_present_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    T::deserialize(deserializer).map(Some)
}

pub fn pinned_ethdebug_schema() -> Value {
    json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$id": ETHDEBUG_SCHEMA_VERSION,
        "title": "Fe ethdebug instruction/source MVP",
        "type": "object",
        "additionalProperties": false,
        "required": ["schema_version", "compilation", "programs"],
        "properties": {
            "schema_version": { "const": ETHDEBUG_SCHEMA_VERSION },
            "compilation": { "$ref": "#/$defs/compilation" },
            "programs": {
                "type": "array",
                "minItems": 1,
                "items": { "$ref": "#/$defs/program" }
            }
        },
        "$defs": {
            "compilation": {
                "type": "object",
                "additionalProperties": false,
                "required": ["id", "compiler", "sources", "fe_origin_attribution_hash"],
                "properties": {
                    "id": { "type": "string", "minLength": 1 },
                    "compiler": { "$ref": "#/$defs/compiler" },
                    "sources": {
                        "type": "array",
                        "items": { "$ref": "#/$defs/source_material" }
                    },
                    "fe_origin_attribution_hash": {
                        "type": "string",
                        "pattern": "^blake3:[0-9A-Fa-f]{64}$"
                    }
                }
            },
            "compiler": {
                "type": "object",
                "additionalProperties": false,
                "required": ["name", "version"],
                "properties": {
                    "name": { "type": "string", "minLength": 1 },
                    "version": { "type": "string", "minLength": 1 }
                }
            },
            "source_material": {
                "type": "object",
                "additionalProperties": false,
                "required": ["id", "path", "uri", "language", "content_hash"],
                "properties": {
                    "id": { "$ref": "#/$defs/u32" },
                    "path": { "type": "string", "minLength": 1 },
                    "uri": { "type": "string", "minLength": 1 },
                    "language": { "type": "string", "minLength": 1 },
                    "content_hash": { "type": "string", "minLength": 1 }
                }
            },
            "program": {
                "type": "object",
                "additionalProperties": false,
                "required": ["id", "environment", "contract", "instructions"],
                "properties": {
                    "id": { "type": "string", "minLength": 1 },
                    "environment": { "enum": ["call", "create"] },
                    "contract": { "$ref": "#/$defs/contract" },
                    "bytecode_hash": { "type": "string" },
                    "instructions": {
                        "type": "array",
                        "minItems": 1,
                        "items": { "$ref": "#/$defs/instruction" }
                    }
                }
            },
            "contract": {
                "type": "object",
                "additionalProperties": false,
                "required": ["name"],
                "properties": {
                    "name": { "type": "string", "minLength": 1 }
                }
            },
            "instruction": {
                "type": "object",
                "additionalProperties": false,
                "required": [
                    "offset",
                    "operation",
                    "fe_origin_key",
                    "confidence",
                    "classification"
                ],
                "properties": {
                    "offset": { "$ref": "#/$defs/u32" },
                    "operation": { "$ref": "#/$defs/operation" },
                    "context": { "$ref": "#/$defs/instruction_context" },
                    "fe_origin_key": { "type": "string", "minLength": 1 },
                    "confidence": {
                        "enum": ["high", "ambiguous", "unmapped"]
                    },
                    "classification": {
                        "enum": ["source_mapped", "synthetic", "ambiguous", "unmapped"]
                    }
                }
            },
            "operation": {
                "type": "object",
                "additionalProperties": false,
                "required": ["mnemonic"],
                "properties": {
                    "mnemonic": { "type": "string", "minLength": 1 }
                }
            },
            "instruction_context": {
                "type": "object",
                "additionalProperties": false,
                "required": ["code"],
                "properties": {
                    "code": { "$ref": "#/$defs/source_range" }
                }
            },
            "source_range": {
                "type": "object",
                "additionalProperties": false,
                "required": ["source", "range"],
                "properties": {
                    "source": { "$ref": "#/$defs/reference" },
                    "range": { "$ref": "#/$defs/byte_range" }
                }
            },
            "reference": {
                "type": "object",
                "additionalProperties": false,
                "required": ["id"],
                "properties": {
                    "id": { "$ref": "#/$defs/u32" }
                }
            },
            "byte_range": {
                "type": "object",
                "additionalProperties": false,
                "required": ["offset", "length"],
                "properties": {
                    "offset": { "$ref": "#/$defs/u32" },
                    "length": {
                        "type": "integer",
                        "minimum": 1,
                        "maximum": 4294967295u64
                    }
                }
            },
            "u32": {
                "type": "integer",
                "minimum": 0,
                "maximum": 4294967295u64
            }
        }
    })
}

pub fn emit_ethdebug_artifact(bundle: &DebugBundle) -> Result<EthdebugArtifact, String> {
    let projected_instructions = projected_ethdebug_instructions(bundle)?;
    let attribution_index = origin_attribution_index(&projected_instructions);
    let (source_rows, source_ids) = source_registry(bundle);
    let spans = bundle
        .source_spans
        .iter()
        .map(|span| (span.origin.clone(), span))
        .collect::<BTreeMap<_, _>>();
    let compilation = EthdebugCompilation {
        id: bundle.trace_hash.clone(),
        compiler: EthdebugCompiler {
            name: "fe".to_string(),
            version: bundle.compiler.commit.clone(),
        },
        sources: source_rows
            .into_iter()
            .map(|source| EthdebugSourceMaterial {
                id: *source_ids.get(&source.file_key).unwrap_or(&0),
                path: source
                    .uri
                    .strip_prefix("file://")
                    .unwrap_or(&source.display_name)
                    .to_string(),
                uri: source.uri.clone(),
                language: "Fe".to_string(),
                content_hash: source.content_hash.clone(),
            })
            .collect(),
        fe_origin_attribution_hash: ethdebug_origin_attribution_hash(&attribution_index),
    };

    let mut programs = Vec::new();
    let code_objects = evm_code_objects(bundle);
    if code_objects.is_empty() {
        programs.push(program_for_instructions(
            ETHDEBUG_FALLBACK_PROGRAM_ID,
            EthdebugEnvironment::Call,
            "runtime",
            None,
            projected_instructions
                .iter()
                .map(|projected| projected.instruction)
                .collect(),
            &spans,
            &source_ids,
        ));
    } else {
        for code_object in code_objects {
            let instructions = projected_instructions
                .iter()
                .filter(|projected| projected.code_object == Some(&code_object.key))
                .map(|projected| projected.instruction)
                .collect::<Vec<_>>();
            if instructions.is_empty() {
                continue;
            }
            programs.push(program_for_instructions(
                &code_object.key.canonical_storage_key(),
                environment_for(code_object),
                &contract_name(code_object),
                code_object.code_hash.clone(),
                instructions,
                &spans,
                &source_ids,
            ));
        }
    }

    let artifact = EthdebugArtifact {
        schema_version: ETHDEBUG_SCHEMA_VERSION.to_string(),
        compilation,
        programs,
    };
    validate_ethdebug_artifact(&artifact)?;
    Ok(artifact)
}

pub fn validate_ethdebug_artifact(artifact: &EthdebugArtifact) -> Result<(), String> {
    if artifact.schema_version != ETHDEBUG_SCHEMA_VERSION {
        return Err(format!(
            "unsupported ethdebug schema version {}; expected {ETHDEBUG_SCHEMA_VERSION}",
            artifact.schema_version
        ));
    }
    if artifact.compilation.id.trim().is_empty() {
        return Err("ethdebug compilation id is empty".to_string());
    }
    if artifact.compilation.compiler.name.trim().is_empty()
        || artifact.compilation.compiler.version.trim().is_empty()
    {
        return Err("ethdebug compiler identity is incomplete".to_string());
    }
    if !is_blake3_hash_label(&artifact.compilation.fe_origin_attribution_hash) {
        return Err("ethdebug origin attribution hash is not a BLAKE3 digest".to_string());
    }
    let mut source_ids = BTreeSet::new();
    for source in &artifact.compilation.sources {
        if !source_ids.insert(source.id) {
            return Err(format!("duplicate ethdebug source id {}", source.id));
        }
        if source.path.trim().is_empty()
            || source.uri.trim().is_empty()
            || source.language.trim().is_empty()
            || source.content_hash.trim().is_empty()
        {
            return Err(format!("ethdebug source {} is incomplete", source.id));
        }
    }
    if artifact.programs.is_empty() {
        return Err("ethdebug artifact has no programs".to_string());
    }
    let mut program_ids = BTreeSet::new();
    let mut instruction_keys = BTreeSet::new();
    for program in &artifact.programs {
        if program.id.trim().is_empty() || !program_ids.insert(&program.id) {
            return Err(format!(
                "empty or duplicate ethdebug program id {}",
                program.id
            ));
        }
        if program.contract.name.trim().is_empty() {
            return Err(format!(
                "ethdebug program {} has no contract name",
                program.id
            ));
        }
        if program.instructions.is_empty() {
            return Err(format!(
                "ethdebug program {} has no instructions",
                program.id
            ));
        }
        let mut offsets = BTreeSet::new();
        for instruction in &program.instructions {
            OriginExportKey::parse_canonical_storage_key(&instruction.fe_origin_key).map_err(
                |err| {
                    format!(
                        "ethdebug instruction {} has invalid Fe origin key: {err}",
                        instruction.offset
                    )
                },
            )?;
            if !instruction_keys.insert(instruction.fe_origin_key.as_str()) {
                return Err(format!(
                    "duplicate ethdebug instruction key {}",
                    instruction.fe_origin_key
                ));
            }
            let valid_attribution = matches!(
                (instruction.classification, instruction.confidence),
                (
                    InstructionClassification::SourceMapped,
                    AttributionConfidence::High
                ) | (
                    InstructionClassification::Ambiguous,
                    AttributionConfidence::Ambiguous
                ) | (
                    InstructionClassification::Synthetic | InstructionClassification::Unmapped,
                    AttributionConfidence::Unmapped
                )
            );
            if !valid_attribution {
                return Err(format!(
                    "ethdebug instruction {} has inconsistent classification/confidence",
                    instruction.offset
                ));
            }
            let context_required =
                instruction.classification == InstructionClassification::SourceMapped;
            match (context_required, instruction.context.is_some()) {
                (true, false) => {
                    return Err(format!(
                        "ethdebug source-mapped instruction {} requires source context",
                        instruction.offset
                    ));
                }
                (false, true) => {
                    return Err(format!(
                        "ethdebug non-source-mapped instruction {} must not have source context",
                        instruction.offset
                    ));
                }
                _ => {}
            }
            if instruction.operation.mnemonic.trim().is_empty() {
                return Err(format!(
                    "ethdebug instruction {} has an empty mnemonic",
                    instruction.offset
                ));
            }
            if !offsets.insert(instruction.offset) {
                return Err(format!(
                    "duplicate ethdebug instruction offset {} in {}",
                    instruction.offset, program.id
                ));
            }
            if let Some(context) = &instruction.context {
                if !source_ids.contains(&context.code.source.id) {
                    return Err(format!(
                        "ethdebug instruction {} references missing source {}",
                        instruction.offset, context.code.source.id
                    ));
                }
                if context.code.range.length == 0 {
                    return Err(format!(
                        "ethdebug instruction {} has an empty source range",
                        instruction.offset
                    ));
                }
            }
        }
    }
    Ok(())
}

struct ProjectedEthdebugInstruction<'a> {
    instruction: &'a DebugInstruction,
    code_object: Option<&'a OriginExportKey>,
}

fn projected_ethdebug_instructions(
    bundle: &DebugBundle,
) -> Result<Vec<ProjectedEthdebugInstruction<'_>>, String> {
    let instructions = bundle
        .instructions
        .iter()
        .filter(|instruction| instruction.key.kind() == EVM_BYTECODE_INSTRUCTION_KIND)
        .collect::<Vec<_>>();
    if instructions.is_empty() {
        return Err("debug bundle has no EVM bytecode instructions to export".to_string());
    }

    let emitted_code_objects = evm_code_objects(bundle)
        .into_iter()
        .map(|code_object| &code_object.key)
        .collect::<BTreeSet<_>>();
    if emitted_code_objects.is_empty() {
        let instruction_owners = instructions
            .iter()
            .map(|instruction| instruction.key.owner_key())
            .collect::<BTreeSet<_>>();
        let referenced_code_objects = instructions
            .iter()
            .filter_map(|instruction| instruction.code_object.as_ref())
            .collect::<BTreeSet<_>>();
        if instruction_owners.len() > 1 || referenced_code_objects.len() > 1 {
            return Err(
                "cannot export multiple EVM bytecode programs without code object facts"
                    .to_string(),
            );
        }
        return Ok(instructions
            .into_iter()
            .map(|instruction| ProjectedEthdebugInstruction {
                instruction,
                code_object: None,
            })
            .collect());
    }

    instructions
        .into_iter()
        .map(|instruction| {
            let Some(code_object) = instruction
                .code_object
                .as_ref()
                .filter(|code_object| emitted_code_objects.contains(code_object))
            else {
                return Err(format!(
                    "ethdebug instruction {} is not assigned to an emitted EVM code object",
                    instruction.key.canonical_storage_key()
                ));
            };
            Ok(ProjectedEthdebugInstruction {
                instruction,
                code_object: Some(code_object),
            })
        })
        .collect()
}

fn origin_attribution_index(
    instructions: &[ProjectedEthdebugInstruction<'_>],
) -> Vec<EthdebugOriginAttribution> {
    instructions
        .iter()
        .map(|projected| {
            let instruction = projected.instruction;
            EthdebugOriginAttribution {
                instruction_key: instruction.key.canonical_storage_key(),
                code_object: projected
                    .code_object
                    .map(OriginExportKey::canonical_storage_key),
                pc_start: instruction.pc_range.start,
                pc_end: instruction.pc_range.end,
                primary_source: instruction
                    .primary_source
                    .as_ref()
                    .map(OriginExportKey::canonical_storage_key),
                all_origins: instruction
                    .all_origins
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                classification: instruction.classification,
                confidence: instruction.confidence,
            }
        })
        .collect()
}

pub fn ethdebug_origin_attribution_index(
    bundle: &DebugBundle,
) -> Result<Vec<EthdebugOriginAttribution>, String> {
    projected_ethdebug_instructions(bundle)
        .map(|instructions| origin_attribution_index(&instructions))
}

pub fn ethdebug_origin_attribution_hash(index: &[EthdebugOriginAttribution]) -> String {
    let encoded = serde_json::to_vec(index).expect("ethdebug attribution index should serialize");
    let mut hasher = blake3::Hasher::new();
    hasher.update(ORIGIN_ATTRIBUTION_HASH_DOMAIN);
    hasher.update(&encoded);
    format!("blake3:{}", hasher.finalize().to_hex())
}

fn is_blake3_hash_label(value: &str) -> bool {
    value.strip_prefix("blake3:").is_some_and(|digest| {
        digest.len() == 64 && digest.bytes().all(|byte| byte.is_ascii_hexdigit())
    })
}

/// Canonical source registry: one row per distinct file, with every
/// `source.file` key aliased onto its row's id. Two keys minted for the same
/// underlying file (same content hash and same filesystem path, whether the
/// producer spelled it as a file URL or a bare path) must not produce two
/// registry rows, and neither key's references may dangle. Distinct files
/// with coincidentally identical content stay separate rows.
fn source_registry(
    bundle: &DebugBundle,
) -> (Vec<&DebugSourceFile>, BTreeMap<OriginExportKey, u32>) {
    fn uri_path(uri: &str) -> &str {
        uri.strip_prefix("file://").unwrap_or(uri)
    }

    let mut rows: Vec<&DebugSourceFile> = Vec::new();
    let mut row_by_identity: BTreeMap<(&str, &str), usize> = BTreeMap::new();
    let mut row_by_key: BTreeMap<&OriginExportKey, usize> = BTreeMap::new();
    for source in &bundle.sources {
        if row_by_key.contains_key(&source.file_key) {
            continue;
        }
        let row = if source.content_hash.is_empty() {
            rows.push(source);
            rows.len() - 1
        } else {
            *row_by_identity
                .entry((uri_path(&source.uri), source.content_hash.as_str()))
                .or_insert_with(|| {
                    rows.push(source);
                    rows.len() - 1
                })
        };
        row_by_key.insert(&source.file_key, row);
    }

    let mut row_ids = Vec::with_capacity(rows.len());
    let mut used = BTreeSet::new();
    let mut next = 0u32;
    for row in &rows {
        let id = row
            .source_id
            .filter(|id| used.insert(*id))
            .unwrap_or_else(|| {
                while used.contains(&next) {
                    next = next.saturating_add(1);
                }
                let id = next;
                used.insert(id);
                next = next.saturating_add(1);
                id
            });
        row_ids.push(id);
    }

    let ids = row_by_key
        .into_iter()
        .map(|(key, row)| (key.clone(), row_ids[row]))
        .collect();
    (rows, ids)
}

/// Every EVM code object becomes a program: runtime bytecode as a call
/// environment, creation bytecode as a create environment. Creation objects
/// must not be silently dropped just because runtime objects exist.
fn evm_code_objects(bundle: &DebugBundle) -> Vec<&DebugCodeObject> {
    let evm = bundle
        .code_objects
        .iter()
        .filter(|code_object| code_object.kind.to_ascii_lowercase().contains("bytecode"))
        .collect::<Vec<_>>();
    if evm.is_empty() {
        bundle.code_objects.iter().collect()
    } else {
        evm
    }
}

fn program_for_instructions(
    id: &str,
    environment: EthdebugEnvironment,
    contract_name: &str,
    bytecode_hash: Option<String>,
    mut instructions: Vec<&DebugInstruction>,
    spans: &BTreeMap<OriginExportKey, &DebugSourceSpan>,
    source_ids: &BTreeMap<OriginExportKey, u32>,
) -> EthdebugProgram {
    instructions.sort_by_key(|instruction| instruction.pc_range.start);
    EthdebugProgram {
        id: id.to_string(),
        environment,
        contract: EthdebugContract {
            name: contract_name.to_string(),
        },
        bytecode_hash,
        instructions: instructions
            .into_iter()
            .map(|instruction| ethdebug_instruction(instruction, spans, source_ids))
            .collect(),
    }
}

fn ethdebug_instruction(
    instruction: &DebugInstruction,
    spans: &BTreeMap<OriginExportKey, &DebugSourceSpan>,
    source_ids: &BTreeMap<OriginExportKey, u32>,
) -> EthdebugInstruction {
    EthdebugInstruction {
        offset: instruction.pc_range.start,
        operation: EthdebugOperation {
            mnemonic: instruction.opcode_or_mnemonic.clone(),
        },
        context: source_context(instruction, spans, source_ids),
        fe_origin_key: instruction.key.canonical_storage_key(),
        confidence: instruction.confidence,
        classification: instruction.classification,
    }
}

fn source_context(
    instruction: &DebugInstruction,
    spans: &BTreeMap<OriginExportKey, &DebugSourceSpan>,
    source_ids: &BTreeMap<OriginExportKey, u32>,
) -> Option<EthdebugInstructionContext> {
    if instruction.classification != InstructionClassification::SourceMapped
        || instruction.confidence != AttributionConfidence::High
    {
        return None;
    }
    let span = spans.get(instruction.primary_source.as_ref()?)?;
    let source_id = *source_ids.get(&span.file)?;
    Some(EthdebugInstructionContext {
        code: EthdebugSourceRange {
            source: EthdebugReference { id: source_id },
            range: EthdebugByteRange {
                offset: span.start_byte,
                length: span.end_byte.saturating_sub(span.start_byte),
            },
        },
    })
}

fn environment_for(code_object: &DebugCodeObject) -> EthdebugEnvironment {
    if code_object.kind.to_ascii_lowercase().contains("creation") {
        EthdebugEnvironment::Create
    } else {
        EthdebugEnvironment::Call
    }
}

fn contract_name(code_object: &DebugCodeObject) -> String {
    code_object
        .owner_function_or_contract
        .as_ref()
        .map(|key| key.display_label())
        .unwrap_or_else(|| code_object.key.display_label())
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use serde_json::Value;
    use trace_facts::PcRange;

    use crate::model::{
        AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugBundle,
        DebugCodeObject, DebugInstruction, DebugSourceFile, DebugSourceSpan,
        InstructionClassification,
    };

    use super::{
        ETHDEBUG_FALLBACK_PROGRAM_ID, ETHDEBUG_SCHEMA_VERSION, EVM_BYTECODE_INSTRUCTION_KIND,
        EthdebugArtifact, EthdebugEnvironment, emit_ethdebug_artifact,
        ethdebug_origin_attribution_hash, ethdebug_origin_attribution_index,
        pinned_ethdebug_schema, validate_ethdebug_artifact,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn bundle() -> DebugBundle {
        let source_file = key("source.file", "demo", "src/main.fe");
        let source_expr = key("hir.expr", "demo", "expr:add");
        let contract = key("contract", "demo", "Fib");
        let code_object = key("code.object", "demo", "runtime");
        let function = key("function", "demo", "runtime");
        DebugBundle {
            trace_hash: "blake3:00000000000000000000000000000000000000000000000000000000e7deb060"
                .to_string(),
            compiler: CompilerInfo {
                commit: "abc123".to_string(),
                target: "evm/sonatina".to_string(),
                command: vec!["fe".to_string()],
                flags: vec![],
                input_path: "src/main.fe".to_string(),
                data_source: "compiler_emitted".to_string(),
            },
            sources: vec![DebugSourceFile {
                file_key: source_file.clone(),
                uri: "file:///src/main.fe".to_string(),
                display_name: "src/main.fe".to_string(),
                content_hash:
                    "blake3:0000000000000000000000000000000000000000000000000000000000001234"
                        .to_string(),
                source_id: Some(7),
            }],
            source_spans: vec![DebugSourceSpan {
                origin: source_expr.clone(),
                file: source_file,
                start_byte: 10,
                end_byte: 14,
                start_line: 2,
                start_column: 3,
                end_line: 2,
                end_column: 7,
            }],
            code_objects: vec![DebugCodeObject {
                key: code_object.clone(),
                kind: "EvmRuntimeBytecode".to_string(),
                owner_function_or_contract: Some(contract),
                target: "evm/sonatina".to_string(),
                code_hash: Some(
                    "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                        .to_string(),
                ),
            }],
            functions: vec![],
            scopes: vec![],
            variables: vec![],
            types: vec![],
            instructions: vec![
                DebugInstruction {
                    key: key("bytecode.pc", "demo", "pc:0"),
                    function: function.clone(),
                    code_object: Some(code_object.clone()),
                    pc_range: PcRange::new(0, 1),
                    opcode_or_mnemonic: "ADD".to_string(),
                    primary_source: Some(source_expr),
                    all_origins: vec![],
                    classification: InstructionClassification::SourceMapped,
                    classification_reason: None,
                    category: None,
                    confidence: AttributionConfidence::High,
                },
                DebugInstruction {
                    key: key("bytecode.pc", "demo", "pc:1"),
                    function,
                    code_object: Some(code_object),
                    pc_range: PcRange::new(1, 2),
                    opcode_or_mnemonic: "PUSH0".to_string(),
                    primary_source: None,
                    all_origins: vec![],
                    classification: InstructionClassification::Synthetic,
                    classification_reason: Some("test".to_string()),
                    category: None,
                    confidence: AttributionConfidence::Unmapped,
                },
            ],
            locations: vec![],
            gas: vec![],
            attribution_policy: AttributionPolicyVersion::PrimarySourceV1,
        }
    }

    #[test]
    fn every_evm_code_object_becomes_a_program_with_its_environment() {
        let mut bundle = bundle();
        let creation = key("code.object", "demo", "creation");
        let creation_function = key("function", "demo", "creation");
        bundle.code_objects.push(DebugCodeObject {
            key: creation.clone(),
            kind: "evm_creation_bytecode".to_string(),
            owner_function_or_contract: None,
            target: "evm/sonatina".to_string(),
            code_hash: None,
        });
        bundle.instructions.push(DebugInstruction {
            key: key("bytecode.pc", "demo", "init-pc:0"),
            function: creation_function,
            code_object: Some(creation),
            pc_range: PcRange::new(0, 1),
            opcode_or_mnemonic: "PUSH0".to_string(),
            primary_source: None,
            all_origins: vec![],
            classification: InstructionClassification::Synthetic,
            classification_reason: None,
            category: None,
            confidence: AttributionConfidence::Unmapped,
        });

        let artifact = emit_ethdebug_artifact(&bundle).unwrap();
        validate_ethdebug_artifact(&artifact).unwrap();

        assert_eq!(
            artifact.programs.len(),
            2,
            "creation bytecode must not be silently dropped"
        );
        let environments = artifact
            .programs
            .iter()
            .map(|program| program.environment)
            .collect::<Vec<_>>();
        assert!(environments.contains(&EthdebugEnvironment::Call));
        assert!(environments.contains(&EthdebugEnvironment::Create));
    }

    #[test]
    fn ethdebug_artifact_emits_sources_program_and_instruction_ranges() {
        let artifact = emit_ethdebug_artifact(&bundle()).unwrap();

        assert_eq!(artifact.schema_version, ETHDEBUG_SCHEMA_VERSION);
        assert_eq!(artifact.compilation.sources[0].id, 7);
        assert_eq!(artifact.programs.len(), 1);
        assert_eq!(artifact.programs[0].environment, EthdebugEnvironment::Call);
        assert_eq!(artifact.programs[0].instructions.len(), 2);
        assert!(artifact.programs[0].instructions[0].context.is_some());
        assert!(artifact.programs[0].instructions[1].context.is_none());
        let code = &artifact.programs[0].instructions[0]
            .context
            .as_ref()
            .unwrap()
            .code;
        assert_eq!(code.source.id, 7);
        assert_eq!(code.range.offset, 10);
        assert_eq!(code.range.length, 4);
    }

    #[test]
    fn ethdebug_artifact_dedupes_source_files_and_allocates_unique_ids() {
        let mut bundle = bundle();
        let duplicate = bundle.sources[0].clone();
        let mut conflicting = duplicate.clone();
        conflicting.file_key = key("source.file", "demo", "src/lib.fe");
        conflicting.uri = "file:///src/lib.fe".to_string();
        conflicting.display_name = "src/lib.fe".to_string();
        conflicting.source_id = duplicate.source_id;
        bundle.sources.push(duplicate);
        bundle.sources.push(conflicting);

        let artifact = emit_ethdebug_artifact(&bundle).unwrap();
        validate_ethdebug_artifact(&artifact).unwrap();

        assert_eq!(artifact.compilation.sources.len(), 2);
        assert_eq!(artifact.compilation.sources[0].id, 7);
        assert_ne!(
            artifact.compilation.sources[0].id,
            artifact.compilation.sources[1].id
        );
    }

    #[test]
    fn ethdebug_validator_enforces_attribution_context_matrix() {
        let artifact = emit_ethdebug_artifact(&bundle()).unwrap();
        let source_context = artifact.programs[0].instructions[0]
            .context
            .clone()
            .expect("fixture source instruction should have context");

        let valid = [
            (
                InstructionClassification::SourceMapped,
                AttributionConfidence::High,
                Some(source_context.clone()),
            ),
            (
                InstructionClassification::Ambiguous,
                AttributionConfidence::Ambiguous,
                None,
            ),
            (
                InstructionClassification::Synthetic,
                AttributionConfidence::Unmapped,
                None,
            ),
            (
                InstructionClassification::Unmapped,
                AttributionConfidence::Unmapped,
                None,
            ),
        ];
        for (classification, confidence, context) in valid {
            let mut candidate = artifact.clone();
            let instruction = &mut candidate.programs[0].instructions[0];
            instruction.classification = classification;
            instruction.confidence = confidence;
            instruction.context = context;
            assert!(
                validate_ethdebug_artifact(&candidate).is_ok(),
                "expected {classification:?}/{confidence:?} to accept its canonical context"
            );
        }

        let invalid = [
            (
                InstructionClassification::SourceMapped,
                AttributionConfidence::High,
                None,
            ),
            (
                InstructionClassification::Synthetic,
                AttributionConfidence::Unmapped,
                Some(source_context.clone()),
            ),
            (
                InstructionClassification::Ambiguous,
                AttributionConfidence::Ambiguous,
                Some(source_context.clone()),
            ),
            (
                InstructionClassification::Unmapped,
                AttributionConfidence::Unmapped,
                Some(source_context.clone()),
            ),
            (
                InstructionClassification::SourceMapped,
                AttributionConfidence::Unmapped,
                Some(source_context),
            ),
        ];
        for (classification, confidence, context) in invalid {
            let mut candidate = artifact.clone();
            let instruction = &mut candidate.programs[0].instructions[0];
            instruction.classification = classification;
            instruction.confidence = confidence;
            instruction.context = context;
            assert!(
                validate_ethdebug_artifact(&candidate).is_err(),
                "expected {classification:?}/{confidence:?} to reject contradictory context"
            );
        }
    }

    #[test]
    fn ethdebug_validator_rejects_wrong_schema_version() {
        let mut artifact = emit_ethdebug_artifact(&bundle()).unwrap();
        artifact.schema_version = "wrong".to_string();

        let err = validate_ethdebug_artifact(&artifact).unwrap_err();

        assert!(err.contains("unsupported ethdebug schema version"));
    }

    #[test]
    fn ethdebug_schema_describes_complete_wire_shape() {
        let schema = pinned_ethdebug_schema();

        assert_eq!(
            schema.get("$schema").and_then(|value| value.as_str()),
            Some("https://json-schema.org/draft/2020-12/schema")
        );
        assert_eq!(
            schema.get("$id").and_then(|value| value.as_str()),
            Some(ETHDEBUG_SCHEMA_VERSION)
        );
        assert_eq!(
            schema.get("additionalProperties"),
            Some(&Value::Bool(false))
        );
        assert_eq!(
            schema["properties"]["compilation"]["$ref"].as_str(),
            Some("#/$defs/compilation")
        );
        assert_eq!(
            schema["properties"]["programs"]["items"]["$ref"].as_str(),
            Some("#/$defs/program")
        );

        let definitions = schema["$defs"]
            .as_object()
            .expect("schema should provide reusable definitions");
        for name in [
            "compilation",
            "compiler",
            "source_material",
            "program",
            "contract",
            "instruction",
            "operation",
            "instruction_context",
            "source_range",
            "reference",
            "byte_range",
        ] {
            assert_eq!(
                definitions[name].get("additionalProperties"),
                Some(&Value::Bool(false)),
                "{name} must reject unknown properties"
            );
        }
        assert_eq!(
            definitions["instruction"]["properties"]["context"]["$ref"].as_str(),
            Some("#/$defs/instruction_context")
        );
        assert_eq!(
            definitions["byte_range"]["properties"]["length"]["minimum"].as_u64(),
            Some(1)
        );
    }

    #[test]
    fn ethdebug_wire_rejects_explicit_null_for_omittable_fields() {
        let artifact = emit_ethdebug_artifact(&bundle()).unwrap();

        let mut null_bytecode_hash = serde_json::to_value(&artifact).unwrap();
        null_bytecode_hash["programs"][0]["bytecode_hash"] = Value::Null;
        let err = serde_json::from_value::<EthdebugArtifact>(null_bytecode_hash).unwrap_err();
        assert!(err.to_string().contains("invalid type: null"));

        let mut null_context = serde_json::to_value(&artifact).unwrap();
        null_context["programs"][0]["instructions"][1]["context"] = Value::Null;
        let err = serde_json::from_value::<EthdebugArtifact>(null_context).unwrap_err();
        assert!(err.to_string().contains("invalid type: null"));

        let roundtrip =
            serde_json::from_value::<EthdebugArtifact>(serde_json::to_value(&artifact).unwrap())
                .unwrap();
        assert_eq!(
            roundtrip, artifact,
            "omitted optional fields must remain valid"
        );
    }

    #[test]
    fn ethdebug_wire_rejects_unknown_or_missing_nested_fields() {
        let artifact = emit_ethdebug_artifact(&bundle()).unwrap();

        let mut unknown = serde_json::to_value(&artifact).unwrap();
        unknown["programs"][0]["instructions"][0]["operation"]["unexpected"] =
            Value::String("x".to_string());
        let err = serde_json::from_value::<EthdebugArtifact>(unknown).unwrap_err();
        assert!(err.to_string().contains("unknown field"));

        let mut missing = serde_json::to_value(&artifact).unwrap();
        missing["programs"][0]["instructions"][0]["operation"]
            .as_object_mut()
            .unwrap()
            .remove("mnemonic");
        let err = serde_json::from_value::<EthdebugArtifact>(missing).unwrap_err();
        assert!(err.to_string().contains("missing field"));
    }

    #[test]
    fn artifact_and_index_ignore_non_bytecode_instructions() {
        let mut bundle = bundle();
        bundle.instructions.push(DebugInstruction {
            key: key("runtime.stmt", "demo", "block:0:stmt:0"),
            function: key("runtime.function", "demo", "runtime"),
            code_object: None,
            pc_range: PcRange::new(0, 1),
            opcode_or_mnemonic: "assign".to_string(),
            primary_source: None,
            all_origins: vec![],
            classification: InstructionClassification::Unmapped,
            classification_reason: None,
            category: None,
            confidence: AttributionConfidence::Unmapped,
        });

        let artifact = emit_ethdebug_artifact(&bundle).unwrap();
        let attribution_index = ethdebug_origin_attribution_index(&bundle).unwrap();

        assert_eq!(artifact.programs[0].instructions.len(), 2);
        assert_eq!(attribution_index.len(), 2);
        assert!(
            attribution_index
                .iter()
                .all(|instruction| instruction.instruction_key.starts_with("bytecode.pc"))
        );
        assert_eq!(
            artifact.compilation.fe_origin_attribution_hash,
            ethdebug_origin_attribution_hash(&attribution_index)
        );
    }

    #[test]
    fn artifact_without_code_objects_uses_the_fallback_program() {
        let mut bundle = bundle();
        bundle.code_objects.clear();
        bundle.instructions.push(DebugInstruction {
            key: key("runtime.stmt", "demo", "block:0:stmt:0"),
            function: key("runtime.function", "demo", "runtime"),
            code_object: None,
            pc_range: PcRange::new(0, 1),
            opcode_or_mnemonic: "assign".to_string(),
            primary_source: None,
            all_origins: vec![],
            classification: InstructionClassification::Unmapped,
            classification_reason: None,
            category: None,
            confidence: AttributionConfidence::Unmapped,
        });

        let artifact = emit_ethdebug_artifact(&bundle).unwrap();
        let attribution_index = ethdebug_origin_attribution_index(&bundle).unwrap();

        assert_eq!(artifact.programs.len(), 1);
        assert_eq!(artifact.programs[0].id, ETHDEBUG_FALLBACK_PROGRAM_ID);
        assert_eq!(artifact.programs[0].instructions.len(), 2);
        assert_eq!(attribution_index.len(), 2);
        assert!(
            attribution_index
                .iter()
                .all(|instruction| instruction.code_object.is_none())
        );
        validate_ethdebug_artifact(&artifact).unwrap();
    }

    #[test]
    fn artifact_rejects_multiple_fallback_programs_without_code_object_facts() {
        let mut bundle = bundle();
        bundle.code_objects.clear();
        let mut other_program_instruction = bundle.instructions[0].clone();
        other_program_instruction.key = key("bytecode.pc", "other", "pc:2");
        other_program_instruction.code_object = Some(key("code.object", "other", "runtime"));
        other_program_instruction.pc_range = PcRange::new(2, 3);
        bundle.instructions.push(other_program_instruction);

        let err = emit_ethdebug_artifact(&bundle).unwrap_err();

        assert!(err.contains("multiple EVM bytecode programs"));
    }

    #[test]
    fn artifact_validator_rejects_duplicate_instruction_keys_without_sidecar() {
        let mut artifact = emit_ethdebug_artifact(&bundle()).unwrap();
        artifact.programs[0].instructions[1].fe_origin_key =
            artifact.programs[0].instructions[0].fe_origin_key.clone();

        let err = validate_ethdebug_artifact(&artifact).unwrap_err();

        assert!(err.contains("duplicate ethdebug instruction key"));
    }

    #[test]
    fn artifact_rejects_bundle_without_bytecode_instructions() {
        let mut bundle = bundle();
        bundle
            .instructions
            .retain(|instruction| instruction.key.kind() != EVM_BYTECODE_INSTRUCTION_KIND);

        let err = emit_ethdebug_artifact(&bundle).unwrap_err();

        assert!(err.contains("no EVM bytecode instructions"));
    }

    #[test]
    fn artifact_rejects_partial_code_object_assignment() {
        let mut bundle = bundle();
        bundle.instructions[0].code_object = None;

        let err = emit_ethdebug_artifact(&bundle).unwrap_err();

        assert!(err.contains("is not assigned to an emitted EVM code object"));
    }
}
