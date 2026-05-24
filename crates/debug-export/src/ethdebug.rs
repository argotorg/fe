use std::collections::{BTreeMap, BTreeSet};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::model::{
    AttributionConfidence, DebugBundle, DebugCodeObject, DebugInstruction, DebugSourceSpan,
    InstructionClassification,
};

pub const ETHDEBUG_SCHEMA_VERSION: &str = "ethdebug/format/draft-2020-12+fe-instruction-source-v1";

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugArtifact {
    pub schema_version: String,
    pub compilation: EthdebugCompilation,
    pub programs: Vec<EthdebugProgram>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugCompilation {
    pub id: String,
    pub compiler: EthdebugCompiler,
    pub sources: Vec<EthdebugSourceMaterial>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugCompiler {
    pub name: String,
    pub version: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugSourceMaterial {
    pub id: u32,
    pub path: String,
    pub uri: String,
    pub language: String,
    pub content_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugProgram {
    pub id: String,
    pub environment: EthdebugEnvironment,
    pub contract: EthdebugContract,
    #[serde(skip_serializing_if = "Option::is_none")]
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
pub struct EthdebugContract {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugInstruction {
    pub offset: u32,
    pub operation: EthdebugOperation,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<EthdebugInstructionContext>,
    pub fe_origin_key: String,
    pub confidence: String,
    pub classification: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugOperation {
    pub mnemonic: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugInstructionContext {
    pub code: EthdebugSourceRange,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugSourceRange {
    pub source: EthdebugReference,
    pub range: EthdebugByteRange,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugReference {
    pub id: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EthdebugByteRange {
    pub offset: u32,
    pub length: u32,
}

pub fn pinned_ethdebug_schema() -> Value {
    json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$id": ETHDEBUG_SCHEMA_VERSION,
        "title": "Fe ethdebug instruction/source MVP",
        "type": "object",
        "required": ["schema_version", "compilation", "programs"],
        "properties": {
            "schema_version": { "const": ETHDEBUG_SCHEMA_VERSION },
            "compilation": { "type": "object" },
            "programs": {
                "type": "array",
                "items": { "type": "object" }
            }
        }
    })
}

pub fn emit_ethdebug_artifact(bundle: &DebugBundle) -> Result<EthdebugArtifact, String> {
    let source_ids = source_ids(bundle);
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
        sources: bundle
            .sources
            .iter()
            .map(|source| EthdebugSourceMaterial {
                id: *source_ids.get(&source.file_key).unwrap_or(&0),
                path: source.display_name.clone(),
                uri: source.uri.clone(),
                language: "Fe".to_string(),
                content_hash: source.content_hash.clone(),
            })
            .collect(),
    };

    let mut programs = Vec::new();
    let code_objects = runtime_code_objects(bundle);
    if code_objects.is_empty() {
        programs.push(program_for_instructions(
            "program:runtime",
            EthdebugEnvironment::Call,
            "runtime",
            None,
            bundle.instructions.iter().collect(),
            &spans,
            &source_ids,
        ));
    } else {
        for code_object in code_objects {
            let instructions = bundle
                .instructions
                .iter()
                .filter(|instruction| instruction.code_object.as_ref() == Some(&code_object.key))
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
    let mut source_ids = BTreeSet::new();
    for source in &artifact.compilation.sources {
        if !source_ids.insert(source.id) {
            return Err(format!("duplicate ethdebug source id {}", source.id));
        }
        if source.path.trim().is_empty()
            || source.language.trim().is_empty()
            || source.content_hash.trim().is_empty()
        {
            return Err(format!("ethdebug source {} is incomplete", source.id));
        }
    }
    if artifact.programs.is_empty() {
        return Err("ethdebug artifact has no programs".to_string());
    }
    for program in &artifact.programs {
        if program.instructions.is_empty() {
            return Err(format!(
                "ethdebug program {} has no instructions",
                program.id
            ));
        }
        let mut offsets = BTreeSet::new();
        for instruction in &program.instructions {
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

fn source_ids(bundle: &DebugBundle) -> BTreeMap<OriginExportKey, u32> {
    bundle
        .sources
        .iter()
        .enumerate()
        .map(|(index, source)| {
            (
                source.file_key.clone(),
                source.source_id.unwrap_or(index as u32),
            )
        })
        .collect()
}

fn runtime_code_objects(bundle: &DebugBundle) -> Vec<&DebugCodeObject> {
    let runtime = bundle
        .code_objects
        .iter()
        .filter(|code_object| code_object.kind.contains("Runtime"))
        .collect::<Vec<_>>();
    if runtime.is_empty() {
        bundle.code_objects.iter().collect()
    } else {
        runtime
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
        confidence: format!("{:?}", instruction.confidence),
        classification: format!("{:?}", instruction.classification),
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
    if code_object.kind.contains("Runtime") {
        EthdebugEnvironment::Call
    } else {
        EthdebugEnvironment::Create
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
    use trace_facts::PcRange;

    use crate::model::{
        AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugBundle,
        DebugCodeObject, DebugInstruction, DebugSourceFile, DebugSourceSpan,
        InstructionClassification,
    };

    use super::{
        ETHDEBUG_SCHEMA_VERSION, EthdebugEnvironment, emit_ethdebug_artifact,
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
    fn ethdebug_validator_rejects_wrong_schema_version() {
        let mut artifact = emit_ethdebug_artifact(&bundle()).unwrap();
        artifact.schema_version = "wrong".to_string();

        let err = validate_ethdebug_artifact(&artifact).unwrap_err();

        assert!(err.contains("unsupported ethdebug schema version"));
    }

    #[test]
    fn ethdebug_schema_is_pinned_to_draft_2020_12() {
        let schema = pinned_ethdebug_schema();

        assert_eq!(
            schema.get("$schema").and_then(|value| value.as_str()),
            Some("https://json-schema.org/draft/2020-12/schema")
        );
        assert_eq!(
            schema.get("$id").and_then(|value| value.as_str()),
            Some(ETHDEBUG_SCHEMA_VERSION)
        );
    }
}
