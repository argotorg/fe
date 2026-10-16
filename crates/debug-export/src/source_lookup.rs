//! Source lookup shared by execution diagnostics and debug consumers.
//! A source is attached only after matching the exact code bytes and phase.
use std::collections::BTreeMap;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use trace_facts::CodeObjectKind;

use crate::{DebugBundle, DebugInstruction, DebugSourceSpan};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: String,
    pub span: DebugSourceSpan,
    /// Snippet from the verified build-time source, never the current filesystem.
    pub excerpt: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ResolvedInstruction {
    pub instruction: DebugInstruction,
    pub primary: Option<SourceLocation>,
    pub candidates: Vec<SourceLocation>,
}

pub struct SourceLookup<'a> {
    bundle: &'a DebugBundle,
    instructions: BTreeMap<&'a OriginExportKey, Vec<&'a DebugInstruction>>,
    sources: &'a BTreeMap<String, String>,
}

impl<'a> SourceLookup<'a> {
    pub fn new(bundle: &'a DebugBundle, sources: &'a BTreeMap<String, String>) -> Self {
        let mut instructions: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for instruction in &bundle.instructions {
            if let Some(code) = &instruction.code_object {
                instructions.entry(code).or_default().push(instruction);
            }
        }
        for entries in instructions.values_mut() {
            entries.sort_by_key(|instruction| instruction.pc_range.start);
        }
        Self {
            bundle,
            instructions,
            sources,
        }
    }

    /// The caller must associate these bytes with the executed frame. This method
    /// additionally verifies their identity against the compiler's code object.
    pub fn resolve(
        &self,
        code: &OriginExportKey,
        phase: CodeObjectKind,
        bytecode: &[u8],
        pc: u32,
    ) -> Option<ResolvedInstruction> {
        let object = self
            .bundle
            .code_objects
            .iter()
            .find(|object| &object.key == code)?;
        if object.kind != phase
            || object.code_hash.as_deref() != Some(content_hash(bytecode).as_str())
        {
            return None;
        }
        let entries = self.instructions.get(code)?;
        let index = entries
            .partition_point(|entry| entry.pc_range.start <= pc)
            .checked_sub(1)?;
        let instruction = entries[index];
        // EVM PCs point to instruction starts, not immediate bytes of PUSH data.
        if instruction.pc_range.start != pc || !instruction.pc_range.is_valid() {
            return None;
        }
        if index > 0 && entries[index - 1].pc_range.end > pc {
            return None;
        }
        let primary = instruction
            .primary_source
            .as_ref()
            .and_then(|key| self.source(key));
        let candidates = instruction
            .all_origins
            .iter()
            .filter_map(|key| self.source(key))
            .collect();
        Some(ResolvedInstruction {
            instruction: instruction.clone(),
            primary,
            candidates,
        })
    }

    fn source(&self, key: &OriginExportKey) -> Option<SourceLocation> {
        let span = self
            .bundle
            .source_spans
            .iter()
            .find(|span| &span.origin == key)?;
        let file = self
            .bundle
            .sources
            .iter()
            .find(|file| file.file_key == span.file)?;
        let excerpt = self.sources.get(&file.content_hash).and_then(|text| {
            if content_hash(text.as_bytes()) != file.content_hash {
                return None;
            }
            let selected = text.get(span.start_byte as usize..span.end_byte as usize)?;
            Some(selected.chars().take(512).collect())
        });
        Some(SourceLocation {
            file: file.display_name.clone(),
            span: span.clone(),
            excerpt,
        })
    }
}

pub fn content_hash(bytes: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytes).to_hex())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AttributionConfidence, AttributionPolicyVersion, CompilerInfo, DebugCodeObject,
        DebugSourceFile, InstructionClassification,
    };
    use trace_facts::PcRange;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn fixture() -> (DebugBundle, BTreeMap<String, String>, OriginExportKey) {
        let code = key("code.object", "test", "runtime");
        let source = key("source.file", "test", "file");
        let origin = key("hir.expr", "test", "revert");
        let content = "revert()";
        let hash = content_hash(content.as_bytes());
        let bundle = DebugBundle {
            trace_hash: content_hash(b"test"),
            compiler: CompilerInfo {
                commit: "test".into(),
                target: "evm".into(),
                command: vec![],
                flags: vec![],
                input_path: "test.fe".into(),
                data_source: "compiler_emitted".into(),
            },
            sources: vec![DebugSourceFile {
                file_key: source.clone(),
                uri: "file:///test.fe".into(),
                display_name: "test.fe".into(),
                content_hash: hash.clone(),
                source_id: Some(0),
            }],
            source_spans: vec![DebugSourceSpan {
                origin: origin.clone(),
                file: source,
                start_byte: 0,
                end_byte: 8,
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 9,
            }],
            code_objects: vec![DebugCodeObject {
                key: code.clone(),
                kind: CodeObjectKind::EvmRuntimeBytecode,
                owner_function_or_contract: None,
                target: "evm".into(),
                code_hash: Some(content_hash(&[0xfd])),
            }],
            functions: vec![],
            scopes: vec![],
            variables: vec![],
            types: vec![],
            locations: vec![],
            gas: vec![],
            instructions: vec![DebugInstruction {
                key: key("instruction", "test", "0"),
                function: key("function", "test", "f"),
                code_object: Some(code.clone()),
                pc_range: PcRange::new(0, 1),
                opcode_or_mnemonic: "REVERT".into(),
                primary_source: Some(origin.clone()),
                all_origins: vec![origin],
                classification: InstructionClassification::SourceMapped,
                classification_reason: None,
                category: None,
                confidence: AttributionConfidence::High,
            }],
            attribution_policy: AttributionPolicyVersion::PrimarySourceV1,
        };
        (bundle, BTreeMap::from([(hash, content.into())]), code)
    }

    #[test]
    fn verifies_bytes_phase_and_instruction_boundary() {
        let (bundle, sources, code) = fixture();
        let lookup = SourceLookup::new(&bundle, &sources);
        let resolved = lookup
            .resolve(&code, CodeObjectKind::EvmRuntimeBytecode, &[0xfd], 0)
            .unwrap();
        assert_eq!(
            resolved.primary.unwrap().excerpt.as_deref(),
            Some("revert()")
        );
        assert!(
            lookup
                .resolve(&code, CodeObjectKind::EvmCreationBytecode, &[0xfd], 0)
                .is_none()
        );
        assert!(
            lookup
                .resolve(&code, CodeObjectKind::EvmRuntimeBytecode, &[0xfe], 0)
                .is_none()
        );
        assert!(
            lookup
                .resolve(&code, CodeObjectKind::EvmRuntimeBytecode, &[0xfd], 1)
                .is_none()
        );
    }

    #[test]
    fn changed_source_is_not_presented_as_build_source() {
        let (bundle, mut sources, code) = fixture();
        *sources.values_mut().next().unwrap() = "different text".into();
        let lookup = SourceLookup::new(&bundle, &sources);
        let resolved = lookup
            .resolve(&code, CodeObjectKind::EvmRuntimeBytecode, &[0xfd], 0)
            .unwrap();
        assert!(resolved.primary.unwrap().excerpt.is_none());
    }

    #[test]
    fn attribution_uncertainty_is_preserved() {
        for (classification, confidence) in [
            (
                InstructionClassification::Ambiguous,
                AttributionConfidence::Ambiguous,
            ),
            (
                InstructionClassification::Synthetic,
                AttributionConfidence::Unmapped,
            ),
            (
                InstructionClassification::Unmapped,
                AttributionConfidence::Unmapped,
            ),
        ] {
            let (mut bundle, sources, code) = fixture();
            bundle.instructions[0].classification = classification;
            bundle.instructions[0].confidence = confidence;
            bundle.instructions[0].primary_source = None;
            let lookup = SourceLookup::new(&bundle, &sources);
            let resolved = lookup
                .resolve(&code, CodeObjectKind::EvmRuntimeBytecode, &[0xfd], 0)
                .unwrap();
            assert_eq!(resolved.instruction.classification, classification);
            assert_eq!(resolved.instruction.confidence, confidence);
            assert!(resolved.primary.is_none());
        }
    }
}
