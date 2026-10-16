//! Verified artifact identities used by test execution diagnostics.
use std::collections::BTreeMap;

use codegen::{TestDebugCode, TestDebugInfo};
use contract_harness::execution_trace::{
    CaptureLimits, CodePrefixHash, ExecutionFrame, ExecutionTrace,
};
use debug_export::{DebugBundle, ResolvedInstruction, SourceLookup};
use ethers_core::utils::keccak256;
use trace_facts::{CodeObjectKind, TraceBundle, TraceMetadata, TraceSnapshot};

pub(super) struct ArtifactRegistry<'a> {
    info: &'a TestDebugInfo,
    pub debug: DebugBundle,
    /// Full hashes of actual deployed code, including verified immutable tails.
    runtime: BTreeMap<String, Vec<usize>>,
}

fn code_hash(bytes: &[u8]) -> String {
    format!("0x{}", hex::encode(keccak256(bytes)))
}

fn prefix_matches(prefixes: &[CodePrefixHash], bytes: &[u8]) -> bool {
    let hash = code_hash(bytes);
    prefixes
        .iter()
        .any(|prefix| prefix.length == bytes.len() && prefix.hash == hash)
}

impl<'a> ArtifactRegistry<'a> {
    pub fn new(info: &'a TestDebugInfo, metadata: TraceMetadata) -> Result<Self, String> {
        let snapshot = TraceSnapshot::new(TraceBundle::new(metadata, info.facts.clone()))
            .map_err(|error| format!("invalid test debug facts: {error}"))?;
        let debug = DebugBundle::from_snapshot(&snapshot);
        let mut registry = Self {
            info,
            debug,
            runtime: BTreeMap::new(),
        };
        for (index, code) in info.codes.iter().enumerate() {
            // A template requiring immutable data is not a deployed runtime.
            if code.immutable_tail_bytes == 0 {
                registry
                    .runtime
                    .entry(code_hash(&code.runtime))
                    .or_default()
                    .push(index);
            }
        }
        Ok(registry)
    }

    pub fn capture_limits(&self, mut limits: CaptureLimits) -> CaptureLimits {
        limits.code_prefix_lengths = self
            .info
            .codes
            .iter()
            .flat_map(|code| [code.deploy.len(), code.runtime.len()])
            .collect();
        limits
    }

    fn creation_index(&self, frame: &ExecutionFrame) -> Option<usize> {
        if frame.phase != "create" {
            return None;
        }
        let mut matches = self.info.codes.iter().enumerate().filter(|(_, code)| {
            frame.code_hash.as_deref() == Some(code_hash(&code.deploy).as_str())
                || (frame.code_len.is_some_and(|len| len >= code.deploy.len())
                    && prefix_matches(&frame.code_prefix_hashes, &code.deploy))
        });
        let (index, _) = matches.next()?;
        // Different source artifacts may compile to identical bytes. Do not pick one.
        matches.next().is_none().then_some(index)
    }

    pub fn observe_deployments(&mut self, trace: &ExecutionTrace) {
        for frame in &trace.frames {
            if frame.success != Some(true) {
                continue;
            }
            let Some(index) = self.creation_index(frame) else {
                continue;
            };
            let code = &self.info.codes[index];
            let Some(hash) = &frame.deployed_code_hash else {
                continue;
            };
            let Some(expected_len) = code.runtime.len().checked_add(code.immutable_tail_bytes)
            else {
                continue;
            };
            if frame.deployed_code_len != Some(expected_len) {
                continue;
            }
            let matches = if code.immutable_tail_bytes == 0 {
                *hash == code_hash(&code.runtime)
            } else {
                prefix_matches(&frame.deployed_code_prefix_hashes, &code.runtime)
            };
            if matches {
                let candidates = self.runtime.entry(hash.clone()).or_default();
                if !candidates.contains(&index) {
                    candidates.push(index);
                }
            }
        }
    }

    pub fn code(&self, frame: &ExecutionFrame) -> Option<&TestDebugCode> {
        let index = if frame.phase == "create" {
            self.creation_index(frame)?
        } else if frame.phase == "runtime" {
            let candidates = self.runtime.get(frame.code_hash.as_ref()?)?;
            if candidates.len() != 1 {
                return None;
            }
            candidates[0]
        } else {
            return None;
        };
        self.info.codes.get(index)
    }

    pub fn resolve(
        &self,
        lookup: &SourceLookup<'_>,
        frame: &ExecutionFrame,
        pc: usize,
    ) -> Option<ResolvedInstruction> {
        let code = self.code(frame)?;
        let (key, phase, bytes) = if frame.phase == "create" {
            (
                &code.create_key,
                CodeObjectKind::EvmCreationBytecode,
                &code.deploy,
            )
        } else {
            (
                &code.runtime_key,
                CodeObjectKind::EvmRuntimeBytecode,
                &code.runtime,
            )
        };
        // The resolver verifies these compiler bytes against their debug facts;
        // the registry above verifies the executed bytes against the compiler.
        lookup.resolve(key, phase, bytes, u32::try_from(pc).ok()?)
    }
}

pub(super) struct FailureDiagnostics<'a> {
    pub registry: Result<ArtifactRegistry<'a>, String>,
    pub traces: Vec<ExecutionTrace>,
    pub limits: CaptureLimits,
}

impl<'a> FailureDiagnostics<'a> {
    pub fn new(
        info: &'a TestDebugInfo,
        name: &str,
        steps: Option<&contract_harness::EvmTraceOptions>,
    ) -> Self {
        let metadata = TraceMetadata::compiler_emitted(
            option_env!("FE_GIT_COMMIT").unwrap_or("unknown"),
            "evm",
            vec!["test".into()],
            name,
            info.compiler_flags
                .iter()
                .cloned()
                .chain([
                    format!("optimization={}", info.optimization),
                    "--explain-failure".into(),
                ])
                .collect(),
        );
        let registry = ArtifactRegistry::new(info, metadata);
        let mut limits = CaptureLimits::default();
        if let Some(steps) = steps {
            limits.steps = steps.keep_steps;
            limits.stack_items = steps.stack_n;
        }
        if let Ok(registry) = &registry {
            limits = registry.capture_limits(limits);
        }
        Self {
            registry,
            traces: vec![],
            limits,
        }
    }

    pub fn record(&mut self, trace: ExecutionTrace) {
        if let Ok(registry) = &mut self.registry {
            registry.observe_deployments(&trace);
        }
        self.traces.push(trace);
    }

    pub fn render(&self) -> String {
        use std::fmt::Write;
        let mut out =
            String::from("Execution diagnostics (observed calls; no inferred root cause):\n");
        if let Err(error) = &self.registry {
            let _ = writeln!(out, "  Source mapping unavailable: {error}");
        }
        let lookup = self
            .registry
            .as_ref()
            .ok()
            .map(|registry| SourceLookup::new(&registry.debug, &registry.info.sources));
        for (index, trace) in self.traces.iter().enumerate() {
            let _ = writeln!(
                out,
                "  {}:",
                if index == 0 {
                    "Test deployment"
                } else {
                    "Test execution"
                }
            );
            let mut depths = BTreeMap::new();
            for frame in &trace.frames {
                let depth = frame
                    .parent
                    .and_then(|parent| depths.get(&parent).copied())
                    .map_or(0, |depth: usize| depth + 1);
                depths.insert(frame.id, depth);
                let indent = "  ".repeat(depth.min(32) + 2);
                let code = self
                    .registry
                    .as_ref()
                    .ok()
                    .and_then(|registry| registry.code(frame));
                let name = code
                    .map(|code| code.name.as_str())
                    .unwrap_or("unknown code");
                let _ = writeln!(
                    out,
                    "{indent}#{} {} {name}: {} [{}]",
                    frame.id,
                    frame.kind,
                    frame.outcome.as_deref().unwrap_or("unfinished"),
                    frame.execution_kind
                );
                if frame.input.truncated() || frame.output.truncated() {
                    let _ = writeln!(
                        out,
                        "{indent}  Payload capture truncated: input {}/{}, output {}/{} bytes",
                        frame.input.hex.len() / 2,
                        frame.input.original_len,
                        frame.output.hex.len() / 2,
                        frame.output.original_len
                    );
                }
                if let Some(address) = &frame.code_address {
                    let _ = writeln!(
                        out,
                        "{indent}  code={address} context={}",
                        frame.context_address.as_deref().unwrap_or("unknown")
                    );
                }
                if frame.success == Some(false) {
                    let _ = writeln!(
                        out,
                        "{indent}  Output: 0x{}{}",
                        frame.output.hex,
                        if frame.output.truncated() {
                            " [truncated]"
                        } else {
                            ""
                        }
                    );
                    if let Some(decoded) = decode_standard_error(&frame.output).or_else(|| {
                        code.and_then(|code| {
                            super::failure_decode::decode_custom_error(
                                &frame.output,
                                &code.custom_errors,
                            )
                        })
                    }) {
                        let _ = writeln!(out, "{indent}  Decoded payload: {decoded}");
                    }
                    if frame
                        .parent
                        .and_then(|id| trace.frames.iter().find(|parent| parent.id == id))
                        .is_some_and(|parent| parent.success == Some(true))
                    {
                        let _ = writeln!(
                            out,
                            "{indent}  Enclosing call succeeded after this failure."
                        );
                    }
                }
                if let Some(terminal) = frame.terminal {
                    let _ = writeln!(
                        out,
                        "{indent}  Terminal: pc=0x{:x} opcode=0x{:02x}",
                        terminal.pc, terminal.opcode
                    );
                    if let Some(resolved) =
                        self.registry.as_ref().ok().and_then(|registry| {
                            registry.resolve(lookup.as_ref()?, frame, terminal.pc)
                        })
                    {
                        let class = serde_json::to_value(resolved.instruction.classification)
                            .unwrap_or_default();
                        let confidence = serde_json::to_value(resolved.instruction.confidence)
                            .unwrap_or_default();
                        let _ = writeln!(
                            out,
                            "{indent}  Attribution: {class}, confidence={confidence}"
                        );
                        if let Some(reason) = &resolved.instruction.classification_reason {
                            let _ = writeln!(out, "{indent}  Attribution reason: {reason}");
                        }
                        if let Some(source) = &resolved.primary {
                            let _ = writeln!(
                                out,
                                "{indent}  Source: {}:{}:{}",
                                source.file, source.span.start_line, source.span.start_column
                            );
                            if let Some(excerpt) = &source.excerpt {
                                let _ = writeln!(out, "{indent}    {}", excerpt.escape_debug());
                            }
                        }
                        for candidate in resolved.candidates {
                            if resolved
                                .primary
                                .as_ref()
                                .is_some_and(|primary| primary.span.origin == candidate.span.origin)
                            {
                                continue;
                            }
                            let _ = writeln!(
                                out,
                                "{indent}  Candidate: {}:{}:{}",
                                candidate.file,
                                candidate.span.start_line,
                                candidate.span.start_column
                            );
                        }
                    } else {
                        let _ = writeln!(
                            out,
                            "{indent}  Source: unavailable (unverified code or unmapped PC)"
                        );
                    }
                }
            }
            if trace.omitted_code_prefixes > 0 {
                let _ = writeln!(
                    out,
                    "  Code identity capture truncated: {} omitted boundaries; some sources may be unavailable",
                    trace.omitted_code_prefixes
                );
            }
            if trace.omitted_frames > 0 {
                let _ = writeln!(
                    out,
                    "  Trace truncated: {} omitted frames",
                    trace.omitted_frames
                );
            }
        }
        out
    }

    pub fn render_steps(&self) -> String {
        use std::fmt::Write;
        let mut out = String::new();
        for (index, trace) in self.traces.iter().enumerate() {
            let _ = writeln!(
                out,
                "TRACE execution={index} (last {} of {} steps)",
                trace.steps.len(),
                trace.total_steps
            );
            for step in &trace.steps {
                let _ = writeln!(
                    out,
                    "frame={} pc={:04} op=0x{:02x} stack={} gas_rem={} top={}",
                    step.frame,
                    step.location.pc,
                    step.location.opcode,
                    step.stack_len,
                    step.gas_remaining,
                    step.stack.join(",")
                );
            }
        }
        out
    }
}

fn decode_standard_error(
    payload: &contract_harness::execution_trace::CapturedBytes,
) -> Option<String> {
    if payload.truncated()
        || payload.original_len > 16384
        || payload.hex.len() != payload.original_len.checked_mul(2)?
    {
        return None;
    }
    let bytes = hex::decode(&payload.hex).ok()?;
    match bytes.get(..4)? {
        [0x4e, 0x48, 0x7b, 0x71] if bytes.len() == 36 => Some(format!(
            "Panic({:#x})",
            contract_harness::U256::from_be_slice(&bytes[4..])
        )),
        [0x08, 0xc3, 0x79, 0xa0] if bytes.len() >= 68 => {
            fn word(bytes: &[u8]) -> Option<usize> {
                if bytes.len() != 32 || bytes[..24].iter().any(|byte| *byte != 0) {
                    return None;
                }
                usize::try_from(u64::from_be_bytes(bytes[24..].try_into().ok()?)).ok()
            }
            if word(&bytes[4..36])? != 32 {
                return None;
            }
            let len = word(&bytes[36..68])?;
            let end = 68usize.checked_add(len)?;
            let value = std::str::from_utf8(bytes.get(68..end)?).ok()?;
            let padded_end = 68usize.checked_add(len.checked_add(31)? / 32 * 32)?;
            if padded_end != bytes.len()
                || bytes.get(end..padded_end)?.iter().any(|byte| *byte != 0)
            {
                return None;
            }
            Some(format!("Error({value:?})"))
        }
        _ => None,
    }
}

impl FailureDiagnostics<'_> {
    pub fn write_report(&self, root: &camino::Utf8Path, name: &str) -> Result<(), String> {
        // Include a stable suffix so distinct names sanitizing to the same path stay isolated.
        let suffix = blake3::hash(name.as_bytes()).to_hex();
        let directory = root
            .join("artifacts")
            .join("tests")
            .join(format!(
                "{}-{}",
                crate::report::sanitize_filename(name)
                    .chars()
                    .take(80)
                    .collect::<String>(),
                suffix
            ))
            .join("failure");
        std::fs::create_dir_all(&directory).map_err(|error| error.to_string())?;
        fn write(path: camino::Utf8PathBuf, value: &impl serde::Serialize) -> Result<(), String> {
            let bytes = serde_json::to_vec_pretty(value).map_err(|error| error.to_string())?;
            std::fs::write(path, bytes).map_err(|error| error.to_string())
        }
        write(
            directory.join("execution-trace.json"),
            &serde_json::json!({
                "schema_version": "fe-test-execution-v1", "executions": self.traces,
            }),
        )?;
        if let Ok(registry) = &self.registry {
            write(
                directory.join("artifact-manifest.json"),
                &serde_json::json!({
                    "schema_version": "fe-test-artifacts-v1",
                    "build_id": registry.debug.trace_hash,
                    "compiler": registry.debug.compiler,
                    "artifacts": registry.info.codes,
                    "optimization": registry.info.optimization,
                    "compiler_flags": registry.info.compiler_flags,
                }),
            )?;
            write(directory.join("debug-bundle.json"), &registry.debug)?;
            write(directory.join("compiler-facts.json"), &registry.info.facts)?;
            let sources = root.join("sources").join("by-hash");
            std::fs::create_dir_all(&sources).map_err(|error| error.to_string())?;
            for (hash, text) in &registry.info.sources {
                // Hash our snapshot bytes; never use a URI or arbitrary manifest path.
                let actual_hash = debug_export::source_lookup::content_hash(text.as_bytes());
                if *hash != actual_hash {
                    return Err("source snapshot hash mismatch".into());
                }
                let file = sources.join(format!("{}.fe", blake3::hash(text.as_bytes()).to_hex()));
                // Parallel tests may share a snapshot. A completed atomic rename avoids
                // exposing a partially written source to report readers.
                let temp =
                    tempfile::NamedTempFile::new_in(&sources).map_err(|error| error.to_string())?;
                std::fs::write(temp.path(), text).map_err(|error| error.to_string())?;
                temp.persist(&file).map_err(|error| error.to_string())?;
            }
        }
        std::fs::write(directory.join("explanation.txt"), self.render())
            .map_err(|error| error.to_string())
    }
}

pub(super) fn attach_error_metadata(
    db: &driver::DriverDataBase,
    output: &mut codegen::TestModuleOutput,
) {
    use common::InputDb;
    use std::sync::Arc;
    let mut enriched = BTreeMap::new();
    for case in &mut output.tests {
        let Some(info) = &case.debug_info else {
            continue;
        };
        let id = Arc::as_ptr(info) as usize;
        let info = enriched.entry(id).or_insert_with(|| {
            let mut info = (**info).clone();
            for code in &mut info.codes {
                let Some(owner) = &code.owner else {
                    continue;
                };
                let Ok(uri) = url::Url::parse(&owner.source_uri) else {
                    continue;
                };
                let Some(file) = db.workspace().get(db, &uri) else {
                    continue;
                };
                // Metadata is taken from the same immutable compiler database.
                // Missing/unsupported metadata leaves raw payloads usable.
                if let Ok(errors) = crate::abi::diagnostic_error_entries(
                    db,
                    db.top_mod(file),
                    &owner.kind,
                    &owner.name,
                ) {
                    code.custom_errors = errors;
                }
            }
            Arc::new(info)
        });
        case.debug_info = Some(Arc::clone(info));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::origin::OriginExportKey;
    use contract_harness::{ExecutionOptions, RuntimeInstance};

    fn fixture(tail: usize) -> TestDebugInfo {
        TestDebugInfo {
            optimization: "O0".into(),
            compiler_flags: vec![],
            facts: vec![],
            sources: BTreeMap::new(),
            codes: vec![TestDebugCode {
                owner: None,
                custom_errors: vec![],
                name: "ImmutableContract".into(),
                // Copy STOP and return it followed by 32 zero immutable bytes.
                deploy: hex::decode("6001600a5f3960215ff300").unwrap(),
                runtime: vec![0],
                immutable_tail_bytes: tail,
                create_key: OriginExportKey::try_from_raw_parts("code.object", "test", "create")
                    .unwrap(),
                runtime_key: OriginExportKey::try_from_raw_parts("code.object", "test", "runtime")
                    .unwrap(),
            }],
        }
    }

    fn metadata() -> TraceMetadata {
        TraceMetadata::compiler_emitted("test", "evm", vec!["test".into()], "fixture.fe", vec![])
    }

    #[test]
    fn immutable_runtime_requires_a_verified_deployment_even_with_truncated_payloads() {
        let info = fixture(32);
        let mut registry = ArtifactRegistry::new(&info, metadata()).unwrap();
        let limits = registry.capture_limits(CaptureLimits {
            payload_bytes: 0,
            total_payload_bytes: 0,
            ..Default::default()
        });
        let (deployed, deployment) = RuntimeInstance::deploy_observed(
            &hex::encode(&info.codes[0].deploy),
            &[0xa5; 64],
            Some(limits.clone()),
        );
        let (mut instance, _) = deployed.unwrap();
        let deployment = deployment.unwrap();
        let (result, call, _) =
            instance.call_raw_observed(&[], ExecutionOptions::default(), limits, false);
        assert!(result.is_ok());
        let frame = &call.frames[0];
        assert!(registry.code(frame).is_none());
        assert!(deployment.frames[0].input.truncated());
        assert_eq!(
            registry.code(&deployment.frames[0]).unwrap().name,
            "ImmutableContract"
        );
        registry.observe_deployments(&deployment);
        assert_eq!(registry.code(frame).unwrap().name, "ImmutableContract");

        // Source matching follows executed code, independently of storage context.
        let mut delegate = frame.clone();
        delegate.kind = "DelegateCall".into();
        delegate.context_address = Some("different storage account".into());
        assert!(registry.code(&delegate).is_some());
        // Reusing an address with changed code must not retain the association.
        delegate.code_hash = Some(code_hash(&[0xfe]));
        assert!(registry.code(&delegate).is_none());

        // Archived data retains the same identity without any workspace lookup.
        let saved_info: TestDebugInfo =
            serde_json::from_slice(&serde_json::to_vec(&info).unwrap()).unwrap();
        let saved_deployment: ExecutionTrace =
            serde_json::from_slice(&serde_json::to_vec(&deployment).unwrap()).unwrap();
        let mut offline = ArtifactRegistry::new(&saved_info, metadata()).unwrap();
        offline.observe_deployments(&saved_deployment);
        assert!(offline.code(frame).is_some());

        let wrong_info = fixture(64);
        let mut wrong = ArtifactRegistry::new(&wrong_info, metadata()).unwrap();
        wrong.observe_deployments(&deployment);
        assert!(wrong.code(frame).is_none());
    }

    #[test]
    fn archived_reports_render_from_source_snapshots_after_workspace_edits() {
        use common::InputDb;
        let temporary = tempfile::tempdir().unwrap();
        let input = temporary.path().join("original.fe");
        let source = "use std::evm::Evm\n#[test]\nfn failure() uses (evm: mut Evm) { assert!(false, \"archived\") }\n";
        std::fs::write(&input, source).unwrap();
        let mut db = driver::DriverDataBase::default();
        let uri = url::Url::from_file_path(&input).unwrap();
        let file = db.workspace().touch(&mut db, uri, Some(source.into()));
        let compiled = codegen::emit_test_module_sonatina(
            &db,
            db.top_mod(file),
            codegen::OptLevel::O0,
            codegen::SonatinaTestOptions {
                emit_observability: true,
                explain_failure: true,
            },
            None,
        )
        .unwrap();
        let case = &compiled.tests[0];
        let info = case.debug_info.as_deref().unwrap();
        let mut diagnostics = FailureDiagnostics::new(info, "failure", None);
        let (deployed, deployment) = RuntimeInstance::deploy_observed(
            &hex::encode(&case.bytecode),
            &[],
            Some(diagnostics.limits.clone()),
        );
        diagnostics.record(deployment.unwrap());
        let (mut instance, _) = deployed.unwrap();
        let (result, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            diagnostics.limits.clone(),
            false,
        );
        assert!(result.is_err());
        diagnostics.record(trace);
        let expected = diagnostics.render();
        assert!(expected.contains("Source: original.fe:3:"), "{expected}");
        let root = camino::Utf8PathBuf::from_path_buf(temporary.path().join("report")).unwrap();
        diagnostics.write_report(&root, "failure").unwrap();
        std::fs::write(&input, "completely different workspace contents").unwrap();

        let case_dir = std::fs::read_dir(root.join("artifacts/tests"))
            .unwrap()
            .next()
            .unwrap()
            .unwrap()
            .path()
            .join("failure");
        let read_json = |name: &str| -> serde_json::Value {
            serde_json::from_slice(&std::fs::read(case_dir.join(name)).unwrap()).unwrap()
        };
        let manifest = read_json("artifact-manifest.json");
        let facts: Vec<trace_facts::TraceFact> =
            serde_json::from_value(read_json("compiler-facts.json")).unwrap();
        let mut sources = BTreeMap::new();
        for fact in &facts {
            if let trace_facts::TraceFact::SourceFile(file) = fact {
                let hash = file.content_hash.strip_prefix("blake3:").unwrap();
                let text = std::fs::read_to_string(
                    root.join("sources/by-hash").join(format!("{hash}.fe")),
                )
                .unwrap();
                sources.insert(file.content_hash.clone(), text);
            }
        }
        let archived = TestDebugInfo {
            optimization: manifest["optimization"].as_str().unwrap().into(),
            compiler_flags: serde_json::from_value(manifest["compiler_flags"].clone()).unwrap(),
            codes: serde_json::from_value(manifest["artifacts"].clone()).unwrap(),
            facts,
            sources,
        };
        let mut offline = FailureDiagnostics::new(&archived, "failure", None);
        assert_eq!(
            offline.registry.as_ref().unwrap().debug.trace_hash,
            manifest["build_id"].as_str().unwrap()
        );
        let executions: Vec<ExecutionTrace> =
            serde_json::from_value(read_json("execution-trace.json")["executions"].clone())
                .unwrap();
        for trace in executions {
            offline.record(trace);
        }
        assert_eq!(offline.render(), expected);

        // Distinct test names that sanitize identically still get distinct artifact paths.
        diagnostics.write_report(&root, "a/b").unwrap();
        diagnostics.write_report(&root, "a?b").unwrap();
        assert_eq!(
            std::fs::read_dir(root.join("artifacts/tests"))
                .unwrap()
                .count(),
            3
        );
    }

    #[test]
    fn standard_error_decoding_rejects_malformed_or_truncated_data() {
        use contract_harness::execution_trace::CapturedBytes;
        let encode = |bytes: &[u8]| CapturedBytes {
            hex: hex::encode(bytes),
            original_len: bytes.len(),
        };
        let mut error = vec![0u8; 100];
        error[..4].copy_from_slice(&[0x08, 0xc3, 0x79, 0xa0]);
        error[35] = 32;
        error[67] = 4;
        error[68..72].copy_from_slice(b"boom");
        assert_eq!(
            decode_standard_error(&encode(&error)).as_deref(),
            Some("Error(\"boom\")")
        );
        for len in 0..error.len() {
            assert!(decode_standard_error(&encode(&error[..len])).is_none());
        }
        let mut truncated = encode(&error);
        truncated.original_len += 1;
        assert!(decode_standard_error(&truncated).is_none());
        error[67] = 255;
        assert!(decode_standard_error(&encode(&error)).is_none());
        error[36..68].fill(255);
        assert!(decode_standard_error(&encode(&error)).is_none());
        let mut panic = vec![0; 36];
        panic[..4].copy_from_slice(&[0x4e, 0x48, 0x7b, 0x71]);
        panic[35] = 0x11;
        assert_eq!(
            decode_standard_error(&encode(&panic)).as_deref(),
            Some("Panic(0x11)")
        );
    }

    #[test]
    fn identical_bytecode_from_distinct_artifacts_is_not_arbitrarily_attributed() {
        let mut info = fixture(0);
        let mut other = info.codes[0].clone();
        other.name = "OtherSource".into();
        info.codes.push(other);
        let registry = ArtifactRegistry::new(&info, metadata()).unwrap();
        let mut instance = RuntimeInstance::new("00").unwrap();
        let (_, trace, _) = instance.call_raw_observed(
            &[],
            ExecutionOptions::default(),
            CaptureLimits::default(),
            false,
        );
        assert!(registry.code(&trace.frames[0]).is_none());
    }
}
