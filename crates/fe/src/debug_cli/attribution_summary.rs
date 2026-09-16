use std::collections::BTreeMap;

use debug_export::{DebugBundle, EthdebugArtifact, EthdebugEnvironment, InstructionClassification};

/// Human-readable diagnostics over the same model used for export, not a wire API.
pub(super) fn render(bundle: &DebugBundle, artifact: &EthdebugArtifact) -> Result<String, String> {
    let instructions = bundle
        .instructions
        .iter()
        .map(|instruction| (instruction.key.canonical_storage_key(), instruction))
        .collect::<BTreeMap<_, _>>();
    let mut output = String::new();
    for program in &artifact.programs {
        let environment = match program.environment {
            EthdebugEnvironment::Create => "creation",
            EthdebugEnvironment::Call => "runtime",
        };
        let mut counts = [0usize; 4];
        let mut reasons = BTreeMap::<&str, usize>::new();
        for emitted in &program.instructions {
            let instruction = instructions.get(&emitted.fe_origin_key).ok_or_else(|| {
                format!(
                    "exported instruction {} is absent from debug bundle",
                    emitted.fe_origin_key
                )
            })?;
            let slot = match instruction.classification {
                InstructionClassification::SourceMapped => 0,
                InstructionClassification::Ambiguous => 1,
                InstructionClassification::Synthetic => 2,
                InstructionClassification::Unmapped => 3,
            };
            counts[slot] += 1;
            if instruction.classification == InstructionClassification::Unmapped {
                *reasons
                    .entry(
                        instruction
                            .classification_reason
                            .as_deref()
                            .unwrap_or("unspecified"),
                    )
                    .or_default() += 1;
            }
        }
        output.push_str(&format!(
            "\nAttribution: {} / {environment}\n",
            program.contract.name
        ));
        if artifact.programs.iter().any(|other| {
            other.id != program.id
                && other.contract.name == program.contract.name
                && other.environment == program.environment
        }) {
            output.push_str(&format!("  Program: {}\n", program.id));
        }
        for (label, count) in [
            ("Unique exact source", counts[0]),
            ("Multiple candidate sources", counts[1]),
            ("Generated-code explanation", counts[2]),
            ("No source attribution", counts[3]),
            ("Total instructions", program.instructions.len()),
        ] {
            output.push_str(&format!("  {label:<30} {count:>6}\n"));
        }
        if !reasons.is_empty() {
            output.push_str("  Why source attribution is missing:\n");
            for (reason, count) in reasons {
                output.push_str(&format!("    {reason}: {count}\n"));
            }
        }
    }
    output.push_str("\nAttribution is partial; these counts do not measure complete source coverage or transformation history.\n");
    Ok(output)
}
