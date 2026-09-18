use sonatina_codegen::object::{
    OBSERVABILITY_SCHEMA_VERSION, PcAttribution, PcMapEntry, SectionObservability, UnmappedReason,
    UnmappedReasonCoverage,
};

use super::{LowerError, SonatinaContractBytecode};

impl SonatinaContractBytecode {
    /// Admit the final result, after embed composition and wrapper construction.
    /// Both trace and ordinary observability APIs return through this boundary.
    pub(super) fn checked(self, object: &str, required: bool) -> Result<Self, LowerError> {
        for (section, bytes, observability) in [
            ("creation", &self.deploy, &self.deploy_observability),
            ("runtime", &self.runtime, &self.runtime_observability),
        ] {
            let label = format!("{object}:{section}");
            match observability {
                Some(observability) => verify_received_observability(&label, bytes, observability)?,
                None if required => {
                    return Err(LowerError::Internal(format!(
                        "section `{label}` observability: requested metadata is missing"
                    )));
                }
                None => {}
            }
        }
        Ok(self)
    }
}

/// Re-check on the consumer side the conservation invariants Sonatina enforces
/// at link time, so trust in the received observability survives every future
/// backend pin bump instead of being assumed. Failures are hard errors.
pub(crate) fn verify_received_observability(
    section: &str,
    section_bytes: &[u8],
    obs: &SectionObservability,
) -> Result<(), crate::LowerError> {
    if obs.schema_version != OBSERVABILITY_SCHEMA_VERSION {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: unsupported schema `{}`, expected `{OBSERVABILITY_SCHEMA_VERSION}`",
            obs.schema_version
        )));
    }
    let emitted_section_bytes = u32::try_from(section_bytes.len()).map_err(|_| {
        crate::LowerError::Internal(format!(
            "section `{section}` observability: emitted section length exceeds u32"
        ))
    })?;
    if obs.section_bytes != emitted_section_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: metadata describes {} section bytes, emitted section has {emitted_section_bytes}",
            obs.section_bytes
        )));
    }
    let mapped_plus_unmapped = obs
        .mapped_code_bytes
        .checked_add(obs.unmapped_code_bytes)
        .ok_or_else(|| {
            crate::LowerError::Internal(format!(
                "section `{section}` observability: mapped + unmapped byte count overflow"
            ))
        })?;
    if mapped_plus_unmapped != obs.code_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: mapped {} + unmapped {} != code {}",
            obs.mapped_code_bytes, obs.unmapped_code_bytes, obs.code_bytes
        )));
    }
    let reason_total = checked_unmapped_reason_total(section, obs.unmapped_reason_coverage)?;
    if reason_total != obs.unmapped_code_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: unmapped-reason total {reason_total} != unmapped {}",
            obs.unmapped_code_bytes
        )));
    }
    // Layout identity: the section is code, then data, then embeds (link.rs), so
    // the three regions must exactly tile the section.
    let layout_total = obs
        .code_bytes
        .checked_add(obs.data_bytes)
        .and_then(|total| total.checked_add(obs.embed_bytes))
        .ok_or_else(|| {
            crate::LowerError::Internal(format!(
                "section `{section}` observability: section layout byte count overflow"
            ))
        })?;
    if layout_total != obs.section_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: code {} + data {} + embed {} != section {}",
            obs.code_bytes, obs.data_bytes, obs.embed_bytes, obs.section_bytes
        )));
    }
    // Bounds and overlap. The sort key is the full (pc_start, pc_end) tuple so a
    // zero-length entry sharing a real entry's start does not read as an overlap.
    let mut ordered: Vec<&PcMapEntry> = obs.pc_map.iter().collect();
    ordered.sort_by_key(|entry| (entry.pc_start, entry.pc_end));
    let mut prev_end = 0u32;
    let mut code_cursor = 0u32;
    let mut derived_mapped = 0u32;
    let mut derived_unmapped = 0u32;
    let mut derived_unmapped_reasons = UnmappedReasonCoverage::default();
    for entry in ordered {
        if entry.pc_end < entry.pc_start {
            return Err(crate::LowerError::Internal(format!(
                "section `{section}` observability: reversed range [{}, {})",
                entry.pc_start, entry.pc_end
            )));
        }
        // Code-region entries obey Sonatina's per-section contract (bounded by
        // code_bytes, link.rs). Entries wholly past the code region are Fe's own
        // embed merge (merge_embeds), which offsets embedded objects' pc maps
        // into [code_bytes + data_bytes, section_bytes); they may not straddle
        // the code boundary and may not leave the section.
        let in_code_region = entry.pc_start < obs.code_bytes;
        let bound = if in_code_region {
            obs.code_bytes
        } else {
            obs.section_bytes
        };
        if entry.pc_end > bound {
            return Err(crate::LowerError::Internal(format!(
                "section `{section}` observability: range [{}, {}) exceeds its {} bound {bound}",
                entry.pc_start,
                entry.pc_end,
                if in_code_region {
                    "code-region"
                } else {
                    "section"
                },
            )));
        }
        if !in_code_region {
            let embed_start = obs.code_bytes.checked_add(obs.data_bytes).ok_or_else(|| {
                crate::LowerError::Internal(format!(
                    "section `{section}` observability: embed start overflow"
                ))
            })?;
            if entry.pc_start < embed_start {
                return Err(crate::LowerError::Internal(format!(
                    "section `{section}` observability: embed-region range [{}, {}) starts before the embed region at {embed_start}",
                    entry.pc_start, entry.pc_end
                )));
            }
        }
        if entry.pc_start < prev_end {
            return Err(crate::LowerError::Internal(format!(
                "section `{section}` observability: overlapping range at pc {}",
                entry.pc_start
            )));
        }
        if in_code_region {
            if entry.pc_start > code_cursor {
                let gap = entry.pc_start - code_cursor;
                derived_unmapped = checked_observability_add(
                    section,
                    "derived unmapped byte count",
                    derived_unmapped,
                    gap,
                )?;
                checked_add_unmapped_reason(
                    section,
                    &mut derived_unmapped_reasons,
                    UnmappedReason::Unknown,
                    gap,
                )?;
            }
            let width = entry.pc_end - entry.pc_start;
            match &entry.attribution {
                PcAttribution::Mapped { .. } => {
                    derived_mapped = checked_observability_add(
                        section,
                        "derived mapped byte count",
                        derived_mapped,
                        width,
                    )?;
                }
                PcAttribution::Unmapped { reason, .. } => {
                    derived_unmapped = checked_observability_add(
                        section,
                        "derived unmapped byte count",
                        derived_unmapped,
                        width,
                    )?;
                    checked_add_unmapped_reason(
                        section,
                        &mut derived_unmapped_reasons,
                        *reason,
                        width,
                    )?;
                }
            }
            code_cursor = entry.pc_end;
        }
        prev_end = entry.pc_end;
    }
    if code_cursor < obs.code_bytes {
        let gap = obs.code_bytes - code_cursor;
        derived_unmapped = checked_observability_add(
            section,
            "derived unmapped byte count",
            derived_unmapped,
            gap,
        )?;
        checked_add_unmapped_reason(
            section,
            &mut derived_unmapped_reasons,
            UnmappedReason::Unknown,
            gap,
        )?;
    }
    if derived_mapped != obs.mapped_code_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: pc map derives {derived_mapped} mapped bytes, aggregate reports {}",
            obs.mapped_code_bytes
        )));
    }
    if derived_unmapped != obs.unmapped_code_bytes {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: pc map derives {derived_unmapped} unmapped bytes, aggregate reports {}",
            obs.unmapped_code_bytes
        )));
    }
    if derived_unmapped_reasons != obs.unmapped_reason_coverage {
        return Err(crate::LowerError::Internal(format!(
            "section `{section}` observability: pc map derives unmapped reasons {derived_unmapped_reasons:?}, aggregate reports {:?}",
            obs.unmapped_reason_coverage
        )));
    }
    Ok(())
}

fn checked_observability_add(
    section: &str,
    field: &str,
    current: u32,
    bytes: u32,
) -> Result<u32, crate::LowerError> {
    current.checked_add(bytes).ok_or_else(|| {
        crate::LowerError::Internal(format!(
            "section `{section}` observability: {field} overflow"
        ))
    })
}

fn checked_unmapped_reason_total(
    section: &str,
    coverage: UnmappedReasonCoverage,
) -> Result<u32, crate::LowerError> {
    // Exhaustive destructuring makes a new backend counter a compile-time
    // obligation here, rather than silently omitting it from conservation.
    let UnmappedReasonCoverage {
        missing_provenance,
        no_machine_inst,
        label_or_fixup_only,
        synthetic,
        unknown,
    } = coverage;
    [
        missing_provenance,
        no_machine_inst,
        label_or_fixup_only,
        synthetic,
        unknown,
    ]
    .into_iter()
    .try_fold(0u32, |total, bytes| {
        checked_observability_add(section, "unmapped-reason total", total, bytes)
    })
}

fn checked_add_unmapped_reason(
    section: &str,
    coverage: &mut UnmappedReasonCoverage,
    reason: UnmappedReason,
    bytes: u32,
) -> Result<(), crate::LowerError> {
    let counter = match reason {
        UnmappedReason::MissingProvenance => &mut coverage.missing_provenance,
        UnmappedReason::NoMachineInst => &mut coverage.no_machine_inst,
        UnmappedReason::LabelOrFixupOnly => &mut coverage.label_or_fixup_only,
        UnmappedReason::Synthetic => &mut coverage.synthetic,
        UnmappedReason::Unknown => &mut coverage.unknown,
    };
    *counter =
        checked_observability_add(section, "derived unmapped-reason count", *counter, bytes)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn wrapped_contract() -> SonatinaContractBytecode {
        let runtime = vec![0x60, 0x01, 0x00];
        let deploy = super::super::wrap_as_init_code(&runtime);
        let deploy_observability =
            super::super::wrapped_init_observability(deploy.len(), deploy.len() - runtime.len());
        let runtime_observability = Some(super::super::all_unmapped_observability(
            "runtime",
            runtime.len() as u32,
            runtime.len() as u32,
            UnmappedReason::Synthetic,
        ));
        SonatinaContractBytecode {
            deploy,
            runtime,
            deploy_observability,
            runtime_observability,
        }
    }

    #[test]
    fn final_admission_checks_both_sections_including_synthetic_wrapper() {
        let valid = wrapped_contract();
        assert!(valid.clone().checked("contract", true).is_ok());
        let corruptions: [fn(&mut SectionObservability); 8] = [
            |obs| obs.schema_version = "unsupported",
            |obs| obs.section_bytes += 1,
            |obs| obs.data_bytes += 1,
            |obs| obs.mapped_code_bytes = u32::MAX,
            |obs| obs.unmapped_reason_coverage.synthetic = 0,
            |obs| obs.pc_map.clear(),
            |obs| obs.pc_map[0].pc_end = obs.section_bytes + 1,
            |obs| obs.pc_map.push(obs.pc_map[0].clone()),
        ];
        for creation in [false, true] {
            for (case, corrupt) in corruptions.iter().enumerate() {
                for required in [false, true] {
                    let mut broken = valid.clone();
                    let obs = if creation {
                        &mut broken.deploy_observability
                    } else {
                        &mut broken.runtime_observability
                    };
                    corrupt(obs.as_mut().unwrap());
                    let error = broken
                        .checked("contract", required)
                        .unwrap_err()
                        .to_string();
                    assert!(
                        error.contains(if creation {
                            "contract:creation"
                        } else {
                            "contract:runtime"
                        }),
                        "case {case}: {error}"
                    );
                }
            }
        }
    }

    #[test]
    fn final_admission_rejects_missing_requested_observability() {
        for creation in [false, true] {
            let mut missing = wrapped_contract();
            if creation {
                missing.deploy_observability = None;
            } else {
                missing.runtime_observability = None;
            }
            assert!(missing.clone().checked("contract", false).is_ok());
            assert!(
                missing
                    .checked("contract", true)
                    .unwrap_err()
                    .to_string()
                    .contains("requested metadata is missing")
            );
        }
        let mut unobserved = wrapped_contract();
        unobserved.deploy_observability = None;
        unobserved.runtime_observability = None;
        assert!(unobserved.checked("contract", false).is_ok());
    }

    #[test]
    fn received_observability_bounds_split_code_and_embed_regions() {
        use sonatina_codegen::machinst::vcode::VCodeInst;
        use sonatina_codegen::object::{
            MachineInstId, OBSERVABILITY_SCHEMA_VERSION, PcAttribution, PcMapEntry, PcMapUnit,
            SectionObservability, UnmappedReason, UnmappedReasonCoverage,
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        // code_bytes=4, section_bytes=8: bytes [4, 8) are the embed region that
        // Fe's merge_embeds splices in past the code. The scalars satisfy the
        // conservation equation; only the per-entry bound is under test.
        let make = |pc_start: u32, pc_end: u32| SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 8,
            code_bytes: 4,
            data_bytes: 0,
            embed_bytes: 4,
            mapped_code_bytes: 0,
            unmapped_code_bytes: 4,
            unmapped_reason_coverage: {
                let mut c = UnmappedReasonCoverage::default();
                c.add_bytes(UnmappedReason::Unknown, 4);
                c
            },
            pc_map: vec![PcMapEntry {
                pc_start,
                pc_end,
                unit: PcMapUnit::Function(func),
                func_name: "x".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                attribution: PcAttribution::Unmapped {
                    machine_inst: Some(MachineInstId(InstId(0))),
                    reason: UnmappedReason::Unknown,
                },
            }],
        };

        // Embed-region entry wholly past code_bytes: accepted (bounded by section).
        super::verify_received_observability("runtime", &[0; 8], &make(4, 8))
            .expect("embed-region entry within the section is valid");
        // Straddles the code boundary: rejected.
        assert!(super::verify_received_observability("runtime", &[0; 8], &make(2, 6)).is_err());
        // Past section_bytes: rejected.
        assert!(super::verify_received_observability("runtime", &[0; 8], &make(4, 9)).is_err());

        let mut wrong_schema = make(4, 8);
        wrong_schema.schema_version = "tampered";
        assert!(super::verify_received_observability("runtime", &[0; 8], &wrong_schema).is_err());
        assert!(super::verify_received_observability("runtime", &[0; 7], &make(4, 8)).is_err());

        let mut mapped = make(0, 4);
        mapped.mapped_code_bytes = 4;
        mapped.unmapped_code_bytes = 0;
        mapped.unmapped_reason_coverage = UnmappedReasonCoverage::default();
        mapped.pc_map[0].attribution = PcAttribution::Mapped {
            machine_inst: MachineInstId(InstId(0)),
            post_opt_provenance: "mapped".to_string(),
        };
        super::verify_received_observability("runtime", &[0; 8], &mapped)
            .expect("mapped code entry matches its aggregate coverage");

        let mut missing_entry = mapped.clone();
        missing_entry.pc_map.clear();
        assert!(super::verify_received_observability("runtime", &[0; 8], &missing_entry).is_err());

        let mut changed_classification = mapped.clone();
        changed_classification.pc_map[0].attribution = PcAttribution::Unmapped {
            machine_inst: Some(MachineInstId(InstId(0))),
            reason: UnmappedReason::Unknown,
        };
        assert!(
            super::verify_received_observability("runtime", &[0; 8], &changed_classification,)
                .is_err()
        );

        let mut changed_reason = make(0, 4);
        changed_reason.pc_map[0].attribution = PcAttribution::Unmapped {
            machine_inst: Some(MachineInstId(InstId(0))),
            reason: UnmappedReason::MissingProvenance,
        };
        assert!(super::verify_received_observability("runtime", &[0; 8], &changed_reason).is_err());

        let mut unreported_gap = make(1, 4);
        unreported_gap.unmapped_reason_coverage.unknown = 3;
        assert!(super::verify_received_observability("runtime", &[0; 8], &unreported_gap).is_err());

        assert!(super::checked_observability_add("runtime", "layout", u32::MAX, 1).is_err());
    }
}
