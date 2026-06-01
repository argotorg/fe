use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::str::FromStr;

pub mod datalog_emit;
pub mod origin_closure;
pub mod static_analysis;
pub mod trace_index;

use common::origin::OriginExportKey;
use introspection_config::FeToolingConfig;
use serde::{Deserialize, Serialize};
use trace_facts::{
    CompilerEventKind, CompilerPhase, GasKind, InstructionCategory, InstructionFact, LoopBlockRole,
    OriginEdgeLabel, OriginEdgeTraversalClass, RuntimePcJoinConfidence, RuntimeValue,
    StorageAccessKind, StorageFact, StorageLocation, TraceDataSource, TraceFact, TraceMetadata,
    TraceSnapshot,
};

pub type QueryResult<T> = Result<T, QueryError>;

pub trait IntrospectionService {
    fn status(&self) -> IntrospectionStatus;
    fn effective_config(&self) -> FeToolingConfig;
    fn loop_cost(&self, request: LoopCostRequest) -> QueryResult<LoopCostReport>;
    fn loop_contents(&self, request: LoopContentsRequest) -> QueryResult<LoopContentsReport>;
    fn explain_local(&self, request: ExplainLocalRequest) -> QueryResult<ExplainLocalReport>;
    fn gas_breakdown(&self, request: GasBreakdownRequest) -> QueryResult<GasBreakdownReport>;
    fn explain_pc(&self, request: ExplainPcRequest) -> QueryResult<ExplainPcReport>;
    fn gas_by_source(&self, request: GasBySourceRequest) -> QueryResult<GasBySourceReport>;
    fn bytecode_size_by_source(
        &self,
        request: BytecodeSizeBySourceRequest,
    ) -> QueryResult<BytecodeSizeBySourceReport>;
    fn dynamic_gas_by_source(
        &self,
        request: DynamicGasBySourceRequest,
    ) -> QueryResult<DynamicGasBySourceReport>;
    fn gas_to_source(&self, request: GasToSourceRequest) -> QueryResult<GasToSourceReport>;
    fn optimized_code_honesty(
        &self,
        request: OptimizedCodeHonestyRequest,
    ) -> QueryResult<OptimizedCodeHonestyReport>;
    fn attribution_audit(&self) -> QueryResult<AttributionAuditReport>;
    fn variables_at_pc(&self, request: VariablesAtPcRequest) -> QueryResult<VariablesAtPcReport>;
    fn runtime_gas_by_source(
        &self,
        request: RuntimeGasBySourceRequest,
    ) -> QueryResult<RuntimeGasBySourceReport>;
    fn storage_writes_by_source(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<StorageWritesBySourceReport>;
    fn storage_accesses_by_slot(
        &self,
        request: StorageAccessesBySlotRequest,
    ) -> QueryResult<StorageAccessesBySlotReport>;
    fn call_cost_by_callsite(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<CallCostByCallsiteReport>;
    fn memory_growth_by_source(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<MemoryGrowthBySourceReport>;
    fn revert_attribution(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<RevertAttributionReport>;
    fn hot_path_by_iteration(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<HotPathByIterationReport>;
    fn value_flow_at_pc(&self, request: ValueFlowAtPcRequest) -> QueryResult<ValueFlowAtPcReport>;
}

#[derive(Clone, Debug)]
pub struct TraceIntrospectionService {
    snapshot: TraceSnapshot,
    config: FeToolingConfig,
}

impl TraceIntrospectionService {
    pub fn new(snapshot: TraceSnapshot) -> Self {
        Self::with_config(snapshot, FeToolingConfig::default())
    }

    pub fn with_config(snapshot: TraceSnapshot, config: FeToolingConfig) -> Self {
        Self { snapshot, config }
    }

    pub fn snapshot(&self) -> &TraceSnapshot {
        &self.snapshot
    }
}

impl IntrospectionService for TraceIntrospectionService {
    fn status(&self) -> IntrospectionStatus {
        IntrospectionStatus {
            trace_hash: self.snapshot.trace_hash().to_string(),
            fact_count: self.snapshot.validation().summary.fact_count,
            instruction_count: self.snapshot.validation().summary.instruction_count,
            data_source: data_source_label(self.snapshot.metadata()),
            target: self.snapshot.metadata().target.clone(),
            config_hash: self.config.stable_hash(),
        }
    }

    fn effective_config(&self) -> FeToolingConfig {
        self.config.clone()
    }

    fn loop_cost(&self, request: LoopCostRequest) -> QueryResult<LoopCostReport> {
        let index = TraceIndex::new(&self.snapshot);
        let loop_key = request.loop_key.or_else(|| index.loop_key.clone());
        let instructions = if let Some(loop_key) = &loop_key {
            index
                .loop_members
                .get(loop_key)
                .cloned()
                .unwrap_or_default()
        } else {
            BTreeSet::new()
        };
        let available = !instructions.is_empty();
        let instruction_keys = if available {
            instructions
        } else {
            index.all_instruction_keys()
        };
        let summary = index.category_counts(&instruction_keys);
        let repeated_zero_extends = index.zero_extends_by_local(&instruction_keys);
        let storage_impacts = index.storage_impacts(&instruction_keys);
        let findings = loop_cost_findings(
            available,
            &summary,
            &repeated_zero_extends,
            &storage_impacts,
        );

        Ok(LoopCostReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            available,
            unavailable_reason: (!available).then(|| {
                "compiler-derived LoopMembershipFact rows are not emitted yet, so the report cannot truthfully isolate the hot loop".to_string()
            }),
            loop_key,
            loop_label: index.loop_key.as_ref().map(|key| index.label(key)),
            summary,
            repeated_zero_extends,
            storage_impacts,
            findings,
            confidence: if available {
                Confidence::High
            } else {
                Confidence::Unknown
            },
        })
    }

    fn loop_contents(&self, request: LoopContentsRequest) -> QueryResult<LoopContentsReport> {
        let index = TraceIndex::new(&self.snapshot);
        let loop_key = request.loop_key.or_else(|| index.loop_key.clone());
        let instructions = loop_key
            .as_ref()
            .and_then(|key| index.loop_members.get(key))
            .cloned()
            .unwrap_or_default();
        let available = !instructions.is_empty();
        let instruction_rows = index.sorted_instruction_rows(&instructions);
        let target_instruction_keys = loop_key
            .as_ref()
            .filter(|key| index.loop_phase(key) == Some(CompilerPhase::SonatinaPostOpt))
            .map(|_| index.bytecode_instructions_for_loop_members(&instructions))
            .unwrap_or_default();
        let target_instructions = index.sorted_instruction_rows(&target_instruction_keys);
        let bytecode_bridge_available = !target_instructions.is_empty();
        let bytecode_origin_edges_available = index.has_bytecode_origin_edges();
        let blocks = loop_key
            .as_ref()
            .map(|key| index.loop_block_contents(key, &instructions))
            .unwrap_or_default();

        Ok(LoopContentsReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            available,
            unavailable_reason: (!available).then(|| {
                "compiler-derived LoopMembershipFact rows are missing for this trace".to_string()
            }),
            loop_key: loop_key.clone(),
            loop_label: loop_key.as_ref().map(|key| index.label(key)),
            blocks,
            instructions: instruction_rows,
            target_instructions,
            bytecode_bridge_available,
            bytecode_origin_edges_available,
            findings: if available {
                vec![Insight::info(
                    "CFG-derived loop membership",
                    "loop contents come from compiler-emitted LoopFact, LoopBlockFact, and LoopMembershipFact rows",
                )]
            } else {
                vec![Insight::info(
                    "Loop contents unavailable",
                    "no phase-owned loop membership facts were emitted for this trace",
                )]
            },
            confidence: if available {
                Confidence::High
            } else {
                Confidence::Unknown
            },
        })
    }

    fn explain_local(&self, request: ExplainLocalRequest) -> QueryResult<ExplainLocalReport> {
        let index = TraceIndex::new(&self.snapshot);
        let local_key = if let Some(local_key) = request.local_key {
            if index.local_keys.contains(&local_key) {
                Some(local_key)
            } else {
                return Ok(explain_local_unavailable(
                    &self.snapshot,
                    request.local,
                    Some("requested local_key is not present in this trace".to_string()),
                    index.local_choices(),
                    vec![local_key],
                ));
            }
        } else {
            let candidates = index.local_candidates(&request.local);
            match candidates.as_slice() {
                [local_key] => Some(local_key.clone()),
                [] => None,
                _ => {
                    return Ok(explain_local_unavailable(
                        &self.snapshot,
                        request.local,
                        Some(
                            "local display name is ambiguous; pass an exact local_key".to_string(),
                        ),
                        index.local_choices(),
                        candidates,
                    ));
                }
            }
        };
        let loop_instructions = index.active_loop_instructions();
        let available_locals = index.local_choices();

        let Some(local_key) = local_key else {
            return Ok(explain_local_unavailable(
                &self.snapshot,
                request.local,
                Some(
                    "source-local display facts or matching local identity are missing".to_string(),
                ),
                available_locals,
                Vec::new(),
            ));
        };

        let storage_history = index.storage_for(&local_key);
        let related_instructions = index.related_instruction_edges(&local_key, &loop_instructions);
        let zero_extends = related_instructions
            .iter()
            .filter(|related| related.edge_label == OriginEdgeLabel::IntegerLegalizationFor)
            .cloned()
            .collect::<Vec<_>>();
        let mut findings = Vec::new();
        if storage_history
            .iter()
            .any(|step| step.location.contains("stack slot"))
        {
            findings.push(Insight::hint(
                "Stack-resident local",
                "storage facts show this local was assigned a backend stack slot",
            ));
        } else if storage_history
            .iter()
            .any(|step| step.location == "memory place")
        {
            findings.push(Insight::hint(
                "MIR memory-backed local",
                "storage facts show this local was materialized as a MIR memory place",
            ));
        }
        if !zero_extends.is_empty() {
            findings.push(Insight::hint(
                "Repeated integer normalization",
                "compiler events or origin edges link zero-extend instructions to this local",
            ));
        }

        Ok(ExplainLocalReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            local: request.local,
            local_key: Some(local_key.clone()),
            storage_history,
            related_instructions,
            zero_extends,
            findings,
            available: true,
            unavailable_reason: None,
            available_locals,
            candidate_local_keys: vec![local_key],
            confidence: Confidence::High,
        })
    }

    fn gas_breakdown(&self, request: GasBreakdownRequest) -> QueryResult<GasBreakdownReport> {
        let index = TraceIndex::new(&self.snapshot);
        let rows = index.static_gas_rows(&request.schedule);
        let total_gas = rows.iter().map(|row| row.gas).sum::<u64>();
        let available = !rows.is_empty();
        Ok(GasBreakdownReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            schedule: request.schedule,
            policy: "opcode-table-static".to_string(),
            available,
            total_gas: available.then_some(total_gas),
            rows,
            findings: if available {
                vec![Insight::info(
                    "Static gas estimate",
                    "gas rows are static opcode-table estimates under the named EVM schedule",
                )]
            } else {
                vec![Insight::info(
                    "Static gas unavailable",
                    "opcode gas facts are not present in this trace snapshot",
                )]
            },
            confidence: if available {
                Confidence::Medium
            } else {
                Confidence::Unknown
            },
        })
    }

    fn explain_pc(&self, request: ExplainPcRequest) -> QueryResult<ExplainPcReport> {
        let index = TraceIndex::new(&self.snapshot);
        let instruction = index.instruction_at_pc(request.pc);
        let source_candidates = instruction
            .as_ref()
            .map(|instruction| index.source_candidates_for_instruction(&instruction.key))
            .unwrap_or_default();
        let static_gas = instruction
            .as_ref()
            .and_then(|instruction| index.static_gas_for_instruction(&instruction.key, "cancun"));
        let category = instruction
            .as_ref()
            .and_then(|instruction| index.category_for_instruction(&instruction.key));
        let available = instruction.is_some();

        Ok(ExplainPcReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            pc: request.pc,
            instruction,
            primary_source: primary_source(&source_candidates),
            source_candidates,
            category,
            static_gas,
            available,
            unavailable_reason: (!available).then(|| {
                "no bytecode.pc InstructionFact with this PC exists in the trace".to_string()
            }),
            confidence: if available {
                Confidence::Medium
            } else {
                Confidence::Unknown
            },
        })
    }

    fn gas_by_source(&self, request: GasBySourceRequest) -> QueryResult<GasBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let mut rows: BTreeMap<String, GasBySourceRow> = BTreeMap::new();
        let mut total_gas = 0;

        for gas in index.static_gas_rows(&request.schedule) {
            total_gas += gas.gas;
            for bucket in index.gas_attribution_buckets(&gas.subject, request.policy) {
                let row = rows
                    .entry(bucket.key.clone())
                    .or_insert_with(|| GasBySourceRow {
                        label: bucket.label.clone(),
                        source: bucket.source.clone(),
                        gas: 0,
                        instruction_count: 0,
                        confidence: bucket.confidence,
                    });
                row.gas += gas.gas;
                row.instruction_count += 1;
                if bucket.confidence == Confidence::Low {
                    row.confidence = Confidence::Low;
                }
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| b.gas.cmp(&a.gas).then_with(|| a.label.cmp(&b.label)));
        let confidence = if total_gas == 0 {
            Confidence::Unknown
        } else if rows
            .iter()
            .any(|row| matches!(row.confidence, Confidence::Low | Confidence::Unknown))
        {
            Confidence::Low
        } else {
            Confidence::Medium
        };
        Ok(GasBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            schedule: request.schedule,
            policy: request.policy.to_string(),
            total_gas,
            rows,
            confidence,
        })
    }

    fn bytecode_size_by_source(
        &self,
        request: BytecodeSizeBySourceRequest,
    ) -> QueryResult<BytecodeSizeBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let mut rows: BTreeMap<String, BytecodeSizeBySourceRow> = BTreeMap::new();
        let mut total_bytes = 0;

        for extent in index.instruction_extents.values() {
            total_bytes += u64::from(extent.byte_len);
            for bucket in index.gas_attribution_buckets(&extent.instruction, request.policy) {
                let row =
                    rows.entry(bucket.key.clone())
                        .or_insert_with(|| BytecodeSizeBySourceRow {
                            label: bucket.label.clone(),
                            source: bucket.source.clone(),
                            bytes: 0,
                            instruction_count: 0,
                            confidence: bucket.confidence,
                        });
                row.bytes += u64::from(extent.byte_len);
                row.instruction_count += 1;
                if bucket.confidence == Confidence::Low {
                    row.confidence = Confidence::Low;
                }
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| b.bytes.cmp(&a.bytes).then_with(|| a.label.cmp(&b.label)));
        let confidence = if total_bytes == 0 {
            Confidence::Unknown
        } else if rows
            .iter()
            .any(|row| matches!(row.confidence, Confidence::Low | Confidence::Unknown))
        {
            Confidence::Low
        } else {
            Confidence::Medium
        };
        Ok(BytecodeSizeBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            policy: request.policy.to_string(),
            total_bytes,
            rows,
            confidence,
        })
    }

    fn dynamic_gas_by_source(
        &self,
        request: DynamicGasBySourceRequest,
    ) -> QueryResult<DynamicGasBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let mut rows: BTreeMap<String, GasBySourceRow> = BTreeMap::new();
        let mut total_gas = 0;
        let mut unattributed_steps = 0;

        for step in index.dynamic_gas_steps(request.trace_id.as_deref()) {
            total_gas += step.gas_cost;
            let instruction = index.instruction_for_dynamic_step(step);
            let buckets = instruction
                .as_ref()
                .map(|instruction| index.gas_attribution_buckets(&instruction.key, request.policy))
                .unwrap_or_else(|| vec![GasAttributionBucket::unmapped()]);
            if buckets.iter().all(|bucket| bucket.source.is_none()) {
                unattributed_steps += 1;
            }
            for bucket in buckets {
                let row = rows
                    .entry(bucket.key.clone())
                    .or_insert_with(|| GasBySourceRow {
                        label: bucket.label.clone(),
                        source: bucket.source.clone(),
                        gas: 0,
                        instruction_count: 0,
                        confidence: bucket.confidence,
                    });
                row.gas += step.gas_cost;
                row.instruction_count += 1;
                if bucket.confidence == Confidence::Low {
                    row.confidence = Confidence::Low;
                }
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| b.gas.cmp(&a.gas).then_with(|| a.label.cmp(&b.label)));
        Ok(DynamicGasBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            target_schedule: "runtime-measured".to_string(),
            policy: request.policy.to_string(),
            total_gas,
            unattributed_steps,
            rows,
            confidence: if total_gas > 0 {
                Confidence::Medium
            } else {
                Confidence::Unknown
            },
        })
    }

    fn gas_to_source(&self, request: GasToSourceRequest) -> QueryResult<GasToSourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let mut rows: BTreeMap<String, GasToSourceRow> = BTreeMap::new();
        let mut static_gas_total = 0;
        let mut dynamic_gas_total = 0;

        for gas in index.static_gas_rows(&request.schedule) {
            static_gas_total += gas.gas;
            for bucket in index.gas_attribution_buckets(&gas.subject, request.policy) {
                let row = gas_to_source_row(&mut rows, &bucket);
                row.static_gas += gas.gas;
                row.total_gas += gas.gas;
                row.instruction_count += 1;
            }
        }

        for step in index.dynamic_gas_steps(request.trace_id.as_deref()) {
            dynamic_gas_total += step.gas_cost;
            let buckets = index
                .instruction_for_dynamic_step(step)
                .map(|instruction| index.gas_attribution_buckets(&instruction.key, request.policy))
                .unwrap_or_else(|| vec![GasAttributionBucket::unmapped()]);
            for bucket in buckets {
                let row = gas_to_source_row(&mut rows, &bucket);
                row.dynamic_gas += step.gas_cost;
                row.total_gas += step.gas_cost;
                row.instruction_count += 1;
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| {
            b.total_gas
                .cmp(&a.total_gas)
                .then_with(|| a.label.cmp(&b.label))
        });
        Ok(GasToSourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            schedule: request.schedule,
            trace_id: request.trace_id,
            policy: request.policy.to_string(),
            static_gas: static_gas_total,
            dynamic_gas: dynamic_gas_total,
            total_gas: static_gas_total + dynamic_gas_total,
            rows,
            confidence: if static_gas_total + dynamic_gas_total > 0 {
                Confidence::Medium
            } else {
                Confidence::Unknown
            },
        })
    }

    fn optimized_code_honesty(
        &self,
        request: OptimizedCodeHonestyRequest,
    ) -> QueryResult<OptimizedCodeHonestyReport> {
        let index = TraceIndex::new(&self.snapshot);
        let schedule = request.schedule.unwrap_or_else(default_gas_schedule);
        let mut ambiguous_instructions = Vec::new();
        let mut synthetic_overheads = Vec::new();
        let mut unmapped_instructions = Vec::new();

        for instruction_key in index.all_instruction_keys() {
            let Some(instruction) = index.instruction_row(&instruction_key) else {
                continue;
            };
            let source_candidates = index.source_candidates_for_instruction(&instruction_key);
            let static_gas = index.static_gas_for_instruction(&instruction_key, &schedule);
            let dynamic_gas = index.dynamic_gas_for_instruction(&instruction_key);

            if source_candidates.len() > 1 {
                ambiguous_instructions.push(AmbiguousInstructionOriginRow {
                    instruction: instruction.clone(),
                    source_candidates,
                    static_gas,
                    dynamic_gas,
                    confidence: Confidence::Low,
                });
                continue;
            }

            let synthetic_edges = index.synthetic_edges_from(&instruction_key);
            if !synthetic_edges.is_empty() {
                let cause_sources = synthetic_edges
                    .iter()
                    .flat_map(|edge| index.source_candidates_for_instruction(&edge.to))
                    .collect::<Vec<_>>();
                if !cause_sources.is_empty() {
                    synthetic_overheads.push(SyntheticOverheadRow {
                        instruction: instruction.clone(),
                        cause_sources,
                        edge_labels: synthetic_edges.iter().map(|edge| edge.label).collect(),
                        static_gas,
                        dynamic_gas,
                        confidence: Confidence::Medium,
                    });
                    continue;
                }
            }

            if source_candidates.is_empty() {
                unmapped_instructions.push(instruction);
            }
        }

        Ok(OptimizedCodeHonestyReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            schedule,
            policy: "precision-honest-v1".to_string(),
            ambiguous_instructions,
            synthetic_overheads,
            unmapped_instructions,
            confidence: Confidence::Medium,
        })
    }

    fn attribution_audit(&self) -> QueryResult<AttributionAuditReport> {
        Ok(attribution_audit_report(&self.snapshot))
    }

    fn variables_at_pc(&self, request: VariablesAtPcRequest) -> QueryResult<VariablesAtPcReport> {
        let index = TraceIndex::new(&self.snapshot);
        let variables = index.variables_at_pc(request.pc, request.code_object.as_ref());
        Ok(VariablesAtPcReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            pc: request.pc,
            code_object: request.code_object,
            variables,
            confidence: Confidence::Medium,
        })
    }

    fn runtime_gas_by_source(
        &self,
        request: RuntimeGasBySourceRequest,
    ) -> QueryResult<RuntimeGasBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows: BTreeMap<String, GasBySourceRow> = BTreeMap::new();
        let mut total_gas = 0;
        let mut unattributed_steps = 0;

        for step in index.execution_steps(request.trace_id.as_deref()) {
            total_gas += step.gas_cost;
            let buckets = step
                .instruction
                .as_ref()
                .map(|instruction| index.gas_attribution_buckets(instruction, request.policy))
                .unwrap_or_else(|| vec![GasAttributionBucket::unmapped()]);
            if buckets.iter().all(|bucket| bucket.source.is_none()) {
                unattributed_steps += 1;
            }
            for bucket in buckets {
                let row = rows
                    .entry(bucket.key.clone())
                    .or_insert_with(|| GasBySourceRow {
                        label: bucket.label.clone(),
                        source: bucket.source.clone(),
                        gas: 0,
                        instruction_count: 0,
                        confidence: bucket.confidence,
                    });
                row.gas += step.gas_cost;
                row.instruction_count += 1;
                row.confidence = merge_confidence(row.confidence, bucket.confidence);
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| b.gas.cmp(&a.gas).then_with(|| a.label.cmp(&b.label)));
        Ok(RuntimeGasBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: request.policy.to_string(),
            total_gas,
            unattributed_steps,
            rows,
            confidence: runtime_report_confidence(total_gas > 0, &evidence),
        })
    }

    fn storage_writes_by_source(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<StorageWritesBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows: BTreeMap<String, StorageWriteBySourceRow> = BTreeMap::new();
        let mut total_writes = 0;
        let mut total_gas = 0;

        for access in index.storage_accesses(request.trace_id.as_deref()) {
            if access.kind != StorageAccessKind::Write {
                continue;
            }
            total_writes += 1;
            let gas = index.gas_for_step(&access.step);
            total_gas += gas;
            let buckets = index
                .instruction_for_runtime_access(access.instruction.as_ref(), &access.step)
                .map(|instruction| index.gas_attribution_buckets(&instruction, request.policy))
                .unwrap_or_else(|| vec![GasAttributionBucket::unmapped()]);
            let slot = runtime_value_label(&access.slot);
            for bucket in buckets {
                let row =
                    rows.entry(bucket.key.clone())
                        .or_insert_with(|| StorageWriteBySourceRow {
                            source: bucket.source.clone(),
                            label: bucket.label.clone(),
                            writes: 0,
                            gas: 0,
                            slots: Vec::new(),
                            confidence: bucket.confidence,
                        });
                row.writes += 1;
                row.gas += gas;
                if !row.slots.contains(&slot) {
                    row.slots.push(slot.clone());
                }
                row.confidence = merge_confidence(row.confidence, bucket.confidence);
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        for row in &mut rows {
            row.slots.sort();
        }
        rows.sort_by(|a, b| b.writes.cmp(&a.writes).then_with(|| a.label.cmp(&b.label)));
        Ok(StorageWritesBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: request.policy.to_string(),
            total_writes,
            total_gas,
            rows,
            confidence: runtime_report_confidence(total_writes > 0, &evidence),
        })
    }

    fn storage_accesses_by_slot(
        &self,
        request: StorageAccessesBySlotRequest,
    ) -> QueryResult<StorageAccessesBySlotReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows: BTreeMap<String, StorageAccessBySlotRow> = BTreeMap::new();

        for access in index.storage_accesses(request.trace_id.as_deref()) {
            let slot = runtime_value_label(&access.slot);
            if request.slot.as_ref().is_some_and(|filter| filter != &slot) {
                continue;
            }
            let gas = index.gas_for_step(&access.step);
            let sources =
                index.sources_for_runtime_access(access.instruction.as_ref(), &access.step);
            let row = rows
                .entry(slot.clone())
                .or_insert_with(|| StorageAccessBySlotRow {
                    slot,
                    reads: 0,
                    writes: 0,
                    gas: 0,
                    sources: Vec::new(),
                    confidence: Confidence::Unknown,
                });
            match access.kind {
                StorageAccessKind::Read => row.reads += 1,
                StorageAccessKind::Write => row.writes += 1,
            }
            row.gas += gas;
            for source in sources {
                if !row
                    .sources
                    .iter()
                    .any(|candidate| candidate.origin == source.origin)
                {
                    row.sources.push(source);
                }
            }
            row.confidence = if row.sources.is_empty() {
                Confidence::Unknown
            } else {
                Confidence::Medium
            };
        }

        let total_reads = rows.values().map(|row| row.reads).sum();
        let total_writes = rows.values().map(|row| row.writes).sum();
        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| {
            (b.reads + b.writes)
                .cmp(&(a.reads + a.writes))
                .then_with(|| a.slot.cmp(&b.slot))
        });
        Ok(StorageAccessesBySlotReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: request.policy.to_string(),
            slot_filter: request.slot,
            total_reads,
            total_writes,
            rows,
            confidence: runtime_report_confidence(total_reads + total_writes > 0, &evidence),
        })
    }

    fn call_cost_by_callsite(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<CallCostByCallsiteReport> {
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows = Vec::new();
        let mut total_gas_used = 0;

        for call in index.calls(request.trace_id.as_deref()) {
            if let Some(gas) = call.gas_used {
                total_gas_used += gas;
            }
            let instruction = call.callsite_instruction.clone().or_else(|| {
                index
                    .execution_step(&call.step)
                    .and_then(|step| step.instruction.clone())
            });
            let source = instruction
                .as_ref()
                .and_then(|instruction| index.primary_source_for_instruction(instruction));
            rows.push(CallCostByCallsiteRow {
                call: call.call.clone(),
                callsite_instruction: instruction,
                source,
                kind: format!("{:?}", call.kind),
                callee: call.callee.clone(),
                gas_requested: call.gas_requested,
                gas_used: call.gas_used,
                success: call.success,
                confidence: if call.callsite_instruction.is_some() {
                    Confidence::Medium
                } else {
                    Confidence::Low
                },
            });
        }

        rows.sort_by(|a, b| {
            b.gas_used
                .unwrap_or_default()
                .cmp(&a.gas_used.unwrap_or_default())
                .then_with(|| a.call.display_label().cmp(&b.call.display_label()))
        });
        Ok(CallCostByCallsiteReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: "call-inclusive-frame".to_string(),
            total_calls: rows.len(),
            total_gas_used,
            rows,
            confidence: runtime_report_confidence(total_gas_used > 0, &evidence),
        })
    }

    fn memory_growth_by_source(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<MemoryGrowthBySourceReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows: BTreeMap<String, MemoryGrowthBySourceRow> = BTreeMap::new();
        let mut total_accesses = 0;
        let mut total_bytes_touched = 0;
        let mut max_end_offset = 0;

        for access in index.memory_accesses(request.trace_id.as_deref()) {
            total_accesses += 1;
            total_bytes_touched += access.length;
            let end_offset = access.offset.saturating_add(access.length);
            max_end_offset = max_end_offset.max(end_offset);
            let gas = index.gas_for_step(&access.step);
            let buckets = index
                .execution_step(&access.step)
                .and_then(|step| step.instruction.as_ref())
                .map(|instruction| index.gas_attribution_buckets(instruction, request.policy))
                .unwrap_or_else(|| vec![GasAttributionBucket::unmapped()]);
            for bucket in buckets {
                let row =
                    rows.entry(bucket.key.clone())
                        .or_insert_with(|| MemoryGrowthBySourceRow {
                            source: bucket.source.clone(),
                            label: bucket.label.clone(),
                            accesses: 0,
                            bytes_touched: 0,
                            max_end_offset: 0,
                            gas: 0,
                            confidence: bucket.confidence,
                        });
                row.accesses += 1;
                row.bytes_touched += access.length;
                row.max_end_offset = row.max_end_offset.max(end_offset);
                row.gas += gas;
                row.confidence = merge_confidence(row.confidence, bucket.confidence);
            }
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| {
            b.max_end_offset
                .cmp(&a.max_end_offset)
                .then_with(|| b.bytes_touched.cmp(&a.bytes_touched))
                .then_with(|| a.label.cmp(&b.label))
        });
        Ok(MemoryGrowthBySourceReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: request.policy.to_string(),
            total_accesses,
            total_bytes_touched,
            max_end_offset,
            rows,
            confidence: runtime_report_confidence(total_accesses > 0, &evidence),
        })
    }

    fn revert_attribution(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<RevertAttributionReport> {
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows = Vec::new();

        for revert in index.reverts(request.trace_id.as_deref()) {
            let step = index.execution_step(&revert.step);
            let instruction = step.and_then(|step| step.instruction.clone());
            let source = instruction
                .as_ref()
                .and_then(|instruction| index.primary_source_for_instruction(instruction));
            rows.push(RevertAttributionRow {
                revert: revert.revert.clone(),
                step: revert.step.clone(),
                instruction,
                source,
                reason: revert.reason.clone(),
                data: runtime_value_label(&revert.data),
                confidence: step
                    .map(|step| confidence_for_join(step.join_confidence))
                    .unwrap_or(Confidence::Unknown),
            });
        }

        rows.sort_by_key(|row| row.revert.display_label());
        let available = !rows.is_empty();
        Ok(RevertAttributionReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: "runtime-step-to-source".to_string(),
            total_reverts: rows.len(),
            rows,
            confidence: runtime_report_confidence(available, &evidence),
        })
    }

    fn hot_path_by_iteration(
        &self,
        request: RuntimeTraceFilterRequest,
    ) -> QueryResult<HotPathByIterationReport> {
        reject_call_policy(request.policy)?;
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let loop_members = index.active_loop_instructions();
        let mut rows: BTreeMap<(Option<OriginExportKey>, u32, String), HotPathRow> =
            BTreeMap::new();
        let mut total_steps = 0;
        let mut total_gas = 0;

        for step in index.execution_steps(request.trace_id.as_deref()) {
            if !loop_members.is_empty()
                && step
                    .instruction
                    .as_ref()
                    .is_none_or(|instruction| !loop_members.contains(instruction))
            {
                continue;
            }
            total_steps += 1;
            total_gas += step.gas_cost;
            let key = (
                step.instruction.clone(),
                step.pc,
                step.opcode.to_ascii_uppercase(),
            );
            let source = step
                .instruction
                .as_ref()
                .and_then(|instruction| index.primary_source_for_instruction(instruction));
            let row = rows.entry(key).or_insert_with(|| HotPathRow {
                pc: step.pc,
                opcode: step.opcode.clone(),
                instruction: step.instruction.clone(),
                executions: 0,
                gas: 0,
                source,
                confidence: confidence_for_join(step.join_confidence),
            });
            row.executions += 1;
            row.gas += step.gas_cost;
            row.confidence =
                merge_confidence(row.confidence, confidence_for_join(step.join_confidence));
        }

        let mut rows = rows.into_values().collect::<Vec<_>>();
        rows.sort_by(|a, b| {
            b.gas
                .cmp(&a.gas)
                .then_with(|| b.executions.cmp(&a.executions))
                .then_with(|| a.pc.cmp(&b.pc))
        });
        Ok(HotPathByIterationReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            policy: request.policy.to_string(),
            loop_key: index.loop_key,
            scope: if loop_members.is_empty() {
                "all runtime steps; loop iteration facts unavailable".to_string()
            } else {
                "compiler-derived loop membership".to_string()
            },
            total_steps,
            total_gas,
            rows,
            confidence: runtime_report_confidence(total_steps > 0, &evidence),
        })
    }

    fn value_flow_at_pc(&self, request: ValueFlowAtPcRequest) -> QueryResult<ValueFlowAtPcReport> {
        let index = TraceIndex::new(&self.snapshot);
        let evidence = index.runtime_evidence(request.trace_id.as_deref());
        let mut rows = Vec::new();

        for step in index.execution_steps(request.trace_id.as_deref()) {
            if step.pc != request.pc {
                continue;
            }
            if request
                .code_object
                .as_ref()
                .is_some_and(|code_object| code_object != &step.code_object)
            {
                continue;
            }
            rows.push(ValueFlowAtPcRow {
                step: step.step.clone(),
                instruction: step.instruction.clone(),
                opcode: step.opcode.clone(),
                gas_cost: step.gas_cost,
                stack_top: index
                    .stack_sample_for_step(&step.step)
                    .map(|sample| {
                        sample
                            .values_top_first
                            .iter()
                            .map(runtime_value_label)
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default(),
                storage_accesses: index
                    .storage_accesses_for_step(&step.step)
                    .into_iter()
                    .map(|access| RuntimeValueAccessRow {
                        event: access.access.clone(),
                        kind: format!("{:?}", access.kind),
                        location: runtime_value_label(&access.slot),
                        value_before: access.value_before.as_ref().map(runtime_value_label),
                        value_after: access.value_after.as_ref().map(runtime_value_label),
                        value: None,
                    })
                    .collect(),
                memory_accesses: index
                    .memory_accesses_for_step(&step.step)
                    .into_iter()
                    .map(|access| RuntimeValueAccessRow {
                        event: access.access.clone(),
                        kind: format!("{:?}", access.kind),
                        location: format!(
                            "mem[{}..{})",
                            access.offset,
                            access.offset + access.length
                        ),
                        value_before: None,
                        value_after: None,
                        value: access.value.as_ref().map(runtime_value_label),
                    })
                    .collect(),
                confidence: confidence_for_join(step.join_confidence),
            });
        }

        rows.sort_by_key(|row| row.step.display_label());
        let available = !rows.is_empty();
        Ok(ValueFlowAtPcReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            pc: request.pc,
            code_object: request.code_object,
            trace_id: request.trace_id,
            runtime: evidence.clone(),
            rows,
            confidence: runtime_report_confidence(available, &evidence),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct IntrospectionStatus {
    pub trace_hash: String,
    pub fact_count: usize,
    pub instruction_count: usize,
    pub data_source: String,
    pub target: String,
    pub config_hash: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopCostRequest {
    pub loop_key: Option<OriginExportKey>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopContentsRequest {
    pub loop_key: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainLocalRequest {
    pub local: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub local_key: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownRequest {
    pub schedule: String,
}

impl Default for GasBreakdownRequest {
    fn default() -> Self {
        Self {
            schedule: "cancun".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainPcRequest {
    pub pc: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBySourceRequest {
    pub schedule: String,
    pub policy: GasAttributionPolicy,
}

impl Default for GasBySourceRequest {
    fn default() -> Self {
        Self {
            schedule: "cancun".to_string(),
            policy: GasAttributionPolicy::default(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DynamicGasBySourceRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytecodeSizeBySourceRequest {
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasToSourceRequest {
    pub schedule: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

impl Default for GasToSourceRequest {
    fn default() -> Self {
        Self {
            schedule: "cancun".to_string(),
            trace_id: None,
            policy: GasAttributionPolicy::default(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct OptimizedCodeHonestyRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub schedule: Option<String>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum GasAttributionPolicy {
    #[default]
    ExclusivePrimary,
    Inclusive,
    SyntheticOverhead,
    CallInclusive,
    CallExclusive,
    RuntimeStepExclusive,
}

impl GasAttributionPolicy {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::ExclusivePrimary => "exclusive-primary",
            Self::Inclusive => "inclusive",
            Self::SyntheticOverhead => "synthetic-overhead",
            Self::CallInclusive => "call-inclusive",
            Self::CallExclusive => "call-exclusive",
            Self::RuntimeStepExclusive => "runtime-step-exclusive",
        }
    }
}

impl fmt::Display for GasAttributionPolicy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for GasAttributionPolicy {
    type Err = QueryError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "exclusive-primary" => Ok(Self::ExclusivePrimary),
            "inclusive" => Ok(Self::Inclusive),
            "synthetic-overhead" => Ok(Self::SyntheticOverhead),
            "call-inclusive" => Ok(Self::CallInclusive),
            "call-exclusive" => Ok(Self::CallExclusive),
            "runtime-step-exclusive" => Ok(Self::RuntimeStepExclusive),
            _ => Err(QueryError::InvalidRequest(format!(
                "unknown gas attribution policy {value:?}"
            ))),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariablesAtPcRequest {
    pub pc: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub code_object: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeGasBySourceRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

impl Default for RuntimeGasBySourceRequest {
    fn default() -> Self {
        Self {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeTraceFilterRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

impl Default for RuntimeTraceFilterRequest {
    fn default() -> Self {
        Self {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageAccessesBySlotRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub slot: Option<String>,
    #[serde(default)]
    pub policy: GasAttributionPolicy,
}

impl Default for StorageAccessesBySlotRequest {
    fn default() -> Self {
        Self {
            trace_id: None,
            slot: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValueFlowAtPcRequest {
    pub pc: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub code_object: Option<OriginExportKey>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceQueryHttpRequest {
    pub auth_token: String,
    pub uri: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub config_hash: Option<String>,
    #[serde(flatten)]
    pub query: TraceQueryRequest,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TraceQueryRequest {
    LoopCost {
        #[serde(default)]
        loop_key: Option<OriginExportKey>,
    },
    LoopContents {
        #[serde(default)]
        loop_key: Option<OriginExportKey>,
    },
    ExplainLocal {
        local: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        local_key: Option<OriginExportKey>,
    },
    GasBreakdown {
        #[serde(default = "default_gas_schedule")]
        schedule: String,
    },
    ExplainPc {
        pc: u32,
    },
    GasBySource {
        #[serde(default = "default_gas_schedule")]
        schedule: String,
        #[serde(default)]
        policy: GasAttributionPolicy,
    },
    BytecodeSizeBySource {
        #[serde(default)]
        policy: GasAttributionPolicy,
    },
    DynamicGasBySource {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default)]
        policy: GasAttributionPolicy,
    },
    GasToSource {
        #[serde(default = "default_gas_schedule")]
        schedule: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default)]
        policy: GasAttributionPolicy,
    },
    OptimizedCodeHonesty {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        schedule: Option<String>,
    },
    AttributionAudit,
    VariablesAtPc {
        pc: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        code_object: Option<OriginExportKey>,
    },
    RuntimeGasBySource {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default = "default_runtime_gas_policy")]
        policy: GasAttributionPolicy,
    },
    StorageWritesBySource {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default = "default_runtime_gas_policy")]
        policy: GasAttributionPolicy,
    },
    StorageAccessesBySlot {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        slot: Option<String>,
        #[serde(default = "default_runtime_gas_policy")]
        policy: GasAttributionPolicy,
    },
    CallCostByCallsite {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
    },
    MemoryGrowthBySource {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default = "default_runtime_gas_policy")]
        policy: GasAttributionPolicy,
    },
    RevertAttribution {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
    },
    HotPathByIteration {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
        #[serde(default = "default_runtime_gas_policy")]
        policy: GasAttributionPolicy,
    },
    ValueFlowAtPc {
        pc: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        code_object: Option<OriginExportKey>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trace_id: Option<String>,
    },
}

impl TraceQueryRequest {
    pub fn loop_cost() -> Self {
        Self::LoopCost { loop_key: None }
    }

    pub fn loop_contents() -> Self {
        Self::LoopContents { loop_key: None }
    }

    pub fn explain_local(local: impl Into<String>) -> Self {
        Self::ExplainLocal {
            local: local.into(),
            local_key: None,
        }
    }

    pub fn gas_breakdown(schedule: impl Into<String>) -> Self {
        Self::GasBreakdown {
            schedule: schedule.into(),
        }
    }

    pub fn explain_pc(pc: u32) -> Self {
        Self::ExplainPc { pc }
    }

    pub fn gas_by_source(schedule: impl Into<String>) -> Self {
        Self::GasBySource {
            schedule: schedule.into(),
            policy: GasAttributionPolicy::default(),
        }
    }

    pub fn bytecode_size_by_source() -> Self {
        Self::BytecodeSizeBySource {
            policy: GasAttributionPolicy::default(),
        }
    }

    pub fn dynamic_gas_by_source() -> Self {
        Self::DynamicGasBySource {
            trace_id: None,
            policy: GasAttributionPolicy::default(),
        }
    }

    pub fn gas_to_source(schedule: impl Into<String>) -> Self {
        Self::GasToSource {
            schedule: schedule.into(),
            trace_id: None,
            policy: GasAttributionPolicy::default(),
        }
    }

    pub fn optimized_code_honesty() -> Self {
        Self::OptimizedCodeHonesty { schedule: None }
    }

    pub fn attribution_audit() -> Self {
        Self::AttributionAudit
    }

    pub fn variables_at_pc(pc: u32) -> Self {
        Self::VariablesAtPc {
            pc,
            code_object: None,
        }
    }

    pub fn runtime_gas_by_source() -> Self {
        Self::RuntimeGasBySource {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }

    pub fn storage_writes_by_source() -> Self {
        Self::StorageWritesBySource {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }

    pub fn storage_accesses_by_slot() -> Self {
        Self::StorageAccessesBySlot {
            trace_id: None,
            slot: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }

    pub fn call_cost_by_callsite() -> Self {
        Self::CallCostByCallsite { trace_id: None }
    }

    pub fn memory_growth_by_source() -> Self {
        Self::MemoryGrowthBySource {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }

    pub fn revert_attribution() -> Self {
        Self::RevertAttribution { trace_id: None }
    }

    pub fn hot_path_by_iteration() -> Self {
        Self::HotPathByIteration {
            trace_id: None,
            policy: GasAttributionPolicy::RuntimeStepExclusive,
        }
    }

    pub fn value_flow_at_pc(pc: u32) -> Self {
        Self::ValueFlowAtPc {
            pc,
            code_object: None,
            trace_id: None,
        }
    }
}

fn default_gas_schedule() -> String {
    GasBreakdownRequest::default().schedule
}

fn default_runtime_gas_policy() -> GasAttributionPolicy {
    GasAttributionPolicy::RuntimeStepExclusive
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
#[allow(clippy::large_enum_variant)]
pub enum TraceQueryHttpResponse {
    Ok {
        report: TraceQueryReport,
        #[serde(default)]
        cache_hit: bool,
        #[serde(default)]
        query_duration_ms: u64,
    },
    Error {
        reason: String,
        #[serde(default)]
        cache_hit: bool,
        #[serde(default)]
        query_duration_ms: u64,
    },
    Unauthorized {
        reason: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "report", rename_all = "snake_case")]
pub enum TraceQueryReport {
    LoopCost(LoopCostReport),
    LoopContents(LoopContentsReport),
    ExplainLocal(ExplainLocalReport),
    GasBreakdown(GasBreakdownReport),
    ExplainPc(ExplainPcReport),
    GasBySource(GasBySourceReport),
    BytecodeSizeBySource(BytecodeSizeBySourceReport),
    DynamicGasBySource(DynamicGasBySourceReport),
    GasToSource(GasToSourceReport),
    OptimizedCodeHonesty(OptimizedCodeHonestyReport),
    AttributionAudit(AttributionAuditReport),
    VariablesAtPc(VariablesAtPcReport),
    RuntimeGasBySource(RuntimeGasBySourceReport),
    StorageWritesBySource(StorageWritesBySourceReport),
    StorageAccessesBySlot(StorageAccessesBySlotReport),
    CallCostByCallsite(CallCostByCallsiteReport),
    MemoryGrowthBySource(MemoryGrowthBySourceReport),
    RevertAttribution(RevertAttributionReport),
    HotPathByIteration(HotPathByIterationReport),
    ValueFlowAtPc(ValueFlowAtPcReport),
}

pub fn run_trace_query(
    service: &impl IntrospectionService,
    request: TraceQueryRequest,
) -> QueryResult<TraceQueryReport> {
    match request {
        TraceQueryRequest::LoopCost { loop_key } => service
            .loop_cost(LoopCostRequest { loop_key })
            .map(TraceQueryReport::LoopCost),
        TraceQueryRequest::LoopContents { loop_key } => service
            .loop_contents(LoopContentsRequest { loop_key })
            .map(TraceQueryReport::LoopContents),
        TraceQueryRequest::ExplainLocal { local, local_key } => service
            .explain_local(ExplainLocalRequest { local, local_key })
            .map(TraceQueryReport::ExplainLocal),
        TraceQueryRequest::GasBreakdown { schedule } => service
            .gas_breakdown(GasBreakdownRequest { schedule })
            .map(TraceQueryReport::GasBreakdown),
        TraceQueryRequest::ExplainPc { pc } => service
            .explain_pc(ExplainPcRequest { pc })
            .map(TraceQueryReport::ExplainPc),
        TraceQueryRequest::GasBySource { schedule, policy } => service
            .gas_by_source(GasBySourceRequest { schedule, policy })
            .map(TraceQueryReport::GasBySource),
        TraceQueryRequest::BytecodeSizeBySource { policy } => service
            .bytecode_size_by_source(BytecodeSizeBySourceRequest { policy })
            .map(TraceQueryReport::BytecodeSizeBySource),
        TraceQueryRequest::DynamicGasBySource { trace_id, policy } => service
            .dynamic_gas_by_source(DynamicGasBySourceRequest { trace_id, policy })
            .map(TraceQueryReport::DynamicGasBySource),
        TraceQueryRequest::GasToSource {
            schedule,
            trace_id,
            policy,
        } => service
            .gas_to_source(GasToSourceRequest {
                schedule,
                trace_id,
                policy,
            })
            .map(TraceQueryReport::GasToSource),
        TraceQueryRequest::OptimizedCodeHonesty { schedule } => service
            .optimized_code_honesty(OptimizedCodeHonestyRequest { schedule })
            .map(TraceQueryReport::OptimizedCodeHonesty),
        TraceQueryRequest::AttributionAudit => service
            .attribution_audit()
            .map(TraceQueryReport::AttributionAudit),
        TraceQueryRequest::VariablesAtPc { pc, code_object } => service
            .variables_at_pc(VariablesAtPcRequest { pc, code_object })
            .map(TraceQueryReport::VariablesAtPc),
        TraceQueryRequest::RuntimeGasBySource { trace_id, policy } => service
            .runtime_gas_by_source(RuntimeGasBySourceRequest { trace_id, policy })
            .map(TraceQueryReport::RuntimeGasBySource),
        TraceQueryRequest::StorageWritesBySource { trace_id, policy } => service
            .storage_writes_by_source(RuntimeTraceFilterRequest { trace_id, policy })
            .map(TraceQueryReport::StorageWritesBySource),
        TraceQueryRequest::StorageAccessesBySlot {
            trace_id,
            slot,
            policy,
        } => service
            .storage_accesses_by_slot(StorageAccessesBySlotRequest {
                trace_id,
                slot,
                policy,
            })
            .map(TraceQueryReport::StorageAccessesBySlot),
        TraceQueryRequest::CallCostByCallsite { trace_id } => service
            .call_cost_by_callsite(RuntimeTraceFilterRequest {
                trace_id,
                policy: GasAttributionPolicy::RuntimeStepExclusive,
            })
            .map(TraceQueryReport::CallCostByCallsite),
        TraceQueryRequest::MemoryGrowthBySource { trace_id, policy } => service
            .memory_growth_by_source(RuntimeTraceFilterRequest { trace_id, policy })
            .map(TraceQueryReport::MemoryGrowthBySource),
        TraceQueryRequest::RevertAttribution { trace_id } => service
            .revert_attribution(RuntimeTraceFilterRequest {
                trace_id,
                policy: GasAttributionPolicy::RuntimeStepExclusive,
            })
            .map(TraceQueryReport::RevertAttribution),
        TraceQueryRequest::HotPathByIteration { trace_id, policy } => service
            .hot_path_by_iteration(RuntimeTraceFilterRequest { trace_id, policy })
            .map(TraceQueryReport::HotPathByIteration),
        TraceQueryRequest::ValueFlowAtPc {
            pc,
            code_object,
            trace_id,
        } => service
            .value_flow_at_pc(ValueFlowAtPcRequest {
                pc,
                code_object,
                trace_id,
            })
            .map(TraceQueryReport::ValueFlowAtPc),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceWorkbenchProjectionRequest {
    pub input_path: String,
    pub target: String,
    pub opt_level: String,
    pub view: String,
    pub source_text: Option<String>,
    pub related_source_texts: BTreeMap<String, String>,
    pub document_version: Option<i32>,
    pub query_duration_ms: u64,
    pub compiler_commit: String,
    pub data_source: String,
}

pub fn trace_workbench_report_projection(
    service: &impl IntrospectionService,
    snapshot: &TraceSnapshot,
    request: TraceWorkbenchProjectionRequest,
) -> serde_json::Value {
    let status = service.status();
    let attribution_audit = service.attribution_audit().ok();
    let validation = snapshot.validation();
    let optimized_linked = attribution_audit
        .as_ref()
        .map(|audit| audit.optimized_sonatina_linked_pcs)
        .unwrap_or_default();
    let prepared_linked = attribution_audit
        .as_ref()
        .map(|audit| audit.prepared_linked_pcs)
        .unwrap_or_default();
    let missing_prepared = attribution_audit
        .as_ref()
        .map(|audit| audit.missing_optimized_to_prepared_lineage_pcs)
        .unwrap_or_default();
    let bytecode_count = attribution_audit
        .as_ref()
        .map(|audit| audit.total_bytecode_pcs)
        .unwrap_or_default();
    let loop_report = service.loop_contents(LoopContentsRequest::default()).ok();
    let closure_set = loop_report.as_ref().map(|loop_report| {
        origin_closure::build_origin_closure_set(&request.input_path, snapshot, loop_report)
    });
    let component_classes_by_key = origin_closure::component_classes_by_origin_key(snapshot);
    let mut classes_by_key = closure_set
        .as_ref()
        .map(|closure_set| origin_closure::classes_by_origin_key(&closure_set.closures))
        .unwrap_or_default();
    trace_workbench_merge_classes_by_key(&mut classes_by_key, component_classes_by_key.clone());
    let source = trace_workbench_source_projection(
        &request.input_path,
        request.source_text.as_deref(),
        &request.related_source_texts,
        snapshot,
        &classes_by_key,
    );
    let missing_lineage = trace_workbench_missing_lineage_index(attribution_audit.as_ref());
    let panels = trace_workbench_panes(
        snapshot,
        &request.input_path,
        request.source_text.as_deref(),
        &request.related_source_texts,
        &classes_by_key,
        &missing_lineage,
    );
    let indexes = trace_workbench_projection_indexes(&source, &panels);
    let source_lines_for_audit = request
        .source_text
        .as_deref()
        .map(trace_workbench_source_lines_for_audit)
        .unwrap_or_default();
    let closure_audit = closure_set.as_ref().map(|closure_set| {
        origin_closure::audit_origin_closures(
            &request.input_path,
            status.target.as_str(),
            request.data_source.as_str(),
            loop_report
                .as_ref()
                .map(|loop_report| loop_report.target_instructions.len())
                .unwrap_or_default(),
            &closure_set.closures,
            &source_lines_for_audit,
            snapshot,
        )
    });
    let closures = closure_set
        .as_ref()
        .map(|closure_set| serde_json::to_value(&closure_set.closures).unwrap_or_default())
        .unwrap_or_else(|| serde_json::json!([]));
    let static_analysis = static_analysis::static_analysis_report(snapshot);
    let duplicate_shapes = trace_workbench_duplicate_shape_report(snapshot);
    let provenance = trace_workbench_provenance_status(
        closure_set.as_ref(),
        optimized_linked,
        prepared_linked,
        missing_prepared,
    );
    let trace_profile = trace_workbench_trace_profile(snapshot);
    let parity_summary = trace_workbench_projection_parity_summary(
        snapshot,
        &request,
        trace_profile,
        &provenance,
        attribution_audit.as_ref(),
        &panels,
        &component_classes_by_key,
    );
    serde_json::json!({
        "revision": {
            "id": request.document_version.unwrap_or_default().max(0) as u64,
            "document_version": request.document_version,
            "status": "ready",
            "config_hash": status.config_hash,
        },
        "metadata": {
            "input_path": request.input_path,
            "target": status.target,
            "data_source": request.data_source,
            "compiler_commit": request.compiler_commit,
            "trace_profile": trace_profile,
            "flags": [
                format!("source={}", request.data_source),
                format!("target={}", request.target),
                format!("optimize={}", request.opt_level),
                format!("view={}", request.view),
            ],
            "document_version": request.document_version,
            "query_duration_ms": request.query_duration_ms,
        },
        "provenance": {
            "source_to_optimized": provenance.source_to_optimized,
            "optimized_to_prepared": provenance.optimized_to_prepared,
            "prepared_to_bytecode": provenance.prepared_to_bytecode,
            "trace_profile": trace_profile,
            "summary": if provenance.source_to_optimized == "available" && provenance.prepared_to_bytecode == "available" && provenance.optimized_to_prepared == "missing" {
                "bytecode is prepared-linked; exact source attribution needs optimized to prepared lineage"
            } else if trace_profile.profile == "partial_preopt" {
                "live trace is partial: MIR and Sonatina pre-opt are available, but optimized/prepared lineage is not emitted in this model"
            } else if provenance.source_to_optimized == "available" && provenance.optimized_to_prepared == "available" && provenance.prepared_to_bytecode == "available" {
                "source to optimized to prepared to bytecode available"
            } else {
                "provenance is partial; inspect gaps for missing compiler evidence"
            },
        },
        "parity_summary": parity_summary,
        "counts": {
            "facts": validation.summary.fact_count,
            "origin_edges": validation.summary.edge_count,
            "instructions": validation.summary.instruction_count,
            "source_spans": snapshot.facts().iter().filter(|fact| matches!(fact, trace_facts::TraceFact::SourceSpan(_))).count(),
        },
        "salsa": null,
        "audit": closure_audit,
        "static_analysis": static_analysis,
        "attribution_audit": attribution_audit,
        "duplicate_shapes": duplicate_shapes,
        "source": source,
        "panels": panels,
        "indexes": indexes,
        "selection_remap": trace_workbench_selection_remap(),
        "rail_components": {
            "classes_by_origin": component_classes_by_key,
        },
        "closures": closures,
        "bytecode_count": bytecode_count,
        "notes": [
            "Live trace workbench is served from the LSP-owned compiler state.",
            "The live endpoint uses the shared trace-query workbench projection path.",
            "The browser does not compute provenance or graph reachability."
        ],
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchDuplicateShapeReport {
    groups: Vec<TraceWorkbenchDuplicateShapeGroup>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchDuplicateShapeGroup {
    policy: String,
    dimension: String,
    digest: String,
    occurrence_count: usize,
    origins: Vec<String>,
}

fn trace_workbench_duplicate_shape_report(
    snapshot: &TraceSnapshot,
) -> TraceWorkbenchDuplicateShapeReport {
    let mut by_digest = BTreeMap::<(String, String, String), BTreeSet<String>>::new();
    for fact in snapshot.facts() {
        let TraceFact::ShapeNodeHash(hash) = fact else {
            continue;
        };
        for (dimension, digest) in hash.tree.iter() {
            by_digest
                .entry((
                    hash.policy.to_string(),
                    dimension.as_str().to_string(),
                    digest.to_string(),
                ))
                .or_default()
                .insert(hash.node.canonical_storage_key());
        }
    }
    let mut groups = by_digest
        .into_iter()
        .filter_map(|((policy, dimension, digest), origins)| {
            (origins.len() > 1).then(|| TraceWorkbenchDuplicateShapeGroup {
                policy,
                dimension,
                digest,
                occurrence_count: origins.len(),
                origins: origins.into_iter().take(12).collect(),
            })
        })
        .collect::<Vec<_>>();
    groups.sort_by(|left, right| {
        right
            .occurrence_count
            .cmp(&left.occurrence_count)
            .then_with(|| left.dimension.cmp(&right.dimension))
            .then_with(|| left.digest.cmp(&right.digest))
    });
    groups.truncate(25);
    TraceWorkbenchDuplicateShapeReport { groups }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TraceWorkbenchProvenanceStatus {
    source_to_optimized: &'static str,
    optimized_to_prepared: &'static str,
    prepared_to_bytecode: &'static str,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchTraceProfile {
    profile: &'static str,
    has_sonatina_preopt: bool,
    has_sonatina_postopt: bool,
    has_evm_prepared: bool,
    has_bytecode: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchProjectionParitySummary {
    target: String,
    opt_level: String,
    view: String,
    trace_profile: TraceWorkbenchTraceProfile,
    pane_rows: BTreeMap<&'static str, usize>,
    origin_counts: BTreeMap<String, usize>,
    edge_class_counts: BTreeMap<String, usize>,
    rail_component_counts: BTreeMap<&'static str, usize>,
    bytecode: TraceWorkbenchBytecodeParitySummary,
    missing_link_status: Option<LinkOverallStatus>,
    missing_required_count: usize,
    invalid_count: usize,
    source_to_optimized: &'static str,
    optimized_to_prepared: &'static str,
    prepared_to_bytecode: &'static str,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchBytecodeParitySummary {
    total_pcs: usize,
    source_exact_pcs: usize,
    optimized_sonatina_linked_pcs: usize,
    prepared_linked_pcs: usize,
    unmapped_pcs: usize,
}

fn trace_workbench_projection_parity_summary(
    snapshot: &TraceSnapshot,
    request: &TraceWorkbenchProjectionRequest,
    trace_profile: TraceWorkbenchTraceProfile,
    provenance: &TraceWorkbenchProvenanceStatus,
    attribution_audit: Option<&AttributionAuditReport>,
    panels: &[TraceWorkbenchPane],
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
) -> TraceWorkbenchProjectionParitySummary {
    let mut pane_rows = BTreeMap::new();
    for panel in panels {
        pane_rows.insert(panel.id, panel.rows.len());
    }

    let mut origin_counts = BTreeMap::new();
    let mut edge_class_counts = BTreeMap::new();
    for fact in snapshot.facts() {
        match fact {
            TraceFact::OriginNode(node) => {
                *origin_counts
                    .entry(node.key.kind().to_string())
                    .or_default() += 1;
            }
            TraceFact::OriginEdge(edge) => {
                *edge_class_counts
                    .entry(trace_workbench_edge_class_key(edge.traversal_class()).to_string())
                    .or_default() += 1;
            }
            _ => {}
        }
    }

    let mut rail_component_counts = BTreeMap::<&'static str, BTreeSet<String>>::new();
    for classes in component_classes_by_key.values() {
        for class in classes {
            if let Some(rail) = trace_workbench_component_rail(class) {
                rail_component_counts
                    .entry(rail)
                    .or_default()
                    .insert(class.clone());
            }
        }
    }
    let rail_component_counts = rail_component_counts
        .into_iter()
        .map(|(rail, classes)| (rail, classes.len()))
        .collect();

    let missing_links = attribution_audit.and_then(|audit| audit.missing_links.as_ref());
    TraceWorkbenchProjectionParitySummary {
        target: request.target.clone(),
        opt_level: request.opt_level.clone(),
        view: request.view.clone(),
        trace_profile,
        pane_rows,
        origin_counts,
        edge_class_counts,
        rail_component_counts,
        bytecode: TraceWorkbenchBytecodeParitySummary {
            total_pcs: attribution_audit
                .map(|audit| audit.total_bytecode_pcs)
                .unwrap_or_default(),
            source_exact_pcs: attribution_audit
                .map(|audit| audit.source_exact_pcs)
                .unwrap_or_default(),
            optimized_sonatina_linked_pcs: attribution_audit
                .map(|audit| audit.optimized_sonatina_linked_pcs)
                .unwrap_or_default(),
            prepared_linked_pcs: attribution_audit
                .map(|audit| audit.prepared_linked_pcs)
                .unwrap_or_default(),
            unmapped_pcs: attribution_audit
                .map(|audit| audit.unmapped_pcs)
                .unwrap_or_default(),
        },
        missing_link_status: missing_links.map(|report| report.summary.status),
        missing_required_count: missing_links
            .map(|report| report.summary.missing_required_count)
            .unwrap_or_default(),
        invalid_count: missing_links
            .map(|report| report.summary.invalid_count)
            .unwrap_or_default(),
        source_to_optimized: provenance.source_to_optimized,
        optimized_to_prepared: provenance.optimized_to_prepared,
        prepared_to_bytecode: provenance.prepared_to_bytecode,
    }
}

fn trace_workbench_edge_class_key(class: OriginEdgeTraversalClass) -> &'static str {
    match class {
        OriginEdgeTraversalClass::ExactAttribution => "exact_attribution",
        OriginEdgeTraversalClass::Structural => "structural",
        OriginEdgeTraversalClass::Contextual => "contextual",
        OriginEdgeTraversalClass::Synthetic => "synthetic",
        OriginEdgeTraversalClass::SnapshotAlias => "snapshot_alias",
        OriginEdgeTraversalClass::Unmapped => "unmapped",
    }
}

fn trace_workbench_component_rail(class: &str) -> Option<&'static str> {
    if class.starts_with("exact-c-") {
        Some("exact")
    } else if class.starts_with("generated-c-") {
        Some("generated")
    } else if class.starts_with("prepared-c-") {
        Some("prepared")
    } else if class.starts_with("context-c-") {
        Some("context")
    } else if class.starts_with("structural-c-") {
        Some("structural")
    } else {
        None
    }
}

fn trace_workbench_trace_profile(snapshot: &TraceSnapshot) -> TraceWorkbenchTraceProfile {
    let mut has_sonatina_preopt = false;
    let mut has_sonatina_postopt = false;
    let mut has_evm_prepared = false;
    let mut has_bytecode = false;
    for fact in snapshot.facts() {
        let TraceFact::OriginNode(node) = fact else {
            continue;
        };
        match node.key.kind() {
            kind if kind.starts_with("sonatina.preopt.") => has_sonatina_preopt = true,
            kind if kind.starts_with("sonatina.postopt.") => has_sonatina_postopt = true,
            kind if kind.starts_with("sonatina.evm.prepared.") => has_evm_prepared = true,
            kind if kind.starts_with("bytecode.") => has_bytecode = true,
            _ => {}
        }
    }
    let profile = if has_sonatina_preopt && !has_sonatina_postopt {
        "partial_preopt"
    } else if has_sonatina_postopt && has_evm_prepared && has_bytecode {
        "postopt_prepared_bytecode"
    } else if has_sonatina_postopt {
        "postopt"
    } else {
        "partial"
    };
    TraceWorkbenchTraceProfile {
        profile,
        has_sonatina_preopt,
        has_sonatina_postopt,
        has_evm_prepared,
        has_bytecode,
    }
}

fn trace_workbench_provenance_status(
    closure_set: Option<&origin_closure::OriginClosureSet>,
    optimized_linked: usize,
    prepared_linked: usize,
    missing_prepared: usize,
) -> TraceWorkbenchProvenanceStatus {
    let source_to_optimized = closure_set.is_some_and(|closure_set| {
        closure_set
            .closures
            .iter()
            .any(|closure| closure.counts.hir > 0 && closure.counts.sonatina_post > 0)
    }) || optimized_linked > 0;
    let optimized_to_prepared = closure_set.is_some_and(|closure_set| {
        closure_set.closures.iter().any(|closure| {
            closure
                .keys
                .iter()
                .any(|key| key.contains("sonatina.postopt."))
                && closure
                    .keys
                    .iter()
                    .any(|key| key.contains("sonatina.evm.prepared."))
        })
    }) || (prepared_linked > 0 && missing_prepared == 0);
    let prepared_to_bytecode = closure_set.is_some_and(|closure_set| {
        closure_set.closures.iter().any(|closure| {
            closure
                .keys
                .iter()
                .any(|key| key.contains("sonatina.evm.prepared."))
                && closure.keys.iter().any(|key| key.contains("bytecode.pc"))
        })
    }) || prepared_linked > 0;
    TraceWorkbenchProvenanceStatus {
        source_to_optimized: trace_workbench_status_word(source_to_optimized),
        optimized_to_prepared: trace_workbench_status_word(optimized_to_prepared),
        prepared_to_bytecode: trace_workbench_status_word(prepared_to_bytecode),
    }
}

fn trace_workbench_status_word(available: bool) -> &'static str {
    if available { "available" } else { "missing" }
}

fn trace_workbench_selection_remap() -> serde_json::Value {
    serde_json::json!({
        "strategy_order": [
            "origin",
            "bytecode_pc",
            "source_line",
            "component",
            "nearest_row",
        ],
        "indexes": [
            "origin_to_rows",
            "bytecode_pcs",
            "source_lines",
            "source_intervals",
            "component_to_rows",
            "stable_identities",
            "pc_intervals",
        ],
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[allow(dead_code)]
#[serde(rename_all = "snake_case")]
enum TraceWorkbenchPaneRowKind {
    FileHeader,
    FunctionHeader,
    BlockHeader,
    Instruction,
    Statement,
    Terminator,
    BoundaryMarker,
    DerivedBytecodeBlockHeader,
    Blank,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[allow(dead_code)]
#[serde(rename_all = "snake_case")]
enum TraceWorkbenchDisplayStatus {
    Exact,
    Generated,
    GeneratedDownstream,
    Context,
    PreparedLinked,
    MissingOptimizedToPrepared,
    MissingDownstreamLineage,
    SourceOnly,
    CompilerGenerated,
    Unmapped,
    Ambiguous,
    Invalid,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize)]
struct TraceWorkbenchRailClasses {
    exact: Vec<String>,
    generated: Vec<String>,
    prepared: Vec<String>,
    context: Vec<String>,
    boundary: Vec<String>,
    legacy: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchRowStableIdentity {
    kind: &'static str,
    value: String,
}

fn trace_workbench_stable_identity(
    kind: &'static str,
    value: impl Into<String>,
) -> TraceWorkbenchRowStableIdentity {
    TraceWorkbenchRowStableIdentity {
        kind,
        value: value.into(),
    }
}

fn trace_workbench_stable_identity_index_key(identity: &TraceWorkbenchRowStableIdentity) -> String {
    format!("{}:{}", identity.kind, identity.value)
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchPane {
    id: &'static str,
    title: &'static str,
    summary: String,
    rows: Vec<TraceWorkbenchPaneRow>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchPaneRow {
    row_id: String,
    key: Option<String>,
    kind: TraceWorkbenchPaneRowKind,
    indent: u8,
    label: String,
    meta: String,
    text: String,
    compact_text: String,
    stable_identities: Vec<TraceWorkbenchRowStableIdentity>,
    display_status: Option<TraceWorkbenchDisplayStatus>,
    rail_classes: TraceWorkbenchRailClasses,
    classes: Vec<String>,
    debug: TraceWorkbenchRowDebugInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchSourceLine {
    row_id: String,
    number: u32,
    text: String,
    stable_identities: Vec<TraceWorkbenchRowStableIdentity>,
    classes: Vec<String>,
    suppress_rail_status: bool,
    display_status: Option<TraceWorkbenchDisplayStatus>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct TraceWorkbenchRowDebugInfo {
    origin_key: Option<String>,
    origin_kind: Option<String>,
    owner_key: Option<String>,
    local_key: Option<String>,
    instruction_index: Option<u32>,
    raw_text: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct TraceWorkbenchMissingLineageIndex {
    origins: BTreeSet<String>,
}

impl TraceWorkbenchMissingLineageIndex {
    fn contains(&self, key: &OriginExportKey) -> bool {
        self.origins.contains(&key.canonical_storage_key())
    }
}

fn trace_workbench_missing_lineage_index(
    attribution_audit: Option<&AttributionAuditReport>,
) -> TraceWorkbenchMissingLineageIndex {
    let mut origins = BTreeSet::new();
    if let Some(attribution_audit) = attribution_audit {
        for gap in &attribution_audit.lineage_gaps {
            origins.insert(gap.bytecode_pc.canonical_storage_key());
            origins.insert(gap.prepared_origin.canonical_storage_key());
        }
    }
    TraceWorkbenchMissingLineageIndex { origins }
}

fn trace_workbench_source_projection(
    input_path: &str,
    source_text: Option<&str>,
    related_source_texts: &BTreeMap<String, String>,
    snapshot: &TraceSnapshot,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
) -> serde_json::Value {
    let lines = source_text
        .map(|text| {
            trace_workbench_source_lines(input_path, text, snapshot, component_classes_by_key)
        })
        .unwrap_or_default();
    let confidence = if source_text.is_some() {
        "live source text from LSP workspace"
    } else {
        "source text unavailable; showing trace reports only"
    };
    serde_json::json!({
        "display_name": input_path,
        "confidence": confidence,
        "lines": lines,
        "related_sources": trace_workbench_related_sources(input_path, snapshot, component_classes_by_key, related_source_texts),
    })
}

fn trace_workbench_related_sources(
    input_path: &str,
    snapshot: &TraceSnapshot,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
    related_source_texts: &BTreeMap<String, String>,
) -> Vec<serde_json::Value> {
    let mut source_files = BTreeMap::<OriginExportKey, &trace_facts::SourceFileFact>::new();
    let mut lines_by_file = BTreeMap::<OriginExportKey, BTreeMap<u32, BTreeSet<String>>>::new();
    for fact in snapshot.facts() {
        if let TraceFact::SourceFile(source_file) = fact {
            source_files.insert(source_file.file_key.clone(), source_file);
        }
    }
    for fact in snapshot.facts() {
        let TraceFact::SourceSpan(span) = fact else {
            continue;
        };
        if origin_closure::source_owner_matches_input(span.file.owner_key(), input_path) {
            continue;
        }
        let classes = component_classes_by_key
            .get(&span.origin.canonical_storage_key())
            .into_iter()
            .flatten()
            .filter(|class| trace_workbench_is_component_class(class.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        for line in span.start_line..=span.end_line {
            lines_by_file
                .entry(span.file.clone())
                .or_default()
                .entry(line)
                .or_default()
                .extend(classes.iter().cloned());
        }
    }
    lines_by_file
        .into_iter()
        .filter_map(|(file, lines)| {
            let source_file = source_files.get(&file)?;
            let line_numbers = lines.keys().copied().collect::<Vec<_>>();
            let row_prefix = trace_workbench_origin_row_id(&file.canonical_storage_key());
            let source_text = related_source_texts
                .get(&file.canonical_storage_key())
                .or_else(|| related_source_texts.get(&source_file.uri));
            let rows = lines
                .into_iter()
                .map(|(number, classes)| {
                    let classes = classes.into_iter().collect::<Vec<_>>();
                    let text = source_text
                        .and_then(|source_text| trace_workbench_line_text(source_text, number))
                        .unwrap_or("source text unavailable");
                    serde_json::json!({
                        "row_id": format!("{row_prefix}-line-{number}"),
                        "number": number,
                        "text": text,
                        "stable_identities": [
                            trace_workbench_stable_identity(
                                "source_line",
                                format!("{}:{number}", file.canonical_storage_key()),
                            ),
                        ],
                        "classes": classes,
                        "display_status": trace_workbench_status_for_source_line(&classes),
                    })
                })
                .collect::<Vec<_>>();
            Some(serde_json::json!({
                "display_name": source_file.display_name,
                "origin": file.canonical_storage_key(),
                "uri": source_file.uri,
                "content_hash": source_file.content_hash,
                "summary": trace_workbench_line_range_summary(&line_numbers),
                "source_text_available": source_text.is_some(),
                "lines": rows,
            }))
        })
        .collect()
}

fn trace_workbench_line_text(source_text: &str, line_number: u32) -> Option<&str> {
    let index = usize::try_from(line_number.checked_sub(1)?).ok()?;
    source_text.lines().nth(index)
}

fn trace_workbench_line_range_summary(lines: &[u32]) -> String {
    if lines.is_empty() {
        return "no referenced lines".to_string();
    }
    let mut ranges = Vec::new();
    let mut start = lines[0];
    let mut end = lines[0];
    for line in lines.iter().copied().skip(1) {
        if line == end.saturating_add(1) {
            end = line;
            continue;
        }
        ranges.push(trace_workbench_format_line_range(start, end));
        start = line;
        end = line;
    }
    ranges.push(trace_workbench_format_line_range(start, end));
    format!("referenced lines {}", ranges.join(", "))
}

fn trace_workbench_format_line_range(start: u32, end: u32) -> String {
    if start == end {
        start.to_string()
    } else {
        format!("{start}-{end}")
    }
}

fn trace_workbench_source_lines_for_audit(
    source_text: &str,
) -> Vec<origin_closure::OriginClosureSourceLine> {
    source_text
        .lines()
        .enumerate()
        .map(|(index, text)| origin_closure::OriginClosureSourceLine {
            number: index as u32 + 1,
            text: text.to_string(),
        })
        .collect()
}

fn trace_workbench_merge_classes_by_key(
    classes_by_key: &mut BTreeMap<String, Vec<String>>,
    extra_classes: BTreeMap<String, Vec<String>>,
) {
    for (key, classes) in extra_classes {
        let entry = classes_by_key.entry(key).or_default();
        let mut seen = entry.iter().cloned().collect::<BTreeSet<_>>();
        for class in classes {
            if seen.insert(class.clone()) {
                entry.push(class);
            }
        }
    }
}

fn trace_workbench_source_lines(
    input_path: &str,
    source_text: &str,
    snapshot: &TraceSnapshot,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
) -> Vec<TraceWorkbenchSourceLine> {
    let mut exact_classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    let mut enclosing_classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    for fact in snapshot.facts() {
        let TraceFact::SourceSpan(span) = fact else {
            continue;
        };
        if !origin_closure::source_owner_matches_input(span.file.owner_key(), input_path) {
            continue;
        }
        let target = if span.start_line == span.end_line {
            exact_classes_by_line.entry(span.start_line).or_default()
        } else {
            for line in span.start_line..=span.end_line {
                if let Some(classes) =
                    component_classes_by_key.get(&span.origin.canonical_storage_key())
                {
                    enclosing_classes_by_line.entry(line).or_default().extend(
                        classes
                            .iter()
                            .filter(|class| trace_workbench_is_component_class(class.as_str()))
                            .cloned(),
                    );
                }
            }
            continue;
        };
        if let Some(classes) = component_classes_by_key.get(&span.origin.canonical_storage_key()) {
            target.extend(
                classes
                    .iter()
                    .filter(|class| trace_workbench_is_component_class(class.as_str()))
                    .cloned(),
            );
        }
    }

    source_text
        .lines()
        .enumerate()
        .map(|(index, text)| {
            let number = index as u32 + 1;
            let exact_classes = exact_classes_by_line.get(&number);
            let enclosing_classes = enclosing_classes_by_line.get(&number);
            let classes = exact_classes
                .or(enclosing_classes)
                .map(|classes| classes.iter().cloned().collect::<Vec<_>>())
                .unwrap_or_default();
            TraceWorkbenchSourceLine {
                row_id: trace_workbench_source_row_id(number),
                number,
                text: text.to_string(),
                stable_identities: vec![trace_workbench_stable_identity(
                    "source_line",
                    format!("main:{number}"),
                )],
                suppress_rail_status: exact_classes.is_none() && enclosing_classes.is_some(),
                display_status: exact_classes
                    .map(|_| trace_workbench_status_for_source_line(&classes))
                    .unwrap_or_default(),
                classes,
            }
        })
        .collect()
}

fn trace_workbench_projection_indexes(
    source: &serde_json::Value,
    panels: &[TraceWorkbenchPane],
) -> serde_json::Value {
    let mut source_lines = BTreeMap::<String, String>::new();
    let mut origin_to_rows = BTreeMap::<String, BTreeSet<String>>::new();
    let mut component_to_rows = BTreeMap::<String, BTreeSet<String>>::new();
    let mut bytecode_pcs = BTreeMap::<String, BTreeSet<String>>::new();
    let mut stable_identities = BTreeMap::<String, BTreeSet<String>>::new();
    let mut source_intervals = Vec::<serde_json::Value>::new();
    let mut pc_intervals = Vec::<serde_json::Value>::new();

    for line in source
        .get("lines")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
    {
        let Some(row_id) = line.get("row_id").and_then(serde_json::Value::as_str) else {
            continue;
        };
        if let Some(number) = line.get("number").and_then(serde_json::Value::as_u64) {
            source_lines.insert(format!("main:{number}"), row_id.to_string());
            source_intervals.push(serde_json::json!({
                "source": "main",
                "start_line": number,
                "end_line": number,
                "row_id": row_id,
            }));
        }
        trace_workbench_index_stable_identities(line, row_id, &mut stable_identities);
        for class in line
            .get("classes")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(serde_json::Value::as_str)
        {
            component_to_rows
                .entry(class.to_string())
                .or_default()
                .insert(row_id.to_string());
        }
    }

    for line in source
        .get("related_sources")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .flat_map(|related_source| {
            related_source
                .get("lines")
                .and_then(serde_json::Value::as_array)
                .into_iter()
                .flatten()
        })
    {
        let Some(row_id) = line.get("row_id").and_then(serde_json::Value::as_str) else {
            continue;
        };
        let source = line
            .get("stable_identities")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .find_map(|identity| {
                (identity.get("kind").and_then(serde_json::Value::as_str) == Some("source_line"))
                    .then(|| identity.get("value").and_then(serde_json::Value::as_str))
                    .flatten()
            })
            .and_then(|value| value.rsplit_once(':').map(|(source, _)| source.to_string()))
            .unwrap_or_else(|| "related".to_string());
        if let Some(number) = line.get("number").and_then(serde_json::Value::as_u64) {
            source_intervals.push(serde_json::json!({
                "source": source,
                "start_line": number,
                "end_line": number,
                "row_id": row_id,
            }));
        }
        trace_workbench_index_stable_identities(line, row_id, &mut stable_identities);
        for class in line
            .get("classes")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(serde_json::Value::as_str)
        {
            component_to_rows
                .entry(class.to_string())
                .or_default()
                .insert(row_id.to_string());
        }
    }

    for panel in panels {
        for row in &panel.rows {
            if let Some(key) = row.key.as_ref() {
                origin_to_rows
                    .entry(key.clone())
                    .or_default()
                    .insert(row.row_id.clone());
            }
            if row.debug.origin_kind.as_deref() == Some("bytecode.pc")
                && let Some(local_key) = row.debug.local_key.as_deref()
                && let Some(pc) = local_key.strip_prefix("pc:")
            {
                bytecode_pcs
                    .entry(pc.to_string())
                    .or_default()
                    .insert(row.row_id.clone());
                if let Ok(pc) = pc.parse::<u64>() {
                    pc_intervals.push(serde_json::json!({
                        "pc_start": pc,
                        "pc_end": pc.saturating_add(1),
                        "row_id": row.row_id.clone(),
                        "origin": row.key.clone(),
                    }));
                }
            }
            for identity in &row.stable_identities {
                stable_identities
                    .entry(trace_workbench_stable_identity_index_key(identity))
                    .or_default()
                    .insert(row.row_id.clone());
            }
            for class in &row.classes {
                component_to_rows
                    .entry(class.clone())
                    .or_default()
                    .insert(row.row_id.clone());
            }
        }
    }

    serde_json::json!({
        "source_lines": source_lines,
        "origin_to_rows": trace_workbench_index_sets_to_lists(origin_to_rows),
        "component_to_rows": trace_workbench_index_sets_to_lists(component_to_rows),
        "bytecode_pcs": trace_workbench_index_sets_to_lists(bytecode_pcs),
        "stable_identities": trace_workbench_index_sets_to_lists(stable_identities),
        "source_intervals": source_intervals,
        "pc_intervals": pc_intervals,
    })
}

fn trace_workbench_index_stable_identities(
    row: &serde_json::Value,
    row_id: &str,
    stable_identities: &mut BTreeMap<String, BTreeSet<String>>,
) {
    for identity in row
        .get("stable_identities")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
    {
        let Some(kind) = identity.get("kind").and_then(serde_json::Value::as_str) else {
            continue;
        };
        let Some(value) = identity.get("value").and_then(serde_json::Value::as_str) else {
            continue;
        };
        stable_identities
            .entry(format!("{kind}:{value}"))
            .or_default()
            .insert(row_id.to_string());
    }
}

fn trace_workbench_index_sets_to_lists(
    values: BTreeMap<String, BTreeSet<String>>,
) -> BTreeMap<String, Vec<String>> {
    values
        .into_iter()
        .map(|(key, rows)| (key, rows.into_iter().collect()))
        .collect()
}

fn trace_workbench_panes(
    snapshot: &TraceSnapshot,
    input_path: &str,
    source_text: Option<&str>,
    related_source_texts: &BTreeMap<String, String>,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
    missing_lineage: &TraceWorkbenchMissingLineageIndex,
) -> Vec<TraceWorkbenchPane> {
    let index =
        TraceWorkbenchProjectionIndex::new(snapshot, input_path, source_text, related_source_texts);
    [
        ("hir", "HIR", "High-level IR"),
        ("mir", "MIR", "Runtime MIR"),
        (
            "sonatina-pre",
            "Sonatina pre-opt",
            "Sonatina before optimization",
        ),
        (
            "sonatina-post",
            "Optimized Sonatina",
            "Optimized Sonatina trace view",
        ),
        (
            "sonatina-prepared",
            "EVM prepared",
            "EVM prepared/codegen identity",
        ),
        ("bytecode", "EVM bytecode", "Final bytecode disassembly"),
    ]
    .into_iter()
    .filter_map(|(id, title, summary)| {
        let rows =
            trace_workbench_panel_rows(id, &index, component_classes_by_key, missing_lineage);
        (!rows.is_empty()).then_some(TraceWorkbenchPane {
            id,
            title,
            summary: summary.to_string(),
            rows,
        })
    })
    .collect()
}

struct TraceWorkbenchProjectionIndex<'a> {
    origin_nodes: BTreeSet<OriginExportKey>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    source_spans: BTreeMap<OriginExportKey, &'a trace_facts::SourceSpanFact>,
    source_snippets: BTreeMap<OriginExportKey, String>,
    display_names: BTreeMap<OriginExportKey, String>,
}

impl<'a> TraceWorkbenchProjectionIndex<'a> {
    fn new(
        snapshot: &'a TraceSnapshot,
        input_path: &str,
        source_text: Option<&str>,
        related_source_texts: &BTreeMap<String, String>,
    ) -> Self {
        let mut origin_nodes = BTreeSet::new();
        let mut instructions = BTreeMap::new();
        let mut source_spans = BTreeMap::new();
        let mut source_snippets = BTreeMap::new();
        let mut display_names = BTreeMap::new();
        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginNode(node) => {
                    origin_nodes.insert(node.key.clone());
                }
                TraceFact::Instruction(instruction) => {
                    origin_nodes.insert(instruction.instruction.clone());
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                    if let Some(snippet) = trace_workbench_source_snippet(
                        input_path,
                        source_text,
                        related_source_texts,
                        span,
                    ) {
                        source_snippets.insert(span.origin.clone(), snippet);
                    }
                }
                TraceFact::DisplayName(display_name) => {
                    origin_nodes.insert(display_name.subject.clone());
                    display_names.insert(display_name.subject.clone(), display_name.name.clone());
                }
                _ => {}
            }
        }
        Self {
            origin_nodes,
            instructions,
            source_spans,
            source_snippets,
            display_names,
        }
    }
}

fn trace_workbench_source_snippet(
    input_path: &str,
    source_text: Option<&str>,
    related_source_texts: &BTreeMap<String, String>,
    span: &trace_facts::SourceSpanFact,
) -> Option<String> {
    let source_text =
        if origin_closure::source_owner_matches_input(span.file.owner_key(), input_path) {
            source_text
        } else {
            related_source_texts
                .get(&span.file.canonical_storage_key())
                .map(String::as_str)
                .or_else(|| {
                    related_source_texts
                        .get(span.file.owner_key())
                        .map(String::as_str)
                })
        }?;
    let start = span.start_byte as usize;
    let end = span.end_byte as usize;
    if start >= end || end > source_text.len() {
        return None;
    }
    let raw = source_text.get(start..end)?;
    trace_workbench_compact_source_snippet(raw)
}

fn trace_workbench_compact_source_snippet(raw: &str) -> Option<String> {
    let text = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    if text.is_empty() {
        None
    } else if text.len() > 80 {
        let mut truncated = text.chars().take(77).collect::<String>();
        truncated.push_str("...");
        Some(truncated)
    } else {
        Some(text)
    }
}

fn trace_workbench_panel_rows(
    panel: &str,
    index: &TraceWorkbenchProjectionIndex<'_>,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
    missing_lineage: &TraceWorkbenchMissingLineageIndex,
) -> Vec<TraceWorkbenchPaneRow> {
    let mut keys = index
        .origin_nodes
        .iter()
        .filter(|key| trace_workbench_key_belongs_to_panel(key, panel))
        .cloned()
        .collect::<Vec<_>>();
    keys.sort_by(|left, right| trace_workbench_compare_panel_keys(left, right, index));
    keys.into_iter()
        .map(|key| {
            trace_workbench_panel_row(&key, index, component_classes_by_key, missing_lineage)
        })
        .collect()
}

fn trace_workbench_compare_panel_keys(
    left: &OriginExportKey,
    right: &OriginExportKey,
    index: &TraceWorkbenchProjectionIndex<'_>,
) -> std::cmp::Ordering {
    match (index.instructions.get(left), index.instructions.get(right)) {
        (Some(left), Some(right)) => left
            .function
            .canonical_storage_key()
            .cmp(&right.function.canonical_storage_key())
            .then(left.index.cmp(&right.index))
            .then_with(|| {
                trace_workbench_natural_key_cmp(
                    &left.instruction.canonical_storage_key(),
                    &right.instruction.canonical_storage_key(),
                )
            }),
        (Some(left), None) => left
            .function
            .canonical_storage_key()
            .as_str()
            .cmp(right.owner_key())
            .then(std::cmp::Ordering::Less),
        (None, Some(right)) => left
            .owner_key()
            .cmp(&right.function.canonical_storage_key())
            .then(std::cmp::Ordering::Greater),
        (None, None) => left
            .owner_key()
            .cmp(right.owner_key())
            .then_with(|| trace_workbench_natural_key_cmp(left.local_key(), right.local_key())),
    }
}

fn trace_workbench_natural_key_cmp(left: &str, right: &str) -> Ordering {
    let left_bytes = left.as_bytes();
    let right_bytes = right.as_bytes();
    let mut left_index = 0;
    let mut right_index = 0;
    while left_index < left_bytes.len() && right_index < right_bytes.len() {
        let left_byte = left_bytes[left_index];
        let right_byte = right_bytes[right_index];
        if left_byte.is_ascii_digit() && right_byte.is_ascii_digit() {
            let left_start = left_index;
            let right_start = right_index;
            while left_index < left_bytes.len() && left_bytes[left_index].is_ascii_digit() {
                left_index += 1;
            }
            while right_index < right_bytes.len() && right_bytes[right_index].is_ascii_digit() {
                right_index += 1;
            }
            let ordering = trace_workbench_digit_run_cmp(
                &left[left_start..left_index],
                &right[right_start..right_index],
            );
            if ordering != Ordering::Equal {
                return ordering;
            }
            continue;
        }
        let ordering = left_byte.cmp(&right_byte);
        if ordering != Ordering::Equal {
            return ordering;
        }
        left_index += 1;
        right_index += 1;
    }
    left_bytes.len().cmp(&right_bytes.len())
}

fn trace_workbench_digit_run_cmp(left: &str, right: &str) -> Ordering {
    let left_significant = left.trim_start_matches('0');
    let right_significant = right.trim_start_matches('0');
    let left_significant = if left_significant.is_empty() {
        "0"
    } else {
        left_significant
    };
    let right_significant = if right_significant.is_empty() {
        "0"
    } else {
        right_significant
    };
    left_significant
        .len()
        .cmp(&right_significant.len())
        .then_with(|| left_significant.cmp(right_significant))
        .then_with(|| left.len().cmp(&right.len()))
}

fn trace_workbench_panel_row(
    key: &OriginExportKey,
    index: &TraceWorkbenchProjectionIndex<'_>,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
    missing_lineage: &TraceWorkbenchMissingLineageIndex,
) -> TraceWorkbenchPaneRow {
    let storage_key = key.canonical_storage_key();
    let instruction = index.instructions.get(key).copied();
    let mut classes = component_classes_by_key
        .get(&storage_key)
        .cloned()
        .unwrap_or_default();
    if trace_workbench_origin_key_is_generated(key) {
        classes.push("origin-generated".to_string());
    }
    classes.sort();
    classes.dedup();
    let kind = trace_workbench_row_kind(key, instruction);
    let compact_text = instruction
        .map(trace_workbench_compact_instruction_text)
        .unwrap_or_else(|| trace_workbench_compact_origin_text(key, index));
    let raw_text = instruction
        .map(|instruction| instruction.mnemonic.clone())
        .unwrap_or_else(|| storage_key.clone());
    TraceWorkbenchPaneRow {
        row_id: trace_workbench_origin_row_id(&storage_key),
        key: Some(storage_key),
        kind,
        indent: trace_workbench_row_indent(kind),
        label: trace_workbench_compact_origin_label(key, instruction),
        meta: trace_workbench_compact_origin_meta(key),
        text: compact_text.clone(),
        compact_text,
        stable_identities: trace_workbench_panel_row_stable_identities(key),
        display_status: trace_workbench_status_for_row(key, kind, &classes, missing_lineage),
        rail_classes: trace_workbench_split_rail_classes(&classes),
        classes,
        debug: TraceWorkbenchRowDebugInfo {
            origin_key: Some(key.canonical_storage_key()),
            origin_kind: Some(key.kind().to_string()),
            owner_key: Some(key.owner_key().to_string()),
            local_key: Some(key.local_key().to_string()),
            instruction_index: instruction.map(|instruction| instruction.index),
            raw_text,
        },
    }
}

fn trace_workbench_panel_row_stable_identities(
    key: &OriginExportKey,
) -> Vec<TraceWorkbenchRowStableIdentity> {
    let mut identities = vec![trace_workbench_stable_identity(
        "origin",
        key.canonical_storage_key(),
    )];
    if key.kind() == "bytecode.pc"
        && let Some(pc) = key.local_key().strip_prefix("pc:")
    {
        identities.push(trace_workbench_stable_identity(
            "bytecode_pc",
            pc.to_string(),
        ));
    }
    identities
}

fn trace_workbench_source_row_id(line_number: u32) -> String {
    format!("source-main-line-{line_number}")
}

fn trace_workbench_origin_row_id(storage_key: &str) -> String {
    let mut hash = 2166136261u32;
    for byte in storage_key.bytes() {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16777619);
    }
    format!("origin-{hash:08x}")
}

fn trace_workbench_row_kind(
    key: &OriginExportKey,
    instruction: Option<&InstructionFact>,
) -> TraceWorkbenchPaneRowKind {
    if instruction.is_some() {
        return match key.kind() {
            "runtime.stmt" => TraceWorkbenchPaneRowKind::Statement,
            "runtime.terminator" => TraceWorkbenchPaneRowKind::Terminator,
            _ => TraceWorkbenchPaneRowKind::Instruction,
        };
    }
    match key.kind() {
        "source.file" => TraceWorkbenchPaneRowKind::FileHeader,
        "runtime.function" => TraceWorkbenchPaneRowKind::FunctionHeader,
        "runtime.block" => TraceWorkbenchPaneRowKind::BlockHeader,
        "runtime.terminator" => TraceWorkbenchPaneRowKind::Terminator,
        kind if kind.ends_with(".function") => TraceWorkbenchPaneRowKind::FunctionHeader,
        kind if kind.ends_with(".block") => TraceWorkbenchPaneRowKind::BlockHeader,
        kind if kind.contains(".loop") => TraceWorkbenchPaneRowKind::BoundaryMarker,
        _ => TraceWorkbenchPaneRowKind::Instruction,
    }
}

fn trace_workbench_row_indent(kind: TraceWorkbenchPaneRowKind) -> u8 {
    match kind {
        TraceWorkbenchPaneRowKind::FileHeader | TraceWorkbenchPaneRowKind::FunctionHeader => 0,
        TraceWorkbenchPaneRowKind::BlockHeader
        | TraceWorkbenchPaneRowKind::BoundaryMarker
        | TraceWorkbenchPaneRowKind::DerivedBytecodeBlockHeader
        | TraceWorkbenchPaneRowKind::Blank => 1,
        TraceWorkbenchPaneRowKind::Instruction
        | TraceWorkbenchPaneRowKind::Statement
        | TraceWorkbenchPaneRowKind::Terminator => 2,
    }
}

fn trace_workbench_status_for_source_line(
    classes: &[String],
) -> Option<TraceWorkbenchDisplayStatus> {
    if classes.iter().any(|class| class.starts_with("exact-c-")) {
        return Some(TraceWorkbenchDisplayStatus::Exact);
    }
    if classes
        .iter()
        .any(|class| class.starts_with("generated-c-"))
    {
        return Some(TraceWorkbenchDisplayStatus::GeneratedDownstream);
    }
    if classes.iter().any(|class| class.starts_with("context-c-")) {
        return Some(TraceWorkbenchDisplayStatus::Context);
    }
    None
}

fn trace_workbench_status_for_row(
    key: &OriginExportKey,
    kind: TraceWorkbenchPaneRowKind,
    classes: &[String],
    missing_lineage: &TraceWorkbenchMissingLineageIndex,
) -> Option<TraceWorkbenchDisplayStatus> {
    if classes.iter().any(|class| class.starts_with("exact-c-")) {
        return Some(TraceWorkbenchDisplayStatus::Exact);
    }
    if missing_lineage.contains(key) {
        return Some(TraceWorkbenchDisplayStatus::MissingOptimizedToPrepared);
    }
    if kind == TraceWorkbenchPaneRowKind::Instruction
        && key.kind() == "bytecode.pc"
        && classes.iter().any(|class| class.starts_with("prepared-c-"))
    {
        return Some(TraceWorkbenchDisplayStatus::PreparedLinked);
    }
    if trace_workbench_origin_key_is_generated(key)
        || classes.iter().any(|class| class == "origin-generated")
    {
        return Some(match key.kind() {
            kind if kind.starts_with("source.") => TraceWorkbenchDisplayStatus::GeneratedDownstream,
            _ => TraceWorkbenchDisplayStatus::Generated,
        });
    }
    if classes
        .iter()
        .any(|class| class.starts_with("generated-c-"))
    {
        return Some(TraceWorkbenchDisplayStatus::GeneratedDownstream);
    }
    if classes
        .iter()
        .any(|class| class.starts_with("context-c-") || class == "origin-contextual")
    {
        return Some(TraceWorkbenchDisplayStatus::Context);
    }
    None
}

fn trace_workbench_split_rail_classes(classes: &[String]) -> TraceWorkbenchRailClasses {
    let mut rails = TraceWorkbenchRailClasses::default();
    for class in classes {
        if class.starts_with("exact-c-") {
            rails.exact.push(class.clone());
        } else if class.starts_with("generated-c-") || class == "origin-generated" {
            rails.generated.push(class.clone());
        } else if class.starts_with("prepared-c-") {
            rails.prepared.push(class.clone());
        } else if class.starts_with("context-c-") || class == "origin-contextual" {
            rails.context.push(class.clone());
        } else if class.starts_with("structural-c-") || class == "origin-structural" {
            rails.boundary.push(class.clone());
        } else {
            rails.legacy.push(class.clone());
        }
    }
    rails
}

fn trace_workbench_is_component_class(class: &str) -> bool {
    class.starts_with("exact-c-")
        || class.starts_with("generated-c-")
        || class.starts_with("prepared-c-")
        || class.starts_with("context-c-")
        || class.starts_with("structural-c-")
}

fn trace_workbench_compact_instruction_text(instruction: &InstructionFact) -> String {
    let mnemonic = trace_workbench_compact_instruction_mnemonic(&instruction.mnemonic);
    match instruction.instruction.kind() {
        "bytecode.pc" => mnemonic,
        "runtime.stmt" | "runtime.terminator" => trace_workbench_compact_mir_text(&mnemonic),
        _ => format!("%{} = {}", instruction.index, mnemonic),
    }
}

fn trace_workbench_compact_instruction_mnemonic(text: &str) -> String {
    let text = text.trim();
    if let Some(rest) = text.strip_prefix("ir[")
        && let Some((_, mnemonic)) = rest.split_once("] ")
    {
        return mnemonic.trim().to_string();
    }
    text.to_string()
}

fn trace_workbench_compact_origin_label(
    key: &OriginExportKey,
    instruction: Option<&InstructionFact>,
) -> String {
    if let Some(instruction) = instruction
        && !matches!(key.kind(), "runtime.stmt" | "runtime.terminator")
    {
        return if key.kind() == "bytecode.pc" {
            trace_workbench_compact_pc_label(key)
        } else {
            format!("%{}", instruction.index)
        };
    }
    match key.kind() {
        "bytecode.pc" => trace_workbench_compact_pc_label(key),
        kind if kind.contains(".block") => trace_workbench_compact_block_label(key),
        "runtime.stmt" => trace_workbench_compact_runtime_stmt_label(key),
        "runtime.terminator" => trace_workbench_compact_runtime_terminator_label(key),
        kind if kind.starts_with("hir.") => trace_workbench_compact_hir_label(key),
        kind if kind.starts_with("sonatina.") => trace_workbench_compact_sonatina_label(key),
        _ => trace_workbench_compact_tail_label(key.local_key()),
    }
}

fn trace_workbench_compact_origin_meta(key: &OriginExportKey) -> String {
    match key.kind() {
        "runtime.stmt" => "MIR".to_string(),
        "runtime.terminator" => "MIR term".to_string(),
        "runtime.block" => "MIR block".to_string(),
        "runtime.function" => "MIR function".to_string(),
        "hir.expr" | "hir.stmt" => "HIR".to_string(),
        kind if kind.starts_with("sonatina.postopt.") => "Optimized Sonatina".to_string(),
        kind if kind.starts_with("sonatina.preopt.") => "Sonatina pre-opt".to_string(),
        kind if kind.starts_with("sonatina.evm.prepared.") => "EVM prepared".to_string(),
        "bytecode.pc" => "bytecode".to_string(),
        _ => key.kind().to_string(),
    }
}

fn trace_workbench_compact_origin_text(
    key: &OriginExportKey,
    index: &TraceWorkbenchProjectionIndex<'_>,
) -> String {
    if let Some(display_name) = index.display_names.get(key) {
        return display_name.clone();
    }
    if let Some(snippet) = index.source_snippets.get(key) {
        return match key.kind() {
            kind if kind.starts_with("hir.") => snippet.clone(),
            "runtime.stmt" | "runtime.terminator" | "runtime.block" => {
                format!("lowered from {snippet}")
            }
            _ => snippet.clone(),
        };
    }
    if let Some(span) = index.source_spans.get(key) {
        return format!("source span L{}:{}", span.start_line, span.start_column);
    }
    if key.kind().contains(".block") {
        trace_workbench_compact_block_label(key)
    } else if trace_workbench_origin_key_is_generated(key) {
        match key.kind() {
            "runtime.terminator" => "generated terminator".to_string(),
            "runtime.stmt" => "generated statement".to_string(),
            kind if kind.starts_with("hir.") => "generated HIR node".to_string(),
            kind if kind.starts_with("sonatina.evm.prepared.") => {
                "generated prepared code".to_string()
            }
            kind if kind.starts_with("sonatina.") => "generated Sonatina node".to_string(),
            _ => "generated compiler node".to_string(),
        }
    } else {
        trace_workbench_compact_origin_fallback_text(key)
    }
}

fn trace_workbench_compact_origin_fallback_text(key: &OriginExportKey) -> String {
    match key.kind() {
        "hir.expr" => "HIR expression".to_string(),
        "hir.stmt" => "HIR statement".to_string(),
        "runtime.function" => "MIR function".to_string(),
        "runtime.block" => "MIR block".to_string(),
        "runtime.stmt" => "MIR statement".to_string(),
        "runtime.terminator" => "MIR terminator".to_string(),
        kind if kind.starts_with("sonatina.postopt.") => "optimized instruction".to_string(),
        kind if kind.starts_with("sonatina.preopt.") => "pre-opt instruction".to_string(),
        kind if kind.starts_with("sonatina.evm.prepared.") => "prepared instruction".to_string(),
        "bytecode.pc" => "bytecode instruction".to_string(),
        _ => trace_workbench_compact_origin_meta(key),
    }
}

fn trace_workbench_compact_pc_label(key: &OriginExportKey) -> String {
    key.local_key()
        .strip_prefix("pc:")
        .and_then(|pc| pc.parse::<u32>().ok())
        .map(|pc| format!("{pc:04}"))
        .unwrap_or_else(|| trace_workbench_compact_tail_label(key.local_key()))
}

fn trace_workbench_compact_block_label(key: &OriginExportKey) -> String {
    if let Some(block) = key.local_key().strip_prefix("block:") {
        return format!("b{}", trace_workbench_compact_tail_label(block));
    }
    trace_workbench_compact_tail_label(key.local_key())
}

fn trace_workbench_compact_runtime_stmt_label(key: &OriginExportKey) -> String {
    if let Some(rest) = key.local_key().strip_prefix("block:") {
        let parts = rest.split(':').collect::<Vec<_>>();
        if parts.len() >= 3 && parts[1] == "stmt" {
            return format!("bb{}.s{}", parts[0], parts[2]);
        }
    }
    trace_workbench_compact_tail_label(key.local_key())
}

fn trace_workbench_compact_runtime_terminator_label(key: &OriginExportKey) -> String {
    if let Some(rest) = key.local_key().strip_prefix("block:")
        && let Some(block) = rest.strip_suffix(":terminator")
    {
        return format!("bb{block}.term");
    }
    trace_workbench_compact_tail_label(key.local_key())
}

fn trace_workbench_compact_hir_label(key: &OriginExportKey) -> String {
    let prefix = match key.kind() {
        "hir.expr" => "e",
        "hir.stmt" => "s",
        _ => "h",
    };
    format!(
        "{prefix}{}",
        trace_workbench_compact_tail_label(key.local_key())
    )
}

fn trace_workbench_compact_sonatina_label(key: &OriginExportKey) -> String {
    let local = key.local_key();
    if let Some(inst) = trace_workbench_extract_wrapped_id(local, "InstId(") {
        return format!("%{inst}");
    }
    if let Some(block) = trace_workbench_extract_wrapped_id(local, "BlockId(") {
        return format!("b{block}");
    }
    if let Some(func) = trace_workbench_extract_wrapped_id(local, "FuncRef(") {
        return format!("fn {func}");
    }
    trace_workbench_compact_tail_label(local)
}

fn trace_workbench_extract_wrapped_id(value: &str, marker: &str) -> Option<String> {
    let start = value.rfind(marker)? + marker.len();
    let id = value[start..].split(')').next()?;
    (!id.is_empty()).then(|| id.to_string())
}

fn trace_workbench_compact_tail_label(value: &str) -> String {
    value
        .rsplit([':', '$'])
        .next()
        .filter(|tail| !tail.is_empty())
        .unwrap_or(value)
        .replace("InstId(", "")
        .replace("FuncRef(", "")
        .replace("BlockId(", "")
        .replace(')', "")
        .to_string()
}

fn trace_workbench_compact_mir_text(text: &str) -> String {
    let normalized = trace_workbench_compact_instruction_mnemonic(text);
    let without_phantom = trace_workbench_strip_phantom_data(&normalized);
    let with_locals = trace_workbench_replace_wrapped_ids(&without_phantom, "RLocalId(", "%");
    let with_layouts = trace_workbench_replace_wrapped_ids(&with_locals, "LayoutId(Id(", "layout#");
    let with_runtime =
        trace_workbench_replace_wrapped_ids(&with_layouts, "RuntimeInstance(Id(", "runtime#");
    trace_workbench_compact_int_literals(&with_runtime)
        .replace("))", ")")
        .replace(")(", "(")
        .replace("),", ",")
        .replace("](", "]")
}

fn trace_workbench_strip_phantom_data(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(start) = rest.find(", PhantomData<") {
        out.push_str(&rest[..start]);
        let skip = &rest[start + 2..];
        let mut depth = 0usize;
        let mut end = skip.len();
        for (idx, ch) in skip.char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        end = idx + ch.len_utf8();
                        break;
                    }
                }
                _ => {}
            }
        }
        rest = &skip[end..];
    }
    out.push_str(rest);
    out
}

fn trace_workbench_replace_wrapped_ids(text: &str, marker: &str, prefix: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(start) = rest.find(marker) {
        out.push_str(&rest[..start]);
        let tail = &rest[start + marker.len()..];
        if let Some(end) = tail.find(')') {
            out.push_str(prefix);
            out.push_str(&tail[..end]);
            rest = &tail[end + 1..];
        } else {
            out.push_str(&rest[start..]);
            rest = "";
        }
    }
    out.push_str(rest);
    out
}

fn trace_workbench_compact_int_literals(text: &str) -> String {
    let mut out = text.to_string();
    let needle = "const Int { bits: ";
    while let Some(start) = out.find(needle) {
        let bits_start = start + needle.len();
        let Some(bits_end) = out[bits_start..]
            .find(", signed: ")
            .map(|idx| bits_start + idx)
        else {
            break;
        };
        let signed_start = bits_end + ", signed: ".len();
        let Some(signed_end) = out[signed_start..]
            .find(", words: [")
            .map(|idx| signed_start + idx)
        else {
            break;
        };
        let value_start = signed_end + ", words: [".len();
        let Some(value_end) = out[value_start..].find("] }").map(|idx| value_start + idx) else {
            break;
        };
        let bits = out[bits_start..bits_end].to_string();
        let signed = out[signed_start..signed_end].trim() == "true";
        let value = out[value_start..value_end].to_string();
        let prefix = if signed { "i" } else { "u" };
        let replacement = format!("const {prefix}{bits} {value}");
        out.replace_range(start..value_end + 3, &replacement);
    }
    out
}

fn trace_workbench_key_belongs_to_panel(key: &OriginExportKey, panel: &str) -> bool {
    match panel {
        "hir" => key.kind().starts_with("hir."),
        "mir" => key.kind().starts_with("runtime."),
        "sonatina-pre" => key.kind().starts_with("sonatina.preopt."),
        "sonatina-post" => key.kind().starts_with("sonatina.postopt."),
        "sonatina-prepared" => key.kind().starts_with("sonatina.evm.prepared."),
        "bytecode" => key.kind() == "bytecode.pc",
        _ => false,
    }
}

fn trace_workbench_origin_key_is_generated(key: &OriginExportKey) -> bool {
    key.owner_key().contains("__synthetic") || key.local_key().contains("__synthetic")
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceViewManifest {
    pub revision: u64,
    pub root_digest: String,
    pub summary_digest: String,
    pub metadata_digest: String,
    pub source_digest: String,
    pub indexes_digest: String,
    pub rail_components_digest: String,
    pub panes: BTreeMap<String, String>,
    pub reports: BTreeMap<String, String>,
}

pub fn trace_workbench_manifest(projection: &serde_json::Value) -> TraceViewManifest {
    let revision = projection
        .get("revision")
        .and_then(|revision| revision.get("id"))
        .and_then(serde_json::Value::as_u64)
        .unwrap_or_default();
    let metadata = projection
        .get("metadata")
        .cloned()
        .unwrap_or(serde_json::Value::Null);
    let source = projection
        .get("source")
        .cloned()
        .unwrap_or(serde_json::Value::Null);
    let indexes = projection
        .get("indexes")
        .cloned()
        .unwrap_or(serde_json::Value::Null);
    let summary = trace_workbench_summary_chunk(projection);
    let reports = [
        (
            "attribution".to_string(),
            projection.get("attribution_audit"),
        ),
        (
            "static_analysis".to_string(),
            projection.get("static_analysis"),
        ),
        ("closure_audit".to_string(), projection.get("audit")),
        (
            "duplicate_shapes".to_string(),
            projection.get("duplicate_shapes"),
        ),
    ]
    .into_iter()
    .filter_map(|(name, value)| value.map(|value| (name, digest_json(value))))
    .collect();
    let panes = projection
        .get("panels")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|pane| {
            let id = pane
                .get("id")
                .and_then(serde_json::Value::as_str)
                .map(str::to_string)?;
            Some((id, digest_json(pane)))
        })
        .collect();
    TraceViewManifest {
        revision,
        root_digest: digest_json(projection),
        summary_digest: digest_json(&summary),
        metadata_digest: digest_json(&metadata),
        source_digest: digest_json(&source),
        indexes_digest: digest_json(&indexes),
        rail_components_digest: digest_json(
            projection
                .get("rail_components")
                .unwrap_or(&serde_json::Value::Null),
        ),
        panes,
        reports,
    }
}

pub fn trace_workbench_summary_chunk(projection: &serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "revision": projection.get("revision").cloned().unwrap_or(serde_json::Value::Null),
        "metadata": projection.get("metadata").cloned().unwrap_or(serde_json::Value::Null),
        "provenance": projection.get("provenance").cloned().unwrap_or(serde_json::Value::Null),
        "counts": projection.get("counts").cloned().unwrap_or(serde_json::Value::Null),
        "salsa": projection.get("salsa").cloned().unwrap_or(serde_json::Value::Null),
        "bytecode_count": projection.get("bytecode_count").cloned().unwrap_or(serde_json::Value::Null),
        "selection_remap": projection.get("selection_remap").cloned().unwrap_or(serde_json::Value::Null),
        "notes": projection.get("notes").cloned().unwrap_or(serde_json::Value::Null),
    })
}

fn digest_json(value: &serde_json::Value) -> String {
    let bytes = serde_json::to_vec(value).unwrap_or_default();
    format!("blake3:{}", blake3::hash(&bytes).to_hex())
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopCostReport {
    pub metadata: ReportMetadata,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub loop_key: Option<OriginExportKey>,
    pub loop_label: Option<String>,
    pub summary: InstructionSummary,
    pub repeated_zero_extends: Vec<LocalInstructionGroup>,
    pub storage_impacts: Vec<StorageImpact>,
    pub findings: Vec<Insight>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopContentsReport {
    pub metadata: ReportMetadata,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub loop_key: Option<OriginExportKey>,
    pub loop_label: Option<String>,
    pub blocks: Vec<LoopBlockContents>,
    pub instructions: Vec<InstructionRow>,
    pub target_instructions: Vec<InstructionRow>,
    pub bytecode_bridge_available: bool,
    pub bytecode_origin_edges_available: bool,
    pub findings: Vec<Insight>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopBlockContents {
    pub block: OriginExportKey,
    pub role: String,
    pub instructions: Vec<InstructionRow>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainLocalReport {
    pub metadata: ReportMetadata,
    pub local: String,
    pub local_key: Option<OriginExportKey>,
    pub candidate_local_keys: Vec<OriginExportKey>,
    pub storage_history: Vec<StorageStep>,
    pub related_instructions: Vec<RelatedInstruction>,
    pub zero_extends: Vec<RelatedInstruction>,
    pub findings: Vec<Insight>,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub available_locals: Vec<String>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownReport {
    pub metadata: ReportMetadata,
    pub schedule: String,
    pub policy: String,
    pub available: bool,
    pub total_gas: Option<u64>,
    pub rows: Vec<GasBreakdownRow>,
    pub findings: Vec<Insight>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainPcReport {
    pub metadata: ReportMetadata,
    pub pc: u32,
    pub instruction: Option<InstructionRow>,
    pub primary_source: Option<SourceAttribution>,
    pub source_candidates: Vec<SourceAttribution>,
    pub category: Option<InstructionCategory>,
    pub static_gas: Option<u64>,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBySourceReport {
    pub metadata: ReportMetadata,
    pub schedule: String,
    pub policy: String,
    pub total_gas: u64,
    pub rows: Vec<GasBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DynamicGasBySourceReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub target_schedule: String,
    pub policy: String,
    pub total_gas: u64,
    pub unattributed_steps: usize,
    pub rows: Vec<GasBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytecodeSizeBySourceReport {
    pub metadata: ReportMetadata,
    pub policy: String,
    pub total_bytes: u64,
    pub rows: Vec<BytecodeSizeBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytecodeSizeBySourceRow {
    pub source: Option<OriginExportKey>,
    pub label: String,
    pub bytes: u64,
    pub instruction_count: usize,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasToSourceReport {
    pub metadata: ReportMetadata,
    pub schedule: String,
    pub trace_id: Option<String>,
    pub policy: String,
    pub static_gas: u64,
    pub dynamic_gas: u64,
    pub total_gas: u64,
    pub rows: Vec<GasToSourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasToSourceRow {
    pub source: Option<OriginExportKey>,
    pub label: String,
    pub static_gas: u64,
    pub dynamic_gas: u64,
    pub total_gas: u64,
    pub instruction_count: usize,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OptimizedCodeHonestyReport {
    pub metadata: ReportMetadata,
    pub schedule: String,
    pub policy: String,
    pub ambiguous_instructions: Vec<AmbiguousInstructionOriginRow>,
    pub synthetic_overheads: Vec<SyntheticOverheadRow>,
    pub unmapped_instructions: Vec<InstructionRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AmbiguousInstructionOriginRow {
    pub instruction: InstructionRow,
    pub source_candidates: Vec<SourceAttribution>,
    pub static_gas: Option<u64>,
    pub dynamic_gas: u64,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SyntheticOverheadRow {
    pub instruction: InstructionRow,
    pub cause_sources: Vec<SourceAttribution>,
    pub edge_labels: Vec<OriginEdgeLabel>,
    pub static_gas: Option<u64>,
    pub dynamic_gas: u64,
    pub confidence: Confidence,
}

const MISSING_LINK_AUDIT_SCHEMA_VERSION: &str = "missing_link_audit_v0";
const MISSING_LINK_QUERY_PACK: &str = "argot_static_checks_v0";
const TRACE_SCHEMA_VERSION: &str = "trace_v0";
const EDGE_SEMANTICS_VERSION: &str = "origin_edge_semantics_v0";
const MISSING_LINK_DETAIL_LIMIT: usize = 100;
const MISSING_LINK_CLUSTER_SAMPLE_LIMIT: usize = 5;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditReport {
    pub metadata: ReportMetadata,
    pub total_bytecode_pcs: usize,
    pub source_exact_pcs: usize,
    pub source_ambiguous_pcs: usize,
    pub unmapped_pcs: usize,
    pub optimized_sonatina_linked_pcs: usize,
    pub prepared_linked_pcs: usize,
    pub missing_optimized_to_prepared_lineage_pcs: usize,
    pub direct_bytecode_edges: Vec<AttributionAuditEdgeCount>,
    pub direct_edges_by_class: Vec<AttributionAuditClassCount>,
    pub source_lines: Vec<AttributionAuditSourceLineCount>,
    pub sonatina_targets: Vec<AttributionAuditTargetCount>,
    pub optimized_sonatina_targets: Vec<AttributionAuditTargetCount>,
    pub prepared_targets: Vec<AttributionAuditTargetCount>,
    pub missing_lineage_targets: Vec<AttributionAuditTargetCount>,
    pub lineage_gaps: Vec<AttributionAuditLineageGap>,
    pub suspicious_edges: Vec<AttributionAuditSuspiciousEdge>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub missing_links: Option<MissingLinkAuditReport>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditEdgeCount {
    pub label: OriginEdgeLabel,
    pub to_kind: String,
    pub introduced_by: Option<CompilerPhase>,
    pub traversal_class: OriginEdgeTraversalClass,
    pub count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditClassCount {
    pub traversal_class: OriginEdgeTraversalClass,
    pub count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditSourceLineCount {
    pub label: String,
    pub origin_kind: String,
    pub count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditTargetCount {
    pub target: OriginExportKey,
    pub count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditLineageGap {
    pub bytecode_pc: OriginExportKey,
    pub prepared_origin: OriginExportKey,
    pub reason: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AttributionAuditSuspiciousEdge {
    pub from: OriginExportKey,
    pub to: OriginExportKey,
    pub label: OriginEdgeLabel,
    pub introduced_by: Option<CompilerPhase>,
    pub traversal_class: OriginEdgeTraversalClass,
    pub reason: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MissingLinkAuditReport {
    pub schema_version: String,
    pub query_pack: String,
    pub artifact: MissingLinkAuditArtifact,
    pub summary: MissingLinkSummary,
    pub boundary_summaries: Vec<LinkBoundarySummary>,
    pub clusters: Vec<LinkGapCluster>,
    pub gaps: Vec<LinkGap>,
    pub expected_absent: Vec<ExpectedAbsentLink>,
    pub invalid: Vec<InvalidAttributionLink>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MissingLinkAuditArtifact {
    pub source_uri: String,
    pub compiler_commit: String,
    pub sonatina_commit: Option<String>,
    pub opt_level: Option<String>,
    pub target: String,
    pub trace_schema_version: String,
    pub relation_schema_version: Option<String>,
    pub edge_semantics_version: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MissingLinkSummary {
    pub status: LinkOverallStatus,
    pub top_blockers: Vec<LinkBoundaryKind>,
    pub exact_source_to_bytecode_pcs: usize,
    pub prepared_linked_bytecode_pcs: usize,
    pub unmapped_bytecode_pcs: usize,
    pub missing_required_count: usize,
    pub invalid_count: usize,
    pub expected_absent_count: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkOverallStatus {
    Pass,
    PassWithExpectedAbsences,
    Warning,
    Invalid,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinkBoundarySummary {
    pub boundary: LinkBoundaryKind,
    pub owner_phase: CompilerPhase,
    pub status_counts: BTreeMap<LinkStatus, usize>,
    pub affected_origins: usize,
    pub affected_bytecode_pcs: usize,
    pub top_issue_codes: Vec<LinkIssueCode>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkBoundaryKind {
    HirToMir,
    MirToSonatinaPreOpt,
    PreOptToPostOpt,
    PostOptToPrepared,
    PreparedToBytecode,
    BytecodeToRuntime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkStatus {
    SatisfiedExact,
    SatisfiedGenerated,
    SatisfiedElided,
    ContextOnly,
    CandidateOnly,
    ExpectedAbsent,
    MissingRequired,
    MissingOptional,
    MissingLineageButCandidatesExist,
    Ambiguous,
    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkSeverity {
    Info,
    Notice,
    Warning,
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkIssueCode {
    ExpectedAbsentSyntax,
    ExpectedAbsentDeclaration,
    ExpectedAbsentNoRuntimeSession,
    GeneratedExplanationOnly,
    ContextOnlyEvidence,
    CandidateShapeMatchOnly,
    MissingHirToMirLowering,
    MissingMirToPreoptLowering,
    MissingOptimizerLineage,
    MissingOptimizedToPreparedLineage,
    MissingPreparedPcExtent,
    MissingRuntimeJoin,
    ElidedWithoutReason,
    AmbiguousLineageCandidates,
    InvalidCrossRepresentationJoin,
    PreparedKeyedAsPostopt,
    PcMapInstNotInClaimedRepresentation,
    RawLocalIdJoinedAcrossSnapshots,
    DebugContextUsedAsExact,
    ShapeMatchUsedAsProvenance,
    StructuralEdgeUsedForAttribution,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinkGapCluster {
    pub cluster_id: String,
    pub boundary: LinkBoundaryKind,
    pub status: LinkStatus,
    pub issue_code: LinkIssueCode,
    pub severity: LinkSeverity,
    pub owner_phase: CompilerPhase,
    pub headline: String,
    pub explanation: String,
    pub affected_origins: Vec<AttributionAuditTargetCount>,
    pub affected_bytecode_pcs: Vec<OriginExportKey>,
    pub gap_count: usize,
    pub sample_gap_ids: Vec<String>,
    pub candidate_hints: Vec<CandidateHint>,
    pub required_evidence: Vec<RequiredEvidence>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinkGap {
    pub gap_id: String,
    pub boundary: LinkBoundaryKind,
    pub status: LinkStatus,
    pub issue_code: LinkIssueCode,
    pub severity: LinkSeverity,
    pub from_origin: OriginExportKey,
    pub from_representation: Option<String>,
    pub expected_to_phase: String,
    pub reached_frontier: String,
    pub candidate_hints: Vec<CandidateHint>,
    pub required_evidence: Vec<RequiredEvidence>,
    pub cluster_id: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpectedAbsentLink {
    pub boundary: LinkBoundaryKind,
    pub origin: OriginExportKey,
    pub issue_code: LinkIssueCode,
    pub explanation: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InvalidAttributionLink {
    pub boundary: LinkBoundaryKind,
    pub origin: OriginExportKey,
    pub issue_code: LinkIssueCode,
    pub explanation: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CandidateHint {
    pub kind: CandidateHintKind,
    pub confidence: CandidateConfidence,
    pub from: Option<OriginExportKey>,
    pub to: Option<OriginExportKey>,
    pub explanation: String,
    pub dimensions: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CandidateHintKind {
    SameRawLocalId,
    SameDebugFrontendOrigin,
    SameSourceSpan,
    SameShapeDigest,
    SimilarShapeDigest,
    SameBlockNeighborhood,
    SameOpcodeCategoryPattern,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CandidateConfidence {
    Weak,
    Medium,
    Strong,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RequiredEvidence {
    pub kind: RequiredEvidenceKind,
    pub owner_phase: CompilerPhase,
    pub description: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RequiredEvidenceKind {
    ExactOriginEdge,
    PreparedLineageFact,
    OptimizerLineageFact,
    PcExtentFact,
    RuntimeCodeHashJoin,
    ElisionReason,
    RepresentationIdentity,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariablesAtPcReport {
    pub metadata: ReportMetadata,
    pub pc: u32,
    pub code_object: Option<OriginExportKey>,
    pub variables: Vec<VariableAtPcRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeEvidenceSummary {
    pub session_count: usize,
    pub runtime_sources: Vec<String>,
    pub value_policies: Vec<String>,
    pub exact_join_steps: usize,
    pub pc_only_join_steps: usize,
    pub ambiguous_join_steps: usize,
    pub missing_join_steps: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeGasBySourceReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub total_gas: u64,
    pub unattributed_steps: usize,
    pub rows: Vec<GasBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageWritesBySourceReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub total_writes: usize,
    pub total_gas: u64,
    pub rows: Vec<StorageWriteBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageWriteBySourceRow {
    pub source: Option<OriginExportKey>,
    pub label: String,
    pub writes: usize,
    pub gas: u64,
    pub slots: Vec<String>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageAccessesBySlotReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub slot_filter: Option<String>,
    pub total_reads: usize,
    pub total_writes: usize,
    pub rows: Vec<StorageAccessBySlotRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageAccessBySlotRow {
    pub slot: String,
    pub reads: usize,
    pub writes: usize,
    pub gas: u64,
    pub sources: Vec<SourceAttribution>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CallCostByCallsiteReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub total_calls: usize,
    pub total_gas_used: u64,
    pub rows: Vec<CallCostByCallsiteRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CallCostByCallsiteRow {
    pub call: OriginExportKey,
    pub callsite_instruction: Option<OriginExportKey>,
    pub source: Option<SourceAttribution>,
    pub kind: String,
    pub callee: Option<String>,
    pub gas_requested: Option<u64>,
    pub gas_used: Option<u64>,
    pub success: Option<bool>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryGrowthBySourceReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub total_accesses: usize,
    pub total_bytes_touched: u64,
    pub max_end_offset: u64,
    pub rows: Vec<MemoryGrowthBySourceRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryGrowthBySourceRow {
    pub source: Option<OriginExportKey>,
    pub label: String,
    pub accesses: usize,
    pub bytes_touched: u64,
    pub max_end_offset: u64,
    pub gas: u64,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RevertAttributionReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub total_reverts: usize,
    pub rows: Vec<RevertAttributionRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RevertAttributionRow {
    pub revert: OriginExportKey,
    pub step: OriginExportKey,
    pub instruction: Option<OriginExportKey>,
    pub source: Option<SourceAttribution>,
    pub reason: Option<String>,
    pub data: String,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct HotPathByIterationReport {
    pub metadata: ReportMetadata,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub policy: String,
    pub loop_key: Option<OriginExportKey>,
    pub scope: String,
    pub total_steps: usize,
    pub total_gas: u64,
    pub rows: Vec<HotPathRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct HotPathRow {
    pub pc: u32,
    pub opcode: String,
    pub instruction: Option<OriginExportKey>,
    pub executions: usize,
    pub gas: u64,
    pub source: Option<SourceAttribution>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValueFlowAtPcReport {
    pub metadata: ReportMetadata,
    pub pc: u32,
    pub code_object: Option<OriginExportKey>,
    pub trace_id: Option<String>,
    pub runtime: RuntimeEvidenceSummary,
    pub rows: Vec<ValueFlowAtPcRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValueFlowAtPcRow {
    pub step: OriginExportKey,
    pub instruction: Option<OriginExportKey>,
    pub opcode: String,
    pub gas_cost: u64,
    pub stack_top: Vec<String>,
    pub storage_accesses: Vec<RuntimeValueAccessRow>,
    pub memory_accesses: Vec<RuntimeValueAccessRow>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeValueAccessRow {
    pub event: OriginExportKey,
    pub kind: String,
    pub location: String,
    pub value_before: Option<String>,
    pub value_after: Option<String>,
    pub value: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReportMetadata {
    pub trace_hash: String,
    pub data_source: String,
    pub target: String,
    pub input_path: String,
    pub compiler_commit: String,
    pub flags: Vec<String>,
}

impl ReportMetadata {
    fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        let metadata = snapshot.metadata();
        Self {
            trace_hash: snapshot.trace_hash().to_string(),
            data_source: data_source_label(metadata),
            target: metadata.target.clone(),
            input_path: metadata.input_path.clone(),
            compiler_commit: metadata.compiler_commit.clone(),
            flags: metadata.flags.clone(),
        }
    }

    pub fn function_label(&self) -> Option<&str> {
        self.flags
            .iter()
            .find_map(|flag| flag.strip_prefix("function="))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstructionSummary {
    pub total_instructions: usize,
    pub loads: usize,
    pub stores: usize,
    pub zero_extends: usize,
    pub stack_loads: usize,
    pub stack_stores: usize,
    pub moves: usize,
    pub branches: usize,
    pub jumps: usize,
    pub arithmetic: usize,
}

impl InstructionSummary {
    pub fn branch_like(&self) -> usize {
        self.branches + self.jumps
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalInstructionGroup {
    pub local: String,
    pub instructions: Vec<InstructionRow>,
    pub reason: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageImpact {
    pub local: String,
    pub storage_history: Vec<StorageStep>,
    pub loads: usize,
    pub stores: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageStep {
    pub phase: String,
    pub location: String,
    pub reason: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelatedInstruction {
    pub instruction: InstructionRow,
    pub edge_label: OriginEdgeLabel,
    pub reason: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstructionRow {
    pub key: OriginExportKey,
    pub index: u32,
    pub mnemonic: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownRow {
    pub subject: OriginExportKey,
    pub gas: u64,
    pub label: String,
    pub confidence: String,
    pub source: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceAttribution {
    pub origin: OriginExportKey,
    pub file: OriginExportKey,
    pub label: String,
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBySourceRow {
    pub source: Option<OriginExportKey>,
    pub label: String,
    pub gas: u64,
    pub instruction_count: usize,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariableAtPcRow {
    pub variable: OriginExportKey,
    pub name: String,
    pub location: String,
    pub reason: String,
    pub confidence: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Confidence {
    Exact,
    High,
    Medium,
    Low,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Insight {
    pub severity: InsightSeverity,
    pub title: String,
    pub summary: String,
}

impl Insight {
    pub fn info(title: impl Into<String>, summary: impl Into<String>) -> Self {
        Self {
            severity: InsightSeverity::Info,
            title: title.into(),
            summary: summary.into(),
        }
    }

    pub fn hint(title: impl Into<String>, summary: impl Into<String>) -> Self {
        Self {
            severity: InsightSeverity::Hint,
            title: title.into(),
            summary: summary.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InsightSeverity {
    Info,
    Hint,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryError {
    InvalidRequest(String),
}

impl fmt::Display for QueryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRequest(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for QueryError {}

fn explain_local_unavailable(
    snapshot: &TraceSnapshot,
    local: String,
    reason: Option<String>,
    available_locals: Vec<String>,
    candidate_local_keys: Vec<OriginExportKey>,
) -> ExplainLocalReport {
    ExplainLocalReport {
        metadata: ReportMetadata::from_snapshot(snapshot),
        local,
        local_key: None,
        candidate_local_keys,
        storage_history: Vec::new(),
        related_instructions: Vec::new(),
        zero_extends: Vec::new(),
        findings: vec![Insight::info(
            "Local explanation unavailable",
            "compiler-derived local identity must be selected unambiguously before storage or instruction facts are queried",
        )],
        available: false,
        unavailable_reason: reason,
        available_locals,
        confidence: Confidence::Unknown,
    }
}

fn reject_call_policy(policy: GasAttributionPolicy) -> QueryResult<()> {
    if matches!(
        policy,
        GasAttributionPolicy::CallInclusive | GasAttributionPolicy::CallExclusive
    ) {
        return Err(QueryError::InvalidRequest(format!(
            "{policy} attribution requires call graph and inline context facts, which are not emitted yet"
        )));
    }
    Ok(())
}

struct TraceIndex<'a> {
    snapshot: &'a TraceSnapshot,
    loop_key: Option<OriginExportKey>,
    loop_phases: BTreeMap<OriginExportKey, CompilerPhase>,
    loop_members: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>>,
    loop_blocks: BTreeMap<OriginExportKey, Vec<(OriginExportKey, LoopBlockRole)>>,
    locals: BTreeMap<String, Vec<OriginExportKey>>,
    local_keys: BTreeSet<OriginExportKey>,
    display_names: BTreeMap<OriginExportKey, String>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    instruction_blocks: BTreeMap<OriginExportKey, OriginExportKey>,
    instruction_extents: BTreeMap<OriginExportKey, &'a trace_facts::InstructionExtentFact>,
    function_code_objects: BTreeMap<OriginExportKey, OriginExportKey>,
}

impl<'a> TraceIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut first_loop_key = None;
        let mut loop_phases = BTreeMap::new();
        let mut loop_members: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>> =
            BTreeMap::new();
        let mut loop_blocks: BTreeMap<OriginExportKey, Vec<(OriginExportKey, LoopBlockRole)>> =
            BTreeMap::new();
        let mut locals: BTreeMap<String, Vec<OriginExportKey>> = BTreeMap::new();
        let mut local_keys = BTreeSet::new();
        let mut display_names = BTreeMap::new();
        let mut instructions = BTreeMap::new();
        let mut instruction_blocks = BTreeMap::new();
        let mut instruction_extents = BTreeMap::new();
        let mut function_code_objects = BTreeMap::new();

        for fact in snapshot.facts() {
            if let TraceFact::DisplayName(display_name) = fact {
                display_names.insert(display_name.subject.clone(), display_name.name.clone());
            }
        }

        for fact in snapshot.facts() {
            match fact {
                TraceFact::Loop(loop_fact) => {
                    loop_phases.insert(loop_fact.loop_key.clone(), loop_fact.phase);
                }
                TraceFact::LoopMembership(membership) => {
                    first_loop_key.get_or_insert_with(|| membership.loop_key.clone());
                    loop_members
                        .entry(membership.loop_key.clone())
                        .or_default()
                        .insert(membership.instruction.clone());
                }
                TraceFact::LoopBlock(loop_block) => {
                    loop_blocks
                        .entry(loop_block.loop_key.clone())
                        .or_default()
                        .push((loop_block.block.clone(), loop_block.role));
                }
                TraceFact::OriginNode(node) if node.key.kind() == "runtime.local" => {
                    local_keys.insert(node.key.clone());
                    let name = display_names
                        .get(&node.key)
                        .cloned()
                        .unwrap_or_else(|| local_display_name(&node.key));
                    locals.entry(name).or_default().push(node.key.clone());
                }
                TraceFact::Instruction(instruction) => {
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::InstructionBlock(instruction_block) => {
                    instruction_blocks.insert(
                        instruction_block.instruction.clone(),
                        instruction_block.block.clone(),
                    );
                }
                TraceFact::InstructionExtent(extent) => {
                    instruction_extents.insert(extent.instruction.clone(), extent);
                }
                TraceFact::Function(function) => {
                    if let Some(code_object) = &function.code_object {
                        function_code_objects
                            .insert(function.function.clone(), code_object.clone());
                    }
                }
                _ => {}
            }
        }

        let loop_key = preferred_loop_key(&loop_members, &loop_phases).or(first_loop_key);
        Self {
            snapshot,
            loop_key,
            loop_phases,
            loop_members,
            loop_blocks,
            locals,
            local_keys,
            display_names,
            instructions,
            instruction_blocks,
            instruction_extents,
            function_code_objects,
        }
    }

    fn loop_phase(&self, loop_key: &OriginExportKey) -> Option<CompilerPhase> {
        self.loop_phases.get(loop_key).copied()
    }

    fn active_loop_instructions(&self) -> BTreeSet<OriginExportKey> {
        self.loop_key
            .as_ref()
            .and_then(|key| self.loop_members.get(key))
            .cloned()
            .unwrap_or_default()
    }

    fn all_instruction_keys(&self) -> BTreeSet<OriginExportKey> {
        self.instructions.keys().cloned().collect()
    }

    fn local_candidates(&self, query: &str) -> Vec<OriginExportKey> {
        if let Some(candidates) = self.locals.get(query) {
            return candidates.clone();
        }
        self.local_keys
            .iter()
            .filter(|key| key.display_label() == query || key.canonical_storage_key() == query)
            .cloned()
            .collect()
    }

    fn local_choices(&self) -> Vec<String> {
        let mut choices = Vec::new();
        for (name, keys) in &self.locals {
            for key in keys {
                choices.push(format!("{name} => {}", key.display_label()));
            }
        }
        choices.into_iter().take(20).collect()
    }

    fn sorted_instruction_rows(&self, keys: &BTreeSet<OriginExportKey>) -> Vec<InstructionRow> {
        let mut rows = keys
            .iter()
            .filter_map(|key| self.instruction_row(key))
            .collect::<Vec<_>>();
        rows.sort_by(|a, b| {
            a.index
                .cmp(&b.index)
                .then_with(|| a.key.display_label().cmp(&b.key.display_label()))
        });
        rows
    }

    fn loop_block_contents(
        &self,
        loop_key: &OriginExportKey,
        instructions: &BTreeSet<OriginExportKey>,
    ) -> Vec<LoopBlockContents> {
        let mut instructions_by_block =
            BTreeMap::<OriginExportKey, BTreeSet<OriginExportKey>>::new();
        for instruction in instructions {
            if let Some(block) = self.instruction_blocks.get(instruction) {
                instructions_by_block
                    .entry(block.clone())
                    .or_default()
                    .insert(instruction.clone());
            }
        }

        let mut rows = Vec::new();
        if let Some(blocks) = self.loop_blocks.get(loop_key) {
            for (block, role) in blocks {
                rows.push(LoopBlockContents {
                    block: block.clone(),
                    role: loop_block_role_label(*role).to_string(),
                    instructions: instructions_by_block
                        .remove(block)
                        .map(|keys| self.sorted_instruction_rows(&keys))
                        .unwrap_or_default(),
                });
            }
        }
        for (block, keys) in instructions_by_block {
            rows.push(LoopBlockContents {
                block,
                role: "unknown".to_string(),
                instructions: self.sorted_instruction_rows(&keys),
            });
        }
        rows
    }

    fn bytecode_instructions_for_loop_members(
        &self,
        loop_members: &BTreeSet<OriginExportKey>,
    ) -> BTreeSet<OriginExportKey> {
        let mut loop_member_origins = BTreeSet::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if loop_members.contains(&edge.from)
                && trace_index::TraceReachabilityPolicy::ExactOnly.allows_edge(edge)
            {
                loop_member_origins.insert(edge.to.clone());
            }
        }

        let mut bytecode = BTreeSet::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if edge.from.kind() != "bytecode.pc" {
                continue;
            }
            if trace_index::TraceReachabilityPolicy::ExactOnly.allows_edge(edge)
                && loop_members.contains(&edge.to)
            {
                bytecode.insert(edge.from.clone());
            }
            if trace_index::TraceReachabilityPolicy::ExactOnly.allows_edge(edge)
                && loop_member_origins.contains(&edge.to)
            {
                bytecode.insert(edge.from.clone());
            }
        }
        bytecode
    }

    fn has_bytecode_origin_edges(&self) -> bool {
        self.snapshot.facts().iter().any(|fact| {
            matches!(
                fact,
                TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && edge.to.kind() != "code.object"
                        && trace_index::TraceReachabilityPolicy::ExactOnly.allows_edge(edge)
            )
        })
    }

    fn instruction_row(&self, key: &OriginExportKey) -> Option<InstructionRow> {
        self.instructions
            .get(key)
            .map(|instruction| InstructionRow {
                key: instruction.instruction.clone(),
                index: instruction.index,
                mnemonic: instruction.mnemonic.clone(),
            })
    }

    fn category_counts(&self, instructions: &BTreeSet<OriginExportKey>) -> InstructionSummary {
        let mut counts = InstructionSummary {
            total_instructions: instructions.len(),
            ..Default::default()
        };
        for fact in self.snapshot.facts() {
            let TraceFact::InstructionCategory(category) = fact else {
                continue;
            };
            if !instructions.contains(&category.instruction) {
                continue;
            }
            match category.category {
                InstructionCategory::Load => counts.loads += 1,
                InstructionCategory::Store => counts.stores += 1,
                InstructionCategory::ZeroExtend => counts.zero_extends += 1,
                InstructionCategory::StackLoad => counts.stack_loads += 1,
                InstructionCategory::StackStore => counts.stack_stores += 1,
                InstructionCategory::Move => counts.moves += 1,
                InstructionCategory::Branch => counts.branches += 1,
                InstructionCategory::Jump => counts.jumps += 1,
                InstructionCategory::Arithmetic => counts.arithmetic += 1,
                _ => {}
            }
        }
        counts
    }

    fn zero_extends_by_local(
        &self,
        instructions: &BTreeSet<OriginExportKey>,
    ) -> Vec<LocalInstructionGroup> {
        let mut groups: BTreeMap<String, Vec<InstructionRow>> = BTreeMap::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if edge.label != OriginEdgeLabel::IntegerLegalizationFor
                || !instructions.contains(&edge.from)
            {
                continue;
            }
            if let Some(row) = self.instruction_row(&edge.from) {
                groups.entry(self.label(&edge.to)).or_default().push(row);
            }
        }
        groups
            .into_iter()
            .map(|(local, instructions)| {
                let reason = instructions
                    .first()
                    .and_then(|row| self.compiler_event_reason_for_output(&row.key));
                LocalInstructionGroup {
                    local,
                    instructions,
                    reason,
                }
            })
            .collect()
    }

    fn storage_impacts(&self, instructions: &BTreeSet<OriginExportKey>) -> Vec<StorageImpact> {
        let mut edge_counts: BTreeMap<OriginExportKey, (usize, usize)> = BTreeMap::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if !instructions.contains(&edge.from) {
                continue;
            }
            match edge.label {
                OriginEdgeLabel::LoadOf => edge_counts.entry(edge.to.clone()).or_default().0 += 1,
                OriginEdgeLabel::StoreOf => edge_counts.entry(edge.to.clone()).or_default().1 += 1,
                _ => {}
            }
        }

        edge_counts
            .into_iter()
            .map(|(local_key, (loads, stores))| StorageImpact {
                local: self.label(&local_key),
                storage_history: self.storage_for(&local_key),
                loads,
                stores,
            })
            .collect()
    }

    fn related_instruction_edges(
        &self,
        local_key: &OriginExportKey,
        instructions: &BTreeSet<OriginExportKey>,
    ) -> Vec<RelatedInstruction> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::OriginEdge(edge)
                    if &edge.to == local_key && instructions.contains(&edge.from) =>
                {
                    self.instruction_row(&edge.from)
                        .map(|instruction| RelatedInstruction {
                            reason: self.compiler_event_reason_for_output(&edge.from),
                            instruction,
                            edge_label: edge.label,
                        })
                }
                _ => None,
            })
            .collect()
    }

    fn storage_for(&self, key: &OriginExportKey) -> Vec<StorageStep> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Storage(storage) if &storage.subject == key => {
                    Some(storage_step(storage))
                }
                _ => None,
            })
            .collect()
    }

    fn static_gas_rows(&self, schedule: &str) -> Vec<GasBreakdownRow> {
        let mut rows = BTreeMap::new();
        for fact in self.snapshot.facts() {
            match fact {
                TraceFact::StaticGas(gas) if gas.schedule.as_str() == schedule => {
                    rows.insert(
                        gas.instruction.clone(),
                        GasBreakdownRow {
                            subject: gas.instruction.clone(),
                            gas: gas.base_cost,
                            label: self
                                .instruction_row(&gas.instruction)
                                .map(|row| format!("pc[{}] {}", row.index, row.mnemonic))
                                .unwrap_or_else(|| self.label(&gas.instruction)),
                            confidence: format!("{:?}", gas.confidence),
                            source: "StaticGasFact".to_string(),
                        },
                    );
                }
                TraceFact::GasCost(gas)
                    if gas.schedule.as_str() == schedule
                        && gas.gas_kind == GasKind::OpcodeStatic =>
                {
                    rows.entry(gas.subject.clone())
                        .or_insert_with(|| GasBreakdownRow {
                            subject: gas.subject.clone(),
                            gas: gas.gas,
                            label: self
                                .instruction_row(&gas.subject)
                                .map(|row| format!("pc[{}] {}", row.index, row.mnemonic))
                                .unwrap_or_else(|| self.label(&gas.subject)),
                            confidence: format!("{:?}", gas.confidence),
                            source: format!("{:?}", gas.source),
                        });
                }
                _ => {}
            }
        }
        rows.into_values().collect()
    }

    fn static_gas_for_instruction(
        &self,
        instruction: &OriginExportKey,
        schedule: &str,
    ) -> Option<u64> {
        self.static_gas_rows(schedule)
            .into_iter()
            .find(|row| &row.subject == instruction)
            .map(|row| row.gas)
    }

    fn category_for_instruction(
        &self,
        instruction: &OriginExportKey,
    ) -> Option<InstructionCategory> {
        self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::InstructionCategory(category) if &category.instruction == instruction => {
                Some(category.category)
            }
            _ => None,
        })
    }

    fn instruction_at_pc(&self, pc: u32) -> Option<InstructionRow> {
        let exact_pc = format!("pc:{pc}");
        self.instructions
            .values()
            .find(|instruction| {
                instruction.instruction.kind() == "bytecode.pc"
                    && instruction.instruction.local_key() == exact_pc
            })
            .map(|instruction| InstructionRow {
                key: instruction.instruction.clone(),
                index: instruction.index,
                mnemonic: instruction.mnemonic.clone(),
            })
    }

    fn instruction_at_pc_in_code_object(
        &self,
        pc: u32,
        code_object: &OriginExportKey,
    ) -> Option<InstructionRow> {
        let exact_pc = format!("pc:{pc}");
        self.instructions
            .values()
            .find(|instruction| {
                instruction.instruction.kind() == "bytecode.pc"
                    && instruction.instruction.local_key() == exact_pc
                    && self
                        .function_code_objects
                        .get(&instruction.function)
                        .is_none_or(|candidate| candidate == code_object)
            })
            .map(|instruction| InstructionRow {
                key: instruction.instruction.clone(),
                index: instruction.index,
                mnemonic: instruction.mnemonic.clone(),
            })
    }

    fn dynamic_gas_steps(
        &self,
        trace_id: Option<&str>,
    ) -> Vec<&'a trace_facts::DynamicGasStepFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::DynamicGasStep(step)
                    if trace_id.is_none_or(|trace_id| trace_id == step.trace_id) =>
                {
                    Some(step)
                }
                _ => None,
            })
            .collect()
    }

    fn instruction_for_dynamic_step(
        &self,
        step: &trace_facts::DynamicGasStepFact,
    ) -> Option<InstructionRow> {
        step.instruction
            .as_ref()
            .and_then(|instruction| self.instruction_row(instruction))
            .or_else(|| self.instruction_at_pc_in_code_object(step.pc, &step.code_object))
            .or_else(|| self.instruction_at_pc(step.pc))
    }

    fn gas_attribution_buckets(
        &self,
        instruction: &OriginExportKey,
        policy: GasAttributionPolicy,
    ) -> Vec<GasAttributionBucket> {
        let sources = self.source_candidates_for_instruction(instruction);
        if sources.is_empty() {
            return vec![GasAttributionBucket::unmapped()];
        }
        match policy {
            GasAttributionPolicy::Inclusive | GasAttributionPolicy::CallInclusive => sources
                .into_iter()
                .map(|source| GasAttributionBucket::source(source, policy, false))
                .collect(),
            GasAttributionPolicy::SyntheticOverhead if self.synthetic_edge_labels(instruction) => {
                if sources.len() == 1 {
                    vec![GasAttributionBucket::source(
                        sources[0].clone(),
                        policy,
                        true,
                    )]
                } else {
                    vec![GasAttributionBucket {
                        key: "<synthetic-overhead:ambiguous>".to_string(),
                        source: None,
                        label: "<synthetic-overhead:ambiguous>".to_string(),
                        confidence: Confidence::Low,
                    }]
                }
            }
            GasAttributionPolicy::ExclusivePrimary
            | GasAttributionPolicy::RuntimeStepExclusive
            | GasAttributionPolicy::SyntheticOverhead
            | GasAttributionPolicy::CallExclusive => {
                if sources.len() == 1 {
                    vec![GasAttributionBucket::source(
                        sources[0].clone(),
                        policy,
                        false,
                    )]
                } else {
                    vec![GasAttributionBucket {
                        key: "<ambiguous>".to_string(),
                        source: None,
                        label: "<ambiguous>".to_string(),
                        confidence: Confidence::Low,
                    }]
                }
            }
        }
    }

    fn synthetic_edge_labels(&self, instruction: &OriginExportKey) -> bool {
        self.snapshot.facts().iter().any(|fact| match fact {
            TraceFact::OriginEdge(edge) if &edge.from == instruction => matches!(
                edge.label,
                OriginEdgeLabel::SyntheticFor
                    | OriginEdgeLabel::BackendPrepared
                    | OriginEdgeLabel::Unmapped
            ),
            _ => false,
        })
    }

    fn synthetic_edges_from(
        &self,
        instruction: &OriginExportKey,
    ) -> Vec<&'a trace_facts::OriginEdgeFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::OriginEdge(edge)
                    if &edge.from == instruction
                        && matches!(
                            edge.label,
                            OriginEdgeLabel::SyntheticFor
                                | OriginEdgeLabel::BackendPrepared
                                | OriginEdgeLabel::Unmapped
                        ) =>
                {
                    Some(edge)
                }
                _ => None,
            })
            .collect()
    }

    fn dynamic_gas_for_instruction(&self, instruction: &OriginExportKey) -> u64 {
        self.dynamic_gas_steps(None)
            .into_iter()
            .filter(|step| {
                step.instruction.as_ref() == Some(instruction)
                    || step.instruction.is_none()
                        && self
                            .instruction_for_dynamic_step(step)
                            .is_some_and(|row| &row.key == instruction)
            })
            .map(|step| step.gas_cost)
            .sum()
    }

    fn source_candidates_for_instruction(
        &self,
        instruction: &OriginExportKey,
    ) -> Vec<SourceAttribution> {
        let mut candidates = BTreeSet::new();
        if self.source_attribution(instruction).is_some() {
            candidates.insert(instruction.clone());
        }

        let mut seen = BTreeSet::new();
        let mut stack = vec![instruction.clone()];
        while let Some(current) = stack.pop() {
            if !seen.insert(current.clone()) {
                continue;
            }
            for edge in self.snapshot.facts().iter().filter_map(|fact| match fact {
                TraceFact::OriginEdge(edge)
                    if edge.from == current
                        && trace_index::TraceReachabilityPolicy::ExactOnly.allows_edge(edge) =>
                {
                    Some(edge)
                }
                _ => None,
            }) {
                if self.source_attribution(&edge.to).is_some() {
                    candidates.insert(edge.to.clone());
                }
                stack.push(edge.to.clone());
            }
        }
        candidates
            .into_iter()
            .filter_map(|origin| self.source_attribution(&origin))
            .collect()
    }

    fn source_attribution(&self, origin: &OriginExportKey) -> Option<SourceAttribution> {
        if !is_precise_source_candidate(origin) {
            return None;
        }
        let span = self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::SourceSpan(span) if &span.origin == origin => Some(span),
            _ => None,
        })?;
        Some(SourceAttribution {
            origin: span.origin.clone(),
            file: span.file.clone(),
            label: self.source_span_label(span),
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        })
    }

    fn source_span_label(&self, span: &trace_facts::SourceSpanFact) -> String {
        let file = self
            .snapshot
            .facts()
            .iter()
            .find_map(|fact| match fact {
                TraceFact::SourceFile(file) if file.file_key == span.file => {
                    Some(file.display_name.as_str())
                }
                _ => None,
            })
            .unwrap_or_else(|| span.file.local_key());
        format!(
            "{file}:{}:{}-{}:{}",
            span.start_line, span.start_column, span.end_line, span.end_column
        )
    }

    fn variables_at_pc(
        &self,
        pc: u32,
        code_object_filter: Option<&OriginExportKey>,
    ) -> Vec<VariableAtPcRow> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::LocationRange(location)
                    if location.pc_range.start <= pc
                        && pc < location.pc_range.end
                        && code_object_filter
                            .is_none_or(|code_object| code_object == &location.code_object) =>
                {
                    Some(VariableAtPcRow {
                        variable: location.subject.clone(),
                        name: self.variable_name(&location.subject),
                        location: format!("{:?}", location.location),
                        reason: format!("{:?}", location.reason),
                        confidence: format!("{:?}", location.confidence),
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn runtime_evidence(&self, trace_id: Option<&str>) -> RuntimeEvidenceSummary {
        let mut runtime_sources = BTreeSet::new();
        let mut value_policies = BTreeSet::new();
        let mut session_count = 0;
        let mut exact_join_steps = 0;
        let mut pc_only_join_steps = 0;
        let mut ambiguous_join_steps = 0;
        let mut missing_join_steps = 0;

        for fact in self.snapshot.facts() {
            if let TraceFact::ExecutionTraceSession(session) = fact
                && runtime_session_matches(&session.session, trace_id)
            {
                session_count += 1;
                runtime_sources.insert(format!("{:?}", session.source));
                value_policies.insert(format!("{:?}", session.value_policy));
            }
        }

        for step in self.execution_steps(trace_id) {
            match step.join_confidence {
                RuntimePcJoinConfidence::ExactCodeHashAndPc
                | RuntimePcJoinConfidence::ExactCodeObjectAndPc => exact_join_steps += 1,
                RuntimePcJoinConfidence::PcOnlyWithinUniqueCodeObject => pc_only_join_steps += 1,
                RuntimePcJoinConfidence::AmbiguousPc
                | RuntimePcJoinConfidence::CodeHashMismatch => ambiguous_join_steps += 1,
                RuntimePcJoinConfidence::MissingStaticInstruction => missing_join_steps += 1,
            }
        }

        RuntimeEvidenceSummary {
            session_count,
            runtime_sources: runtime_sources.into_iter().collect(),
            value_policies: value_policies.into_iter().collect(),
            exact_join_steps,
            pc_only_join_steps,
            ambiguous_join_steps,
            missing_join_steps,
        }
    }

    fn execution_steps(&self, trace_id: Option<&str>) -> Vec<&'a trace_facts::ExecutionStepFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::ExecutionStep(step)
                    if runtime_session_matches(&step.session, trace_id) =>
                {
                    Some(step)
                }
                _ => None,
            })
            .collect()
    }

    fn execution_step(
        &self,
        step_key: &OriginExportKey,
    ) -> Option<&'a trace_facts::ExecutionStepFact> {
        self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::ExecutionStep(step) if &step.step == step_key => Some(step),
            _ => None,
        })
    }

    fn storage_accesses(&self, trace_id: Option<&str>) -> Vec<&'a trace_facts::StorageAccessFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::StorageAccess(access)
                    if self
                        .execution_step(&access.step)
                        .is_some_and(|step| runtime_session_matches(&step.session, trace_id)) =>
                {
                    Some(access)
                }
                _ => None,
            })
            .collect()
    }

    fn memory_accesses(&self, trace_id: Option<&str>) -> Vec<&'a trace_facts::MemoryAccessFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::MemoryAccess(access)
                    if self
                        .execution_step(&access.step)
                        .is_some_and(|step| runtime_session_matches(&step.session, trace_id)) =>
                {
                    Some(access)
                }
                _ => None,
            })
            .collect()
    }

    fn calls(&self, trace_id: Option<&str>) -> Vec<&'a trace_facts::CallFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Call(call)
                    if self
                        .execution_step(&call.step)
                        .is_some_and(|step| runtime_session_matches(&step.session, trace_id)) =>
                {
                    Some(call)
                }
                _ => None,
            })
            .collect()
    }

    fn reverts(&self, trace_id: Option<&str>) -> Vec<&'a trace_facts::RevertFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Revert(revert)
                    if self
                        .execution_step(&revert.step)
                        .is_some_and(|step| runtime_session_matches(&step.session, trace_id)) =>
                {
                    Some(revert)
                }
                _ => None,
            })
            .collect()
    }

    fn stack_sample_for_step(
        &self,
        step_key: &OriginExportKey,
    ) -> Option<&'a trace_facts::StackSampleFact> {
        self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::StackSample(sample) if &sample.step == step_key => Some(sample),
            _ => None,
        })
    }

    fn storage_accesses_for_step(
        &self,
        step_key: &OriginExportKey,
    ) -> Vec<&'a trace_facts::StorageAccessFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::StorageAccess(access) if &access.step == step_key => Some(access),
                _ => None,
            })
            .collect()
    }

    fn memory_accesses_for_step(
        &self,
        step_key: &OriginExportKey,
    ) -> Vec<&'a trace_facts::MemoryAccessFact> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::MemoryAccess(access) if &access.step == step_key => Some(access),
                _ => None,
            })
            .collect()
    }

    fn gas_for_step(&self, step_key: &OriginExportKey) -> u64 {
        self.execution_step(step_key)
            .map(|step| step.gas_cost)
            .unwrap_or_default()
    }

    fn instruction_for_runtime_access(
        &self,
        instruction: Option<&OriginExportKey>,
        step: &OriginExportKey,
    ) -> Option<OriginExportKey> {
        instruction.cloned().or_else(|| {
            self.execution_step(step)
                .and_then(|step| step.instruction.clone())
        })
    }

    fn sources_for_runtime_access(
        &self,
        instruction: Option<&OriginExportKey>,
        step: &OriginExportKey,
    ) -> Vec<SourceAttribution> {
        self.instruction_for_runtime_access(instruction, step)
            .map(|instruction| self.source_candidates_for_instruction(&instruction))
            .unwrap_or_default()
    }

    fn primary_source_for_instruction(
        &self,
        instruction: &OriginExportKey,
    ) -> Option<SourceAttribution> {
        primary_source(&self.source_candidates_for_instruction(instruction))
    }

    fn variable_name(&self, variable: &OriginExportKey) -> String {
        self.snapshot
            .facts()
            .iter()
            .find_map(|fact| match fact {
                TraceFact::Variable(var) if &var.variable == variable => Some(var.name.clone()),
                TraceFact::DisplayName(name) if &name.subject == variable => {
                    Some(name.name.clone())
                }
                _ => None,
            })
            .unwrap_or_else(|| self.label(variable))
    }

    fn compiler_event_reason_for_output(&self, output: &OriginExportKey) -> Option<String> {
        self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::InsertIntegerZeroExtend
                    && event.outputs.iter().any(|candidate| candidate == output) =>
            {
                event
                    .reason
                    .as_ref()
                    .map(|reason| reason.as_str().to_string())
            }
            _ => None,
        })
    }

    fn label(&self, key: &OriginExportKey) -> String {
        if let Some(name) = self.display_names.get(key) {
            return name.clone();
        }
        match key.kind() {
            "runtime.local" => local_display_name(key),
            "loop" => key.local_key().replace(':', " "),
            _ => key.display_label(),
        }
    }
}

fn is_precise_source_candidate(origin: &OriginExportKey) -> bool {
    origin.kind() != "code.object" && origin.kind() != "source.file"
}

fn storage_step(storage: &StorageFact) -> StorageStep {
    StorageStep {
        phase: format!("{:?}", storage.phase),
        location: format_storage_location(&storage.location),
        reason: format!("{:?}", storage.reason),
    }
}

fn loop_block_role_label(role: LoopBlockRole) -> &'static str {
    match role {
        LoopBlockRole::Header => "header",
        LoopBlockRole::Body => "body",
        LoopBlockRole::Latch => "latch",
        LoopBlockRole::Preheader => "preheader",
        LoopBlockRole::Exit => "exit",
    }
}

fn preferred_loop_key(
    loop_members: &BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>>,
    loop_phases: &BTreeMap<OriginExportKey, CompilerPhase>,
) -> Option<OriginExportKey> {
    [
        CompilerPhase::SonatinaPostOpt,
        CompilerPhase::SonatinaPreOpt,
        CompilerPhase::Mir,
    ]
    .into_iter()
    .find_map(|preferred| {
        loop_members
            .keys()
            .find(|key| loop_phases.get(*key) == Some(&preferred))
            .cloned()
    })
    .or_else(|| loop_members.keys().next().cloned())
}

#[derive(Clone, Debug)]
struct GasAttributionBucket {
    key: String,
    source: Option<OriginExportKey>,
    label: String,
    confidence: Confidence,
}

impl GasAttributionBucket {
    fn source(source: SourceAttribution, policy: GasAttributionPolicy, synthetic: bool) -> Self {
        let label = if synthetic {
            format!("<synthetic-overhead> {}", source.label)
        } else {
            source.label.clone()
        };
        let confidence = match policy {
            _ if source.origin.kind() == "code.object" => Confidence::Low,
            GasAttributionPolicy::Inclusive | GasAttributionPolicy::CallInclusive => {
                Confidence::Low
            }
            GasAttributionPolicy::SyntheticOverhead if synthetic => Confidence::Medium,
            _ => Confidence::Medium,
        };
        Self {
            key: source.origin.canonical_storage_key(),
            source: Some(source.origin),
            label,
            confidence,
        }
    }

    fn unmapped() -> Self {
        Self {
            key: "<unmapped>".to_string(),
            source: None,
            label: "<unmapped>".to_string(),
            confidence: Confidence::Unknown,
        }
    }
}

fn gas_to_source_row<'a>(
    rows: &'a mut BTreeMap<String, GasToSourceRow>,
    bucket: &GasAttributionBucket,
) -> &'a mut GasToSourceRow {
    rows.entry(bucket.key.clone())
        .or_insert_with(|| GasToSourceRow {
            source: bucket.source.clone(),
            label: bucket.label.clone(),
            static_gas: 0,
            dynamic_gas: 0,
            total_gas: 0,
            instruction_count: 0,
            confidence: bucket.confidence,
        })
}

fn loop_cost_findings(
    available: bool,
    summary: &InstructionSummary,
    repeated_zero_extends: &[LocalInstructionGroup],
    storage_impacts: &[StorageImpact],
) -> Vec<Insight> {
    let mut findings = Vec::new();
    if !available {
        findings.push(Insight::info(
            "Loop cost unavailable",
            "required loop membership facts are missing",
        ));
    }
    if summary.zero_extends > 0 {
        findings.push(Insight::hint(
            "Repeated zero-extensions",
            format!(
                "{} zero-extend instructions are attributed to {} local group(s)",
                summary.zero_extends,
                repeated_zero_extends.len()
            ),
        ));
    }
    let stack_traffic: usize = storage_impacts
        .iter()
        .map(|impact| impact.loads + impact.stores)
        .sum();
    if stack_traffic > 0 {
        findings.push(Insight::hint(
            "Stack traffic in loop",
            format!("{stack_traffic} stack-related load/store edge(s) are attributed"),
        ));
    }
    findings
}

fn primary_source(candidates: &[SourceAttribution]) -> Option<SourceAttribution> {
    (candidates.len() == 1).then(|| candidates[0].clone())
}

fn missing_link_audit_report(
    snapshot: &TraceSnapshot,
    source_exact_pcs: usize,
    prepared_linked_pcs: usize,
    unmapped_pcs: usize,
    lineage_gap_targets: &BTreeMap<OriginExportKey, usize>,
    lineage_gaps: &[AttributionAuditLineageGap],
    invalid: Vec<InvalidAttributionLink>,
) -> MissingLinkAuditReport {
    let missing_required_count = lineage_gaps.len();
    let expected_absent: Vec<ExpectedAbsentLink> = Vec::new();
    let overall_status = if !invalid.is_empty() {
        LinkOverallStatus::Invalid
    } else if missing_required_count > 0 {
        LinkOverallStatus::Warning
    } else if !expected_absent.is_empty() {
        LinkOverallStatus::PassWithExpectedAbsences
    } else {
        LinkOverallStatus::Pass
    };
    let mut top_blockers = Vec::new();
    if missing_required_count > 0 || !invalid.is_empty() {
        top_blockers.push(LinkBoundaryKind::PostOptToPrepared);
    }

    let mut boundary_summaries = Vec::new();
    if prepared_linked_pcs > 0 || missing_required_count > 0 || !invalid.is_empty() {
        let mut status_counts = BTreeMap::new();
        if !invalid.is_empty() {
            status_counts.insert(LinkStatus::Invalid, invalid.len());
        }
        if missing_required_count > 0 {
            status_counts.insert(LinkStatus::MissingRequired, missing_required_count);
        }
        if status_counts.is_empty() {
            status_counts.insert(LinkStatus::SatisfiedExact, prepared_linked_pcs);
        }
        let top_issue_codes = [
            (!invalid.is_empty()).then_some(LinkIssueCode::PreparedKeyedAsPostopt),
            (missing_required_count > 0)
                .then_some(LinkIssueCode::MissingOptimizedToPreparedLineage),
        ]
        .into_iter()
        .flatten()
        .collect();
        boundary_summaries.push(LinkBoundarySummary {
            boundary: LinkBoundaryKind::PostOptToPrepared,
            owner_phase: CompilerPhase::Backend,
            status_counts,
            affected_origins: lineage_gap_targets.len() + invalid.len(),
            affected_bytecode_pcs: missing_required_count + invalid.len(),
            top_issue_codes,
        });
    }

    if prepared_linked_pcs > 0 {
        let mut prepared_to_bytecode_counts = BTreeMap::new();
        prepared_to_bytecode_counts.insert(LinkStatus::SatisfiedExact, prepared_linked_pcs);
        boundary_summaries.push(LinkBoundarySummary {
            boundary: LinkBoundaryKind::PreparedToBytecode,
            owner_phase: CompilerPhase::BytecodeEmission,
            status_counts: prepared_to_bytecode_counts,
            affected_origins: lineage_gap_targets.len(),
            affected_bytecode_pcs: prepared_linked_pcs,
            top_issue_codes: Vec::new(),
        });
    }

    let required_evidence = required_optimized_to_prepared_evidence();
    let cluster_id = "postopt-to-prepared:missing-optimized-to-prepared-lineage".to_string();
    let details = lineage_gaps
        .iter()
        .take(MISSING_LINK_DETAIL_LIMIT)
        .map(|gap| LinkGap {
            gap_id: link_gap_id(
                LinkBoundaryKind::PostOptToPrepared,
                &gap.bytecode_pc,
                &gap.prepared_origin,
            ),
            boundary: LinkBoundaryKind::PostOptToPrepared,
            status: LinkStatus::MissingRequired,
            issue_code: LinkIssueCode::MissingOptimizedToPreparedLineage,
            severity: LinkSeverity::Warning,
            from_origin: gap.prepared_origin.clone(),
            from_representation: Some("sonatina.evm.prepared".to_string()),
            expected_to_phase: "sonatina.postopt".to_string(),
            reached_frontier: "evm.prepared.bytecode".to_string(),
            candidate_hints: Vec::new(),
            required_evidence: required_evidence.clone(),
            cluster_id: Some(cluster_id.clone()),
        })
        .collect::<Vec<_>>();

    let clusters = if missing_required_count == 0 {
        Vec::new()
    } else {
        let mut affected_origins = attribution_audit_target_counts(lineage_gap_targets.clone());
        affected_origins.truncate(25);
        let affected_bytecode_pcs = lineage_gaps
            .iter()
            .map(|gap| gap.bytecode_pc.clone())
            .take(25)
            .collect::<Vec<_>>();
        vec![LinkGapCluster {
            cluster_id: cluster_id.clone(),
            boundary: LinkBoundaryKind::PostOptToPrepared,
            status: LinkStatus::MissingRequired,
            issue_code: LinkIssueCode::MissingOptimizedToPreparedLineage,
            severity: LinkSeverity::Warning,
            owner_phase: CompilerPhase::Backend,
            headline: "Optimized Sonatina reaches no EVM prepared lineage".to_string(),
            explanation: "Source and optimized Sonatina evidence may exist, and EVM prepared/bytecode evidence exists, but no explicit optimized→prepared lineage edge is present for these prepared origins.".to_string(),
            affected_origins,
            affected_bytecode_pcs,
            gap_count: missing_required_count,
            sample_gap_ids: details
                .iter()
                .take(MISSING_LINK_CLUSTER_SAMPLE_LIMIT)
                .map(|gap| gap.gap_id.clone())
                .collect(),
            candidate_hints: Vec::new(),
            required_evidence: required_evidence.clone(),
        }]
    };

    MissingLinkAuditReport {
        schema_version: MISSING_LINK_AUDIT_SCHEMA_VERSION.to_string(),
        query_pack: MISSING_LINK_QUERY_PACK.to_string(),
        artifact: missing_link_artifact(snapshot),
        summary: MissingLinkSummary {
            status: overall_status,
            top_blockers,
            exact_source_to_bytecode_pcs: source_exact_pcs,
            prepared_linked_bytecode_pcs: prepared_linked_pcs,
            unmapped_bytecode_pcs: unmapped_pcs,
            missing_required_count,
            invalid_count: invalid.len(),
            expected_absent_count: expected_absent.len(),
        },
        boundary_summaries,
        clusters,
        gaps: details,
        expected_absent,
        invalid,
    }
}

fn missing_link_artifact(snapshot: &TraceSnapshot) -> MissingLinkAuditArtifact {
    let metadata = snapshot.metadata();
    MissingLinkAuditArtifact {
        source_uri: metadata.input_path.clone(),
        compiler_commit: metadata.compiler_commit.clone(),
        sonatina_commit: trace_metadata_flag_value(metadata, "sonatina_commit")
            .or_else(|| trace_metadata_flag_value(metadata, "sonatina")),
        opt_level: trace_metadata_flag_value(metadata, "optimize")
            .or_else(|| trace_metadata_flag_value(metadata, "opt_level")),
        target: metadata.target.clone(),
        trace_schema_version: TRACE_SCHEMA_VERSION.to_string(),
        relation_schema_version: None,
        edge_semantics_version: EDGE_SEMANTICS_VERSION.to_string(),
    }
}

fn trace_metadata_flag_value(metadata: &TraceMetadata, name: &str) -> Option<String> {
    let prefix = format!("{name}=");
    metadata
        .flags
        .iter()
        .find_map(|flag| flag.strip_prefix(&prefix).map(str::to_string))
}

fn required_optimized_to_prepared_evidence() -> Vec<RequiredEvidence> {
    vec![RequiredEvidence {
        kind: RequiredEvidenceKind::PreparedLineageFact,
        owner_phase: CompilerPhase::Backend,
        description: "Emit explicit EVM prepared/codegen → optimized Sonatina lineage with exact, backend-prepared, synthetic, debug-context, split/merge, or elision semantics.".to_string(),
    }]
}

fn link_gap_id(
    boundary: LinkBoundaryKind,
    bytecode_pc: &OriginExportKey,
    prepared_origin: &OriginExportKey,
) -> String {
    stable_short_id(&format!(
        "{boundary:?}\n{}\n{}",
        bytecode_pc.canonical_storage_key(),
        prepared_origin.canonical_storage_key()
    ))
}

fn stable_short_id(input: &str) -> String {
    let mut hash = 2166136261u32;
    for byte in input.bytes() {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16777619);
    }
    format!("gap-{hash:08x}")
}

fn invalid_direct_bytecode_to_postopt_edge(edge: &trace_facts::OriginEdgeFact) -> bool {
    edge.from.kind() == "bytecode.pc"
        && is_optimized_sonatina_audit_origin(&edge.to)
        && edge.has_transform_claim_label()
}

fn attribution_audit_report(snapshot: &TraceSnapshot) -> AttributionAuditReport {
    let index = TraceIndex::new(snapshot);
    let semantic_index = trace_index::TraceIndex::new(snapshot);
    let mut direct_edges = BTreeMap::<
        (
            OriginEdgeLabel,
            String,
            Option<CompilerPhase>,
            OriginEdgeTraversalClass,
        ),
        usize,
    >::new();
    let mut class_counts = BTreeMap::<OriginEdgeTraversalClass, usize>::new();
    let mut source_lines = BTreeMap::<(String, String), usize>::new();
    let mut sonatina_targets = BTreeMap::<OriginExportKey, usize>::new();
    let mut optimized_sonatina_targets = BTreeMap::<OriginExportKey, usize>::new();
    let mut prepared_targets = BTreeMap::<OriginExportKey, usize>::new();
    let mut lineage_gap_targets = BTreeMap::<OriginExportKey, usize>::new();
    let mut lineage_gaps = Vec::new();
    let mut suspicious_edges = Vec::new();
    let mut invalid_links = Vec::new();
    let mut total_bytecode_pcs = 0usize;
    let mut source_exact_pcs = 0usize;
    let mut source_ambiguous_pcs = 0usize;
    let mut unmapped_pcs = 0usize;
    let mut optimized_sonatina_linked_pcs = 0usize;
    let mut prepared_linked_pcs = 0usize;
    let mut missing_optimized_to_prepared_lineage_pcs = 0usize;

    for instruction in index.all_instruction_keys() {
        if instruction.kind() != "bytecode.pc" {
            continue;
        }
        total_bytecode_pcs += 1;
        let sources = semantic_index
            .source_candidates_for_instruction(
                &instruction,
                trace_index::TraceReachabilityPolicy::ExactOnly,
            )
            .into_iter()
            .filter_map(|origin| index.source_attribution(&origin))
            .collect::<Vec<_>>();
        match sources.len() {
            0 => unmapped_pcs += 1,
            1 => source_exact_pcs += 1,
            _ => source_ambiguous_pcs += 1,
        }
        for source in sources {
            *source_lines
                .entry((source.label, source.origin.kind().to_string()))
                .or_default() += 1;
        }

        let reachable = semantic_index.reachable_targets(
            &instruction,
            trace_index::TraceReachabilityPolicy::ExactOnly,
        );
        let optimized_targets_for_pc = reachable
            .iter()
            .filter(|target| is_optimized_sonatina_audit_origin(target))
            .cloned()
            .collect::<Vec<_>>();
        let prepared_targets_for_pc = reachable
            .iter()
            .filter(|target| is_prepared_sonatina_audit_origin(target))
            .cloned()
            .collect::<Vec<_>>();

        if !optimized_targets_for_pc.is_empty() {
            optimized_sonatina_linked_pcs += 1;
            for target in optimized_targets_for_pc {
                *optimized_sonatina_targets.entry(target).or_default() += 1;
            }
        }
        if !prepared_targets_for_pc.is_empty() {
            prepared_linked_pcs += 1;
            for target in &prepared_targets_for_pc {
                *prepared_targets.entry(target.clone()).or_default() += 1;
            }
            let mut has_missing_prepared_lineage = false;
            for prepared in prepared_targets_for_pc {
                let prepared_reachable = semantic_index
                    .reachable_targets(&prepared, trace_index::TraceReachabilityPolicy::ExactOnly);
                if prepared_reachable
                    .iter()
                    .any(is_optimized_sonatina_audit_origin)
                {
                    continue;
                }
                has_missing_prepared_lineage = true;
                *lineage_gap_targets.entry(prepared.clone()).or_default() += 1;
                lineage_gaps.push(AttributionAuditLineageGap {
                    bytecode_pc: instruction.clone(),
                    prepared_origin: prepared,
                    reason: "missing_optimized_to_prepared_lineage".to_string(),
                });
            }
            if has_missing_prepared_lineage {
                missing_optimized_to_prepared_lineage_pcs += 1;
            }
        }

        for edge in snapshot.facts().iter().filter_map(|fact| match fact {
            TraceFact::OriginEdge(edge) if edge.from == instruction => Some(edge),
            _ => None,
        }) {
            let class = edge.traversal_class();
            *direct_edges
                .entry((
                    edge.label,
                    edge.to.kind().to_string(),
                    edge.introduced_by,
                    class,
                ))
                .or_default() += 1;
            *class_counts.entry(class).or_default() += 1;
            if edge.to.kind().starts_with("sonatina.") {
                *sonatina_targets.entry(edge.to.clone()).or_default() += 1;
            }
            if invalid_direct_bytecode_to_postopt_edge(edge) {
                invalid_links.push(InvalidAttributionLink {
                    boundary: LinkBoundaryKind::PostOptToPrepared,
                    origin: edge.to.clone(),
                    issue_code: LinkIssueCode::PreparedKeyedAsPostopt,
                    explanation: "bytecode PC is linked directly to optimized Sonatina; bytecode emission must use EVM prepared/VCode identity and explicit optimized→prepared lineage".to_string(),
                });
            }
            if matches!(class, OriginEdgeTraversalClass::Contextual)
                && edge.has_transform_claim_label()
            {
                suspicious_edges.push(AttributionAuditSuspiciousEdge {
                    from: edge.from.clone(),
                    to: edge.to.clone(),
                    label: edge.label,
                    introduced_by: edge.introduced_by,
                    traversal_class: class,
                    reason:
                        "exact-looking bytecode edge is contextual under endpoint/phase semantics"
                            .to_string(),
                });
            }
        }
    }

    let mut direct_bytecode_edges = direct_edges
        .into_iter()
        .map(
            |((label, to_kind, introduced_by, traversal_class), count)| AttributionAuditEdgeCount {
                label,
                to_kind,
                introduced_by,
                traversal_class,
                count,
            },
        )
        .collect::<Vec<_>>();
    direct_bytecode_edges.sort_by(|a, b| {
        b.count
            .cmp(&a.count)
            .then_with(|| a.to_kind.cmp(&b.to_kind))
            .then_with(|| format!("{:?}", a.label).cmp(&format!("{:?}", b.label)))
    });

    let mut direct_edges_by_class = class_counts
        .into_iter()
        .map(|(traversal_class, count)| AttributionAuditClassCount {
            traversal_class,
            count,
        })
        .collect::<Vec<_>>();
    direct_edges_by_class.sort_by(|a, b| b.count.cmp(&a.count));

    let mut source_lines = source_lines
        .into_iter()
        .map(
            |((label, origin_kind), count)| AttributionAuditSourceLineCount {
                label,
                origin_kind,
                count,
            },
        )
        .collect::<Vec<_>>();
    source_lines.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.label.cmp(&b.label)));

    let mut sonatina_targets = sonatina_targets
        .into_iter()
        .map(|(target, count)| AttributionAuditTargetCount { target, count })
        .collect::<Vec<_>>();
    sonatina_targets.sort_by(|a, b| {
        b.count.cmp(&a.count).then_with(|| {
            a.target
                .canonical_storage_key()
                .cmp(&b.target.canonical_storage_key())
        })
    });
    sonatina_targets.truncate(25);

    let mut optimized_sonatina_targets =
        attribution_audit_target_counts(optimized_sonatina_targets);
    optimized_sonatina_targets.truncate(25);

    let mut prepared_targets = attribution_audit_target_counts(prepared_targets);
    prepared_targets.truncate(25);

    let missing_links = missing_link_audit_report(
        snapshot,
        source_exact_pcs,
        prepared_linked_pcs,
        unmapped_pcs,
        &lineage_gap_targets,
        &lineage_gaps,
        invalid_links,
    );

    let mut lineage_gap_targets = attribution_audit_target_counts(lineage_gap_targets);
    lineage_gap_targets.truncate(25);
    lineage_gaps.sort_by(|a, b| {
        a.bytecode_pc
            .canonical_storage_key()
            .cmp(&b.bytecode_pc.canonical_storage_key())
            .then_with(|| {
                a.prepared_origin
                    .canonical_storage_key()
                    .cmp(&b.prepared_origin.canonical_storage_key())
            })
    });
    lineage_gaps.truncate(25);
    suspicious_edges.truncate(25);

    AttributionAuditReport {
        metadata: ReportMetadata::from_snapshot(snapshot),
        total_bytecode_pcs,
        source_exact_pcs,
        source_ambiguous_pcs,
        unmapped_pcs,
        optimized_sonatina_linked_pcs,
        prepared_linked_pcs,
        missing_optimized_to_prepared_lineage_pcs,
        direct_bytecode_edges,
        direct_edges_by_class,
        source_lines,
        sonatina_targets,
        optimized_sonatina_targets,
        prepared_targets,
        missing_lineage_targets: lineage_gap_targets,
        lineage_gaps,
        suspicious_edges,
        missing_links: Some(missing_links),
    }
}

fn is_optimized_sonatina_audit_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.post")
}

fn is_prepared_sonatina_audit_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.evm.prepared")
}

fn attribution_audit_target_counts(
    counts: BTreeMap<OriginExportKey, usize>,
) -> Vec<AttributionAuditTargetCount> {
    let mut targets = counts
        .into_iter()
        .map(|(target, count)| AttributionAuditTargetCount { target, count })
        .collect::<Vec<_>>();
    targets.sort_by(|a, b| {
        b.count.cmp(&a.count).then_with(|| {
            a.target
                .canonical_storage_key()
                .cmp(&b.target.canonical_storage_key())
        })
    });
    targets
}

fn runtime_session_matches(session: &OriginExportKey, trace_id: Option<&str>) -> bool {
    trace_id.is_none_or(|trace_id| {
        trace_id == session.canonical_storage_key()
            || trace_id == session.display_label()
            || trace_id == session.owner_key()
            || trace_id == session.local_key()
    })
}

fn runtime_value_label(value: &RuntimeValue) -> String {
    match value {
        RuntimeValue::Redacted => "<redacted>".to_string(),
        RuntimeValue::Hash { algorithm, digest } => format!("{algorithm}:{digest}"),
        RuntimeValue::Bytes { hex } => hex.clone(),
    }
}

fn confidence_for_join(join: RuntimePcJoinConfidence) -> Confidence {
    match join {
        RuntimePcJoinConfidence::ExactCodeHashAndPc
        | RuntimePcJoinConfidence::ExactCodeObjectAndPc => Confidence::Exact,
        RuntimePcJoinConfidence::PcOnlyWithinUniqueCodeObject => Confidence::Medium,
        RuntimePcJoinConfidence::AmbiguousPc | RuntimePcJoinConfidence::CodeHashMismatch => {
            Confidence::Low
        }
        RuntimePcJoinConfidence::MissingStaticInstruction => Confidence::Unknown,
    }
}

fn merge_confidence(left: Confidence, right: Confidence) -> Confidence {
    if confidence_rank(left) <= confidence_rank(right) {
        left
    } else {
        right
    }
}

fn confidence_rank(confidence: Confidence) -> u8 {
    match confidence {
        Confidence::Unknown => 0,
        Confidence::Low => 1,
        Confidence::Medium => 2,
        Confidence::High => 3,
        Confidence::Exact => 4,
    }
}

fn runtime_report_confidence(has_rows: bool, evidence: &RuntimeEvidenceSummary) -> Confidence {
    if !has_rows {
        return Confidence::Unknown;
    }
    if evidence.missing_join_steps > 0 || evidence.ambiguous_join_steps > 0 {
        Confidence::Low
    } else if evidence.pc_only_join_steps > 0 {
        Confidence::Medium
    } else if evidence.exact_join_steps > 0 {
        Confidence::High
    } else {
        Confidence::Unknown
    }
}

pub fn data_source_label(metadata: &TraceMetadata) -> String {
    match metadata.data_source {
        TraceDataSource::Fixture => {
            let marker = metadata.fixture_marker.as_deref().unwrap_or("unspecified");
            format!("fixture ({marker}; not compiler-derived)")
        }
        TraceDataSource::CompilerEmitted => "compiler_emitted".to_string(),
    }
}

fn local_display_name(key: &OriginExportKey) -> String {
    let local = key.local_key();
    local
        .strip_prefix("local:")
        .or_else(|| local.rsplit_once(':').map(|(_, name)| name))
        .unwrap_or(local)
        .to_string()
}

fn format_storage_location(location: &StorageLocation) -> String {
    match location {
        StorageLocation::SsaValue => "SSA value".to_string(),
        StorageLocation::MemoryPlace => "memory place".to_string(),
        StorageLocation::StackSlot { offset } => format!("stack slot sp+{offset}"),
        StorageLocation::VirtualRegister(name) => format!("virtual register {name}"),
        StorageLocation::PhysicalRegister(name) => format!("physical register {name}"),
        StorageLocation::Unknown => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use std::collections::BTreeMap;
    use trace_facts::{
        BlockFact, CallFact, CategorySource, CodeObjectFact, CodeObjectKind, CompilerEventFact,
        CompilerEventKind, CompilerPhase, CompilerReason, DisplayNameFact, DisplayNameKind,
        DynamicGasStepFact, EvmSchedule, ExecutionStepFact, ExecutionTraceSessionFact,
        GasConfidence, GasCostFact, GasKind, GasSource, InstructionBlockFact, InstructionCategory,
        InstructionCategoryFact, InstructionExtentFact, InstructionFact, LocationConfidence,
        LocationRangeFact, LoopBlockFact, LoopBlockRole, LoopConfidence, LoopDerivation, LoopFact,
        LoopMembershipFact, MemoryAccessFact, MemoryAccessKind, OriginEdgeFact, OriginEdgeLabel,
        OriginEdgeTraversalClass, OriginNodeFact, OriginNodeKind, PcRange, RevertFact,
        RuntimeCallKind, RuntimeCaptureMode, RuntimeCodeObjectBindingFact, RuntimePcJoinConfidence,
        RuntimeTraceDataSource, RuntimeValue, RuntimeValuePolicy, SourceFileFact, SourceSpanFact,
        StackSampleFact, StaticGasFact, StorageAccessFact, StorageAccessKind, StorageFact,
        StorageLocation, StorageReason, TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
        TypeFact, TypeKind, ValueLocation, VariableFact, VariableStorageClass,
    };

    use super::{
        CallCostByCallsiteReport, DynamicGasBySourceRequest, ExplainLocalRequest, ExplainPcRequest,
        GasBySourceRequest, GasToSourceRequest, IntrospectionService, LinkBoundaryKind,
        LinkIssueCode, LinkOverallStatus, LinkStatus, LoopContentsRequest, LoopCostRequest,
        MISSING_LINK_AUDIT_SCHEMA_VERSION, OptimizedCodeHonestyRequest, RequiredEvidenceKind,
        RuntimeGasBySourceRequest, RuntimeTraceFilterRequest, StorageAccessesBySlotRequest,
        TraceIntrospectionService, TraceQueryHttpRequest, TraceQueryHttpResponse, TraceQueryReport,
        TraceQueryRequest, TraceWorkbenchDisplayStatus, TraceWorkbenchMissingLineageIndex,
        TraceWorkbenchPaneRowKind, TraceWorkbenchProjectionIndex, TraceWorkbenchProjectionRequest,
        ValueFlowAtPcRequest, VariablesAtPcRequest, run_trace_query,
        trace_workbench_compact_mir_text, trace_workbench_compact_origin_text,
        trace_workbench_manifest, trace_workbench_natural_key_cmp,
        trace_workbench_report_projection, trace_workbench_source_lines,
        trace_workbench_source_projection, trace_workbench_status_for_row,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn snapshot(facts: Vec<TraceFact>) -> TraceSnapshot {
        TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::fixture(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string()],
                "demo.fe",
                vec!["optimize=2".to_string()],
                "query-test",
            ),
            facts,
        ))
        .unwrap()
    }

    fn demo_service() -> TraceIntrospectionService {
        let source_file = key("source.file", "demo", "fib.fe");
        let source_expr = key("hir.expr", "demo", "expr:b");
        let source_expr_alt = key("hir.expr", "demo", "expr:c");
        let code_object = key("code.object", "demo", "runtime");
        let function = key("function", "demo", "recv");
        let loop_key = key("loop", "demo", "while:i<n");
        let block = key("block", "demo", "loop-body");
        let local = key("runtime.local", "demo", "local:b");
        let ty = key("type", "demo", "u32");
        let inst = key("bytecode.pc", "demo", "pc:0");
        let zext = key("bytecode.pc", "demo", "pc:1");
        let inst_postopt = key("sonatina.postopt.inst", "demo", "inst:0");
        let zext_postopt = key("sonatina.postopt.inst", "demo", "inst:1");
        let ambiguous = key("bytecode.pc", "demo", "pc:2");
        let synthetic = key("bytecode.pc", "demo", "pc:3");
        let event = key("compiler.event", "demo", "event:0");
        let facts = vec![
            node(source_file.clone()),
            node(source_expr.clone()),
            node(source_expr_alt.clone()),
            node(code_object.clone()),
            node(function.clone()),
            node(loop_key.clone()),
            node(block.clone()),
            node(local.clone()),
            node(ty.clone()),
            node(inst.clone()),
            node(zext.clone()),
            node(inst_postopt.clone()),
            node(zext_postopt.clone()),
            node(ambiguous.clone()),
            node(synthetic.clone()),
            node(event.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///demo/fib.fe",
                "fib.fe",
                "blake3:000000000000000000000000000000000000000000000000000000000000abcd",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source_expr.clone(),
                source_file.clone(),
                10,
                11,
                2,
                8,
                2,
                9,
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                code_object.clone(),
                source_file.clone(),
                0,
                100,
                1,
                1,
                9,
                1,
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source_expr_alt.clone(),
                source_file,
                12,
                13,
                2,
                10,
                2,
                11,
            )),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                Some(
                    "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                        .to_string(),
                ),
            )),
            TraceFact::Function(trace_facts::FunctionFact::new(
                function.clone(),
                "recv",
                Some(source_expr.clone()),
                Some(code_object.clone()),
            )),
            TraceFact::Block(BlockFact::new(
                block.clone(),
                function.clone(),
                CompilerPhase::Backend,
                0,
                Some("loop-body".to_string()),
            )),
            TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function.clone(),
                CompilerPhase::Backend,
                block.clone(),
                LoopDerivation::BackendBlockMapping,
                LoopConfidence::BackendBlockMapping,
            )),
            TraceFact::LoopBlock(LoopBlockFact::new(
                loop_key.clone(),
                block.clone(),
                LoopBlockRole::Header,
            )),
            TraceFact::Type(TypeFact::new(
                ty.clone(),
                TypeKind::UnsignedInteger,
                Some("u32".to_string()),
                Some(32),
                Vec::new(),
            )),
            TraceFact::Variable(VariableFact::new(
                local.clone(),
                "b",
                ty,
                source_expr.clone(),
                None,
                VariableStorageClass::Local,
            )),
            TraceFact::LocationRange(LocationRangeFact::new(
                local.clone(),
                code_object.clone(),
                PcRange::new(0, 2),
                ValueLocation::StackSlot { offset: 24 },
                StorageReason::FrameSlot,
                LocationConfidence::Conservative,
            )),
            TraceFact::Storage(StorageFact::new(
                local.clone(),
                CompilerPhase::Mir,
                StorageLocation::MemoryPlace,
                StorageReason::MutableLocalLowering,
            )),
            TraceFact::Storage(StorageFact::new(
                local.clone(),
                CompilerPhase::Backend,
                StorageLocation::StackSlot { offset: 24 },
                StorageReason::FrameSlot,
            )),
            TraceFact::Instruction(InstructionFact::new(
                inst.clone(),
                function.clone(),
                0,
                "lw",
            )),
            TraceFact::InstructionExtent(InstructionExtentFact::new(
                inst.clone(),
                code_object.clone(),
                PcRange::new(0, 1),
                1,
            )),
            TraceFact::InstructionBlock(InstructionBlockFact::new(
                inst.clone(),
                block.clone(),
                CompilerPhase::Backend,
            )),
            TraceFact::Instruction(InstructionFact::new(
                zext.clone(),
                function.clone(),
                1,
                "slli",
            )),
            TraceFact::InstructionExtent(InstructionExtentFact::new(
                zext.clone(),
                code_object.clone(),
                PcRange::new(1, 3),
                2,
            )),
            TraceFact::InstructionBlock(InstructionBlockFact::new(
                zext.clone(),
                block,
                CompilerPhase::Backend,
            )),
            TraceFact::Instruction(InstructionFact::new(
                ambiguous.clone(),
                function.clone(),
                2,
                "add",
            )),
            TraceFact::Instruction(InstructionFact::new(synthetic.clone(), function, 3, "dup")),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                inst.clone(),
                InstructionCategory::StackLoad,
                CategorySource::PosthocClassifier {
                    version: "test".to_string(),
                },
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                zext.clone(),
                InstructionCategory::ZeroExtend,
                CategorySource::PosthocClassifier {
                    version: "test".to_string(),
                },
            )),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key.clone(),
                inst.clone(),
                LoopDerivation::BackendBlockMapping,
            )),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key,
                zext.clone(),
                LoopDerivation::BackendBlockMapping,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                inst.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                zext.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                inst.clone(),
                inst_postopt.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                zext.clone(),
                zext_postopt.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                inst_postopt,
                source_expr.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPostOpt),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                zext_postopt,
                source_expr.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPostOpt),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                inst.clone(),
                local.clone(),
                OriginEdgeLabel::LoadOf,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                local.clone(),
                source_expr.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::Mir),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                zext.clone(),
                local.clone(),
                OriginEdgeLabel::IntegerLegalizationFor,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                ambiguous.clone(),
                source_expr.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                ambiguous,
                source_expr_alt,
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                synthetic.clone(),
                source_expr.clone(),
                OriginEdgeLabel::SyntheticFor,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::CompilerEvent(CompilerEventFact::new(
                event,
                CompilerPhase::Backend,
                CompilerEventKind::InsertIntegerZeroExtend,
                vec![local],
                vec![zext.clone()],
                Some(CompilerReason::new("test reason")),
            )),
            TraceFact::GasCost(GasCostFact::new(
                inst.clone(),
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new("cancun"),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                inst.clone(),
                EvmSchedule::new("cancun"),
                3,
                None,
            )),
            TraceFact::GasCost(GasCostFact::new(
                zext,
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new("cancun"),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
            TraceFact::StaticGas(StaticGasFact::new(
                key("bytecode.pc", "demo", "pc:1"),
                EvmSchedule::new("cancun"),
                3,
                None,
            )),
            TraceFact::DynamicGasStep(DynamicGasStepFact::new(
                "tx:1",
                0,
                code_object.clone(),
                0,
                Some(inst.clone()),
                100,
                93,
                7,
            )),
            TraceFact::DynamicGasStep(DynamicGasStepFact::new(
                "tx:1",
                1,
                code_object.clone(),
                1,
                None,
                93,
                90,
                3,
            )),
            TraceFact::DynamicGasStep(DynamicGasStepFact::new(
                "tx:synthetic",
                0,
                code_object,
                3,
                Some(synthetic),
                90,
                85,
                5,
            )),
        ];
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::fixture(
                "abc123",
                "riscv64-demo",
                vec!["fe".to_string()],
                "fib_demo.fe",
                vec!["function=Fib.recv".to_string()],
                "query-test",
            ),
            facts,
        ))
        .unwrap();
        TraceIntrospectionService::new(snapshot)
    }

    fn runtime_demo_service() -> TraceIntrospectionService {
        let mut bundle = demo_service().snapshot().clone().into_bundle();
        let code_object = key("code.object", "demo", "runtime");
        let inst = key("bytecode.pc", "demo", "pc:0");
        let zext = key("bytecode.pc", "demo", "pc:1");
        let session = key("runtime.session", "tx:1", "session");
        let binding = key("runtime.binding", "tx:1", "runtime");
        let step0 = key("runtime.step", "tx:1", "step:0");
        let step1 = key("runtime.step", "tx:1", "step:1");
        let step2 = key("runtime.step", "tx:1", "step:2");
        let sample = key("runtime.stack", "tx:1", "sample:0");
        let storage_write = key("runtime.storage", "tx:1", "write:0");
        let storage_read = key("runtime.storage", "tx:1", "read:0");
        let memory = key("runtime.memory", "tx:1", "access:0");
        let call = key("runtime.call", "tx:1", "call:0");
        let revert = key("runtime.revert", "tx:1", "revert:0");

        bundle.facts.extend([
            node(session.clone()),
            node(binding.clone()),
            node(step0.clone()),
            node(step1.clone()),
            node(step2.clone()),
            node(sample.clone()),
            node(storage_write.clone()),
            node(storage_read.clone()),
            node(memory.clone()),
            node(call.clone()),
            node(revert.clone()),
            TraceFact::ExecutionTraceSession(ExecutionTraceSessionFact {
                session: session.clone(),
                source: RuntimeTraceDataSource::RevmInspector,
                capture_mode: RuntimeCaptureMode::Standard,
                value_policy: RuntimeValuePolicy::HashOnly,
                transaction_hash: Some("0xabc".to_string()),
                chain_id: Some(31337),
                block_number: Some(1),
                entry_code_object: Some(code_object.clone()),
            }),
            TraceFact::RuntimeCodeObjectBinding(RuntimeCodeObjectBindingFact {
                binding,
                session: session.clone(),
                code_object: code_object.clone(),
                runtime_code_hash:
                    "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                        .to_string(),
                address: Some("0x0000000000000000000000000000000000000001".to_string()),
                confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
            }),
            TraceFact::ExecutionStep(ExecutionStepFact {
                step: step0.clone(),
                session: session.clone(),
                step_index: 0,
                code_object: code_object.clone(),
                pc: 0,
                opcode: "SSTORE".to_string(),
                instruction: Some(inst.clone()),
                gas_before: 100,
                gas_after: 80,
                gas_cost: 20,
                depth: 1,
                join_confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
            }),
            TraceFact::ExecutionStep(ExecutionStepFact {
                step: step1.clone(),
                session: session.clone(),
                step_index: 1,
                code_object: code_object.clone(),
                pc: 1,
                opcode: "MSTORE".to_string(),
                instruction: Some(zext.clone()),
                gas_before: 80,
                gas_after: 70,
                gas_cost: 10,
                depth: 1,
                join_confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
            }),
            TraceFact::ExecutionStep(ExecutionStepFact {
                step: step2.clone(),
                session,
                step_index: 2,
                code_object: code_object.clone(),
                pc: 0,
                opcode: "SSTORE".to_string(),
                instruction: Some(inst.clone()),
                gas_before: 70,
                gas_after: 55,
                gas_cost: 15,
                depth: 1,
                join_confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
            }),
            TraceFact::StackSample(StackSampleFact {
                sample,
                step: step0.clone(),
                policy: RuntimeValuePolicy::HashOnly,
                values_top_first: vec![RuntimeValue::hash("blake3", "top")],
            }),
            TraceFact::StorageAccess(StorageAccessFact {
                access: storage_write,
                step: step0,
                code_object: code_object.clone(),
                instruction: Some(inst.clone()),
                kind: StorageAccessKind::Write,
                address: Some("0x0000000000000000000000000000000000000001".to_string()),
                slot: RuntimeValue::hash("blake3", "slot-0"),
                value_before: Some(RuntimeValue::redacted()),
                value_after: Some(RuntimeValue::hash("blake3", "value-0")),
                policy: RuntimeValuePolicy::HashOnly,
            }),
            TraceFact::StorageAccess(StorageAccessFact {
                access: storage_read,
                step: step2.clone(),
                code_object,
                instruction: Some(inst.clone()),
                kind: StorageAccessKind::Read,
                address: Some("0x0000000000000000000000000000000000000001".to_string()),
                slot: RuntimeValue::hash("blake3", "slot-0"),
                value_before: Some(RuntimeValue::hash("blake3", "value-0")),
                value_after: None,
                policy: RuntimeValuePolicy::HashOnly,
            }),
            TraceFact::MemoryAccess(MemoryAccessFact {
                access: memory,
                step: step1.clone(),
                kind: MemoryAccessKind::Write,
                offset: 32,
                length: 64,
                value: Some(RuntimeValue::hash("blake3", "mem")),
                policy: RuntimeValuePolicy::HashOnly,
            }),
            TraceFact::Call(CallFact {
                call,
                step: step1,
                kind: RuntimeCallKind::Call,
                caller: Some("0x0000000000000000000000000000000000000001".to_string()),
                callee: Some("0x0000000000000000000000000000000000000002".to_string()),
                value: Some(RuntimeValue::redacted()),
                gas_requested: Some(12),
                gas_used: Some(7),
                success: Some(true),
                callsite_instruction: Some(zext),
                policy: RuntimeValuePolicy::HashOnly,
            }),
            TraceFact::Revert(RevertFact {
                revert,
                step: step2,
                reason: Some("demo revert".to_string()),
                data: RuntimeValue::hash("blake3", "revert"),
                policy: RuntimeValuePolicy::HashOnly,
            }),
        ]);

        TraceIntrospectionService::new(TraceSnapshot::new(bundle).unwrap())
    }

    fn ambiguous_local_service() -> (TraceIntrospectionService, OriginExportKey, OriginExportKey) {
        let first = key("runtime.local", "demo:func_a", "body:0:local:b");
        let second = key("runtime.local", "demo:func_b", "body:0:local:b");
        let facts = vec![
            node(first.clone()),
            node(second.clone()),
            TraceFact::DisplayName(DisplayNameFact::new(
                first.clone(),
                DisplayNameKind::SourceLocal,
                "b",
            )),
            TraceFact::DisplayName(DisplayNameFact::new(
                second.clone(),
                DisplayNameKind::SourceLocal,
                "b",
            )),
            TraceFact::Storage(StorageFact::new(
                second.clone(),
                CompilerPhase::Backend,
                StorageLocation::StackSlot { offset: 32 },
                StorageReason::FrameSlot,
            )),
        ];
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::fixture(
                "abc123",
                "riscv64-demo",
                vec!["fe".to_string()],
                "fib_demo.fe",
                vec!["function=Fib.recv".to_string()],
                "ambiguous-local-query-test",
            ),
            facts,
        ))
        .unwrap();
        (TraceIntrospectionService::new(snapshot), first, second)
    }

    fn index_only_instruction_service() -> TraceIntrospectionService {
        let function = key("function", "demo", "recv");
        let inst = key("bytecode.inst", "demo", "inst:7");
        let facts = vec![
            node(function.clone()),
            node(inst.clone()),
            TraceFact::Instruction(InstructionFact::new(inst, function, 7, "add")),
        ];
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::fixture(
                "abc123",
                "evm-demo",
                vec!["fe".to_string()],
                "fib_demo.fe",
                vec!["function=Fib.recv".to_string()],
                "index-only-pc-query-test",
            ),
            facts,
        ))
        .unwrap();
        TraceIntrospectionService::new(snapshot)
    }

    #[test]
    fn loop_cost_report_counts_categories_and_evidence() {
        let report = demo_service()
            .loop_cost(LoopCostRequest::default())
            .unwrap();

        assert!(report.available);
        assert_eq!(report.summary.total_instructions, 2);
        assert_eq!(report.summary.zero_extends, 1);
        assert_eq!(report.summary.stack_loads, 1);
        assert_eq!(
            report.repeated_zero_extends[0].reason.as_deref(),
            Some("test reason")
        );
    }

    #[test]
    fn loop_contents_report_groups_instructions_by_loop_block() {
        let report = demo_service()
            .loop_contents(LoopContentsRequest::default())
            .unwrap();

        assert!(report.available);
        assert_eq!(report.instructions.len(), 2);
        assert_eq!(report.blocks.len(), 1);
        assert_eq!(report.blocks[0].role, "header");
        assert_eq!(report.blocks[0].instructions.len(), 2);
    }

    #[test]
    fn explain_local_report_uses_storage_and_instruction_edges() {
        let report = demo_service()
            .explain_local(ExplainLocalRequest {
                local: "b".to_string(),
                local_key: None,
            })
            .unwrap();

        assert!(report.available);
        assert_eq!(report.storage_history.len(), 2);
        assert_eq!(report.related_instructions.len(), 2);
        assert_eq!(report.zero_extends.len(), 1);
    }

    #[test]
    fn explain_local_display_name_ambiguity_fails_closed() {
        let (service, first, second) = ambiguous_local_service();
        let report = service
            .explain_local(ExplainLocalRequest {
                local: "b".to_string(),
                local_key: None,
            })
            .unwrap();

        assert!(!report.available);
        assert_eq!(report.candidate_local_keys, vec![first, second]);
        assert!(
            report
                .unavailable_reason
                .as_deref()
                .unwrap()
                .contains("ambiguous")
        );
    }

    #[test]
    fn explain_local_exact_key_disambiguates_display_name() {
        let (service, _first, second) = ambiguous_local_service();
        let report = service
            .explain_local(ExplainLocalRequest {
                local: "b".to_string(),
                local_key: Some(second.clone()),
            })
            .unwrap();

        assert!(report.available);
        assert_eq!(report.local_key, Some(second));
        assert_eq!(report.storage_history.len(), 1);
    }

    #[test]
    fn gas_breakdown_reports_static_opcode_rows() {
        let report = demo_service()
            .gas_breakdown(super::GasBreakdownRequest::default())
            .unwrap();

        assert!(report.available);
        assert_eq!(report.total_gas, Some(6));
        assert_eq!(report.schedule, "cancun");
        assert_eq!(report.rows.len(), 2);
    }

    #[test]
    fn explain_pc_reports_instruction_source_and_gas() {
        let report = demo_service()
            .explain_pc(ExplainPcRequest { pc: 0 })
            .unwrap();

        assert!(report.available);
        assert_eq!(report.instruction.unwrap().mnemonic, "lw");
        assert_eq!(report.static_gas, Some(3));
        assert_eq!(report.primary_source.unwrap().label, "fib.fe:2:8-2:9");
        assert!(
            report
                .source_candidates
                .iter()
                .all(|source| source.origin.kind() != "code.object")
        );
    }

    #[test]
    fn explain_pc_does_not_fallback_to_instruction_index() {
        let report = index_only_instruction_service()
            .explain_pc(ExplainPcRequest { pc: 7 })
            .unwrap();

        assert!(!report.available);
        assert_eq!(report.instruction, None);
        assert!(
            report
                .unavailable_reason
                .as_deref()
                .unwrap()
                .contains("bytecode.pc")
        );
    }

    #[test]
    fn gas_by_source_groups_static_gas_by_source_span() {
        let report = demo_service()
            .gas_by_source(GasBySourceRequest::default())
            .unwrap();

        assert_eq!(report.total_gas, 6);
        assert_eq!(report.rows[0].label, "fib.fe:2:8-2:9");
        assert_eq!(report.rows[0].gas, 6);
        assert_eq!(report.rows[0].instruction_count, 2);
    }

    #[test]
    fn gas_by_source_json_does_not_double_count_legacy_gas_view() {
        let report = demo_service()
            .gas_by_source(GasBySourceRequest::default())
            .unwrap();
        let json = serde_json::to_value(&report).unwrap();

        assert_eq!(json["schedule"], "cancun");
        assert_eq!(json["policy"], "exclusive-primary");
        assert_eq!(json["total_gas"], 6);
        assert_eq!(json["rows"][0]["label"], "fib.fe:2:8-2:9");
        assert_eq!(json["rows"][0]["gas"], 6);
        assert_eq!(json["rows"][0]["instruction_count"], 2);
    }

    #[test]
    fn call_attribution_policies_are_gated_until_call_facts_exist() {
        for policy in [
            super::GasAttributionPolicy::CallInclusive,
            super::GasAttributionPolicy::CallExclusive,
        ] {
            let err = demo_service()
                .gas_by_source(GasBySourceRequest {
                    policy,
                    ..Default::default()
                })
                .unwrap_err();

            assert!(
                err.to_string()
                    .contains("requires call graph and inline context facts")
            );
        }
    }

    #[test]
    fn bytecode_size_by_source_groups_extents_by_source_span() {
        let report = demo_service()
            .bytecode_size_by_source(super::BytecodeSizeBySourceRequest::default())
            .unwrap();

        assert_eq!(report.total_bytes, 3);
        assert_eq!(report.policy, "exclusive-primary");
        assert_eq!(report.rows[0].label, "fib.fe:2:8-2:9");
        assert_eq!(report.rows[0].bytes, 3);
        assert_eq!(report.rows[0].instruction_count, 2);
    }

    #[test]
    fn dynamic_gas_by_source_joins_steps_to_instruction_sources() {
        let report = demo_service()
            .dynamic_gas_by_source(DynamicGasBySourceRequest {
                trace_id: Some("tx:1".to_string()),
                ..Default::default()
            })
            .unwrap();

        assert_eq!(report.total_gas, 10);
        assert_eq!(report.unattributed_steps, 0);
        assert_eq!(report.rows.len(), 1);
        assert_eq!(report.rows[0].label, "fib.fe:2:8-2:9");
        assert_eq!(report.rows[0].gas, 10);
    }

    #[test]
    fn runtime_gas_by_source_uses_execution_steps() {
        let report = runtime_demo_service()
            .runtime_gas_by_source(RuntimeGasBySourceRequest::default())
            .unwrap();

        assert_eq!(report.policy, "runtime-step-exclusive");
        assert_eq!(report.runtime.runtime_sources, vec!["RevmInspector"]);
        assert_eq!(report.total_gas, 45);
        assert_eq!(report.unattributed_steps, 0);
        assert_eq!(report.rows.len(), 1);
        assert_eq!(report.rows[0].label, "fib.fe:2:8-2:9");
        assert_eq!(report.rows[0].gas, 45);
    }

    #[test]
    fn runtime_storage_reports_group_by_source_and_slot() {
        let service = runtime_demo_service();
        let writes = service
            .storage_writes_by_source(RuntimeTraceFilterRequest::default())
            .unwrap();

        assert_eq!(writes.total_writes, 1);
        assert_eq!(writes.total_gas, 20);
        assert_eq!(writes.rows[0].label, "fib.fe:2:8-2:9");
        assert_eq!(writes.rows[0].slots, vec!["blake3:slot-0"]);

        let slots = service
            .storage_accesses_by_slot(StorageAccessesBySlotRequest::default())
            .unwrap();

        assert_eq!(slots.total_reads, 1);
        assert_eq!(slots.total_writes, 1);
        assert_eq!(slots.rows[0].slot, "blake3:slot-0");
        assert_eq!(slots.rows[0].sources[0].label, "fib.fe:2:8-2:9");
    }

    #[test]
    fn runtime_call_memory_revert_and_hot_path_reports_are_derived() {
        let service = runtime_demo_service();
        let calls = service
            .call_cost_by_callsite(RuntimeTraceFilterRequest::default())
            .unwrap();
        assert_call_cost(&calls);

        let memory = service
            .memory_growth_by_source(RuntimeTraceFilterRequest::default())
            .unwrap();
        assert_eq!(memory.total_accesses, 1);
        assert_eq!(memory.max_end_offset, 96);
        assert_eq!(memory.rows[0].bytes_touched, 64);

        let reverts = service
            .revert_attribution(RuntimeTraceFilterRequest::default())
            .unwrap();
        assert_eq!(reverts.total_reverts, 1);
        assert_eq!(reverts.rows[0].reason.as_deref(), Some("demo revert"));
        assert_eq!(
            reverts.rows[0].source.as_ref().unwrap().label,
            "fib.fe:2:8-2:9"
        );

        let hot = service
            .hot_path_by_iteration(RuntimeTraceFilterRequest::default())
            .unwrap();
        assert_eq!(hot.total_steps, 3);
        assert_eq!(hot.total_gas, 45);
        assert_eq!(hot.rows[0].pc, 0);
        assert_eq!(hot.rows[0].executions, 2);
        assert_eq!(hot.rows[0].gas, 35);
    }

    fn assert_call_cost(report: &CallCostByCallsiteReport) {
        assert_eq!(report.policy, "call-inclusive-frame");
        assert_eq!(report.total_calls, 1);
        assert_eq!(report.total_gas_used, 7);
        assert_eq!(report.rows[0].gas_requested, Some(12));
        assert_eq!(
            report.rows[0].source.as_ref().unwrap().label,
            "fib.fe:2:8-2:9"
        );
    }

    #[test]
    fn value_flow_at_pc_reports_stack_storage_and_memory_events() {
        let service = runtime_demo_service();
        let pc0 = service
            .value_flow_at_pc(ValueFlowAtPcRequest {
                pc: 0,
                code_object: None,
                trace_id: None,
            })
            .unwrap();

        assert_eq!(pc0.rows.len(), 2);
        assert_eq!(pc0.rows[0].stack_top, vec!["blake3:top"]);
        assert_eq!(pc0.rows[0].storage_accesses[0].location, "blake3:slot-0");

        let pc1 = service
            .value_flow_at_pc(ValueFlowAtPcRequest {
                pc: 1,
                code_object: None,
                trace_id: None,
            })
            .unwrap();
        assert_eq!(pc1.rows[0].memory_accesses[0].location, "mem[32..96)");
    }

    #[test]
    fn gas_to_source_combines_static_and_dynamic_attribution() {
        let report = demo_service()
            .gas_to_source(GasToSourceRequest {
                trace_id: Some("tx:1".to_string()),
                ..Default::default()
            })
            .unwrap();

        assert_eq!(report.static_gas, 6);
        assert_eq!(report.dynamic_gas, 10);
        assert_eq!(report.total_gas, 16);
        assert_eq!(report.policy, "exclusive-primary");
        assert_eq!(report.schedule, "cancun");
        assert_eq!(report.rows.len(), 1);
        assert_eq!(report.rows[0].total_gas, 16);
    }

    #[test]
    fn optimized_code_honesty_reports_ambiguous_and_synthetic_work() {
        let report = demo_service()
            .optimized_code_honesty(OptimizedCodeHonestyRequest::default())
            .unwrap();

        assert_eq!(report.policy, "precision-honest-v1");
        assert_eq!(report.ambiguous_instructions.len(), 1);
        assert_eq!(report.ambiguous_instructions[0].source_candidates.len(), 2);
        assert_eq!(report.synthetic_overheads.len(), 1);
        assert_eq!(report.synthetic_overheads[0].dynamic_gas, 5);
        assert_eq!(
            report.synthetic_overheads[0].cause_sources[0].label,
            "fib.fe:2:8-2:9"
        );
    }

    #[test]
    fn attribution_audit_splits_exact_structural_and_contextual_edges() {
        let report = demo_service().attribution_audit().unwrap();

        assert_eq!(report.total_bytecode_pcs, 4);
        assert_eq!(report.source_exact_pcs, 2);
        assert_eq!(report.source_ambiguous_pcs, 1);
        assert_eq!(report.unmapped_pcs, 1);
        assert_eq!(report.optimized_sonatina_linked_pcs, 2);
        assert_eq!(report.prepared_linked_pcs, 0);
        assert_eq!(report.missing_optimized_to_prepared_lineage_pcs, 0);
        assert!(report.direct_edges_by_class.iter().any(|row| {
            row.traversal_class == OriginEdgeTraversalClass::ExactAttribution && row.count >= 2
        }));
        assert!(report.direct_edges_by_class.iter().any(|row| {
            row.traversal_class == OriginEdgeTraversalClass::Structural && row.count >= 2
        }));
        assert!(
            report
                .source_lines
                .iter()
                .any(|row| { row.label == "fib.fe:2:8-2:9" && row.origin_kind == "hir.expr" })
        );
        let missing_links = report.missing_links.as_ref().unwrap();
        assert_eq!(
            missing_links.schema_version,
            MISSING_LINK_AUDIT_SCHEMA_VERSION
        );
        assert_eq!(missing_links.summary.status, LinkOverallStatus::Invalid);
        assert_eq!(missing_links.summary.invalid_count, 2);
        assert_eq!(
            missing_links.summary.top_blockers,
            vec![LinkBoundaryKind::PostOptToPrepared]
        );
        assert!(
            missing_links
                .invalid
                .iter()
                .all(|entry| entry.issue_code == LinkIssueCode::PreparedKeyedAsPostopt)
        );
        assert!(missing_links.clusters.is_empty());
    }

    #[test]
    fn attribution_audit_reports_prepared_without_postopt_lineage() {
        let function = key("function", "demo", "recv");
        let source_file = key("source.file", "demo", "demo.fe");
        let hir = key("hir.expr", "demo", "expr:source");
        let postopt = key("sonatina.postopt.inst", "demo", "inst:postopt");
        let prepared_linked = key("sonatina.evm.prepared.inst", "demo", "inst:prepared-linked");
        let prepared_missing = key(
            "sonatina.evm.prepared.inst",
            "demo",
            "inst:prepared-missing",
        );
        let pc_linked = key("bytecode.pc", "demo", "pc:0");
        let pc_missing = key("bytecode.pc", "demo", "pc:1");
        let facts = vec![
            node(function.clone()),
            node(source_file.clone()),
            node(hir.clone()),
            node(postopt.clone()),
            node(prepared_linked.clone()),
            node(prepared_missing.clone()),
            node(pc_linked.clone()),
            node(pc_missing.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir.clone(),
                source_file,
                0,
                1,
                1,
                1,
                1,
                2,
            )),
            TraceFact::Instruction(InstructionFact::new(
                pc_linked.clone(),
                function.clone(),
                0,
                "ADD",
            )),
            TraceFact::Instruction(InstructionFact::new(
                pc_missing.clone(),
                function,
                1,
                "DUP1",
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                pc_linked,
                prepared_linked.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                prepared_linked,
                postopt.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                postopt,
                hir,
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPostOpt),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                pc_missing.clone(),
                prepared_missing.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
        ];
        let snapshot = snapshot(facts);
        let report = TraceIntrospectionService::new(snapshot)
            .attribution_audit()
            .unwrap();

        assert_eq!(report.total_bytecode_pcs, 2);
        assert_eq!(report.source_exact_pcs, 1);
        assert_eq!(report.source_ambiguous_pcs, 0);
        assert_eq!(report.unmapped_pcs, 1);
        assert_eq!(report.optimized_sonatina_linked_pcs, 1);
        assert_eq!(report.prepared_linked_pcs, 2);
        assert_eq!(report.missing_optimized_to_prepared_lineage_pcs, 1);
        assert_eq!(report.lineage_gaps.len(), 1);
        assert_eq!(report.lineage_gaps[0].bytecode_pc, pc_missing);
        assert_eq!(report.lineage_gaps[0].prepared_origin, prepared_missing);
        assert_eq!(
            report.lineage_gaps[0].reason,
            "missing_optimized_to_prepared_lineage"
        );
        assert!(
            report
                .missing_lineage_targets
                .iter()
                .any(|row| row.target == report.lineage_gaps[0].prepared_origin && row.count == 1)
        );

        let missing_links = report.missing_links.as_ref().unwrap();
        assert_eq!(missing_links.summary.status, LinkOverallStatus::Warning);
        assert_eq!(
            missing_links.summary.top_blockers,
            vec![LinkBoundaryKind::PostOptToPrepared]
        );
        assert_eq!(missing_links.summary.missing_required_count, 1);
        assert_eq!(missing_links.summary.prepared_linked_bytecode_pcs, 2);
        assert_eq!(missing_links.gaps.len(), 1);
        assert_eq!(
            missing_links.gaps[0].issue_code,
            LinkIssueCode::MissingOptimizedToPreparedLineage
        );
        assert_eq!(
            missing_links.gaps[0].required_evidence[0].kind,
            RequiredEvidenceKind::PreparedLineageFact
        );
        assert_eq!(missing_links.clusters.len(), 1);
        assert_eq!(
            missing_links.clusters[0].boundary,
            LinkBoundaryKind::PostOptToPrepared
        );
        assert_eq!(
            missing_links.boundary_summaries[0]
                .status_counts
                .get(&LinkStatus::MissingRequired),
            Some(&1)
        );
    }

    #[test]
    fn variables_at_pc_reports_location_ranges() {
        let report = demo_service()
            .variables_at_pc(VariablesAtPcRequest {
                pc: 1,
                code_object: None,
            })
            .unwrap();

        assert_eq!(report.variables.len(), 1);
        assert_eq!(report.variables[0].name, "b");
        assert!(report.variables[0].location.contains("StackSlot"));
    }

    #[test]
    fn live_http_request_is_typed_and_defaults_gas_schedule() {
        let request: TraceQueryHttpRequest = serde_json::from_str(
            r#"{
                "auth_token": "token",
                "uri": "file:///tmp/fib.fe",
                "kind": "gas_breakdown"
            }"#,
        )
        .unwrap();

        assert_eq!(request.auth_token, "token");
        assert_eq!(request.uri, "file:///tmp/fib.fe");
        assert!(matches!(
            request.query,
            TraceQueryRequest::GasBreakdown { ref schedule } if schedule == "cancun"
        ));
    }

    #[test]
    fn live_http_response_reports_cache_and_duration_metadata() {
        let report = run_trace_query(&demo_service(), TraceQueryRequest::loop_cost()).unwrap();
        let response = TraceQueryHttpResponse::Ok {
            report,
            cache_hit: true,
            query_duration_ms: 7,
        };
        let json = serde_json::to_value(&response).unwrap();

        assert_eq!(json["status"], "ok");
        assert_eq!(json["cache_hit"], true);
        assert_eq!(json["query_duration_ms"], 7);

        let legacy: TraceQueryHttpResponse = serde_json::from_str(
            r#"{
                "status": "error",
                "reason": "not available"
            }"#,
        )
        .unwrap();
        assert_eq!(
            legacy,
            TraceQueryHttpResponse::Error {
                reason: "not available".to_string(),
                cache_hit: false,
                query_duration_ms: 0,
            }
        );
    }

    #[test]
    fn trace_workbench_report_projection_uses_shared_trace_query_reports() {
        let service = demo_service();
        let projection = trace_workbench_report_projection(
            &service,
            service.snapshot(),
            TraceWorkbenchProjectionRequest {
                input_path: "file:///tmp/fib.fe".to_string(),
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                source_text: Some("fn fib() {\n  b + c\n}\n".to_string()),
                related_source_texts: BTreeMap::new(),
                document_version: Some(3),
                query_duration_ms: 9,
                compiler_commit: "test".to_string(),
                data_source: "lsp-live".to_string(),
            },
        );

        assert_eq!(projection["revision"]["id"], 3);
        assert_eq!(projection["metadata"]["data_source"], "lsp-live");
        assert_eq!(projection["metadata"]["flags"][0], "source=lsp-live");
        assert_eq!(projection["provenance"]["source_to_optimized"], "available");
        assert_eq!(projection["parity_summary"]["target"], "evm");
        assert_eq!(projection["parity_summary"]["opt_level"], "O2");
        assert_eq!(
            projection["parity_summary"]["source_to_optimized"],
            projection["provenance"]["source_to_optimized"]
        );
        assert_eq!(
            projection["parity_summary"]["bytecode"]["total_pcs"],
            projection["attribution_audit"]["total_bytecode_pcs"]
        );
        assert_eq!(
            projection["parity_summary"]["missing_link_status"],
            projection["attribution_audit"]["missing_links"]["summary"]["status"]
        );
        assert_eq!(
            projection["parity_summary"]["edge_class_counts"]["exact_attribution"],
            7
        );
        assert!(projection["attribution_audit"].is_object());
        assert!(projection["duplicate_shapes"].is_object());
        assert_eq!(projection["source"]["lines"].as_array().unwrap().len(), 3);
        assert_eq!(
            projection["source"]["lines"][1]["row_id"],
            "source-main-line-2"
        );
        assert!(
            projection["panels"]
                .as_array()
                .unwrap()
                .iter()
                .any(|panel| {
                    panel["id"] == "bytecode"
                        && panel["rows"]
                            .as_array()
                            .is_some_and(|rows| !rows.is_empty())
                })
        );
        let bytecode_panel = projection["panels"]
            .as_array()
            .unwrap()
            .iter()
            .find(|panel| panel["id"] == "bytecode")
            .unwrap();
        assert!(
            bytecode_panel["rows"][0]["row_id"]
                .as_str()
                .is_some_and(|row_id| row_id.starts_with("origin-"))
        );
        assert_eq!(
            projection["indexes"]["source_lines"]["main:2"],
            "source-main-line-2"
        );
        assert_eq!(
            projection["source"]["lines"][1]["stable_identities"][0]["kind"],
            "source_line"
        );
        assert_eq!(
            projection["source"]["lines"][1]["stable_identities"][0]["value"],
            "main:2"
        );
        assert_eq!(
            projection["indexes"]["stable_identities"]["source_line:main:2"][0],
            "source-main-line-2"
        );
        assert!(
            projection["indexes"]["source_intervals"]
                .as_array()
                .unwrap()
                .iter()
                .any(|interval| {
                    interval["source"] == "main"
                        && interval["start_line"] == 2
                        && interval["end_line"] == 2
                        && interval["row_id"] == "source-main-line-2"
                })
        );
        let first_bytecode_row = &bytecode_panel["rows"][0];
        let first_bytecode_key = first_bytecode_row["key"].as_str().unwrap();
        let first_bytecode_row_id = first_bytecode_row["row_id"].as_str().unwrap();
        assert_eq!(
            projection["indexes"]["origin_to_rows"][first_bytecode_key][0],
            first_bytecode_row_id
        );
        assert!(
            first_bytecode_row["stable_identities"]
                .as_array()
                .unwrap()
                .iter()
                .any(|identity| {
                    identity["kind"] == "origin" && identity["value"] == first_bytecode_key
                })
        );
        assert!(
            first_bytecode_row["stable_identities"]
                .as_array()
                .unwrap()
                .iter()
                .any(|identity| identity["kind"] == "bytecode_pc")
        );
        assert!(
            projection["indexes"]["pc_intervals"]
                .as_array()
                .unwrap()
                .iter()
                .any(|interval| {
                    interval["row_id"] == first_bytecode_row_id
                        && interval["origin"] == first_bytecode_key
                        && interval["pc_start"].as_u64().is_some()
                        && interval["pc_end"].as_u64().is_some_and(|end| end > 0)
                })
        );
        assert_eq!(
            projection["selection_remap"]["strategy_order"]
                .as_array()
                .unwrap()
                .iter()
                .map(serde_json::Value::as_str)
                .collect::<Option<Vec<_>>>()
                .unwrap(),
            vec![
                "origin",
                "bytecode_pc",
                "source_line",
                "component",
                "nearest_row"
            ]
        );
        assert!(
            projection["selection_remap"]["indexes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|index| index == "stable_identities")
        );
        assert!(
            projection["selection_remap"]["indexes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|index| index == "source_intervals")
        );
        assert!(
            projection["selection_remap"]["indexes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|index| index == "pc_intervals")
        );
        assert_eq!(
            projection["notes"][2],
            "The browser does not compute provenance or graph reachability."
        );
        let manifest = trace_workbench_manifest(&projection);
        assert_eq!(manifest.revision, 3);
        assert!(manifest.root_digest.starts_with("blake3:"));
        assert!(manifest.summary_digest.starts_with("blake3:"));
        assert!(manifest.source_digest.starts_with("blake3:"));
        assert!(manifest.indexes_digest.starts_with("blake3:"));
        assert!(manifest.reports.contains_key("attribution"));
        assert!(manifest.reports.contains_key("duplicate_shapes"));
    }

    #[test]
    fn trace_workbench_provenance_does_not_claim_preopt_as_optimized() {
        let closure_set = crate::origin_closure::OriginClosureSet {
            closures: vec![crate::origin_closure::OriginClosure {
                class_name: "trace-c-demo".to_string(),
                label: "demo".to_string(),
                root_key: "hir.expr:demo:expr:0".to_string(),
                keys: vec![
                    "hir.expr:demo:expr:0".to_string(),
                    "sonatina.preopt.inst:demo:inst:0".to_string(),
                ],
                generated_keys: Vec::new(),
                contextual_keys: Vec::new(),
                structural_keys: Vec::new(),
                counts: crate::origin_closure::OriginClosureCounts {
                    hir: 1,
                    mir: 0,
                    sonatina_pre: 1,
                    sonatina_post: 0,
                    sonatina_prepared: 0,
                    bytecode: 0,
                },
                traversal: crate::origin_closure::OriginClosureTraversalSummary {
                    mode: "test".to_string(),
                    max_depth: 0,
                    max_nodes: 0,
                    truncated: false,
                    truncation_reason: None,
                    skipped_hubs: Vec::new(),
                },
                gap: None,
                edges: Vec::new(),
                source_spans: Vec::new(),
            }],
            edge_count: 1,
            instruction_count: 1,
            source_span_count: 0,
        };

        let status = super::trace_workbench_provenance_status(Some(&closure_set), 0, 0, 0);

        assert_eq!(status.source_to_optimized, "missing");
        assert_eq!(status.optimized_to_prepared, "missing");
        assert_eq!(status.prepared_to_bytecode, "missing");
    }

    #[test]
    fn trace_workbench_trace_profile_marks_preopt_only_model_partial() {
        let snapshot = snapshot(vec![
            node(key("sonatina.preopt.inst", "demo", "inst:0")),
            node(key("bytecode.pc", "demo", "pc:0")),
        ]);

        let profile = super::trace_workbench_trace_profile(&snapshot);

        assert_eq!(profile.profile, "partial_preopt");
        assert!(profile.has_sonatina_preopt);
        assert!(!profile.has_sonatina_postopt);
        assert!(profile.has_bytecode);
    }

    #[test]
    fn trace_workbench_compact_mir_text_removes_debug_noise() {
        let text = "RLocalId(7) = call RuntimeInstance(Id(1fc03), PhantomData<&salsa::tracked_struct::Value<fe_mir::instance::runtime::RuntimeInstance<'_>>>)([RLocalId(6)])";

        assert_eq!(
            trace_workbench_compact_mir_text(text),
            "%7 = call runtime#1fc03([%6])"
        );
    }

    #[test]
    fn trace_workbench_compact_mir_text_simplifies_int_literals() {
        let text = "RLocalId(22) = const Int { bits: 256, signed: false, words: [32] }";
        let signed = "RLocalId(17) = const Int { bits: 32, signed: true, words: [0] }";

        assert_eq!(
            trace_workbench_compact_mir_text(text),
            "%22 = const u256 32"
        );
        assert_eq!(
            trace_workbench_compact_mir_text(signed),
            "%17 = const i32 0"
        );
    }

    #[test]
    fn trace_workbench_hir_rows_use_source_snippets() {
        let source_file = key("source.file", "file:///demo.fe", "file:0");
        let hir = key("hir.expr", "file:///demo.fe", "expr:0");
        let snapshot = snapshot(vec![
            node(source_file.clone()),
            node(hir.clone()),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir.clone(),
                source_file,
                0,
                9,
                1,
                1,
                1,
                10,
            )),
        ]);
        let index = TraceWorkbenchProjectionIndex::new(
            &snapshot,
            "file:///demo.fe",
            Some("let x = y\n"),
            &BTreeMap::new(),
        );

        assert_eq!(
            trace_workbench_compact_origin_text(&hir, &index),
            "let x = y"
        );
    }

    #[test]
    fn trace_workbench_related_hir_rows_use_related_source_snippets() {
        let source_file = key("source.file", "file:///std/lib.fe", "file:0");
        let hir = key("hir.expr", "file:///std/lib.fe", "expr:0");
        let snapshot = snapshot(vec![
            node(source_file.clone()),
            node(hir.clone()),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir.clone(),
                source_file.clone(),
                4,
                7,
                1,
                5,
                1,
                8,
            )),
        ]);
        let mut related = BTreeMap::new();
        related.insert(source_file.canonical_storage_key(), "abc def\n".to_string());
        let index =
            TraceWorkbenchProjectionIndex::new(&snapshot, "file:///demo.fe", None, &related);

        assert_eq!(trace_workbench_compact_origin_text(&hir, &index), "def");
    }

    #[test]
    fn trace_workbench_rows_use_display_name_facts() {
        let hir = key("hir.expr", "demo", "expr:0");
        let mir_local = key("runtime.local", "demo", "local:0");
        let snapshot = snapshot(vec![
            node(hir.clone()),
            node(mir_local.clone()),
            TraceFact::DisplayName(DisplayNameFact::new(
                hir.clone(),
                DisplayNameKind::SourceLocal,
                "balance + amount",
            )),
            TraceFact::DisplayName(DisplayNameFact::new(
                mir_local.clone(),
                DisplayNameKind::SourceLocal,
                "balance",
            )),
        ]);
        let index =
            TraceWorkbenchProjectionIndex::new(&snapshot, "demo.fe", None, &BTreeMap::new());

        assert_eq!(
            trace_workbench_compact_origin_text(&hir, &index),
            "balance + amount"
        );
        assert_eq!(
            trace_workbench_compact_origin_text(&mir_local, &index),
            "balance"
        );
    }

    #[test]
    fn trace_workbench_origin_fallback_text_is_representation_first() {
        let hir = key("hir.expr", "demo", "expr:0");
        let postopt = key(
            "sonatina.postopt.inst",
            "demo",
            "function:FuncRef(1):inst:InstId(122)",
        );
        let generated_mir = key(
            "runtime.stmt",
            "runtime-instance:semantic:__synthetic_wrapper",
            "block:0:stmt:1",
        );
        let snapshot = snapshot(vec![
            node(hir.clone()),
            node(postopt.clone()),
            node(generated_mir.clone()),
        ]);
        let index =
            TraceWorkbenchProjectionIndex::new(&snapshot, "demo.fe", None, &BTreeMap::new());

        assert_eq!(
            trace_workbench_compact_origin_text(&hir, &index),
            "HIR expression"
        );
        assert_eq!(
            trace_workbench_compact_origin_text(&postopt, &index),
            "optimized instruction"
        );
        assert_eq!(
            trace_workbench_compact_origin_text(&generated_mir, &index),
            "generated statement"
        );
    }

    #[test]
    fn trace_workbench_source_projection_inventories_related_sources() {
        let input_file = key("source.file", "file:///main.fe", "file:main");
        let lib_file = key("source.file", "file:///std/lib.fe", "file:std");
        let lib_hir = key("hir.expr", "file:///std/lib.fe", "expr:abi");
        let snapshot = snapshot(vec![
            node(input_file),
            node(lib_file.clone()),
            node(lib_hir.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                lib_file.clone(),
                "file:///std/lib.fe",
                "std/lib.fe",
                "blake3:std",
                None,
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                lib_hir,
                lib_file.clone(),
                0,
                12,
                7,
                1,
                8,
                3,
            )),
        ]);
        let mut related_source_texts = BTreeMap::new();
        related_source_texts.insert(
            lib_file.canonical_storage_key(),
            "one\ntwo\nthree\nfour\nfive\nsix\nabi line\nreturn line\n".to_string(),
        );
        let source = trace_workbench_source_projection(
            "file:///main.fe",
            Some("fn main() {}\n"),
            &related_source_texts,
            &snapshot,
            &BTreeMap::new(),
        );
        let related = source["related_sources"].as_array().unwrap();

        assert_eq!(related.len(), 1);
        assert_eq!(related[0]["display_name"], "std/lib.fe");
        assert_eq!(related[0]["summary"], "referenced lines 7-8");
        assert_eq!(related[0]["source_text_available"], true);
        assert_eq!(related[0]["lines"][0]["number"], 7);
        assert_eq!(related[0]["lines"][0]["text"], "abi line");
    }

    #[test]
    fn trace_workbench_broad_source_spans_do_not_badge_each_line() {
        let source_file = key("source.file", "file:///main.fe", "file:main");
        let hir = key("hir.expr", "file:///main.fe", "expr:function-body");
        let snapshot = snapshot(vec![
            node(source_file.clone()),
            node(hir.clone()),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir.clone(),
                source_file,
                0,
                28,
                1,
                1,
                3,
                2,
            )),
        ]);
        let mut classes = BTreeMap::new();
        classes.insert(
            hir.canonical_storage_key(),
            vec!["generated-c-broad".to_string()],
        );

        let lines = trace_workbench_source_lines(
            "file:///main.fe",
            "fn main() {\n  // comment\n}\n",
            &snapshot,
            &classes,
        );

        assert_eq!(lines[1].classes, vec!["generated-c-broad"]);
        assert!(lines[1].suppress_rail_status);
        assert_eq!(lines[1].display_status, None);
    }

    #[test]
    fn trace_workbench_panel_sort_uses_natural_numeric_origin_order() {
        let mut locals = vec![
            "function:FuncRef(1):inst:InstId(663)",
            "function:FuncRef(1):inst:InstId(122)",
            "function:FuncRef(1):inst:InstId(68)",
            "function:FuncRef(1):inst:InstId(9)",
        ];
        locals.sort_by(|left, right| trace_workbench_natural_key_cmp(left, right));

        assert_eq!(
            locals,
            vec![
                "function:FuncRef(1):inst:InstId(9)",
                "function:FuncRef(1):inst:InstId(68)",
                "function:FuncRef(1):inst:InstId(122)",
                "function:FuncRef(1):inst:InstId(663)",
            ]
        );
        assert_eq!(
            trace_workbench_natural_key_cmp("block:0:stmt:9", "block:0:stmt:10"),
            std::cmp::Ordering::Less
        );
    }

    #[test]
    fn trace_workbench_rows_surface_missing_optimized_to_prepared_lineage() {
        let bytecode = key("bytecode.pc", "demo", "pc:68");
        let mut missing_lineage = TraceWorkbenchMissingLineageIndex::default();
        missing_lineage
            .origins
            .insert(bytecode.canonical_storage_key());
        let classes = vec!["prepared-c-demo".to_string()];

        assert_eq!(
            trace_workbench_status_for_row(
                &bytecode,
                TraceWorkbenchPaneRowKind::Instruction,
                &classes,
                &missing_lineage,
            ),
            Some(TraceWorkbenchDisplayStatus::MissingOptimizedToPrepared)
        );
        assert_eq!(
            trace_workbench_status_for_row(
                &bytecode,
                TraceWorkbenchPaneRowKind::Instruction,
                &classes,
                &TraceWorkbenchMissingLineageIndex::default(),
            ),
            Some(TraceWorkbenchDisplayStatus::PreparedLinked)
        );
    }

    #[test]
    fn typed_query_dispatch_returns_matching_report_variant() {
        let service = demo_service();
        let report = run_trace_query(&service, TraceQueryRequest::loop_cost()).unwrap();

        assert!(matches!(report, TraceQueryReport::LoopCost(_)));
    }

    #[test]
    fn typed_query_dispatch_returns_new_report_variants() {
        let service = demo_service();

        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::explain_pc(0)).unwrap(),
            TraceQueryReport::ExplainPc(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::loop_contents()).unwrap(),
            TraceQueryReport::LoopContents(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::gas_by_source("cancun")).unwrap(),
            TraceQueryReport::GasBySource(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::bytecode_size_by_source()).unwrap(),
            TraceQueryReport::BytecodeSizeBySource(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::dynamic_gas_by_source()).unwrap(),
            TraceQueryReport::DynamicGasBySource(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::gas_to_source("cancun")).unwrap(),
            TraceQueryReport::GasToSource(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::optimized_code_honesty()).unwrap(),
            TraceQueryReport::OptimizedCodeHonesty(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::attribution_audit()).unwrap(),
            TraceQueryReport::AttributionAudit(_)
        ));
        assert!(matches!(
            run_trace_query(&service, TraceQueryRequest::variables_at_pc(0)).unwrap(),
            TraceQueryReport::VariablesAtPc(_)
        ));

        let runtime_service = runtime_demo_service();
        assert!(matches!(
            run_trace_query(&runtime_service, TraceQueryRequest::runtime_gas_by_source()).unwrap(),
            TraceQueryReport::RuntimeGasBySource(_)
        ));
        assert!(matches!(
            run_trace_query(
                &runtime_service,
                TraceQueryRequest::storage_writes_by_source()
            )
            .unwrap(),
            TraceQueryReport::StorageWritesBySource(_)
        ));
        assert!(matches!(
            run_trace_query(
                &runtime_service,
                TraceQueryRequest::storage_accesses_by_slot()
            )
            .unwrap(),
            TraceQueryReport::StorageAccessesBySlot(_)
        ));
        assert!(matches!(
            run_trace_query(&runtime_service, TraceQueryRequest::call_cost_by_callsite()).unwrap(),
            TraceQueryReport::CallCostByCallsite(_)
        ));
        assert!(matches!(
            run_trace_query(
                &runtime_service,
                TraceQueryRequest::memory_growth_by_source()
            )
            .unwrap(),
            TraceQueryReport::MemoryGrowthBySource(_)
        ));
        assert!(matches!(
            run_trace_query(&runtime_service, TraceQueryRequest::revert_attribution()).unwrap(),
            TraceQueryReport::RevertAttribution(_)
        ));
        assert!(matches!(
            run_trace_query(&runtime_service, TraceQueryRequest::hot_path_by_iteration()).unwrap(),
            TraceQueryReport::HotPathByIteration(_)
        ));
        assert!(matches!(
            run_trace_query(&runtime_service, TraceQueryRequest::value_flow_at_pc(0)).unwrap(),
            TraceQueryReport::ValueFlowAtPc(_)
        ));
    }
}
