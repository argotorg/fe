//! Gas comparison, measurement, CSV parsing, report writing, and aggregation.

use camino::Utf8PathBuf;
use codegen::{
    OptLevel, SonatinaTestDebugConfig, TestMetadata, emit_test_module_sonatina,
    emit_test_module_yul,
};
use contract_harness::CallGasProfile;
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use rustc_hash::{FxHashMap, FxHashSet};
use serde_json::Value;

use crate::report::create_dir_all_utf8;

use super::{
    ComparisonTotals, DEPLOYMENT_ATTRIBUTION_CSV_HEADER,
    DEPLOYMENT_ATTRIBUTION_CSV_HEADER_WITH_SUITE, DEPLOYMENT_ATTRIBUTION_FIELD_COUNT,
    DeltaMagnitudeTotals, DeploymentGasAttributionRow, DeploymentGasAttributionTotals,
    EvmRuntimeMetrics, GasComparisonCase, GasHotspotRow, GasMagnitudeTotals, GasMeasurement,
    GasTotals, ObservabilityCoverageRow, ObservabilityCoverageTotals, ObservabilityPcRange,
    ObservabilityRuntimeSnapshot, OpcodeAggregateTotals, OpcodeMagnitudeTotals, ReportContext,
    SuiteDeltaTotals, SymtabEntry, TestResult, TraceObservabilityHotspotRow, TraceSymbolHotspotRow,
    YUL_VERIFY_RUNTIME, compile_and_run_test, emit_with_catch_unwind, test_case_matches_filter,
    write_report_error, write_yul_case_artifacts,
};

pub(super) fn collect_gas_comparison_cases(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    suite: &str,
    filter: Option<&str>,
    report: &ReportContext,
    primary_backend: &str,
    opt_level: OptLevel,
    primary_cases: &[TestMetadata],
) -> Vec<GasComparisonCase> {
    let mut by_symbol: FxHashMap<String, GasComparisonCase> = FxHashMap::default();
    let mut setup_errors = Vec::new();

    for case in primary_cases
        .iter()
        .filter(|case| test_case_matches_filter(case, filter))
    {
        let key = case.symbol_name.clone();
        let entry = by_symbol
            .entry(key.clone())
            .or_insert_with(|| GasComparisonCase {
                display_name: case.display_name.clone(),
                symbol_name: key,
                yul: None,
                sonatina: None,
            });

        if primary_backend.eq_ignore_ascii_case("yul") {
            entry.yul = Some(case.clone());
        } else if primary_backend.eq_ignore_ascii_case("sonatina") {
            entry.sonatina = Some(case.clone());
        }
    }

    if primary_backend.eq_ignore_ascii_case("yul") {
        let mut emit_output = String::new();
        match emit_with_catch_unwind(
            || {
                emit_test_module_sonatina(
                    db,
                    top_mod,
                    opt_level,
                    &SonatinaTestDebugConfig::default(),
                )
            },
            "Sonatina",
            suite,
            None,
            &mut emit_output,
        ) {
            Ok(output) => {
                for case in output
                    .tests
                    .into_iter()
                    .filter(|case| test_case_matches_filter(case, filter))
                {
                    let key = case.symbol_name.clone();
                    let entry = by_symbol
                        .entry(key.clone())
                        .or_insert_with(|| GasComparisonCase {
                            display_name: case.display_name.clone(),
                            symbol_name: key,
                            yul: None,
                            sonatina: None,
                        });
                    entry.sonatina = Some(case);
                }
            }
            Err(results) => {
                setup_errors.push(format!(
                    "failed to emit Sonatina tests for gas comparison:\n{}",
                    format_results_for_report(&results)
                ));
            }
        }
    } else if primary_backend.eq_ignore_ascii_case("sonatina") {
        let mut emit_output = String::new();
        match emit_with_catch_unwind(
            || emit_test_module_yul(db, top_mod),
            "Yul",
            suite,
            None,
            &mut emit_output,
        ) {
            Ok(output) => {
                for case in output
                    .tests
                    .into_iter()
                    .filter(|case| test_case_matches_filter(case, filter))
                {
                    let key = case.symbol_name.clone();
                    let entry = by_symbol
                        .entry(key.clone())
                        .or_insert_with(|| GasComparisonCase {
                            display_name: case.display_name.clone(),
                            symbol_name: key,
                            yul: None,
                            sonatina: None,
                        });
                    entry.yul = Some(case);
                }
            }
            Err(results) => {
                setup_errors.push(format!(
                    "failed to emit Yul tests for gas comparison:\n{}",
                    format_results_for_report(&results)
                ));
            }
        }
    } else {
        setup_errors.push(format!(
            "unknown backend `{primary_backend}` (expected 'yul' or 'sonatina')"
        ));
    }

    if !setup_errors.is_empty() {
        write_report_error(
            report,
            "gas_comparison_setup_error.txt",
            &setup_errors.join("\n\n"),
        );
    }

    let mut cases: Vec<_> = by_symbol.into_values().collect();
    cases.sort_by(|a, b| a.display_name.cmp(&b.display_name));
    cases
}

fn format_results_for_report(results: &[TestResult]) -> String {
    let mut out = String::new();
    for result in results {
        out.push_str(&format!("- {}\n", result.name));
        if let Some(msg) = &result.error_message {
            out.push_str(&format!("  {msg}\n"));
        }
    }
    if out.is_empty() {
        "no additional details".to_string()
    } else {
        out
    }
}

fn measure_case_gas(
    case: &TestMetadata,
    backend: &str,
    yul_optimize: bool,
    collect_step_count: bool,
) -> GasMeasurement {
    let outcome = compile_and_run_test(
        case,
        false,
        backend,
        yul_optimize,
        None,
        None,
        false,
        collect_step_count,
    );
    GasMeasurement::from_test_outcome(&outcome)
}

fn normalize_inline_text(value: &str) -> String {
    value.replace('\r', "\\r").replace('\n', "\\n")
}

fn csv_escape(value: &str) -> String {
    let value = normalize_inline_text(value);
    if value.contains(',') || value.contains('"') {
        format!("\"{}\"", value.replace('"', "\"\""))
    } else {
        value
    }
}

fn parse_csv_fields(line: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut chars = line.chars().peekable();
    let mut in_quotes = false;

    while let Some(ch) = chars.next() {
        match ch {
            '"' => {
                if in_quotes {
                    if chars.peek() == Some(&'"') {
                        current.push('"');
                        let _ = chars.next();
                    } else {
                        in_quotes = false;
                    }
                } else if current.is_empty() {
                    in_quotes = true;
                } else {
                    current.push(ch);
                }
            }
            ',' if !in_quotes => {
                fields.push(current);
                current = String::new();
            }
            _ => current.push(ch),
        }
    }
    fields.push(current);
    fields
}

fn parse_optional_u64_cell(value: &str) -> Option<u64> {
    value.trim().parse::<u64>().ok()
}

fn parse_optional_i128_cell(value: &str) -> Option<i128> {
    value.trim().parse::<i128>().ok()
}

fn deployment_attribution_row_to_csv_line(row: &DeploymentGasAttributionRow) -> String {
    let cells = [
        row.test.clone(),
        row.symbol.clone(),
        u64_cell(row.yul_unopt_step_total_gas),
        u64_cell(row.yul_opt_step_total_gas),
        u64_cell(row.sonatina_step_total_gas),
        u64_cell(row.yul_unopt_create_opcode_gas),
        u64_cell(row.yul_opt_create_opcode_gas),
        u64_cell(row.sonatina_create_opcode_gas),
        u64_cell(row.yul_unopt_create2_opcode_gas),
        u64_cell(row.yul_opt_create2_opcode_gas),
        u64_cell(row.sonatina_create2_opcode_gas),
        u64_cell(row.yul_unopt_constructor_frame_gas),
        u64_cell(row.yul_opt_constructor_frame_gas),
        u64_cell(row.sonatina_constructor_frame_gas),
        u64_cell(row.yul_unopt_non_constructor_frame_gas),
        u64_cell(row.yul_opt_non_constructor_frame_gas),
        u64_cell(row.sonatina_non_constructor_frame_gas),
        u64_cell(row.yul_unopt_create_opcode_steps),
        u64_cell(row.yul_opt_create_opcode_steps),
        u64_cell(row.sonatina_create_opcode_steps),
        u64_cell(row.yul_unopt_create2_opcode_steps),
        u64_cell(row.yul_opt_create2_opcode_steps),
        u64_cell(row.sonatina_create2_opcode_steps),
        row.note.clone(),
    ];
    let escaped: Vec<String> = cells.into_iter().map(|cell| csv_escape(&cell)).collect();
    escaped.join(",")
}

fn parse_deployment_attribution_row(fields: &[String]) -> Option<DeploymentGasAttributionRow> {
    if fields.len() != DEPLOYMENT_ATTRIBUTION_FIELD_COUNT {
        return None;
    }
    Some(DeploymentGasAttributionRow {
        test: fields[0].clone(),
        symbol: fields[1].clone(),
        yul_unopt_step_total_gas: parse_optional_u64_cell(&fields[2]),
        yul_opt_step_total_gas: parse_optional_u64_cell(&fields[3]),
        sonatina_step_total_gas: parse_optional_u64_cell(&fields[4]),
        yul_unopt_create_opcode_gas: parse_optional_u64_cell(&fields[5]),
        yul_opt_create_opcode_gas: parse_optional_u64_cell(&fields[6]),
        sonatina_create_opcode_gas: parse_optional_u64_cell(&fields[7]),
        yul_unopt_create2_opcode_gas: parse_optional_u64_cell(&fields[8]),
        yul_opt_create2_opcode_gas: parse_optional_u64_cell(&fields[9]),
        sonatina_create2_opcode_gas: parse_optional_u64_cell(&fields[10]),
        yul_unopt_constructor_frame_gas: parse_optional_u64_cell(&fields[11]),
        yul_opt_constructor_frame_gas: parse_optional_u64_cell(&fields[12]),
        sonatina_constructor_frame_gas: parse_optional_u64_cell(&fields[13]),
        yul_unopt_non_constructor_frame_gas: parse_optional_u64_cell(&fields[14]),
        yul_opt_non_constructor_frame_gas: parse_optional_u64_cell(&fields[15]),
        sonatina_non_constructor_frame_gas: parse_optional_u64_cell(&fields[16]),
        yul_unopt_create_opcode_steps: parse_optional_u64_cell(&fields[17]),
        yul_opt_create_opcode_steps: parse_optional_u64_cell(&fields[18]),
        sonatina_create_opcode_steps: parse_optional_u64_cell(&fields[19]),
        yul_unopt_create2_opcode_steps: parse_optional_u64_cell(&fields[20]),
        yul_opt_create2_opcode_steps: parse_optional_u64_cell(&fields[21]),
        sonatina_create2_opcode_steps: parse_optional_u64_cell(&fields[22]),
        note: fields[23].clone(),
    })
}

fn deployment_attribution_row_profiles_for_totals(
    row: &DeploymentGasAttributionRow,
) -> Option<(CallGasProfile, CallGasProfile)> {
    let yul_opt_total_step_gas = row.yul_opt_step_total_gas?;
    let sonatina_total_step_gas = row.sonatina_step_total_gas?;
    let yul_opt_profile = CallGasProfile {
        total_step_gas: yul_opt_total_step_gas,
        create_opcode_gas: row.yul_opt_create_opcode_gas.unwrap_or(0),
        create2_opcode_gas: row.yul_opt_create2_opcode_gas.unwrap_or(0),
        constructor_frame_gas: row.yul_opt_constructor_frame_gas.unwrap_or(0),
        non_constructor_frame_gas: row.yul_opt_non_constructor_frame_gas.unwrap_or(0),
        ..CallGasProfile::default()
    };
    let sonatina_profile = CallGasProfile {
        total_step_gas: sonatina_total_step_gas,
        create_opcode_gas: row.sonatina_create_opcode_gas.unwrap_or(0),
        create2_opcode_gas: row.sonatina_create2_opcode_gas.unwrap_or(0),
        constructor_frame_gas: row.sonatina_constructor_frame_gas.unwrap_or(0),
        non_constructor_frame_gas: row.sonatina_non_constructor_frame_gas.unwrap_or(0),
        ..CallGasProfile::default()
    };
    Some((yul_opt_profile, sonatina_profile))
}

fn gas_profile_partition_violation(label: &str, profile: CallGasProfile) -> Option<String> {
    let opcode_partition_delta = profile.create_opcode_gas as i128
        + profile.create2_opcode_gas as i128
        + profile.non_create_opcode_gas as i128
        - profile.total_step_gas as i128;
    let frame_partition_delta = profile.constructor_frame_gas as i128
        + profile.non_constructor_frame_gas as i128
        - profile.total_step_gas as i128;
    if opcode_partition_delta == 0 && frame_partition_delta == 0 {
        None
    } else {
        Some(format!(
            "{label} attribution_residual(opcode={opcode_partition_delta}, frame={frame_partition_delta})"
        ))
    }
}

fn parse_symtab_entries(contents: &str) -> Vec<SymtabEntry> {
    let mut rows = Vec::new();
    for line in contents.lines() {
        let line = line.trim();
        if !line.starts_with("off=") {
            continue;
        }
        // Format:
        // off=  3996 size=   700 test_erc20__StorPtr_Evm___...
        let mut parts = line.split_whitespace();
        let off_token = parts.next().unwrap_or_default();
        let off_value = parts.next().unwrap_or_default();
        let size_token = parts.next().unwrap_or_default();
        let size_value = parts.next().unwrap_or_default();
        if off_token != "off=" || size_token != "size=" {
            continue;
        }
        let Ok(start) = off_value.parse::<u32>() else {
            continue;
        };
        let Ok(size) = size_value.parse::<u32>() else {
            continue;
        };
        let symbol = parts.collect::<Vec<_>>().join(" ");
        if symbol.is_empty() {
            continue;
        }
        rows.push(SymtabEntry {
            start,
            end: start.saturating_add(size),
            symbol,
        });
    }
    rows.sort_by_key(|row| row.start);
    rows
}

fn parse_trace_tail_pcs(contents: &str) -> Vec<u32> {
    let mut pcs = Vec::new();
    for line in contents.lines() {
        let Some(rest) = line.strip_prefix("pc=") else {
            continue;
        };
        let mut parts = rest.split_whitespace();
        let Some(pc_text) = parts.next() else {
            continue;
        };
        if let Ok(pc) = pc_text.parse::<u32>() {
            pcs.push(pc);
        }
    }
    pcs
}

fn map_pc_to_symbol(pc: u32, symtab: &[SymtabEntry]) -> Option<&str> {
    let mut lo = 0usize;
    let mut hi = symtab.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        let row = &symtab[mid];
        if pc < row.start {
            hi = mid;
        } else if pc >= row.end {
            lo = mid + 1;
        } else {
            return Some(&row.symbol);
        }
    }
    None
}

fn json_u64(value: Option<&Value>) -> u64 {
    value.and_then(Value::as_u64).unwrap_or(0)
}

fn parse_observability_runtime_snapshot(contents: &str) -> Option<ObservabilityRuntimeSnapshot> {
    let root: Value = serde_json::from_str(contents).ok()?;
    let section = root
        .get("sections")
        .and_then(Value::as_array)?
        .iter()
        .find(|entry| entry.get("section").and_then(Value::as_str) == Some("runtime"))?;

    let unmapped_obj = section
        .get("unmapped_reason_coverage")
        .and_then(Value::as_object);
    let unmapped_reason = |key: &str| -> u64 {
        unmapped_obj
            .and_then(|obj| obj.get(key))
            .and_then(Value::as_u64)
            .unwrap_or(0)
    };

    let mut pc_ranges = Vec::new();
    if let Some(entries) = section.get("pc_map").and_then(Value::as_array) {
        for entry in entries {
            let start = entry.get("pc_start").and_then(Value::as_u64);
            let end = entry.get("pc_end").and_then(Value::as_u64);
            let (Some(start), Some(end)) = (start, end) else {
                continue;
            };
            if end <= start || end > u32::MAX as u64 {
                continue;
            }
            let func_name = entry
                .get("func_name")
                .and_then(Value::as_str)
                .unwrap_or("<unknown>")
                .to_string();
            let reason = entry
                .get("reason")
                .and_then(Value::as_str)
                .map(str::to_string);
            pc_ranges.push(ObservabilityPcRange {
                start: start as u32,
                end: end as u32,
                func_name,
                reason,
            });
        }
    }
    pc_ranges.sort_by_key(|range| range.start);

    Some(ObservabilityRuntimeSnapshot {
        section: section
            .get("section")
            .and_then(Value::as_str)
            .unwrap_or("runtime")
            .to_string(),
        schema_version: section
            .get("schema_version")
            .and_then(Value::as_str)
            .unwrap_or("-")
            .to_string(),
        section_bytes: json_u64(section.get("section_bytes")),
        code_bytes: json_u64(section.get("code_bytes")),
        data_bytes: json_u64(section.get("data_bytes")),
        embed_bytes: json_u64(section.get("embed_bytes")),
        mapped_code_bytes: json_u64(section.get("mapped_code_bytes")),
        unmapped_code_bytes: json_u64(section.get("unmapped_code_bytes")),
        unmapped_no_ir_inst: unmapped_reason("no_ir_inst"),
        unmapped_label_or_fixup_only: unmapped_reason("label_or_fixup_only"),
        unmapped_synthetic: unmapped_reason("synthetic"),
        unmapped_unknown: unmapped_reason("unknown"),
        pc_ranges,
    })
}

fn load_suite_observability_runtime(
    suite_dir: &Utf8PathBuf,
    suite: &str,
) -> (
    Vec<ObservabilityCoverageRow>,
    FxHashMap<String, Vec<ObservabilityPcRange>>,
) {
    let mut rows = Vec::new();
    let mut ranges_by_test: FxHashMap<String, Vec<ObservabilityPcRange>> = FxHashMap::default();
    let tests_dir = suite_dir.join("artifacts").join("tests");
    let Ok(entries) = std::fs::read_dir(&tests_dir) else {
        return (rows, ranges_by_test);
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let test = entry.file_name().to_string_lossy().to_string();
        let obs_path = path.join("sonatina").join("observability.json");
        let Ok(contents) = std::fs::read_to_string(&obs_path) else {
            continue;
        };
        let Some(snapshot) = parse_observability_runtime_snapshot(&contents) else {
            continue;
        };
        rows.push(ObservabilityCoverageRow {
            suite: suite.to_string(),
            test: test.clone(),
            section: snapshot.section,
            schema_version: snapshot.schema_version,
            section_bytes: snapshot.section_bytes,
            code_bytes: snapshot.code_bytes,
            data_bytes: snapshot.data_bytes,
            embed_bytes: snapshot.embed_bytes,
            mapped_code_bytes: snapshot.mapped_code_bytes,
            unmapped_code_bytes: snapshot.unmapped_code_bytes,
            unmapped_no_ir_inst: snapshot.unmapped_no_ir_inst,
            unmapped_label_or_fixup_only: snapshot.unmapped_label_or_fixup_only,
            unmapped_synthetic: snapshot.unmapped_synthetic,
            unmapped_unknown: snapshot.unmapped_unknown,
        });
        ranges_by_test.insert(test, snapshot.pc_ranges);
    }
    rows.sort_by(|a, b| a.test.cmp(&b.test).then_with(|| a.section.cmp(&b.section)));
    (rows, ranges_by_test)
}

fn map_pc_to_observability(
    pc: u32,
    ranges: &[ObservabilityPcRange],
) -> Option<&ObservabilityPcRange> {
    let mut lo = 0usize;
    let mut hi = ranges.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        let range = &ranges[mid];
        if pc < range.start {
            hi = mid;
        } else if pc >= range.end {
            lo = mid + 1;
        } else {
            return Some(range);
        }
    }
    None
}

fn resolve_observability_test_ranges<'a>(
    suite: &str,
    trace_test_name: &'a str,
    ranges_by_test: &'a FxHashMap<String, Vec<ObservabilityPcRange>>,
) -> Option<&'a [ObservabilityPcRange]> {
    let suite_prefixed = format!("{suite}__");
    let mut candidates: Vec<&str> = Vec::new();
    candidates.push(trace_test_name);
    if let Some(stripped) = trace_test_name.strip_prefix(&suite_prefixed) {
        candidates.push(stripped);
    }
    if let Some((_, suffix)) = trace_test_name.split_once("__") {
        candidates.push(suffix);
    }
    if let Some((_, suffix)) = trace_test_name.rsplit_once("__") {
        candidates.push(suffix);
    }

    let mut seen: Vec<&str> = Vec::new();
    for candidate in candidates {
        if seen.contains(&candidate) {
            continue;
        }
        seen.push(candidate);
        if let Some(ranges) = ranges_by_test.get(candidate) {
            return Some(ranges.as_slice());
        }
    }

    None
}

pub(super) fn evm_runtime_metrics_from_bytes(bytes: &[u8]) -> EvmRuntimeMetrics {
    let mut metrics = EvmRuntimeMetrics {
        byte_len: bytes.len(),
        ..EvmRuntimeMetrics::default()
    };
    let mut idx = 0usize;
    while idx < bytes.len() {
        let op = bytes[idx];
        idx += 1;
        metrics.op_count += 1;
        match op {
            0x5f => metrics.push_ops += 1, // PUSH0
            0x50 => metrics.pop_ops += 1,
            0x51 => metrics.mload_ops += 1,
            0x52 => metrics.mstore_ops += 1,
            0x54 => metrics.sload_ops += 1,
            0x55 => metrics.sstore_ops += 1,
            0x56 => metrics.jump_ops += 1,
            0x57 => metrics.jumpi_ops += 1,
            0x5b => metrics.jumpdest_ops += 1,
            0x15 => metrics.iszero_ops += 1,
            0x20 => metrics.keccak_ops += 1,
            0x37 => metrics.calldatacopy_ops += 1,
            0x3e => metrics.returndatacopy_ops += 1,
            0x5e => metrics.mcopy_ops += 1,
            0xf1 => metrics.call_ops += 1,
            0xfa => metrics.staticcall_ops += 1,
            0xf3 => metrics.return_ops += 1,
            0xfd => metrics.revert_ops += 1,
            0x60..=0x7f => {
                metrics.push_ops += 1;
                let push_n = (op - 0x5f) as usize;
                idx = idx.saturating_add(push_n).min(bytes.len());
            }
            0x80..=0x8f => metrics.dup_ops += 1,
            0x90..=0x9f => metrics.swap_ops += 1,
            _ => {}
        }
    }
    metrics
}

pub(super) fn evm_runtime_metrics_from_hex(runtime_hex: &str) -> Option<EvmRuntimeMetrics> {
    let bytes = hex::decode(runtime_hex.trim()).ok()?;
    Some(evm_runtime_metrics_from_bytes(&bytes))
}

fn usize_cell(value: Option<usize>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

fn u64_cell(value: Option<u64>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

fn ratio_cell_usize(numerator: Option<usize>, denominator: Option<usize>) -> String {
    match (numerator, denominator) {
        (Some(n), Some(d)) if d > 0 => format!("{:.2}", n as f64 / d as f64),
        _ => "n/a".to_string(),
    }
}

fn ratio_cell_u64(numerator: Option<u64>, denominator: Option<u64>) -> String {
    match (numerator, denominator) {
        (Some(n), Some(d)) if d > 0 => format!("{:.2}", n as f64 / d as f64),
        _ => "n/a".to_string(),
    }
}

fn stack_ops_pct_cell(metrics: Option<EvmRuntimeMetrics>) -> String {
    match metrics {
        Some(metrics) if metrics.op_count > 0 => {
            format!(
                "{:.2}%",
                (metrics.stack_ops_total() as f64 * 100.0) / (metrics.op_count as f64)
            )
        }
        _ => "n/a".to_string(),
    }
}

fn format_ratio_percent(numerator: usize, denominator: usize) -> String {
    if denominator == 0 {
        "n/a".to_string()
    } else {
        format!("{:.1}%", (numerator as f64 * 100.0) / (denominator as f64))
    }
}

fn format_delta_percent(delta: i128, baseline: u64) -> String {
    if baseline == 0 {
        "n/a".to_string()
    } else {
        let pct = (delta as f64 * 100.0) / (baseline as f64);
        format!("{pct:.2}%")
    }
}

fn gas_comparison_settings_text(opt_level: OptLevel) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "yul.primary.optimize={}\n",
        opt_level.yul_optimize()
    ));
    out.push_str("yul.compare.unoptimized.optimize=false\n");
    out.push_str("yul.compare.optimized.optimize=true\n");
    out.push_str(&format!("yul.solc.verify_runtime={YUL_VERIFY_RUNTIME}\n"));
    out.push_str(&format!("sonatina.opt_level={opt_level}\n"));
    out.push_str("sonatina.codegen.path=emit_test_module_sonatina (default)\n");
    out.push_str("measurement.call=RuntimeInstance::call_raw(empty calldata)\n");
    out.push_str("measurement.deploy=RuntimeInstance::deploy_tracked(init bytecode)\n");
    out.push_str("measurement.total=deploy_gas + call_gas (when both are available)\n");
    out
}

fn write_comparison_totals_rows(
    out: &mut String,
    baseline: &str,
    comparison: ComparisonTotals,
    tests_in_scope: usize,
) {
    out.push_str(&format!(
        "{baseline},compared_with_gas,{},{},{}\n",
        comparison.compared_with_gas,
        format_ratio_percent(comparison.compared_with_gas, comparison.compared_with_gas),
        format_ratio_percent(comparison.compared_with_gas, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},sonatina_lower,{},{},{}\n",
        comparison.sonatina_lower,
        format_ratio_percent(comparison.sonatina_lower, comparison.compared_with_gas),
        format_ratio_percent(comparison.sonatina_lower, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},sonatina_higher,{},{},{}\n",
        comparison.sonatina_higher,
        format_ratio_percent(comparison.sonatina_higher, comparison.compared_with_gas),
        format_ratio_percent(comparison.sonatina_higher, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},equal,{},{},{}\n",
        comparison.equal,
        format_ratio_percent(comparison.equal, comparison.compared_with_gas),
        format_ratio_percent(comparison.equal, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},incomplete,{},n/a,{}\n",
        comparison.incomplete,
        format_ratio_percent(comparison.incomplete, tests_in_scope)
    ));
}

fn write_gas_totals_csv(path: &Utf8PathBuf, totals: GasTotals) {
    let mut out = String::new();
    out.push_str("baseline,metric,count,pct_of_compared,pct_of_scope\n");
    out.push_str(&format!(
        "all,tests_in_scope,{},n/a,{}\n",
        totals.tests_in_scope,
        format_ratio_percent(totals.tests_in_scope, totals.tests_in_scope)
    ));
    write_comparison_totals_rows(
        &mut out,
        "yul_unopt",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    write_comparison_totals_rows(
        &mut out,
        "yul_opt",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );

    let _ = std::fs::write(path, out);
}

fn write_magnitude_totals_rows(out: &mut String, baseline: &str, totals: DeltaMagnitudeTotals) {
    out.push_str(&format!(
        "{baseline},compared_with_gas,{}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!("{baseline},pct_rows,{}\n", totals.pct_rows));
    out.push_str(&format!(
        "{baseline},baseline_gas_sum,{}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},sonatina_gas_sum,{}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},delta_gas_sum,{}\n",
        totals.delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},abs_delta_gas_sum,{}\n",
        totals.abs_delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},delta_pct_sum,{:.6}\n",
        totals.delta_pct_sum
    ));
    out.push_str(&format!(
        "{baseline},abs_delta_pct_sum,{:.6}\n",
        totals.abs_delta_pct_sum
    ));
}

fn write_gas_magnitude_csv(path: &Utf8PathBuf, totals: GasMagnitudeTotals) {
    let mut out = String::new();
    out.push_str("baseline,metric,value\n");
    write_magnitude_totals_rows(&mut out, "yul_unopt", totals.vs_yul_unopt);
    write_magnitude_totals_rows(&mut out, "yul_opt", totals.vs_yul_opt);
    let _ = std::fs::write(path, out);
}

fn write_gas_breakdown_magnitude_component_rows(
    out: &mut String,
    baseline: &str,
    component: &str,
    totals: DeltaMagnitudeTotals,
) {
    out.push_str(&format!(
        "{baseline},{component},compared_with_gas,{}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!(
        "{baseline},{component},pct_rows,{}\n",
        totals.pct_rows
    ));
    out.push_str(&format!(
        "{baseline},{component},baseline_gas_sum,{}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},sonatina_gas_sum,{}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},delta_gas_sum,{}\n",
        totals.delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},abs_delta_gas_sum,{}\n",
        totals.abs_delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},delta_pct_sum,{:.6}\n",
        totals.delta_pct_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},abs_delta_pct_sum,{:.6}\n",
        totals.abs_delta_pct_sum
    ));
}

fn write_gas_breakdown_magnitude_csv(
    path: &Utf8PathBuf,
    call_totals: GasMagnitudeTotals,
    deploy_totals: GasMagnitudeTotals,
    total_totals: GasMagnitudeTotals,
) {
    let mut out = String::new();
    out.push_str("baseline,component,metric,value\n");
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "call",
        call_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "call",
        call_totals.vs_yul_opt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "deploy",
        deploy_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "deploy",
        deploy_totals.vs_yul_opt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "total",
        total_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "total",
        total_totals.vs_yul_opt,
    );
    let _ = std::fs::write(path, out);
}

fn write_opcode_magnitude_rows(out: &mut String, side: &str, totals: OpcodeAggregateTotals) {
    out.push_str(&format!("{side},steps_sum,{}\n", totals.steps_sum));
    out.push_str(&format!(
        "{side},runtime_bytes_sum,{}\n",
        totals.runtime_bytes_sum
    ));
    out.push_str(&format!(
        "{side},runtime_ops_sum,{}\n",
        totals.runtime_ops_sum
    ));
    out.push_str(&format!("{side},swap_ops_sum,{}\n", totals.swap_ops_sum));
    out.push_str(&format!("{side},pop_ops_sum,{}\n", totals.pop_ops_sum));
    out.push_str(&format!("{side},jump_ops_sum,{}\n", totals.jump_ops_sum));
    out.push_str(&format!("{side},jumpi_ops_sum,{}\n", totals.jumpi_ops_sum));
    out.push_str(&format!(
        "{side},iszero_ops_sum,{}\n",
        totals.iszero_ops_sum
    ));
    out.push_str(&format!(
        "{side},mem_rw_ops_sum,{}\n",
        totals.mem_rw_ops_sum
    ));
    out.push_str(&format!(
        "{side},storage_rw_ops_sum,{}\n",
        totals.storage_rw_ops_sum
    ));
    out.push_str(&format!("{side},mload_ops_sum,{}\n", totals.mload_ops_sum));
    out.push_str(&format!(
        "{side},mstore_ops_sum,{}\n",
        totals.mstore_ops_sum
    ));
    out.push_str(&format!("{side},sload_ops_sum,{}\n", totals.sload_ops_sum));
    out.push_str(&format!(
        "{side},sstore_ops_sum,{}\n",
        totals.sstore_ops_sum
    ));
    out.push_str(&format!(
        "{side},keccak_ops_sum,{}\n",
        totals.keccak_ops_sum
    ));
    out.push_str(&format!(
        "{side},call_family_ops_sum,{}\n",
        totals.call_family_ops_sum
    ));
    out.push_str(&format!("{side},copy_ops_sum,{}\n", totals.copy_ops_sum));
}

fn write_opcode_magnitude_csv(path: &Utf8PathBuf, totals: OpcodeMagnitudeTotals) {
    let mut out = String::new();
    out.push_str("side,metric,value\n");
    out.push_str(&format!(
        "all,compared_with_metrics,{}\n",
        totals.compared_with_metrics
    ));
    write_opcode_magnitude_rows(&mut out, "yul_opt", totals.yul_opt);
    write_opcode_magnitude_rows(&mut out, "sonatina", totals.sonatina);
    let _ = std::fs::write(path, out);
}

fn write_gas_hotspots_csv(path: &Utf8PathBuf, rows: &[GasHotspotRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| b.delta_vs_yul_opt.cmp(&a.delta_vs_yul_opt));

    let total_delta: i128 = sorted.iter().map(|row| row.delta_vs_yul_opt).sum();
    let mut cumulative: i128 = 0;

    let mut out = String::new();
    out.push_str("rank,suite,test,symbol,yul_opt_gas,sonatina_gas,delta_vs_yul_opt,delta_vs_yul_opt_pct,share_of_total_delta_pct,cumulative_share_pct\n");
    for (idx, row) in sorted.into_iter().enumerate() {
        cumulative += row.delta_vs_yul_opt;
        let share = ratio_percent_i128(row.delta_vs_yul_opt, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        let cumulative_share = ratio_percent_i128(cumulative, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{}\n",
            idx + 1,
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.symbol),
            csv_escape(&u64_cell(row.yul_opt_gas)),
            csv_escape(&u64_cell(row.sonatina_gas)),
            row.delta_vs_yul_opt,
            csv_escape(&row.delta_vs_yul_opt_pct),
            csv_escape(&share),
            csv_escape(&cumulative_share)
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_suite_delta_summary_csv(path: &Utf8PathBuf, suite_rollup: &[(String, SuiteDeltaTotals)]) {
    let total_delta: i128 = suite_rollup
        .iter()
        .map(|(_, totals)| totals.delta_vs_yul_opt_sum)
        .sum();
    let mut out = String::new();
    out.push_str("suite,tests_with_delta,delta_vs_yul_opt_sum,avg_delta_vs_yul_opt,share_of_total_delta_pct\n");
    for (suite, totals) in suite_rollup {
        let avg = if totals.tests_with_delta == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}",
                totals.delta_vs_yul_opt_sum as f64 / totals.tests_with_delta as f64
            )
        };
        let share = ratio_percent_i128(totals.delta_vs_yul_opt_sum, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        out.push_str(&format!(
            "{},{},{},{},{}\n",
            csv_escape(suite),
            totals.tests_with_delta,
            totals.delta_vs_yul_opt_sum,
            csv_escape(&avg),
            csv_escape(&share),
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_trace_symbol_hotspots_csv(path: &Utf8PathBuf, rows: &[TraceSymbolHotspotRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_symbol
            .cmp(&a.steps_in_symbol)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
    });

    let mut out = String::new();
    out.push_str("suite,test,symbol,tail_steps_total,tail_steps_mapped,steps_in_symbol,symbol_share_of_tail_pct\n");
    for row in sorted {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_symbol as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.symbol),
            row.tail_steps_total,
            row.tail_steps_mapped,
            row.steps_in_symbol,
            csv_escape(&pct)
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_observability_coverage_csv(path: &Utf8PathBuf, rows: &[ObservabilityCoverageRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        a.suite
            .cmp(&b.suite)
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.section.cmp(&b.section))
    });

    let mut out = String::new();
    out.push_str("suite,test,section,schema_version,section_bytes,code_bytes,data_bytes,embed_bytes,mapped_code_bytes,unmapped_code_bytes,mapped_code_pct,unmapped_code_pct,unmapped_no_ir_inst,unmapped_label_or_fixup_only,unmapped_synthetic,unmapped_unknown\n");
    for row in sorted {
        let mapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.mapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        let unmapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.unmapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.section),
            csv_escape(&row.schema_version),
            row.section_bytes,
            row.code_bytes,
            row.data_bytes,
            row.embed_bytes,
            row.mapped_code_bytes,
            row.unmapped_code_bytes,
            csv_escape(&mapped_pct),
            csv_escape(&unmapped_pct),
            row.unmapped_no_ir_inst,
            row.unmapped_label_or_fixup_only,
            row.unmapped_synthetic,
            row.unmapped_unknown,
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_trace_observability_hotspots_csv(
    path: &Utf8PathBuf,
    rows: &[TraceObservabilityHotspotRow],
) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_bucket
            .cmp(&a.steps_in_bucket)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.function.cmp(&b.function))
            .then_with(|| a.reason.cmp(&b.reason))
    });

    let mut out = String::new();
    out.push_str("suite,test,function,reason,tail_steps_total,tail_steps_mapped,steps_in_bucket,bucket_share_of_tail_pct\n");
    for row in sorted {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_bucket as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.function),
            csv_escape(&row.reason),
            row.tail_steps_total,
            row.tail_steps_mapped,
            row.steps_in_bucket,
            csv_escape(&pct),
        ));
    }
    let _ = std::fs::write(path, out);
}

fn parse_gas_totals_csv(contents: &str) -> GasTotals {
    let mut totals = GasTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(5, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let count = parts
            .next()
            .and_then(|raw| raw.trim().parse::<usize>().ok())
            .unwrap_or(0);
        match (baseline, metric) {
            ("all", "tests_in_scope") => totals.tests_in_scope = count,
            ("yul_unopt", "compared_with_gas") => totals.vs_yul_unopt.compared_with_gas = count,
            ("yul_unopt", "sonatina_lower") => totals.vs_yul_unopt.sonatina_lower = count,
            ("yul_unopt", "sonatina_higher") => totals.vs_yul_unopt.sonatina_higher = count,
            ("yul_unopt", "equal") => totals.vs_yul_unopt.equal = count,
            ("yul_unopt", "incomplete") => totals.vs_yul_unopt.incomplete = count,
            ("yul_opt", "compared_with_gas") => totals.vs_yul_opt.compared_with_gas = count,
            ("yul_opt", "sonatina_lower") => totals.vs_yul_opt.sonatina_lower = count,
            ("yul_opt", "sonatina_higher") => totals.vs_yul_opt.sonatina_higher = count,
            ("yul_opt", "equal") => totals.vs_yul_opt.equal = count,
            ("yul_opt", "incomplete") => totals.vs_yul_opt.incomplete = count,
            _ => {}
        }
    }
    totals
}

fn parse_gas_magnitude_csv(contents: &str) -> GasMagnitudeTotals {
    let mut totals = GasMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(3, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();
        let target = match baseline {
            "yul_unopt" => &mut totals.vs_yul_unopt,
            "yul_opt" => &mut totals.vs_yul_opt,
            _ => continue,
        };
        match metric {
            "compared_with_gas" => {
                target.compared_with_gas = value.parse::<usize>().unwrap_or(0);
            }
            "pct_rows" => {
                target.pct_rows = value.parse::<usize>().unwrap_or(0);
            }
            "baseline_gas_sum" => {
                target.baseline_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "sonatina_gas_sum" => {
                target.sonatina_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_gas_sum" => {
                target.delta_gas_sum = value.parse::<i128>().unwrap_or(0);
            }
            "abs_delta_gas_sum" => {
                target.abs_delta_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_pct_sum" => {
                target.delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            "abs_delta_pct_sum" => {
                target.abs_delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            _ => {}
        }
    }
    totals
}

fn parse_gas_breakdown_magnitude_csv(
    contents: &str,
) -> (GasMagnitudeTotals, GasMagnitudeTotals, GasMagnitudeTotals) {
    let mut call_totals = GasMagnitudeTotals::default();
    let mut deploy_totals = GasMagnitudeTotals::default();
    let mut total_totals = GasMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(4, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let component = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();

        let component_totals = match component {
            "call" => &mut call_totals,
            "deploy" => &mut deploy_totals,
            "total" => &mut total_totals,
            _ => continue,
        };

        let target = match baseline {
            "yul_unopt" => &mut component_totals.vs_yul_unopt,
            "yul_opt" => &mut component_totals.vs_yul_opt,
            _ => continue,
        };

        match metric {
            "compared_with_gas" => {
                target.compared_with_gas = value.parse::<usize>().unwrap_or(0);
            }
            "pct_rows" => {
                target.pct_rows = value.parse::<usize>().unwrap_or(0);
            }
            "baseline_gas_sum" => {
                target.baseline_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "sonatina_gas_sum" => {
                target.sonatina_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_gas_sum" => {
                target.delta_gas_sum = value.parse::<i128>().unwrap_or(0);
            }
            "abs_delta_gas_sum" => {
                target.abs_delta_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_pct_sum" => {
                target.delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            "abs_delta_pct_sum" => {
                target.abs_delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            _ => {}
        }
    }
    (call_totals, deploy_totals, total_totals)
}

fn parse_gas_opcode_magnitude_csv(contents: &str) -> OpcodeMagnitudeTotals {
    let mut totals = OpcodeMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(3, ',');
        let side = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();
        if side == "all" && metric == "compared_with_metrics" {
            totals.compared_with_metrics = value.parse::<usize>().unwrap_or(0);
            continue;
        }
        let target = match side {
            "yul_opt" => &mut totals.yul_opt,
            "sonatina" => &mut totals.sonatina,
            _ => continue,
        };
        let parsed = value.parse::<u128>().unwrap_or(0);
        match metric {
            "steps_sum" => target.steps_sum = parsed,
            "runtime_bytes_sum" => target.runtime_bytes_sum = parsed,
            "runtime_ops_sum" => target.runtime_ops_sum = parsed,
            "swap_ops_sum" => target.swap_ops_sum = parsed,
            "pop_ops_sum" => target.pop_ops_sum = parsed,
            "jump_ops_sum" => target.jump_ops_sum = parsed,
            "jumpi_ops_sum" => target.jumpi_ops_sum = parsed,
            "iszero_ops_sum" => target.iszero_ops_sum = parsed,
            "mem_rw_ops_sum" => target.mem_rw_ops_sum = parsed,
            "storage_rw_ops_sum" => target.storage_rw_ops_sum = parsed,
            "mload_ops_sum" => target.mload_ops_sum = parsed,
            "mstore_ops_sum" => target.mstore_ops_sum = parsed,
            "sload_ops_sum" => target.sload_ops_sum = parsed,
            "sstore_ops_sum" => target.sstore_ops_sum = parsed,
            "keccak_ops_sum" => target.keccak_ops_sum = parsed,
            "call_family_ops_sum" => target.call_family_ops_sum = parsed,
            "copy_ops_sum" => target.copy_ops_sum = parsed,
            _ => {}
        }
    }
    totals
}

fn measurement_status(has_metadata: bool, measurement: Option<&GasMeasurement>) -> &'static str {
    if !has_metadata {
        return "missing";
    }
    let Some(measurement) = measurement else {
        return "not_run";
    };
    if measurement.passed {
        if measurement.gas_used.is_some() {
            "ok"
        } else {
            "ok_no_gas"
        }
    } else {
        "failed"
    }
}

fn record_delta(
    baseline_gas: Option<u64>,
    sonatina_gas: Option<u64>,
    totals: &mut ComparisonTotals,
) -> (String, String) {
    match (baseline_gas, sonatina_gas) {
        (Some(baseline_gas), Some(sonatina_gas)) => {
            totals.compared_with_gas += 1;
            let diff = sonatina_gas as i128 - baseline_gas as i128;
            if diff < 0 {
                totals.sonatina_lower += 1;
            } else if diff > 0 {
                totals.sonatina_higher += 1;
            } else {
                totals.equal += 1;
            }
            (diff.to_string(), format_delta_percent(diff, baseline_gas))
        }
        _ => {
            totals.incomplete += 1;
            ("n/a".to_string(), "n/a".to_string())
        }
    }
}

fn delta_cells(baseline_gas: Option<u64>, sonatina_gas: Option<u64>) -> (String, String) {
    match (baseline_gas, sonatina_gas) {
        (Some(baseline_gas), Some(sonatina_gas)) => {
            let diff = sonatina_gas as i128 - baseline_gas as i128;
            (diff.to_string(), format_delta_percent(diff, baseline_gas))
        }
        _ => ("n/a".to_string(), "n/a".to_string()),
    }
}

fn record_delta_magnitude(
    baseline_gas: Option<u64>,
    sonatina_gas: Option<u64>,
    totals: &mut DeltaMagnitudeTotals,
) {
    let (Some(baseline_gas), Some(sonatina_gas)) = (baseline_gas, sonatina_gas) else {
        return;
    };

    let delta_gas = sonatina_gas as i128 - baseline_gas as i128;
    totals.compared_with_gas += 1;
    totals.baseline_gas_sum += baseline_gas as u128;
    totals.sonatina_gas_sum += sonatina_gas as u128;
    totals.delta_gas_sum += delta_gas;
    totals.abs_delta_gas_sum += if delta_gas < 0 {
        (-delta_gas) as u128
    } else {
        delta_gas as u128
    };

    if baseline_gas > 0 {
        let delta_pct = (delta_gas as f64 * 100.0) / baseline_gas as f64;
        totals.pct_rows += 1;
        totals.delta_pct_sum += delta_pct;
        totals.abs_delta_pct_sum += delta_pct.abs();
    }
}

fn append_comparison_summary(
    out: &mut String,
    label: &str,
    totals: ComparisonTotals,
    tests_in_scope: usize,
) {
    out.push_str(&format!("### {label}\n\n"));
    out.push_str(&format!(
        "- compared_with_gas: {} ({})\n",
        totals.compared_with_gas,
        format_ratio_percent(totals.compared_with_gas, tests_in_scope)
    ));
    out.push_str(&format!(
        "- sonatina_lower: {} ({})\n",
        totals.sonatina_lower,
        format_ratio_percent(totals.sonatina_lower, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- sonatina_higher: {} ({})\n",
        totals.sonatina_higher,
        format_ratio_percent(totals.sonatina_higher, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- equal: {} ({})\n",
        totals.equal,
        format_ratio_percent(totals.equal, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- incomplete: {} ({})\n\n",
        totals.incomplete,
        format_ratio_percent(totals.incomplete, tests_in_scope)
    ));
}

fn format_percent_cell(value: Option<f64>) -> String {
    value
        .map(|v| format!("{v:.2}%"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn format_float_cell(value: Option<f64>) -> String {
    value
        .map(|v| format!("{v:.2}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn append_magnitude_summary(out: &mut String, label: &str, totals: DeltaMagnitudeTotals) {
    out.push_str(&format!("### {label}\n\n"));
    out.push_str(&format!(
        "- compared_with_gas: {}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!(
        "- baseline_gas_sum: {}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "- sonatina_gas_sum: {}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!("- total_delta_gas: {}\n", totals.delta_gas_sum));
    out.push_str(&format!(
        "- weighted_delta_pct: {}\n",
        format_percent_cell(totals.weighted_delta_pct())
    ));
    out.push_str(&format!(
        "- mean_delta_gas: {}\n",
        format_float_cell(totals.mean_delta_gas())
    ));
    out.push_str(&format!(
        "- mean_abs_delta_gas: {}\n",
        format_float_cell(totals.mean_abs_delta_gas())
    ));
    out.push_str(&format!(
        "- mean_delta_pct: {}\n",
        format_percent_cell(totals.mean_delta_pct())
    ));
    out.push_str(&format!(
        "- mean_abs_delta_pct: {}\n\n",
        format_percent_cell(totals.mean_abs_delta_pct())
    ));
}

fn delta_pct_from_sums(baseline_sum: u128, sonatina_sum: u128) -> Option<f64> {
    if baseline_sum == 0 {
        None
    } else {
        Some((sonatina_sum as f64 - baseline_sum as f64) * 100.0 / baseline_sum as f64)
    }
}

fn append_opcode_delta_metric(
    out: &mut String,
    label: &str,
    baseline_sum: u128,
    sonatina_sum: u128,
) {
    let delta = sonatina_sum as i128 - baseline_sum as i128;
    out.push_str(&format!(
        "- {label}: {} -> {} (delta {}, {})\n",
        baseline_sum,
        sonatina_sum,
        delta,
        format_percent_cell(delta_pct_from_sums(baseline_sum, sonatina_sum))
    ));
}

fn append_opcode_magnitude_summary(out: &mut String, totals: OpcodeMagnitudeTotals) {
    out.push_str("## Opcode Aggregate Delta Metrics (vs Yul optimized)\n\n");
    out.push_str(&format!(
        "- compared_with_metrics: {}\n",
        totals.compared_with_metrics
    ));
    append_opcode_delta_metric(
        out,
        "steps_sum",
        totals.yul_opt.steps_sum,
        totals.sonatina.steps_sum,
    );
    append_opcode_delta_metric(
        out,
        "runtime_bytes_sum",
        totals.yul_opt.runtime_bytes_sum,
        totals.sonatina.runtime_bytes_sum,
    );
    append_opcode_delta_metric(
        out,
        "runtime_ops_sum",
        totals.yul_opt.runtime_ops_sum,
        totals.sonatina.runtime_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "swap_ops_sum",
        totals.yul_opt.swap_ops_sum,
        totals.sonatina.swap_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "pop_ops_sum",
        totals.yul_opt.pop_ops_sum,
        totals.sonatina.pop_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "jump_ops_sum",
        totals.yul_opt.jump_ops_sum,
        totals.sonatina.jump_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "jumpi_ops_sum",
        totals.yul_opt.jumpi_ops_sum,
        totals.sonatina.jumpi_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "iszero_ops_sum",
        totals.yul_opt.iszero_ops_sum,
        totals.sonatina.iszero_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mem_rw_ops_sum",
        totals.yul_opt.mem_rw_ops_sum,
        totals.sonatina.mem_rw_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "storage_rw_ops_sum",
        totals.yul_opt.storage_rw_ops_sum,
        totals.sonatina.storage_rw_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mload_ops_sum",
        totals.yul_opt.mload_ops_sum,
        totals.sonatina.mload_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mstore_ops_sum",
        totals.yul_opt.mstore_ops_sum,
        totals.sonatina.mstore_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "sload_ops_sum",
        totals.yul_opt.sload_ops_sum,
        totals.sonatina.sload_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "sstore_ops_sum",
        totals.yul_opt.sstore_ops_sum,
        totals.sonatina.sstore_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "keccak_ops_sum",
        totals.yul_opt.keccak_ops_sum,
        totals.sonatina.keccak_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "call_family_ops_sum",
        totals.yul_opt.call_family_ops_sum,
        totals.sonatina.call_family_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "copy_ops_sum",
        totals.yul_opt.copy_ops_sum,
        totals.sonatina.copy_ops_sum,
    );
    out.push('\n');
}

fn ratio_percent_i128(numerator: i128, denominator: i128) -> Option<f64> {
    if denominator == 0 {
        None
    } else {
        Some((numerator as f64 * 100.0) / denominator as f64)
    }
}

fn append_opcode_inflation_attribution(out: &mut String, totals: OpcodeMagnitudeTotals) {
    let runtime_ops_delta =
        totals.sonatina.runtime_ops_sum as i128 - totals.yul_opt.runtime_ops_sum as i128;
    let swap_delta = totals.sonatina.swap_ops_sum as i128 - totals.yul_opt.swap_ops_sum as i128;
    let pop_delta = totals.sonatina.pop_ops_sum as i128 - totals.yul_opt.pop_ops_sum as i128;
    let jump_delta = totals.sonatina.jump_ops_sum as i128 - totals.yul_opt.jump_ops_sum as i128;
    let jumpi_delta = totals.sonatina.jumpi_ops_sum as i128 - totals.yul_opt.jumpi_ops_sum as i128;
    let iszero_delta =
        totals.sonatina.iszero_ops_sum as i128 - totals.yul_opt.iszero_ops_sum as i128;
    let mem_rw_delta =
        totals.sonatina.mem_rw_ops_sum as i128 - totals.yul_opt.mem_rw_ops_sum as i128;
    let storage_rw_delta =
        totals.sonatina.storage_rw_ops_sum as i128 - totals.yul_opt.storage_rw_ops_sum as i128;

    let swap_pop_delta = swap_delta + pop_delta;
    let control_delta = jump_delta + jumpi_delta;
    let mem_storage_delta = mem_rw_delta + storage_rw_delta;
    let stack_control_delta = swap_pop_delta + control_delta;

    out.push_str("## Inflation Attribution Snapshot\n\n");
    out.push_str(&format!("- runtime_ops_delta: {runtime_ops_delta}\n"));
    out.push_str(&format!(
        "- swap_pop_delta: {} ({})\n",
        swap_pop_delta,
        format_percent_cell(ratio_percent_i128(swap_pop_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- control_flow_delta (jump+jumpi): {} ({})\n",
        control_delta,
        format_percent_cell(ratio_percent_i128(control_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- bool_normalization_delta (iszero): {} ({})\n",
        iszero_delta,
        format_percent_cell(ratio_percent_i128(iszero_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- stack_control_delta (swap+pop+jump+jumpi): {} ({})\n",
        stack_control_delta,
        format_percent_cell(ratio_percent_i128(stack_control_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- mem_storage_delta (mem_rw+storage_rw): {} ({})\n",
        mem_storage_delta,
        format_percent_cell(ratio_percent_i128(mem_storage_delta, runtime_ops_delta))
    ));
    out.push('\n');
}

fn append_deployment_attribution_summary(
    out: &mut String,
    heading: &str,
    totals: DeploymentGasAttributionTotals,
) {
    out.push_str(&format!("## {heading}\n\n"));
    out.push_str(&format!(
        "- compared_with_profile: {}\n",
        totals.compared_with_profile
    ));
    append_opcode_delta_metric(
        out,
        "step_total_gas_sum (vs Yul optimized)",
        totals.yul_opt_step_total_gas,
        totals.sonatina_step_total_gas,
    );
    append_opcode_delta_metric(
        out,
        "create_opcode_gas_sum (vs Yul optimized)",
        totals.yul_opt_create_opcode_gas,
        totals.sonatina_create_opcode_gas,
    );
    append_opcode_delta_metric(
        out,
        "create2_opcode_gas_sum (vs Yul optimized)",
        totals.yul_opt_create2_opcode_gas,
        totals.sonatina_create2_opcode_gas,
    );
    append_opcode_delta_metric(
        out,
        "constructor_frame_gas_sum (vs Yul optimized)",
        totals.yul_opt_constructor_frame_gas,
        totals.sonatina_constructor_frame_gas,
    );
    append_opcode_delta_metric(
        out,
        "non_constructor_frame_gas_sum (vs Yul optimized)",
        totals.yul_opt_non_constructor_frame_gas,
        totals.sonatina_non_constructor_frame_gas,
    );
    out.push('\n');
}

fn append_hotspot_summary(
    out: &mut String,
    hotspots: &[GasHotspotRow],
    suite_rollup: &[(String, SuiteDeltaTotals)],
) {
    let total_delta: i128 = hotspots.iter().map(|row| row.delta_vs_yul_opt).sum();
    let mut sorted = hotspots.to_vec();
    sorted.sort_by(|a, b| b.delta_vs_yul_opt.cmp(&a.delta_vs_yul_opt));

    out.push_str("## Top Gas Regressions (vs Yul optimized)\n\n");
    out.push_str(&format!("- rows_with_delta: {}\n", sorted.len()));
    out.push_str(&format!("- total_delta_vs_yul_opt: {}\n\n", total_delta));
    out.push_str("| rank | suite | test | delta_vs_yul_opt | pct_vs_yul_opt | share_of_total |\n");
    out.push_str("| ---: | --- | --- | ---: | ---: | ---: |\n");

    let mut cumulative: i128 = 0;
    for (idx, row) in sorted.iter().take(10).enumerate() {
        cumulative += row.delta_vs_yul_opt;
        let share = format_percent_cell(ratio_percent_i128(row.delta_vs_yul_opt, total_delta));
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.delta_vs_yul_opt,
            row.delta_vs_yul_opt_pct,
            share
        ));
    }
    out.push_str(&format!(
        "\n- top_10_cumulative_share: {}\n\n",
        format_percent_cell(ratio_percent_i128(cumulative, total_delta))
    ));

    out.push_str("## Suite Delta Rollup (vs Yul optimized)\n\n");
    out.push_str("| suite | tests_with_delta | delta_vs_yul_opt_sum | avg_delta_vs_yul_opt | share_of_total |\n");
    out.push_str("| --- | ---: | ---: | ---: | ---: |\n");
    for (suite, totals) in suite_rollup.iter().take(10) {
        let avg = if totals.tests_with_delta == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}",
                totals.delta_vs_yul_opt_sum as f64 / totals.tests_with_delta as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} |\n",
            suite,
            totals.tests_with_delta,
            totals.delta_vs_yul_opt_sum,
            avg,
            format_percent_cell(ratio_percent_i128(totals.delta_vs_yul_opt_sum, total_delta))
        ));
    }
    out.push('\n');
}

fn append_trace_symbol_hotspots_summary(out: &mut String, rows: &[TraceSymbolHotspotRow]) {
    if rows.is_empty() {
        return;
    }
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_symbol
            .cmp(&a.steps_in_symbol)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
    });

    out.push_str("## Tail Trace Symbol Attribution (Sonatina)\n\n");
    out.push_str(
        "Sampled from each suite trace artifact (`debug/*.evm_trace.txt`), which keeps the last N steps.\n\n",
    );
    out.push_str("| rank | suite | test | symbol | steps_in_symbol | symbol_share_of_tail |\n");
    out.push_str("| ---: | --- | --- | --- | ---: | ---: |\n");
    for (idx, row) in sorted.iter().take(12).enumerate() {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_symbol as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.symbol,
            row.steps_in_symbol,
            pct
        ));
    }
    out.push('\n');
}

fn append_observability_coverage_summary(
    out: &mut String,
    rows: &[ObservabilityCoverageRow],
    totals: ObservabilityCoverageTotals,
) {
    if rows.is_empty() {
        return;
    }

    let mut test_keys: FxHashSet<(String, String)> = FxHashSet::default();
    for row in rows {
        test_keys.insert((row.suite.clone(), row.test.clone()));
    }

    out.push_str("## Sonatina Observability Coverage\n\n");
    out.push_str(&format!("- observed_sections: {}\n", rows.len()));
    out.push_str(&format!("- observed_tests: {}\n", test_keys.len()));
    out.push_str(&format!("- code_bytes_total: {}\n", totals.code_bytes));
    out.push_str(&format!(
        "- mapped_code_bytes_total: {}\n",
        totals.mapped_code_bytes
    ));
    out.push_str(&format!(
        "- unmapped_code_bytes_total: {}\n",
        totals.unmapped_code_bytes
    ));
    let mapped_pct = if totals.code_bytes == 0 {
        "n/a".to_string()
    } else {
        format!(
            "{:.2}%",
            (totals.mapped_code_bytes as f64 * 100.0) / totals.code_bytes as f64
        )
    };
    let unmapped_pct = if totals.code_bytes == 0 {
        "n/a".to_string()
    } else {
        format!(
            "{:.2}%",
            (totals.unmapped_code_bytes as f64 * 100.0) / totals.code_bytes as f64
        )
    };
    out.push_str(&format!("- mapped_code_pct: {mapped_pct}\n"));
    out.push_str(&format!("- unmapped_code_pct: {unmapped_pct}\n"));
    out.push_str(&format!(
        "- unmapped_synthetic_bytes: {} ({})\n",
        totals.unmapped_synthetic,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_synthetic as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_label_or_fixup_only_bytes: {} ({})\n",
        totals.unmapped_label_or_fixup_only,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_label_or_fixup_only as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_no_ir_inst_bytes: {} ({})\n",
        totals.unmapped_no_ir_inst,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_no_ir_inst as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_unknown_bytes: {} ({})\n\n",
        totals.unmapped_unknown,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_unknown as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));

    let mut top_rows = rows.to_vec();
    top_rows.sort_by(|a, b| {
        b.unmapped_code_bytes
            .cmp(&a.unmapped_code_bytes)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.section.cmp(&b.section))
    });
    out.push_str("| rank | suite | test | section | unmapped_code_bytes | unmapped_code_pct | synthetic_share_of_unmapped |\n");
    out.push_str("| ---: | --- | --- | --- | ---: | ---: | ---: |\n");
    for (idx, row) in top_rows.iter().take(10).enumerate() {
        let unmapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.unmapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        let synthetic_share = format_percent_cell(ratio_percent_i128(
            row.unmapped_synthetic as i128,
            row.unmapped_code_bytes as i128,
        ));
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.section,
            row.unmapped_code_bytes,
            unmapped_pct,
            synthetic_share
        ));
    }
    out.push('\n');
}

fn append_trace_observability_hotspots_summary(
    out: &mut String,
    rows: &[TraceObservabilityHotspotRow],
) {
    if rows.is_empty() {
        return;
    }
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_bucket
            .cmp(&a.steps_in_bucket)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.function.cmp(&b.function))
            .then_with(|| a.reason.cmp(&b.reason))
    });

    out.push_str("## Tail Trace Observability Attribution (Sonatina)\n\n");
    out.push_str(
        "Mapped from tail trace PCs to observability PC ranges (`debug/*.evm_trace.txt` + `artifacts/tests/<test>/sonatina/observability.json`).\n\n",
    );
    out.push_str(
        "| rank | suite | test | function | reason | steps_in_bucket | bucket_share_of_tail |\n",
    );
    out.push_str("| ---: | --- | --- | --- | --- | ---: | ---: |\n");
    for (idx, row) in sorted.iter().take(12).enumerate() {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_bucket as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.function,
            row.reason,
            row.steps_in_bucket,
            pct
        ));
    }
    out.push('\n');
}

fn gas_opcode_comparison_header() -> &'static str {
    "test,symbol,yul_unopt_steps,yul_opt_steps,sonatina_steps,steps_ratio_vs_yul_unopt,steps_ratio_vs_yul_opt,yul_unopt_runtime_bytes,yul_opt_runtime_bytes,sonatina_runtime_bytes,bytes_ratio_vs_yul_unopt,bytes_ratio_vs_yul_opt,yul_unopt_runtime_ops,yul_opt_runtime_ops,sonatina_runtime_ops,ops_ratio_vs_yul_unopt,ops_ratio_vs_yul_opt,yul_unopt_stack_ops_pct,yul_opt_stack_ops_pct,sonatina_stack_ops_pct,yul_opt_swap_ops,sonatina_swap_ops,swap_ratio_vs_yul_opt,yul_opt_pop_ops,sonatina_pop_ops,pop_ratio_vs_yul_opt,yul_opt_jump_ops,sonatina_jump_ops,jump_ratio_vs_yul_opt,yul_opt_jumpi_ops,sonatina_jumpi_ops,jumpi_ratio_vs_yul_opt,yul_opt_iszero_ops,sonatina_iszero_ops,iszero_ratio_vs_yul_opt,yul_opt_mem_rw_ops,sonatina_mem_rw_ops,mem_rw_ratio_vs_yul_opt,yul_opt_storage_rw_ops,sonatina_storage_rw_ops,storage_rw_ratio_vs_yul_opt,yul_opt_keccak_ops,sonatina_keccak_ops,keccak_ratio_vs_yul_opt,yul_opt_call_family_ops,sonatina_call_family_ops,call_family_ratio_vs_yul_opt,yul_opt_copy_ops,sonatina_copy_ops,copy_ratio_vs_yul_opt,note"
}

pub(super) fn write_gas_comparison_report(
    report: &ReportContext,
    primary_backend: &str,
    opt_level: OptLevel,
    cases: &[GasComparisonCase],
    primary_measurements: &FxHashMap<String, GasMeasurement>,
) {
    let artifacts_dir = report.root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(opt_level),
    );

    let mut markdown = String::new();
    markdown.push_str("# Gas Comparison\n\n");
    markdown.push_str("Comparison of runtime test-call gas usage (`gas_used`) between Yul and Sonatina backends.\n");
    markdown.push_str(
        "`delta` columns are `sonatina - yul`; negative means Sonatina used less gas.\n\n",
    );
    markdown.push_str("| test | yul_unopt | yul_opt | sonatina | delta_vs_unopt | pct_vs_unopt | delta_vs_opt | pct_vs_opt | note |\n");
    markdown.push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");
    let mut opcode_markdown = String::new();
    opcode_markdown.push_str("# EVM Opcode/Trace Comparison\n\n");
    opcode_markdown.push_str(
        "Static runtime opcode shape and dynamic EVM step counts for the same test call.\n\n",
    );
    opcode_markdown.push_str("| test | steps_ratio_vs_opt | bytes_ratio_vs_opt | ops_ratio_vs_opt | swap_ratio_vs_opt | pop_ratio_vs_opt | jump_ratio_vs_opt | jumpi_ratio_vs_opt | iszero_ratio_vs_opt | mem_rw_ratio_vs_opt | storage_rw_ratio_vs_opt | keccak_ratio_vs_opt | call_family_ratio_vs_opt | copy_ratio_vs_opt | note |\n");
    opcode_markdown.push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");
    let mut deployment_attribution_markdown = String::new();
    deployment_attribution_markdown.push_str("# Deployment Gas Attribution (Step Replay)\n\n");
    deployment_attribution_markdown.push_str(
        "CREATE/CREATE2 opcode and constructor-frame attribution from full-step call replay on cloned EVM state.\n\n",
    );
    deployment_attribution_markdown.push_str("| test | yul_opt_create_ops_gas | sonatina_create_ops_gas | delta_create_ops_vs_opt | yul_opt_constructor_frame_gas | sonatina_constructor_frame_gas | delta_constructor_frame_vs_opt | yul_opt_non_constructor_frame_gas | sonatina_non_constructor_frame_gas | delta_non_constructor_vs_opt | note |\n");
    deployment_attribution_markdown
        .push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");

    let mut csv = String::new();
    csv.push_str(
        "test,symbol,yul_unopt_gas,yul_opt_gas,sonatina_gas,delta_vs_yul_unopt,delta_vs_yul_unopt_pct,delta_vs_yul_opt,delta_vs_yul_opt_pct,yul_unopt_status,yul_opt_status,sonatina_status,note\n",
    );
    let mut breakdown_csv = String::new();
    breakdown_csv.push_str(
        "test,symbol,yul_unopt_call_gas,yul_opt_call_gas,sonatina_call_gas,delta_call_vs_yul_unopt,delta_call_vs_yul_unopt_pct,delta_call_vs_yul_opt,delta_call_vs_yul_opt_pct,yul_unopt_deploy_gas,yul_opt_deploy_gas,sonatina_deploy_gas,delta_deploy_vs_yul_unopt,delta_deploy_vs_yul_unopt_pct,delta_deploy_vs_yul_opt,delta_deploy_vs_yul_opt_pct,yul_unopt_total_gas,yul_opt_total_gas,sonatina_total_gas,delta_total_vs_yul_unopt,delta_total_vs_yul_unopt_pct,delta_total_vs_yul_opt,delta_total_vs_yul_opt_pct,note\n",
    );
    let mut opcode_csv = String::new();
    opcode_csv.push_str(gas_opcode_comparison_header());
    opcode_csv.push('\n');
    let mut deployment_attribution_csv = String::new();
    deployment_attribution_csv.push_str(DEPLOYMENT_ATTRIBUTION_CSV_HEADER);
    deployment_attribution_csv.push('\n');

    let mut totals = GasTotals {
        tests_in_scope: cases.len(),
        ..GasTotals::default()
    };
    let mut call_magnitude_totals = GasMagnitudeTotals::default();
    let mut deploy_magnitude_totals = GasMagnitudeTotals::default();
    let mut total_magnitude_totals = GasMagnitudeTotals::default();
    let mut opcode_magnitude_totals = OpcodeMagnitudeTotals::default();
    let mut deployment_attribution_totals = DeploymentGasAttributionTotals::default();

    for case in cases {
        // In Sonatina-primary runs, comparisons are still made against Yul.
        // Persist the exact Yul source/bytecode pair used for those baselines.
        if primary_backend.eq_ignore_ascii_case("sonatina")
            && let Some(yul_case) = case.yul.as_ref()
        {
            write_yul_case_artifacts(report, yul_case);
        }

        let yul_opt = if primary_backend.eq_ignore_ascii_case("yul") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.yul
                .as_ref()
                .map(|test| measure_case_gas(test, "yul", true, true))
        };

        let yul_unopt = case
            .yul
            .as_ref()
            .map(|test| measure_case_gas(test, "yul", false, true));

        let sonatina = if primary_backend.eq_ignore_ascii_case("sonatina") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.sonatina
                .as_ref()
                .map(|test| measure_case_gas(test, "sonatina", opt_level.yul_optimize(), true))
        };

        let yul_unopt_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let yul_opt_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let sonatina_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let yul_unopt_deploy_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let yul_opt_deploy_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let sonatina_deploy_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let yul_unopt_total_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let yul_opt_total_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let sonatina_total_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let yul_unopt_cell = yul_unopt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_cell = yul_opt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_cell = sonatina_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_deploy_cell =
            yul_unopt_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_deploy_cell =
            yul_opt_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_deploy_cell =
            sonatina_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_total_cell =
            yul_unopt_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_total_cell =
            yul_opt_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_total_cell =
            sonatina_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_profile = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);
        let yul_opt_profile = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);
        let sonatina_profile = sonatina
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);

        let mut notes = Vec::new();
        if case.yul.is_none() {
            notes.push("missing yul test metadata".to_string());
        }
        if case.sonatina.is_none() {
            notes.push("missing sonatina test metadata".to_string());
        }
        if let Some(measurement) = &yul_unopt
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("yul_unopt {}", measurement.status_label()));
        }
        if let Some(measurement) = &yul_opt
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("yul_opt {}", measurement.status_label()));
        }
        if let Some(measurement) = &sonatina
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("sonatina {}", measurement.status_label()));
        }
        if yul_opt_profile.is_none() || sonatina_profile.is_none() {
            notes.push("missing step-attribution profile".to_string());
        }
        if let Some(profile) = yul_opt_profile
            && let Some(violation) = gas_profile_partition_violation("yul_opt", profile)
        {
            notes.push(violation);
        }
        if let Some(profile) = sonatina_profile
            && let Some(violation) = gas_profile_partition_violation("sonatina", profile)
        {
            notes.push(violation);
        }

        let yul_unopt_status = measurement_status(case.yul.is_some(), yul_unopt.as_ref());
        let yul_opt_status = measurement_status(case.yul.is_some(), yul_opt.as_ref());
        let sonatina_status = measurement_status(case.sonatina.is_some(), sonatina.as_ref());

        let (delta_unopt_cell, delta_unopt_pct_cell) =
            record_delta(yul_unopt_gas, sonatina_gas, &mut totals.vs_yul_unopt);
        let (delta_opt_cell, delta_opt_pct_cell) =
            record_delta(yul_opt_gas, sonatina_gas, &mut totals.vs_yul_opt);
        record_delta_magnitude(
            yul_unopt_gas,
            sonatina_gas,
            &mut call_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_gas,
            sonatina_gas,
            &mut call_magnitude_totals.vs_yul_opt,
        );
        record_delta_magnitude(
            yul_unopt_deploy_gas,
            sonatina_deploy_gas,
            &mut deploy_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_deploy_gas,
            sonatina_deploy_gas,
            &mut deploy_magnitude_totals.vs_yul_opt,
        );
        record_delta_magnitude(
            yul_unopt_total_gas,
            sonatina_total_gas,
            &mut total_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_total_gas,
            sonatina_total_gas,
            &mut total_magnitude_totals.vs_yul_opt,
        );

        let note = if notes.is_empty() {
            String::new()
        } else {
            normalize_inline_text(&notes.join("; "))
        };

        markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            yul_unopt_cell,
            yul_opt_cell,
            sonatina_cell,
            delta_unopt_cell,
            delta_unopt_pct_cell,
            delta_opt_cell,
            delta_opt_pct_cell,
            note
        ));

        csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&yul_unopt_cell),
            csv_escape(&yul_opt_cell),
            csv_escape(&sonatina_cell),
            csv_escape(&delta_unopt_cell),
            csv_escape(&delta_unopt_pct_cell),
            csv_escape(&delta_opt_cell),
            csv_escape(&delta_opt_pct_cell),
            csv_escape(yul_unopt_status),
            csv_escape(yul_opt_status),
            csv_escape(sonatina_status),
            csv_escape(&note),
        ));

        let (delta_call_unopt_cell, delta_call_unopt_pct_cell) =
            delta_cells(yul_unopt_gas, sonatina_gas);
        let (delta_call_opt_cell, delta_call_opt_pct_cell) = delta_cells(yul_opt_gas, sonatina_gas);
        let (delta_deploy_unopt_cell, delta_deploy_unopt_pct_cell) =
            delta_cells(yul_unopt_deploy_gas, sonatina_deploy_gas);
        let (delta_deploy_opt_cell, delta_deploy_opt_pct_cell) =
            delta_cells(yul_opt_deploy_gas, sonatina_deploy_gas);
        let (delta_total_unopt_cell, delta_total_unopt_pct_cell) =
            delta_cells(yul_unopt_total_gas, sonatina_total_gas);
        let (delta_total_opt_cell, delta_total_opt_pct_cell) =
            delta_cells(yul_opt_total_gas, sonatina_total_gas);
        breakdown_csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&yul_unopt_cell),
            csv_escape(&yul_opt_cell),
            csv_escape(&sonatina_cell),
            csv_escape(&delta_call_unopt_cell),
            csv_escape(&delta_call_unopt_pct_cell),
            csv_escape(&delta_call_opt_cell),
            csv_escape(&delta_call_opt_pct_cell),
            csv_escape(&yul_unopt_deploy_cell),
            csv_escape(&yul_opt_deploy_cell),
            csv_escape(&sonatina_deploy_cell),
            csv_escape(&delta_deploy_unopt_cell),
            csv_escape(&delta_deploy_unopt_pct_cell),
            csv_escape(&delta_deploy_opt_cell),
            csv_escape(&delta_deploy_opt_pct_cell),
            csv_escape(&yul_unopt_total_cell),
            csv_escape(&yul_opt_total_cell),
            csv_escape(&sonatina_total_cell),
            csv_escape(&delta_total_unopt_cell),
            csv_escape(&delta_total_unopt_pct_cell),
            csv_escape(&delta_total_opt_cell),
            csv_escape(&delta_total_opt_pct_cell),
            csv_escape(&note),
        ));

        let yul_unopt_steps = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.step_count);
        let yul_opt_steps = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.step_count);
        let sonatina_steps = sonatina
            .as_ref()
            .and_then(|measurement| measurement.step_count);

        let yul_unopt_metrics = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);
        let yul_opt_metrics = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);
        let sonatina_metrics = sonatina
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);

        let yul_unopt_bytes = yul_unopt_metrics.map(|metrics| metrics.byte_len);
        let yul_opt_bytes = yul_opt_metrics.map(|metrics| metrics.byte_len);
        let sonatina_bytes = sonatina_metrics.map(|metrics| metrics.byte_len);

        let yul_unopt_ops = yul_unopt_metrics.map(|metrics| metrics.op_count);
        let yul_opt_ops = yul_opt_metrics.map(|metrics| metrics.op_count);
        let sonatina_ops = sonatina_metrics.map(|metrics| metrics.op_count);

        let steps_ratio_unopt = ratio_cell_u64(sonatina_steps, yul_unopt_steps);
        let steps_ratio_opt = ratio_cell_u64(sonatina_steps, yul_opt_steps);
        let bytes_ratio_unopt = ratio_cell_usize(sonatina_bytes, yul_unopt_bytes);
        let bytes_ratio_opt = ratio_cell_usize(sonatina_bytes, yul_opt_bytes);
        let ops_ratio_unopt = ratio_cell_usize(sonatina_ops, yul_unopt_ops);
        let ops_ratio_opt = ratio_cell_usize(sonatina_ops, yul_opt_ops);

        let yul_opt_swap = yul_opt_metrics.map(|metrics| metrics.swap_ops);
        let sonatina_swap = sonatina_metrics.map(|metrics| metrics.swap_ops);
        let swap_ratio_opt = ratio_cell_usize(sonatina_swap, yul_opt_swap);

        let yul_opt_pop = yul_opt_metrics.map(|metrics| metrics.pop_ops);
        let sonatina_pop = sonatina_metrics.map(|metrics| metrics.pop_ops);
        let pop_ratio_opt = ratio_cell_usize(sonatina_pop, yul_opt_pop);

        let yul_opt_jump = yul_opt_metrics.map(|metrics| metrics.jump_ops);
        let sonatina_jump = sonatina_metrics.map(|metrics| metrics.jump_ops);
        let jump_ratio_opt = ratio_cell_usize(sonatina_jump, yul_opt_jump);

        let yul_opt_jumpi = yul_opt_metrics.map(|metrics| metrics.jumpi_ops);
        let sonatina_jumpi = sonatina_metrics.map(|metrics| metrics.jumpi_ops);
        let jumpi_ratio_opt = ratio_cell_usize(sonatina_jumpi, yul_opt_jumpi);

        let yul_opt_iszero = yul_opt_metrics.map(|metrics| metrics.iszero_ops);
        let sonatina_iszero = sonatina_metrics.map(|metrics| metrics.iszero_ops);
        let iszero_ratio_opt = ratio_cell_usize(sonatina_iszero, yul_opt_iszero);

        let yul_opt_mem_rw = yul_opt_metrics.map(|metrics| metrics.mem_rw_ops_total());
        let sonatina_mem_rw = sonatina_metrics.map(|metrics| metrics.mem_rw_ops_total());
        let mem_rw_ratio_opt = ratio_cell_usize(sonatina_mem_rw, yul_opt_mem_rw);

        let yul_opt_storage_rw = yul_opt_metrics.map(|metrics| metrics.storage_rw_ops_total());
        let sonatina_storage_rw = sonatina_metrics.map(|metrics| metrics.storage_rw_ops_total());
        let storage_rw_ratio_opt = ratio_cell_usize(sonatina_storage_rw, yul_opt_storage_rw);

        let yul_opt_keccak = yul_opt_metrics.map(|metrics| metrics.keccak_ops);
        let sonatina_keccak = sonatina_metrics.map(|metrics| metrics.keccak_ops);
        let keccak_ratio_opt = ratio_cell_usize(sonatina_keccak, yul_opt_keccak);

        let yul_opt_call_family = yul_opt_metrics.map(|metrics| metrics.call_family_ops_total());
        let sonatina_call_family = sonatina_metrics.map(|metrics| metrics.call_family_ops_total());
        let call_family_ratio_opt = ratio_cell_usize(sonatina_call_family, yul_opt_call_family);

        let yul_opt_copy = yul_opt_metrics.map(|metrics| metrics.copy_ops_total());
        let sonatina_copy = sonatina_metrics.map(|metrics| metrics.copy_ops_total());
        let copy_ratio_opt = ratio_cell_usize(sonatina_copy, yul_opt_copy);

        if let (Some(y_steps), Some(y_metrics), Some(s_steps), Some(s_metrics)) = (
            yul_opt_steps,
            yul_opt_metrics,
            sonatina_steps,
            sonatina_metrics,
        ) {
            opcode_magnitude_totals.compared_with_metrics += 1;
            opcode_magnitude_totals
                .yul_opt
                .add_observation(y_steps, y_metrics);
            opcode_magnitude_totals
                .sonatina
                .add_observation(s_steps, s_metrics);
        }

        opcode_markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            steps_ratio_opt,
            bytes_ratio_opt,
            ops_ratio_opt,
            swap_ratio_opt,
            pop_ratio_opt,
            jump_ratio_opt,
            jumpi_ratio_opt,
            iszero_ratio_opt,
            mem_rw_ratio_opt,
            storage_rw_ratio_opt,
            keccak_ratio_opt,
            call_family_ratio_opt,
            copy_ratio_opt,
            note
        ));

        let opcode_cells = vec![
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&u64_cell(yul_unopt_steps)),
            csv_escape(&u64_cell(yul_opt_steps)),
            csv_escape(&u64_cell(sonatina_steps)),
            csv_escape(&steps_ratio_unopt),
            csv_escape(&steps_ratio_opt),
            csv_escape(&usize_cell(yul_unopt_bytes)),
            csv_escape(&usize_cell(yul_opt_bytes)),
            csv_escape(&usize_cell(sonatina_bytes)),
            csv_escape(&bytes_ratio_unopt),
            csv_escape(&bytes_ratio_opt),
            csv_escape(&usize_cell(yul_unopt_ops)),
            csv_escape(&usize_cell(yul_opt_ops)),
            csv_escape(&usize_cell(sonatina_ops)),
            csv_escape(&ops_ratio_unopt),
            csv_escape(&ops_ratio_opt),
            csv_escape(&stack_ops_pct_cell(yul_unopt_metrics)),
            csv_escape(&stack_ops_pct_cell(yul_opt_metrics)),
            csv_escape(&stack_ops_pct_cell(sonatina_metrics)),
            csv_escape(&usize_cell(yul_opt_swap)),
            csv_escape(&usize_cell(sonatina_swap)),
            csv_escape(&swap_ratio_opt),
            csv_escape(&usize_cell(yul_opt_pop)),
            csv_escape(&usize_cell(sonatina_pop)),
            csv_escape(&pop_ratio_opt),
            csv_escape(&usize_cell(yul_opt_jump)),
            csv_escape(&usize_cell(sonatina_jump)),
            csv_escape(&jump_ratio_opt),
            csv_escape(&usize_cell(yul_opt_jumpi)),
            csv_escape(&usize_cell(sonatina_jumpi)),
            csv_escape(&jumpi_ratio_opt),
            csv_escape(&usize_cell(yul_opt_iszero)),
            csv_escape(&usize_cell(sonatina_iszero)),
            csv_escape(&iszero_ratio_opt),
            csv_escape(&usize_cell(yul_opt_mem_rw)),
            csv_escape(&usize_cell(sonatina_mem_rw)),
            csv_escape(&mem_rw_ratio_opt),
            csv_escape(&usize_cell(yul_opt_storage_rw)),
            csv_escape(&usize_cell(sonatina_storage_rw)),
            csv_escape(&storage_rw_ratio_opt),
            csv_escape(&usize_cell(yul_opt_keccak)),
            csv_escape(&usize_cell(sonatina_keccak)),
            csv_escape(&keccak_ratio_opt),
            csv_escape(&usize_cell(yul_opt_call_family)),
            csv_escape(&usize_cell(sonatina_call_family)),
            csv_escape(&call_family_ratio_opt),
            csv_escape(&usize_cell(yul_opt_copy)),
            csv_escape(&usize_cell(sonatina_copy)),
            csv_escape(&copy_ratio_opt),
            csv_escape(&note),
        ];
        opcode_csv.push_str(&opcode_cells.join(","));
        opcode_csv.push('\n');

        let yul_unopt_step_total = yul_unopt_profile.map(|profile| profile.total_step_gas);
        let yul_opt_step_total = yul_opt_profile.map(|profile| profile.total_step_gas);
        let sonatina_step_total = sonatina_profile.map(|profile| profile.total_step_gas);
        let yul_unopt_create_opcode_gas =
            yul_unopt_profile.map(|profile| profile.create_opcode_gas);
        let yul_opt_create_opcode_gas = yul_opt_profile.map(|profile| profile.create_opcode_gas);
        let sonatina_create_opcode_gas = sonatina_profile.map(|profile| profile.create_opcode_gas);
        let yul_unopt_create2_opcode_gas =
            yul_unopt_profile.map(|profile| profile.create2_opcode_gas);
        let yul_opt_create2_opcode_gas = yul_opt_profile.map(|profile| profile.create2_opcode_gas);
        let sonatina_create2_opcode_gas =
            sonatina_profile.map(|profile| profile.create2_opcode_gas);
        let yul_unopt_constructor_frame_gas =
            yul_unopt_profile.map(|profile| profile.constructor_frame_gas);
        let yul_opt_constructor_frame_gas =
            yul_opt_profile.map(|profile| profile.constructor_frame_gas);
        let sonatina_constructor_frame_gas =
            sonatina_profile.map(|profile| profile.constructor_frame_gas);
        let yul_unopt_non_constructor_frame_gas =
            yul_unopt_profile.map(|profile| profile.non_constructor_frame_gas);
        let yul_opt_non_constructor_frame_gas =
            yul_opt_profile.map(|profile| profile.non_constructor_frame_gas);
        let sonatina_non_constructor_frame_gas =
            sonatina_profile.map(|profile| profile.non_constructor_frame_gas);
        let yul_unopt_create_opcode_steps =
            yul_unopt_profile.map(|profile| profile.create_opcode_steps);
        let yul_opt_create_opcode_steps =
            yul_opt_profile.map(|profile| profile.create_opcode_steps);
        let sonatina_create_opcode_steps =
            sonatina_profile.map(|profile| profile.create_opcode_steps);
        let yul_unopt_create2_opcode_steps =
            yul_unopt_profile.map(|profile| profile.create2_opcode_steps);
        let yul_opt_create2_opcode_steps =
            yul_opt_profile.map(|profile| profile.create2_opcode_steps);
        let sonatina_create2_opcode_steps =
            sonatina_profile.map(|profile| profile.create2_opcode_steps);

        let yul_opt_create_ops_gas =
            yul_opt_profile.map(|profile| profile.create_opcode_gas + profile.create2_opcode_gas);
        let sonatina_create_ops_gas =
            sonatina_profile.map(|profile| profile.create_opcode_gas + profile.create2_opcode_gas);
        let (delta_create_ops_vs_opt, _) =
            delta_cells(yul_opt_create_ops_gas, sonatina_create_ops_gas);
        let (delta_constructor_vs_opt, _) = delta_cells(
            yul_opt_constructor_frame_gas,
            sonatina_constructor_frame_gas,
        );
        let (delta_non_constructor_vs_opt, _) = delta_cells(
            yul_opt_non_constructor_frame_gas,
            sonatina_non_constructor_frame_gas,
        );

        deployment_attribution_markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            u64_cell(yul_opt_create_ops_gas),
            u64_cell(sonatina_create_ops_gas),
            delta_create_ops_vs_opt,
            u64_cell(yul_opt_constructor_frame_gas),
            u64_cell(sonatina_constructor_frame_gas),
            delta_constructor_vs_opt,
            u64_cell(yul_opt_non_constructor_frame_gas),
            u64_cell(sonatina_non_constructor_frame_gas),
            delta_non_constructor_vs_opt,
            note,
        ));

        let deployment_attribution_row = DeploymentGasAttributionRow {
            test: case.display_name.clone(),
            symbol: case.symbol_name.clone(),
            yul_unopt_step_total_gas: yul_unopt_step_total,
            yul_opt_step_total_gas: yul_opt_step_total,
            sonatina_step_total_gas: sonatina_step_total,
            yul_unopt_create_opcode_gas,
            yul_opt_create_opcode_gas,
            sonatina_create_opcode_gas,
            yul_unopt_create2_opcode_gas,
            yul_opt_create2_opcode_gas,
            sonatina_create2_opcode_gas,
            yul_unopt_constructor_frame_gas,
            yul_opt_constructor_frame_gas,
            sonatina_constructor_frame_gas,
            yul_unopt_non_constructor_frame_gas,
            yul_opt_non_constructor_frame_gas,
            sonatina_non_constructor_frame_gas,
            yul_unopt_create_opcode_steps,
            yul_opt_create_opcode_steps,
            sonatina_create_opcode_steps,
            yul_unopt_create2_opcode_steps,
            yul_opt_create2_opcode_steps,
            sonatina_create2_opcode_steps,
            note: note.clone(),
        };
        deployment_attribution_csv.push_str(&deployment_attribution_row_to_csv_line(
            &deployment_attribution_row,
        ));
        deployment_attribution_csv.push('\n');

        if let Some((yul_opt_profile, sonatina_profile)) =
            deployment_attribution_row_profiles_for_totals(&deployment_attribution_row)
        {
            deployment_attribution_totals.add_observation(yul_opt_profile, sonatina_profile);
        }
    }

    markdown.push_str("\n## Summary\n\n");
    markdown.push_str(&format!("- tests_in_scope: {}\n", totals.tests_in_scope));
    markdown.push('\n');
    append_comparison_summary(
        &mut markdown,
        "vs Yul (unoptimized)",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    append_comparison_summary(
        &mut markdown,
        "vs Yul (optimized)",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );
    markdown.push_str("\n## Aggregate Delta Metrics\n\n");
    append_magnitude_summary(
        &mut markdown,
        "Runtime Call Gas vs Yul (unoptimized)",
        call_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Runtime Call Gas vs Yul (optimized)",
        call_magnitude_totals.vs_yul_opt,
    );
    markdown.push_str("\n## Deploy/Call/Total Breakdown\n\n");
    append_magnitude_summary(
        &mut markdown,
        "Deployment Gas vs Yul (unoptimized)",
        deploy_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Deployment Gas vs Yul (optimized)",
        deploy_magnitude_totals.vs_yul_opt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Total Gas (deploy+call) vs Yul (unoptimized)",
        total_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Total Gas (deploy+call) vs Yul (optimized)",
        total_magnitude_totals.vs_yul_opt,
    );
    append_deployment_attribution_summary(
        &mut markdown,
        "Deployment Attribution (Step Replay)",
        deployment_attribution_totals,
    );
    append_opcode_magnitude_summary(&mut markdown, opcode_magnitude_totals);

    markdown.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text(opt_level).lines() {
        markdown.push_str(&format!("- {line}\n"));
    }
    markdown.push_str(
        "\nMachine-readable aggregates: `artifacts/gas_comparison_totals.csv`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_comparison.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, and `artifacts/gas_deployment_attribution.csv`.\n",
    );
    markdown.push_str(
        "\n## Opcode/Trace Profile\n\nSee `artifacts/gas_opcode_comparison.md` and `artifacts/gas_opcode_comparison.csv` for bytecode shape and dynamic step-count diagnostics.\n",
    );

    let _ = std::fs::write(artifacts_dir.join("gas_comparison.md"), markdown);
    let _ = std::fs::write(artifacts_dir.join("gas_comparison.csv"), csv);
    let _ = std::fs::write(
        artifacts_dir.join("gas_breakdown_comparison.csv"),
        breakdown_csv,
    );
    let _ = std::fs::write(
        artifacts_dir.join("gas_opcode_comparison.md"),
        opcode_markdown,
    );
    let _ = std::fs::write(artifacts_dir.join("gas_opcode_comparison.csv"), opcode_csv);
    let _ = std::fs::write(
        artifacts_dir.join("gas_deployment_attribution.md"),
        deployment_attribution_markdown,
    );
    let _ = std::fs::write(
        artifacts_dir.join("gas_deployment_attribution.csv"),
        deployment_attribution_csv,
    );
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);
    write_gas_magnitude_csv(
        &artifacts_dir.join("gas_comparison_magnitude.csv"),
        call_magnitude_totals,
    );
    write_gas_breakdown_magnitude_csv(
        &artifacts_dir.join("gas_breakdown_magnitude.csv"),
        call_magnitude_totals,
        deploy_magnitude_totals,
        total_magnitude_totals,
    );
    write_opcode_magnitude_csv(
        &artifacts_dir.join("gas_opcode_magnitude.csv"),
        opcode_magnitude_totals,
    );
}

pub(super) fn write_run_gas_comparison_summary(root_dir: &Utf8PathBuf, opt_level: OptLevel) {
    let artifacts_dir = root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(opt_level),
    );

    let mut suite_dirs: Vec<(String, Utf8PathBuf)> = Vec::new();
    for status_dir in ["passed", "failed"] {
        let dir = root_dir.join(status_dir);
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let suite = entry.file_name().to_string_lossy().to_string();
            if let Ok(path) = Utf8PathBuf::from_path_buf(path) {
                suite_dirs.push((suite, path));
            }
        }
    }
    suite_dirs.sort_by(|a, b| a.0.cmp(&b.0));

    let mut all_rows = String::new();
    all_rows.push_str("suite,test,symbol,yul_unopt_gas,yul_opt_gas,sonatina_gas,delta_vs_yul_unopt,delta_vs_yul_unopt_pct,delta_vs_yul_opt,delta_vs_yul_opt_pct,yul_unopt_status,yul_opt_status,sonatina_status,note\n");
    let mut all_breakdown_rows = String::new();
    all_breakdown_rows.push_str("suite,test,symbol,yul_unopt_call_gas,yul_opt_call_gas,sonatina_call_gas,delta_call_vs_yul_unopt,delta_call_vs_yul_unopt_pct,delta_call_vs_yul_opt,delta_call_vs_yul_opt_pct,yul_unopt_deploy_gas,yul_opt_deploy_gas,sonatina_deploy_gas,delta_deploy_vs_yul_unopt,delta_deploy_vs_yul_unopt_pct,delta_deploy_vs_yul_opt,delta_deploy_vs_yul_opt_pct,yul_unopt_total_gas,yul_opt_total_gas,sonatina_total_gas,delta_total_vs_yul_unopt,delta_total_vs_yul_unopt_pct,delta_total_vs_yul_opt,delta_total_vs_yul_opt_pct,note\n");
    let mut all_opcode_rows = String::new();
    all_opcode_rows.push_str("suite,");
    all_opcode_rows.push_str(gas_opcode_comparison_header());
    all_opcode_rows.push('\n');
    let mut all_deployment_attr_rows = String::new();
    all_deployment_attr_rows.push_str(DEPLOYMENT_ATTRIBUTION_CSV_HEADER_WITH_SUITE);
    all_deployment_attr_rows.push('\n');
    let mut wrote_any_rows = false;
    let mut wrote_any_breakdown_rows = false;
    let mut wrote_any_opcode_rows = false;
    let mut wrote_any_deployment_attr_rows = false;
    let mut totals = GasTotals::default();
    let mut call_magnitude_totals = GasMagnitudeTotals::default();
    let mut deploy_magnitude_totals = GasMagnitudeTotals::default();
    let mut total_magnitude_totals = GasMagnitudeTotals::default();
    let mut opcode_magnitude_totals = OpcodeMagnitudeTotals::default();
    let mut deployment_attribution_totals = DeploymentGasAttributionTotals::default();
    let mut hotspots: Vec<GasHotspotRow> = Vec::new();
    let mut suite_rollup: FxHashMap<String, SuiteDeltaTotals> = FxHashMap::default();
    let mut trace_symbol_hotspots: Vec<TraceSymbolHotspotRow> = Vec::new();
    let mut observability_coverage_rows: Vec<ObservabilityCoverageRow> = Vec::new();
    let mut observability_coverage_totals = ObservabilityCoverageTotals::default();
    let mut trace_observability_hotspots: Vec<TraceObservabilityHotspotRow> = Vec::new();

    for (suite, suite_dir) in suite_dirs {
        let (suite_observability_rows, suite_observability_ranges) =
            load_suite_observability_runtime(&suite_dir, &suite);
        for row in suite_observability_rows {
            observability_coverage_totals.add_row(&row);
            observability_coverage_rows.push(row);
        }

        let suite_rows_path = suite_dir.join("artifacts").join("gas_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                let fields = parse_csv_fields(line);
                if fields.len() >= 9 {
                    let yul_opt_gas = parse_optional_u64_cell(&fields[3]);
                    let sonatina_gas = parse_optional_u64_cell(&fields[4]);
                    if let Some(delta_vs_yul_opt) = parse_optional_i128_cell(&fields[7]) {
                        hotspots.push(GasHotspotRow {
                            suite: suite.clone(),
                            test: fields[0].clone(),
                            symbol: fields[1].clone(),
                            yul_opt_gas,
                            sonatina_gas,
                            delta_vs_yul_opt,
                            delta_vs_yul_opt_pct: fields[8].clone(),
                        });
                        let entry = suite_rollup.entry(suite.clone()).or_default();
                        entry.tests_with_delta += 1;
                        entry.delta_vs_yul_opt_sum += delta_vs_yul_opt;
                    }
                }
                all_rows.push_str(&csv_escape(&suite));
                all_rows.push(',');
                all_rows.push_str(line);
                all_rows.push('\n');
                wrote_any_rows = true;
            }
        }

        let suite_breakdown_rows_path = suite_dir
            .join("artifacts")
            .join("gas_breakdown_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_breakdown_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                all_breakdown_rows.push_str(&csv_escape(&suite));
                all_breakdown_rows.push(',');
                all_breakdown_rows.push_str(line);
                all_breakdown_rows.push('\n');
                wrote_any_breakdown_rows = true;
            }
        }

        let suite_opcode_rows_path = suite_dir
            .join("artifacts")
            .join("gas_opcode_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_opcode_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                all_opcode_rows.push_str(&csv_escape(&suite));
                all_opcode_rows.push(',');
                all_opcode_rows.push_str(line);
                all_opcode_rows.push('\n');
                wrote_any_opcode_rows = true;
            }
        }

        let suite_deployment_attr_rows_path = suite_dir
            .join("artifacts")
            .join("gas_deployment_attribution.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_deployment_attr_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                let fields = parse_csv_fields(line);
                if let Some(row) = parse_deployment_attribution_row(&fields)
                    && let Some((yul_opt_profile, sonatina_profile)) =
                        deployment_attribution_row_profiles_for_totals(&row)
                {
                    deployment_attribution_totals
                        .add_observation(yul_opt_profile, sonatina_profile);
                }
                all_deployment_attr_rows.push_str(&csv_escape(&suite));
                all_deployment_attr_rows.push(',');
                all_deployment_attr_rows.push_str(line);
                all_deployment_attr_rows.push('\n');
                wrote_any_deployment_attr_rows = true;
            }
        }

        let suite_totals_path = suite_dir
            .join("artifacts")
            .join("gas_comparison_totals.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_totals_path) {
            totals.add(parse_gas_totals_csv(&contents));
        }

        let suite_magnitude_path = suite_dir
            .join("artifacts")
            .join("gas_comparison_magnitude.csv");
        let mut has_legacy_call_magnitude = false;
        if let Ok(contents) = std::fs::read_to_string(&suite_magnitude_path) {
            call_magnitude_totals.add(parse_gas_magnitude_csv(&contents));
            has_legacy_call_magnitude = true;
        }

        let suite_breakdown_magnitude_path = suite_dir
            .join("artifacts")
            .join("gas_breakdown_magnitude.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_breakdown_magnitude_path) {
            let (call, deploy, total) = parse_gas_breakdown_magnitude_csv(&contents);
            if !has_legacy_call_magnitude {
                call_magnitude_totals.add(call);
            }
            deploy_magnitude_totals.add(deploy);
            total_magnitude_totals.add(total);
        }

        let suite_opcode_magnitude_path =
            suite_dir.join("artifacts").join("gas_opcode_magnitude.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_opcode_magnitude_path) {
            opcode_magnitude_totals.add(parse_gas_opcode_magnitude_csv(&contents));
        }

        let debug_dir = suite_dir.join("debug");
        let symtab_entries = std::fs::read_to_string(debug_dir.join("sonatina_symtab.txt"))
            .ok()
            .map(|contents| parse_symtab_entries(&contents))
            .unwrap_or_default();
        if let Ok(entries) = std::fs::read_dir(&debug_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }
                let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
                    continue;
                };
                if !file_name.ends_with(".evm_trace.txt") {
                    continue;
                }
                let Ok(contents) = std::fs::read_to_string(&path) else {
                    continue;
                };
                let pcs = parse_trace_tail_pcs(&contents);
                if pcs.is_empty() {
                    continue;
                }
                let mut counts: FxHashMap<String, usize> = FxHashMap::default();
                let mut mapped = 0usize;
                if !symtab_entries.is_empty() {
                    for pc in &pcs {
                        if let Some(symbol) = map_pc_to_symbol(*pc, &symtab_entries) {
                            *counts.entry(symbol.to_string()).or_default() += 1;
                            mapped += 1;
                        }
                    }
                }
                let test_name = file_name
                    .strip_suffix(".evm_trace.txt")
                    .unwrap_or(file_name)
                    .to_string();
                if !counts.is_empty() {
                    for (symbol, steps_in_symbol) in counts {
                        trace_symbol_hotspots.push(TraceSymbolHotspotRow {
                            suite: suite.clone(),
                            test: test_name.clone(),
                            symbol,
                            tail_steps_total: pcs.len(),
                            tail_steps_mapped: mapped,
                            steps_in_symbol,
                        });
                    }
                }

                if let Some(pc_ranges) = resolve_observability_test_ranges(
                    &suite,
                    &test_name,
                    &suite_observability_ranges,
                ) {
                    let mut obs_counts: FxHashMap<(String, String), usize> = FxHashMap::default();
                    let mut obs_mapped = 0usize;
                    for pc in &pcs {
                        if let Some(range) = map_pc_to_observability(*pc, pc_ranges) {
                            obs_mapped += 1;
                            let reason =
                                range.reason.clone().unwrap_or_else(|| "mapped".to_string());
                            *obs_counts
                                .entry((range.func_name.clone(), reason))
                                .or_default() += 1;
                        } else {
                            *obs_counts
                                .entry(("<unmapped_pc>".to_string(), "pc_not_in_map".to_string()))
                                .or_default() += 1;
                        }
                    }

                    for ((function, reason), steps_in_bucket) in obs_counts {
                        trace_observability_hotspots.push(TraceObservabilityHotspotRow {
                            suite: suite.clone(),
                            test: test_name.clone(),
                            function,
                            reason,
                            tail_steps_total: pcs.len(),
                            tail_steps_mapped: obs_mapped,
                            steps_in_bucket,
                        });
                    }
                } else {
                    trace_observability_hotspots.push(TraceObservabilityHotspotRow {
                        suite: suite.clone(),
                        test: test_name.clone(),
                        function: "<unmatched_test_name>".to_string(),
                        reason: "no_observability_test_match".to_string(),
                        tail_steps_total: pcs.len(),
                        tail_steps_mapped: 0,
                        steps_in_bucket: pcs.len(),
                    });
                }
            }
        }
    }

    let mut suite_rollup_rows: Vec<(String, SuiteDeltaTotals)> = suite_rollup.into_iter().collect();
    suite_rollup_rows.sort_by(|a, b| {
        b.1.delta_vs_yul_opt_sum
            .cmp(&a.1.delta_vs_yul_opt_sum)
            .then_with(|| a.0.cmp(&b.0))
    });

    if wrote_any_rows {
        let _ = std::fs::write(artifacts_dir.join("gas_comparison_all.csv"), all_rows);
        write_gas_hotspots_csv(
            &artifacts_dir.join("gas_hotspots_vs_yul_opt.csv"),
            &hotspots,
        );
        write_suite_delta_summary_csv(
            &artifacts_dir.join("gas_suite_delta_summary.csv"),
            &suite_rollup_rows,
        );
        write_trace_symbol_hotspots_csv(
            &artifacts_dir.join("gas_tail_trace_symbol_hotspots.csv"),
            &trace_symbol_hotspots,
        );
        if !trace_observability_hotspots.is_empty() {
            write_trace_observability_hotspots_csv(
                &artifacts_dir.join("gas_tail_trace_observability_hotspots.csv"),
                &trace_observability_hotspots,
            );
        }
    }
    if !observability_coverage_rows.is_empty() {
        write_observability_coverage_csv(
            &artifacts_dir.join("observability_coverage_all.csv"),
            &observability_coverage_rows,
        );
    }
    if wrote_any_breakdown_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_breakdown_comparison_all.csv"),
            all_breakdown_rows,
        );
    }
    if wrote_any_opcode_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_opcode_comparison_all.csv"),
            all_opcode_rows,
        );
    }
    if wrote_any_deployment_attr_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_deployment_attribution_all.csv"),
            all_deployment_attr_rows,
        );
    }
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);
    write_gas_magnitude_csv(
        &artifacts_dir.join("gas_comparison_magnitude.csv"),
        call_magnitude_totals,
    );
    write_gas_breakdown_magnitude_csv(
        &artifacts_dir.join("gas_breakdown_magnitude.csv"),
        call_magnitude_totals,
        deploy_magnitude_totals,
        total_magnitude_totals,
    );
    write_opcode_magnitude_csv(
        &artifacts_dir.join("gas_opcode_magnitude.csv"),
        opcode_magnitude_totals,
    );

    let mut summary = String::new();
    summary.push_str("# Gas Comparison Summary\n\n");
    summary.push_str("Aggregated totals across all suite reports in this archive.\n\n");
    summary.push_str(&format!("- tests_in_scope: {}\n", totals.tests_in_scope));
    summary.push('\n');
    append_comparison_summary(
        &mut summary,
        "vs Yul (unoptimized)",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    append_comparison_summary(
        &mut summary,
        "vs Yul (optimized)",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );
    summary.push_str("\n## Aggregate Delta Metrics\n\n");
    append_magnitude_summary(
        &mut summary,
        "Runtime Call Gas vs Yul (unoptimized)",
        call_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Runtime Call Gas vs Yul (optimized)",
        call_magnitude_totals.vs_yul_opt,
    );
    summary.push_str("\n## Deploy/Call/Total Breakdown\n\n");
    append_magnitude_summary(
        &mut summary,
        "Deployment Gas vs Yul (unoptimized)",
        deploy_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Deployment Gas vs Yul (optimized)",
        deploy_magnitude_totals.vs_yul_opt,
    );
    append_magnitude_summary(
        &mut summary,
        "Total Gas (deploy+call) vs Yul (unoptimized)",
        total_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Total Gas (deploy+call) vs Yul (optimized)",
        total_magnitude_totals.vs_yul_opt,
    );
    append_deployment_attribution_summary(
        &mut summary,
        "Deployment Attribution (Step Replay, vs Yul optimized)",
        deployment_attribution_totals,
    );
    append_opcode_magnitude_summary(&mut summary, opcode_magnitude_totals);
    append_opcode_inflation_attribution(&mut summary, opcode_magnitude_totals);
    if wrote_any_rows {
        append_hotspot_summary(&mut summary, &hotspots, &suite_rollup_rows);
        append_trace_symbol_hotspots_summary(&mut summary, &trace_symbol_hotspots);
        append_trace_observability_hotspots_summary(&mut summary, &trace_observability_hotspots);
    }
    append_observability_coverage_summary(
        &mut summary,
        &observability_coverage_rows,
        observability_coverage_totals,
    );
    summary.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text(opt_level).lines() {
        summary.push_str(&format!("- {line}\n"));
    }
    if wrote_any_rows {
        summary.push_str("\nSee `artifacts/gas_comparison_all.csv`, `artifacts/gas_comparison_totals.csv`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_comparison_all.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, `artifacts/gas_deployment_attribution_all.csv`, `artifacts/gas_hotspots_vs_yul_opt.csv`, `artifacts/gas_suite_delta_summary.csv`, `artifacts/gas_tail_trace_symbol_hotspots.csv`, and `artifacts/gas_tail_trace_observability_hotspots.csv` for machine-readable totals and rollups.\n");
    }
    if !observability_coverage_rows.is_empty() {
        summary.push_str(
            "See `artifacts/observability_coverage_all.csv` for per-test Sonatina observability coverage totals.\n",
        );
    }
    if wrote_any_opcode_rows {
        summary.push_str(
            "See `artifacts/gas_opcode_comparison_all.csv` for aggregated opcode and step-count diagnostics.\n",
        );
    }
    let _ = std::fs::write(artifacts_dir.join("gas_comparison_summary.md"), summary);
}
