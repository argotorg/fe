#![allow(clippy::print_stderr)]

mod test_helpers;
use test_helpers::*;

use common::InputDb;
use common::ir_describe::{DescribeCtx, IrDescribe};

// --- Test 9: Full pipeline: IrDescribe → FactConsumer → Cozo → Datalog query ---

#[cfg(feature = "datalog")]
#[test]
fn full_pipeline_ir_describe_to_cozo_query() {
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let cx = DescribeCtx::new(&a.db);

    let mut all_facts = Vec::new();
    let mut next_id: u32 = 0;
    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        let mut fc = common::fact_consumer::FactConsumer::with_starting_id(next_id);
        body.describe(&cx, &mut fc);
        next_id = fc.next_id();
        all_facts.extend(fc.into_facts());
    }

    assert!(
        all_facts.len() > 100,
        "should produce many facts from ERC20-like contract"
    );

    // Ingest into Cozo
    let trace = fe_codegen::trace::CompilationTrace::new();
    trace.ingest(&all_facts);

    // Runtime Datalog queries over real compiled code
    let nodes = trace
        .query("?[node_id, kind] := *node_hash[node_id, kind, _, _, _]")
        .expect("query nodes");
    assert!(!nodes.rows.is_empty(), "should have node_hash facts");

    let origins = trace
        .query("?[node_id, level] := *origin[node_id, level, _, _]")
        .expect("query origins");
    assert!(!origins.rows.is_empty(), "should have origin facts");

    // Cross-join: nodes with origins
    let nodes_with_origins = trace.query(
        "?[node_id, kind, level] := *node_hash[node_id, kind, _, _, _], *origin[node_id, level, _, _]"
    ).expect("cross join");
    assert!(
        !nodes_with_origins.rows.is_empty(),
        "should have nodes with origins"
    );

    eprintln!(
        "Full pipeline: {} total facts, {} nodes, {} origins, {} joined",
        all_facts.len(),
        nodes.rows.len(),
        origins.rows.len(),
        nodes_with_origins.rows.len()
    );
}

// --- Test: node_effect facts enable security analysis queries ---

#[cfg(feature = "datalog")]
#[test]
fn node_effects_trace_storage_writes_to_source() {
    use common::fact_consumer::FactConsumer;

    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");
    let cx = DescribeCtx::new(&a.db);

    let mut all_facts = Vec::new();
    let mut next_id: u32 = 0;
    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        let mut fc = FactConsumer::with_starting_id(next_id);
        body.describe(&cx, &mut fc);
        next_id = fc.next_id();
        all_facts.extend(fc.into_facts());
    }

    let effect_facts: Vec<_> = all_facts
        .iter()
        .filter(|f| matches!(f, common::fact_consumer::Fact::NodeEffect { .. }))
        .collect();
    assert!(
        !effect_facts.is_empty(),
        "ERC20 contract must produce at least one NodeEffect fact"
    );

    let trace = fe_codegen::trace::CompilationTrace::new();
    trace.ingest(&all_facts);

    // Query: find all storage_write nodes
    let writes = trace
        .query("?[node_id] := *node_effect[node_id, 'storage_write']")
        .expect("query storage writes");

    // The ERC20 Transfer function does store.balances.set() twice → storage writes
    // These lower through ABI helpers that eventually emit SSTORE builtins
    eprintln!("storage_write nodes: {}", writes.rows.len());

    // Query: find all distinct effects
    let effects = trace
        .query("?[effect, count(node_id)] := *node_effect[node_id, effect]")
        .expect("query effects");
    eprintln!("Effect summary:");
    for row in &effects.rows {
        eprintln!("  {} — {} occurrences", row[0], row[1]);
    }

    // Query: storage writes that have provenance back to source
    let writes_with_source = trace
        .query(
            "?[node_id, file, line] := *node_effect[node_id, 'storage_write'], \
         *origin[node_id, _, _, _], \
         *source_span[node_id, file, line, _, _, _]",
        )
        .expect("query writes with source");
    eprintln!(
        "storage_write nodes with source location: {}",
        writes_with_source.rows.len()
    );

    // Query: storage reads
    let reads = trace
        .query("?[node_id] := *node_effect[node_id, 'storage_read']")
        .expect("query storage reads");
    eprintln!("storage_read nodes: {}", reads.rows.len());

    assert!(
        !effects.rows.is_empty(),
        "ERC20 contract must emit at least one effect type"
    );
}

// --- Test: data_flow edges enable taint tracking ---

#[cfg(feature = "datalog")]
#[test]
fn data_flow_edges_track_def_use_chains() {
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");
    let cx = DescribeCtx::new(&a.db);

    let mut all_facts = Vec::new();
    let mut next_id: u32 = 0;
    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        let mut fc = common::fact_consumer::FactConsumer::with_starting_id(next_id);
        body.describe(&cx, &mut fc);
        next_id = fc.next_id();
        all_facts.extend(fc.into_facts());
    }

    let flow_facts: Vec<_> = all_facts
        .iter()
        .filter(|f| matches!(f, common::fact_consumer::Fact::DataFlowEdge { .. }))
        .collect();

    assert!(
        !flow_facts.is_empty(),
        "ERC20 contract must produce data_flow edges (def→use chains)"
    );
    eprintln!("data_flow edges: {}", flow_facts.len());

    let trace = fe_codegen::trace::CompilationTrace::new();
    trace.ingest(&all_facts);

    // Query: count total data_flow edges
    let flows = trace
        .query("?[count(from_id)] := *data_flow[from_id, _]")
        .expect("count data_flow");
    eprintln!("Total data_flow edges in Cozo: {:?}", flows.rows);

    // Query: find transitive data flow (2-hop chains)
    let chains = trace
        .query("?[a, b, c] := *data_flow[a, b], *data_flow[b, c], a != c :limit 10")
        .expect("2-hop chains");
    eprintln!("2-hop data flow chains (sample): {}", chains.rows.len());

    assert!(
        !chains.rows.is_empty(),
        "ERC20 should have multi-hop data flow chains (value flows through computation)"
    );
}

// --- Test: storage effects are emitted for storage-touching operations ---

#[cfg(feature = "datalog")]
#[test]
fn storage_effects_emitted_for_reads_and_writes() {
    let a = analyze(BASE_CONTRACT);
    let trace = fe_codegen::trace::CompilationTrace::new();
    trace.ingest(&a.facts);

    let effects = trace
        .query("?[effect, count(node_id)] := *node_effect[node_id, effect]")
        .expect("query effects");
    eprintln!("Effect distribution:");
    for row in &effects.rows {
        eprintln!("  {}: {}", row[0], row[1]);
    }

    let writes = trace
        .query("?[node_id] := *node_effect[node_id, 'storage_write']")
        .expect("storage_write");
    assert!(
        !writes.rows.is_empty(),
        "ERC20 contract with store.balances.set() must emit storage_write effects"
    );

    let reads = trace
        .query("?[node_id] := *node_effect[node_id, 'storage_read']")
        .expect("storage_read");
    assert!(
        !reads.rows.is_empty(),
        "ERC20 contract with store.balances.get() must emit storage_read effects"
    );

    let refs = trace
        .query("?[node_id] := *node_effect[node_id, 'storage_ref']")
        .expect("storage_ref");
    eprintln!("storage_ref nodes: {}", refs.rows.len());

    let reverts = trace
        .query("?[node_id] := *node_effect[node_id, 'revert']")
        .expect("revert");
    eprintln!("revert nodes: {}", reverts.rows.len());
}

// --- Test: revert effects emitted for assert/overflow ---

#[cfg(feature = "datalog")]
#[test]
fn revert_effects_emitted() {
    // The BASE_CONTRACT uses checked arithmetic (x + y) which generates
    // overflow checks that revert on failure. Fe also generates reverts
    // for ABI decode failures. Both should produce revert effects.
    let a = analyze(BASE_CONTRACT);
    let trace = fe_codegen::trace::CompilationTrace::new();
    trace.ingest(&a.facts);

    let reverts = trace
        .query("?[node_id] := *node_effect[node_id, 'revert']")
        .expect("revert");
    assert!(
        !reverts.rows.is_empty(),
        "ERC20 contract with checked arithmetic must emit revert effects \
         (overflow checks, ABI bounds checks)"
    );
    eprintln!("revert nodes: {}", reverts.rows.len());
}

// --- SecurityAnalyzer integration tests ---

#[cfg(feature = "datalog")]
#[test]
fn security_analyzer_runs_on_erc20() {
    use fe_codegen::security::SecurityAnalyzer;

    let a = analyze(BASE_CONTRACT);
    let analyzer = SecurityAnalyzer::from_analysis(&a);
    let findings = analyzer.run_all();

    eprintln!("SecurityAnalyzer findings on ERC20:");
    for f in &findings {
        eprintln!(
            "  [{:?}] {}: {} (nodes: {:?})",
            f.severity, f.detector, f.description, f.node_ids
        );
    }
    eprintln!("Total findings: {}", findings.len());

    let eliminated = fe_codegen::security::SecurityAnalyzer::fe_eliminated_by_design();
    eprintln!("\nFe eliminates by design:");
    for e in eliminated {
        eprintln!("  - {e}");
    }
    assert!(eliminated.len() >= 5);
}

#[cfg(feature = "datalog")]
#[test]
fn tainted_calldata_flows_to_storage() {
    use fe_codegen::security::SecurityAnalyzer;

    let a = analyze(BASE_CONTRACT);
    let analyzer = SecurityAnalyzer::from_analysis(&a);
    let taint = analyzer.detect_tainted_calldata_to_storage();

    eprintln!("Tainted calldata→storage flows: {}", taint.len());
    for f in &taint {
        eprintln!("  src={}, sink={}", f.node_ids[0], f.node_ids[1]);
    }
}
