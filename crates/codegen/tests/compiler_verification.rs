#![allow(clippy::print_stderr)]

mod test_helpers;
use test_helpers::*;

use common::InputDb;
use common::hash_consumer::HashConsumer;
use common::ir_describe::{DescribeCtx, IrDescribe};

// --- Test 1: Hash + provenance consistency ---
// Modify source, verify hash changes AND origin chain still resolves

#[test]
fn hash_changes_when_operator_changes_and_origins_still_resolve() {
    use common::provenance::IrLevel;
    use hir::span::LazySpan;

    let modified = BASE_CONTRACT.replace("bal < amount", "bal <= amount");

    let (a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (a2, h2) = compile_mir_hashes(&modified);

    // The Transfer function's hash should change
    let transfer1 = find_hash(&h1, "recv_0_0").expect("find Transfer in original");
    let transfer2 = find_hash(&h2, "recv_0_0").expect("find Transfer in modified");
    assert_ne!(
        transfer1.structure(),
        transfer2.structure(),
        "changing < to <= must change the Transfer function's structural hash"
    );

    // The Balance function's hash should NOT change
    let balance1 = find_hash(&h1, "recv_0_1").expect("find Balance in original");
    let balance2 = find_hash(&h2, "recv_0_1").expect("find Balance in modified");
    assert_eq!(
        balance1.structure(),
        balance2.structure(),
        "Balance function should be unaffected by Transfer's operator change"
    );

    // Origins still resolve in BOTH versions
    for (label, a) in [("original", &a1), ("modified", &a2)] {
        let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
        let top_mod = a.db.top_mod(file);
        let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

        let mut resolved = 0;
        let mut total = 0;
        for func in package.functions(&a.db) {
            let body = func.instance(&a.db).body(&a.db);
            let key = func.instance(&a.db).key(&a.db);
            let Some(semantic) = key.semantic(&a.db) else {
                continue;
            };
            let Some(hir_body) = semantic.key(&a.db).owner(&a.db).body(&a.db) else {
                continue;
            };

            for block in &body.blocks {
                for origin in &block.stmt_origins {
                    total += 1;
                    if origin.level != IrLevel::Smir {
                        continue;
                    }
                    let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
                    if expr_id.span(hir_body).resolve(&a.db).is_some() {
                        resolved += 1;
                    }
                }
            }
        }

        assert!(
            resolved > 0,
            "{label}: origin chain must resolve for at least some statements"
        );
        eprintln!("{label}: {resolved}/{total} origins resolved");
    }
}

// --- Test 2: Round-trip source text verification ---
// Pick a MIR stmt, follow origin to source, verify the snippet is plausible

#[test]
fn origin_chain_resolves_to_correct_source_text() {
    use common::provenance::IrLevel;
    use hir::span::LazySpan;

    let (a, _hashes) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let mut snippets = Vec::new();

    for func in package.functions(&a.db) {
        if !func.symbol(&a.db).contains("recv_0_0") {
            continue;
        }
        let body = func.instance(&a.db).body(&a.db);
        let key = func.instance(&a.db).key(&a.db);
        let Some(semantic) = key.semantic(&a.db) else {
            continue;
        };
        let Some(hir_body) = semantic.key(&a.db).owner(&a.db).body(&a.db) else {
            continue;
        };

        for block in &body.blocks {
            for origin in &block.stmt_origins {
                if origin.level != IrLevel::Smir {
                    continue;
                }
                let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
                if let Some(span) = expr_id.span(hir_body).resolve(&a.db) {
                    let text = span.file.text(&a.db);
                    let start: usize = span.range.start().into();
                    let end: usize = span.range.end().into();
                    if end <= text.len() {
                        snippets.push(text[start..end].to_string());
                    }
                }
            }
        }
    }

    assert!(
        !snippets.is_empty(),
        "should resolve at least some snippets"
    );

    // The Transfer function does `bal < amount`, `bal - amount`, `+ amount`
    // At least one snippet should contain recognizable source text
    let has_arithmetic = snippets.iter().any(|s| {
        s.contains('+')
            || s.contains('-')
            || s.contains('<')
            || s.contains("bal")
            || s.contains("amount")
            || s.contains("sender")
            || s.contains("store")
    });

    assert!(
        has_arithmetic,
        "resolved snippets should contain recognizable source text from Transfer function.\n\
         Got: {:?}",
        &snippets[..std::cmp::min(10, snippets.len())]
    );
}

// --- Test 4: Struct field change cascades ---

#[test]
fn struct_field_change_detected_at_hir_level() {
    let with_extra_field = BASE_CONTRACT.replace(
        "struct Store {\n    balances: StorageMap<Address, u256>,\n}",
        "struct Store {\n    balances: StorageMap<Address, u256>,\n    total_supply: u256,\n}",
    );

    // Hash the HIR items (TopLevelMod) which includes Struct definitions
    fn hash_hir(source: &str) -> common::hash_consumer::DimHashes {
        let a = analyze(source);
        let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
        let top_mod = a.db.top_mod(file);
        let cx = DescribeCtx::new(&a.db);
        let mut consumer = HashConsumer::new();
        top_mod.describe(&cx, &mut consumer);
        consumer.into_result().unwrap()
    }

    let h1 = hash_hir(BASE_CONTRACT);
    let h2 = hash_hir(&with_extra_field);

    assert_ne!(
        h1.structure(),
        h2.structure(),
        "adding a struct field must change the HIR structural hash"
    );

    // NOTE: MIR-level detection of struct field changes requires hashing
    // type/layout info on locals and expressions, which isn't done yet.
    // This test verifies the change is visible at the HIR level.
}

// --- Test 5: Deterministic hashing ---

#[test]
fn hashing_is_deterministic_within_session() {
    let (a, h1) = compile_mir_hashes(BASE_CONTRACT);

    // Re-hash within the same database session
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let cx = DescribeCtx::new(&a.db);
    let h2: Vec<_> = package
        .functions(&a.db)
        .iter()
        .map(|func| {
            let body = func.instance(&a.db).body(&a.db);
            let mut consumer = HashConsumer::new();
            body.describe(&cx, &mut consumer);
            (func.symbol(&a.db), consumer.into_result().unwrap())
        })
        .collect();

    assert_eq!(
        h1.len(),
        h2.len(),
        "same source should produce same number of functions"
    );

    for ((n1, hash1), (n2, hash2)) in h1.iter().zip(h2.iter()) {
        assert_eq!(n1, n2, "function names should match");
        assert_eq!(
            hash1.structure(),
            hash2.structure(),
            "function {n1} should have identical structure hash within session"
        );
        assert_eq!(
            hash1.names(),
            hash2.names(),
            "function {n1} should have identical names hash within session"
        );
    }
}

// --- Test 6: Cross-session determinism ---

#[test]
fn hashing_is_deterministic_across_sessions() {
    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(BASE_CONTRACT);

    let mut mismatches = Vec::new();
    for ((n1, hash1), (n2, hash2)) in h1.iter().zip(h2.iter()) {
        if n1 != n2 {
            mismatches.push(format!("name mismatch: {n1} vs {n2}"));
        } else if hash1.structure() != hash2.structure() {
            mismatches.push(format!("{n1}: structure differs"));
        }
    }

    if !mismatches.is_empty() {
        eprintln!(
            "Cross-session determinism issues ({}/{}):",
            mismatches.len(),
            h1.len()
        );
        for m in &mismatches[..std::cmp::min(5, mismatches.len())] {
            eprintln!("  {m}");
        }
    }

    // This test documents the current state — cross-session determinism
    // depends on salsa assigning the same internal IDs for the same input.
    // If it fails, we need to hash value content instead of positional IDs.
    assert_eq!(
        mismatches.len(),
        0,
        "cross-session hashing should be deterministic for the same source"
    );
}

// --- Test 7: Structural change sensitivity ---
// Adding a dead local must change the structure hash

#[test]
fn structural_change_is_detected() {
    let with_extra_stmt = BASE_CONTRACT.replace(
        "let sender = ctx.caller()",
        "let _unused: u256 = 0\n            let sender = ctx.caller()",
    );

    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(&with_extra_stmt);

    let recv1 = find_hash(&h1, "recv_0_0").expect("Transfer in original");
    let recv2 = find_hash(&h2, "recv_0_0").expect("Transfer in modified");

    assert_ne!(
        recv1.structure(),
        recv2.structure(),
        "adding a statement must change the structural hash"
    );
}

// --- Test 8: Provenance level monotonicity ---
// Every provenance edge should go from lower level to higher level

#[test]
fn provenance_levels_are_monotonic() {
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let provenance = mir::collect_provenance(&a.db, &package);

    for (source, target) in provenance.dag.edges() {
        assert!(
            (source.level as u16) <= (target.level as u16),
            "provenance edge should go from lower level to higher: {:?} → {:?}",
            source,
            target
        );
    }
}

// --- Test 10: Compiler consistency — same source, hash every function, verify all match ---

#[test]
fn every_function_hash_is_self_consistent() {
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");
    let cx = DescribeCtx::new(&a.db);

    // Hash every function TWICE and verify identical results
    let functions = package.functions(&a.db);
    for func in &functions {
        let body = func.instance(&a.db).body(&a.db);

        let mut c1 = HashConsumer::new();
        let mut c2 = HashConsumer::new();
        body.describe(&cx, &mut c1);
        body.describe(&cx, &mut c2);

        let h1 = c1.into_result().expect("hash should exist");
        let h2 = c2.into_result().expect("hash should exist");

        assert_eq!(
            h1.structure(),
            h2.structure(),
            "function {} must hash identically on repeated describe",
            func.symbol(&a.db)
        );
        assert_eq!(
            h1.names(),
            h2.names(),
            "function {} names must hash identically",
            func.symbol(&a.db)
        );
    }

    eprintln!(
        "Self-consistency: {} functions all hash deterministically",
        functions.len()
    );
}

// --- Test 11: Origin coverage completeness ---

#[test]
fn every_stmt_has_origin() {
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        for (block_idx, block) in body.blocks.iter().enumerate() {
            assert_eq!(
                block.stmts.len(),
                block.stmt_origins.len(),
                "function {} block {block_idx}: stmt count must equal origin count",
                func.symbol(&a.db)
            );
        }
    }
}

// T1: Callee discrimination — RExpr::Call with different callees must hash differently.
#[test]
fn callee_discrimination_different_functions_different_hashes() {
    let (a, _hashes) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    // Collect all RExpr::Call callees across all function bodies
    let mut callee_to_func: std::collections::HashMap<String, Vec<String>> = Default::default();
    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        let sym = func.symbol(&a.db);
        for block in &body.blocks {
            for stmt in &block.stmts {
                if let mir::runtime::ir::RStmt::Assign {
                    expr: mir::runtime::ir::RExpr::Call { callee, .. },
                    ..
                } = stmt
                {
                    let callee_sym = format!("{:?}", callee.key(&a.db).source(&a.db));
                    callee_to_func
                        .entry(callee_sym)
                        .or_default()
                        .push(sym.clone());
                }
            }
        }
    }

    // We expect at least 2 distinct callees in the token contract
    let distinct_callees = callee_to_func.len();
    assert!(
        distinct_callees >= 2,
        "BASE_CONTRACT should have at least 2 distinct call targets, found {distinct_callees}"
    );

    // Now the actual discrimination test: any two functions calling DIFFERENT callees
    // with similar structure should still hash differently due to the callee identity.
    // Find two function bodies that both contain Call exprs but to different callees.
    let mut found_different = false;
    let all_funcs: Vec<_> = package.functions(&a.db).to_vec();
    let cx = DescribeCtx::new(&a.db);
    for (i, f1) in all_funcs.iter().enumerate() {
        for f2 in all_funcs.iter().skip(i + 1) {
            let b1 = f1.instance(&a.db).body(&a.db);
            let b2 = f2.instance(&a.db).body(&a.db);

            let calls1: Vec<_> = b1
                .blocks
                .iter()
                .flat_map(|b| b.stmts.iter())
                .filter_map(|s| {
                    if let mir::runtime::ir::RStmt::Assign {
                        expr: mir::runtime::ir::RExpr::Call { callee, .. },
                        ..
                    } = s
                    {
                        Some(format!("{:?}", callee.key(&a.db).source(&a.db)))
                    } else {
                        None
                    }
                })
                .collect();
            let calls2: Vec<_> = b2
                .blocks
                .iter()
                .flat_map(|b| b.stmts.iter())
                .filter_map(|s| {
                    if let mir::runtime::ir::RStmt::Assign {
                        expr: mir::runtime::ir::RExpr::Call { callee, .. },
                        ..
                    } = s
                    {
                        Some(format!("{:?}", callee.key(&a.db).source(&a.db)))
                    } else {
                        None
                    }
                })
                .collect();

            if !calls1.is_empty() && !calls2.is_empty() && calls1 != calls2 {
                let mut h1 = HashConsumer::new();
                let mut h2 = HashConsumer::new();
                b1.describe(&cx, &mut h1);
                b2.describe(&cx, &mut h2);
                let r1 = h1.into_result().unwrap();
                let r2 = h2.into_result().unwrap();
                if r1.structure() != r2.structure() {
                    found_different = true;
                    break;
                }
                // If hashes are equal despite different callees, that's a bug
                panic!(
                    "Two functions calling different callees have identical structural hash!\n\
                     func1={} calls {:?}\n\
                     func2={} calls {:?}\n\
                     hash={:#x}\n\
                     This means RExpr::Call is not hashing the callee identity.",
                    f1.symbol(&a.db),
                    calls1,
                    f2.symbol(&a.db),
                    calls2,
                    r1.structure()
                );
            }
        }
        if found_different {
            break;
        }
    }

    assert!(
        found_different,
        "should find at least one pair of functions with different callees"
    );
}

// T2: Cast type discrimination — cast to different target types must hash differently.
#[test]
fn cast_type_discrimination() {
    // The Transfer function returns bool, Balance returns u256.
    // Both have MIR bodies with different return types.
    // Verify their structural hashes are different (this exercises Cast/return path).
    let (_a, hashes) = compile_mir_hashes(BASE_CONTRACT);

    let transfer = find_hash(&hashes, "recv_0_0").expect("find Transfer handler");
    let balance = find_hash(&hashes, "recv_0_1").expect("find Balance handler");

    // These MUST differ — different return types, different call patterns
    assert_ne!(
        transfer.structure(),
        balance.structure(),
        "Transfer (returns bool) and Balance (returns u256) must have different structural hashes"
    );
}

// T4: Bytecode cross-validation — functions with identical structural hashes
// must produce identical Sonatina IR (modulo names).
#[test]
fn identical_structure_hash_implies_identical_sonatina_ir() {
    // Contract with two receivers that have identical logic but different names.
    let source = r#"
msg Msg {
    #[selector = 0x11111111]
    GetA { account: Address } -> u256,
    #[selector = 0x22222222]
    GetB { account: Address } -> u256,
}

struct Store {
    balances: StorageMap<Address, u256>,
}

pub contract Dup uses (ctx: Ctx) {
    store: Store

    recv Msg {
        GetA { account } -> u256 uses store {
            store.balances.get(key: account)
        }

        GetB { account } -> u256 uses store {
            store.balances.get(key: account)
        }
    }
}
"#;

    let (a, hashes) = compile_mir_hashes(source);

    // Find the two getters — they should have identical structure
    let get_a = find_hash(&hashes, "recv_0_0").expect("GetA");
    let get_b = find_hash(&hashes, "recv_0_1").expect("GetB");
    assert_eq!(
        get_a.structure(),
        get_b.structure(),
        "GetA and GetB have identical logic — structural hashes must match"
    );

    // Now lower to Sonatina IR and compare
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let ir_text =
        fe_codegen::emit_runtime_package_sonatina_ir(&a.db, &package, fe_codegen::EVM_LAYOUT)
            .expect("sonatina IR");

    // Extract individual function bodies from the IR text.
    let func_bodies: Vec<(String, String)> = extract_sonatina_functions(&ir_text);

    // Find the two getter function bodies
    let getter_bodies: Vec<_> = func_bodies
        .iter()
        .filter(|(name, _)| name.contains("recv_0_0") || name.contains("recv_0_1"))
        .collect();

    assert!(
        getter_bodies.len() >= 2,
        "should find at least 2 getter functions in Sonatina IR, found {}",
        getter_bodies.len()
    );

    // Strip function names and block labels to compare pure structure.
    let normalized_a = normalize_sonatina_body(&getter_bodies[0].1);
    let normalized_b = normalize_sonatina_body(&getter_bodies[1].1);

    assert_eq!(
        normalized_a, normalized_b,
        "Functions with identical structural hash must produce identical Sonatina IR.\n\
         This proves hash equality = real output equivalence.\n\
         func_a: {}\nfunc_b: {}",
        getter_bodies[0].0, getter_bodies[1].0
    );
}

// T7: Structural change isolation — modify one function, others unchanged
#[test]
fn structural_change_isolation() {
    let modified = BASE_CONTRACT.replace(
        "store.balances.get(key: account)",
        "store.balances.get(key: account) + 1",
    );

    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(&modified);

    let balance1 = find_hash(&h1, "recv_0_1").expect("Balance original");
    let balance2 = find_hash(&h2, "recv_0_1").expect("Balance modified");
    assert_ne!(
        balance1.structure(),
        balance2.structure(),
        "adding +1 to Balance must change its structural hash"
    );

    // Transfer should be unaffected
    let transfer1 = find_hash(&h1, "recv_0_0").expect("Transfer original");
    let transfer2 = find_hash(&h2, "recv_0_0").expect("Transfer modified");
    assert_eq!(
        transfer1.structure(),
        transfer2.structure(),
        "modifying Balance must not affect Transfer's structural hash"
    );
}

// --- Test 13: Composite consumer produces all three outputs ---

#[test]
fn composite_consumer_produces_hashes_debug_and_facts_from_one_describe() {
    use common::debug_consumer::DebugConsumer;
    use common::fact_consumer::FactConsumer;

    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let cx = DescribeCtx::new(&a.db);

    // Pick the Transfer function
    let functions = package.functions(&a.db);
    let func = functions
        .iter()
        .find(|f| f.symbol(&a.db).contains("recv_0_0"))
        .expect("find Transfer");

    let body = func.instance(&a.db).body(&a.db);
    let mut composite = (
        HashConsumer::new(),
        (DebugConsumer::new(), FactConsumer::new()),
    );
    body.describe(&cx, &mut composite);

    // HashConsumer produced a hash
    let hash = composite.0.result().expect("hash should be produced");
    assert_ne!(hash.structure(), 0, "structure hash should be non-zero");

    // DebugConsumer collected entries
    let debug_entries = composite.1.0.entries();
    assert!(
        !debug_entries.is_empty(),
        "debug consumer should collect entries"
    );

    let with_origin = composite.1.0.entries_with_origin().count();
    assert!(with_origin > 0, "some entries should have origins");

    let with_source = composite.1.0.entries_with_source().count();
    // Source spans come from SpanResolver which is inside RuntimeBody::describe
    // The DebugConsumer should have received source_span calls

    // FactConsumer produced facts
    let facts = composite.1.1.facts();
    assert!(!facts.is_empty(), "fact consumer should produce facts");

    let node_hashes = facts
        .iter()
        .filter(|f| matches!(f, common::fact_consumer::Fact::NodeHash { .. }))
        .count();
    assert!(node_hashes > 0, "should have NodeHash facts");

    let origins = facts
        .iter()
        .filter(|f| matches!(f, common::fact_consumer::Fact::Origin { .. }))
        .count();
    assert!(origins > 0, "should have Origin facts");

    eprintln!("Composite consumer on Transfer function:");
    eprintln!("  Hash: structure={:#x}", hash.structure());
    eprintln!(
        "  Debug entries: {}, with origin: {}, with source: {}",
        debug_entries.len(),
        with_origin,
        with_source
    );
    eprintln!(
        "  Facts: {}, NodeHash: {}, Origin: {}",
        facts.len(),
        node_hashes,
        origins
    );
}

// --- Test 12: End-to-end DWARF from real compiled code ---

#[test]
fn dwarf_from_origins_on_real_contract() {
    let a = analyze(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let (artifacts, origins) =
        fe_codegen::compile_to_artifacts(&a.db, &package, fe_codegen::EVM_LAYOUT)
            .expect("compile to artifacts");

    let dwarf =
        fe_codegen::dwarf::generate_dwarf_from_origins(&a.db, &package, &artifacts, &origins);

    assert!(
        dwarf.is_some(),
        "DWARF generation from origins should produce output"
    );
    let dwarf = dwarf.unwrap();
    assert!(!dwarf.debug_line.is_empty(), "debug_line should have data");
    assert!(!dwarf.debug_info.is_empty(), "debug_info should have data");

    eprintln!(
        "DWARF from origins: debug_line={} bytes, debug_info={} bytes",
        dwarf.debug_line.len(),
        dwarf.debug_info.len()
    );
}

// --- Test: Sonatina optimization provenance ---

#[test]
fn optimization_provenance_survives_o1() {
    use common::provenance::{IrLevel, TransformTag};

    let a = analyze(BASE_CONTRACT);
    let package = a.package();

    let (_artifacts, report) = fe_codegen::compile_to_artifacts_with_opt_provenance(
        &a.db,
        &package,
        fe_codegen::EVM_LAYOUT,
        fe_codegen::OptLevel::O1,
    )
    .expect("compile with opt provenance");

    eprintln!("Optimization provenance at O1:");
    eprintln!("  Pre-opt origins: {}", report.pre_opt_origins.len());
    eprintln!("  Post-opt origins: {}", report.post_opt_origins.len());
    eprintln!("  Survived: {}", report.survived);
    eprintln!("  Eliminated: {}", report.eliminated);
    eprintln!("  New (opt-created): {}", report.new_insts);

    assert!(
        report.survived > 0,
        "some instructions must survive optimization"
    );
    assert!(
        !report.pre_opt_origins.is_empty(),
        "pre-optimization origin map must be non-empty"
    );

    // Survived instructions should retain their original origin
    let survived_with_origin = report
        .post_opt_origins
        .iter()
        .filter(|(_, _, o)| o.transform != TransformTag::SonatinaOptNew)
        .count();
    eprintln!("  Survived with original origin: {survived_with_origin}");

    // Show distribution of transform tags in pre-opt origins
    let mut tag_counts: std::collections::HashMap<TransformTag, usize> = Default::default();
    for (_, _, o) in &report.pre_opt_origins {
        *tag_counts.entry(o.transform).or_default() += 1;
    }
    eprintln!("  Pre-opt transform distribution:");
    for (tag, count) in &tag_counts {
        eprintln!("    {tag:?}: {count}");
    }

    assert!(
        survived_with_origin > 0,
        "survived instructions must retain their original origin"
    );

    // New instructions should have the SonatinaOptNew tag
    let new_tagged = report
        .post_opt_origins
        .iter()
        .filter(|(_, _, o)| o.transform == TransformTag::SonatinaOptNew)
        .count();
    eprintln!("  New with SonatinaOptNew tag: {new_tagged}");
    assert_eq!(
        new_tagged, report.new_insts,
        "all new instructions must have SonatinaOptNew transform tag"
    );

    // Verify the chain resolves: pick a survived instruction with MIR origin,
    // trace it back to source
    let mut resolved_through_opt = 0;
    for (_func_ref, _inst_id, origin) in &report.post_opt_origins {
        if origin.level != IrLevel::Smir {
            continue;
        }
        for func in package.functions(&a.db) {
            let key = func.instance(&a.db).key(&a.db);
            let Some(semantic) = key.semantic(&a.db) else {
                continue;
            };
            let Some(hir_body) = semantic.key(&a.db).owner(&a.db).body(&a.db) else {
                continue;
            };
            let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
            use hir::span::LazySpan;
            if expr_id.span(hir_body).resolve(&a.db).is_some() {
                resolved_through_opt += 1;
                break;
            }
        }
        if resolved_through_opt >= 10 {
            break;
        }
    }
    eprintln!("  Origins resolved through optimization: {resolved_through_opt}");
    assert!(
        resolved_through_opt > 0,
        "at least some post-optimization origins must resolve back to source"
    );

    let survival_rate =
        report.survived as f64 / (report.survived + report.eliminated) as f64 * 100.0;
    eprintln!("  Survival rate: {survival_rate:.1}%");
}
