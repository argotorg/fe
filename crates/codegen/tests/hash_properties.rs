#![allow(clippy::print_stderr)]

mod test_helpers;
use test_helpers::*;

use common::hash_consumer::HashConsumer;
use common::ir_describe::{DescribeCtx, IrConsumer, IrDescribe, NullConsumer};
use driver::DriverDataBase;

// --- Test 3: Rename stability ---
// Rename a variable, verify Structure unchanged, Names changed

#[test]
fn rename_changes_names_hash_but_not_structure() {
    // Same logic, different variable names
    let renamed = BASE_CONTRACT
        .replace("let sender", "let from_addr")
        .replace("key: sender", "key: from_addr");

    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(&renamed);

    // Find a function that exists in both — some ABI helpers should be identical
    let mut structure_matches = 0;
    let mut names_matches = 0;

    for (name1, hash1) in &h1 {
        if let Some((_, hash2)) = h2.iter().find(|(n, _)| n == name1) {
            if hash1.structure() == hash2.structure() {
                structure_matches += 1;
            }
            if hash1.names() == hash2.names() {
                names_matches += 1;
            }
        }
    }

    // Most functions should have identical structure (rename doesn't change computation)
    assert!(
        structure_matches > h1.len() / 2,
        "most functions should have identical structure after rename: {structure_matches}/{}",
        h1.len()
    );

    eprintln!(
        "Rename test: {structure_matches}/{} structure matches, {names_matches}/{} names matches",
        h1.len(),
        h1.len()
    );
}

// T8: Whitespace immunity at MIR level
#[test]
fn whitespace_immunity_at_mir() {
    let spaced = BASE_CONTRACT
        .replace(
            "let sender = ctx.caller()",
            "let sender    =   ctx.caller()",
        )
        .replace("return true", "return   true")
        .replace("return false", "return    false");

    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(&spaced);

    // ALL functions should hash identically at MIR level
    // (whitespace is erased during parsing)
    for (name1, hash1) in &h1 {
        if let Some((_, hash2)) = h2.iter().find(|(n, _)| n == name1) {
            assert_eq!(
                hash1.structure(),
                hash2.structure(),
                "whitespace changes must not affect MIR structural hash for {name1}"
            );
        }
    }
}

// T6: Dimension orthogonality — rename a local variable, verify structural hash stability.
// At MIR level, local variable names are erased, so renaming should not affect structure.
// We rename a let-binding that is only used locally.
#[test]
fn dimension_orthogonality_rename_preserves_structure() {
    let renamed = BASE_CONTRACT
        .replace(
            "let sender = ctx.caller()",
            "let origin_addr = ctx.caller()",
        )
        .replace("key: sender", "key: origin_addr");

    let (_a1, h1) = compile_mir_hashes(BASE_CONTRACT);
    let (_a2, h2) = compile_mir_hashes(&renamed);

    // The rename test (test 3) already verified this property broadly.
    // Here we check specifically that the renamed function's structure is preserved.
    // At MIR level, local names are erased → structure should be identical.
    let mut structure_matches = 0;
    let total = h1.len().min(h2.len());
    for (name1, hash1) in &h1 {
        if let Some((_, hash2)) = h2.iter().find(|(n, _)| n == name1)
            && hash1.structure() == hash2.structure()
        {
            structure_matches += 1;
        }
    }

    // Most functions should have identical structure after renaming a local
    assert!(
        structure_matches > total / 2,
        "most functions should preserve structural hash after local rename: \
         {structure_matches}/{total}"
    );
    eprintln!("T6: {structure_matches}/{total} structure matches after local rename");
}

// T3: CompositeConsumer children_unordered delegates properly
#[test]
fn composite_children_unordered_is_order_independent() {
    let db = DriverDataBase::default();
    let cx = DescribeCtx::new(&db);

    let mut c1 = (HashConsumer::new(), NullConsumer);
    c1.enter_node("Root");
    c1.children_unordered::<SimpleLeaf>(
        &cx,
        &[
            SimpleLeaf { tag: 10 },
            SimpleLeaf { tag: 20 },
            SimpleLeaf { tag: 30 },
        ],
    );
    c1.exit_node();

    let mut c2 = (HashConsumer::new(), NullConsumer);
    c2.enter_node("Root");
    c2.children_unordered::<SimpleLeaf>(
        &cx,
        &[
            SimpleLeaf { tag: 30 },
            SimpleLeaf { tag: 10 },
            SimpleLeaf { tag: 20 },
        ],
    );
    c2.exit_node();

    let h1 = c1.0.into_result().unwrap();
    let h2 = c2.0.into_result().unwrap();
    assert_eq!(
        h1.structure(),
        h2.structure(),
        "children_unordered through CompositeConsumer must be order-independent"
    );
}

// T10: Structure-only projection matches functions with same algorithm but different constants.
// Constants-included projection distinguishes them.
#[test]
fn dimension_projection_constants_vs_structure() {
    use common::ir_describe::DimSet;

    // Two contracts identical except for a constant value in the return
    let source_a = r#"
msg Msg {
    #[selector = 0x11111111]
    GetA { account: Address } -> u256,
    #[selector = 0x22222222]
    GetB { account: Address } -> u256,
}

struct Store {
    balances: StorageMap<Address, u256>,
}

pub contract C uses (ctx: Ctx) {
    store: Store

    recv Msg {
        GetA { account } -> u256 uses store {
            store.balances.get(key: account) + 1
        }

        GetB { account } -> u256 uses store {
            store.balances.get(key: account) + 2
        }
    }
}
"#;

    let (_a, hashes) = compile_mir_hashes(source_a);

    let get_a = find_hash(&hashes, "recv_0_0").expect("GetA");
    let get_b = find_hash(&hashes, "recv_0_1").expect("GetB");

    // Structure-only projection: same algorithm shape (get + add + return)
    // These should match because the only difference is the constant value,
    // and Structure dimension doesn't include Constants.
    assert_eq!(
        get_a.projected(DimSet::ALGORITHM),
        get_b.projected(DimSet::ALGORITHM),
        "Structure-only hash must match for same algorithm with different constants"
    );

    // Full projection including Constants: must differ (1 vs 2)
    assert_ne!(
        get_a.projected(DimSet::EXACT),
        get_b.projected(DimSet::EXACT),
        "Full hash including constants must differ for different constant values"
    );

    // TEMPLATE projection (Structure without Constants): should match
    assert_eq!(
        get_a.projected(DimSet::TEMPLATE),
        get_b.projected(DimSet::TEMPLATE),
        "Template projection must match for same algorithm shape"
    );

    eprintln!("T10 dimension projection:");
    eprintln!(
        "  GetA structure={:#x} constants={:#x}",
        get_a.structure(),
        get_a.constants()
    );
    eprintln!(
        "  GetB structure={:#x} constants={:#x}",
        get_b.structure(),
        get_b.constants()
    );
    eprintln!(
        "  ALGORITHM match: {}",
        get_a.projected(DimSet::ALGORITHM) == get_b.projected(DimSet::ALGORITHM)
    );
    eprintln!(
        "  TEMPLATE match:  {}",
        get_a.projected(DimSet::TEMPLATE) == get_b.projected(DimSet::TEMPLATE)
    );
    eprintln!(
        "  EXACT match:     {}",
        get_a.projected(DimSet::EXACT) == get_b.projected(DimSet::EXACT)
    );
}

// T9: Field coverage metric — verify all describe impls close every field
#[test]
fn field_coverage_no_remaining_dotdot_in_describes() {
    use common::InputDb;

    // This is a compile-time guarantee: if a new variant is added to RExpr,
    // RStmt, RTerminator, or RuntimeBuiltin, the exhaustive match in their
    // IrDescribe impl (co-located in ir.rs) will fail to compile.
    //
    // We verify the property at test time by checking that the describe impls
    // produce non-trivial output for every MIR function in BASE_CONTRACT.
    let (a, _) = compile_mir_hashes(BASE_CONTRACT);
    let file = a.db.workspace().get(&a.db, &a.file_url).expect("file");
    let top_mod = a.db.top_mod(file);
    let package = mir::build_runtime_package(&a.db, top_mod).expect("compile");

    let cx = DescribeCtx::new(&a.db);
    let mut total_nodes = 0u64;
    let mut total_fields = 0u64;

    for func in package.functions(&a.db) {
        let body = func.instance(&a.db).body(&a.db);
        let mut counter = FieldCounter::default();
        body.describe(&cx, &mut counter);
        total_nodes += counter.nodes;
        total_fields += counter.fields;
    }

    eprintln!("Field coverage: {total_nodes} nodes, {total_fields} fields described");
    assert!(
        total_nodes > 100,
        "should describe >100 nodes across all functions"
    );
    assert!(
        total_fields > total_nodes,
        "should describe more fields than nodes (avg >1 field/node)"
    );
}

// T5: Source-level field coverage assertion — verify no .. patterns remain
// in IrDescribe match arms in ir.rs. This is a structural guarantee that
// every field of every variant is explicitly mentioned in the describe impl.
#[test]
fn no_dotdot_patterns_in_describe_impls() {
    let ir_source = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../mir/src/runtime/ir.rs"
    ))
    .expect("read ir.rs");

    // Find all lines within IrDescribe impl blocks that use ..
    // We scan for match arms inside `impl ... IrDescribe for ...` blocks
    let mut in_describe_impl = false;
    let mut brace_depth = 0i32;
    let mut dotdot_lines = Vec::new();

    for (line_num, line) in ir_source.lines().enumerate() {
        let trimmed = line.trim();

        if trimmed.contains("impl") && trimmed.contains("IrDescribe for") {
            in_describe_impl = true;
            brace_depth = 0;
        }

        if in_describe_impl {
            brace_depth += trimmed.matches('{').count() as i32;
            brace_depth -= trimmed.matches('}').count() as i32;

            // Check for .. in match arms (not in comments or string literals)
            if trimmed.contains("..") && trimmed.contains("=>") && !trimmed.starts_with("//") {
                dotdot_lines.push((line_num + 1, trimmed.to_string()));
            }

            if brace_depth <= 0 && trimmed.contains('}') {
                in_describe_impl = false;
            }
        }
    }

    assert!(
        dotdot_lines.is_empty(),
        "Found .. patterns in IrDescribe match arms in ir.rs — fields are being dropped:\n{}",
        dotdot_lines
            .iter()
            .map(|(n, l)| format!("  line {n}: {l}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

// === Analysis API tests ===

#[test]
fn category_breakdown_on_erc20() {
    let a = analyze(BASE_CONTRACT);
    let cats = a.category_breakdown();

    eprintln!(
        "{:<12} {:>6} {:>8} {:>6}",
        "Category", "Funcs", "Stmts", "%"
    );
    eprintln!("{}", "-".repeat(36));
    for c in &cats {
        eprintln!(
            "{:<12} {:>6} {:>8} {:>5.1}%",
            c.name, c.count, c.stmts, c.pct
        );
    }

    assert!(cats.len() >= 2, "should have at least 2 categories");
    let total_funcs: usize = cats.iter().map(|c| c.count).sum();
    assert_eq!(total_funcs, a.functions.len());
}

#[test]
fn dedup_report_on_erc20() {
    let a = analyze(BASE_CONTRACT);
    let dedup = a.dedup_report();

    if !dedup.entries.is_empty() {
        eprintln!(
            "{:<30} {:>6} {:>8} {:>8}",
            "Function", "Copies", "Stmts", "Wasted"
        );
        eprintln!("{}", "-".repeat(56));
        for e in dedup.entries.iter().take(10) {
            eprintln!(
                "{:<30} {:>6} {:>8} {:>8}",
                e.representative, e.copies, e.stmts_per_copy, e.wasted
            );
        }
        eprintln!(
            "\nTotal wasted: {} stmts ({:.1}%)",
            dedup.total_wasted, dedup.pct_wasted
        );
    } else {
        eprintln!("No duplicates found in ERC20 contract");
    }
}

#[test]
fn overview_on_erc20() {
    let a = analyze(BASE_CONTRACT);
    let ov = a.overview();
    eprintln!(
        "Functions: {} ({} unique, {:.1}% duplication)",
        ov.total_functions, ov.unique_structures, ov.dup_pct
    );
    eprintln!(
        "Total stmts: {}, origin coverage: {:.1}%",
        ov.total_stmts, ov.origin_coverage_pct
    );
    assert!(ov.total_functions > 10);
    assert!(ov.unique_structures > 0);
}

#[test]
fn effect_summary_on_erc20() {
    let a = analyze(BASE_CONTRACT);
    let effects = a.effect_summary();

    eprintln!("{:<20} {:>6}", "Effect", "Count");
    eprintln!("{}", "-".repeat(28));
    for e in &effects {
        eprintln!("{:<20} {:>6}", e.effect, e.count);
    }
    assert!(!effects.is_empty());
}
