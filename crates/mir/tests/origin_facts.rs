use common::{InputDb, facts::OriginFactIndex};
use driver::DriverDataBase;
use fe_mir::{
    RuntimePackage, build_runtime_package, legacy_runtime_package_origin_facts,
    trace::emit_mir_facts,
};
use hir::hir_def::{Func, TopLevelMod};
use trace_facts::{TraceFact, TraceValidator};
use url::Url;

fn find_func<'db>(db: &'db DriverDataBase, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn package_statement_and_terminator_count<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
) -> usize {
    package
        .functions(db)
        .iter()
        .map(|function| {
            let body = function.instance(db).body(db);
            body.blocks
                .iter()
                .map(|block| block.stmts.len() + 1)
                .sum::<usize>()
        })
        .sum()
}

fn package_runtime_local_count<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
) -> usize {
    package
        .functions(db)
        .iter()
        .map(|function| function.instance(db).body(db).locals.len())
        .sum()
}

fn package_block_count<'db>(db: &'db DriverDataBase, package: RuntimePackage<'db>) -> usize {
    package
        .functions(db)
        .iter()
        .map(|function| function.instance(db).body(db).blocks.len())
        .sum()
}

fn origin_node_kind_count(facts: &[TraceFact], kind: &str) -> usize {
    facts
        .iter()
        .filter(|fact| matches!(fact, TraceFact::OriginNode(node) if node.key.kind() == kind))
        .count()
}

#[test]
fn legacy_runtime_package_origin_facts_cover_statements_and_terminators() {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse("file:///runtime_origin_facts.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        file_url,
        Some(
            r#"
fn main() -> u256 {
    let x: u256 = 1
    x
}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);
    let _ = find_func(&db, top_mod, "main");
    let package = build_runtime_package(&db, top_mod).expect("runtime package should build");
    let facts = legacy_runtime_package_origin_facts(&db, package);
    let index = OriginFactIndex::from_facts(&facts);

    assert_eq!(
        facts.origin_node_count(),
        package_statement_and_terminator_count(&db, package)
    );
    assert_eq!(index.node_count(), facts.origin_node_count());
    assert_eq!(facts.origin_link_count(), 0);
}

#[test]
fn mir_trace_emitter_covers_statements_and_terminators() {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse("file:///runtime_trace_facts.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        file_url,
        Some(
            r#"
fn main() -> u256 {
    let x: u256 = 1
    x
}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);
    let _ = find_func(&db, top_mod, "main");
    let package = build_runtime_package(&db, top_mod).expect("runtime package should build");
    let facts = emit_mir_facts(&db, package);
    let summary = TraceValidator::validate(&facts).unwrap();

    for fact in &facts {
        if let trace_facts::TraceFact::OriginNode(node) = fact
            && node.key.kind().starts_with("runtime.")
        {
            assert!(node.key.owner_key().starts_with("runtime-instance:"));
        }
    }
    assert_eq!(
        origin_node_kind_count(&facts, "runtime.stmt")
            + origin_node_kind_count(&facts, "runtime.terminator"),
        package_statement_and_terminator_count(&db, package)
    );
    assert_eq!(
        origin_node_kind_count(&facts, "runtime.local"),
        package_runtime_local_count(&db, package)
    );
    assert_eq!(
        origin_node_kind_count(&facts, "runtime.block"),
        package_block_count(&db, package)
    );
    assert_eq!(
        origin_node_kind_count(&facts, "runtime.function"),
        package.functions(&db).len()
    );
    assert_eq!(summary.edge_count, 0);
    assert!(
        facts
            .iter()
            .any(|fact| matches!(fact, TraceFact::DisplayName(display) if display.name == "x")),
        "MIR trace should preserve source-local display names for semantic locals"
    );
    assert!(
        facts
            .iter()
            .any(|fact| matches!(fact, TraceFact::CompilerEvent(event) if event.phase == trace_facts::CompilerPhase::Mir)),
        "MIR trace should include causal MIR compiler events"
    );
    assert!(
        facts
            .iter()
            .any(|fact| matches!(fact, TraceFact::Variable(variable) if variable.name == "x")),
        "MIR trace should include source-local variable facts for semantic locals"
    );
    assert!(
        facts
            .iter()
            .any(|fact| matches!(fact, TraceFact::Storage(_))),
        "MIR trace should include storage facts for runtime locals"
    );
}

#[test]
fn mir_trace_emits_cfg_and_natural_loop_facts() {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse("file:///runtime_trace_loop_facts.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        file_url,
        Some(
            r#"
fn main() -> u32 {
    let mut i: u32 = 0
    while i < 4 {
        i = i + 1
    }
    i
}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);
    let _ = find_func(&db, top_mod, "main");
    let package = build_runtime_package(&db, top_mod).expect("runtime package should build");
    let facts = emit_mir_facts(&db, package);

    TraceValidator::validate(&facts).unwrap();
    assert!(
        facts.iter().any(|fact| matches!(fact, TraceFact::Block(_))),
        "MIR trace should emit target-neutral block facts"
    );
    assert!(
        facts
            .iter()
            .any(|fact| matches!(fact, TraceFact::CfgEdge(_))),
        "MIR trace should emit CFG edges from MIR terminators"
    );
    assert!(
        facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Loop(loop_fact)
                if loop_fact.phase == trace_facts::CompilerPhase::Mir
        )),
        "MIR trace should derive natural loops from the MIR CFG"
    );
    assert!(
        facts.iter().any(|fact| matches!(
            fact,
            TraceFact::LoopBlock(block) if block.role == trace_facts::LoopBlockRole::Header
        )),
        "MIR loop facts should identify a header block"
    );
}
