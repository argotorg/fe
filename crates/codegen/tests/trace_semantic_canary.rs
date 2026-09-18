use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::{OptLevel, trace};
use trace_facts::{TraceBundle, TraceFact, TraceMetadata, TraceSnapshot};
use url::Url;

const CONTRIBUTOR: &str = "0x123456789abcde";
const ADJACENT: &str = "0x223456789abcde";

fn snapshot(source: &str, file_url: &Url, opt_level: OptLevel) -> TraceSnapshot {
    let mut db = DriverDataBase::default();
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(source.to_string()));
    let file = db.workspace().get(&db, file_url).unwrap();
    let top_mod = db.top_mod(file);
    let facts = trace::emit_observable_module_trace_facts(
        &db,
        trace::ObservableModuleTraceInput {
            top_mod,
            input_owner_key: file_url.as_str(),
            source_uri: file_url.as_str(),
            source_display_name: "mask_elimination.fe",
            source_text: source,
            opt_level,
            contract: None,
        },
    )
    .expect("semantic canary should compile");
    let metadata = TraceMetadata::compiler_emitted(
        "test",
        "evm/sonatina",
        vec!["trace-semantic-canary".to_string()],
        file_url.as_str(),
        vec![format!("optimize={opt_level}")],
    );
    TraceSnapshot::new(TraceBundle::new(metadata, facts)).unwrap()
}

fn byte_range(source: &str, needle: &str) -> (u32, u32) {
    let start = source.find(needle).unwrap() as u32;
    (start, start + needle.len() as u32)
}

fn immediate_push_origins(
    snapshot: &TraceSnapshot,
    immediate: &str,
) -> Vec<Vec<common::origin::OriginExportKey>> {
    let index = trace_facts::trace_index::TraceIndex::new(snapshot);
    snapshot
        .facts()
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::Opcode(opcode)
                if opcode.opcode == "PUSH7" && opcode.immediate.as_deref() == Some(immediate) =>
            {
                Some(index.source_candidates_for_instruction(
                    &opcode.pc,
                    trace_facts::trace_index::TraceReachabilityPolicy::ExactOnly,
                ))
            }
            _ => None,
        })
        .collect()
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace_semantic_canary",
    glob: "*.fe"
)]
fn o2_mask_elimination_preserves_contributing_source_and_rejects_adjacent_source(
    fixture: Fixture<&str>,
) {
    let file_url = Url::from_file_path(fixture.path()).expect("fixture path should be absolute");
    // Exercise both checkout line endings on every host. The exact variant
    // bytes are supplied to the compiler and used by the source-span oracle.
    let lf = test_utils::normalize::normalize_newlines(fixture.content());
    let crlf = lf.replace('\n', "\r\n");
    for source in [lf.as_ref(), crlf.as_str()] {
        assert_mask_attribution(source, &file_url);
    }
}

fn assert_mask_attribution(source: &str, file_url: &Url) {
    let first = byte_range(source, "let first = n & 0x123456789abcde");
    let second = byte_range(source, "let second = n & 0x123456789abcde");
    let adjacent = byte_range(source, "let unrelated = m & 0x223456789abcde");

    let o0 = snapshot(source, file_url, OptLevel::O0);
    let o2 = snapshot(source, file_url, OptLevel::O2);
    let o0_contributors = immediate_push_origins(&o0, CONTRIBUTOR);
    let o2_contributors = immediate_push_origins(&o2, CONTRIBUTOR);

    let count_and = |snapshot: &TraceSnapshot| {
        snapshot
            .facts()
            .iter()
            .filter(|fact| matches!(fact, TraceFact::Opcode(opcode) if opcode.opcode == "AND"))
            .count()
    };
    assert!(
        count_and(&o0) > count_and(&o2),
        "O2 must actually eliminate mask operations: O0={}, O2={}",
        count_and(&o0),
        count_and(&o2)
    );
    assert!(
        !o0_contributors.is_empty(),
        "O0 must emit the contributor marker"
    );
    assert!(
        !immediate_push_origins(&o0, ADJACENT).is_empty(),
        "O0 must emit the independently live adjacent marker"
    );
    assert_eq!(
        o2_contributors.len(),
        1,
        "O2 must retain one contributor constant after eliminating mask operations"
    );
    assert!(
        !immediate_push_origins(&o2, ADJACENT).is_empty(),
        "O2 must retain the independently live adjacent marker"
    );

    let spans = o2
        .facts()
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::SourceSpan(span) => Some((&span.origin, span)),
            _ => None,
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let source_file = o2
        .facts()
        .iter()
        .find_map(|fact| match fact {
            TraceFact::SourceFile(file) if file.uri == file_url.as_str() => Some(&file.file_key),
            _ => None,
        })
        .expect("canary source file must be recorded");
    // This fixture is a positive coverage floor, not a general promise that
    // every optimized instruction has an exact origin. Multiple contributing
    // origins are legitimate; none may name the unrelated adjacent statement.
    assert!(
        !o2_contributors[0].is_empty(),
        "this canary requires a surviving attribution to exercise its semantic check"
    );
    for origin in &o2_contributors[0] {
        let span = spans
            .get(origin)
            .expect("the exact candidate must have a source span");
        assert_eq!(
            &span.file, source_file,
            "candidate must belong to the canary source"
        );
        let overlaps = |range: (u32, u32)| span.start_byte < range.1 && range.0 < span.end_byte;
        assert!(
            overlaps(first) || overlaps(second),
            "the surviving mask must map to a genuine contributor: {span:?}"
        );
        assert!(
            !overlaps(adjacent),
            "the surviving mask must not inherit the unrelated adjacent statement: {span:?}"
        );
    }
}
