use std::fmt::Write;

use fe_parser::{RecoveryMode, SyntaxKind, parse_source_file, syntax_node::SyntaxNode};

fn nest(depth: usize, innermost: &str) -> String {
    let mut ty = innermost.to_string();
    for _ in 0..depth {
        ty = format!("Wrap<<{ty} as Model>::Point>");
    }
    ty
}

/// Each level of `Wrap<<T as Model>::Point>` probed the positions inside it to
/// tell generic arguments from a shift, and every enclosing level probed them
/// all again, so parsing cost grew by roughly 5x per level: depth 12 took about
/// 96 seconds. This depth is unreachable that way and immediate with probe
/// outcomes reused per position, so the test bounds the speculative work as much
/// as it checks the tree.
#[test]
fn deeply_nested_qualified_generic_args_parse_without_repeating_probes() {
    const DEPTH: usize = 32;

    let source = format!("type A = {}\n", nest(DEPTH, "T"));

    let (green, errors) = parse_source_file(&source, RecoveryMode::new(false));
    assert!(errors.is_empty(), "{errors:#?}");

    let cst = SyntaxNode::new_root(green);
    assert_eq!(cst.text().to_string(), source);
    assert_eq!(
        cst.descendants()
            .filter(|node| node.kind() == SyntaxKind::QualifiedType)
            .count(),
        DEPTH
    );
}

/// Malformed nesting must be bounded too, which means reusing the outcomes of
/// probes that failed. Excluding those instead, as an earlier revision did, left
/// each enclosing level re-parsing everything below it: depth 10 cost about
/// 13 ms and grew over twofold per level, putting this depth out of reach.
/// Reuse is sound because a probe never recovers, so its outcome cannot depend
/// on the caller.
#[test]
fn deeply_nested_malformed_generic_args_do_not_repeat_probes() {
    const DEPTH: usize = 32;

    // A qualified type missing its `as`, innermost, and an argument list that
    // never closes: both force recovery inside the probes at every level.
    for innermost in ["Wrap<<T Model>::Point>", "Wrap<<T as Model>::Point"] {
        for recover in [false, true] {
            let source = format!("type A = {}\n", nest(DEPTH, innermost));
            let (green, errors) = parse_source_file(&source, RecoveryMode::new(recover));
            assert!(!errors.is_empty(), "{innermost:?} recover={recover}");
            // Recovery keeps consuming, so the tree still spans the input; a
            // `NoRecover` parse stops at the error and is not expected to.
            if recover {
                assert_eq!(
                    SyntaxNode::new_root(green).text().to_string(),
                    source,
                    "{innermost:?} recover={recover}",
                );
            }
        }
    }
}

/// A path followed by `<<` probes whether the `<<` opens generic arguments, as
/// in `Wrap<<T as Model>::Point>`. In `w << 4` the probe fails, and recovery
/// inside it used to consume the rest of the file looking for a token to stop
/// at, so each shift cost a scan to the end: 4,000 shifts took 0.8 s. A probe
/// now fails at its first error instead.
#[test]
fn failed_shift_probes_stop_at_their_first_error() {
    const SHIFTS: usize = 50_000;

    let mut source = "fn f(w: u256) -> u256 {\n".to_string();
    for idx in 0..SHIFTS {
        writeln!(source, "    let n{idx} = w << 4").unwrap();
    }
    source.push_str("    w\n}\n");

    let (green, errors) = parse_source_file(&source, RecoveryMode::new(true));
    assert!(errors.is_empty(), "{errors:#?}");
    assert_eq!(
        SyntaxNode::new_root(green)
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::BinExpr)
            .count(),
        SHIFTS
    );
}
