use fe_hir::test_db::HirAnalysisTestDb;

/// Wraps `innermost` in `wrap` `depth` times.
fn nest(depth: usize, innermost: &str, wrap: impl Fn(&str) -> String) -> String {
    (0..depth).fold(innermost.to_string(), |ty, _| wrap(&ty))
}

fn diagnostic_messages(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("nested_type_paths.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    db.run_on_top_mod(top_mod)
        .iter()
        .map(|diag| diag.to_complete(&db).message)
        .collect()
}

const DEPTH: usize = 64;
const WRAPPER: &str = "pub struct W<T> { value: T }\n";
const PROJECTION: &str = "pub trait M { type P }\nimpl<T> M for W<T> { type P = T }\n";

/// A path in a type position was resolved again for each namespace it was
/// tried in: a generic argument as a constant and then as a type, and a type
/// that was not found as a value. Each resolution lowered every generic
/// argument nested in the path again, so the work doubled per nesting level
/// and depth 20 took seconds. This depth is out of reach that way; resolving
/// each path once, it is immediate.
#[test]
fn deeply_nested_type_paths_are_resolved_once() {
    for source in [
        format!(
            "{WRAPPER}pub struct S<T> {{ value: *{} }}\n",
            nest(DEPTH, "T", |ty| format!("W<{ty}>"))
        ),
        // A function of the same name is a value that is not a constant, so
        // each argument is the type.
        format!(
            "{WRAPPER}pub fn W() {{}}\npub struct S {{ value: {} }}\n",
            nest(DEPTH, "u8", |ty| format!("W<{ty}>"))
        ),
        // A constant argument beside each nested type.
        format!(
            "pub struct C<T, const N: usize> {{ value: T, values: [u8; N] }}\n\
             const N: usize = 1\npub struct S {{ value: {} }}\n",
            nest(DEPTH, "u8", |ty| format!("C<{ty}, N>"))
        ),
        // Arguments that are qualified projections, whose parent segment
        // lowers the next level.
        format!(
            "{WRAPPER}{PROJECTION}pub struct S<T: M> {{ value: {} }}\n",
            nest(DEPTH, "T", |ty| format!("W<<{ty} as M>::P>"))
        ),
    ] {
        assert_eq!(
            diagnostic_messages(&source),
            Vec::<String>::new(),
            "{source}"
        );
    }
}

/// Malformed nesting is bounded as well. An argument that failed was resolved
/// again as a type, a projection that was not found again as a value, and a
/// qualified type's own type again to recover its error, so each of these grew
/// two- to threefold per level.
#[test]
fn deeply_nested_malformed_type_paths_are_resolved_once() {
    for (source, message) in [
        (
            format!(
                "{WRAPPER}pub struct S {{ value: {} }}\n",
                nest(DEPTH, "u8", |ty| format!("W<{ty}, u8>"))
            ),
            "incorrect number of generic arguments for `W`; expected 1, given 2",
        ),
        (
            format!(
                "{WRAPPER}{PROJECTION}pub struct S<T: M> {{ value: {} }}\n",
                nest(DEPTH, "T", |ty| format!("W<<{ty} as M>::Q>"))
            ),
            "`Q` is not found",
        ),
        (
            format!(
                "pub trait M {{ type P: M }}\npub struct S<T: M> {{ value: {} }}\n",
                nest(DEPTH, "T", |ty| format!("<{ty} as M>::Q"))
            ),
            "`Q` is not found",
        ),
    ] {
        assert_eq!(diagnostic_messages(&source), [message], "{source}");
    }
}

/// A parameter's callable layout walk checks each nested level against every
/// enclosing one. The structural embedding test it uses was not memoized and,
/// on types that do not embed, explored exponentially many pairs of subterms.
#[test]
fn deeply_nested_parameter_types_are_checked_in_polynomial_time() {
    let source = format!(
        "{WRAPPER}fn f(x: {}) {{}}\n",
        nest(DEPTH, "u8", |ty| format!("W<{ty}>"))
    );
    assert_eq!(diagnostic_messages(&source), Vec::<String>::new());
}
