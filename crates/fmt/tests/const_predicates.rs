//! Regression guard: `fe fmt` must preserve where-clause const predicates.
//!
//! Kept as an inline assertion rather than a `tests/fixtures/*.fe` file: the
//! tree-sitter grammar does not yet model const-expression predicates, so such a
//! fixture would trip `tree_sitter_parse_strict` (grammar support is a separate
//! follow-up). The hand-written parser and the formatter handle them today, and
//! the formatter previously walked only trait-bound predicates and silently
//! dropped these.

use fe_fmt::{Config, format_str};

#[test]
fn fmt_preserves_where_const_predicates() {
    let src = "\
trait Sized {
    const SIZE: u256
}

fn check_big<T>() where T: Sized, T::SIZE >= 50 {
}

fn check_mixed<T>() where T: Sized, T::SIZE < 500, { T::SIZE > 0 }
{
}

struct Wrap<T> where T: Sized, T::SIZE <= 32 {
    inner: T
}
";

    let out = format_str(src, &Config::default()).expect("formats without error");

    for needle in [
        "T::SIZE >= 50",   // comparison predicate on a fn
        "T::SIZE < 500",   // mixed with a trait bound
        "{ T::SIZE > 0 }", // brace predicate
        "T::SIZE <= 32",   // predicate on a struct
    ] {
        assert!(
            out.contains(needle),
            "`fe fmt` dropped const predicate `{needle}`:\n{out}"
        );
    }
}
