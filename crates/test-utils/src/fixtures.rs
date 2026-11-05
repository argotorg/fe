//! Fixture tests that share a database.
//!
//! Each test process sets up its own database, and with it the core and std
//! libraries, so a suite with one test per fixture repeats that setup for
//! every fixture. [`fixture_shards!`](crate::fixture_shards) instead splits a
//! suite into a few tests that each check their share of the fixtures against
//! one database and report every failure at the end. Set `FE_FIXTURE` to part
//! of a fixture's path to check only the fixtures whose paths contain it.

use std::{
    any::Any,
    panic::{self, AssertUnwindSafe},
    path::Path,
};

/// A fixture file and its contents.
pub struct Fixture {
    path: String,
    content: String,
}

impl Fixture {
    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn content(&self) -> &str {
        &self.content
    }
}

/// Checks every `shards`th fixture in `dir` matching `glob`, starting at
/// `shard`, with `check`, which gets one database from `new_db` for all of
/// them. A fixture whose check panics fails, and the fixtures after it get a
/// new database, since the panic may have left the old one mid-query.
pub fn run_shard<DB>(
    dir: &str,
    glob: &str,
    shard: usize,
    shards: usize,
    mut new_db: impl FnMut() -> DB,
    mut check: impl FnMut(&mut DB, &Fixture),
) {
    let pattern = Path::new(dir).join(glob);
    let pattern = pattern.to_str().unwrap();
    let mut paths: Vec<_> = glob::glob(pattern)
        .unwrap()
        .map(|path| path.unwrap().to_str().unwrap().to_string())
        .collect();
    assert!(!paths.is_empty(), "no fixtures match {pattern}");
    paths.sort();

    let filter = std::env::var("FE_FIXTURE").ok();
    let mut db = None;
    let mut checked = 0;
    let mut failures = Vec::new();
    for path in paths.into_iter().skip(shard).step_by(shards) {
        if filter.as_ref().is_some_and(|filter| !path.contains(filter)) {
            continue;
        }
        let fixture = Fixture {
            content: std::fs::read_to_string(&path).unwrap(),
            path,
        };
        checked += 1;
        let db_ref = db.get_or_insert_with(&mut new_db);
        if let Err(payload) = panic::catch_unwind(AssertUnwindSafe(|| check(db_ref, &fixture))) {
            failures.push(format!("{}: {}", fixture.path, panic_message(&*payload)));
            db = None;
        }
    }
    assert!(
        failures.is_empty(),
        "{} of {checked} fixtures failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

fn panic_message(payload: &(dyn Any + Send)) -> &str {
    payload
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
        .unwrap_or("panicked")
}

/// Defines a module `$name` with one test per shard of the fixtures in `$dir`
/// matching `$glob`. Each test checks its fixtures with `$check`, a
/// `fn(&mut DB, &Fixture)`, on one database from `$new_db`. See
/// [`fixtures`](crate::fixtures).
#[macro_export]
macro_rules! fixture_shards {
    ($name:ident, $dir:expr, $glob:expr, $new_db:expr, $check:expr $(,)?) => {
        mod $name {
            use super::*;

            fn run(shard: usize) {
                $crate::fixtures::run_shard($dir, $glob, shard, 4, $new_db, $check);
            }

            #[test]
            fn shard_0() {
                run(0);
            }

            #[test]
            fn shard_1() {
                run(1);
            }

            #[test]
            fn shard_2() {
                run(2);
            }

            #[test]
            fn shard_3() {
                run(3);
            }
        }
    };
}
