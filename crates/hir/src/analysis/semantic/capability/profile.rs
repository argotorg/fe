//! Optional accounting for retained graphs, construction churn, and solver progress.
//! `FE_BORROWCK_PROFILE=1` enables diagnostics in a `borrowck-profile` build.
//! `FE_BORROWCK_SUBGRAPHS=1` additionally enables expensive subgraph census.
#![allow(clippy::print_stderr)] // Opt-in profiling reports are intentionally written to stderr.

use super::{
    decision::{GraphFingerprint, LiveGraphs, live_graphs},
    guard,
};
#[cfg(test)]
use std::cell::Cell;
use std::{
    cell::RefCell,
    ffi::OsStr,
    marker::PhantomData,
    sync::{Arc, LazyLock, Mutex},
    time::{Duration, Instant},
};

#[derive(Clone, Copy)]
struct Config {
    enabled: bool,
    subgraphs: bool,
}

impl Config {
    fn from_vars(profile: Option<&OsStr>, subgraphs: Option<&OsStr>) -> Self {
        let enabled = profile == Some(OsStr::new("1"));
        Self {
            enabled,
            subgraphs: enabled && subgraphs == Some(OsStr::new("1")),
        }
    }
}

static CONFIG: LazyLock<Config> = LazyLock::new(|| {
    Config::from_vars(
        std::env::var_os("FE_BORROWCK_PROFILE").as_deref(),
        std::env::var_os("FE_BORROWCK_SUBGRAPHS").as_deref(),
    )
});

thread_local! {
    static PROFILE: RefCell<Option<Profile>> = const { RefCell::new(None) };
    #[cfg(test)]
    static TEST_CONFIG: Cell<Option<Config>> = const { Cell::new(None) };
}

pub(super) fn enabled() -> bool {
    #[cfg(test)]
    if let Some(config) = TEST_CONFIG.get() {
        return config.enabled;
    }
    CONFIG.enabled
}

fn with_profile<R>(f: impl FnOnce(&mut Profile) -> R) -> Option<R> {
    if !enabled() {
        return None;
    }
    Some(PROFILE.with_borrow_mut(|p| f(p.get_or_insert_with(|| Profile::new(*CONFIG)))))
}

struct Scope {
    label: String,
    started: Instant,
    created: usize,
    sweep: usize,
    block: usize,
    statement: usize,
    phase: &'static str,
}

struct Profile {
    started: Instant,
    reported: Instant,
    live: Arc<Mutex<LiveGraphs>>,
    created: [usize; 2],
    completed: usize,
    peak_graph_and_scratch_capacity: usize,
    next_overlap_bytes: usize,
    scopes: Vec<Scope>,
    operation: &'static str,
    operation_counts: [usize; 4],
    input_nodes: [usize; 4],
}

impl Profile {
    fn new(config: Config) -> Self {
        eprintln!("GRAPH_PROFILE event=start");
        Self {
            started: Instant::now(),
            reported: Instant::now(),
            live: live_graphs(),
            created: [0; 2],
            completed: 0,
            peak_graph_and_scratch_capacity: 0,
            next_overlap_bytes: if config.subgraphs {
                128 * 1024 * 1024
            } else {
                usize::MAX
            },
            scopes: Vec::new(),
            operation: "none",
            operation_counts: [0; 4],
            input_nodes: [0; 4],
        }
    }

    fn report(&mut self, event: &str) {
        let (bytes, count, largest, duplicate_bytes, unique_bytes) = {
            let live = self.live.lock().expect("graph accounting");
            let large = live
                .copies
                .iter()
                .filter(|((_, nodes, _, _), _)| *nodes >= 1024);
            (
                live.bytes,
                large.clone().count(),
                large
                    .clone()
                    .map(|((_, nodes, _, _), _)| *nodes)
                    .max()
                    .unwrap_or(0),
                large
                    .clone()
                    .map(|((_, nodes, size, _), copies)| nodes * size * (copies - 1))
                    .sum::<usize>(),
                large
                    .map(|((_, nodes, size, _), _)| nodes * size)
                    .sum::<usize>(),
            )
        };
        let scope = self.scopes.last();
        eprintln!(
            "GRAPH_PROFILE event={event} seconds={:.3} scope={:?} phase={:?} sweep={} block={} statement={} operation={} choice_live_bytes={} bit_live_bytes={} choice_created_bytes={} bit_created_bytes={} completed={} large_unique={count} largest_nodes={largest} duplicate_bytes={duplicate_bytes} unique_bytes={unique_bytes} operation_counts={:?} input_nodes={:?} peak_graph_and_scratch_capacity={}",
            self.started.elapsed().as_secs_f64(),
            scope.map(|s| &s.label),
            scope.map(|s| s.phase),
            scope.map_or(0, |s| s.sweep),
            scope.map_or(0, |s| s.block),
            scope.map_or(0, |s| s.statement),
            self.operation,
            bytes[1],
            bytes[0],
            self.created[1],
            self.created[0],
            self.completed,
            self.operation_counts,
            self.input_nodes,
            self.peak_graph_and_scratch_capacity,
        );
        self.reported = Instant::now();
    }
}

pub(crate) fn scratch(bytes: usize) {
    with_profile(|p| {
        let live_bytes = p
            .live
            .lock()
            .expect("graph accounting")
            .bytes
            .iter()
            .sum::<usize>();
        p.peak_graph_and_scratch_capacity =
            p.peak_graph_and_scratch_capacity.max(live_bytes + bytes);
    });
}

pub(crate) fn created(key: GraphFingerprint) {
    with_profile(|p| {
        let (_, nodes, size, name) = key;
        p.created[usize::from(name.contains("SlotChoice"))] += nodes * size;
        p.completed += 1;
        if p.completed.is_multiple_of(1024) && p.reported.elapsed().as_secs_f64() >= 1.0 {
            p.report("tick");
        }
    });
}

pub(crate) struct ProfileScope {
    depth: Option<usize>,
    thread: PhantomData<*const ()>,
}

impl ProfileScope {
    pub(crate) fn new(label: impl FnOnce() -> String) -> Self {
        // Labels can perform salsa lookups, so evaluate outside the PROFILE borrow.
        let label = enabled().then(label);
        Self {
            depth: label.and_then(|label| {
                with_profile(|p| {
                    let depth = p.scopes.len();
                    p.scopes.push(Scope {
                        label,
                        started: Instant::now(),
                        created: p.created.iter().sum(),
                        sweep: 0,
                        block: 0,
                        statement: 0,
                        phase: "prepare",
                    });
                    depth
                })
            }),
            thread: PhantomData,
        }
    }

    pub(crate) fn point(&self, phase: &'static str, block: usize, statement: usize) {
        if let Some(depth) = self.depth {
            PROFILE.with_borrow_mut(|p| {
                let scope = &mut p.as_mut().expect("active profile").scopes[depth];
                scope.phase = phase;
                scope.block = block;
                scope.statement = statement;
            });
        }
    }

    pub(crate) fn sweep(&self, loans: usize, generation: usize) {
        if let Some(depth) = self.depth {
            PROFILE.with_borrow_mut(|p| {
                let p = p.as_mut().expect("active profile");
                let scope = &mut p.scopes[depth];
                scope.sweep += 1;
                scope.phase = "sweep";
                if scope.started.elapsed() >= Duration::from_millis(500)
                    && p.reported.elapsed() >= Duration::from_millis(500)
                {
                    eprintln!(
                        "SOLVER_SWEEP scope={:?} sweep={} loans={loans} generation={generation}",
                        scope.label, scope.sweep
                    );
                    p.report("sweep");
                }
            });
        }
    }
}

impl Drop for ProfileScope {
    fn drop(&mut self) {
        let Some(depth) = self.depth else { return };
        let overlap = PROFILE.with_borrow_mut(|p| {
            let p = p.as_mut().expect("active profile");
            assert_eq!(
                p.scopes.len(),
                depth + 1,
                "profile scopes must drop in stack order"
            );
            let scope = &p.scopes[depth];
            if scope.started.elapsed().as_secs_f64() >= 0.1
                || p.created.iter().sum::<usize>() - scope.created >= 32 * 1024 * 1024
            {
                eprintln!(
                    "SOLVER_SCOPE seconds={:.3} scope={:?}",
                    scope.started.elapsed().as_secs_f64(),
                    scope.label
                );
                p.report("scope_exit");
            }
            let overlap = p
                .live
                .lock()
                .expect("graph accounting")
                .bytes
                .iter()
                .sum::<usize>()
                >= p.next_overlap_bytes;
            if overlap {
                p.report("subgraph_snapshot");
                p.next_overlap_bytes = p.next_overlap_bytes.saturating_mul(2);
            }
            p.scopes.pop();
            overlap
        });
        if overlap {
            guard::profile_subgraphs();
        }
    }
}

pub(crate) struct Operation {
    previous: Option<&'static str>,
    thread: PhantomData<*const ()>,
}

impl Operation {
    pub(crate) fn new(kind: usize, nodes: usize) -> Self {
        Self {
            previous: with_profile(|p| {
                p.operation_counts[kind] += 1;
                p.input_nodes[kind] += nodes;
                std::mem::replace(
                    &mut p.operation,
                    ["map", "apply", "exists", "restrict"][kind],
                )
            }),
            thread: PhantomData,
        }
    }
}

impl Drop for Operation {
    fn drop(&mut self) {
        if let Some(previous) = self.previous {
            PROFILE.with_borrow_mut(|p| p.as_mut().expect("active profile").operation = previous);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{panic::catch_unwind, sync::mpsc, thread};

    fn install(config: Config) {
        TEST_CONFIG.set(Some(config));
        PROFILE.with_borrow_mut(|p| *p = config.enabled.then(|| Profile::new(config)));
    }

    #[test]
    fn config_requires_exact_activation_and_subgraph_opt_in() {
        let values = [
            None,
            Some(OsStr::new("")),
            Some(OsStr::new("0")),
            Some(OsStr::new("1")),
            Some(OsStr::new("true")),
        ];
        for profile in values {
            for subgraphs in values {
                let config = Config::from_vars(profile, subgraphs);
                assert_eq!(config.enabled, profile == Some(OsStr::new("1")));
                assert_eq!(
                    config.subgraphs,
                    config.enabled && subgraphs == Some(OsStr::new("1"))
                );
            }
        }
    }

    #[test]
    fn disabled_guards_do_not_evaluate_labels_or_borrow_profile() {
        thread::spawn(|| {
            install(Config::from_vars(None, None));
            PROFILE.with_borrow_mut(|p| {
                let scope = ProfileScope::new(|| panic!("disabled label evaluated"));
                scope.point("disabled", 0, 0);
                scope.sweep(0, 0);
                let operation = Operation::new(usize::MAX, usize::MAX);
                scratch(usize::MAX);
                created((0, usize::MAX, usize::MAX, "disabled"));
                drop((operation, scope));
                assert!(p.is_none());
            });
        })
        .join()
        .unwrap();
    }

    #[test]
    fn nested_guards_update_their_own_scope_and_restore_operations() {
        thread::spawn(|| {
            install(Config::from_vars(Some(OsStr::new("1")), None));
            // A label may allocate graphs through a salsa query.
            let outer = ProfileScope::new(|| {
                created((0, 1, 1, "label"));
                "outer".into()
            });
            let map = Operation::new(0, 2);
            let inner = ProfileScope::new(|| "inner".into());
            let apply = Operation::new(1, 3);
            outer.point("outer_point", 4, 5);
            inner.sweep(0, 0);
            PROFILE.with_borrow(|p| {
                let p = p.as_ref().unwrap();
                assert_eq!(p.scopes[0].phase, "outer_point");
                assert_eq!(p.scopes[0].block, 4);
                assert_eq!(p.scopes[0].statement, 5);
                assert_eq!(p.scopes[1].phase, "sweep");
                assert_eq!(p.scopes[1].sweep, 1);
                assert_eq!(p.operation, "apply");
            });
            drop((apply, inner));
            PROFILE.with_borrow(|p| {
                assert_eq!(p.as_ref().unwrap().operation, "map");
                assert_eq!(p.as_ref().unwrap().scopes.len(), 1);
            });
            drop((map, outer));
            PROFILE.with_borrow(|p| {
                assert_eq!(p.as_ref().unwrap().operation, "none");
                assert!(p.as_ref().unwrap().scopes.is_empty());
            });
        })
        .join()
        .unwrap();
    }

    #[test]
    fn sweeps_are_rate_limited_without_losing_progress() {
        thread::spawn(|| {
            install(Config::from_vars(Some(OsStr::new("1")), None));
            let scope = ProfileScope::new(|| "long-running solver".into());
            let old_report = Instant::now() - Duration::from_secs(1);
            PROFILE.with_borrow_mut(|p| {
                let p = p.as_mut().unwrap();
                p.scopes[0].started = old_report;
                p.reported = old_report;
            });
            scope.sweep(3, 1);
            PROFILE.with_borrow(|p| assert_ne!(p.as_ref().unwrap().reported, old_report));

            // Future instants keep ineligible cases deterministic without sleeps.
            let recent_report = Instant::now() + Duration::from_secs(60);
            PROFILE.with_borrow_mut(|p| p.as_mut().unwrap().reported = recent_report);
            scope.sweep(4, 2);
            scope.sweep(5, 3);
            PROFILE.with_borrow(|p| {
                let p = p.as_ref().unwrap();
                assert_eq!(p.reported, recent_report);
                assert_eq!(p.scopes[0].sweep, 3);
                assert_eq!(p.scopes[0].phase, "sweep");
            });

            PROFILE.with_borrow_mut(|p| p.as_mut().unwrap().reported = old_report);
            scope.sweep(6, 4);
            PROFILE.with_borrow(|p| {
                let p = p.as_ref().unwrap();
                assert_ne!(p.reported, old_report);
                assert_eq!(p.scopes[0].sweep, 4);
            });

            PROFILE.with_borrow_mut(|p| {
                let p = p.as_mut().unwrap();
                p.scopes[0].started = recent_report;
                p.reported = old_report;
            });
            scope.sweep(7, 5);
            PROFILE.with_borrow(|p| {
                let p = p.as_ref().unwrap();
                assert_eq!(p.reported, old_report);
                assert_eq!(p.scopes[0].sweep, 5);
            });
        })
        .join()
        .unwrap();
    }

    #[test]
    fn scope_exit_allows_subgraph_initialization_to_reenter_accounting() {
        let (tx, rx) = mpsc::channel();
        let worker = thread::spawn(move || {
            let result = catch_unwind(|| {
                install(Config::from_vars(
                    Some(OsStr::new("1")),
                    Some(OsStr::new("1")),
                ));
                PROFILE.with_borrow_mut(|p| p.as_mut().unwrap().next_overlap_bytes = 0);
                drop(ProfileScope::new(|| "fresh subgraph census".into()));
                PROFILE.with_borrow(|p| {
                    let p = p.as_ref().unwrap();
                    assert_eq!(
                        p.completed, 2,
                        "census must initialize both constant graphs"
                    );
                    assert!(p.scopes.is_empty());
                });
            });
            tx.send(result).unwrap();
        });
        rx.recv_timeout(Duration::from_secs(10))
            .expect("subgraph census deadlocked or disconnected")
            .expect("subgraph census reentered a borrowed profile");
        worker.join().unwrap();
    }
}
