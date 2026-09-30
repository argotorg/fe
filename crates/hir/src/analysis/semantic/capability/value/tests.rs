use super::*;
use crate::{
    analysis::semantic::capability::tests::{array_shape, leaf, leaf_shape, runtime, scope},
    test_db::HirAnalysisTestDb,
};

#[test]
fn repeated_operations_reuse_live_results() {
    let db = HirAnalysisTestDb::default();
    let shape = leaf_shape(&db);
    let mut values = ValueInterner::new(
        &db,
        ValueLimits {
            guarded_alternatives: 1,
            ..ValueLimits::default()
        },
    );
    let left = leaf(&mut values, shape, &scope(), 1, vec![runtime(0)]);
    let right = leaf(&mut values, shape, &scope(), 2, vec![runtime(1)]);
    let guard = Guard::always(&scope()).with_bound(runtime(2), 3).unwrap();
    let joined = values.join(&left, &right);
    let guarded = values.with_guard(&joined, &guard);
    let widened = values.widen(&guarded);
    assert_ne!(joined, guarded);
    assert_ne!(guarded, widened);
    let evaluated = values.metrics().operations_evaluated;
    assert!(evaluated > 0);
    for _ in 0..8 {
        assert_eq!(values.join(&left, &right), joined);
        assert_eq!(values.with_guard(&joined, &guard), guarded);
        assert_eq!(values.widen(&guarded), widened);
        assert_eq!(values.metrics().operations_evaluated, evaluated);
    }
}

#[test]
fn operation_caching_preserves_nested_array_semantics_across_eviction() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let inner = array_shape(&db, element, 3);
    let outer = array_shape(&db, inner, 4);
    let guard = Guard::always(&scope()).with_bound(runtime(0), 3).unwrap();
    let path = StructuralPath::new([
        Projection::Index(IndexExpr::Const(1)),
        Projection::Index(runtime(0)),
    ]);
    let mut reference = None;
    for interned_nodes in [None, Some(1), Some(4), Some(16_384)] {
        let mut values = ValueInterner::new(
            &db,
            ValueLimits {
                exact_members: 0,
                guarded_alternatives: 1,
                interned_nodes,
                ..ValueLimits::default()
            },
        );
        let left = leaf(&mut values, element, &scope(), 1, vec![runtime(1)]);
        let right = leaf(&mut values, element, &scope(), 2, vec![runtime(2)]);
        let row = values.array_repeat(inner, &left);
        let initial = values.array_repeat(outer, &row);
        let replaced = values
            .replace_family(
                &initial,
                &path,
                &right,
                &guard,
                &Guarded {
                    guard: Guard::always(&scope()),
                    payload: BTreeMap::new(),
                },
            )
            .unwrap();
        let restricted = values.with_guard(&replaced, &guard);
        let joined = values.join(&initial, &restricted);
        let widened = values.widen(&joined);
        assert_ne!(joined, widened, "exercise forced array collapse");
        let output = (replaced, restricted, joined, widened);
        for _ in 0..8 {
            assert_eq!(values.with_guard(&output.0, &guard), output.1);
            assert_eq!(values.join(&initial, &output.1), output.2);
            assert_eq!(values.widen(&output.2), output.3);
        }
        if let Some(reference) = &reference {
            assert_eq!(&output, reference, "cache budget {interned_nodes:?}");
        } else {
            reference = Some(output);
        }
    }
}

#[test]
fn forced_widening_is_distinct_from_normal_widening() {
    let db = HirAnalysisTestDb::default();
    let shape = leaf_shape(&db);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let initial = leaf(&mut values, shape, &scope(), 1, vec![]);
    let guard = Guard::always(&scope()).with_bound(runtime(0), 3).unwrap();
    let guarded = values.with_guard(&initial, &guard);
    let normal = values.widen_node(&guarded, false);
    let forced = values.widen_node(&guarded, true);
    assert_eq!(normal, guarded);
    assert_eq!(forced, initial);
    let evaluated = values.metrics().operations_evaluated;
    assert_eq!(values.widen_node(&guarded, true), forced);
    assert_eq!(values.widen_node(&guarded, false), normal);
    assert_eq!(values.metrics().operations_evaluated, evaluated);
}

#[test]
fn operation_cache_does_not_keep_values_or_guards_alive() {
    let db = HirAnalysisTestDb::default();
    let shape = leaf_shape(&db);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let left = leaf(&mut values, shape, &scope(), 1, vec![]);
    let right = leaf(&mut values, shape, &scope(), 2, vec![]);
    let guard = Guard::always(&scope()).with_bound(runtime(0), 3).unwrap();
    let joined = values.join(&left, &right);
    let guarded = values.with_guard(&joined, &guard);
    let widened = values.widen_node(&guarded, true);
    let probes = [&left, &right, &joined, &guarded, &widened].map(|value| Arc::downgrade(&value.0));
    let guard_probe = guard.downgrade();
    assert!(!values.operations.is_empty());
    // Release the pre-existing caches to isolate ownership by the operation cache.
    values.nodes.clear();
    values.normalized.clear();
    *values.guards.borrow_mut() = GuardCache::default();
    drop((left, right, joined, guarded, widened, guard));
    assert!(probes.iter().all(|probe| probe.upgrade().is_none()));
    assert!(!guard_probe.is_live());
    assert!(!values.operations.is_empty());
}

#[test]
fn no_cache_evaluates_each_operation_and_small_caches_stay_bounded() {
    let db = HirAnalysisTestDb::default();
    let shape = leaf_shape(&db);
    let mut uncached = ValueInterner::new(
        &db,
        ValueLimits {
            interned_nodes: None,
            ..ValueLimits::default()
        },
    );
    let left = leaf(&mut uncached, shape, &scope(), 1, vec![]);
    let right = leaf(&mut uncached, shape, &scope(), 2, vec![]);
    let guard = Guard::always(&scope()).with_bound(runtime(0), 3).unwrap();
    let joined = uncached.join(&left, &right);
    let guarded = uncached.with_guard(&joined, &guard);
    let widened = uncached.widen(&guarded);
    for _ in 0..8 {
        let evaluated = uncached.metrics().operations_evaluated;
        assert_eq!(uncached.join(&left, &right), joined);
        assert_eq!(uncached.with_guard(&joined, &guard), guarded);
        assert_eq!(uncached.widen(&guarded), widened);
        assert_eq!(uncached.metrics().operations_evaluated, evaluated + 3);
        assert!(uncached.operations.is_empty());
    }

    let mut bounded = ValueInterner::new(
        &db,
        ValueLimits {
            interned_nodes: Some(4),
            ..ValueLimits::default()
        },
    );
    let mut owners = Vec::new();
    for tag in 0..32 {
        let left = leaf(&mut bounded, shape, &scope(), tag, vec![]);
        let right = leaf(&mut bounded, shape, &scope(), tag + 1, vec![]);
        let result = bounded.join(&left, &right);
        owners.push((left, right, result));
        assert!(bounded.operations.len() <= 4);
    }
}
