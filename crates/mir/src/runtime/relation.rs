//! Typed runtime equivalence for differently spelled classes.
//!
//! Raw pointees are identified by immutable recipes, so the same runtime type
//! can be spelled as a deferred source recipe in one place and as an exact
//! class in another, and two aggregates holding such pointees can have
//! different `LayoutId`s. These relations answer whether two spellings denote
//! the same runtime typing. They never replace identity: interning, hashing,
//! and exact emitted-boundary checks still use `==`.
//!
//! Comparison is identity first, then one-level resolved-class identity, and
//! only then a bisimulation over the class/layout/pointee graph. The graph walk
//! treats a revisited pair as satisfied; a mismatch anywhere reachable from the
//! root still fails the whole query, and no child result is published.

use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{LayoutId, LayoutKey, RawPointeeId, RefView, RuntimeClass, ScalarRole},
};

/// Pair budget guarding against non-regular type families, whose unfolding
/// never revisits a pair. Well-typed programs compare regular graphs.
const MAX_PAIR_OBLIGATIONS: usize = 1 << 20;

pub fn raw_pointees_equivalent<'db>(
    db: &'db dyn MirDb,
    lhs: RawPointeeId<'db>,
    rhs: RawPointeeId<'db>,
) -> bool {
    if lhs == rhs {
        return true;
    }
    let lhs = lhs.target(db);
    let rhs = rhs.target(db);
    lhs == rhs || graph_equivalent(db, Obligation::Class(lhs, rhs))
}

pub fn raw_pointee_matches_class<'db>(
    db: &'db dyn MirDb,
    pointee: RawPointeeId<'db>,
    class: &RuntimeClass<'db>,
) -> bool {
    let target = pointee.target(db);
    target == *class || graph_equivalent(db, Obligation::Class(target, class.clone()))
}

/// Whether dereferencing `transport` yields a class equivalent to `target`.
/// Opaque raw addresses and non-transports never match.
pub fn transport_target_matches<'db>(
    db: &'db dyn MirDb,
    transport: &RuntimeClass<'db>,
    target: &RuntimeClass<'db>,
) -> bool {
    match transport {
        RuntimeClass::Ref { pointee, .. } => runtime_classes_equivalent(db, pointee, target),
        RuntimeClass::RawAddr {
            pointee: Some(pointee),
            ..
        } => raw_pointee_matches_class(db, *pointee, target),
        RuntimeClass::RawAddr { pointee: None, .. }
        | RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. } => false,
    }
}

pub fn runtime_classes_equivalent<'db>(
    db: &'db dyn MirDb,
    lhs: &RuntimeClass<'db>,
    rhs: &RuntimeClass<'db>,
) -> bool {
    lhs == rhs || graph_equivalent(db, Obligation::Class(lhs.clone(), rhs.clone()))
}

pub fn runtime_layouts_equivalent<'db>(
    db: &'db dyn MirDb,
    lhs: LayoutId<'db>,
    rhs: LayoutId<'db>,
) -> bool {
    lhs == rhs || graph_equivalent(db, Obligation::Layout(lhs, rhs))
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum Obligation<'db> {
    Class(RuntimeClass<'db>, RuntimeClass<'db>),
    Layout(LayoutId<'db>, LayoutId<'db>),
    Pointee(RawPointeeId<'db>, RawPointeeId<'db>),
}

fn graph_equivalent<'db>(db: &'db dyn MirDb, root: Obligation<'db>) -> bool {
    let mut pending = vec![root];
    let mut seen = FxHashSet::default();
    while let Some(obligation) = pending.pop() {
        let identical = match &obligation {
            Obligation::Class(lhs, rhs) => lhs == rhs,
            Obligation::Layout(lhs, rhs) => lhs == rhs,
            Obligation::Pointee(lhs, rhs) => lhs == rhs,
        };
        if identical || !seen.insert(obligation.clone()) {
            continue;
        }
        assert!(
            seen.len() <= MAX_PAIR_OBLIGATIONS,
            "runtime type equivalence exceeded {MAX_PAIR_OBLIGATIONS} pair obligations; \
             the compared pointee graphs are not regular"
        );
        let matched = match obligation {
            Obligation::Class(lhs, rhs) => push_class_edges(lhs, rhs, &mut pending),
            Obligation::Layout(lhs, rhs) => push_layout_edges(db, lhs, rhs, &mut pending),
            Obligation::Pointee(lhs, rhs) => {
                pending.push(Obligation::Class(lhs.target(db), rhs.target(db)));
                true
            }
        };
        if !matched {
            return false;
        }
    }
    true
}

fn push_class_edges<'db>(
    lhs: RuntimeClass<'db>,
    rhs: RuntimeClass<'db>,
    pending: &mut Vec<Obligation<'db>>,
) -> bool {
    match (lhs, rhs) {
        (RuntimeClass::Scalar(lhs), RuntimeClass::Scalar(rhs)) => {
            lhs.repr == rhs.repr
                && match (lhs.role, rhs.role) {
                    (ScalarRole::Plain, ScalarRole::Plain) => true,
                    (
                        ScalarRole::EnumTag { enum_layout: lhs },
                        ScalarRole::EnumTag { enum_layout: rhs },
                    ) => {
                        pending.push(Obligation::Layout(lhs, rhs));
                        true
                    }
                    (ScalarRole::Plain | ScalarRole::EnumTag { .. }, _) => false,
                }
        }
        (
            RuntimeClass::AggregateValue { layout: lhs },
            RuntimeClass::AggregateValue { layout: rhs },
        ) => {
            pending.push(Obligation::Layout(lhs, rhs));
            true
        }
        (
            RuntimeClass::Ref {
                pointee: lhs_pointee,
                kind: lhs_kind,
                view: lhs_view,
            },
            RuntimeClass::Ref {
                pointee: rhs_pointee,
                kind: rhs_kind,
                view: rhs_view,
            },
        ) => {
            let views_match = match (lhs_view, rhs_view) {
                (RefView::Whole, RefView::Whole) => true,
                (RefView::EnumVariant(lhs), RefView::EnumVariant(rhs)) => {
                    pending.push(Obligation::Layout(lhs.enum_layout, rhs.enum_layout));
                    lhs.index == rhs.index
                }
                (RefView::Whole | RefView::EnumVariant(_), _) => false,
            };
            pending.push(Obligation::Class(*lhs_pointee, *rhs_pointee));
            views_match && lhs_kind == rhs_kind
        }
        (
            RuntimeClass::RawAddr {
                space: lhs_space,
                pointee: lhs_pointee,
            },
            RuntimeClass::RawAddr {
                space: rhs_space,
                pointee: rhs_pointee,
            },
        ) => {
            lhs_space == rhs_space
                && match (lhs_pointee, rhs_pointee) {
                    (None, None) => true,
                    (Some(lhs), Some(rhs)) => {
                        pending.push(Obligation::Pointee(lhs, rhs));
                        true
                    }
                    (None, Some(_)) | (Some(_), None) => false,
                }
        }
        (
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref { .. }
            | RuntimeClass::RawAddr { .. },
            _,
        ) => false,
    }
}

/// Compares layout keys directly. `LayoutId::data` synthesizes an enum tag
/// owned by the enum itself; following that role would compare the pair
/// against itself, so enum identity is decided by its variants alone.
fn push_layout_edges<'db>(
    db: &'db dyn MirDb,
    lhs: LayoutId<'db>,
    rhs: LayoutId<'db>,
    pending: &mut Vec<Obligation<'db>>,
) -> bool {
    let mut push_fields = |lhs: &[RuntimeClass<'db>], rhs: &[RuntimeClass<'db>]| {
        pending.extend(
            lhs.iter()
                .zip(rhs)
                .map(|(lhs, rhs)| Obligation::Class(lhs.clone(), rhs.clone())),
        );
        lhs.len() == rhs.len()
    };
    match (lhs.key(db), rhs.key(db)) {
        (LayoutKey::Struct(lhs), LayoutKey::Struct(rhs)) => push_fields(&lhs.fields, &rhs.fields),
        (LayoutKey::Array(lhs), LayoutKey::Array(rhs)) => {
            lhs.len == rhs.len
                && push_fields(
                    std::slice::from_ref(&lhs.elem),
                    std::slice::from_ref(&rhs.elem),
                )
        }
        (LayoutKey::Enum(lhs), LayoutKey::Enum(rhs)) => {
            lhs.variants.len() == rhs.variants.len()
                && lhs
                    .variants
                    .iter()
                    .zip(rhs.variants.iter())
                    .all(|(lhs, rhs)| push_fields(&lhs.fields, &rhs.fields))
        }
        (LayoutKey::Struct(_) | LayoutKey::Array(_) | LayoutKey::Enum(_), _) => false,
    }
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;

    use super::*;
    use crate::runtime::{
        AddressSpaceKind, EnumLayoutKey, EnumVariantLayout, RefKind, ScalarClass, ScalarRepr,
        StructLayout, VariantId,
    };

    fn int<'db>(bits: u16) -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    fn enum_layout<'db>(db: &'db dyn MirDb, payload: RuntimeClass<'db>) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: vec![
                    EnumVariantLayout {
                        fields: vec![payload].into(),
                    },
                    EnumVariantLayout {
                        fields: vec![].into(),
                    },
                ]
                .into(),
            }),
        )
    }

    fn holder<'db>(db: &'db dyn MirDb, target: RuntimeClass<'db>) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                fields: vec![RuntimeClass::raw_addr(db, AddressSpaceKind::Memory, target)].into(),
            }),
        )
    }

    fn tag<'db>(enum_layout: LayoutId<'db>) -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 8,
                signed: false,
            },
            role: ScalarRole::EnumTag { enum_layout },
        })
    }

    #[test]
    fn raw_targets_compare_space_and_knownness() {
        let db = DriverDataBase::default();
        let word = RuntimeClass::raw_addr(&db, AddressSpaceKind::Memory, int(256));
        let byte = RuntimeClass::raw_addr(&db, AddressSpaceKind::Memory, int(8));
        let opaque = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Memory);
        let storage = RuntimeClass::raw_addr(&db, AddressSpaceKind::Storage, int(256));
        assert!(runtime_classes_equivalent(&db, &word, &word));
        assert!(!runtime_classes_equivalent(&db, &word, &byte));
        assert!(!runtime_classes_equivalent(&db, &word, &opaque));
        assert!(!runtime_classes_equivalent(&db, &opaque, &word));
        assert!(!runtime_classes_equivalent(&db, &word, &storage));
    }

    #[test]
    fn enum_owners_compare_by_variant_structure() {
        let db = DriverDataBase::default();
        let pointer = |bits| RuntimeClass::raw_addr(&db, AddressSpaceKind::Memory, int(bits));
        let exact = enum_layout(&db, pointer(256));
        let respelled = enum_layout(
            &db,
            RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                pointee: Some(RawPointeeId::exact(&db, int(256))),
            },
        );
        assert_eq!(exact, respelled, "exact targets intern structurally");

        let other = enum_layout(&db, pointer(8));
        let lhs = holder(&db, RuntimeClass::AggregateValue { layout: exact });
        let rhs = holder(&db, RuntimeClass::AggregateValue { layout: other });
        assert!(!runtime_layouts_equivalent(&db, lhs, rhs));
        assert!(runtime_classes_equivalent(&db, &tag(exact), &tag(exact)));
        assert!(!runtime_classes_equivalent(&db, &tag(exact), &tag(other)));

        let view = |layout, index| RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue { layout }),
            kind: RefKind::Object,
            view: RefView::EnumVariant(VariantId {
                enum_layout: layout,
                index,
            }),
        };
        assert!(runtime_classes_equivalent(
            &db,
            &view(exact, 0),
            &view(exact, 0)
        ));
        assert!(!runtime_classes_equivalent(
            &db,
            &view(exact, 0),
            &view(exact, 1)
        ));
        assert!(!runtime_classes_equivalent(
            &db,
            &view(exact, 0),
            &view(other, 0)
        ));
    }
}
