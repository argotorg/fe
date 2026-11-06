use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{
            CarrierBase, CarrierTarget, FieldIndex, NDataProjection, NExpr, NIndex, NPlaceBase,
            NRootKind, NStatementKind, NValueDefinition, NValueId, NormalizedBody,
            SemanticInstance, identity_semantic_instance_key, normalize_semantic_body,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
    test_db::HirAnalysisTestDb,
};

const SOURCE: &str = r#"
struct Pair { x: u256, y: u256 }

fn bump(_ x: mut u256) {
    x += 1
}

fn local_field() {
    var p = Pair { x: 1, y: 2 }
    bump(mut p.x)
}

fn param_element(_ m: mut [u256; 4], _ i: usize) {
    bump(mut m[i])
}

fn derived(_ p: mut Pair) {
    let h = mut p
    bump(mut h.y)
}

fn first(_ p: mut Pair) -> mut u256 {
    yield mut p.x
}

fn granted(_ p: mut Pair) {
    let x = mut first(mut p)
    x = 5
}

fn product_part(_ (mut a, mut b): (mut u256, mut u256)) {
    bump(mut b)
}

trait Driver {
    shape Step
    fn at(mut? self) -> Self::Step
}

impl Driver for Pair {
    shape Step = (own u256, mut? u256)
    fn at(mut? self) -> (own u256, mut? u256) {
        (self.x, mut? self.y)
    }
}

fn shape_part(_ s: Pair::Step<mut>) {
    let (n, mut b) = s
    bump(mut b)
}

fn shape_part_named(_ s: Pair::Step<ref>) -> u256 {
    let x = ref s.1
    x + s.0
}
"#;

/// Calls `check` with the normalized body of `name` in `SOURCE`.
fn with_body(name: &str, check: impl FnOnce(&HirAnalysisTestDb, &NormalizedBody<'_>)) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("carrier_targets.fe".into(), SOURCE);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let db = &db;
    let func = top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name) =>
            {
                Some(*func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"));
    let instance = SemanticInstance::new(
        db,
        identity_semantic_instance_key(db, BodyOwner::Func(func, None)),
    );
    let body = normalize_semantic_body(db, instance)
        .expect("normalized body")
        .body
        .clone();
    check(db, &body);
}

/// Calls `check` with the normalized body of `name` in `SOURCE` and the
/// targets of the accesses it passes to a call or writes through.
fn with_targets(name: &str, check: impl FnOnce(&NormalizedBody<'_>, &[CarrierTarget<'_>])) {
    with_body(name, |db, body| {
        let carriers: Vec<NValueId> = body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { args, .. },
                    ..
                } => args.first().map(|arg| arg.value),
                NStatementKind::Store { destination, .. } => match destination.base {
                    NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
                    _ => None,
                },
                _ => None,
            })
            .filter(|value| body.values[value.index()].ty.as_capability(db).is_some())
            .collect();
        let targets: Vec<_> = carriers
            .into_iter()
            .map(|carrier| {
                body.carrier_target(db, carrier)
                    .unwrap_or_else(|| panic!("no target for {carrier:?} in `{name}`"))
            })
            .collect();
        check(body, &targets);
    });
}

#[test]
fn local_field_access_targets_its_root() {
    with_targets("local_field", |_, targets| {
        let [target] = targets else {
            panic!("expected one access: {targets:?}");
        };
        assert!(matches!(target.base, CarrierBase::Root(_)), "{target:?}");
        assert_eq!(
            target.path.as_slice(),
            [NDataProjection::Field(FieldIndex(0))]
        );
    });
}

#[test]
fn param_element_access_keeps_its_index() {
    with_targets("param_element", |body, targets| {
        let [target] = targets else {
            panic!("expected one access: {targets:?}");
        };
        assert_eq!(target.base, CarrierBase::Param { param: 0 });
        let [NDataProjection::Index(NIndex::Value(index))] = target.path.as_slice() else {
            panic!("expected a dynamic index: {target:?}");
        };
        // The index is `i` itself, not an abstract `[*]`: the parameter, or a
        // read of the slot holding it.
        let mut index = *index;
        while let Some((_, NExpr::Forward { src })) = body.defining_expr(index) {
            index = src.value;
        }
        let is_i = match body.defining_expr(index) {
            Some((_, NExpr::Load { place, .. })) => matches!(
                place.base,
                NPlaceBase::Root(root) if body.roots[root.index()].kind == NRootKind::ParamPlace { param: 1 }
            ),
            _ => body.values[index.index()].definition == NValueDefinition::EntryParam { param: 1 },
        };
        assert!(is_i, "the index is not `i`: {target:?}");
    });
}

#[test]
fn derived_access_targets_the_original_place() {
    with_targets("derived", |_, targets| {
        let [target] = targets else {
            panic!("expected one access: {targets:?}");
        };
        assert_eq!(target.base, CarrierBase::Param { param: 0 });
        assert_eq!(
            target.path.as_slice(),
            [NDataProjection::Field(FieldIndex(1))]
        );
    });
}

#[test]
fn projection_result_targets_its_grant() {
    with_targets("granted", |_, targets| {
        assert!(
            targets
                .iter()
                .any(|target| matches!(target.base, CarrierBase::Grant { .. })),
            "{targets:?}"
        );
    });
}

#[test]
fn product_parameter_part_targets_its_input() {
    with_targets("product_part", |_, targets| {
        let [target] = targets else {
            panic!("expected one access: {targets:?}");
        };
        assert_eq!(target.base, CarrierBase::Param { param: 1 });
        assert!(target.path.is_empty(), "{target:?}");
    });
}

#[test]
fn shape_parameter_part_targets_its_input() {
    with_targets("shape_part", |_, targets| {
        let [target] = targets else {
            panic!("expected one access: {targets:?}");
        };
        assert_eq!(target.base, CarrierBase::Param { param: 1 });
        assert!(target.path.is_empty(), "{target:?}");
    });
}

#[test]
fn access_loaded_from_a_slot_has_no_target() {
    // `ref s.1` spills `s` to a slot, and the access is loaded back out of it.
    with_body("shape_part_named", |db, body| {
        let loaded: Vec<_> = body
            .values
            .iter()
            .enumerate()
            .map(|(index, value)| (NValueId::new(index), value))
            .filter(|(id, value)| {
                value.ty.as_capability(db).is_some()
                    && matches!(body.defining_expr(*id), Some((_, NExpr::Load { .. })))
            })
            .map(|(id, _)| id)
            .collect();
        assert!(!loaded.is_empty());
        for carrier in loaded {
            assert_eq!(body.carrier_target(db, carrier), None);
        }
    });
}
