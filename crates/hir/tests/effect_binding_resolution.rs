use fe_hir::analysis::semantic::owner_effect_bindings;
use fe_hir::analysis::ty::ty_check::{BodyOwner, EffectParamSite, LocalBinding};
use fe_hir::analysis::ty::{ProviderAddressSpace, ProviderKind};
use fe_hir::semantic::{EffectEnvView, ProviderSource};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn labeled_effect_bindings_keep_the_declared_name() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "labeled_effect_bindings_keep_the_declared_name.fe".into(),
        r#"trait Foo {
    fn bar(self) -> u256
}

fn f() -> u256 uses (foo: Foo) {
    0
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let funcs = top_mod.all_funcs(&db);
    let func = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "f")
        })
        .expect("expected function `f`");

    let bindings = owner_effect_bindings(&db, BodyOwner::Func(func));
    let [binding] = bindings.as_slice() else {
        panic!("expected exactly one effect binding");
    };
    let LocalBinding::EffectParam { binding_name, .. } = binding else {
        panic!("expected an effect binding");
    };
    assert_eq!(binding_name.data(&db), "foo");
}

#[test]
fn contract_field_effect_bindings_resolve_to_storage_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "contract_field_effect_bindings_resolve_to_storage_providers.fe".into(),
        r#"msg Msg {
    #[selector = 1]
    Test -> u256,
}

pub contract C {
    mut value: u256,

    recv Msg {
        Test -> u256 uses (mut value) {
            value
        }
    }
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let [contract] = top_mod.all_contracts(&db).as_slice() else {
        panic!("expected exactly one contract");
    };
    let site = EffectParamSite::ContractRecvArm {
        contract: *contract,
        recv_idx: 0,
        arm_idx: 0,
    };
    let resolved = EffectEnvView::new(site)
        .resolved_binding(&db, 0)
        .expect("expected a resolved effect binding");

    assert!(matches!(
        resolved.provider.source,
        ProviderSource::ContractField { field } if field.contract == *contract
    ));
    assert_eq!(
        resolved.provider.semantics.address_space,
        Some(ProviderAddressSpace::Storage)
    );
}

#[test]
fn contract_level_effect_bindings_forward_to_the_root_provider() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "contract_level_effect_bindings_forward_to_the_root_provider.fe".into(),
        r#"struct Ctx {}

msg Msg {
    #[selector = 1]
    Test -> u256,
}

pub contract C uses (ctx: Ctx) {
    recv Msg {
        Test -> u256 uses (ctx) {
            0
        }
    }
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let [contract] = top_mod.all_contracts(&db).as_slice() else {
        panic!("expected exactly one contract");
    };
    let site = EffectParamSite::ContractRecvArm {
        contract: *contract,
        recv_idx: 0,
        arm_idx: 0,
    };
    let resolved = EffectEnvView::new(site)
        .resolved_binding(&db, 0)
        .expect("expected a resolved effect binding");

    assert!(matches!(
        resolved.provider.source,
        ProviderSource::RootProvider { site: provider_site, .. } if provider_site == site
    ));
    assert_eq!(resolved.provider.semantics.kind, ProviderKind::RootObject);
}
