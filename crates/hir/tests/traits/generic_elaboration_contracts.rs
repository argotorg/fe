use camino::Utf8PathBuf;
use common::{InputDb, indexmap::IndexMap, stdlib::HasBuiltinCore};
use fe_hir::{
    analysis::{
        semantic::{
            SemanticInstance, SemanticInstanceKey, check_semantic_accesses,
            identity_semantic_instance_key, root_semantic_instance_key,
        },
        ty::{
            binder::Binder,
            normalize::normalize_ty,
            trait_def::{AssocBinding, TraitInstId},
            trait_resolution::PredicateListId,
            ty_check::BodyOwner,
            ty_def::{Kind, TyData, TyId, TyVarSort},
            ty_lower::{
                CompleteSubst, LoweredSlot, ParamBasis, ParamDomainId, ParamKey, SourceParamIndex,
                param_schema,
            },
            unify::{InferenceKey, UnificationTableBase},
        },
    },
    hir_def::{CallableDef, IdentId, ItemKind},
    test_db::{HirAnalysisTestDb, find_func},
};

/// The selected callee of the first call site in `key`'s instance.
fn first_callee_key<'db>(
    db: &'db HirAnalysisTestDb,
    key: SemanticInstanceKey<'db>,
) -> SemanticInstanceKey<'db> {
    SemanticInstance::new(db, key)
        .call_sites(db)
        .iter()
        .flatten()
        .find_map(|site| site.callee)
        .expect("missing semantic callee")
        .key
}

#[test]
fn concrete_callees_do_not_inherit_unrelated_caller_bounds() {
    for source in [
        r#"
trait A {}
trait B {}
trait Out { fn f() {} }
impl Out for bool {}
fn first<T: A>() { <bool as Out>::f() }
fn second<U: B>() { <bool as Out>::f() }
"#,
        r#"
trait A {}
trait B {}
trait Need {}
impl Need for bool {}
fn target<T: Need>(_ x: T) {}
fn first<X: A>() { target(true) }
fn second<Y: B>() { target(true) }
"#,
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("concrete_callee_evidence.fe"), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let [first, second] = ["first", "second"].map(|name| {
            let func = find_func(&db, module, name);
            first_callee_key(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
            )
        });
        assert_eq!(first, second, "{source}");
    }
}

#[test]
fn concrete_roots_share_callee_keys_in_both_query_orders() {
    for source in [
        "fn target(_ x: u256) -> u256 { x }
fn caller() -> u256 { target(7) }",
        "struct Counter {}
impl Counter { fn target(_ x: u256) -> u256 { x } }
fn caller() -> u256 { Counter::target(7) }",
        "struct Counter { value: u256 }
impl Counter { fn target(mut self) { self.value += 1 } }
fn caller(_ x: mut Counter) { x.target() }",
    ] {
        for root_first in [true, false] {
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone(Utf8PathBuf::from("root_callee_sharing.fe"), source);
            let (module, _) = db.top_mod(file);
            let target = BodyOwner::Func(
                module
                    .all_funcs(&db)
                    .iter()
                    .copied()
                    .find(|func| {
                        func.name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "target")
                    })
                    .expect("target function or method"),
                None,
            );
            let caller = BodyOwner::Func(find_func(&db, module, "caller"), None);
            let root = || root_semantic_instance_key(&db, target).expect("closed root");
            let called = || first_callee_key(&db, identity_semantic_instance_key(&db, caller));
            let (root, called) = if root_first {
                (root(), called())
            } else {
                let called = called();
                (root(), called)
            };
            assert_eq!(root, called, "{source}");
            assert_eq!(identity_semantic_instance_key(&db, target), called);
            let instance = SemanticInstance::new(&db, root);
            check_semantic_accesses(&db, instance).expect("valid root accesses");
            db.assert_no_diags(module);
        }
    }
}

#[test]
fn symbolic_and_effect_roots_keep_their_declaration_environment() {
    for source in [
        "trait Value {}
fn target<T: Value>(_ x: own T) -> T { x }",
        "fn target() uses (value: mut u256) { value += 1 }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("root_callee_evidence.fe"), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(find_func(&db, module, "target"), None);
        let key = identity_semantic_instance_key(&db, owner);
        assert_eq!(key.impl_env(&db).normalization_scope(&db), owner.scope());
        let instance = SemanticInstance::new(&db, key);
        check_semantic_accesses(&db, instance).expect("valid root accesses");
    }
}

#[test]
fn root_callee_sharing_preserves_the_callers_ingot() {
    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    let mut files = Vec::new();
    for (name, manifest, source) in [
        (
            "library",
            "",
            "pub fn target(_ x: u256) -> u256 { x }\nfn caller() -> u256 { target(7) }",
        ),
        (
            "first",
            "[dependencies]\nlibrary = { path = \"../library\" }",
            "fn caller() -> u256 { library::target(7) }",
        ),
        (
            "second",
            "[dependencies]\nlibrary = { path = \"../library\" }",
            "fn caller() -> u256 { library::target(7) }",
        ),
    ] {
        db.workspace().touch(
            &mut db,
            format!("file:///root-callee-ingots/{name}/fe.toml")
                .parse()
                .unwrap(),
            Some(format!(
                "[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{manifest}\n"
            )),
        );
        files.push(
            db.workspace().touch(
                &mut db,
                format!("file:///root-callee-ingots/{name}/src/lib.fe")
                    .parse()
                    .unwrap(),
                Some(source.to_string()),
            ),
        );
    }
    let keys = files
        .into_iter()
        .map(|file| {
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            first_callee_key(
                &db,
                identity_semantic_instance_key(
                    &db,
                    BodyOwner::Func(find_func(&db, module, "caller"), None),
                ),
            )
        })
        .collect::<Vec<_>>();
    let root = root_semantic_instance_key(&db, keys[0].owner(&db)).expect("closed library root");
    assert_eq!(root, keys[0]);
    for key in &keys[1..] {
        assert_eq!(root.owner(&db), key.owner(&db));
        assert_eq!(root.subst(&db), key.subst(&db));
        assert_eq!(root.effect_providers(&db), key.effect_providers(&db));
        assert_ne!(root.impl_env(&db), key.impl_env(&db));
    }
    assert_ne!(keys[1], keys[2]);
}

#[test]
fn source_and_full_method_schemas_keep_inherited_keys_in_both_query_orders() {
    for source_first in [true, false] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("method_schema_basis.fe"),
            r#"
struct K {}
trait T<X> { fn f<Y>(_ extra: Y) uses (k: K) {} }
fn foreign<Z>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = module
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "f")
            })
            .expect("missing trait method");
        let (source, full) = if source_first {
            (
                param_schema(&db, func.into(), ParamBasis::Source),
                param_schema(&db, func.into(), ParamBasis::Full),
            )
        } else {
            let full = param_schema(&db, func.into(), ParamBasis::Full);
            let source = param_schema(&db, func.into(), ParamBasis::Source);
            (source, full)
        };
        assert_eq!(source.basis(&db), ParamBasis::Source);
        assert_eq!(full.basis(&db), ParamBasis::Full);
        assert!(
            full.keys(&db)
                .iter()
                .any(|key| matches!(key, ParamKey::EffectProvider { .. })),
            "method signature did not reserve a hidden provider slot"
        );
        let own_key = source
            .source_key(&db, SourceParamIndex(0))
            .expect("missing method source parameter");
        assert_eq!(full.source_key(&db, SourceParamIndex(0)), Some(own_key));
        assert!(
            full.slot_for(&db, own_key).expect("missing full slot").0
                > source
                    .slot_for(&db, own_key)
                    .expect("missing source slot")
                    .0
        );
        assert_eq!(&source.keys(&db)[..2], &full.keys(&db)[..2]);
        let inherited = source.key_at(&db, LoweredSlot(0)).unwrap();
        let inherited_formal = source.formal_at(&db, LoweredSlot(0)).unwrap();
        assert_eq!(source.original_key(&db, inherited_formal), Some(inherited));
        assert_eq!(full.original_key(&db, inherited_formal), Some(inherited));
        let ParamKey::TraitSelf(trait_) = inherited else {
            panic!("first inherited key should be trait Self");
        };
        let parent = param_schema(&db, trait_.into(), ParamBasis::Full);
        let parent_formal = parent.formal_at(&db, LoweredSlot(0)).unwrap();
        assert_ne!(parent_formal, inherited_formal);
        assert_eq!(full.original_key(&db, parent_formal), Some(inherited));
        let TyData::TyParam(param) = inherited_formal.data(&db) else {
            panic!("trait Self should be a parameter");
        };
        assert_eq!(param.owner, func.scope());

        let source_own = source
            .formal_at(&db, source.slot_for(&db, own_key).unwrap())
            .unwrap();
        let full_own = full
            .formal_at(&db, full.slot_for(&db, own_key).unwrap())
            .unwrap();
        assert_eq!(source.original_key(&db, source_own), Some(own_key));
        assert_eq!(full.original_key(&db, full_own), Some(own_key));
        assert_eq!(full.original_key(&db, source_own), None);
        let hidden_key = full.key_at(&db, LoweredSlot(2)).unwrap();
        assert!(matches!(hidden_key, ParamKey::EffectProvider { .. }));
        let hidden = full.formal_at(&db, LoweredSlot(2)).unwrap();
        assert_eq!(source.original_key(&db, hidden), None);

        let foreign = module
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|candidate| {
                candidate
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "foreign")
            })
            .expect("missing foreign function");
        let foreign_param = CallableDef::Func(foreign).params(&db)[0];
        assert_eq!(source.original_key(&db, foreign_param), None);
        assert_eq!(full.original_key(&db, foreign_param), None);

        let domain = ParamDomainId::full(&db, full);
        let values = domain
            .slots(&db)
            .map(|slot| full.formal_at(&db, slot).unwrap())
            .collect();
        let subst = CompleteSubst::new(domain, &db, values).unwrap();
        assert_eq!(subst.get(&db, own_key), Some(full_own));
        let prefix = source
            .allowed_default_dependencies(&db, SourceParamIndex(0))
            .unwrap();
        assert!(prefix.position_for(&db, inherited).is_some());
        assert_eq!(prefix.position_for(&db, own_key), None);
    }
}

#[test]
fn sealed_effect_identity_retains_bodyless_call_after_semantic_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("sealed_effect_trait_selection.fe"),
        r#"
use std::evm::RawStorage
fn access() -> u256 uses (raw: mut RawStorage) { unsafe { raw.sload(0) } }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "access");
    let identity = identity_semantic_instance_key(&db, BodyOwner::Func(func, None));
    let instance = SemanticInstance::new(&db, identity);
    check_semantic_accesses(&db, instance).expect("sealed effect call must pass access validation");
    let BodyOwner::Func(selected, _) = first_callee_key(&db, identity).owner(&db) else {
        panic!("sealed effect callee is not a function");
    };
    assert!(
        selected.body(&db).is_none(),
        "sealed effect call unexpectedly selected its runtime body in HIR"
    );

    let root_key = root_semantic_instance_key(&db, BodyOwner::Func(func, None))
        .expect("standalone root must synthesize the sealed effect provider");
    let BodyOwner::Func(selected, _) = first_callee_key(&db, root_key).owner(&db) else {
        panic!("root sealed effect callee is not a function");
    };
    assert!(
        selected.body(&db).is_some(),
        "standalone root did not select a concrete effect method body"
    );
}

#[test]
fn substituting_projection_and_evidence_preserves_both_arguments_and_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("projection_binding_substitution.fe"),
        "trait Out { type Item }\nfn f<T>() {}\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let (trait_, func) =
        module
            .children_non_nested(&db)
            .fold((None, None), |acc, item| match item {
                ItemKind::Trait(trait_) => (Some(trait_), acc.1),
                ItemKind::Func(func) => (acc.0, Some(func)),
                _ => acc,
            });
    let trait_ = trait_.expect("missing Out");
    let func = func.expect("missing f");
    let param = CallableDef::Func(func).params(&db)[0];
    let item = IdentId::new(&db, "Item");
    let inst = TraitInstId::new(
        &db,
        trait_,
        vec![param],
        IndexMap::from_iter([(AssocBinding::of(item), param)]),
        fe_hir::hir_def::ModeMarker::None,
    );
    let projection = TyId::assoc_ty(&db, inst.trait_ref(&db), item);
    let result = Binder::bind(func.into(), projection).instantiate(&db, &[TyId::bool(&db)]);
    let evidence = Binder::bind(func.into(), inst).instantiate(&db, &[TyId::bool(&db)]);
    let TyData::AssocTy(result) = result.data(&db) else {
        panic!("expected projection after substitution");
    };
    assert_eq!(result.trait_.args(&db), &[TyId::bool(&db)]);
    assert_eq!(
        evidence
            .assoc_type_bindings(&db)
            .get(&AssocBinding::of(item)),
        Some(&TyId::bool(&db)),
        "associated binding retained a stale declaration parameter"
    );
}

#[test]
fn projection_normalization_does_not_commit_speculative_trait_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("speculative_trait_reference_matching.fe"),
        "trait Out<A> { type Item }\nimpl Out<u8> for bool { type Item = u256 }\nfn f<T>() {}\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let trait_ = module
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Trait(trait_) => Some(trait_),
            _ => None,
        })
        .expect("missing Out");
    let item = IdentId::new(&db, "Item");
    let mut table = UnificationTableBase::<ena::unify::InPlace<InferenceKey<'_>>>::new(&db);
    let unknown = table.new_var(TyVarSort::General, &Kind::Star);
    let projection_ref = TraitInstId::new_simple(&db, trait_, vec![TyId::bool(&db), unknown]);
    let projection = TyId::assoc_ty(&db, projection_ref.trait_ref(&db), item);
    let assumption = TraitInstId::new(
        &db,
        trait_,
        vec![TyId::bool(&db), TyId::u8(&db)],
        IndexMap::from_iter([(AssocBinding::of(item), TyId::u256(&db))]),
        fe_hir::hir_def::ModeMarker::None,
    );
    let assumptions = PredicateListId::new(&db, vec![assumption]);
    assert_eq!(
        normalize_ty(&db, projection, module.scope(), assumptions),
        projection,
        "possible equality or impl selection cannot assign a caller inference variable"
    );
}
