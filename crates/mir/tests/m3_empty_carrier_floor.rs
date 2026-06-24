//! FCO M3 EMPTY-CARRIER byte-identity tripwire (interning + symbol layer).
//!
//! M3 widened `ImplEnv`'s scope-selection carrier from a single
//! `Option<ImplementorId>` to a per-goal `Vec<(TraitInstId, ImplementorId)>` so a
//! scoped selection can propagate through a generic helper boundary. The
//! load-bearing soundness floor is that an EMPTY carrier must be BYTE-IDENTICAL
//! to the pre-M3 `None` — at BOTH the salsa interning layer AND the codegen-
//! symbol layer — so every existing (non-scope-selected) instance interns and
//! symbolizes EXACTLY as before (no mass re-keying, no symbol churn, no
//! miscompile). The value-layer twin (manual `Hash`/`Eq`/`Update`, canonical
//! ordering, by-goal lookup) lives in
//! `crates/hir/.../instance/template.rs::impl_env_identity_tests`.
//!
//! Two legs:
//!
//! * INTERNING leg — a `SemanticInstanceKey` rebuilt with an explicitly-EMPTY
//!   carrier (`with_selected_implementors(vec![])`) must be the SAME interned key
//!   as the one the ordinary instance-builder mints (which carries an empty
//!   carrier for a non-scope-selected func). Equal salsa ids ⇒ the carrier field
//!   added nothing observable when empty.
//! * SYMBOL leg — the codegen-symbol identity of both keys' instances must be
//!   byte-equal strings, so the carrier discriminator contributes the EMPTY
//!   STRING for an empty carrier (the floor `stable_key.rs` guarantees).

use camino::Utf8PathBuf;
use fe_mir::runtime::stable_key::semantic_instance_symbol_identity;
use hir::analysis::semantic::{
    SemanticInstanceKey, get_or_build_semantic_instance, identity_semantic_instance_key,
};
use hir::analysis::ty::ty_check::BodyOwner;
use hir::hir_def::{Func, TopLevelMod};
use hir::test_db::HirAnalysisTestDb;

const SRC: &str = r#"
fn host() {}
"#;

fn host_func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == "host"))
        .expect("missing `host` function")
}

#[test]
fn empty_carrier_interns_and_symbolizes_identically_to_pre_m3() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("m3_empty_carrier_floor.fe"), SRC);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let host = host_func(&db, top_mod);

    // The key the ordinary instance-builder mints for a non-scope-selected func —
    // its `ImplEnv` carries an EMPTY carrier (today every instance does).
    let identity_key = identity_semantic_instance_key(&db, BodyOwner::Func(host));
    let identity_symbol =
        semantic_instance_symbol_identity(&db, get_or_build_semantic_instance(&db, identity_key));

    // Rebuild the SAME key but force the carrier explicitly empty via the M3
    // setter. If `with_selected_implementors(vec![])` is byte-identical to the
    // `None` floor, this must intern to the SAME salsa id.
    let empty_env = identity_key
        .impl_env(&db)
        .clone()
        .with_selected_implementors(vec![]);
    let rebuilt_key = SemanticInstanceKey::new(
        &db,
        identity_key.owner(&db),
        identity_key.subst(&db),
        identity_key.effect_providers(&db),
        empty_env,
    );

    // INTERNING leg: same interned key (salsa interned-id equality).
    assert_eq!(
        identity_key, rebuilt_key,
        "an explicitly-EMPTY carrier must INTERN identically to the pre-M3 `None` floor",
    );

    // SYMBOL leg: byte-equal codegen-symbol identity strings.
    let rebuilt_symbol =
        semantic_instance_symbol_identity(&db, get_or_build_semantic_instance(&db, rebuilt_key));
    assert_eq!(
        identity_symbol, rebuilt_symbol,
        "an explicitly-EMPTY carrier must SYMBOLIZE identically to the pre-M3 `None` floor\n\
         identity: {identity_symbol}\nrebuilt:  {rebuilt_symbol}",
    );

    // And the carrier discriminator must contribute NOTHING for an empty carrier:
    // the symbol must not mention the selection discriminator at all.
    assert!(
        !identity_symbol.contains("selected_implementor"),
        "an empty-carrier symbol must not carry a selection discriminator, got: {identity_symbol}",
    );
}
