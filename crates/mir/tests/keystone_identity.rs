//! FCO KEYSTONE tripwire (Rung 0).
//!
//! A generated `impl Trait for Ty`'s salsa interning identity must be *stable*
//! under reordering of sibling derive targets. Before the keystone fix it was
//! NOT: the impl's identity bottomed out at a positional `TrackedItemVariant::
//! ImplTrait(u32)` counter, so swapping two `#[derive(Eq)]` structs renumbered
//! every downstream id and minted a *distinct* `TrackedItemId` (and therefore a
//! distinct `ImplementorId` / `SemanticInstanceKey`) for the very same
//! `impl Eq for Alpha`.
//!
//! Two legs, deliberately split to localize the bug:
//!
//! * INTERNING leg — the impl's `TrackedItemVariant` content view. RED before
//!   the fix (`ImplTrait(ord:0)` vs `ImplTrait(ord:1)`), GREEN after
//!   (`GeneratedImplTrait(goal:..,self:..)` identical across orderings).
//! * SYMBOL leg — the codegen-symbol identity of the generated `eq` method
//!   (`semantic_instance_symbol_identity`). GREEN both before and after: codegen
//!   symbols were already content-keyed (module path + name + trait/self), so
//!   they never carried the ordinal. This isolates the instability to the
//!   interning-identity layer.
//!
//! The two orderings are lowered in two SEPARATE databases that share the same
//! file path, so module-path components match and the symbol/content strings are
//! directly comparable across databases (salsa interned ids are not).

use camino::Utf8PathBuf;
use fe_mir::runtime::stable_key::semantic_instance_symbol_identity;
use hir::analysis::semantic::{get_or_build_semantic_instance, identity_semantic_instance_key};
use hir::analysis::ty::ty_check::BodyOwner;
use hir::hir_def::{Func, ImplTrait, TopLevelMod};
use hir::test_db::HirAnalysisTestDb;
use hir::{HirDb, lower::map_file_to_mod};

const ORDER_AB: &str = r#"
#[derive(Eq)]
pub struct Alpha {
    pub x: u8,
}

#[derive(Eq)]
pub struct Beta {
    pub y: u8,
}
"#;

const ORDER_BA: &str = r#"
#[derive(Eq)]
pub struct Beta {
    pub y: u8,
}

#[derive(Eq)]
pub struct Alpha {
    pub x: u8,
}
"#;

/// The generated `impl Eq for Alpha` in `top_mod`, found by content (goal trait
/// path ends in `Eq`, self type is `Alpha`). Panics if not present, which would
/// itself be a regression (the derive must expand).
fn eq_for_alpha<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> ImplTrait<'db> {
    // Match on the (ordinal-independent) pretty-printed head: `impl Eq<..> for
    // Alpha { .. }`. Works identically before and after the keystone fix, so the
    // *same* generated impl is selected in both regimes.
    top_mod
        .all_impl_traits(db)
        .iter()
        .copied()
        .find(|impl_trait| {
            let head = impl_trait.pretty_print(db);
            let head = head.split('{').next().unwrap_or("");
            head.contains("Eq") && head.contains("for Alpha")
        })
        .expect("generated `impl Eq for Alpha` must exist (derive must expand)")
}

/// The `eq` method of a generated `Eq` impl.
fn eq_method<'db>(db: &'db dyn HirDb, impl_trait: ImplTrait<'db>) -> Func<'db> {
    impl_trait
        .methods(db)
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == "eq"))
        .expect("generated `Eq` impl must have an `eq` method")
}

/// Lowers `src` (under a fixed file path) and returns the keystone projections
/// of its generated `impl Eq for Alpha`:
/// `(interning content view, codegen symbol identity of `eq`)`.
///
/// Takes `db` by value so the caller hands over a fresh database per ordering;
/// the fixed file path keeps module-path components identical across the two
/// databases, so the symbol/content strings are directly comparable.
fn projections(mut db: HirAnalysisTestDb, src: &str) -> (String, String) {
    let file = db.new_stand_alone(Utf8PathBuf::from("keystone.fe"), src);
    let top_mod = map_file_to_mod(&db, file);

    let impl_trait = eq_for_alpha(&db, top_mod);
    let interning = impl_trait.interning_identity_repr(&db);

    let eq = eq_method(&db, impl_trait);
    let key = identity_semantic_instance_key(&db, BodyOwner::Func(eq));
    let instance = get_or_build_semantic_instance(&db, key);
    let symbol = semantic_instance_symbol_identity(&db, instance);

    (interning, symbol)
}

#[test]
fn generated_impl_identity_is_stable_under_derive_reordering() {
    let (interning_ab, symbol_ab) = projections(HirAnalysisTestDb::default(), ORDER_AB);
    let (interning_ba, symbol_ba) = projections(HirAnalysisTestDb::default(), ORDER_BA);

    // SYMBOL leg: codegen symbols were ALWAYS content-keyed → stable. This
    // passed before the keystone fix too; it proves the bug is confined to the
    // interning layer (and that the fix keeps symbols byte-identical).
    assert_eq!(
        symbol_ab, symbol_ba,
        "codegen symbol of generated `Eq::eq` must be stable under derive reordering\n\
         order AB: {symbol_ab}\norder BA: {symbol_ba}"
    );

    // INTERNING leg: this is the keystone. RED before the fix
    // (`ImplTrait(ord:0)` vs `ImplTrait(ord:1)`), GREEN after
    // (`GeneratedImplTrait(goal:..,self:Alpha)` identical).
    assert_eq!(
        interning_ab, interning_ba,
        "interning identity of generated `impl Eq for Alpha` must be stable under \
         derive reordering\norder AB: {interning_ab}\norder BA: {interning_ba}"
    );

    // The generated impl must actually be on the content key, not the ordinal.
    assert!(
        interning_ab.contains("GeneratedImplTrait"),
        "generated impl must use the content key, got: {interning_ab}"
    );
}

#[test]
fn handwritten_impl_keeps_positional_ordinal() {
    // A hand-written `impl Trait for Ty` stays on the positional `ImplTrait`
    // ordinal (it is already reorder-stable via `HirOrigin::raw`). Guards
    // against accidentally migrating `item.rs`'s source-impl lowering onto the
    // generated content key.
    let mut db = HirAnalysisTestDb::default();
    let src = r#"
pub trait Marker {}
pub struct S {}
impl Marker for S {}
"#;
    let file = db.new_stand_alone(Utf8PathBuf::from("handwritten.fe"), src);
    let top_mod = map_file_to_mod(&db, file);

    let marker_impl = *top_mod
        .all_impl_traits(&db)
        .first()
        .expect("hand-written impl must exist");
    let repr = marker_impl.interning_identity_repr(&db);
    assert!(
        repr.contains("ImplTrait(ord:") && !repr.contains("GeneratedImplTrait"),
        "hand-written impl must stay on the positional ordinal, got: {repr}"
    );
}
