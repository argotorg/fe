//! Layout snapshot tests: verify type sizes and field offsets.
//!
//! These tests ensure layout computation is consistent and correct.
//! The snapshots serve as golden values that catch layout drift.

use std::path::Path;

use dir_test::{Fixture, dir_test};
use hir::hir_def::{Enum, Struct};
use hir::test_db::HirAnalysisTestDb;
use mir::layout;
use test_utils::snap_test;

/// Generates a layout report for all ADTs in a module.
fn generate_layout_report<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: hir::hir_def::TopLevelMod<'db>,
) -> String {
    let mut report = String::new();

    // Collect structs
    for &strct in top_mod.all_structs(db) {
        report.push_str(&format_struct_layout(db, strct));
        report.push('\n');
    }

    // Collect enums
    for &enm in top_mod.all_enums(db) {
        report.push_str(&format_enum_layout(db, enm));
        report.push('\n');
    }

    report
}

fn format_struct_layout<'db>(db: &'db HirAnalysisTestDb, strct: Struct<'db>) -> String {
    let name = strct
        .name(db)
        .to_opt()
        .map(|n| n.data(db).to_string())
        .unwrap_or_else(|| "<anon>".to_string());

    let mut lines = vec![format!("struct {name}:")];

    // Get field types via semantic API
    let field_tys = strct.field_tys(db);

    // Compute total size
    let mut total_size = 0;
    for field_ty_binder in &field_tys {
        let field_ty = *field_ty_binder.skip_binder();
        total_size += layout::ty_size_bytes(db, field_ty).expect("field size known");
    }
    lines.push(format!("  size: {total_size} bytes"));

    lines.push("  fields:".to_string());

    let mut offset = 0;
    for (idx, field_ty_binder) in field_tys.iter().enumerate() {
        let field_ty = *field_ty_binder.skip_binder();
        let field_size = layout::ty_size_bytes(db, field_ty).expect("field size known");

        lines.push(format!("    [{idx}]: offset={offset}, size={field_size}"));
        offset += field_size;
    }

    lines.join("\n")
}

fn format_enum_layout<'db>(db: &'db HirAnalysisTestDb, enm: Enum<'db>) -> String {
    let name = enm
        .name(db)
        .to_opt()
        .map(|n| n.data(db).to_string())
        .unwrap_or_else(|| "<anon>".to_string());

    let adt_def = enm.as_adt(db);

    let mut lines = vec![format!("enum {name}:")];

    // Compute enum size (discriminant + max payload)
    let mut max_payload_size = 0;
    for adt_field in adt_def.fields(db).iter() {
        let mut payload_size = 0;
        for field_ty_binder in adt_field.iter_types(db) {
            let field_ty = *field_ty_binder.skip_binder();
            payload_size += layout::ty_size_bytes(db, field_ty).expect("variant field size known");
        }
        max_payload_size = max_payload_size.max(payload_size);
    }

    let total_size = layout::DISCRIMINANT_SIZE_BYTES + max_payload_size;
    lines.push(format!("  size: {total_size} bytes"));
    lines.push(format!(
        "  discriminant: {} bytes",
        layout::DISCRIMINANT_SIZE_BYTES
    ));

    lines.push("  variants:".to_string());

    // Get variant names from HIR
    let variants: Vec<_> = enm.variants(db).collect();

    for (idx, (variant, adt_field)) in variants.iter().zip(adt_def.fields(db).iter()).enumerate() {
        let variant_name = variant
            .name(db)
            .map(|n| n.data(db).to_string())
            .unwrap_or_else(|| format!("{idx}"));

        if adt_field.num_types() == 0 {
            lines.push(format!("    {variant_name}: (unit)"));
        } else {
            lines.push(format!("    {variant_name}:"));
            let mut field_offset = 0;
            for (field_idx, field_ty_binder) in adt_field.iter_types(db).enumerate() {
                let field_ty = *field_ty_binder.skip_binder();
                let field_size =
                    layout::ty_size_bytes(db, field_ty).expect("variant field size known");

                // Absolute offset = discriminant + payload offset
                let abs_offset = layout::DISCRIMINANT_SIZE_BYTES + field_offset;
                lines.push(format!(
                    "      [{field_idx}]: offset={abs_offset}, size={field_size}"
                ));
                field_offset += field_size;
            }
        }
    }

    lines.join("\n")
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/layout",
    glob: "*.fe"
)]
fn layout_snapshot(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _prop_formatter) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let report = generate_layout_report(&db, top_mod);
    snap_test!(report, fixture.path());
}
