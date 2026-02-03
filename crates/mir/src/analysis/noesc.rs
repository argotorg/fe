//! MIR validation for NoEsc (stack-only) values.
//!
//! NoEsc values (e.g. `mut T` / `ref T`) may live in locals/temps, but must not be stored into
//! non-memory address spaces such as storage.

use hir::analysis::{HirAnalysisDb, ty::ty_is_noesc};

use crate::{
    MirFunction, MirInst, ValueId,
    ir::{AddressSpaceKind, Place},
};

pub fn check_noesc_escapes<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
) -> Option<String> {
    for block in &func.body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Store { place, value, .. } => {
                    if let Some(err) = check_store(db, func, place, *value) {
                        return Some(err);
                    }
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    for (_, value) in inits {
                        if let Some(err) = check_store(db, func, place, *value) {
                            return Some(err);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    None
}

fn check_store<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    place: &Place<'db>,
    value: ValueId,
) -> Option<String> {
    let space = func.body.place_address_space(place);
    if matches!(space, AddressSpaceKind::Memory) {
        return None;
    }

    let ty = func.body.value(value).ty;
    if !ty_is_noesc(db, ty) {
        return None;
    }

    Some(format!(
        "cannot store NoEsc value `{}` into `{space:?}`",
        ty.pretty_print(db)
    ))
}
