//! Shared utility helpers used across the Yul emitter modules.

use driver::DriverDataBase;
use hir::hir_def::{Func, HirIngot, item::ItemKind, scope_graph::ScopeId};

pub(super) fn prefix_yul_name(name: &str) -> String {
    if name.starts_with('$') {
        name.to_string()
    } else {
        format!("${name}")
    }
}

pub(super) fn is_std_evm_ops(db: &DriverDataBase, func: Func<'_>) -> bool {
    if func.body(db).is_some() {
        return false;
    }

    let ingot = func.top_mod(db).ingot(db);
    let root_mod = ingot.root_mod(db);

    let mut path = Vec::new();
    let mut scope = func.scope();
    while let Some(parent) = scope.parent_module(db) {
        match parent {
            ScopeId::Item(ItemKind::Mod(mod_)) => {
                if let Some(name) = mod_.name(db).to_opt() {
                    path.push(name.data(db).to_string());
                }
            }
            ScopeId::Item(ItemKind::TopMod(top_mod)) => {
                if top_mod != root_mod {
                    path.push(top_mod.name(db).data(db).to_string());
                }
            }
            _ => {}
        }
        scope = parent;
    }
    path.reverse();

    path.last().is_some_and(|seg| seg == "ops")
        && path.iter().rev().nth(1).is_some_and(|seg| seg == "evm")
}

/// Returns the display name of a function or `<anonymous>` if one does not exist.
///
/// * `func` - HIR function to name.
///
/// Returns the display string used for diagnostics and Yul names.
pub(super) fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}
