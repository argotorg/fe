//! Module-level Yul emission helpers (functions + runtime wrappers).

use driver::DriverDataBase;
use hir::hir_def::{Contract, TopLevelMod};
use mir::lower_module;

use crate::yul::doc::{YulDoc, render_docs};

use super::{EmitModuleError, function::FunctionEmitter};

/// Emits Yul for every function in the lowered MIR module.
///
/// * `db` - Driver database used to query compiler facts.
/// * `top_mod` - Root module to lower.
///
/// Returns a single Yul string containing all lowered functions followed by any
/// auto-generated contract runtimes, or [`EmitModuleError`] if MIR lowering or
/// Yul emission fails.
pub fn emit_module_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, EmitModuleError> {
    let module = lower_module(db, top_mod).map_err(EmitModuleError::MirLower)?;
    let mut function_docs = Vec::new();
    for func in &module.functions {
        let emitter = FunctionEmitter::new(db, func).map_err(EmitModuleError::Yul)?;
        function_docs.extend(emitter.emit_doc().map_err(EmitModuleError::Yul)?);
    }
    let mut docs = Vec::new();
    let mut contract_docs = emit_contract_runtimes(db, top_mod, &function_docs);
    if contract_docs.is_empty() {
        docs = function_docs;
    } else {
        for mut contract_doc in contract_docs.drain(..) {
            docs.append(&mut contract_doc);
        }
    }
    let mut lines = Vec::new();
    render_docs(&docs, 0, &mut lines);
    Ok(join_lines(lines))
}

/// Builds Yul doc trees for every contract found in `top_mod`.
///
/// * `db` - Compiler database providing HIR access.
/// * `top_mod` - Module containing potential contract definitions.
///
/// Returns a vector of doc lists, one per contract, ready to append to the
/// module output.
fn emit_contract_runtimes(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    functions: &[YulDoc],
) -> Vec<Vec<YulDoc>> {
    top_mod
        .all_contracts(db)
        .iter()
        .copied()
        .filter_map(|contract| contract_runtime_object(db, contract, functions))
        .collect()
}

/// Converts a HIR contract into the doc tree describing its constructor/runtime object.
///
/// * `db` - Compiler database used to resolve the contract name.
/// * `contract` - Target contract to wrap.
///
/// Returns the constructed doc tree or `None` when the contract lacks a resolvable name.
fn contract_runtime_object(
    db: &DriverDataBase,
    contract: Contract<'_>,
    functions: &[YulDoc],
) -> Option<Vec<YulDoc>> {
    let name = contract
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())?;
    Some(render_contract_runtime_docs(&name, functions))
}

/// Creates a Yul doc tree describing the dispatcher-based runtime wrapper for `name`.
///
/// * `name` - Contract identifier used for the Yul `object`.
///
/// Returns the doc list containing constructor and runtime sections.
fn render_contract_runtime_docs(name: &str, functions: &[YulDoc]) -> Vec<YulDoc> {
    let mut runtime_body = Vec::new();
    if !functions.is_empty() {
        runtime_body.extend(functions.iter().cloned());
        runtime_body.push(YulDoc::line(String::new()));
    }
    // TODO: This is just temporary until we have a real dispatcher implementation.
    runtime_body.push(YulDoc::line("pop(dispatch())"));
    runtime_body.push(YulDoc::line("stop()"));
    vec![YulDoc::block(
        format!("object \"{name}\" "),
        vec![
            YulDoc::block(
                "code ",
                vec![
                    YulDoc::line("datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))"),
                    YulDoc::line("return(0, datasize(\"runtime\"))"),
                ],
            ),
            YulDoc::line(String::new()),
            YulDoc::block(
                "object \"runtime\" ",
                vec![YulDoc::block("code ", runtime_body)],
            ),
        ],
    )]
}

/// Joins rendered lines while trimming trailing whitespace-only entries.
///
/// * `lines` - Vector of rendered Yul lines.
///
/// Returns the normalized Yul output string.
fn join_lines(mut lines: Vec<String>) -> String {
    while lines.last().is_some_and(|line| line.is_empty()) {
        lines.pop();
    }
    lines.join("\n")
}
