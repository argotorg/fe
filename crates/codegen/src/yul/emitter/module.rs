//! Module-level Yul emission helpers (functions + runtime wrappers).

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
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

    // Emit Yul docs for each function
    let mut function_docs: Vec<Vec<YulDoc>> = Vec::with_capacity(module.functions.len());
    for func in &module.functions {
        let emitter = FunctionEmitter::new(db, func).map_err(EmitModuleError::Yul)?;
        let docs = emitter.emit_doc().map_err(EmitModuleError::Yul)?;
        function_docs.push(docs);
    }

    let mut docs = Vec::new();

    if module.contracts.is_empty() {
        // No contracts: emit all functions as top-level
        for func_docs in function_docs {
            docs.extend(func_docs);
        }
    } else {
        // Emit each contract with its reachable functions
        for contract in &module.contracts {
            let mut contract_fn_docs = Vec::new();
            for &idx in &contract.function_indices {
                contract_fn_docs.extend(function_docs[idx].clone());
            }
            docs.extend(render_contract_runtime_docs(
                &contract.name,
                &contract_fn_docs,
            ));
        }
    }

    let mut lines = Vec::new();
    render_docs(&docs, 0, &mut lines);
    Ok(join_lines(lines))
}

/// Creates a Yul doc tree describing the dispatcher-based runtime wrapper for `name`.
///
/// * `name` - Contract identifier used for the Yul `object`.
/// * `functions` - Yul docs for all functions reachable from this contract's dispatcher.
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
