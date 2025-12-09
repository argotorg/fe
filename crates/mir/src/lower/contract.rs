//! Contract attribute helpers for MIR lowering: identifies init/runtime functions and extracts
//! contract metadata needed by codegen.

use super::*;

/// Extracts contract init/runtime metadata from function attributes.
///
/// # Parameters
/// - `db`: HIR analysis database.
/// - `func`: Function to inspect for contract attributes.
///
/// # Returns
/// Contract metadata when present, otherwise `None`.
pub(crate) fn extract_contract_function(
    db: &dyn HirAnalysisDb,
    func: Func<'_>,
) -> Option<ContractFunction> {
    let attrs = ItemKind::Func(func).attrs(db)?;
    for attr in attrs.data(db) {
        let Attr::Normal(normal) = attr else {
            continue;
        };
        let Some(path) = normal.path.to_opt() else {
            continue;
        };
        let Some(name) = path.as_ident(db) else {
            continue;
        };
        let kind = match name.data(db).as_str() {
            "contract_init" => ContractFunctionKind::Init,
            "contract_runtime" => ContractFunctionKind::Runtime,
            _ => continue,
        };
        let Some(contract_name) = contract_name_from_attr_args(db, &normal.args) else {
            continue;
        };
        return Some(ContractFunction {
            contract_name,
            kind,
        });
    }
    None
}

/// Extracts the contract name from attribute arguments.
///
/// # Parameters
/// - `db`: HIR analysis database.
/// - `args`: Attribute arguments to inspect.
///
/// # Returns
/// Contract name if one can be resolved.
fn contract_name_from_attr_args(db: &dyn HirAnalysisDb, args: &[AttrArg<'_>]) -> Option<String> {
    args.first().and_then(|arg| {
        arg.key
            .to_opt()
            .and_then(|path| path.as_ident(db))
            .map(|id| id.data(db).to_string())
            .or_else(|| match arg.value.clone().to_opt()? {
                AttrArgValue::Ident(id) => Some(id.data(db).to_string()),
                _ => None,
            })
    })
}
