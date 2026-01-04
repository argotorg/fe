//! Shared utility helpers used across the Yul emitter modules.

use driver::DriverDataBase;
use hir::hir_def::Func;

use super::YulError;

/// Yul reserved words that cannot be used as identifiers.
///
/// This includes all Yul opcodes and built-in functions from the EVM dialect.
const YUL_RESERVED: &[&str] = &[
    // Arithmetic
    "add",
    "sub",
    "mul",
    "div",
    "sdiv",
    "mod",
    "smod",
    "addmod",
    "mulmod",
    "exp",
    "signextend",
    // Comparison
    "lt",
    "gt",
    "slt",
    "sgt",
    "eq",
    "iszero",
    // Bitwise
    "and",
    "or",
    "xor",
    "not",
    "byte",
    "shl",
    "shr",
    "sar",
    // Hashing
    "keccak256",
    // Memory/Storage
    "pop",
    "mload",
    "mstore",
    "mstore8",
    "sload",
    "sstore",
    "msize",
    "tload",
    "tstore",
    "mcopy",
    // Environment
    "gas",
    "address",
    "balance",
    "selfbalance",
    "caller",
    "callvalue",
    "calldataload",
    "calldatasize",
    "calldatacopy",
    "codesize",
    "codecopy",
    "extcodesize",
    "extcodecopy",
    "extcodehash",
    "returndatasize",
    "returndatacopy",
    // Contract creation/call
    "create",
    "create2",
    "call",
    "callcode",
    "delegatecall",
    "staticcall",
    "return",
    "revert",
    "selfdestruct",
    "invalid",
    "stop",
    // Block
    "blockhash",
    "coinbase",
    "timestamp",
    "number",
    "difficulty",
    "prevrandao",
    "gaslimit",
    "chainid",
    "basefee",
    "blobbasefee",
    "origin",
    "gasprice",
    "blobhash",
    // Logging
    "log0",
    "log1",
    "log2",
    "log3",
    "log4",
    // Data access
    "datasize",
    "dataoffset",
    "datacopy",
    // Misc
    "verbatim",
    "linkersymbol",
    "memoryguard",
    // Keywords
    "true",
    "false",
    "leave",
    "for",
    "if",
    "let",
    "switch",
    "case",
    "default",
    "function",
    "break",
    "continue",
];

/// Escapes a name if it conflicts with a Yul reserved word.
///
/// Adds an underscore prefix to names that would conflict with Yul builtins.
///
/// * `name` - Raw identifier to sanitize.
///
/// Returns the escaped name when reserved, otherwise the original name.
pub(super) fn escape_yul_reserved(name: &str) -> String {
    if YUL_RESERVED.contains(&name) {
        format!("_{name}")
    } else {
        name.to_string()
    }
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

/// Converts usages of cast shims into their lone argument so we don't emit fake calls.
///
/// * `name` - Function identifier for the shim.
/// * `args` - Already-lowered argument expressions.
///
/// Returns the collapsed argument when `name` is a shim, otherwise `None`.
pub(super) fn try_collapse_cast_shim(
    name: &str,
    args: &[String],
) -> Result<Option<String>, YulError> {
    if !is_cast_shim(name) {
        return Ok(None);
    }
    debug_assert_eq!(
        args.len(),
        1,
        "cast shims are expected to take a single argument"
    );
    let arg = args
        .first()
        .cloned()
        .ok_or_else(|| YulError::Unsupported("cast shim missing argument".into()))?;
    Ok(Some(arg))
}

/// Returns `true` when `name` matches one of the temporary casting shims
/// (`__{src}_as_{dst}`) used while the `as` syntax is unavailable.
fn is_cast_shim(name: &str) -> bool {
    cast_shim_parts(name).is_some()
}

/// Validates that a name follows the `__{src}_as_{dst}` convention and returns the parts.
fn cast_shim_parts(name: &str) -> Option<(&str, &str)> {
    let stripped = name.strip_prefix("__")?;
    let (src, dst) = stripped.split_once("_as_")?;
    if src.is_empty() || dst.is_empty() {
        return None;
    }
    if !is_cast_ident(src) || !is_cast_ident(dst) {
        return None;
    }
    Some((src, dst))
}

/// Validates that a substring of a shim name matches the allowed identifier schema.
fn is_cast_ident(part: &str) -> bool {
    part.chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit())
}
