//! Mapping from semantic Fe types to Solidity ABI types.
//!
//! Shared by the declaration-time `msg` selector signature check
//! (`analysis::ty::msg_selector`) and JSON ABI emission in the `fe` crate.

use std::fmt;

use common::ingot::IngotKind;

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::{
    adt_def::AdtRef,
    const_ty::{ConstTyData, EvaluatedConstTy},
    ty_def::{PrimTy, TyBase, TyData, TyId},
};

/// The Solidity ABI type of a semantic Fe type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiTypeDesc {
    /// JSON ABI `type` string (`"tuple"` for structs and tuples).
    pub abi_type: String,
    /// Canonical type as it appears in a selector signature
    /// (e.g. `uint256`, `(uint256,address)`).
    pub canonical_type: String,
    /// Component types for tuples and structs.
    pub components: Option<Vec<AbiComponent>>,
}

/// A named component of a tuple/struct ABI type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiComponent {
    pub name: String,
    pub ty: String,
    pub components: Option<Vec<AbiComponent>>,
}

/// Why a semantic Fe type cannot be represented as a Solidity ABI type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbiTypeError {
    /// The type contains an invalid nominal recursion cycle.
    Recursive(String),
    /// The type has no supported Solidity ABI representation.
    Unsupported(String),
}

impl AbiTypeError {
    fn unsupported(message: impl Into<String>) -> Self {
        Self::Unsupported(message.into())
    }
}

impl fmt::Display for AbiTypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Recursive(message) | Self::Unsupported(message) => message.fmt(f),
        }
    }
}

impl AbiTypeDesc {
    fn simple(ty: &str) -> Self {
        Self {
            abi_type: ty.to_string(),
            canonical_type: ty.to_string(),
            components: None,
        }
    }

    fn tuple(components: Vec<AbiComponent>, canonical_type: String) -> Self {
        Self {
            abi_type: "tuple".to_string(),
            canonical_type,
            components: Some(components),
        }
    }

    fn array(self, len: &str) -> Self {
        Self {
            abi_type: format!("{}[{len}]", self.abi_type),
            canonical_type: format!("{}[{len}]", self.canonical_type),
            components: self.components,
        }
    }
}

fn canonical_tuple_type(component_descs: &[AbiTypeDesc]) -> String {
    let mut out = String::from("(");
    for (idx, desc) in component_descs.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        out.push_str(&desc.canonical_type);
    }
    out.push(')');
    out
}

/// Compute the Solidity ABI type of a semantic Fe type.
pub fn semantic_ty_to_abi_desc<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Result<AbiTypeDesc, AbiTypeError> {
    if let Some((_, inner)) = ty.as_capability(db) {
        return semantic_ty_to_abi_desc(db, inner);
    }

    if ty == TyId::unit(db) {
        return Err(AbiTypeError::unsupported(
            "unit type is not a valid external ABI parameter",
        ));
    }

    if ty.is_tuple(db) {
        let components = ty.field_types(db);
        let component_descs: Vec<_> = components
            .into_iter()
            .map(|field_ty| semantic_ty_to_abi_desc(db, field_ty))
            .collect::<Result<_, _>>()?;
        return Ok(AbiTypeDesc::tuple(
            component_descs
                .iter()
                .map(|desc| AbiComponent {
                    name: String::new(),
                    ty: desc.abi_type.clone(),
                    components: desc.components.clone(),
                })
                .collect(),
            canonical_tuple_type(&component_descs),
        ));
    }

    if ty.is_array(db) {
        let (_, args) = ty.decompose_ty_app(db);
        let elem_ty = args
            .first()
            .copied()
            .ok_or_else(|| AbiTypeError::unsupported("array type is missing its element type"))?;
        let len_ty = args
            .get(1)
            .copied()
            .ok_or_else(|| AbiTypeError::unsupported("array type is missing its length"))?;
        let elem_desc = semantic_ty_to_abi_desc(db, elem_ty)?;
        let len = array_len_to_string(db, len_ty)?;
        return Ok(elem_desc.array(&len));
    }

    if let Some(elem_ty) = core_dyn_array_elem_ty(db, ty) {
        let elem_desc = semantic_ty_to_abi_desc(db, elem_ty)?;
        return Ok(elem_desc.array(""));
    }

    if is_core_dyn_string_ty(db, ty) {
        return Ok(AbiTypeDesc::simple("string"));
    }

    if is_core_bytes_ty(db, ty) {
        return Ok(AbiTypeDesc::simple("bytes"));
    }

    if ty.is_string(db) {
        return Ok(AbiTypeDesc::simple("string"));
    }

    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            PrimTy::Bool => Ok(AbiTypeDesc::simple("bool")),
            PrimTy::U8 => Ok(AbiTypeDesc::simple("uint8")),
            PrimTy::U16 => Ok(AbiTypeDesc::simple("uint16")),
            PrimTy::U32 => Ok(AbiTypeDesc::simple("uint32")),
            PrimTy::U64 => Ok(AbiTypeDesc::simple("uint64")),
            PrimTy::U128 => Ok(AbiTypeDesc::simple("uint128")),
            PrimTy::U256 | PrimTy::Usize => Ok(AbiTypeDesc::simple("uint256")),
            PrimTy::I8 => Ok(AbiTypeDesc::simple("int8")),
            PrimTy::I16 => Ok(AbiTypeDesc::simple("int16")),
            PrimTy::I32 => Ok(AbiTypeDesc::simple("int32")),
            PrimTy::I64 => Ok(AbiTypeDesc::simple("int64")),
            PrimTy::I128 => Ok(AbiTypeDesc::simple("int128")),
            PrimTy::I256 | PrimTy::Isize => Ok(AbiTypeDesc::simple("int256")),
            PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) => unreachable!(),
            PrimTy::Ptr | PrimTy::View | PrimTy::BorrowMut | PrimTy::BorrowRef => {
                Err(AbiTypeError::unsupported(format!(
                    "unsupported ABI type `{}`",
                    ty.pretty_print(db)
                )))
            }
        },
        TyData::TyBase(TyBase::Adt(adt)) => {
            let adt_ref = adt.adt_ref(db);
            if is_std_address_ty(db, ty, adt_ref) {
                return Ok(AbiTypeDesc::simple("address"));
            }
            if let Some(sol_type) = std_sol_compat_abi_type(db, ty, adt_ref) {
                return Ok(AbiTypeDesc::simple(&sol_type));
            }
            match adt_ref {
                AdtRef::Struct(struct_) => {
                    if adt_ref.as_adt(db).recursive_cycle(db).is_some() {
                        return Err(AbiTypeError::Recursive(format!(
                            "recursive ABI type `{}`",
                            ty.pretty_print(db)
                        )));
                    }
                    let field_tys = ty.field_types(db);
                    let hir_fields = struct_.hir_fields(db).data(db);
                    if hir_fields.len() != field_tys.len() {
                        return Err(AbiTypeError::unsupported(format!(
                            "field count mismatch: {} HIR fields vs {} semantic fields",
                            hir_fields.len(),
                            field_tys.len()
                        )));
                    }
                    let component_descs: Vec<(String, AbiTypeDesc)> = hir_fields
                        .iter()
                        .zip(field_tys)
                        .map(|(field, field_ty)| {
                            let name = field
                                .name
                                .to_opt()
                                .map(|ident| ident.data(db).to_string())
                                .unwrap_or_default();
                            Ok((name, semantic_ty_to_abi_desc(db, field_ty)?))
                        })
                        .collect::<Result<_, AbiTypeError>>()?;
                    let canonical = canonical_tuple_type(
                        &component_descs
                            .iter()
                            .map(|(_, desc)| desc.clone())
                            .collect::<Vec<_>>(),
                    );
                    Ok(AbiTypeDesc::tuple(
                        component_descs
                            .into_iter()
                            .map(|(name, desc)| AbiComponent {
                                name,
                                ty: desc.abi_type,
                                components: desc.components,
                            })
                            .collect(),
                        canonical,
                    ))
                }
                AdtRef::Enum(_) => Err(AbiTypeError::unsupported(format!(
                    "unsupported ABI enum type `{}`",
                    ty.pretty_print(db)
                ))),
            }
        }
        TyData::Invalid(_) => Err(AbiTypeError::unsupported(format!(
            "unresolved ABI type `{}`",
            ty.pretty_print(db)
        ))),
        _ => Err(AbiTypeError::unsupported(format!(
            "unsupported ABI type `{}`",
            ty.pretty_print(db)
        ))),
    }
}

fn array_len_to_string(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Result<String, AbiTypeError> {
    match ty.data(db) {
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                Ok(value.data(db).to_string())
            }
            _ => Err(AbiTypeError::unsupported(format!(
                "array length `{}` is not a concrete integer",
                ty.pretty_print(db)
            ))),
        },
        _ => Err(AbiTypeError::unsupported(format!(
            "array length `{}` is not represented as a const type",
            ty.pretty_print(db)
        ))),
    }
}

fn is_std_address_ty(db: &dyn HirAnalysisDb, ty: TyId<'_>, adt_ref: AdtRef<'_>) -> bool {
    let Some(name) = adt_ref.name(db) else {
        return false;
    };
    if name.data(db) != "Address" {
        return false;
    }
    ty.ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Std)
}

fn core_dyn_array_elem_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    if let Some((_, inner)) = ty.as_capability(db) {
        return core_dyn_array_elem_ty(db, inner);
    }

    let (base, args) = ty.decompose_ty_app(db);
    let TyData::TyBase(TyBase::Adt(adt)) = base.data(db) else {
        return None;
    };
    let adt_ref = adt.adt_ref(db);
    let name = adt_ref.name(db)?;
    if name.data(db) != "DynArray" {
        return None;
    }
    if !base
        .ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Core)
    {
        return None;
    }

    args.first().copied()
}

fn is_core_adt_named(db: &dyn HirAnalysisDb, ty: TyId<'_>, expected_name: &str) -> bool {
    if let Some((_, inner)) = ty.as_capability(db) {
        return is_core_adt_named(db, inner, expected_name);
    }

    let base = ty.base_ty(db);
    let TyData::TyBase(TyBase::Adt(adt)) = base.data(db) else {
        return false;
    };
    let adt_ref = adt.adt_ref(db);
    let Some(name) = adt_ref.name(db) else {
        return false;
    };
    if name.data(db) != expected_name {
        return false;
    }

    base.ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Core)
}

fn is_core_bytes_ty(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    is_core_adt_named(db, ty, "Bytes")
}

fn is_core_dyn_string_ty(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    is_core_adt_named(db, ty, "DynString")
}

/// Recognise `std::abi::sol` SolCompat wrapper types like `Uint160` / `Int24`
/// and return their Solidity ABI type string (e.g. `"uint160"`, `"int24"`).
fn std_sol_compat_abi_type(
    db: &dyn HirAnalysisDb,
    ty: TyId<'_>,
    adt_ref: AdtRef<'_>,
) -> Option<String> {
    if !ty
        .ingot(db)
        .is_some_and(|ingot| ingot.kind(db) == IngotKind::Std)
    {
        return None;
    }
    let name = adt_ref.name(db)?.data(db).to_string();

    if name == "FixedBytes" {
        let (_, args) = ty.decompose_ty_app(db);
        let len_ty = args.first().copied().or_else(|| args.get(1).copied())?;
        let len = fixed_bytes_len_to_string(db, len_ty)?;
        return Some(format!("bytes{len}"));
    }

    if let Some(rest) = name.strip_prefix("Bytes") {
        let len: u16 = rest.parse().ok()?;
        if (1..=32).contains(&len) {
            return Some(format!("bytes{len}"));
        }
        return None;
    }

    // Match Uint{N} or Int{N} where N is a valid Solidity bit width (8..=256, multiple of 8)
    let (prefix, digits) = if let Some(rest) = name.strip_prefix("Uint") {
        ("uint", rest)
    } else {
        let rest = name.strip_prefix("Int")?;
        ("int", rest)
    };

    let bits: u16 = digits.parse().ok()?;
    if (8..=256).contains(&bits) && bits.is_multiple_of(8) {
        Some(format!("{prefix}{bits}"))
    } else {
        None
    }
}

fn fixed_bytes_len_to_string(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<String> {
    match ty.data(db) {
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                let len = value.data(db).to_string().parse::<u16>().ok()?;
                (1..=32).contains(&len).then(|| len.to_string())
            }
            _ => None,
        },
        _ => None,
    }
}

/// A selector signature literal such as `transfer(address,uint256)`, split
/// into its function name and canonical argument types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedFunctionSignature {
    pub name: String,
    pub arg_types: Vec<String>,
}

/// Parse a selector signature literal of the form `name(type,...)`.
pub fn parse_function_signature(signature: &str) -> Result<ParsedFunctionSignature, String> {
    let (name, args) = signature
        .split_once('(')
        .ok_or_else(|| "selector signature must be of the form `name(type,...)`".to_string())?;
    let args = args
        .strip_suffix(')')
        .ok_or_else(|| "selector signature must end with `)`".to_string())?;
    if name.is_empty() || name.trim() != name || name.chars().any(char::is_whitespace) {
        return Err("selector function name must be a single identifier".to_string());
    }

    Ok(ParsedFunctionSignature {
        name: name.to_string(),
        arg_types: split_signature_args(args)?,
    })
}

fn split_signature_args(args: &str) -> Result<Vec<String>, String> {
    if args.is_empty() {
        return Ok(Vec::new());
    }

    let mut arg_types = Vec::new();
    let mut start = 0;
    let mut tuple_depth = 0usize;
    let mut array_depth = 0usize;

    for (idx, ch) in args.char_indices() {
        match ch {
            '(' => tuple_depth += 1,
            ')' => {
                if tuple_depth == 0 {
                    return Err("unbalanced `)` in selector signature".to_string());
                }
                tuple_depth -= 1;
            }
            '[' => array_depth += 1,
            ']' => {
                if array_depth == 0 {
                    return Err("unbalanced `]` in selector signature".to_string());
                }
                array_depth -= 1;
            }
            ',' if tuple_depth == 0 && array_depth == 0 => {
                let arg = args[start..idx].trim();
                if arg.is_empty() {
                    return Err("empty selector argument type".to_string());
                }
                arg_types.push(arg.to_string());
                start = idx + 1;
            }
            _ => {}
        }
    }

    if tuple_depth != 0 || array_depth != 0 {
        return Err("unbalanced selector argument list".to_string());
    }

    let last = args[start..].trim();
    if last.is_empty() {
        return Err("empty selector argument type".to_string());
    }
    arg_types.push(last.to_string());
    Ok(arg_types)
}

/// The Fe type to suggest when a selector signature names a Solidity type
/// whose semantic counterpart mismatches a field's type.
///
/// This is the single source of suggestions for selector/field ABI type
/// mismatch diagnostics; extend it here when new ABI wrapper types are added
/// to the standard library.
pub fn suggested_fe_type_for_sol_type(sol_ty: &str) -> Option<String> {
    match sol_ty {
        "string" => return Some("std::abi::DynString".to_string()),
        "bytes" => return Some("std::abi::Bytes".to_string()),
        "address" => return Some("std::evm::Address".to_string()),
        _ => {}
    }

    if let Some(digits) = sol_ty.strip_prefix("bytes") {
        let len: u16 = digits.parse().ok()?;
        return (1..=32)
            .contains(&len)
            .then(|| format!("std::abi::Bytes{len}"));
    }

    let (native_prefix, wrapper_prefix, digits) = if let Some(rest) = sol_ty.strip_prefix("uint") {
        ("u", "Uint", rest)
    } else {
        let rest = sol_ty.strip_prefix("int")?;
        ("i", "Int", rest)
    };

    let bits: u16 = digits.parse().ok()?;
    if !(8..=256).contains(&bits) || !bits.is_multiple_of(8) {
        return None;
    }
    if matches!(bits, 8 | 16 | 32 | 64 | 128 | 256) {
        Some(format!("{native_prefix}{bits}"))
    } else {
        Some(format!("std::abi::{wrapper_prefix}{bits}"))
    }
}
