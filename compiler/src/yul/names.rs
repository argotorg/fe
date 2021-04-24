use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, Integer};
use yultsur::*;

/// Generate a function name to perform checked addition
pub fn checked_add(size: &Integer) -> yul::Identifier {
    let size: &str = size.into();
    identifier! {(format!("checked_add_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked division
pub fn checked_div(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.into()
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_div_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked modulo
pub fn checked_mod(size: &Integer) -> yul::Identifier {
    let sign: &str = if size.is_signed() {
        "signed"
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_mod_{}", sign.to_lowercase()))}
}

/// Generate a function name to perform checked exponentiation
pub fn checked_exp(size: &Integer) -> yul::Identifier {
    let size: &str = size.into();
    identifier! {(format!("checked_exp_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked multiplication
pub fn checked_mul(size: &Integer) -> yul::Identifier {
    let size: &str = size.into();
    identifier! {(format!("checked_mul_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked subtraction
pub fn checked_sub(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.into()
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_sub_{}", size.to_lowercase()))}
}

/// Generate a safe function name for a user defined function
pub fn func_name(name: &str) -> yul::Identifier {
    identifier! { (format!("$${}", name)) }
}

/// Generate a safe variable name for a user defined function
pub fn var_name(name: &str) -> yul::Identifier {
    identifier! { (format!("${}", name)) }
}

/// Generates an ABI encoding function name for a given set of types.
pub fn encode_name<T: AbiEncoding>(types: &[T]) -> yul::Identifier {
    let mut full_name = "abi_encode".to_string();

    for typ in types {
        full_name.push('_');
        full_name.push_str(&typ.abi_safe_name());
    }

    identifier! { (full_name) }
}

/// Generates an ABI decoding function name for a given type and location.
pub fn decode_name<T: AbiEncoding>(typ: &T, location: AbiDecodeLocation) -> yul::Identifier {
    let mut full_name = "abi_decode".to_string();
    let loc = match location {
        AbiDecodeLocation::Memory => "mem",
        AbiDecodeLocation::Calldata => "calldata",
    };
    full_name.push('_');
    full_name.push_str(&typ.abi_safe_name());
    full_name.push('_');
    full_name.push_str(loc);

    identifier! { (full_name) }
}

/// Generates an external call function name for a given type and location.
pub fn contract_call(contract_name: &str, func_name: &str) -> yul::Identifier {
    let name = format!("{}_{}", contract_name, func_name);
    identifier! { (name) }
}

/// Generates a function name for to interact with a certain struct type
pub fn struct_function_name(struct_name: &str, func_name: &str) -> yul::Identifier {
    let name = format!("struct_{}_{}", struct_name, func_name);
    identifier! { (name) }
}

/// Generates a function name for creating a certain struct type
pub fn struct_new_call(struct_name: &str) -> yul::Identifier {
    struct_function_name(struct_name, "new")
}

/// Generates a function name for reading a named property of a certain struct
/// type
pub fn struct_getter_call(struct_name: &str, field_name: &str) -> yul::Identifier {
    struct_function_name(struct_name, &format!("get_{}_ptr", field_name))
}

#[cfg(test)]
mod tests {
    use crate::yul::names::{decode_name, encode_name};
    use fe_analyzer::namespace::types::{
        AbiDecodeLocation, Array, Base, FeString, FixedSize, U256,
    };

    #[test]
    fn test_encode_name() {
        assert_eq!(
            encode_name(&vec![
                FixedSize::Base(U256),
                FixedSize::Array(Array {
                    inner: Base::Byte,
                    size: 100
                })
            ])
            .to_string(),
            "abi_encode_uint256_bytes100"
        )
    }

    #[test]
    fn test_decode_name_u256_calldata() {
        assert_eq!(
            decode_name(&U256, AbiDecodeLocation::Calldata).to_string(),
            "abi_decode_uint256_calldata"
        )
    }

    #[test]
    fn test_decode_name() {
        assert_eq!(
            decode_name(&FeString { max_size: 42 }, AbiDecodeLocation::Memory).to_string(),
            "abi_decode_string42_mem"
        )
    }
}
