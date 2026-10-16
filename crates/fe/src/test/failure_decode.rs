//! Bounded decoding of error payloads. Never interpret a selector as provenance.
use contract_harness::execution_trace::CapturedBytes;
use ethers_core::abi::{ErrorExt, ParamType, ethabi};

pub(super) fn decode_custom_error(
    payload: &CapturedBytes,
    definitions: &[serde_json::Value],
) -> Option<String> {
    if payload.truncated() || payload.original_len > 16384 || definitions.len() > 1024 {
        return None;
    }
    if payload.hex.len() != payload.original_len.checked_mul(2)? {
        return None;
    }
    let bytes = hex::decode(&payload.hex).ok()?;
    let selector = bytes.get(..4)?;
    if selector == [0x08, 0xc3, 0x79, 0xa0] || selector == [0x4e, 0x48, 0x7b, 0x71] {
        return None;
    }
    let mut matches = std::collections::BTreeMap::new();
    for definition in definitions {
        let Ok(error) = serde_json::from_value::<ethabi::AbiError>(definition.clone()) else {
            continue;
        };
        if error.selector() == selector {
            matches.insert(error.abi_signature(), error);
        }
    }
    if matches.len() != 1 {
        return None;
    }
    let error = matches.into_values().next()?;
    let types: Vec<_> = error.inputs.iter().map(|param| &param.kind).collect();
    let data = &bytes[4..];
    let mut budget = 256;
    validate_sequence(&types, data, 0, &mut budget, 0)?;
    let values = error.decode(data).ok()?;
    // Require a canonical encoding: reject overlapping tails, padding, trailing
    // bytes and offsets into unrelated values even if the ABI library accepts them.
    if ethabi::encode(&values) != data {
        return None;
    }
    let fields = error
        .inputs
        .iter()
        .zip(values)
        .map(|(param, value)| {
            let value = format_token(&value);
            if param.name.is_empty() {
                value
            } else {
                format!("{}={value}", param.name)
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    Some(format!("{}({fields})", error.name))
}

fn format_token(token: &ethabi::Token) -> String {
    use ethabi::Token;
    match token {
        Token::Uint(value) => value.to_string(),
        Token::Int(value) => ethers_core::types::I256::from_raw(*value).to_string(),
        Token::Address(value) => format!("{value:#x}"),
        Token::Bool(value) => value.to_string(),
        Token::String(value) => format!("{value:?}"),
        Token::Bytes(value) | Token::FixedBytes(value) => format!("0x{}", hex::encode(value)),
        Token::Array(values) | Token::FixedArray(values) => format!(
            "[{}]",
            values
                .iter()
                .map(format_token)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Token::Tuple(values) => format!(
            "({})",
            values
                .iter()
                .map(format_token)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn word(data: &[u8], start: usize) -> Option<usize> {
    let word = data.get(start..start.checked_add(32)?)?;
    if word[..24].iter().any(|byte| *byte != 0) {
        return None;
    }
    usize::try_from(u64::from_be_bytes(word[24..].try_into().ok()?)).ok()
}

fn head_size(ty: &ParamType, depth: usize) -> Option<usize> {
    if depth > 16 {
        return None;
    }
    if ty.is_dynamic() {
        return Some(32);
    }
    match ty {
        ParamType::FixedArray(child, count) if *count <= 256 => {
            head_size(child, depth + 1)?.checked_mul(*count)
        }
        ParamType::Tuple(fields) if fields.len() <= 256 => {
            fields.iter().try_fold(0usize, |size, field| {
                size.checked_add(head_size(field, depth + 1)?)
            })
        }
        ParamType::FixedArray(..) | ParamType::Tuple(..) => None,
        _ => Some(32),
    }
}

fn validate_sequence(
    types: &[&ParamType],
    data: &[u8],
    base: usize,
    budget: &mut usize,
    depth: usize,
) -> Option<()> {
    if depth > 16 || types.len() > *budget {
        return None;
    }
    let size = types
        .iter()
        .try_fold(0usize, |size, ty| size.checked_add(head_size(ty, depth)?))?;
    data.get(base..base.checked_add(size)?)?;
    let mut head = base;
    for ty in types {
        let start = if ty.is_dynamic() {
            let offset = word(data, head)?;
            if offset < size || offset % 32 != 0 {
                return None;
            }
            base.checked_add(offset)?
        } else {
            head
        };
        validate_value(ty, data, start, budget, depth + 1)?;
        head = head.checked_add(head_size(ty, depth)?)?;
    }
    Some(())
}

fn validate_value(
    ty: &ParamType,
    data: &[u8],
    start: usize,
    budget: &mut usize,
    depth: usize,
) -> Option<()> {
    if depth > 16 {
        return None;
    }
    *budget = budget.checked_sub(1)?;
    match ty {
        ParamType::Bytes | ParamType::String => {
            let len = word(data, start)?;
            let begin = start.checked_add(32)?;
            let value = data.get(begin..begin.checked_add(len)?)?;
            if matches!(ty, ParamType::String) {
                std::str::from_utf8(value).ok()?;
            }
            let padded = len.checked_add(31)? / 32 * 32;
            let padding = data.get(begin.checked_add(len)?..begin.checked_add(padded)?)?;
            if padding.iter().any(|byte| *byte != 0) {
                return None;
            }
        }
        ParamType::Array(child) => {
            let count = word(data, start)?;
            if count > *budget {
                return None;
            }
            validate_sequence(
                &vec![child.as_ref(); count],
                data,
                start.checked_add(32)?,
                budget,
                depth,
            )?;
        }
        ParamType::FixedArray(child, count) => {
            if *count > *budget {
                return None;
            }
            validate_sequence(&vec![child.as_ref(); *count], data, start, budget, depth)?;
        }
        ParamType::Tuple(fields) => {
            if fields.len() > *budget {
                return None;
            }
            validate_sequence(
                &fields.iter().collect::<Vec<_>>(),
                data,
                start,
                budget,
                depth,
            )?;
        }
        ParamType::Uint(bits) | ParamType::Int(bits) => {
            if *bits == 0 || *bits > 256 || *bits % 8 != 0 {
                return None;
            }
            let value = data.get(start..start.checked_add(32)?)?;
            let prefix = 32 - bits / 8;
            let sign = if matches!(ty, ParamType::Int(_)) && value[prefix] & 0x80 != 0 {
                0xff
            } else {
                0
            };
            if value[..prefix].iter().any(|byte| *byte != sign) {
                return None;
            }
        }
        ParamType::Address => {
            let value = data.get(start..start.checked_add(32)?)?;
            if value[..12].iter().any(|byte| *byte != 0) {
                return None;
            }
        }
        ParamType::Bool => {
            if word(data, start)? > 1 {
                return None;
            }
        }
        ParamType::FixedBytes(count) => {
            if *count == 0 || *count > 32 {
                return None;
            }
            let value = data.get(start..start.checked_add(32)?)?;
            if value[*count..].iter().any(|byte| *byte != 0) {
                return None;
            }
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ethers_core::abi::Token;

    fn payload(bytes: Vec<u8>) -> CapturedBytes {
        CapturedBytes {
            original_len: bytes.len(),
            hex: hex::encode(bytes),
        }
    }

    #[test]
    fn wrapped_error_bytes_are_decoded_without_claiming_a_cause() {
        let definition =
            serde_json::json!({"name":"Wrapped", "inputs":[{"name":"reason","type":"bytes"}]});
        let error: ethabi::AbiError = serde_json::from_value(definition.clone()).unwrap();
        let encoded = error.encode(&[Token::Bytes(vec![0xde, 0xad])]).unwrap();
        assert_eq!(
            decode_custom_error(&payload(encoded.clone()), std::slice::from_ref(&definition))
                .as_deref(),
            Some("Wrapped(reason=0xdead)")
        );
        for len in 0..encoded.len() {
            assert!(
                decode_custom_error(
                    &payload(encoded[..len].to_vec()),
                    std::slice::from_ref(&definition)
                )
                .is_none()
            );
        }
    }

    #[test]
    fn decoded_values_use_decimal_numbers_and_escaped_strings() {
        let definition = serde_json::json!({"name":"Values", "inputs":[
            {"name":"amount","type":"uint256"}, {"name":"delta","type":"int256"},
            {"name":"message","type":"string"}, {"name":"bytes","type":"bytes"}
        ]});
        let error: ethabi::AbiError = serde_json::from_value(definition.clone()).unwrap();
        let encoded = error
            .encode(&[
                Token::Uint(100.into()),
                Token::Int(ethers_core::types::I256::from(-2).into_raw()),
                Token::String("line\nnext".into()),
                Token::Bytes(vec![0xde, 0xad]),
            ])
            .unwrap();
        assert_eq!(
            decode_custom_error(&payload(encoded), &[definition]).as_deref(),
            Some("Values(amount=100, delta=-2, message=\"line\\nnext\", bytes=0xdead)")
        );
    }

    #[test]
    fn selector_collisions_and_unbounded_array_lengths_remain_raw() {
        let a = serde_json::json!({"name":"burn","inputs":[{"name":"x","type":"uint256"}]});
        let b = serde_json::json!({"name":"collate_propagate_storage","inputs":[{"name":"x","type":"bytes16"}]});
        let first: ethabi::AbiError = serde_json::from_value(a.clone()).unwrap();
        let second: ethabi::AbiError = serde_json::from_value(b.clone()).unwrap();
        assert_eq!(first.selector(), second.selector());
        let encoded = first.encode(&[Token::Uint(1.into())]).unwrap();
        assert!(decode_custom_error(&payload(encoded), &[a, b]).is_none());
        let array = serde_json::json!({"name":"ArrayError","inputs":[{"name":"values","type":"uint256[]"}]});
        let error: ethabi::AbiError = serde_json::from_value(array.clone()).unwrap();
        let mut encoded = error.encode(&[Token::Array(vec![])]).unwrap();
        encoded[36..68].fill(0xff);
        assert!(decode_custom_error(&payload(encoded), &[array]).is_none());
    }
}
