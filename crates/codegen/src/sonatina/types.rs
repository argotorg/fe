//! Type mapping from Fe MIR to Sonatina IR types.

use mir::ValueRepr;
use sonatina_ir::Type;

/// Convert a MIR ValueRepr to a Sonatina Type.
///
/// For EVM:
/// - Word values are I256 (EVM word size)
/// - Pointers are I256 (memory addresses / storage slots)
/// - References are I256 (same as pointers for EVM)
#[allow(dead_code)]
pub fn lower_value_repr(repr: &ValueRepr) -> Type {
    match repr {
        ValueRepr::Word => Type::I256,
        ValueRepr::Ptr(_) => Type::I256, // Memory/storage addresses are words
        ValueRepr::Ref(_) => Type::I256, // References are also words on EVM
    }
}

/// Returns the Sonatina type for a boolean value.
#[allow(dead_code)]
pub fn bool_type() -> Type {
    Type::I1
}

/// Returns the Sonatina type for an EVM word (256 bits).
pub fn word_type() -> Type {
    Type::I256
}

/// Returns the Sonatina type for a memory pointer.
#[allow(dead_code)]
pub fn mem_ptr_type() -> Type {
    Type::I256
}

/// Returns the Sonatina type for a storage slot.
#[allow(dead_code)]
pub fn storage_slot_type() -> Type {
    Type::I256
}

/// Returns the unit type (for functions returning nothing).
pub fn unit_type() -> Type {
    Type::Unit
}
