//! Type mapping from Fe MIR to Sonatina IR types.

use sonatina_ir::Type;

/// Returns the Sonatina type for an EVM word (256 bits).
pub fn word_type() -> Type {
    Type::I256
}

/// Returns the unit type (for functions returning nothing).
pub fn unit_type() -> Type {
    Type::Unit
}
