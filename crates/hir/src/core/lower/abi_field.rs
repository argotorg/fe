/// ABI field contexts that share field-type validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiFieldContext {
    Event,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiFieldDiagnosticKind {
    /// A tuple, or a type that is not a path.
    Unsupported,
    /// A type without a Solidity type name for the signature.
    MissingSolCompat,
}

/// Diagnostics for unsupported field types in ABI-bearing structs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiFieldDiagnostic {
    pub kind: AbiFieldDiagnosticKind,
    pub context: AbiFieldContext,
    pub ty: String,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
    pub field_name: Option<String>,
}
