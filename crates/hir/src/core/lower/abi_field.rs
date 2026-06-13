/// ABI field contexts that share field-type validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiFieldContext {
    Event,
    Error,
}

/// Diagnostics for unsupported field types in ABI-bearing structs.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiFieldDiagnostic {
    pub context: AbiFieldContext,
    pub ty: String,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
    pub field_name: Option<String>,
}
