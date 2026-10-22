//! Immutable output-view inputs. No semantic context or proof authority lives
//! here: source identity uses existing file URLs and authored syntax ranges.

use parser::TextRange;
use url::Url;

/// One authored lexical provider operand. Ranges belong to unchanged source
/// text in a preparation transaction; semantic IDs never cross database views.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ScopedDeriveOpeningKey {
    pub file: Url,
    pub binding_range: TextRange,
}

/// An exact authored trait declaration, never a path to resolve by spelling.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenTraitRef {
    pub file: Url,
    pub range: TextRange,
}

/// Candidate availability requested by the ordinary evidence engine. Provider
/// identity is retained by the checked opening in the current database view.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ScopedDeriveDemand {
    pub opening: ScopedDeriveOpeningKey,
    pub trait_ref: FrozenTraitRef,
    pub target: FrozenRecordRef,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenScalar {
    Bool(bool),
    Int {
        negative: bool,
        bytes: Vec<u8>,
        primitive: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenFunctionTemplate {
    pub target_file: Url,
    pub quote_range: TextRange,
    pub producer_module: Option<TextRange>,
    pub producer_block: Option<TextRange>,
    pub holes: Vec<FrozenScalar>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FrozenCodeKind {
    Item,
    Expr,
    Pat,
    Arm,
    Where,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FrozenRecordOwnerKind {
    Struct,
    Enum,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenRecordRef {
    pub owner_kind: FrozenRecordOwnerKind,
    pub target_file: Url,
    pub target_range: TextRange,
    pub variant_index: Option<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenFieldRef {
    pub record: FrozenRecordRef,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenHoleValue {
    Scalar(FrozenScalar),
    Record(FrozenRecordRef),
    Field(FrozenFieldRef),
    FieldType(FrozenFieldRef),
    Code(FrozenCode),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenCode {
    Template {
        kind: FrozenCodeKind,
        quote_file: Url,
        quote_range: TextRange,
        producer_module: Option<TextRange>,
        producer_block: Option<TextRange>,
        holes: Vec<FrozenHoleValue>,
    },
    Empty {
        kind: FrozenCodeKind,
    },
    VariantBinders {
        record: FrozenRecordRef,
        group: usize,
    },
    VariantBinderField {
        record: FrozenRecordRef,
        group: usize,
        field_index: usize,
    },
}

/// A checked scoped output binds one lexical opening and ground goal to a
/// compiler-private named implementation in this immutable output view.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenScopedImpl {
    pub demand: ScopedDeriveDemand,
    pub selection_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenGenerationRequest {
    pub target_file: Url,
    pub scoped_impl: Option<FrozenScopedImpl>,
    /// Authored derive header receiving this generated method.
    pub request_impl: Option<TextRange>,
    pub code: FrozenCode,
}

#[salsa::input]
#[derive(Debug)]
pub struct GenerationInputs {
    pub functions: Vec<FrozenFunctionTemplate>,
    pub requests: Vec<FrozenGenerationRequest>,
}
