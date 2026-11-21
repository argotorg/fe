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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ScopedDeriveMode {
    Ref,
    Mut,
}

/// Candidate availability requested by the ordinary evidence engine. Provider
/// identity is retained by the checked opening in the current database view.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ScopedDeriveDemand {
    pub mode: ScopedDeriveMode,
    pub opening: ScopedDeriveOpeningKey,
    pub trait_ref: FrozenTraitRef,
    /// The non-subject arguments of the goal in declaration order.
    pub trait_args: Vec<FrozenGenericArg>,
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
    /// The record's actual arguments, or `None` for the record over its own
    /// parameters, which a generated item then declares.
    pub args: Option<Vec<FrozenGenericArg>>,
}

/// An authored type declaration, by its file and syntax range.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenDeclRef {
    pub file: Url,
    pub range: TextRange,
}

/// A type of the authored view by declaration identity: the actual type and
/// constant arguments of a reflected record or field, ground except for the
/// parameters of the request's own header, rebound by the same declarations
/// in the output view.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenTy {
    /// An authored struct or enum applied to its arguments.
    Declared {
        decl: FrozenDeclRef,
        args: Vec<FrozenGenericArg>,
    },
    /// A builtin type by its name, such as `u8` or `String`, applied to its
    /// arguments.
    Primitive {
        name: String,
        args: Vec<FrozenGenericArg>,
    },
    Tuple(Vec<FrozenTy>),
    Array {
        elem: Box<FrozenTy>,
        len: FrozenConst,
    },
    Ptr(Box<FrozenTy>),
    Never,
    /// A type parameter of the request's own authored header, by position.
    HeaderParam(usize),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenGenericArg {
    Type(FrozenTy),
    Const(FrozenConst),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrozenConst {
    Value(FrozenScalar),
    /// A const parameter of the request's own authored header, by position.
    HeaderParam(usize),
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
    /// A field's declared type, over the record's own parameters.
    FieldType(FrozenFieldRef),
    /// Ordered field types as written on a record over its own parameters.
    FieldTypes {
        record: FrozenRecordRef,
        count: usize,
    },
    /// A ground type: the instantiated type of a field of a record with
    /// actual arguments.
    Type(FrozenTy),
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
    /// A value of `record`, a struct or a selected variant, built from one
    /// initializer per field in declaration order. Lowering writes the
    /// record's own constructor: `R { .. }`, `R(..)`, `E::V { .. }`,
    /// `E::V(..)` or `E::V`.
    Construct {
        record: FrozenRecordRef,
        fields: Vec<FrozenCode>,
    },
    /// An ordered, unit-valued block of checked expressions.
    FieldStatements {
        fields: Vec<FrozenCode>,
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
    /// The slot of a short form (`#[abi]`, `#[error]`) whose attribute is at
    /// `request_impl`; `None` for a written header.
    pub request_slot: Option<u8>,
    pub code: FrozenCode,
}

#[salsa::input]
#[derive(Debug)]
pub struct GenerationInputs {
    pub functions: Vec<FrozenFunctionTemplate>,
    pub requests: Vec<FrozenGenerationRequest>,
    /// Whether an explicit derive header, `impl Trait for T with (Provider)`,
    /// counts as the fact that the implementation exists although preparation
    /// has not produced its members. The input view of a preparation sets it;
    /// the authored view and the prepared view leave it off.
    pub declared_headers: bool,
}
