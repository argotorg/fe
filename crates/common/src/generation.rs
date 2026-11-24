//! Immutable output-view inputs. No semantic context or proof authority lives
//! here: source identity uses existing file URLs and authored syntax ranges.

use parser::TextRange;
use rustc_hash::FxHashMap;
use url::Url;

use crate::{InputDb, file::File};

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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FrozenGenerationRequest {
    pub target_file: Url,
    /// Authored derive header receiving this generated method.
    pub request_impl: Option<TextRange>,
    pub code: FrozenCode,
}

#[salsa::input]
#[derive(Debug)]
pub struct GenerationInputs {
    #[returns(ref)]
    pub functions: Vec<FrozenFunctionTemplate>,
    #[returns(ref)]
    pub requests: Vec<FrozenGenerationRequest>,
    /// Whether an explicit derive header, `impl Trait for T with (Provider)`,
    /// counts as the fact that the implementation exists although preparation
    /// has not produced its members. The input view of a preparation sets it;
    /// the authored view and the prepared view leave it off.
    pub declared_headers: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FileGeneration {
    pub functions: Vec<FrozenFunctionTemplate>,
    pub items: Vec<FrozenGenerationRequest>,
    pub members: FxHashMap<TextRange, FrozenGenerationRequest>,
}

#[salsa::tracked(returns(ref))]
fn generation_by_file(
    db: &dyn InputDb,
    inputs: GenerationInputs,
) -> FxHashMap<Url, FileGeneration> {
    let mut files = FxHashMap::<Url, FileGeneration>::default();
    for plan in inputs.functions(db) {
        files
            .entry(plan.target_file.clone())
            .or_default()
            .functions
            .push(plan.clone());
    }
    for plan in inputs.requests(db) {
        let output = files.entry(plan.target_file.clone()).or_default();
        if let Some(range) = plan.request_impl {
            output.members.entry(range).or_insert_with(|| plan.clone());
        } else {
            output.items.push(plan.clone());
        }
    }
    files
}

/// Backdate an unchanged file's output when another file is prepared.
#[salsa::tracked(returns(ref))]
pub fn file_generation(db: &dyn InputDb, file: File) -> Option<FileGeneration> {
    let url = file.url(db)?;
    generation_by_file(db, db.generation_inputs())
        .get(&url)
        .cloned()
}

#[cfg(test)]
mod tests {
    use salsa::Setter;

    use super::*;
    use crate::define_input_db;

    define_input_db!(TestDatabase);

    #[test]
    fn file_generation_tracks_removal_and_distinguishes_equal_ranges() {
        let mut db = TestDatabase::default();
        let first_url = Url::parse("file:///first.fe").unwrap();
        let second_url = Url::parse("file:///second.fe").unwrap();
        let workspace = db.workspace();
        let first = workspace.touch(&mut db, first_url.clone(), None);
        let second = workspace.touch(&mut db, second_url.clone(), None);
        let range = TextRange::empty(1.into());
        let plan = |target_file, request_impl, kind| FrozenGenerationRequest {
            target_file,
            request_impl,
            code: FrozenCode::Empty { kind },
        };
        let first_members = plan(first_url, Some(range), FrozenCodeKind::Item);
        let second_members = plan(second_url.clone(), Some(range), FrozenCodeKind::Expr);
        let second_item = plan(second_url, None, FrozenCodeKind::Item);
        let inputs = db.generation_inputs();
        inputs.set_requests(&mut db).to(vec![
            first_members.clone(),
            second_members.clone(),
            second_item.clone(),
        ]);

        assert_eq!(
            file_generation(&db, first).as_ref().unwrap().members[&range],
            first_members
        );
        let output = file_generation(&db, second).as_ref().unwrap();
        assert_eq!(output.members[&range], second_members);
        assert_eq!(output.items, [second_item]);

        inputs.set_requests(&mut db).to(vec![first_members.clone()]);
        assert!(file_generation(&db, second).is_none());
        assert_eq!(
            file_generation(&db, first).as_ref().unwrap().members[&range],
            first_members
        );
    }
}
