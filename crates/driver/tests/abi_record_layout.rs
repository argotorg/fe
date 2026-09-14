use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::DriverDataBase;
use hir::{
    analysis::semantic::eval_body_owner_const_with_args, analysis::ty::ty_check::BodyOwner,
    hir_def::Func,
};
use url::Url;

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    db.workspace().touch(
        db,
        Url::parse(&format!("file:///abi-record-layout/{name}.fe")).unwrap(),
        Some(source.to_owned()),
    )
}

fn diagnostics(db: &DriverDataBase, file: File) -> String {
    let ingot = db.top_mod(file).ingot(db);
    let hir = db.run_on_ingot(ingot);
    let mut errors = if hir.has_errors(db) {
        hir.format_diags(db)
    } else {
        String::new()
    };
    let semantic = db.mir_diagnostics_for_ingot(ingot);
    if semantic
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        errors.push_str(&db.format_complete_diagnostics(&semantic));
    }
    errors
}

fn checked(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    let file = input(db, name, source);
    let errors = diagnostics(db, file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    file
}

fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    db.top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn evaluate(db: &DriverDataBase, file: File, name: &str) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, name)),
        Vec::new(),
        Vec::new(),
    )
    .unwrap_or_else(|error| panic!("failed to evaluate `{name}`: {error:?}"))
    .pretty_print(db)
}

fn assert_values(db: &DriverDataBase, file: File, expected: &[(&str, &str)]) {
    for &(name, value) in expected {
        assert_eq!(evaluate(db, file, name), value, "constant `{name}`");
    }
}

#[test]
fn generated_scalar_and_empty_message_layouts_match_abi_metadata() {
    let mut db = database();
    let file = checked(
        &mut db,
        "scalar-empty",
        r#"
msg Records {
    #[selector = 1]
    Scalars { first: u256, second: bool, third: u256 } -> bool,
    #[selector = 2]
    Empty -> bool,
}

#[error]
pub struct Failure { pub code: u256, pub flag: bool }

const fn scalar_offset0() -> u256 { <Records::Scalars as core::abi::AbiRecord<3>>::LAYOUT.offsets[0] }
const fn scalar_offset1() -> u256 { <Records::Scalars as core::abi::AbiRecord<3>>::LAYOUT.offsets[1] }
const fn scalar_offset2() -> u256 { <Records::Scalars as core::abi::AbiRecord<3>>::LAYOUT.offsets[2] }
const fn scalar_head() -> u256 { <Records::Scalars as core::abi::AbiRecord<3>>::LAYOUT.head_size }
const fn scalar_metadata_head() -> u256 { <Records::Scalars as core::abi::AbiSize>::HEAD_SIZE }
const fn scalar_metadata_dynamic() -> bool { <Records::Scalars as core::abi::AbiSize>::IS_DYNAMIC }
const fn empty_head() -> u256 { <Records::Empty as core::abi::AbiRecord<0>>::LAYOUT.head_size }
const fn empty_metadata_dynamic() -> bool { <Records::Empty as core::abi::AbiSize>::IS_DYNAMIC }
const fn error_offset1() -> u256 { <Failure as core::abi::AbiRecord<2>>::LAYOUT.offsets[1] }
const fn error_head() -> u256 { <Failure as core::abi::AbiRecord<2>>::LAYOUT.head_size }
const fn error_metadata_head() -> u256 { <Failure as core::abi::AbiSize>::HEAD_SIZE }
const fn error_metadata_dynamic() -> bool { <Failure as core::abi::AbiSize>::IS_DYNAMIC }
"#,
    );
    assert_values(
        &db,
        file,
        &[
            ("scalar_offset0", "0"),
            ("scalar_offset1", "32"),
            ("scalar_offset2", "64"),
            ("scalar_head", "96"),
            ("scalar_metadata_head", "96"),
            ("scalar_metadata_dynamic", "false"),
            ("empty_head", "0"),
            ("empty_metadata_dynamic", "false"),
            ("error_offset1", "32"),
            ("error_head", "64"),
            ("error_metadata_head", "64"),
            ("error_metadata_dynamic", "false"),
        ],
    );
}

#[test]
fn generated_static_arrays_and_dynamic_bytes_use_field_head_contributions() {
    let mut db = database();
    let file = checked(
        &mut db,
        "static-dynamic",
        r#"
use core::abi::Bytes

msg Records {
    #[selector = 1]
    Static { flag: bool, words: [u256; 2], last: bool } -> bool,
    #[selector = 2]
    Dynamic { first: u256, data: Bytes, last: bool } -> bool,
}

const fn static_offset1() -> u256 { <Records::Static as core::abi::AbiRecord<3>>::LAYOUT.offsets[1] }
const fn static_offset2() -> u256 { <Records::Static as core::abi::AbiRecord<3>>::LAYOUT.offsets[2] }
const fn static_head() -> u256 { <Records::Static as core::abi::AbiRecord<3>>::LAYOUT.head_size }
const fn dynamic_offset1() -> u256 { <Records::Dynamic as core::abi::AbiRecord<3>>::LAYOUT.offsets[1] }
const fn dynamic_offset2() -> u256 { <Records::Dynamic as core::abi::AbiRecord<3>>::LAYOUT.offsets[2] }
const fn dynamic_head() -> u256 { <Records::Dynamic as core::abi::AbiRecord<3>>::LAYOUT.head_size }
const fn dynamic_metadata_head() -> u256 { <Records::Dynamic as core::abi::AbiSize>::HEAD_SIZE }
const fn dynamic_metadata() -> bool { <Records::Dynamic as core::abi::AbiSize>::IS_DYNAMIC }
"#,
    );
    assert_values(
        &db,
        file,
        &[
            ("static_offset1", "32"),
            ("static_offset2", "96"),
            ("static_head", "128"),
            ("dynamic_offset1", "32"),
            ("dynamic_offset2", "64"),
            ("dynamic_head", "96"),
            ("dynamic_metadata_head", "96"),
            ("dynamic_metadata", "true"),
        ],
    );
}

#[test]
fn direct_planner_preserves_custom_static_sizes_and_normalizes_dynamic_heads() {
    let mut db = database();
    let file = checked(
        &mut db,
        "custom",
        r#"
use core::abi::{AbiRecordLayout, AbiSize, abi_field_head_size, abi_record_layout}

struct One {}
impl AbiSize for One {
    const HEAD_SIZE: u256 = 1
    const IS_DYNAMIC: bool = false
}
struct Seven {}
impl AbiSize for Seven {
    const HEAD_SIZE: u256 = 7
    const IS_DYNAMIC: bool = false
}
struct DynamicHuge {}
impl AbiSize for DynamicHuge {
    const HEAD_SIZE: u256 = 999
    const IS_DYNAMIC: bool = true
}

const LAYOUT: AbiRecordLayout<4> = abi_record_layout([
    abi_field_head_size<One>(),
    abi_field_head_size<Seven>(),
    abi_field_head_size<DynamicHuge>(),
    abi_field_head_size<One>(),
])
const fn offset1() -> u256 { LAYOUT.offsets[1] }
const fn offset2() -> u256 { LAYOUT.offsets[2] }
const fn offset3() -> u256 { LAYOUT.offsets[3] }
const fn head() -> u256 { LAYOUT.head_size }
"#,
    );
    assert_values(
        &db,
        file,
        &[
            ("offset1", "1"),
            ("offset2", "8"),
            ("offset3", "40"),
            ("head", "41"),
        ],
    );
}

#[test]
fn field_head_size_can_depend_on_the_enclosing_record_dynamic_metadata() {
    let mut db = database();
    let file = checked(
        &mut db,
        "metadata-dependency",
        r#"
struct RecursiveHead {}

msg CycleMsg {
    #[selector = 1]
    Probe { value: RecursiveHead } -> bool,
}

impl core::abi::AbiSize for RecursiveHead {
    const HEAD_SIZE: u256 = if <CycleMsg::Probe as core::abi::AbiSize>::IS_DYNAMIC { 32 } else { 7 }
    const IS_DYNAMIC: bool = false
}

impl core::abi::Encode<std::abi::Sol> for RecursiveHead {
    fn encode(own self, _ ptr: *u8) {}
}

impl core::abi::Decode<std::abi::Sol> for RecursiveHead {
    fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>>(_ decoder: mut D) -> Self {
        RecursiveHead {}
    }
}

const fn message_dynamic() -> bool {
    <CycleMsg::Probe as core::abi::AbiSize>::IS_DYNAMIC
}

const fn message_head() -> u256 {
    <CycleMsg::Probe as core::abi::AbiSize>::HEAD_SIZE
}
"#,
    );
    assert_values(
        &db,
        file,
        &[("message_dynamic", "false"), ("message_head", "7")],
    );
}

#[test]
fn ordinary_const_checking_rejects_a_wrong_descriptor_field_type() {
    let mut db = database();
    let file = input(
        &mut db,
        "wrong-field-type",
        r#"
use core::abi::AbiRecordLayout
const BAD: AbiRecordLayout<1> = AbiRecordLayout {
    offsets: [0],
    head_size: false,
}
"#,
    );
    let errors = diagnostics(&db, file);
    assert!(errors.contains("error[8-0000]"), "{errors}");
    assert!(
        errors.contains("u256") && errors.contains("bool"),
        "{errors}"
    );
}

#[test]
fn checked_prefix_sum_rejects_an_invalid_sum() {
    let mut db = database();
    let file = input(
        &mut db,
        "invalid-sum",
        r#"
use core::abi::{AbiRecordLayout, abi_record_layout}
const LAYOUT: AbiRecordLayout<2> = abi_record_layout([
    115792089237316195423570985008687907853269984665640564039457584007913129639935,
    1,
])
const TOTAL: u256 = LAYOUT.head_size
struct Require<const N: u256> {}
fn force(_ value: Require<TOTAL>) {}
"#,
    );
    let errors = diagnostics(&db, file);
    assert!(!errors.is_empty(), "invalid layout sum was accepted");
    assert!(
        errors.contains("overflow"),
        "expected checked prefix-sum failure:\n{errors}"
    );
}
