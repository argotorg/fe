use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::hir_def::ItemKind;
use url::Url;

const NATIVE_PAYLOADS: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Score { const fn score() -> usize }\npub enum Shape { Rect { x: u256, y: u256, visible: bool }, Empty, Pair(u64, bool) }\n#[permit_root]\nconst ROOT: PermitAuthority<Score<Shape>> = PermitAuthority {}\nmod author {\n use super::Score\n use core::derive::Derive\n use core::meta::{Record, Code, Item, variant, variant_count, field, field_count, field_type, type_is}\n enum Shape { Wrong }\n const ROOT: usize = 999\n const fn preserve(_ n: usize) -> usize { n }\n pub struct Provider {}\n impl Derive<Score> for Provider {\n  const fn derive(_ r: Record) -> Code<Item> {\n   let rect = variant(r, 0)\n   let empty = variant(r, 1)\n   let pair = variant(r, 2)\n   let counts = field_count(rect) * 100 + field_count(empty) * 10 + field_count(pair)\n   let kinds = type_is<u256>(field_type(field(rect, 0))) && type_is<bool>(field_type(field(rect, 2))) && !type_is<u256>(field_type(field(rect, 2))) && type_is<u64>(field_type(field(pair, 0))) && type_is<bool>(field_type(field(pair, 1)))\n   let answer = if kinds && variant_count(r) == 3 { counts } else { 9999 }\n   quote { const fn score() -> usize { preserve(${answer}) } }\n  }\n }\n}\n#[permit(ROOT)]\nimpl Score for Shape with (author::Provider)\npub enum Hand { Separate }\n#[permit_root]\nconst HAND_ROOT: PermitAuthority<Score<Hand>> = PermitAuthority {}\n#[permit(HAND_ROOT)]\nimpl Score for Hand { const fn score() -> usize { 302 } }\n#[test]\nfn independent_enum_payload_scalar_and_handwritten_twin() {\n assert(Shape::score() == 302)\n assert(Shape::score() == Hand::score())\n assert(Shape::score() + Shape::score() == 604)\n}\n";

const MISSING_AUTHORITY: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Score { const fn score() -> usize }\npub enum Shape { Rect { x: u256, y: u256, visible: bool }, Empty, Pair(u64, bool) }\n#[permit_root]\nconst ROOT: PermitAuthority<Score<Shape>> = PermitAuthority {}\nmod author {\n use super::Score\n use core::derive::Derive\n use core::meta::{Record, Code, Item, variant, variant_count, field, field_count, field_type, type_is}\n enum Shape { Wrong }\n const ROOT: usize = 999\n const fn preserve(_ n: usize) -> usize { n }\n pub struct Provider {}\n impl Derive<Score> for Provider {\n  const fn derive(_ r: Record) -> Code<Item> {\n   let zero: usize = 0\n   let fault = 1 / zero\n   let rect = variant(r, 0)\n   let empty = variant(r, 1)\n   let pair = variant(r, 2)\n   let counts = field_count(rect) * 100 + field_count(empty) * 10 + field_count(pair)\n   let kinds = type_is<u256>(field_type(field(rect, 0))) && type_is<bool>(field_type(field(rect, 2))) && !type_is<u256>(field_type(field(rect, 2))) && type_is<u64>(field_type(field(pair, 0))) && type_is<bool>(field_type(field(pair, 1)))\n   let answer = if kinds && variant_count(r) == 3 { counts } else { 9999 }\n   quote { const fn score() -> usize { preserve(${answer}) } }\n  }\n }\n}\n// missing permit\nimpl Score for Shape with (author::Provider)\npub enum Hand { Separate }\n#[permit_root]\nconst HAND_ROOT: PermitAuthority<Score<Hand>> = PermitAuthority {}\n#[permit(HAND_ROOT)]\nimpl Score for Hand { const fn score() -> usize { 302 } }\n#[test]\nfn independent_enum_payload_scalar_and_handwritten_twin() {\n assert(Shape::score() == 302)\n assert(Shape::score() == Hand::score())\n assert(Shape::score() + Shape::score() == 604)\n}\n";

#[test]
fn concrete_enum_request_publishes_into_original_header_once() {
    let source = NATIVE_PAYLOADS;
    let mut input = DriverDataBase::default();
    let file = input.workspace().touch(
        &mut input,
        Url::parse("file:///tmp/native-enum-payloads.fe").unwrap(),
        Some(source.into()),
    );
    let output = prepare_file(&input, file).unwrap().unwrap();
    assert!(input.generation_inputs().functions(&input).is_empty());
    assert!(input.generation_inputs().requests(&input).is_empty());
    let plans = output.generation_inputs().requests(&output);
    assert_eq!(plans.len(), 1);
    assert!(plans[0].request_impl.is_some());
    let output_file = output
        .workspace()
        .get(&output, &file.url(&input).unwrap())
        .unwrap();
    assert_eq!(output_file.text(&output).as_str(), source);
    let method = output
        .top_mod(output_file)
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "score")
                && f.scope().data(&output).publication_parent.is_some()
        })
        .copied()
        .unwrap();
    let Some(parent) = method.scope().parent(&output) else {
        panic!("missing authored home")
    };
    assert!(matches!(parent.to_item(), Some(ItemKind::ImplTrait(_))));
    assert!(
        prepare_file(&output, output_file)
            .err()
            .expect("second preparation must refuse")
            .cause
            .contains("authored input view")
    );
}

#[test]
fn refused_enum_authority_cannot_evaluate_fault_or_publish() {
    let source = MISSING_AUTHORITY;
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/native-enum-refused.fe").unwrap(),
        Some(source.into()),
    );
    let error = prepare_file(&db, file)
        .err()
        .expect("authority must refuse");
    assert!(
        error.cause.contains("exactly one root authority"),
        "{error}"
    );
    assert!(!error.cause.contains("division by zero"));
    assert!(db.generation_inputs().functions(&db).is_empty());
}
