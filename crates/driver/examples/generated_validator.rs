//! Run with `cargo run --release -p fe-driver --example generated_validator`.
//! All source stages are held in memory; the example writes no generated files.

use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{
        FunctionTemplate, GeneratedFunction, GenerationBudget, GenerationError, GenerationRequest,
        GenerationSession,
        imports::{BoundFunction, FrozenArtifact, FrozenFunction, ImportError, bind_function},
    },
};
use hir::{
    analysis::{semantic::eval_body_owner_const_with_args, ty::ty_check::BodyOwner},
    hir_def::Func,
};
use url::Url;

const PROVIDER: &str = r#"
use core::meta::FunctionBody
const fn schema() -> FunctionBody<(u256, u256)> {
    let lower: u256 = 2 * 5
    let width: u256 = 10
    FunctionBody { value: (lower, lower + width) }
}
"#;

const PACKAGE: &str = r#"
const fn bounds() -> (u256, u256) {}
pub const fn within(value: u256) -> bool {
    let limits = ingot::bounds()
    value >= limits.0 && value <= limits.1
}
"#;

const CONSUMER: &str = r#"
pub const fn validate(value: u256) -> bool {}
const fn below() -> bool { validate(value: 9) }
const fn lower() -> bool { validate(value: 10) }
const fn inside() -> bool { validate(value: 15) }
const fn upper() -> bool { validate(value: 20) }
const fn above() -> bool { validate(value: 21) }
"#;

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

fn provider(db: &mut DriverDataBase, source: &str) -> File {
    db.workspace().update(
        db,
        "file:///validator/schema.fe".parse().unwrap(),
        source.to_owned(),
    )
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
        .expect("example contains its named function")
}

fn template(url: &str, source: &str, name: &str) -> FunctionTemplate {
    FunctionTemplate {
        url: Url::parse(url).unwrap(),
        source: source.to_owned(),
        function_name: name.to_owned(),
    }
}

fn generate(
    db: &DriverDataBase,
    file: File,
    package: &str,
) -> Result<GeneratedFunction, GenerationError> {
    GenerationSession::new(
        "validator-schema".to_owned(),
        GenerationBudget::new(1, 65536),
    )
    .generate_value_function(
        db,
        named(db, file, "schema"),
        GenerationRequest {
            key: "bounds".to_owned(),
            template: template("file:///validator/package.fe", package, "bounds"),
        },
    )
}

fn bind(target: &FrozenFunction, consumer: &str) -> Result<BoundFunction, ImportError> {
    bind_function(
        template("file:///validator/consumer.fe", consumer, "validate"),
        target,
        &[0],
        &mut GenerationBudget::new(1, 65536),
    )
}

fn evaluate(bound: &BoundFunction, name: &str) -> String {
    let db = bound.database();
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, bound.file(), name)),
        Vec::new(),
        Vec::new(),
    )
    .expect("checked example caller evaluates")
    .pretty_print(db)
}

#[cfg(not(test))]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut db = database();
    let file = provider(&mut db, PROVIDER);
    let frozen = FrozenArtifact::new(generate(&db, file, PACKAGE)?);
    let selected = frozen.export("within")?;
    let validator = bind(&selected, CONSUMER)?;
    for name in ["below", "lower", "inside", "upper", "above"] {
        println!("{name}: {}", evaluate(&validator, name));
    }
    Ok(())
}

#[cfg(test)]
#[path = "../tests/support/generated_validator_checks.rs"]
mod tests;
