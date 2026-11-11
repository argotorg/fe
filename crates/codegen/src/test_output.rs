use driver::DriverDataBase;
use hir::{
    HirDb,
    analysis::ty::ty_check::BodyOwner,
    hir_def::{ItemKind, LitKind, attr::AttrArgValue},
};
use num_bigint::BigUint;

use crate::LowerError;

/// A test of an EVM test suite.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestMetadata {
    pub name: String,
    /// The calldata selector that runs the test.
    pub selector: u32,
    pub expected_revert: Option<ExpectedRevert>,
    pub initial_balance: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedRevert {
    Any,
    Selector([u8; 4]),
    PanicCode(Vec<u8>),
}

/// The selected tests of an EVM test run, compiled into one object. Its
/// runtime code runs the test whose selector is the calldata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestSuiteOutput {
    /// The init code that deploys the object.
    pub bytecode: Vec<u8>,
    pub sonatina_observability_json: Option<String>,
    pub tests: Vec<TestMetadata>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TestRootMetadata {
    pub name: String,
    pub expected_revert: Option<ExpectedRevert>,
    pub initial_balance: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TestRootMetadataError {
    InvalidPackage(String),
    Unsupported(String),
}

impl From<TestRootMetadataError> for LowerError {
    fn from(err: TestRootMetadataError) -> Self {
        match err {
            TestRootMetadataError::InvalidPackage(message) => LowerError::Internal(message),
            TestRootMetadataError::Unsupported(message) => LowerError::Unsupported(message),
        }
    }
}

/// The name and `#[test]` attribute arguments of the test that `owner`, a
/// test root, runs.
pub(crate) fn runtime_test_root_metadata<'db>(
    db: &'db DriverDataBase,
    owner: &mir::RuntimeFunctionOwner<'db>,
) -> Result<TestRootMetadata, TestRootMetadataError> {
    let mir::RuntimeFunctionOwner::Synthetic(mir::RuntimeSyntheticSpec::TestRoot {
        name,
        callee,
        ..
    }) = owner
    else {
        return Err(TestRootMetadataError::InvalidPackage(
            "a test is not run by a TestRoot wrapper".to_string(),
        ));
    };
    let Some(semantic) = callee.key(db).semantic(db) else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "test `{name}` does not target a semantic instance"
        )));
    };
    let BodyOwner::Func(func, _) = semantic.key(db).owner(db) else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "test `{name}` does not target a function body"
        )));
    };
    let attrs = ItemKind::from(func).attrs(db).ok_or_else(|| {
        TestRootMetadataError::InvalidPackage(format!("test function `{name}` has no attrs"))
    })?;
    let test_attr = attrs.get_attr(db, "test").ok_or_else(|| {
        TestRootMetadataError::InvalidPackage(format!("test function `{name}` is missing #[test]"))
    })?;
    let expected_revert =
        parse_expected_revert(db, name, test_attr).map_err(TestRootMetadataError::Unsupported)?;
    let initial_balance = parse_test_balance_arg(db, name, test_attr)?;
    Ok(TestRootMetadata {
        name: name.clone(),
        expected_revert,
        initial_balance,
    })
}

fn parse_test_balance_arg<'db>(
    db: &'db dyn HirDb,
    test_name: &str,
    test_attr: &hir::hir_def::attr::NormalAttr<'db>,
) -> Result<Option<Vec<u8>>, TestRootMetadataError> {
    let balance = parse_test_attr_int_arg(
        db,
        test_name,
        test_attr,
        "balance",
        "balance = ...",
        "u256",
        32,
    )
    .map_err(TestRootMetadataError::Unsupported)?;
    Ok(balance.map(|value| value.to_bytes_be()))
}

pub fn parse_expected_revert<'db>(
    db: &'db dyn HirDb,
    test_name: &str,
    test_attr: &hir::hir_def::attr::NormalAttr<'db>,
) -> Result<Option<ExpectedRevert>, String> {
    let should_revert = test_attr.has_arg(db, "should_revert");
    let has_panic = has_test_attr_key(db, test_attr, "panic");
    let has_selector = has_test_attr_key(db, test_attr, "selector");

    if !should_revert {
        if has_panic && has_selector {
            return Err(format!(
                "invalid #[test] function `{test_name}`: `panic = ...` and `selector = ...` require `should_revert`"
            ));
        }
        if has_panic {
            return Err(format!(
                "invalid #[test] function `{test_name}`: `panic = ...` requires `should_revert`"
            ));
        }
        if has_selector {
            return Err(format!(
                "invalid #[test] function `{test_name}`: `selector = ...` requires `should_revert`"
            ));
        }
        return Ok(None);
    }

    let panic = parse_test_attr_int_arg(
        db,
        test_name,
        test_attr,
        "panic",
        "should_revert, panic = ...",
        "u256",
        32,
    )?;
    let selector = parse_test_attr_int_arg(
        db,
        test_name,
        test_attr,
        "selector",
        "should_revert, selector = ...",
        "u32",
        4,
    )?;

    if panic.is_some() && selector.is_some() {
        return Err(format!(
            "invalid #[test] function `{test_name}`: #[test(should_revert)] cannot combine `panic = ...` and `selector = ...`"
        ));
    }

    if let Some(code) = panic {
        let mut payload = Vec::with_capacity(36);
        payload.extend_from_slice(&[0x4e, 0x48, 0x7b, 0x71]);
        let code_bytes = code.to_bytes_be();
        let mut padded = [0u8; 32];
        padded[32 - code_bytes.len()..].copy_from_slice(&code_bytes);
        payload.extend_from_slice(&padded);
        return Ok(Some(ExpectedRevert::PanicCode(payload)));
    }

    if let Some(sel) = selector {
        let bytes = sel.to_bytes_be();
        let mut selector = [0u8; 4];
        selector[4 - bytes.len()..].copy_from_slice(&bytes);
        return Ok(Some(ExpectedRevert::Selector(selector)));
    }

    Ok(Some(ExpectedRevert::Any))
}

fn has_test_attr_key<'db>(
    db: &'db dyn HirDb,
    test_attr: &hir::hir_def::attr::NormalAttr<'db>,
    key: &str,
) -> bool {
    test_attr
        .args
        .iter()
        .any(|arg| arg.key_str(db) == Some(key))
}

fn parse_test_attr_int_arg<'db>(
    db: &'db dyn HirDb,
    test_name: &str,
    test_attr: &hir::hir_def::attr::NormalAttr<'db>,
    key: &str,
    attr_form: &str,
    type_name: &str,
    max_bytes: usize,
) -> Result<Option<BigUint>, String> {
    for arg in &test_attr.args {
        if arg.key_str(db) != Some(key) {
            continue;
        }
        let Some(value) = arg.value.as_ref() else {
            return Err(format!(
                "invalid #[test] function `{test_name}`: #[test({attr_form})] expects an integer literal"
            ));
        };
        let AttrArgValue::Lit(LitKind::Int(int_id)) = value else {
            return Err(format!(
                "invalid #[test] function `{test_name}`: #[test({attr_form})] expects an integer literal"
            ));
        };
        let value = int_id.data(db).clone();
        if value.to_bytes_be().len() > max_bytes {
            return Err(format!(
                "invalid #[test] function `{test_name}`: #[test({attr_form})] must fit in {type_name}"
            ));
        }
        return Ok(Some(value));
    }

    Ok(None)
}
