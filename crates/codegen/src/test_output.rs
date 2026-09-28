use driver::DriverDataBase;
use hir::{
    HirDb,
    analysis::ty::ty_check::BodyOwner,
    hir_def::{ItemKind, LitKind, attr::AttrArgValue},
};
use num_bigint::BigUint;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestMetadata {
    pub display_name: String,
    pub hir_name: String,
    pub symbol_name: String,
    pub object_name: String,
    pub bytecode: Vec<u8>,
    pub sonatina_observability_json: Option<String>,
    pub debug_info: Option<std::sync::Arc<TestDebugInfo>>,
    pub value_param_count: usize,
    pub effect_param_count: usize,
    pub init_bytecode: Vec<u8>,
    pub expected_revert: Option<ExpectedRevert>,
    pub initial_balance: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedRevert {
    Any,
    Selector([u8; 4]),
    PanicCode(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestModuleOutput {
    pub tests: Vec<TestMetadata>,
}

impl TestModuleOutput {
    pub(crate) fn extend(&mut self, other: Self) {
        self.tests.extend(other.tests);
    }

    pub(crate) fn sort_tests(&mut self) {
        self.tests.sort_by(|left, right| {
            left.display_name
                .cmp(&right.display_name)
                .then_with(|| left.hir_name.cmp(&right.hir_name))
                .then_with(|| left.symbol_name.cmp(&right.symbol_name))
                .then_with(|| left.object_name.cmp(&right.object_name))
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TestRootMetadata {
    pub hir_name: String,
    pub display_name: String,
    pub expected_revert: Option<ExpectedRevert>,
    pub initial_balance: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TestRootMetadataError {
    InvalidPackage(String),
    Unsupported(String),
}

pub(crate) fn runtime_test_root_metadata<'db>(
    db: &'db DriverDataBase,
    owner: &mir::RuntimeFunctionOwner<'db>,
    section_name: &mir::RuntimeSectionName,
) -> Result<TestRootMetadata, TestRootMetadataError> {
    let mir::RuntimeSectionName::Test(hir_name) = section_name else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "non-test section `{section_name:?}` used as a test root"
        )));
    };
    let mir::RuntimeFunctionOwner::Synthetic(mir::RuntimeSyntheticSpec::TestRoot {
        callee, ..
    }) = owner
    else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "test section `{hir_name}` does not use a TestRoot wrapper"
        )));
    };
    let Some(semantic) = callee.key(db).semantic(db) else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "test section `{hir_name}` does not target a semantic instance"
        )));
    };
    let BodyOwner::Func(func) = semantic.key(db).owner(db) else {
        return Err(TestRootMetadataError::InvalidPackage(format!(
            "test section `{hir_name}` does not target a function body"
        )));
    };
    let attrs = ItemKind::from(func).attrs(db).ok_or_else(|| {
        TestRootMetadataError::InvalidPackage(format!("test function `{hir_name}` has no attrs"))
    })?;
    let test_attr = attrs.get_attr(db, "test").ok_or_else(|| {
        TestRootMetadataError::InvalidPackage(format!(
            "test function `{hir_name}` is missing #[test]"
        ))
    })?;
    let expected_revert = parse_expected_revert(db, hir_name, test_attr)
        .map_err(TestRootMetadataError::Unsupported)?;
    let initial_balance = parse_test_balance_arg(db, hir_name, test_attr)?;
    Ok(TestRootMetadata {
        hir_name: hir_name.clone(),
        display_name: hir_name.clone(),
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

/// Metadata captured alongside the bytecode returned by the test emitter.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct TestDebugInfo {
    pub optimization: String,
    pub compiler_flags: Vec<String>,
    pub facts: Vec<trace_facts::TraceFact>,
    pub codes: Vec<TestDebugCode>,
    /// Source snapshots keyed by the content hashes recorded in the facts.
    pub sources: std::collections::BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct TestDebugCode {
    pub owner: Option<TestDebugOwner>,
    /// Typed error ABI entries added by the test driver, separate from public ABI output.
    pub custom_errors: Vec<serde_json::Value>,
    /// Compiler-declared immutable words appended to deployed runtime code.
    pub immutable_tail_bytes: usize,
    pub name: String,
    pub deploy: Vec<u8>,
    pub runtime: Vec<u8>,
    pub create_key: common::origin::OriginExportKey,
    pub runtime_key: common::origin::OriginExportKey,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct TestDebugOwner {
    pub source_uri: String,
    pub kind: String,
    pub name: String,
}
