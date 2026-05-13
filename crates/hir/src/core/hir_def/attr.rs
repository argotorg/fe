use crate::HirDb;

use super::{IdentId, Partial, PathId, StringId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithmeticMode {
    Checked,
    Unchecked,
}

impl ArithmeticMode {
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "checked" => Some(Self::Checked),
            "unchecked" => Some(Self::Unchecked),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum InlineHint {
    Hint,
    Always,
    Never,
}

impl InlineHint {
    pub fn pretty_print(self) -> &'static str {
        match self {
            Self::Hint => "#[inline]",
            Self::Always => "#[inline(always)]",
            Self::Never => "#[inline(never)]",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum InlineAttrErrorKind {
    Duplicate,
    InvalidForm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum LoopUnrollAttrErrorKind {
    Duplicate,
    InvalidForm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum InlineAttr {
    Hint(InlineHint),
    Error(InlineAttrErrorKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum LoopUnrollAttr {
    Hint(bool),
    Error(LoopUnrollAttrErrorKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum ManualContractRootAttr<'db> {
    Init { contract_name: StringId<'db> },
    Runtime { contract_name: StringId<'db> },
    Error(ManualContractRootAttrError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum ManualContractRootAttrError {
    InvalidForm,
    Duplicate,
    WrongTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct KeywordAttrSpec {
    pub has_value: bool,
    pub has_args: bool,
    pub args: Vec<KeywordAttrArgSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct KeywordAttrArgSpec {
    pub key: Option<String>,
    pub has_value: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct InlineAttrParseError {
    pub kind: InlineAttrErrorKind,
    pub attr_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LoopUnrollAttrParseError {
    pub kind: LoopUnrollAttrErrorKind,
    pub attr_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ManualContractRootAttrParseError {
    pub kind: ManualContractRootAttrError,
    pub attr_index: usize,
}

pub(crate) fn parse_inline_attr_specs(
    attrs: impl IntoIterator<Item = KeywordAttrSpec>,
) -> Result<Option<InlineHint>, InlineAttrParseError> {
    let mut inline_hint = None;

    for (attr_index, attr) in attrs.into_iter().enumerate() {
        let parsed_hint = parse_inline_attr_spec(&attr)
            .map_err(|kind| InlineAttrParseError { kind, attr_index })?;

        if inline_hint.is_some() {
            return Err(InlineAttrParseError {
                kind: InlineAttrErrorKind::Duplicate,
                attr_index,
            });
        }

        inline_hint = Some(parsed_hint);
    }

    Ok(inline_hint)
}

pub(crate) fn parse_loop_unroll_attr_specs(
    attrs: impl IntoIterator<Item = KeywordAttrSpec>,
) -> Result<Option<bool>, LoopUnrollAttrParseError> {
    let mut unroll_hint = None;

    for (attr_index, attr) in attrs.into_iter().enumerate() {
        let parsed_hint = parse_keyword_attr_spec(&attr, true, &[("never", false)])
            .map_err(|()| LoopUnrollAttrErrorKind::InvalidForm)
            .map_err(|kind| LoopUnrollAttrParseError { kind, attr_index })?;

        if unroll_hint.is_some() {
            return Err(LoopUnrollAttrParseError {
                kind: LoopUnrollAttrErrorKind::Duplicate,
                attr_index,
            });
        }

        unroll_hint = Some(parsed_hint);
    }

    Ok(unroll_hint)
}

pub(crate) fn parse_manual_contract_root_attr_specs<'db>(
    db: &'db dyn HirDb,
    attrs: impl IntoIterator<Item = (&'db str, KeywordAttrSpec)>,
) -> Result<Option<ManualContractRootAttr<'db>>, ManualContractRootAttrParseError> {
    let mut root_attr = None;

    for (attr_index, (name, attr)) in attrs.into_iter().enumerate() {
        let parsed = parse_manual_contract_root_attr_spec(db, name, &attr)
            .map_err(|kind| ManualContractRootAttrParseError { kind, attr_index })?;

        if root_attr.is_some() {
            return Err(ManualContractRootAttrParseError {
                kind: ManualContractRootAttrError::Duplicate,
                attr_index,
            });
        }

        root_attr = Some(parsed);
    }

    Ok(root_attr)
}

fn parse_inline_attr_spec(attr: &KeywordAttrSpec) -> Result<InlineHint, InlineAttrErrorKind> {
    parse_keyword_attr_spec(
        attr,
        InlineHint::Hint,
        &[("always", InlineHint::Always), ("never", InlineHint::Never)],
    )
    .map_err(|()| InlineAttrErrorKind::InvalidForm)
}

fn parse_manual_contract_root_attr_spec<'db>(
    db: &'db dyn HirDb,
    name: &str,
    attr: &KeywordAttrSpec,
) -> Result<ManualContractRootAttr<'db>, ManualContractRootAttrError> {
    if attr.has_value || !attr.has_args || attr.args.len() != 1 {
        return Err(ManualContractRootAttrError::InvalidForm);
    }

    let arg = &attr.args[0];
    if arg.has_value {
        return Err(ManualContractRootAttrError::InvalidForm);
    }
    let Some(key) = arg.key.as_deref() else {
        return Err(ManualContractRootAttrError::InvalidForm);
    };
    let contract_name = StringId::new(db, key.to_string());
    match name {
        "contract_init" => Ok(ManualContractRootAttr::Init { contract_name }),
        "contract_runtime" => Ok(ManualContractRootAttr::Runtime { contract_name }),
        _ => Err(ManualContractRootAttrError::InvalidForm),
    }
}

pub(crate) fn parse_marker_attr_spec(attr: &KeywordAttrSpec) -> Result<(), ()> {
    parse_keyword_attr_spec(attr, (), &[])
}

fn parse_keyword_attr_spec<T: Copy>(
    attr: &KeywordAttrSpec,
    bare: T,
    args: &[(&str, T)],
) -> Result<T, ()> {
    if attr.has_value {
        return Err(());
    }
    if !attr.has_args {
        return Ok(bare);
    }
    if attr.args.len() != 1 {
        return Err(());
    }

    let arg = &attr.args[0];
    if arg.has_value {
        return Err(());
    }

    args.iter()
        .find_map(|(name, value)| (arg.key.as_deref() == Some(*name)).then_some(*value))
        .ok_or(())
}

#[salsa::interned]
#[derive(Debug)]
pub struct AttrListId<'db> {
    #[return_ref]
    pub data: Vec<Attr<'db>>,
}

impl<'db> AttrListId<'db> {
    /// Returns true if this attribute list contains an attribute with the given name.
    ///
    /// Only checks simple identifier attributes (e.g., `#[msg]`), not path attributes.
    pub fn has_attr(self, db: &'db dyn HirDb, name: &str) -> bool {
        self.data(db).iter().any(|attr| {
            if let Attr::Normal(normal_attr) = attr
                && let Some(path) = normal_attr.path.to_opt()
                && let Some(ident) = path.as_ident(db)
            {
                ident.data(db) == name
            } else {
                false
            }
        })
    }

    /// Returns true if this attribute list contains a marker attribute with the given name.
    ///
    /// Marker attributes have no arguments, no value, and no `()` in lowered HIR,
    /// for example `#[payable]`.
    pub fn has_marker_attr(self, db: &'db dyn HirDb, name: &str) -> bool {
        self.data(db).iter().any(|attr| {
            let Attr::Normal(normal_attr) = attr else {
                return false;
            };
            if normal_attr
                .path
                .to_opt()
                .and_then(|path| path.as_ident(db))
                .is_none_or(|ident| ident.data(db) != name)
            {
                return false;
            }

            parse_marker_attr_spec(&normal_attr.keyword_attr_spec(db)).is_ok()
        })
    }

    /// Returns the attribute with the given name, if present.
    pub fn get_attr(self, db: &'db dyn HirDb, name: &str) -> Option<&'db NormalAttr<'db>> {
        self.data(db).iter().find_map(|attr| {
            if let Attr::Normal(normal_attr) = attr
                && let Some(path) = normal_attr.path.to_opt()
                && let Some(ident) = path.as_ident(db)
                && ident.data(db) == name
            {
                Some(normal_attr)
            } else {
                None
            }
        })
    }

    /// Returns the ABI path from `#[abi = path::ToAbi]` or `#[abi(path::ToAbi)]`.
    pub fn abi_path(self, db: &'db dyn HirDb) -> Option<PathId<'db>> {
        self.data(db)
            .iter()
            .filter_map(|attr| {
                let Attr::Normal(normal_attr) = attr else {
                    return None;
                };
                let path = normal_attr.path.to_opt()?;
                let ident = path.as_ident(db)?;
                (ident.data(db) == "abi")
                    .then(|| normal_attr.path_value(db))
                    .flatten()
            })
            .last()
    }

    pub fn arithmetic_mode(self, db: &'db dyn HirDb) -> Option<ArithmeticMode> {
        self.data(db)
            .iter()
            .filter_map(|attr| {
                let Attr::Normal(normal_attr) = attr else {
                    return None;
                };
                let path = normal_attr.path.to_opt()?;
                let ident = path.as_ident(db)?;
                if ident.data(db) != "arithmetic" {
                    return None;
                }
                normal_attr.arithmetic_mode_arg(db)
            })
            .last()
    }
    pub fn inline_attr(self, db: &'db dyn HirDb) -> Option<InlineAttr> {
        match parse_inline_attr_specs(self.data(db).iter().filter_map(|attr| {
            let Attr::Normal(normal_attr) = attr else {
                return None;
            };
            if normal_attr
                .path
                .to_opt()
                .and_then(|path| path.as_ident(db))
                .is_none_or(|ident| ident.data(db) != "inline")
            {
                return None;
            }

            Some(normal_attr.keyword_attr_spec(db))
        })) {
            Ok(Some(hint)) => Some(InlineAttr::Hint(hint)),
            Ok(None) => None,
            Err(err) => Some(InlineAttr::Error(err.kind)),
        }
    }

    pub(crate) fn parse_loop_unroll_attr(
        self,
        db: &'db dyn HirDb,
    ) -> Result<Option<bool>, LoopUnrollAttrParseError> {
        parse_loop_unroll_attr_specs(self.data(db).iter().filter_map(|attr| {
            let Attr::Normal(normal_attr) = attr else {
                return None;
            };
            normal_attr.loop_unroll_attr_spec(db)
        }))
    }

    pub fn loop_unroll_attr(self, db: &'db dyn HirDb) -> Option<LoopUnrollAttr> {
        match self.parse_loop_unroll_attr(db) {
            Ok(Some(hint)) => Some(LoopUnrollAttr::Hint(hint)),
            Ok(None) => None,
            Err(err) => Some(LoopUnrollAttr::Error(err.kind)),
        }
    }

    pub fn manual_contract_root_attr(
        self,
        db: &'db dyn HirDb,
    ) -> Option<ManualContractRootAttr<'db>> {
        match parse_manual_contract_root_attr_specs(
            db,
            self.data(db).iter().filter_map(|attr| {
                let Attr::Normal(normal_attr) = attr else {
                    return None;
                };
                let name = normal_attr
                    .path
                    .to_opt()
                    .and_then(|path| path.as_ident(db))
                    .map(|ident| ident.data(db).as_str())?;
                matches!(name, "contract_init" | "contract_runtime")
                    .then_some((name, normal_attr.keyword_attr_spec(db)))
            }),
        ) {
            Ok(Some(attr)) => Some(attr),
            Ok(None) => None,
            Err(err) => Some(ManualContractRootAttr::Error(err.kind)),
        }
    }
}

impl<'db> NormalAttr<'db> {
    /// Returns true if this attribute has an argument with the given key (no value).
    ///
    /// For example, `#[test(should_revert)]` has the argument `should_revert`.
    pub fn has_arg(&self, db: &'db dyn HirDb, key: &str) -> bool {
        self.args.iter().any(|arg| {
            !arg.has_value
                && arg
                    .key
                    .to_opt()
                    .and_then(|p| p.as_ident(db))
                    .is_some_and(|ident| ident.data(db) == key)
        })
    }

    /// Returns the integer value for an attribute argument with the given key.
    ///
    /// For example, `#[test(panic = 0x11)]` with key `"panic"` returns `Some(BigUint(0x11))`.
    pub fn int_arg(&self, db: &'db dyn HirDb, key: &str) -> Option<num_bigint::BigUint> {
        self.args.iter().find_map(|arg| {
            let ident = arg.key.to_opt().and_then(|p| p.as_ident(db))?;
            if ident.data(db) != key {
                return None;
            }
            match &arg.value {
                Some(AttrArgValue::Lit(super::LitKind::Int(int_id))) => {
                    Some(int_id.data(db).clone())
                }
                _ => None,
            }
        })
    }

    pub fn arithmetic_mode_arg(&self, db: &'db dyn HirDb) -> Option<ArithmeticMode> {
        let [arg] = self.args.as_slice() else {
            return None;
        };
        if arg.has_value {
            return None;
        }
        let mode = arg
            .key
            .to_opt()
            .and_then(|path| path.as_ident(db))
            .map(|ident| ident.data(db).as_str())?;
        ArithmeticMode::parse(mode)
    }

    pub fn path_value(&self, db: &'db dyn HirDb) -> Option<PathId<'db>> {
        if self.has_value {
            return match self.value {
                Some(AttrArgValue::Path(path)) => Some(path),
                Some(AttrArgValue::Ident(ident)) => Some(PathId::from_ident(db, ident)),
                Some(AttrArgValue::Lit(_)) | None => None,
            };
        }

        if self.has_args {
            let [arg] = self.args.as_slice() else {
                return None;
            };
            if arg.has_value {
                return match arg.value {
                    Some(AttrArgValue::Path(path)) => Some(path),
                    Some(AttrArgValue::Ident(ident)) => Some(PathId::from_ident(db, ident)),
                    Some(AttrArgValue::Lit(_)) | None => None,
                };
            }
            return arg.key.to_opt();
        }

        None
    }

    pub(crate) fn keyword_attr_spec(&self, db: &'db dyn HirDb) -> KeywordAttrSpec {
        KeywordAttrSpec {
            has_value: self.has_value,
            has_args: self.has_args,
            args: self
                .args
                .iter()
                .map(|arg| KeywordAttrArgSpec {
                    key: arg.key_str(db).map(str::to_owned),
                    has_value: arg.has_value,
                })
                .collect(),
        }
    }

    pub(crate) fn loop_unroll_attr_spec(&self, db: &'db dyn HirDb) -> Option<KeywordAttrSpec> {
        match self
            .path
            .to_opt()
            .and_then(|path| path.as_ident(db))
            .map(|ident| ident.data(db).as_str())
        {
            Some("unroll") => {}
            Some(_) | None => return None,
        }

        Some(self.keyword_attr_spec(db))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum Attr<'db> {
    Normal(NormalAttr<'db>),
    DocComment(DocCommentAttr<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalAttr<'db> {
    pub path: Partial<PathId<'db>>,
    /// The value after `=` in `#[attr = value]`.
    pub value: Option<AttrArgValue<'db>>,
    /// True when the source contained `= ...`, even if the typed value was not lowerable.
    pub has_value: bool,
    /// True when the source contained `(...)`, even if the lowered arg list is empty.
    pub has_args: bool,
    pub args: Vec<AttrArg<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocCommentAttr<'db> {
    /// This is the text of the doc comment, excluding the `///` prefix.
    pub text: StringId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrArg<'db> {
    pub key: Partial<PathId<'db>>,
    /// The value after `=` in `#[attr(key = value)]`.
    pub value: Option<AttrArgValue<'db>>,
    /// True when the source contained `= ...`, even if the typed value was not lowerable.
    pub has_value: bool,
}

impl<'db> AttrArg<'db> {
    pub fn key_str(&self, db: &'db dyn HirDb) -> Option<&str> {
        self.key
            .to_opt()
            .and_then(|p| p.as_ident(db))
            .map(|i| i.data(db).as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrArgValue<'db> {
    Ident(IdentId<'db>),
    Path(PathId<'db>),
    Lit(super::LitKind<'db>),
}
