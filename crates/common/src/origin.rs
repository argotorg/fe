use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

const ORIGIN_EXPORT_KEY_STORAGE_SEPARATOR: char = '\u{1f}';

/// Owner-aware identity for origin nodes whose local IDs are scoped.
///
/// The fields are intentionally private: callers must provide an owner and a
/// local ID together, so a body-local ID cannot masquerade as a global origin.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OriginKey<Owner, Local> {
    owner: Owner,
    local: Local,
}

// SAFETY: `OriginKey` is a plain owner/local product. Delegating updates to the
// field implementations preserves Salsa's revision semantics for both parts.
unsafe impl<Owner, Local> salsa::Update for OriginKey<Owner, Local>
where
    Owner: salsa::Update,
    Local: salsa::Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let mut changed = false;
        unsafe {
            changed |= Owner::maybe_update(&mut (*old_pointer).owner, new_value.owner);
            changed |= Local::maybe_update(&mut (*old_pointer).local, new_value.local);
        }
        changed
    }
}

/// Error returned when building an invalid export-facing origin key.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OriginExportKeyError {
    EmptyKind,
    EmptyOwnerKey,
    EmptyLocalKey,
    ReservedStorageSeparator { field: &'static str },
}

impl fmt::Display for OriginExportKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyKind => write!(f, "origin export kind must not be empty"),
            Self::EmptyOwnerKey => write!(f, "origin export owner key must not be empty"),
            Self::EmptyLocalKey => write!(f, "origin export local key must not be empty"),
            Self::ReservedStorageSeparator { field } => write!(
                f,
                "origin export {field} must not contain the reserved storage separator"
            ),
        }
    }
}

impl std::error::Error for OriginExportKeyError {}

/// Stable key for an origin node that leaves the compiler.
///
/// `kind` is owned by the phase crate, while `owner_key` identifies the
/// containing object and `local_key` identifies the node inside that owner.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct OriginExportKey {
    kind: String,
    owner_key: String,
    local_key: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct OriginExportKeySerde {
    kind: String,
    owner_key: String,
    local_key: String,
}

impl OriginExportKey {
    pub fn new<Owner, Local>(kind: impl AsRef<str>, owner_key: &Owner, local_key: &Local) -> Self
    where
        Owner: OriginExportOwnerKey + ?Sized,
        Local: OriginExportLocalKey + ?Sized,
    {
        Self::try_new(kind, owner_key, local_key)
            .unwrap_or_else(|err| panic!("invalid origin export key: {err}"))
    }

    pub fn try_new<Owner, Local>(
        kind: impl AsRef<str>,
        owner_key: &Owner,
        local_key: &Local,
    ) -> Result<Self, OriginExportKeyError>
    where
        Owner: OriginExportOwnerKey + ?Sized,
        Local: OriginExportLocalKey + ?Sized,
    {
        Self::try_from_raw_parts(
            kind.as_ref(),
            owner_key.as_str(),
            local_key.to_export_local_key(),
        )
    }

    /// Build an export key from decoded or imported wire fields.
    ///
    /// Prefer [`OriginExportKey::new`] at compiler construction sites so owner
    /// and local-key namespaces stay nominal.
    pub fn try_from_raw_parts(
        kind: impl Into<String>,
        owner_key: impl Into<String>,
        local_key: impl Into<String>,
    ) -> Result<Self, OriginExportKeyError> {
        let kind = kind.into();
        let owner_key = owner_key.into();
        let local_key = local_key.into();
        validate_origin_export_key_part("kind", &kind)?;
        validate_origin_export_key_part("owner_key", &owner_key)?;
        validate_origin_export_key_part("local_key", &local_key)?;
        Ok(Self {
            kind,
            owner_key,
            local_key,
        })
    }

    pub fn kind(&self) -> &str {
        &self.kind
    }

    pub fn owner_key(&self) -> &str {
        &self.owner_key
    }

    pub fn local_key(&self) -> &str {
        &self.local_key
    }

    pub fn into_parts(self) -> (String, String, String) {
        (self.kind, self.owner_key, self.local_key)
    }

    /// Collision-resistant string for internal maps and fact ID allocation.
    pub fn canonical_storage_key(&self) -> String {
        format!(
            "{}{}{}{}{}",
            self.kind,
            ORIGIN_EXPORT_KEY_STORAGE_SEPARATOR,
            self.owner_key,
            ORIGIN_EXPORT_KEY_STORAGE_SEPARATOR,
            self.local_key
        )
    }

    /// Human-readable label for diagnostics and frontend origin labels.
    pub fn display_label(&self) -> String {
        format!("{}:{}:{}", self.kind, self.owner_key, self.local_key)
    }
}

impl Serialize for OriginExportKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        OriginExportKeySerde {
            kind: self.kind.clone(),
            owner_key: self.owner_key.clone(),
            local_key: self.local_key.clone(),
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for OriginExportKey {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = OriginExportKeySerde::deserialize(deserializer)?;
        Self::try_from_raw_parts(raw.kind, raw.owner_key, raw.local_key).map_err(de::Error::custom)
    }
}

fn validate_origin_export_key_part(
    field: &'static str,
    value: &str,
) -> Result<(), OriginExportKeyError> {
    if value.is_empty() {
        return match field {
            "kind" => Err(OriginExportKeyError::EmptyKind),
            "owner_key" => Err(OriginExportKeyError::EmptyOwnerKey),
            "local_key" => Err(OriginExportKeyError::EmptyLocalKey),
            _ => Err(OriginExportKeyError::ReservedStorageSeparator { field }),
        };
    }
    if value.contains(ORIGIN_EXPORT_KEY_STORAGE_SEPARATOR) {
        return Err(OriginExportKeyError::ReservedStorageSeparator { field });
    }
    Ok(())
}

pub trait OriginExportOwnerKey {
    fn as_str(&self) -> &str;
}

pub trait OriginExportLocalKey {
    fn to_export_local_key(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OriginKeyTextError {
    Empty { kind: &'static str },
    ReservedStorageSeparator { kind: &'static str },
}

impl fmt::Display for OriginKeyTextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty { kind } => write!(f, "{kind} must not be empty"),
            Self::ReservedStorageSeparator { kind } => {
                write!(
                    f,
                    "{kind} must not contain reserved origin storage separator"
                )
            }
        }
    }
}

impl std::error::Error for OriginKeyTextError {}

pub fn validate_origin_key_text(kind: &'static str, value: &str) -> Result<(), OriginKeyTextError> {
    if value.is_empty() {
        return Err(OriginKeyTextError::Empty { kind });
    }
    if value.contains(ORIGIN_EXPORT_KEY_STORAGE_SEPARATOR) {
        return Err(OriginKeyTextError::ReservedStorageSeparator { kind });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        OriginExportKey, OriginExportKeyError, OriginExportLocalKey, OriginExportOwnerKey,
        OriginKey, validate_origin_key_text,
    };

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
    struct TestOwner(u32);

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
    struct TestLocal(u32);

    struct TestOwnerKey(&'static str);

    impl OriginExportOwnerKey for TestOwnerKey {
        fn as_str(&self) -> &str {
            self.0
        }
    }

    struct TestLocalKey(&'static str);

    impl OriginExportLocalKey for TestLocalKey {
        fn to_export_local_key(&self) -> String {
            self.0.to_string()
        }
    }

    fn raw_export_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn same_local_id_in_different_owners_does_not_collide() {
        let first = OriginKey::new(TestOwner(0), TestLocal(7));
        let second = OriginKey::new(TestOwner(1), TestLocal(7));

        assert_ne!(first, second);
    }

    #[test]
    fn owner_and_local_id_round_trip() {
        let key = OriginKey::new(TestOwner(2), TestLocal(3));

        assert_eq!(key.owner(), &TestOwner(2));
        assert_eq!(key.local(), &TestLocal(3));
        assert_eq!(key.into_parts(), (TestOwner(2), TestLocal(3)));
    }

    #[test]
    fn export_key_keeps_kind_owner_and_local_separate() {
        let expr = raw_export_key("hir.expr", "body:a", "0");
        let stmt = raw_export_key("hir.stmt", "body:a", "0");
        let other_body_expr = raw_export_key("hir.expr", "body:b", "0");

        assert_ne!(expr, stmt);
        assert_ne!(expr, other_body_expr);
        assert_eq!(expr.kind(), "hir.expr");
        assert_eq!(expr.owner_key(), "body:a");
        assert_eq!(expr.local_key(), "0");
    }

    #[test]
    fn export_key_constructor_requires_typed_owner_and_local_key_parts() {
        let key = OriginExportKey::new(
            "semantic",
            &TestOwnerKey("semantic:test"),
            &TestLocalKey("expr:0"),
        );

        assert_eq!(key.owner_key(), "semantic:test");
        assert_eq!(key.local_key(), "expr:0");
    }

    #[test]
    fn export_key_roundtrips_through_raw_parts() {
        let key = raw_export_key("runtime.stmt", "runtime:test", "block:0:stmt:1");
        let (kind, owner, local) = key.clone().into_parts();

        assert_eq!(
            OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap(),
            key
        );
    }

    #[test]
    fn export_key_roundtrips_through_json() {
        let key = raw_export_key("runtime.terminator", "runtime:test", "block:0:terminator");
        let json = serde_json::to_string(&key).unwrap();

        assert_eq!(serde_json::from_str::<OriginExportKey>(&json).unwrap(), key);
    }

    #[test]
    fn export_key_formats_canonical_storage_key_and_display_label() {
        let key = raw_export_key("bytecode.pc", "object:Foo:section:runtime", "pc:4..8");

        assert_eq!(
            key.canonical_storage_key(),
            "bytecode.pc\u{1f}object:Foo:section:runtime\u{1f}pc:4..8"
        );
        assert_eq!(
            key.display_label(),
            "bytecode.pc:object:Foo:section:runtime:pc:4..8"
        );
    }

    #[test]
    fn export_key_rejects_empty_kind_owner_and_local_parts() {
        assert_eq!(
            OriginExportKey::try_from_raw_parts("", "semantic:test", "expr:0"),
            Err(OriginExportKeyError::EmptyKind)
        );
        assert_eq!(
            OriginExportKey::try_from_raw_parts("semantic", "", "expr:0"),
            Err(OriginExportKeyError::EmptyOwnerKey)
        );
        assert_eq!(
            OriginExportKey::try_from_raw_parts("semantic", "semantic:test", ""),
            Err(OriginExportKeyError::EmptyLocalKey)
        );
    }

    #[test]
    fn export_key_rejects_reserved_storage_separator() {
        assert_eq!(
            OriginExportKey::try_from_raw_parts("semantic\u{1f}origin", "semantic:test", "expr:0",),
            Err(OriginExportKeyError::ReservedStorageSeparator { field: "kind" })
        );
        assert_eq!(
            OriginExportKey::try_from_raw_parts("semantic", "semantic\u{1f}test", "expr:0",),
            Err(OriginExportKeyError::ReservedStorageSeparator { field: "owner_key" })
        );
        assert_eq!(
            OriginExportKey::try_from_raw_parts("semantic", "semantic:test", "expr\u{1f}0",),
            Err(OriginExportKeyError::ReservedStorageSeparator { field: "local_key" })
        );
    }

    #[test]
    fn export_key_deserialization_validates_parts() {
        let json = r#"{
            "kind": "semantic",
            "owner_key": "",
            "local_key": "expr:0"
        }"#;

        let err = serde_json::from_str::<OriginExportKey>(json)
            .expect_err("origin export key decoding should validate owner/local parts");
        assert!(
            err.to_string()
                .contains("origin export owner key must not be empty")
        );
    }

    #[test]
    fn origin_key_text_validation_rejects_invalid_wrapper_text() {
        assert!(validate_origin_key_text("origin owner key", "runtime:test").is_ok());
        assert!(validate_origin_key_text("origin owner key", "").is_err());
        assert!(validate_origin_key_text("origin owner key", "runtime\u{1f}test").is_err());
    }
}

impl<Owner, Local> OriginKey<Owner, Local> {
    pub const fn new(owner: Owner, local: Local) -> Self {
        Self { owner, local }
    }

    pub fn owner(&self) -> &Owner {
        &self.owner
    }

    pub fn local(&self) -> &Local {
        &self.local
    }

    pub fn into_parts(self) -> (Owner, Local) {
        (self.owner, self.local)
    }
}
