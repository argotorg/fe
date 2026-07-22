//! Input side of the `metadata.json` round trip (`fe build --from-metadata`).
//!
//! Parses and validates a contract metadata artifact produced by
//! `fe build --emit metadata` (see `write_metadata_artifacts` in `build.rs`)
//! and materializes the recorded project so the normal build path can
//! recompile it.

use std::{fs, io::Read};

use camino::{Utf8Path, Utf8PathBuf};
use serde_json::Value;

/// Read metadata JSON from `input`; `-` reads from stdin.
pub fn read_metadata(input: &Utf8Path) -> Result<Value, String> {
    let (raw, source) = if input.as_str() == "-" {
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .map_err(|err| format!("Failed to read metadata from stdin: {err}"))?;
        (buf, "<stdin>".to_string())
    } else {
        (
            fs::read_to_string(input.as_std_path())
                .map_err(|err| format!("Failed to read metadata file {input}: {err}"))?,
            input.to_string(),
        )
    };
    serde_json::from_str(&raw)
        .map_err(|err| format!("Failed to parse metadata JSON from {source}: {err}"))
}

/// Check the fields the reconstruction relies on, with actionable messages.
pub fn validate_metadata(metadata: &Value) -> Result<(), String> {
    match metadata["language"].as_str() {
        Some("Fe") => {}
        Some(other) => {
            return Err(format!(
                "metadata `language` must be \"Fe\", found \"{other}\""
            ));
        }
        None => return Err("metadata is missing the `language` field".to_string()),
    }
    let sources = metadata["sources"]
        .as_object()
        .ok_or_else(|| "metadata is missing the `sources` object".to_string())?;
    if sources.is_empty() {
        return Err("metadata `sources` is empty".to_string());
    }
    let ingots = metadata["settings"]["ingots"]
        .as_array()
        .ok_or_else(|| "metadata is missing the `settings.ingots` array".to_string())?;
    let root_count = ingots
        .iter()
        .filter(|ingot| ingot["namespace"].as_str().unwrap_or("").is_empty())
        .count();
    if root_count != 1 {
        return Err(format!(
            "metadata `settings.ingots` must contain exactly one root entry (empty namespace), found {root_count}"
        ));
    }
    Ok(())
}

/// Materialize a buildable project under `dest` from validated metadata: one
/// directory per `settings.ingots[]` entry (root at `dest/<name>`, deps at
/// `dest/<namespace>`), regenerating each `fe.toml` and writing every
/// `sources` entry into its namespaced layout. Returns the root ingot
/// directory.
pub fn reconstruct_project(metadata: &Value, dest: &Utf8Path) -> Result<Utf8PathBuf, String> {
    let ingots = metadata["settings"]["ingots"]
        .as_array()
        .ok_or_else(|| "metadata is missing the `settings.ingots` array".to_string())?;

    // Non-root namespaces, used to route each source key to its owning ingot.
    let mut non_root_namespaces: Vec<String> = Vec::new();
    for ingot in ingots {
        let namespace = ingot["namespace"].as_str().unwrap_or("");
        if !namespace.is_empty() {
            checked_dir_name(namespace, "ingot namespace")?;
            non_root_namespaces.push(namespace.to_string());
        }
    }

    // The root ingot directory is named after the root ingot; dodge the
    // (unlikely) case of a dependency namespace with the same name. Nothing
    // references the root directory by name, so renaming it is safe.
    let root = ingots
        .iter()
        .find(|ingot| ingot["namespace"].as_str().unwrap_or("").is_empty())
        .ok_or_else(|| {
            "metadata `settings.ingots` has no root entry (empty namespace)".to_string()
        })?;
    let root_name = root["name"]
        .as_str()
        .ok_or_else(|| "root `settings.ingots` entry is missing `name`".to_string())?;
    // Emitted names can be arbitrary directory basenames (e.g. `my%20project`
    // for a standalone target in a URL-encoded path), so sanitize rather than
    // validate; unlike namespaces, the name never has to survive as-is.
    let mut root_dir_name = sanitize_ingot_name(root_name);
    while non_root_namespaces.contains(&root_dir_name) {
        root_dir_name.push_str("-root");
    }

    let dir_for = |namespace: &str| -> Utf8PathBuf {
        if namespace.is_empty() {
            dest.join(&root_dir_name)
        } else {
            dest.join(namespace)
        }
    };

    // 1. Regenerate each ingot's fe.toml.
    let empty_deps = serde_json::Map::new();
    for ingot in ingots {
        let name = ingot["name"]
            .as_str()
            .ok_or_else(|| "`settings.ingots` entry is missing `name`".to_string())?;
        let namespace = ingot["namespace"].as_str().unwrap_or("");
        let dir = dir_for(namespace);
        fs::create_dir_all(dir.join("src").as_std_path())
            .map_err(|err| format!("Failed to create {dir}/src: {err}"))?;

        let mut toml = format!("[ingot]\nname = \"{}\"\n", sanitize_ingot_name(name));
        if let Some(version) = ingot["version"].as_str() {
            checked_version(version)?;
            toml.push_str(&format!("version = \"{version}\"\n"));
        } else {
            // Standalone-target metadata records `version: null` (the pseudo-ingot
            // has none), but fe.toml requires a version; it does not affect codegen.
            toml.push_str("version = \"0.1.0\"\n");
        }
        if let Some(arith) = ingot["arithmetic"].as_str() {
            if arith != "checked" && arith != "unchecked" {
                return Err(format!(
                    "invalid `arithmetic` value \"{arith}\" for ingot `{name}` in metadata"
                ));
            }
            toml.push_str(&format!("arithmetic = \"{arith}\"\n"));
        }
        let deps = ingot["dependencies"].as_object().unwrap_or(&empty_deps);
        let path_deps: Vec<(&String, &str)> = deps
            .iter()
            .filter_map(|(alias, target)| {
                let target = target.as_str()?;
                // std/core are provided by the compiler; only scaffold real path deps.
                (target != "std" && target != "core").then_some((alias, target))
            })
            .collect();
        if !path_deps.is_empty() {
            toml.push_str("\n[dependencies]\n");
            for (alias, target) in path_deps {
                checked_dependency_alias(alias)?;
                if !non_root_namespaces.iter().any(|ns| ns == target) {
                    return Err(format!(
                        "dependency `{alias}` of ingot `{name}` points at unknown namespace \"{target}\""
                    ));
                }
                toml.push_str(&format!("{alias} = {{ path = \"../{target}\" }}\n"));
            }
        }
        fs::write(dir.join("fe.toml").as_std_path(), toml)
            .map_err(|err| format!("Failed to write {dir}/fe.toml: {err}"))?;
    }

    // 2. Write every source into its owning ingot's namespaced src/ layout.
    let sources = metadata["sources"]
        .as_object()
        .ok_or_else(|| "metadata is missing the `sources` object".to_string())?;
    for (key, source) in sources {
        let content = source["content"]
            .as_str()
            .ok_or_else(|| format!("metadata source \"{key}\" is missing string `content`"))?;
        let (namespace, rel) = non_root_namespaces
            .iter()
            .find_map(|ns| {
                key.strip_prefix(&format!("{ns}/"))
                    .map(|rel| (ns.as_str(), rel))
            })
            .unwrap_or(("", key.as_str()));
        checked_source_path(rel)?;
        // Standalone-target metadata keys its single source by file basename
        // (no directory); the ingot resolver only loads `src/**/*.fe`, so
        // materialize bare keys under `src/`.
        let target = if rel.contains('/') {
            dir_for(namespace).join(rel)
        } else {
            dir_for(namespace).join("src").join(rel)
        };
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent.as_std_path())
                .map_err(|err| format!("Failed to create {parent}: {err}"))?;
        }
        fs::write(target.as_std_path(), content)
            .map_err(|err| format!("Failed to write {target}: {err}"))?;
    }

    Ok(dest.join(root_dir_name))
}

/// Ingot names and namespaces become directory names and TOML string values,
/// so restrict them to the alphabet the metadata emitter produces.
fn checked_dir_name(name: &str, what: &str) -> Result<(), String> {
    let valid = !name.is_empty()
        && name != "."
        && name != ".."
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '.'));
    if valid {
        Ok(())
    } else {
        Err(format!("invalid {what} \"{name}\" in metadata"))
    }
}

/// The emitter falls back to directory basenames for unnamed ingots (e.g. a
/// standalone target in a temp dir), which may not be valid Fe ingot names
/// (alphanumeric + `_`). The name does not affect codegen, so sanitize it for
/// the regenerated `fe.toml`.
fn sanitize_ingot_name(name: &str) -> String {
    if name.is_empty() {
        return "ingot".to_string();
    }
    name.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Versions are written into `fe.toml` string values; allow the SemVer alphabet.
fn checked_version(version: &str) -> Result<(), String> {
    let valid = version
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '+' | '_'));
    if valid {
        Ok(())
    } else {
        Err(format!("invalid ingot version \"{version}\" in metadata"))
    }
}

/// Dependency aliases become bare `fe.toml` keys.
fn checked_dependency_alias(alias: &str) -> Result<(), String> {
    let valid = !alias.is_empty()
        && alias
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-'));
    if valid {
        Ok(())
    } else {
        Err(format!("invalid dependency alias \"{alias}\" in metadata"))
    }
}

/// Source keys must stay inside their owning ingot directory.
fn checked_source_path(rel: &str) -> Result<(), String> {
    let valid = !rel.is_empty()
        && !rel.contains('\\')
        && !Utf8Path::new(rel).is_absolute()
        && rel
            .split('/')
            .all(|component| !component.is_empty() && component != "." && component != "..");
    if valid {
        Ok(())
    } else {
        Err(format!("invalid source path \"{rel}\" in metadata"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn minimal_metadata(source_key: &str) -> Value {
        serde_json::json!({
            "version": 1,
            "language": "Fe",
            "sources": {
                source_key: { "content": "pub contract Foo {\n}\n" },
            },
            "settings": {
                "ingots": [{
                    "name": "app",
                    "version": "0.1.0",
                    "namespace": "",
                    "arithmetic": "checked",
                    "dependencies": {},
                }],
            },
        })
    }

    #[test]
    fn validate_rejects_wrong_language() {
        let mut metadata = minimal_metadata("src/lib.fe");
        metadata["language"] = "Solidity".into();
        let err = validate_metadata(&metadata).unwrap_err();
        assert!(err.contains("language"), "unexpected error: {err}");
    }

    #[test]
    fn validate_rejects_missing_sources() {
        let mut metadata = minimal_metadata("src/lib.fe");
        metadata.as_object_mut().unwrap().remove("sources");
        let err = validate_metadata(&metadata).unwrap_err();
        assert!(err.contains("sources"), "unexpected error: {err}");
    }

    #[test]
    fn reconstruct_rejects_escaping_source_paths() {
        for key in ["../evil.fe", "/etc/evil.fe", "src/../../evil.fe"] {
            let metadata = minimal_metadata(key);
            let temp = tempdir().expect("tempdir");
            let dest = Utf8Path::from_path(temp.path()).expect("utf8 tempdir");
            let err = reconstruct_project(&metadata, dest)
                .expect_err(&format!("key {key} must be rejected"));
            assert!(
                err.contains("invalid source path"),
                "unexpected error: {err}"
            );
        }
    }

    #[test]
    fn reconstruct_rejects_escaping_namespaces() {
        let mut metadata = minimal_metadata("src/lib.fe");
        metadata["settings"]["ingots"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::json!({
                "name": "dep",
                "namespace": "../dep",
                "dependencies": {},
            }));
        let temp = tempdir().expect("tempdir");
        let dest = Utf8Path::from_path(temp.path()).expect("utf8 tempdir");
        let err = reconstruct_project(&metadata, dest).expect_err("namespace must be rejected");
        assert!(
            err.contains("invalid ingot namespace"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn reconstruct_accepts_url_encoded_standalone_root_name() {
        // Standalone-target metadata records the parent directory's URL
        // basename as the root name, which may contain characters (e.g. `%20`)
        // that are not valid directory names. Reconstruction must sanitize,
        // not reject, since the emitter produces such names.
        let mut metadata = minimal_metadata("main.fe");
        metadata["settings"]["ingots"][0]["name"] = "my%20project".into();
        metadata["settings"]["ingots"][0]["version"] = Value::Null;
        let temp = tempdir().expect("tempdir");
        let dest = Utf8Path::from_path(temp.path()).expect("utf8 tempdir");
        let root_dir = reconstruct_project(&metadata, dest).expect("reconstruct");
        assert_eq!(root_dir, dest.join("my_20project"));
        let toml = fs::read_to_string(root_dir.join("fe.toml").as_std_path()).expect("fe.toml");
        assert!(toml.contains("name = \"my_20project\""), "toml: {toml}");
        assert!(root_dir.join("src/main.fe").is_file());
    }

    #[test]
    fn reconstruct_root_dir_dodges_dependency_namespace_collision() {
        let mut metadata = minimal_metadata("src/lib.fe");
        metadata["settings"]["ingots"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::json!({
                "name": "app",
                "version": "1.0.0",
                "namespace": "app",
                "arithmetic": "checked",
                "dependencies": {},
            }));
        metadata["sources"].as_object_mut().unwrap().insert(
            "app/src/lib.fe".to_string(),
            serde_json::json!({ "content": "pub fn helper() {}\n" }),
        );
        let temp = tempdir().expect("tempdir");
        let dest = Utf8Path::from_path(temp.path()).expect("utf8 tempdir");
        let root_dir = reconstruct_project(&metadata, dest).expect("reconstruct");
        assert_eq!(root_dir, dest.join("app-root"));
        assert!(root_dir.join("fe.toml").is_file());
        assert!(dest.join("app/fe.toml").is_file());
    }
}
