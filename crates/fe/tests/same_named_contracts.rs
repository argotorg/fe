//! Contracts in different modules, or in a dependency, may share a name. Each
//! keeps its own code, and `fe build` names an artifact after the module path
//! only when its bare name would clash.

use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    process::Command,
};

use serde_json::Value;
use tempfile::tempdir;

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name)
}

/// Runs `fe` and returns its combined output and exit code.
fn run_fe(args: &[&str], paths: &[&Path]) -> (String, i32) {
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(args)
        .args(paths)
        .env("NO_COLOR", "1")
        .output()
        .expect("run fe");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    (text, output.status.code().unwrap_or(-1))
}

fn file_names(dir: &Path) -> BTreeSet<String> {
    fs::read_dir(dir)
        .unwrap_or_else(|err| panic!("read {}: {err}", dir.display()))
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect()
}

fn read(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()))
}

/// Builds the fixture ingot with every artifact kind into a fresh directory.
fn build_fixture() -> tempfile::TempDir {
    let out = tempdir().expect("tempdir");
    let (output, code) = run_fe(
        &[
            "build",
            "--emit",
            "bytecode,runtime-bytecode,abi,metadata",
            "--out-dir",
            out.path().to_str().unwrap(),
        ],
        &[&fixture("same_named_contracts")],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    out
}

#[test]
fn fe_test_deploys_every_same_named_contract_with_its_own_code() {
    let (output, code) = run_fe(&["test"], &[&fixture("same_named_contracts")]);
    assert_eq!(code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS") && output.contains("1 passed"),
        "unexpected output:\n{output}"
    );
}

#[test]
fn fe_test_deploys_a_dependency_contract_that_shares_a_name() {
    let (output, code) = run_fe(&["test"], &[&fixture("same_named_contracts_dependency")]);
    assert_eq!(code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("app each_ingot_keeps_its_own_code") && output.contains("1 passed"),
        "unexpected output:\n{output}"
    );
}

#[test]
fn fe_build_qualifies_only_the_artifacts_whose_names_clash() {
    let out = build_fixture();
    let mut expected = BTreeSet::new();
    for stem in ["Other", "lib__a__C", "lib__a__inner__C", "lib__b__C"] {
        for suffix in ["abi.json", "bin", "metadata.json", "runtime.bin"] {
            expected.insert(format!("{stem}.{suffix}"));
        }
    }
    assert_eq!(file_names(out.path()), expected);

    let runtime: Vec<String> = ["lib__a__C", "lib__a__inner__C", "lib__b__C"]
        .iter()
        .map(|stem| read(&out.path().join(format!("{stem}.runtime.bin"))))
        .collect();
    assert!(runtime.iter().all(|code| !code.trim().is_empty()));
    assert!(
        runtime[0] != runtime[1] && runtime[1] != runtime[2] && runtime[0] != runtime[2],
        "the three contracts have different code"
    );

    // The metadata target is the source file and the contract's path in it.
    for (stem, source, path) in [
        ("lib__a__C", "src/a.fe", "C"),
        ("lib__a__inner__C", "src/a.fe", "inner::C"),
        ("lib__b__C", "src/b.fe", "C"),
    ] {
        let metadata: Value =
            serde_json::from_str(&read(&out.path().join(format!("{stem}.metadata.json"))))
                .expect("metadata json");
        assert_eq!(
            metadata["settings"]["compilationTarget"],
            serde_json::json!({ source: path })
        );
    }
}

#[test]
fn a_qualified_artifact_is_the_code_the_contract_has_on_its_own() {
    let out = build_fixture();

    // The same contract in an ingot where its name is not shared.
    let alone = tempdir().expect("tempdir");
    fs::create_dir_all(alone.path().join("src")).unwrap();
    fs::write(
        alone.path().join("fe.toml"),
        "[ingot]\nname = \"alone\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    for file in ["lib.fe", "b.fe"] {
        fs::copy(
            fixture("same_named_contracts/src").join(file),
            alone.path().join("src").join(file),
        )
        .unwrap();
    }
    let alone_out = alone.path().join("out");
    let (output, code) = run_fe(
        &[
            "build",
            "--emit",
            "bytecode,runtime-bytecode",
            "--out-dir",
            alone_out.to_str().unwrap(),
        ],
        &[alone.path()],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    for suffix in ["bin", "runtime.bin"] {
        assert_eq!(
            read(&out.path().join(format!("lib__b__C.{suffix}"))),
            read(&alone_out.join(format!("C.{suffix}"))),
            "{suffix} differs from the contract built alone"
        );
    }
}

#[test]
fn contract_selection_names_the_clashing_artifacts() {
    let fixture = fixture("same_named_contracts");
    let out = tempdir().expect("tempdir");
    let out_dir = out.path().to_str().unwrap();

    // A source name that several contracts share cannot pick one.
    let (output, code) = run_fe(
        &["build", "--contract", "C", "--out-dir", out_dir],
        &[&fixture],
    );
    assert_eq!(code, 1, "expected failure:\n{output}");
    assert!(
        output.contains("Contract \"C\" is defined more than once"),
        "{output}"
    );
    for line in [
        "lib__a__C (src/a.fe)",
        "lib__a__inner__C (src/a.fe)",
        "lib__b__C (src/b.fe)",
    ] {
        assert!(output.contains(line), "missing `{line}`:\n{output}");
    }
    assert!(file_names(out.path()).is_empty());

    // The artifact name picks one.
    let (output, code) = run_fe(
        &[
            "build",
            "--contract",
            "lib__a__inner__C",
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            out_dir,
        ],
        &[&fixture],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    assert_eq!(
        file_names(out.path()),
        BTreeSet::from(["lib__a__inner__C.runtime.bin".to_string()])
    );

    // A source name that only one contract has still works.
    let (output, code) = run_fe(
        &[
            "build",
            "--contract",
            "Other",
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            out_dir,
        ],
        &[&fixture],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    assert!(file_names(out.path()).contains("Other.runtime.bin"));

    // An unknown name lists the artifact names.
    let (output, code) = run_fe(
        &["build", "--contract", "Missing", "--out-dir", out_dir],
        &[&fixture],
    );
    assert_eq!(code, 1, "expected failure:\n{output}");
    assert!(
        output.contains("Contract \"Missing\" not found"),
        "{output}"
    );
    assert!(output.contains("  - lib__b__C"), "{output}");
}

fn copy_dir(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

#[test]
fn a_workspace_root_passes_contract_names_on_to_the_member() {
    let workspace = tempdir().expect("tempdir");
    fs::write(
        workspace.path().join("fe.toml"),
        "[workspace]\nname = \"shared_workspace\"\nversion = \"0.1.0\"\nmembers = [\"ingots/*\"]\n",
    )
    .unwrap();
    copy_dir(
        &fixture("same_named_contracts"),
        &workspace.path().join("ingots/shared"),
    );
    let out = tempdir().expect("tempdir");
    let out_dir = out.path().to_str().unwrap();

    // The source name is declared by the member, which reports the clash.
    let (output, code) = run_fe(
        &["build", "--contract", "C", "--out-dir", out_dir],
        &[workspace.path()],
    );
    assert_eq!(code, 1, "expected failure:\n{output}");
    assert!(
        output.contains("Contract \"C\" is defined more than once")
            && output.contains("lib__b__C (src/b.fe)"),
        "{output}"
    );

    // An artifact name selects one contract.
    let (output, code) = run_fe(
        &[
            "build",
            "--contract",
            "lib__b__C",
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            out_dir,
        ],
        &[workspace.path()],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    assert_eq!(
        file_names(out.path()),
        BTreeSet::from(["lib__b__C.runtime.bin".to_string()])
    );
}

#[test]
fn a_workspace_root_reports_a_source_name_shared_across_members() {
    let workspace = tempdir().expect("tempdir");
    fs::write(
        workspace.path().join("fe.toml"),
        "[workspace]\nname = \"shared_workspace\"\nversion = \"0.1.0\"\nmembers = [\"ingots/*\"]\n",
    )
    .unwrap();
    copy_dir(
        &fixture("same_named_contracts"),
        &workspace.path().join("ingots/shared"),
    );
    // A member whose only `C` keeps its bare artifact name.
    let single = workspace.path().join("ingots/single");
    fs::create_dir_all(single.join("src")).unwrap();
    fs::write(
        single.join("fe.toml"),
        "[ingot]\nname = \"single\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(single.join("src/lib.fe"), "pub contract C {}\n").unwrap();
    let out = tempdir().expect("tempdir");

    let (output, code) = run_fe(
        &[
            "build",
            "--contract",
            "C",
            "--out-dir",
            out.path().to_str().unwrap(),
        ],
        &[workspace.path()],
    );
    assert_eq!(code, 1, "expected failure:\n{output}");
    assert!(
        output.contains("Contract \"C\" is defined in multiple workspace members")
            && output.contains("- same_named_contracts (")
            && output.contains("- single ("),
        "{output}"
    );
    assert!(file_names(out.path()).is_empty());
}

#[test]
fn from_metadata_rebuilds_a_qualified_contract() {
    let out = build_fixture();
    for stem in ["lib__a__inner__C", "lib__b__C"] {
        let rebuilt = tempdir().expect("tempdir");
        let (output, code) = run_fe(
            &[
                "build",
                "--emit",
                "runtime-bytecode",
                "--out-dir",
                rebuilt.path().to_str().unwrap(),
                "--from-metadata",
            ],
            &[&out.path().join(format!("{stem}.metadata.json"))],
        );
        assert_eq!(code, 0, "rebuild of {stem} failed:\n{output}");
        assert_eq!(
            read(&rebuilt.path().join(format!("{stem}.runtime.bin"))),
            read(&out.path().join(format!("{stem}.runtime.bin"))),
            "rebuilt {stem} differs from the original"
        );
    }
}

#[test]
fn a_dependency_contract_does_not_rename_the_ingots_own_contract() {
    let out = tempdir().expect("tempdir");
    let (output, code) = run_fe(
        &[
            "build",
            "--ingot",
            "app",
            "--emit",
            "bytecode,ir",
            "--out-dir",
            out.path().to_str().unwrap(),
        ],
        &[&fixture("same_named_contracts_dependency")],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    // Only the ingot's own contracts are artifacts, under their own names.
    assert!(out.path().join("C.bin").is_file(), "{output}");
    assert!(out.path().join("Factory.bin").is_file(), "{output}");
    assert!(!out.path().join("lib__C.bin").exists(), "{output}");

    // The dependency's contract, created by `Factory`, is an object of its own.
    let ir = read(&out.path().join("app/app.sona"));
    for object in ["object @C {", "object @Factory {", "object @lib__C {"] {
        assert!(ir.contains(object), "missing `{object}` in:\n{ir}");
    }
}

#[test]
fn contracts_written_as_manual_roots_are_qualified_like_the_others() {
    let out = tempdir().expect("tempdir");
    let (output, code) = run_fe(
        &[
            "build",
            "--emit",
            "bytecode,runtime-bytecode,metadata",
            "--out-dir",
            out.path().to_str().unwrap(),
        ],
        &[&fixture("same_named_manual_contracts")],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    let mut expected = BTreeSet::new();
    for stem in ["lib__a__M", "lib__b__M"] {
        for suffix in ["bin", "runtime.bin", "metadata.json"] {
            expected.insert(format!("{stem}.{suffix}"));
        }
    }
    assert_eq!(file_names(out.path()), expected);
}

#[test]
fn a_single_file_with_a_nested_module_qualifies_clashing_names() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("shared.fe");
    fs::write(
        &file,
        "pub contract C {}\n\npub mod inner {\n    pub contract C {}\n}\n",
    )
    .unwrap();
    let out = dir.path().join("out");
    let out_dir = out.to_str().unwrap();

    let (output, code) = run_fe(
        &["build", "--emit", "bytecode", "--out-dir", out_dir],
        &[&file],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    assert_eq!(
        file_names(&out),
        BTreeSet::from([
            "shared__C.bin".to_string(),
            "shared__inner__C.bin".to_string()
        ])
    );

    let (output, code) = run_fe(
        &["build", "--contract", "C", "--out-dir", out_dir],
        &[&file],
    );
    assert_eq!(code, 1, "expected failure:\n{output}");
    assert!(
        output.contains("Contract \"C\" is defined more than once"),
        "{output}"
    );
}

#[test]
fn a_single_file_rebuilds_from_metadata() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("shared.fe");
    fs::write(
        &file,
        "pub msg Get {\n    #[selector = sol(\"get()\")]\n    Get -> u256,\n}\n\n\
         pub contract C {\n    recv Get {\n        Get -> u256 {\n            1\n        }\n    }\n}\n\n\
         pub mod inner {\n    use super::Get\n\n    pub contract C {\n        recv Get {\n            \
         Get -> u256 {\n                2\n            }\n        }\n    }\n}\n",
    )
    .unwrap();
    let out = dir.path().join("out");
    let (output, code) = run_fe(
        &[
            "build",
            "--emit",
            "runtime-bytecode,metadata",
            "--out-dir",
            out.to_str().unwrap(),
        ],
        &[&file],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    assert_ne!(
        read(&out.join("shared__C.runtime.bin")),
        read(&out.join("shared__inner__C.runtime.bin"))
    );

    // The rebuild puts the file in an ingot, where the artifact is named after
    // the ingot's module path, but it is the same contract.
    for stem in ["shared__C", "shared__inner__C"] {
        let rebuilt = tempdir().expect("tempdir");
        let (output, code) = run_fe(
            &[
                "build",
                "--emit",
                "runtime-bytecode",
                "--out-dir",
                rebuilt.path().to_str().unwrap(),
                "--from-metadata",
            ],
            &[&out.join(format!("{stem}.metadata.json"))],
        );
        assert_eq!(code, 0, "rebuild of {stem} failed:\n{output}");
        let files = file_names(rebuilt.path());
        let [rebuilt_file] = files.iter().collect::<Vec<_>>()[..] else {
            panic!("expected one artifact, found {files:?}");
        };
        assert_eq!(
            read(&rebuilt.path().join(rebuilt_file)),
            read(&out.join(format!("{stem}.runtime.bin"))),
            "rebuilt {stem} differs from the original"
        );
    }
}

#[test]
fn contracts_with_the_same_identity_get_distinct_names() {
    // Floating modules (`src/x/baz.fe` without `src/x.fe`) have the same
    // module path, so their contracts share every candidate name. They are
    // numbered once for the whole ingot, so the module packages agree.
    let dir = tempdir().expect("tempdir");
    fs::write(
        dir.path().join("fe.toml"),
        "[ingot]\nname = \"floating\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::create_dir_all(dir.path().join("src")).unwrap();
    fs::write(dir.path().join("src/lib.fe"), "").unwrap();
    for (module, value) in [("x", 1), ("y", 2)] {
        fs::create_dir_all(dir.path().join("src").join(module)).unwrap();
        fs::write(
            dir.path().join("src").join(module).join("baz.fe"),
            format!(
                "pub msg Get {{\n    #[selector = sol(\"get()\")]\n    Get -> u256,\n}}\n\n\
                 pub contract C {{\n    recv Get {{\n        Get -> u256 {{\n            {value}\n        }}\n    }}\n}}\n"
            ),
        )
        .unwrap();
    }
    let out = dir.path().join("out");
    let (output, code) = run_fe(
        &[
            "build",
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            out.to_str().unwrap(),
        ],
        &[dir.path()],
    );
    assert_eq!(code, 0, "fe build failed:\n{output}");
    let files = file_names(&out);
    let [first, second] = files.iter().collect::<Vec<_>>()[..] else {
        panic!("expected two artifacts, found {files:?}");
    };
    assert_ne!(read(&out.join(first)), read(&out.join(second)));
}

#[test]
fn same_named_tests_each_run_their_own_code() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("tests.fe");
    // If `b::t` ran the code of `a::t`, it would not revert.
    fs::write(
        &file,
        "mod a {\n    #[test]\n    fn t() {\n        assert!(true)\n    }\n}\n\n\
         mod b {\n    #[test(should_revert)]\n    fn t() {\n        assert!(false)\n    }\n}\n",
    )
    .unwrap();
    let (output, code) = run_fe(&["test"], &[&file]);
    assert_eq!(code, 0, "fe test failed:\n{output}");
    assert!(output.contains("2 passed"), "{output}");
}
