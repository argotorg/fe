use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Once,
};

use tempfile::tempdir;

fn build_fe_binary() {
    static BUILD: Once = Once::new();
    BUILD.call_once(|| {
        let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
        let output = Command::new(&cargo_exe)
            .args(["build", "--bin", "fe"])
            .output()
            .expect("Failed to build fe binary");

        if !output.status.success() {
            panic!(
                "Failed to build fe binary: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    });
}

fn fe_binary_path() -> PathBuf {
    std::env::current_exe()
        .expect("Failed to get current exe")
        .parent()
        .expect("Failed to get parent")
        .parent()
        .expect("Failed to get parent")
        .join("fe")
}

fn render_output(output: &std::process::Output) -> String {
    let mut full_output = String::new();
    if !output.stdout.is_empty() {
        full_output.push_str("=== STDOUT ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        if !full_output.is_empty() {
            full_output.push('\n');
        }
        full_output.push_str("=== STDERR ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stderr));
    }
    let exit_code = output.status.code().unwrap_or(-1);
    full_output.push_str(&format!("\n=== EXIT CODE: {exit_code} ==="));
    full_output
}

fn find_executable_in_path(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join(name);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn run_fe_main_with_env(args: &[&str], extra_env: &[(&str, &str)]) -> (String, i32) {
    build_fe_binary();
    let fe_binary = fe_binary_path();

    let mut command = Command::new(&fe_binary);
    command.args(args).env("NO_COLOR", "1");
    for (key, value) in extra_env {
        command.env(key, value);
    }
    let output = command
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {:?}", args));

    let exit_code = output.status.code().unwrap_or(-1);
    (render_output(&output), exit_code)
}

fn write_foundry_base(root: &Path) -> Result<(), String> {
    fs::create_dir_all(root.join("src")).map_err(|err| format!("create src: {err}"))?;
    fs::create_dir_all(root.join("test")).map_err(|err| format!("create test: {err}"))?;

    let foundry_toml = r#"[profile.default]
src = "src"
test = "test"
fs_permissions = [{ access = "read", path = "./" }]
"#;
    fs::write(root.join("foundry.toml"), foundry_toml)
        .map_err(|err| format!("write foundry.toml: {err}"))?;

    Ok(())
}

fn write_foundry_project(
    root: &Path,
    deploy_rel_path: &str,
    runtime_rel_path: &str,
) -> Result<(), String> {
    let solidity_test = format!(
        r#"// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

interface Vm {{
    function readFile(string calldata path) external returns (string memory);
}}

contract FeBuildArtifactsTest {{
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    function testBuildArtifactsDeployAndRun() public {{
        bytes memory initCode = fromHex(vm.readFile("{deploy_rel_path}"));
        bytes memory expectedRuntime = fromHex(vm.readFile("{runtime_rel_path}"));

        address deployed = deploy(initCode);
        require(deployed != address(0), "create failed");

        (bool ok, bytes memory out) = deployed.staticcall(
            abi.encodeWithSelector(bytes4(0x12345678))
        );
        require(ok, "call failed");

        uint256 value = abi.decode(out, (uint256));
        require(value == 1, "unexpected return");

        bytes memory deployedCode = deployed.code;
        require(
            keccak256(deployedCode) == keccak256(expectedRuntime),
            "runtime mismatch"
        );
    }}

    function deploy(bytes memory initCode) internal returns (address deployed) {{
        assembly {{
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }}
    }}

    function fromHex(string memory s) internal pure returns (bytes memory) {{
        bytes memory strBytes = bytes(s);
        uint256 start = 0;
        while (start < strBytes.length && isWhitespace(strBytes[start])) {{
            start++;
        }}

        if (
            start + 1 < strBytes.length &&
            strBytes[start] == bytes1("0") &&
            (strBytes[start + 1] == bytes1("x") || strBytes[start + 1] == bytes1("X"))
        ) {{
            start += 2;
        }}

        uint256 digits = 0;
        for (uint256 i = start; i < strBytes.length; i++) {{
            if (isWhitespace(strBytes[i])) continue;
            digits++;
        }}
        require(digits % 2 == 0, "odd hex length");

        bytes memory out = new bytes(digits / 2);
        uint256 outIndex = 0;
        uint8 high = 0;
        bool highNibble = true;
        for (uint256 i = start; i < strBytes.length; i++) {{
            bytes1 ch = strBytes[i];
            if (isWhitespace(ch)) continue;
            uint8 val = fromHexChar(ch);
            if (highNibble) {{
                high = val;
                highNibble = false;
            }} else {{
                out[outIndex] = bytes1((high << 4) | val);
                outIndex++;
                highNibble = true;
            }}
        }}
        return out;
    }}

    function isWhitespace(bytes1 ch) private pure returns (bool) {{
        return ch == 0x20 || ch == 0x0a || ch == 0x0d || ch == 0x09;
    }}

    function fromHexChar(bytes1 c) private pure returns (uint8) {{
        uint8 b = uint8(c);
        if (b >= 48 && b <= 57) return b - 48;
        if (b >= 65 && b <= 70) return b - 55;
        if (b >= 97 && b <= 102) return b - 87;
        revert("invalid hex");
    }}
}}
"#
    );
    fs::write(root.join("test/FeBuildArtifacts.t.sol"), solidity_test)
        .map_err(|err| format!("write FeBuildArtifacts.t.sol: {err}"))?;

    Ok(())
}

fn write_foundry_project_erc20(
    root: &Path,
    deploy_rel_path: &str,
    runtime_rel_path: &str,
) -> Result<(), String> {
    let solidity_test = format!(
        r#"// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

interface Vm {{
    function readFile(string calldata path) external returns (string memory);
    function prank(address msgSender) external;
    function expectEmit(bool checkTopic1, bool checkTopic2, bool checkTopic3, bool checkData) external;
}}

interface ICoolCoin {{
    function name() external view returns (uint256);
    function symbol() external view returns (uint256);
    function decimals() external view returns (uint8);
    function totalSupply() external view returns (uint256);
    function balanceOf(address account) external view returns (uint256);
    function allowance(address owner, address spender) external view returns (uint256);
    function transfer(address to, uint256 amount) external returns (bool);
    function approve(address spender, uint256 amount) external returns (bool);
    function transferFrom(address from, address to, uint256 amount) external returns (bool);
    function mint(address to, uint256 amount) external returns (bool);
}}

contract FeErc20ArtifactsTest {{
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed owner, address indexed spender, uint256 value);

    function testErc20Artifacts() public {{
        bytes memory initCode = fromHex(vm.readFile("{deploy_rel_path}"));
        bytes memory expectedRuntime = fromHex(vm.readFile("{runtime_rel_path}"));

        address owner = address(0x1000000000000000000000000000000000000001);
        address alice = address(0x1000000000000000000000000000000000000002);
        address bob = address(0x1000000000000000000000000000000000000003);
        address spender = address(0x1000000000000000000000000000000000000004);

        uint256 initialSupply = 1000;
        bytes memory initWithArgs = bytes.concat(initCode, abi.encode(initialSupply, owner));

        address deployed = deploy(initWithArgs);
        require(deployed != address(0), "create failed");

        bytes memory deployedCode = deployed.code;
        require(
            keccak256(deployedCode) == keccak256(expectedRuntime),
            "runtime mismatch"
        );

        ICoolCoin token = ICoolCoin(deployed);
        require(token.totalSupply() == initialSupply, "totalSupply");
        require(token.balanceOf(owner) == initialSupply, "balanceOf(owner)");
        require(token.balanceOf(alice) == 0, "balanceOf(alice)");

        vm.expectEmit(true, true, false, true);
        emit Transfer(owner, alice, 50);
        vm.prank(owner);
        require(token.transfer(alice, 50), "transfer");
        require(token.balanceOf(owner) == 950, "balanceOf(owner) after transfer");
        require(token.balanceOf(alice) == 50, "balanceOf(alice) after transfer");

        vm.expectEmit(true, true, false, true);
        emit Approval(owner, spender, 100);
        vm.prank(owner);
        require(token.approve(spender, 100), "approve");
        require(token.allowance(owner, spender) == 100, "allowance after approve");

        vm.expectEmit(true, true, false, true);
        emit Transfer(owner, bob, 40);
        vm.prank(spender);
        require(token.transferFrom(owner, bob, 40), "transferFrom");
        require(token.allowance(owner, spender) == 60, "allowance after transferFrom");
        require(token.balanceOf(owner) == 910, "balanceOf(owner) after transferFrom");
        require(token.balanceOf(bob) == 40, "balanceOf(bob) after transferFrom");

        vm.prank(spender);
        require(!token.transferFrom(owner, bob, 1000), "transferFrom should fail");
        require(token.allowance(owner, spender) == 60, "allowance unchanged");
        require(token.balanceOf(owner) == 910, "owner unchanged");
        require(token.balanceOf(bob) == 40, "bob unchanged");

        require(token.name() == 0x436f6f6c436f696e, "name");
        require(token.symbol() == 0x434f4f4c, "symbol");
        require(token.decimals() == 18, "decimals");

        vm.expectEmit(true, true, false, true);
        emit Transfer(address(0), alice, 500);
        require(token.mint(alice, 500), "mint");
        require(token.totalSupply() == 1500, "totalSupply after mint");
        require(token.balanceOf(alice) == 550, "balanceOf(alice) after mint");
    }}

    function deploy(bytes memory initCode) internal returns (address deployed) {{
        assembly {{
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }}
    }}

    function fromHex(string memory s) internal pure returns (bytes memory) {{
        bytes memory strBytes = bytes(s);
        uint256 start = 0;
        while (start < strBytes.length && isWhitespace(strBytes[start])) {{
            start++;
        }}

        if (
            start + 1 < strBytes.length &&
            strBytes[start] == bytes1("0") &&
            (strBytes[start + 1] == bytes1("x") || strBytes[start + 1] == bytes1("X"))
        ) {{
            start += 2;
        }}

        uint256 digits = 0;
        for (uint256 i = start; i < strBytes.length; i++) {{
            if (isWhitespace(strBytes[i])) continue;
            digits++;
        }}
        require(digits % 2 == 0, "odd hex length");

        bytes memory out = new bytes(digits / 2);
        uint256 outIndex = 0;
        uint8 high = 0;
        bool highNibble = true;
        for (uint256 i = start; i < strBytes.length; i++) {{
            bytes1 ch = strBytes[i];
            if (isWhitespace(ch)) continue;
            uint8 val = fromHexChar(ch);
            if (highNibble) {{
                high = val;
                highNibble = false;
            }} else {{
                out[outIndex] = bytes1((high << 4) | val);
                outIndex++;
                highNibble = true;
            }}
        }}
        return out;
    }}

    function isWhitespace(bytes1 ch) private pure returns (bool) {{
        return ch == 0x20 || ch == 0x0a || ch == 0x0d || ch == 0x09;
    }}

    function fromHexChar(bytes1 c) private pure returns (uint8) {{
        uint8 b = uint8(c);
        if (b >= 48 && b <= 57) return b - 48;
        if (b >= 65 && b <= 70) return b - 55;
        if (b >= 97 && b <= 102) return b - 87;
        revert("invalid hex");
    }}
}}
"#
    );
    fs::write(root.join("test/FeErc20Artifacts.t.sol"), solidity_test)
        .map_err(|err| format!("write FeErc20Artifacts.t.sol: {err}"))?;

    Ok(())
}

#[cfg(unix)]
#[test]
fn test_fe_build_artifacts_with_foundry() {
    let Some(forge) = find_executable_in_path("forge") else {
        #[allow(clippy::print_stdout)]
        {
            println!("skipping foundry integration test because `forge` is missing");
        }
        return;
    };
    let solc = std::env::var_os("FE_SOLC_PATH")
        .map(PathBuf::from)
        .filter(|path| path.is_file())
        .or_else(|| find_executable_in_path("solc"));
    let Some(solc) = solc else {
        #[allow(clippy::print_stdout)]
        {
            println!("skipping foundry integration test because `solc` is missing");
        }
        return;
    };

    let solc_str = solc.to_str().expect("solc utf8");

    let fixture_basic_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_basic_path_str = fixture_basic_path.to_str().expect("fixture path utf8");
    let fixture_erc20_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/build_foundry/erc20.fe");
    let fixture_erc20_path_str = fixture_erc20_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");

    let forge_root = temp.path().join("forge-project");
    fs::create_dir_all(&forge_root).expect("create forge project root");

    let out_dir = forge_root.join("fe-out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "sonatina",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_basic_path_str,
        ],
        &[],
    );
    assert_eq!(exit_code, 0, "fe build Foo failed:\n{output}");

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "sonatina",
            "--contract",
            "CoolCoin",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_erc20_path_str,
        ],
        &[],
    );
    assert_eq!(exit_code, 0, "fe build CoolCoin failed:\n{output}");

    for artifact in [
        out_dir.join("Foo.bin"),
        out_dir.join("Foo.runtime.bin"),
        out_dir.join("CoolCoin.bin"),
        out_dir.join("CoolCoin.runtime.bin"),
    ] {
        assert!(artifact.is_file(), "expected artifact at {artifact:?}");
    }

    write_foundry_base(&forge_root).expect("write foundry project");
    write_foundry_project(&forge_root, "fe-out/Foo.bin", "fe-out/Foo.runtime.bin")
        .expect("write Foo foundry test");
    write_foundry_project_erc20(
        &forge_root,
        "fe-out/CoolCoin.bin",
        "fe-out/CoolCoin.runtime.bin",
    )
    .expect("write CoolCoin foundry test");

    let foundry_home = forge_root.join("foundry-home");
    fs::create_dir_all(&foundry_home).expect("create foundry home");

    let forge_output = Command::new(&forge)
        .args([
            "test",
            "--root",
            forge_root.to_str().expect("forge root utf8"),
            "--use",
            solc_str,
            "--offline",
            "-q",
        ])
        .env("FOUNDRY_HOME", &foundry_home)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("run forge test");

    assert!(
        forge_output.status.success(),
        "forge test failed:\n{}",
        render_output(&forge_output)
    );
}
