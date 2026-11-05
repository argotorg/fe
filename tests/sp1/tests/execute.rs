use std::{env, fs, path::PathBuf, process::Command, sync::OnceLock};

use sp1_sdk::{
    Elf, ProvingKey,
    blocking::{ProveRequest, Prover, ProverClient, SP1Stdin},
};

fn compile(source: &str, level: u8) -> Elf {
    let binary = PathBuf::from(
        env::var_os("FE_SP1_BINARY").expect("set FE_SP1_BINARY to Fe built with --features sp1"),
    );
    let binary = binary.canonicalize().expect("Fe binary must exist");
    let temp = tempfile::tempdir().unwrap();
    let file = temp.path().join("guest.fe");
    let out = temp.path().join("out");
    fs::write(&file, source).unwrap();
    let result = Command::new(binary)
        .args([
            "build",
            "--backend",
            "sp1",
            "--emit",
            "elf,ir",
            "-O",
            &level.to_string(),
            "--out-dir",
        ])
        .arg(&out)
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "Fe SP1 build failed:\n{}\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    let ir = fs::read_to_string(out.join("guest.sp1.sona")).unwrap();
    assert!(ir.contains("riscv64im-succinct-zkvm-elf"));
    fs::read(out.join("guest.elf")).unwrap().into()
}

fn programs() -> &'static [Elf; 2] {
    static PROGRAMS: OnceLock<[Elf; 2]> = OnceLock::new();
    PROGRAMS.get_or_init(|| {
        [0, 2].map(|level| compile(include_str!("../../../examples/sp1_increment.fe"), level))
    })
}

fn input(value: u32, wide: u64) -> SP1Stdin {
    let mut stdin = SP1Stdin::new();
    stdin.write_slice(&value.to_le_bytes());
    stdin.write_slice(&wide.to_le_bytes());
    stdin
}

#[test]
fn executes_checked_io_and_rejects_overflow_and_bad_input() {
    let client = ProverClient::builder().light().build();
    for elf in programs() {
        for value in [0, 41, i32::MAX as u32, u32::MAX - 1] {
            for wide in [0, i64::MAX as u64, u64::MAX] {
                let (values, report) = client
                    .execute(elf.clone(), input(value, wide))
                    .run()
                    .unwrap();
                assert_eq!(report.exit_code, 0);
                assert_eq!(
                    values.as_slice(),
                    [&(value + 1).to_le_bytes()[..], &wide.to_le_bytes()[..]].concat()
                );
            }
        }
        assert!(
            client
                .execute(elf.clone(), input(u32::MAX, 0))
                .run()
                .is_err()
        );
        let (values, report) = client.execute(elf.clone(), SP1Stdin::new()).run().unwrap();
        assert_ne!(report.exit_code, 0);
        assert!(values.as_slice().is_empty());
        for len in [0, 3, 5, 8] {
            let mut stdin = SP1Stdin::new();
            stdin.write_slice(&vec![0; len]);
            stdin.write_slice(&0u64.to_le_bytes());
            let (values, report) = client.execute(elf.clone(), stdin).run().unwrap();
            assert_ne!(report.exit_code, 0);
            assert!(values.as_slice().is_empty());
        }
        for len in [0, 4, 7, 9] {
            let mut stdin = SP1Stdin::new();
            stdin.write_slice(&41u32.to_le_bytes());
            stdin.write_slice(&vec![0; len]);
            let (values, report) = client.execute(elf.clone(), stdin).run().unwrap();
            assert_ne!(report.exit_code, 0);
            assert!(values.as_slice().is_empty());
        }
    }
}

#[test]
fn preserves_runtime_exit_status() {
    let client = ProverClient::builder().light().build();
    let source = r#"
use std::sp1::{ProverInput, read_u32, sp1}
pub fn main() -> i32 {
    with (ProverInput = sp1()) { read_u32().downcast_truncate() }
}
"#;
    for level in [0, 2] {
        let elf = compile(source, level);
        for (value, expected) in [(0u32, 0), (7, 7), (u32::MAX, 255), (256, 0)] {
            let mut stdin = SP1Stdin::new();
            stdin.write_slice(&value.to_le_bytes());
            let (values, report) = client.execute(elf.clone(), stdin).run().unwrap();
            assert_eq!(report.exit_code, expected);
            assert!(values.as_slice().is_empty());
        }
    }
}

#[test]
fn executes_wrapping_division_and_bounded_power_across_integer_widths() {
    let mut source =
        String::from("use std::sp1::{ProverInput, PublicValues, read_u32, commit_u32, sp1}\n");
    for bits in [8, 16, 32, 64, 128, 256] {
        for prefix in ["u", "i"] {
            let ty = format!("{prefix}{bits}");
            source.push_str(&format!("#[arithmetic(unchecked)]\nfn wrap_{ty}(base: {ty}, exponent: {ty}, zero: {ty}) -> bool {{ base ** exponent == 0 && base / zero == 0 && base % zero == 0 }}\nfn checked_{ty}(base: {ty}, exponent: {ty}) -> {ty} {{ base ** exponent }}\n"));
        }
    }
    source.push_str("pub fn main() -> i32 {\nwith (ProverInput = sp1(), PublicValues = sp1()) {\nlet base = read_u32()\nlet zero = read_u32()\n");
    for bits in [8, 16, 32, 64, 128, 256] {
        for prefix in ["u", "i"] {
            let ty = format!("{prefix}{bits}");
            let exp_bit = if prefix == "u" { bits - 1 } else { bits - 2 };
            source.push_str(&format!("if !wrap_{ty}(base: base.downcast_truncate(), exponent: {bits}, zero: zero.downcast_truncate()) {{ return 1 }}\nif checked_{ty}(base: (base - 1).downcast_truncate(), exponent: 1 << {exp_bit}) != 1 {{ return 2 }}\nif checked_{ty}(base: base.downcast_truncate(), exponent: 5) != 32 {{ return 3 }}\n"));
        }
    }
    source.push_str("commit_u32(base)\n}\n0\n}\n");
    let client = ProverClient::builder().light().build();
    for level in [0, 2] {
        let elf = compile(&source, level);
        let mut stdin = SP1Stdin::new();
        stdin.write_slice(&2u32.to_le_bytes());
        stdin.write_slice(&0u32.to_le_bytes());
        let (values, report) = client.execute(elf, stdin).run().unwrap();
        assert_eq!(report.exit_code, 0);
        assert_eq!(values.as_slice(), 2u32.to_le_bytes());
    }
}

#[test]
#[ignore = "CPU core proof; run explicitly in the SP1 integration job"]
fn proves_and_verifies_fe_guest_and_rejects_failure() {
    let client = ProverClient::builder().cpu().build();
    let pk = client.setup(programs()[1].clone()).unwrap();
    let proof = client.prove(&pk, input(41, u64::MAX)).core().run().unwrap();
    assert_eq!(
        proof.public_values.as_slice(),
        [&42u32.to_le_bytes()[..], &u64::MAX.to_le_bytes()[..]].concat()
    );
    client.verify(&proof, pk.verifying_key(), None).unwrap();
    assert!(client.prove(&pk, input(u32::MAX, 0)).core().run().is_err());
    if let Ok(proof) = client.prove(&pk, SP1Stdin::new()).core().run() {
        assert!(client.verify(&proof, pk.verifying_key(), None).is_err());
    }
}
