#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
HELPER="$ROOT_DIR/benchmarks/foundry-abi/scripts/run_hevm_equivalence.sh"
REPORT_PATH="${1:-$ROOT_DIR/benchmarks/foundry-abi/reports/hevm-equivalence-status.md}"
ADJACENT_HEVM_BIN="$ROOT_DIR/../hevm/local/bin"

if [ -d "$ADJACENT_HEVM_BIN" ]; then
  PATH="$ADJACENT_HEVM_BIN:$PATH"
fi

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

PASS_FILE="$TMP_DIR/pass.txt"
UNSUPPORTED_FILE="$TMP_DIR/unsupported.txt"
PARTIAL_FILE="$TMP_DIR/partial.txt"
SUSPECT_FILE="$TMP_DIR/suspect.txt"
FAIL_FILE="$TMP_DIR/fail.txt"

: > "$PASS_FILE"
: > "$UNSUPPORTED_FILE"
: > "$PARTIAL_FILE"
: > "$SUSPECT_FILE"
: > "$FAIL_FILE"

pass_count=0
unsupported_count=0
partial_count=0
suspect_count=0
fail_count=0

run_case() {
  local category="$1"
  local fe_runtime="$2"
  local sol_artifact="$3"
  local signature="$4"
  local timeout_secs="$5"
  shift 5

  local out_file
  out_file="$(mktemp "$TMP_DIR/case.XXXXXX")"

  set +e
  timeout "${timeout_secs}s" "$HELPER" "$fe_runtime" "$sol_artifact" "$signature" "$@" >"$out_file" 2>&1
  local status=$?
  set -e

  local line="- \`$category\`: \`$signature\`"

  if grep -q '\[PASS\]' "$out_file"; then
    printf '%s\n' "$line" >> "$PASS_FILE"
    pass_count=$((pass_count + 1))
    return
  fi

  if grep -q 'TODO: symbolic abi encoding' "$out_file" || grep -q 'unable to parse function signature' "$out_file" || grep -q 'unable to parse solc output' "$out_file" || grep -q 'ParserError:' "$out_file"; then
    printf '%s\n' "$line" >> "$UNSUPPORTED_FILE"
    unsupported_count=$((unsupported_count + 1))
    return
  fi

  if [ "$status" -eq 124 ] || grep -q 'partially explore' "$out_file" || grep -q 'Max iterations reached' "$out_file"; then
    printf '%s\n' "$line" >> "$PARTIAL_FILE"
    partial_count=$((partial_count + 1))
    return
  fi

  if grep -q 'Calldata:' "$out_file" && grep -q '0x00' "$out_file"; then
    printf '%s\n' "$line" >> "$SUSPECT_FILE"
    suspect_count=$((suspect_count + 1))
    return
  fi

  printf '%s\n' "$line" >> "$FAIL_FILE"
  fail_count=$((fail_count + 1))
}

mapfile -t scalar_sigs < <(
  rg -o 'sol\("[^"]+"\)' "$ROOT_DIR/benchmarks/foundry-abi/fe/AbiRoundtrip.fe" \
    | sed 's/.*sol("//; s/").*//' \
    | grep -v '\[' \
    | grep -v 'string' \
    | grep -v 'bytes' \
    | grep -v 'Pair' \
    | grep -v 'Triple'
)

for sig in "${scalar_sigs[@]}"; do
  run_case \
    "AbiRoundtrip scalars" \
    "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
    "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
    "$sig" \
    30 \
    --solver z3 --smttimeout 30 --max-iterations 8
done

run_case \
  "FixedArraySuite static" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoUintArray8(uint256[8] calldata)' \
  60 \
  --solver z3 --smttimeout 30 --max-iterations 32

run_case \
  "FixedArraySuite static" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoUintArray16(uint256[16] calldata)' \
  60 \
  --solver z3 --smttimeout 30 --max-iterations 64

run_case \
  "FixedArraySuite static" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoUintArray32(uint256[32] calldata)' \
  120 \
  --solver z3 --smttimeout 30 --max-iterations 64

run_case \
  "FixedArraySuite static" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoNestedUintArray2x5(uint256[5][2] calldata)' \
  120 \
  --solver z3 --smttimeout 30 --max-iterations 64

run_case \
  "FixedArraySuite bool bug" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoBoolArray5(bool[5] calldata)' \
  30 \
  --solver z3 --smttimeout 30 --max-iterations 32

run_case \
  "FixedArraySuite bool timeout" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoBoolArray17(bool[17] calldata)' \
  60 \
  --solver z3 --smttimeout 30 --max-iterations 96

run_case \
  "AbiRoundtrip dynamic" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoString(string memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8 --max-buf-size 8

run_case \
  "AbiRoundtrip dynamic" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoUintArray(uint256[] memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8 --max-buf-size 8

run_case \
  "BytesSuite dynamic" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/BytesSuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/BytesSuiteSol.sol/BytesSuiteSol.json" \
  'echoBytes(bytes memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8 --max-buf-size 8

run_case \
  "FixedArraySuite dynamic fixed-array" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json" \
  'echoStringArray5(string[5] calldata)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8 --max-buf-size 8

run_case \
  "AbiRoundtrip tuples" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoBoolAddressPair((bool,address) memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8

run_case \
  "AbiRoundtrip tuples" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoUint24Int40Pair((uint24,int40) memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8

run_case \
  "AbiRoundtrip tuples" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoBoolAddressU256Triple((bool,address,uint256) memory)' \
  15 \
  --solver z3 --smttimeout 10 --max-iterations 8

run_case \
  "AbiRoundtrip tuple arrays" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoBoolAddressPairArray4((bool,address)[4] calldata)' \
  30 \
  --solver z3 --smttimeout 20 --max-iterations 16

run_case \
  "AbiRoundtrip tuple arrays" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoUint24Int40PairArray4((uint24,int40)[4] calldata)' \
  30 \
  --solver z3 --smttimeout 20 --max-iterations 16

run_case \
  "AbiRoundtrip tuple arrays" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/AbiRoundtripFe.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/AbiRoundtripSol.sol/AbiRoundtripSol.json" \
  'echoBoolAddressU256TripleArray4((bool,address,uint256)[4] calldata)' \
  30 \
  --solver z3 --smttimeout 20 --max-iterations 16

run_case \
  "NestedTupleSuite tuples" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/NestedTupleSuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/NestedTupleSuiteSol.sol/NestedTupleSuiteSol.json" \
  'echoNestedStatic(((bool,address),uint256) memory)' \
  30 \
  --solver z3 --smttimeout 20 --max-iterations 16

run_case \
  "NestedTupleSuite tuples" \
  "$ROOT_DIR/benchmarks/foundry-abi/fe-out/NestedTupleSuite.runtime.bin" \
  "$ROOT_DIR/benchmarks/foundry-abi/out/NestedTupleSuiteSol.sol/NestedTupleSuiteSol.json" \
  'echoNestedStaticFlipped((bool,(address,uint256)) memory)' \
  30 \
  --solver z3 --smttimeout 20 --max-iterations 16

{
  report_date="$(date +%Y-%m-%d)"
  hevm_version="$(hevm version 2>/dev/null | awk 'NR==1 {print $1}')"
  z3_version="$(z3 -version 2>/dev/null | awk 'NR==1 {print $3}')"
  # When committing the generated report, we often want a stable source revision
  # (e.g. the last "source" commit in a stack) rather than the commit that
  # contains the report itself.
  fe_opt_rev_input="${FE_OPT_REV:-}"
  if [ -n "$fe_opt_rev_input" ]; then
    fe_opt_rev="$(git -C "$ROOT_DIR" rev-parse --short "$fe_opt_rev_input" 2>/dev/null || printf '%s' "$fe_opt_rev_input")"
  else
    fe_opt_rev="$(git -C "$ROOT_DIR" rev-parse --short HEAD 2>/dev/null || true)"
  fi

  printf '# hevm Equivalence Status\n\n'
  printf 'Date: %s\n\n' "$report_date"
  printf 'Environment:\n'
  printf -- '- `hevm %s`\n' "$hevm_version"
  printf -- '- `z3 %s`\n' "$z3_version"
  printf -- '- `fe-opt %s`\n' "$fe_opt_rev"
  printf -- '- local install from the adjacent `hevm` checkout at `../hevm/local/bin`\n\n'

  printf 'Semantics:\n'
  printf -- '- hevm checks matching success/failure, returndata, and storage.\n'
  printf -- '- if both sides revert, hevm compares the revert payloads, not just the fact of reversion.\n'
  printf -- '- logs and gas are not part of the equivalence relation.\n\n'

  printf 'Helpers:\n'
  printf -- '- `benchmarks/foundry-abi/scripts/run_hevm_equivalence.sh`\n'
  printf -- '- `benchmarks/foundry-abi/scripts/run_hevm_curated_matrix.sh`\n\n'

  printf 'Summary:\n'
  printf -- '- proven equivalent: %d\n' "$pass_count"
  printf -- '- unsupported by current hevm parser/encoder: %d\n' "$unsupported_count"
  printf -- '- partial or timed out: %d\n' "$partial_count"
  printf -- '- suspicious malformed-calldata mismatches: %d\n' "$suspect_count"
  printf -- '- other failures: %d\n\n' "$fail_count"

  printf '## Proven Equivalent\n\n'
  cat "$PASS_FILE"
  printf '\n## Unsupported in hevm 0.57.0\n\n'
  cat "$UNSUPPORTED_FILE"
  printf '\n## Partial / Timeout\n\n'
  cat "$PARTIAL_FILE"
  printf '\n## Suspected hevm Front-End Bugs\n\n'
  cat "$SUSPECT_FILE"
  printf '\n## Other Failures\n\n'
  cat "$FAIL_FILE"
  printf '\n'
} > "$REPORT_PATH"

printf 'wrote %s\n' "$REPORT_PATH"
