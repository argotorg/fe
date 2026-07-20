#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
ADJACENT_HEVM_BIN="$ROOT_DIR/../hevm/local/bin"

usage() {
  cat <<'EOF'
Usage:
  run_hevm_equivalence.sh <fe-runtime-bin> <sol-artifact-json> <signature> [hevm args...]
  run_hevm_equivalence.sh --raw <fe-runtime-bin> <sol-artifact-json> [hevm args...]

Examples:
  benchmarks/foundry-abi/scripts/run_hevm_equivalence.sh \
    benchmarks/foundry-abi/fe-out/FixedArraySuite.runtime.bin \
    benchmarks/foundry-abi/out/FixedArraySuiteSol.sol/FixedArraySuiteSol.json \
    'echoBoolArray4(bool[4] calldata)' \
    --solver z3 --smttimeout 30 --max-iterations 8

  benchmarks/foundry-abi/scripts/run_hevm_equivalence.sh --raw \
    benchmarks/foundry-abi/fe-out/BytesSuite.runtime.bin \
    benchmarks/foundry-abi/out/BytesSuiteSol.sol/BytesSuiteSol.json \
    --solver z3 --smttimeout 5 --max-iterations 4 --max-buf-size 6
EOF
}

require_tool() {
  if ! command -v "$1" >/dev/null 2>&1; then
    printf 'missing required tool: %s\n' "$1" >&2
    exit 2
  fi
}

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
  usage
  exit 0
fi

raw_mode=0
if [ "${1:-}" = "--raw" ]; then
  raw_mode=1
  shift
fi

if [ "$raw_mode" -eq 1 ]; then
  if [ "$#" -lt 2 ]; then
    usage >&2
    exit 2
  fi
  fe_runtime_bin="$1"
  sol_artifact_json="$2"
  shift 2
  signature=""
else
  if [ "$#" -lt 3 ]; then
    usage >&2
    exit 2
  fi
  fe_runtime_bin="$1"
  sol_artifact_json="$2"
  signature="$3"
  shift 3
fi

require_tool jq

if [ -d "$ADJACENT_HEVM_BIN" ]; then
  PATH="$ADJACENT_HEVM_BIN:$PATH"
fi

require_tool hevm
require_tool z3

tmp_sol_runtime="$(mktemp)"
trap 'rm -f "$tmp_sol_runtime"' EXIT

jq -r '.deployedBytecode.object' "$sol_artifact_json" > "$tmp_sol_runtime"

cmd=(
  hevm
  equivalence
  --code-a-file "$fe_runtime_bin"
  --code-b-file "$tmp_sol_runtime"
)

if [ "$raw_mode" -eq 0 ]; then
  cmd+=(--sig "$signature")
fi

cmd+=("$@")

printf 'running:'
printf ' %q' "${cmd[@]}"
printf '\n'

"${cmd[@]}"
