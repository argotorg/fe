#!/usr/bin/env bash
# Builds the programs of the WebAssembly target, validates every module, checks
# the modules against their manifests, checks that builds are reproducible, and
# runs the Node tests and `fe test --backend wasm`.
#
#   examples/wasm/check.sh [FE_BINARY]
#
# FE_BINARY defaults to target/release/fe and must be built with
# `--features wasm,cranelift` (the native backend is used for one comparison;
# without it that step is left out). Requires Node.js 20 or later and wasm-tools.
# Exits with status 1 at the first failing step.

set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
fe=$(realpath "${1:-$root/target/release/fe}")
fixtures="$root/crates/fe/tests/fixtures"

for tool in node wasm-tools; do
  command -v "$tool" >/dev/null || { echo "error: \`$tool\` is required but was not found on PATH" >&2; exit 1; }
done

work=$(mktemp -d "${TMPDIR:-/tmp}/fe-wasm-check.XXXXXX")
trap 'rm -rf "$work"' EXIT

echo "== Node host tests"
node --test "$here/host.test.mjs"

echo "== build and validate"
programs=(
  "$fixtures"/wasm/*.fe
  "$fixtures"/wasm_manifest/shapes.fe
  "$fixtures"/wasm_files/forge.fe
  "$root/examples/wasm/lines.fe"
  "$root/examples/wasm_host.fe"
)
count=0
for source in "${programs[@]}"; do
  stem=$(basename "$source" .fe)
  for level in 0 s; do
    out="$work/build-$level"
    "$fe" build --backend wasm -O "$level" --emit wasm,ir --out-dir "$out" "$source" >/dev/null
    wasm-tools validate "$out/$stem.wasm"
    # The manifest names the module it describes, and the host accepts the pair.
    node "$here/verify.mjs" "$out/$stem.wasm" "$out/$stem.manifest.json" >/dev/null
    count=$((count + 1))
  done
done
echo "$count modules built, validated and verified against their manifests"

echo "== a module that imports something the host lacks is refused, naming it"
"$fe" build --backend wasm --out-dir "$work/custom" "$fixtures/wasm_manifest/custom_import.fe" >/dev/null
wasm-tools validate "$work/custom/custom_import.wasm"
if refusal=$(node "$here/verify.mjs" "$work/custom/custom_import.wasm" 2>&1); then
  echo "error: the host accepted a module with an import it does not provide" >&2
  exit 1
fi
case "$refusal" in
  *"env.custom_import"*"does not provide"*) echo "$refusal" ;;
  *) echo "error: unexpected refusal: $refusal" >&2; exit 1 ;;
esac

echo "== reproducibility"
"$here/reproducible.sh" "$fe" \
  "$fixtures/wasm/struct_complex.fe" "$fixtures/wasm/vec_arena.fe" "$fixtures/wasm/const_crc.fe" \
  "$fixtures/wasm_manifest/shapes.fe" "$root/examples/wasm/lines.fe"

echo "== fe test --backend wasm"
"$fe" test --backend wasm --color never "$here/tests.fe"
build_help=$("$fe" build --help)
if [[ "$build_help" == *native* && "$(uname -s)-$(uname -m)" == "Linux-x86_64" ]]; then
  echo "== the same tests on the native backend"
  "$fe" test --backend native --color never "$here/tests.fe"
fi

echo "all checks passed"
