#!/usr/bin/env bash
# Checks that `fe build --backend wasm` gives identical bytes when only the
# surroundings of the build change.
#
#   examples/wasm/reproducible.sh FE_BINARY FILE.fe...
#
# Every file is built at the optimization levels 0, 1, 2 and s, twice:
#
#   - build 1: working directory and source in one directory, output in `out`;
#   - build 2: a different working directory, a source path that differs in
#     length and directory names, an old modification time for the source, a
#     different output directory, an empty environment apart from PATH, another
#     time zone and locale, and (where `setarch` exists) address space
#     randomization switched off.
#
# The module (`.wasm`), its Sonatina IR (`.wasm.sona`) and the manifest
# (`.manifest.json`) of both builds must be identical. The script prints one line
# per difference and exits with status 1 if there is any.
#
# Set REPRO_PRELOAD to a shared library to load into build 2 (for example a
# shim that moves the clock) and REPRO_ENV to extra NAME=value words for its
# environment. Set REPRO_JOBS to the number of builds that run
# at once (default 4).

set -u

if [ "$#" -lt 2 ]; then
  echo "usage: $0 FE_BINARY FILE.fe..." >&2
  exit 2
fi

fe=$(realpath "$1")
shift
work=$(mktemp -d "${TMPDIR:-/tmp}/fe-repro.XXXXXX")
trap 'rm -rf "$work"' EXIT
jobs=${REPRO_JOBS:-4}
preload=${REPRO_PRELOAD:-}
extra_env=${REPRO_ENV:-}

norandom=""
if command -v setarch >/dev/null 2>&1 && setarch "$(uname -m)" -R true 2>/dev/null; then
  norandom="setarch $(uname -m) -R"
fi

# One file at one level. Prints "same" or the names of the files that differ.
check_one() {
  local src=$1 level=$2
  local name
  name=$(basename "$src" .fe)
  local first="$work/one/$name-$level"
  local second="$work/two-with-a-longer-directory-name/nested/$name-$level"
  mkdir -p "$first/src" "$second/deeper/src"
  cp "$src" "$first/src/$name.fe"
  cp "$src" "$second/deeper/src/$name.fe"
  touch -d '2001-02-03 04:05:06' "$second/deeper/src/$name.fe"
  if ! (cd "$first" && "$fe" build --backend wasm -O "$level" --emit wasm,ir \
        --out-dir out "src/$name.fe") >"$first/log" 2>&1; then
    echo "$name -O$level: build 1 failed: $(tail -n 1 "$first/log")"
    return
  fi
  if ! (cd / && env -i PATH="$PATH" HOME="$second" TZ=Pacific/Kiritimati LC_ALL=C \
        ${preload:+LD_PRELOAD="$preload"} $extra_env $norandom "$fe" build --backend wasm \
        -O "$level" --emit wasm,ir --out-dir "$second/elsewhere" \
        "$second/deeper/src/$name.fe") >"$second/log" 2>&1; then
    echo "$name -O$level: build 2 failed: $(tail -n 1 "$second/log")"
    return
  fi
  local bad=""
  for ext in wasm wasm.sona manifest.json; do
    local a="$first/out/$name.$ext" b="$second/elsewhere/$name.$ext"
    if [ ! -e "$a" ] && [ ! -e "$b" ]; then continue; fi
    cmp -s "$a" "$b" || bad="$bad $name.$ext"
  done
  if [ -n "$bad" ]; then
    echo "$name -O$level: differs:$bad"
  fi
  rm -rf "$first" "$second"
}

export -f check_one
export fe work preload extra_env
export norandom

total=0
{
  for src in "$@"; do
    for level in 0 1 2 s; do
      printf '%s\0%s\0' "$src" "$level"
    done
  done
} | xargs -0 -n 2 -P "$jobs" bash -c 'check_one "$0" "$1"' | sort >"$work/differences"
total=$(($# * 4))

if [ -s "$work/differences" ]; then
  cat "$work/differences"
  echo "reproducibility: FAILED, $(wc -l <"$work/differences") of $total builds differ"
  exit 1
fi
echo "reproducibility: ok, $total builds ($# files at 4 levels) are identical"
