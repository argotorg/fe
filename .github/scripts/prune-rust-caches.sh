#!/usr/bin/env bash
# Deletes all but the newest of master's Rust dependency caches for each job,
# OS and architecture.
#
# rust-cache keys end in a toolchain hash and a lockfile hash, so a change to
# either gives each job a new cache on master while the old one stays. Both sets
# together overflow the repository's cache quota, and GitHub then evicts the
# least recently used caches, often the small tree-sitter and solc ones.
set -euo pipefail

gh cache list --ref refs/heads/master --key v0-rust- --sort created_at --order desc \
  --limit 1000 --json id,key --jq '.[] | "\(.id) \(.key)"' |
  awk '{ job = $2; sub(/-[^-]*-[^-]*$/, "", job); if (seen[job]++) print $1, $2 }' |
  while read -r id key; do
    echo "Deleting $key"
    # Another workflow's run may have deleted it first.
    gh cache delete "$id" || echo "::warning::Could not delete $key"
  done
