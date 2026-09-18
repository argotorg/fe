# Compiler test fixtures

Use `dir_test` with checked-in fixtures for fixed compiler programs and ingot or
workspace layouts. Keep semantic assertions in the test: fixture discovery does
not require snapshots, and a regression must fail if its required compilation or
attribution is missing.

- Obtain source paths and contents from `Fixture`, rather than reconstructing
  paths or duplicating existing fixture files. Convert native absolute paths with
  `Url::from_file_path`; do not manufacture `file:///...` identities.
- Use exact globs for cases with individual expectations. Put specialized fixtures
  in a dedicated subdirectory so broad snapshot globs do not collect them.
- Keep static program variants explicit. Do not depend on multiline replacements
  in `include_str!` data: checkout line endings can make them silently do nothing.
- Match source identities by their actual URLs or keys, not filename suffixes.
  Canonical filesystem paths can have platform-specific spellings, including
  Windows extended-length prefixes. Compare canonicalized paths when appropriate.
- Source-span assertions must use the same bytes supplied to the compiler. A
  newline test may deliberately supply LF and CRLF variants, but must compute its
  expectations separately from each variant. Snapshot normalization is not a
  substitute for testing source offsets.
- Retain in-memory synthetic facts for validator/projection unit tests. Retain
  temporary files when filesystem behavior or mutation is the subject of the
  test; use `tempfile` ownership for cleanup rather than timestamps and manual
  recursive deletion. Honor the execution environment's `TMPDIR` setting.

The Sonatina semantic tests and trace semantic canary in `crates/codegen/tests`,
and the CLI trace emission tests in `crates/fe/src/trace/trace_emit.rs`, demonstrate
these patterns. Existing snapshot tests that skip unsupported cases are not a
template for hard semantic regression checks.
