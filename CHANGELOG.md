# Changelog

[//]: # (towncrier release notes start)
## 26.0.1 (2026-04-13)

### Bugfixes

- Suppress downstream type mismatch diagnostics when the underlying type is already invalid. This reduces cascading error noise and makes compiler output easier to read. ([#1386](https://github.com/argotorg/fe/issues/1386))
- Fix chained method call type inference (e.g. `result.map(fn1).map(fn2)`). The compiler now correctly unifies types through canonicalized receivers, resolving incorrect type mismatch errors on valid method chains. ([#1389](https://github.com/argotorg/fe/issues/1389))
- Overhaul LSP stability and observability: worker-thread panics now surface as visible errors instead of being silently swallowed, a dual-layer logging system writes detailed diagnostics to workspace-local `.fe-lsp/` log files with automatic rotation and retention, and a dispatch-deadlock in concurrent request handling has been fixed via an upgraded async-lsp dependency. ([#1392](https://github.com/argotorg/fe/issues/1392))

### Internal Changes - for Fe Contributors

- Bump sonatina to eb50941. ([#1393](https://github.com/argotorg/fe/issues/1393))
