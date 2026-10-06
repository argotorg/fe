.PHONY: docker-test
docker-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		rustlang/rust:nightly \
		cargo test --workspace

.PHONY: docker-wasm-test
docker-wasm-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		rust:latest \
		/bin/bash -c "rustup target add wasm32-unknown-unknown && cargo test -p fe-common -p fe-parser -p fe-hir --target wasm32-unknown-unknown"

.PHONY: treesitter-generate
treesitter-generate:
	# Generate the tree-sitter parser with the pinned CLI. The generated
	# sources (parser.c, grammar.json, node-types.json, src/tree_sitter/) are
	# not tracked in git; they're regenerated from grammar.js at build time.
	cd crates/tree-sitter-fe && npm ci --ignore-scripts && npm rebuild tree-sitter-cli && npx tree-sitter generate --abi=14

.PHONY: test
test: treesitter-generate
	# Builds and runs the workspace tests, including the tree-sitter grammar
	# test (crates/parser/tests/tree_sitter_parse.rs), which parses every .fe
	# fixture against the freshly generated grammar.
	cargo nextest run --cargo-profile test-release --workspace --all-features --no-fail-fast \
		--exclude fe-bench

.PHONY: check-wasm
check-wasm:
	@echo "Checking core crates for wasm32-unknown-unknown..."
	cargo check -p fe-common -p fe-parser -p fe-hir --target wasm32-unknown-unknown
	@echo "✓ Core crates support wasm32-unknown-unknown"

.PHONY: check-wasi
check-wasi:
	@echo "Checking filesystem-dependent crates for wasm32-wasip1..."
	cargo check -p fe-driver -p fe-resolver --target wasm32-wasip1
	@echo "✓ Filesystem crates support wasm32-wasip1"

.PHONY: check-wasm-all
check-wasm-all: check-wasm check-wasi
	@echo "✓ All WASM/WASI checks passed"

.PHONY: coverage
coverage:
	cargo tarpaulin --workspace --all-features --verbose --timeout 120 --exclude-files 'tests/*' --exclude-files 'main.rs' --out xml html -- --skip differential::

.PHONY: clippy
clippy:
	cargo clippy --workspace --all-targets --all-features -- -D warnings -A clippy::upper-case-acronyms -A clippy::large-enum-variant -W clippy::print_stdout -W clippy::print_stderr

.PHONY: rustfmt
rustfmt:
	cargo fmt --all -- --check

.PHONY: lint
lint: rustfmt clippy

.PHONY: build-docs
build-docs:
	cargo doc --no-deps --workspace

README.md: src/main.rs
	cargo readme --no-title --no-indent-headings > README.md

# eisenbote (https://github.com/fe-lang/eisenbote) assembles the release notes
# from newsfragments/. It is checked out next to this repository and built with
# the Fe compiler about to be released, which also checks the native backend on
# a real program. If that build fails, eisenbote's last working executable is
# downloaded instead. Its settings are in eisenbote.toml.
EISENBOTE ?= ../eisenbote

.PHONY: eisenbote
eisenbote:
	cargo build --release -p fe --features cranelift
	$(MAKE) -C $(EISENBOTE) -B FE=$(CURDIR)/target/release/fe || { \
		echo "Building eisenbote with this Fe failed; using its last working executable."; \
		$(MAKE) -C $(EISENBOTE) download; }

# Any eisenbote executable, for checks that don't need a fresh build.
$(EISENBOTE)/out/eisenbote:
	$(MAKE) -C $(EISENBOTE) download

notes: eisenbote
	$(EISENBOTE)/bin/eisenbote build --yes --version $(version)
	git commit -m "Compile release notes"

.PHONY: release release-test
release: $(EISENBOTE)/out/eisenbote
	# Ensure release notes where generated before running the release command
	$(EISENBOTE)/bin/eisenbote check --empty
	cargo release $(version) --execute --all --no-tag --no-push
	$(MAKE) release-test

# Repeat validation after a version bump without rerunning cargo-release.
release-test:
	# Optimize compiler-heavy tests and give deeply nested type queries enough stack.
	RUST_MIN_STACK=16777216 cargo test --profile test-release --locked --workspace

push-tag: $(EISENBOTE)/out/eisenbote
	# Run `make release version=<version>` first
	$(EISENBOTE)/bin/eisenbote check --empty
	# Tag the release with the current version number
	git tag "v$$(cargo pkgid fe | cut -d# -f2 | cut -d: -f2)"
	git push --tags upstream
