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
		/bin/bash -c "rustup target add wasm32-unknown-unknown && cargo test -p fe-common -p fe-parser -p fe-hir -p fe-hir-analysis --target wasm32-unknown-unknown"

.PHONY: check-wasm
check-wasm:
	@echo "Checking core crates for wasm32-unknown-unknown..."
	cargo check -p fe-common -p fe-parser -p fe-hir -p fe-hir-analysis --target wasm32-unknown-unknown
	@echo "Core crates support wasm32-unknown-unknown"

.PHONY: check-wasi
check-wasi:
	@echo "Checking filesystem-dependent crates for wasm32-wasip1..."
	cargo check -p fe-driver -p fe-resolver -p fe --target wasm32-wasip1
	@echo "Filesystem crates support wasm32-wasip1"

.PHONY: check-wasm-all
check-wasm-all: check-wasm check-wasi
	@echo "All WASM/WASI checks passed"

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

notes:
	towncrier build --yes --version $(version)
	git commit -m "Compile release notes"

release:
	# Ensure release notes where generated before running the release command
	./newsfragments/validate_files.py is-empty
	cargo release $(version) --execute --all --skip-tag --skip-push
	# Run the tests again because we may have to adjust some based on the update version
	cargo test --workspace --features solc-backend

push-tag:
	# Run `make release <version>` first
	./newsfragments/validate_files.py is-empty
	# Tag the release with the current version number
	git tag "v$$(cargo pkgid fe | cut -d# -f2 | cut -d: -f2)"
	git push --tags upstream master
