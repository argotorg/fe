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

.PHONY: foundry-abi-generate
foundry-abi-generate:
	python3 benchmarks/foundry-abi/scripts/generate_matrix.py

.PHONY: foundry-abi-build-fe
foundry-abi-build-fe: foundry-abi-generate
	cargo run --release -q -p fe -- build --backend sonatina --contract AbiRoundtripFe --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/AbiRoundtrip.fe

.PHONY: foundry-abi-test
foundry-abi-test: foundry-abi-build-fe-all
	forge test --root benchmarks/foundry-abi --offline

.PHONY: foundry-abi-gas
foundry-abi-gas: foundry-abi-build-fe-all
	python3 benchmarks/foundry-abi/scripts/run_gas_report.py

.PHONY: foundry-abi-build-fe-all
foundry-abi-build-fe-all: foundry-abi-build-fe foundry-abi-build-fe-dyn foundry-abi-build-fe-bytes foundry-abi-build-fe-deep foundry-abi-build-fe-fixed foundry-abi-build-fe-ceiling foundry-abi-build-fe-nested

.PHONY: foundry-abi-build-fe-dyn
foundry-abi-build-fe-dyn:
	cargo run --release -q -p fe -- build --backend sonatina --contract DynArraySuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/DynArraySuite.fe

.PHONY: foundry-abi-test-dyn
foundry-abi-test-dyn: foundry-abi-build-fe-dyn
	forge test --root benchmarks/foundry-abi --offline --match-path test/DynArraySuiteEquivalence.t.sol

.PHONY: foundry-abi-gas-dyn
foundry-abi-gas-dyn: foundry-abi-build-fe-dyn
	forge test --root benchmarks/foundry-abi --offline --match-path test/DynArraySuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-build-fe-bytes
foundry-abi-build-fe-bytes:
	cargo run --release -q -p fe -- build --backend sonatina --contract BytesSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/BytesSuite.fe

.PHONY: foundry-abi-test-bytes
foundry-abi-test-bytes: foundry-abi-build-fe-bytes
	forge test --root benchmarks/foundry-abi --offline --match-path test/BytesSuiteEquivalence.t.sol

.PHONY: foundry-abi-gas-bytes
foundry-abi-gas-bytes: foundry-abi-build-fe-bytes
	forge test --root benchmarks/foundry-abi --offline --match-path test/BytesSuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-build-fe-deep
foundry-abi-build-fe-deep:
	cargo run --release -q -p fe -- build --backend sonatina --contract DeepDynamicSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/DeepDynamicSuite.fe

.PHONY: foundry-abi-build-fe-fixed
foundry-abi-build-fe-fixed:
	cargo run --release -q -p fe -- build --backend sonatina --contract FixedArraySuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/FixedArraySuite.fe

.PHONY: foundry-abi-build-fe-ceiling
foundry-abi-build-fe-ceiling:
	cargo run --release -q -p fe -- build --backend sonatina --contract FixedArrayCeilingSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/FixedArrayCeilingSuite.fe

.PHONY: foundry-abi-build-fe-nested
foundry-abi-build-fe-nested:
	cargo run --release -q -p fe -- build --backend sonatina --contract NestedTupleSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/NestedTupleSuite.fe

.PHONY: foundry-abi-test-deep
foundry-abi-test-deep: foundry-abi-build-fe-deep
	forge test --root benchmarks/foundry-abi --offline --match-path test/DeepDynamicSuiteEquivalence.t.sol

.PHONY: foundry-abi-test-fixed
foundry-abi-test-fixed: foundry-abi-build-fe-fixed
	forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArraySuiteEquivalence.t.sol

.PHONY: foundry-abi-test-ceiling
foundry-abi-test-ceiling: foundry-abi-build-fe-ceiling
	forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol

.PHONY: foundry-abi-test-nested
foundry-abi-test-nested: foundry-abi-build-fe-nested
	forge test --root benchmarks/foundry-abi --offline --match-path test/NestedTupleSuiteEquivalence.t.sol

.PHONY: foundry-abi-gas-deep
foundry-abi-gas-deep: foundry-abi-build-fe-deep
	forge test --root benchmarks/foundry-abi --offline --match-path test/DeepDynamicSuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-gas-fixed
foundry-abi-gas-fixed: foundry-abi-build-fe-fixed
	forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArraySuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-gas-ceiling
foundry-abi-gas-ceiling: foundry-abi-build-fe-ceiling
	forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-gas-nested
foundry-abi-gas-nested: foundry-abi-build-fe-nested
	forge test --root benchmarks/foundry-abi --offline --match-path test/NestedTupleSuiteEquivalence.t.sol --gas-report

.PHONY: foundry-abi-stress-dyn
foundry-abi-stress-dyn: foundry-abi-build-fe-dyn
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-20000} --match-path test/DynArraySuiteEquivalence.t.sol

.PHONY: foundry-abi-stress-bytes
foundry-abi-stress-bytes: foundry-abi-build-fe-bytes
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-20000} --match-path test/BytesSuiteEquivalence.t.sol

.PHONY: foundry-abi-stress-deep
foundry-abi-stress-deep: foundry-abi-build-fe-deep
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-20000} --match-path test/DeepDynamicSuiteEquivalence.t.sol

.PHONY: foundry-abi-stress-fixed
foundry-abi-stress-fixed: foundry-abi-build-fe-fixed
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-5000} --match-path test/FixedArraySuiteEquivalence.t.sol

.PHONY: foundry-abi-stress-ceiling
foundry-abi-stress-ceiling: foundry-abi-build-fe-ceiling
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-2000} --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol

.PHONY: foundry-abi-stress-nested
foundry-abi-stress-nested: foundry-abi-build-fe-nested
	forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs $${FUZZ_RUNS:-5000} --match-path test/NestedTupleSuiteEquivalence.t.sol

README.md: src/main.rs
	cargo readme --no-title --no-indent-headings > README.md

notes:
	towncrier build --yes --version $(version)
	git commit -m "Compile release notes"

release:
	# Ensure release notes where generated before running the release command
	./newsfragments/validate_files.py is-empty
	cargo release $(version) --execute --all --no-tag --no-push
	# Run the tests again because we may have to adjust some based on the update version
	cargo test --workspace

push-tag:
	# Run `make release version=<version>` first
	./newsfragments/validate_files.py is-empty
	# Tag the release with the current version number
	git tag "v$$(cargo pkgid fe | cut -d# -f2 | cut -d: -f2)"
	git push --tags upstream
