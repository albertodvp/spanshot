set working-directory := "hs-spanshot"

# List available recipes
default:
    @just --list

# Build the project (compiles binaries to dist-newstyle/)
build:
    cabal build

# Run all tests (unit + integration) on latest built binaries
test: build
    cabal test hs-spanshot-test
    cabal test hs-spanshot-integration-cli

# Run only unit tests
test-unit: build
    cabal test hs-spanshot-test

# Run only CLI integration tests (uses binary from dist-newstyle/)
test-integration-cli: build
    cabal test hs-spanshot-integration-cli

# Get the path to the compiled binary in dist-newstyle/
bin-path:
    @cabal list-bin spanshot

# Collect logs from a specific file using the built binary
collect-file FILE:
    $(just bin-path) collect --logfile {{FILE}}

# Capture errors from a specific file using the built binary
capture-file FILE PATTERN:
    $(just bin-path) capture --logfile {{FILE}} --regex-pattern "{{PATTERN}}"

# Run full pipeline on a specific file
run-file FILE PATTERN:
    $(just bin-path) run --logfile {{FILE}} --regex-pattern "{{PATTERN}}"

# Demo: capture errors from python test fixture
demo-capture:
    timeout 3s $(just bin-path) capture --logfile test/fixtures/python_errors.log --regex-pattern "ERROR" --pre-window 2 --post-window 2 || true

# Demo: run full pipeline on java test fixture
demo-run:
    timeout 3s $(just bin-path) run --logfile test/fixtures/java_errors.log --regex-pattern "ERROR|Exception" || true

# Generate golden files from current binary output (overwrites existing goldens)
generate-golden: build
    #!/usr/bin/env bash
    set -euo pipefail
    BINARY=$(just bin-path)
    echo "Generating golden files using binary: $BINARY"
    
    # Generate golden for small.log (5 lines)
    timeout 2s $BINARY collect --logfile test/fixtures/small.log > test/golden/small.golden.jsonl || true
    
    # Generate golden for empty.log (0 lines, immediate EOF)
    timeout 1s $BINARY collect --logfile test/fixtures/empty.log > test/golden/empty.golden.jsonl || true
    
    # Generate golden for long_file.log (1133 lines)
    timeout 5s $BINARY collect --logfile test/fixtures/long_file.log > test/golden/long_file.golden.jsonl || true
    
    echo "Golden files generated in test/golden/"

# Clean build artifacts (removes dist-newstyle/)
clean:
    cabal clean
    rm -rf dist-newstyle

# Clean test artifacts (removes .actual files and temp logs)
clean-test:
    rm -f test/golden/*.actual
    find /tmp -name 'stream_test*.log' -delete 2>/dev/null || true
