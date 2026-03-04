# Implementation Plan: Capture Phase (v0.1)

**Branch**: `002-capture-phase` | **Date**: 2026-03-04 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-capture-phase/spec.md`

## Summary

Implement the capture phase of SpanShot: a streaming combinator `captureFromStream` that transforms `CollectEvent` streams into `SpanShot` streams, plus CLI commands (`capture`, `run`) for end-user access. The implementation leverages existing pure core functions (`processEvent`, `addToPreWindow`) and streaming infrastructure (`collectFromFile`), adding the stream combinator and CLI integration layer.

## Technical Context

**Language/Version**: Haskell (GHC 9.12+)
**Primary Dependencies**: streaming, streaming-bytestring, aeson, opt-env-conf, regex-tdfa
**Storage**: N/A (file-based streaming, no persistent storage)
**Testing**: hspec + QuickCheck for unit tests, separate CLI integration test suite
**Target Platform**: Linux/macOS CLI
**Project Type**: CLI tool with library core
**Performance Goals**: Process log files up to 100MB without memory issues; first SpanShot output \<1s after error (excluding post-window)
**Constraints**: Memory usage bounded by window sizes (not input size); graceful SIGINT handling
**Scale/Scope**: Single-user CLI tool, streaming processing of arbitrarily large files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. CLI-First Architecture | ✅ PASS | Feature adds `capture` and `run` CLI commands with JSONL output |
| II. Pure Functional Core | ✅ PASS | `captureFromStream` is a pure stream transformer; `processEvent` already pure |
| III. Test-First Development | ✅ PASS | Plan includes unit tests for combinator, integration tests for CLI |
| IV. Nix-Centered Development | ✅ PASS | No new tooling required; uses existing nix develop environment |
| V. Type Safety | ✅ PASS | Uses existing validated types (CaptureOptions via mkCaptureOptions) |
| VI. Clean Code Design | ✅ PASS | Extends existing modules (Capture.hs, Main.hs); no over-engineering |

**Gate Result**: PASS - All principles satisfied. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/002-capture-phase/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (CLI command schemas)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
hs-spanshot/
├── src/
│   ├── Types.hs         # Existing: CollectEvent, SpanShot, CaptureOptions, CaptureState
│   ├── Collect.hs       # MODIFY: Add shutdown MVar check to bytesWithPolling
│   ├── Capture.hs       # MODIFY: Add captureFromStream combinator, finalizeCapture
│   └── Config.hs        # Existing: Configuration loading
├── app/
│   └── Main.hs          # MODIFY: Add capture/run commands, --verbose flag, SIGINT handling
└── test/
    ├── CaptureStreamSpec.hs    # MODIFY: Add captureFromStream tests
    ├── CLIIntegration.hs       # MODIFY: Add capture/run integration tests
    └── fixtures/               # ADD: Test log files for integration tests
```

**Structure Decision**: Single project structure maintained. Extends existing modules rather than adding new ones, following Clean Code Design principle (Principle VI).

## Post-Design Constitution Check

*Re-evaluated after Phase 1 design artifacts.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. CLI-First Architecture | ✅ PASS | JSONL output, `--verbose` to stderr, composable with Unix tools |
| II. Pure Functional Core | ✅ PASS | `captureFromStream` uses pure `processEvent`; side effects at CLI boundary |
| III. Test-First Development | ✅ PASS | Unit tests for combinator, integration tests for CLI defined |
| IV. Nix-Centered Development | ✅ PASS | No new tools; `unix` package already available |
| V. Type Safety | ✅ PASS | CLI args validated; smart constructors used |
| VI. Clean Code Design | ✅ PASS | Minimal changes to existing modules; simple DIY retry |

**Post-Design Gate Result**: PASS - Design artifacts align with all constitution principles.

## Complexity Tracking

> No violations requiring justification. Plan adheres to all constitution principles.
