# Implementation Plan: Developer-First Input Modes

**Branch**: `003-dev-input-modes` | **Date**: 2026-03-04 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-dev-input-modes/spec.md`

## Summary

Implement three input modes for SpanShot: PTY session mode (P1), wrap mode for single commands (P2), and enhance existing run mode (P3). Add `status` and `show` commands for viewing captures. All modes feed into the existing capture pipeline and save SpanShots to `.spanshot/captures/`.

**Technical approach**: Use Haskell's `unix` package for PTY management on Unix systems. Windows returns "not supported" stub. Captures stored as JSON files with LRU eviction (100 default, configurable).

## Technical Context

**Language/Version**: Haskell (GHC 9.12+)\
**Primary Dependencies**: streaming, streaming-bytestring, aeson, opt-env-conf, regex-tdfa, unix (for PTY)\
**Storage**: Filesystem (`.spanshot/captures/*.json`)\
**Testing**: hspec + QuickCheck (unit), CLI integration tests\
**Target Platform**: Unix (Linux, macOS); Windows returns "not supported" stub\
**Project Type**: CLI tool\
**Performance Goals**: Capture saved within 1 second of post-window elapsing; PTY I/O latency imperceptible (\<10ms)\
**Constraints**: Memory bounded (streaming); max 100 captures by default (LRU eviction)\
**Scale/Scope**: Single developer usage; handles log files of arbitrary size via streaming

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. CLI-First Architecture** | ✅ PASS | All features via CLI (`session`, `wrap`, `status`, `show`); JSONL output supported |
| **II. Pure Functional Core** | ✅ PASS | PTY I/O at edges; capture logic remains pure; streaming for output processing |
| **III. Test-First Development** | ✅ PASS | Tests required before implementation; unit + integration tests planned |
| **IV. Nix-Centered Development** | ✅ PASS | All dependencies via Nix; `nix develop` for environment |
| **V. Type Safety** | ✅ PASS | Smart constructors for Session, Capture; newtypes where appropriate |
| **VI. Clean Code Design** | ✅ PASS | Single responsibility modules; no over-engineering |

**Gate Status**: PASS - No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/003-dev-input-modes/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
hs-spanshot/
├── app/
│   └── Main.hs              # CLI entry point (add session, wrap, status, show commands)
├── src/
│   ├── Types.hs             # Core types (extend with Session type)
│   ├── Collect.hs           # Log collection (existing)
│   ├── Capture.hs           # Error capture logic (existing)
│   ├── Config.hs            # Configuration (add max_captures)
│   ├── Session.hs           # NEW: PTY session management
│   ├── Session/
│   │   ├── Pty.hs           # NEW: PTY creation and I/O (Unix-only)
│   │   └── State.hs         # NEW: Session state (ID, captures, start time)
│   ├── Wrap.hs              # NEW: Single command wrapping
│   └── Storage.hs           # NEW: Capture storage with LRU eviction
├── test/
│   ├── SessionSpec.hs       # NEW: Session unit tests
│   ├── WrapSpec.hs          # NEW: Wrap mode unit tests
│   ├── StorageSpec.hs       # NEW: Storage/LRU tests
│   └── CLIIntegration.hs    # Extend with session/wrap/status/show tests
```

**Structure Decision**: Extend existing single-package structure. Add `Session.hs` and `Session/` submodule for PTY logic. Keep PTY-specific code isolated for Windows stub pattern.

## Complexity Tracking

No constitution violations requiring justification.
