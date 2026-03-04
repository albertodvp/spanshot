# Tasks: Capture Phase (v0.1)

**Input**: Design documents from `/specs/002-capture-phase/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle III (Test-First Development - NON-NEGOTIABLE)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story?] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

Based on plan.md project structure:

- Source: `hs-spanshot/src/`
- App: `hs-spanshot/app/`
- Tests: `hs-spanshot/test/`
- Docs: `docs/`

______________________________________________________________________

## Phase 1: Setup

**Purpose**: Project initialization and test infrastructure

- [ ] T001 [P] Create test fixture directory at hs-spanshot/test/fixtures/
- [ ] T002 [P] Create test log file with errors at hs-spanshot/test/fixtures/sample-errors.log
- [ ] T003 [P] Create test log file without errors at hs-spanshot/test/fixtures/sample-clean.log
- [ ] T004 Verify project builds with `just build`

______________________________________________________________________

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before user stories

**⚠️ CRITICAL**: The `captureFromStream` combinator and `finalizeCapture` are foundational - both US1 and US2 depend on them.

### Tests (TDD - Red Phase)

- [ ] T005 [P] Write unit test for `captureFromStream` basic capture in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T006 [P] Write unit test for `captureFromStream` finalization (stream ends during capture) in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T007 [P] Write unit test for `captureFromStream` with no matching events in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T008 [P] Write unit test for `finalizeCapture` in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T009 [P] Write unit test for empty input stream (no events) in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T010 [P] Write unit test for rapid error bursts (single-active-capture policy) in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T011 [P] Write unit test for sparse pre-window (fewer events than duration) in hs-spanshot/test/CaptureStreamSpec.hs
- [ ] T012 Verify all foundational tests FAIL (Red phase complete)

### Implementation (TDD - Green Phase)

- [ ] T013 Implement `finalizeCapture` function in hs-spanshot/src/Capture.hs
- [ ] T014 Implement `captureFromStream` combinator in hs-spanshot/src/Capture.hs (pattern matching on Stream constructors per research.md)
- [ ] T015 Export `captureFromStream` and `finalizeCapture` from Capture module in hs-spanshot/src/Capture.hs
- [ ] T016 Verify all foundational tests PASS (Green phase complete)
- [ ] T017 Run `just test-unit` to confirm no regressions

**Checkpoint**: Foundation ready - `captureFromStream` is functional and tested

______________________________________________________________________

## Phase 3: User Story 1 - Basic Error Capture via CLI (Priority: P1) 🎯 MVP

**Goal**: Implement `spanshot capture` command for one-shot error capture from log files

**Independent Test**: Run `spanshot capture --logfile test.log --regex-pattern "ERROR" --pre-window 5 --post-window 5` and verify JSONL output

### Tests for User Story 1 (TDD - Red Phase)

- [ ] T018 [P] [US1] Write integration test for `capture` command with valid file (verify JSONL output format) in hs-spanshot/test/CLIIntegration.hs
- [ ] T019 [P] [US1] Write integration test for `capture` command with no matches in hs-spanshot/test/CLIIntegration.hs
- [ ] T020 [P] [US1] Write integration test for `capture` command with invalid regex in hs-spanshot/test/CLIIntegration.hs
- [ ] T021 [P] [US1] Write integration test for `capture` command with missing file in hs-spanshot/test/CLIIntegration.hs
- [ ] T022 [US1] Verify all US1 tests FAIL (Red phase complete)

### Implementation for User Story 1 (TDD - Green Phase)

- [ ] T023 [US1] Add `CaptureSettings` data type with CLI flags in hs-spanshot/app/Main.hs
- [ ] T024 [US1] Add `HasParser CaptureSettings` instance with --logfile, --regex-pattern, --pre-window, --post-window, --verbose in hs-spanshot/app/Main.hs
- [ ] T025 [US1] Add `DispatchCapture` to `Dispatch` type and parser in hs-spanshot/app/Main.hs
- [ ] T026 [US1] Implement `runCapture` function (collect → capture → output JSONL to stdout, one SpanShot per line) in hs-spanshot/app/Main.hs
- [ ] T027 [US1] Add argument validation with helpful error messages in hs-spanshot/app/Main.hs
- [ ] T028 [US1] Verify all US1 tests PASS (Green phase complete)
- [ ] T029 [US1] Run `just test-integration-cli` to confirm US1 works end-to-end

**Checkpoint**: User Story 1 complete - `spanshot capture` is functional and tested

______________________________________________________________________

## Phase 4: User Story 2 - Full Pipeline via CLI (Priority: P2)

**Goal**: Implement `spanshot run` command for continuous log monitoring with SIGINT handling

**Independent Test**: Run `spanshot run --logfile app.log`, append errors to the file, verify SpanShots output; press Ctrl+C and verify graceful exit

### Tests for User Story 2 (TDD - Red Phase)

- [ ] T030 [P] [US2] Write integration test for `run` command with config file in hs-spanshot/test/CLIIntegration.hs
- [ ] T031 [P] [US2] Write integration test for `run` command with missing file in hs-spanshot/test/CLIIntegration.hs
- [ ] T032 [P] [US2] Write unit test for `retryWithBackoff` function in hs-spanshot/test/CLIIntegration.hs
- [ ] T033 [US2] Verify all US2 tests FAIL (Red phase complete)

### Implementation for User Story 2 (TDD - Green Phase)

- [ ] T034 [US2] Add `RunSettings` data type with CLI flags in hs-spanshot/app/Main.hs
- [ ] T035 [US2] Add `HasParser RunSettings` instance with --logfile, --verbose in hs-spanshot/app/Main.hs
- [ ] T036 [US2] Add `DispatchRun` to `Dispatch` type and parser in hs-spanshot/app/Main.hs
- [ ] T037 [US2] Implement `installShutdownHandler` for SIGINT handling in hs-spanshot/app/Main.hs
- [ ] T038 [US2] Implement `retryWithBackoff` for transient file errors in hs-spanshot/app/Main.hs
- [ ] T039 [US2] Modify `bytesWithPolling` to check shutdown MVar in hs-spanshot/src/Collect.hs
- [ ] T040 [US2] Implement `runRun` function (collect → capture → output JSONL to stdout, one SpanShot per line, with shutdown handling) in hs-spanshot/app/Main.hs
- [ ] T041 [US2] Add verbose mode output to stderr with `[spanshot]` prefix in hs-spanshot/app/Main.hs
- [ ] T042 [US2] Verify all US2 tests PASS (Green phase complete)
- [ ] T043 [US2] Run `just test-integration-cli` to confirm US2 works end-to-end

**Checkpoint**: User Story 2 complete - `spanshot run` is functional with graceful shutdown

______________________________________________________________________

## Phase 5: User Story 3 - Real-World Usage Documentation (Priority: P3)

**Goal**: Create documentation showing how to use SpanShot with real applications

**Independent Test**: Follow each documented example end-to-end and successfully capture errors

### Implementation for User Story 3

- [ ] T044 [US3] Create docs/ directory if not exists
- [ ] T045 [P] [US3] Write web server monitoring example (e.g., simple HTTP server writing logs) in docs/usage-examples.md
- [ ] T046 [P] [US3] Write background process example (e.g., long-running script) in docs/usage-examples.md
- [ ] T047 [P] [US3] Write Docker container example (redirect logs, monitor from host) in docs/usage-examples.md
- [ ] T048 [US3] Add quick-start section with copy-paste commands in docs/usage-examples.md
- [ ] T049 [US3] Add troubleshooting section (common issues, error messages) in docs/usage-examples.md
- [ ] T050 [US3] Validate all examples work by running them manually

**Checkpoint**: User Story 3 complete - Users can follow real-world examples

______________________________________________________________________

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final cleanup, refactoring, and validation

- [ ] T051 [P] Update README.md with new `capture` and `run` commands
- [ ] T052 [P] Add `--help` examples to CLI parser descriptions in hs-spanshot/app/Main.hs
- [ ] T053 Run `nix fmt` to format all code
- [ ] T054 Run `just test` to verify all tests pass
- [ ] T055 Run `just build` to verify clean build
- [ ] T056 Validate quickstart.md examples still work
- [ ] T057 Manual smoke test: run `spanshot capture` and `spanshot run` against real log files
- [ ] T058 Validate SC-001: Verify first capture within 2 minutes using only CLI help
- [ ] T059 Validate SC-003: Test with 100MB log file to verify streaming (no memory issues)
- [ ] T060 Validate SC-005: Review all error messages for clarity and actionability

______________________________________________________________________

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational - MVP target
- **US2 (Phase 4)**: Depends on Foundational - Can parallelize with US1 (different files)
- **US3 (Phase 5)**: Depends on US1 and US2 being functional (needs working CLI to document)
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Depends only on Foundational - No cross-story dependencies
- **User Story 2 (P2)**: Depends only on Foundational - Independent of US1 (different CLI command)
- **User Story 3 (P3)**: Depends on US1 + US2 (needs working CLI to document examples)

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD Red phase)
1. Implementation MUST make tests PASS (TDD Green phase)
1. Refactor while tests stay green (TDD Refactor phase)

### Parallel Opportunities

**Phase 1 (Setup)**:

- T001, T002, T003 can run in parallel

**Phase 2 (Foundational)**:

- T005-T011 can run in parallel (different test cases)
- T013, T014 are sequential (finalizeCapture before captureFromStream)

**Phase 3 (US1) and Phase 4 (US2)**:

- US1 and US2 can be worked in parallel after Foundational completes
- Different CLI commands in different sections of Main.hs

**Phase 5 (US3)**:

- T045, T046, T047 can run in parallel (different examples)

______________________________________________________________________

## Parallel Example: US1 Tests

```bash
# Launch all US1 tests together:
Task: T018 "Write integration test for capture command with valid file"
Task: T019 "Write integration test for capture command with no matches"
Task: T020 "Write integration test for capture command with invalid regex"
Task: T021 "Write integration test for capture command with missing file"
```

______________________________________________________________________

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (fixtures ready)
1. Complete Phase 2: Foundational (`captureFromStream` works)
1. Complete Phase 3: User Story 1 (`spanshot capture` works)
1. **STOP and VALIDATE**: Test US1 independently
1. Can ship MVP with just `capture` command

### Incremental Delivery

1. Setup + Foundational → Core capture logic ready
1. Add US1 → `spanshot capture` works → Demo/Release v0.1-alpha
1. Add US2 → `spanshot run` works → Demo/Release v0.1-beta
1. Add US3 → Documentation ready → Release v0.1

### TDD Discipline (NON-NEGOTIABLE per Constitution)

Each phase follows strict Red-Green-Refactor:

1. **Red**: Write tests that describe expected behavior, verify they FAIL
1. **Green**: Write minimal code to make tests PASS
1. **Refactor**: Clean up code while keeping tests green

______________________________________________________________________

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Constitution Principle III requires TDD - tests before implementation
- Exit code 130 for SIGINT (128 + signal 2)
- Verbose output uses `[spanshot]` prefix to stderr
- JSONL output to stdout for Unix composability
