# Tasks: Developer-First Input Modes

**Input**: Design documents from `/specs/003-dev-input-modes/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: TDD is mandatory per constitution - tests MUST be written first and fail before implementation.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

______________________________________________________________________

## Phase 1: Setup

**Purpose**: Add new dependencies and create module structure

- [ ] T001 Add `posix-pty` and `async` dependencies to `hs-spanshot.cabal`
- [ ] T002 [P] Create `hs-spanshot/src/Session.hs` module stub (exports only)
- [ ] T003 [P] Create `hs-spanshot/src/Session/Pty.hs` module stub with CPP for Windows
- [ ] T004 [P] Create `hs-spanshot/src/Session/State.hs` module stub
- [ ] T005 [P] Create `hs-spanshot/src/Wrap.hs` module stub
- [ ] T006 [P] Create `hs-spanshot/src/Storage.hs` module stub

______________________________________________________________________

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure needed by all user stories

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T007 Extend `Types.hs`: Add `Session` type with `sessionId`, `startTime`, `shellPath`
- [ ] T008 Extend `Types.hs`: Add `CaptureSource` enum (`SessionCapture`, `WrapCapture`, `FileCapture`)
- [ ] T009 Extend `Types.hs`: Add `sessionId` and `source` fields to `SpanShot` type
- [ ] T010 Extend `Config.hs`: Add `maxCaptures` config field (default 100)
- [ ] T011 Implement `Storage.hs`: `saveCapture`, `listCaptures`, `loadCapture`, `enforceLimit` (LRU)
- [ ] T012 Add `StorageSpec.hs`: Test LRU eviction, file naming, JSON round-trip

**Checkpoint**: Foundation ready - user story implementation can begin

______________________________________________________________________

## Phase 3: User Story 1 - PTY Session Mode (Priority: P1) 🎯 MVP

**Goal**: Developer can run `spns session`, work normally, errors captured automatically

**Independent Test**: Run `spns session`, trigger ERROR, verify capture saved

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T013 [P] [US1] Unit test `Session/State.hs`: session creation, ID generation in `test/SessionStateSpec.hs`
- [ ] T014 [P] [US1] Unit test `Session/Pty.hs`: PTY spawn, read/write (mock) in `test/SessionPtySpec.hs`
- [ ] T015 [US1] Integration test: `spanshot session` basic flow in `test/CLIIntegration.hs`

### Implementation for User Story 1

- [ ] T016 [US1] Implement `Session/State.hs`: `newSession`, `addCapture`, `getCaptures`
- [ ] T017 [US1] Implement `Session/Pty.hs`: `spawnPty`, `readPty`, `writePty` (Unix)
- [ ] T018 [US1] Implement `Session/Pty.hs`: Windows stub returning "not supported"
- [ ] T019 [US1] Implement `Session.hs`: `runSession` main loop with concurrent I/O
- [ ] T020 [US1] Implement `Session.hs`: Signal handlers (SIGINT forward, SIGWINCH resize)
- [ ] T021 [US1] Implement `Session.hs`: Integration with capture pipeline (feed PTY output)
- [ ] T022 [US1] Implement concurrent session detection (lock file + warning)
- [ ] T023 [US1] Add CLI command `session` in `app/Main.hs`
- [ ] T024 [US1] Implement welcome message on session start, summary on exit

**Checkpoint**: `spanshot session` works - can capture errors from PTY

______________________________________________________________________

## Phase 4: User Story 2 - Wrap Mode (Priority: P2)

**Goal**: Run single command with monitoring, preserve exit code

**Independent Test**: Run `spns wrap -- echo test`, verify exit code matches

### Tests for User Story 2 ⚠️

- [ ] T025 [P] [US2] Unit test `Wrap.hs`: command execution, exit code passthrough in `test/WrapSpec.hs`
- [ ] T026 [US2] Integration test: `spanshot wrap -- cmd` in `test/CLIIntegration.hs`

### Implementation for User Story 2

- [ ] T027 [US2] Implement `Wrap.hs`: `runWrap` using PTY for single command
- [ ] T028 [US2] Implement `Wrap.hs`: Exit code capture and passthrough
- [ ] T029 [US2] Implement `Wrap.hs`: Integration with capture pipeline
- [ ] T030 [US2] Add CLI command `wrap` with `--` delimiter parsing in `app/Main.hs`

**Checkpoint**: `spanshot wrap -- cmd` works - exit code preserved, errors captured

______________________________________________________________________

## Phase 5: User Story 3 - Status/Show Commands (Priority: P3)

**Goal**: View captured errors with `status` and `show` commands

**Independent Test**: Create capture manually, run `spanshot status`, verify listed

### Tests for User Story 3 ⚠️

- [ ] T031 [P] [US3] Unit test `Storage.hs`: listing and filtering in `test/StorageSpec.hs`
- [ ] T032 [US3] Integration test: `spanshot status` and `spanshot show` in `test/CLIIntegration.hs`

### Implementation for User Story 3

- [ ] T033 [US3] Implement `status` command: list recent captures with timestamps
- [ ] T034 [US3] Implement `show N` command: display full capture with context
- [ ] T035 [US3] Implement `show N --json` flag: output as JSON
- [ ] T036 [US3] Add CLI commands `status` and `show` in `app/Main.hs`

**Checkpoint**: All core commands work independently

______________________________________________________________________

## Phase 6: Polish & Integration

**Purpose**: Final integration and documentation

- [ ] T037 [P] Update README.md with new commands (session, wrap, status, show)
- [ ] T038 [P] Add `spns` alias documentation
- [ ] T039 Run `just test` - all tests pass
- [ ] T040 Run `just build` - no warnings
- [ ] T041 Validate quickstart.md examples work end-to-end

______________________________________________________________________

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup
- **User Stories (Phase 3-5)**: All depend on Foundational
  - US1 (Session) → US2 (Wrap) can reuse PTY logic
  - US3 (Status/Show) only depends on Storage
- **Polish (Phase 6)**: After all user stories

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
1. Types/models before logic
1. Core implementation before CLI wiring
1. Commit after each task

### Parallel Opportunities

```
After Phase 2 (Foundational):
├── US1: Session Mode (T013-T024)
│   └── US2: Wrap Mode (T025-T030) - can start after T017-T019 done
└── US3: Status/Show (T031-T036) - independent, only needs Storage
```

______________________________________________________________________

## Task Count Summary

| Phase | Tasks | Parallel |
|-------|-------|----------|
| Setup | 6 | 5 |
| Foundational | 6 | 0 |
| US1 Session | 12 | 3 |
| US2 Wrap | 6 | 2 |
| US3 Status/Show | 6 | 2 |
| Polish | 5 | 2 |
| **Total** | **41** | **14** |
