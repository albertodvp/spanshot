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

- [x] T001 Add `unix` (for PTY via `openPseudoTerminal`) and `async` dependencies to `hs-spanshot.cabal`
- [x] T002 [P] Create `hs-spanshot/src/Session.hs` module stub (exports only)
- [x] T003 [P] Create `hs-spanshot/src/Session/Pty.hs` module stub with CPP for Windows
- [x] T004 [P] Create `hs-spanshot/src/Session/State.hs` module stub
- [x] T005 [P] Create `hs-spanshot/src/Wrap.hs` module stub
- [x] T006 [P] Create `hs-spanshot/src/Storage.hs` module stub

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

## Phase 3: User Story 2 - Wrap Mode (Build First) 🎯 MVP

**Goal**: Run single command with monitoring, preserve exit code

**Why first**: Wrap is a strict subset of session - learn PTY, signals, exit codes on simpler problem.
Per contrarian analysis: shippable value sooner, lessons inform session implementation.

**Independent Test**: Run `spns wrap -- echo test`, verify exit code matches

### Tests for User Story 2 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T013 [P] [US2] Unit test `Wrap.hs`: command execution, exit code passthrough in `test/WrapSpec.hs`
- [ ] T014 [US2] Integration test: `spanshot wrap -- cmd` in `test/CLIIntegration.hs`

### Implementation for User Story 2

- [ ] T015 [US2] Implement `Pty.hs`: `spawnPty`, `readPty`, `writePty` (Unix) - core PTY operations
- [ ] T016 [US2] Implement `Pty.hs`: Windows stub returning "not supported"
- [ ] T017 [US2] Implement `Wrap.hs`: `runWrap` using PTY for single command
- [ ] T018 [US2] Implement `Wrap.hs`: Exit code capture and passthrough
- [ ] T019 [US2] Implement `Wrap.hs`: Integration with capture pipeline
- [ ] T020 [US2] Add CLI command `wrap` with `--` delimiter parsing in `app/Main.hs`

**Checkpoint**: `spanshot wrap -- cmd` works - exit code preserved, errors captured

______________________________________________________________________

## Phase 4: User Story 1 - PTY Session Mode

**Goal**: Developer can run `spns session`, work normally, errors captured automatically

**Builds on**: Reuses PTY logic from wrap mode (T015-T016)

**Independent Test**: Run `spns session`, trigger ERROR, verify capture saved

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T021 [P] [US1] Unit test `Session/State.hs`: session creation, ID generation in `test/SessionStateSpec.hs`
- [ ] T022 [US1] Integration test: `spanshot session` basic flow in `test/CLIIntegration.hs`

### Implementation for User Story 1

- [ ] T023 [US1] Implement `Session/State.hs`: `newSession`, `addCapture`, `getCaptures`
- [ ] T024 [US1] Implement `Session.hs`: `runSession` main loop with concurrent I/O
- [ ] T025 [US1] Implement `Session.hs`: Signal handlers (SIGINT forward, SIGWINCH resize)
- [ ] T026 [US1] Implement `Session.hs`: Integration with capture pipeline (feed PTY output)
- [ ] T027 [US1] Implement concurrent session detection (lock file + warning)
- [ ] T028 [US1] Add CLI command `session` in `app/Main.hs`
- [ ] T029 [US1] Implement welcome message on session start, summary on exit

**Checkpoint**: `spanshot session` works - can capture errors from PTY

______________________________________________________________________

## Phase 5: User Story 3 - Status/Show Commands

**Goal**: View captured errors with `status` and `show` commands

**Independent Test**: Create capture manually, run `spanshot status`, verify listed

### Tests for User Story 3 ⚠️

- [ ] T030 [P] [US3] Unit test `Storage.hs`: listing and filtering in `test/StorageSpec.hs`
- [ ] T031 [US3] Integration test: `spanshot status` and `spanshot show` in `test/CLIIntegration.hs`

### Implementation for User Story 3

- [ ] T032 [US3] Implement `status` command: list recent captures with timestamps
- [ ] T033 [US3] Implement `show N` command: display full capture with context
- [ ] T034 [US3] Implement `show N --json` flag: output as JSON
- [ ] T035 [US3] Add CLI commands `status` and `show` in `app/Main.hs`

**Checkpoint**: All core commands work independently

______________________________________________________________________

## Phase 6: Polish & Integration

**Purpose**: Final integration and documentation

- [ ] T036 [P] Update README.md with new commands (session, wrap, status, show)
- [ ] T037 [P] Add `spns` alias documentation
- [ ] T038 Run `just test` - all tests pass
- [ ] T039 Run `just build` - no warnings
- [ ] T040 Validate quickstart.md examples work end-to-end

______________________________________________________________________

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup
- **User Stories (Phase 3-5)**: All depend on Foundational
  - US2 (Wrap) first → teaches PTY basics
  - US1 (Session) reuses PTY logic from wrap
  - US3 (Status/Show) only depends on Storage (can run in parallel with US1)
- **Polish (Phase 6)**: After all user stories

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
1. Types/models before logic
1. Core implementation before CLI wiring
1. Commit after each task

### Parallel Opportunities

```
After Phase 2 (Foundational):
├── US2: Wrap Mode (T013-T020) - BUILD FIRST
│   └── US1: Session Mode (T021-T029) - reuses PTY from wrap
└── US3: Status/Show (T030-T035) - independent, only needs Storage
```

______________________________________________________________________

## Task Count Summary

| Phase | Tasks | Parallel |
|-------|-------|----------|
| Setup | 6 | 5 |
| Foundational | 6 | 0 |
| US2 Wrap | 8 | 2 |
| US1 Session | 9 | 2 |
| US3 Status/Show | 6 | 2 |
| Polish | 5 | 2 |
| **Total** | **40** | **13** |
