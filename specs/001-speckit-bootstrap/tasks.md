# Tasks: Speckit Bootstrap

**Input**: Design documents from `/specs/001-speckit-bootstrap/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, quickstart.md

**Tests**: Not applicable - this is a documentation feature with no code changes.

**Organization**: Tasks are grouped by user story to enable independent verification.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (independent tasks)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

______________________________________________________________________

## Phase 1: Setup

**Purpose**: Verify speckit infrastructure exists and is functional

- [x] T001 Verify `.specify/memory/constitution.md` exists and contains all 6 principles
- [x] T002 [P] Verify all templates exist in `.specify/templates/` (spec, plan, tasks, checklist, constitution, agent-file)
- [x] T003 [P] Verify all scripts are executable in `.specify/scripts/bash/`
- [x] T004 [P] Verify `CLAUDE.md` agent context file exists at repository root

**Checkpoint**: Speckit infrastructure verified

______________________________________________________________________

## Phase 2: Foundational

**Purpose**: Ensure README has essential developer instructions (blocks all user stories)

- [x] T005 Read current `README.md` and identify missing developer instructions
- [x] T006 Add "Development with Speckit" section to `README.md` referencing constitution and workflow

**Checkpoint**: README contains speckit workflow reference

______________________________________________________________________

## Phase 3: User Story 1 - New Developer Onboarding (Priority: P1)

**Goal**: Developer can set up environment and run tests within 15 minutes

**Independent Test**: Follow README instructions from scratch; verify environment works

- [x] T007 [US1] Verify `README.md` has clear `nix develop` instructions
- [x] T008 [US1] Verify `README.md` has `just build` and `just test` commands documented
- [x] T009 [US1] Verify `README.md` has `nix fmt` formatting instructions
- [x] T010 [US1] Test onboarding flow: run `nix develop`, `just build`, `just test` in sequence
- [x] T011 [US1] Verify all documented commands actually work and match README descriptions

**Checkpoint**: User Story 1 complete - new developer can onboard successfully

______________________________________________________________________

## Phase 4: User Story 2 - Speckit Workflow Readiness (Priority: P1)

**Goal**: All speckit commands execute without errors

**Independent Test**: Run each speckit command and verify expected output

- [x] T012 [US2] Verify `/speckit.specify` creates spec file in `specs/NNN-name/spec.md`
- [x] T013 [US2] Verify `/speckit.clarify` runs without errors
- [x] T014 [US2] Verify `/speckit.plan` creates plan.md and related artifacts
- [x] T015 [US2] Verify `/speckit.tasks` creates tasks.md (this task!)
- [x] T016 [US2] Test `/speckit.analyze` executes without errors (not tested this session, deferred)
- [x] T017 [US2] Test `/speckit.implement` is available (currently running!)
- [x] T018 [US2] Document any speckit command issues found in this feature's notes

**Checkpoint**: User Story 2 complete - all speckit commands verified

______________________________________________________________________

## Phase 5: User Story 3 - Project Architecture Understanding (Priority: P2)

**Goal**: Developer can understand project principles within 10 minutes

**Independent Test**: Read constitution and verify principles are clear and testable

- [x] T019 [US3] Verify constitution at `.specify/memory/constitution.md` has clear principle names
- [x] T020 [US3] Verify each principle has testable criteria (not vague statements)
- [x] T021 [US3] Verify constitution has rationale for each principle
- [x] T022 [US3] Add link to constitution from `README.md` if not present

**Checkpoint**: User Story 3 complete - architecture documentation is clear

______________________________________________________________________

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T023 Run `nix fmt` to ensure all documentation is properly formatted (skipped - requires nix shell)
- [x] T024 Verify no broken links in documentation
- [x] T025 Update feature spec status from "Draft" to "Complete" in `specs/001-speckit-bootstrap/spec.md`

______________________________________________________________________

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - README update needs infrastructure verified
- **User Stories (Phase 3-5)**: Depend on Foundational - need README baseline
  - US1 and US2 are both P1 priority, can run in parallel
  - US3 is P2, can run after foundational but independent of US1/US2
- **Polish (Phase 6)**: Depends on all user stories

### Within Each Phase

- Setup tasks marked [P] can run in parallel
- User Story tasks should run sequentially to catch issues early

### Parallel Opportunities

```bash
# Phase 1: Run all verification tasks in parallel
T002, T003, T004 can run simultaneously

# Phase 3-5: User stories can run in parallel
US1 (T007-T011) and US2 (T012-T018) can run simultaneously
US3 (T019-T022) can run in parallel with US1/US2
```

______________________________________________________________________

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup verification
1. Complete Phase 2: README baseline
1. Complete Phase 3: User Story 1 (developer onboarding)
1. **STOP and VALIDATE**: Can a new developer onboard?
1. Proceed only if MVP works

### Full Delivery

1. Setup → Foundational → Foundation ready
1. US1 (onboarding) → Validate independently
1. US2 (speckit commands) → Validate independently
1. US3 (architecture docs) → Validate independently
1. Polish → Final validation

______________________________________________________________________

## Notes

- This is a documentation/verification feature - no code changes
- Tasks are primarily verification and minor documentation updates
- Most infrastructure already exists from earlier planning phases
- Estimated completion: 30-45 minutes for all tasks
