# Feature Specification: Developer-First Input Modes

**Feature Branch**: `003-dev-input-modes`\
**Created**: 2026-03-04\
**Status**: Draft\
**Input**: User description: "Developer-first UX for SpanShot with PTY session mode, wrap mode for single commands, and status/show commands for viewing captures"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - PTY Session Mode (Priority: P1)

A developer wants to start a monitored terminal session and work naturally, with SpanShot capturing errors invisibly in the background.

**Why this priority**: This is the core UX - fire-and-forget monitoring. Developer starts a session, works normally, and queries status when needed. Zero friction during normal workflow.

**Independent Test**: Run `spns session`, execute a command that produces an ERROR, run `spns status`, verify capture appears in the list.

**Acceptance Scenarios**:

1. **Given** I am in a terminal, **When** I run `spns session`, **Then** a new shell starts inside a PTY with a welcome message indicating I'm in a SpanShot session
1. **Given** I'm inside a session, **When** I run `cargo test` and it outputs a line matching the ERROR pattern, **Then** a capture is saved automatically to `.spanshot/captures/`
1. **Given** I'm inside a session, **When** I run `spns status`, **Then** I see a list of captures from this session with timestamps and brief descriptions
1. **Given** I'm inside a session, **When** I run `spns show 1`, **Then** I see the full snapshot including pre-window context, the error event, and post-window context
1. **Given** I'm inside a session, **When** I type `exit`, **Then** the session ends with a summary message showing how many captures were saved

______________________________________________________________________

### User Story 2 - Wrap Mode for Single Commands (Priority: P2)

A developer or AI agent wants to run a single command with error monitoring, preserving the command's exit code.

**Why this priority**: Essential for non-interactive contexts: CI/CD pipelines, shell scripts, and AI agent development loops where commands are run programmatically.

**Independent Test**: Run `spns wrap -- npm test` where the test fails with an ERROR, verify capture is saved and exit code matches npm's exit code.

**Acceptance Scenarios**:

1. **Given** I run `spns wrap -- npm test`, **Then** I see npm's normal stdout and stderr output in my terminal
1. **Given** the wrapped command outputs a line matching ERROR pattern, **Then** a capture is saved to `.spanshot/captures/` with full context
1. **Given** the wrapped command exits with code 1, **Then** `spns wrap` also exits with code 1 (exit code passthrough)
1. **Given** I run `spns wrap -- cmd` without errors, **Then** no capture is created and exit code is 0

______________________________________________________________________

### User Story 3 - Watch Existing Log File (Priority: P3)

A developer wants to monitor an existing log file for errors while working on something else.

**Why this priority**: Already partially implemented (`run` command exists). Useful when the application is already running or logs are written by external processes.

**Independent Test**: Run `spns run --logfile app.log`, append an ERROR line to the file, verify capture is saved.

**Acceptance Scenarios**:

1. **Given** a log file `app.log` exists, **When** I run `spns run --logfile app.log`, **Then** SpanShot tails the file for new content
1. **Given** SpanShot is tailing a file, **When** an ERROR line is appended, **Then** a capture is saved with pre-window and post-window context

______________________________________________________________________

### Edge Cases

- What happens when multiple errors occur within the same span window? (Subsequent errors should be captured in the post-window of the first, then start their own capture)
- How does SpanShot handle output without newlines (streaming progress bars)? (Buffer until newline or timeout)
- What if the app crashes before SpanShot can save the capture? (Flush on signal, best-effort save)
- How to handle very long-running sessions with many captures? (Bounded capture count per session, oldest rotated)
- What if `spns status` is called outside a session? (Show recent captures from any source)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide `session` command that starts a PTY-based monitored shell
- **FR-002**: System MUST provide `wrap` command that runs a single command with monitoring, using `--` as delimiter
- **FR-003**: System MUST capture errors matching configured detection rules (regex patterns)
- **FR-004**: System MUST save captures to `.spanshot/captures/` with timestamp-based naming (e.g., `2026-03-04-001.json`)
- **FR-005**: System MUST provide `status` command showing captures from current session or recent activity
- **FR-006**: System MUST provide `show` command to view a specific capture with full context
- **FR-007**: System MUST preserve exit code from wrapped commands (exit code passthrough)
- **FR-008**: System MUST handle Ctrl+C gracefully (forward signal to child, save pending captures, clean exit)
- **FR-009**: System MUST display a welcome message when entering a session and summary when exiting
- **FR-010**: System SHOULD provide `spns` as a short alias for `spanshot`
- **FR-011**: On Windows, `session` and `wrap` commands MUST return a clear "not supported on Windows" error message and exit gracefully
- **FR-012**: System MUST limit stored captures to 100 by default, evicting oldest when limit exceeded (LRU); limit configurable via `max_captures` in config
- **FR-013**: System MUST detect concurrent sessions and display a warning when starting a new session while another is running

### Key Entities

- **Session**: A PTY-based monitored shell instance with a unique ID, start time, and list of associated captures
- **Capture (SpanShot)**: An error event with pre-window context, post-window context, detection metadata, and timestamp
- **DetectionRule**: A pattern (regex) used to identify error lines in the output stream

## Clarifications

### Session 2026-03-04

- Q: What platform support for PTY-based session/wrap modes? → A: Unix only; Windows returns "not supported" error (stub implementation)
- Q: What is the capture storage retention policy? → A: Keep last 100 captures (default) with LRU eviction; limit configurable via `max_captures` in `.spanshot.yaml`
- Q: What happens with concurrent sessions? → A: Allow multiple sessions but warn user when another session is already running

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developer can run `spns session` and work normally with automatic error capture - no manual intervention required
- **SC-002**: Developer can query `spns status` to see captures without leaving the session or disrupting workflow
- **SC-003**: Error captures are saved within 1 second of the post-window duration elapsing
- **SC-004**: 100% of errors matching configured detection rules are captured (no silent drops)
- **SC-005**: `spns wrap` preserves exact exit code of wrapped command (verified by `echo $?` matching)
- **SC-006**: Session mode captures all stdout and stderr from child processes transparently
- **SC-007**: Ctrl+C during a session or wrap ends gracefully with captures saved
