# Feature Specification: Capture Phase (v0.1)

**Feature Branch**: `002-capture-phase`\
**Created**: 2026-03-04\
**Status**: Draft\
**Input**: User description: "Capture Phase (v0.1 Priority): Stream combinator captureFromStream, CLI commands for capture and run, integration tests, documentation"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Error Capture via CLI (Priority: P1)

A developer wants to capture errors from a log file with context. They run a single command specifying the log file, pattern to match, and window sizes, and receive captured error snapshots (SpanShots) as output.

**Why this priority**: This is the core value proposition - developers need a simple way to capture errors with surrounding context from log files without writing custom scripts or manually scrolling through logs.

**Independent Test**: Can be fully tested by running `spanshot capture --logfile test.log --regex-pattern "ERROR" --pre-window 5 --post-window 5` against a file with known errors, verifying SpanShots are output with correct pre/post context windows.

**Acceptance Scenarios**:

1. **Given** a log file containing an ERROR line with preceding and following context, **When** the user runs `spanshot capture --logfile app.log --regex-pattern "ERROR" --pre-window 5 --post-window 5`, **Then** the system outputs a SpanShot containing the error event, up to 5 seconds of pre-context events, and up to 5 seconds of post-context events.
1. **Given** a log file with no matching patterns, **When** the user runs the capture command, **Then** the system produces no SpanShots and exits cleanly.
1. **Given** an invalid regex pattern, **When** the user runs the capture command, **Then** the system displays an error message describing the invalid pattern and exits with a non-zero status code.

______________________________________________________________________

### User Story 2 - Full Pipeline via CLI (Priority: P2)

A developer wants an all-in-one command that collects log events from a file and captures errors in a single pipeline. They run `spanshot run` and the system continuously monitors the log file, detecting errors and outputting SpanShots as they occur.

**Why this priority**: While capture alone provides value, the full pipeline reduces cognitive load by combining collection and capture into a single command, making it easier for developers to monitor logs in real-time.

**Independent Test**: Can be fully tested by running `spanshot run --logfile app.log` while appending error lines to the log file, verifying SpanShots are output in real-time.

**Acceptance Scenarios**:

1. **Given** an active log file being written to, **When** the user runs `spanshot run --logfile app.log`, **Then** the system continuously monitors the file and outputs SpanShots as errors are detected.
1. **Given** the user presses Ctrl+C while monitoring, **When** the signal is received, **Then** the system emits any in-flight captures (errors detected but post-window incomplete) and exits gracefully.
1. **Given** the log file does not exist, **When** the user runs the command, **Then** the system displays an error message and exits with a non-zero status code.

______________________________________________________________________

### User Story 3 - Real-World Usage Documentation (Priority: P3)

A developer wants to understand how to use SpanShot with their actual applications. They need documentation showing concrete examples of running SpanShot alongside real services to capture errors from their logs.

**Why this priority**: Without real-world examples, users may understand the CLI syntax but not how to integrate SpanShot into their development or production workflow. Examples bridge the gap between "what does this tool do" and "how do I actually use it".

**Independent Test**: Can be validated by following the documented examples end-to-end and successfully capturing errors from sample applications.

**Acceptance Scenarios**:

1. **Given** a developer with a web server writing logs to a file, **When** they follow the usage documentation, **Then** they can successfully run `spanshot run` to capture errors as the server runs.
1. **Given** a developer running a process that outputs to stdout, **When** they follow the documentation, **Then** they can pipe or redirect logs to a file and use SpanShot to monitor it.
1. **Given** a new user reading the documentation, **When** they look for examples, **Then** they find at least 3 realistic scenarios (e.g., web server, background job, containerized app) with step-by-step instructions.

______________________________________________________________________

### Edge Cases

- What happens when the log file is empty? System should exit cleanly with no output.
- What happens when multiple errors occur within each other's post-windows? The single-active-capture policy means the second error is added to the first error's post-window but doesn't trigger a new capture.
- How does the system handle very rapid error bursts? Same as above - only the first error starts a capture until its post-window completes.
- What happens when pre-window has fewer events than requested? System captures whatever context is available (may be less than the requested duration).
- How does the system handle binary/non-UTF8 content in logs? UTF-8 decoding with lenient error handling (replacement characters for invalid bytes).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `captureFromStream` function that transforms a stream of `CollectEvent`s into a stream of `SpanShot`s
- **FR-002**: System MUST emit in-flight captures when the input stream ends, yielding partial SpanShots with available post-context
- **FR-003**: System MUST provide a `capture` CLI command accepting logfile path, regex pattern, pre-window duration, and post-window duration as arguments
- **FR-004**: System MUST provide a `run` CLI command that combines log collection and capture into a single pipeline
- **FR-005**: System MUST output SpanShots to stdout in a structured format (one per line)
- **FR-006**: System MUST handle graceful termination via SIGINT (Ctrl+C), emitting any in-flight captures before exit
- **FR-007**: System MUST validate all CLI arguments and display helpful error messages for invalid inputs
- **FR-008**: System MUST exit with non-zero status code on errors (invalid arguments, file not found, invalid regex)
- **FR-009**: System MUST support a `--verbose` flag that emits progress/status information to stderr (default: silent)
- **FR-010**: System MUST retry file access with backoff on transient errors (up to 3 attempts), then exit with error if unrecoverable
- **FR-011**: Project MUST include usage documentation with real-world examples showing how to use SpanShot with common application types (web servers, background processes, containers)

### Key Entities

- **CollectEvent**: A timestamped log line from a source file, including source path, session order ID, timestamp, and line content
- **SpanShot**: A captured error with context, containing the error event, pre-window events, post-window events, detection rules that matched, and capture timestamp
- **CaptureOptions**: Configuration for capture behavior including pre/post window durations, minimum context events, and detection rules
- **CaptureState**: Internal state machine tracking the pre-window buffer and any active (in-progress) capture

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can capture their first error snapshot within 2 minutes of installation using only the CLI help
- **SC-002**: Captured SpanShots include at least 80% of available context within the specified time windows
- **SC-003**: System handles log files up to 100MB without memory issues (streaming, not loading entire file)
- **SC-004**: CLI response time for first SpanShot output is under 1 second after error occurrence (excluding post-window wait time)
- **SC-005**: All CLI commands provide meaningful error messages that help users correct their input on first retry

## Clarifications

### Session 2026-03-04

- Q: Should the CLI emit progress/status information to stderr? → A: Optional verbose mode via `--verbose` flag (default: silent)
- Q: How should the system handle file permission errors during monitoring? → A: Retry with backoff (e.g., 3 attempts), then exit with error
- Q: Should the CLI support writing SpanShots to a file via --output option? → A: Stdout only for v0.1 (users can shell-redirect)

## Assumptions

- Users have basic familiarity with regex patterns for error detection
- Log files are text-based and primarily UTF-8 encoded
- Pre and post window durations are specified in seconds (integer values)
- Default detection pattern of "ERROR" is sufficient for initial use cases
- SpanShots are output as JSONL (newline-delimited JSON, one object per line) to stdout for streaming consumption and Unix tool composability
