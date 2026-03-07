# Feature Specification: Daemon & OTEL Mode

**Feature Branch**: `004-daemon-otel-mode`\
**Created**: 2026-03-07\
**Status**: Draft\
**Input**: User description: "Daemon mode with OTEL receiver for capturing telemetry from OpenTelemetry-instrumented applications"

## Vision

SpanShot evolves from a developer tool to an **OTEL-native error capture system** that works identically in development and production. The name "SpanShot" gains deeper meaning: "span" refers both to the temporal window around errors (current) and to distributed tracing spans (OTEL).

**Core insight**: For OTEL-instrumented apps, the local and production experience should be identical—eliminating context switching and ensuring consistent error capture behavior.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - OTEL Receiver for Instrumented Apps (Priority: P1)

A developer with an OTEL-instrumented application wants to run SpanShot as a daemon that receives telemetry and automatically captures errors from traces, logs, and metrics.

**Why this priority**: This is the core new capability—receiving OTEL telemetry and applying SpanShot's error capture. Without this, there is no OTEL integration.

**Independent Test**: Start `spanshot start --otel :4318`, run an OTEL-instrumented app that produces an error span, verify capture appears in `spanshot status`.

**Acceptance Scenarios**:

1. **Given** I run `spanshot start --otel :4318`, **When** the daemon starts successfully, **Then** it listens for OTLP/HTTP traffic on port 4318 and displays a confirmation message
1. **Given** the daemon is running, **When** an OTEL-instrumented app sends a trace with a span status ERROR, **Then** a capture is saved to `.spanshot/captures/` with the error span and its context
1. **Given** the daemon is running, **When** an OTEL-instrumented app sends logs with severity ERROR or higher, **Then** a capture is saved with the error log and surrounding context
1. **Given** the daemon is running, **When** I run `spanshot stop`, **Then** the daemon shuts down gracefully with a summary of captures saved

______________________________________________________________________

### User Story 2 - Daemon Lifecycle Management (Priority: P1)

A developer wants to start SpanShot as a background daemon and manage its lifecycle (start, stop, status) without keeping a terminal open.

**Why this priority**: Daemon infrastructure is foundational—without it, OTEL receiver cannot run in the background persistently.

**Independent Test**: Run `spanshot start`, close terminal, verify daemon still runs, run `spanshot stop` in new terminal.

**Acceptance Scenarios**:

1. **Given** no daemon is running, **When** I run `spanshot start`, **Then** a daemon process starts in the background and a PID file is created
1. **Given** a daemon is running, **When** I run `spanshot status`, **Then** I see the daemon status (running/stopped), uptime, OTEL endpoint, and recent capture count
1. **Given** a daemon is running, **When** I run `spanshot stop`, **Then** the daemon shuts down gracefully, saves pending captures, and removes the PID file
1. **Given** a daemon is running, **When** I run `spanshot start`, **Then** I receive a warning that a daemon is already running with its PID
1. **Given** the daemon receives SIGTERM or SIGINT, **When** the signal is received, **Then** it performs graceful shutdown (flush captures, close connections)

______________________________________________________________________

### User Story 3 - View OTEL Captures (Priority: P2)

A developer wants to view captures from OTEL telemetry using the same `status` and `show` commands used for wrap/session captures.

**Why this priority**: Unified viewing experience across input modes. Builds on existing v0.2 commands.

**Independent Test**: After OTEL captures are saved, run `spanshot show 1` and verify OTEL-specific context (trace ID, span hierarchy) is displayed.

**Acceptance Scenarios**:

1. **Given** OTEL captures exist, **When** I run `spanshot status`, **Then** I see a list of captures showing source type (OTEL), timestamp, and error summary
1. **Given** OTEL trace captures exist, **When** I run `spanshot show N`, **Then** I see the error span, parent spans, sibling spans, trace ID, and span attributes
1. **Given** OTEL log captures exist, **When** I run `spanshot show N`, **Then** I see the error log, severity, surrounding logs in the time window, and any associated trace ID
1. **Given** captures from both wrap/session and OTEL exist, **When** I run `spanshot status`, **Then** both types appear in a unified list sorted by timestamp

______________________________________________________________________

### User Story 4 - OTEL Signal Support (Priority: P2)

A developer wants SpanShot to capture errors from all three OTEL signal types: traces, logs, and metrics.

**Why this priority**: Full OTEL support differentiates SpanShot. Traces are richest, but logs and metrics provide additional error signals.

**Independent Test**: Send traces, logs, and metrics with errors via OTLP, verify each creates appropriate captures.

**Acceptance Scenarios**:

1. **Given** the daemon receives a trace with span status ERROR, **When** the span is processed, **Then** a capture is created with the error span and its trace context (parent, siblings, attributes)
1. **Given** the daemon receives a trace with an exception event, **When** the span is processed, **Then** a capture is created including the exception message and stack trace
1. **Given** the daemon receives a log with severity ERROR, FATAL, or CRITICAL, **When** the log is processed, **Then** a capture is created with the log and time-window context
1. **Given** the daemon receives metrics, **When** metrics are processed, **Then** they are stored for potential error correlation (error detection from metrics is deferred)

______________________________________________________________________

### Edge Cases

- What happens when the daemon crashes unexpectedly? (PID file remains stale; `spanshot start` detects and cleans up)
- What happens when OTLP endpoint receives malformed data? (Log warning, skip bad data, continue processing)
- What happens when captures exceed storage limit? (Apply existing LRU eviction from v0.2)
- What happens when multiple apps send telemetry simultaneously? (Handle concurrent requests; captures are thread-safe)
- What happens when trace spans arrive out of order? (Buffer briefly to reconstruct trace hierarchy before capture)
- What happens when a span has status ERROR but no exception? (Still capture—status ERROR is sufficient trigger)

## Requirements *(mandatory)*

### Functional Requirements

**Daemon Infrastructure**

- **FR-001**: System MUST provide `start` command that launches a background daemon process
- **FR-002**: System MUST create a PID file when daemon starts and remove it on clean shutdown
- **FR-003**: System MUST provide `stop` command that gracefully shuts down the daemon
- **FR-004**: System MUST handle SIGTERM and SIGINT signals for graceful shutdown
- **FR-005**: System MUST detect and report if a daemon is already running when `start` is invoked
- **FR-006**: System MUST extend `status` command to show daemon state (running/stopped, PID, uptime)

**OTEL Receiver**

- **FR-007**: System MUST accept OTLP/HTTP requests on a configurable port (default: 4318)
- **FR-008**: System MUST accept JSON-encoded OTLP payloads (Protobuf encoding deferred)
- **FR-009**: System MUST receive and process OTEL traces (spans with context)
- **FR-010**: System MUST receive and process OTEL logs
- **FR-011**: System MUST receive and process OTEL metrics (storage only; error detection from metrics deferred)
- **FR-012**: System MUST respond with appropriate OTLP success/error responses

**Error Capture from OTEL**

- **FR-013**: System MUST capture traces where any span has status code ERROR
- **FR-014**: System MUST capture traces where any span has exception events attached
- **FR-015**: System MUST capture logs with severity level ERROR, FATAL, or CRITICAL
- **FR-016**: System MUST include trace context (trace ID, parent span, sibling spans) in trace captures
- **FR-017**: System MUST include time-window context for log captures (logs before/after error)
- **FR-018**: System MUST correlate logs to traces when trace ID is present in log attributes
- **FR-019**: System MUST store captures in the existing `.spanshot/captures/` directory format

**Unified Detection System**

- **FR-022**: System MUST use a unified `DetectionRule` abstraction that supports both regex patterns (for text signals) and semantic matchers (for OTEL signals)
- **FR-023**: System MUST apply appropriate detection rule variant based on signal type (regex for terminal/logfile, semantic for OTEL traces/logs)
- **FR-027**: System MUST provide sensible default detection rules that work without user configuration (zero-config experience)
- **FR-028**: System SHOULD design the detection rule architecture to support future extensibility via OTEL attributes (deferred to post-v0.3)
- **FR-029**: System MUST make ALL detection rules configurable (enable/disable/customize)—sensible defaults are a starting point, not fixed behavior
- **FR-030**: System MUST apply regex detection rules to OTEL log body text by default (configurable)
- **FR-032**: System MUST allow enabling/disabling individual default rules: span ERROR, exception events, log severity ERROR+, regex on OTEL logs

**Shared Capture Pipeline**

- **FR-024**: System MUST normalize events from all input sources (terminal, logfile, OTEL traces, OTEL logs, OTEL metrics) into a common event format before processing
- **FR-025**: System MUST use a single capture engine that processes normalized events regardless of source
- **FR-026**: System MUST produce a unified `Capture` type with source-specific context embedded (e.g., trace ID for OTEL, command for wrap)
- **FR-031**: System MUST store OTEL metrics in the shared pipeline for future correlation and detection (metric-based detection rules deferred)

**Unified Experience**

- **FR-020**: System MUST display OTEL captures in `status` and `show` commands alongside wrap/session captures
- **FR-021**: System MUST indicate capture source type (wrap, session, otel-trace, otel-log) in status output

### Key Entities

- **Daemon**: A background process with PID, start time, OTEL endpoint configuration, and running state
- **DetectionRule**: A unified rule type with variants—regex patterns for text-based signals (terminal, log files), semantic matchers for OTEL signals (span status, log severity, exception events, span attributes), and threshold/rate matchers for metrics (deferred). All input modes share this abstraction.
- **NormalizedEvent**: A common event format that all input sources produce after initial parsing. Contains timestamp, source type, content, and source-specific metadata. Feeds into the shared capture pipeline.
- **OTELTrace**: A collection of spans sharing a trace ID, with hierarchical parent-child relationships
- **OTELSpan**: A unit of work with span ID, parent span ID, status, attributes, and optional exception events
- **OTELLog**: A log record with timestamp, severity, body, and optional trace correlation
- **OTELMetric**: A metric data point (gauge, counter, histogram) with labels and value
- **Capture**: Extended from v0.2 to include OTEL-specific context (trace ID, span hierarchy, signal type)

## Clarifications

### Session 2026-03-07

- Q: What OTLP protocol variant? → A: OTLP/HTTP with JSON encoding (gRPC and Protobuf deferred)
- Q: Which OTEL signals to support? → A: All three (traces, logs, metrics), but error detection from metrics deferred
- Q: Incoming authentication? → A: Deferred; rely on network isolation for now
- Q: Upstream forwarding? → A: Deferred to v0.3.x; v0.3 focuses on receive + capture
- Q: Error detection strategy? → A: OTEL-semantic defaults (span status ERROR, log severity ERROR+); custom rules deferred
- Q: Detection rule architecture? → A: Unified `DetectionRule` type with variants (regex for text, semantic for OTEL); capture engine is source-agnostic
- Q: Capture pipeline architecture? → A: Shared pipeline—all sources normalize to common event format, feed single capture engine, produce unified `Capture` output
- Q: OTEL detection rule extensibility? → A: Sensible defaults in v0.3 (span ERROR, log severity ERROR+, exceptions); architecture supports future custom rules via OTEL attributes, but automatic detection is the core value
- Q: Default OTEL detection rules? → A: Minimal universal set—span status ERROR, exception events on spans, log severity ERROR/FATAL/CRITICAL. HTTP status codes and other domain-specific rules deferred to custom rules.
- Q: Regex rules on OTEL log body? → A: Yes by default, regex rules apply to OTEL log body text—consistent detection across all log sources; configurable (can be disabled)
- Q: OTEL metrics in unified system? → A: Metrics flow through shared pipeline (normalized, stored); detection rules for metrics (threshold/rate-based) deferred but architecture supports them
- Q: Detection rule configurability? → A: ALL detection rules are configurable (enable/disable/customize)—sensible defaults are a starting point, not fixed behavior

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developer can start OTEL receiver daemon with a single command and have it running within 2 seconds
- **SC-002**: Developer can stop daemon gracefully with all pending captures saved within 5 seconds
- **SC-003**: OTEL-instrumented applications can send telemetry to SpanShot by changing only the OTLP endpoint environment variable
- **SC-004**: 100% of spans with status ERROR or exception events are captured (no silent drops)
- **SC-005**: 100% of logs with severity ERROR or higher are captured
- **SC-006**: Trace captures include full trace context (all spans in the trace, not just the error span)
- **SC-007**: Captures from OTEL and wrap/session appear in unified `status` output
- **SC-008**: Daemon survives terminal close and runs persistently until explicitly stopped
- **SC-009**: Daemon handles 100 concurrent OTLP requests without dropping data
- **SC-010**: Error detection works with zero configuration—defaults capture all standard OTEL error signals without user-defined rules
