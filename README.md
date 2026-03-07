<div align="center">
  <img src="spanshot-logo.svg" alt="SpanShot Logo" width="400"/>
</div>

# SpanShot

[![CI](https://github.com/albertodvp/spanshot/actions/workflows/ci.yml/badge.svg)](https://github.com/albertodvp/spanshot/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/albertodvp/spanshot/branch/main/graph/badge.svg)](https://codecov.io/gh/albertodvp/spanshot)
![CodeRabbit Pull Request Reviews](https://img.shields.io/coderabbit/prs/github/albertodvp/spanshot?utm_source=oss&utm_medium=github&utm_campaign=albertodvp%2Fspanshot&labelColor=171717&color=FF570A&link=https%3A%2F%2Fcoderabbit.ai&label=CodeRabbit+Reviews)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

> Catch the error. Keep the flow.

**Cross-platform testing:** Ubuntu, macOS, and Windows with GHC 9.12

## The Problem

Developers waste **10-30 minutes per error** manually:

- Hunting through scattered logs
- Copy-pasting to ChatGPT
- Gathering code context (files, git history, stack traces)
- Reasoning about root causes

With **5-8 errors per day**, this compounds into **hours of lost productivity**.

## The Solution

SpanShot is a **local CLI daemon** that automatically:

1. **Watches** your logs
1. **Detects** errors with temporal context (the "span window" of what happened before/after)
1. **Diagnoses** root causes using AI with full code context
1. **Delivers** actionable fixes through notifications

All **without leaving your terminal**.

## The Impact

**Reduces error diagnosis time from 10-30 minutes to under 1 minute** by automating context gathering and AI analysis, while keeping you in flow state.

- No cloud backend
- No signup
- BYOK (bring your own API key)
- Open source
- Privacy-first (local-first, optional local AI)

## Pipeline Vision

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│   COLLECT    │───▶│   CAPTURE    │───▶│   ANALYZE    │───▶│   DELIVER    │
└──────────────┘    └──────────────┘    └──────────────┘    └──────────────┘
  Gather logs        Detect errors       AI reasoning        Get fixes
  Unify sources      Span window         Root cause          Notifications
```

| Phase | Purpose | Input | Output | AI Used? |
|-------|---------|-------|--------|----------|
| **Collect** | Unify scattered error sources into one stream | Raw logs, stdout, Docker | Normalized event stream | No |
| **Capture** | Identify errors + capture temporal context (span) | Event stream | Error event + span window | No |
| **Analyze** | Understand *why* error happened + *how* to fix | Error + span + code context | Diagnosis + solution | Yes |
| **Deliver** | Present insights without disrupting workflow | Diagnosis | Notification/alert | No |

**Key Insight:** Capture doesn't just find the error line—it captures a **time-window snapshot** (e.g., 5 seconds before/after) to give AI the full story of what led to the failure.

**Current Status:** v0.2 complete - **Input Modes & Storage**. Session mode, wrap mode, and status/show commands are fully implemented. v0.3 (Daemon + OTEL) in development.

## Daemon Mode Vision

SpanShot is designed to run as an **autonomous daemon** that watches, captures, analyzes, and delivers—without manual intervention:

```bash
spanshot start                    # Start daemon in current project
# ... you work normally ...
# Errors are captured, analyzed, and insights delivered automatically
spanshot stop                     # Stop daemon
```

### Two Input Modes

SpanShot supports two complementary input modes for different use cases:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            SPANSHOT MODES                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌─────────────────────┐          ┌─────────────────────────────────────┐ │
│   │   wrap/session      │          │   OTEL Mode                         │ │
│   │   (non-OTEL apps)   │          │   (OTEL-instrumented apps)          │ │
│   │                     │          │                                     │ │
│   │   Terminal capture  │          │   Receives OTLP                     │ │
│   │   PTY-based         │          │   Captures errors from spans/logs   │ │
│   └──────────┬──────────┘          │   Optionally forwards upstream      │ │
│              │                     └──────────────────┬──────────────────┘ │
│              │                                        │                    │
│              └────────────────┬───────────────────────┘                    │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Capture   │                                     │
│                        │   Engine    │                                     │
│                        └──────┬──────┘                                     │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Analyze   │  (AI agent)                         │
│                        └──────┬──────┘                                     │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Deliver   │  (notifications)                    │
│                        └─────────────┘                                     │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

| Mode | Best For | Input |
|------|----------|-------|
| **wrap/session** | Non-OTEL apps, quick debugging | Terminal stdout/stderr |
| **OTEL** | Instrumented apps, production parity | OTLP traces, logs, metrics |

**Key Insight:** For OTEL-instrumented apps, the local and production experience is identical—same daemon, same detection, same captures.

### Unified Error Detection

All input sources flow through a single, cohesive detection system:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     UNIFIED ERROR DETECTION SYSTEM                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   Input Sources              Normalization           Detection Rules        │
│   ─────────────              ─────────────           ───────────────        │
│                                                      (all configurable,     │
│   Terminal (PTY)  ─┐                                  sensible defaults)    │
│   Log files       ─┤                                                        │
│   OTEL traces     ─┼──► NormalizedEvent ──────────► ┌─ Regex patterns       │
│   OTEL logs       ─┤                                ├─ Semantic matchers    │
│   OTEL metrics    ─┘                                │  (span ERROR,         │
│                                                     │   exceptions,         │
│                                                     │   log severity)       │
│                                                     ├─ Regex on log body    │
│                                                     └─ Threshold/rate*      │
│                              │                                              │
│                              ▼                                              │
│                    ┌─────────────────┐                                      │
│                    │ Capture Engine  │  (single, source-agnostic)           │
│                    └────────┬────────┘                                      │
│                             │                                               │
│                             ▼                                               │
│                    ┌─────────────────┐                                      │
│                    │ Unified Capture │  (with source-specific context)      │
│                    └─────────────────┘                                      │
│                                                                             │
│   * For metrics (architecture ready, detection rules planned)               │
│                                                                             │
│   Design Principle: Sensible defaults work out of the box.                  │
│                     ALL rules are configurable.                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Default Detection Rules (zero-config):**

- Span status ERROR
- Exception events on spans
- Log severity ERROR/FATAL/CRITICAL
- Regex patterns (configurable)

### How It Works

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         SPANSHOT DAEMON LIFECYCLE                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   Developer starts daemon          Error detected              AI analyzes  │
│          │                              │                          │        │
│          ▼                              ▼                          ▼        │
│   ┌─────────────┐    logs flow    ┌───────────┐   triggers   ┌──────────┐  │
│   │   START     │ ──────────────▶ │  CAPTURE  │ ───────────▶ │ ANALYZE  │  │
│   │  (daemon)   │                 │ SpanShot  │              │ (agent)  │  │
│   └─────────────┘                 └───────────┘              └──────────┘  │
│         │                                                          │        │
│         │ monitors:                                                ▼        │
│         │ • log files                                      ┌──────────┐    │
│         │ • stdout/stderr (wrap mode)              stores  │ INSIGHT  │    │
│         │ • terminal session (session mode)         ◀───── │ .spanshot│    │
│         │ • OTEL telemetry (OTLP receiver)                 └──────────┘    │
│         │                                                          │        │
│         │                                                          ▼        │
│         │                                                  ┌──────────┐    │
│         │                                         delivers │ DELIVER  │    │
│         │                                          ◀────── │ to user  │    │
│         │                                                  └──────────┘    │
│         ▼                                                                   │
│   Developer stays in flow, gets notified only with actionable insights     │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Analyze Phase: AI Agent (Planned)

When a SpanShot is captured, the daemon **automatically** invokes an AI agent to diagnose the error:

**Agent Capabilities (BYOK - Bring Your Own Key):**

| Tool | Purpose | Permission Model |
|------|---------|------------------|
| **Read Codebase** | Access source files referenced in stack traces | Default: project files only |
| **Search Code** | Find related code (grep, AST queries) | Default: project scope |
| **Run Scripts** | Execute test commands, linters, build tools | Explicit allowlist required |
| **Git Context** | Read recent commits, blame, diff | Default: read-only |
| **External APIs** | Fetch docs, search StackOverflow | Opt-in per source |

**Permission Model (TBD):**

- Scripts require explicit allowlist in config
- Sandboxed execution environment
- Audit log of all agent actions
- User confirmation for sensitive operations (optional)

**Output: Insight**

The agent produces an **Insight** (working name) containing:

- Root cause analysis
- Relevant code snippets with annotations
- Suggested fix (code diff or steps)
- Confidence level
- References (docs, similar issues)

### Storage: `.spanshot/` Directory

```
.spanshot/
├── config.yaml              # Project configuration
├── captures/                # Raw SpanShots (error + context)
│   ├── 2025-03-04-001.json
│   └── 2025-03-04-002.json
├── insights/                # AI-generated analysis
│   ├── 2025-03-04-001.json  # Linked to capture
│   └── 2025-03-04-002.json
└── sessions/                # Daemon session logs
    └── current.log
```

### Deliver Phase: Smart Notification (Planned)

How insights reach the developer (TBD - exploring options):

| Channel | Use Case | Status |
|---------|----------|--------|
| **Terminal inline** | Working in same terminal | Planned |
| **Desktop notification** | Background daemon, quick alert | Exploring |
| **Editor integration** | Show in VS Code, Neovim, etc. | Future |
| **Web dashboard** | Review multiple insights | Future |
| **CLI review** | `spanshot show`, `spanshot insights` | Planned |

**Design Principle:** Deliver insights **without breaking flow**. A notification should give enough context to decide "fix now" vs "fix later" without forcing a context switch.

### Why OTEL? Structured Context

With OTEL traces, SpanShot receives structured context far richer than log lines:

```
Trace: req-abc123
│
├─ Span: HTTP POST /api/orders (500ms)
│  ├─ attributes: {user_id: 42, cart_size: 3}
│  │
│  ├─ Span: validate_cart (10ms) ✓
│  │
│  ├─ Span: reserve_inventory (200ms) ✓
│  │
│  ├─ Span: charge_payment (250ms) ✗ ERROR
│  │  ├─ attributes: {payment_provider: "stripe", amount: 99.99}
│  │  └─ exception: "PaymentDeclined: insufficient_funds"
│  │
│  └─ Span: send_confirmation (skipped)
```

**What SpanShot captures:**

- The error span + its attributes
- Parent context (what operation was this part of?)
- Sibling spans (what succeeded before failure?)
- Trace attributes (user_id, request_id for correlation)
- Full exception details with stack traces

This is **10x richer** than parsing `ERROR: PaymentDeclined` from a log line.

### The "Span" in SpanShot

The name gains deeper meaning with OTEL:

- **Temporal span**: Time window around error (5 seconds before/after)
- **Distributed span**: OTEL trace span (structural context)

Both meanings are valid and complementary.

## Installation

**Prerequisites:** Nix with flakes (recommended for consistent dev environment with pre-configured tooling) or GHC 9.12.2+ and Cabal 3.10+

```bash
# Enter development environment (Nix)
nix develop

# Build
just build

# Run tests
just test

```

## Quick Start

### Current (v0.1)

```bash
# One-shot capture: process existing log file
spanshot capture --logfile app.log --regex-pattern "ERROR|FATAL"

# Continuous monitoring: tail log file for new errors
spanshot run --logfile app.log

# Stream collection only (JSONL output)
spanshot collect --logfile /var/log/app.log

# Filter with jq
spanshot collect --logfile app.log | jq 'select(.line | contains("ERROR"))'
```

### Input Modes (v0.2)

```bash
# Wrap mode: run command with monitoring (preserves exit code)
spanshot wrap -- npm test
spanshot wrap -- cargo build
spanshot wrap -- docker-compose up

# Session mode: start an interactive PTY session with monitoring
spanshot session
npm test          # captured
cargo build       # captured
exit              # session ends, captures saved

# View recent captures
spanshot status

# Show specific capture (1 = most recent)
spanshot show 1
spanshot show 1 --json   # output as JSON
```

### Future (v0.3 - Daemon + OTEL)

```bash
# Start daemon with OTEL receiver
spanshot start --otel :4318

# Non-OTEL apps: use wrap mode as before
spanshot wrap -- npm test

# OTEL-instrumented apps: point to SpanShot
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 npm test

# View captures (unified across all sources)
spanshot status
spanshot show 1

# Stop daemon
spanshot stop
```

**Production with upstream forwarding (v0.3.x):**

```bash
# SpanShot as transparent proxy - captures errors AND forwards to your observability stack
spanshot start --otel :4318 --upstream https://otlp.grafana.net:4317

# Your app points to SpanShot instead of Grafana directly
# SpanShot captures + analyzes errors, then forwards all telemetry unchanged
```

## Configuration

SpanShot uses hierarchical configuration with project-level overrides:

1. **User config**: `~/.config/spanshot/config.yaml`
1. **Project config**: `.spanshot.yaml` (in git project root)

Project config overrides user config field-by-field. If no config is found, sensible defaults are used.

### Config Commands

```bash
# Show merged configuration (defaults + user + project overrides)
spanshot config show

# Show config file paths and status
spanshot config path

# Initialize a config file (optional - defaults work out of the box)
spanshot config init              # creates .spanshot.yaml at project root
spanshot config init --user       # creates user config
spanshot config init --force      # overwrite existing
```

### Example Config

```yaml
capture:
  pre_window_duration: 5    # seconds before error
  post_window_duration: 5   # seconds after error
  min_context_events: 10

detection:
  # All rules have sensible defaults - configure only what you need to change
  rules:
    # Regex patterns (apply to terminal, logfile, and OTEL log body)
    - regex_pattern: "ERROR"
    - regex_pattern: "FATAL"
    - regex_pattern: "OutOfMemoryError"

    # Semantic rules for OTEL (enabled by default, shown for reference)
    # - semantic: span_status_error      # enabled by default
    # - semantic: exception_events       # enabled by default
    # - semantic: log_severity_error     # enabled by default

  # Fine-tune default behavior
  regex_on_otel_logs: true   # apply regex patterns to OTEL log body (default: true)

# OTEL receiver (v0.3+)
otel:
  listen: ":4318"            # OTLP/HTTP endpoint
  # upstream:                # optional forwarding (v0.3.x)
  #   endpoint: "https://otlp.grafana.net:4317"
  #   headers:
  #     Authorization: "Basic ${GRAFANA_OTLP_TOKEN}"
```

## Output Format

### Collection Events (Current)

Each line is a JSON event:

```json
{"source":"./app.log","session_order_id":0,"read_at_utc":"2025-10-15T16:32:05.123456Z","line":"[INFO] Application started"}
```

**Fields:**

- `source`: Log file path
- `session_order_id`: Sequential counter (resets per run)
- `read_at_utc`: UTC timestamp when event was read
- `line`: Raw line content (no trailing newline)

### Span Snapshots (Coming Soon)

```json
{
  "error_event": {
    "source": "./app.log",
    "session_order_id": 42,
    "read_at_utc": "2025-10-15T16:32:15.123Z",
    "line": "ERROR: Database connection failed"
  },
  "pre_window": [
    {
      "source": "./app.log",
      "session_order_id": 40,
      "read_at_utc": "2025-10-15T16:32:10.000Z",
      "line": "INFO: Connecting to database..."
    },
    {
      "source": "./app.log",
      "session_order_id": 41,
      "read_at_utc": "2025-10-15T16:32:12.000Z",
      "line": "INFO: Retrying connection..."
    }
  ],
  "post_window": [
    {
      "source": "./app.log",
      "session_order_id": 43,
      "read_at_utc": "2025-10-15T16:32:16.000Z",
      "line": "WARN: Falling back to cache"
    },
    {
      "source": "./app.log",
      "session_order_id": 44,
      "read_at_utc": "2025-10-15T16:32:18.000Z",
      "line": "INFO: Request completed with degraded service"
    }
  ],
  "detected_by": [
    {"regex_pattern": "ERROR"}
  ],
  "captured_at_utc": "2025-10-15T16:32:20.456Z"
}
```

## Current Limitations (v0.1)

**Scope:**

- Single file only (no Docker logs, stdout, multiple sources yet)
- No log rotation handling
- Line-based parsing (multi-line stack traces split into separate events)
- No structured field extraction from JSON/logfmt logs
- In-memory state only (no persistence)

**Capture (in progress):**

- Regex patterns only (no keyword or log-level detectors yet)
- Capture pipeline implemented in library only (CLI commands `capture`/`run` not yet wired)
- No hard limit on post-window event count (bounded by `postWindowDuration` only; in high-throughput scenarios, memory usage scales with log volume within that time window)

**Not Yet Built:**

- Analyze phase (AI diagnosis)
- Deliver phase (notifications)

**Platform-Specific:**

- **Windows**: Concurrent file access limitation - When `spanshot` is reading a log file, other processes cannot append to it simultaneously due to Windows' strict file locking. This affects real-time log tailing scenarios where logs are actively being written. Future versions will implement proper file sharing using Win32 API. **Workaround**: Use log rotation or batch processing instead of real-time tailing on Windows.

## Development

```bash
# Run all tests
just test

# Run specific test suite
just test-unit
just test-integration-cli

# Generate coverage report
just test-coverage
just coverage-report

# Build
just build

# Format all files (requires nix develop shell)
nix fmt
```

See [`justfile`](justfile) for all available commands.

## Testing

The project includes two test suites:

1. **Unit tests** - Test core library functionality (streaming, event generation, error detection)
1. **CLI integration tests** - End-to-end validation of the compiled binary, including JSONL output format and error handling

Run with `just test` or `cabal test`.

## Architecture

SpanShot is designed as a modular pipeline with distinct phases:

- **Collect** (`src/Collect.hs`): Stream log files, normalize to JSONL events
- **Capture** (`src/Capture.hs`): Detect errors, capture temporal span windows
- **Session** (`src/Session.hs`): PTY-based terminal monitoring *(v0.2)*
- **OTEL** *(v0.3)*: OTLP/HTTP receiver for traces, logs, metrics
- **Analyze**: AI-powered root cause diagnosis *(planned)*
- **Deliver**: Notification and alert delivery *(planned)*

**Key Abstractions:**

- **NormalizedEvent**: Common event format for all input sources
- **DetectionRule**: Unified rule type with variants (regex, semantic, threshold)
- **Capture**: Error + context snapshot, source-agnostic

**Current Implementation:**

- **Library** (`src/`): Collect (complete), Capture (complete), Session (complete)
- **Executable** (`app/`): CLI with wrap, session, status, show commands
- **Tests** (`test/`): Unit tests and CLI integration tests with fixtures

**Key Principle:** Phases 1-2 (Collect + Capture) are **AI-free** and use deterministic pattern matching. Only Phase 3 (Analyze) uses AI.

## Contributing

This project follows the [Angular Commit Message Convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#commit).

**Types:** `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`

**Format:** `<type>(<scope>): <subject>`

### Feature Development with Speckit

New features are developed using the speckit workflow:

```
/speckit.specify → /speckit.clarify → /speckit.plan → /speckit.tasks → /speckit.implement
```

This ensures consistent specification, planning, and implementation across all features.
See the [Project Constitution](.specify/memory/constitution.md) for development principles and non-negotiable constraints.

## Roadmap

### v0.1 - Collect + Capture (In Progress)

**Collect**

- [x] Stream log files with tail-f behavior
- [x] JSONL output with metadata (source, timestamp, line number)
- [x] UTF-8 decoding with lenient error handling
- [x] Unit tests + CLI integration tests

**Capture**

- [x] Core types (DetectionRule, SpanShot, CaptureOptions)
- [x] Regex-based error detection
- [x] Time-based window buffering (span window: pre/post context)
- [x] Stream combinator (`captureFromStream`)
- [x] CLI commands (`capture` and `run`)
- [ ] CLI integration tests for capture
- [ ] Documentation and examples

### v0.2 - Input Modes & Storage (Complete)

**Input Flexibility**

- [x] `spanshot wrap -- COMMAND` (capture stdout/stderr of any command)
- [x] `spanshot session` (PTY-based terminal session monitoring)
- [x] Exit code passthrough in wrap mode
- [ ] Execution context in captures (command, duration, PWD)

**Capture Storage**

- [x] `.spanshot/captures/` directory structure
- [x] `spanshot status` command to list captures
- [x] `spanshot show N` command to view specific capture
- [x] `spanshot show N --json` flag for JSON output
- [x] Session ID linking (group related captures)

### v0.3 - Daemon Mode + OTEL Integration

**Daemon Infrastructure**

- [ ] `spanshot start` / `spanshot stop` daemon commands
- [ ] Background process with PID management
- [ ] Graceful shutdown and signal handling (SIGTERM, SIGINT)
- [ ] Extended `status` command showing daemon state

**OTEL Receiver**

- [ ] OTLP/HTTP endpoint (JSON encoding, port 4318)
- [ ] Receive and process traces, logs, metrics
- [ ] Error capture from span status ERROR, exceptions, log severity ERROR+

**Unified Detection System**

- [ ] Unified `DetectionRule` abstraction (regex + semantic matchers)
- [ ] Shared capture pipeline with `NormalizedEvent` format
- [ ] All detection rules configurable with sensible defaults
- [ ] Regex rules apply to OTEL log body (configurable)

**v0.3.x - Upstream Forwarding**

- [ ] Forward telemetry to upstream OTEL collector (Grafana, Datadog, etc.)
- [ ] Header-based authentication with env var interpolation

### v0.4 - Analyze (AI Agent)

**Agent Core**

- [ ] BYOK integration (OpenAI, Anthropic, local models)
- [ ] Automatic trigger on SpanShot capture
- [ ] Insight generation and storage

**Agent Tools**

- [ ] Read codebase (project files, stack trace references)
- [ ] Search code (grep, pattern matching)
- [ ] Git context (blame, recent commits, diff)
- [ ] Run scripts (sandboxed, allowlist-based)

**Permission Model**

- [ ] Configuration-based allowlist for script execution
- [ ] Audit logging of agent actions
- [ ] Scoped access (project-only by default)

### v0.5 - Deliver

**Notification Channels**

- [ ] CLI review (`spanshot show`, `spanshot insights`)
- [ ] Terminal inline notifications
- [ ] Desktop notifications (optional)

**Future Channels (Exploring)**

- Editor integration (VS Code, Neovim)
- Web dashboard
- Webhooks / Slack integration

### Configuration (Complete)

- [x] YAML configuration file support
- [x] Hierarchical config (user + project)
- [x] `config init` command
