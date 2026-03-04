# Data Model: Capture Phase (v0.1)

**Feature Branch**: `002-capture-phase`
**Date**: 2026-03-04

## Existing Entities (from Types.hs)

These entities already exist and are used as-is:

### CollectEvent

A timestamped log line from a source file.

| Field | Type | Description |
|-------|------|-------------|
| `source` | `Text` | Source file path |
| `sessionOrderId` | `Int` | Zero-based line number within session |
| `readAtUtc` | `UTCTime` | Timestamp when event was read |
| `line` | `Text` | The log line content (UTF-8 decoded) |

**JSON Serialization**: snake_case field names (`source`, `session_order_id`, `read_at_utc`, `line`)

______________________________________________________________________

### SpanShot

A captured error with surrounding context.

| Field | Type | Description |
|-------|------|-------------|
| `errorEvent` | `CollectEvent` | The event that triggered capture |
| `preWindow` | `[CollectEvent]` | Events before the error (within time window) |
| `postWindow` | `[CollectEvent]` | Events after the error (within time window) |
| `detectedBy` | `[DetectionRule]` | Rules that matched the error event |
| `capturedAtUtc` | `UTCTime` | Timestamp when capture completed |

**JSON Serialization**: snake_case field names

**Invariants**:

- `errorEvent` is never in `preWindow` or `postWindow`
- `preWindow` events have `readAtUtc` < `errorEvent.readAtUtc`
- `postWindow` events have `readAtUtc` > `errorEvent.readAtUtc`

______________________________________________________________________

### DetectionRule

A rule for detecting errors in log lines.

| Variant | Field | Type | Description |
|---------|-------|------|-------------|
| `RegexRule` | `regexPattern` | `String` | Regex pattern to match |

**JSON Serialization**: `{"regex_pattern": "ERROR"}`

______________________________________________________________________

### CaptureOptions

Configuration for capture behavior. Created via `mkCaptureOptions` smart constructor.

| Field | Type | Description | Validation |
|-------|------|-------------|------------|
| `preWindowDuration` | `NominalDiffTime` | How far back to look for context | >= 0 seconds |
| `postWindowDuration` | `NominalDiffTime` | How far forward to look for context | >= 0 seconds |
| `minContextEvents` | `Int` | Minimum events to keep in pre-window | >= 1 |
| `detectionRules` | `[DetectionRule]` | Rules to detect errors | Non-empty |
| `compiledRules` | `[CompiledRule]` | Pre-compiled regex patterns | Internal |

**Validation**: At least one of `preWindowDuration` or `postWindowDuration` must be > 0.

______________________________________________________________________

### CaptureState

Internal state machine for streaming capture.

| Field | Type | Description |
|-------|------|-------------|
| `csPreWindow` | `Seq CollectEvent` | Rolling buffer of recent events |
| `csActiveCapture` | `Maybe ActiveCapture` | In-progress capture, if any |

**Initial State**: `CaptureState Seq.empty Nothing`

______________________________________________________________________

### ActiveCapture

In-progress capture during post-window collection.

| Field | Type | Description |
|-------|------|-------------|
| `acErrorEvent` | `CollectEvent` | The error that started this capture |
| `acDetectedBy` | `[DetectionRule]` | Rules that matched |
| `acPreWindowSnapshot` | `Seq CollectEvent` | Snapshot of pre-window at capture start |
| `acPostEvents` | `Seq CollectEvent` | Post-window events collected so far |

______________________________________________________________________

## State Transitions

```
                    ┌─────────────────────────────────────────┐
                    │                                         │
                    ▼                                         │
┌──────────────────────────────────┐                         │
│         Idle State               │                         │
│  csActiveCapture = Nothing       │                         │
│  csPreWindow = [...events...]    │                         │
└──────────────────────────────────┘                         │
        │                                                     │
        │ Error detected (matchedRules not empty)            │
        ▼                                                     │
┌──────────────────────────────────┐                         │
│      Capturing State             │                         │
│  csActiveCapture = Just cap      │                         │
│  acPostEvents growing            │                         │
└──────────────────────────────────┘                         │
        │                                                     │
        │ postWindowDuration elapsed OR stream ends          │
        │                                                     │
        ▼                                                     │
┌──────────────────────────────────┐                         │
│     Emit SpanShot                │─────────────────────────┘
│  (output to stream)              │
└──────────────────────────────────┘
```

**Transition Rules**:

1. **Idle → Capturing**: When `processEvent` detects an error (non-empty `matchedRules`)
1. **Capturing → Capturing**: Non-error events added to `acPostEvents`
1. **Capturing → Idle + Emit**: When `postWindowDuration` elapses since error, emit SpanShot
1. **Capturing → Idle + Emit**: When stream ends (finalization), emit partial SpanShot

**Single-Active-Capture Policy**: If already capturing, new errors are added to post-window but don't start new captures.

______________________________________________________________________

## New Functions (Capture.hs)

### captureFromStream

```haskell
captureFromStream :: Monad m
                  => CaptureOptions
                  -> Stream (Of CollectEvent) m r
                  -> Stream (Of SpanShot) m r
```

**Purpose**: Transform a stream of collect events into a stream of SpanShots.

**Behavior**:

- Maintains `CaptureState` across events
- Calls existing `processEvent` for each event
- Emits SpanShots when post-window completes
- On stream end, emits any in-flight capture (finalization)

______________________________________________________________________

### finalizeCapture

```haskell
finalizeCapture :: ActiveCapture -> UTCTime -> SpanShot
```

**Purpose**: Convert an in-flight capture to a SpanShot when stream ends early.

**Behavior**:

- Uses current timestamp as `capturedAtUtc`
- Includes all accumulated `acPostEvents` (may be less than full post-window)

______________________________________________________________________

## CLI Data Structures (Main.hs)

### CaptureSettings (NEW)

CLI settings for the `capture` command.

| Field | Type | CLI Flag | Description |
|-------|------|----------|-------------|
| `captureLogfile` | `FilePath` | `--logfile` | Path to log file |
| `capturePattern` | `String` | `--regex-pattern` | Error detection regex |
| `capturePreWindow` | `Int` | `--pre-window` | Pre-window duration (seconds) |
| `capturePostWindow` | `Int` | `--post-window` | Post-window duration (seconds) |
| `captureVerbose` | `Bool` | `--verbose` | Enable progress output to stderr |

______________________________________________________________________

### RunSettings (NEW)

CLI settings for the `run` command.

| Field | Type | CLI Flag | Description |
|-------|------|----------|-------------|
| `runLogfile` | `FilePath` | `--logfile` | Path to log file |
| `runVerbose` | `Bool` | `--verbose` | Enable progress output to stderr |

**Note**: Uses configuration file for detection rules and window settings.

______________________________________________________________________

## Output Format

SpanShots are output as JSONL (one JSON object per line) to stdout:

```json
{"error_event":{"source":"app.log","session_order_id":42,"read_at_utc":"2026-03-04T10:15:30Z","line":"ERROR: Connection failed"},"pre_window":[...],"post_window":[...],"detected_by":[{"regex_pattern":"ERROR"}],"captured_at_utc":"2026-03-04T10:15:35Z"}
```

**Design Notes**:

- JSONL format enables streaming consumption (line-by-line parsing)
- Conforms to Constitution Principle I (CLI-First Architecture)
- Compatible with Unix tools: `spanshot run | jq '.error_event.line'`

______________________________________________________________________

## Documentation Deliverables

### Usage Examples (NEW)

Real-world usage documentation showing how to use SpanShot with actual applications.

**Location**: `docs/usage-examples.md` or README section

**Required Examples**:

1. **Web Server Monitoring**

   - Run a web server that logs to a file
   - Use `spanshot run` to capture errors in real-time
   - Show example output

1. **Background Process**

   - Long-running script/process writing logs
   - Use `spanshot capture` to analyze historical logs
   - Use `spanshot run` for live monitoring

1. **Container/Docker**

   - Application running in Docker
   - Redirect container logs to file
   - Monitor with SpanShot from host or sidecar

**Format**: Step-by-step with copy-paste commands and expected output.

**Note**: These examples show SpanShot as a CLI tool you run alongside your application - it reads log files, it doesn't require code changes to your app.
