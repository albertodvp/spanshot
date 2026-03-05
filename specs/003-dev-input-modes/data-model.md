# Data Model: Developer-First Input Modes

**Feature**: 003-dev-input-modes
**Date**: 2026-03-04

## Entities

### Session (NEW)

A PTY-based monitored shell instance.

| Field | Type | Description |
|-------|------|-------------|
| `sessionId` | `Text` | UUID v4 identifier |
| `startTime` | `UTCTime` | When session started |
| `shellPath` | `FilePath` | Path to shell executable (e.g., `/bin/bash`) |
| `captureIds` | `[Text]` | List of capture IDs created during this session |
| `isActive` | `Bool` | Whether session is currently running |

**Validation rules**:

- `sessionId` must be valid UUID format
- `shellPath` must exist and be executable
- `captureIds` references must point to existing captures

**State transitions**:

```
Created → Active → Ended
           ↓
        (on error)
           ↓
         Ended
```

### Capture (SpanShot) - EXTENDED

Existing type from `Types.hs`, extended with session reference.

| Field | Type | Description |
|-------|------|-------------|
| `captureId` | `Text` | Unique identifier (timestamp-based) |
| `errorEvent` | `CollectEvent` | The error line that triggered capture |
| `preWindow` | `[CollectEvent]` | Events before error (within time window) |
| `postWindow` | `[CollectEvent]` | Events after error (within time window) |
| `detectedBy` | `[DetectionRule]` | Rules that matched the error |
| `capturedAtUtc` | `UTCTime` | When capture was created |
| `sessionId` | `Maybe Text` | **NEW**: Session that created this capture (Nothing for file-based) |
| `source` | `CaptureSource` | **NEW**: How capture was created |

### CaptureSource (NEW)

Enum indicating how a capture was created.

```haskell
data CaptureSource
  = SessionCapture Text     -- Session ID
  | WrapCapture Text        -- Command that was wrapped
  | FileCapture FilePath    -- Log file path
  deriving (Show, Eq, Generic)
```

### CollectEvent (EXISTING)

No changes. Already defined in `Types.hs`.

| Field | Type | Description |
|-------|------|-------------|
| `source` | `Text` | File path or PTY identifier |
| `sessionOrderId` | `Int` | Sequential order within session |
| `readAtUtc` | `UTCTime` | When line was read |
| `line` | `Text` | Raw line content |

### DetectionRule (EXISTING)

No changes. Already defined in `Types.hs`.

| Field | Type | Description |
|-------|------|-------------|
| `regexPattern` | `Text` | Regex pattern to match errors |

### CaptureConfig (EXTENDED)

Configuration for capture behavior.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `preWindowDuration` | `Int` | 5 | Seconds before error |
| `postWindowDuration` | `Int` | 5 | Seconds after error |
| `detectionRules` | `[DetectionRule]` | `[ERROR, FATAL]` | Patterns to detect |
| `maxCaptures` | `Int` | 100 | **NEW**: Max captures to keep (LRU) |

## Relationships

```
Session 1 ──────< Capture (via sessionId)
    │
    └── has many captures during its lifetime

Capture 1 ──────1 CollectEvent (errorEvent)
    │
    ├── has many CollectEvent (preWindow)
    └── has many CollectEvent (postWindow)

CaptureConfig 1 ──────< DetectionRule
```

## Storage Layout

```
.spanshot/
├── captures/
│   ├── 2026-03-04-001.json    # Individual capture files
│   ├── 2026-03-04-002.json
│   └── ...
├── .session.lock              # Lock file for concurrent session detection
└── config.yaml                # User configuration (existing)
```

### Capture File Schema

```json
{
  "capture_id": "2026-03-04-001",
  "error_event": {
    "source": "pty:abc123",
    "session_order_id": 42,
    "read_at_utc": "2026-03-04T10:15:30.123Z",
    "line": "ERROR: Connection failed"
  },
  "pre_window": [...],
  "post_window": [...],
  "detected_by": [
    {"regex_pattern": "ERROR"}
  ],
  "captured_at_utc": "2026-03-04T10:15:35.456Z",
  "session_id": "abc123-def456-...",
  "source": {"tag": "SessionCapture", "contents": "abc123-def456-..."}
}
```

## Invariants

1. **Capture limit**: Number of files in `.spanshot/captures/` ≤ `maxCaptures`
1. **Session uniqueness**: Only one active session per terminal (lock file enforced with warning)
1. **Capture ordering**: `captureId` format ensures lexicographic ordering equals chronological ordering
1. **Window bounds**: `preWindow` and `postWindow` respect configured durations
