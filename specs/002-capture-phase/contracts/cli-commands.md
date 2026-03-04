# CLI Command Contracts: Capture Phase (v0.1)

**Feature Branch**: `002-capture-phase`
**Date**: 2026-03-04

## Command: `spanshot capture`

Capture errors from a log file with context windows.

### Synopsis

```
spanshot capture --logfile <PATH> --regex-pattern <PATTERN> --pre-window <SECONDS> --post-window <SECONDS> [--verbose]
```

### Arguments

| Flag | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| `--logfile` | `PATH` | Yes | - | Path to the log file to process |
| `--regex-pattern` | `STRING` | Yes | - | Regex pattern to detect errors |
| `--pre-window` | `INT` | Yes | - | Pre-context window duration in seconds |
| `--post-window` | `INT` | Yes | - | Post-context window duration in seconds |
| `--verbose` | `FLAG` | No | false | Emit progress/status to stderr |

### Output

- **stdout**: JSONL format, one SpanShot per line
- **stderr**: Error messages, progress info (if `--verbose`)

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (including zero captures) |
| 1 | General error (invalid args, file not found, invalid regex) |
| 130 | Interrupted by SIGINT |

### Examples

```bash
# Basic usage
spanshot capture --logfile app.log --regex-pattern "ERROR" --pre-window 5 --post-window 5

# With verbose output
spanshot capture --logfile /var/log/app.log --regex-pattern "FATAL|ERROR" \
    --pre-window 10 --post-window 10 --verbose

# Pipe to jq for filtering
spanshot capture --logfile app.log --regex-pattern "ERROR" \
    --pre-window 5 --post-window 5 | jq '.error_event.line'
```

### Error Messages

| Scenario | Message |
|----------|---------|
| File not found | `Error: File not found: <path>` |
| Permission denied | `Error: Permission denied: <path>` |
| Invalid regex | `Error: Invalid regex pattern: <pattern>` |
| Invalid window | `Error: Window duration must be positive` |

______________________________________________________________________

## Command: `spanshot run`

Full pipeline: collect log events and capture errors continuously.

### Synopsis

```
spanshot run --logfile <PATH> [--verbose]
```

### Arguments

| Flag | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| `--logfile` | `PATH` | Yes | - | Path to the log file to monitor |
| `--verbose` | `FLAG` | No | false | Emit progress/status to stderr |

### Behavior

- Reads capture configuration from config file (detection rules, window durations)
- Tails the log file continuously (like `tail -f`)
- Outputs SpanShots as errors are detected and post-windows complete
- Handles SIGINT gracefully: emits any in-flight captures before exit

### Output

- **stdout**: JSONL format, one SpanShot per line (streaming)
- **stderr**: Error messages, progress info (if `--verbose`)

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Normal termination (file closed, EOF on non-tailing mode) |
| 1 | General error (invalid config, file not found) |
| 130 | Interrupted by SIGINT (graceful shutdown) |

### Examples

```bash
# Monitor a log file
spanshot run --logfile /var/log/app.log

# With verbose output
spanshot run --logfile app.log --verbose

# Process output in real-time
spanshot run --logfile app.log | while read line; do
    echo "$line" | jq -r '.error_event.line'
done

# Save captures to file
spanshot run --logfile app.log > captures.jsonl
```

### Configuration

Uses settings from config file (`.spanshot.yaml` or `~/.config/spanshot/config.yaml`):

```yaml
capture:
  pre_window_seconds: 5
  post_window_seconds: 5
  min_context_events: 10
  detection_rules:
    - regex_pattern: "ERROR"
    - regex_pattern: "FATAL"
```

______________________________________________________________________

## SpanShot JSON Schema

Output format for both `capture` and `run` commands.

### Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["error_event", "pre_window", "post_window", "detected_by", "captured_at_utc"],
  "properties": {
    "error_event": {
      "type": "object",
      "required": ["source", "session_order_id", "read_at_utc", "line"],
      "properties": {
        "source": { "type": "string" },
        "session_order_id": { "type": "integer", "minimum": 0 },
        "read_at_utc": { "type": "string", "format": "date-time" },
        "line": { "type": "string" }
      }
    },
    "pre_window": {
      "type": "array",
      "items": { "$ref": "#/definitions/collect_event" }
    },
    "post_window": {
      "type": "array",
      "items": { "$ref": "#/definitions/collect_event" }
    },
    "detected_by": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["regex_pattern"],
        "properties": {
          "regex_pattern": { "type": "string" }
        }
      }
    },
    "captured_at_utc": { "type": "string", "format": "date-time" }
  },
  "definitions": {
    "collect_event": {
      "type": "object",
      "required": ["source", "session_order_id", "read_at_utc", "line"],
      "properties": {
        "source": { "type": "string" },
        "session_order_id": { "type": "integer", "minimum": 0 },
        "read_at_utc": { "type": "string", "format": "date-time" },
        "line": { "type": "string" }
      }
    }
  }
}
```

### Example Output

```json
{
  "error_event": {
    "source": "app.log",
    "session_order_id": 42,
    "read_at_utc": "2026-03-04T10:15:30.123456Z",
    "line": "2026-03-04 10:15:30 ERROR: Database connection failed"
  },
  "pre_window": [
    {
      "source": "app.log",
      "session_order_id": 40,
      "read_at_utc": "2026-03-04T10:15:28.000000Z",
      "line": "2026-03-04 10:15:28 INFO: Processing request"
    },
    {
      "source": "app.log",
      "session_order_id": 41,
      "read_at_utc": "2026-03-04T10:15:29.000000Z",
      "line": "2026-03-04 10:15:29 DEBUG: Attempting database query"
    }
  ],
  "post_window": [
    {
      "source": "app.log",
      "session_order_id": 43,
      "read_at_utc": "2026-03-04T10:15:31.000000Z",
      "line": "2026-03-04 10:15:31 INFO: Retrying connection"
    }
  ],
  "detected_by": [
    { "regex_pattern": "ERROR" }
  ],
  "captured_at_utc": "2026-03-04T10:15:35.123456Z"
}
```

______________________________________________________________________

## Verbose Mode Output (stderr)

When `--verbose` is enabled, progress information is written to stderr:

```
[spanshot] Monitoring file: /var/log/app.log
[spanshot] Using config: ~/.config/spanshot/config.yaml
[spanshot] Detection rules: 2 patterns loaded
[spanshot] Events processed: 1000
[spanshot] Captures: 3 completed, 1 in-flight
[spanshot] Shutdown requested, emitting in-flight capture...
[spanshot] Exiting gracefully
```

Format: `[spanshot] <message>`

This allows users to see progress while still being able to pipe stdout to other tools.
