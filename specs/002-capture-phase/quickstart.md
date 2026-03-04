# Quickstart: Capture Phase (v0.1)

**Feature Branch**: `002-capture-phase`
**Date**: 2026-03-04

## Overview

This feature adds error capture functionality to SpanShot. After implementation, you'll be able to:

1. **Capture errors with context**: `spanshot capture` processes a log file and outputs SpanShots
1. **Monitor logs in real-time**: `spanshot run` continuously watches a log file for errors

## Prerequisites

```bash
# Enter development environment
nix develop

# Build the project
just build
```

## Quick Usage

### Capture errors from a log file

```bash
# Basic capture with explicit settings
spanshot capture \
    --logfile /path/to/app.log \
    --regex-pattern "ERROR|FATAL" \
    --pre-window 5 \
    --post-window 5
```

Output (one JSON per line):

```json
{"error_event":{"line":"ERROR: Connection failed",...},"pre_window":[...],"post_window":[...],...}
```

### Monitor logs continuously

```bash
# Use configuration file for settings
spanshot run --logfile /var/log/app.log

# With verbose progress output
spanshot run --logfile /var/log/app.log --verbose
```

Press `Ctrl+C` to stop - any in-progress captures will be emitted before exit.

## Configuration

Create `.spanshot.yaml` in your project or `~/.config/spanshot/config.yaml`:

```yaml
capture:
  pre_window_seconds: 5
  post_window_seconds: 5
  min_context_events: 10
  detection_rules:
    - regex_pattern: "ERROR"
    - regex_pattern: "FATAL"
    - regex_pattern: "CRITICAL"
```

## Common Patterns

### Filter output with jq

```bash
# Extract just the error lines
spanshot run --logfile app.log | jq -r '.error_event.line'

# Count errors by pattern
spanshot run --logfile app.log | jq '.detected_by[0].regex_pattern' | sort | uniq -c
```

### Save captures to file

```bash
spanshot run --logfile app.log > captures.jsonl &
```

### Process captures as they arrive

```bash
spanshot run --logfile app.log | while read capture; do
    # Send to alerting system
    echo "$capture" | curl -X POST -d @- http://alerts.example.com/api/errors
done
```

## Testing the Feature

### Unit tests

```bash
just test-unit
```

Tests for:

- `captureFromStream` combinator
- Stream finalization (in-flight captures)
- `processEvent` state machine

### Integration tests

```bash
just test-integration-cli
```

Tests for:

- `spanshot capture` command
- `spanshot run` command
- Graceful SIGINT handling

## Architecture Notes

The implementation follows the Constitution principles:

- **Pure Functional Core**: `captureFromStream` is a pure stream transformer
- **CLI-First**: JSONL output, composable with Unix tools
- **Test-First**: All features have corresponding tests

Key files:

- `hs-spanshot/src/Capture.hs` - Stream combinator
- `hs-spanshot/app/Main.hs` - CLI commands
- `hs-spanshot/test/CaptureStreamSpec.hs` - Unit tests
- `hs-spanshot/test/CLIIntegration.hs` - Integration tests
