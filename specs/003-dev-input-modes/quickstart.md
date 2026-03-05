# Quickstart: Developer-First Input Modes

**Feature**: 003-dev-input-modes
**Date**: 2026-03-04

## Prerequisites

- Unix-based system (Linux or macOS)
- SpanShot built and in PATH (`just build`)
- Nix development shell (`nix develop`)

## Session Mode (P1)

Start a monitored terminal session:

```bash
# Start session
spanshot session

# Welcome message appears:
# SpanShot session started (ID: abc123)
# Type 'exit' to end session

# Work normally - errors are captured automatically
$ cargo test
# ... test output with ERROR ...

# Check captured errors
$ spanshot status
  [1] 2026-03-04 10:15:32 - ERROR detected (cargo test)

# View full capture
$ spanshot show 1
  === SpanShot #1 ===
  Captured: 2026-03-04 10:15:32
  Source: session (abc123)

  --- Pre-window ---
  [10:15:30] Running `target/debug/myapp`
  [10:15:31] Compiling myapp v0.1.0

  --- Error ---
  [10:15:32] ERROR: assertion failed: expected 42, got 0

  --- Post-window ---
  [10:15:33] test result: FAILED. 0 passed; 1 failed

# Exit session
$ exit
# SpanShot session ended. 1 capture saved.
```

## Wrap Mode (P2)

Run a single command with monitoring:

```bash
# Wrap a command
spanshot wrap -- npm test

# Output appears normally
# If ERROR detected, capture saved automatically
# Exit code preserved

# Check result
echo $?  # Shows npm's exit code

# View any captures
spanshot status
```

## Run Mode (P3)

Monitor an existing log file:

```bash
# In one terminal, tail a log file
spanshot run --logfile /var/log/app.log

# In another terminal, your app writes to the log
# SpanShot captures errors automatically
```

## View Captures

```bash
# List recent captures
spanshot status

# Show specific capture
spanshot show 1

# Show capture as JSON
spanshot show 1 --json
```

## Configuration

Create `.spanshot.yaml` in project root:

```yaml
capture:
  pre_window_duration: 5    # seconds
  post_window_duration: 5   # seconds
  max_captures: 100         # LRU eviction limit
  detection_rules:
    - regex_pattern: "ERROR"
    - regex_pattern: "FATAL"
    - regex_pattern: "panic:"
```

## Troubleshooting

### "Not supported on Windows"

Session and wrap modes require PTY, which is Unix-only. Use WSL on Windows.

### "Another session is running"

SpanShot detected a concurrent session. This is a warning - you can continue, but captures may interleave.

### No captures appearing

Check your detection rules match your error format:

```bash
spanshot config show
```

Ensure your errors contain patterns like `ERROR`, `FATAL`, etc.
