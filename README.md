<div align="center">
  <img src="spanshot-logo.svg" alt="SpanShot Logo" width="400"/>
</div>

# SpanShot

[![CI](https://github.com/albertodvp/spanshot/actions/workflows/ci.yml/badge.svg)](https://github.com/albertodvp/spanshot/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/albertodvp/spanshot/branch/main/graph/badge.svg)](https://codecov.io/gh/albertodvp/spanshot)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

SpanShot is a local command-line tool that records errors together with the log
events immediately before and after them. The resulting capture provides a
small, reproducible snapshot of what happened around a failure.

SpanShot currently runs a command, monitors its standard output and standard
error, detects configured patterns, and stores captures as JSON. It does not
send source code or logs to a remote service.

## How it works

```text
command output -> collect events -> detect errors -> capture context -> store JSON
```

Each capture contains:

- the event that triggered detection;
- a time-bounded pre-error window;
- a time-bounded post-error window;
- the matching detection rules and capture metadata.

Collection and capture are deterministic and do not use AI. Automated analysis
is planned, but is not part of the current public release.

## Requirements

- Nix with flakes; or
- GHC 9.12.2+ and Cabal 3.10+.

The project is tested on Linux, macOS, and Windows with GHC 9.12.

## Build

With Nix:

```bash
nix develop
just build
```

With Cabal:

```bash
cd hs-spanshot
cabal build
```

To run the test suites:

```bash
just test
```

## Usage

Run a command under SpanShot:

```bash
spanshot exec npm test
spanshot exec cargo build
spanshot exec -- docker compose up
```

The child process output remains visible and its exit code is preserved.
Detected errors are stored in `.spanshot/captures/` in the current project.

List stored captures:

```bash
spanshot captures list
```

Show the most recent capture:

```bash
spanshot captures show 1
spanshot captures show 1 --json
```

## Configuration

SpanShot combines configuration from three levels:

1. built-in defaults;
1. user configuration at `~/.config/spanshot/config.yaml`;
1. project configuration at `.spanshot.yaml` in the Git project root.

Project values override user values field by field.

```bash
spanshot config show
spanshot config path
spanshot config validate
spanshot config init
spanshot config init --user
```

Example `.spanshot.yaml`:

```yaml
capture:
  pre_window_duration: 5
  post_window_duration: 5
  min_context_events: 10
  max_post_window_events: 1000
  max_captures: 100
  detection_rules:
    - regex_pattern: "ERROR"
    - regex_pattern: "FATAL"
    - regex_pattern: "Exception"
```

The pre-window uses time-based retention while preserving at least
`min_context_events`. The post-window is bounded by both duration and event
count. When `max_captures` is exceeded, the oldest stored captures are removed.

## Project structure

```text
hs-spanshot/
├── app/Main.hs       CLI commands
├── src/Collect.hs    output collection and event creation
├── src/Capture.hs    detection and context-window state machine
├── src/Config.hs     hierarchical YAML configuration
├── src/Storage.hs    capture persistence and retention
├── src/Types.hs      core domain types
└── src/Wrap.hs       monitored process execution
```

The implementation keeps capture logic pure where practical and isolates
process, filesystem, and terminal effects at the edges.

## Current scope

The public v0.2 release supports:

- monitoring one child process at a time;
- merged stdout and stderr collection;
- configurable regular-expression detection;
- bounded pre- and post-error context;
- local JSON storage with retention limits;
- human-readable and JSON capture inspection.

Current limitations include line-oriented parsing, no log rotation handling,
and no structured JSON/logfmt field extraction. Daemon operation, OpenTelemetry
input, automated diagnosis, and notifications are future work.

## Development

Common commands:

```bash
just build
just test
just test-unit
just test-integration-cli
nix fmt
```

Feature work follows the specifications under [`specs/`](specs/) and the
project principles in [`.specify/memory/constitution.md`](.specify/memory/constitution.md).

Contributions use the Angular commit convention:

```text
<type>(<scope>): <subject>
```

## License

[Apache License 2.0](LICENSE)
