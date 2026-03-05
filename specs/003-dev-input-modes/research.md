# Research: Developer-First Input Modes

**Feature**: 003-dev-input-modes
**Date**: 2026-03-04

## Decision 1: PTY Library Choice

**Decision**: Use `posix-pty` package for PTY operations + `unix` for signals

**Rationale**:

- `posix-pty` provides high-level `spawnWithPty` function that handles PTY master/slave creation, process forking, and FD setup in one call
- More lightweight than `vk-posix-pty` (similar API, fewer dependencies)
- `unix` package required anyway for signal handling (SIGINT, SIGWINCH)
- Well-maintained, works on Linux and macOS

**Alternatives considered**:

- `vk-posix-pty`: Similar to posix-pty, slightly more dependencies
- Raw `unix` package only: Too low-level, manual PTY/fork logic error-prone
- `process` package with PTY: Doesn't support PTY, only pipes

## Decision 2: Bidirectional I/O Architecture

**Decision**: Use `async` package with three concurrent threads

**Rationale**:

- Thread 1: Read PTY output → forward to stdout AND feed to capture pipeline
- Thread 2: Read stdin → forward to PTY input
- Thread 3: Signal handling (SIGINT forwarding, SIGWINCH resize)
- `async` provides `race_` and `concurrently` for clean concurrent composition
- GHC I/O manager handles non-blocking I/O efficiently

**Alternatives considered**:

- Single-threaded with select/poll: Complex, error-prone in Haskell
- Event-driven with callbacks: Doesn't fit functional style
- Green threads without async: Harder to manage lifecycle

## Decision 3: Capture Storage Format

**Decision**: JSON files in `.spanshot/captures/` with timestamp-based naming

**Rationale**:

- Format: `YYYY-MM-DD-NNN.json` (e.g., `2026-03-04-001.json`)
- Each file contains one SpanShot with error event + pre/post windows
- Simple filesystem-based storage, no database dependency
- Easy to inspect, grep, and process with `jq`
- LRU eviction: count files, delete oldest when > 100 (configurable)

**Alternatives considered**:

- SQLite: Overkill for single-user CLI tool
- Single append-only JSONL file: Harder to implement LRU eviction
- Binary format: Less debuggable, no benefit for small captures

## Decision 4: Session State Tracking

**Decision**: Session state stored in memory (IORef/TVar) during session; no persistent session file

**Rationale**:

- Session is transient - exists only while PTY is running
- Session ID used to correlate captures saved during that session
- Concurrent session detection via lock file (`.spanshot/.session.lock`)
- On exit, session state not persisted (captures already saved individually)

**Alternatives considered**:

- Persistent session file: Unnecessary complexity, session is transient
- Daemon with session registry: Overkill per earlier decision to avoid daemon

## Decision 5: Signal Handling Strategy

**Decision**: Install custom handlers for SIGINT and SIGWINCH; forward SIGINT to child

**Rationale**:

- SIGINT (Ctrl+C): Forward to child process group, don't kill parent immediately
- Parent catches signal, saves pending captures, then exits gracefully
- SIGWINCH (terminal resize): Update PTY dimensions via `resizePty`
- Use `bracket` pattern for cleanup on any exit path

**Alternatives considered**:

- Ignore SIGINT: Child would receive it directly but parent state unclear
- Default handlers: Would kill parent before captures saved

## Decision 6: Windows Stub Implementation

**Decision**: Compile-time conditional with CPP pragma; runtime "not supported" error

**Rationale**:

- Use `#if !defined(mingw32_HOST_OS)` for Unix-only code
- Windows code path returns clear error message and exits with code 1
- Keeps codebase simple, avoids ConPTY complexity
- Matches pattern from existing `feat/daemon` branch

**Alternatives considered**:

- ConPTY support: Complex, different API, limited value for target users
- No Windows build: Would break cross-platform CI

## Key Implementation Notes

### PTY Session Lifecycle

```
1. Create PTY pair (spawnWithPty)
2. Install signal handlers
3. Start concurrent I/O threads:
   - PTY output → stdout + capture buffer
   - stdin → PTY input
4. Feed capture buffer to existing Capture.hs pipeline
5. On child exit or SIGINT:
   - Save pending captures
   - Print session summary
   - Exit with child's exit code
```

### Dependencies to Add

```cabal
build-depends:
  posix-pty >= 0.2,    -- PTY operations
  unix >= 2.7,         -- Signals, already a dependency
  async >= 2.2,        -- Concurrent I/O
```

### Platform-Specific Code Pattern

```haskell
{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
runSession :: IO ()
runSession = do
  hPutStrLn stderr "Error: session mode not supported on Windows"
  exitWith (ExitFailure 1)
#else
runSession :: IO ()
runSession = do
  -- Unix PTY implementation
  ...
#endif
```
