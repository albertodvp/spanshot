# Research: Capture Phase (v0.1)

**Feature Branch**: `002-capture-phase`
**Date**: 2026-03-04

## Research Topics

### 1. Stateful Stream Transformers in Haskell's `streaming` Library

**Context**: Need to implement `captureFromStream :: CaptureOptions -> Stream (Of CollectEvent) m r -> Stream (Of SpanShot) m r`

**Decision**: Use direct pattern matching on Stream constructors (`Return`/`Effect`/`Step`) with strict state accumulator.

**Rationale**:

- The canonical pattern from `streaming`'s own `scan` implementation
- Direct constructor matching is more efficient than using `inspect`
- Allows explicit handling of stream finalization

**Core Pattern**:

```haskell
captureFromStream :: Monad m => CaptureOptions -> Stream (Of CollectEvent) m r -> Stream (Of SpanShot) m r
captureFromStream opts = loop initialCaptureState
  where
    loop !state stream = case stream of
        -- FINALIZATION: Stream ended - emit in-flight capture
        Return r ->
            case csActiveCapture state of
                Nothing -> Return r
                Just cap -> Step (finalizeCapture cap :> Return r)

        -- EFFECTS: Preserve monadic layers
        Effect m -> Effect (fmap (loop state) m)

        -- DATA: Process event through existing pure processEvent
        Step (event :> rest) ->
            let (newState, maybeShot) = processEvent opts state event
            in case maybeShot of
                Nothing -> loop newState rest
                Just shot -> Step (shot :> loop newState rest)
```

**Key Points**:

- Use `!state` bang pattern to prevent space leaks
- Must handle finalization in `Return r` case (emit in-flight captures)
- Must preserve return value `r` for stream composition
- Existing `processEvent` pure function slots in directly

**Alternatives Considered**:

- `S.scan` / `S.mapAccum`: Doesn't support finalization (emitting on stream end)
- `conduit` migration: Would require significant refactoring for no benefit
- `streamly`: Different API, would require learning new library

______________________________________________________________________

### 2. SIGINT Handling in Haskell CLI Applications

**Context**: FR-006 requires graceful termination via SIGINT (Ctrl+C), emitting any in-flight captures before exit.

**Decision**: Use MVar-based signal handler pattern with cooperative shutdown.

**Rationale**:

- Explicit control over shutdown timing
- Stream terminates naturally rather than being killed mid-operation
- All in-flight events can be processed and flushed before exit
- Can distinguish between normal completion and interrupted execution

**Core Pattern**:

```haskell
import Control.Concurrent (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import System.Posix.Signals (Handler(..), installHandler, sigINT)

installShutdownHandler :: IO (MVar ())
installShutdownHandler = do
    shutdownMVar <- newEmptyMVar
    let handler = Catch $ putMVar shutdownMVar ()
    _ <- installHandler sigINT handler Nothing
    pure shutdownMVar

checkShutdown :: MVar () -> IO Bool
checkShutdown mvar = do
    result <- tryTakeMVar mvar
    case result of
        Just () -> putMVar mvar () >> pure True  -- Put back for future checks
        Nothing -> pure False
```

**Integration with Streaming**:

- Check `shutdownMVar` in polling loop (`bytesWithPolling`)
- When shutdown detected, terminate stream gracefully
- Finalization in `captureFromStream` emits any in-flight capture
- Return exit code 130 (128 + SIGINT signal number 2)

**Alternatives Considered**:

- Catch `UserInterrupt` exception: Less control, harder to emit in-flight data
- `async` package with cancellation: More complex, adds dependency
- No special handling: Data loss on interrupt

**Exit Code Convention**:

- 0: Success
- 1: General errors (config, file not found, invalid regex)
- 130: Terminated by SIGINT

______________________________________________________________________

### 3. Retry with Exponential Backoff for File Access

**Context**: FR-010 requires retry with backoff on transient errors (up to 3 attempts).

**Decision**: Simple DIY pattern without new dependencies for v0.1.

**Rationale**:

- Minimal dependencies (constitution prefers simplicity)
- Well understood, easy to test
- Sufficient for v0.1 scope
- Can upgrade to `retry` package later if needed

**Core Pattern**:

```haskell
retryWithBackoff :: Int -> Int -> IO a -> IO a
retryWithBackoff maxAttempts initialDelayMicros action = go 1 initialDelayMicros
  where
    go attempt delay
      | attempt > maxAttempts = action  -- Final attempt, let exception propagate
      | otherwise = action `catch` \(e :: IOException) ->
          if isRetriable e && attempt < maxAttempts
            then do
              threadDelay delay
              go (attempt + 1) (delay * 2)  -- Exponential backoff
            else throwIO e

isRetriable :: IOException -> Bool
isRetriable e = isPermissionError e || isDoesNotExistError e || isResourceVanishedError e
```

**Configuration**:

- Max attempts: 3 (as specified in FR-010)
- Initial delay: 100ms
- Backoff: 2x multiplier (100ms → 200ms → 400ms)
- Max total wait: ~700ms before final attempt

**Alternatives Considered**:

- `retry` package: Good API but adds dependency
- `stamina` package: More features than needed
- No retry: Too fragile for log rotation scenarios

______________________________________________________________________

## Summary

| Topic | Decision | Dependency Impact |
|-------|----------|-------------------|
| Stream combinator | Direct constructor pattern match | None (uses existing `streaming`) |
| SIGINT handling | MVar-based cooperative shutdown | `unix` (already in base) |
| Retry backoff | Simple DIY pattern | None |

All research complete. No NEEDS CLARIFICATION items remain. Ready for Phase 1: Design & Contracts.
