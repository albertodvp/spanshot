# SpanShot — MVP v0.0 (Collect-Only)

## 0) Big Picture

### The Problem

Developers waste minutes per error hunting through scattered logs, copy-pasting context, and stitching together what happened before/after a failure. The friction compounds across teams and time.

### The Solution (overall vision)

**SpanShot** is a local, privacy-first developer tool that:

1. **Collects** logs from sources into a normalized event stream,
2. *(later)* **Captures** temporal windows (“spans”) around detected errors,
3. *(later)* **Analyzes** spans with AI for root causes,
4. *(later)* **Delivers** actionable fixes without leaving the terminal.

This README/spec defines **MVP v0.0** focused **only on COLLECT** so we can test ingestion and streaming behavior in isolation.

---

## USAGE

### Installation

#### Prerequisites

- **Nix with flakes enabled** (recommended), or
- **GHC 9.12.2+** and **Cabal 3.10+**

#### Building from Source

**Using Nix (recommended):**
```bash
# Enter development environment
nix develop

# Build the project
cabal build

# Run tests
cabal test

# Run the executable
cabal run hs-spanshot -- --logfile /path/to/your/app.log
```

**Using Cabal directly:**
```bash
cd hs-spanshot
cabal build
cabal run hs-spanshot -- --logfile /path/to/your/app.log
```

#### Installing the Binary

```bash
# Build and install to ~/.cabal/bin/
cabal install

# Or copy the built binary
cp $(cabal list-bin hs-spanshot) ~/.local/bin/spanshot
```

### Quick Start

```bash
# Tail a log file and stream normalized events as JSONL
spanshot --logfile /path/to/your/application.log

# Or using cabal run during development
cabal run hs-spanshot -- --logfile /path/to/your/app.log
```

### Example Output

```json
{"source":"./app.log","session_order_id":0,"read_at_utc":"2025-10-15T16:32:05.123456Z","line":"[INFO] Application started"}
{"source":"./app.log","session_order_id":1,"read_at_utc":"2025-10-15T16:32:05.234567Z","line":"[ERROR] Connection failed"}
```

### Practical Examples

#### Monitor a Live Application Log

```bash
# Tail your application's log file
spanshot --logfile /var/log/myapp/application.log
```

#### Process and Filter Events with jq

```bash
# Extract only ERROR lines
spanshot --logfile app.log | jq 'select(.line | contains("ERROR"))'

# Count events by pattern
spanshot --logfile app.log | jq -s 'group_by(.line | match("\\[(\\w+)\\]") | .captures[0].string) | map({level: .[0].line, count: length})'

# Get timestamp range
spanshot --logfile app.log | jq -s '[.[0].read_at_utc, .[-1].read_at_utc]'
```

#### Save to File

```bash
# Capture first 100 events
spanshot --logfile app.log | head -n 100 > events.jsonl

# Monitor and save to file (continues until Ctrl-C)
spanshot --logfile app.log > events.jsonl
```

#### Test with Sample Fixtures

```bash
# Read a small test file
cabal run hs-spanshot -- --logfile test/fixtures/small.log

# Test with Python error logs
cabal run hs-spanshot -- --logfile test/fixtures/python_errors.log
```

### Command-Line Options

- `--logfile <path>` — Path to the log file to monitor (required)

**Note**: Configuration options like polling interval are currently set programmatically via `defaultCollectOptions` (150ms). Future versions may expose these as CLI flags.

### Behavior

- **Reads from the beginning** of the file (not just new lines)
- **Continuously monitors** for new lines appended to the file
- **Outputs JSONL** (one JSON event per line) to stdout
- **Stops cleanly** on Ctrl-C (SIGINT)
- **UTF-8 decoding** with lenient error handling (replaces invalid bytes)

### Current Limitations (v0.0)

- **Single file only** (no multiple sources)
- **No log rotation handling** (won't follow renamed files)
- **UTF-8 encoding assumed** (other encodings may produce replacement characters)
- **In-memory state only** (no persistence, `session_order_id` resets on restart)
- **Naive line-based parsing**: Two key limitations (NOTE: look for already working solutions for this problem):
  1. **Multi-line entries**: Stack traces, continuation lines, and wrapped log entries are split into separate events (one event per line). For example, a Python traceback spanning 5 lines becomes 5 separate events instead of one grouped error event.
  2. **Structured/JSON logging**: When parsing structured logs (e.g., JSON-formatted lines), the current schema only captures the raw line text. If a log line contains `{"timestamp":"2025-10-14T10:00:00Z","level":"ERROR","message":"..."}`, we emit both our collector's `read_at_utc` timestamp AND preserve the original line's timestamp in the `line` field as raw text. There's no parsing/extraction of embedded fields yet, which means:
     - Timestamp collision: the event's `read_at_utc` (when we read it) vs. the application's original timestamp (when it logged it)
     - No structured field extraction: `level`, `message`, `user_id`, etc. remain unparsed text
     - Future versions will need an extended schema (e.g., `original_timestamp`, `parsed_fields`, `raw_line`) to avoid data loss and confusion

### Performance Characteristics

- **Polling interval**: 150ms (configurable via library API)
- **Chunk size**: 32KB for efficient I/O
- **Memory usage**: Minimal - streaming architecture processes one line at a time
- **File handle**: Kept open for the lifetime of the process (tail-f behavior)

---

## 1) Scope of MVP v0.0 (Collect Only)

* Tail **one** logfile (UTF-8 assumed).
* Emit **normalized events** to the caller (library) or **JSONL** to stdout (executable).
* Keep state only in process memory; no daemon/background mode required for v0.0.
* **No** detection, spans, AI, or notifications yet.

**Out of scope (for later MVPs):**
* Multiple sources, Docker/journald, rotation handling, debounce, config files.
* Captures/Analyze/Deliver phases.

---

## 2) Repo Split: Library vs Executable

* **Library**: core “collect” functionality (stream lines → normalized events).
* **Executable**: thin CLI for debugging and manual use: `spanshot collect --logfile <path>` prints JSONL events.

> Language-agnostic; if using Haskell, think `spanshot-lib` (modules like `Collect`, `Types`) and `spanshot` (CLI).

---

## 3) Data Contract

### 3.1 Event (JSON schema)

```json
{
  "source": "/absolute/or/relative/path/to/log",
  "session_order_id": 1,
  "read_at_utc": "2025-10-14T16:32:05.123Z",
  "line": "raw line text without trailing newline"
}
```

**Semantics**

* `source`: the path used to open the file (echoed back verbatim).
* `session_order_id`: monotonic counter **within this process run**, starting at 1 per source.
* `read_at_utc`: UTC wall-clock time when the collector emitted the event, RFC3339 with milliseconds and `Z`.
* `line`: the raw line content (no trailing newline; preserve internal spacing).

**Stability notes**

* `session_order_id` resets on each process start; it is not a file line number.
* `read_at_utc` is non-decreasing in practice, but not guaranteed strictly monotonic (wall clock).

**Structured logging considerations (v0.0 limitation)**

For v0.0, the `line` field contains the raw, unparsed text. If the source log uses structured formats (JSON, logfmt, etc.), the entire serialized string is stored in `line`, including any timestamps, levels, or metadata already present in that format.

This creates potential issues:
* **Timestamp ambiguity**: A JSON log like `{"timestamp":"2025-10-14T10:15:30Z",...}` will have BOTH the embedded `timestamp` (from the application) AND our `read_at_utc` (when SpanShot read it). These may differ significantly for old logs or during catch-up reading.
* **No field extraction**: Structured fields (severity, trace IDs, user IDs, etc.) are not extracted or indexed; they remain as text in `line`.
* **Future schema evolution needed**: Later versions should consider:
  - `original_timestamp` or `log_timestamp` field (extracted from the log line if parseable)
  - `parsed_fields` or `metadata` object for extracted structured data
  - `raw_line` to preserve the original unmodified text
  - Schema versioning to handle migration

For now, downstream consumers must parse `line` themselves if they need structured field access.

---

## 4) Library Spec

### 4.1 Public API (conceptual)

I want 2 interfaces: one takes the the filepath and produces the stream, the other works directly on the stream of text and produces a stream of events,
then we need to collap (use streaming library), we will need to sink too

* `collect(logfile_path, on_event, options)`

  * **Input**: `logfile_path` (string), `on_event` (callback or iterator yield), `options` (optional; see below).
  * **Behavior**:
    * Open the file; error if missing.
    * Read from start to EOF, emitting one **Event** per line.
    * When at EOF, periodically check for new content (simple polling, e.g., 100–300ms).
    * On Ctrl-C/requested cancellation, stop cleanly.
  * **Output**: emits **Event** objects through `on_event` (or yields them if using an iterator/stream).
* **Options (optional for v0.0)**

  * `poll_interval_ms` (default: 150) — sleep interval when waiting for new lines.

### 4.2 Error Handling

* If the file cannot be opened → raise/return a clear error (`ENOENT: logfile not found`), non-zero exit for CLI.
* If a line has invalid UTF-8 → **replace invalid bytes** with a replacement character (or document “skip line”).
* Do not crash on empty lines or extremely long lines; emit them as is.

---

## 5) Executable (CLI) Spec

### 5.1 Command

```
spanshot collect --logfile <path>
```

### 5.2 Behavior

* Opens `<path>`, streams lines, and prints **one JSON object per line** (JSONL) to **stdout** using the **Event** schema.
* Exits non-zero with an error message if `<path>` does not exist or cannot be opened.
* Terminates cleanly on Ctrl-C.

### 5.3 Output example (JSONL)

```
{"source":"./tests/fixtures/small.log","session_order_id":1,"read_at_utc":"2025-10-14T16:32:05.123Z","line":"first line"}
{"source":"./tests/fixtures/small.log","session_order_id":2,"read_at_utc":"2025-10-14T16:32:05.234Z","line":"second line"}
```

---

## 6) Behavior Details

* Start reading at **beginning of file** (not just new lines).
* Continue reading appended lines (basic tail).
* **Ignore rotation/rename** for v0.0 (documented limitation).
* `session_order_id` starts at `1` and increments for each emitted line.
* `read_at_utc` captured **at emit time**, UTC, RFC3339 w/ millis and trailing `Z`.
* **Line-based parsing only**: Each newline-delimited line becomes exactly one event, with two important implications:
  1. **Multi-line splitting**: Stack traces (Python, Java, Node.js), wrapped messages, and continuation patterns are split across multiple events. Example: a 10-line Java exception becomes 10 separate events with sequential `session_order_id` values.
  2. **No structured parsing**: JSON logs, logfmt, or other structured formats are kept as raw text in the `line` field. This means timestamps, log levels, and metadata embedded in the log line are NOT extracted into separate fields—they coexist with SpanShot's own `read_at_utc` and metadata, potentially causing confusion about which timestamp or metadata is authoritative.
  
  These limitations are intentional for v0.0 to keep ingestion simple and fast. Future versions will add multi-line grouping heuristics and optional structured format parsers.

---

## 7) Project Structure (suggested)

```
spanshot/
  lib/ or src/
    Types.*       # Event definition + JSON encode/decode
    Collect.*     # collect(logfile, on_event, options)
  app/ or bin/
    spanshot.*    # CLI: spanshot collect --logfile <path>
  tests/
    unit/
      test_collect_read_all.*
      test_collect_stream_append.*
      test_collect_missing_file.*
      test_collect_invalid_utf8.*   # optional
    fixtures/
      small.log
      streaming_part1.log
      streaming_part2.log
      clean.log
  README.md       # this spec
```

---

## 8) Tests (as part of spec)

### 8.1 Unit Tests (Library)

1. **read_all_lines**

* **Given**: `fixtures/small.log` with 5 lines.
* **When**: `collect()` is run to completion.
* **Then**:

  * Emits 5 events.
  * `session_order_id` is `[1,2,3,4,5]`.
  * Each event has a valid `read_at_utc` (RFC3339 with `Z`) and correct `source`.
  * `line` contents match file lines (without trailing newline).

2. **stream_append**

* **Given**: a temp file created empty; start `collect()` in a background thread/process capturing events; append two lines; wait; append three more lines.
* **Then**:

  * Emits 5 events in order (`session_order_id` 1..5).
  * `read_at_utc` timestamp is present for each and **non-decreasing** across events.

3. **missing_file_fails**

* **Given**: a non-existent path.
* **When**: `collect()` is invoked (or CLI run).
* **Then**: A clear error is returned/raised; CLI exits non-zero with an error message.

4. **invalid_utf8_is_handled** *(optional)*

* **Given**: a file containing a line with invalid UTF-8 sequence.
* **Then**: the collector emits an event with that line where invalid bytes are replaced (or the line is skipped if that’s the chosen policy), and the process does not crash.

### 8.2 Integration Tests (CLI)

1. **cli_read_all_lines**

* Run: `spanshot collect --logfile tests/fixtures/small.log`.
* **Then**: stdout has 5 JSONL lines; each parses as valid JSON matching the Event schema; IDs `1..5`.

2. **cli_stream_append**

* Start the CLI against a temp logfile; append lines while running.
* **Then**: stdout accumulates valid JSONL events with sequential `session_order_id` and non-decreasing `read_at_utc`.

3. **cli_missing_file_fails**

* Run CLI on a missing path.
* **Then**: non-zero exit; error message on stderr.

---

## 9) Acceptance Criteria (v0.0)
* Library function `collect(...)` emits correct **Event** objects for static and appended content.
* CLI `spanshot collect --logfile <path>` prints valid **JSONL** events with sequential `session_order_id` and `read_at_utc`.
* Tests (unit + integration) pass reliably.
* Clean termination on Ctrl-C.
* Documented limitations: single file, no rotation handling, UTF-8 assumption.

---

## 10) Contributing

### Commit Message Convention

This project follows the [Angular Commit Message Convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#commit) for commit messages:

**Format:**
```
<type>(<scope>): <subject>

<body>

<footer>
```

**Type** must be one of:
- `feat`: A new feature
- `fix`: A bug fix
- `docs`: Documentation only changes
- `style`: Changes that do not affect the meaning of the code (white-space, formatting, etc)
- `refactor`: A code change that neither fixes a bug nor adds a feature
- `perf`: A code change that improves performance
- `test`: Adding missing tests or correcting existing tests
- `build`: Changes that affect the build system or external dependencies
- `ci`: Changes to CI configuration files and scripts
- `chore`: Other changes that don't modify src or test files
