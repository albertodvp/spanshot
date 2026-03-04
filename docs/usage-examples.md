# SpanShot Usage Examples

Real-world examples showing how to use SpanShot with common application types.

## Quick Start

### Capture errors from a log file

```bash
# Basic capture with explicit settings
spanshot capture \
    --logfile /path/to/app.log \
    --regex-pattern "ERROR|FATAL" \
    --pre-window 5 \
    --post-window 5
```

### Monitor logs continuously

```bash
# Use configuration file for detection rules
spanshot run --logfile /var/log/app.log

# With verbose progress output
spanshot run --logfile /var/log/app.log --verbose
```

Press `Ctrl+C` to stop monitoring. Any in-progress captures will be emitted before exit.

______________________________________________________________________

## Example 1: Web Server Monitoring

Monitor a web application server that logs to a file.

### Setup

1. Ensure your web server writes logs to a file:

```bash
# Example: Running a Python Flask app with file logging
python app.py 2>&1 | tee -a /var/log/myapp/app.log
```

2. Configure SpanShot detection rules (optional `.spanshot.yaml`):

```yaml
capture:
  pre_window_duration: 5
  post_window_duration: 5
  detection_rules:
    - regex_pattern: "ERROR"
    - regex_pattern: "Exception"
    - regex_pattern: "500 Internal Server Error"
```

3. Run SpanShot to monitor the logs:

```bash
spanshot run --logfile /var/log/myapp/app.log
```

### Expected Output

When an error occurs, SpanShot outputs JSONL:

```json
{"error_event":{"line":"ERROR: Database connection timeout","source":"/var/log/myapp/app.log",...},"pre_window":[...],"post_window":[...],...}
```

### Process captures with jq

```bash
# Extract just the error lines
spanshot run --logfile /var/log/myapp/app.log | jq -r '.error_event.line'

# Count errors by pattern
spanshot run --logfile /var/log/myapp/app.log | jq '.detected_by[0].regex_pattern' | sort | uniq -c
```

______________________________________________________________________

## Example 2: Background Process

Monitor a long-running background job that writes periodic logs.

### Setup

1. Start your background process with logging:

```bash
# Example: A batch processing script
./process_queue.sh >> /var/log/batch/processor.log 2>&1 &
```

2. Capture historical errors (one-shot):

```bash
spanshot capture \
    --logfile /var/log/batch/processor.log \
    --regex-pattern "ERROR|FAILED" \
    --pre-window 10 \
    --post-window 10
```

3. Or monitor continuously:

```bash
spanshot run --logfile /var/log/batch/processor.log --verbose
```

### Saving Captures to a File

```bash
# Save SpanShots for later analysis
spanshot run --logfile /var/log/batch/processor.log > captures.jsonl &

# Review captured errors
cat captures.jsonl | jq '.error_event.line'
```

______________________________________________________________________

## Example 3: Docker Container

Monitor logs from a Docker container.

### Option A: Log to a file inside the container

1. Configure your application to log to a file, then mount a volume:

```bash
docker run -d \
    -v /var/log/myapp:/app/logs \
    myapp:latest

spanshot run --logfile /var/log/myapp/app.log
```

### Option B: Redirect docker logs to a file

1. Capture container logs to a file:

```bash
docker logs -f mycontainer >> /var/log/docker/mycontainer.log 2>&1 &
```

2. Monitor with SpanShot:

```bash
spanshot run --logfile /var/log/docker/mycontainer.log
```

### Option C: Using docker compose

```yaml
# docker-compose.yml
services:
  app:
    image: myapp:latest
    logging:
      driver: "json-file"
      options:
        max-size: "100m"
        max-file: "3"
```

Then monitor the Docker JSON log file:

```bash
# Find the log file location
docker inspect --format='{{.LogPath}}' mycontainer

# Monitor it with SpanShot (note: Docker JSON logs have a different format)
# You may need to preprocess or use a different regex pattern
```

______________________________________________________________________

## Common Patterns

### Send alerts on errors

```bash
spanshot run --logfile app.log | while read capture; do
    error_line=$(echo "$capture" | jq -r '.error_event.line')
    curl -X POST -d "text=Error detected: $error_line" \
        https://hooks.slack.com/services/YOUR/WEBHOOK/URL
done
```

### Filter specific error types

```bash
# Only show database errors
spanshot capture --logfile app.log --regex-pattern "Database.*ERROR" \
    --pre-window 5 --post-window 5

# Show all errors but filter output
spanshot run --logfile app.log | jq 'select(.error_event.line | contains("timeout"))'
```

### Multiple log files

```bash
# Monitor multiple files with separate SpanShot instances
spanshot run --logfile /var/log/app1.log &
spanshot run --logfile /var/log/app2.log &

# Or combine logs first
tail -f /var/log/app1.log /var/log/app2.log | tee combined.log &
spanshot run --logfile combined.log
```

______________________________________________________________________

## Troubleshooting

### No output is produced

1. **Check if the log file exists and has content:**

   ```bash
   ls -la /path/to/your.log
   head /path/to/your.log
   ```

1. **Verify your regex pattern matches:**

   ```bash
   grep -E "ERROR" /path/to/your.log
   ```

1. **Use verbose mode to see progress:**

   ```bash
   spanshot run --logfile /path/to/your.log --verbose
   ```

### "File not found" error

- Ensure the file path is correct and the file exists
- Check file permissions: SpanShot needs read access

### Invalid regex pattern error

- Test your regex with `grep -E "pattern" yourfile.log`
- Escape special characters properly
- Common patterns:
  - `ERROR` - matches lines containing "ERROR"
  - `ERROR|FATAL` - matches ERROR or FATAL
  - `\[ERROR\]` - matches "[ERROR]" literally

### High memory usage

- SpanShot streams files and keeps only window-sized buffers in memory
- For very high-throughput logs (thousands of lines/second), consider:
  - Reducing window durations
  - Using more specific regex patterns

### Exit codes

- `0` - Success
- `1` - General error (invalid config, file not found, invalid regex)
- `130` - Terminated by SIGINT (Ctrl+C)
