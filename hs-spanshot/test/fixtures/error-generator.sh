#!/usr/bin/env bash
# Simple script that generates logs with occasional errors
# Usage: ./error-generator.sh > app.log

set -e

log() {
    echo "$(date -Iseconds) $1"
}

counter=0
while true; do
    counter=$((counter + 1))

    # Normal log messages
    log "INFO Starting iteration $counter"
    sleep 0.5
    log "INFO Processing request $counter"
    sleep 0.3
    log "DEBUG Checking database connection"
    sleep 0.2

    # Every 5th iteration, generate an error
    if [ $((counter % 5)) -eq 0 ]; then
        log "ERROR Database connection timeout after 30s"
        sleep 0.3
        log "WARN Retrying connection..."
        sleep 0.2
        log "INFO Connection restored"
    fi

    # Every 12th iteration, generate a different error
    if [ $((counter % 12)) -eq 0 ]; then
        log "ERROR Failed to process request: invalid input"
        sleep 0.2
        log "INFO Request rejected, continuing"
    fi

    log "INFO Completed iteration $counter"
    sleep 1
done
