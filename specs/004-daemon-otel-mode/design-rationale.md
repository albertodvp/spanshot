# Design Rationale: Daemon & OTEL Mode

**Feature Branch**: `004-daemon-otel-mode`\
**Created**: 2026-03-07\
**Status**: Draft\
**Context**: Brainstorming session on v0.3 architecture

## Vision

SpanShot evolves from a developer tool to an **OTEL-native error capture system** that works identically in development and production.

### The Core Insight

There are two distinct usage patterns:

| Context | App Type | Best Input Method |
|---------|----------|-------------------|
| **Development** | Non-OTEL apps | `wrap`/`session` (terminal capture) |
| **Development + Production** | OTEL-instrumented apps | OTEL receiver (OTLP) |

For OTEL-instrumented apps, **the local and production experience should be identical**. This eliminates context switching and ensures errors captured locally behave the same as in production.

### The "Span" Double Meaning

The name "SpanShot" gains deeper meaning with OTEL:

- **Current**: "span" = time window around error (temporal context)
- **With OTEL**: "span" = distributed tracing span (structural context)

Both meanings are valid and complementary.

## Architecture

### Two Input Modes

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            SPANSHOT MODES                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌─────────────────────┐          ┌─────────────────────────────────────┐ │
│   │   wrap/session      │          │   OTEL Mode                         │ │
│   │   (non-OTEL apps)   │          │   (OTEL-instrumented apps)          │ │
│   │                     │          │                                     │ │
│   │   Terminal capture  │          │   Receives OTLP                     │ │
│   │   PTY-based         │          │   Captures errors from spans/logs   │ │
│   └──────────┬──────────┘          │   Optionally forwards upstream      │ │
│              │                     └──────────────────┬──────────────────┘ │
│              │                                        │                    │
│              └────────────────┬───────────────────────┘                    │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Capture   │                                     │
│                        │   Engine    │                                     │
│                        └──────┬──────┘                                     │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Analyze   │  (v0.4 - AI)                        │
│                        └──────┬──────┘                                     │
│                               │                                            │
│                               ▼                                            │
│                        ┌─────────────┐                                     │
│                        │   Deliver   │  (v0.5)                             │
│                        └─────────────┘                                     │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### OTEL Mode: Config-Driven Forwarding

The distinction between "local OTEL" and "production OTEL proxy" is purely configuration:

```yaml
# Local dev - receive only
otel:
  listen: ":4318"
  upstream: null

# Production - receive + forward
otel:
  listen: ":4318"
  upstream:
    endpoint: "https://otlp.grafana.net:4317"
    headers:
      Authorization: "Basic ${GRAFANA_OTLP_TOKEN}"
```

**Same code path. Same behavior.** Just:

- `upstream: null` → receive + capture + analyze
- `upstream: {...}` → receive + capture + analyze + forward

### Production Proxy Value Proposition

```
┌─────────┐         ┌───────────┐         ┌─────────────┐
│  App    │  OTLP   │ SpanShot  │  OTLP   │ Your        │
│ (OTEL)  │ ──────► │  Proxy    │ ──────► │ Observability│
└─────────┘         └─────┬─────┘         │ (Grafana,   │
                          │               │  Datadog)   │
                          │ captures +    └─────────────┘
                          │ analyzes
                          ▼
                    ┌───────────┐
                    │ AI Agent  │  ◄── SpanShot adds this layer
                    │ Insights  │
                    └───────────┘
```

**Key benefits:**

- **Zero disruption**: Existing observability keeps working unchanged
- **Additive value**: AI analysis layer on top of existing stack
- **Easy adoption**: Change one env var (`OTEL_EXPORTER_OTLP_ENDPOINT`)
- **Same local/prod**: OTEL apps work identically everywhere

## Decisions

### Protocol & Encoding

| Decision | Choice | Rationale |
|----------|--------|-----------|
| **OTLP variant** | OTLP/HTTP | Simpler to implement in Haskell than gRPC |
| **Encoding** | JSON first | Debug-friendly; Protobuf can be added later |
| **Signal types** | Traces + Logs + Metrics | Full OTEL support from the start |

### Authentication

| Direction | v0.3 Scope | Rationale |
|-----------|------------|-----------|
| **Incoming** (apps → SpanShot) | None | Rely on network isolation; add later if needed |
| **Outgoing** (SpanShot → upstream) | Header-based + env var interpolation | Covers ~95% of providers (Grafana, Datadog, etc.) |

Example outgoing auth config:

```yaml
upstream:
  endpoint: "${OTEL_UPSTREAM_ENDPOINT}"
  headers:
    Authorization: "${OTEL_UPSTREAM_AUTH}"
```

### Scope

| Feature | v0.3 Scope | Deferred |
|---------|------------|----------|
| Daemon infrastructure | ✅ Yes | - |
| OTEL receiver (OTLP/HTTP) | ✅ Yes | - |
| Traces support | ✅ Yes | - |
| Logs support | ✅ Yes | - |
| Metrics support | ✅ Yes | - |
| Error capture from OTEL signals | ✅ Yes | - |
| Upstream forwarding | ❌ Deferred | v0.3.x or later |
| Incoming auth | ❌ Deferred | Production hardening |
| OTLP/gRPC | ❌ Deferred | If needed |
| Protobuf encoding | ❌ Deferred | If needed |

## Open Questions (TBD)

### Error Detection in OTEL Signals

**This requires deeper analysis.** Unlike regex patterns on log lines, OTEL provides structured data with multiple error indicators:

**Potential error signals in traces:**

- Span status code = ERROR
- Exception events attached to spans
- HTTP status code >= 500 (in span attributes)
- gRPC status code != OK
- Custom error attributes

**Potential error signals in logs:**

- Severity level = ERROR, FATAL, CRITICAL
- Exception records
- Custom patterns in log body

**Potential error signals in metrics:**

- Error rate thresholds (requires stateful analysis)
- Anomaly detection (more complex)

**Questions to resolve:**

1. Should error detection be rule-based (like current regex) or semantic (understand OTEL conventions)?
1. How to handle span hierarchies? (Error in child span → capture parent context?)
1. Should we support custom detection rules per signal type?
1. How to deduplicate errors across signals? (Same error appears in trace + log)

**Proposed approach**: Start with OTEL-semantic defaults (span status ERROR, log severity ERROR+) and allow custom rules as extension.

## Usage Examples

### Non-OTEL App (wrap/session)

```bash
# Same as v0.2
spanshot wrap -- npm test
spanshot session
```

### OTEL App Locally

```bash
# Start SpanShot daemon
spanshot start --otel :4318

# Run OTEL-instrumented app pointing to SpanShot
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 npm test

# View captures
spanshot status
spanshot show 1
```

### OTEL App in Production (future, with forwarding)

```yaml
# docker-compose.yml
services:
  spanshot:
    image: spanshot
    ports:
      - "4318:4318"
    environment:
      - OTEL_UPSTREAM_ENDPOINT=https://otlp.grafana.net:4317
      - OTEL_UPSTREAM_AUTH=Basic xxx
      - ANTHROPIC_API_KEY=...

  myapp:
    environment:
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://spanshot:4318
```

## OTEL Data Richness

With OTEL traces, SpanShot receives structured context far richer than log lines:

```
Trace: req-abc123
│
├─ Span: HTTP POST /api/orders (500ms)
│  ├─ attributes: {user_id: 42, cart_size: 3}
│  │
│  ├─ Span: validate_cart (10ms) ✓
│  │
│  ├─ Span: reserve_inventory (200ms) ✓
│  │
│  ├─ Span: charge_payment (250ms) ✗ ERROR
│  │  ├─ attributes: {payment_provider: "stripe", amount: 99.99}
│  │  └─ exception: "PaymentDeclined: insufficient_funds"
│  │
│  └─ Span: send_confirmation (skipped)
```

**What SpanShot can capture:**

- The error span + its attributes
- Parent context (what operation was this part of?)
- Sibling spans (what succeeded before failure?)
- Trace attributes (user_id, request_id for correlation)
- Full exception details with stack traces

This is **10x richer** than parsing `ERROR: PaymentDeclined` from a log line.

## Phasing

| Phase | Scope |
|-------|-------|
| **v0.3** | Daemon + OTEL receiver + capture (traces, logs, metrics) |
| **v0.3.x** | Upstream forwarding with auth |
| **v0.4** | Analyze (AI agent on captured errors) |
| **v0.5** | Deliver (notifications, integrations) |

## Competitive Positioning

SpanShot with OTEL mode positions itself uniquely:

| Tool | What it does | SpanShot difference |
|------|--------------|---------------------|
| Datadog | Collect + visualize + alert | SpanShot adds AI diagnosis |
| Sentry | Error tracking + grouping | SpanShot adds AI fix suggestions |
| OTEL Collector | Collect + process + export | SpanShot adds AI analysis layer |

**SpanShot = OTEL Collector + AI Error Analyst**

## Implementation Notes

### Haskell OTEL Ecosystem

- `hs-opentelemetry`: Official SDK for *emitting* OTEL data from Haskell apps
- For *receiving* OTLP: May need to implement receiver (parse OTLP/HTTP JSON)
- Consider: protobuf codegen from OTEL proto definitions (for future Protobuf support)

### Daemon Infrastructure

Required for v0.3:

- Process daemonization (background, PID file)
- IPC mechanism (`spanshot status` → daemon communication)
- Signal handling (graceful shutdown)
- HTTP server for OTLP endpoint

## References

- [OTLP Specification](https://opentelemetry.io/docs/specs/otlp/)
- [OTLP/HTTP JSON Encoding](https://opentelemetry.io/docs/specs/otlp/#json-encoding)
- [OTEL Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/)
