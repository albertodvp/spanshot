<div align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/assets/spanshot-mark-inverse.svg">
    <img src="spanshot-logo.svg" alt="spanshot" width="72" height="72">
  </picture>
  <h1>spanshot</h1>
  <p><strong>Investigate failures from the telemetry your system already emits.</strong></p>
  <p>OpenTelemetry signals. Focused investigations. Evidence you can inspect.</p>
  <p>
    <a href="https://spanshot.com">Website & early access</a> ·
    <a href="#from-signals-to-investigation">How it works</a> ·
    <a href="docs/legacy-cli.md">Legacy&nbsp;CLI</a>
  </p>
</div>

---

**Development status:** the current version is closed source while development
continues. An open-source release is planned, but no release date has been
announced. This repository contains the **earlier public CLI**, not the revamp.

## From signals to investigation

Failures leave traces, metrics, and logs. Understanding what happened still
means finding the relevant evidence and putting it together.

spanshot is being rebuilt around a focused investigation: the runtime context
around a failure, prepared for an engineer or AI agent to reason about.

### Capture the context. Keep the evidence.

The revamp is built around three principles:

- **Start from the failure.** Gather the surrounding runtime context instead of
  starting an investigation from scratch.
- **Prepare it for reasoning.** Bring the relevant signals together for an
  engineer or AI agent to inspect.
- **Keep the source visible.** An AI conclusion should not replace the telemetry
  it is based on.

For the current product overview and access requests, visit **[spanshot.com](https://spanshot.com)**.

## Looking for the public CLI?

The earlier implementation runs a command, detects errors in its output, and
stores the surrounding log context locally as JSON. Its source, history, and
build configuration remain available here.

**[Build and use the legacy CLI →](docs/legacy-cli.md)**

Those instructions apply only to the code in this repository. Building the CLI
does not provide access to the current version.

## License

The existing public code remains under the [Apache License 2.0](LICENSE).
The unpublished revamp is separate; licensing details for its planned
open-source release will be published with that release.
