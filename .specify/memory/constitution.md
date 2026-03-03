<!--
SYNC IMPACT REPORT
==================
Version change: N/A → 1.0.0 (INITIAL)
Amendment date: 2026-03-03

Modified sections:
- Initial creation of all principles
- Principle III strengthened to NON-NEGOTIABLE with explicit TDD cycle

Added:
- Principle I: CLI-First Architecture
- Principle II: Pure Functional Core (NON-NEGOTIABLE)
- Principle III: Test-First Development (NON-NEGOTIABLE)
- Principle IV: Nix-Centered Development
- Principle V: Type Safety
- Principle VI: Clean Code Design
- Technical Standards section
- Development Workflow section
- Governance section

Removed:
- None (initial creation)

Templates requiring updates:
- plan-template.md: ✅ No changes needed (constitution check section is generic)
- spec-template.md: ✅ No changes needed (does not reference constitution specifics)
- tasks-template.md: ✅ No changes needed (does not reference constitution specifics)

Follow-up TODOs:
- None
-->

# SpanShot Constitution

## Core Principles

### I. CLI-First Architecture

All user-facing features MUST be accessible via command-line interface. The CLI is the primary
interface; other interfaces (API, GUI) are secondary. Every feature MUST support:

- Text in/out protocol: args/stdin for input, stdout for output, stderr for errors
- Structured output in JSONL format for machine consumption
- Human-readable output mode when appropriate
- Composability with Unix tools (pipes, jq, grep)

**Rationale**: SpanShot is a developer tool. CLI-first ensures scriptability, integration with
existing workflows, and testability. JSONL output enables streaming processing and composability
with the Unix tool ecosystem.

### II. Pure Functional Core (NON-NEGOTIABLE)

Code MUST follow functional programming principles with a strict separation of concerns:

**Pure Functions First**:

- Business logic MUST be implemented as pure functions (same input → same output, no side effects)
- Pure functions MUST NOT perform I/O, access current time, or modify state
- Prefer returning new data structures over mutation
- Use types to make illegal states unrepresentable

**Functional Core, Imperative Shell**:

- Core domain logic MUST be pure and side-effect-free (Types.hs, Capture.hs core)
- Side effects (I/O, time access) MUST be pushed to the outer edges (main, CLI handlers)
- Impure code (the "shell") MUST call pure code (the "core"), never the reverse
- Use streaming abstractions (Streaming library) for efficient I/O with pure transformations

**Streaming by Default**:

- File processing MUST use streaming to handle arbitrarily large inputs
- Memory usage MUST NOT grow unbounded with input size (document exceptions explicitly)
- Prefer lazy evaluation with explicit strictness annotations where needed

**Rationale**: Pure functions are easier to test, reason about, and compose. Streaming ensures
SpanShot can process large log files without memory exhaustion. Pushing side effects to edges
makes the codebase more predictable and testable.

### III. Test-First Development (NON-NEGOTIABLE)

TDD is mandatory for all code changes:

1. Tests MUST be written before implementation
1. Tests MUST fail before implementation begins (Red phase)
1. Implementation MUST make tests pass (Green phase)
1. Code MUST be refactored while maintaining passing tests (Refactor phase)

**Unit Tests**:

- Core logic MUST have comprehensive unit tests (hspec)
- Tests MUST cover happy paths, edge cases, and error conditions
- Property-based tests (QuickCheck) MUST be used for serialization roundtrips and invariants
- New functions MUST NOT be merged without corresponding tests

**Integration Tests**:

- CLI integration tests MUST validate end-to-end behavior
- Golden tests MUST verify output format stability
- Tests MUST run against the compiled binary, not just library code
- All user-facing commands MUST have integration test coverage

**Test Organization**:

- Unit tests in `test/` directory with `*Spec.hs` naming
- Integration tests in separate test suite (`integration-cli`)
- Fixtures in `test/fixtures/`, golden outputs in `test/golden/`

**CI Enforcement**:

- All tests MUST pass before merge (enforced by CI)
- Test coverage MUST NOT decrease without explicit justification
- Flaky tests MUST be fixed immediately or quarantined with tracking issue

**Rationale**: SpanShot processes critical error data. Untested code introduces unacceptable
risk to reliability. TDD ensures code is designed for testability from the start. Integration
tests ensure the CLI behaves correctly from the user's perspective.

### IV. Nix-Centered Development

All project tooling MUST be managed through Nix flakes with flake-parts:

- Development environment MUST be provided via `nix develop` with all dependencies declared
- No tooling MAY require global installation outside of Nix
- Pre-commit hooks MUST be configured through Nix (pre-commit-hooks.nix)
- Environment variables and secrets MUST be managed declaratively (agenix-shell)

**Centralized Formatting with treefmt-nix**:

- All formatting and linting MUST be managed through treefmt-nix
- `nix fmt` MUST run all configured formatters (fourmolu, alejandra, mdformat)
- Pre-commit hooks MUST use treefmt for consistent formatting

**Required flake outputs**:

- `devShells.default`: Development shell with all tools (GHC, cabal, HLS, hlint, fourmolu)
- `formatter`: treefmt configuration for `nix fmt`
- `packages.default`: The built spanshot binary
- `apps.default`: Run spanshot directly

**Rationale**: Declarative, reproducible environments eliminate "works on my machine" issues
and ensure all contributors and CI have identical tooling. treefmt-nix centralizes all
formatting rules.

### V. Type Safety

Type safety MUST be enforced at multiple layers:

**Static Types**:

- All code MUST compile with `-Wall -Werror` (all warnings are errors)
- Smart constructors MUST be used for validated types (mkCollectOptions, mkCaptureOptions)
- Use newtypes to distinguish semantically different values
- Avoid partial functions; use Maybe/Either for fallible operations

**Runtime Validation**:

- All external input (CLI args, config files, log content) MUST be validated at boundaries
- Validation errors MUST include clear, actionable error messages
- Use Aeson for JSON serialization with explicit field mappings

**Rationale**: Strong types catch errors at compile time rather than runtime. Smart
constructors ensure invalid states cannot be constructed. Clear error messages help users
fix problems quickly.

### VI. Clean Code Design

Code MUST follow established software design principles:

**No Over-Engineering**:

- Solve the problem at hand, not hypothetical future problems
- YAGNI (You Aren't Gonna Need It): Do NOT add functionality until necessary
- Prefer simple, direct solutions over clever abstractions
- Three similar lines of code are better than a premature abstraction

**Single Responsibility**:

- Each module MUST have exactly one reason to change
- Functions MUST do one thing and do it well
- Keep modules small and focused (Collect.hs, Capture.hs, Types.hs, Config.hs)

**Documentation**:

- All public APIs MUST have Haddock documentation
- Complex algorithms MUST include inline comments explaining the approach
- Time/space complexity MUST be documented for performance-critical code

**Rationale**: Over-engineered code is harder to understand, test, and maintain. Clean design
reduces cognitive load and prevents technical debt accumulation.

## Technical Standards

**Language/Runtime**: Haskell (GHC 9.12+)
**Build System**: Cabal + Nix flakes with flake-parts
**Package Management**: Cabal (dependencies in cabal file), Nix for reproducibility
**Testing**: hspec + QuickCheck for unit tests, separate test suite for CLI integration
**Output Format**: JSONL (newline-delimited JSON) for streaming
**Code Style**: fourmolu formatting (managed via treefmt)
**Task Runner**: just (justfile at repository root)

Code style:

- Haddock documentation required on public APIs
- fourmolu formatting (managed via treefmt)
- hlint suggestions SHOULD be followed
- All imports MUST use qualified imports or explicit import lists
- Language extensions MUST be declared per-file, not globally

## Development Workflow

### Feature Development Process

1. **Specification**: Define requirements and user scenarios in spec.md
1. **Planning**: Design implementation approach in plan.md
1. **Constitution Check**: Verify plan aligns with these principles
1. **Task Generation**: Create actionable task list in tasks.md
1. **Implementation**: Execute tasks with tests written before implementation
1. **Integration**: Run full test suite (`just test`)
1. **Documentation**: Update README and Haddock docs as needed

### Environment Setup

All development MUST occur within the Nix development shell:

```bash
nix develop                    # Enter development environment
nix fmt                        # Format all files via treefmt
just build                     # Build the project
just test                      # Run all tests
just test-unit                 # Run unit tests only
just test-integration-cli      # Run CLI integration tests only
```

### Commit Message Standards

All commits MUST follow the [Conventional Commits](https://www.conventionalcommits.org/)
specification as documented in CONTRIBUTING section of README:

**Format**: `<type>(<scope>): <description>`

**Required types**:

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Formatting (no code change)
- `refactor`: Code restructuring (no behavior change)
- `perf`: Performance improvement
- `test`: Adding or updating tests
- `build`: Build system changes
- `ci`: CI configuration changes
- `chore`: Maintenance tasks

**Rules**:

- Description MUST be lowercase and imperative ("add feature" not "Added feature")
- Scope is optional but encouraged (e.g., `feat(capture): add span window buffering`)
- Commit messages MUST NOT reference AI assistants or tools
- Commit messages MUST NOT include co-author attributions to non-human entities

### Code Review Requirements

- All PRs MUST include passing tests
- All PRs MUST pass formatting checks (`nix fmt`)
- All PRs MUST pass lint checks (hlint)
- All PRs MUST be reviewed for Clean Code Design compliance (Principle VI)
- All PRs MUST be reviewed for Functional Programming Style compliance (Principle II)

### Branch Strategy

- `main`: Protected; CI must pass
- Feature branches: `feature/[feature-name]` or issue-linked branches

## Governance

This constitution supersedes all other development practices. Amendments require:

1. Written proposal documenting the change and rationale
1. Review of impact on existing features and templates
1. Version bump following semantic versioning:
   - MAJOR: Backward-incompatible principle changes or removals
   - MINOR: New principles or significant expansions
   - PATCH: Clarifications and non-semantic refinements
1. Update of all dependent templates and documentation

All code reviews MUST verify constitutional compliance. Violations MUST be flagged and
resolved before merge.

**Version**: 1.0.0 | **Ratified**: 2026-03-03 | **Last Amended**: 2026-03-03
