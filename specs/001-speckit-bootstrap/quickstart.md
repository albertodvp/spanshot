# SpanShot Developer Quickstart

## Environment Setup

```bash
# Enter development environment (all tools included)
nix develop

# Verify setup
ghc --version      # Should show GHC 9.12.x
cabal --version    # Should show 3.x
just --version     # Should show just
```

## Common Commands

| Task | Command | Notes |
|------|---------|-------|
| Build | `just build` | Compiles to `dist-newstyle/` |
| Run all tests | `just test` | Unit + CLI integration |
| Run unit tests only | `just test-unit` | Fast feedback loop |
| Run CLI integration tests | `just test-integration-cli` | End-to-end validation |
| Format all files | `nix fmt` | Uses treefmt (fourmolu, alejandra, mdformat) |
| Get binary path | `just bin-path` | Path to compiled binary |
| Run spanshot | `$(just bin-path) collect --logfile <file>` | Or use `nix run .#default` |

## Speckit Workflow

SpanShot uses speckit for feature development. The workflow is:

```
/speckit.specify → /speckit.clarify → /speckit.plan → /speckit.tasks → /speckit.implement
```

| Command | Purpose | Output |
|---------|---------|--------|
| `/speckit.specify <description>` | Create feature spec | `specs/NNN-name/spec.md` |
| `/speckit.clarify` | Resolve ambiguities | Updated spec |
| `/speckit.plan` | Create implementation plan | `plan.md`, `research.md`, `quickstart.md` |
| `/speckit.tasks` | Generate task list | `tasks.md` |
| `/speckit.implement` | Execute tasks | Code changes |
| `/speckit.analyze` | Cross-artifact validation | Consistency report |

## Key Documents

| Document | Location | Purpose |
|----------|----------|---------|
| Constitution | `.specify/memory/constitution.md` | Core principles (NON-NEGOTIABLE) |
| README | `README.md` | Project overview and usage |
| Justfile | `justfile` | Available just commands |

## Constitution Principles (Summary)

1. **CLI-First Architecture** - All features via CLI, JSONL output
1. **Pure Functional Core** (NON-NEGOTIABLE) - Business logic in pure functions
1. **Test-First Development** (NON-NEGOTIABLE) - TDD mandatory
1. **Nix-Centered Development** - All tools via Nix
1. **Type Safety** - Smart constructors, validated inputs
1. **Clean Code Design** - YAGNI, single responsibility

## Git Workflow

```bash
# Commit format (Conventional Commits)
git commit -m "feat(capture): add span window buffering"
git commit -m "fix(collect): handle empty log files"
git commit -m "docs: update README installation section"

# Branch naming
# feature/name or NNN-name (speckit creates these)
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| `nix develop` fails | Ensure flakes enabled: `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf` |
| Tests fail to find binary | Run `just build` first |
| Formatting fails | Ensure you're in `nix develop` shell |
| Pre-commit hooks not running | Run `nix develop` to install hooks |
