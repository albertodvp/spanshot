# SpanShot Development Guidelines

Auto-generated from feature plans. Last updated: 2026-03-03

## Active Technologies

- **Language**: Haskell (GHC 9.12+)
- **Build**: Cabal + Nix flakes with flake-parts
- **Testing**: hspec, QuickCheck (unit), CLI integration tests
- **Libraries**: streaming, streaming-bytestring, aeson, opt-env-conf

## Project Structure

```text
/
├── hs-spanshot/           # Main Haskell package
│   ├── src/               # Library source
│   │   ├── Types.hs       # Core types and smart constructors
│   │   ├── Collect.hs     # Log collection (streaming)
│   │   ├── Capture.hs     # Error capture logic
│   │   └── Config.hs      # Configuration handling
│   ├── app/               # CLI executable
│   └── test/              # Test suites
├── .specify/              # Speckit configuration
│   ├── memory/            # Constitution and memory
│   └── templates/         # Document templates
├── specs/                 # Feature specifications
├── flake.nix              # Nix flake
└── justfile               # Task runner
```

## Commands

```bash
# Environment
nix develop                    # Enter dev shell

# Build & Test
just build                     # Build project
just test                      # All tests
just test-unit                 # Unit tests only
just test-integration-cli      # CLI integration tests

# Formatting
nix fmt                        # Format all (treefmt)
```

## Code Style

- **Formatting**: fourmolu (via treefmt)
- **Imports**: Qualified imports or explicit import lists
- **Extensions**: Per-file declarations only
- **Documentation**: Haddock on all public APIs
- **Types**: Smart constructors for validated data

## Constitution Principles (NON-NEGOTIABLE)

1. **Pure Functional Core** - Business logic in pure functions, side effects at edges
1. **Test-First Development** - TDD mandatory (Red-Green-Refactor)

See `.specify/memory/constitution.md` for full details.

## Recent Changes

- 001-speckit-bootstrap: Initial speckit setup and constitution

<!-- MANUAL ADDITIONS START -->

<!-- MANUAL ADDITIONS END -->
