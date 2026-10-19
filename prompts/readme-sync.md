# README Sync Instructions

Compare docs/legacy-cli.md against the public CLI codebase and update any outdated sections there.

README.md is the product overview and explicitly distinguishes the private revamp
from this earlier public implementation. Preserve that distinction, the development
status, and the branding. Do not infer the revamp's capabilities or release timing
from this codebase, or replace the overview with legacy CLI documentation.

## Files to Read

- `README.md` - product overview and development status (preserve)
- `docs/legacy-cli.md` - public CLI documentation to synchronize
- `hs-spanshot/app/Main.hs` - CLI commands and dispatch types
- `hs-spanshot/src/Types.hs` - data types (CollectEvent, SpanShot, CaptureOptions)
- `hs-spanshot/src/Config.hs` - configuration types
- `hs-spanshot/hs-spanshot.cabal` - exposed modules, dependencies

## What to Check

1. **CLI commands**: Ensure documented commands match the `Dispatch` type in Main.hs

   - Remove "Coming Soon" for implemented commands
   - Add any new commands not yet documented

1. **Output format**: Ensure JSON examples match actual types

   - `CollectEvent` fields and JSON keys
   - `SpanShot` structure (if implemented)

1. **Roadmap checkboxes**:

   - Mark completed items with `[x]`
   - Update "in progress" items based on actual code state

1. **Limitations section**:

   - Remove limitations that have been addressed
   - Add new known limitations if discovered

1. **Configuration section**:

   - Verify example config matches actual schema
   - Ensure documented config commands exist

## Rules

- Keep existing structure, tone, and formatting style
- Don't add emojis
- Only update factually incorrect or outdated information
- Open a PR with changes (don't commit directly to main)
