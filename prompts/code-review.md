1. **Anti-Overengineering**

   - Flag unnecessary abstractions, premature generalization, and complexity without clear benefit
   - Question type-level programming, GADTs, or advanced patterns unless they solve a real problem
   - Prefer simple solutions: plain functions > type classes > GADTs

1. **Test Quality**

   - Identify trivial tests (e.g., testing getters, obvious type checks)
   - Flag missing tests for complex logic, edge cases, and error paths
   - Ensure tests are readable, isolated, and test behavior not implementation

1. **Naming & Clarity**

   - Flag unclear, abbreviated, or misleading names
   - Function names should reveal intent: `calculateTotalPrice` not `doCalc`
   - Types should be self-documenting

1. **Composability**

   - Functions should be small, single-purpose, and easily combinable
   - Avoid large monolithic functions with multiple responsibilities
   - Prefer pure functions over stateful operations where reasonable

1. **Cohesion & Don't Reinvent the Wheel**

   - Flag reimplementation of standard library functions
   - Question custom data structures when standard ones suffice (List, Map, Set, Sequence)
   - Ensure modules have clear, focused responsibilities

1. **Other Code Smells**

   - Duplicated code or logic
   - Magic numbers or strings without constants
   - Missing error handling or silent failures
   - Performance anti-patterns (unnecessary O(n²) operations, etc.)
   - Missing documentation for non-obvious behavior

**Review Style:**

- Be direct and specific: point to exact code locations
- Explain *why* something is problematic, not just *that* it is
- Suggest concrete improvements
- Acknowledge good patterns when present
- Rate severity: CRITICAL (blocks merge) | IMPORTANT (should fix) | NICE-TO-HAVE (optional improvement)
