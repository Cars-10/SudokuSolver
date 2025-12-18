# Phase 2: Compiled Languages Wave - Context

**Gathered:** 2025-12-18
**Status:** Ready for planning

<vision>
## How This Should Work

Sequential deep-dive into each language — same intensity and rigor as the C baseline. One language at a time, fully polished and validated before moving to the next.

The core validation model remains: iteration counts are the source of truth. Each implementation must produce exactly the same iteration counts as C across all matrices (1-5). This is non-negotiable algorithmic validation, not just "close enough."

Order: C++ first (closest to C, easiest port), then Go, then Rust.

</vision>

<essential>
## What Must Be Nailed

- **All three languages validated** — C++, Go, and Rust all pass iteration count validation against the C reference. That's the milestone.
- **Exact algorithm match** — Iteration counts MUST match perfectly. This is the core validation requirement.

</essential>

<boundaries>
## What's Out of Scope

- Matrix 6 testing — that's Phase 6, don't touch the 622M iteration matrix
- Extensive optimization — get basic variants working, don't hunt micro-optimizations
- UI/report changes — no new columns or features, just add languages to existing system

</boundaries>

<specifics>
## Specific Ideas

- C++ first — closest to C, should be the easiest port and builds confidence
- Follow the established pattern from Phase 1 (runMe.sh, get_compile_flags(), README.md)
- Basic compiler variants (-O2, -O3) but no rabbit holes

</specifics>

<notes>
## Additional Context

This is the first expansion beyond the C baseline. Success here validates the whole benchmark framework can scale to multiple languages. The pattern established will carry through Phases 3-5.

</notes>

---

*Phase: 02-compiled-languages-wave*
*Context gathered: 2025-12-18*
