# Sudoku Solver (SS) - The "Red Pill" Benchmark

## Current State (Updated: 2025-12-24)

**Shipped:** v1.0 (Multiple iterations, 15+ Tier 1 languages stable)
**Status:** Live / Active Benchmarking
**Users:** Polyglot developers, performance enthusiasts.
**Feedback:** Core benchmarking logic is solid; UI and Docker infrastructure need maintenance.

**Codebase:**
- **Languages**: 80+ directory entries, with ~15+ considered "Tier 1".
- **Stack**: C (Reference), Bash (Runner), TypeScript/Node.js (Metrics/Server), Docker.
- **Architecture**: Recursive backtracking algorithm required for all implementations to ensure "Algorithmic Purity".

**Known Issues:**
- Content Server UI (modals, editing) is partially broken.
- Iteration counts in history might not match the fresh C reference.
- Docker infrastructure needs rebuilding on the new server.

## vNext Goals

**Vision:** Scale to 80+ fully verified languages while maintaining strict algorithmic consistency and a world-class "Red Pill" visual reporting dashboard.

**Motivation:**
- Ensure fair performance comparisons across diverse programming paradigms.
- provide a high-visibility tool for analyzing language overhead in a standardized computational task.

**Scope (v1.x):**
- **Infrastructure**: Stabilize Docker builds and `runMeGlobal.sh` cross-platform compatibility.
- **Accuracy**: Audit all implementations to ensure Matrix 1 yields exactly 656 iterations.
- **UI/UX**: Repair the Content Server modal and editing features; enhance the "Neon" theme.
- **Expansion**: Verify and "Lock" more languages from the `selected` list in `session_state.json`.

**Success Criteria:**
- [ ] Matrix 1 rule (656 iterations) strictly enforced across all Tier 1 languages.
- [ ] 100% functional Content Server UI for editing language metadata.
- [ ] Reproducible Docker environment for all 80+ languages.
- [ ] Single-file `benchmark_report.html` remains high-performance and visually stunning.

**Not Building (this version):**
- Advanced optimizations like DLX or constraint propagation (kept as separate variants only).
- Multi-threaded solvers (unless explicitly marked as a variant).

## Constraints

- **Algorithmic Consistency**: Every implementation must use the *exact same* recursive backtracking algorithm.
- **"Red Pill" Aesthetic**: High-contrast, Neon dark mode is the standard for all UI/Reports.
- **Darwin Primary**: Development focus on macOS/Darwin, with Docker for Linux/CI parity.

## Open Questions

- [ ] How to best handle language-specific optimizations that might technically follow backtracking but skip steps (e.g. pre-calculated sets)?
- [ ] Should we migrate from `session_state.json` to a more robust persistence layer for UI edits?

---

<details>
<summary>Original Vision (v1.0 - Archived)</summary>

## Vision
A polyglot benchmarking project designed to compare the performance of 15+ "Tier 1" programming languages on an identical brute-force Sudoku solving task.

## Problem
Comparing programming language performance fairly on a computational task with identical algorithms.

## Success Criteria
- [x] 15+ Tier 1 languages implemented.
- [x] Identical recursive backtracking algorithm used across implementations.
- [x] "Red Pill" neon theme for reports.
- [x] Functional `runMeGlobal.sh` script.

## Decisions Made

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Algorithm | Recursive Backtracking | Simplest fair comparison point for brute-force. |
| Reporting | Single-file HTML | Portability and ease of sharing. |
| Visual Style | Neon Dark Mode | Project identity and "Red Pill" philosophy. |
| Database | Raw SQL/JSON | Pragmatism and visibility. |

</details>

*Initialized: 2025-12-24*
