# Sudoku Solver Benchmark

## What This Is
A polyglot benchmarking project that compares the performance of 70+ programming languages on a brute-force Sudoku solving task. It provides a visual dashboard for analyzing metrics like execution time, memory usage, and iteration consistency.

## Core Value
Providing a fair, transparent, and visually engaging comparison of programming language performance using a standardized algorithmic implementation.

## Requirements

### Validated
- ✓ Standard recursive backtracking algorithm across 70+ languages — existing
- ✓ Automated benchmarking suite with iteration count verification — existing
- ✓ Single-file HTML report generation with data visualization — existing
- ✓ Language metadata and author attribution system — existing
- ✓ Matrix-based testing (1.matrix to 6.matrix) — existing

### Active
- [ ] Define and implement a robust language scoring algorithm
- [ ] Weight performance metrics (CPU, Memory, Time) for ranking
- [ ] Clean up UI: Remove redundant "run" buttons from cells
- [ ] Fix Matrix Race fullscreen exit behavior
- [ ] Disable automatic fullscreen re-entry in Matrix Race
- [ ] Embellish and refine language metadata

### Out of Scope
- [ ] Non-brute-force optimizations (DLX, constraint propagation) — must maintain algorithmic purity for benchmark fairness
- [ ] Remote/Docker execution for this milestone — focusing on local performance and UI refinement

## Context
- The project uses a Neon dark mode visual theme.
- Solvers are implemented in a wide variety of "Tier 1" and esoteric languages.
- Current UI issues include sticky fullscreen modes and cluttered cell interactions.
- Performance data is stored in SQLite and JSON formats.

## Constraints
- **Algorithmic**: Standard recursive backtracking — Required for fair comparison across all languages.
- **Environment**: macOS (Darwin) — Primary development and execution environment.
- **Consistency**: Iteration counts must match C reference exactly (656 for Matrix 1).

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Local Execution Focus | User specified ignoring docker commands for now to focus on local refinements. | — Pending |
| Scored Ranking | Moving from raw metrics to a weighted score to provide a "best" ranking. | — Pending |

---
*Last updated: 2026-01-18 after initialization*
