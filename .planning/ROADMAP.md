# Roadmap: Sudoku Solver Enhancements

## Overview

Multi-algorithm Sudoku benchmark suite demonstrating performance trade-offs across different solving approaches. The core brute-force benchmark remains the "Red Pill" baseline, while advanced algorithms (DLX, CP) are housed in separate directory structures for clean comparison.

## Domain Expertise

None

## Milestones

- ✅ **v1.1 Algorithmic Expansion** - [milestones/v1.1-ROADMAP.md](milestones/v1.1-ROADMAP.md) (Phases 1-3, shipped 2026-01-13)
- 🚧 **v1.2 Interactive Reporting** - Phases 4-6 (in progress)

## All Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

<details>
<summary>✅ v1.1 Algorithmic Expansion (Phases 1-3) — SHIPPED 2026-01-13</summary>

- [x] Phase 1: Metadata Enrichment (1/1 plans) — completed 2026-01-13
- [x] Phase 2: Algorithm - Dancing Links (3/3 plans) — completed 2026-01-13
  - [x] Phase 2.1: Refactor Structure (INSERTED) — completed 2026-01-13
- [x] Phase 3: Algorithm - Constraint Propagation (2/2 plans) — completed 2026-01-13

[Full details in milestones/v1.1-ROADMAP.md](milestones/v1.1-ROADMAP.md)

</details>

### 🚧 v1.2 Interactive Reporting (In Progress)

**Milestone Goal:** Enhanced reporting UI with algorithm selection, interactive charts, and infrastructure fixes for directory restructuring.

#### Phase 4: Infrastructure Fixes

**Goal**: Update all BruteForce runMe.sh scripts for new directory structure and fix modal paths
**Depends on**: Phase 3 (v1.1 complete)
**Research**: Unlikely (internal patterns, path updates)
**Plans**: TBD

Plans:
- [ ] 04-01: TBD (run /gsd:plan-phase 4 to break down)

#### Phase 5: Algorithm Selector UI

**Goal**: Build interactive UI controls for algorithm filtering and selection
**Depends on**: Phase 4
**Research**: Unlikely (UI patterns, existing report framework)
**Plans**: TBD

Plans:
- [ ] 05-01: TBD (run /gsd:plan-phase 5 to break down)

#### Phase 6: Core Performance Charts

**Goal**: Implement essential performance comparison visualizations (algorithm comparison, language performance, iteration counts)
**Depends on**: Phase 5
**Research**: Unlikely (charting libraries already used in project)
**Plans**: TBD

Plans:
- [ ] 06-01: TBD (run /gsd:plan-phase 6 to break down)

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 2.1 (inserted) → 3 → 4 → 5 → 6

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Metadata Enrichment | v1.1 | 1/1 | Complete | 2026-01-13 |
| 2. Algorithm - Dancing Links | v1.1 | 3/3 | Complete | 2026-01-13 |
| 3. Algorithm - Constraint Propagation | v1.1 | 2/2 | Complete | 2026-01-13 |
| 4. Infrastructure Fixes | v1.2 | 0/? | Not started | - |
| 5. Algorithm Selector UI | v1.2 | 0/? | Not started | - |
| 6. Core Performance Charts | v1.2 | 0/? | Not started | - |
