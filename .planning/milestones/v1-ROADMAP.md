# Milestone v1: Scoring, UI, & Metadata

**Status:** ✅ SHIPPED 2026-01-20
**Phases:** 1-3
**Total Plans:** ~5

## Overview

Milestone v1 focused on establishing a fair, weighted scoring system for the benchmarks, refining the report UI (removing clutter, fixing fullscreen modes), and aligning the documentation with the implementation.

## Phases

### Phase 1: Scoring Engine

**Goal**: Establish a fair and transparent ranking system.
**Depends on**: None
**Plans**: 1 plan (implied)

Plans:

- [x] 01-01: Implement weighted geometric mean scoring (80% Time, 20% Memory) normalized against C baseline.

**Details:**
(Summary reconstructed from requirements)
- Implemented `SCORE-01`, `SCORE-02`, `SCORE-03`, `SCORE-04`.
- Normalized all metrics against C baseline.
- Updated HTMLGenerator to calculate scores at build time.

### Phase 2: UI/UX Refinement

**Goal**: Clean up the interface and fix Matrix Race behavior.
**Depends on**: Phase 1
**Plans**: 2 plans

Plans:

- [x] 02-01: Remove individual "Run" buttons
- [x] 02-02: Fix Matrix Race fullscreen & re-entry bugs

**Details:**
- Removed "Run" buttons from table cells.
- Integrated `screenfull` library.
- Fixed Matrix Race screensaver re-entry loop.
- Added visual indication of fullscreen state.

### Phase 3: Metadata Alignment

**Goal**: Ensure consistency between implementation and documentation.
**Depends on**: Phase 2
**Plans**: 1 plan

Plans:

- [x] 03-01: Update metadata and scoring formula

**Details:**
- Updated README.md with 80/20 weighted formula.
- Aligned UI tooltips and modals with new logic.
- Removed CPU metric from scoring view (not used in formula).

---

## Milestone Summary

**Key Decisions:**
- Decision: Use 80% Time / 20% Memory weighting (Rationale: Time is primary metric for brute force, memory is secondary).
- Decision: Local execution focus (Rationale: Docker integration deferred to later).

**Issues Resolved:**
- Fixed "sticky" fullscreen mode in Matrix Race.
- Removed clutter from main table.

**Technical Debt Incurred:**
- `screenfull` dependency installed but native API used in some parts (noted in audit).
- Polling disabled in HTMLGenerator due to missing timestamp mechanism.

---

_For current project status, see .planning/ROADMAP.md_
