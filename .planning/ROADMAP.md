# Roadmap: Sudoku Solver Enhancements

## Overview

We are expanding the Sudoku Solver benchmark to include advanced algorithms (Dancing Links and Constraint Propagation) and enriching the metadata for better reporting. The core brute-force benchmark will remain the "Red Pill" standard, while new algorithms will be housed in separate directory structures to allow for performance comparisons without polluting the main namespace.

## Domain Expertise

None

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [x] **Phase 1: Metadata Enrichment** - Enrich metadata.json with descriptions, categories, and history.
- [ ] **Phase 2: Algorithm - Dancing Links** - Implement DLX framework and reference solver.
- [ ] **Phase 3: Algorithm - Constraint Propagation** - Implement CP framework and reference solver.

## Phase Details

### Phase 1: Metadata Enrichment
**Goal**: Update `metadata.json` to include descriptions, categorization, and history tracking for all 15+ languages.
**Depends on**: Nothing (first phase)
**Research**: Unlikely (JSON schema update and data entry)
**Plans**: 1 (Complete)

### Phase 2: Algorithm - Dancing Links
**Goal**: Establish `Algorithms/DLX` structure and implement the reference C solver using Algorithm X.
**Depends on**: Phase 1 (conceptually independent, but good to have metadata sorted)
**Research**: Likely (DLX implementation details in C)
**Research topics**: DLX algorithm specifics, data structure memory management in C.
**Plans**: TBD

### Phase 3: Algorithm - Constraint Propagation
**Goal**: Establish `Algorithms/CP` structure and implement the reference C solver using Constraint Propagation.
**Depends on**: Phase 2
**Research**: Likely (CP implementation details)
**Research topics**: Constraint propagation techniques suitable for Sudoku, potential optimizations.
**Plans**: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Metadata Enrichment | 1/1 | Done | 2026-01-13 |
| 2. Algorithm - Dancing Links | 0/TBD | Not started | - |
| 3. Algorithm - Constraint Propagation | 0/TBD | Not started | - |