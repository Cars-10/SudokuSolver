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
- [x] **Phase 2: Algorithm - Dancing Links** - Implement DLX framework and reference solver.
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
**Status**: COMPLETE (2026-01-13)
**Completed**: 2026-01-13
**Plans**:
  - [x] 1. Scaffolding (Create dir structure, build script, headers)
  - [x] 2.1. Refactor (INSERTED - Unify directory structure to Algorithms/)
  - [x] 2. Implementation (Core search algo, Sudoku mapping)

### Phase 3: Algorithm - Constraint Propagation
**Goal**: Establish `Algorithms/CP` structure and implement the reference C solver using Constraint Propagation.
**Depends on**: Phase 2
**Research**: Completed during planning
**Research topics**: Constraint propagation techniques (singleton elimination + hidden singles), MRV heuristic
**Status**: COMPLETE (2026-01-13)
**Completed**: 2026-01-13
**Plans**:
  - [x] 1. Scaffolding (Create dir structure, build script, headers, data structures)
  - [x] 2. Implementation (Core propagation, backtracking search, I/O integration)

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Metadata Enrichment | 1/1 | Done | 2026-01-13 |
| 2. Algorithm - Dancing Links | 3/3 | Done | 2026-01-13 |
| 3. Algorithm - Constraint Propagation | 2/2 | Done | 2026-01-13 |
