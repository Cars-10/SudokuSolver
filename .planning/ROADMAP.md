# Roadmap

## Proposed Phases

| # | Phase | Goal | Requirements | Success Criteria |
|---|-------|------|--------------|------------------|
| 1 | Scoring Engine | Implement weighted scoring | SCORE-01, SCORE-02, SCORE-03, SCORE-04 | 4 |
| 2 | UI/UX Refinement | Clean up dashboard & fix fullscreen | UI-01, UI-02, UI-03, UI-04 | 4 |
| 3 | Metadata Alignment| Ensure docs match code | META-01, META-02 | 2 |

---

## Phase Details

### Phase 1: Scoring Engine
**Goal:** Establish a fair and transparent ranking system.
**Requirements:** SCORE-01, SCORE-02, SCORE-03, SCORE-04
**Success Criteria:**
1. Scoring algorithm correctly calculates weighted geometric mean for all languages.
2. C baseline normalization is verified (C score is 1.0/reference point).
3. HTML report successfully renders scores in the main table.
4. Weights can be adjusted in configuration without changing core logic.

### Phase 2: UI/UX Refinement
**Goal:** Clean up the interface and fix Matrix Race behavior.
**Requirements:** UI-01, UI-02, UI-03, UI-04
**Success Criteria:**
1. Table cell clutter is reduced by removing redundant 'Run' buttons.
2. Matrix Race fullscreen toggle is stable using `screenfull`.
3. Fullscreen exit does not trigger immediate re-entry.
4. UI provides clear feedback on active/inactive fullscreen state.

### Phase 3: Metadata Alignment
**Goal:** Ensure consistency between implementation and documentation.
**Requirements:** META-01, META-02
**Success Criteria:**
1. Methodology documentation matches the implemented scoring formula.
2. All UI tooltips and labels are consistent with the new metrics.
