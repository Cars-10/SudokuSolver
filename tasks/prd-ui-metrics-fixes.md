# PRD: UI and Metrics Fixes

## Introduction

This PRD addresses several UI issues in the benchmark report and restructures the modal system. The main goals are: (1) fix bugs causing duplicate entries, missing run buttons, and empty cells, (2) move Performance Profile and Matrix Results from the language modal to a new Score Modal, and (3) improve cell interaction with auto-refresh after runs.

## Goals

- Fix F# duplicate entry caused by mixed naming conventions
- Fix C language missing run buttons due to incorrect lock state loading
- Enable run buttons on empty cells and auto-populate results
- Move Performance Profile (radar chart) and Matrix Results to a new Score Modal
- Add C baseline comparison visualization in Score Modal
- Show computer icon (💻) only when `runType` is explicitly "Local"
- Auto-refresh cells after benchmark runs complete

## User Stories

---

### US-001: Fix F# duplicate entry
**Description:** As a user, I want to see F# listed only once so the report is accurate.

**Acceptance Criteria:**
- [ ] Identify source of duplicate in `session_state.json` (both "F_Sharp" and "F#" in selected list)
- [ ] Normalize all language references to use directory name format (F_Sharp, C_Sharp)
- [ ] Display conversion happens only at render time (F_Sharp → F#, C_Sharp → C#)
- [ ] Verify F# appears exactly once in the report
- [ ] Typecheck passes

---

### US-002: Fix C language missing run buttons
**Description:** As a user, I want run buttons on C language cells so I can re-run benchmarks.

**Acceptance Criteria:**
- [ ] Investigate `lockedLanguages` loading - currently reads from `benchmark_config.json` but lock state is in `session_state.json`
- [ ] Update HTMLGenerator.ts to read locked languages from correct source OR sync the two files
- [ ] C language cells show run buttons (⏵) unless explicitly locked via UI toggle
- [ ] All other languages also correctly show/hide run buttons based on lock state
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-003: Fix empty cells for Ada, CoffeeScript, and similar languages
**Description:** As a user, I want to see run buttons on empty cells so I can run benchmarks for languages without results.

**Acceptance Criteria:**
- [ ] Cells with no results (empty metrics.json `[]`) still render with a run button
- [ ] Cells show "-" for time/iterations/memory when no data exists
- [ ] Run button is visible and functional for empty cells
- [ ] After running, cell populates with actual results (see US-007)
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-004: Show computer icon only for explicit Local runs
**Description:** As a user, I want the computer icon (💻) to indicate actual local runs, not just missing data.

**Acceptance Criteria:**
- [ ] Computer icon (💻) appears ONLY when `runType === "Local"` (exact match)
- [ ] No icon shown when `runType` is missing, undefined, or empty string
- [ ] Docker icon (🐳) still appears for `runType === "Docker"`
- [ ] AI tag still appears for `runType === "AI"`
- [ ] Update HTMLGenerator.ts lines 2009-2016 to remove default fallback to Local
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-005: Create Score Modal component
**Description:** As a user, I want to click on a score cell to see detailed performance breakdown in a dedicated modal.

**Acceptance Criteria:**
- [ ] New modal opens when clicking on score cell (tier badge or score number)
- [ ] Modal title shows language name and overall score
- [ ] Modal is visually distinct from language modal (different header color or icon)
- [ ] Modal closes with X button, Escape key, or clicking outside
- [ ] Modal positions near click location (same behavior as language modal)
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-006: Move Performance Profile to Score Modal
**Description:** As a user, I want to see the radar chart in the Score Modal with C baseline comparison.

**Acceptance Criteria:**
- [ ] Radar chart (D3.js) moved from language modal to Score Modal
- [ ] Chart shows 4 categories: Speed, Memory, Efficiency, I/O
- [ ] C baseline (1.0) shown as reference circle with label
- [ ] Add visual comparison: overlay C's scores (all 1.0) as a second polygon in different color
- [ ] Legend indicates "This Language" vs "C Baseline"
- [ ] Tooltip on hover shows exact values for both
- [ ] Remove radar chart from language modal
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-007: Move Matrix Results to Score Modal
**Description:** As a user, I want to see per-matrix details in the Score Modal with C comparison.

**Acceptance Criteria:**
- [ ] Matrix Results section moved from language modal to Score Modal
- [ ] Each matrix row shows: Language time | C baseline time | Ratio
- [ ] Color coding: green if faster than C, red if slower
- [ ] Expandable details still available (memory, CPU, page faults, etc.)
- [ ] Remove Matrix Results from language modal
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-008: Auto-refresh cell after benchmark run
**Description:** As a user, I want cells to update automatically after running a benchmark.

**Acceptance Criteria:**
- [ ] After `runSolver()` completes successfully, refresh the specific matrix cell
- [ ] Cell updates with new time, iterations, memory values
- [ ] If all 5 matrices for a language complete, also update the score cell
- [ ] Show brief loading indicator during run (e.g., spinner in cell)
- [ ] Handle errors gracefully - show error state in cell if run fails
- [ ] No full page reload required
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-009: Update language modal without performance sections
**Description:** As a developer, I need to clean up the language modal after moving sections to Score Modal.

**Acceptance Criteria:**
- [ ] Language modal no longer contains Performance Profile section
- [ ] Language modal no longer contains Matrix Results section
- [ ] Language modal still contains: Description, Creators, Authors, Image, Edit functionality
- [ ] Modal sizing adjusts appropriately to reduced content
- [ ] Add link/button in language modal to open Score Modal (e.g., "View Performance Details →")
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

## Functional Requirements

- FR-1: Normalize language names to directory format internally, convert to display format only at render
- FR-2: Load locked language state from `session_state.json` or sync with `benchmark_config.json`
- FR-3: Render run buttons for all non-locked cells, including those with no data
- FR-4: Show run type icon only when `runType` field explicitly matches known values
- FR-5: Score cells must be clickable and open Score Modal
- FR-6: Score Modal must display radar chart with C baseline overlay
- FR-7: Score Modal must display matrix results with C comparison columns
- FR-8: `runSolver()` callback must update DOM without page reload
- FR-9: Language modal must link to Score Modal for performance details

## Non-Goals

- No changes to the actual benchmark running logic (common.sh, runMe.sh)
- No changes to metrics.json schema
- No historical trending in Score Modal (future enhancement)
- No batch run functionality changes
- No changes to the server API endpoints

## Technical Considerations

- Score Modal reuses existing D3.js radar chart code with modifications for baseline overlay
- Cell refresh uses existing `/api/run-solver` endpoint response data
- Language name normalization should be centralized in a utility function
- Lock state source of truth needs to be documented (recommend session_state.json)
- Both modals can be open simultaneously (language + score) - consider z-index management

## Design Considerations

- Score Modal header: Use a different accent color (e.g., blue) to distinguish from language modal (green)
- Baseline comparison in radar chart: C baseline as dashed line, language as solid filled polygon
- Matrix comparison table: Side-by-side columns with ratio percentage (e.g., "1.5x slower")
- Empty cell styling: Subtle dashed border to indicate "no data yet"
- Loading state: Small spinner icon replacing the play button during run

## Success Metrics

- F# appears exactly once in report (bug fix verified)
- All languages show appropriate run buttons based on lock state
- Users can access detailed performance data via Score Modal
- Cell updates occur within 2 seconds of benchmark completion
- No increase in page load time despite additional modal code

## Open Questions

- Should the Score Modal also show variant selector if multiple runs exist?
- Should we add keyboard navigation between Score Modal entries (arrow keys)?
- Should empty cells show "Run to see results" text or just the run button?
