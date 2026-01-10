# PRD: UI Refinements v2

## Introduction

This PRD covers the next phase of UI refinements for the benchmark report, focusing on cleaning up visual clutter in language cells, improving Score Modal layout with side-by-side sections, narrowing table columns for better fit, adding variant comparison capability, and fixing single-matrix run behavior including timestamp updates.

## Goals

- Remove visual clutter from language cells (chevron "›" and ellipsis "...")
- Redesign Score Modal layout: wider modal with Performance Profile next to Composite Score
- Match Performance Profile graph colors to score tier colors
- Reduce table column widths by ~10% for better viewport fit
- Add variant selector dropdown to Score Modal for comparing runs
- Fix single-matrix run button to only run the selected matrix
- Update the "Updated" timestamp when any single matrix is run from UI
- Add API endpoints for historical data and variant comparison

## User Stories

---

### US-001: Remove expand chevron from language cells
**Description:** As a user, I want a cleaner language cell without the "›" chevron so the interface looks less cluttered.

**Acceptance Criteria:**
- [ ] Remove `<span class="expand-chevron">›</span>` from language cell in HTMLGenerator.ts line ~2073
- [ ] Remove any CSS for `.expand-chevron` class
- [ ] Language cells show only: logo, name, type icon (Docker/Local/AI), and year
- [ ] Verify row expansion still works (if applicable) via other click targets
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-002: Remove ellipsis indicator from language cells
**Description:** As a user, I want no "..." or truncation indicators in language cells for a cleaner look.

**Acceptance Criteria:**
- [ ] Identify and remove any "..." text or ellipsis styling from language cells
- [ ] Check `.lang-col` CSS for `text-overflow: ellipsis` and remove if causing visual issues
- [ ] Language names display fully without truncation indicators
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-003: Make Score Modal wider with two-column layout
**Description:** As a user, I want the Score Modal to be wider with the Performance Profile chart displayed next to the Composite Score section for better information density.

**Acceptance Criteria:**
- [ ] Increase Score Modal width from current size to ~800px or 90vw (whichever is smaller)
- [ ] Create two-column layout in modal body:
  - Left column: Composite Score value + Score Breakdown ratios
  - Right column: Performance Profile radar chart with legend
- [ ] Use CSS flexbox or grid for responsive side-by-side layout
- [ ] On narrow screens (<768px), stack columns vertically
- [ ] Matrix Results table remains full-width below the two columns
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-004: Match radar chart color to score tier color
**Description:** As a user, I want the Performance Profile radar chart polygon to use the same color as the language's tier badge for visual consistency.

**Acceptance Criteria:**
- [ ] Radar chart polygon color matches tier color scheme:
  - S tier: Gold/Yellow (#ffd700)
  - A tier: Green (#00ff9d)
  - B tier: Blue (#7aa2f7)
  - C tier: Orange (#ff9e64)
  - D tier: Red (#f7768e)
  - F tier: Dark Red (#db4b4b)
- [ ] Update `drawScoreRadarChart()` function to accept tier as parameter
- [ ] C baseline polygon remains a neutral color (e.g., rgba(122, 162, 247, 0.3))
- [ ] Legend updates to show tier-colored swatch for "This Language"
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-005: Reduce table column widths by 10%
**Description:** As a user, I want the benchmark table columns to be ~10% thinner so the entire table fits within the viewport without horizontal scrolling.

**Acceptance Criteria:**
- [ ] Reduce width of all table columns proportionally:
  - Language column: reduce padding/min-width
  - Score column: reduce padding
  - Updated column: reduce width
  - Matrix columns: reduce min-width from current value
  - Total Time column: reduce padding
- [ ] Table fits within 1920px viewport without horizontal scroll
- [ ] Content remains readable and not overly cramped
- [ ] Test on 1440px and 1280px viewports for acceptable display
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-006: Add variant selector dropdown to Score Modal
**Description:** As a user, I want to select different benchmark variants (compiler flags, runtime versions) in the Score Modal to compare performance across runs.

**Acceptance Criteria:**
- [ ] Add dropdown selector above or near the score display in Score Modal
- [ ] Dropdown populated with available variants for the selected language
- [ ] Default selection is the most recent run
- [ ] Selecting a variant updates:
  - Composite score and breakdown ratios
  - Radar chart data
  - Matrix results table
- [ ] If only one variant exists, dropdown is hidden or disabled
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-007: Create API endpoint for language variants
**Description:** As a developer, I need an API endpoint to retrieve all available variants for a language to populate the variant selector.

**Acceptance Criteria:**
- [ ] Create `GET /api/variants/:language` endpoint in server/index.js
- [ ] Returns array of variant objects: `[{variant: "O2", timestamp: "...", runType: "Local"}, ...]`
- [ ] Sorted by timestamp descending (most recent first)
- [ ] If no variants field exists in metrics.json, return single entry with variant: "default"
- [ ] Handle missing language gracefully with 404
- [ ] Typecheck passes

---

### US-008: Create API endpoint for specific variant metrics
**Description:** As a developer, I need an API endpoint to fetch metrics for a specific variant to display in the Score Modal.

**Acceptance Criteria:**
- [ ] Create `GET /api/metrics/:language/:variant` endpoint in server/index.js
- [ ] Returns the full metrics run object for the specified variant
- [ ] If variant not found, return 404
- [ ] Include all matrix results for that variant
- [ ] Typecheck passes

---

### US-009: Fix single-matrix run button behavior
**Description:** As a user, when I click the run button on a specific matrix cell, I want only that matrix to run, not all matrices.

**Acceptance Criteria:**
- [ ] Verify `runSolver(lang, matrix, event)` correctly passes the matrix parameter
- [ ] Server `/api/run` endpoint correctly handles single matrix argument
- [ ] Only the specified matrix runs (e.g., `./runMe.sh ../../Matrices/1.matrix`)
- [ ] Other matrix results in metrics.json are preserved (merge logic works)
- [ ] Cell loading indicator shows only on the clicked cell
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-010: Update timestamp after single matrix run
**Description:** As a user, I want the "Updated" column to reflect the most recent run time, even if I only ran a single matrix.

**Acceptance Criteria:**
- [ ] After a single matrix run completes, update the row's timestamp in the UI
- [ ] "Updated" column shows "Just now" or appropriate relative time
- [ ] The `data-timestamp` attribute on the row is updated
- [ ] If sorting by timestamp, the row moves to appropriate position
- [ ] Server merges metrics correctly, updating the outer timestamp field
- [ ] Typecheck passes
- [ ] Verify in browser using dev-browser skill

---

### US-011: Persist per-matrix run timestamps
**Description:** As a developer, I need each matrix result to track its individual run timestamp so users know when each matrix was last benchmarked.

**Acceptance Criteria:**
- [ ] Update `/api/run` endpoint to add `run_timestamp` to each matrix result
- [ ] When running single matrix, only that result's `run_timestamp` is updated
- [ ] When running all matrices, all results get the same `run_timestamp`
- [ ] `refreshLanguageCells()` can optionally display per-matrix timestamps in tooltip
- [ ] Backward compatible: existing results without `run_timestamp` handled gracefully
- [ ] Typecheck passes

---

## Functional Requirements

### UI Cleanup
- FR-1: Remove expand chevron element from language cells
- FR-2: Remove any ellipsis indicators from language cells
- FR-3: Language cell shows only: logo + name + type icon + year

### Score Modal Layout
- FR-4: Score Modal minimum width: 800px (or 90vw on smaller screens)
- FR-5: Two-column layout: scores left, radar chart right
- FR-6: Responsive stacking below 768px viewport width
- FR-7: Matrix results table spans full width below columns

### Visual Consistency
- FR-8: Radar chart polygon color matches tier badge color
- FR-9: Legend dynamically shows correct tier color

### Table Sizing
- FR-10: Reduce all column widths by approximately 10%
- FR-11: Table must fit 1920px viewport without horizontal scroll
- FR-12: Maintain readability on 1280px+ viewports

### Variant Comparison
- FR-13: Variant selector populated from `/api/variants/:language`
- FR-14: Variant selection triggers metrics refresh via `/api/metrics/:language/:variant`
- FR-15: All modal sections update when variant changes

### Single Matrix Runs
- FR-16: Run button executes only the specified matrix
- FR-17: Metrics merge preserves other matrix results
- FR-18: Timestamp updated on any run (single or all)
- FR-19: Per-matrix `run_timestamp` tracked in results

## Non-Goals (Out of Scope)

- No changes to the benchmark algorithm or runMe.sh scripts
- No SQLite integration (covered in prd-benchmark-improvements.md)
- No variant auto-detection from compiler (covered in prd-benchmark-improvements.md)
- No historical trending charts
- No batch operations for multiple languages

## Technical Considerations

### CSS Changes
- Use CSS custom properties (variables) for tier colors to ensure consistency
- Consider using CSS Grid for the two-column Score Modal layout
- Test column width changes across multiple viewport sizes

### API Design
- Variant endpoints should be RESTful and follow existing patterns
- Consider caching variant list to avoid repeated file reads
- Merge logic in `/api/run` must handle edge cases (missing files, corrupt JSON)

### Backward Compatibility
- metrics.json files without `variant` field treated as "default"
- Results without `run_timestamp` use parent timestamp or "unknown"
- UI gracefully handles missing optional fields

## Design Considerations

### Score Modal Two-Column Layout
```
+------------------------------------------+
|  [X]  Language Name        Tier: S       |
+------------------------------------------+
|                                          |
|  +----------------+  +----------------+  |
|  | Composite: 0.85|  |   [Radar Chart]|  |
|  | Time:    0.92x |  |                |  |
|  | Memory:  0.78x |  |    /\          |  |
|  | CPU:     0.89x |  |   /  \         |  |
|  | Iter:    1.00x |  |  /    \        |  |
|  +----------------+  +----------------+  |
|                                          |
|  +------------------------------------+  |
|  |  Matrix Results Table              |  |
|  |  1 | 0.52ms | 0.51ms | 1.02x      |  |
|  |  2 | 45.2ms | 42.1ms | 1.07x      |  |
|  +------------------------------------+  |
+------------------------------------------+
```

### Tier Color Mapping
| Tier | Color Name | Hex Code |
|------|------------|----------|
| S | Gold | #ffd700 |
| A | Neon Green | #00ff9d |
| B | Blue | #7aa2f7 |
| C | Orange | #ff9e64 |
| D | Pink/Red | #f7768e |
| F | Dark Red | #db4b4b |

## Success Metrics

- Language cells appear cleaner with reduced visual elements
- Score Modal displays all information without scrolling on 1080p screens
- Table fits viewport without horizontal scrolling at 1440px
- Variant comparison works for languages with multiple runs
- Single matrix runs complete in <5 seconds and update only that cell
- Timestamp updates immediately after any benchmark run

## Open Questions

1. Should the variant dropdown show run date alongside variant name?
2. Should we add a "Compare" mode that overlays two variants on the radar chart?
3. Should matrix results show per-matrix timestamps in the table or just tooltips?
