# PRD: UI Fixes v6 - Sorting Toggle, Score Modal Color, Remove Redundant Buttons

## Introduction

This PRD addresses three UI issues in the benchmark report:
1. Column sorting doesn't toggle between ascending/descending when clicking the same column
2. Score modal radar chart graph color doesn't match the tier color (regression)
3. Redundant sort buttons (NAME, TIME, MEMORY, SCORE) should be removed from the controls area

## Goals

- Fix column sorting toggle to work on same-column clicks
- Restore radar chart color to match tier badge color in Score modal
- Remove redundant sort buttons from the top bar controls
- Maintain existing functionality for table header sorting

## User Stories

### US-001: Fix Column Sorting Toggle
**Description:** As a user, I want to click the same column header repeatedly to toggle between ascending and descending order, so I can quickly reverse the sort direction without clicking another column first.

**Acceptance Criteria:**
- [ ] Clicking the same column header toggles sort direction (asc -> desc -> asc)
- [ ] The sort arrow rotates 180 degrees when direction changes to descending
- [ ] Both `sortRows()` and table header `sort-header` clicks work with toggle
- [ ] Initial click establishes natural sort order (time/score: ascending, others: may vary)
- [ ] Typecheck passes
- [ ] Verify in browser: click "Score" header twice, observe direction toggle

### US-002: Fix Score Modal Radar Chart Color
**Description:** As a user, I want the radar chart in the Score modal to display using the same color as the tier badge, so the visual presentation is consistent.

**Acceptance Criteria:**
- [ ] Radar chart polygon fill uses tier color (S=Gold, A=Green, B=Blue, etc.)
- [ ] Radar chart polygon stroke uses tier color
- [ ] Legend swatch color matches tier color
- [ ] Data point dots on radar use tier color
- [ ] Typecheck passes
- [ ] Verify in browser: open Score modal for different tier languages, confirm chart color matches tier badge

### US-003: Remove Redundant Sort Buttons
**Description:** As a user, I want a cleaner UI without redundant sort buttons, since the table headers already provide sorting functionality.

**Acceptance Criteria:**
- [ ] Remove the "Name" sort button from controls area
- [ ] Remove the "Time" sort button from controls area
- [ ] Remove the "Memory" sort button from controls area
- [ ] Remove the "Score" sort button from controls area
- [ ] Keep "PERSONA" selector, "Show Mismatches" toggle, and "Diagnostics" button
- [ ] Layout remains clean after button removal
- [ ] Typecheck passes
- [ ] Verify in browser: confirm buttons removed, table header sorting still works

## Functional Requirements

- FR-1: The `sortRows()` function in `report_client.js` must toggle `currentSort.dir` when the same metric is clicked consecutively
- FR-2: The `sortRows()` function must update `currentSort.metric` on each call to enable same-column toggle detection
- FR-3: The `openScoreModal()` function must pass the correct `tierColor` to `drawScoreRadarChart()`
- FR-4: The `drawScoreRadarChart()` must use the provided `tierColor` parameter for polygon fill/stroke
- FR-5: Remove the four sort buttons (`Name`, `Time`, `Memory`, `Score`) from HTMLGenerator.ts lines 1052-1055

## Non-Goals

- No changes to the actual sorting algorithm logic
- No changes to tier color definitions
- No changes to table header click behavior
- No changes to other modal functionality

## Technical Considerations

### Sorting Toggle Fix
The issue is in `report_client.js` `sortRows()` function. Compare with `sortMatrix()` which correctly toggles:

```javascript
// sortMatrix has this (correct):
if (currentSort.metric === fullMetric) {
    currentSort.dir *= -1;
} else {
    currentSort.metric = fullMetric;
    currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
}

// sortRows is missing the toggle logic - it only uses currentSort.dir but never updates it
```

### Score Modal Color
The code at lines 3718-3724 in `report_client.js` already gets tierColor and passes it to `drawScoreRadarChart()`. The function at line 3736 receives it. Need to verify the color is being used correctly throughout the drawing code.

### Files to Modify
- `Metrics/report_client.js` - Fix sortRows toggle logic, verify radar chart color usage
- `Metrics/HTMLGenerator.ts` - Remove four sort buttons (lines 1052-1055)

## Success Metrics

- User can click same column header twice to reverse sort order
- Score modal radar chart color visually matches tier badge for all tier levels
- UI has 4 fewer buttons in the controls area, cleaner appearance

## Open Questions

None - requirements are clear.
