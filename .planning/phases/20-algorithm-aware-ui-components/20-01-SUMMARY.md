# Phase 20-01 Summary: Algorithm-Aware Line and Jockey Charts

**Phase:** 20-algorithm-aware-ui-components
**Plan:** 01
**Date:** 2026-01-14
**Status:** Complete ✓

## Objective

Add algorithm filtering to line chart and jockey chart for consistent multi-algorithm experience. Ensure all D3.js performance charts respect the selected algorithm filter.

## Tasks Completed

### Task 1: Add algorithm filtering to drawLineChart ✓
**Files:** `Metrics/report_client.js`
**Commit:** 268448b

Added algorithm-aware filtering to the line chart (performance over matrices):
- Added `selectedAlgo` from `window.currentAlgorithm` (defaults to 'BruteForce')
- Created `algoLabel` map for algorithm display names
- Filtered `data` array by `algorithmType` matching `selectedAlgo`
- For "All Algorithms" mode, appended algorithm suffix to solver names (BF/DLX/CP)
- Created `displayData` with enhanced `displayName` property
- Updated all labels, tooltips, and text to use `displayName`

### Task 2: Add algorithm filtering to drawJockeyChart ✓
**Files:** `Metrics/report_client.js`
**Commit:** ae7af05

Added algorithm-aware filtering to the jockey chart (horizontal bar race):
- Added `selectedAlgo` from `window.currentAlgorithm` (defaults to 'BruteForce')
- Created `algoLabel` map for algorithm display names
- Filtered `data` array by `algorithmType` matching `selectedAlgo`
- For "All Algorithms" mode, appended algorithm suffix to solver names
- Updated `sortedData` to use `filteredData` and include `displayName`
- Updated Y axis domain to use `displayName`
- Updated all Y positioning (tracks, bars, logos) to use `displayName`
- Updated tooltips to show `displayName`

## Verification

All verification steps completed successfully:

- ✓ `npx tsc --noEmit` passes without errors
- ✓ `cd Metrics && npx ts-node generate_report_only.ts` generates report successfully
- ✓ Both `drawLineChart` and `drawJockeyChart` have `selectedAlgo` and `filteredData` variables
- ✓ `grep "window.currentAlgorithm" Metrics/report_client.js` shows 5 usages (4 chart functions + 1 filter function)
- ✓ No undefined variable errors in report_client.js
- ✓ Report generation succeeds with 176 languages processed
- ✓ No regressions in existing chart functions

## Technical Details

### Pattern Consistency

Both charts now follow the same algorithm filtering pattern as `drawLanguagePerformanceChart` and `drawIterationCountChart`:

```javascript
const selectedAlgo = window.currentAlgorithm || 'BruteForce';
const algoLabel = {
    'BruteForce': 'Brute Force',
    'DLX': 'Dancing Links',
    'CP': 'Constraint Propagation',
    'all': 'All Algorithms'
}[selectedAlgo] || 'BruteForce';

const filteredData = data.filter(d => {
    const algoType = d.algorithmType || 'BruteForce';
    if (selectedAlgo === 'all') return true;
    return algoType === selectedAlgo;
});
```

### "All Algorithms" Mode Enhancement

When `selectedAlgo === 'all'`, solvers get algorithm suffixes:
- BruteForce implementations: ` (BF)`
- Dancing Links implementations: ` (DLX)`
- Constraint Propagation implementations: ` (CP)`

This makes it clear which algorithm each solver is using when all algorithms are displayed together.

## Files Modified

- `Metrics/report_client.js` - Added algorithm filtering to drawLineChart and drawJockeyChart functions

## Impact

All four performance charts now consistently respect the global algorithm filter:
1. ✓ Line Chart (Performance Over Matrices)
2. ✓ Jockey Chart (Horizontal Bar Race)
3. ✓ Language Performance Chart (already had filtering)
4. ✓ Iteration Count Chart (already had filtering)

Users can now switch between BruteForce, DLX, CP, or "All Algorithms" and see consistent filtering across all visualizations.

## Success Criteria Met

- ✓ All tasks completed
- ✓ Both charts filter by window.currentAlgorithm
- ✓ Chart data accurately reflects algorithm selection
- ✓ "All Algorithms" mode displays algorithm suffixes
- ✓ TypeScript compilation passes
- ✓ Report generation succeeds
- ✓ No regressions in existing chart functions
- ✓ Commits follow atomic task structure

## Next Steps

Continue with remaining plans in Phase 20 to enhance algorithm-aware UI components.
