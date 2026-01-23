# report_client.js Modularization Plan

## Current State
- **File size**: 5,547 lines
- **Status**: Monolithic file with all functionality mixed together
- **Issues**: Hard to maintain, test, and understand

## Proposed Module Structure

### 1. `modules/globals.js` ✅ COMPLETED
**Purpose**: Global state and configuration
- Export `state` object with current sort, docker mode flags
- `switchExecutionMode()` - Toggle between Docker/Local
- `initializeDockerToggle()` - Initialize UI based on port
- **Status**: Fully implemented and tested

### 2. `modules/utils.js` ✅ COMPLETED
**Purpose**: Shared utility functions
- `normalizeMatrix(m)` - Handle matrix name formats
- `hexToRgba(hex, alpha)` - Color conversion
- `trapFocus(element)` - Accessibility helper
- `updateRelativeTimes()` - Format timestamps (e.g., "3d ago")
- **Status**: Fully implemented

### 3. `modules/table-operations.js` ✅ COMPLETED
**Purpose**: Table sorting, filtering, and display
**Functions extracted**:
- `filterLanguages()` - Search/filter table rows
- `sortRows(metric, btn)` - Sort main table by column
- `sortMatrix(index, metric, btn)` - Sort matrix columns
- `toggleMismatches()` - Show/hide mismatch rows

**Dependencies**: globals.js (for currentSort state)
**Status**: Fully implemented

### 4. `modules/modal-operations.js` ✅ COMPLETED
**Purpose**: Modal window management
**Functions extracted**:
- `closeModal(event)` - Close language detail modal
- `showMethodology()` / `closeMethodology(event)`
- `showGoals()` / `closeGoals(event)`
- `showWhy()` / `closeWhy(event)`
- `closeSourceModal()` / `closeMismatchModal()` / `closeScoreModal()`
- `makeDraggable(modal, handle)` - Make modals draggable
- `makeElementDraggable(elmnt, header)` - Generic drag handler
- `enableModalDragging()` - Initialize dragging for all modals

**Dependencies**: utils.js (for trapFocus)
**Status**: Fully implemented

### 5. `modules/personality.js` ✅ COMPLETED
**Purpose**: Narrator personality/voice changes
**Functions extracted**:
- `changePersonality()` - Switch narrator voice
- Update tooltips, labels, and methodology based on persona
- Mismatch button text updates

**Dependencies**: None (uses window.* global metadata)
**Status**: Fully implemented

### 6. `modules/ui-persistence.js` ✅ COMPLETED
**Purpose**: Save/restore UI state in localStorage
**Functions extracted**:
- `saveUIState()` - Save current UI state
- `restoreUIState()` - Restore saved state on load
- `initializeAutoSave()` - Setup auto-save on changes
- Persists: scroll position, chart mode, algorithm filter, personality, sort, etc.

**Dependencies**: globals.js
**Status**: Fully implemented

### 7. `modules/d3-charts.js` ⏳ TODO
**Purpose**: All D3.js chart rendering
**Section**: Lines 2221-4863 ("D3.js Chart Implementation")
**Functions to extract**:
- `drawScatterPlot()` - Scatter plot rendering
- `drawBarChart()` - Bar chart rendering
- `drawRadarChart()` - Radar chart rendering
- `init3DChart()` - 3D chart with Three.js
- Chart update/resize handlers
- Zoom/pan functionality

**Dependencies**: utils.js (for hexToRgba)

### 8. `modules/diagnostics.js` ⏳ TODO
**Purpose**: Language diagnostics display
**Functions to extract**:
- `showDiagnostics()` - Open diagnostics modal
- `closeDiagnostics(event)` - Close diagnostics modal
- `drawScoreRadarChart(lang, tier, tierColor, ...)` - Score breakdown chart
- `populateMatrixResults(lang, langMetrics, cMetrics)` - Matrix comparison table

**Dependencies**: utils.js, modal-operations.js

### 9. `modules/matrix-results.js` ⏳ TODO
**Purpose**: Matrix result comparison and display
**Functions to extract**:
- `populateMatrixResults()` - Generate comparison tables
- Matrix-specific formatters and helpers

**Dependencies**: utils.js

## Implementation Steps

1. ✅ Create `modules/globals.js` and `modules/utils.js`
2. ✅ Extract table operations (filtering, sorting)
3. ✅ Extract modal operations (open, close, drag)
4. ✅ Extract personality system
5. ✅ Extract UI persistence (localStorage)
6. ✅ Create main `report_client_modular.js` that imports all modules
7. ⏳ Update `HTMLGenerator.ts` to use modular version
8. ⏳ Test all functionality
9. 🔜 Extract D3 chart rendering (largest section - can be done later)
10. 🔜 Extract diagnostics display (can be done later)
11. 🔜 Extract matrix results (can be done later)

## Module Loading Pattern

Use ES6 modules with type="module":

```html
<script type="module">
  import { state, switchExecutionMode, initializeDockerToggle } from './Metrics/modules/globals.js';
  import { normalizeMatrix, hexToRgba, trapFocus } from './Metrics/modules/utils.js';
  import { filterLanguages, sortRows, sortMatrix } from './Metrics/modules/table-operations.js';
  // ... more imports

  // Expose to window for onclick handlers
  window.filterLanguages = filterLanguages;
  window.sortRows = sortRows;
  // ... etc

  // Initialize on load
  document.addEventListener('DOMContentLoaded', () => {
    initializeDockerToggle();
    // ... other init
  });
</script>
```

## Benefits

1. **Maintainability**: Each module has single responsibility
2. **Testability**: Can test modules independently
3. **Performance**: Modules can be lazy-loaded
4. **Collaboration**: Team members can work on different modules
5. **Debugging**: Easier to locate and fix issues
6. **Reusability**: Modules can be used in other projects

## Migration Strategy

1. Create modules incrementally (don't break existing code)
2. Keep original `report_client.js` until all modules complete
3. Create `report_client_modular.js` that imports modules
4. Test in parallel (can switch between old/new)
5. Once verified, replace old with new
6. Remove old `report_client.js`

## Testing Checklist

After modularization, verify:
- [ ] Table sorting works (all columns)
- [ ] Search/filter works
- [ ] Mismatch toggle works
- [ ] All modals open/close correctly
- [ ] Modal dragging works
- [ ] Personality switching works
- [ ] All charts render (scatter, bar, radar, 3D)
- [ ] Chart controls work (zoom, pan, reset)
- [ ] Diagnostics modal displays correctly
- [ ] Matrix results comparison works
- [ ] UI state persists across page reloads
- [ ] Docker/Local toggle works
- [ ] No JavaScript errors in console

## Next Steps

Review this plan and let me know if you'd like me to:
1. Continue creating the remaining modules
2. Adjust the module structure
3. Start with a specific module first
4. Something else

The foundation is in place with `globals.js` and `utils.js` already created.
