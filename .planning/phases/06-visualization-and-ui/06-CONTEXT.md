# Phase 6: Visualization & UI - Context

**Gathered:** 2026-01-24
**Status:** Ready for planning

<domain>
## Phase Boundary

Enhance the benchmark report with advanced chart visualizations (scatter plot for Time vs Memory, heatmap for Language x Matrix performance, histogram for score distribution) while fixing existing UI bugs (algorithm dropdown sorting, Matrix Race fullscreen exit). Also add validation UI elements deferred from Phase 4 (warning badges VAL-04, diagnostics modal VAL-05). This phase visualizes and presents data - it does not add new data collection or benchmarking capabilities.

</domain>

<decisions>
## Implementation Decisions

### Chart library
- Use D3.js for all new visualizations
- Provides maximum flexibility and control for custom interactions
- Enables sophisticated handling of 70+ languages and 6 orders of magnitude

### Scatter plot (Time vs Memory)
- Click to highlight + tooltips interaction model
- Clicking a point highlights it AND highlights the related row in the main table
- Hover shows tooltips with language name, exact time/memory values
- Creates visual connection between chart and table data

### Heatmap (Language x Matrix)
- Navigate to detail view on cell click
- Clicking a cell opens a focused view of that specific language-matrix result
- Shows the complete metrics for that combination
- Provides drill-down capability for detailed analysis

### Histogram (Score distribution)
- Show both performance tiers/clusters AND statistical annotations
- Group languages into tiers (fast, medium, slow) with clear boundaries
- Overlay mean/median/quartile markers and statistics
- Combines categorical and statistical views for richer insights

### Logarithmic scaling
- User-toggleable log/linear scale controls
- Provide toggle button to switch between logarithmic and linear scales
- Necessary for handling 6 orders of magnitude variation
- Gives users flexibility to view data in most meaningful way for their analysis

### Language count handling
- Filter/search by language name
- Provide search/filter controls to narrow down which languages are shown in charts
- Prevents overwhelming the viewer with 70+ data points
- Maintains clarity while preserving access to all data

### Outlier visualization
- Different color + marker shape for outliers
- Outliers identified by Phase 5 analysis use distinct color (e.g., red) and different point shape
- Clear visual distinction from normal data points
- Makes statistical anomalies immediately visible

### Axis labeling
- Hybrid approach: human units for small values, scientific notation for large
- Format: "1ms, 100ms, 1s" transitions to "1e3, 1e6" for larger values
- Clear gridlines to aid reading
- Optimizes readability across the full range

### Algorithm dropdown bug fix
- Alphabetical by display name sorting
- Sort options by what the user sees: BruteForce, CP, DLX
- Simple, predictable ordering for users

### Matrix Race fullscreen exit bug fix
- Both ESC key and exit button should work reliably
- Either method exits fullscreen cleanly without temporary exit/re-enter bug
- Comprehensive fix for all exit paths

### Testing approach
- Verify fixes once and document in code
- No automated testing framework for these UI fixes
- Document the fix clearly in code comments
- Manual verification is sufficient for these straightforward bugs

### Claude's Discretion
- Exact D3.js API patterns and implementation details
- Specific color palette for charts (maintaining Neon/Matrix theme)
- Detail view modal layout and styling
- Tooltip positioning and animation
- Filter/search UI component styling
- Chart responsiveness and mobile handling
- Validation warning badge styling (VAL-04)
- Diagnostics modal layout (VAL-05)

</decisions>

<specifics>
## Specific Ideas

- Maintain consistency with existing Neon/Matrix theme throughout new visualizations
- The click-to-highlight between scatter plot and table should feel connected - visual feedback on both elements
- Heatmap detail view should feel like a focused modal overlay, not a separate page
- Toggle controls for log/linear should be intuitive - consider standard chart UI patterns
- Search/filter should be immediate (no search button needed - filter as you type)

</specifics>

<deferred>
## Deferred Ideas

- Data integrity issue: "undefined solver" metric file - noted in STATE.md, defer to separate phase/todo (out of scope for visualization work)

</deferred>

---

*Phase: 06-visualization-and-ui*
*Context gathered: 2026-01-24*
