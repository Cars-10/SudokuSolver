# Phase 6: Visualization & UI - Research

**Researched:** 2026-01-24
**Domain:** D3.js data visualization, interactive charts, UI bug fixes
**Confidence:** HIGH

## Summary

This phase adds advanced D3.js visualizations (scatter plot, heatmap, histogram) and fixes existing UI bugs (algorithm dropdown sorting, Matrix Race fullscreen exit). The research confirms D3.js v7.9.0 is the current version, already included in the project via CDN. Key findings: D3.js handles 70+ data points efficiently (modest dataset), logarithmic scales are essential for 6 orders of magnitude variation, cross-highlighting between charts requires careful state management, and the Fullscreen API `fullscreenchange` event is the reliable way to detect ESC key exits.

**Primary recommendation:** Use D3.js v7 (already loaded) with modular chart functions following enter-update-exit pattern, implement logarithmic scales with d3.scaleLog for scatter plots, use d3-scale-chromatic for color palettes compatible with neon theme, and fix fullscreen bug by properly handling fullscreenchange events without triggering chart redraws that cause exit/re-enter loops.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| D3.js | 7.9.0 | Data visualization, DOM manipulation, scales, axes | Industry standard for custom interactive visualizations, complete control over SVG rendering |
| d3-scale | (included in D3 v7) | Linear, logarithmic, categorical scales | Built-in D3 module for mapping data domains to visual ranges |
| d3-scale-chromatic | (included in D3 v7) | Color palettes (sequential, diverging, categorical) | ColorBrewer-derived palettes designed for data visualization accessibility |
| Fullscreen API | Native browser API | Fullscreen mode management | Standard Web API for fullscreen interactions |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| d3-brush | (included in D3 v7) | Interactive selection/filtering | For linked brushing across multiple charts (future enhancement) |
| d3-array | (included in D3 v7) | Statistical functions (mean, median, quantile) | For histogram percentile calculations and outlier detection |
| d3-format | (included in D3 v7) | Number formatting | For hybrid axis labels (human units vs scientific notation) |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| D3.js | Observable Plot | Plot simpler (histogram in 1 line vs 50), but lacks low-level control for custom neon theme and cross-highlighting interactions |
| SVG rendering | Canvas rendering | Canvas 10x faster for large datasets, but 70+ points is modest - SVG better for tooltips, hover states, and DOM manipulation |
| Manual sorting | Array.prototype.sort() | Both work, but localeCompare() provides better internationalization |

**Installation:**
```html
<!-- Already included in project -->
<script src="https://d3js.org/d3.v7.min.js"></script>
```

## Architecture Patterns

### Recommended Project Structure
Visualizations are embedded in `index.html` within D3 closure, following existing pattern:
```
index.html
├── <script> D3 v7 CDN import
├── <style> Neon/Matrix theme CSS (--primary: #00ff9d, --secondary: #00b8ff)
└── <script> D3 closure
    ├── function drawScatterPlot()        # Time vs Memory scatter
    ├── function drawHeatmap()             # Language x Matrix heatmap
    ├── function drawHistogram()           # Score distribution histogram
    ├── function drawValidationBadges()    # VAL-04 warning UI
    └── function drawDiagnosticsModal()    # VAL-05 iteration details
```

### Pattern 1: D3 Enter-Update-Exit for Chart Redrawing
**What:** Efficient DOM updates using D3's data join pattern
**When to use:** All chart redraw operations (window resize, data filter, scale toggle)
**Example:**
```javascript
// Source: D3.js official documentation + existing project pattern
function drawScatterPlot(data, useLogScale) {
    const svg = d3.select("#chart-container").select("svg");

    // Enter-Update-Exit pattern
    const circles = svg.selectAll("circle")
        .data(data, d => d.solver); // Key function for object constancy

    // EXIT: Remove old elements
    circles.exit().remove();

    // ENTER: Create new elements
    const enter = circles.enter().append("circle")
        .attr("r", 5)
        .style("fill", d => isOutlier(d) ? "#ff4444" : "#00ff9d");

    // UPDATE: Merge enter + update selections
    enter.merge(circles)
        .transition().duration(750)
        .attr("cx", d => xScale(d.time))
        .attr("cy", d => yScale(d.memory));
}
```

### Pattern 2: Logarithmic Scale with Hybrid Axis Formatting
**What:** d3.scaleLog() for 6 orders of magnitude + custom tick formatting
**When to use:** Scatter plot axes, any visualization with exponential data ranges
**Example:**
```javascript
// Source: https://d3js.org/d3-scale/log
const xScale = d3.scaleLog()
    .domain([0.0001, 1000]) // 7 orders of magnitude
    .range([0, chartWidth])
    .base(10)
    .clamp(true); // Prevent values outside domain

// Custom hybrid formatting (human units → scientific notation)
const xAxis = d3.axisBottom(xScale)
    .tickFormat(d => {
        if (d < 0.001) return d.toExponential(1);
        if (d < 1) return `${(d * 1000).toFixed(0)}ms`;
        if (d < 1000) return `${d.toFixed(2)}s`;
        return d.toExponential(1);
    });
```

### Pattern 3: Cross-Highlighting Between Chart and Table
**What:** Click scatter point → highlight table row, and vice versa
**When to use:** Scatter plot click interactions
**Example:**
```javascript
// Source: Existing project pattern + https://medium.com/@pbesh/linked-highlighting-with-react-d3-js-and-reflux-16e9c0b2210b
circles.on("click", (event, d) => {
    // Highlight clicked circle
    d3.selectAll("circle").classed("highlighted", false);
    d3.select(event.currentTarget).classed("highlighted", true);

    // Highlight corresponding table row (existing global function)
    const row = document.querySelector(`tr[data-lang="${d.solver}"]`);
    if (row) {
        // Remove existing highlights
        document.querySelectorAll('.active-row').forEach(r =>
            r.classList.remove('active-row'));
        // Add highlight to clicked row
        row.classList.add('active-row');
        row.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
});
```

### Pattern 4: Tooltip Edge Detection (Prevent Viewport Overflow)
**What:** Dynamic tooltip positioning that flips when near viewport edges
**When to use:** All tooltips in scatter plot, heatmap, histogram
**Example:**
```javascript
// Source: https://gist.github.com/GerHobbelt/2505393
circles.on("mousemove", (event, d) => {
    const tooltip = d3.select("#tooltip");
    const tooltipNode = tooltip.node();
    const tooltipWidth = tooltipNode.offsetWidth;
    const tooltipHeight = tooltipNode.offsetHeight;

    // Check right edge
    let left = event.pageX + 15;
    if (left + tooltipWidth > window.innerWidth) {
        left = event.pageX - tooltipWidth - 15;
    }

    // Check bottom edge
    let top = event.pageY + 15;
    if (top + tooltipHeight > window.innerHeight) {
        top = event.pageY - tooltipHeight - 15;
    }

    tooltip
        .style("left", left + "px")
        .style("top", top + "px")
        .style("display", "block")
        .html(`<strong style="color:#00ff9d">${d.solver}</strong><br>
               Time: ${d.time.toFixed(4)}s<br>
               Memory: ${d.memory.toFixed(2)}MB`);
});
```

### Pattern 5: Responsive SVG with viewBox
**What:** SVG scales with container using viewBox instead of fixed dimensions
**When to use:** All new chart functions (scatter, heatmap, histogram)
**Example:**
```javascript
// Source: https://d3-graph-gallery.com/graph/custom_responsive.html
const container = document.getElementById('chart-container');
const width = container.clientWidth;
const height = container.clientHeight;

const svg = d3.select("#chart-container")
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMidYMid meet");

// Redraw on window resize
window.addEventListener("resize", () => {
    const newWidth = container.clientWidth;
    const newHeight = container.clientHeight;
    svg.attr("viewBox", `0 0 ${newWidth} ${newHeight}`);
    drawScatterPlot(); // Redraw with new dimensions
});
```

### Anti-Patterns to Avoid
- **Monolithic draw functions:** Don't put all chart logic in one 500+ line function - modularize scales, axes, tooltips, interactions
- **Brittle selections:** Don't call `.enter()`, `.exit()`, `.transition()` multiple times on same selection - store selection, operate once
- **Unnecessary recalculation:** Don't recreate scales/color functions on every mousemove - define once, reuse
- **Blocking event listeners:** Don't add window resize listener inside draw function - listener persists, creates memory leaks
- **Ignoring fullscreenchange timing:** Don't trigger chart redraws inside fullscreenchange handler that also exits fullscreen - creates exit/re-enter loop

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Logarithmic axis ticks | Custom log tick calculation | `d3.scaleLog()` with `.ticks()` | D3 handles edge cases (zero values, negative domains, tick spacing for readability) |
| Color palettes | Manual hex color arrays | `d3.scaleOrdinal(d3.schemeCategory10)` or d3-scale-chromatic | ColorBrewer palettes tested for colorblind accessibility and grayscale conversion |
| Number formatting | String concatenation + toFixed() | `d3.format()` with SI prefixes | Handles edge cases (negative zero, Infinity, locale-specific separators) |
| Percentile calculation | Manual array sorting + index math | `d3.quantile(data, 0.25)` | Handles ties, empty arrays, NaN values correctly |
| Tooltip positioning | Manual boundary checks | Established edge detection pattern (see Pattern 4) | Handles all 4 edges, scroll offsets, nested containers |
| Outlier detection | Custom threshold logic | Phase 5 `identifyOutliers()` IQR method | Already implemented, tested, consistent with Phase 5 scoring analysis |
| Fullscreen detection | Keydown listener for ESC | `fullscreenchange` event + `document.fullscreenElement` check | ESC key events blocked by browser in fullscreen mode for security - only fullscreenchange is reliable |

**Key insight:** D3.js provides battle-tested solutions for most visualization math (scales, axes, statistics). Custom implementations miss edge cases and reduce maintainability. The project already uses D3 v7 - leverage its full module suite.

## Common Pitfalls

### Pitfall 1: Logarithmic Scale with Zero or Negative Values
**What goes wrong:** `d3.scaleLog()` returns `NaN` or `-Infinity` for zero/negative values, breaking chart rendering
**Why it happens:** log(0) = -∞ mathematically, D3 can't map to pixel coordinates
**How to avoid:**
- Filter out zero/negative values before creating scale domain
- OR use `.clamp(true)` to constrain values to domain min/max
- OR add small epsilon (0.0001) to all values as floor
**Warning signs:** Console errors "NaN" in SVG attributes, circles/points not rendering

### Pitfall 2: Fullscreen Exit/Re-enter Loop
**What goes wrong:** Exiting fullscreen triggers chart redraw, which re-enters fullscreen, creating infinite loop
**Why it happens:** `fullscreenchange` event fires on both enter AND exit, redraw logic calls `toggleChartFullscreen()` unconditionally
**How to avoid:**
- Check `document.fullscreenElement` state inside fullscreenchange handler
- Only redraw if exiting fullscreen (element === null)
- Avoid calling fullscreen API methods inside fullscreenchange handler
**Warning signs:** Fullscreen flickers, multiple rapid fullscreen transitions, browser becomes unresponsive

**Example fix:**
```javascript
// WRONG - causes loop
document.addEventListener('fullscreenchange', () => {
    window.switchChart(currentChart); // This may trigger fullscreen again
});

// CORRECT - check state first
document.addEventListener('fullscreenchange', () => {
    if (!document.fullscreenElement) {
        // Only redraw when EXITING fullscreen
        setTimeout(() => window.switchChart(currentChart), 100);
    }
    // Do nothing when ENTERING fullscreen (already drawn)
});
```

### Pitfall 3: Tooltip Overflow on Chart Edges
**What goes wrong:** Tooltips positioned near viewport edges get cut off or trigger scrollbars
**Why it happens:** Fixed offset (event.pageX + 15) doesn't check if tooltip extends beyond viewport
**How to avoid:** Use edge detection pattern (Pattern 4) that checks tooltip dimensions against viewport bounds
**Warning signs:** Tooltips partially hidden, horizontal/vertical scrollbars appear on hover

### Pitfall 4: Select Dropdown Not Updating Label
**What goes wrong:** Dropdown value changes but visible label still shows old option
**Why it happens:** Label is separate element from <select>, needs manual sync on change event
**How to avoid:** Update both dropdown value AND associated label in change handler
**Warning signs:** Dropdown shows "BruteForce" but selection is actually "DLX"

**Current bug in project:**
```javascript
// Algorithm dropdown at line ~1413-1419 in index.html
<select id="chart-selector" onchange="switchChart(this.value)">
    <option value="algorithm">Algorithm Comparison</option>
    <!-- ... other options ... -->
</select>
```
The dropdown itself works, but if there's a separate label element displaying the selection, it's not updated. Need to verify if label element exists and update it.

### Pitfall 5: Heatmap Cell Click Not Navigating
**What goes wrong:** Click handler fires but modal/detail view doesn't open
**Why it happens:** Event propagation blocked by parent elements, or z-index prevents click from reaching cell
**How to avoid:**
- Use `event.stopPropagation()` to prevent parent handlers from canceling
- Ensure heatmap cells have higher z-index than gridlines/axes
- Test click handler fires with `console.log` before building modal
**Warning signs:** No console errors but click does nothing, cursor changes to pointer but no action

### Pitfall 6: Color Accessibility with Neon Palette
**What goes wrong:** Bright neon text on pure black causes eye strain, some colors indistinguishable to colorblind users
**Why it happens:** High contrast neon colors overwhelming in large areas, certain hue combinations (red/green) problematic for deuteranopia
**How to avoid:**
- Use neon colors for accents/highlights only, not large fill areas
- Background should be dark grey (#0d0d12) not pure black (#000)
- Use d3-scale-chromatic schemes (ColorBrewer) tested for colorblind accessibility
- Differentiate categories with marker shapes AND colors (circles vs squares)
**Warning signs:** User feedback about eye strain, colors look same in browser colorblind simulation

### Pitfall 7: 70+ Languages Overwhelming Scatter Plot
**What goes wrong:** Scatter plot becomes cluttered, overlapping points, unreadable labels
**Why it happens:** 70 languages × 6 matrices = 420 data points in small chart area
**How to avoid:**
- Aggregate by language (one point per language showing average or best result)
- Implement filter/search to show subset of languages
- Use clustering/binning for very dense regions
- Make labels appear only on hover, not permanently
**Warning signs:** Chart looks like solid mass of dots, can't distinguish individual points, labels overlap completely

## Code Examples

Verified patterns from official sources:

### Logarithmic Scale Toggle (Linear ↔ Log)
```javascript
// Source: https://d3js.org/d3-scale/log + project CONTEXT decisions
let useLogScale = true; // State variable

function drawScatterPlot(data) {
    // Toggle scale based on state
    const xScale = useLogScale
        ? d3.scaleLog().domain([0.0001, 1000]).range([0, chartWidth]).base(10)
        : d3.scaleLinear().domain([0, 1000]).range([0, chartWidth]);

    // Same for Y scale
    const yScale = useLogScale
        ? d3.scaleLog().domain([1, 10000]).range([chartHeight, 0]).base(10)
        : d3.scaleLinear().domain([0, 10000]).range([chartHeight, 0]);

    // Update button UI to show current state
    d3.select("#log-scale-toggle")
        .text(useLogScale ? "Switch to Linear" : "Switch to Logarithmic")
        .on("click", () => {
            useLogScale = !useLogScale;
            drawScatterPlot(data); // Redraw with new scale
        });
}
```

### Heatmap with Click Navigation
```javascript
// Source: https://d3-graph-gallery.com/graph/heatmap_basic.html + project CONTEXT
function drawHeatmap(data) {
    const languages = [...new Set(data.map(d => d.solver))];
    const matrices = [1, 2, 3, 4, 5, 6];

    const xScale = d3.scaleBand()
        .domain(matrices)
        .range([0, chartWidth])
        .padding(0.05);

    const yScale = d3.scaleBand()
        .domain(languages)
        .range([0, chartHeight])
        .padding(0.05);

    // Color scale for performance (faster = greener)
    const colorScale = d3.scaleSequential(d3.interpolateViridis)
        .domain([d3.max(data, d => d.time), 0]); // Reverse: lower time = brighter

    svg.selectAll("rect")
        .data(data)
        .enter().append("rect")
        .attr("x", d => xScale(d.matrix))
        .attr("y", d => yScale(d.solver))
        .attr("width", xScale.bandwidth())
        .attr("height", yScale.bandwidth())
        .style("fill", d => colorScale(d.time))
        .style("stroke", "#2a2a35")
        .on("click", (event, d) => {
            // Open detail modal with full metrics for this language+matrix
            showDetailModal(d.solver, d.matrix, d);
        })
        .on("mouseover", (event, d) => {
            d3.select(event.currentTarget).style("stroke", "#00ff9d").style("stroke-width", 2);
            showTooltip(event, d);
        })
        .on("mouseout", (event, d) => {
            d3.select(event.currentTarget).style("stroke", "#2a2a35").style("stroke-width", 1);
            hideTooltip();
        });
}
```

### Histogram with Percentile Markers
```javascript
// Source: https://d3-graph-gallery.com/graph/histogram_basic.html + d3-array docs
function drawHistogram(scores) {
    // Calculate percentiles using d3-array
    const q1 = d3.quantile(scores, 0.25);
    const median = d3.quantile(scores, 0.5);
    const q3 = d3.quantile(scores, 0.75);
    const mean = d3.mean(scores);

    // Create bins
    const histogram = d3.histogram()
        .domain(xScale.domain())
        .thresholds(20); // 20 bins

    const bins = histogram(scores);

    // Draw bars
    svg.selectAll("rect")
        .data(bins)
        .enter().append("rect")
        .attr("x", d => xScale(d.x0))
        .attr("y", d => yScale(d.length))
        .attr("width", d => xScale(d.x1) - xScale(d.x0) - 1)
        .attr("height", d => chartHeight - yScale(d.length))
        .style("fill", "#00ff9d")
        .style("opacity", 0.7);

    // Draw percentile lines
    const percentiles = [
        { value: q1, label: "Q1 (25%)", color: "#00b8ff" },
        { value: median, label: "Median (50%)", color: "#00ff9d" },
        { value: q3, label: "Q3 (75%)", color: "#00b8ff" },
        { value: mean, label: "Mean", color: "#ff9d00", dash: "5,5" }
    ];

    percentiles.forEach(p => {
        svg.append("line")
            .attr("x1", xScale(p.value))
            .attr("x2", xScale(p.value))
            .attr("y1", 0)
            .attr("y2", chartHeight)
            .style("stroke", p.color)
            .style("stroke-width", 2)
            .style("stroke-dasharray", p.dash || "none");

        svg.append("text")
            .attr("x", xScale(p.value) + 5)
            .attr("y", 20)
            .style("fill", p.color)
            .style("font-family", "JetBrains Mono")
            .style("font-size", "10px")
            .text(p.label);
    });
}
```

### Algorithm Dropdown Alphabetical Sort
```javascript
// Source: https://www.geeksforgeeks.org/jquery/how-to-sort-option-elements-alphabetically-using-jquery/
// Fix for UI-02: Algorithm dropdown label not updating
const selector = document.getElementById('chart-selector');

// Sort options alphabetically by display text
const options = Array.from(selector.querySelectorAll('option'));
options.sort((a, b) => a.text.localeCompare(b.text));
selector.innerHTML = '';
options.forEach(option => selector.appendChild(option));

// Update label when selection changes
selector.addEventListener('change', (event) => {
    const selectedOption = event.target.options[event.target.selectedIndex];
    const label = document.getElementById('chart-label'); // If label element exists
    if (label) {
        label.textContent = selectedOption.text;
    }
});
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| D3 v3 category20 colors | d3-scale-chromatic (ColorBrewer) | D3 v5 (2017) | Better accessibility, removed default schemes to force intentional color choices |
| Fixed width/height SVG | viewBox + preserveAspectRatio | D3 v4+ best practice | Responsive charts work across devices without media queries |
| Global d3.event | Event passed directly to listeners | D3 v6 (2020) | Aligns with modern JS standards, prevents event conflicts |
| Monolithic chart functions | Reusable chart pattern | Ongoing best practice | Modular, testable, updatable charts |
| Manual tooltip positioning | Edge-detection patterns | Community standard (2015+) | Prevents overflow, better UX |

**Deprecated/outdated:**
- `d3.scale.category20()`: Removed in v5, use `d3.scaleOrdinal(d3.schemeCategory10)` or other chromatic schemes
- `d3.event`: Use event parameter in listener callback instead
- `d3.scale.linear()`: Now `d3.scaleLinear()` (camelCase, no dot between scale and type)

## Open Questions

Things that couldn't be fully resolved:

1. **Matrix Race fullscreen exit bug root cause**
   - What we know: UI-03 requires fixing fullscreen exit without temporary exit/re-enter bug
   - What's unclear: Current code at line ~36569-36577 already checks `!document.fullscreenElement` before redrawing - is the bug in a different fullscreen handler? Is Matrix Race using custom fullscreen logic separate from toggleChartFullscreen()?
   - Recommendation: Audit ALL fullscreenchange event listeners and Matrix Race-specific fullscreen code (around line 34895-34902 shows Matrix Race auto-enters fullscreen). Likely culprit: Multiple competing fullscreenchange handlers, or redraw triggering new fullscreen request.

2. **Algorithm dropdown label element existence**
   - What we know: UI-02 requires label updates when selection changes
   - What's unclear: Is there a separate label element (e.g., `<span id="chart-label">`) that displays the selected chart type, or is the bug that the dropdown itself doesn't reflect the active chart?
   - Recommendation: Inspect HTML around line 1413 for associated label elements. If no label exists, bug may be that `selector.value` not synced when programmatically calling `switchChart()`.

3. **Optimal aggregation strategy for scatter plot**
   - What we know: 70+ languages could create 420 data points (70 langs × 6 matrices), possibly overwhelming scatter plot
   - What's unclear: Should scatter show one point per language (aggregate across matrices), one point per language-matrix pair (filterable), or only show "best" result per language?
   - Recommendation: Default to one point per language showing Matrix 1 results (simplest puzzle, most comparable). Provide matrix selector dropdown to switch which matrix is visualized. Avoids overload while preserving detailed drill-down capability.

4. **Validation badge styling integration**
   - What we know: VAL-04 requires visual warning badges for invalid implementations
   - What's unclear: Should badges be inline in table cells, overlay on chart points, or separate panel? How to maintain neon theme while indicating "error" state?
   - Recommendation: Use existing status badge pattern (line ~34600 shows status-ready class). Add `status-invalid` class with orange/red neon color (#ff9d00 or #ff4444). Position in language name column as existing badges do.

## Sources

### Primary (HIGH confidence)
- [D3.js Official Documentation](https://d3js.org/) - v7.9.0 current version, getting started guide
- [D3.js Logarithmic Scales](https://d3js.org/d3-scale/log) - Official scale documentation
- [D3.js Color Schemes](https://observablehq.com/@d3/color-schemes) - Official color palette reference
- [MDN Fullscreen API](https://developer.mozilla.org/en-US/docs/Web/API/Fullscreen_API) - Browser fullscreen standard
- [MDN fullscreenchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenchange_event) - Event handling specification
- Project codebase (`index.html`, line 1015) - Confirms D3 v7 already loaded via CDN

### Secondary (MEDIUM confidence)
- [D3 Graph Gallery - Scatter Plot with Tooltip](https://d3-graph-gallery.com/graph/scatter_tooltip.html) - Verified pattern examples
- [D3 Graph Gallery - Heatmap](https://d3-graph-gallery.com/heatmap.html) - Basic heatmap implementation
- [D3 Graph Gallery - Histogram](https://d3-graph-gallery.com/graph/histogram_basic.html) - Histogram with binning
- [D3 Graph Gallery - Responsive Charts](https://d3-graph-gallery.com/graph/custom_responsive.html) - viewBox pattern
- [Observable: Linked Brushing](https://observablehq.com/blog/linked-brushing) - Jan 2025 article, cross-chart interaction
- [Reintech: Optimizing D3 for Large Datasets](https://reintech.io/blog/optimizing-d3-chart-performance-large-data) - Performance patterns
- [Sling Academy: ESC Key Fullscreen Exit](https://www.slingacademy.com/article/javascript-press-esc-to-exit-fullscreen-mode-2-examples/) - JavaScript examples
- [GeeksforGeeks: Sort Dropdown Alphabetically](https://www.geeksforgeeks.org/jquery/how-to-sort-option-elements-alphabetically-using-jquery/) - Dropdown sorting pattern

### Tertiary (LOW confidence - flagged for validation)
- [Medium: Linked Highlighting with React, D3, Reflux](https://medium.com/@pbesh/linked-highlighting-with-react-d3-js-and-reflux-16e9c0b2210b) - Pattern concepts transferable, but uses React (project uses vanilla JS)
- [Analytics Vidhya: Cyberpunk Themed Charts](https://www.analyticsvidhya.com/blog/2021/07/cyberpunk-themed-charts-advanced-data-visualization-in-python/) - Python-based, concepts transferable
- [Medium: Cyberpunk Style with Matplotlib](https://medium.com/data-science/cyberpunk-style-with-matplotlib-f47404c9d4c5) - Neon glow technique (redraw with low alpha), but Python library

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - D3 v7.9.0 confirmed in project, official docs authoritative
- Architecture: HIGH - D3 Graph Gallery examples verified, project codebase inspection confirms patterns
- Pitfalls: HIGH - MDN Fullscreen API spec definitive, observed bug patterns documented
- Neon color palette: MEDIUM - Cyberpunk color research from multiple sources, but accessibility testing needed
- UI bugs root cause: MEDIUM - Can see symptoms in code, but need to audit full HTML for definitive fix locations

**Research date:** 2026-01-24
**Valid until:** 30 days (2026-02-23) - D3.js v7 stable, visualization patterns evergreen, but check for v8 announcements
