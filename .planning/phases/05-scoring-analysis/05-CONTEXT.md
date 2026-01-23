# Phase 5: Scoring Analysis - Context

**Gathered:** 2026-01-23
**Status:** Ready for planning

<domain>
## Phase Boundary

Provide statistical rigor through sensitivity analysis, correlation computation, and versioned scoring to validate current methodology and enable informed future improvements. This phase analyzes validated performance data (from Phase 4) across different scoring weight configurations and provides transparency into composite score calculation. Scope is limited to analysis and visualization of existing data—no changes to scoring formula itself.

</domain>

<decisions>
## Implementation Decisions

### Sensitivity Analysis Presentation
- **Weight scenarios**: Core 4 scenarios (Time-only 100/0, Current 80/20, Balanced 50/50, Memory-only 0/100)
- **Presentation format**: Interactive comparison table with sortable columns
- **Integration**: Expandable detail in main rankings table (not separate section)
- **Detail content**: When language row expanded, show rank positions across 4 scenarios plus "Max rank swing: X positions" metric
- **Interaction pattern**: Click language row → expands to show sensitivity data inline

### Score Decomposition Visualization
- **Location**: In the main rankings table, adjacent to composite score
- **Visualization type**: Mini stacked bar chart (horizontal bar with 80% blue for time + 20% orange for memory)
- **Bar representation**: Normalized contributions (80/20 visual split, not raw scores)
- **Interactivity**: Hover over stacked bar reveals tooltip with exact breakdown: "Time: 68.2 (80%), Memory: 17.0 (20%), Total: 85.2"
- **Purpose**: Make the 80/20 weighting transparent and visible at a glance

### Rank Stability Metrics
- **Metric**: Max rank swing (difference between best and worst rank across 4 weight scenarios)
- **Highlights**: Both extremes—most stable (top 5) AND most unstable (top 5) languages
- **Location**: Summary insight section ("Rank Stability Analysis") separate from main table
- **Format**: Two lists with language names and their rank swing values

### Statistical Analysis Depth
- **Correlation analysis**: R² correlation coefficient + plain-English interpretation (e.g., "R² = 0.23 indicates weak correlation between time and memory performance")
- **Outlier detection**: Yes, detect and flag statistical outliers
- **Outlier presentation**: Summary insights section ("Statistical Outliers") with flagged languages and brief explanation—no badges in main table
- **Analysis tone**: Accessible to non-statisticians while maintaining technical accuracy

### Claude's Discretion
- Outlier detection method (IQR, Z-score, or percentile-based)—choose based on statistical rigor and data distribution
- Significance threshold for rank stability highlights—choose based on data patterns (suggested: adaptive threshold)
- Color palette for stacked bars and sensitivity indicators
- Exact tooltip styling and animation timing
- Section ordering in the report

</decisions>

<specifics>
## Specific Ideas

**Interactive patterns:**
- Expandable rows should use smooth animation (not jarring instant expand)
- Stacked bar tooltips should appear on hover without delay

**Visual hierarchy:**
- Summary insights sections provide high-level findings before users dive into detailed tables
- Main table remains clean—no visual clutter from badges or excessive columns

**Interpretation focus:**
- Every metric should be accompanied by plain-English explanation
- Statistical rigor maintained but presented accessibly

</specifics>

<deferred>
## Deferred Ideas

None—discussion stayed within phase scope. Phase boundary explicitly excludes changes to scoring formula itself (that would be a separate phase if needed).

</deferred>

---

*Phase: 05-scoring-analysis*
*Context gathered: 2026-01-23*
