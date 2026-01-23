# Phase 5: Scoring Analysis - Research

**Researched:** 2026-01-23
**Domain:** Statistical analysis and data visualization for benchmark scoring
**Confidence:** HIGH

## Summary

Phase 5 adds statistical rigor to the benchmark report through sensitivity analysis, correlation analysis, and score decomposition visualization. The phase analyzes how rankings change across different time/memory weight scenarios (100/0, 80/20, 50/50, 0/100), computes correlation between time and memory performance, and identifies statistical outliers. This research confirms that robust solutions exist in the JavaScript/TypeScript ecosystem for all required statistical computations, with simple-statistics being the standard library for correlation and percentile analysis, and well-established IQR and Z-score methods available for outlier detection. Visualization will use native HTML/CSS for stacked bars and expandable table rows, avoiding heavyweight charting library dependencies.

The current scoring implementation already uses weighted geometric mean (scoring.ts), which is mathematically sound for normalizing benchmark ratios. This phase extends the analysis without changing the formula itself.

**Primary recommendation:** Use simple-statistics for correlation/percentile calculations, implement IQR-based outlier detection (more robust for skewed distributions), and build interactive UI components with native HTML/CSS/JavaScript rather than external charting libraries.

## Standard Stack

The established libraries/tools for statistical analysis in JavaScript/TypeScript:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| simple-statistics | 7.8.8 | Statistical calculations (correlation, R², percentiles, IQR, zScore) | Most popular JS stats library (3.5k stars), zero dependencies, TypeScript definitions included, exhaustively documented |
| Native HTML/CSS/JS | ES2020+ | Interactive UI (expandable rows, stacked bars, tooltips) | Zero dependencies, full control, no bundle bloat, works with existing codebase |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| TypeScript | 5.x | Type safety for statistical computations | Already in codebase, prevents calculation errors |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| simple-statistics | jStat | jStat has more features but heavier (60KB vs 20KB), less maintained (last update 2020) |
| simple-statistics | statistics.js | Less popular, less maintained, missing TypeScript definitions |
| Chart.js/D3.js | Native HTML/CSS | Heavy dependencies (200KB+) overkill for simple stacked bars; native gives full control and matches existing report style |

**Installation:**
```bash
npm install simple-statistics
npm install --save-dev @types/simple-statistics  # TypeScript definitions (included in 7.8.8+)
```

## Architecture Patterns

### Recommended Code Structure
```
Metrics/
├── scoring.ts                    # Existing: calculateOverallScore with weights
├── scoring-analysis.ts           # NEW: Sensitivity analysis functions
│   ├── calculateSensitivityScores()    # Compute scores across 4 weight scenarios
│   ├── calculateRankStability()        # Max rank swing per language
│   └── identifyOutliers()              # IQR-based outlier detection
├── statistical-analysis.ts       # NEW: Correlation and percentile functions
│   ├── computeCorrelation()            # R² between time and memory
│   ├── calculatePercentiles()          # p50, p90, p99 calculations
│   └── interpretCorrelation()          # Plain-English R² interpretation
└── HTMLGenerator.ts              # MODIFY: Add new sections for analysis
    ├── generateSensitivityTable()      # Expandable rows in main table
    ├── generateScoreDecomposition()    # Stacked bar mini-charts
    ├── generateRankStabilitySection()  # Summary insights
    └── generateOutliersSection()       # Flagged outliers with explanation
```

### Pattern 1: Sensitivity Analysis Loop
**What:** Calculate scores across multiple weight scenarios without duplicating logic
**When to use:** Computing time-only, 80/20, 50/50, memory-only rankings
**Example:**
```typescript
// Source: Based on existing scoring.ts + sensitivity analysis best practices
import { calculateOverallScore } from './scoring.ts';
import type { SolverMetrics } from './types.ts';

interface WeightScenario {
    name: string;
    weights: { time: number; memory: number };
}

const scenarios: WeightScenario[] = [
    { name: 'Time Only', weights: { time: 1.0, memory: 0.0 } },
    { name: 'Current (80/20)', weights: { time: 0.8, memory: 0.2 } },
    { name: 'Balanced (50/50)', weights: { time: 0.5, memory: 0.5 } },
    { name: 'Memory Only', weights: { time: 0.0, memory: 1.0 } }
];

export function calculateSensitivityScores(
    metrics: SolverMetrics[],
    cMetrics: SolverMetrics
): Map<string, Array<{ scenario: string; score: number; rank: number }>> {
    const resultsByLanguage = new Map<string, Array<{ scenario: string; score: number; rank: number }>>();

    scenarios.forEach(scenario => {
        // Calculate scores for all languages under this scenario
        const languageScores = metrics.map(m => ({
            language: m.solver,
            score: calculateOverallScore(m.results, cMetrics.results, scenario.weights)
        }));

        // Sort and assign ranks (lower score = better rank)
        languageScores.sort((a, b) => a.score - b.score);
        languageScores.forEach((lang, index) => {
            const rank = index + 1;
            if (!resultsByLanguage.has(lang.language)) {
                resultsByLanguage.set(lang.language, []);
            }
            resultsByLanguage.get(lang.language)!.push({
                scenario: scenario.name,
                score: lang.score,
                rank
            });
        });
    });

    return resultsByLanguage;
}

export function calculateRankStability(
    sensitivityResults: Map<string, Array<{ scenario: string; score: number; rank: number }>>
): Array<{ language: string; maxSwing: number; bestRank: number; worstRank: number }> {
    const stability = [];

    for (const [language, scenarios] of sensitivityResults.entries()) {
        const ranks = scenarios.map(s => s.rank);
        const bestRank = Math.min(...ranks);
        const worstRank = Math.max(...ranks);
        const maxSwing = worstRank - bestRank;

        stability.push({ language, maxSwing, bestRank, worstRank });
    }

    // Sort by max swing (descending) to find most unstable
    stability.sort((a, b) => b.maxSwing - a.maxSwing);

    return stability;
}
```

### Pattern 2: Correlation Analysis with Interpretation
**What:** Compute R² and provide plain-English interpretation for non-statisticians
**When to use:** Analyzing relationship between time and memory performance
**Example:**
```typescript
// Source: simple-statistics documentation + statistical best practices
import { sampleCorrelation, rSquared, linearRegression, linearRegressionLine } from 'simple-statistics';
import type { SolverMetrics } from './types.ts';

export function computeCorrelation(metrics: SolverMetrics[]): {
    rValue: number;
    rSquared: number;
    interpretation: string;
} {
    // Extract time and memory data points
    const timeData: number[] = [];
    const memoryData: number[] = [];

    metrics.forEach(m => {
        const totalTime = m.results.reduce((acc, r) => acc + r.time, 0);
        const totalMemory = m.results.reduce((acc, r) => acc + r.memory, 0);
        timeData.push(totalTime);
        memoryData.push(totalMemory);
    });

    // Calculate Pearson correlation coefficient
    const rValue = sampleCorrelation(timeData, memoryData);

    // Calculate R² (coefficient of determination)
    const samples = timeData.map((t, i) => [t, memoryData[i]]);
    const regression = linearRegression(samples);
    const regressionLine = linearRegressionLine(regression);
    const r2 = rSquared(samples, regressionLine);

    // Interpret R² value
    const interpretation = interpretCorrelation(r2);

    return { rValue, rSquared: r2, interpretation };
}

export function interpretCorrelation(r2: number): string {
    // R² ranges from 0 to 1, indicating proportion of variance explained
    if (r2 >= 0.8) {
        return `R² = ${r2.toFixed(2)} indicates a very strong correlation between time and memory performance. Languages that are fast tend to be memory-efficient.`;
    } else if (r2 >= 0.5) {
        return `R² = ${r2.toFixed(2)} indicates a moderate correlation between time and memory performance. There's a noticeable relationship, but other factors also influence memory usage.`;
    } else if (r2 >= 0.3) {
        return `R² = ${r2.toFixed(2)} indicates a weak correlation between time and memory performance. Time and memory are somewhat independent characteristics.`;
    } else {
        return `R² = ${r2.toFixed(2)} indicates little to no correlation between time and memory performance. Being fast doesn't predict memory efficiency in this benchmark suite.`;
    }
}
```

### Pattern 3: IQR-Based Outlier Detection
**What:** Identify statistical outliers using robust IQR method (preferred over Z-score for skewed data)
**When to use:** Flagging languages with unusual performance characteristics
**Example:**
```typescript
// Source: simple-statistics + IQR outlier detection methodology
import { interquartileRange, quantile } from 'simple-statistics';
import type { SolverMetrics } from './types.ts';

export interface OutlierAnalysis {
    language: string;
    metric: 'time' | 'memory';
    value: number;
    threshold: number;
    direction: 'high' | 'low';
    explanation: string;
}

export function identifyOutliers(metrics: SolverMetrics[]): OutlierAnalysis[] {
    const outliers: OutlierAnalysis[] = [];

    // Extract time and memory arrays
    const times = metrics.map(m => m.results.reduce((acc, r) => acc + r.time, 0));
    const memories = metrics.map(m => m.results.reduce((acc, r) => acc + r.memory, 0));

    // IQR outlier detection for time
    const timeOutliers = detectOutliersIQR(times, metrics, 'time');
    outliers.push(...timeOutliers);

    // IQR outlier detection for memory
    const memoryOutliers = detectOutliersIQR(memories, metrics, 'memory');
    outliers.push(...memoryOutliers);

    return outliers;
}

function detectOutliersIQR(
    data: number[],
    metrics: SolverMetrics[],
    metricType: 'time' | 'memory'
): OutlierAnalysis[] {
    const outliers: OutlierAnalysis[] = [];

    // Calculate quartiles
    const q1 = quantile(data, 0.25);
    const q3 = quantile(data, 0.75);
    const iqr = interquartileRange(data);

    // IQR outlier thresholds: Q1 - 1.5*IQR and Q3 + 1.5*IQR
    const lowerBound = q1 - 1.5 * iqr;
    const upperBound = q3 + 1.5 * iqr;

    // Check each language
    data.forEach((value, index) => {
        if (value < lowerBound) {
            outliers.push({
                language: metrics[index].solver,
                metric: metricType,
                value,
                threshold: lowerBound,
                direction: 'low',
                explanation: `Exceptionally fast ${metricType} performance (below Q1 - 1.5×IQR threshold)`
            });
        } else if (value > upperBound) {
            outliers.push({
                language: metrics[index].solver,
                metric: metricType,
                value,
                threshold: upperBound,
                direction: 'high',
                explanation: `Unusually slow ${metricType} performance (above Q3 + 1.5×IQR threshold)`
            });
        }
    });

    return outliers;
}
```

### Pattern 4: Stacked Bar Chart with Native HTML/CSS
**What:** Mini stacked bar chart showing 80/20 time/memory contribution
**When to use:** Visualizing score decomposition in rankings table
**Example:**
```typescript
// Source: HTML/CSS best practices for inline visualizations
export function generateScoreDecomposition(
    timeScore: number,
    memoryScore: number,
    totalScore: number
): string {
    // Contributions are already weighted in the geometric mean
    // Show the 80/20 visual split since weights are { time: 0.8, memory: 0.2 }
    const timePercentage = 80;
    const memoryPercentage = 20;

    return `
        <div class="score-decomposition"
             title="Time: ${timeScore.toFixed(2)} (80%), Memory: ${memoryScore.toFixed(2)} (20%), Total: ${totalScore.toFixed(2)}">
            <div class="stacked-bar">
                <div class="bar-segment bar-time" style="width: ${timePercentage}%"></div>
                <div class="bar-segment bar-memory" style="width: ${memoryPercentage}%"></div>
            </div>
            <div class="score-value">${totalScore.toFixed(2)}</div>
        </div>
    `;
}

// CSS styles (add to SharedStyles.ts)
const scoreDecompositionStyles = `
.score-decomposition {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    cursor: help;
}

.stacked-bar {
    display: flex;
    width: 60px;
    height: 16px;
    border-radius: 3px;
    overflow: hidden;
    border: 1px solid #ddd;
}

.bar-segment {
    height: 100%;
    transition: opacity 0.2s ease;
}

.bar-time {
    background-color: #4A90E2;  /* Blue for time */
}

.bar-memory {
    background-color: #F5A623;  /* Orange for memory */
}

.score-decomposition:hover .stacked-bar {
    transform: scale(1.1);
    transition: transform 0.2s ease;
}

.score-value {
    font-weight: 600;
    font-size: 14px;
}
`;
```

### Pattern 5: Expandable Table Rows
**What:** Smooth animation for expanding sensitivity analysis data
**When to use:** Showing per-language rank positions across 4 scenarios
**Example:**
```typescript
// Source: HTML table expandable row patterns + CSS transitions
export function generateExpandableRow(
    language: string,
    mainRowData: string,
    sensitivityData: Array<{ scenario: string; rank: number; score: number }>,
    maxSwing: number
): string {
    const rowId = `expand-${language.replace(/[^a-zA-Z0-9]/g, '_')}`;

    return `
        <tr class="main-row" onclick="toggleRow('${rowId}')">
            ${mainRowData}
            <td class="expand-indicator">▼</td>
        </tr>
        <tr id="${rowId}" class="expandable-row" style="display: none;">
            <td colspan="100%">
                <div class="sensitivity-details">
                    <h4>Rank Sensitivity Analysis</h4>
                    <table class="sensitivity-table">
                        <thead>
                            <tr>
                                <th>Weight Scenario</th>
                                <th>Rank</th>
                                <th>Score</th>
                            </tr>
                        </thead>
                        <tbody>
                            ${sensitivityData.map(s => `
                                <tr>
                                    <td>${s.scenario}</td>
                                    <td>${s.rank}</td>
                                    <td>${s.score.toFixed(3)}</td>
                                </tr>
                            `).join('')}
                        </tbody>
                    </table>
                    <p class="rank-swing">Max rank swing: <strong>${maxSwing} positions</strong></p>
                </div>
            </td>
        </tr>
    `;
}

// JavaScript toggle function (inline in HTML)
const expandableRowScript = `
function toggleRow(rowId) {
    const row = document.getElementById(rowId);
    const isHidden = row.style.display === 'none';

    if (isHidden) {
        row.style.display = 'table-row';
        row.classList.add('expanding');
        setTimeout(() => row.classList.remove('expanding'), 300);
    } else {
        row.classList.add('collapsing');
        setTimeout(() => {
            row.style.display = 'none';
            row.classList.remove('collapsing');
        }, 300);
    }
}
`;

// CSS styles for smooth animation
const expandableRowStyles = `
.expandable-row {
    transition: opacity 0.3s ease, max-height 0.3s ease;
}

.expandable-row.expanding {
    animation: expandRow 0.3s ease;
}

.expandable-row.collapsing {
    animation: collapseRow 0.3s ease;
}

@keyframes expandRow {
    from {
        opacity: 0;
        transform: scaleY(0);
    }
    to {
        opacity: 1;
        transform: scaleY(1);
    }
}

@keyframes collapseRow {
    from {
        opacity: 1;
        transform: scaleY(1);
    }
    to {
        opacity: 0;
        transform: scaleY(0);
    }
}

.sensitivity-details {
    padding: 16px;
    background-color: #f8f9fa;
    border-left: 3px solid #4A90E2;
}

.expand-indicator {
    cursor: pointer;
    user-select: none;
    transition: transform 0.2s ease;
}

.main-row.expanded .expand-indicator {
    transform: rotate(180deg);
}
`;
```

### Anti-Patterns to Avoid
- **Modifying scoring formula in this phase:** Phase is explicitly scoped to analysis only, not formula changes. Keep calculateOverallScore() unchanged.
- **Using Z-score for outlier detection:** Benchmark data may be skewed (not normally distributed). IQR is more robust for non-normal distributions.
- **Heavy charting libraries:** D3.js/Chart.js add 200KB+ for simple stacked bars. Use native HTML/CSS for lightweight, maintainable solution.
- **Separate sensitivity analysis section:** User decision is to integrate into main table as expandable rows, not create separate section.
- **Raw score display:** User wants normalized contributions (80/20 visual split), not raw component scores.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Correlation coefficient | Custom sum-of-squares calculation | `simple-statistics.sampleCorrelation()` | Handles edge cases (zero variance, missing data), numerically stable |
| R² calculation | Manual regression fitting | `simple-statistics.rSquared()` with `linearRegression()` | Correct formula for coefficient of determination, handles degenerate cases |
| Percentiles (p50, p90, p99) | Sorting + indexing | `simple-statistics.quantile()` | Handles interpolation between values, edge cases (n=1, n=2) |
| IQR calculation | Q3 - Q1 manually | `simple-statistics.interquartileRange()` | Correct quantile calculation method, consistent with quartile definitions |
| Geometric mean for composite scores | Product of ratios ^ (1/n) | Use existing `calculateOverallScore()` | Already implemented correctly with weighted geometric mean formula |
| Expandable table rows | Custom JavaScript state management | Native CSS transitions + simple toggle function | No framework needed, accessible, performant |

**Key insight:** Statistical calculations have subtle correctness issues (numerical stability, edge cases, interpolation methods). Use battle-tested libraries rather than reinventing formulas from Wikipedia.

## Common Pitfalls

### Pitfall 1: Misinterpreting R² as Correlation Strength
**What goes wrong:** Treating R² = 0.3 as "30% correlated" when it means "30% of variance explained"
**Why it happens:** R² is squared correlation coefficient; its interpretation is non-linear
**How to avoid:** Always provide plain-English interpretation alongside R² value (see Pattern 2)
**Warning signs:** User confusion about what R² means; comparing R² directly to correlation coefficient

### Pitfall 2: Using Z-Score on Skewed Distributions
**What goes wrong:** Z-score outlier detection misses outliers or flags false positives when data isn't normal
**Why it happens:** Z-score assumes normal distribution; benchmark times are often right-skewed (long tail of slow languages)
**How to avoid:** Use IQR method (Q1 - 1.5×IQR, Q3 + 1.5×IQR) which doesn't assume distribution shape
**Warning signs:** C or other fast languages flagged as outliers; very slow languages not flagged

### Pitfall 3: Geometric Mean Domain Errors
**What goes wrong:** Math.pow() with negative numbers raises errors or produces NaN
**Why it happens:** Geometric mean formula uses fractional exponents; negative ratios break it
**How to avoid:** Scores should always be positive ratios (solver/C). Validate inputs before geometric mean calculation.
**Warning signs:** NaN scores in output; errors in calculateOverallScore()

### Pitfall 4: Table Row Animation Jank
**What goes wrong:** Expandable rows jump or flicker instead of smooth animation
**Why it happens:** CSS can't animate `display: none` → `display: table-row`; need to animate opacity/max-height instead
**How to avoid:** Use separate CSS classes for expanding/collapsing states with `@keyframes` animations (see Pattern 5)
**Warning signs:** No animation visible; instant show/hide; visual glitches during transition

### Pitfall 5: Sensitivity Analysis Rank Ties
**What goes wrong:** Multiple languages have identical scores, creating ambiguous ranks
**Why it happens:** Discrete benchmark matrices can produce identical total scores for different languages
**How to avoid:** Use stable sorting (Array.sort is stable in modern JS); document tie-breaking strategy (e.g., alphabetical)
**Warning signs:** Rank stability reports showing 0-position swings but ranks visually changing

### Pitfall 6: Percentile Confusion (p90 ≠ top 90%)
**What goes wrong:** Interpreting p90 as "top 90% performers" when it means "worse than 90% of data"
**Why it happens:** Percentile terminology is counterintuitive for performance metrics (lower is better)
**How to avoid:** Use clear labels: "p90: 90% of languages are faster than this threshold"
**Warning signs:** Users confused why p90 shows slow performance; expecting opposite interpretation

### Pitfall 7: Stacked Bar Width Arithmetic
**What goes wrong:** Stacked bar segments don't sum to 100% due to rounding errors
**Why it happens:** CSS width percentages with many decimal places; floating point precision
**How to avoid:** Hardcode 80% / 20% segments since weights are fixed; don't calculate dynamically
**Warning signs:** Visual gaps or overlaps in stacked bars; segments don't align properly

## Code Examples

Verified patterns from documentation and best practices:

### Computing Percentiles with simple-statistics
```typescript
// Source: simple-statistics documentation
import { quantile } from 'simple-statistics';

export function calculatePercentiles(data: number[]): {
    p50: number;
    p90: number;
    p99: number;
} {
    return {
        p50: quantile(data, 0.50),  // Median
        p90: quantile(data, 0.90),  // 90th percentile
        p99: quantile(data, 0.99)   // 99th percentile
    };
}

// Example usage with time data
const times = metrics.map(m => m.results.reduce((acc, r) => acc + r.time, 0));
const percentiles = calculatePercentiles(times);
console.log(`Median time: ${percentiles.p50.toFixed(2)}s`);
console.log(`90% of languages complete in: ${percentiles.p90.toFixed(2)}s`);
console.log(`99% of languages complete in: ${percentiles.p99.toFixed(2)}s`);
```

### Current Weighted Geometric Mean Scoring (Existing)
```typescript
// Source: Existing Metrics/scoring.ts
// This is the CURRENT implementation - Phase 5 does NOT change this

export function calculateMatrixScore(
    result: MetricResult | undefined,
    cResult: MetricResult | undefined,
    weights: Weights,
    timeout: number = TIMEOUT_SECONDS
): number {
    if (!cResult || cResult.time === 0) return 1.0;

    let time = 0;
    let memory = 0;

    if (!result || result.status !== 'success') {
        time = timeout;
        memory = cResult.memory;
    } else {
        time = result.time;
        memory = result.memory;
    }

    const timeRatio = Math.max(0.001, time / cResult.time);
    const cMem = Math.max(1, cResult.memory);
    const memRatio = Math.max(0.001, memory / cMem);

    // Weighted Geometric Mean: Score = (TimeRatio ^ w_time) * (MemRatio ^ w_mem)
    const score = Math.pow(timeRatio, weights.time) * Math.pow(memRatio, weights.memory);

    return score;
}
```

### Adaptive Threshold for Rank Stability Highlights
```typescript
// Source: Statistical best practices for threshold selection
export function selectStabilityThreshold(
    stabilityData: Array<{ language: string; maxSwing: number }>
): number {
    // Adaptive threshold: show languages with swings in top 20% (most unstable)
    const swings = stabilityData.map(d => d.maxSwing).sort((a, b) => b - a);
    const threshold = quantile(swings, 0.80);  // 80th percentile

    return Math.max(threshold, 5);  // Minimum threshold of 5 positions to be interesting
}

// Usage
const mostUnstable = stabilityData
    .filter(d => d.maxSwing >= selectStabilityThreshold(stabilityData))
    .slice(0, 5);  // Top 5 most unstable

const mostStable = stabilityData
    .filter(d => d.maxSwing <= 2)  // Very stable (0-2 position swing)
    .slice(0, 5);  // Top 5 most stable
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Arithmetic mean of ratios | Weighted geometric mean | Pre-2024 (existing in codebase) | Geometric mean handles ratios correctly; arithmetic mean would be dominated by outliers |
| Single composite score | Sensitivity analysis across weight scenarios | 2026 (this phase) | Transparency into how weighting affects rankings; validates 80/20 choice |
| No statistical validation | Correlation analysis + outlier detection | 2026 (this phase) | Identifies whether time/memory are independent; flags unusual performance |
| Z-score outlier detection | IQR-based detection | 2023+ | IQR more robust for non-normal distributions common in benchmarks |
| Heavy charting libraries (D3, Chart.js) | Native HTML/CSS for simple visualizations | 2024+ | Reduced bundle size; better performance; easier maintenance |

**Deprecated/outdated:**
- **Arithmetic mean for ratios:** Mathematically incorrect for normalized benchmark scores (ratios to baseline). Geometric mean is the only correct approach.
- **Underscore-cased function names in simple-statistics:** Version 7+ uses camelCase (`rSquared` not `r_squared`)
- **jQuery for table interactions:** Modern vanilla JS with CSS transitions is lighter and more maintainable

## Open Questions

Things that couldn't be fully resolved:

1. **Distribution analysis for performance tiers**
   - What we know: SCORE-06 requires "distribution analysis to identify performance tiers"
   - What's unclear: Specific algorithm for tier identification (k-means clustering? percentile breaks? manual thresholds?)
   - Recommendation: Use quartile-based tiers (p25/p50/p75 boundaries) as simple, interpretable approach. Document as "Top Tier (<p25), Mid Tier (p25-p75), Low Tier (>p75)"

2. **Historical comparability with scoring version changes**
   - What we know: SCORE-06 requires "scoring methodology changes preserve historical comparability through versioning"
   - What's unclear: No scoring formula changes in this phase, but if future phases change weights, how to version?
   - Recommendation: Add `scoringVersion: string` field to SolverMetrics interface; current version "v1.0" (80/20 weights, weighted geometric mean). Future changes increment version.

3. **Correlation significance testing**
   - What we know: Compute R² correlation coefficient
   - What's unclear: Should we test statistical significance (p-value) or just report R²?
   - Recommendation: Report R² with interpretation only. Statistical significance testing requires distributional assumptions; R² is descriptive and sufficient for this use case.

## Sources

### Primary (HIGH confidence)
- [simple-statistics GitHub](https://github.com/simple-statistics/simple-statistics) - Library documentation and TypeScript definitions
- [simple-statistics API Documentation](https://simple-statistics.github.io/docs/) - Function signatures for correlation, rSquared, quantile, interquartileRange, zScore
- [Weighted Geometric Mean for Benchmarks - SPEC](https://www.spec.org/gwpg/gpc.static/geometric.html) - Confirms geometric mean is correct for ratio normalization
- [IQR Outlier Detection Method - Procogia](https://procogia.com/interquartile-range-method-for-reliable-data-analysis/) - IQR formula and Q1 - 1.5×IQR thresholds
- [Presenting Statistical Data Best Practices - Statsig](https://www.statsig.com/perspectives/from-data-to-decisions-how-to-communicate-findings-to-non-technical-teams) - Plain-English interpretation guidelines

### Secondary (MEDIUM confidence)
- [D3.js Stacked Barplot with Tooltip](https://d3-graph-gallery.com/graph/barplot_stacked_hover.html) - Verified pattern for stacked bar tooltips (adapted to native HTML/CSS)
- [Google Charts Tooltip Customization](https://developers.google.com/chart/interactive/docs/customizing_tooltip_content) - HTML tooltip patterns (adapted)
- [HTML Table Expandable Rows - CodeHim](https://codehim.com/html5-css3/expand-and-collapse-table-rows-in-html-css/) - Verified CSS animation patterns
- [Understanding Percentiles p50, p90, p99 - DebugBear](https://www.debugbear.com/docs/rum/percentiles) - Percentile interpretation for performance metrics
- [10 Rules for Communicating Statistics - JMP](https://www.jmp.com/en/blog/inspiration/10-rules-for-communicating-statistical-concepts-to-a-non-technical-audience) - Accessibility guidelines for statistical presentation

### Tertiary (LOW confidence)
- [Sensitivity Analysis - Wikipedia](https://en.wikipedia.org/wiki/Sensitivity_analysis) - General concepts, not specific to weight scenarios
- [Rank-Order Stability - ScienceDirect Topics](https://www.sciencedirect.com/topics/psychology/rank-order-stability) - General definition, not specific implementation

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - simple-statistics is well-established (7+ years, active maintenance, TypeScript support)
- Architecture patterns: HIGH - Patterns verified against existing codebase (scoring.ts) and official documentation
- Outlier detection: HIGH - IQR method is standard practice, well-documented, mathematically sound
- Visualization: MEDIUM - Native HTML/CSS approach is sound but requires careful implementation for animations
- Correlation interpretation: HIGH - R² interpretation guidelines from multiple statistical sources

**Research date:** 2026-01-23
**Valid until:** 60 days (libraries are stable; simple-statistics updates infrequently; statistical methods are timeless)
