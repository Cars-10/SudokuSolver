# Technology Stack

**Project:** SudokuSolver Benchmark Quality & Visualization
**Researched:** 2026-01-23

## Recommended Stack

### Statistical Analysis

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| simple-statistics | ^7.8.8 | Core statistical functions | Lightweight (no dependencies), browser-compatible, provides correlation, outlier detection (MAD), standard deviation, quantiles. Perfect for scoring methodology analysis without adding build complexity. |
| stdlib-js/stats | ^0.x | Advanced statistical methods | Comprehensive statistical library with TypeScript support for distribution analysis, Grubbs' test for outliers. Use selectively for advanced features simple-statistics lacks. |

### Data Visualization - New Chart Types

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| D3.js | ^7.9.0 | Scatter plots, histograms, heatmaps | Already in use (v7 via CDN), proven in codebase. Native scatter, histogram, and heatmap support. No additional dependency needed - extend existing D3 usage. |
| @observablehq/plot | ^0.6.17 | Optional: High-level chart API | Consider for rapid prototyping only. Built on D3, offers concise API (histogram in 1 line vs 50). NOT recommended for production - adds 100KB+ and abstracts control you already have with D3. |

### Testing & Validation

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| Vitest | ^2.x | Test framework | 10-20x faster than Jest, native ESM/TypeScript support, excellent for concurrent tests. Matches project's modern stack (ES modules, TypeScript). Runs validation tests in watch mode during development. |
| Zod | ^3.x | Schema validation | TypeScript-first validation for metrics.json iteration counts, solver output format. Type-safe validation with automatic type inference. Perfect for enforcing correctness rules. |

### Supporting Libraries

| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| d3-array | included in D3 | Data binning, grouping | Already available via D3 v7, use for histogram bins and data transformations |
| d3-scale | included in D3 | Color scales for heatmaps | Already available via D3 v7, use for heatmap color interpolation |
| d3-axis | included in D3 | Chart axes | Already available via D3 v7, use for scatter plot axes |

## Alternatives Considered

| Category | Recommended | Alternative | Why Not |
|----------|-------------|-------------|---------|
| Statistics | simple-statistics | math.js | Math.js is comprehensive (matrices, symbolic computation) but overkill for statistical analysis. 500KB+ vs simple-statistics 30KB. Project needs statistics, not computer algebra. |
| Statistics | simple-statistics | stdlib-js (full) | Full stdlib is 50MB+. Use selective @stdlib/stats packages only for features simple-statistics lacks (e.g., Grubbs' test). |
| Visualization | D3.js (extend existing) | Plotly.js | Plotly.js is 3.5MB (vs extending existing D3). Built on D3 anyway. Abstracts control you already have. Only choose if team lacks D3 expertise (not the case). |
| Visualization | D3.js (extend existing) | Chart.js | Chart.js excels at simple charts but lacks flexibility for custom visualizations. You already have D3 expertise in codebase. |
| Testing | Vitest | Jest | Jest is mature but 10-20x slower in watch mode, experimental ESM support. Vitest is native ESM, better TypeScript DX, faster CI. Jest only if legacy compatibility required. |
| Testing | Vitest | AVA | AVA is fast (1.29s vs Vitest 2.75s for async tests) but smaller ecosystem, less mainstream. Vitest has better IDE integration and matches Vite-style projects. |
| Validation | Zod | Joi | Joi lacks TypeScript-first design. Zod infers types automatically. For TypeScript projects, Zod is the clear choice. |

## Installation

```bash
# Statistical analysis
npm install simple-statistics --save
npm install @stdlib/stats-base-dists --save  # Only if needed for specific distributions

# Visualization - D3 already included via CDN in index.html
# No new dependencies needed for scatter/histogram/heatmap

# Testing & validation
npm install -D vitest @vitest/ui
npm install zod --save

# Update Metrics/package.json
cd Metrics
npm install simple-statistics zod
npm install -D vitest @vitest/ui
```

## Integration Points with Existing Stack

### 1. Statistical Analysis in Report Generation

**File:** `Metrics/generate_report_only.ts`

**Integration:**
```typescript
import * as ss from 'simple-statistics';

// Analyze scoring distribution
const scores = metrics.map(m => m.score);
const mean = ss.mean(scores);
const median = ss.median(scores);
const stdDev = ss.standardDeviation(scores);
const correlation = ss.sampleCorrelation(times, memoryUsages);

// Outlier detection using MAD (Median Absolute Deviation)
const outliers = detectOutliers(scores); // Use simple-statistics MAD methods
```

**Why this works:** `generate_report_only.ts` already aggregates metrics. Add statistical analysis here before passing to HTMLGenerator.

### 2. D3 Visualization Extensions

**File:** `Metrics/modules/d3-chart.js` (already exists based on grep results)

**Integration:**
```javascript
// Scatter plot: Time vs Memory
function createScatterPlot(data) {
  const x = d3.scaleLog().domain(d3.extent(data, d => d.time));
  const y = d3.scaleLog().domain(d3.extent(data, d => d.memory));
  // ... existing D3 patterns
}

// Histogram: Score distribution
function createHistogram(data) {
  const histogram = d3.histogram()
    .domain(d3.extent(data, d => d.score))
    .thresholds(20);
  // ... existing D3 patterns
}

// Heatmap: Algorithm x Matrix performance
function createHeatmap(data) {
  const colorScale = d3.scaleSequential(d3.interpolateYlOrRd)
    .domain(d3.extent(data, d => d.time));
  // ... existing D3 patterns
}
```

**Why this works:** You already have D3 v7 loaded and modular chart structure. New chart types fit existing architecture.

### 3. Test Validation Framework

**File:** `Metrics/__tests__/validation.test.ts` (new)

**Integration:**
```typescript
import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import { readFileSync } from 'fs';

const MetricSchema = z.object({
  matrix: z.string(),
  iterations: z.number(),
  time: z.number().positive(),
  // ... other fields
});

describe('Algorithm Correctness', () => {
  it('validates iteration count for 1.matrix', () => {
    const metrics = JSON.parse(readFileSync('path/to/metrics.json'));
    const result = metrics.results.find(r => r.matrix.includes('1.matrix'));
    expect(result.iterations).toBe(656); // Reference count
  });
});
```

**Why this works:** Vitest runs in Node.js, can read filesystem. Validates metrics.json files after benchmark runs. Fast watch mode for TDD.

### 4. Runtime Validation in Report Generation

**File:** `Metrics/types.ts` (extend existing)

**Integration:**
```typescript
import { z } from 'zod';

export const MetricResultSchema = z.object({
  matrix: z.string(),
  time: z.number().positive(),
  iterations: z.number().int().positive(),
  memory: z.number().nonnegative(),
  cpu_user: z.number().nonnegative(),
  cpu_sys: z.number().nonnegative(),
  status: z.enum(['success', 'timeout', 'error']),
  output: z.string().optional()
});

export const SolverMetricsSchema = z.object({
  solver: z.string(),
  algorithmType: z.enum(['BruteForce', 'DLX', 'CP']).optional(),
  timestamp: z.string().datetime(),
  results: z.array(MetricResultSchema),
  // ... validation rules
});

// Use in gather_metrics.ts
const validatedMetrics = SolverMetricsSchema.parse(rawMetrics);
```

**Why this works:** Types.ts already defines interfaces. Add Zod schemas alongside. Validate at runtime when reading metrics.json files.

## What NOT to Add

### Do NOT Add: Observable Plot

**Temptation:** Concise API for quick charts
**Reality:**
- You already have D3 expertise in codebase (see `Metrics/modules/d3-chart.js`)
- Plot is 100KB+ additional bundle size for abstraction over D3
- Plot limits customization - you'll hit walls with complex interactions
- Your team already writes D3 code comfortably

**When to reconsider:** If adding team members with zero D3 experience who need to create simple charts quickly.

### Do NOT Add: Full stdlib-js

**Temptation:** Comprehensive scientific computing library
**Reality:**
- Full stdlib is 50MB+ (vs 30KB simple-statistics)
- You need statistics, not matrix operations or symbolic math
- Browser bundle size matters for report performance
- Use selective @stdlib packages only for specific advanced features

**When to reconsider:** If you need advanced statistical distributions or specialized numerical methods not in simple-statistics.

### Do NOT Add: Lodash/Ramda for Statistical Functions

**Temptation:** Already familiar with these utilities
**Reality:**
- Lodash/Ramda lack statistical functions (no correlation, no outlier detection)
- simple-statistics is purpose-built for statistics
- Smaller footprint when used alongside (tree-shaking works)

**When to reconsider:** Never. Use lodash for utilities, simple-statistics for statistics.

### Do NOT Add: Separate Build Step for Statistics

**Temptation:** Pre-compute statistics at build time
**Reality:**
- Statistics need to run on fresh benchmark data
- Report generation already runs in Node.js (generate_report_only.ts)
- Simple-statistics works in both Node and browser
- Keep flexibility to compute statistics client-side if needed

**When to reconsider:** If report generation becomes too slow (unlikely with simple-statistics performance).

## Statistical Methods Implementation

You don't need libraries for everything. Implement these in pure JavaScript:

### Weighted Geometric Mean (Already Implemented)

The project already uses weighted geometric mean for scoring. Verify the current implementation rather than replacing it.

```typescript
// Existing scoring logic in generate_report_only.ts
// Keep this - it's domain-specific and already tested
```

### Iteration Count Reference Validation

```typescript
// New: Validation rules
const REFERENCE_ITERATIONS: Record<string, number> = {
  '1.matrix': 656,
  '2.matrix': 439269,
  '3.matrix': 98847,
  '4.matrix': 9085,
  '5.matrix': 445778
};

function validateIterations(result: MetricResult): ValidationResult {
  const expected = REFERENCE_ITERATIONS[result.matrix];
  if (!expected) return { valid: false, reason: 'Unknown matrix' };
  if (result.iterations !== expected) {
    return {
      valid: false,
      reason: `Expected ${expected}, got ${result.iterations}`
    };
  }
  return { valid: true };
}
```

**Why pure JavaScript:** This is business logic, not statistical computation. No library needed.

### Outlier Detection Strategy

**Use simple-statistics for MAD (Median Absolute Deviation):**
```typescript
import * as ss from 'simple-statistics';

function detectOutliers(values: number[], threshold: number = 3): number[] {
  const median = ss.median(values);
  const deviations = values.map(v => Math.abs(v - median));
  const mad = ss.median(deviations);
  const modifiedZScores = values.map(v =>
    0.6745 * Math.abs(v - median) / mad
  );
  return values.filter((_, i) => modifiedZScores[i] > threshold);
}
```

**Why simple-statistics:** Provides median calculation. MAD formula is simple enough to implement. More robust than standard deviation for skewed distributions (common in benchmark data).

### Distribution Analysis

```typescript
import * as ss from 'simple-statistics';

function analyzeDistribution(scores: number[]) {
  return {
    mean: ss.mean(scores),
    median: ss.median(scores),
    stdDev: ss.standardDeviation(scores),
    quartiles: [
      ss.quantile(scores, 0.25),
      ss.quantile(scores, 0.50),
      ss.quantile(scores, 0.75)
    ],
    skewness: ss.sampleSkewness(scores)
  };
}
```

**Why simple-statistics:** All methods available. Skewness helps identify if geometric mean is appropriate for scoring.

### Correlation Analysis

```typescript
import * as ss from 'simple-statistics';

function analyzeCorrelations(metrics: SolverMetrics[]) {
  const times = metrics.map(m => m.results[0].time);
  const memories = metrics.map(m => m.results[0].memory);
  const scores = metrics.map(m => m.score);

  return {
    timeMemoryCorr: ss.sampleCorrelation(times, memories),
    timeScoreCorr: ss.sampleCorrelation(times, scores),
    memoryScoreCorr: ss.sampleCorrelation(memories, scores)
  };
}
```

**Why simple-statistics:** Pearson correlation implementation handles your use case. For non-linear relationships, implement Spearman's rank if needed (simple-statistics doesn't have it, but algorithm is straightforward).

## Browser vs Node Environments

### Current Architecture (Keep This)

- **Node.js:** Report generation (`generate_report_only.ts`) aggregates metrics, calculates scores
- **Browser:** HTML report (`index.html`) displays charts, interactive visualizations

### Recommended Environment Assignment

| Task | Environment | Library | Why |
|------|-------------|---------|-----|
| Scoring analysis | Node | simple-statistics | Run once during report generation, results embedded in HTML |
| Correlation analysis | Node | simple-statistics | Run once, display results in report |
| Outlier detection | Node | simple-statistics | Flag outliers during report generation |
| Scatter plots | Browser | D3.js | Interactive, needs user zoom/pan |
| Histograms | Browser | D3.js | Interactive bin selection |
| Heatmaps | Browser | D3.js | Interactive tooltips, highlighting |
| Test validation | Node | Vitest + Zod | Runs in CI, validates benchmark correctness |

### Bundle Size Impact

**Current:** `index.html` loads D3 v7 from CDN (~280KB minified)

**After additions:**
- **No change to browser bundle** - statistics run in Node during report generation
- Test dependencies (Vitest, Zod) are dev dependencies, don't affect production
- Statistical results embedded in HTML as JSON data

**Result:** Zero browser bundle size increase for statistical features.

## Configuration Files

### vitest.config.ts (new)

```typescript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['Metrics/__tests__/**/*.test.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'html'],
      include: ['Metrics/**/*.ts'],
      exclude: ['Metrics/__tests__/**']
    }
  }
});
```

### Update tsconfig.json

```json
{
  "compilerOptions": {
    "types": ["vitest/globals"]
  }
}
```

### Update Metrics/package.json scripts

```json
{
  "scripts": {
    "test": "vitest",
    "test:ui": "vitest --ui",
    "test:run": "vitest run",
    "validate": "vitest run --testPathPattern=validation"
  }
}
```

## Migration Path

### Phase 1: Statistical Analysis (Week 1)

1. Install simple-statistics in Metrics/
2. Add statistical analysis to `generate_report_only.ts`
3. Compute distribution stats, correlations, outliers
4. Embed results in HTML data payload
5. Display stats in report (no visualizations yet)

**Deliverable:** Report shows "Score distribution: mean=X, median=Y, outliers=Z"

### Phase 2: Validation Framework (Week 1-2)

1. Install Vitest and Zod
2. Create Zod schemas for MetricResult and SolverMetrics
3. Add runtime validation in `gather_metrics.ts`
4. Write test suite for iteration count validation
5. Run tests in CI before report generation

**Deliverable:** CI fails if any implementation has incorrect iteration counts

### Phase 3: New Visualizations (Week 2-3)

1. Extend `Metrics/modules/d3-chart.js` with new chart functions
2. Add scatter plot (time vs memory, colored by algorithm type)
3. Add histogram (score distribution)
4. Add heatmap (algorithm x matrix performance)
5. Add UI controls to switch between chart types
6. Use statistical results from Phase 1 (mean lines, outlier highlighting)

**Deliverable:** Interactive visualizations accessible from main report

### Phase 4: Integration & Polish (Week 3-4)

1. Link statistical analysis to visualizations (click outlier stat → highlight in scatter)
2. Add chart export functionality
3. Document new metrics and interpretations
4. Write tests for statistical edge cases
5. Performance optimization if needed

**Deliverable:** Production-ready enhanced benchmark report

## Version Pinning Strategy

**Exact versions for:**
- simple-statistics: Lock to minor version (^7.8.8) - stable API
- Zod: Lock to major version (^3.x) - breaking changes in major releases

**Flexible versions for:**
- Vitest: Allow minor updates (^2.x) - rapid development, good backwards compatibility
- D3.js: Already v7 via CDN, monitor for v8 release

**Why:** Statistical libraries change slowly (simple-statistics hasn't had major release in years). Testing frameworks iterate rapidly (Vitest releases monthly). Lock statistics libs for stability, update test tools for improvements.

## Sources

**Statistical Libraries:**
- [stats-analysis - npm](https://www.npmjs.com/package/stats-analysis)
- [Statistical Libraries in JavaScript](https://scribbler.live/2024/07/24/Statistical-Libraries-in-JavaScript.html)
- [simple-statistics - npm](https://www.npmjs.com/package/simple-statistics)
- [simple-statistics GitHub](https://github.com/simple-statistics/simple-statistics)
- [stdlib-js GitHub](https://github.com/stdlib-js/stdlib)
- [mathjs vs simple-statistics comparison](https://npm-compare.com/mathjs,jstat,simple-statistics)

**D3.js Visualization:**
- [D3 Graph Gallery - Heatmap](https://d3-graph-gallery.com/heatmap.html)
- [D3 Graph Gallery - Correlogram with scatterplot](https://d3-graph-gallery.com/graph/correlogram_histo.html)
- [D3.js Official Site](https://d3js.org/)
- [D3 v7 Releases](https://github.com/d3/d3/releases)
- [cdnjs D3 Library](https://cdnjs.com/libraries/d3)

**Testing & Validation:**
- [Jest vs Vitest](https://jestjs.io/)
- [Vitest comparison 2026](https://dev.to/dataformathub/vitest-vs-jest-30-why-2026-is-the-year-of-browser-native-testing-2fgb)
- [Vitest vs Jest Performance](https://betterstack.com/community/guides/scaling-nodejs/vitest-vs-jest/)
- [Zod Documentation](https://zod.dev/)
- [AVA Test Framework](https://github.com/avajs/ava)

**Observable Plot:**
- [@observablehq/plot npm](https://www.npmjs.com/package/@observablehq/plot)
- [Plot for D3 Users](https://observablehq.com/@observablehq/plot-for-d3-users)

**Comparisons:**
- [Plotly vs D3.js comparison](https://medium.com/@ebojacky/d3-js-vs-plotly-which-javascript-visualization-library-should-you-choose-dbf8ad67321f)
- [JavaScript Testing Frameworks 2026](https://vivasoftltd.com/javascript-testing-frameworks/)
