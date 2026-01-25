# Architecture: Validation, Scoring Analysis & Visualization Integration

**Project:** SudokuSolver Benchmark Quality Improvements
**Researched:** 2026-01-23
**Confidence:** HIGH

## Executive Summary

The SudokuSolver benchmark has a mature, three-stage pipeline: **execution → aggregation → visualization**. All new features (validation, scoring analysis, new charts) should integrate into this existing flow without breaking backward compatibility. The architecture follows a "fail-late, flag-early" pattern: collect all data during execution, validate during aggregation, and visualize warnings in the report.

**Key Integration Points:**
1. **Validation**: Build-time in TypeScript (gather_metrics.ts), with runtime optional checks in common.sh
2. **Scoring Analysis**: New TypeScript module consumed by HTMLGenerator.ts
3. **New Charts**: Extend existing D3.js chart selector pattern
4. **Invalid Warnings**: Extend existing diagnostics UI pattern

## Current Architecture

### Data Flow (Existing)

```
┌─────────────────────────────────────────────────────────────────┐
│ STAGE 1: Execution (Bash + Python)                             │
├─────────────────────────────────────────────────────────────────┤
│  runMe.sh (per language)                                        │
│    └─> sources common.sh                                        │
│        └─> Python subprocess wrapper (timing/memory)            │
│            └─> Solver binary execution                          │
│                └─> metrics.json (per language/algorithm)        │
│                    {                                            │
│                      solver: "Language",                        │
│                      algorithmType: "BruteForce|DLX|CP",        │
│                      timestamp: "ISO8601",                      │
│                      results: [{                                │
│                        matrix: "1",                             │
│                        time: 5.25,  // milliseconds             │
│                        iterations: 656,                         │
│                        memory: 1507328,  // bytes               │
│                        status: "success|timeout|error"          │
│                      }]                                         │
│                    }                                            │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ STAGE 2: Aggregation (TypeScript)                              │
├─────────────────────────────────────────────────────────────────┤
│  gather_metrics.ts                                              │
│    └─> Glob all Algorithms/**/metrics.json                     │
│    └─> Parse and merge into SolverMetrics[]                    │
│    └─> Call HTMLGenerator.generateHtml()                       │
│                                                                 │
│  HTMLGenerator.ts                                               │
│    └─> Calculate mismatch counts (lines 204-235)               │
│    └─> Build diagnostics object (lines 237-279)                │
│    └─> Embed data as JSON in <script> tags                     │
│    └─> Generate single-file index.html                         │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ STAGE 3: Visualization (Browser + D3.js)                       │
├─────────────────────────────────────────────────────────────────┤
│  index.html (single file, ~1500 lines)                         │
│    └─> Embedded JavaScript reads metricsData                   │
│    └─> D3.js chart rendering (client-side)                     │
│        └─> Chart selector: line|jockey|race|algorithm|         │
│                            language|iterations                  │
│    └─> Interactive table with sorting/filtering                │
│    └─> Diagnostics modal (env_error, timeout, mismatch)        │
└─────────────────────────────────────────────────────────────────┘
```

### Key Files & Responsibilities

| File | Role | Size | Integration Surface |
|------|------|------|-------------------|
| `Algorithms/common.sh` | Benchmark execution wrapper | 529 lines | RUNTIME validation entry point |
| `Metrics/types.ts` | TypeScript interfaces | 22 lines | ADD validation flags here |
| `Metrics/gather_metrics.ts` | Aggregates all metrics.json | ~50 lines | Minimal, calls HTMLGenerator |
| `Metrics/HTMLGenerator.ts` | Report generation | 1581 lines | PRIMARY integration point |
| `Metrics/C_Baselines.ts` | Reference iteration counts | 27 lines | Used for validation |
| `benchmark_issues.json` | Failure tracking | Dynamic | READ for diagnostics |
| `index.html` | Final report | Generated | Chart rendering, diagnostics UI |

## Integration Architecture for New Features

### 1. Iteration Count Validation

**Decision: Fail-late, flag-early (collect-and-warn pattern)**

The existing architecture already implements this pattern:
- Execution stage: Always completes, writes metrics.json
- Aggregation stage: Validates against C_Baselines.ts (lines 204-235)
- Visualization stage: Displays warnings in UI

**Why not fail-fast?**
- Existing flow: `benchmark_issues.json` logs failures but doesn't stop execution
- Value: Partial results are useful (some matrices may pass)
- Debugging: Seeing all failures together helps identify patterns

**Integration Points:**

```typescript
// EXISTING: HTMLGenerator.ts lines 204-235
// Validates per-matrix against algorithm-specific baseline
mismatchCount = metrics.filter(m => {
    const algoType = m.algorithmType || 'BruteForce';
    const cBaselineForAlgo = cMetricsByAlgorithm.get(algoType);
    // Check each result...
})
```

**NEW: Extend validation logic**

Location: `Metrics/HTMLGenerator.ts` (add after line 235)

```typescript
// NEW: Detailed mismatch tracking per language/matrix
interface ValidationResult {
    language: string;
    algorithm: string;
    matrix: string;
    actual: number;
    expected: number;
    percentDiff: number;
}

const validationResults: ValidationResult[] = [];

metrics.forEach(m => {
    const algoType = m.algorithmType || 'BruteForce';
    const cBaselineForAlgo = cMetricsByAlgorithm.get(algoType);

    if (!cBaselineForAlgo || m.solver === 'C') return;
    if (m.algorithmType === 'CP') return; // CP uses heuristics

    const cMap = new Map<string, number>();
    cBaselineForAlgo.results.forEach(r =>
        cMap.set(normalizeMatrix(r.matrix), r.iterations)
    );

    m.results.forEach(r => {
        const expected = cMap.get(normalizeMatrix(r.matrix));
        if (expected !== undefined && r.iterations !== expected) {
            validationResults.push({
                language: m.solver,
                algorithm: algoType,
                matrix: normalizeMatrix(r.matrix),
                actual: r.iterations,
                expected: expected,
                percentDiff: ((r.iterations - expected) / expected) * 100
            });
        }
    });
});
```

**NEW: Add to types.ts**

```typescript
export interface MetricResult {
    matrix: string;
    time: number;
    iterations: number;
    memory: number;
    status: string;
    output?: string;
    // NEW: Validation flags
    iterationMismatch?: boolean;
    expectedIterations?: number;
}
```

**OPTIONAL: Runtime validation in common.sh**

Add after line 246 (after extract_iterations):

```bash
# Optional: Validate iterations against reference
validate_iterations() {
    local matrix_num="$1"
    local actual="$2"
    local reference_file="../../../Matrices/reference_iterations.json"

    if [ ! -f "$reference_file" ]; then
        return 0  # No reference, skip validation
    fi

    local expected=$(python3 -c "import json; data=json.load(open('$reference_file')); print(data.get('$matrix_num', 0))")

    if [ "$expected" -ne 0 ] && [ "$actual" -ne "$expected" ]; then
        echo "WARNING: Iteration mismatch for matrix $matrix_num: expected $expected, got $actual" >&2
    fi
}

# Call after extraction (line 246)
local iterations=$(extract_iterations "$output")
validate_iterations "$matrix_num" "$iterations"
```

**Decision: Make runtime validation OPTIONAL via environment variable**

```bash
if [ "${VALIDATE_ITERATIONS:-0}" = "1" ]; then
    validate_iterations "$matrix_num" "$iterations"
fi
```

Rationale: Avoid slowing down benchmark runs, but enable for debugging.

### 2. Scoring Methodology Analysis

**Decision: Offline analysis module, integrated during report generation**

Create new TypeScript module that analyzes scoring sensitivity across different weight combinations.

**New Component: `Metrics/ScoringAnalyzer.ts`**

```typescript
export interface ScoringAnalysis {
    currentWeights: { time: number; memory: number };
    alternativeScenarios: {
        name: string;
        weights: { time: number; memory: number };
        topLanguages: string[];
        rankChanges: { language: string; currentRank: number; newRank: number }[];
    }[];
    sensitivity: {
        language: string;
        rankStability: number;  // 0-1, higher = more stable
    }[];
}

export function analyzeScoringMethodology(
    metrics: SolverMetrics[],
    baselineMetrics: SolverMetrics
): ScoringAnalysis {
    const scenarios = [
        { name: "Time-Only", time: 1.0, memory: 0.0 },
        { name: "Current", time: 0.8, memory: 0.2 },
        { name: "Balanced", time: 0.5, memory: 0.5 },
        { name: "Memory-Heavy", time: 0.2, memory: 0.8 },
        { name: "Memory-Only", time: 0.0, memory: 1.0 }
    ];

    // For each scenario, recalculate scores and ranks
    // Track rank changes per language
    // Calculate stability metric

    return analysis;
}
```

**Integration Point: HTMLGenerator.ts**

Add after line 200 (after C baseline calculation):

```typescript
// NEW: Scoring analysis
import { analyzeScoringMethodology } from './ScoringAnalyzer.ts';

const scoringAnalysis = analyzeScoringMethodology(metrics, cMetrics);
const scoringAnalysisJson = safeJSON(scoringAnalysis);
```

Embed in HTML template (after line 1413):

```html
<script>
    const scoringAnalysisData = ${scoringAnalysisJson};
</script>
```

**UI Integration: Add to chart selector (line 809)**

```html
<option value="scoring">Scoring Analysis</option>
```

**Chart Implementation: Add D3.js visualization**

Location: index.html client script (around line 1300)

```javascript
function renderScoringAnalysisChart() {
    const container = d3.select('#d3-chart');
    container.html(''); // Clear

    // Heatmap showing rank changes across scenarios
    // X-axis: Scenarios (Time-Only → Memory-Only)
    // Y-axis: Languages
    // Color: Rank change magnitude
    // Tooltip: Detailed rank info
}
```

### 3. New Chart Types

**Decision: Extend existing chart selector pattern**

The report already has a chart selector (line 809) with 6 chart types. New charts follow the same pattern.

**Current Chart Architecture:**

```
Chart Selector (HTML <select>)
    ↓ onchange="switchChart(value)"
    ↓
Client JavaScript function
    ↓ if (chartType === 'line') renderLineChart()
    ↓ if (chartType === 'jockey') renderJockeyChart()
    ↓ if (chartType === 'race') renderRaceChart()
    ↓ etc.
    ↓
D3.js rendering in #d3-chart div
```

**New Charts to Add:**

1. **Validation Dashboard** (`value="validation"`)
   - Shows languages with iteration mismatches
   - Per-matrix mismatch heatmap
   - Percent difference visualization

2. **Scoring Sensitivity** (`value="scoring"`)
   - Rank stability across weight scenarios
   - Interactive weight slider
   - Top-10 changes visualization

3. **Algorithm Comparison Enhanced** (extend existing)
   - Currently shows BruteForce vs DLX vs CP
   - Add per-language breakdown
   - Add iteration efficiency metric

**Implementation Pattern:**

```javascript
// Add to switchChart() function
function switchChart(chartType) {
    const container = d3.select('#d3-chart');
    container.html('');

    // Existing charts...
    if (chartType === 'line') renderLineChart();
    // ...

    // NEW charts
    if (chartType === 'validation') renderValidationDashboard();
    if (chartType === 'scoring') renderScoringAnalysisChart();
}

// NEW rendering function
function renderValidationDashboard() {
    // Access validationResultsData (embedded in HTML)
    const data = window.validationResultsData;

    // Create grouped bar chart: languages on X, matrices stacked
    // Color code: green (match), red (mismatch)
    // Tooltip: expected vs actual iterations
}
```

**Component Boundaries:**

| Chart Type | Data Source | Rendering Complexity | Dependencies |
|------------|-------------|---------------------|--------------|
| Validation Dashboard | validationResultsData | Medium (heatmap) | D3.js scales |
| Scoring Sensitivity | scoringAnalysisData | High (interactive) | D3.js transitions |
| Algorithm Comparison | metricsData (existing) | Low (extend existing) | None |

### 4. Invalid Implementation Warnings

**Decision: Extend existing diagnostics modal pattern**

The report already has a diagnostics system (line 237-279, UI at line 869).

**Current Diagnostics Structure:**

```typescript
const diagnostics = {
    env_error: { count: 0, languages: [{language, matrices}] },
    timeout: { count: 0, languages: [{language, matrices}] },
    error: { count: 0, languages: [{language, matrices}] },
    missing: { count: 0, languages: [{language, matrices}] }
};
```

**NEW: Add validation category**

```typescript
const diagnostics = {
    // Existing...
    env_error: { count: 0, languages: [] },
    // ...

    // NEW
    iteration_mismatch: {
        count: 0,
        languages: [{
            language: string,
            matrices: string[],
            details: {
                matrix: string,
                expected: number,
                actual: number,
                percentDiff: number
            }[]
        }]
    }
};
```

**UI Integration: Diagnostics Modal**

The modal is triggered by "Diagnostics" button (line 869). Extend the modal content generation:

```javascript
function showDiagnostics() {
    // Existing modal logic...

    // NEW section
    html += `<h3>⚠ Iteration Mismatches (${diagnostics.iteration_mismatch.count})</h3>`;
    html += `<p class="diagnostic-description">Languages where iteration counts don't match the C reference algorithm.</p>`;

    diagnostics.iteration_mismatch.languages.forEach(lang => {
        html += `<div class="diagnostic-item">`;
        html += `<strong>${lang.language}</strong>: `;
        html += `${lang.matrices.length} matrices with discrepancies`;

        // Expandable details
        html += `<details><summary>Details</summary>`;
        lang.details.forEach(d => {
            html += `Matrix ${d.matrix}: expected ${d.expected}, got ${d.actual} `;
            html += `(${d.percentDiff > 0 ? '+' : ''}${d.percentDiff.toFixed(2)}%)`;
        });
        html += `</details></div>`;
    });
}
```

**Visual Indicators in Main Table:**

Extend existing table row generation (around line 1000) to add warning icons:

```html
<tr class="${hasIterationMismatch ? 'mismatch' : ''}">
    <td>
        ${language}
        ${hasIterationMismatch ? '<span title="Iteration count mismatch">⚠</span>' : ''}
    </td>
    <!-- ... -->
</tr>
```

## Data Flow Changes Summary

### Before (Current)

```
runMe.sh → metrics.json → gather_metrics.ts → HTMLGenerator.ts → index.html
                                                     ↓
                                            Calculate mismatches
                                            Build diagnostics
                                                     ↓
                                            Embed as JSON
```

### After (With New Features)

```
runMe.sh → metrics.json → gather_metrics.ts → HTMLGenerator.ts → index.html
             ↓                                      ↓
     (optional runtime                    Load C_Baselines.ts
      validation)                                   ↓
                                          Validate iterations (detailed)
                                                   ↓
                                          ScoringAnalyzer.ts
                                                   ↓
                                          Build enhanced diagnostics
                                                   ↓
                                          Embed: metricsData,
                                                 validationResultsData,
                                                 scoringAnalysisData,
                                                 diagnostics
                                                   ↓
                                          Generate index.html
                                                   ↓
                                          Browser renders:
                                            - Validation dashboard
                                            - Scoring analysis chart
                                            - Enhanced diagnostics
```

## Backward Compatibility Strategy

### Existing metrics.json Format (Must Preserve)

```json
[{
    "solver": "C",
    "runType": "automated",
    "timestamp": "2026-01-22T20:31:48Z",
    "results": [{
        "matrix": "1",
        "time": 5.2529,
        "iterations": 656,
        "memory": 1507328,
        "status": "success"
    }]
}]
```

**Compatibility Rules:**

1. **Never break existing fields** - All new fields are additive
2. **Graceful degradation** - New features work with old metrics.json files
3. **Optional fields** - New validation flags are optional in types.ts

```typescript
// SAFE: Optional fields with ? suffix
export interface MetricResult {
    matrix: string;          // REQUIRED (existing)
    time: number;            // REQUIRED (existing)
    iterations: number;      // REQUIRED (existing)
    memory: number;          // REQUIRED (existing)
    status: string;          // REQUIRED (existing)
    iterationMismatch?: boolean;      // OPTIONAL (new)
    expectedIterations?: number;      // OPTIONAL (new)
}
```

### HTMLGenerator.ts Changes (Defensive)

```typescript
// BEFORE: Direct access
const iterations = result.iterations;

// AFTER: Safe access with fallback
const iterations = result.iterations || 0;
const hasValidation = result.iterationMismatch !== undefined;
```

## Phased Integration Approach

### Phase 1: Validation Infrastructure (Minimal Risk)

**Goal:** Add validation without changing execution flow

**Changes:**
1. Extend `types.ts` with optional validation fields
2. Add detailed validation logic to `HTMLGenerator.ts` (after line 235)
3. Embed `validationResultsData` in HTML template
4. Add validation chart to chart selector
5. Extend diagnostics modal

**Testing:**
- Run on existing metrics.json files (should work unchanged)
- Verify new chart renders with old data (empty state)
- Confirm table rendering unchanged

**Risk:** LOW - All changes are additive to aggregation/visualization stages

### Phase 2: Scoring Analysis Module (Isolated)

**Goal:** Add scoring insights without changing core metrics

**Changes:**
1. Create `Metrics/ScoringAnalyzer.ts` (new file, zero impact on existing code)
2. Import and call from `HTMLGenerator.ts` (after line 200)
3. Embed `scoringAnalysisData` in HTML template
4. Add scoring chart to chart selector

**Testing:**
- Verify report generation time doesn't degrade significantly
- Test with different metric scenarios
- Confirm analysis is read-only (doesn't modify metrics)

**Risk:** LOW - Pure analysis module, no side effects

### Phase 3: Enhanced Charts (UI Extension)

**Goal:** Improve visualization without breaking existing charts

**Changes:**
1. Add new chart types to selector (line 809)
2. Implement rendering functions (follow existing pattern)
3. Test with existing chart infrastructure

**Testing:**
- Verify chart switching still works
- Confirm D3.js library compatibility
- Test fullscreen/export functionality

**Risk:** LOW - Client-side only, existing charts unaffected

### Phase 4: Runtime Validation (Optional, Highest Risk)

**Goal:** Add validation feedback during benchmark execution

**Changes:**
1. Add `validate_iterations()` to `common.sh`
2. Make it opt-in via `VALIDATE_ITERATIONS` env var
3. Log warnings to stderr (don't fail execution)

**Testing:**
- Run with validation enabled and disabled
- Verify metrics.json output unchanged
- Confirm warnings don't break parsing

**Risk:** MEDIUM - Touches execution stage, but opt-in reduces risk

## Component Dependencies

```
┌─────────────────────────────────────────────────────────────┐
│ New Components (To Be Created)                             │
├─────────────────────────────────────────────────────────────┤
│ ScoringAnalyzer.ts                                          │
│   ├─ Depends on: types.ts (SolverMetrics)                  │
│   └─ Used by: HTMLGenerator.ts                             │
│                                                             │
│ ValidationDashboard (D3.js chart)                           │
│   ├─ Depends on: validationResultsData (embedded JSON)     │
│   └─ Rendered in: index.html client script                 │
│                                                             │
│ ScoringAnalysisChart (D3.js chart)                          │
│   ├─ Depends on: scoringAnalysisData (embedded JSON)       │
│   └─ Rendered in: index.html client script                 │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Modified Components                                         │
├─────────────────────────────────────────────────────────────┤
│ types.ts                                                    │
│   └─ Add: iterationMismatch?, expectedIterations? to       │
│           MetricResult                                      │
│                                                             │
│ HTMLGenerator.ts (PRIMARY integration point)                │
│   ├─ Add: Detailed validation logic (after line 235)       │
│   ├─ Add: Import and call ScoringAnalyzer (after line 200) │
│   ├─ Add: Enhanced diagnostics (extend line 237-279)       │
│   └─ Add: Embed new JSON data in template (after line 1413)│
│                                                             │
│ index.html (generated)                                      │
│   ├─ Add: New chart options in selector (line 809)         │
│   ├─ Add: Chart rendering functions (around line 1300)     │
│   └─ Add: Enhanced diagnostics UI (around line 869)        │
│                                                             │
│ common.sh (OPTIONAL)                                        │
│   └─ Add: validate_iterations() function (after line 246)  │
│           (opt-in via VALIDATE_ITERATIONS env var)         │
└─────────────────────────────────────────────────────────────┘
```

## Build Order Recommendations

Based on dependencies and risk, implement in this order:

1. **Foundation: Type System**
   - Extend `types.ts` with validation fields
   - No dependencies, minimal risk
   - Enables all subsequent work

2. **Validation Logic**
   - Add detailed validation to `HTMLGenerator.ts`
   - Depends on: types.ts
   - Risk: Low (read-only analysis)

3. **Validation UI**
   - Add diagnostics category
   - Add validation chart
   - Depends on: Validation logic
   - Risk: Low (UI only)

4. **Scoring Analysis Module**
   - Create `ScoringAnalyzer.ts`
   - Integrate into `HTMLGenerator.ts`
   - Depends on: types.ts
   - Risk: Low (isolated module)

5. **Scoring UI**
   - Add scoring chart
   - Depends on: ScoringAnalyzer.ts
   - Risk: Low (UI only)

6. **Runtime Validation (Optional)**
   - Add to `common.sh`
   - Make opt-in
   - Depends on: Nothing (standalone)
   - Risk: Medium (execution stage)

## Performance Considerations

### Current Report Generation Time

Based on architecture analysis:
- Metrics aggregation: Fast (glob + JSON parse, ~100ms)
- HTMLGenerator.ts: Medium (~1-2s for 88 languages)
- D3.js rendering: Client-side (variable, typically <1s)

### Performance Impact of New Features

| Feature | Stage | Impact | Mitigation |
|---------|-------|--------|------------|
| Detailed validation | Build-time | +10-50ms | Acceptable (one-time cost) |
| Scoring analysis | Build-time | +50-200ms | Cache if used in multiple charts |
| Validation chart | Render-time | +100-300ms | Lazy render (only when selected) |
| Scoring chart | Render-time | +200-500ms | Lazy render (only when selected) |
| Runtime validation | Per-matrix | +5-10ms | Opt-in only (default off) |

**Total Impact:** ~1s added to report generation, negligible to client-side rendering (charts load on demand).

## Anti-Patterns to Avoid

### Don't: Fail Fast on Validation Errors

```typescript
// BAD: Stops execution on mismatch
if (result.iterations !== expected) {
    throw new Error('Validation failed');
}
```

**Why:** Loses partial results, makes debugging harder.

**Do Instead:** Collect all errors, display in diagnostics.

### Don't: Modify metrics.json During Aggregation

```typescript
// BAD: Mutates source data
metricsData.forEach(m => {
    m.score = calculateScore(m); // Writes back to parsed JSON
    fs.writeFileSync('metrics.json', JSON.stringify(m));
});
```

**Why:** Breaks separation of concerns, creates side effects.

**Do Instead:** Calculate derived data in-memory, keep metrics.json pristine.

### Don't: Create Separate Validation JSON File

```typescript
// BAD: Parallel data structure
fs.writeFileSync('validation_results.json', validationData);
```

**Why:** Increases complexity, risk of desync.

**Do Instead:** Embed validation flags in existing MetricResult objects.

### Don't: Duplicate Chart Rendering Logic

```javascript
// BAD: Copy-paste pattern
function renderValidationChart() { /* 200 lines */ }
function renderScoringChart() { /* 180 lines, mostly duplicate */ }
```

**Why:** Maintenance burden, inconsistent UX.

**Do Instead:** Extract common D3.js patterns into helper functions.

```javascript
function renderBarChart(data, options) { /* reusable */ }
function renderValidationChart() {
    renderBarChart(validationData, { color: 'red', ... });
}
```

## Integration Checklist

- [ ] Extended types.ts with optional validation fields
- [ ] Added detailed validation logic to HTMLGenerator.ts
- [ ] Created ScoringAnalyzer.ts module
- [ ] Integrated scoring analysis into report generation
- [ ] Added validationResultsData to HTML template
- [ ] Added scoringAnalysisData to HTML template
- [ ] Extended chart selector with new options
- [ ] Implemented validation dashboard rendering
- [ ] Implemented scoring analysis chart rendering
- [ ] Extended diagnostics modal with validation category
- [ ] Added table row indicators for mismatches
- [ ] Tested backward compatibility with old metrics.json
- [ ] Verified performance impact acceptable
- [ ] (Optional) Added runtime validation to common.sh
- [ ] (Optional) Documented VALIDATE_ITERATIONS env var

## Success Criteria

Integration is complete when:

1. **Validation works seamlessly**
   - Old metrics.json files render without errors
   - New validation data appears in diagnostics
   - Validation chart shows meaningful insights
   - Table indicates mismatched languages visually

2. **Scoring analysis is insightful**
   - Multiple scenarios computed efficiently
   - Chart shows rank stability clearly
   - Interactive weight adjustment works smoothly

3. **New charts follow existing patterns**
   - Chart selector includes new options
   - Rendering performance is acceptable
   - Fullscreen/export features work
   - Charts gracefully handle missing data

4. **No regressions**
   - Existing charts still work
   - Table sorting/filtering intact
   - Report generation time < 5s (for 88 languages)
   - All existing diagnostics functional

## Sources

**HIGH confidence - Direct codebase analysis:**
- `/Users/vibe/ClaudeCode/SudokuSolver/Algorithms/common.sh` - Execution wrapper
- `/Users/vibe/ClaudeCode/SudokuSolver/Metrics/types.ts` - Type definitions
- `/Users/vibe/ClaudeCode/SudokuSolver/Metrics/HTMLGenerator.ts` - Report generation (1581 lines)
- `/Users/vibe/ClaudeCode/SudokuSolver/Metrics/C_Baselines.ts` - Reference iteration counts
- `/Users/vibe/ClaudeCode/SudokuSolver/benchmark_config.json` - Current configuration
- `/Users/vibe/ClaudeCode/SudokuSolver/server/index.js` - Express server API
- `/Users/vibe/ClaudeCode/SudokuSolver/CLAUDE.md` - Project documentation

**Validation logic:** Lines 204-235 in HTMLGenerator.ts already implement per-matrix validation against C baseline
**Diagnostics pattern:** Lines 237-279 in HTMLGenerator.ts show existing error categorization
**Chart architecture:** Line 809 (chart selector) and surrounding D3.js rendering code
**Data flow:** common.sh line 246 (iteration extraction) → metrics.json → gather_metrics.ts → HTMLGenerator.ts → index.html
