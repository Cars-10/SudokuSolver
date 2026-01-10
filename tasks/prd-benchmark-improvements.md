# PRD: Benchmark Suite Improvements

## Introduction

This PRD covers a set of improvements to the Sudoku benchmark suite focusing on: a new multi-dimensional scoring system, fixing modal data persistence issues, enhanced metrics storage with historical run tracking for compiler/runtime variant comparison, richer per-matrix details in the UI modal, and cleanup of invalid metrics entries.

## Goals

- Create a comprehensive, creative scoring system that evaluates languages across all available performance dimensions
- Fix data loss issues where modal-saved images disappear after updates
- Enable historical run tracking to compare compiler flags, runtime options, and optimization variants
- Design SQLite schema to support variant comparison and historical analysis
- Enhance modal display to show detailed per-matrix timing and metrics
- Clean up metrics.json files by removing invalid "N/A" matrix entries
- Maintain backward compatibility with existing metrics.json structure
- Fix fullscreen graph resizing issues

## User Stories

### US-001: Design New Multi-Dimensional Scoring System
**Description:** As a benchmark analyst, I want a comprehensive scoring system that evaluates all available metrics so that I can fairly compare languages across multiple performance dimensions.

**Acceptance Criteria:**
- [ ] New scoring formula uses all available metrics: time, memory, cpu_user, cpu_sys, iterations, page_faults_major, page_faults_minor, context_switches_voluntary, context_switches_involuntary, io_inputs, io_outputs
- [ ] Score broken into weighted categories:
  - Speed Score (time, cpu_user, cpu_sys)
  - Memory Score (memory, page_faults)
  - Efficiency Score (iterations, context switches)
  - I/O Score (io_inputs, io_outputs)
- [ ] Overall composite score combines category scores with fixed weights (not user-configurable)
- [ ] Each category score normalized against C baseline (score of 1.0 = matches C)
- [ ] Lower scores are better (like golf: closer to 0 = better performance)
- [ ] New tier system reflects multi-dimensional performance (e.g., "Fast but Memory Heavy")
- [ ] Formula documented in code comments and README
- [ ] Typecheck/lint passes

### US-002: Implement Scoring Visualization in Report
**Description:** As a user viewing the report, I want to see both the composite score and individual category breakdowns so that I understand each language's strengths and weaknesses.

**Acceptance Criteria:**
- [ ] Report displays composite score prominently
- [ ] Radar/spider chart or bar chart shows category scores (Speed, Memory, Efficiency, I/O)
- [ ] Hover tooltip explains what each category measures
- [ ] Color coding indicates performance relative to baseline (green = better, red = worse)
- [ ] Typecheck/lint passes
- [ ] Verify in browser using dev-browser skill

### US-003: Fix Modal Image Data Persistence
**Description:** As a user editing language metadata, I want my image updates to persist permanently so that I don't lose my changes.

**Acceptance Criteria:**
- [ ] Investigate and identify root cause of image data loss (likely race condition or overwrite)
- [ ] Implement atomic save to metadata.json (write to temp file, then rename)
- [ ] Add timestamp check before overwriting to prevent stale data overwrites
- [ ] Modal shows "Saving..." indicator during save operation
- [ ] Modal shows "Saved" confirmation when complete (without page reload)
- [ ] Image URLs validated before save (non-empty if field was filled)
- [ ] Typecheck/lint passes
- [ ] Verify in browser using dev-browser skill

### US-004: Add Historical Run Storage to metrics.json
**Description:** As a developer testing compiler variants, I want each run stored as a separate historical entry so that I can compare performance across different compilation options.

**Acceptance Criteria:**
- [ ] metrics.json structure updated to support multiple runs per matrix:
  ```json
  [{
    "solver": "C",
    "variant": "O2",  // NEW: compiler flag, runtime option, etc.
    "runType": "Local",
    "timestamp": "...",
    "results": [...]
  }]
  ```
- [ ] Each run identified by combination of (solver, variant, timestamp)
- [ ] Running `VARIANT=Ofast ./runMe.sh` creates new entry with variant="Ofast"
- [ ] Running without VARIANT auto-detects default variant from language toolchain:
  - Compiled languages: detect compiler and version (e.g., "gcc-14", "clang-18", "rustc-1.75")
  - Interpreted languages: detect runtime version (e.g., "python-3.12", "node-22", "ruby-3.3")
  - JVM languages: detect JDK version (e.g., "openjdk-21")
- [ ] Each language's runMe.sh should have logic to detect available variant info
- [ ] Historical runs preserved (not overwritten) - ALL runs kept indefinitely
- [ ] Same variant re-run creates new timestamped entry (history preserved)
- [ ] Typecheck/lint passes

### US-005: Single Matrix Run Updates Only That Matrix
**Description:** As a developer, when I run a single matrix manually, I want only that matrix's entry updated in the current run so that other matrix results are preserved.

**Acceptance Criteria:**
- [ ] `./runMe.sh ../../Matrices/1.matrix` updates only matrix "1" in results array
- [ ] Other matrix entries (2-6) in the same run entry remain unchanged
- [ ] If matrix entry doesn't exist, it's added to results array
- [ ] Results array stays sorted by matrix number
- [ ] Timestamp updated to reflect the new run time
- [ ] Typecheck/lint passes

### US-006: Design SQLite Schema for Benchmark History
**Description:** As a system architect, I want a SQLite database schema that supports variant comparison and historical analysis so that we can query and analyze performance trends.

**Acceptance Criteria:**
- [ ] Create `database/schema.sql` with tables:
  - `languages` (id, name, created_at)
  - `variants` (id, language_id, name, description, compiler_flags, created_at)
  - `benchmark_runs` (id, variant_id, run_type, timestamp, environment_info)
  - `matrix_results` (id, run_id, matrix_number, time_ms, iterations, memory_bytes, cpu_user_ms, cpu_sys_ms, page_faults_major, page_faults_minor, context_switches_voluntary, context_switches_involuntary, io_inputs, io_outputs, status, output_text)
  - `scores` (id, run_id, composite_score, speed_score, memory_score, efficiency_score, io_score)
- [ ] Add indexes for common queries (by language, by variant, by timestamp)
- [ ] Before importing: backup existing database to `database/benchmarks.db.bak-{timestamp}`
- [ ] Start with empty schema on each full import (fresh database)
- [ ] Create import script to populate from all existing metrics.json files
- [ ] Document schema in `database/README.md`

### US-007: Enhance Modal Per-Matrix Display
**Description:** As a user viewing language details, I want to see comprehensive metrics for each matrix run including timing so that I can analyze performance in detail.

**Acceptance Criteria:**
- [ ] Modal shows expandable/collapsible section for each matrix (1-6)
- [ ] Each matrix section displays:
  - Solve time (formatted appropriately: ms or seconds)
  - Iteration count
  - Memory usage (formatted: KB, MB as appropriate)
  - CPU time breakdown (user + system)
  - Page faults (major/minor)
  - Context switches (voluntary/involuntary)
  - I/O operations (inputs/outputs)
  - Run timestamp (when this specific matrix was last run)
  - Status indicator (success/error/timeout)
- [ ] Visual comparison bar showing time relative to C baseline for that matrix
- [ ] Variant selector dropdown if multiple variants exist
- [ ] Typecheck/lint passes
- [ ] Verify in browser using dev-browser skill

### US-008: Display Input and Solved Puzzle in Output
**Description:** As a user running benchmarks, I want to see both the input puzzle and solved puzzle in the output so that I can verify correctness.

**Acceptance Criteria:**
- [ ] Terminal output clearly shows "Input Puzzle:" section with initial state
- [ ] Terminal output shows "Solved Puzzle:" section with final state
- [ ] Puzzles formatted as 9x9 grid with space-separated digits
- [ ] Empty cells shown as "0" or "." consistently
- [ ] Both puzzles visible in modal output section when viewing results
- [ ] Typecheck/lint passes
- [ ] Verify in browser using dev-browser skill

### US-009: Clean Up Invalid Metrics Entries
**Description:** As a maintainer, I want to remove invalid entries from metrics.json files so that the data is clean and reports are accurate.

**Acceptance Criteria:**
- [ ] Create cleanup script: `scripts/cleanup_metrics.ts`
- [ ] Script scans all `Languages/*/metrics.json` files
- [ ] Removes any result entries where `"matrix": "N/A"`
- [ ] Removes entire run entries if all results are invalid
- [ ] Creates backup before modification (`metrics.json.bak`)
- [ ] Reports count of removed entries per language
- [ ] Script runs automatically before report generation (integrated into pipeline)
- [ ] Script is idempotent (safe to run multiple times)
- [ ] Typecheck/lint passes

### US-010: Add Run Timestamp to Each Matrix Result
**Description:** As a user, I want each matrix result to track when it was individually run so that I know the freshness of each data point.

**Acceptance Criteria:**
- [ ] Each result object in metrics.json includes `"run_timestamp": "ISO-8601"`
- [ ] Timestamp set when that specific matrix is run (not just the outer run timestamp)
- [ ] Modal displays this timestamp for each matrix row
- [ ] Existing results without timestamp show "Unknown" or migration adds current time
- [ ] Typecheck/lint passes

### US-011: Fix Fullscreen Graph Resizing
**Description:** As a user viewing the report in fullscreen mode, I want the graphs to resize and fill the available screen space so that I can see the data more clearly.

**Acceptance Criteria:**
- [ ] Graphs detect when browser enters fullscreen mode
- [ ] Graphs resize to fill available viewport when in fullscreen
- [ ] Graphs maintain proper aspect ratio during resize
- [ ] Graphs return to original size when exiting fullscreen
- [ ] All chart types (bar, radar, scatter, etc.) handle resize correctly
- [ ] No visual glitches or flickering during resize transition
- [ ] Resize works with F11 fullscreen, browser fullscreen button, and any fullscreen API triggers
- [ ] Typecheck/lint passes
- [ ] Verify in browser using dev-browser skill

## Functional Requirements

### Scoring System
- FR-1: Calculate Speed Score as weighted combination of normalized time, cpu_user, and cpu_sys metrics
- FR-2: Calculate Memory Score from normalized memory and page fault metrics
- FR-3: Calculate Efficiency Score from iterations and context switch metrics
- FR-4: Calculate I/O Score from io_inputs and io_outputs metrics
- FR-5: Composite Score = geometric mean of category scores (or configurable weighted average)
- FR-6: All scores normalized against C language baseline (C = 1.0)
- FR-7: Display letter grades based on composite score thresholds

### Data Persistence
- FR-8: Modal saves must be atomic (temp file + rename)
- FR-9: Modal saves must not trigger full page reload
- FR-10: Metadata saves must preserve all existing fields not being edited
- FR-11: Add optimistic locking using lastUpdated timestamp

### Metrics Storage
- FR-12: Support VARIANT environment variable to tag runs
- FR-13: Merge logic: same (solver, variant) overwrites; different variant creates new entry
- FR-14: Single matrix runs merge into existing results array for that variant
- FR-15: Results sorted by matrix number after any modification

### UI Enhancements
- FR-16: Modal must display all available metrics per matrix
- FR-17: Modal must show per-matrix run timestamps
- FR-18: Modal must support variant selection for comparison
- FR-19: Puzzle output must show both input and solved states

### Data Cleanup
- FR-20: Remove entries where matrix field is "N/A"
- FR-21: Maintain backup of original files before cleanup
- FR-22: Cleanup runs automatically before report generation

### Chart/Graph Display
- FR-23: Charts must respond to window resize events
- FR-24: Charts must detect fullscreen state changes
- FR-25: Chart containers must use responsive CSS (100% width/height when fullscreen)
- FR-26: Chart.js instances must call `.resize()` on fullscreen change

## Non-Goals (Out of Scope)

- Real-time benchmark streaming/progress updates
- Historical trend graphs over time (future enhancement)
- Multi-machine benchmark comparison
- Automated regression detection/alerts
- Changes to the core solving algorithm
- Adding new test matrices
- User-configurable scoring weights in UI

## Design Considerations

### Scoring Visualization
- Consider radar/spider chart for multi-dimensional score display
- Use consistent color scheme: green (good), yellow (average), red (poor)
- Reuse existing Chart.js library if available, or consider lightweight alternative

### Modal Layout
- Each matrix should be a collapsible accordion section
- Consider tabs for "Summary" vs "Detailed Metrics" view
- Mobile-responsive design for modal content

### metrics.json Evolution
Current structure:
```json
[{
  "solver": "C",
  "runType": "Local",
  "timestamp": "2026-01-09T...",
  "results": [{"matrix": "1", "time": 0.01, ...}]
}]
```

Proposed structure:
```json
[{
  "solver": "C",
  "variant": "O2",           // NEW
  "runType": "Local",
  "timestamp": "2026-01-09T...",
  "results": [{
    "matrix": "1",
    "time": 0.01,
    "run_timestamp": "...",  // NEW: per-matrix timestamp
    ...
  }]
}]
```

## Technical Considerations

### Backward Compatibility
- Existing metrics.json files without "variant" field should default to "default"
- Existing results without "run_timestamp" should use parent timestamp or "unknown"
- Migration script should handle both old and new formats

### SQLite Integration
- Use better-sqlite3 or sql.js for Node.js compatibility
- Database file location: `database/benchmarks.db`
- Consider lazy initialization (create on first write)

### Atomic File Operations
- Use `fs.writeFileSync` to temp file, then `fs.renameSync`
- Handle errors gracefully with user feedback
- Consider file locking for concurrent access (rare but possible)

### Performance
- Scoring calculations should be cached after initial computation
- Modal should lazy-load detailed metrics on expand
- Cleanup script should process files in parallel

## Success Metrics

- All 11+ metrics captured per matrix run are utilized in scoring
- No data loss when editing language metadata in modal
- Users can compare at least 3 variants of the same language side-by-side
- Modal shows per-matrix timing with sub-millisecond precision where available
- Zero "N/A" matrix entries remain after cleanup
- SQLite schema supports efficient queries for variant comparison
- Charts fill 100% of viewport when in fullscreen mode

## Resolved Questions

1. **Should the category weights in the composite score be user-configurable in the UI?**
   - **Answer: No.** Weights are fixed in code.

2. **What should the default variant name be when VARIANT env is not set?**
   - **Answer:** Auto-detect from language toolchain (e.g., "gcc-14", "python-3.12", "openjdk-21").

3. **How many historical runs per variant should be retained?**
   - **Answer: All.** Keep all historical runs indefinitely. Before SQLite import, backup existing database and start fresh.

4. **Should the cleanup script run automatically before report generation?**
   - **Answer: Yes.** Integrated into report generation pipeline (likely only needs to run once to clean existing data).

5. **For the scoring formula, should lower scores be better (like golf) or higher scores be better?**
   - **Answer: Lower is better** (like golf). Score of 1.0 = matches C baseline. Score of 0.5 = twice as fast as C.

## Open Questions

1. Should variant comparison in modal show side-by-side or overlay charts?

---

## Appendix: Proposed Scoring Formula

### Scoring Philosophy: Lower is Better (Golf Style)

A score of **1.0 = matches C baseline**. Scores below 1.0 are better than C; scores above 1.0 are worse.

### Category Calculations (all normalized against C baseline)

Each category produces a ratio where lower = better:

```
Speed Score = geometric_mean(
  time / C_time,
  (cpu_user + cpu_sys) / (C_cpu_user + C_cpu_sys)
)
// Example: If language is 2x faster than C, Speed Score = 0.5

Memory Score = geometric_mean(
  memory / C_memory,
  (page_faults_major + page_faults_minor + 1) / (C_page_faults_major + C_page_faults_minor + 1)
)
// Example: If language uses half the memory of C, Memory Score = 0.5

Efficiency Score = geometric_mean(
  iterations / C_iterations,
  (ctx_vol + ctx_invol + 1) / (C_ctx_vol + C_ctx_invol + 1)
)
// Note: Iterations should always be identical if algorithm is correct

I/O Score = (io_in + io_out + 1) / (C_io_in + C_io_out + 1)
// +1 to handle zeros gracefully
```

### Composite Score (Weighted Geometric Mean)
```
Composite = (Speed^0.4 × Memory^0.3 × Efficiency^0.2 × I/O^0.1)

// Weights reflect typical importance:
// - Speed: 40% (primary concern for benchmarks)
// - Memory: 30% (critical for resource-constrained environments)
// - Efficiency: 20% (algorithmic correctness verification)
// - I/O: 10% (usually minimal for CPU-bound workload)
```

### Tier Assignment (lower score = better)
| Score Range | Tier | Label | Description |
|-------------|------|-------|-------------|
| < 0.50 | S | Elite | Significantly outperforms C |
| 0.50 - 0.85 | A | Excellent | Better than C |
| 0.85 - 1.15 | B | Baseline | On par with C |
| 1.15 - 2.00 | C | Acceptable | Moderately slower than C |
| 2.00 - 5.00 | D | Slow | Significantly slower than C |
| > 5.00 | F | Poor | Much slower than C |

### Example Calculations

**Rust (hypothetical):**
- Speed: 0.95 (5% faster than C)
- Memory: 1.10 (10% more memory than C)
- Efficiency: 1.00 (same iterations)
- I/O: 1.00 (same I/O)
- Composite = (0.95^0.4 × 1.10^0.3 × 1.00^0.2 × 1.00^0.1) = 0.98 → **Tier B**

**Python (hypothetical):**
- Speed: 50.0 (50x slower than C)
- Memory: 5.0 (5x more memory than C)
- Efficiency: 1.00 (same iterations)
- I/O: 1.00 (same I/O)
- Composite = (50^0.4 × 5^0.3 × 1^0.2 × 1^0.1) = 8.9 → **Tier F**
