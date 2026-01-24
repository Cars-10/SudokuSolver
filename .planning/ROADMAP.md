# Roadmap: Sudoku Solver Benchmark

## Milestones

- v1.0 Scoring & UI - Phases 1-3 (shipped 2026-01-20)
- v2.0 Metadata Enrichment - (shipped 2026-01-20, retroactive)
- v3.0 Quality & Insights - Phases 4-7 (in progress)

## Phases

<details>
<summary>v1.0 Scoring & UI (Phases 1-3) - SHIPPED 2026-01-20</summary>

**Milestone Goal:** Scoring engine, UI cleanup, and metadata alignment.

**Key accomplishments:**
- Weighted geometric mean scoring (80/20)
- UI cleanup (Run buttons removed)
- Robust Matrix Race fullscreen (screenfull)
- Aligned metadata & documentation

**Stats:** ~10 files modified, ~7700 LOC TypeScript, 3 phases, ~5 plans, ~33 days

</details>

<details>
<summary>v2.0 Metadata Enrichment - SHIPPED 2026-01-20</summary>

**Milestone Goal:** Wikipedia-powered metadata automation and language documentation enrichment.

**Key accomplishments:**
- Wikipedia metadata scraper (fetch_metadata.py)
- Enhanced LanguagesMetadata.ts with encyclopedic descriptions
- Author attribution system with biographical metadata
- Media cleanup (removed 200+ obsolete images)
- Python tooling infrastructure

**Stats:** 231 files changed (+1,355 / -1,567 lines), ~1,247 LOC Python/JavaScript, completed in 1 day

</details>

### v3.0 Quality & Insights (In Progress)

**Milestone Goal:** Ensure algorithmic correctness through enhanced validation, determine optimal scoring methodology through data analysis, and provide richer insights through improved visualizations.

**Phase Numbering:**
- Integer phases (4, 5, 6, 7): Planned milestone work
- Decimal phases (5.1, 5.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [x] **Phase 4: Validation Infrastructure** - Algorithmic correctness foundation
- [x] **Phase 5: Scoring Analysis** - Statistical rigor and methodology optimization
- [x] **Phase 6: Visualization & UI** - Enhanced insights and polish
- [ ] **Phase 7: Interactive Solver** - Engaging user experience

## Phase Details

### Phase 4: Validation Infrastructure
**Goal:** Establish credibility foundation through algorithmic correctness validation at benchmark execution time, ensuring iteration counts match C reference and solutions satisfy Sudoku constraints, with fail-fast error handling and benchmark_issues.json logging.

**Depends on:** Nothing (first phase of v3.0)

**Requirements:** VAL-01, VAL-02, VAL-03, VAL-06 (backend validation only)

**Note:** VAL-04 (visual warning badges) and VAL-05 (diagnostics modal) are UI-layer concerns deferred to Phase 6 per user decision in 04-CONTEXT.md: "NO new UI elements in report--validation results only appear in benchmark_issues.json log".

**Implementation Scope:**
This phase adds validation logic to the benchmark execution pipeline (common.sh) with transparent integration into all language implementations via run_matrix().

**Key components:**
1. **Validation functions in common.sh** - detect_algorithm_type, get_reference_iterations, validate_iteration_count, validate_solution, write_validation_failure
2. **Integration in run_matrix()** - Validation executes after output extraction, before metrics write
3. **Fail-fast behavior** - Validation failure exits immediately with error code 1
4. **Logging to benchmark_issues.json** - All validation failures logged with timestamp, severity, details

**Success Criteria** (what must be TRUE):
1. common.sh validates iteration count against C reference (656 for matrix 1) with exact match for BruteForce, +/-1 for DLX/CP
2. common.sh validates solution correctness (9x9 Sudoku constraints satisfied) during benchmark execution
3. Validation failure stops benchmark immediately (fail-fast) and logs to benchmark_issues.json
4. C reference implementation passes all validation (it defines correctness)
5. No changes required to any runMe.sh scripts (transparent integration)

**Plans:** 1 plan

Plans:
- [x] 04-01-PLAN.md - Add validation functions to common.sh and integrate into run_matrix()

### Phase 5: Scoring Analysis
**Goal:** Provide statistical rigor through sensitivity analysis, correlation computation, and versioned scoring to validate current methodology and enable informed future improvements.

**Depends on:** Phase 4 (needs validated data for accurate scoring analysis)

**Requirements:** SCORE-01, SCORE-02, SCORE-03, SCORE-04, SCORE-05, SCORE-06, SCORE-07

**Implementation Scope:**
This phase creates a scoring analysis module (Metrics/scoring-analysis.ts) with statistical functions, then integrates them into HTMLGenerator.ts with interactive UI elements including stacked bar score decomposition, expandable sensitivity rows, and summary insight cards.

**Key components:**
1. **Analysis Module (scoring-analysis.ts)** - calculateSensitivityScores, calculateRankStability, computeCorrelation, identifyOutliers, calculatePercentiles
2. **UI Integration (HTMLGenerator.ts)** - Score decomposition stacked bars, expandable sensitivity rows, Scoring Insights summary section
3. **Dependency** - simple-statistics library for statistical calculations (correlation, quartiles, IQR)

**Success Criteria** (what must be TRUE):
1. System performs sensitivity analysis across weight scenarios (time-only, 80/20, 50/50, memory-only)
2. Report shows score decomposition view (time 80% + memory 20% contributions visible)
3. System calculates rank stability per language across weight scenarios
4. Report displays correlation analysis (R^2 for time vs memory relationship)
5. System identifies and flags statistical outliers with analysis
6. Scoring methodology changes preserve historical comparability through versioning

**Plans:** 2 plans

Plans:
- [x] 05-01-PLAN.md - Create scoring analysis module with sensitivity, correlation, and outlier functions
- [x] 05-02-PLAN.md - Integrate analysis into HTMLGenerator.ts with interactive UI components

### Phase 6: Visualization & UI
**Goal:** Reveal performance patterns through advanced chart types while fixing UI bugs to provide polished, insightful visual experience.

**Depends on:** Phase 4 + Phase 5 (visualizes validation results and scoring insights)

**Requirements:** VAL-04, VAL-05, VIZ-01, VIZ-02, VIZ-03, UI-01, UI-02, UI-03

**Note:** VAL-04 and VAL-05 are validation UI visualization (deferred from Phase 4). VIZ-04, VIZ-05, VIZ-06, UI-04 deferred to future phases.

**Implementation Scope:**
This phase fixes UI bugs (dropdown sorting, fullscreen exit), adds three advanced D3.js visualizations (scatter plot, heatmap, histogram), and surfaces validation results through warning badges and diagnostics modal.

**Key components:**
1. **UI Bug Fixes (06-01)** - Algorithm dropdown alphabetical sorting, Matrix Race fullscreen exit loop fix
2. **Advanced Visualizations (06-02)** - Time vs Memory scatter plot with log scale, Language x Matrix heatmap with click-to-detail, Score distribution histogram with percentile markers
3. **Validation UI (06-03)** - Warning badges on invalid implementations, diagnostics modal showing iteration mismatch details
4. **Gap Closure (06-04)** - Wire validation styling to scatter plot and heatmap charts

**Success Criteria** (what must be TRUE):
1. Report includes scatter plot showing Time vs Memory with logarithmic scales
2. Report includes heatmap showing Language x Matrix performance patterns
3. Report includes distribution histogram showing score clusters/tiers
4. Algorithm dropdown sorts options alphabetically and updates labels correctly
5. Matrix Race fullscreen exit works correctly without temporary exit/re-enter bug
6. All visualizations handle 70+ languages and 6 orders of magnitude without misleading scales
7. Report displays visual warning badges for invalid implementations (VAL-04)
8. Diagnostics modal shows iteration mismatch details (VAL-05)
9. Invalid implementations visually distinguished in scatter plot and heatmap charts

**Plans:** 4 plans

Plans:
- [x] 06-01-PLAN.md - Fix UI bugs: algorithm dropdown sorting and Matrix Race fullscreen exit
- [x] 06-02-PLAN.md - Add advanced visualizations: scatter plot, heatmap, histogram
- [x] 06-03-PLAN.md - Add validation UI: warning badges and diagnostics modal
- [x] 06-04-PLAN.md - Wire validation styling to scatter plot and heatmap charts (gap closure)

### Phase 7: Interactive Solver
**Goal:** Provide engaging, visually entertaining solver animation with Neon/Matrix theme styling for interactive user exploration.

**Depends on:** Phase 6 (UI foundation and visualization patterns established)

**Requirements:** INT-01, INT-02, INT-03, INT-04, INT-05

**Implementation Scope:**
This phase creates a browser-based interactive solver with animated visualization. Users select a matrix and algorithm, then watch the solving process with step-by-step animation, speed control, and visual effects.

**Key components:**
1. **Solver Engine (07-01)** - Browser-compatible BruteForce solver with state emission, immutable state history with 10K limit
2. **Visual Layer (07-02)** - 3D CSS grid with neon glow, spin animations, glitch effects (screen shake, alien scramble)
3. **Animation + Controls (07-03)** - requestAnimationFrame loop with speed control (1x-100x), playback controls UI
4. **Integration (07-04)** - Dedicated tab in report, matrix/algorithm selection, full orchestration

**Success Criteria** (what must be TRUE):
1. User can select matrix and algorithm to run interactive solver in browser
2. Interactive solver runs using JavaScript implementation without server calls
3. Interactive solver provides visually entertaining animation with alien glitch effect
4. Visual styling matches Neon and Matrix theme consistently
5. Solver animation spins letters along vertical axis during backtracking

**Plans:** 4 plans

Plans:
- [ ] 07-01-PLAN.md - Create browser solver engine with state emission and history management
- [ ] 07-02-PLAN.md - Create 3D CSS grid with neon styling and glitch effects
- [ ] 07-03-PLAN.md - Create animation controller with speed control and playback UI
- [ ] 07-04-PLAN.md - Integrate into HTMLGenerator with dedicated tab and human verification

## Progress

**Execution Order:**
Phases execute in numeric order: 4 -> 5 -> 6 -> 7
(Decimal phases like 5.1 would execute between 5 and 6)

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Scoring Engine | v1.0 | 1/1 | Complete | 2026-01-20 |
| 2. UI/UX Refine | v1.0 | 2/2 | Complete | 2026-01-20 |
| 3. Metadata Align | v1.0 | 1/1 | Complete | 2026-01-20 |
| 4. Validation Infrastructure | v3.0 | 1/1 | Complete | 2026-01-23 |
| 5. Scoring Analysis | v3.0 | 2/2 | Complete | 2026-01-23 |
| 6. Visualization & UI | v3.0 | 4/4 | Complete | 2026-01-24 |
| 7. Interactive Solver | v3.0 | 0/4 | Not started | - |

---
*Last updated: 2026-01-24 after Phase 7 planning*
