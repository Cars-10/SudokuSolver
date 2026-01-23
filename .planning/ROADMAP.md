# Roadmap: Sudoku Solver Benchmark

## Milestones

- ✅ **v1.0 Scoring & UI** - Phases 1-3 (shipped 2026-01-20)
- ✅ **v2.0 Metadata Enrichment** - (shipped 2026-01-20, retroactive)
- 🚧 **v3.0 Quality & Insights** - Phases 4-7 (in progress)

## Phases

<details>
<summary>✅ v1.0 Scoring & UI (Phases 1-3) - SHIPPED 2026-01-20</summary>

**Milestone Goal:** Scoring engine, UI cleanup, and metadata alignment.

**Key accomplishments:**
- Weighted geometric mean scoring (80/20)
- UI cleanup (Run buttons removed)
- Robust Matrix Race fullscreen (screenfull)
- Aligned metadata & documentation

**Stats:** ~10 files modified, ~7700 LOC TypeScript, 3 phases, ~5 plans, ~33 days

</details>

<details>
<summary>✅ v2.0 Metadata Enrichment - SHIPPED 2026-01-20</summary>

**Milestone Goal:** Wikipedia-powered metadata automation and language documentation enrichment.

**Key accomplishments:**
- Wikipedia metadata scraper (fetch_metadata.py)
- Enhanced LanguagesMetadata.ts with encyclopedic descriptions
- Author attribution system with biographical metadata
- Media cleanup (removed 200+ obsolete images)
- Python tooling infrastructure

**Stats:** 231 files changed (+1,355 / -1,567 lines), ~1,247 LOC Python/JavaScript, completed in 1 day

</details>

### 🚧 v3.0 Quality & Insights (In Progress)

**Milestone Goal:** Ensure algorithmic correctness through enhanced validation, determine optimal scoring methodology through data analysis, and provide richer insights through improved visualizations.

**Phase Numbering:**
- Integer phases (4, 5, 6, 7): Planned milestone work
- Decimal phases (5.1, 5.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [ ] **Phase 4: Validation Infrastructure** - Algorithmic correctness foundation
- [ ] **Phase 5: Scoring Analysis** - Statistical rigor and methodology optimization
- [ ] **Phase 6: Visualization & UI** - Enhanced insights and polish
- [ ] **Phase 7: Interactive Solver** - Engaging user experience

## Phase Details

### Phase 4: Validation Infrastructure
**Goal:** Establish credibility foundation through algorithmic correctness validation at benchmark execution time, ensuring iteration counts match C reference and solutions satisfy Sudoku constraints, with results captured in metrics.json and displayed in reports.

**Depends on:** Nothing (first phase of v3.0)

**Requirements:** VAL-01, VAL-02, VAL-03, VAL-04, VAL-05, VAL-06

**Implementation Scope:**
This phase adds validation logic to the benchmark execution pipeline (common.sh) and integrates it into all language implementations (runMe.sh scripts), not just report-time validation.

**Key components:**
1. **Validation logic in common.sh** - Executes during benchmark runs to validate iteration counts and solution correctness
2. **Integration points in runMe.sh** - Each language's runMe.sh script calls validation functions from common.sh
3. **Metrics capture** - Validation results stored in metrics.json during execution
4. **Report display** - HTMLGenerator.ts reads validation data from metrics.json and displays warnings/badges

**Success Criteria** (what must be TRUE):
1. common.sh validates iteration count against C reference (656 for matrix 1) with ±1 tolerance during benchmark execution
2. common.sh validates solution correctness (9×9 Sudoku constraints satisfied) during benchmark execution
3. Validation results (pass/fail, delta, severity) are captured in metrics.json for each language/matrix combination
4. Report displays visual warning badges for implementations with iteration mismatches (read from metrics.json)
5. Diagnostics modal shows iteration mismatch details with severity categorization (read from metrics.json)
6. Invalid implementations are flagged without blocking valid results (fail-late pattern at execution time)

**Plans:** TBD (estimated 2-3 plans)

Plans:
- [ ] 04-01: TBD during planning
- [ ] 04-02: TBD during planning

### Phase 5: Scoring Analysis
**Goal:** Provide statistical rigor through sensitivity analysis, correlation computation, and versioned scoring to validate current methodology and enable informed future improvements.

**Depends on:** Phase 4 (needs validated data for accurate scoring analysis)

**Requirements:** SCORE-01, SCORE-02, SCORE-03, SCORE-04, SCORE-05, SCORE-06, SCORE-07

**Success Criteria** (what must be TRUE):
1. System performs sensitivity analysis across weight scenarios (time-only, 80/20, 50/50, memory-only)
2. Report shows score decomposition view (time 80% + memory 20% contributions visible)
3. System calculates rank stability per language across weight scenarios
4. Report displays correlation analysis (R² for time vs memory relationship)
5. System identifies and flags statistical outliers with analysis
6. Scoring methodology changes preserve historical comparability through versioning

**Plans:** TBD (estimated 2-3 plans)

Plans:
- [ ] 05-01: TBD during planning
- [ ] 05-02: TBD during planning

### Phase 6: Visualization & UI
**Goal:** Reveal performance patterns through advanced chart types while fixing UI bugs to provide polished, insightful visual experience.

**Depends on:** Phase 4 + Phase 5 (visualizes validation results and scoring insights)

**Requirements:** VIZ-01, VIZ-02, VIZ-03, VIZ-04, VIZ-05, VIZ-06, UI-01, UI-02, UI-03, UI-04

**Success Criteria** (what must be TRUE):
1. Report includes scatter plot showing Time vs Memory with logarithmic scales
2. Report includes heatmap showing Language × Matrix performance patterns
3. Report includes distribution histogram showing score clusters/tiers
4. Algorithm dropdown sorts options alphabetically and updates labels correctly
5. Matrix Race fullscreen exit works correctly without temporary exit/re-enter bug
6. All visualizations handle 70+ languages and 6 orders of magnitude without misleading scales

**Plans:** TBD (estimated 3-4 plans)

Plans:
- [ ] 06-01: TBD during planning
- [ ] 06-02: TBD during planning
- [ ] 06-03: TBD during planning

### Phase 7: Interactive Solver
**Goal:** Provide engaging, visually entertaining solver animation with Neon/Matrix theme styling for interactive user exploration.

**Depends on:** Phase 6 (UI foundation and visualization patterns established)

**Requirements:** INT-01, INT-02, INT-03, INT-04, INT-05

**Success Criteria** (what must be TRUE):
1. User can select matrix and algorithm to run interactive solver in browser
2. Interactive solver runs using JavaScript implementation without server calls
3. Interactive solver provides visually entertaining animation with alien glitch effect
4. Visual styling matches Neon and Matrix theme consistently
5. Solver animation spins letters along vertical axis during backtracking

**Plans:** TBD (estimated 2 plans)

Plans:
- [ ] 07-01: TBD during planning
- [ ] 07-02: TBD during planning

## Progress

**Execution Order:**
Phases execute in numeric order: 4 → 5 → 6 → 7
(Decimal phases like 5.1 would execute between 5 and 6)

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Scoring Engine | v1.0 | 1/1 | Complete | 2026-01-20 |
| 2. UI/UX Refine | v1.0 | 2/2 | Complete | 2026-01-20 |
| 3. Metadata Align | v1.0 | 1/1 | Complete | 2026-01-20 |
| 4. Validation Infrastructure | v3.0 | 0/2 | Not started | - |
| 5. Scoring Analysis | v3.0 | 0/2 | Not started | - |
| 6. Visualization & UI | v3.0 | 0/3 | Not started | - |
| 7. Interactive Solver | v3.0 | 0/2 | Not started | - |

---
*Last updated: 2026-01-23 after Phase 4 revision (validation infrastructure scope clarification)*
