# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-01-23)

**Core value:** Providing a fair, transparent, and visually engaging comparison of programming language performance using a standardized algorithmic implementation.

**Current focus:** Phase 6 - Visualization & UI

## Current Position

Phase: 6 of 7 (Visualization & UI)
Plan: 3 of ~3 in current phase
Status: In progress
Last activity: 2026-01-24 — Completed 06-03-PLAN.md (Validation UI integration)

Progress: [████████░░] 83% (10 of 12 estimated total plans)

## Performance Metrics

**Velocity:**
- Total plans completed: 8 (v1.0: 4, v3.0: 4)
- Average duration: ~5.5 days per plan (v1.0 avg), 3.5 minutes (v3.0 avg)
- Total execution time: ~33 days (v1.0), 14.5 minutes (v3.0)

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 1. Scoring Engine | 1 | ~10 days | ~10 days |
| 2. UI/UX Refine | 2 | ~15 days | ~7.5 days |
| 3. Metadata Align | 1 | ~8 days | ~8 days |
| 4. Validation Infra | 1 | 2 min | 2 min |
| 5. Scoring Analysis | 2 | 9 min | 4.5 min |
| 6. Visualization & UI | 3 | 12 min | 4 min |

**Recent Trend:**
- v1.0 completed in 33 days (manual exploration)
- v2.0 completed in 1 day (retroactive metadata work)
- v3.0 Phase 4 completed in 2 minutes (autonomous execution)
- v3.0 Phase 5 completed in 9 minutes (autonomous execution, 2 plans)
- v3.0 Phase 6 in progress: 12 minutes (3 plans: 06-01 UI fixes, 06-02 advanced viz, 06-03 validation UI)
- Trend: Sustained velocity with autonomous execution - averaging 3-4 minutes per plan

*Updated after 06-03 completion*

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- Local Execution Focus: User specified ignoring docker commands for now to focus on local refinements
- Wikipedia Integration: Automate metadata extraction using Wikipedia API (v2.0)
- Media Cleanup: Remove obsolete timestamped images, keeping only curated/logo images (v2.0)
- Phase 4 Scope Clarification (2026-01-23): Validation infrastructure includes common.sh execution logic and runMe.sh integration points, not just report-time HTMLGenerator.ts validation
- **04-01 Validation Timing (2026-01-23)**: Validate during execution (not report generation) for immediate feedback - prevents invalid metrics from being written
- **04-01 Algorithm-specific Tolerance (2026-01-23)**: BruteForce requires exact iteration match; DLX/CP allow +/-1 tolerance for non-deterministic algorithm choices
- **04-01 Severity Categorization (2026-01-23)**: CRITICAL for solution_invalid or >1 iteration delta; WARNING for +/-1 delta to prioritize fixes
- **05-01 Outlier Detection Method (2026-01-23)**: IQR method (Q1-1.5×IQR, Q3+1.5×IQR) chosen over Z-score for robustness to skewed distributions - benchmark has 6 orders of magnitude variation
- **05-01 Correlation Interpretation (2026-01-23)**: R² thresholds: very strong ≥0.8, moderate ≥0.5, weak ≥0.3 - accessible to non-statisticians while maintaining accuracy
- **05-01 Weight Scenarios (2026-01-23)**: Four scenarios (Time Only 100/0, Current 80/20, Balanced 50/50, Memory Only 0/100) capture spectrum for sensitivity analysis
- **05-02 Stacked Bar Visualization (2026-01-23)**: 80% blue (time) + 20% orange (memory) inline stacked bars make 80/20 weighting transparent at a glance
- **05-02 Expandable Row Interaction (2026-01-23)**: Inline expandable rows with expand indicator (▼) in total time column - reduces cognitive load vs separate table
- **05-02 Special Character Normalization (2026-01-23)**: Consistent C_Sharp→C#, F_Sharp→F# handling across toggleRow, populateSensitivityRow, showScoreTooltip functions
- **06-01 Chart Selector Sorting (2026-01-24)**: Alphabetical sorting by display text (not value) for chart selector dropdown - predictable UX
- **06-01 Fullscreen Exit Guard (2026-01-24)**: Surgical guard for Matrix Race fullscreen to prevent exit/re-enter loop - check currentChart before redraw
- **06-03 Validation Data Embedding (2026-01-24)**: Embed benchmark_issues.json as window.validationIssues for client-side badge/modal rendering - avoids server-side complexity
- **06-03 Separate Validation Modal (2026-01-24)**: Distinct modal for validation diagnostics vs runtime diagnostics - prevents confusion between algorithm errors and environment issues

### Pending Todos

None yet (v3.0 just started).

### Blockers/Concerns

**Phase 4 considerations:**
- ✅ Validation logic added to common.sh (execution stage) - COMPLETED 04-01
- ✅ Transparent integration via sourced common.sh - all runMe.sh scripts automatically inherit validation - COMPLETED 04-01
- ✅ Iteration count tolerance established: BruteForce exact, DLX/CP +/-1 - COMPLETED 04-01
- Memory measurement reliability (RSS variance across OS/runtime) - monitoring in 04-02
- Expect validation failures across 88+ implementations (goal is to find broken implementations) - will address in 04-02

**Phase 5 considerations:**
- ✅ Statistical analysis module complete with IQR outlier detection - COMPLETED 05-01
- ✅ Sensitivity analysis across 4 weight scenarios implemented - COMPLETED 05-01
- ✅ Rank stability metrics (max swing) calculation ready - COMPLETED 05-01
- ✅ R² correlation with plain-English interpretation - COMPLETED 05-01
- ✅ Integration of analysis functions into HTMLGenerator.ts - COMPLETED 05-02
- ✅ UI design: expandable rows for sensitivity data, mini stacked bars for score decomposition - COMPLETED 05-02
- ✅ Summary insight sections for rank stability, correlation, and outliers - COMPLETED 05-02
- Data integrity issue: One metric file has undefined solver (handled defensively in 3 locations)

**Phase 6 considerations:**
- ✅ UI bugs fixed: alphabetical dropdown, Matrix Race fullscreen exit - COMPLETED 06-01
- ✅ Advanced visualizations added: scatter plot, heatmap, histogram - COMPLETED 06-02
- ✅ Validation UI elements: badges and diagnostics modal - COMPLETED 06-03
- ✅ Logarithmic scaling and language filtering implemented - COMPLETED 06-02
- Phase 6 Wave 1 and Wave 2 complete
- May want to address data integrity issue (undefined solver metric) at source

## Session Continuity

Last session: 2026-01-24
Stopped at: Completed 06-03-PLAN.md — Validation UI integration (badges, diagnostics modal)
Resume file: None

**Next steps:**
1. Phase 6 Wave 1 and Wave 2 complete (06-01 UI fixes, 06-02 advanced viz, 06-03 validation UI)
2. Phase 6 appears complete - all planned visualizations and UI improvements delivered
3. Ready for Phase 7: Cleanup & Polish
   - Address data integrity issue (undefined solver metric)
   - Final polish and refinement
   - Documentation updates
