# PRD: Benchmark History Report Cleanup and UI Refinement

## Introduction
The current benchmarking reporting system consists of fragmented scripts and inconsistent UI designs between the single-run report (`benchmark_report.html`) and the history report (`benchmark_history.html`). This initiative aims to clean up the codebase by removing legacy "simple" JavaScript files and unifying the visual design of both reports to match the project's "Red Pill/Neon" aesthetic, ensuring a consistent user experience and easier maintenance.

## Goals
- **Remove Legacy Code:** Delete redundant/simple generation scripts to reduce noise.
- **Unify Design:** Ensure `benchmark_history.html` and `benchmark_report.html` share the same visual language (CSS, layout, headers).
- **Consolidate Logic:** Refactor generation logic so shared components (tables, headers) are defined in one place.
- **Maintain Functionality:** Ensure both reports remain fully functional after cleanup.

## User Stories

### US-001: Remove legacy generation scripts
**Description:** As a developer, I want to remove unused and "simple" versions of the report generators to clean up the workspace.

**Acceptance Criteria:**
- [ ] Delete `Metrics/generate_report_simple.js`
- [ ] Delete `Metrics/generate_report_only.js` (if fully superseded by TS version)
- [ ] Update `package.json` or shell scripts (`runBenchmarks.sh`, `common.sh`) to ensure they no longer reference deleted files
- [ ] Typecheck passes

### US-002: Extract shared CSS and Design Constants
**Description:** As a developer, I want a single source of truth for the "Neon" styling to ensure consistency across both reports.

**Acceptance Criteria:**
- [ ] Create `Metrics/SharedStyles.ts` (or similar) to export the CSS string used in reports.
- [ ] Move existing CSS from `HTMLGenerator.ts` into this shared module.
- [ ] Ensure the CSS includes all necessary classes for both history and single-run views.
- [ ] Typecheck passes

### US-003: Update History Report to use Shared Styles
**Description:** As a user, I want the History Report to look like the main Benchmark Report so the UI feels cohesive.

**Acceptance Criteria:**
- [ ] Update `HistoryManager.ts` (or the relevant history generator) to import and use the CSS from `SharedStyles.ts`.
- [ ] Update the HTML structure of the History Report to match the class names/layout of the main report.
- [ ] Verify `benchmark_history.html` renders with the correct Neon theme.
- [ ] Verify in browser using dev-browser skill (open `benchmark_history.html`)
- [ ] Typecheck passes

### US-004: Update Single Run Report to use Shared Styles
**Description:** As a developer, I want the Single Run Report to consume the new shared styles to verify the refactor didn't break it.

**Acceptance Criteria:**
- [ ] Update `HTMLGenerator.ts` to import CSS from `SharedStyles.ts`.
- [ ] Verify `benchmark_report.html` still renders correctly.
- [ ] Verify in browser using dev-browser skill (open `benchmark_report.html`)
- [ ] Typecheck passes

## Functional Requirements
- FR-1: The project must not contain `generate_report_simple.js`.
- FR-2: Both `benchmark_report.html` and `benchmark_history.html` must link to or embed the exact same CSS definitions.
- FR-3: The History report must use the same "Neon" color palette (#0f0, #000, etc.) as the main report.

## Non-Goals
- Adding new charts or data visualizations (focus is on design parity).
- rewriting the database layer.
- Changing the actual data collection logic (common.sh).

## Technical Considerations
- The project uses TypeScript in `Metrics/`.
- Ensure `ts-node` is available or files are compiled before running generation scripts.
- Careful not to break the `runMeGlobal.sh` flow which depends on report generation.

## Success Metrics
- 0 references to `generate_report_simple.js` in the codebase.
- Visual difference between the header/footer of History and Single reports is 0.

## Open Questions
- Do we need to preserve any specific logic from `generate_report_simple.js` that isn't in the TS version? (Assumption: No, it was a backup).
