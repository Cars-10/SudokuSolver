# PRD: Language Verification System

## Introduction

Add a comprehensive verification system to validate that all 88+ language implementations produce correct results. The system verifies iteration counts match the C reference, output format compliance, and that solved puzzles are valid Sudoku boards. Verification can run on-demand via CLI or automatically after each benchmark run.

## Goals

- Verify all language implementations produce correct iteration counts matching the C reference
- Validate output format compliance (expected structure with puzzle display and iteration output)
- Confirm solved Sudoku boards are mathematically valid (no duplicates in rows/columns/boxes)
- Provide detailed verification reports (console + JSON/HTML)
- Integrate seamlessly with existing benchmark workflow (automatic + on-demand)
- Support verification of all 88+ languages regardless of completion status

## Reference Iteration Counts

The C implementation serves as the reference. Current reference values (from `Languages/C/metrics.json`):

| Matrix | Reference Iterations |
|--------|---------------------|
| 1      | 656                 |
| 2      | 439,269             |
| 3      | 98,847              |
| 4      | 9,085               |
| 5      | 445,778             |
| 6      | 622,577,597         |

## User Stories

### US-001: Create reference iteration counts configuration
**Description:** As a developer, I need a single source of truth for reference iteration counts so verification can compare against known-correct values.

**Acceptance Criteria:**
- [ ] Create `verification_config.json` with reference iteration counts for all 6 matrices
- [ ] Reference counts sourced from C implementation metrics
- [ ] Config includes tolerance settings (exact match required by default)
- [ ] Typecheck/lint passes

---

### US-002: Implement iteration count verification
**Description:** As a developer, I want to verify that each language's iteration count matches the C reference so I can detect algorithm implementation errors.

**Acceptance Criteria:**
- [ ] Parse `metrics.json` from each language directory
- [ ] Compare iterations against reference for each matrix
- [ ] Report PASS/FAIL per matrix per language
- [ ] Handle missing metrics files gracefully (report as "not run")
- [ ] Handle languages with partial matrix coverage (only verify what exists)

---

### US-003: Implement output format verification
**Description:** As a developer, I want to verify output format compliance so I can ensure all implementations follow the standard output structure.

**Acceptance Criteria:**
- [ ] Verify output contains "Puzzle:" headers (initial and solved)
- [ ] Verify output contains "Solved in Iterations=NNN" line
- [ ] Verify output contains "Seconds to process X.XXX" line
- [ ] Verify 9x9 grid format with space-separated digits
- [ ] Report specific format violations per language

---

### US-004: Implement Sudoku board validity verification
**Description:** As a developer, I want to verify solved boards are valid Sudoku solutions so I can catch solver bugs that produce invalid solutions.

**Acceptance Criteria:**
- [ ] Extract solved puzzle grid from output
- [ ] Verify all rows contain digits 1-9 with no duplicates
- [ ] Verify all columns contain digits 1-9 with no duplicates
- [ ] Verify all 3x3 boxes contain digits 1-9 with no duplicates
- [ ] Verify no zeros remain in solved puzzle
- [ ] Report specific validity errors (e.g., "Row 3 has duplicate 7")

---

### US-005: Create verification CLI command
**Description:** As a user, I want to run verification on-demand via `./runBenchmarks.sh --verify` so I can check correctness at any time.

**Acceptance Criteria:**
- [ ] Add `--verify` flag to `runBenchmarks.sh`
- [ ] Run verification on all 88+ languages when invoked
- [ ] Display progress during verification (language by language)
- [ ] Output summary table showing PASS/FAIL per language
- [ ] Exit with non-zero code if any verification fails

---

### US-006: Integrate verification into benchmark runs
**Description:** As a user, I want verification to run automatically after each benchmark so I get immediate feedback on correctness.

**Acceptance Criteria:**
- [ ] After `run_benchmarks()` completes, automatically run verification
- [ ] Verification runs on the language(s) just benchmarked
- [ ] Display verification results in console output
- [ ] Write verification status to metrics.json alongside benchmark results
- [ ] Do not block metrics file creation on verification failure (just record status)

---

### US-007: Generate JSON verification report
**Description:** As a developer, I want a detailed JSON verification report so I can programmatically analyze results or integrate with CI.

**Acceptance Criteria:**
- [ ] Create `verification_report.json` in project root
- [ ] Include per-language verification results with:
  - Overall status (pass/fail/not_run)
  - Per-matrix iteration check results
  - Format compliance results
  - Board validity results
  - Specific error messages for failures
- [ ] Include summary statistics (total, passed, failed, not_run)
- [ ] Include timestamp and verification config used

---

### US-008: Generate HTML verification report
**Description:** As a user, I want an HTML verification report so I can visually review results in a browser.

**Acceptance Criteria:**
- [ ] Create `verification_report.html` in project root
- [ ] Display summary dashboard with pass/fail counts
- [ ] Color-coded table: green=pass, red=fail, gray=not_run
- [ ] Expandable details showing specific failures per language
- [ ] Sortable/filterable columns (by status, language name)
- [ ] Link to existing benchmark report for context

---

### US-009: Handle verification errors gracefully
**Description:** As a developer, I want verification to handle edge cases gracefully so it doesn't crash on unexpected input.

**Acceptance Criteria:**
- [ ] Handle missing `metrics.json` files (status: "not_run")
- [ ] Handle malformed JSON in metrics files (status: "error", include parse error)
- [ ] Handle missing output field in results (status: "error")
- [ ] Handle timeout/error status in benchmark results (skip verification, note status)
- [ ] Continue verification for remaining languages on individual failures

---

### US-010: Add verification status to metrics.json schema
**Description:** As a developer, I need to extend the metrics schema to include verification results so they're persisted with benchmark data.

**Acceptance Criteria:**
- [ ] Add `verification` object to each result in metrics.json
- [ ] Include `iterations_match: boolean`
- [ ] Include `format_valid: boolean`
- [ ] Include `solution_valid: boolean`
- [ ] Include `errors: string[]` for failure details
- [ ] Update TypeScript types in `Metrics/types.ts`

## Functional Requirements

- FR-1: Create `verification_config.json` storing reference iteration counts for matrices 1-6, sourced from C implementation
- FR-2: Implement `verify_language(language_dir)` function that checks iteration counts, output format, and solution validity
- FR-3: Implement `verify_iterations(actual, expected, matrix)` returning pass/fail with tolerance support
- FR-4: Implement `verify_output_format(output)` checking for required sections and structure
- FR-5: Implement `verify_sudoku_solution(grid)` validating rows, columns, and 3x3 boxes
- FR-6: Add `--verify` flag to `runBenchmarks.sh` that runs verification on all languages
- FR-7: Integrate verification into `common.sh` to run automatically after benchmark completion
- FR-8: Generate `verification_report.json` with detailed per-language results
- FR-9: Generate `verification_report.html` with visual summary and drill-down details
- FR-10: Add `verification` object to metrics.json result schema
- FR-11: Update TypeScript types to include verification fields
- FR-12: Display console summary with pass/fail counts and failed language list

## Non-Goals

- No performance sanity checks (e.g., flagging abnormally slow implementations)
- No automatic fixing of incorrect implementations
- No verification of intermediate solving steps (only final result)
- No support for alternative algorithm implementations (all must match C reference exactly)
- No parallel verification execution (run sequentially for simplicity)
- No historical verification tracking (each run is independent)

## Technical Considerations

- **Language:** Verification logic in Python (available in Docker environment, used by `common.sh`)
- **Integration Point:** Add to `common.sh` for automatic post-benchmark verification
- **Report Generation:** Extend existing `Metrics/HTMLGenerator.ts` or create separate verification report generator
- **Reference Data:** C implementation is authoritative; update reference counts if C algorithm changes
- **File Locations:**
  - Config: `verification_config.json` (project root)
  - Reports: `verification_report.json`, `verification_report.html` (project root)
  - Per-language: Verification results embedded in `metrics.json`

## Success Metrics

- All 88+ languages can be verified in a single command
- Verification correctly identifies languages with incorrect iteration counts
- Verification correctly identifies invalid Sudoku solutions
- Verification completes for all languages without crashing
- Reports clearly communicate which languages pass/fail and why

## Open Questions

1. Should we store historical verification results to track improvements over time?
2. Should the HTML report be integrated into the existing benchmark report or remain separate?
3. Should we add a `--verify-only` flag that skips benchmarking entirely?
4. How should we handle languages that intentionally use different algorithms (if any exist)?
