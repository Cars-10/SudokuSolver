# PRD: Matrix 1 Test Suite for All Languages

## Introduction

Create a standalone test script that runs Matrix 1 (the simplest puzzle with 656 iterations) against all 88+ language implementations in the benchmark suite. This serves as a comprehensive health check to identify broken implementations, establish baseline performance metrics, and verify algorithm correctness. Running in Docker ensures a consistent environment across all languages.

## Goals

- Run Matrix 1 against all language implementations in a single command
- Identify languages with broken/non-working code (compile errors, runtime errors, timeouts)
- Verify iteration count correctness (must equal 656 for Matrix 1)
- Establish baseline performance metrics for comparison
- Log all failures for manual review
- Complete full test suite in reasonable time (~60 seconds per language max)

## User Stories

### US-001: Create test script skeleton
**Description:** As a developer, I want a standalone test script that can iterate through all language directories.

**Acceptance Criteria:**
- [ ] Create `scripts/test_all_matrix1.sh` executable script
- [ ] Script discovers all language directories in `Languages/`
- [ ] Script outputs start/end timestamps
- [ ] Script accepts optional `--language` flag to test single language
- [ ] Script runs without errors when invoked: `./scripts/test_all_matrix1.sh --help`

---

### US-002: Add Docker environment check
**Description:** As a developer, I want the script to verify Docker is available and the benchmark container is running.

**Acceptance Criteria:**
- [ ] Check if Docker is installed and running
- [ ] Check if `sudoku-benchmark` container exists and is running
- [ ] If container not running, display helpful error message with start command
- [ ] Exit with clear error if Docker requirements not met

---

### US-003: Implement single language test function
**Description:** As a developer, I want a function that tests one language against Matrix 1 and captures results.

**Acceptance Criteria:**
- [ ] Create `test_language()` function that runs Matrix 1 for a given language
- [ ] Execute inside Docker container using `docker exec`
- [ ] Capture exit code, stdout, stderr
- [ ] Enforce 60-second timeout per language
- [ ] Parse output to extract iteration count
- [ ] Return structured result: status (pass/fail/timeout/error), iterations, time, error message

---

### US-004: Implement test loop for all languages
**Description:** As a developer, I want the script to test all languages sequentially and track progress.

**Acceptance Criteria:**
- [ ] Loop through all language directories
- [ ] Display progress: "Testing [N/Total]: LanguageName..."
- [ ] Call `test_language()` for each
- [ ] Collect results in arrays for summary
- [ ] Handle interrupts gracefully (Ctrl+C shows partial results)

---

### US-005: Categorize and log failures
**Description:** As a developer, I want failures categorized by type so I can prioritize fixes.

**Acceptance Criteria:**
- [ ] Categorize failures into: compile_error, runtime_error, timeout, wrong_iterations, missing_runme
- [ ] Log each failure with: language name, category, error snippet (first 3 lines)
- [ ] Write failures to `test_results/matrix1_failures.log`
- [ ] Create `test_results/` directory if it doesn't exist

---

### US-006: Generate summary report
**Description:** As a developer, I want a summary showing pass/fail counts and lists of broken languages.

**Acceptance Criteria:**
- [ ] Display summary table at end of run:
  - Total languages tested
  - Passed (correct iterations)
  - Failed by category (compile, runtime, timeout, wrong iterations)
  - Skipped (no runMe.sh)
- [ ] List all failed languages grouped by failure category
- [ ] Display total execution time
- [ ] Write summary to `test_results/matrix1_summary.txt`

---

### US-007: Add JSON output option
**Description:** As a developer, I want machine-readable JSON output for CI integration.

**Acceptance Criteria:**
- [ ] Add `--json` flag to output results as JSON
- [ ] JSON includes: timestamp, total_time, results array with per-language data
- [ ] Each result includes: language, status, iterations, time_seconds, error (if any)
- [ ] Write to `test_results/matrix1_results.json`

---

### US-008: Add verbose and quiet modes
**Description:** As a developer, I want to control output verbosity for different use cases.

**Acceptance Criteria:**
- [ ] Add `--verbose` flag to show full output from each language
- [ ] Add `--quiet` flag to show only summary (no per-language progress)
- [ ] Default mode shows progress line per language
- [ ] Verbose mode shows compilation output and solver output

## Functional Requirements

- FR-1: Script location: `scripts/test_all_matrix1.sh`
- FR-2: Must run inside Docker container `sudoku-benchmark`
- FR-3: Timeout: 60 seconds per language
- FR-4: Reference iteration count for Matrix 1: 656
- FR-5: Output directory: `test_results/`
- FR-6: Exit code 0 if all pass, 1 if any failures
- FR-7: Support flags: `--help`, `--language <name>`, `--json`, `--verbose`, `--quiet`

## Non-Goals

- No automatic fixing of broken implementations
- No parallel execution (sequential for predictable Docker resource usage)
- No testing of matrices 2-6 (separate feature)
- No performance ranking or comparison (just pass/fail)
- No GitHub issue creation (manual review only)

## Technical Considerations

- Use `docker exec -w /app/Languages/<lang> sudoku-benchmark ./runMe.sh ../../Matrices/1.matrix`
- Use `timeout` command for 60-second limit
- Parse "Solved in Iterations=NNN" from output to verify correctness
- Some languages may need compilation before running - `runMe.sh` handles this
- Handle languages with no `runMe.sh` (skip with warning)

## Success Metrics

- Full test suite completes in under 90 minutes (88 languages × 60s max)
- Clear identification of all non-working languages
- Zero false positives (working languages marked as failed)
- Output actionable enough to begin fixing broken implementations

## Open Questions

- Should we add a `--retry` flag to retry failed languages once?
- Should results be committed to the repo or gitignored?
