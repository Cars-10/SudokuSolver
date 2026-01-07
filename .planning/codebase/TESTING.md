# Testing Patterns

**Analysis Date:** 2026-01-07

## Test Framework

**Runner:**
- No traditional test framework installed (no Jest, Vitest, Mocha)
- Testing is script-based and manual

**Assertion Library:**
- None configured
- Ad-hoc validation in utility scripts

**Run Commands:**
```bash
# No npm test script configured
# Manual validation via:
./runMeGlobal.sh [Language] [MatrixNumber]    # Run single benchmark
./runBenchmarks.sh                             # Run all benchmarks
node Metrics/validate_run.js                   # Validate output format
```

## Test File Organization

**Location:**
- No dedicated test directories (`__tests__/`, `tests/`)
- No `.test.ts` or `.spec.ts` files found

**Validation Scripts:**
- `Metrics/validate_run.js` - Validates metrics output format
- `Metrics/format_validation.js` - Checks data structure compliance
- `check_invalid.mjs` - Validation utility

## Test Philosophy (from MANIFESTO.md)

**TDD Approach:**
> "When adding a new language, your first and only goal is to get `1.matrix` (the simplest case) to run, solve, and output valid metrics."

**Test Workflow:**
1. Setup language environment (compiler/interpreter)
2. Verify `setupAndRunMe.sh` exists or create it
3. Compile (if needed)
4. Execute on simplest test case (`1.matrix`)
5. Validate metrics output format
6. Check status field for success/failure

## Test Types

**Benchmark Tests (Primary):**
- Run via `./runMeGlobal.sh [Language] [Matrix]`
- Example: `./runMeGlobal.sh C 1` runs C solver on matrix 1
- Validates: execution completes, metrics.json produced, status is success

**Reference Comparison:**
- Compare solver output against `Matrices/ReferenceForAllMatrixRun.txt`
- Ensures algorithmic correctness

**Integration Tests:**
- Manual: Run server, execute benchmarks via UI
- No automated integration test suite

**E2E Tests:**
- Not implemented
- CLI and UI tested manually

## Metrics Validation

**Required Fields in metrics.json:**
```json
{
  "time_ms": number,
  "memory_bytes": number,
  "cpu_user": number,
  "cpu_sys": number,
  "iterations": number,
  "status": "success" | "timeout" | "error",
  "output": string (optional)
}
```

**Extended Fields (10 total):**
- `page_faults_major`, `page_faults_minor`
- `context_switches_voluntary`, `context_switches_involuntary`
- `io_inputs`, `io_outputs`
- `compiler_variant`, `toolchain_version`

**Validation Location:**
- `Metrics/validate_run.js` - JSON structure validation
- `Metrics/db_utils.js` - Database constraints

## Coverage

**Requirements:**
- No formal coverage requirements
- No coverage tooling configured
- Focus on solver correctness over code coverage

**Critical Paths Untested:**
- `server/index.js` API endpoints
- `Metrics/db_utils.js` database operations
- `Metrics/HTMLGenerator.ts` report generation

## Test Data

**Test Matrices:**
- `Matrices/1.matrix` through `Matrices/6.matrix`
- Increasing difficulty levels
- `1.matrix` is simplest (TDD starting point)

**Reference Outputs:**
- `Matrices/reference_output/` - Expected solutions
- `Matrices/ReferenceForAllMatrixRun.txt` - Combined reference

**Factory Pattern:**
- Not used (test data is file-based)

## Run Types

**Local:**
- Direct system execution via shell scripts
- Uses system-installed compilers/interpreters

**Docker:**
- Containerized execution via `Languages/*/Dockerfile`
- Isolated environment for reproducibility

**AI:**
- Placeholder for AI-assisted runs
- Noted in metadata but not fully implemented

## Common Patterns

**Solver Execution Test:**
```bash
# From MANIFESTO.md workflow
cd Languages/[LanguageName]
./setupAndRunMe.sh ../../Matrices/1.matrix
cat metrics.json  # Verify output
```

**Timeout Handling:**
- Default timeout: 180 seconds
- Returns `status: 'timeout'` if exceeded
- Configured in `SolverRunner.ts`

## Linting & Formatting

**TypeScript Configuration:**
- File: `Metrics/tsconfig.json`
- Target: ES2022
- Module: ES2022
- strict: false
- noEmit: true

**No Linting Tools:**
- No ESLint configuration
- No Prettier configuration
- Formatting is manual/IDE-based

## Development Dependencies

```json
// From Metrics/package.json
"devDependencies": {
  "@types/node": "^20.10.0",
  "typescript": "^5.3.2"
}
```

## Recommended Improvements

1. Add Jest or Vitest for unit testing
2. Create test fixtures for API endpoints
3. Add integration tests for report generation
4. Configure ESLint for code quality
5. Add GitHub Actions CI pipeline
6. Implement coverage reporting

---

*Testing analysis: 2026-01-07*
*Update when test patterns change*
