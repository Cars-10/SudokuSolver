# Testing Patterns

**Analysis Date:** 2025-12-24

## Test Framework

**Runner:**
- None configured - No vitest, jest, or mocha

**Assertion Library:**
- Not applicable

**Run Commands:**
```bash
# No test commands available
# Validation is done via utility scripts:
node check_invalid.mjs          # Find invalid metrics entries
node Metrics/validation.js      # Validate iteration counts
```

## Test File Organization

**Location:**
- No `*.test.ts` or `*.spec.ts` files
- No `__tests__/` directories
- Validation scripts in `Metrics/` and root

**Naming:**
- Not applicable (no test files)

**Structure:**
```
SudokuSolver/
  check_invalid.mjs         # Metrics validation script
  Metrics/
    validation.js           # Iteration count validation
    format_validation.js    # Format validation utilities
    validate_run.js         # Run validation
    db_utils.js             # Database utilities (with JSDoc)
```

## Test Structure

**No formal test suites.** The project uses manual validation:

**Validation Pattern (from `check_invalid.mjs`):**
```javascript
for (const file of files) {
    try {
        const data = JSON.parse(fs.readFileSync(file, 'utf8'));
        if (Array.isArray(data)) {
            for (const entry of data) {
                if (!entry.results || entry.results.length === 0) {
                    invalid.push({ file, reason: 'No results array' });
                }
                if (r.status === 'env_error' || r.status === 'error') {
                    invalid.push({ file, reason: 'Error status: ' + r.status });
                }
            }
        }
    } catch (e) {
        invalid.push({ file, reason: 'JSON parse error' });
    }
}
```

## Mocking

**Framework:**
- Not applicable (no test framework)

**Patterns:**
- Not applicable

## Fixtures and Factories

**Test Data:**
- Reference outputs in `Matrices/ReferenceForAllMatrixRun.txt`
- Expected iterations in `Matrices/reference_iterations.json`
- 6 matrix puzzle files (`Matrices/1.matrix` through `6.matrix`)

**Location:**
- `Matrices/` directory contains reference data
- Per-language `metrics.json` files serve as historical fixtures

## Coverage

**Requirements:**
- No coverage tracking
- No coverage targets

**Configuration:**
- Not applicable

## Test Types

**Unit Tests:**
- Not implemented

**Integration Tests:**
- Not implemented

**Validation Scripts (informal testing):**
- `check_invalid.mjs` - Validates metrics JSON structure
- `Metrics/validation.js` - Validates iteration counts match reference
- These are run manually, not in CI

**Benchmark Testing (the project IS a benchmark tool):**
- `Metrics/SolverRunner.ts` executes language solvers
- Captures: time, iterations, memory, CPU user/sys, status, output
- Validates output against reference solutions

## Common Patterns

**Data Validation:**
```typescript
// From types.ts - Type definitions serve as validation contracts
export interface MetricResult {
    matrix: string;
    time: number;
    iterations: number;
    memory: number;
    cpu_user: number;
    cpu_sys: number;
    status: string;
    output?: string;
}
```

**Error Detection:**
```javascript
// Status-based error detection
if (r.status === 'env_error' || r.status === 'error') {
    // Mark as invalid
}
```

**Reference Comparison:**
- Solver output compared against `ReferenceForAllMatrixRun.txt`
- Iteration counts compared against `reference_iterations.json`

## Benchmark Testing Infrastructure

**The project itself is a benchmarking tool:**

**SolverRunner (`Metrics/SolverRunner.ts`):**
- Executes language-specific solvers
- Supports local and Docker execution
- Captures performance metrics

**HistoryManager (`Metrics/HistoryManager.ts`):**
- Tracks all benchmark runs
- Append-only log in `benchmark_history.json`

**RepositoryAnalyzer (`Metrics/RepositoryAnalyzer.ts`):**
- Discovers solver implementations
- Reads reference outputs for validation

**HTMLGenerator (`Metrics/HTMLGenerator.ts`):**
- Generates comprehensive reports
- Includes mismatch detection and scoring

## Known Limitations

- **No unit tests** - All validation is script-based
- **No CI/CD pipeline** - No automated testing on push
- **No coverage reports** - No tracking of code coverage
- **Manual validation** - Relies on running scripts manually
- **Status-based errors** - Uses status codes instead of exceptions

## Recommendations

To add proper testing:

1. **Install vitest:**
   ```bash
   npm install -D vitest @vitest/coverage-v8
   ```

2. **Add test script to package.json:**
   ```json
   "scripts": {
       "test": "vitest",
       "test:coverage": "vitest --coverage"
   }
   ```

3. **Create test files co-located with source:**
   ```
   Metrics/
     SolverRunner.ts
     SolverRunner.test.ts
     HistoryManager.ts
     HistoryManager.test.ts
   ```

4. **Priority areas for testing:**
   - `HistoryManager` - Read/write operations
   - `RepositoryAnalyzer` - File discovery logic
   - `SolverRunner` - Timeout and error handling
   - Input validation in `server/index.js`

---

*Testing analysis: 2025-12-24*
*Update when test patterns change*
