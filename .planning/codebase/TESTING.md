# Testing Patterns

**Analysis Date:** 2025-12-24

## Test Framework

**Runner:**
- No traditional test framework (Jest, Vitest, Mocha) installed
- Custom validation scripts replace unit testing
- Reference-based regression testing

**Validation Library:**
- Custom implementation in `Metrics/validation.js`
- Format validation in `Metrics/format_validation.js`
- CLI runner in `Metrics/validate_run.js`

**Run Commands:**
```bash
node Metrics/validate_run.js <path>           # Iteration validation (default)
node Metrics/validate_run.js --format <path>  # Format validation
node Metrics/validate_run.js --all <path>     # Both validations
```

## Test File Organization

**Location:**
- Validation scripts in `Metrics/` directory
- No separate `tests/` or `__tests__/` directories
- Co-located with production code

**Naming:**
- `validation.js` - Core validation logic
- `format_validation.js` - Output format checking
- `validate_run.js` - CLI test runner

**Structure:**
```
Metrics/
  ├── validation.js         # Iteration count validation
  ├── format_validation.js  # Output format validation
  ├── validate_run.js       # CLI test runner
  └── [core logic files]
```

## Test Structure

**Validation Pattern:**
```javascript
// Load reference
const reference = loadReferenceIterations();

// Validate against expected
const result = validateIterations(matrix, actualIterations);

// Result structure
{
  valid: boolean,
  expected: number,
  actual: number,
  matrix: string
}
```

**Patterns:**
- Reference-based comparison against C baseline
- Expected iterations stored in `Matrices/reference_iterations.json`
- Status codes: `success`, `timeout`, `error`, `env_error`

## Mocking

**Framework:**
- No mocking framework installed
- Integration testing via actual execution
- No unit test isolation

**What Gets Tested:**
- Actual solver execution
- Real file I/O
- Full benchmark pipeline

## Fixtures and Factories

**Test Data:**
- Reference outputs in `Matrices/reference_output/`
- Reference iterations in `Matrices/reference_iterations.json`
- Test matrices: `Matrices/1.matrix` through `Matrices/6.matrix`

**Location:**
- All fixtures in `Matrices/` directory
- No factory functions for test data

## Coverage

**Requirements:**
- No coverage target defined
- Focus on correctness via reference comparison
- Manual verification of algorithm matching

**View Coverage:**
- No coverage tooling configured

## Test Types

**Validation Tests (Primary):**
- Compare iteration counts against C reference (656 for Matrix 1)
- Verify output format matches specification
- Check status codes are correct

**Integration Tests:**
- Full solver execution via shell
- End-to-end benchmark pipeline
- Report generation verification

**Manual Tests:**
- Visual verification of HTML reports
- Screenshot comparison
- UI interaction testing

## Common Patterns

**Language Solver Testing:**
```bash
# Run benchmarks
cd Languages/YourLanguage
./runMe.sh ../../../Matrices/1.matrix

# Check metrics generated
cat metrics.json

# Validate against reference
node /app/Metrics/validate_run.js /app/Languages/YourLanguage/metrics.json
```

**Validation Execution:**
```bash
# Full validation
node Metrics/validate_run.js Languages/C/metrics.json

# Output format
# ✓ PASS: Matrix 1 - Expected 656, Got 656
# ✗ FAIL: Matrix 2 - Expected 1000, Got 999
```

**Environment Validation:**
```bash
# In language script
check_toolchain gcc  # Validates compiler
report_env_error "GCC not installed"  # Generates error metrics
```

## Validation Success Criteria

**Iteration Matching:**
- Matrix 1: 656 iterations (C reference)
- Must match exactly for validation pass
- Timeout: 180 seconds maximum

**Output Format:**
- Must match C reference output format
- JSON structure must be valid
- Status field must be present

**Metrics File Structure:**
```json
{
  "solver": "LanguageName",
  "runType": "local|docker",
  "metrics": {
    "1.matrix": { "time": 0.5, "iterations": 656, "status": "success" },
    "2.matrix": { ... }
  }
}
```

---

*Testing analysis: 2025-12-24*
*Update when test patterns change*
