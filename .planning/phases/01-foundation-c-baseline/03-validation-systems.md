# Plan: Validation Systems

**Phase:** 1 - Foundation & C Baseline
**Plan:** 03 of 05
**Created:** 2025-12-16
**Status:** Ready for execution

## Objective

Implement iteration count validation and output format validation systems. These ensure algorithmic consistency by verifying that every language produces EXACTLY the same results as the C baseline.

## Context

From PROJECT.md: "Iteration counts must match C reference exactly (zero tolerance)" and "Output format matches C exactly - spacing, headers, paths, solved puzzle"

Reference data in Matrices/ReferenceForAllMatrixRun.txt contains canonical C output with expected iteration counts:
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations

Validation must be strict - any deviation indicates algorithm drift and blocks language approval.

## Tasks

<task id="1" type="auto">
<description>Implement iteration count validation system</description>

<context>
Need to validate that each language's iteration count matches the reference exactly. This is the primary algorithmic consistency check - if iteration counts match, the algorithm is identical (same search order, same backtracking decisions).

Validation should:
1. Read reference iteration counts from ReferenceForAllMatrixRun.txt
2. Compare against actual run output
3. Report pass/fail with clear error messages
4. Store validation results in SQLite database
</context>

<files>
- /Metrics/validation.js - New validation library
- /Matrices/reference_iterations.json - Extracted reference data
- /Metrics/validate_run.js - CLI tool for manual validation
</files>

<actions>
1. Parse ReferenceForAllMatrixRun.txt and extract iteration counts:
   ```bash
   # Extract lines like "Solved in Iterations=656"
   # Create reference_iterations.json:
   {
     "1": 656,
     "2": 439269,
     "3": 98847,
     "4": 9085,
     "5": 445778,
     "6": 622577597
   }
   ```

2. Create Metrics/validation.js:
   ```javascript
   import fs from 'fs';
   import path from 'path';

   // Load reference iteration counts
   export function loadReferenceIterations() {
     const refPath = path.join(process.cwd(), 'Matrices', 'reference_iterations.json');
     return JSON.parse(fs.readFileSync(refPath, 'utf8'));
   }

   // Validate iteration count for a matrix
   export function validateIterations(matrix, actualIterations) {
     const reference = loadReferenceIterations();
     const matrixNum = String(matrix).replace('.matrix', '');
     const expected = reference[matrixNum];

     if (!expected) {
       return { valid: false, error: `No reference for matrix ${matrixNum}` };
     }

     if (actualIterations !== expected) {
       return {
         valid: false,
         error: `Iteration mismatch for matrix ${matrixNum}: expected ${expected}, got ${actualIterations}`,
         expected,
         actual: actualIterations,
         diff: actualIterations - expected
       };
     }

     return { valid: true, expected, actual: actualIterations };
   }

   // Validate all results from a metrics.json file
   export function validateMetricsFile(metricsPath) {
     const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
     const results = [];

     for (const run of metrics) {
       for (const result of run.results || []) {
         if (result.status === 'success') {
           const validation = validateIterations(result.matrix, result.iterations);
           results.push({
             language: run.solver,
             matrix: result.matrix,
             ...validation
           });
         }
       }
     }

     return results;
   }

   // Insert validation result into database
   export async function recordValidation(db, validation) {
     const stmt = db.prepare(`
       INSERT INTO validations (language, matrix, expected_iterations, actual_iterations, valid, error_message, validated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?)
     `);

     stmt.run(
       validation.language,
       validation.matrix,
       validation.expected,
       validation.actual,
       validation.valid ? 1 : 0,
       validation.error || null,
       new Date().toISOString()
     );
   }
   ```

3. Update SQLite schema (in Plan 01) to include validations table:
   ```sql
   CREATE TABLE IF NOT EXISTS validations (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       language TEXT NOT NULL,
       matrix INTEGER NOT NULL,
       expected_iterations INTEGER NOT NULL,
       actual_iterations INTEGER NOT NULL,
       valid INTEGER NOT NULL,  -- 0 or 1
       error_message TEXT,
       validated_at DATETIME DEFAULT CURRENT_TIMESTAMP
   );

   CREATE INDEX idx_validations_language ON validations(language);
   CREATE INDEX idx_validations_valid ON validations(valid);
   ```

4. Create CLI tool Metrics/validate_run.js:
   ```javascript
   #!/usr/bin/env node
   import { validateMetricsFile } from './validation.js';

   const metricsPath = process.argv[2] || 'Languages/C/metrics.json';
   const results = validateMetricsFile(metricsPath);

   console.log('\n=== Validation Results ===\n');
   let allValid = true;

   for (const result of results) {
     const status = result.valid ? '✓ PASS' : '✗ FAIL';
     console.log(`${status} - ${result.language} Matrix ${result.matrix}`);

     if (!result.valid) {
       console.log(`  Expected: ${result.expected} iterations`);
       console.log(`  Got:      ${result.actual} iterations`);
       console.log(`  Error:    ${result.error}`);
       allValid = false;
     }
   }

   console.log(`\nOverall: ${allValid ? 'ALL PASSED' : 'SOME FAILED'}\n`);
   process.exit(allValid ? 0 : 1);
   ```

5. Add validation call to generate_report_only.ts after loading metrics
</actions>

<verification>
- [ ] reference_iterations.json created with correct values
- [ ] validation.js exports all functions
- [ ] validateIterations() works for single matrix
- [ ] validateMetricsFile() processes full metrics.json
- [ ] CLI tool runs and reports pass/fail clearly
- [ ] Database schema includes validations table
- [ ] Validation results stored in database
</verification>

<acceptance>
Iteration count validation system operational. Can validate any language's metrics.json against reference, report clear pass/fail, and store results in database.
</acceptance>
</task>

<task id="2" type="auto">
<description>Implement output format validation</description>

<context>
Beyond iteration counts, the actual puzzle output must match C exactly:
- Line spacing (blank lines between sections)
- Number formatting (single digit with space after)
- Headers ("Puzzle:" exactly)
- Path format ("../Matrices/1.matrix" not "/app/Matrices/1.matrix")
- "Solved in Iterations=N" format (not "Solved: N iterations")

This ensures consistent output across all languages for fair comparison and debugging.
</context>

<files>
- /Metrics/format_validation.js - Output format validator
- /Matrices/reference_output/ - Directory with canonical output for each matrix
</files>

<actions>
1. Extract reference outputs from ReferenceForAllMatrixRun.txt:
   - Split by matrix (sections start with "../Matrices/N.matrix")
   - Save each to Matrices/reference_output/1.txt, 2.txt, etc.
   - Include full output from path to "Solved in Iterations=N"

2. Create Metrics/format_validation.js:
   ```javascript
   import fs from 'fs';
   import path from 'path';

   // Normalize output for comparison (handle minor whitespace variations)
   function normalizeOutput(output) {
     return output
       .trim()
       .split('\n')
       .map(line => line.trimEnd())  // Remove trailing spaces
       .join('\n');
   }

   // Load reference output for a matrix
   export function loadReferenceOutput(matrix) {
     const matrixNum = String(matrix).replace('.matrix', '');
     const refPath = path.join(process.cwd(), 'Matrices', 'reference_output', `${matrixNum}.txt`);

     if (!fs.existsSync(refPath)) {
       throw new Error(`Reference output not found for matrix ${matrixNum}`);
     }

     return fs.readFileSync(refPath, 'utf8');
   }

   // Compare two outputs (actual vs reference)
   export function compareOutputs(actual, reference) {
     const normalizedActual = normalizeOutput(actual);
     const normalizedRef = normalizeOutput(reference);

     if (normalizedActual === normalizedRef) {
       return { valid: true, message: 'Output matches reference exactly' };
     }

     // Find first difference
     const actualLines = normalizedActual.split('\n');
     const refLines = normalizedRef.split('\n');

     for (let i = 0; i < Math.max(actualLines.length, refLines.length); i++) {
       if (actualLines[i] !== refLines[i]) {
         return {
           valid: false,
           error: `Output mismatch at line ${i + 1}`,
           expectedLine: refLines[i] || '(end of output)',
           actualLine: actualLines[i] || '(end of output)',
           lineNumber: i + 1
         };
       }
     }

     return { valid: false, error: 'Unknown difference' };
   }

   // Validate output format for a matrix result
   export function validateOutputFormat(matrix, actualOutput) {
     try {
       const reference = loadReferenceOutput(matrix);
       return compareOutputs(actualOutput, reference);
     } catch (error) {
       return { valid: false, error: error.message };
     }
   }

   // Validate all outputs from metrics file
   export function validateOutputFormats(metricsPath) {
     const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
     const results = [];

     for (const run of metrics) {
       for (const result of run.results || []) {
         if (result.status === 'success' && result.output) {
           const validation = validateOutputFormat(result.matrix, result.output);
           results.push({
             language: run.solver,
             matrix: result.matrix,
             ...validation
           });
         }
       }
     }

     return results;
   }
   ```

3. Update validations table schema to include format validation:
   ```sql
   ALTER TABLE validations ADD COLUMN format_valid INTEGER;
   ALTER TABLE validations ADD COLUMN format_error TEXT;
   ```

4. Update validate_run.js CLI tool to run both validations:
   - Iteration count check
   - Output format check
   - Report both results

5. Add format validation to generate_report_only.ts
</actions>

<verification>
- [ ] Reference outputs extracted to Matrices/reference_output/
- [ ] format_validation.js exports all functions
- [ ] normalizeOutput() handles minor whitespace variations
- [ ] compareOutputs() finds first difference accurately
- [ ] validateOutputFormat() works for single matrix
- [ ] CLI tool reports format validation results
- [ ] Database stores format validation alongside iteration validation
</verification>

<acceptance>
Output format validation system operational. Can compare any language's output to C reference, identify first difference line-by-line, and report clear pass/fail.
</acceptance>
</task>

<task id="3" type="checkpoint:validation_test">
<description>Test validation systems with C reference data</description>

<context>
Run both validation systems against C baseline to ensure:
1. C passes iteration count validation (or identify algorithm drift)
2. C passes format validation (or fix reference extraction)
3. Validation reports are clear and actionable
4. Database stores validation results correctly

This checkpoint is critical - if C doesn't validate, we need to fix C or the reference before proceeding.
</context>

<files>
- /Languages/C/metrics.json - From Plan 02 test run
- /Metrics/benchmarks.db - Validation results stored here
</files>

<actions>
1. Run C solver for all 5 matrices (using modular script from Plan 02):
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && ./runMe.sh ../../../Matrices/{1,2,3,4,5}.matrix"
   ```

2. Run iteration count validation:
   ```bash
   docker exec -it <container> node /app/Metrics/validate_run.js /app/Languages/C/metrics.json
   ```

3. Expected results for C:
   - Matrix 1: 656 iterations ✓
   - Matrix 2: 439,269 iterations ✓
   - Matrix 3: 98,847 iterations ✓
   - Matrix 4: 9,085 iterations ✓
   - Matrix 5: 445,778 iterations ✓

4. If ANY mismatch found:
   - Compare with benchmark_history.json (shows 100 iterations, likely wrong)
   - Check if C algorithm has drifted from reference
   - BLOCK Phase 1 until C fixed (per 01-CONTEXT.md Risk 2 mitigation)

5. Run output format validation:
   ```bash
   docker exec -it <container> node /app/Metrics/validate_run.js --format /app/Languages/C/metrics.json
   ```

6. Verify database records:
   ```bash
   docker exec -it <container> sqlite3 /app/Metrics/benchmarks.db "SELECT * FROM validations WHERE language='C';"
   ```

7. Test validation with intentionally wrong data:
   - Modify a metrics.json to have wrong iterations
   - Verify validation catches it and reports clearly
   - Modify output to have wrong format
   - Verify format validation catches it

8. Generate validation report:
   ```bash
   docker exec -it <container> node /app/Metrics/generate_validation_report.js
   ```
   Should show C: 5/5 passed (or identify failures)
</actions>

<verification>
- [ ] C metrics.json generated with all 5 matrices
- [ ] Iteration count validation runs without errors
- [ ] C passes all iteration checks OR algorithm drift identified
- [ ] Format validation runs without errors
- [ ] C passes all format checks OR reference format corrected
- [ ] Database contains validation records
- [ ] Intentional failures caught correctly
- [ ] Validation reports are clear and actionable
</verification>

<acceptance>
Validation systems tested end-to-end with C baseline. C passes validation (or issues identified and documented for fixing in Plan 04). Validation results stored in database. System ready for use with remaining 14 languages.
</acceptance>
</task>

## Dependencies

**Blocks:**
- Plan 04 (C Baseline) - needs validation systems to verify C
- Plan 05 (Content Server) - validation status shown in UI
- Phases 2-5 (All languages) - every language must pass validation

**Blocked by:**
- Plan 01 (Docker & Database) - needs SQLite schema with validations table
- Plan 02 (Modular Scripts) - needs working scripts to generate metrics

## Risks

- **C fails validation**: High likelihood based on benchmark_history.json showing 100 iterations instead of 656. Mitigation: Fix C algorithm before declaring Phase 1 complete.
- **Reference data extraction errors**: Could have typos in reference_iterations.json. Mitigation: Double-check against ReferenceForAllMatrixRun.txt manually.
- **Format validation too strict**: Minor whitespace differences could cause false failures. Mitigation: normalizeOutput() handles trailing spaces gracefully.
- **No reference for Matrix 6**: Acceptable - Matrix 6 validation deferred to Phase 6.

## Success Criteria

- [x] Iteration count validation system implemented
- [x] Output format validation system implemented
- [x] Validation systems tested with C baseline
- [x] Reference data extracted and verified
- [x] Database stores validation results
- [x] CLI tool provides clear pass/fail reporting

## Notes

Per 01-CONTEXT.md Risk 2: "If C doesn't match, fix C algorithm before modularizing scripts (don't propagate broken pattern)." This validation checkpoint is the gate - if C fails, Plan 04 must fix the algorithm before Phase 1 can complete.

Zero tolerance policy from PROJECT.md: Even 1 iteration difference indicates algorithm drift and blocks approval.
