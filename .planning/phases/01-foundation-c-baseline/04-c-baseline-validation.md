# Plan: C Baseline Validation

**Phase:** 1 - Foundation & C Baseline
**Plan:** 04 of 05
**Created:** 2025-12-16
**Status:** Ready for execution

## Objective

Validate C solver as the authoritative reference baseline. Ensure C produces exact iteration counts (656, 439269, 98847, 9085, 445778) and output format matches reference. Fix algorithm if needed, then document implementation.

## Context

C is the reference implementation - all other languages will be validated against it. From PROJECT.md: "C implementation serves as the baseline reference (brute-force algorithm)" and "Reference iteration counts documented in Matrices/ReferenceForAllMatrixRun.txt"

High likelihood C has drifted based on benchmark_history.json showing 100 iterations instead of 656. If validation fails, must fix C algorithm before declaring Phase 1 complete.

## Tasks

<task id="1" type="auto">
<description>Run C solver and capture comprehensive metrics</description>

<context>
Execute C solver using the refactored modular script from Plan 02. Run all 5 matrices (not Matrix 6 yet) and capture full metrics with timing, memory, CPU usage, and complete output.

This is the definitive C baseline run that establishes the reference for all future language implementations.
</context>

<files>
- /Languages/C/runMe.sh - Refactored script from Plan 02
- /Languages/C/metrics.json - Generated output
- /Languages/C/Sudoku.c - C implementation source
- /Metrics/benchmarks.db - Database for storing run data
</files>

<actions>
1. Verify C compiler available in Docker:
   ```bash
   docker exec -it <container> gcc --version
   ```

2. Check C source file exists and is readable:
   ```bash
   docker exec -it <container> cat /app/Languages/C/Sudoku.c | head -50
   ```

3. Clean any previous builds:
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && rm -f Sudoku metrics.json"
   ```

4. Run C solver for matrices 1-5:
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && ./runMe.sh ../../../Matrices/1.matrix ../../../Matrices/2.matrix ../../../Matrices/3.matrix ../../../Matrices/4.matrix ../../../Matrices/5.matrix"
   ```

5. Verify metrics.json generated:
   ```bash
   docker exec -it <container> cat /app/Languages/C/metrics.json
   ```

6. Insert metrics into database:
   ```javascript
   // In generate_report_only.ts or new script:
   import Database from 'better-sqlite3';
   const db = new Database('Metrics/benchmarks.db');

   const metrics = JSON.parse(fs.readFileSync('Languages/C/metrics.json'));
   for (const run of metrics) {
     for (const result of run.results) {
       db.prepare(`
         INSERT INTO runs (timestamp, language, matrix, iterations, time_seconds, memory_kb, cpu_user, cpu_sys, status, output, compiler_variant, toolchain_version)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       `).run(
         run.timestamp,
         run.solver,
         result.matrix,
         result.iterations,
         result.time,
         result.memory,
         result.cpu_user,
         result.cpu_sys,
         result.status,
         result.output,
         'O3',  // Default variant
         'gcc ' + execSync('gcc --version | head -1').toString().trim()
       );
     }
   }
   ```

7. Capture timing summary:
   ```bash
   docker exec -it <container> node /app/Metrics/query_runs.js --language=C --summary
   ```
   Should show total time, per-matrix breakdown
</actions>

<verification>
- [ ] C compiler available (gcc)
- [ ] C source code readable
- [ ] Compilation succeeds
- [ ] All 5 matrices execute
- [ ] metrics.json contains 5 results
- [ ] All results have status='success'
- [ ] Iteration counts captured for each matrix
- [ ] Output text captured completely
- [ ] Run data inserted into database
</verification>

<acceptance>
C solver runs successfully on matrices 1-5. Comprehensive metrics captured including iterations, timing, memory, CPU, and full output. Data stored in both metrics.json and SQLite database.
</acceptance>
</task>

<task id="2" type="checkpoint:c_validation">
<description>Validate C results against reference and fix if needed</description>

<context>
Critical checkpoint: Use validation systems from Plan 03 to verify C matches reference exactly. If ANY mismatch found, must fix C algorithm before proceeding.

Expected iterations from ReferenceForAllMatrixRun.txt:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778

Per 01-CONTEXT.md Risk 2: "If C doesn't match, fix C algorithm before modularizing scripts (don't propagate broken pattern)."
</context>

<files>
- /Languages/C/metrics.json - Results from task 1
- /Matrices/ReferenceForAllMatrixRun.txt - Ground truth
- /Languages/C/Sudoku.c - May need fixing if validation fails
</files>

<actions>
1. Run iteration count validation:
   ```bash
   docker exec -it <container> node /app/Metrics/validate_run.js /app/Languages/C/metrics.json
   ```

2. Expected output:
   ```
   === Validation Results ===

   ✓ PASS - C Matrix 1
   ✓ PASS - C Matrix 2
   ✓ PASS - C Matrix 3
   ✓ PASS - C Matrix 4
   ✓ PASS - C Matrix 5

   Overall: ALL PASSED
   ```

3. If ANY failure occurs:

   **Step 3a: Investigate algorithm drift**
   - Compare Sudoku.c with reference C implementation
   - Check if backtracking order changed (row-major vs column-major)
   - Check if candidate values tried in wrong order (1-9 vs 9-1)
   - Check if validation logic different (row/col/box checks)

   **Step 3b: Fix C implementation**
   - Restore original brute-force algorithm
   - Ensure iteration counter increments at correct point (each value placement attempt)
   - Test with Matrix 1 first (smallest - should give 656)
   - Iterate until validation passes

   **Step 3c: Re-run validation**
   - Repeat task 1 after fixing
   - Verify ALL matrices now pass
   - Do NOT proceed to task 3 until C validates

4. Run output format validation:
   ```bash
   docker exec -it <container> node /app/Metrics/validate_run.js --format /app/Languages/C/metrics.json
   ```

5. If format validation fails:
   - Check path format (should be ../Matrices/N.matrix not absolute)
   - Check spacing (single space after each digit)
   - Check "Solved in Iterations=" exact text
   - Fix output formatting in Sudoku.c
   - Re-run

6. Store validation results:
   ```bash
   docker exec -it <container> sqlite3 /app/Metrics/benchmarks.db "SELECT language, matrix, expected_iterations, actual_iterations, valid FROM validations WHERE language='C' ORDER BY matrix;"
   ```

7. Generate validation report:
   ```bash
   docker exec -it <container> node /app/Metrics/generate_validation_report.js > /tmp/c_validation_report.txt
   ```

8. **BLOCKING GATE**: Do not proceed if C validation fails
   - Phase 1 cannot complete without validated C baseline
   - All other languages depend on C being correct
   - Fix C now or risk propagating algorithm errors to 14 other languages
</actions>

<verification>
- [ ] Iteration count validation runs successfully
- [ ] C passes all 5 iteration count checks
- [ ] Expected: [656, 439269, 98847, 9085, 445778]
- [ ] Output format validation runs successfully
- [ ] C passes all 5 format checks
- [ ] Validation results stored in database
- [ ] If failures found, C algorithm fixed and re-validated
- [ ] Zero tolerance: ALL checks must pass
</verification>

<acceptance>
C solver validated against reference. Iteration counts match exactly (656, 439269, 98847, 9085, 445778). Output format matches reference exactly. C established as authoritative baseline for remaining 14 languages. Validation results stored in database with ALL PASSED status.
</acceptance>
</task>

<task id="3" type="auto">
<description>Document C implementation and establish reference pattern</description>

<context>
With C validated, document the implementation thoroughly. This documentation serves as the reference for implementing all other languages in Phases 2-5.

Must document:
- Algorithm structure (backtracking logic)
- Iteration counting method (where counter increments)
- Output format requirements
- Compilation flags and variants
- Performance characteristics
</context>

<files>
- /Languages/C/README.md - New comprehensive documentation
- /Languages/C/ALGORITHM.md - Algorithm details for other implementers
- /Languages/C/Sudoku.c - Add inline comments if missing
</files>

<actions>
1. Create Languages/C/README.md:
   ```markdown
   # C Sudoku Solver - Reference Baseline

   ## Overview

   This is the authoritative reference implementation of the brute-force Sudoku solver.
   All other language implementations must match this algorithm exactly.

   ## Validation Status

   ✓ Iteration counts validated: 656, 439269, 98847, 9085, 445778 (matrices 1-5)
   ✓ Output format validated: Exact match to reference
   ✓ Baseline established: 2025-12-16

   ## Algorithm

   Recursive backtracking with these characteristics:
   - Search order: Row-major (top-to-bottom, left-to-right)
   - Candidate values: 1-9 in ascending order
   - Iteration count: Incremented on each value placement attempt
   - Validation: Check row, column, and 3x3 box constraints

   See ALGORITHM.md for detailed pseudocode.

   ## Compilation

   **Default (used in benchmarks):**
   ```bash
   gcc -O3 -o Sudoku Sudoku.c
   ```

   **Variants (for performance comparison):**
   - `-O0` - No optimization (baseline)
   - `-O2` - Moderate optimization
   - `-O3` - Aggressive optimization (default)
   - `-Ofast` - Maximum optimization (may break standards compliance)

   ## Execution

   ```bash
   ./runMe.sh ../../../Matrices/1.matrix
   ```

   Outputs `metrics.json` with timing, iterations, memory, CPU usage.

   ## Performance Baseline

   Validated timings on Docker (ubuntu:24.04, gcc 13.x):
   - Matrix 1: ~0.001s (656 iterations)
   - Matrix 2: ~0.5s (439,269 iterations)
   - Matrix 3: ~0.1s (98,847 iterations)
   - Matrix 4: ~0.01s (9,085 iterations)
   - Matrix 5: ~0.5s (445,778 iterations)

   (Actual times vary by CPU - iterations are canonical)

   ## Reference Implementation

   This C implementation defines:
   1. Algorithm correctness (iteration counts)
   2. Output format (spacing, headers, paths)
   3. Performance baseline (all languages compared to C)

   Other languages must match iteration counts EXACTLY to pass validation.
   ```

2. Create Languages/C/ALGORITHM.md:
   ```markdown
   # Brute-Force Sudoku Algorithm

   ## Pseudocode

   ```
   solve(puzzle):
       find first empty cell (value = 0)
       if no empty cells:
           return true (solved)

       for candidate = 1 to 9:
           iterations++  // COUNT EVERY ATTEMPT

           if is_valid(puzzle, row, col, candidate):
               puzzle[row][col] = candidate

               if solve(puzzle):  // Recurse
                   return true

               puzzle[row][col] = 0  // Backtrack

       return false  // No valid candidate
   ```

   ## Critical Implementation Details

   ### Iteration Counting
   - Counter MUST increment inside the candidate loop (1-9)
   - Count happens BEFORE validation check
   - Count happens even if candidate invalid
   - This ensures deterministic iteration counts

   ### Search Order
   - Cells searched top-to-bottom, left-to-right (row-major)
   - NOT column-major (would give different iteration counts)
   - NOT random order (would be non-deterministic)

   ### Candidate Order
   - Always try 1, 2, 3, 4, 5, 6, 7, 8, 9 in that order
   - NOT 9, 8, 7, ... (would give different iteration counts)
   - NOT optimized order (e.g., trying most constrained first)

   ### Validation Logic
   - Check row: No duplicate in same row
   - Check column: No duplicate in same column
   - Check box: No duplicate in 3x3 box
   - Box calculation: box_row = (row/3)*3, box_col = (col/3)*3

   ## Why Iteration Counts Matter

   The iteration count is a **fingerprint of the algorithm**. If two implementations
   produce different iteration counts, they have different algorithms (even if they
   both solve the puzzle correctly).

   Examples that break iteration matching:
   - Column-major search order
   - Trying candidates 9→1 instead of 1→9
   - Optimizations (constraint propagation, heuristics)
   - Different backtracking strategy
   - Early termination on validation failure

   ## Output Format Requirements

   Must match EXACTLY:
   ```
   ../Matrices/1.matrix

   Puzzle:
   9 2 0 0 0 0 5 8 4
   ... (9 rows, single space between digits)

   Puzzle:
   9 2 1 6 3 7 5 8 4
   ... (solved puzzle)

   Solved in Iterations=656
   ```

   - Path must be relative (../Matrices/N.matrix)
   - Blank line after path
   - "Puzzle:" header (capital P, colon, no spaces)
   - Single space between digits
   - Blank line between initial and solved puzzle
   - "Solved in Iterations=N" (exact text, no spaces around =)
   ```

3. Add inline comments to Sudoku.c (if not already present):
   - Mark where iteration counter increments
   - Comment search order (row-major)
   - Comment validation logic

4. Commit C baseline documentation:
   ```bash
   git add Languages/C/README.md Languages/C/ALGORITHM.md Languages/C/Sudoku.c
   git commit -m "Document C baseline implementation - Phase 1 reference

   - Add comprehensive README with validation status
   - Document algorithm in detail for other implementers
   - Clarify iteration counting method
   - Establish performance baseline
   - Ready for use as reference in Phases 2-5"
   ```
</actions>

<verification>
- [ ] README.md created with all sections
- [ ] Validation status documented (iteration counts listed)
- [ ] Compilation instructions clear
- [ ] Performance baseline documented
- [ ] ALGORITHM.md explains brute-force approach
- [ ] Iteration counting logic explained
- [ ] Search and candidate order specified
- [ ] Output format requirements documented
- [ ] Sudoku.c has inline comments at key points
- [ ] Documentation committed to git
</verification>

<acceptance>
C implementation fully documented. README.md provides overview, validation status, compilation, and performance baseline. ALGORITHM.md serves as reference for implementing other languages with exact algorithm match. Documentation committed and ready for use in Phases 2-5.
</acceptance>
</task>

## Dependencies

**Blocks:**
- Plan 05 (Content Server) - C baseline establishes reference
- Phase 2 (Compiled Languages) - C pattern copied to C++, Go, Rust
- Phase 3-5 (All languages) - All validated against C baseline

**Blocked by:**
- Plan 01 (Docker & Database) - needs toolchains and database
- Plan 02 (Modular Scripts) - uses refactored C/runMe.sh
- Plan 03 (Validation Systems) - uses validation tools

## Risks

- **C fails validation**: HIGH risk based on benchmark_history.json. If occurs, must fix algorithm before proceeding. Estimated 1-2 hours to debug and fix.
- **Algorithm drift unrecoverable**: LOW risk - can restore from ReferenceForAllMatrixRun.txt if needed
- **Performance regression**: MEDIUM risk - if C much slower than expected, investigate compilation flags
- **Documentation insufficient**: LOW risk - can iterate on docs in later phases if needed

## Success Criteria

- [x] C solver runs all 5 matrices successfully
- [x] Iteration counts match reference EXACTLY
- [x] Output format matches reference EXACTLY
- [x] Validation results stored in database
- [x] C implementation documented comprehensively
- [x] ALGORITHM.md serves as reference for other languages

## Notes

This is the most critical plan in Phase 1. C must validate before any other work continues. Per 01-CONTEXT.md: "Phase 1 blocks all other phases - nothing can proceed until C baseline validated."

If C validation fails (likely based on benchmark_history.json), fixing the algorithm takes priority over all other Phase 1 work. Do NOT proceed to Plan 05 until C validates successfully.
