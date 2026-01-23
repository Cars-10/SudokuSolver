# Phase 4: Validation Infrastructure - Context

**Gathered:** 2026-01-23
**Status:** Ready for planning

<domain>
## Phase Boundary

Establish algorithmic correctness validation during benchmark execution—ensuring iteration counts match algorithm-specific reference values (C reference for each algorithm) and solutions satisfy Sudoku constraints, with results captured in metrics.json and logged to benchmark_issues.json.

This phase adds validation to the execution pipeline (common.sh) and integrates it into all language runMe.sh scripts. Does not add new benchmark capabilities or change existing UI/report functionality.

</domain>

<decisions>
## Implementation Decisions

### Validation execution timing
- Validate during each benchmark run (in common.sh)
- Validation executes after solver output captured, before metrics.json write
- Validation reads solver stdout/stderr, analyzes iteration count and solution, adds validation fields to metrics data structure

### Failure handling
- Fail-fast: validation failure stops execution immediately with error code
- Write partial metrics.json with validation failure details even on failure (helps diagnose issues)
- Failed validations force developers to fix issues before collecting valid metrics

### Algorithm detection and reference data
- Detect algorithm type from directory path (Algorithms/BruteForce/ vs Algorithms/DLX/)
- Handle special directory naming: C_Sharp and F_Sharp for C# and F#
- Use existing validation matrix for C reference iteration counts
- Validate each algorithm against its own reference (BruteForce vs DLX have different iteration counts)
- Support both BruteForce and DLX algorithm validation

### Failure presentation
- Preserve existing UI error display patterns—don't break current functionality
- Preserve existing diagnostics/debugging views
- Log all validation failures to benchmark_issues.json
- Reset benchmark_issues.json on each run (clear and rewrite, not accumulate)
- NO new UI elements in report—validation results only appear in benchmark_issues.json log

### benchmark_issues.json schema
- Language and matrix that failed (basic identification with timestamp)
- Expected vs actual iteration counts (reference value vs implementation output)
- Solution correctness status (whether solution satisfied Sudoku constraints)
- Error severity level (warning for ±1 delta, critical error for wrong solution or major mismatch)

### Claude's Discretion
- Exact error message formatting in benchmark_issues.json
- Implementation details of Sudoku constraint checking
- Tolerance boundary calculation logic (±1 for DLX/CP)
- How to parse iteration count from various language output formats

</decisions>

<specifics>
## Specific Ideas

- Note on execution context: Each runMe.sh is called from higher-level runBenchmarks.sh
- Enhance diagnostics modal: Add "# of implementations" (sum of all algo implementations) to Algo summary section
- Use existing patterns in common.sh for integration points

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 04-validation-infrastructure*
*Context gathered: 2026-01-23*
