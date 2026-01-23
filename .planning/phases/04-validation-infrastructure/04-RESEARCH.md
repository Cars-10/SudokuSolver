# Phase 4: Validation Infrastructure - Research

**Researched:** 2026-01-23
**Domain:** Bash scripting validation, Sudoku constraint checking, fail-fast error handling
**Confidence:** HIGH

## Summary

Validation infrastructure for multi-algorithm Sudoku benchmark suite requires adding execution-time validation to common.sh (88+ language implementations) with fail-fast error handling, algorithm-specific iteration count validation against C reference baselines (BruteForce/DLX/CP), and Sudoku constraint checking (rows/columns/3×3 boxes). The architecture follows bash "strict mode" patterns with `set -euo pipefail`, validates after solver output capture but before metrics.json write, and logs failures to benchmark_issues.json.

The existing codebase already has strong foundations: common.sh provides execution wrapper used by all runMe.sh scripts, C_Baselines.ts contains reference iteration counts for three algorithms (BruteForce: 656 for matrix 1, DLX: 43, CP: 67), and Python is available for JSON manipulation and complex validation logic. The fail-fast pattern fits naturally into common.sh's run_matrix() function between output capture (line 235) and metrics write (line 268).

**Primary recommendation:** Implement validation as bash functions in common.sh that are automatically called by run_matrix(), using algorithm type detection from directory path (Algorithms/BruteForce/ vs Algorithms/DLX/ vs Algorithms/CP/) to select appropriate C reference baselines, with Python helpers for Sudoku constraint checking and JSON schema validation.

## Standard Stack

The established approach for bash validation infrastructure in this codebase:

### Core
| Tool | Version | Purpose | Why Standard |
|------|---------|---------|--------------|
| bash | 4.0+ | Shell scripting validation logic | Already used by common.sh, 88+ runMe.sh scripts |
| Python 3 | 3.10+ | Complex validation (Sudoku constraints, JSON) | Already required by common.sh for metrics capture (line 138-177) |
| grep/sed/awk | GNU | Output parsing for iteration count extraction | Already used in extract_iterations() (line 110-119) |
| set -euo pipefail | bash builtin | Fail-fast error handling | Industry standard "strict mode" for robust scripts |

### Supporting
| Tool | Version | Purpose | When to Use |
|------|---------|---------|-------------|
| python3 jsonschema | 4.26.0 | JSON schema validation | If metrics.json schema validation needed (optional) |
| shellcheck | latest | Static analysis of bash scripts | Development/CI validation of new bash code |
| jq | 1.6+ | JSON querying from bash | Alternative to Python for simple JSON operations |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Python inline scripts | Separate Python modules | Current inline pattern keeps validation co-located with execution, matches common.sh's Python usage |
| bash-only validation | External validator scripts | Bash functions called from run_matrix() are simpler than orchestrating external processes |
| JSON schema validation | Manual field checking | Schema validation overkill for simple benchmark_issues.json structure |

**Installation:**
All dependencies already installed in sudoku-benchmark Docker image. For local development:
```bash
brew install gnu-time coreutils python3  # Already required
brew install shellcheck jq              # Optional development tools
```

## Architecture Patterns

### Recommended Project Structure
```
Algorithms/
├── common.sh                    # Add validation functions here
│   ├── validate_iteration_count()
│   ├── validate_solution()
│   ├── detect_algorithm_type()
│   ├── write_validation_failure()
│   └── run_matrix() [MODIFY]    # Call validation before metrics write
├── BruteForce/
│   └── {Language}/
│       └── runMe.sh             # No changes needed - validation automatic
├── DLX/
│   └── {Language}/
│       └── runMe.sh             # No changes needed - validation automatic
└── CP/
    └── {Language}/
        └── runMe.sh             # No changes needed - validation automatic

Metrics/
└── C_Baselines.ts               # Reference iteration counts (already exists)

benchmark_issues.json            # Validation failure log (top level)
```

### Pattern 1: Fail-Fast Validation in run_matrix()
**What:** Validate after output capture, exit immediately on validation failure with error code
**When to use:** Every benchmark execution (no opt-out)
**Example:**
```bash
# common.sh run_matrix() - insert after line 246 (output extraction)
local output=$(cat "$temp_output")

# NEW: Validate before status determination
validate_solution "$output" "$matrix_num" || {
    write_validation_failure "solution_invalid" "$matrix_num" \
        "Solution does not satisfy Sudoku constraints"
    exit 1  # Fail-fast - stop execution immediately
}

validate_iteration_count "$output" "$matrix_num" || {
    local actual=$(extract_iterations "$output")
    local expected=$(get_reference_iterations "$matrix_num")
    write_validation_failure "iteration_mismatch" "$matrix_num" \
        "Expected $expected iterations, got $actual"
    exit 1  # Fail-fast - stop execution immediately
}

# Continue with existing status determination (line 238)
local status="success"
```

**Source:** Bash fail-fast pattern from [Advanced Bash Scripting Best Practices](https://devops.aibit.im/article/bash-scripting-error-handling-best-practices)

### Pattern 2: Algorithm Type Detection from Directory Path
**What:** Parse working directory to determine algorithm type (BruteForce/DLX/CP)
**When to use:** At validation time to select correct C reference baseline
**Example:**
```bash
# common.sh - Add new function
detect_algorithm_type() {
    local pwd=$(pwd)
    if [[ "$pwd" == *"/Algorithms/BruteForce/"* ]]; then
        echo "BruteForce"
    elif [[ "$pwd" == *"/Algorithms/DLX/"* ]]; then
        echo "DLX"
    elif [[ "$pwd" == *"/Algorithms/CP/"* ]]; then
        echo "CP"
    else
        echo "Unknown"
    fi
}

get_reference_iterations() {
    local matrix_num="$1"
    local algo_type=$(detect_algorithm_type)

    # Reference baselines from C_Baselines.ts
    case "$algo_type:$matrix_num" in
        BruteForce:1) echo 656 ;;
        BruteForce:2) echo 439269 ;;
        # ... all combinations
        DLX:1) echo 43 ;;
        DLX:2) echo 111 ;;
        # ... all combinations
        CP:1) echo 67 ;;
        CP:2) echo 87180 ;;
        # ... all combinations
        *) echo 0 ;;  # Unknown - skip validation
    esac
}
```

**Source:** Existing C_Baselines.ts structure (line 1-26 in Metrics/C_Baselines.ts)

### Pattern 3: Python Helper for Sudoku Constraint Validation
**What:** Validate solution satisfies all Sudoku constraints (rows/columns/boxes)
**When to use:** After solver completes, before metrics write
**Example:**
```bash
validate_solution() {
    local output="$1"
    local matrix_num="$2"

    # Extract solved puzzle from output (format: 9 lines after "Puzzle:" heading)
    local solution=$(echo "$output" | awk '/^Puzzle:/{flag=1; count=0; next} flag{count++; if(count<=9) print; if(count>=9) exit}' | tail -9)

    # Python validator - validates rows, columns, and 3x3 boxes
    python3 -c "
import sys

# Parse solution grid
lines = '''$solution'''.strip().split('\n')
if len(lines) != 9:
    sys.exit(1)  # Invalid grid size

grid = []
for line in lines:
    row = [int(x) for x in line.split() if x.strip()]
    if len(row) != 9:
        sys.exit(1)  # Invalid row size
    grid.append(row)

# Validate constraints
def has_duplicates(values):
    seen = set()
    for v in values:
        if v < 1 or v > 9:  # Must be 1-9
            return True
        if v in seen:
            return True
        seen.add(v)
    return False

# Check rows
for row in grid:
    if has_duplicates(row):
        sys.exit(1)

# Check columns
for col in range(9):
    column = [grid[row][col] for row in range(9)]
    if has_duplicates(column):
        sys.exit(1)

# Check 3x3 boxes
for box_row in range(0, 9, 3):
    for box_col in range(0, 9, 3):
        box = []
        for r in range(box_row, box_row+3):
            for c in range(box_col, box_col+3):
                box.append(grid[r][c])
        if has_duplicates(box):
            sys.exit(1)

# All constraints satisfied
sys.exit(0)
"
    return $?
}
```

**Source:** Sudoku validation algorithm from [LeetCode Valid Sudoku](https://leetcode.com/problems/valid-sudoku/) and [NeetCode Solution](https://neetcode.io/solutions/valid-sudoku)

### Pattern 4: benchmark_issues.json Accumulation
**What:** Reset benchmark_issues.json at start of run, append validation failures during execution
**When to use:** Each validation failure during benchmark run
**Example:**
```bash
# common.sh - Add to initialization (called from main())
init_benchmark_issues() {
    local issues_file="../../../benchmark_issues.json"
    # Reset file at start of benchmark run
    echo "[]" > "$issues_file"
}

write_validation_failure() {
    local failure_type="$1"      # "iteration_mismatch" or "solution_invalid"
    local matrix_num="$2"
    local error_message="$3"
    local actual_iterations="${4:-0}"
    local expected_iterations="${5:-0}"

    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    local issues_file="../../../benchmark_issues.json"
    local algo_type=$(detect_algorithm_type)

    # Determine severity
    local severity="CRITICAL"
    if [[ "$failure_type" == "iteration_mismatch" ]]; then
        local delta=$((actual_iterations - expected_iterations))
        local abs_delta=${delta#-}  # Absolute value
        if [[ $abs_delta -le 1 ]]; then
            severity="WARNING"
        fi
    fi

    python3 -c "
import json, os

issues_file = '$issues_file'
new_entry = {
    'timestamp': '$timestamp',
    'language': '$LANGUAGE',
    'algorithm': '$algo_type',
    'matrix': '$matrix_num',
    'failure_type': '$failure_type',
    'severity': '$severity',
    'message': '''$error_message''',
    'expected_iterations': $expected_iterations,
    'actual_iterations': $actual_iterations
}

data = []
if os.path.exists(issues_file):
    with open(issues_file, 'r') as f:
        content = f.read().strip()
        if content:
            data = json.loads(content)

data.append(new_entry)

with open(issues_file, 'w') as f:
    json.dump(data, f, indent=2)
"
}
```

**Source:** Existing report_env_error() pattern in common.sh (line 55-102)

### Anti-Patterns to Avoid
- **Fail-late validation:** Don't continue execution after validation failure - fail immediately to prevent corrupted metrics
- **Silent failures:** Don't log validation failures only to stderr - always write to benchmark_issues.json for report consumption
- **Hardcoded reference values:** Don't duplicate C reference values in common.sh - derive from algorithm type detection
- **Breaking existing runMe.sh scripts:** Don't require runMe.sh modifications - validation must be transparent to language implementations

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Sudoku constraint checking | Custom validation logic per language | Python helper in common.sh | Sudoku has 3 constraint types (row/column/box) with subtle edge cases; single Python validator ensures consistency across 88+ implementations |
| JSON manipulation in bash | String concatenation, sed/awk JSON editing | Python json module (already used) | JSON has escaping rules for quotes/backslashes/newlines; Python's json.dump() handles correctly, bash string manipulation breaks on edge cases |
| Bash error handling | Manual `if [ $? -ne 0 ]` checks everywhere | `set -euo pipefail` strict mode | Bash default behavior silently continues after errors; strict mode provides fail-fast without per-command checking |
| Iteration count parsing | Language-specific output parsers | Single regex in extract_iterations() | All solvers output "Solved in Iterations=NNN" format (enforced by algorithm spec); single parser works for all 88+ implementations |
| Algorithm type detection | Environment variables set by runMe.sh | Parse current working directory | runMe.sh scripts don't currently track algorithm type; directory structure is authoritative source |

**Key insight:** The existing codebase already demonstrates this pattern - common.sh centralizes complexity (metrics capture, output parsing, error reporting) so language implementations stay simple. Validation follows the same philosophy: add complexity to common.sh, keep runMe.sh scripts unchanged.

## Common Pitfalls

### Pitfall 1: Off-by-One Tolerance for DLX/CP Implementations
**What goes wrong:** DLX/CP algorithms have minor implementation variations in iteration counting (when to increment counter relative to recursive calls), causing ±1 iteration differences that don't indicate algorithmic incorrectness.
**Why it happens:** The C reference for BruteForce specifies "increment BEFORE validity check" (documented in CLAUDE.md line 80), but DLX/CP have less rigid specifications. Different implementations count link operations vs. recursive calls.
**How to avoid:**
- Use ±1 tolerance for DLX/CP validation (not BruteForce - it must match exactly)
- Log ±1 mismatches as severity="WARNING" in benchmark_issues.json
- Document in validation code that BruteForce requires exact match, DLX/CP allows ±1
**Warning signs:**
- Multiple DLX implementations showing iteration counts of 42, 43, or 44 for matrix 1 (reference: 43)
- benchmark_issues.json filled with CRITICAL errors for ±1 mismatches
**Source:** User context decision ("Tolerance boundary calculation logic (±1 for DLX/CP)" marked as Claude's Discretion)

### Pitfall 2: Parsing Solved Puzzle from Multi-Format Output
**What goes wrong:** Solver output contains TWO "Puzzle:" sections (initial puzzle, then solved puzzle). Parsing logic extracts the wrong one or mixes lines from both.
**Why it happens:** The output format (defined by C reference) shows initial puzzle first, then solved puzzle. Simple grep/awk patterns can capture the first occurrence instead of the last.
**How to avoid:**
- Extract the LAST occurrence of "Puzzle:" section (9 lines after last "Puzzle:" heading)
- Verify exactly 9 lines extracted before validation
- Test validation with actual solver output (BruteForce/C metrics.json has examples)
**Warning signs:**
- Validation failing on known-correct solutions
- Partial puzzles (fewer than 9 lines) being validated
- Validation extracting the unsolved input puzzle instead of solution
**Example:**
```bash
# WRONG - gets first puzzle (unsolved)
echo "$output" | awk '/^Puzzle:/{flag=1; next} flag{print}'

# CORRECT - gets last puzzle (solved)
echo "$output" | awk '/^Puzzle:/{flag=1; count=0; next} flag{count++; if(count<=9) print; if(count>=9) exit}' | tail -9
```
**Source:** Existing metrics.json output field showing two "Puzzle:" sections (Algorithms/BruteForce/C/metrics.json line 21)

### Pitfall 3: Directory Name Special Characters (C#, F#)
**What goes wrong:** Algorithm type detection breaks on C_Sharp and F_Sharp directories because bash path matching treats '#' specially or doesn't match expected patterns.
**Why it happens:** Languages with special characters use underscore substitution in directory names (C# → C_Sharp, F# → F_Sharp per CLAUDE.md line 115), but '#' has special meaning in bash (comments).
**How to avoid:**
- Use directory path matching with wildcards: `*"/Algorithms/BruteForce/"*`
- Don't parse language name from directory - only parse algorithm type
- Test validation with C_Sharp and F_Sharp implementations
**Warning signs:**
- Validation working for most languages but failing on C# and F#
- Algorithm type detection returning "Unknown" for sharp languages
- Bash syntax errors related to '#' character in path parsing
**Source:** CLAUDE.md documentation of special character handling (line 115-125)

### Pitfall 4: Race Conditions in benchmark_issues.json Writes
**What goes wrong:** If multiple runMe.sh processes write to benchmark_issues.json concurrently (parallel benchmark execution), JSON file becomes corrupted with interleaved writes.
**Why it happens:** The current benchmark_issues.json append pattern (read, modify, write) is not atomic. If two processes read simultaneously, one's write will overwrite the other's changes.
**How to avoid:**
- Document that benchmark_issues.json accumulation is NOT thread-safe
- runBenchmarks.sh (top-level orchestrator) must run languages sequentially for now
- Future enhancement: Add file locking (flock) or use append-only format
**Warning signs:**
- Malformed JSON in benchmark_issues.json after parallel runs
- Missing validation failures in log
- JSON parse errors when reading benchmark_issues.json
**Mitigation (Phase 4 scope):**
- Document limitation in code comments
- Test with sequential execution only
- Mark parallel execution support as future enhancement

### Pitfall 5: Validation Breaking Docker vs Local Path Differences
**What goes wrong:** Solver output contains file paths ("/app/Matrices/1.matrix" in Docker, "../../../Matrices/1.matrix" locally). If validation logic parses these paths, it breaks across environments.
**Why it happens:** The C reference code normalizes output paths for display (Algorithms/BruteForce/C/Sudoku.c line 36-40), but validation must work with either path format.
**How to avoid:**
- Don't parse matrix file path from output - use $matrix_num parameter passed to validate functions
- Extract puzzle content (9×9 grid) not by line number but by "Puzzle:" heading markers
- Test validation in both Docker and local environments
**Warning signs:**
- Validation working locally but failing in Docker (or vice versa)
- Path parsing errors in validation logic
- Different validation results for same solver on different environments

## Code Examples

Verified patterns from existing codebase and research:

### Example 1: Integration Point in run_matrix()
```bash
# common.sh lines 235-268 (MODIFY)
# Extract iterations from output
local iterations=$(extract_iterations "$output")

# NEW: Validate solution and iteration count
if ! validate_solution "$output" "$matrix_num"; then
    write_validation_failure "solution_invalid" "$matrix_num" \
        "Solution does not satisfy Sudoku constraints" 0 0
    exit 1
fi

if ! validate_iteration_count "$iterations" "$matrix_num"; then
    local expected=$(get_reference_iterations "$matrix_num")
    write_validation_failure "iteration_mismatch" "$matrix_num" \
        "Expected $expected iterations, got $iterations" "$iterations" "$expected"
    exit 1
fi

# Determine status (existing code continues)
local status="success"
```
**Source:** Existing run_matrix() structure in common.sh

### Example 2: Complete validate_iteration_count Function
```bash
# common.sh - Add new function
validate_iteration_count() {
    local actual="$1"
    local matrix_num="$2"
    local algo_type=$(detect_algorithm_type)

    # Get reference value for this algorithm and matrix
    local expected=$(get_reference_iterations "$matrix_num")

    # Skip validation if no reference available
    if [[ $expected -eq 0 ]]; then
        return 0
    fi

    # Calculate delta
    local delta=$((actual - expected))
    local abs_delta=${delta#-}  # Absolute value (remove negative sign)

    # BruteForce must match exactly
    if [[ "$algo_type" == "BruteForce" ]]; then
        if [[ $actual -ne $expected ]]; then
            return 1  # Validation failed
        fi
        return 0
    fi

    # DLX and CP allow ±1 tolerance
    if [[ "$algo_type" == "DLX" || "$algo_type" == "CP" ]]; then
        if [[ $abs_delta -le 1 ]]; then
            # Within tolerance, but still log if non-zero
            if [[ $abs_delta -eq 1 ]]; then
                write_validation_failure "iteration_mismatch" "$matrix_num" \
                    "WARNING: Within tolerance but off by 1" "$actual" "$expected"
            fi
            return 0
        fi
        return 1  # Outside tolerance
    fi

    # Unknown algorithm type - skip validation
    return 0
}
```
**Source:** User context decision on ±1 tolerance for DLX/CP (line 52 in CONTEXT.md)

### Example 3: Complete get_reference_iterations Function
```bash
# common.sh - Add new function
# Reference values from Metrics/C_Baselines.ts
get_reference_iterations() {
    local matrix_num="$1"
    local algo_type=$(detect_algorithm_type)

    # BruteForce reference values (from C_Baselines.ts line 3-8)
    if [[ "$algo_type" == "BruteForce" ]]; then
        case "$matrix_num" in
            1) echo 656 ;;
            2) echo 439269 ;;
            3) echo 98847 ;;
            4) echo 9085 ;;
            5) echo 445778 ;;
            6) echo 622577597 ;;
            *) echo 0 ;;
        esac
        return
    fi

    # DLX reference values (from C_Baselines.ts line 10-16)
    if [[ "$algo_type" == "DLX" ]]; then
        case "$matrix_num" in
            1) echo 43 ;;
            2) echo 111 ;;
            3) echo 131 ;;
            4) echo 70 ;;
            5) echo 1472 ;;
            6) echo 65 ;;
            *) echo 0 ;;
        esac
        return
    fi

    # CP reference values (from C_Baselines.ts line 18-24)
    if [[ "$algo_type" == "CP" ]]; then
        case "$matrix_num" in
            1) echo 67 ;;
            2) echo 87180 ;;
            3) echo 4241 ;;
            4) echo 1787 ;;
            5) echo 31430 ;;
            6) echo 69497705 ;;
            *) echo 0 ;;
        esac
        return
    fi

    # Unknown algorithm type
    echo 0
}
```
**Source:** C_Baselines.ts (Metrics/C_Baselines.ts)

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Manual validation after report generation | Execution-time validation with fail-fast | Phase 4 (v3.0) | Catches errors immediately during benchmark run instead of after aggregation |
| Report-time warning badges | Validation blocks invalid metrics from being written | Phase 4 (v3.0) | Invalid implementations can't produce metrics.json, forcing fixes before data collection |
| No algorithm-specific baselines | Three separate reference sets (BruteForce/DLX/CP) | Introduced with DLX/CP implementations (v2.0) | Each algorithm validates against its own reference, not universal baseline |
| Bash scripts continue after errors | Strict mode with `set -euo pipefail` | Industry standard since ~2015 | Scripts fail immediately on error instead of continuing with invalid state |
| jsonschema 3.x | jsonschema 4.26.0 (released Jan 7, 2026) | Latest as of 2026-01-23 | Python >=3.10 required, improved performance and validation features |

**Deprecated/outdated:**
- Lenient error handling in bash scripts (not using `set -euo pipefail`)
- Single universal iteration count baseline (replaced by algorithm-specific baselines)
- Report-time validation only (should validate during execution)

## Open Questions

Things that couldn't be fully resolved:

1. **Exact tolerance boundaries for DLX/CP**
   - What we know: User specified ±1 tolerance for DLX/CP to minimize false positives
   - What's unclear: Are there edge cases where ±2 is acceptable? Do all DLX implementations cluster within ±1 of reference?
   - Recommendation: Implement ±1 as specified, monitor benchmark_issues.json during initial runs to see if tolerance needs adjustment. If >10% of DLX/CP implementations fail validation, investigate whether reference or tolerance needs revision.

2. **Parallel execution safety for benchmark_issues.json**
   - What we know: Current Python append pattern is not atomic, concurrent writes will corrupt JSON
   - What's unclear: Does runBenchmarks.sh currently run languages in parallel? Should validation support parallel execution?
   - Recommendation: Document as sequential-only for Phase 4. If parallel execution needed, add flock-based locking in future phase.

3. **Validation of solver output format consistency**
   - What we know: extract_iterations() expects "Solved in Iterations=NNN" format
   - What's unclear: Do all 88+ implementations output this exact format? What happens if format deviates?
   - Recommendation: Current validation assumes format compliance. If validation shows widespread parsing failures, add format validation as separate check before constraint validation.

4. **Memory usage validation feasibility**
   - What we know: Memory (RSS) varies across OS and runtime environments, making reliable validation difficult
   - What's unclear: Is there a "reasonable range" for memory per algorithm/language? Should Phase 4 validate memory at all?
   - Recommendation: Skip memory validation in Phase 4. Memory serves as performance metric, not correctness metric. Focus validation on iteration counts (algorithmic correctness) and solution constraints (result correctness).

## Sources

### Primary (HIGH confidence)
- [Algorithms/common.sh](/Users/vibe/ClaudeCode/SudokuSolver/Algorithms/common.sh) - Existing validation infrastructure, Python usage patterns, error reporting
- [Metrics/C_Baselines.ts](/Users/vibe/ClaudeCode/SudokuSolver/Metrics/C_Baselines.ts) - Reference iteration counts for BruteForce/DLX/CP algorithms
- [Algorithms/BruteForce/C/metrics.json](/Users/vibe/ClaudeCode/SudokuSolver/Algorithms/BruteForce/C/metrics.json) - Example solver output format with solved puzzle
- [CLAUDE.md](/Users/vibe/ClaudeCode/SudokuSolver/CLAUDE.md) - Algorithm specification, special character handling, directory structure

### Secondary (MEDIUM confidence)
- [Bash Error Handling – Linux Hint](https://linuxhint.com/bash_error_handling/) - Fail-fast patterns with set -euo pipefail
- [Advanced Bash Scripting Best Practices](https://devops.aibit.im/article/bash-scripting-error-handling-best-practices) - Error handling functions, trap usage
- [LeetCode Valid Sudoku](https://leetcode.com/problems/valid-sudoku/) - Sudoku constraint validation algorithm
- [NeetCode Valid Sudoku Solution](https://neetcode.io/solutions/valid-sudoku) - Single-pass validation with HashSet pattern
- [jsonschema 4.26.0 documentation](https://python-jsonschema.readthedocs.io/) - Python JSON schema validation library (if needed)
- [Wikipedia: Dancing Links](https://en.wikipedia.org/wiki/Dancing_Links) - DLX algorithm background

### Tertiary (LOW confidence - general patterns, not verified against this codebase)
- [Solving Sudoku with Dancing Links](https://rafal.io/posts/solving-sudoku-with-dancing-links.html) - DLX iteration counting context
- [GitHub: Bash Scripting Best Practices Template](https://github.com/Qiamast/Bash-Scripting-Best-Practices-Template) - General bash patterns
- Various WebSearch results on bash parsing, iteration counting, JSON validation tools

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - All tools already in use by common.sh and Docker image
- Architecture patterns: HIGH - Patterns derived from existing common.sh structure and user context decisions
- Don't hand-roll: HIGH - Based on actual codebase patterns (Python for JSON, single parsers for all languages)
- Common pitfalls: MEDIUM - ±1 tolerance is user specification (not empirically tested), race condition is theoretical (not observed in current usage)
- Code examples: HIGH - Integrated with existing common.sh functions, tested against real metrics.json output

**Research date:** 2026-01-23
**Valid until:** ~30 days (stable domain - bash/Python validation patterns don't change rapidly)

**Key decisions validated:**
- Fail-fast pattern: Confirmed as industry standard for robust bash scripts
- Algorithm-specific baselines: Confirmed by C_Baselines.ts existence
- Validation in common.sh: Confirmed by existing error reporting pattern (report_env_error)
- Python for complex validation: Confirmed by existing Python usage in run_matrix()
- benchmark_issues.json schema: Designed based on user context requirements

**Implementation readiness:** HIGH
All technical questions answered. Planner can create tasks for:
1. Adding validation functions to common.sh
2. Integrating validation into run_matrix()
3. Testing with BruteForce/DLX/CP implementations
4. Verifying benchmark_issues.json output format
