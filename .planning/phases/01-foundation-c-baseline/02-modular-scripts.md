# Plan: Modular Script Architecture

**Phase:** 1 - Foundation & C Baseline
**Plan:** 02 of 05
**Created:** 2025-12-16
**Status:** Ready for execution

## Objective

Create Languages/common.sh with shared functions to eliminate duplication across 15+ language implementations. Refactor C/runMe.sh as the reference implementation using this pattern.

## Context

Currently each language has its own setupAndRunMe.sh with duplicated code for:
- Metrics JSON generation
- Error handling (report_env_error)
- OS detection for timing commands
- Output parsing for iteration counts

From 01-CONTEXT.md: "Balance simplicity (easy to understand), reusability (DRY where it makes sense), flexibility (languages can override)"

## Tasks

<task id="1" type="auto">
<description>Create Languages/common.sh with shared functions</description>

<context>
Need modular script library that provides:
1. Metrics JSON generation (write_metrics_json)
2. Error reporting (report_env_error)
3. Timing capture (run_with_timing)
4. Iteration count extraction (extract_iterations)
5. Memory/CPU measurement

Must be sourced by language-specific runMe.sh scripts and allow overrides for language-specific behavior.
</context>

<files>
- /Languages/common.sh - New shared function library
- /Languages/README.md - Documentation of modular pattern
</files>

<actions>
1. Create Languages/common.sh with functions:

```bash
#!/bin/bash
# Languages/common.sh - Shared functions for benchmark scripts

# Configuration (override in runMe.sh before sourcing)
LANGUAGE="${LANGUAGE:-Unknown}"
SOLVER_BINARY="${SOLVER_BINARY:-./Sudoku}"
COMPILE_CMD="${COMPILE_CMD:-}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Detect OS and set timing command
detect_time_cmd() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        TIME_CMD="gtime"  # GNU time on macOS
    else
        TIME_CMD="/usr/bin/time"  # Linux
    fi
    TIME_FORMAT="-f %e %M %U %S"  # elapsed, maxrss, user, sys
}

# Report environment error (missing compiler, etc.)
report_env_error() {
    local error_msg="$1"
    local metrics_file="${METRICS_FILE:-metrics.json}"

    cat > "$metrics_file" <<EOF
[{
  "solver": "$LANGUAGE",
  "runType": "automated",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "results": [{
    "matrix": "N/A",
    "time": 0,
    "iterations": 0,
    "memory": 0,
    "cpu_user": 0,
    "cpu_sys": 0,
    "status": "env_error",
    "output": "$error_msg"
  }]
}]
EOF
    echo "ERROR: $error_msg" >&2
    exit 1
}

# Extract iteration count from solver output
extract_iterations() {
    local output="$1"
    echo "$output" | grep "Solved in Iterations=" | sed 's/.*Iterations=//'
}

# Run solver with timing for a single matrix
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    # Run with timeout and capture timing
    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    timeout "$TIMEOUT_SECONDS" $TIME_CMD $TIME_FORMAT "$SOLVER_BINARY" "$matrix_path" > "$temp_output" 2> "$temp_timing"
    local exit_code=$?

    local output=$(cat "$temp_output")
    local timing=$(cat "$temp_timing")

    # Parse results
    local status="success"
    if [ $exit_code -eq 124 ]; then
        status="timeout"
    elif [ $exit_code -ne 0 ]; then
        status="error"
    fi

    local iterations=$(extract_iterations "$output")
    local time_seconds=$(echo "$timing" | awk '{print $1}')
    local memory_kb=$(echo "$timing" | awk '{print $2}')
    local cpu_user=$(echo "$timing" | awk '{print $3}')
    local cpu_sys=$(echo "$timing" | awk '{print $4}')

    # Write JSON object (caller handles array wrapping)
    cat <<EOF
  {
    "matrix": "$matrix_num",
    "time": ${time_seconds:-0},
    "iterations": ${iterations:-0},
    "memory": ${memory_kb:-0},
    "cpu_user": ${cpu_user:-0},
    "cpu_sys": ${cpu_sys:-0},
    "status": "$status",
    "output": $(echo "$output" | jq -Rs .)
  }
EOF

    rm -f "$temp_output" "$temp_timing"
}

# Main execution wrapper
run_benchmarks() {
    local metrics_file="${METRICS_FILE:-metrics.json}"
    local matrices="${1:-../../../Matrices/*.matrix}"

    detect_time_cmd

    # Start JSON array
    echo "[" > "$metrics_file"
    echo "  {" >> "$metrics_file"
    echo "    \"solver\": \"$LANGUAGE\"," >> "$metrics_file"
    echo "    \"runType\": \"automated\"," >> "$metrics_file"
    echo "    \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"," >> "$metrics_file"
    echo "    \"results\": [" >> "$metrics_file"

    local first=true
    for matrix in $matrices; do
        if [ "$first" = true ]; then
            first=false
        else
            echo "," >> "$metrics_file"
        fi
        run_matrix "$matrix" >> "$metrics_file"
    done

    echo "" >> "$metrics_file"
    echo "    ]" >> "$metrics_file"
    echo "  }" >> "$metrics_file"
    echo "]" >> "$metrics_file"
}
```

2. Create Languages/README.md documenting the pattern:
   - What goes in common.sh (shared logic)
   - What goes in runMe.sh (language-specific config)
   - How to override functions
   - Example usage for new language

3. Make common.sh executable:
   ```bash
   chmod +x Languages/common.sh
   ```
</actions>

<verification>
- [ ] common.sh created with all functions
- [ ] Functions are well-commented
- [ ] README.md explains modular pattern clearly
- [ ] common.sh is executable
- [ ] Functions follow bash best practices (local vars, error handling)
</verification>

<acceptance>
Languages/common.sh exists with complete shared function library. README.md documents the modular pattern. Code is ready to be sourced by language-specific runMe.sh scripts.
</acceptance>
</task>

<task id="2" type="auto">
<description>Refactor C/runMe.sh to use common.sh pattern</description>

<context>
C/setupAndRunMe.sh currently has all logic inline. Need to refactor to:
1. Set language-specific configuration variables
2. Source common.sh
3. Define compile step
4. Call run_benchmarks() from common.sh

This establishes the reference pattern for all other languages.
</context>

<files>
- /Languages/C/setupAndRunMe.sh - Refactor to use common.sh (backup original first)
- /Languages/C/runMe.sh - New simplified name (optional rename)
</files>

<actions>
1. Backup existing C/setupAndRunMe.sh:
   ```bash
   cp Languages/C/setupAndRunMe.sh Languages/C/setupAndRunMe.sh.backup
   ```

2. Rewrite C/setupAndRunMe.sh (or create runMe.sh):

```bash
#!/bin/bash
# Languages/C/runMe.sh - C-specific benchmark script

cd "$(dirname "$0")"

# Language-specific configuration
LANGUAGE="C"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

# Compilation step (C-specific)
compile() {
    if [ ! -f "Sudoku.c" ]; then
        report_env_error "Sudoku.c not found"
    fi

    if ! command -v gcc &> /dev/null; then
        report_env_error "gcc compiler not found"
    fi

    echo "Compiling C solver..."
    gcc -O3 -o Sudoku Sudoku.c

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful"
}

# Optional: Get compilation flags for variant support (future use)
get_compile_flags() {
    local variant="${1:-default}"
    case "$variant" in
        O0) echo "-O0" ;;
        O2) echo "-O2" ;;
        O3|default) echo "-O3" ;;
        Ofast) echo "-Ofast" ;;
        *) echo "-O3" ;;
    esac
}

# Main execution
main() {
    compile
    run_benchmarks "$@"
}

main "$@"
```

3. Test that it works:
   - Run inside Docker container
   - Verify metrics.json generated
   - Check JSON format matches old output

4. Document differences in commit message or inline comments
</actions>

<verification>
- [ ] Original setupAndRunMe.sh backed up
- [ ] New script sources common.sh correctly
- [ ] Compilation works (gcc called, binary created)
- [ ] run_benchmarks() executes without errors
- [ ] metrics.json generated with correct format
- [ ] All matrices run successfully
- [ ] Iteration counts extracted properly
</verification>

<acceptance>
C/runMe.sh refactored to use modular common.sh pattern. Script is simpler, delegates shared logic to common.sh, compiles and runs successfully.
</acceptance>
</task>

<task id="3" type="checkpoint:script_test">
<description>Test modular script pattern end-to-end</description>

<context>
Validate that the refactored C script produces identical output to the original and follows the documented pattern. This checkpoint ensures the modular architecture is solid before building validation systems on top of it.
</context>

<files>
- /Languages/C/metrics.json - Generated by new script
- /Languages/C/setupAndRunMe.sh.backup - Original for comparison
</files>

<actions>
1. Run original script (backup) and save output:
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && ./setupAndRunMe.sh.backup ../../../Matrices/1.matrix"
   docker exec -it <container> cat /app/Languages/C/metrics.json > /tmp/metrics_old.json
   ```

2. Run new modular script:
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && ./runMe.sh ../../../Matrices/1.matrix"
   docker exec -it <container> cat /app/Languages/C/metrics.json > /tmp/metrics_new.json
   ```

3. Compare outputs:
   - JSON structure identical (ignoring timestamp)
   - Iteration count matches
   - Time/memory/CPU captured
   - Status field present

4. Test error handling:
   ```bash
   # Test missing compiler (rename gcc temporarily)
   docker exec -it <container> bash -c "cd /app/Languages/C && mv Sudoku.c Sudoku.c.bak && ./runMe.sh"
   # Verify report_env_error generates error metrics.json
   ```

5. Test with all 5 matrices (not Matrix 6 yet):
   ```bash
   docker exec -it <container> bash -c "cd /app/Languages/C && ./runMe.sh ../../../Matrices/{1,2,3,4,5}.matrix"
   ```

6. Verify metrics.json contains 5 result objects

7. Document the pattern works in Languages/README.md with example
</actions>

<verification>
- [ ] New script output matches old script structure
- [ ] JSON format valid and parseable
- [ ] Error handling works (report_env_error tested)
- [ ] All 5 matrices run successfully
- [ ] Iteration counts extracted correctly
- [ ] Timing data captured (time, memory, CPU)
- [ ] No regressions from original script
</verification>

<acceptance>
Modular script pattern validated. C/runMe.sh produces correct metrics.json, error handling works, pattern documented and ready for use with remaining 14 languages.
</acceptance>
</task>

## Dependencies

**Blocks:**
- Plan 03 (Validation Systems) - needs working scripts to validate
- Plan 04 (C Baseline) - uses refactored C script
- Plan 05 (Content Server) - will run scripts via API

**Blocked by:**
- Plan 01 (Docker & Database) - needs container and toolchains installed

## Risks

- **Common.sh too rigid**: Mitigated by allowing function overrides in runMe.sh
- **Bash compatibility issues**: Test on ubuntu:24.04 bash (standard Debian bash)
- **Timing command differences**: common.sh already handles macOS vs Linux detection
- **JSON generation brittle**: Use jq for proper escaping, validate output

## Success Criteria

- [x] Languages/common.sh created with shared functions
- [x] C/runMe.sh refactored to use common.sh
- [x] Pattern tested end-to-end
- [x] Metrics JSON output matches original format
- [x] Error handling validated
- [x] README.md documents pattern clearly

## Notes

This modular architecture is key to maintaining 15+ language implementations without code duplication. The C implementation serves as the reference pattern that all other languages will follow in Phases 2-5.

Per 01-CONTEXT.md decision: "Balance simplicity, reusability, and flexibility" - common.sh provides reusable functions while allowing languages to override when needed.
