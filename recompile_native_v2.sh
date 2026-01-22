#!/bin/bash
# recompile_native_v2.sh
# Recompiles all language implementations natively for macOS
# Logs compilation failures to benchmark_issues.json

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

ISSUES_FILE="benchmark_issues.json"
LOG_FILE="recompile_native.log"

echo "=== Native macOS Recompilation v2 ===" | tee "$LOG_FILE"
echo "Started: $(date)" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Initialize issues file
echo "[]" > "$ISSUES_FILE"

# Function to log compilation result
log_result() {
    local algo=$1
    local lang=$2
    local status=$3
    local message=$4

    if [ "$status" = "success" ]; then
        echo "  ✓ $message" | tee -a "$LOG_FILE"
    elif [ "$status" = "skipped" ]; then
        echo "  ○ $message" | tee -a "$LOG_FILE"
    else
        echo "  ✗ $message" | tee -a "$LOG_FILE"

        # Escape message for JSON
        local escaped=$(printf '%s' "$message" | python3 -c "import sys, json; print(json.dumps(sys.stdin.read()))")

        python3 << PYEOF
import json
with open('$ISSUES_FILE', 'r') as f:
    issues = json.load(f)
issues.append({
    'solver': '$lang',
    'algorithm': '$algo',
    'runType': 'recompile',
    'timestamp': '$(date -u +"%Y-%m-%dT%H:%M:%SZ")',
    'status': 'compile_error',
    'output': $escaped
})
with open('$ISSUES_FILE', 'w') as f:
    json.dump(issues, f, indent=2)
PYEOF
    fi
}

# Function to compile a language
compile_lang() {
    local lang_dir=$1
    local algo=$2
    local lang=$3

    # Check runMe.sh exists
    if [ ! -f "$lang_dir/runMe.sh" ]; then
        log_result "$algo" "$lang" "failed" "No runMe.sh found"
        return 1
    fi

    # Check for compile function
    if ! grep -q "^compile()" "$lang_dir/runMe.sh" && ! grep -q "^function compile" "$lang_dir/runMe.sh"; then
        log_result "$algo" "$lang" "skipped" "Interpreted language"
        return 0
    fi

    # Run compilation in subdirectory
    (
        cd "$lang_dir" || exit 1

        # Create wrapper that calls compile
        timeout 60 bash << 'WRAPPER_END' 2>&1
#!/bin/bash
set +e  # Don't exit on error

# Stub out main
main() { :; }

# Source runMe.sh
source ./runMe.sh 2>&1 || true

# Call compile
if declare -f compile >/dev/null 2>&1; then
    compile 2>&1
    exit $?
else
    echo "ERROR: compile function not found"
    exit 1
fi
WRAPPER_END
    ) > /tmp/compile_$$.log 2>&1

    local exit_code=$?
    local output=$(cat /tmp/compile_$$.log 2>/dev/null)
    rm -f /tmp/compile_$$.log

    if [ $exit_code -eq 0 ]; then
        log_result "$algo" "$lang" "success" "Compiled"
        return 0
    elif [ $exit_code -eq 124 ]; then
        log_result "$algo" "$lang" "failed" "Timeout"
        return 1
    else
        # Get meaningful error
        local err=$(echo "$output" | grep -iE "error|fatal|failed" | head -2 | tr '\n' ' ' | cut -c1-120)
        if [ -z "$err" ]; then
            err=$(echo "$output" | tail -3 | tr '\n' ' ' | cut -c1-120)
        fi
        if [ -z "$err" ]; then
            err="Exit code $exit_code"
        fi
        log_result "$algo" "$lang" "failed" "$err"
        return 1
    fi
}

# Counters
total=0
success=0
failed=0
skipped=0

# Process each algorithm
for algo_dir in Algorithms/BruteForce Algorithms/DLX Algorithms/CP; do
    [ ! -d "$algo_dir" ] && continue

    algo=$(basename "$algo_dir")
    echo "" | tee -a "$LOG_FILE"
    echo "=== $algo ===" | tee -a "$LOG_FILE"

    for lang_dir in $(find "$algo_dir" -maxdepth 1 -type d | tail -n +2 | sort); do
        lang=$(basename "$lang_dir")
        ((total++))

        printf "[%3d] %-20s" "$total" "$lang:" | tee -a "$LOG_FILE"

        if compile_lang "$lang_dir" "$algo" "$lang"; then
            if grep -q "^compile()" "$lang_dir/runMe.sh" || grep -q "^function compile" "$lang_dir/runMe.sh"; then
                ((success++))
            else
                ((skipped++))
            fi
        else
            ((failed++))
        fi
    done
done

# Summary
echo "" | tee -a "$LOG_FILE"
echo "========================================" | tee -a "$LOG_FILE"
echo "=== Summary ===" | tee -a "$LOG_FILE"
echo "========================================" | tee -a "$LOG_FILE"
echo "Completed: $(date)" | tee -a "$LOG_FILE"
echo "Total: $total" | tee -a "$LOG_FILE"
echo "Success: $success" | tee -a "$LOG_FILE"
echo "Skipped: $skipped" | tee -a "$LOG_FILE"
echo "Failed: $failed" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

if [ $failed -gt 0 ]; then
    echo "Failed compilations in: $ISSUES_FILE" | tee -a "$LOG_FILE"
fi
