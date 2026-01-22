#!/bin/bash
# recompile_all_native.sh
# Recompiles all language implementations natively for macOS
# Logs compilation failures to benchmark_issues.json

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

ISSUES_FILE="benchmark_issues.json"
LOG_FILE="recompile_native.log"

echo "=== Native macOS Recompilation ===" | tee "$LOG_FILE"
echo "Started: $(date)" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Initialize issues file as empty array
echo "[]" > "$ISSUES_FILE"

# Function to log compilation result
log_compilation() {
    local algo=$1
    local lang=$2
    local status=$3  # "success" or "failed" or "skipped"
    local message=$4

    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    if [ "$status" = "success" ]; then
        echo "  ✓ $message" | tee -a "$LOG_FILE"
    elif [ "$status" = "skipped" ]; then
        echo "  ○ $message" | tee -a "$LOG_FILE"
    else
        echo "  ✗ $message" | tee -a "$LOG_FILE"

        # Add to issues file - escape message for JSON
        escaped_msg=$(printf '%s' "$message" | python3 -c "import sys, json; print(json.dumps(sys.stdin.read()))")

        python3 << PYEOF
import json

with open('$ISSUES_FILE', 'r') as f:
    issues = json.load(f)

issues.append({
    'solver': '$lang',
    'algorithm': '$algo',
    'runType': 'recompile',
    'timestamp': '$timestamp',
    'status': 'compile_error',
    'output': $escaped_msg
})

with open('$ISSUES_FILE', 'w') as f:
    json.dump(issues, f, indent=2)
PYEOF
    fi
}

# Function to compile a language
compile_language() {
    local lang_dir=$1
    local algo=$2
    local lang=$3

    # Check if runMe.sh exists
    if [ ! -f "$lang_dir/runMe.sh" ]; then
        log_compilation "$algo" "$lang" "failed" "No runMe.sh found"
        return 1
    fi

    # Check if there's a compile function defined
    if ! grep -q "^compile()" "$lang_dir/runMe.sh" && ! grep -q "^function compile" "$lang_dir/runMe.sh"; then
        log_compilation "$algo" "$lang" "skipped" "No compile function (interpreted)"
        return 0
    fi

    # Create a temp script that will do the compilation
    local temp_script=$(mktemp)
    local temp_output=$(mktemp)

    cat > "$temp_script" << 'COMPILE_SCRIPT_END'
#!/bin/bash
set -e

cd "$1"

# Override main function to prevent execution
main() { :; }

# Source runMe.sh but suppress stderr from common.sh load message
source ./runMe.sh 2>/dev/null || true

# Call compile if it exists
if declare -f compile > /dev/null 2>&1; then
    compile
else
    echo "ERROR: No compile function found" >&2
    exit 1
fi
COMPILE_SCRIPT_END

    chmod +x "$temp_script"

    # Run with timeout
    if timeout 60 bash "$temp_script" "$lang_dir" > "$temp_output" 2>&1; then
        local exit_code=0
    else
        local exit_code=$?
    fi

    local compile_output=$(cat "$temp_output")
    rm -f "$temp_script" "$temp_output"

    # Check result
    if [ $exit_code -eq 0 ]; then
        log_compilation "$algo" "$lang" "success" "Compiled successfully"
        return 0
    elif [ $exit_code -eq 124 ]; then
        log_compilation "$algo" "$lang" "failed" "Timeout (60s)"
        return 1
    else
        # Truncate error message for readability
        error_msg=$(echo "$compile_output" | grep -i "error" | head -3 | tr '\n' ' ' | cut -c1-100)
        if [ -z "$error_msg" ]; then
            error_msg=$(echo "$compile_output" | tail -3 | tr '\n' ' ' | cut -c1-100)
        fi
        if [ -z "$error_msg" ]; then
            error_msg="Compilation failed (exit $exit_code)"
        fi
        log_compilation "$algo" "$lang" "failed" "$error_msg"
        return 1
    fi
}

# Counters
total=0
success=0
failed=0
skipped=0

# Process each algorithm directory
for algo_dir in Algorithms/BruteForce Algorithms/DLX Algorithms/CP; do
    if [ ! -d "$algo_dir" ]; then
        continue
    fi

    algo=$(basename "$algo_dir")
    echo "" | tee -a "$LOG_FILE"
    echo "=== $algo ===" | tee -a "$LOG_FILE"

    # Get all language directories, sorted
    for lang_dir in $(find "$algo_dir" -maxdepth 1 -mindepth 1 -type d | sort); do
        lang=$(basename "$lang_dir")
        ((total++))

        printf "[%3d] %-20s" "$total" "$lang:" | tee -a "$LOG_FILE"

        if compile_language "$lang_dir" "$algo" "$lang"; then
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
echo "======================================" | tee -a "$LOG_FILE"
echo "=== Summary ===" | tee -a "$LOG_FILE"
echo "======================================" | tee -a "$LOG_FILE"
echo "Completed: $(date)" | tee -a "$LOG_FILE"
echo "Total languages: $total" | tee -a "$LOG_FILE"
echo "Compiled successfully: $success" | tee -a "$LOG_FILE"
echo "Skipped (interpreted): $skipped" | tee -a "$LOG_FILE"
echo "Failed: $failed" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

if [ $failed -gt 0 ]; then
    echo "Failed compilations logged to: $ISSUES_FILE" | tee -a "$LOG_FILE"
    echo "" | tee -a "$LOG_FILE"
    echo "Failed languages:" | tee -a "$LOG_FILE"
    jq -r '.[] | select(.status == "compile_error") | "  \(.algorithm)/\(.solver)"' "$ISSUES_FILE" 2>/dev/null | tee -a "$LOG_FILE"
fi

echo "" | tee -a "$LOG_FILE"
echo "Full log saved to: $LOG_FILE" | tee -a "$LOG_FILE"
