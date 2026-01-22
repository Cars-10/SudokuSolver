#!/bin/bash
# Run full benchmark suite with prioritization and failure tracking

cd "$(dirname "$0")/.."

# Slow languages to defer to end
SLOW_LANGS="Bash Bc Dc Sed Awk Zsh Fish Ksh Tcsh Dash PowerShell AppleScript Make M4 Jq XSLT Gnuplot PostScript"

# Output files
ISSUES_FILE="benchmark_issues.json"
PROGRESS_LOG="benchmark_progress.log"

# Initialize issues file
echo '{"failures": [], "timestamp": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"}' > "$ISSUES_FILE"

# Get all language directories
LANG_DIRS=$(find Algorithms/BruteForce -mindepth 1 -maxdepth 1 -type d | sort)

# Categorize languages
FAST_LANGS=()
SLOW_LANGS_ARRAY=()

for dir in $LANG_DIRS; do
    lang=$(basename "$dir")

    # Check if slow
    is_slow=false
    for slow in $SLOW_LANGS; do
        if [ "$lang" = "$slow" ]; then
            is_slow=true
            break
        fi
    done

    if [ "$is_slow" = true ]; then
        SLOW_LANGS_ARRAY+=("$lang")
    else
        FAST_LANGS+=("$lang")
    fi
done

echo "=== Benchmark Plan ===" | tee "$PROGRESS_LOG"
echo "Fast/Medium languages: ${#FAST_LANGS[@]}" | tee -a "$PROGRESS_LOG"
echo "Slow languages (deferred): ${#SLOW_LANGS_ARRAY[@]}" | tee -a "$PROGRESS_LOG"
echo "Matrices: 1-5" | tee -a "$PROGRESS_LOG"
echo "Recompile: No" | tee -a "$PROGRESS_LOG"
echo "" | tee -a "$PROGRESS_LOG"

# Function to run benchmark for a language
run_benchmark() {
    local lang=$1
    local lang_dir="Algorithms/BruteForce/$lang"

    echo "[$(date +%H:%M:%S)] Running $lang..." | tee -a "$PROGRESS_LOG"

    if [ ! -f "$lang_dir/runMe.sh" ]; then
        echo "  ⚠ No runMe.sh found" | tee -a "$PROGRESS_LOG"
        add_failure "$lang" "no_runme_script" "runMe.sh not found"
        return 1
    fi

    cd "$lang_dir"

    # Run only matrices 1-5, skip compilation
    export SKIP_COMPILE=1
    timeout 600 bash runMe.sh ../../Matrices/1.matrix ../../Matrices/2.matrix ../../Matrices/3.matrix ../../Matrices/4.matrix ../../Matrices/5.matrix 2>&1 | tee -a "../../$PROGRESS_LOG"
    exit_code=$?

    cd - > /dev/null

    if [ $exit_code -ne 0 ]; then
        if [ $exit_code -eq 124 ]; then
            echo "  ✗ Timeout (>10min)" | tee -a "$PROGRESS_LOG"
            add_failure "$lang" "timeout" "Benchmark exceeded 10 minute timeout"
        else
            echo "  ✗ Failed (exit $exit_code)" | tee -a "$PROGRESS_LOG"
            add_failure "$lang" "execution_error" "Exit code: $exit_code"
        fi
        return 1
    else
        echo "  ✓ Complete" | tee -a "$PROGRESS_LOG"
        return 0
    fi
}

# Function to add failure to JSON
add_failure() {
    local lang=$1
    local error_type=$2
    local message=$3

    python3 << EOF
import json
with open('$ISSUES_FILE', 'r') as f:
    data = json.load(f)
data['failures'].append({
    'language': '$lang',
    'error_type': '$error_type',
    'message': '$message',
    'timestamp': '$(date -u +%Y-%m-%dT%H:%M:%SZ)'
})
with open('$ISSUES_FILE', 'w') as f:
    json.dump(data, f, indent=2)
EOF
}

# Run fast/medium languages first
echo "=== Phase 1: Fast/Medium Languages ===" | tee -a "$PROGRESS_LOG"
success_count=0
fail_count=0

for lang in "${FAST_LANGS[@]}"; do
    if run_benchmark "$lang"; then
        ((success_count++))
    else
        ((fail_count++))
    fi
    echo "" | tee -a "$PROGRESS_LOG"
done

echo "Phase 1 Complete: $success_count succeeded, $fail_count failed" | tee -a "$PROGRESS_LOG"
echo "" | tee -a "$PROGRESS_LOG"

# Run slow languages
echo "=== Phase 2: Slow Languages ===" | tee -a "$PROGRESS_LOG"
slow_success=0
slow_fail=0

for lang in "${SLOW_LANGS_ARRAY[@]}"; do
    if run_benchmark "$lang"; then
        ((slow_success++))
    else
        ((slow_fail++))
    fi
    echo "" | tee -a "$PROGRESS_LOG"
done

echo "Phase 2 Complete: $slow_success succeeded, $slow_fail failed" | tee -a "$PROGRESS_LOG"
echo "" | tee -a "$PROGRESS_LOG"

# Summary
total_success=$((success_count + slow_success))
total_fail=$((fail_count + slow_fail))

echo "=== Final Summary ===" | tee -a "$PROGRESS_LOG"
echo "Total: $total_success succeeded, $total_fail failed" | tee -a "$PROGRESS_LOG"
echo "Issues logged to: $ISSUES_FILE" | tee -a "$PROGRESS_LOG"
echo "Full log: $PROGRESS_LOG" | tee -a "$PROGRESS_LOG"
