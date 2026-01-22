#!/bin/bash
# Run benchmarks for all languages, prioritizing fast ones
# Usage: ./benchmark_all.sh

set -e
cd "$(dirname "$0")/.."

# Output files
ISSUES_FILE="benchmark_issues.json"
LOG_FILE="benchmark_run.log"

# Slow languages (run last)
SLOW_LANGS="Bash Bc Dc Sed Awk Zsh Fish Ksh Tcsh Dash PowerShell AppleScript Make M4 Jq XSLT Gnuplot PostScript"

# Initialize issues file
cat > "$ISSUES_FILE" << 'EOF'
{
  "timestamp": "",
  "failures": [],
  "successes": [],
  "skipped": []
}
EOF

python3 << 'PYEOF'
import json
from datetime import datetime
with open('benchmark_issues.json', 'r') as f:
    data = json.load(f)
data['timestamp'] = datetime.utcnow().isoformat() + 'Z'
with open('benchmark_issues.json', 'w') as f:
    json.dump(data, f, indent=2)
PYEOF

# Function to add result to JSON
log_result() {
    local lang=$1
    local status=$2  # success, failure, skipped
    local message=$3

    python3 << PYEOF
import json
with open('$ISSUES_FILE', 'r') as f:
    data = json.load(f)

result = {
    'language': '$lang',
    'message': '$message'
}

if '$status' == 'failure':
    data['failures'].append(result)
elif '$status' == 'success':
    data['successes'].append(result)
else:
    data['skipped'].append(result)

with open('$ISSUES_FILE', 'w') as f:
    json.dump(data, f, indent=2)
PYEOF
}

# Get all language directories
echo "=== Sudoku Benchmark Runner ===" | tee "$LOG_FILE"
echo "Started: $(date)" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Collect languages
FAST_LANGS=()
SLOW_LANGS_LIST=()

for dir in Algorithms/BruteForce/*/; do
    [ -d "$dir" ] || continue
    lang=$(basename "$dir")

    # Check if slow
    is_slow=0
    for slow in $SLOW_LANGS; do
        if [ "$lang" = "$slow" ]; then
            is_slow=1
            break
        fi
    done

    if [ $is_slow -eq 1 ]; then
        SLOW_LANGS_LIST+=("$lang")
    else
        FAST_LANGS+=("$lang")
    fi
done

echo "Fast/Medium languages: ${#FAST_LANGS[@]}" | tee -a "$LOG_FILE"
echo "Slow languages: ${#SLOW_LANGS_LIST[@]}" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Run benchmark for a language
run_lang() {
    local lang=$1
    local dir="Algorithms/BruteForce/$lang"

    echo "[$(date +%H:%M:%S)] $lang" | tee -a "$LOG_FILE"

    if [ ! -f "$dir/runMe.sh" ]; then
        echo "  ⚠ No runMe.sh" | tee -a "$LOG_FILE"
        log_result "$lang" "skipped" "No runMe.sh found"
        return 1
    fi

    cd "$dir"

    # Run matrices 1-5 only, with timeout
    # Don't pass SKIP_COMPILE - let each script decide
    timeout 900 bash runMe.sh \
        ../../../Matrices/1.matrix \
        ../../../Matrices/2.matrix \
        ../../../Matrices/3.matrix \
        ../../../Matrices/4.matrix \
        ../../../Matrices/5.matrix >> "../../../$LOG_FILE" 2>&1

    local exit_code=$?
    cd - > /dev/null

    if [ $exit_code -eq 0 ]; then
        echo "  ✓ Success" | tee -a "$LOG_FILE"
        log_result "$lang" "success" "Completed matrices 1-5"
        return 0
    elif [ $exit_code -eq 124 ]; then
        echo "  ✗ Timeout (15min)" | tee -a "$LOG_FILE"
        log_result "$lang" "failure" "Timeout after 15 minutes"
        return 1
    else
        echo "  ✗ Failed (exit $exit_code)" | tee -a "$LOG_FILE"
        log_result "$lang" "failure" "Exit code $exit_code"
        return 1
    fi
}

# Phase 1: Fast/Medium languages
echo "=== Phase 1: Fast/Medium Languages ===" | tee -a "$LOG_FILE"
fast_ok=0
fast_fail=0

for lang in "${FAST_LANGS[@]}"; do
    if run_lang "$lang"; then
        ((fast_ok++))
    else
        ((fast_fail++))
    fi
    echo "" | tee -a "$LOG_FILE"
done

echo "Phase 1: $fast_ok succeeded, $fast_fail failed" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Phase 2: Slow languages
echo "=== Phase 2: Slow Languages ===" | tee -a "$LOG_FILE"
slow_ok=0
slow_fail=0

for lang in "${SLOW_LANGS_LIST[@]}"; do
    if run_lang "$lang"; then
        ((slow_ok++))
    else
        ((slow_fail++))
    fi
    echo "" | tee -a "$LOG_FILE"
done

echo "Phase 2: $slow_ok succeeded, $slow_fail failed" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

# Summary
total_ok=$((fast_ok + slow_ok))
total_fail=$((fast_fail + slow_fail))

echo "=== Summary ===" | tee -a "$LOG_FILE"
echo "Completed: $(date)" | tee -a "$LOG_FILE"
echo "Success: $total_ok" | tee -a "$LOG_FILE"
echo "Failed: $total_fail" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"
echo "Results: $ISSUES_FILE" | tee -a "$LOG_FILE"
echo "Full log: $LOG_FILE" | tee -a "$LOG_FILE"
