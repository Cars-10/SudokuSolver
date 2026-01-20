#!/bin/bash
# Local benchmark runner for all algorithms (matrices 1-5)

# Use absolute paths or relative to the script location
cd "$(dirname "$0")"
PROJECT_ROOT=$(pwd)

MATRICES="$PROJECT_ROOT/Matrices/1.matrix $PROJECT_ROOT/Matrices/2.matrix $PROJECT_ROOT/Matrices/3.matrix $PROJECT_ROOT/Matrices/4.matrix $PROJECT_ROOT/Matrices/5.matrix"

export TIMEOUT_SECONDS=1800 # 30 minutes

echo "========================================="
echo "Running comprehensive benchmarks (Local macOS)"
echo "Algorithms: DLX, CP, BruteForce"
echo "Matrices: 1-5"
echo "Timeout: ${TIMEOUT_SECONDS}s"
echo "Started: $(date)"
echo "========================================="

# Counter
total=0
success=0
failed=0
skipped=0

# Function to run benchmark for a language/algorithm
run_benchmark() {
    local algo=$1
    local lang_dir=$2
    local lang=$(basename "$lang_dir")

    total=$((total + 1))

    # Check if already has matrix 5 data
    if [ -f "$lang_dir/metrics.json" ]; then
        if jq -e '.[].results[] | select(.matrix == "5")' "$lang_dir/metrics.json" &>/dev/null; then
            echo "[$total] SKIP: $algo/$lang (already has matrix 5 data)"
            skipped=$((skipped + 1))
            return 0
        fi
    fi

    echo ""
    echo "========================================="
    echo "[$total] Running: $algo/$lang"
    echo "========================================="

    if [ ! -f "$lang_dir/runMe.sh" ]; then
        echo "  ERROR: No runMe.sh found"
        failed=$((failed + 1))
        return 1
    fi

    cd "$lang_dir"

    # Execute runMe.sh
    if ./runMe.sh $MATRICES; then
        echo "  ✓ SUCCESS: $algo/$lang"
        success=$((success + 1))
    else
        exit_code=$?
        echo "  ✗ FAILED: $algo/$lang (exit code: $exit_code)"
        failed=$((failed + 1))
    fi

    cd "$PROJECT_ROOT"
}

# Run DLX benchmarks
echo ""
echo "========================================="
echo "PHASE 1: DLX Algorithm"
echo "========================================="

for lang_dir in Algorithms/DLX/*/; do
    [ -e "$lang_dir" ] || continue
    run_benchmark "DLX" "$lang_dir"
done

# Run CP benchmarks
echo ""
echo "========================================="
echo "PHASE 2: CP Algorithm"
echo "========================================="

for lang_dir in Algorithms/CP/*/; do
    [ -e "$lang_dir" ] || continue
    run_benchmark "CP" "$lang_dir"
done

# Run BF benchmarks
echo ""
echo "========================================="
echo "PHASE 3: BF Algorithm"
echo "========================================="

for lang_dir in Algorithms/BruteForce/*/; do
    [ -e "$lang_dir" ] || continue
    run_benchmark "BruteForce" "$lang_dir"
done

# Summary
echo ""
echo "========================================="
echo "BENCHMARK SUMMARY"
echo "========================================="
echo "Completed: $(date)"
echo "Total processed: $total"
echo "  Success: $success"
echo "  Failed: $failed"
echo "  Skipped: $skipped"
echo "========================================="

# Generate Report
echo ""
echo "========================================="
echo "Generating Report"
echo "========================================="
npx tsx Metrics/HTMLGenerator.ts
