#!/bin/bash
# Comprehensive benchmark runner for all algorithms (matrices 1-5)
# Run in Docker: docker-compose exec app bash /app/run_all_algorithms_matrices_1-5.sh


MATRICES="/app/Matrices/1.matrix /app/Matrices/2.matrix /app/Matrices/3.matrix /app/Matrices/4.matrix /app/Matrices/5.matrix"

echo "========================================="
echo "Running comprehensive benchmarks"
echo "Algorithms: DLX, CP, BruteForce"
echo "Matrices: 1-5"
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

    # Run with timeout (5 minutes per language)
    if timeout 300 ./runMe.sh $MATRICES 2>&1; then
        echo "  ✓ SUCCESS: $algo/$lang"
        success=$((success + 1))
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "  ✗ TIMEOUT: $algo/$lang (5 min exceeded)"
        else
            echo "  ✗ FAILED: $algo/$lang (exit code: $exit_code)"
        fi
        failed=$((failed + 1))
    fi

    cd /app
}

# Run DLX benchmarks
echo ""
echo "========================================="
echo "PHASE 1: DLX Algorithm"
echo "========================================="

for lang_dir in Algorithms/DLX/*/; do
    run_benchmark "DLX" "$lang_dir"
done

# Run CP benchmarks
echo ""
echo "========================================="
echo "PHASE 2: CP Algorithm"
echo "========================================="

for lang_dir in Algorithms/CP/*/; do
    run_benchmark "CP" "$lang_dir"
done

# Run BF benchmarks
echo ""
echo "========================================="
echo "PHASE 3: BF Algorithm"
echo "========================================="

for lang_dir in Algorithms/BruteForce/*/; do
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
