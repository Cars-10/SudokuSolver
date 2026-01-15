#!/bin/bash
# Run all benchmarks for all algorithms, matrices 1-5
# Skipping: dc, bc, fish, tcsh (very slow)

cd /app

SKIP_LANGS="dc bc fish tcsh Fish Tcsh"
MATRIX_DIR="/app/Matrices"

should_skip() {
    local lang=$1
    for skip in $SKIP_LANGS; do
        if [ "$lang" = "$skip" ]; then
            return 0
        fi
    done
    return 1
}

run_benchmark() {
    local algo=$1
    local lang=$2
    local dir="/app/Algorithms/$algo/$lang"
    
    if should_skip "$lang"; then
        echo "SKIP: $lang (in skip list)"
        return
    fi
    
    if [ ! -f "$dir/runMe.sh" ]; then
        echo "SKIP: No runMe.sh in $dir"
        return
    fi
    
    echo "=== Running $algo/$lang ==="
    cd "$dir"
    
    timeout 600 ./runMe.sh $MATRIX_DIR/1.matrix $MATRIX_DIR/2.matrix $MATRIX_DIR/3.matrix $MATRIX_DIR/4.matrix $MATRIX_DIR/5.matrix
    
    local result=$?
    if [ $result -eq 0 ]; then
        echo "SUCCESS: $algo/$lang"
    elif [ $result -eq 124 ]; then
        echo "TIMEOUT: $algo/$lang"
    else
        echo "ERROR: $algo/$lang (exit $result)"
    fi
    
    cd /app
}

total=0
for algo in BruteForce DLX CP; do
    count=$(ls -d /app/Algorithms/$algo/*/ 2>/dev/null | wc -l)
    total=$((total + count))
done
echo "Total: $total (skipping: dc bc fish tcsh)"
echo ""

current=0
for algo in BruteForce DLX CP; do
    echo ""
    echo "=== $algo ==="
    
    for lang_dir in /app/Algorithms/$algo/*/; do
        lang=$(basename "$lang_dir")
        current=$((current + 1))
        echo "[$current/$total] $algo/$lang"
        run_benchmark "$algo" "$lang"
    done
done

echo ""
echo "=== COMPLETE ==="
